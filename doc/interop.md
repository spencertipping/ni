# Language interop
ni's language interop is much more sophisticated than the model used by nfu. In
particular, it addresses the following issues with nfu's protocol:

1. Separate operators were required for mapping and keyed aggregation.
2. Because language subprocesses consumed TSV, numeric operations were
   expensive.
3. Also because language subprocesses consumed TSV, parsing was expensive.
4. nfu doesn't help you do things like pushback or advance-dequeueing.

## High-level differences from nfu
nfu's protocol worked like this:

```perl
while (<>) {
  print "$_\n" for $your_function->(split /\t/);
}
```

ni's protocol works like this (ported appropriately for each supported
language):

```perl
our @deferred;
our $current_record;
sub next_record {
  push @deferred, read_record unless @deferred;
  $current_record = pop @deferred;
}

sub defer { push @deferred, @_ }

# field accessor functions
sub f0 { $current_record->text(0) }
sub i0 { $current_record->int64(0) }
sub d0 { $current_record->double(0) }
...

until (eof and not @deferred) {
  next_record;
  emit_record $_ for $your_function->();
}
```

There are some significant differences here:

1. ni functions use type-aware accessors like `i0` to read data.
2. ni functions are at liberty to read multiple records per invocation.
3. ni functions can pushback arbitrary data for future invocations.

A useful consequence of (2) is that there is no distinction between map and
reduce operations.

## Writing reducers in ni
As always, best illustrated in terms of word count. Using the formal map/reduce
model where we emit `<word, count>` tuples and then aggregate them, the nfu
workflow looks like this:

```sh
$ nfu words.txt -m 'row %0, 1' -gA 'row $_, sum @{%1}'
```

One disadvantage of the above is that nfu loads the entire reducer group into
memory at once; it has no operation that streams reducer entries. This means
that if some word occurs 10,000,000 times, we're using tens (if not hundreds)
of megabytes of memory.

ni implements reduction in terms of lazy sequences that are materialized using
the `next_record` function. (This means you can defer records and they'll be
reducer inputs.) Here's what the above word count example looks like in ni:

```sh
$ ni words.txt -m 'r f0, 1' -gm 'r f0, a0->i1->sum'
```

ni doesn't provide a `sum()` function for reasons described below.

## Memory considerations
A simple linear reduction like the one above will run in constant space, but
things get more complicated when you access multiple fields of an aggregation.
For example, suppose you want to calculate the mean and variance of row groups
simultaneously:

```sh
$ ni data.txt -m 'r f0, mean(a0->i1), variance(a0->i2)'
```

If you write this, ni has no choice but to buffer either `->i1` or `->i2` into
memory. Getting things to run in constant space requires that you hand control
over to ni's record iterator with a pub/sub-style mechanism. You can do this by
using the `mean` and `variance` methods, which are internally expressed as
reductions:

```sh
$ ni data.txt -m 'r f0, a0->i1->mean, a0->i2->variance'
```

The `mean` method is implemented like this (but faster; see the discussion
about JIT below):

```perl
sub ni::mean {
  my ($self) = @_;
  $self->reduce(
    [0, 0],
    sub {               # update function
      my ($state, $x) = @_;
      $$state[0] += $x;
      ++$$state[1];
      $state;
    },
    sub {               # finalize function
      my ($state) = @_;
      $$state[0] / ($$state[1] || 1);
    });
}
```

Framing the computation as a reduction allows ni to IO-synchronize computation
across multiple covariant sequences.

An important consequence of this is that you need to create all reductive
computations before forcing any of them:

```sh
# this command will fail:
$ ni data.txt -m 'my $mean = ${a0->i1->mean};           # forces the reduction
                  my $variance = ${a0->i2->variance};   # error: aliasing
                  r f0, $mean, $variance'

# this is what you want:
$ ni data.txt -m 'my $mean = a0->i1->mean;              # lazy
                  my $variance = a0->i2->variance;      # also lazy
                  r f0, $mean, $variance'               # ok (r forces both)
```

In the unlikely event that you actually do want to force a group and load the
next one within a single invocation, you can use `a0->reset` to suppress the
aliasing error:

```sh
$ ni data.txt -m 'my $f0 = f0;                          # capture current f0
                  my $mean = ${a0->i1->mean};           # force the group
                  my $var = ${a0->reset->i2->variance}; # force the next group
                  r $f0, $mean, $var'
```

In general you won't want to do this.

## JIT
Most dynamic languages impose significant abstraction overhead, but they also
typically support `eval`. ni capitalizes on the latter by allowing you to pass
strings instead of subroutine references in almost all cases. These strings
will then be compiled into an optimized function. JIT will almost always be
significantly faster than the equivalent functional code, particularly when
you're calculating multiple reductions.

All you need to do to enable JIT is quote your code using `q{}` or similar
instead of `sub{}` and use different parameter addressing:

```perl
# non-jit version
sub ni::sum {
  my ($self) = @_;
  $self->reduce(
    0,
    sub { $_[0] + $_[1] },
    sub { $_[0] });
}

# jit version
sub ni::sum {
  my ($self) = @_;
  $self->reduce(
    0,
    q{ $x, $y | $x + $y },              # or q{ |$x, $y| $x + $y }
    q{ $x | $x };                       # or q{ |$x| $x }
}
```

```ruby
# non-jit version
def Ni::sum
  reduce 0,
         lambda {|x, y| x + y},
         lambda {|x| x}
end

# jit version
def Ni::sum
  reduce 0,
         %(x, y| x + y),                # or %(|x, y| x + y)
         %(x| x)                        # or %(|x| x)
end
```

JIT has three minor disadvantages that are worth being aware of:

1. Subroutines are no longer closures, though you can pass in named references
   to similar effect.
2. Error reporting is much less helpful.
