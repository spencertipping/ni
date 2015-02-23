#!/usr/bin/env perl
eval($ni::selfcode = join '', <DATA>); die $@ if $@;
__DATA__
use v5.14;
no strict 'refs';
package ni;
sub ni;
sub ::ni;

sub self {
  join "\n", "#!/usr/bin/env perl",
             q{eval($ni::selfcode = join '', <DATA>); die $@ if $@;},
             "__DATA__",
             $ni::selfcode;
}

use POSIX qw/:sys_wait_h/;

$SIG{CHLD} = sub {
  local ($!, $?);
  waitpid -1, WNOHANG;
};
# Memoized function compilation
our %compiled_functions;

sub expand_function_shorthands {
  my ($code) = @_;
  $code =~ s/%(\d+)/\$_[$1]/g;

  # JSON shortcuts
  1 while $code =~ s/([a-zA-Z0-9_\)\}\]\?\$])
                     \.
                     ([\$_a-zA-Z](?:-[0-9\w\?\$]|[0-9_\w?\$])*)
                    /$1\->{'$2'}/x;
  $code;
}

sub compile {
  return $_[0] if ref $_[0] eq 'CODE';
  return $compiled_functions{$_[0]}
     //= eval "package main; sub {\n" . expand_function_shorthands($_[0])
                                      . "\n}";
}
# Code generator
# This exists because I want gensyms and external references to be easier to
# deal with. It also supports some nice stuff like insertion points and
# peephole optimizations.

BEGIN {

sub ni::gen::new;
sub gen       { local $_; ni::gen->new(@_) }
sub gen_empty { gen('empty', {}, '') }

package ni::gen;

use overload qw# % subst  * map  @{} inserted_code_keys  "" compile #;

our $gensym_id  = 0;
our $randomness = join '', map sprintf("%04x", rand(65536)), 0..3;
sub gensym { '$' . ($_[0] // '') . "_${randomness}_" . $gensym_id++ }

sub parse_code;
sub new {
  my ($class, $sig, $refs, $code) = @_;
  my ($fragments, $gensyms, $insertions) = parse_code $code;

  # Substitutions can be specified as refs, in which case we pull them out and
  # do a rewrite automatically (this is more notationally expedient than having
  # to do a % operation right away).
  my %subst;
  for (keys %$refs) {
    if (exists $$insertions{$_}) {
      $subst{$_} = $$refs{$_};
      delete $$refs{$_};
    }
  }

  exists $$gensyms{$_} or die "undefined ref $_ in $code" for keys %$refs;
  exists $$refs{$_}    or die    "unused ref $_ in $code" for keys %$gensyms;

  # NB: always copy the fragments because parse_code returns cached results
  bless({ sig               => $sig,
          fragments         => [@$fragments],
          gensym_names      => $gensyms,
          insertion_indexes => $insertions,
          inserted_code     => {},
          refs              => $refs // {} },
        $class) % {%subst};
}

sub genify {
  return $_[0] if ref $_[0] && $_[0]->isa('ni::gen');
  return ni::gen('genified', {}, $_[0]);
}

sub build_ref_hash {
  my ($self, $refs) = @_;
  $refs //= {};
  $$refs{$$self{gensym_names}{$_}} = $$self{refs}{$_} for keys %{$$self{refs}};
  $$self{inserted_code}{$_}->build_ref_hash($refs) for @$self;
  $refs;
}

sub inserted_code_keys {
  my ($self) = @_;
  [sort keys %{$$self{inserted_code}}];
}

sub subst {
  my ($self, $vars) = @_;
  for my $k (keys %$vars) {
    die "unknown subst var: $k (code is $self)"
      unless defined(my $i = $$self{insertion_indexes}{$k});
    $$self{inserted_code}{$k} = $$self{fragments}[$i] = genify $$vars{$k};
  }
  $self;
}

sub map {
  my ($self, $f) = @_;
  $f = ni::compile $f;
  my $y = &$f($self);
  return $y unless $y eq $self;

  # If we haven't changed, then operate independently on the
  # already-substituted code fragments and build a new instance.
  my $new = bless {}, ref $self;
  $$new{$_} = $$self{$_} for keys %$self;
  $$new{fragments}     = [@{$$new{fragments}}];
  $$new{inserted_code} = {%{$$new{inserted_code}}};

  $new % {map {$_, $$new{inserted_code}{$_} * $f} @$new};
}

sub compile {
  my ($self) = @_;
  join '', @{$$self{fragments}};
}

sub run {
  my ($self) = @_;
  my $code     = $self->compile;
  my $refs     = $self->build_ref_hash;
  my $bindings = join "\n", map sprintf("my %s = \$_[0]->{'%s'};", $_, $_),
                                keys %$refs;
  my $f        = eval($code = "package main; sub {\n$bindings\n$code\n}");
  die "$@ compiling\n$code" if $@;

  my @result = &$f($refs);
  delete $$refs{$_} for keys %$refs;    # we create circular refs sometimes
  @result;
}

our %parsed_code_cache;
sub parse_code {
  # Returns ([@code_fragments], {gensym_mapping}, {insertion_indexes})
  my ($code) = @_;
  return @$_ if defined($_ = $parsed_code_cache{$code});

  my @pieces = split /(\%:\w+|\%\@\w+)/, $code;
  my @fragments;
  my %gensyms;
  my %insertion_points;
  for (0..$#pieces) {
    if ($pieces[$_] =~ /^\%:(\w+)$/) {
      push @fragments, $gensyms{$1} = gensym $1;
    } elsif ($pieces[$_] =~ /^\%\@(\w+)$/) {
      $insertion_points{$1} = $_;
      push @fragments, "\ndie 'unfilled fragment: %\@$1';\n";
    } else {
      push @fragments, $pieces[$_];
    }
  }
  @{$parsed_code_cache{$code} = [[@fragments],
                                 {%gensyms},
                                 {%insertion_points}]};
}

}
# Profile-guided optimization
# Sometimes we'll be in a situation where we can't (or I'm too lazy to) tell at
# compile-time which strategy will be fastest. In these cases we can emit
# multiple alternatives, dispatch randomly, and measure empirically,
# recompiling when we reach a conclusion.
#
# The obvious problem with this approach is how we resume an interrupted
# computation. It turns out, though, that because we maintain all of the code
# references it isn't particularly difficult. We just need a few contrivances:
#
# 1. When compiling a loop over an array, we need to save the array and index.
# 2. When resuming within new code, we need a "repair" branch to finish any
#    incomplete loops.
#
# This is explained further below.

use constant GENOPT_REFINE_STDDEV => 3;
use constant GENOPT_REFINE_MIN_MS => 100;
use constant GENOPT_REFINE_MIN_N  => 5;
use constant UNREACHABLE          => gen('unreachable',
                                         {},
                                         q{ die 'unreachable'; });

use List::Util qw/max min/;

sub profiled_element {
  my ($body) = @_;
  my $counters = [0, 0.0];
  gen('profiled', {counters => $counters, body => $body},
    q{ $%:counters[0] = -($%:counters[0] + 1);
       $%:counters[1] -= time;
       %@body
       $%:counters[1] += time;
       $%:counters[0] = -$%:counters[0]; });
}

# Resumable control flow structures
# Computation is resumed in two stages. First we finish out any loops that were
# happening when we exited. This code is different because a resumable loop is
# slower than a non-resumable loop:
#
#   while ($i < @stuff) { ... }    slower than    for (@stuff) { ... }
#
# We want to capitalize on better performance as soon as we can, so rather than
# emitting code that resumes and then reuses that loop, e.g.:
#
#   $fh_ = $stashed_fh_;
#   $i = 20;
#   @i_array = @$stashed_i_array;
#   goto resume_while_loop;
#   while (defined($fh_ = <$fh>)) {
#     @i_array = ...;
#     resume_while_loop:
#     while ($i < @i_array) {
#       ...
#     }
#   }
#
# we instead want the outer loop to use the non-resumable fast version on the
# next iteration. So what we actually emit is this:
#
#   $fh_ = $stashed_fh_;
#   $i = 20;
#   @i_array = @$stashed_i_array;
#   while ($i < @i_array) {             # slow i_array loop
#     ...
#   }
#   while (defined($fh_ = <$fh>)) {
#     @i_array = ...;
#     for (@i_array) {                  # fast, non-resumable loop
#       ...
#     }
#   }

sub nth_option;
sub should_refine;
sub refinement_signal;
sub alternatives {
  return $_[0] if @_ <= 1;

  # Generate the selector structure, which should prefer each alternative
  # equally but produce random decisions. Randomness matters because it
  # guarantees expected unbiased sampling wrt data elements.
  my @options   = @_;
  my @multiples = map $_ / @options, 1..@options;
  my $r = \0;           # NB: must be a reference since gensyms differ

  my $decision = UNREACHABLE;
  for (my $i = $#options; $i >= 0; --$i) {
    $decision = gen("alternative_branch:$i", {r      => $r,
                                              m      => $multiples[$i],
                                              option => $options[$i],
                                              next   => $decision},
                  q{ if ($%:r < %:m) {
                       %@option
                     } else {
                       %@next
                     } });
  }

  my $counters       = [map $$_{refs}{counters}, @options];
  my $alternative_id = ni::gen::gensym 'alternative';
  my $alternative    =
    gen($alternative_id, {r               => $r,
                          counters        => $counters,
                          refinement_data => 0,
                          decision        => $decision},
      q{ # state gensym: %:refinement_data (NB: don't remove this comment)
         $%:r = rand();
         %@decision
         if (ni::should_refine(%:counters)) {
           %@signal_refinement
         } });

  $alternative % {signal_refinement => signal_refinement $alternative};
}

sub nth_option {
  my ($gen, $n) = @_;
  die "must be an alternative branch"
    unless $$gen{sig} =~ /^alternative_branch:(\d+)/;

  $gen = $$gen{inserted_code}{next} while $n--;
  $$gen{option};
}

sub should_refine {
  # TODO: redo this logic completely. We should be considering expected gain
  # per refinement cost, which we can estimate by timing the initial
  # compilation.

  # The time taken by each branch should follow an exponential distribution, so
  # the goal is to look for cases where we're unlikely to have gotten it wrong.
  # In this case we want a 1% error rate; 99% of refinements should result in
  # us choosing the long-run optimal branch. 3 standard deviations of a normal
  # distribution can be used to approximate the exponential confidence
  # interval, which should give us well under 1% error.
  #
  # Note that we refine only if each alternative has used at least 100ms;
  # otherwise the effort isn't really justifiable.

  my ($counters) = @_;
  $$_[1] * 1000 >= GENOPT_REFINE_MIN_MS or return 0 for @$counters;
  $$_[0]        >= GENOPT_REFINE_MIN_N  or return 0 for @$counters;

  # Ok, we probably have enough to work with. The goal is to identify a case
  # where we stand to gain significant performance by eliminating branches. If
  # we have several high-performance options, it probably doesn't matter much
  # which one we take.
  my @ps;
  my @ls;
  my @us;

  for (@$counters) {
    my ($n, $t) = @$_;
    my $adjust = GENOPT_REFINE_STDDEV / sqrt $n;
    push @ps, my $p = $t / $n;
    push @ls, $p * (1 - $adjust);
    push @us, $p * (1 + $adjust);
  }

  # Refine if we have a decisive preference. (TODO: weigh the implications of
  # having a single refinement vs progressive ones in cases of fast-branch
  # uncertainty.)
  min(@us) > max(@ls);
}

sub signal_refinement {
  my ($alternative) = @_;
  gen('refinement_signal', {alternative => $alternative},
    q{ %@save_iterator_state
       %@rewrite_alternative
       return [%@alternative_key, %:alternative] });
}
# Extensible IO stream abstraction
# Streams are defined by the Perl code that runs in order to put their values
# somewhere. This abstraction ends up getting completely erased at runtime,
# which is good because Perl OO is really slow.
#
# If you want to construct one of these and use it for IO purposes, the fastest
# option should be to get a filehandle for it first:
#
# my $fh = $ni_io->into_fh;
# while (<$fh>) {
#   ...
# }
#
# my $fh = $ni_io->from_fh;
# print $fh "foo bar\n";
#
# This will fork the compiled code into a separate process, which is still
# usually faster than the abstraction otherwise required.

our %io_constructors;

sub is_io { ref $_[0] && $_[0]->isa('ni::io') }

sub defio {
  my ($name, $constructor, $methods) = @_;
  *{"ni::io::${name}::new"} = $io_constructors{$name} = sub {
    my ($class, @args) = @_;
    bless $constructor->(@args), $class;
  };
  *{"::ni_$name"} = *{"ni::ni_$name"} =
    sub { ${"ni::io::${name}::"}{new}("ni::io::$name", @_) };
  *{"ni::io::$name::$_"} = $methods->{$_} for keys %$methods;
  push @{"ni::io::${name}::ISA"}, 'ni::io';
}

# Codegen adapters
# We want to avoid converting between lines and fields as much as possible, so
# each code element tracks its operand and return types and we convert between
# them automatically. Types are:
#
# F = array of fields stored in @_
# L = single line with \n, stored in $_
# R = single row without \n, stored in $_
# V = void; used as a return type to indicate that values all go to a sink but
#     are not usable in a post-side-effect state, or as an input type to
#     indicate that the input is ignored.
#
# These types are annotated after a : in a code block's signature.

our %sig_conversions = (
  FL => q{ %@before chomp @_; $_ = join("\t", @_) . "\n"; %@after },
  FR => q{ %@before           $_ = join("\t", @_);        %@after },
  LF => q{ %@before chomp;    @_ = split /\t/;            %@after },
  LR => q{ %@before chomp;                                %@after },
  RF => q{ %@before           @_ = split /\t/;            %@after },
  RL => q{ %@before           $_ .= "\n" unless /\n$/;    %@after },

  FF => q{ %@before %@after },
  LL => q{ %@before %@after },
  RR => q{ %@before %@after },
);

$sig_conversions{$_} = gen("conv:$_", {}, $sig_conversions{$_})
  for keys %sig_conversions;

sub input_sig  { (${$_[0]}{sig} =~ /:(\w)\w$/)[0] }
sub output_sig { (${$_[0]}{sig} =~ /:\w(\w)$/)[0] }

sub with_input_type {
  my ($sig, $code) = @_;
  die "unsigned code block: $code ($$code{sig})"
    unless my $codesig = input_sig $code;
  die "unknown code transform $sig$codesig for $sig > $$code{sig} ($codesig)"
    unless defined(my $transform = $sig_conversions{"$sig$codesig"});
  $transform % {before => gen_empty, after => $code};
}

sub with_output_type {
  my ($sig, $code) = @_;
  die "unsigned code block: $code ($$code{sig})"
    unless my $codesig = output_sig $code;
  die "unknown code transform $sig$codesig for $$code{sig} ($codesig) > $sig"
    unless defined(my $transform = $sig_conversions{"$codesig$sig"});
  $transform % {before => $code, after => gen_empty};
}

sub mapone_binding;
sub flatmap_binding;
sub reduce_binding;
sub grep_binding;
sub pipe_binding;

# Internally we're using these IO objects to generate imperative code, so it's
# going to be source-driven. This means we can't do much until we know where
# the values need to go (though we can defer that by fork/piping).

{

package ni::io;
use overload qw# + plus_op  * mapone_op  / reduce_op  % grep_op  | pipe_op
                 >>= bind_op
                 > into
                 < from #;

BEGIN { *gen = \&ni::gen }

use POSIX qw/dup2/;

# Methods implemented by children
sub source_gen { ... }     # gen to source from this thing
sub sink_gen   { ... }     # gen to sink into this thing

sub flatten { ($_[0]) }

# Transforms
sub plus_op   { $_[0]->plus($_[1]) }
sub bind_op   { $_[0]->bind($_[1]) }
sub mapone_op { $_[0]->mapone($_[1]) }
sub reduce_op { $_[0]->reduce($_[1], {}) }
sub grep_op   { $_[0]->grep($_[1]) }
sub pipe_op   { $_[0]->pipe($_[1]) }

sub plus    { ::ni_sum(@_) }
sub bind    { ::ni_bind(@_) }
sub mapone  { $_[0] >>= ni::mapone_binding  @_[1..$#_] }
sub flatmap { $_[0] >>= ni::flatmap_binding @_[1..$#_] }
sub reduce  { $_[0] >>= ni::reduce_binding  @_[1..$#_] }
sub grep    { $_[0] >>= ni::grep_binding    @_[1..$#_] }
sub pipe    { $_[0] >>= ni::pipe_binding    @_[1..$#_] }

# User-facing methods
sub from {
  my ($self, $source_fh) = @_;
  gen('from_fh:VV', {fh   => $source_fh,
                     body => with_input_type('L', $self->sink_gen)},
    q{ while (<%:fh>) {
         %@body
       } })->run;
}

sub from_fh {
  my ($self) = @_;
  pipe my $out, my $in or die "pipe failed: $!";
  unless (fork) {
    close $in;
    $self->from($out);
    close $out;
    exit;
  }
  close $out;
  $in;
}

sub into {
  my ($self, $dest_fh) = @_;
  $self->source_gen(gen 'into_fh:LV', {fh => $dest_fh},
    q{ print %:fh $_; })->run;
}

sub into_fh {
  my ($self) = @_;
  pipe my $out, my $in or die "pipe failed: $!";
  unless (fork) {
    close $out;
    $self->into($in);
    close $in;
    exit;
  }
  close $in;
  $out;
}

}
# Data source definitions
BEGIN {
  our @data_names;
  our %data_matchers;
  our %data_transformers;

  sub defdata {
    my ($name, $matcher, $transfomer) = @_;
    die "data type $name is already defined" if exists $data_matchers{$name};
    push @data_names, $name;
    $data_matchers{$name}     = $matcher;
    $data_transformers{$name} = $transfomer;
  }

  sub ni_io_for {
    my ($f, @args) = @_;
    for my $n (@data_names) {
      return $data_transformers{$n}->($f, @args)
        if $data_matchers{$n}->($f, @args);
    }
    die "$f does not match any known ni::io constructor";
  }

  sub ::ni {
    my ($f, @args) = @_;
    return undef unless defined $f;
    return $f if ref $f && $f->isa('ni::io');
    return ni_io_for($f, @args);
  }

  *{"ni::ni"} = *{"::ni"};
}
# Data source/sink implementations

defdata 'globfile', sub { ref $_[0] eq 'GLOB' }, sub { ni_file($_[0]) };

=comment

sub deffilter {
  my ($extension, $read, $write) = @_;
  $extension = qr/\.$extension$/;
  defdata $extension,
    sub { $_[0] =~ /$extension/ },
    sub { ni_filter(ni_file($_[0]), $read, $write) };
}

deffilter 'gz',  'gzip -d',  'gzip';
deffilter 'lzo', 'lzop -d',  'lzop';
deffilter 'xz',  'xz -d',    'xz';
deffilter 'bz2', 'bzip2 -d', 'bzip2';

=cut

defdata 'file', sub { -e $_[0] || $_[0] =~ s/^file:// }, sub { ni_file($_[0]) };
BEGIN {

# Bidirectional file IO
defio 'file',
sub { [@_] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    unless (ref $$self[0] eq 'GLOB') {
      open my $fh, $$self[0] or die "failed to open $$self[0]: $!";
      $$self[0] = $fh;
    }
    gen('file_source:VV', {fh   => $$self[0],
                           body => with_input_type('L', $destination)},
      q{ while (<%:fh>) {
           %@body
         } });
  },

  sink_gen => sub {
    my ($self) = @_;
    unless (ref $$self[1] eq 'GLOB') {
      open my $fh, $$self[1] or die "failed to open $$self[1]: $!";
      $$self[1] = $fh;
    }
    gen('file_sink:LV', {fh => $$self[1]}, q{ print %:fh $_; });
  },
};

defio 'memory',
sub { [@_] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    gen('memory_source:VV', {xs   => $self,
                             body => with_input_type('R', $destination)},
      q{ for (@{%:xs}) {
           %@body
         } });
  },

  sink_gen => sub {
    my ($self) = @_;
    gen('memory_sink:RV', {xs => $self}, q{ push @{%:xs}, $_; });
  },
};

# Sum of multiple IOs
defio 'sum',
sub { [map $_->flatten, @_] },
{
  flatten    => sub { @{$_[0]} },
  source_gen => sub {
    my ($self, $destination) = @_;
    return gen('empty', {}, '') unless @$self;
    my ($first, @others) = @$self;
    my $gen = $first->source_gen($destination);
    $gen = gen('sum_source:VV', {x => $gen,
                                 y => $_->source_gen($destination)},
               q{ %@x; %@y }) for @others;
    $gen;
  },
};

# Concatenation of an IO of IOs
defio 'cat',
sub { \$_[0] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self->source_gen(gen('cat_source:VV', {dest => $destination},
      q{ $_->source_gen(%:dest)->run; }));
  },
};

# Introduces arbitrary indirection into an IO's code stream.
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen($$self{code_transform}($destination));
  },
};

sub invocation {
  my ($f, @args) = @_;
  if (@args || ref $f eq 'CODE' || $f =~ s/^;//) {
    # We need to generate a function call.
    gen('fn:FF', {f => compile($f), args => [@args]},
      q{ %:f->(@_, @{%:args}) });
  } else {
    # We can inline the expression to avoid function call overhead.
    gen('fn:FF', {f => $f}, q{ (%@f) });
  }
}

# Bindings for common transformations
sub flatmap_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("flatmap:${is}V", {invocation => $i,
                           body       => with_input_type('R', $into)},
      q{ for (%@invocation) {
           %@body
         } });
  };
}

sub mapone_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("mapone:${is}V", {invocation => $i,
                          body       => with_input_type('F', $into)},
      q{ if (@_ = %@invocation) {
           %@body
         } });
  };
}

sub grep_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("grep:${is}V", {invocation => $i,
                        body       => with_input_type($is, $into)},
      q{ if (%@invocation) {
           %@body
         } });
  };
}

sub reduce_binding {
  my ($f, $init) = @_;
  $f = compile $f;
  sub {
    my ($into) = @_;
    gen('reduce:FV', {f    => $f,
                      init => $init,
                      body => with_input_type('R', $into)},
      q{ (%:init, @_) = %:f->(%:init, @_);
         for (@_) {
           %@body
         } });
  };
}

}
# High-level pipe operations, each of which corresponds to a command-line
# option. They can also be used from compiled code.

our %op_shorthand_lookups;      # keyed by short
our %op_shorthands;             # keyed by long
our %op_formats;                # ditto
our %op_usage;                  # ditto
our %op_fns;                    # ditto

sub defop {
  my ($long, $short, $format, $usage, $fn) = @_;
  if (defined $short) {
    $op_shorthands{$long}         = $short;
    $op_shorthand_lookups{$short} = "--$long";
  }
  $op_formats{$long} = $format;
  $op_usage{$long}   = $usage;
  $op_fns{$long}     = $fn;

  die "operator $long already exists (possibly as a method rather than an op)"
    if exists $ni::io::{$long};

  *{"ni::io::$long"} = $fn;     # programmatic access
}

our %format_matchers = (
  a => qr/^[a-zA-Z]+$/,
  d => qr/^[-+\.0-9]+$/,
  s => qr/^.*$/,
  v => qr/^[^-].*$/,
);

sub apply_format {
  my ($format, @args) = @_;
  my @format = split //, $format;
  my @parsed;

  for (@format) {
    die "too few arguments for $format" if !@args && !/[A-Z]/;
    my $a = shift @args;
    if ($a =~ /$format_matchers{lc $_}/) {
      push @parsed, $a;
    } else {
      die "failed to match format $format" unless /[A-Z]/;
      push @parsed, undef;
    }
  }

  [@parsed], @args;
}

sub file_opt { ['plus', ni $_[0]] }
sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map file_opt($_), @_ if $o eq '--';
    if ($o =~ /^--/) {
      my $c = $o =~ s/^--//r;
      die "unknown long command: $o" unless exists $op_fns{$c};
      my ($args, @rest) = apply_format $op_formats{$c}, @_;
      push @parsed, [$c, @$args];
      @_ = @rest;
    } elsif ($o =~ s/^-//) {
      unshift @_, map $op_shorthand_lookups{$_} // $_,
                      $o =~ /([:+^=%\/]?[a-zA-Z]|[-+\.0-9]+)/g;
    } else {
      push @parsed, file_opt $o;
    }
  }
  @parsed;
}
# Operator implementations

use File::Temp qw/tmpnam/;

# Meta
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_memory(self) };

# Debugging
defop 'debug-compile', undef, '',
  'shows the compiled code generated for the given io',
  sub { ni_memory($_[0]->source_gen(gen('print:LV', {}, 'print $_;'))) };

# Functional transforms
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % $_[1] };

defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni_cat($_[0] * \&ni) };

defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my $f = $_[1] // tmpnam;
        $_[0]->into(ni $_[1]);
        ni_memory($f) };

defop 'branch', 'b', 's',
  'splits input by its first field, forwarding to subprocesses',
  sub {
    my ($in, $subprocesses) = @_;
    my @subs = unpack_branch_map $subprocesses;
    my $fifo = ni::io::fifo->new->from(map ${$_}[1], @subs);

    unless (fork) {
      my $line;
      while (defined($line = <$in>)) {
        my ($k, $v) = split /\t/, $line, 2;
        for my $s (@subs) {
          if ($s->[0]->($k)) {
            $s->[1]->enqueue($line);
            last;
          }
        }
      }
      exit;
    }
    $fifo;
  };

# Sorting (shells out to command-line sort)
sub sort_options {
  my @fieldspec = split //, $_[0] // '';
  # TODO
}

defop 'order', 'o', 'AD',
  'order {n|N|g|G|l|L|u|U|m} [fields]',
  sub {
    my ($in, $flags, $fields) = @_;
    $in | 'sort';
  };
# Preprocess command line, collapsing stuff into array and hash references as
# appropriate.

use POSIX qw/dup2/;

sub preprocess_cli {
  my @preprocessed;
  for (my $o; defined($o = shift @_);) {
    if ($o =~ s/\[$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\]$/;
        $depth += /\[$/;
        push @xs, $_;
      }
      push @preprocessed, bless [@xs], $o;
    } elsif ($o =~ s/\{$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\}$/;
        $depth += /\{$/;
        push @xs, $_;
      }
      push @preprocessed, bless {@xs}, $o;
    } else {
      push @preprocessed, $o;
    }
  }
  @preprocessed;
}

$|++;
my $data = -t STDIN ? ni_sum() : ni_file(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $data = $ni::io::{$command}($data, @args);
}

if (-t STDOUT && !exists $ENV{NI_NO_PAGER}) {
  # Use a pager rather than writing straight to the terminal
  close STDIN;
  dup2 0, fileno $data->into_fh or die "dup2 failed: $!";
  exec $ENV{NI_PAGER} // $ENV{PAGER} // 'less';
  exec 'more';
  # Ok, we're out of options; just write to the terminal after all
  print while <>;
} else {
  $data > \*STDOUT;
}
