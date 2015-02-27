# Operator implementations

use B::Deparse;
use File::Temp qw/tmpnam/;

# Meta
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_memory(self) };

defop 'explain-stream', undef, '',
  'explains the current stream',
  sub { ni_memory($_[0]->explain) };

defop 'explain-compilation', undef, '',
  'shows the compiled output for the current stream',
  sub {
    my $gen = $_[0]->source_gen(sink_as {
      with_input_type $_[0],
        gen 'print:LV', {}, "print STDOUT \$_;"});
    my $deparser = B::Deparse->new;
    my ($f, $refs) = $gen->compile_to_sub;
    delete $$refs{$_} for keys %$refs;
    ni_memory($deparser->coderef2text($f));
  };

defop 'defined-methods', undef, '',
  'lists defined long and short methods on IO objects',
  sub { ni_memory(map "$_\n", grep /^_/, sort keys %{ni::io::}) };

DEBUG
defop 'debug-compile', undef, '',
  'shows the compiled code generated for the given io',
  sub {
    my $gen = $_[0]->source_gen(sink_as {
                with_input_type $_[0],
                  gen 'print:LV', {}, "print STDOUT \$_;"});
    ni_memory("\nCOMPILED\n" . $gen->compile,
              "\n",
              "\nDEBUG\n"    . $gen->debug_to_string);
  };
DEBUG_END

# Stream transforms
defop 'plus', undef, '',
  'adds two streams together (implied for files)',
  sub { $_[0] + $_[1] };

defop 'tee', undef, 's',
  'tees current output into the specified io',
  sub { $_[0] >>= tee_binding(ni $_[1]) };

defop 'take', undef, 'd',
  'takes the first or last N records from the specified io',
  sub { $_[1] > 0 ? $_[0] >>= take_binding($_[1])
                  : ni_ring(-$_[1]) < $_[0] };

defop 'drop', undef, 'd',
  'drops the first or last N records from the specified io',
  sub {
    my ($self, $n) = @_;
    $n >= 0
      ? $self->bind(drop_binding($n))
      : ni_source_as("$self >>= drop " . -$n . "]", sub {
          my ($destination) = @_;
          $self->source_gen(ni_ring(-$n, $destination));
        });
  };

defop 'zip', 'z', 's',
  'zips lines together with those from the specified IO',
  sub { $_[0] >>= zip_binding(ni $_[1]) };

# Functional transforms
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % $_[1] };

defop 'transform', 'M', 's',
  'transforms the stream as an object using the specified function',
  sub { compile($_[1])->($_[0]) };

defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni_cat($_[0] * \&ni) };

defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my ($self, $f) = @_;
        $self > ni($f //= "file:" . tmpnam);
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
