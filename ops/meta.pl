use B::Deparse;
use List::MoreUtils qw/firstidx/;

defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_memory(self) };

defop 'modules', undef, '',
  'lists names of defined modules',
  sub { $_[0] + ni_memory(map $$_[0], @ni::modules) };

defop 'module', undef, 's',
  'lists the source code of the specified module',
  sub {
    my ($self, $name) = @_;
    my $index = firstidx {$$_[0] eq $name} @ni::modules;
    $_[0] + ni_memory($index >= 0 ? $ni::modules[$index][1] : '');
  };

defop 'ops', undef, '',
  'lists short and long stream operations',
  sub {
    $_[0] + ni_memory(map sprintf("%s\t--%s\t%s", exists $op_shorthands{$_}
                                                    ? "-$op_shorthands{$_}"
                                                    : '',
                                                  $_,
                                                  $op_usage{$_}),
                          sort keys %op_usage);
  };

defop 'explain-stream', undef, '',
  'explains the current stream',
  sub { ni_memory($_[0]->explain) };

defop 'explain-compilation', undef, '',
  'shows the compiled output for the current stream',
  sub {
    my $gen = $_[0]->source_gen(sink_as {
      with_type $_[0], gen 'print:L', {}, "print \$_;"});
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
                with_type $_[0],
                  gen 'print:L', {}, "print STDOUT \$_;"});
    ni_memory("\nCOMPILED\n" . $gen->compile,
              "\n",
              "\nDEBUG\n"    . $gen->debug_to_string);
  };
DEBUG_END
