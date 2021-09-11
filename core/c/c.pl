# C/C++ language interfacing
# This allows you to use C99 as a compilation target, rather than executing all
# operators in perl.

defconfenv cc      => 'CC',      'c99';
defconfenv cc_opts => 'CC_OPTS', '';

defconfenv cpp      => 'CPP',      'c++';
defconfenv cpp_opts => 'CPP_OPTS', '';

# exec_c($compiler, $cc_opts, $ext, $c_source, @argv...) -> doesn't return
sub exec_c
{
  my $compiler      = shift;
  my $compiler_opts = shift;
  my $extension     = shift;

  local $SIG{CHLD} = 'DEFAULT';

  # First write a tempfile for the c99 compiler. It's important that we put
  # this into a directory that already exists, since all the program should do
  # is unlink itself as a binary.
  my $source_tmp = conf('tmpdir') . "/ni-$ENV{USER}-" . noise_str(16) . $extension;
  {
    open my $source, '>', $source_tmp or die "ni exec_c: can't write source: $!";
    print $source shift;
    close $source;
  }

  (my $binary = $source_tmp) =~ s/$extension$//;
  system "$compiler $compiler_opts -o '$binary' '$source_tmp' >/dev/null && rm -f '$source_tmp'"
     and die "ni exec_c: failed to compile code";

  exec $binary, @_;
  die "ni exec_c: failed to run compiled binary: $!";
}

defoperator c99 => q{exec_c conf('cc'),  conf('cc_opts'),  '.c',  shift};
defoperator cpp => q{exec_c conf('cpp'), conf('cpp_opts'), '.cc', shift};

defshort '/c99', pmap q{c99_op pydent $_}, generic_code;
defshort '/c++', pmap q{cpp_op pydent $_}, generic_code;
