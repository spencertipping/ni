# C language interfacing
# This allows you to use C99 as a compilation target, rather than executing all
# operators in perl.

defconfenv cc      => 'CC',      'c99';
defconfenv cc_opts => 'CC_OPTS', '';

# exec_c99($c_source, @argv...) -> doesn't return
sub exec_c99
{
  local $SIG{CHLD} = 'DEFAULT';

  # First write a tempfile for the c99 compiler. It's important that we put
  # this into a directory that already exists, since all the program should do
  # is unlink itself as a binary.
  my $source_tmp = conf('tmpdir') . "/ni-$ENV{USER}-" . noise_str(16) . ".c";
  {
    open my $source, '>', $source_tmp or die "ni exec_c99: can't write source: $!";
    print $source shift;
    close $source;
  }

  my $compiler      = conf 'cc';
  my $compiler_opts = conf 'cc_opts';
  (my $binary = $source_tmp) =~ s/\.c$//;
  system "$compiler $compiler_opts -o '$binary' '$source_tmp' >/dev/null && rm -f '$source_tmp'"
     and die "ni exec_c99: failed to compile code";

  exec $binary, @_;
  die "ni exec_c99: failed to run compiled binary: $!";
}
