# Zig language interfacing
# Allows you to stream data through a Zig program.

defconfenv zig => 'ZIG', 'zig';


# exec_zig($zig_binary, $zig_source, @argv...) -> doesn't return
sub exec_zig
{
  my $zig_binary = shift;
  local $SIG{CHLD} = 'DEFAULT';

  # First write a tempfile for the zig compiler. It's important that we put
  # this into a directory that already exists, since all the program should do
  # is unlink its source (which will be in argv[1]).
  my $source_tmp = conf('tmpdir') . "/ni-$ENV{USER}-" . noise_str(16) . ".zig";
  {
    open my $source, '>', $source_tmp or die "ni exec_zig: can't write source: $!";
    print $source shift;
    close $source;
  }

  exec $zig_binary, 'run', $source_tmp, '--', $source_tmp, @_;
  die "ni exec_zig: failed to run: $!";
}


defoperator zig => q{exec_zig conf('zig'), shift};

defshort '/zig', pmap q{zig_op pydent $_}, generic_code;
