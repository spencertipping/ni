# ni support for Haskell programs run via Stack

defoperator haskell_stack => q{
  my ($hs_source) = @_;
  my $filename = uri_path resource_tmp 'file://';
  my $proc = siproc { wf $filename, $hs_source;
                      exec '/usr/bin/env', 'stack', $filename };
  sforward \*STDIN, $proc;
  close $proc;
  $proc->await;
  unlink $filename;
};

defshort '/hs', pmap q{haskell_stack_op pydent $_}, generic_code;
