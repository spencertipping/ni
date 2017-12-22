# File-backed data closures.
# Sometimes you have data that's too large to store in-memory in the ni image,
# but you still want it to be forwarded automatically. To handle this case, you
# can use a file-backed data closure: the data is streamed after ni's image state
# and is written directly to disk, never stored in memory. (All that's stored in
# memory is the name of the file.)

# A point of subtlety about the way file closures are handled. I'm doing this
# through URIs because tempdirs might not be portable between machines: on a
# Linux machine all tempfiles might be in /tmp, but on Mac they might be
# somewhere else. So we make sure that the name of the tempfile is computed in
# the same place that it's written, minimizing the likelihood that we'll hit
# permission errors.

defresource 'file-closure',
  read  => q{resource_read closure_data $_[1]},
  write => q{my $tmp = resource_tmp 'file://';
             add_closure_key $_[1], $tmp;
             resource_write $tmp},
  nuke  => q{resource_nuke closure_data $_[1]};

defmetaoperator file_data_closure => q{
  my ($name, $f) = @{$_[0]};
  my $c    = "file-closure://$name";
  my $file = resource_write $c;
  my $fh   = sni @$f;
  sforward $fh, $file;
  close $file;
  close $fh;
  $fh->await;
  nuke_on_exit $c;
  add_quoted_resource $c;
  ();
};

defoperator file_closure_append => q{
  sio;
  sforward resource_read(closure_data $_[0]), \*STDOUT;
};

defshort '///@', pmap q{file_closure_append_op $_}, closure_name;
defshort '/:@',  pmap q{file_data_closure_op @$_},
                 pseq pc closure_name, _qfn;
