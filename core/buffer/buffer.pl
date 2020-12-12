# Buffering operators.
# Buffering a stream causes it to be forced in its entirety. Buffering does not
# imply, however, that the stream will be consumed at any particular rate; some
# buffers may be size-limited, at which point writes will block until there's
# space available.

defshort '/B',
  defdsp 'bufferalt', 'dispatch table for /B buffer operator',
    n => pmap(q{buffer_null_op}, pnone),
    d => pmap(q{buffer_disk_op $_}, datasize);


# Null buffer.
# Forwards data 1:1, but ignores pipe signals on its output.

defoperator buffer_null => q{local $SIG{PIPE} = 'IGNORE'; sio};


# Disk buffer.
# A circular file-backed data buffer that cuts through if both sides are
# available. If the sink-side is blocked, then up to the specified number of
# bytes will be buffered into the file to be asynchronously forwarded as the
# sink becomes available.

defoperator buffer_disk =>
q{
  use bytes;
  use Fcntl 'SEEK_SET';
  use constant membuf => 1048576;

  my ($bytes) = @_;
  my ($rmask, $wmask, $emask) = ('', '', '');
  vec($rmask, 0, 1) = 1;
  vec($wmask, 1, 1) = 1;
  $emask = $rmask | $wmask;

  fh_nonblock \*STDOUT;

  # Create and immediately unlink the file so the OS can reclaim the disk blocks
  # if we exit early -- e.g. due to SIGPIPE. The downside of this approach is
  # that we'll be using disk blocks without the user having a name to track
  # them, but the upside is that there's no chance we'll leave a file lying
  # around if anything goes wrong.
  my $f = substr resource_tmp('file://'), 7;
  open my $fw, '+>', $f or die "buffer_disk $bytes +> $f: $!";
  open my $fr, '<',  $f or die "buffer_disk $bytes  < $f: $!";
  unlink $f;

  my ($r, $w) = (0, 0);
  my $buf = '';
  while (1)
  {
    # $r and $w are the canonical unwrapped read/write position markers. Data is
    # readable iff $r < $w.
    my $rmark = $r % $bytes;
    my $wmark = $w % $bytes;
    my $readable = $r < $w          && ($rmark <  $wmark ? $wmark : $bytes) - $rmark;
    my $writable = $w - $r < $bytes && ($rmark <= $wmark ? $bytes : $rmark) - $wmark;

    vec($rmask, 0, 1) = $writable > 0;
    vec($wmask, 1, 1) = $readable > 0;

    select(my $rout = $rmask, my $wout = $wmask, my $eout = $emask, undef);

    # EOF or broken pipe: either means we cut over to the finalization loop.
    last if vec $eout, 0, 1 or vec $eout, 1, 1;

    if (!$readable and vec $rout, 0, 1 and vec $wout, 1, 1)
    {
      # Cut-through case: read-side and write-side are both free, so skip the
      # disk unless STDOUT backs up partway through the write.
      my $i = saferead \*STDIN, $buf, min membuf, $writable;
      last unless $i;

      my $o = safewrite \*STDOUT, $buf;
      if ($o < $i)
      {
        sysseek $fw, $wmark, SEEK_SET;
        $w += safewrite_exactly $fw, substr $buf, $o if $o < $i;
      }
    }
    else
    {
      if ($readable and vec $wout, 1, 1)
      {
        sysseek $fr, $rmark, SEEK_SET;
        saferead $fr, $buf, min $readable, membuf;

        # We don't know how much of the data is going to be written to nonblocking
        # STDOUT, so advance the buffer-read pointer only by as much as we
        # successfully write.
        $r += safewrite \*STDOUT, $buf;
      }

      if ($writable and vec $rout, 0, 1)
      {
        last unless saferead \*STDIN, $buf, min $writable, membuf;
        sysseek $fw, $wmark, SEEK_SET;
        $w += safewrite_exactly $fw, $buf;
      }
    }
  }

  fh_block \*STDOUT;

  # Write any remaining disk-buffer contents
  while ($r < $w)
  {
    my $rmark = $r % $bytes;
    my $wmark = $w % $bytes;
    my $readable = ($rmark <= $wmark ? $wmark : $bytes) - $rmark;

    sysseek $fr, $rmark, SEEK_SET;
    $r += saferead $fr, $buf, min membuf, $readable;
    safewrite_exactly \*STDOUT, $buf;
  }
};
