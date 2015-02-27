use List::Util qw/min/;
my $buf = "\0" x 65536;
my $l = 0;
my $prev;
my $f0;
my $n = 1;
my $lastline = 0;
my $offset = 0;
my $d = 0;
while ($l = syscall(0, 0, $buf, 65536) // die "sysread: $!") {
  my $buf_start = index(substr($buf, 0, $l), "\n") + 1;
  $prev .= substr($buf, 0, $buf_start);
  study $prev;
  $n = 1;
  while ($n > 0) {
    $n = index($prev, "\n", $n) + 1;
    if ($n > $lastline) {
      $f0 = index($prev, "\t", $lastline) + 1;
      $d = $f0 - $lastline;
      print substr($prev, $lastline, $d - 1) . "\t$d\n";
      $lastline = $n;
    }
  }

  # At this point we've processed everything in $prev. Now do the same thing to
  # $buf.
  $lastline = $n = $buf_start;
  study $buf;
  while ($n > 0) {
    $n = index($buf, "\n", $n) + 1;
    if ($n > $lastline) {
      $f0 = index($buf, "\t", $lastline) + 1;
      $d = $f0 - $lastline;
      print substr($buf, $lastline, $d - 1) . "\t$d\n";
      $lastline = $n;
    }
  }

  # Now copy the remaining buffer into $prev and go again.
  $prev = substr($buf, $lastline, $l - $lastline);
  $lastline = 0;
}
