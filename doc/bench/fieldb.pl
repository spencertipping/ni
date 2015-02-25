my $buf = "\0" x 65536;
my $l = 0;
my $prev;
my $f0;
my $n = 1;
my $lastline = 0;
my $offset = 0;
my $d = 0;
while ($l = syscall(0, 0, $buf, 65536) // die "sysread: $!") {
  $prev .= substr($buf, 0, $l);
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
  $prev = substr($buf, $lastline);
  $lastline = 0;
  $offset = length $buf;
}
