my $buf;
my $f0;
my $n = 1;
my $lastline = 0;
my $offset = 0;
my $d = 0;
while (sysread(STDIN, $buf, 65536, $offset) // die "sysread: $!") {
  $n = 1;
  while ($n > 0) {
    $n = index($buf, "\n", $n) + 1;
    if ($n > $lastline) {
      $f0 = index($buf, "\t", $lastline) + 1;
      $d = $f0 - $lastline;
      print substr($buf, $lastline, $d - 1) . "\t$d\n";
      $lastline = $n;
    }
  }
  $buf = substr($buf, $lastline);
  $lastline = 0;
  $offset = length $buf;
}
