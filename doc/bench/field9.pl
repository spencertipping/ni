my $f0;
my $f1;
my $f2;

while (<>) {
  $f0 = index($_, "\t") + 1;
  $f1 = index($_, "\t", $f0) + 1;
  $f2 = index($_, "\t", $f1) + 1;

  print substr($_, 0, $f0), "\t$f0\n";
}
