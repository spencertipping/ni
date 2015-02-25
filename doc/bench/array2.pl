while (<>) {
  my @fs = ((split /\t/)[0]);
  print "$fs[0]\t" . length($fs[0]) . "\n";
}
