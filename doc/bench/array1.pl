while (<>) {
  my @fs = split /\t/;
  print "$fs[0]\t" . length($fs[0]) . "\n";
}
