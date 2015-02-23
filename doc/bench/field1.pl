while (<>) {
  chomp;
  my @xs = split /\t/;
  print $xs[0], "\t", length($xs[0]), "\n";
}
