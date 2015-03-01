my $in  = \*STDIN;
my $out = \*STDOUT;

while (<$in>) {
  chomp;
  my @xs = split /\t/;
  print $out join("\t", $xs[0], length $xs[0]) . "\n";
}
