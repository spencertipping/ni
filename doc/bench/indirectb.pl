my $in  = \*STDIN;
my $out = \*STDOUT;

my @xs;
while (<$in>) {
  chomp;
  @xs = split /\t/;
  print $out join("\t", $xs[0], length $xs[0]) . "\n";
}
