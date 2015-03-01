my $in  = \*STDIN;
my $out = \*STDOUT;
while (<$in>) {
  chomp;
  @_ = split /\t/;
  print $out join("\t", $_[0], length $_[0]) . "\n";
}
