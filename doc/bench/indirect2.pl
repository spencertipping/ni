my $in  = \*STDIN;
my $out = \*STDOUT;
while (<$in>) {
  chomp;
  @_ = split /\t/;
  $_ = join("\t", $_[0], length $_[0]) . "\n";
  print $out $_;
}
