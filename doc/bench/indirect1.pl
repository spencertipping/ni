my $in  = \*STDIN;
my $out = \*STDOUT;
while (<$in>) {
  chomp;
  @_ = split /\t/;
  @_ = ($_[0], length $_[0]);
  $_ = join("\t", @_) . "\n";
  print $out $_;
}
