my $in  = \*STDIN;
my $out = \*STDOUT;

my $line = sub {
  print $out join("\t", $_[0], length $_[0]) . "\n";
};

while (<$in>) {
  chomp;
  $line->(split /\t/);
}
