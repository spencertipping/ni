my $in  = \*STDIN;
my $out = \*STDOUT;

my $line = sub {
  print $out $_[0] . "\t" . length($_[0]) . "\n";
};

while (<$in>) {
  chomp;
  $line->(split /\t/);
}
