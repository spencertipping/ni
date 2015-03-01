my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
  print $out join("\t", ($_[0], length $_[0])) . "\n";
}

while (<$in>) {
  chomp;
  line split /\t/;
}
