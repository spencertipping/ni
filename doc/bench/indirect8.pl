my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
}

while (<$in>) {
  chomp;
  (sub {
    print $out join("\t", $_[0], length $_[0]) . "\n";
  })->(split /\t/);
}
