my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, sqrt(length %0)' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $x;
sub f4 {
  $x = sqrt(length $_[0]);
  print join("\t", $x + $x * $x) . "\n";
}

while (<$in>) {
  chomp;
  f4 split /\t/;
}
