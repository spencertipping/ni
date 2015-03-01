my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, sqrt(length %0)' -f1 -m '%0, %0 * %0' -m '%0 + %1'

sub f4 { print join("\t", $_[0] + $_[0] * $_[0]) . "\n" }
sub f2 { f4($_[1]) }
sub f1 { f2($_[0], sqrt(length $_[0])) }

while (<$in>) {
  chomp;
  f1 split /\t/;
}
