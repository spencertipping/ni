my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, sqrt(length %0)' -f1 -m '%0, %0 * %0' -m '%0 + %1'

sub f4 { print join("\t", sqrt(length $_[0])
                        + sqrt(length $_[0]) * sqrt(length $_[0])) . "\n" }

while (<$in>) {
  chomp;
  f4 split /\t/;
}
