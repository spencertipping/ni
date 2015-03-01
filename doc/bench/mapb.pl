my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $f2 = sub { print(($_[1] + $_[1] * $_[1]) . "\n") };
my $f1 = sub { $f2->($_[0], length $_[0]) };

while (<$in>) {
  chomp;
  $f1->(split /\t/);
}
