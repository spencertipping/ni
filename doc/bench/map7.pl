my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $line = sub { print $_[0] . "\n" };
my $f4   = sub { $line->($_[0] + $_[1]) };
my $f3   = sub { $f4->($_[0], $_[0] * $_[0]) };
my $f2   = sub { $f3->($_[1]) };
my $f1   = sub { $f2->($_[0], length $_[0]) };

while (<$in>) {
  chomp;
  $f1->(split /\t/);
}
