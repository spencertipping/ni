my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $f1 = sub { print((length($_[0]) + length($_[0]) * length($_[0])) . "\n") };

while (<$in>) {
  chomp;
  $f1->(split /\t/);
}
