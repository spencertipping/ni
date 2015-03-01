my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $y;
my $x = \$y;
my $f1 = sub {
  $$x = length $_[0];
  print($$x + $$x * $$x) . "\n";
};

while (<$in>) {
  chomp;
  $f1->(split /\t/);
}
