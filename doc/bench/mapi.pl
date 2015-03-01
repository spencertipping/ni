use strict;

my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $depth = 0;

my ($f1, $f2, $f3, $f4, $line);
my $f;
my @args;

$line = sub { ($f, @args) = ($line, @_), die if ++$depth > 262144;
              print((length($_[0]) + length($_[0]) * length($_[0])) . "\n");
              return () };

while (<$in>) {
  chomp;
  eval { $line->(split /\t/) };
  while ($@) {
    $depth = 0;
    eval { $f->(@args) };
  }
}
