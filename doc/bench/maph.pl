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
              print join("\t", @_) . "\n";
              return () };
$f4   = sub { ($f, @args) = ($f4, @_), die if ++$depth > 262144;
              $line->($_[0] + $_[1]) };
$f3   = sub { ($f, @args) = ($f3, @_), die if ++$depth > 262144;
              $f4->($_[0], $_[0] * $_[0]) };
$f2   = sub { ($f, @args) = ($f2, @_), die if ++$depth > 262144;
              $f3->($_[1]) };
$f1   = sub { ($f, @args) = ($f1, @_), die if ++$depth > 262144;
              $f2->($_[0], length $_[0]) };

while (<$in>) {
  chomp;
  eval { $f1->(split /\t/) };
  while ($@) {
    $depth = 0;
    eval { $f->(@args) };
  }
}
