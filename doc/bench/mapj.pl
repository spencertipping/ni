use strict;

my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

my $depth = 0;

my ($f1, $f2, $f3, $f4, $line, $next);
my $f;
my @args;

my $exit = 0;

$next = sub { ($f, @args) = ($next, @_), die if ++$depth > 262144;
              unless (defined($_ = <$in>)) {
                $exit = 1;
                die;
              }
              chomp;
              $f1->(split /\t/) };

$line = sub { ($f, @args) = ($line, @_), die if ++$depth > 262144;
              $next->(print join("\t", @_) . "\n"); };
$f4   = sub { ($f, @args) = ($f4, @_), die if ++$depth > 262144;
              $line->($_[0] + $_[1]) };
$f3   = sub { ($f, @args) = ($f3, @_), die if ++$depth > 262144;
              $f4->($_[0], $_[0] * $_[0]) };
$f2   = sub { ($f, @args) = ($f2, @_), die if ++$depth > 262144;
              $f3->($_[1]) };
$f1   = sub { ($f, @args) = ($f1, @_), die if ++$depth > 262144;
              $f2->($_[0], length $_[0]) };

($f, @args) = ($next);
$@ = 1;
until ($exit) {
  $depth = 0;
  eval { ($f, @args) = $f->(@args) };
}
