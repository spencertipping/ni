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
              $line->(split /\t/) };

$line = sub { ($f, @args) = ($line, @_), die if ++$depth > 262144;
              $next->(print((length($_[0]) + length($_[0]) * length($_[0])) . "\n")); };

($f, @args) = ($next);
$@ = 1;
until ($exit) {
  $depth = 0;
  eval { ($f, @args) = $f->(@args) };
}
