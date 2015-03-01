my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

sub f1 { $_[0], length $_[0] }
sub f3 { $_[1], $_[1] * $_[1] }
sub f4 { $_[0] + $_[1] }

sub line { print join("\t", f4(f3(f1(@_)))) . "\n" }

while (<$in>) {
  chomp;
  line split /\t/;
}
