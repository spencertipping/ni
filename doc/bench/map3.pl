my $in  = \*STDIN;
my $out = \*STDOUT;

# Workflow being encoded is this:
#
# ni data -m '%0, length %0' -f1 -m '%0, %0 * %0' -m '%0 + %1'

sub f1 { $_[0], length $_[0] }
sub f2 { $_[1] }
sub f3 { $_[0], $_[0] * $_[0] }
sub f4 { $_[0] + $_[1] }

sub line {
  @_ = ($_[0], length $_[0]);
  @_ = ($_[1]);
  @_ = ($_[0], $_[0] * $_[0]);
  @_ = ($_[0] + $_[1]);
  print join("\t", @_) . "\n";
}

while (<$in>) {
  chomp;
  line split /\t/;
}
