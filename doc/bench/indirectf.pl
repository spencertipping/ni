my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
  print $out join("\t", substr($_[0], 0, $_[1]), $_[1] - 0) . "\n";
}

my $f0;
while (<$in>) {
  chomp;
  $f0 = index $_, "\t";
  $f0 = length $_ if $f0 > length $_;
  line($_, $f0);
}
