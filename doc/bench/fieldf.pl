sub f {
  my $t = index $_[0], "\t", 1 + index($_[0], "\t");
  $t = length unless $t >= 0;
  print substr($_[0], 0, $t), "\t$t\n";
}
while (<>) {
  f $_;
}
