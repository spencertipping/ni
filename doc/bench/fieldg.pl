my $x;
sub f {
  $x = \$_[0];
  my $t = index $$x, "\t", 1 + index($$x, "\t");
  $t = length unless $t >= 0;
  print substr($$x, 0, $t), "\t$t\n";
}
while (<>) {
  f $_;
}
