while (<>) {
  my $t = index $_, "\t";
  $t = length unless $t >= 0;
  print substr($_, 0, $t), "\t$t\n";
}
