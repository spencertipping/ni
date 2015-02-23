while (<>) {
  my $t = index $_, "\t";
  print substr($_, 0, $t), "\t$t\n";
}
