use List::Util qw/max/;
while (<>) {
  my $t = max length($_), index($_, "\t", 1 + index($_, "\t"));
  print substr($_, 0, $t), "\t$t\n";
}
