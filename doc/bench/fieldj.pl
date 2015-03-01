my $t;
while (<>) {
  $t = length if ($t = index $_, "\t") == -1;
  print substr($_, 0, $t) . "\t$t\n";
}
