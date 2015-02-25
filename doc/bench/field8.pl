my @cs;
my $i;
while (<>) {
  $i = @cs = ();
  push @cs, $i until ($i = index($_, "\t", $i) + 1) <= 0;
  print substr($_, 0, $cs[0]), "\t$cs[0]\n";
}
