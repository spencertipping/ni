my $f0;
while (<>) {
  my $t = index $_, "\t";
  $t = length unless $t >= 0;
  $f0 = substr($_, 0, $t);
  print $f0, "\t", length($f0), "\n";
}
