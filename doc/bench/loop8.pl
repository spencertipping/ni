@xs = 0..2000;
@ys = 0..2000;

for (my $x = 0; $x < @xs; ++$x) {
  for (my $y = 0; $y < @ys; ++$y) {
    print "$ys[$y]\n";
  }
}
