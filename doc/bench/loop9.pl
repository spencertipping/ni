@xs = 0..2000;
@ys = 0..2000;

my $xi = 0;
for (@xs) {
  ++$xi;
  my $yi = 0;
  for (@ys) {
    ++$yi;
    print "$_\n";
  }
}
