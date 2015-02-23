@xs = 0..2000;
@ys = 0..2000;

for (@xs) {
  my $x = $_;
  for (@ys) {
    print "$_\n";
  }
}
