@xs = 0..2000;
@ys = 0..2000;

for (0..$#xs) {
  for (@ys) {
    print "$_\n";
  }
}
