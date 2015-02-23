@xs = 0..2000;
@ys = 0..2000;

for (0..$#xs) {
  for (0..$#ys) {
    print "$ys[$_]\n";
  }
}
