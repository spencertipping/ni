@xs = 0..2000;
@ys = 0..2000;

goto label if 0;

for (@xs) {
  for (@ys) {
    label:
    print "$_\n";
  }
}
