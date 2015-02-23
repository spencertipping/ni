@xs = 0..2000;
@ys = 0..2000;

goto label if 0;

for (@xs) {
  for (@ys) {
    print "$_\n";
    next;
    label:
      $i = 10;
  }
}
