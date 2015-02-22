sub f {
  "$_\t" . length($_) . "\n";
}

while (<>) {
  chomp;
  print f;
}
