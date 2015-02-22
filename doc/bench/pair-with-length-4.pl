sub f {
  "$_[0]\t" . length($_[0]) . "\n";
}

while (<>) {
  chomp;
  print f $_;
}
