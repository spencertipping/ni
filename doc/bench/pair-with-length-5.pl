sub f {
  ("$_[0]\t" . length($_[0]) . "\n");
}

while (<>) {
  chomp;
  print for f $_;
}
