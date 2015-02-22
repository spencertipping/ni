while (<>) {
  chomp;
  STDOUT->print("$_\t" . length($_) . "\n");
}
