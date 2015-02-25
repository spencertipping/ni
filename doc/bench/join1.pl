while (<>) {
  print join("\t", $_, length($_)) . "\n";
}
