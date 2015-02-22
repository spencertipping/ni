while (<>) {
  chomp;
  print $_, "\t", length($_), "\n";
}
