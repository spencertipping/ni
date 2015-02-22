while (<STDIN>) {
  chomp;
  print STDOUT "$_\t" . length($_) . "\n";
}
