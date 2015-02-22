my $fh = \*STDOUT;

while (<STDIN>) {
  chomp;
  print $fh "$_\t" . length($_) . "\n";
}
