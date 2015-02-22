my $fh = \*STDOUT;

while (<STDIN>) {
  chomp;
  $fh->print("$_\t" . length($_) . "\n");
}
