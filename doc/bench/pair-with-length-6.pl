eval q{
while (<>) {
  chomp;
  print "$_\t" . length($_) . "\n";
}
};
die $@ if $@;
