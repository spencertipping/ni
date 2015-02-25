my $x = '';
while (<>) {
  $x = join("\t", $_, length($_)) . "\n";
  print $x;
}
