my $x = ' ' x 1024;
while (<>) {
  $x = join("\t", $_, length($_)) . "\n";
  print $x;
}
