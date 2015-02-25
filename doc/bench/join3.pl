my $x = '';
my @xs;
while (<>) {
  @xs = ($_, length($_));
  $x = shift @xs;
  $x .= "\t" . $_ for @xs;
  print $x . "\n";
}
