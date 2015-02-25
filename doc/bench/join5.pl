my $x = ' ' x 1024;
while (<>) {
  $x = '';
  $x .= $_;
  $x .= "\t";
  $x .= length($_);
  $x .= "\n";
  print $x;
}
