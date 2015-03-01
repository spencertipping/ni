sub line {
  print join("\t", $_[0], length $_[0]) . "\n";
}

while (<STDIN>) {
  chomp;
  line split /\t/;
}
