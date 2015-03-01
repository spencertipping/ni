sub line {
  print join("\t", $_[0], length $_[0]) . "\n";
}

chomp, line split /\t/ while <STDIN>;
