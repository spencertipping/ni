my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
  print $out join("\t", substr($_[0], 0, $_[1]), $_[1] - 0) . "\n";
}

chomp, line($_, index($_, "\t")) while <$in>;
