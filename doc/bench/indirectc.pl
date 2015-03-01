my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
  print $out join("\t", $_[0], length $_[0]) . "\n";
}

chomp, line((split /\t/)[0]) while <$in>;
