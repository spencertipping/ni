my $in  = \*STDIN;
my $out = \*STDOUT;

sub line {
  print $out ($_[0] . "\t" . length $_[0] . "\n");
}

chomp, line split /\t/ while <$in>;
