use B::Deparse;
my $deparser = B::Deparse->new;

for my $f (ni::lisp::parse join '', <>) {
  print STDERR "> $f\n";
  my $m = eval { $f->macroexpand };
  die "error macroexpanding $f: $@" if $@;
  my $c = $m->compile;
  print "\n\n=begin comment\n\n" . $m->pprint(0) . "\n\n=end comment\n\n=cut\n\n";
  print $c =~ s/\h+/ /gr, ";\n";
  my $coderef  = eval "sub{\n$c\n}";
  die "error compiling coderef $c: $@" if $@;
  my $readable = $deparser->coderef2text($coderef);
  my $r = eval $c;
  die "error evaluating compilation for $f -> $m -> $readable: $@" if $@;
  print STDERR "= $r\n";
}
