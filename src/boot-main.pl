use B::Deparse;
my $deparser = B::Deparse->new;

for my $f (ni::lisp::parse join '', <>) {
  print STDERR "> $f\n";
  my $m = eval { $f->macroexpand };
  die "error macroexpanding " . $f->pprint(0) . "\n-- $@" if $@;
  my $c = ni::lisp::compile $m;
  print "\n\n=begin comment\n\n" . $m->pprint(0) . "\n\n=end comment\n\n=cut\n\n";
  my $coderef = eval "sub{\n$c\n}";
  die "error compiling coderef $c\n-- macroexpansion is "
    . $m->pprint(0) . "\n-- $@" if $@;
  my $readable = $deparser->coderef2text($coderef);
  print "$readable\n";
  my $r = eval $c;
  die "error evaluating compilation for "
    . $f->pprint(0) . " -> "
    . $m->pprint(0) . " -> $readable\n-- $@" if $@;
  print STDERR "= $r\n";
}
