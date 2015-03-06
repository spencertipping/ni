for my $f (ni::lisp::parse join '', <>) {
  my $m = eval { $f->macroexpand };
  die "error macroexpanding $f: $@" if $@;

  my $c = $m->compile;
  eval $m->compile;
  die "error evaluating compilation for $f -> $m -> $c: $@" if $@;
}
