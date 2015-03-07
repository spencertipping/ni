use B::Deparse;
my $deparser = B::Deparse->new;

for my $f (ni::lisp::parse join '', <>) {
  my $m = eval { $f->macroexpand };
  die "error macroexpanding $f: $@" if $@;

  my $c        = $m->compile;
  my $readable = $deparser->coderef2text(eval "sub{\n$c\n}");
  print STDERR "$f -> $m -> $readable\n";
  my $r = eval $c;
  die "error evaluating compilation for $f -> $m -> $readable: $@" if $@;
  print STDERR "> $r\n";
}
