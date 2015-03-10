use B::Deparse;
my $deparser = B::Deparse->new;

if (@ARGV) {
  for my $f (ni::lisp::parse join '', <>) {
    print STDERR "> $f\n";
    my $m = eval { $f->macroexpand };
    die "error macroexpanding " . $f->pprint(0) . "\n-- $@" if $@;
    my $c = ni::lisp::compile $m;
    print "\n\n=begin comment\n\n" . $m->pprint(0)
        . "\n\n=end comment\n\n=cut\n\n" unless -t STDOUT;
    my $coderef = eval "sub{\n$c\n}";
    die "error compiling coderef $c\n-- macroexpansion is "
      . $m->pprint(0) . "\n-- $@" if $@;
    my $readable = $deparser->coderef2text($coderef);
    print "$readable\n" unless -t STDOUT;
    my $r = eval $c;
    die "error evaluating compilation for "
      . $f->pprint(0) . " -> "
      . $m->pprint(0) . " -> $readable\n-- $@" if $@;
    print STDERR "= $r\n";
  }
}

# Start a repl
select((select(STDERR), $|++)[0]);
if (-t STDIN) {
  print STDERR "> ";
  while (<STDIN>) {
    for my $f (ni::lisp::parse $_) {
      # Assume anything the user types in needs to be CPS-converted.
      my $m = eval { ni::lisp::list(ni::lisp::symbol('cps*'), $f)->macroexpand };
      die "error macroexpanding " . $f->pprint(0) . "\n-- $@" if $@;
      my $c = ni::lisp::compile $m;
      my $coderef = eval "sub{\n$c\n}";
      die "error compiling coderef $c\n-- macroexpansion is "
        . $m->pprint(0) . "\n-- $@" if $@;
      my $readable = $deparser->coderef2text($coderef);
      my $r = eval $c;
      die "error evaluating compilation for "
        . $f->pprint(0) . " -> "
        . $m->pprint(0) . " -> $readable\n-- $@" if $@;
      print STDERR "= $r\n";

      print STDERR "> ";
    }
  }
}
