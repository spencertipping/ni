# Operator-level text substitution.
# WARNING: This is a hack, but possibly a useful one.

sub rewrite_atoms_in {
  my ($op, $fn) = @_;
  return $fn->($op) unless ref $op;
  return [map rewrite_atoms_in($_, $fn), @$op] if ref $op eq 'ARRAY';
  return {map rewrite_atoms_in($_, $fn), %$op} if ref $op eq 'HASH';
  die "rewrite_atoms_in: not sure how to rewrite $op of type " . ref($op);
}

defmetaoperator op_let => q{
  my ($args, $left, $right) = @_;
  my ($bindings, $ops) = @$args;
  my @keys = map $$_[0], @$bindings;
  my %replacements = map @$_, @$bindings;
  my $rewritten = rewrite_atoms_in $ops, sub {
    my $a = shift;
    $a =~ s/\Q$_\E/$replacements{$_}/g for @keys;
    $a;
  };
  ($left, [@$rewritten, @$right]);
};

defoperator op_fn => q{
  my ($bindings, $ops) = @_;
  while (<STDIN>) {
    chomp;
    my @vars = split /\t/;
    my %replacements;
    @replacements{@$bindings} = @vars;
    my $rewritten = rewrite_atoms_in $ops, sub {
      my $a = shift;
      $a =~ s/\Q$_\E/$replacements{$_}/g for @$bindings;
      $a;
    };
    close(my $fh = siproc {exec_ni @$rewritten});
    $fh->await;
  }
};

BEGIN {defparseralias fn_bindings => pn 0, prep(prc qr/[^:=]+/), prc qr/:/}
BEGIN {defparseralias let_binding => pn [0, 2], prx qr/[^:=]+/, pstr '=', prc '[\s\S]+'}
BEGIN {defparseralias let_bindings => pn 0, prep(let_binding), prc qr/:/}

defshort '/l[' => pmap q{op_let_op @$_},
                  pn [1, 2], popt pempty, let_bindings, '/series', pstr ']';

defshort '/f[' => pmap q{op_fn_op @$_},
                  pn [1, 2], popt pempty, fn_bindings, '/series', pstr ']';
