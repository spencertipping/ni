# Really simple macroexpander
{

package ni::lisp;

%ni::lisp::macros = ();

deftypemethod 'macroexpand',
  list => sub {
    my ($self, $macros) = @_;
    my ($h, @xs) = @$self;
    $macros //= \%ni::lisp::macros;

    unless (ref $h eq 'ni::lisp::symbol' && exists $$macros{$$h}) {
      my @macroexpanded = eval { map $_->macroexpand($macros), @$self };
      die "failed to macroexpand $self: $@" if $@;
      ni::lisp::list @macroexpanded;
    } else {
      my $return;
      $$macros{$$h}->(sub { $return = $_[0] }, @xs);
      $return->macroexpand($macros);
    }
  },

  array  => sub { ni::lisp::array map $_->macroexpand($_[1]), @{$_[0]} },
  hash   => sub { ni::lisp::hash  map $_->macroexpand($_[1]), @{$_[0]} },
  qstr   => sub { $_[0] },
  str    => sub { $_[0] },
  symbol => sub { $_[0] },
  number => sub { $_[0] };

}
