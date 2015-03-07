# Really simple macroexpander
{

package ni::lisp;

%ni::lisp::macros = ();

deftypemethod 'macroexpand',
  list => sub {
    my ($self, $macros) = @_;
    my ($h, @xs) = @$self;
    $macros //= \%ni::lisp::macros;
    print STDERR "macroexpanding $self\n";
    return ni::lisp::list map $_->macroexpand($macros), @$self
      unless ref $h eq 'ni::lisp::symbol' && exists $$macros{$$h};

    # Macros will be in CPS (even though they have no business being written
    # this way), so catch the return value.
    my $return;
    $$macros{$$h}->(@xs, sub { $return = $_[0] });
    $return->macroexpand($macros);
  },

  array  => sub { ni::lisp::array map $_->macroexpand($_[1]), @{$_[0]} },
  hash   => sub { ni::lisp::hash  map $_->macroexpand($_[1]), @{$_[0]} },
  qstr   => sub { $_[0] },
  str    => sub { $_[0] },
  symbol => sub { $_[0] },
  number => sub { $_[0] };

}
