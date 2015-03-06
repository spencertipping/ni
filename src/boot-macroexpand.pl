# Really simple macroexpander
{

package ni::lisp;

our %macros;

deftypemethod 'macroexpand',
  list => sub {
    my ($self, $macros) = @_;
    my ($h, @xs) = @$self;
    $macros //= \%macros;
    return ni::lisp::list map $_->macroexpand, @$self
      unless ref $h eq 'ni::lisp::symbol' and exists $$macros{$$h};

    # Macros will be in CPS (even though they have no business being written
    # this way), so catch the return value.
    my $return;
    $$macros{$$h}->(@xs, sub { $return = $_[0] });
    $return->macroexpand;
  },

  array  => sub { ni::lisp::array map $_->macroexpand, @{$_[0]} },
  hash   => sub { ni::lisp::hash  map $_->macroexpand, @{$_[0]} },
  qstr   => sub { $_[0] },
  str    => sub { $_[0] },
  symbol => sub { $_[0] },
  number => sub { $_[0] };

}
