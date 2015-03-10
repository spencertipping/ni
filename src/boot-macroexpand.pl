# Really simple macroexpander
{

package ni::lisp;

%ni::lisp::macros = ();

deftypemethod 'macroexpand',
  list => sub {
    my ($self, $macros) = @_;
    my ($h, @xs) = @$self;
    $macros //= \%ni::lisp::macros;

    my $result = eval {
      if (ref $h eq 'ni::lisp::symbol' && exists $$macros{$$h}) {
        my $return;
        my $invoked = 0;
        $$macros{$$h}->(sub { ++$invoked; $return = $_[0] }, @xs);
        die "failed to macroexpand "
          . $self->pprint(0)
          . "\n-- macro '$$h' failed to invoke its return continuation"
          unless $invoked;

        die "failed to macroexpand "
          . $self->pprint(0)
          . "\n-- macro '$$h' invoked its return continuation with undef"
          unless defined $return;

        my $result = eval { $return->macroexpand($macros) };
        die "failed to sub-macroexpand " . $return->pprint(0) . "\n-- $@" if $@;
        $result;
      } else {
        # Easy case: sub-macroexpand each list element.
        ni::lisp::list map $_->macroexpand($macros), @$self;
      }
    };

    die "failed to macroexpand " . $self->pprint(0) . "\n-- $@" if $@;
    $result;
  },

  array  => sub { ni::lisp::array map $_->macroexpand($_[1]), @{$_[0]} },
  hash   => sub { ni::lisp::hash  map $_->macroexpand($_[1]),
                                      @{$_[0]->sequential} },
  str    => sub { $_[0] },
  symbol => sub { $_[0] },
  number => sub { $_[0] };

}
