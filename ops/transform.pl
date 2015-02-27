defop 'plus', undef, '',
  'adds two streams together (implied for files)',
  sub { $_[0] + $_[1] };

defop 'tee', undef, 's',
  'tees current output into the specified io',
  sub { $_[0] >>= tee_binding(ni $_[1]) };

defop 'take', undef, 'd',
  'takes the first or last N records from the specified io',
  sub { $_[1] > 0 ? $_[0] >>= take_binding($_[1])
                  : ni_ring(-$_[1]) < $_[0] };

defop 'drop', undef, 'd',
  'drops the first or last N records from the specified io',
  sub {
    my ($self, $n) = @_;
    $n >= 0
      ? $self->bind(drop_binding($n))
      : ni_source_as("$self >>= drop " . -$n . "]", sub {
          my ($destination) = @_;
          $self->source_gen(ni_ring(-$n, $destination));
        });
  };

defop 'zip', 'z', 's',
  'zips lines together with those from the specified IO',
  sub { $_[0] >>= zip_binding(ni $_[1]) };
