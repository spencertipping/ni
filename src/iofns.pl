# Extra IO functions

# Eagerly reads N items, returning (buffer, rest) both as IOs.
sub ni::io::peek {
  my ($self, $n) = @_;
  my $buffer = ni_memory() < $self >> take_binding($n);
  ($buffer, $self);
}

# Takes unique records, optionally counting and selecting by specific fields.
sub ni::io::uniq {
  my ($self, $count, @fields) = @_;
  ni_source_as "$self >> uniq $count @fields", sub {
    my ($destination) = @_;
    # TODO
  };
}
