# Extra IO functions

# Eagerly reads N items, returning (buffer, rest) both as IOs.
sub ni::io::peek {
  my ($self, $n) = @_;
  my $buffer = ni_memory();
  ($buffer < $self->bind(take_binding($n)), $self);
}
