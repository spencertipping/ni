# Extra IO functions

# Eagerly reads N items, returning (buffer, rest) both as IOs.
sub ni::io::peek {
  my ($self, $n) = @_;
  my $buffer = ni_memory() < $self >> take_binding($n);
  ($buffer, $self);
}
