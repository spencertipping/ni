# Binary byte stream driver.
# Functions that read data in blocks. The lookahead is 8192 bytes by default, but
# you can peek further using the 'pb' function.

our $stdin_ok = 1;
our $offset = 0;
our $binary = '';

sub bi() {$offset}

sub pb($) {
  use bytes;
  $stdin_ok &&= sysread STDIN, $binary, $_[0], length $binary
    while $stdin_ok && length($binary) < $_[0];
  substr $binary, 0, $_[0];
}

sub available() {length pb 1}

# TODO: optimize
sub rb($) {
  use bytes;
  my $r = pb $_[0];
  $binary = substr $binary, $_[0];
  $offset += $_[0];
  $r;
}

sub rp($) {unpack $_[0], rb length pack $_[0], unpack $_[0], pb 8192}
