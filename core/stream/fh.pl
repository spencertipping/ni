# Filehandle functions.

use Fcntl qw/:DEFAULT/;

sub fh_nonblock($) {
  my ($fh) = @_;
  my $flags = fcntl $fh, F_GETFL, 0       or die "ni: fcntl get $fh: $!";
  fcntl $fh, F_SETFL, $flags | O_NONBLOCK or die "ni: fcntl set $fh: $!";
}

sub fh_block($) {
  my ($fh) = @_;
  my $flags = fcntl $fh, F_GETFL, 0        or die "ni: fcntl get $fh: $!";
  fcntl $fh, F_SETFL, $flags & ~O_NONBLOCK or die "ni: fcntl set $fh: $!";
}
