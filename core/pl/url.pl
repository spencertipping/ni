# Credit to S. Vertigan from this post:
# http://code.activestate.com/recipes/577450-perl-url-encode-and-decode/#c6

sub url_encode($) {
  my ($rv) = @_;
  $rv =~ s/([^A-Za-z0-9])/sprintf("%%%2.2X", ord($1))/ge;
  return $rv;
}

sub url_decode($) {
  my ($rv) = @_;
  $rv =~ s/\+/ /g;
  $rv =~ s/%(..)/pack("c",hex($1))/ge;
  return $rv;
}
