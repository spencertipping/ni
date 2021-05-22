# Some utilities to work with common formats.

sub rppm()
{
  # Reads a binary PPM header from the stream, returning a list of descriptors:
  # ($nbytes, $magic, $width, $height, $level).

  my ($magic, $wh, $level) = split/\n/, pb 256;
  die "not a binary PPM magic number: $magic" unless $magic =~ /^P[456]/;

  my ($width, $height) = split/\s+/, $wh;
  if ($magic eq "P4")
  {
    # No level, and one bit per pixel
    my $nbytes = $width * $height + 7 >> 3;
    rb(length($magic) + length($wh) + 2);
    return ($nbytes, $magic, $width, $height, 1);
  }
  else
  {
    my $bytes_per_pixel = ($level <= 255 ? 1 : 2) * ($magic eq "P5" ? 1 : 3);
    my $nbytes          = $bytes_per_pixel * $width * $height;
    rb(length($magic) + length($wh) + length($level) + 3);
    return ($nbytes, $magic, $width, $height, $level);
  }
}
