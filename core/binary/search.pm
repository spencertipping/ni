# Binary searching functions, esp for packed values
#
# These functions are useful when you don't have space for a hashtable but want
# associative lookups anyway.

# bsf(packed-string, unpack-template, reclength, target-value) -> recindex
sub bsf
{
  my $packed = \shift;
  my ($unpacker, $reclength, $target) = @_;
  my $upper = length($$packed) / $reclength;
  die "bsf: total length " . length($$packed) . " isn't evenly divided "
    . "by record length $reclength" unless $upper == int $upper;

  for (my ($lower, $mid) = (0, 0);
       $mid = $upper + $lower >> 1, $upper > 1 + $lower;)
  {
    my $midval = unpack $unpacker,
                 substr $$packed, $mid * $reclength, $reclength;
    return $mid   if $target == $midval;
    $upper = $mid if $target <  $midval;
    $lower = $mid if $target >  $midval;
  }
  $upper - 1;
}

# bsflookup(packed, unpacker, reclength, target, valunpacker) -> val...
sub bsflookup
{
  my $index = bsf @_[0..3];
  my $record = substr $_[0], $index * $_[2], $_[2];
  my $key    = unpack $_[1], $record;
  $key == $_[3] ? unpack $_[4], $record : undef;
}
