# minhash sets
# These are mutable arrays of 32-bit slices of MD5s.

BEGIN {eval {require Digest::MD5}}

sub minhash_new($) { [map 0xffffffff, 1..$_[0]] }
sub minhash_add
{
  my $minhash = shift;
  @$minhash =
    (sort {$a <=> $b} @$minhash, grep $_ < $$minhash[-1],
                                 map unpack('N', Digest::MD5::md5($_)), @_)
    [0..$#$minhash];
  $minhash;
}

sub minhash_count($)
{
  my ($minhash) = @_;
  my $i = $#$minhash;
  --$i while $i && $$minhash[$i] == 0xffffffff;
  return -1 if $$minhash[$i] == 0;
  0xffffffff * ($i + 1) / $$minhash[$i];
}

sub minhash_union { [(sort {$a <=> $b} map @$_, @_)[0..$#{$_[0]}]] }
sub minhash_intersect
{
  my %m;
  for my $i (0..$#_) { $m{$_} |= 1 << $i for @{$_[$i]} }
  my $all = (1 << @_) - 1;
  my @i   = grep $m{$_} == $all, keys %m;
  return minhash_new @{$_[0]} unless @i;
  push @i, (0xffffffff) x (@{$_[0]} - @i) if @{$_[0]} > @i;
  [(sort {$a <=> $b} @i)[0..$#{$_[0]}]];
}
