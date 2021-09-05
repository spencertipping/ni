# Array processors
sub first  {$_[0]}
sub final  {$_[$#_]}   # `last` is reserved for breaking out of a loop
sub rando  {$_[int(rand($#_ + 1))]}
sub max    {local $_; my $m = pop @_; $m = defined && $_ >  $m ? $_ : $m for @_; $m}
sub min    {local $_; my $m = pop @_; $m = defined && $_ <  $m ? $_ : $m for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = defined && $_ gt $m ? $_ : $m for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = defined && $_ lt $m ? $_ : $m for @_; $m}

sub any(&@) {local $_; my ($f, @xs) = @_; &$f($_) && return 1 for @xs; 0}
sub all(&@) {local $_; my ($f, @xs) = @_; &$f($_) || return 0 for @xs; 1}

sub uniq  {local $_; my(%seen, @xs); $seen{$_}++ or push @xs, $_ for @_; @xs}
sub freqs {local $_; my %fs; ++$fs{$_} for @_; \%fs}

sub union
{
  local $_;
  my (@r, %seen);
  for (@_) { $seen{$_}++ or push @r, $_ for @$_ }
  @r;
}

sub intersect($$)
{
  local $_;
  my %left;
  ++$left{$_} for @{+shift};
  grep exists $left{$_}, @{+shift};
}

sub reduce(&$@) {local $_; my ($f, $x, @xs) = @_; $x = &$f($x, $_) for @xs; $x}
sub reductions(&$@) {
  local $_;
  my ($f, $x, @xs, @ys) = @_;
  push @ys, $x = &$f($x, $_) for @xs;
  @ys;
}

sub deltas {local $_; return () unless @_ > 1; map $_[$_] - $_[$_ - 1], 1..$#_}
sub totals {local $_; my ($x, @xs) = 0; push @xs, $x += $_ for @_; @xs}

sub take($@) {my ($n, @xs) = @_; @xs[0..($n-1)]}
sub drop($@) {my ($n, @xs) = @_; @xs[$n..$#xs]}
sub take_last($@) {my ($n, @xs) = @_; @xs[($#xs-$n)..$#xs]}
sub drop_last($@) {my ($n, @xs) = @_; @xs[0..($#xs-$n-1)]}

sub take_while(&@) {
  local $_;
  my ($f, @xs) = @_; 
  my @out; 
  for (@xs) { if(&$f($_)) {push @out, $_} else {last} }
  @out;
}

sub drop_while(&@) {
  local $_;
  my ($f, @xs) = @_;
  my @out;
  my $count = 0;
  for (@xs) { if(!&$f($_)) {return drop $count, @xs} $count++;}
  ();
} 

sub take_every($$@) {
  my ($every, $start, @r) = @_;
  @r[grep { ($_ - $start) % $every == 0 } 0..$#r];
}

sub take_even(@) { take_every(2, 0, @_); }
sub take_odd(@) { take_every(2, 1, @_); }

sub argmax(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) > $fm;
  }
  $m;
}

sub argmin(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) < $fm;
  }
  $m;
}

# Index of Maximum Element
sub indmax(&@) {
  local $_;
  my ($f, @xs) = @_;
  my $im = 0;
  my $fm = &$f($xs[$im]);
  my $fx;
  for (1..$#xs) {
    ($im, $fm) = ($_, $fx) if ($fx = &$f($xs[$_])) > $fm;
  }
  $im;
}

# Index of Minimum Element
sub indmin(&@) {
  local $_;
  my ($f, @xs) = @_;
  my $im = 0;
  my $fm = &$f($xs[$im]);
  my $fx;
  for (1..$#xs) {
    ($im, $fm) = ($_, $fx) if ($fx = &$f($xs[$_])) < $fm;
  }
  $im;
}

sub most_common(@)
{
  local $_;
  my %freqs;
  ++$freqs{$_} for @_;
  my $most = max values %freqs;
  grep $freqs{$_} == $most, keys %freqs;
}

sub zip {
  my @rs = @_;
  my $min_length = min map {$#{$_}} @rs;
  my @r;
  for my $idx(0..$min_length) {
    push @r, ${$rs[$_]}[$idx] for 0..$#rs;
  }
  @r;
}

sub cart {
  use integer;
  local $_;
  return () unless @_;
  @$_ or return () for @_;
  return map [$_], @{$_[0]} if @_ == 1;
  my @ns = map scalar(@$_), @_;
  map {
    my ($i, $xs) = ($_ - 1, []);
    for (reverse 0..$#ns) {
      unshift @$xs, ${$_[$_]}[$i % $ns[$_]];
      $i /= $ns[$_];
    }
    $xs;
  } 1..prod(@ns);
}

sub clip {
  local $_;
  my ($lower, $upper, @xs) = @_;
  wantarray ? map min($upper, max $lower, $_), @xs
            : min $upper, max $lower, $xs[0];
}

# Array element fetch, with interpolation for non-integer indexes
sub aget_interp(\@$)
{
  my ($xs, $i) = @_;
  $i = clip(0, $#$xs, $i);
  my $l = clip(0, $#$xs - 1, int $i);
  interp($i - $l, $$xs[$l], $$xs[$l + 1]);
}
