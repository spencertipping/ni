# Math utility functions.
# Mostly geometric and statistical stuff.

use constant tau => 2 * 3.14159265358979323846264;
use constant tau2 => tau/2;
use constant tau4 => tau/4;

use constant LOG2  => log 2;
use constant LOG2R => 1 / LOG2;

sub sum  {local $_; my $s = 0; $s += $_ for @_; $s}
sub prod {local $_; my $p = 1; $p *= $_ for @_; $p}
sub mean {scalar @_ && sum(@_) / @_;}
sub median {my $length = scalar @_; my @sorted = sort {$a <=> $b} @_; $sorted[int($length/2)];}
sub gmean {exp mean map {log $_} @_;}
sub hmean {scalar @_ && @_/sum(map {1/$_} @_) or 1;}
 
sub log2($) {LOG2R * log $_[0]}

sub entropy {
  local $_;
  my $sum = sum @_;
  my $t = 0;
  $t += $_ / $sum * ($_ > 0 ? -log2($_ / $sum) : 0) for @_;
  $t;
}

sub quant {my ($x, $q) = @_; $q ||= 1;
           my $s = $x < 0 ? -1 : 1; int(abs($x) / $q + 0.5) * $q * $s}

sub interp {
  my $f = shift;
  my @r;
  while (@_) {
    push @r, $_[0] * (1 - $f) + $_[1] * $f;
    shift; shift;
  }
  @r;
}

## Vector Functions
sub proj($$)
{ local $_; my ($a, $b) = @_;
  my $f = dot($a, $b) / dot($b, $b);
  map $f * $_, @$b }

sub orth($$)
{ local $_; my ($a, $b) = @_;
  my @proj = proj $a, $b;
  map $$a[$_] - $proj[$_], 0..$#{$a} }

sub cross($$)
{ my ($x1, $y1, $z1, $x2, $y2, $z2) = (@{$_[0]}, @{$_[1]});
  ($y1*$z2 - $z1*$y2, $z1*$x2 - $x1*$z2, $x1*$y2 - $y1*$x2) }


sub dot($$) {local $_; my ($u, $v) = @_;
             sum map $$u[$_] * $$v[$_], 0..min $#{$u}, $#{$v}}

sub l1norm {local $_; sum map abs($_), @_}
sub l2norm {local $_; sqrt sum map $_*$_, @_}

sub vec_sum($$) {
  local $_; my ($u, $v) = @_;
  map $$u[$_] + $$v[$_], 0..$#u;
}

sub vec_diff($$) {
  local $_; my ($u, $v) = @_;
  map $$u[$_] - $$v[$_], 0..$#u;
}

sub distance_to_line($$$) {
  local $_;
  my ($a, $l, $p) = @_;
  my @n = vec_diff($a, $l);
  my @d = vec_diff($a, $p);
  
  l2norm orth(\@d, \@n);
}


## Trig Functions
sub rdeg($) {$_[0] * 360 / tau}
sub drad($) {$_[0] / 360 * tau}

sub prec {($_[0] * sin drad $_[1], $_[0] * cos drad $_[1])}
sub rpol {(l2norm(@_), rdeg atan2($_[0], $_[1]))}

# Numpy synonyms and extensions
*radians = \&drad;
*degrees = \&rdeg;

sub linspace($$$) {
  my $n_spaces = $_[2] - 1;
  map {$_[0] + $_/$n_spaces *($_[1] - $_[0])} 0..$n_spaces;
}

sub arange($$$) {
  my $n_spaces = int abs ($_[1] - $_[0])/$_[2];
  linspace $_[0], $_[0] + $_[2]*$n_spaces, $n_spaces;
}

sub aspace($$$) {
  linspace $_[0], $_[1], 1 + abs int(($_[1] - $_[0])/$_[2]);
}

sub logspace($$$;$) {
  my @powers = linspace($_[0], $_[1], $_[2]);
  my $base = defined $_[3] && $_[3] or 10; 
  map {$base ** $_} @powers;
}



BEGIN {
if (eval {require Math::Trig}) {
  Math::Trig->import('!sec');   # sec() conflicts with stream reducers
  sub haversine {
    local $_;
    my ($t1, $p1, $t2, $p2) = map drad $_, @_;
    my ($dt, $dp) = ($t2 - $t1, $p2 - $p1);
    my $a = clip 0, 1, sin($dp / 2)**2 + cos($p1) * cos($p2) * sin($dt / 2)**2;
    2 * atan2(sqrt($a), sqrt(1 - $a));
  }
}
}
