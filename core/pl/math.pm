# Math utility functions.
# Mostly geometric and statistical stuff.

use constant tau => 2 * 3.14159265358979323846264;
use constant tau2 => tau/2;
use constant tau4 => tau/4;

use constant LOG2  => log 2;
use constant LOG2R => 1 / LOG2;

sub sum  {local $_; my $s = 0; $s += $_ for @_; $s}
sub prod {local $_; my $p = 1; $p *= $_ for @_; $p}
sub mean {scalar @_ && sum(@_) / @_}

sub log2($) {LOG2R * log $_[0]}
sub quant {my ($x, $q) = @_; $q ||= 1;
           my $s = $x < 0 ? -1 : 1; int(abs($x) / $q + 0.5) * $q * $s}

sub dot($$) {local $_; my ($u, $v) = @_;
             sum map $$u[$_] * $$v[$_], 0..min $#{$u}, $#{$v}}

sub l1norm {local $_; sum map abs($_), @_}
sub l2norm {local $_; sqrt sum map $_*$_, @_}

sub proj($$)
{ local $_; my ($a, $b) = @_;
  my $f = dot($a, $b) / dot($b, $b);
  map $f * $_, @$b }

sub orth($$)
{ local $_; my ($a, $b) = @_;
  my $proj = proj $a, $b;
  map $$a[$_] - $$proj[$_], 0..$#{$a} }

sub cross($$)
{ my ($x1, $y1, $z1, $x2, $y2, $z2) = (@{$_[0]}, @{$_[1]});
  ($y1*$z2 - $z1*$y2, $z1*$x2 - $x1*$z2, $x1*$y2 - $y1*$x2) }

sub rdeg($) {$_[0] * 360 / tau}
sub drad($) {$_[0] / 360 * tau}

sub prec {($_[0] * sin drad $_[1], $_[0] * cos drad $_[1])}
sub rpol {(l2norm(@_), rdeg atan2($_[0], $_[1]))}

sub entropy {
  local $_;
  my $sum = sum @_;
  my $t = 0;
  $t += $_ / $sum * ($_ > 0 ? -log2($_ / $sum) : 0) for @_;
  $t;
}

if (eval {require Math::Trig}) {
  sub haversine {
    local $_;
    my ($t1, $p1, $t2, $p2) = map drad $_, @_;
    my ($dt, $dp) = ($t2 - $t1, $p2 - $p1);
    my $a = clip 0, 1, sin($dp / 2)**2 + cos($p1) * cos($p2) * sin($dt / 2)**2;
    2 * atan2(sqrt($a), sqrt(1 - $a));
  }
}
