#!/usr/bin/env perl
chomp($ni::license=<<'_');
ni: https://github.com/spencertipping/ni
Copyright (c) 2016-2017 Spencer Tipping

MIT license

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
_
BEGIN{eval($ni::boot=<<'_')}
use strict;
use warnings;
no warnings 'redefine';
no strict 'refs';
use Scalar::Util;
chomp $ni::boot;
$ni::self = bless {}, '/class/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::named{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::name {my %h = @_; @ni::named{keys %h} = values %h}
sub ni::eval {eval shift}
*{'/class/ni::def'} = sub {
  my ($self, %kvs) = @_;
  *$_ = $ni::named{"ni.def:$_"} = $kvs{$_} for keys %kvs;
};
*{'/class/fn::new'} = sub {
  my $self = bless {code => $_[1]}, $_[0];
  $self->compile;
  $self;
};
*{'/class/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/class/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
*{'/class/fn::(('}    = sub {};
*{'/class/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'/class/fn::(bool'} = sub {1};
sub fn($) {'/class/fn'->new(shift)}
_
$c=q'ni:/class/image';$d=q'applied_to';$e=q'/class/class';$f={$e,1};$g='name';$h='slices';$i=q'/class/behavior';$j=q'/class/slice';$k={$i,1,$e,1,$j,1};$l={$j,1};$m=undef;$n=q'/class/fn';$o={$n,1};$p={};$q='ctor';$r='code';$s=q'shift->compile';$t=bless({$r,$s},$n);$u=q'/class/fn::ctors';$v='dtor';$w='isa';$x=[];$y='methods';$z='compile';$A=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/behavior/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$B=bless({$r,$A},$n);$C=q'/class/fn::ctors';$D=q'instantiate';$E=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$F=bless({$r,$E},$n);$G=q'/class/fn::ctors';$H={$z,$B,$D,$F};$I=q'/behavior/fn_init';$J=bless({$d,$p,$q,$t,$v,$m,$w,$x,$y,$H,$g,$I},$j);$K=q'/class/slice::ctors';$L={};$M=[];$N=q'serialize';$O=q'annotations';$P=[];$Q=q'local $_;
my ($self, $quote) = @_;
$quote->quote_class(ref $self);

(my $code = $$self{code}) =~ s/^\\s*\\n|\\s*$//g;
my @lines = split /\\n/, $code;
my $spaces = length $code;
for (@lines) {
  $spaces = length $1 if /^(\\h*)\\S/ && length $1 < $spaces;
}
$spaces = \' \' x $spaces;
s/^$spaces// for @lines;

my %state = %$self;
delete $state{fn};
$state{code} = join "\\n", @lines;
$quote->quote_blessed(\\%state, ref $self);';$R=bless({$O,$P,$r,$Q},$n);$S=q'/class/fn::ctors';$T={$N,$R};$U=bless({$d,$L,$q,$m,$v,$m,$w,$M,$y,$T,$g,$m},$j);$V=q'/class/slice::ctors';$W=[$m,$J,$U];$X=bless({$d,$o,$g,$n,$h,$W},$e);$Y=q'/class/class::ctors';$Z=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$c1=bless({$r,$Z},$n);$d1=q'/class/fn::ctors';$e1=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{isa}};
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$f1=bless({$r,$e1},$n);$g1=q'/class/fn::ctors';$h1=q'/behavior/slice::apply';$i1=q'/behavior/slice::apply_unsafe';$j1=q'/class/slice::apply';$k1=q'/class/slice::apply_unsafe';$l1={};$m1=[];$n1='apply';$o1=q'apply_unsafe';$p1={$n1,$c1,$o1,$f1};$q1=q'/behavior/slice';$r1=bless({$d,$l1,$q,$m,$v,$m,$w,$m1,$y,$p1,$g,$q1},$j);$s1=q'/class/slice::ctors';$t1={};$u1=[];$v1=q'my ($class, $name, @methods) = @_;
my $self = &{\'/behavior/slice::instantiate\'}($class, @methods);
$$self{name} = $name;
$self;';$w1=bless({$r,$v1},$n);$x1=q'/class/fn::ctors';$y1={$D,$w1};$z1=q'/behavior/slice_named_init';$A1=bless({$d,$t1,$q,$m,$v,$m,$w,$u1,$y,$y1,$g,$z1},$j);$B1=q'/class/slice::ctors';$C1={};$D1=[];$E1=[];$F1=q'local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq \'ni:/behavior/slice\') {
  my %methods;
  my @ks = sort keys %{$$self{methods}};
  @methods{@ks} = map $quote->quote($_), @{$$self{methods}}{@ks};
  for my $p (sort keys %{$$self{applied_to}}) {
    $quote->boot_side_effect(
      \'*\' . $quote->quote("$p\\::$_") . "=\\\\\\&$methods{$_};")
      for @ks;
  }
}

my $g = $quote->allocate_gensym($self,
  $quote->quote_blessed({%$self, applied_to => {}}, ref $self));
$quote->side_effect("$g\\->apply_unsafe(" . $quote->quote($_) . ");")
  for sort keys %{$$self{applied_to}};
$g;';$G1=bless({$O,$E1,$r,$F1},$n);$H1=q'/class/fn::ctors';$I1={$N,$G1};$J1=bless({$d,$C1,$q,$m,$v,$m,$w,$D1,$y,$I1,$g,$m},$j);$K1=q'/class/slice::ctors';$L1=[$m,$r1,$A1,$J1];$M1=bless({$d,$l,$g,$j,$h,$L1},$e);$N1=q'/class/class::ctors';$O1={};$P1=[];$Q1='DESTROY';$R1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$S1=bless({$r,$R1},$n);$T1=q'/class/fn::ctors';$U1='new';$V1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$W1=bless({$r,$V1},$n);$X1=q'/class/fn::ctors';$Y1={$Q1,$S1,$U1,$W1};$Z1=q'/behavior/lifecycle';$c2=bless({$d,$O1,$q,$m,$v,$m,$w,$P1,$y,$Y1,$g,$Z1},$j);$d2=q'/class/slice::ctors';$e2={};$f2=[];$g2='package';$h2=q'shift->{name}';$i2=bless({$r,$h2},$n);$j2=q'/class/fn::ctors';$k2={$g2,$i2};$l2=q'/behavior/mapped_to_package';$m2=bless({$d,$e2,$q,$m,$v,$m,$w,$f2,$y,$k2,$g,$l2},$j);$n2=q'/class/slice::ctors';$o2={};$p2=[];$q2=q'namespace';$r2='\'ni\'';$s2=bless({$r,$r2},$n);$t2=q'/class/fn::ctors';$u2={$q2,$s2};$v2=q'/behavior/ni_namespaced';$w2=bless({$d,$o2,$q,$m,$v,$m,$w,$p2,$y,$u2,$g,$v2},$j);$x2=q'/class/slice::ctors';$y2={};$z2=q'my $self = shift; ni::name($self->name, $self) if defined $self->name';$A2=bless({$r,$z2},$n);$B2=q'/class/fn::ctors';$C2={};$D2=[];$E2=q'my $s = shift;
defined $$s{name} ? $s->namespace . ":$$s{name}" : undef;';$F2=bless({$r,$E2},$n);$G2=q'/class/fn::ctors';$H2={$g,$F2};$I2=q'/behavior/named';$J2=bless({$d,$C2,$q,$m,$v,$m,$w,$D2,$y,$H2,$g,$I2},$j);$K2=q'/class/slice::ctors';$L2=[$J2];$M2={};$N2=q'/behavior/named_persistent';$O2=bless({$d,$y2,$q,$A2,$v,$m,$w,$L2,$y,$M2,$g,$N2},$j);$P2=q'/class/slice::ctors';$Q2=[$c2,$m2,$w2,$O2];$R2=bless({$d,$k,$g,$i,$h,$Q2},$e);$S2=q'/class/class::ctors';$T2={};$U2=[];$V2='add';$W2=q'local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @_}
$self;';$X2=bless({$r,$W2},$n);$Y2=q'/class/fn::ctors';$Z2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{slices}};
$self;';$c3=bless({$r,$Z2},$n);$d3=q'/class/fn::ctors';$e3={$V2,$X2,$n1,$c3};$f3=q'/behavior/branch';$g3=bless({$d,$T2,$q,$m,$v,$m,$w,$U2,$y,$e3,$g,$f3},$j);$h3=q'/class/slice::ctors';$i3={};$j3=[];$k3='def';$l3=q'shift->add(ni(\'ni:/class/slice\')->new(undef, @_))';$m3=bless({$r,$l3},$n);$n3=q'/class/fn::ctors';$o3={$k3,$m3};$p3=q'/behavior/class_method_def';$q3=bless({$d,$i3,$q,$m,$v,$m,$w,$j3,$y,$o3,$g,$p3},$j);$r3=q'/class/slice::ctors';$s3=[$R2,$g3,$q3];$t3=bless({$d,$f,$g,$e,$h,$s3},$e);$u3=q'/class/class::ctors';$v3=q'/class/image';$w3={$v3,1};$x3={};$y3=[];$z3=[];$A3=q'my $class = shift;
my %args  = (
  include_shebang => 1,
  include_license => 1,
  include_boot    => 1,
  include_classes => 1,
  local_vars      => 0,
  use_newlines    => 0,
  @_);

+{include_shebang => $args{include_shebang},
  include_license => $args{include_license},
  include_boot    => $args{include_boot},
  include_classes => $args{include_classes},
  local_vars      => $args{local_vars},
  use_newlines    => $args{use_newlines},

  gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};';$B3=bless({$O,$z3,$r,$A3},$n);$C3=q'/class/fn::ctors';$D3={$D,$B3};$E3=q'/behavior/image_init';$F3=bless({$d,$x3,$q,$m,$v,$m,$w,$y3,$y,$D3,$g,$E3},$j);$G3=q'/class/slice::ctors';$H3={};$I3=[];$J3='address';$K3=[];$L3=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$M3=bless({$O,$K3,$r,$L3},$n);$N3=q'/class/fn::ctors';$O3=q'allocate_gensym';$P3=[];$Q3=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$R3=bless({$O,$P3,$r,$Q3},$n);$S3=q'/class/fn::ctors';$T3=q'boot_side_effect';$U3=[];$V3=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$W3=bless({$O,$U3,$r,$V3},$n);$X3=q'/class/fn::ctors';$Y3=q'circular_links';$Z3=[];$c4=q'local $_;
my $self = shift;
map "$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$d4=bless({$O,$Z3,$r,$c4},$n);$e4=q'/class/fn::ctors';$f4=q'finalizer';$g4=[];$h4=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$i4=bless({$O,$g4,$r,$h4},$n);$j4=q'/class/fn::ctors';$k4='gensym';$l4=[];$m4=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$n4=bless({$O,$l4,$r,$m4},$n);$o4=q'/class/fn::ctors';$p4=q'is_circular';$q4=[];$r4=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$s4=bless({$O,$q4,$r,$r4},$n);$t4=q'/class/fn::ctors';$u4=q'partial_image';$v4=[];$w4=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\@ni::named{keys\\%$g}=values\\%$g;");';$x4=bless({$O,$v4,$r,$w4},$n);$y4=q'/class/fn::ctors';$z4='quote';$A4=[];$B4=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$C4=bless({$O,$A4,$r,$B4},$n);$D4=q'/class/fn::ctors';$E4=q'quote_array';$F4=[];$G4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$H4=bless({$O,$F4,$r,$G4},$n);$I4=q'/class/fn::ctors';$J4=q'quote_blessed';$K4=[];$L4=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$M4=bless({$O,$K4,$r,$L4},$n);$N4=q'/class/fn::ctors';$O4=q'quote_class';$P4=[];$Q4=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && exists $ni::named{"ni:$class"};';$R4=bless({$O,$P4,$r,$Q4},$n);$S4=q'/class/fn::ctors';$T4=q'quote_hash';$U4=[];$V4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  push @{$$self{circular}}, [$a, "{" . $self->quote($k) . "}",
                                 $self->address($$v{$k})]
  if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
\'{\' . join(",", @qs) . \'}\';';$W4=bless({$O,$U4,$r,$V4},$n);$X4=q'/class/fn::ctors';$Y4=q'quote_object';$Z4=[];$c5=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$d5=bless({$O,$Z4,$r,$c5},$n);$e5=q'/class/fn::ctors';$f5=q'quote_scalar';$g5=[];$h5=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
length $v > 8 ? "q\'$v\'" : "\'$v\'";';$i5=bless({$O,$g5,$r,$h5},$n);$j5=q'/class/fn::ctors';$k5=q'quote_value';$l5=[];$m5=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$n5=bless({$O,$l5,$r,$m5},$n);$o5=q'/class/fn::ctors';$p5=q'reconstruction';$q5=[];$r5=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$s5=bless({$O,$q5,$r,$r5},$n);$t5=q'/class/fn::ctors';$u5=q'side_effect';$v5=[];$w5=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$x5=bless({$O,$v5,$r,$w5},$n);$y5=q'/class/fn::ctors';$z5='write';$A5=[];$B5=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n";';$C5=bless({$O,$A5,$r,$B5},$n);$D5=q'/class/fn::ctors';$E5={$J3,$M3,$O3,$R3,$T3,$W3,$Y3,$d4,$f4,$i4,$k4,$n4,$p4,$s4,$u4,$x4,$z4,$C4,$E4,$H4,$J4,$M4,$O4,$R4,$T4,$W4,$Y4,$d5,$f5,$i5,$k5,$n5,$p5,$s5,$u5,$x5,$z5,$C5};$F5=q'/behavior/image_quoting';$G5=bless({$d,$H3,$q,$m,$v,$m,$w,$I3,$y,$E5,$g,$F5},$j);$H5=q'/class/slice::ctors';$I5=[$F3,$G5];$J5=bless({$d,$w3,$g,$v3,$h,$I5},$e);$K5=q'/class/class::ctors';$L5=q'ni:/class/ni';$M5=q'/class/ni';$N5={$M5,1};$O5={};$P5=[];$Q5='defclass';$R5=q'shift; ni(\'ni:/class/class\')->new("/class/$_[0]", @_[1..$#_])';$S5=bless({$r,$R5},$n);$T5=q'/class/fn::ctors';$U5='defslice';$V5=q'shift; ni(\'ni:/class/slice\')->new("/behavior/$_[0]", @_[1..$#_])';$W5=bless({$r,$V5},$n);$X5=q'/class/fn::ctors';$Y5={$Q5,$S5,$U5,$W5};$Z5=bless({$d,$O5,$q,$m,$v,$m,$w,$P5,$y,$Y5,$g,$m},$j);$c6=q'/class/slice::ctors';$d6={};$e6=[];$f6=q'internal/eval';$g6=[];$h6=q'my $self = shift;
for (@_) {
  print "$_ -> ";
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$i6=bless({$O,$g6,$r,$h6},$n);$j6=q'/class/fn::ctors';$k6=q'internal/image';$l6=[];$m6=q'my $self = shift;
my $q = ni(\'ni:/class/image\')->new
  ->partial_image(\'ni:/class/ni\', \'ni:/class/image\');
$q->write(\\*STDOUT);
0;';$n6=bless({$O,$l6,$r,$m6},$n);$o6=q'/class/fn::ctors';$p6='run';$q6=[];$r6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';$s6=bless({$O,$q6,$r,$r6},$n);$t6=q'/class/fn::ctors';$u6={$f6,$i6,$k6,$n6,$p6,$s6};$v6=bless({$d,$d6,$q,$m,$v,$m,$w,$e6,$y,$u6,$g,$m},$j);$w6=q'/class/slice::ctors';$x6=[$Z5,$v6];$y6=bless({$d,$N5,$g,$M5,$h,$x6},$e);$z6=q'/class/class::ctors';$A6={$c,$J5,$L5,$y6};*$k1=\&$f1;*$j1=\&$c1;*$i1=\&$f1;*$h1=\&$c1;$J->apply_unsafe($n);$U->apply_unsafe($n);$r1->apply_unsafe($q1);$r1->apply_unsafe($j);$A1->apply_unsafe($j);$J1->apply_unsafe($j);$c2->apply_unsafe($q1);$c2->apply_unsafe($i);$c2->apply_unsafe($e);$c2->apply_unsafe($n);$c2->apply_unsafe($j);$m2->apply_unsafe($i);$m2->apply_unsafe($e);$m2->apply_unsafe($j);$w2->apply_unsafe($i);$w2->apply_unsafe($e);$w2->apply_unsafe($j);$J2->apply_unsafe($i);$J2->apply_unsafe($e);$J2->apply_unsafe($j);$O2->apply_unsafe($i);$O2->apply_unsafe($e);$O2->apply_unsafe($j);$g3->apply_unsafe($e);$q3->apply_unsafe($e);$F3->apply_unsafe($v3);$G5->apply_unsafe($v3);$Z5->apply_unsafe($M5);$v6->apply_unsafe($M5);@ni::named{keys%$A6}=values%$A6;&$_($t)for@$u;&$_($B)for@$C;&$_($F)for@$G;&$_($J)for@$K;&$_($R)for@$S;&$_($U)for@$V;&$_($X)for@$Y;&$_($c1)for@$d1;&$_($f1)for@$g1;&$_($r1)for@$s1;&$_($w1)for@$x1;&$_($A1)for@$B1;&$_($G1)for@$H1;&$_($J1)for@$K1;&$_($M1)for@$N1;&$_($S1)for@$T1;&$_($W1)for@$X1;&$_($c2)for@$d2;&$_($i2)for@$j2;&$_($m2)for@$n2;&$_($s2)for@$t2;&$_($w2)for@$x2;&$_($A2)for@$B2;&$_($F2)for@$G2;&$_($J2)for@$K2;&$_($O2)for@$P2;&$_($R2)for@$S2;&$_($X2)for@$Y2;&$_($c3)for@$d3;&$_($g3)for@$h3;&$_($m3)for@$n3;&$_($q3)for@$r3;&$_($t3)for@$u3;&$_($B3)for@$C3;&$_($F3)for@$G3;&$_($M3)for@$N3;&$_($R3)for@$S3;&$_($W3)for@$X3;&$_($d4)for@$e4;&$_($i4)for@$j4;&$_($n4)for@$o4;&$_($s4)for@$t4;&$_($x4)for@$y4;&$_($C4)for@$D4;&$_($H4)for@$I4;&$_($M4)for@$N4;&$_($R4)for@$S4;&$_($W4)for@$X4;&$_($d5)for@$e5;&$_($i5)for@$j5;&$_($n5)for@$o5;&$_($s5)for@$t5;&$_($x5)for@$y5;&$_($C5)for@$D5;&$_($G5)for@$H5;&$_($J5)for@$K5;&$_($S5)for@$T5;&$_($W5)for@$X5;&$_($Z5)for@$c6;&$_($i6)for@$j6;&$_($n6)for@$o6;&$_($s6)for@$t6;&$_($v6)for@$w6;&$_($y6)for@$z6;ni->run(@ARGV);
__DATA__
