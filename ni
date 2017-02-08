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
$ni::self = bless {}, '/lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::named{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::name {my %h = @_; @ni::named{keys %h} = values %h}
sub ni::eval {eval shift}
*{'/lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  *$_ = $ni::named{"ni.def:$_"} = $kvs{$_} for keys %kvs;
};
*{'/lib/fn::new'} = sub {
  my $self = bless {code => $_[1]}, $_[0];
  $self->compile;
  $self;
};
*{'/lib/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/c/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
*{'/lib/fn::(('}    = sub {};
*{'/lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'/lib/fn::(bool'} = sub {1};
sub fn($) {'/lib/fn'->new(shift)}
_
$c=q'ni:/lib/image';$d=q'applied_to';$e=q'/metaclass.c';$f={$e,1};$g=q'name';$h=q'slices';$i=q'/class.c';$j={$i,1};$k=q'/lib/behavior.c';$l=q'/lib/branch.c';$m=q'/lib/doc.c';$n=q'/lib/fn.c';$o=q'/lib/image.c';$p=q'/lib/ni.c';$q=q'/lib/slice.c';$r=q'/lib/tag.c';$s=q'/object.c';$t={$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1};$u={$q,1};$v={$k,1,$l,1,$q,1,$r,1};$w=[undef];$x=q'/metaclass';$y=bless({$d,$v,$g,$k,$h,$w},$x);$z=q'/metaclass::ctors';$A=[$y];$B=bless({$d,$u,$g,$q,$h,$A},$x);$C=q'/metaclass::ctors';$D=q'/lib/slice';$E={$D,1};$F=q'/lib/behavior';$G=q'/lib/branch';$H=q'/lib/tag';$I={$F,1,$G,1,$D,1,$H,1};$J=q'/class';$K=q'/object';$L={$J,1,$i,1,$F,1,$k,1,$G,1,$l,1,$m,1,$n,1,$o,1,$p,1,$D,1,$q,1,$H,1,$r,1,$x,1,$e,1,$K,1,$s,1};$M={};$N=q'ctor';$O=undef;$P=q'dtor';$Q=q'methods';$R=q'class';$S={$n,1};$T=[undef];$U=bless({$d,$S,$g,$n,$h,$T},$x);$V=q'/metaclass::ctors';$W=q'/lib/fn';$X={$W,1};$Y={};$Z=q'DESTROY';$c1=q'code';$d1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$e1=bless({$c1,$d1},$W);$f1=q'/lib/fn::ctors';$g1=q'new';$h1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$i1=bless({$c1,$h1},$W);$j1={$Z,$e1,$g1,$i1};$k1=q'/lib/instantiable.b';$l1=bless({$d,$Y,$Q,$j1,$g,$k1},$D);$m1=q'/lib/slice::ctors';$n1={};$o1=q'shift->compile';$p1=bless({$c1,$o1},$W);$q1=q'compile';$r1=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
++$eval_n;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$s1=bless({$c1,$r1},$W);$t1=q'instantiate';$u1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$v1=bless({$c1,$u1},$W);$w1={$q1,$s1,$t1,$v1};$x1=q'/lib/fn_init.b';$y1=bless({$d,$n1,$N,$p1,$P,$O,$Q,$w1,$g,$x1},$D);$z1=q'/lib/slice::ctors';$A1={};$B1=q'serialize';$C1=q'annotations';$D1=[];$E1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$F1=bless({$C1,$D1,$c1,$E1},$W);$G1={$B1,$F1};$H1=q'/lib/fn_serialize.b';$I1=bless({$d,$A1,$N,$O,$P,$O,$Q,$G1,$g,$H1},$D);$J1=q'/lib/slice::ctors';$K1=[$l1,$y1,$I1];$L1=bless({$d,$X,$g,$W,$h,$K1},$n);$M1=q'/lib/fn.c::ctors';$N1=q'ni \'ni:\' . ref shift';$O1=bless({$c1,$N1},$W);$P1={$R,$O1};$Q1=q'/lib/instance.b';$R1=bless({$d,$M,$N,$O,$P,$O,$Q,$P1,$g,$Q1},$D);$S1=q'/lib/slice::ctors';$T1=[$R1];$U1=bless({$d,$L,$g,$K,$h,$T1},$s);$V1=q'/object.c::ctors';$W1=[$U1];$X1=bless({$d,$I,$g,$F,$h,$W1},$k);$Y1=q'/lib/behavior.c::ctors';$Z1={};$c2=q'my $s = shift; ni::name($s->name, $s)';$d2=bless({$c1,$c2},$W);$e2=q'$_[0]->namespace . ":" . $_[0]->{name}';$f2=bless({$c1,$e2},$W);$g2={$g,$f2};$h2=q'/lib/named.b';$i2=bless({$d,$Z1,$N,$d2,$P,$O,$Q,$g2,$g,$h2},$D);$j2=q'/lib/doc';$k2=q'/lib/slice::ctors';$l2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$m2=bless({$c1,$l2},$W);$n2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$o2=bless({$c1,$n2},$W);$p2=q'/lib/slice::apply';$q2=q'/lib/slice::apply_unsafe';$r2={};$s2=q'apply';$t2=q'apply_unsafe';$u2={$s2,$m2,$t2,$o2};$v2=q'/lib/slice.b';$w2=bless({$d,$r2,$Q,$u2,$g,$v2},$D);$x2=q'/lib/slice::ctors';$y2={};$z2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$A2=bless({$c1,$z2},$W);$B2={$t1,$A2};$C2=q'/lib/slice_init.b';$D2=bless({$d,$y2,$Q,$B2,$g,$C2},$D);$E2=q'/lib/slice::ctors';$F2={};$G2=[];$H2=q'local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq \'ni:/lib/slice.b\') {
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
$g;';$I2=bless({$C1,$G2,$c1,$H2},$W);$J2={$B1,$I2};$K2=q'/lib/slice_serialize.b';$L2=bless({$d,$F2,$N,$O,$P,$O,$Q,$J2,$g,$K2},$D);$M2=q'/lib/slice::ctors';$N2=[$X1,$i2,$w2,$D2,$L2];$O2=bless({$d,$E,$g,$D,$h,$N2},$q);$P2=q'/lib/slice.c::ctors';$Q2={};$R2=q'doc';$S2=[];$T2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$U2=bless({$C1,$S2,$c1,$T2},$W);$V2={$R2,$U2};$W2=q'/lib/documentable.b';$X2=bless({$d,$Q2,$N,$O,$P,$O,$Q,$V2,$g,$W2},$D);$Y2=q'/lib/slice::ctors';$Z2=[undef,$X2];$c3=bless({$d,$t,$g,$s,$h,$Z2},$x);$d3=q'/metaclass::ctors';$e3=[$c3];$f3=bless({$d,$j,$g,$i,$h,$e3},$x);$g3=q'/metaclass::ctors';$h3={$J,1,$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$e,1,$s,1};$i3={$r,1};$j3=[$y];$k3=bless({$d,$i3,$g,$r,$h,$j3},$x);$l3=q'/metaclass::ctors';$m3={$H,1};$n3={};$o3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$p3=bless({$c1,$o3},$W);$q3={$s2,$p3};$r3=q'/lib/tag.b';$s3=bless({$d,$n3,$N,$O,$P,$O,$Q,$q3,$g,$r3},$D);$t3=q'/lib/slice::ctors';$u3={};$v3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$w3=bless({$c1,$v3},$W);$x3={$t1,$w3};$y3=q'/lib/tag_init.b';$z3=bless({$d,$u3,$N,$O,$P,$O,$Q,$x3,$g,$y3},$D);$A3=q'/lib/slice::ctors';$B3=[$X1,$i2,$s3,$z3];$C3=bless({$d,$m3,$g,$H,$h,$B3},$r);$D3=q'/lib/tag.c::ctors';$E3=q'/lib/perlbranch.b';$F3={};$G3=q'add';$H3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$I3=bless({$c1,$H3},$W);$J3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$K3=bless({$c1,$J3},$W);$L3={$G3,$I3,$s2,$K3};$M3=q'/lib/branch.b';$N3=bless({$d,$F3,$N,$O,$P,$O,$Q,$L3,$g,$M3},$D);$O3=q'/lib/slice::ctors';$P3={};$Q3=q'namespace';$R3=q'\'ni\'';$S3=bless({$c1,$R3},$W);$T3={$Q3,$S3};$U3=q'/lib/named_in_ni.b';$V3=bless({$d,$P3,$N,$O,$P,$O,$Q,$T3,$g,$U3},$D);$W3=q'/lib/slice::ctors';$X3={};$Y3=q'package';$Z3=q'shift->{name}';$c4=bless({$c1,$Z3},$W);$d4={$Y3,$c4};$e4=q'/lib/namespaced.b';$f4=bless({$d,$X3,$N,$O,$P,$O,$Q,$d4,$g,$e4},$D);$g4=q'/lib/slice::ctors';$h4={};$i4=q'resolve';$j4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$k4=bless({$c1,$j4},$W);$l4={$i4,$k4};$m4=q'/lib/resolver.b';$n4=bless({$d,$h4,$N,$O,$P,$O,$Q,$l4,$g,$m4},$D);$o4=q'/lib/slice::ctors';$p4=[$N3,$l1,$i2,$V3,$f4,$n4];$q4=bless({$g,$E3,$h,$p4},$H);$r4=q'/lib/tag::ctors';$s4={};$t4=q'my $s = shift; $s->apply($s->package)';$u4=bless({$c1,$t4},$W);$v4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$w4=bless({$c1,$v4},$W);$x4={$t1,$w4};$y4=q'/lib/class_init.b';$z4=bless({$d,$s4,$N,$u4,$P,$O,$Q,$x4,$g,$y4},$D);$A4=q'/lib/slice::ctors';$B4={$l,1};$C4=[$y];$D4=bless({$d,$B4,$g,$l,$h,$C4},$x);$E4=q'/metaclass::ctors';$F4={$G,1};$G4={};$H4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$I4=bless({$c1,$H4},$W);$J4={$t1,$I4};$K4=q'/lib/branch_init.b';$L4=bless({$d,$G4,$N,$O,$P,$O,$Q,$J4,$g,$K4},$D);$M4=q'/lib/slice::ctors';$N4=[$X1,$i2,$N3,$L4,undef];$O4=bless({$d,$F4,$g,$G,$h,$N4},$l);$P4=q'/lib/branch.c::ctors';$Q4={$J,1,$i,1,$k,1,$G,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$x,1,$e,1,$s,1};$R4=q'/lib/definition.b';$S4={};$T4=q'def';$U4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$V4=bless({$c1,$U4},$W);$W4={$T4,$V4};$X4=q'/lib/classdef.b';$Y4=bless({$d,$S4,$N,$O,$P,$O,$Q,$W4,$g,$X4},$D);$Z4=q'/lib/slice::ctors';$c5={};$d5=q'ro';$e5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$f5=bless({$c1,$e5},$W);$g5=q'rw';$h5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$i5=bless({$c1,$h5},$W);$j5={$d5,$f5,$g5,$i5};$k5=q'/lib/accessor.b';$l5=bless({$d,$c5,$N,$O,$P,$O,$Q,$j5,$g,$k5},$D);$m5=q'/lib/slice::ctors';$n5=[$Y4,$l5];$o5=bless({$d,$Q4,$g,$R4,$h,$n5},$G);$p5=q'/lib/branch::ctors';$q5={};$r5=q'child';$s5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$t5=bless({$c1,$s5},$W);$u5={$r5,$t5};$v5=q'/lib/subclass.b';$w5=bless({$d,$q5,$N,$O,$P,$O,$Q,$u5,$g,$v5},$D);$x5=q'/lib/slice::ctors';$y5=[$q4,$z4,$U1,$o5,$w5];$z5=bless({$d,$h3,$g,$J,$h,$y5},$i);$A5=q'/class.c::ctors';$B5=[$z5];$C5=bless({$d,$f,$g,$e,$h,$B5},$x);$D5=q'/metaclass::ctors';$E5={$x,1};$F5=[$q4,$z4,$U1,$o5];$G5=bless({$d,$E5,$g,$x,$h,$F5},$e);$H5=q'/metaclass.c::ctors';$I5={$o,1};$J5=[$c3];$K5=bless({$d,$I5,$g,$o,$h,$J5},$x);$L5=q'/metaclass::ctors';$M5=q'/lib/image';$N5={$M5,1};$O5={};$P5=[];$Q5=q'my $class = shift;
my %args  = (
  include_shebang => 1,
  include_license => 1,
  include_boot    => 1,
  include_classes => 1,
  include_run     => 1,
  local_vars      => 0,
  use_newlines    => 0,
  @_);

+{include_shebang => $args{include_shebang},
  include_license => $args{include_license},
  include_boot    => $args{include_boot},
  include_classes => $args{include_classes},
  include_run     => $args{include_run},
  local_vars      => $args{local_vars},
  use_newlines    => $args{use_newlines},

  gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};';$R5=bless({$C1,$P5,$c1,$Q5},$W);$S5={$t1,$R5};$T5=q'/lib/image_init.b';$U5=bless({$d,$O5,$N,$O,$P,$O,$Q,$S5,$g,$T5},$D);$V5=q'/lib/slice::ctors';$W5={};$X5=q'address';$Y5=[];$Z5=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$c6=bless({$C1,$Y5,$c1,$Z5},$W);$d6=q'allocate_gensym';$e6=[];$f6=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$g6=bless({$C1,$e6,$c1,$f6},$W);$h6=q'boot_side_effect';$i6=[];$j6=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$k6=bless({$C1,$i6,$c1,$j6},$W);$l6=q'circular_links';$m6=[];$n6=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$o6=bless({$C1,$m6,$c1,$n6},$W);$p6=q'finalizer';$q6=[];$r6=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$s6=bless({$C1,$q6,$c1,$r6},$W);$t6=q'gensym';$u6=[];$v6=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$w6=bless({$C1,$u6,$c1,$v6},$W);$x6=q'is_circular';$y6=[];$z6=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$A6=bless({$C1,$y6,$c1,$z6},$W);$B6=q'partial_image';$C6=[];$D6=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\@ni::named{keys\\%$g}=values\\%$g;");';$E6=bless({$C1,$C6,$c1,$D6},$W);$F6=q'quote';$G6=[];$H6=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$I6=bless({$C1,$G6,$c1,$H6},$W);$J6=q'quote_array';$K6=[];$L6=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$M6=bless({$C1,$K6,$c1,$L6},$W);$N6=q'quote_blessed';$O6=[];$P6=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$Q6=bless({$C1,$O6,$c1,$P6},$W);$R6=q'quote_class';$S6=[];$T6=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && exists $ni::named{"ni:$class"};';$U6=bless({$C1,$S6,$c1,$T6},$W);$V6=q'quote_hash';$W6=[];$X6=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$Y6=bless({$C1,$W6,$c1,$X6},$W);$Z6=q'quote_object';$c7=[];$d7=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$e7=bless({$C1,$c7,$c1,$d7},$W);$f7=q'quote_scalar';$g7=[];$h7=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$i7=bless({$C1,$g7,$c1,$h7},$W);$j7=q'quote_value';$k7=[];$l7=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$m7=bless({$C1,$k7,$c1,$l7},$W);$n7=q'reconstruction';$o7=[];$p7=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$q7=bless({$C1,$o7,$c1,$p7},$W);$r7=q'side_effect';$s7=[];$t7=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$u7=bless({$C1,$s7,$c1,$t7},$W);$v7=q'write';$w7=[];$x7=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$y7=bless({$C1,$w7,$c1,$x7},$W);$z7={$X5,$c6,$d6,$g6,$h6,$k6,$l6,$o6,$p6,$s6,$t6,$w6,$x6,$A6,$B6,$E6,$F6,$I6,$J6,$M6,$N6,$Q6,$R6,$U6,$V6,$Y6,$Z6,$e7,$f7,$i7,$j7,$m7,$n7,$q7,$r7,$u7,$v7,$y7};$A7=q'/lib/image_quoting.b';$B7=bless({$d,$W5,$N,$O,$P,$O,$Q,$z7,$g,$A7},$D);$C7=q'/lib/slice::ctors';$D7=[$U5,$B7];$E7=bless({$d,$N5,$g,$M5,$h,$D7},$o);$F7=q'/lib/image.c::ctors';$G7=q'ni:/lib/ni';$H7={$p,1};$I7=[$c3];$J7=bless({$d,$H7,$g,$p,$h,$I7},$x);$K7=q'/metaclass::ctors';$L7=q'/lib/ni';$M7={$L7,1};$N7={};$O7=q'is_mutable';$P7=[];$Q7=q'$0 ne "-" && -w $0';$R7=bless({$C1,$P7,$c1,$Q7},$W);$S7=q'modify';$T7=[];$U7=q'my ($self, $fn) = @_;
die "ni: cannot modify immutable instance" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
my $temp = map chr 97 + rand(26), 1..16;
my @r = split /\\//, $0;
$r[-1] =~ s/^/./;
$r[-1] =~ s/$/.$temp/;
my $r = join \'/\', @r;
open my $w, \'>\', $r or die "ni: failed to create staging file: $!";
chmod $mode, $r or die "ni: failed to chmod $r to $mode: $!";
&$fn($w);
close $w;
rename $r, $0 or die "ni: failed to rename: $!";';$V7=bless({$C1,$T7,$c1,$U7},$W);$W7={$O7,$R7,$S7,$V7};$X7=q'/lib/ni_self.b';$Y7=bless({$d,$N7,$N,$O,$P,$O,$Q,$W7,$g,$X7},$D);$Z7=q'/lib/slice::ctors';$c8={};$d8=q'internal/+=';$e8=[];$f8=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
# TODO: fix this by using namespace objects that indicate whether they
# should be serialized
my @ks = grep !/^ni\\.eval:/, sort keys %ni::named;
my $q = ni(\'ni:/lib/image\')->new->partial_image(@ks);
$self->modify(sub {$q->write(shift)});
0;';$g8=bless({$C1,$e8,$c1,$f8},$W);$h8=q'internal/eval';$i8=[];$j8=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$k8=bless({$C1,$i8,$c1,$j8},$W);$l8=q'internal/image';$m8=[];$n8=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new->partial_image(\'ni:/lib/ni\', \'ni:/lib/image\');
$q->write(\\*STDOUT);
0;';$o8=bless({$C1,$m8,$c1,$n8},$W);$p8=q'run';$q8=[];$r8=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';$s8=bless({$C1,$q8,$c1,$r8},$W);$t8={$d8,$g8,$h8,$k8,$l8,$o8,$p8,$s8};$u8=q'/lib/ni_main.b';$v8=bless({$d,$c8,$N,$O,$P,$O,$Q,$t8,$g,$u8},$D);$w8=q'/lib/slice::ctors';$x8=[$Y7,$v8];$y8=bless({$d,$M7,$g,$L7,$h,$x8},$p);$z8=q'/lib/ni.c::ctors';$A8={$c,$E7,$G7,$y8};$$Z2[0]=$z5;$$w[0]=$c3;$$T[0]=$c3;$$N4[4]=$o5;*$q2=\&$o2;*$p2=\&$m2;$l1->apply_unsafe($J);$l1->apply_unsafe($i);$l1->apply_unsafe($k);$l1->apply_unsafe($G);$l1->apply_unsafe($l);$l1->apply_unsafe($m);$l1->apply_unsafe($W);$l1->apply_unsafe($n);$l1->apply_unsafe($o);$l1->apply_unsafe($p);$l1->apply_unsafe($D);$l1->apply_unsafe($q);$l1->apply_unsafe($H);$l1->apply_unsafe($r);$l1->apply_unsafe($x);$l1->apply_unsafe($e);$l1->apply_unsafe($s);$y1->apply_unsafe($W);$I1->apply_unsafe($W);$R1->apply_unsafe($J);$R1->apply_unsafe($i);$R1->apply_unsafe($F);$R1->apply_unsafe($k);$R1->apply_unsafe($G);$R1->apply_unsafe($l);$R1->apply_unsafe($m);$R1->apply_unsafe($n);$R1->apply_unsafe($o);$R1->apply_unsafe($p);$R1->apply_unsafe($D);$R1->apply_unsafe($q);$R1->apply_unsafe($H);$R1->apply_unsafe($r);$R1->apply_unsafe($x);$R1->apply_unsafe($e);$R1->apply_unsafe($K);$R1->apply_unsafe($s);$i2->apply_unsafe($J);$i2->apply_unsafe($i);$i2->apply_unsafe($k);$i2->apply_unsafe($G);$i2->apply_unsafe($l);$i2->apply_unsafe($j2);$i2->apply_unsafe($m);$i2->apply_unsafe($n);$i2->apply_unsafe($o);$i2->apply_unsafe($p);$i2->apply_unsafe($D);$i2->apply_unsafe($q);$i2->apply_unsafe($H);$i2->apply_unsafe($r);$i2->apply_unsafe($x);$i2->apply_unsafe($e);$i2->apply_unsafe($s);$w2->apply_unsafe($D);$D2->apply_unsafe($D);$L2->apply_unsafe($D);$X2->apply_unsafe($i);$X2->apply_unsafe($k);$X2->apply_unsafe($l);$X2->apply_unsafe($m);$X2->apply_unsafe($n);$X2->apply_unsafe($o);$X2->apply_unsafe($p);$X2->apply_unsafe($q);$X2->apply_unsafe($r);$X2->apply_unsafe($s);$s3->apply_unsafe($H);$z3->apply_unsafe($H);$N3->apply_unsafe($J);$N3->apply_unsafe($i);$N3->apply_unsafe($k);$N3->apply_unsafe($G);$N3->apply_unsafe($l);$N3->apply_unsafe($m);$N3->apply_unsafe($n);$N3->apply_unsafe($o);$N3->apply_unsafe($p);$N3->apply_unsafe($q);$N3->apply_unsafe($r);$N3->apply_unsafe($x);$N3->apply_unsafe($e);$N3->apply_unsafe($s);$V3->apply_unsafe($J);$V3->apply_unsafe($i);$V3->apply_unsafe($k);$V3->apply_unsafe($G);$V3->apply_unsafe($l);$V3->apply_unsafe($m);$V3->apply_unsafe($n);$V3->apply_unsafe($o);$V3->apply_unsafe($p);$V3->apply_unsafe($D);$V3->apply_unsafe($q);$V3->apply_unsafe($H);$V3->apply_unsafe($r);$V3->apply_unsafe($x);$V3->apply_unsafe($e);$V3->apply_unsafe($s);$f4->apply_unsafe($J);$f4->apply_unsafe($i);$f4->apply_unsafe($k);$f4->apply_unsafe($G);$f4->apply_unsafe($l);$f4->apply_unsafe($m);$f4->apply_unsafe($n);$f4->apply_unsafe($o);$f4->apply_unsafe($p);$f4->apply_unsafe($D);$f4->apply_unsafe($q);$f4->apply_unsafe($H);$f4->apply_unsafe($r);$f4->apply_unsafe($x);$f4->apply_unsafe($e);$f4->apply_unsafe($s);$n4->apply_unsafe($J);$n4->apply_unsafe($i);$n4->apply_unsafe($k);$n4->apply_unsafe($G);$n4->apply_unsafe($l);$n4->apply_unsafe($m);$n4->apply_unsafe($n);$n4->apply_unsafe($o);$n4->apply_unsafe($p);$n4->apply_unsafe($q);$n4->apply_unsafe($H);$n4->apply_unsafe($r);$n4->apply_unsafe($x);$n4->apply_unsafe($e);$n4->apply_unsafe($s);$z4->apply_unsafe($J);$z4->apply_unsafe($i);$z4->apply_unsafe($k);$z4->apply_unsafe($l);$z4->apply_unsafe($m);$z4->apply_unsafe($n);$z4->apply_unsafe($o);$z4->apply_unsafe($p);$z4->apply_unsafe($q);$z4->apply_unsafe($r);$z4->apply_unsafe($x);$z4->apply_unsafe($e);$z4->apply_unsafe($s);$L4->apply_unsafe($G);$Y4->apply_unsafe($J);$Y4->apply_unsafe($i);$Y4->apply_unsafe($k);$Y4->apply_unsafe($G);$Y4->apply_unsafe($l);$Y4->apply_unsafe($m);$Y4->apply_unsafe($n);$Y4->apply_unsafe($o);$Y4->apply_unsafe($p);$Y4->apply_unsafe($q);$Y4->apply_unsafe($r);$Y4->apply_unsafe($x);$Y4->apply_unsafe($e);$Y4->apply_unsafe($s);$l5->apply_unsafe($J);$l5->apply_unsafe($i);$l5->apply_unsafe($k);$l5->apply_unsafe($G);$l5->apply_unsafe($l);$l5->apply_unsafe($m);$l5->apply_unsafe($n);$l5->apply_unsafe($o);$l5->apply_unsafe($p);$l5->apply_unsafe($q);$l5->apply_unsafe($r);$l5->apply_unsafe($x);$l5->apply_unsafe($e);$l5->apply_unsafe($s);$w5->apply_unsafe($J);$w5->apply_unsafe($i);$w5->apply_unsafe($k);$w5->apply_unsafe($l);$w5->apply_unsafe($m);$w5->apply_unsafe($n);$w5->apply_unsafe($o);$w5->apply_unsafe($p);$w5->apply_unsafe($q);$w5->apply_unsafe($r);$w5->apply_unsafe($e);$w5->apply_unsafe($s);$U5->apply_unsafe($M5);$B7->apply_unsafe($M5);$Y7->apply_unsafe($L7);$v8->apply_unsafe($L7);@ni::named{keys%$A8}=values%$A8;&$_($y)for@$z;&$_($B)for@$C;&$_($U)for@$V;&$_($e1)for@$f1;&$_($i1)for@$f1;&$_($l1)for@$m1;&$_($p1)for@$f1;&$_($s1)for@$f1;&$_($v1)for@$f1;&$_($y1)for@$z1;&$_($F1)for@$f1;&$_($I1)for@$J1;&$_($L1)for@$M1;&$_($O1)for@$f1;&$_($R1)for@$S1;&$_($U1)for@$V1;&$_($X1)for@$Y1;&$_($d2)for@$f1;&$_($f2)for@$f1;&$_($i2)for@$k2;&$_($m2)for@$f1;&$_($o2)for@$f1;&$_($w2)for@$x2;&$_($A2)for@$f1;&$_($D2)for@$E2;&$_($I2)for@$f1;&$_($L2)for@$M2;&$_($O2)for@$P2;&$_($U2)for@$f1;&$_($X2)for@$Y2;&$_($c3)for@$d3;&$_($f3)for@$g3;&$_($k3)for@$l3;&$_($p3)for@$f1;&$_($s3)for@$t3;&$_($w3)for@$f1;&$_($z3)for@$A3;&$_($C3)for@$D3;&$_($I3)for@$f1;&$_($K3)for@$f1;&$_($N3)for@$O3;&$_($S3)for@$f1;&$_($V3)for@$W3;&$_($c4)for@$f1;&$_($f4)for@$g4;&$_($k4)for@$f1;&$_($n4)for@$o4;&$_($q4)for@$r4;&$_($u4)for@$f1;&$_($w4)for@$f1;&$_($z4)for@$A4;&$_($D4)for@$E4;&$_($I4)for@$f1;&$_($L4)for@$M4;&$_($O4)for@$P4;&$_($V4)for@$f1;&$_($Y4)for@$Z4;&$_($f5)for@$f1;&$_($i5)for@$f1;&$_($l5)for@$m5;&$_($o5)for@$p5;&$_($t5)for@$f1;&$_($w5)for@$x5;&$_($z5)for@$A5;&$_($C5)for@$D5;&$_($G5)for@$H5;&$_($K5)for@$L5;&$_($R5)for@$f1;&$_($U5)for@$V5;&$_($c6)for@$f1;&$_($g6)for@$f1;&$_($k6)for@$f1;&$_($o6)for@$f1;&$_($s6)for@$f1;&$_($w6)for@$f1;&$_($A6)for@$f1;&$_($E6)for@$f1;&$_($I6)for@$f1;&$_($M6)for@$f1;&$_($Q6)for@$f1;&$_($U6)for@$f1;&$_($Y6)for@$f1;&$_($e7)for@$f1;&$_($i7)for@$f1;&$_($m7)for@$f1;&$_($q7)for@$f1;&$_($u7)for@$f1;&$_($y7)for@$f1;&$_($B7)for@$C7;&$_($E7)for@$F7;&$_($J7)for@$K7;&$_($R7)for@$f1;&$_($V7)for@$f1;&$_($Y7)for@$Z7;&$_($g8)for@$f1;&$_($k8)for@$f1;&$_($o8)for@$f1;&$_($s8)for@$f1;&$_($v8)for@$w8;&$_($y8)for@$z8;ni->run(@ARGV);
__DATA__
