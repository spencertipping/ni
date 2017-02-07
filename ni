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
$c=q'ni:/lib/image';
$d=q'applied_to';
$e=q'/metaclass.c';
$f={$e,1};
$g=q'name';
$h=q'slices';
$i=q'/class.c';
$j={$i,1};
$k=q'/lib/behavior.c';
$l=q'/lib/branch.c';
$m=q'/lib/fn.c';
$n=q'/lib/image.c';
$o=q'/lib/ni.c';
$p=q'/lib/slice.c';
$q=q'/lib/tag.c';
$r=q'/object.c';
$s={$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1};
$t=[undef];
$u=q'/metaclass';
$v=bless({$d,$s,$g,$r,$h,$t},$u);
$w=q'/metaclass::ctors';
$x=[$v];
$y=bless({$d,$j,$g,$i,$h,$x},$u);
$z=q'/metaclass::ctors';
$A=q'/class';
$B={$A,1,$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$e,1,$r,1};
$C={$q,1};
$D={$k,1,$l,1,$p,1,$q,1};
$E=[$v];
$F=bless({$d,$D,$g,$k,$h,$E},$u);
$G=q'/metaclass::ctors';
$H=[$F];
$I=bless({$d,$C,$g,$q,$h,$H},$u);
$J=q'/metaclass::ctors';
$K=q'/lib/tag';
$L={$K,1};
$M=q'/lib/behavior';
$N=q'/lib/branch';
$O=q'/lib/slice';
$P={$M,1,$N,1,$O,1,$K,1};
$Q=q'/object';
$R={$A,1,$i,1,$M,1,$k,1,$N,1,$l,1,$m,1,$n,1,$o,1,$O,1,$p,1,$K,1,$q,1,$u,1,$e,1,$Q,1,$r,1};
$S={$p,1};
$T=[$F];
$U=bless({$d,$S,$g,$p,$h,$T},$u);
$V=q'/metaclass::ctors';
$W={$O,1};
$X={};
$Y=q'ctor';
$Z={$m,1};
$c1=[$v];
$d1=bless({$d,$Z,$g,$m,$h,$c1},$u);
$e1=q'/metaclass::ctors';
$f1=q'/lib/fn';
$g1={$f1,1};
$h1={};
$i1=q'methods';
$j1=q'DESTROY';
$k1=q'code';
$l1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';
$m1=bless({$k1,$l1},$f1);
$n1=q'/lib/fn::ctors';
$o1=q'new';
$p1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';
$q1=bless({$k1,$p1},$f1);
$r1={$j1,$m1,$o1,$q1};
$s1=q'/lib/instantiable.b';
$t1=bless({$d,$h1,$i1,$r1,$g,$s1},$O);
$u1=q'/lib/slice::ctors';
$v1={};
$w1=q'shift->compile';
$x1=bless({$k1,$w1},$f1);
$y1=q'dtor';
$z1=undef;
$A1=q'compile';
$B1=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';
$C1=bless({$k1,$B1},$f1);
$D1=q'instantiate';
$E1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';
$F1=bless({$k1,$E1},$f1);
$G1={$A1,$C1,$D1,$F1};
$H1=q'/lib/fn_init.b';
$I1=bless({$d,$v1,$Y,$x1,$y1,$z1,$i1,$G1,$g,$H1},$O);
$J1=q'/lib/slice::ctors';
$K1={};
$L1=q'serialize';
$M1=q'annotations';
$N1=[];
$O1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';
$P1=bless({$M1,$N1,$k1,$O1},$f1);
$Q1={$L1,$P1};
$R1=q'/lib/fn_serialize.b';
$S1=bless({$d,$K1,$Y,$z1,$y1,$z1,$i1,$Q1,$g,$R1},$O);
$T1=q'/lib/slice::ctors';
$U1=[$t1,$I1,$S1];
$V1=bless({$d,$g1,$g,$f1,$h,$U1},$m);
$W1=q'/lib/fn.c::ctors';
$X1=q'my $s = shift; ni::name($s->name, $s)';
$Y1=bless({$k1,$X1},$f1);
$Z1=q'"ni:" . shift->{name}';
$c2=bless({$k1,$Z1},$f1);
$d2={$g,$c2};
$e2=q'/lib/named.b';
$f2=bless({$d,$X,$Y,$Y1,$y1,$z1,$i1,$d2,$g,$e2},$O);
$g2=q'/lib/slice::ctors';
$h2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';
$i2=bless({$k1,$h2},$f1);
$j2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';
$k2=bless({$k1,$j2},$f1);
$l2=q'/lib/slice::apply';
$m2=q'/lib/slice::apply_unsafe';
$n2={};
$o2=q'apply';
$p2=q'apply_unsafe';
$q2={$o2,$i2,$p2,$k2};
$r2=q'/lib/slice.b';
$s2=bless({$d,$n2,$i1,$q2,$g,$r2},$O);
$t2=q'/lib/slice::ctors';
$u2={};
$v2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';
$w2=bless({$k1,$v2},$f1);
$x2={$D1,$w2};
$y2=q'/lib/slice_init.b';
$z2=bless({$d,$u2,$i1,$x2,$g,$y2},$O);
$A2=q'/lib/slice::ctors';
$B2={};
$C2=[];
$D2=q'local $_;
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
$g;';
$E2=bless({$M1,$C2,$k1,$D2},$f1);
$F2={$L1,$E2};
$G2=q'/lib/slice_serialize.b';
$H2=bless({$d,$B2,$Y,$z1,$y1,$z1,$i1,$F2,$g,$G2},$O);
$I2=q'/lib/slice::ctors';
$J2=[undef,$f2,$s2,$z2,$H2];
$K2=bless({$d,$W,$g,$O,$h,$J2},$p);
$L2=q'/lib/slice.c::ctors';
$M2={};
$N2=q'class';
$O2=q'ni \'ni:\' . ref shift';
$P2=bless({$k1,$O2},$f1);
$Q2={$N2,$P2};
$R2=q'/lib/instance.b';
$S2=bless({$d,$M2,$Y,$z1,$y1,$z1,$i1,$Q2,$g,$R2},$O);
$T2=q'/lib/slice::ctors';
$U2=[$S2];
$V2=bless({$d,$R,$g,$Q,$h,$U2},$r);
$W2=q'/object.c::ctors';
$X2=[$V2];
$Y2=bless({$d,$P,$g,$M,$h,$X2},$k);
$Z2=q'/lib/behavior.c::ctors';
$c3={};
$d3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';
$e3=bless({$k1,$d3},$f1);
$f3={$o2,$e3};
$g3=q'/lib/tag.b';
$h3=bless({$d,$c3,$Y,$z1,$y1,$z1,$i1,$f3,$g,$g3},$O);
$i3=q'/lib/slice::ctors';
$j3={};
$k3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';
$l3=bless({$k1,$k3},$f1);
$m3={$D1,$l3};
$n3=q'/lib/tag_init.b';
$o3=bless({$d,$j3,$Y,$z1,$y1,$z1,$i1,$m3,$g,$n3},$O);
$p3=q'/lib/slice::ctors';
$q3=[$Y2,$f2,$h3,$o3];
$r3=bless({$d,$L,$g,$K,$h,$q3},$q);
$s3=q'/lib/tag.c::ctors';
$t3=q'/lib/perlbranch.b';
$u3={};
$v3=q'add';
$w3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';
$x3=bless({$k1,$w3},$f1);
$y3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';
$z3=bless({$k1,$y3},$f1);
$A3={$v3,$x3,$o2,$z3};
$B3=q'/lib/branch.b';
$C3=bless({$d,$u3,$Y,$z1,$y1,$z1,$i1,$A3,$g,$B3},$O);
$D3=q'/lib/slice::ctors';
$E3={};
$F3=q'package';
$G3=q'shift->{name}';
$H3=bless({$k1,$G3},$f1);
$I3={$F3,$H3};
$J3=q'/lib/namespaced.b';
$K3=bless({$d,$E3,$Y,$z1,$y1,$z1,$i1,$I3,$g,$J3},$O);
$L3=q'/lib/slice::ctors';
$M3={};
$N3=q'resolve';
$O3=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';
$P3=bless({$k1,$O3},$f1);
$Q3={$N3,$P3};
$R3=q'/lib/resolver.b';
$S3=bless({$d,$M3,$Y,$z1,$y1,$z1,$i1,$Q3,$g,$R3},$O);
$T3=q'/lib/slice::ctors';
$U3=[$C3,$t1,$f2,$K3,$S3];
$V3=bless({$g,$t3,$h,$U3},$K);
$W3=q'/lib/tag::ctors';
$X3={};
$Y3=q'my $s = shift; $s->apply($s->package)';
$Z3=bless({$k1,$Y3},$f1);
$c4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';
$d4=bless({$k1,$c4},$f1);
$e4={$D1,$d4};
$f4=q'/lib/class_init.b';
$g4=bless({$d,$X3,$Y,$Z3,$y1,$z1,$i1,$e4,$g,$f4},$O);
$h4=q'/lib/slice::ctors';
$i4={};
$j4=q'def';
$k4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';
$l4=bless({$k1,$k4},$f1);
$m4={$j4,$l4};
$n4=q'/lib/classdef.b';
$o4=bless({$d,$i4,$Y,$z1,$y1,$z1,$i1,$m4,$g,$n4},$O);
$p4=q'/lib/slice::ctors';
$q4={};
$r4=q'child';
$s4=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';
$t4=bless({$k1,$s4},$f1);
$u4={$r4,$t4};
$v4=q'/lib/subclass.b';
$w4=bless({$d,$q4,$Y,$z1,$y1,$z1,$i1,$u4,$g,$v4},$O);
$x4=q'/lib/slice::ctors';
$y4=[$V3,$g4,$V2,$o4,$w4];
$z4=bless({$d,$B,$g,$A,$h,$y4},$i);
$A4=q'/class.c::ctors';
$B4=[$z4];
$C4=bless({$d,$f,$g,$e,$h,$B4},$u);
$D4=q'/metaclass::ctors';
$E4={$u,1};
$F4=[$V3,$g4,$V2];
$G4=bless({$d,$E4,$g,$u,$h,$F4},$e);
$H4=q'/metaclass.c::ctors';
$I4={$n,1};
$J4=[$v];
$K4=bless({$d,$I4,$g,$n,$h,$J4},$u);
$L4=q'/metaclass::ctors';
$M4=q'/lib/image';
$N4={$M4,1};
$O4={};
$P4=[];
$Q4=q'my $class = shift;
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
  ordering     => []};';
$R4=bless({$M1,$P4,$k1,$Q4},$f1);
$S4={$D1,$R4};
$T4=q'/lib/image_init.b';
$U4=bless({$d,$O4,$Y,$z1,$y1,$z1,$i1,$S4,$g,$T4},$O);
$V4=q'/lib/slice::ctors';
$W4={};
$X4=q'address';
$Y4=[];
$Z4=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';
$c5=bless({$M1,$Y4,$k1,$Z4},$f1);
$d5=q'allocate_gensym';
$e5=[];
$f5=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';
$g5=bless({$M1,$e5,$k1,$f5},$f1);
$h5=q'boot_side_effect';
$i5=[];
$j5=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$k5=bless({$M1,$i5,$k1,$j5},$f1);
$l5=q'circular_links';
$m5=[];
$n5=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';
$o5=bless({$M1,$m5,$k1,$n5},$f1);
$p5=q'finalizer';
$q5=[];
$r5=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';
$s5=bless({$M1,$q5,$k1,$r5},$f1);
$t5=q'gensym';
$u5=[];
$v5=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';
$w5=bless({$M1,$u5,$k1,$v5},$f1);
$x5=q'is_circular';
$y5=[];
$z5=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';
$A5=bless({$M1,$y5,$k1,$z5},$f1);
$B5=q'partial_image';
$C5=[];
$D5=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\@ni::named{keys\\%$g}=values\\%$g;");';
$E5=bless({$M1,$C5,$k1,$D5},$f1);
$F5=q'quote';
$G5=[];
$H5=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';
$I5=bless({$M1,$G5,$k1,$H5},$f1);
$J5=q'quote_array';
$K5=[];
$L5=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';
$M5=bless({$M1,$K5,$k1,$L5},$f1);
$N5=q'quote_blessed';
$O5=[];
$P5=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';
$Q5=bless({$M1,$O5,$k1,$P5},$f1);
$R5=q'quote_class';
$S5=[];
$T5=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && exists $ni::named{"ni:$class"};';
$U5=bless({$M1,$S5,$k1,$T5},$f1);
$V5=q'quote_hash';
$W5=[];
$X5=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';
$Y5=bless({$M1,$W5,$k1,$X5},$f1);
$Z5=q'quote_object';
$c6=[];
$d6=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';
$e6=bless({$M1,$c6,$k1,$d6},$f1);
$f6=q'quote_scalar';
$g6=[];
$h6=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';
$i6=bless({$M1,$g6,$k1,$h6},$f1);
$j6=q'quote_value';
$k6=[];
$l6=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';
$m6=bless({$M1,$k6,$k1,$l6},$f1);
$n6=q'reconstruction';
$o6=[];
$p6=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';
$q6=bless({$M1,$o6,$k1,$p6},$f1);
$r6=q'side_effect';
$s6=[];
$t6=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$u6=bless({$M1,$s6,$k1,$t6},$f1);
$v6=q'write';
$w6=[];
$x6=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';
$y6=bless({$M1,$w6,$k1,$x6},$f1);
$z6={$X4,$c5,$d5,$g5,$h5,$k5,$l5,$o5,$p5,$s5,$t5,$w5,$x5,$A5,$B5,$E5,$F5,$I5,$J5,$M5,$N5,$Q5,$R5,$U5,$V5,$Y5,$Z5,$e6,$f6,$i6,$j6,$m6,$n6,$q6,$r6,$u6,$v6,$y6};
$A6=q'/lib/image_quoting.b';
$B6=bless({$d,$W4,$Y,$z1,$y1,$z1,$i1,$z6,$g,$A6},$O);
$C6=q'/lib/slice::ctors';
$D6=[$U4,$B6];
$E6=bless({$d,$N4,$g,$M4,$h,$D6},$n);
$F6=q'/lib/image.c::ctors';
$G6=q'ni:/lib/ni';
$H6={$o,1};
$I6=[$v];
$J6=bless({$d,$H6,$g,$o,$h,$I6},$u);
$K6=q'/metaclass::ctors';
$L6=q'/lib/ni';
$M6={$L6,1};
$N6={};
$O6=q'internal/eval';
$P6=[];
$Q6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';
$R6=bless({$M1,$P6,$k1,$Q6},$f1);
$S6=q'internal/image';
$T6=[];
$U6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(use_newlines => 1)
  ->partial_image(\'ni:/lib/ni\', \'ni:/lib/image\');
$q->write(\\*STDOUT);
0;';
$V6=bless({$M1,$T6,$k1,$U6},$f1);
$W6=q'run';
$X6=[];
$Y6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';
$Z6=bless({$M1,$X6,$k1,$Y6},$f1);
$c7={$O6,$R6,$S6,$V6,$W6,$Z6};
$d7=q'/lib/ni_main.b';
$e7=bless({$d,$N6,$Y,$z1,$y1,$z1,$i1,$c7,$g,$d7},$O);
$f7=q'/lib/slice::ctors';
$g7=[$e7];
$h7=bless({$d,$M6,$g,$L6,$h,$g7},$o);
$i7=q'/lib/ni.c::ctors';
$j7={$c,$E6,$G6,$h7};
$$t[0]=$z4;
$$J2[0]=$Y2;
*$m2=\&$k2;
*$l2=\&$i2;
$t1->apply_unsafe($A);
$t1->apply_unsafe($i);
$t1->apply_unsafe($k);
$t1->apply_unsafe($N);
$t1->apply_unsafe($l);
$t1->apply_unsafe($f1);
$t1->apply_unsafe($m);
$t1->apply_unsafe($n);
$t1->apply_unsafe($o);
$t1->apply_unsafe($O);
$t1->apply_unsafe($p);
$t1->apply_unsafe($K);
$t1->apply_unsafe($q);
$t1->apply_unsafe($u);
$t1->apply_unsafe($e);
$t1->apply_unsafe($r);
$I1->apply_unsafe($f1);
$S1->apply_unsafe($f1);
$f2->apply_unsafe($A);
$f2->apply_unsafe($i);
$f2->apply_unsafe($k);
$f2->apply_unsafe($N);
$f2->apply_unsafe($l);
$f2->apply_unsafe($m);
$f2->apply_unsafe($n);
$f2->apply_unsafe($o);
$f2->apply_unsafe($O);
$f2->apply_unsafe($p);
$f2->apply_unsafe($K);
$f2->apply_unsafe($q);
$f2->apply_unsafe($u);
$f2->apply_unsafe($e);
$f2->apply_unsafe($r);
$s2->apply_unsafe($O);
$z2->apply_unsafe($O);
$H2->apply_unsafe($O);
$S2->apply_unsafe($A);
$S2->apply_unsafe($i);
$S2->apply_unsafe($M);
$S2->apply_unsafe($k);
$S2->apply_unsafe($N);
$S2->apply_unsafe($l);
$S2->apply_unsafe($m);
$S2->apply_unsafe($n);
$S2->apply_unsafe($o);
$S2->apply_unsafe($O);
$S2->apply_unsafe($p);
$S2->apply_unsafe($K);
$S2->apply_unsafe($q);
$S2->apply_unsafe($u);
$S2->apply_unsafe($e);
$S2->apply_unsafe($Q);
$S2->apply_unsafe($r);
$h3->apply_unsafe($K);
$o3->apply_unsafe($K);
$C3->apply_unsafe($A);
$C3->apply_unsafe($i);
$C3->apply_unsafe($k);
$C3->apply_unsafe($N);
$C3->apply_unsafe($l);
$C3->apply_unsafe($m);
$C3->apply_unsafe($n);
$C3->apply_unsafe($o);
$C3->apply_unsafe($p);
$C3->apply_unsafe($q);
$C3->apply_unsafe($u);
$C3->apply_unsafe($e);
$C3->apply_unsafe($r);
$K3->apply_unsafe($A);
$K3->apply_unsafe($i);
$K3->apply_unsafe($k);
$K3->apply_unsafe($N);
$K3->apply_unsafe($l);
$K3->apply_unsafe($m);
$K3->apply_unsafe($n);
$K3->apply_unsafe($o);
$K3->apply_unsafe($O);
$K3->apply_unsafe($p);
$K3->apply_unsafe($K);
$K3->apply_unsafe($q);
$K3->apply_unsafe($u);
$K3->apply_unsafe($e);
$K3->apply_unsafe($r);
$S3->apply_unsafe($A);
$S3->apply_unsafe($i);
$S3->apply_unsafe($k);
$S3->apply_unsafe($N);
$S3->apply_unsafe($l);
$S3->apply_unsafe($m);
$S3->apply_unsafe($n);
$S3->apply_unsafe($o);
$S3->apply_unsafe($p);
$S3->apply_unsafe($K);
$S3->apply_unsafe($q);
$S3->apply_unsafe($u);
$S3->apply_unsafe($e);
$S3->apply_unsafe($r);
$g4->apply_unsafe($A);
$g4->apply_unsafe($i);
$g4->apply_unsafe($k);
$g4->apply_unsafe($l);
$g4->apply_unsafe($m);
$g4->apply_unsafe($n);
$g4->apply_unsafe($o);
$g4->apply_unsafe($p);
$g4->apply_unsafe($q);
$g4->apply_unsafe($u);
$g4->apply_unsafe($e);
$g4->apply_unsafe($r);
$o4->apply_unsafe($A);
$o4->apply_unsafe($i);
$o4->apply_unsafe($k);
$o4->apply_unsafe($l);
$o4->apply_unsafe($m);
$o4->apply_unsafe($n);
$o4->apply_unsafe($o);
$o4->apply_unsafe($p);
$o4->apply_unsafe($q);
$o4->apply_unsafe($e);
$o4->apply_unsafe($r);
$w4->apply_unsafe($A);
$w4->apply_unsafe($i);
$w4->apply_unsafe($k);
$w4->apply_unsafe($l);
$w4->apply_unsafe($m);
$w4->apply_unsafe($n);
$w4->apply_unsafe($o);
$w4->apply_unsafe($p);
$w4->apply_unsafe($q);
$w4->apply_unsafe($e);
$w4->apply_unsafe($r);
$U4->apply_unsafe($M4);
$B6->apply_unsafe($M4);
$e7->apply_unsafe($L6);
@ni::named{keys%$j7}=values%$j7;
&$_($v)for@$w;
&$_($y)for@$z;
&$_($F)for@$G;
&$_($I)for@$J;
&$_($U)for@$V;
&$_($d1)for@$e1;
&$_($m1)for@$n1;
&$_($q1)for@$n1;
&$_($t1)for@$u1;
&$_($x1)for@$n1;
&$_($C1)for@$n1;
&$_($F1)for@$n1;
&$_($I1)for@$J1;
&$_($P1)for@$n1;
&$_($S1)for@$T1;
&$_($V1)for@$W1;
&$_($Y1)for@$n1;
&$_($c2)for@$n1;
&$_($f2)for@$g2;
&$_($i2)for@$n1;
&$_($k2)for@$n1;
&$_($s2)for@$t2;
&$_($w2)for@$n1;
&$_($z2)for@$A2;
&$_($E2)for@$n1;
&$_($H2)for@$I2;
&$_($K2)for@$L2;
&$_($P2)for@$n1;
&$_($S2)for@$T2;
&$_($V2)for@$W2;
&$_($Y2)for@$Z2;
&$_($e3)for@$n1;
&$_($h3)for@$i3;
&$_($l3)for@$n1;
&$_($o3)for@$p3;
&$_($r3)for@$s3;
&$_($x3)for@$n1;
&$_($z3)for@$n1;
&$_($C3)for@$D3;
&$_($H3)for@$n1;
&$_($K3)for@$L3;
&$_($P3)for@$n1;
&$_($S3)for@$T3;
&$_($V3)for@$W3;
&$_($Z3)for@$n1;
&$_($d4)for@$n1;
&$_($g4)for@$h4;
&$_($l4)for@$n1;
&$_($o4)for@$p4;
&$_($t4)for@$n1;
&$_($w4)for@$x4;
&$_($z4)for@$A4;
&$_($C4)for@$D4;
&$_($G4)for@$H4;
&$_($K4)for@$L4;
&$_($R4)for@$n1;
&$_($U4)for@$V4;
&$_($c5)for@$n1;
&$_($g5)for@$n1;
&$_($k5)for@$n1;
&$_($o5)for@$n1;
&$_($s5)for@$n1;
&$_($w5)for@$n1;
&$_($A5)for@$n1;
&$_($E5)for@$n1;
&$_($I5)for@$n1;
&$_($M5)for@$n1;
&$_($Q5)for@$n1;
&$_($U5)for@$n1;
&$_($Y5)for@$n1;
&$_($e6)for@$n1;
&$_($i6)for@$n1;
&$_($m6)for@$n1;
&$_($q6)for@$n1;
&$_($u6)for@$n1;
&$_($y6)for@$n1;
&$_($B6)for@$C6;
&$_($E6)for@$F6;
&$_($J6)for@$K6;
&$_($R6)for@$n1;
&$_($V6)for@$n1;
&$_($Z6)for@$n1;
&$_($e7)for@$f7;
&$_($h7)for@$i7;
ni->run(@ARGV);
__DATA__
