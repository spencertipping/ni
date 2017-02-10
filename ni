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
$ni::self = bless {named => {}}, '/lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->resolve($_[0]) : $ni::self}
sub ni::eval {eval shift}
*{'/lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'/lib/fn::(bool'} = sub {1};
*{'/lib/fn::(('}    = sub {};
*{'/lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'/lib/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/lib/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
sub fn($);
_
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/module.c';$k={$h,1,$j,1};$l=q'/lib/behavior.c';$m=q'/lib/branch.c';$n=q'/lib/doc.c';$o=q'/lib/fn.c';$p=q'/lib/image.c';$q=q'/lib/ni.c';$r=q'/lib/slice.c';$s=q'/lib/tag.c';$t=q'/object.c';$u=q'/unix/cat.c';$v=q'/unix/fd.c';$w=q'/unix/fifo.c';$x=q'/unix/file.c';$y=q'/unix/io.c';$z=q'/unix/pid.c';$A=q'/unix/pipeline.c';$B=q'/unix/str.c';$C={$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$j,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1};$D={$r,1};$E={$l,1,$m,1,$r,1,$s,1};$F=[undef];$G=q'/metaclass';$H=bless({$c,$E,$f,$l,$g,$F},$G);$I=q'/metaclass::ctors';$J=[$H];$K=bless({$c,$D,$f,$r,$g,$J},$G);$L=q'/metaclass::ctors';$M=q'/lib/slice';$N={$M,1};$O=q'/lib/behavior';$P=q'/lib/branch';$Q=q'/lib/tag';$R={$O,1,$P,1,$M,1,$Q,1};$S=q'/class';$T=q'/lib/doc';$U=q'/lib/fn';$V=q'/lib/image';$W=q'/lib/ni';$X=q'/module';$Y=q'/object';$Z=q'/unix/cat';$c1=q'/unix/fd';$d1=q'/unix/fifo';$e1=q'/unix/file';$f1=q'/unix/io';$g1=q'/unix/pid';$h1=q'/unix/pipeline';$i1=q'/unix/pipeline.c';$j1=q'/unix/str';$k1={$S,1,$h,1,$O,1,$l,1,$P,1,$m,1,$T,1,$n,1,$U,1,$o,1,$V,1,$p,1,$W,1,$q,1,$M,1,$r,1,$Q,1,$s,1,$G,1,$d,1,$X,1,$j,1,$Y,1,$t,1,$Z,1,$u,1,$c1,1,$v,1,$d1,1,$w,1,$e1,1,$x,1,$f1,1,$y,1,$g1,1,$z,1,$h1,1,$i1,1,$j1,1,$B,1};$l1={};$m1=q'ctor';$n1=undef;$o1=q'dtor';$p1=q'methods';$q1=q'class';$r1={$o,1};$s1=[undef];$t1=bless({$c,$r1,$f,$o,$g,$s1},$G);$u1=q'/metaclass::ctors';$v1={$U,1};$w1={};$x1=q'DESTROY';$y1=q'code';$z1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$A1=q'proto';$B1=q'';$C1=bless({$y1,$z1,$A1,$B1},$U);$D1=q'/lib/fn::ctors';$E1=q'new';$F1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$G1=bless({$y1,$F1,$A1,$B1},$U);$H1={$x1,$C1,$E1,$G1};$I1=q'/lib/instantiable.b';$J1=bless({$c,$w1,$p1,$H1,$f,$I1},$M);$K1=q'/unix/pipeline.c';$L1=q'/lib/slice::ctors';$M1={};$N1=q'shift->compile';$O1=bless({$y1,$N1,$A1,$B1},$U);$P1=q'compile';$Q1=q'local $@;
my $self = shift;
$$self{proto} ||= \'\';
$$self{fn} = ni::eval "sub $$self{proto} {$$self{code}\\n}";
die "ni:/lib/fn: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$R1=bless({$y1,$Q1,$A1,$B1},$U);$S1=q'instantiate';$T1=q'my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : \'\';
+{code        => $code,
  proto       => $proto,
  annotations => [@_]};';$U1=bless({$y1,$T1,$A1,$B1},$U);$V1={$P1,$R1,$S1,$U1};$W1=q'/lib/fn_init.b';$X1=bless({$c,$M1,$m1,$O1,$o1,$n1,$p1,$V1,$f,$W1},$M);$Y1=q'/lib/slice::ctors';$Z1={};$c2=q'annotations';$d2=[];$e2=q'shift->{\'annotations\'}';$f2=bless({$c2,$d2,$y1,$e2,$A1,$B1},$U);$g2=[];$h2=q'shift->{\'code\'}';$i2=bless({$c2,$g2,$y1,$h2,$A1,$B1},$U);$j2=q'fn';$k2=[];$l2=q'shift->{\'fn\'}';$m2=bless({$c2,$k2,$y1,$l2,$A1,$B1},$U);$n2={$c2,$f2,$y1,$i2,$j2,$m2};$o2=q'/lib/fn_ro.b';$p2=bless({$c,$Z1,$m1,$n1,$o1,$n1,$p1,$n2,$f,$o2},$M);$q2=q'/lib/slice::ctors';$r2={};$s2=q'(""';$t2=[];$u2=q'shift->{code}';$v2=bless({$c2,$t2,$y1,$u2,$A1,$B1},$U);$w2=q'(eq';$x2=[];$y2=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])';$z2=bless({$c2,$x2,$y1,$y2,$A1,$B1},$U);$A2={$s2,$v2,$w2,$z2};$B2=q'/lib/fn_ops.b';$C2=bless({$c,$r2,$m1,$n1,$o1,$n1,$p1,$A2,$f,$B2},$M);$D2=q'/lib/slice::ctors';$E2={};$F2=q'serialize';$G2=[];$H2=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$I2=bless({$c2,$G2,$y1,$H2,$A1,$B1},$U);$J2={$F2,$I2};$K2=q'/lib/fn_serialize.b';$L2=bless({$c,$E2,$m1,$n1,$o1,$n1,$p1,$J2,$f,$K2},$M);$M2=q'/lib/slice::ctors';$N2=[undef,$J1,$X1,$p2,$C2,$L2];$O2=bless({$c,$v1,$f,$U,$g,$N2},$o);$P2=q'/lib/fn.c::ctors';$Q2=q'ni \'ni:\' . ref shift';$R2=bless({$y1,$Q2,$A1,$B1},$U);$S2={$q1,$R2};$T2=q'/lib/instance.b';$U2=bless({$c,$l1,$m1,$n1,$o1,$n1,$p1,$S2,$f,$T2},$M);$V2=q'/unix/pipeline.c';$W2=q'/lib/slice::ctors';$X2=[$U2];$Y2=bless({$c,$k1,$f,$Y,$g,$X2},$t);$Z2=q'/object.c::ctors';$c3=[$Y2];$d3=bless({$c,$R,$f,$O,$g,$c3},$l);$e3=q'/lib/behavior.c::ctors';$f3={};$g3=q'my $s = shift; ni->def($s->name, $s)';$h3=bless({$y1,$g3,$A1,$B1},$U);$i3=q'$_[0]->namespace . ":" . $_[0]->{name}';$j3=bless({$y1,$i3,$A1,$B1},$U);$k3={$f,$j3};$l3=q'/lib/named.b';$m3=bless({$c,$f3,$m1,$h3,$o1,$n1,$p1,$k3,$f,$l3},$M);$n3=q'/unix/pipeline.c';$o3=q'/lib/slice::ctors';$p3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$q3=bless({$y1,$p3,$A1,$B1},$U);$r3=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$s3=bless({$y1,$r3,$A1,$B1},$U);$t3=q'/lib/slice::apply';$u3=q'/lib/slice::apply_unsafe';$v3={};$w3=q'apply';$x3=q'apply_unsafe';$y3={$w3,$q3,$x3,$s3};$z3=q'/lib/slice.b';$A3=bless({$c,$v3,$p1,$y3,$f,$z3},$M);$B3=q'/lib/slice::ctors';$C3={};$D3=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$E3=bless({$y1,$D3,$A1,$B1},$U);$F3={$S1,$E3};$G3=q'/lib/slice_init.b';$H3=bless({$c,$C3,$p1,$F3,$f,$G3},$M);$I3=q'/lib/slice::ctors';$J3={};$K3=[];$L3=q'local $_;
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
$g;';$M3=bless({$c2,$K3,$y1,$L3,$A1,$B1},$U);$N3={$F2,$M3};$O3=q'/lib/slice_serialize.b';$P3=bless({$c,$J3,$m1,$n1,$o1,$n1,$p1,$N3,$f,$O3},$M);$Q3=q'/lib/slice::ctors';$R3=[$d3,$m3,$A3,$H3,$P3];$S3=bless({$c,$N,$f,$M,$g,$R3},$r);$T3=q'/lib/slice.c::ctors';$U3={};$V3=q'doc';$W3=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:/lib/doc\')->new($name);';$X3=bless({$y1,$W3,$A1,$B1},$U);$Y3={$V3,$X3};$Z3=q'/lib/documentable.b';$c4=bless({$c,$U3,$m1,$n1,$o1,$n1,$p1,$Y3,$f,$Z3},$M);$d4=q'/unix/pipeline.c';$e4=q'/lib/slice::ctors';$f4=[undef,$c4];$g4=bless({$c,$C,$f,$t,$g,$f4},$G);$h4=q'/metaclass::ctors';$i4=[$g4,$J1];$j4=bless({$c,$k,$f,$j,$g,$i4},$G);$k4=q'/metaclass::ctors';$l4=[$j4];$m4=bless({$c,$i,$f,$h,$g,$l4},$G);$n4=q'/metaclass::ctors';$o4=q'/unix/pipeline.c';$p4={$S,1,$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$d,1,$j,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1,$z,1,$o4,1,$B,1};$q4=q'/unix/pipeline.c';$r4={$S,1,$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$G,1,$d,1,$X,1,$j,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1,$z,1,$q4,1,$B,1};$s4={$s,1};$t4=[$H];$u4=bless({$c,$s4,$f,$s,$g,$t4},$G);$v4=q'/metaclass::ctors';$w4={$Q,1};$x4={};$y4=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$z4=bless({$y1,$y4,$A1,$B1},$U);$A4={$w3,$z4};$B4=q'/lib/tag.b';$C4=bless({$c,$x4,$m1,$n1,$o1,$n1,$p1,$A4,$f,$B4},$M);$D4=q'/lib/slice::ctors';$E4={};$F4=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$G4=bless({$y1,$F4,$A1,$B1},$U);$H4={$S1,$G4};$I4=q'/lib/tag_init.b';$J4=bless({$c,$E4,$m1,$n1,$o1,$n1,$p1,$H4,$f,$I4},$M);$K4=q'/lib/slice::ctors';$L4=[$d3,$m3,$C4,$J4];$M4=bless({$c,$w4,$f,$Q,$g,$L4},$s);$N4=q'/lib/tag.c::ctors';$O4=q'/lib/perlbranch.b';$P4={};$Q4=q'add';$R4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$S4=bless({$y1,$R4,$A1,$B1},$U);$T4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$U4=bless({$y1,$T4,$A1,$B1},$U);$V4={$Q4,$S4,$w3,$U4};$W4=q'/lib/branch.b';$X4=bless({$c,$P4,$m1,$n1,$o1,$n1,$p1,$V4,$f,$W4},$M);$Y4=q'/unix/pipeline.c';$Z4=q'/lib/slice::ctors';$c5={};$d5=q'namespace';$e5=q'\'ni\'';$f5=bless({$y1,$e5,$A1,$B1},$U);$g5={$d5,$f5};$h5=q'/lib/named_in_ni.b';$i5=bless({$c,$c5,$m1,$n1,$o1,$n1,$p1,$g5,$f,$h5},$M);$j5=q'/unix/pipeline.c';$k5=q'/lib/slice::ctors';$l5={};$m5=q'package';$n5=q'shift->{name}';$o5=bless({$y1,$n5,$A1,$B1},$U);$p5={$m5,$o5};$q5=q'/lib/namespaced.b';$r5=bless({$c,$l5,$m1,$n1,$o1,$n1,$p1,$p5,$f,$q5},$M);$s5=q'/unix/pipeline.c';$t5=q'/lib/slice::ctors';$u5={};$v5=q'resolve';$w5=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$x5=bless({$y1,$w5,$A1,$B1},$U);$y5={$v5,$x5};$z5=q'/lib/resolver.b';$A5=bless({$c,$u5,$m1,$n1,$o1,$n1,$p1,$y5,$f,$z5},$M);$B5=q'/unix/pipeline.c';$C5=q'/lib/slice::ctors';$D5=[$X4,$m3,$i5,$r5,$A5];$E5=bless({$f,$O4,$g,$D5},$Q);$F5=q'/lib/tag::ctors';$G5={};$H5=q'my $s = shift; $s->apply($s->package)';$I5=bless({$y1,$H5,$A1,$B1},$U);$J5=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$K5=bless({$y1,$J5,$A1,$B1},$U);$L5={$S1,$K5};$M5=q'/lib/class_init.b';$N5=bless({$c,$G5,$m1,$I5,$o1,$n1,$p1,$L5,$f,$M5},$M);$O5=q'/unix/pipeline.c';$P5=q'/lib/slice::ctors';$Q5={$m,1};$R5=[$H];$S5=bless({$c,$Q5,$f,$m,$g,$R5},$G);$T5=q'/metaclass::ctors';$U5={$P,1};$V5={};$W5=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$X5=bless({$y1,$W5,$A1,$B1},$U);$Y5={$S1,$X5};$Z5=q'/lib/branch_init.b';$c6=bless({$c,$V5,$m1,$n1,$o1,$n1,$p1,$Y5,$f,$Z5},$M);$d6=q'/lib/slice::ctors';$e6=[$d3,$m3,$X4,$c6,undef];$f6=bless({$c,$U5,$f,$P,$g,$e6},$m);$g6=q'/lib/branch.c::ctors';$h6=q'/unix/pipeline.c';$i6={$S,1,$h,1,$l,1,$P,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$G,1,$d,1,$X,1,$j,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1,$z,1,$h6,1,$B,1};$j6=q'/lib/definition.b';$k6={};$l6=q'def';$m6=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$n6=bless({$y1,$m6,$A1,$B1},$U);$o6={$l6,$n6};$p6=q'/lib/classdef.b';$q6=bless({$c,$k6,$m1,$n1,$o1,$n1,$p1,$o6,$f,$p6},$M);$r6=q'/unix/pipeline.c';$s6=q'/lib/slice::ctors';$t6={};$u6=q'ro';$v6=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$w6=bless({$y1,$v6,$A1,$B1},$U);$x6=q'rw';$y6=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$z6=bless({$y1,$y6,$A1,$B1},$U);$A6={$u6,$w6,$x6,$z6};$B6=q'/lib/accessor.b';$C6=bless({$c,$t6,$m1,$n1,$o1,$n1,$p1,$A6,$f,$B6},$M);$D6=q'/unix/pipeline.c';$E6=q'/lib/slice::ctors';$F6={};$G6=q'shift->name';$H6=bless({$y1,$G6,$A1,$B1},$U);$I6={$s2,$H6};$J6=q'/lib/name_as_string.b';$K6=bless({$c,$F6,$m1,$n1,$o1,$n1,$p1,$I6,$f,$J6},$M);$L6=q'/unix/pipeline.c';$M6=q'/lib/slice::ctors';$N6={};$O6=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);';$P6=bless({$y1,$O6,$A1,$B1},$U);$Q6={$w2,$P6};$R6=q'/lib/ref_eq.b';$S6=bless({$c,$N6,$m1,$n1,$o1,$n1,$p1,$Q6,$f,$R6},$M);$T6=q'/unix/pipeline.c';$U6=q'/lib/slice::ctors';$V6=[$q6,$C6,$K6,$S6];$W6=bless({$c,$i6,$f,$j6,$g,$V6},$P);$X6=q'/lib/branch::ctors';$Y6=[$E5,$N5,$Y2,$W6];$Z6=bless({$c,$r4,$f,$X,$g,$Y6},$j);$c7=q'/module.c::ctors';$d7={};$e7=q'child';$f7=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$g7=bless({$y1,$f7,$A1,$B1},$U);$h7={$e7,$g7};$i7=q'/lib/subclass.b';$j7=bless({$c,$d7,$m1,$n1,$o1,$n1,$p1,$h7,$f,$i7},$M);$k7=q'/unix/pipeline.c';$l7=q'/lib/slice::ctors';$m7=[$Z6,$J1,$Z6,$j7];$n7=bless({$c,$p4,$f,$S,$g,$m7},$h);$o7=q'/class.c::ctors';$p7=[$n7];$q7=bless({$c,$e,$f,$d,$g,$p7},$G);$r7=q'/metaclass::ctors';$s7={$G,1};$t7=[$E5,$J1,$N5,$Z6];$u7=bless({$c,$s7,$f,$G,$g,$t7},$d);$v7=q'/metaclass.c::ctors';$w7={$q,1};$x7=[$g4];$y7=bless({$c,$w7,$f,$q,$g,$x7},$G);$z7=q'/metaclass::ctors';$A7={$W,1};$B7={};$C7=q'is_mutable';$D7=[];$E7=q'$0 ne "-" && -w $0';$F7=bless({$c2,$D7,$y1,$E7,$A1,$B1},$U);$G7=q'modify';$H7=[];$I7=q'my ($self, $fn) = @_;
# TODO: replace all of this with a generalized "atomic-update" function
# against UNIX files.
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
rename $r, $0 or die "ni: failed to rename: $!";';$J7=bless({$c2,$H7,$y1,$I7,$A1,$B1},$U);$K7={$C7,$F7,$G7,$J7};$L7=q'/lib/ni_self.b';$M7=bless({$c,$B7,$m1,$n1,$o1,$n1,$p1,$K7,$f,$L7},$M);$N7=q'/lib/slice::ctors';$O7={};$P7=q'exists';$Q7=[];$R7=q'exists $_[0]->{named}{$_[1]}';$S7=bless({$c2,$Q7,$y1,$R7,$A1,$B1},$U);$T7=q'quoted';$U7=[];$V7=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$W7=bless({$c2,$U7,$y1,$V7,$A1,$B1},$U);$X7={$P7,$S7,$T7,$W7};$Y7=q'/lib/ni_image.b';$Z7=bless({$c,$O7,$m1,$n1,$o1,$n1,$p1,$X7,$f,$Y7},$M);$c8=q'/lib/slice::ctors';$d8={};$e8=q'--internal/+=';$f8=[];$g8=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$h8=bless({$c2,$f8,$y1,$g8,$A1,$B1},$U);$i8=q'--internal/eval';$j8=[];$k8=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$l8=bless({$c2,$j8,$y1,$k8,$A1,$B1},$U);$m8=q'--internal/image';$n8=[];$o8=q'shift->quoted->write(\\*STDOUT);
0;';$p8=bless({$c2,$n8,$y1,$o8,$A1,$B1},$U);$q8=q'--internal/test';$r8=[];$s8=q'my $self = shift;
my @tests = map ni($_)->tests, grep /^ni\\.doc:/, keys %{$$self{named}};
my $fails = 0;
print STDERR scalar(@tests) . " test(s)\\n";
my %names = %{ni->{named}};
for my $t (@tests) {
  %{ni->{named}} = %names;
  &$t or $fails += print STDERR "FAIL: $t->{code}\\n";
}
my $passed = @tests - $fails;
print STDERR "$passed test(s) passed\\n";
!!$fails;';$t8=bless({$c2,$r8,$y1,$s8,$A1,$B1},$U);$u8=q'run';$v8=[];$w8=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$x8=bless({$c2,$v8,$y1,$w8,$A1,$B1},$U);$y8={$e8,$h8,$i8,$l8,$m8,$p8,$q8,$t8,$u8,$x8};$z8=q'/lib/ni_main.b';$A8=bless({$c,$d8,$m1,$n1,$o1,$n1,$p1,$y8,$f,$z8},$M);$B8=q'/lib/slice::ctors';$C8={};$D8=[];$E8=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$F8=bless({$c2,$D8,$y1,$E8,$A1,$B1},$U);$G8=q'resolver_for';$H8=[];$I8=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$J8=bless({$c2,$H8,$y1,$I8,$A1,$B1},$U);$K8={$v5,$F8,$G8,$J8};$L8=q'/lib/ni_resolver.b';$M8=bless({$c,$C8,$m1,$n1,$o1,$n1,$p1,$K8,$f,$L8},$M);$N8=q'/lib/slice::ctors';$O8={};$P8=q'fork';$Q8=[];$R8=q'my ($class, $fn) = @_;
my $stdin  = ni(\'ni:/unix/fifo\')->new;
my $stdout = ni(\'ni:/unix/fifo\')->new;
my $stderr = ni(\'ni:/unix/fifo\')->new;
my $pid    = fork;
die "ni:/unix/pid.c: failed to fork: $!" unless defined $pid;

return ni(\'ni:/unix/pid\')->new($pid,
  $stdin->write_side,
  $stdout->read_side,
  $stderr->read_side) if $pid;

$stdin->read_side;
$stdout->write_side;
$stderr->write_side;
exit &$fn($stdin, $stdout, $stderr);';$S8=bless({$c2,$Q8,$y1,$R8,$A1,$B1},$U);$T8=q'fork_exec';$U8=[];$V8=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$W8=bless({$c2,$U8,$y1,$V8,$A1,$B1},$U);$X8={$P8,$S8,$T8,$W8};$Y8=q'/lib/ni_pid_ctors';$Z8=bless({$c,$O8,$m1,$n1,$o1,$n1,$p1,$X8,$f,$Y8},$M);$c9=q'/lib/slice::ctors';$d9=[$Y2,$M7,$Z7,$A8,$M8,$Z8];$e9=bless({$c,$A7,$f,$W,$g,$d9},$q);$f9=q'/lib/ni.c::ctors';$g9=q'named';$h9=q'ni.doc:/class';$i9={$n,1};$j9=[$g4];$k9=bless({$c,$i9,$f,$n,$g,$j9},$G);$l9=q'/metaclass::ctors';$m9={$T,1};$n9={};$o9=q'shift; +{name => shift, doc => []}';$p9=bless({$y1,$o9,$A1,$B1},$U);$q9={$S1,$p9};$r9=q'/lib/doc_init.b';$s9=bless({$c,$n9,$m1,$n1,$o1,$n1,$p1,$q9,$f,$r9},$M);$t9=q'/lib/slice::ctors';$u9={};$v9=q'\'ni.doc\'';$w9=bless({$y1,$v9,$A1,$B1},$U);$x9={$d5,$w9};$y9=q'/lib/doc_namespace.b';$z9=bless({$c,$u9,$m1,$n1,$o1,$n1,$p1,$x9,$f,$y9},$M);$A9=q'/lib/slice::ctors';$B9={};$C9=q'AUTOLOAD';$D9=q'my $self = shift;
my $method = ${__PACKAGE__ . "::AUTOLOAD"};
push @{$$self{doc}}, [$method, @_];
$self;';$E9=bless({$y1,$D9,$A1,$B1},$U);$F9={$C9,$E9};$G9=q'/lib/doc_define.b';$H9=bless({$c,$B9,$m1,$n1,$o1,$n1,$p1,$F9,$f,$G9},$M);$I9=q'/lib/slice::ctors';$J9={};$K9=q'eg';$L9=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$M9=bless({$y1,$L9,$A1,$B1},$U);$N9=q'tests';$O9=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$P9=bless({$y1,$O9,$A1,$B1},$U);$Q9={$K9,$M9,$N9,$P9};$R9=q'/lib/doc_test.b';$S9=bless({$c,$J9,$m1,$n1,$o1,$n1,$p1,$Q9,$f,$R9},$M);$T9=q'/lib/slice::ctors';$U9=[$Y2,$m3,$s9,$z9,$H9,$S9];$V9=bless({$c,$m9,$f,$T,$g,$U9},$n);$W9=q'/lib/doc.c::ctors';$X9=q'/lib/doc::synopsis';$Y9=q'
    ni(\'ni:/object\')->child(\'/message\')
      ->add(\'/behaviorname.b\')          # add existing behavior
      ->def(\'/message_init.b\',          # define new behavior
        instantiate => fn q{            # called from ->new()
          my ($class, $message) = @_;
          +{message => $message};       # return object to be blessed
        })
      ->def(\'/behaviorname.b\',          # define another behavior
        method1 => fn q{
          my $self = shift;
          print "message for you sir! \'" . $$self{message} . "\'\\n";
        });
    ni(\'ni:/child\')->new(\'hello world!\')->method1;
  ';$Z9=[$X9,$Y9];$ca=q'/lib/doc::description';$da=q'ni:/class is at the core of ni\'s object-oriented system, along with core
      classes like ni:/object and ni:/metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$ea=[$ca,$da];$fa=q'/lib/doc::behaviors';$ga=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:/lib/slice, which represents a set of methods you can add to a
      package. TODO...';$ha=[$fa,$ga];$ia=q'/lib/doc::classes';$ja=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$ka=q'TODO...';$la=[$ia,$ja,$ka];$ma=[$Z9,$ea,$ha,$la];$na=bless({$V3,$ma,$f,$S},$T);$oa=q'/lib/doc::ctors';$pa=q'ni.doc:/lib/doc';$qa=q'/lib/doc::synopsis';$ra=q'
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$sa=[$qa,$ra];$ta=q'/lib/doc::description';$ua=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$va=q'Documentation objects are internally represented as arrays of quoted
      method calls:';$wa=q'my $doc = ni("ni:/lib/doc")->new("foo");
# TODO: make this into a test, but first we need a testing DSL
$doc->foo("bar bif baz");
# state is now [["foo", ["bar bif baz"]]]';$xa=bless({$y1,$wa,$A1,$B1},$U);$ya=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":';$za=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$Aa=bless({$y1,$za,$A1,$B1},$U);$Ba=[$ta,$ua,$va,$K9,$xa,$ya,$K9,$Aa];$Ca=[$sa,$Ba];$Da=bless({$V3,$Ca,$f,$T},$T);$Ea=q'ni.doc:/unix/cat';$Fa=q'/lib/doc::synopsis';$Ga=q'
    my $combined = ni(\'ni:/unix/cat\')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into($destination_io);
  ';$Ha=[$Fa,$Ga];$Ia=q'/lib/doc::description';$Ja=q'Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.';$Ka=[$Ia,$Ja];$La=[];$Ma=q'my $io1 = ni("str:foo");
my $io2 = ni("str:bar");
($io1 + $io2)->read($_, 128) == 8 && $_ eq "foo\\nbar\\n";';$Na=bless({$c2,$La,$y1,$Ma,$A1,$B1},$U);$Oa=[$K9,$Na];$Pa=[$Ha,$Ka,$Oa];$Qa=bless({$V3,$Pa,$f,$Z},$T);$Ra=q'ni:/class';$Sa=q'ni:/class.c';$Ta=q'ni:/lib/accessor.b';$Ua=q'ni:/lib/behavior';$Va=q'ni:/lib/behavior.c';$Wa=q'ni:/lib/branch';$Xa=q'ni:/lib/branch.b';$Ya=q'ni:/lib/branch.c';$Za=q'ni:/lib/branch_init.b';$cb=q'ni:/lib/class_init.b';$db=q'ni:/lib/classdef.b';$eb=q'ni:/lib/definition.b';$fb=q'ni:/lib/doc';$gb=q'ni:/lib/doc.c';$hb=q'ni:/lib/doc_define.b';$ib=q'ni:/lib/doc_init.b';$jb=q'ni:/lib/doc_namespace.b';$kb=q'ni:/lib/doc_test.b';$lb=q'ni:/lib/documentable.b';$mb=q'ni:/lib/fn';$nb=q'ni:/lib/fn.c';$ob=q'ni:/lib/fn_init.b';$pb=q'ni:/lib/fn_ops.b';$qb=q'ni:/lib/fn_ro.b';$rb=q'ni:/lib/fn_serialize.b';$sb=q'ni:/lib/global_static';$tb=q'/lib/global_static';$ub=q'main';$vb={$tb,1,$ub,1};$wb=q'/lib/global_static';$xb={};$yb=[];$zb=q'ni(\'ni:/lib/fn\')->new(@_)';$Ab=q'($)';$Bb=bless({$c2,$yb,$y1,$zb,$A1,$Ab},$U);$Cb=q'fp';$Db=[];$Eb=q'ni(\'ni:/lib/fn\')->new(@_)';$Fb=q'($$)';$Gb=bless({$c2,$Db,$y1,$Eb,$A1,$Fb},$U);$Hb={$j2,$Bb,$Cb,$Gb};$Ib=q'/lib/static_fn.b';$Jb=bless({$c,$xb,$m1,$n1,$o1,$n1,$p1,$Hb,$f,$Ib},$M);$Kb=q'/lib/global_static';$Lb=q'/lib/slice::ctors';$Mb=[$Jb];$Nb=bless({$c,$vb,$f,$wb,$g,$Mb},$X);$Ob=q'/module::ctors';$Pb=q'ni:/lib/image';$Qb={$p,1};$Rb=[$g4];$Sb=bless({$c,$Qb,$f,$p,$g,$Rb},$G);$Tb=q'/metaclass::ctors';$Ub={$V,1};$Vb={};$Wb=[];$Xb=q'my $class = shift;
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
  ordering     => []};';$Yb=bless({$c2,$Wb,$y1,$Xb,$A1,$B1},$U);$Zb={$S1,$Yb};$cc=q'/lib/image_init.b';$dc=bless({$c,$Vb,$m1,$n1,$o1,$n1,$p1,$Zb,$f,$cc},$M);$ec=q'/lib/slice::ctors';$fc={};$gc=q'address';$hc=[];$ic=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$jc=bless({$c2,$hc,$y1,$ic,$A1,$B1},$U);$kc=q'allocate_gensym';$lc=[];$mc=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$nc=bless({$c2,$lc,$y1,$mc,$A1,$B1},$U);$oc=q'boot_side_effect';$pc=[];$qc=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$rc=bless({$c2,$pc,$y1,$qc,$A1,$B1},$U);$sc=q'circular_links';$tc=[];$uc=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$vc=bless({$c2,$tc,$y1,$uc,$A1,$B1},$U);$wc=q'finalizer';$xc=[];$yc=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$zc=bless({$c2,$xc,$y1,$yc,$A1,$B1},$U);$Ac=q'gensym';$Bc=[];$Cc=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$Dc=bless({$c2,$Bc,$y1,$Cc,$A1,$B1},$U);$Ec=q'is_circular';$Fc=[];$Gc=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$Hc=bless({$c2,$Fc,$y1,$Gc,$A1,$B1},$U);$Ic=q'quote';$Jc=[];$Kc=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$Lc=bless({$c2,$Jc,$y1,$Kc,$A1,$B1},$U);$Mc=q'quote_array';$Nc=[];$Oc=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$Pc=bless({$c2,$Nc,$y1,$Oc,$A1,$B1},$U);$Qc=q'quote_blessed';$Rc=[];$Sc=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$Tc=bless({$c2,$Rc,$y1,$Sc,$A1,$B1},$U);$Uc=q'quote_class';$Vc=[];$Wc=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$Xc=bless({$c2,$Vc,$y1,$Wc,$A1,$B1},$U);$Yc=q'quote_hash';$Zc=[];$cd=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$dd=bless({$c2,$Zc,$y1,$cd,$A1,$B1},$U);$ed=q'quote_object';$fd=[];$gd=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$hd=bless({$c2,$fd,$y1,$gd,$A1,$B1},$U);$id=q'quote_scalar';$jd=[];$kd=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$ld=bless({$c2,$jd,$y1,$kd,$A1,$B1},$U);$md=q'quote_value';$nd=[];$od=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$pd=bless({$c2,$nd,$y1,$od,$A1,$B1},$U);$qd=q'reconstruction';$rd=[];$sd=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$td=bless({$c2,$rd,$y1,$sd,$A1,$B1},$U);$ud=q'side_effect';$vd=[];$wd=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$xd=bless({$c2,$vd,$y1,$wd,$A1,$B1},$U);$yd=q'write';$zd=[];$Ad=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Bd=bless({$c2,$zd,$y1,$Ad,$A1,$B1},$U);$Cd={$gc,$jc,$kc,$nc,$oc,$rc,$sc,$vc,$wc,$zc,$Ac,$Dc,$Ec,$Hc,$Ic,$Lc,$Mc,$Pc,$Qc,$Tc,$Uc,$Xc,$Yc,$dd,$ed,$hd,$id,$ld,$md,$pd,$qd,$td,$ud,$xd,$yd,$Bd};$Dd=q'/lib/image_quoting.b';$Ed=bless({$c,$fc,$m1,$n1,$o1,$n1,$p1,$Cd,$f,$Dd},$M);$Fd=q'/lib/slice::ctors';$Gd=[$Y2,$dc,$Ed];$Hd=bless({$c,$Ub,$f,$V,$g,$Gd},$p);$Id=q'/lib/image.c::ctors';$Jd=q'ni:/lib/image.c';$Kd=q'ni:/lib/image_init.b';$Ld=q'ni:/lib/image_quoting.b';$Md=q'ni:/lib/instance.b';$Nd=q'ni:/lib/instantiable.b';$Od=q'ni:/lib/name_as_string.b';$Pd=q'ni:/lib/named.b';$Qd=q'ni:/lib/named_in_ni.b';$Rd=q'ni:/lib/namespaced.b';$Sd=q'ni:/lib/ni';$Td=q'ni:/lib/ni.c';$Ud=q'ni:/lib/ni_image.b';$Vd=q'ni:/lib/ni_main.b';$Wd=q'ni:/lib/ni_pid_ctors';$Xd=q'ni:/lib/ni_resolver.b';$Yd=q'ni:/lib/ni_self.b';$Zd=q'ni:/lib/ni_static';$ce=q'/lib/ni_static';$de=q'ni';$ee={$ce,1,$de,1};$fe={};$ge=q'abbrev';$he=[];$ie=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$je=bless({$c2,$he,$y1,$ie,$A1,$B1},$U);$ke=q'dor';$le=[];$me=q'defined $_[0] ? $_[0] : $_[1]';$ne=bless({$c2,$le,$y1,$me,$A1,$B1},$U);$oe=q'indent';$pe=[];$qe=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$re=bless({$c2,$pe,$y1,$qe,$A1,$B1},$U);$se=q'max';$te=[];$ue=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$ve=bless({$c2,$te,$y1,$ue,$A1,$B1},$U);$we=q'maxstr';$xe=[];$ye=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$ze=bless({$c2,$xe,$y1,$ye,$A1,$B1},$U);$Ae=q'mean';$Be=[];$Ce=q'sum(@_) / (@_ || 1)';$De=bless({$c2,$Be,$y1,$Ce,$A1,$B1},$U);$Ee=q'min';$Fe=[];$Ge=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$He=bless({$c2,$Fe,$y1,$Ge,$A1,$B1},$U);$Ie=q'minstr';$Je=[];$Ke=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Le=bless({$c2,$Je,$y1,$Ke,$A1,$B1},$U);$Me=q'sgr';$Ne=[];$Oe=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Pe=bless({$c2,$Ne,$y1,$Oe,$A1,$B1},$U);$Qe=q'sr';$Re=[];$Se=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Te=bless({$c2,$Re,$y1,$Se,$A1,$B1},$U);$Ue=q'sum';$Ve=[];$We=q'local $_; my $x = 0; $x += $_ for @_; $x';$Xe=bless({$c2,$Ve,$y1,$We,$A1,$B1},$U);$Ye=q'swap';$Ze=[];$cf=q'@_[0, 1] = @_[1, 0]';$df=bless({$c2,$Ze,$y1,$cf,$A1,$B1},$U);$ef={$ge,$je,$ke,$ne,$oe,$re,$se,$ve,$we,$ze,$Ae,$De,$Ee,$He,$Ie,$Le,$Me,$Pe,$Qe,$Te,$Ue,$Xe,$Ye,$df};$ff=q'/lib/ni_static_util.b';$gf=bless({$c,$fe,$m1,$n1,$o1,$n1,$p1,$ef,$f,$ff},$M);$hf=q'/lib/slice::ctors';$if=[$gf];$jf=bless({$c,$ee,$f,$ce,$g,$if},$X);$kf=q'ni:/lib/ni_static_util.b';$lf=q'ni:/lib/perlbranch.b';$mf=q'ni:/lib/ref_eq.b';$nf=q'ni:/lib/resolver.b';$of=q'ni:/lib/slice';$pf=q'ni:/lib/slice.b';$qf=q'ni:/lib/slice.c';$rf=q'ni:/lib/slice_init.b';$sf=q'ni:/lib/slice_serialize.b';$tf=q'ni:/lib/static_fn.b';$uf=q'ni:/lib/subclass.b';$vf=q'ni:/lib/tag';$wf=q'ni:/lib/tag.b';$xf=q'ni:/lib/tag.c';$yf=q'ni:/lib/tag_init.b';$zf=q'ni:/metaclass';$Af=q'ni:/metaclass.c';$Bf=q'ni:/module';$Cf=q'ni:/module.c';$Df=q'ni:/object';$Ef=q'ni:/object.c';$Ff=q'ni:/unix/cat';$Gf={$u,1};$Hf=q'/unix/pipeline.c';$If={$u,1,$v,1,$w,1,$x,1,$y,1,$z,1,$Hf,1,$B,1};$Jf=[$g4];$Kf=bless({$c,$If,$f,$y,$g,$Jf},$G);$Lf=q'/metaclass::ctors';$Mf=[$Kf];$Nf=bless({$c,$Gf,$f,$u,$g,$Mf},$G);$Of=q'/metaclass::ctors';$Pf={$Z,1};$Qf={$Z,1,$c1,1,$d1,1,$e1,1,$f1,1,$g1,1,$h1,1,$j1,1};$Rf={};$Sf=q'into';$Tf=[];$Uf=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Vf=bless({$c2,$Tf,$y1,$Uf,$A1,$B1},$U);$Wf={$Sf,$Vf};$Xf=q'/unix/io_stream.b';$Yf=bless({$c,$Rf,$m1,$n1,$o1,$n1,$p1,$Wf,$f,$Xf},$M);$Zf=q'/lib/slice::ctors';$cg={};$dg=q'(+';$eg=[];$fg=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$gg=bless({$c2,$eg,$y1,$fg,$A1,$B1},$U);$hg={$dg,$gg};$ig=q'/unix/io_constructors.b';$jg=bless({$c,$cg,$m1,$n1,$o1,$n1,$p1,$hg,$f,$ig},$M);$kg=q'/lib/slice::ctors';$lg={};$mg=q'(<>';$ng=[];$og=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$pg=bless({$c2,$ng,$y1,$og,$A1,$B1},$U);$qg=q'(@{}';$rg=[];$sg=q'my $self = shift; [<$self>]';$tg=bless({$c2,$rg,$y1,$sg,$A1,$B1},$U);$ug={$mg,$pg,$qg,$tg};$vg=q'/unix/io_readers.b';$wg=bless({$c,$lg,$m1,$n1,$o1,$n1,$p1,$ug,$f,$vg},$M);$xg=q'/lib/slice::ctors';$yg=[$Y2,$Yf,$jg,$wg];$zg=bless({$c,$Qf,$f,$f1,$g,$yg},$y);$Ag=q'/unix/io.c::ctors';$Bg={};$Cg=[];$Dg=q'shift; +{fs => [@_]}';$Eg=bless({$c2,$Cg,$y1,$Dg,$A1,$B1},$U);$Fg={$S1,$Eg};$Gg=q'/unix/cat_init.b';$Hg=bless({$c,$Bg,$m1,$n1,$o1,$n1,$p1,$Fg,$f,$Gg},$M);$Ig=q'/lib/slice::ctors';$Jg={};$Kg=q'read';$Lg=[];$Mg=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Ng=bless({$c2,$Lg,$y1,$Mg,$A1,$B1},$U);$Og={$Kg,$Ng};$Pg=q'/unix/cat_read.b';$Qg=bless({$c,$Jg,$m1,$n1,$o1,$n1,$p1,$Og,$f,$Pg},$M);$Rg=q'/lib/slice::ctors';$Sg=[$zg,$Hg,$Qg];$Tg=bless({$c,$Pf,$f,$Z,$g,$Sg},$u);$Ug=q'/unix/cat.c::ctors';$Vg=q'ni:/unix/cat.c';$Wg=q'ni:/unix/cat_init.b';$Xg=q'ni:/unix/cat_read.b';$Yg=q'ni:/unix/fd';$Zg={$v,1};$ch=[$Kf];$dh=bless({$c,$Zg,$f,$v,$g,$ch},$G);$eh=q'/metaclass::ctors';$fh={$c1,1};$gh={};$hh=q'fd';$ih=[];$jh=q'shift->{\'fd\'}';$kh=bless({$c2,$ih,$y1,$jh,$A1,$B1},$U);$lh={$hh,$kh};$mh=q'/unix/fd_readers.b';$nh=bless({$c,$gh,$m1,$n1,$o1,$n1,$p1,$lh,$f,$mh},$M);$oh=q'/lib/slice::ctors';$ph={};$qh=[];$rh=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$sh=bless({$c2,$qh,$y1,$rh,$A1,$B1},$U);$th={$S1,$sh};$uh=q'/unix/fd_init.b';$vh=bless({$c,$ph,$m1,$n1,$o1,$n1,$p1,$th,$f,$uh},$M);$wh=q'/lib/slice::ctors';$xh={};$yh=q'move_to';$zh=[];$Ah=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Bh=bless({$c2,$zh,$y1,$Ah,$A1,$B1},$U);$Ch={$yh,$Bh};$Dh=q'/unix/fd_shell.b';$Eh=bless({$c,$xh,$m1,$n1,$o1,$n1,$p1,$Ch,$f,$Dh},$M);$Fh=q'/lib/slice::ctors';$Gh={$c1,1,$d1,1,$e1,1,$g1,1,$h1,1};$Hh=q'/unix/has_fd.b';$Ih={};$Jh=[];$Kh=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Lh=bless({$c2,$Jh,$y1,$Kh,$A1,$B1},$U);$Mh=[];$Nh=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Oh=bless({$c2,$Mh,$y1,$Nh,$A1,$B1},$U);$Ph={$Kg,$Lh,$yd,$Oh};$Qh=q'/unix/fd_safeio.b';$Rh=bless({$c,$Ih,$m1,$n1,$o1,$n1,$p1,$Ph,$f,$Qh},$M);$Sh=q'/lib/slice::ctors';$Th=[$Rh];$Uh=bless({$c,$Gh,$f,$Hh,$g,$Th},$P);$Vh=q'/lib/branch::ctors';$Wh={};$Xh=q'read_fh';$Yh=[];$Zh=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$ci=bless({$c2,$Yh,$y1,$Zh,$A1,$B1},$U);$di=q'write_fh';$ei=[];$fi=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$gi=bless({$c2,$ei,$y1,$fi,$A1,$B1},$U);$hi={$Xh,$ci,$di,$gi};$ii=q'/unix/fd_io.b';$ji=bless({$c,$Wh,$m1,$n1,$o1,$n1,$p1,$hi,$f,$ii},$M);$ki=q'/lib/slice::ctors';$li=[$zg,$nh,$vh,$Eh,$Uh,$ji];$mi=bless({$c,$fh,$f,$c1,$g,$li},$v);$ni=q'/unix/fd.c::ctors';$oi=q'ni:/unix/fd.c';$pi=q'ni:/unix/fd_init.b';$qi=q'ni:/unix/fd_io.b';$ri=q'ni:/unix/fd_readers.b';$si=q'ni:/unix/fd_safeio.b';$ti=q'ni:/unix/fd_shell.b';$ui=q'ni:/unix/fifo';$vi={$w,1};$wi=[$Kf];$xi=bless({$c,$vi,$f,$w,$g,$wi},$G);$yi=q'/metaclass::ctors';$zi={$d1,1};$Ai={};$Bi=[];$Ci=q'shift->{\'read_fh\'}';$Di=bless({$c2,$Bi,$y1,$Ci,$A1,$B1},$U);$Ei=[];$Fi=q'shift->{\'write_fh\'}';$Gi=bless({$c2,$Ei,$y1,$Fi,$A1,$B1},$U);$Hi={$Xh,$Di,$di,$Gi};$Ii=q'/unix/fifo_io.b';$Ji=bless({$c,$Ai,$m1,$n1,$o1,$n1,$p1,$Hi,$f,$Ii},$M);$Ki=q'/lib/slice::ctors';$Li={};$Mi=[];$Ni=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Oi=bless({$c2,$Mi,$y1,$Ni,$A1,$B1},$U);$Pi={$S1,$Oi};$Qi=q'/unix/fifo_init.b';$Ri=bless({$c,$Li,$m1,$n1,$o1,$n1,$p1,$Pi,$f,$Qi},$M);$Si=q'/lib/slice::ctors';$Ti={};$Ui=q'read_side';$Vi=[];$Wi=q'my $self = shift; close $$self{write_fh}; $self';$Xi=bless({$c2,$Vi,$y1,$Wi,$A1,$B1},$U);$Yi=q'write_side';$Zi=[];$cj=q'my $self = shift; close $$self{read_fh};  $self';$dj=bless({$c2,$Zi,$y1,$cj,$A1,$B1},$U);$ej={$Ui,$Xi,$Yi,$dj};$fj=q'/unix/fifo_direction.b';$gj=bless({$c,$Ti,$m1,$n1,$o1,$n1,$p1,$ej,$f,$fj},$M);$hj=q'/lib/slice::ctors';$ij=[$zg,$Ji,$Ri,$Uh,$gj];$jj=bless({$c,$zi,$f,$d1,$g,$ij},$w);$kj=q'/unix/fifo.c::ctors';$lj=q'ni:/unix/fifo.c';$mj=q'ni:/unix/fifo_direction.b';$nj=q'ni:/unix/fifo_init.b';$oj=q'ni:/unix/fifo_io.b';$pj=q'ni:/unix/file';$qj={$x,1};$rj=[$Kf];$sj=bless({$c,$qj,$f,$x,$g,$rj},$G);$tj=q'/metaclass::ctors';$uj={$e1,1};$vj={};$wj=[];$xj=q'shift->{\'name\'}';$yj=bless({$c2,$wj,$y1,$xj,$A1,$B1},$U);$zj={$f,$yj};$Aj=q'/unix/file_readers.b';$Bj=bless({$c,$vj,$m1,$n1,$o1,$n1,$p1,$zj,$f,$Aj},$M);$Cj=q'/lib/slice::ctors';$Dj={};$Ej=[];$Fj=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Gj=bless({$c2,$Ej,$y1,$Fj,$A1,$B1},$U);$Hj={$S1,$Gj};$Ij=q'/unix/file_init.b';$Jj=bless({$c,$Dj,$m1,$n1,$o1,$n1,$p1,$Hj,$f,$Ij},$M);$Kj=q'/lib/slice::ctors';$Lj={};$Mj=[];$Nj=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Oj=bless({$c2,$Mj,$y1,$Nj,$A1,$B1},$U);$Pj=[];$Qj=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Rj=bless({$c2,$Pj,$y1,$Qj,$A1,$B1},$U);$Sj={$Xh,$Oj,$di,$Rj};$Tj=q'/unix/file_io.b';$Uj=bless({$c,$Lj,$m1,$n1,$o1,$n1,$p1,$Sj,$f,$Tj},$M);$Vj=q'/lib/slice::ctors';$Wj=[$zg,$Bj,$Jj,$Uh,$Uj];$Xj=bless({$c,$uj,$f,$e1,$g,$Wj},$x);$Yj=q'/unix/file.c::ctors';$Zj=q'ni:/unix/file.c';$ck=q'ni:/unix/file_init.b';$dk=q'ni:/unix/file_io.b';$ek=q'ni:/unix/file_readers.b';$fk=q'ni:/unix/has_fd.b';$gk=q'ni:/unix/io';$hk=q'ni:/unix/io.c';$ik=q'ni:/unix/io_constructors.b';$jk=q'ni:/unix/io_readers.b';$kk=q'ni:/unix/io_stream.b';$lk=q'ni:/unix/pid';$mk={$z,1};$nk=[$Kf];$ok=bless({$c,$mk,$f,$z,$g,$nk},$G);$pk=q'/metaclass::ctors';$qk={$g1,1};$rk={};$sk=q'pid';$tk=[];$uk=q'shift->{\'pid\'}';$vk=bless({$c2,$tk,$y1,$uk,$A1,$B1},$U);$wk=q'stderr';$xk=[];$yk=q'shift->{\'stderr\'}';$zk=bless({$c2,$xk,$y1,$yk,$A1,$B1},$U);$Ak=q'stdin';$Bk=[];$Ck=q'shift->{\'stdin\'}';$Dk=bless({$c2,$Bk,$y1,$Ck,$A1,$B1},$U);$Ek=q'stdout';$Fk=[];$Gk=q'shift->{\'stdout\'}';$Hk=bless({$c2,$Fk,$y1,$Gk,$A1,$B1},$U);$Ik={$sk,$vk,$wk,$zk,$Ak,$Dk,$Ek,$Hk};$Jk=q'/unix/pid_readers.b';$Kk=bless({$c,$rk,$m1,$n1,$o1,$n1,$p1,$Ik,$f,$Jk},$M);$Lk=q'/lib/slice::ctors';$Mk={};$Nk=[];$Ok=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Pk=bless({$c2,$Nk,$y1,$Ok,$A1,$B1},$U);$Qk={$S1,$Pk};$Rk=q'/unix/pid_init.b';$Sk=bless({$c,$Mk,$m1,$n1,$o1,$n1,$p1,$Qk,$f,$Rk},$M);$Tk=q'/lib/slice::ctors';$Uk={};$Vk={};$Wk=q'/unix/pid_wait.b';$Xk=bless({$c,$Uk,$m1,$n1,$o1,$n1,$p1,$Vk,$f,$Wk},$M);$Yk=q'/lib/slice::ctors';$Zk={};$cl=[];$dl=q'shift->{stdout}->read_fh';$el=bless({$c2,$cl,$y1,$dl,$A1,$B1},$U);$fl=[];$gl=q'shift->{stdin}->write_fh';$hl=bless({$c2,$fl,$y1,$gl,$A1,$B1},$U);$il={$Xh,$el,$di,$hl};$jl=q'/unix/pid_io.b';$kl=bless({$c,$Zk,$m1,$n1,$o1,$n1,$p1,$il,$f,$jl},$M);$ll=q'/lib/slice::ctors';$ml=[$zg,$Kk,$Sk,$Xk,$Uh,$kl];$nl=bless({$c,$qk,$f,$g1,$g,$ml},$z);$ol=q'/unix/pid.c::ctors';$pl=q'ni:/unix/pid.c';$ql=q'ni:/unix/pid_init.b';$rl=q'ni:/unix/pid_io.b';$sl=q'ni:/unix/pid_readers.b';$tl=q'ni:/unix/pid_wait.b';$ul=q'ni:/unix/pipeline';$vl=q'/unix/pipeline.c';$wl={$vl,1};$xl=q'/unix/pipeline.c';$yl=[$Kf];$zl=bless({$c,$wl,$f,$xl,$g,$yl},$G);$Al=q'/metaclass::ctors';$Bl={$h1,1};$Cl={};$Dl=[];$El=q'shift->{\'stdin\'}';$Fl=bless({$c2,$Dl,$y1,$El,$A1,$B1},$U);$Gl=[];$Hl=q'shift->{\'stdout\'}';$Il=bless({$c2,$Gl,$y1,$Hl,$A1,$B1},$U);$Jl={$Ak,$Fl,$Ek,$Il};$Kl=q'/unix/pipeline_ro.b';$Ll=bless({$c,$Cl,$m1,$n1,$o1,$n1,$p1,$Jl,$f,$Kl},$M);$Ml=q'/lib/slice::ctors';$Nl={};$Ol=[];$Pl=q'my $class  = shift;
my $stdin  = ni(\'ni:/unix/fifo\')->new;
my $stdout = ni(\'ni:/unix/fifo\')->new;
# TODO: stderr and multiplexing, which probably happens here

my @rs = ($stdin, @_);
my @ws = (@_, $stdout);
my $rv; vec($rv, fileno $_->read_fh,  1) = 1 for @rs;
my $wv; vec($wv, fileno $_->write_fh, 1) = 1 for @ws;

+{ps     => [@_],
  stdin  => $stdin,
  stdout => $stdout,
  rs => \\@rs, rv => $rv,
  ws => \\@ws, wv => $wv};';$Ql=bless({$c2,$Ol,$y1,$Pl,$A1,$B1},$U);$Rl={$S1,$Ql};$Sl=q'/unix/pipeline_init.b';$Tl=bless({$c,$Nl,$m1,$n1,$o1,$n1,$p1,$Rl,$f,$Sl},$M);$Ul=q'/lib/slice::ctors';$Vl={};$Wl=q'async_step';$Xl=[];$Yl=q'local $_;
my $self = shift;
my $rv = $$self{rv};
my $wv = $$self{wv};
my $ev = $$self{ev};
return $self unless select $rv, $wv, $ev, 0;
for my $i (0..$#{$$self{rs}}) {
  my $rfh = $$self{rs}[$i]->read_fh;
  my $wfh = $$self{ws}[$i]->write_fh;
  next unless vec $rv, fileno $rfh, 1 and vec $wv, fileno $wfh, 1;
  $$self{rs}[$i]->read($_, 8192);
  $$self{ws}[$i]->write($_);
}
$self;';$Zl=bless({$c2,$Xl,$y1,$Yl,$A1,$B1},$U);$cm={$Wl,$Zl};$dm=q'/unix/pipeline_async.b';$em=bless({$c,$Vl,$m1,$n1,$o1,$n1,$p1,$cm,$f,$dm},$M);$fm=q'/lib/slice::ctors';$gm={};$hm=[];$im=q'shift->{stdout}->read_fh';$jm=bless({$c2,$hm,$y1,$im,$A1,$B1},$U);$km=[];$lm=q'shift->{stdin}->write_fh';$mm=bless({$c2,$km,$y1,$lm,$A1,$B1},$U);$nm={$Xh,$jm,$di,$mm};$om=q'/unix/pipeline_io.b';$pm=bless({$c,$gm,$m1,$n1,$o1,$n1,$p1,$nm,$f,$om},$M);$qm=q'/lib/slice::ctors';$rm=[$zg,$Ll,$Tl,$em,$Uh,$pm];$sm=q'/unix/pipeline.c';$tm=bless({$c,$Bl,$f,$h1,$g,$rm},$sm);$um=q'/unix/pipeline.c::ctors';$vm=q'ni:/unix/pipeline.c';$wm=q'ni:/unix/pipeline_async.b';$xm=q'ni:/unix/pipeline_init.b';$ym=q'ni:/unix/pipeline_io.b';$zm=q'ni:/unix/pipeline_ro.b';$Am=q'ni:/unix/str';$Bm={$B,1};$Cm=[$Kf];$Dm=bless({$c,$Bm,$f,$B,$g,$Cm},$G);$Em=q'/metaclass::ctors';$Fm={$j1,1};$Gm={};$Hm=[];$Im=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Jm=bless({$c2,$Hm,$y1,$Im,$A1,$B1},$U);$Km={$S1,$Jm};$Lm=q'/unix/str_init.b';$Mm=bless({$c,$Gm,$m1,$n1,$o1,$n1,$p1,$Km,$f,$Lm},$M);$Nm=q'/lib/slice::ctors';$Om={};$Pm=[];$Qm=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Rm=bless({$c2,$Pm,$y1,$Qm,$A1,$B1},$U);$Sm=q'remaining';$Tm=[];$Um=q'my $self = shift; $$self{end} - $$self{start}';$Vm=bless({$c2,$Tm,$y1,$Um,$A1,$B1},$U);$Wm=[];$Xm=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Ym=bless({$c2,$Wm,$y1,$Xm,$A1,$B1},$U);$Zm={$Kg,$Rm,$Sm,$Vm,$yd,$Ym};$cn=q'/unix/str_io.b';$dn=bless({$c,$Om,$m1,$n1,$o1,$n1,$p1,$Zm,$f,$cn},$M);$en=q'/lib/slice::ctors';$fn=[$zg,$Mm,$dn];$gn=bless({$c,$Fm,$f,$j1,$g,$fn},$B);$hn=q'/unix/str.c::ctors';$in=q'ni:/unix/str.c';$jn=q'ni:/unix/str_init.b';$kn=q'ni:/unix/str_io.b';$ln={$h9,$na,$pa,$Da,$Ea,$Qa,$Ra,$n7,$Sa,$m4,$Ta,$C6,$Ua,$d3,$Va,$H,$Wa,$f6,$Xa,$X4,$Ya,$S5,$Za,$c6,$cb,$N5,$db,$q6,$eb,$W6,$fb,$V9,$gb,$k9,$hb,$H9,$ib,$s9,$jb,$z9,$kb,$S9,$lb,$c4,$mb,$O2,$nb,$t1,$ob,$X1,$pb,$C2,$qb,$p2,$rb,$L2,$sb,$Nb,$Pb,$Hd,$Jd,$Sb,$Kd,$dc,$Ld,$Ed,$Md,$U2,$Nd,$J1,$Od,$K6,$Pd,$m3,$Qd,$i5,$Rd,$r5,$Sd,$e9,$Td,$y7,$Ud,$Z7,$Vd,$A8,$Wd,$Z8,$Xd,$M8,$Yd,$M7,$Zd,$jf,$kf,$gf,$lf,$E5,$mf,$S6,$nf,$A5,$of,$S3,$pf,$A3,$qf,$K,$rf,$H3,$sf,$P3,$tf,$Jb,$uf,$j7,$vf,$M4,$wf,$C4,$xf,$u4,$yf,$J4,$zf,$u7,$Af,$q7,$Bf,$Z6,$Cf,$j4,$Df,$Y2,$Ef,$g4,$Ff,$Tg,$Vg,$Nf,$Wg,$Hg,$Xg,$Qg,$Yg,$mi,$oi,$dh,$pi,$vh,$qi,$ji,$ri,$nh,$si,$Rh,$ti,$Eh,$ui,$jj,$lj,$xi,$mj,$gj,$nj,$Ri,$oj,$Ji,$pj,$Xj,$Zj,$sj,$ck,$Jj,$dk,$Uj,$ek,$Bj,$fk,$Uh,$gk,$zg,$hk,$Kf,$ik,$jg,$jk,$wg,$kk,$Yf,$lk,$nl,$pl,$ok,$ql,$Sk,$rl,$kl,$sl,$Kk,$tl,$Xk,$ul,$tm,$vm,$zl,$wm,$em,$xm,$Tl,$ym,$pm,$zm,$Ll,$Am,$gn,$in,$Dm,$jn,$Mm,$kn,$dn};$mn=q'resolvers';$nn=[];$on=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$pn=bless({$c2,$nn,$y1,$on,$A1,$B1},$U);$qn=q'file';$rn=[];$sn=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$tn=bless({$c2,$rn,$y1,$sn,$A1,$B1},$U);$un=q'str';$vn=[];$wn=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$xn=bless({$c2,$vn,$y1,$wn,$A1,$B1},$U);$yn={$hh,$pn,$qn,$tn,$un,$xn};$zn=bless({$g9,$ln,$mn,$yn},$W);$An=q'/lib/ni::ctors';$$f4[0]=$n7;$$F[0]=$g4;$$s1[0]=$g4;$$N2[0]=$Y2;$$e6[4]=$W6;*$u3=\&$s3;*$t3=\&$q3;$J1->apply_unsafe($S);$J1->apply_unsafe($h);$J1->apply_unsafe($l);$J1->apply_unsafe($m);$J1->apply_unsafe($n);$J1->apply_unsafe($U);$J1->apply_unsafe($o);$J1->apply_unsafe($p);$J1->apply_unsafe($q);$J1->apply_unsafe($M);$J1->apply_unsafe($r);$J1->apply_unsafe($Q);$J1->apply_unsafe($s);$J1->apply_unsafe($G);$J1->apply_unsafe($d);$J1->apply_unsafe($j);$J1->apply_unsafe($t);$J1->apply_unsafe($u);$J1->apply_unsafe($v);$J1->apply_unsafe($w);$J1->apply_unsafe($x);$J1->apply_unsafe($y);$J1->apply_unsafe($z);$J1->apply_unsafe($K1);$J1->apply_unsafe($B);$X1->apply_unsafe($U);$p2->apply_unsafe($U);$C2->apply_unsafe($U);$L2->apply_unsafe($U);$U2->apply_unsafe($S);$U2->apply_unsafe($h);$U2->apply_unsafe($O);$U2->apply_unsafe($l);$U2->apply_unsafe($P);$U2->apply_unsafe($m);$U2->apply_unsafe($T);$U2->apply_unsafe($n);$U2->apply_unsafe($U);$U2->apply_unsafe($o);$U2->apply_unsafe($V);$U2->apply_unsafe($p);$U2->apply_unsafe($W);$U2->apply_unsafe($q);$U2->apply_unsafe($M);$U2->apply_unsafe($r);$U2->apply_unsafe($Q);$U2->apply_unsafe($s);$U2->apply_unsafe($G);$U2->apply_unsafe($d);$U2->apply_unsafe($X);$U2->apply_unsafe($j);$U2->apply_unsafe($Y);$U2->apply_unsafe($t);$U2->apply_unsafe($Z);$U2->apply_unsafe($u);$U2->apply_unsafe($c1);$U2->apply_unsafe($v);$U2->apply_unsafe($d1);$U2->apply_unsafe($w);$U2->apply_unsafe($e1);$U2->apply_unsafe($x);$U2->apply_unsafe($f1);$U2->apply_unsafe($y);$U2->apply_unsafe($g1);$U2->apply_unsafe($z);$U2->apply_unsafe($h1);$U2->apply_unsafe($V2);$U2->apply_unsafe($j1);$U2->apply_unsafe($B);$m3->apply_unsafe($S);$m3->apply_unsafe($h);$m3->apply_unsafe($l);$m3->apply_unsafe($P);$m3->apply_unsafe($m);$m3->apply_unsafe($T);$m3->apply_unsafe($n);$m3->apply_unsafe($o);$m3->apply_unsafe($p);$m3->apply_unsafe($q);$m3->apply_unsafe($M);$m3->apply_unsafe($r);$m3->apply_unsafe($Q);$m3->apply_unsafe($s);$m3->apply_unsafe($G);$m3->apply_unsafe($d);$m3->apply_unsafe($X);$m3->apply_unsafe($j);$m3->apply_unsafe($t);$m3->apply_unsafe($u);$m3->apply_unsafe($v);$m3->apply_unsafe($w);$m3->apply_unsafe($x);$m3->apply_unsafe($y);$m3->apply_unsafe($z);$m3->apply_unsafe($n3);$m3->apply_unsafe($B);$A3->apply_unsafe($M);$H3->apply_unsafe($M);$P3->apply_unsafe($M);$c4->apply_unsafe($h);$c4->apply_unsafe($l);$c4->apply_unsafe($m);$c4->apply_unsafe($n);$c4->apply_unsafe($o);$c4->apply_unsafe($p);$c4->apply_unsafe($q);$c4->apply_unsafe($r);$c4->apply_unsafe($s);$c4->apply_unsafe($j);$c4->apply_unsafe($t);$c4->apply_unsafe($u);$c4->apply_unsafe($v);$c4->apply_unsafe($w);$c4->apply_unsafe($x);$c4->apply_unsafe($y);$c4->apply_unsafe($z);$c4->apply_unsafe($d4);$c4->apply_unsafe($B);$C4->apply_unsafe($Q);$J4->apply_unsafe($Q);$X4->apply_unsafe($S);$X4->apply_unsafe($h);$X4->apply_unsafe($l);$X4->apply_unsafe($P);$X4->apply_unsafe($m);$X4->apply_unsafe($n);$X4->apply_unsafe($o);$X4->apply_unsafe($p);$X4->apply_unsafe($q);$X4->apply_unsafe($r);$X4->apply_unsafe($s);$X4->apply_unsafe($G);$X4->apply_unsafe($d);$X4->apply_unsafe($X);$X4->apply_unsafe($j);$X4->apply_unsafe($t);$X4->apply_unsafe($u);$X4->apply_unsafe($v);$X4->apply_unsafe($w);$X4->apply_unsafe($x);$X4->apply_unsafe($y);$X4->apply_unsafe($z);$X4->apply_unsafe($Y4);$X4->apply_unsafe($B);$i5->apply_unsafe($S);$i5->apply_unsafe($h);$i5->apply_unsafe($l);$i5->apply_unsafe($P);$i5->apply_unsafe($m);$i5->apply_unsafe($n);$i5->apply_unsafe($o);$i5->apply_unsafe($p);$i5->apply_unsafe($q);$i5->apply_unsafe($M);$i5->apply_unsafe($r);$i5->apply_unsafe($Q);$i5->apply_unsafe($s);$i5->apply_unsafe($G);$i5->apply_unsafe($d);$i5->apply_unsafe($X);$i5->apply_unsafe($j);$i5->apply_unsafe($t);$i5->apply_unsafe($u);$i5->apply_unsafe($v);$i5->apply_unsafe($w);$i5->apply_unsafe($x);$i5->apply_unsafe($y);$i5->apply_unsafe($z);$i5->apply_unsafe($j5);$i5->apply_unsafe($B);$r5->apply_unsafe($S);$r5->apply_unsafe($h);$r5->apply_unsafe($l);$r5->apply_unsafe($P);$r5->apply_unsafe($m);$r5->apply_unsafe($n);$r5->apply_unsafe($o);$r5->apply_unsafe($p);$r5->apply_unsafe($q);$r5->apply_unsafe($M);$r5->apply_unsafe($r);$r5->apply_unsafe($Q);$r5->apply_unsafe($s);$r5->apply_unsafe($G);$r5->apply_unsafe($d);$r5->apply_unsafe($X);$r5->apply_unsafe($j);$r5->apply_unsafe($t);$r5->apply_unsafe($u);$r5->apply_unsafe($v);$r5->apply_unsafe($w);$r5->apply_unsafe($x);$r5->apply_unsafe($y);$r5->apply_unsafe($z);$r5->apply_unsafe($s5);$r5->apply_unsafe($B);$A5->apply_unsafe($S);$A5->apply_unsafe($h);$A5->apply_unsafe($l);$A5->apply_unsafe($P);$A5->apply_unsafe($m);$A5->apply_unsafe($n);$A5->apply_unsafe($o);$A5->apply_unsafe($p);$A5->apply_unsafe($q);$A5->apply_unsafe($r);$A5->apply_unsafe($Q);$A5->apply_unsafe($s);$A5->apply_unsafe($G);$A5->apply_unsafe($d);$A5->apply_unsafe($X);$A5->apply_unsafe($j);$A5->apply_unsafe($t);$A5->apply_unsafe($u);$A5->apply_unsafe($v);$A5->apply_unsafe($w);$A5->apply_unsafe($x);$A5->apply_unsafe($y);$A5->apply_unsafe($z);$A5->apply_unsafe($B5);$A5->apply_unsafe($B);$N5->apply_unsafe($S);$N5->apply_unsafe($h);$N5->apply_unsafe($l);$N5->apply_unsafe($m);$N5->apply_unsafe($n);$N5->apply_unsafe($o);$N5->apply_unsafe($p);$N5->apply_unsafe($q);$N5->apply_unsafe($r);$N5->apply_unsafe($s);$N5->apply_unsafe($G);$N5->apply_unsafe($d);$N5->apply_unsafe($X);$N5->apply_unsafe($j);$N5->apply_unsafe($t);$N5->apply_unsafe($u);$N5->apply_unsafe($v);$N5->apply_unsafe($w);$N5->apply_unsafe($x);$N5->apply_unsafe($y);$N5->apply_unsafe($z);$N5->apply_unsafe($O5);$N5->apply_unsafe($B);$c6->apply_unsafe($P);$q6->apply_unsafe($S);$q6->apply_unsafe($h);$q6->apply_unsafe($l);$q6->apply_unsafe($P);$q6->apply_unsafe($m);$q6->apply_unsafe($n);$q6->apply_unsafe($o);$q6->apply_unsafe($p);$q6->apply_unsafe($q);$q6->apply_unsafe($r);$q6->apply_unsafe($s);$q6->apply_unsafe($G);$q6->apply_unsafe($d);$q6->apply_unsafe($X);$q6->apply_unsafe($j);$q6->apply_unsafe($t);$q6->apply_unsafe($u);$q6->apply_unsafe($v);$q6->apply_unsafe($w);$q6->apply_unsafe($x);$q6->apply_unsafe($y);$q6->apply_unsafe($z);$q6->apply_unsafe($r6);$q6->apply_unsafe($B);$C6->apply_unsafe($S);$C6->apply_unsafe($h);$C6->apply_unsafe($l);$C6->apply_unsafe($P);$C6->apply_unsafe($m);$C6->apply_unsafe($n);$C6->apply_unsafe($o);$C6->apply_unsafe($p);$C6->apply_unsafe($q);$C6->apply_unsafe($r);$C6->apply_unsafe($s);$C6->apply_unsafe($G);$C6->apply_unsafe($d);$C6->apply_unsafe($X);$C6->apply_unsafe($j);$C6->apply_unsafe($t);$C6->apply_unsafe($u);$C6->apply_unsafe($v);$C6->apply_unsafe($w);$C6->apply_unsafe($x);$C6->apply_unsafe($y);$C6->apply_unsafe($z);$C6->apply_unsafe($D6);$C6->apply_unsafe($B);$K6->apply_unsafe($S);$K6->apply_unsafe($h);$K6->apply_unsafe($l);$K6->apply_unsafe($P);$K6->apply_unsafe($m);$K6->apply_unsafe($n);$K6->apply_unsafe($o);$K6->apply_unsafe($p);$K6->apply_unsafe($q);$K6->apply_unsafe($r);$K6->apply_unsafe($s);$K6->apply_unsafe($G);$K6->apply_unsafe($d);$K6->apply_unsafe($X);$K6->apply_unsafe($j);$K6->apply_unsafe($t);$K6->apply_unsafe($u);$K6->apply_unsafe($v);$K6->apply_unsafe($w);$K6->apply_unsafe($x);$K6->apply_unsafe($y);$K6->apply_unsafe($z);$K6->apply_unsafe($L6);$K6->apply_unsafe($B);$S6->apply_unsafe($S);$S6->apply_unsafe($h);$S6->apply_unsafe($l);$S6->apply_unsafe($P);$S6->apply_unsafe($m);$S6->apply_unsafe($n);$S6->apply_unsafe($o);$S6->apply_unsafe($p);$S6->apply_unsafe($q);$S6->apply_unsafe($r);$S6->apply_unsafe($s);$S6->apply_unsafe($G);$S6->apply_unsafe($d);$S6->apply_unsafe($X);$S6->apply_unsafe($j);$S6->apply_unsafe($t);$S6->apply_unsafe($u);$S6->apply_unsafe($v);$S6->apply_unsafe($w);$S6->apply_unsafe($x);$S6->apply_unsafe($y);$S6->apply_unsafe($z);$S6->apply_unsafe($T6);$S6->apply_unsafe($B);$j7->apply_unsafe($S);$j7->apply_unsafe($h);$j7->apply_unsafe($l);$j7->apply_unsafe($m);$j7->apply_unsafe($n);$j7->apply_unsafe($o);$j7->apply_unsafe($p);$j7->apply_unsafe($q);$j7->apply_unsafe($r);$j7->apply_unsafe($s);$j7->apply_unsafe($d);$j7->apply_unsafe($j);$j7->apply_unsafe($t);$j7->apply_unsafe($u);$j7->apply_unsafe($v);$j7->apply_unsafe($w);$j7->apply_unsafe($x);$j7->apply_unsafe($y);$j7->apply_unsafe($z);$j7->apply_unsafe($k7);$j7->apply_unsafe($B);$M7->apply_unsafe($W);$Z7->apply_unsafe($W);$A8->apply_unsafe($W);$M8->apply_unsafe($W);$Z8->apply_unsafe($W);$s9->apply_unsafe($T);$z9->apply_unsafe($T);$H9->apply_unsafe($T);$S9->apply_unsafe($T);$Jb->apply_unsafe($Kb);$Jb->apply_unsafe($ub);$dc->apply_unsafe($V);$Ed->apply_unsafe($V);$gf->apply_unsafe($ce);$gf->apply_unsafe($de);$Yf->apply_unsafe($Z);$Yf->apply_unsafe($c1);$Yf->apply_unsafe($d1);$Yf->apply_unsafe($e1);$Yf->apply_unsafe($f1);$Yf->apply_unsafe($g1);$Yf->apply_unsafe($h1);$Yf->apply_unsafe($j1);$jg->apply_unsafe($Z);$jg->apply_unsafe($c1);$jg->apply_unsafe($d1);$jg->apply_unsafe($e1);$jg->apply_unsafe($f1);$jg->apply_unsafe($g1);$jg->apply_unsafe($h1);$jg->apply_unsafe($j1);$wg->apply_unsafe($Z);$wg->apply_unsafe($c1);$wg->apply_unsafe($d1);$wg->apply_unsafe($e1);$wg->apply_unsafe($f1);$wg->apply_unsafe($g1);$wg->apply_unsafe($h1);$wg->apply_unsafe($j1);$Hg->apply_unsafe($Z);$Qg->apply_unsafe($Z);$nh->apply_unsafe($c1);$vh->apply_unsafe($c1);$Eh->apply_unsafe($c1);$Rh->apply_unsafe($c1);$Rh->apply_unsafe($d1);$Rh->apply_unsafe($e1);$Rh->apply_unsafe($g1);$Rh->apply_unsafe($h1);$ji->apply_unsafe($c1);$Ji->apply_unsafe($d1);$Ri->apply_unsafe($d1);$gj->apply_unsafe($d1);$Bj->apply_unsafe($e1);$Jj->apply_unsafe($e1);$Uj->apply_unsafe($e1);$Kk->apply_unsafe($g1);$Sk->apply_unsafe($g1);$Xk->apply_unsafe($g1);$kl->apply_unsafe($g1);$Ll->apply_unsafe($h1);$Tl->apply_unsafe($h1);$em->apply_unsafe($h1);$pm->apply_unsafe($h1);$Mm->apply_unsafe($j1);$dn->apply_unsafe($j1);$ni::self=$zn;&$_($H)for@$I;&$_($K)for@$L;&$_($t1)for@$u1;&$_($C1)for@$D1;&$_($G1)for@$D1;&$_($J1)for@$L1;&$_($O1)for@$D1;&$_($R1)for@$D1;&$_($U1)for@$D1;&$_($X1)for@$Y1;&$_($f2)for@$D1;&$_($i2)for@$D1;&$_($m2)for@$D1;&$_($p2)for@$q2;&$_($v2)for@$D1;&$_($z2)for@$D1;&$_($C2)for@$D2;&$_($I2)for@$D1;&$_($L2)for@$M2;&$_($O2)for@$P2;&$_($R2)for@$D1;&$_($U2)for@$W2;&$_($Y2)for@$Z2;&$_($d3)for@$e3;&$_($h3)for@$D1;&$_($j3)for@$D1;&$_($m3)for@$o3;&$_($q3)for@$D1;&$_($s3)for@$D1;&$_($A3)for@$B3;&$_($E3)for@$D1;&$_($H3)for@$I3;&$_($M3)for@$D1;&$_($P3)for@$Q3;&$_($S3)for@$T3;&$_($X3)for@$D1;&$_($c4)for@$e4;&$_($g4)for@$h4;&$_($j4)for@$k4;&$_($m4)for@$n4;&$_($u4)for@$v4;&$_($z4)for@$D1;&$_($C4)for@$D4;&$_($G4)for@$D1;&$_($J4)for@$K4;&$_($M4)for@$N4;&$_($S4)for@$D1;&$_($U4)for@$D1;&$_($X4)for@$Z4;&$_($f5)for@$D1;&$_($i5)for@$k5;&$_($o5)for@$D1;&$_($r5)for@$t5;&$_($x5)for@$D1;&$_($A5)for@$C5;&$_($E5)for@$F5;&$_($I5)for@$D1;&$_($K5)for@$D1;&$_($N5)for@$P5;&$_($S5)for@$T5;&$_($X5)for@$D1;&$_($c6)for@$d6;&$_($f6)for@$g6;&$_($n6)for@$D1;&$_($q6)for@$s6;&$_($w6)for@$D1;&$_($z6)for@$D1;&$_($C6)for@$E6;&$_($H6)for@$D1;&$_($K6)for@$M6;&$_($P6)for@$D1;&$_($S6)for@$U6;&$_($W6)for@$X6;&$_($Z6)for@$c7;&$_($g7)for@$D1;&$_($j7)for@$l7;&$_($n7)for@$o7;&$_($q7)for@$r7;&$_($u7)for@$v7;&$_($y7)for@$z7;&$_($F7)for@$D1;&$_($J7)for@$D1;&$_($M7)for@$N7;&$_($S7)for@$D1;&$_($W7)for@$D1;&$_($Z7)for@$c8;&$_($h8)for@$D1;&$_($l8)for@$D1;&$_($p8)for@$D1;&$_($t8)for@$D1;&$_($x8)for@$D1;&$_($A8)for@$B8;&$_($F8)for@$D1;&$_($J8)for@$D1;&$_($M8)for@$N8;&$_($S8)for@$D1;&$_($W8)for@$D1;&$_($Z8)for@$c9;&$_($e9)for@$f9;&$_($k9)for@$l9;&$_($p9)for@$D1;&$_($s9)for@$t9;&$_($w9)for@$D1;&$_($z9)for@$A9;&$_($E9)for@$D1;&$_($H9)for@$I9;&$_($M9)for@$D1;&$_($P9)for@$D1;&$_($S9)for@$T9;&$_($V9)for@$W9;&$_($na)for@$oa;&$_($xa)for@$D1;&$_($Aa)for@$D1;&$_($Da)for@$oa;&$_($Na)for@$D1;&$_($Qa)for@$oa;&$_($Bb)for@$D1;&$_($Gb)for@$D1;&$_($Jb)for@$Lb;&$_($Nb)for@$Ob;&$_($Sb)for@$Tb;&$_($Yb)for@$D1;&$_($dc)for@$ec;&$_($jc)for@$D1;&$_($nc)for@$D1;&$_($rc)for@$D1;&$_($vc)for@$D1;&$_($zc)for@$D1;&$_($Dc)for@$D1;&$_($Hc)for@$D1;&$_($Lc)for@$D1;&$_($Pc)for@$D1;&$_($Tc)for@$D1;&$_($Xc)for@$D1;&$_($dd)for@$D1;&$_($hd)for@$D1;&$_($ld)for@$D1;&$_($pd)for@$D1;&$_($td)for@$D1;&$_($xd)for@$D1;&$_($Bd)for@$D1;&$_($Ed)for@$Fd;&$_($Hd)for@$Id;&$_($je)for@$D1;&$_($ne)for@$D1;&$_($re)for@$D1;&$_($ve)for@$D1;&$_($ze)for@$D1;&$_($De)for@$D1;&$_($He)for@$D1;&$_($Le)for@$D1;&$_($Pe)for@$D1;&$_($Te)for@$D1;&$_($Xe)for@$D1;&$_($df)for@$D1;&$_($gf)for@$hf;&$_($jf)for@$Ob;&$_($Kf)for@$Lf;&$_($Nf)for@$Of;&$_($Vf)for@$D1;&$_($Yf)for@$Zf;&$_($gg)for@$D1;&$_($jg)for@$kg;&$_($pg)for@$D1;&$_($tg)for@$D1;&$_($wg)for@$xg;&$_($zg)for@$Ag;&$_($Eg)for@$D1;&$_($Hg)for@$Ig;&$_($Ng)for@$D1;&$_($Qg)for@$Rg;&$_($Tg)for@$Ug;&$_($dh)for@$eh;&$_($kh)for@$D1;&$_($nh)for@$oh;&$_($sh)for@$D1;&$_($vh)for@$wh;&$_($Bh)for@$D1;&$_($Eh)for@$Fh;&$_($Lh)for@$D1;&$_($Oh)for@$D1;&$_($Rh)for@$Sh;&$_($Uh)for@$Vh;&$_($ci)for@$D1;&$_($gi)for@$D1;&$_($ji)for@$ki;&$_($mi)for@$ni;&$_($xi)for@$yi;&$_($Di)for@$D1;&$_($Gi)for@$D1;&$_($Ji)for@$Ki;&$_($Oi)for@$D1;&$_($Ri)for@$Si;&$_($Xi)for@$D1;&$_($dj)for@$D1;&$_($gj)for@$hj;&$_($jj)for@$kj;&$_($sj)for@$tj;&$_($yj)for@$D1;&$_($Bj)for@$Cj;&$_($Gj)for@$D1;&$_($Jj)for@$Kj;&$_($Oj)for@$D1;&$_($Rj)for@$D1;&$_($Uj)for@$Vj;&$_($Xj)for@$Yj;&$_($ok)for@$pk;&$_($vk)for@$D1;&$_($zk)for@$D1;&$_($Dk)for@$D1;&$_($Hk)for@$D1;&$_($Kk)for@$Lk;&$_($Pk)for@$D1;&$_($Sk)for@$Tk;&$_($Xk)for@$Yk;&$_($el)for@$D1;&$_($hl)for@$D1;&$_($kl)for@$ll;&$_($nl)for@$ol;&$_($zl)for@$Al;&$_($Fl)for@$D1;&$_($Il)for@$D1;&$_($Ll)for@$Ml;&$_($Ql)for@$D1;&$_($Tl)for@$Ul;&$_($Zl)for@$D1;&$_($em)for@$fm;&$_($jm)for@$D1;&$_($mm)for@$D1;&$_($pm)for@$qm;&$_($tm)for@$um;&$_($Dm)for@$Em;&$_($Jm)for@$D1;&$_($Mm)for@$Nm;&$_($Rm)for@$D1;&$_($Vm)for@$D1;&$_($Ym)for@$D1;&$_($dn)for@$en;&$_($gn)for@$hn;&$_($pn)for@$D1;&$_($tn)for@$D1;&$_($xn)for@$D1;&$_($zn)for@$An;ni->run(@ARGV);
__DATA__
