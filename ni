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
*{'/lib/ni::resolve'} = sub {shift->{named}{$_[0]} || die "ni: failed to resolve $_[0]"};
*{'/lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'/lib/fn::new'} = sub {
  my $self = bless {code => $_[1]}, $_[0];
  $self->compile;
  $self;
};
*{'/lib/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/lib/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
*{'/lib/fn::(('}    = sub {};
*{'/lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'/lib/fn::(bool'} = sub {1};
sub fn($) {'/lib/fn'->new(shift)}
_
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fd.c';$u=q'/unix/fifo.c';$v=q'/unix/file.c';$w=q'/unix/io.c';$x=q'/unix/pid.c';$y=q'/unix/pipeline.c';$z=q'/unix/str.c';$A={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1,$z,1};$B={$p,1};$C={$j,1,$k,1,$p,1,$q,1};$D=[undef];$E=q'/metaclass';$F=bless({$c,$C,$f,$j,$g,$D},$E);$G=q'/metaclass::ctors';$H=[$F];$I=bless({$c,$B,$f,$p,$g,$H},$E);$J=q'/metaclass::ctors';$K=q'/lib/slice';$L={$K,1};$M=q'/lib/behavior';$N=q'/lib/branch';$O=q'/lib/tag';$P={$M,1,$N,1,$K,1,$O,1};$Q=q'/class';$R=q'/lib/doc';$S=q'/lib/fn';$T=q'/lib/image';$U=q'/lib/ni';$V=q'/object';$W=q'/unix/cat';$X=q'/unix/fd';$Y=q'/unix/fifo';$Z=q'/unix/file';$c1=q'/unix/io';$d1=q'/unix/pid';$e1=q'/unix/pipeline';$f1=q'/unix/pipeline.c';$g1=q'/unix/str';$h1={$Q,1,$h,1,$M,1,$j,1,$N,1,$k,1,$R,1,$l,1,$S,1,$m,1,$T,1,$n,1,$U,1,$o,1,$K,1,$p,1,$O,1,$q,1,$E,1,$d,1,$V,1,$r,1,$W,1,$s,1,$X,1,$t,1,$Y,1,$u,1,$Z,1,$v,1,$c1,1,$w,1,$d1,1,$x,1,$e1,1,$f1,1,$g1,1,$z,1};$i1={};$j1=q'ctor';$k1=undef;$l1=q'dtor';$m1=q'methods';$n1=q'class';$o1={$m,1};$p1=[undef];$q1=bless({$c,$o1,$f,$m,$g,$p1},$E);$r1=q'/metaclass::ctors';$s1={$S,1};$t1={};$u1=q'DESTROY';$v1=q'code';$w1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$x1=bless({$v1,$w1},$S);$y1=q'/lib/fn::ctors';$z1=q'new';$A1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$B1=bless({$v1,$A1},$S);$C1={$u1,$x1,$z1,$B1};$D1=q'/lib/instantiable.b';$E1=bless({$c,$t1,$m1,$C1,$f,$D1},$K);$F1=q'/unix/pipeline.c';$G1=q'/lib/slice::ctors';$H1={};$I1=q'shift->compile';$J1=bless({$v1,$I1},$S);$K1=q'compile';$L1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/lib/fn: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$M1=bless({$v1,$L1},$S);$N1=q'instantiate';$O1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$P1=bless({$v1,$O1},$S);$Q1={$K1,$M1,$N1,$P1};$R1=q'/lib/fn_init.b';$S1=bless({$c,$H1,$j1,$J1,$l1,$k1,$m1,$Q1,$f,$R1},$K);$T1=q'/lib/slice::ctors';$U1={};$V1=q'annotations';$W1=[];$X1=q'shift->{\'annotations\'}';$Y1=bless({$V1,$W1,$v1,$X1},$S);$Z1=[];$c2=q'shift->{\'code\'}';$d2=bless({$V1,$Z1,$v1,$c2},$S);$e2=q'fn';$f2=[];$g2=q'shift->{\'fn\'}';$h2=bless({$V1,$f2,$v1,$g2},$S);$i2={$V1,$Y1,$v1,$d2,$e2,$h2};$j2=q'/lib/fn_ro.b';$k2=bless({$c,$U1,$j1,$k1,$l1,$k1,$m1,$i2,$f,$j2},$K);$l2=q'/lib/slice::ctors';$m2={};$n2=q'(""';$o2=[];$p2=q'shift->{code}';$q2=bless({$V1,$o2,$v1,$p2},$S);$r2=q'(eq';$s2=[];$t2=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])';$u2=bless({$V1,$s2,$v1,$t2},$S);$v2={$n2,$q2,$r2,$u2};$w2=q'/lib/fn_ops.b';$x2=bless({$c,$m2,$j1,$k1,$l1,$k1,$m1,$v2,$f,$w2},$K);$y2=q'/lib/slice::ctors';$z2={};$A2=q'serialize';$B2=[];$C2=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$D2=bless({$V1,$B2,$v1,$C2},$S);$E2={$A2,$D2};$F2=q'/lib/fn_serialize.b';$G2=bless({$c,$z2,$j1,$k1,$l1,$k1,$m1,$E2,$f,$F2},$K);$H2=q'/lib/slice::ctors';$I2=[undef,$E1,$S1,$k2,$x2,$G2];$J2=bless({$c,$s1,$f,$S,$g,$I2},$m);$K2=q'/lib/fn.c::ctors';$L2=q'ni \'ni:\' . ref shift';$M2=bless({$v1,$L2},$S);$N2={$n1,$M2};$O2=q'/lib/instance.b';$P2=bless({$c,$i1,$j1,$k1,$l1,$k1,$m1,$N2,$f,$O2},$K);$Q2=q'/unix/pipeline.c';$R2=q'/lib/slice::ctors';$S2=[$P2];$T2=bless({$c,$h1,$f,$V,$g,$S2},$r);$U2=q'/object.c::ctors';$V2=[$T2];$W2=bless({$c,$P,$f,$M,$g,$V2},$j);$X2=q'/lib/behavior.c::ctors';$Y2={};$Z2=q'my $s = shift; ni->def($s->name, $s)';$c3=bless({$v1,$Z2},$S);$d3=q'$_[0]->namespace . ":" . $_[0]->{name}';$e3=bless({$v1,$d3},$S);$f3={$f,$e3};$g3=q'/lib/named.b';$h3=bless({$c,$Y2,$j1,$c3,$l1,$k1,$m1,$f3,$f,$g3},$K);$i3=q'/unix/pipeline.c';$j3=q'/lib/slice::ctors';$k3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$l3=bless({$v1,$k3},$S);$m3=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$n3=bless({$v1,$m3},$S);$o3=q'/lib/slice::apply';$p3=q'/lib/slice::apply_unsafe';$q3={};$r3=q'apply';$s3=q'apply_unsafe';$t3={$r3,$l3,$s3,$n3};$u3=q'/lib/slice.b';$v3=bless({$c,$q3,$m1,$t3,$f,$u3},$K);$w3=q'/lib/slice::ctors';$x3={};$y3=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$z3=bless({$v1,$y3},$S);$A3={$N1,$z3};$B3=q'/lib/slice_init.b';$C3=bless({$c,$x3,$m1,$A3,$f,$B3},$K);$D3=q'/lib/slice::ctors';$E3={};$F3=[];$G3=q'local $_;
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
$g;';$H3=bless({$V1,$F3,$v1,$G3},$S);$I3={$A2,$H3};$J3=q'/lib/slice_serialize.b';$K3=bless({$c,$E3,$j1,$k1,$l1,$k1,$m1,$I3,$f,$J3},$K);$L3=q'/lib/slice::ctors';$M3=[$W2,$h3,$v3,$C3,$K3];$N3=bless({$c,$L,$f,$K,$g,$M3},$p);$O3=q'/lib/slice.c::ctors';$P3={};$Q3=q'doc';$R3=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:/lib/doc\')->new($name);';$S3=bless({$v1,$R3},$S);$T3={$Q3,$S3};$U3=q'/lib/documentable.b';$V3=bless({$c,$P3,$j1,$k1,$l1,$k1,$m1,$T3,$f,$U3},$K);$W3=q'/unix/pipeline.c';$X3=q'/lib/slice::ctors';$Y3=[undef,$V3];$Z3=bless({$c,$A,$f,$r,$g,$Y3},$E);$c4=q'/metaclass::ctors';$d4=[$Z3];$e4=bless({$c,$i,$f,$h,$g,$d4},$E);$f4=q'/metaclass::ctors';$g4=q'/unix/pipeline.c';$h4={$Q,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$g4,1,$z,1};$i4={$q,1};$j4=[$F];$k4=bless({$c,$i4,$f,$q,$g,$j4},$E);$l4=q'/metaclass::ctors';$m4={$O,1};$n4={};$o4=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$p4=bless({$v1,$o4},$S);$q4={$r3,$p4};$r4=q'/lib/tag.b';$s4=bless({$c,$n4,$j1,$k1,$l1,$k1,$m1,$q4,$f,$r4},$K);$t4=q'/lib/slice::ctors';$u4={};$v4=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$w4=bless({$v1,$v4},$S);$x4={$N1,$w4};$y4=q'/lib/tag_init.b';$z4=bless({$c,$u4,$j1,$k1,$l1,$k1,$m1,$x4,$f,$y4},$K);$A4=q'/lib/slice::ctors';$B4=[$W2,$h3,$s4,$z4];$C4=bless({$c,$m4,$f,$O,$g,$B4},$q);$D4=q'/lib/tag.c::ctors';$E4=q'/lib/perlbranch.b';$F4={};$G4=q'add';$H4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$I4=bless({$v1,$H4},$S);$J4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$K4=bless({$v1,$J4},$S);$L4={$G4,$I4,$r3,$K4};$M4=q'/lib/branch.b';$N4=bless({$c,$F4,$j1,$k1,$l1,$k1,$m1,$L4,$f,$M4},$K);$O4=q'/unix/pipeline.c';$P4=q'/lib/slice::ctors';$Q4={};$R4=q'namespace';$S4=q'\'ni\'';$T4=bless({$v1,$S4},$S);$U4={$R4,$T4};$V4=q'/lib/named_in_ni.b';$W4=bless({$c,$Q4,$j1,$k1,$l1,$k1,$m1,$U4,$f,$V4},$K);$X4=q'/unix/pipeline.c';$Y4=q'/lib/slice::ctors';$Z4={};$c5=q'package';$d5=q'shift->{name}';$e5=bless({$v1,$d5},$S);$f5={$c5,$e5};$g5=q'/lib/namespaced.b';$h5=bless({$c,$Z4,$j1,$k1,$l1,$k1,$m1,$f5,$f,$g5},$K);$i5=q'/unix/pipeline.c';$j5=q'/lib/slice::ctors';$k5={};$l5=q'resolve';$m5=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$n5=bless({$v1,$m5},$S);$o5={$l5,$n5};$p5=q'/lib/resolver.b';$q5=bless({$c,$k5,$j1,$k1,$l1,$k1,$m1,$o5,$f,$p5},$K);$r5=q'/unix/pipeline.c';$s5=q'/lib/slice::ctors';$t5=[$N4,$E1,$h3,$W4,$h5,$q5];$u5=bless({$f,$E4,$g,$t5},$O);$v5=q'/lib/tag::ctors';$w5={};$x5=q'my $s = shift; $s->apply($s->package)';$y5=bless({$v1,$x5},$S);$z5=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$A5=bless({$v1,$z5},$S);$B5={$N1,$A5};$C5=q'/lib/class_init.b';$D5=bless({$c,$w5,$j1,$y5,$l1,$k1,$m1,$B5,$f,$C5},$K);$E5=q'/unix/pipeline.c';$F5=q'/lib/slice::ctors';$G5={$k,1};$H5=[$F];$I5=bless({$c,$G5,$f,$k,$g,$H5},$E);$J5=q'/metaclass::ctors';$K5={$N,1};$L5={};$M5=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$N5=bless({$v1,$M5},$S);$O5={$N1,$N5};$P5=q'/lib/branch_init.b';$Q5=bless({$c,$L5,$j1,$k1,$l1,$k1,$m1,$O5,$f,$P5},$K);$R5=q'/lib/slice::ctors';$S5=[$W2,$h3,$N4,$Q5,undef];$T5=bless({$c,$K5,$f,$N,$g,$S5},$k);$U5=q'/lib/branch.c::ctors';$V5=q'/unix/pipeline.c';$W5={$Q,1,$h,1,$j,1,$N,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$E,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$V5,1,$z,1};$X5=q'/lib/definition.b';$Y5={};$Z5=q'def';$c6=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$d6=bless({$v1,$c6},$S);$e6={$Z5,$d6};$f6=q'/lib/classdef.b';$g6=bless({$c,$Y5,$j1,$k1,$l1,$k1,$m1,$e6,$f,$f6},$K);$h6=q'/unix/pipeline.c';$i6=q'/lib/slice::ctors';$j6={};$k6=q'ro';$l6=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$m6=bless({$v1,$l6},$S);$n6=q'rw';$o6=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$p6=bless({$v1,$o6},$S);$q6={$k6,$m6,$n6,$p6};$r6=q'/lib/accessor.b';$s6=bless({$c,$j6,$j1,$k1,$l1,$k1,$m1,$q6,$f,$r6},$K);$t6=q'/unix/pipeline.c';$u6=q'/lib/slice::ctors';$v6={};$w6=q'shift->name';$x6=bless({$v1,$w6},$S);$y6={$n2,$x6};$z6=q'/lib/name_as_string.b';$A6=bless({$c,$v6,$j1,$k1,$l1,$k1,$m1,$y6,$f,$z6},$K);$B6=q'/unix/pipeline.c';$C6=q'/lib/slice::ctors';$D6={};$E6=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);';$F6=bless({$v1,$E6},$S);$G6={$r2,$F6};$H6=q'/lib/ref_eq.b';$I6=bless({$c,$D6,$j1,$k1,$l1,$k1,$m1,$G6,$f,$H6},$K);$J6=q'/unix/pipeline.c';$K6=q'/lib/slice::ctors';$L6=[$g6,$s6,$A6,$I6];$M6=bless({$c,$W5,$f,$X5,$g,$L6},$N);$N6=q'/lib/branch::ctors';$O6={};$P6=q'child';$Q6=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$R6=bless({$v1,$Q6},$S);$S6={$P6,$R6};$T6=q'/lib/subclass.b';$U6=bless({$c,$O6,$j1,$k1,$l1,$k1,$m1,$S6,$f,$T6},$K);$V6=q'/unix/pipeline.c';$W6=q'/lib/slice::ctors';$X6=[$u5,$D5,$T2,$M6,$U6];$Y6=bless({$c,$h4,$f,$Q,$g,$X6},$h);$Z6=q'/class.c::ctors';$c7=[$Y6];$d7=bless({$c,$e,$f,$d,$g,$c7},$E);$e7=q'/metaclass::ctors';$f7={$E,1};$g7=[$u5,$D5,$T2,$M6];$h7=bless({$c,$f7,$f,$E,$g,$g7},$d);$i7=q'/metaclass.c::ctors';$j7={$o,1};$k7=[$Z3];$l7=bless({$c,$j7,$f,$o,$g,$k7},$E);$m7=q'/metaclass::ctors';$n7={$U,1};$o7={};$p7=q'is_mutable';$q7=[];$r7=q'$0 ne "-" && -w $0';$s7=bless({$V1,$q7,$v1,$r7},$S);$t7=q'modify';$u7=[];$v7=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$w7=bless({$V1,$u7,$v1,$v7},$S);$x7={$p7,$s7,$t7,$w7};$y7=q'/lib/ni_self.b';$z7=bless({$c,$o7,$j1,$k1,$l1,$k1,$m1,$x7,$f,$y7},$K);$A7=q'/lib/slice::ctors';$B7={};$C7=q'exists';$D7=[];$E7=q'exists $_[0]->{named}{$_[1]}';$F7=bless({$V1,$D7,$v1,$E7},$S);$G7=q'quoted';$H7=[];$I7=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$J7=bless({$V1,$H7,$v1,$I7},$S);$K7={$C7,$F7,$G7,$J7};$L7=q'/lib/ni_image.b';$M7=bless({$c,$B7,$j1,$k1,$l1,$k1,$m1,$K7,$f,$L7},$K);$N7=q'/lib/slice::ctors';$O7={};$P7=q'--internal/+=';$Q7=[];$R7=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$S7=bless({$V1,$Q7,$v1,$R7},$S);$T7=q'--internal/eval';$U7=[];$V7=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$W7=bless({$V1,$U7,$v1,$V7},$S);$X7=q'--internal/image';$Y7=[];$Z7=q'shift->quoted->write(\\*STDOUT);
0;';$c8=bless({$V1,$Y7,$v1,$Z7},$S);$d8=q'--internal/test';$e8=[];$f8=q'my $self = shift;
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
!!$fails;';$g8=bless({$V1,$e8,$v1,$f8},$S);$h8=q'run';$i8=[];$j8=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$k8=bless({$V1,$i8,$v1,$j8},$S);$l8={$P7,$S7,$T7,$W7,$X7,$c8,$d8,$g8,$h8,$k8};$m8=q'/lib/ni_main.b';$n8=bless({$c,$O7,$j1,$k1,$l1,$k1,$m1,$l8,$f,$m8},$K);$o8=q'/lib/slice::ctors';$p8={};$q8=[];$r8=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$s8=bless({$V1,$q8,$v1,$r8},$S);$t8=q'resolver_for';$u8=[];$v8=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$w8=bless({$V1,$u8,$v1,$v8},$S);$x8={$l5,$s8,$t8,$w8};$y8=q'/lib/ni_resolver.b';$z8=bless({$c,$p8,$j1,$k1,$l1,$k1,$m1,$x8,$f,$y8},$K);$A8=q'/lib/slice::ctors';$B8={};$C8=q'fork';$D8=[];$E8=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$F8=bless({$V1,$D8,$v1,$E8},$S);$G8=q'fork_exec';$H8=[];$I8=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$J8=bless({$V1,$H8,$v1,$I8},$S);$K8={$C8,$F8,$G8,$J8};$L8=q'/lib/ni_pid_ctors';$M8=bless({$c,$B8,$j1,$k1,$l1,$k1,$m1,$K8,$f,$L8},$K);$N8=q'/lib/slice::ctors';$O8=[$T2,$z7,$M7,$n8,$z8,$M8];$P8=bless({$c,$n7,$f,$U,$g,$O8},$o);$Q8=q'/lib/ni.c::ctors';$R8=q'named';$S8=q'ni.doc:/class';$T8={$l,1};$U8=[$Z3];$V8=bless({$c,$T8,$f,$l,$g,$U8},$E);$W8=q'/metaclass::ctors';$X8={$R,1};$Y8={};$Z8=q'shift; +{name => shift, doc => []}';$c9=bless({$v1,$Z8},$S);$d9={$N1,$c9};$e9=q'/lib/doc_init.b';$f9=bless({$c,$Y8,$j1,$k1,$l1,$k1,$m1,$d9,$f,$e9},$K);$g9=q'/lib/slice::ctors';$h9={};$i9=q'\'ni.doc\'';$j9=bless({$v1,$i9},$S);$k9={$R4,$j9};$l9=q'/lib/doc_namespace.b';$m9=bless({$c,$h9,$j1,$k1,$l1,$k1,$m1,$k9,$f,$l9},$K);$n9=q'/lib/slice::ctors';$o9={};$p9=q'AUTOLOAD';$q9=q'my $self = shift;
my $method = ${__PACKAGE__ . "::AUTOLOAD"};
push @{$$self{doc}}, [$method, @_];
$self;';$r9=bless({$v1,$q9},$S);$s9={$p9,$r9};$t9=q'/lib/doc_define.b';$u9=bless({$c,$o9,$j1,$k1,$l1,$k1,$m1,$s9,$f,$t9},$K);$v9=q'/lib/slice::ctors';$w9={};$x9=q'eg';$y9=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$z9=bless({$v1,$y9},$S);$A9=q'tests';$B9=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$C9=bless({$v1,$B9},$S);$D9={$x9,$z9,$A9,$C9};$E9=q'/lib/doc_test.b';$F9=bless({$c,$w9,$j1,$k1,$l1,$k1,$m1,$D9,$f,$E9},$K);$G9=q'/lib/slice::ctors';$H9=[$T2,$h3,$f9,$m9,$u9,$F9];$I9=bless({$c,$X8,$f,$R,$g,$H9},$l);$J9=q'/lib/doc.c::ctors';$K9=q'/lib/doc::synopsis';$L9=q'
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
  ';$M9=[$K9,$L9];$N9=q'/lib/doc::description';$O9=q'ni:/class is at the core of ni\'s object-oriented system, along with core
      classes like ni:/object and ni:/metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$P9=[$N9,$O9];$Q9=q'/lib/doc::behaviors';$R9=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:/lib/slice, which represents a set of methods you can add to a
      package. TODO...';$S9=[$Q9,$R9];$T9=q'/lib/doc::classes';$U9=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$V9=q'TODO...';$W9=[$T9,$U9,$V9];$X9=[$M9,$P9,$S9,$W9];$Y9=bless({$Q3,$X9,$f,$Q},$R);$Z9=q'/lib/doc::ctors';$ca=q'ni.doc:/lib/doc';$da=q'/lib/doc::synopsis';$ea=q'
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$fa=[$da,$ea];$ga=q'/lib/doc::description';$ha=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$ia=q'Documentation objects are internally represented as arrays of quoted
      method calls; for example:';$ja=q'my $doc = ni("ni:/lib/doc")->new("foo");
# TODO: make this into a test, but first we need a testing DSL
$doc->foo("bar bif baz");
# state is now [["foo", ["bar bif baz"]]]';$ka=bless({$v1,$ja},$S);$la=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions.';$ma=q'Documentation also stores unit tests, which are specified using "eg";
      e.g.:';$na=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$oa=bless({$v1,$na},$S);$pa=[$ga,$ha,$ia,$x9,$ka,$la,$ma,$x9,$oa];$qa=[$fa,$pa];$ra=bless({$Q3,$qa,$f,$R},$R);$sa=q'ni:/class';$ta=q'ni:/class.c';$ua=q'ni:/lib/accessor.b';$va=q'ni:/lib/behavior';$wa=q'ni:/lib/behavior.c';$xa=q'ni:/lib/branch';$ya=q'ni:/lib/branch.b';$za=q'ni:/lib/branch.c';$Aa=q'ni:/lib/branch_init.b';$Ba=q'ni:/lib/class_init.b';$Ca=q'ni:/lib/classdef.b';$Da=q'ni:/lib/definition.b';$Ea=q'ni:/lib/doc';$Fa=q'ni:/lib/doc.c';$Ga=q'ni:/lib/doc_define.b';$Ha=q'ni:/lib/doc_init.b';$Ia=q'ni:/lib/doc_namespace.b';$Ja=q'ni:/lib/doc_test.b';$Ka=q'ni:/lib/documentable.b';$La=q'ni:/lib/fn';$Ma=q'ni:/lib/fn.c';$Na=q'ni:/lib/fn_init.b';$Oa=q'ni:/lib/fn_ops.b';$Pa=q'ni:/lib/fn_ro.b';$Qa=q'ni:/lib/fn_serialize.b';$Ra=q'ni:/lib/image';$Sa={$n,1};$Ta=[$Z3];$Ua=bless({$c,$Sa,$f,$n,$g,$Ta},$E);$Va=q'/metaclass::ctors';$Wa={$T,1};$Xa={};$Ya=[];$Za=q'my $class = shift;
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
  ordering     => []};';$cb=bless({$V1,$Ya,$v1,$Za},$S);$db={$N1,$cb};$eb=q'/lib/image_init.b';$fb=bless({$c,$Xa,$j1,$k1,$l1,$k1,$m1,$db,$f,$eb},$K);$gb=q'/lib/slice::ctors';$hb={};$ib=q'address';$jb=[];$kb=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$lb=bless({$V1,$jb,$v1,$kb},$S);$mb=q'allocate_gensym';$nb=[];$ob=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$pb=bless({$V1,$nb,$v1,$ob},$S);$qb=q'boot_side_effect';$rb=[];$sb=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$tb=bless({$V1,$rb,$v1,$sb},$S);$ub=q'circular_links';$vb=[];$wb=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$xb=bless({$V1,$vb,$v1,$wb},$S);$yb=q'finalizer';$zb=[];$Ab=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Bb=bless({$V1,$zb,$v1,$Ab},$S);$Cb=q'gensym';$Db=[];$Eb=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$Fb=bless({$V1,$Db,$v1,$Eb},$S);$Gb=q'is_circular';$Hb=[];$Ib=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$Jb=bless({$V1,$Hb,$v1,$Ib},$S);$Kb=q'quote';$Lb=[];$Mb=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$Nb=bless({$V1,$Lb,$v1,$Mb},$S);$Ob=q'quote_array';$Pb=[];$Qb=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$Rb=bless({$V1,$Pb,$v1,$Qb},$S);$Sb=q'quote_blessed';$Tb=[];$Ub=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$Vb=bless({$V1,$Tb,$v1,$Ub},$S);$Wb=q'quote_class';$Xb=[];$Yb=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$Zb=bless({$V1,$Xb,$v1,$Yb},$S);$cc=q'quote_hash';$dc=[];$ec=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$fc=bless({$V1,$dc,$v1,$ec},$S);$gc=q'quote_object';$hc=[];$ic=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$jc=bless({$V1,$hc,$v1,$ic},$S);$kc=q'quote_scalar';$lc=[];$mc=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$nc=bless({$V1,$lc,$v1,$mc},$S);$oc=q'quote_value';$pc=[];$qc=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$rc=bless({$V1,$pc,$v1,$qc},$S);$sc=q'reconstruction';$tc=[];$uc=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$vc=bless({$V1,$tc,$v1,$uc},$S);$wc=q'side_effect';$xc=[];$yc=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$zc=bless({$V1,$xc,$v1,$yc},$S);$Ac=q'write';$Bc=[];$Cc=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Dc=bless({$V1,$Bc,$v1,$Cc},$S);$Ec={$ib,$lb,$mb,$pb,$qb,$tb,$ub,$xb,$yb,$Bb,$Cb,$Fb,$Gb,$Jb,$Kb,$Nb,$Ob,$Rb,$Sb,$Vb,$Wb,$Zb,$cc,$fc,$gc,$jc,$kc,$nc,$oc,$rc,$sc,$vc,$wc,$zc,$Ac,$Dc};$Fc=q'/lib/image_quoting.b';$Gc=bless({$c,$hb,$j1,$k1,$l1,$k1,$m1,$Ec,$f,$Fc},$K);$Hc=q'/lib/slice::ctors';$Ic=[$T2,$fb,$Gc];$Jc=bless({$c,$Wa,$f,$T,$g,$Ic},$n);$Kc=q'/lib/image.c::ctors';$Lc=q'ni:/lib/image.c';$Mc=q'ni:/lib/image_init.b';$Nc=q'ni:/lib/image_quoting.b';$Oc=q'ni:/lib/instance.b';$Pc=q'ni:/lib/instantiable.b';$Qc=q'ni:/lib/name_as_string.b';$Rc=q'ni:/lib/named.b';$Sc=q'ni:/lib/named_in_ni.b';$Tc=q'ni:/lib/namespaced.b';$Uc=q'ni:/lib/ni';$Vc=q'ni:/lib/ni.c';$Wc=q'ni:/lib/ni_image.b';$Xc=q'ni:/lib/ni_main.b';$Yc=q'ni:/lib/ni_pid_ctors';$Zc=q'ni:/lib/ni_resolver.b';$cd=q'ni:/lib/ni_self.b';$dd=q'ni:/lib/ni_static';$ed=q'/lib/ni_static';$fd=q'ni';$gd={$ed,1,$fd,1};$hd={};$id=q'abbrev';$jd=[];$kd=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$ld=bless({$V1,$jd,$v1,$kd},$S);$md=q'dor';$nd=[];$od=q'defined $_[0] ? $_[0] : $_[1]';$pd=bless({$V1,$nd,$v1,$od},$S);$qd=q'indent';$rd=[];$sd=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$td=bless({$V1,$rd,$v1,$sd},$S);$ud=q'max';$vd=[];$wd=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$xd=bless({$V1,$vd,$v1,$wd},$S);$yd=q'maxstr';$zd=[];$Ad=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Bd=bless({$V1,$zd,$v1,$Ad},$S);$Cd=q'mean';$Dd=[];$Ed=q'sum(@_) / (@_ || 1)';$Fd=bless({$V1,$Dd,$v1,$Ed},$S);$Gd=q'min';$Hd=[];$Id=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$Jd=bless({$V1,$Hd,$v1,$Id},$S);$Kd=q'minstr';$Ld=[];$Md=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Nd=bless({$V1,$Ld,$v1,$Md},$S);$Od=q'sgr';$Pd=[];$Qd=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Rd=bless({$V1,$Pd,$v1,$Qd},$S);$Sd=q'sr';$Td=[];$Ud=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Vd=bless({$V1,$Td,$v1,$Ud},$S);$Wd=q'sum';$Xd=[];$Yd=q'local $_; my $x = 0; $x += $_ for @_; $x';$Zd=bless({$V1,$Xd,$v1,$Yd},$S);$ce=q'swap';$de=[];$ee=q'@_[0, 1] = @_[1, 0]';$fe=bless({$V1,$de,$v1,$ee},$S);$ge={$id,$ld,$md,$pd,$qd,$td,$ud,$xd,$yd,$Bd,$Cd,$Fd,$Gd,$Jd,$Kd,$Nd,$Od,$Rd,$Sd,$Vd,$Wd,$Zd,$ce,$fe};$he=q'/lib/ni_static_util.b';$ie=bless({$c,$hd,$j1,$k1,$l1,$k1,$m1,$ge,$f,$he},$K);$je=q'/lib/slice::ctors';$ke=[$ie];$le=bless({$c,$gd,$f,$ed,$g,$ke},$Q);$me=q'/class::ctors';$ne=q'ni:/lib/ni_static_util.b';$oe=q'ni:/lib/perlbranch.b';$pe=q'ni:/lib/ref_eq.b';$qe=q'ni:/lib/resolver.b';$re=q'ni:/lib/slice';$se=q'ni:/lib/slice.b';$te=q'ni:/lib/slice.c';$ue=q'ni:/lib/slice_init.b';$ve=q'ni:/lib/slice_serialize.b';$we=q'ni:/lib/subclass.b';$xe=q'ni:/lib/tag';$ye=q'ni:/lib/tag.b';$ze=q'ni:/lib/tag.c';$Ae=q'ni:/lib/tag_init.b';$Be=q'ni:/metaclass';$Ce=q'ni:/metaclass.c';$De=q'ni:/object';$Ee=q'ni:/object.c';$Fe=q'ni:/unix/cat';$Ge={$s,1};$He=q'/unix/pipeline.c';$Ie={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$He,1,$z,1};$Je=[$Z3];$Ke=bless({$c,$Ie,$f,$w,$g,$Je},$E);$Le=q'/metaclass::ctors';$Me=[$Ke];$Ne=bless({$c,$Ge,$f,$s,$g,$Me},$E);$Oe=q'/metaclass::ctors';$Pe={$W,1};$Qe={$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1,$e1,1,$g1,1};$Re={};$Se=q'into';$Te=[];$Ue=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Ve=bless({$V1,$Te,$v1,$Ue},$S);$We={$Se,$Ve};$Xe=q'/unix/io_stream.b';$Ye=bless({$c,$Re,$j1,$k1,$l1,$k1,$m1,$We,$f,$Xe},$K);$Ze=q'/lib/slice::ctors';$cf={};$df=q'(+';$ef=[];$ff=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$gf=bless({$V1,$ef,$v1,$ff},$S);$hf={$df,$gf};$if=q'/unix/io_constructors.b';$jf=bless({$c,$cf,$j1,$k1,$l1,$k1,$m1,$hf,$f,$if},$K);$kf=q'/lib/slice::ctors';$lf={};$mf=q'(<>';$nf=[];$of=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$pf=bless({$V1,$nf,$v1,$of},$S);$qf=q'(@{}';$rf=[];$sf=q'my $self = shift; [<$self>]';$tf=bless({$V1,$rf,$v1,$sf},$S);$uf={$mf,$pf,$qf,$tf};$vf=q'/unix/io_readers.b';$wf=bless({$c,$lf,$j1,$k1,$l1,$k1,$m1,$uf,$f,$vf},$K);$xf=q'/lib/slice::ctors';$yf=[$T2,$Ye,$jf,$wf];$zf=bless({$c,$Qe,$f,$c1,$g,$yf},$w);$Af=q'/unix/io.c::ctors';$Bf={};$Cf=[];$Df=q'shift; +{fs => [@_]}';$Ef=bless({$V1,$Cf,$v1,$Df},$S);$Ff={$N1,$Ef};$Gf=q'/unix/cat_init.b';$Hf=bless({$c,$Bf,$j1,$k1,$l1,$k1,$m1,$Ff,$f,$Gf},$K);$If=q'/lib/slice::ctors';$Jf={};$Kf=q'read';$Lf=[];$Mf=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Nf=bless({$V1,$Lf,$v1,$Mf},$S);$Of={$Kf,$Nf};$Pf=q'/unix/cat_read.b';$Qf=bless({$c,$Jf,$j1,$k1,$l1,$k1,$m1,$Of,$f,$Pf},$K);$Rf=q'/lib/slice::ctors';$Sf=[$zf,$Hf,$Qf];$Tf=bless({$c,$Pe,$f,$W,$g,$Sf},$s);$Uf=q'/unix/cat.c::ctors';$Vf=q'ni:/unix/cat.c';$Wf=q'ni:/unix/cat_init.b';$Xf=q'ni:/unix/cat_read.b';$Yf=q'ni:/unix/fd';$Zf={$t,1};$cg=[$Ke];$dg=bless({$c,$Zf,$f,$t,$g,$cg},$E);$eg=q'/metaclass::ctors';$fg={$X,1};$gg={};$hg=q'fd';$ig=[];$jg=q'shift->{\'fd\'}';$kg=bless({$V1,$ig,$v1,$jg},$S);$lg={$hg,$kg};$mg=q'/unix/fd_readers.b';$ng=bless({$c,$gg,$j1,$k1,$l1,$k1,$m1,$lg,$f,$mg},$K);$og=q'/lib/slice::ctors';$pg={};$qg=[];$rg=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$sg=bless({$V1,$qg,$v1,$rg},$S);$tg={$N1,$sg};$ug=q'/unix/fd_init.b';$vg=bless({$c,$pg,$j1,$k1,$l1,$k1,$m1,$tg,$f,$ug},$K);$wg=q'/lib/slice::ctors';$xg={};$yg=q'move_to';$zg=[];$Ag=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Bg=bless({$V1,$zg,$v1,$Ag},$S);$Cg={$yg,$Bg};$Dg=q'/unix/fd_shell.b';$Eg=bless({$c,$xg,$j1,$k1,$l1,$k1,$m1,$Cg,$f,$Dg},$K);$Fg=q'/lib/slice::ctors';$Gg={$X,1,$Y,1,$Z,1,$d1,1,$e1,1};$Hg=q'/unix/has_fd.b';$Ig={};$Jg=[];$Kg=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Lg=bless({$V1,$Jg,$v1,$Kg},$S);$Mg=[];$Ng=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Og=bless({$V1,$Mg,$v1,$Ng},$S);$Pg={$Kf,$Lg,$Ac,$Og};$Qg=q'/unix/fd_safeio.b';$Rg=bless({$c,$Ig,$j1,$k1,$l1,$k1,$m1,$Pg,$f,$Qg},$K);$Sg=q'/lib/slice::ctors';$Tg=[$Rg];$Ug=bless({$c,$Gg,$f,$Hg,$g,$Tg},$N);$Vg=q'/lib/branch::ctors';$Wg={};$Xg=q'read_fh';$Yg=[];$Zg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$ch=bless({$V1,$Yg,$v1,$Zg},$S);$dh=q'write_fh';$eh=[];$fh=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$gh=bless({$V1,$eh,$v1,$fh},$S);$hh={$Xg,$ch,$dh,$gh};$ih=q'/unix/fd_io.b';$jh=bless({$c,$Wg,$j1,$k1,$l1,$k1,$m1,$hh,$f,$ih},$K);$kh=q'/lib/slice::ctors';$lh=[$zf,$ng,$vg,$Eg,$Ug,$jh];$mh=bless({$c,$fg,$f,$X,$g,$lh},$t);$nh=q'/unix/fd.c::ctors';$oh=q'ni:/unix/fd.c';$ph=q'ni:/unix/fd_init.b';$qh=q'ni:/unix/fd_io.b';$rh=q'ni:/unix/fd_readers.b';$sh=q'ni:/unix/fd_safeio.b';$th=q'ni:/unix/fd_shell.b';$uh=q'ni:/unix/fifo';$vh={$u,1};$wh=[$Ke];$xh=bless({$c,$vh,$f,$u,$g,$wh},$E);$yh=q'/metaclass::ctors';$zh={$Y,1};$Ah={};$Bh=[];$Ch=q'shift->{\'read_fh\'}';$Dh=bless({$V1,$Bh,$v1,$Ch},$S);$Eh=[];$Fh=q'shift->{\'write_fh\'}';$Gh=bless({$V1,$Eh,$v1,$Fh},$S);$Hh={$Xg,$Dh,$dh,$Gh};$Ih=q'/unix/fifo_io.b';$Jh=bless({$c,$Ah,$j1,$k1,$l1,$k1,$m1,$Hh,$f,$Ih},$K);$Kh=q'/lib/slice::ctors';$Lh={};$Mh=[];$Nh=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Oh=bless({$V1,$Mh,$v1,$Nh},$S);$Ph={$N1,$Oh};$Qh=q'/unix/fifo_init.b';$Rh=bless({$c,$Lh,$j1,$k1,$l1,$k1,$m1,$Ph,$f,$Qh},$K);$Sh=q'/lib/slice::ctors';$Th={};$Uh=q'read_side';$Vh=[];$Wh=q'my $self = shift; close $$self{write_fh}; $self';$Xh=bless({$V1,$Vh,$v1,$Wh},$S);$Yh=q'write_side';$Zh=[];$ci=q'my $self = shift; close $$self{read_fh};  $self';$di=bless({$V1,$Zh,$v1,$ci},$S);$ei={$Uh,$Xh,$Yh,$di};$fi=q'/unix/fifo_direction.b';$gi=bless({$c,$Th,$j1,$k1,$l1,$k1,$m1,$ei,$f,$fi},$K);$hi=q'/lib/slice::ctors';$ii=[$zf,$Jh,$Rh,$Ug,$gi];$ji=bless({$c,$zh,$f,$Y,$g,$ii},$u);$ki=q'/unix/fifo.c::ctors';$li=q'ni:/unix/fifo.c';$mi=q'ni:/unix/fifo_direction.b';$ni=q'ni:/unix/fifo_init.b';$oi=q'ni:/unix/fifo_io.b';$pi=q'ni:/unix/file';$qi={$v,1};$ri=[$Ke];$si=bless({$c,$qi,$f,$v,$g,$ri},$E);$ti=q'/metaclass::ctors';$ui={$Z,1};$vi={};$wi=[];$xi=q'shift->{\'name\'}';$yi=bless({$V1,$wi,$v1,$xi},$S);$zi={$f,$yi};$Ai=q'/unix/file_readers.b';$Bi=bless({$c,$vi,$j1,$k1,$l1,$k1,$m1,$zi,$f,$Ai},$K);$Ci=q'/lib/slice::ctors';$Di={};$Ei=[];$Fi=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Gi=bless({$V1,$Ei,$v1,$Fi},$S);$Hi={$N1,$Gi};$Ii=q'/unix/file_init.b';$Ji=bless({$c,$Di,$j1,$k1,$l1,$k1,$m1,$Hi,$f,$Ii},$K);$Ki=q'/lib/slice::ctors';$Li={};$Mi=[];$Ni=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Oi=bless({$V1,$Mi,$v1,$Ni},$S);$Pi=[];$Qi=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Ri=bless({$V1,$Pi,$v1,$Qi},$S);$Si={$Xg,$Oi,$dh,$Ri};$Ti=q'/unix/file_io.b';$Ui=bless({$c,$Li,$j1,$k1,$l1,$k1,$m1,$Si,$f,$Ti},$K);$Vi=q'/lib/slice::ctors';$Wi=[$zf,$Bi,$Ji,$Ug,$Ui];$Xi=bless({$c,$ui,$f,$Z,$g,$Wi},$v);$Yi=q'/unix/file.c::ctors';$Zi=q'ni:/unix/file.c';$cj=q'ni:/unix/file_init.b';$dj=q'ni:/unix/file_io.b';$ej=q'ni:/unix/file_readers.b';$fj=q'ni:/unix/has_fd.b';$gj=q'ni:/unix/io';$hj=q'ni:/unix/io.c';$ij=q'ni:/unix/io_constructors.b';$jj=q'ni:/unix/io_readers.b';$kj=q'ni:/unix/io_stream.b';$lj=q'ni:/unix/pid';$mj={$x,1};$nj=[$Ke];$oj=bless({$c,$mj,$f,$x,$g,$nj},$E);$pj=q'/metaclass::ctors';$qj={$d1,1};$rj={};$sj=q'pid';$tj=[];$uj=q'shift->{\'pid\'}';$vj=bless({$V1,$tj,$v1,$uj},$S);$wj=q'stderr';$xj=[];$yj=q'shift->{\'stderr\'}';$zj=bless({$V1,$xj,$v1,$yj},$S);$Aj=q'stdin';$Bj=[];$Cj=q'shift->{\'stdin\'}';$Dj=bless({$V1,$Bj,$v1,$Cj},$S);$Ej=q'stdout';$Fj=[];$Gj=q'shift->{\'stdout\'}';$Hj=bless({$V1,$Fj,$v1,$Gj},$S);$Ij={$sj,$vj,$wj,$zj,$Aj,$Dj,$Ej,$Hj};$Jj=q'/unix/pid_readers.b';$Kj=bless({$c,$rj,$j1,$k1,$l1,$k1,$m1,$Ij,$f,$Jj},$K);$Lj=q'/lib/slice::ctors';$Mj={};$Nj=[];$Oj=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Pj=bless({$V1,$Nj,$v1,$Oj},$S);$Qj={$N1,$Pj};$Rj=q'/unix/pid_init.b';$Sj=bless({$c,$Mj,$j1,$k1,$l1,$k1,$m1,$Qj,$f,$Rj},$K);$Tj=q'/lib/slice::ctors';$Uj={};$Vj={};$Wj=q'/unix/pid_wait.b';$Xj=bless({$c,$Uj,$j1,$k1,$l1,$k1,$m1,$Vj,$f,$Wj},$K);$Yj=q'/lib/slice::ctors';$Zj={};$ck=[];$dk=q'shift->{stdout}->read_fh';$ek=bless({$V1,$ck,$v1,$dk},$S);$fk=[];$gk=q'shift->{stdin}->write_fh';$hk=bless({$V1,$fk,$v1,$gk},$S);$ik={$Xg,$ek,$dh,$hk};$jk=q'/unix/pid_io.b';$kk=bless({$c,$Zj,$j1,$k1,$l1,$k1,$m1,$ik,$f,$jk},$K);$lk=q'/lib/slice::ctors';$mk=[$zf,$Kj,$Sj,$Xj,$Ug,$kk];$nk=bless({$c,$qj,$f,$d1,$g,$mk},$x);$ok=q'/unix/pid.c::ctors';$pk=q'ni:/unix/pid.c';$qk=q'ni:/unix/pid_init.b';$rk=q'ni:/unix/pid_io.b';$sk=q'ni:/unix/pid_readers.b';$tk=q'ni:/unix/pid_wait.b';$uk=q'ni:/unix/pipeline';$vk=q'/unix/pipeline.c';$wk={$vk,1};$xk=q'/unix/pipeline.c';$yk=[$Ke];$zk=bless({$c,$wk,$f,$xk,$g,$yk},$E);$Ak=q'/metaclass::ctors';$Bk={$e1,1};$Ck={};$Dk=[];$Ek=q'shift->{\'stdin\'}';$Fk=bless({$V1,$Dk,$v1,$Ek},$S);$Gk=[];$Hk=q'shift->{\'stdout\'}';$Ik=bless({$V1,$Gk,$v1,$Hk},$S);$Jk={$Aj,$Fk,$Ej,$Ik};$Kk=q'/unix/pipeline_ro.b';$Lk=bless({$c,$Ck,$j1,$k1,$l1,$k1,$m1,$Jk,$f,$Kk},$K);$Mk=q'/lib/slice::ctors';$Nk={};$Ok=[];$Pk=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Qk=bless({$V1,$Ok,$v1,$Pk},$S);$Rk={$N1,$Qk};$Sk=q'/unix/pipeline_init.b';$Tk=bless({$c,$Nk,$j1,$k1,$l1,$k1,$m1,$Rk,$f,$Sk},$K);$Uk=q'/lib/slice::ctors';$Vk={};$Wk=q'async_step';$Xk=[];$Yk=q'local $_;
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
$self;';$Zk=bless({$V1,$Xk,$v1,$Yk},$S);$cl={$Wk,$Zk};$dl=q'/unix/pipeline_async.b';$el=bless({$c,$Vk,$j1,$k1,$l1,$k1,$m1,$cl,$f,$dl},$K);$fl=q'/lib/slice::ctors';$gl={};$hl=[];$il=q'shift->{stdout}->read_fh';$jl=bless({$V1,$hl,$v1,$il},$S);$kl=[];$ll=q'shift->{stdin}->write_fh';$ml=bless({$V1,$kl,$v1,$ll},$S);$nl={$Xg,$jl,$dh,$ml};$ol=q'/unix/pipeline_io.b';$pl=bless({$c,$gl,$j1,$k1,$l1,$k1,$m1,$nl,$f,$ol},$K);$ql=q'/lib/slice::ctors';$rl=[$zf,$Lk,$Tk,$el,$Ug,$pl];$sl=q'/unix/pipeline.c';$tl=bless({$c,$Bk,$f,$e1,$g,$rl},$sl);$ul=q'/unix/pipeline.c::ctors';$vl=q'ni:/unix/pipeline.c';$wl=q'ni:/unix/pipeline_async.b';$xl=q'ni:/unix/pipeline_init.b';$yl=q'ni:/unix/pipeline_io.b';$zl=q'ni:/unix/pipeline_ro.b';$Al=q'ni:/unix/str';$Bl={$z,1};$Cl=[$Ke];$Dl=bless({$c,$Bl,$f,$z,$g,$Cl},$E);$El=q'/metaclass::ctors';$Fl={$g1,1};$Gl={};$Hl=[];$Il=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Jl=bless({$V1,$Hl,$v1,$Il},$S);$Kl={$N1,$Jl};$Ll=q'/unix/str_init.b';$Ml=bless({$c,$Gl,$j1,$k1,$l1,$k1,$m1,$Kl,$f,$Ll},$K);$Nl=q'/lib/slice::ctors';$Ol={};$Pl=[];$Ql=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Rl=bless({$V1,$Pl,$v1,$Ql},$S);$Sl=q'remaining';$Tl=[];$Ul=q'my $self = shift; $$self{end} - $$self{start}';$Vl=bless({$V1,$Tl,$v1,$Ul},$S);$Wl=[];$Xl=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Yl=bless({$V1,$Wl,$v1,$Xl},$S);$Zl={$Kf,$Rl,$Sl,$Vl,$Ac,$Yl};$cm=q'/unix/str_io.b';$dm=bless({$c,$Ol,$j1,$k1,$l1,$k1,$m1,$Zl,$f,$cm},$K);$em=q'/lib/slice::ctors';$fm=[$zf,$Ml,$dm];$gm=bless({$c,$Fl,$f,$g1,$g,$fm},$z);$hm=q'/unix/str.c::ctors';$im=q'ni:/unix/str.c';$jm=q'ni:/unix/str_init.b';$km=q'ni:/unix/str_io.b';$lm={$S8,$Y9,$ca,$ra,$sa,$Y6,$ta,$e4,$ua,$s6,$va,$W2,$wa,$F,$xa,$T5,$ya,$N4,$za,$I5,$Aa,$Q5,$Ba,$D5,$Ca,$g6,$Da,$M6,$Ea,$I9,$Fa,$V8,$Ga,$u9,$Ha,$f9,$Ia,$m9,$Ja,$F9,$Ka,$V3,$La,$J2,$Ma,$q1,$Na,$S1,$Oa,$x2,$Pa,$k2,$Qa,$G2,$Ra,$Jc,$Lc,$Ua,$Mc,$fb,$Nc,$Gc,$Oc,$P2,$Pc,$E1,$Qc,$A6,$Rc,$h3,$Sc,$W4,$Tc,$h5,$Uc,$P8,$Vc,$l7,$Wc,$M7,$Xc,$n8,$Yc,$M8,$Zc,$z8,$cd,$z7,$dd,$le,$ne,$ie,$oe,$u5,$pe,$I6,$qe,$q5,$re,$N3,$se,$v3,$te,$I,$ue,$C3,$ve,$K3,$we,$U6,$xe,$C4,$ye,$s4,$ze,$k4,$Ae,$z4,$Be,$h7,$Ce,$d7,$De,$T2,$Ee,$Z3,$Fe,$Tf,$Vf,$Ne,$Wf,$Hf,$Xf,$Qf,$Yf,$mh,$oh,$dg,$ph,$vg,$qh,$jh,$rh,$ng,$sh,$Rg,$th,$Eg,$uh,$ji,$li,$xh,$mi,$gi,$ni,$Rh,$oi,$Jh,$pi,$Xi,$Zi,$si,$cj,$Ji,$dj,$Ui,$ej,$Bi,$fj,$Ug,$gj,$zf,$hj,$Ke,$ij,$jf,$jj,$wf,$kj,$Ye,$lj,$nk,$pk,$oj,$qk,$Sj,$rk,$kk,$sk,$Kj,$tk,$Xj,$uk,$tl,$vl,$zk,$wl,$el,$xl,$Tk,$yl,$pl,$zl,$Lk,$Al,$gm,$im,$Dl,$jm,$Ml,$km,$dm};$mm=q'resolvers';$nm=[];$om=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$pm=bless({$V1,$nm,$v1,$om},$S);$qm=q'file';$rm=[];$sm=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$tm=bless({$V1,$rm,$v1,$sm},$S);$um=q'str';$vm=[];$wm=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$xm=bless({$V1,$vm,$v1,$wm},$S);$ym={$hg,$pm,$qm,$tm,$um,$xm};$zm=bless({$R8,$lm,$mm,$ym},$U);$Am=q'/lib/ni::ctors';$$Y3[0]=$Y6;$$D[0]=$Z3;$$p1[0]=$Z3;$$I2[0]=$T2;$$S5[4]=$M6;*$p3=\&$n3;*$o3=\&$l3;$E1->apply_unsafe($Q);$E1->apply_unsafe($h);$E1->apply_unsafe($j);$E1->apply_unsafe($N);$E1->apply_unsafe($k);$E1->apply_unsafe($l);$E1->apply_unsafe($S);$E1->apply_unsafe($m);$E1->apply_unsafe($n);$E1->apply_unsafe($o);$E1->apply_unsafe($K);$E1->apply_unsafe($p);$E1->apply_unsafe($O);$E1->apply_unsafe($q);$E1->apply_unsafe($E);$E1->apply_unsafe($d);$E1->apply_unsafe($r);$E1->apply_unsafe($s);$E1->apply_unsafe($t);$E1->apply_unsafe($u);$E1->apply_unsafe($v);$E1->apply_unsafe($w);$E1->apply_unsafe($x);$E1->apply_unsafe($F1);$E1->apply_unsafe($z);$S1->apply_unsafe($S);$k2->apply_unsafe($S);$x2->apply_unsafe($S);$G2->apply_unsafe($S);$P2->apply_unsafe($Q);$P2->apply_unsafe($h);$P2->apply_unsafe($M);$P2->apply_unsafe($j);$P2->apply_unsafe($N);$P2->apply_unsafe($k);$P2->apply_unsafe($R);$P2->apply_unsafe($l);$P2->apply_unsafe($S);$P2->apply_unsafe($m);$P2->apply_unsafe($T);$P2->apply_unsafe($n);$P2->apply_unsafe($U);$P2->apply_unsafe($o);$P2->apply_unsafe($K);$P2->apply_unsafe($p);$P2->apply_unsafe($O);$P2->apply_unsafe($q);$P2->apply_unsafe($E);$P2->apply_unsafe($d);$P2->apply_unsafe($V);$P2->apply_unsafe($r);$P2->apply_unsafe($W);$P2->apply_unsafe($s);$P2->apply_unsafe($X);$P2->apply_unsafe($t);$P2->apply_unsafe($Y);$P2->apply_unsafe($u);$P2->apply_unsafe($Z);$P2->apply_unsafe($v);$P2->apply_unsafe($c1);$P2->apply_unsafe($w);$P2->apply_unsafe($d1);$P2->apply_unsafe($x);$P2->apply_unsafe($e1);$P2->apply_unsafe($Q2);$P2->apply_unsafe($g1);$P2->apply_unsafe($z);$h3->apply_unsafe($Q);$h3->apply_unsafe($h);$h3->apply_unsafe($j);$h3->apply_unsafe($N);$h3->apply_unsafe($k);$h3->apply_unsafe($R);$h3->apply_unsafe($l);$h3->apply_unsafe($m);$h3->apply_unsafe($n);$h3->apply_unsafe($o);$h3->apply_unsafe($K);$h3->apply_unsafe($p);$h3->apply_unsafe($O);$h3->apply_unsafe($q);$h3->apply_unsafe($E);$h3->apply_unsafe($d);$h3->apply_unsafe($r);$h3->apply_unsafe($s);$h3->apply_unsafe($t);$h3->apply_unsafe($u);$h3->apply_unsafe($v);$h3->apply_unsafe($w);$h3->apply_unsafe($x);$h3->apply_unsafe($i3);$h3->apply_unsafe($z);$v3->apply_unsafe($K);$C3->apply_unsafe($K);$K3->apply_unsafe($K);$V3->apply_unsafe($h);$V3->apply_unsafe($j);$V3->apply_unsafe($k);$V3->apply_unsafe($l);$V3->apply_unsafe($m);$V3->apply_unsafe($n);$V3->apply_unsafe($o);$V3->apply_unsafe($p);$V3->apply_unsafe($q);$V3->apply_unsafe($r);$V3->apply_unsafe($s);$V3->apply_unsafe($t);$V3->apply_unsafe($u);$V3->apply_unsafe($v);$V3->apply_unsafe($w);$V3->apply_unsafe($x);$V3->apply_unsafe($W3);$V3->apply_unsafe($z);$s4->apply_unsafe($O);$z4->apply_unsafe($O);$N4->apply_unsafe($Q);$N4->apply_unsafe($h);$N4->apply_unsafe($j);$N4->apply_unsafe($N);$N4->apply_unsafe($k);$N4->apply_unsafe($l);$N4->apply_unsafe($m);$N4->apply_unsafe($n);$N4->apply_unsafe($o);$N4->apply_unsafe($p);$N4->apply_unsafe($q);$N4->apply_unsafe($E);$N4->apply_unsafe($d);$N4->apply_unsafe($r);$N4->apply_unsafe($s);$N4->apply_unsafe($t);$N4->apply_unsafe($u);$N4->apply_unsafe($v);$N4->apply_unsafe($w);$N4->apply_unsafe($x);$N4->apply_unsafe($O4);$N4->apply_unsafe($z);$W4->apply_unsafe($Q);$W4->apply_unsafe($h);$W4->apply_unsafe($j);$W4->apply_unsafe($N);$W4->apply_unsafe($k);$W4->apply_unsafe($l);$W4->apply_unsafe($m);$W4->apply_unsafe($n);$W4->apply_unsafe($o);$W4->apply_unsafe($K);$W4->apply_unsafe($p);$W4->apply_unsafe($O);$W4->apply_unsafe($q);$W4->apply_unsafe($E);$W4->apply_unsafe($d);$W4->apply_unsafe($r);$W4->apply_unsafe($s);$W4->apply_unsafe($t);$W4->apply_unsafe($u);$W4->apply_unsafe($v);$W4->apply_unsafe($w);$W4->apply_unsafe($x);$W4->apply_unsafe($X4);$W4->apply_unsafe($z);$h5->apply_unsafe($Q);$h5->apply_unsafe($h);$h5->apply_unsafe($j);$h5->apply_unsafe($N);$h5->apply_unsafe($k);$h5->apply_unsafe($l);$h5->apply_unsafe($m);$h5->apply_unsafe($n);$h5->apply_unsafe($o);$h5->apply_unsafe($K);$h5->apply_unsafe($p);$h5->apply_unsafe($O);$h5->apply_unsafe($q);$h5->apply_unsafe($E);$h5->apply_unsafe($d);$h5->apply_unsafe($r);$h5->apply_unsafe($s);$h5->apply_unsafe($t);$h5->apply_unsafe($u);$h5->apply_unsafe($v);$h5->apply_unsafe($w);$h5->apply_unsafe($x);$h5->apply_unsafe($i5);$h5->apply_unsafe($z);$q5->apply_unsafe($Q);$q5->apply_unsafe($h);$q5->apply_unsafe($j);$q5->apply_unsafe($N);$q5->apply_unsafe($k);$q5->apply_unsafe($l);$q5->apply_unsafe($m);$q5->apply_unsafe($n);$q5->apply_unsafe($o);$q5->apply_unsafe($p);$q5->apply_unsafe($O);$q5->apply_unsafe($q);$q5->apply_unsafe($E);$q5->apply_unsafe($d);$q5->apply_unsafe($r);$q5->apply_unsafe($s);$q5->apply_unsafe($t);$q5->apply_unsafe($u);$q5->apply_unsafe($v);$q5->apply_unsafe($w);$q5->apply_unsafe($x);$q5->apply_unsafe($r5);$q5->apply_unsafe($z);$D5->apply_unsafe($Q);$D5->apply_unsafe($h);$D5->apply_unsafe($j);$D5->apply_unsafe($k);$D5->apply_unsafe($l);$D5->apply_unsafe($m);$D5->apply_unsafe($n);$D5->apply_unsafe($o);$D5->apply_unsafe($p);$D5->apply_unsafe($q);$D5->apply_unsafe($E);$D5->apply_unsafe($d);$D5->apply_unsafe($r);$D5->apply_unsafe($s);$D5->apply_unsafe($t);$D5->apply_unsafe($u);$D5->apply_unsafe($v);$D5->apply_unsafe($w);$D5->apply_unsafe($x);$D5->apply_unsafe($E5);$D5->apply_unsafe($z);$Q5->apply_unsafe($N);$g6->apply_unsafe($Q);$g6->apply_unsafe($h);$g6->apply_unsafe($j);$g6->apply_unsafe($N);$g6->apply_unsafe($k);$g6->apply_unsafe($l);$g6->apply_unsafe($m);$g6->apply_unsafe($n);$g6->apply_unsafe($o);$g6->apply_unsafe($p);$g6->apply_unsafe($q);$g6->apply_unsafe($E);$g6->apply_unsafe($d);$g6->apply_unsafe($r);$g6->apply_unsafe($s);$g6->apply_unsafe($t);$g6->apply_unsafe($u);$g6->apply_unsafe($v);$g6->apply_unsafe($w);$g6->apply_unsafe($x);$g6->apply_unsafe($h6);$g6->apply_unsafe($z);$s6->apply_unsafe($Q);$s6->apply_unsafe($h);$s6->apply_unsafe($j);$s6->apply_unsafe($N);$s6->apply_unsafe($k);$s6->apply_unsafe($l);$s6->apply_unsafe($m);$s6->apply_unsafe($n);$s6->apply_unsafe($o);$s6->apply_unsafe($p);$s6->apply_unsafe($q);$s6->apply_unsafe($E);$s6->apply_unsafe($d);$s6->apply_unsafe($r);$s6->apply_unsafe($s);$s6->apply_unsafe($t);$s6->apply_unsafe($u);$s6->apply_unsafe($v);$s6->apply_unsafe($w);$s6->apply_unsafe($x);$s6->apply_unsafe($t6);$s6->apply_unsafe($z);$A6->apply_unsafe($Q);$A6->apply_unsafe($h);$A6->apply_unsafe($j);$A6->apply_unsafe($N);$A6->apply_unsafe($k);$A6->apply_unsafe($l);$A6->apply_unsafe($m);$A6->apply_unsafe($n);$A6->apply_unsafe($o);$A6->apply_unsafe($p);$A6->apply_unsafe($q);$A6->apply_unsafe($E);$A6->apply_unsafe($d);$A6->apply_unsafe($r);$A6->apply_unsafe($s);$A6->apply_unsafe($t);$A6->apply_unsafe($u);$A6->apply_unsafe($v);$A6->apply_unsafe($w);$A6->apply_unsafe($x);$A6->apply_unsafe($B6);$A6->apply_unsafe($z);$I6->apply_unsafe($Q);$I6->apply_unsafe($h);$I6->apply_unsafe($j);$I6->apply_unsafe($N);$I6->apply_unsafe($k);$I6->apply_unsafe($l);$I6->apply_unsafe($m);$I6->apply_unsafe($n);$I6->apply_unsafe($o);$I6->apply_unsafe($p);$I6->apply_unsafe($q);$I6->apply_unsafe($E);$I6->apply_unsafe($d);$I6->apply_unsafe($r);$I6->apply_unsafe($s);$I6->apply_unsafe($t);$I6->apply_unsafe($u);$I6->apply_unsafe($v);$I6->apply_unsafe($w);$I6->apply_unsafe($x);$I6->apply_unsafe($J6);$I6->apply_unsafe($z);$U6->apply_unsafe($Q);$U6->apply_unsafe($h);$U6->apply_unsafe($j);$U6->apply_unsafe($k);$U6->apply_unsafe($l);$U6->apply_unsafe($m);$U6->apply_unsafe($n);$U6->apply_unsafe($o);$U6->apply_unsafe($p);$U6->apply_unsafe($q);$U6->apply_unsafe($d);$U6->apply_unsafe($r);$U6->apply_unsafe($s);$U6->apply_unsafe($t);$U6->apply_unsafe($u);$U6->apply_unsafe($v);$U6->apply_unsafe($w);$U6->apply_unsafe($x);$U6->apply_unsafe($V6);$U6->apply_unsafe($z);$z7->apply_unsafe($U);$M7->apply_unsafe($U);$n8->apply_unsafe($U);$z8->apply_unsafe($U);$M8->apply_unsafe($U);$f9->apply_unsafe($R);$m9->apply_unsafe($R);$u9->apply_unsafe($R);$F9->apply_unsafe($R);$fb->apply_unsafe($T);$Gc->apply_unsafe($T);$ie->apply_unsafe($ed);$ie->apply_unsafe($fd);$Ye->apply_unsafe($W);$Ye->apply_unsafe($X);$Ye->apply_unsafe($Y);$Ye->apply_unsafe($Z);$Ye->apply_unsafe($c1);$Ye->apply_unsafe($d1);$Ye->apply_unsafe($e1);$Ye->apply_unsafe($g1);$jf->apply_unsafe($W);$jf->apply_unsafe($X);$jf->apply_unsafe($Y);$jf->apply_unsafe($Z);$jf->apply_unsafe($c1);$jf->apply_unsafe($d1);$jf->apply_unsafe($e1);$jf->apply_unsafe($g1);$wf->apply_unsafe($W);$wf->apply_unsafe($X);$wf->apply_unsafe($Y);$wf->apply_unsafe($Z);$wf->apply_unsafe($c1);$wf->apply_unsafe($d1);$wf->apply_unsafe($e1);$wf->apply_unsafe($g1);$Hf->apply_unsafe($W);$Qf->apply_unsafe($W);$ng->apply_unsafe($X);$vg->apply_unsafe($X);$Eg->apply_unsafe($X);$Rg->apply_unsafe($X);$Rg->apply_unsafe($Y);$Rg->apply_unsafe($Z);$Rg->apply_unsafe($d1);$Rg->apply_unsafe($e1);$jh->apply_unsafe($X);$Jh->apply_unsafe($Y);$Rh->apply_unsafe($Y);$gi->apply_unsafe($Y);$Bi->apply_unsafe($Z);$Ji->apply_unsafe($Z);$Ui->apply_unsafe($Z);$Kj->apply_unsafe($d1);$Sj->apply_unsafe($d1);$Xj->apply_unsafe($d1);$kk->apply_unsafe($d1);$Lk->apply_unsafe($e1);$Tk->apply_unsafe($e1);$el->apply_unsafe($e1);$pl->apply_unsafe($e1);$Ml->apply_unsafe($g1);$dm->apply_unsafe($g1);$ni::self=$zm;&$_($F)for@$G;&$_($I)for@$J;&$_($q1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$y1;&$_($E1)for@$G1;&$_($J1)for@$y1;&$_($M1)for@$y1;&$_($P1)for@$y1;&$_($S1)for@$T1;&$_($Y1)for@$y1;&$_($d2)for@$y1;&$_($h2)for@$y1;&$_($k2)for@$l2;&$_($q2)for@$y1;&$_($u2)for@$y1;&$_($x2)for@$y2;&$_($D2)for@$y1;&$_($G2)for@$H2;&$_($J2)for@$K2;&$_($M2)for@$y1;&$_($P2)for@$R2;&$_($T2)for@$U2;&$_($W2)for@$X2;&$_($c3)for@$y1;&$_($e3)for@$y1;&$_($h3)for@$j3;&$_($l3)for@$y1;&$_($n3)for@$y1;&$_($v3)for@$w3;&$_($z3)for@$y1;&$_($C3)for@$D3;&$_($H3)for@$y1;&$_($K3)for@$L3;&$_($N3)for@$O3;&$_($S3)for@$y1;&$_($V3)for@$X3;&$_($Z3)for@$c4;&$_($e4)for@$f4;&$_($k4)for@$l4;&$_($p4)for@$y1;&$_($s4)for@$t4;&$_($w4)for@$y1;&$_($z4)for@$A4;&$_($C4)for@$D4;&$_($I4)for@$y1;&$_($K4)for@$y1;&$_($N4)for@$P4;&$_($T4)for@$y1;&$_($W4)for@$Y4;&$_($e5)for@$y1;&$_($h5)for@$j5;&$_($n5)for@$y1;&$_($q5)for@$s5;&$_($u5)for@$v5;&$_($y5)for@$y1;&$_($A5)for@$y1;&$_($D5)for@$F5;&$_($I5)for@$J5;&$_($N5)for@$y1;&$_($Q5)for@$R5;&$_($T5)for@$U5;&$_($d6)for@$y1;&$_($g6)for@$i6;&$_($m6)for@$y1;&$_($p6)for@$y1;&$_($s6)for@$u6;&$_($x6)for@$y1;&$_($A6)for@$C6;&$_($F6)for@$y1;&$_($I6)for@$K6;&$_($M6)for@$N6;&$_($R6)for@$y1;&$_($U6)for@$W6;&$_($Y6)for@$Z6;&$_($d7)for@$e7;&$_($h7)for@$i7;&$_($l7)for@$m7;&$_($s7)for@$y1;&$_($w7)for@$y1;&$_($z7)for@$A7;&$_($F7)for@$y1;&$_($J7)for@$y1;&$_($M7)for@$N7;&$_($S7)for@$y1;&$_($W7)for@$y1;&$_($c8)for@$y1;&$_($g8)for@$y1;&$_($k8)for@$y1;&$_($n8)for@$o8;&$_($s8)for@$y1;&$_($w8)for@$y1;&$_($z8)for@$A8;&$_($F8)for@$y1;&$_($J8)for@$y1;&$_($M8)for@$N8;&$_($P8)for@$Q8;&$_($V8)for@$W8;&$_($c9)for@$y1;&$_($f9)for@$g9;&$_($j9)for@$y1;&$_($m9)for@$n9;&$_($r9)for@$y1;&$_($u9)for@$v9;&$_($z9)for@$y1;&$_($C9)for@$y1;&$_($F9)for@$G9;&$_($I9)for@$J9;&$_($Y9)for@$Z9;&$_($ka)for@$y1;&$_($oa)for@$y1;&$_($ra)for@$Z9;&$_($Ua)for@$Va;&$_($cb)for@$y1;&$_($fb)for@$gb;&$_($lb)for@$y1;&$_($pb)for@$y1;&$_($tb)for@$y1;&$_($xb)for@$y1;&$_($Bb)for@$y1;&$_($Fb)for@$y1;&$_($Jb)for@$y1;&$_($Nb)for@$y1;&$_($Rb)for@$y1;&$_($Vb)for@$y1;&$_($Zb)for@$y1;&$_($fc)for@$y1;&$_($jc)for@$y1;&$_($nc)for@$y1;&$_($rc)for@$y1;&$_($vc)for@$y1;&$_($zc)for@$y1;&$_($Dc)for@$y1;&$_($Gc)for@$Hc;&$_($Jc)for@$Kc;&$_($ld)for@$y1;&$_($pd)for@$y1;&$_($td)for@$y1;&$_($xd)for@$y1;&$_($Bd)for@$y1;&$_($Fd)for@$y1;&$_($Jd)for@$y1;&$_($Nd)for@$y1;&$_($Rd)for@$y1;&$_($Vd)for@$y1;&$_($Zd)for@$y1;&$_($fe)for@$y1;&$_($ie)for@$je;&$_($le)for@$me;&$_($Ke)for@$Le;&$_($Ne)for@$Oe;&$_($Ve)for@$y1;&$_($Ye)for@$Ze;&$_($gf)for@$y1;&$_($jf)for@$kf;&$_($pf)for@$y1;&$_($tf)for@$y1;&$_($wf)for@$xf;&$_($zf)for@$Af;&$_($Ef)for@$y1;&$_($Hf)for@$If;&$_($Nf)for@$y1;&$_($Qf)for@$Rf;&$_($Tf)for@$Uf;&$_($dg)for@$eg;&$_($kg)for@$y1;&$_($ng)for@$og;&$_($sg)for@$y1;&$_($vg)for@$wg;&$_($Bg)for@$y1;&$_($Eg)for@$Fg;&$_($Lg)for@$y1;&$_($Og)for@$y1;&$_($Rg)for@$Sg;&$_($Ug)for@$Vg;&$_($ch)for@$y1;&$_($gh)for@$y1;&$_($jh)for@$kh;&$_($mh)for@$nh;&$_($xh)for@$yh;&$_($Dh)for@$y1;&$_($Gh)for@$y1;&$_($Jh)for@$Kh;&$_($Oh)for@$y1;&$_($Rh)for@$Sh;&$_($Xh)for@$y1;&$_($di)for@$y1;&$_($gi)for@$hi;&$_($ji)for@$ki;&$_($si)for@$ti;&$_($yi)for@$y1;&$_($Bi)for@$Ci;&$_($Gi)for@$y1;&$_($Ji)for@$Ki;&$_($Oi)for@$y1;&$_($Ri)for@$y1;&$_($Ui)for@$Vi;&$_($Xi)for@$Yi;&$_($oj)for@$pj;&$_($vj)for@$y1;&$_($zj)for@$y1;&$_($Dj)for@$y1;&$_($Hj)for@$y1;&$_($Kj)for@$Lj;&$_($Pj)for@$y1;&$_($Sj)for@$Tj;&$_($Xj)for@$Yj;&$_($ek)for@$y1;&$_($hk)for@$y1;&$_($kk)for@$lk;&$_($nk)for@$ok;&$_($zk)for@$Ak;&$_($Fk)for@$y1;&$_($Ik)for@$y1;&$_($Lk)for@$Mk;&$_($Qk)for@$y1;&$_($Tk)for@$Uk;&$_($Zk)for@$y1;&$_($el)for@$fl;&$_($jl)for@$y1;&$_($ml)for@$y1;&$_($pl)for@$ql;&$_($tl)for@$ul;&$_($Dl)for@$El;&$_($Jl)for@$y1;&$_($Ml)for@$Nl;&$_($Rl)for@$y1;&$_($Vl)for@$y1;&$_($Yl)for@$y1;&$_($dm)for@$em;&$_($gm)for@$hm;&$_($pm)for@$y1;&$_($tm)for@$y1;&$_($xm)for@$y1;&$_($zm)for@$Am;ni->run(@ARGV);
__DATA__
