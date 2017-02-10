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
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$p6=bless({$v1,$o6},$S);$q6={$k6,$m6,$n6,$p6};$r6=q'/lib/accessor.b';$s6=bless({$c,$j6,$j1,$k1,$l1,$k1,$m1,$q6,$f,$r6},$K);$t6=q'/unix/pipeline.c';$u6=q'/lib/slice::ctors';$v6=[$g6,$s6];$w6=bless({$c,$W5,$f,$X5,$g,$v6},$N);$x6=q'/lib/branch::ctors';$y6={};$z6=q'child';$A6=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$B6=bless({$v1,$A6},$S);$C6={$z6,$B6};$D6=q'/lib/subclass.b';$E6=bless({$c,$y6,$j1,$k1,$l1,$k1,$m1,$C6,$f,$D6},$K);$F6=q'/unix/pipeline.c';$G6=q'/lib/slice::ctors';$H6=[$u5,$D5,$T2,$w6,$E6];$I6=bless({$c,$h4,$f,$Q,$g,$H6},$h);$J6=q'/class.c::ctors';$K6=[$I6];$L6=bless({$c,$e,$f,$d,$g,$K6},$E);$M6=q'/metaclass::ctors';$N6={$E,1};$O6=[$u5,$D5,$T2,$w6];$P6=bless({$c,$N6,$f,$E,$g,$O6},$d);$Q6=q'/metaclass.c::ctors';$R6={$o,1};$S6=[$Z3];$T6=bless({$c,$R6,$f,$o,$g,$S6},$E);$U6=q'/metaclass::ctors';$V6={$U,1};$W6={};$X6=q'is_mutable';$Y6=[];$Z6=q'$0 ne "-" && -w $0';$c7=bless({$V1,$Y6,$v1,$Z6},$S);$d7=q'modify';$e7=[];$f7=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$g7=bless({$V1,$e7,$v1,$f7},$S);$h7={$X6,$c7,$d7,$g7};$i7=q'/lib/ni_self.b';$j7=bless({$c,$W6,$j1,$k1,$l1,$k1,$m1,$h7,$f,$i7},$K);$k7=q'/lib/slice::ctors';$l7={};$m7=q'exists';$n7=[];$o7=q'exists $_[0]->{named}{$_[1]}';$p7=bless({$V1,$n7,$v1,$o7},$S);$q7=q'quoted';$r7=[];$s7=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$t7=bless({$V1,$r7,$v1,$s7},$S);$u7={$m7,$p7,$q7,$t7};$v7=q'/lib/ni_image.b';$w7=bless({$c,$l7,$j1,$k1,$l1,$k1,$m1,$u7,$f,$v7},$K);$x7=q'/lib/slice::ctors';$y7={};$z7=q'--internal/+=';$A7=[];$B7=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$C7=bless({$V1,$A7,$v1,$B7},$S);$D7=q'--internal/eval';$E7=[];$F7=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$G7=bless({$V1,$E7,$v1,$F7},$S);$H7=q'--internal/image';$I7=[];$J7=q'shift->quoted->write(\\*STDOUT);
0;';$K7=bless({$V1,$I7,$v1,$J7},$S);$L7=q'--internal/test';$M7=[];$N7=q'my $self = shift;
my @tests = map ni($_)->tests, grep /^ni\\.doc:/, keys %{$$self{named}};
my $fails = 0;
print STDERR scalar(@tests) . " test(s)\\n";
for my $t (@tests) {
  &$t or $fails += print STDERR "FAIL: $t->{code}\\n";
}
my $passed = @tests - $fails;
print STDERR "$passed test(s) passed\\n";
!!$fails;';$O7=bless({$V1,$M7,$v1,$N7},$S);$P7=q'run';$Q7=[];$R7=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$S7=bless({$V1,$Q7,$v1,$R7},$S);$T7={$z7,$C7,$D7,$G7,$H7,$K7,$L7,$O7,$P7,$S7};$U7=q'/lib/ni_main.b';$V7=bless({$c,$y7,$j1,$k1,$l1,$k1,$m1,$T7,$f,$U7},$K);$W7=q'/lib/slice::ctors';$X7={};$Y7=[];$Z7=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$c8=bless({$V1,$Y7,$v1,$Z7},$S);$d8=q'resolver_for';$e8=[];$f8=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$g8=bless({$V1,$e8,$v1,$f8},$S);$h8={$l5,$c8,$d8,$g8};$i8=q'/lib/ni_resolver.b';$j8=bless({$c,$X7,$j1,$k1,$l1,$k1,$m1,$h8,$f,$i8},$K);$k8=q'/lib/slice::ctors';$l8={};$m8=q'fork';$n8=[];$o8=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$p8=bless({$V1,$n8,$v1,$o8},$S);$q8=q'fork_exec';$r8=[];$s8=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$t8=bless({$V1,$r8,$v1,$s8},$S);$u8={$m8,$p8,$q8,$t8};$v8=q'/lib/ni_pid_ctors';$w8=bless({$c,$l8,$j1,$k1,$l1,$k1,$m1,$u8,$f,$v8},$K);$x8=q'/lib/slice::ctors';$y8=[$T2,$j7,$w7,$V7,$j8,$w8];$z8=bless({$c,$V6,$f,$U,$g,$y8},$o);$A8=q'/lib/ni.c::ctors';$B8=q'named';$C8=q'ni.doc:/class';$D8={$l,1};$E8=[$Z3];$F8=bless({$c,$D8,$f,$l,$g,$E8},$E);$G8=q'/metaclass::ctors';$H8={$R,1};$I8={};$J8=q'shift; +{name => shift, doc => []}';$K8=bless({$v1,$J8},$S);$L8={$N1,$K8};$M8=q'/lib/doc_init.b';$N8=bless({$c,$I8,$j1,$k1,$l1,$k1,$m1,$L8,$f,$M8},$K);$O8=q'/lib/slice::ctors';$P8={};$Q8=q'\'ni.doc\'';$R8=bless({$v1,$Q8},$S);$S8={$R4,$R8};$T8=q'/lib/doc_namespace.b';$U8=bless({$c,$P8,$j1,$k1,$l1,$k1,$m1,$S8,$f,$T8},$K);$V8=q'/lib/slice::ctors';$W8={};$X8=q'AUTOLOAD';$Y8=q'my $self = shift;
my $method = ${__PACKAGE__ . "::AUTOLOAD"};
push @{$$self{doc}}, [$method, @_];
$self;';$Z8=bless({$v1,$Y8},$S);$c9={$X8,$Z8};$d9=q'/lib/doc_define.b';$e9=bless({$c,$W8,$j1,$k1,$l1,$k1,$m1,$c9,$f,$d9},$K);$f9=q'/lib/slice::ctors';$g9={};$h9=q'eg';$i9=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$j9=bless({$v1,$i9},$S);$k9=q'tests';$l9=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$m9=bless({$v1,$l9},$S);$n9={$h9,$j9,$k9,$m9};$o9=q'/lib/doc_test.b';$p9=bless({$c,$g9,$j1,$k1,$l1,$k1,$m1,$n9,$f,$o9},$K);$q9=q'/lib/slice::ctors';$r9=[$T2,$h3,$N8,$U8,$e9,$p9];$s9=bless({$c,$H8,$f,$R,$g,$r9},$l);$t9=q'/lib/doc.c::ctors';$u9=[];$v9=bless({$Q3,$u9,$f,$Q},$R);$w9=q'/lib/doc::ctors';$x9=q'ni.doc:/lib/doc';$y9=q'/lib/doc::synopsis';$z9=q'
    ni("ni:/some/class")->doc
      ->name(...)
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$A9=[$y9,$z9];$B9=q'/lib/doc::description';$C9=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$D9=q'Documentation objects are internally represented as arrays of quoted
      method calls; for example:';$E9=q'perl';$F9=q'
      # state is []
      $doc->foo("bar bif baz");
      # state is now [["foo", ["bar bif baz"]]]
    ';$G9=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions.';$H9=q'Documentation also stores unit tests, which are specified using "eg";
      e.g.:';$I9=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$J9=bless({$v1,$I9},$S);$K9=[$B9,$C9,$D9,$E9,$F9,$G9,$H9,$h9,$J9];$L9=[$A9,$K9];$M9=bless({$Q3,$L9,$f,$R},$R);$N9=q'ni:/class';$O9=q'ni:/class.c';$P9=q'ni:/lib/accessor.b';$Q9=q'ni:/lib/behavior';$R9=q'ni:/lib/behavior.c';$S9=q'ni:/lib/branch';$T9=q'ni:/lib/branch.b';$U9=q'ni:/lib/branch.c';$V9=q'ni:/lib/branch_init.b';$W9=q'ni:/lib/class_init.b';$X9=q'ni:/lib/classdef.b';$Y9=q'ni:/lib/definition.b';$Z9=q'ni:/lib/doc';$ca=q'ni:/lib/doc.c';$da=q'ni:/lib/doc_define.b';$ea=q'ni:/lib/doc_init.b';$fa=q'ni:/lib/doc_namespace.b';$ga=q'ni:/lib/doc_test.b';$ha=q'ni:/lib/documentable.b';$ia=q'ni:/lib/fn';$ja=q'ni:/lib/fn.c';$ka=q'ni:/lib/fn_init.b';$la=q'ni:/lib/fn_ops.b';$ma=q'ni:/lib/fn_ro.b';$na=q'ni:/lib/fn_serialize.b';$oa=q'ni:/lib/image';$pa={$n,1};$qa=[$Z3];$ra=bless({$c,$pa,$f,$n,$g,$qa},$E);$sa=q'/metaclass::ctors';$ta={$T,1};$ua={};$va=[];$wa=q'my $class = shift;
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
  ordering     => []};';$xa=bless({$V1,$va,$v1,$wa},$S);$ya={$N1,$xa};$za=q'/lib/image_init.b';$Aa=bless({$c,$ua,$j1,$k1,$l1,$k1,$m1,$ya,$f,$za},$K);$Ba=q'/lib/slice::ctors';$Ca={};$Da=q'address';$Ea=[];$Fa=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$Ga=bless({$V1,$Ea,$v1,$Fa},$S);$Ha=q'allocate_gensym';$Ia=[];$Ja=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Ka=bless({$V1,$Ia,$v1,$Ja},$S);$La=q'boot_side_effect';$Ma=[];$Na=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Oa=bless({$V1,$Ma,$v1,$Na},$S);$Pa=q'circular_links';$Qa=[];$Ra=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$Sa=bless({$V1,$Qa,$v1,$Ra},$S);$Ta=q'finalizer';$Ua=[];$Va=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Wa=bless({$V1,$Ua,$v1,$Va},$S);$Xa=q'gensym';$Ya=[];$Za=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$cb=bless({$V1,$Ya,$v1,$Za},$S);$db=q'is_circular';$eb=[];$fb=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$gb=bless({$V1,$eb,$v1,$fb},$S);$hb=q'quote';$ib=[];$jb=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$kb=bless({$V1,$ib,$v1,$jb},$S);$lb=q'quote_array';$mb=[];$nb=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$ob=bless({$V1,$mb,$v1,$nb},$S);$pb=q'quote_blessed';$qb=[];$rb=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$sb=bless({$V1,$qb,$v1,$rb},$S);$tb=q'quote_class';$ub=[];$vb=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$wb=bless({$V1,$ub,$v1,$vb},$S);$xb=q'quote_hash';$yb=[];$zb=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$Ab=bless({$V1,$yb,$v1,$zb},$S);$Bb=q'quote_object';$Cb=[];$Db=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$Eb=bless({$V1,$Cb,$v1,$Db},$S);$Fb=q'quote_scalar';$Gb=[];$Hb=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Ib=bless({$V1,$Gb,$v1,$Hb},$S);$Jb=q'quote_value';$Kb=[];$Lb=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Mb=bless({$V1,$Kb,$v1,$Lb},$S);$Nb=q'reconstruction';$Ob=[];$Pb=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Qb=bless({$V1,$Ob,$v1,$Pb},$S);$Rb=q'side_effect';$Sb=[];$Tb=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Ub=bless({$V1,$Sb,$v1,$Tb},$S);$Vb=q'write';$Wb=[];$Xb=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Yb=bless({$V1,$Wb,$v1,$Xb},$S);$Zb={$Da,$Ga,$Ha,$Ka,$La,$Oa,$Pa,$Sa,$Ta,$Wa,$Xa,$cb,$db,$gb,$hb,$kb,$lb,$ob,$pb,$sb,$tb,$wb,$xb,$Ab,$Bb,$Eb,$Fb,$Ib,$Jb,$Mb,$Nb,$Qb,$Rb,$Ub,$Vb,$Yb};$cc=q'/lib/image_quoting.b';$dc=bless({$c,$Ca,$j1,$k1,$l1,$k1,$m1,$Zb,$f,$cc},$K);$ec=q'/lib/slice::ctors';$fc=[$T2,$Aa,$dc];$gc=bless({$c,$ta,$f,$T,$g,$fc},$n);$hc=q'/lib/image.c::ctors';$ic=q'ni:/lib/image.c';$jc=q'ni:/lib/image_init.b';$kc=q'ni:/lib/image_quoting.b';$lc=q'ni:/lib/instance.b';$mc=q'ni:/lib/instantiable.b';$nc=q'ni:/lib/named.b';$oc=q'ni:/lib/named_in_ni.b';$pc=q'ni:/lib/namespaced.b';$qc=q'ni:/lib/ni';$rc=q'ni:/lib/ni.c';$sc=q'ni:/lib/ni_image.b';$tc=q'ni:/lib/ni_main.b';$uc=q'ni:/lib/ni_pid_ctors';$vc=q'ni:/lib/ni_resolver.b';$wc=q'ni:/lib/ni_self.b';$xc=q'ni:/lib/ni_static';$yc=q'/lib/ni_static';$zc=q'ni';$Ac={$yc,1,$zc,1};$Bc={};$Cc=q'abbrev';$Dc=[];$Ec=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$Fc=bless({$V1,$Dc,$v1,$Ec},$S);$Gc=q'dor';$Hc=[];$Ic=q'defined $_[0] ? $_[0] : $_[1]';$Jc=bless({$V1,$Hc,$v1,$Ic},$S);$Kc=q'indent';$Lc=[];$Mc=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$Nc=bless({$V1,$Lc,$v1,$Mc},$S);$Oc=q'max';$Pc=[];$Qc=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$Rc=bless({$V1,$Pc,$v1,$Qc},$S);$Sc=q'maxstr';$Tc=[];$Uc=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Vc=bless({$V1,$Tc,$v1,$Uc},$S);$Wc=q'mean';$Xc=[];$Yc=q'sum(@_) / (@_ || 1)';$Zc=bless({$V1,$Xc,$v1,$Yc},$S);$cd=q'min';$dd=[];$ed=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$fd=bless({$V1,$dd,$v1,$ed},$S);$gd=q'minstr';$hd=[];$id=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$jd=bless({$V1,$hd,$v1,$id},$S);$kd=q'sgr';$ld=[];$md=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$nd=bless({$V1,$ld,$v1,$md},$S);$od=q'sr';$pd=[];$qd=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$rd=bless({$V1,$pd,$v1,$qd},$S);$sd=q'sum';$td=[];$ud=q'local $_; my $x = 0; $x += $_ for @_; $x';$vd=bless({$V1,$td,$v1,$ud},$S);$wd=q'swap';$xd=[];$yd=q'@_[0, 1] = @_[1, 0]';$zd=bless({$V1,$xd,$v1,$yd},$S);$Ad={$Cc,$Fc,$Gc,$Jc,$Kc,$Nc,$Oc,$Rc,$Sc,$Vc,$Wc,$Zc,$cd,$fd,$gd,$jd,$kd,$nd,$od,$rd,$sd,$vd,$wd,$zd};$Bd=q'/lib/ni_static_util.b';$Cd=bless({$c,$Bc,$j1,$k1,$l1,$k1,$m1,$Ad,$f,$Bd},$K);$Dd=q'/lib/slice::ctors';$Ed=[$Cd];$Fd=bless({$c,$Ac,$f,$yc,$g,$Ed},$Q);$Gd=q'/class::ctors';$Hd=q'ni:/lib/ni_static_util.b';$Id=q'ni:/lib/perlbranch.b';$Jd=q'ni:/lib/resolver.b';$Kd=q'ni:/lib/slice';$Ld=q'ni:/lib/slice.b';$Md=q'ni:/lib/slice.c';$Nd=q'ni:/lib/slice_init.b';$Od=q'ni:/lib/slice_serialize.b';$Pd=q'ni:/lib/subclass.b';$Qd=q'ni:/lib/tag';$Rd=q'ni:/lib/tag.b';$Sd=q'ni:/lib/tag.c';$Td=q'ni:/lib/tag_init.b';$Ud=q'ni:/metaclass';$Vd=q'ni:/metaclass.c';$Wd=q'ni:/object';$Xd=q'ni:/object.c';$Yd=q'ni:/unix/cat';$Zd={$s,1};$ce=q'/unix/pipeline.c';$de={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$ce,1,$z,1};$ee=[$Z3];$fe=bless({$c,$de,$f,$w,$g,$ee},$E);$ge=q'/metaclass::ctors';$he=[$fe];$ie=bless({$c,$Zd,$f,$s,$g,$he},$E);$je=q'/metaclass::ctors';$ke={$W,1};$le={$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1,$e1,1,$g1,1};$me={};$ne=q'into';$oe=[];$pe=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$qe=bless({$V1,$oe,$v1,$pe},$S);$re={$ne,$qe};$se=q'/unix/io_stream.b';$te=bless({$c,$me,$j1,$k1,$l1,$k1,$m1,$re,$f,$se},$K);$ue=q'/lib/slice::ctors';$ve={};$we=q'(+';$xe=[];$ye=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$ze=bless({$V1,$xe,$v1,$ye},$S);$Ae={$we,$ze};$Be=q'/unix/io_constructors.b';$Ce=bless({$c,$ve,$j1,$k1,$l1,$k1,$m1,$Ae,$f,$Be},$K);$De=q'/lib/slice::ctors';$Ee={};$Fe=q'(<>';$Ge=[];$He=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Ie=bless({$V1,$Ge,$v1,$He},$S);$Je=q'(@{}';$Ke=[];$Le=q'my $self = shift; [<$self>]';$Me=bless({$V1,$Ke,$v1,$Le},$S);$Ne={$Fe,$Ie,$Je,$Me};$Oe=q'/unix/io_readers.b';$Pe=bless({$c,$Ee,$j1,$k1,$l1,$k1,$m1,$Ne,$f,$Oe},$K);$Qe=q'/lib/slice::ctors';$Re=[$T2,$te,$Ce,$Pe];$Se=bless({$c,$le,$f,$c1,$g,$Re},$w);$Te=q'/unix/io.c::ctors';$Ue={};$Ve=[];$We=q'shift; +{fs => [@_]}';$Xe=bless({$V1,$Ve,$v1,$We},$S);$Ye={$N1,$Xe};$Ze=q'/unix/cat_init.b';$cf=bless({$c,$Ue,$j1,$k1,$l1,$k1,$m1,$Ye,$f,$Ze},$K);$df=q'/lib/slice::ctors';$ef={};$ff=q'read';$gf=[];$hf=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$if=bless({$V1,$gf,$v1,$hf},$S);$jf={$ff,$if};$kf=q'/unix/cat_read.b';$lf=bless({$c,$ef,$j1,$k1,$l1,$k1,$m1,$jf,$f,$kf},$K);$mf=q'/lib/slice::ctors';$nf=[$Se,$cf,$lf];$of=bless({$c,$ke,$f,$W,$g,$nf},$s);$pf=q'/unix/cat.c::ctors';$qf=q'ni:/unix/cat.c';$rf=q'ni:/unix/cat_init.b';$sf=q'ni:/unix/cat_read.b';$tf=q'ni:/unix/fd';$uf={$t,1};$vf=[$fe];$wf=bless({$c,$uf,$f,$t,$g,$vf},$E);$xf=q'/metaclass::ctors';$yf={$X,1};$zf={};$Af=q'fd';$Bf=[];$Cf=q'shift->{\'fd\'}';$Df=bless({$V1,$Bf,$v1,$Cf},$S);$Ef={$Af,$Df};$Ff=q'/unix/fd_readers.b';$Gf=bless({$c,$zf,$j1,$k1,$l1,$k1,$m1,$Ef,$f,$Ff},$K);$Hf=q'/lib/slice::ctors';$If={};$Jf=[];$Kf=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$Lf=bless({$V1,$Jf,$v1,$Kf},$S);$Mf={$N1,$Lf};$Nf=q'/unix/fd_init.b';$Of=bless({$c,$If,$j1,$k1,$l1,$k1,$m1,$Mf,$f,$Nf},$K);$Pf=q'/lib/slice::ctors';$Qf={};$Rf=q'move_to';$Sf=[];$Tf=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Uf=bless({$V1,$Sf,$v1,$Tf},$S);$Vf={$Rf,$Uf};$Wf=q'/unix/fd_shell.b';$Xf=bless({$c,$Qf,$j1,$k1,$l1,$k1,$m1,$Vf,$f,$Wf},$K);$Yf=q'/lib/slice::ctors';$Zf={$X,1,$Y,1,$Z,1,$d1,1,$e1,1};$cg=q'/unix/has_fd.b';$dg={};$eg=[];$fg=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$gg=bless({$V1,$eg,$v1,$fg},$S);$hg=[];$ig=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$jg=bless({$V1,$hg,$v1,$ig},$S);$kg={$ff,$gg,$Vb,$jg};$lg=q'/unix/fd_safeio.b';$mg=bless({$c,$dg,$j1,$k1,$l1,$k1,$m1,$kg,$f,$lg},$K);$ng=q'/lib/slice::ctors';$og=[$mg];$pg=bless({$c,$Zf,$f,$cg,$g,$og},$N);$qg=q'/lib/branch::ctors';$rg={};$sg=q'read_fh';$tg=[];$ug=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$vg=bless({$V1,$tg,$v1,$ug},$S);$wg=q'write_fh';$xg=[];$yg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$zg=bless({$V1,$xg,$v1,$yg},$S);$Ag={$sg,$vg,$wg,$zg};$Bg=q'/unix/fd_io.b';$Cg=bless({$c,$rg,$j1,$k1,$l1,$k1,$m1,$Ag,$f,$Bg},$K);$Dg=q'/lib/slice::ctors';$Eg=[$Se,$Gf,$Of,$Xf,$pg,$Cg];$Fg=bless({$c,$yf,$f,$X,$g,$Eg},$t);$Gg=q'/unix/fd.c::ctors';$Hg=q'ni:/unix/fd.c';$Ig=q'ni:/unix/fd_init.b';$Jg=q'ni:/unix/fd_io.b';$Kg=q'ni:/unix/fd_readers.b';$Lg=q'ni:/unix/fd_safeio.b';$Mg=q'ni:/unix/fd_shell.b';$Ng=q'ni:/unix/fifo';$Og={$u,1};$Pg=[$fe];$Qg=bless({$c,$Og,$f,$u,$g,$Pg},$E);$Rg=q'/metaclass::ctors';$Sg={$Y,1};$Tg={};$Ug=[];$Vg=q'shift->{\'read_fh\'}';$Wg=bless({$V1,$Ug,$v1,$Vg},$S);$Xg=[];$Yg=q'shift->{\'write_fh\'}';$Zg=bless({$V1,$Xg,$v1,$Yg},$S);$ch={$sg,$Wg,$wg,$Zg};$dh=q'/unix/fifo_io.b';$eh=bless({$c,$Tg,$j1,$k1,$l1,$k1,$m1,$ch,$f,$dh},$K);$fh=q'/lib/slice::ctors';$gh={};$hh=[];$ih=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$jh=bless({$V1,$hh,$v1,$ih},$S);$kh={$N1,$jh};$lh=q'/unix/fifo_init.b';$mh=bless({$c,$gh,$j1,$k1,$l1,$k1,$m1,$kh,$f,$lh},$K);$nh=q'/lib/slice::ctors';$oh={};$ph=q'read_side';$qh=[];$rh=q'my $self = shift; close $$self{write_fh}; $self';$sh=bless({$V1,$qh,$v1,$rh},$S);$th=q'write_side';$uh=[];$vh=q'my $self = shift; close $$self{read_fh};  $self';$wh=bless({$V1,$uh,$v1,$vh},$S);$xh={$ph,$sh,$th,$wh};$yh=q'/unix/fifo_direction.b';$zh=bless({$c,$oh,$j1,$k1,$l1,$k1,$m1,$xh,$f,$yh},$K);$Ah=q'/lib/slice::ctors';$Bh=[$Se,$eh,$mh,$pg,$zh];$Ch=bless({$c,$Sg,$f,$Y,$g,$Bh},$u);$Dh=q'/unix/fifo.c::ctors';$Eh=q'ni:/unix/fifo.c';$Fh=q'ni:/unix/fifo_direction.b';$Gh=q'ni:/unix/fifo_init.b';$Hh=q'ni:/unix/fifo_io.b';$Ih=q'ni:/unix/file';$Jh={$v,1};$Kh=[$fe];$Lh=bless({$c,$Jh,$f,$v,$g,$Kh},$E);$Mh=q'/metaclass::ctors';$Nh={$Z,1};$Oh={};$Ph=[];$Qh=q'shift->{\'name\'}';$Rh=bless({$V1,$Ph,$v1,$Qh},$S);$Sh={$f,$Rh};$Th=q'/unix/file_readers.b';$Uh=bless({$c,$Oh,$j1,$k1,$l1,$k1,$m1,$Sh,$f,$Th},$K);$Vh=q'/lib/slice::ctors';$Wh={};$Xh=[];$Yh=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Zh=bless({$V1,$Xh,$v1,$Yh},$S);$ci={$N1,$Zh};$di=q'/unix/file_init.b';$ei=bless({$c,$Wh,$j1,$k1,$l1,$k1,$m1,$ci,$f,$di},$K);$fi=q'/lib/slice::ctors';$gi={};$hi=[];$ii=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$ji=bless({$V1,$hi,$v1,$ii},$S);$ki=[];$li=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$mi=bless({$V1,$ki,$v1,$li},$S);$ni={$sg,$ji,$wg,$mi};$oi=q'/unix/file_io.b';$pi=bless({$c,$gi,$j1,$k1,$l1,$k1,$m1,$ni,$f,$oi},$K);$qi=q'/lib/slice::ctors';$ri=[$Se,$Uh,$ei,$pg,$pi];$si=bless({$c,$Nh,$f,$Z,$g,$ri},$v);$ti=q'/unix/file.c::ctors';$ui=q'ni:/unix/file.c';$vi=q'ni:/unix/file_init.b';$wi=q'ni:/unix/file_io.b';$xi=q'ni:/unix/file_readers.b';$yi=q'ni:/unix/has_fd.b';$zi=q'ni:/unix/io';$Ai=q'ni:/unix/io.c';$Bi=q'ni:/unix/io_constructors.b';$Ci=q'ni:/unix/io_readers.b';$Di=q'ni:/unix/io_stream.b';$Ei=q'ni:/unix/pid';$Fi={$x,1};$Gi=[$fe];$Hi=bless({$c,$Fi,$f,$x,$g,$Gi},$E);$Ii=q'/metaclass::ctors';$Ji={$d1,1};$Ki={};$Li=q'pid';$Mi=[];$Ni=q'shift->{\'pid\'}';$Oi=bless({$V1,$Mi,$v1,$Ni},$S);$Pi=q'stderr';$Qi=[];$Ri=q'shift->{\'stderr\'}';$Si=bless({$V1,$Qi,$v1,$Ri},$S);$Ti=q'stdin';$Ui=[];$Vi=q'shift->{\'stdin\'}';$Wi=bless({$V1,$Ui,$v1,$Vi},$S);$Xi=q'stdout';$Yi=[];$Zi=q'shift->{\'stdout\'}';$cj=bless({$V1,$Yi,$v1,$Zi},$S);$dj={$Li,$Oi,$Pi,$Si,$Ti,$Wi,$Xi,$cj};$ej=q'/unix/pid_readers.b';$fj=bless({$c,$Ki,$j1,$k1,$l1,$k1,$m1,$dj,$f,$ej},$K);$gj=q'/lib/slice::ctors';$hj={};$ij=[];$jj=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$kj=bless({$V1,$ij,$v1,$jj},$S);$lj={$N1,$kj};$mj=q'/unix/pid_init.b';$nj=bless({$c,$hj,$j1,$k1,$l1,$k1,$m1,$lj,$f,$mj},$K);$oj=q'/lib/slice::ctors';$pj={};$qj={};$rj=q'/unix/pid_wait.b';$sj=bless({$c,$pj,$j1,$k1,$l1,$k1,$m1,$qj,$f,$rj},$K);$tj=q'/lib/slice::ctors';$uj={};$vj=[];$wj=q'shift->{stdout}->read_fh';$xj=bless({$V1,$vj,$v1,$wj},$S);$yj=[];$zj=q'shift->{stdin}->write_fh';$Aj=bless({$V1,$yj,$v1,$zj},$S);$Bj={$sg,$xj,$wg,$Aj};$Cj=q'/unix/pid_io.b';$Dj=bless({$c,$uj,$j1,$k1,$l1,$k1,$m1,$Bj,$f,$Cj},$K);$Ej=q'/lib/slice::ctors';$Fj=[$Se,$fj,$nj,$sj,$pg,$Dj];$Gj=bless({$c,$Ji,$f,$d1,$g,$Fj},$x);$Hj=q'/unix/pid.c::ctors';$Ij=q'ni:/unix/pid.c';$Jj=q'ni:/unix/pid_init.b';$Kj=q'ni:/unix/pid_io.b';$Lj=q'ni:/unix/pid_readers.b';$Mj=q'ni:/unix/pid_wait.b';$Nj=q'ni:/unix/pipeline';$Oj=q'/unix/pipeline.c';$Pj={$Oj,1};$Qj=q'/unix/pipeline.c';$Rj=[$fe];$Sj=bless({$c,$Pj,$f,$Qj,$g,$Rj},$E);$Tj=q'/metaclass::ctors';$Uj={$e1,1};$Vj={};$Wj=[];$Xj=q'shift->{\'stdin\'}';$Yj=bless({$V1,$Wj,$v1,$Xj},$S);$Zj=[];$ck=q'shift->{\'stdout\'}';$dk=bless({$V1,$Zj,$v1,$ck},$S);$ek={$Ti,$Yj,$Xi,$dk};$fk=q'/unix/pipeline_ro.b';$gk=bless({$c,$Vj,$j1,$k1,$l1,$k1,$m1,$ek,$f,$fk},$K);$hk=q'/lib/slice::ctors';$ik={};$jk=[];$kk=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$lk=bless({$V1,$jk,$v1,$kk},$S);$mk={$N1,$lk};$nk=q'/unix/pipeline_init.b';$ok=bless({$c,$ik,$j1,$k1,$l1,$k1,$m1,$mk,$f,$nk},$K);$pk=q'/lib/slice::ctors';$qk={};$rk=q'async_step';$sk=[];$tk=q'local $_;
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
$self;';$uk=bless({$V1,$sk,$v1,$tk},$S);$vk={$rk,$uk};$wk=q'/unix/pipeline_async.b';$xk=bless({$c,$qk,$j1,$k1,$l1,$k1,$m1,$vk,$f,$wk},$K);$yk=q'/lib/slice::ctors';$zk={};$Ak=[];$Bk=q'shift->{stdout}->read_fh';$Ck=bless({$V1,$Ak,$v1,$Bk},$S);$Dk=[];$Ek=q'shift->{stdin}->write_fh';$Fk=bless({$V1,$Dk,$v1,$Ek},$S);$Gk={$sg,$Ck,$wg,$Fk};$Hk=q'/unix/pipeline_io.b';$Ik=bless({$c,$zk,$j1,$k1,$l1,$k1,$m1,$Gk,$f,$Hk},$K);$Jk=q'/lib/slice::ctors';$Kk=[$Se,$gk,$ok,$xk,$pg,$Ik];$Lk=q'/unix/pipeline.c';$Mk=bless({$c,$Uj,$f,$e1,$g,$Kk},$Lk);$Nk=q'/unix/pipeline.c::ctors';$Ok=q'ni:/unix/pipeline.c';$Pk=q'ni:/unix/pipeline_async.b';$Qk=q'ni:/unix/pipeline_init.b';$Rk=q'ni:/unix/pipeline_io.b';$Sk=q'ni:/unix/pipeline_ro.b';$Tk=q'ni:/unix/str';$Uk={$z,1};$Vk=[$fe];$Wk=bless({$c,$Uk,$f,$z,$g,$Vk},$E);$Xk=q'/metaclass::ctors';$Yk={$g1,1};$Zk={};$cl=[];$dl=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$el=bless({$V1,$cl,$v1,$dl},$S);$fl={$N1,$el};$gl=q'/unix/str_init.b';$hl=bless({$c,$Zk,$j1,$k1,$l1,$k1,$m1,$fl,$f,$gl},$K);$il=q'/lib/slice::ctors';$jl={};$kl=[];$ll=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$ml=bless({$V1,$kl,$v1,$ll},$S);$nl=q'remaining';$ol=[];$pl=q'my $self = shift; $$self{end} - $$self{start}';$ql=bless({$V1,$ol,$v1,$pl},$S);$rl=[];$sl=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$tl=bless({$V1,$rl,$v1,$sl},$S);$ul={$ff,$ml,$nl,$ql,$Vb,$tl};$vl=q'/unix/str_io.b';$wl=bless({$c,$jl,$j1,$k1,$l1,$k1,$m1,$ul,$f,$vl},$K);$xl=q'/lib/slice::ctors';$yl=[$Se,$hl,$wl];$zl=bless({$c,$Yk,$f,$g1,$g,$yl},$z);$Al=q'/unix/str.c::ctors';$Bl=q'ni:/unix/str.c';$Cl=q'ni:/unix/str_init.b';$Dl=q'ni:/unix/str_io.b';$El={$C8,$v9,$x9,$M9,$N9,$I6,$O9,$e4,$P9,$s6,$Q9,$W2,$R9,$F,$S9,$T5,$T9,$N4,$U9,$I5,$V9,$Q5,$W9,$D5,$X9,$g6,$Y9,$w6,$Z9,$s9,$ca,$F8,$da,$e9,$ea,$N8,$fa,$U8,$ga,$p9,$ha,$V3,$ia,$J2,$ja,$q1,$ka,$S1,$la,$x2,$ma,$k2,$na,$G2,$oa,$gc,$ic,$ra,$jc,$Aa,$kc,$dc,$lc,$P2,$mc,$E1,$nc,$h3,$oc,$W4,$pc,$h5,$qc,$z8,$rc,$T6,$sc,$w7,$tc,$V7,$uc,$w8,$vc,$j8,$wc,$j7,$xc,$Fd,$Hd,$Cd,$Id,$u5,$Jd,$q5,$Kd,$N3,$Ld,$v3,$Md,$I,$Nd,$C3,$Od,$K3,$Pd,$E6,$Qd,$C4,$Rd,$s4,$Sd,$k4,$Td,$z4,$Ud,$P6,$Vd,$L6,$Wd,$T2,$Xd,$Z3,$Yd,$of,$qf,$ie,$rf,$cf,$sf,$lf,$tf,$Fg,$Hg,$wf,$Ig,$Of,$Jg,$Cg,$Kg,$Gf,$Lg,$mg,$Mg,$Xf,$Ng,$Ch,$Eh,$Qg,$Fh,$zh,$Gh,$mh,$Hh,$eh,$Ih,$si,$ui,$Lh,$vi,$ei,$wi,$pi,$xi,$Uh,$yi,$pg,$zi,$Se,$Ai,$fe,$Bi,$Ce,$Ci,$Pe,$Di,$te,$Ei,$Gj,$Ij,$Hi,$Jj,$nj,$Kj,$Dj,$Lj,$fj,$Mj,$sj,$Nj,$Mk,$Ok,$Sj,$Pk,$xk,$Qk,$ok,$Rk,$Ik,$Sk,$gk,$Tk,$zl,$Bl,$Wk,$Cl,$hl,$Dl,$wl};$Fl=q'resolvers';$Gl=[];$Hl=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$Il=bless({$V1,$Gl,$v1,$Hl},$S);$Jl=q'file';$Kl=[];$Ll=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$Ml=bless({$V1,$Kl,$v1,$Ll},$S);$Nl=q'str';$Ol=[];$Pl=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$Ql=bless({$V1,$Ol,$v1,$Pl},$S);$Rl={$Af,$Il,$Jl,$Ml,$Nl,$Ql};$Sl=bless({$B8,$El,$Fl,$Rl},$U);$Tl=q'/lib/ni::ctors';$$Y3[0]=$I6;$$D[0]=$Z3;$$p1[0]=$Z3;$$I2[0]=$T2;$$S5[4]=$w6;*$p3=\&$n3;*$o3=\&$l3;$E1->apply_unsafe($Q);$E1->apply_unsafe($h);$E1->apply_unsafe($j);$E1->apply_unsafe($N);$E1->apply_unsafe($k);$E1->apply_unsafe($l);$E1->apply_unsafe($S);$E1->apply_unsafe($m);$E1->apply_unsafe($n);$E1->apply_unsafe($o);$E1->apply_unsafe($K);$E1->apply_unsafe($p);$E1->apply_unsafe($O);$E1->apply_unsafe($q);$E1->apply_unsafe($E);$E1->apply_unsafe($d);$E1->apply_unsafe($r);$E1->apply_unsafe($s);$E1->apply_unsafe($t);$E1->apply_unsafe($u);$E1->apply_unsafe($v);$E1->apply_unsafe($w);$E1->apply_unsafe($x);$E1->apply_unsafe($F1);$E1->apply_unsafe($z);$S1->apply_unsafe($S);$k2->apply_unsafe($S);$x2->apply_unsafe($S);$G2->apply_unsafe($S);$P2->apply_unsafe($Q);$P2->apply_unsafe($h);$P2->apply_unsafe($M);$P2->apply_unsafe($j);$P2->apply_unsafe($N);$P2->apply_unsafe($k);$P2->apply_unsafe($R);$P2->apply_unsafe($l);$P2->apply_unsafe($S);$P2->apply_unsafe($m);$P2->apply_unsafe($T);$P2->apply_unsafe($n);$P2->apply_unsafe($U);$P2->apply_unsafe($o);$P2->apply_unsafe($K);$P2->apply_unsafe($p);$P2->apply_unsafe($O);$P2->apply_unsafe($q);$P2->apply_unsafe($E);$P2->apply_unsafe($d);$P2->apply_unsafe($V);$P2->apply_unsafe($r);$P2->apply_unsafe($W);$P2->apply_unsafe($s);$P2->apply_unsafe($X);$P2->apply_unsafe($t);$P2->apply_unsafe($Y);$P2->apply_unsafe($u);$P2->apply_unsafe($Z);$P2->apply_unsafe($v);$P2->apply_unsafe($c1);$P2->apply_unsafe($w);$P2->apply_unsafe($d1);$P2->apply_unsafe($x);$P2->apply_unsafe($e1);$P2->apply_unsafe($Q2);$P2->apply_unsafe($g1);$P2->apply_unsafe($z);$h3->apply_unsafe($Q);$h3->apply_unsafe($h);$h3->apply_unsafe($j);$h3->apply_unsafe($N);$h3->apply_unsafe($k);$h3->apply_unsafe($R);$h3->apply_unsafe($l);$h3->apply_unsafe($m);$h3->apply_unsafe($n);$h3->apply_unsafe($o);$h3->apply_unsafe($K);$h3->apply_unsafe($p);$h3->apply_unsafe($O);$h3->apply_unsafe($q);$h3->apply_unsafe($E);$h3->apply_unsafe($d);$h3->apply_unsafe($r);$h3->apply_unsafe($s);$h3->apply_unsafe($t);$h3->apply_unsafe($u);$h3->apply_unsafe($v);$h3->apply_unsafe($w);$h3->apply_unsafe($x);$h3->apply_unsafe($i3);$h3->apply_unsafe($z);$v3->apply_unsafe($K);$C3->apply_unsafe($K);$K3->apply_unsafe($K);$V3->apply_unsafe($h);$V3->apply_unsafe($j);$V3->apply_unsafe($k);$V3->apply_unsafe($l);$V3->apply_unsafe($m);$V3->apply_unsafe($n);$V3->apply_unsafe($o);$V3->apply_unsafe($p);$V3->apply_unsafe($q);$V3->apply_unsafe($r);$V3->apply_unsafe($s);$V3->apply_unsafe($t);$V3->apply_unsafe($u);$V3->apply_unsafe($v);$V3->apply_unsafe($w);$V3->apply_unsafe($x);$V3->apply_unsafe($W3);$V3->apply_unsafe($z);$s4->apply_unsafe($O);$z4->apply_unsafe($O);$N4->apply_unsafe($Q);$N4->apply_unsafe($h);$N4->apply_unsafe($j);$N4->apply_unsafe($N);$N4->apply_unsafe($k);$N4->apply_unsafe($l);$N4->apply_unsafe($m);$N4->apply_unsafe($n);$N4->apply_unsafe($o);$N4->apply_unsafe($p);$N4->apply_unsafe($q);$N4->apply_unsafe($E);$N4->apply_unsafe($d);$N4->apply_unsafe($r);$N4->apply_unsafe($s);$N4->apply_unsafe($t);$N4->apply_unsafe($u);$N4->apply_unsafe($v);$N4->apply_unsafe($w);$N4->apply_unsafe($x);$N4->apply_unsafe($O4);$N4->apply_unsafe($z);$W4->apply_unsafe($Q);$W4->apply_unsafe($h);$W4->apply_unsafe($j);$W4->apply_unsafe($N);$W4->apply_unsafe($k);$W4->apply_unsafe($l);$W4->apply_unsafe($m);$W4->apply_unsafe($n);$W4->apply_unsafe($o);$W4->apply_unsafe($K);$W4->apply_unsafe($p);$W4->apply_unsafe($O);$W4->apply_unsafe($q);$W4->apply_unsafe($E);$W4->apply_unsafe($d);$W4->apply_unsafe($r);$W4->apply_unsafe($s);$W4->apply_unsafe($t);$W4->apply_unsafe($u);$W4->apply_unsafe($v);$W4->apply_unsafe($w);$W4->apply_unsafe($x);$W4->apply_unsafe($X4);$W4->apply_unsafe($z);$h5->apply_unsafe($Q);$h5->apply_unsafe($h);$h5->apply_unsafe($j);$h5->apply_unsafe($N);$h5->apply_unsafe($k);$h5->apply_unsafe($l);$h5->apply_unsafe($m);$h5->apply_unsafe($n);$h5->apply_unsafe($o);$h5->apply_unsafe($K);$h5->apply_unsafe($p);$h5->apply_unsafe($O);$h5->apply_unsafe($q);$h5->apply_unsafe($E);$h5->apply_unsafe($d);$h5->apply_unsafe($r);$h5->apply_unsafe($s);$h5->apply_unsafe($t);$h5->apply_unsafe($u);$h5->apply_unsafe($v);$h5->apply_unsafe($w);$h5->apply_unsafe($x);$h5->apply_unsafe($i5);$h5->apply_unsafe($z);$q5->apply_unsafe($Q);$q5->apply_unsafe($h);$q5->apply_unsafe($j);$q5->apply_unsafe($N);$q5->apply_unsafe($k);$q5->apply_unsafe($l);$q5->apply_unsafe($m);$q5->apply_unsafe($n);$q5->apply_unsafe($o);$q5->apply_unsafe($p);$q5->apply_unsafe($O);$q5->apply_unsafe($q);$q5->apply_unsafe($E);$q5->apply_unsafe($d);$q5->apply_unsafe($r);$q5->apply_unsafe($s);$q5->apply_unsafe($t);$q5->apply_unsafe($u);$q5->apply_unsafe($v);$q5->apply_unsafe($w);$q5->apply_unsafe($x);$q5->apply_unsafe($r5);$q5->apply_unsafe($z);$D5->apply_unsafe($Q);$D5->apply_unsafe($h);$D5->apply_unsafe($j);$D5->apply_unsafe($k);$D5->apply_unsafe($l);$D5->apply_unsafe($m);$D5->apply_unsafe($n);$D5->apply_unsafe($o);$D5->apply_unsafe($p);$D5->apply_unsafe($q);$D5->apply_unsafe($E);$D5->apply_unsafe($d);$D5->apply_unsafe($r);$D5->apply_unsafe($s);$D5->apply_unsafe($t);$D5->apply_unsafe($u);$D5->apply_unsafe($v);$D5->apply_unsafe($w);$D5->apply_unsafe($x);$D5->apply_unsafe($E5);$D5->apply_unsafe($z);$Q5->apply_unsafe($N);$g6->apply_unsafe($Q);$g6->apply_unsafe($h);$g6->apply_unsafe($j);$g6->apply_unsafe($N);$g6->apply_unsafe($k);$g6->apply_unsafe($l);$g6->apply_unsafe($m);$g6->apply_unsafe($n);$g6->apply_unsafe($o);$g6->apply_unsafe($p);$g6->apply_unsafe($q);$g6->apply_unsafe($E);$g6->apply_unsafe($d);$g6->apply_unsafe($r);$g6->apply_unsafe($s);$g6->apply_unsafe($t);$g6->apply_unsafe($u);$g6->apply_unsafe($v);$g6->apply_unsafe($w);$g6->apply_unsafe($x);$g6->apply_unsafe($h6);$g6->apply_unsafe($z);$s6->apply_unsafe($Q);$s6->apply_unsafe($h);$s6->apply_unsafe($j);$s6->apply_unsafe($N);$s6->apply_unsafe($k);$s6->apply_unsafe($l);$s6->apply_unsafe($m);$s6->apply_unsafe($n);$s6->apply_unsafe($o);$s6->apply_unsafe($p);$s6->apply_unsafe($q);$s6->apply_unsafe($E);$s6->apply_unsafe($d);$s6->apply_unsafe($r);$s6->apply_unsafe($s);$s6->apply_unsafe($t);$s6->apply_unsafe($u);$s6->apply_unsafe($v);$s6->apply_unsafe($w);$s6->apply_unsafe($x);$s6->apply_unsafe($t6);$s6->apply_unsafe($z);$E6->apply_unsafe($Q);$E6->apply_unsafe($h);$E6->apply_unsafe($j);$E6->apply_unsafe($k);$E6->apply_unsafe($l);$E6->apply_unsafe($m);$E6->apply_unsafe($n);$E6->apply_unsafe($o);$E6->apply_unsafe($p);$E6->apply_unsafe($q);$E6->apply_unsafe($d);$E6->apply_unsafe($r);$E6->apply_unsafe($s);$E6->apply_unsafe($t);$E6->apply_unsafe($u);$E6->apply_unsafe($v);$E6->apply_unsafe($w);$E6->apply_unsafe($x);$E6->apply_unsafe($F6);$E6->apply_unsafe($z);$j7->apply_unsafe($U);$w7->apply_unsafe($U);$V7->apply_unsafe($U);$j8->apply_unsafe($U);$w8->apply_unsafe($U);$N8->apply_unsafe($R);$U8->apply_unsafe($R);$e9->apply_unsafe($R);$p9->apply_unsafe($R);$Aa->apply_unsafe($T);$dc->apply_unsafe($T);$Cd->apply_unsafe($yc);$Cd->apply_unsafe($zc);$te->apply_unsafe($W);$te->apply_unsafe($X);$te->apply_unsafe($Y);$te->apply_unsafe($Z);$te->apply_unsafe($c1);$te->apply_unsafe($d1);$te->apply_unsafe($e1);$te->apply_unsafe($g1);$Ce->apply_unsafe($W);$Ce->apply_unsafe($X);$Ce->apply_unsafe($Y);$Ce->apply_unsafe($Z);$Ce->apply_unsafe($c1);$Ce->apply_unsafe($d1);$Ce->apply_unsafe($e1);$Ce->apply_unsafe($g1);$Pe->apply_unsafe($W);$Pe->apply_unsafe($X);$Pe->apply_unsafe($Y);$Pe->apply_unsafe($Z);$Pe->apply_unsafe($c1);$Pe->apply_unsafe($d1);$Pe->apply_unsafe($e1);$Pe->apply_unsafe($g1);$cf->apply_unsafe($W);$lf->apply_unsafe($W);$Gf->apply_unsafe($X);$Of->apply_unsafe($X);$Xf->apply_unsafe($X);$mg->apply_unsafe($X);$mg->apply_unsafe($Y);$mg->apply_unsafe($Z);$mg->apply_unsafe($d1);$mg->apply_unsafe($e1);$Cg->apply_unsafe($X);$eh->apply_unsafe($Y);$mh->apply_unsafe($Y);$zh->apply_unsafe($Y);$Uh->apply_unsafe($Z);$ei->apply_unsafe($Z);$pi->apply_unsafe($Z);$fj->apply_unsafe($d1);$nj->apply_unsafe($d1);$sj->apply_unsafe($d1);$Dj->apply_unsafe($d1);$gk->apply_unsafe($e1);$ok->apply_unsafe($e1);$xk->apply_unsafe($e1);$Ik->apply_unsafe($e1);$hl->apply_unsafe($g1);$wl->apply_unsafe($g1);$ni::self=$Sl;&$_($F)for@$G;&$_($I)for@$J;&$_($q1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$y1;&$_($E1)for@$G1;&$_($J1)for@$y1;&$_($M1)for@$y1;&$_($P1)for@$y1;&$_($S1)for@$T1;&$_($Y1)for@$y1;&$_($d2)for@$y1;&$_($h2)for@$y1;&$_($k2)for@$l2;&$_($q2)for@$y1;&$_($u2)for@$y1;&$_($x2)for@$y2;&$_($D2)for@$y1;&$_($G2)for@$H2;&$_($J2)for@$K2;&$_($M2)for@$y1;&$_($P2)for@$R2;&$_($T2)for@$U2;&$_($W2)for@$X2;&$_($c3)for@$y1;&$_($e3)for@$y1;&$_($h3)for@$j3;&$_($l3)for@$y1;&$_($n3)for@$y1;&$_($v3)for@$w3;&$_($z3)for@$y1;&$_($C3)for@$D3;&$_($H3)for@$y1;&$_($K3)for@$L3;&$_($N3)for@$O3;&$_($S3)for@$y1;&$_($V3)for@$X3;&$_($Z3)for@$c4;&$_($e4)for@$f4;&$_($k4)for@$l4;&$_($p4)for@$y1;&$_($s4)for@$t4;&$_($w4)for@$y1;&$_($z4)for@$A4;&$_($C4)for@$D4;&$_($I4)for@$y1;&$_($K4)for@$y1;&$_($N4)for@$P4;&$_($T4)for@$y1;&$_($W4)for@$Y4;&$_($e5)for@$y1;&$_($h5)for@$j5;&$_($n5)for@$y1;&$_($q5)for@$s5;&$_($u5)for@$v5;&$_($y5)for@$y1;&$_($A5)for@$y1;&$_($D5)for@$F5;&$_($I5)for@$J5;&$_($N5)for@$y1;&$_($Q5)for@$R5;&$_($T5)for@$U5;&$_($d6)for@$y1;&$_($g6)for@$i6;&$_($m6)for@$y1;&$_($p6)for@$y1;&$_($s6)for@$u6;&$_($w6)for@$x6;&$_($B6)for@$y1;&$_($E6)for@$G6;&$_($I6)for@$J6;&$_($L6)for@$M6;&$_($P6)for@$Q6;&$_($T6)for@$U6;&$_($c7)for@$y1;&$_($g7)for@$y1;&$_($j7)for@$k7;&$_($p7)for@$y1;&$_($t7)for@$y1;&$_($w7)for@$x7;&$_($C7)for@$y1;&$_($G7)for@$y1;&$_($K7)for@$y1;&$_($O7)for@$y1;&$_($S7)for@$y1;&$_($V7)for@$W7;&$_($c8)for@$y1;&$_($g8)for@$y1;&$_($j8)for@$k8;&$_($p8)for@$y1;&$_($t8)for@$y1;&$_($w8)for@$x8;&$_($z8)for@$A8;&$_($F8)for@$G8;&$_($K8)for@$y1;&$_($N8)for@$O8;&$_($R8)for@$y1;&$_($U8)for@$V8;&$_($Z8)for@$y1;&$_($e9)for@$f9;&$_($j9)for@$y1;&$_($m9)for@$y1;&$_($p9)for@$q9;&$_($s9)for@$t9;&$_($v9)for@$w9;&$_($J9)for@$y1;&$_($M9)for@$w9;&$_($ra)for@$sa;&$_($xa)for@$y1;&$_($Aa)for@$Ba;&$_($Ga)for@$y1;&$_($Ka)for@$y1;&$_($Oa)for@$y1;&$_($Sa)for@$y1;&$_($Wa)for@$y1;&$_($cb)for@$y1;&$_($gb)for@$y1;&$_($kb)for@$y1;&$_($ob)for@$y1;&$_($sb)for@$y1;&$_($wb)for@$y1;&$_($Ab)for@$y1;&$_($Eb)for@$y1;&$_($Ib)for@$y1;&$_($Mb)for@$y1;&$_($Qb)for@$y1;&$_($Ub)for@$y1;&$_($Yb)for@$y1;&$_($dc)for@$ec;&$_($gc)for@$hc;&$_($Fc)for@$y1;&$_($Jc)for@$y1;&$_($Nc)for@$y1;&$_($Rc)for@$y1;&$_($Vc)for@$y1;&$_($Zc)for@$y1;&$_($fd)for@$y1;&$_($jd)for@$y1;&$_($nd)for@$y1;&$_($rd)for@$y1;&$_($vd)for@$y1;&$_($zd)for@$y1;&$_($Cd)for@$Dd;&$_($Fd)for@$Gd;&$_($fe)for@$ge;&$_($ie)for@$je;&$_($qe)for@$y1;&$_($te)for@$ue;&$_($ze)for@$y1;&$_($Ce)for@$De;&$_($Ie)for@$y1;&$_($Me)for@$y1;&$_($Pe)for@$Qe;&$_($Se)for@$Te;&$_($Xe)for@$y1;&$_($cf)for@$df;&$_($if)for@$y1;&$_($lf)for@$mf;&$_($of)for@$pf;&$_($wf)for@$xf;&$_($Df)for@$y1;&$_($Gf)for@$Hf;&$_($Lf)for@$y1;&$_($Of)for@$Pf;&$_($Uf)for@$y1;&$_($Xf)for@$Yf;&$_($gg)for@$y1;&$_($jg)for@$y1;&$_($mg)for@$ng;&$_($pg)for@$qg;&$_($vg)for@$y1;&$_($zg)for@$y1;&$_($Cg)for@$Dg;&$_($Fg)for@$Gg;&$_($Qg)for@$Rg;&$_($Wg)for@$y1;&$_($Zg)for@$y1;&$_($eh)for@$fh;&$_($jh)for@$y1;&$_($mh)for@$nh;&$_($sh)for@$y1;&$_($wh)for@$y1;&$_($zh)for@$Ah;&$_($Ch)for@$Dh;&$_($Lh)for@$Mh;&$_($Rh)for@$y1;&$_($Uh)for@$Vh;&$_($Zh)for@$y1;&$_($ei)for@$fi;&$_($ji)for@$y1;&$_($mi)for@$y1;&$_($pi)for@$qi;&$_($si)for@$ti;&$_($Hi)for@$Ii;&$_($Oi)for@$y1;&$_($Si)for@$y1;&$_($Wi)for@$y1;&$_($cj)for@$y1;&$_($fj)for@$gj;&$_($kj)for@$y1;&$_($nj)for@$oj;&$_($sj)for@$tj;&$_($xj)for@$y1;&$_($Aj)for@$y1;&$_($Dj)for@$Ej;&$_($Gj)for@$Hj;&$_($Sj)for@$Tj;&$_($Yj)for@$y1;&$_($dk)for@$y1;&$_($gk)for@$hk;&$_($lk)for@$y1;&$_($ok)for@$pk;&$_($uk)for@$y1;&$_($xk)for@$yk;&$_($Ck)for@$y1;&$_($Fk)for@$y1;&$_($Ik)for@$Jk;&$_($Mk)for@$Nk;&$_($Wk)for@$Xk;&$_($el)for@$y1;&$_($hl)for@$il;&$_($ml)for@$y1;&$_($ql)for@$y1;&$_($tl)for@$y1;&$_($wl)for@$xl;&$_($zl)for@$Al;&$_($Il)for@$y1;&$_($Ml)for@$y1;&$_($Ql)for@$y1;&$_($Sl)for@$Tl;ni->run(@ARGV);
__DATA__
