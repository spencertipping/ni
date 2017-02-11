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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/module.c';$k={$h,1,$j,1};$l=q'/lib/behavior.c';$m=q'/lib/branch.c';$n=q'/lib/dataslice.c';$o=q'/lib/doc.c';$p=q'/lib/fn.c';$q=q'/lib/image.c';$r=q'/lib/ni.c';$s=q'/lib/slice.c';$t=q'/lib/tag.c';$u=q'/lib/test_value.c';$v=q'/object.c';$w=q'/unix/cat.c';$x=q'/unix/fd.c';$y=q'/unix/fifo.c';$z=q'/unix/file.c';$A=q'/unix/io.c';$B=q'/unix/pid.c';$C=q'/unix/pipeline.c';$D=q'/unix/str.c';$E={$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$F={$s,1};$G=q'/lib/dataslice.c';$H={$l,1,$m,1,$G,1,$s,1,$t,1};$I=[undef];$J=q'/metaclass';$K=bless({$c,$H,$f,$l,$g,$I},$J);$L=q'/metaclass::ctors';$M=[$K];$N=bless({$c,$F,$f,$s,$g,$M},$J);$O=q'/metaclass::ctors';$P=q'/lib/slice';$Q={$P,1};$R=q'/lib/behavior';$S=q'/lib/branch';$T=q'/lib/dataslice';$U=q'/lib/tag';$V={$R,1,$S,1,$T,1,$P,1,$U,1};$W=q'/class';$X=q'/lib/dataslice.c';$Y=q'/lib/doc';$Z=q'/lib/fn';$c1=q'/lib/image';$d1=q'/lib/ni';$e1=q'/lib/test_value';$f1=q'/lib/test_value.c';$g1=q'/module';$h1=q'/object';$i1=q'/unix/cat';$j1=q'/unix/fd';$k1=q'/unix/fifo';$l1=q'/unix/file';$m1=q'/unix/io';$n1=q'/unix/pid';$o1=q'/unix/pipeline';$p1=q'/unix/pipeline.c';$q1=q'/unix/str';$r1={$W,1,$h,1,$R,1,$l,1,$S,1,$m,1,$T,1,$X,1,$Y,1,$o,1,$Z,1,$p,1,$c1,1,$q,1,$d1,1,$r,1,$P,1,$s,1,$U,1,$t,1,$e1,1,$f1,1,$J,1,$d,1,$g1,1,$j,1,$h1,1,$v,1,$i1,1,$w,1,$j1,1,$x,1,$k1,1,$y,1,$l1,1,$z,1,$m1,1,$A,1,$n1,1,$B,1,$o1,1,$p1,1,$q1,1,$D,1};$s1={};$t1=q'ctor';$u1=undef;$v1=q'dtor';$w1=q'methods';$x1=q'class';$y1={$p,1};$z1=[undef];$A1=bless({$c,$y1,$f,$p,$g,$z1},$J);$B1=q'/metaclass::ctors';$C1={$Z,1};$D1={};$E1=q'DESTROY';$F1=q'code';$G1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$H1=q'proto';$I1=q'';$J1=bless({$F1,$G1,$H1,$I1},$Z);$K1=q'/lib/fn::ctors';$L1=q'new';$M1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$N1=bless({$F1,$M1,$H1,$I1},$Z);$O1={$E1,$J1,$L1,$N1};$P1=q'/lib/instantiable.b';$Q1=bless({$c,$D1,$w1,$O1,$f,$P1},$P);$R1=q'/lib/dataslice.c';$S1=q'/lib/test_value.c';$T1=q'/unix/pipeline.c';$U1=q'/lib/slice::ctors';$V1={};$W1=q'shift->compile';$X1=bless({$F1,$W1,$H1,$I1},$Z);$Y1=q'compile';$Z1=q'local $@;
my $self = shift;
$$self{proto} ||= \'\';
$$self{fn} = ni::eval "sub $$self{proto} {$$self{code}\\n}";
die "ni:/lib/fn: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$c2=bless({$F1,$Z1,$H1,$I1},$Z);$d2=q'instantiate';$e2=q'my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : \'\';
+{code        => $code,
  proto       => $proto,
  annotations => [@_]};';$f2=bless({$F1,$e2,$H1,$I1},$Z);$g2={$Y1,$c2,$d2,$f2};$h2=q'/lib/fn_init.b';$i2=bless({$c,$V1,$t1,$X1,$v1,$u1,$w1,$g2,$f,$h2},$P);$j2=q'/lib/slice::ctors';$k2={};$l2=q'annotations';$m2=[];$n2=q'shift->{\'annotations\'}';$o2=bless({$l2,$m2,$F1,$n2,$H1,$I1},$Z);$p2=[];$q2=q'shift->{\'code\'}';$r2=bless({$l2,$p2,$F1,$q2,$H1,$I1},$Z);$s2=q'fn';$t2=[];$u2=q'shift->{\'fn\'}';$v2=bless({$l2,$t2,$F1,$u2,$H1,$I1},$Z);$w2={$l2,$o2,$F1,$r2,$s2,$v2};$x2=q'/lib/fn_ro.b';$y2=bless({$c,$k2,$t1,$u1,$v1,$u1,$w1,$w2,$f,$x2},$P);$z2=q'/lib/slice::ctors';$A2={};$B2=q'(""';$C2=[];$D2=q'shift->{code}';$E2=bless({$l2,$C2,$F1,$D2,$H1,$I1},$Z);$F2=q'(eq';$G2=[];$H2=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])';$I2=bless({$l2,$G2,$F1,$H2,$H1,$I1},$Z);$J2={$B2,$E2,$F2,$I2};$K2=q'/lib/fn_ops.b';$L2=bless({$c,$A2,$t1,$u1,$v1,$u1,$w1,$J2,$f,$K2},$P);$M2=q'/lib/slice::ctors';$N2={};$O2=q'serialize';$P2=[];$Q2=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$R2=bless({$l2,$P2,$F1,$Q2,$H1,$I1},$Z);$S2={$O2,$R2};$T2=q'/lib/fn_serialize.b';$U2=bless({$c,$N2,$t1,$u1,$v1,$u1,$w1,$S2,$f,$T2},$P);$V2=q'/lib/slice::ctors';$W2=[undef,$Q1,$i2,$y2,$L2,$U2];$X2=bless({$c,$C1,$f,$Z,$g,$W2},$p);$Y2=q'/lib/fn.c::ctors';$Z2=q'ni \'ni:\' . ref shift';$c3=bless({$F1,$Z2,$H1,$I1},$Z);$d3={$x1,$c3};$e3=q'/lib/instance.b';$f3=bless({$c,$s1,$t1,$u1,$v1,$u1,$w1,$d3,$f,$e3},$P);$g3=q'/lib/dataslice.c';$h3=q'/lib/test_value.c';$i3=q'/unix/pipeline.c';$j3=q'/lib/slice::ctors';$k3=[$f3];$l3=bless({$c,$r1,$f,$h1,$g,$k3},$v);$m3=q'/object.c::ctors';$n3=[$l3];$o3=bless({$c,$V,$f,$R,$g,$n3},$l);$p3=q'/lib/behavior.c::ctors';$q3={};$r3=q'my $s = shift; ni->def($s->name, $s)';$s3=bless({$F1,$r3,$H1,$I1},$Z);$t3=q'$_[0]->namespace . ":" . $_[0]->{name}';$u3=bless({$F1,$t3,$H1,$I1},$Z);$v3={$f,$u3};$w3=q'/lib/named.b';$x3=bless({$c,$q3,$t1,$s3,$v1,$u1,$w1,$v3,$f,$w3},$P);$y3=q'/lib/dataslice.c';$z3=q'/lib/test_value.c';$A3=q'/unix/pipeline.c';$B3=q'/lib/slice::ctors';$C3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$D3=bless({$F1,$C3,$H1,$I1},$Z);$E3=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$F3=bless({$F1,$E3,$H1,$I1},$Z);$G3=q'/lib/slice::apply';$H3=q'/lib/slice::apply_unsafe';$I3={};$J3=q'apply';$K3=q'apply_unsafe';$L3={$J3,$D3,$K3,$F3};$M3=q'/lib/slice.b';$N3=bless({$c,$I3,$w1,$L3,$f,$M3},$P);$O3=q'/lib/slice::ctors';$P3={};$Q3=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$R3=bless({$F1,$Q3,$H1,$I1},$Z);$S3={$d2,$R3};$T3=q'/lib/slice_init.b';$U3=bless({$c,$P3,$w1,$S3,$f,$T3},$P);$V3=q'/lib/slice::ctors';$W3={};$X3=[];$Y3=q'local $_;
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
$g;';$Z3=bless({$l2,$X3,$F1,$Y3,$H1,$I1},$Z);$c4={$O2,$Z3};$d4=q'/lib/slice_serialize.b';$e4=bless({$c,$W3,$t1,$u1,$v1,$u1,$w1,$c4,$f,$d4},$P);$f4=q'/lib/slice::ctors';$g4=[$o3,$x3,$N3,$U3,$e4];$h4=bless({$c,$Q,$f,$P,$g,$g4},$s);$i4=q'/lib/slice.c::ctors';$j4={};$k4=q'doc';$l4=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:/lib/doc\')->new($name);';$m4=bless({$F1,$l4,$H1,$I1},$Z);$n4={$k4,$m4};$o4=q'/lib/documentable.b';$p4=bless({$c,$j4,$t1,$u1,$v1,$u1,$w1,$n4,$f,$o4},$P);$q4=q'/lib/dataslice.c';$r4=q'/lib/test_value.c';$s4=q'/unix/pipeline.c';$t4=q'/lib/slice::ctors';$u4=[undef,$p4];$v4=bless({$c,$E,$f,$v,$g,$u4},$J);$w4=q'/metaclass::ctors';$x4=[$v4,$Q1];$y4=bless({$c,$k,$f,$j,$g,$x4},$J);$z4=q'/metaclass::ctors';$A4=[$y4];$B4=bless({$c,$i,$f,$h,$g,$A4},$J);$C4=q'/metaclass::ctors';$D4=q'/lib/dataslice.c';$E4=q'/lib/test_value.c';$F4=q'/unix/pipeline.c';$G4={$W,1,$h,1,$l,1,$m,1,$D4,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$E4,1,$d,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$F4,1,$D,1};$H4=q'/lib/dataslice.c';$I4=q'/lib/test_value.c';$J4=q'/unix/pipeline.c';$K4={$W,1,$h,1,$l,1,$m,1,$H4,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$I4,1,$J,1,$d,1,$g1,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$J4,1,$D,1};$L4={$t,1};$M4=[$K];$N4=bless({$c,$L4,$f,$t,$g,$M4},$J);$O4=q'/metaclass::ctors';$P4={$U,1};$Q4={};$R4=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$S4=bless({$F1,$R4,$H1,$I1},$Z);$T4={$J3,$S4};$U4=q'/lib/tag.b';$V4=bless({$c,$Q4,$t1,$u1,$v1,$u1,$w1,$T4,$f,$U4},$P);$W4=q'/lib/slice::ctors';$X4={};$Y4=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$Z4=bless({$F1,$Y4,$H1,$I1},$Z);$c5={$d2,$Z4};$d5=q'/lib/tag_init.b';$e5=bless({$c,$X4,$t1,$u1,$v1,$u1,$w1,$c5,$f,$d5},$P);$f5=q'/lib/slice::ctors';$g5=[$o3,$x3,$V4,$e5];$h5=bless({$c,$P4,$f,$U,$g,$g5},$t);$i5=q'/lib/tag.c::ctors';$j5=q'/lib/perlbranch.b';$k5={};$l5=q'add';$m5=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$n5=bless({$F1,$m5,$H1,$I1},$Z);$o5=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$p5=bless({$F1,$o5,$H1,$I1},$Z);$q5={$l5,$n5,$J3,$p5};$r5=q'/lib/branch.b';$s5=bless({$c,$k5,$t1,$u1,$v1,$u1,$w1,$q5,$f,$r5},$P);$t5=q'/lib/dataslice.c';$u5=q'/lib/test_value.c';$v5=q'/unix/pipeline.c';$w5=q'/lib/slice::ctors';$x5={};$y5=q'namespace';$z5=q'\'ni\'';$A5=bless({$F1,$z5,$H1,$I1},$Z);$B5={$y5,$A5};$C5=q'/lib/named_in_ni.b';$D5=bless({$c,$x5,$t1,$u1,$v1,$u1,$w1,$B5,$f,$C5},$P);$E5=q'/lib/dataslice.c';$F5=q'/lib/test_value.c';$G5=q'/unix/pipeline.c';$H5=q'/lib/slice::ctors';$I5={};$J5=q'package';$K5=q'shift->{name}';$L5=bless({$F1,$K5,$H1,$I1},$Z);$M5={$J5,$L5};$N5=q'/lib/namespaced.b';$O5=bless({$c,$I5,$t1,$u1,$v1,$u1,$w1,$M5,$f,$N5},$P);$P5=q'/lib/dataslice.c';$Q5=q'/lib/test_value.c';$R5=q'/unix/pipeline.c';$S5=q'/lib/slice::ctors';$T5={};$U5=q'resolve';$V5=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$W5=bless({$F1,$V5,$H1,$I1},$Z);$X5={$U5,$W5};$Y5=q'/lib/resolver.b';$Z5=bless({$c,$T5,$t1,$u1,$v1,$u1,$w1,$X5,$f,$Y5},$P);$c6=q'/lib/dataslice.c';$d6=q'/lib/test_value.c';$e6=q'/unix/pipeline.c';$f6=q'/lib/slice::ctors';$g6=[$s5,$x3,$D5,$O5,$Z5];$h6=bless({$f,$j5,$g,$g6},$U);$i6=q'/lib/tag::ctors';$j6={};$k6=q'my $s = shift; $s->apply($s->package)';$l6=bless({$F1,$k6,$H1,$I1},$Z);$m6=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$n6=bless({$F1,$m6,$H1,$I1},$Z);$o6={$d2,$n6};$p6=q'/lib/class_init.b';$q6=bless({$c,$j6,$t1,$l6,$v1,$u1,$w1,$o6,$f,$p6},$P);$r6=q'/lib/dataslice.c';$s6=q'/lib/test_value.c';$t6=q'/unix/pipeline.c';$u6=q'/lib/slice::ctors';$v6={$m,1};$w6=[$K];$x6=bless({$c,$v6,$f,$m,$g,$w6},$J);$y6=q'/metaclass::ctors';$z6={$S,1};$A6={};$B6=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$C6=bless({$F1,$B6,$H1,$I1},$Z);$D6={$d2,$C6};$E6=q'/lib/branch_init.b';$F6=bless({$c,$A6,$t1,$u1,$v1,$u1,$w1,$D6,$f,$E6},$P);$G6=q'/lib/slice::ctors';$H6=[$o3,$x3,$s5,$F6,undef];$I6=bless({$c,$z6,$f,$S,$g,$H6},$m);$J6=q'/lib/branch.c::ctors';$K6=q'/lib/dataslice.c';$L6=q'/lib/test_value.c';$M6=q'/unix/pipeline.c';$N6={$W,1,$h,1,$l,1,$S,1,$m,1,$K6,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$L6,1,$J,1,$d,1,$g1,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$M6,1,$D,1};$O6=q'/lib/definition.b';$P6={};$Q6=q'def';$R6=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$S6=bless({$F1,$R6,$H1,$I1},$Z);$T6={$Q6,$S6};$U6=q'/lib/definition_def.b';$V6=bless({$c,$P6,$t1,$u1,$v1,$u1,$w1,$T6,$f,$U6},$P);$W6=q'/lib/dataslice.c';$X6=q'/lib/test_value.c';$Y6=q'/unix/pipeline.c';$Z6=q'/lib/slice::ctors';$c7={};$d7=q'ro';$e7=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$f7=bless({$F1,$e7,$H1,$I1},$Z);$g7=q'rw';$h7=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$i7=bless({$F1,$h7,$H1,$I1},$Z);$j7={$d7,$f7,$g7,$i7};$k7=q'/lib/accessor.b';$l7=bless({$c,$c7,$t1,$u1,$v1,$u1,$w1,$j7,$f,$k7},$P);$m7=q'/lib/dataslice.c';$n7=q'/lib/test_value.c';$o7=q'/unix/pipeline.c';$p7=q'/lib/slice::ctors';$q7={};$r7=q'shift->name';$s7=bless({$F1,$r7,$H1,$I1},$Z);$t7={$B2,$s7};$u7=q'/lib/name_as_string.b';$v7=bless({$c,$q7,$t1,$u1,$v1,$u1,$w1,$t7,$f,$u7},$P);$w7=q'/lib/dataslice.c';$x7=q'/lib/test_value.c';$y7=q'/unix/pipeline.c';$z7=q'/lib/slice::ctors';$A7={};$B7=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);';$C7=bless({$F1,$B7,$H1,$I1},$Z);$D7={$F2,$C7};$E7=q'/lib/ref_eq.b';$F7=bless({$c,$A7,$t1,$u1,$v1,$u1,$w1,$D7,$f,$E7},$P);$G7=q'/lib/dataslice.c';$H7=q'/lib/test_value.c';$I7=q'/unix/pipeline.c';$J7=q'/lib/slice::ctors';$K7={};$L7=q'defdata';$M7=q'shift->add(ni(\'ni:/lib/dataslice\')->new(@_))';$N7=bless({$F1,$M7,$H1,$I1},$Z);$O7={$L7,$N7};$P7=q'/lib/definition_defdata.b';$Q7=bless({$c,$K7,$t1,$u1,$v1,$u1,$w1,$O7,$f,$P7},$P);$R7=q'/lib/dataslice.c';$S7=q'/lib/test_value.c';$T7=q'/unix/pipeline.c';$U7=q'/lib/slice::ctors';$V7=[$V6,$l7,$v7,$F7,$Q7];$W7=bless({$c,$N6,$f,$O6,$g,$V7},$S);$X7=q'/lib/branch::ctors';$Y7=[$h6,$q6,$l3,$W7];$Z7=bless({$c,$K4,$f,$g1,$g,$Y7},$j);$c8=q'/module.c::ctors';$d8={};$e8=q'child';$f8=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$g8=bless({$F1,$f8,$H1,$I1},$Z);$h8={$e8,$g8};$i8=q'/lib/subclass.b';$j8=bless({$c,$d8,$t1,$u1,$v1,$u1,$w1,$h8,$f,$i8},$P);$k8=q'/lib/dataslice.c';$l8=q'/lib/test_value.c';$m8=q'/unix/pipeline.c';$n8=q'/lib/slice::ctors';$o8=[$Z7,$Q1,$Z7,$j8];$p8=bless({$c,$G4,$f,$W,$g,$o8},$h);$q8=q'/class.c::ctors';$r8=[$p8];$s8=bless({$c,$e,$f,$d,$g,$r8},$J);$t8=q'/metaclass::ctors';$u8={$J,1};$v8=[$h6,$Q1,$q6,$Z7];$w8=bless({$c,$u8,$f,$J,$g,$v8},$d);$x8=q'/metaclass.c::ctors';$y8={$r,1};$z8=[$v4];$A8=bless({$c,$y8,$f,$r,$g,$z8},$J);$B8=q'/metaclass::ctors';$C8={$d1,1};$D8={};$E8=q'is_mutable';$F8=[];$G8=q'$0 ne "-" && -w $0';$H8=bless({$l2,$F8,$F1,$G8,$H1,$I1},$Z);$I8=q'modify';$J8=[];$K8=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$L8=bless({$l2,$J8,$F1,$K8,$H1,$I1},$Z);$M8={$E8,$H8,$I8,$L8};$N8=q'/lib/ni_self.b';$O8=bless({$c,$D8,$t1,$u1,$v1,$u1,$w1,$M8,$f,$N8},$P);$P8=q'/lib/slice::ctors';$Q8={};$R8=q'exists';$S8=[];$T8=q'exists $_[0]->{named}{$_[1]}';$U8=bless({$l2,$S8,$F1,$T8,$H1,$I1},$Z);$V8=q'quoted';$W8=[];$X8=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$Y8=bless({$l2,$W8,$F1,$X8,$H1,$I1},$Z);$Z8={$R8,$U8,$V8,$Y8};$c9=q'/lib/ni_image.b';$d9=bless({$c,$Q8,$t1,$u1,$v1,$u1,$w1,$Z8,$f,$c9},$P);$e9=q'/lib/slice::ctors';$f9={};$g9=q'--internal/+=';$h9=[];$i9=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$j9=bless({$l2,$h9,$F1,$i9,$H1,$I1},$Z);$k9=q'--internal/eval';$l9=[];$m9=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$n9=bless({$l2,$l9,$F1,$m9,$H1,$I1},$Z);$o9=q'--internal/image';$p9=[];$q9=q'shift->quoted->write(\\*STDOUT);
0;';$r9=bless({$l2,$p9,$F1,$q9,$H1,$I1},$Z);$s9=q'--internal/test';$t9=[];$u9=q'my $self = shift;
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
!!$fails;';$v9=bless({$l2,$t9,$F1,$u9,$H1,$I1},$Z);$w9=q'run';$x9=[];$y9=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$z9=bless({$l2,$x9,$F1,$y9,$H1,$I1},$Z);$A9={$g9,$j9,$k9,$n9,$o9,$r9,$s9,$v9,$w9,$z9};$B9=q'/lib/ni_main.b';$C9=bless({$c,$f9,$t1,$u1,$v1,$u1,$w1,$A9,$f,$B9},$P);$D9=q'/lib/slice::ctors';$E9={};$F9=[];$G9=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$H9=bless({$l2,$F9,$F1,$G9,$H1,$I1},$Z);$I9=q'resolver_for';$J9=[];$K9=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$L9=bless({$l2,$J9,$F1,$K9,$H1,$I1},$Z);$M9={$U5,$H9,$I9,$L9};$N9=q'/lib/ni_resolver.b';$O9=bless({$c,$E9,$t1,$u1,$v1,$u1,$w1,$M9,$f,$N9},$P);$P9=q'/lib/slice::ctors';$Q9={};$R9=q'fork';$S9=[];$T9=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$U9=bless({$l2,$S9,$F1,$T9,$H1,$I1},$Z);$V9=q'fork_exec';$W9=[];$X9=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$Y9=bless({$l2,$W9,$F1,$X9,$H1,$I1},$Z);$Z9={$R9,$U9,$V9,$Y9};$ca=q'/lib/ni_pid_ctors';$da=bless({$c,$Q9,$t1,$u1,$v1,$u1,$w1,$Z9,$f,$ca},$P);$ea=q'/lib/slice::ctors';$fa=[$l3,$O8,$d9,$C9,$O9,$da];$ga=bless({$c,$C8,$f,$d1,$g,$fa},$r);$ha=q'/lib/ni.c::ctors';$ia=q'named';$ja=q'ni.doc:/class';$ka={$o,1};$la=[$v4];$ma=bless({$c,$ka,$f,$o,$g,$la},$J);$na=q'/metaclass::ctors';$oa={$Y,1};$pa={};$qa=q'shift; +{name => shift, doc => []}';$ra=bless({$F1,$qa,$H1,$I1},$Z);$sa={$d2,$ra};$ta=q'/lib/doc_init.b';$ua=bless({$c,$pa,$t1,$u1,$v1,$u1,$w1,$sa,$f,$ta},$P);$va=q'/lib/slice::ctors';$wa={};$xa=q'\'ni.doc\'';$ya=bless({$F1,$xa,$H1,$I1},$Z);$za={$y5,$ya};$Aa=q'/lib/doc_namespace.b';$Ba=bless({$c,$wa,$t1,$u1,$v1,$u1,$w1,$za,$f,$Aa},$P);$Ca=q'/lib/slice::ctors';$Da={};$Ea=q'AUTOLOAD';$Fa=q'my $self = shift;
my $method = ${__PACKAGE__ . "::AUTOLOAD"};
push @{$$self{doc}}, [$method, @_];
$self;';$Ga=bless({$F1,$Fa,$H1,$I1},$Z);$Ha={$Ea,$Ga};$Ia=q'/lib/doc_define.b';$Ja=bless({$c,$Da,$t1,$u1,$v1,$u1,$w1,$Ha,$f,$Ia},$P);$Ka=q'/lib/slice::ctors';$La={};$Ma=q'eg';$Na=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$Oa=bless({$F1,$Na,$H1,$I1},$Z);$Pa=q'tests';$Qa=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$Ra=bless({$F1,$Qa,$H1,$I1},$Z);$Sa={$Ma,$Oa,$Pa,$Ra};$Ta=q'/lib/doc_test.b';$Ua=bless({$c,$La,$t1,$u1,$v1,$u1,$w1,$Sa,$f,$Ta},$P);$Va=q'/lib/slice::ctors';$Wa=[$l3,$x3,$ua,$Ba,$Ja,$Ua];$Xa=bless({$c,$oa,$f,$Y,$g,$Wa},$o);$Ya=q'/lib/doc.c::ctors';$Za=q'/lib/doc::synopsis';$cb=q'
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
  ';$db=[$Za,$cb];$eb=q'/lib/doc::description';$fb=q'ni:/class is at the core of ni\'s object-oriented system, along with core
      classes like ni:/object and ni:/metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$gb=[$eb,$fb];$hb=q'/lib/doc::behaviors';$ib=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:/lib/slice, which represents a set of methods you can add to a
      package. TODO...';$jb=[$hb,$ib];$kb=q'/lib/doc::classes';$lb=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$mb=q'TODO...';$nb=[$kb,$lb,$mb];$ob=[$db,$gb,$jb,$nb];$pb=bless({$k4,$ob,$f,$W},$Y);$qb=q'/lib/doc::ctors';$rb=q'ni.doc:/lib/doc';$sb=q'/lib/doc::synopsis';$tb=q'
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$ub=[$sb,$tb];$vb=q'/lib/doc::description';$wb=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$xb=q'Documentation objects are internally represented as arrays of quoted
      method calls:';$yb=q'my $doc = ni("ni:/lib/doc")->new("foo");
# TODO: make this into a test, but first we need a testing DSL
$doc->foo("bar bif baz");
# state is now [["foo", ["bar bif baz"]]]';$zb=bless({$F1,$yb,$H1,$I1},$Z);$Ab=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":';$Bb=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$Cb=bless({$F1,$Bb,$H1,$I1},$Z);$Db=[$vb,$wb,$xb,$Ma,$zb,$Ab,$Ma,$Cb];$Eb=[$ub,$Db];$Fb=bless({$k4,$Eb,$f,$Y},$Y);$Gb=q'ni.doc:/unix/cat';$Hb=q'/lib/doc::synopsis';$Ib=q'
    my $combined = ni(\'ni:/unix/cat\')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into($destination_io);
  ';$Jb=[$Hb,$Ib];$Kb=q'/lib/doc::description';$Lb=q'Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.';$Mb=[$Kb,$Lb];$Nb=[];$Ob=q'my $cat = ni("str:foo") + ni("str:bar");
ie $cat->read($_, 128) == 8 && $_ eq "foo\\nbar\\n";';$Pb=bless({$l2,$Nb,$F1,$Ob,$H1,$I1},$Z);$Qb=[$Ma,$Pb];$Rb=[$Jb,$Mb,$Qb];$Sb=bless({$k4,$Rb,$f,$i1},$Y);$Tb=q'ni:/class';$Ub=q'ni:/class.c';$Vb=q'ni:/lib/accessor.b';$Wb=q'ni:/lib/behavior';$Xb=q'ni:/lib/behavior.c';$Yb=q'ni:/lib/branch';$Zb=q'ni:/lib/branch.b';$cc=q'ni:/lib/branch.c';$dc=q'ni:/lib/branch_init.b';$ec=q'ni:/lib/class_init.b';$fc=q'ni:/lib/dataslice';$gc=q'/lib/dataslice.c';$hc={$gc,1};$ic=q'/lib/dataslice.c';$jc=[$K];$kc=bless({$c,$hc,$f,$ic,$g,$jc},$J);$lc=q'/metaclass::ctors';$mc={$T,1};$nc={};$oc=q'my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};';$pc=bless({$F1,$oc,$H1,$I1},$Z);$qc={$d2,$pc};$rc=q'/lib/dataslice_init.b';$sc=bless({$c,$nc,$t1,$u1,$v1,$u1,$w1,$qc,$f,$rc},$P);$tc=q'/lib/slice::ctors';$uc={};$vc=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;';$wc=bless({$F1,$vc,$H1,$I1},$Z);$xc={$J3,$wc};$yc=q'/lib/dataslice_apply.b';$zc=bless({$c,$uc,$t1,$u1,$v1,$u1,$w1,$xc,$f,$yc},$P);$Ac=q'/lib/slice::ctors';$Bc=[$o3,$sc,$zc];$Cc=q'/lib/dataslice.c';$Dc=bless({$c,$mc,$f,$T,$g,$Bc},$Cc);$Ec=q'/lib/dataslice.c::ctors';$Fc=q'ni:/lib/dataslice.c';$Gc=q'ni:/lib/dataslice_apply.b';$Hc=q'ni:/lib/dataslice_init.b';$Ic=q'ni:/lib/definition.b';$Jc=q'ni:/lib/definition_def.b';$Kc=q'ni:/lib/definition_defdata.b';$Lc=q'ni:/lib/doc';$Mc=q'ni:/lib/doc.c';$Nc=q'ni:/lib/doc_define.b';$Oc=q'ni:/lib/doc_init.b';$Pc=q'ni:/lib/doc_namespace.b';$Qc=q'ni:/lib/doc_test.b';$Rc=q'ni:/lib/documentable.b';$Sc=q'ni:/lib/fn';$Tc=q'ni:/lib/fn.c';$Uc=q'ni:/lib/fn_init.b';$Vc=q'ni:/lib/fn_ops.b';$Wc=q'ni:/lib/fn_ro.b';$Xc=q'ni:/lib/fn_serialize.b';$Yc=q'ni:/lib/global_static_test.b';$Zc={};$cd=q'ie';$dd=[];$ed=q'ni(\'ni:/lib/test_value\')->new(shift)';$fd=q'($)';$gd=bless({$l2,$dd,$F1,$ed,$H1,$fd},$Z);$hd={$cd,$gd};$id=q'/lib/global_static_test.b';$jd=bless({$c,$Zc,$t1,$u1,$v1,$u1,$w1,$hd,$f,$id},$P);$kd=q'main';$ld=q'/lib/slice::ctors';$md=q'ni:/lib/image';$nd={$q,1};$od=[$v4];$pd=bless({$c,$nd,$f,$q,$g,$od},$J);$qd=q'/metaclass::ctors';$rd={$c1,1};$sd={};$td=[];$ud=q'my $class = shift;
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
  ordering     => []};';$vd=bless({$l2,$td,$F1,$ud,$H1,$I1},$Z);$wd={$d2,$vd};$xd=q'/lib/image_init.b';$yd=bless({$c,$sd,$t1,$u1,$v1,$u1,$w1,$wd,$f,$xd},$P);$zd=q'/lib/slice::ctors';$Ad={};$Bd=q'address';$Cd=[];$Dd=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$Ed=bless({$l2,$Cd,$F1,$Dd,$H1,$I1},$Z);$Fd=q'allocate_gensym';$Gd=[];$Hd=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Id=bless({$l2,$Gd,$F1,$Hd,$H1,$I1},$Z);$Jd=q'boot_side_effect';$Kd=[];$Ld=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Md=bless({$l2,$Kd,$F1,$Ld,$H1,$I1},$Z);$Nd=q'circular_links';$Od=[];$Pd=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$Qd=bless({$l2,$Od,$F1,$Pd,$H1,$I1},$Z);$Rd=q'finalizer';$Sd=[];$Td=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Ud=bless({$l2,$Sd,$F1,$Td,$H1,$I1},$Z);$Vd=q'gensym';$Wd=[];$Xd=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$Yd=bless({$l2,$Wd,$F1,$Xd,$H1,$I1},$Z);$Zd=q'is_circular';$ce=[];$de=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$ee=bless({$l2,$ce,$F1,$de,$H1,$I1},$Z);$fe=q'quote';$ge=[];$he=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$ie=bless({$l2,$ge,$F1,$he,$H1,$I1},$Z);$je=q'quote_array';$ke=[];$le=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$me=bless({$l2,$ke,$F1,$le,$H1,$I1},$Z);$ne=q'quote_blessed';$oe=[];$pe=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$qe=bless({$l2,$oe,$F1,$pe,$H1,$I1},$Z);$re=q'quote_class';$se=[];$te=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$ue=bless({$l2,$se,$F1,$te,$H1,$I1},$Z);$ve=q'quote_hash';$we=[];$xe=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$ye=bless({$l2,$we,$F1,$xe,$H1,$I1},$Z);$ze=q'quote_object';$Ae=[];$Be=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$Ce=bless({$l2,$Ae,$F1,$Be,$H1,$I1},$Z);$De=q'quote_scalar';$Ee=[];$Fe=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Ge=bless({$l2,$Ee,$F1,$Fe,$H1,$I1},$Z);$He=q'quote_value';$Ie=[];$Je=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Ke=bless({$l2,$Ie,$F1,$Je,$H1,$I1},$Z);$Le=q'reconstruction';$Me=[];$Ne=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Oe=bless({$l2,$Me,$F1,$Ne,$H1,$I1},$Z);$Pe=q'side_effect';$Qe=[];$Re=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Se=bless({$l2,$Qe,$F1,$Re,$H1,$I1},$Z);$Te=q'write';$Ue=[];$Ve=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$We=bless({$l2,$Ue,$F1,$Ve,$H1,$I1},$Z);$Xe={$Bd,$Ed,$Fd,$Id,$Jd,$Md,$Nd,$Qd,$Rd,$Ud,$Vd,$Yd,$Zd,$ee,$fe,$ie,$je,$me,$ne,$qe,$re,$ue,$ve,$ye,$ze,$Ce,$De,$Ge,$He,$Ke,$Le,$Oe,$Pe,$Se,$Te,$We};$Ye=q'/lib/image_quoting.b';$Ze=bless({$c,$Ad,$t1,$u1,$v1,$u1,$w1,$Xe,$f,$Ye},$P);$cf=q'/lib/slice::ctors';$df=[$l3,$yd,$Ze];$ef=bless({$c,$rd,$f,$c1,$g,$df},$q);$ff=q'/lib/image.c::ctors';$gf=q'ni:/lib/image.c';$hf=q'ni:/lib/image_init.b';$if=q'ni:/lib/image_quoting.b';$jf=q'ni:/lib/instance.b';$kf=q'ni:/lib/instantiable.b';$lf=q'ni:/lib/json.b';$mf={};$nf=q'json_decode';$of=[];$pf=q'local $_;
my @v = [];
for ($_[0] =~ /[][{}]|true|false|null|"(?:[^"\\\\]+|\\\\.)*"|[-+eE\\d.]+/g) {
  if (/^[[{]$/) {
    push @v, [];
  } elsif (/^\\]$/) {
    die "json_decode $_[0]: too many closing brackets" if @v < 2;
    push @{$v[-2]}, $v[-1];
    pop @v;
  } elsif (/^\\}$/) {
    die "json_decode $_[0]: too many closing brackets" if @v < 2;
    push @{$v[-2]}, {@{$v[-1]}};
    pop @v;
  } else {
    push @{$v[-1]}, /^"/      ? json_unescape $_
                  : /^true$/  ? 1
                  : /^false$/ ? 0
                  : /^null$/  ? undef
                  :             0 + $_;
  }
}
my $r = pop @v;
die "json_decode $_[0]: not enough closing brackets" if @v;
wantarray ? @$r : $$r[0];';$qf=bless({$l2,$of,$F1,$pf,$H1,$fd},$Z);$rf=q'json_encode';$sf=[];$tf=q'local $_;
my ($v) = @_;
return "[" . join(\',\', map json_encode($_), @$v) . "]" if \'ARRAY\' eq ref $v;
return "{" . join(\',\', map json_escape($_) . ":" . json_encode($$v{$_}),
                           sort keys %$v) . "}" if \'HASH\' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? json_escape $v : \'null\';';$uf=bless({$l2,$sf,$F1,$tf,$H1,$fd},$Z);$vf=q'json_escape';$wf=[];$xf=q'(my $x = $_[0]) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . $ni::json_escapes{$1}/eg;
"\\"$x\\"";';$yf=bless({$l2,$wf,$F1,$xf,$H1,$fd},$Z);$zf=q'json_unescape';$Af=[];$Bf=q'my $x = substr $_[0], 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;';$Cf=bless({$l2,$Af,$F1,$Bf,$H1,$fd},$Z);$Df=q'json_unescape_one';$Ef=[];$Ff=q'$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1';$Gf=bless({$l2,$Ef,$F1,$Ff,$H1,$fd},$Z);$Hf={$nf,$qf,$rf,$uf,$vf,$yf,$zf,$Cf,$Df,$Gf};$If=q'/lib/json.b';$Jf=bless({$c,$mf,$t1,$u1,$v1,$u1,$w1,$Hf,$f,$If},$P);$Kf=q'ni';$Lf=q'/lib/slice::ctors';$Mf=q'ni:/lib/name_as_string.b';$Nf=q'ni:/lib/named.b';$Of=q'ni:/lib/named_in_ni.b';$Pf=q'ni:/lib/namespaced.b';$Qf=q'ni:/lib/ni';$Rf=q'ni:/lib/ni.c';$Sf=q'ni:/lib/ni_image.b';$Tf=q'ni:/lib/ni_main.b';$Uf=q'ni:/lib/ni_pid_ctors';$Vf=q'ni:/lib/ni_resolver.b';$Wf=q'ni:/lib/ni_self.b';$Xf=q'ni:/lib/ni_static_util.b';$Yf={};$Zf=q'abbrev';$cg=[];$dg=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$eg=bless({$l2,$cg,$F1,$dg,$H1,$I1},$Z);$fg=q'dor';$gg=[];$hg=q'defined $_[0] ? $_[0] : $_[1]';$ig=bless({$l2,$gg,$F1,$hg,$H1,$I1},$Z);$jg=q'indent';$kg=[];$lg=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$mg=bless({$l2,$kg,$F1,$lg,$H1,$I1},$Z);$ng=q'max';$og=[];$pg=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$qg=bless({$l2,$og,$F1,$pg,$H1,$I1},$Z);$rg=q'maxstr';$sg=[];$tg=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$ug=bless({$l2,$sg,$F1,$tg,$H1,$I1},$Z);$vg=q'mean';$wg=[];$xg=q'sum(@_) / (@_ || 1)';$yg=bless({$l2,$wg,$F1,$xg,$H1,$I1},$Z);$zg=q'min';$Ag=[];$Bg=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$Cg=bless({$l2,$Ag,$F1,$Bg,$H1,$I1},$Z);$Dg=q'minstr';$Eg=[];$Fg=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Gg=bless({$l2,$Eg,$F1,$Fg,$H1,$I1},$Z);$Hg=q'sgr';$Ig=[];$Jg=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Kg=bless({$l2,$Ig,$F1,$Jg,$H1,$I1},$Z);$Lg=q'sr';$Mg=[];$Ng=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Og=bless({$l2,$Mg,$F1,$Ng,$H1,$I1},$Z);$Pg=q'sum';$Qg=[];$Rg=q'local $_; my $x = 0; $x += $_ for @_; $x';$Sg=bless({$l2,$Qg,$F1,$Rg,$H1,$I1},$Z);$Tg=q'swap';$Ug=[];$Vg=q'@_[0, 1] = @_[1, 0]';$Wg=bless({$l2,$Ug,$F1,$Vg,$H1,$I1},$Z);$Xg={$Zf,$eg,$fg,$ig,$jg,$mg,$ng,$qg,$rg,$ug,$vg,$yg,$zg,$Cg,$Dg,$Gg,$Hg,$Kg,$Lg,$Og,$Pg,$Sg,$Tg,$Wg};$Yg=q'/lib/ni_static_util.b';$Zg=bless({$c,$Yf,$t1,$u1,$v1,$u1,$w1,$Xg,$f,$Yg},$P);$ch=q'/lib/slice::ctors';$dh=q'ni:/lib/perlbranch.b';$eh=q'ni:/lib/ref_eq.b';$fh=q'ni:/lib/resolver.b';$gh=q'ni:/lib/slice';$hh=q'ni:/lib/slice.b';$ih=q'ni:/lib/slice.c';$jh=q'ni:/lib/slice_init.b';$kh=q'ni:/lib/slice_serialize.b';$lh=q'ni:/lib/static_fn.b';$mh={};$nh=[];$oh=q'ni(\'ni:/lib/fn\')->new(@_)';$ph=bless({$l2,$nh,$F1,$oh,$H1,$fd},$Z);$qh=q'fp';$rh=[];$sh=q'ni(\'ni:/lib/fn\')->new(@_)';$th=q'($$)';$uh=bless({$l2,$rh,$F1,$sh,$H1,$th},$Z);$vh={$s2,$ph,$qh,$uh};$wh=q'/lib/static_fn.b';$xh=bless({$c,$mh,$t1,$u1,$v1,$u1,$w1,$vh,$f,$wh},$P);$yh=q'/lib/slice::ctors';$zh=q'ni:/lib/subclass.b';$Ah=q'ni:/lib/tag';$Bh=q'ni:/lib/tag.b';$Ch=q'ni:/lib/tag.c';$Dh=q'ni:/lib/tag_init.b';$Eh=q'ni:/lib/test_value';$Fh=q'/lib/test_value.c';$Gh={$Fh,1};$Hh=q'/lib/test_value.c';$Ih=[$v4];$Jh=bless({$c,$Gh,$f,$Hh,$g,$Ih},$J);$Kh=q'/metaclass::ctors';$Lh={$e1,1};$Mh={};$Nh=[];$Oh=q'\\$_[1]';$Ph=bless({$l2,$Nh,$F1,$Oh,$H1,$I1},$Z);$Qh={$d2,$Ph};$Rh=q'/lib/test_value_init.b';$Sh=bless({$c,$Mh,$t1,$u1,$v1,$u1,$w1,$Qh,$f,$Rh},$P);$Th=q'/lib/slice::ctors';$Uh={};$Vh=q'(==';$Wh=[];$Xh=q'my ($self, $rhs) = @_;
my $diff = $self->diff($rhs);
die "== fail: " . ref($self)->new($diff) if defined $diff;
1;';$Yh=bless({$l2,$Wh,$F1,$Xh,$H1,$I1},$Z);$Zh=q'diff';$ci=[];$di=q'my ($self, $rhs) = @_;
my $lhs = $$self;
my $rl = ref $lhs;
my $rr = ref $rhs;
my $realtype = Scalar::Util::reftype($lhs) || "";
return {type_difference => [$rl, $rr]} unless $rl eq $rr;
if ($realtype eq \'HASH\') {
  my @left_only  = grep !exists $$rhs{$_}, keys %$lhs;
  my @right_only = grep !exists $$lhs{$_}, keys %$rhs;
  return {hash_key_mismatch => 1,
          object_type       => $rl,
          left_only         => \\@left_only,
          right_only        => \\@right_only}
    if @left_only || @right_only;
  my %diff;
  $diff{$_} = ref($self)->new($$lhs{$_})->diff($$rhs{$_})
    for keys %$lhs;
  delete @diff{grep !defined($diff{$_}), keys %diff};
  return {hash_value_mismatch => 1,
          object_type         => $rl,
          diffs               => \\%diff} if keys %diff;
  return undef;
} elsif ($realtype eq \'ARRAY\') {
  my $n_diff = @$lhs - @$rhs;
  return {array_length_mismatch => $n_diff} if $n_diff;
  my %diff;
  $diff{$_} = ref($self)->new($$lhs[$_])->diff($$rhs[$_])
    for 0..$#{$lhs};
  delete @diff{grep !defined($diff{$_}), keys %diff};
  return {array_value_mismatch => 1,
          object_type          => $rl,
          diffs                => \\%diff} if keys %diff;
  return undef;
} elsif ($realtype eq \'SCALAR\') {
  return ref($self)->new($$lhs)->diff($$rhs);
} elsif (!$rl) {
  return {scalar_difference => [$lhs, $rhs]} unless $lhs eq $rhs;
  return undef;
}';$ei=bless({$l2,$ci,$F1,$di,$H1,$I1},$Z);$fi={$Vh,$Yh,$Zh,$ei};$gi=q'/lib/test_value_eq.b';$hi=bless({$c,$Uh,$t1,$u1,$v1,$u1,$w1,$fi,$f,$gi},$P);$ii=q'/lib/slice::ctors';$ji={};$ki=[];$li=q'ni::json_encode ${$_[0]}';$mi=bless({$l2,$ki,$F1,$li,$H1,$I1},$Z);$ni={$B2,$mi};$oi=q'/lib/test_value_str.b';$pi=bless({$c,$ji,$t1,$u1,$v1,$u1,$w1,$ni,$f,$oi},$P);$qi=q'/lib/slice::ctors';$ri=[$l3,$Sh,$hi,$pi];$si=q'/lib/test_value.c';$ti=bless({$c,$Lh,$f,$e1,$g,$ri},$si);$ui=q'/lib/test_value.c::ctors';$vi=q'ni:/lib/test_value.c';$wi=q'ni:/lib/test_value_eq.b';$xi=q'ni:/lib/test_value_init.b';$yi=q'ni:/lib/test_value_str.b';$zi=q'ni:/metaclass';$Ai=q'ni:/metaclass.c';$Bi=q'ni:/module';$Ci=q'ni:/module.c';$Di=q'ni:/object';$Ei=q'ni:/object.c';$Fi=q'ni:/unix/cat';$Gi={$w,1};$Hi=q'/unix/pipeline.c';$Ii={$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$Hi,1,$D,1};$Ji=[$v4];$Ki=bless({$c,$Ii,$f,$A,$g,$Ji},$J);$Li=q'/metaclass::ctors';$Mi=[$Ki];$Ni=bless({$c,$Gi,$f,$w,$g,$Mi},$J);$Oi=q'/metaclass::ctors';$Pi={$i1,1};$Qi={$i1,1,$j1,1,$k1,1,$l1,1,$m1,1,$n1,1,$o1,1,$q1,1};$Ri={};$Si=q'into';$Ti=[];$Ui=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Vi=bless({$l2,$Ti,$F1,$Ui,$H1,$I1},$Z);$Wi={$Si,$Vi};$Xi=q'/unix/io_stream.b';$Yi=bless({$c,$Ri,$t1,$u1,$v1,$u1,$w1,$Wi,$f,$Xi},$P);$Zi=q'/lib/slice::ctors';$cj={};$dj=q'(+';$ej=[];$fj=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$gj=bless({$l2,$ej,$F1,$fj,$H1,$I1},$Z);$hj={$dj,$gj};$ij=q'/unix/io_constructors.b';$jj=bless({$c,$cj,$t1,$u1,$v1,$u1,$w1,$hj,$f,$ij},$P);$kj=q'/lib/slice::ctors';$lj={};$mj=q'(<>';$nj=[];$oj=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$pj=bless({$l2,$nj,$F1,$oj,$H1,$I1},$Z);$qj=q'(@{}';$rj=[];$sj=q'my $self = shift; [<$self>]';$tj=bless({$l2,$rj,$F1,$sj,$H1,$I1},$Z);$uj={$mj,$pj,$qj,$tj};$vj=q'/unix/io_readers.b';$wj=bless({$c,$lj,$t1,$u1,$v1,$u1,$w1,$uj,$f,$vj},$P);$xj=q'/lib/slice::ctors';$yj=[$l3,$Yi,$jj,$wj];$zj=bless({$c,$Qi,$f,$m1,$g,$yj},$A);$Aj=q'/unix/io.c::ctors';$Bj={};$Cj=[];$Dj=q'shift; +{fs => [@_]}';$Ej=bless({$l2,$Cj,$F1,$Dj,$H1,$I1},$Z);$Fj={$d2,$Ej};$Gj=q'/unix/cat_init.b';$Hj=bless({$c,$Bj,$t1,$u1,$v1,$u1,$w1,$Fj,$f,$Gj},$P);$Ij=q'/lib/slice::ctors';$Jj={};$Kj=q'read';$Lj=[];$Mj=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Nj=bless({$l2,$Lj,$F1,$Mj,$H1,$I1},$Z);$Oj={$Kj,$Nj};$Pj=q'/unix/cat_read.b';$Qj=bless({$c,$Jj,$t1,$u1,$v1,$u1,$w1,$Oj,$f,$Pj},$P);$Rj=q'/lib/slice::ctors';$Sj=[$zj,$Hj,$Qj];$Tj=bless({$c,$Pi,$f,$i1,$g,$Sj},$w);$Uj=q'/unix/cat.c::ctors';$Vj=q'ni:/unix/cat.c';$Wj=q'ni:/unix/cat_init.b';$Xj=q'ni:/unix/cat_read.b';$Yj=q'ni:/unix/fd';$Zj={$x,1};$ck=[$Ki];$dk=bless({$c,$Zj,$f,$x,$g,$ck},$J);$ek=q'/metaclass::ctors';$fk={$j1,1};$gk={};$hk=q'fd';$ik=[];$jk=q'shift->{\'fd\'}';$kk=bless({$l2,$ik,$F1,$jk,$H1,$I1},$Z);$lk={$hk,$kk};$mk=q'/unix/fd_readers.b';$nk=bless({$c,$gk,$t1,$u1,$v1,$u1,$w1,$lk,$f,$mk},$P);$ok=q'/lib/slice::ctors';$pk={};$qk=[];$rk=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$sk=bless({$l2,$qk,$F1,$rk,$H1,$I1},$Z);$tk={$d2,$sk};$uk=q'/unix/fd_init.b';$vk=bless({$c,$pk,$t1,$u1,$v1,$u1,$w1,$tk,$f,$uk},$P);$wk=q'/lib/slice::ctors';$xk={};$yk=q'move_to';$zk=[];$Ak=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Bk=bless({$l2,$zk,$F1,$Ak,$H1,$I1},$Z);$Ck={$yk,$Bk};$Dk=q'/unix/fd_shell.b';$Ek=bless({$c,$xk,$t1,$u1,$v1,$u1,$w1,$Ck,$f,$Dk},$P);$Fk=q'/lib/slice::ctors';$Gk={$j1,1,$k1,1,$l1,1,$n1,1,$o1,1};$Hk=q'/unix/has_fd.b';$Ik={};$Jk=[];$Kk=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Lk=bless({$l2,$Jk,$F1,$Kk,$H1,$I1},$Z);$Mk=[];$Nk=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Ok=bless({$l2,$Mk,$F1,$Nk,$H1,$I1},$Z);$Pk={$Kj,$Lk,$Te,$Ok};$Qk=q'/unix/fd_safeio.b';$Rk=bless({$c,$Ik,$t1,$u1,$v1,$u1,$w1,$Pk,$f,$Qk},$P);$Sk=q'/lib/slice::ctors';$Tk=[$Rk];$Uk=bless({$c,$Gk,$f,$Hk,$g,$Tk},$S);$Vk=q'/lib/branch::ctors';$Wk={};$Xk=q'read_fh';$Yk=[];$Zk=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$cl=bless({$l2,$Yk,$F1,$Zk,$H1,$I1},$Z);$dl=q'write_fh';$el=[];$fl=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$gl=bless({$l2,$el,$F1,$fl,$H1,$I1},$Z);$hl={$Xk,$cl,$dl,$gl};$il=q'/unix/fd_io.b';$jl=bless({$c,$Wk,$t1,$u1,$v1,$u1,$w1,$hl,$f,$il},$P);$kl=q'/lib/slice::ctors';$ll=[$zj,$nk,$vk,$Ek,$Uk,$jl];$ml=bless({$c,$fk,$f,$j1,$g,$ll},$x);$nl=q'/unix/fd.c::ctors';$ol=q'ni:/unix/fd.c';$pl=q'ni:/unix/fd_init.b';$ql=q'ni:/unix/fd_io.b';$rl=q'ni:/unix/fd_readers.b';$sl=q'ni:/unix/fd_safeio.b';$tl=q'ni:/unix/fd_shell.b';$ul=q'ni:/unix/fifo';$vl={$y,1};$wl=[$Ki];$xl=bless({$c,$vl,$f,$y,$g,$wl},$J);$yl=q'/metaclass::ctors';$zl={$k1,1};$Al={};$Bl=[];$Cl=q'shift->{\'read_fh\'}';$Dl=bless({$l2,$Bl,$F1,$Cl,$H1,$I1},$Z);$El=[];$Fl=q'shift->{\'write_fh\'}';$Gl=bless({$l2,$El,$F1,$Fl,$H1,$I1},$Z);$Hl={$Xk,$Dl,$dl,$Gl};$Il=q'/unix/fifo_io.b';$Jl=bless({$c,$Al,$t1,$u1,$v1,$u1,$w1,$Hl,$f,$Il},$P);$Kl=q'/lib/slice::ctors';$Ll={};$Ml=[];$Nl=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Ol=bless({$l2,$Ml,$F1,$Nl,$H1,$I1},$Z);$Pl={$d2,$Ol};$Ql=q'/unix/fifo_init.b';$Rl=bless({$c,$Ll,$t1,$u1,$v1,$u1,$w1,$Pl,$f,$Ql},$P);$Sl=q'/lib/slice::ctors';$Tl={};$Ul=q'read_side';$Vl=[];$Wl=q'my $self = shift; close $$self{write_fh}; $self';$Xl=bless({$l2,$Vl,$F1,$Wl,$H1,$I1},$Z);$Yl=q'write_side';$Zl=[];$cm=q'my $self = shift; close $$self{read_fh};  $self';$dm=bless({$l2,$Zl,$F1,$cm,$H1,$I1},$Z);$em={$Ul,$Xl,$Yl,$dm};$fm=q'/unix/fifo_direction.b';$gm=bless({$c,$Tl,$t1,$u1,$v1,$u1,$w1,$em,$f,$fm},$P);$hm=q'/lib/slice::ctors';$im=[$zj,$Jl,$Rl,$Uk,$gm];$jm=bless({$c,$zl,$f,$k1,$g,$im},$y);$km=q'/unix/fifo.c::ctors';$lm=q'ni:/unix/fifo.c';$mm=q'ni:/unix/fifo_direction.b';$nm=q'ni:/unix/fifo_init.b';$om=q'ni:/unix/fifo_io.b';$pm=q'ni:/unix/file';$qm={$z,1};$rm=[$Ki];$sm=bless({$c,$qm,$f,$z,$g,$rm},$J);$tm=q'/metaclass::ctors';$um={$l1,1};$vm={};$wm=[];$xm=q'shift->{\'name\'}';$ym=bless({$l2,$wm,$F1,$xm,$H1,$I1},$Z);$zm={$f,$ym};$Am=q'/unix/file_readers.b';$Bm=bless({$c,$vm,$t1,$u1,$v1,$u1,$w1,$zm,$f,$Am},$P);$Cm=q'/lib/slice::ctors';$Dm={};$Em=[];$Fm=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Gm=bless({$l2,$Em,$F1,$Fm,$H1,$I1},$Z);$Hm={$d2,$Gm};$Im=q'/unix/file_init.b';$Jm=bless({$c,$Dm,$t1,$u1,$v1,$u1,$w1,$Hm,$f,$Im},$P);$Km=q'/lib/slice::ctors';$Lm={};$Mm=[];$Nm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Om=bless({$l2,$Mm,$F1,$Nm,$H1,$I1},$Z);$Pm=[];$Qm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Rm=bless({$l2,$Pm,$F1,$Qm,$H1,$I1},$Z);$Sm={$Xk,$Om,$dl,$Rm};$Tm=q'/unix/file_io.b';$Um=bless({$c,$Lm,$t1,$u1,$v1,$u1,$w1,$Sm,$f,$Tm},$P);$Vm=q'/lib/slice::ctors';$Wm=[$zj,$Bm,$Jm,$Uk,$Um];$Xm=bless({$c,$um,$f,$l1,$g,$Wm},$z);$Ym=q'/unix/file.c::ctors';$Zm=q'ni:/unix/file.c';$cn=q'ni:/unix/file_init.b';$dn=q'ni:/unix/file_io.b';$en=q'ni:/unix/file_readers.b';$fn=q'ni:/unix/has_fd.b';$gn=q'ni:/unix/io';$hn=q'ni:/unix/io.c';$in=q'ni:/unix/io_constructors.b';$jn=q'ni:/unix/io_readers.b';$kn=q'ni:/unix/io_stream.b';$ln=q'ni:/unix/pid';$mn={$B,1};$nn=[$Ki];$on=bless({$c,$mn,$f,$B,$g,$nn},$J);$pn=q'/metaclass::ctors';$qn={$n1,1};$rn={};$sn=q'pid';$tn=[];$un=q'shift->{\'pid\'}';$vn=bless({$l2,$tn,$F1,$un,$H1,$I1},$Z);$wn=q'stderr';$xn=[];$yn=q'shift->{\'stderr\'}';$zn=bless({$l2,$xn,$F1,$yn,$H1,$I1},$Z);$An=q'stdin';$Bn=[];$Cn=q'shift->{\'stdin\'}';$Dn=bless({$l2,$Bn,$F1,$Cn,$H1,$I1},$Z);$En=q'stdout';$Fn=[];$Gn=q'shift->{\'stdout\'}';$Hn=bless({$l2,$Fn,$F1,$Gn,$H1,$I1},$Z);$In={$sn,$vn,$wn,$zn,$An,$Dn,$En,$Hn};$Jn=q'/unix/pid_readers.b';$Kn=bless({$c,$rn,$t1,$u1,$v1,$u1,$w1,$In,$f,$Jn},$P);$Ln=q'/lib/slice::ctors';$Mn={};$Nn=[];$On=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Pn=bless({$l2,$Nn,$F1,$On,$H1,$I1},$Z);$Qn={$d2,$Pn};$Rn=q'/unix/pid_init.b';$Sn=bless({$c,$Mn,$t1,$u1,$v1,$u1,$w1,$Qn,$f,$Rn},$P);$Tn=q'/lib/slice::ctors';$Un={};$Vn={};$Wn=q'/unix/pid_wait.b';$Xn=bless({$c,$Un,$t1,$u1,$v1,$u1,$w1,$Vn,$f,$Wn},$P);$Yn=q'/lib/slice::ctors';$Zn={};$co=[];$do=q'shift->{stdout}->read_fh';$eo=bless({$l2,$co,$F1,$do,$H1,$I1},$Z);$fo=[];$go=q'shift->{stdin}->write_fh';$ho=bless({$l2,$fo,$F1,$go,$H1,$I1},$Z);$io={$Xk,$eo,$dl,$ho};$jo=q'/unix/pid_io.b';$ko=bless({$c,$Zn,$t1,$u1,$v1,$u1,$w1,$io,$f,$jo},$P);$lo=q'/lib/slice::ctors';$mo=[$zj,$Kn,$Sn,$Xn,$Uk,$ko];$no=bless({$c,$qn,$f,$n1,$g,$mo},$B);$oo=q'/unix/pid.c::ctors';$po=q'ni:/unix/pid.c';$qo=q'ni:/unix/pid_init.b';$ro=q'ni:/unix/pid_io.b';$so=q'ni:/unix/pid_readers.b';$to=q'ni:/unix/pid_wait.b';$uo=q'ni:/unix/pipeline';$vo=q'/unix/pipeline.c';$wo={$vo,1};$xo=q'/unix/pipeline.c';$yo=[$Ki];$zo=bless({$c,$wo,$f,$xo,$g,$yo},$J);$Ao=q'/metaclass::ctors';$Bo={$o1,1};$Co={};$Do=[];$Eo=q'shift->{\'stdin\'}';$Fo=bless({$l2,$Do,$F1,$Eo,$H1,$I1},$Z);$Go=[];$Ho=q'shift->{\'stdout\'}';$Io=bless({$l2,$Go,$F1,$Ho,$H1,$I1},$Z);$Jo={$An,$Fo,$En,$Io};$Ko=q'/unix/pipeline_ro.b';$Lo=bless({$c,$Co,$t1,$u1,$v1,$u1,$w1,$Jo,$f,$Ko},$P);$Mo=q'/lib/slice::ctors';$No={};$Oo=[];$Po=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Qo=bless({$l2,$Oo,$F1,$Po,$H1,$I1},$Z);$Ro={$d2,$Qo};$So=q'/unix/pipeline_init.b';$To=bless({$c,$No,$t1,$u1,$v1,$u1,$w1,$Ro,$f,$So},$P);$Uo=q'/lib/slice::ctors';$Vo={};$Wo=q'async_step';$Xo=[];$Yo=q'local $_;
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
$self;';$Zo=bless({$l2,$Xo,$F1,$Yo,$H1,$I1},$Z);$cp={$Wo,$Zo};$dp=q'/unix/pipeline_async.b';$ep=bless({$c,$Vo,$t1,$u1,$v1,$u1,$w1,$cp,$f,$dp},$P);$fp=q'/lib/slice::ctors';$gp={};$hp=[];$ip=q'shift->{stdout}->read_fh';$jp=bless({$l2,$hp,$F1,$ip,$H1,$I1},$Z);$kp=[];$lp=q'shift->{stdin}->write_fh';$mp=bless({$l2,$kp,$F1,$lp,$H1,$I1},$Z);$np={$Xk,$jp,$dl,$mp};$op=q'/unix/pipeline_io.b';$pp=bless({$c,$gp,$t1,$u1,$v1,$u1,$w1,$np,$f,$op},$P);$qp=q'/lib/slice::ctors';$rp=[$zj,$Lo,$To,$ep,$Uk,$pp];$sp=q'/unix/pipeline.c';$tp=bless({$c,$Bo,$f,$o1,$g,$rp},$sp);$up=q'/unix/pipeline.c::ctors';$vp=q'ni:/unix/pipeline.c';$wp=q'ni:/unix/pipeline_async.b';$xp=q'ni:/unix/pipeline_init.b';$yp=q'ni:/unix/pipeline_io.b';$zp=q'ni:/unix/pipeline_ro.b';$Ap=q'ni:/unix/str';$Bp={$D,1};$Cp=[$Ki];$Dp=bless({$c,$Bp,$f,$D,$g,$Cp},$J);$Ep=q'/metaclass::ctors';$Fp={$q1,1};$Gp={};$Hp=[];$Ip=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Jp=bless({$l2,$Hp,$F1,$Ip,$H1,$I1},$Z);$Kp={$d2,$Jp};$Lp=q'/unix/str_init.b';$Mp=bless({$c,$Gp,$t1,$u1,$v1,$u1,$w1,$Kp,$f,$Lp},$P);$Np=q'/lib/slice::ctors';$Op={};$Pp=[];$Qp=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Rp=bless({$l2,$Pp,$F1,$Qp,$H1,$I1},$Z);$Sp=q'remaining';$Tp=[];$Up=q'my $self = shift; $$self{end} - $$self{start}';$Vp=bless({$l2,$Tp,$F1,$Up,$H1,$I1},$Z);$Wp=[];$Xp=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Yp=bless({$l2,$Wp,$F1,$Xp,$H1,$I1},$Z);$Zp={$Kj,$Rp,$Sp,$Vp,$Te,$Yp};$cq=q'/unix/str_io.b';$dq=bless({$c,$Op,$t1,$u1,$v1,$u1,$w1,$Zp,$f,$cq},$P);$eq=q'/lib/slice::ctors';$fq=[$zj,$Mp,$dq];$gq=bless({$c,$Fp,$f,$q1,$g,$fq},$D);$hq=q'/unix/str.c::ctors';$iq=q'ni:/unix/str.c';$jq=q'ni:/unix/str_init.b';$kq=q'ni:/unix/str_io.b';$lq=q'ni:main';$mq={$kd,1};$nq=[$xh,$jd];$oq=bless({$c,$mq,$f,$kd,$g,$nq},$g1);$pq=q'/module::ctors';$qq=q'ni:ni';$rq={$Kf,1};$sq={$Kf,1};$tq=q'data';$uq=q'json_escapes';$vq=q'';$wq=q'b';$xq=q'	';$yq=q't';$zq=q'
';$Aq=q'n';$Bq=q'';$Cq=q'r';$Dq=q'"';$Eq=q'/';$Fq=q'\\';$Gq={$vq,$wq,$xq,$yq,$zq,$Aq,$Bq,$Cq,$Dq,$Dq,$Eq,$Eq,$Fq,$Fq};$Hq=q'json_unescapes';$Iq={$Dq,$Dq,$Eq,$Eq,$Fq,$Fq,$wq,$vq,$Aq,$zq,$Cq,$Bq,$yq,$xq};$Jq={$uq,$Gq,$Hq,$Iq};$Kq=q'/lib/json_data.b';$Lq=bless({$c,$sq,$tq,$Jq,$f,$Kq},$T);$Mq=q'/lib/dataslice::ctors';$Nq=[$Lq,$Jf,$Zg];$Oq=bless({$c,$rq,$f,$Kf,$g,$Nq},$g1);$Pq={$ja,$pb,$rb,$Fb,$Gb,$Sb,$Tb,$p8,$Ub,$B4,$Vb,$l7,$Wb,$o3,$Xb,$K,$Yb,$I6,$Zb,$s5,$cc,$x6,$dc,$F6,$ec,$q6,$fc,$Dc,$Fc,$kc,$Gc,$zc,$Hc,$sc,$Ic,$W7,$Jc,$V6,$Kc,$Q7,$Lc,$Xa,$Mc,$ma,$Nc,$Ja,$Oc,$ua,$Pc,$Ba,$Qc,$Ua,$Rc,$p4,$Sc,$X2,$Tc,$A1,$Uc,$i2,$Vc,$L2,$Wc,$y2,$Xc,$U2,$Yc,$jd,$md,$ef,$gf,$pd,$hf,$yd,$if,$Ze,$jf,$f3,$kf,$Q1,$lf,$Jf,$Mf,$v7,$Nf,$x3,$Of,$D5,$Pf,$O5,$Qf,$ga,$Rf,$A8,$Sf,$d9,$Tf,$C9,$Uf,$da,$Vf,$O9,$Wf,$O8,$Xf,$Zg,$dh,$h6,$eh,$F7,$fh,$Z5,$gh,$h4,$hh,$N3,$ih,$N,$jh,$U3,$kh,$e4,$lh,$xh,$zh,$j8,$Ah,$h5,$Bh,$V4,$Ch,$N4,$Dh,$e5,$Eh,$ti,$vi,$Jh,$wi,$hi,$xi,$Sh,$yi,$pi,$zi,$w8,$Ai,$s8,$Bi,$Z7,$Ci,$y4,$Di,$l3,$Ei,$v4,$Fi,$Tj,$Vj,$Ni,$Wj,$Hj,$Xj,$Qj,$Yj,$ml,$ol,$dk,$pl,$vk,$ql,$jl,$rl,$nk,$sl,$Rk,$tl,$Ek,$ul,$jm,$lm,$xl,$mm,$gm,$nm,$Rl,$om,$Jl,$pm,$Xm,$Zm,$sm,$cn,$Jm,$dn,$Um,$en,$Bm,$fn,$Uk,$gn,$zj,$hn,$Ki,$in,$jj,$jn,$wj,$kn,$Yi,$ln,$no,$po,$on,$qo,$Sn,$ro,$ko,$so,$Kn,$to,$Xn,$uo,$tp,$vp,$zo,$wp,$ep,$xp,$To,$yp,$pp,$zp,$Lo,$Ap,$gq,$iq,$Dp,$jq,$Mp,$kq,$dq,$lq,$oq,$qq,$Oq};$Qq=q'resolvers';$Rq=[];$Sq=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$Tq=bless({$l2,$Rq,$F1,$Sq,$H1,$I1},$Z);$Uq=q'file';$Vq=[];$Wq=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$Xq=bless({$l2,$Vq,$F1,$Wq,$H1,$I1},$Z);$Yq=q'str';$Zq=[];$cr=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$dr=bless({$l2,$Zq,$F1,$cr,$H1,$I1},$Z);$er={$hk,$Tq,$Uq,$Xq,$Yq,$dr};$fr=bless({$ia,$Pq,$Qq,$er},$d1);$gr=q'/lib/ni::ctors';$$u4[0]=$p8;$$I[0]=$v4;$$z1[0]=$v4;$$W2[0]=$l3;$$H6[4]=$W7;*$H3=\&$F3;*$G3=\&$D3;$Q1->apply_unsafe($W);$Q1->apply_unsafe($h);$Q1->apply_unsafe($l);$Q1->apply_unsafe($m);$Q1->apply_unsafe($R1);$Q1->apply_unsafe($o);$Q1->apply_unsafe($Z);$Q1->apply_unsafe($p);$Q1->apply_unsafe($q);$Q1->apply_unsafe($r);$Q1->apply_unsafe($P);$Q1->apply_unsafe($s);$Q1->apply_unsafe($U);$Q1->apply_unsafe($t);$Q1->apply_unsafe($S1);$Q1->apply_unsafe($J);$Q1->apply_unsafe($d);$Q1->apply_unsafe($j);$Q1->apply_unsafe($v);$Q1->apply_unsafe($w);$Q1->apply_unsafe($x);$Q1->apply_unsafe($y);$Q1->apply_unsafe($z);$Q1->apply_unsafe($A);$Q1->apply_unsafe($B);$Q1->apply_unsafe($T1);$Q1->apply_unsafe($D);$i2->apply_unsafe($Z);$y2->apply_unsafe($Z);$L2->apply_unsafe($Z);$U2->apply_unsafe($Z);$f3->apply_unsafe($W);$f3->apply_unsafe($h);$f3->apply_unsafe($R);$f3->apply_unsafe($l);$f3->apply_unsafe($S);$f3->apply_unsafe($m);$f3->apply_unsafe($T);$f3->apply_unsafe($g3);$f3->apply_unsafe($Y);$f3->apply_unsafe($o);$f3->apply_unsafe($Z);$f3->apply_unsafe($p);$f3->apply_unsafe($c1);$f3->apply_unsafe($q);$f3->apply_unsafe($d1);$f3->apply_unsafe($r);$f3->apply_unsafe($P);$f3->apply_unsafe($s);$f3->apply_unsafe($U);$f3->apply_unsafe($t);$f3->apply_unsafe($e1);$f3->apply_unsafe($h3);$f3->apply_unsafe($J);$f3->apply_unsafe($d);$f3->apply_unsafe($g1);$f3->apply_unsafe($j);$f3->apply_unsafe($h1);$f3->apply_unsafe($v);$f3->apply_unsafe($i1);$f3->apply_unsafe($w);$f3->apply_unsafe($j1);$f3->apply_unsafe($x);$f3->apply_unsafe($k1);$f3->apply_unsafe($y);$f3->apply_unsafe($l1);$f3->apply_unsafe($z);$f3->apply_unsafe($m1);$f3->apply_unsafe($A);$f3->apply_unsafe($n1);$f3->apply_unsafe($B);$f3->apply_unsafe($o1);$f3->apply_unsafe($i3);$f3->apply_unsafe($q1);$f3->apply_unsafe($D);$x3->apply_unsafe($W);$x3->apply_unsafe($h);$x3->apply_unsafe($l);$x3->apply_unsafe($S);$x3->apply_unsafe($m);$x3->apply_unsafe($y3);$x3->apply_unsafe($Y);$x3->apply_unsafe($o);$x3->apply_unsafe($p);$x3->apply_unsafe($q);$x3->apply_unsafe($r);$x3->apply_unsafe($P);$x3->apply_unsafe($s);$x3->apply_unsafe($U);$x3->apply_unsafe($t);$x3->apply_unsafe($z3);$x3->apply_unsafe($J);$x3->apply_unsafe($d);$x3->apply_unsafe($g1);$x3->apply_unsafe($j);$x3->apply_unsafe($v);$x3->apply_unsafe($w);$x3->apply_unsafe($x);$x3->apply_unsafe($y);$x3->apply_unsafe($z);$x3->apply_unsafe($A);$x3->apply_unsafe($B);$x3->apply_unsafe($A3);$x3->apply_unsafe($D);$N3->apply_unsafe($P);$U3->apply_unsafe($P);$e4->apply_unsafe($P);$p4->apply_unsafe($h);$p4->apply_unsafe($l);$p4->apply_unsafe($m);$p4->apply_unsafe($q4);$p4->apply_unsafe($o);$p4->apply_unsafe($p);$p4->apply_unsafe($q);$p4->apply_unsafe($r);$p4->apply_unsafe($s);$p4->apply_unsafe($t);$p4->apply_unsafe($r4);$p4->apply_unsafe($j);$p4->apply_unsafe($v);$p4->apply_unsafe($w);$p4->apply_unsafe($x);$p4->apply_unsafe($y);$p4->apply_unsafe($z);$p4->apply_unsafe($A);$p4->apply_unsafe($B);$p4->apply_unsafe($s4);$p4->apply_unsafe($D);$V4->apply_unsafe($U);$e5->apply_unsafe($U);$s5->apply_unsafe($W);$s5->apply_unsafe($h);$s5->apply_unsafe($l);$s5->apply_unsafe($S);$s5->apply_unsafe($m);$s5->apply_unsafe($t5);$s5->apply_unsafe($o);$s5->apply_unsafe($p);$s5->apply_unsafe($q);$s5->apply_unsafe($r);$s5->apply_unsafe($s);$s5->apply_unsafe($t);$s5->apply_unsafe($u5);$s5->apply_unsafe($J);$s5->apply_unsafe($d);$s5->apply_unsafe($g1);$s5->apply_unsafe($j);$s5->apply_unsafe($v);$s5->apply_unsafe($w);$s5->apply_unsafe($x);$s5->apply_unsafe($y);$s5->apply_unsafe($z);$s5->apply_unsafe($A);$s5->apply_unsafe($B);$s5->apply_unsafe($v5);$s5->apply_unsafe($D);$D5->apply_unsafe($W);$D5->apply_unsafe($h);$D5->apply_unsafe($l);$D5->apply_unsafe($S);$D5->apply_unsafe($m);$D5->apply_unsafe($E5);$D5->apply_unsafe($o);$D5->apply_unsafe($p);$D5->apply_unsafe($q);$D5->apply_unsafe($r);$D5->apply_unsafe($P);$D5->apply_unsafe($s);$D5->apply_unsafe($U);$D5->apply_unsafe($t);$D5->apply_unsafe($F5);$D5->apply_unsafe($J);$D5->apply_unsafe($d);$D5->apply_unsafe($g1);$D5->apply_unsafe($j);$D5->apply_unsafe($v);$D5->apply_unsafe($w);$D5->apply_unsafe($x);$D5->apply_unsafe($y);$D5->apply_unsafe($z);$D5->apply_unsafe($A);$D5->apply_unsafe($B);$D5->apply_unsafe($G5);$D5->apply_unsafe($D);$O5->apply_unsafe($W);$O5->apply_unsafe($h);$O5->apply_unsafe($l);$O5->apply_unsafe($S);$O5->apply_unsafe($m);$O5->apply_unsafe($P5);$O5->apply_unsafe($o);$O5->apply_unsafe($p);$O5->apply_unsafe($q);$O5->apply_unsafe($r);$O5->apply_unsafe($P);$O5->apply_unsafe($s);$O5->apply_unsafe($U);$O5->apply_unsafe($t);$O5->apply_unsafe($Q5);$O5->apply_unsafe($J);$O5->apply_unsafe($d);$O5->apply_unsafe($g1);$O5->apply_unsafe($j);$O5->apply_unsafe($v);$O5->apply_unsafe($w);$O5->apply_unsafe($x);$O5->apply_unsafe($y);$O5->apply_unsafe($z);$O5->apply_unsafe($A);$O5->apply_unsafe($B);$O5->apply_unsafe($R5);$O5->apply_unsafe($D);$Z5->apply_unsafe($W);$Z5->apply_unsafe($h);$Z5->apply_unsafe($l);$Z5->apply_unsafe($S);$Z5->apply_unsafe($m);$Z5->apply_unsafe($c6);$Z5->apply_unsafe($o);$Z5->apply_unsafe($p);$Z5->apply_unsafe($q);$Z5->apply_unsafe($r);$Z5->apply_unsafe($s);$Z5->apply_unsafe($U);$Z5->apply_unsafe($t);$Z5->apply_unsafe($d6);$Z5->apply_unsafe($J);$Z5->apply_unsafe($d);$Z5->apply_unsafe($g1);$Z5->apply_unsafe($j);$Z5->apply_unsafe($v);$Z5->apply_unsafe($w);$Z5->apply_unsafe($x);$Z5->apply_unsafe($y);$Z5->apply_unsafe($z);$Z5->apply_unsafe($A);$Z5->apply_unsafe($B);$Z5->apply_unsafe($e6);$Z5->apply_unsafe($D);$q6->apply_unsafe($W);$q6->apply_unsafe($h);$q6->apply_unsafe($l);$q6->apply_unsafe($m);$q6->apply_unsafe($r6);$q6->apply_unsafe($o);$q6->apply_unsafe($p);$q6->apply_unsafe($q);$q6->apply_unsafe($r);$q6->apply_unsafe($s);$q6->apply_unsafe($t);$q6->apply_unsafe($s6);$q6->apply_unsafe($J);$q6->apply_unsafe($d);$q6->apply_unsafe($g1);$q6->apply_unsafe($j);$q6->apply_unsafe($v);$q6->apply_unsafe($w);$q6->apply_unsafe($x);$q6->apply_unsafe($y);$q6->apply_unsafe($z);$q6->apply_unsafe($A);$q6->apply_unsafe($B);$q6->apply_unsafe($t6);$q6->apply_unsafe($D);$F6->apply_unsafe($S);$V6->apply_unsafe($W);$V6->apply_unsafe($h);$V6->apply_unsafe($l);$V6->apply_unsafe($S);$V6->apply_unsafe($m);$V6->apply_unsafe($W6);$V6->apply_unsafe($o);$V6->apply_unsafe($p);$V6->apply_unsafe($q);$V6->apply_unsafe($r);$V6->apply_unsafe($s);$V6->apply_unsafe($t);$V6->apply_unsafe($X6);$V6->apply_unsafe($J);$V6->apply_unsafe($d);$V6->apply_unsafe($g1);$V6->apply_unsafe($j);$V6->apply_unsafe($v);$V6->apply_unsafe($w);$V6->apply_unsafe($x);$V6->apply_unsafe($y);$V6->apply_unsafe($z);$V6->apply_unsafe($A);$V6->apply_unsafe($B);$V6->apply_unsafe($Y6);$V6->apply_unsafe($D);$l7->apply_unsafe($W);$l7->apply_unsafe($h);$l7->apply_unsafe($l);$l7->apply_unsafe($S);$l7->apply_unsafe($m);$l7->apply_unsafe($m7);$l7->apply_unsafe($o);$l7->apply_unsafe($p);$l7->apply_unsafe($q);$l7->apply_unsafe($r);$l7->apply_unsafe($s);$l7->apply_unsafe($t);$l7->apply_unsafe($n7);$l7->apply_unsafe($J);$l7->apply_unsafe($d);$l7->apply_unsafe($g1);$l7->apply_unsafe($j);$l7->apply_unsafe($v);$l7->apply_unsafe($w);$l7->apply_unsafe($x);$l7->apply_unsafe($y);$l7->apply_unsafe($z);$l7->apply_unsafe($A);$l7->apply_unsafe($B);$l7->apply_unsafe($o7);$l7->apply_unsafe($D);$v7->apply_unsafe($W);$v7->apply_unsafe($h);$v7->apply_unsafe($l);$v7->apply_unsafe($S);$v7->apply_unsafe($m);$v7->apply_unsafe($w7);$v7->apply_unsafe($o);$v7->apply_unsafe($p);$v7->apply_unsafe($q);$v7->apply_unsafe($r);$v7->apply_unsafe($s);$v7->apply_unsafe($t);$v7->apply_unsafe($x7);$v7->apply_unsafe($J);$v7->apply_unsafe($d);$v7->apply_unsafe($g1);$v7->apply_unsafe($j);$v7->apply_unsafe($v);$v7->apply_unsafe($w);$v7->apply_unsafe($x);$v7->apply_unsafe($y);$v7->apply_unsafe($z);$v7->apply_unsafe($A);$v7->apply_unsafe($B);$v7->apply_unsafe($y7);$v7->apply_unsafe($D);$F7->apply_unsafe($W);$F7->apply_unsafe($h);$F7->apply_unsafe($l);$F7->apply_unsafe($S);$F7->apply_unsafe($m);$F7->apply_unsafe($G7);$F7->apply_unsafe($o);$F7->apply_unsafe($p);$F7->apply_unsafe($q);$F7->apply_unsafe($r);$F7->apply_unsafe($s);$F7->apply_unsafe($t);$F7->apply_unsafe($H7);$F7->apply_unsafe($J);$F7->apply_unsafe($d);$F7->apply_unsafe($g1);$F7->apply_unsafe($j);$F7->apply_unsafe($v);$F7->apply_unsafe($w);$F7->apply_unsafe($x);$F7->apply_unsafe($y);$F7->apply_unsafe($z);$F7->apply_unsafe($A);$F7->apply_unsafe($B);$F7->apply_unsafe($I7);$F7->apply_unsafe($D);$Q7->apply_unsafe($W);$Q7->apply_unsafe($h);$Q7->apply_unsafe($l);$Q7->apply_unsafe($S);$Q7->apply_unsafe($m);$Q7->apply_unsafe($R7);$Q7->apply_unsafe($o);$Q7->apply_unsafe($p);$Q7->apply_unsafe($q);$Q7->apply_unsafe($r);$Q7->apply_unsafe($s);$Q7->apply_unsafe($t);$Q7->apply_unsafe($S7);$Q7->apply_unsafe($J);$Q7->apply_unsafe($d);$Q7->apply_unsafe($g1);$Q7->apply_unsafe($j);$Q7->apply_unsafe($v);$Q7->apply_unsafe($w);$Q7->apply_unsafe($x);$Q7->apply_unsafe($y);$Q7->apply_unsafe($z);$Q7->apply_unsafe($A);$Q7->apply_unsafe($B);$Q7->apply_unsafe($T7);$Q7->apply_unsafe($D);$j8->apply_unsafe($W);$j8->apply_unsafe($h);$j8->apply_unsafe($l);$j8->apply_unsafe($m);$j8->apply_unsafe($k8);$j8->apply_unsafe($o);$j8->apply_unsafe($p);$j8->apply_unsafe($q);$j8->apply_unsafe($r);$j8->apply_unsafe($s);$j8->apply_unsafe($t);$j8->apply_unsafe($l8);$j8->apply_unsafe($d);$j8->apply_unsafe($j);$j8->apply_unsafe($v);$j8->apply_unsafe($w);$j8->apply_unsafe($x);$j8->apply_unsafe($y);$j8->apply_unsafe($z);$j8->apply_unsafe($A);$j8->apply_unsafe($B);$j8->apply_unsafe($m8);$j8->apply_unsafe($D);$O8->apply_unsafe($d1);$d9->apply_unsafe($d1);$C9->apply_unsafe($d1);$O9->apply_unsafe($d1);$da->apply_unsafe($d1);$ua->apply_unsafe($Y);$Ba->apply_unsafe($Y);$Ja->apply_unsafe($Y);$Ua->apply_unsafe($Y);$sc->apply_unsafe($T);$zc->apply_unsafe($T);$jd->apply_unsafe($kd);$yd->apply_unsafe($c1);$Ze->apply_unsafe($c1);$Jf->apply_unsafe($Kf);$Zg->apply_unsafe($Kf);$xh->apply_unsafe($kd);$Sh->apply_unsafe($e1);$hi->apply_unsafe($e1);$pi->apply_unsafe($e1);$Yi->apply_unsafe($i1);$Yi->apply_unsafe($j1);$Yi->apply_unsafe($k1);$Yi->apply_unsafe($l1);$Yi->apply_unsafe($m1);$Yi->apply_unsafe($n1);$Yi->apply_unsafe($o1);$Yi->apply_unsafe($q1);$jj->apply_unsafe($i1);$jj->apply_unsafe($j1);$jj->apply_unsafe($k1);$jj->apply_unsafe($l1);$jj->apply_unsafe($m1);$jj->apply_unsafe($n1);$jj->apply_unsafe($o1);$jj->apply_unsafe($q1);$wj->apply_unsafe($i1);$wj->apply_unsafe($j1);$wj->apply_unsafe($k1);$wj->apply_unsafe($l1);$wj->apply_unsafe($m1);$wj->apply_unsafe($n1);$wj->apply_unsafe($o1);$wj->apply_unsafe($q1);$Hj->apply_unsafe($i1);$Qj->apply_unsafe($i1);$nk->apply_unsafe($j1);$vk->apply_unsafe($j1);$Ek->apply_unsafe($j1);$Rk->apply_unsafe($j1);$Rk->apply_unsafe($k1);$Rk->apply_unsafe($l1);$Rk->apply_unsafe($n1);$Rk->apply_unsafe($o1);$jl->apply_unsafe($j1);$Jl->apply_unsafe($k1);$Rl->apply_unsafe($k1);$gm->apply_unsafe($k1);$Bm->apply_unsafe($l1);$Jm->apply_unsafe($l1);$Um->apply_unsafe($l1);$Kn->apply_unsafe($n1);$Sn->apply_unsafe($n1);$Xn->apply_unsafe($n1);$ko->apply_unsafe($n1);$Lo->apply_unsafe($o1);$To->apply_unsafe($o1);$ep->apply_unsafe($o1);$pp->apply_unsafe($o1);$Mp->apply_unsafe($q1);$dq->apply_unsafe($q1);$ni::self=$fr;&$_($K)for@$L;&$_($N)for@$O;&$_($A1)for@$B1;&$_($J1)for@$K1;&$_($N1)for@$K1;&$_($Q1)for@$U1;&$_($X1)for@$K1;&$_($c2)for@$K1;&$_($f2)for@$K1;&$_($i2)for@$j2;&$_($o2)for@$K1;&$_($r2)for@$K1;&$_($v2)for@$K1;&$_($y2)for@$z2;&$_($E2)for@$K1;&$_($I2)for@$K1;&$_($L2)for@$M2;&$_($R2)for@$K1;&$_($U2)for@$V2;&$_($X2)for@$Y2;&$_($c3)for@$K1;&$_($f3)for@$j3;&$_($l3)for@$m3;&$_($o3)for@$p3;&$_($s3)for@$K1;&$_($u3)for@$K1;&$_($x3)for@$B3;&$_($D3)for@$K1;&$_($F3)for@$K1;&$_($N3)for@$O3;&$_($R3)for@$K1;&$_($U3)for@$V3;&$_($Z3)for@$K1;&$_($e4)for@$f4;&$_($h4)for@$i4;&$_($m4)for@$K1;&$_($p4)for@$t4;&$_($v4)for@$w4;&$_($y4)for@$z4;&$_($B4)for@$C4;&$_($N4)for@$O4;&$_($S4)for@$K1;&$_($V4)for@$W4;&$_($Z4)for@$K1;&$_($e5)for@$f5;&$_($h5)for@$i5;&$_($n5)for@$K1;&$_($p5)for@$K1;&$_($s5)for@$w5;&$_($A5)for@$K1;&$_($D5)for@$H5;&$_($L5)for@$K1;&$_($O5)for@$S5;&$_($W5)for@$K1;&$_($Z5)for@$f6;&$_($h6)for@$i6;&$_($l6)for@$K1;&$_($n6)for@$K1;&$_($q6)for@$u6;&$_($x6)for@$y6;&$_($C6)for@$K1;&$_($F6)for@$G6;&$_($I6)for@$J6;&$_($S6)for@$K1;&$_($V6)for@$Z6;&$_($f7)for@$K1;&$_($i7)for@$K1;&$_($l7)for@$p7;&$_($s7)for@$K1;&$_($v7)for@$z7;&$_($C7)for@$K1;&$_($F7)for@$J7;&$_($N7)for@$K1;&$_($Q7)for@$U7;&$_($W7)for@$X7;&$_($Z7)for@$c8;&$_($g8)for@$K1;&$_($j8)for@$n8;&$_($p8)for@$q8;&$_($s8)for@$t8;&$_($w8)for@$x8;&$_($A8)for@$B8;&$_($H8)for@$K1;&$_($L8)for@$K1;&$_($O8)for@$P8;&$_($U8)for@$K1;&$_($Y8)for@$K1;&$_($d9)for@$e9;&$_($j9)for@$K1;&$_($n9)for@$K1;&$_($r9)for@$K1;&$_($v9)for@$K1;&$_($z9)for@$K1;&$_($C9)for@$D9;&$_($H9)for@$K1;&$_($L9)for@$K1;&$_($O9)for@$P9;&$_($U9)for@$K1;&$_($Y9)for@$K1;&$_($da)for@$ea;&$_($ga)for@$ha;&$_($ma)for@$na;&$_($ra)for@$K1;&$_($ua)for@$va;&$_($ya)for@$K1;&$_($Ba)for@$Ca;&$_($Ga)for@$K1;&$_($Ja)for@$Ka;&$_($Oa)for@$K1;&$_($Ra)for@$K1;&$_($Ua)for@$Va;&$_($Xa)for@$Ya;&$_($pb)for@$qb;&$_($zb)for@$K1;&$_($Cb)for@$K1;&$_($Fb)for@$qb;&$_($Pb)for@$K1;&$_($Sb)for@$qb;&$_($kc)for@$lc;&$_($pc)for@$K1;&$_($sc)for@$tc;&$_($wc)for@$K1;&$_($zc)for@$Ac;&$_($Dc)for@$Ec;&$_($gd)for@$K1;&$_($jd)for@$ld;&$_($pd)for@$qd;&$_($vd)for@$K1;&$_($yd)for@$zd;&$_($Ed)for@$K1;&$_($Id)for@$K1;&$_($Md)for@$K1;&$_($Qd)for@$K1;&$_($Ud)for@$K1;&$_($Yd)for@$K1;&$_($ee)for@$K1;&$_($ie)for@$K1;&$_($me)for@$K1;&$_($qe)for@$K1;&$_($ue)for@$K1;&$_($ye)for@$K1;&$_($Ce)for@$K1;&$_($Ge)for@$K1;&$_($Ke)for@$K1;&$_($Oe)for@$K1;&$_($Se)for@$K1;&$_($We)for@$K1;&$_($Ze)for@$cf;&$_($ef)for@$ff;&$_($qf)for@$K1;&$_($uf)for@$K1;&$_($yf)for@$K1;&$_($Cf)for@$K1;&$_($Gf)for@$K1;&$_($Jf)for@$Lf;&$_($eg)for@$K1;&$_($ig)for@$K1;&$_($mg)for@$K1;&$_($qg)for@$K1;&$_($ug)for@$K1;&$_($yg)for@$K1;&$_($Cg)for@$K1;&$_($Gg)for@$K1;&$_($Kg)for@$K1;&$_($Og)for@$K1;&$_($Sg)for@$K1;&$_($Wg)for@$K1;&$_($Zg)for@$ch;&$_($ph)for@$K1;&$_($uh)for@$K1;&$_($xh)for@$yh;&$_($Jh)for@$Kh;&$_($Ph)for@$K1;&$_($Sh)for@$Th;&$_($Yh)for@$K1;&$_($ei)for@$K1;&$_($hi)for@$ii;&$_($mi)for@$K1;&$_($pi)for@$qi;&$_($ti)for@$ui;&$_($Ki)for@$Li;&$_($Ni)for@$Oi;&$_($Vi)for@$K1;&$_($Yi)for@$Zi;&$_($gj)for@$K1;&$_($jj)for@$kj;&$_($pj)for@$K1;&$_($tj)for@$K1;&$_($wj)for@$xj;&$_($zj)for@$Aj;&$_($Ej)for@$K1;&$_($Hj)for@$Ij;&$_($Nj)for@$K1;&$_($Qj)for@$Rj;&$_($Tj)for@$Uj;&$_($dk)for@$ek;&$_($kk)for@$K1;&$_($nk)for@$ok;&$_($sk)for@$K1;&$_($vk)for@$wk;&$_($Bk)for@$K1;&$_($Ek)for@$Fk;&$_($Lk)for@$K1;&$_($Ok)for@$K1;&$_($Rk)for@$Sk;&$_($Uk)for@$Vk;&$_($cl)for@$K1;&$_($gl)for@$K1;&$_($jl)for@$kl;&$_($ml)for@$nl;&$_($xl)for@$yl;&$_($Dl)for@$K1;&$_($Gl)for@$K1;&$_($Jl)for@$Kl;&$_($Ol)for@$K1;&$_($Rl)for@$Sl;&$_($Xl)for@$K1;&$_($dm)for@$K1;&$_($gm)for@$hm;&$_($jm)for@$km;&$_($sm)for@$tm;&$_($ym)for@$K1;&$_($Bm)for@$Cm;&$_($Gm)for@$K1;&$_($Jm)for@$Km;&$_($Om)for@$K1;&$_($Rm)for@$K1;&$_($Um)for@$Vm;&$_($Xm)for@$Ym;&$_($on)for@$pn;&$_($vn)for@$K1;&$_($zn)for@$K1;&$_($Dn)for@$K1;&$_($Hn)for@$K1;&$_($Kn)for@$Ln;&$_($Pn)for@$K1;&$_($Sn)for@$Tn;&$_($Xn)for@$Yn;&$_($eo)for@$K1;&$_($ho)for@$K1;&$_($ko)for@$lo;&$_($no)for@$oo;&$_($zo)for@$Ao;&$_($Fo)for@$K1;&$_($Io)for@$K1;&$_($Lo)for@$Mo;&$_($Qo)for@$K1;&$_($To)for@$Uo;&$_($Zo)for@$K1;&$_($ep)for@$fp;&$_($jp)for@$K1;&$_($mp)for@$K1;&$_($pp)for@$qp;&$_($tp)for@$up;&$_($Dp)for@$Ep;&$_($Jp)for@$K1;&$_($Mp)for@$Np;&$_($Rp)for@$K1;&$_($Vp)for@$K1;&$_($Yp)for@$K1;&$_($dq)for@$eq;&$_($gq)for@$hq;&$_($oq)for@$pq;&$_($Lq)for@$Mq;&$_($Oq)for@$pq;&$_($Tq)for@$K1;&$_($Xq)for@$K1;&$_($dr)for@$K1;&$_($fr)for@$gr;ni->run(@ARGV);
__DATA__
