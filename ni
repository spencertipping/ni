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
no warnings qw/redefine void/;
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
->new($name, $self, @slices);';$g8=bless({$F1,$f8,$H1,$I1},$Z);$h8={$e8,$g8};$i8=q'/lib/subclass.b';$j8=bless({$c,$d8,$t1,$u1,$v1,$u1,$w1,$h8,$f,$i8},$P);$k8=q'/lib/dataslice.c';$l8=q'/lib/test_value.c';$m8=q'/unix/pipeline.c';$n8=q'/lib/slice::ctors';$o8=[$Z7,$Q1,$q6,$Z7,$j8];$p8=bless({$c,$G4,$f,$W,$g,$o8},$h);$q8=q'/class.c::ctors';$r8=[$p8];$s8=bless({$c,$e,$f,$d,$g,$r8},$J);$t8=q'/metaclass::ctors';$u8={$J,1};$v8=[$h6,$Q1,$q6,$Z7];$w8=bless({$c,$u8,$f,$J,$g,$v8},$d);$x8=q'/metaclass.c::ctors';$y8={$r,1};$z8=[$v4];$A8=bless({$c,$y8,$f,$r,$g,$z8},$J);$B8=q'/metaclass::ctors';$C8={$d1,1};$D8={};$E8=q'is_mutable';$F8=[];$G8=q'$0 ne "-" && -w $0';$H8=bless({$l2,$F8,$F1,$G8,$H1,$I1},$Z);$I8=q'modify';$J8=[];$K8=q'my ($self, $fn) = @_;
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
  my $r = eval {&$t};
  if ($@) {
    ++$fails;
    print STDERR "FAIL: $@ in $t\\n";
  } elsif (!$r) {
    ++$fails;
    print STDERR "FAIL: $r\\n";
  }
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
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
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
@tests;';$Ra=bless({$F1,$Qa,$H1,$I1},$Z);$Sa={$Ma,$Oa,$Pa,$Ra};$Ta=q'/lib/doc_test.b';$Ua=bless({$c,$La,$t1,$u1,$v1,$u1,$w1,$Sa,$f,$Ta},$P);$Va=q'/lib/slice::ctors';$Wa=[$l3,$x3,$ua,$Ba,$Ja,$Ua];$Xa=bless({$c,$oa,$f,$Y,$g,$Wa},$o);$Ya=q'/lib/doc.c::ctors';$Za=q'synopsis';$cb=q'
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
  ';$db=[$Za,$cb];$eb=q'description';$fb=q'ni:/class is at the core of ni\'s object-oriented system, along with core
      classes like ni:/object and ni:/metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$gb=[$eb,$fb];$hb=q'behaviors';$ib=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:/lib/slice, which represents a set of methods you can add to a
      package. TODO...';$jb=[$hb,$ib];$kb=q'classes';$lb=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$mb=q'TODO...';$nb=[$kb,$lb,$mb];$ob=[$db,$gb,$jb,$nb];$pb=bless({$k4,$ob,$f,$W},$Y);$qb=q'/lib/doc::ctors';$rb=q'ni.doc:/lib/doc';$sb=q'
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$tb=[$Za,$sb];$ub=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$vb=q'Documentation objects are internally represented as arrays of quoted
      method calls:';$wb=q'my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];';$xb=bless({$F1,$wb,$H1,$I1},$Z);$yb=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":';$zb=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$Ab=bless({$F1,$zb,$H1,$I1},$Z);$Bb=[$eb,$ub,$vb,$Ma,$xb,$yb,$Ma,$Ab];$Cb=[$tb,$Bb];$Db=bless({$k4,$Cb,$f,$Y},$Y);$Eb=q'ni.doc:/unix/cat';$Fb=q'
    my $combined = ni(\'ni:/unix/cat\')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into($destination_io);
  ';$Gb=[$Za,$Fb];$Hb=q'Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.';$Ib=[$eb,$Hb];$Jb=[];$Kb=q'my $cat = ni("str:foo") + ni("str:bar");
my $dest = ni(\'ni:/unix/str\')->new(my $data = "");
$cat->into($dest);
now ${$dest->data} == "foo\\nbar\\n";';$Lb=bless({$l2,$Jb,$F1,$Kb,$H1,$I1},$Z);$Mb=[$Ma,$Lb];$Nb=[$Gb,$Ib,$Mb];$Ob=bless({$k4,$Nb,$f,$i1},$Y);$Pb=q'ni:/class';$Qb=q'ni:/class.c';$Rb=q'ni:/lib/accessor.b';$Sb=q'ni:/lib/behavior';$Tb=q'ni:/lib/behavior.c';$Ub=q'ni:/lib/branch';$Vb=q'ni:/lib/branch.b';$Wb=q'ni:/lib/branch.c';$Xb=q'ni:/lib/branch_init.b';$Yb=q'ni:/lib/class_init.b';$Zb=q'ni:/lib/dataslice';$cc=q'/lib/dataslice.c';$dc={$cc,1};$ec=q'/lib/dataslice.c';$fc=[$K];$gc=bless({$c,$dc,$f,$ec,$g,$fc},$J);$hc=q'/metaclass::ctors';$ic={$T,1};$jc={};$kc=q'my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};';$lc=bless({$F1,$kc,$H1,$I1},$Z);$mc={$d2,$lc};$nc=q'/lib/dataslice_init.b';$oc=bless({$c,$jc,$t1,$u1,$v1,$u1,$w1,$mc,$f,$nc},$P);$pc=q'/lib/slice::ctors';$qc={};$rc=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;';$sc=bless({$F1,$rc,$H1,$I1},$Z);$tc={$J3,$sc};$uc=q'/lib/dataslice_apply.b';$vc=bless({$c,$qc,$t1,$u1,$v1,$u1,$w1,$tc,$f,$uc},$P);$wc=q'/lib/slice::ctors';$xc=[$o3,$oc,$vc];$yc=q'/lib/dataslice.c';$zc=bless({$c,$ic,$f,$T,$g,$xc},$yc);$Ac=q'/lib/dataslice.c::ctors';$Bc=q'ni:/lib/dataslice.c';$Cc=q'ni:/lib/dataslice_apply.b';$Dc=q'ni:/lib/dataslice_init.b';$Ec=q'ni:/lib/definition.b';$Fc=q'ni:/lib/definition_def.b';$Gc=q'ni:/lib/definition_defdata.b';$Hc=q'ni:/lib/doc';$Ic=q'ni:/lib/doc.c';$Jc=q'ni:/lib/doc_define.b';$Kc=q'ni:/lib/doc_init.b';$Lc=q'ni:/lib/doc_namespace.b';$Mc=q'ni:/lib/doc_test.b';$Nc=q'ni:/lib/documentable.b';$Oc=q'ni:/lib/fn';$Pc=q'ni:/lib/fn.c';$Qc=q'ni:/lib/fn_init.b';$Rc=q'ni:/lib/fn_ops.b';$Sc=q'ni:/lib/fn_ro.b';$Tc=q'ni:/lib/fn_serialize.b';$Uc=q'ni:/lib/global_static_test.b';$Vc={};$Wc=q'now';$Xc=[];$Yc=q'ni(\'ni:/lib/test_value\')->new(shift)';$Zc=q'($)';$cd=bless({$l2,$Xc,$F1,$Yc,$H1,$Zc},$Z);$dd={$Wc,$cd};$ed=q'/lib/global_static_test.b';$fd=bless({$c,$Vc,$t1,$u1,$v1,$u1,$w1,$dd,$f,$ed},$P);$gd=q'main';$hd=q'/lib/slice::ctors';$id=q'ni:/lib/image';$jd={$q,1};$kd=[$v4];$ld=bless({$c,$jd,$f,$q,$g,$kd},$J);$md=q'/metaclass::ctors';$nd={$c1,1};$od={};$pd=[];$qd=q'my $class = shift;
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
  ordering     => []};';$rd=bless({$l2,$pd,$F1,$qd,$H1,$I1},$Z);$sd={$d2,$rd};$td=q'/lib/image_init.b';$ud=bless({$c,$od,$t1,$u1,$v1,$u1,$w1,$sd,$f,$td},$P);$vd=q'/lib/slice::ctors';$wd={};$xd=q'address';$yd=[];$zd=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$Ad=bless({$l2,$yd,$F1,$zd,$H1,$I1},$Z);$Bd=q'allocate_gensym';$Cd=[];$Dd=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Ed=bless({$l2,$Cd,$F1,$Dd,$H1,$I1},$Z);$Fd=q'boot_side_effect';$Gd=[];$Hd=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Id=bless({$l2,$Gd,$F1,$Hd,$H1,$I1},$Z);$Jd=q'circular_links';$Kd=[];$Ld=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$Md=bless({$l2,$Kd,$F1,$Ld,$H1,$I1},$Z);$Nd=q'finalizer';$Od=[];$Pd=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Qd=bless({$l2,$Od,$F1,$Pd,$H1,$I1},$Z);$Rd=q'gensym';$Sd=[];$Td=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$Ud=bless({$l2,$Sd,$F1,$Td,$H1,$I1},$Z);$Vd=q'is_circular';$Wd=[];$Xd=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$Yd=bless({$l2,$Wd,$F1,$Xd,$H1,$I1},$Z);$Zd=q'quote';$ce=[];$de=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$ee=bless({$l2,$ce,$F1,$de,$H1,$I1},$Z);$fe=q'quote_array';$ge=[];$he=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$ie=bless({$l2,$ge,$F1,$he,$H1,$I1},$Z);$je=q'quote_blessed';$ke=[];$le=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$me=bless({$l2,$ke,$F1,$le,$H1,$I1},$Z);$ne=q'quote_class';$oe=[];$pe=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$qe=bless({$l2,$oe,$F1,$pe,$H1,$I1},$Z);$re=q'quote_hash';$se=[];$te=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$ue=bless({$l2,$se,$F1,$te,$H1,$I1},$Z);$ve=q'quote_object';$we=[];$xe=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$ye=bless({$l2,$we,$F1,$xe,$H1,$I1},$Z);$ze=q'quote_scalar';$Ae=[];$Be=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Ce=bless({$l2,$Ae,$F1,$Be,$H1,$I1},$Z);$De=q'quote_value';$Ee=[];$Fe=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Ge=bless({$l2,$Ee,$F1,$Fe,$H1,$I1},$Z);$He=q'reconstruction';$Ie=[];$Je=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Ke=bless({$l2,$Ie,$F1,$Je,$H1,$I1},$Z);$Le=q'side_effect';$Me=[];$Ne=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Oe=bless({$l2,$Me,$F1,$Ne,$H1,$I1},$Z);$Pe=q'write';$Qe=[];$Re=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Se=bless({$l2,$Qe,$F1,$Re,$H1,$I1},$Z);$Te={$xd,$Ad,$Bd,$Ed,$Fd,$Id,$Jd,$Md,$Nd,$Qd,$Rd,$Ud,$Vd,$Yd,$Zd,$ee,$fe,$ie,$je,$me,$ne,$qe,$re,$ue,$ve,$ye,$ze,$Ce,$De,$Ge,$He,$Ke,$Le,$Oe,$Pe,$Se};$Ue=q'/lib/image_quoting.b';$Ve=bless({$c,$wd,$t1,$u1,$v1,$u1,$w1,$Te,$f,$Ue},$P);$We=q'/lib/slice::ctors';$Xe=[$l3,$ud,$Ve];$Ye=bless({$c,$nd,$f,$c1,$g,$Xe},$q);$Ze=q'/lib/image.c::ctors';$cf=q'ni:/lib/image.c';$df=q'ni:/lib/image_init.b';$ef=q'ni:/lib/image_quoting.b';$ff=q'ni:/lib/instance.b';$gf=q'ni:/lib/instantiable.b';$hf=q'ni:/lib/json.b';$if={};$jf=q'json_decode';$kf=[];$lf=q'local $_;
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
wantarray ? @$r : $$r[0];';$mf=bless({$l2,$kf,$F1,$lf,$H1,$Zc},$Z);$nf=q'json_encode';$of=[];$pf=q'local $_;
my ($v) = @_;
return "[" . join(\',\', map ni::json_encode($_), @$v) . "]" if \'ARRAY\' eq ref $v;
return "{" . join(\',\', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if \'HASH\' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : \'null\';';$qf=bless({$l2,$of,$F1,$pf,$H1,$Zc},$Z);$rf=q'json_escape';$sf=[];$tf=q'(my $x = $_[0]) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . $ni::json_escapes{$1}/eg;
"\\"$x\\"";';$uf=bless({$l2,$sf,$F1,$tf,$H1,$Zc},$Z);$vf=q'json_unescape';$wf=[];$xf=q'my $x = substr $_[0], 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;';$yf=bless({$l2,$wf,$F1,$xf,$H1,$Zc},$Z);$zf=q'json_unescape_one';$Af=[];$Bf=q'$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1';$Cf=bless({$l2,$Af,$F1,$Bf,$H1,$Zc},$Z);$Df={$jf,$mf,$nf,$qf,$rf,$uf,$vf,$yf,$zf,$Cf};$Ef=q'/lib/json.b';$Ff=bless({$c,$if,$t1,$u1,$v1,$u1,$w1,$Df,$f,$Ef},$P);$Gf=q'ni';$Hf=q'/lib/slice::ctors';$If=q'ni:/lib/name_as_string.b';$Jf=q'ni:/lib/named.b';$Kf=q'ni:/lib/named_in_ni.b';$Lf=q'ni:/lib/namespaced.b';$Mf=q'ni:/lib/ni';$Nf=q'ni:/lib/ni.c';$Of=q'ni:/lib/ni_image.b';$Pf=q'ni:/lib/ni_main.b';$Qf=q'ni:/lib/ni_pid_ctors';$Rf=q'ni:/lib/ni_resolver.b';$Sf=q'ni:/lib/ni_self.b';$Tf=q'ni:/lib/ni_static_util.b';$Uf={};$Vf=q'abbrev';$Wf=[];$Xf=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$Yf=bless({$l2,$Wf,$F1,$Xf,$H1,$I1},$Z);$Zf=q'dor';$cg=[];$dg=q'defined $_[0] ? $_[0] : $_[1]';$eg=bless({$l2,$cg,$F1,$dg,$H1,$I1},$Z);$fg=q'indent';$gg=[];$hg=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$ig=bless({$l2,$gg,$F1,$hg,$H1,$I1},$Z);$jg=q'max';$kg=[];$lg=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$mg=bless({$l2,$kg,$F1,$lg,$H1,$I1},$Z);$ng=q'maxstr';$og=[];$pg=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$qg=bless({$l2,$og,$F1,$pg,$H1,$I1},$Z);$rg=q'mean';$sg=[];$tg=q'sum(@_) / (@_ || 1)';$ug=bless({$l2,$sg,$F1,$tg,$H1,$I1},$Z);$vg=q'min';$wg=[];$xg=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$yg=bless({$l2,$wg,$F1,$xg,$H1,$I1},$Z);$zg=q'minstr';$Ag=[];$Bg=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Cg=bless({$l2,$Ag,$F1,$Bg,$H1,$I1},$Z);$Dg=q'sgr';$Eg=[];$Fg=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Gg=bless({$l2,$Eg,$F1,$Fg,$H1,$I1},$Z);$Hg=q'sr';$Ig=[];$Jg=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Kg=bless({$l2,$Ig,$F1,$Jg,$H1,$I1},$Z);$Lg=q'sum';$Mg=[];$Ng=q'local $_; my $x = 0; $x += $_ for @_; $x';$Og=bless({$l2,$Mg,$F1,$Ng,$H1,$I1},$Z);$Pg=q'swap';$Qg=[];$Rg=q'@_[0, 1] = @_[1, 0]';$Sg=bless({$l2,$Qg,$F1,$Rg,$H1,$I1},$Z);$Tg={$Vf,$Yf,$Zf,$eg,$fg,$ig,$jg,$mg,$ng,$qg,$rg,$ug,$vg,$yg,$zg,$Cg,$Dg,$Gg,$Hg,$Kg,$Lg,$Og,$Pg,$Sg};$Ug=q'/lib/ni_static_util.b';$Vg=bless({$c,$Uf,$t1,$u1,$v1,$u1,$w1,$Tg,$f,$Ug},$P);$Wg=q'/lib/slice::ctors';$Xg=q'ni:/lib/perlbranch.b';$Yg=q'ni:/lib/ref_eq.b';$Zg=q'ni:/lib/resolver.b';$ch=q'ni:/lib/slice';$dh=q'ni:/lib/slice.b';$eh=q'ni:/lib/slice.c';$fh=q'ni:/lib/slice_init.b';$gh=q'ni:/lib/slice_serialize.b';$hh=q'ni:/lib/static_fn.b';$ih={};$jh=[];$kh=q'ni(\'ni:/lib/fn\')->new(@_)';$lh=bless({$l2,$jh,$F1,$kh,$H1,$Zc},$Z);$mh=q'fp';$nh=[];$oh=q'ni(\'ni:/lib/fn\')->new(@_)';$ph=q'($$)';$qh=bless({$l2,$nh,$F1,$oh,$H1,$ph},$Z);$rh={$s2,$lh,$mh,$qh};$sh=q'/lib/static_fn.b';$th=bless({$c,$ih,$t1,$u1,$v1,$u1,$w1,$rh,$f,$sh},$P);$uh=q'/lib/slice::ctors';$vh=q'ni:/lib/subclass.b';$wh=q'ni:/lib/tag';$xh=q'ni:/lib/tag.b';$yh=q'ni:/lib/tag.c';$zh=q'ni:/lib/tag_init.b';$Ah=q'ni:/lib/test_value';$Bh=q'/lib/test_value.c';$Ch={$Bh,1};$Dh=q'/lib/test_value.c';$Eh=[$v4];$Fh=bless({$c,$Ch,$f,$Dh,$g,$Eh},$J);$Gh=q'/metaclass::ctors';$Hh={$e1,1};$Ih={};$Jh=[];$Kh=q'\\$_[1]';$Lh=bless({$l2,$Jh,$F1,$Kh,$H1,$I1},$Z);$Mh={$d2,$Lh};$Nh=q'/lib/test_value_init.b';$Oh=bless({$c,$Ih,$t1,$u1,$v1,$u1,$w1,$Mh,$f,$Nh},$P);$Ph=q'/lib/slice::ctors';$Qh={};$Rh=q'(==';$Sh=[];$Th=q'my ($self, $rhs) = @_;
my $diff = $self->diff($rhs);
die $self->class->new($diff) if defined $diff;
1;';$Uh=bless({$l2,$Sh,$F1,$Th,$H1,$I1},$Z);$Vh=q'diff';$Wh=[];$Xh=q'my ($self, $rhs) = @_;
my $class = $self->class;
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
  $diff{$_} = $class->new($$lhs{$_})->diff($$rhs{$_})
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
  $diff{$_} = $class->new($$lhs[$_])->diff($$rhs[$_])
    for 0..$#{$lhs};
  delete @diff{grep !defined($diff{$_}), keys %diff};
  return {array_value_mismatch => 1,
          object_type          => $rl,
          diffs                => \\%diff} if keys %diff;
  return undef;
} elsif ($realtype eq \'SCALAR\') {
  return $class->new($$lhs)->diff($$rhs);
} elsif (!$rl) {
  return {scalar_difference => [$lhs, $rhs]} unless $lhs eq $rhs;
  return undef;
}';$Yh=bless({$l2,$Wh,$F1,$Xh,$H1,$I1},$Z);$Zh={$Rh,$Uh,$Vh,$Yh};$ci=q'/lib/test_value_eq.b';$di=bless({$c,$Qh,$t1,$u1,$v1,$u1,$w1,$Zh,$f,$ci},$P);$ei=q'/lib/slice::ctors';$fi={};$gi=[];$hi=q'ni::json_encode ${$_[0]}';$ii=bless({$l2,$gi,$F1,$hi,$H1,$I1},$Z);$ji={$B2,$ii};$ki=q'/lib/test_value_str.b';$li=bless({$c,$fi,$t1,$u1,$v1,$u1,$w1,$ji,$f,$ki},$P);$mi=q'/lib/slice::ctors';$ni=[$l3,$Oh,$di,$li];$oi=q'/lib/test_value.c';$pi=bless({$c,$Hh,$f,$e1,$g,$ni},$oi);$qi=q'/lib/test_value.c::ctors';$ri=q'ni:/lib/test_value.c';$si=q'ni:/lib/test_value_eq.b';$ti=q'ni:/lib/test_value_init.b';$ui=q'ni:/lib/test_value_str.b';$vi=q'ni:/metaclass';$wi=q'ni:/metaclass.c';$xi=q'ni:/module';$yi=q'ni:/module.c';$zi=q'ni:/object';$Ai=q'ni:/object.c';$Bi=q'ni:/unix/cat';$Ci={$w,1};$Di=q'/unix/pipeline.c';$Ei={$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$Di,1,$D,1};$Fi=[$v4];$Gi=bless({$c,$Ei,$f,$A,$g,$Fi},$J);$Hi=q'/metaclass::ctors';$Ii=[$Gi];$Ji=bless({$c,$Ci,$f,$w,$g,$Ii},$J);$Ki=q'/metaclass::ctors';$Li={$i1,1};$Mi={$i1,1,$j1,1,$k1,1,$l1,1,$m1,1,$n1,1,$o1,1,$q1,1};$Ni={};$Oi=q'into';$Pi=[];$Qi=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Ri=bless({$l2,$Pi,$F1,$Qi,$H1,$I1},$Z);$Si={$Oi,$Ri};$Ti=q'/unix/io_stream.b';$Ui=bless({$c,$Ni,$t1,$u1,$v1,$u1,$w1,$Si,$f,$Ti},$P);$Vi=q'/lib/slice::ctors';$Wi={};$Xi=q'(+';$Yi=[];$Zi=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$cj=bless({$l2,$Yi,$F1,$Zi,$H1,$I1},$Z);$dj={$Xi,$cj};$ej=q'/unix/io_constructors.b';$fj=bless({$c,$Wi,$t1,$u1,$v1,$u1,$w1,$dj,$f,$ej},$P);$gj=q'/lib/slice::ctors';$hj={};$ij=q'(<>';$jj=[];$kj=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$lj=bless({$l2,$jj,$F1,$kj,$H1,$I1},$Z);$mj=q'(@{}';$nj=[];$oj=q'my $self = shift; [<$self>]';$pj=bless({$l2,$nj,$F1,$oj,$H1,$I1},$Z);$qj={$ij,$lj,$mj,$pj};$rj=q'/unix/io_readers.b';$sj=bless({$c,$hj,$t1,$u1,$v1,$u1,$w1,$qj,$f,$rj},$P);$tj=q'/lib/slice::ctors';$uj=[$l3,$Ui,$fj,$sj];$vj=bless({$c,$Mi,$f,$m1,$g,$uj},$A);$wj=q'/unix/io.c::ctors';$xj={};$yj=[];$zj=q'shift; +{fs => [@_]}';$Aj=bless({$l2,$yj,$F1,$zj,$H1,$I1},$Z);$Bj={$d2,$Aj};$Cj=q'/unix/cat_init.b';$Dj=bless({$c,$xj,$t1,$u1,$v1,$u1,$w1,$Bj,$f,$Cj},$P);$Ej=q'/lib/slice::ctors';$Fj={};$Gj=q'read';$Hj=[];$Ij=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Jj=bless({$l2,$Hj,$F1,$Ij,$H1,$I1},$Z);$Kj={$Gj,$Jj};$Lj=q'/unix/cat_read.b';$Mj=bless({$c,$Fj,$t1,$u1,$v1,$u1,$w1,$Kj,$f,$Lj},$P);$Nj=q'/lib/slice::ctors';$Oj=[$vj,$Dj,$Mj];$Pj=bless({$c,$Li,$f,$i1,$g,$Oj},$w);$Qj=q'/unix/cat.c::ctors';$Rj=q'ni:/unix/cat.c';$Sj=q'ni:/unix/cat_init.b';$Tj=q'ni:/unix/cat_read.b';$Uj=q'ni:/unix/fd';$Vj={$x,1};$Wj=[$Gi];$Xj=bless({$c,$Vj,$f,$x,$g,$Wj},$J);$Yj=q'/metaclass::ctors';$Zj={$j1,1};$ck={};$dk=q'fd';$ek=[];$fk=q'shift->{\'fd\'}';$gk=bless({$l2,$ek,$F1,$fk,$H1,$I1},$Z);$hk={$dk,$gk};$ik=q'/unix/fd_readers.b';$jk=bless({$c,$ck,$t1,$u1,$v1,$u1,$w1,$hk,$f,$ik},$P);$kk=q'/lib/slice::ctors';$lk={};$mk=[];$nk=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$ok=bless({$l2,$mk,$F1,$nk,$H1,$I1},$Z);$pk={$d2,$ok};$qk=q'/unix/fd_init.b';$rk=bless({$c,$lk,$t1,$u1,$v1,$u1,$w1,$pk,$f,$qk},$P);$sk=q'/lib/slice::ctors';$tk={};$uk=q'move_to';$vk=[];$wk=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$xk=bless({$l2,$vk,$F1,$wk,$H1,$I1},$Z);$yk={$uk,$xk};$zk=q'/unix/fd_shell.b';$Ak=bless({$c,$tk,$t1,$u1,$v1,$u1,$w1,$yk,$f,$zk},$P);$Bk=q'/lib/slice::ctors';$Ck={$j1,1,$k1,1,$l1,1,$n1,1,$o1,1};$Dk=q'/unix/has_fd.b';$Ek={};$Fk=[];$Gk=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Hk=bless({$l2,$Fk,$F1,$Gk,$H1,$I1},$Z);$Ik=[];$Jk=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Kk=bless({$l2,$Ik,$F1,$Jk,$H1,$I1},$Z);$Lk={$Gj,$Hk,$Pe,$Kk};$Mk=q'/unix/fd_safeio.b';$Nk=bless({$c,$Ek,$t1,$u1,$v1,$u1,$w1,$Lk,$f,$Mk},$P);$Ok=q'/lib/slice::ctors';$Pk=[$Nk];$Qk=bless({$c,$Ck,$f,$Dk,$g,$Pk},$S);$Rk=q'/lib/branch::ctors';$Sk={};$Tk=q'read_fh';$Uk=[];$Vk=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$Wk=bless({$l2,$Uk,$F1,$Vk,$H1,$I1},$Z);$Xk=q'write_fh';$Yk=[];$Zk=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$cl=bless({$l2,$Yk,$F1,$Zk,$H1,$I1},$Z);$dl={$Tk,$Wk,$Xk,$cl};$el=q'/unix/fd_io.b';$fl=bless({$c,$Sk,$t1,$u1,$v1,$u1,$w1,$dl,$f,$el},$P);$gl=q'/lib/slice::ctors';$hl=[$vj,$jk,$rk,$Ak,$Qk,$fl];$il=bless({$c,$Zj,$f,$j1,$g,$hl},$x);$jl=q'/unix/fd.c::ctors';$kl=q'ni:/unix/fd.c';$ll=q'ni:/unix/fd_init.b';$ml=q'ni:/unix/fd_io.b';$nl=q'ni:/unix/fd_readers.b';$ol=q'ni:/unix/fd_safeio.b';$pl=q'ni:/unix/fd_shell.b';$ql=q'ni:/unix/fifo';$rl={$y,1};$sl=[$Gi];$tl=bless({$c,$rl,$f,$y,$g,$sl},$J);$ul=q'/metaclass::ctors';$vl={$k1,1};$wl={};$xl=[];$yl=q'shift->{\'read_fh\'}';$zl=bless({$l2,$xl,$F1,$yl,$H1,$I1},$Z);$Al=[];$Bl=q'shift->{\'write_fh\'}';$Cl=bless({$l2,$Al,$F1,$Bl,$H1,$I1},$Z);$Dl={$Tk,$zl,$Xk,$Cl};$El=q'/unix/fifo_io.b';$Fl=bless({$c,$wl,$t1,$u1,$v1,$u1,$w1,$Dl,$f,$El},$P);$Gl=q'/lib/slice::ctors';$Hl={};$Il=[];$Jl=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Kl=bless({$l2,$Il,$F1,$Jl,$H1,$I1},$Z);$Ll={$d2,$Kl};$Ml=q'/unix/fifo_init.b';$Nl=bless({$c,$Hl,$t1,$u1,$v1,$u1,$w1,$Ll,$f,$Ml},$P);$Ol=q'/lib/slice::ctors';$Pl={};$Ql=q'read_side';$Rl=[];$Sl=q'my $self = shift; close $$self{write_fh}; $self';$Tl=bless({$l2,$Rl,$F1,$Sl,$H1,$I1},$Z);$Ul=q'write_side';$Vl=[];$Wl=q'my $self = shift; close $$self{read_fh};  $self';$Xl=bless({$l2,$Vl,$F1,$Wl,$H1,$I1},$Z);$Yl={$Ql,$Tl,$Ul,$Xl};$Zl=q'/unix/fifo_direction.b';$cm=bless({$c,$Pl,$t1,$u1,$v1,$u1,$w1,$Yl,$f,$Zl},$P);$dm=q'/lib/slice::ctors';$em=[$vj,$Fl,$Nl,$Qk,$cm];$fm=bless({$c,$vl,$f,$k1,$g,$em},$y);$gm=q'/unix/fifo.c::ctors';$hm=q'ni:/unix/fifo.c';$im=q'ni:/unix/fifo_direction.b';$jm=q'ni:/unix/fifo_init.b';$km=q'ni:/unix/fifo_io.b';$lm=q'ni:/unix/file';$mm={$z,1};$nm=[$Gi];$om=bless({$c,$mm,$f,$z,$g,$nm},$J);$pm=q'/metaclass::ctors';$qm={$l1,1};$rm={};$sm=[];$tm=q'shift->{\'name\'}';$um=bless({$l2,$sm,$F1,$tm,$H1,$I1},$Z);$vm={$f,$um};$wm=q'/unix/file_readers.b';$xm=bless({$c,$rm,$t1,$u1,$v1,$u1,$w1,$vm,$f,$wm},$P);$ym=q'/lib/slice::ctors';$zm={};$Am=[];$Bm=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Cm=bless({$l2,$Am,$F1,$Bm,$H1,$I1},$Z);$Dm={$d2,$Cm};$Em=q'/unix/file_init.b';$Fm=bless({$c,$zm,$t1,$u1,$v1,$u1,$w1,$Dm,$f,$Em},$P);$Gm=q'/lib/slice::ctors';$Hm={};$Im=[];$Jm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Km=bless({$l2,$Im,$F1,$Jm,$H1,$I1},$Z);$Lm=[];$Mm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Nm=bless({$l2,$Lm,$F1,$Mm,$H1,$I1},$Z);$Om={$Tk,$Km,$Xk,$Nm};$Pm=q'/unix/file_io.b';$Qm=bless({$c,$Hm,$t1,$u1,$v1,$u1,$w1,$Om,$f,$Pm},$P);$Rm=q'/lib/slice::ctors';$Sm=[$vj,$xm,$Fm,$Qk,$Qm];$Tm=bless({$c,$qm,$f,$l1,$g,$Sm},$z);$Um=q'/unix/file.c::ctors';$Vm=q'ni:/unix/file.c';$Wm=q'ni:/unix/file_init.b';$Xm=q'ni:/unix/file_io.b';$Ym=q'ni:/unix/file_readers.b';$Zm=q'ni:/unix/has_fd.b';$cn=q'ni:/unix/io';$dn=q'ni:/unix/io.c';$en=q'ni:/unix/io_constructors.b';$fn=q'ni:/unix/io_readers.b';$gn=q'ni:/unix/io_stream.b';$hn=q'ni:/unix/pid';$in={$B,1};$jn=[$Gi];$kn=bless({$c,$in,$f,$B,$g,$jn},$J);$ln=q'/metaclass::ctors';$mn={$n1,1};$nn={};$on=q'pid';$pn=[];$qn=q'shift->{\'pid\'}';$rn=bless({$l2,$pn,$F1,$qn,$H1,$I1},$Z);$sn=q'stderr';$tn=[];$un=q'shift->{\'stderr\'}';$vn=bless({$l2,$tn,$F1,$un,$H1,$I1},$Z);$wn=q'stdin';$xn=[];$yn=q'shift->{\'stdin\'}';$zn=bless({$l2,$xn,$F1,$yn,$H1,$I1},$Z);$An=q'stdout';$Bn=[];$Cn=q'shift->{\'stdout\'}';$Dn=bless({$l2,$Bn,$F1,$Cn,$H1,$I1},$Z);$En={$on,$rn,$sn,$vn,$wn,$zn,$An,$Dn};$Fn=q'/unix/pid_readers.b';$Gn=bless({$c,$nn,$t1,$u1,$v1,$u1,$w1,$En,$f,$Fn},$P);$Hn=q'/lib/slice::ctors';$In={};$Jn=[];$Kn=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Ln=bless({$l2,$Jn,$F1,$Kn,$H1,$I1},$Z);$Mn={$d2,$Ln};$Nn=q'/unix/pid_init.b';$On=bless({$c,$In,$t1,$u1,$v1,$u1,$w1,$Mn,$f,$Nn},$P);$Pn=q'/lib/slice::ctors';$Qn={};$Rn={};$Sn=q'/unix/pid_wait.b';$Tn=bless({$c,$Qn,$t1,$u1,$v1,$u1,$w1,$Rn,$f,$Sn},$P);$Un=q'/lib/slice::ctors';$Vn={};$Wn=[];$Xn=q'shift->{stdout}->read_fh';$Yn=bless({$l2,$Wn,$F1,$Xn,$H1,$I1},$Z);$Zn=[];$co=q'shift->{stdin}->write_fh';$do=bless({$l2,$Zn,$F1,$co,$H1,$I1},$Z);$eo={$Tk,$Yn,$Xk,$do};$fo=q'/unix/pid_io.b';$go=bless({$c,$Vn,$t1,$u1,$v1,$u1,$w1,$eo,$f,$fo},$P);$ho=q'/lib/slice::ctors';$io=[$vj,$Gn,$On,$Tn,$Qk,$go];$jo=bless({$c,$mn,$f,$n1,$g,$io},$B);$ko=q'/unix/pid.c::ctors';$lo=q'ni:/unix/pid.c';$mo=q'ni:/unix/pid_init.b';$no=q'ni:/unix/pid_io.b';$oo=q'ni:/unix/pid_readers.b';$po=q'ni:/unix/pid_wait.b';$qo=q'ni:/unix/pipeline';$ro=q'/unix/pipeline.c';$so={$ro,1};$to=q'/unix/pipeline.c';$uo=[$Gi];$vo=bless({$c,$so,$f,$to,$g,$uo},$J);$wo=q'/metaclass::ctors';$xo={$o1,1};$yo={};$zo=[];$Ao=q'shift->{\'stdin\'}';$Bo=bless({$l2,$zo,$F1,$Ao,$H1,$I1},$Z);$Co=[];$Do=q'shift->{\'stdout\'}';$Eo=bless({$l2,$Co,$F1,$Do,$H1,$I1},$Z);$Fo={$wn,$Bo,$An,$Eo};$Go=q'/unix/pipeline_ro.b';$Ho=bless({$c,$yo,$t1,$u1,$v1,$u1,$w1,$Fo,$f,$Go},$P);$Io=q'/lib/slice::ctors';$Jo={};$Ko=[];$Lo=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Mo=bless({$l2,$Ko,$F1,$Lo,$H1,$I1},$Z);$No={$d2,$Mo};$Oo=q'/unix/pipeline_init.b';$Po=bless({$c,$Jo,$t1,$u1,$v1,$u1,$w1,$No,$f,$Oo},$P);$Qo=q'/lib/slice::ctors';$Ro={};$So=q'async_step';$To=[];$Uo=q'local $_;
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
$self;';$Vo=bless({$l2,$To,$F1,$Uo,$H1,$I1},$Z);$Wo={$So,$Vo};$Xo=q'/unix/pipeline_async.b';$Yo=bless({$c,$Ro,$t1,$u1,$v1,$u1,$w1,$Wo,$f,$Xo},$P);$Zo=q'/lib/slice::ctors';$cp={};$dp=[];$ep=q'shift->{stdout}->read_fh';$fp=bless({$l2,$dp,$F1,$ep,$H1,$I1},$Z);$gp=[];$hp=q'shift->{stdin}->write_fh';$ip=bless({$l2,$gp,$F1,$hp,$H1,$I1},$Z);$jp={$Tk,$fp,$Xk,$ip};$kp=q'/unix/pipeline_io.b';$lp=bless({$c,$cp,$t1,$u1,$v1,$u1,$w1,$jp,$f,$kp},$P);$mp=q'/lib/slice::ctors';$np=[$vj,$Ho,$Po,$Yo,$Qk,$lp];$op=q'/unix/pipeline.c';$pp=bless({$c,$xo,$f,$o1,$g,$np},$op);$qp=q'/unix/pipeline.c::ctors';$rp=q'ni:/unix/pipeline.c';$sp=q'ni:/unix/pipeline_async.b';$tp=q'ni:/unix/pipeline_init.b';$up=q'ni:/unix/pipeline_io.b';$vp=q'ni:/unix/pipeline_ro.b';$wp=q'ni:/unix/str';$xp={$D,1};$yp=[$Gi];$zp=bless({$c,$xp,$f,$D,$g,$yp},$J);$Ap=q'/metaclass::ctors';$Bp={$q1,1};$Cp={};$Dp=q'data';$Ep=[];$Fp=q'shift->{\'data\'}';$Gp=bless({$l2,$Ep,$F1,$Fp,$H1,$I1},$Z);$Hp=q'end';$Ip=[];$Jp=q'shift->{\'end\'}';$Kp=bless({$l2,$Ip,$F1,$Jp,$H1,$I1},$Z);$Lp=q'start';$Mp=[];$Np=q'shift->{\'start\'}';$Op=bless({$l2,$Mp,$F1,$Np,$H1,$I1},$Z);$Pp={$Dp,$Gp,$Hp,$Kp,$Lp,$Op};$Qp=q'/unix/str_ro.b';$Rp=bless({$c,$Cp,$t1,$u1,$v1,$u1,$w1,$Pp,$f,$Qp},$P);$Sp=q'/lib/slice::ctors';$Tp={};$Up=[];$Vp=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Wp=bless({$l2,$Up,$F1,$Vp,$H1,$I1},$Z);$Xp={$d2,$Wp};$Yp=q'/unix/str_init.b';$Zp=bless({$c,$Tp,$t1,$u1,$v1,$u1,$w1,$Xp,$f,$Yp},$P);$cq=q'/lib/slice::ctors';$dq={};$eq=[];$fq=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = @_ >= 2 ? ni::min($self->remaining, $_[1]) : $self->remaining;
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$gq=bless({$l2,$eq,$F1,$fq,$H1,$I1},$Z);$hq=q'remaining';$iq=[];$jq=q'my $self = shift; $$self{end} - $$self{start}';$kq=bless({$l2,$iq,$F1,$jq,$H1,$I1},$Z);$lq=[];$mq=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$nq=bless({$l2,$lq,$F1,$mq,$H1,$I1},$Z);$oq={$Gj,$gq,$hq,$kq,$Pe,$nq};$pq=q'/unix/str_io.b';$qq=bless({$c,$dq,$t1,$u1,$v1,$u1,$w1,$oq,$f,$pq},$P);$rq=q'/lib/slice::ctors';$sq=[$vj,$Rp,$Zp,$qq];$tq=bless({$c,$Bp,$f,$q1,$g,$sq},$D);$uq=q'/unix/str.c::ctors';$vq=q'ni:/unix/str.c';$wq=q'ni:/unix/str_init.b';$xq=q'ni:/unix/str_io.b';$yq=q'ni:/unix/str_ro.b';$zq=q'ni:main';$Aq={$gd,1};$Bq=[$th,$fd];$Cq=bless({$c,$Aq,$f,$gd,$g,$Bq},$g1);$Dq=q'/module::ctors';$Eq=q'ni:ni';$Fq={$Gf,1};$Gq={$Gf,1};$Hq=q'json_escapes';$Iq=q'';$Jq=q'b';$Kq=q'	';$Lq=q't';$Mq=q'
';$Nq=q'n';$Oq=q'';$Pq=q'r';$Qq=q'"';$Rq=q'/';$Sq=q'\\';$Tq={$Iq,$Jq,$Kq,$Lq,$Mq,$Nq,$Oq,$Pq,$Qq,$Qq,$Rq,$Rq,$Sq,$Sq};$Uq=q'json_unescapes';$Vq={$Qq,$Qq,$Rq,$Rq,$Sq,$Sq,$Jq,$Iq,$Nq,$Mq,$Pq,$Oq,$Lq,$Kq};$Wq={$Hq,$Tq,$Uq,$Vq};$Xq=q'/lib/json_data.b';$Yq=bless({$c,$Gq,$Dp,$Wq,$f,$Xq},$T);$Zq=q'/lib/dataslice::ctors';$cr=[$Yq,$Ff,$Vg];$dr=bless({$c,$Fq,$f,$Gf,$g,$cr},$g1);$er={$ja,$pb,$rb,$Db,$Eb,$Ob,$Pb,$p8,$Qb,$B4,$Rb,$l7,$Sb,$o3,$Tb,$K,$Ub,$I6,$Vb,$s5,$Wb,$x6,$Xb,$F6,$Yb,$q6,$Zb,$zc,$Bc,$gc,$Cc,$vc,$Dc,$oc,$Ec,$W7,$Fc,$V6,$Gc,$Q7,$Hc,$Xa,$Ic,$ma,$Jc,$Ja,$Kc,$ua,$Lc,$Ba,$Mc,$Ua,$Nc,$p4,$Oc,$X2,$Pc,$A1,$Qc,$i2,$Rc,$L2,$Sc,$y2,$Tc,$U2,$Uc,$fd,$id,$Ye,$cf,$ld,$df,$ud,$ef,$Ve,$ff,$f3,$gf,$Q1,$hf,$Ff,$If,$v7,$Jf,$x3,$Kf,$D5,$Lf,$O5,$Mf,$ga,$Nf,$A8,$Of,$d9,$Pf,$C9,$Qf,$da,$Rf,$O9,$Sf,$O8,$Tf,$Vg,$Xg,$h6,$Yg,$F7,$Zg,$Z5,$ch,$h4,$dh,$N3,$eh,$N,$fh,$U3,$gh,$e4,$hh,$th,$vh,$j8,$wh,$h5,$xh,$V4,$yh,$N4,$zh,$e5,$Ah,$pi,$ri,$Fh,$si,$di,$ti,$Oh,$ui,$li,$vi,$w8,$wi,$s8,$xi,$Z7,$yi,$y4,$zi,$l3,$Ai,$v4,$Bi,$Pj,$Rj,$Ji,$Sj,$Dj,$Tj,$Mj,$Uj,$il,$kl,$Xj,$ll,$rk,$ml,$fl,$nl,$jk,$ol,$Nk,$pl,$Ak,$ql,$fm,$hm,$tl,$im,$cm,$jm,$Nl,$km,$Fl,$lm,$Tm,$Vm,$om,$Wm,$Fm,$Xm,$Qm,$Ym,$xm,$Zm,$Qk,$cn,$vj,$dn,$Gi,$en,$fj,$fn,$sj,$gn,$Ui,$hn,$jo,$lo,$kn,$mo,$On,$no,$go,$oo,$Gn,$po,$Tn,$qo,$pp,$rp,$vo,$sp,$Yo,$tp,$Po,$up,$lp,$vp,$Ho,$wp,$tq,$vq,$zp,$wq,$Zp,$xq,$qq,$yq,$Rp,$zq,$Cq,$Eq,$dr};$fr=q'resolvers';$gr=[];$hr=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$ir=bless({$l2,$gr,$F1,$hr,$H1,$I1},$Z);$jr=q'file';$kr=[];$lr=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$mr=bless({$l2,$kr,$F1,$lr,$H1,$I1},$Z);$nr=q'str';$or=[];$pr=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$qr=bless({$l2,$or,$F1,$pr,$H1,$I1},$Z);$rr={$dk,$ir,$jr,$mr,$nr,$qr};$sr=bless({$ia,$er,$fr,$rr},$d1);$tr=q'/lib/ni::ctors';$$u4[0]=$p8;$$I[0]=$v4;$$z1[0]=$v4;$$W2[0]=$l3;$$H6[4]=$W7;*$H3=\&$F3;*$G3=\&$D3;$Q1->apply_unsafe($W);$Q1->apply_unsafe($h);$Q1->apply_unsafe($l);$Q1->apply_unsafe($m);$Q1->apply_unsafe($R1);$Q1->apply_unsafe($o);$Q1->apply_unsafe($Z);$Q1->apply_unsafe($p);$Q1->apply_unsafe($q);$Q1->apply_unsafe($r);$Q1->apply_unsafe($P);$Q1->apply_unsafe($s);$Q1->apply_unsafe($U);$Q1->apply_unsafe($t);$Q1->apply_unsafe($S1);$Q1->apply_unsafe($J);$Q1->apply_unsafe($d);$Q1->apply_unsafe($j);$Q1->apply_unsafe($v);$Q1->apply_unsafe($w);$Q1->apply_unsafe($x);$Q1->apply_unsafe($y);$Q1->apply_unsafe($z);$Q1->apply_unsafe($A);$Q1->apply_unsafe($B);$Q1->apply_unsafe($T1);$Q1->apply_unsafe($D);$i2->apply_unsafe($Z);$y2->apply_unsafe($Z);$L2->apply_unsafe($Z);$U2->apply_unsafe($Z);$f3->apply_unsafe($W);$f3->apply_unsafe($h);$f3->apply_unsafe($R);$f3->apply_unsafe($l);$f3->apply_unsafe($S);$f3->apply_unsafe($m);$f3->apply_unsafe($T);$f3->apply_unsafe($g3);$f3->apply_unsafe($Y);$f3->apply_unsafe($o);$f3->apply_unsafe($Z);$f3->apply_unsafe($p);$f3->apply_unsafe($c1);$f3->apply_unsafe($q);$f3->apply_unsafe($d1);$f3->apply_unsafe($r);$f3->apply_unsafe($P);$f3->apply_unsafe($s);$f3->apply_unsafe($U);$f3->apply_unsafe($t);$f3->apply_unsafe($e1);$f3->apply_unsafe($h3);$f3->apply_unsafe($J);$f3->apply_unsafe($d);$f3->apply_unsafe($g1);$f3->apply_unsafe($j);$f3->apply_unsafe($h1);$f3->apply_unsafe($v);$f3->apply_unsafe($i1);$f3->apply_unsafe($w);$f3->apply_unsafe($j1);$f3->apply_unsafe($x);$f3->apply_unsafe($k1);$f3->apply_unsafe($y);$f3->apply_unsafe($l1);$f3->apply_unsafe($z);$f3->apply_unsafe($m1);$f3->apply_unsafe($A);$f3->apply_unsafe($n1);$f3->apply_unsafe($B);$f3->apply_unsafe($o1);$f3->apply_unsafe($i3);$f3->apply_unsafe($q1);$f3->apply_unsafe($D);$x3->apply_unsafe($W);$x3->apply_unsafe($h);$x3->apply_unsafe($l);$x3->apply_unsafe($S);$x3->apply_unsafe($m);$x3->apply_unsafe($y3);$x3->apply_unsafe($Y);$x3->apply_unsafe($o);$x3->apply_unsafe($p);$x3->apply_unsafe($q);$x3->apply_unsafe($r);$x3->apply_unsafe($P);$x3->apply_unsafe($s);$x3->apply_unsafe($U);$x3->apply_unsafe($t);$x3->apply_unsafe($z3);$x3->apply_unsafe($J);$x3->apply_unsafe($d);$x3->apply_unsafe($g1);$x3->apply_unsafe($j);$x3->apply_unsafe($v);$x3->apply_unsafe($w);$x3->apply_unsafe($x);$x3->apply_unsafe($y);$x3->apply_unsafe($z);$x3->apply_unsafe($A);$x3->apply_unsafe($B);$x3->apply_unsafe($A3);$x3->apply_unsafe($D);$N3->apply_unsafe($P);$U3->apply_unsafe($P);$e4->apply_unsafe($P);$p4->apply_unsafe($h);$p4->apply_unsafe($l);$p4->apply_unsafe($m);$p4->apply_unsafe($q4);$p4->apply_unsafe($o);$p4->apply_unsafe($p);$p4->apply_unsafe($q);$p4->apply_unsafe($r);$p4->apply_unsafe($s);$p4->apply_unsafe($t);$p4->apply_unsafe($r4);$p4->apply_unsafe($j);$p4->apply_unsafe($v);$p4->apply_unsafe($w);$p4->apply_unsafe($x);$p4->apply_unsafe($y);$p4->apply_unsafe($z);$p4->apply_unsafe($A);$p4->apply_unsafe($B);$p4->apply_unsafe($s4);$p4->apply_unsafe($D);$V4->apply_unsafe($U);$e5->apply_unsafe($U);$s5->apply_unsafe($W);$s5->apply_unsafe($h);$s5->apply_unsafe($l);$s5->apply_unsafe($S);$s5->apply_unsafe($m);$s5->apply_unsafe($t5);$s5->apply_unsafe($o);$s5->apply_unsafe($p);$s5->apply_unsafe($q);$s5->apply_unsafe($r);$s5->apply_unsafe($s);$s5->apply_unsafe($t);$s5->apply_unsafe($u5);$s5->apply_unsafe($J);$s5->apply_unsafe($d);$s5->apply_unsafe($g1);$s5->apply_unsafe($j);$s5->apply_unsafe($v);$s5->apply_unsafe($w);$s5->apply_unsafe($x);$s5->apply_unsafe($y);$s5->apply_unsafe($z);$s5->apply_unsafe($A);$s5->apply_unsafe($B);$s5->apply_unsafe($v5);$s5->apply_unsafe($D);$D5->apply_unsafe($W);$D5->apply_unsafe($h);$D5->apply_unsafe($l);$D5->apply_unsafe($S);$D5->apply_unsafe($m);$D5->apply_unsafe($E5);$D5->apply_unsafe($o);$D5->apply_unsafe($p);$D5->apply_unsafe($q);$D5->apply_unsafe($r);$D5->apply_unsafe($P);$D5->apply_unsafe($s);$D5->apply_unsafe($U);$D5->apply_unsafe($t);$D5->apply_unsafe($F5);$D5->apply_unsafe($J);$D5->apply_unsafe($d);$D5->apply_unsafe($g1);$D5->apply_unsafe($j);$D5->apply_unsafe($v);$D5->apply_unsafe($w);$D5->apply_unsafe($x);$D5->apply_unsafe($y);$D5->apply_unsafe($z);$D5->apply_unsafe($A);$D5->apply_unsafe($B);$D5->apply_unsafe($G5);$D5->apply_unsafe($D);$O5->apply_unsafe($W);$O5->apply_unsafe($h);$O5->apply_unsafe($l);$O5->apply_unsafe($S);$O5->apply_unsafe($m);$O5->apply_unsafe($P5);$O5->apply_unsafe($o);$O5->apply_unsafe($p);$O5->apply_unsafe($q);$O5->apply_unsafe($r);$O5->apply_unsafe($P);$O5->apply_unsafe($s);$O5->apply_unsafe($U);$O5->apply_unsafe($t);$O5->apply_unsafe($Q5);$O5->apply_unsafe($J);$O5->apply_unsafe($d);$O5->apply_unsafe($g1);$O5->apply_unsafe($j);$O5->apply_unsafe($v);$O5->apply_unsafe($w);$O5->apply_unsafe($x);$O5->apply_unsafe($y);$O5->apply_unsafe($z);$O5->apply_unsafe($A);$O5->apply_unsafe($B);$O5->apply_unsafe($R5);$O5->apply_unsafe($D);$Z5->apply_unsafe($W);$Z5->apply_unsafe($h);$Z5->apply_unsafe($l);$Z5->apply_unsafe($S);$Z5->apply_unsafe($m);$Z5->apply_unsafe($c6);$Z5->apply_unsafe($o);$Z5->apply_unsafe($p);$Z5->apply_unsafe($q);$Z5->apply_unsafe($r);$Z5->apply_unsafe($s);$Z5->apply_unsafe($U);$Z5->apply_unsafe($t);$Z5->apply_unsafe($d6);$Z5->apply_unsafe($J);$Z5->apply_unsafe($d);$Z5->apply_unsafe($g1);$Z5->apply_unsafe($j);$Z5->apply_unsafe($v);$Z5->apply_unsafe($w);$Z5->apply_unsafe($x);$Z5->apply_unsafe($y);$Z5->apply_unsafe($z);$Z5->apply_unsafe($A);$Z5->apply_unsafe($B);$Z5->apply_unsafe($e6);$Z5->apply_unsafe($D);$q6->apply_unsafe($W);$q6->apply_unsafe($h);$q6->apply_unsafe($l);$q6->apply_unsafe($m);$q6->apply_unsafe($r6);$q6->apply_unsafe($o);$q6->apply_unsafe($p);$q6->apply_unsafe($q);$q6->apply_unsafe($r);$q6->apply_unsafe($s);$q6->apply_unsafe($t);$q6->apply_unsafe($s6);$q6->apply_unsafe($J);$q6->apply_unsafe($d);$q6->apply_unsafe($g1);$q6->apply_unsafe($j);$q6->apply_unsafe($v);$q6->apply_unsafe($w);$q6->apply_unsafe($x);$q6->apply_unsafe($y);$q6->apply_unsafe($z);$q6->apply_unsafe($A);$q6->apply_unsafe($B);$q6->apply_unsafe($t6);$q6->apply_unsafe($D);$F6->apply_unsafe($S);$V6->apply_unsafe($W);$V6->apply_unsafe($h);$V6->apply_unsafe($l);$V6->apply_unsafe($S);$V6->apply_unsafe($m);$V6->apply_unsafe($W6);$V6->apply_unsafe($o);$V6->apply_unsafe($p);$V6->apply_unsafe($q);$V6->apply_unsafe($r);$V6->apply_unsafe($s);$V6->apply_unsafe($t);$V6->apply_unsafe($X6);$V6->apply_unsafe($J);$V6->apply_unsafe($d);$V6->apply_unsafe($g1);$V6->apply_unsafe($j);$V6->apply_unsafe($v);$V6->apply_unsafe($w);$V6->apply_unsafe($x);$V6->apply_unsafe($y);$V6->apply_unsafe($z);$V6->apply_unsafe($A);$V6->apply_unsafe($B);$V6->apply_unsafe($Y6);$V6->apply_unsafe($D);$l7->apply_unsafe($W);$l7->apply_unsafe($h);$l7->apply_unsafe($l);$l7->apply_unsafe($S);$l7->apply_unsafe($m);$l7->apply_unsafe($m7);$l7->apply_unsafe($o);$l7->apply_unsafe($p);$l7->apply_unsafe($q);$l7->apply_unsafe($r);$l7->apply_unsafe($s);$l7->apply_unsafe($t);$l7->apply_unsafe($n7);$l7->apply_unsafe($J);$l7->apply_unsafe($d);$l7->apply_unsafe($g1);$l7->apply_unsafe($j);$l7->apply_unsafe($v);$l7->apply_unsafe($w);$l7->apply_unsafe($x);$l7->apply_unsafe($y);$l7->apply_unsafe($z);$l7->apply_unsafe($A);$l7->apply_unsafe($B);$l7->apply_unsafe($o7);$l7->apply_unsafe($D);$v7->apply_unsafe($W);$v7->apply_unsafe($h);$v7->apply_unsafe($l);$v7->apply_unsafe($S);$v7->apply_unsafe($m);$v7->apply_unsafe($w7);$v7->apply_unsafe($o);$v7->apply_unsafe($p);$v7->apply_unsafe($q);$v7->apply_unsafe($r);$v7->apply_unsafe($s);$v7->apply_unsafe($t);$v7->apply_unsafe($x7);$v7->apply_unsafe($J);$v7->apply_unsafe($d);$v7->apply_unsafe($g1);$v7->apply_unsafe($j);$v7->apply_unsafe($v);$v7->apply_unsafe($w);$v7->apply_unsafe($x);$v7->apply_unsafe($y);$v7->apply_unsafe($z);$v7->apply_unsafe($A);$v7->apply_unsafe($B);$v7->apply_unsafe($y7);$v7->apply_unsafe($D);$F7->apply_unsafe($W);$F7->apply_unsafe($h);$F7->apply_unsafe($l);$F7->apply_unsafe($S);$F7->apply_unsafe($m);$F7->apply_unsafe($G7);$F7->apply_unsafe($o);$F7->apply_unsafe($p);$F7->apply_unsafe($q);$F7->apply_unsafe($r);$F7->apply_unsafe($s);$F7->apply_unsafe($t);$F7->apply_unsafe($H7);$F7->apply_unsafe($J);$F7->apply_unsafe($d);$F7->apply_unsafe($g1);$F7->apply_unsafe($j);$F7->apply_unsafe($v);$F7->apply_unsafe($w);$F7->apply_unsafe($x);$F7->apply_unsafe($y);$F7->apply_unsafe($z);$F7->apply_unsafe($A);$F7->apply_unsafe($B);$F7->apply_unsafe($I7);$F7->apply_unsafe($D);$Q7->apply_unsafe($W);$Q7->apply_unsafe($h);$Q7->apply_unsafe($l);$Q7->apply_unsafe($S);$Q7->apply_unsafe($m);$Q7->apply_unsafe($R7);$Q7->apply_unsafe($o);$Q7->apply_unsafe($p);$Q7->apply_unsafe($q);$Q7->apply_unsafe($r);$Q7->apply_unsafe($s);$Q7->apply_unsafe($t);$Q7->apply_unsafe($S7);$Q7->apply_unsafe($J);$Q7->apply_unsafe($d);$Q7->apply_unsafe($g1);$Q7->apply_unsafe($j);$Q7->apply_unsafe($v);$Q7->apply_unsafe($w);$Q7->apply_unsafe($x);$Q7->apply_unsafe($y);$Q7->apply_unsafe($z);$Q7->apply_unsafe($A);$Q7->apply_unsafe($B);$Q7->apply_unsafe($T7);$Q7->apply_unsafe($D);$j8->apply_unsafe($W);$j8->apply_unsafe($h);$j8->apply_unsafe($l);$j8->apply_unsafe($m);$j8->apply_unsafe($k8);$j8->apply_unsafe($o);$j8->apply_unsafe($p);$j8->apply_unsafe($q);$j8->apply_unsafe($r);$j8->apply_unsafe($s);$j8->apply_unsafe($t);$j8->apply_unsafe($l8);$j8->apply_unsafe($d);$j8->apply_unsafe($j);$j8->apply_unsafe($v);$j8->apply_unsafe($w);$j8->apply_unsafe($x);$j8->apply_unsafe($y);$j8->apply_unsafe($z);$j8->apply_unsafe($A);$j8->apply_unsafe($B);$j8->apply_unsafe($m8);$j8->apply_unsafe($D);$O8->apply_unsafe($d1);$d9->apply_unsafe($d1);$C9->apply_unsafe($d1);$O9->apply_unsafe($d1);$da->apply_unsafe($d1);$ua->apply_unsafe($Y);$Ba->apply_unsafe($Y);$Ja->apply_unsafe($Y);$Ua->apply_unsafe($Y);$oc->apply_unsafe($T);$vc->apply_unsafe($T);$fd->apply_unsafe($gd);$ud->apply_unsafe($c1);$Ve->apply_unsafe($c1);$Ff->apply_unsafe($Gf);$Vg->apply_unsafe($Gf);$th->apply_unsafe($gd);$Oh->apply_unsafe($e1);$di->apply_unsafe($e1);$li->apply_unsafe($e1);$Ui->apply_unsafe($i1);$Ui->apply_unsafe($j1);$Ui->apply_unsafe($k1);$Ui->apply_unsafe($l1);$Ui->apply_unsafe($m1);$Ui->apply_unsafe($n1);$Ui->apply_unsafe($o1);$Ui->apply_unsafe($q1);$fj->apply_unsafe($i1);$fj->apply_unsafe($j1);$fj->apply_unsafe($k1);$fj->apply_unsafe($l1);$fj->apply_unsafe($m1);$fj->apply_unsafe($n1);$fj->apply_unsafe($o1);$fj->apply_unsafe($q1);$sj->apply_unsafe($i1);$sj->apply_unsafe($j1);$sj->apply_unsafe($k1);$sj->apply_unsafe($l1);$sj->apply_unsafe($m1);$sj->apply_unsafe($n1);$sj->apply_unsafe($o1);$sj->apply_unsafe($q1);$Dj->apply_unsafe($i1);$Mj->apply_unsafe($i1);$jk->apply_unsafe($j1);$rk->apply_unsafe($j1);$Ak->apply_unsafe($j1);$Nk->apply_unsafe($j1);$Nk->apply_unsafe($k1);$Nk->apply_unsafe($l1);$Nk->apply_unsafe($n1);$Nk->apply_unsafe($o1);$fl->apply_unsafe($j1);$Fl->apply_unsafe($k1);$Nl->apply_unsafe($k1);$cm->apply_unsafe($k1);$xm->apply_unsafe($l1);$Fm->apply_unsafe($l1);$Qm->apply_unsafe($l1);$Gn->apply_unsafe($n1);$On->apply_unsafe($n1);$Tn->apply_unsafe($n1);$go->apply_unsafe($n1);$Ho->apply_unsafe($o1);$Po->apply_unsafe($o1);$Yo->apply_unsafe($o1);$lp->apply_unsafe($o1);$Rp->apply_unsafe($q1);$Zp->apply_unsafe($q1);$qq->apply_unsafe($q1);$ni::self=$sr;&$_($K)for@$L;&$_($N)for@$O;&$_($A1)for@$B1;&$_($J1)for@$K1;&$_($N1)for@$K1;&$_($Q1)for@$U1;&$_($X1)for@$K1;&$_($c2)for@$K1;&$_($f2)for@$K1;&$_($i2)for@$j2;&$_($o2)for@$K1;&$_($r2)for@$K1;&$_($v2)for@$K1;&$_($y2)for@$z2;&$_($E2)for@$K1;&$_($I2)for@$K1;&$_($L2)for@$M2;&$_($R2)for@$K1;&$_($U2)for@$V2;&$_($X2)for@$Y2;&$_($c3)for@$K1;&$_($f3)for@$j3;&$_($l3)for@$m3;&$_($o3)for@$p3;&$_($s3)for@$K1;&$_($u3)for@$K1;&$_($x3)for@$B3;&$_($D3)for@$K1;&$_($F3)for@$K1;&$_($N3)for@$O3;&$_($R3)for@$K1;&$_($U3)for@$V3;&$_($Z3)for@$K1;&$_($e4)for@$f4;&$_($h4)for@$i4;&$_($m4)for@$K1;&$_($p4)for@$t4;&$_($v4)for@$w4;&$_($y4)for@$z4;&$_($B4)for@$C4;&$_($N4)for@$O4;&$_($S4)for@$K1;&$_($V4)for@$W4;&$_($Z4)for@$K1;&$_($e5)for@$f5;&$_($h5)for@$i5;&$_($n5)for@$K1;&$_($p5)for@$K1;&$_($s5)for@$w5;&$_($A5)for@$K1;&$_($D5)for@$H5;&$_($L5)for@$K1;&$_($O5)for@$S5;&$_($W5)for@$K1;&$_($Z5)for@$f6;&$_($h6)for@$i6;&$_($l6)for@$K1;&$_($n6)for@$K1;&$_($q6)for@$u6;&$_($x6)for@$y6;&$_($C6)for@$K1;&$_($F6)for@$G6;&$_($I6)for@$J6;&$_($S6)for@$K1;&$_($V6)for@$Z6;&$_($f7)for@$K1;&$_($i7)for@$K1;&$_($l7)for@$p7;&$_($s7)for@$K1;&$_($v7)for@$z7;&$_($C7)for@$K1;&$_($F7)for@$J7;&$_($N7)for@$K1;&$_($Q7)for@$U7;&$_($W7)for@$X7;&$_($Z7)for@$c8;&$_($g8)for@$K1;&$_($j8)for@$n8;&$_($p8)for@$q8;&$_($s8)for@$t8;&$_($w8)for@$x8;&$_($A8)for@$B8;&$_($H8)for@$K1;&$_($L8)for@$K1;&$_($O8)for@$P8;&$_($U8)for@$K1;&$_($Y8)for@$K1;&$_($d9)for@$e9;&$_($j9)for@$K1;&$_($n9)for@$K1;&$_($r9)for@$K1;&$_($v9)for@$K1;&$_($z9)for@$K1;&$_($C9)for@$D9;&$_($H9)for@$K1;&$_($L9)for@$K1;&$_($O9)for@$P9;&$_($U9)for@$K1;&$_($Y9)for@$K1;&$_($da)for@$ea;&$_($ga)for@$ha;&$_($ma)for@$na;&$_($ra)for@$K1;&$_($ua)for@$va;&$_($ya)for@$K1;&$_($Ba)for@$Ca;&$_($Ga)for@$K1;&$_($Ja)for@$Ka;&$_($Oa)for@$K1;&$_($Ra)for@$K1;&$_($Ua)for@$Va;&$_($Xa)for@$Ya;&$_($pb)for@$qb;&$_($xb)for@$K1;&$_($Ab)for@$K1;&$_($Db)for@$qb;&$_($Lb)for@$K1;&$_($Ob)for@$qb;&$_($gc)for@$hc;&$_($lc)for@$K1;&$_($oc)for@$pc;&$_($sc)for@$K1;&$_($vc)for@$wc;&$_($zc)for@$Ac;&$_($cd)for@$K1;&$_($fd)for@$hd;&$_($ld)for@$md;&$_($rd)for@$K1;&$_($ud)for@$vd;&$_($Ad)for@$K1;&$_($Ed)for@$K1;&$_($Id)for@$K1;&$_($Md)for@$K1;&$_($Qd)for@$K1;&$_($Ud)for@$K1;&$_($Yd)for@$K1;&$_($ee)for@$K1;&$_($ie)for@$K1;&$_($me)for@$K1;&$_($qe)for@$K1;&$_($ue)for@$K1;&$_($ye)for@$K1;&$_($Ce)for@$K1;&$_($Ge)for@$K1;&$_($Ke)for@$K1;&$_($Oe)for@$K1;&$_($Se)for@$K1;&$_($Ve)for@$We;&$_($Ye)for@$Ze;&$_($mf)for@$K1;&$_($qf)for@$K1;&$_($uf)for@$K1;&$_($yf)for@$K1;&$_($Cf)for@$K1;&$_($Ff)for@$Hf;&$_($Yf)for@$K1;&$_($eg)for@$K1;&$_($ig)for@$K1;&$_($mg)for@$K1;&$_($qg)for@$K1;&$_($ug)for@$K1;&$_($yg)for@$K1;&$_($Cg)for@$K1;&$_($Gg)for@$K1;&$_($Kg)for@$K1;&$_($Og)for@$K1;&$_($Sg)for@$K1;&$_($Vg)for@$Wg;&$_($lh)for@$K1;&$_($qh)for@$K1;&$_($th)for@$uh;&$_($Fh)for@$Gh;&$_($Lh)for@$K1;&$_($Oh)for@$Ph;&$_($Uh)for@$K1;&$_($Yh)for@$K1;&$_($di)for@$ei;&$_($ii)for@$K1;&$_($li)for@$mi;&$_($pi)for@$qi;&$_($Gi)for@$Hi;&$_($Ji)for@$Ki;&$_($Ri)for@$K1;&$_($Ui)for@$Vi;&$_($cj)for@$K1;&$_($fj)for@$gj;&$_($lj)for@$K1;&$_($pj)for@$K1;&$_($sj)for@$tj;&$_($vj)for@$wj;&$_($Aj)for@$K1;&$_($Dj)for@$Ej;&$_($Jj)for@$K1;&$_($Mj)for@$Nj;&$_($Pj)for@$Qj;&$_($Xj)for@$Yj;&$_($gk)for@$K1;&$_($jk)for@$kk;&$_($ok)for@$K1;&$_($rk)for@$sk;&$_($xk)for@$K1;&$_($Ak)for@$Bk;&$_($Hk)for@$K1;&$_($Kk)for@$K1;&$_($Nk)for@$Ok;&$_($Qk)for@$Rk;&$_($Wk)for@$K1;&$_($cl)for@$K1;&$_($fl)for@$gl;&$_($il)for@$jl;&$_($tl)for@$ul;&$_($zl)for@$K1;&$_($Cl)for@$K1;&$_($Fl)for@$Gl;&$_($Kl)for@$K1;&$_($Nl)for@$Ol;&$_($Tl)for@$K1;&$_($Xl)for@$K1;&$_($cm)for@$dm;&$_($fm)for@$gm;&$_($om)for@$pm;&$_($um)for@$K1;&$_($xm)for@$ym;&$_($Cm)for@$K1;&$_($Fm)for@$Gm;&$_($Km)for@$K1;&$_($Nm)for@$K1;&$_($Qm)for@$Rm;&$_($Tm)for@$Um;&$_($kn)for@$ln;&$_($rn)for@$K1;&$_($vn)for@$K1;&$_($zn)for@$K1;&$_($Dn)for@$K1;&$_($Gn)for@$Hn;&$_($Ln)for@$K1;&$_($On)for@$Pn;&$_($Tn)for@$Un;&$_($Yn)for@$K1;&$_($do)for@$K1;&$_($go)for@$ho;&$_($jo)for@$ko;&$_($vo)for@$wo;&$_($Bo)for@$K1;&$_($Eo)for@$K1;&$_($Ho)for@$Io;&$_($Mo)for@$K1;&$_($Po)for@$Qo;&$_($Vo)for@$K1;&$_($Yo)for@$Zo;&$_($fp)for@$K1;&$_($ip)for@$K1;&$_($lp)for@$mp;&$_($pp)for@$qp;&$_($zp)for@$Ap;&$_($Gp)for@$K1;&$_($Kp)for@$K1;&$_($Op)for@$K1;&$_($Rp)for@$Sp;&$_($Wp)for@$K1;&$_($Zp)for@$cq;&$_($gq)for@$K1;&$_($kq)for@$K1;&$_($nq)for@$K1;&$_($qq)for@$rq;&$_($tq)for@$uq;&$_($Cq)for@$Dq;&$_($Yq)for@$Zq;&$_($dr)for@$Dq;&$_($ir)for@$K1;&$_($mr)for@$K1;&$_($qr)for@$K1;&$_($sr)for@$tr;ni->run(@ARGV);
__DATA__
