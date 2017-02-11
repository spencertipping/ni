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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/module.c';$k={$h,1,$j,1};$l=q'/lib/behavior.c';$m=q'/lib/branch.c';$n=q'/lib/dataslice.c';$o=q'/lib/doc.c';$p=q'/lib/fn.c';$q=q'/lib/image.c';$r=q'/lib/ni.c';$s=q'/lib/slice.c';$t=q'/lib/tag.c';$u=q'/lib/test_value.c';$v=q'/object.c';$w=q'/unix/cat.c';$x=q'/unix/fd.c';$y=q'/unix/fifo.c';$z=q'/unix/file.c';$A=q'/unix/io.c';$B=q'/unix/pid.c';$C=q'/unix/pipeline.c';$D=q'/unix/str.c';$E={$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$F=[undef];$G=q'/metaclass';$H=bless({$c,$E,$f,$v,$g,$F},$G);$I=q'/metaclass::ctors';$J={$s,1};$K=q'/lib/dataslice.c';$L={$h,1,$l,1,$m,1,$K,1,$s,1,$t,1,$j,1};$M=[$H];$N=bless({$c,$L,$f,$l,$g,$M},$G);$O=q'/metaclass::ctors';$P=[$N];$Q=bless({$c,$J,$f,$s,$g,$P},$G);$R=q'/metaclass::ctors';$S=q'/lib/slice';$T={$S,1};$U=q'/class';$V=q'/lib/behavior';$W=q'/lib/branch';$X=q'/lib/dataslice';$Y=q'/lib/dataslice.c';$Z=q'/lib/tag';$c1=q'/lib/test_value.c';$d1=q'/module';$e1=q'/unix/pipeline.c';$f1={$U,1,$h,1,$V,1,$l,1,$W,1,$m,1,$X,1,$Y,1,$o,1,$p,1,$q,1,$r,1,$S,1,$s,1,$Z,1,$t,1,$c1,1,$G,1,$d,1,$d1,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$e1,1,$D,1};$g1=q'/lib/dataslice.c';$h1=q'/lib/doc';$i1=q'/lib/fn';$j1=q'/lib/image';$k1=q'/lib/ni';$l1=q'/lib/test_value';$m1=q'/lib/test_value.c';$n1=q'/object';$o1=q'/unix/cat';$p1=q'/unix/fd';$q1=q'/unix/fifo';$r1=q'/unix/file';$s1=q'/unix/io';$t1=q'/unix/pid';$u1=q'/unix/pipeline';$v1=q'/unix/pipeline.c';$w1=q'/unix/str';$x1={$U,1,$h,1,$V,1,$l,1,$W,1,$m,1,$X,1,$g1,1,$h1,1,$o,1,$i1,1,$p,1,$j1,1,$q,1,$k1,1,$r,1,$S,1,$s,1,$Z,1,$t,1,$l1,1,$m1,1,$G,1,$d,1,$d1,1,$j,1,$n1,1,$v,1,$o1,1,$w,1,$p1,1,$x,1,$q1,1,$y,1,$r1,1,$z,1,$s1,1,$A,1,$t1,1,$B,1,$u1,1,$v1,1,$w1,1,$D,1};$y1={};$z1=q'ctor';$A1=undef;$B1=q'dtor';$C1=q'methods';$D1=q'class';$E1={$p,1};$F1=[$H];$G1=bless({$c,$E1,$f,$p,$g,$F1},$G);$H1=q'/metaclass::ctors';$I1={$i1,1};$J1={};$K1=q'code';$L1=q'shift->compile';$M1=q'proto';$N1=q'';$O1=bless({$K1,$L1,$M1,$N1},$i1);$P1=q'/lib/fn::ctors';$Q1=q'compile';$R1=q'local $@;
my $self = shift;
$$self{proto} ||= \'\';
$$self{fn} = ni::eval "sub $$self{proto} {$$self{code}\\n}";
die "ni:/lib/fn: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$S1=bless({$K1,$R1,$M1,$N1},$i1);$T1=q'instantiate';$U1=q'my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : \'\';
+{code        => $code,
  proto       => $proto,
  annotations => [@_]};';$V1=bless({$K1,$U1,$M1,$N1},$i1);$W1={$Q1,$S1,$T1,$V1};$X1=q'/lib/fn_init.b';$Y1=bless({$c,$J1,$z1,$O1,$B1,$A1,$C1,$W1,$f,$X1},$S);$Z1=q'/lib/slice::ctors';$c2={};$d2=q'annotations';$e2=[];$f2=q'shift->{\'annotations\'}';$g2=bless({$d2,$e2,$K1,$f2,$M1,$N1},$i1);$h2=[];$i2=q'shift->{\'code\'}';$j2=bless({$d2,$h2,$K1,$i2,$M1,$N1},$i1);$k2=q'fn';$l2=[];$m2=q'shift->{\'fn\'}';$n2=bless({$d2,$l2,$K1,$m2,$M1,$N1},$i1);$o2={$d2,$g2,$K1,$j2,$k2,$n2};$p2=q'/lib/fn_ro.b';$q2=bless({$c,$c2,$z1,$A1,$B1,$A1,$C1,$o2,$f,$p2},$S);$r2=q'/lib/slice::ctors';$s2={};$t2=q'(""';$u2=[];$v2=q'shift->{code}';$w2=bless({$d2,$u2,$K1,$v2,$M1,$N1},$i1);$x2=q'(eq';$y2=[];$z2=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])';$A2=bless({$d2,$y2,$K1,$z2,$M1,$N1},$i1);$B2={$t2,$w2,$x2,$A2};$C2=q'/lib/fn_ops.b';$D2=bless({$c,$s2,$z1,$A1,$B1,$A1,$C1,$B2,$f,$C2},$S);$E2=q'/lib/slice::ctors';$F2={};$G2=q'serialize';$H2=[];$I2=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$J2=bless({$d2,$H2,$K1,$I2,$M1,$N1},$i1);$K2={$G2,$J2};$L2=q'/lib/fn_serialize.b';$M2=bless({$c,$F2,$z1,$A1,$B1,$A1,$C1,$K2,$f,$L2},$S);$N2=q'/lib/slice::ctors';$O2=[undef,undef,$Y1,$q2,$D2,$M2];$P2=bless({$c,$I1,$f,$i1,$g,$O2},$p);$Q2=q'/lib/fn.c::ctors';$R2=q'ni \'ni:\' . ref shift';$S2=bless({$K1,$R2,$M1,$N1},$i1);$T2={$D1,$S2};$U2=q'/lib/instance.b';$V2=bless({$c,$y1,$z1,$A1,$B1,$A1,$C1,$T2,$f,$U2},$S);$W2=q'/lib/dataslice.c';$X2=q'/lib/test_value.c';$Y2=q'/unix/pipeline.c';$Z2=q'/lib/slice::ctors';$c3=[$V2];$d3=bless({$c,$x1,$f,$n1,$g,$c3},$v);$e3=q'/object.c::ctors';$f3={};$g3=q'doc';$h3=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:/lib/doc\')->new($name);';$i3=bless({$K1,$h3,$M1,$N1},$i1);$j3={$g3,$i3};$k3=q'/lib/documentable.b';$l3=bless({$c,$f3,$z1,$A1,$B1,$A1,$C1,$j3,$f,$k3},$S);$m3=q'/lib/dataslice.c';$n3=q'/lib/test_value.c';$o3=q'/unix/pipeline.c';$p3=q'/lib/slice::ctors';$q3=[$d3,$l3];$r3=bless({$c,$f1,$f,$V,$g,$q3},$l);$s3=q'/lib/behavior.c::ctors';$t3={};$u3=q'my $s = shift; ni->def($s->name, $s)';$v3=bless({$K1,$u3,$M1,$N1},$i1);$w3=q'$_[0]->namespace . ":" . $_[0]->{name}';$x3=bless({$K1,$w3,$M1,$N1},$i1);$y3={$f,$x3};$z3=q'/lib/named.b';$A3=bless({$c,$t3,$z1,$v3,$B1,$A1,$C1,$y3,$f,$z3},$S);$B3=q'/lib/dataslice.c';$C3=q'/lib/test_value.c';$D3=q'/unix/pipeline.c';$E3=q'/lib/slice::ctors';$F3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$G3=bless({$K1,$F3,$M1,$N1},$i1);$H3=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$I3=bless({$K1,$H3,$M1,$N1},$i1);$J3=q'/lib/slice::apply';$K3=q'/lib/slice::apply_unsafe';$L3={};$M3=q'apply';$N3=q'apply_unsafe';$O3={$M3,$G3,$N3,$I3};$P3=q'/lib/slice.b';$Q3=bless({$c,$L3,$C1,$O3,$f,$P3},$S);$R3=q'/lib/slice::ctors';$S3={};$T3=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$U3=bless({$K1,$T3,$M1,$N1},$i1);$V3={$T1,$U3};$W3=q'/lib/slice_init.b';$X3=bless({$c,$S3,$C1,$V3,$f,$W3},$S);$Y3=q'/lib/slice::ctors';$Z3={};$c4=[];$d4=q'local $_;
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
$g;';$e4=bless({$d2,$c4,$K1,$d4,$M1,$N1},$i1);$f4={$G2,$e4};$g4=q'/lib/slice_serialize.b';$h4=bless({$c,$Z3,$z1,$A1,$B1,$A1,$C1,$f4,$f,$g4},$S);$i4=q'/lib/slice::ctors';$j4=[$r3,$A3,$Q3,$X3,$h4];$k4=bless({$c,$T,$f,$S,$g,$j4},$s);$l4=q'/lib/slice.c::ctors';$m4={};$n4=q'DESTROY';$o4=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$p4=bless({$K1,$o4,$M1,$N1},$i1);$q4=q'new';$r4=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$s4=bless({$K1,$r4,$M1,$N1},$i1);$t4={$n4,$p4,$q4,$s4};$u4=q'/lib/instantiable.b';$v4=bless({$c,$m4,$C1,$t4,$f,$u4},$S);$w4=q'/lib/dataslice.c';$x4=q'/lib/test_value.c';$y4=q'/unix/pipeline.c';$z4=q'/lib/slice::ctors';$A4=[$H,$v4,$N];$B4=bless({$c,$k,$f,$j,$g,$A4},$G);$C4=q'/metaclass::ctors';$D4=[$B4];$E4=bless({$c,$i,$f,$h,$g,$D4},$G);$F4=q'/metaclass::ctors';$G4=q'/lib/dataslice.c';$H4=q'/lib/test_value.c';$I4=q'/unix/pipeline.c';$J4={$U,1,$h,1,$l,1,$m,1,$G4,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$H4,1,$d,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$I4,1,$D,1};$K4=q'/lib/dataslice.c';$L4=q'/lib/test_value.c';$M4=q'/unix/pipeline.c';$N4={$U,1,$h,1,$l,1,$m,1,$K4,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$L4,1,$G,1,$d,1,$d1,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$M4,1,$D,1};$O4={$t,1};$P4=[$N];$Q4=bless({$c,$O4,$f,$t,$g,$P4},$G);$R4=q'/metaclass::ctors';$S4={$Z,1};$T4={};$U4=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$V4=bless({$K1,$U4,$M1,$N1},$i1);$W4={$M3,$V4};$X4=q'/lib/tag.b';$Y4=bless({$c,$T4,$z1,$A1,$B1,$A1,$C1,$W4,$f,$X4},$S);$Z4=q'/lib/slice::ctors';$c5={};$d5=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$e5=bless({$K1,$d5,$M1,$N1},$i1);$f5={$T1,$e5};$g5=q'/lib/tag_init.b';$h5=bless({$c,$c5,$z1,$A1,$B1,$A1,$C1,$f5,$f,$g5},$S);$i5=q'/lib/slice::ctors';$j5=[$r3,$A3,$Y4,$h5];$k5=bless({$c,$S4,$f,$Z,$g,$j5},$t);$l5=q'/lib/tag.c::ctors';$m5=q'/lib/perlbranch.b';$n5={};$o5=q'add';$p5=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$q5=bless({$K1,$p5,$M1,$N1},$i1);$r5=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$s5=bless({$K1,$r5,$M1,$N1},$i1);$t5={$o5,$q5,$M3,$s5};$u5=q'/lib/branch.b';$v5=bless({$c,$n5,$z1,$A1,$B1,$A1,$C1,$t5,$f,$u5},$S);$w5=q'/lib/dataslice.c';$x5=q'/lib/test_value.c';$y5=q'/unix/pipeline.c';$z5=q'/lib/slice::ctors';$A5={};$B5=q'namespace';$C5=q'\'ni\'';$D5=bless({$K1,$C5,$M1,$N1},$i1);$E5={$B5,$D5};$F5=q'/lib/named_in_ni.b';$G5=bless({$c,$A5,$z1,$A1,$B1,$A1,$C1,$E5,$f,$F5},$S);$H5=q'/lib/dataslice.c';$I5=q'/lib/test_value.c';$J5=q'/unix/pipeline.c';$K5=q'/lib/slice::ctors';$L5={};$M5=q'package';$N5=q'shift->{name}';$O5=bless({$K1,$N5,$M1,$N1},$i1);$P5={$M5,$O5};$Q5=q'/lib/namespaced.b';$R5=bless({$c,$L5,$z1,$A1,$B1,$A1,$C1,$P5,$f,$Q5},$S);$S5=q'/lib/dataslice.c';$T5=q'/lib/test_value.c';$U5=q'/unix/pipeline.c';$V5=q'/lib/slice::ctors';$W5={};$X5=q'resolve';$Y5=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$Z5=bless({$K1,$Y5,$M1,$N1},$i1);$c6={$X5,$Z5};$d6=q'/lib/resolver.b';$e6=bless({$c,$W5,$z1,$A1,$B1,$A1,$C1,$c6,$f,$d6},$S);$f6=q'/lib/dataslice.c';$g6=q'/lib/test_value.c';$h6=q'/unix/pipeline.c';$i6=q'/lib/slice::ctors';$j6=[$v5,$A3,$G5,$R5,$e6];$k6=bless({$f,$m5,$g,$j6},$Z);$l6=q'/lib/tag::ctors';$m6={};$n6=q'my $s = shift; $s->apply($s->package)';$o6=bless({$K1,$n6,$M1,$N1},$i1);$p6=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$q6=bless({$K1,$p6,$M1,$N1},$i1);$r6={$T1,$q6};$s6=q'/lib/class_init.b';$t6=bless({$c,$m6,$z1,$o6,$B1,$A1,$C1,$r6,$f,$s6},$S);$u6=q'/lib/dataslice.c';$v6=q'/lib/test_value.c';$w6=q'/unix/pipeline.c';$x6=q'/lib/slice::ctors';$y6={$m,1};$z6=[$N];$A6=bless({$c,$y6,$f,$m,$g,$z6},$G);$B6=q'/metaclass::ctors';$C6={$W,1};$D6={};$E6=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$F6=bless({$K1,$E6,$M1,$N1},$i1);$G6={$T1,$F6};$H6=q'/lib/branch_init.b';$I6=bless({$c,$D6,$z1,$A1,$B1,$A1,$C1,$G6,$f,$H6},$S);$J6=q'/lib/slice::ctors';$K6=[$r3,$A3,$v5,$I6,undef];$L6=bless({$c,$C6,$f,$W,$g,$K6},$m);$M6=q'/lib/branch.c::ctors';$N6=q'/lib/dataslice.c';$O6=q'/lib/test_value.c';$P6=q'/unix/pipeline.c';$Q6={$U,1,$h,1,$l,1,$W,1,$m,1,$N6,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$O6,1,$G,1,$d,1,$d1,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$P6,1,$D,1};$R6=q'/lib/definition.b';$S6={};$T6=q'def';$U6=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$V6=bless({$K1,$U6,$M1,$N1},$i1);$W6={$T6,$V6};$X6=q'/lib/definition_def.b';$Y6=bless({$c,$S6,$z1,$A1,$B1,$A1,$C1,$W6,$f,$X6},$S);$Z6=q'/lib/dataslice.c';$c7=q'/lib/test_value.c';$d7=q'/unix/pipeline.c';$e7=q'/lib/slice::ctors';$f7={};$g7=q'ro';$h7=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$i7=bless({$K1,$h7,$M1,$N1},$i1);$j7=q'rw';$k7=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$l7=bless({$K1,$k7,$M1,$N1},$i1);$m7={$g7,$i7,$j7,$l7};$n7=q'/lib/accessor.b';$o7=bless({$c,$f7,$z1,$A1,$B1,$A1,$C1,$m7,$f,$n7},$S);$p7=q'/lib/dataslice.c';$q7=q'/lib/test_value.c';$r7=q'/unix/pipeline.c';$s7=q'/lib/slice::ctors';$t7={};$u7=q'shift->name';$v7=bless({$K1,$u7,$M1,$N1},$i1);$w7={$t2,$v7};$x7=q'/lib/name_as_string.b';$y7=bless({$c,$t7,$z1,$A1,$B1,$A1,$C1,$w7,$f,$x7},$S);$z7=q'/lib/dataslice.c';$A7=q'/lib/test_value.c';$B7=q'/unix/pipeline.c';$C7=q'/lib/slice::ctors';$D7={};$E7=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);';$F7=bless({$K1,$E7,$M1,$N1},$i1);$G7={$x2,$F7};$H7=q'/lib/ref_eq.b';$I7=bless({$c,$D7,$z1,$A1,$B1,$A1,$C1,$G7,$f,$H7},$S);$J7=q'/lib/dataslice.c';$K7=q'/lib/test_value.c';$L7=q'/unix/pipeline.c';$M7=q'/lib/slice::ctors';$N7={};$O7=q'defdata';$P7=q'shift->add(ni(\'ni:/lib/dataslice\')->new(@_))';$Q7=bless({$K1,$P7,$M1,$N1},$i1);$R7={$O7,$Q7};$S7=q'/lib/definition_defdata.b';$T7=bless({$c,$N7,$z1,$A1,$B1,$A1,$C1,$R7,$f,$S7},$S);$U7=q'/lib/dataslice.c';$V7=q'/lib/test_value.c';$W7=q'/unix/pipeline.c';$X7=q'/lib/slice::ctors';$Y7=[$Y6,$o7,$y7,$I7,$T7];$Z7=bless({$c,$Q6,$f,$R6,$g,$Y7},$W);$c8=q'/lib/branch::ctors';$d8=[$k6,$t6,$d3,$r3,$Z7];$e8=bless({$c,$N4,$f,$d1,$g,$d8},$j);$f8=q'/module.c::ctors';$g8={};$h8=q'child';$i8=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$j8=bless({$K1,$i8,$M1,$N1},$i1);$k8={$h8,$j8};$l8=q'/lib/subclass.b';$m8=bless({$c,$g8,$z1,$A1,$B1,$A1,$C1,$k8,$f,$l8},$S);$n8=q'/lib/dataslice.c';$o8=q'/lib/test_value.c';$p8=q'/unix/pipeline.c';$q8=q'/lib/slice::ctors';$r8=[$e8,$v4,$t6,$e8,$m8];$s8=bless({$c,$J4,$f,$U,$g,$r8},$h);$t8=q'/class.c::ctors';$u8=[$s8];$v8=bless({$c,$e,$f,$d,$g,$u8},$G);$w8=q'/metaclass::ctors';$x8={$G,1};$y8=[$k6,$v4,$t6,$e8];$z8=bless({$c,$x8,$f,$G,$g,$y8},$d);$A8=q'/metaclass.c::ctors';$B8={$r,1};$C8=[$H];$D8=bless({$c,$B8,$f,$r,$g,$C8},$G);$E8=q'/metaclass::ctors';$F8={$k1,1};$G8={};$H8=q'is_mutable';$I8=[];$J8=q'$0 ne "-" && -w $0';$K8=bless({$d2,$I8,$K1,$J8,$M1,$N1},$i1);$L8=q'modify';$M8=[];$N8=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$O8=bless({$d2,$M8,$K1,$N8,$M1,$N1},$i1);$P8={$H8,$K8,$L8,$O8};$Q8=q'/lib/ni_self.b';$R8=bless({$c,$G8,$z1,$A1,$B1,$A1,$C1,$P8,$f,$Q8},$S);$S8=q'/lib/slice::ctors';$T8={};$U8=q'exists';$V8=[];$W8=q'exists $_[0]->{named}{$_[1]}';$X8=bless({$d2,$V8,$K1,$W8,$M1,$N1},$i1);$Y8=q'quoted';$Z8=[];$c9=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$d9=bless({$d2,$Z8,$K1,$c9,$M1,$N1},$i1);$e9={$U8,$X8,$Y8,$d9};$f9=q'/lib/ni_image.b';$g9=bless({$c,$T8,$z1,$A1,$B1,$A1,$C1,$e9,$f,$f9},$S);$h9=q'/lib/slice::ctors';$i9={};$j9=q'--internal/+=';$k9=[];$l9=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$m9=bless({$d2,$k9,$K1,$l9,$M1,$N1},$i1);$n9=q'--internal/eval';$o9=[];$p9=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$q9=bless({$d2,$o9,$K1,$p9,$M1,$N1},$i1);$r9=q'--internal/image';$s9=[];$t9=q'shift->quoted->write(\\*STDOUT);
0;';$u9=bless({$d2,$s9,$K1,$t9,$M1,$N1},$i1);$v9=q'--internal/test';$w9=[];$x9=q'my $self = shift;
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
!!$fails;';$y9=bless({$d2,$w9,$K1,$x9,$M1,$N1},$i1);$z9=q'run';$A9=[];$B9=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$C9=bless({$d2,$A9,$K1,$B9,$M1,$N1},$i1);$D9={$j9,$m9,$n9,$q9,$r9,$u9,$v9,$y9,$z9,$C9};$E9=q'/lib/ni_main.b';$F9=bless({$c,$i9,$z1,$A1,$B1,$A1,$C1,$D9,$f,$E9},$S);$G9=q'/lib/slice::ctors';$H9={};$I9=[];$J9=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$K9=bless({$d2,$I9,$K1,$J9,$M1,$N1},$i1);$L9=q'resolver_for';$M9=[];$N9=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$O9=bless({$d2,$M9,$K1,$N9,$M1,$N1},$i1);$P9={$X5,$K9,$L9,$O9};$Q9=q'/lib/ni_resolver.b';$R9=bless({$c,$H9,$z1,$A1,$B1,$A1,$C1,$P9,$f,$Q9},$S);$S9=q'/lib/slice::ctors';$T9={};$U9=q'fork';$V9=[];$W9=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$X9=bless({$d2,$V9,$K1,$W9,$M1,$N1},$i1);$Y9=q'fork_exec';$Z9=[];$ca=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$da=bless({$d2,$Z9,$K1,$ca,$M1,$N1},$i1);$ea={$U9,$X9,$Y9,$da};$fa=q'/lib/ni_pid_ctors';$ga=bless({$c,$T9,$z1,$A1,$B1,$A1,$C1,$ea,$f,$fa},$S);$ha=q'/lib/slice::ctors';$ia=[$d3,$R8,$g9,$F9,$R9,$ga];$ja=bless({$c,$F8,$f,$k1,$g,$ia},$r);$ka=q'/lib/ni.c::ctors';$la=q'named';$ma=q'ni.doc:/class';$na={$o,1};$oa=[$H];$pa=bless({$c,$na,$f,$o,$g,$oa},$G);$qa=q'/metaclass::ctors';$ra={$h1,1};$sa={};$ta=q'shift; +{name => shift, doc => []}';$ua=bless({$K1,$ta,$M1,$N1},$i1);$va={$T1,$ua};$wa=q'/lib/doc_init.b';$xa=bless({$c,$sa,$z1,$A1,$B1,$A1,$C1,$va,$f,$wa},$S);$ya=q'/lib/slice::ctors';$za={};$Aa=q'\'ni.doc\'';$Ba=bless({$K1,$Aa,$M1,$N1},$i1);$Ca={$B5,$Ba};$Da=q'/lib/doc_namespace.b';$Ea=bless({$c,$za,$z1,$A1,$B1,$A1,$C1,$Ca,$f,$Da},$S);$Fa=q'/lib/slice::ctors';$Ga={};$Ha=q'AUTOLOAD';$Ia=q'my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;';$Ja=bless({$K1,$Ia,$M1,$N1},$i1);$Ka={$Ha,$Ja};$La=q'/lib/doc_define.b';$Ma=bless({$c,$Ga,$z1,$A1,$B1,$A1,$C1,$Ka,$f,$La},$S);$Na=q'/lib/slice::ctors';$Oa={};$Pa=q'eg';$Qa=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$Ra=bless({$K1,$Qa,$M1,$N1},$i1);$Sa=q'tests';$Ta=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$Ua=bless({$K1,$Ta,$M1,$N1},$i1);$Va={$Pa,$Ra,$Sa,$Ua};$Wa=q'/lib/doc_test.b';$Xa=bless({$c,$Oa,$z1,$A1,$B1,$A1,$C1,$Va,$f,$Wa},$S);$Ya=q'/lib/slice::ctors';$Za=[$d3,$A3,$xa,$Ea,$Ma,$Xa];$cb=bless({$c,$ra,$f,$h1,$g,$Za},$o);$db=q'/lib/doc.c::ctors';$eb=q'synopsis';$fb=q'
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
  ';$gb=[$eb,$fb];$hb=q'description';$ib=q'ni:/class is at the core of ni\'s object-oriented system, along with core
      classes like ni:/object and ni:/metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$jb=[$hb,$ib];$kb=q'behaviors';$lb=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:/lib/slice, which represents a set of methods you can add to a
      package. TODO...';$mb=[$kb,$lb];$nb=q'classes';$ob=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$pb=q'TODO...';$qb=[$nb,$ob,$pb];$rb=[$gb,$jb,$mb,$qb];$sb=bless({$g3,$rb,$f,$U},$h1);$tb=q'/lib/doc::ctors';$ub=q'ni.doc:/lib/doc';$vb=q'
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$wb=[$eb,$vb];$xb=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$yb=q'Documentation objects are internally represented as arrays of quoted
      method calls:';$zb=q'my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];';$Ab=bless({$K1,$zb,$M1,$N1},$i1);$Bb=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":';$Cb=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] eq $passing_test
            && $tests[1] eq $failing_test;';$Db=bless({$K1,$Cb,$M1,$N1},$i1);$Eb=[$hb,$xb,$yb,$Pa,$Ab,$Bb,$Pa,$Db];$Fb=[$wb,$Eb];$Gb=bless({$g3,$Fb,$f,$h1},$h1);$Hb=q'ni.doc:/unix/cat';$Ib=q'
    my $combined = ni(\'ni:/unix/cat\')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into($destination_io);
  ';$Jb=[$eb,$Ib];$Kb=q'Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.';$Lb=[$hb,$Kb];$Mb=[];$Nb=q'my $cat = ni("str:foo") + ni("str:bar");
my $dest = ni(\'ni:/unix/str\')->new(my $data = "");
$cat->into($dest);
now ${$dest->data} == "foo\\nbar\\n";';$Ob=bless({$d2,$Mb,$K1,$Nb,$M1,$N1},$i1);$Pb=[$Pa,$Ob];$Qb=[$Jb,$Lb,$Pb];$Rb=bless({$g3,$Qb,$f,$o1},$h1);$Sb=q'ni:/class';$Tb=q'ni:/class.c';$Ub=q'ni:/lib/accessor.b';$Vb=q'ni:/lib/behavior';$Wb=q'ni:/lib/behavior.c';$Xb=q'ni:/lib/branch';$Yb=q'ni:/lib/branch.b';$Zb=q'ni:/lib/branch.c';$cc=q'ni:/lib/branch_init.b';$dc=q'ni:/lib/class_init.b';$ec=q'ni:/lib/dataslice';$fc=q'/lib/dataslice.c';$gc={$fc,1};$hc=q'/lib/dataslice.c';$ic=[$N];$jc=bless({$c,$gc,$f,$hc,$g,$ic},$G);$kc=q'/metaclass::ctors';$lc={$X,1};$mc={};$nc=q'my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};';$oc=bless({$K1,$nc,$M1,$N1},$i1);$pc={$T1,$oc};$qc=q'/lib/dataslice_init.b';$rc=bless({$c,$mc,$z1,$A1,$B1,$A1,$C1,$pc,$f,$qc},$S);$sc=q'/lib/slice::ctors';$tc={};$uc=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;';$vc=bless({$K1,$uc,$M1,$N1},$i1);$wc={$M3,$vc};$xc=q'/lib/dataslice_apply.b';$yc=bless({$c,$tc,$z1,$A1,$B1,$A1,$C1,$wc,$f,$xc},$S);$zc=q'/lib/slice::ctors';$Ac=[$r3,$rc,$yc];$Bc=q'/lib/dataslice.c';$Cc=bless({$c,$lc,$f,$X,$g,$Ac},$Bc);$Dc=q'/lib/dataslice.c::ctors';$Ec=q'ni:/lib/dataslice.c';$Fc=q'ni:/lib/dataslice_apply.b';$Gc=q'ni:/lib/dataslice_init.b';$Hc=q'ni:/lib/definition.b';$Ic=q'ni:/lib/definition_def.b';$Jc=q'ni:/lib/definition_defdata.b';$Kc=q'ni:/lib/doc';$Lc=q'ni:/lib/doc.c';$Mc=q'ni:/lib/doc_define.b';$Nc=q'ni:/lib/doc_init.b';$Oc=q'ni:/lib/doc_namespace.b';$Pc=q'ni:/lib/doc_test.b';$Qc=q'ni:/lib/documentable.b';$Rc=q'ni:/lib/fn';$Sc=q'ni:/lib/fn.c';$Tc=q'ni:/lib/fn_init.b';$Uc=q'ni:/lib/fn_ops.b';$Vc=q'ni:/lib/fn_ro.b';$Wc=q'ni:/lib/fn_serialize.b';$Xc=q'ni:/lib/global_static_test.b';$Yc={};$Zc=q'now';$cd=[];$dd=q'ni(\'ni:/lib/test_value\')->new(shift)';$ed=q'($)';$fd=bless({$d2,$cd,$K1,$dd,$M1,$ed},$i1);$gd={$Zc,$fd};$hd=q'/lib/global_static_test.b';$id=bless({$c,$Yc,$z1,$A1,$B1,$A1,$C1,$gd,$f,$hd},$S);$jd=q'main';$kd=q'/lib/slice::ctors';$ld=q'ni:/lib/image';$md={$q,1};$nd=[$H];$od=bless({$c,$md,$f,$q,$g,$nd},$G);$pd=q'/metaclass::ctors';$qd={$j1,1};$rd={};$sd=[];$td=q'my $class = shift;
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
  ordering     => []};';$ud=bless({$d2,$sd,$K1,$td,$M1,$N1},$i1);$vd={$T1,$ud};$wd=q'/lib/image_init.b';$xd=bless({$c,$rd,$z1,$A1,$B1,$A1,$C1,$vd,$f,$wd},$S);$yd=q'/lib/slice::ctors';$zd={};$Ad=q'address';$Bd=[];$Cd=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$Dd=bless({$d2,$Bd,$K1,$Cd,$M1,$N1},$i1);$Ed=q'allocate_gensym';$Fd=[];$Gd=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Hd=bless({$d2,$Fd,$K1,$Gd,$M1,$N1},$i1);$Id=q'boot_side_effect';$Jd=[];$Kd=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Ld=bless({$d2,$Jd,$K1,$Kd,$M1,$N1},$i1);$Md=q'circular_links';$Nd=[];$Od=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$Pd=bless({$d2,$Nd,$K1,$Od,$M1,$N1},$i1);$Qd=q'finalizer';$Rd=[];$Sd=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Td=bless({$d2,$Rd,$K1,$Sd,$M1,$N1},$i1);$Ud=q'gensym';$Vd=[];$Wd=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$Xd=bless({$d2,$Vd,$K1,$Wd,$M1,$N1},$i1);$Yd=q'is_circular';$Zd=[];$ce=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$de=bless({$d2,$Zd,$K1,$ce,$M1,$N1},$i1);$ee=q'quote';$fe=[];$ge=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$he=bless({$d2,$fe,$K1,$ge,$M1,$N1},$i1);$ie=q'quote_array';$je=[];$ke=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$le=bless({$d2,$je,$K1,$ke,$M1,$N1},$i1);$me=q'quote_blessed';$ne=[];$oe=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$pe=bless({$d2,$ne,$K1,$oe,$M1,$N1},$i1);$qe=q'quote_class';$re=[];$se=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$te=bless({$d2,$re,$K1,$se,$M1,$N1},$i1);$ue=q'quote_hash';$ve=[];$we=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$xe=bless({$d2,$ve,$K1,$we,$M1,$N1},$i1);$ye=q'quote_object';$ze=[];$Ae=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$Be=bless({$d2,$ze,$K1,$Ae,$M1,$N1},$i1);$Ce=q'quote_scalar';$De=[];$Ee=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Fe=bless({$d2,$De,$K1,$Ee,$M1,$N1},$i1);$Ge=q'quote_value';$He=[];$Ie=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Je=bless({$d2,$He,$K1,$Ie,$M1,$N1},$i1);$Ke=q'reconstruction';$Le=[];$Me=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Ne=bless({$d2,$Le,$K1,$Me,$M1,$N1},$i1);$Oe=q'side_effect';$Pe=[];$Qe=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Re=bless({$d2,$Pe,$K1,$Qe,$M1,$N1},$i1);$Se=q'write';$Te=[];$Ue=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Ve=bless({$d2,$Te,$K1,$Ue,$M1,$N1},$i1);$We={$Ad,$Dd,$Ed,$Hd,$Id,$Ld,$Md,$Pd,$Qd,$Td,$Ud,$Xd,$Yd,$de,$ee,$he,$ie,$le,$me,$pe,$qe,$te,$ue,$xe,$ye,$Be,$Ce,$Fe,$Ge,$Je,$Ke,$Ne,$Oe,$Re,$Se,$Ve};$Xe=q'/lib/image_quoting.b';$Ye=bless({$c,$zd,$z1,$A1,$B1,$A1,$C1,$We,$f,$Xe},$S);$Ze=q'/lib/slice::ctors';$cf=[$d3,$xd,$Ye];$df=bless({$c,$qd,$f,$j1,$g,$cf},$q);$ef=q'/lib/image.c::ctors';$ff=q'ni:/lib/image.c';$gf=q'ni:/lib/image_init.b';$hf=q'ni:/lib/image_quoting.b';$if=q'ni:/lib/instance.b';$jf=q'ni:/lib/instantiable.b';$kf=q'ni:/lib/json.b';$lf={};$mf=q'json_decode';$nf=[];$of=q'local $_;
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
wantarray ? @$r : $$r[0];';$pf=bless({$d2,$nf,$K1,$of,$M1,$ed},$i1);$qf=q'json_encode';$rf=[];$sf=q'local $_;
my ($v) = @_;
return "[" . join(\',\', map ni::json_encode($_), @$v) . "]" if \'ARRAY\' eq ref $v;
return "{" . join(\',\', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if \'HASH\' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : \'null\';';$tf=bless({$d2,$rf,$K1,$sf,$M1,$ed},$i1);$uf=q'json_escape';$vf=[];$wf=q'(my $x = $_[0]) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . $ni::json_escapes{$1}/eg;
"\\"$x\\"";';$xf=bless({$d2,$vf,$K1,$wf,$M1,$ed},$i1);$yf=q'json_unescape';$zf=[];$Af=q'my $x = substr $_[0], 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;';$Bf=bless({$d2,$zf,$K1,$Af,$M1,$ed},$i1);$Cf=q'json_unescape_one';$Df=[];$Ef=q'$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1';$Ff=bless({$d2,$Df,$K1,$Ef,$M1,$ed},$i1);$Gf={$mf,$pf,$qf,$tf,$uf,$xf,$yf,$Bf,$Cf,$Ff};$Hf=q'/lib/json.b';$If=bless({$c,$lf,$z1,$A1,$B1,$A1,$C1,$Gf,$f,$Hf},$S);$Jf=q'ni';$Kf=q'/lib/slice::ctors';$Lf=q'ni:/lib/name_as_string.b';$Mf=q'ni:/lib/named.b';$Nf=q'ni:/lib/named_in_ni.b';$Of=q'ni:/lib/namespaced.b';$Pf=q'ni:/lib/ni';$Qf=q'ni:/lib/ni.c';$Rf=q'ni:/lib/ni_image.b';$Sf=q'ni:/lib/ni_main.b';$Tf=q'ni:/lib/ni_pid_ctors';$Uf=q'ni:/lib/ni_resolver.b';$Vf=q'ni:/lib/ni_self.b';$Wf=q'ni:/lib/ni_static_util.b';$Xf={};$Yf=q'abbrev';$Zf=[];$cg=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$dg=bless({$d2,$Zf,$K1,$cg,$M1,$N1},$i1);$eg=q'dor';$fg=[];$gg=q'defined $_[0] ? $_[0] : $_[1]';$hg=bless({$d2,$fg,$K1,$gg,$M1,$N1},$i1);$ig=q'indent';$jg=[];$kg=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$lg=bless({$d2,$jg,$K1,$kg,$M1,$N1},$i1);$mg=q'max';$ng=[];$og=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$pg=bless({$d2,$ng,$K1,$og,$M1,$N1},$i1);$qg=q'maxstr';$rg=[];$sg=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$tg=bless({$d2,$rg,$K1,$sg,$M1,$N1},$i1);$ug=q'mean';$vg=[];$wg=q'sum(@_) / (@_ || 1)';$xg=bless({$d2,$vg,$K1,$wg,$M1,$N1},$i1);$yg=q'min';$zg=[];$Ag=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$Bg=bless({$d2,$zg,$K1,$Ag,$M1,$N1},$i1);$Cg=q'minstr';$Dg=[];$Eg=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Fg=bless({$d2,$Dg,$K1,$Eg,$M1,$N1},$i1);$Gg=q'sgr';$Hg=[];$Ig=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Jg=bless({$d2,$Hg,$K1,$Ig,$M1,$N1},$i1);$Kg=q'sr';$Lg=[];$Mg=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Ng=bless({$d2,$Lg,$K1,$Mg,$M1,$N1},$i1);$Og=q'sum';$Pg=[];$Qg=q'local $_; my $x = 0; $x += $_ for @_; $x';$Rg=bless({$d2,$Pg,$K1,$Qg,$M1,$N1},$i1);$Sg=q'swap';$Tg=[];$Ug=q'@_[0, 1] = @_[1, 0]';$Vg=bless({$d2,$Tg,$K1,$Ug,$M1,$N1},$i1);$Wg={$Yf,$dg,$eg,$hg,$ig,$lg,$mg,$pg,$qg,$tg,$ug,$xg,$yg,$Bg,$Cg,$Fg,$Gg,$Jg,$Kg,$Ng,$Og,$Rg,$Sg,$Vg};$Xg=q'/lib/ni_static_util.b';$Yg=bless({$c,$Xf,$z1,$A1,$B1,$A1,$C1,$Wg,$f,$Xg},$S);$Zg=q'/lib/slice::ctors';$ch=q'ni:/lib/perlbranch.b';$dh=q'ni:/lib/ref_eq.b';$eh=q'ni:/lib/resolver.b';$fh=q'ni:/lib/slice';$gh=q'ni:/lib/slice.b';$hh=q'ni:/lib/slice.c';$ih=q'ni:/lib/slice_init.b';$jh=q'ni:/lib/slice_serialize.b';$kh=q'ni:/lib/static_fn.b';$lh={};$mh=[];$nh=q'ni(\'ni:/lib/fn\')->new(@_)';$oh=bless({$d2,$mh,$K1,$nh,$M1,$ed},$i1);$ph=q'fp';$qh=[];$rh=q'ni(\'ni:/lib/fn\')->new(@_)';$sh=q'($$)';$th=bless({$d2,$qh,$K1,$rh,$M1,$sh},$i1);$uh={$k2,$oh,$ph,$th};$vh=q'/lib/static_fn.b';$wh=bless({$c,$lh,$z1,$A1,$B1,$A1,$C1,$uh,$f,$vh},$S);$xh=q'/lib/slice::ctors';$yh=q'ni:/lib/subclass.b';$zh=q'ni:/lib/tag';$Ah=q'ni:/lib/tag.b';$Bh=q'ni:/lib/tag.c';$Ch=q'ni:/lib/tag_init.b';$Dh=q'ni:/lib/test_value';$Eh=q'/lib/test_value.c';$Fh={$Eh,1};$Gh=q'/lib/test_value.c';$Hh=[$H];$Ih=bless({$c,$Fh,$f,$Gh,$g,$Hh},$G);$Jh=q'/metaclass::ctors';$Kh={$l1,1};$Lh={};$Mh=[];$Nh=q'\\$_[1]';$Oh=bless({$d2,$Mh,$K1,$Nh,$M1,$N1},$i1);$Ph={$T1,$Oh};$Qh=q'/lib/test_value_init.b';$Rh=bless({$c,$Lh,$z1,$A1,$B1,$A1,$C1,$Ph,$f,$Qh},$S);$Sh=q'/lib/slice::ctors';$Th={};$Uh=q'(==';$Vh=[];$Wh=q'my ($self, $rhs) = @_;
my $diff = $self->diff($rhs);
die $self->class->new($diff) if defined $diff;
1;';$Xh=bless({$d2,$Vh,$K1,$Wh,$M1,$N1},$i1);$Yh=q'diff';$Zh=[];$ci=q'my ($self, $rhs) = @_;
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
}';$di=bless({$d2,$Zh,$K1,$ci,$M1,$N1},$i1);$ei={$Uh,$Xh,$Yh,$di};$fi=q'/lib/test_value_eq.b';$gi=bless({$c,$Th,$z1,$A1,$B1,$A1,$C1,$ei,$f,$fi},$S);$hi=q'/lib/slice::ctors';$ii={};$ji=[];$ki=q'ni::json_encode ${$_[0]}';$li=bless({$d2,$ji,$K1,$ki,$M1,$N1},$i1);$mi={$t2,$li};$ni=q'/lib/test_value_str.b';$oi=bless({$c,$ii,$z1,$A1,$B1,$A1,$C1,$mi,$f,$ni},$S);$pi=q'/lib/slice::ctors';$qi=[$d3,$Rh,$gi,$oi];$ri=q'/lib/test_value.c';$si=bless({$c,$Kh,$f,$l1,$g,$qi},$ri);$ti=q'/lib/test_value.c::ctors';$ui=q'ni:/lib/test_value.c';$vi=q'ni:/lib/test_value_eq.b';$wi=q'ni:/lib/test_value_init.b';$xi=q'ni:/lib/test_value_str.b';$yi=q'ni:/metaclass';$zi=q'ni:/metaclass.c';$Ai=q'ni:/module';$Bi=q'ni:/module.c';$Ci=q'ni:/object';$Di=q'ni:/object.c';$Ei=q'ni:/unix/cat';$Fi={$w,1};$Gi=q'/unix/pipeline.c';$Hi={$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$Gi,1,$D,1};$Ii=[$H];$Ji=bless({$c,$Hi,$f,$A,$g,$Ii},$G);$Ki=q'/metaclass::ctors';$Li=[$Ji];$Mi=bless({$c,$Fi,$f,$w,$g,$Li},$G);$Ni=q'/metaclass::ctors';$Oi={$o1,1};$Pi={$o1,1,$p1,1,$q1,1,$r1,1,$s1,1,$t1,1,$u1,1,$w1,1};$Qi={};$Ri=q'into';$Si=[];$Ti=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Ui=bless({$d2,$Si,$K1,$Ti,$M1,$N1},$i1);$Vi={$Ri,$Ui};$Wi=q'/unix/io_stream.b';$Xi=bless({$c,$Qi,$z1,$A1,$B1,$A1,$C1,$Vi,$f,$Wi},$S);$Yi=q'/lib/slice::ctors';$Zi={};$cj=q'(+';$dj=[];$ej=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$fj=bless({$d2,$dj,$K1,$ej,$M1,$N1},$i1);$gj={$cj,$fj};$hj=q'/unix/io_constructors.b';$ij=bless({$c,$Zi,$z1,$A1,$B1,$A1,$C1,$gj,$f,$hj},$S);$jj=q'/lib/slice::ctors';$kj={};$lj=q'(<>';$mj=[];$nj=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$oj=bless({$d2,$mj,$K1,$nj,$M1,$N1},$i1);$pj=q'(@{}';$qj=[];$rj=q'my $self = shift; [<$self>]';$sj=bless({$d2,$qj,$K1,$rj,$M1,$N1},$i1);$tj={$lj,$oj,$pj,$sj};$uj=q'/unix/io_readers.b';$vj=bless({$c,$kj,$z1,$A1,$B1,$A1,$C1,$tj,$f,$uj},$S);$wj=q'/lib/slice::ctors';$xj=[$d3,$Xi,$ij,$vj];$yj=bless({$c,$Pi,$f,$s1,$g,$xj},$A);$zj=q'/unix/io.c::ctors';$Aj={};$Bj=[];$Cj=q'shift; +{fs => [@_]}';$Dj=bless({$d2,$Bj,$K1,$Cj,$M1,$N1},$i1);$Ej={$T1,$Dj};$Fj=q'/unix/cat_init.b';$Gj=bless({$c,$Aj,$z1,$A1,$B1,$A1,$C1,$Ej,$f,$Fj},$S);$Hj=q'/lib/slice::ctors';$Ij={};$Jj=q'read';$Kj=[];$Lj=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Mj=bless({$d2,$Kj,$K1,$Lj,$M1,$N1},$i1);$Nj={$Jj,$Mj};$Oj=q'/unix/cat_read.b';$Pj=bless({$c,$Ij,$z1,$A1,$B1,$A1,$C1,$Nj,$f,$Oj},$S);$Qj=q'/lib/slice::ctors';$Rj=[$yj,$Gj,$Pj];$Sj=bless({$c,$Oi,$f,$o1,$g,$Rj},$w);$Tj=q'/unix/cat.c::ctors';$Uj=q'ni:/unix/cat.c';$Vj=q'ni:/unix/cat_init.b';$Wj=q'ni:/unix/cat_read.b';$Xj=q'ni:/unix/fd';$Yj={$x,1};$Zj=[$Ji];$ck=bless({$c,$Yj,$f,$x,$g,$Zj},$G);$dk=q'/metaclass::ctors';$ek={$p1,1};$fk={};$gk=q'fd';$hk=[];$ik=q'shift->{\'fd\'}';$jk=bless({$d2,$hk,$K1,$ik,$M1,$N1},$i1);$kk={$gk,$jk};$lk=q'/unix/fd_readers.b';$mk=bless({$c,$fk,$z1,$A1,$B1,$A1,$C1,$kk,$f,$lk},$S);$nk=q'/lib/slice::ctors';$ok={};$pk=[];$qk=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$rk=bless({$d2,$pk,$K1,$qk,$M1,$N1},$i1);$sk={$T1,$rk};$tk=q'/unix/fd_init.b';$uk=bless({$c,$ok,$z1,$A1,$B1,$A1,$C1,$sk,$f,$tk},$S);$vk=q'/lib/slice::ctors';$wk={};$xk=q'move_to';$yk=[];$zk=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Ak=bless({$d2,$yk,$K1,$zk,$M1,$N1},$i1);$Bk={$xk,$Ak};$Ck=q'/unix/fd_shell.b';$Dk=bless({$c,$wk,$z1,$A1,$B1,$A1,$C1,$Bk,$f,$Ck},$S);$Ek=q'/lib/slice::ctors';$Fk={$p1,1,$q1,1,$r1,1,$t1,1,$u1,1};$Gk=q'/unix/has_fd.b';$Hk={};$Ik=[];$Jk=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Kk=bless({$d2,$Ik,$K1,$Jk,$M1,$N1},$i1);$Lk=[];$Mk=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Nk=bless({$d2,$Lk,$K1,$Mk,$M1,$N1},$i1);$Ok={$Jj,$Kk,$Se,$Nk};$Pk=q'/unix/fd_safeio.b';$Qk=bless({$c,$Hk,$z1,$A1,$B1,$A1,$C1,$Ok,$f,$Pk},$S);$Rk=q'/lib/slice::ctors';$Sk=[$Qk];$Tk=bless({$c,$Fk,$f,$Gk,$g,$Sk},$W);$Uk=q'/lib/branch::ctors';$Vk={};$Wk=q'read_fh';$Xk=[];$Yk=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$Zk=bless({$d2,$Xk,$K1,$Yk,$M1,$N1},$i1);$cl=q'write_fh';$dl=[];$el=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$fl=bless({$d2,$dl,$K1,$el,$M1,$N1},$i1);$gl={$Wk,$Zk,$cl,$fl};$hl=q'/unix/fd_io.b';$il=bless({$c,$Vk,$z1,$A1,$B1,$A1,$C1,$gl,$f,$hl},$S);$jl=q'/lib/slice::ctors';$kl=[$yj,$mk,$uk,$Dk,$Tk,$il];$ll=bless({$c,$ek,$f,$p1,$g,$kl},$x);$ml=q'/unix/fd.c::ctors';$nl=q'ni:/unix/fd.c';$ol=q'ni:/unix/fd_init.b';$pl=q'ni:/unix/fd_io.b';$ql=q'ni:/unix/fd_readers.b';$rl=q'ni:/unix/fd_safeio.b';$sl=q'ni:/unix/fd_shell.b';$tl=q'ni:/unix/fifo';$ul={$y,1};$vl=[$Ji];$wl=bless({$c,$ul,$f,$y,$g,$vl},$G);$xl=q'/metaclass::ctors';$yl={$q1,1};$zl={};$Al=[];$Bl=q'shift->{\'read_fh\'}';$Cl=bless({$d2,$Al,$K1,$Bl,$M1,$N1},$i1);$Dl=[];$El=q'shift->{\'write_fh\'}';$Fl=bless({$d2,$Dl,$K1,$El,$M1,$N1},$i1);$Gl={$Wk,$Cl,$cl,$Fl};$Hl=q'/unix/fifo_io.b';$Il=bless({$c,$zl,$z1,$A1,$B1,$A1,$C1,$Gl,$f,$Hl},$S);$Jl=q'/lib/slice::ctors';$Kl={};$Ll=[];$Ml=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Nl=bless({$d2,$Ll,$K1,$Ml,$M1,$N1},$i1);$Ol={$T1,$Nl};$Pl=q'/unix/fifo_init.b';$Ql=bless({$c,$Kl,$z1,$A1,$B1,$A1,$C1,$Ol,$f,$Pl},$S);$Rl=q'/lib/slice::ctors';$Sl={};$Tl=q'read_side';$Ul=[];$Vl=q'my $self = shift; close $$self{write_fh}; $self';$Wl=bless({$d2,$Ul,$K1,$Vl,$M1,$N1},$i1);$Xl=q'write_side';$Yl=[];$Zl=q'my $self = shift; close $$self{read_fh};  $self';$cm=bless({$d2,$Yl,$K1,$Zl,$M1,$N1},$i1);$dm={$Tl,$Wl,$Xl,$cm};$em=q'/unix/fifo_direction.b';$fm=bless({$c,$Sl,$z1,$A1,$B1,$A1,$C1,$dm,$f,$em},$S);$gm=q'/lib/slice::ctors';$hm=[$yj,$Il,$Ql,$Tk,$fm];$im=bless({$c,$yl,$f,$q1,$g,$hm},$y);$jm=q'/unix/fifo.c::ctors';$km=q'ni:/unix/fifo.c';$lm=q'ni:/unix/fifo_direction.b';$mm=q'ni:/unix/fifo_init.b';$nm=q'ni:/unix/fifo_io.b';$om=q'ni:/unix/file';$pm={$z,1};$qm=[$Ji];$rm=bless({$c,$pm,$f,$z,$g,$qm},$G);$sm=q'/metaclass::ctors';$tm={$r1,1};$um={};$vm=[];$wm=q'shift->{\'name\'}';$xm=bless({$d2,$vm,$K1,$wm,$M1,$N1},$i1);$ym={$f,$xm};$zm=q'/unix/file_readers.b';$Am=bless({$c,$um,$z1,$A1,$B1,$A1,$C1,$ym,$f,$zm},$S);$Bm=q'/lib/slice::ctors';$Cm={};$Dm=[];$Em=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Fm=bless({$d2,$Dm,$K1,$Em,$M1,$N1},$i1);$Gm={$T1,$Fm};$Hm=q'/unix/file_init.b';$Im=bless({$c,$Cm,$z1,$A1,$B1,$A1,$C1,$Gm,$f,$Hm},$S);$Jm=q'/lib/slice::ctors';$Km={};$Lm=[];$Mm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Nm=bless({$d2,$Lm,$K1,$Mm,$M1,$N1},$i1);$Om=[];$Pm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Qm=bless({$d2,$Om,$K1,$Pm,$M1,$N1},$i1);$Rm={$Wk,$Nm,$cl,$Qm};$Sm=q'/unix/file_io.b';$Tm=bless({$c,$Km,$z1,$A1,$B1,$A1,$C1,$Rm,$f,$Sm},$S);$Um=q'/lib/slice::ctors';$Vm=[$yj,$Am,$Im,$Tk,$Tm];$Wm=bless({$c,$tm,$f,$r1,$g,$Vm},$z);$Xm=q'/unix/file.c::ctors';$Ym=q'ni:/unix/file.c';$Zm=q'ni:/unix/file_init.b';$cn=q'ni:/unix/file_io.b';$dn=q'ni:/unix/file_readers.b';$en=q'ni:/unix/has_fd.b';$fn=q'ni:/unix/io';$gn=q'ni:/unix/io.c';$hn=q'ni:/unix/io_constructors.b';$in=q'ni:/unix/io_readers.b';$jn=q'ni:/unix/io_stream.b';$kn=q'ni:/unix/pid';$ln={$B,1};$mn=[$Ji];$nn=bless({$c,$ln,$f,$B,$g,$mn},$G);$on=q'/metaclass::ctors';$pn={$t1,1};$qn={};$rn=q'pid';$sn=[];$tn=q'shift->{\'pid\'}';$un=bless({$d2,$sn,$K1,$tn,$M1,$N1},$i1);$vn=q'stderr';$wn=[];$xn=q'shift->{\'stderr\'}';$yn=bless({$d2,$wn,$K1,$xn,$M1,$N1},$i1);$zn=q'stdin';$An=[];$Bn=q'shift->{\'stdin\'}';$Cn=bless({$d2,$An,$K1,$Bn,$M1,$N1},$i1);$Dn=q'stdout';$En=[];$Fn=q'shift->{\'stdout\'}';$Gn=bless({$d2,$En,$K1,$Fn,$M1,$N1},$i1);$Hn={$rn,$un,$vn,$yn,$zn,$Cn,$Dn,$Gn};$In=q'/unix/pid_readers.b';$Jn=bless({$c,$qn,$z1,$A1,$B1,$A1,$C1,$Hn,$f,$In},$S);$Kn=q'/lib/slice::ctors';$Ln={};$Mn=[];$Nn=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$On=bless({$d2,$Mn,$K1,$Nn,$M1,$N1},$i1);$Pn={$T1,$On};$Qn=q'/unix/pid_init.b';$Rn=bless({$c,$Ln,$z1,$A1,$B1,$A1,$C1,$Pn,$f,$Qn},$S);$Sn=q'/lib/slice::ctors';$Tn={};$Un={};$Vn=q'/unix/pid_wait.b';$Wn=bless({$c,$Tn,$z1,$A1,$B1,$A1,$C1,$Un,$f,$Vn},$S);$Xn=q'/lib/slice::ctors';$Yn={};$Zn=[];$co=q'shift->{stdout}->read_fh';$do=bless({$d2,$Zn,$K1,$co,$M1,$N1},$i1);$eo=[];$fo=q'shift->{stdin}->write_fh';$go=bless({$d2,$eo,$K1,$fo,$M1,$N1},$i1);$ho={$Wk,$do,$cl,$go};$io=q'/unix/pid_io.b';$jo=bless({$c,$Yn,$z1,$A1,$B1,$A1,$C1,$ho,$f,$io},$S);$ko=q'/lib/slice::ctors';$lo=[$yj,$Jn,$Rn,$Wn,$Tk,$jo];$mo=bless({$c,$pn,$f,$t1,$g,$lo},$B);$no=q'/unix/pid.c::ctors';$oo=q'ni:/unix/pid.c';$po=q'ni:/unix/pid_init.b';$qo=q'ni:/unix/pid_io.b';$ro=q'ni:/unix/pid_readers.b';$so=q'ni:/unix/pid_wait.b';$to=q'ni:/unix/pipeline';$uo=q'/unix/pipeline.c';$vo={$uo,1};$wo=q'/unix/pipeline.c';$xo=[$Ji];$yo=bless({$c,$vo,$f,$wo,$g,$xo},$G);$zo=q'/metaclass::ctors';$Ao={$u1,1};$Bo={};$Co=[];$Do=q'shift->{\'stdin\'}';$Eo=bless({$d2,$Co,$K1,$Do,$M1,$N1},$i1);$Fo=[];$Go=q'shift->{\'stdout\'}';$Ho=bless({$d2,$Fo,$K1,$Go,$M1,$N1},$i1);$Io={$zn,$Eo,$Dn,$Ho};$Jo=q'/unix/pipeline_ro.b';$Ko=bless({$c,$Bo,$z1,$A1,$B1,$A1,$C1,$Io,$f,$Jo},$S);$Lo=q'/lib/slice::ctors';$Mo={};$No=[];$Oo=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Po=bless({$d2,$No,$K1,$Oo,$M1,$N1},$i1);$Qo={$T1,$Po};$Ro=q'/unix/pipeline_init.b';$So=bless({$c,$Mo,$z1,$A1,$B1,$A1,$C1,$Qo,$f,$Ro},$S);$To=q'/lib/slice::ctors';$Uo={};$Vo=q'async_step';$Wo=[];$Xo=q'local $_;
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
$self;';$Yo=bless({$d2,$Wo,$K1,$Xo,$M1,$N1},$i1);$Zo={$Vo,$Yo};$cp=q'/unix/pipeline_async.b';$dp=bless({$c,$Uo,$z1,$A1,$B1,$A1,$C1,$Zo,$f,$cp},$S);$ep=q'/lib/slice::ctors';$fp={};$gp=[];$hp=q'shift->{stdout}->read_fh';$ip=bless({$d2,$gp,$K1,$hp,$M1,$N1},$i1);$jp=[];$kp=q'shift->{stdin}->write_fh';$lp=bless({$d2,$jp,$K1,$kp,$M1,$N1},$i1);$mp={$Wk,$ip,$cl,$lp};$np=q'/unix/pipeline_io.b';$op=bless({$c,$fp,$z1,$A1,$B1,$A1,$C1,$mp,$f,$np},$S);$pp=q'/lib/slice::ctors';$qp=[$yj,$Ko,$So,$dp,$Tk,$op];$rp=q'/unix/pipeline.c';$sp=bless({$c,$Ao,$f,$u1,$g,$qp},$rp);$tp=q'/unix/pipeline.c::ctors';$up=q'ni:/unix/pipeline.c';$vp=q'ni:/unix/pipeline_async.b';$wp=q'ni:/unix/pipeline_init.b';$xp=q'ni:/unix/pipeline_io.b';$yp=q'ni:/unix/pipeline_ro.b';$zp=q'ni:/unix/str';$Ap={$D,1};$Bp=[$Ji];$Cp=bless({$c,$Ap,$f,$D,$g,$Bp},$G);$Dp=q'/metaclass::ctors';$Ep={$w1,1};$Fp={};$Gp=q'data';$Hp=[];$Ip=q'shift->{\'data\'}';$Jp=bless({$d2,$Hp,$K1,$Ip,$M1,$N1},$i1);$Kp=q'end';$Lp=[];$Mp=q'shift->{\'end\'}';$Np=bless({$d2,$Lp,$K1,$Mp,$M1,$N1},$i1);$Op=q'start';$Pp=[];$Qp=q'shift->{\'start\'}';$Rp=bless({$d2,$Pp,$K1,$Qp,$M1,$N1},$i1);$Sp={$Gp,$Jp,$Kp,$Np,$Op,$Rp};$Tp=q'/unix/str_ro.b';$Up=bless({$c,$Fp,$z1,$A1,$B1,$A1,$C1,$Sp,$f,$Tp},$S);$Vp=q'/lib/slice::ctors';$Wp={};$Xp=[];$Yp=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Zp=bless({$d2,$Xp,$K1,$Yp,$M1,$N1},$i1);$cq={$T1,$Zp};$dq=q'/unix/str_init.b';$eq=bless({$c,$Wp,$z1,$A1,$B1,$A1,$C1,$cq,$f,$dq},$S);$fq=q'/lib/slice::ctors';$gq={};$hq=[];$iq=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = @_ >= 2 ? ni::min($self->remaining, $_[1]) : $self->remaining;
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$jq=bless({$d2,$hq,$K1,$iq,$M1,$N1},$i1);$kq=q'remaining';$lq=[];$mq=q'my $self = shift; $$self{end} - $$self{start}';$nq=bless({$d2,$lq,$K1,$mq,$M1,$N1},$i1);$oq=[];$pq=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$qq=bless({$d2,$oq,$K1,$pq,$M1,$N1},$i1);$rq={$Jj,$jq,$kq,$nq,$Se,$qq};$sq=q'/unix/str_io.b';$tq=bless({$c,$gq,$z1,$A1,$B1,$A1,$C1,$rq,$f,$sq},$S);$uq=q'/lib/slice::ctors';$vq=[$yj,$Up,$eq,$tq];$wq=bless({$c,$Ep,$f,$w1,$g,$vq},$D);$xq=q'/unix/str.c::ctors';$yq=q'ni:/unix/str.c';$zq=q'ni:/unix/str_init.b';$Aq=q'ni:/unix/str_io.b';$Bq=q'ni:/unix/str_ro.b';$Cq=q'ni:main';$Dq={$jd,1};$Eq=[$wh,$id];$Fq=bless({$c,$Dq,$f,$jd,$g,$Eq},$d1);$Gq=q'/module::ctors';$Hq=q'ni:ni';$Iq={$Jf,1};$Jq={$Jf,1};$Kq=q'json_escapes';$Lq=q'';$Mq=q'b';$Nq=q'	';$Oq=q't';$Pq=q'
';$Qq=q'n';$Rq=q'';$Sq=q'r';$Tq=q'"';$Uq=q'/';$Vq=q'\\';$Wq={$Lq,$Mq,$Nq,$Oq,$Pq,$Qq,$Rq,$Sq,$Tq,$Tq,$Uq,$Uq,$Vq,$Vq};$Xq=q'json_unescapes';$Yq={$Tq,$Tq,$Uq,$Uq,$Vq,$Vq,$Mq,$Lq,$Qq,$Pq,$Sq,$Rq,$Oq,$Nq};$Zq={$Kq,$Wq,$Xq,$Yq};$cr=q'/lib/json_data.b';$dr=bless({$c,$Jq,$Gp,$Zq,$f,$cr},$X);$er=q'/lib/dataslice::ctors';$fr=[$dr,$If,$Yg];$gr=bless({$c,$Iq,$f,$Jf,$g,$fr},$d1);$hr={$ma,$sb,$ub,$Gb,$Hb,$Rb,$Sb,$s8,$Tb,$E4,$Ub,$o7,$Vb,$r3,$Wb,$N,$Xb,$L6,$Yb,$v5,$Zb,$A6,$cc,$I6,$dc,$t6,$ec,$Cc,$Ec,$jc,$Fc,$yc,$Gc,$rc,$Hc,$Z7,$Ic,$Y6,$Jc,$T7,$Kc,$cb,$Lc,$pa,$Mc,$Ma,$Nc,$xa,$Oc,$Ea,$Pc,$Xa,$Qc,$l3,$Rc,$P2,$Sc,$G1,$Tc,$Y1,$Uc,$D2,$Vc,$q2,$Wc,$M2,$Xc,$id,$ld,$df,$ff,$od,$gf,$xd,$hf,$Ye,$if,$V2,$jf,$v4,$kf,$If,$Lf,$y7,$Mf,$A3,$Nf,$G5,$Of,$R5,$Pf,$ja,$Qf,$D8,$Rf,$g9,$Sf,$F9,$Tf,$ga,$Uf,$R9,$Vf,$R8,$Wf,$Yg,$ch,$k6,$dh,$I7,$eh,$e6,$fh,$k4,$gh,$Q3,$hh,$Q,$ih,$X3,$jh,$h4,$kh,$wh,$yh,$m8,$zh,$k5,$Ah,$Y4,$Bh,$Q4,$Ch,$h5,$Dh,$si,$ui,$Ih,$vi,$gi,$wi,$Rh,$xi,$oi,$yi,$z8,$zi,$v8,$Ai,$e8,$Bi,$B4,$Ci,$d3,$Di,$H,$Ei,$Sj,$Uj,$Mi,$Vj,$Gj,$Wj,$Pj,$Xj,$ll,$nl,$ck,$ol,$uk,$pl,$il,$ql,$mk,$rl,$Qk,$sl,$Dk,$tl,$im,$km,$wl,$lm,$fm,$mm,$Ql,$nm,$Il,$om,$Wm,$Ym,$rm,$Zm,$Im,$cn,$Tm,$dn,$Am,$en,$Tk,$fn,$yj,$gn,$Ji,$hn,$ij,$in,$vj,$jn,$Xi,$kn,$mo,$oo,$nn,$po,$Rn,$qo,$jo,$ro,$Jn,$so,$Wn,$to,$sp,$up,$yo,$vp,$dp,$wp,$So,$xp,$op,$yp,$Ko,$zp,$wq,$yq,$Cp,$zq,$eq,$Aq,$tq,$Bq,$Up,$Cq,$Fq,$Hq,$gr};$ir=q'resolvers';$jr=[];$kr=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$lr=bless({$d2,$jr,$K1,$kr,$M1,$N1},$i1);$mr=q'file';$nr=[];$or=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$pr=bless({$d2,$nr,$K1,$or,$M1,$N1},$i1);$qr=q'str';$rr=[];$sr=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$tr=bless({$d2,$rr,$K1,$sr,$M1,$N1},$i1);$ur={$gk,$lr,$mr,$pr,$qr,$tr};$vr=bless({$la,$hr,$ir,$ur},$k1);$wr=q'/lib/ni::ctors';$$F[0]=$s8;$$O2[0]=$d3;$$O2[1]=$v4;$$K6[4]=$Z7;*$K3=\&$I3;*$J3=\&$G3;$Y1->apply_unsafe($i1);$q2->apply_unsafe($i1);$D2->apply_unsafe($i1);$M2->apply_unsafe($i1);$V2->apply_unsafe($U);$V2->apply_unsafe($h);$V2->apply_unsafe($V);$V2->apply_unsafe($l);$V2->apply_unsafe($W);$V2->apply_unsafe($m);$V2->apply_unsafe($X);$V2->apply_unsafe($W2);$V2->apply_unsafe($h1);$V2->apply_unsafe($o);$V2->apply_unsafe($i1);$V2->apply_unsafe($p);$V2->apply_unsafe($j1);$V2->apply_unsafe($q);$V2->apply_unsafe($k1);$V2->apply_unsafe($r);$V2->apply_unsafe($S);$V2->apply_unsafe($s);$V2->apply_unsafe($Z);$V2->apply_unsafe($t);$V2->apply_unsafe($l1);$V2->apply_unsafe($X2);$V2->apply_unsafe($G);$V2->apply_unsafe($d);$V2->apply_unsafe($d1);$V2->apply_unsafe($j);$V2->apply_unsafe($n1);$V2->apply_unsafe($v);$V2->apply_unsafe($o1);$V2->apply_unsafe($w);$V2->apply_unsafe($p1);$V2->apply_unsafe($x);$V2->apply_unsafe($q1);$V2->apply_unsafe($y);$V2->apply_unsafe($r1);$V2->apply_unsafe($z);$V2->apply_unsafe($s1);$V2->apply_unsafe($A);$V2->apply_unsafe($t1);$V2->apply_unsafe($B);$V2->apply_unsafe($u1);$V2->apply_unsafe($Y2);$V2->apply_unsafe($w1);$V2->apply_unsafe($D);$l3->apply_unsafe($U);$l3->apply_unsafe($h);$l3->apply_unsafe($V);$l3->apply_unsafe($l);$l3->apply_unsafe($W);$l3->apply_unsafe($m);$l3->apply_unsafe($X);$l3->apply_unsafe($m3);$l3->apply_unsafe($o);$l3->apply_unsafe($p);$l3->apply_unsafe($q);$l3->apply_unsafe($r);$l3->apply_unsafe($S);$l3->apply_unsafe($s);$l3->apply_unsafe($Z);$l3->apply_unsafe($t);$l3->apply_unsafe($n3);$l3->apply_unsafe($G);$l3->apply_unsafe($d);$l3->apply_unsafe($d1);$l3->apply_unsafe($j);$l3->apply_unsafe($v);$l3->apply_unsafe($w);$l3->apply_unsafe($x);$l3->apply_unsafe($y);$l3->apply_unsafe($z);$l3->apply_unsafe($A);$l3->apply_unsafe($B);$l3->apply_unsafe($o3);$l3->apply_unsafe($D);$A3->apply_unsafe($U);$A3->apply_unsafe($h);$A3->apply_unsafe($l);$A3->apply_unsafe($W);$A3->apply_unsafe($m);$A3->apply_unsafe($B3);$A3->apply_unsafe($h1);$A3->apply_unsafe($o);$A3->apply_unsafe($p);$A3->apply_unsafe($q);$A3->apply_unsafe($r);$A3->apply_unsafe($S);$A3->apply_unsafe($s);$A3->apply_unsafe($Z);$A3->apply_unsafe($t);$A3->apply_unsafe($C3);$A3->apply_unsafe($G);$A3->apply_unsafe($d);$A3->apply_unsafe($d1);$A3->apply_unsafe($j);$A3->apply_unsafe($v);$A3->apply_unsafe($w);$A3->apply_unsafe($x);$A3->apply_unsafe($y);$A3->apply_unsafe($z);$A3->apply_unsafe($A);$A3->apply_unsafe($B);$A3->apply_unsafe($D3);$A3->apply_unsafe($D);$Q3->apply_unsafe($S);$X3->apply_unsafe($S);$h4->apply_unsafe($S);$v4->apply_unsafe($U);$v4->apply_unsafe($h);$v4->apply_unsafe($l);$v4->apply_unsafe($m);$v4->apply_unsafe($w4);$v4->apply_unsafe($o);$v4->apply_unsafe($i1);$v4->apply_unsafe($p);$v4->apply_unsafe($q);$v4->apply_unsafe($r);$v4->apply_unsafe($S);$v4->apply_unsafe($s);$v4->apply_unsafe($Z);$v4->apply_unsafe($t);$v4->apply_unsafe($x4);$v4->apply_unsafe($G);$v4->apply_unsafe($d);$v4->apply_unsafe($j);$v4->apply_unsafe($v);$v4->apply_unsafe($w);$v4->apply_unsafe($x);$v4->apply_unsafe($y);$v4->apply_unsafe($z);$v4->apply_unsafe($A);$v4->apply_unsafe($B);$v4->apply_unsafe($y4);$v4->apply_unsafe($D);$Y4->apply_unsafe($Z);$h5->apply_unsafe($Z);$v5->apply_unsafe($U);$v5->apply_unsafe($h);$v5->apply_unsafe($l);$v5->apply_unsafe($W);$v5->apply_unsafe($m);$v5->apply_unsafe($w5);$v5->apply_unsafe($o);$v5->apply_unsafe($p);$v5->apply_unsafe($q);$v5->apply_unsafe($r);$v5->apply_unsafe($s);$v5->apply_unsafe($t);$v5->apply_unsafe($x5);$v5->apply_unsafe($G);$v5->apply_unsafe($d);$v5->apply_unsafe($d1);$v5->apply_unsafe($j);$v5->apply_unsafe($v);$v5->apply_unsafe($w);$v5->apply_unsafe($x);$v5->apply_unsafe($y);$v5->apply_unsafe($z);$v5->apply_unsafe($A);$v5->apply_unsafe($B);$v5->apply_unsafe($y5);$v5->apply_unsafe($D);$G5->apply_unsafe($U);$G5->apply_unsafe($h);$G5->apply_unsafe($l);$G5->apply_unsafe($W);$G5->apply_unsafe($m);$G5->apply_unsafe($H5);$G5->apply_unsafe($o);$G5->apply_unsafe($p);$G5->apply_unsafe($q);$G5->apply_unsafe($r);$G5->apply_unsafe($S);$G5->apply_unsafe($s);$G5->apply_unsafe($Z);$G5->apply_unsafe($t);$G5->apply_unsafe($I5);$G5->apply_unsafe($G);$G5->apply_unsafe($d);$G5->apply_unsafe($d1);$G5->apply_unsafe($j);$G5->apply_unsafe($v);$G5->apply_unsafe($w);$G5->apply_unsafe($x);$G5->apply_unsafe($y);$G5->apply_unsafe($z);$G5->apply_unsafe($A);$G5->apply_unsafe($B);$G5->apply_unsafe($J5);$G5->apply_unsafe($D);$R5->apply_unsafe($U);$R5->apply_unsafe($h);$R5->apply_unsafe($l);$R5->apply_unsafe($W);$R5->apply_unsafe($m);$R5->apply_unsafe($S5);$R5->apply_unsafe($o);$R5->apply_unsafe($p);$R5->apply_unsafe($q);$R5->apply_unsafe($r);$R5->apply_unsafe($S);$R5->apply_unsafe($s);$R5->apply_unsafe($Z);$R5->apply_unsafe($t);$R5->apply_unsafe($T5);$R5->apply_unsafe($G);$R5->apply_unsafe($d);$R5->apply_unsafe($d1);$R5->apply_unsafe($j);$R5->apply_unsafe($v);$R5->apply_unsafe($w);$R5->apply_unsafe($x);$R5->apply_unsafe($y);$R5->apply_unsafe($z);$R5->apply_unsafe($A);$R5->apply_unsafe($B);$R5->apply_unsafe($U5);$R5->apply_unsafe($D);$e6->apply_unsafe($U);$e6->apply_unsafe($h);$e6->apply_unsafe($l);$e6->apply_unsafe($W);$e6->apply_unsafe($m);$e6->apply_unsafe($f6);$e6->apply_unsafe($o);$e6->apply_unsafe($p);$e6->apply_unsafe($q);$e6->apply_unsafe($r);$e6->apply_unsafe($s);$e6->apply_unsafe($Z);$e6->apply_unsafe($t);$e6->apply_unsafe($g6);$e6->apply_unsafe($G);$e6->apply_unsafe($d);$e6->apply_unsafe($d1);$e6->apply_unsafe($j);$e6->apply_unsafe($v);$e6->apply_unsafe($w);$e6->apply_unsafe($x);$e6->apply_unsafe($y);$e6->apply_unsafe($z);$e6->apply_unsafe($A);$e6->apply_unsafe($B);$e6->apply_unsafe($h6);$e6->apply_unsafe($D);$t6->apply_unsafe($U);$t6->apply_unsafe($h);$t6->apply_unsafe($l);$t6->apply_unsafe($m);$t6->apply_unsafe($u6);$t6->apply_unsafe($o);$t6->apply_unsafe($p);$t6->apply_unsafe($q);$t6->apply_unsafe($r);$t6->apply_unsafe($s);$t6->apply_unsafe($t);$t6->apply_unsafe($v6);$t6->apply_unsafe($G);$t6->apply_unsafe($d);$t6->apply_unsafe($d1);$t6->apply_unsafe($j);$t6->apply_unsafe($v);$t6->apply_unsafe($w);$t6->apply_unsafe($x);$t6->apply_unsafe($y);$t6->apply_unsafe($z);$t6->apply_unsafe($A);$t6->apply_unsafe($B);$t6->apply_unsafe($w6);$t6->apply_unsafe($D);$I6->apply_unsafe($W);$Y6->apply_unsafe($U);$Y6->apply_unsafe($h);$Y6->apply_unsafe($l);$Y6->apply_unsafe($W);$Y6->apply_unsafe($m);$Y6->apply_unsafe($Z6);$Y6->apply_unsafe($o);$Y6->apply_unsafe($p);$Y6->apply_unsafe($q);$Y6->apply_unsafe($r);$Y6->apply_unsafe($s);$Y6->apply_unsafe($t);$Y6->apply_unsafe($c7);$Y6->apply_unsafe($G);$Y6->apply_unsafe($d);$Y6->apply_unsafe($d1);$Y6->apply_unsafe($j);$Y6->apply_unsafe($v);$Y6->apply_unsafe($w);$Y6->apply_unsafe($x);$Y6->apply_unsafe($y);$Y6->apply_unsafe($z);$Y6->apply_unsafe($A);$Y6->apply_unsafe($B);$Y6->apply_unsafe($d7);$Y6->apply_unsafe($D);$o7->apply_unsafe($U);$o7->apply_unsafe($h);$o7->apply_unsafe($l);$o7->apply_unsafe($W);$o7->apply_unsafe($m);$o7->apply_unsafe($p7);$o7->apply_unsafe($o);$o7->apply_unsafe($p);$o7->apply_unsafe($q);$o7->apply_unsafe($r);$o7->apply_unsafe($s);$o7->apply_unsafe($t);$o7->apply_unsafe($q7);$o7->apply_unsafe($G);$o7->apply_unsafe($d);$o7->apply_unsafe($d1);$o7->apply_unsafe($j);$o7->apply_unsafe($v);$o7->apply_unsafe($w);$o7->apply_unsafe($x);$o7->apply_unsafe($y);$o7->apply_unsafe($z);$o7->apply_unsafe($A);$o7->apply_unsafe($B);$o7->apply_unsafe($r7);$o7->apply_unsafe($D);$y7->apply_unsafe($U);$y7->apply_unsafe($h);$y7->apply_unsafe($l);$y7->apply_unsafe($W);$y7->apply_unsafe($m);$y7->apply_unsafe($z7);$y7->apply_unsafe($o);$y7->apply_unsafe($p);$y7->apply_unsafe($q);$y7->apply_unsafe($r);$y7->apply_unsafe($s);$y7->apply_unsafe($t);$y7->apply_unsafe($A7);$y7->apply_unsafe($G);$y7->apply_unsafe($d);$y7->apply_unsafe($d1);$y7->apply_unsafe($j);$y7->apply_unsafe($v);$y7->apply_unsafe($w);$y7->apply_unsafe($x);$y7->apply_unsafe($y);$y7->apply_unsafe($z);$y7->apply_unsafe($A);$y7->apply_unsafe($B);$y7->apply_unsafe($B7);$y7->apply_unsafe($D);$I7->apply_unsafe($U);$I7->apply_unsafe($h);$I7->apply_unsafe($l);$I7->apply_unsafe($W);$I7->apply_unsafe($m);$I7->apply_unsafe($J7);$I7->apply_unsafe($o);$I7->apply_unsafe($p);$I7->apply_unsafe($q);$I7->apply_unsafe($r);$I7->apply_unsafe($s);$I7->apply_unsafe($t);$I7->apply_unsafe($K7);$I7->apply_unsafe($G);$I7->apply_unsafe($d);$I7->apply_unsafe($d1);$I7->apply_unsafe($j);$I7->apply_unsafe($v);$I7->apply_unsafe($w);$I7->apply_unsafe($x);$I7->apply_unsafe($y);$I7->apply_unsafe($z);$I7->apply_unsafe($A);$I7->apply_unsafe($B);$I7->apply_unsafe($L7);$I7->apply_unsafe($D);$T7->apply_unsafe($U);$T7->apply_unsafe($h);$T7->apply_unsafe($l);$T7->apply_unsafe($W);$T7->apply_unsafe($m);$T7->apply_unsafe($U7);$T7->apply_unsafe($o);$T7->apply_unsafe($p);$T7->apply_unsafe($q);$T7->apply_unsafe($r);$T7->apply_unsafe($s);$T7->apply_unsafe($t);$T7->apply_unsafe($V7);$T7->apply_unsafe($G);$T7->apply_unsafe($d);$T7->apply_unsafe($d1);$T7->apply_unsafe($j);$T7->apply_unsafe($v);$T7->apply_unsafe($w);$T7->apply_unsafe($x);$T7->apply_unsafe($y);$T7->apply_unsafe($z);$T7->apply_unsafe($A);$T7->apply_unsafe($B);$T7->apply_unsafe($W7);$T7->apply_unsafe($D);$m8->apply_unsafe($U);$m8->apply_unsafe($h);$m8->apply_unsafe($l);$m8->apply_unsafe($m);$m8->apply_unsafe($n8);$m8->apply_unsafe($o);$m8->apply_unsafe($p);$m8->apply_unsafe($q);$m8->apply_unsafe($r);$m8->apply_unsafe($s);$m8->apply_unsafe($t);$m8->apply_unsafe($o8);$m8->apply_unsafe($d);$m8->apply_unsafe($j);$m8->apply_unsafe($v);$m8->apply_unsafe($w);$m8->apply_unsafe($x);$m8->apply_unsafe($y);$m8->apply_unsafe($z);$m8->apply_unsafe($A);$m8->apply_unsafe($B);$m8->apply_unsafe($p8);$m8->apply_unsafe($D);$R8->apply_unsafe($k1);$g9->apply_unsafe($k1);$F9->apply_unsafe($k1);$R9->apply_unsafe($k1);$ga->apply_unsafe($k1);$xa->apply_unsafe($h1);$Ea->apply_unsafe($h1);$Ma->apply_unsafe($h1);$Xa->apply_unsafe($h1);$rc->apply_unsafe($X);$yc->apply_unsafe($X);$id->apply_unsafe($jd);$xd->apply_unsafe($j1);$Ye->apply_unsafe($j1);$If->apply_unsafe($Jf);$Yg->apply_unsafe($Jf);$wh->apply_unsafe($jd);$Rh->apply_unsafe($l1);$gi->apply_unsafe($l1);$oi->apply_unsafe($l1);$Xi->apply_unsafe($o1);$Xi->apply_unsafe($p1);$Xi->apply_unsafe($q1);$Xi->apply_unsafe($r1);$Xi->apply_unsafe($s1);$Xi->apply_unsafe($t1);$Xi->apply_unsafe($u1);$Xi->apply_unsafe($w1);$ij->apply_unsafe($o1);$ij->apply_unsafe($p1);$ij->apply_unsafe($q1);$ij->apply_unsafe($r1);$ij->apply_unsafe($s1);$ij->apply_unsafe($t1);$ij->apply_unsafe($u1);$ij->apply_unsafe($w1);$vj->apply_unsafe($o1);$vj->apply_unsafe($p1);$vj->apply_unsafe($q1);$vj->apply_unsafe($r1);$vj->apply_unsafe($s1);$vj->apply_unsafe($t1);$vj->apply_unsafe($u1);$vj->apply_unsafe($w1);$Gj->apply_unsafe($o1);$Pj->apply_unsafe($o1);$mk->apply_unsafe($p1);$uk->apply_unsafe($p1);$Dk->apply_unsafe($p1);$Qk->apply_unsafe($p1);$Qk->apply_unsafe($q1);$Qk->apply_unsafe($r1);$Qk->apply_unsafe($t1);$Qk->apply_unsafe($u1);$il->apply_unsafe($p1);$Il->apply_unsafe($q1);$Ql->apply_unsafe($q1);$fm->apply_unsafe($q1);$Am->apply_unsafe($r1);$Im->apply_unsafe($r1);$Tm->apply_unsafe($r1);$Jn->apply_unsafe($t1);$Rn->apply_unsafe($t1);$Wn->apply_unsafe($t1);$jo->apply_unsafe($t1);$Ko->apply_unsafe($u1);$So->apply_unsafe($u1);$dp->apply_unsafe($u1);$op->apply_unsafe($u1);$Up->apply_unsafe($w1);$eq->apply_unsafe($w1);$tq->apply_unsafe($w1);$ni::self=$vr;&$_($H)for@$I;&$_($N)for@$O;&$_($Q)for@$R;&$_($G1)for@$H1;&$_($O1)for@$P1;&$_($S1)for@$P1;&$_($V1)for@$P1;&$_($Y1)for@$Z1;&$_($g2)for@$P1;&$_($j2)for@$P1;&$_($n2)for@$P1;&$_($q2)for@$r2;&$_($w2)for@$P1;&$_($A2)for@$P1;&$_($D2)for@$E2;&$_($J2)for@$P1;&$_($M2)for@$N2;&$_($P2)for@$Q2;&$_($S2)for@$P1;&$_($V2)for@$Z2;&$_($d3)for@$e3;&$_($i3)for@$P1;&$_($l3)for@$p3;&$_($r3)for@$s3;&$_($v3)for@$P1;&$_($x3)for@$P1;&$_($A3)for@$E3;&$_($G3)for@$P1;&$_($I3)for@$P1;&$_($Q3)for@$R3;&$_($U3)for@$P1;&$_($X3)for@$Y3;&$_($e4)for@$P1;&$_($h4)for@$i4;&$_($k4)for@$l4;&$_($p4)for@$P1;&$_($s4)for@$P1;&$_($v4)for@$z4;&$_($B4)for@$C4;&$_($E4)for@$F4;&$_($Q4)for@$R4;&$_($V4)for@$P1;&$_($Y4)for@$Z4;&$_($e5)for@$P1;&$_($h5)for@$i5;&$_($k5)for@$l5;&$_($q5)for@$P1;&$_($s5)for@$P1;&$_($v5)for@$z5;&$_($D5)for@$P1;&$_($G5)for@$K5;&$_($O5)for@$P1;&$_($R5)for@$V5;&$_($Z5)for@$P1;&$_($e6)for@$i6;&$_($k6)for@$l6;&$_($o6)for@$P1;&$_($q6)for@$P1;&$_($t6)for@$x6;&$_($A6)for@$B6;&$_($F6)for@$P1;&$_($I6)for@$J6;&$_($L6)for@$M6;&$_($V6)for@$P1;&$_($Y6)for@$e7;&$_($i7)for@$P1;&$_($l7)for@$P1;&$_($o7)for@$s7;&$_($v7)for@$P1;&$_($y7)for@$C7;&$_($F7)for@$P1;&$_($I7)for@$M7;&$_($Q7)for@$P1;&$_($T7)for@$X7;&$_($Z7)for@$c8;&$_($e8)for@$f8;&$_($j8)for@$P1;&$_($m8)for@$q8;&$_($s8)for@$t8;&$_($v8)for@$w8;&$_($z8)for@$A8;&$_($D8)for@$E8;&$_($K8)for@$P1;&$_($O8)for@$P1;&$_($R8)for@$S8;&$_($X8)for@$P1;&$_($d9)for@$P1;&$_($g9)for@$h9;&$_($m9)for@$P1;&$_($q9)for@$P1;&$_($u9)for@$P1;&$_($y9)for@$P1;&$_($C9)for@$P1;&$_($F9)for@$G9;&$_($K9)for@$P1;&$_($O9)for@$P1;&$_($R9)for@$S9;&$_($X9)for@$P1;&$_($da)for@$P1;&$_($ga)for@$ha;&$_($ja)for@$ka;&$_($pa)for@$qa;&$_($ua)for@$P1;&$_($xa)for@$ya;&$_($Ba)for@$P1;&$_($Ea)for@$Fa;&$_($Ja)for@$P1;&$_($Ma)for@$Na;&$_($Ra)for@$P1;&$_($Ua)for@$P1;&$_($Xa)for@$Ya;&$_($cb)for@$db;&$_($sb)for@$tb;&$_($Ab)for@$P1;&$_($Db)for@$P1;&$_($Gb)for@$tb;&$_($Ob)for@$P1;&$_($Rb)for@$tb;&$_($jc)for@$kc;&$_($oc)for@$P1;&$_($rc)for@$sc;&$_($vc)for@$P1;&$_($yc)for@$zc;&$_($Cc)for@$Dc;&$_($fd)for@$P1;&$_($id)for@$kd;&$_($od)for@$pd;&$_($ud)for@$P1;&$_($xd)for@$yd;&$_($Dd)for@$P1;&$_($Hd)for@$P1;&$_($Ld)for@$P1;&$_($Pd)for@$P1;&$_($Td)for@$P1;&$_($Xd)for@$P1;&$_($de)for@$P1;&$_($he)for@$P1;&$_($le)for@$P1;&$_($pe)for@$P1;&$_($te)for@$P1;&$_($xe)for@$P1;&$_($Be)for@$P1;&$_($Fe)for@$P1;&$_($Je)for@$P1;&$_($Ne)for@$P1;&$_($Re)for@$P1;&$_($Ve)for@$P1;&$_($Ye)for@$Ze;&$_($df)for@$ef;&$_($pf)for@$P1;&$_($tf)for@$P1;&$_($xf)for@$P1;&$_($Bf)for@$P1;&$_($Ff)for@$P1;&$_($If)for@$Kf;&$_($dg)for@$P1;&$_($hg)for@$P1;&$_($lg)for@$P1;&$_($pg)for@$P1;&$_($tg)for@$P1;&$_($xg)for@$P1;&$_($Bg)for@$P1;&$_($Fg)for@$P1;&$_($Jg)for@$P1;&$_($Ng)for@$P1;&$_($Rg)for@$P1;&$_($Vg)for@$P1;&$_($Yg)for@$Zg;&$_($oh)for@$P1;&$_($th)for@$P1;&$_($wh)for@$xh;&$_($Ih)for@$Jh;&$_($Oh)for@$P1;&$_($Rh)for@$Sh;&$_($Xh)for@$P1;&$_($di)for@$P1;&$_($gi)for@$hi;&$_($li)for@$P1;&$_($oi)for@$pi;&$_($si)for@$ti;&$_($Ji)for@$Ki;&$_($Mi)for@$Ni;&$_($Ui)for@$P1;&$_($Xi)for@$Yi;&$_($fj)for@$P1;&$_($ij)for@$jj;&$_($oj)for@$P1;&$_($sj)for@$P1;&$_($vj)for@$wj;&$_($yj)for@$zj;&$_($Dj)for@$P1;&$_($Gj)for@$Hj;&$_($Mj)for@$P1;&$_($Pj)for@$Qj;&$_($Sj)for@$Tj;&$_($ck)for@$dk;&$_($jk)for@$P1;&$_($mk)for@$nk;&$_($rk)for@$P1;&$_($uk)for@$vk;&$_($Ak)for@$P1;&$_($Dk)for@$Ek;&$_($Kk)for@$P1;&$_($Nk)for@$P1;&$_($Qk)for@$Rk;&$_($Tk)for@$Uk;&$_($Zk)for@$P1;&$_($fl)for@$P1;&$_($il)for@$jl;&$_($ll)for@$ml;&$_($wl)for@$xl;&$_($Cl)for@$P1;&$_($Fl)for@$P1;&$_($Il)for@$Jl;&$_($Nl)for@$P1;&$_($Ql)for@$Rl;&$_($Wl)for@$P1;&$_($cm)for@$P1;&$_($fm)for@$gm;&$_($im)for@$jm;&$_($rm)for@$sm;&$_($xm)for@$P1;&$_($Am)for@$Bm;&$_($Fm)for@$P1;&$_($Im)for@$Jm;&$_($Nm)for@$P1;&$_($Qm)for@$P1;&$_($Tm)for@$Um;&$_($Wm)for@$Xm;&$_($nn)for@$on;&$_($un)for@$P1;&$_($yn)for@$P1;&$_($Cn)for@$P1;&$_($Gn)for@$P1;&$_($Jn)for@$Kn;&$_($On)for@$P1;&$_($Rn)for@$Sn;&$_($Wn)for@$Xn;&$_($do)for@$P1;&$_($go)for@$P1;&$_($jo)for@$ko;&$_($mo)for@$no;&$_($yo)for@$zo;&$_($Eo)for@$P1;&$_($Ho)for@$P1;&$_($Ko)for@$Lo;&$_($Po)for@$P1;&$_($So)for@$To;&$_($Yo)for@$P1;&$_($dp)for@$ep;&$_($ip)for@$P1;&$_($lp)for@$P1;&$_($op)for@$pp;&$_($sp)for@$tp;&$_($Cp)for@$Dp;&$_($Jp)for@$P1;&$_($Np)for@$P1;&$_($Rp)for@$P1;&$_($Up)for@$Vp;&$_($Zp)for@$P1;&$_($eq)for@$fq;&$_($jq)for@$P1;&$_($nq)for@$P1;&$_($qq)for@$P1;&$_($tq)for@$uq;&$_($wq)for@$xq;&$_($Fq)for@$Gq;&$_($dr)for@$er;&$_($gr)for@$Gq;&$_($lr)for@$P1;&$_($pr)for@$P1;&$_($tr)for@$P1;&$_($vr)for@$wr;ni->run(@ARGV);
__DATA__
