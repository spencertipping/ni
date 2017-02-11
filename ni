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
$ni::self = bless {named => {}}, 'lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->resolve($_[0]) : $ni::self}
sub ni::eval {eval shift}
*{'lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'lib/fn::OVERLOAD'} = {};
*{'lib/fn::(bool'} = sub {1};
*{'lib/fn::()'}    = sub {};
*{'lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'lib/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:lib/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
sub fn($);
_
$c=q'applied_to';$d=q'metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'class.c';$i={$h,1};$j=q'module.c';$k={$h,1,$j,1};$l=q'lib/behavior.c';$m=q'lib/branch.c';$n=q'lib/dataslice.c';$o=q'lib/doc.c';$p=q'lib/fn.c';$q=q'lib/image.c';$r=q'lib/ni.c';$s=q'lib/slice.c';$t=q'lib/tag.c';$u=q'lib/test_value.c';$v=q'object.c';$w=q'unix/cat.c';$x=q'unix/fd.c';$y=q'unix/fifo.c';$z=q'unix/file.c';$A=q'unix/io.c';$B=q'unix/pid.c';$C=q'unix/pipeline.c';$D=q'unix/str.c';$E={$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$F=[undef];$G=q'metaclass';$H=bless({$c,$E,$f,$v,$g,$F},$G);$I=q'metaclass::ctors';$J={$s,1};$K={$h,1,$l,1,$m,1,$n,1,$s,1,$t,1,$j,1};$L=[$H];$M=bless({$c,$K,$f,$l,$g,$L},$G);$N=q'metaclass::ctors';$O=[$M];$P=bless({$c,$J,$f,$s,$g,$O},$G);$Q=q'metaclass::ctors';$R=q'lib/slice';$S={$R,1};$T=q'class';$U=q'lib/behavior';$V=q'lib/branch';$W=q'lib/dataslice';$X=q'lib/tag';$Y=q'lib/test_value.c';$Z=q'module';$c1={$T,1,$h,1,$U,1,$l,1,$V,1,$m,1,$W,1,$n,1,$o,1,$p,1,$q,1,$r,1,$R,1,$s,1,$X,1,$t,1,$Y,1,$G,1,$d,1,$Z,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$d1=q'lib/doc';$e1=q'lib/fn';$f1=q'lib/image';$g1=q'lib/ni';$h1=q'lib/test_value';$i1=q'lib/test_value.c';$j1=q'object';$k1=q'unix/cat';$l1=q'unix/fd';$m1=q'unix/fifo';$n1=q'unix/file';$o1=q'unix/io';$p1=q'unix/pid';$q1=q'unix/pipeline';$r1=q'unix/str';$s1={$T,1,$h,1,$U,1,$l,1,$V,1,$m,1,$W,1,$n,1,$d1,1,$o,1,$e1,1,$p,1,$f1,1,$q,1,$g1,1,$r,1,$R,1,$s,1,$X,1,$t,1,$h1,1,$i1,1,$G,1,$d,1,$Z,1,$j,1,$j1,1,$v,1,$k1,1,$w,1,$l1,1,$x,1,$m1,1,$y,1,$n1,1,$z,1,$o1,1,$A,1,$p1,1,$B,1,$q1,1,$C,1,$r1,1,$D,1};$t1={};$u1=q'ctor';$v1=undef;$w1=q'dtor';$x1=q'methods';$y1={$p,1};$z1=[$H];$A1=bless({$c,$y1,$f,$p,$g,$z1},$G);$B1=q'metaclass::ctors';$C1={$e1,1};$D1={};$E1=q'code';$F1=q'shift->compile';$G1=q'proto';$H1=q'';$I1=bless({$E1,$F1,$G1,$H1},$e1);$J1=q'lib/fn::ctors';$K1=q'compile';$L1=q'local $@;
my $self = shift;
$$self{proto} ||= \'\';
$$self{fn} = ni::eval "sub $$self{proto} {$$self{code}\\n}";
die "ni:lib/fn: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$M1=bless({$E1,$L1,$G1,$H1},$e1);$N1=q'instantiate';$O1=q'my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : \'\';
+{code        => $code,
  proto       => $proto,
  annotations => [@_]};';$P1=bless({$E1,$O1,$G1,$H1},$e1);$Q1={$K1,$M1,$N1,$P1};$R1=q'lib/fn_init.b';$S1=bless({$c,$D1,$u1,$I1,$w1,$v1,$x1,$Q1,$f,$R1},$R);$T1=q'lib/slice::ctors';$U1={};$V1=q'annotations';$W1=[];$X1=q'shift->{\'annotations\'}';$Y1=bless({$V1,$W1,$E1,$X1,$G1,$H1},$e1);$Z1=[];$c2=q'shift->{\'code\'}';$d2=bless({$V1,$Z1,$E1,$c2,$G1,$H1},$e1);$e2=q'fn';$f2=[];$g2=q'shift->{\'fn\'}';$h2=bless({$V1,$f2,$E1,$g2,$G1,$H1},$e1);$i2={$V1,$Y1,$E1,$d2,$e2,$h2};$j2=q'lib/fn_ro.b';$k2=bless({$c,$U1,$u1,$v1,$w1,$v1,$x1,$i2,$f,$j2},$R);$l2=q'lib/slice::ctors';$m2={};$n2=q'(""';$o2=[];$p2=q'shift->{code}';$q2=bless({$V1,$o2,$E1,$p2,$G1,$H1},$e1);$r2=q'(eq';$s2=[];$t2=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])';$u2=bless({$V1,$s2,$E1,$t2,$G1,$H1},$e1);$v2={$n2,$q2,$r2,$u2};$w2=q'lib/fn_ops.b';$x2=bless({$c,$m2,$u1,$v1,$w1,$v1,$x1,$v2,$f,$w2},$R);$y2=q'lib/slice::ctors';$z2={};$A2=q'serialize';$B2=[];$C2=q'local $_;
my ($self, $quote) = @_;
$quote->quote_class(ref $self);

(my $code = $$self{code}) =~ s/^\\s*\\n|\\s*$//g;
my @lines = split /\\n/, $code;
my $spaces = length $code;
for (@lines) {
  $spaces = length $1 if /^([ \\t]*)\\S/ && length $1 < $spaces;
}
$spaces = \' \' x $spaces;
s/^$spaces// for @lines;

my %state = %$self;
delete $state{fn};
$state{code} = join "\\n", @lines;
$quote->quote_blessed(\\%state, ref $self);';$D2=bless({$V1,$B2,$E1,$C2,$G1,$H1},$e1);$E2={$A2,$D2};$F2=q'lib/fn_serialize.b';$G2=bless({$c,$z2,$u1,$v1,$w1,$v1,$x1,$E2,$f,$F2},$R);$H2=q'lib/slice::ctors';$I2=[undef,undef,$S1,$k2,$x2,$G2];$J2=bless({$c,$C1,$f,$e1,$g,$I2},$p);$K2=q'lib/fn.c::ctors';$L2=q'ni \'ni:\' . ref shift';$M2=bless({$E1,$L2,$G1,$H1},$e1);$N2={$T,$M2};$O2=q'lib/instance.b';$P2=bless({$c,$t1,$u1,$v1,$w1,$v1,$x1,$N2,$f,$O2},$R);$Q2=q'lib/test_value.c';$R2=q'lib/slice::ctors';$S2=[$P2];$T2=bless({$c,$s1,$f,$j1,$g,$S2},$v);$U2=q'object.c::ctors';$V2={};$W2=q'doc';$X2=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:lib/doc\')->new($name);';$Y2=bless({$E1,$X2,$G1,$H1},$e1);$Z2={$W2,$Y2};$c3=q'lib/documentable.b';$d3=bless({$c,$V2,$u1,$v1,$w1,$v1,$x1,$Z2,$f,$c3},$R);$e3=q'lib/test_value.c';$f3=q'lib/slice::ctors';$g3=[$T2,$d3];$h3=bless({$c,$c1,$f,$U,$g,$g3},$l);$i3=q'lib/behavior.c::ctors';$j3={};$k3=q'my $s = shift; ni->def($s->name, $s)';$l3=bless({$E1,$k3,$G1,$H1},$e1);$m3=q'$_[0]->namespace . ":" . $_[0]->{name}';$n3=bless({$E1,$m3,$G1,$H1},$e1);$o3={$f,$n3};$p3=q'lib/named.b';$q3=bless({$c,$j3,$u1,$l3,$w1,$v1,$x1,$o3,$f,$p3},$R);$r3=q'lib/test_value.c';$s3=q'lib/slice::ctors';$t3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$u3=bless({$E1,$t3,$G1,$H1},$e1);$v3=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
if (grep /^\\(/, keys %{$$self{methods}}) {
  *{"$p\\::()"} = sub {};
  *{"$p\\::OVERLOAD"} = {};
}
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$w3=bless({$E1,$v3,$G1,$H1},$e1);$x3=q'lib/slice::apply';$y3=q'lib/slice::apply_unsafe';$z3={};$A3=q'apply';$B3=q'apply_unsafe';$C3={$A3,$u3,$B3,$w3};$D3=q'lib/slice.b';$E3=bless({$c,$z3,$x1,$C3,$f,$D3},$R);$F3=q'lib/slice::ctors';$G3={};$H3=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$I3=bless({$E1,$H3,$G1,$H1},$e1);$J3={$N1,$I3};$K3=q'lib/slice_init.b';$L3=bless({$c,$G3,$x1,$J3,$f,$K3},$R);$M3=q'lib/slice::ctors';$N3={};$O3=[];$P3=q'local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq \'ni:lib/slice.b\') {
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
$g;';$Q3=bless({$V1,$O3,$E1,$P3,$G1,$H1},$e1);$R3={$A2,$Q3};$S3=q'lib/slice_serialize.b';$T3=bless({$c,$N3,$u1,$v1,$w1,$v1,$x1,$R3,$f,$S3},$R);$U3=q'lib/slice::ctors';$V3=[$h3,$q3,$E3,$L3,$T3];$W3=bless({$c,$S,$f,$R,$g,$V3},$s);$X3=q'lib/slice.c::ctors';$Y3={};$Z3=q'DESTROY';$c4=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$d4=bless({$E1,$c4,$G1,$H1},$e1);$e4=q'new';$f4=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$g4=bless({$E1,$f4,$G1,$H1},$e1);$h4={$Z3,$d4,$e4,$g4};$i4=q'lib/instantiable.b';$j4=bless({$c,$Y3,$x1,$h4,$f,$i4},$R);$k4=q'lib/test_value.c';$l4=q'lib/slice::ctors';$m4=[$H,$j4,$M];$n4=bless({$c,$k,$f,$j,$g,$m4},$G);$o4=q'metaclass::ctors';$p4=[$n4];$q4=bless({$c,$i,$f,$h,$g,$p4},$G);$r4=q'metaclass::ctors';$s4=q'lib/test_value.c';$t4={$T,1,$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$s4,1,$d,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$u4=q'lib/test_value.c';$v4={$T,1,$h,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u4,1,$G,1,$d,1,$Z,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$w4={$t,1};$x4=[$M];$y4=bless({$c,$w4,$f,$t,$g,$x4},$G);$z4=q'metaclass::ctors';$A4={$X,1};$B4={};$C4=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$D4=bless({$E1,$C4,$G1,$H1},$e1);$E4={$A3,$D4};$F4=q'lib/tag.b';$G4=bless({$c,$B4,$u1,$v1,$w1,$v1,$x1,$E4,$f,$F4},$R);$H4=q'lib/slice::ctors';$I4={};$J4=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$K4=bless({$E1,$J4,$G1,$H1},$e1);$L4={$N1,$K4};$M4=q'lib/tag_init.b';$N4=bless({$c,$I4,$u1,$v1,$w1,$v1,$x1,$L4,$f,$M4},$R);$O4=q'lib/slice::ctors';$P4=[$h3,$q3,$G4,$N4];$Q4=bless({$c,$A4,$f,$X,$g,$P4},$t);$R4=q'lib/tag.c::ctors';$S4=q'lib/perlbranch.b';$T4={};$U4=q'add';$V4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$W4=bless({$E1,$V4,$G1,$H1},$e1);$X4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$Y4=bless({$E1,$X4,$G1,$H1},$e1);$Z4={$U4,$W4,$A3,$Y4};$c5=q'lib/branch.b';$d5=bless({$c,$T4,$u1,$v1,$w1,$v1,$x1,$Z4,$f,$c5},$R);$e5=q'lib/test_value.c';$f5=q'lib/slice::ctors';$g5={};$h5=q'namespace';$i5=q'\'ni\'';$j5=bless({$E1,$i5,$G1,$H1},$e1);$k5={$h5,$j5};$l5=q'lib/named_in_ni.b';$m5=bless({$c,$g5,$u1,$v1,$w1,$v1,$x1,$k5,$f,$l5},$R);$n5=q'lib/test_value.c';$o5=q'lib/slice::ctors';$p5={};$q5=q'package';$r5=q'shift->{name}';$s5=bless({$E1,$r5,$G1,$H1},$e1);$t5={$q5,$s5};$u5=q'lib/namespaced.b';$v5=bless({$c,$p5,$u1,$v1,$w1,$v1,$x1,$t5,$f,$u5},$R);$w5=q'lib/test_value.c';$x5=q'lib/slice::ctors';$y5={};$z5=q'resolve';$A5=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$B5=bless({$E1,$A5,$G1,$H1},$e1);$C5={$z5,$B5};$D5=q'lib/resolver.b';$E5=bless({$c,$y5,$u1,$v1,$w1,$v1,$x1,$C5,$f,$D5},$R);$F5=q'lib/test_value.c';$G5=q'lib/slice::ctors';$H5=[$d5,$q3,$m5,$v5,$E5];$I5=bless({$f,$S4,$g,$H5},$X);$J5=q'lib/tag::ctors';$K5={};$L5=q'my $s = shift; $s->apply($s->package)';$M5=bless({$E1,$L5,$G1,$H1},$e1);$N5=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$O5=bless({$E1,$N5,$G1,$H1},$e1);$P5={$N1,$O5};$Q5=q'lib/class_init.b';$R5=bless({$c,$K5,$u1,$M5,$w1,$v1,$x1,$P5,$f,$Q5},$R);$S5=q'lib/test_value.c';$T5=q'lib/slice::ctors';$U5={$m,1};$V5=[$M];$W5=bless({$c,$U5,$f,$m,$g,$V5},$G);$X5=q'metaclass::ctors';$Y5={$V,1};$Z5={};$c6=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$d6=bless({$E1,$c6,$G1,$H1},$e1);$e6={$N1,$d6};$f6=q'lib/branch_init.b';$g6=bless({$c,$Z5,$u1,$v1,$w1,$v1,$x1,$e6,$f,$f6},$R);$h6=q'lib/slice::ctors';$i6=[$h3,$q3,$d5,$g6,undef];$j6=bless({$c,$Y5,$f,$V,$g,$i6},$m);$k6=q'lib/branch.c::ctors';$l6=q'lib/test_value.c';$m6={$T,1,$h,1,$l,1,$V,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$l6,1,$G,1,$d,1,$Z,1,$j,1,$v,1,$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$n6=q'lib/definition.b';$o6={};$p6=q'def';$q6=q'shift->add(ni(\'ni:lib/slice\')->new(@_))';$r6=bless({$E1,$q6,$G1,$H1},$e1);$s6={$p6,$r6};$t6=q'lib/definition_def.b';$u6=bless({$c,$o6,$u1,$v1,$w1,$v1,$x1,$s6,$f,$t6},$R);$v6=q'lib/test_value.c';$w6=q'lib/slice::ctors';$x6={};$y6=q'ro';$z6=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$A6=bless({$E1,$z6,$G1,$H1},$e1);$B6=q'rw';$C6=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$D6=bless({$E1,$C6,$G1,$H1},$e1);$E6={$y6,$A6,$B6,$D6};$F6=q'lib/accessor.b';$G6=bless({$c,$x6,$u1,$v1,$w1,$v1,$x1,$E6,$f,$F6},$R);$H6=q'lib/test_value.c';$I6=q'lib/slice::ctors';$J6={};$K6=q'shift->name';$L6=bless({$E1,$K6,$G1,$H1},$e1);$M6={$n2,$L6};$N6=q'lib/name_as_string.b';$O6=bless({$c,$J6,$u1,$v1,$w1,$v1,$x1,$M6,$f,$N6},$R);$P6=q'lib/test_value.c';$Q6=q'lib/slice::ctors';$R6={};$S6=q'ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);';$T6=bless({$E1,$S6,$G1,$H1},$e1);$U6={$r2,$T6};$V6=q'lib/ref_eq.b';$W6=bless({$c,$R6,$u1,$v1,$w1,$v1,$x1,$U6,$f,$V6},$R);$X6=q'lib/test_value.c';$Y6=q'lib/slice::ctors';$Z6={};$c7=q'defdata';$d7=q'shift->add(ni(\'ni:lib/dataslice\')->new(@_))';$e7=bless({$E1,$d7,$G1,$H1},$e1);$f7={$c7,$e7};$g7=q'lib/definition_defdata.b';$h7=bless({$c,$Z6,$u1,$v1,$w1,$v1,$x1,$f7,$f,$g7},$R);$i7=q'lib/test_value.c';$j7=q'lib/slice::ctors';$k7=[$u6,$G6,$O6,$W6,$h7];$l7=bless({$c,$m6,$f,$n6,$g,$k7},$V);$m7=q'lib/branch::ctors';$n7=[$I5,$R5,$T2,$h3,$l7];$o7=bless({$c,$v4,$f,$Z,$g,$n7},$j);$p7=q'module.c::ctors';$q7={};$r7=q'child';$s7=q'my ($self, $name, @slices) = @_;
ni("ni:metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$t7=bless({$E1,$s7,$G1,$H1},$e1);$u7={$r7,$t7};$v7=q'lib/subclass.b';$w7=bless({$c,$q7,$u1,$v1,$w1,$v1,$x1,$u7,$f,$v7},$R);$x7=q'lib/test_value.c';$y7=q'lib/slice::ctors';$z7=[$o7,$j4,$R5,$o7,$w7];$A7=bless({$c,$t4,$f,$T,$g,$z7},$h);$B7=q'class.c::ctors';$C7=[$A7];$D7=bless({$c,$e,$f,$d,$g,$C7},$G);$E7=q'metaclass::ctors';$F7={$G,1};$G7=[$I5,$j4,$R5,$o7];$H7=bless({$c,$F7,$f,$G,$g,$G7},$d);$I7=q'metaclass.c::ctors';$J7={$r,1};$K7=[$H];$L7=bless({$c,$J7,$f,$r,$g,$K7},$G);$M7=q'metaclass::ctors';$N7={$g1,1};$O7={};$P7=q'is_mutable';$Q7=[];$R7=q'$0 ne "-" && -w $0';$S7=bless({$V1,$Q7,$E1,$R7,$G1,$H1},$e1);$T7=q'modify';$U7=[];$V7=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$W7=bless({$V1,$U7,$E1,$V7,$G1,$H1},$e1);$X7={$P7,$S7,$T7,$W7};$Y7=q'lib/ni_self.b';$Z7=bless({$c,$O7,$u1,$v1,$w1,$v1,$x1,$X7,$f,$Y7},$R);$c8=q'lib/slice::ctors';$d8={};$e8=q'exists';$f8=[];$g8=q'exists $_[0]->{named}{$_[1]}';$h8=bless({$V1,$f8,$E1,$g8,$G1,$H1},$e1);$i8=q'quoted';$j8=[];$k8=q'my $self = shift;
my $q = ni(\'ni:lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$l8=bless({$V1,$j8,$E1,$k8,$G1,$H1},$e1);$m8={$e8,$h8,$i8,$l8};$n8=q'lib/ni_image.b';$o8=bless({$c,$d8,$u1,$v1,$w1,$v1,$x1,$m8,$f,$n8},$R);$p8=q'lib/slice::ctors';$q8={};$r8=q'--internal/+=';$s8=[];$t8=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$u8=bless({$V1,$s8,$E1,$t8,$G1,$H1},$e1);$v8=q'--internal/eval';$w8=[];$x8=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$y8=bless({$V1,$w8,$E1,$x8,$G1,$H1},$e1);$z8=q'--internal/image';$A8=[];$B8=q'shift->quoted->write(\\*STDOUT);
0;';$C8=bless({$V1,$A8,$E1,$B8,$G1,$H1},$e1);$D8=q'--internal/test';$E8=[];$F8=q'my $self = shift;
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
!!$fails;';$G8=bless({$V1,$E8,$E1,$F8,$G1,$H1},$e1);$H8=q'run';$I8=[];$J8=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$K8=bless({$V1,$I8,$E1,$J8,$G1,$H1},$e1);$L8={$r8,$u8,$v8,$y8,$z8,$C8,$D8,$G8,$H8,$K8};$M8=q'lib/ni_main.b';$N8=bless({$c,$q8,$u1,$v1,$w1,$v1,$x1,$L8,$f,$M8},$R);$O8=q'lib/slice::ctors';$P8={};$Q8=[];$R8=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";';$S8=bless({$V1,$Q8,$E1,$R8,$G1,$H1},$e1);$T8=q'resolver_for';$U8=[];$V8=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$W8=bless({$V1,$U8,$E1,$V8,$G1,$H1},$e1);$X8={$z5,$S8,$T8,$W8};$Y8=q'lib/ni_resolver.b';$Z8=bless({$c,$P8,$u1,$v1,$w1,$v1,$x1,$X8,$f,$Y8},$R);$c9=q'lib/slice::ctors';$d9={};$e9=q'fork';$f9=[];$g9=q'my ($class, $fn) = @_;
my $stdin  = ni(\'ni:unix/fifo\')->new;
my $stdout = ni(\'ni:unix/fifo\')->new;
my $stderr = ni(\'ni:unix/fifo\')->new;
my $pid    = fork;
die "ni:unix/pid.c: failed to fork: $!" unless defined $pid;

return ni(\'ni:unix/pid\')->new($pid,
  $stdin->write_side,
  $stdout->read_side,
  $stderr->read_side) if $pid;

$stdin->read_side;
$stdout->write_side;
$stderr->write_side;
exit &$fn($stdin, $stdout, $stderr);';$h9=bless({$V1,$f9,$E1,$g9,$G1,$H1},$e1);$i9=q'fork_exec';$j9=[];$k9=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:unix/pid.c: failed to exec @argv: $!";
});';$l9=bless({$V1,$j9,$E1,$k9,$G1,$H1},$e1);$m9={$e9,$h9,$i9,$l9};$n9=q'/lib/ni_pid_ctors';$o9=bless({$c,$d9,$u1,$v1,$w1,$v1,$x1,$m9,$f,$n9},$R);$p9=q'lib/slice::ctors';$q9=[$T2,$Z7,$o8,$N8,$Z8,$o9];$r9=bless({$c,$N7,$f,$g1,$g,$q9},$r);$s9=q'lib/ni.c::ctors';$t9=q'named';$u9=q'ni.doc:class';$v9={$o,1};$w9=[$H];$x9=bless({$c,$v9,$f,$o,$g,$w9},$G);$y9=q'metaclass::ctors';$z9={$d1,1};$A9={};$B9=q'shift; +{name => shift, doc => []}';$C9=bless({$E1,$B9,$G1,$H1},$e1);$D9={$N1,$C9};$E9=q'lib/doc_init.b';$F9=bless({$c,$A9,$u1,$v1,$w1,$v1,$x1,$D9,$f,$E9},$R);$G9=q'lib/slice::ctors';$H9={};$I9=q'\'ni.doc\'';$J9=bless({$E1,$I9,$G1,$H1},$e1);$K9={$h5,$J9};$L9=q'lib/doc_namespace.b';$M9=bless({$c,$H9,$u1,$v1,$w1,$v1,$x1,$K9,$f,$L9},$R);$N9=q'lib/slice::ctors';$O9={};$P9=q'AUTOLOAD';$Q9=q'my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;';$R9=bless({$E1,$Q9,$G1,$H1},$e1);$S9={$P9,$R9};$T9=q'lib/doc_define.b';$U9=bless({$c,$O9,$u1,$v1,$w1,$v1,$x1,$S9,$f,$T9},$R);$V9=q'lib/slice::ctors';$W9={};$X9=q'eg';$Y9=q'my $self = shift;
push @{$$self{doc}}, [eg => $_] for @_;
$self;';$Z9=bless({$E1,$Y9,$G1,$H1},$e1);$ca=q'tests';$da=q'my $self = shift;
my @flattened = map @$_, @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1]
    if !ref $flattened[$_] && $flattened[$_] eq \'eg\';
}
@tests;';$ea=bless({$E1,$da,$G1,$H1},$e1);$fa={$X9,$Z9,$ca,$ea};$ga=q'lib/doc_test.b';$ha=bless({$c,$W9,$u1,$v1,$w1,$v1,$x1,$fa,$f,$ga},$R);$ia=q'lib/slice::ctors';$ja=[$T2,$q3,$F9,$M9,$U9,$ha];$ka=bless({$c,$z9,$f,$d1,$g,$ja},$o);$la=q'lib/doc.c::ctors';$ma=q'synopsis';$na=q'
    ni(\'ni:object\')->child(\'message\')
      ->add(\'behaviorname.b\')           # add existing behavior
      ->def(\'message_init.b\',           # define new behavior
        instantiate => fn q{            # called from ->new()
          my ($class, $message) = @_;
          +{message => $message};       # return object to be blessed
        })
      ->def(\'behaviorname.b\',           # define another behavior
        method1 => fn q{
          my $self = shift;
          print "message for you sir! \'" . $$self{message} . "\'\\n";
        });
    ni(\'ni:child\')->new(\'hello world!\')->method1;
  ';$oa=[$ma,$na];$pa=q'description';$qa=q'ni:class is at the core of ni\'s object-oriented system, along with core
      classes like ni:object and ni:metaclass. There are two layers of
      abstraction involved here: Perl packages are modified by behaviors, and
      classes encode the higher-level declarative features you\'d expect from a
      language like Ruby or Smalltalk. This documentation covers both layers.';$ra=[$pa,$qa];$sa=q'behaviors';$ta=q'ni\'s objects are blessed Perl references, and behaviors are objects
      that modify Perl packages in specific ways. The simplest is
      ni:lib/slice, which represents a set of methods you can add to a
      package. TODO...';$ua=[$sa,$ta];$va=q'classes';$wa=q'ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni\'s classes are slice unions and as such don\'t
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn\'t in the picture,
      which makes multiple inheritance straightforward to implement.';$xa=q'TODO...';$ya=[$va,$wa,$xa];$za=[$oa,$ra,$ua,$ya];$Aa=bless({$W2,$za,$f,$T},$d1);$Ba=q'lib/doc::ctors';$Ca=q'ni.doc:lib/doc';$Da=q'
    ni("ni:some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$Ea=[$ma,$Da];$Fa=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$Ga=q'Documentation objects are internally represented as arrays of quoted
      method calls:';$Ha=[];$Ia=q'my $doc = ni("ni:lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];';$Ja=bless({$V1,$Ha,$E1,$Ia,$G1,$H1},$e1);$Ka=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":';$La=[];$Ma=q'my $doc = ni("ni:lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
now [$doc->tests] == [$passing_test, $failing_test];';$Na=bless({$V1,$La,$E1,$Ma,$G1,$H1},$e1);$Oa=[$pa,$Fa,$Ga,$X9,$Ja,$Ka,$X9,$Na];$Pa=[$Ea,$Oa];$Qa=bless({$W2,$Pa,$f,$d1},$d1);$Ra=q'ni.doc:unix/cat';$Sa=q'
    my $combined = ni(\'ni:unix/cat\')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into($destination_io);
  ';$Ta=[$ma,$Sa];$Ua=q'Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.';$Va=[$pa,$Ua];$Wa=[];$Xa=q'my $cat = ni("str:foo") + ni("str:bar");
my $dest = ni(\'ni:unix/str\')->new(my $data = "");
$cat->into($dest);
now ${$dest->data} == "foo\\nbar\\n";';$Ya=bless({$V1,$Wa,$E1,$Xa,$G1,$H1},$e1);$Za=[$X9,$Ya];$cb=[$Ta,$Va,$Za];$db=bless({$W2,$cb,$f,$k1},$d1);$eb=q'ni:/lib/ni_pid_ctors';$fb=q'ni:class';$gb=q'ni:class.c';$hb=q'ni:lib/accessor.b';$ib=q'ni:lib/behavior';$jb=q'ni:lib/behavior.c';$kb=q'ni:lib/branch';$lb=q'ni:lib/branch.b';$mb=q'ni:lib/branch.c';$nb=q'ni:lib/branch_init.b';$ob=q'ni:lib/class_init.b';$pb=q'ni:lib/dataslice';$qb={$n,1};$rb=[$M];$sb=bless({$c,$qb,$f,$n,$g,$rb},$G);$tb=q'metaclass::ctors';$ub={$W,1};$vb={};$wb=q'my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};';$xb=bless({$E1,$wb,$G1,$H1},$e1);$yb={$N1,$xb};$zb=q'lib/dataslice_init.b';$Ab=bless({$c,$vb,$u1,$v1,$w1,$v1,$x1,$yb,$f,$zb},$R);$Bb=q'lib/slice::ctors';$Cb={};$Db=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;';$Eb=bless({$E1,$Db,$G1,$H1},$e1);$Fb={$A3,$Eb};$Gb=q'lib/dataslice_apply.b';$Hb=bless({$c,$Cb,$u1,$v1,$w1,$v1,$x1,$Fb,$f,$Gb},$R);$Ib=q'lib/slice::ctors';$Jb=[$h3,$Ab,$Hb];$Kb=bless({$c,$ub,$f,$W,$g,$Jb},$n);$Lb=q'lib/dataslice.c::ctors';$Mb=q'ni:lib/dataslice.c';$Nb=q'ni:lib/dataslice_apply.b';$Ob=q'ni:lib/dataslice_init.b';$Pb=q'ni:lib/definition.b';$Qb=q'ni:lib/definition_def.b';$Rb=q'ni:lib/definition_defdata.b';$Sb=q'ni:lib/doc';$Tb=q'ni:lib/doc.c';$Ub=q'ni:lib/doc_define.b';$Vb=q'ni:lib/doc_init.b';$Wb=q'ni:lib/doc_namespace.b';$Xb=q'ni:lib/doc_test.b';$Yb=q'ni:lib/documentable.b';$Zb=q'ni:lib/fn';$cc=q'ni:lib/fn.c';$dc=q'ni:lib/fn_init.b';$ec=q'ni:lib/fn_ops.b';$fc=q'ni:lib/fn_ro.b';$gc=q'ni:lib/fn_serialize.b';$hc=q'ni:lib/global_static_test.b';$ic={};$jc=q'now';$kc=[];$lc=q'ni(\'ni:lib/test_value\')->new(shift)';$mc=q'($)';$nc=bless({$V1,$kc,$E1,$lc,$G1,$mc},$e1);$oc={$jc,$nc};$pc=q'lib/global_static_test.b';$qc=bless({$c,$ic,$u1,$v1,$w1,$v1,$x1,$oc,$f,$pc},$R);$rc=q'main';$sc=q'lib/slice::ctors';$tc=q'ni:lib/image';$uc={$q,1};$vc=[$H];$wc=bless({$c,$uc,$f,$q,$g,$vc},$G);$xc=q'metaclass::ctors';$yc={$f1,1};$zc={};$Ac=[];$Bc=q'my $class = shift;
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
  ordering     => []};';$Cc=bless({$V1,$Ac,$E1,$Bc,$G1,$H1},$e1);$Dc={$N1,$Cc};$Ec=q'lib/image_init.b';$Fc=bless({$c,$zc,$u1,$v1,$w1,$v1,$x1,$Dc,$f,$Ec},$R);$Gc=q'lib/slice::ctors';$Hc={};$Ic=q'address';$Jc=[];$Kc=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$Lc=bless({$V1,$Jc,$E1,$Kc,$G1,$H1},$e1);$Mc=q'allocate_gensym';$Nc=[];$Oc=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Pc=bless({$V1,$Nc,$E1,$Oc,$G1,$H1},$e1);$Qc=q'boot_side_effect';$Rc=[];$Sc=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Tc=bless({$V1,$Rc,$E1,$Sc,$G1,$H1},$e1);$Uc=q'circular_links';$Vc=[];$Wc=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$Xc=bless({$V1,$Vc,$E1,$Wc,$G1,$H1},$e1);$Yc=q'finalizer';$Zc=[];$cd=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$dd=bless({$V1,$Zc,$E1,$cd,$G1,$H1},$e1);$ed=q'gensym';$fd=[];$gd=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$hd=bless({$V1,$fd,$E1,$gd,$G1,$H1},$e1);$id=q'is_circular';$jd=[];$kd=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$ld=bless({$V1,$jd,$E1,$kd,$G1,$H1},$e1);$md=q'quote';$nd=[];$od=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$pd=bless({$V1,$nd,$E1,$od,$G1,$H1},$e1);$qd=q'quote_array';$rd=[];$sd=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$td=bless({$V1,$rd,$E1,$sd,$G1,$H1},$e1);$ud=q'quote_blessed';$vd=[];$wd=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$xd=bless({$V1,$vd,$E1,$wd,$G1,$H1},$e1);$yd=q'quote_class';$zd=[];$Ad=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$Bd=bless({$V1,$zd,$E1,$Ad,$G1,$H1},$e1);$Cd=q'quote_hash';$Dd=[];$Ed=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$Fd=bless({$V1,$Dd,$E1,$Ed,$G1,$H1},$e1);$Gd=q'quote_object';$Hd=[];$Id=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$Jd=bless({$V1,$Hd,$E1,$Id,$G1,$H1},$e1);$Kd=q'quote_scalar';$Ld=[];$Md=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Nd=bless({$V1,$Ld,$E1,$Md,$G1,$H1},$e1);$Od=q'quote_value';$Pd=[];$Qd=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Rd=bless({$V1,$Pd,$E1,$Qd,$G1,$H1},$e1);$Sd=q'reconstruction';$Td=[];$Ud=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Vd=bless({$V1,$Td,$E1,$Ud,$G1,$H1},$e1);$Wd=q'side_effect';$Xd=[];$Yd=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Zd=bless({$V1,$Xd,$E1,$Yd,$G1,$H1},$e1);$ce=q'write';$de=[];$ee=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$fe=bless({$V1,$de,$E1,$ee,$G1,$H1},$e1);$ge={$Ic,$Lc,$Mc,$Pc,$Qc,$Tc,$Uc,$Xc,$Yc,$dd,$ed,$hd,$id,$ld,$md,$pd,$qd,$td,$ud,$xd,$yd,$Bd,$Cd,$Fd,$Gd,$Jd,$Kd,$Nd,$Od,$Rd,$Sd,$Vd,$Wd,$Zd,$ce,$fe};$he=q'lib/image_quoting.b';$ie=bless({$c,$Hc,$u1,$v1,$w1,$v1,$x1,$ge,$f,$he},$R);$je=q'lib/slice::ctors';$ke=[$T2,$Fc,$ie];$le=bless({$c,$yc,$f,$f1,$g,$ke},$q);$me=q'lib/image.c::ctors';$ne=q'ni:lib/image.c';$oe=q'ni:lib/image_init.b';$pe=q'ni:lib/image_quoting.b';$qe=q'ni:lib/instance.b';$re=q'ni:lib/instantiable.b';$se=q'ni:lib/json.b';$te={};$ue=q'json_decode';$ve=[];$we=q'local $_;
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
wantarray ? @$r : $$r[0];';$xe=bless({$V1,$ve,$E1,$we,$G1,$mc},$e1);$ye=q'json_encode';$ze=[];$Ae=q'local $_;
my ($v) = @_;
return "[" . join(\',\', map ni::json_encode($_), @$v) . "]" if \'ARRAY\' eq ref $v;
return "{" . join(\',\', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if \'HASH\' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : \'null\';';$Be=bless({$V1,$ze,$E1,$Ae,$G1,$mc},$e1);$Ce=q'json_escape';$De=[];$Ee=q'(my $x = $_[0]) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . $ni::json_escapes{$1}/eg;
"\\"$x\\"";';$Fe=bless({$V1,$De,$E1,$Ee,$G1,$mc},$e1);$Ge=q'json_unescape';$He=[];$Ie=q'my $x = substr $_[0], 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;';$Je=bless({$V1,$He,$E1,$Ie,$G1,$mc},$e1);$Ke=q'json_unescape_one';$Le=[];$Me=q'$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1';$Ne=bless({$V1,$Le,$E1,$Me,$G1,$mc},$e1);$Oe={$ue,$xe,$ye,$Be,$Ce,$Fe,$Ge,$Je,$Ke,$Ne};$Pe=q'lib/json.b';$Qe=bless({$c,$te,$u1,$v1,$w1,$v1,$x1,$Oe,$f,$Pe},$R);$Re=q'ni';$Se=q'lib/slice::ctors';$Te=q'ni:lib/name_as_string.b';$Ue=q'ni:lib/named.b';$Ve=q'ni:lib/named_in_ni.b';$We=q'ni:lib/namespaced.b';$Xe=q'ni:lib/ni';$Ye=q'ni:lib/ni.c';$Ze=q'ni:lib/ni_image.b';$cf=q'ni:lib/ni_main.b';$df=q'ni:lib/ni_resolver.b';$ef=q'ni:lib/ni_self.b';$ff=q'ni:lib/ni_static_util.b';$gf={};$hf=q'abbrev';$if=[];$jf=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$kf=bless({$V1,$if,$E1,$jf,$G1,$H1},$e1);$lf=q'dor';$mf=[];$nf=q'defined $_[0] ? $_[0] : $_[1]';$of=bless({$V1,$mf,$E1,$nf,$G1,$H1},$e1);$pf=q'indent';$qf=[];$rf=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$sf=bless({$V1,$qf,$E1,$rf,$G1,$H1},$e1);$tf=q'max';$uf=[];$vf=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$wf=bless({$V1,$uf,$E1,$vf,$G1,$H1},$e1);$xf=q'maxstr';$yf=[];$zf=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Af=bless({$V1,$yf,$E1,$zf,$G1,$H1},$e1);$Bf=q'mean';$Cf=[];$Df=q'sum(@_) / (@_ || 1)';$Ef=bless({$V1,$Cf,$E1,$Df,$G1,$H1},$e1);$Ff=q'min';$Gf=[];$Hf=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$If=bless({$V1,$Gf,$E1,$Hf,$G1,$H1},$e1);$Jf=q'minstr';$Kf=[];$Lf=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Mf=bless({$V1,$Kf,$E1,$Lf,$G1,$H1},$e1);$Nf=q'sgr';$Of=[];$Pf=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Qf=bless({$V1,$Of,$E1,$Pf,$G1,$H1},$e1);$Rf=q'sr';$Sf=[];$Tf=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Uf=bless({$V1,$Sf,$E1,$Tf,$G1,$H1},$e1);$Vf=q'sum';$Wf=[];$Xf=q'local $_; my $x = 0; $x += $_ for @_; $x';$Yf=bless({$V1,$Wf,$E1,$Xf,$G1,$H1},$e1);$Zf=q'swap';$cg=[];$dg=q'@_[0, 1] = @_[1, 0]';$eg=bless({$V1,$cg,$E1,$dg,$G1,$H1},$e1);$fg={$hf,$kf,$lf,$of,$pf,$sf,$tf,$wf,$xf,$Af,$Bf,$Ef,$Ff,$If,$Jf,$Mf,$Nf,$Qf,$Rf,$Uf,$Vf,$Yf,$Zf,$eg};$gg=q'lib/ni_static_util.b';$hg=bless({$c,$gf,$u1,$v1,$w1,$v1,$x1,$fg,$f,$gg},$R);$ig=q'lib/slice::ctors';$jg=q'ni:lib/perlbranch.b';$kg=q'ni:lib/ref_eq.b';$lg=q'ni:lib/resolver.b';$mg=q'ni:lib/slice';$ng=q'ni:lib/slice.b';$og=q'ni:lib/slice.c';$pg=q'ni:lib/slice_init.b';$qg=q'ni:lib/slice_serialize.b';$rg=q'ni:lib/static_fn.b';$sg={};$tg=[];$ug=q'ni(\'ni:lib/fn\')->new(@_)';$vg=bless({$V1,$tg,$E1,$ug,$G1,$mc},$e1);$wg=q'fp';$xg=[];$yg=q'ni(\'ni:lib/fn\')->new(@_)';$zg=q'($$)';$Ag=bless({$V1,$xg,$E1,$yg,$G1,$zg},$e1);$Bg={$e2,$vg,$wg,$Ag};$Cg=q'lib/static_fn.b';$Dg=bless({$c,$sg,$u1,$v1,$w1,$v1,$x1,$Bg,$f,$Cg},$R);$Eg=q'lib/slice::ctors';$Fg=q'ni:lib/subclass.b';$Gg=q'ni:lib/tag';$Hg=q'ni:lib/tag.b';$Ig=q'ni:lib/tag.c';$Jg=q'ni:lib/tag_init.b';$Kg=q'ni:lib/test_value';$Lg=q'lib/test_value.c';$Mg={$Lg,1};$Ng=q'lib/test_value.c';$Og=[$H];$Pg=bless({$c,$Mg,$f,$Ng,$g,$Og},$G);$Qg=q'metaclass::ctors';$Rg={$h1,1};$Sg={};$Tg=[];$Ug=q'\\$_[1]';$Vg=bless({$V1,$Tg,$E1,$Ug,$G1,$H1},$e1);$Wg={$N1,$Vg};$Xg=q'lib/test_value_init.b';$Yg=bless({$c,$Sg,$u1,$v1,$w1,$v1,$x1,$Wg,$f,$Xg},$R);$Zg=q'lib/slice::ctors';$ch={};$dh=q'(==';$eh=[];$fh=q'my ($self, $rhs) = @_;
my $diff = $self->diff($rhs);
die $self->class->new($diff) if defined $diff;
1;';$gh=bless({$V1,$eh,$E1,$fh,$G1,$H1},$e1);$hh=q'diff';$ih=[];$jh=q'my ($self, $rhs) = @_;
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
} elsif ($realtype eq \'SCALAR\') {
  return $class->new($$lhs)->diff($$rhs);
} elsif (!$rl) {
  return {scalar_difference => [$lhs, $rhs]} unless $lhs eq $rhs;
}
return undef;';$kh=bless({$V1,$ih,$E1,$jh,$G1,$H1},$e1);$lh={$dh,$gh,$hh,$kh};$mh=q'lib/test_value_eq.b';$nh=bless({$c,$ch,$u1,$v1,$w1,$v1,$x1,$lh,$f,$mh},$R);$oh=q'lib/slice::ctors';$ph={};$qh=[];$rh=q'ni::json_encode ${$_[0]}';$sh=bless({$V1,$qh,$E1,$rh,$G1,$H1},$e1);$th={$n2,$sh};$uh=q'lib/test_value_str.b';$vh=bless({$c,$ph,$u1,$v1,$w1,$v1,$x1,$th,$f,$uh},$R);$wh=q'lib/slice::ctors';$xh=[$T2,$Yg,$nh,$vh];$yh=q'lib/test_value.c';$zh=bless({$c,$Rg,$f,$h1,$g,$xh},$yh);$Ah=q'lib/test_value.c::ctors';$Bh=q'ni:lib/test_value.c';$Ch=q'ni:lib/test_value_eq.b';$Dh=q'ni:lib/test_value_init.b';$Eh=q'ni:lib/test_value_str.b';$Fh=q'ni:main';$Gh={$rc,1};$Hh=[$Dg,$qc];$Ih=bless({$c,$Gh,$f,$rc,$g,$Hh},$Z);$Jh=q'module::ctors';$Kh=q'ni:metaclass';$Lh=q'ni:metaclass.c';$Mh=q'ni:module';$Nh=q'ni:module.c';$Oh=q'ni:ni';$Ph={$Re,1};$Qh={$Re,1};$Rh=q'data';$Sh=q'json_escapes';$Th=q'';$Uh=q'b';$Vh=q'	';$Wh=q't';$Xh=q'
';$Yh=q'n';$Zh=q'';$ci=q'r';$di=q'"';$ei=q'/';$fi=q'\\';$gi={$Th,$Uh,$Vh,$Wh,$Xh,$Yh,$Zh,$ci,$di,$di,$ei,$ei,$fi,$fi};$hi=q'json_unescapes';$ii={$di,$di,$ei,$ei,$fi,$fi,$Uh,$Th,$Yh,$Xh,$ci,$Zh,$Wh,$Vh};$ji={$Sh,$gi,$hi,$ii};$ki=q'lib/json_data.b';$li=bless({$c,$Qh,$Rh,$ji,$f,$ki},$W);$mi=q'lib/dataslice::ctors';$ni=[$li,$Qe,$hg];$oi=bless({$c,$Ph,$f,$Re,$g,$ni},$Z);$pi=q'ni:object';$qi=q'ni:object.c';$ri=q'ni:unix/cat';$si={$w,1};$ti={$w,1,$x,1,$y,1,$z,1,$A,1,$B,1,$C,1,$D,1};$ui=[$H];$vi=bless({$c,$ti,$f,$A,$g,$ui},$G);$wi=q'metaclass::ctors';$xi=[$vi];$yi=bless({$c,$si,$f,$w,$g,$xi},$G);$zi=q'metaclass::ctors';$Ai={$k1,1};$Bi={$k1,1,$l1,1,$m1,1,$n1,1,$o1,1,$p1,1,$q1,1,$r1,1};$Ci={};$Di=q'into';$Ei=[];$Fi=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Gi=bless({$V1,$Ei,$E1,$Fi,$G1,$H1},$e1);$Hi={$Di,$Gi};$Ii=q'unix/io_stream.b';$Ji=bless({$c,$Ci,$u1,$v1,$w1,$v1,$x1,$Hi,$f,$Ii},$R);$Ki=q'lib/slice::ctors';$Li={};$Mi=q'(+';$Ni=[];$Oi=q'ni(\'ni:unix/cat\')->new(@_[0, 1])';$Pi=bless({$V1,$Ni,$E1,$Oi,$G1,$H1},$e1);$Qi={$Mi,$Pi};$Ri=q'unix/io_constructors.b';$Si=bless({$c,$Li,$u1,$v1,$w1,$v1,$x1,$Qi,$f,$Ri},$R);$Ti=q'lib/slice::ctors';$Ui={};$Vi=q'(<>';$Wi=[];$Xi=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Yi=bless({$V1,$Wi,$E1,$Xi,$G1,$H1},$e1);$Zi=q'(@{}';$cj=[];$dj=q'my $self = shift; [<$self>]';$ej=bless({$V1,$cj,$E1,$dj,$G1,$H1},$e1);$fj={$Vi,$Yi,$Zi,$ej};$gj=q'unix/io_readers.b';$hj=bless({$c,$Ui,$u1,$v1,$w1,$v1,$x1,$fj,$f,$gj},$R);$ij=q'lib/slice::ctors';$jj=[$T2,$Ji,$Si,$hj];$kj=bless({$c,$Bi,$f,$o1,$g,$jj},$A);$lj=q'unix/io.c::ctors';$mj={};$nj=[];$oj=q'shift; +{fs => [@_]}';$pj=bless({$V1,$nj,$E1,$oj,$G1,$H1},$e1);$qj={$N1,$pj};$rj=q'unix/cat_init.b';$sj=bless({$c,$mj,$u1,$v1,$w1,$v1,$x1,$qj,$f,$rj},$R);$tj=q'lib/slice::ctors';$uj={};$vj=q'read';$wj=[];$xj=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$yj=bless({$V1,$wj,$E1,$xj,$G1,$H1},$e1);$zj={$vj,$yj};$Aj=q'unix/cat_read.b';$Bj=bless({$c,$uj,$u1,$v1,$w1,$v1,$x1,$zj,$f,$Aj},$R);$Cj=q'lib/slice::ctors';$Dj=[$kj,$sj,$Bj];$Ej=bless({$c,$Ai,$f,$k1,$g,$Dj},$w);$Fj=q'unix/cat.c::ctors';$Gj=q'ni:unix/cat.c';$Hj=q'ni:unix/cat_init.b';$Ij=q'ni:unix/cat_read.b';$Jj=q'ni:unix/fd';$Kj={$x,1};$Lj=[$vi];$Mj=bless({$c,$Kj,$f,$x,$g,$Lj},$G);$Nj=q'metaclass::ctors';$Oj={$l1,1};$Pj={};$Qj=q'fd';$Rj=[];$Sj=q'shift->{\'fd\'}';$Tj=bless({$V1,$Rj,$E1,$Sj,$G1,$H1},$e1);$Uj={$Qj,$Tj};$Vj=q'unix/fd_readers.b';$Wj=bless({$c,$Pj,$u1,$v1,$w1,$v1,$x1,$Uj,$f,$Vj},$R);$Xj=q'lib/slice::ctors';$Yj={};$Zj=[];$ck=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$dk=bless({$V1,$Zj,$E1,$ck,$G1,$H1},$e1);$ek={$N1,$dk};$fk=q'unix/fd_init.b';$gk=bless({$c,$Yj,$u1,$v1,$w1,$v1,$x1,$ek,$f,$fk},$R);$hk=q'lib/slice::ctors';$ik={};$jk=q'move_to';$kk=[];$lk=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$mk=bless({$V1,$kk,$E1,$lk,$G1,$H1},$e1);$nk={$jk,$mk};$ok=q'unix/fd_shell.b';$pk=bless({$c,$ik,$u1,$v1,$w1,$v1,$x1,$nk,$f,$ok},$R);$qk=q'lib/slice::ctors';$rk={$l1,1,$m1,1,$n1,1,$p1,1,$q1,1};$sk=q'unix/has_fd.b';$tk={};$uk=[];$vk=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$wk=bless({$V1,$uk,$E1,$vk,$G1,$H1},$e1);$xk=[];$yk=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$zk=bless({$V1,$xk,$E1,$yk,$G1,$H1},$e1);$Ak={$vj,$wk,$ce,$zk};$Bk=q'unix/fd_safeio.b';$Ck=bless({$c,$tk,$u1,$v1,$w1,$v1,$x1,$Ak,$f,$Bk},$R);$Dk=q'lib/slice::ctors';$Ek=[$Ck];$Fk=bless({$c,$rk,$f,$sk,$g,$Ek},$V);$Gk=q'lib/branch::ctors';$Hk={};$Ik=q'read_fh';$Jk=[];$Kk=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$Lk=bless({$V1,$Jk,$E1,$Kk,$G1,$H1},$e1);$Mk=q'write_fh';$Nk=[];$Ok=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$Pk=bless({$V1,$Nk,$E1,$Ok,$G1,$H1},$e1);$Qk={$Ik,$Lk,$Mk,$Pk};$Rk=q'unix/fd_io.b';$Sk=bless({$c,$Hk,$u1,$v1,$w1,$v1,$x1,$Qk,$f,$Rk},$R);$Tk=q'lib/slice::ctors';$Uk=[$kj,$Wj,$gk,$pk,$Fk,$Sk];$Vk=bless({$c,$Oj,$f,$l1,$g,$Uk},$x);$Wk=q'unix/fd.c::ctors';$Xk=q'ni:unix/fd.c';$Yk=q'ni:unix/fd_init.b';$Zk=q'ni:unix/fd_io.b';$cl=q'ni:unix/fd_readers.b';$dl=q'ni:unix/fd_safeio.b';$el=q'ni:unix/fd_shell.b';$fl=q'ni:unix/fifo';$gl={$y,1};$hl=[$vi];$il=bless({$c,$gl,$f,$y,$g,$hl},$G);$jl=q'metaclass::ctors';$kl={$m1,1};$ll={};$ml=[];$nl=q'shift->{\'read_fh\'}';$ol=bless({$V1,$ml,$E1,$nl,$G1,$H1},$e1);$pl=[];$ql=q'shift->{\'write_fh\'}';$rl=bless({$V1,$pl,$E1,$ql,$G1,$H1},$e1);$sl={$Ik,$ol,$Mk,$rl};$tl=q'unix/fifo_io.b';$ul=bless({$c,$ll,$u1,$v1,$w1,$v1,$x1,$sl,$f,$tl},$R);$vl=q'lib/slice::ctors';$wl={};$xl=[];$yl=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$zl=bless({$V1,$xl,$E1,$yl,$G1,$H1},$e1);$Al={$N1,$zl};$Bl=q'unix/fifo_init.b';$Cl=bless({$c,$wl,$u1,$v1,$w1,$v1,$x1,$Al,$f,$Bl},$R);$Dl=q'lib/slice::ctors';$El={};$Fl=q'read_side';$Gl=[];$Hl=q'my $self = shift; close $$self{write_fh}; $self';$Il=bless({$V1,$Gl,$E1,$Hl,$G1,$H1},$e1);$Jl=q'write_side';$Kl=[];$Ll=q'my $self = shift; close $$self{read_fh};  $self';$Ml=bless({$V1,$Kl,$E1,$Ll,$G1,$H1},$e1);$Nl={$Fl,$Il,$Jl,$Ml};$Ol=q'unix/fifo_direction.b';$Pl=bless({$c,$El,$u1,$v1,$w1,$v1,$x1,$Nl,$f,$Ol},$R);$Ql=q'lib/slice::ctors';$Rl=[$kj,$ul,$Cl,$Fk,$Pl];$Sl=bless({$c,$kl,$f,$m1,$g,$Rl},$y);$Tl=q'unix/fifo.c::ctors';$Ul=q'ni:unix/fifo.c';$Vl=q'ni:unix/fifo_direction.b';$Wl=q'ni:unix/fifo_init.b';$Xl=q'ni:unix/fifo_io.b';$Yl=q'ni:unix/file';$Zl={$z,1};$cm=[$vi];$dm=bless({$c,$Zl,$f,$z,$g,$cm},$G);$em=q'metaclass::ctors';$fm={$n1,1};$gm={};$hm=[];$im=q'shift->{\'name\'}';$jm=bless({$V1,$hm,$E1,$im,$G1,$H1},$e1);$km={$f,$jm};$lm=q'unix/file_readers.b';$mm=bless({$c,$gm,$u1,$v1,$w1,$v1,$x1,$km,$f,$lm},$R);$nm=q'lib/slice::ctors';$om={};$pm=[];$qm=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$rm=bless({$V1,$pm,$E1,$qm,$G1,$H1},$e1);$sm={$N1,$rm};$tm=q'unix/file_init.b';$um=bless({$c,$om,$u1,$v1,$w1,$v1,$x1,$sm,$f,$tm},$R);$vm=q'lib/slice::ctors';$wm={};$xm=[];$ym=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$zm=bless({$V1,$xm,$E1,$ym,$G1,$H1},$e1);$Am=[];$Bm=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Cm=bless({$V1,$Am,$E1,$Bm,$G1,$H1},$e1);$Dm={$Ik,$zm,$Mk,$Cm};$Em=q'unix/file_io.b';$Fm=bless({$c,$wm,$u1,$v1,$w1,$v1,$x1,$Dm,$f,$Em},$R);$Gm=q'lib/slice::ctors';$Hm=[$kj,$mm,$um,$Fk,$Fm];$Im=bless({$c,$fm,$f,$n1,$g,$Hm},$z);$Jm=q'unix/file.c::ctors';$Km=q'ni:unix/file.c';$Lm=q'ni:unix/file_init.b';$Mm=q'ni:unix/file_io.b';$Nm=q'ni:unix/file_readers.b';$Om=q'ni:unix/has_fd.b';$Pm=q'ni:unix/io';$Qm=q'ni:unix/io.c';$Rm=q'ni:unix/io_constructors.b';$Sm=q'ni:unix/io_readers.b';$Tm=q'ni:unix/io_stream.b';$Um=q'ni:unix/pid';$Vm={$B,1};$Wm=[$vi];$Xm=bless({$c,$Vm,$f,$B,$g,$Wm},$G);$Ym=q'metaclass::ctors';$Zm={$p1,1};$cn={};$dn=q'pid';$en=[];$fn=q'shift->{\'pid\'}';$gn=bless({$V1,$en,$E1,$fn,$G1,$H1},$e1);$hn=q'stderr';$in=[];$jn=q'shift->{\'stderr\'}';$kn=bless({$V1,$in,$E1,$jn,$G1,$H1},$e1);$ln=q'stdin';$mn=[];$nn=q'shift->{\'stdin\'}';$on=bless({$V1,$mn,$E1,$nn,$G1,$H1},$e1);$pn=q'stdout';$qn=[];$rn=q'shift->{\'stdout\'}';$sn=bless({$V1,$qn,$E1,$rn,$G1,$H1},$e1);$tn={$dn,$gn,$hn,$kn,$ln,$on,$pn,$sn};$un=q'unix/pid_readers.b';$vn=bless({$c,$cn,$u1,$v1,$w1,$v1,$x1,$tn,$f,$un},$R);$wn=q'lib/slice::ctors';$xn={};$yn=[];$zn=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$An=bless({$V1,$yn,$E1,$zn,$G1,$H1},$e1);$Bn={$N1,$An};$Cn=q'unix/pid_init.b';$Dn=bless({$c,$xn,$u1,$v1,$w1,$v1,$x1,$Bn,$f,$Cn},$R);$En=q'lib/slice::ctors';$Fn={};$Gn={};$Hn=q'unix/pid_wait.b';$In=bless({$c,$Fn,$u1,$v1,$w1,$v1,$x1,$Gn,$f,$Hn},$R);$Jn=q'lib/slice::ctors';$Kn={};$Ln=[];$Mn=q'shift->{stdout}->read_fh';$Nn=bless({$V1,$Ln,$E1,$Mn,$G1,$H1},$e1);$On=[];$Pn=q'shift->{stdin}->write_fh';$Qn=bless({$V1,$On,$E1,$Pn,$G1,$H1},$e1);$Rn={$Ik,$Nn,$Mk,$Qn};$Sn=q'unix/pid_io.b';$Tn=bless({$c,$Kn,$u1,$v1,$w1,$v1,$x1,$Rn,$f,$Sn},$R);$Un=q'lib/slice::ctors';$Vn=[$kj,$vn,$Dn,$In,$Fk,$Tn];$Wn=bless({$c,$Zm,$f,$p1,$g,$Vn},$B);$Xn=q'unix/pid.c::ctors';$Yn=q'ni:unix/pid.c';$Zn=q'ni:unix/pid_init.b';$co=q'ni:unix/pid_io.b';$do=q'ni:unix/pid_readers.b';$eo=q'ni:unix/pid_wait.b';$fo=q'ni:unix/pipeline';$go={$C,1};$ho=[$vi];$io=bless({$c,$go,$f,$C,$g,$ho},$G);$jo=q'metaclass::ctors';$ko={$q1,1};$lo={};$mo=[];$no=q'shift->{\'stdin\'}';$oo=bless({$V1,$mo,$E1,$no,$G1,$H1},$e1);$po=[];$qo=q'shift->{\'stdout\'}';$ro=bless({$V1,$po,$E1,$qo,$G1,$H1},$e1);$so={$ln,$oo,$pn,$ro};$to=q'unix/pipeline_ro.b';$uo=bless({$c,$lo,$u1,$v1,$w1,$v1,$x1,$so,$f,$to},$R);$vo=q'lib/slice::ctors';$wo={};$xo=[];$yo=q'my $class  = shift;
my $stdin  = ni(\'ni:unix/fifo\')->new;
my $stdout = ni(\'ni:unix/fifo\')->new;
# TODO: stderr and multiplexing, which probably happens here

my @rs = ($stdin, @_);
my @ws = (@_, $stdout);
my $rv; vec($rv, fileno $_->read_fh,  1) = 1 for @rs;
my $wv; vec($wv, fileno $_->write_fh, 1) = 1 for @ws;

+{ps     => [@_],
  stdin  => $stdin,
  stdout => $stdout,
  rs => \\@rs, rv => $rv,
  ws => \\@ws, wv => $wv};';$zo=bless({$V1,$xo,$E1,$yo,$G1,$H1},$e1);$Ao={$N1,$zo};$Bo=q'unix/pipeline_init.b';$Co=bless({$c,$wo,$u1,$v1,$w1,$v1,$x1,$Ao,$f,$Bo},$R);$Do=q'lib/slice::ctors';$Eo={};$Fo=q'async_step';$Go=[];$Ho=q'local $_;
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
$self;';$Io=bless({$V1,$Go,$E1,$Ho,$G1,$H1},$e1);$Jo={$Fo,$Io};$Ko=q'unix/pipeline_async.b';$Lo=bless({$c,$Eo,$u1,$v1,$w1,$v1,$x1,$Jo,$f,$Ko},$R);$Mo=q'lib/slice::ctors';$No={};$Oo=[];$Po=q'shift->{stdout}->read_fh';$Qo=bless({$V1,$Oo,$E1,$Po,$G1,$H1},$e1);$Ro=[];$So=q'shift->{stdin}->write_fh';$To=bless({$V1,$Ro,$E1,$So,$G1,$H1},$e1);$Uo={$Ik,$Qo,$Mk,$To};$Vo=q'unix/pipeline_io.b';$Wo=bless({$c,$No,$u1,$v1,$w1,$v1,$x1,$Uo,$f,$Vo},$R);$Xo=q'lib/slice::ctors';$Yo=[$kj,$uo,$Co,$Lo,$Fk,$Wo];$Zo=bless({$c,$ko,$f,$q1,$g,$Yo},$C);$cp=q'unix/pipeline.c::ctors';$dp=q'ni:unix/pipeline.c';$ep=q'ni:unix/pipeline_async.b';$fp=q'ni:unix/pipeline_init.b';$gp=q'ni:unix/pipeline_io.b';$hp=q'ni:unix/pipeline_ro.b';$ip=q'ni:unix/str';$jp={$D,1};$kp=[$vi];$lp=bless({$c,$jp,$f,$D,$g,$kp},$G);$mp=q'metaclass::ctors';$np={$r1,1};$op={};$pp=[];$qp=q'shift->{\'data\'}';$rp=bless({$V1,$pp,$E1,$qp,$G1,$H1},$e1);$sp=q'end';$tp=[];$up=q'shift->{\'end\'}';$vp=bless({$V1,$tp,$E1,$up,$G1,$H1},$e1);$wp=q'start';$xp=[];$yp=q'shift->{\'start\'}';$zp=bless({$V1,$xp,$E1,$yp,$G1,$H1},$e1);$Ap={$Rh,$rp,$sp,$vp,$wp,$zp};$Bp=q'unix/str_ro.b';$Cp=bless({$c,$op,$u1,$v1,$w1,$v1,$x1,$Ap,$f,$Bp},$R);$Dp=q'lib/slice::ctors';$Ep={};$Fp=[];$Gp=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Hp=bless({$V1,$Fp,$E1,$Gp,$G1,$H1},$e1);$Ip={$N1,$Hp};$Jp=q'unix/str_init.b';$Kp=bless({$c,$Ep,$u1,$v1,$w1,$v1,$x1,$Ip,$f,$Jp},$R);$Lp=q'lib/slice::ctors';$Mp={};$Np=[];$Op=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = @_ >= 2 ? ni::min($self->remaining, $_[1]) : $self->remaining;
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Pp=bless({$V1,$Np,$E1,$Op,$G1,$H1},$e1);$Qp=q'remaining';$Rp=[];$Sp=q'my $self = shift; $$self{end} - $$self{start}';$Tp=bless({$V1,$Rp,$E1,$Sp,$G1,$H1},$e1);$Up=[];$Vp=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Wp=bless({$V1,$Up,$E1,$Vp,$G1,$H1},$e1);$Xp={$vj,$Pp,$Qp,$Tp,$ce,$Wp};$Yp=q'unix/str_io.b';$Zp=bless({$c,$Mp,$u1,$v1,$w1,$v1,$x1,$Xp,$f,$Yp},$R);$cq=q'lib/slice::ctors';$dq=[$kj,$Cp,$Kp,$Zp];$eq=bless({$c,$np,$f,$r1,$g,$dq},$D);$fq=q'unix/str.c::ctors';$gq=q'ni:unix/str.c';$hq=q'ni:unix/str_init.b';$iq=q'ni:unix/str_io.b';$jq=q'ni:unix/str_ro.b';$kq={$u9,$Aa,$Ca,$Qa,$Ra,$db,$eb,$o9,$fb,$A7,$gb,$q4,$hb,$G6,$ib,$h3,$jb,$M,$kb,$j6,$lb,$d5,$mb,$W5,$nb,$g6,$ob,$R5,$pb,$Kb,$Mb,$sb,$Nb,$Hb,$Ob,$Ab,$Pb,$l7,$Qb,$u6,$Rb,$h7,$Sb,$ka,$Tb,$x9,$Ub,$U9,$Vb,$F9,$Wb,$M9,$Xb,$ha,$Yb,$d3,$Zb,$J2,$cc,$A1,$dc,$S1,$ec,$x2,$fc,$k2,$gc,$G2,$hc,$qc,$tc,$le,$ne,$wc,$oe,$Fc,$pe,$ie,$qe,$P2,$re,$j4,$se,$Qe,$Te,$O6,$Ue,$q3,$Ve,$m5,$We,$v5,$Xe,$r9,$Ye,$L7,$Ze,$o8,$cf,$N8,$df,$Z8,$ef,$Z7,$ff,$hg,$jg,$I5,$kg,$W6,$lg,$E5,$mg,$W3,$ng,$E3,$og,$P,$pg,$L3,$qg,$T3,$rg,$Dg,$Fg,$w7,$Gg,$Q4,$Hg,$G4,$Ig,$y4,$Jg,$N4,$Kg,$zh,$Bh,$Pg,$Ch,$nh,$Dh,$Yg,$Eh,$vh,$Fh,$Ih,$Kh,$H7,$Lh,$D7,$Mh,$o7,$Nh,$n4,$Oh,$oi,$pi,$T2,$qi,$H,$ri,$Ej,$Gj,$yi,$Hj,$sj,$Ij,$Bj,$Jj,$Vk,$Xk,$Mj,$Yk,$gk,$Zk,$Sk,$cl,$Wj,$dl,$Ck,$el,$pk,$fl,$Sl,$Ul,$il,$Vl,$Pl,$Wl,$Cl,$Xl,$ul,$Yl,$Im,$Km,$dm,$Lm,$um,$Mm,$Fm,$Nm,$mm,$Om,$Fk,$Pm,$kj,$Qm,$vi,$Rm,$Si,$Sm,$hj,$Tm,$Ji,$Um,$Wn,$Yn,$Xm,$Zn,$Dn,$co,$Tn,$do,$vn,$eo,$In,$fo,$Zo,$dp,$io,$ep,$Lo,$fp,$Co,$gp,$Wo,$hp,$uo,$ip,$eq,$gq,$lp,$hq,$Kp,$iq,$Zp,$jq,$Cp};$lq=q'resolvers';$mq=[];$nq=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:unix/fd\')->new($f);';$oq=bless({$V1,$mq,$E1,$nq,$G1,$H1},$e1);$pq=q'file';$qq=[];$rq=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:unix/file\')->new($f);';$sq=bless({$V1,$qq,$E1,$rq,$G1,$H1},$e1);$tq=q'str';$uq=[];$vq=q'my $s = shift;
ni(\'ni:unix/str\')->new(substr($s, 4) . "\\n");';$wq=bless({$V1,$uq,$E1,$vq,$G1,$H1},$e1);$xq={$Qj,$oq,$pq,$sq,$tq,$wq};$yq=bless({$t9,$kq,$lq,$xq},$g1);$zq=q'lib/ni::ctors';$$F[0]=$A7;$$I2[0]=$T2;$$I2[1]=$j4;$$i6[4]=$l7;*$y3=\&$w3;*$x3=\&$u3;$S1->apply_unsafe($e1);$k2->apply_unsafe($e1);$x2->apply_unsafe($e1);$G2->apply_unsafe($e1);$P2->apply_unsafe($T);$P2->apply_unsafe($h);$P2->apply_unsafe($U);$P2->apply_unsafe($l);$P2->apply_unsafe($V);$P2->apply_unsafe($m);$P2->apply_unsafe($W);$P2->apply_unsafe($n);$P2->apply_unsafe($d1);$P2->apply_unsafe($o);$P2->apply_unsafe($e1);$P2->apply_unsafe($p);$P2->apply_unsafe($f1);$P2->apply_unsafe($q);$P2->apply_unsafe($g1);$P2->apply_unsafe($r);$P2->apply_unsafe($R);$P2->apply_unsafe($s);$P2->apply_unsafe($X);$P2->apply_unsafe($t);$P2->apply_unsafe($h1);$P2->apply_unsafe($Q2);$P2->apply_unsafe($G);$P2->apply_unsafe($d);$P2->apply_unsafe($Z);$P2->apply_unsafe($j);$P2->apply_unsafe($j1);$P2->apply_unsafe($v);$P2->apply_unsafe($k1);$P2->apply_unsafe($w);$P2->apply_unsafe($l1);$P2->apply_unsafe($x);$P2->apply_unsafe($m1);$P2->apply_unsafe($y);$P2->apply_unsafe($n1);$P2->apply_unsafe($z);$P2->apply_unsafe($o1);$P2->apply_unsafe($A);$P2->apply_unsafe($p1);$P2->apply_unsafe($B);$P2->apply_unsafe($q1);$P2->apply_unsafe($C);$P2->apply_unsafe($r1);$P2->apply_unsafe($D);$d3->apply_unsafe($T);$d3->apply_unsafe($h);$d3->apply_unsafe($U);$d3->apply_unsafe($l);$d3->apply_unsafe($V);$d3->apply_unsafe($m);$d3->apply_unsafe($W);$d3->apply_unsafe($n);$d3->apply_unsafe($o);$d3->apply_unsafe($p);$d3->apply_unsafe($q);$d3->apply_unsafe($r);$d3->apply_unsafe($R);$d3->apply_unsafe($s);$d3->apply_unsafe($X);$d3->apply_unsafe($t);$d3->apply_unsafe($e3);$d3->apply_unsafe($G);$d3->apply_unsafe($d);$d3->apply_unsafe($Z);$d3->apply_unsafe($j);$d3->apply_unsafe($v);$d3->apply_unsafe($w);$d3->apply_unsafe($x);$d3->apply_unsafe($y);$d3->apply_unsafe($z);$d3->apply_unsafe($A);$d3->apply_unsafe($B);$d3->apply_unsafe($C);$d3->apply_unsafe($D);$q3->apply_unsafe($T);$q3->apply_unsafe($h);$q3->apply_unsafe($l);$q3->apply_unsafe($V);$q3->apply_unsafe($m);$q3->apply_unsafe($n);$q3->apply_unsafe($d1);$q3->apply_unsafe($o);$q3->apply_unsafe($p);$q3->apply_unsafe($q);$q3->apply_unsafe($r);$q3->apply_unsafe($R);$q3->apply_unsafe($s);$q3->apply_unsafe($X);$q3->apply_unsafe($t);$q3->apply_unsafe($r3);$q3->apply_unsafe($G);$q3->apply_unsafe($d);$q3->apply_unsafe($Z);$q3->apply_unsafe($j);$q3->apply_unsafe($v);$q3->apply_unsafe($w);$q3->apply_unsafe($x);$q3->apply_unsafe($y);$q3->apply_unsafe($z);$q3->apply_unsafe($A);$q3->apply_unsafe($B);$q3->apply_unsafe($C);$q3->apply_unsafe($D);$E3->apply_unsafe($R);$L3->apply_unsafe($R);$T3->apply_unsafe($R);$j4->apply_unsafe($T);$j4->apply_unsafe($h);$j4->apply_unsafe($l);$j4->apply_unsafe($m);$j4->apply_unsafe($n);$j4->apply_unsafe($o);$j4->apply_unsafe($e1);$j4->apply_unsafe($p);$j4->apply_unsafe($q);$j4->apply_unsafe($r);$j4->apply_unsafe($R);$j4->apply_unsafe($s);$j4->apply_unsafe($X);$j4->apply_unsafe($t);$j4->apply_unsafe($k4);$j4->apply_unsafe($G);$j4->apply_unsafe($d);$j4->apply_unsafe($j);$j4->apply_unsafe($v);$j4->apply_unsafe($w);$j4->apply_unsafe($x);$j4->apply_unsafe($y);$j4->apply_unsafe($z);$j4->apply_unsafe($A);$j4->apply_unsafe($B);$j4->apply_unsafe($C);$j4->apply_unsafe($D);$G4->apply_unsafe($X);$N4->apply_unsafe($X);$d5->apply_unsafe($T);$d5->apply_unsafe($h);$d5->apply_unsafe($l);$d5->apply_unsafe($V);$d5->apply_unsafe($m);$d5->apply_unsafe($n);$d5->apply_unsafe($o);$d5->apply_unsafe($p);$d5->apply_unsafe($q);$d5->apply_unsafe($r);$d5->apply_unsafe($s);$d5->apply_unsafe($t);$d5->apply_unsafe($e5);$d5->apply_unsafe($G);$d5->apply_unsafe($d);$d5->apply_unsafe($Z);$d5->apply_unsafe($j);$d5->apply_unsafe($v);$d5->apply_unsafe($w);$d5->apply_unsafe($x);$d5->apply_unsafe($y);$d5->apply_unsafe($z);$d5->apply_unsafe($A);$d5->apply_unsafe($B);$d5->apply_unsafe($C);$d5->apply_unsafe($D);$m5->apply_unsafe($T);$m5->apply_unsafe($h);$m5->apply_unsafe($l);$m5->apply_unsafe($V);$m5->apply_unsafe($m);$m5->apply_unsafe($n);$m5->apply_unsafe($o);$m5->apply_unsafe($p);$m5->apply_unsafe($q);$m5->apply_unsafe($r);$m5->apply_unsafe($R);$m5->apply_unsafe($s);$m5->apply_unsafe($X);$m5->apply_unsafe($t);$m5->apply_unsafe($n5);$m5->apply_unsafe($G);$m5->apply_unsafe($d);$m5->apply_unsafe($Z);$m5->apply_unsafe($j);$m5->apply_unsafe($v);$m5->apply_unsafe($w);$m5->apply_unsafe($x);$m5->apply_unsafe($y);$m5->apply_unsafe($z);$m5->apply_unsafe($A);$m5->apply_unsafe($B);$m5->apply_unsafe($C);$m5->apply_unsafe($D);$v5->apply_unsafe($T);$v5->apply_unsafe($h);$v5->apply_unsafe($l);$v5->apply_unsafe($V);$v5->apply_unsafe($m);$v5->apply_unsafe($n);$v5->apply_unsafe($o);$v5->apply_unsafe($p);$v5->apply_unsafe($q);$v5->apply_unsafe($r);$v5->apply_unsafe($R);$v5->apply_unsafe($s);$v5->apply_unsafe($X);$v5->apply_unsafe($t);$v5->apply_unsafe($w5);$v5->apply_unsafe($G);$v5->apply_unsafe($d);$v5->apply_unsafe($Z);$v5->apply_unsafe($j);$v5->apply_unsafe($v);$v5->apply_unsafe($w);$v5->apply_unsafe($x);$v5->apply_unsafe($y);$v5->apply_unsafe($z);$v5->apply_unsafe($A);$v5->apply_unsafe($B);$v5->apply_unsafe($C);$v5->apply_unsafe($D);$E5->apply_unsafe($T);$E5->apply_unsafe($h);$E5->apply_unsafe($l);$E5->apply_unsafe($V);$E5->apply_unsafe($m);$E5->apply_unsafe($n);$E5->apply_unsafe($o);$E5->apply_unsafe($p);$E5->apply_unsafe($q);$E5->apply_unsafe($r);$E5->apply_unsafe($s);$E5->apply_unsafe($X);$E5->apply_unsafe($t);$E5->apply_unsafe($F5);$E5->apply_unsafe($G);$E5->apply_unsafe($d);$E5->apply_unsafe($Z);$E5->apply_unsafe($j);$E5->apply_unsafe($v);$E5->apply_unsafe($w);$E5->apply_unsafe($x);$E5->apply_unsafe($y);$E5->apply_unsafe($z);$E5->apply_unsafe($A);$E5->apply_unsafe($B);$E5->apply_unsafe($C);$E5->apply_unsafe($D);$R5->apply_unsafe($T);$R5->apply_unsafe($h);$R5->apply_unsafe($l);$R5->apply_unsafe($m);$R5->apply_unsafe($n);$R5->apply_unsafe($o);$R5->apply_unsafe($p);$R5->apply_unsafe($q);$R5->apply_unsafe($r);$R5->apply_unsafe($s);$R5->apply_unsafe($t);$R5->apply_unsafe($S5);$R5->apply_unsafe($G);$R5->apply_unsafe($d);$R5->apply_unsafe($Z);$R5->apply_unsafe($j);$R5->apply_unsafe($v);$R5->apply_unsafe($w);$R5->apply_unsafe($x);$R5->apply_unsafe($y);$R5->apply_unsafe($z);$R5->apply_unsafe($A);$R5->apply_unsafe($B);$R5->apply_unsafe($C);$R5->apply_unsafe($D);$g6->apply_unsafe($V);$u6->apply_unsafe($T);$u6->apply_unsafe($h);$u6->apply_unsafe($l);$u6->apply_unsafe($V);$u6->apply_unsafe($m);$u6->apply_unsafe($n);$u6->apply_unsafe($o);$u6->apply_unsafe($p);$u6->apply_unsafe($q);$u6->apply_unsafe($r);$u6->apply_unsafe($s);$u6->apply_unsafe($t);$u6->apply_unsafe($v6);$u6->apply_unsafe($G);$u6->apply_unsafe($d);$u6->apply_unsafe($Z);$u6->apply_unsafe($j);$u6->apply_unsafe($v);$u6->apply_unsafe($w);$u6->apply_unsafe($x);$u6->apply_unsafe($y);$u6->apply_unsafe($z);$u6->apply_unsafe($A);$u6->apply_unsafe($B);$u6->apply_unsafe($C);$u6->apply_unsafe($D);$G6->apply_unsafe($T);$G6->apply_unsafe($h);$G6->apply_unsafe($l);$G6->apply_unsafe($V);$G6->apply_unsafe($m);$G6->apply_unsafe($n);$G6->apply_unsafe($o);$G6->apply_unsafe($p);$G6->apply_unsafe($q);$G6->apply_unsafe($r);$G6->apply_unsafe($s);$G6->apply_unsafe($t);$G6->apply_unsafe($H6);$G6->apply_unsafe($G);$G6->apply_unsafe($d);$G6->apply_unsafe($Z);$G6->apply_unsafe($j);$G6->apply_unsafe($v);$G6->apply_unsafe($w);$G6->apply_unsafe($x);$G6->apply_unsafe($y);$G6->apply_unsafe($z);$G6->apply_unsafe($A);$G6->apply_unsafe($B);$G6->apply_unsafe($C);$G6->apply_unsafe($D);$O6->apply_unsafe($T);$O6->apply_unsafe($h);$O6->apply_unsafe($l);$O6->apply_unsafe($V);$O6->apply_unsafe($m);$O6->apply_unsafe($n);$O6->apply_unsafe($o);$O6->apply_unsafe($p);$O6->apply_unsafe($q);$O6->apply_unsafe($r);$O6->apply_unsafe($s);$O6->apply_unsafe($t);$O6->apply_unsafe($P6);$O6->apply_unsafe($G);$O6->apply_unsafe($d);$O6->apply_unsafe($Z);$O6->apply_unsafe($j);$O6->apply_unsafe($v);$O6->apply_unsafe($w);$O6->apply_unsafe($x);$O6->apply_unsafe($y);$O6->apply_unsafe($z);$O6->apply_unsafe($A);$O6->apply_unsafe($B);$O6->apply_unsafe($C);$O6->apply_unsafe($D);$W6->apply_unsafe($T);$W6->apply_unsafe($h);$W6->apply_unsafe($l);$W6->apply_unsafe($V);$W6->apply_unsafe($m);$W6->apply_unsafe($n);$W6->apply_unsafe($o);$W6->apply_unsafe($p);$W6->apply_unsafe($q);$W6->apply_unsafe($r);$W6->apply_unsafe($s);$W6->apply_unsafe($t);$W6->apply_unsafe($X6);$W6->apply_unsafe($G);$W6->apply_unsafe($d);$W6->apply_unsafe($Z);$W6->apply_unsafe($j);$W6->apply_unsafe($v);$W6->apply_unsafe($w);$W6->apply_unsafe($x);$W6->apply_unsafe($y);$W6->apply_unsafe($z);$W6->apply_unsafe($A);$W6->apply_unsafe($B);$W6->apply_unsafe($C);$W6->apply_unsafe($D);$h7->apply_unsafe($T);$h7->apply_unsafe($h);$h7->apply_unsafe($l);$h7->apply_unsafe($V);$h7->apply_unsafe($m);$h7->apply_unsafe($n);$h7->apply_unsafe($o);$h7->apply_unsafe($p);$h7->apply_unsafe($q);$h7->apply_unsafe($r);$h7->apply_unsafe($s);$h7->apply_unsafe($t);$h7->apply_unsafe($i7);$h7->apply_unsafe($G);$h7->apply_unsafe($d);$h7->apply_unsafe($Z);$h7->apply_unsafe($j);$h7->apply_unsafe($v);$h7->apply_unsafe($w);$h7->apply_unsafe($x);$h7->apply_unsafe($y);$h7->apply_unsafe($z);$h7->apply_unsafe($A);$h7->apply_unsafe($B);$h7->apply_unsafe($C);$h7->apply_unsafe($D);$w7->apply_unsafe($T);$w7->apply_unsafe($h);$w7->apply_unsafe($l);$w7->apply_unsafe($m);$w7->apply_unsafe($n);$w7->apply_unsafe($o);$w7->apply_unsafe($p);$w7->apply_unsafe($q);$w7->apply_unsafe($r);$w7->apply_unsafe($s);$w7->apply_unsafe($t);$w7->apply_unsafe($x7);$w7->apply_unsafe($d);$w7->apply_unsafe($j);$w7->apply_unsafe($v);$w7->apply_unsafe($w);$w7->apply_unsafe($x);$w7->apply_unsafe($y);$w7->apply_unsafe($z);$w7->apply_unsafe($A);$w7->apply_unsafe($B);$w7->apply_unsafe($C);$w7->apply_unsafe($D);$Z7->apply_unsafe($g1);$o8->apply_unsafe($g1);$N8->apply_unsafe($g1);$Z8->apply_unsafe($g1);$o9->apply_unsafe($g1);$F9->apply_unsafe($d1);$M9->apply_unsafe($d1);$U9->apply_unsafe($d1);$ha->apply_unsafe($d1);$Ab->apply_unsafe($W);$Hb->apply_unsafe($W);$qc->apply_unsafe($rc);$Fc->apply_unsafe($f1);$ie->apply_unsafe($f1);$Qe->apply_unsafe($Re);$hg->apply_unsafe($Re);$Dg->apply_unsafe($rc);$Yg->apply_unsafe($h1);$nh->apply_unsafe($h1);$vh->apply_unsafe($h1);$Ji->apply_unsafe($k1);$Ji->apply_unsafe($l1);$Ji->apply_unsafe($m1);$Ji->apply_unsafe($n1);$Ji->apply_unsafe($o1);$Ji->apply_unsafe($p1);$Ji->apply_unsafe($q1);$Ji->apply_unsafe($r1);$Si->apply_unsafe($k1);$Si->apply_unsafe($l1);$Si->apply_unsafe($m1);$Si->apply_unsafe($n1);$Si->apply_unsafe($o1);$Si->apply_unsafe($p1);$Si->apply_unsafe($q1);$Si->apply_unsafe($r1);$hj->apply_unsafe($k1);$hj->apply_unsafe($l1);$hj->apply_unsafe($m1);$hj->apply_unsafe($n1);$hj->apply_unsafe($o1);$hj->apply_unsafe($p1);$hj->apply_unsafe($q1);$hj->apply_unsafe($r1);$sj->apply_unsafe($k1);$Bj->apply_unsafe($k1);$Wj->apply_unsafe($l1);$gk->apply_unsafe($l1);$pk->apply_unsafe($l1);$Ck->apply_unsafe($l1);$Ck->apply_unsafe($m1);$Ck->apply_unsafe($n1);$Ck->apply_unsafe($p1);$Ck->apply_unsafe($q1);$Sk->apply_unsafe($l1);$ul->apply_unsafe($m1);$Cl->apply_unsafe($m1);$Pl->apply_unsafe($m1);$mm->apply_unsafe($n1);$um->apply_unsafe($n1);$Fm->apply_unsafe($n1);$vn->apply_unsafe($p1);$Dn->apply_unsafe($p1);$In->apply_unsafe($p1);$Tn->apply_unsafe($p1);$uo->apply_unsafe($q1);$Co->apply_unsafe($q1);$Lo->apply_unsafe($q1);$Wo->apply_unsafe($q1);$Cp->apply_unsafe($r1);$Kp->apply_unsafe($r1);$Zp->apply_unsafe($r1);$ni::self=$yq;&$_($H)for@$I;&$_($M)for@$N;&$_($P)for@$Q;&$_($A1)for@$B1;&$_($I1)for@$J1;&$_($M1)for@$J1;&$_($P1)for@$J1;&$_($S1)for@$T1;&$_($Y1)for@$J1;&$_($d2)for@$J1;&$_($h2)for@$J1;&$_($k2)for@$l2;&$_($q2)for@$J1;&$_($u2)for@$J1;&$_($x2)for@$y2;&$_($D2)for@$J1;&$_($G2)for@$H2;&$_($J2)for@$K2;&$_($M2)for@$J1;&$_($P2)for@$R2;&$_($T2)for@$U2;&$_($Y2)for@$J1;&$_($d3)for@$f3;&$_($h3)for@$i3;&$_($l3)for@$J1;&$_($n3)for@$J1;&$_($q3)for@$s3;&$_($u3)for@$J1;&$_($w3)for@$J1;&$_($E3)for@$F3;&$_($I3)for@$J1;&$_($L3)for@$M3;&$_($Q3)for@$J1;&$_($T3)for@$U3;&$_($W3)for@$X3;&$_($d4)for@$J1;&$_($g4)for@$J1;&$_($j4)for@$l4;&$_($n4)for@$o4;&$_($q4)for@$r4;&$_($y4)for@$z4;&$_($D4)for@$J1;&$_($G4)for@$H4;&$_($K4)for@$J1;&$_($N4)for@$O4;&$_($Q4)for@$R4;&$_($W4)for@$J1;&$_($Y4)for@$J1;&$_($d5)for@$f5;&$_($j5)for@$J1;&$_($m5)for@$o5;&$_($s5)for@$J1;&$_($v5)for@$x5;&$_($B5)for@$J1;&$_($E5)for@$G5;&$_($I5)for@$J5;&$_($M5)for@$J1;&$_($O5)for@$J1;&$_($R5)for@$T5;&$_($W5)for@$X5;&$_($d6)for@$J1;&$_($g6)for@$h6;&$_($j6)for@$k6;&$_($r6)for@$J1;&$_($u6)for@$w6;&$_($A6)for@$J1;&$_($D6)for@$J1;&$_($G6)for@$I6;&$_($L6)for@$J1;&$_($O6)for@$Q6;&$_($T6)for@$J1;&$_($W6)for@$Y6;&$_($e7)for@$J1;&$_($h7)for@$j7;&$_($l7)for@$m7;&$_($o7)for@$p7;&$_($t7)for@$J1;&$_($w7)for@$y7;&$_($A7)for@$B7;&$_($D7)for@$E7;&$_($H7)for@$I7;&$_($L7)for@$M7;&$_($S7)for@$J1;&$_($W7)for@$J1;&$_($Z7)for@$c8;&$_($h8)for@$J1;&$_($l8)for@$J1;&$_($o8)for@$p8;&$_($u8)for@$J1;&$_($y8)for@$J1;&$_($C8)for@$J1;&$_($G8)for@$J1;&$_($K8)for@$J1;&$_($N8)for@$O8;&$_($S8)for@$J1;&$_($W8)for@$J1;&$_($Z8)for@$c9;&$_($h9)for@$J1;&$_($l9)for@$J1;&$_($o9)for@$p9;&$_($r9)for@$s9;&$_($x9)for@$y9;&$_($C9)for@$J1;&$_($F9)for@$G9;&$_($J9)for@$J1;&$_($M9)for@$N9;&$_($R9)for@$J1;&$_($U9)for@$V9;&$_($Z9)for@$J1;&$_($ea)for@$J1;&$_($ha)for@$ia;&$_($ka)for@$la;&$_($Aa)for@$Ba;&$_($Ja)for@$J1;&$_($Na)for@$J1;&$_($Qa)for@$Ba;&$_($Ya)for@$J1;&$_($db)for@$Ba;&$_($sb)for@$tb;&$_($xb)for@$J1;&$_($Ab)for@$Bb;&$_($Eb)for@$J1;&$_($Hb)for@$Ib;&$_($Kb)for@$Lb;&$_($nc)for@$J1;&$_($qc)for@$sc;&$_($wc)for@$xc;&$_($Cc)for@$J1;&$_($Fc)for@$Gc;&$_($Lc)for@$J1;&$_($Pc)for@$J1;&$_($Tc)for@$J1;&$_($Xc)for@$J1;&$_($dd)for@$J1;&$_($hd)for@$J1;&$_($ld)for@$J1;&$_($pd)for@$J1;&$_($td)for@$J1;&$_($xd)for@$J1;&$_($Bd)for@$J1;&$_($Fd)for@$J1;&$_($Jd)for@$J1;&$_($Nd)for@$J1;&$_($Rd)for@$J1;&$_($Vd)for@$J1;&$_($Zd)for@$J1;&$_($fe)for@$J1;&$_($ie)for@$je;&$_($le)for@$me;&$_($xe)for@$J1;&$_($Be)for@$J1;&$_($Fe)for@$J1;&$_($Je)for@$J1;&$_($Ne)for@$J1;&$_($Qe)for@$Se;&$_($kf)for@$J1;&$_($of)for@$J1;&$_($sf)for@$J1;&$_($wf)for@$J1;&$_($Af)for@$J1;&$_($Ef)for@$J1;&$_($If)for@$J1;&$_($Mf)for@$J1;&$_($Qf)for@$J1;&$_($Uf)for@$J1;&$_($Yf)for@$J1;&$_($eg)for@$J1;&$_($hg)for@$ig;&$_($vg)for@$J1;&$_($Ag)for@$J1;&$_($Dg)for@$Eg;&$_($Pg)for@$Qg;&$_($Vg)for@$J1;&$_($Yg)for@$Zg;&$_($gh)for@$J1;&$_($kh)for@$J1;&$_($nh)for@$oh;&$_($sh)for@$J1;&$_($vh)for@$wh;&$_($zh)for@$Ah;&$_($Ih)for@$Jh;&$_($li)for@$mi;&$_($oi)for@$Jh;&$_($vi)for@$wi;&$_($yi)for@$zi;&$_($Gi)for@$J1;&$_($Ji)for@$Ki;&$_($Pi)for@$J1;&$_($Si)for@$Ti;&$_($Yi)for@$J1;&$_($ej)for@$J1;&$_($hj)for@$ij;&$_($kj)for@$lj;&$_($pj)for@$J1;&$_($sj)for@$tj;&$_($yj)for@$J1;&$_($Bj)for@$Cj;&$_($Ej)for@$Fj;&$_($Mj)for@$Nj;&$_($Tj)for@$J1;&$_($Wj)for@$Xj;&$_($dk)for@$J1;&$_($gk)for@$hk;&$_($mk)for@$J1;&$_($pk)for@$qk;&$_($wk)for@$J1;&$_($zk)for@$J1;&$_($Ck)for@$Dk;&$_($Fk)for@$Gk;&$_($Lk)for@$J1;&$_($Pk)for@$J1;&$_($Sk)for@$Tk;&$_($Vk)for@$Wk;&$_($il)for@$jl;&$_($ol)for@$J1;&$_($rl)for@$J1;&$_($ul)for@$vl;&$_($zl)for@$J1;&$_($Cl)for@$Dl;&$_($Il)for@$J1;&$_($Ml)for@$J1;&$_($Pl)for@$Ql;&$_($Sl)for@$Tl;&$_($dm)for@$em;&$_($jm)for@$J1;&$_($mm)for@$nm;&$_($rm)for@$J1;&$_($um)for@$vm;&$_($zm)for@$J1;&$_($Cm)for@$J1;&$_($Fm)for@$Gm;&$_($Im)for@$Jm;&$_($Xm)for@$Ym;&$_($gn)for@$J1;&$_($kn)for@$J1;&$_($on)for@$J1;&$_($sn)for@$J1;&$_($vn)for@$wn;&$_($An)for@$J1;&$_($Dn)for@$En;&$_($In)for@$Jn;&$_($Nn)for@$J1;&$_($Qn)for@$J1;&$_($Tn)for@$Un;&$_($Wn)for@$Xn;&$_($io)for@$jo;&$_($oo)for@$J1;&$_($ro)for@$J1;&$_($uo)for@$vo;&$_($zo)for@$J1;&$_($Co)for@$Do;&$_($Io)for@$J1;&$_($Lo)for@$Mo;&$_($Qo)for@$J1;&$_($To)for@$J1;&$_($Wo)for@$Xo;&$_($Zo)for@$cp;&$_($lp)for@$mp;&$_($rp)for@$J1;&$_($vp)for@$J1;&$_($zp)for@$J1;&$_($Cp)for@$Dp;&$_($Hp)for@$J1;&$_($Kp)for@$Lp;&$_($Pp)for@$J1;&$_($Tp)for@$J1;&$_($Wp)for@$J1;&$_($Zp)for@$cq;&$_($eq)for@$fq;&$_($oq)for@$J1;&$_($sq)for@$J1;&$_($wq)for@$J1;&$_($yq)for@$zq;ni->run(@ARGV);
__DATA__
