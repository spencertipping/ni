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
  die "ni:/c/fn failed to compile $$self{code}: $@\n" if $@;
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
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$M1=bless({$v1,$L1},$S);$N1=q'instantiate';$O1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$P1=bless({$v1,$O1},$S);$Q1={$K1,$M1,$N1,$P1};$R1=q'/lib/fn_init.b';$S1=bless({$c,$H1,$j1,$J1,$l1,$k1,$m1,$Q1,$f,$R1},$K);$T1=q'/lib/slice::ctors';$U1={};$V1=q'serialize';$W1=q'annotations';$X1=[];$Y1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$Z1=bless({$W1,$X1,$v1,$Y1},$S);$c2={$V1,$Z1};$d2=q'/lib/fn_serialize.b';$e2=bless({$c,$U1,$j1,$k1,$l1,$k1,$m1,$c2,$f,$d2},$K);$f2=q'/lib/slice::ctors';$g2=[undef,$E1,$S1,$e2];$h2=bless({$c,$s1,$f,$S,$g,$g2},$m);$i2=q'/lib/fn.c::ctors';$j2=q'ni \'ni:\' . ref shift';$k2=bless({$v1,$j2},$S);$l2={$n1,$k2};$m2=q'/lib/instance.b';$n2=bless({$c,$i1,$j1,$k1,$l1,$k1,$m1,$l2,$f,$m2},$K);$o2=q'/unix/pipeline.c';$p2=q'/lib/slice::ctors';$q2=[$n2];$r2=bless({$c,$h1,$f,$V,$g,$q2},$r);$s2=q'/object.c::ctors';$t2=[$r2];$u2=bless({$c,$P,$f,$M,$g,$t2},$j);$v2=q'/lib/behavior.c::ctors';$w2={};$x2=q'my $s = shift; ni->def($s->name, $s)';$y2=bless({$v1,$x2},$S);$z2=q'$_[0]->namespace . ":" . $_[0]->{name}';$A2=bless({$v1,$z2},$S);$B2={$f,$A2};$C2=q'/lib/named.b';$D2=bless({$c,$w2,$j1,$y2,$l1,$k1,$m1,$B2,$f,$C2},$K);$E2=q'/unix/pipeline.c';$F2=q'/lib/slice::ctors';$G2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$H2=bless({$v1,$G2},$S);$I2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$J2=bless({$v1,$I2},$S);$K2=q'/lib/slice::apply';$L2=q'/lib/slice::apply_unsafe';$M2={};$N2=q'apply';$O2=q'apply_unsafe';$P2={$N2,$H2,$O2,$J2};$Q2=q'/lib/slice.b';$R2=bless({$c,$M2,$m1,$P2,$f,$Q2},$K);$S2=q'/lib/slice::ctors';$T2={};$U2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$V2=bless({$v1,$U2},$S);$W2={$N1,$V2};$X2=q'/lib/slice_init.b';$Y2=bless({$c,$T2,$m1,$W2,$f,$X2},$K);$Z2=q'/lib/slice::ctors';$c3={};$d3=[];$e3=q'local $_;
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
$g;';$f3=bless({$W1,$d3,$v1,$e3},$S);$g3={$V1,$f3};$h3=q'/lib/slice_serialize.b';$i3=bless({$c,$c3,$j1,$k1,$l1,$k1,$m1,$g3,$f,$h3},$K);$j3=q'/lib/slice::ctors';$k3=[$u2,$D2,$R2,$Y2,$i3];$l3=bless({$c,$L,$f,$K,$g,$k3},$p);$m3=q'/lib/slice.c::ctors';$n3={};$o3=q'doc';$p3=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$q3=bless({$v1,$p3},$S);$r3={$o3,$q3};$s3=q'/lib/documentable.b';$t3=bless({$c,$n3,$j1,$k1,$l1,$k1,$m1,$r3,$f,$s3},$K);$u3=q'/unix/pipeline.c';$v3=q'/lib/slice::ctors';$w3=[undef,$t3];$x3=bless({$c,$A,$f,$r,$g,$w3},$E);$y3=q'/metaclass::ctors';$z3=[$x3];$A3=bless({$c,$i,$f,$h,$g,$z3},$E);$B3=q'/metaclass::ctors';$C3=q'/unix/pipeline.c';$D3={$Q,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$C3,1,$z,1};$E3={$q,1};$F3=[$F];$G3=bless({$c,$E3,$f,$q,$g,$F3},$E);$H3=q'/metaclass::ctors';$I3={$O,1};$J3={};$K3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$L3=bless({$v1,$K3},$S);$M3={$N2,$L3};$N3=q'/lib/tag.b';$O3=bless({$c,$J3,$j1,$k1,$l1,$k1,$m1,$M3,$f,$N3},$K);$P3=q'/lib/slice::ctors';$Q3={};$R3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$S3=bless({$v1,$R3},$S);$T3={$N1,$S3};$U3=q'/lib/tag_init.b';$V3=bless({$c,$Q3,$j1,$k1,$l1,$k1,$m1,$T3,$f,$U3},$K);$W3=q'/lib/slice::ctors';$X3=[$u2,$D2,$O3,$V3];$Y3=bless({$c,$I3,$f,$O,$g,$X3},$q);$Z3=q'/lib/tag.c::ctors';$c4=q'/lib/perlbranch.b';$d4={};$e4=q'add';$f4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$g4=bless({$v1,$f4},$S);$h4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$i4=bless({$v1,$h4},$S);$j4={$e4,$g4,$N2,$i4};$k4=q'/lib/branch.b';$l4=bless({$c,$d4,$j1,$k1,$l1,$k1,$m1,$j4,$f,$k4},$K);$m4=q'/unix/pipeline.c';$n4=q'/lib/slice::ctors';$o4={};$p4=q'namespace';$q4=q'\'ni\'';$r4=bless({$v1,$q4},$S);$s4={$p4,$r4};$t4=q'/lib/named_in_ni.b';$u4=bless({$c,$o4,$j1,$k1,$l1,$k1,$m1,$s4,$f,$t4},$K);$v4=q'/unix/pipeline.c';$w4=q'/lib/slice::ctors';$x4={};$y4=q'package';$z4=q'shift->{name}';$A4=bless({$v1,$z4},$S);$B4={$y4,$A4};$C4=q'/lib/namespaced.b';$D4=bless({$c,$x4,$j1,$k1,$l1,$k1,$m1,$B4,$f,$C4},$K);$E4=q'/unix/pipeline.c';$F4=q'/lib/slice::ctors';$G4={};$H4=q'resolve';$I4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$J4=bless({$v1,$I4},$S);$K4={$H4,$J4};$L4=q'/lib/resolver.b';$M4=bless({$c,$G4,$j1,$k1,$l1,$k1,$m1,$K4,$f,$L4},$K);$N4=q'/unix/pipeline.c';$O4=q'/lib/slice::ctors';$P4=[$l4,$E1,$D2,$u4,$D4,$M4];$Q4=bless({$f,$c4,$g,$P4},$O);$R4=q'/lib/tag::ctors';$S4={};$T4=q'my $s = shift; $s->apply($s->package)';$U4=bless({$v1,$T4},$S);$V4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$W4=bless({$v1,$V4},$S);$X4={$N1,$W4};$Y4=q'/lib/class_init.b';$Z4=bless({$c,$S4,$j1,$U4,$l1,$k1,$m1,$X4,$f,$Y4},$K);$c5=q'/unix/pipeline.c';$d5=q'/lib/slice::ctors';$e5={$k,1};$f5=[$F];$g5=bless({$c,$e5,$f,$k,$g,$f5},$E);$h5=q'/metaclass::ctors';$i5={$N,1};$j5={};$k5=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$l5=bless({$v1,$k5},$S);$m5={$N1,$l5};$n5=q'/lib/branch_init.b';$o5=bless({$c,$j5,$j1,$k1,$l1,$k1,$m1,$m5,$f,$n5},$K);$p5=q'/lib/slice::ctors';$q5=[$u2,$D2,$l4,$o5,undef];$r5=bless({$c,$i5,$f,$N,$g,$q5},$k);$s5=q'/lib/branch.c::ctors';$t5=q'/unix/pipeline.c';$u5={$Q,1,$h,1,$j,1,$N,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$E,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$t5,1,$z,1};$v5=q'/lib/definition.b';$w5={};$x5=q'def';$y5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$z5=bless({$v1,$y5},$S);$A5={$x5,$z5};$B5=q'/lib/classdef.b';$C5=bless({$c,$w5,$j1,$k1,$l1,$k1,$m1,$A5,$f,$B5},$K);$D5=q'/unix/pipeline.c';$E5=q'/lib/slice::ctors';$F5={};$G5=q'ro';$H5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$I5=bless({$v1,$H5},$S);$J5=q'rw';$K5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$L5=bless({$v1,$K5},$S);$M5={$G5,$I5,$J5,$L5};$N5=q'/lib/accessor.b';$O5=bless({$c,$F5,$j1,$k1,$l1,$k1,$m1,$M5,$f,$N5},$K);$P5=q'/unix/pipeline.c';$Q5=q'/lib/slice::ctors';$R5=[$C5,$O5];$S5=bless({$c,$u5,$f,$v5,$g,$R5},$N);$T5=q'/lib/branch::ctors';$U5={};$V5=q'child';$W5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$X5=bless({$v1,$W5},$S);$Y5={$V5,$X5};$Z5=q'/lib/subclass.b';$c6=bless({$c,$U5,$j1,$k1,$l1,$k1,$m1,$Y5,$f,$Z5},$K);$d6=q'/unix/pipeline.c';$e6=q'/lib/slice::ctors';$f6=[$Q4,$Z4,$r2,$S5,$c6];$g6=bless({$c,$D3,$f,$Q,$g,$f6},$h);$h6=q'/class.c::ctors';$i6=[$g6];$j6=bless({$c,$e,$f,$d,$g,$i6},$E);$k6=q'/metaclass::ctors';$l6={$E,1};$m6=[$Q4,$Z4,$r2,$S5];$n6=bless({$c,$l6,$f,$E,$g,$m6},$d);$o6=q'/metaclass.c::ctors';$p6={$o,1};$q6=[$x3];$r6=bless({$c,$p6,$f,$o,$g,$q6},$E);$s6=q'/metaclass::ctors';$t6={$U,1};$u6={};$v6=q'is_mutable';$w6=[];$x6=q'$0 ne "-" && -w $0';$y6=bless({$W1,$w6,$v1,$x6},$S);$z6=q'modify';$A6=[];$B6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$C6=bless({$W1,$A6,$v1,$B6},$S);$D6={$v6,$y6,$z6,$C6};$E6=q'/lib/ni_self.b';$F6=bless({$c,$u6,$j1,$k1,$l1,$k1,$m1,$D6,$f,$E6},$K);$G6=q'/lib/slice::ctors';$H6={};$I6=q'exists';$J6=[];$K6=q'exists $_[0]->{named}{$_[1]}';$L6=bless({$W1,$J6,$v1,$K6},$S);$M6=q'quoted';$N6=[];$O6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$P6=bless({$W1,$N6,$v1,$O6},$S);$Q6={$I6,$L6,$M6,$P6};$R6=q'/lib/ni_image.b';$S6=bless({$c,$H6,$j1,$k1,$l1,$k1,$m1,$Q6,$f,$R6},$K);$T6=q'/lib/slice::ctors';$U6={};$V6=q'--internal/+=';$W6=[];$X6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$Y6=bless({$W1,$W6,$v1,$X6},$S);$Z6=q'--internal/eval';$c7=[];$d7=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$e7=bless({$W1,$c7,$v1,$d7},$S);$f7=q'--internal/image';$g7=[];$h7=q'shift->quoted->write(\\*STDOUT);
0;';$i7=bless({$W1,$g7,$v1,$h7},$S);$j7=q'run';$k7=[];$l7=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$m7=bless({$W1,$k7,$v1,$l7},$S);$n7={$V6,$Y6,$Z6,$e7,$f7,$i7,$j7,$m7};$o7=q'/lib/ni_main.b';$p7=bless({$c,$U6,$j1,$k1,$l1,$k1,$m1,$n7,$f,$o7},$K);$q7=q'/lib/slice::ctors';$r7={};$s7=[];$t7=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$u7=bless({$W1,$s7,$v1,$t7},$S);$v7=q'resolver_for';$w7=[];$x7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$y7=bless({$W1,$w7,$v1,$x7},$S);$z7={$H4,$u7,$v7,$y7};$A7=q'/lib/ni_resolver.b';$B7=bless({$c,$r7,$j1,$k1,$l1,$k1,$m1,$z7,$f,$A7},$K);$C7=q'/lib/slice::ctors';$D7={};$E7=q'fork';$F7=[];$G7=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$H7=bless({$W1,$F7,$v1,$G7},$S);$I7=q'fork_exec';$J7=[];$K7=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$L7=bless({$W1,$J7,$v1,$K7},$S);$M7={$E7,$H7,$I7,$L7};$N7=q'/lib/ni_pid_ctors';$O7=bless({$c,$D7,$j1,$k1,$l1,$k1,$m1,$M7,$f,$N7},$K);$P7=q'/lib/slice::ctors';$Q7=[$r2,$F6,$S6,$p7,$B7,$O7];$R7=bless({$c,$t6,$f,$U,$g,$Q7},$o);$S7=q'/lib/ni.c::ctors';$T7=q'named';$U7=q'ni.doc:/class';$V7={$l,1};$W7=[$x3];$X7=bless({$c,$V7,$f,$l,$g,$W7},$E);$Y7=q'/metaclass::ctors';$Z7={$R,1};$c8={};$d8=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$e8=bless({$v1,$d8},$S);$f8={$N1,$e8};$g8=q'/lib/doc_init.b';$h8=bless({$c,$c8,$j1,$k1,$l1,$k1,$m1,$f8,$f,$g8},$K);$i8=q'/lib/slice::ctors';$j8={};$k8=q'\'ni.doc\'';$l8=bless({$v1,$k8},$S);$m8={$p4,$l8};$n8=q'/lib/doc_namespace.b';$o8=bless({$c,$j8,$j1,$k1,$l1,$k1,$m1,$m8,$f,$n8},$K);$p8=q'/lib/slice::ctors';$q8=[$r2,$D2,$h8,$o8];$r8=bless({$c,$Z7,$f,$R,$g,$q8},$l);$s8=q'/lib/doc.c::ctors';$t8=q'apropos';$u8=q'ni:/class';$v8=[$u8];$w8=q'# Classes and metaclasses
This documentation assumes that you understand how Smalltalk-80 metaclasses
work (Ruby eigenclasses might do it, but they\'re fairly different). The
[wikipedia article on metaclasses](https://en.wikipedia.org/wiki/Metaclass) has
a good explanation of the different paradigms.

ni\'s metaclass structure is fairly similar to Smalltalk\'s, but with a few
important differences:

1. ni\'s classes are built as sums of _behaviors_, which are objects that modify
   Perl packages.
2. ni\'s classes and behaviors are named objects, accessible by using
   `ni("ni:<name>")`. For class objects, instances are blessed into the
   `<name>` Perl package.
3. We don\'t use Perl\'s inheritance mechanism; all inheritance is resolved up
   front. This means that you can combine behaviors only when their method sets
   are disjoint.

ni namespaces its classes and related objects in two ways, each of which is a
convention and has no structural significance:

- Each object is prefixed with a directory, e.g. `/lib/`.
- Each object is suffixed with an extension specifying its role in the
  object-oriented system: `.b` for behavior, no extension for class, and `.c`
  for metaclass

## Object hierarchy
In the table below I use the following notation:

- `a : b` means "`a` is an instance of `b`", i.e. "the object `a` is blessed
  into the Perl package owned by the object `b`"
- `a = b + c + d` means "`a` inherits from `b`, `c`, and `d`" -- i.e. "`b`,
  `c`, and `d` will be applied to the Perl package owned by object `a`"

```
/object    : /object.c    = /lib/instance.b + ...
/class     : /class.c     = /object + /lib/perlbranch.b + /lib/class_init.b + ...
/metaclass : /metaclass.c = /object + /lib/perlbranch.b + /lib/class_init.b + ...
/fn        : /fn.c        = /object + /lib/fn_init.b + ...

/object.c    : /metaclass = /class
/class.c     : /metaclass = /object.c
/metaclass.c : /metaclass = /class
/fn.c        : /metaclass = /object.c

/lib/slice    : /lib/slice.c    = /lib/behavior + /lib/named.b + /lib/slice.b + /lib/slice_init.b + ...
/lib/branch   : /lib/branch.c   = /lib/behavior + /lib/named.b + /lib/branch.b + /lib/branch_init.b + ...
/lib/tag      : /lib/tag.c      = /lib/behavior + /lib/named.b + /lib/tag.b + /lib/tag_init.b + ...
/lib/behavior : /lib/behavior.c = /object + ...

/lib/slice.c    : /metaclass = /lib/behavior.c + ...
/lib/branch.c   : /metaclass = /lib/behavior.c + ...
/lib/tag.c      : /metaclass = /lib/behavior.c + ...
/lib/behavior.c : /metaclass = /object.c

/lib/branch.b       : /lib/slice
/lib/tag.b          : /lib/slice
/lib/slice.b        : /lib/slice
/lib/instantiable.b : /lib/slice
/lib/instance.b     : /lib/slice
/lib/named.b        : /lib/slice
/lib/namespaced.b   : /lib/slice
/lib/resolver.b     : /lib/slice
/lib/perlbranch.b   : /lib/tag = /lib/branch.b
                               + /lib/instantiable.b
                               + /lib/named.b
                               + /lib/namespaced.b
                               + /lib/resolver.b
```
';$x8=bless({$t8,$v8,$o3,$w8,$f,$Q},$R);$y8=q'/lib/doc::ctors';$z8=q'ni:/class.c';$A8=q'ni:/lib/accessor.b';$B8=q'ni:/lib/behavior';$C8=q'ni:/lib/behavior.c';$D8=q'ni:/lib/branch';$E8=q'ni:/lib/branch.b';$F8=q'ni:/lib/branch.c';$G8=q'ni:/lib/branch_init.b';$H8=q'ni:/lib/class_init.b';$I8=q'ni:/lib/classdef.b';$J8=q'ni:/lib/definition.b';$K8=q'ni:/lib/doc';$L8=q'ni:/lib/doc.c';$M8=q'ni:/lib/doc_init.b';$N8=q'ni:/lib/doc_namespace.b';$O8=q'ni:/lib/documentable.b';$P8=q'ni:/lib/fn';$Q8=q'ni:/lib/fn.c';$R8=q'ni:/lib/fn_init.b';$S8=q'ni:/lib/fn_serialize.b';$T8=q'ni:/lib/image';$U8={$n,1};$V8=[$x3];$W8=bless({$c,$U8,$f,$n,$g,$V8},$E);$X8=q'/metaclass::ctors';$Y8={$T,1};$Z8={};$c9=[];$d9=q'my $class = shift;
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
  ordering     => []};';$e9=bless({$W1,$c9,$v1,$d9},$S);$f9={$N1,$e9};$g9=q'/lib/image_init.b';$h9=bless({$c,$Z8,$j1,$k1,$l1,$k1,$m1,$f9,$f,$g9},$K);$i9=q'/lib/slice::ctors';$j9={};$k9=q'address';$l9=[];$m9=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$n9=bless({$W1,$l9,$v1,$m9},$S);$o9=q'allocate_gensym';$p9=[];$q9=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$r9=bless({$W1,$p9,$v1,$q9},$S);$s9=q'boot_side_effect';$t9=[];$u9=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$v9=bless({$W1,$t9,$v1,$u9},$S);$w9=q'circular_links';$x9=[];$y9=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$z9=bless({$W1,$x9,$v1,$y9},$S);$A9=q'finalizer';$B9=[];$C9=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$D9=bless({$W1,$B9,$v1,$C9},$S);$E9=q'gensym';$F9=[];$G9=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$H9=bless({$W1,$F9,$v1,$G9},$S);$I9=q'is_circular';$J9=[];$K9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$L9=bless({$W1,$J9,$v1,$K9},$S);$M9=q'quote';$N9=[];$O9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$P9=bless({$W1,$N9,$v1,$O9},$S);$Q9=q'quote_array';$R9=[];$S9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$T9=bless({$W1,$R9,$v1,$S9},$S);$U9=q'quote_blessed';$V9=[];$W9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$X9=bless({$W1,$V9,$v1,$W9},$S);$Y9=q'quote_class';$Z9=[];$ca=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$da=bless({$W1,$Z9,$v1,$ca},$S);$ea=q'quote_hash';$fa=[];$ga=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$ha=bless({$W1,$fa,$v1,$ga},$S);$ia=q'quote_object';$ja=[];$ka=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$la=bless({$W1,$ja,$v1,$ka},$S);$ma=q'quote_scalar';$na=[];$oa=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$pa=bless({$W1,$na,$v1,$oa},$S);$qa=q'quote_value';$ra=[];$sa=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$ta=bless({$W1,$ra,$v1,$sa},$S);$ua=q'reconstruction';$va=[];$wa=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$xa=bless({$W1,$va,$v1,$wa},$S);$ya=q'side_effect';$za=[];$Aa=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Ba=bless({$W1,$za,$v1,$Aa},$S);$Ca=q'write';$Da=[];$Ea=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Fa=bless({$W1,$Da,$v1,$Ea},$S);$Ga={$k9,$n9,$o9,$r9,$s9,$v9,$w9,$z9,$A9,$D9,$E9,$H9,$I9,$L9,$M9,$P9,$Q9,$T9,$U9,$X9,$Y9,$da,$ea,$ha,$ia,$la,$ma,$pa,$qa,$ta,$ua,$xa,$ya,$Ba,$Ca,$Fa};$Ha=q'/lib/image_quoting.b';$Ia=bless({$c,$j9,$j1,$k1,$l1,$k1,$m1,$Ga,$f,$Ha},$K);$Ja=q'/lib/slice::ctors';$Ka=[$r2,$h9,$Ia];$La=bless({$c,$Y8,$f,$T,$g,$Ka},$n);$Ma=q'/lib/image.c::ctors';$Na=q'ni:/lib/image.c';$Oa=q'ni:/lib/image_init.b';$Pa=q'ni:/lib/image_quoting.b';$Qa=q'ni:/lib/instance.b';$Ra=q'ni:/lib/instantiable.b';$Sa=q'ni:/lib/named.b';$Ta=q'ni:/lib/named_in_ni.b';$Ua=q'ni:/lib/namespaced.b';$Va=q'ni:/lib/ni';$Wa=q'ni:/lib/ni.c';$Xa=q'ni:/lib/ni_image.b';$Ya=q'ni:/lib/ni_main.b';$Za=q'ni:/lib/ni_pid_ctors';$cb=q'ni:/lib/ni_resolver.b';$db=q'ni:/lib/ni_self.b';$eb=q'ni:/lib/ni_static';$fb=q'/lib/ni_static';$gb=q'ni';$hb={$fb,1,$gb,1};$ib={};$jb=q'abbrev';$kb=[];$lb=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$mb=bless({$W1,$kb,$v1,$lb},$S);$nb=q'dor';$ob=[];$pb=q'defined $_[0] ? $_[0] : $_[1]';$qb=bless({$W1,$ob,$v1,$pb},$S);$rb=q'indent';$sb=[];$tb=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$ub=bless({$W1,$sb,$v1,$tb},$S);$vb=q'max';$wb=[];$xb=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$yb=bless({$W1,$wb,$v1,$xb},$S);$zb=q'maxstr';$Ab=[];$Bb=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Cb=bless({$W1,$Ab,$v1,$Bb},$S);$Db=q'mean';$Eb=[];$Fb=q'sum(@_) / (@_ || 1)';$Gb=bless({$W1,$Eb,$v1,$Fb},$S);$Hb=q'min';$Ib=[];$Jb=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$Kb=bless({$W1,$Ib,$v1,$Jb},$S);$Lb=q'minstr';$Mb=[];$Nb=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Ob=bless({$W1,$Mb,$v1,$Nb},$S);$Pb=q'sgr';$Qb=[];$Rb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Sb=bless({$W1,$Qb,$v1,$Rb},$S);$Tb=q'sr';$Ub=[];$Vb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Wb=bless({$W1,$Ub,$v1,$Vb},$S);$Xb=q'sum';$Yb=[];$Zb=q'local $_; my $x = 0; $x += $_ for @_; $x';$cc=bless({$W1,$Yb,$v1,$Zb},$S);$dc=q'swap';$ec=[];$fc=q'@_[0, 1] = @_[1, 0]';$gc=bless({$W1,$ec,$v1,$fc},$S);$hc={$jb,$mb,$nb,$qb,$rb,$ub,$vb,$yb,$zb,$Cb,$Db,$Gb,$Hb,$Kb,$Lb,$Ob,$Pb,$Sb,$Tb,$Wb,$Xb,$cc,$dc,$gc};$ic=q'/lib/ni_static_util.b';$jc=bless({$c,$ib,$j1,$k1,$l1,$k1,$m1,$hc,$f,$ic},$K);$kc=q'/lib/slice::ctors';$lc=[$jc];$mc=bless({$c,$hb,$f,$fb,$g,$lc},$Q);$nc=q'/class::ctors';$oc=q'ni:/lib/ni_static_util.b';$pc=q'ni:/lib/perlbranch.b';$qc=q'ni:/lib/resolver.b';$rc=q'ni:/lib/slice';$sc=q'ni:/lib/slice.b';$tc=q'ni:/lib/slice.c';$uc=q'ni:/lib/slice_init.b';$vc=q'ni:/lib/slice_serialize.b';$wc=q'ni:/lib/subclass.b';$xc=q'ni:/lib/tag';$yc=q'ni:/lib/tag.b';$zc=q'ni:/lib/tag.c';$Ac=q'ni:/lib/tag_init.b';$Bc=q'ni:/metaclass';$Cc=q'ni:/metaclass.c';$Dc=q'ni:/object';$Ec=q'ni:/object.c';$Fc=q'ni:/unix/cat';$Gc={$s,1};$Hc=q'/unix/pipeline.c';$Ic={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$Hc,1,$z,1};$Jc=[$x3];$Kc=bless({$c,$Ic,$f,$w,$g,$Jc},$E);$Lc=q'/metaclass::ctors';$Mc=[$Kc];$Nc=bless({$c,$Gc,$f,$s,$g,$Mc},$E);$Oc=q'/metaclass::ctors';$Pc={$W,1};$Qc={$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1,$e1,1,$g1,1};$Rc={};$Sc=q'into';$Tc=[];$Uc=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Vc=bless({$W1,$Tc,$v1,$Uc},$S);$Wc={$Sc,$Vc};$Xc=q'/unix/io_stream.b';$Yc=bless({$c,$Rc,$j1,$k1,$l1,$k1,$m1,$Wc,$f,$Xc},$K);$Zc=q'/lib/slice::ctors';$cd={};$dd=q'(+';$ed=[];$fd=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$gd=bless({$W1,$ed,$v1,$fd},$S);$hd={$dd,$gd};$id=q'/unix/io_constructors.b';$jd=bless({$c,$cd,$j1,$k1,$l1,$k1,$m1,$hd,$f,$id},$K);$kd=q'/lib/slice::ctors';$ld={};$md=q'(<>';$nd=[];$od=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$pd=bless({$W1,$nd,$v1,$od},$S);$qd=q'(@{}';$rd=[];$sd=q'my $self = shift; [<$self>]';$td=bless({$W1,$rd,$v1,$sd},$S);$ud={$md,$pd,$qd,$td};$vd=q'/unix/io_readers.b';$wd=bless({$c,$ld,$j1,$k1,$l1,$k1,$m1,$ud,$f,$vd},$K);$xd=q'/lib/slice::ctors';$yd=[$r2,$Yc,$jd,$wd];$zd=bless({$c,$Qc,$f,$c1,$g,$yd},$w);$Ad=q'/unix/io.c::ctors';$Bd={};$Cd=[];$Dd=q'shift; +{fs => [@_]}';$Ed=bless({$W1,$Cd,$v1,$Dd},$S);$Fd={$N1,$Ed};$Gd=q'/unix/cat_init.b';$Hd=bless({$c,$Bd,$j1,$k1,$l1,$k1,$m1,$Fd,$f,$Gd},$K);$Id=q'/lib/slice::ctors';$Jd={};$Kd=q'read';$Ld=[];$Md=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Nd=bless({$W1,$Ld,$v1,$Md},$S);$Od={$Kd,$Nd};$Pd=q'/unix/cat_read.b';$Qd=bless({$c,$Jd,$j1,$k1,$l1,$k1,$m1,$Od,$f,$Pd},$K);$Rd=q'/lib/slice::ctors';$Sd=[$zd,$Hd,$Qd];$Td=bless({$c,$Pc,$f,$W,$g,$Sd},$s);$Ud=q'/unix/cat.c::ctors';$Vd=q'ni:/unix/cat.c';$Wd=q'ni:/unix/cat_init.b';$Xd=q'ni:/unix/cat_read.b';$Yd=q'ni:/unix/fd';$Zd={$t,1};$ce=[$Kc];$de=bless({$c,$Zd,$f,$t,$g,$ce},$E);$ee=q'/metaclass::ctors';$fe={$X,1};$ge={};$he=q'fd';$ie=[];$je=q'shift->{\'fd\'}';$ke=bless({$W1,$ie,$v1,$je},$S);$le={$he,$ke};$me=q'/unix/fd_readers.b';$ne=bless({$c,$ge,$j1,$k1,$l1,$k1,$m1,$le,$f,$me},$K);$oe=q'/lib/slice::ctors';$pe={};$qe=[];$re=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$se=bless({$W1,$qe,$v1,$re},$S);$te={$N1,$se};$ue=q'/unix/fd_init.b';$ve=bless({$c,$pe,$j1,$k1,$l1,$k1,$m1,$te,$f,$ue},$K);$we=q'/lib/slice::ctors';$xe={};$ye=q'move_to';$ze=[];$Ae=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Be=bless({$W1,$ze,$v1,$Ae},$S);$Ce={$ye,$Be};$De=q'/unix/fd_shell.b';$Ee=bless({$c,$xe,$j1,$k1,$l1,$k1,$m1,$Ce,$f,$De},$K);$Fe=q'/lib/slice::ctors';$Ge={$X,1,$Y,1,$Z,1,$d1,1,$e1,1};$He=q'/unix/has_fd.b';$Ie={};$Je=[];$Ke=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Le=bless({$W1,$Je,$v1,$Ke},$S);$Me=[];$Ne=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Oe=bless({$W1,$Me,$v1,$Ne},$S);$Pe={$Kd,$Le,$Ca,$Oe};$Qe=q'/unix/fd_safeio.b';$Re=bless({$c,$Ie,$j1,$k1,$l1,$k1,$m1,$Pe,$f,$Qe},$K);$Se=q'/lib/slice::ctors';$Te=[$Re];$Ue=bless({$c,$Ge,$f,$He,$g,$Te},$N);$Ve=q'/lib/branch::ctors';$We={};$Xe=q'read_fh';$Ye=[];$Ze=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$cf=bless({$W1,$Ye,$v1,$Ze},$S);$df=q'write_fh';$ef=[];$ff=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$gf=bless({$W1,$ef,$v1,$ff},$S);$hf={$Xe,$cf,$df,$gf};$if=q'/unix/fd_io.b';$jf=bless({$c,$We,$j1,$k1,$l1,$k1,$m1,$hf,$f,$if},$K);$kf=q'/lib/slice::ctors';$lf=[$zd,$ne,$ve,$Ee,$Ue,$jf];$mf=bless({$c,$fe,$f,$X,$g,$lf},$t);$nf=q'/unix/fd.c::ctors';$of=q'ni:/unix/fd.c';$pf=q'ni:/unix/fd_init.b';$qf=q'ni:/unix/fd_io.b';$rf=q'ni:/unix/fd_readers.b';$sf=q'ni:/unix/fd_safeio.b';$tf=q'ni:/unix/fd_shell.b';$uf=q'ni:/unix/fifo';$vf={$u,1};$wf=[$Kc];$xf=bless({$c,$vf,$f,$u,$g,$wf},$E);$yf=q'/metaclass::ctors';$zf={$Y,1};$Af={};$Bf=[];$Cf=q'shift->{\'read_fh\'}';$Df=bless({$W1,$Bf,$v1,$Cf},$S);$Ef=[];$Ff=q'shift->{\'write_fh\'}';$Gf=bless({$W1,$Ef,$v1,$Ff},$S);$Hf={$Xe,$Df,$df,$Gf};$If=q'/unix/fifo_io.b';$Jf=bless({$c,$Af,$j1,$k1,$l1,$k1,$m1,$Hf,$f,$If},$K);$Kf=q'/lib/slice::ctors';$Lf={};$Mf=[];$Nf=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Of=bless({$W1,$Mf,$v1,$Nf},$S);$Pf={$N1,$Of};$Qf=q'/unix/fifo_init.b';$Rf=bless({$c,$Lf,$j1,$k1,$l1,$k1,$m1,$Pf,$f,$Qf},$K);$Sf=q'/lib/slice::ctors';$Tf={};$Uf=q'read_side';$Vf=[];$Wf=q'my $self = shift; close $$self{write_fh}; $self';$Xf=bless({$W1,$Vf,$v1,$Wf},$S);$Yf=q'write_side';$Zf=[];$cg=q'my $self = shift; close $$self{read_fh};  $self';$dg=bless({$W1,$Zf,$v1,$cg},$S);$eg={$Uf,$Xf,$Yf,$dg};$fg=q'/unix/fifo_direction.b';$gg=bless({$c,$Tf,$j1,$k1,$l1,$k1,$m1,$eg,$f,$fg},$K);$hg=q'/lib/slice::ctors';$ig=[$zd,$Jf,$Rf,$Ue,$gg];$jg=bless({$c,$zf,$f,$Y,$g,$ig},$u);$kg=q'/unix/fifo.c::ctors';$lg=q'ni:/unix/fifo.c';$mg=q'ni:/unix/fifo_direction.b';$ng=q'ni:/unix/fifo_init.b';$og=q'ni:/unix/fifo_io.b';$pg=q'ni:/unix/file';$qg={$v,1};$rg=[$Kc];$sg=bless({$c,$qg,$f,$v,$g,$rg},$E);$tg=q'/metaclass::ctors';$ug={$Z,1};$vg={};$wg=[];$xg=q'shift->{\'name\'}';$yg=bless({$W1,$wg,$v1,$xg},$S);$zg={$f,$yg};$Ag=q'/unix/file_readers.b';$Bg=bless({$c,$vg,$j1,$k1,$l1,$k1,$m1,$zg,$f,$Ag},$K);$Cg=q'/lib/slice::ctors';$Dg={};$Eg=[];$Fg=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Gg=bless({$W1,$Eg,$v1,$Fg},$S);$Hg={$N1,$Gg};$Ig=q'/unix/file_init.b';$Jg=bless({$c,$Dg,$j1,$k1,$l1,$k1,$m1,$Hg,$f,$Ig},$K);$Kg=q'/lib/slice::ctors';$Lg={};$Mg=[];$Ng=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Og=bless({$W1,$Mg,$v1,$Ng},$S);$Pg=[];$Qg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Rg=bless({$W1,$Pg,$v1,$Qg},$S);$Sg={$Xe,$Og,$df,$Rg};$Tg=q'/unix/file_io.b';$Ug=bless({$c,$Lg,$j1,$k1,$l1,$k1,$m1,$Sg,$f,$Tg},$K);$Vg=q'/lib/slice::ctors';$Wg=[$zd,$Bg,$Jg,$Ue,$Ug];$Xg=bless({$c,$ug,$f,$Z,$g,$Wg},$v);$Yg=q'/unix/file.c::ctors';$Zg=q'ni:/unix/file.c';$ch=q'ni:/unix/file_init.b';$dh=q'ni:/unix/file_io.b';$eh=q'ni:/unix/file_readers.b';$fh=q'ni:/unix/has_fd.b';$gh=q'ni:/unix/io';$hh=q'ni:/unix/io.c';$ih=q'ni:/unix/io_constructors.b';$jh=q'ni:/unix/io_readers.b';$kh=q'ni:/unix/io_stream.b';$lh=q'ni:/unix/pid';$mh={$x,1};$nh=[$Kc];$oh=bless({$c,$mh,$f,$x,$g,$nh},$E);$ph=q'/metaclass::ctors';$qh={$d1,1};$rh={};$sh=q'pid';$th=[];$uh=q'shift->{\'pid\'}';$vh=bless({$W1,$th,$v1,$uh},$S);$wh=q'stderr';$xh=[];$yh=q'shift->{\'stderr\'}';$zh=bless({$W1,$xh,$v1,$yh},$S);$Ah=q'stdin';$Bh=[];$Ch=q'shift->{\'stdin\'}';$Dh=bless({$W1,$Bh,$v1,$Ch},$S);$Eh=q'stdout';$Fh=[];$Gh=q'shift->{\'stdout\'}';$Hh=bless({$W1,$Fh,$v1,$Gh},$S);$Ih={$sh,$vh,$wh,$zh,$Ah,$Dh,$Eh,$Hh};$Jh=q'/unix/pid_readers.b';$Kh=bless({$c,$rh,$j1,$k1,$l1,$k1,$m1,$Ih,$f,$Jh},$K);$Lh=q'/lib/slice::ctors';$Mh={};$Nh=[];$Oh=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Ph=bless({$W1,$Nh,$v1,$Oh},$S);$Qh={$N1,$Ph};$Rh=q'/unix/pid_init.b';$Sh=bless({$c,$Mh,$j1,$k1,$l1,$k1,$m1,$Qh,$f,$Rh},$K);$Th=q'/lib/slice::ctors';$Uh={};$Vh={};$Wh=q'/unix/pid_wait.b';$Xh=bless({$c,$Uh,$j1,$k1,$l1,$k1,$m1,$Vh,$f,$Wh},$K);$Yh=q'/lib/slice::ctors';$Zh={};$ci=[];$di=q'shift->{stdout}->read_fh';$ei=bless({$W1,$ci,$v1,$di},$S);$fi=[];$gi=q'shift->{stdin}->write_fh';$hi=bless({$W1,$fi,$v1,$gi},$S);$ii={$Xe,$ei,$df,$hi};$ji=q'/unix/pid_io.b';$ki=bless({$c,$Zh,$j1,$k1,$l1,$k1,$m1,$ii,$f,$ji},$K);$li=q'/lib/slice::ctors';$mi=[$zd,$Kh,$Sh,$Xh,$Ue,$ki];$ni=bless({$c,$qh,$f,$d1,$g,$mi},$x);$oi=q'/unix/pid.c::ctors';$pi=q'ni:/unix/pid.c';$qi=q'ni:/unix/pid_init.b';$ri=q'ni:/unix/pid_io.b';$si=q'ni:/unix/pid_readers.b';$ti=q'ni:/unix/pid_wait.b';$ui=q'ni:/unix/pipeline';$vi=q'/unix/pipeline.c';$wi={$vi,1};$xi=q'/unix/pipeline.c';$yi=[$Kc];$zi=bless({$c,$wi,$f,$xi,$g,$yi},$E);$Ai=q'/metaclass::ctors';$Bi={$e1,1};$Ci={};$Di=[];$Ei=q'shift->{\'stdin\'}';$Fi=bless({$W1,$Di,$v1,$Ei},$S);$Gi=[];$Hi=q'shift->{\'stdout\'}';$Ii=bless({$W1,$Gi,$v1,$Hi},$S);$Ji={$Ah,$Fi,$Eh,$Ii};$Ki=q'/unix/pipeline_ro.b';$Li=bless({$c,$Ci,$j1,$k1,$l1,$k1,$m1,$Ji,$f,$Ki},$K);$Mi=q'/lib/slice::ctors';$Ni={};$Oi=[];$Pi=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Qi=bless({$W1,$Oi,$v1,$Pi},$S);$Ri={$N1,$Qi};$Si=q'/unix/pipeline_init.b';$Ti=bless({$c,$Ni,$j1,$k1,$l1,$k1,$m1,$Ri,$f,$Si},$K);$Ui=q'/lib/slice::ctors';$Vi={};$Wi=q'async_step';$Xi=[];$Yi=q'local $_;
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
$self;';$Zi=bless({$W1,$Xi,$v1,$Yi},$S);$cj={$Wi,$Zi};$dj=q'/unix/pipeline_async.b';$ej=bless({$c,$Vi,$j1,$k1,$l1,$k1,$m1,$cj,$f,$dj},$K);$fj=q'/lib/slice::ctors';$gj={};$hj=[];$ij=q'shift->{stdout}->read_fh';$jj=bless({$W1,$hj,$v1,$ij},$S);$kj=[];$lj=q'shift->{stdin}->write_fh';$mj=bless({$W1,$kj,$v1,$lj},$S);$nj={$Xe,$jj,$df,$mj};$oj=q'/unix/pipeline_io.b';$pj=bless({$c,$gj,$j1,$k1,$l1,$k1,$m1,$nj,$f,$oj},$K);$qj=q'/lib/slice::ctors';$rj=[$zd,$Li,$Ti,$ej,$Ue,$pj];$sj=q'/unix/pipeline.c';$tj=bless({$c,$Bi,$f,$e1,$g,$rj},$sj);$uj=q'/unix/pipeline.c::ctors';$vj=q'ni:/unix/pipeline.c';$wj=q'ni:/unix/pipeline_async.b';$xj=q'ni:/unix/pipeline_init.b';$yj=q'ni:/unix/pipeline_io.b';$zj=q'ni:/unix/pipeline_ro.b';$Aj=q'ni:/unix/str';$Bj={$z,1};$Cj=[$Kc];$Dj=bless({$c,$Bj,$f,$z,$g,$Cj},$E);$Ej=q'/metaclass::ctors';$Fj={$g1,1};$Gj={};$Hj=[];$Ij=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Jj=bless({$W1,$Hj,$v1,$Ij},$S);$Kj={$N1,$Jj};$Lj=q'/unix/str_init.b';$Mj=bless({$c,$Gj,$j1,$k1,$l1,$k1,$m1,$Kj,$f,$Lj},$K);$Nj=q'/lib/slice::ctors';$Oj={};$Pj=[];$Qj=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Rj=bless({$W1,$Pj,$v1,$Qj},$S);$Sj=q'remaining';$Tj=[];$Uj=q'my $self = shift; $$self{end} - $$self{start}';$Vj=bless({$W1,$Tj,$v1,$Uj},$S);$Wj=[];$Xj=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Yj=bless({$W1,$Wj,$v1,$Xj},$S);$Zj={$Kd,$Rj,$Sj,$Vj,$Ca,$Yj};$ck=q'/unix/str_io.b';$dk=bless({$c,$Oj,$j1,$k1,$l1,$k1,$m1,$Zj,$f,$ck},$K);$ek=q'/lib/slice::ctors';$fk=[$zd,$Mj,$dk];$gk=bless({$c,$Fj,$f,$g1,$g,$fk},$z);$hk=q'/unix/str.c::ctors';$ik=q'ni:/unix/str.c';$jk=q'ni:/unix/str_init.b';$kk=q'ni:/unix/str_io.b';$lk={$U7,$x8,$u8,$g6,$z8,$A3,$A8,$O5,$B8,$u2,$C8,$F,$D8,$r5,$E8,$l4,$F8,$g5,$G8,$o5,$H8,$Z4,$I8,$C5,$J8,$S5,$K8,$r8,$L8,$X7,$M8,$h8,$N8,$o8,$O8,$t3,$P8,$h2,$Q8,$q1,$R8,$S1,$S8,$e2,$T8,$La,$Na,$W8,$Oa,$h9,$Pa,$Ia,$Qa,$n2,$Ra,$E1,$Sa,$D2,$Ta,$u4,$Ua,$D4,$Va,$R7,$Wa,$r6,$Xa,$S6,$Ya,$p7,$Za,$O7,$cb,$B7,$db,$F6,$eb,$mc,$oc,$jc,$pc,$Q4,$qc,$M4,$rc,$l3,$sc,$R2,$tc,$I,$uc,$Y2,$vc,$i3,$wc,$c6,$xc,$Y3,$yc,$O3,$zc,$G3,$Ac,$V3,$Bc,$n6,$Cc,$j6,$Dc,$r2,$Ec,$x3,$Fc,$Td,$Vd,$Nc,$Wd,$Hd,$Xd,$Qd,$Yd,$mf,$of,$de,$pf,$ve,$qf,$jf,$rf,$ne,$sf,$Re,$tf,$Ee,$uf,$jg,$lg,$xf,$mg,$gg,$ng,$Rf,$og,$Jf,$pg,$Xg,$Zg,$sg,$ch,$Jg,$dh,$Ug,$eh,$Bg,$fh,$Ue,$gh,$zd,$hh,$Kc,$ih,$jd,$jh,$wd,$kh,$Yc,$lh,$ni,$pi,$oh,$qi,$Sh,$ri,$ki,$si,$Kh,$ti,$Xh,$ui,$tj,$vj,$zi,$wj,$ej,$xj,$Ti,$yj,$pj,$zj,$Li,$Aj,$gk,$ik,$Dj,$jk,$Mj,$kk,$dk};$mk=q'resolvers';$nk=[];$ok=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$pk=bless({$W1,$nk,$v1,$ok},$S);$qk=q'file';$rk=[];$sk=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$tk=bless({$W1,$rk,$v1,$sk},$S);$uk=q'str';$vk=[];$wk=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$xk=bless({$W1,$vk,$v1,$wk},$S);$yk={$he,$pk,$qk,$tk,$uk,$xk};$zk=bless({$T7,$lk,$mk,$yk},$U);$Ak=q'/lib/ni::ctors';$$w3[0]=$g6;$$D[0]=$x3;$$p1[0]=$x3;$$g2[0]=$r2;$$q5[4]=$S5;*$L2=\&$J2;*$K2=\&$H2;$E1->apply_unsafe($Q);$E1->apply_unsafe($h);$E1->apply_unsafe($j);$E1->apply_unsafe($N);$E1->apply_unsafe($k);$E1->apply_unsafe($l);$E1->apply_unsafe($S);$E1->apply_unsafe($m);$E1->apply_unsafe($n);$E1->apply_unsafe($o);$E1->apply_unsafe($K);$E1->apply_unsafe($p);$E1->apply_unsafe($O);$E1->apply_unsafe($q);$E1->apply_unsafe($E);$E1->apply_unsafe($d);$E1->apply_unsafe($r);$E1->apply_unsafe($s);$E1->apply_unsafe($t);$E1->apply_unsafe($u);$E1->apply_unsafe($v);$E1->apply_unsafe($w);$E1->apply_unsafe($x);$E1->apply_unsafe($F1);$E1->apply_unsafe($z);$S1->apply_unsafe($S);$e2->apply_unsafe($S);$n2->apply_unsafe($Q);$n2->apply_unsafe($h);$n2->apply_unsafe($M);$n2->apply_unsafe($j);$n2->apply_unsafe($N);$n2->apply_unsafe($k);$n2->apply_unsafe($R);$n2->apply_unsafe($l);$n2->apply_unsafe($S);$n2->apply_unsafe($m);$n2->apply_unsafe($T);$n2->apply_unsafe($n);$n2->apply_unsafe($U);$n2->apply_unsafe($o);$n2->apply_unsafe($K);$n2->apply_unsafe($p);$n2->apply_unsafe($O);$n2->apply_unsafe($q);$n2->apply_unsafe($E);$n2->apply_unsafe($d);$n2->apply_unsafe($V);$n2->apply_unsafe($r);$n2->apply_unsafe($W);$n2->apply_unsafe($s);$n2->apply_unsafe($X);$n2->apply_unsafe($t);$n2->apply_unsafe($Y);$n2->apply_unsafe($u);$n2->apply_unsafe($Z);$n2->apply_unsafe($v);$n2->apply_unsafe($c1);$n2->apply_unsafe($w);$n2->apply_unsafe($d1);$n2->apply_unsafe($x);$n2->apply_unsafe($e1);$n2->apply_unsafe($o2);$n2->apply_unsafe($g1);$n2->apply_unsafe($z);$D2->apply_unsafe($Q);$D2->apply_unsafe($h);$D2->apply_unsafe($j);$D2->apply_unsafe($N);$D2->apply_unsafe($k);$D2->apply_unsafe($R);$D2->apply_unsafe($l);$D2->apply_unsafe($m);$D2->apply_unsafe($n);$D2->apply_unsafe($o);$D2->apply_unsafe($K);$D2->apply_unsafe($p);$D2->apply_unsafe($O);$D2->apply_unsafe($q);$D2->apply_unsafe($E);$D2->apply_unsafe($d);$D2->apply_unsafe($r);$D2->apply_unsafe($s);$D2->apply_unsafe($t);$D2->apply_unsafe($u);$D2->apply_unsafe($v);$D2->apply_unsafe($w);$D2->apply_unsafe($x);$D2->apply_unsafe($E2);$D2->apply_unsafe($z);$R2->apply_unsafe($K);$Y2->apply_unsafe($K);$i3->apply_unsafe($K);$t3->apply_unsafe($h);$t3->apply_unsafe($j);$t3->apply_unsafe($k);$t3->apply_unsafe($l);$t3->apply_unsafe($m);$t3->apply_unsafe($n);$t3->apply_unsafe($o);$t3->apply_unsafe($p);$t3->apply_unsafe($q);$t3->apply_unsafe($r);$t3->apply_unsafe($s);$t3->apply_unsafe($t);$t3->apply_unsafe($u);$t3->apply_unsafe($v);$t3->apply_unsafe($w);$t3->apply_unsafe($x);$t3->apply_unsafe($u3);$t3->apply_unsafe($z);$O3->apply_unsafe($O);$V3->apply_unsafe($O);$l4->apply_unsafe($Q);$l4->apply_unsafe($h);$l4->apply_unsafe($j);$l4->apply_unsafe($N);$l4->apply_unsafe($k);$l4->apply_unsafe($l);$l4->apply_unsafe($m);$l4->apply_unsafe($n);$l4->apply_unsafe($o);$l4->apply_unsafe($p);$l4->apply_unsafe($q);$l4->apply_unsafe($E);$l4->apply_unsafe($d);$l4->apply_unsafe($r);$l4->apply_unsafe($s);$l4->apply_unsafe($t);$l4->apply_unsafe($u);$l4->apply_unsafe($v);$l4->apply_unsafe($w);$l4->apply_unsafe($x);$l4->apply_unsafe($m4);$l4->apply_unsafe($z);$u4->apply_unsafe($Q);$u4->apply_unsafe($h);$u4->apply_unsafe($j);$u4->apply_unsafe($N);$u4->apply_unsafe($k);$u4->apply_unsafe($l);$u4->apply_unsafe($m);$u4->apply_unsafe($n);$u4->apply_unsafe($o);$u4->apply_unsafe($K);$u4->apply_unsafe($p);$u4->apply_unsafe($O);$u4->apply_unsafe($q);$u4->apply_unsafe($E);$u4->apply_unsafe($d);$u4->apply_unsafe($r);$u4->apply_unsafe($s);$u4->apply_unsafe($t);$u4->apply_unsafe($u);$u4->apply_unsafe($v);$u4->apply_unsafe($w);$u4->apply_unsafe($x);$u4->apply_unsafe($v4);$u4->apply_unsafe($z);$D4->apply_unsafe($Q);$D4->apply_unsafe($h);$D4->apply_unsafe($j);$D4->apply_unsafe($N);$D4->apply_unsafe($k);$D4->apply_unsafe($l);$D4->apply_unsafe($m);$D4->apply_unsafe($n);$D4->apply_unsafe($o);$D4->apply_unsafe($K);$D4->apply_unsafe($p);$D4->apply_unsafe($O);$D4->apply_unsafe($q);$D4->apply_unsafe($E);$D4->apply_unsafe($d);$D4->apply_unsafe($r);$D4->apply_unsafe($s);$D4->apply_unsafe($t);$D4->apply_unsafe($u);$D4->apply_unsafe($v);$D4->apply_unsafe($w);$D4->apply_unsafe($x);$D4->apply_unsafe($E4);$D4->apply_unsafe($z);$M4->apply_unsafe($Q);$M4->apply_unsafe($h);$M4->apply_unsafe($j);$M4->apply_unsafe($N);$M4->apply_unsafe($k);$M4->apply_unsafe($l);$M4->apply_unsafe($m);$M4->apply_unsafe($n);$M4->apply_unsafe($o);$M4->apply_unsafe($p);$M4->apply_unsafe($O);$M4->apply_unsafe($q);$M4->apply_unsafe($E);$M4->apply_unsafe($d);$M4->apply_unsafe($r);$M4->apply_unsafe($s);$M4->apply_unsafe($t);$M4->apply_unsafe($u);$M4->apply_unsafe($v);$M4->apply_unsafe($w);$M4->apply_unsafe($x);$M4->apply_unsafe($N4);$M4->apply_unsafe($z);$Z4->apply_unsafe($Q);$Z4->apply_unsafe($h);$Z4->apply_unsafe($j);$Z4->apply_unsafe($k);$Z4->apply_unsafe($l);$Z4->apply_unsafe($m);$Z4->apply_unsafe($n);$Z4->apply_unsafe($o);$Z4->apply_unsafe($p);$Z4->apply_unsafe($q);$Z4->apply_unsafe($E);$Z4->apply_unsafe($d);$Z4->apply_unsafe($r);$Z4->apply_unsafe($s);$Z4->apply_unsafe($t);$Z4->apply_unsafe($u);$Z4->apply_unsafe($v);$Z4->apply_unsafe($w);$Z4->apply_unsafe($x);$Z4->apply_unsafe($c5);$Z4->apply_unsafe($z);$o5->apply_unsafe($N);$C5->apply_unsafe($Q);$C5->apply_unsafe($h);$C5->apply_unsafe($j);$C5->apply_unsafe($N);$C5->apply_unsafe($k);$C5->apply_unsafe($l);$C5->apply_unsafe($m);$C5->apply_unsafe($n);$C5->apply_unsafe($o);$C5->apply_unsafe($p);$C5->apply_unsafe($q);$C5->apply_unsafe($E);$C5->apply_unsafe($d);$C5->apply_unsafe($r);$C5->apply_unsafe($s);$C5->apply_unsafe($t);$C5->apply_unsafe($u);$C5->apply_unsafe($v);$C5->apply_unsafe($w);$C5->apply_unsafe($x);$C5->apply_unsafe($D5);$C5->apply_unsafe($z);$O5->apply_unsafe($Q);$O5->apply_unsafe($h);$O5->apply_unsafe($j);$O5->apply_unsafe($N);$O5->apply_unsafe($k);$O5->apply_unsafe($l);$O5->apply_unsafe($m);$O5->apply_unsafe($n);$O5->apply_unsafe($o);$O5->apply_unsafe($p);$O5->apply_unsafe($q);$O5->apply_unsafe($E);$O5->apply_unsafe($d);$O5->apply_unsafe($r);$O5->apply_unsafe($s);$O5->apply_unsafe($t);$O5->apply_unsafe($u);$O5->apply_unsafe($v);$O5->apply_unsafe($w);$O5->apply_unsafe($x);$O5->apply_unsafe($P5);$O5->apply_unsafe($z);$c6->apply_unsafe($Q);$c6->apply_unsafe($h);$c6->apply_unsafe($j);$c6->apply_unsafe($k);$c6->apply_unsafe($l);$c6->apply_unsafe($m);$c6->apply_unsafe($n);$c6->apply_unsafe($o);$c6->apply_unsafe($p);$c6->apply_unsafe($q);$c6->apply_unsafe($d);$c6->apply_unsafe($r);$c6->apply_unsafe($s);$c6->apply_unsafe($t);$c6->apply_unsafe($u);$c6->apply_unsafe($v);$c6->apply_unsafe($w);$c6->apply_unsafe($x);$c6->apply_unsafe($d6);$c6->apply_unsafe($z);$F6->apply_unsafe($U);$S6->apply_unsafe($U);$p7->apply_unsafe($U);$B7->apply_unsafe($U);$O7->apply_unsafe($U);$h8->apply_unsafe($R);$o8->apply_unsafe($R);$h9->apply_unsafe($T);$Ia->apply_unsafe($T);$jc->apply_unsafe($fb);$jc->apply_unsafe($gb);$Yc->apply_unsafe($W);$Yc->apply_unsafe($X);$Yc->apply_unsafe($Y);$Yc->apply_unsafe($Z);$Yc->apply_unsafe($c1);$Yc->apply_unsafe($d1);$Yc->apply_unsafe($e1);$Yc->apply_unsafe($g1);$jd->apply_unsafe($W);$jd->apply_unsafe($X);$jd->apply_unsafe($Y);$jd->apply_unsafe($Z);$jd->apply_unsafe($c1);$jd->apply_unsafe($d1);$jd->apply_unsafe($e1);$jd->apply_unsafe($g1);$wd->apply_unsafe($W);$wd->apply_unsafe($X);$wd->apply_unsafe($Y);$wd->apply_unsafe($Z);$wd->apply_unsafe($c1);$wd->apply_unsafe($d1);$wd->apply_unsafe($e1);$wd->apply_unsafe($g1);$Hd->apply_unsafe($W);$Qd->apply_unsafe($W);$ne->apply_unsafe($X);$ve->apply_unsafe($X);$Ee->apply_unsafe($X);$Re->apply_unsafe($X);$Re->apply_unsafe($Y);$Re->apply_unsafe($Z);$Re->apply_unsafe($d1);$Re->apply_unsafe($e1);$jf->apply_unsafe($X);$Jf->apply_unsafe($Y);$Rf->apply_unsafe($Y);$gg->apply_unsafe($Y);$Bg->apply_unsafe($Z);$Jg->apply_unsafe($Z);$Ug->apply_unsafe($Z);$Kh->apply_unsafe($d1);$Sh->apply_unsafe($d1);$Xh->apply_unsafe($d1);$ki->apply_unsafe($d1);$Li->apply_unsafe($e1);$Ti->apply_unsafe($e1);$ej->apply_unsafe($e1);$pj->apply_unsafe($e1);$Mj->apply_unsafe($g1);$dk->apply_unsafe($g1);$ni::self=$zk;&$_($F)for@$G;&$_($I)for@$J;&$_($q1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$y1;&$_($E1)for@$G1;&$_($J1)for@$y1;&$_($M1)for@$y1;&$_($P1)for@$y1;&$_($S1)for@$T1;&$_($Z1)for@$y1;&$_($e2)for@$f2;&$_($h2)for@$i2;&$_($k2)for@$y1;&$_($n2)for@$p2;&$_($r2)for@$s2;&$_($u2)for@$v2;&$_($y2)for@$y1;&$_($A2)for@$y1;&$_($D2)for@$F2;&$_($H2)for@$y1;&$_($J2)for@$y1;&$_($R2)for@$S2;&$_($V2)for@$y1;&$_($Y2)for@$Z2;&$_($f3)for@$y1;&$_($i3)for@$j3;&$_($l3)for@$m3;&$_($q3)for@$y1;&$_($t3)for@$v3;&$_($x3)for@$y3;&$_($A3)for@$B3;&$_($G3)for@$H3;&$_($L3)for@$y1;&$_($O3)for@$P3;&$_($S3)for@$y1;&$_($V3)for@$W3;&$_($Y3)for@$Z3;&$_($g4)for@$y1;&$_($i4)for@$y1;&$_($l4)for@$n4;&$_($r4)for@$y1;&$_($u4)for@$w4;&$_($A4)for@$y1;&$_($D4)for@$F4;&$_($J4)for@$y1;&$_($M4)for@$O4;&$_($Q4)for@$R4;&$_($U4)for@$y1;&$_($W4)for@$y1;&$_($Z4)for@$d5;&$_($g5)for@$h5;&$_($l5)for@$y1;&$_($o5)for@$p5;&$_($r5)for@$s5;&$_($z5)for@$y1;&$_($C5)for@$E5;&$_($I5)for@$y1;&$_($L5)for@$y1;&$_($O5)for@$Q5;&$_($S5)for@$T5;&$_($X5)for@$y1;&$_($c6)for@$e6;&$_($g6)for@$h6;&$_($j6)for@$k6;&$_($n6)for@$o6;&$_($r6)for@$s6;&$_($y6)for@$y1;&$_($C6)for@$y1;&$_($F6)for@$G6;&$_($L6)for@$y1;&$_($P6)for@$y1;&$_($S6)for@$T6;&$_($Y6)for@$y1;&$_($e7)for@$y1;&$_($i7)for@$y1;&$_($m7)for@$y1;&$_($p7)for@$q7;&$_($u7)for@$y1;&$_($y7)for@$y1;&$_($B7)for@$C7;&$_($H7)for@$y1;&$_($L7)for@$y1;&$_($O7)for@$P7;&$_($R7)for@$S7;&$_($X7)for@$Y7;&$_($e8)for@$y1;&$_($h8)for@$i8;&$_($l8)for@$y1;&$_($o8)for@$p8;&$_($r8)for@$s8;&$_($x8)for@$y8;&$_($W8)for@$X8;&$_($e9)for@$y1;&$_($h9)for@$i9;&$_($n9)for@$y1;&$_($r9)for@$y1;&$_($v9)for@$y1;&$_($z9)for@$y1;&$_($D9)for@$y1;&$_($H9)for@$y1;&$_($L9)for@$y1;&$_($P9)for@$y1;&$_($T9)for@$y1;&$_($X9)for@$y1;&$_($da)for@$y1;&$_($ha)for@$y1;&$_($la)for@$y1;&$_($pa)for@$y1;&$_($ta)for@$y1;&$_($xa)for@$y1;&$_($Ba)for@$y1;&$_($Fa)for@$y1;&$_($Ia)for@$Ja;&$_($La)for@$Ma;&$_($mb)for@$y1;&$_($qb)for@$y1;&$_($ub)for@$y1;&$_($yb)for@$y1;&$_($Cb)for@$y1;&$_($Gb)for@$y1;&$_($Kb)for@$y1;&$_($Ob)for@$y1;&$_($Sb)for@$y1;&$_($Wb)for@$y1;&$_($cc)for@$y1;&$_($gc)for@$y1;&$_($jc)for@$kc;&$_($mc)for@$nc;&$_($Kc)for@$Lc;&$_($Nc)for@$Oc;&$_($Vc)for@$y1;&$_($Yc)for@$Zc;&$_($gd)for@$y1;&$_($jd)for@$kd;&$_($pd)for@$y1;&$_($td)for@$y1;&$_($wd)for@$xd;&$_($zd)for@$Ad;&$_($Ed)for@$y1;&$_($Hd)for@$Id;&$_($Nd)for@$y1;&$_($Qd)for@$Rd;&$_($Td)for@$Ud;&$_($de)for@$ee;&$_($ke)for@$y1;&$_($ne)for@$oe;&$_($se)for@$y1;&$_($ve)for@$we;&$_($Be)for@$y1;&$_($Ee)for@$Fe;&$_($Le)for@$y1;&$_($Oe)for@$y1;&$_($Re)for@$Se;&$_($Ue)for@$Ve;&$_($cf)for@$y1;&$_($gf)for@$y1;&$_($jf)for@$kf;&$_($mf)for@$nf;&$_($xf)for@$yf;&$_($Df)for@$y1;&$_($Gf)for@$y1;&$_($Jf)for@$Kf;&$_($Of)for@$y1;&$_($Rf)for@$Sf;&$_($Xf)for@$y1;&$_($dg)for@$y1;&$_($gg)for@$hg;&$_($jg)for@$kg;&$_($sg)for@$tg;&$_($yg)for@$y1;&$_($Bg)for@$Cg;&$_($Gg)for@$y1;&$_($Jg)for@$Kg;&$_($Og)for@$y1;&$_($Rg)for@$y1;&$_($Ug)for@$Vg;&$_($Xg)for@$Yg;&$_($oh)for@$ph;&$_($vh)for@$y1;&$_($zh)for@$y1;&$_($Dh)for@$y1;&$_($Hh)for@$y1;&$_($Kh)for@$Lh;&$_($Ph)for@$y1;&$_($Sh)for@$Th;&$_($Xh)for@$Yh;&$_($ei)for@$y1;&$_($hi)for@$y1;&$_($ki)for@$li;&$_($ni)for@$oi;&$_($zi)for@$Ai;&$_($Fi)for@$y1;&$_($Ii)for@$y1;&$_($Li)for@$Mi;&$_($Qi)for@$y1;&$_($Ti)for@$Ui;&$_($Zi)for@$y1;&$_($ej)for@$fj;&$_($jj)for@$y1;&$_($mj)for@$y1;&$_($pj)for@$qj;&$_($tj)for@$uj;&$_($Dj)for@$Ej;&$_($Jj)for@$y1;&$_($Mj)for@$Nj;&$_($Rj)for@$y1;&$_($Vj)for@$y1;&$_($Yj)for@$y1;&$_($dk)for@$ek;&$_($gk)for@$hk;&$_($pk)for@$y1;&$_($tk)for@$y1;&$_($xk)for@$y1;&$_($zk)for@$Ak;ni->run(@ARGV);
__DATA__
