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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fifo.c';$u=q'/unix/file.c';$v=q'/unix/io.c';$w=q'/unix/pid.c';$x={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$y={$p,1};$z={$j,1,$k,1,$p,1,$q,1};$A=[undef];$B=q'/metaclass';$C=bless({$c,$z,$f,$j,$g,$A},$B);$D=q'/metaclass::ctors';$E=[$C];$F=bless({$c,$y,$f,$p,$g,$E},$B);$G=q'/metaclass::ctors';$H=q'/lib/slice';$I={$H,1};$J=q'/lib/behavior';$K=q'/lib/branch';$L=q'/lib/tag';$M={$J,1,$K,1,$H,1,$L,1};$N=q'/class';$O=q'/object';$P={$N,1,$h,1,$J,1,$j,1,$K,1,$k,1,$l,1,$m,1,$n,1,$o,1,$H,1,$p,1,$L,1,$q,1,$B,1,$d,1,$O,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$Q={};$R=q'ctor';$S=undef;$T=q'dtor';$U=q'methods';$V=q'class';$W={$m,1};$X=[undef];$Y=bless({$c,$W,$f,$m,$g,$X},$B);$Z=q'/metaclass::ctors';$c1=q'/lib/fn';$d1={$c1,1};$e1={};$f1=q'DESTROY';$g1=q'code';$h1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$i1=bless({$g1,$h1},$c1);$j1=q'/lib/fn::ctors';$k1=q'new';$l1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$m1=bless({$g1,$l1},$c1);$n1={$f1,$i1,$k1,$m1};$o1=q'/lib/instantiable.b';$p1=bless({$c,$e1,$U,$n1,$f,$o1},$H);$q1=q'/lib/slice::ctors';$r1={};$s1=q'shift->compile';$t1=bless({$g1,$s1},$c1);$u1=q'compile';$v1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$w1=bless({$g1,$v1},$c1);$x1=q'instantiate';$y1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$z1=bless({$g1,$y1},$c1);$A1={$u1,$w1,$x1,$z1};$B1=q'/lib/fn_init.b';$C1=bless({$c,$r1,$R,$t1,$T,$S,$U,$A1,$f,$B1},$H);$D1=q'/lib/slice::ctors';$E1={};$F1=q'serialize';$G1=q'annotations';$H1=[];$I1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$J1=bless({$G1,$H1,$g1,$I1},$c1);$K1={$F1,$J1};$L1=q'/lib/fn_serialize.b';$M1=bless({$c,$E1,$R,$S,$T,$S,$U,$K1,$f,$L1},$H);$N1=q'/lib/slice::ctors';$O1=[$p1,$C1,$M1];$P1=bless({$c,$d1,$f,$c1,$g,$O1},$m);$Q1=q'/lib/fn.c::ctors';$R1=q'ni \'ni:\' . ref shift';$S1=bless({$g1,$R1},$c1);$T1={$V,$S1};$U1=q'/lib/instance.b';$V1=bless({$c,$Q,$R,$S,$T,$S,$U,$T1,$f,$U1},$H);$W1=q'/lib/slice::ctors';$X1=[$V1];$Y1=bless({$c,$P,$f,$O,$g,$X1},$r);$Z1=q'/object.c::ctors';$c2=[$Y1];$d2=bless({$c,$M,$f,$J,$g,$c2},$j);$e2=q'/lib/behavior.c::ctors';$f2={};$g2=q'my $s = shift; ni->def($s->name, $s)';$h2=bless({$g1,$g2},$c1);$i2=q'$_[0]->namespace . ":" . $_[0]->{name}';$j2=bless({$g1,$i2},$c1);$k2={$f,$j2};$l2=q'/lib/named.b';$m2=bless({$c,$f2,$R,$h2,$T,$S,$U,$k2,$f,$l2},$H);$n2=q'/lib/doc';$o2=q'/lib/slice::ctors';$p2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$q2=bless({$g1,$p2},$c1);$r2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$s2=bless({$g1,$r2},$c1);$t2=q'/lib/slice::apply';$u2=q'/lib/slice::apply_unsafe';$v2={};$w2=q'apply';$x2=q'apply_unsafe';$y2={$w2,$q2,$x2,$s2};$z2=q'/lib/slice.b';$A2=bless({$c,$v2,$U,$y2,$f,$z2},$H);$B2=q'/lib/slice::ctors';$C2={};$D2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$E2=bless({$g1,$D2},$c1);$F2={$x1,$E2};$G2=q'/lib/slice_init.b';$H2=bless({$c,$C2,$U,$F2,$f,$G2},$H);$I2=q'/lib/slice::ctors';$J2={};$K2=[];$L2=q'local $_;
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
$g;';$M2=bless({$G1,$K2,$g1,$L2},$c1);$N2={$F1,$M2};$O2=q'/lib/slice_serialize.b';$P2=bless({$c,$J2,$R,$S,$T,$S,$U,$N2,$f,$O2},$H);$Q2=q'/lib/slice::ctors';$R2=[$d2,$m2,$A2,$H2,$P2];$S2=bless({$c,$I,$f,$H,$g,$R2},$p);$T2=q'/lib/slice.c::ctors';$U2={};$V2=q'doc';$W2=[];$X2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$Y2=bless({$G1,$W2,$g1,$X2},$c1);$Z2={$V2,$Y2};$c3=q'/lib/documentable.b';$d3=bless({$c,$U2,$R,$S,$T,$S,$U,$Z2,$f,$c3},$H);$e3=q'/lib/slice::ctors';$f3=[undef,$d3];$g3=bless({$c,$x,$f,$r,$g,$f3},$B);$h3=q'/metaclass::ctors';$i3=[$g3];$j3=bless({$c,$i,$f,$h,$g,$i3},$B);$k3=q'/metaclass::ctors';$l3={$N,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$m3={$q,1};$n3=[$C];$o3=bless({$c,$m3,$f,$q,$g,$n3},$B);$p3=q'/metaclass::ctors';$q3={$L,1};$r3={};$s3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$t3=bless({$g1,$s3},$c1);$u3={$w2,$t3};$v3=q'/lib/tag.b';$w3=bless({$c,$r3,$R,$S,$T,$S,$U,$u3,$f,$v3},$H);$x3=q'/lib/slice::ctors';$y3={};$z3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$A3=bless({$g1,$z3},$c1);$B3={$x1,$A3};$C3=q'/lib/tag_init.b';$D3=bless({$c,$y3,$R,$S,$T,$S,$U,$B3,$f,$C3},$H);$E3=q'/lib/slice::ctors';$F3=[$d2,$m2,$w3,$D3];$G3=bless({$c,$q3,$f,$L,$g,$F3},$q);$H3=q'/lib/tag.c::ctors';$I3=q'/lib/perlbranch.b';$J3={};$K3=q'add';$L3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$M3=bless({$g1,$L3},$c1);$N3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$O3=bless({$g1,$N3},$c1);$P3={$K3,$M3,$w2,$O3};$Q3=q'/lib/branch.b';$R3=bless({$c,$J3,$R,$S,$T,$S,$U,$P3,$f,$Q3},$H);$S3=q'/lib/slice::ctors';$T3={};$U3=q'namespace';$V3=q'\'ni\'';$W3=bless({$g1,$V3},$c1);$X3={$U3,$W3};$Y3=q'/lib/named_in_ni.b';$Z3=bless({$c,$T3,$R,$S,$T,$S,$U,$X3,$f,$Y3},$H);$c4=q'/lib/slice::ctors';$d4={};$e4=q'package';$f4=q'shift->{name}';$g4=bless({$g1,$f4},$c1);$h4={$e4,$g4};$i4=q'/lib/namespaced.b';$j4=bless({$c,$d4,$R,$S,$T,$S,$U,$h4,$f,$i4},$H);$k4=q'/lib/slice::ctors';$l4={};$m4=q'resolve';$n4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$o4=bless({$g1,$n4},$c1);$p4={$m4,$o4};$q4=q'/lib/resolver.b';$r4=bless({$c,$l4,$R,$S,$T,$S,$U,$p4,$f,$q4},$H);$s4=q'/lib/slice::ctors';$t4=[$R3,$p1,$m2,$Z3,$j4,$r4];$u4=bless({$f,$I3,$g,$t4},$L);$v4=q'/lib/tag::ctors';$w4={};$x4=q'my $s = shift; $s->apply($s->package)';$y4=bless({$g1,$x4},$c1);$z4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$A4=bless({$g1,$z4},$c1);$B4={$x1,$A4};$C4=q'/lib/class_init.b';$D4=bless({$c,$w4,$R,$y4,$T,$S,$U,$B4,$f,$C4},$H);$E4=q'/lib/slice::ctors';$F4={$k,1};$G4=[$C];$H4=bless({$c,$F4,$f,$k,$g,$G4},$B);$I4=q'/metaclass::ctors';$J4={$K,1};$K4={};$L4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$M4=bless({$g1,$L4},$c1);$N4={$x1,$M4};$O4=q'/lib/branch_init.b';$P4=bless({$c,$K4,$R,$S,$T,$S,$U,$N4,$f,$O4},$H);$Q4=q'/lib/slice::ctors';$R4=[$d2,$m2,$R3,$P4,undef];$S4=bless({$c,$J4,$f,$K,$g,$R4},$k);$T4=q'/lib/branch.c::ctors';$U4={$N,1,$h,1,$j,1,$K,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$B,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$V4=q'/lib/definition.b';$W4={};$X4=q'def';$Y4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$Z4=bless({$g1,$Y4},$c1);$c5={$X4,$Z4};$d5=q'/lib/classdef.b';$e5=bless({$c,$W4,$R,$S,$T,$S,$U,$c5,$f,$d5},$H);$f5=q'/lib/slice::ctors';$g5={};$h5=q'ro';$i5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$j5=bless({$g1,$i5},$c1);$k5=q'rw';$l5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$m5=bless({$g1,$l5},$c1);$n5={$h5,$j5,$k5,$m5};$o5=q'/lib/accessor.b';$p5=bless({$c,$g5,$R,$S,$T,$S,$U,$n5,$f,$o5},$H);$q5=q'/lib/slice::ctors';$r5=[$e5,$p5];$s5=bless({$c,$U4,$f,$V4,$g,$r5},$K);$t5=q'/lib/branch::ctors';$u5={};$v5=q'child';$w5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$x5=bless({$g1,$w5},$c1);$y5={$v5,$x5};$z5=q'/lib/subclass.b';$A5=bless({$c,$u5,$R,$S,$T,$S,$U,$y5,$f,$z5},$H);$B5=q'/lib/slice::ctors';$C5=[$u4,$D4,$Y1,$s5,$A5];$D5=bless({$c,$l3,$f,$N,$g,$C5},$h);$E5=q'/class.c::ctors';$F5=[$D5];$G5=bless({$c,$e,$f,$d,$g,$F5},$B);$H5=q'/metaclass::ctors';$I5={$B,1};$J5=[$u4,$D4,$Y1,$s5];$K5=bless({$c,$I5,$f,$B,$g,$J5},$d);$L5=q'/metaclass.c::ctors';$M5={$o,1};$N5=[$g3];$O5=bless({$c,$M5,$f,$o,$g,$N5},$B);$P5=q'/metaclass::ctors';$Q5=q'/lib/ni';$R5={$Q5,1};$S5={};$T5=q'is_mutable';$U5=[];$V5=q'$0 ne "-" && -w $0';$W5=bless({$G1,$U5,$g1,$V5},$c1);$X5=q'modify';$Y5=[];$Z5=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$c6=bless({$G1,$Y5,$g1,$Z5},$c1);$d6={$T5,$W5,$X5,$c6};$e6=q'/lib/ni_self.b';$f6=bless({$c,$S5,$R,$S,$T,$S,$U,$d6,$f,$e6},$H);$g6=q'/lib/slice::ctors';$h6={};$i6=q'exists';$j6=[];$k6=q'exists $_[0]->{named}{$_[1]}';$l6=bless({$G1,$j6,$g1,$k6},$c1);$m6=q'quoted';$n6=[];$o6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$p6=bless({$G1,$n6,$g1,$o6},$c1);$q6={$i6,$l6,$m6,$p6};$r6=q'/lib/ni_image.b';$s6=bless({$c,$h6,$R,$S,$T,$S,$U,$q6,$f,$r6},$H);$t6=q'/lib/slice::ctors';$u6={};$v6=q'--internal/+=';$w6=[];$x6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$y6=bless({$G1,$w6,$g1,$x6},$c1);$z6=q'--internal/eval';$A6=[];$B6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$C6=bless({$G1,$A6,$g1,$B6},$c1);$D6=q'--internal/image';$E6=[];$F6=q'shift->quoted->write(\\*STDOUT);
0;';$G6=bless({$G1,$E6,$g1,$F6},$c1);$H6=q'run';$I6=[];$J6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$K6=bless({$G1,$I6,$g1,$J6},$c1);$L6={$v6,$y6,$z6,$C6,$D6,$G6,$H6,$K6};$M6=q'/lib/ni_main.b';$N6=bless({$c,$u6,$R,$S,$T,$S,$U,$L6,$f,$M6},$H);$O6=q'/lib/slice::ctors';$P6={};$Q6=[];$R6=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$S6=bless({$G1,$Q6,$g1,$R6},$c1);$T6=q'resolver_for';$U6=[];$V6=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$W6=bless({$G1,$U6,$g1,$V6},$c1);$X6={$m4,$S6,$T6,$W6};$Y6=q'/lib/ni_resolver.b';$Z6=bless({$c,$P6,$R,$S,$T,$S,$U,$X6,$f,$Y6},$H);$c7=q'/lib/slice::ctors';$d7=[$f6,$s6,$N6,$Z6];$e7=bless({$c,$R5,$f,$Q5,$g,$d7},$o);$f7=q'/lib/ni.c::ctors';$g7=q'named';$h7=q'ni.doc:/class';$i7={$l,1};$j7=[$g3];$k7=bless({$c,$i7,$f,$l,$g,$j7},$B);$l7=q'/metaclass::ctors';$m7={$n2,1};$n7={};$o7=[];$p7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$q7=bless({$G1,$o7,$g1,$p7},$c1);$r7={$x1,$q7};$s7=q'/lib/doc_init.b';$t7=bless({$c,$n7,$R,$S,$T,$S,$U,$r7,$f,$s7},$H);$u7=q'/lib/slice::ctors';$v7={};$w7=[];$x7=q'\'ni.doc\'';$y7=bless({$G1,$w7,$g1,$x7},$c1);$z7={$U3,$y7};$A7=q'/lib/doc_namespace.b';$B7=bless({$c,$v7,$R,$S,$T,$S,$U,$z7,$f,$A7},$H);$C7=q'/lib/slice::ctors';$D7=[$m2,$t7,$B7];$E7=bless({$c,$m7,$f,$n2,$g,$D7},$l);$F7=q'/lib/doc.c::ctors';$G7=q'apropos';$H7=q'ni:/class';$I7=[$H7];$J7=q'# Classes and metaclasses
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
';$K7=bless({$G7,$I7,$V2,$J7,$f,$N},$n2);$L7=q'/lib/doc::ctors';$M7=q'ni:/class.c';$N7=q'ni:/lib/accessor.b';$O7=q'ni:/lib/behavior';$P7=q'ni:/lib/behavior.c';$Q7=q'ni:/lib/branch';$R7=q'ni:/lib/branch.b';$S7=q'ni:/lib/branch.c';$T7=q'ni:/lib/branch_init.b';$U7=q'ni:/lib/class_init.b';$V7=q'ni:/lib/classdef.b';$W7=q'ni:/lib/definition.b';$X7=q'ni:/lib/doc';$Y7=q'ni:/lib/doc.c';$Z7=q'ni:/lib/doc_init.b';$c8=q'ni:/lib/doc_namespace.b';$d8=q'ni:/lib/documentable.b';$e8=q'ni:/lib/fn';$f8=q'ni:/lib/fn.c';$g8=q'ni:/lib/fn_init.b';$h8=q'ni:/lib/fn_serialize.b';$i8=q'ni:/lib/image';$j8={$n,1};$k8=[$g3];$l8=bless({$c,$j8,$f,$n,$g,$k8},$B);$m8=q'/metaclass::ctors';$n8=q'/lib/image';$o8={$n8,1};$p8={};$q8=[];$r8=q'my $class = shift;
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
  ordering     => []};';$s8=bless({$G1,$q8,$g1,$r8},$c1);$t8={$x1,$s8};$u8=q'/lib/image_init.b';$v8=bless({$c,$p8,$R,$S,$T,$S,$U,$t8,$f,$u8},$H);$w8=q'/lib/slice::ctors';$x8={};$y8=q'address';$z8=[];$A8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$B8=bless({$G1,$z8,$g1,$A8},$c1);$C8=q'allocate_gensym';$D8=[];$E8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$F8=bless({$G1,$D8,$g1,$E8},$c1);$G8=q'boot_side_effect';$H8=[];$I8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$J8=bless({$G1,$H8,$g1,$I8},$c1);$K8=q'circular_links';$L8=[];$M8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$N8=bless({$G1,$L8,$g1,$M8},$c1);$O8=q'finalizer';$P8=[];$Q8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$R8=bless({$G1,$P8,$g1,$Q8},$c1);$S8=q'gensym';$T8=[];$U8=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$V8=bless({$G1,$T8,$g1,$U8},$c1);$W8=q'is_circular';$X8=[];$Y8=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$Z8=bless({$G1,$X8,$g1,$Y8},$c1);$c9=q'quote';$d9=[];$e9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$f9=bless({$G1,$d9,$g1,$e9},$c1);$g9=q'quote_array';$h9=[];$i9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$j9=bless({$G1,$h9,$g1,$i9},$c1);$k9=q'quote_blessed';$l9=[];$m9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$n9=bless({$G1,$l9,$g1,$m9},$c1);$o9=q'quote_class';$p9=[];$q9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$r9=bless({$G1,$p9,$g1,$q9},$c1);$s9=q'quote_hash';$t9=[];$u9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$v9=bless({$G1,$t9,$g1,$u9},$c1);$w9=q'quote_object';$x9=[];$y9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$z9=bless({$G1,$x9,$g1,$y9},$c1);$A9=q'quote_scalar';$B9=[];$C9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$D9=bless({$G1,$B9,$g1,$C9},$c1);$E9=q'quote_value';$F9=[];$G9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$H9=bless({$G1,$F9,$g1,$G9},$c1);$I9=q'reconstruction';$J9=[];$K9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$L9=bless({$G1,$J9,$g1,$K9},$c1);$M9=q'side_effect';$N9=[];$O9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$P9=bless({$G1,$N9,$g1,$O9},$c1);$Q9=q'write';$R9=[];$S9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$T9=bless({$G1,$R9,$g1,$S9},$c1);$U9={$y8,$B8,$C8,$F8,$G8,$J8,$K8,$N8,$O8,$R8,$S8,$V8,$W8,$Z8,$c9,$f9,$g9,$j9,$k9,$n9,$o9,$r9,$s9,$v9,$w9,$z9,$A9,$D9,$E9,$H9,$I9,$L9,$M9,$P9,$Q9,$T9};$V9=q'/lib/image_quoting.b';$W9=bless({$c,$x8,$R,$S,$T,$S,$U,$U9,$f,$V9},$H);$X9=q'/lib/slice::ctors';$Y9=[$v8,$W9];$Z9=bless({$c,$o8,$f,$n8,$g,$Y9},$n);$ca=q'/lib/image.c::ctors';$da=q'ni:/lib/image.c';$ea=q'ni:/lib/image_init.b';$fa=q'ni:/lib/image_quoting.b';$ga=q'ni:/lib/instance.b';$ha=q'ni:/lib/instantiable.b';$ia=q'ni:/lib/named.b';$ja=q'ni:/lib/named_in_ni.b';$ka=q'ni:/lib/namespaced.b';$la=q'ni:/lib/ni';$ma=q'ni:/lib/ni.c';$na=q'ni:/lib/ni_image.b';$oa=q'ni:/lib/ni_main.b';$pa=q'ni:/lib/ni_resolver.b';$qa=q'ni:/lib/ni_self.b';$ra=q'ni:/lib/perlbranch.b';$sa=q'ni:/lib/resolver.b';$ta=q'ni:/lib/slice';$ua=q'ni:/lib/slice.b';$va=q'ni:/lib/slice.c';$wa=q'ni:/lib/slice_init.b';$xa=q'ni:/lib/slice_serialize.b';$ya=q'ni:/lib/subclass.b';$za=q'ni:/lib/tag';$Aa=q'ni:/lib/tag.b';$Ba=q'ni:/lib/tag.c';$Ca=q'ni:/lib/tag_init.b';$Da=q'ni:/metaclass';$Ea=q'ni:/metaclass.c';$Fa=q'ni:/object';$Ga=q'ni:/object.c';$Ha=q'ni:/unix/cat';$Ia={$s,1};$Ja={$s,1,$u,1,$v,1,$w,1};$Ka=[$g3];$La=bless({$c,$Ja,$f,$v,$g,$Ka},$B);$Ma=q'/metaclass::ctors';$Na=[$La];$Oa=bless({$c,$Ia,$f,$s,$g,$Na},$B);$Pa=q'/metaclass::ctors';$Qa=q'/unix/cat';$Ra={$Qa,1};$Sa={};$Ta=[];$Ua=q'shift; [@_]';$Va=bless({$G1,$Ta,$g1,$Ua},$c1);$Wa={$x1,$Va};$Xa=q'/unix/cat_init.b';$Ya=bless({$c,$Sa,$R,$S,$T,$S,$U,$Wa,$f,$Xa},$H);$Za=q'/lib/slice::ctors';$cb={};$db=q'read';$eb=[];$fb=q'my $self = shift;
my $n;
shift @$self until !@$self or $n = $$self[0]->read(@_);
return undef;';$gb=bless({$G1,$eb,$g1,$fb},$c1);$hb={$db,$gb};$ib=q'/unix/cat_read.b';$jb=bless({$c,$cb,$R,$S,$T,$S,$U,$hb,$f,$ib},$H);$kb=q'/lib/slice::ctors';$lb=[$Ya,$jb];$mb=bless({$c,$Ra,$f,$Qa,$g,$lb},$s);$nb=q'/unix/cat.c::ctors';$ob=q'ni:/unix/cat.c';$pb=q'ni:/unix/cat_init.b';$qb=q'ni:/unix/cat_read.b';$rb=q'ni:/unix/fd.b';$sb=q'/unix/fifo';$tb=q'/unix/file';$ub={$sb,1,$tb,1};$vb=q'/unix/fd.b';$wb={};$xb=[];$yb=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$zb=bless({$G1,$xb,$g1,$yb},$c1);$Ab=[];$Bb=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Cb=bless({$G1,$Ab,$g1,$Bb},$c1);$Db={$db,$zb,$Q9,$Cb};$Eb=q'/unix/fd_safeio.b';$Fb=bless({$c,$wb,$R,$S,$T,$S,$U,$Db,$f,$Eb},$H);$Gb=q'/lib/slice::ctors';$Hb=[$Fb];$Ib=bless({$c,$ub,$f,$vb,$g,$Hb},$K);$Jb=q'/lib/branch::ctors';$Kb=q'ni:/unix/fd_safeio.b';$Lb=q'ni:/unix/fifo';$Mb={$t,1};$Nb=[$g3];$Ob=bless({$c,$Mb,$f,$t,$g,$Nb},$B);$Pb=q'/metaclass::ctors';$Qb={$sb,1};$Rb={};$Sb=[];$Tb=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Ub=bless({$G1,$Sb,$g1,$Tb},$c1);$Vb={$x1,$Ub};$Wb=q'/unix/fifo_init.b';$Xb=bless({$c,$Rb,$R,$S,$T,$S,$U,$Vb,$f,$Wb},$H);$Yb=q'/lib/slice::ctors';$Zb={};$cc=q'read_fh';$dc=[];$ec=q'shift->{read_fh}';$fc=bless({$G1,$dc,$g1,$ec},$c1);$gc=q'write_fh';$hc=[];$ic=q'shift->{write_fh}';$jc=bless({$G1,$hc,$g1,$ic},$c1);$kc={$cc,$fc,$gc,$jc};$lc=q'/unix/file_io.b';$mc=bless({$c,$Zb,$R,$S,$T,$S,$U,$kc,$f,$lc},$H);$nc=q'/lib/slice::ctors';$oc=[$Xb,$Ib,$mc];$pc=bless({$c,$Qb,$f,$sb,$g,$oc},$t);$qc=q'/unix/fifo.c::ctors';$rc=q'ni:/unix/fifo.c';$sc=q'ni:/unix/fifo_init.b';$tc=q'ni:/unix/file';$uc={$u,1};$vc=[$La];$wc=bless({$c,$uc,$f,$u,$g,$vc},$B);$xc=q'/metaclass::ctors';$yc={$tb,1};$zc={};$Ac=[];$Bc=q'shift->{\'name\'}';$Cc=bless({$G1,$Ac,$g1,$Bc},$c1);$Dc={$f,$Cc};$Ec=q'/unix/file_readers.b';$Fc=bless({$c,$zc,$R,$S,$T,$S,$U,$Dc,$f,$Ec},$H);$Gc=q'/lib/slice::ctors';$Hc={};$Ic=[];$Jc=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Kc=bless({$G1,$Ic,$g1,$Jc},$c1);$Lc={$x1,$Kc};$Mc=q'/unix/file_init.b';$Nc=bless({$c,$Hc,$R,$S,$T,$S,$U,$Lc,$f,$Mc},$H);$Oc=q'/lib/slice::ctors';$Pc={};$Qc=[];$Rc=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Sc=bless({$G1,$Qc,$g1,$Rc},$c1);$Tc=[];$Uc=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Vc=bless({$G1,$Tc,$g1,$Uc},$c1);$Wc={$cc,$Sc,$gc,$Vc};$Xc=bless({$c,$Pc,$R,$S,$T,$S,$U,$Wc,$f,$lc},$H);$Yc=q'/lib/slice::ctors';$Zc=[$Fc,$Nc,$Ib,$Xc];$cd=bless({$c,$yc,$f,$tb,$g,$Zc},$u);$dd=q'/unix/file.c::ctors';$ed=q'ni:/unix/file.c';$fd=q'ni:/unix/file_init.b';$gd=q'ni:/unix/file_io.b';$hd=q'ni:/unix/file_readers.b';$id=q'ni:/unix/io';$jd=q'/unix/io';$kd={$jd,1};$ld={};$md=q'into';$nd=[];$od=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->read_size;
while (defined($self->read($_, $block_size))) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$pd=bless({$G1,$nd,$g1,$od},$c1);$qd={$md,$pd};$rd=q'/unix/io_stream.b';$sd=bless({$c,$ld,$R,$S,$T,$S,$U,$qd,$f,$rd},$H);$td=q'/lib/slice::ctors';$ud={};$vd=q'(+';$wd=[];$xd=q'ni(\'ni:/unix/cat\')->new(@_)';$yd=bless({$G1,$wd,$g1,$xd},$c1);$zd={$vd,$yd};$Ad=q'/unix/io_constructors.b';$Bd=bless({$c,$ud,$R,$S,$T,$S,$U,$zd,$f,$Ad},$H);$Cd=q'/lib/slice::ctors';$Dd={};$Ed=q'(<>';$Fd=[];$Gd=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Hd=bless({$G1,$Fd,$g1,$Gd},$c1);$Id=q'(@{}';$Jd=[];$Kd=q'my $self = shift; [<$self>]';$Ld=bless({$G1,$Jd,$g1,$Kd},$c1);$Md={$Ed,$Hd,$Id,$Ld};$Nd=q'/unix/io_readers.b';$Od=bless({$c,$Dd,$R,$S,$T,$S,$U,$Md,$f,$Nd},$H);$Pd=q'/lib/slice::ctors';$Qd=[$sd,$Bd,$Od];$Rd=bless({$c,$kd,$f,$jd,$g,$Qd},$v);$Sd=q'/unix/io.c::ctors';$Td=q'ni:/unix/io.c';$Ud=q'ni:/unix/io_constructors.b';$Vd=q'ni:/unix/io_readers.b';$Wd=q'ni:/unix/io_stream.b';$Xd=q'ni:/unix/pid';$Yd={$w,1};$Zd=[$La];$ce=bless({$c,$Yd,$f,$w,$g,$Zd},$B);$de=q'/metaclass::ctors';$ee=q'/unix/pid';$fe={$ee,1};$ge={};$he=q'pid';$ie=[];$je=q'shift->{\'pid\'}';$ke=bless({$G1,$ie,$g1,$je},$c1);$le=q'stderr';$me=[];$ne=q'shift->{\'stderr\'}';$oe=bless({$G1,$me,$g1,$ne},$c1);$pe=q'stdin';$qe=[];$re=q'shift->{\'stdin\'}';$se=bless({$G1,$qe,$g1,$re},$c1);$te=q'stdout';$ue=[];$ve=q'shift->{\'stdout\'}';$we=bless({$G1,$ue,$g1,$ve},$c1);$xe={$he,$ke,$le,$oe,$pe,$se,$te,$we};$ye=q'/unix/pid_readers.b';$ze=bless({$c,$ge,$R,$S,$T,$S,$U,$xe,$f,$ye},$H);$Ae=q'/lib/slice::ctors';$Be={};$Ce=[];$De=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Ee=bless({$G1,$Ce,$g1,$De},$c1);$Fe={$x1,$Ee};$Ge=q'/unix/pid_init.b';$He=bless({$c,$Be,$R,$S,$T,$S,$U,$Fe,$f,$Ge},$H);$Ie=q'/lib/slice::ctors';$Je={};$Ke=[];$Le=q'shift->{stdout}';$Me=bless({$G1,$Ke,$g1,$Le},$c1);$Ne=[];$Oe=q'shift->{stdin}';$Pe=bless({$G1,$Ne,$g1,$Oe},$c1);$Qe={$cc,$Me,$gc,$Pe};$Re=q'/unix/pid_io.b';$Se=bless({$c,$Je,$R,$S,$T,$S,$U,$Qe,$f,$Re},$H);$Te=q'/lib/slice::ctors';$Ue=[$ze,$He,$Se];$Ve=bless({$c,$fe,$f,$ee,$g,$Ue},$w);$We=q'/unix/pid.c::ctors';$Xe=q'ni:/unix/pid.c';$Ye=q'ni:/unix/pid_init.b';$Ze=q'ni:/unix/pid_io.b';$cf=q'ni:/unix/pid_readers.b';$df={$h7,$K7,$H7,$D5,$M7,$j3,$N7,$p5,$O7,$d2,$P7,$C,$Q7,$S4,$R7,$R3,$S7,$H4,$T7,$P4,$U7,$D4,$V7,$e5,$W7,$s5,$X7,$E7,$Y7,$k7,$Z7,$t7,$c8,$B7,$d8,$d3,$e8,$P1,$f8,$Y,$g8,$C1,$h8,$M1,$i8,$Z9,$da,$l8,$ea,$v8,$fa,$W9,$ga,$V1,$ha,$p1,$ia,$m2,$ja,$Z3,$ka,$j4,$la,$e7,$ma,$O5,$na,$s6,$oa,$N6,$pa,$Z6,$qa,$f6,$ra,$u4,$sa,$r4,$ta,$S2,$ua,$A2,$va,$F,$wa,$H2,$xa,$P2,$ya,$A5,$za,$G3,$Aa,$w3,$Ba,$o3,$Ca,$D3,$Da,$K5,$Ea,$G5,$Fa,$Y1,$Ga,$g3,$Ha,$mb,$ob,$Oa,$pb,$Ya,$qb,$jb,$rb,$Ib,$Kb,$Fb,$Lb,$pc,$rc,$Ob,$sc,$Xb,$tc,$cd,$ed,$wc,$fd,$Nc,$gd,$Xc,$hd,$Fc,$id,$Rd,$Td,$La,$Ud,$Bd,$Vd,$Od,$Wd,$sd,$Xd,$Ve,$Xe,$ce,$Ye,$He,$Ze,$Se,$cf,$ze};$ef=q'resolvers';$ff=q'file';$gf=[];$hf=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$if=bless({$G1,$gf,$g1,$hf},$c1);$jf={$ff,$if};$kf=bless({$g7,$df,$ef,$jf},$Q5);$lf=q'/lib/ni::ctors';$$f3[0]=$D5;$$A[0]=$g3;$$X[0]=$g3;$$R4[4]=$s5;*$u2=\&$s2;*$t2=\&$q2;$p1->apply_unsafe($N);$p1->apply_unsafe($h);$p1->apply_unsafe($j);$p1->apply_unsafe($K);$p1->apply_unsafe($k);$p1->apply_unsafe($l);$p1->apply_unsafe($c1);$p1->apply_unsafe($m);$p1->apply_unsafe($n);$p1->apply_unsafe($o);$p1->apply_unsafe($H);$p1->apply_unsafe($p);$p1->apply_unsafe($L);$p1->apply_unsafe($q);$p1->apply_unsafe($B);$p1->apply_unsafe($d);$p1->apply_unsafe($r);$p1->apply_unsafe($s);$p1->apply_unsafe($t);$p1->apply_unsafe($u);$p1->apply_unsafe($v);$p1->apply_unsafe($w);$C1->apply_unsafe($c1);$M1->apply_unsafe($c1);$V1->apply_unsafe($N);$V1->apply_unsafe($h);$V1->apply_unsafe($J);$V1->apply_unsafe($j);$V1->apply_unsafe($K);$V1->apply_unsafe($k);$V1->apply_unsafe($l);$V1->apply_unsafe($m);$V1->apply_unsafe($n);$V1->apply_unsafe($o);$V1->apply_unsafe($H);$V1->apply_unsafe($p);$V1->apply_unsafe($L);$V1->apply_unsafe($q);$V1->apply_unsafe($B);$V1->apply_unsafe($d);$V1->apply_unsafe($O);$V1->apply_unsafe($r);$V1->apply_unsafe($s);$V1->apply_unsafe($t);$V1->apply_unsafe($u);$V1->apply_unsafe($v);$V1->apply_unsafe($w);$m2->apply_unsafe($N);$m2->apply_unsafe($h);$m2->apply_unsafe($j);$m2->apply_unsafe($K);$m2->apply_unsafe($k);$m2->apply_unsafe($n2);$m2->apply_unsafe($l);$m2->apply_unsafe($m);$m2->apply_unsafe($n);$m2->apply_unsafe($o);$m2->apply_unsafe($H);$m2->apply_unsafe($p);$m2->apply_unsafe($L);$m2->apply_unsafe($q);$m2->apply_unsafe($B);$m2->apply_unsafe($d);$m2->apply_unsafe($r);$m2->apply_unsafe($s);$m2->apply_unsafe($t);$m2->apply_unsafe($u);$m2->apply_unsafe($v);$m2->apply_unsafe($w);$A2->apply_unsafe($H);$H2->apply_unsafe($H);$P2->apply_unsafe($H);$d3->apply_unsafe($h);$d3->apply_unsafe($j);$d3->apply_unsafe($k);$d3->apply_unsafe($l);$d3->apply_unsafe($m);$d3->apply_unsafe($n);$d3->apply_unsafe($o);$d3->apply_unsafe($p);$d3->apply_unsafe($q);$d3->apply_unsafe($r);$d3->apply_unsafe($s);$d3->apply_unsafe($t);$d3->apply_unsafe($u);$d3->apply_unsafe($v);$d3->apply_unsafe($w);$w3->apply_unsafe($L);$D3->apply_unsafe($L);$R3->apply_unsafe($N);$R3->apply_unsafe($h);$R3->apply_unsafe($j);$R3->apply_unsafe($K);$R3->apply_unsafe($k);$R3->apply_unsafe($l);$R3->apply_unsafe($m);$R3->apply_unsafe($n);$R3->apply_unsafe($o);$R3->apply_unsafe($p);$R3->apply_unsafe($q);$R3->apply_unsafe($B);$R3->apply_unsafe($d);$R3->apply_unsafe($r);$R3->apply_unsafe($s);$R3->apply_unsafe($t);$R3->apply_unsafe($u);$R3->apply_unsafe($v);$R3->apply_unsafe($w);$Z3->apply_unsafe($N);$Z3->apply_unsafe($h);$Z3->apply_unsafe($j);$Z3->apply_unsafe($K);$Z3->apply_unsafe($k);$Z3->apply_unsafe($l);$Z3->apply_unsafe($m);$Z3->apply_unsafe($n);$Z3->apply_unsafe($o);$Z3->apply_unsafe($H);$Z3->apply_unsafe($p);$Z3->apply_unsafe($L);$Z3->apply_unsafe($q);$Z3->apply_unsafe($B);$Z3->apply_unsafe($d);$Z3->apply_unsafe($r);$Z3->apply_unsafe($s);$Z3->apply_unsafe($t);$Z3->apply_unsafe($u);$Z3->apply_unsafe($v);$Z3->apply_unsafe($w);$j4->apply_unsafe($N);$j4->apply_unsafe($h);$j4->apply_unsafe($j);$j4->apply_unsafe($K);$j4->apply_unsafe($k);$j4->apply_unsafe($l);$j4->apply_unsafe($m);$j4->apply_unsafe($n);$j4->apply_unsafe($o);$j4->apply_unsafe($H);$j4->apply_unsafe($p);$j4->apply_unsafe($L);$j4->apply_unsafe($q);$j4->apply_unsafe($B);$j4->apply_unsafe($d);$j4->apply_unsafe($r);$j4->apply_unsafe($s);$j4->apply_unsafe($t);$j4->apply_unsafe($u);$j4->apply_unsafe($v);$j4->apply_unsafe($w);$r4->apply_unsafe($N);$r4->apply_unsafe($h);$r4->apply_unsafe($j);$r4->apply_unsafe($K);$r4->apply_unsafe($k);$r4->apply_unsafe($l);$r4->apply_unsafe($m);$r4->apply_unsafe($n);$r4->apply_unsafe($o);$r4->apply_unsafe($p);$r4->apply_unsafe($L);$r4->apply_unsafe($q);$r4->apply_unsafe($B);$r4->apply_unsafe($d);$r4->apply_unsafe($r);$r4->apply_unsafe($s);$r4->apply_unsafe($t);$r4->apply_unsafe($u);$r4->apply_unsafe($v);$r4->apply_unsafe($w);$D4->apply_unsafe($N);$D4->apply_unsafe($h);$D4->apply_unsafe($j);$D4->apply_unsafe($k);$D4->apply_unsafe($l);$D4->apply_unsafe($m);$D4->apply_unsafe($n);$D4->apply_unsafe($o);$D4->apply_unsafe($p);$D4->apply_unsafe($q);$D4->apply_unsafe($B);$D4->apply_unsafe($d);$D4->apply_unsafe($r);$D4->apply_unsafe($s);$D4->apply_unsafe($t);$D4->apply_unsafe($u);$D4->apply_unsafe($v);$D4->apply_unsafe($w);$P4->apply_unsafe($K);$e5->apply_unsafe($N);$e5->apply_unsafe($h);$e5->apply_unsafe($j);$e5->apply_unsafe($K);$e5->apply_unsafe($k);$e5->apply_unsafe($l);$e5->apply_unsafe($m);$e5->apply_unsafe($n);$e5->apply_unsafe($o);$e5->apply_unsafe($p);$e5->apply_unsafe($q);$e5->apply_unsafe($B);$e5->apply_unsafe($d);$e5->apply_unsafe($r);$e5->apply_unsafe($s);$e5->apply_unsafe($t);$e5->apply_unsafe($u);$e5->apply_unsafe($v);$e5->apply_unsafe($w);$p5->apply_unsafe($N);$p5->apply_unsafe($h);$p5->apply_unsafe($j);$p5->apply_unsafe($K);$p5->apply_unsafe($k);$p5->apply_unsafe($l);$p5->apply_unsafe($m);$p5->apply_unsafe($n);$p5->apply_unsafe($o);$p5->apply_unsafe($p);$p5->apply_unsafe($q);$p5->apply_unsafe($B);$p5->apply_unsafe($d);$p5->apply_unsafe($r);$p5->apply_unsafe($s);$p5->apply_unsafe($t);$p5->apply_unsafe($u);$p5->apply_unsafe($v);$p5->apply_unsafe($w);$A5->apply_unsafe($N);$A5->apply_unsafe($h);$A5->apply_unsafe($j);$A5->apply_unsafe($k);$A5->apply_unsafe($l);$A5->apply_unsafe($m);$A5->apply_unsafe($n);$A5->apply_unsafe($o);$A5->apply_unsafe($p);$A5->apply_unsafe($q);$A5->apply_unsafe($d);$A5->apply_unsafe($r);$A5->apply_unsafe($s);$A5->apply_unsafe($t);$A5->apply_unsafe($u);$A5->apply_unsafe($v);$A5->apply_unsafe($w);$f6->apply_unsafe($Q5);$s6->apply_unsafe($Q5);$N6->apply_unsafe($Q5);$Z6->apply_unsafe($Q5);$t7->apply_unsafe($n2);$B7->apply_unsafe($n2);$v8->apply_unsafe($n8);$W9->apply_unsafe($n8);$Ya->apply_unsafe($Qa);$jb->apply_unsafe($Qa);$Fb->apply_unsafe($sb);$Fb->apply_unsafe($tb);$Xb->apply_unsafe($sb);$mc->apply_unsafe($sb);$Fc->apply_unsafe($tb);$Nc->apply_unsafe($tb);$Xc->apply_unsafe($tb);$sd->apply_unsafe($jd);$Bd->apply_unsafe($jd);$Od->apply_unsafe($jd);$ze->apply_unsafe($ee);$He->apply_unsafe($ee);$Se->apply_unsafe($ee);$ni::self=$kf;&$_($C)for@$D;&$_($F)for@$G;&$_($Y)for@$Z;&$_($i1)for@$j1;&$_($m1)for@$j1;&$_($p1)for@$q1;&$_($t1)for@$j1;&$_($w1)for@$j1;&$_($z1)for@$j1;&$_($C1)for@$D1;&$_($J1)for@$j1;&$_($M1)for@$N1;&$_($P1)for@$Q1;&$_($S1)for@$j1;&$_($V1)for@$W1;&$_($Y1)for@$Z1;&$_($d2)for@$e2;&$_($h2)for@$j1;&$_($j2)for@$j1;&$_($m2)for@$o2;&$_($q2)for@$j1;&$_($s2)for@$j1;&$_($A2)for@$B2;&$_($E2)for@$j1;&$_($H2)for@$I2;&$_($M2)for@$j1;&$_($P2)for@$Q2;&$_($S2)for@$T2;&$_($Y2)for@$j1;&$_($d3)for@$e3;&$_($g3)for@$h3;&$_($j3)for@$k3;&$_($o3)for@$p3;&$_($t3)for@$j1;&$_($w3)for@$x3;&$_($A3)for@$j1;&$_($D3)for@$E3;&$_($G3)for@$H3;&$_($M3)for@$j1;&$_($O3)for@$j1;&$_($R3)for@$S3;&$_($W3)for@$j1;&$_($Z3)for@$c4;&$_($g4)for@$j1;&$_($j4)for@$k4;&$_($o4)for@$j1;&$_($r4)for@$s4;&$_($u4)for@$v4;&$_($y4)for@$j1;&$_($A4)for@$j1;&$_($D4)for@$E4;&$_($H4)for@$I4;&$_($M4)for@$j1;&$_($P4)for@$Q4;&$_($S4)for@$T4;&$_($Z4)for@$j1;&$_($e5)for@$f5;&$_($j5)for@$j1;&$_($m5)for@$j1;&$_($p5)for@$q5;&$_($s5)for@$t5;&$_($x5)for@$j1;&$_($A5)for@$B5;&$_($D5)for@$E5;&$_($G5)for@$H5;&$_($K5)for@$L5;&$_($O5)for@$P5;&$_($W5)for@$j1;&$_($c6)for@$j1;&$_($f6)for@$g6;&$_($l6)for@$j1;&$_($p6)for@$j1;&$_($s6)for@$t6;&$_($y6)for@$j1;&$_($C6)for@$j1;&$_($G6)for@$j1;&$_($K6)for@$j1;&$_($N6)for@$O6;&$_($S6)for@$j1;&$_($W6)for@$j1;&$_($Z6)for@$c7;&$_($e7)for@$f7;&$_($k7)for@$l7;&$_($q7)for@$j1;&$_($t7)for@$u7;&$_($y7)for@$j1;&$_($B7)for@$C7;&$_($E7)for@$F7;&$_($K7)for@$L7;&$_($l8)for@$m8;&$_($s8)for@$j1;&$_($v8)for@$w8;&$_($B8)for@$j1;&$_($F8)for@$j1;&$_($J8)for@$j1;&$_($N8)for@$j1;&$_($R8)for@$j1;&$_($V8)for@$j1;&$_($Z8)for@$j1;&$_($f9)for@$j1;&$_($j9)for@$j1;&$_($n9)for@$j1;&$_($r9)for@$j1;&$_($v9)for@$j1;&$_($z9)for@$j1;&$_($D9)for@$j1;&$_($H9)for@$j1;&$_($L9)for@$j1;&$_($P9)for@$j1;&$_($T9)for@$j1;&$_($W9)for@$X9;&$_($Z9)for@$ca;&$_($La)for@$Ma;&$_($Oa)for@$Pa;&$_($Va)for@$j1;&$_($Ya)for@$Za;&$_($gb)for@$j1;&$_($jb)for@$kb;&$_($mb)for@$nb;&$_($zb)for@$j1;&$_($Cb)for@$j1;&$_($Fb)for@$Gb;&$_($Ib)for@$Jb;&$_($Ob)for@$Pb;&$_($Ub)for@$j1;&$_($Xb)for@$Yb;&$_($fc)for@$j1;&$_($jc)for@$j1;&$_($mc)for@$nc;&$_($pc)for@$qc;&$_($wc)for@$xc;&$_($Cc)for@$j1;&$_($Fc)for@$Gc;&$_($Kc)for@$j1;&$_($Nc)for@$Oc;&$_($Sc)for@$j1;&$_($Vc)for@$j1;&$_($Xc)for@$Yc;&$_($cd)for@$dd;&$_($pd)for@$j1;&$_($sd)for@$td;&$_($yd)for@$j1;&$_($Bd)for@$Cd;&$_($Hd)for@$j1;&$_($Ld)for@$j1;&$_($Od)for@$Pd;&$_($Rd)for@$Sd;&$_($ce)for@$de;&$_($ke)for@$j1;&$_($oe)for@$j1;&$_($se)for@$j1;&$_($we)for@$j1;&$_($ze)for@$Ae;&$_($Ee)for@$j1;&$_($He)for@$Ie;&$_($Me)for@$j1;&$_($Pe)for@$j1;&$_($Se)for@$Te;&$_($Ve)for@$We;&$_($if)for@$j1;&$_($kf)for@$lf;ni->run(@ARGV);
__DATA__
