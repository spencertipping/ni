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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fifo.c';$u=q'/unix/file.c';$v=q'/unix/io.c';$w=q'/unix/pid.c';$x={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$y={$p,1};$z={$j,1,$k,1,$p,1,$q,1};$A=[undef];$B=q'/metaclass';$C=bless({$c,$z,$f,$j,$g,$A},$B);$D=q'/metaclass::ctors';$E=[$C];$F=bless({$c,$y,$f,$p,$g,$E},$B);$G=q'/metaclass::ctors';$H=q'/lib/slice';$I={$H,1};$J=q'/lib/behavior';$K=q'/lib/branch';$L=q'/lib/tag';$M={$J,1,$K,1,$H,1,$L,1};$N=q'/class';$O=q'/lib/doc';$P=q'/lib/fn';$Q=q'/lib/image';$R=q'/lib/ni';$S=q'/object';$T=q'/unix/cat';$U=q'/unix/fifo';$V=q'/unix/file';$W=q'/unix/io';$X=q'/unix/pid';$Y={$N,1,$h,1,$J,1,$j,1,$K,1,$k,1,$O,1,$l,1,$P,1,$m,1,$Q,1,$n,1,$R,1,$o,1,$H,1,$p,1,$L,1,$q,1,$B,1,$d,1,$S,1,$r,1,$T,1,$s,1,$U,1,$t,1,$V,1,$u,1,$W,1,$v,1,$X,1,$w,1};$Z={};$c1=q'ctor';$d1=undef;$e1=q'dtor';$f1=q'methods';$g1=q'class';$h1={$m,1};$i1=[undef];$j1=bless({$c,$h1,$f,$m,$g,$i1},$B);$k1=q'/metaclass::ctors';$l1={$P,1};$m1={};$n1=q'DESTROY';$o1=q'code';$p1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$q1=bless({$o1,$p1},$P);$r1=q'/lib/fn::ctors';$s1=q'new';$t1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$u1=bless({$o1,$t1},$P);$v1={$n1,$q1,$s1,$u1};$w1=q'/lib/instantiable.b';$x1=bless({$c,$m1,$f1,$v1,$f,$w1},$H);$y1=q'/lib/slice::ctors';$z1={};$A1=q'shift->compile';$B1=bless({$o1,$A1},$P);$C1=q'compile';$D1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$E1=bless({$o1,$D1},$P);$F1=q'instantiate';$G1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$H1=bless({$o1,$G1},$P);$I1={$C1,$E1,$F1,$H1};$J1=q'/lib/fn_init.b';$K1=bless({$c,$z1,$c1,$B1,$e1,$d1,$f1,$I1,$f,$J1},$H);$L1=q'/lib/slice::ctors';$M1={};$N1=q'serialize';$O1=q'annotations';$P1=[];$Q1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$R1=bless({$O1,$P1,$o1,$Q1},$P);$S1={$N1,$R1};$T1=q'/lib/fn_serialize.b';$U1=bless({$c,$M1,$c1,$d1,$e1,$d1,$f1,$S1,$f,$T1},$H);$V1=q'/lib/slice::ctors';$W1=[undef,$x1,$K1,$U1];$X1=bless({$c,$l1,$f,$P,$g,$W1},$m);$Y1=q'/lib/fn.c::ctors';$Z1=q'ni \'ni:\' . ref shift';$c2=bless({$o1,$Z1},$P);$d2={$g1,$c2};$e2=q'/lib/instance.b';$f2=bless({$c,$Z,$c1,$d1,$e1,$d1,$f1,$d2,$f,$e2},$H);$g2=q'/lib/slice::ctors';$h2=[$f2];$i2=bless({$c,$Y,$f,$S,$g,$h2},$r);$j2=q'/object.c::ctors';$k2=[$i2];$l2=bless({$c,$M,$f,$J,$g,$k2},$j);$m2=q'/lib/behavior.c::ctors';$n2={};$o2=q'my $s = shift; ni->def($s->name, $s)';$p2=bless({$o1,$o2},$P);$q2=q'$_[0]->namespace . ":" . $_[0]->{name}';$r2=bless({$o1,$q2},$P);$s2={$f,$r2};$t2=q'/lib/named.b';$u2=bless({$c,$n2,$c1,$p2,$e1,$d1,$f1,$s2,$f,$t2},$H);$v2=q'/lib/slice::ctors';$w2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$x2=bless({$o1,$w2},$P);$y2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$z2=bless({$o1,$y2},$P);$A2=q'/lib/slice::apply';$B2=q'/lib/slice::apply_unsafe';$C2={};$D2=q'apply';$E2=q'apply_unsafe';$F2={$D2,$x2,$E2,$z2};$G2=q'/lib/slice.b';$H2=bless({$c,$C2,$f1,$F2,$f,$G2},$H);$I2=q'/lib/slice::ctors';$J2={};$K2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$L2=bless({$o1,$K2},$P);$M2={$F1,$L2};$N2=q'/lib/slice_init.b';$O2=bless({$c,$J2,$f1,$M2,$f,$N2},$H);$P2=q'/lib/slice::ctors';$Q2={};$R2=[];$S2=q'local $_;
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
$g;';$T2=bless({$O1,$R2,$o1,$S2},$P);$U2={$N1,$T2};$V2=q'/lib/slice_serialize.b';$W2=bless({$c,$Q2,$c1,$d1,$e1,$d1,$f1,$U2,$f,$V2},$H);$X2=q'/lib/slice::ctors';$Y2=[$l2,$u2,$H2,$O2,$W2];$Z2=bless({$c,$I,$f,$H,$g,$Y2},$p);$c3=q'/lib/slice.c::ctors';$d3={};$e3=q'doc';$f3=[];$g3=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$h3=bless({$O1,$f3,$o1,$g3},$P);$i3={$e3,$h3};$j3=q'/lib/documentable.b';$k3=bless({$c,$d3,$c1,$d1,$e1,$d1,$f1,$i3,$f,$j3},$H);$l3=q'/lib/slice::ctors';$m3=[undef,$k3];$n3=bless({$c,$x,$f,$r,$g,$m3},$B);$o3=q'/metaclass::ctors';$p3=[$n3];$q3=bless({$c,$i,$f,$h,$g,$p3},$B);$r3=q'/metaclass::ctors';$s3={$N,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$t3={$q,1};$u3=[$C];$v3=bless({$c,$t3,$f,$q,$g,$u3},$B);$w3=q'/metaclass::ctors';$x3={$L,1};$y3={};$z3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$A3=bless({$o1,$z3},$P);$B3={$D2,$A3};$C3=q'/lib/tag.b';$D3=bless({$c,$y3,$c1,$d1,$e1,$d1,$f1,$B3,$f,$C3},$H);$E3=q'/lib/slice::ctors';$F3={};$G3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$H3=bless({$o1,$G3},$P);$I3={$F1,$H3};$J3=q'/lib/tag_init.b';$K3=bless({$c,$F3,$c1,$d1,$e1,$d1,$f1,$I3,$f,$J3},$H);$L3=q'/lib/slice::ctors';$M3=[$l2,$u2,$D3,$K3];$N3=bless({$c,$x3,$f,$L,$g,$M3},$q);$O3=q'/lib/tag.c::ctors';$P3=q'/lib/perlbranch.b';$Q3={};$R3=q'add';$S3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$T3=bless({$o1,$S3},$P);$U3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$V3=bless({$o1,$U3},$P);$W3={$R3,$T3,$D2,$V3};$X3=q'/lib/branch.b';$Y3=bless({$c,$Q3,$c1,$d1,$e1,$d1,$f1,$W3,$f,$X3},$H);$Z3=q'/lib/slice::ctors';$c4={};$d4=q'namespace';$e4=q'\'ni\'';$f4=bless({$o1,$e4},$P);$g4={$d4,$f4};$h4=q'/lib/named_in_ni.b';$i4=bless({$c,$c4,$c1,$d1,$e1,$d1,$f1,$g4,$f,$h4},$H);$j4=q'/lib/slice::ctors';$k4={};$l4=q'package';$m4=q'shift->{name}';$n4=bless({$o1,$m4},$P);$o4={$l4,$n4};$p4=q'/lib/namespaced.b';$q4=bless({$c,$k4,$c1,$d1,$e1,$d1,$f1,$o4,$f,$p4},$H);$r4=q'/lib/slice::ctors';$s4={};$t4=q'resolve';$u4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$v4=bless({$o1,$u4},$P);$w4={$t4,$v4};$x4=q'/lib/resolver.b';$y4=bless({$c,$s4,$c1,$d1,$e1,$d1,$f1,$w4,$f,$x4},$H);$z4=q'/lib/slice::ctors';$A4=[$Y3,$x1,$u2,$i4,$q4,$y4];$B4=bless({$f,$P3,$g,$A4},$L);$C4=q'/lib/tag::ctors';$D4={};$E4=q'my $s = shift; $s->apply($s->package)';$F4=bless({$o1,$E4},$P);$G4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$H4=bless({$o1,$G4},$P);$I4={$F1,$H4};$J4=q'/lib/class_init.b';$K4=bless({$c,$D4,$c1,$F4,$e1,$d1,$f1,$I4,$f,$J4},$H);$L4=q'/lib/slice::ctors';$M4={$k,1};$N4=[$C];$O4=bless({$c,$M4,$f,$k,$g,$N4},$B);$P4=q'/metaclass::ctors';$Q4={$K,1};$R4={};$S4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$T4=bless({$o1,$S4},$P);$U4={$F1,$T4};$V4=q'/lib/branch_init.b';$W4=bless({$c,$R4,$c1,$d1,$e1,$d1,$f1,$U4,$f,$V4},$H);$X4=q'/lib/slice::ctors';$Y4=[$l2,$u2,$Y3,$W4,undef];$Z4=bless({$c,$Q4,$f,$K,$g,$Y4},$k);$c5=q'/lib/branch.c::ctors';$d5={$N,1,$h,1,$j,1,$K,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$B,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1};$e5=q'/lib/definition.b';$f5={};$g5=q'def';$h5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$i5=bless({$o1,$h5},$P);$j5={$g5,$i5};$k5=q'/lib/classdef.b';$l5=bless({$c,$f5,$c1,$d1,$e1,$d1,$f1,$j5,$f,$k5},$H);$m5=q'/lib/slice::ctors';$n5={};$o5=q'ro';$p5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$q5=bless({$o1,$p5},$P);$r5=q'rw';$s5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$t5=bless({$o1,$s5},$P);$u5={$o5,$q5,$r5,$t5};$v5=q'/lib/accessor.b';$w5=bless({$c,$n5,$c1,$d1,$e1,$d1,$f1,$u5,$f,$v5},$H);$x5=q'/lib/slice::ctors';$y5=[$l5,$w5];$z5=bless({$c,$d5,$f,$e5,$g,$y5},$K);$A5=q'/lib/branch::ctors';$B5={};$C5=q'child';$D5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$E5=bless({$o1,$D5},$P);$F5={$C5,$E5};$G5=q'/lib/subclass.b';$H5=bless({$c,$B5,$c1,$d1,$e1,$d1,$f1,$F5,$f,$G5},$H);$I5=q'/lib/slice::ctors';$J5=[$B4,$K4,$i2,$z5,$H5];$K5=bless({$c,$s3,$f,$N,$g,$J5},$h);$L5=q'/class.c::ctors';$M5=[$K5];$N5=bless({$c,$e,$f,$d,$g,$M5},$B);$O5=q'/metaclass::ctors';$P5={$B,1};$Q5=[$B4,$K4,$i2,$z5];$R5=bless({$c,$P5,$f,$B,$g,$Q5},$d);$S5=q'/metaclass.c::ctors';$T5={$o,1};$U5=[$n3];$V5=bless({$c,$T5,$f,$o,$g,$U5},$B);$W5=q'/metaclass::ctors';$X5={$R,1};$Y5={};$Z5=q'is_mutable';$c6=[];$d6=q'$0 ne "-" && -w $0';$e6=bless({$O1,$c6,$o1,$d6},$P);$f6=q'modify';$g6=[];$h6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$i6=bless({$O1,$g6,$o1,$h6},$P);$j6={$Z5,$e6,$f6,$i6};$k6=q'/lib/ni_self.b';$l6=bless({$c,$Y5,$c1,$d1,$e1,$d1,$f1,$j6,$f,$k6},$H);$m6=q'/lib/slice::ctors';$n6={};$o6=q'exists';$p6=[];$q6=q'exists $_[0]->{named}{$_[1]}';$r6=bless({$O1,$p6,$o1,$q6},$P);$s6=q'quoted';$t6=[];$u6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$v6=bless({$O1,$t6,$o1,$u6},$P);$w6={$o6,$r6,$s6,$v6};$x6=q'/lib/ni_image.b';$y6=bless({$c,$n6,$c1,$d1,$e1,$d1,$f1,$w6,$f,$x6},$H);$z6=q'/lib/slice::ctors';$A6={};$B6=q'--internal/+=';$C6=[];$D6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$E6=bless({$O1,$C6,$o1,$D6},$P);$F6=q'--internal/eval';$G6=[];$H6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$I6=bless({$O1,$G6,$o1,$H6},$P);$J6=q'--internal/image';$K6=[];$L6=q'shift->quoted->write(\\*STDOUT);
0;';$M6=bless({$O1,$K6,$o1,$L6},$P);$N6=q'run';$O6=[];$P6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$Q6=bless({$O1,$O6,$o1,$P6},$P);$R6={$B6,$E6,$F6,$I6,$J6,$M6,$N6,$Q6};$S6=q'/lib/ni_main.b';$T6=bless({$c,$A6,$c1,$d1,$e1,$d1,$f1,$R6,$f,$S6},$H);$U6=q'/lib/slice::ctors';$V6={};$W6=[];$X6=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$Y6=bless({$O1,$W6,$o1,$X6},$P);$Z6=q'resolver_for';$c7=[];$d7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$e7=bless({$O1,$c7,$o1,$d7},$P);$f7={$t4,$Y6,$Z6,$e7};$g7=q'/lib/ni_resolver.b';$h7=bless({$c,$V6,$c1,$d1,$e1,$d1,$f1,$f7,$f,$g7},$H);$i7=q'/lib/slice::ctors';$j7=[$i2,$l6,$y6,$T6,$h7];$k7=bless({$c,$X5,$f,$R,$g,$j7},$o);$l7=q'/lib/ni.c::ctors';$m7=q'named';$n7=q'ni.doc:/class';$o7={$l,1};$p7=[$n3];$q7=bless({$c,$o7,$f,$l,$g,$p7},$B);$r7=q'/metaclass::ctors';$s7={$O,1};$t7={};$u7=[];$v7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$w7=bless({$O1,$u7,$o1,$v7},$P);$x7={$F1,$w7};$y7=q'/lib/doc_init.b';$z7=bless({$c,$t7,$c1,$d1,$e1,$d1,$f1,$x7,$f,$y7},$H);$A7=q'/lib/slice::ctors';$B7={};$C7=[];$D7=q'\'ni.doc\'';$E7=bless({$O1,$C7,$o1,$D7},$P);$F7={$d4,$E7};$G7=q'/lib/doc_namespace.b';$H7=bless({$c,$B7,$c1,$d1,$e1,$d1,$f1,$F7,$f,$G7},$H);$I7=q'/lib/slice::ctors';$J7=[$i2,$u2,$z7,$H7];$K7=bless({$c,$s7,$f,$O,$g,$J7},$l);$L7=q'/lib/doc.c::ctors';$M7=q'apropos';$N7=q'ni:/class';$O7=[$N7];$P7=q'# Classes and metaclasses
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
';$Q7=bless({$M7,$O7,$e3,$P7,$f,$N},$O);$R7=q'/lib/doc::ctors';$S7=q'ni:/class.c';$T7=q'ni:/lib/accessor.b';$U7=q'ni:/lib/behavior';$V7=q'ni:/lib/behavior.c';$W7=q'ni:/lib/branch';$X7=q'ni:/lib/branch.b';$Y7=q'ni:/lib/branch.c';$Z7=q'ni:/lib/branch_init.b';$c8=q'ni:/lib/class_init.b';$d8=q'ni:/lib/classdef.b';$e8=q'ni:/lib/definition.b';$f8=q'ni:/lib/doc';$g8=q'ni:/lib/doc.c';$h8=q'ni:/lib/doc_init.b';$i8=q'ni:/lib/doc_namespace.b';$j8=q'ni:/lib/documentable.b';$k8=q'ni:/lib/fn';$l8=q'ni:/lib/fn.c';$m8=q'ni:/lib/fn_init.b';$n8=q'ni:/lib/fn_serialize.b';$o8=q'ni:/lib/image';$p8={$n,1};$q8=[$n3];$r8=bless({$c,$p8,$f,$n,$g,$q8},$B);$s8=q'/metaclass::ctors';$t8={$Q,1};$u8={};$v8=[];$w8=q'my $class = shift;
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
  ordering     => []};';$x8=bless({$O1,$v8,$o1,$w8},$P);$y8={$F1,$x8};$z8=q'/lib/image_init.b';$A8=bless({$c,$u8,$c1,$d1,$e1,$d1,$f1,$y8,$f,$z8},$H);$B8=q'/lib/slice::ctors';$C8={};$D8=q'address';$E8=[];$F8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$G8=bless({$O1,$E8,$o1,$F8},$P);$H8=q'allocate_gensym';$I8=[];$J8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$K8=bless({$O1,$I8,$o1,$J8},$P);$L8=q'boot_side_effect';$M8=[];$N8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$O8=bless({$O1,$M8,$o1,$N8},$P);$P8=q'circular_links';$Q8=[];$R8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$S8=bless({$O1,$Q8,$o1,$R8},$P);$T8=q'finalizer';$U8=[];$V8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$W8=bless({$O1,$U8,$o1,$V8},$P);$X8=q'gensym';$Y8=[];$Z8=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$c9=bless({$O1,$Y8,$o1,$Z8},$P);$d9=q'is_circular';$e9=[];$f9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$g9=bless({$O1,$e9,$o1,$f9},$P);$h9=q'quote';$i9=[];$j9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$k9=bless({$O1,$i9,$o1,$j9},$P);$l9=q'quote_array';$m9=[];$n9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$o9=bless({$O1,$m9,$o1,$n9},$P);$p9=q'quote_blessed';$q9=[];$r9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$s9=bless({$O1,$q9,$o1,$r9},$P);$t9=q'quote_class';$u9=[];$v9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$w9=bless({$O1,$u9,$o1,$v9},$P);$x9=q'quote_hash';$y9=[];$z9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$A9=bless({$O1,$y9,$o1,$z9},$P);$B9=q'quote_object';$C9=[];$D9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$E9=bless({$O1,$C9,$o1,$D9},$P);$F9=q'quote_scalar';$G9=[];$H9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$I9=bless({$O1,$G9,$o1,$H9},$P);$J9=q'quote_value';$K9=[];$L9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$M9=bless({$O1,$K9,$o1,$L9},$P);$N9=q'reconstruction';$O9=[];$P9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Q9=bless({$O1,$O9,$o1,$P9},$P);$R9=q'side_effect';$S9=[];$T9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$U9=bless({$O1,$S9,$o1,$T9},$P);$V9=q'write';$W9=[];$X9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Y9=bless({$O1,$W9,$o1,$X9},$P);$Z9={$D8,$G8,$H8,$K8,$L8,$O8,$P8,$S8,$T8,$W8,$X8,$c9,$d9,$g9,$h9,$k9,$l9,$o9,$p9,$s9,$t9,$w9,$x9,$A9,$B9,$E9,$F9,$I9,$J9,$M9,$N9,$Q9,$R9,$U9,$V9,$Y9};$ca=q'/lib/image_quoting.b';$da=bless({$c,$C8,$c1,$d1,$e1,$d1,$f1,$Z9,$f,$ca},$H);$ea=q'/lib/slice::ctors';$fa=[$i2,$A8,$da];$ga=bless({$c,$t8,$f,$Q,$g,$fa},$n);$ha=q'/lib/image.c::ctors';$ia=q'ni:/lib/image.c';$ja=q'ni:/lib/image_init.b';$ka=q'ni:/lib/image_quoting.b';$la=q'ni:/lib/instance.b';$ma=q'ni:/lib/instantiable.b';$na=q'ni:/lib/named.b';$oa=q'ni:/lib/named_in_ni.b';$pa=q'ni:/lib/namespaced.b';$qa=q'ni:/lib/ni';$ra=q'ni:/lib/ni.c';$sa=q'ni:/lib/ni_image.b';$ta=q'ni:/lib/ni_main.b';$ua=q'ni:/lib/ni_resolver.b';$va=q'ni:/lib/ni_self.b';$wa=q'ni:/lib/perlbranch.b';$xa=q'ni:/lib/resolver.b';$ya=q'ni:/lib/slice';$za=q'ni:/lib/slice.b';$Aa=q'ni:/lib/slice.c';$Ba=q'ni:/lib/slice_init.b';$Ca=q'ni:/lib/slice_serialize.b';$Da=q'ni:/lib/subclass.b';$Ea=q'ni:/lib/tag';$Fa=q'ni:/lib/tag.b';$Ga=q'ni:/lib/tag.c';$Ha=q'ni:/lib/tag_init.b';$Ia=q'ni:/metaclass';$Ja=q'ni:/metaclass.c';$Ka=q'ni:/object';$La=q'ni:/object.c';$Ma=q'ni:/unix/cat';$Na={$s,1};$Oa={$s,1,$u,1,$v,1,$w,1};$Pa=[$n3];$Qa=bless({$c,$Oa,$f,$v,$g,$Pa},$B);$Ra=q'/metaclass::ctors';$Sa=[$Qa];$Ta=bless({$c,$Na,$f,$s,$g,$Sa},$B);$Ua=q'/metaclass::ctors';$Va={$T,1};$Wa={$T,1,$V,1,$W,1,$X,1};$Xa={};$Ya=q'into';$Za=[];$cb=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$db=bless({$O1,$Za,$o1,$cb},$P);$eb={$Ya,$db};$fb=q'/unix/io_stream.b';$gb=bless({$c,$Xa,$c1,$d1,$e1,$d1,$f1,$eb,$f,$fb},$H);$hb=q'/lib/slice::ctors';$ib={};$jb=q'(+';$kb=[];$lb=q'ni(\'ni:/unix/cat\')->new(@_)';$mb=bless({$O1,$kb,$o1,$lb},$P);$nb={$jb,$mb};$ob=q'/unix/io_constructors.b';$pb=bless({$c,$ib,$c1,$d1,$e1,$d1,$f1,$nb,$f,$ob},$H);$qb=q'/lib/slice::ctors';$rb={};$sb=q'(<>';$tb=[];$ub=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$vb=bless({$O1,$tb,$o1,$ub},$P);$wb=q'(@{}';$xb=[];$yb=q'my $self = shift; [<$self>]';$zb=bless({$O1,$xb,$o1,$yb},$P);$Ab={$sb,$vb,$wb,$zb};$Bb=q'/unix/io_readers.b';$Cb=bless({$c,$rb,$c1,$d1,$e1,$d1,$f1,$Ab,$f,$Bb},$H);$Db=q'/lib/slice::ctors';$Eb=[$i2,$gb,$pb,$Cb];$Fb=bless({$c,$Wa,$f,$W,$g,$Eb},$v);$Gb=q'/unix/io.c::ctors';$Hb={};$Ib=[];$Jb=q'shift; [@_]';$Kb=bless({$O1,$Ib,$o1,$Jb},$P);$Lb={$F1,$Kb};$Mb=q'/unix/cat_init.b';$Nb=bless({$c,$Hb,$c1,$d1,$e1,$d1,$f1,$Lb,$f,$Mb},$H);$Ob=q'/lib/slice::ctors';$Pb={};$Qb=q'read';$Rb=[];$Sb=q'my $self = shift;
my $n;
shift @$self until !@$self or $n = $$self[0]->read(@_);
return undef;';$Tb=bless({$O1,$Rb,$o1,$Sb},$P);$Ub={$Qb,$Tb};$Vb=q'/unix/cat_read.b';$Wb=bless({$c,$Pb,$c1,$d1,$e1,$d1,$f1,$Ub,$f,$Vb},$H);$Xb=q'/lib/slice::ctors';$Yb=[$Fb,$Nb,$Wb];$Zb=bless({$c,$Va,$f,$T,$g,$Yb},$s);$cc=q'/unix/cat.c::ctors';$dc=q'ni:/unix/cat.c';$ec=q'ni:/unix/cat_init.b';$fc=q'ni:/unix/cat_read.b';$gc=q'ni:/unix/fd.b';$hc={$U,1,$V,1};$ic=q'/unix/fd.b';$jc={};$kc=[];$lc=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$mc=bless({$O1,$kc,$o1,$lc},$P);$nc=[];$oc=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$pc=bless({$O1,$nc,$o1,$oc},$P);$qc={$Qb,$mc,$V9,$pc};$rc=q'/unix/fd_safeio.b';$sc=bless({$c,$jc,$c1,$d1,$e1,$d1,$f1,$qc,$f,$rc},$H);$tc=q'/lib/slice::ctors';$uc=[$sc];$vc=bless({$c,$hc,$f,$ic,$g,$uc},$K);$wc=q'/lib/branch::ctors';$xc=q'ni:/unix/fd_safeio.b';$yc=q'ni:/unix/fifo';$zc={$t,1};$Ac=[$n3];$Bc=bless({$c,$zc,$f,$t,$g,$Ac},$B);$Cc=q'/metaclass::ctors';$Dc={$U,1};$Ec={};$Fc=[];$Gc=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Hc=bless({$O1,$Fc,$o1,$Gc},$P);$Ic={$F1,$Hc};$Jc=q'/unix/fifo_init.b';$Kc=bless({$c,$Ec,$c1,$d1,$e1,$d1,$f1,$Ic,$f,$Jc},$H);$Lc=q'/lib/slice::ctors';$Mc={};$Nc=q'read_fh';$Oc=[];$Pc=q'shift->{read_fh}';$Qc=bless({$O1,$Oc,$o1,$Pc},$P);$Rc=q'write_fh';$Sc=[];$Tc=q'shift->{write_fh}';$Uc=bless({$O1,$Sc,$o1,$Tc},$P);$Vc={$Nc,$Qc,$Rc,$Uc};$Wc=q'/unix/file_io.b';$Xc=bless({$c,$Mc,$c1,$d1,$e1,$d1,$f1,$Vc,$f,$Wc},$H);$Yc=q'/lib/slice::ctors';$Zc=[$i2,$Kc,$vc,$Xc];$cd=bless({$c,$Dc,$f,$U,$g,$Zc},$t);$dd=q'/unix/fifo.c::ctors';$ed=q'ni:/unix/fifo.c';$fd=q'ni:/unix/fifo_init.b';$gd=q'ni:/unix/file';$hd={$u,1};$id=[$Qa];$jd=bless({$c,$hd,$f,$u,$g,$id},$B);$kd=q'/metaclass::ctors';$ld={$V,1};$md={};$nd=[];$od=q'shift->{\'name\'}';$pd=bless({$O1,$nd,$o1,$od},$P);$qd={$f,$pd};$rd=q'/unix/file_readers.b';$sd=bless({$c,$md,$c1,$d1,$e1,$d1,$f1,$qd,$f,$rd},$H);$td=q'/lib/slice::ctors';$ud={};$vd=[];$wd=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$xd=bless({$O1,$vd,$o1,$wd},$P);$yd={$F1,$xd};$zd=q'/unix/file_init.b';$Ad=bless({$c,$ud,$c1,$d1,$e1,$d1,$f1,$yd,$f,$zd},$H);$Bd=q'/lib/slice::ctors';$Cd={};$Dd=[];$Ed=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Fd=bless({$O1,$Dd,$o1,$Ed},$P);$Gd=[];$Hd=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Id=bless({$O1,$Gd,$o1,$Hd},$P);$Jd={$Nc,$Fd,$Rc,$Id};$Kd=bless({$c,$Cd,$c1,$d1,$e1,$d1,$f1,$Jd,$f,$Wc},$H);$Ld=q'/lib/slice::ctors';$Md=[$Fb,$sd,$Ad,$vc,$Kd];$Nd=bless({$c,$ld,$f,$V,$g,$Md},$u);$Od=q'/unix/file.c::ctors';$Pd=q'ni:/unix/file.c';$Qd=q'ni:/unix/file_init.b';$Rd=q'ni:/unix/file_io.b';$Sd=q'ni:/unix/file_readers.b';$Td=q'ni:/unix/io';$Ud=q'ni:/unix/io.c';$Vd=q'ni:/unix/io_constructors.b';$Wd=q'ni:/unix/io_readers.b';$Xd=q'ni:/unix/io_stream.b';$Yd=q'ni:/unix/pid';$Zd={$w,1};$ce=[$Qa];$de=bless({$c,$Zd,$f,$w,$g,$ce},$B);$ee=q'/metaclass::ctors';$fe={$X,1};$ge={};$he=q'pid';$ie=[];$je=q'shift->{\'pid\'}';$ke=bless({$O1,$ie,$o1,$je},$P);$le=q'stderr';$me=[];$ne=q'shift->{\'stderr\'}';$oe=bless({$O1,$me,$o1,$ne},$P);$pe=q'stdin';$qe=[];$re=q'shift->{\'stdin\'}';$se=bless({$O1,$qe,$o1,$re},$P);$te=q'stdout';$ue=[];$ve=q'shift->{\'stdout\'}';$we=bless({$O1,$ue,$o1,$ve},$P);$xe={$he,$ke,$le,$oe,$pe,$se,$te,$we};$ye=q'/unix/pid_readers.b';$ze=bless({$c,$ge,$c1,$d1,$e1,$d1,$f1,$xe,$f,$ye},$H);$Ae=q'/lib/slice::ctors';$Be={};$Ce=[];$De=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Ee=bless({$O1,$Ce,$o1,$De},$P);$Fe={$F1,$Ee};$Ge=q'/unix/pid_init.b';$He=bless({$c,$Be,$c1,$d1,$e1,$d1,$f1,$Fe,$f,$Ge},$H);$Ie=q'/lib/slice::ctors';$Je={};$Ke=[];$Le=q'shift->{stdout}';$Me=bless({$O1,$Ke,$o1,$Le},$P);$Ne=[];$Oe=q'shift->{stdin}';$Pe=bless({$O1,$Ne,$o1,$Oe},$P);$Qe={$Nc,$Me,$Rc,$Pe};$Re=q'/unix/pid_io.b';$Se=bless({$c,$Je,$c1,$d1,$e1,$d1,$f1,$Qe,$f,$Re},$H);$Te=q'/lib/slice::ctors';$Ue=[$Fb,$ze,$He,$Se];$Ve=bless({$c,$fe,$f,$X,$g,$Ue},$w);$We=q'/unix/pid.c::ctors';$Xe=q'ni:/unix/pid.c';$Ye=q'ni:/unix/pid_init.b';$Ze=q'ni:/unix/pid_io.b';$cf=q'ni:/unix/pid_readers.b';$df={$n7,$Q7,$N7,$K5,$S7,$q3,$T7,$w5,$U7,$l2,$V7,$C,$W7,$Z4,$X7,$Y3,$Y7,$O4,$Z7,$W4,$c8,$K4,$d8,$l5,$e8,$z5,$f8,$K7,$g8,$q7,$h8,$z7,$i8,$H7,$j8,$k3,$k8,$X1,$l8,$j1,$m8,$K1,$n8,$U1,$o8,$ga,$ia,$r8,$ja,$A8,$ka,$da,$la,$f2,$ma,$x1,$na,$u2,$oa,$i4,$pa,$q4,$qa,$k7,$ra,$V5,$sa,$y6,$ta,$T6,$ua,$h7,$va,$l6,$wa,$B4,$xa,$y4,$ya,$Z2,$za,$H2,$Aa,$F,$Ba,$O2,$Ca,$W2,$Da,$H5,$Ea,$N3,$Fa,$D3,$Ga,$v3,$Ha,$K3,$Ia,$R5,$Ja,$N5,$Ka,$i2,$La,$n3,$Ma,$Zb,$dc,$Ta,$ec,$Nb,$fc,$Wb,$gc,$vc,$xc,$sc,$yc,$cd,$ed,$Bc,$fd,$Kc,$gd,$Nd,$Pd,$jd,$Qd,$Ad,$Rd,$Kd,$Sd,$sd,$Td,$Fb,$Ud,$Qa,$Vd,$pb,$Wd,$Cb,$Xd,$gb,$Yd,$Ve,$Xe,$de,$Ye,$He,$Ze,$Se,$cf,$ze};$ef=q'resolvers';$ff=q'file';$gf=[];$hf=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$if=bless({$O1,$gf,$o1,$hf},$P);$jf={$ff,$if};$kf=bless({$m7,$df,$ef,$jf},$R);$lf=q'/lib/ni::ctors';$$m3[0]=$K5;$$A[0]=$n3;$$i1[0]=$n3;$$W1[0]=$i2;$$Y4[4]=$z5;*$B2=\&$z2;*$A2=\&$x2;$x1->apply_unsafe($N);$x1->apply_unsafe($h);$x1->apply_unsafe($j);$x1->apply_unsafe($K);$x1->apply_unsafe($k);$x1->apply_unsafe($l);$x1->apply_unsafe($P);$x1->apply_unsafe($m);$x1->apply_unsafe($n);$x1->apply_unsafe($o);$x1->apply_unsafe($H);$x1->apply_unsafe($p);$x1->apply_unsafe($L);$x1->apply_unsafe($q);$x1->apply_unsafe($B);$x1->apply_unsafe($d);$x1->apply_unsafe($r);$x1->apply_unsafe($s);$x1->apply_unsafe($t);$x1->apply_unsafe($u);$x1->apply_unsafe($v);$x1->apply_unsafe($w);$K1->apply_unsafe($P);$U1->apply_unsafe($P);$f2->apply_unsafe($N);$f2->apply_unsafe($h);$f2->apply_unsafe($J);$f2->apply_unsafe($j);$f2->apply_unsafe($K);$f2->apply_unsafe($k);$f2->apply_unsafe($O);$f2->apply_unsafe($l);$f2->apply_unsafe($P);$f2->apply_unsafe($m);$f2->apply_unsafe($Q);$f2->apply_unsafe($n);$f2->apply_unsafe($R);$f2->apply_unsafe($o);$f2->apply_unsafe($H);$f2->apply_unsafe($p);$f2->apply_unsafe($L);$f2->apply_unsafe($q);$f2->apply_unsafe($B);$f2->apply_unsafe($d);$f2->apply_unsafe($S);$f2->apply_unsafe($r);$f2->apply_unsafe($T);$f2->apply_unsafe($s);$f2->apply_unsafe($U);$f2->apply_unsafe($t);$f2->apply_unsafe($V);$f2->apply_unsafe($u);$f2->apply_unsafe($W);$f2->apply_unsafe($v);$f2->apply_unsafe($X);$f2->apply_unsafe($w);$u2->apply_unsafe($N);$u2->apply_unsafe($h);$u2->apply_unsafe($j);$u2->apply_unsafe($K);$u2->apply_unsafe($k);$u2->apply_unsafe($O);$u2->apply_unsafe($l);$u2->apply_unsafe($m);$u2->apply_unsafe($n);$u2->apply_unsafe($o);$u2->apply_unsafe($H);$u2->apply_unsafe($p);$u2->apply_unsafe($L);$u2->apply_unsafe($q);$u2->apply_unsafe($B);$u2->apply_unsafe($d);$u2->apply_unsafe($r);$u2->apply_unsafe($s);$u2->apply_unsafe($t);$u2->apply_unsafe($u);$u2->apply_unsafe($v);$u2->apply_unsafe($w);$H2->apply_unsafe($H);$O2->apply_unsafe($H);$W2->apply_unsafe($H);$k3->apply_unsafe($h);$k3->apply_unsafe($j);$k3->apply_unsafe($k);$k3->apply_unsafe($l);$k3->apply_unsafe($m);$k3->apply_unsafe($n);$k3->apply_unsafe($o);$k3->apply_unsafe($p);$k3->apply_unsafe($q);$k3->apply_unsafe($r);$k3->apply_unsafe($s);$k3->apply_unsafe($t);$k3->apply_unsafe($u);$k3->apply_unsafe($v);$k3->apply_unsafe($w);$D3->apply_unsafe($L);$K3->apply_unsafe($L);$Y3->apply_unsafe($N);$Y3->apply_unsafe($h);$Y3->apply_unsafe($j);$Y3->apply_unsafe($K);$Y3->apply_unsafe($k);$Y3->apply_unsafe($l);$Y3->apply_unsafe($m);$Y3->apply_unsafe($n);$Y3->apply_unsafe($o);$Y3->apply_unsafe($p);$Y3->apply_unsafe($q);$Y3->apply_unsafe($B);$Y3->apply_unsafe($d);$Y3->apply_unsafe($r);$Y3->apply_unsafe($s);$Y3->apply_unsafe($t);$Y3->apply_unsafe($u);$Y3->apply_unsafe($v);$Y3->apply_unsafe($w);$i4->apply_unsafe($N);$i4->apply_unsafe($h);$i4->apply_unsafe($j);$i4->apply_unsafe($K);$i4->apply_unsafe($k);$i4->apply_unsafe($l);$i4->apply_unsafe($m);$i4->apply_unsafe($n);$i4->apply_unsafe($o);$i4->apply_unsafe($H);$i4->apply_unsafe($p);$i4->apply_unsafe($L);$i4->apply_unsafe($q);$i4->apply_unsafe($B);$i4->apply_unsafe($d);$i4->apply_unsafe($r);$i4->apply_unsafe($s);$i4->apply_unsafe($t);$i4->apply_unsafe($u);$i4->apply_unsafe($v);$i4->apply_unsafe($w);$q4->apply_unsafe($N);$q4->apply_unsafe($h);$q4->apply_unsafe($j);$q4->apply_unsafe($K);$q4->apply_unsafe($k);$q4->apply_unsafe($l);$q4->apply_unsafe($m);$q4->apply_unsafe($n);$q4->apply_unsafe($o);$q4->apply_unsafe($H);$q4->apply_unsafe($p);$q4->apply_unsafe($L);$q4->apply_unsafe($q);$q4->apply_unsafe($B);$q4->apply_unsafe($d);$q4->apply_unsafe($r);$q4->apply_unsafe($s);$q4->apply_unsafe($t);$q4->apply_unsafe($u);$q4->apply_unsafe($v);$q4->apply_unsafe($w);$y4->apply_unsafe($N);$y4->apply_unsafe($h);$y4->apply_unsafe($j);$y4->apply_unsafe($K);$y4->apply_unsafe($k);$y4->apply_unsafe($l);$y4->apply_unsafe($m);$y4->apply_unsafe($n);$y4->apply_unsafe($o);$y4->apply_unsafe($p);$y4->apply_unsafe($L);$y4->apply_unsafe($q);$y4->apply_unsafe($B);$y4->apply_unsafe($d);$y4->apply_unsafe($r);$y4->apply_unsafe($s);$y4->apply_unsafe($t);$y4->apply_unsafe($u);$y4->apply_unsafe($v);$y4->apply_unsafe($w);$K4->apply_unsafe($N);$K4->apply_unsafe($h);$K4->apply_unsafe($j);$K4->apply_unsafe($k);$K4->apply_unsafe($l);$K4->apply_unsafe($m);$K4->apply_unsafe($n);$K4->apply_unsafe($o);$K4->apply_unsafe($p);$K4->apply_unsafe($q);$K4->apply_unsafe($B);$K4->apply_unsafe($d);$K4->apply_unsafe($r);$K4->apply_unsafe($s);$K4->apply_unsafe($t);$K4->apply_unsafe($u);$K4->apply_unsafe($v);$K4->apply_unsafe($w);$W4->apply_unsafe($K);$l5->apply_unsafe($N);$l5->apply_unsafe($h);$l5->apply_unsafe($j);$l5->apply_unsafe($K);$l5->apply_unsafe($k);$l5->apply_unsafe($l);$l5->apply_unsafe($m);$l5->apply_unsafe($n);$l5->apply_unsafe($o);$l5->apply_unsafe($p);$l5->apply_unsafe($q);$l5->apply_unsafe($B);$l5->apply_unsafe($d);$l5->apply_unsafe($r);$l5->apply_unsafe($s);$l5->apply_unsafe($t);$l5->apply_unsafe($u);$l5->apply_unsafe($v);$l5->apply_unsafe($w);$w5->apply_unsafe($N);$w5->apply_unsafe($h);$w5->apply_unsafe($j);$w5->apply_unsafe($K);$w5->apply_unsafe($k);$w5->apply_unsafe($l);$w5->apply_unsafe($m);$w5->apply_unsafe($n);$w5->apply_unsafe($o);$w5->apply_unsafe($p);$w5->apply_unsafe($q);$w5->apply_unsafe($B);$w5->apply_unsafe($d);$w5->apply_unsafe($r);$w5->apply_unsafe($s);$w5->apply_unsafe($t);$w5->apply_unsafe($u);$w5->apply_unsafe($v);$w5->apply_unsafe($w);$H5->apply_unsafe($N);$H5->apply_unsafe($h);$H5->apply_unsafe($j);$H5->apply_unsafe($k);$H5->apply_unsafe($l);$H5->apply_unsafe($m);$H5->apply_unsafe($n);$H5->apply_unsafe($o);$H5->apply_unsafe($p);$H5->apply_unsafe($q);$H5->apply_unsafe($d);$H5->apply_unsafe($r);$H5->apply_unsafe($s);$H5->apply_unsafe($t);$H5->apply_unsafe($u);$H5->apply_unsafe($v);$H5->apply_unsafe($w);$l6->apply_unsafe($R);$y6->apply_unsafe($R);$T6->apply_unsafe($R);$h7->apply_unsafe($R);$z7->apply_unsafe($O);$H7->apply_unsafe($O);$A8->apply_unsafe($Q);$da->apply_unsafe($Q);$gb->apply_unsafe($T);$gb->apply_unsafe($V);$gb->apply_unsafe($W);$gb->apply_unsafe($X);$pb->apply_unsafe($T);$pb->apply_unsafe($V);$pb->apply_unsafe($W);$pb->apply_unsafe($X);$Cb->apply_unsafe($T);$Cb->apply_unsafe($V);$Cb->apply_unsafe($W);$Cb->apply_unsafe($X);$Nb->apply_unsafe($T);$Wb->apply_unsafe($T);$sc->apply_unsafe($U);$sc->apply_unsafe($V);$Kc->apply_unsafe($U);$Xc->apply_unsafe($U);$sd->apply_unsafe($V);$Ad->apply_unsafe($V);$Kd->apply_unsafe($V);$ze->apply_unsafe($X);$He->apply_unsafe($X);$Se->apply_unsafe($X);$ni::self=$kf;&$_($C)for@$D;&$_($F)for@$G;&$_($j1)for@$k1;&$_($q1)for@$r1;&$_($u1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$r1;&$_($E1)for@$r1;&$_($H1)for@$r1;&$_($K1)for@$L1;&$_($R1)for@$r1;&$_($U1)for@$V1;&$_($X1)for@$Y1;&$_($c2)for@$r1;&$_($f2)for@$g2;&$_($i2)for@$j2;&$_($l2)for@$m2;&$_($p2)for@$r1;&$_($r2)for@$r1;&$_($u2)for@$v2;&$_($x2)for@$r1;&$_($z2)for@$r1;&$_($H2)for@$I2;&$_($L2)for@$r1;&$_($O2)for@$P2;&$_($T2)for@$r1;&$_($W2)for@$X2;&$_($Z2)for@$c3;&$_($h3)for@$r1;&$_($k3)for@$l3;&$_($n3)for@$o3;&$_($q3)for@$r3;&$_($v3)for@$w3;&$_($A3)for@$r1;&$_($D3)for@$E3;&$_($H3)for@$r1;&$_($K3)for@$L3;&$_($N3)for@$O3;&$_($T3)for@$r1;&$_($V3)for@$r1;&$_($Y3)for@$Z3;&$_($f4)for@$r1;&$_($i4)for@$j4;&$_($n4)for@$r1;&$_($q4)for@$r4;&$_($v4)for@$r1;&$_($y4)for@$z4;&$_($B4)for@$C4;&$_($F4)for@$r1;&$_($H4)for@$r1;&$_($K4)for@$L4;&$_($O4)for@$P4;&$_($T4)for@$r1;&$_($W4)for@$X4;&$_($Z4)for@$c5;&$_($i5)for@$r1;&$_($l5)for@$m5;&$_($q5)for@$r1;&$_($t5)for@$r1;&$_($w5)for@$x5;&$_($z5)for@$A5;&$_($E5)for@$r1;&$_($H5)for@$I5;&$_($K5)for@$L5;&$_($N5)for@$O5;&$_($R5)for@$S5;&$_($V5)for@$W5;&$_($e6)for@$r1;&$_($i6)for@$r1;&$_($l6)for@$m6;&$_($r6)for@$r1;&$_($v6)for@$r1;&$_($y6)for@$z6;&$_($E6)for@$r1;&$_($I6)for@$r1;&$_($M6)for@$r1;&$_($Q6)for@$r1;&$_($T6)for@$U6;&$_($Y6)for@$r1;&$_($e7)for@$r1;&$_($h7)for@$i7;&$_($k7)for@$l7;&$_($q7)for@$r7;&$_($w7)for@$r1;&$_($z7)for@$A7;&$_($E7)for@$r1;&$_($H7)for@$I7;&$_($K7)for@$L7;&$_($Q7)for@$R7;&$_($r8)for@$s8;&$_($x8)for@$r1;&$_($A8)for@$B8;&$_($G8)for@$r1;&$_($K8)for@$r1;&$_($O8)for@$r1;&$_($S8)for@$r1;&$_($W8)for@$r1;&$_($c9)for@$r1;&$_($g9)for@$r1;&$_($k9)for@$r1;&$_($o9)for@$r1;&$_($s9)for@$r1;&$_($w9)for@$r1;&$_($A9)for@$r1;&$_($E9)for@$r1;&$_($I9)for@$r1;&$_($M9)for@$r1;&$_($Q9)for@$r1;&$_($U9)for@$r1;&$_($Y9)for@$r1;&$_($da)for@$ea;&$_($ga)for@$ha;&$_($Qa)for@$Ra;&$_($Ta)for@$Ua;&$_($db)for@$r1;&$_($gb)for@$hb;&$_($mb)for@$r1;&$_($pb)for@$qb;&$_($vb)for@$r1;&$_($zb)for@$r1;&$_($Cb)for@$Db;&$_($Fb)for@$Gb;&$_($Kb)for@$r1;&$_($Nb)for@$Ob;&$_($Tb)for@$r1;&$_($Wb)for@$Xb;&$_($Zb)for@$cc;&$_($mc)for@$r1;&$_($pc)for@$r1;&$_($sc)for@$tc;&$_($vc)for@$wc;&$_($Bc)for@$Cc;&$_($Hc)for@$r1;&$_($Kc)for@$Lc;&$_($Qc)for@$r1;&$_($Uc)for@$r1;&$_($Xc)for@$Yc;&$_($cd)for@$dd;&$_($jd)for@$kd;&$_($pd)for@$r1;&$_($sd)for@$td;&$_($xd)for@$r1;&$_($Ad)for@$Bd;&$_($Fd)for@$r1;&$_($Id)for@$r1;&$_($Kd)for@$Ld;&$_($Nd)for@$Od;&$_($de)for@$ee;&$_($ke)for@$r1;&$_($oe)for@$r1;&$_($se)for@$r1;&$_($we)for@$r1;&$_($ze)for@$Ae;&$_($Ee)for@$r1;&$_($He)for@$Ie;&$_($Me)for@$r1;&$_($Pe)for@$r1;&$_($Se)for@$Te;&$_($Ve)for@$We;&$_($if)for@$r1;&$_($kf)for@$lf;ni->run(@ARGV);
__DATA__
