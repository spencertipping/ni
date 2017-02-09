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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fd.c';$u=q'/unix/fifo.c';$v=q'/unix/file.c';$w=q'/unix/io.c';$x=q'/unix/pid.c';$y={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1};$z={$p,1};$A={$j,1,$k,1,$p,1,$q,1};$B=[undef];$C=q'/metaclass';$D=bless({$c,$A,$f,$j,$g,$B},$C);$E=q'/metaclass::ctors';$F=[$D];$G=bless({$c,$z,$f,$p,$g,$F},$C);$H=q'/metaclass::ctors';$I=q'/lib/slice';$J={$I,1};$K=q'/lib/behavior';$L=q'/lib/branch';$M=q'/lib/tag';$N={$K,1,$L,1,$I,1,$M,1};$O=q'/class';$P=q'/lib/doc';$Q=q'/lib/fn';$R=q'/lib/image';$S=q'/lib/ni';$T=q'/object';$U=q'/unix/cat';$V=q'/unix/fd';$W=q'/unix/fifo';$X=q'/unix/file';$Y=q'/unix/io';$Z=q'/unix/pid';$c1={$O,1,$h,1,$K,1,$j,1,$L,1,$k,1,$P,1,$l,1,$Q,1,$m,1,$R,1,$n,1,$S,1,$o,1,$I,1,$p,1,$M,1,$q,1,$C,1,$d,1,$T,1,$r,1,$U,1,$s,1,$V,1,$t,1,$W,1,$u,1,$X,1,$v,1,$Y,1,$w,1,$Z,1,$x,1};$d1={};$e1=q'ctor';$f1=undef;$g1=q'dtor';$h1=q'methods';$i1=q'class';$j1={$m,1};$k1=[undef];$l1=bless({$c,$j1,$f,$m,$g,$k1},$C);$m1=q'/metaclass::ctors';$n1={$Q,1};$o1={};$p1=q'DESTROY';$q1=q'code';$r1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$s1=bless({$q1,$r1},$Q);$t1=q'/lib/fn::ctors';$u1=q'new';$v1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$w1=bless({$q1,$v1},$Q);$x1={$p1,$s1,$u1,$w1};$y1=q'/lib/instantiable.b';$z1=bless({$c,$o1,$h1,$x1,$f,$y1},$I);$A1=q'/lib/slice::ctors';$B1={};$C1=q'shift->compile';$D1=bless({$q1,$C1},$Q);$E1=q'compile';$F1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$G1=bless({$q1,$F1},$Q);$H1=q'instantiate';$I1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$J1=bless({$q1,$I1},$Q);$K1={$E1,$G1,$H1,$J1};$L1=q'/lib/fn_init.b';$M1=bless({$c,$B1,$e1,$D1,$g1,$f1,$h1,$K1,$f,$L1},$I);$N1=q'/lib/slice::ctors';$O1={};$P1=q'serialize';$Q1=q'annotations';$R1=[];$S1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$T1=bless({$Q1,$R1,$q1,$S1},$Q);$U1={$P1,$T1};$V1=q'/lib/fn_serialize.b';$W1=bless({$c,$O1,$e1,$f1,$g1,$f1,$h1,$U1,$f,$V1},$I);$X1=q'/lib/slice::ctors';$Y1=[undef,$z1,$M1,$W1];$Z1=bless({$c,$n1,$f,$Q,$g,$Y1},$m);$c2=q'/lib/fn.c::ctors';$d2=q'ni \'ni:\' . ref shift';$e2=bless({$q1,$d2},$Q);$f2={$i1,$e2};$g2=q'/lib/instance.b';$h2=bless({$c,$d1,$e1,$f1,$g1,$f1,$h1,$f2,$f,$g2},$I);$i2=q'/lib/slice::ctors';$j2=[$h2];$k2=bless({$c,$c1,$f,$T,$g,$j2},$r);$l2=q'/object.c::ctors';$m2=[$k2];$n2=bless({$c,$N,$f,$K,$g,$m2},$j);$o2=q'/lib/behavior.c::ctors';$p2={};$q2=q'my $s = shift; ni->def($s->name, $s)';$r2=bless({$q1,$q2},$Q);$s2=q'$_[0]->namespace . ":" . $_[0]->{name}';$t2=bless({$q1,$s2},$Q);$u2={$f,$t2};$v2=q'/lib/named.b';$w2=bless({$c,$p2,$e1,$r2,$g1,$f1,$h1,$u2,$f,$v2},$I);$x2=q'/lib/slice::ctors';$y2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$z2=bless({$q1,$y2},$Q);$A2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$B2=bless({$q1,$A2},$Q);$C2=q'/lib/slice::apply';$D2=q'/lib/slice::apply_unsafe';$E2={};$F2=q'apply';$G2=q'apply_unsafe';$H2={$F2,$z2,$G2,$B2};$I2=q'/lib/slice.b';$J2=bless({$c,$E2,$h1,$H2,$f,$I2},$I);$K2=q'/lib/slice::ctors';$L2={};$M2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$N2=bless({$q1,$M2},$Q);$O2={$H1,$N2};$P2=q'/lib/slice_init.b';$Q2=bless({$c,$L2,$h1,$O2,$f,$P2},$I);$R2=q'/lib/slice::ctors';$S2={};$T2=[];$U2=q'local $_;
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
$g;';$V2=bless({$Q1,$T2,$q1,$U2},$Q);$W2={$P1,$V2};$X2=q'/lib/slice_serialize.b';$Y2=bless({$c,$S2,$e1,$f1,$g1,$f1,$h1,$W2,$f,$X2},$I);$Z2=q'/lib/slice::ctors';$c3=[$n2,$w2,$J2,$Q2,$Y2];$d3=bless({$c,$J,$f,$I,$g,$c3},$p);$e3=q'/lib/slice.c::ctors';$f3={};$g3=q'doc';$h3=[];$i3=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$j3=bless({$Q1,$h3,$q1,$i3},$Q);$k3={$g3,$j3};$l3=q'/lib/documentable.b';$m3=bless({$c,$f3,$e1,$f1,$g1,$f1,$h1,$k3,$f,$l3},$I);$n3=q'/lib/slice::ctors';$o3=[undef,$m3];$p3=bless({$c,$y,$f,$r,$g,$o3},$C);$q3=q'/metaclass::ctors';$r3=[$p3];$s3=bless({$c,$i,$f,$h,$g,$r3},$C);$t3=q'/metaclass::ctors';$u3={$O,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1};$v3={$q,1};$w3=[$D];$x3=bless({$c,$v3,$f,$q,$g,$w3},$C);$y3=q'/metaclass::ctors';$z3={$M,1};$A3={};$B3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$C3=bless({$q1,$B3},$Q);$D3={$F2,$C3};$E3=q'/lib/tag.b';$F3=bless({$c,$A3,$e1,$f1,$g1,$f1,$h1,$D3,$f,$E3},$I);$G3=q'/lib/slice::ctors';$H3={};$I3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$J3=bless({$q1,$I3},$Q);$K3={$H1,$J3};$L3=q'/lib/tag_init.b';$M3=bless({$c,$H3,$e1,$f1,$g1,$f1,$h1,$K3,$f,$L3},$I);$N3=q'/lib/slice::ctors';$O3=[$n2,$w2,$F3,$M3];$P3=bless({$c,$z3,$f,$M,$g,$O3},$q);$Q3=q'/lib/tag.c::ctors';$R3=q'/lib/perlbranch.b';$S3={};$T3=q'add';$U3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$V3=bless({$q1,$U3},$Q);$W3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$X3=bless({$q1,$W3},$Q);$Y3={$T3,$V3,$F2,$X3};$Z3=q'/lib/branch.b';$c4=bless({$c,$S3,$e1,$f1,$g1,$f1,$h1,$Y3,$f,$Z3},$I);$d4=q'/lib/slice::ctors';$e4={};$f4=q'namespace';$g4=q'\'ni\'';$h4=bless({$q1,$g4},$Q);$i4={$f4,$h4};$j4=q'/lib/named_in_ni.b';$k4=bless({$c,$e4,$e1,$f1,$g1,$f1,$h1,$i4,$f,$j4},$I);$l4=q'/lib/slice::ctors';$m4={};$n4=q'package';$o4=q'shift->{name}';$p4=bless({$q1,$o4},$Q);$q4={$n4,$p4};$r4=q'/lib/namespaced.b';$s4=bless({$c,$m4,$e1,$f1,$g1,$f1,$h1,$q4,$f,$r4},$I);$t4=q'/lib/slice::ctors';$u4={};$v4=q'resolve';$w4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$x4=bless({$q1,$w4},$Q);$y4={$v4,$x4};$z4=q'/lib/resolver.b';$A4=bless({$c,$u4,$e1,$f1,$g1,$f1,$h1,$y4,$f,$z4},$I);$B4=q'/lib/slice::ctors';$C4=[$c4,$z1,$w2,$k4,$s4,$A4];$D4=bless({$f,$R3,$g,$C4},$M);$E4=q'/lib/tag::ctors';$F4={};$G4=q'my $s = shift; $s->apply($s->package)';$H4=bless({$q1,$G4},$Q);$I4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$J4=bless({$q1,$I4},$Q);$K4={$H1,$J4};$L4=q'/lib/class_init.b';$M4=bless({$c,$F4,$e1,$H4,$g1,$f1,$h1,$K4,$f,$L4},$I);$N4=q'/lib/slice::ctors';$O4={$k,1};$P4=[$D];$Q4=bless({$c,$O4,$f,$k,$g,$P4},$C);$R4=q'/metaclass::ctors';$S4={$L,1};$T4={};$U4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$V4=bless({$q1,$U4},$Q);$W4={$H1,$V4};$X4=q'/lib/branch_init.b';$Y4=bless({$c,$T4,$e1,$f1,$g1,$f1,$h1,$W4,$f,$X4},$I);$Z4=q'/lib/slice::ctors';$c5=[$n2,$w2,$c4,$Y4,undef];$d5=bless({$c,$S4,$f,$L,$g,$c5},$k);$e5=q'/lib/branch.c::ctors';$f5={$O,1,$h,1,$j,1,$L,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$C,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1};$g5=q'/lib/definition.b';$h5={};$i5=q'def';$j5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$k5=bless({$q1,$j5},$Q);$l5={$i5,$k5};$m5=q'/lib/classdef.b';$n5=bless({$c,$h5,$e1,$f1,$g1,$f1,$h1,$l5,$f,$m5},$I);$o5=q'/lib/slice::ctors';$p5={};$q5=q'ro';$r5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$s5=bless({$q1,$r5},$Q);$t5=q'rw';$u5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$v5=bless({$q1,$u5},$Q);$w5={$q5,$s5,$t5,$v5};$x5=q'/lib/accessor.b';$y5=bless({$c,$p5,$e1,$f1,$g1,$f1,$h1,$w5,$f,$x5},$I);$z5=q'/lib/slice::ctors';$A5=[$n5,$y5];$B5=bless({$c,$f5,$f,$g5,$g,$A5},$L);$C5=q'/lib/branch::ctors';$D5={};$E5=q'child';$F5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$G5=bless({$q1,$F5},$Q);$H5={$E5,$G5};$I5=q'/lib/subclass.b';$J5=bless({$c,$D5,$e1,$f1,$g1,$f1,$h1,$H5,$f,$I5},$I);$K5=q'/lib/slice::ctors';$L5=[$D4,$M4,$k2,$B5,$J5];$M5=bless({$c,$u3,$f,$O,$g,$L5},$h);$N5=q'/class.c::ctors';$O5=[$M5];$P5=bless({$c,$e,$f,$d,$g,$O5},$C);$Q5=q'/metaclass::ctors';$R5={$C,1};$S5=[$D4,$M4,$k2,$B5];$T5=bless({$c,$R5,$f,$C,$g,$S5},$d);$U5=q'/metaclass.c::ctors';$V5={$o,1};$W5=[$p3];$X5=bless({$c,$V5,$f,$o,$g,$W5},$C);$Y5=q'/metaclass::ctors';$Z5={$S,1};$c6={};$d6=q'is_mutable';$e6=[];$f6=q'$0 ne "-" && -w $0';$g6=bless({$Q1,$e6,$q1,$f6},$Q);$h6=q'modify';$i6=[];$j6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$k6=bless({$Q1,$i6,$q1,$j6},$Q);$l6={$d6,$g6,$h6,$k6};$m6=q'/lib/ni_self.b';$n6=bless({$c,$c6,$e1,$f1,$g1,$f1,$h1,$l6,$f,$m6},$I);$o6=q'/lib/slice::ctors';$p6={};$q6=q'exists';$r6=[];$s6=q'exists $_[0]->{named}{$_[1]}';$t6=bless({$Q1,$r6,$q1,$s6},$Q);$u6=q'quoted';$v6=[];$w6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$x6=bless({$Q1,$v6,$q1,$w6},$Q);$y6={$q6,$t6,$u6,$x6};$z6=q'/lib/ni_image.b';$A6=bless({$c,$p6,$e1,$f1,$g1,$f1,$h1,$y6,$f,$z6},$I);$B6=q'/lib/slice::ctors';$C6={};$D6=q'--internal/+=';$E6=[];$F6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$G6=bless({$Q1,$E6,$q1,$F6},$Q);$H6=q'--internal/eval';$I6=[];$J6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$K6=bless({$Q1,$I6,$q1,$J6},$Q);$L6=q'--internal/image';$M6=[];$N6=q'shift->quoted->write(\\*STDOUT);
0;';$O6=bless({$Q1,$M6,$q1,$N6},$Q);$P6=q'run';$Q6=[];$R6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$S6=bless({$Q1,$Q6,$q1,$R6},$Q);$T6={$D6,$G6,$H6,$K6,$L6,$O6,$P6,$S6};$U6=q'/lib/ni_main.b';$V6=bless({$c,$C6,$e1,$f1,$g1,$f1,$h1,$T6,$f,$U6},$I);$W6=q'/lib/slice::ctors';$X6={};$Y6=[];$Z6=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$c7=bless({$Q1,$Y6,$q1,$Z6},$Q);$d7=q'resolver_for';$e7=[];$f7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$g7=bless({$Q1,$e7,$q1,$f7},$Q);$h7={$v4,$c7,$d7,$g7};$i7=q'/lib/ni_resolver.b';$j7=bless({$c,$X6,$e1,$f1,$g1,$f1,$h1,$h7,$f,$i7},$I);$k7=q'/lib/slice::ctors';$l7=[$k2,$n6,$A6,$V6,$j7];$m7=bless({$c,$Z5,$f,$S,$g,$l7},$o);$n7=q'/lib/ni.c::ctors';$o7=q'named';$p7=q'ni.doc:/class';$q7={$l,1};$r7=[$p3];$s7=bless({$c,$q7,$f,$l,$g,$r7},$C);$t7=q'/metaclass::ctors';$u7={$P,1};$v7={};$w7=[];$x7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$y7=bless({$Q1,$w7,$q1,$x7},$Q);$z7={$H1,$y7};$A7=q'/lib/doc_init.b';$B7=bless({$c,$v7,$e1,$f1,$g1,$f1,$h1,$z7,$f,$A7},$I);$C7=q'/lib/slice::ctors';$D7={};$E7=[];$F7=q'\'ni.doc\'';$G7=bless({$Q1,$E7,$q1,$F7},$Q);$H7={$f4,$G7};$I7=q'/lib/doc_namespace.b';$J7=bless({$c,$D7,$e1,$f1,$g1,$f1,$h1,$H7,$f,$I7},$I);$K7=q'/lib/slice::ctors';$L7=[$k2,$w2,$B7,$J7];$M7=bless({$c,$u7,$f,$P,$g,$L7},$l);$N7=q'/lib/doc.c::ctors';$O7=q'apropos';$P7=q'ni:/class';$Q7=[$P7];$R7=q'# Classes and metaclasses
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
';$S7=bless({$O7,$Q7,$g3,$R7,$f,$O},$P);$T7=q'/lib/doc::ctors';$U7=q'ni:/class.c';$V7=q'ni:/lib/accessor.b';$W7=q'ni:/lib/behavior';$X7=q'ni:/lib/behavior.c';$Y7=q'ni:/lib/branch';$Z7=q'ni:/lib/branch.b';$c8=q'ni:/lib/branch.c';$d8=q'ni:/lib/branch_init.b';$e8=q'ni:/lib/class_init.b';$f8=q'ni:/lib/classdef.b';$g8=q'ni:/lib/definition.b';$h8=q'ni:/lib/doc';$i8=q'ni:/lib/doc.c';$j8=q'ni:/lib/doc_init.b';$k8=q'ni:/lib/doc_namespace.b';$l8=q'ni:/lib/documentable.b';$m8=q'ni:/lib/fn';$n8=q'ni:/lib/fn.c';$o8=q'ni:/lib/fn_init.b';$p8=q'ni:/lib/fn_serialize.b';$q8=q'ni:/lib/image';$r8={$n,1};$s8=[$p3];$t8=bless({$c,$r8,$f,$n,$g,$s8},$C);$u8=q'/metaclass::ctors';$v8={$R,1};$w8={};$x8=[];$y8=q'my $class = shift;
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
  ordering     => []};';$z8=bless({$Q1,$x8,$q1,$y8},$Q);$A8={$H1,$z8};$B8=q'/lib/image_init.b';$C8=bless({$c,$w8,$e1,$f1,$g1,$f1,$h1,$A8,$f,$B8},$I);$D8=q'/lib/slice::ctors';$E8={};$F8=q'address';$G8=[];$H8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$I8=bless({$Q1,$G8,$q1,$H8},$Q);$J8=q'allocate_gensym';$K8=[];$L8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$M8=bless({$Q1,$K8,$q1,$L8},$Q);$N8=q'boot_side_effect';$O8=[];$P8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Q8=bless({$Q1,$O8,$q1,$P8},$Q);$R8=q'circular_links';$S8=[];$T8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$U8=bless({$Q1,$S8,$q1,$T8},$Q);$V8=q'finalizer';$W8=[];$X8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$Y8=bless({$Q1,$W8,$q1,$X8},$Q);$Z8=q'gensym';$c9=[];$d9=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$e9=bless({$Q1,$c9,$q1,$d9},$Q);$f9=q'is_circular';$g9=[];$h9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$i9=bless({$Q1,$g9,$q1,$h9},$Q);$j9=q'quote';$k9=[];$l9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$m9=bless({$Q1,$k9,$q1,$l9},$Q);$n9=q'quote_array';$o9=[];$p9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$q9=bless({$Q1,$o9,$q1,$p9},$Q);$r9=q'quote_blessed';$s9=[];$t9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$u9=bless({$Q1,$s9,$q1,$t9},$Q);$v9=q'quote_class';$w9=[];$x9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$y9=bless({$Q1,$w9,$q1,$x9},$Q);$z9=q'quote_hash';$A9=[];$B9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$C9=bless({$Q1,$A9,$q1,$B9},$Q);$D9=q'quote_object';$E9=[];$F9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$G9=bless({$Q1,$E9,$q1,$F9},$Q);$H9=q'quote_scalar';$I9=[];$J9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$K9=bless({$Q1,$I9,$q1,$J9},$Q);$L9=q'quote_value';$M9=[];$N9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$O9=bless({$Q1,$M9,$q1,$N9},$Q);$P9=q'reconstruction';$Q9=[];$R9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$S9=bless({$Q1,$Q9,$q1,$R9},$Q);$T9=q'side_effect';$U9=[];$V9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$W9=bless({$Q1,$U9,$q1,$V9},$Q);$X9=q'write';$Y9=[];$Z9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$ca=bless({$Q1,$Y9,$q1,$Z9},$Q);$da={$F8,$I8,$J8,$M8,$N8,$Q8,$R8,$U8,$V8,$Y8,$Z8,$e9,$f9,$i9,$j9,$m9,$n9,$q9,$r9,$u9,$v9,$y9,$z9,$C9,$D9,$G9,$H9,$K9,$L9,$O9,$P9,$S9,$T9,$W9,$X9,$ca};$ea=q'/lib/image_quoting.b';$fa=bless({$c,$E8,$e1,$f1,$g1,$f1,$h1,$da,$f,$ea},$I);$ga=q'/lib/slice::ctors';$ha=[$k2,$C8,$fa];$ia=bless({$c,$v8,$f,$R,$g,$ha},$n);$ja=q'/lib/image.c::ctors';$ka=q'ni:/lib/image.c';$la=q'ni:/lib/image_init.b';$ma=q'ni:/lib/image_quoting.b';$na=q'ni:/lib/instance.b';$oa=q'ni:/lib/instantiable.b';$pa=q'ni:/lib/named.b';$qa=q'ni:/lib/named_in_ni.b';$ra=q'ni:/lib/namespaced.b';$sa=q'ni:/lib/ni';$ta=q'ni:/lib/ni.c';$ua=q'ni:/lib/ni_image.b';$va=q'ni:/lib/ni_main.b';$wa=q'ni:/lib/ni_resolver.b';$xa=q'ni:/lib/ni_self.b';$ya=q'ni:/lib/perlbranch.b';$za=q'ni:/lib/resolver.b';$Aa=q'ni:/lib/slice';$Ba=q'ni:/lib/slice.b';$Ca=q'ni:/lib/slice.c';$Da=q'ni:/lib/slice_init.b';$Ea=q'ni:/lib/slice_serialize.b';$Fa=q'ni:/lib/subclass.b';$Ga=q'ni:/lib/tag';$Ha=q'ni:/lib/tag.b';$Ia=q'ni:/lib/tag.c';$Ja=q'ni:/lib/tag_init.b';$Ka=q'ni:/metaclass';$La=q'ni:/metaclass.c';$Ma=q'ni:/object';$Na=q'ni:/object.c';$Oa=q'ni:/unix/cat';$Pa={$s,1};$Qa={$s,1,$t,1,$v,1,$w,1,$x,1};$Ra=[$p3];$Sa=bless({$c,$Qa,$f,$w,$g,$Ra},$C);$Ta=q'/metaclass::ctors';$Ua=[$Sa];$Va=bless({$c,$Pa,$f,$s,$g,$Ua},$C);$Wa=q'/metaclass::ctors';$Xa={$U,1};$Ya={$U,1,$V,1,$X,1,$Y,1,$Z,1};$Za={};$cb=q'into';$db=[];$eb=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$fb=bless({$Q1,$db,$q1,$eb},$Q);$gb={$cb,$fb};$hb=q'/unix/io_stream.b';$ib=bless({$c,$Za,$e1,$f1,$g1,$f1,$h1,$gb,$f,$hb},$I);$jb=q'/lib/slice::ctors';$kb={};$lb=q'(+';$mb=[];$nb=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$ob=bless({$Q1,$mb,$q1,$nb},$Q);$pb={$lb,$ob};$qb=q'/unix/io_constructors.b';$rb=bless({$c,$kb,$e1,$f1,$g1,$f1,$h1,$pb,$f,$qb},$I);$sb=q'/lib/slice::ctors';$tb={};$ub=q'(<>';$vb=[];$wb=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$xb=bless({$Q1,$vb,$q1,$wb},$Q);$yb=q'(@{}';$zb=[];$Ab=q'my $self = shift; [<$self>]';$Bb=bless({$Q1,$zb,$q1,$Ab},$Q);$Cb={$ub,$xb,$yb,$Bb};$Db=q'/unix/io_readers.b';$Eb=bless({$c,$tb,$e1,$f1,$g1,$f1,$h1,$Cb,$f,$Db},$I);$Fb=q'/lib/slice::ctors';$Gb=[$k2,$ib,$rb,$Eb];$Hb=bless({$c,$Ya,$f,$Y,$g,$Gb},$w);$Ib=q'/unix/io.c::ctors';$Jb={};$Kb=[];$Lb=q'shift; +{fs => [@_]}';$Mb=bless({$Q1,$Kb,$q1,$Lb},$Q);$Nb={$H1,$Mb};$Ob=q'/unix/cat_init.b';$Pb=bless({$c,$Jb,$e1,$f1,$g1,$f1,$h1,$Nb,$f,$Ob},$I);$Qb=q'/lib/slice::ctors';$Rb={};$Sb=q'read';$Tb=[];$Ub=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Vb=bless({$Q1,$Tb,$q1,$Ub},$Q);$Wb={$Sb,$Vb};$Xb=q'/unix/cat_read.b';$Yb=bless({$c,$Rb,$e1,$f1,$g1,$f1,$h1,$Wb,$f,$Xb},$I);$Zb=q'/lib/slice::ctors';$cc=[$Hb,$Pb,$Yb];$dc=bless({$c,$Xa,$f,$U,$g,$cc},$s);$ec=q'/unix/cat.c::ctors';$fc=q'ni:/unix/cat.c';$gc=q'ni:/unix/cat_init.b';$hc=q'ni:/unix/cat_read.b';$ic=q'ni:/unix/fd';$jc={$t,1};$kc=[$Sa];$lc=bless({$c,$jc,$f,$t,$g,$kc},$C);$mc=q'/metaclass::ctors';$nc={$V,1};$oc={};$pc=q'fd';$qc=[];$rc=q'shift->{\'fd\'}';$sc=bless({$Q1,$qc,$q1,$rc},$Q);$tc={$pc,$sc};$uc=q'/unix/fd_readers.b';$vc=bless({$c,$oc,$e1,$f1,$g1,$f1,$h1,$tc,$f,$uc},$I);$wc=q'/lib/slice::ctors';$xc={};$yc=[];$zc=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$Ac=bless({$Q1,$yc,$q1,$zc},$Q);$Bc={$H1,$Ac};$Cc=q'/unix/fd_init.b';$Dc=bless({$c,$xc,$e1,$f1,$g1,$f1,$h1,$Bc,$f,$Cc},$I);$Ec=q'/lib/slice::ctors';$Fc={};$Gc=q'move_to';$Hc=[];$Ic=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Jc=bless({$Q1,$Hc,$q1,$Ic},$Q);$Kc={$Gc,$Jc};$Lc=q'/unix/fd_shell.b';$Mc=bless({$c,$Fc,$e1,$f1,$g1,$f1,$h1,$Kc,$f,$Lc},$I);$Nc=q'/lib/slice::ctors';$Oc={$V,1,$W,1,$X,1};$Pc=q'/unix/has_fd.b';$Qc={};$Rc=[];$Sc=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Tc=bless({$Q1,$Rc,$q1,$Sc},$Q);$Uc=[];$Vc=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Wc=bless({$Q1,$Uc,$q1,$Vc},$Q);$Xc={$Sb,$Tc,$X9,$Wc};$Yc=q'/unix/fd_safeio.b';$Zc=bless({$c,$Qc,$e1,$f1,$g1,$f1,$h1,$Xc,$f,$Yc},$I);$cd=q'/lib/slice::ctors';$dd=[$Zc];$ed=bless({$c,$Oc,$f,$Pc,$g,$dd},$L);$fd=q'/lib/branch::ctors';$gd={};$hd=q'read_fh';$id=[];$jd=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$kd=bless({$Q1,$id,$q1,$jd},$Q);$ld=q'write_fh';$md=[];$nd=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$od=bless({$Q1,$md,$q1,$nd},$Q);$pd={$hd,$kd,$ld,$od};$qd=q'/unix/fd_io.b';$rd=bless({$c,$gd,$e1,$f1,$g1,$f1,$h1,$pd,$f,$qd},$I);$sd=q'/lib/slice::ctors';$td=[$Hb,$vc,$Dc,$Mc,$ed,$rd];$ud=bless({$c,$nc,$f,$V,$g,$td},$t);$vd=q'/unix/fd.c::ctors';$wd=q'ni:/unix/fd.c';$xd=q'ni:/unix/fd_init.b';$yd=q'ni:/unix/fd_io.b';$zd=q'ni:/unix/fd_readers.b';$Ad=q'ni:/unix/fd_safeio.b';$Bd=q'ni:/unix/fd_shell.b';$Cd=q'ni:/unix/fifo';$Dd={$u,1};$Ed=[$p3];$Fd=bless({$c,$Dd,$f,$u,$g,$Ed},$C);$Gd=q'/metaclass::ctors';$Hd={$W,1};$Id={};$Jd=[];$Kd=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Ld=bless({$Q1,$Jd,$q1,$Kd},$Q);$Md={$H1,$Ld};$Nd=q'/unix/fifo_init.b';$Od=bless({$c,$Id,$e1,$f1,$g1,$f1,$h1,$Md,$f,$Nd},$I);$Pd=q'/lib/slice::ctors';$Qd={};$Rd=[];$Sd=q'shift->{read_fh}';$Td=bless({$Q1,$Rd,$q1,$Sd},$Q);$Ud=[];$Vd=q'shift->{write_fh}';$Wd=bless({$Q1,$Ud,$q1,$Vd},$Q);$Xd={$hd,$Td,$ld,$Wd};$Yd=q'/unix/file_io.b';$Zd=bless({$c,$Qd,$e1,$f1,$g1,$f1,$h1,$Xd,$f,$Yd},$I);$ce=q'/lib/slice::ctors';$de=[$k2,$Od,$ed,$Zd];$ee=bless({$c,$Hd,$f,$W,$g,$de},$u);$fe=q'/unix/fifo.c::ctors';$ge=q'ni:/unix/fifo.c';$he=q'ni:/unix/fifo_init.b';$ie=q'ni:/unix/file';$je={$v,1};$ke=[$Sa];$le=bless({$c,$je,$f,$v,$g,$ke},$C);$me=q'/metaclass::ctors';$ne={$X,1};$oe={};$pe=[];$qe=q'shift->{\'name\'}';$re=bless({$Q1,$pe,$q1,$qe},$Q);$se={$f,$re};$te=q'/unix/file_readers.b';$ue=bless({$c,$oe,$e1,$f1,$g1,$f1,$h1,$se,$f,$te},$I);$ve=q'/lib/slice::ctors';$we={};$xe=[];$ye=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$ze=bless({$Q1,$xe,$q1,$ye},$Q);$Ae={$H1,$ze};$Be=q'/unix/file_init.b';$Ce=bless({$c,$we,$e1,$f1,$g1,$f1,$h1,$Ae,$f,$Be},$I);$De=q'/lib/slice::ctors';$Ee={};$Fe=[];$Ge=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$He=bless({$Q1,$Fe,$q1,$Ge},$Q);$Ie=[];$Je=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Ke=bless({$Q1,$Ie,$q1,$Je},$Q);$Le={$hd,$He,$ld,$Ke};$Me=bless({$c,$Ee,$e1,$f1,$g1,$f1,$h1,$Le,$f,$Yd},$I);$Ne=q'/lib/slice::ctors';$Oe=[$Hb,$ue,$Ce,$ed,$Me];$Pe=bless({$c,$ne,$f,$X,$g,$Oe},$v);$Qe=q'/unix/file.c::ctors';$Re=q'ni:/unix/file.c';$Se=q'ni:/unix/file_init.b';$Te=q'ni:/unix/file_io.b';$Ue=q'ni:/unix/file_readers.b';$Ve=q'ni:/unix/has_fd.b';$We=q'ni:/unix/io';$Xe=q'ni:/unix/io.c';$Ye=q'ni:/unix/io_constructors.b';$Ze=q'ni:/unix/io_readers.b';$cf=q'ni:/unix/io_stream.b';$df=q'ni:/unix/pid';$ef={$x,1};$ff=[$Sa];$gf=bless({$c,$ef,$f,$x,$g,$ff},$C);$hf=q'/metaclass::ctors';$if={$Z,1};$jf={};$kf=q'pid';$lf=[];$mf=q'shift->{\'pid\'}';$nf=bless({$Q1,$lf,$q1,$mf},$Q);$of=q'stderr';$pf=[];$qf=q'shift->{\'stderr\'}';$rf=bless({$Q1,$pf,$q1,$qf},$Q);$sf=q'stdin';$tf=[];$uf=q'shift->{\'stdin\'}';$vf=bless({$Q1,$tf,$q1,$uf},$Q);$wf=q'stdout';$xf=[];$yf=q'shift->{\'stdout\'}';$zf=bless({$Q1,$xf,$q1,$yf},$Q);$Af={$kf,$nf,$of,$rf,$sf,$vf,$wf,$zf};$Bf=q'/unix/pid_readers.b';$Cf=bless({$c,$jf,$e1,$f1,$g1,$f1,$h1,$Af,$f,$Bf},$I);$Df=q'/lib/slice::ctors';$Ef={};$Ff=[];$Gf=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Hf=bless({$Q1,$Ff,$q1,$Gf},$Q);$If={$H1,$Hf};$Jf=q'/unix/pid_init.b';$Kf=bless({$c,$Ef,$e1,$f1,$g1,$f1,$h1,$If,$f,$Jf},$I);$Lf=q'/lib/slice::ctors';$Mf={};$Nf=[];$Of=q'shift->{stdout}';$Pf=bless({$Q1,$Nf,$q1,$Of},$Q);$Qf=[];$Rf=q'shift->{stdin}';$Sf=bless({$Q1,$Qf,$q1,$Rf},$Q);$Tf={$hd,$Pf,$ld,$Sf};$Uf=q'/unix/pid_io.b';$Vf=bless({$c,$Mf,$e1,$f1,$g1,$f1,$h1,$Tf,$f,$Uf},$I);$Wf=q'/lib/slice::ctors';$Xf=[$Hb,$Cf,$Kf,$Vf];$Yf=bless({$c,$if,$f,$Z,$g,$Xf},$x);$Zf=q'/unix/pid.c::ctors';$cg=q'ni:/unix/pid.c';$dg=q'ni:/unix/pid_init.b';$eg=q'ni:/unix/pid_io.b';$fg=q'ni:/unix/pid_readers.b';$gg={$p7,$S7,$P7,$M5,$U7,$s3,$V7,$y5,$W7,$n2,$X7,$D,$Y7,$d5,$Z7,$c4,$c8,$Q4,$d8,$Y4,$e8,$M4,$f8,$n5,$g8,$B5,$h8,$M7,$i8,$s7,$j8,$B7,$k8,$J7,$l8,$m3,$m8,$Z1,$n8,$l1,$o8,$M1,$p8,$W1,$q8,$ia,$ka,$t8,$la,$C8,$ma,$fa,$na,$h2,$oa,$z1,$pa,$w2,$qa,$k4,$ra,$s4,$sa,$m7,$ta,$X5,$ua,$A6,$va,$V6,$wa,$j7,$xa,$n6,$ya,$D4,$za,$A4,$Aa,$d3,$Ba,$J2,$Ca,$G,$Da,$Q2,$Ea,$Y2,$Fa,$J5,$Ga,$P3,$Ha,$F3,$Ia,$x3,$Ja,$M3,$Ka,$T5,$La,$P5,$Ma,$k2,$Na,$p3,$Oa,$dc,$fc,$Va,$gc,$Pb,$hc,$Yb,$ic,$ud,$wd,$lc,$xd,$Dc,$yd,$rd,$zd,$vc,$Ad,$Zc,$Bd,$Mc,$Cd,$ee,$ge,$Fd,$he,$Od,$ie,$Pe,$Re,$le,$Se,$Ce,$Te,$Me,$Ue,$ue,$Ve,$ed,$We,$Hb,$Xe,$Sa,$Ye,$rb,$Ze,$Eb,$cf,$ib,$df,$Yf,$cg,$gf,$dg,$Kf,$eg,$Vf,$fg,$Cf};$hg=q'resolvers';$ig=[];$jg=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$kg=bless({$Q1,$ig,$q1,$jg},$Q);$lg=q'file';$mg=[];$ng=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$og=bless({$Q1,$mg,$q1,$ng},$Q);$pg={$pc,$kg,$lg,$og};$qg=bless({$o7,$gg,$hg,$pg},$S);$rg=q'/lib/ni::ctors';$$o3[0]=$M5;$$B[0]=$p3;$$k1[0]=$p3;$$Y1[0]=$k2;$$c5[4]=$B5;*$D2=\&$B2;*$C2=\&$z2;$z1->apply_unsafe($O);$z1->apply_unsafe($h);$z1->apply_unsafe($j);$z1->apply_unsafe($L);$z1->apply_unsafe($k);$z1->apply_unsafe($l);$z1->apply_unsafe($Q);$z1->apply_unsafe($m);$z1->apply_unsafe($n);$z1->apply_unsafe($o);$z1->apply_unsafe($I);$z1->apply_unsafe($p);$z1->apply_unsafe($M);$z1->apply_unsafe($q);$z1->apply_unsafe($C);$z1->apply_unsafe($d);$z1->apply_unsafe($r);$z1->apply_unsafe($s);$z1->apply_unsafe($t);$z1->apply_unsafe($u);$z1->apply_unsafe($v);$z1->apply_unsafe($w);$z1->apply_unsafe($x);$M1->apply_unsafe($Q);$W1->apply_unsafe($Q);$h2->apply_unsafe($O);$h2->apply_unsafe($h);$h2->apply_unsafe($K);$h2->apply_unsafe($j);$h2->apply_unsafe($L);$h2->apply_unsafe($k);$h2->apply_unsafe($P);$h2->apply_unsafe($l);$h2->apply_unsafe($Q);$h2->apply_unsafe($m);$h2->apply_unsafe($R);$h2->apply_unsafe($n);$h2->apply_unsafe($S);$h2->apply_unsafe($o);$h2->apply_unsafe($I);$h2->apply_unsafe($p);$h2->apply_unsafe($M);$h2->apply_unsafe($q);$h2->apply_unsafe($C);$h2->apply_unsafe($d);$h2->apply_unsafe($T);$h2->apply_unsafe($r);$h2->apply_unsafe($U);$h2->apply_unsafe($s);$h2->apply_unsafe($V);$h2->apply_unsafe($t);$h2->apply_unsafe($W);$h2->apply_unsafe($u);$h2->apply_unsafe($X);$h2->apply_unsafe($v);$h2->apply_unsafe($Y);$h2->apply_unsafe($w);$h2->apply_unsafe($Z);$h2->apply_unsafe($x);$w2->apply_unsafe($O);$w2->apply_unsafe($h);$w2->apply_unsafe($j);$w2->apply_unsafe($L);$w2->apply_unsafe($k);$w2->apply_unsafe($P);$w2->apply_unsafe($l);$w2->apply_unsafe($m);$w2->apply_unsafe($n);$w2->apply_unsafe($o);$w2->apply_unsafe($I);$w2->apply_unsafe($p);$w2->apply_unsafe($M);$w2->apply_unsafe($q);$w2->apply_unsafe($C);$w2->apply_unsafe($d);$w2->apply_unsafe($r);$w2->apply_unsafe($s);$w2->apply_unsafe($t);$w2->apply_unsafe($u);$w2->apply_unsafe($v);$w2->apply_unsafe($w);$w2->apply_unsafe($x);$J2->apply_unsafe($I);$Q2->apply_unsafe($I);$Y2->apply_unsafe($I);$m3->apply_unsafe($h);$m3->apply_unsafe($j);$m3->apply_unsafe($k);$m3->apply_unsafe($l);$m3->apply_unsafe($m);$m3->apply_unsafe($n);$m3->apply_unsafe($o);$m3->apply_unsafe($p);$m3->apply_unsafe($q);$m3->apply_unsafe($r);$m3->apply_unsafe($s);$m3->apply_unsafe($t);$m3->apply_unsafe($u);$m3->apply_unsafe($v);$m3->apply_unsafe($w);$m3->apply_unsafe($x);$F3->apply_unsafe($M);$M3->apply_unsafe($M);$c4->apply_unsafe($O);$c4->apply_unsafe($h);$c4->apply_unsafe($j);$c4->apply_unsafe($L);$c4->apply_unsafe($k);$c4->apply_unsafe($l);$c4->apply_unsafe($m);$c4->apply_unsafe($n);$c4->apply_unsafe($o);$c4->apply_unsafe($p);$c4->apply_unsafe($q);$c4->apply_unsafe($C);$c4->apply_unsafe($d);$c4->apply_unsafe($r);$c4->apply_unsafe($s);$c4->apply_unsafe($t);$c4->apply_unsafe($u);$c4->apply_unsafe($v);$c4->apply_unsafe($w);$c4->apply_unsafe($x);$k4->apply_unsafe($O);$k4->apply_unsafe($h);$k4->apply_unsafe($j);$k4->apply_unsafe($L);$k4->apply_unsafe($k);$k4->apply_unsafe($l);$k4->apply_unsafe($m);$k4->apply_unsafe($n);$k4->apply_unsafe($o);$k4->apply_unsafe($I);$k4->apply_unsafe($p);$k4->apply_unsafe($M);$k4->apply_unsafe($q);$k4->apply_unsafe($C);$k4->apply_unsafe($d);$k4->apply_unsafe($r);$k4->apply_unsafe($s);$k4->apply_unsafe($t);$k4->apply_unsafe($u);$k4->apply_unsafe($v);$k4->apply_unsafe($w);$k4->apply_unsafe($x);$s4->apply_unsafe($O);$s4->apply_unsafe($h);$s4->apply_unsafe($j);$s4->apply_unsafe($L);$s4->apply_unsafe($k);$s4->apply_unsafe($l);$s4->apply_unsafe($m);$s4->apply_unsafe($n);$s4->apply_unsafe($o);$s4->apply_unsafe($I);$s4->apply_unsafe($p);$s4->apply_unsafe($M);$s4->apply_unsafe($q);$s4->apply_unsafe($C);$s4->apply_unsafe($d);$s4->apply_unsafe($r);$s4->apply_unsafe($s);$s4->apply_unsafe($t);$s4->apply_unsafe($u);$s4->apply_unsafe($v);$s4->apply_unsafe($w);$s4->apply_unsafe($x);$A4->apply_unsafe($O);$A4->apply_unsafe($h);$A4->apply_unsafe($j);$A4->apply_unsafe($L);$A4->apply_unsafe($k);$A4->apply_unsafe($l);$A4->apply_unsafe($m);$A4->apply_unsafe($n);$A4->apply_unsafe($o);$A4->apply_unsafe($p);$A4->apply_unsafe($M);$A4->apply_unsafe($q);$A4->apply_unsafe($C);$A4->apply_unsafe($d);$A4->apply_unsafe($r);$A4->apply_unsafe($s);$A4->apply_unsafe($t);$A4->apply_unsafe($u);$A4->apply_unsafe($v);$A4->apply_unsafe($w);$A4->apply_unsafe($x);$M4->apply_unsafe($O);$M4->apply_unsafe($h);$M4->apply_unsafe($j);$M4->apply_unsafe($k);$M4->apply_unsafe($l);$M4->apply_unsafe($m);$M4->apply_unsafe($n);$M4->apply_unsafe($o);$M4->apply_unsafe($p);$M4->apply_unsafe($q);$M4->apply_unsafe($C);$M4->apply_unsafe($d);$M4->apply_unsafe($r);$M4->apply_unsafe($s);$M4->apply_unsafe($t);$M4->apply_unsafe($u);$M4->apply_unsafe($v);$M4->apply_unsafe($w);$M4->apply_unsafe($x);$Y4->apply_unsafe($L);$n5->apply_unsafe($O);$n5->apply_unsafe($h);$n5->apply_unsafe($j);$n5->apply_unsafe($L);$n5->apply_unsafe($k);$n5->apply_unsafe($l);$n5->apply_unsafe($m);$n5->apply_unsafe($n);$n5->apply_unsafe($o);$n5->apply_unsafe($p);$n5->apply_unsafe($q);$n5->apply_unsafe($C);$n5->apply_unsafe($d);$n5->apply_unsafe($r);$n5->apply_unsafe($s);$n5->apply_unsafe($t);$n5->apply_unsafe($u);$n5->apply_unsafe($v);$n5->apply_unsafe($w);$n5->apply_unsafe($x);$y5->apply_unsafe($O);$y5->apply_unsafe($h);$y5->apply_unsafe($j);$y5->apply_unsafe($L);$y5->apply_unsafe($k);$y5->apply_unsafe($l);$y5->apply_unsafe($m);$y5->apply_unsafe($n);$y5->apply_unsafe($o);$y5->apply_unsafe($p);$y5->apply_unsafe($q);$y5->apply_unsafe($C);$y5->apply_unsafe($d);$y5->apply_unsafe($r);$y5->apply_unsafe($s);$y5->apply_unsafe($t);$y5->apply_unsafe($u);$y5->apply_unsafe($v);$y5->apply_unsafe($w);$y5->apply_unsafe($x);$J5->apply_unsafe($O);$J5->apply_unsafe($h);$J5->apply_unsafe($j);$J5->apply_unsafe($k);$J5->apply_unsafe($l);$J5->apply_unsafe($m);$J5->apply_unsafe($n);$J5->apply_unsafe($o);$J5->apply_unsafe($p);$J5->apply_unsafe($q);$J5->apply_unsafe($d);$J5->apply_unsafe($r);$J5->apply_unsafe($s);$J5->apply_unsafe($t);$J5->apply_unsafe($u);$J5->apply_unsafe($v);$J5->apply_unsafe($w);$J5->apply_unsafe($x);$n6->apply_unsafe($S);$A6->apply_unsafe($S);$V6->apply_unsafe($S);$j7->apply_unsafe($S);$B7->apply_unsafe($P);$J7->apply_unsafe($P);$C8->apply_unsafe($R);$fa->apply_unsafe($R);$ib->apply_unsafe($U);$ib->apply_unsafe($V);$ib->apply_unsafe($X);$ib->apply_unsafe($Y);$ib->apply_unsafe($Z);$rb->apply_unsafe($U);$rb->apply_unsafe($V);$rb->apply_unsafe($X);$rb->apply_unsafe($Y);$rb->apply_unsafe($Z);$Eb->apply_unsafe($U);$Eb->apply_unsafe($V);$Eb->apply_unsafe($X);$Eb->apply_unsafe($Y);$Eb->apply_unsafe($Z);$Pb->apply_unsafe($U);$Yb->apply_unsafe($U);$vc->apply_unsafe($V);$Dc->apply_unsafe($V);$Mc->apply_unsafe($V);$Zc->apply_unsafe($V);$Zc->apply_unsafe($W);$Zc->apply_unsafe($X);$rd->apply_unsafe($V);$Od->apply_unsafe($W);$Zd->apply_unsafe($W);$ue->apply_unsafe($X);$Ce->apply_unsafe($X);$Me->apply_unsafe($X);$Cf->apply_unsafe($Z);$Kf->apply_unsafe($Z);$Vf->apply_unsafe($Z);$ni::self=$qg;&$_($D)for@$E;&$_($G)for@$H;&$_($l1)for@$m1;&$_($s1)for@$t1;&$_($w1)for@$t1;&$_($z1)for@$A1;&$_($D1)for@$t1;&$_($G1)for@$t1;&$_($J1)for@$t1;&$_($M1)for@$N1;&$_($T1)for@$t1;&$_($W1)for@$X1;&$_($Z1)for@$c2;&$_($e2)for@$t1;&$_($h2)for@$i2;&$_($k2)for@$l2;&$_($n2)for@$o2;&$_($r2)for@$t1;&$_($t2)for@$t1;&$_($w2)for@$x2;&$_($z2)for@$t1;&$_($B2)for@$t1;&$_($J2)for@$K2;&$_($N2)for@$t1;&$_($Q2)for@$R2;&$_($V2)for@$t1;&$_($Y2)for@$Z2;&$_($d3)for@$e3;&$_($j3)for@$t1;&$_($m3)for@$n3;&$_($p3)for@$q3;&$_($s3)for@$t3;&$_($x3)for@$y3;&$_($C3)for@$t1;&$_($F3)for@$G3;&$_($J3)for@$t1;&$_($M3)for@$N3;&$_($P3)for@$Q3;&$_($V3)for@$t1;&$_($X3)for@$t1;&$_($c4)for@$d4;&$_($h4)for@$t1;&$_($k4)for@$l4;&$_($p4)for@$t1;&$_($s4)for@$t4;&$_($x4)for@$t1;&$_($A4)for@$B4;&$_($D4)for@$E4;&$_($H4)for@$t1;&$_($J4)for@$t1;&$_($M4)for@$N4;&$_($Q4)for@$R4;&$_($V4)for@$t1;&$_($Y4)for@$Z4;&$_($d5)for@$e5;&$_($k5)for@$t1;&$_($n5)for@$o5;&$_($s5)for@$t1;&$_($v5)for@$t1;&$_($y5)for@$z5;&$_($B5)for@$C5;&$_($G5)for@$t1;&$_($J5)for@$K5;&$_($M5)for@$N5;&$_($P5)for@$Q5;&$_($T5)for@$U5;&$_($X5)for@$Y5;&$_($g6)for@$t1;&$_($k6)for@$t1;&$_($n6)for@$o6;&$_($t6)for@$t1;&$_($x6)for@$t1;&$_($A6)for@$B6;&$_($G6)for@$t1;&$_($K6)for@$t1;&$_($O6)for@$t1;&$_($S6)for@$t1;&$_($V6)for@$W6;&$_($c7)for@$t1;&$_($g7)for@$t1;&$_($j7)for@$k7;&$_($m7)for@$n7;&$_($s7)for@$t7;&$_($y7)for@$t1;&$_($B7)for@$C7;&$_($G7)for@$t1;&$_($J7)for@$K7;&$_($M7)for@$N7;&$_($S7)for@$T7;&$_($t8)for@$u8;&$_($z8)for@$t1;&$_($C8)for@$D8;&$_($I8)for@$t1;&$_($M8)for@$t1;&$_($Q8)for@$t1;&$_($U8)for@$t1;&$_($Y8)for@$t1;&$_($e9)for@$t1;&$_($i9)for@$t1;&$_($m9)for@$t1;&$_($q9)for@$t1;&$_($u9)for@$t1;&$_($y9)for@$t1;&$_($C9)for@$t1;&$_($G9)for@$t1;&$_($K9)for@$t1;&$_($O9)for@$t1;&$_($S9)for@$t1;&$_($W9)for@$t1;&$_($ca)for@$t1;&$_($fa)for@$ga;&$_($ia)for@$ja;&$_($Sa)for@$Ta;&$_($Va)for@$Wa;&$_($fb)for@$t1;&$_($ib)for@$jb;&$_($ob)for@$t1;&$_($rb)for@$sb;&$_($xb)for@$t1;&$_($Bb)for@$t1;&$_($Eb)for@$Fb;&$_($Hb)for@$Ib;&$_($Mb)for@$t1;&$_($Pb)for@$Qb;&$_($Vb)for@$t1;&$_($Yb)for@$Zb;&$_($dc)for@$ec;&$_($lc)for@$mc;&$_($sc)for@$t1;&$_($vc)for@$wc;&$_($Ac)for@$t1;&$_($Dc)for@$Ec;&$_($Jc)for@$t1;&$_($Mc)for@$Nc;&$_($Tc)for@$t1;&$_($Wc)for@$t1;&$_($Zc)for@$cd;&$_($ed)for@$fd;&$_($kd)for@$t1;&$_($od)for@$t1;&$_($rd)for@$sd;&$_($ud)for@$vd;&$_($Fd)for@$Gd;&$_($Ld)for@$t1;&$_($Od)for@$Pd;&$_($Td)for@$t1;&$_($Wd)for@$t1;&$_($Zd)for@$ce;&$_($ee)for@$fe;&$_($le)for@$me;&$_($re)for@$t1;&$_($ue)for@$ve;&$_($ze)for@$t1;&$_($Ce)for@$De;&$_($He)for@$t1;&$_($Ke)for@$t1;&$_($Me)for@$Ne;&$_($Pe)for@$Qe;&$_($gf)for@$hf;&$_($nf)for@$t1;&$_($rf)for@$t1;&$_($vf)for@$t1;&$_($zf)for@$t1;&$_($Cf)for@$Df;&$_($Hf)for@$t1;&$_($Kf)for@$Lf;&$_($Pf)for@$t1;&$_($Sf)for@$t1;&$_($Vf)for@$Wf;&$_($Yf)for@$Zf;&$_($kg)for@$t1;&$_($og)for@$t1;&$_($qg)for@$rg;ni->run(@ARGV);
__DATA__
