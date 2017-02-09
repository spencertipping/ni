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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fd.c';$u=q'/unix/fifo.c';$v=q'/unix/file.c';$w=q'/unix/io.c';$x=q'/unix/pid.c';$y=q'/unix/str.c';$z={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1};$A={$p,1};$B={$j,1,$k,1,$p,1,$q,1};$C=[undef];$D=q'/metaclass';$E=bless({$c,$B,$f,$j,$g,$C},$D);$F=q'/metaclass::ctors';$G=[$E];$H=bless({$c,$A,$f,$p,$g,$G},$D);$I=q'/metaclass::ctors';$J=q'/lib/slice';$K={$J,1};$L=q'/lib/behavior';$M=q'/lib/branch';$N=q'/lib/tag';$O={$L,1,$M,1,$J,1,$N,1};$P=q'/class';$Q=q'/lib/doc';$R=q'/lib/fn';$S=q'/lib/image';$T=q'/lib/ni';$U=q'/object';$V=q'/unix/cat';$W=q'/unix/fd';$X=q'/unix/fifo';$Y=q'/unix/file';$Z=q'/unix/io';$c1=q'/unix/pid';$d1=q'/unix/str';$e1={$P,1,$h,1,$L,1,$j,1,$M,1,$k,1,$Q,1,$l,1,$R,1,$m,1,$S,1,$n,1,$T,1,$o,1,$J,1,$p,1,$N,1,$q,1,$D,1,$d,1,$U,1,$r,1,$V,1,$s,1,$W,1,$t,1,$X,1,$u,1,$Y,1,$v,1,$Z,1,$w,1,$c1,1,$x,1,$d1,1,$y,1};$f1={};$g1=q'ctor';$h1=undef;$i1=q'dtor';$j1=q'methods';$k1=q'class';$l1={$m,1};$m1=[undef];$n1=bless({$c,$l1,$f,$m,$g,$m1},$D);$o1=q'/metaclass::ctors';$p1={$R,1};$q1={};$r1=q'DESTROY';$s1=q'code';$t1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$u1=bless({$s1,$t1},$R);$v1=q'/lib/fn::ctors';$w1=q'new';$x1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$y1=bless({$s1,$x1},$R);$z1={$r1,$u1,$w1,$y1};$A1=q'/lib/instantiable.b';$B1=bless({$c,$q1,$j1,$z1,$f,$A1},$J);$C1=q'/lib/slice::ctors';$D1={};$E1=q'shift->compile';$F1=bless({$s1,$E1},$R);$G1=q'compile';$H1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$I1=bless({$s1,$H1},$R);$J1=q'instantiate';$K1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$L1=bless({$s1,$K1},$R);$M1={$G1,$I1,$J1,$L1};$N1=q'/lib/fn_init.b';$O1=bless({$c,$D1,$g1,$F1,$i1,$h1,$j1,$M1,$f,$N1},$J);$P1=q'/lib/slice::ctors';$Q1={};$R1=q'serialize';$S1=q'annotations';$T1=[];$U1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$V1=bless({$S1,$T1,$s1,$U1},$R);$W1={$R1,$V1};$X1=q'/lib/fn_serialize.b';$Y1=bless({$c,$Q1,$g1,$h1,$i1,$h1,$j1,$W1,$f,$X1},$J);$Z1=q'/lib/slice::ctors';$c2=[undef,$B1,$O1,$Y1];$d2=bless({$c,$p1,$f,$R,$g,$c2},$m);$e2=q'/lib/fn.c::ctors';$f2=q'ni \'ni:\' . ref shift';$g2=bless({$s1,$f2},$R);$h2={$k1,$g2};$i2=q'/lib/instance.b';$j2=bless({$c,$f1,$g1,$h1,$i1,$h1,$j1,$h2,$f,$i2},$J);$k2=q'/lib/slice::ctors';$l2=[$j2];$m2=bless({$c,$e1,$f,$U,$g,$l2},$r);$n2=q'/object.c::ctors';$o2=[$m2];$p2=bless({$c,$O,$f,$L,$g,$o2},$j);$q2=q'/lib/behavior.c::ctors';$r2={};$s2=q'my $s = shift; ni->def($s->name, $s)';$t2=bless({$s1,$s2},$R);$u2=q'$_[0]->namespace . ":" . $_[0]->{name}';$v2=bless({$s1,$u2},$R);$w2={$f,$v2};$x2=q'/lib/named.b';$y2=bless({$c,$r2,$g1,$t2,$i1,$h1,$j1,$w2,$f,$x2},$J);$z2=q'/lib/slice::ctors';$A2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$B2=bless({$s1,$A2},$R);$C2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$D2=bless({$s1,$C2},$R);$E2=q'/lib/slice::apply';$F2=q'/lib/slice::apply_unsafe';$G2={};$H2=q'apply';$I2=q'apply_unsafe';$J2={$H2,$B2,$I2,$D2};$K2=q'/lib/slice.b';$L2=bless({$c,$G2,$j1,$J2,$f,$K2},$J);$M2=q'/lib/slice::ctors';$N2={};$O2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$P2=bless({$s1,$O2},$R);$Q2={$J1,$P2};$R2=q'/lib/slice_init.b';$S2=bless({$c,$N2,$j1,$Q2,$f,$R2},$J);$T2=q'/lib/slice::ctors';$U2={};$V2=[];$W2=q'local $_;
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
$g;';$X2=bless({$S1,$V2,$s1,$W2},$R);$Y2={$R1,$X2};$Z2=q'/lib/slice_serialize.b';$c3=bless({$c,$U2,$g1,$h1,$i1,$h1,$j1,$Y2,$f,$Z2},$J);$d3=q'/lib/slice::ctors';$e3=[$p2,$y2,$L2,$S2,$c3];$f3=bless({$c,$K,$f,$J,$g,$e3},$p);$g3=q'/lib/slice.c::ctors';$h3={};$i3=q'doc';$j3=[];$k3=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$l3=bless({$S1,$j3,$s1,$k3},$R);$m3={$i3,$l3};$n3=q'/lib/documentable.b';$o3=bless({$c,$h3,$g1,$h1,$i1,$h1,$j1,$m3,$f,$n3},$J);$p3=q'/lib/slice::ctors';$q3=[undef,$o3];$r3=bless({$c,$z,$f,$r,$g,$q3},$D);$s3=q'/metaclass::ctors';$t3=[$r3];$u3=bless({$c,$i,$f,$h,$g,$t3},$D);$v3=q'/metaclass::ctors';$w3={$P,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1};$x3={$q,1};$y3=[$E];$z3=bless({$c,$x3,$f,$q,$g,$y3},$D);$A3=q'/metaclass::ctors';$B3={$N,1};$C3={};$D3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$E3=bless({$s1,$D3},$R);$F3={$H2,$E3};$G3=q'/lib/tag.b';$H3=bless({$c,$C3,$g1,$h1,$i1,$h1,$j1,$F3,$f,$G3},$J);$I3=q'/lib/slice::ctors';$J3={};$K3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$L3=bless({$s1,$K3},$R);$M3={$J1,$L3};$N3=q'/lib/tag_init.b';$O3=bless({$c,$J3,$g1,$h1,$i1,$h1,$j1,$M3,$f,$N3},$J);$P3=q'/lib/slice::ctors';$Q3=[$p2,$y2,$H3,$O3];$R3=bless({$c,$B3,$f,$N,$g,$Q3},$q);$S3=q'/lib/tag.c::ctors';$T3=q'/lib/perlbranch.b';$U3={};$V3=q'add';$W3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$X3=bless({$s1,$W3},$R);$Y3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$Z3=bless({$s1,$Y3},$R);$c4={$V3,$X3,$H2,$Z3};$d4=q'/lib/branch.b';$e4=bless({$c,$U3,$g1,$h1,$i1,$h1,$j1,$c4,$f,$d4},$J);$f4=q'/lib/slice::ctors';$g4={};$h4=q'namespace';$i4=q'\'ni\'';$j4=bless({$s1,$i4},$R);$k4={$h4,$j4};$l4=q'/lib/named_in_ni.b';$m4=bless({$c,$g4,$g1,$h1,$i1,$h1,$j1,$k4,$f,$l4},$J);$n4=q'/lib/slice::ctors';$o4={};$p4=q'package';$q4=q'shift->{name}';$r4=bless({$s1,$q4},$R);$s4={$p4,$r4};$t4=q'/lib/namespaced.b';$u4=bless({$c,$o4,$g1,$h1,$i1,$h1,$j1,$s4,$f,$t4},$J);$v4=q'/lib/slice::ctors';$w4={};$x4=q'resolve';$y4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$z4=bless({$s1,$y4},$R);$A4={$x4,$z4};$B4=q'/lib/resolver.b';$C4=bless({$c,$w4,$g1,$h1,$i1,$h1,$j1,$A4,$f,$B4},$J);$D4=q'/lib/slice::ctors';$E4=[$e4,$B1,$y2,$m4,$u4,$C4];$F4=bless({$f,$T3,$g,$E4},$N);$G4=q'/lib/tag::ctors';$H4={};$I4=q'my $s = shift; $s->apply($s->package)';$J4=bless({$s1,$I4},$R);$K4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$L4=bless({$s1,$K4},$R);$M4={$J1,$L4};$N4=q'/lib/class_init.b';$O4=bless({$c,$H4,$g1,$J4,$i1,$h1,$j1,$M4,$f,$N4},$J);$P4=q'/lib/slice::ctors';$Q4={$k,1};$R4=[$E];$S4=bless({$c,$Q4,$f,$k,$g,$R4},$D);$T4=q'/metaclass::ctors';$U4={$M,1};$V4={};$W4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$X4=bless({$s1,$W4},$R);$Y4={$J1,$X4};$Z4=q'/lib/branch_init.b';$c5=bless({$c,$V4,$g1,$h1,$i1,$h1,$j1,$Y4,$f,$Z4},$J);$d5=q'/lib/slice::ctors';$e5=[$p2,$y2,$e4,$c5,undef];$f5=bless({$c,$U4,$f,$M,$g,$e5},$k);$g5=q'/lib/branch.c::ctors';$h5={$P,1,$h,1,$j,1,$M,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$D,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1};$i5=q'/lib/definition.b';$j5={};$k5=q'def';$l5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$m5=bless({$s1,$l5},$R);$n5={$k5,$m5};$o5=q'/lib/classdef.b';$p5=bless({$c,$j5,$g1,$h1,$i1,$h1,$j1,$n5,$f,$o5},$J);$q5=q'/lib/slice::ctors';$r5={};$s5=q'ro';$t5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$u5=bless({$s1,$t5},$R);$v5=q'rw';$w5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$x5=bless({$s1,$w5},$R);$y5={$s5,$u5,$v5,$x5};$z5=q'/lib/accessor.b';$A5=bless({$c,$r5,$g1,$h1,$i1,$h1,$j1,$y5,$f,$z5},$J);$B5=q'/lib/slice::ctors';$C5=[$p5,$A5];$D5=bless({$c,$h5,$f,$i5,$g,$C5},$M);$E5=q'/lib/branch::ctors';$F5={};$G5=q'child';$H5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$I5=bless({$s1,$H5},$R);$J5={$G5,$I5};$K5=q'/lib/subclass.b';$L5=bless({$c,$F5,$g1,$h1,$i1,$h1,$j1,$J5,$f,$K5},$J);$M5=q'/lib/slice::ctors';$N5=[$F4,$O4,$m2,$D5,$L5];$O5=bless({$c,$w3,$f,$P,$g,$N5},$h);$P5=q'/class.c::ctors';$Q5=[$O5];$R5=bless({$c,$e,$f,$d,$g,$Q5},$D);$S5=q'/metaclass::ctors';$T5={$D,1};$U5=[$F4,$O4,$m2,$D5];$V5=bless({$c,$T5,$f,$D,$g,$U5},$d);$W5=q'/metaclass.c::ctors';$X5={$o,1};$Y5=[$r3];$Z5=bless({$c,$X5,$f,$o,$g,$Y5},$D);$c6=q'/metaclass::ctors';$d6={$T,1};$e6={};$f6=q'is_mutable';$g6=[];$h6=q'$0 ne "-" && -w $0';$i6=bless({$S1,$g6,$s1,$h6},$R);$j6=q'modify';$k6=[];$l6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$m6=bless({$S1,$k6,$s1,$l6},$R);$n6={$f6,$i6,$j6,$m6};$o6=q'/lib/ni_self.b';$p6=bless({$c,$e6,$g1,$h1,$i1,$h1,$j1,$n6,$f,$o6},$J);$q6=q'/lib/slice::ctors';$r6={};$s6=q'exists';$t6=[];$u6=q'exists $_[0]->{named}{$_[1]}';$v6=bless({$S1,$t6,$s1,$u6},$R);$w6=q'quoted';$x6=[];$y6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$z6=bless({$S1,$x6,$s1,$y6},$R);$A6={$s6,$v6,$w6,$z6};$B6=q'/lib/ni_image.b';$C6=bless({$c,$r6,$g1,$h1,$i1,$h1,$j1,$A6,$f,$B6},$J);$D6=q'/lib/slice::ctors';$E6={};$F6=q'--internal/+=';$G6=[];$H6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$I6=bless({$S1,$G6,$s1,$H6},$R);$J6=q'--internal/eval';$K6=[];$L6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$M6=bless({$S1,$K6,$s1,$L6},$R);$N6=q'--internal/image';$O6=[];$P6=q'shift->quoted->write(\\*STDOUT);
0;';$Q6=bless({$S1,$O6,$s1,$P6},$R);$R6=q'run';$S6=[];$T6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$U6=bless({$S1,$S6,$s1,$T6},$R);$V6={$F6,$I6,$J6,$M6,$N6,$Q6,$R6,$U6};$W6=q'/lib/ni_main.b';$X6=bless({$c,$E6,$g1,$h1,$i1,$h1,$j1,$V6,$f,$W6},$J);$Y6=q'/lib/slice::ctors';$Z6={};$c7=[];$d7=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$e7=bless({$S1,$c7,$s1,$d7},$R);$f7=q'resolver_for';$g7=[];$h7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$i7=bless({$S1,$g7,$s1,$h7},$R);$j7={$x4,$e7,$f7,$i7};$k7=q'/lib/ni_resolver.b';$l7=bless({$c,$Z6,$g1,$h1,$i1,$h1,$j1,$j7,$f,$k7},$J);$m7=q'/lib/slice::ctors';$n7=[$m2,$p6,$C6,$X6,$l7];$o7=bless({$c,$d6,$f,$T,$g,$n7},$o);$p7=q'/lib/ni.c::ctors';$q7=q'named';$r7=q'ni.doc:/class';$s7={$l,1};$t7=[$r3];$u7=bless({$c,$s7,$f,$l,$g,$t7},$D);$v7=q'/metaclass::ctors';$w7={$Q,1};$x7={};$y7=[];$z7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$A7=bless({$S1,$y7,$s1,$z7},$R);$B7={$J1,$A7};$C7=q'/lib/doc_init.b';$D7=bless({$c,$x7,$g1,$h1,$i1,$h1,$j1,$B7,$f,$C7},$J);$E7=q'/lib/slice::ctors';$F7={};$G7=[];$H7=q'\'ni.doc\'';$I7=bless({$S1,$G7,$s1,$H7},$R);$J7={$h4,$I7};$K7=q'/lib/doc_namespace.b';$L7=bless({$c,$F7,$g1,$h1,$i1,$h1,$j1,$J7,$f,$K7},$J);$M7=q'/lib/slice::ctors';$N7=[$m2,$y2,$D7,$L7];$O7=bless({$c,$w7,$f,$Q,$g,$N7},$l);$P7=q'/lib/doc.c::ctors';$Q7=q'apropos';$R7=q'ni:/class';$S7=[$R7];$T7=q'# Classes and metaclasses
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
';$U7=bless({$Q7,$S7,$i3,$T7,$f,$P},$Q);$V7=q'/lib/doc::ctors';$W7=q'ni:/class.c';$X7=q'ni:/lib/accessor.b';$Y7=q'ni:/lib/behavior';$Z7=q'ni:/lib/behavior.c';$c8=q'ni:/lib/branch';$d8=q'ni:/lib/branch.b';$e8=q'ni:/lib/branch.c';$f8=q'ni:/lib/branch_init.b';$g8=q'ni:/lib/class_init.b';$h8=q'ni:/lib/classdef.b';$i8=q'ni:/lib/definition.b';$j8=q'ni:/lib/doc';$k8=q'ni:/lib/doc.c';$l8=q'ni:/lib/doc_init.b';$m8=q'ni:/lib/doc_namespace.b';$n8=q'ni:/lib/documentable.b';$o8=q'ni:/lib/fn';$p8=q'ni:/lib/fn.c';$q8=q'ni:/lib/fn_init.b';$r8=q'ni:/lib/fn_serialize.b';$s8=q'ni:/lib/image';$t8={$n,1};$u8=[$r3];$v8=bless({$c,$t8,$f,$n,$g,$u8},$D);$w8=q'/metaclass::ctors';$x8={$S,1};$y8={};$z8=[];$A8=q'my $class = shift;
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
  ordering     => []};';$B8=bless({$S1,$z8,$s1,$A8},$R);$C8={$J1,$B8};$D8=q'/lib/image_init.b';$E8=bless({$c,$y8,$g1,$h1,$i1,$h1,$j1,$C8,$f,$D8},$J);$F8=q'/lib/slice::ctors';$G8={};$H8=q'address';$I8=[];$J8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$K8=bless({$S1,$I8,$s1,$J8},$R);$L8=q'allocate_gensym';$M8=[];$N8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$O8=bless({$S1,$M8,$s1,$N8},$R);$P8=q'boot_side_effect';$Q8=[];$R8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$S8=bless({$S1,$Q8,$s1,$R8},$R);$T8=q'circular_links';$U8=[];$V8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$W8=bless({$S1,$U8,$s1,$V8},$R);$X8=q'finalizer';$Y8=[];$Z8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$c9=bless({$S1,$Y8,$s1,$Z8},$R);$d9=q'gensym';$e9=[];$f9=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$g9=bless({$S1,$e9,$s1,$f9},$R);$h9=q'is_circular';$i9=[];$j9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$k9=bless({$S1,$i9,$s1,$j9},$R);$l9=q'quote';$m9=[];$n9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$o9=bless({$S1,$m9,$s1,$n9},$R);$p9=q'quote_array';$q9=[];$r9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$s9=bless({$S1,$q9,$s1,$r9},$R);$t9=q'quote_blessed';$u9=[];$v9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$w9=bless({$S1,$u9,$s1,$v9},$R);$x9=q'quote_class';$y9=[];$z9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$A9=bless({$S1,$y9,$s1,$z9},$R);$B9=q'quote_hash';$C9=[];$D9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$E9=bless({$S1,$C9,$s1,$D9},$R);$F9=q'quote_object';$G9=[];$H9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$I9=bless({$S1,$G9,$s1,$H9},$R);$J9=q'quote_scalar';$K9=[];$L9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$M9=bless({$S1,$K9,$s1,$L9},$R);$N9=q'quote_value';$O9=[];$P9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$Q9=bless({$S1,$O9,$s1,$P9},$R);$R9=q'reconstruction';$S9=[];$T9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$U9=bless({$S1,$S9,$s1,$T9},$R);$V9=q'side_effect';$W9=[];$X9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Y9=bless({$S1,$W9,$s1,$X9},$R);$Z9=q'write';$ca=[];$da=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$ea=bless({$S1,$ca,$s1,$da},$R);$fa={$H8,$K8,$L8,$O8,$P8,$S8,$T8,$W8,$X8,$c9,$d9,$g9,$h9,$k9,$l9,$o9,$p9,$s9,$t9,$w9,$x9,$A9,$B9,$E9,$F9,$I9,$J9,$M9,$N9,$Q9,$R9,$U9,$V9,$Y9,$Z9,$ea};$ga=q'/lib/image_quoting.b';$ha=bless({$c,$G8,$g1,$h1,$i1,$h1,$j1,$fa,$f,$ga},$J);$ia=q'/lib/slice::ctors';$ja=[$m2,$E8,$ha];$ka=bless({$c,$x8,$f,$S,$g,$ja},$n);$la=q'/lib/image.c::ctors';$ma=q'ni:/lib/image.c';$na=q'ni:/lib/image_init.b';$oa=q'ni:/lib/image_quoting.b';$pa=q'ni:/lib/instance.b';$qa=q'ni:/lib/instantiable.b';$ra=q'ni:/lib/named.b';$sa=q'ni:/lib/named_in_ni.b';$ta=q'ni:/lib/namespaced.b';$ua=q'ni:/lib/ni';$va=q'ni:/lib/ni.c';$wa=q'ni:/lib/ni_image.b';$xa=q'ni:/lib/ni_main.b';$ya=q'ni:/lib/ni_resolver.b';$za=q'ni:/lib/ni_self.b';$Aa=q'ni:/lib/ni_static';$Ba=q'/lib/ni_static';$Ca=q'ni';$Da={$Ba,1,$Ca,1};$Ea={};$Fa=q'abbrev';$Ga=[];$Ha=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$Ia=bless({$S1,$Ga,$s1,$Ha},$R);$Ja=q'dor';$Ka=[];$La=q'defined $_[0] ? $_[0] : $_[1]';$Ma=bless({$S1,$Ka,$s1,$La},$R);$Na=q'indent';$Oa=[];$Pa=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$Qa=bless({$S1,$Oa,$s1,$Pa},$R);$Ra=q'max';$Sa=[];$Ta=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$Ua=bless({$S1,$Sa,$s1,$Ta},$R);$Va=q'maxstr';$Wa=[];$Xa=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Ya=bless({$S1,$Wa,$s1,$Xa},$R);$Za=q'mean';$cb=[];$db=q'sum(@_) / (@_ || 1)';$eb=bless({$S1,$cb,$s1,$db},$R);$fb=q'min';$gb=[];$hb=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$ib=bless({$S1,$gb,$s1,$hb},$R);$jb=q'minstr';$kb=[];$lb=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$mb=bless({$S1,$kb,$s1,$lb},$R);$nb=q'sgr';$ob=[];$pb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$qb=bless({$S1,$ob,$s1,$pb},$R);$rb=q'sr';$sb=[];$tb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$ub=bless({$S1,$sb,$s1,$tb},$R);$vb=q'sum';$wb=[];$xb=q'local $_; my $x = 0; $x += $_ for @_; $x';$yb=bless({$S1,$wb,$s1,$xb},$R);$zb=q'swap';$Ab=[];$Bb=q'@_[0, 1] = @_[1, 0]';$Cb=bless({$S1,$Ab,$s1,$Bb},$R);$Db={$Fa,$Ia,$Ja,$Ma,$Na,$Qa,$Ra,$Ua,$Va,$Ya,$Za,$eb,$fb,$ib,$jb,$mb,$nb,$qb,$rb,$ub,$vb,$yb,$zb,$Cb};$Eb=q'/lib/ni_static_util.b';$Fb=bless({$c,$Ea,$g1,$h1,$i1,$h1,$j1,$Db,$f,$Eb},$J);$Gb=q'/lib/slice::ctors';$Hb=[$Fb];$Ib=bless({$c,$Da,$f,$Ba,$g,$Hb},$P);$Jb=q'/class::ctors';$Kb=q'ni:/lib/ni_static_util.b';$Lb=q'ni:/lib/perlbranch.b';$Mb=q'ni:/lib/resolver.b';$Nb=q'ni:/lib/slice';$Ob=q'ni:/lib/slice.b';$Pb=q'ni:/lib/slice.c';$Qb=q'ni:/lib/slice_init.b';$Rb=q'ni:/lib/slice_serialize.b';$Sb=q'ni:/lib/subclass.b';$Tb=q'ni:/lib/tag';$Ub=q'ni:/lib/tag.b';$Vb=q'ni:/lib/tag.c';$Wb=q'ni:/lib/tag_init.b';$Xb=q'ni:/metaclass';$Yb=q'ni:/metaclass.c';$Zb=q'ni:/object';$cc=q'ni:/object.c';$dc=q'ni:/unix/cat';$ec={$s,1};$fc={$s,1,$t,1,$v,1,$w,1,$x,1,$y,1};$gc=[$r3];$hc=bless({$c,$fc,$f,$w,$g,$gc},$D);$ic=q'/metaclass::ctors';$jc=[$hc];$kc=bless({$c,$ec,$f,$s,$g,$jc},$D);$lc=q'/metaclass::ctors';$mc={$V,1};$nc={$V,1,$W,1,$Y,1,$Z,1,$c1,1,$d1,1};$oc={};$pc=q'into';$qc=[];$rc=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$sc=bless({$S1,$qc,$s1,$rc},$R);$tc={$pc,$sc};$uc=q'/unix/io_stream.b';$vc=bless({$c,$oc,$g1,$h1,$i1,$h1,$j1,$tc,$f,$uc},$J);$wc=q'/lib/slice::ctors';$xc={};$yc=q'(+';$zc=[];$Ac=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$Bc=bless({$S1,$zc,$s1,$Ac},$R);$Cc={$yc,$Bc};$Dc=q'/unix/io_constructors.b';$Ec=bless({$c,$xc,$g1,$h1,$i1,$h1,$j1,$Cc,$f,$Dc},$J);$Fc=q'/lib/slice::ctors';$Gc={};$Hc=q'(<>';$Ic=[];$Jc=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Kc=bless({$S1,$Ic,$s1,$Jc},$R);$Lc=q'(@{}';$Mc=[];$Nc=q'my $self = shift; [<$self>]';$Oc=bless({$S1,$Mc,$s1,$Nc},$R);$Pc={$Hc,$Kc,$Lc,$Oc};$Qc=q'/unix/io_readers.b';$Rc=bless({$c,$Gc,$g1,$h1,$i1,$h1,$j1,$Pc,$f,$Qc},$J);$Sc=q'/lib/slice::ctors';$Tc=[$m2,$vc,$Ec,$Rc];$Uc=bless({$c,$nc,$f,$Z,$g,$Tc},$w);$Vc=q'/unix/io.c::ctors';$Wc={};$Xc=[];$Yc=q'shift; +{fs => [@_]}';$Zc=bless({$S1,$Xc,$s1,$Yc},$R);$cd={$J1,$Zc};$dd=q'/unix/cat_init.b';$ed=bless({$c,$Wc,$g1,$h1,$i1,$h1,$j1,$cd,$f,$dd},$J);$fd=q'/lib/slice::ctors';$gd={};$hd=q'read';$id=[];$jd=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$kd=bless({$S1,$id,$s1,$jd},$R);$ld={$hd,$kd};$md=q'/unix/cat_read.b';$nd=bless({$c,$gd,$g1,$h1,$i1,$h1,$j1,$ld,$f,$md},$J);$od=q'/lib/slice::ctors';$pd=[$Uc,$ed,$nd];$qd=bless({$c,$mc,$f,$V,$g,$pd},$s);$rd=q'/unix/cat.c::ctors';$sd=q'ni:/unix/cat.c';$td=q'ni:/unix/cat_init.b';$ud=q'ni:/unix/cat_read.b';$vd=q'ni:/unix/fd';$wd={$t,1};$xd=[$hc];$yd=bless({$c,$wd,$f,$t,$g,$xd},$D);$zd=q'/metaclass::ctors';$Ad={$W,1};$Bd={};$Cd=q'fd';$Dd=[];$Ed=q'shift->{\'fd\'}';$Fd=bless({$S1,$Dd,$s1,$Ed},$R);$Gd={$Cd,$Fd};$Hd=q'/unix/fd_readers.b';$Id=bless({$c,$Bd,$g1,$h1,$i1,$h1,$j1,$Gd,$f,$Hd},$J);$Jd=q'/lib/slice::ctors';$Kd={};$Ld=[];$Md=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$Nd=bless({$S1,$Ld,$s1,$Md},$R);$Od={$J1,$Nd};$Pd=q'/unix/fd_init.b';$Qd=bless({$c,$Kd,$g1,$h1,$i1,$h1,$j1,$Od,$f,$Pd},$J);$Rd=q'/lib/slice::ctors';$Sd={};$Td=q'move_to';$Ud=[];$Vd=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Wd=bless({$S1,$Ud,$s1,$Vd},$R);$Xd={$Td,$Wd};$Yd=q'/unix/fd_shell.b';$Zd=bless({$c,$Sd,$g1,$h1,$i1,$h1,$j1,$Xd,$f,$Yd},$J);$ce=q'/lib/slice::ctors';$de={$W,1,$X,1,$Y,1};$ee=q'/unix/has_fd.b';$fe={};$ge=[];$he=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$ie=bless({$S1,$ge,$s1,$he},$R);$je=[];$ke=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$le=bless({$S1,$je,$s1,$ke},$R);$me={$hd,$ie,$Z9,$le};$ne=q'/unix/fd_safeio.b';$oe=bless({$c,$fe,$g1,$h1,$i1,$h1,$j1,$me,$f,$ne},$J);$pe=q'/lib/slice::ctors';$qe=[$oe];$re=bless({$c,$de,$f,$ee,$g,$qe},$M);$se=q'/lib/branch::ctors';$te={};$ue=q'read_fh';$ve=[];$we=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$xe=bless({$S1,$ve,$s1,$we},$R);$ye=q'write_fh';$ze=[];$Ae=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$Be=bless({$S1,$ze,$s1,$Ae},$R);$Ce={$ue,$xe,$ye,$Be};$De=q'/unix/fd_io.b';$Ee=bless({$c,$te,$g1,$h1,$i1,$h1,$j1,$Ce,$f,$De},$J);$Fe=q'/lib/slice::ctors';$Ge=[$Uc,$Id,$Qd,$Zd,$re,$Ee];$He=bless({$c,$Ad,$f,$W,$g,$Ge},$t);$Ie=q'/unix/fd.c::ctors';$Je=q'ni:/unix/fd.c';$Ke=q'ni:/unix/fd_init.b';$Le=q'ni:/unix/fd_io.b';$Me=q'ni:/unix/fd_readers.b';$Ne=q'ni:/unix/fd_safeio.b';$Oe=q'ni:/unix/fd_shell.b';$Pe=q'ni:/unix/fifo';$Qe={$u,1};$Re=[$r3];$Se=bless({$c,$Qe,$f,$u,$g,$Re},$D);$Te=q'/metaclass::ctors';$Ue={$X,1};$Ve={};$We=[];$Xe=q'shift->{\'read_fh\'}';$Ye=bless({$S1,$We,$s1,$Xe},$R);$Ze=[];$cf=q'shift->{\'write_fh\'}';$df=bless({$S1,$Ze,$s1,$cf},$R);$ef={$ue,$Ye,$ye,$df};$ff=q'/unix/fifo_io.b';$gf=bless({$c,$Ve,$g1,$h1,$i1,$h1,$j1,$ef,$f,$ff},$J);$hf=q'/lib/slice::ctors';$if={};$jf=[];$kf=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$lf=bless({$S1,$jf,$s1,$kf},$R);$mf={$J1,$lf};$nf=q'/unix/fifo_init.b';$of=bless({$c,$if,$g1,$h1,$i1,$h1,$j1,$mf,$f,$nf},$J);$pf=q'/lib/slice::ctors';$qf={};$rf=q'read_side';$sf=[];$tf=q'my $self = shift; close $$self{write_fh}; $self';$uf=bless({$S1,$sf,$s1,$tf},$R);$vf=q'write_side';$wf=[];$xf=q'my $self = shift; close $$self{read_fh};  $self';$yf=bless({$S1,$wf,$s1,$xf},$R);$zf={$rf,$uf,$vf,$yf};$Af=q'/unix/fifo_direction.b';$Bf=bless({$c,$qf,$g1,$h1,$i1,$h1,$j1,$zf,$f,$Af},$J);$Cf=q'/lib/slice::ctors';$Df=[$m2,$gf,$of,$re,$Bf];$Ef=bless({$c,$Ue,$f,$X,$g,$Df},$u);$Ff=q'/unix/fifo.c::ctors';$Gf=q'ni:/unix/fifo.c';$Hf=q'ni:/unix/fifo_direction.b';$If=q'ni:/unix/fifo_init.b';$Jf=q'ni:/unix/fifo_io.b';$Kf=q'ni:/unix/file';$Lf={$v,1};$Mf=[$hc];$Nf=bless({$c,$Lf,$f,$v,$g,$Mf},$D);$Of=q'/metaclass::ctors';$Pf={$Y,1};$Qf={};$Rf=[];$Sf=q'shift->{\'name\'}';$Tf=bless({$S1,$Rf,$s1,$Sf},$R);$Uf={$f,$Tf};$Vf=q'/unix/file_readers.b';$Wf=bless({$c,$Qf,$g1,$h1,$i1,$h1,$j1,$Uf,$f,$Vf},$J);$Xf=q'/lib/slice::ctors';$Yf={};$Zf=[];$cg=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$dg=bless({$S1,$Zf,$s1,$cg},$R);$eg={$J1,$dg};$fg=q'/unix/file_init.b';$gg=bless({$c,$Yf,$g1,$h1,$i1,$h1,$j1,$eg,$f,$fg},$J);$hg=q'/lib/slice::ctors';$ig={};$jg=[];$kg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$lg=bless({$S1,$jg,$s1,$kg},$R);$mg=[];$ng=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$og=bless({$S1,$mg,$s1,$ng},$R);$pg={$ue,$lg,$ye,$og};$qg=q'/unix/file_io.b';$rg=bless({$c,$ig,$g1,$h1,$i1,$h1,$j1,$pg,$f,$qg},$J);$sg=q'/lib/slice::ctors';$tg=[$Uc,$Wf,$gg,$re,$rg];$ug=bless({$c,$Pf,$f,$Y,$g,$tg},$v);$vg=q'/unix/file.c::ctors';$wg=q'ni:/unix/file.c';$xg=q'ni:/unix/file_init.b';$yg=q'ni:/unix/file_io.b';$zg=q'ni:/unix/file_readers.b';$Ag=q'ni:/unix/has_fd.b';$Bg=q'ni:/unix/io';$Cg=q'ni:/unix/io.c';$Dg=q'ni:/unix/io_constructors.b';$Eg=q'ni:/unix/io_readers.b';$Fg=q'ni:/unix/io_stream.b';$Gg=q'ni:/unix/pid';$Hg={$x,1};$Ig=[$hc];$Jg=bless({$c,$Hg,$f,$x,$g,$Ig},$D);$Kg=q'/metaclass::ctors';$Lg={$c1,1};$Mg={};$Ng=q'pid';$Og=[];$Pg=q'shift->{\'pid\'}';$Qg=bless({$S1,$Og,$s1,$Pg},$R);$Rg=q'stderr';$Sg=[];$Tg=q'shift->{\'stderr\'}';$Ug=bless({$S1,$Sg,$s1,$Tg},$R);$Vg=q'stdin';$Wg=[];$Xg=q'shift->{\'stdin\'}';$Yg=bless({$S1,$Wg,$s1,$Xg},$R);$Zg=q'stdout';$ch=[];$dh=q'shift->{\'stdout\'}';$eh=bless({$S1,$ch,$s1,$dh},$R);$fh={$Ng,$Qg,$Rg,$Ug,$Vg,$Yg,$Zg,$eh};$gh=q'/unix/pid_readers.b';$hh=bless({$c,$Mg,$g1,$h1,$i1,$h1,$j1,$fh,$f,$gh},$J);$ih=q'/lib/slice::ctors';$jh={};$kh=[];$lh=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$mh=bless({$S1,$kh,$s1,$lh},$R);$nh={$J1,$mh};$oh=q'/unix/pid_init.b';$ph=bless({$c,$jh,$g1,$h1,$i1,$h1,$j1,$nh,$f,$oh},$J);$qh=q'/lib/slice::ctors';$rh={};$sh=[];$th=q'shift->{stdout}';$uh=bless({$S1,$sh,$s1,$th},$R);$vh=[];$wh=q'shift->{stdin}';$xh=bless({$S1,$vh,$s1,$wh},$R);$yh={$ue,$uh,$ye,$xh};$zh=q'/unix/pid_io.b';$Ah=bless({$c,$rh,$g1,$h1,$i1,$h1,$j1,$yh,$f,$zh},$J);$Bh=q'/lib/slice::ctors';$Ch=[$Uc,$hh,$ph,$Ah];$Dh=bless({$c,$Lg,$f,$c1,$g,$Ch},$x);$Eh=q'/unix/pid.c::ctors';$Fh=q'ni:/unix/pid.c';$Gh=q'ni:/unix/pid_init.b';$Hh=q'ni:/unix/pid_io.b';$Ih=q'ni:/unix/pid_readers.b';$Jh=q'ni:/unix/str';$Kh={$y,1};$Lh=[$hc];$Mh=bless({$c,$Kh,$f,$y,$g,$Lh},$D);$Nh=q'/metaclass::ctors';$Oh={$d1,1};$Ph={};$Qh=[];$Rh=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Sh=bless({$S1,$Qh,$s1,$Rh},$R);$Th={$J1,$Sh};$Uh=q'/unix/str_init.b';$Vh=bless({$c,$Ph,$g1,$h1,$i1,$h1,$j1,$Th,$f,$Uh},$J);$Wh=q'/lib/slice::ctors';$Xh={};$Yh=[];$Zh=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$ci=bless({$S1,$Yh,$s1,$Zh},$R);$di=q'remaining';$ei=[];$fi=q'my $self = shift; $$self{end} - $$self{start}';$gi=bless({$S1,$ei,$s1,$fi},$R);$hi=[];$ii=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$ji=bless({$S1,$hi,$s1,$ii},$R);$ki={$hd,$ci,$di,$gi,$Z9,$ji};$li=q'/unix/str_io.b';$mi=bless({$c,$Xh,$g1,$h1,$i1,$h1,$j1,$ki,$f,$li},$J);$ni=q'/lib/slice::ctors';$oi=[$Uc,$Vh,$mi];$pi=bless({$c,$Oh,$f,$d1,$g,$oi},$y);$qi=q'/unix/str.c::ctors';$ri=q'ni:/unix/str.c';$si=q'ni:/unix/str_init.b';$ti=q'ni:/unix/str_io.b';$ui={$r7,$U7,$R7,$O5,$W7,$u3,$X7,$A5,$Y7,$p2,$Z7,$E,$c8,$f5,$d8,$e4,$e8,$S4,$f8,$c5,$g8,$O4,$h8,$p5,$i8,$D5,$j8,$O7,$k8,$u7,$l8,$D7,$m8,$L7,$n8,$o3,$o8,$d2,$p8,$n1,$q8,$O1,$r8,$Y1,$s8,$ka,$ma,$v8,$na,$E8,$oa,$ha,$pa,$j2,$qa,$B1,$ra,$y2,$sa,$m4,$ta,$u4,$ua,$o7,$va,$Z5,$wa,$C6,$xa,$X6,$ya,$l7,$za,$p6,$Aa,$Ib,$Kb,$Fb,$Lb,$F4,$Mb,$C4,$Nb,$f3,$Ob,$L2,$Pb,$H,$Qb,$S2,$Rb,$c3,$Sb,$L5,$Tb,$R3,$Ub,$H3,$Vb,$z3,$Wb,$O3,$Xb,$V5,$Yb,$R5,$Zb,$m2,$cc,$r3,$dc,$qd,$sd,$kc,$td,$ed,$ud,$nd,$vd,$He,$Je,$yd,$Ke,$Qd,$Le,$Ee,$Me,$Id,$Ne,$oe,$Oe,$Zd,$Pe,$Ef,$Gf,$Se,$Hf,$Bf,$If,$of,$Jf,$gf,$Kf,$ug,$wg,$Nf,$xg,$gg,$yg,$rg,$zg,$Wf,$Ag,$re,$Bg,$Uc,$Cg,$hc,$Dg,$Ec,$Eg,$Rc,$Fg,$vc,$Gg,$Dh,$Fh,$Jg,$Gh,$ph,$Hh,$Ah,$Ih,$hh,$Jh,$pi,$ri,$Mh,$si,$Vh,$ti,$mi};$vi=q'resolvers';$wi=[];$xi=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$yi=bless({$S1,$wi,$s1,$xi},$R);$zi=q'file';$Ai=[];$Bi=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$Ci=bless({$S1,$Ai,$s1,$Bi},$R);$Di=q'str';$Ei=[];$Fi=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$Gi=bless({$S1,$Ei,$s1,$Fi},$R);$Hi={$Cd,$yi,$zi,$Ci,$Di,$Gi};$Ii=bless({$q7,$ui,$vi,$Hi},$T);$Ji=q'/lib/ni::ctors';$$q3[0]=$O5;$$C[0]=$r3;$$m1[0]=$r3;$$c2[0]=$m2;$$e5[4]=$D5;*$F2=\&$D2;*$E2=\&$B2;$B1->apply_unsafe($P);$B1->apply_unsafe($h);$B1->apply_unsafe($j);$B1->apply_unsafe($M);$B1->apply_unsafe($k);$B1->apply_unsafe($l);$B1->apply_unsafe($R);$B1->apply_unsafe($m);$B1->apply_unsafe($n);$B1->apply_unsafe($o);$B1->apply_unsafe($J);$B1->apply_unsafe($p);$B1->apply_unsafe($N);$B1->apply_unsafe($q);$B1->apply_unsafe($D);$B1->apply_unsafe($d);$B1->apply_unsafe($r);$B1->apply_unsafe($s);$B1->apply_unsafe($t);$B1->apply_unsafe($u);$B1->apply_unsafe($v);$B1->apply_unsafe($w);$B1->apply_unsafe($x);$B1->apply_unsafe($y);$O1->apply_unsafe($R);$Y1->apply_unsafe($R);$j2->apply_unsafe($P);$j2->apply_unsafe($h);$j2->apply_unsafe($L);$j2->apply_unsafe($j);$j2->apply_unsafe($M);$j2->apply_unsafe($k);$j2->apply_unsafe($Q);$j2->apply_unsafe($l);$j2->apply_unsafe($R);$j2->apply_unsafe($m);$j2->apply_unsafe($S);$j2->apply_unsafe($n);$j2->apply_unsafe($T);$j2->apply_unsafe($o);$j2->apply_unsafe($J);$j2->apply_unsafe($p);$j2->apply_unsafe($N);$j2->apply_unsafe($q);$j2->apply_unsafe($D);$j2->apply_unsafe($d);$j2->apply_unsafe($U);$j2->apply_unsafe($r);$j2->apply_unsafe($V);$j2->apply_unsafe($s);$j2->apply_unsafe($W);$j2->apply_unsafe($t);$j2->apply_unsafe($X);$j2->apply_unsafe($u);$j2->apply_unsafe($Y);$j2->apply_unsafe($v);$j2->apply_unsafe($Z);$j2->apply_unsafe($w);$j2->apply_unsafe($c1);$j2->apply_unsafe($x);$j2->apply_unsafe($d1);$j2->apply_unsafe($y);$y2->apply_unsafe($P);$y2->apply_unsafe($h);$y2->apply_unsafe($j);$y2->apply_unsafe($M);$y2->apply_unsafe($k);$y2->apply_unsafe($Q);$y2->apply_unsafe($l);$y2->apply_unsafe($m);$y2->apply_unsafe($n);$y2->apply_unsafe($o);$y2->apply_unsafe($J);$y2->apply_unsafe($p);$y2->apply_unsafe($N);$y2->apply_unsafe($q);$y2->apply_unsafe($D);$y2->apply_unsafe($d);$y2->apply_unsafe($r);$y2->apply_unsafe($s);$y2->apply_unsafe($t);$y2->apply_unsafe($u);$y2->apply_unsafe($v);$y2->apply_unsafe($w);$y2->apply_unsafe($x);$y2->apply_unsafe($y);$L2->apply_unsafe($J);$S2->apply_unsafe($J);$c3->apply_unsafe($J);$o3->apply_unsafe($h);$o3->apply_unsafe($j);$o3->apply_unsafe($k);$o3->apply_unsafe($l);$o3->apply_unsafe($m);$o3->apply_unsafe($n);$o3->apply_unsafe($o);$o3->apply_unsafe($p);$o3->apply_unsafe($q);$o3->apply_unsafe($r);$o3->apply_unsafe($s);$o3->apply_unsafe($t);$o3->apply_unsafe($u);$o3->apply_unsafe($v);$o3->apply_unsafe($w);$o3->apply_unsafe($x);$o3->apply_unsafe($y);$H3->apply_unsafe($N);$O3->apply_unsafe($N);$e4->apply_unsafe($P);$e4->apply_unsafe($h);$e4->apply_unsafe($j);$e4->apply_unsafe($M);$e4->apply_unsafe($k);$e4->apply_unsafe($l);$e4->apply_unsafe($m);$e4->apply_unsafe($n);$e4->apply_unsafe($o);$e4->apply_unsafe($p);$e4->apply_unsafe($q);$e4->apply_unsafe($D);$e4->apply_unsafe($d);$e4->apply_unsafe($r);$e4->apply_unsafe($s);$e4->apply_unsafe($t);$e4->apply_unsafe($u);$e4->apply_unsafe($v);$e4->apply_unsafe($w);$e4->apply_unsafe($x);$e4->apply_unsafe($y);$m4->apply_unsafe($P);$m4->apply_unsafe($h);$m4->apply_unsafe($j);$m4->apply_unsafe($M);$m4->apply_unsafe($k);$m4->apply_unsafe($l);$m4->apply_unsafe($m);$m4->apply_unsafe($n);$m4->apply_unsafe($o);$m4->apply_unsafe($J);$m4->apply_unsafe($p);$m4->apply_unsafe($N);$m4->apply_unsafe($q);$m4->apply_unsafe($D);$m4->apply_unsafe($d);$m4->apply_unsafe($r);$m4->apply_unsafe($s);$m4->apply_unsafe($t);$m4->apply_unsafe($u);$m4->apply_unsafe($v);$m4->apply_unsafe($w);$m4->apply_unsafe($x);$m4->apply_unsafe($y);$u4->apply_unsafe($P);$u4->apply_unsafe($h);$u4->apply_unsafe($j);$u4->apply_unsafe($M);$u4->apply_unsafe($k);$u4->apply_unsafe($l);$u4->apply_unsafe($m);$u4->apply_unsafe($n);$u4->apply_unsafe($o);$u4->apply_unsafe($J);$u4->apply_unsafe($p);$u4->apply_unsafe($N);$u4->apply_unsafe($q);$u4->apply_unsafe($D);$u4->apply_unsafe($d);$u4->apply_unsafe($r);$u4->apply_unsafe($s);$u4->apply_unsafe($t);$u4->apply_unsafe($u);$u4->apply_unsafe($v);$u4->apply_unsafe($w);$u4->apply_unsafe($x);$u4->apply_unsafe($y);$C4->apply_unsafe($P);$C4->apply_unsafe($h);$C4->apply_unsafe($j);$C4->apply_unsafe($M);$C4->apply_unsafe($k);$C4->apply_unsafe($l);$C4->apply_unsafe($m);$C4->apply_unsafe($n);$C4->apply_unsafe($o);$C4->apply_unsafe($p);$C4->apply_unsafe($N);$C4->apply_unsafe($q);$C4->apply_unsafe($D);$C4->apply_unsafe($d);$C4->apply_unsafe($r);$C4->apply_unsafe($s);$C4->apply_unsafe($t);$C4->apply_unsafe($u);$C4->apply_unsafe($v);$C4->apply_unsafe($w);$C4->apply_unsafe($x);$C4->apply_unsafe($y);$O4->apply_unsafe($P);$O4->apply_unsafe($h);$O4->apply_unsafe($j);$O4->apply_unsafe($k);$O4->apply_unsafe($l);$O4->apply_unsafe($m);$O4->apply_unsafe($n);$O4->apply_unsafe($o);$O4->apply_unsafe($p);$O4->apply_unsafe($q);$O4->apply_unsafe($D);$O4->apply_unsafe($d);$O4->apply_unsafe($r);$O4->apply_unsafe($s);$O4->apply_unsafe($t);$O4->apply_unsafe($u);$O4->apply_unsafe($v);$O4->apply_unsafe($w);$O4->apply_unsafe($x);$O4->apply_unsafe($y);$c5->apply_unsafe($M);$p5->apply_unsafe($P);$p5->apply_unsafe($h);$p5->apply_unsafe($j);$p5->apply_unsafe($M);$p5->apply_unsafe($k);$p5->apply_unsafe($l);$p5->apply_unsafe($m);$p5->apply_unsafe($n);$p5->apply_unsafe($o);$p5->apply_unsafe($p);$p5->apply_unsafe($q);$p5->apply_unsafe($D);$p5->apply_unsafe($d);$p5->apply_unsafe($r);$p5->apply_unsafe($s);$p5->apply_unsafe($t);$p5->apply_unsafe($u);$p5->apply_unsafe($v);$p5->apply_unsafe($w);$p5->apply_unsafe($x);$p5->apply_unsafe($y);$A5->apply_unsafe($P);$A5->apply_unsafe($h);$A5->apply_unsafe($j);$A5->apply_unsafe($M);$A5->apply_unsafe($k);$A5->apply_unsafe($l);$A5->apply_unsafe($m);$A5->apply_unsafe($n);$A5->apply_unsafe($o);$A5->apply_unsafe($p);$A5->apply_unsafe($q);$A5->apply_unsafe($D);$A5->apply_unsafe($d);$A5->apply_unsafe($r);$A5->apply_unsafe($s);$A5->apply_unsafe($t);$A5->apply_unsafe($u);$A5->apply_unsafe($v);$A5->apply_unsafe($w);$A5->apply_unsafe($x);$A5->apply_unsafe($y);$L5->apply_unsafe($P);$L5->apply_unsafe($h);$L5->apply_unsafe($j);$L5->apply_unsafe($k);$L5->apply_unsafe($l);$L5->apply_unsafe($m);$L5->apply_unsafe($n);$L5->apply_unsafe($o);$L5->apply_unsafe($p);$L5->apply_unsafe($q);$L5->apply_unsafe($d);$L5->apply_unsafe($r);$L5->apply_unsafe($s);$L5->apply_unsafe($t);$L5->apply_unsafe($u);$L5->apply_unsafe($v);$L5->apply_unsafe($w);$L5->apply_unsafe($x);$L5->apply_unsafe($y);$p6->apply_unsafe($T);$C6->apply_unsafe($T);$X6->apply_unsafe($T);$l7->apply_unsafe($T);$D7->apply_unsafe($Q);$L7->apply_unsafe($Q);$E8->apply_unsafe($S);$ha->apply_unsafe($S);$Fb->apply_unsafe($Ba);$Fb->apply_unsafe($Ca);$vc->apply_unsafe($V);$vc->apply_unsafe($W);$vc->apply_unsafe($Y);$vc->apply_unsafe($Z);$vc->apply_unsafe($c1);$vc->apply_unsafe($d1);$Ec->apply_unsafe($V);$Ec->apply_unsafe($W);$Ec->apply_unsafe($Y);$Ec->apply_unsafe($Z);$Ec->apply_unsafe($c1);$Ec->apply_unsafe($d1);$Rc->apply_unsafe($V);$Rc->apply_unsafe($W);$Rc->apply_unsafe($Y);$Rc->apply_unsafe($Z);$Rc->apply_unsafe($c1);$Rc->apply_unsafe($d1);$ed->apply_unsafe($V);$nd->apply_unsafe($V);$Id->apply_unsafe($W);$Qd->apply_unsafe($W);$Zd->apply_unsafe($W);$oe->apply_unsafe($W);$oe->apply_unsafe($X);$oe->apply_unsafe($Y);$Ee->apply_unsafe($W);$gf->apply_unsafe($X);$of->apply_unsafe($X);$Bf->apply_unsafe($X);$Wf->apply_unsafe($Y);$gg->apply_unsafe($Y);$rg->apply_unsafe($Y);$hh->apply_unsafe($c1);$ph->apply_unsafe($c1);$Ah->apply_unsafe($c1);$Vh->apply_unsafe($d1);$mi->apply_unsafe($d1);$ni::self=$Ii;&$_($E)for@$F;&$_($H)for@$I;&$_($n1)for@$o1;&$_($u1)for@$v1;&$_($y1)for@$v1;&$_($B1)for@$C1;&$_($F1)for@$v1;&$_($I1)for@$v1;&$_($L1)for@$v1;&$_($O1)for@$P1;&$_($V1)for@$v1;&$_($Y1)for@$Z1;&$_($d2)for@$e2;&$_($g2)for@$v1;&$_($j2)for@$k2;&$_($m2)for@$n2;&$_($p2)for@$q2;&$_($t2)for@$v1;&$_($v2)for@$v1;&$_($y2)for@$z2;&$_($B2)for@$v1;&$_($D2)for@$v1;&$_($L2)for@$M2;&$_($P2)for@$v1;&$_($S2)for@$T2;&$_($X2)for@$v1;&$_($c3)for@$d3;&$_($f3)for@$g3;&$_($l3)for@$v1;&$_($o3)for@$p3;&$_($r3)for@$s3;&$_($u3)for@$v3;&$_($z3)for@$A3;&$_($E3)for@$v1;&$_($H3)for@$I3;&$_($L3)for@$v1;&$_($O3)for@$P3;&$_($R3)for@$S3;&$_($X3)for@$v1;&$_($Z3)for@$v1;&$_($e4)for@$f4;&$_($j4)for@$v1;&$_($m4)for@$n4;&$_($r4)for@$v1;&$_($u4)for@$v4;&$_($z4)for@$v1;&$_($C4)for@$D4;&$_($F4)for@$G4;&$_($J4)for@$v1;&$_($L4)for@$v1;&$_($O4)for@$P4;&$_($S4)for@$T4;&$_($X4)for@$v1;&$_($c5)for@$d5;&$_($f5)for@$g5;&$_($m5)for@$v1;&$_($p5)for@$q5;&$_($u5)for@$v1;&$_($x5)for@$v1;&$_($A5)for@$B5;&$_($D5)for@$E5;&$_($I5)for@$v1;&$_($L5)for@$M5;&$_($O5)for@$P5;&$_($R5)for@$S5;&$_($V5)for@$W5;&$_($Z5)for@$c6;&$_($i6)for@$v1;&$_($m6)for@$v1;&$_($p6)for@$q6;&$_($v6)for@$v1;&$_($z6)for@$v1;&$_($C6)for@$D6;&$_($I6)for@$v1;&$_($M6)for@$v1;&$_($Q6)for@$v1;&$_($U6)for@$v1;&$_($X6)for@$Y6;&$_($e7)for@$v1;&$_($i7)for@$v1;&$_($l7)for@$m7;&$_($o7)for@$p7;&$_($u7)for@$v7;&$_($A7)for@$v1;&$_($D7)for@$E7;&$_($I7)for@$v1;&$_($L7)for@$M7;&$_($O7)for@$P7;&$_($U7)for@$V7;&$_($v8)for@$w8;&$_($B8)for@$v1;&$_($E8)for@$F8;&$_($K8)for@$v1;&$_($O8)for@$v1;&$_($S8)for@$v1;&$_($W8)for@$v1;&$_($c9)for@$v1;&$_($g9)for@$v1;&$_($k9)for@$v1;&$_($o9)for@$v1;&$_($s9)for@$v1;&$_($w9)for@$v1;&$_($A9)for@$v1;&$_($E9)for@$v1;&$_($I9)for@$v1;&$_($M9)for@$v1;&$_($Q9)for@$v1;&$_($U9)for@$v1;&$_($Y9)for@$v1;&$_($ea)for@$v1;&$_($ha)for@$ia;&$_($ka)for@$la;&$_($Ia)for@$v1;&$_($Ma)for@$v1;&$_($Qa)for@$v1;&$_($Ua)for@$v1;&$_($Ya)for@$v1;&$_($eb)for@$v1;&$_($ib)for@$v1;&$_($mb)for@$v1;&$_($qb)for@$v1;&$_($ub)for@$v1;&$_($yb)for@$v1;&$_($Cb)for@$v1;&$_($Fb)for@$Gb;&$_($Ib)for@$Jb;&$_($hc)for@$ic;&$_($kc)for@$lc;&$_($sc)for@$v1;&$_($vc)for@$wc;&$_($Bc)for@$v1;&$_($Ec)for@$Fc;&$_($Kc)for@$v1;&$_($Oc)for@$v1;&$_($Rc)for@$Sc;&$_($Uc)for@$Vc;&$_($Zc)for@$v1;&$_($ed)for@$fd;&$_($kd)for@$v1;&$_($nd)for@$od;&$_($qd)for@$rd;&$_($yd)for@$zd;&$_($Fd)for@$v1;&$_($Id)for@$Jd;&$_($Nd)for@$v1;&$_($Qd)for@$Rd;&$_($Wd)for@$v1;&$_($Zd)for@$ce;&$_($ie)for@$v1;&$_($le)for@$v1;&$_($oe)for@$pe;&$_($re)for@$se;&$_($xe)for@$v1;&$_($Be)for@$v1;&$_($Ee)for@$Fe;&$_($He)for@$Ie;&$_($Se)for@$Te;&$_($Ye)for@$v1;&$_($df)for@$v1;&$_($gf)for@$hf;&$_($lf)for@$v1;&$_($of)for@$pf;&$_($uf)for@$v1;&$_($yf)for@$v1;&$_($Bf)for@$Cf;&$_($Ef)for@$Ff;&$_($Nf)for@$Of;&$_($Tf)for@$v1;&$_($Wf)for@$Xf;&$_($dg)for@$v1;&$_($gg)for@$hg;&$_($lg)for@$v1;&$_($og)for@$v1;&$_($rg)for@$sg;&$_($ug)for@$vg;&$_($Jg)for@$Kg;&$_($Qg)for@$v1;&$_($Ug)for@$v1;&$_($Yg)for@$v1;&$_($eh)for@$v1;&$_($hh)for@$ih;&$_($mh)for@$v1;&$_($ph)for@$qh;&$_($uh)for@$v1;&$_($xh)for@$v1;&$_($Ah)for@$Bh;&$_($Dh)for@$Eh;&$_($Mh)for@$Nh;&$_($Sh)for@$v1;&$_($Vh)for@$Wh;&$_($ci)for@$v1;&$_($gi)for@$v1;&$_($ji)for@$v1;&$_($mi)for@$ni;&$_($pi)for@$qi;&$_($yi)for@$v1;&$_($Ci)for@$v1;&$_($Gi)for@$v1;&$_($Ii)for@$Ji;ni->run(@ARGV);
__DATA__
