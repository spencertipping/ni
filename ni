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
$self;';$i7=bless({$S1,$g7,$s1,$h7},$R);$j7={$x4,$e7,$f7,$i7};$k7=q'/lib/ni_resolver.b';$l7=bless({$c,$Z6,$g1,$h1,$i1,$h1,$j1,$j7,$f,$k7},$J);$m7=q'/lib/slice::ctors';$n7={};$o7=q'fork';$p7=[];$q7=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$r7=bless({$S1,$p7,$s1,$q7},$R);$s7=q'fork_exec';$t7=[];$u7=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$v7=bless({$S1,$t7,$s1,$u7},$R);$w7={$o7,$r7,$s7,$v7};$x7=q'/lib/ni_pid_ctors';$y7=bless({$c,$n7,$g1,$h1,$i1,$h1,$j1,$w7,$f,$x7},$J);$z7=q'/lib/slice::ctors';$A7=[$m2,$p6,$C6,$X6,$l7,$y7];$B7=bless({$c,$d6,$f,$T,$g,$A7},$o);$C7=q'/lib/ni.c::ctors';$D7=q'named';$E7=q'ni.doc:/class';$F7={$l,1};$G7=[$r3];$H7=bless({$c,$F7,$f,$l,$g,$G7},$D);$I7=q'/metaclass::ctors';$J7={$Q,1};$K7={};$L7=[];$M7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$N7=bless({$S1,$L7,$s1,$M7},$R);$O7={$J1,$N7};$P7=q'/lib/doc_init.b';$Q7=bless({$c,$K7,$g1,$h1,$i1,$h1,$j1,$O7,$f,$P7},$J);$R7=q'/lib/slice::ctors';$S7={};$T7=[];$U7=q'\'ni.doc\'';$V7=bless({$S1,$T7,$s1,$U7},$R);$W7={$h4,$V7};$X7=q'/lib/doc_namespace.b';$Y7=bless({$c,$S7,$g1,$h1,$i1,$h1,$j1,$W7,$f,$X7},$J);$Z7=q'/lib/slice::ctors';$c8=[$m2,$y2,$Q7,$Y7];$d8=bless({$c,$J7,$f,$Q,$g,$c8},$l);$e8=q'/lib/doc.c::ctors';$f8=q'apropos';$g8=q'ni:/class';$h8=[$g8];$i8=q'# Classes and metaclasses
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
';$j8=bless({$f8,$h8,$i3,$i8,$f,$P},$Q);$k8=q'/lib/doc::ctors';$l8=q'ni:/class.c';$m8=q'ni:/lib/accessor.b';$n8=q'ni:/lib/behavior';$o8=q'ni:/lib/behavior.c';$p8=q'ni:/lib/branch';$q8=q'ni:/lib/branch.b';$r8=q'ni:/lib/branch.c';$s8=q'ni:/lib/branch_init.b';$t8=q'ni:/lib/class_init.b';$u8=q'ni:/lib/classdef.b';$v8=q'ni:/lib/definition.b';$w8=q'ni:/lib/doc';$x8=q'ni:/lib/doc.c';$y8=q'ni:/lib/doc_init.b';$z8=q'ni:/lib/doc_namespace.b';$A8=q'ni:/lib/documentable.b';$B8=q'ni:/lib/fn';$C8=q'ni:/lib/fn.c';$D8=q'ni:/lib/fn_init.b';$E8=q'ni:/lib/fn_serialize.b';$F8=q'ni:/lib/image';$G8={$n,1};$H8=[$r3];$I8=bless({$c,$G8,$f,$n,$g,$H8},$D);$J8=q'/metaclass::ctors';$K8={$S,1};$L8={};$M8=[];$N8=q'my $class = shift;
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
  ordering     => []};';$O8=bless({$S1,$M8,$s1,$N8},$R);$P8={$J1,$O8};$Q8=q'/lib/image_init.b';$R8=bless({$c,$L8,$g1,$h1,$i1,$h1,$j1,$P8,$f,$Q8},$J);$S8=q'/lib/slice::ctors';$T8={};$U8=q'address';$V8=[];$W8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$X8=bless({$S1,$V8,$s1,$W8},$R);$Y8=q'allocate_gensym';$Z8=[];$c9=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$d9=bless({$S1,$Z8,$s1,$c9},$R);$e9=q'boot_side_effect';$f9=[];$g9=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$h9=bless({$S1,$f9,$s1,$g9},$R);$i9=q'circular_links';$j9=[];$k9=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$l9=bless({$S1,$j9,$s1,$k9},$R);$m9=q'finalizer';$n9=[];$o9=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$p9=bless({$S1,$n9,$s1,$o9},$R);$q9=q'gensym';$r9=[];$s9=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$t9=bless({$S1,$r9,$s1,$s9},$R);$u9=q'is_circular';$v9=[];$w9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$x9=bless({$S1,$v9,$s1,$w9},$R);$y9=q'quote';$z9=[];$A9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$B9=bless({$S1,$z9,$s1,$A9},$R);$C9=q'quote_array';$D9=[];$E9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$F9=bless({$S1,$D9,$s1,$E9},$R);$G9=q'quote_blessed';$H9=[];$I9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$J9=bless({$S1,$H9,$s1,$I9},$R);$K9=q'quote_class';$L9=[];$M9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$N9=bless({$S1,$L9,$s1,$M9},$R);$O9=q'quote_hash';$P9=[];$Q9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$R9=bless({$S1,$P9,$s1,$Q9},$R);$S9=q'quote_object';$T9=[];$U9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$V9=bless({$S1,$T9,$s1,$U9},$R);$W9=q'quote_scalar';$X9=[];$Y9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Z9=bless({$S1,$X9,$s1,$Y9},$R);$ca=q'quote_value';$da=[];$ea=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$fa=bless({$S1,$da,$s1,$ea},$R);$ga=q'reconstruction';$ha=[];$ia=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$ja=bless({$S1,$ha,$s1,$ia},$R);$ka=q'side_effect';$la=[];$ma=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$na=bless({$S1,$la,$s1,$ma},$R);$oa=q'write';$pa=[];$qa=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$ra=bless({$S1,$pa,$s1,$qa},$R);$sa={$U8,$X8,$Y8,$d9,$e9,$h9,$i9,$l9,$m9,$p9,$q9,$t9,$u9,$x9,$y9,$B9,$C9,$F9,$G9,$J9,$K9,$N9,$O9,$R9,$S9,$V9,$W9,$Z9,$ca,$fa,$ga,$ja,$ka,$na,$oa,$ra};$ta=q'/lib/image_quoting.b';$ua=bless({$c,$T8,$g1,$h1,$i1,$h1,$j1,$sa,$f,$ta},$J);$va=q'/lib/slice::ctors';$wa=[$m2,$R8,$ua];$xa=bless({$c,$K8,$f,$S,$g,$wa},$n);$ya=q'/lib/image.c::ctors';$za=q'ni:/lib/image.c';$Aa=q'ni:/lib/image_init.b';$Ba=q'ni:/lib/image_quoting.b';$Ca=q'ni:/lib/instance.b';$Da=q'ni:/lib/instantiable.b';$Ea=q'ni:/lib/named.b';$Fa=q'ni:/lib/named_in_ni.b';$Ga=q'ni:/lib/namespaced.b';$Ha=q'ni:/lib/ni';$Ia=q'ni:/lib/ni.c';$Ja=q'ni:/lib/ni_image.b';$Ka=q'ni:/lib/ni_main.b';$La=q'ni:/lib/ni_pid_ctors';$Ma=q'ni:/lib/ni_resolver.b';$Na=q'ni:/lib/ni_self.b';$Oa=q'ni:/lib/ni_static';$Pa=q'/lib/ni_static';$Qa=q'ni';$Ra={$Pa,1,$Qa,1};$Sa={};$Ta=q'abbrev';$Ua=[];$Va=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$Wa=bless({$S1,$Ua,$s1,$Va},$R);$Xa=q'dor';$Ya=[];$Za=q'defined $_[0] ? $_[0] : $_[1]';$cb=bless({$S1,$Ya,$s1,$Za},$R);$db=q'indent';$eb=[];$fb=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$gb=bless({$S1,$eb,$s1,$fb},$R);$hb=q'max';$ib=[];$jb=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$kb=bless({$S1,$ib,$s1,$jb},$R);$lb=q'maxstr';$mb=[];$nb=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$ob=bless({$S1,$mb,$s1,$nb},$R);$pb=q'mean';$qb=[];$rb=q'sum(@_) / (@_ || 1)';$sb=bless({$S1,$qb,$s1,$rb},$R);$tb=q'min';$ub=[];$vb=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$wb=bless({$S1,$ub,$s1,$vb},$R);$xb=q'minstr';$yb=[];$zb=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Ab=bless({$S1,$yb,$s1,$zb},$R);$Bb=q'sgr';$Cb=[];$Db=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Eb=bless({$S1,$Cb,$s1,$Db},$R);$Fb=q'sr';$Gb=[];$Hb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Ib=bless({$S1,$Gb,$s1,$Hb},$R);$Jb=q'sum';$Kb=[];$Lb=q'local $_; my $x = 0; $x += $_ for @_; $x';$Mb=bless({$S1,$Kb,$s1,$Lb},$R);$Nb=q'swap';$Ob=[];$Pb=q'@_[0, 1] = @_[1, 0]';$Qb=bless({$S1,$Ob,$s1,$Pb},$R);$Rb={$Ta,$Wa,$Xa,$cb,$db,$gb,$hb,$kb,$lb,$ob,$pb,$sb,$tb,$wb,$xb,$Ab,$Bb,$Eb,$Fb,$Ib,$Jb,$Mb,$Nb,$Qb};$Sb=q'/lib/ni_static_util.b';$Tb=bless({$c,$Sa,$g1,$h1,$i1,$h1,$j1,$Rb,$f,$Sb},$J);$Ub=q'/lib/slice::ctors';$Vb=[$Tb];$Wb=bless({$c,$Ra,$f,$Pa,$g,$Vb},$P);$Xb=q'/class::ctors';$Yb=q'ni:/lib/ni_static_util.b';$Zb=q'ni:/lib/perlbranch.b';$cc=q'ni:/lib/resolver.b';$dc=q'ni:/lib/slice';$ec=q'ni:/lib/slice.b';$fc=q'ni:/lib/slice.c';$gc=q'ni:/lib/slice_init.b';$hc=q'ni:/lib/slice_serialize.b';$ic=q'ni:/lib/subclass.b';$jc=q'ni:/lib/tag';$kc=q'ni:/lib/tag.b';$lc=q'ni:/lib/tag.c';$mc=q'ni:/lib/tag_init.b';$nc=q'ni:/metaclass';$oc=q'ni:/metaclass.c';$pc=q'ni:/object';$qc=q'ni:/object.c';$rc=q'ni:/unix/cat';$sc={$s,1};$tc={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$y,1};$uc=[$r3];$vc=bless({$c,$tc,$f,$w,$g,$uc},$D);$wc=q'/metaclass::ctors';$xc=[$vc];$yc=bless({$c,$sc,$f,$s,$g,$xc},$D);$zc=q'/metaclass::ctors';$Ac={$V,1};$Bc={$V,1,$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1};$Cc={};$Dc=q'into';$Ec=[];$Fc=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Gc=bless({$S1,$Ec,$s1,$Fc},$R);$Hc={$Dc,$Gc};$Ic=q'/unix/io_stream.b';$Jc=bless({$c,$Cc,$g1,$h1,$i1,$h1,$j1,$Hc,$f,$Ic},$J);$Kc=q'/lib/slice::ctors';$Lc={};$Mc=q'(+';$Nc=[];$Oc=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$Pc=bless({$S1,$Nc,$s1,$Oc},$R);$Qc={$Mc,$Pc};$Rc=q'/unix/io_constructors.b';$Sc=bless({$c,$Lc,$g1,$h1,$i1,$h1,$j1,$Qc,$f,$Rc},$J);$Tc=q'/lib/slice::ctors';$Uc={};$Vc=q'(<>';$Wc=[];$Xc=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Yc=bless({$S1,$Wc,$s1,$Xc},$R);$Zc=q'(@{}';$cd=[];$dd=q'my $self = shift; [<$self>]';$ed=bless({$S1,$cd,$s1,$dd},$R);$fd={$Vc,$Yc,$Zc,$ed};$gd=q'/unix/io_readers.b';$hd=bless({$c,$Uc,$g1,$h1,$i1,$h1,$j1,$fd,$f,$gd},$J);$id=q'/lib/slice::ctors';$jd=[$m2,$Jc,$Sc,$hd];$kd=bless({$c,$Bc,$f,$Z,$g,$jd},$w);$ld=q'/unix/io.c::ctors';$md={};$nd=[];$od=q'shift; +{fs => [@_]}';$pd=bless({$S1,$nd,$s1,$od},$R);$qd={$J1,$pd};$rd=q'/unix/cat_init.b';$sd=bless({$c,$md,$g1,$h1,$i1,$h1,$j1,$qd,$f,$rd},$J);$td=q'/lib/slice::ctors';$ud={};$vd=q'read';$wd=[];$xd=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$yd=bless({$S1,$wd,$s1,$xd},$R);$zd={$vd,$yd};$Ad=q'/unix/cat_read.b';$Bd=bless({$c,$ud,$g1,$h1,$i1,$h1,$j1,$zd,$f,$Ad},$J);$Cd=q'/lib/slice::ctors';$Dd=[$kd,$sd,$Bd];$Ed=bless({$c,$Ac,$f,$V,$g,$Dd},$s);$Fd=q'/unix/cat.c::ctors';$Gd=q'ni:/unix/cat.c';$Hd=q'ni:/unix/cat_init.b';$Id=q'ni:/unix/cat_read.b';$Jd=q'ni:/unix/fd';$Kd={$t,1};$Ld=[$vc];$Md=bless({$c,$Kd,$f,$t,$g,$Ld},$D);$Nd=q'/metaclass::ctors';$Od={$W,1};$Pd={};$Qd=q'fd';$Rd=[];$Sd=q'shift->{\'fd\'}';$Td=bless({$S1,$Rd,$s1,$Sd},$R);$Ud={$Qd,$Td};$Vd=q'/unix/fd_readers.b';$Wd=bless({$c,$Pd,$g1,$h1,$i1,$h1,$j1,$Ud,$f,$Vd},$J);$Xd=q'/lib/slice::ctors';$Yd={};$Zd=[];$ce=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$de=bless({$S1,$Zd,$s1,$ce},$R);$ee={$J1,$de};$fe=q'/unix/fd_init.b';$ge=bless({$c,$Yd,$g1,$h1,$i1,$h1,$j1,$ee,$f,$fe},$J);$he=q'/lib/slice::ctors';$ie={};$je=q'move_to';$ke=[];$le=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$me=bless({$S1,$ke,$s1,$le},$R);$ne={$je,$me};$oe=q'/unix/fd_shell.b';$pe=bless({$c,$ie,$g1,$h1,$i1,$h1,$j1,$ne,$f,$oe},$J);$qe=q'/lib/slice::ctors';$re={$W,1,$X,1,$Y,1};$se=q'/unix/has_fd.b';$te={};$ue=[];$ve=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$we=bless({$S1,$ue,$s1,$ve},$R);$xe=[];$ye=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$ze=bless({$S1,$xe,$s1,$ye},$R);$Ae={$vd,$we,$oa,$ze};$Be=q'/unix/fd_safeio.b';$Ce=bless({$c,$te,$g1,$h1,$i1,$h1,$j1,$Ae,$f,$Be},$J);$De=q'/lib/slice::ctors';$Ee=[$Ce];$Fe=bless({$c,$re,$f,$se,$g,$Ee},$M);$Ge=q'/lib/branch::ctors';$He={};$Ie=q'read_fh';$Je=[];$Ke=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$Le=bless({$S1,$Je,$s1,$Ke},$R);$Me=q'write_fh';$Ne=[];$Oe=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$Pe=bless({$S1,$Ne,$s1,$Oe},$R);$Qe={$Ie,$Le,$Me,$Pe};$Re=q'/unix/fd_io.b';$Se=bless({$c,$He,$g1,$h1,$i1,$h1,$j1,$Qe,$f,$Re},$J);$Te=q'/lib/slice::ctors';$Ue=[$kd,$Wd,$ge,$pe,$Fe,$Se];$Ve=bless({$c,$Od,$f,$W,$g,$Ue},$t);$We=q'/unix/fd.c::ctors';$Xe=q'ni:/unix/fd.c';$Ye=q'ni:/unix/fd_init.b';$Ze=q'ni:/unix/fd_io.b';$cf=q'ni:/unix/fd_readers.b';$df=q'ni:/unix/fd_safeio.b';$ef=q'ni:/unix/fd_shell.b';$ff=q'ni:/unix/fifo';$gf={$u,1};$hf=[$vc];$if=bless({$c,$gf,$f,$u,$g,$hf},$D);$jf=q'/metaclass::ctors';$kf={$X,1};$lf={};$mf=[];$nf=q'shift->{\'read_fh\'}';$of=bless({$S1,$mf,$s1,$nf},$R);$pf=[];$qf=q'shift->{\'write_fh\'}';$rf=bless({$S1,$pf,$s1,$qf},$R);$sf={$Ie,$of,$Me,$rf};$tf=q'/unix/fifo_io.b';$uf=bless({$c,$lf,$g1,$h1,$i1,$h1,$j1,$sf,$f,$tf},$J);$vf=q'/lib/slice::ctors';$wf={};$xf=[];$yf=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$zf=bless({$S1,$xf,$s1,$yf},$R);$Af={$J1,$zf};$Bf=q'/unix/fifo_init.b';$Cf=bless({$c,$wf,$g1,$h1,$i1,$h1,$j1,$Af,$f,$Bf},$J);$Df=q'/lib/slice::ctors';$Ef={};$Ff=q'read_side';$Gf=[];$Hf=q'my $self = shift; close $$self{write_fh}; $self';$If=bless({$S1,$Gf,$s1,$Hf},$R);$Jf=q'write_side';$Kf=[];$Lf=q'my $self = shift; close $$self{read_fh};  $self';$Mf=bless({$S1,$Kf,$s1,$Lf},$R);$Nf={$Ff,$If,$Jf,$Mf};$Of=q'/unix/fifo_direction.b';$Pf=bless({$c,$Ef,$g1,$h1,$i1,$h1,$j1,$Nf,$f,$Of},$J);$Qf=q'/lib/slice::ctors';$Rf=[$kd,$uf,$Cf,$Fe,$Pf];$Sf=bless({$c,$kf,$f,$X,$g,$Rf},$u);$Tf=q'/unix/fifo.c::ctors';$Uf=q'ni:/unix/fifo.c';$Vf=q'ni:/unix/fifo_direction.b';$Wf=q'ni:/unix/fifo_init.b';$Xf=q'ni:/unix/fifo_io.b';$Yf=q'ni:/unix/file';$Zf={$v,1};$cg=[$vc];$dg=bless({$c,$Zf,$f,$v,$g,$cg},$D);$eg=q'/metaclass::ctors';$fg={$Y,1};$gg={};$hg=[];$ig=q'shift->{\'name\'}';$jg=bless({$S1,$hg,$s1,$ig},$R);$kg={$f,$jg};$lg=q'/unix/file_readers.b';$mg=bless({$c,$gg,$g1,$h1,$i1,$h1,$j1,$kg,$f,$lg},$J);$ng=q'/lib/slice::ctors';$og={};$pg=[];$qg=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$rg=bless({$S1,$pg,$s1,$qg},$R);$sg={$J1,$rg};$tg=q'/unix/file_init.b';$ug=bless({$c,$og,$g1,$h1,$i1,$h1,$j1,$sg,$f,$tg},$J);$vg=q'/lib/slice::ctors';$wg={};$xg=[];$yg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$zg=bless({$S1,$xg,$s1,$yg},$R);$Ag=[];$Bg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Cg=bless({$S1,$Ag,$s1,$Bg},$R);$Dg={$Ie,$zg,$Me,$Cg};$Eg=q'/unix/file_io.b';$Fg=bless({$c,$wg,$g1,$h1,$i1,$h1,$j1,$Dg,$f,$Eg},$J);$Gg=q'/lib/slice::ctors';$Hg=[$kd,$mg,$ug,$Fe,$Fg];$Ig=bless({$c,$fg,$f,$Y,$g,$Hg},$v);$Jg=q'/unix/file.c::ctors';$Kg=q'ni:/unix/file.c';$Lg=q'ni:/unix/file_init.b';$Mg=q'ni:/unix/file_io.b';$Ng=q'ni:/unix/file_readers.b';$Og=q'ni:/unix/has_fd.b';$Pg=q'ni:/unix/io';$Qg=q'ni:/unix/io.c';$Rg=q'ni:/unix/io_constructors.b';$Sg=q'ni:/unix/io_readers.b';$Tg=q'ni:/unix/io_stream.b';$Ug=q'ni:/unix/pid';$Vg={$x,1};$Wg=[$vc];$Xg=bless({$c,$Vg,$f,$x,$g,$Wg},$D);$Yg=q'/metaclass::ctors';$Zg={$c1,1};$ch={};$dh=q'pid';$eh=[];$fh=q'shift->{\'pid\'}';$gh=bless({$S1,$eh,$s1,$fh},$R);$hh=q'stderr';$ih=[];$jh=q'shift->{\'stderr\'}';$kh=bless({$S1,$ih,$s1,$jh},$R);$lh=q'stdin';$mh=[];$nh=q'shift->{\'stdin\'}';$oh=bless({$S1,$mh,$s1,$nh},$R);$ph=q'stdout';$qh=[];$rh=q'shift->{\'stdout\'}';$sh=bless({$S1,$qh,$s1,$rh},$R);$th={$dh,$gh,$hh,$kh,$lh,$oh,$ph,$sh};$uh=q'/unix/pid_readers.b';$vh=bless({$c,$ch,$g1,$h1,$i1,$h1,$j1,$th,$f,$uh},$J);$wh=q'/lib/slice::ctors';$xh={};$yh=[];$zh=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Ah=bless({$S1,$yh,$s1,$zh},$R);$Bh={$J1,$Ah};$Ch=q'/unix/pid_init.b';$Dh=bless({$c,$xh,$g1,$h1,$i1,$h1,$j1,$Bh,$f,$Ch},$J);$Eh=q'/lib/slice::ctors';$Fh={};$Gh={};$Hh=q'/unix/pid_wait.b';$Ih=bless({$c,$Fh,$g1,$h1,$i1,$h1,$j1,$Gh,$f,$Hh},$J);$Jh=q'/lib/slice::ctors';$Kh={};$Lh=[];$Mh=q'shift->{stdout}';$Nh=bless({$S1,$Lh,$s1,$Mh},$R);$Oh=[];$Ph=q'shift->{stdin}';$Qh=bless({$S1,$Oh,$s1,$Ph},$R);$Rh={$Ie,$Nh,$Me,$Qh};$Sh=q'/unix/pid_io.b';$Th=bless({$c,$Kh,$g1,$h1,$i1,$h1,$j1,$Rh,$f,$Sh},$J);$Uh=q'/lib/slice::ctors';$Vh=[$kd,$vh,$Dh,$Ih,$Th];$Wh=bless({$c,$Zg,$f,$c1,$g,$Vh},$x);$Xh=q'/unix/pid.c::ctors';$Yh=q'ni:/unix/pid.c';$Zh=q'ni:/unix/pid_init.b';$ci=q'ni:/unix/pid_io.b';$di=q'ni:/unix/pid_readers.b';$ei=q'ni:/unix/pid_wait.b';$fi=q'ni:/unix/str';$gi={$y,1};$hi=[$vc];$ii=bless({$c,$gi,$f,$y,$g,$hi},$D);$ji=q'/metaclass::ctors';$ki={$d1,1};$li={};$mi=[];$ni=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$oi=bless({$S1,$mi,$s1,$ni},$R);$pi={$J1,$oi};$qi=q'/unix/str_init.b';$ri=bless({$c,$li,$g1,$h1,$i1,$h1,$j1,$pi,$f,$qi},$J);$si=q'/lib/slice::ctors';$ti={};$ui=[];$vi=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$wi=bless({$S1,$ui,$s1,$vi},$R);$xi=q'remaining';$yi=[];$zi=q'my $self = shift; $$self{end} - $$self{start}';$Ai=bless({$S1,$yi,$s1,$zi},$R);$Bi=[];$Ci=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Di=bless({$S1,$Bi,$s1,$Ci},$R);$Ei={$vd,$wi,$xi,$Ai,$oa,$Di};$Fi=q'/unix/str_io.b';$Gi=bless({$c,$ti,$g1,$h1,$i1,$h1,$j1,$Ei,$f,$Fi},$J);$Hi=q'/lib/slice::ctors';$Ii=[$kd,$ri,$Gi];$Ji=bless({$c,$ki,$f,$d1,$g,$Ii},$y);$Ki=q'/unix/str.c::ctors';$Li=q'ni:/unix/str.c';$Mi=q'ni:/unix/str_init.b';$Ni=q'ni:/unix/str_io.b';$Oi={$E7,$j8,$g8,$O5,$l8,$u3,$m8,$A5,$n8,$p2,$o8,$E,$p8,$f5,$q8,$e4,$r8,$S4,$s8,$c5,$t8,$O4,$u8,$p5,$v8,$D5,$w8,$d8,$x8,$H7,$y8,$Q7,$z8,$Y7,$A8,$o3,$B8,$d2,$C8,$n1,$D8,$O1,$E8,$Y1,$F8,$xa,$za,$I8,$Aa,$R8,$Ba,$ua,$Ca,$j2,$Da,$B1,$Ea,$y2,$Fa,$m4,$Ga,$u4,$Ha,$B7,$Ia,$Z5,$Ja,$C6,$Ka,$X6,$La,$y7,$Ma,$l7,$Na,$p6,$Oa,$Wb,$Yb,$Tb,$Zb,$F4,$cc,$C4,$dc,$f3,$ec,$L2,$fc,$H,$gc,$S2,$hc,$c3,$ic,$L5,$jc,$R3,$kc,$H3,$lc,$z3,$mc,$O3,$nc,$V5,$oc,$R5,$pc,$m2,$qc,$r3,$rc,$Ed,$Gd,$yc,$Hd,$sd,$Id,$Bd,$Jd,$Ve,$Xe,$Md,$Ye,$ge,$Ze,$Se,$cf,$Wd,$df,$Ce,$ef,$pe,$ff,$Sf,$Uf,$if,$Vf,$Pf,$Wf,$Cf,$Xf,$uf,$Yf,$Ig,$Kg,$dg,$Lg,$ug,$Mg,$Fg,$Ng,$mg,$Og,$Fe,$Pg,$kd,$Qg,$vc,$Rg,$Sc,$Sg,$hd,$Tg,$Jc,$Ug,$Wh,$Yh,$Xg,$Zh,$Dh,$ci,$Th,$di,$vh,$ei,$Ih,$fi,$Ji,$Li,$ii,$Mi,$ri,$Ni,$Gi};$Pi=q'resolvers';$Qi=[];$Ri=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$Si=bless({$S1,$Qi,$s1,$Ri},$R);$Ti=q'file';$Ui=[];$Vi=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$Wi=bless({$S1,$Ui,$s1,$Vi},$R);$Xi=q'str';$Yi=[];$Zi=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$cj=bless({$S1,$Yi,$s1,$Zi},$R);$dj={$Qd,$Si,$Ti,$Wi,$Xi,$cj};$ej=bless({$D7,$Oi,$Pi,$dj},$T);$fj=q'/lib/ni::ctors';$$q3[0]=$O5;$$C[0]=$r3;$$m1[0]=$r3;$$c2[0]=$m2;$$e5[4]=$D5;*$F2=\&$D2;*$E2=\&$B2;$B1->apply_unsafe($P);$B1->apply_unsafe($h);$B1->apply_unsafe($j);$B1->apply_unsafe($M);$B1->apply_unsafe($k);$B1->apply_unsafe($l);$B1->apply_unsafe($R);$B1->apply_unsafe($m);$B1->apply_unsafe($n);$B1->apply_unsafe($o);$B1->apply_unsafe($J);$B1->apply_unsafe($p);$B1->apply_unsafe($N);$B1->apply_unsafe($q);$B1->apply_unsafe($D);$B1->apply_unsafe($d);$B1->apply_unsafe($r);$B1->apply_unsafe($s);$B1->apply_unsafe($t);$B1->apply_unsafe($u);$B1->apply_unsafe($v);$B1->apply_unsafe($w);$B1->apply_unsafe($x);$B1->apply_unsafe($y);$O1->apply_unsafe($R);$Y1->apply_unsafe($R);$j2->apply_unsafe($P);$j2->apply_unsafe($h);$j2->apply_unsafe($L);$j2->apply_unsafe($j);$j2->apply_unsafe($M);$j2->apply_unsafe($k);$j2->apply_unsafe($Q);$j2->apply_unsafe($l);$j2->apply_unsafe($R);$j2->apply_unsafe($m);$j2->apply_unsafe($S);$j2->apply_unsafe($n);$j2->apply_unsafe($T);$j2->apply_unsafe($o);$j2->apply_unsafe($J);$j2->apply_unsafe($p);$j2->apply_unsafe($N);$j2->apply_unsafe($q);$j2->apply_unsafe($D);$j2->apply_unsafe($d);$j2->apply_unsafe($U);$j2->apply_unsafe($r);$j2->apply_unsafe($V);$j2->apply_unsafe($s);$j2->apply_unsafe($W);$j2->apply_unsafe($t);$j2->apply_unsafe($X);$j2->apply_unsafe($u);$j2->apply_unsafe($Y);$j2->apply_unsafe($v);$j2->apply_unsafe($Z);$j2->apply_unsafe($w);$j2->apply_unsafe($c1);$j2->apply_unsafe($x);$j2->apply_unsafe($d1);$j2->apply_unsafe($y);$y2->apply_unsafe($P);$y2->apply_unsafe($h);$y2->apply_unsafe($j);$y2->apply_unsafe($M);$y2->apply_unsafe($k);$y2->apply_unsafe($Q);$y2->apply_unsafe($l);$y2->apply_unsafe($m);$y2->apply_unsafe($n);$y2->apply_unsafe($o);$y2->apply_unsafe($J);$y2->apply_unsafe($p);$y2->apply_unsafe($N);$y2->apply_unsafe($q);$y2->apply_unsafe($D);$y2->apply_unsafe($d);$y2->apply_unsafe($r);$y2->apply_unsafe($s);$y2->apply_unsafe($t);$y2->apply_unsafe($u);$y2->apply_unsafe($v);$y2->apply_unsafe($w);$y2->apply_unsafe($x);$y2->apply_unsafe($y);$L2->apply_unsafe($J);$S2->apply_unsafe($J);$c3->apply_unsafe($J);$o3->apply_unsafe($h);$o3->apply_unsafe($j);$o3->apply_unsafe($k);$o3->apply_unsafe($l);$o3->apply_unsafe($m);$o3->apply_unsafe($n);$o3->apply_unsafe($o);$o3->apply_unsafe($p);$o3->apply_unsafe($q);$o3->apply_unsafe($r);$o3->apply_unsafe($s);$o3->apply_unsafe($t);$o3->apply_unsafe($u);$o3->apply_unsafe($v);$o3->apply_unsafe($w);$o3->apply_unsafe($x);$o3->apply_unsafe($y);$H3->apply_unsafe($N);$O3->apply_unsafe($N);$e4->apply_unsafe($P);$e4->apply_unsafe($h);$e4->apply_unsafe($j);$e4->apply_unsafe($M);$e4->apply_unsafe($k);$e4->apply_unsafe($l);$e4->apply_unsafe($m);$e4->apply_unsafe($n);$e4->apply_unsafe($o);$e4->apply_unsafe($p);$e4->apply_unsafe($q);$e4->apply_unsafe($D);$e4->apply_unsafe($d);$e4->apply_unsafe($r);$e4->apply_unsafe($s);$e4->apply_unsafe($t);$e4->apply_unsafe($u);$e4->apply_unsafe($v);$e4->apply_unsafe($w);$e4->apply_unsafe($x);$e4->apply_unsafe($y);$m4->apply_unsafe($P);$m4->apply_unsafe($h);$m4->apply_unsafe($j);$m4->apply_unsafe($M);$m4->apply_unsafe($k);$m4->apply_unsafe($l);$m4->apply_unsafe($m);$m4->apply_unsafe($n);$m4->apply_unsafe($o);$m4->apply_unsafe($J);$m4->apply_unsafe($p);$m4->apply_unsafe($N);$m4->apply_unsafe($q);$m4->apply_unsafe($D);$m4->apply_unsafe($d);$m4->apply_unsafe($r);$m4->apply_unsafe($s);$m4->apply_unsafe($t);$m4->apply_unsafe($u);$m4->apply_unsafe($v);$m4->apply_unsafe($w);$m4->apply_unsafe($x);$m4->apply_unsafe($y);$u4->apply_unsafe($P);$u4->apply_unsafe($h);$u4->apply_unsafe($j);$u4->apply_unsafe($M);$u4->apply_unsafe($k);$u4->apply_unsafe($l);$u4->apply_unsafe($m);$u4->apply_unsafe($n);$u4->apply_unsafe($o);$u4->apply_unsafe($J);$u4->apply_unsafe($p);$u4->apply_unsafe($N);$u4->apply_unsafe($q);$u4->apply_unsafe($D);$u4->apply_unsafe($d);$u4->apply_unsafe($r);$u4->apply_unsafe($s);$u4->apply_unsafe($t);$u4->apply_unsafe($u);$u4->apply_unsafe($v);$u4->apply_unsafe($w);$u4->apply_unsafe($x);$u4->apply_unsafe($y);$C4->apply_unsafe($P);$C4->apply_unsafe($h);$C4->apply_unsafe($j);$C4->apply_unsafe($M);$C4->apply_unsafe($k);$C4->apply_unsafe($l);$C4->apply_unsafe($m);$C4->apply_unsafe($n);$C4->apply_unsafe($o);$C4->apply_unsafe($p);$C4->apply_unsafe($N);$C4->apply_unsafe($q);$C4->apply_unsafe($D);$C4->apply_unsafe($d);$C4->apply_unsafe($r);$C4->apply_unsafe($s);$C4->apply_unsafe($t);$C4->apply_unsafe($u);$C4->apply_unsafe($v);$C4->apply_unsafe($w);$C4->apply_unsafe($x);$C4->apply_unsafe($y);$O4->apply_unsafe($P);$O4->apply_unsafe($h);$O4->apply_unsafe($j);$O4->apply_unsafe($k);$O4->apply_unsafe($l);$O4->apply_unsafe($m);$O4->apply_unsafe($n);$O4->apply_unsafe($o);$O4->apply_unsafe($p);$O4->apply_unsafe($q);$O4->apply_unsafe($D);$O4->apply_unsafe($d);$O4->apply_unsafe($r);$O4->apply_unsafe($s);$O4->apply_unsafe($t);$O4->apply_unsafe($u);$O4->apply_unsafe($v);$O4->apply_unsafe($w);$O4->apply_unsafe($x);$O4->apply_unsafe($y);$c5->apply_unsafe($M);$p5->apply_unsafe($P);$p5->apply_unsafe($h);$p5->apply_unsafe($j);$p5->apply_unsafe($M);$p5->apply_unsafe($k);$p5->apply_unsafe($l);$p5->apply_unsafe($m);$p5->apply_unsafe($n);$p5->apply_unsafe($o);$p5->apply_unsafe($p);$p5->apply_unsafe($q);$p5->apply_unsafe($D);$p5->apply_unsafe($d);$p5->apply_unsafe($r);$p5->apply_unsafe($s);$p5->apply_unsafe($t);$p5->apply_unsafe($u);$p5->apply_unsafe($v);$p5->apply_unsafe($w);$p5->apply_unsafe($x);$p5->apply_unsafe($y);$A5->apply_unsafe($P);$A5->apply_unsafe($h);$A5->apply_unsafe($j);$A5->apply_unsafe($M);$A5->apply_unsafe($k);$A5->apply_unsafe($l);$A5->apply_unsafe($m);$A5->apply_unsafe($n);$A5->apply_unsafe($o);$A5->apply_unsafe($p);$A5->apply_unsafe($q);$A5->apply_unsafe($D);$A5->apply_unsafe($d);$A5->apply_unsafe($r);$A5->apply_unsafe($s);$A5->apply_unsafe($t);$A5->apply_unsafe($u);$A5->apply_unsafe($v);$A5->apply_unsafe($w);$A5->apply_unsafe($x);$A5->apply_unsafe($y);$L5->apply_unsafe($P);$L5->apply_unsafe($h);$L5->apply_unsafe($j);$L5->apply_unsafe($k);$L5->apply_unsafe($l);$L5->apply_unsafe($m);$L5->apply_unsafe($n);$L5->apply_unsafe($o);$L5->apply_unsafe($p);$L5->apply_unsafe($q);$L5->apply_unsafe($d);$L5->apply_unsafe($r);$L5->apply_unsafe($s);$L5->apply_unsafe($t);$L5->apply_unsafe($u);$L5->apply_unsafe($v);$L5->apply_unsafe($w);$L5->apply_unsafe($x);$L5->apply_unsafe($y);$p6->apply_unsafe($T);$C6->apply_unsafe($T);$X6->apply_unsafe($T);$l7->apply_unsafe($T);$y7->apply_unsafe($T);$Q7->apply_unsafe($Q);$Y7->apply_unsafe($Q);$R8->apply_unsafe($S);$ua->apply_unsafe($S);$Tb->apply_unsafe($Pa);$Tb->apply_unsafe($Qa);$Jc->apply_unsafe($V);$Jc->apply_unsafe($W);$Jc->apply_unsafe($X);$Jc->apply_unsafe($Y);$Jc->apply_unsafe($Z);$Jc->apply_unsafe($c1);$Jc->apply_unsafe($d1);$Sc->apply_unsafe($V);$Sc->apply_unsafe($W);$Sc->apply_unsafe($X);$Sc->apply_unsafe($Y);$Sc->apply_unsafe($Z);$Sc->apply_unsafe($c1);$Sc->apply_unsafe($d1);$hd->apply_unsafe($V);$hd->apply_unsafe($W);$hd->apply_unsafe($X);$hd->apply_unsafe($Y);$hd->apply_unsafe($Z);$hd->apply_unsafe($c1);$hd->apply_unsafe($d1);$sd->apply_unsafe($V);$Bd->apply_unsafe($V);$Wd->apply_unsafe($W);$ge->apply_unsafe($W);$pe->apply_unsafe($W);$Ce->apply_unsafe($W);$Ce->apply_unsafe($X);$Ce->apply_unsafe($Y);$Se->apply_unsafe($W);$uf->apply_unsafe($X);$Cf->apply_unsafe($X);$Pf->apply_unsafe($X);$mg->apply_unsafe($Y);$ug->apply_unsafe($Y);$Fg->apply_unsafe($Y);$vh->apply_unsafe($c1);$Dh->apply_unsafe($c1);$Ih->apply_unsafe($c1);$Th->apply_unsafe($c1);$ri->apply_unsafe($d1);$Gi->apply_unsafe($d1);$ni::self=$ej;&$_($E)for@$F;&$_($H)for@$I;&$_($n1)for@$o1;&$_($u1)for@$v1;&$_($y1)for@$v1;&$_($B1)for@$C1;&$_($F1)for@$v1;&$_($I1)for@$v1;&$_($L1)for@$v1;&$_($O1)for@$P1;&$_($V1)for@$v1;&$_($Y1)for@$Z1;&$_($d2)for@$e2;&$_($g2)for@$v1;&$_($j2)for@$k2;&$_($m2)for@$n2;&$_($p2)for@$q2;&$_($t2)for@$v1;&$_($v2)for@$v1;&$_($y2)for@$z2;&$_($B2)for@$v1;&$_($D2)for@$v1;&$_($L2)for@$M2;&$_($P2)for@$v1;&$_($S2)for@$T2;&$_($X2)for@$v1;&$_($c3)for@$d3;&$_($f3)for@$g3;&$_($l3)for@$v1;&$_($o3)for@$p3;&$_($r3)for@$s3;&$_($u3)for@$v3;&$_($z3)for@$A3;&$_($E3)for@$v1;&$_($H3)for@$I3;&$_($L3)for@$v1;&$_($O3)for@$P3;&$_($R3)for@$S3;&$_($X3)for@$v1;&$_($Z3)for@$v1;&$_($e4)for@$f4;&$_($j4)for@$v1;&$_($m4)for@$n4;&$_($r4)for@$v1;&$_($u4)for@$v4;&$_($z4)for@$v1;&$_($C4)for@$D4;&$_($F4)for@$G4;&$_($J4)for@$v1;&$_($L4)for@$v1;&$_($O4)for@$P4;&$_($S4)for@$T4;&$_($X4)for@$v1;&$_($c5)for@$d5;&$_($f5)for@$g5;&$_($m5)for@$v1;&$_($p5)for@$q5;&$_($u5)for@$v1;&$_($x5)for@$v1;&$_($A5)for@$B5;&$_($D5)for@$E5;&$_($I5)for@$v1;&$_($L5)for@$M5;&$_($O5)for@$P5;&$_($R5)for@$S5;&$_($V5)for@$W5;&$_($Z5)for@$c6;&$_($i6)for@$v1;&$_($m6)for@$v1;&$_($p6)for@$q6;&$_($v6)for@$v1;&$_($z6)for@$v1;&$_($C6)for@$D6;&$_($I6)for@$v1;&$_($M6)for@$v1;&$_($Q6)for@$v1;&$_($U6)for@$v1;&$_($X6)for@$Y6;&$_($e7)for@$v1;&$_($i7)for@$v1;&$_($l7)for@$m7;&$_($r7)for@$v1;&$_($v7)for@$v1;&$_($y7)for@$z7;&$_($B7)for@$C7;&$_($H7)for@$I7;&$_($N7)for@$v1;&$_($Q7)for@$R7;&$_($V7)for@$v1;&$_($Y7)for@$Z7;&$_($d8)for@$e8;&$_($j8)for@$k8;&$_($I8)for@$J8;&$_($O8)for@$v1;&$_($R8)for@$S8;&$_($X8)for@$v1;&$_($d9)for@$v1;&$_($h9)for@$v1;&$_($l9)for@$v1;&$_($p9)for@$v1;&$_($t9)for@$v1;&$_($x9)for@$v1;&$_($B9)for@$v1;&$_($F9)for@$v1;&$_($J9)for@$v1;&$_($N9)for@$v1;&$_($R9)for@$v1;&$_($V9)for@$v1;&$_($Z9)for@$v1;&$_($fa)for@$v1;&$_($ja)for@$v1;&$_($na)for@$v1;&$_($ra)for@$v1;&$_($ua)for@$va;&$_($xa)for@$ya;&$_($Wa)for@$v1;&$_($cb)for@$v1;&$_($gb)for@$v1;&$_($kb)for@$v1;&$_($ob)for@$v1;&$_($sb)for@$v1;&$_($wb)for@$v1;&$_($Ab)for@$v1;&$_($Eb)for@$v1;&$_($Ib)for@$v1;&$_($Mb)for@$v1;&$_($Qb)for@$v1;&$_($Tb)for@$Ub;&$_($Wb)for@$Xb;&$_($vc)for@$wc;&$_($yc)for@$zc;&$_($Gc)for@$v1;&$_($Jc)for@$Kc;&$_($Pc)for@$v1;&$_($Sc)for@$Tc;&$_($Yc)for@$v1;&$_($ed)for@$v1;&$_($hd)for@$id;&$_($kd)for@$ld;&$_($pd)for@$v1;&$_($sd)for@$td;&$_($yd)for@$v1;&$_($Bd)for@$Cd;&$_($Ed)for@$Fd;&$_($Md)for@$Nd;&$_($Td)for@$v1;&$_($Wd)for@$Xd;&$_($de)for@$v1;&$_($ge)for@$he;&$_($me)for@$v1;&$_($pe)for@$qe;&$_($we)for@$v1;&$_($ze)for@$v1;&$_($Ce)for@$De;&$_($Fe)for@$Ge;&$_($Le)for@$v1;&$_($Pe)for@$v1;&$_($Se)for@$Te;&$_($Ve)for@$We;&$_($if)for@$jf;&$_($of)for@$v1;&$_($rf)for@$v1;&$_($uf)for@$vf;&$_($zf)for@$v1;&$_($Cf)for@$Df;&$_($If)for@$v1;&$_($Mf)for@$v1;&$_($Pf)for@$Qf;&$_($Sf)for@$Tf;&$_($dg)for@$eg;&$_($jg)for@$v1;&$_($mg)for@$ng;&$_($rg)for@$v1;&$_($ug)for@$vg;&$_($zg)for@$v1;&$_($Cg)for@$v1;&$_($Fg)for@$Gg;&$_($Ig)for@$Jg;&$_($Xg)for@$Yg;&$_($gh)for@$v1;&$_($kh)for@$v1;&$_($oh)for@$v1;&$_($sh)for@$v1;&$_($vh)for@$wh;&$_($Ah)for@$v1;&$_($Dh)for@$Eh;&$_($Ih)for@$Jh;&$_($Nh)for@$v1;&$_($Qh)for@$v1;&$_($Th)for@$Uh;&$_($Wh)for@$Xh;&$_($ii)for@$ji;&$_($oi)for@$v1;&$_($ri)for@$si;&$_($wi)for@$v1;&$_($Ai)for@$v1;&$_($Di)for@$v1;&$_($Gi)for@$Hi;&$_($Ji)for@$Ki;&$_($Si)for@$v1;&$_($Wi)for@$v1;&$_($cj)for@$v1;&$_($ej)for@$fj;ni->run(@ARGV);
__DATA__
