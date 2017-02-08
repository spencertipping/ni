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
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->{named}{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::eval {eval shift}
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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1};$t={$p,1};$u={$j,1,$k,1,$p,1,$q,1};$v=[undef];$w=q'/metaclass';$x=bless({$c,$u,$f,$j,$g,$v},$w);$y=q'/metaclass::ctors';$z=[$x];$A=bless({$c,$t,$f,$p,$g,$z},$w);$B=q'/metaclass::ctors';$C=q'/lib/slice';$D={$C,1};$E=q'/lib/behavior';$F=q'/lib/branch';$G=q'/lib/tag';$H={$E,1,$F,1,$C,1,$G,1};$I=q'/class';$J=q'/object';$K={$I,1,$h,1,$E,1,$j,1,$F,1,$k,1,$l,1,$m,1,$n,1,$o,1,$C,1,$p,1,$G,1,$q,1,$w,1,$d,1,$J,1,$r,1};$L={};$M=q'ctor';$N=undef;$O=q'dtor';$P=q'methods';$Q=q'class';$R={$m,1};$S=[undef];$T=bless({$c,$R,$f,$m,$g,$S},$w);$U=q'/metaclass::ctors';$V=q'/lib/fn';$W={$V,1};$X={};$Y=q'DESTROY';$Z=q'code';$c1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$d1=bless({$Z,$c1},$V);$e1=q'/lib/fn::ctors';$f1=q'new';$g1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$h1=bless({$Z,$g1},$V);$i1={$Y,$d1,$f1,$h1};$j1=q'/lib/instantiable.b';$k1=bless({$c,$X,$P,$i1,$f,$j1},$C);$l1=q'/lib/slice::ctors';$m1={};$n1=q'shift->compile';$o1=bless({$Z,$n1},$V);$p1=q'compile';$q1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$r1=bless({$Z,$q1},$V);$s1=q'instantiate';$t1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$u1=bless({$Z,$t1},$V);$v1={$p1,$r1,$s1,$u1};$w1=q'/lib/fn_init.b';$x1=bless({$c,$m1,$M,$o1,$O,$N,$P,$v1,$f,$w1},$C);$y1=q'/lib/slice::ctors';$z1={};$A1=q'serialize';$B1=q'annotations';$C1=[];$D1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$E1=bless({$B1,$C1,$Z,$D1},$V);$F1={$A1,$E1};$G1=q'/lib/fn_serialize.b';$H1=bless({$c,$z1,$M,$N,$O,$N,$P,$F1,$f,$G1},$C);$I1=q'/lib/slice::ctors';$J1=[$k1,$x1,$H1];$K1=bless({$c,$W,$f,$V,$g,$J1},$m);$L1=q'/lib/fn.c::ctors';$M1=q'ni \'ni:\' . ref shift';$N1=bless({$Z,$M1},$V);$O1={$Q,$N1};$P1=q'/lib/instance.b';$Q1=bless({$c,$L,$M,$N,$O,$N,$P,$O1,$f,$P1},$C);$R1=q'/lib/slice::ctors';$S1=[$Q1];$T1=bless({$c,$K,$f,$J,$g,$S1},$r);$U1=q'/object.c::ctors';$V1=[$T1];$W1=bless({$c,$H,$f,$E,$g,$V1},$j);$X1=q'/lib/behavior.c::ctors';$Y1={};$Z1=q'my $s = shift; ni->def($s->name, $s)';$c2=bless({$Z,$Z1},$V);$d2=q'$_[0]->namespace . ":" . $_[0]->{name}';$e2=bless({$Z,$d2},$V);$f2={$f,$e2};$g2=q'/lib/named.b';$h2=bless({$c,$Y1,$M,$c2,$O,$N,$P,$f2,$f,$g2},$C);$i2=q'/lib/doc';$j2=q'/lib/slice::ctors';$k2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$l2=bless({$Z,$k2},$V);$m2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$n2=bless({$Z,$m2},$V);$o2=q'/lib/slice::apply';$p2=q'/lib/slice::apply_unsafe';$q2={};$r2=q'apply';$s2=q'apply_unsafe';$t2={$r2,$l2,$s2,$n2};$u2=q'/lib/slice.b';$v2=bless({$c,$q2,$P,$t2,$f,$u2},$C);$w2=q'/lib/slice::ctors';$x2={};$y2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$z2=bless({$Z,$y2},$V);$A2={$s1,$z2};$B2=q'/lib/slice_init.b';$C2=bless({$c,$x2,$P,$A2,$f,$B2},$C);$D2=q'/lib/slice::ctors';$E2={};$F2=[];$G2=q'local $_;
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
$g;';$H2=bless({$B1,$F2,$Z,$G2},$V);$I2={$A1,$H2};$J2=q'/lib/slice_serialize.b';$K2=bless({$c,$E2,$M,$N,$O,$N,$P,$I2,$f,$J2},$C);$L2=q'/lib/slice::ctors';$M2=[$W1,$h2,$v2,$C2,$K2];$N2=bless({$c,$D,$f,$C,$g,$M2},$p);$O2=q'/lib/slice.c::ctors';$P2={};$Q2=q'doc';$R2=[];$S2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$T2=bless({$B1,$R2,$Z,$S2},$V);$U2={$Q2,$T2};$V2=q'/lib/documentable.b';$W2=bless({$c,$P2,$M,$N,$O,$N,$P,$U2,$f,$V2},$C);$X2=q'/lib/slice::ctors';$Y2=[undef,$W2];$Z2=bless({$c,$s,$f,$r,$g,$Y2},$w);$c3=q'/metaclass::ctors';$d3=[$Z2];$e3=bless({$c,$i,$f,$h,$g,$d3},$w);$f3=q'/metaclass::ctors';$g3={$I,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1};$h3={$q,1};$i3=[$x];$j3=bless({$c,$h3,$f,$q,$g,$i3},$w);$k3=q'/metaclass::ctors';$l3={$G,1};$m3={};$n3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$o3=bless({$Z,$n3},$V);$p3={$r2,$o3};$q3=q'/lib/tag.b';$r3=bless({$c,$m3,$M,$N,$O,$N,$P,$p3,$f,$q3},$C);$s3=q'/lib/slice::ctors';$t3={};$u3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$v3=bless({$Z,$u3},$V);$w3={$s1,$v3};$x3=q'/lib/tag_init.b';$y3=bless({$c,$t3,$M,$N,$O,$N,$P,$w3,$f,$x3},$C);$z3=q'/lib/slice::ctors';$A3=[$W1,$h2,$r3,$y3];$B3=bless({$c,$l3,$f,$G,$g,$A3},$q);$C3=q'/lib/tag.c::ctors';$D3=q'/lib/perlbranch.b';$E3={};$F3=q'add';$G3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$H3=bless({$Z,$G3},$V);$I3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$J3=bless({$Z,$I3},$V);$K3={$F3,$H3,$r2,$J3};$L3=q'/lib/branch.b';$M3=bless({$c,$E3,$M,$N,$O,$N,$P,$K3,$f,$L3},$C);$N3=q'/lib/slice::ctors';$O3={};$P3=q'namespace';$Q3=q'\'ni\'';$R3=bless({$Z,$Q3},$V);$S3={$P3,$R3};$T3=q'/lib/named_in_ni.b';$U3=bless({$c,$O3,$M,$N,$O,$N,$P,$S3,$f,$T3},$C);$V3=q'/lib/slice::ctors';$W3={};$X3=q'package';$Y3=q'shift->{name}';$Z3=bless({$Z,$Y3},$V);$c4={$X3,$Z3};$d4=q'/lib/namespaced.b';$e4=bless({$c,$W3,$M,$N,$O,$N,$P,$c4,$f,$d4},$C);$f4=q'/lib/slice::ctors';$g4={};$h4=q'resolve';$i4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$j4=bless({$Z,$i4},$V);$k4={$h4,$j4};$l4=q'/lib/resolver.b';$m4=bless({$c,$g4,$M,$N,$O,$N,$P,$k4,$f,$l4},$C);$n4=q'/lib/slice::ctors';$o4=[$M3,$k1,$h2,$U3,$e4,$m4];$p4=bless({$f,$D3,$g,$o4},$G);$q4=q'/lib/tag::ctors';$r4={};$s4=q'my $s = shift; $s->apply($s->package)';$t4=bless({$Z,$s4},$V);$u4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$v4=bless({$Z,$u4},$V);$w4={$s1,$v4};$x4=q'/lib/class_init.b';$y4=bless({$c,$r4,$M,$t4,$O,$N,$P,$w4,$f,$x4},$C);$z4=q'/lib/slice::ctors';$A4={$k,1};$B4=[$x];$C4=bless({$c,$A4,$f,$k,$g,$B4},$w);$D4=q'/metaclass::ctors';$E4={$F,1};$F4={};$G4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$H4=bless({$Z,$G4},$V);$I4={$s1,$H4};$J4=q'/lib/branch_init.b';$K4=bless({$c,$F4,$M,$N,$O,$N,$P,$I4,$f,$J4},$C);$L4=q'/lib/slice::ctors';$M4=[$W1,$h2,$M3,$K4,undef];$N4=bless({$c,$E4,$f,$F,$g,$M4},$k);$O4=q'/lib/branch.c::ctors';$P4={$I,1,$h,1,$j,1,$F,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$w,1,$d,1,$r,1};$Q4=q'/lib/definition.b';$R4={};$S4=q'def';$T4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$U4=bless({$Z,$T4},$V);$V4={$S4,$U4};$W4=q'/lib/classdef.b';$X4=bless({$c,$R4,$M,$N,$O,$N,$P,$V4,$f,$W4},$C);$Y4=q'/lib/slice::ctors';$Z4={};$c5=q'ro';$d5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$e5=bless({$Z,$d5},$V);$f5=q'rw';$g5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$h5=bless({$Z,$g5},$V);$i5={$c5,$e5,$f5,$h5};$j5=q'/lib/accessor.b';$k5=bless({$c,$Z4,$M,$N,$O,$N,$P,$i5,$f,$j5},$C);$l5=q'/lib/slice::ctors';$m5=[$X4,$k5];$n5=bless({$c,$P4,$f,$Q4,$g,$m5},$F);$o5=q'/lib/branch::ctors';$p5={};$q5=q'child';$r5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$s5=bless({$Z,$r5},$V);$t5={$q5,$s5};$u5=q'/lib/subclass.b';$v5=bless({$c,$p5,$M,$N,$O,$N,$P,$t5,$f,$u5},$C);$w5=q'/lib/slice::ctors';$x5=[$p4,$y4,$T1,$n5,$v5];$y5=bless({$c,$g3,$f,$I,$g,$x5},$h);$z5=q'/class.c::ctors';$A5=[$y5];$B5=bless({$c,$e,$f,$d,$g,$A5},$w);$C5=q'/metaclass::ctors';$D5={$w,1};$E5=[$p4,$y4,$T1,$n5];$F5=bless({$c,$D5,$f,$w,$g,$E5},$d);$G5=q'/metaclass.c::ctors';$H5={$o,1};$I5=[$Z2];$J5=bless({$c,$H5,$f,$o,$g,$I5},$w);$K5=q'/metaclass::ctors';$L5=q'/lib/ni';$M5={$L5,1};$N5={};$O5=q'is_mutable';$P5=[];$Q5=q'$0 ne "-" && -w $0';$R5=bless({$B1,$P5,$Z,$Q5},$V);$S5=q'modify';$T5=[];$U5=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$V5=bless({$B1,$T5,$Z,$U5},$V);$W5={$O5,$R5,$S5,$V5};$X5=q'/lib/ni_self.b';$Y5=bless({$c,$N5,$M,$N,$O,$N,$P,$W5,$f,$X5},$C);$Z5=q'/lib/slice::ctors';$c6={};$d6=q'exists';$e6=[];$f6=q'exists $_[0]->{named}{$_[1]}';$g6=bless({$B1,$e6,$Z,$f6},$V);$h6=q'quoted';$i6=[];$j6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$k6=bless({$B1,$i6,$Z,$j6},$V);$l6={$d6,$g6,$h6,$k6};$m6=q'/lib/ni_image.b';$n6=bless({$c,$c6,$M,$N,$O,$N,$P,$l6,$f,$m6},$C);$o6=q'/lib/slice::ctors';$p6={};$q6=q'internal/+=';$r6=[];$s6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$t6=bless({$B1,$r6,$Z,$s6},$V);$u6=q'internal/eval';$v6=[];$w6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$x6=bless({$B1,$v6,$Z,$w6},$V);$y6=q'internal/image';$z6=[];$A6=q'shift->quoted->write(\\*STDOUT);
0;';$B6=bless({$B1,$z6,$Z,$A6},$V);$C6=q'run';$D6=[];$E6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';$F6=bless({$B1,$D6,$Z,$E6},$V);$G6={$q6,$t6,$u6,$x6,$y6,$B6,$C6,$F6};$H6=q'/lib/ni_main.b';$I6=bless({$c,$p6,$M,$N,$O,$N,$P,$G6,$f,$H6},$C);$J6=q'/lib/slice::ctors';$K6=[$Y5,$n6,$I6];$L6=bless({$c,$M5,$f,$L5,$g,$K6},$o);$M6=q'/lib/ni.c::ctors';$N6=q'named';$O6=q'ni.doc:/class';$P6={$l,1};$Q6=[$Z2];$R6=bless({$c,$P6,$f,$l,$g,$Q6},$w);$S6=q'/metaclass::ctors';$T6={$i2,1};$U6={};$V6=[];$W6=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$X6=bless({$B1,$V6,$Z,$W6},$V);$Y6={$s1,$X6};$Z6=q'/lib/doc_init.b';$c7=bless({$c,$U6,$M,$N,$O,$N,$P,$Y6,$f,$Z6},$C);$d7=q'/lib/slice::ctors';$e7={};$f7=[];$g7=q'\'ni.doc\'';$h7=bless({$B1,$f7,$Z,$g7},$V);$i7={$P3,$h7};$j7=q'/lib/doc_namespace.b';$k7=bless({$c,$e7,$M,$N,$O,$N,$P,$i7,$f,$j7},$C);$l7=q'/lib/slice::ctors';$m7=[$h2,$c7,$k7];$n7=bless({$c,$T6,$f,$i2,$g,$m7},$l);$o7=q'/lib/doc.c::ctors';$p7=q'apropos';$q7=q'ni:/class';$r7=[$q7];$s7=q'# Classes and metaclasses
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
';$t7=bless({$p7,$r7,$Q2,$s7,$f,$I},$i2);$u7=q'/lib/doc::ctors';$v7=q'ni:/class.c';$w7=q'ni:/lib/accessor.b';$x7=q'ni:/lib/behavior';$y7=q'ni:/lib/behavior.c';$z7=q'ni:/lib/branch';$A7=q'ni:/lib/branch.b';$B7=q'ni:/lib/branch.c';$C7=q'ni:/lib/branch_init.b';$D7=q'ni:/lib/class_init.b';$E7=q'ni:/lib/classdef.b';$F7=q'ni:/lib/definition.b';$G7=q'ni:/lib/doc';$H7=q'ni:/lib/doc.c';$I7=q'ni:/lib/doc_init.b';$J7=q'ni:/lib/doc_namespace.b';$K7=q'ni:/lib/documentable.b';$L7=q'ni:/lib/fn';$M7=q'ni:/lib/fn.c';$N7=q'ni:/lib/fn_init.b';$O7=q'ni:/lib/fn_serialize.b';$P7=q'ni:/lib/image';$Q7={$n,1};$R7=[$Z2];$S7=bless({$c,$Q7,$f,$n,$g,$R7},$w);$T7=q'/metaclass::ctors';$U7=q'/lib/image';$V7={$U7,1};$W7={};$X7=[];$Y7=q'my $class = shift;
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
  ordering     => []};';$Z7=bless({$B1,$X7,$Z,$Y7},$V);$c8={$s1,$Z7};$d8=q'/lib/image_init.b';$e8=bless({$c,$W7,$M,$N,$O,$N,$P,$c8,$f,$d8},$C);$f8=q'/lib/slice::ctors';$g8={};$h8=q'address';$i8=[];$j8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$k8=bless({$B1,$i8,$Z,$j8},$V);$l8=q'allocate_gensym';$m8=[];$n8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$o8=bless({$B1,$m8,$Z,$n8},$V);$p8=q'boot_side_effect';$q8=[];$r8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$s8=bless({$B1,$q8,$Z,$r8},$V);$t8=q'circular_links';$u8=[];$v8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$w8=bless({$B1,$u8,$Z,$v8},$V);$x8=q'finalizer';$y8=[];$z8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$A8=bless({$B1,$y8,$Z,$z8},$V);$B8=q'gensym';$C8=[];$D8=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$E8=bless({$B1,$C8,$Z,$D8},$V);$F8=q'is_circular';$G8=[];$H8=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$I8=bless({$B1,$G8,$Z,$H8},$V);$J8=q'quote';$K8=[];$L8=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$M8=bless({$B1,$K8,$Z,$L8},$V);$N8=q'quote_array';$O8=[];$P8=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$Q8=bless({$B1,$O8,$Z,$P8},$V);$R8=q'quote_blessed';$S8=[];$T8=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$U8=bless({$B1,$S8,$Z,$T8},$V);$V8=q'quote_class';$W8=[];$X8=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$Y8=bless({$B1,$W8,$Z,$X8},$V);$Z8=q'quote_hash';$c9=[];$d9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$e9=bless({$B1,$c9,$Z,$d9},$V);$f9=q'quote_object';$g9=[];$h9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$i9=bless({$B1,$g9,$Z,$h9},$V);$j9=q'quote_scalar';$k9=[];$l9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$m9=bless({$B1,$k9,$Z,$l9},$V);$n9=q'quote_value';$o9=[];$p9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$q9=bless({$B1,$o9,$Z,$p9},$V);$r9=q'reconstruction';$s9=[];$t9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$u9=bless({$B1,$s9,$Z,$t9},$V);$v9=q'side_effect';$w9=[];$x9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$y9=bless({$B1,$w9,$Z,$x9},$V);$z9=q'write';$A9=[];$B9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$C9=bless({$B1,$A9,$Z,$B9},$V);$D9={$h8,$k8,$l8,$o8,$p8,$s8,$t8,$w8,$x8,$A8,$B8,$E8,$F8,$I8,$J8,$M8,$N8,$Q8,$R8,$U8,$V8,$Y8,$Z8,$e9,$f9,$i9,$j9,$m9,$n9,$q9,$r9,$u9,$v9,$y9,$z9,$C9};$E9=q'/lib/image_quoting.b';$F9=bless({$c,$g8,$M,$N,$O,$N,$P,$D9,$f,$E9},$C);$G9=q'/lib/slice::ctors';$H9=[$e8,$F9];$I9=bless({$c,$V7,$f,$U7,$g,$H9},$n);$J9=q'/lib/image.c::ctors';$K9=q'ni:/lib/image.c';$L9=q'ni:/lib/image_init.b';$M9=q'ni:/lib/image_quoting.b';$N9=q'ni:/lib/instance.b';$O9=q'ni:/lib/instantiable.b';$P9=q'ni:/lib/named.b';$Q9=q'ni:/lib/named_in_ni.b';$R9=q'ni:/lib/namespaced.b';$S9=q'ni:/lib/ni';$T9=q'ni:/lib/ni.c';$U9=q'ni:/lib/ni_image.b';$V9=q'ni:/lib/ni_main.b';$W9=q'ni:/lib/ni_self.b';$X9=q'ni:/lib/perlbranch.b';$Y9=q'ni:/lib/resolver.b';$Z9=q'ni:/lib/slice';$ca=q'ni:/lib/slice.b';$da=q'ni:/lib/slice.c';$ea=q'ni:/lib/slice_init.b';$fa=q'ni:/lib/slice_serialize.b';$ga=q'ni:/lib/subclass.b';$ha=q'ni:/lib/tag';$ia=q'ni:/lib/tag.b';$ja=q'ni:/lib/tag.c';$ka=q'ni:/lib/tag_init.b';$la=q'ni:/metaclass';$ma=q'ni:/metaclass.c';$na=q'ni:/object';$oa=q'ni:/object.c';$pa={$O6,$t7,$q7,$y5,$v7,$e3,$w7,$k5,$x7,$W1,$y7,$x,$z7,$N4,$A7,$M3,$B7,$C4,$C7,$K4,$D7,$y4,$E7,$X4,$F7,$n5,$G7,$n7,$H7,$R6,$I7,$c7,$J7,$k7,$K7,$W2,$L7,$K1,$M7,$T,$N7,$x1,$O7,$H1,$P7,$I9,$K9,$S7,$L9,$e8,$M9,$F9,$N9,$Q1,$O9,$k1,$P9,$h2,$Q9,$U3,$R9,$e4,$S9,$L6,$T9,$J5,$U9,$n6,$V9,$I6,$W9,$Y5,$X9,$p4,$Y9,$m4,$Z9,$N2,$ca,$v2,$da,$A,$ea,$C2,$fa,$K2,$ga,$v5,$ha,$B3,$ia,$r3,$ja,$j3,$ka,$y3,$la,$F5,$ma,$B5,$na,$T1,$oa,$Z2};$qa=bless({$N6,$pa},$L5);$ra=q'/lib/ni::ctors';$$Y2[0]=$y5;$$v[0]=$Z2;$$S[0]=$Z2;$$M4[4]=$n5;*$p2=\&$n2;*$o2=\&$l2;$k1->apply_unsafe($I);$k1->apply_unsafe($h);$k1->apply_unsafe($j);$k1->apply_unsafe($F);$k1->apply_unsafe($k);$k1->apply_unsafe($l);$k1->apply_unsafe($V);$k1->apply_unsafe($m);$k1->apply_unsafe($n);$k1->apply_unsafe($o);$k1->apply_unsafe($C);$k1->apply_unsafe($p);$k1->apply_unsafe($G);$k1->apply_unsafe($q);$k1->apply_unsafe($w);$k1->apply_unsafe($d);$k1->apply_unsafe($r);$x1->apply_unsafe($V);$H1->apply_unsafe($V);$Q1->apply_unsafe($I);$Q1->apply_unsafe($h);$Q1->apply_unsafe($E);$Q1->apply_unsafe($j);$Q1->apply_unsafe($F);$Q1->apply_unsafe($k);$Q1->apply_unsafe($l);$Q1->apply_unsafe($m);$Q1->apply_unsafe($n);$Q1->apply_unsafe($o);$Q1->apply_unsafe($C);$Q1->apply_unsafe($p);$Q1->apply_unsafe($G);$Q1->apply_unsafe($q);$Q1->apply_unsafe($w);$Q1->apply_unsafe($d);$Q1->apply_unsafe($J);$Q1->apply_unsafe($r);$h2->apply_unsafe($I);$h2->apply_unsafe($h);$h2->apply_unsafe($j);$h2->apply_unsafe($F);$h2->apply_unsafe($k);$h2->apply_unsafe($i2);$h2->apply_unsafe($l);$h2->apply_unsafe($m);$h2->apply_unsafe($n);$h2->apply_unsafe($o);$h2->apply_unsafe($C);$h2->apply_unsafe($p);$h2->apply_unsafe($G);$h2->apply_unsafe($q);$h2->apply_unsafe($w);$h2->apply_unsafe($d);$h2->apply_unsafe($r);$v2->apply_unsafe($C);$C2->apply_unsafe($C);$K2->apply_unsafe($C);$W2->apply_unsafe($h);$W2->apply_unsafe($j);$W2->apply_unsafe($k);$W2->apply_unsafe($l);$W2->apply_unsafe($m);$W2->apply_unsafe($n);$W2->apply_unsafe($o);$W2->apply_unsafe($p);$W2->apply_unsafe($q);$W2->apply_unsafe($r);$r3->apply_unsafe($G);$y3->apply_unsafe($G);$M3->apply_unsafe($I);$M3->apply_unsafe($h);$M3->apply_unsafe($j);$M3->apply_unsafe($F);$M3->apply_unsafe($k);$M3->apply_unsafe($l);$M3->apply_unsafe($m);$M3->apply_unsafe($n);$M3->apply_unsafe($o);$M3->apply_unsafe($p);$M3->apply_unsafe($q);$M3->apply_unsafe($w);$M3->apply_unsafe($d);$M3->apply_unsafe($r);$U3->apply_unsafe($I);$U3->apply_unsafe($h);$U3->apply_unsafe($j);$U3->apply_unsafe($F);$U3->apply_unsafe($k);$U3->apply_unsafe($l);$U3->apply_unsafe($m);$U3->apply_unsafe($n);$U3->apply_unsafe($o);$U3->apply_unsafe($C);$U3->apply_unsafe($p);$U3->apply_unsafe($G);$U3->apply_unsafe($q);$U3->apply_unsafe($w);$U3->apply_unsafe($d);$U3->apply_unsafe($r);$e4->apply_unsafe($I);$e4->apply_unsafe($h);$e4->apply_unsafe($j);$e4->apply_unsafe($F);$e4->apply_unsafe($k);$e4->apply_unsafe($l);$e4->apply_unsafe($m);$e4->apply_unsafe($n);$e4->apply_unsafe($o);$e4->apply_unsafe($C);$e4->apply_unsafe($p);$e4->apply_unsafe($G);$e4->apply_unsafe($q);$e4->apply_unsafe($w);$e4->apply_unsafe($d);$e4->apply_unsafe($r);$m4->apply_unsafe($I);$m4->apply_unsafe($h);$m4->apply_unsafe($j);$m4->apply_unsafe($F);$m4->apply_unsafe($k);$m4->apply_unsafe($l);$m4->apply_unsafe($m);$m4->apply_unsafe($n);$m4->apply_unsafe($o);$m4->apply_unsafe($p);$m4->apply_unsafe($G);$m4->apply_unsafe($q);$m4->apply_unsafe($w);$m4->apply_unsafe($d);$m4->apply_unsafe($r);$y4->apply_unsafe($I);$y4->apply_unsafe($h);$y4->apply_unsafe($j);$y4->apply_unsafe($k);$y4->apply_unsafe($l);$y4->apply_unsafe($m);$y4->apply_unsafe($n);$y4->apply_unsafe($o);$y4->apply_unsafe($p);$y4->apply_unsafe($q);$y4->apply_unsafe($w);$y4->apply_unsafe($d);$y4->apply_unsafe($r);$K4->apply_unsafe($F);$X4->apply_unsafe($I);$X4->apply_unsafe($h);$X4->apply_unsafe($j);$X4->apply_unsafe($F);$X4->apply_unsafe($k);$X4->apply_unsafe($l);$X4->apply_unsafe($m);$X4->apply_unsafe($n);$X4->apply_unsafe($o);$X4->apply_unsafe($p);$X4->apply_unsafe($q);$X4->apply_unsafe($w);$X4->apply_unsafe($d);$X4->apply_unsafe($r);$k5->apply_unsafe($I);$k5->apply_unsafe($h);$k5->apply_unsafe($j);$k5->apply_unsafe($F);$k5->apply_unsafe($k);$k5->apply_unsafe($l);$k5->apply_unsafe($m);$k5->apply_unsafe($n);$k5->apply_unsafe($o);$k5->apply_unsafe($p);$k5->apply_unsafe($q);$k5->apply_unsafe($w);$k5->apply_unsafe($d);$k5->apply_unsafe($r);$v5->apply_unsafe($I);$v5->apply_unsafe($h);$v5->apply_unsafe($j);$v5->apply_unsafe($k);$v5->apply_unsafe($l);$v5->apply_unsafe($m);$v5->apply_unsafe($n);$v5->apply_unsafe($o);$v5->apply_unsafe($p);$v5->apply_unsafe($q);$v5->apply_unsafe($d);$v5->apply_unsafe($r);$Y5->apply_unsafe($L5);$n6->apply_unsafe($L5);$I6->apply_unsafe($L5);$c7->apply_unsafe($i2);$k7->apply_unsafe($i2);$e8->apply_unsafe($U7);$F9->apply_unsafe($U7);$ni::self=$qa;&$_($x)for@$y;&$_($A)for@$B;&$_($T)for@$U;&$_($d1)for@$e1;&$_($h1)for@$e1;&$_($k1)for@$l1;&$_($o1)for@$e1;&$_($r1)for@$e1;&$_($u1)for@$e1;&$_($x1)for@$y1;&$_($E1)for@$e1;&$_($H1)for@$I1;&$_($K1)for@$L1;&$_($N1)for@$e1;&$_($Q1)for@$R1;&$_($T1)for@$U1;&$_($W1)for@$X1;&$_($c2)for@$e1;&$_($e2)for@$e1;&$_($h2)for@$j2;&$_($l2)for@$e1;&$_($n2)for@$e1;&$_($v2)for@$w2;&$_($z2)for@$e1;&$_($C2)for@$D2;&$_($H2)for@$e1;&$_($K2)for@$L2;&$_($N2)for@$O2;&$_($T2)for@$e1;&$_($W2)for@$X2;&$_($Z2)for@$c3;&$_($e3)for@$f3;&$_($j3)for@$k3;&$_($o3)for@$e1;&$_($r3)for@$s3;&$_($v3)for@$e1;&$_($y3)for@$z3;&$_($B3)for@$C3;&$_($H3)for@$e1;&$_($J3)for@$e1;&$_($M3)for@$N3;&$_($R3)for@$e1;&$_($U3)for@$V3;&$_($Z3)for@$e1;&$_($e4)for@$f4;&$_($j4)for@$e1;&$_($m4)for@$n4;&$_($p4)for@$q4;&$_($t4)for@$e1;&$_($v4)for@$e1;&$_($y4)for@$z4;&$_($C4)for@$D4;&$_($H4)for@$e1;&$_($K4)for@$L4;&$_($N4)for@$O4;&$_($U4)for@$e1;&$_($X4)for@$Y4;&$_($e5)for@$e1;&$_($h5)for@$e1;&$_($k5)for@$l5;&$_($n5)for@$o5;&$_($s5)for@$e1;&$_($v5)for@$w5;&$_($y5)for@$z5;&$_($B5)for@$C5;&$_($F5)for@$G5;&$_($J5)for@$K5;&$_($R5)for@$e1;&$_($V5)for@$e1;&$_($Y5)for@$Z5;&$_($g6)for@$e1;&$_($k6)for@$e1;&$_($n6)for@$o6;&$_($t6)for@$e1;&$_($x6)for@$e1;&$_($B6)for@$e1;&$_($F6)for@$e1;&$_($I6)for@$J6;&$_($L6)for@$M6;&$_($R6)for@$S6;&$_($X6)for@$e1;&$_($c7)for@$d7;&$_($h7)for@$e1;&$_($k7)for@$l7;&$_($n7)for@$o7;&$_($t7)for@$u7;&$_($S7)for@$T7;&$_($Z7)for@$e1;&$_($e8)for@$f8;&$_($k8)for@$e1;&$_($o8)for@$e1;&$_($s8)for@$e1;&$_($w8)for@$e1;&$_($A8)for@$e1;&$_($E8)for@$e1;&$_($I8)for@$e1;&$_($M8)for@$e1;&$_($Q8)for@$e1;&$_($U8)for@$e1;&$_($Y8)for@$e1;&$_($e9)for@$e1;&$_($i9)for@$e1;&$_($m9)for@$e1;&$_($q9)for@$e1;&$_($u9)for@$e1;&$_($y9)for@$e1;&$_($C9)for@$e1;&$_($F9)for@$G9;&$_($I9)for@$J9;&$_($qa)for@$ra;ni->run(@ARGV);
__DATA__
