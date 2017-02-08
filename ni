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
$ni::self = bless {}, '/lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::named{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::name {my %h = @_; @ni::named{keys %h} = values %h}
sub ni::eval {eval shift}
*{'/lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  *$_ = $ni::named{"ni.def:$_"} = $kvs{$_} for keys %kvs;
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
$c=q'ni.doc:/class';$d=q'applied_to';$e=q'/metaclass.c';$f={$e,1};$g=q'name';$h=q'slices';$i=q'/class.c';$j={$i,1};$k=q'/lib/behavior.c';$l=q'/lib/branch.c';$m=q'/lib/doc.c';$n=q'/lib/fn.c';$o=q'/lib/image.c';$p=q'/lib/ni.c';$q=q'/lib/slice.c';$r=q'/lib/tag.c';$s=q'/object.c';$t={$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1};$u={$q,1};$v={$k,1,$l,1,$q,1,$r,1};$w=[undef];$x=q'/metaclass';$y=bless({$d,$v,$g,$k,$h,$w},$x);$z=q'/metaclass::ctors';$A=[$y];$B=bless({$d,$u,$g,$q,$h,$A},$x);$C=q'/metaclass::ctors';$D=q'/lib/slice';$E={$D,1};$F=q'/lib/behavior';$G=q'/lib/branch';$H=q'/lib/tag';$I={$F,1,$G,1,$D,1,$H,1};$J=q'/class';$K=q'/object';$L={$J,1,$i,1,$F,1,$k,1,$G,1,$l,1,$m,1,$n,1,$o,1,$p,1,$D,1,$q,1,$H,1,$r,1,$x,1,$e,1,$K,1,$s,1};$M={};$N=q'ctor';$O=undef;$P=q'dtor';$Q=q'methods';$R=q'class';$S={$n,1};$T=[undef];$U=bless({$d,$S,$g,$n,$h,$T},$x);$V=q'/metaclass::ctors';$W=q'/lib/fn';$X={$W,1};$Y={};$Z=q'DESTROY';$c1=q'code';$d1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$e1=bless({$c1,$d1},$W);$f1=q'/lib/fn::ctors';$g1=q'new';$h1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$i1=bless({$c1,$h1},$W);$j1={$Z,$e1,$g1,$i1};$k1=q'/lib/instantiable.b';$l1=bless({$d,$Y,$Q,$j1,$g,$k1},$D);$m1=q'/lib/slice::ctors';$n1={};$o1=q'shift->compile';$p1=bless({$c1,$o1},$W);$q1=q'compile';$r1=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
++$eval_n;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$s1=bless({$c1,$r1},$W);$t1=q'instantiate';$u1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$v1=bless({$c1,$u1},$W);$w1={$q1,$s1,$t1,$v1};$x1=q'/lib/fn_init.b';$y1=bless({$d,$n1,$N,$p1,$P,$O,$Q,$w1,$g,$x1},$D);$z1=q'/lib/slice::ctors';$A1={};$B1=q'serialize';$C1=q'annotations';$D1=[];$E1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$F1=bless({$C1,$D1,$c1,$E1},$W);$G1={$B1,$F1};$H1=q'/lib/fn_serialize.b';$I1=bless({$d,$A1,$N,$O,$P,$O,$Q,$G1,$g,$H1},$D);$J1=q'/lib/slice::ctors';$K1=[$l1,$y1,$I1];$L1=bless({$d,$X,$g,$W,$h,$K1},$n);$M1=q'/lib/fn.c::ctors';$N1=q'ni \'ni:\' . ref shift';$O1=bless({$c1,$N1},$W);$P1={$R,$O1};$Q1=q'/lib/instance.b';$R1=bless({$d,$M,$N,$O,$P,$O,$Q,$P1,$g,$Q1},$D);$S1=q'/lib/slice::ctors';$T1=[$R1];$U1=bless({$d,$L,$g,$K,$h,$T1},$s);$V1=q'/object.c::ctors';$W1=[$U1];$X1=bless({$d,$I,$g,$F,$h,$W1},$k);$Y1=q'/lib/behavior.c::ctors';$Z1={};$c2=q'my $s = shift; ni::name($s->name, $s)';$d2=bless({$c1,$c2},$W);$e2=q'$_[0]->namespace . ":" . $_[0]->{name}';$f2=bless({$c1,$e2},$W);$g2={$g,$f2};$h2=q'/lib/named.b';$i2=bless({$d,$Z1,$N,$d2,$P,$O,$Q,$g2,$g,$h2},$D);$j2=q'/lib/doc';$k2=q'/lib/slice::ctors';$l2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$m2=bless({$c1,$l2},$W);$n2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$o2=bless({$c1,$n2},$W);$p2=q'/lib/slice::apply';$q2=q'/lib/slice::apply_unsafe';$r2={};$s2=q'apply';$t2=q'apply_unsafe';$u2={$s2,$m2,$t2,$o2};$v2=q'/lib/slice.b';$w2=bless({$d,$r2,$Q,$u2,$g,$v2},$D);$x2=q'/lib/slice::ctors';$y2={};$z2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$A2=bless({$c1,$z2},$W);$B2={$t1,$A2};$C2=q'/lib/slice_init.b';$D2=bless({$d,$y2,$Q,$B2,$g,$C2},$D);$E2=q'/lib/slice::ctors';$F2={};$G2=[];$H2=q'local $_;
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
$g;';$I2=bless({$C1,$G2,$c1,$H2},$W);$J2={$B1,$I2};$K2=q'/lib/slice_serialize.b';$L2=bless({$d,$F2,$N,$O,$P,$O,$Q,$J2,$g,$K2},$D);$M2=q'/lib/slice::ctors';$N2=[$X1,$i2,$w2,$D2,$L2];$O2=bless({$d,$E,$g,$D,$h,$N2},$q);$P2=q'/lib/slice.c::ctors';$Q2={};$R2=q'doc';$S2=[];$T2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$U2=bless({$C1,$S2,$c1,$T2},$W);$V2={$R2,$U2};$W2=q'/lib/documentable.b';$X2=bless({$d,$Q2,$N,$O,$P,$O,$Q,$V2,$g,$W2},$D);$Y2=q'/lib/slice::ctors';$Z2=[undef,$X2];$c3=bless({$d,$t,$g,$s,$h,$Z2},$x);$d3=q'/metaclass::ctors';$e3=[$c3];$f3=bless({$d,$j,$g,$i,$h,$e3},$x);$g3=q'/metaclass::ctors';$h3={$J,1,$i,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$e,1,$s,1};$i3={$r,1};$j3=[$y];$k3=bless({$d,$i3,$g,$r,$h,$j3},$x);$l3=q'/metaclass::ctors';$m3={$H,1};$n3={};$o3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$p3=bless({$c1,$o3},$W);$q3={$s2,$p3};$r3=q'/lib/tag.b';$s3=bless({$d,$n3,$N,$O,$P,$O,$Q,$q3,$g,$r3},$D);$t3=q'/lib/slice::ctors';$u3={};$v3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$w3=bless({$c1,$v3},$W);$x3={$t1,$w3};$y3=q'/lib/tag_init.b';$z3=bless({$d,$u3,$N,$O,$P,$O,$Q,$x3,$g,$y3},$D);$A3=q'/lib/slice::ctors';$B3=[$X1,$i2,$s3,$z3];$C3=bless({$d,$m3,$g,$H,$h,$B3},$r);$D3=q'/lib/tag.c::ctors';$E3=q'/lib/perlbranch.b';$F3={};$G3=q'add';$H3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$I3=bless({$c1,$H3},$W);$J3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$K3=bless({$c1,$J3},$W);$L3={$G3,$I3,$s2,$K3};$M3=q'/lib/branch.b';$N3=bless({$d,$F3,$N,$O,$P,$O,$Q,$L3,$g,$M3},$D);$O3=q'/lib/slice::ctors';$P3={};$Q3=q'namespace';$R3=q'\'ni\'';$S3=bless({$c1,$R3},$W);$T3={$Q3,$S3};$U3=q'/lib/named_in_ni.b';$V3=bless({$d,$P3,$N,$O,$P,$O,$Q,$T3,$g,$U3},$D);$W3=q'/lib/slice::ctors';$X3={};$Y3=q'package';$Z3=q'shift->{name}';$c4=bless({$c1,$Z3},$W);$d4={$Y3,$c4};$e4=q'/lib/namespaced.b';$f4=bless({$d,$X3,$N,$O,$P,$O,$Q,$d4,$g,$e4},$D);$g4=q'/lib/slice::ctors';$h4={};$i4=q'resolve';$j4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$k4=bless({$c1,$j4},$W);$l4={$i4,$k4};$m4=q'/lib/resolver.b';$n4=bless({$d,$h4,$N,$O,$P,$O,$Q,$l4,$g,$m4},$D);$o4=q'/lib/slice::ctors';$p4=[$N3,$l1,$i2,$V3,$f4,$n4];$q4=bless({$g,$E3,$h,$p4},$H);$r4=q'/lib/tag::ctors';$s4={};$t4=q'my $s = shift; $s->apply($s->package)';$u4=bless({$c1,$t4},$W);$v4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$w4=bless({$c1,$v4},$W);$x4={$t1,$w4};$y4=q'/lib/class_init.b';$z4=bless({$d,$s4,$N,$u4,$P,$O,$Q,$x4,$g,$y4},$D);$A4=q'/lib/slice::ctors';$B4={$l,1};$C4=[$y];$D4=bless({$d,$B4,$g,$l,$h,$C4},$x);$E4=q'/metaclass::ctors';$F4={$G,1};$G4={};$H4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$I4=bless({$c1,$H4},$W);$J4={$t1,$I4};$K4=q'/lib/branch_init.b';$L4=bless({$d,$G4,$N,$O,$P,$O,$Q,$J4,$g,$K4},$D);$M4=q'/lib/slice::ctors';$N4=[$X1,$i2,$N3,$L4,undef];$O4=bless({$d,$F4,$g,$G,$h,$N4},$l);$P4=q'/lib/branch.c::ctors';$Q4={$J,1,$i,1,$k,1,$G,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$x,1,$e,1,$s,1};$R4=q'/lib/definition.b';$S4={};$T4=q'def';$U4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$V4=bless({$c1,$U4},$W);$W4={$T4,$V4};$X4=q'/lib/classdef.b';$Y4=bless({$d,$S4,$N,$O,$P,$O,$Q,$W4,$g,$X4},$D);$Z4=q'/lib/slice::ctors';$c5={};$d5=q'ro';$e5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$f5=bless({$c1,$e5},$W);$g5=q'rw';$h5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$i5=bless({$c1,$h5},$W);$j5={$d5,$f5,$g5,$i5};$k5=q'/lib/accessor.b';$l5=bless({$d,$c5,$N,$O,$P,$O,$Q,$j5,$g,$k5},$D);$m5=q'/lib/slice::ctors';$n5=[$Y4,$l5];$o5=bless({$d,$Q4,$g,$R4,$h,$n5},$G);$p5=q'/lib/branch::ctors';$q5={};$r5=q'child';$s5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$t5=bless({$c1,$s5},$W);$u5={$r5,$t5};$v5=q'/lib/subclass.b';$w5=bless({$d,$q5,$N,$O,$P,$O,$Q,$u5,$g,$v5},$D);$x5=q'/lib/slice::ctors';$y5=[$q4,$z4,$U1,$o5,$w5];$z5=bless({$d,$h3,$g,$J,$h,$y5},$i);$A5=q'/class.c::ctors';$B5=[$z5];$C5=bless({$d,$f,$g,$e,$h,$B5},$x);$D5=q'/metaclass::ctors';$E5={$x,1};$F5=[$q4,$z4,$U1,$o5];$G5=bless({$d,$E5,$g,$x,$h,$F5},$e);$H5=q'/metaclass.c::ctors';$I5={$m,1};$J5=[$c3];$K5=bless({$d,$I5,$g,$m,$h,$J5},$x);$L5=q'/metaclass::ctors';$M5={$j2,1};$N5={};$O5=[];$P5=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$Q5=bless({$C1,$O5,$c1,$P5},$W);$R5={$t1,$Q5};$S5=q'/lib/doc_init.b';$T5=bless({$d,$N5,$N,$O,$P,$O,$Q,$R5,$g,$S5},$D);$U5=q'/lib/slice::ctors';$V5={};$W5=[];$X5=q'\'ni.doc\'';$Y5=bless({$C1,$W5,$c1,$X5},$W);$Z5={$Q3,$Y5};$c6=q'/lib/doc_namespace.b';$d6=bless({$d,$V5,$N,$O,$P,$O,$Q,$Z5,$g,$c6},$D);$e6=q'/lib/slice::ctors';$f6=[$i2,$T5,$d6];$g6=bless({$d,$M5,$g,$j2,$h,$f6},$m);$h6=q'/lib/doc.c::ctors';$i6=q'apropos';$j6=q'ni:/class';$k6=[$j6];$l6=q'# Classes and metaclasses
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
';$m6=bless({$i6,$k6,$R2,$l6,$g,$J},$j2);$n6=q'/lib/doc::ctors';$o6=q'ni:/class.c';$p6=q'ni:/lib/accessor.b';$q6=q'ni:/lib/behavior';$r6=q'ni:/lib/behavior.c';$s6=q'ni:/lib/branch';$t6=q'ni:/lib/branch.b';$u6=q'ni:/lib/branch.c';$v6=q'ni:/lib/branch_init.b';$w6=q'ni:/lib/class_init.b';$x6=q'ni:/lib/classdef.b';$y6=q'ni:/lib/definition.b';$z6=q'ni:/lib/doc';$A6=q'ni:/lib/doc.c';$B6=q'ni:/lib/doc_init.b';$C6=q'ni:/lib/doc_namespace.b';$D6=q'ni:/lib/documentable.b';$E6=q'ni:/lib/fn';$F6=q'ni:/lib/fn.c';$G6=q'ni:/lib/fn_init.b';$H6=q'ni:/lib/fn_serialize.b';$I6=q'ni:/lib/image';$J6={$o,1};$K6=[$c3];$L6=bless({$d,$J6,$g,$o,$h,$K6},$x);$M6=q'/metaclass::ctors';$N6=q'/lib/image';$O6={$N6,1};$P6={};$Q6=[];$R6=q'my $class = shift;
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
  ordering     => []};';$S6=bless({$C1,$Q6,$c1,$R6},$W);$T6={$t1,$S6};$U6=q'/lib/image_init.b';$V6=bless({$d,$P6,$N,$O,$P,$O,$Q,$T6,$g,$U6},$D);$W6=q'/lib/slice::ctors';$X6={};$Y6=q'address';$Z6=[];$c7=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$d7=bless({$C1,$Z6,$c1,$c7},$W);$e7=q'allocate_gensym';$f7=[];$g7=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$h7=bless({$C1,$f7,$c1,$g7},$W);$i7=q'boot_side_effect';$j7=[];$k7=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$l7=bless({$C1,$j7,$c1,$k7},$W);$m7=q'circular_links';$n7=[];$o7=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$p7=bless({$C1,$n7,$c1,$o7},$W);$q7=q'finalizer';$r7=[];$s7=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$t7=bless({$C1,$r7,$c1,$s7},$W);$u7=q'gensym';$v7=[];$w7=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$x7=bless({$C1,$v7,$c1,$w7},$W);$y7=q'is_circular';$z7=[];$A7=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$B7=bless({$C1,$z7,$c1,$A7},$W);$C7=q'partial_image';$D7=[];$E7=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\@ni::named{keys\\%$g}=values\\%$g;");';$F7=bless({$C1,$D7,$c1,$E7},$W);$G7=q'quote';$H7=[];$I7=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$J7=bless({$C1,$H7,$c1,$I7},$W);$K7=q'quote_array';$L7=[];$M7=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$N7=bless({$C1,$L7,$c1,$M7},$W);$O7=q'quote_blessed';$P7=[];$Q7=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$R7=bless({$C1,$P7,$c1,$Q7},$W);$S7=q'quote_class';$T7=[];$U7=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && exists $ni::named{"ni:$class"};';$V7=bless({$C1,$T7,$c1,$U7},$W);$W7=q'quote_hash';$X7=[];$Y7=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$Z7=bless({$C1,$X7,$c1,$Y7},$W);$c8=q'quote_object';$d8=[];$e8=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$f8=bless({$C1,$d8,$c1,$e8},$W);$g8=q'quote_scalar';$h8=[];$i8=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$j8=bless({$C1,$h8,$c1,$i8},$W);$k8=q'quote_value';$l8=[];$m8=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$n8=bless({$C1,$l8,$c1,$m8},$W);$o8=q'reconstruction';$p8=[];$q8=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$r8=bless({$C1,$p8,$c1,$q8},$W);$s8=q'side_effect';$t8=[];$u8=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$v8=bless({$C1,$t8,$c1,$u8},$W);$w8=q'write';$x8=[];$y8=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$z8=bless({$C1,$x8,$c1,$y8},$W);$A8={$Y6,$d7,$e7,$h7,$i7,$l7,$m7,$p7,$q7,$t7,$u7,$x7,$y7,$B7,$C7,$F7,$G7,$J7,$K7,$N7,$O7,$R7,$S7,$V7,$W7,$Z7,$c8,$f8,$g8,$j8,$k8,$n8,$o8,$r8,$s8,$v8,$w8,$z8};$B8=q'/lib/image_quoting.b';$C8=bless({$d,$X6,$N,$O,$P,$O,$Q,$A8,$g,$B8},$D);$D8=q'/lib/slice::ctors';$E8=[$V6,$C8];$F8=bless({$d,$O6,$g,$N6,$h,$E8},$o);$G8=q'/lib/image.c::ctors';$H8=q'ni:/lib/image.c';$I8=q'ni:/lib/image_init.b';$J8=q'ni:/lib/image_quoting.b';$K8=q'ni:/lib/instance.b';$L8=q'ni:/lib/instantiable.b';$M8=q'ni:/lib/named.b';$N8=q'ni:/lib/named_in_ni.b';$O8=q'ni:/lib/namespaced.b';$P8=q'ni:/lib/ni';$Q8={$p,1};$R8=[$c3];$S8=bless({$d,$Q8,$g,$p,$h,$R8},$x);$T8=q'/metaclass::ctors';$U8=q'/lib/ni';$V8={$U8,1};$W8={};$X8=q'is_mutable';$Y8=[];$Z8=q'$0 ne "-" && -w $0';$c9=bless({$C1,$Y8,$c1,$Z8},$W);$d9=q'modify';$e9=[];$f9=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$g9=bless({$C1,$e9,$c1,$f9},$W);$h9={$X8,$c9,$d9,$g9};$i9=q'/lib/ni_self.b';$j9=bless({$d,$W8,$N,$O,$P,$O,$Q,$h9,$g,$i9},$D);$k9=q'/lib/slice::ctors';$l9={};$m9=q'quoted';$n9=[];$o9=q'my $self = shift;
ni(\'ni:/lib/image\')->new(@_)->partial_image(grep !/^ni\\.eval:/, keys %ni::named);';$p9=bless({$C1,$n9,$c1,$o9},$W);$q9={$m9,$p9};$r9=q'/lib/ni_image.b';$s9=bless({$d,$l9,$N,$O,$P,$O,$Q,$q9,$g,$r9},$D);$t9=q'/lib/slice::ctors';$u9={};$v9=q'internal/+=';$w9=[];$x9=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$y9=bless({$C1,$w9,$c1,$x9},$W);$z9=q'internal/eval';$A9=[];$B9=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$C9=bless({$C1,$A9,$c1,$B9},$W);$D9=q'internal/image';$E9=[];$F9=q'shift->quoted->write(\\*STDOUT);
0;';$G9=bless({$C1,$E9,$c1,$F9},$W);$H9=q'run';$I9=[];$J9=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';$K9=bless({$C1,$I9,$c1,$J9},$W);$L9={$v9,$y9,$z9,$C9,$D9,$G9,$H9,$K9};$M9=q'/lib/ni_main.b';$N9=bless({$d,$u9,$N,$O,$P,$O,$Q,$L9,$g,$M9},$D);$O9=q'/lib/slice::ctors';$P9=[$j9,$s9,$N9];$Q9=bless({$d,$V8,$g,$U8,$h,$P9},$p);$R9=q'/lib/ni.c::ctors';$S9=q'ni:/lib/ni.c';$T9=q'ni:/lib/ni_image.b';$U9=q'ni:/lib/ni_main.b';$V9=q'ni:/lib/ni_self.b';$W9=q'ni:/lib/perlbranch.b';$X9=q'ni:/lib/resolver.b';$Y9=q'ni:/lib/slice';$Z9=q'ni:/lib/slice.b';$ca=q'ni:/lib/slice.c';$da=q'ni:/lib/slice_init.b';$ea=q'ni:/lib/slice_serialize.b';$fa=q'ni:/lib/subclass.b';$ga=q'ni:/lib/tag';$ha=q'ni:/lib/tag.b';$ia=q'ni:/lib/tag.c';$ja=q'ni:/lib/tag_init.b';$ka=q'ni:/metaclass';$la=q'ni:/metaclass.c';$ma=q'ni:/object';$na=q'ni:/object.c';$oa={$c,$m6,$j6,$z5,$o6,$f3,$p6,$l5,$q6,$X1,$r6,$y,$s6,$O4,$t6,$N3,$u6,$D4,$v6,$L4,$w6,$z4,$x6,$Y4,$y6,$o5,$z6,$g6,$A6,$K5,$B6,$T5,$C6,$d6,$D6,$X2,$E6,$L1,$F6,$U,$G6,$y1,$H6,$I1,$I6,$F8,$H8,$L6,$I8,$V6,$J8,$C8,$K8,$R1,$L8,$l1,$M8,$i2,$N8,$V3,$O8,$f4,$P8,$Q9,$S9,$S8,$T9,$s9,$U9,$N9,$V9,$j9,$W9,$q4,$X9,$n4,$Y9,$O2,$Z9,$w2,$ca,$B,$da,$D2,$ea,$L2,$fa,$w5,$ga,$C3,$ha,$s3,$ia,$k3,$ja,$z3,$ka,$G5,$la,$C5,$ma,$U1,$na,$c3};$$Z2[0]=$z5;$$w[0]=$c3;$$T[0]=$c3;$$N4[4]=$o5;*$q2=\&$o2;*$p2=\&$m2;$l1->apply_unsafe($J);$l1->apply_unsafe($i);$l1->apply_unsafe($k);$l1->apply_unsafe($G);$l1->apply_unsafe($l);$l1->apply_unsafe($m);$l1->apply_unsafe($W);$l1->apply_unsafe($n);$l1->apply_unsafe($o);$l1->apply_unsafe($p);$l1->apply_unsafe($D);$l1->apply_unsafe($q);$l1->apply_unsafe($H);$l1->apply_unsafe($r);$l1->apply_unsafe($x);$l1->apply_unsafe($e);$l1->apply_unsafe($s);$y1->apply_unsafe($W);$I1->apply_unsafe($W);$R1->apply_unsafe($J);$R1->apply_unsafe($i);$R1->apply_unsafe($F);$R1->apply_unsafe($k);$R1->apply_unsafe($G);$R1->apply_unsafe($l);$R1->apply_unsafe($m);$R1->apply_unsafe($n);$R1->apply_unsafe($o);$R1->apply_unsafe($p);$R1->apply_unsafe($D);$R1->apply_unsafe($q);$R1->apply_unsafe($H);$R1->apply_unsafe($r);$R1->apply_unsafe($x);$R1->apply_unsafe($e);$R1->apply_unsafe($K);$R1->apply_unsafe($s);$i2->apply_unsafe($J);$i2->apply_unsafe($i);$i2->apply_unsafe($k);$i2->apply_unsafe($G);$i2->apply_unsafe($l);$i2->apply_unsafe($j2);$i2->apply_unsafe($m);$i2->apply_unsafe($n);$i2->apply_unsafe($o);$i2->apply_unsafe($p);$i2->apply_unsafe($D);$i2->apply_unsafe($q);$i2->apply_unsafe($H);$i2->apply_unsafe($r);$i2->apply_unsafe($x);$i2->apply_unsafe($e);$i2->apply_unsafe($s);$w2->apply_unsafe($D);$D2->apply_unsafe($D);$L2->apply_unsafe($D);$X2->apply_unsafe($i);$X2->apply_unsafe($k);$X2->apply_unsafe($l);$X2->apply_unsafe($m);$X2->apply_unsafe($n);$X2->apply_unsafe($o);$X2->apply_unsafe($p);$X2->apply_unsafe($q);$X2->apply_unsafe($r);$X2->apply_unsafe($s);$s3->apply_unsafe($H);$z3->apply_unsafe($H);$N3->apply_unsafe($J);$N3->apply_unsafe($i);$N3->apply_unsafe($k);$N3->apply_unsafe($G);$N3->apply_unsafe($l);$N3->apply_unsafe($m);$N3->apply_unsafe($n);$N3->apply_unsafe($o);$N3->apply_unsafe($p);$N3->apply_unsafe($q);$N3->apply_unsafe($r);$N3->apply_unsafe($x);$N3->apply_unsafe($e);$N3->apply_unsafe($s);$V3->apply_unsafe($J);$V3->apply_unsafe($i);$V3->apply_unsafe($k);$V3->apply_unsafe($G);$V3->apply_unsafe($l);$V3->apply_unsafe($m);$V3->apply_unsafe($n);$V3->apply_unsafe($o);$V3->apply_unsafe($p);$V3->apply_unsafe($D);$V3->apply_unsafe($q);$V3->apply_unsafe($H);$V3->apply_unsafe($r);$V3->apply_unsafe($x);$V3->apply_unsafe($e);$V3->apply_unsafe($s);$f4->apply_unsafe($J);$f4->apply_unsafe($i);$f4->apply_unsafe($k);$f4->apply_unsafe($G);$f4->apply_unsafe($l);$f4->apply_unsafe($m);$f4->apply_unsafe($n);$f4->apply_unsafe($o);$f4->apply_unsafe($p);$f4->apply_unsafe($D);$f4->apply_unsafe($q);$f4->apply_unsafe($H);$f4->apply_unsafe($r);$f4->apply_unsafe($x);$f4->apply_unsafe($e);$f4->apply_unsafe($s);$n4->apply_unsafe($J);$n4->apply_unsafe($i);$n4->apply_unsafe($k);$n4->apply_unsafe($G);$n4->apply_unsafe($l);$n4->apply_unsafe($m);$n4->apply_unsafe($n);$n4->apply_unsafe($o);$n4->apply_unsafe($p);$n4->apply_unsafe($q);$n4->apply_unsafe($H);$n4->apply_unsafe($r);$n4->apply_unsafe($x);$n4->apply_unsafe($e);$n4->apply_unsafe($s);$z4->apply_unsafe($J);$z4->apply_unsafe($i);$z4->apply_unsafe($k);$z4->apply_unsafe($l);$z4->apply_unsafe($m);$z4->apply_unsafe($n);$z4->apply_unsafe($o);$z4->apply_unsafe($p);$z4->apply_unsafe($q);$z4->apply_unsafe($r);$z4->apply_unsafe($x);$z4->apply_unsafe($e);$z4->apply_unsafe($s);$L4->apply_unsafe($G);$Y4->apply_unsafe($J);$Y4->apply_unsafe($i);$Y4->apply_unsafe($k);$Y4->apply_unsafe($G);$Y4->apply_unsafe($l);$Y4->apply_unsafe($m);$Y4->apply_unsafe($n);$Y4->apply_unsafe($o);$Y4->apply_unsafe($p);$Y4->apply_unsafe($q);$Y4->apply_unsafe($r);$Y4->apply_unsafe($x);$Y4->apply_unsafe($e);$Y4->apply_unsafe($s);$l5->apply_unsafe($J);$l5->apply_unsafe($i);$l5->apply_unsafe($k);$l5->apply_unsafe($G);$l5->apply_unsafe($l);$l5->apply_unsafe($m);$l5->apply_unsafe($n);$l5->apply_unsafe($o);$l5->apply_unsafe($p);$l5->apply_unsafe($q);$l5->apply_unsafe($r);$l5->apply_unsafe($x);$l5->apply_unsafe($e);$l5->apply_unsafe($s);$w5->apply_unsafe($J);$w5->apply_unsafe($i);$w5->apply_unsafe($k);$w5->apply_unsafe($l);$w5->apply_unsafe($m);$w5->apply_unsafe($n);$w5->apply_unsafe($o);$w5->apply_unsafe($p);$w5->apply_unsafe($q);$w5->apply_unsafe($r);$w5->apply_unsafe($e);$w5->apply_unsafe($s);$T5->apply_unsafe($j2);$d6->apply_unsafe($j2);$V6->apply_unsafe($N6);$C8->apply_unsafe($N6);$j9->apply_unsafe($U8);$s9->apply_unsafe($U8);$N9->apply_unsafe($U8);@ni::named{keys%$oa}=values%$oa;&$_($y)for@$z;&$_($B)for@$C;&$_($U)for@$V;&$_($e1)for@$f1;&$_($i1)for@$f1;&$_($l1)for@$m1;&$_($p1)for@$f1;&$_($s1)for@$f1;&$_($v1)for@$f1;&$_($y1)for@$z1;&$_($F1)for@$f1;&$_($I1)for@$J1;&$_($L1)for@$M1;&$_($O1)for@$f1;&$_($R1)for@$S1;&$_($U1)for@$V1;&$_($X1)for@$Y1;&$_($d2)for@$f1;&$_($f2)for@$f1;&$_($i2)for@$k2;&$_($m2)for@$f1;&$_($o2)for@$f1;&$_($w2)for@$x2;&$_($A2)for@$f1;&$_($D2)for@$E2;&$_($I2)for@$f1;&$_($L2)for@$M2;&$_($O2)for@$P2;&$_($U2)for@$f1;&$_($X2)for@$Y2;&$_($c3)for@$d3;&$_($f3)for@$g3;&$_($k3)for@$l3;&$_($p3)for@$f1;&$_($s3)for@$t3;&$_($w3)for@$f1;&$_($z3)for@$A3;&$_($C3)for@$D3;&$_($I3)for@$f1;&$_($K3)for@$f1;&$_($N3)for@$O3;&$_($S3)for@$f1;&$_($V3)for@$W3;&$_($c4)for@$f1;&$_($f4)for@$g4;&$_($k4)for@$f1;&$_($n4)for@$o4;&$_($q4)for@$r4;&$_($u4)for@$f1;&$_($w4)for@$f1;&$_($z4)for@$A4;&$_($D4)for@$E4;&$_($I4)for@$f1;&$_($L4)for@$M4;&$_($O4)for@$P4;&$_($V4)for@$f1;&$_($Y4)for@$Z4;&$_($f5)for@$f1;&$_($i5)for@$f1;&$_($l5)for@$m5;&$_($o5)for@$p5;&$_($t5)for@$f1;&$_($w5)for@$x5;&$_($z5)for@$A5;&$_($C5)for@$D5;&$_($G5)for@$H5;&$_($K5)for@$L5;&$_($Q5)for@$f1;&$_($T5)for@$U5;&$_($Y5)for@$f1;&$_($d6)for@$e6;&$_($g6)for@$h6;&$_($m6)for@$n6;&$_($L6)for@$M6;&$_($S6)for@$f1;&$_($V6)for@$W6;&$_($d7)for@$f1;&$_($h7)for@$f1;&$_($l7)for@$f1;&$_($p7)for@$f1;&$_($t7)for@$f1;&$_($x7)for@$f1;&$_($B7)for@$f1;&$_($F7)for@$f1;&$_($J7)for@$f1;&$_($N7)for@$f1;&$_($R7)for@$f1;&$_($V7)for@$f1;&$_($Z7)for@$f1;&$_($f8)for@$f1;&$_($j8)for@$f1;&$_($n8)for@$f1;&$_($r8)for@$f1;&$_($v8)for@$f1;&$_($z8)for@$f1;&$_($C8)for@$D8;&$_($F8)for@$G8;&$_($S8)for@$T8;&$_($c9)for@$f1;&$_($g9)for@$f1;&$_($j9)for@$k9;&$_($p9)for@$f1;&$_($s9)for@$t9;&$_($y9)for@$f1;&$_($C9)for@$f1;&$_($G9)for@$f1;&$_($K9)for@$f1;&$_($N9)for@$O9;&$_($Q9)for@$R9;ni->run(@ARGV);
__DATA__
