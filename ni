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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/file.c';$t=q'/unix/pid.c';$u={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1};$v={$p,1};$w={$j,1,$k,1,$p,1,$q,1};$x=[undef];$y=q'/metaclass';$z=bless({$c,$w,$f,$j,$g,$x},$y);$A=q'/metaclass::ctors';$B=[$z];$C=bless({$c,$v,$f,$p,$g,$B},$y);$D=q'/metaclass::ctors';$E=q'/lib/slice';$F={$E,1};$G=q'/lib/behavior';$H=q'/lib/branch';$I=q'/lib/tag';$J={$G,1,$H,1,$E,1,$I,1};$K=q'/class';$L=q'/object';$M={$K,1,$h,1,$G,1,$j,1,$H,1,$k,1,$l,1,$m,1,$n,1,$o,1,$E,1,$p,1,$I,1,$q,1,$y,1,$d,1,$L,1,$r,1,$s,1,$t,1};$N={};$O=q'ctor';$P=undef;$Q=q'dtor';$R=q'methods';$S=q'class';$T={$m,1};$U=[undef];$V=bless({$c,$T,$f,$m,$g,$U},$y);$W=q'/metaclass::ctors';$X=q'/lib/fn';$Y={$X,1};$Z={};$c1=q'DESTROY';$d1=q'code';$e1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$f1=bless({$d1,$e1},$X);$g1=q'/lib/fn::ctors';$h1=q'new';$i1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$j1=bless({$d1,$i1},$X);$k1={$c1,$f1,$h1,$j1};$l1=q'/lib/instantiable.b';$m1=bless({$c,$Z,$R,$k1,$f,$l1},$E);$n1=q'/lib/slice::ctors';$o1={};$p1=q'shift->compile';$q1=bless({$d1,$p1},$X);$r1=q'compile';$s1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$t1=bless({$d1,$s1},$X);$u1=q'instantiate';$v1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$w1=bless({$d1,$v1},$X);$x1={$r1,$t1,$u1,$w1};$y1=q'/lib/fn_init.b';$z1=bless({$c,$o1,$O,$q1,$Q,$P,$R,$x1,$f,$y1},$E);$A1=q'/lib/slice::ctors';$B1={};$C1=q'serialize';$D1=q'annotations';$E1=[];$F1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$G1=bless({$D1,$E1,$d1,$F1},$X);$H1={$C1,$G1};$I1=q'/lib/fn_serialize.b';$J1=bless({$c,$B1,$O,$P,$Q,$P,$R,$H1,$f,$I1},$E);$K1=q'/lib/slice::ctors';$L1=[$m1,$z1,$J1];$M1=bless({$c,$Y,$f,$X,$g,$L1},$m);$N1=q'/lib/fn.c::ctors';$O1=q'ni \'ni:\' . ref shift';$P1=bless({$d1,$O1},$X);$Q1={$S,$P1};$R1=q'/lib/instance.b';$S1=bless({$c,$N,$O,$P,$Q,$P,$R,$Q1,$f,$R1},$E);$T1=q'/lib/slice::ctors';$U1=[$S1];$V1=bless({$c,$M,$f,$L,$g,$U1},$r);$W1=q'/object.c::ctors';$X1=[$V1];$Y1=bless({$c,$J,$f,$G,$g,$X1},$j);$Z1=q'/lib/behavior.c::ctors';$c2={};$d2=q'my $s = shift; ni->def($s->name, $s)';$e2=bless({$d1,$d2},$X);$f2=q'$_[0]->namespace . ":" . $_[0]->{name}';$g2=bless({$d1,$f2},$X);$h2={$f,$g2};$i2=q'/lib/named.b';$j2=bless({$c,$c2,$O,$e2,$Q,$P,$R,$h2,$f,$i2},$E);$k2=q'/lib/doc';$l2=q'/lib/slice::ctors';$m2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$n2=bless({$d1,$m2},$X);$o2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$p2=bless({$d1,$o2},$X);$q2=q'/lib/slice::apply';$r2=q'/lib/slice::apply_unsafe';$s2={};$t2=q'apply';$u2=q'apply_unsafe';$v2={$t2,$n2,$u2,$p2};$w2=q'/lib/slice.b';$x2=bless({$c,$s2,$R,$v2,$f,$w2},$E);$y2=q'/lib/slice::ctors';$z2={};$A2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$B2=bless({$d1,$A2},$X);$C2={$u1,$B2};$D2=q'/lib/slice_init.b';$E2=bless({$c,$z2,$R,$C2,$f,$D2},$E);$F2=q'/lib/slice::ctors';$G2={};$H2=[];$I2=q'local $_;
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
$g;';$J2=bless({$D1,$H2,$d1,$I2},$X);$K2={$C1,$J2};$L2=q'/lib/slice_serialize.b';$M2=bless({$c,$G2,$O,$P,$Q,$P,$R,$K2,$f,$L2},$E);$N2=q'/lib/slice::ctors';$O2=[$Y1,$j2,$x2,$E2,$M2];$P2=bless({$c,$F,$f,$E,$g,$O2},$p);$Q2=q'/lib/slice.c::ctors';$R2={};$S2=q'doc';$T2=[];$U2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$V2=bless({$D1,$T2,$d1,$U2},$X);$W2={$S2,$V2};$X2=q'/lib/documentable.b';$Y2=bless({$c,$R2,$O,$P,$Q,$P,$R,$W2,$f,$X2},$E);$Z2=q'/lib/slice::ctors';$c3=[undef,$Y2];$d3=bless({$c,$u,$f,$r,$g,$c3},$y);$e3=q'/metaclass::ctors';$f3=[$d3];$g3=bless({$c,$i,$f,$h,$g,$f3},$y);$h3=q'/metaclass::ctors';$i3={$K,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1};$j3={$q,1};$k3=[$z];$l3=bless({$c,$j3,$f,$q,$g,$k3},$y);$m3=q'/metaclass::ctors';$n3={$I,1};$o3={};$p3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$q3=bless({$d1,$p3},$X);$r3={$t2,$q3};$s3=q'/lib/tag.b';$t3=bless({$c,$o3,$O,$P,$Q,$P,$R,$r3,$f,$s3},$E);$u3=q'/lib/slice::ctors';$v3={};$w3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$x3=bless({$d1,$w3},$X);$y3={$u1,$x3};$z3=q'/lib/tag_init.b';$A3=bless({$c,$v3,$O,$P,$Q,$P,$R,$y3,$f,$z3},$E);$B3=q'/lib/slice::ctors';$C3=[$Y1,$j2,$t3,$A3];$D3=bless({$c,$n3,$f,$I,$g,$C3},$q);$E3=q'/lib/tag.c::ctors';$F3=q'/lib/perlbranch.b';$G3={};$H3=q'add';$I3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$J3=bless({$d1,$I3},$X);$K3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$L3=bless({$d1,$K3},$X);$M3={$H3,$J3,$t2,$L3};$N3=q'/lib/branch.b';$O3=bless({$c,$G3,$O,$P,$Q,$P,$R,$M3,$f,$N3},$E);$P3=q'/lib/slice::ctors';$Q3={};$R3=q'namespace';$S3=q'\'ni\'';$T3=bless({$d1,$S3},$X);$U3={$R3,$T3};$V3=q'/lib/named_in_ni.b';$W3=bless({$c,$Q3,$O,$P,$Q,$P,$R,$U3,$f,$V3},$E);$X3=q'/lib/slice::ctors';$Y3={};$Z3=q'package';$c4=q'shift->{name}';$d4=bless({$d1,$c4},$X);$e4={$Z3,$d4};$f4=q'/lib/namespaced.b';$g4=bless({$c,$Y3,$O,$P,$Q,$P,$R,$e4,$f,$f4},$E);$h4=q'/lib/slice::ctors';$i4={};$j4=q'resolve';$k4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$l4=bless({$d1,$k4},$X);$m4={$j4,$l4};$n4=q'/lib/resolver.b';$o4=bless({$c,$i4,$O,$P,$Q,$P,$R,$m4,$f,$n4},$E);$p4=q'/lib/slice::ctors';$q4=[$O3,$m1,$j2,$W3,$g4,$o4];$r4=bless({$f,$F3,$g,$q4},$I);$s4=q'/lib/tag::ctors';$t4={};$u4=q'my $s = shift; $s->apply($s->package)';$v4=bless({$d1,$u4},$X);$w4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$x4=bless({$d1,$w4},$X);$y4={$u1,$x4};$z4=q'/lib/class_init.b';$A4=bless({$c,$t4,$O,$v4,$Q,$P,$R,$y4,$f,$z4},$E);$B4=q'/lib/slice::ctors';$C4={$k,1};$D4=[$z];$E4=bless({$c,$C4,$f,$k,$g,$D4},$y);$F4=q'/metaclass::ctors';$G4={$H,1};$H4={};$I4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$J4=bless({$d1,$I4},$X);$K4={$u1,$J4};$L4=q'/lib/branch_init.b';$M4=bless({$c,$H4,$O,$P,$Q,$P,$R,$K4,$f,$L4},$E);$N4=q'/lib/slice::ctors';$O4=[$Y1,$j2,$O3,$M4,undef];$P4=bless({$c,$G4,$f,$H,$g,$O4},$k);$Q4=q'/lib/branch.c::ctors';$R4={$K,1,$h,1,$j,1,$H,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$y,1,$d,1,$r,1,$s,1,$t,1};$S4=q'/lib/definition.b';$T4={};$U4=q'def';$V4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$W4=bless({$d1,$V4},$X);$X4={$U4,$W4};$Y4=q'/lib/classdef.b';$Z4=bless({$c,$T4,$O,$P,$Q,$P,$R,$X4,$f,$Y4},$E);$c5=q'/lib/slice::ctors';$d5={};$e5=q'ro';$f5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$g5=bless({$d1,$f5},$X);$h5=q'rw';$i5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$j5=bless({$d1,$i5},$X);$k5={$e5,$g5,$h5,$j5};$l5=q'/lib/accessor.b';$m5=bless({$c,$d5,$O,$P,$Q,$P,$R,$k5,$f,$l5},$E);$n5=q'/lib/slice::ctors';$o5=[$Z4,$m5];$p5=bless({$c,$R4,$f,$S4,$g,$o5},$H);$q5=q'/lib/branch::ctors';$r5={};$s5=q'child';$t5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$u5=bless({$d1,$t5},$X);$v5={$s5,$u5};$w5=q'/lib/subclass.b';$x5=bless({$c,$r5,$O,$P,$Q,$P,$R,$v5,$f,$w5},$E);$y5=q'/lib/slice::ctors';$z5=[$r4,$A4,$V1,$p5,$x5];$A5=bless({$c,$i3,$f,$K,$g,$z5},$h);$B5=q'/class.c::ctors';$C5=[$A5];$D5=bless({$c,$e,$f,$d,$g,$C5},$y);$E5=q'/metaclass::ctors';$F5={$y,1};$G5=[$r4,$A4,$V1,$p5];$H5=bless({$c,$F5,$f,$y,$g,$G5},$d);$I5=q'/metaclass.c::ctors';$J5={$o,1};$K5=[$d3];$L5=bless({$c,$J5,$f,$o,$g,$K5},$y);$M5=q'/metaclass::ctors';$N5=q'/lib/ni';$O5={$N5,1};$P5={};$Q5=q'is_mutable';$R5=[];$S5=q'$0 ne "-" && -w $0';$T5=bless({$D1,$R5,$d1,$S5},$X);$U5=q'modify';$V5=[];$W5=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$X5=bless({$D1,$V5,$d1,$W5},$X);$Y5={$Q5,$T5,$U5,$X5};$Z5=q'/lib/ni_self.b';$c6=bless({$c,$P5,$O,$P,$Q,$P,$R,$Y5,$f,$Z5},$E);$d6=q'/lib/slice::ctors';$e6={};$f6=q'exists';$g6=[];$h6=q'exists $_[0]->{named}{$_[1]}';$i6=bless({$D1,$g6,$d1,$h6},$X);$j6=q'quoted';$k6=[];$l6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$m6=bless({$D1,$k6,$d1,$l6},$X);$n6={$f6,$i6,$j6,$m6};$o6=q'/lib/ni_image.b';$p6=bless({$c,$e6,$O,$P,$Q,$P,$R,$n6,$f,$o6},$E);$q6=q'/lib/slice::ctors';$r6={};$s6=q'--internal/+=';$t6=[];$u6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$v6=bless({$D1,$t6,$d1,$u6},$X);$w6=q'--internal/eval';$x6=[];$y6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$z6=bless({$D1,$x6,$d1,$y6},$X);$A6=q'--internal/image';$B6=[];$C6=q'shift->quoted->write(\\*STDOUT);
0;';$D6=bless({$D1,$B6,$d1,$C6},$X);$E6=q'run';$F6=[];$G6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$H6=bless({$D1,$F6,$d1,$G6},$X);$I6={$s6,$v6,$w6,$z6,$A6,$D6,$E6,$H6};$J6=q'/lib/ni_main.b';$K6=bless({$c,$r6,$O,$P,$Q,$P,$R,$I6,$f,$J6},$E);$L6=q'/lib/slice::ctors';$M6=[$c6,$p6,$K6];$N6=bless({$c,$O5,$f,$N5,$g,$M6},$o);$O6=q'/lib/ni.c::ctors';$P6=q'named';$Q6=q'ni.doc:/class';$R6={$l,1};$S6=[$d3];$T6=bless({$c,$R6,$f,$l,$g,$S6},$y);$U6=q'/metaclass::ctors';$V6={$k2,1};$W6={};$X6=[];$Y6=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$Z6=bless({$D1,$X6,$d1,$Y6},$X);$c7={$u1,$Z6};$d7=q'/lib/doc_init.b';$e7=bless({$c,$W6,$O,$P,$Q,$P,$R,$c7,$f,$d7},$E);$f7=q'/lib/slice::ctors';$g7={};$h7=[];$i7=q'\'ni.doc\'';$j7=bless({$D1,$h7,$d1,$i7},$X);$k7={$R3,$j7};$l7=q'/lib/doc_namespace.b';$m7=bless({$c,$g7,$O,$P,$Q,$P,$R,$k7,$f,$l7},$E);$n7=q'/lib/slice::ctors';$o7=[$j2,$e7,$m7];$p7=bless({$c,$V6,$f,$k2,$g,$o7},$l);$q7=q'/lib/doc.c::ctors';$r7=q'apropos';$s7=q'ni:/class';$t7=[$s7];$u7=q'# Classes and metaclasses
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
';$v7=bless({$r7,$t7,$S2,$u7,$f,$K},$k2);$w7=q'/lib/doc::ctors';$x7=q'ni:/class.c';$y7=q'ni:/lib/accessor.b';$z7=q'ni:/lib/behavior';$A7=q'ni:/lib/behavior.c';$B7=q'ni:/lib/branch';$C7=q'ni:/lib/branch.b';$D7=q'ni:/lib/branch.c';$E7=q'ni:/lib/branch_init.b';$F7=q'ni:/lib/class_init.b';$G7=q'ni:/lib/classdef.b';$H7=q'ni:/lib/definition.b';$I7=q'ni:/lib/doc';$J7=q'ni:/lib/doc.c';$K7=q'ni:/lib/doc_init.b';$L7=q'ni:/lib/doc_namespace.b';$M7=q'ni:/lib/documentable.b';$N7=q'ni:/lib/fn';$O7=q'ni:/lib/fn.c';$P7=q'ni:/lib/fn_init.b';$Q7=q'ni:/lib/fn_serialize.b';$R7=q'ni:/lib/image';$S7={$n,1};$T7=[$d3];$U7=bless({$c,$S7,$f,$n,$g,$T7},$y);$V7=q'/metaclass::ctors';$W7=q'/lib/image';$X7={$W7,1};$Y7={};$Z7=[];$c8=q'my $class = shift;
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
  ordering     => []};';$d8=bless({$D1,$Z7,$d1,$c8},$X);$e8={$u1,$d8};$f8=q'/lib/image_init.b';$g8=bless({$c,$Y7,$O,$P,$Q,$P,$R,$e8,$f,$f8},$E);$h8=q'/lib/slice::ctors';$i8={};$j8=q'address';$k8=[];$l8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$m8=bless({$D1,$k8,$d1,$l8},$X);$n8=q'allocate_gensym';$o8=[];$p8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$q8=bless({$D1,$o8,$d1,$p8},$X);$r8=q'boot_side_effect';$s8=[];$t8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$u8=bless({$D1,$s8,$d1,$t8},$X);$v8=q'circular_links';$w8=[];$x8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$y8=bless({$D1,$w8,$d1,$x8},$X);$z8=q'finalizer';$A8=[];$B8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$C8=bless({$D1,$A8,$d1,$B8},$X);$D8=q'gensym';$E8=[];$F8=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$G8=bless({$D1,$E8,$d1,$F8},$X);$H8=q'is_circular';$I8=[];$J8=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$K8=bless({$D1,$I8,$d1,$J8},$X);$L8=q'quote';$M8=[];$N8=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$O8=bless({$D1,$M8,$d1,$N8},$X);$P8=q'quote_array';$Q8=[];$R8=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$S8=bless({$D1,$Q8,$d1,$R8},$X);$T8=q'quote_blessed';$U8=[];$V8=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$W8=bless({$D1,$U8,$d1,$V8},$X);$X8=q'quote_class';$Y8=[];$Z8=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$c9=bless({$D1,$Y8,$d1,$Z8},$X);$d9=q'quote_hash';$e9=[];$f9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$g9=bless({$D1,$e9,$d1,$f9},$X);$h9=q'quote_object';$i9=[];$j9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$k9=bless({$D1,$i9,$d1,$j9},$X);$l9=q'quote_scalar';$m9=[];$n9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$o9=bless({$D1,$m9,$d1,$n9},$X);$p9=q'quote_value';$q9=[];$r9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$s9=bless({$D1,$q9,$d1,$r9},$X);$t9=q'reconstruction';$u9=[];$v9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$w9=bless({$D1,$u9,$d1,$v9},$X);$x9=q'side_effect';$y9=[];$z9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$A9=bless({$D1,$y9,$d1,$z9},$X);$B9=q'write';$C9=[];$D9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$E9=bless({$D1,$C9,$d1,$D9},$X);$F9={$j8,$m8,$n8,$q8,$r8,$u8,$v8,$y8,$z8,$C8,$D8,$G8,$H8,$K8,$L8,$O8,$P8,$S8,$T8,$W8,$X8,$c9,$d9,$g9,$h9,$k9,$l9,$o9,$p9,$s9,$t9,$w9,$x9,$A9,$B9,$E9};$G9=q'/lib/image_quoting.b';$H9=bless({$c,$i8,$O,$P,$Q,$P,$R,$F9,$f,$G9},$E);$I9=q'/lib/slice::ctors';$J9=[$g8,$H9];$K9=bless({$c,$X7,$f,$W7,$g,$J9},$n);$L9=q'/lib/image.c::ctors';$M9=q'ni:/lib/image.c';$N9=q'ni:/lib/image_init.b';$O9=q'ni:/lib/image_quoting.b';$P9=q'ni:/lib/instance.b';$Q9=q'ni:/lib/instantiable.b';$R9=q'ni:/lib/named.b';$S9=q'ni:/lib/named_in_ni.b';$T9=q'ni:/lib/namespaced.b';$U9=q'ni:/lib/ni';$V9=q'ni:/lib/ni.c';$W9=q'ni:/lib/ni_image.b';$X9=q'ni:/lib/ni_main.b';$Y9=q'ni:/lib/ni_self.b';$Z9=q'ni:/lib/perlbranch.b';$ca=q'ni:/lib/resolver.b';$da=q'ni:/lib/slice';$ea=q'ni:/lib/slice.b';$fa=q'ni:/lib/slice.c';$ga=q'ni:/lib/slice_init.b';$ha=q'ni:/lib/slice_serialize.b';$ia=q'ni:/lib/subclass.b';$ja=q'ni:/lib/tag';$ka=q'ni:/lib/tag.b';$la=q'ni:/lib/tag.c';$ma=q'ni:/lib/tag_init.b';$na=q'ni:/metaclass';$oa=q'ni:/metaclass.c';$pa=q'ni:/object';$qa=q'ni:/object.c';$ra=q'ni:/unix/fd.b';$sa=q'/unix/file';$ta={$sa,1};$ua=q'/unix/fd.b';$va={};$wa=q'read';$xa=[];$ya=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$za=bless({$D1,$xa,$d1,$ya},$X);$Aa=[];$Ba=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Ca=bless({$D1,$Aa,$d1,$Ba},$X);$Da={$wa,$za,$B9,$Ca};$Ea=q'/unix/fd_safeio.b';$Fa=bless({$c,$va,$O,$P,$Q,$P,$R,$Da,$f,$Ea},$E);$Ga=q'/lib/slice::ctors';$Ha=[$Fa];$Ia=bless({$c,$ta,$f,$ua,$g,$Ha},$H);$Ja=q'/lib/branch::ctors';$Ka=q'ni:/unix/fd_safeio.b';$La=q'ni:/unix/file';$Ma={$s,1};$Na=[$d3];$Oa=bless({$c,$Ma,$f,$s,$g,$Na},$y);$Pa=q'/metaclass::ctors';$Qa={$sa,1};$Ra={};$Sa=[];$Ta=q'shift->{\'name\'}';$Ua=bless({$D1,$Sa,$d1,$Ta},$X);$Va={$f,$Ua};$Wa=q'/unix/file_readers.b';$Xa=bless({$c,$Ra,$O,$P,$Q,$P,$R,$Va,$f,$Wa},$E);$Ya=q'/lib/slice::ctors';$Za={};$cb=[];$db=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$eb=bless({$D1,$cb,$d1,$db},$X);$fb={$u1,$eb};$gb=q'/unix/file_init.b';$hb=bless({$c,$Za,$O,$P,$Q,$P,$R,$fb,$f,$gb},$E);$ib=q'/lib/slice::ctors';$jb={$sa,1};$kb=q'/unix/io.b';$lb={};$mb=q'into';$nb=[];$ob=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->read_size;
while (defined($self->read($_, $block_size))) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$pb=bless({$D1,$nb,$d1,$ob},$X);$qb={$mb,$pb};$rb=q'/unix/io_stream.b';$sb=bless({$c,$lb,$O,$P,$Q,$P,$R,$qb,$f,$rb},$E);$tb=q'/lib/slice::ctors';$ub={};$vb=q'(<>';$wb=[];$xb=q'my $fh = shift->read_fh; <$fh>';$yb=bless({$D1,$wb,$d1,$xb},$X);$zb=q'(@{}';$Ab=[];$Bb=q'my $fh = shift->read_fh; [<$fh>]';$Cb=bless({$D1,$Ab,$d1,$Bb},$X);$Db={$vb,$yb,$zb,$Cb};$Eb=q'/unix/io_readers.b';$Fb=bless({$c,$ub,$O,$P,$Q,$P,$R,$Db,$f,$Eb},$E);$Gb=q'/lib/slice::ctors';$Hb=[$sb,$Fb];$Ib=bless({$c,$jb,$f,$kb,$g,$Hb},$H);$Jb=q'/lib/branch::ctors';$Kb={};$Lb=q'read_fh';$Mb=[];$Nb=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Ob=bless({$D1,$Mb,$d1,$Nb},$X);$Pb=q'write_fh';$Qb=[];$Rb=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Sb=bless({$D1,$Qb,$d1,$Rb},$X);$Tb={$Lb,$Ob,$Pb,$Sb};$Ub=q'/unix/file_io.b';$Vb=bless({$c,$Kb,$O,$P,$Q,$P,$R,$Tb,$f,$Ub},$E);$Wb=q'/lib/slice::ctors';$Xb=[$Xa,$hb,$Ib,$Ia,$Vb];$Yb=bless({$c,$Qa,$f,$sa,$g,$Xb},$s);$Zb=q'/unix/file.c::ctors';$cc=q'ni:/unix/file.c';$dc=q'ni:/unix/file_init.b';$ec=q'ni:/unix/file_io.b';$fc=q'ni:/unix/file_readers.b';$gc=q'ni:/unix/io.b';$hc=q'ni:/unix/io_readers.b';$ic=q'ni:/unix/io_stream.b';$jc=q'ni:/unix/pid';$kc={$t,1};$lc=[$d3];$mc=bless({$c,$kc,$f,$t,$g,$lc},$y);$nc=q'/metaclass::ctors';$oc=q'/unix/pid';$pc={$oc,1};$qc={};$rc=q'pid';$sc=[];$tc=q'shift->{\'pid\'}';$uc=bless({$D1,$sc,$d1,$tc},$X);$vc=q'stderr';$wc=[];$xc=q'shift->{\'stderr\'}';$yc=bless({$D1,$wc,$d1,$xc},$X);$zc=q'stdin';$Ac=[];$Bc=q'shift->{\'stdin\'}';$Cc=bless({$D1,$Ac,$d1,$Bc},$X);$Dc=q'stdout';$Ec=[];$Fc=q'shift->{\'stdout\'}';$Gc=bless({$D1,$Ec,$d1,$Fc},$X);$Hc={$rc,$uc,$vc,$yc,$zc,$Cc,$Dc,$Gc};$Ic=q'/unix/pid_readers.b';$Jc=bless({$c,$qc,$O,$P,$Q,$P,$R,$Hc,$f,$Ic},$E);$Kc=q'/lib/slice::ctors';$Lc={};$Mc=[];$Nc=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Oc=bless({$D1,$Mc,$d1,$Nc},$X);$Pc={$u1,$Oc};$Qc=q'/unix/pid_init.b';$Rc=bless({$c,$Lc,$O,$P,$Q,$P,$R,$Pc,$f,$Qc},$E);$Sc=q'/lib/slice::ctors';$Tc={};$Uc=[];$Vc=q'shift->{stdout}';$Wc=bless({$D1,$Uc,$d1,$Vc},$X);$Xc=[];$Yc=q'shift->{stdin}';$Zc=bless({$D1,$Xc,$d1,$Yc},$X);$cd={$Lb,$Wc,$Pb,$Zc};$dd=q'/unix/pid_io.b';$ed=bless({$c,$Tc,$O,$P,$Q,$P,$R,$cd,$f,$dd},$E);$fd=q'/lib/slice::ctors';$gd=[$Jc,$Rc,$ed];$hd=bless({$c,$pc,$f,$oc,$g,$gd},$t);$id=q'/unix/pid.c::ctors';$jd=q'ni:/unix/pid.c';$kd=q'ni:/unix/pid_init.b';$ld=q'ni:/unix/pid_io.b';$md=q'ni:/unix/pid_readers.b';$nd={$Q6,$v7,$s7,$A5,$x7,$g3,$y7,$m5,$z7,$Y1,$A7,$z,$B7,$P4,$C7,$O3,$D7,$E4,$E7,$M4,$F7,$A4,$G7,$Z4,$H7,$p5,$I7,$p7,$J7,$T6,$K7,$e7,$L7,$m7,$M7,$Y2,$N7,$M1,$O7,$V,$P7,$z1,$Q7,$J1,$R7,$K9,$M9,$U7,$N9,$g8,$O9,$H9,$P9,$S1,$Q9,$m1,$R9,$j2,$S9,$W3,$T9,$g4,$U9,$N6,$V9,$L5,$W9,$p6,$X9,$K6,$Y9,$c6,$Z9,$r4,$ca,$o4,$da,$P2,$ea,$x2,$fa,$C,$ga,$E2,$ha,$M2,$ia,$x5,$ja,$D3,$ka,$t3,$la,$l3,$ma,$A3,$na,$H5,$oa,$D5,$pa,$V1,$qa,$d3,$ra,$Ia,$Ka,$Fa,$La,$Yb,$cc,$Oa,$dc,$hb,$ec,$Vb,$fc,$Xa,$gc,$Ib,$hc,$Fb,$ic,$sb,$jc,$hd,$jd,$mc,$kd,$Rc,$ld,$ed,$md,$Jc};$od=bless({$P6,$nd},$N5);$pd=q'/lib/ni::ctors';$$c3[0]=$A5;$$x[0]=$d3;$$U[0]=$d3;$$O4[4]=$p5;*$r2=\&$p2;*$q2=\&$n2;$m1->apply_unsafe($K);$m1->apply_unsafe($h);$m1->apply_unsafe($j);$m1->apply_unsafe($H);$m1->apply_unsafe($k);$m1->apply_unsafe($l);$m1->apply_unsafe($X);$m1->apply_unsafe($m);$m1->apply_unsafe($n);$m1->apply_unsafe($o);$m1->apply_unsafe($E);$m1->apply_unsafe($p);$m1->apply_unsafe($I);$m1->apply_unsafe($q);$m1->apply_unsafe($y);$m1->apply_unsafe($d);$m1->apply_unsafe($r);$m1->apply_unsafe($s);$m1->apply_unsafe($t);$z1->apply_unsafe($X);$J1->apply_unsafe($X);$S1->apply_unsafe($K);$S1->apply_unsafe($h);$S1->apply_unsafe($G);$S1->apply_unsafe($j);$S1->apply_unsafe($H);$S1->apply_unsafe($k);$S1->apply_unsafe($l);$S1->apply_unsafe($m);$S1->apply_unsafe($n);$S1->apply_unsafe($o);$S1->apply_unsafe($E);$S1->apply_unsafe($p);$S1->apply_unsafe($I);$S1->apply_unsafe($q);$S1->apply_unsafe($y);$S1->apply_unsafe($d);$S1->apply_unsafe($L);$S1->apply_unsafe($r);$S1->apply_unsafe($s);$S1->apply_unsafe($t);$j2->apply_unsafe($K);$j2->apply_unsafe($h);$j2->apply_unsafe($j);$j2->apply_unsafe($H);$j2->apply_unsafe($k);$j2->apply_unsafe($k2);$j2->apply_unsafe($l);$j2->apply_unsafe($m);$j2->apply_unsafe($n);$j2->apply_unsafe($o);$j2->apply_unsafe($E);$j2->apply_unsafe($p);$j2->apply_unsafe($I);$j2->apply_unsafe($q);$j2->apply_unsafe($y);$j2->apply_unsafe($d);$j2->apply_unsafe($r);$j2->apply_unsafe($s);$j2->apply_unsafe($t);$x2->apply_unsafe($E);$E2->apply_unsafe($E);$M2->apply_unsafe($E);$Y2->apply_unsafe($h);$Y2->apply_unsafe($j);$Y2->apply_unsafe($k);$Y2->apply_unsafe($l);$Y2->apply_unsafe($m);$Y2->apply_unsafe($n);$Y2->apply_unsafe($o);$Y2->apply_unsafe($p);$Y2->apply_unsafe($q);$Y2->apply_unsafe($r);$Y2->apply_unsafe($s);$Y2->apply_unsafe($t);$t3->apply_unsafe($I);$A3->apply_unsafe($I);$O3->apply_unsafe($K);$O3->apply_unsafe($h);$O3->apply_unsafe($j);$O3->apply_unsafe($H);$O3->apply_unsafe($k);$O3->apply_unsafe($l);$O3->apply_unsafe($m);$O3->apply_unsafe($n);$O3->apply_unsafe($o);$O3->apply_unsafe($p);$O3->apply_unsafe($q);$O3->apply_unsafe($y);$O3->apply_unsafe($d);$O3->apply_unsafe($r);$O3->apply_unsafe($s);$O3->apply_unsafe($t);$W3->apply_unsafe($K);$W3->apply_unsafe($h);$W3->apply_unsafe($j);$W3->apply_unsafe($H);$W3->apply_unsafe($k);$W3->apply_unsafe($l);$W3->apply_unsafe($m);$W3->apply_unsafe($n);$W3->apply_unsafe($o);$W3->apply_unsafe($E);$W3->apply_unsafe($p);$W3->apply_unsafe($I);$W3->apply_unsafe($q);$W3->apply_unsafe($y);$W3->apply_unsafe($d);$W3->apply_unsafe($r);$W3->apply_unsafe($s);$W3->apply_unsafe($t);$g4->apply_unsafe($K);$g4->apply_unsafe($h);$g4->apply_unsafe($j);$g4->apply_unsafe($H);$g4->apply_unsafe($k);$g4->apply_unsafe($l);$g4->apply_unsafe($m);$g4->apply_unsafe($n);$g4->apply_unsafe($o);$g4->apply_unsafe($E);$g4->apply_unsafe($p);$g4->apply_unsafe($I);$g4->apply_unsafe($q);$g4->apply_unsafe($y);$g4->apply_unsafe($d);$g4->apply_unsafe($r);$g4->apply_unsafe($s);$g4->apply_unsafe($t);$o4->apply_unsafe($K);$o4->apply_unsafe($h);$o4->apply_unsafe($j);$o4->apply_unsafe($H);$o4->apply_unsafe($k);$o4->apply_unsafe($l);$o4->apply_unsafe($m);$o4->apply_unsafe($n);$o4->apply_unsafe($o);$o4->apply_unsafe($p);$o4->apply_unsafe($I);$o4->apply_unsafe($q);$o4->apply_unsafe($y);$o4->apply_unsafe($d);$o4->apply_unsafe($r);$o4->apply_unsafe($s);$o4->apply_unsafe($t);$A4->apply_unsafe($K);$A4->apply_unsafe($h);$A4->apply_unsafe($j);$A4->apply_unsafe($k);$A4->apply_unsafe($l);$A4->apply_unsafe($m);$A4->apply_unsafe($n);$A4->apply_unsafe($o);$A4->apply_unsafe($p);$A4->apply_unsafe($q);$A4->apply_unsafe($y);$A4->apply_unsafe($d);$A4->apply_unsafe($r);$A4->apply_unsafe($s);$A4->apply_unsafe($t);$M4->apply_unsafe($H);$Z4->apply_unsafe($K);$Z4->apply_unsafe($h);$Z4->apply_unsafe($j);$Z4->apply_unsafe($H);$Z4->apply_unsafe($k);$Z4->apply_unsafe($l);$Z4->apply_unsafe($m);$Z4->apply_unsafe($n);$Z4->apply_unsafe($o);$Z4->apply_unsafe($p);$Z4->apply_unsafe($q);$Z4->apply_unsafe($y);$Z4->apply_unsafe($d);$Z4->apply_unsafe($r);$Z4->apply_unsafe($s);$Z4->apply_unsafe($t);$m5->apply_unsafe($K);$m5->apply_unsafe($h);$m5->apply_unsafe($j);$m5->apply_unsafe($H);$m5->apply_unsafe($k);$m5->apply_unsafe($l);$m5->apply_unsafe($m);$m5->apply_unsafe($n);$m5->apply_unsafe($o);$m5->apply_unsafe($p);$m5->apply_unsafe($q);$m5->apply_unsafe($y);$m5->apply_unsafe($d);$m5->apply_unsafe($r);$m5->apply_unsafe($s);$m5->apply_unsafe($t);$x5->apply_unsafe($K);$x5->apply_unsafe($h);$x5->apply_unsafe($j);$x5->apply_unsafe($k);$x5->apply_unsafe($l);$x5->apply_unsafe($m);$x5->apply_unsafe($n);$x5->apply_unsafe($o);$x5->apply_unsafe($p);$x5->apply_unsafe($q);$x5->apply_unsafe($d);$x5->apply_unsafe($r);$x5->apply_unsafe($s);$x5->apply_unsafe($t);$c6->apply_unsafe($N5);$p6->apply_unsafe($N5);$K6->apply_unsafe($N5);$e7->apply_unsafe($k2);$m7->apply_unsafe($k2);$g8->apply_unsafe($W7);$H9->apply_unsafe($W7);$Fa->apply_unsafe($sa);$Xa->apply_unsafe($sa);$hb->apply_unsafe($sa);$sb->apply_unsafe($sa);$Fb->apply_unsafe($sa);$Vb->apply_unsafe($sa);$Jc->apply_unsafe($oc);$Rc->apply_unsafe($oc);$ed->apply_unsafe($oc);$ni::self=$od;&$_($z)for@$A;&$_($C)for@$D;&$_($V)for@$W;&$_($f1)for@$g1;&$_($j1)for@$g1;&$_($m1)for@$n1;&$_($q1)for@$g1;&$_($t1)for@$g1;&$_($w1)for@$g1;&$_($z1)for@$A1;&$_($G1)for@$g1;&$_($J1)for@$K1;&$_($M1)for@$N1;&$_($P1)for@$g1;&$_($S1)for@$T1;&$_($V1)for@$W1;&$_($Y1)for@$Z1;&$_($e2)for@$g1;&$_($g2)for@$g1;&$_($j2)for@$l2;&$_($n2)for@$g1;&$_($p2)for@$g1;&$_($x2)for@$y2;&$_($B2)for@$g1;&$_($E2)for@$F2;&$_($J2)for@$g1;&$_($M2)for@$N2;&$_($P2)for@$Q2;&$_($V2)for@$g1;&$_($Y2)for@$Z2;&$_($d3)for@$e3;&$_($g3)for@$h3;&$_($l3)for@$m3;&$_($q3)for@$g1;&$_($t3)for@$u3;&$_($x3)for@$g1;&$_($A3)for@$B3;&$_($D3)for@$E3;&$_($J3)for@$g1;&$_($L3)for@$g1;&$_($O3)for@$P3;&$_($T3)for@$g1;&$_($W3)for@$X3;&$_($d4)for@$g1;&$_($g4)for@$h4;&$_($l4)for@$g1;&$_($o4)for@$p4;&$_($r4)for@$s4;&$_($v4)for@$g1;&$_($x4)for@$g1;&$_($A4)for@$B4;&$_($E4)for@$F4;&$_($J4)for@$g1;&$_($M4)for@$N4;&$_($P4)for@$Q4;&$_($W4)for@$g1;&$_($Z4)for@$c5;&$_($g5)for@$g1;&$_($j5)for@$g1;&$_($m5)for@$n5;&$_($p5)for@$q5;&$_($u5)for@$g1;&$_($x5)for@$y5;&$_($A5)for@$B5;&$_($D5)for@$E5;&$_($H5)for@$I5;&$_($L5)for@$M5;&$_($T5)for@$g1;&$_($X5)for@$g1;&$_($c6)for@$d6;&$_($i6)for@$g1;&$_($m6)for@$g1;&$_($p6)for@$q6;&$_($v6)for@$g1;&$_($z6)for@$g1;&$_($D6)for@$g1;&$_($H6)for@$g1;&$_($K6)for@$L6;&$_($N6)for@$O6;&$_($T6)for@$U6;&$_($Z6)for@$g1;&$_($e7)for@$f7;&$_($j7)for@$g1;&$_($m7)for@$n7;&$_($p7)for@$q7;&$_($v7)for@$w7;&$_($U7)for@$V7;&$_($d8)for@$g1;&$_($g8)for@$h8;&$_($m8)for@$g1;&$_($q8)for@$g1;&$_($u8)for@$g1;&$_($y8)for@$g1;&$_($C8)for@$g1;&$_($G8)for@$g1;&$_($K8)for@$g1;&$_($O8)for@$g1;&$_($S8)for@$g1;&$_($W8)for@$g1;&$_($c9)for@$g1;&$_($g9)for@$g1;&$_($k9)for@$g1;&$_($o9)for@$g1;&$_($s9)for@$g1;&$_($w9)for@$g1;&$_($A9)for@$g1;&$_($E9)for@$g1;&$_($H9)for@$I9;&$_($K9)for@$L9;&$_($za)for@$g1;&$_($Ca)for@$g1;&$_($Fa)for@$Ga;&$_($Ia)for@$Ja;&$_($Oa)for@$Pa;&$_($Ua)for@$g1;&$_($Xa)for@$Ya;&$_($eb)for@$g1;&$_($hb)for@$ib;&$_($pb)for@$g1;&$_($sb)for@$tb;&$_($yb)for@$g1;&$_($Cb)for@$g1;&$_($Fb)for@$Gb;&$_($Ib)for@$Jb;&$_($Ob)for@$g1;&$_($Sb)for@$g1;&$_($Vb)for@$Wb;&$_($Yb)for@$Zb;&$_($mc)for@$nc;&$_($uc)for@$g1;&$_($yc)for@$g1;&$_($Cc)for@$g1;&$_($Gc)for@$g1;&$_($Jc)for@$Kc;&$_($Oc)for@$g1;&$_($Rc)for@$Sc;&$_($Wc)for@$g1;&$_($Zc)for@$g1;&$_($ed)for@$fd;&$_($hd)for@$id;&$_($od)for@$pd;ni->run(@ARGV);
__DATA__
