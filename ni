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
$c=q'applied_to';$d=q'/metaclass.c';$e={$d,1};$f=q'name';$g=q'slices';$h=q'/class.c';$i={$h,1};$j=q'/lib/behavior.c';$k=q'/lib/branch.c';$l=q'/lib/doc.c';$m=q'/lib/fn.c';$n=q'/lib/image.c';$o=q'/lib/ni.c';$p=q'/lib/slice.c';$q=q'/lib/tag.c';$r=q'/object.c';$s=q'/unix/cat.c';$t=q'/unix/fifo.c';$u=q'/unix/file.c';$v=q'/unix/pid.c';$w={$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$r,1,$s,1,$t,1,$u,1,$v,1};$x={$p,1};$y={$j,1,$k,1,$p,1,$q,1};$z=[undef];$A=q'/metaclass';$B=bless({$c,$y,$f,$j,$g,$z},$A);$C=q'/metaclass::ctors';$D=[$B];$E=bless({$c,$x,$f,$p,$g,$D},$A);$F=q'/metaclass::ctors';$G=q'/lib/slice';$H={$G,1};$I=q'/lib/behavior';$J=q'/lib/branch';$K=q'/lib/tag';$L={$I,1,$J,1,$G,1,$K,1};$M=q'/class';$N=q'/object';$O={$M,1,$h,1,$I,1,$j,1,$J,1,$k,1,$l,1,$m,1,$n,1,$o,1,$G,1,$p,1,$K,1,$q,1,$A,1,$d,1,$N,1,$r,1,$s,1,$t,1,$u,1,$v,1};$P={};$Q=q'ctor';$R=undef;$S=q'dtor';$T=q'methods';$U=q'class';$V={$m,1};$W=[undef];$X=bless({$c,$V,$f,$m,$g,$W},$A);$Y=q'/metaclass::ctors';$Z=q'/lib/fn';$c1={$Z,1};$d1={};$e1=q'DESTROY';$f1=q'code';$g1=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$h1=bless({$f1,$g1},$Z);$i1=q'/lib/fn::ctors';$j1=q'new';$k1=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self) for @{ref($self) . "::ctors"};
$self;';$l1=bless({$f1,$k1},$Z);$m1={$e1,$h1,$j1,$l1};$n1=q'/lib/instantiable.b';$o1=bless({$c,$d1,$T,$m1,$f,$n1},$G);$p1=q'/lib/slice::ctors';$q1={};$r1=q'shift->compile';$s1=bless({$f1,$r1},$Z);$t1=q'compile';$u1=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/b/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$v1=bless({$f1,$u1},$Z);$w1=q'instantiate';$x1=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$y1=bless({$f1,$x1},$Z);$z1={$t1,$v1,$w1,$y1};$A1=q'/lib/fn_init.b';$B1=bless({$c,$q1,$Q,$s1,$S,$R,$T,$z1,$f,$A1},$G);$C1=q'/lib/slice::ctors';$D1={};$E1=q'serialize';$F1=q'annotations';$G1=[];$H1=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$I1=bless({$F1,$G1,$f1,$H1},$Z);$J1={$E1,$I1};$K1=q'/lib/fn_serialize.b';$L1=bless({$c,$D1,$Q,$R,$S,$R,$T,$J1,$f,$K1},$G);$M1=q'/lib/slice::ctors';$N1=[$o1,$B1,$L1];$O1=bless({$c,$c1,$f,$Z,$g,$N1},$m);$P1=q'/lib/fn.c::ctors';$Q1=q'ni \'ni:\' . ref shift';$R1=bless({$f1,$Q1},$Z);$S1={$U,$R1};$T1=q'/lib/instance.b';$U1=bless({$c,$P,$Q,$R,$S,$R,$T,$S1,$f,$T1},$G);$V1=q'/lib/slice::ctors';$W1=[$U1];$X1=bless({$c,$O,$f,$N,$g,$W1},$r);$Y1=q'/object.c::ctors';$Z1=[$X1];$c2=bless({$c,$L,$f,$I,$g,$Z1},$j);$d2=q'/lib/behavior.c::ctors';$e2={};$f2=q'my $s = shift; ni->def($s->name, $s)';$g2=bless({$f1,$f2},$Z);$h2=q'$_[0]->namespace . ":" . $_[0]->{name}';$i2=bless({$f1,$h2},$Z);$j2={$f,$i2};$k2=q'/lib/named.b';$l2=bless({$c,$e2,$Q,$g2,$S,$R,$T,$j2,$f,$k2},$G);$m2=q'/lib/doc';$n2=q'/lib/slice::ctors';$o2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$p2=bless({$f1,$o2},$Z);$q2=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$r2=bless({$f1,$q2},$Z);$s2=q'/lib/slice::apply';$t2=q'/lib/slice::apply_unsafe';$u2={};$v2=q'apply';$w2=q'apply_unsafe';$x2={$v2,$p2,$w2,$r2};$y2=q'/lib/slice.b';$z2=bless({$c,$u2,$T,$x2,$f,$y2},$G);$A2=q'/lib/slice::ctors';$B2={};$C2=q'my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};';$D2=bless({$f1,$C2},$Z);$E2={$w1,$D2};$F2=q'/lib/slice_init.b';$G2=bless({$c,$B2,$T,$E2,$f,$F2},$G);$H2=q'/lib/slice::ctors';$I2={};$J2=[];$K2=q'local $_;
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
$g;';$L2=bless({$F1,$J2,$f1,$K2},$Z);$M2={$E1,$L2};$N2=q'/lib/slice_serialize.b';$O2=bless({$c,$I2,$Q,$R,$S,$R,$T,$M2,$f,$N2},$G);$P2=q'/lib/slice::ctors';$Q2=[$c2,$l2,$z2,$G2,$O2];$R2=bless({$c,$H,$f,$G,$g,$Q2},$p);$S2=q'/lib/slice.c::ctors';$T2={};$U2=q'doc';$V2=[];$W2=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$X2=bless({$F1,$V2,$f1,$W2},$Z);$Y2={$U2,$X2};$Z2=q'/lib/documentable.b';$c3=bless({$c,$T2,$Q,$R,$S,$R,$T,$Y2,$f,$Z2},$G);$d3=q'/lib/slice::ctors';$e3=[undef,$c3];$f3=bless({$c,$w,$f,$r,$g,$e3},$A);$g3=q'/metaclass::ctors';$h3=[$f3];$i3=bless({$c,$i,$f,$h,$g,$h3},$A);$j3=q'/metaclass::ctors';$k3={$M,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1};$l3={$q,1};$m3=[$B];$n3=bless({$c,$l3,$f,$q,$g,$m3},$A);$o3=q'/metaclass::ctors';$p3={$K,1};$q3={};$r3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$s3=bless({$f1,$r3},$Z);$t3={$v2,$s3};$u3=q'/lib/tag.b';$v3=bless({$c,$q3,$Q,$R,$S,$R,$T,$t3,$f,$u3},$G);$w3=q'/lib/slice::ctors';$x3={};$y3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$z3=bless({$f1,$y3},$Z);$A3={$w1,$z3};$B3=q'/lib/tag_init.b';$C3=bless({$c,$x3,$Q,$R,$S,$R,$T,$A3,$f,$B3},$G);$D3=q'/lib/slice::ctors';$E3=[$c2,$l2,$v3,$C3];$F3=bless({$c,$p3,$f,$K,$g,$E3},$q);$G3=q'/lib/tag.c::ctors';$H3=q'/lib/perlbranch.b';$I3={};$J3=q'add';$K3=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$L3=bless({$f1,$K3},$Z);$M3=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$N3=bless({$f1,$M3},$Z);$O3={$J3,$L3,$v2,$N3};$P3=q'/lib/branch.b';$Q3=bless({$c,$I3,$Q,$R,$S,$R,$T,$O3,$f,$P3},$G);$R3=q'/lib/slice::ctors';$S3={};$T3=q'namespace';$U3=q'\'ni\'';$V3=bless({$f1,$U3},$Z);$W3={$T3,$V3};$X3=q'/lib/named_in_ni.b';$Y3=bless({$c,$S3,$Q,$R,$S,$R,$T,$W3,$f,$X3},$G);$Z3=q'/lib/slice::ctors';$c4={};$d4=q'package';$e4=q'shift->{name}';$f4=bless({$f1,$e4},$Z);$g4={$d4,$f4};$h4=q'/lib/namespaced.b';$i4=bless({$c,$c4,$Q,$R,$S,$R,$T,$g4,$f,$h4},$G);$j4=q'/lib/slice::ctors';$k4={};$l4=q'resolve';$m4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$n4=bless({$f1,$m4},$Z);$o4={$l4,$n4};$p4=q'/lib/resolver.b';$q4=bless({$c,$k4,$Q,$R,$S,$R,$T,$o4,$f,$p4},$G);$r4=q'/lib/slice::ctors';$s4=[$Q3,$o1,$l2,$Y3,$i4,$q4];$t4=bless({$f,$H3,$g,$s4},$K);$u4=q'/lib/tag::ctors';$v4={};$w4=q'my $s = shift; $s->apply($s->package)';$x4=bless({$f1,$w4},$Z);$y4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$z4=bless({$f1,$y4},$Z);$A4={$w1,$z4};$B4=q'/lib/class_init.b';$C4=bless({$c,$v4,$Q,$x4,$S,$R,$T,$A4,$f,$B4},$G);$D4=q'/lib/slice::ctors';$E4={$k,1};$F4=[$B];$G4=bless({$c,$E4,$f,$k,$g,$F4},$A);$H4=q'/metaclass::ctors';$I4={$J,1};$J4={};$K4=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$L4=bless({$f1,$K4},$Z);$M4={$w1,$L4};$N4=q'/lib/branch_init.b';$O4=bless({$c,$J4,$Q,$R,$S,$R,$T,$M4,$f,$N4},$G);$P4=q'/lib/slice::ctors';$Q4=[$c2,$l2,$Q3,$O4,undef];$R4=bless({$c,$I4,$f,$J,$g,$Q4},$k);$S4=q'/lib/branch.c::ctors';$T4={$M,1,$h,1,$j,1,$J,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$A,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1};$U4=q'/lib/definition.b';$V4={};$W4=q'def';$X4=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$Y4=bless({$f1,$X4},$Z);$Z4={$W4,$Y4};$c5=q'/lib/classdef.b';$d5=bless({$c,$V4,$Q,$R,$S,$R,$T,$Z4,$f,$c5},$G);$e5=q'/lib/slice::ctors';$f5={};$g5=q'ro';$h5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$i5=bless({$f1,$h5},$Z);$j5=q'rw';$k5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$l5=bless({$f1,$k5},$Z);$m5={$g5,$i5,$j5,$l5};$n5=q'/lib/accessor.b';$o5=bless({$c,$f5,$Q,$R,$S,$R,$T,$m5,$f,$n5},$G);$p5=q'/lib/slice::ctors';$q5=[$d5,$o5];$r5=bless({$c,$T4,$f,$U4,$g,$q5},$J);$s5=q'/lib/branch::ctors';$t5={};$u5=q'child';$v5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, @slices);';$w5=bless({$f1,$v5},$Z);$x5={$u5,$w5};$y5=q'/lib/subclass.b';$z5=bless({$c,$t5,$Q,$R,$S,$R,$T,$x5,$f,$y5},$G);$A5=q'/lib/slice::ctors';$B5=[$t4,$C4,$X1,$r5,$z5];$C5=bless({$c,$k3,$f,$M,$g,$B5},$h);$D5=q'/class.c::ctors';$E5=[$C5];$F5=bless({$c,$e,$f,$d,$g,$E5},$A);$G5=q'/metaclass::ctors';$H5={$A,1};$I5=[$t4,$C4,$X1,$r5];$J5=bless({$c,$H5,$f,$A,$g,$I5},$d);$K5=q'/metaclass.c::ctors';$L5={$o,1};$M5=[$f3];$N5=bless({$c,$L5,$f,$o,$g,$M5},$A);$O5=q'/metaclass::ctors';$P5=q'/lib/ni';$Q5={$P5,1};$R5={};$S5=q'is_mutable';$T5=[];$U5=q'$0 ne "-" && -w $0';$V5=bless({$F1,$T5,$f1,$U5},$Z);$W5=q'modify';$X5=[];$Y5=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$Z5=bless({$F1,$X5,$f1,$Y5},$Z);$c6={$S5,$V5,$W5,$Z5};$d6=q'/lib/ni_self.b';$e6=bless({$c,$R5,$Q,$R,$S,$R,$T,$c6,$f,$d6},$G);$f6=q'/lib/slice::ctors';$g6={};$h6=q'exists';$i6=[];$j6=q'exists $_[0]->{named}{$_[1]}';$k6=bless({$F1,$i6,$f1,$j6},$Z);$l6=q'quoted';$m6=[];$n6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$o6=bless({$F1,$m6,$f1,$n6},$Z);$p6={$h6,$k6,$l6,$o6};$q6=q'/lib/ni_image.b';$r6=bless({$c,$g6,$Q,$R,$S,$R,$T,$p6,$f,$q6},$G);$s6=q'/lib/slice::ctors';$t6={};$u6=q'--internal/+=';$v6=[];$w6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$x6=bless({$F1,$v6,$f1,$w6},$Z);$y6=q'--internal/eval';$z6=[];$A6=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$B6=bless({$F1,$z6,$f1,$A6},$Z);$C6=q'--internal/image';$D6=[];$E6=q'shift->quoted->write(\\*STDOUT);
0;';$F6=bless({$F1,$D6,$f1,$E6},$Z);$G6=q'run';$H6=[];$I6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$J6=bless({$F1,$H6,$f1,$I6},$Z);$K6={$u6,$x6,$y6,$B6,$C6,$F6,$G6,$J6};$L6=q'/lib/ni_main.b';$M6=bless({$c,$t6,$Q,$R,$S,$R,$T,$K6,$f,$L6},$G);$N6=q'/lib/slice::ctors';$O6=[$e6,$r6,$M6];$P6=bless({$c,$Q5,$f,$P5,$g,$O6},$o);$Q6=q'/lib/ni.c::ctors';$R6=q'named';$S6=q'ni.doc:/class';$T6={$l,1};$U6=[$f3];$V6=bless({$c,$T6,$f,$l,$g,$U6},$A);$W6=q'/metaclass::ctors';$X6={$m2,1};$Y6={};$Z6=[];$c7=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$d7=bless({$F1,$Z6,$f1,$c7},$Z);$e7={$w1,$d7};$f7=q'/lib/doc_init.b';$g7=bless({$c,$Y6,$Q,$R,$S,$R,$T,$e7,$f,$f7},$G);$h7=q'/lib/slice::ctors';$i7={};$j7=[];$k7=q'\'ni.doc\'';$l7=bless({$F1,$j7,$f1,$k7},$Z);$m7={$T3,$l7};$n7=q'/lib/doc_namespace.b';$o7=bless({$c,$i7,$Q,$R,$S,$R,$T,$m7,$f,$n7},$G);$p7=q'/lib/slice::ctors';$q7=[$l2,$g7,$o7];$r7=bless({$c,$X6,$f,$m2,$g,$q7},$l);$s7=q'/lib/doc.c::ctors';$t7=q'apropos';$u7=q'ni:/class';$v7=[$u7];$w7=q'# Classes and metaclasses
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
';$x7=bless({$t7,$v7,$U2,$w7,$f,$M},$m2);$y7=q'/lib/doc::ctors';$z7=q'ni:/class.c';$A7=q'ni:/lib/accessor.b';$B7=q'ni:/lib/behavior';$C7=q'ni:/lib/behavior.c';$D7=q'ni:/lib/branch';$E7=q'ni:/lib/branch.b';$F7=q'ni:/lib/branch.c';$G7=q'ni:/lib/branch_init.b';$H7=q'ni:/lib/class_init.b';$I7=q'ni:/lib/classdef.b';$J7=q'ni:/lib/definition.b';$K7=q'ni:/lib/doc';$L7=q'ni:/lib/doc.c';$M7=q'ni:/lib/doc_init.b';$N7=q'ni:/lib/doc_namespace.b';$O7=q'ni:/lib/documentable.b';$P7=q'ni:/lib/fn';$Q7=q'ni:/lib/fn.c';$R7=q'ni:/lib/fn_init.b';$S7=q'ni:/lib/fn_serialize.b';$T7=q'ni:/lib/image';$U7={$n,1};$V7=[$f3];$W7=bless({$c,$U7,$f,$n,$g,$V7},$A);$X7=q'/metaclass::ctors';$Y7=q'/lib/image';$Z7={$Y7,1};$c8={};$d8=[];$e8=q'my $class = shift;
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
  ordering     => []};';$f8=bless({$F1,$d8,$f1,$e8},$Z);$g8={$w1,$f8};$h8=q'/lib/image_init.b';$i8=bless({$c,$c8,$Q,$R,$S,$R,$T,$g8,$f,$h8},$G);$j8=q'/lib/slice::ctors';$k8={};$l8=q'address';$m8=[];$n8=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$o8=bless({$F1,$m8,$f1,$n8},$Z);$p8=q'allocate_gensym';$q8=[];$r8=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$s8=bless({$F1,$q8,$f1,$r8},$Z);$t8=q'boot_side_effect';$u8=[];$v8=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$w8=bless({$F1,$u8,$f1,$v8},$Z);$x8=q'circular_links';$y8=[];$z8=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$A8=bless({$F1,$y8,$f1,$z8},$Z);$B8=q'finalizer';$C8=[];$D8=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$E8=bless({$F1,$C8,$f1,$D8},$Z);$F8=q'gensym';$G8=[];$H8=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$I8=bless({$F1,$G8,$f1,$H8},$Z);$J8=q'is_circular';$K8=[];$L8=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$M8=bless({$F1,$K8,$f1,$L8},$Z);$N8=q'quote';$O8=[];$P8=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$Q8=bless({$F1,$O8,$f1,$P8},$Z);$R8=q'quote_array';$S8=[];$T8=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$U8=bless({$F1,$S8,$f1,$T8},$Z);$V8=q'quote_blessed';$W8=[];$X8=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$Y8=bless({$F1,$W8,$f1,$X8},$Z);$Z8=q'quote_class';$c9=[];$d9=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$e9=bless({$F1,$c9,$f1,$d9},$Z);$f9=q'quote_hash';$g9=[];$h9=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$i9=bless({$F1,$g9,$f1,$h9},$Z);$j9=q'quote_object';$k9=[];$l9=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$m9=bless({$F1,$k9,$f1,$l9},$Z);$n9=q'quote_scalar';$o9=[];$p9=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$q9=bless({$F1,$o9,$f1,$p9},$Z);$r9=q'quote_value';$s9=[];$t9=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$u9=bless({$F1,$s9,$f1,$t9},$Z);$v9=q'reconstruction';$w9=[];$x9=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$y9=bless({$F1,$w9,$f1,$x9},$Z);$z9=q'side_effect';$A9=[];$B9=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$C9=bless({$F1,$A9,$f1,$B9},$Z);$D9=q'write';$E9=[];$F9=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$G9=bless({$F1,$E9,$f1,$F9},$Z);$H9={$l8,$o8,$p8,$s8,$t8,$w8,$x8,$A8,$B8,$E8,$F8,$I8,$J8,$M8,$N8,$Q8,$R8,$U8,$V8,$Y8,$Z8,$e9,$f9,$i9,$j9,$m9,$n9,$q9,$r9,$u9,$v9,$y9,$z9,$C9,$D9,$G9};$I9=q'/lib/image_quoting.b';$J9=bless({$c,$k8,$Q,$R,$S,$R,$T,$H9,$f,$I9},$G);$K9=q'/lib/slice::ctors';$L9=[$i8,$J9];$M9=bless({$c,$Z7,$f,$Y7,$g,$L9},$n);$N9=q'/lib/image.c::ctors';$O9=q'ni:/lib/image.c';$P9=q'ni:/lib/image_init.b';$Q9=q'ni:/lib/image_quoting.b';$R9=q'ni:/lib/instance.b';$S9=q'ni:/lib/instantiable.b';$T9=q'ni:/lib/named.b';$U9=q'ni:/lib/named_in_ni.b';$V9=q'ni:/lib/namespaced.b';$W9=q'ni:/lib/ni';$X9=q'ni:/lib/ni.c';$Y9=q'ni:/lib/ni_image.b';$Z9=q'ni:/lib/ni_main.b';$ca=q'ni:/lib/ni_self.b';$da=q'ni:/lib/perlbranch.b';$ea=q'ni:/lib/resolver.b';$fa=q'ni:/lib/slice';$ga=q'ni:/lib/slice.b';$ha=q'ni:/lib/slice.c';$ia=q'ni:/lib/slice_init.b';$ja=q'ni:/lib/slice_serialize.b';$ka=q'ni:/lib/subclass.b';$la=q'ni:/lib/tag';$ma=q'ni:/lib/tag.b';$na=q'ni:/lib/tag.c';$oa=q'ni:/lib/tag_init.b';$pa=q'ni:/metaclass';$qa=q'ni:/metaclass.c';$ra=q'ni:/object';$sa=q'ni:/object.c';$ta=q'ni:/unix/cat';$ua={$s,1};$va=[$f3];$wa=bless({$c,$ua,$f,$s,$g,$va},$A);$xa=q'/metaclass::ctors';$ya=q'/unix/cat';$za={$ya,1};$Aa={};$Ba=[];$Ca=q'shift; [@_]';$Da=bless({$F1,$Ba,$f1,$Ca},$Z);$Ea={$w1,$Da};$Fa=q'/unix/cat_init.b';$Ga=bless({$c,$Aa,$Q,$R,$S,$R,$T,$Ea,$f,$Fa},$G);$Ha=q'/lib/slice::ctors';$Ia={};$Ja=q'read';$Ka=[];$La=q'my $self = shift;
my $n;
shift @$self until !@$self or $n = $$self[0]->read(@_);
return undef;';$Ma=bless({$F1,$Ka,$f1,$La},$Z);$Na={$Ja,$Ma};$Oa=q'/unix/cat_read.b';$Pa=bless({$c,$Ia,$Q,$R,$S,$R,$T,$Na,$f,$Oa},$G);$Qa=q'/lib/slice::ctors';$Ra=[$Ga,$Pa];$Sa=bless({$c,$za,$f,$ya,$g,$Ra},$s);$Ta=q'/unix/cat.c::ctors';$Ua=q'ni:/unix/cat.c';$Va=q'ni:/unix/cat_init.b';$Wa=q'ni:/unix/cat_read.b';$Xa=q'ni:/unix/fd.b';$Ya=q'/unix/fifo';$Za=q'/unix/file';$cb={$Ya,1,$Za,1};$db=q'/unix/fd.b';$eb={};$fb=[];$gb=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$hb=bless({$F1,$fb,$f1,$gb},$Z);$ib=[];$jb=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$kb=bless({$F1,$ib,$f1,$jb},$Z);$lb={$Ja,$hb,$D9,$kb};$mb=q'/unix/fd_safeio.b';$nb=bless({$c,$eb,$Q,$R,$S,$R,$T,$lb,$f,$mb},$G);$ob=q'/lib/slice::ctors';$pb=[$nb];$qb=bless({$c,$cb,$f,$db,$g,$pb},$J);$rb=q'/lib/branch::ctors';$sb=q'ni:/unix/fd_safeio.b';$tb=q'ni:/unix/fifo';$ub={$t,1};$vb=[$f3];$wb=bless({$c,$ub,$f,$t,$g,$vb},$A);$xb=q'/metaclass::ctors';$yb={$Ya,1};$zb={};$Ab=[];$Bb=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Cb=bless({$F1,$Ab,$f1,$Bb},$Z);$Db={$w1,$Cb};$Eb=q'/unix/fifo_init.b';$Fb=bless({$c,$zb,$Q,$R,$S,$R,$T,$Db,$f,$Eb},$G);$Gb=q'/lib/slice::ctors';$Hb={$Ya,1,$Za,1};$Ib=q'/unix/io.b';$Jb={};$Kb=q'into';$Lb=[];$Mb=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->read_size;
while (defined($self->read($_, $block_size))) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Nb=bless({$F1,$Lb,$f1,$Mb},$Z);$Ob={$Kb,$Nb};$Pb=q'/unix/io_stream.b';$Qb=bless({$c,$Jb,$Q,$R,$S,$R,$T,$Ob,$f,$Pb},$G);$Rb=q'/lib/slice::ctors';$Sb={};$Tb=q'(+';$Ub=[];$Vb=q'ni(\'ni:/unix/cat\')->new(@_)';$Wb=bless({$F1,$Ub,$f1,$Vb},$Z);$Xb={$Tb,$Wb};$Yb=q'/unix/io_constructors.b';$Zb=bless({$c,$Sb,$Q,$R,$S,$R,$T,$Xb,$f,$Yb},$G);$cc=q'/lib/slice::ctors';$dc={};$ec=q'(<>';$fc=[];$gc=q'my $fh = shift->read_fh; <$fh>';$hc=bless({$F1,$fc,$f1,$gc},$Z);$ic=q'(@{}';$jc=[];$kc=q'my $fh = shift->read_fh; [<$fh>]';$lc=bless({$F1,$jc,$f1,$kc},$Z);$mc={$ec,$hc,$ic,$lc};$nc=q'/unix/io_readers.b';$oc=bless({$c,$dc,$Q,$R,$S,$R,$T,$mc,$f,$nc},$G);$pc=q'/lib/slice::ctors';$qc=[$Qb,$Zb,$oc];$rc=bless({$c,$Hb,$f,$Ib,$g,$qc},$J);$sc=q'/lib/branch::ctors';$tc={};$uc=q'read_fh';$vc=[];$wc=q'shift->{read_fh}';$xc=bless({$F1,$vc,$f1,$wc},$Z);$yc=q'write_fh';$zc=[];$Ac=q'shift->{write_fh}';$Bc=bless({$F1,$zc,$f1,$Ac},$Z);$Cc={$uc,$xc,$yc,$Bc};$Dc=q'/unix/file_io.b';$Ec=bless({$c,$tc,$Q,$R,$S,$R,$T,$Cc,$f,$Dc},$G);$Fc=q'/lib/slice::ctors';$Gc=[$Fb,$rc,$qb,$Ec];$Hc=bless({$c,$yb,$f,$Ya,$g,$Gc},$t);$Ic=q'/unix/fifo.c::ctors';$Jc=q'ni:/unix/fifo.c';$Kc=q'ni:/unix/fifo_init.b';$Lc=q'ni:/unix/file';$Mc={$u,1};$Nc=[$f3];$Oc=bless({$c,$Mc,$f,$u,$g,$Nc},$A);$Pc=q'/metaclass::ctors';$Qc={$Za,1};$Rc={};$Sc=[];$Tc=q'shift->{\'name\'}';$Uc=bless({$F1,$Sc,$f1,$Tc},$Z);$Vc={$f,$Uc};$Wc=q'/unix/file_readers.b';$Xc=bless({$c,$Rc,$Q,$R,$S,$R,$T,$Vc,$f,$Wc},$G);$Yc=q'/lib/slice::ctors';$Zc={};$cd=[];$dd=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$ed=bless({$F1,$cd,$f1,$dd},$Z);$fd={$w1,$ed};$gd=q'/unix/file_init.b';$hd=bless({$c,$Zc,$Q,$R,$S,$R,$T,$fd,$f,$gd},$G);$id=q'/lib/slice::ctors';$jd={};$kd=[];$ld=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$md=bless({$F1,$kd,$f1,$ld},$Z);$nd=[];$od=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$pd=bless({$F1,$nd,$f1,$od},$Z);$qd={$uc,$md,$yc,$pd};$rd=bless({$c,$jd,$Q,$R,$S,$R,$T,$qd,$f,$Dc},$G);$sd=q'/lib/slice::ctors';$td=[$Xc,$hd,$rc,$qb,$rd];$ud=bless({$c,$Qc,$f,$Za,$g,$td},$u);$vd=q'/unix/file.c::ctors';$wd=q'ni:/unix/file.c';$xd=q'ni:/unix/file_init.b';$yd=q'ni:/unix/file_io.b';$zd=q'ni:/unix/file_readers.b';$Ad=q'ni:/unix/io.b';$Bd=q'ni:/unix/io_constructors.b';$Cd=q'ni:/unix/io_readers.b';$Dd=q'ni:/unix/io_stream.b';$Ed=q'ni:/unix/pid';$Fd={$v,1};$Gd=[$f3];$Hd=bless({$c,$Fd,$f,$v,$g,$Gd},$A);$Id=q'/metaclass::ctors';$Jd=q'/unix/pid';$Kd={$Jd,1};$Ld={};$Md=q'pid';$Nd=[];$Od=q'shift->{\'pid\'}';$Pd=bless({$F1,$Nd,$f1,$Od},$Z);$Qd=q'stderr';$Rd=[];$Sd=q'shift->{\'stderr\'}';$Td=bless({$F1,$Rd,$f1,$Sd},$Z);$Ud=q'stdin';$Vd=[];$Wd=q'shift->{\'stdin\'}';$Xd=bless({$F1,$Vd,$f1,$Wd},$Z);$Yd=q'stdout';$Zd=[];$ce=q'shift->{\'stdout\'}';$de=bless({$F1,$Zd,$f1,$ce},$Z);$ee={$Md,$Pd,$Qd,$Td,$Ud,$Xd,$Yd,$de};$fe=q'/unix/pid_readers.b';$ge=bless({$c,$Ld,$Q,$R,$S,$R,$T,$ee,$f,$fe},$G);$he=q'/lib/slice::ctors';$ie={};$je=[];$ke=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$le=bless({$F1,$je,$f1,$ke},$Z);$me={$w1,$le};$ne=q'/unix/pid_init.b';$oe=bless({$c,$ie,$Q,$R,$S,$R,$T,$me,$f,$ne},$G);$pe=q'/lib/slice::ctors';$qe={};$re=[];$se=q'shift->{stdout}';$te=bless({$F1,$re,$f1,$se},$Z);$ue=[];$ve=q'shift->{stdin}';$we=bless({$F1,$ue,$f1,$ve},$Z);$xe={$uc,$te,$yc,$we};$ye=q'/unix/pid_io.b';$ze=bless({$c,$qe,$Q,$R,$S,$R,$T,$xe,$f,$ye},$G);$Ae=q'/lib/slice::ctors';$Be=[$ge,$oe,$ze];$Ce=bless({$c,$Kd,$f,$Jd,$g,$Be},$v);$De=q'/unix/pid.c::ctors';$Ee=q'ni:/unix/pid.c';$Fe=q'ni:/unix/pid_init.b';$Ge=q'ni:/unix/pid_io.b';$He=q'ni:/unix/pid_readers.b';$Ie={$S6,$x7,$u7,$C5,$z7,$i3,$A7,$o5,$B7,$c2,$C7,$B,$D7,$R4,$E7,$Q3,$F7,$G4,$G7,$O4,$H7,$C4,$I7,$d5,$J7,$r5,$K7,$r7,$L7,$V6,$M7,$g7,$N7,$o7,$O7,$c3,$P7,$O1,$Q7,$X,$R7,$B1,$S7,$L1,$T7,$M9,$O9,$W7,$P9,$i8,$Q9,$J9,$R9,$U1,$S9,$o1,$T9,$l2,$U9,$Y3,$V9,$i4,$W9,$P6,$X9,$N5,$Y9,$r6,$Z9,$M6,$ca,$e6,$da,$t4,$ea,$q4,$fa,$R2,$ga,$z2,$ha,$E,$ia,$G2,$ja,$O2,$ka,$z5,$la,$F3,$ma,$v3,$na,$n3,$oa,$C3,$pa,$J5,$qa,$F5,$ra,$X1,$sa,$f3,$ta,$Sa,$Ua,$wa,$Va,$Ga,$Wa,$Pa,$Xa,$qb,$sb,$nb,$tb,$Hc,$Jc,$wb,$Kc,$Fb,$Lc,$ud,$wd,$Oc,$xd,$hd,$yd,$rd,$zd,$Xc,$Ad,$rc,$Bd,$Zb,$Cd,$oc,$Dd,$Qb,$Ed,$Ce,$Ee,$Hd,$Fe,$oe,$Ge,$ze,$He,$ge};$Je=bless({$R6,$Ie},$P5);$Ke=q'/lib/ni::ctors';$$e3[0]=$C5;$$z[0]=$f3;$$W[0]=$f3;$$Q4[4]=$r5;*$t2=\&$r2;*$s2=\&$p2;$o1->apply_unsafe($M);$o1->apply_unsafe($h);$o1->apply_unsafe($j);$o1->apply_unsafe($J);$o1->apply_unsafe($k);$o1->apply_unsafe($l);$o1->apply_unsafe($Z);$o1->apply_unsafe($m);$o1->apply_unsafe($n);$o1->apply_unsafe($o);$o1->apply_unsafe($G);$o1->apply_unsafe($p);$o1->apply_unsafe($K);$o1->apply_unsafe($q);$o1->apply_unsafe($A);$o1->apply_unsafe($d);$o1->apply_unsafe($r);$o1->apply_unsafe($s);$o1->apply_unsafe($t);$o1->apply_unsafe($u);$o1->apply_unsafe($v);$B1->apply_unsafe($Z);$L1->apply_unsafe($Z);$U1->apply_unsafe($M);$U1->apply_unsafe($h);$U1->apply_unsafe($I);$U1->apply_unsafe($j);$U1->apply_unsafe($J);$U1->apply_unsafe($k);$U1->apply_unsafe($l);$U1->apply_unsafe($m);$U1->apply_unsafe($n);$U1->apply_unsafe($o);$U1->apply_unsafe($G);$U1->apply_unsafe($p);$U1->apply_unsafe($K);$U1->apply_unsafe($q);$U1->apply_unsafe($A);$U1->apply_unsafe($d);$U1->apply_unsafe($N);$U1->apply_unsafe($r);$U1->apply_unsafe($s);$U1->apply_unsafe($t);$U1->apply_unsafe($u);$U1->apply_unsafe($v);$l2->apply_unsafe($M);$l2->apply_unsafe($h);$l2->apply_unsafe($j);$l2->apply_unsafe($J);$l2->apply_unsafe($k);$l2->apply_unsafe($m2);$l2->apply_unsafe($l);$l2->apply_unsafe($m);$l2->apply_unsafe($n);$l2->apply_unsafe($o);$l2->apply_unsafe($G);$l2->apply_unsafe($p);$l2->apply_unsafe($K);$l2->apply_unsafe($q);$l2->apply_unsafe($A);$l2->apply_unsafe($d);$l2->apply_unsafe($r);$l2->apply_unsafe($s);$l2->apply_unsafe($t);$l2->apply_unsafe($u);$l2->apply_unsafe($v);$z2->apply_unsafe($G);$G2->apply_unsafe($G);$O2->apply_unsafe($G);$c3->apply_unsafe($h);$c3->apply_unsafe($j);$c3->apply_unsafe($k);$c3->apply_unsafe($l);$c3->apply_unsafe($m);$c3->apply_unsafe($n);$c3->apply_unsafe($o);$c3->apply_unsafe($p);$c3->apply_unsafe($q);$c3->apply_unsafe($r);$c3->apply_unsafe($s);$c3->apply_unsafe($t);$c3->apply_unsafe($u);$c3->apply_unsafe($v);$v3->apply_unsafe($K);$C3->apply_unsafe($K);$Q3->apply_unsafe($M);$Q3->apply_unsafe($h);$Q3->apply_unsafe($j);$Q3->apply_unsafe($J);$Q3->apply_unsafe($k);$Q3->apply_unsafe($l);$Q3->apply_unsafe($m);$Q3->apply_unsafe($n);$Q3->apply_unsafe($o);$Q3->apply_unsafe($p);$Q3->apply_unsafe($q);$Q3->apply_unsafe($A);$Q3->apply_unsafe($d);$Q3->apply_unsafe($r);$Q3->apply_unsafe($s);$Q3->apply_unsafe($t);$Q3->apply_unsafe($u);$Q3->apply_unsafe($v);$Y3->apply_unsafe($M);$Y3->apply_unsafe($h);$Y3->apply_unsafe($j);$Y3->apply_unsafe($J);$Y3->apply_unsafe($k);$Y3->apply_unsafe($l);$Y3->apply_unsafe($m);$Y3->apply_unsafe($n);$Y3->apply_unsafe($o);$Y3->apply_unsafe($G);$Y3->apply_unsafe($p);$Y3->apply_unsafe($K);$Y3->apply_unsafe($q);$Y3->apply_unsafe($A);$Y3->apply_unsafe($d);$Y3->apply_unsafe($r);$Y3->apply_unsafe($s);$Y3->apply_unsafe($t);$Y3->apply_unsafe($u);$Y3->apply_unsafe($v);$i4->apply_unsafe($M);$i4->apply_unsafe($h);$i4->apply_unsafe($j);$i4->apply_unsafe($J);$i4->apply_unsafe($k);$i4->apply_unsafe($l);$i4->apply_unsafe($m);$i4->apply_unsafe($n);$i4->apply_unsafe($o);$i4->apply_unsafe($G);$i4->apply_unsafe($p);$i4->apply_unsafe($K);$i4->apply_unsafe($q);$i4->apply_unsafe($A);$i4->apply_unsafe($d);$i4->apply_unsafe($r);$i4->apply_unsafe($s);$i4->apply_unsafe($t);$i4->apply_unsafe($u);$i4->apply_unsafe($v);$q4->apply_unsafe($M);$q4->apply_unsafe($h);$q4->apply_unsafe($j);$q4->apply_unsafe($J);$q4->apply_unsafe($k);$q4->apply_unsafe($l);$q4->apply_unsafe($m);$q4->apply_unsafe($n);$q4->apply_unsafe($o);$q4->apply_unsafe($p);$q4->apply_unsafe($K);$q4->apply_unsafe($q);$q4->apply_unsafe($A);$q4->apply_unsafe($d);$q4->apply_unsafe($r);$q4->apply_unsafe($s);$q4->apply_unsafe($t);$q4->apply_unsafe($u);$q4->apply_unsafe($v);$C4->apply_unsafe($M);$C4->apply_unsafe($h);$C4->apply_unsafe($j);$C4->apply_unsafe($k);$C4->apply_unsafe($l);$C4->apply_unsafe($m);$C4->apply_unsafe($n);$C4->apply_unsafe($o);$C4->apply_unsafe($p);$C4->apply_unsafe($q);$C4->apply_unsafe($A);$C4->apply_unsafe($d);$C4->apply_unsafe($r);$C4->apply_unsafe($s);$C4->apply_unsafe($t);$C4->apply_unsafe($u);$C4->apply_unsafe($v);$O4->apply_unsafe($J);$d5->apply_unsafe($M);$d5->apply_unsafe($h);$d5->apply_unsafe($j);$d5->apply_unsafe($J);$d5->apply_unsafe($k);$d5->apply_unsafe($l);$d5->apply_unsafe($m);$d5->apply_unsafe($n);$d5->apply_unsafe($o);$d5->apply_unsafe($p);$d5->apply_unsafe($q);$d5->apply_unsafe($A);$d5->apply_unsafe($d);$d5->apply_unsafe($r);$d5->apply_unsafe($s);$d5->apply_unsafe($t);$d5->apply_unsafe($u);$d5->apply_unsafe($v);$o5->apply_unsafe($M);$o5->apply_unsafe($h);$o5->apply_unsafe($j);$o5->apply_unsafe($J);$o5->apply_unsafe($k);$o5->apply_unsafe($l);$o5->apply_unsafe($m);$o5->apply_unsafe($n);$o5->apply_unsafe($o);$o5->apply_unsafe($p);$o5->apply_unsafe($q);$o5->apply_unsafe($A);$o5->apply_unsafe($d);$o5->apply_unsafe($r);$o5->apply_unsafe($s);$o5->apply_unsafe($t);$o5->apply_unsafe($u);$o5->apply_unsafe($v);$z5->apply_unsafe($M);$z5->apply_unsafe($h);$z5->apply_unsafe($j);$z5->apply_unsafe($k);$z5->apply_unsafe($l);$z5->apply_unsafe($m);$z5->apply_unsafe($n);$z5->apply_unsafe($o);$z5->apply_unsafe($p);$z5->apply_unsafe($q);$z5->apply_unsafe($d);$z5->apply_unsafe($r);$z5->apply_unsafe($s);$z5->apply_unsafe($t);$z5->apply_unsafe($u);$z5->apply_unsafe($v);$e6->apply_unsafe($P5);$r6->apply_unsafe($P5);$M6->apply_unsafe($P5);$g7->apply_unsafe($m2);$o7->apply_unsafe($m2);$i8->apply_unsafe($Y7);$J9->apply_unsafe($Y7);$Ga->apply_unsafe($ya);$Pa->apply_unsafe($ya);$nb->apply_unsafe($Ya);$nb->apply_unsafe($Za);$Fb->apply_unsafe($Ya);$Qb->apply_unsafe($Ya);$Qb->apply_unsafe($Za);$Zb->apply_unsafe($Ya);$Zb->apply_unsafe($Za);$oc->apply_unsafe($Ya);$oc->apply_unsafe($Za);$Ec->apply_unsafe($Ya);$Xc->apply_unsafe($Za);$hd->apply_unsafe($Za);$rd->apply_unsafe($Za);$ge->apply_unsafe($Jd);$oe->apply_unsafe($Jd);$ze->apply_unsafe($Jd);$ni::self=$Je;&$_($B)for@$C;&$_($E)for@$F;&$_($X)for@$Y;&$_($h1)for@$i1;&$_($l1)for@$i1;&$_($o1)for@$p1;&$_($s1)for@$i1;&$_($v1)for@$i1;&$_($y1)for@$i1;&$_($B1)for@$C1;&$_($I1)for@$i1;&$_($L1)for@$M1;&$_($O1)for@$P1;&$_($R1)for@$i1;&$_($U1)for@$V1;&$_($X1)for@$Y1;&$_($c2)for@$d2;&$_($g2)for@$i1;&$_($i2)for@$i1;&$_($l2)for@$n2;&$_($p2)for@$i1;&$_($r2)for@$i1;&$_($z2)for@$A2;&$_($D2)for@$i1;&$_($G2)for@$H2;&$_($L2)for@$i1;&$_($O2)for@$P2;&$_($R2)for@$S2;&$_($X2)for@$i1;&$_($c3)for@$d3;&$_($f3)for@$g3;&$_($i3)for@$j3;&$_($n3)for@$o3;&$_($s3)for@$i1;&$_($v3)for@$w3;&$_($z3)for@$i1;&$_($C3)for@$D3;&$_($F3)for@$G3;&$_($L3)for@$i1;&$_($N3)for@$i1;&$_($Q3)for@$R3;&$_($V3)for@$i1;&$_($Y3)for@$Z3;&$_($f4)for@$i1;&$_($i4)for@$j4;&$_($n4)for@$i1;&$_($q4)for@$r4;&$_($t4)for@$u4;&$_($x4)for@$i1;&$_($z4)for@$i1;&$_($C4)for@$D4;&$_($G4)for@$H4;&$_($L4)for@$i1;&$_($O4)for@$P4;&$_($R4)for@$S4;&$_($Y4)for@$i1;&$_($d5)for@$e5;&$_($i5)for@$i1;&$_($l5)for@$i1;&$_($o5)for@$p5;&$_($r5)for@$s5;&$_($w5)for@$i1;&$_($z5)for@$A5;&$_($C5)for@$D5;&$_($F5)for@$G5;&$_($J5)for@$K5;&$_($N5)for@$O5;&$_($V5)for@$i1;&$_($Z5)for@$i1;&$_($e6)for@$f6;&$_($k6)for@$i1;&$_($o6)for@$i1;&$_($r6)for@$s6;&$_($x6)for@$i1;&$_($B6)for@$i1;&$_($F6)for@$i1;&$_($J6)for@$i1;&$_($M6)for@$N6;&$_($P6)for@$Q6;&$_($V6)for@$W6;&$_($d7)for@$i1;&$_($g7)for@$h7;&$_($l7)for@$i1;&$_($o7)for@$p7;&$_($r7)for@$s7;&$_($x7)for@$y7;&$_($W7)for@$X7;&$_($f8)for@$i1;&$_($i8)for@$j8;&$_($o8)for@$i1;&$_($s8)for@$i1;&$_($w8)for@$i1;&$_($A8)for@$i1;&$_($E8)for@$i1;&$_($I8)for@$i1;&$_($M8)for@$i1;&$_($Q8)for@$i1;&$_($U8)for@$i1;&$_($Y8)for@$i1;&$_($e9)for@$i1;&$_($i9)for@$i1;&$_($m9)for@$i1;&$_($q9)for@$i1;&$_($u9)for@$i1;&$_($y9)for@$i1;&$_($C9)for@$i1;&$_($G9)for@$i1;&$_($J9)for@$K9;&$_($M9)for@$N9;&$_($wa)for@$xa;&$_($Da)for@$i1;&$_($Ga)for@$Ha;&$_($Ma)for@$i1;&$_($Pa)for@$Qa;&$_($Sa)for@$Ta;&$_($hb)for@$i1;&$_($kb)for@$i1;&$_($nb)for@$ob;&$_($qb)for@$rb;&$_($wb)for@$xb;&$_($Cb)for@$i1;&$_($Fb)for@$Gb;&$_($Nb)for@$i1;&$_($Qb)for@$Rb;&$_($Wb)for@$i1;&$_($Zb)for@$cc;&$_($hc)for@$i1;&$_($lc)for@$i1;&$_($oc)for@$pc;&$_($rc)for@$sc;&$_($xc)for@$i1;&$_($Bc)for@$i1;&$_($Ec)for@$Fc;&$_($Hc)for@$Ic;&$_($Oc)for@$Pc;&$_($Uc)for@$i1;&$_($Xc)for@$Yc;&$_($ed)for@$i1;&$_($hd)for@$id;&$_($md)for@$i1;&$_($pd)for@$i1;&$_($rd)for@$sd;&$_($ud)for@$vd;&$_($Hd)for@$Id;&$_($Pd)for@$i1;&$_($Td)for@$i1;&$_($Xd)for@$i1;&$_($de)for@$i1;&$_($ge)for@$he;&$_($le)for@$i1;&$_($oe)for@$pe;&$_($te)for@$i1;&$_($we)for@$i1;&$_($ze)for@$Ae;&$_($Ce)for@$De;&$_($Je)for@$Ke;ni->run(@ARGV);
__DATA__
