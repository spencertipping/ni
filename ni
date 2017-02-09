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
$g;';$f3=bless({$W1,$d3,$v1,$e3},$S);$g3={$V1,$f3};$h3=q'/lib/slice_serialize.b';$i3=bless({$c,$c3,$j1,$k1,$l1,$k1,$m1,$g3,$f,$h3},$K);$j3=q'/lib/slice::ctors';$k3=[$u2,$D2,$R2,$Y2,$i3];$l3=bless({$c,$L,$f,$K,$g,$k3},$p);$m3=q'/lib/slice.c::ctors';$n3={};$o3=q'doc';$p3=[];$q3=q'my $self = shift;
my $doc  = pop;
ni(\'ni:/lib/doc\')->new(@_, $self, $doc);
$self;';$r3=bless({$W1,$p3,$v1,$q3},$S);$s3={$o3,$r3};$t3=q'/lib/documentable.b';$u3=bless({$c,$n3,$j1,$k1,$l1,$k1,$m1,$s3,$f,$t3},$K);$v3=q'/unix/pipeline.c';$w3=q'/lib/slice::ctors';$x3=[undef,$u3];$y3=bless({$c,$A,$f,$r,$g,$x3},$E);$z3=q'/metaclass::ctors';$A3=[$y3];$B3=bless({$c,$i,$f,$h,$g,$A3},$E);$C3=q'/metaclass::ctors';$D3=q'/unix/pipeline.c';$E3={$Q,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$D3,1,$z,1};$F3={$q,1};$G3=[$F];$H3=bless({$c,$F3,$f,$q,$g,$G3},$E);$I3=q'/metaclass::ctors';$J3={$O,1};$K3={};$L3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$M3=bless({$v1,$L3},$S);$N3={$N2,$M3};$O3=q'/lib/tag.b';$P3=bless({$c,$K3,$j1,$k1,$l1,$k1,$m1,$N3,$f,$O3},$K);$Q3=q'/lib/slice::ctors';$R3={};$S3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$T3=bless({$v1,$S3},$S);$U3={$N1,$T3};$V3=q'/lib/tag_init.b';$W3=bless({$c,$R3,$j1,$k1,$l1,$k1,$m1,$U3,$f,$V3},$K);$X3=q'/lib/slice::ctors';$Y3=[$u2,$D2,$P3,$W3];$Z3=bless({$c,$J3,$f,$O,$g,$Y3},$q);$c4=q'/lib/tag.c::ctors';$d4=q'/lib/perlbranch.b';$e4={};$f4=q'add';$g4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$h4=bless({$v1,$g4},$S);$i4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$j4=bless({$v1,$i4},$S);$k4={$f4,$h4,$N2,$j4};$l4=q'/lib/branch.b';$m4=bless({$c,$e4,$j1,$k1,$l1,$k1,$m1,$k4,$f,$l4},$K);$n4=q'/unix/pipeline.c';$o4=q'/lib/slice::ctors';$p4={};$q4=q'namespace';$r4=q'\'ni\'';$s4=bless({$v1,$r4},$S);$t4={$q4,$s4};$u4=q'/lib/named_in_ni.b';$v4=bless({$c,$p4,$j1,$k1,$l1,$k1,$m1,$t4,$f,$u4},$K);$w4=q'/unix/pipeline.c';$x4=q'/lib/slice::ctors';$y4={};$z4=q'package';$A4=q'shift->{name}';$B4=bless({$v1,$A4},$S);$C4={$z4,$B4};$D4=q'/lib/namespaced.b';$E4=bless({$c,$y4,$j1,$k1,$l1,$k1,$m1,$C4,$f,$D4},$K);$F4=q'/unix/pipeline.c';$G4=q'/lib/slice::ctors';$H4={};$I4=q'resolve';$J4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$K4=bless({$v1,$J4},$S);$L4={$I4,$K4};$M4=q'/lib/resolver.b';$N4=bless({$c,$H4,$j1,$k1,$l1,$k1,$m1,$L4,$f,$M4},$K);$O4=q'/unix/pipeline.c';$P4=q'/lib/slice::ctors';$Q4=[$m4,$E1,$D2,$v4,$E4,$N4];$R4=bless({$f,$d4,$g,$Q4},$O);$S4=q'/lib/tag::ctors';$T4={};$U4=q'my $s = shift; $s->apply($s->package)';$V4=bless({$v1,$U4},$S);$W4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$X4=bless({$v1,$W4},$S);$Y4={$N1,$X4};$Z4=q'/lib/class_init.b';$c5=bless({$c,$T4,$j1,$V4,$l1,$k1,$m1,$Y4,$f,$Z4},$K);$d5=q'/unix/pipeline.c';$e5=q'/lib/slice::ctors';$f5={$k,1};$g5=[$F];$h5=bless({$c,$f5,$f,$k,$g,$g5},$E);$i5=q'/metaclass::ctors';$j5={$N,1};$k5={};$l5=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$m5=bless({$v1,$l5},$S);$n5={$N1,$m5};$o5=q'/lib/branch_init.b';$p5=bless({$c,$k5,$j1,$k1,$l1,$k1,$m1,$n5,$f,$o5},$K);$q5=q'/lib/slice::ctors';$r5=[$u2,$D2,$m4,$p5,undef];$s5=bless({$c,$j5,$f,$N,$g,$r5},$k);$t5=q'/lib/branch.c::ctors';$u5=q'/unix/pipeline.c';$v5={$Q,1,$h,1,$j,1,$N,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$E,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$u5,1,$z,1};$w5=q'/lib/definition.b';$x5={};$y5=q'def';$z5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$A5=bless({$v1,$z5},$S);$B5={$y5,$A5};$C5=q'/lib/classdef.b';$D5=bless({$c,$x5,$j1,$k1,$l1,$k1,$m1,$B5,$f,$C5},$K);$E5=q'/unix/pipeline.c';$F5=q'/lib/slice::ctors';$G5={};$H5=q'ro';$I5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$J5=bless({$v1,$I5},$S);$K5=q'rw';$L5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$M5=bless({$v1,$L5},$S);$N5={$H5,$J5,$K5,$M5};$O5=q'/lib/accessor.b';$P5=bless({$c,$G5,$j1,$k1,$l1,$k1,$m1,$N5,$f,$O5},$K);$Q5=q'/unix/pipeline.c';$R5=q'/lib/slice::ctors';$S5=[$D5,$P5];$T5=bless({$c,$v5,$f,$w5,$g,$S5},$N);$U5=q'/lib/branch::ctors';$V5={};$W5=q'child';$X5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$Y5=bless({$v1,$X5},$S);$Z5={$W5,$Y5};$c6=q'/lib/subclass.b';$d6=bless({$c,$V5,$j1,$k1,$l1,$k1,$m1,$Z5,$f,$c6},$K);$e6=q'/unix/pipeline.c';$f6=q'/lib/slice::ctors';$g6=[$R4,$c5,$r2,$T5,$d6];$h6=bless({$c,$E3,$f,$Q,$g,$g6},$h);$i6=q'/class.c::ctors';$j6=[$h6];$k6=bless({$c,$e,$f,$d,$g,$j6},$E);$l6=q'/metaclass::ctors';$m6={$E,1};$n6=[$R4,$c5,$r2,$T5];$o6=bless({$c,$m6,$f,$E,$g,$n6},$d);$p6=q'/metaclass.c::ctors';$q6={$o,1};$r6=[$y3];$s6=bless({$c,$q6,$f,$o,$g,$r6},$E);$t6=q'/metaclass::ctors';$u6={$U,1};$v6={};$w6=q'is_mutable';$x6=[];$y6=q'$0 ne "-" && -w $0';$z6=bless({$W1,$x6,$v1,$y6},$S);$A6=q'modify';$B6=[];$C6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$D6=bless({$W1,$B6,$v1,$C6},$S);$E6={$w6,$z6,$A6,$D6};$F6=q'/lib/ni_self.b';$G6=bless({$c,$v6,$j1,$k1,$l1,$k1,$m1,$E6,$f,$F6},$K);$H6=q'/lib/slice::ctors';$I6={};$J6=q'exists';$K6=[];$L6=q'exists $_[0]->{named}{$_[1]}';$M6=bless({$W1,$K6,$v1,$L6},$S);$N6=q'quoted';$O6=[];$P6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$Q6=bless({$W1,$O6,$v1,$P6},$S);$R6={$J6,$M6,$N6,$Q6};$S6=q'/lib/ni_image.b';$T6=bless({$c,$I6,$j1,$k1,$l1,$k1,$m1,$R6,$f,$S6},$K);$U6=q'/lib/slice::ctors';$V6={};$W6=q'--internal/+=';$X6=[];$Y6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$Z6=bless({$W1,$X6,$v1,$Y6},$S);$c7=q'--internal/eval';$d7=[];$e7=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$f7=bless({$W1,$d7,$v1,$e7},$S);$g7=q'--internal/image';$h7=[];$i7=q'shift->quoted->write(\\*STDOUT);
0;';$j7=bless({$W1,$h7,$v1,$i7},$S);$k7=q'run';$l7=[];$m7=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$n7=bless({$W1,$l7,$v1,$m7},$S);$o7={$W6,$Z6,$c7,$f7,$g7,$j7,$k7,$n7};$p7=q'/lib/ni_main.b';$q7=bless({$c,$V6,$j1,$k1,$l1,$k1,$m1,$o7,$f,$p7},$K);$r7=q'/lib/slice::ctors';$s7={};$t7=[];$u7=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$v7=bless({$W1,$t7,$v1,$u7},$S);$w7=q'resolver_for';$x7=[];$y7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$z7=bless({$W1,$x7,$v1,$y7},$S);$A7={$I4,$v7,$w7,$z7};$B7=q'/lib/ni_resolver.b';$C7=bless({$c,$s7,$j1,$k1,$l1,$k1,$m1,$A7,$f,$B7},$K);$D7=q'/lib/slice::ctors';$E7={};$F7=q'fork';$G7=[];$H7=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$I7=bless({$W1,$G7,$v1,$H7},$S);$J7=q'fork_exec';$K7=[];$L7=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$M7=bless({$W1,$K7,$v1,$L7},$S);$N7={$F7,$I7,$J7,$M7};$O7=q'/lib/ni_pid_ctors';$P7=bless({$c,$E7,$j1,$k1,$l1,$k1,$m1,$N7,$f,$O7},$K);$Q7=q'/lib/slice::ctors';$R7=[$r2,$G6,$T6,$q7,$C7,$P7];$S7=bless({$c,$u6,$f,$U,$g,$R7},$o);$T7=q'/lib/ni.c::ctors';$U7=q'named';$V7=q'ni.doc:/class';$W7={$l,1};$X7=[$y3];$Y7=bless({$c,$W7,$f,$l,$g,$X7},$E);$Z7=q'/metaclass::ctors';$c8={$R,1};$d8={};$e8=[];$f8=q'my $class = shift;
my $name  = shift;
my $doc   = pop;
+{name    => $name,
  doc     => $doc,
  apropos => [map ref($_) ? $_->name : $_, @_]};';$g8=bless({$W1,$e8,$v1,$f8},$S);$h8={$N1,$g8};$i8=q'/lib/doc_init.b';$j8=bless({$c,$d8,$j1,$k1,$l1,$k1,$m1,$h8,$f,$i8},$K);$k8=q'/lib/slice::ctors';$l8={};$m8=[];$n8=q'\'ni.doc\'';$o8=bless({$W1,$m8,$v1,$n8},$S);$p8={$q4,$o8};$q8=q'/lib/doc_namespace.b';$r8=bless({$c,$l8,$j1,$k1,$l1,$k1,$m1,$p8,$f,$q8},$K);$s8=q'/lib/slice::ctors';$t8=[$r2,$D2,$j8,$r8];$u8=bless({$c,$c8,$f,$R,$g,$t8},$l);$v8=q'/lib/doc.c::ctors';$w8=q'apropos';$x8=q'ni:/class';$y8=[$x8];$z8=q'# Classes and metaclasses
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
';$A8=bless({$w8,$y8,$o3,$z8,$f,$Q},$R);$B8=q'/lib/doc::ctors';$C8=q'ni:/class.c';$D8=q'ni:/lib/accessor.b';$E8=q'ni:/lib/behavior';$F8=q'ni:/lib/behavior.c';$G8=q'ni:/lib/branch';$H8=q'ni:/lib/branch.b';$I8=q'ni:/lib/branch.c';$J8=q'ni:/lib/branch_init.b';$K8=q'ni:/lib/class_init.b';$L8=q'ni:/lib/classdef.b';$M8=q'ni:/lib/definition.b';$N8=q'ni:/lib/doc';$O8=q'ni:/lib/doc.c';$P8=q'ni:/lib/doc_init.b';$Q8=q'ni:/lib/doc_namespace.b';$R8=q'ni:/lib/documentable.b';$S8=q'ni:/lib/fn';$T8=q'ni:/lib/fn.c';$U8=q'ni:/lib/fn_init.b';$V8=q'ni:/lib/fn_serialize.b';$W8=q'ni:/lib/image';$X8={$n,1};$Y8=[$y3];$Z8=bless({$c,$X8,$f,$n,$g,$Y8},$E);$c9=q'/metaclass::ctors';$d9={$T,1};$e9={};$f9=[];$g9=q'my $class = shift;
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
  ordering     => []};';$h9=bless({$W1,$f9,$v1,$g9},$S);$i9={$N1,$h9};$j9=q'/lib/image_init.b';$k9=bless({$c,$e9,$j1,$k1,$l1,$k1,$m1,$i9,$f,$j9},$K);$l9=q'/lib/slice::ctors';$m9={};$n9=q'address';$o9=[];$p9=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$q9=bless({$W1,$o9,$v1,$p9},$S);$r9=q'allocate_gensym';$s9=[];$t9=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$u9=bless({$W1,$s9,$v1,$t9},$S);$v9=q'boot_side_effect';$w9=[];$x9=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$y9=bless({$W1,$w9,$v1,$x9},$S);$z9=q'circular_links';$A9=[];$B9=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$C9=bless({$W1,$A9,$v1,$B9},$S);$D9=q'finalizer';$E9=[];$F9=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$G9=bless({$W1,$E9,$v1,$F9},$S);$H9=q'gensym';$I9=[];$J9=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$K9=bless({$W1,$I9,$v1,$J9},$S);$L9=q'is_circular';$M9=[];$N9=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$O9=bless({$W1,$M9,$v1,$N9},$S);$P9=q'quote';$Q9=[];$R9=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$S9=bless({$W1,$Q9,$v1,$R9},$S);$T9=q'quote_array';$U9=[];$V9=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$W9=bless({$W1,$U9,$v1,$V9},$S);$X9=q'quote_blessed';$Y9=[];$Z9=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$ca=bless({$W1,$Y9,$v1,$Z9},$S);$da=q'quote_class';$ea=[];$fa=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$ga=bless({$W1,$ea,$v1,$fa},$S);$ha=q'quote_hash';$ia=[];$ja=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$ka=bless({$W1,$ia,$v1,$ja},$S);$la=q'quote_object';$ma=[];$na=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$oa=bless({$W1,$ma,$v1,$na},$S);$pa=q'quote_scalar';$qa=[];$ra=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$sa=bless({$W1,$qa,$v1,$ra},$S);$ta=q'quote_value';$ua=[];$va=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$wa=bless({$W1,$ua,$v1,$va},$S);$xa=q'reconstruction';$ya=[];$za=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$Aa=bless({$W1,$ya,$v1,$za},$S);$Ba=q'side_effect';$Ca=[];$Da=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Ea=bless({$W1,$Ca,$v1,$Da},$S);$Fa=q'write';$Ga=[];$Ha=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$Ia=bless({$W1,$Ga,$v1,$Ha},$S);$Ja={$n9,$q9,$r9,$u9,$v9,$y9,$z9,$C9,$D9,$G9,$H9,$K9,$L9,$O9,$P9,$S9,$T9,$W9,$X9,$ca,$da,$ga,$ha,$ka,$la,$oa,$pa,$sa,$ta,$wa,$xa,$Aa,$Ba,$Ea,$Fa,$Ia};$Ka=q'/lib/image_quoting.b';$La=bless({$c,$m9,$j1,$k1,$l1,$k1,$m1,$Ja,$f,$Ka},$K);$Ma=q'/lib/slice::ctors';$Na=[$r2,$k9,$La];$Oa=bless({$c,$d9,$f,$T,$g,$Na},$n);$Pa=q'/lib/image.c::ctors';$Qa=q'ni:/lib/image.c';$Ra=q'ni:/lib/image_init.b';$Sa=q'ni:/lib/image_quoting.b';$Ta=q'ni:/lib/instance.b';$Ua=q'ni:/lib/instantiable.b';$Va=q'ni:/lib/named.b';$Wa=q'ni:/lib/named_in_ni.b';$Xa=q'ni:/lib/namespaced.b';$Ya=q'ni:/lib/ni';$Za=q'ni:/lib/ni.c';$cb=q'ni:/lib/ni_image.b';$db=q'ni:/lib/ni_main.b';$eb=q'ni:/lib/ni_pid_ctors';$fb=q'ni:/lib/ni_resolver.b';$gb=q'ni:/lib/ni_self.b';$hb=q'ni:/lib/ni_static';$ib=q'/lib/ni_static';$jb=q'ni';$kb={$ib,1,$jb,1};$lb={};$mb=q'abbrev';$nb=[];$ob=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$pb=bless({$W1,$nb,$v1,$ob},$S);$qb=q'dor';$rb=[];$sb=q'defined $_[0] ? $_[0] : $_[1]';$tb=bless({$W1,$rb,$v1,$sb},$S);$ub=q'indent';$vb=[];$wb=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$xb=bless({$W1,$vb,$v1,$wb},$S);$yb=q'max';$zb=[];$Ab=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$Bb=bless({$W1,$zb,$v1,$Ab},$S);$Cb=q'maxstr';$Db=[];$Eb=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$Fb=bless({$W1,$Db,$v1,$Eb},$S);$Gb=q'mean';$Hb=[];$Ib=q'sum(@_) / (@_ || 1)';$Jb=bless({$W1,$Hb,$v1,$Ib},$S);$Kb=q'min';$Lb=[];$Mb=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$Nb=bless({$W1,$Lb,$v1,$Mb},$S);$Ob=q'minstr';$Pb=[];$Qb=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$Rb=bless({$W1,$Pb,$v1,$Qb},$S);$Sb=q'sgr';$Tb=[];$Ub=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Vb=bless({$W1,$Tb,$v1,$Ub},$S);$Wb=q'sr';$Xb=[];$Yb=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Zb=bless({$W1,$Xb,$v1,$Yb},$S);$cc=q'sum';$dc=[];$ec=q'local $_; my $x = 0; $x += $_ for @_; $x';$fc=bless({$W1,$dc,$v1,$ec},$S);$gc=q'swap';$hc=[];$ic=q'@_[0, 1] = @_[1, 0]';$jc=bless({$W1,$hc,$v1,$ic},$S);$kc={$mb,$pb,$qb,$tb,$ub,$xb,$yb,$Bb,$Cb,$Fb,$Gb,$Jb,$Kb,$Nb,$Ob,$Rb,$Sb,$Vb,$Wb,$Zb,$cc,$fc,$gc,$jc};$lc=q'/lib/ni_static_util.b';$mc=bless({$c,$lb,$j1,$k1,$l1,$k1,$m1,$kc,$f,$lc},$K);$nc=q'/lib/slice::ctors';$oc=[$mc];$pc=bless({$c,$kb,$f,$ib,$g,$oc},$Q);$qc=q'/class::ctors';$rc=q'ni:/lib/ni_static_util.b';$sc=q'ni:/lib/perlbranch.b';$tc=q'ni:/lib/resolver.b';$uc=q'ni:/lib/slice';$vc=q'ni:/lib/slice.b';$wc=q'ni:/lib/slice.c';$xc=q'ni:/lib/slice_init.b';$yc=q'ni:/lib/slice_serialize.b';$zc=q'ni:/lib/subclass.b';$Ac=q'ni:/lib/tag';$Bc=q'ni:/lib/tag.b';$Cc=q'ni:/lib/tag.c';$Dc=q'ni:/lib/tag_init.b';$Ec=q'ni:/metaclass';$Fc=q'ni:/metaclass.c';$Gc=q'ni:/object';$Hc=q'ni:/object.c';$Ic=q'ni:/unix/cat';$Jc={$s,1};$Kc=q'/unix/pipeline.c';$Lc={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$Kc,1,$z,1};$Mc=[$y3];$Nc=bless({$c,$Lc,$f,$w,$g,$Mc},$E);$Oc=q'/metaclass::ctors';$Pc=[$Nc];$Qc=bless({$c,$Jc,$f,$s,$g,$Pc},$E);$Rc=q'/metaclass::ctors';$Sc={$W,1};$Tc={$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1,$e1,1,$g1,1};$Uc={};$Vc=q'into';$Wc=[];$Xc=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Yc=bless({$W1,$Wc,$v1,$Xc},$S);$Zc={$Vc,$Yc};$cd=q'/unix/io_stream.b';$dd=bless({$c,$Uc,$j1,$k1,$l1,$k1,$m1,$Zc,$f,$cd},$K);$ed=q'/lib/slice::ctors';$fd={};$gd=q'(+';$hd=[];$id=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$jd=bless({$W1,$hd,$v1,$id},$S);$kd={$gd,$jd};$ld=q'/unix/io_constructors.b';$md=bless({$c,$fd,$j1,$k1,$l1,$k1,$m1,$kd,$f,$ld},$K);$nd=q'/lib/slice::ctors';$od={};$pd=q'(<>';$qd=[];$rd=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$sd=bless({$W1,$qd,$v1,$rd},$S);$td=q'(@{}';$ud=[];$vd=q'my $self = shift; [<$self>]';$wd=bless({$W1,$ud,$v1,$vd},$S);$xd={$pd,$sd,$td,$wd};$yd=q'/unix/io_readers.b';$zd=bless({$c,$od,$j1,$k1,$l1,$k1,$m1,$xd,$f,$yd},$K);$Ad=q'/lib/slice::ctors';$Bd=[$r2,$dd,$md,$zd];$Cd=bless({$c,$Tc,$f,$c1,$g,$Bd},$w);$Dd=q'/unix/io.c::ctors';$Ed={};$Fd=[];$Gd=q'shift; +{fs => [@_]}';$Hd=bless({$W1,$Fd,$v1,$Gd},$S);$Id={$N1,$Hd};$Jd=q'/unix/cat_init.b';$Kd=bless({$c,$Ed,$j1,$k1,$l1,$k1,$m1,$Id,$f,$Jd},$K);$Ld=q'/lib/slice::ctors';$Md={};$Nd=q'read';$Od=[];$Pd=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$Qd=bless({$W1,$Od,$v1,$Pd},$S);$Rd={$Nd,$Qd};$Sd=q'/unix/cat_read.b';$Td=bless({$c,$Md,$j1,$k1,$l1,$k1,$m1,$Rd,$f,$Sd},$K);$Ud=q'/lib/slice::ctors';$Vd=[$Cd,$Kd,$Td];$Wd=bless({$c,$Sc,$f,$W,$g,$Vd},$s);$Xd=q'/unix/cat.c::ctors';$Yd=q'ni:/unix/cat.c';$Zd=q'ni:/unix/cat_init.b';$ce=q'ni:/unix/cat_read.b';$de=q'ni:/unix/fd';$ee={$t,1};$fe=[$Nc];$ge=bless({$c,$ee,$f,$t,$g,$fe},$E);$he=q'/metaclass::ctors';$ie={$X,1};$je={};$ke=q'fd';$le=[];$me=q'shift->{\'fd\'}';$ne=bless({$W1,$le,$v1,$me},$S);$oe={$ke,$ne};$pe=q'/unix/fd_readers.b';$qe=bless({$c,$je,$j1,$k1,$l1,$k1,$m1,$oe,$f,$pe},$K);$re=q'/lib/slice::ctors';$se={};$te=[];$ue=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$ve=bless({$W1,$te,$v1,$ue},$S);$we={$N1,$ve};$xe=q'/unix/fd_init.b';$ye=bless({$c,$se,$j1,$k1,$l1,$k1,$m1,$we,$f,$xe},$K);$ze=q'/lib/slice::ctors';$Ae={};$Be=q'move_to';$Ce=[];$De=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$Ee=bless({$W1,$Ce,$v1,$De},$S);$Fe={$Be,$Ee};$Ge=q'/unix/fd_shell.b';$He=bless({$c,$Ae,$j1,$k1,$l1,$k1,$m1,$Fe,$f,$Ge},$K);$Ie=q'/lib/slice::ctors';$Je={$X,1,$Y,1,$Z,1,$d1,1,$e1,1};$Ke=q'/unix/has_fd.b';$Le={};$Me=[];$Ne=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$Oe=bless({$W1,$Me,$v1,$Ne},$S);$Pe=[];$Qe=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$Re=bless({$W1,$Pe,$v1,$Qe},$S);$Se={$Nd,$Oe,$Fa,$Re};$Te=q'/unix/fd_safeio.b';$Ue=bless({$c,$Le,$j1,$k1,$l1,$k1,$m1,$Se,$f,$Te},$K);$Ve=q'/lib/slice::ctors';$We=[$Ue];$Xe=bless({$c,$Je,$f,$Ke,$g,$We},$N);$Ye=q'/lib/branch::ctors';$Ze={};$cf=q'read_fh';$df=[];$ef=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$ff=bless({$W1,$df,$v1,$ef},$S);$gf=q'write_fh';$hf=[];$if=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$jf=bless({$W1,$hf,$v1,$if},$S);$kf={$cf,$ff,$gf,$jf};$lf=q'/unix/fd_io.b';$mf=bless({$c,$Ze,$j1,$k1,$l1,$k1,$m1,$kf,$f,$lf},$K);$nf=q'/lib/slice::ctors';$of=[$Cd,$qe,$ye,$He,$Xe,$mf];$pf=bless({$c,$ie,$f,$X,$g,$of},$t);$qf=q'/unix/fd.c::ctors';$rf=q'ni:/unix/fd.c';$sf=q'ni:/unix/fd_init.b';$tf=q'ni:/unix/fd_io.b';$uf=q'ni:/unix/fd_readers.b';$vf=q'ni:/unix/fd_safeio.b';$wf=q'ni:/unix/fd_shell.b';$xf=q'ni:/unix/fifo';$yf={$u,1};$zf=[$Nc];$Af=bless({$c,$yf,$f,$u,$g,$zf},$E);$Bf=q'/metaclass::ctors';$Cf={$Y,1};$Df={};$Ef=[];$Ff=q'shift->{\'read_fh\'}';$Gf=bless({$W1,$Ef,$v1,$Ff},$S);$Hf=[];$If=q'shift->{\'write_fh\'}';$Jf=bless({$W1,$Hf,$v1,$If},$S);$Kf={$cf,$Gf,$gf,$Jf};$Lf=q'/unix/fifo_io.b';$Mf=bless({$c,$Df,$j1,$k1,$l1,$k1,$m1,$Kf,$f,$Lf},$K);$Nf=q'/lib/slice::ctors';$Of={};$Pf=[];$Qf=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$Rf=bless({$W1,$Pf,$v1,$Qf},$S);$Sf={$N1,$Rf};$Tf=q'/unix/fifo_init.b';$Uf=bless({$c,$Of,$j1,$k1,$l1,$k1,$m1,$Sf,$f,$Tf},$K);$Vf=q'/lib/slice::ctors';$Wf={};$Xf=q'read_side';$Yf=[];$Zf=q'my $self = shift; close $$self{write_fh}; $self';$cg=bless({$W1,$Yf,$v1,$Zf},$S);$dg=q'write_side';$eg=[];$fg=q'my $self = shift; close $$self{read_fh};  $self';$gg=bless({$W1,$eg,$v1,$fg},$S);$hg={$Xf,$cg,$dg,$gg};$ig=q'/unix/fifo_direction.b';$jg=bless({$c,$Wf,$j1,$k1,$l1,$k1,$m1,$hg,$f,$ig},$K);$kg=q'/lib/slice::ctors';$lg=[$Cd,$Mf,$Uf,$Xe,$jg];$mg=bless({$c,$Cf,$f,$Y,$g,$lg},$u);$ng=q'/unix/fifo.c::ctors';$og=q'ni:/unix/fifo.c';$pg=q'ni:/unix/fifo_direction.b';$qg=q'ni:/unix/fifo_init.b';$rg=q'ni:/unix/fifo_io.b';$sg=q'ni:/unix/file';$tg={$v,1};$ug=[$Nc];$vg=bless({$c,$tg,$f,$v,$g,$ug},$E);$wg=q'/metaclass::ctors';$xg={$Z,1};$yg={};$zg=[];$Ag=q'shift->{\'name\'}';$Bg=bless({$W1,$zg,$v1,$Ag},$S);$Cg={$f,$Bg};$Dg=q'/unix/file_readers.b';$Eg=bless({$c,$yg,$j1,$k1,$l1,$k1,$m1,$Cg,$f,$Dg},$K);$Fg=q'/lib/slice::ctors';$Gg={};$Hg=[];$Ig=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$Jg=bless({$W1,$Hg,$v1,$Ig},$S);$Kg={$N1,$Jg};$Lg=q'/unix/file_init.b';$Mg=bless({$c,$Gg,$j1,$k1,$l1,$k1,$m1,$Kg,$f,$Lg},$K);$Ng=q'/lib/slice::ctors';$Og={};$Pg=[];$Qg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$Rg=bless({$W1,$Pg,$v1,$Qg},$S);$Sg=[];$Tg=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Ug=bless({$W1,$Sg,$v1,$Tg},$S);$Vg={$cf,$Rg,$gf,$Ug};$Wg=q'/unix/file_io.b';$Xg=bless({$c,$Og,$j1,$k1,$l1,$k1,$m1,$Vg,$f,$Wg},$K);$Yg=q'/lib/slice::ctors';$Zg=[$Cd,$Eg,$Mg,$Xe,$Xg];$ch=bless({$c,$xg,$f,$Z,$g,$Zg},$v);$dh=q'/unix/file.c::ctors';$eh=q'ni:/unix/file.c';$fh=q'ni:/unix/file_init.b';$gh=q'ni:/unix/file_io.b';$hh=q'ni:/unix/file_readers.b';$ih=q'ni:/unix/has_fd.b';$jh=q'ni:/unix/io';$kh=q'ni:/unix/io.c';$lh=q'ni:/unix/io_constructors.b';$mh=q'ni:/unix/io_readers.b';$nh=q'ni:/unix/io_stream.b';$oh=q'ni:/unix/pid';$ph={$x,1};$qh=[$Nc];$rh=bless({$c,$ph,$f,$x,$g,$qh},$E);$sh=q'/metaclass::ctors';$th={$d1,1};$uh={};$vh=q'pid';$wh=[];$xh=q'shift->{\'pid\'}';$yh=bless({$W1,$wh,$v1,$xh},$S);$zh=q'stderr';$Ah=[];$Bh=q'shift->{\'stderr\'}';$Ch=bless({$W1,$Ah,$v1,$Bh},$S);$Dh=q'stdin';$Eh=[];$Fh=q'shift->{\'stdin\'}';$Gh=bless({$W1,$Eh,$v1,$Fh},$S);$Hh=q'stdout';$Ih=[];$Jh=q'shift->{\'stdout\'}';$Kh=bless({$W1,$Ih,$v1,$Jh},$S);$Lh={$vh,$yh,$zh,$Ch,$Dh,$Gh,$Hh,$Kh};$Mh=q'/unix/pid_readers.b';$Nh=bless({$c,$uh,$j1,$k1,$l1,$k1,$m1,$Lh,$f,$Mh},$K);$Oh=q'/lib/slice::ctors';$Ph={};$Qh=[];$Rh=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$Sh=bless({$W1,$Qh,$v1,$Rh},$S);$Th={$N1,$Sh};$Uh=q'/unix/pid_init.b';$Vh=bless({$c,$Ph,$j1,$k1,$l1,$k1,$m1,$Th,$f,$Uh},$K);$Wh=q'/lib/slice::ctors';$Xh={};$Yh={};$Zh=q'/unix/pid_wait.b';$ci=bless({$c,$Xh,$j1,$k1,$l1,$k1,$m1,$Yh,$f,$Zh},$K);$di=q'/lib/slice::ctors';$ei={};$fi=[];$gi=q'shift->{stdout}->read_fh';$hi=bless({$W1,$fi,$v1,$gi},$S);$ii=[];$ji=q'shift->{stdin}->write_fh';$ki=bless({$W1,$ii,$v1,$ji},$S);$li={$cf,$hi,$gf,$ki};$mi=q'/unix/pid_io.b';$ni=bless({$c,$ei,$j1,$k1,$l1,$k1,$m1,$li,$f,$mi},$K);$oi=q'/lib/slice::ctors';$pi=[$Cd,$Nh,$Vh,$ci,$Xe,$ni];$qi=bless({$c,$th,$f,$d1,$g,$pi},$x);$ri=q'/unix/pid.c::ctors';$si=q'ni:/unix/pid.c';$ti=q'ni:/unix/pid_init.b';$ui=q'ni:/unix/pid_io.b';$vi=q'ni:/unix/pid_readers.b';$wi=q'ni:/unix/pid_wait.b';$xi=q'ni:/unix/pipeline';$yi=q'/unix/pipeline.c';$zi={$yi,1};$Ai=q'/unix/pipeline.c';$Bi=[$Nc];$Ci=bless({$c,$zi,$f,$Ai,$g,$Bi},$E);$Di=q'/metaclass::ctors';$Ei={$e1,1};$Fi={};$Gi=[];$Hi=q'shift->{\'stdin\'}';$Ii=bless({$W1,$Gi,$v1,$Hi},$S);$Ji=[];$Ki=q'shift->{\'stdout\'}';$Li=bless({$W1,$Ji,$v1,$Ki},$S);$Mi={$Dh,$Ii,$Hh,$Li};$Ni=q'/unix/pipeline_ro.b';$Oi=bless({$c,$Fi,$j1,$k1,$l1,$k1,$m1,$Mi,$f,$Ni},$K);$Pi=q'/lib/slice::ctors';$Qi={};$Ri=[];$Si=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$Ti=bless({$W1,$Ri,$v1,$Si},$S);$Ui={$N1,$Ti};$Vi=q'/unix/pipeline_init.b';$Wi=bless({$c,$Qi,$j1,$k1,$l1,$k1,$m1,$Ui,$f,$Vi},$K);$Xi=q'/lib/slice::ctors';$Yi={};$Zi=q'async_step';$cj=[];$dj=q'local $_;
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
$self;';$ej=bless({$W1,$cj,$v1,$dj},$S);$fj={$Zi,$ej};$gj=q'/unix/pipeline_async.b';$hj=bless({$c,$Yi,$j1,$k1,$l1,$k1,$m1,$fj,$f,$gj},$K);$ij=q'/lib/slice::ctors';$jj={};$kj=[];$lj=q'shift->{stdout}->read_fh';$mj=bless({$W1,$kj,$v1,$lj},$S);$nj=[];$oj=q'shift->{stdin}->write_fh';$pj=bless({$W1,$nj,$v1,$oj},$S);$qj={$cf,$mj,$gf,$pj};$rj=q'/unix/pipeline_io.b';$sj=bless({$c,$jj,$j1,$k1,$l1,$k1,$m1,$qj,$f,$rj},$K);$tj=q'/lib/slice::ctors';$uj=[$Cd,$Oi,$Wi,$hj,$Xe,$sj];$vj=q'/unix/pipeline.c';$wj=bless({$c,$Ei,$f,$e1,$g,$uj},$vj);$xj=q'/unix/pipeline.c::ctors';$yj=q'ni:/unix/pipeline.c';$zj=q'ni:/unix/pipeline_async.b';$Aj=q'ni:/unix/pipeline_init.b';$Bj=q'ni:/unix/pipeline_io.b';$Cj=q'ni:/unix/pipeline_ro.b';$Dj=q'ni:/unix/str';$Ej={$z,1};$Fj=[$Nc];$Gj=bless({$c,$Ej,$f,$z,$g,$Fj},$E);$Hj=q'/metaclass::ctors';$Ij={$g1,1};$Jj={};$Kj=[];$Lj=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$Mj=bless({$W1,$Kj,$v1,$Lj},$S);$Nj={$N1,$Mj};$Oj=q'/unix/str_init.b';$Pj=bless({$c,$Jj,$j1,$k1,$l1,$k1,$m1,$Nj,$f,$Oj},$K);$Qj=q'/lib/slice::ctors';$Rj={};$Sj=[];$Tj=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Uj=bless({$W1,$Sj,$v1,$Tj},$S);$Vj=q'remaining';$Wj=[];$Xj=q'my $self = shift; $$self{end} - $$self{start}';$Yj=bless({$W1,$Wj,$v1,$Xj},$S);$Zj=[];$ck=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$dk=bless({$W1,$Zj,$v1,$ck},$S);$ek={$Nd,$Uj,$Vj,$Yj,$Fa,$dk};$fk=q'/unix/str_io.b';$gk=bless({$c,$Rj,$j1,$k1,$l1,$k1,$m1,$ek,$f,$fk},$K);$hk=q'/lib/slice::ctors';$ik=[$Cd,$Pj,$gk];$jk=bless({$c,$Ij,$f,$g1,$g,$ik},$z);$kk=q'/unix/str.c::ctors';$lk=q'ni:/unix/str.c';$mk=q'ni:/unix/str_init.b';$nk=q'ni:/unix/str_io.b';$ok={$V7,$A8,$x8,$h6,$C8,$B3,$D8,$P5,$E8,$u2,$F8,$F,$G8,$s5,$H8,$m4,$I8,$h5,$J8,$p5,$K8,$c5,$L8,$D5,$M8,$T5,$N8,$u8,$O8,$Y7,$P8,$j8,$Q8,$r8,$R8,$u3,$S8,$h2,$T8,$q1,$U8,$S1,$V8,$e2,$W8,$Oa,$Qa,$Z8,$Ra,$k9,$Sa,$La,$Ta,$n2,$Ua,$E1,$Va,$D2,$Wa,$v4,$Xa,$E4,$Ya,$S7,$Za,$s6,$cb,$T6,$db,$q7,$eb,$P7,$fb,$C7,$gb,$G6,$hb,$pc,$rc,$mc,$sc,$R4,$tc,$N4,$uc,$l3,$vc,$R2,$wc,$I,$xc,$Y2,$yc,$i3,$zc,$d6,$Ac,$Z3,$Bc,$P3,$Cc,$H3,$Dc,$W3,$Ec,$o6,$Fc,$k6,$Gc,$r2,$Hc,$y3,$Ic,$Wd,$Yd,$Qc,$Zd,$Kd,$ce,$Td,$de,$pf,$rf,$ge,$sf,$ye,$tf,$mf,$uf,$qe,$vf,$Ue,$wf,$He,$xf,$mg,$og,$Af,$pg,$jg,$qg,$Uf,$rg,$Mf,$sg,$ch,$eh,$vg,$fh,$Mg,$gh,$Xg,$hh,$Eg,$ih,$Xe,$jh,$Cd,$kh,$Nc,$lh,$md,$mh,$zd,$nh,$dd,$oh,$qi,$si,$rh,$ti,$Vh,$ui,$ni,$vi,$Nh,$wi,$ci,$xi,$wj,$yj,$Ci,$zj,$hj,$Aj,$Wi,$Bj,$sj,$Cj,$Oi,$Dj,$jk,$lk,$Gj,$mk,$Pj,$nk,$gk};$pk=q'resolvers';$qk=[];$rk=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$sk=bless({$W1,$qk,$v1,$rk},$S);$tk=q'file';$uk=[];$vk=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$wk=bless({$W1,$uk,$v1,$vk},$S);$xk=q'str';$yk=[];$zk=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$Ak=bless({$W1,$yk,$v1,$zk},$S);$Bk={$ke,$sk,$tk,$wk,$xk,$Ak};$Ck=bless({$U7,$ok,$pk,$Bk},$U);$Dk=q'/lib/ni::ctors';$$x3[0]=$h6;$$D[0]=$y3;$$p1[0]=$y3;$$g2[0]=$r2;$$r5[4]=$T5;*$L2=\&$J2;*$K2=\&$H2;$E1->apply_unsafe($Q);$E1->apply_unsafe($h);$E1->apply_unsafe($j);$E1->apply_unsafe($N);$E1->apply_unsafe($k);$E1->apply_unsafe($l);$E1->apply_unsafe($S);$E1->apply_unsafe($m);$E1->apply_unsafe($n);$E1->apply_unsafe($o);$E1->apply_unsafe($K);$E1->apply_unsafe($p);$E1->apply_unsafe($O);$E1->apply_unsafe($q);$E1->apply_unsafe($E);$E1->apply_unsafe($d);$E1->apply_unsafe($r);$E1->apply_unsafe($s);$E1->apply_unsafe($t);$E1->apply_unsafe($u);$E1->apply_unsafe($v);$E1->apply_unsafe($w);$E1->apply_unsafe($x);$E1->apply_unsafe($F1);$E1->apply_unsafe($z);$S1->apply_unsafe($S);$e2->apply_unsafe($S);$n2->apply_unsafe($Q);$n2->apply_unsafe($h);$n2->apply_unsafe($M);$n2->apply_unsafe($j);$n2->apply_unsafe($N);$n2->apply_unsafe($k);$n2->apply_unsafe($R);$n2->apply_unsafe($l);$n2->apply_unsafe($S);$n2->apply_unsafe($m);$n2->apply_unsafe($T);$n2->apply_unsafe($n);$n2->apply_unsafe($U);$n2->apply_unsafe($o);$n2->apply_unsafe($K);$n2->apply_unsafe($p);$n2->apply_unsafe($O);$n2->apply_unsafe($q);$n2->apply_unsafe($E);$n2->apply_unsafe($d);$n2->apply_unsafe($V);$n2->apply_unsafe($r);$n2->apply_unsafe($W);$n2->apply_unsafe($s);$n2->apply_unsafe($X);$n2->apply_unsafe($t);$n2->apply_unsafe($Y);$n2->apply_unsafe($u);$n2->apply_unsafe($Z);$n2->apply_unsafe($v);$n2->apply_unsafe($c1);$n2->apply_unsafe($w);$n2->apply_unsafe($d1);$n2->apply_unsafe($x);$n2->apply_unsafe($e1);$n2->apply_unsafe($o2);$n2->apply_unsafe($g1);$n2->apply_unsafe($z);$D2->apply_unsafe($Q);$D2->apply_unsafe($h);$D2->apply_unsafe($j);$D2->apply_unsafe($N);$D2->apply_unsafe($k);$D2->apply_unsafe($R);$D2->apply_unsafe($l);$D2->apply_unsafe($m);$D2->apply_unsafe($n);$D2->apply_unsafe($o);$D2->apply_unsafe($K);$D2->apply_unsafe($p);$D2->apply_unsafe($O);$D2->apply_unsafe($q);$D2->apply_unsafe($E);$D2->apply_unsafe($d);$D2->apply_unsafe($r);$D2->apply_unsafe($s);$D2->apply_unsafe($t);$D2->apply_unsafe($u);$D2->apply_unsafe($v);$D2->apply_unsafe($w);$D2->apply_unsafe($x);$D2->apply_unsafe($E2);$D2->apply_unsafe($z);$R2->apply_unsafe($K);$Y2->apply_unsafe($K);$i3->apply_unsafe($K);$u3->apply_unsafe($h);$u3->apply_unsafe($j);$u3->apply_unsafe($k);$u3->apply_unsafe($l);$u3->apply_unsafe($m);$u3->apply_unsafe($n);$u3->apply_unsafe($o);$u3->apply_unsafe($p);$u3->apply_unsafe($q);$u3->apply_unsafe($r);$u3->apply_unsafe($s);$u3->apply_unsafe($t);$u3->apply_unsafe($u);$u3->apply_unsafe($v);$u3->apply_unsafe($w);$u3->apply_unsafe($x);$u3->apply_unsafe($v3);$u3->apply_unsafe($z);$P3->apply_unsafe($O);$W3->apply_unsafe($O);$m4->apply_unsafe($Q);$m4->apply_unsafe($h);$m4->apply_unsafe($j);$m4->apply_unsafe($N);$m4->apply_unsafe($k);$m4->apply_unsafe($l);$m4->apply_unsafe($m);$m4->apply_unsafe($n);$m4->apply_unsafe($o);$m4->apply_unsafe($p);$m4->apply_unsafe($q);$m4->apply_unsafe($E);$m4->apply_unsafe($d);$m4->apply_unsafe($r);$m4->apply_unsafe($s);$m4->apply_unsafe($t);$m4->apply_unsafe($u);$m4->apply_unsafe($v);$m4->apply_unsafe($w);$m4->apply_unsafe($x);$m4->apply_unsafe($n4);$m4->apply_unsafe($z);$v4->apply_unsafe($Q);$v4->apply_unsafe($h);$v4->apply_unsafe($j);$v4->apply_unsafe($N);$v4->apply_unsafe($k);$v4->apply_unsafe($l);$v4->apply_unsafe($m);$v4->apply_unsafe($n);$v4->apply_unsafe($o);$v4->apply_unsafe($K);$v4->apply_unsafe($p);$v4->apply_unsafe($O);$v4->apply_unsafe($q);$v4->apply_unsafe($E);$v4->apply_unsafe($d);$v4->apply_unsafe($r);$v4->apply_unsafe($s);$v4->apply_unsafe($t);$v4->apply_unsafe($u);$v4->apply_unsafe($v);$v4->apply_unsafe($w);$v4->apply_unsafe($x);$v4->apply_unsafe($w4);$v4->apply_unsafe($z);$E4->apply_unsafe($Q);$E4->apply_unsafe($h);$E4->apply_unsafe($j);$E4->apply_unsafe($N);$E4->apply_unsafe($k);$E4->apply_unsafe($l);$E4->apply_unsafe($m);$E4->apply_unsafe($n);$E4->apply_unsafe($o);$E4->apply_unsafe($K);$E4->apply_unsafe($p);$E4->apply_unsafe($O);$E4->apply_unsafe($q);$E4->apply_unsafe($E);$E4->apply_unsafe($d);$E4->apply_unsafe($r);$E4->apply_unsafe($s);$E4->apply_unsafe($t);$E4->apply_unsafe($u);$E4->apply_unsafe($v);$E4->apply_unsafe($w);$E4->apply_unsafe($x);$E4->apply_unsafe($F4);$E4->apply_unsafe($z);$N4->apply_unsafe($Q);$N4->apply_unsafe($h);$N4->apply_unsafe($j);$N4->apply_unsafe($N);$N4->apply_unsafe($k);$N4->apply_unsafe($l);$N4->apply_unsafe($m);$N4->apply_unsafe($n);$N4->apply_unsafe($o);$N4->apply_unsafe($p);$N4->apply_unsafe($O);$N4->apply_unsafe($q);$N4->apply_unsafe($E);$N4->apply_unsafe($d);$N4->apply_unsafe($r);$N4->apply_unsafe($s);$N4->apply_unsafe($t);$N4->apply_unsafe($u);$N4->apply_unsafe($v);$N4->apply_unsafe($w);$N4->apply_unsafe($x);$N4->apply_unsafe($O4);$N4->apply_unsafe($z);$c5->apply_unsafe($Q);$c5->apply_unsafe($h);$c5->apply_unsafe($j);$c5->apply_unsafe($k);$c5->apply_unsafe($l);$c5->apply_unsafe($m);$c5->apply_unsafe($n);$c5->apply_unsafe($o);$c5->apply_unsafe($p);$c5->apply_unsafe($q);$c5->apply_unsafe($E);$c5->apply_unsafe($d);$c5->apply_unsafe($r);$c5->apply_unsafe($s);$c5->apply_unsafe($t);$c5->apply_unsafe($u);$c5->apply_unsafe($v);$c5->apply_unsafe($w);$c5->apply_unsafe($x);$c5->apply_unsafe($d5);$c5->apply_unsafe($z);$p5->apply_unsafe($N);$D5->apply_unsafe($Q);$D5->apply_unsafe($h);$D5->apply_unsafe($j);$D5->apply_unsafe($N);$D5->apply_unsafe($k);$D5->apply_unsafe($l);$D5->apply_unsafe($m);$D5->apply_unsafe($n);$D5->apply_unsafe($o);$D5->apply_unsafe($p);$D5->apply_unsafe($q);$D5->apply_unsafe($E);$D5->apply_unsafe($d);$D5->apply_unsafe($r);$D5->apply_unsafe($s);$D5->apply_unsafe($t);$D5->apply_unsafe($u);$D5->apply_unsafe($v);$D5->apply_unsafe($w);$D5->apply_unsafe($x);$D5->apply_unsafe($E5);$D5->apply_unsafe($z);$P5->apply_unsafe($Q);$P5->apply_unsafe($h);$P5->apply_unsafe($j);$P5->apply_unsafe($N);$P5->apply_unsafe($k);$P5->apply_unsafe($l);$P5->apply_unsafe($m);$P5->apply_unsafe($n);$P5->apply_unsafe($o);$P5->apply_unsafe($p);$P5->apply_unsafe($q);$P5->apply_unsafe($E);$P5->apply_unsafe($d);$P5->apply_unsafe($r);$P5->apply_unsafe($s);$P5->apply_unsafe($t);$P5->apply_unsafe($u);$P5->apply_unsafe($v);$P5->apply_unsafe($w);$P5->apply_unsafe($x);$P5->apply_unsafe($Q5);$P5->apply_unsafe($z);$d6->apply_unsafe($Q);$d6->apply_unsafe($h);$d6->apply_unsafe($j);$d6->apply_unsafe($k);$d6->apply_unsafe($l);$d6->apply_unsafe($m);$d6->apply_unsafe($n);$d6->apply_unsafe($o);$d6->apply_unsafe($p);$d6->apply_unsafe($q);$d6->apply_unsafe($d);$d6->apply_unsafe($r);$d6->apply_unsafe($s);$d6->apply_unsafe($t);$d6->apply_unsafe($u);$d6->apply_unsafe($v);$d6->apply_unsafe($w);$d6->apply_unsafe($x);$d6->apply_unsafe($e6);$d6->apply_unsafe($z);$G6->apply_unsafe($U);$T6->apply_unsafe($U);$q7->apply_unsafe($U);$C7->apply_unsafe($U);$P7->apply_unsafe($U);$j8->apply_unsafe($R);$r8->apply_unsafe($R);$k9->apply_unsafe($T);$La->apply_unsafe($T);$mc->apply_unsafe($ib);$mc->apply_unsafe($jb);$dd->apply_unsafe($W);$dd->apply_unsafe($X);$dd->apply_unsafe($Y);$dd->apply_unsafe($Z);$dd->apply_unsafe($c1);$dd->apply_unsafe($d1);$dd->apply_unsafe($e1);$dd->apply_unsafe($g1);$md->apply_unsafe($W);$md->apply_unsafe($X);$md->apply_unsafe($Y);$md->apply_unsafe($Z);$md->apply_unsafe($c1);$md->apply_unsafe($d1);$md->apply_unsafe($e1);$md->apply_unsafe($g1);$zd->apply_unsafe($W);$zd->apply_unsafe($X);$zd->apply_unsafe($Y);$zd->apply_unsafe($Z);$zd->apply_unsafe($c1);$zd->apply_unsafe($d1);$zd->apply_unsafe($e1);$zd->apply_unsafe($g1);$Kd->apply_unsafe($W);$Td->apply_unsafe($W);$qe->apply_unsafe($X);$ye->apply_unsafe($X);$He->apply_unsafe($X);$Ue->apply_unsafe($X);$Ue->apply_unsafe($Y);$Ue->apply_unsafe($Z);$Ue->apply_unsafe($d1);$Ue->apply_unsafe($e1);$mf->apply_unsafe($X);$Mf->apply_unsafe($Y);$Uf->apply_unsafe($Y);$jg->apply_unsafe($Y);$Eg->apply_unsafe($Z);$Mg->apply_unsafe($Z);$Xg->apply_unsafe($Z);$Nh->apply_unsafe($d1);$Vh->apply_unsafe($d1);$ci->apply_unsafe($d1);$ni->apply_unsafe($d1);$Oi->apply_unsafe($e1);$Wi->apply_unsafe($e1);$hj->apply_unsafe($e1);$sj->apply_unsafe($e1);$Pj->apply_unsafe($g1);$gk->apply_unsafe($g1);$ni::self=$Ck;&$_($F)for@$G;&$_($I)for@$J;&$_($q1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$y1;&$_($E1)for@$G1;&$_($J1)for@$y1;&$_($M1)for@$y1;&$_($P1)for@$y1;&$_($S1)for@$T1;&$_($Z1)for@$y1;&$_($e2)for@$f2;&$_($h2)for@$i2;&$_($k2)for@$y1;&$_($n2)for@$p2;&$_($r2)for@$s2;&$_($u2)for@$v2;&$_($y2)for@$y1;&$_($A2)for@$y1;&$_($D2)for@$F2;&$_($H2)for@$y1;&$_($J2)for@$y1;&$_($R2)for@$S2;&$_($V2)for@$y1;&$_($Y2)for@$Z2;&$_($f3)for@$y1;&$_($i3)for@$j3;&$_($l3)for@$m3;&$_($r3)for@$y1;&$_($u3)for@$w3;&$_($y3)for@$z3;&$_($B3)for@$C3;&$_($H3)for@$I3;&$_($M3)for@$y1;&$_($P3)for@$Q3;&$_($T3)for@$y1;&$_($W3)for@$X3;&$_($Z3)for@$c4;&$_($h4)for@$y1;&$_($j4)for@$y1;&$_($m4)for@$o4;&$_($s4)for@$y1;&$_($v4)for@$x4;&$_($B4)for@$y1;&$_($E4)for@$G4;&$_($K4)for@$y1;&$_($N4)for@$P4;&$_($R4)for@$S4;&$_($V4)for@$y1;&$_($X4)for@$y1;&$_($c5)for@$e5;&$_($h5)for@$i5;&$_($m5)for@$y1;&$_($p5)for@$q5;&$_($s5)for@$t5;&$_($A5)for@$y1;&$_($D5)for@$F5;&$_($J5)for@$y1;&$_($M5)for@$y1;&$_($P5)for@$R5;&$_($T5)for@$U5;&$_($Y5)for@$y1;&$_($d6)for@$f6;&$_($h6)for@$i6;&$_($k6)for@$l6;&$_($o6)for@$p6;&$_($s6)for@$t6;&$_($z6)for@$y1;&$_($D6)for@$y1;&$_($G6)for@$H6;&$_($M6)for@$y1;&$_($Q6)for@$y1;&$_($T6)for@$U6;&$_($Z6)for@$y1;&$_($f7)for@$y1;&$_($j7)for@$y1;&$_($n7)for@$y1;&$_($q7)for@$r7;&$_($v7)for@$y1;&$_($z7)for@$y1;&$_($C7)for@$D7;&$_($I7)for@$y1;&$_($M7)for@$y1;&$_($P7)for@$Q7;&$_($S7)for@$T7;&$_($Y7)for@$Z7;&$_($g8)for@$y1;&$_($j8)for@$k8;&$_($o8)for@$y1;&$_($r8)for@$s8;&$_($u8)for@$v8;&$_($A8)for@$B8;&$_($Z8)for@$c9;&$_($h9)for@$y1;&$_($k9)for@$l9;&$_($q9)for@$y1;&$_($u9)for@$y1;&$_($y9)for@$y1;&$_($C9)for@$y1;&$_($G9)for@$y1;&$_($K9)for@$y1;&$_($O9)for@$y1;&$_($S9)for@$y1;&$_($W9)for@$y1;&$_($ca)for@$y1;&$_($ga)for@$y1;&$_($ka)for@$y1;&$_($oa)for@$y1;&$_($sa)for@$y1;&$_($wa)for@$y1;&$_($Aa)for@$y1;&$_($Ea)for@$y1;&$_($Ia)for@$y1;&$_($La)for@$Ma;&$_($Oa)for@$Pa;&$_($pb)for@$y1;&$_($tb)for@$y1;&$_($xb)for@$y1;&$_($Bb)for@$y1;&$_($Fb)for@$y1;&$_($Jb)for@$y1;&$_($Nb)for@$y1;&$_($Rb)for@$y1;&$_($Vb)for@$y1;&$_($Zb)for@$y1;&$_($fc)for@$y1;&$_($jc)for@$y1;&$_($mc)for@$nc;&$_($pc)for@$qc;&$_($Nc)for@$Oc;&$_($Qc)for@$Rc;&$_($Yc)for@$y1;&$_($dd)for@$ed;&$_($jd)for@$y1;&$_($md)for@$nd;&$_($sd)for@$y1;&$_($wd)for@$y1;&$_($zd)for@$Ad;&$_($Cd)for@$Dd;&$_($Hd)for@$y1;&$_($Kd)for@$Ld;&$_($Qd)for@$y1;&$_($Td)for@$Ud;&$_($Wd)for@$Xd;&$_($ge)for@$he;&$_($ne)for@$y1;&$_($qe)for@$re;&$_($ve)for@$y1;&$_($ye)for@$ze;&$_($Ee)for@$y1;&$_($He)for@$Ie;&$_($Oe)for@$y1;&$_($Re)for@$y1;&$_($Ue)for@$Ve;&$_($Xe)for@$Ye;&$_($ff)for@$y1;&$_($jf)for@$y1;&$_($mf)for@$nf;&$_($pf)for@$qf;&$_($Af)for@$Bf;&$_($Gf)for@$y1;&$_($Jf)for@$y1;&$_($Mf)for@$Nf;&$_($Rf)for@$y1;&$_($Uf)for@$Vf;&$_($cg)for@$y1;&$_($gg)for@$y1;&$_($jg)for@$kg;&$_($mg)for@$ng;&$_($vg)for@$wg;&$_($Bg)for@$y1;&$_($Eg)for@$Fg;&$_($Jg)for@$y1;&$_($Mg)for@$Ng;&$_($Rg)for@$y1;&$_($Ug)for@$y1;&$_($Xg)for@$Yg;&$_($ch)for@$dh;&$_($rh)for@$sh;&$_($yh)for@$y1;&$_($Ch)for@$y1;&$_($Gh)for@$y1;&$_($Kh)for@$y1;&$_($Nh)for@$Oh;&$_($Sh)for@$y1;&$_($Vh)for@$Wh;&$_($ci)for@$di;&$_($hi)for@$y1;&$_($ki)for@$y1;&$_($ni)for@$oi;&$_($qi)for@$ri;&$_($Ci)for@$Di;&$_($Ii)for@$y1;&$_($Li)for@$y1;&$_($Oi)for@$Pi;&$_($Ti)for@$y1;&$_($Wi)for@$Xi;&$_($ej)for@$y1;&$_($hj)for@$ij;&$_($mj)for@$y1;&$_($pj)for@$y1;&$_($sj)for@$tj;&$_($wj)for@$xj;&$_($Gj)for@$Hj;&$_($Mj)for@$y1;&$_($Pj)for@$Qj;&$_($Uj)for@$y1;&$_($Yj)for@$y1;&$_($dk)for@$y1;&$_($gk)for@$hk;&$_($jk)for@$kk;&$_($sk)for@$y1;&$_($wk)for@$y1;&$_($Ak)for@$y1;&$_($Ck)for@$Dk;ni->run(@ARGV);
__DATA__
