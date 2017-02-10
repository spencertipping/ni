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
  die "ni:/lib/fn failed to compile $$self{code}: $@\n" if $@;
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
$g;';$f3=bless({$W1,$d3,$v1,$e3},$S);$g3={$V1,$f3};$h3=q'/lib/slice_serialize.b';$i3=bless({$c,$c3,$j1,$k1,$l1,$k1,$m1,$g3,$f,$h3},$K);$j3=q'/lib/slice::ctors';$k3=[$u2,$D2,$R2,$Y2,$i3];$l3=bless({$c,$L,$f,$K,$g,$k3},$p);$m3=q'/lib/slice.c::ctors';$n3={};$o3=q'doc';$p3=q'my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can(\'exists\') && ni->exists("ni.doc:$name");
ni(\'ni:/lib/doc\')->new($name);';$q3=bless({$v1,$p3},$S);$r3={$o3,$q3};$s3=q'/lib/documentable.b';$t3=bless({$c,$n3,$j1,$k1,$l1,$k1,$m1,$r3,$f,$s3},$K);$u3=q'/unix/pipeline.c';$v3=q'/lib/slice::ctors';$w3=[undef,$t3];$x3=bless({$c,$A,$f,$r,$g,$w3},$E);$y3=q'/metaclass::ctors';$z3=[$x3];$A3=bless({$c,$i,$f,$h,$g,$z3},$E);$B3=q'/metaclass::ctors';$C3=q'/unix/pipeline.c';$D3={$Q,1,$h,1,$j,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$C3,1,$z,1};$E3={$q,1};$F3=[$F];$G3=bless({$c,$E3,$f,$q,$g,$F3},$E);$H3=q'/metaclass::ctors';$I3={$O,1};$J3={};$K3=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$L3=bless({$v1,$K3},$S);$M3={$N2,$L3};$N3=q'/lib/tag.b';$O3=bless({$c,$J3,$j1,$k1,$l1,$k1,$m1,$M3,$f,$N3},$K);$P3=q'/lib/slice::ctors';$Q3={};$R3=q'local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};';$S3=bless({$v1,$R3},$S);$T3={$N1,$S3};$U3=q'/lib/tag_init.b';$V3=bless({$c,$Q3,$j1,$k1,$l1,$k1,$m1,$T3,$f,$U3},$K);$W3=q'/lib/slice::ctors';$X3=[$u2,$D2,$O3,$V3];$Y3=bless({$c,$I3,$f,$O,$g,$X3},$q);$Z3=q'/lib/tag.c::ctors';$c4=q'/lib/perlbranch.b';$d4={};$e4=q'add';$f4=q'local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;';$g4=bless({$v1,$f4},$S);$h4=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;';$i4=bless({$v1,$h4},$S);$j4={$e4,$g4,$N2,$i4};$k4=q'/lib/branch.b';$l4=bless({$c,$d4,$j1,$k1,$l1,$k1,$m1,$j4,$f,$k4},$K);$m4=q'/unix/pipeline.c';$n4=q'/lib/slice::ctors';$o4={};$p4=q'namespace';$q4=q'\'ni\'';$r4=bless({$v1,$q4},$S);$s4={$p4,$r4};$t4=q'/lib/named_in_ni.b';$u4=bless({$c,$o4,$j1,$k1,$l1,$k1,$m1,$s4,$f,$t4},$K);$v4=q'/unix/pipeline.c';$w4=q'/lib/slice::ctors';$x4={};$y4=q'package';$z4=q'shift->{name}';$A4=bless({$v1,$z4},$S);$B4={$y4,$A4};$C4=q'/lib/namespaced.b';$D4=bless({$c,$x4,$j1,$k1,$l1,$k1,$m1,$B4,$f,$C4},$K);$E4=q'/unix/pipeline.c';$F4=q'/lib/slice::ctors';$G4={};$H4=q'resolve';$I4=q'ref $_[1] ? $_[1] : ni"ni:$_[1]"';$J4=bless({$v1,$I4},$S);$K4={$H4,$J4};$L4=q'/lib/resolver.b';$M4=bless({$c,$G4,$j1,$k1,$l1,$k1,$m1,$K4,$f,$L4},$K);$N4=q'/unix/pipeline.c';$O4=q'/lib/slice::ctors';$P4=[$l4,$E1,$D2,$u4,$D4,$M4];$Q4=bless({$f,$c4,$g,$P4},$O);$R4=q'/lib/tag::ctors';$S4={};$T4=q'my $s = shift; $s->apply($s->package)';$U4=bless({$v1,$T4},$S);$V4=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};';$W4=bless({$v1,$V4},$S);$X4={$N1,$W4};$Y4=q'/lib/class_init.b';$Z4=bless({$c,$S4,$j1,$U4,$l1,$k1,$m1,$X4,$f,$Y4},$K);$c5=q'/unix/pipeline.c';$d5=q'/lib/slice::ctors';$e5={$k,1};$f5=[$F];$g5=bless({$c,$e5,$f,$k,$g,$f5},$E);$h5=q'/metaclass::ctors';$i5={$N,1};$j5={};$k5=q'local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};';$l5=bless({$v1,$k5},$S);$m5={$N1,$l5};$n5=q'/lib/branch_init.b';$o5=bless({$c,$j5,$j1,$k1,$l1,$k1,$m1,$m5,$f,$n5},$K);$p5=q'/lib/slice::ctors';$q5=[$u2,$D2,$l4,$o5,undef];$r5=bless({$c,$i5,$f,$N,$g,$q5},$k);$s5=q'/lib/branch.c::ctors';$t5=q'/unix/pipeline.c';$u5={$Q,1,$h,1,$j,1,$N,1,$k,1,$l,1,$m,1,$n,1,$o,1,$p,1,$q,1,$E,1,$d,1,$r,1,$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$t5,1,$z,1};$v5=q'/lib/definition.b';$w5={};$x5=q'def';$y5=q'shift->add(ni(\'ni:/lib/slice\')->new(@_))';$z5=bless({$v1,$y5},$S);$A5={$x5,$z5};$B5=q'/lib/classdef.b';$C5=bless({$c,$w5,$j1,$k1,$l1,$k1,$m1,$A5,$f,$B5},$K);$D5=q'/unix/pipeline.c';$E5=q'/lib/slice::ctors';$F5={};$G5=q'ro';$H5=q'my ($self, $slice, @rs) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{shift->{\'$_\'}}), @rs));';$I5=bless({$v1,$H5},$S);$J5=q'rw';$K5=q'my ($self, $slice, @as) = @_;
$self->add(ni(\'ni:/lib/slice\')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{\'$_\'} = \\$_[1] : shift->{\'$_\'}}), @as));';$L5=bless({$v1,$K5},$S);$M5={$G5,$I5,$J5,$L5};$N5=q'/lib/accessor.b';$O5=bless({$c,$F5,$j1,$k1,$l1,$k1,$m1,$M5,$f,$N5},$K);$P5=q'/unix/pipeline.c';$Q5=q'/lib/slice::ctors';$R5=[$C5,$O5];$S5=bless({$c,$u5,$f,$v5,$g,$R5},$N);$T5=q'/lib/branch::ctors';$U5={};$V5=q'child';$W5=q'my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);';$X5=bless({$v1,$W5},$S);$Y5={$V5,$X5};$Z5=q'/lib/subclass.b';$c6=bless({$c,$U5,$j1,$k1,$l1,$k1,$m1,$Y5,$f,$Z5},$K);$d6=q'/unix/pipeline.c';$e6=q'/lib/slice::ctors';$f6=[$Q4,$Z4,$r2,$S5,$c6];$g6=bless({$c,$D3,$f,$Q,$g,$f6},$h);$h6=q'/class.c::ctors';$i6=[$g6];$j6=bless({$c,$e,$f,$d,$g,$i6},$E);$k6=q'/metaclass::ctors';$l6={$E,1};$m6=[$Q4,$Z4,$r2,$S5];$n6=bless({$c,$l6,$f,$E,$g,$m6},$d);$o6=q'/metaclass.c::ctors';$p6={$o,1};$q6=[$x3];$r6=bless({$c,$p6,$f,$o,$g,$q6},$E);$s6=q'/metaclass::ctors';$t6={$U,1};$u6={};$v6=q'is_mutable';$w6=[];$x6=q'$0 ne "-" && -w $0';$y6=bless({$W1,$w6,$v1,$x6},$S);$z6=q'modify';$A6=[];$B6=q'my ($self, $fn) = @_;
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
rename $r, $0 or die "ni: failed to rename: $!";';$C6=bless({$W1,$A6,$v1,$B6},$S);$D6={$v6,$y6,$z6,$C6};$E6=q'/lib/ni_self.b';$F6=bless({$c,$u6,$j1,$k1,$l1,$k1,$m1,$D6,$f,$E6},$K);$G6=q'/lib/slice::ctors';$H6={};$I6=q'exists';$J6=[];$K6=q'exists $_[0]->{named}{$_[1]}';$L6=bless({$W1,$J6,$v1,$K6},$S);$M6=q'quoted';$N6=[];$O6=q'my $self = shift;
my $q = ni(\'ni:/lib/image\')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;';$P6=bless({$W1,$N6,$v1,$O6},$S);$Q6={$I6,$L6,$M6,$P6};$R6=q'/lib/ni_image.b';$S6=bless({$c,$H6,$j1,$k1,$l1,$k1,$m1,$Q6,$f,$R6},$K);$T6=q'/lib/slice::ctors';$U6={};$V6=q'--internal/+=';$W6=[];$X6=q'my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
  die "ni: failed to run $_: $!" unless $r;
}
my $q = $self->quoted;
$self->modify(sub {$q->write(shift)});
0;';$Y6=bless({$W1,$W6,$v1,$X6},$S);$Z6=q'--internal/eval';$c7=[];$d7=q'my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$e7=bless({$W1,$c7,$v1,$d7},$S);$f7=q'--internal/image';$g7=[];$h7=q'shift->quoted->write(\\*STDOUT);
0;';$i7=bless({$W1,$g7,$v1,$h7},$S);$j7=q'run';$k7=[];$l7=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);';$m7=bless({$W1,$k7,$v1,$l7},$S);$n7={$V6,$Y6,$Z6,$e7,$f7,$i7,$j7,$m7};$o7=q'/lib/ni_main.b';$p7=bless({$c,$U6,$j1,$k1,$l1,$k1,$m1,$n7,$f,$o7},$K);$q7=q'/lib/slice::ctors';$r7={};$s7=[];$t7=q'my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:/self failed to resolve $_[0]";';$u7=bless({$W1,$s7,$v1,$t7},$S);$v7=q'resolver_for';$w7=[];$x7=q'my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;';$y7=bless({$W1,$w7,$v1,$x7},$S);$z7={$H4,$u7,$v7,$y7};$A7=q'/lib/ni_resolver.b';$B7=bless({$c,$r7,$j1,$k1,$l1,$k1,$m1,$z7,$f,$A7},$K);$C7=q'/lib/slice::ctors';$D7={};$E7=q'fork';$F7=[];$G7=q'my ($class, $fn) = @_;
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
exit &$fn($stdin, $stdout, $stderr);';$H7=bless({$W1,$F7,$v1,$G7},$S);$I7=q'fork_exec';$J7=[];$K7=q'my ($class, @argv) = @_;
$class->fork(sub {
  my ($stdin, $stdout, $stderr) = @_;
  my $fd0 = ni(\'ni:/unix/fd\')->new(fileno $stdin->read_side->read_fh)->move_to(0);
  my $fd1 = ni(\'ni:/unix/fd\')->new(fileno $stdout->write_side->write_fh)->move_to(1);
  my $fd2 = ni(\'ni:/unix/fd\')->new(fileno $stderr->write_side->write_fh)->move_to(2);
  exec @argv or die "ni:/unix/pid.c: failed to exec @argv: $!";
});';$L7=bless({$W1,$J7,$v1,$K7},$S);$M7={$E7,$H7,$I7,$L7};$N7=q'/lib/ni_pid_ctors';$O7=bless({$c,$D7,$j1,$k1,$l1,$k1,$m1,$M7,$f,$N7},$K);$P7=q'/lib/slice::ctors';$Q7=[$r2,$F6,$S6,$p7,$B7,$O7];$R7=bless({$c,$t6,$f,$U,$g,$Q7},$o);$S7=q'/lib/ni.c::ctors';$T7=q'named';$U7=q'ni.doc:/class';$V7={$l,1};$W7=[$x3];$X7=bless({$c,$V7,$f,$l,$g,$W7},$E);$Y7=q'/metaclass::ctors';$Z7={$R,1};$c8={};$d8=q'shift; +{name => shift, doc => []}';$e8=bless({$v1,$d8},$S);$f8={$N1,$e8};$g8=q'/lib/doc_init.b';$h8=bless({$c,$c8,$j1,$k1,$l1,$k1,$m1,$f8,$f,$g8},$K);$i8=q'/lib/slice::ctors';$j8={};$k8=q'\'ni.doc\'';$l8=bless({$v1,$k8},$S);$m8={$p4,$l8};$n8=q'/lib/doc_namespace.b';$o8=bless({$c,$j8,$j1,$k1,$l1,$k1,$m1,$m8,$f,$n8},$K);$p8=q'/lib/slice::ctors';$q8={};$r8=q'AUTOLOAD';$s8=q'my $self = shift;
my $method = ${ref($self) . "::AUTOLOAD"};
push @{$$self{doc}}, [$method, @_];
$self;';$t8=bless({$v1,$s8},$S);$u8={$r8,$t8};$v8=q'/lib/doc_define.b';$w8=bless({$c,$q8,$j1,$k1,$l1,$k1,$m1,$u8,$f,$v8},$K);$x8=q'/lib/slice::ctors';$y8={};$z8=q'eg';$A8=q'my $self = shift;
push @{$$self{doc}}, [eg => [$_]] for @_;
$self;';$B8=bless({$v1,$A8},$S);$C8=q'tests';$D8=q'my $self = shift;
my @flattened = map +($$_[0], @{$$_[1]}), @{$$self{doc}};
my @tests;
return () unless @flattened;
for (0..$#flattened - 1) {
  push @tests, $flattened[$_ + 1] if $flattened[$_] eq \'eg\';
}
@tests;';$E8=bless({$v1,$D8},$S);$F8={$z8,$B8,$C8,$E8};$G8=q'/lib/doc_test.b';$H8=bless({$c,$y8,$j1,$k1,$l1,$k1,$m1,$F8,$f,$G8},$K);$I8=q'/lib/slice::ctors';$J8=[$r2,$D2,$h8,$o8,$w8,$H8];$K8=bless({$c,$Z7,$f,$R,$g,$J8},$l);$L8=q'/lib/doc.c::ctors';$M8=[];$N8=bless({$o3,$M8,$f,$Q},$R);$O8=q'/lib/doc::ctors';$P8=q'ni.doc:/lib/doc';$Q8=q'
    ni("ni:/some/class")->doc
      ->name(...)
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...';$R8=[$k1,$Q8];$S8=q'Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class\'s code without bringing along all of
      its documentation and unit tests.';$T8=q'Documentation objects are internally represented as arrays of quoted
      method calls; for example:';$U8=q'perl';$V8=q'
      # state is []
      $doc->foo("bar bif baz");
      # state is now [["foo", ["bar bif baz"]]]
    ';$W8=q'This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions.';$X8=q'Documentation also stores unit tests, which are specified using "eg";
      e.g.:';$Y8=q'my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = fn q{return 1};
my $failing_test = fn q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg => $failing_test,
                  q[So there.]);
my @tests = $doc->tests;
@tests == 2 && $tests[0] == $passing_test
            && $tests[1] == $failing_test;';$Z8=bless({$v1,$Y8},$S);$c9=[$k1,$S8,$T8,$U8,$V8,$W8,$X8,$z8,$Z8];$d9=[$R8,$c9];$e9=bless({$o3,$d9,$f,$R},$R);$f9=q'ni:/class';$g9=q'ni:/class.c';$h9=q'ni:/lib/accessor.b';$i9=q'ni:/lib/behavior';$j9=q'ni:/lib/behavior.c';$k9=q'ni:/lib/branch';$l9=q'ni:/lib/branch.b';$m9=q'ni:/lib/branch.c';$n9=q'ni:/lib/branch_init.b';$o9=q'ni:/lib/class_init.b';$p9=q'ni:/lib/classdef.b';$q9=q'ni:/lib/definition.b';$r9=q'ni:/lib/doc';$s9=q'ni:/lib/doc.c';$t9=q'ni:/lib/doc_define.b';$u9=q'ni:/lib/doc_init.b';$v9=q'ni:/lib/doc_namespace.b';$w9=q'ni:/lib/doc_test.b';$x9=q'ni:/lib/documentable.b';$y9=q'ni:/lib/fn';$z9=q'ni:/lib/fn.c';$A9=q'ni:/lib/fn_init.b';$B9=q'ni:/lib/fn_serialize.b';$C9=q'ni:/lib/image';$D9={$n,1};$E9=[$x3];$F9=bless({$c,$D9,$f,$n,$g,$E9},$E);$G9=q'/metaclass::ctors';$H9={$T,1};$I9={};$J9=[];$K9=q'my $class = shift;
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
  ordering     => []};';$L9=bless({$W1,$J9,$v1,$K9},$S);$M9={$N1,$L9};$N9=q'/lib/image_init.b';$O9=bless({$c,$I9,$j1,$k1,$l1,$k1,$m1,$M9,$f,$N9},$K);$P9=q'/lib/slice::ctors';$Q9={};$R9=q'address';$S9=[];$T9=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$U9=bless({$W1,$S9,$v1,$T9},$S);$V9=q'allocate_gensym';$W9=[];$X9=q'my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$Y9=bless({$W1,$W9,$v1,$X9},$S);$Z9=q'boot_side_effect';$ca=[];$da=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$ea=bless({$W1,$ca,$v1,$da},$S);$fa=q'circular_links';$ga=[];$ha=q'local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$ia=bless({$W1,$ga,$v1,$ha},$S);$ja=q'finalizer';$ka=[];$la=q'push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]';$ma=bless({$W1,$ka,$v1,$la},$S);$na=q'gensym';$oa=[];$pa=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$qa=bless({$W1,$oa,$v1,$pa},$S);$ra=q'is_circular';$sa=[];$ta=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$ua=bless({$W1,$sa,$v1,$ta},$S);$va=q'quote';$wa=[];$xa=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$ya=bless({$W1,$wa,$v1,$xa},$S);$za=q'quote_array';$Aa=[];$Ba=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$Ca=bless({$W1,$Aa,$v1,$Ba},$S);$Da=q'quote_blessed';$Ea=[];$Fa=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$Ga=bless({$W1,$Ea,$v1,$Fa},$S);$Ha=q'quote_class';$Ia=[];$Ja=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");';$Ka=bless({$W1,$Ia,$v1,$Ja},$S);$La=q'quote_hash';$Ma=[];$Na=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$Oa=bless({$W1,$Ma,$v1,$Na},$S);$Pa=q'quote_object';$Qa=[];$Ra=q'my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can(\'serialize\') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
$q;';$Sa=bless({$W1,$Qa,$v1,$Ra},$S);$Ta=q'quote_scalar';$Ua=[];$Va=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
"q\'$v\'";';$Wa=bless({$W1,$Ua,$v1,$Va},$S);$Xa=q'quote_value';$Ya=[];$Za=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$cb=bless({$W1,$Ya,$v1,$Za},$S);$db=q'reconstruction';$eb=[];$fb=q'my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});';$gb=bless({$W1,$eb,$v1,$fb},$S);$hb=q'side_effect';$ib=[];$jb=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$kb=bless({$W1,$ib,$v1,$jb},$S);$lb=q'write';$mb=[];$nb=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  ($$self{include_shebang} ? ("#!/usr/bin/env perl\\n") : ()),
  ($$self{include_license} ? ("chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n") : ()),
  ($$self{include_boot}    ? ("BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n") : ()),
  ($$self{use_newlines}    ? map("$_\\n", $self->reconstruction) : $self->reconstruction),
  ($$self{include_run}     ? ("ni->run(\\@ARGV);", "\\n__DATA__\\n") : ());';$ob=bless({$W1,$mb,$v1,$nb},$S);$pb={$R9,$U9,$V9,$Y9,$Z9,$ea,$fa,$ia,$ja,$ma,$na,$qa,$ra,$ua,$va,$ya,$za,$Ca,$Da,$Ga,$Ha,$Ka,$La,$Oa,$Pa,$Sa,$Ta,$Wa,$Xa,$cb,$db,$gb,$hb,$kb,$lb,$ob};$qb=q'/lib/image_quoting.b';$rb=bless({$c,$Q9,$j1,$k1,$l1,$k1,$m1,$pb,$f,$qb},$K);$sb=q'/lib/slice::ctors';$tb=[$r2,$O9,$rb];$ub=bless({$c,$H9,$f,$T,$g,$tb},$n);$vb=q'/lib/image.c::ctors';$wb=q'ni:/lib/image.c';$xb=q'ni:/lib/image_init.b';$yb=q'ni:/lib/image_quoting.b';$zb=q'ni:/lib/instance.b';$Ab=q'ni:/lib/instantiable.b';$Bb=q'ni:/lib/named.b';$Cb=q'ni:/lib/named_in_ni.b';$Db=q'ni:/lib/namespaced.b';$Eb=q'ni:/lib/ni';$Fb=q'ni:/lib/ni.c';$Gb=q'ni:/lib/ni_image.b';$Hb=q'ni:/lib/ni_main.b';$Ib=q'ni:/lib/ni_pid_ctors';$Jb=q'ni:/lib/ni_resolver.b';$Kb=q'ni:/lib/ni_self.b';$Lb=q'ni:/lib/ni_static';$Mb=q'/lib/ni_static';$Nb=q'ni';$Ob={$Mb,1,$Nb,1};$Pb={};$Qb=q'abbrev';$Rb=[];$Sb=q'length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . \'...\'';$Tb=bless({$W1,$Rb,$v1,$Sb},$S);$Ub=q'dor';$Vb=[];$Wb=q'defined $_[0] ? $_[0] : $_[1]';$Xb=bless({$W1,$Vb,$v1,$Wb},$S);$Yb=q'indent';$Zb=[];$cc=q'my ($s, $indent) = (@_, 2);
join "\\n", map \' \' x $indent . $_, split /\\n/, $s;';$dc=bless({$W1,$Zb,$v1,$cc},$S);$ec=q'max';$fc=[];$gc=q'local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m';$hc=bless({$W1,$fc,$v1,$gc},$S);$ic=q'maxstr';$jc=[];$kc=q'local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m';$lc=bless({$W1,$jc,$v1,$kc},$S);$mc=q'mean';$nc=[];$oc=q'sum(@_) / (@_ || 1)';$pc=bless({$W1,$nc,$v1,$oc},$S);$qc=q'min';$rc=[];$sc=q'local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m';$tc=bless({$W1,$rc,$v1,$sc},$S);$uc=q'minstr';$vc=[];$wc=q'local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m';$xc=bless({$W1,$vc,$v1,$wc},$S);$yc=q'sgr';$zc=[];$Ac=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x';$Bc=bless({$W1,$zc,$v1,$Ac},$S);$Cc=q'sr';$Dc=[];$Ec=q'(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x';$Fc=bless({$W1,$Dc,$v1,$Ec},$S);$Gc=q'sum';$Hc=[];$Ic=q'local $_; my $x = 0; $x += $_ for @_; $x';$Jc=bless({$W1,$Hc,$v1,$Ic},$S);$Kc=q'swap';$Lc=[];$Mc=q'@_[0, 1] = @_[1, 0]';$Nc=bless({$W1,$Lc,$v1,$Mc},$S);$Oc={$Qb,$Tb,$Ub,$Xb,$Yb,$dc,$ec,$hc,$ic,$lc,$mc,$pc,$qc,$tc,$uc,$xc,$yc,$Bc,$Cc,$Fc,$Gc,$Jc,$Kc,$Nc};$Pc=q'/lib/ni_static_util.b';$Qc=bless({$c,$Pb,$j1,$k1,$l1,$k1,$m1,$Oc,$f,$Pc},$K);$Rc=q'/lib/slice::ctors';$Sc=[$Qc];$Tc=bless({$c,$Ob,$f,$Mb,$g,$Sc},$Q);$Uc=q'/class::ctors';$Vc=q'ni:/lib/ni_static_util.b';$Wc=q'ni:/lib/perlbranch.b';$Xc=q'ni:/lib/resolver.b';$Yc=q'ni:/lib/slice';$Zc=q'ni:/lib/slice.b';$cd=q'ni:/lib/slice.c';$dd=q'ni:/lib/slice_init.b';$ed=q'ni:/lib/slice_serialize.b';$fd=q'ni:/lib/subclass.b';$gd=q'ni:/lib/tag';$hd=q'ni:/lib/tag.b';$id=q'ni:/lib/tag.c';$jd=q'ni:/lib/tag_init.b';$kd=q'ni:/metaclass';$ld=q'ni:/metaclass.c';$md=q'ni:/object';$nd=q'ni:/object.c';$od=q'ni:/unix/cat';$pd={$s,1};$qd=q'/unix/pipeline.c';$rd={$s,1,$t,1,$u,1,$v,1,$w,1,$x,1,$qd,1,$z,1};$sd=[$x3];$td=bless({$c,$rd,$f,$w,$g,$sd},$E);$ud=q'/metaclass::ctors';$vd=[$td];$wd=bless({$c,$pd,$f,$s,$g,$vd},$E);$xd=q'/metaclass::ctors';$yd={$W,1};$zd={$W,1,$X,1,$Y,1,$Z,1,$c1,1,$d1,1,$e1,1,$g1,1};$Ad={};$Bd=q'into';$Cd=[];$Dd=q'local $_;
my ($self, $dest, $each) = @_;
my $block_size = $self->can(\'read_size\') ? $self->read_size : 8192;
while ($self->read($_, $block_size)) {
  &$each($_) if defined $each;
  $dest->write($_);
}';$Ed=bless({$W1,$Cd,$v1,$Dd},$S);$Fd={$Bd,$Ed};$Gd=q'/unix/io_stream.b';$Hd=bless({$c,$Ad,$j1,$k1,$l1,$k1,$m1,$Fd,$f,$Gd},$K);$Id=q'/lib/slice::ctors';$Jd={};$Kd=q'(+';$Ld=[];$Md=q'ni(\'ni:/unix/cat\')->new(@_[0, 1])';$Nd=bless({$W1,$Ld,$v1,$Md},$S);$Od={$Kd,$Nd};$Pd=q'/unix/io_constructors.b';$Qd=bless({$c,$Jd,$j1,$k1,$l1,$k1,$m1,$Od,$f,$Pd},$K);$Rd=q'/lib/slice::ctors';$Sd={};$Td=q'(<>';$Ud=[];$Vd=q'my $fh = shift->read_fh;
# TODO: buffered line reading with a pushback queue';$Wd=bless({$W1,$Ud,$v1,$Vd},$S);$Xd=q'(@{}';$Yd=[];$Zd=q'my $self = shift; [<$self>]';$ce=bless({$W1,$Yd,$v1,$Zd},$S);$de={$Td,$Wd,$Xd,$ce};$ee=q'/unix/io_readers.b';$fe=bless({$c,$Sd,$j1,$k1,$l1,$k1,$m1,$de,$f,$ee},$K);$ge=q'/lib/slice::ctors';$he=[$r2,$Hd,$Qd,$fe];$ie=bless({$c,$zd,$f,$c1,$g,$he},$w);$je=q'/unix/io.c::ctors';$ke={};$le=[];$me=q'shift; +{fs => [@_]}';$ne=bless({$W1,$le,$v1,$me},$S);$oe={$N1,$ne};$pe=q'/unix/cat_init.b';$qe=bless({$c,$ke,$j1,$k1,$l1,$k1,$m1,$oe,$f,$pe},$K);$re=q'/lib/slice::ctors';$se={};$te=q'read';$ue=[];$ve=q'my $fs = shift->{fs};
my $n;
shift @$fs until !@$fs or $n = $$fs[0]->read(@_);
return $n;';$we=bless({$W1,$ue,$v1,$ve},$S);$xe={$te,$we};$ye=q'/unix/cat_read.b';$ze=bless({$c,$se,$j1,$k1,$l1,$k1,$m1,$xe,$f,$ye},$K);$Ae=q'/lib/slice::ctors';$Be=[$ie,$qe,$ze];$Ce=bless({$c,$yd,$f,$W,$g,$Be},$s);$De=q'/unix/cat.c::ctors';$Ee=q'ni:/unix/cat.c';$Fe=q'ni:/unix/cat_init.b';$Ge=q'ni:/unix/cat_read.b';$He=q'ni:/unix/fd';$Ie={$t,1};$Je=[$td];$Ke=bless({$c,$Ie,$f,$t,$g,$Je},$E);$Le=q'/metaclass::ctors';$Me={$X,1};$Ne={};$Oe=q'fd';$Pe=[];$Qe=q'shift->{\'fd\'}';$Re=bless({$W1,$Pe,$v1,$Qe},$S);$Se={$Oe,$Re};$Te=q'/unix/fd_readers.b';$Ue=bless({$c,$Ne,$j1,$k1,$l1,$k1,$m1,$Se,$f,$Te},$K);$Ve=q'/lib/slice::ctors';$We={};$Xe=[];$Ye=q'my ($class, $fd) = @_;
+{fd => $fd, fh => undef};';$Ze=bless({$W1,$Xe,$v1,$Ye},$S);$cf={$N1,$Ze};$df=q'/unix/fd_init.b';$ef=bless({$c,$We,$j1,$k1,$l1,$k1,$m1,$cf,$f,$df},$K);$ff=q'/lib/slice::ctors';$gf={};$hf=q'move_to';$if=[];$jf=q'use POSIX qw/dup2/;
my ($self, $new) = @_;
return $self if $new == $$self{fd};
close $$self{fh} if Scalar::Util::openhandle $$self{fh};
dup2 $$self{fd}, $new or die "ni:/unix/fd: dup2($$self{fd}, $new): $!";
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;';$kf=bless({$W1,$if,$v1,$jf},$S);$lf={$hf,$kf};$mf=q'/unix/fd_shell.b';$nf=bless({$c,$gf,$j1,$k1,$l1,$k1,$m1,$lf,$f,$mf},$K);$of=q'/lib/slice::ctors';$pf={$X,1,$Y,1,$Z,1,$d1,1,$e1,1};$qf=q'/unix/has_fd.b';$rf={};$sf=[];$tf=q'no warnings \'io\';
use Errno qw/EINTR/;
my $fh = shift->read_fh;
my $n;
do {
  return $n if defined($n = read $fh, $_[0], $_[1], $_[2] || 0);
} while $!{EINTR};
return undef;';$uf=bless({$W1,$sf,$v1,$tf},$S);$vf=[];$wf=q'my $fh = shift->write_fh;
my $n;
do {
  return $n if defined($n = syswrite $fh, $_[0]);
} while $!{EINTR};
return undef;';$xf=bless({$W1,$vf,$v1,$wf},$S);$yf={$te,$uf,$lb,$xf};$zf=q'/unix/fd_safeio.b';$Af=bless({$c,$rf,$j1,$k1,$l1,$k1,$m1,$yf,$f,$zf},$K);$Bf=q'/lib/slice::ctors';$Cf=[$Af];$Df=bless({$c,$pf,$f,$qf,$g,$Cf},$N);$Ef=q'/lib/branch::ctors';$Ff={};$Gf=q'read_fh';$Hf=[];$If=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<&=\', $self->{fd}
  or die "ni:/unix/fd $self->{fd} failed to read: $!";
$self->{fh} = $fh;';$Jf=bless({$W1,$Hf,$v1,$If},$S);$Kf=q'write_fh';$Lf=[];$Mf=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>&=\', $self->{fd}
  or die "ni:/unix/file $self->{fd} failed to write: $!";
$self->{fh} = $fh;';$Nf=bless({$W1,$Lf,$v1,$Mf},$S);$Of={$Gf,$Jf,$Kf,$Nf};$Pf=q'/unix/fd_io.b';$Qf=bless({$c,$Ff,$j1,$k1,$l1,$k1,$m1,$Of,$f,$Pf},$K);$Rf=q'/lib/slice::ctors';$Sf=[$ie,$Ue,$ef,$nf,$Df,$Qf];$Tf=bless({$c,$Me,$f,$X,$g,$Sf},$t);$Uf=q'/unix/fd.c::ctors';$Vf=q'ni:/unix/fd.c';$Wf=q'ni:/unix/fd_init.b';$Xf=q'ni:/unix/fd_io.b';$Yf=q'ni:/unix/fd_readers.b';$Zf=q'ni:/unix/fd_safeio.b';$cg=q'ni:/unix/fd_shell.b';$dg=q'ni:/unix/fifo';$eg={$u,1};$fg=[$td];$gg=bless({$c,$eg,$f,$u,$g,$fg},$E);$hg=q'/metaclass::ctors';$ig={$Y,1};$jg={};$kg=[];$lg=q'shift->{\'read_fh\'}';$mg=bless({$W1,$kg,$v1,$lg},$S);$ng=[];$og=q'shift->{\'write_fh\'}';$pg=bless({$W1,$ng,$v1,$og},$S);$qg={$Gf,$mg,$Kf,$pg};$rg=q'/unix/fifo_io.b';$sg=bless({$c,$jg,$j1,$k1,$l1,$k1,$m1,$qg,$f,$rg},$K);$tg=q'/lib/slice::ctors';$ug={};$vg=[];$wg=q'my ($class) = @_;
pipe my ($r, $w) or die "ni:/unix/fifo failed: $!";
+{read_fh => $r, write_fh => $w};';$xg=bless({$W1,$vg,$v1,$wg},$S);$yg={$N1,$xg};$zg=q'/unix/fifo_init.b';$Ag=bless({$c,$ug,$j1,$k1,$l1,$k1,$m1,$yg,$f,$zg},$K);$Bg=q'/lib/slice::ctors';$Cg={};$Dg=q'read_side';$Eg=[];$Fg=q'my $self = shift; close $$self{write_fh}; $self';$Gg=bless({$W1,$Eg,$v1,$Fg},$S);$Hg=q'write_side';$Ig=[];$Jg=q'my $self = shift; close $$self{read_fh};  $self';$Kg=bless({$W1,$Ig,$v1,$Jg},$S);$Lg={$Dg,$Gg,$Hg,$Kg};$Mg=q'/unix/fifo_direction.b';$Ng=bless({$c,$Cg,$j1,$k1,$l1,$k1,$m1,$Lg,$f,$Mg},$K);$Og=q'/lib/slice::ctors';$Pg=[$ie,$sg,$Ag,$Df,$Ng];$Qg=bless({$c,$ig,$f,$Y,$g,$Pg},$u);$Rg=q'/unix/fifo.c::ctors';$Sg=q'ni:/unix/fifo.c';$Tg=q'ni:/unix/fifo_direction.b';$Ug=q'ni:/unix/fifo_init.b';$Vg=q'ni:/unix/fifo_io.b';$Wg=q'ni:/unix/file';$Xg={$v,1};$Yg=[$td];$Zg=bless({$c,$Xg,$f,$v,$g,$Yg},$E);$ch=q'/metaclass::ctors';$dh={$Z,1};$eh={};$fh=[];$gh=q'shift->{\'name\'}';$hh=bless({$W1,$fh,$v1,$gh},$S);$ih={$f,$hh};$jh=q'/unix/file_readers.b';$kh=bless({$c,$eh,$j1,$k1,$l1,$k1,$m1,$ih,$f,$jh},$K);$lh=q'/lib/slice::ctors';$mh={};$nh=[];$oh=q'my ($class, $name) = @_;
+{name => $name, fh => undef};';$ph=bless({$W1,$nh,$v1,$oh},$S);$qh={$N1,$ph};$rh=q'/unix/file_init.b';$sh=bless({$c,$mh,$j1,$k1,$l1,$k1,$m1,$qh,$f,$rh},$K);$th=q'/lib/slice::ctors';$uh={};$vh=[];$wh=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'<\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to read: $!";
$self->{fh} = $fh;';$xh=bless({$W1,$vh,$v1,$wh},$S);$yh=[];$zh=q'my $self = shift;
return $self->{fh} if Scalar::Util::openhandle $self->{fh};
open my $fh, \'>\', $self->{name}
  or die "ni:/unix/file $self->{name} failed to write: $!";
$self->{fh} = $fh;';$Ah=bless({$W1,$yh,$v1,$zh},$S);$Bh={$Gf,$xh,$Kf,$Ah};$Ch=q'/unix/file_io.b';$Dh=bless({$c,$uh,$j1,$k1,$l1,$k1,$m1,$Bh,$f,$Ch},$K);$Eh=q'/lib/slice::ctors';$Fh=[$ie,$kh,$sh,$Df,$Dh];$Gh=bless({$c,$dh,$f,$Z,$g,$Fh},$v);$Hh=q'/unix/file.c::ctors';$Ih=q'ni:/unix/file.c';$Jh=q'ni:/unix/file_init.b';$Kh=q'ni:/unix/file_io.b';$Lh=q'ni:/unix/file_readers.b';$Mh=q'ni:/unix/has_fd.b';$Nh=q'ni:/unix/io';$Oh=q'ni:/unix/io.c';$Ph=q'ni:/unix/io_constructors.b';$Qh=q'ni:/unix/io_readers.b';$Rh=q'ni:/unix/io_stream.b';$Sh=q'ni:/unix/pid';$Th={$x,1};$Uh=[$td];$Vh=bless({$c,$Th,$f,$x,$g,$Uh},$E);$Wh=q'/metaclass::ctors';$Xh={$d1,1};$Yh={};$Zh=q'pid';$ci=[];$di=q'shift->{\'pid\'}';$ei=bless({$W1,$ci,$v1,$di},$S);$fi=q'stderr';$gi=[];$hi=q'shift->{\'stderr\'}';$ii=bless({$W1,$gi,$v1,$hi},$S);$ji=q'stdin';$ki=[];$li=q'shift->{\'stdin\'}';$mi=bless({$W1,$ki,$v1,$li},$S);$ni=q'stdout';$oi=[];$pi=q'shift->{\'stdout\'}';$qi=bless({$W1,$oi,$v1,$pi},$S);$ri={$Zh,$ei,$fi,$ii,$ji,$mi,$ni,$qi};$si=q'/unix/pid_readers.b';$ti=bless({$c,$Yh,$j1,$k1,$l1,$k1,$m1,$ri,$f,$si},$K);$ui=q'/lib/slice::ctors';$vi={};$wi=[];$xi=q'my ($class, $pid, $i, $o, $e) = @_;
+{pid    => $pid,
  stdin  => $i,
  stdout => $o,
  stderr => $e};';$yi=bless({$W1,$wi,$v1,$xi},$S);$zi={$N1,$yi};$Ai=q'/unix/pid_init.b';$Bi=bless({$c,$vi,$j1,$k1,$l1,$k1,$m1,$zi,$f,$Ai},$K);$Ci=q'/lib/slice::ctors';$Di={};$Ei={};$Fi=q'/unix/pid_wait.b';$Gi=bless({$c,$Di,$j1,$k1,$l1,$k1,$m1,$Ei,$f,$Fi},$K);$Hi=q'/lib/slice::ctors';$Ii={};$Ji=[];$Ki=q'shift->{stdout}->read_fh';$Li=bless({$W1,$Ji,$v1,$Ki},$S);$Mi=[];$Ni=q'shift->{stdin}->write_fh';$Oi=bless({$W1,$Mi,$v1,$Ni},$S);$Pi={$Gf,$Li,$Kf,$Oi};$Qi=q'/unix/pid_io.b';$Ri=bless({$c,$Ii,$j1,$k1,$l1,$k1,$m1,$Pi,$f,$Qi},$K);$Si=q'/lib/slice::ctors';$Ti=[$ie,$ti,$Bi,$Gi,$Df,$Ri];$Ui=bless({$c,$Xh,$f,$d1,$g,$Ti},$x);$Vi=q'/unix/pid.c::ctors';$Wi=q'ni:/unix/pid.c';$Xi=q'ni:/unix/pid_init.b';$Yi=q'ni:/unix/pid_io.b';$Zi=q'ni:/unix/pid_readers.b';$cj=q'ni:/unix/pid_wait.b';$dj=q'ni:/unix/pipeline';$ej=q'/unix/pipeline.c';$fj={$ej,1};$gj=q'/unix/pipeline.c';$hj=[$td];$ij=bless({$c,$fj,$f,$gj,$g,$hj},$E);$jj=q'/metaclass::ctors';$kj={$e1,1};$lj={};$mj=[];$nj=q'shift->{\'stdin\'}';$oj=bless({$W1,$mj,$v1,$nj},$S);$pj=[];$qj=q'shift->{\'stdout\'}';$rj=bless({$W1,$pj,$v1,$qj},$S);$sj={$ji,$oj,$ni,$rj};$tj=q'/unix/pipeline_ro.b';$uj=bless({$c,$lj,$j1,$k1,$l1,$k1,$m1,$sj,$f,$tj},$K);$vj=q'/lib/slice::ctors';$wj={};$xj=[];$yj=q'my $class  = shift;
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
  ws => \\@ws, wv => $wv};';$zj=bless({$W1,$xj,$v1,$yj},$S);$Aj={$N1,$zj};$Bj=q'/unix/pipeline_init.b';$Cj=bless({$c,$wj,$j1,$k1,$l1,$k1,$m1,$Aj,$f,$Bj},$K);$Dj=q'/lib/slice::ctors';$Ej={};$Fj=q'async_step';$Gj=[];$Hj=q'local $_;
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
$self;';$Ij=bless({$W1,$Gj,$v1,$Hj},$S);$Jj={$Fj,$Ij};$Kj=q'/unix/pipeline_async.b';$Lj=bless({$c,$Ej,$j1,$k1,$l1,$k1,$m1,$Jj,$f,$Kj},$K);$Mj=q'/lib/slice::ctors';$Nj={};$Oj=[];$Pj=q'shift->{stdout}->read_fh';$Qj=bless({$W1,$Oj,$v1,$Pj},$S);$Rj=[];$Sj=q'shift->{stdin}->write_fh';$Tj=bless({$W1,$Rj,$v1,$Sj},$S);$Uj={$Gf,$Qj,$Kf,$Tj};$Vj=q'/unix/pipeline_io.b';$Wj=bless({$c,$Nj,$j1,$k1,$l1,$k1,$m1,$Uj,$f,$Vj},$K);$Xj=q'/lib/slice::ctors';$Yj=[$ie,$uj,$Cj,$Lj,$Df,$Wj];$Zj=q'/unix/pipeline.c';$ck=bless({$c,$kj,$f,$e1,$g,$Yj},$Zj);$dk=q'/unix/pipeline.c::ctors';$ek=q'ni:/unix/pipeline.c';$fk=q'ni:/unix/pipeline_async.b';$gk=q'ni:/unix/pipeline_init.b';$hk=q'ni:/unix/pipeline_io.b';$ik=q'ni:/unix/pipeline_ro.b';$jk=q'ni:/unix/str';$kk={$z,1};$lk=[$td];$mk=bless({$c,$kk,$f,$z,$g,$lk},$E);$nk=q'/metaclass::ctors';$ok={$g1,1};$pk={};$qk=[];$rk=q'my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};';$sk=bless({$W1,$qk,$v1,$rk},$S);$tk={$N1,$sk};$uk=q'/unix/str_init.b';$vk=bless({$c,$pk,$j1,$k1,$l1,$k1,$m1,$tk,$f,$uk},$K);$wk=q'/lib/slice::ctors';$xk={};$yk=[];$zk=q'my $self = shift;
return 0 if $$self{start} >= $$self{end};
my $l = ni::min $self->remaining, $_[1];
if (@_ == 3) {
  substr($_[0], $_[2] || 0, $l, substr(${$$self{data}}, $$self{start}, $l));
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;';$Ak=bless({$W1,$yk,$v1,$zk},$S);$Bk=q'remaining';$Ck=[];$Dk=q'my $self = shift; $$self{end} - $$self{start}';$Ek=bless({$W1,$Ck,$v1,$Dk},$S);$Fk=[];$Gk=q'my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];';$Hk=bless({$W1,$Fk,$v1,$Gk},$S);$Ik={$te,$Ak,$Bk,$Ek,$lb,$Hk};$Jk=q'/unix/str_io.b';$Kk=bless({$c,$xk,$j1,$k1,$l1,$k1,$m1,$Ik,$f,$Jk},$K);$Lk=q'/lib/slice::ctors';$Mk=[$ie,$vk,$Kk];$Nk=bless({$c,$ok,$f,$g1,$g,$Mk},$z);$Ok=q'/unix/str.c::ctors';$Pk=q'ni:/unix/str.c';$Qk=q'ni:/unix/str_init.b';$Rk=q'ni:/unix/str_io.b';$Sk={$U7,$N8,$P8,$e9,$f9,$g6,$g9,$A3,$h9,$O5,$i9,$u2,$j9,$F,$k9,$r5,$l9,$l4,$m9,$g5,$n9,$o5,$o9,$Z4,$p9,$C5,$q9,$S5,$r9,$K8,$s9,$X7,$t9,$w8,$u9,$h8,$v9,$o8,$w9,$H8,$x9,$t3,$y9,$h2,$z9,$q1,$A9,$S1,$B9,$e2,$C9,$ub,$wb,$F9,$xb,$O9,$yb,$rb,$zb,$n2,$Ab,$E1,$Bb,$D2,$Cb,$u4,$Db,$D4,$Eb,$R7,$Fb,$r6,$Gb,$S6,$Hb,$p7,$Ib,$O7,$Jb,$B7,$Kb,$F6,$Lb,$Tc,$Vc,$Qc,$Wc,$Q4,$Xc,$M4,$Yc,$l3,$Zc,$R2,$cd,$I,$dd,$Y2,$ed,$i3,$fd,$c6,$gd,$Y3,$hd,$O3,$id,$G3,$jd,$V3,$kd,$n6,$ld,$j6,$md,$r2,$nd,$x3,$od,$Ce,$Ee,$wd,$Fe,$qe,$Ge,$ze,$He,$Tf,$Vf,$Ke,$Wf,$ef,$Xf,$Qf,$Yf,$Ue,$Zf,$Af,$cg,$nf,$dg,$Qg,$Sg,$gg,$Tg,$Ng,$Ug,$Ag,$Vg,$sg,$Wg,$Gh,$Ih,$Zg,$Jh,$sh,$Kh,$Dh,$Lh,$kh,$Mh,$Df,$Nh,$ie,$Oh,$td,$Ph,$Qd,$Qh,$fe,$Rh,$Hd,$Sh,$Ui,$Wi,$Vh,$Xi,$Bi,$Yi,$Ri,$Zi,$ti,$cj,$Gi,$dj,$ck,$ek,$ij,$fk,$Lj,$gk,$Cj,$hk,$Wj,$ik,$uj,$jk,$Nk,$Pk,$mk,$Qk,$vk,$Rk,$Kk};$Tk=q'resolvers';$Uk=[];$Vk=q'my $f = shift;
$f =~ s/^fd:(?:\\/\\/)?//;
ni(\'ni:/unix/fd\')->new($f);';$Wk=bless({$W1,$Uk,$v1,$Vk},$S);$Xk=q'file';$Yk=[];$Zk=q'my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni(\'ni:/unix/file\')->new($f);';$cl=bless({$W1,$Yk,$v1,$Zk},$S);$dl=q'str';$el=[];$fl=q'my $s = shift;
ni(\'ni:/unix/str\')->new(substr($s, 4) . "\\n");';$gl=bless({$W1,$el,$v1,$fl},$S);$hl={$Oe,$Wk,$Xk,$cl,$dl,$gl};$il=bless({$T7,$Sk,$Tk,$hl},$U);$jl=q'/lib/ni::ctors';$$w3[0]=$g6;$$D[0]=$x3;$$p1[0]=$x3;$$g2[0]=$r2;$$q5[4]=$S5;*$L2=\&$J2;*$K2=\&$H2;$E1->apply_unsafe($Q);$E1->apply_unsafe($h);$E1->apply_unsafe($j);$E1->apply_unsafe($N);$E1->apply_unsafe($k);$E1->apply_unsafe($l);$E1->apply_unsafe($S);$E1->apply_unsafe($m);$E1->apply_unsafe($n);$E1->apply_unsafe($o);$E1->apply_unsafe($K);$E1->apply_unsafe($p);$E1->apply_unsafe($O);$E1->apply_unsafe($q);$E1->apply_unsafe($E);$E1->apply_unsafe($d);$E1->apply_unsafe($r);$E1->apply_unsafe($s);$E1->apply_unsafe($t);$E1->apply_unsafe($u);$E1->apply_unsafe($v);$E1->apply_unsafe($w);$E1->apply_unsafe($x);$E1->apply_unsafe($F1);$E1->apply_unsafe($z);$S1->apply_unsafe($S);$e2->apply_unsafe($S);$n2->apply_unsafe($Q);$n2->apply_unsafe($h);$n2->apply_unsafe($M);$n2->apply_unsafe($j);$n2->apply_unsafe($N);$n2->apply_unsafe($k);$n2->apply_unsafe($R);$n2->apply_unsafe($l);$n2->apply_unsafe($S);$n2->apply_unsafe($m);$n2->apply_unsafe($T);$n2->apply_unsafe($n);$n2->apply_unsafe($U);$n2->apply_unsafe($o);$n2->apply_unsafe($K);$n2->apply_unsafe($p);$n2->apply_unsafe($O);$n2->apply_unsafe($q);$n2->apply_unsafe($E);$n2->apply_unsafe($d);$n2->apply_unsafe($V);$n2->apply_unsafe($r);$n2->apply_unsafe($W);$n2->apply_unsafe($s);$n2->apply_unsafe($X);$n2->apply_unsafe($t);$n2->apply_unsafe($Y);$n2->apply_unsafe($u);$n2->apply_unsafe($Z);$n2->apply_unsafe($v);$n2->apply_unsafe($c1);$n2->apply_unsafe($w);$n2->apply_unsafe($d1);$n2->apply_unsafe($x);$n2->apply_unsafe($e1);$n2->apply_unsafe($o2);$n2->apply_unsafe($g1);$n2->apply_unsafe($z);$D2->apply_unsafe($Q);$D2->apply_unsafe($h);$D2->apply_unsafe($j);$D2->apply_unsafe($N);$D2->apply_unsafe($k);$D2->apply_unsafe($R);$D2->apply_unsafe($l);$D2->apply_unsafe($m);$D2->apply_unsafe($n);$D2->apply_unsafe($o);$D2->apply_unsafe($K);$D2->apply_unsafe($p);$D2->apply_unsafe($O);$D2->apply_unsafe($q);$D2->apply_unsafe($E);$D2->apply_unsafe($d);$D2->apply_unsafe($r);$D2->apply_unsafe($s);$D2->apply_unsafe($t);$D2->apply_unsafe($u);$D2->apply_unsafe($v);$D2->apply_unsafe($w);$D2->apply_unsafe($x);$D2->apply_unsafe($E2);$D2->apply_unsafe($z);$R2->apply_unsafe($K);$Y2->apply_unsafe($K);$i3->apply_unsafe($K);$t3->apply_unsafe($h);$t3->apply_unsafe($j);$t3->apply_unsafe($k);$t3->apply_unsafe($l);$t3->apply_unsafe($m);$t3->apply_unsafe($n);$t3->apply_unsafe($o);$t3->apply_unsafe($p);$t3->apply_unsafe($q);$t3->apply_unsafe($r);$t3->apply_unsafe($s);$t3->apply_unsafe($t);$t3->apply_unsafe($u);$t3->apply_unsafe($v);$t3->apply_unsafe($w);$t3->apply_unsafe($x);$t3->apply_unsafe($u3);$t3->apply_unsafe($z);$O3->apply_unsafe($O);$V3->apply_unsafe($O);$l4->apply_unsafe($Q);$l4->apply_unsafe($h);$l4->apply_unsafe($j);$l4->apply_unsafe($N);$l4->apply_unsafe($k);$l4->apply_unsafe($l);$l4->apply_unsafe($m);$l4->apply_unsafe($n);$l4->apply_unsafe($o);$l4->apply_unsafe($p);$l4->apply_unsafe($q);$l4->apply_unsafe($E);$l4->apply_unsafe($d);$l4->apply_unsafe($r);$l4->apply_unsafe($s);$l4->apply_unsafe($t);$l4->apply_unsafe($u);$l4->apply_unsafe($v);$l4->apply_unsafe($w);$l4->apply_unsafe($x);$l4->apply_unsafe($m4);$l4->apply_unsafe($z);$u4->apply_unsafe($Q);$u4->apply_unsafe($h);$u4->apply_unsafe($j);$u4->apply_unsafe($N);$u4->apply_unsafe($k);$u4->apply_unsafe($l);$u4->apply_unsafe($m);$u4->apply_unsafe($n);$u4->apply_unsafe($o);$u4->apply_unsafe($K);$u4->apply_unsafe($p);$u4->apply_unsafe($O);$u4->apply_unsafe($q);$u4->apply_unsafe($E);$u4->apply_unsafe($d);$u4->apply_unsafe($r);$u4->apply_unsafe($s);$u4->apply_unsafe($t);$u4->apply_unsafe($u);$u4->apply_unsafe($v);$u4->apply_unsafe($w);$u4->apply_unsafe($x);$u4->apply_unsafe($v4);$u4->apply_unsafe($z);$D4->apply_unsafe($Q);$D4->apply_unsafe($h);$D4->apply_unsafe($j);$D4->apply_unsafe($N);$D4->apply_unsafe($k);$D4->apply_unsafe($l);$D4->apply_unsafe($m);$D4->apply_unsafe($n);$D4->apply_unsafe($o);$D4->apply_unsafe($K);$D4->apply_unsafe($p);$D4->apply_unsafe($O);$D4->apply_unsafe($q);$D4->apply_unsafe($E);$D4->apply_unsafe($d);$D4->apply_unsafe($r);$D4->apply_unsafe($s);$D4->apply_unsafe($t);$D4->apply_unsafe($u);$D4->apply_unsafe($v);$D4->apply_unsafe($w);$D4->apply_unsafe($x);$D4->apply_unsafe($E4);$D4->apply_unsafe($z);$M4->apply_unsafe($Q);$M4->apply_unsafe($h);$M4->apply_unsafe($j);$M4->apply_unsafe($N);$M4->apply_unsafe($k);$M4->apply_unsafe($l);$M4->apply_unsafe($m);$M4->apply_unsafe($n);$M4->apply_unsafe($o);$M4->apply_unsafe($p);$M4->apply_unsafe($O);$M4->apply_unsafe($q);$M4->apply_unsafe($E);$M4->apply_unsafe($d);$M4->apply_unsafe($r);$M4->apply_unsafe($s);$M4->apply_unsafe($t);$M4->apply_unsafe($u);$M4->apply_unsafe($v);$M4->apply_unsafe($w);$M4->apply_unsafe($x);$M4->apply_unsafe($N4);$M4->apply_unsafe($z);$Z4->apply_unsafe($Q);$Z4->apply_unsafe($h);$Z4->apply_unsafe($j);$Z4->apply_unsafe($k);$Z4->apply_unsafe($l);$Z4->apply_unsafe($m);$Z4->apply_unsafe($n);$Z4->apply_unsafe($o);$Z4->apply_unsafe($p);$Z4->apply_unsafe($q);$Z4->apply_unsafe($E);$Z4->apply_unsafe($d);$Z4->apply_unsafe($r);$Z4->apply_unsafe($s);$Z4->apply_unsafe($t);$Z4->apply_unsafe($u);$Z4->apply_unsafe($v);$Z4->apply_unsafe($w);$Z4->apply_unsafe($x);$Z4->apply_unsafe($c5);$Z4->apply_unsafe($z);$o5->apply_unsafe($N);$C5->apply_unsafe($Q);$C5->apply_unsafe($h);$C5->apply_unsafe($j);$C5->apply_unsafe($N);$C5->apply_unsafe($k);$C5->apply_unsafe($l);$C5->apply_unsafe($m);$C5->apply_unsafe($n);$C5->apply_unsafe($o);$C5->apply_unsafe($p);$C5->apply_unsafe($q);$C5->apply_unsafe($E);$C5->apply_unsafe($d);$C5->apply_unsafe($r);$C5->apply_unsafe($s);$C5->apply_unsafe($t);$C5->apply_unsafe($u);$C5->apply_unsafe($v);$C5->apply_unsafe($w);$C5->apply_unsafe($x);$C5->apply_unsafe($D5);$C5->apply_unsafe($z);$O5->apply_unsafe($Q);$O5->apply_unsafe($h);$O5->apply_unsafe($j);$O5->apply_unsafe($N);$O5->apply_unsafe($k);$O5->apply_unsafe($l);$O5->apply_unsafe($m);$O5->apply_unsafe($n);$O5->apply_unsafe($o);$O5->apply_unsafe($p);$O5->apply_unsafe($q);$O5->apply_unsafe($E);$O5->apply_unsafe($d);$O5->apply_unsafe($r);$O5->apply_unsafe($s);$O5->apply_unsafe($t);$O5->apply_unsafe($u);$O5->apply_unsafe($v);$O5->apply_unsafe($w);$O5->apply_unsafe($x);$O5->apply_unsafe($P5);$O5->apply_unsafe($z);$c6->apply_unsafe($Q);$c6->apply_unsafe($h);$c6->apply_unsafe($j);$c6->apply_unsafe($k);$c6->apply_unsafe($l);$c6->apply_unsafe($m);$c6->apply_unsafe($n);$c6->apply_unsafe($o);$c6->apply_unsafe($p);$c6->apply_unsafe($q);$c6->apply_unsafe($d);$c6->apply_unsafe($r);$c6->apply_unsafe($s);$c6->apply_unsafe($t);$c6->apply_unsafe($u);$c6->apply_unsafe($v);$c6->apply_unsafe($w);$c6->apply_unsafe($x);$c6->apply_unsafe($d6);$c6->apply_unsafe($z);$F6->apply_unsafe($U);$S6->apply_unsafe($U);$p7->apply_unsafe($U);$B7->apply_unsafe($U);$O7->apply_unsafe($U);$h8->apply_unsafe($R);$o8->apply_unsafe($R);$w8->apply_unsafe($R);$H8->apply_unsafe($R);$O9->apply_unsafe($T);$rb->apply_unsafe($T);$Qc->apply_unsafe($Mb);$Qc->apply_unsafe($Nb);$Hd->apply_unsafe($W);$Hd->apply_unsafe($X);$Hd->apply_unsafe($Y);$Hd->apply_unsafe($Z);$Hd->apply_unsafe($c1);$Hd->apply_unsafe($d1);$Hd->apply_unsafe($e1);$Hd->apply_unsafe($g1);$Qd->apply_unsafe($W);$Qd->apply_unsafe($X);$Qd->apply_unsafe($Y);$Qd->apply_unsafe($Z);$Qd->apply_unsafe($c1);$Qd->apply_unsafe($d1);$Qd->apply_unsafe($e1);$Qd->apply_unsafe($g1);$fe->apply_unsafe($W);$fe->apply_unsafe($X);$fe->apply_unsafe($Y);$fe->apply_unsafe($Z);$fe->apply_unsafe($c1);$fe->apply_unsafe($d1);$fe->apply_unsafe($e1);$fe->apply_unsafe($g1);$qe->apply_unsafe($W);$ze->apply_unsafe($W);$Ue->apply_unsafe($X);$ef->apply_unsafe($X);$nf->apply_unsafe($X);$Af->apply_unsafe($X);$Af->apply_unsafe($Y);$Af->apply_unsafe($Z);$Af->apply_unsafe($d1);$Af->apply_unsafe($e1);$Qf->apply_unsafe($X);$sg->apply_unsafe($Y);$Ag->apply_unsafe($Y);$Ng->apply_unsafe($Y);$kh->apply_unsafe($Z);$sh->apply_unsafe($Z);$Dh->apply_unsafe($Z);$ti->apply_unsafe($d1);$Bi->apply_unsafe($d1);$Gi->apply_unsafe($d1);$Ri->apply_unsafe($d1);$uj->apply_unsafe($e1);$Cj->apply_unsafe($e1);$Lj->apply_unsafe($e1);$Wj->apply_unsafe($e1);$vk->apply_unsafe($g1);$Kk->apply_unsafe($g1);$ni::self=$il;&$_($F)for@$G;&$_($I)for@$J;&$_($q1)for@$r1;&$_($x1)for@$y1;&$_($B1)for@$y1;&$_($E1)for@$G1;&$_($J1)for@$y1;&$_($M1)for@$y1;&$_($P1)for@$y1;&$_($S1)for@$T1;&$_($Z1)for@$y1;&$_($e2)for@$f2;&$_($h2)for@$i2;&$_($k2)for@$y1;&$_($n2)for@$p2;&$_($r2)for@$s2;&$_($u2)for@$v2;&$_($y2)for@$y1;&$_($A2)for@$y1;&$_($D2)for@$F2;&$_($H2)for@$y1;&$_($J2)for@$y1;&$_($R2)for@$S2;&$_($V2)for@$y1;&$_($Y2)for@$Z2;&$_($f3)for@$y1;&$_($i3)for@$j3;&$_($l3)for@$m3;&$_($q3)for@$y1;&$_($t3)for@$v3;&$_($x3)for@$y3;&$_($A3)for@$B3;&$_($G3)for@$H3;&$_($L3)for@$y1;&$_($O3)for@$P3;&$_($S3)for@$y1;&$_($V3)for@$W3;&$_($Y3)for@$Z3;&$_($g4)for@$y1;&$_($i4)for@$y1;&$_($l4)for@$n4;&$_($r4)for@$y1;&$_($u4)for@$w4;&$_($A4)for@$y1;&$_($D4)for@$F4;&$_($J4)for@$y1;&$_($M4)for@$O4;&$_($Q4)for@$R4;&$_($U4)for@$y1;&$_($W4)for@$y1;&$_($Z4)for@$d5;&$_($g5)for@$h5;&$_($l5)for@$y1;&$_($o5)for@$p5;&$_($r5)for@$s5;&$_($z5)for@$y1;&$_($C5)for@$E5;&$_($I5)for@$y1;&$_($L5)for@$y1;&$_($O5)for@$Q5;&$_($S5)for@$T5;&$_($X5)for@$y1;&$_($c6)for@$e6;&$_($g6)for@$h6;&$_($j6)for@$k6;&$_($n6)for@$o6;&$_($r6)for@$s6;&$_($y6)for@$y1;&$_($C6)for@$y1;&$_($F6)for@$G6;&$_($L6)for@$y1;&$_($P6)for@$y1;&$_($S6)for@$T6;&$_($Y6)for@$y1;&$_($e7)for@$y1;&$_($i7)for@$y1;&$_($m7)for@$y1;&$_($p7)for@$q7;&$_($u7)for@$y1;&$_($y7)for@$y1;&$_($B7)for@$C7;&$_($H7)for@$y1;&$_($L7)for@$y1;&$_($O7)for@$P7;&$_($R7)for@$S7;&$_($X7)for@$Y7;&$_($e8)for@$y1;&$_($h8)for@$i8;&$_($l8)for@$y1;&$_($o8)for@$p8;&$_($t8)for@$y1;&$_($w8)for@$x8;&$_($B8)for@$y1;&$_($E8)for@$y1;&$_($H8)for@$I8;&$_($K8)for@$L8;&$_($N8)for@$O8;&$_($Z8)for@$y1;&$_($e9)for@$O8;&$_($F9)for@$G9;&$_($L9)for@$y1;&$_($O9)for@$P9;&$_($U9)for@$y1;&$_($Y9)for@$y1;&$_($ea)for@$y1;&$_($ia)for@$y1;&$_($ma)for@$y1;&$_($qa)for@$y1;&$_($ua)for@$y1;&$_($ya)for@$y1;&$_($Ca)for@$y1;&$_($Ga)for@$y1;&$_($Ka)for@$y1;&$_($Oa)for@$y1;&$_($Sa)for@$y1;&$_($Wa)for@$y1;&$_($cb)for@$y1;&$_($gb)for@$y1;&$_($kb)for@$y1;&$_($ob)for@$y1;&$_($rb)for@$sb;&$_($ub)for@$vb;&$_($Tb)for@$y1;&$_($Xb)for@$y1;&$_($dc)for@$y1;&$_($hc)for@$y1;&$_($lc)for@$y1;&$_($pc)for@$y1;&$_($tc)for@$y1;&$_($xc)for@$y1;&$_($Bc)for@$y1;&$_($Fc)for@$y1;&$_($Jc)for@$y1;&$_($Nc)for@$y1;&$_($Qc)for@$Rc;&$_($Tc)for@$Uc;&$_($td)for@$ud;&$_($wd)for@$xd;&$_($Ed)for@$y1;&$_($Hd)for@$Id;&$_($Nd)for@$y1;&$_($Qd)for@$Rd;&$_($Wd)for@$y1;&$_($ce)for@$y1;&$_($fe)for@$ge;&$_($ie)for@$je;&$_($ne)for@$y1;&$_($qe)for@$re;&$_($we)for@$y1;&$_($ze)for@$Ae;&$_($Ce)for@$De;&$_($Ke)for@$Le;&$_($Re)for@$y1;&$_($Ue)for@$Ve;&$_($Ze)for@$y1;&$_($ef)for@$ff;&$_($kf)for@$y1;&$_($nf)for@$of;&$_($uf)for@$y1;&$_($xf)for@$y1;&$_($Af)for@$Bf;&$_($Df)for@$Ef;&$_($Jf)for@$y1;&$_($Nf)for@$y1;&$_($Qf)for@$Rf;&$_($Tf)for@$Uf;&$_($gg)for@$hg;&$_($mg)for@$y1;&$_($pg)for@$y1;&$_($sg)for@$tg;&$_($xg)for@$y1;&$_($Ag)for@$Bg;&$_($Gg)for@$y1;&$_($Kg)for@$y1;&$_($Ng)for@$Og;&$_($Qg)for@$Rg;&$_($Zg)for@$ch;&$_($hh)for@$y1;&$_($kh)for@$lh;&$_($ph)for@$y1;&$_($sh)for@$th;&$_($xh)for@$y1;&$_($Ah)for@$y1;&$_($Dh)for@$Eh;&$_($Gh)for@$Hh;&$_($Vh)for@$Wh;&$_($ei)for@$y1;&$_($ii)for@$y1;&$_($mi)for@$y1;&$_($qi)for@$y1;&$_($ti)for@$ui;&$_($yi)for@$y1;&$_($Bi)for@$Ci;&$_($Gi)for@$Hi;&$_($Li)for@$y1;&$_($Oi)for@$y1;&$_($Ri)for@$Si;&$_($Ui)for@$Vi;&$_($ij)for@$jj;&$_($oj)for@$y1;&$_($rj)for@$y1;&$_($uj)for@$vj;&$_($zj)for@$y1;&$_($Cj)for@$Dj;&$_($Ij)for@$y1;&$_($Lj)for@$Mj;&$_($Qj)for@$y1;&$_($Tj)for@$y1;&$_($Wj)for@$Xj;&$_($ck)for@$dk;&$_($mk)for@$nk;&$_($sk)for@$y1;&$_($vk)for@$wk;&$_($Ak)for@$y1;&$_($Ek)for@$y1;&$_($Hk)for@$y1;&$_($Kk)for@$Lk;&$_($Nk)for@$Ok;&$_($Wk)for@$y1;&$_($cl)for@$y1;&$_($gl)for@$y1;&$_($il)for@$jl;ni->run(@ARGV);
__DATA__
