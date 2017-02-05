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
$ni::self = bless {}, '/class/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::named{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::name {my %h = @_; @ni::named{keys %h} = values %h}
sub ni::eval {eval shift}
*{'/class/ni::def'} = sub {
  my ($self, %kvs) = @_;
  *$_ = $ni::named{"ni.def:$_"} = $kvs{$_} for keys %kvs;
};
*{'/class/fn::new'} = sub {
  my $self = bless {code => $_[1]}, $_[0];
  $self->compile;
  $self;
};
*{'/class/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/class/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
*{'/class/fn::(('}    = sub {};
*{'/class/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'/class/fn::(bool'} = sub {1};
sub fn($) {'/class/fn'->new(shift)}
_
$c=q'ni:/behavior/branch';$d=q'applied_to';$e=q'/class/class';$f={$e,1};$g='name';$h='slices';$i=q'/class/slice';$j={$e,2,$i,1};$k=q'/class/behavior';$l=q'/class/fn';$m={$l,1};$n={};$o='ctor';$p=undef;$q='dtor';$r='isa';$s=[];$t='methods';$u='DESTROY';$v='code';$w=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';$x=bless({$v,$w},$l);$y='new';$z=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self, @_) for @{ref($self) . "::ctors"};
$self;';$A=bless({$v,$z},$l);$B={$u,$x,$y,$A};$C=q'/behavior/lifecycle';$D=bless({$d,$n,$o,$p,$q,$p,$r,$s,$t,$B,$g,$C},$i);$E=q'/behavior/slice';$F=$D;$G={};$H=q'shift->compile';$I=bless({$v,$H},$l);$J=[];$K='compile';$L=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/behavior/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';$M=bless({$v,$L},$l);$N=q'instantiate';$O=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';$P=bless({$v,$O},$l);$Q={$K,$M,$N,$P};$R=q'/behavior/fn_init';$S=bless({$d,$G,$o,$I,$q,$p,$r,$J,$t,$Q,$g,$R},$i);$T=$S;$U={};$V=[];$W=q'serialize';$X=q'annotations';$Y=[];$Z=q'local $_;
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
$quote->quote_blessed(\\%state, ref $self);';$c1=bless({$X,$Y,$v,$Z},$l);$d1={$W,$c1};$e1=bless({$d,$U,$o,$p,$q,$p,$r,$V,$t,$d1,$g,$p},$i);$f1=$e1;$g1=[$F,$T,$f1];$h1=bless({$d,$m,$g,$l,$h,$g1},$e);$i1=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';$j1=bless({$v,$i1},$l);$k1=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{isa}};
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';$l1=bless({$v,$k1},$l);$m1=q'/behavior/slice::apply';$n1=q'/behavior/slice::apply_unsafe';$o1=q'/class/slice::apply';$p1=q'/class/slice::apply_unsafe';$q1={};$r1=[];$s1='apply';$t1=q'apply_unsafe';$u1={$s1,$j1,$t1,$l1};$v1=bless({$d,$q1,$o,$p,$q,$p,$r,$r1,$t,$u1,$g,$E},$i);$w1=$v1;$x1={$E,1,$e,1,$l,1,$i,1};$y1=[];$z1={$u,$x,$y,$A};$A1=bless({$d,$x1,$r,$y1,$t,$z1},$E);$B1={$e,1,$i,1};$C1=[];$D1='package';$E1=q'shift->{name}';$F1=bless({$v,$E1},$l);$G1={$D1,$F1};$H1=bless({$d,$B1,$o,$p,$q,$p,$r,$C1,$t,$G1},$E);$I1={$e,1,$i,1};$J1=[];$K1=q'namespace';$L1='\'ni\'';$M1=bless({$v,$L1},$l);$N1={$K1,$M1};$O1=bless({$d,$I1,$o,$p,$q,$p,$r,$J1,$t,$N1},$E);$P1={$e,1,$i,1};$Q1=q'my $self = shift; ni::name($self->name, $self) if defined $self->name';$R1=bless({$v,$Q1},$l);$S1={$e,1,$i,1};$T1=[];$U1=q'my $s = shift;
defined $$s{name} ? $s->namespace . ":$$s{name}" : undef;';$V1=bless({$v,$U1},$l);$W1={$g,$V1};$X1=bless({$d,$S1,$o,$p,$q,$p,$r,$T1,$t,$W1},$E);$Y1=[$X1];$Z1={};$c2=bless({$d,$P1,$o,$R1,$q,$p,$r,$Y1,$t,$Z1},$E);$d2=[$A1,$H1,$O1,$c2];$e2=bless({$d,$j,$g,$k,$h,$d2},$e);$f2={$e,1};$g2=[];$h2='add';$i2=q'local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @_}
$self;';$j2=bless({$v,$i2},$l);$k2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{slices}};
$self;';$l2=bless({$v,$k2},$l);$m2={$h2,$j2,$s1,$l2};$n2=bless({$d,$f2,$o,$p,$q,$p,$r,$g2,$t,$m2},$E);$o2={};$p2=[];$q2='def';$r2=q'shift->add(ni(\'ni:/class/slice\')->new(undef, @_))';$s2=bless({$v,$r2},$l);$t2={$q2,$s2};$u2=q'/behavior/class_method_def';$v2=bless({$d,$o2,$o,$p,$q,$p,$r,$p2,$t,$t2,$g,$u2},$i);$w2=$v2;$x2=[$e2,$n2,$w2];$y2=bless({$d,$f,$g,$e,$h,$x2},$e);$z2={$i,1};$A2={$E,1,$i,1};$B2=[];$C2={$s1,$j1,$t1,$l1};$D2=bless({$d,$A2,$r,$B2,$t,$C2},$E);$E2={$i,1};$F2=[];$G2=q'my ($class, $name, @methods) = @_;
my $self = &{\'/behavior/slice::instantiate\'}($class, @methods);
$$self{name} = $name;
$self;';$H2=bless({$v,$G2},$l);$I2={$N,$H2};$J2=bless({$d,$E2,$o,$p,$q,$p,$r,$F2,$t,$I2},$E);$K2={};$L2=[];$M2=[];$N2=q'local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq \'ni:/behavior/slice\') {
  my %methods;
  my @ks = sort keys %{$$self{methods}};
  @methods{@ks} = map $quote->quote($_), @{$$self{methods}}{@ks};
  for my $p (sort keys %{$$self{applied_to}}) {
    $quote->boot_side_effect(
      "\\*{" . $quote->quote("$p\\::$_") . "}=\\\\\\&$methods{$_};")
      for @ks;
  }
}

my %state = %$self;
$state{applied_to} = {};
my $g = $quote->allocate_gensym($self,
  $quote->quote_blessed(\\%state, ref $self));
$quote->side_effect("$g\\->apply_unsafe(" . $quote->quote($_) . ");")
  for sort keys %{$$self{applied_to}};
$g;';$O2=bless({$X,$M2,$v,$N2},$l);$P2={$W,$O2};$Q2=bless({$d,$K2,$o,$p,$q,$p,$r,$L2,$t,$P2,$g,$p},$i);$R2=$Q2;$S2=[$e2,$D2,$J2,$R2];$T2=bless({$d,$z2,$g,$i,$h,$S2},$e);$U2={};$V2=[];$W2={$h2,$j2,$s1,$l2};$X2=q'/behavior/branch';$Y2=bless({$d,$U2,$o,$p,$q,$p,$r,$V2,$t,$W2,$g,$X2},$i);$Z2=$Y2;$c3=q'ni:/behavior/class_init';$d3={};$e3=q'my $s = shift; $s->apply($s->package)';$f3=bless({$v,$e3},$l);$g3=[];$h3=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map ref($_) ? $_ : ni($_), @slices]};';$i3=bless({$v,$h3},$l);$j3={$N,$i3};$k3=q'/behavior/class_init';$l3=bless({$d,$d3,$o,$f3,$q,$p,$r,$g3,$t,$j3,$g,$k3},$i);$m3=$l3;$n3=q'ni:/behavior/class_method_def';$o3=q'ni:/behavior/fn_init';$p3=q'ni:/behavior/image_init';$q3={};$r3=[];$s3=[];$t3=q'my $class = shift;
+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  visited      => {},
  ordering     => []};';$u3=bless({$X,$s3,$v,$t3},$l);$v3={$N,$u3};$w3=q'/behavior/image_init';$x3=bless({$d,$q3,$o,$p,$q,$p,$r,$r3,$t,$v3,$g,$w3},$i);$y3=q'/class/image';$z3=$x3;$A3=q'ni:/behavior/image_quoting';$B3={};$C3=[];$D3='address';$E3=[];$F3=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';$G3=bless({$X,$E3,$v,$F3},$l);$H3=q'allocate_gensym';$I3=[];$J3=q'my $self = shift;
my $a = $self->address(shift);
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';$K3=bless({$X,$I3,$v,$J3},$l);$L3=q'boot_side_effect';$M3=[];$N3=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$O3=bless({$X,$M3,$v,$N3},$l);$P3=q'circular_links';$Q3=[];$R3=q'local $_;
my $self = shift;
map "$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';$S3=bless({$X,$Q3,$v,$R3},$l);$T3='gensym';$U3=[];$V3=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';$W3=bless({$X,$U3,$v,$V3},$l);$X3=q'is_circular';$Y3=[];$Z3=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';$c4=bless({$X,$Y3,$v,$Z3},$l);$d4=q'partial_image';$e4=[];$f4=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\%ni::named=\\%$g;");';$g4=bless({$X,$e4,$v,$f4},$l);$h4='quote';$i4=[];$j4=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';$k4=bless({$X,$i4,$v,$j4},$l);$l4=q'quote_array';$m4=[];$n4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';$o4=bless({$X,$m4,$v,$n4},$l);$p4=q'quote_blessed';$q4=[];$r4=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';$s4=bless({$X,$q4,$v,$r4},$l);$t4=q'quote_class';$u4=[];$v4=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if exists $ni::named{"ni:$class"};';$w4=bless({$X,$u4,$v,$v4},$l);$x4=q'quote_hash';$y4=[];$z4=q'local $_;
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
\'{\' . join(",", @qs) . \'}\';';$A4=bless({$X,$y4,$v,$z4},$l);$B4=q'quote_object';$C4=[];$D4=q'my $self = shift;
return $_[0]->serialize($self) if $_[0]->can(\'serialize\');
$self->quote_blessed(@_);';$E4=bless({$X,$C4,$v,$D4},$l);$F4=q'quote_scalar';$G4=[];$H4=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
length $v > 8 ? "q\'$v\'" : "\'$v\'";';$I4=bless({$X,$G4,$v,$H4},$l);$J4=q'quote_value';$K4=[];$L4=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';$M4=bless({$X,$K4,$v,$L4},$l);$N4=q'side_effect';$O4=[];$P4=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';$Q4=bless({$X,$O4,$v,$P4},$l);$R4='write';$S4=[];$T4=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  "#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<\'_\');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n",
  @{$$self{definitions}}{@{$$self{ordering}}},
  $self->circular_links,
  @{$$self{side_effects}},
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n";';$U4=bless({$X,$S4,$v,$T4},$l);$V4={$D3,$G3,$H3,$K3,$L3,$O3,$P3,$S3,$T3,$W3,$X3,$c4,$d4,$g4,$h4,$k4,$l4,$o4,$p4,$s4,$t4,$w4,$x4,$A4,$B4,$E4,$F4,$I4,$J4,$M4,$N4,$Q4,$R4,$U4};$W4=q'/behavior/image_quoting';$X4=bless({$d,$B3,$o,$p,$q,$p,$r,$C3,$t,$V4,$g,$W4},$i);$Y4=$X4;$Z4=q'ni:/behavior/lifecycle';$c5=q'ni:/behavior/mapped_to_package';$d5={};$e5=[];$f5={$D1,$F1};$g5=q'/behavior/mapped_to_package';$h5=bless({$d,$d5,$o,$p,$q,$p,$r,$e5,$t,$f5,$g,$g5},$i);$i5=$h5;$j5=q'ni:/behavior/named';$k5={};$l5=[];$m5={$g,$V1};$n5=q'/behavior/named';$o5=bless({$d,$k5,$o,$p,$q,$p,$r,$l5,$t,$m5,$g,$n5},$i);$p5=$o5;$q5=q'ni:/behavior/named_persistent';$r5={};$s5=[$X1];$t5={};$u5=q'/behavior/named_persistent';$v5=bless({$d,$r5,$o,$R1,$q,$p,$r,$s5,$t,$t5,$g,$u5},$i);$w5=$v5;$x5=q'ni:/behavior/named_transient';$y5={};$z5=q'my $s = shift; Scalar::Util::weaken($ni::named{$s->name} = $s)';$A5=bless({$v,$z5},$l);$B5=q'delete $ni::named{shift->name}';$C5=bless({$v,$B5},$l);$D5=[$X1];$E5={};$F5=q'/behavior/named_transient';$G5=bless({$d,$y5,$o,$A5,$q,$C5,$r,$D5,$t,$E5,$g,$F5},$i);$H5=$G5;$I5=q'ni:/behavior/ni_namespaced';$J5={};$K5=[];$L5={$K1,$M1};$M5=q'/behavior/ni_namespaced';$N5=bless({$d,$J5,$o,$p,$q,$p,$r,$K5,$t,$L5,$g,$M5},$i);$O5=$N5;$P5=q'ni:/behavior/slice';$Q5=q'ni:/behavior/slice_init';$R5={};$S5=[];$T5=q'my $class = shift;
my @isa;
push @isa, shift while ref $_[0];
my %args = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  isa        => \\@isa,
  methods    => \\%args};';$U5=bless({$v,$T5},$l);$V5=q'instantiation';$W5=q'my $self = shift;
(@{$$self{isa}},
 %{$$self{methods}},
 %$self{qw/applied_to ctor dtor/});';$X5=bless({$v,$W5},$l);$Y5={$N,$U5,$V5,$X5};$Z5=q'/behavior/slice_init';$c6=bless({$d,$R5,$o,$p,$q,$p,$r,$S5,$t,$Y5,$g,$Z5},$i);$d6=$c6;$e6=q'ni:/behavior/slice_named_init';$f6={};$g6=[];$h6={$N,$H2};$i6=q'/behavior/slice_named_init';$j6=bless({$d,$f6,$o,$p,$q,$p,$r,$g6,$t,$h6,$g,$i6},$i);$k6=$j6;$l6=q'ni:/behavior/tag';$m6={};$n6=[];$o6=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';$p6=bless({$v,$o6},$l);$q6={$s1,$p6};$r6=q'/behavior/tag';$s6=bless({$d,$m6,$o,$p,$q,$p,$r,$n6,$t,$q6,$g,$r6},$i);$t6=$s6;$u6=q'ni:/class/behavior';$v6=q'ni:/class/class';$w6=q'ni:/class/fn';$x6=q'ni:/class/image';$y6={$y3,1};$z6=[$z3,$Y4];$A6=bless({$d,$y6,$g,$y3,$h,$z6},$e);$B6=q'ni:/class/ni';$C6=q'/class/ni';$D6={$C6,1};$E6={};$F6=[];$G6='defclass';$H6=q'shift; ni(\'ni:/class/class\')->new("/class/$_[0]", @_[1..$#_])';$I6=bless({$v,$H6},$l);$J6='defslice';$K6=q'shift; ni(\'ni:/class/slice\')->new("/behavior/$_[0]", @_[1..$#_])';$L6=bless({$v,$K6},$l);$M6={$G6,$I6,$J6,$L6};$N6=bless({$d,$E6,$o,$p,$q,$p,$r,$F6,$t,$M6,$g,$p},$i);$O6=$N6;$P6={};$Q6=[];$R6=q'internal/eval';$S6=[];$T6=q'my $self = shift;
for (@_) {
  print "$_ -> ";
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;';$U6=bless({$X,$S6,$v,$T6},$l);$V6=q'internal/image';$W6=[];$X6=q'my $self = shift;
my $q = ni(\'ni:/class/image\')->new->partial_image(grep !/^ni\\.eval/, keys %ni::named);
$q->write(\\*STDOUT);
0;';$Y6=bless({$X,$W6,$v,$X6},$l);$Z6='run';$c7=[];$d7=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';$e7=bless({$X,$c7,$v,$d7},$l);$f7={$R6,$U6,$V6,$Y6,$Z6,$e7};$g7=bless({$d,$P6,$o,$p,$q,$p,$r,$Q6,$t,$f7,$g,$p},$i);$h7=$g7;$i7=[$O6,$h7];$j7=bless({$d,$D6,$g,$C6,$h,$i7},$e);$k7=q'ni:/class/slice';$l7={$c,$Z2,$c3,$m3,$n3,$w2,$o3,$T,$p3,$z3,$A3,$Y4,$Z4,$F,$c5,$i5,$j5,$p5,$q5,$w5,$x5,$H5,$I5,$O5,$P5,$w1,$Q5,$d6,$e6,$k6,$l6,$t6,$u6,$e2,$v6,$y2,$w6,$h1,$x6,$A6,$B6,$j7,$k7,$T2};*{$p1}=\&$l1;*{$o1}=\&$j1;*{$n1}=\&$l1;*{$m1}=\&$j1;$D->apply_unsafe($E);$D->apply_unsafe($e);$D->apply_unsafe($l);$D->apply_unsafe($i);$S->apply_unsafe($l);$e1->apply_unsafe($l);$v1->apply_unsafe($E);$v1->apply_unsafe($i);$v2->apply_unsafe($e);$Q2->apply_unsafe($i);$Y2->apply_unsafe($e);$l3->apply_unsafe($e);$x3->apply_unsafe($y3);$X4->apply_unsafe($y3);$h5->apply_unsafe($e);$h5->apply_unsafe($i);$o5->apply_unsafe($e);$o5->apply_unsafe($i);$v5->apply_unsafe($e);$v5->apply_unsafe($i);$N5->apply_unsafe($e);$N5->apply_unsafe($i);$c6->apply_unsafe($E);$j6->apply_unsafe($i);$N6->apply_unsafe($C6);$g7->apply_unsafe($C6);%ni::named=%$l7;ni->run(@ARGV);
__DATA__
