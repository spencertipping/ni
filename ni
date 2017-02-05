#!/usr/bin/env perl
$ni::license=<<'_';
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
*{"/class/ni::def"} = sub {
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
$c=q'ni:/behavior/branch';
$d=q'applied_to';
$e=q'/class/class';
$f={$e,1};
$g='name';
$h='slices';
$i=q'/class/slice';
$j={$e,2,$i,1};
$k=q'/class/behavior';
$l=q'/class/fn';
$m={$l,1};
$n={};
$o='ctor';
$p=undef;
$q='dtor';
$r='isa';
$s=[];
$t='methods';
$u='DESTROY';
$v='code';
$w=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';
$x=bless({$v,$w},$l);
$y='new';
$z=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self, @_) for @{ref($self) . "::ctors"};
$self;';
$A=bless({$v,$z},$l);
$B={$u,$x,$y,$A};
$C=q'/behavior/lifecycle';
$D=bless({$d,$n,$o,$p,$q,$p,$r,$s,$t,$B,$g,$C},$i);
$E=q'/behavior/slice';
$F=$D;
$G=q'shift->compile';
$H=bless({$v,$G},$l);
$I=[];
$J='compile';
$K=q'local $@;
my $self = shift;
my ($eval_n) = eval(\'__FILE__\') =~ /eval (\\d+)/;
ni::name "ni.eval:$eval_n", $self;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/behavior/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';
$L=bless({$v,$K},$l);
$M=q'instantiate';
$N=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';
$O=bless({$v,$N},$l);
$P={$J,$L,$M,$O};
$Q=q'/behavior/fn_init';
$R=bless({$d,$n,$o,$H,$q,$p,$r,$I,$t,$P,$g,$Q},$i);
$S=$R;
$T={};
$U=[];
$V=q'serialize';
$W=q'annotations';
$X=[];
$Y=q'local $_;
my ($self, $quote) = @_;
$quote->quote_class(ref $self);

(my $code = $$self{code}) =~ s/^(?:\\h*\\n)*|\\s*$//g;
my @lines = split /\\n/, $code;
my $spaces = length $code;
for (@lines) {
  $spaces = length $1 if /^(\\h*)\\S/ && length $1 < $spaces;
}
my $pattern = \' \' x $spaces;
s/^$pattern// for @lines;

my %state = %$self;
delete $state{fn};
$state{code} = join "\\n", @lines;
$quote->quote_blessed(\\%state, ref $self);';
$Z=bless({$W,$X,$v,$Y},$l);
$c1={$V,$Z};
$d1=bless({$d,$T,$o,$p,$q,$p,$r,$U,$t,$c1,$g,$p},$i);
$e1=$d1;
$f1=[$F,$S,$e1];
$g1=bless({$d,$m,$g,$l,$h,$f1},$e);
$h1=bless({$v,$n},$l);
$i1=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{isa}};
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';
$j1=bless({$v,$i1},$l);
$k1=q'/behavior/slice::apply';
$l1=q'/behavior/slice::apply_unsafe';
$m1=q'/class/slice::apply';
$n1=q'/class/slice::apply_unsafe';
$o1={};
$p1=[];
$q1='apply';
$r1=q'apply_unsafe';
$s1={$q1,$h1,$r1,$j1};
$t1=bless({$d,$o1,$o,$p,$q,$p,$r,$p1,$t,$s1,$g,$E},$i);
$u1=$t1;
$v1={$E,1,$e,1,$l,1,$i,1};
$w1=[];
$x1={$u,$x,$y,$A};
$y1=bless({$d,$v1,$r,$w1,$t,$x1},$E);
$z1={$e,1,$i,1};
$A1=[];
$B1='package';
$C1=q'shift->{name}';
$D1=bless({$v,$C1},$l);
$E1={$B1,$D1};
$F1=bless({$d,$z1,$o,$p,$q,$p,$r,$A1,$t,$E1},$E);
$G1={$e,1,$i,1};
$H1=[];
$I1=q'namespace';
$J1='\'ni\'';
$K1=bless({$v,$J1},$l);
$L1={$I1,$K1};
$M1=bless({$d,$G1,$o,$p,$q,$p,$r,$H1,$t,$L1},$E);
$N1={$e,1,$i,1};
$O1=q'my $self = shift; ni::name($self->name, $self) if defined $self->name';
$P1=bless({$v,$O1},$l);
$Q1={$e,1,$i,1};
$R1=[];
$S1=q'my $s = shift;
defined $$s{name} ? $s->namespace . ":$$s{name}" : undef;';
$T1=bless({$v,$S1},$l);
$U1={$g,$T1};
$V1=bless({$d,$Q1,$o,$p,$q,$p,$r,$R1,$t,$U1},$E);
$W1=[$V1];
$X1={};
$Y1=bless({$d,$N1,$o,$P1,$q,$p,$r,$W1,$t,$X1},$E);
$Z1=[$y1,$F1,$M1,$Y1];
$c2=bless({$d,$j,$g,$k,$h,$Z1},$e);
$d2={$e,1};
$e2=[];
$f2='add';
$g2=q'local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @_}
$self;';
$h2=bless({$v,$g2},$l);
$i2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{slices}};
$self;';
$j2=bless({$v,$i2},$l);
$k2={$f2,$h2,$q1,$j2};
$l2=bless({$d,$d2,$o,$p,$q,$p,$r,$e2,$t,$k2},$E);
$m2={};
$n2=[];
$o2='def';
$p2=q'shift->add(ni(\'ni:/class/slice\')->new(undef, @_))';
$q2=bless({$v,$p2},$l);
$r2={$o2,$q2};
$s2=q'/behavior/class_method_def';
$t2=bless({$d,$m2,$o,$p,$q,$p,$r,$n2,$t,$r2,$g,$s2},$i);
$u2=$t2;
$v2=[$c2,$l2,$u2];
$w2=bless({$d,$f,$g,$e,$h,$v2},$e);
$x2={$i,1};
$y2={$E,1,$i,1};
$z2=[];
$A2={$q1,$h1,$r1,$j1};
$B2=bless({$d,$y2,$r,$z2,$t,$A2},$E);
$C2={$i,1};
$D2=[];
$E2=q'my ($class, $name, @methods) = @_;
my $self = &{\'/behavior/slice::instantiate\'}($class, @methods);
$$self{name} = $name;
$self;';
$F2=bless({$v,$E2},$l);
$G2={$M,$F2};
$H2=bless({$d,$C2,$o,$p,$q,$p,$r,$D2,$t,$G2},$E);
$I2={};
$J2=[];
$K2=[];
$L2=q'local $_;
my ($self, $quote) = @_;
my $name = $self->name;
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

$quote->quote_class(ref $self);
my %state = %$self;
$state{applied_to} = {};
my $g = $quote->allocate_gensym($self,
  $quote->quote_blessed(\\%state, ref $self));
$quote->side_effect("$g\\->apply_unsafe(" . $quote->quote($_) . ");")
  for sort keys %{$$self{applied_to}};
$g;';
$M2=bless({$W,$K2,$v,$L2},$l);
$N2={$V,$M2};
$O2=bless({$d,$I2,$o,$p,$q,$p,$r,$J2,$t,$N2,$g,$p},$i);
$P2=$O2;
$Q2=[$c2,$B2,$H2,$P2];
$R2=bless({$d,$x2,$g,$i,$h,$Q2},$e);
$S2={};
$T2=[];
$U2={$f2,$h2,$q1,$j2};
$V2=q'/behavior/branch';
$W2=bless({$d,$S2,$o,$p,$q,$p,$r,$T2,$t,$U2,$g,$V2},$i);
$X2=$W2;
$Y2=q'ni:/behavior/class_init';
$Z2={};
$c3=q'my $s = shift; $s->apply($s->package)';
$d3=bless({$v,$c3},$l);
$e3=[];
$f3=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map ref($_) ? $_ : ni($_), @slices]};';
$g3=bless({$v,$f3},$l);
$h3={$M,$g3};
$i3=q'/behavior/class_init';
$j3=bless({$d,$Z2,$o,$d3,$q,$p,$r,$e3,$t,$h3,$g,$i3},$i);
$k3=$j3;
$l3=q'ni:/behavior/class_method_def';
$m3=q'ni:/behavior/fn_init';
$n3=q'ni:/behavior/image_init';
$o3={};
$p3=[];
$q3=[];
$r3=q'my $class = shift;
+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  side_effects => [],
  visited      => {},
  ordering     => []};';
$s3=bless({$W,$q3,$v,$r3},$l);
$t3={$M,$s3};
$u3=q'/behavior/image_init';
$v3=bless({$d,$o3,$o,$p,$q,$p,$r,$p3,$t,$t3,$g,$u3},$i);
$w3=q'/class/image';
$x3=$v3;
$y3=q'ni:/behavior/image_quoting';
$z3={};
$A3=[];
$B3='address';
$C3=[];
$D3=q'return \'undef\' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 64;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';
$E3=bless({$W,$C3,$v,$D3},$l);
$F3=q'allocate_gensym';
$G3=[];
$H3=q'my $self = shift;
my $a = $self->address(shift);
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';
$I3=bless({$W,$G3,$v,$H3},$l);
$J3=q'boot_side_effect';
$K3=[];
$L3=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$M3=bless({$W,$K3,$v,$L3},$l);
$N3=q'circular_links';
$O3=[];
$P3=q'local $_;
my $self = shift;
map "$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';
$Q3=bless({$W,$O3,$v,$P3},$l);
$R3='gensym';
$S3=[];
$T3=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;';
$U3=bless({$W,$S3,$v,$T3},$l);
$V3=q'is_circular';
$W3=[];
$X3=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';
$Y3=bless({$W,$W3,$v,$X3},$l);
$Z3=q'partial_image';
$c4=[];
$d4=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\%ni::named=\\%$g;");';
$e4=bless({$W,$c4,$v,$d4},$l);
$f4='quote';
$g4=[];
$h4=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';
$i4=bless({$W,$g4,$v,$h4},$l);
$j4=q'quote_array';
$k4=[];
$l4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#{$v};
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';
$m4=bless({$W,$k4,$v,$l4},$l);
$n4=q'quote_blessed';
$o4=[];
$p4=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';
$q4=bless({$W,$o4,$v,$p4},$l);
$r4=q'quote_class';
$s4=[];
$t4=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if exists $ni::named{"ni:$class"};';
$u4=bless({$W,$s4,$v,$t4},$l);
$v4=q'quote_hash';
$w4=[];
$x4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
my @ks = sort keys %$v;
$self->is_circular($$v{$_})
  && push @{$$self{circular}}, [$a, "{" . $self->quote($_) . "}",
                                    $self->address($$v{$_})]
for @ks;
\'{\' . join(\',\', map $self->quote($_) . "," . $self->quote($$v{$_}), @ks)
    . \'}\';';
$y4=bless({$W,$w4,$v,$x4},$l);
$z4=q'quote_object';
$A4=[];
$B4=q'my $self = shift;
return $_[0]->serialize($self) if $_[0]->can(\'serialize\');
$self->quote_blessed(@_);';
$C4=bless({$W,$A4,$v,$B4},$l);
$D4=q'quote_scalar';
$E4=[];
$F4=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
length $v > 8 ? "q\'$v\'" : "\'$v\'";';
$G4=bless({$W,$E4,$v,$F4},$l);
$H4=q'quote_value';
$I4=[];
$J4=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';
$K4=bless({$W,$I4,$v,$J4},$l);
$L4=q'side_effect';
$M4=[];
$N4=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$O4=bless({$W,$M4,$v,$N4},$l);
$P4='write';
$Q4=[];
$R4=q'local $_;
my ($self, $fh) = @_;
$fh->print($_) for
  "#!/usr/bin/env perl\\n",
  "\\$ni::license=<<\'_\';\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<\'_\')}\\n", $ni::boot, "\\n_\\n",
  map("$_\\n", @{$$self{definitions}}{@{$$self{ordering}}},
              $self->circular_links,
              @{$$self{side_effects}}),
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n";';
$S4=bless({$W,$Q4,$v,$R4},$l);
$T4={$B3,$E3,$F3,$I3,$J3,$M3,$N3,$Q3,$R3,$U3,$V3,$Y3,$Z3,$e4,$f4,$i4,$j4,$m4,$n4,$q4,$r4,$u4,$v4,$y4,$z4,$C4,$D4,$G4,$H4,$K4,$L4,$O4,$P4,$S4};
$U4=q'/behavior/image_quoting';
$V4=bless({$d,$z3,$o,$p,$q,$p,$r,$A3,$t,$T4,$g,$U4},$i);
$W4=$V4;
$X4=q'ni:/behavior/lifecycle';
$Y4=q'ni:/behavior/mapped_to_package';
$Z4={};
$c5=[];
$d5={$B1,$D1};
$e5=q'/behavior/mapped_to_package';
$f5=bless({$d,$Z4,$o,$p,$q,$p,$r,$c5,$t,$d5,$g,$e5},$i);
$g5=$f5;
$h5=q'ni:/behavior/named';
$i5={};
$j5=[];
$k5={$g,$T1};
$l5=q'/behavior/named';
$m5=bless({$d,$i5,$o,$p,$q,$p,$r,$j5,$t,$k5,$g,$l5},$i);
$n5=$m5;
$o5=q'ni:/behavior/named_persistent';
$p5={};
$q5=[$V1];
$r5={};
$s5=q'/behavior/named_persistent';
$t5=bless({$d,$p5,$o,$P1,$q,$p,$r,$q5,$t,$r5,$g,$s5},$i);
$u5=$t5;
$v5=q'ni:/behavior/named_transient';
$w5={};
$x5=q'my $s = shift; Scalar::Util::weaken($ni::named{$s->name} = $s)';
$y5=bless({$v,$x5},$l);
$z5=q'delete $ni::named{shift->name}';
$A5=bless({$v,$z5},$l);
$B5=[$V1];
$C5={};
$D5=q'/behavior/named_transient';
$E5=bless({$d,$w5,$o,$y5,$q,$A5,$r,$B5,$t,$C5,$g,$D5},$i);
$F5=$E5;
$G5=q'ni:/behavior/ni_namespaced';
$H5={};
$I5=[];
$J5={$I1,$K1};
$K5=q'/behavior/ni_namespaced';
$L5=bless({$d,$H5,$o,$p,$q,$p,$r,$I5,$t,$J5,$g,$K5},$i);
$M5=$L5;
$N5=q'ni:/behavior/slice';
$O5=q'ni:/behavior/slice_init';
$P5={};
$Q5=[];
$R5=q'my $class = shift;
my @isa;
push @isa, shift while ref $_[0];
my %args = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  isa        => \\@isa,
  methods    => \\%args};';
$S5=bless({$v,$R5},$l);
$T5=q'instantiation';
$U5=q'my $self = shift;
(@{$$self{isa}},
 %{$$self{methods}},
 %$self{qw/applied_to ctor dtor/});';
$V5=bless({$v,$U5},$l);
$W5={$M,$S5,$T5,$V5};
$X5=q'/behavior/slice_init';
$Y5=bless({$d,$P5,$o,$p,$q,$p,$r,$Q5,$t,$W5,$g,$X5},$i);
$Z5=$Y5;
$c6=q'ni:/behavior/slice_named_init';
$d6={};
$e6=[];
$f6={$M,$F2};
$g6=q'/behavior/slice_named_init';
$h6=bless({$d,$d6,$o,$p,$q,$p,$r,$e6,$t,$f6,$g,$g6},$i);
$i6=$h6;
$j6=q'ni:/behavior/tag';
$k6={};
$l6=[];
$m6=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';
$n6=bless({$v,$m6},$l);
$o6={$q1,$n6};
$p6=q'/behavior/tag';
$q6=bless({$d,$k6,$o,$p,$q,$p,$r,$l6,$t,$o6,$g,$p6},$i);
$r6=$q6;
$s6=q'ni:/class/behavior';
$t6=q'ni:/class/class';
$u6=q'ni:/class/fn';
$v6=q'ni:/class/image';
$w6={$w3,1};
$x6=[$x3,$W4];
$y6=bless({$d,$w6,$g,$w3,$h,$x6},$e);
$z6=q'ni:/class/ni';
$A6=q'/class/ni';
$B6={$A6,1};
$C6={};
$D6=[];
$E6='defclass';
$F6=q'shift; ni(\'ni:/class/class\')->new("/class/$_[0]", @_[1..$#_])';
$G6=bless({$v,$F6},$l);
$H6='defslice';
$I6=q'shift; ni(\'ni:/class/slice\')->new("/behavior/$_[0]", @_[1..$#_])';
$J6=bless({$v,$I6},$l);
$K6={$E6,$G6,$H6,$J6};
$L6=bless({$d,$C6,$o,$p,$q,$p,$r,$D6,$t,$K6,$g,$p},$i);
$M6=$L6;
$N6={};
$O6=[];
$P6=q'internal/eval';
$Q6=[];
$R6=q'my $self = shift;
for (@_) {
  print "$_ -> ";
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}';
$S6=bless({$W,$Q6,$v,$R6},$l);
$T6=q'internal/image';
$U6=[];
$V6=q'my $self = shift;
my $q = ni(\'ni:/class/image\')->new->partial_image(grep !/^ni\\.eval/, keys %ni::named);
$q->write(\\*STDOUT);
0;';
$W6=bless({$W,$U6,$v,$V6},$l);
$X6='run';
$Y6=[];
$Z6=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';
$c7=bless({$W,$Y6,$v,$Z6},$l);
$d7={$P6,$S6,$T6,$W6,$X6,$c7};
$e7=bless({$d,$N6,$o,$p,$q,$p,$r,$O6,$t,$d7,$g,$p},$i);
$f7=$e7;
$g7=[$M6,$f7];
$h7=bless({$d,$B6,$g,$A6,$h,$g7},$e);
$i7=q'ni:/class/slice';
$j7={$c,$X2,$Y2,$k3,$l3,$u2,$m3,$S,$n3,$x3,$y3,$W4,$X4,$F,$Y4,$g5,$h5,$n5,$o5,$u5,$v5,$F5,$G5,$M5,$N5,$u1,$O5,$Z5,$c6,$i6,$j6,$r6,$s6,$c2,$t6,$w2,$u6,$g1,$v6,$y6,$z6,$h7,$i7,$R2};
*{$n1}=\&$j1;
*{$m1}=\&$h1;
*{$l1}=\&$j1;
*{$k1}=\&$h1;
$D->apply_unsafe($E);
$D->apply_unsafe($e);
$D->apply_unsafe($l);
$D->apply_unsafe($i);
$R->apply_unsafe($l);
$d1->apply_unsafe($l);
$t1->apply_unsafe($E);
$t1->apply_unsafe($i);
$t2->apply_unsafe($e);
$O2->apply_unsafe($i);
$W2->apply_unsafe($e);
$j3->apply_unsafe($e);
$v3->apply_unsafe($w3);
$V4->apply_unsafe($w3);
$f5->apply_unsafe($e);
$f5->apply_unsafe($i);
$m5->apply_unsafe($e);
$m5->apply_unsafe($i);
$t5->apply_unsafe($e);
$t5->apply_unsafe($i);
$L5->apply_unsafe($e);
$L5->apply_unsafe($i);
$Y5->apply_unsafe($E);
$h6->apply_unsafe($i);
$L6->apply_unsafe($A6);
$e7->apply_unsafe($A6);
%ni::named=%$j7;
ni->run(@ARGV);
__DATA__
