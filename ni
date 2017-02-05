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
use Scalar::Util;
chomp($ni::boot, $ni::license);
$ni::self = bless {}, '/class/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::named{$_[0]} || die "ni: failed to resolve $_[0]" : $ni::self}
sub ni::name {my %h = @_; delete @h{grep !defined, keys %h}; @ni::named{keys %h} = values %h}
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
$c='';
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
$p='dtor';
$q='isa';
$r=[];
$s='methods';
$t='DESTROY';
$u='code';
$v=q'local $_;
my $self = shift;
$_->($self) for @{ref($self) . "::dtors"};';
$w=bless({$u,$v},$l);
$x='new';
$y=q'local $_;
my $class = ref $_[0] ? shift->package : shift;
my $self = bless $class->instantiate(@_), $class;
$_->($self, @_) for @{ref($self) . "::ctors"};
$self;';
$z=bless({$u,$y},$l);
$A={$t,$w,$x,$z};
$B=q'/behavior/lifecycle';
$C=bless({$d,$n,$o,$c,$p,$c,$q,$r,$s,$A,$g,$B},$i);
$D=q'/behavior/slice';
$E=$C;
$F=q'shift->compile';
$G=bless({$u,$F},$l);
$H=[];
$I='compile';
$J=q'local $@;
my $self = shift;
$$self{fn} = ni::eval "sub{$$self{code}\\n}";
die "ni:/behavior/fn_init: failed to compile $$self{code}: $@" if $@;
$$self{fn};';
$K=bless({$u,$J},$l);
$L=q'instantiate';
$M=q'my $class = shift;
my $code  = pop;
+{code        => $code,
  annotations => [@_]};';
$N=bless({$u,$M},$l);
$O={$I,$K,$L,$N};
$P=q'/behavior/fn_init';
$Q=bless({$d,$n,$o,$G,$p,$c,$q,$H,$s,$O,$g,$P},$i);
$R=$Q;
$S=[];
$T=q'serialize';
$U=q'annotations';
$V=[];
$W=q'local $_;
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
$X=bless({$U,$V,$u,$W},$l);
$Y={$T,$X};
$Z=bless({$d,$n,$o,$c,$p,$c,$q,$S,$s,$Y,$g,$c},$i);
$c1=$Z;
$d1=[$E,$R,$c1];
$e1=bless({$d,$m,$g,$l,$h,$d1},$e);
$f1=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);';
$g1=bless({$u,$f1},$l);
$h1=q'local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{isa}};
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
*{"$p\\::(("} = sub {} if grep /^\\(/, keys %{$$self{methods}};
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;';
$i1=bless({$u,$h1},$l);
$j1=q'/behavior/slice::apply';
$k1=q'/behavior/slice::apply_unsafe';
$l1=q'/class/slice::apply';
$m1=q'/class/slice::apply_unsafe';
$n1={};
$o1=[];
$p1='apply';
$q1=q'apply_unsafe';
$r1={$p1,$g1,$q1,$i1};
$s1=bless({$d,$n1,$o,$c,$p,$c,$q,$o1,$s,$r1,$g,$D},$i);
$t1=$s1;
$u1={$D,1,$e,1,$l,1,$i,1};
$v1=[];
$w1={$t,$w,$x,$z};
$x1=bless({$d,$u1,$q,$v1,$s,$w1},$D);
$y1={$e,1,$i,1};
$z1=[];
$A1='package';
$B1=q'shift->{name}';
$C1=bless({$u,$B1},$l);
$D1={$A1,$C1};
$E1=bless({$d,$y1,$o,$c,$p,$c,$q,$z1,$s,$D1},$D);
$F1={$e,1,$i,1};
$G1=[];
$H1=q'namespace';
$I1='\'ni\'';
$J1=bless({$u,$I1},$l);
$K1={$H1,$J1};
$L1=bless({$d,$F1,$o,$c,$p,$c,$q,$G1,$s,$K1},$D);
$M1={$e,1,$i,1};
$N1=q'my $self = shift; ni::name($self->name, $self)';
$O1=bless({$u,$N1},$l);
$P1={$e,1,$i,1};
$Q1=[];
$R1=q'my $s = shift;
return undef unless defined $$s{name};
$s->namespace . ":$$s{name}";';
$S1=bless({$u,$R1},$l);
$T1={$g,$S1};
$U1=bless({$d,$P1,$o,$c,$p,$c,$q,$Q1,$s,$T1},$D);
$V1=[$U1];
$W1={};
$X1=bless({$d,$M1,$o,$O1,$p,$c,$q,$V1,$s,$W1},$D);
$Y1=[$x1,$E1,$L1,$X1];
$Z1=bless({$d,$j,$g,$k,$h,$Y1},$e);
$c2={$e,1};
$d2=[];
$e2='add';
$f2=q'local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @_}
$self;';
$g2=bless({$u,$f2},$l);
$h2=q'local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p}++;
$_->apply($p) for @{$$self{slices}};
$self;';
$i2=bless({$u,$h2},$l);
$j2={$e2,$g2,$p1,$i2};
$k2=bless({$d,$c2,$o,$c,$p,$c,$q,$d2,$s,$j2},$D);
$l2={};
$m2=[];
$n2='def';
$o2=q'shift->add(ni(\'ni:/class/slice\')->new(undef, @_))';
$p2=bless({$u,$o2},$l);
$q2={$n2,$p2};
$r2=q'/behavior/class_method_def';
$s2=bless({$d,$l2,$o,$c,$p,$c,$q,$m2,$s,$q2,$g,$r2},$i);
$t2=$s2;
$u2=[$Z1,$k2,$t2];
$v2=bless({$d,$f,$g,$e,$h,$u2},$e);
$w2={$i,1};
$x2={$D,1,$i,1};
$y2=[];
$z2={$p1,$g1,$q1,$i1};
$A2=bless({$d,$x2,$q,$y2,$s,$z2},$D);
$B2={$i,1};
$C2=[];
$D2=q'my ($class, $name, @methods) = @_;
my $self = &{\'/behavior/slice::instantiate\'}($class, @methods);
$$self{name} = $name;
$self;';
$E2=bless({$u,$D2},$l);
$F2={$L,$E2};
$G2=bless({$d,$B2,$o,$c,$p,$c,$q,$C2,$s,$F2},$D);
$H2={};
$I2=[];
$J2=[];
$K2=q'local $_;
my ($self, $quote) = @_;
if ($self->name eq \'ni:/behavior/slice\') {
  my %methods;
  my @ks = sort keys %{$$self{methods}};
  @methods{@ks} = map $quote->quote($_), @{$$self{methods}}{@ks};
  for my $p (sort keys %{$$self{applied_to}}) {
    $quote->boot_side_effect(
      "\\*{" . $quote->quote("$p\\::$_") . "}=\\\\\\&$methods{$_};")
      for sort keys %methods;
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
$L2=bless({$U,$J2,$u,$K2},$l);
$M2={$T,$L2};
$N2=bless({$d,$H2,$o,$c,$p,$c,$q,$I2,$s,$M2,$g,$c},$i);
$O2=$N2;
$P2=[$Z1,$A2,$G2,$O2];
$Q2=bless({$d,$w2,$g,$i,$h,$P2},$e);
$R2={};
$S2=[];
$T2=q'internal/image';
$U2=[];
$V2=q'my $self = shift;
my $q = ni(\'ni:/class/image\')->new->partial_image(keys %ni::named);
#$q->quote($self);
$q->write(\\*STDOUT);
0;';
$W2=bless({$U,$U2,$u,$V2},$l);
$X2='run';
$Y2=[];
$Z2=q'my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^--(.*)$/ && $self->can($1);
exit $self->default(@_);';
$c3=bless({$U,$Y2,$u,$Z2},$l);
$d3={$T2,$W2,$X2,$c3};
$e3=bless({$d,$R2,$o,$c,$p,$c,$q,$S2,$s,$d3,$g,$c},$i);
$f3=q'/class/ni';
$g3=$e3;
$h3=q'ni:/behavior/branch';
$i3={};
$j3=[];
$k3={$e2,$g2,$p1,$i2};
$l3=q'/behavior/branch';
$m3=bless({$d,$i3,$o,$c,$p,$c,$q,$j3,$s,$k3,$g,$l3},$i);
$n3=$m3;
$o3=q'ni:/behavior/class_init';
$p3=q'my $s = shift; $s->apply($s->package)';
$q3=bless({$u,$p3},$l);
$r3=[];
$s3=q'local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map ref($_) ? $_ : ni($_), @slices]};';
$t3=bless({$u,$s3},$l);
$u3={$L,$t3};
$v3=q'/behavior/class_init';
$w3=bless({$d,$V2,$o,$q3,$p,$c,$q,$r3,$s,$u3,$g,$v3},$i);
$x3=$w3;
$y3=q'ni:/behavior/class_method_def';
$z3=q'ni:/behavior/fn_init';
$A3=q'ni:/behavior/image_init';
$B3={};
$C3=[];
$D3=[];
$E3=q'my $class = shift;
+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  side_effects => [],
  visited      => {},
  ordering     => []};';
$F3=bless({$U,$D3,$u,$E3},$l);
$G3={$L,$F3};
$H3=q'/behavior/image_init';
$I3=bless({$d,$B3,$o,$c,$p,$c,$q,$C3,$s,$G3,$g,$H3},$i);
$J3=q'/class/image';
$K3=$I3;
$L3=q'ni:/behavior/image_quoting';
$M3={};
$N3=[];
$O3='address';
$P3=[];
$Q3=q'return "id:$_[1]" if !ref $_[1] && length $_[1] < 64;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);';
$R3=bless({$U,$P3,$u,$Q3},$l);
$S3=q'allocate_gensym';
$T3=[];
$U3=q'my $self = shift;
my $a = $self->address(shift);
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;';
$V3=bless({$U,$T3,$u,$U3},$l);
$W3=q'boot_side_effect';
$X3=[];
$Y3=q'unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$Z3=bless({$U,$X3,$u,$Y3},$l);
$c4=q'circular_links';
$d4=[];
$e4=q'local $_;
my $self = shift;
map "$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};';
$f4=bless({$U,$d4,$u,$e4},$l);
$g4='gensym';
$h4=[];
$i4=q'my $n = shift->{gensym_n}++;
my $s = \'$\' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int %n / 63;
}
$s;';
$j4=bless({$U,$h4,$u,$i4},$l);
$k4=q'is_circular';
$l4=[];
$m4=q'my $self = shift;
ref $$self{visited}{$self->address($_[0])};';
$n4=bless({$U,$l4,$u,$m4},$l);
$o4=q'partial_image';
$p4=[];
$q4=q'my $self = shift;
my %names;
@names{@_} = @ni::named{@_};
my $g = $self->quote(\\%names);
$self->side_effect("\\%ni::named=\\%$g;");';
$r4=bless({$U,$p4,$u,$q4},$l);
$s4='quote';
$t4=[];
$u4=q'my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
my $v = $$self{visited}{$a};
return ref $v ? \'undef\' : $v if defined $v;
$$self{visited}{$a} = \\\'undef\';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));';
$v4=bless({$U,$t4,$u,$u4},$l);
$w4=q'quote_array';
$x4=[];
$y4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
$self->is_circular($$v[$_])
  && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
for 0..$#v;
\'[\' . join(\',\', map $self->quote($_), @$v) . \']\';';
$z4=bless({$U,$x4,$u,$y4},$l);
$A4=q'quote_blessed';
$B4=[];
$C4=q'my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq \'HASH\' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";';
$D4=bless({$U,$B4,$u,$C4},$l);
$E4=q'quote_class';
$F4=[];
$G4=q'my ($self, $class) = @_;
$self->quote(ni"ni:$class") if exists $ni::named{"ni:$class"};';
$H4=bless({$U,$F4,$u,$G4},$l);
$I4=q'quote_hash';
$J4=[];
$K4=q'local $_;
my ($self, $v) = @_;
my $a = $self->address($v);
my @ks = sort keys %$v;
$self->is_circular($$v{$_})
  && push @{$$self{circular}}, [$a, "{" . $self->quote($_) . "}",
                                    $self->address($$v{$_})]
for @ks;
\'{\' . join(\',\', map $self->quote($_) . "," . $self->quote($$v{$_}), @ks)
    . \'}\';';
$L4=bless({$U,$J4,$u,$K4},$l);
$M4=q'quote_object';
$N4=[];
$O4=q'my $self = shift;
return $_[0]->serialize($self) if $_[0]->can(\'serialize\');
$self->quote_blessed(@_);';
$P4=bless({$U,$N4,$u,$O4},$l);
$Q4=q'quote_scalar';
$R4=[];
$S4=q'my $v = $_[1];
return \'undef\' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\'])/\\\\$1/g;
length $v > 8 ? "q\'$v\'" : "\'$v\'";';
$T4=bless({$U,$R4,$u,$S4},$l);
$U4=q'quote_value';
$V4=[];
$W4=q'my $self = shift;
return $self->quote_scalar($_[0]) unless ref $_[0];
return $self->quote_array($_[0])  if \'ARRAY\' eq ref $_[0];
return $self->quote_hash($_[0])   if \'HASH\'  eq ref $_[0];
die "cannot serialize $_[0]"      if \'CODE\'  eq ref $_[0];
$self->quote_object($_[0]);';
$X4=bless({$U,$V4,$u,$W4},$l);
$Y4=q'side_effect';
$Z4=[];
$c5=q'push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]';
$d5=bless({$U,$Z4,$u,$c5},$l);
$e5='write';
$f5=[];
$g5=q'local $_;
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
$h5=bless({$U,$f5,$u,$g5},$l);
$i5={$O3,$R3,$S3,$V3,$W3,$Z3,$c4,$f4,$g4,$j4,$k4,$n4,$o4,$r4,$s4,$v4,$w4,$z4,$A4,$D4,$E4,$H4,$I4,$L4,$M4,$P4,$Q4,$T4,$U4,$X4,$Y4,$d5,$e5,$h5};
$j5=q'/behavior/image_quoting';
$k5=bless({$d,$M3,$o,$c,$p,$c,$q,$N3,$s,$i5,$g,$j5},$i);
$l5=$k5;
$m5=q'ni:/behavior/lifecycle';
$n5=q'ni:/behavior/mapped_to_package';
$o5={};
$p5=[];
$q5={$A1,$C1};
$r5=q'/behavior/mapped_to_package';
$s5=bless({$d,$o5,$o,$c,$p,$c,$q,$p5,$s,$q5,$g,$r5},$i);
$t5=$s5;
$u5=q'ni:/behavior/named';
$v5={};
$w5=[];
$x5={$g,$S1};
$y5=q'/behavior/named';
$z5=bless({$d,$v5,$o,$c,$p,$c,$q,$w5,$s,$x5,$g,$y5},$i);
$A5=$z5;
$B5=q'ni:/behavior/named_persistent';
$C5={};
$D5=[$U1];
$E5={};
$F5=q'/behavior/named_persistent';
$G5=bless({$d,$C5,$o,$O1,$p,$c,$q,$D5,$s,$E5,$g,$F5},$i);
$H5=$G5;
$I5=q'ni:/behavior/named_transient';
$J5={};
$K5=q'my $s = shift; Scalar::Util::weaken($ni::named{$s->name} = $s)';
$L5=bless({$u,$K5},$l);
$M5=q'delete $ni::named{shift->name}';
$N5=bless({$u,$M5},$l);
$O5=[$U1];
$P5={};
$Q5=q'/behavior/named_transient';
$R5=bless({$d,$J5,$o,$L5,$p,$N5,$q,$O5,$s,$P5,$g,$Q5},$i);
$S5=$R5;
$T5=q'ni:/behavior/ni_namespaced';
$U5={};
$V5=[];
$W5={$H1,$J1};
$X5=q'/behavior/ni_namespaced';
$Y5=bless({$d,$U5,$o,$c,$p,$c,$q,$V5,$s,$W5,$g,$X5},$i);
$Z5=$Y5;
$c6=q'ni:/behavior/slice';
$d6=q'ni:/behavior/slice_init';
$e6={};
$f6=[];
$g6=q'my $class = shift;
my @isa;
push @isa, shift while ref $_[0];
my %args = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  isa        => \\@isa,
  methods    => \\%args};';
$h6=bless({$u,$g6},$l);
$i6=q'instantiation';
$j6=q'my $self = shift;
(@{$$self{isa}},
 %{$$self{methods}},
 %$self{qw/applied_to ctor dtor/});';
$k6=bless({$u,$j6},$l);
$l6={$L,$h6,$i6,$k6};
$m6=q'/behavior/slice_init';
$n6=bless({$d,$e6,$o,$c,$p,$c,$q,$f6,$s,$l6,$g,$m6},$i);
$o6=$n6;
$p6=q'ni:/behavior/slice_named_init';
$q6={};
$r6=[];
$s6={$L,$E2};
$t6=q'/behavior/slice_named_init';
$u6=bless({$d,$q6,$o,$c,$p,$c,$q,$r6,$s,$s6,$g,$t6},$i);
$v6=$u6;
$w6=q'ni:/behavior/tag';
$x6={};
$y6=[];
$z6=q'local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;';
$A6=bless({$u,$z6},$l);
$B6={$p1,$A6};
$C6=q'/behavior/tag';
$D6=bless({$d,$x6,$o,$c,$p,$c,$q,$y6,$s,$B6,$g,$C6},$i);
$E6=$D6;
$F6=q'ni:/class/behavior';
$G6=q'ni:/class/class';
$H6=q'ni:/class/fn';
$I6=q'ni:/class/image';
$J6={$J3,1};
$K6=[$K3,$l5];
$L6=bless({$d,$J6,$g,$J3,$h,$K6},$e);
$M6=q'ni:/class/ni';
$N6={$f3,1};
$O6={};
$P6=[];
$Q6='defclass';
$R6=q'shift; ni(\'ni:/class/class\')->new("/class/$_[0]", @_[1..$#_])';
$S6=bless({$u,$R6},$l);
$T6='defslice';
$U6=q'shift; ni(\'ni:/class/slice\')->new("/behavior/$_[0]", @_[1..$#_])';
$V6=bless({$u,$U6},$l);
$W6={$Q6,$S6,$T6,$V6};
$X6=bless({$d,$O6,$o,$c,$p,$c,$q,$P6,$s,$W6,$g,$c},$i);
$Y6=$X6;
$Z6=[$Y6,$g3];
$c7=bless({$d,$N6,$g,$f3,$h,$Z6},$e);
$d7=q'ni:/class/slice';
$e7={$c,$g3,$h3,$n3,$o3,$x3,$y3,$t2,$z3,$R,$A3,$K3,$L3,$l5,$m5,$E,$n5,$t5,$u5,$A5,$B5,$H5,$I5,$S5,$T5,$Z5,$c6,$t1,$d6,$o6,$p6,$v6,$w6,$E6,$F6,$Z1,$G6,$v2,$H6,$e1,$I6,$L6,$M6,$c7,$d7,$Q2};
*{$m1}=\&$i1;
*{$l1}=\&$g1;
*{$k1}=\&$i1;
*{$j1}=\&$g1;
$C->apply_unsafe($D);
$C->apply_unsafe($e);
$C->apply_unsafe($l);
$C->apply_unsafe($i);
$Q->apply_unsafe($l);
$Z->apply_unsafe($l);
$s1->apply_unsafe($D);
$s1->apply_unsafe($i);
$s2->apply_unsafe($e);
$N2->apply_unsafe($i);
$e3->apply_unsafe($f3);
$m3->apply_unsafe($e);
$w3->apply_unsafe($e);
$I3->apply_unsafe($J3);
$k5->apply_unsafe($J3);
$s5->apply_unsafe($e);
$s5->apply_unsafe($i);
$z5->apply_unsafe($e);
$z5->apply_unsafe($i);
$G5->apply_unsafe($e);
$G5->apply_unsafe($i);
$Y5->apply_unsafe($e);
$Y5->apply_unsafe($i);
$n6->apply_unsafe($D);
$u6->apply_unsafe($i);
$X6->apply_unsafe($f3);
%ni::named=%$e7;
ni->run(@ARGV);
__DATA__
