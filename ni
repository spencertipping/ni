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
use Digest::MD5;
no warnings qw/redefine void/;
no strict 'refs';
use Errno;
use Fcntl;
use POSIX;
use Scalar::Util;
chomp $ni::boot;
$ni::self = bless {named => {}}, 'ni::/lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->resolve($_[0]) : $ni::self}
sub ni::eval {eval shift}
*{'ni::/lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'ni::/lib/fn::OVERLOAD'} = {};
*{'ni::/lib/fn::(bool'} = sub {1};
*{'ni::/lib/fn::()'}    = sub {};
*{'ni::/lib/fn::(&{}'} = sub {
  my $self = shift;
  $$self{fn} ||= $self->compile;
};
*{'ni::/lib/fn::compile'} = sub {
  my $self = shift;
  my ($en) = ni::eval('__FILE__') =~ /eval (\d+)/;
  $$self{eval_number} = ++$en;
  Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:/lib/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
sub fn($);
_
$c=q#named#;$d=q#ni.doc:/class#;$e=q#doc#;$f=q#synopsis#;$g=q#
ni('ni:/object')->child('/message')
  ->add('/behaviorname.b')          \# add existing behavior
  ->def('/message_init.b',          \# define new slice behavior
    instantiate => fn q{            \# called from ->new()
      my ($class, $message) = @_;
      +{message => $message};       \# return object to be blessed
    })
  ->def('/behaviorname.b',          \# define another behavior
    method1 => fn q{
      my $self = shift;
      print "message for you sir! '" . $$self{message} . "'\\n";
    });
ni('ni:/child')->new('hello world!')->method1;#;$h=[$f,$g];$i=q#description#;$j=q#ni:class is at the core of ni's object-oriented system, along with core
classes like ni:object and ni:metaclass. There are two layers of
abstraction involved here: Perl packages are modified by behaviors, and
classes encode the higher-level declarative features you'd expect from a
language like Ruby or Smalltalk. This documentation covers both layers.#;$k=[$i,$j];$l=q#behaviors#;$m=q#ni's objects are blessed Perl references, and behaviors are objects
that modify Perl packages in specific ways. The simplest is
ni:/lib/slice, which represents a set of methods you can add to a
package.#;$n=q#assertions#;$o=[];$p=q#error#;$q=undef;$r=q#outcome#;$s=q#test#;$t=q#annotations#;$u=[];$v=q#closure#;$w=q#code#;$x=q#my $fn = fn q{"hi"};
my $slice = ni('ni:/lib/slice')->new('myslice', f => $fn);
$slice->apply('foo');
now foo->f == 'hi';#;$y=q#proto#;$z=q##;$A=q#ni::/lib/fn#;$B=bless({$t,$u,$v,$q,$w,$x,$y,$z},$A);$C=q#ni::/lib/test_case#;$D=bless({$n,$o,$p,$q,$r,$q,$s,$B},$C);$E=q#todo#;$F=q#document this further#;$G=[$F];$H=q#ni::/lib/todo#;$I=bless({$E,$G},$H);$J=[$l,$m,$D,$I];$K=q#classes#;$L=q#ni implements a Smalltalk 80-style metaclass system with a couple of
differences. First, ni's classes are slice unions and as such don't
support colliding methods; and second, they support multiple inheritance.
These two points are related: method overriding isn't in the picture,
which makes multiple inheritance straightforward to implement.#;$M=[$F];$N=bless({$E,$M},$H);$O=[$K,$L,$N];$P=[$h,$k,$J,$O];$Q=q#name#;$R=q#/class#;$S=q#ni::/lib/doc#;$T=bless({$e,$P,$Q,$R},$S);$U=q#my $s = shift; ni->def($s->name, $s)#;$V=bless({$w,$U,$y,$z},$A);$W=q#ni.doc:/fabric#;$X=q#Abstractions to bridge the gaps between separate machines and processes.
This module is designed to make it appear as though all resources are
local, or at least can be referred to locally -- even when they belong to
an external process (e.g. a Hadoop mapper) or another machine (e.g. a
file over SSH). If we can bidirectionally communicate with a remote ni
instance, then we can see its resources.#;$Y=q#The fabric layer consists of a couple of things. First, we've got RMI
support code that proxies any method call and return value(s) over a
full-duplex data channel. Second, we have an async event loop that
handles multiplexed IO using a single thread.#;$Z=q#There are a couple of RMI-related invariants. First, objects never move;
they are always owned by a particular ni instance, and are assumed to
have unquantifiable dependencies that would be broken if we moved them
using serialization. Second, all argument/return value data is immutable
and stateless; to the extent that it depends on objects, these
dependencies are indirected through a name table.#;$c1=[$i,$X,$Y,$Z];$d1=[$c1];$e1=q#/fabric#;$f1=bless({$e,$d1,$Q,$e1},$S);$g1=q#ni.doc:/fabric/native#;$h1=q#
my $n = ni('ni:/fabric/native')
  ->new
  ->lib(q[C code...])
  ->def('identity', ['long'], ['long'], q{
    /* todo this is awful */
  })
  ->def('f', ...)
  ->start;
print "f(10) = " . $n->f(10) . "\\n";#;$i1=[$f,$h1];$j1=q#Compiles a binary that interfaces with Perl to support calls to the
specified methods. Communication happens over a socket pair, and is done
using packed values.#;$k1=[$i,$j1];$l1=[$i1,$k1];$m1=q#/fabric/native#;$n1=bless({$e,$l1,$Q,$m1},$S);$o1=q#ni.doc:/io#;$p1=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$q1=[$i,$p1];$r1=[$q1];$s1=q#/io#;$t1=bless({$e,$r1,$Q,$s1},$S);$u1=q#ni.doc:/io/buffer#;$v1=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$w1=[$f,$v1];$x1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$y1=[];$z1=[];$A1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$B1=bless({$t,$z1,$v,$q,$w,$A1,$y,$z},$A);$C1=bless({$n,$y1,$p,$q,$r,$q,$s,$B1},$C);$D1=[$i,$x1,$C1];$E1=[$w1,$D1];$F1=q#/io/buffer#;$G1=bless({$e,$E1,$Q,$F1},$S);$H1=q#ni.doc:/io/cat#;$I1=q#
my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
my $combined = $io1 + $io2 + $io3;
$combined->into_sync($destination_io);#;$J1=[$f,$I1];$K1=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$L1=[];$M1=[];$N1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$O1=bless({$t,$M1,$v,$q,$w,$N1,$y,$z},$A);$P1=bless({$n,$L1,$p,$q,$r,$q,$s,$O1},$C);$Q1=[$i,$K1,$P1];$R1=[$J1,$Q1];$S1=q#/io/cat#;$T1=bless({$e,$R1,$Q,$S1},$S);$U1=q#ni.doc:/io/exec#;$V1=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$W1=[$f,$V1];$X1=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$Y1=[];$Z1=[];$c2=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$d2=bless({$t,$Z1,$v,$q,$w,$c2,$y,$z},$A);$e2=bless({$n,$Y1,$p,$q,$r,$q,$s,$d2},$C);$f2=[$i,$X1,$e2];$g2=[$W1,$f2];$h2=q#/io/exec#;$i2=bless({$e,$g2,$Q,$h2},$S);$j2=q#ni.doc:/io/fd#;$k2=q#
open my $fh, ...;
my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
my $fd = ni('ni:/io/fd')->new(0);   \# from number
my $fd = ni('fd:0');                \# same thing
$fd->nonblock(1)->read($_, 100);
$fd->be(10);                        \# move FD number#;$l2=[$f,$k2];$m2=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$n2=[];$o2=[];$p2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$q2=bless({$t,$o2,$v,$q,$w,$p2,$y,$z},$A);$r2=bless({$n,$n2,$p,$q,$r,$q,$s,$q2},$C);$s2=[$i,$m2,$r2];$t2=[$l2,$s2];$u2=q#/io/fd#;$v2=bless({$e,$t2,$Q,$u2},$S);$w2=q#ni.doc:/io/file#;$x2=q#
my $f = ni('ni:/io/file')->new('/etc/passwd');
my $f = ni('file:/etc/passwd');     \# same as above
$f->into_sync(ni('fd:1'));          \# cat to stdout#;$y2=[$f,$x2];$z2=q#warning#;$A2=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$B2=[$z2,$A2];$C2=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$D2=[];$E2=[];$F2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$G2=bless({$t,$E2,$v,$q,$w,$F2,$y,$z},$A);$H2=bless({$n,$D2,$p,$q,$r,$q,$s,$G2},$C);$I2=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$J2=[];$K2=[];$L2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$M2=bless({$t,$K2,$v,$q,$w,$L2,$y,$z},$A);$N2=bless({$n,$J2,$p,$q,$r,$q,$s,$M2},$C);$O2=[$i,$C2,$H2,$I2,$N2];$P2=[$y2,$B2,$O2];$Q2=q#/io/file#;$R2=bless({$e,$P2,$Q,$Q2},$S);$S2=q#ni.doc:/io/file_update_fd#;$T2=q#A write fd that performs a file rename upon closing.#;$U2=[$i,$T2];$V2=[$U2];$W2=q#/io/file_update_fd#;$X2=bless({$e,$V2,$Q,$W2},$S);$Y2=q#ni.doc:/io/object#;$Z2=q#TODO#;$c3=q#referent#;$d3=q#applied_to#;$e3=q#ni::/io/buffer#;$f3=q#ni::/io/cat#;$g3=q#ni::/io/exec#;$h3=q#ni::/io/fd#;$i3=q#ni::/io/file#;$j3=q#ni::/io/file_update_fd#;$k3=q#ni::/io/null#;$l3=q#ni::/io/object#;$m3=q#ni::/io/pid#;$n3=q#ni::/io/str#;$o3={$e3,1,$f3,1,$g3,1,$h3,1,$i3,1,$j3,1,$k3,1,$l3,1,$m3,1,$n3,1};$p3=q#/io/object#;$q3=q#slices#;$r3=q#ni::/class#;$s3=q#ni::/class.c#;$t3=q#ni::/fabric/native#;$u3=q#ni::/fabric/native.c#;$v3=q#ni::/io/buffer.c#;$w3=q#ni::/io/cat.c#;$x3=q#ni::/io/exec.c#;$y3=q#ni::/io/fd.c#;$z3=q#ni::/io/file.c#;$A3=q#ni::/io/file_update_fd.c#;$B3=q#ni::/io/null.c#;$C3=q#ni::/io/object.c#;$D3=q#ni::/io/pid.c#;$E3=q#ni::/io/str.c#;$F3=q#ni::/io/transfer#;$G3=q#ni::/io/transfer.c#;$H3=q#ni::/io/transfer_async#;$I3=q#ni::/io/transfer_async.c#;$J3=q#ni::/io/transfer_sync#;$K3=q#ni::/io/transfer_sync.c#;$L3=q#ni::/lib/behavior#;$M3=q#ni::/lib/behavior.c#;$N3=q#ni::/lib/branch#;$O3=q#ni::/lib/branch.c#;$P3=q#ni::/lib/dataslice#;$Q3=q#ni::/lib/dataslice.c#;$R3=q#ni::/lib/doc.c#;$S3=q#ni::/lib/fn.c#;$T3=q#ni::/lib/future#;$U3=q#ni::/lib/future.c#;$V3=q#ni::/lib/image#;$W3=q#ni::/lib/image.c#;$X3=q#ni::/lib/ni#;$Y3=q#ni::/lib/ni.c#;$Z3=q#ni::/lib/object_metadata#;$c4=q#ni::/lib/object_metadata.c#;$d4=q#ni::/lib/quote_simple#;$e4=q#ni::/lib/quote_simple.c#;$f4=q#ni::/lib/slice#;$g4=q#ni::/lib/slice.c#;$h4=q#ni::/lib/tag#;$i4=q#ni::/lib/tag.c#;$j4=q#ni::/lib/test_assert_eq#;$k4=q#ni::/lib/test_assert_eq.c#;$l4=q#ni::/lib/test_assertion#;$m4=q#ni::/lib/test_assertion.c#;$n4=q#ni::/lib/test_case.c#;$o4=q#ni::/lib/test_value#;$p4=q#ni::/lib/test_value.c#;$q4=q#ni::/lib/todo.c#;$r4=q#ni::/metaclass#;$s4=q#ni::/metaclass.c#;$t4=q#ni::/module#;$u4=q#ni::/module.c#;$v4=q#ni::/object#;$w4=q#ni::/object.c#;$x4=q#ni::/semantic/dimension#;$y4=q#ni::/semantic/dimension.c#;$z4=q#ni::/semantic/task#;$A4=q#ni::/semantic/task.c#;$B4={$r3,1,$s3,1,$t3,1,$u3,1,$e3,1,$v3,1,$f3,1,$w3,1,$g3,1,$x3,1,$h3,1,$y3,1,$i3,1,$z3,1,$j3,1,$A3,1,$k3,1,$B3,1,$l3,1,$C3,1,$m3,1,$D3,1,$n3,1,$E3,1,$F3,1,$G3,1,$H3,1,$I3,1,$J3,1,$K3,1,$L3,1,$M3,1,$N3,1,$O3,1,$P3,1,$Q3,1,$S,1,$R3,1,$A,1,$S3,1,$T3,1,$U3,1,$V3,1,$W3,1,$X3,1,$Y3,1,$Z3,1,$c4,1,$d4,1,$e4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$l4,1,$m4,1,$C,1,$n4,1,$o4,1,$p4,1,$H,1,$q4,1,$r4,1,$s4,1,$t4,1,$u4,1,$v4,1,$w4,1,$x4,1,$y4,1,$z4,1,$A4,1};$C4=q#/object#;$D4={};$E4=q#ctor#;$F4=q#dtor#;$G4=q#methods#;$H4=q#DESTROY#;$I4=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$J4=bless({$w,$I4,$y,$z},$A);$K4=q#class#;$L4=q#(my $r = ref shift) =~ s/^ni::/ni:/; ni$r#;$M4=bless({$w,$L4,$y,$z},$A);$N4={$H4,$J4,$K4,$M4};$O4=q#/lib/instance.b#;$P4=bless({$d3,$D4,$E4,$q,$F4,$q,$G4,$N4,$Q,$O4},$f4);$Q4=[$P4];$R4=bless({$d3,$B4,$Q,$C4,$q3,$Q4},$w4);$S4=q#my $s = shift; $s->apply($s->package)#;$T4=bless({$w,$S4,$y,$z},$A);$U4={};$V4=q#(bool#;$W4=[];$X4=bless({$t,$W4,$v,$q,$w,1,$y,$z},$A);$Y4={$V4,$X4};$Z4=q#/io/object_ops.b#;$c5=bless({$d3,$U4,$E4,$q,$F4,$q,$G4,$Y4,$Q,$Z4},$f4);$d5={};$e5=q#die#;$f5=[];$g5=q#shift; die join " ", @_#;$h5=bless({$t,$f5,$v,$q,$w,$g5,$y,$z},$A);$i5=q#io_check#;$j5=[];$k5=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$l5=bless({$t,$j5,$v,$q,$w,$k5,$y,$z},$A);$m5=q#io_check_defined#;$n5=[];$o5=q#shift->io_check(sub {defined shift}, @_)#;$p5=bless({$t,$n5,$v,$q,$w,$o5,$y,$z},$A);$q5=q#io_check_true#;$r5=[];$s5=q#shift->io_check(sub {shift}, @_)#;$t5=bless({$t,$r5,$v,$q,$w,$s5,$y,$z},$A);$u5={$e5,$h5,$i5,$l5,$m5,$p5,$q5,$t5};$v5=q#/io/object_checks.b#;$w5=bless({$d3,$d5,$E4,$q,$F4,$q,$G4,$u5,$Q,$v5},$f4);$x5={};$y5=q#(+#;$z5=[];$A5=q#ni('ni:/io/cat')->new(@_[0, 1])#;$B5=bless({$t,$z5,$v,$q,$w,$A5,$y,$z},$A);$C5={$y5,$B5};$D5=q#/io/object_constructors.b#;$E5=bless({$d3,$x5,$E4,$q,$F4,$q,$G4,$C5,$Q,$D5},$f4);$F5={};$G5=q#read_all#;$H5=[];$I5=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$J5=bless({$t,$H5,$v,$q,$w,$I5,$y,$z},$A);$K5=q#write_all#;$L5=[];$M5=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$N5=bless({$t,$L5,$v,$q,$w,$M5,$y,$z},$A);$O5={$G5,$J5,$K5,$N5};$P5=q#/io/object_memory.b#;$Q5=bless({$d3,$F5,$E4,$q,$F4,$q,$G4,$O5,$Q,$P5},$f4);$R5={};$S5=q#connect_sync#;$T5=[];$U5=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$V5=bless({$t,$T5,$v,$q,$w,$U5,$y,$z},$A);$W5=q#into_sync#;$X5=[];$Y5=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$Z5=bless({$t,$X5,$v,$q,$w,$Y5,$y,$z},$A);$c6={$S5,$V5,$W5,$Z5};$d6=q#/io/object_transfer_sync.b#;$e6=bless({$d3,$R5,$E4,$q,$F4,$q,$G4,$c6,$Q,$d6},$f4);$f6={};$g6=q#connect_async#;$h6=[];$i6=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$j6=bless({$t,$h6,$v,$q,$w,$i6,$y,$z},$A);$k6=q#into_async#;$l6=[];$m6=q#ni('ni:/io/transfer_async')->new(@_)->run#;$n6=bless({$t,$l6,$v,$q,$w,$m6,$y,$z},$A);$o6={$g6,$j6,$k6,$n6};$p6=q#/io/object_transfer_async.b#;$q6=bless({$d3,$f6,$E4,$q,$F4,$q,$G4,$o6,$Q,$p6},$f4);$r6=[$R4,$c5,$w5,$E5,$Q5,$e6,$q6,$q6,$e6,$q6,$e6];$s6=bless({$d3,$o3,$Q,$p3,$q3,$r6},$C3);$t6=q#migrate die() into /lib/ as a base behavior#;$u6=[$t6];$v6=bless({$c3,$s6,$E,$u6},$H);$w6=[$Z2,$v6];$x6=[$w6];$y6=bless({$e,$x6,$Q,$p3},$S);$z6=q#ni.doc:/io/pid#;$A6=q#eg#;$B6=[];$C6={$m3,1};$D6=q#/io/pid#;$E6={};$F6=q#pid#;$G6=[];$H6=q#shift->{'pid'}#;$I6=bless({$t,$G6,$v,$q,$w,$H6,$y,$z},$A);$J6=q#status#;$K6=[];$L6=q#shift->{'status'}#;$M6=bless({$t,$K6,$v,$q,$w,$L6,$y,$z},$A);$N6={$F6,$I6,$J6,$M6};$O6=q#/io/pid_readers.b#;$P6=bless({$d3,$E6,$E4,$q,$F4,$q,$G4,$N6,$Q,$O6},$f4);$Q6={};$R6=[];$S6=q#shift->await#;$T6=bless({$t,$R6,$v,$q,$w,$S6,$y,$z},$A);$U6=q#instantiate#;$V6=[];$W6=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$X6=bless({$t,$V6,$v,$q,$w,$W6,$y,$z},$A);$Y6={$U6,$X6};$Z6=q#/io/pid_init.b#;$c7=bless({$d3,$Q6,$E4,$q,$F4,$T6,$G4,$Y6,$Q,$Z6},$f4);$d7={};$e7=q#await#;$f7=[];$g7=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$h7=bless({$t,$f7,$v,$q,$w,$g7,$y,$z},$A);$i7=q#running#;$j7=[];$k7=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$l7=bless({$t,$j7,$v,$q,$w,$k7,$y,$z},$A);$m7={$e7,$h7,$i7,$l7};$n7=q#/io/pid_wait.b#;$o7=bless({$d3,$d7,$E4,$q,$F4,$q,$G4,$m7,$Q,$n7},$f4);$p7={};$q7=q#read#;$r7=[];$s7=q#shift->stdout->read(@_)#;$t7=bless({$t,$r7,$v,$q,$w,$s7,$y,$z},$A);$u7=q#write#;$v7=[];$w7=q#shift->stdin->write(@_)#;$x7=bless({$t,$v7,$v,$q,$w,$w7,$y,$z},$A);$y7={$q7,$t7,$u7,$x7};$z7=q#/io/pid_io.b#;$A7=bless({$d3,$p7,$E4,$q,$F4,$q,$G4,$y7,$Q,$z7},$f4);$B7={};$C7=q#fd#;$D7=[];$E7=q#$_[0]->{external_fds}{$_[1]}#;$F7=bless({$t,$D7,$v,$q,$w,$E7,$y,$z},$A);$G7=q#stderr#;$H7=[];$I7=q#shift->fd(2)#;$J7=bless({$t,$H7,$v,$q,$w,$I7,$y,$z},$A);$K7=q#stdin#;$L7=[];$M7=q#shift->fd(0)#;$N7=bless({$t,$L7,$v,$q,$w,$M7,$y,$z},$A);$O7=q#stdout#;$P7=[];$Q7=q#shift->fd(1)#;$R7=bless({$t,$P7,$v,$q,$w,$Q7,$y,$z},$A);$S7={$C7,$F7,$G7,$J7,$K7,$N7,$O7,$R7};$T7=q#/io/pid_accessors.b#;$U7=bless({$d3,$B7,$E4,$q,$F4,$q,$G4,$S7,$Q,$T7},$f4);$V7=[$s6,$P6,$c7,$o7,$A7,$U7];$W7=bless({$d3,$C6,$Q,$D6,$q3,$V7},$D3);$X7=[];$Y7=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$Z7=bless({$t,$X7,$v,$q,$w,$Y7,$y,$z},$A);$c8=bless({$n,$B6,$p,$q,$r,$q,$c3,$W7,$s,$Z7},$C);$d8=[$A6,$c8];$e8=[];$f8=[];$g8=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$h8=bless({$t,$f8,$v,$q,$w,$g8,$y,$z},$A);$i8=bless({$n,$e8,$p,$q,$r,$q,$c3,$W7,$s,$h8},$C);$j8=[$A6,$i8];$k8=[];$l8=[];$m8=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$n8=bless({$t,$l8,$v,$q,$w,$m8,$y,$z},$A);$o8=bless({$n,$k8,$p,$q,$r,$q,$c3,$W7,$s,$n8},$C);$p8=[$A6,$o8];$q8=[$d8,$j8,$p8];$r8=bless({$e,$q8,$Q,$D6},$S);$s8=q#ni.doc:/lib#;$t8=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$u8=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$v8=[$i,$t8,$u8];$w8=[$v8];$x8=q#/lib#;$y8=bless({$e,$w8,$Q,$x8},$S);$z8=q#ni.doc:/lib/dataslice#;$A8={$P3,1};$B8=q#/lib/dataslice#;$C8={$r3,1,$s3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$G3,1,$I3,1,$K3,1,$L3,1,$M3,1,$N3,1,$O3,1,$P3,1,$Q3,1,$R3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$e4,1,$f4,1,$g4,1,$h4,1,$i4,1,$k4,1,$m4,1,$n4,1,$p4,1,$q4,1,$r4,1,$s4,1,$t4,1,$u4,1,$w4,1,$x4,1,$y4,1,$A4,1};$D8=q#/lib/behavior#;$E8={};$F8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$G8=bless({$w,$F8,$y,$z},$A);$H8={$e,$G8};$I8=q#/lib/documentable.b#;$J8=bless({$d3,$E8,$E4,$q,$F4,$q,$G4,$H8,$Q,$I8},$f4);$K8=[$R4,$J8];$L8=bless({$d3,$C8,$Q,$D8,$q3,$K8},$M3);$M8={};$N8=q#$_[0]->namespace . ":" . $_[0]->{name}#;$O8=bless({$w,$N8,$y,$z},$A);$P8={$Q,$O8};$Q8=q#/lib/named.b#;$R8=bless({$d3,$M8,$E4,$V,$F4,$q,$G4,$P8,$Q,$Q8},$f4);$S8={};$T8=q#apply#;$U8=q#shift->apply_(@_)#;$V8=bless({$w,$U8,$y,$z},$A);$W8=q#apply_#;$X8=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Y8=bless({$w,$X8,$y,$z},$A);$Z8={$T8,$V8,$W8,$Y8};$c9=q#/lib/dataslice.b#;$d9=bless({$d3,$S8,$E4,$q,$F4,$q,$G4,$Z8,$Q,$c9},$f4);$e9={};$f9=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$g9=bless({$w,$f9,$y,$z},$A);$h9={$U6,$g9};$i9=q#/lib/dataslice_init.b#;$j9=bless({$d3,$e9,$E4,$q,$F4,$q,$G4,$h9,$Q,$i9},$f4);$k9={};$l9=q#namespace#;$m9=q#'ni'#;$n9=bless({$w,$m9,$y,$z},$A);$o9={$l9,$n9};$p9=q#/lib/named_in_ni.b#;$q9=bless({$d3,$k9,$E4,$q,$F4,$q,$G4,$o9,$Q,$p9},$f4);$r9={};$s9=q#package#;$t9=q#my $name = shift->{name};
$name =~ /^\\// ? "ni::$name" : $name;#;$u9=bless({$w,$t9,$y,$z},$A);$v9={$s9,$u9};$w9=q#/lib/namespaced.b#;$x9=bless({$d3,$r9,$E4,$q,$F4,$q,$G4,$v9,$Q,$w9},$f4);$y9={};$z9=q#resolve#;$A9=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$B9=bless({$w,$A9,$y,$z},$A);$C9={$z9,$B9};$D9=q#/lib/resolver.b#;$E9=bless({$d3,$y9,$E4,$q,$F4,$q,$G4,$C9,$Q,$D9},$f4);$F9={};$G9=q#serialize#;$H9=[];$I9=q#local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq 'ni:/lib/slice.b' || $name eq 'ni:/lib/fn_init.b') {
  my %methods;
  my @ks = sort keys %{$$self{methods}};
  @methods{@ks} = map $quote->quote($_), @{$$self{methods}}{@ks};
  for my $p (sort keys %{$$self{applied_to}}) {
    $quote->boot_side_effect(
      '*' . $quote->quote("$p\\::$_") . "=\\\\\\&$methods{$_};")
      for @ks;
  }
}

my $g = $quote->allocate_gensym($self,
  $quote->quote_blessed({%$self, applied_to => {}}, ref $self));
$quote->side_effect("$g\\->apply_(" . $quote->quote($_) . ");")
  for sort keys %{$$self{applied_to}};
$g;#;$J9=bless({$t,$H9,$v,$q,$w,$I9,$y,$z},$A);$K9={$G9,$J9};$L9=q#/lib/slice_serialize.b#;$M9=bless({$d3,$F9,$E4,$q,$F4,$q,$G4,$K9,$Q,$L9},$f4);$N9=[$L8,$R8,$d9,$j9,$q9,$x9,$E9,$M9];$O9=bless({$d3,$A8,$Q,$B8,$q3,$N9},$Q3);$P9=q#Fix serialization for dataslices#;$Q9=[$P9];$R9=bless({$c3,$O9,$E,$Q9},$H);$S9=[$Z2,$R9];$T9=[$S9];$U9=bless({$e,$T9,$Q,$B8},$S);$V9=q#ni.doc:/lib/doc#;$W9=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$X9=[$f,$W9];$Y9={$S,1};$Z9=q#/lib/doc#;$ca={};$da=q#shift; +{name => shift, doc => []}#;$ea=bless({$w,$da,$y,$z},$A);$fa={$U6,$ea};$ga=q#/lib/doc_init.b#;$ha=bless({$d3,$ca,$E4,$q,$F4,$q,$G4,$fa,$Q,$ga},$f4);$ia={};$ja=q#'ni.doc'#;$ka=bless({$w,$ja,$y,$z},$A);$la={$l9,$ka};$ma=q#/lib/doc_namespace.b#;$na=bless({$d3,$ia,$E4,$q,$F4,$q,$G4,$la,$Q,$ma},$f4);$oa={};$pa=q#(@{}#;$qa=q#[map @$_, @{shift->{doc}}]#;$ra=bless({$w,$qa,$y,$z},$A);$sa=q#AUTOLOAD#;$ta=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$ua=bless({$w,$ta,$y,$z},$A);$va={$pa,$ra,$sa,$ua};$wa=q#/lib/doc_define.b#;$xa=bless({$d3,$oa,$E4,$q,$F4,$q,$G4,$va,$Q,$wa},$f4);$ya={};$za=q#end#;$Aa=q#shift->referent#;$Ba=bless({$w,$Aa,$y,$z},$A);$Ca=q#ni 'ni:' . shift->{name}#;$Da=bless({$w,$Ca,$y,$z},$A);$Ea={$za,$Ba,$c3,$Da};$Fa=q#/lib/doc_end.b#;$Ga=bless({$d3,$ya,$E4,$q,$F4,$q,$G4,$Ea,$Q,$Fa},$f4);$Ha={};$Ia=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Ja=bless({$w,$Ia,$y,$z},$A);$Ka={$Z2,$Ja};$La=q#/lib/doc_TODO.b#;$Ma=bless({$d3,$Ha,$E4,$q,$F4,$q,$G4,$Ka,$Q,$La},$f4);$Na={};$Oa=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Pa=bless({$w,$Oa,$y,$z},$A);$Qa={$A6,$Pa};$Ra=q#/lib/doc_eg.b#;$Sa=bless({$d3,$Na,$E4,$q,$F4,$q,$G4,$Qa,$Q,$Ra},$f4);$Ta={};$Ua=q#tests#;$Va=q#my $self = shift;
my $test_case_class = ni('ni:/lib/test_case')->package;
map $_->referent($self->referent), grep ref($_) eq $test_case_class,
    map @$_, @{$$self{doc}};#;$Wa=bless({$w,$Va,$y,$z},$A);$Xa=q#todos#;$Ya=q#my $self = shift;
my $todo_class = ni('ni:/lib/todo')->package;
map $_->referent($self->referent), grep ref($_) eq $todo_class,
    map @$_, @{$$self{doc}};#;$Za=bless({$w,$Ya,$y,$z},$A);$cb={$Ua,$Wa,$Xa,$Za};$db=q#/lib/doc_process.b#;$eb=bless({$d3,$Ta,$E4,$q,$F4,$q,$G4,$cb,$Q,$db},$f4);$fb=[$R4,$R8,$ha,$na,$xa,$Ga,$Ma,$Sa,$eb];$gb=bless({$d3,$Y9,$Q,$Z9,$q3,$fb},$R3);$hb=q#Use a better preprocessor; ni::outdent fixes indentation, but we also
want to unwrap lines unless they're split at paragraphs (like
markdown).#;$ib=[$hb];$jb=bless({$c3,$gb,$E,$ib},$H);$kb=[$Z2,$jb];$lb=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$mb=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$nb=[];$ob=[];$pb=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$qb=bless({$t,$ob,$v,$q,$w,$pb,$y,$z},$A);$rb=bless({$n,$nb,$p,$q,$r,$q,$s,$qb},$C);$sb=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$tb=[];$ub=[];$vb=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$wb=bless({$t,$ub,$v,$q,$w,$vb,$y,$z},$A);$xb=bless({$n,$tb,$p,$q,$r,$q,$s,$wb},$C);$yb=[$i,$lb,$mb,$rb,$sb,$xb];$zb=[$X9,$kb,$yb];$Ab=bless({$e,$zb,$Q,$Z9},$S);$Bb=q#ni.doc:/lib/fn#;$Cb=q#Give functions a way to name themselves so we can do \#line
reporting#;$Db=[$Cb];$Eb=bless({$E,$Db},$H);$Fb=[$i,$Eb];$Gb=[$Fb];$Hb=q#/lib/fn#;$Ib=bless({$e,$Gb,$Q,$Hb},$S);$Jb=q#ni.doc:/lib/future#;$Kb=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$Lb=[];$Mb=[];$Nb=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$Ob=bless({$t,$Mb,$v,$q,$w,$Nb,$y,$z},$A);$Pb=bless({$n,$Lb,$p,$q,$r,$q,$s,$Ob},$C);$Qb=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$Rb=[];$Sb=[];$Tb=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$Ub=bless({$t,$Sb,$v,$q,$w,$Tb,$y,$z},$A);$Vb=bless({$n,$Rb,$p,$q,$r,$q,$s,$Ub},$C);$Wb=[$i,$Kb,$Pb,$Qb,$Vb];$Xb=[$Wb];$Yb=q#/lib/future#;$Zb=bless({$e,$Xb,$Q,$Yb},$S);$cc=q#ni.doc:/lib/image#;$dc=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$ec=[$f,$dc];$fc=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$gc=[$i,$fc];$hc={$V3,1};$ic=q#/lib/image#;$jc={};$kc=[];$lc=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$mc=bless({$t,$kc,$v,$q,$w,$lc,$y,$z},$A);$nc={$U6,$mc};$oc=q#/lib/image_init.b#;$pc=bless({$d3,$jc,$E4,$q,$F4,$q,$G4,$nc,$Q,$oc},$f4);$qc={};$rc=q#boot_side_effect#;$sc=[];$tc=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$uc=bless({$t,$sc,$v,$q,$w,$tc,$y,$z},$A);$vc=q#finalizer#;$wc=[];$xc=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$yc=bless({$t,$wc,$v,$q,$w,$xc,$y,$z},$A);$zc=q#io#;$Ac=[];$Bc=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Cc=bless({$t,$Ac,$v,$q,$w,$Bc,$y,$z},$A);$Dc=q#reconstruction#;$Ec=[];$Fc=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Gc=bless({$t,$Ec,$v,$q,$w,$Fc,$y,$z},$A);$Hc=q#side_effect#;$Ic=[];$Jc=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Kc=bless({$t,$Ic,$v,$q,$w,$Jc,$y,$z},$A);$Lc={$rc,$uc,$vc,$yc,$zc,$Cc,$Dc,$Gc,$Hc,$Kc};$Mc=q#/lib/image_quoting.b#;$Nc=bless({$d3,$qc,$E4,$q,$F4,$q,$G4,$Lc,$Q,$Mc},$f4);$Oc={};$Pc=q#quote_code#;$Qc=[];$Rc=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Sc=bless({$t,$Qc,$v,$q,$w,$Rc,$y,$z},$A);$Tc={$Pc,$Sc};$Uc=q#/lib/quote_code_fail.b#;$Vc=bless({$d3,$Oc,$E4,$q,$F4,$q,$G4,$Tc,$Q,$Uc},$f4);$Wc={};$Xc=q#quote_array#;$Yc=[];$Zc=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$cd=bless({$t,$Yc,$v,$q,$w,$Zc,$y,$z},$A);$dd=q#quote_hash#;$ed=[];$fd=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$gd=bless({$t,$ed,$v,$q,$w,$fd,$y,$z},$A);$hd=q#quote_scalar#;$id=[];$jd=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$kd=bless({$t,$id,$v,$q,$w,$jd,$y,$z},$A);$ld=q#quote_scalar_ref#;$md=[];$nd=q#'\\\\' . shift->quote(${$_[0]})#;$od=bless({$t,$md,$v,$q,$w,$nd,$y,$z},$A);$pd=q#quote_value#;$qd=[];$rd=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$sd=bless({$t,$qd,$v,$q,$w,$rd,$y,$z},$A);$td={$Xc,$cd,$dd,$gd,$hd,$kd,$ld,$od,$pd,$sd};$ud=q#/lib/quote_values.b#;$vd=bless({$d3,$Wc,$E4,$q,$F4,$q,$G4,$td,$Q,$ud},$f4);$wd={};$xd=q#quote_blessed#;$yd=[];$zd=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Ad=bless({$t,$yd,$v,$q,$w,$zd,$y,$z},$A);$Bd=q#quote_class#;$Cd=[];$Dd=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Ed=bless({$t,$Cd,$v,$q,$w,$Dd,$y,$z},$A);$Fd=q#quote_object#;$Gd=[];$Hd=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$Id=bless({$t,$Gd,$v,$q,$w,$Hd,$y,$z},$A);$Jd={$xd,$Ad,$Bd,$Ed,$Fd,$Id};$Kd=q#/lib/quote_objects.b#;$Ld=bless({$d3,$wd,$E4,$q,$F4,$q,$G4,$Jd,$Q,$Kd},$f4);$Md={};$Nd=q#circular_arrayref#;$Od=[];$Pd=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Qd=bless({$t,$Od,$v,$q,$w,$Pd,$y,$z},$A);$Rd=q#circular_hashref#;$Sd=[];$Td=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Ud=bless({$t,$Sd,$v,$q,$w,$Td,$y,$z},$A);$Vd=q#is_circular#;$Wd=[];$Xd=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Yd=bless({$t,$Wd,$v,$q,$w,$Xd,$y,$z},$A);$Zd={$Nd,$Qd,$Rd,$Ud,$Vd,$Yd};$ce=q#/lib/quote_circular_addressed.b#;$de=bless({$d3,$Md,$E4,$q,$F4,$q,$G4,$Zd,$Q,$ce},$f4);$ee={};$fe=q#address#;$ge=[];$he=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$ie=bless({$t,$ge,$v,$q,$w,$he,$y,$z},$A);$je=q#allocate_gensym#;$ke=[];$le=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$me=bless({$t,$ke,$v,$q,$w,$le,$y,$z},$A);$ne=q#circular_links#;$oe=[];$pe=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$qe=bless({$t,$oe,$v,$q,$w,$pe,$y,$z},$A);$re=q#quote#;$se=[];$te=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$ue=bless({$t,$se,$v,$q,$w,$te,$y,$z},$A);$ve={$fe,$ie,$je,$me,$ne,$qe,$re,$ue};$we=q#/lib/quote_gensym_identity.b#;$xe=bless({$d3,$ee,$E4,$q,$F4,$q,$G4,$ve,$Q,$we},$f4);$ye={};$ze=q#gensym#;$Ae=[];$Be=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Ce=bless({$t,$Ae,$v,$q,$w,$Be,$y,$z},$A);$De={$ze,$Ce};$Ee=q#/lib/gensym_generator_compact.b#;$Fe=bless({$d3,$ye,$E4,$q,$F4,$q,$G4,$De,$Q,$Ee},$f4);$Ge=[$R4,$pc,$Nc,$Vc,$vd,$Ld,$de,$xe,$Fe];$He=bless({$d3,$hc,$Q,$ic,$q3,$Ge},$W3);$Ie=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$Je=[$Ie];$Ke=bless({$c3,$He,$E,$Je},$H);$Le=[$Z2,$Ke];$Me=q#Use a packed format with BER length encoding (which is portable), and
serialize the apply-journals for behaviors.#;$Ne=[$Me];$Oe=bless({$c3,$He,$E,$Ne},$H);$Pe=[$Z2,$Oe];$Qe=[$ec,$gc,$Le,$Pe];$Re=bless({$e,$Qe,$Q,$ic},$S);$Se=q#ni.doc:/lib/ni#;$Te=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$Ue=[$f,$Te];$Ve=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$We=[$i,$Ve];$Xe=[$Ue,$We];$Ye=q#/lib/ni#;$Ze=bless({$e,$Xe,$Q,$Ye},$S);$cf=q#ni.doc:/lib/quote_simple#;$df=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$ef=[];$ff=[];$gf=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$hf=bless({$t,$ff,$v,$q,$w,$gf,$y,$z},$A);$if=bless({$n,$ef,$p,$q,$r,$q,$s,$hf},$C);$jf=[$i,$df,$if];$kf=[$jf];$lf=q#/lib/quote_simple#;$mf=bless({$e,$kf,$Q,$lf},$S);$nf=q#ni.doc:/lib/slice#;$of=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$pf=[$f,$of];$qf={$f4,1};$rf=q#/lib/slice#;$sf=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$tf=bless({$w,$sf,$y,$z},$A);$uf=q#local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
if (grep /^\\(/, keys %{$$self{methods}}) {
  *{"$p\\::()"} = *{"$p\\::(("} = sub {};
  *{"$p\\::OVERLOAD"} = {dummy => 1};
}
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;#;$vf=bless({$w,$uf,$y,$z},$A);$wf=q#ni::/lib/slice::apply#;$xf=q#ni::/lib/slice::apply_#;$yf={};$zf={$T8,$tf,$W8,$vf};$Af=q#/lib/slice.b#;$Bf=bless({$d3,$yf,$G4,$zf,$Q,$Af},$f4);$Cf={};$Df=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$Ef=bless({$w,$Df,$y,$z},$A);$Ff={$U6,$Ef};$Gf=q#/lib/slice_init.b#;$Hf=bless({$d3,$Cf,$G4,$Ff,$Q,$Gf},$f4);$If=[$L8,$R8,$Bf,$Hf,$M9];$Jf=bless({$d3,$qf,$Q,$rf,$q3,$If},$g4);$Kf=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$Lf=[$Kf];$Mf=bless({$c3,$Jf,$E,$Lf},$H);$Nf=[$Z2,$Mf];$Of=q#Anything on the Perl definition-side of things needs to be version
controlled. The case we need to handle here is running unit tests inside
a remote; at the end, we should be able to restore the remote state
cleanly (or branch it, or something). We don't want unit test results to
hang around and corrupt the image we then propagate from there.#;$Pf=[$Of];$Qf=bless({$c3,$Jf,$E,$Pf},$H);$Rf=[$Z2,$Qf];$Sf=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$Tf=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$Uf=[];$Vf=[];$Wf=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$Xf=bless({$t,$Vf,$v,$q,$w,$Wf,$y,$z},$A);$Yf=bless({$n,$Uf,$p,$q,$r,$q,$s,$Xf},$C);$Zf=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$cg=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$dg=[];$eg=[];$fg=q#my $instances = 0;
my $class = ni('ni:/object')->child('test/foo2')
  ->def('test/foo2_init.b',
    instantiate => fn q{+{}},
    ctor => sub {++$instances},
    dtor => sub {--$instances});
now $instances == 0;
{
  my $i1 = $class->new;
  now $instances == 1;
  my $i2 = $class->new;
  now $instances == 2;
}
now $instances == 0;#;$gg=bless({$t,$eg,$v,$q,$w,$fg,$y,$z},$A);$hg=bless({$n,$dg,$p,$q,$r,$q,$s,$gg},$C);$ig=[$i,$Sf,$Tf,$Yf,$Zf,$cg,$hg];$jg=[$pf,$Nf,$Rf,$ig];$kg=bless({$e,$jg,$Q,$rf},$S);$lg=q#ni.doc:/semantic#;$mg=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$ng=[$i,$mg];$og=[$ng];$pg=q#/semantic#;$qg=bless({$e,$og,$Q,$pg},$S);$rg=q#ni.doc:ni#;$sg=q#ni#;$tg={$sg,1};$ug={};$vg=q#abbrev#;$wg=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$xg=bless({$w,$wg,$y,$z},$A);$yg=q#dor#;$zg=q#defined $_[0] ? $_[0] : $_[1]#;$Ag=bless({$w,$zg,$y,$z},$A);$Bg=q#indent#;$Cg=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$Dg=bless({$w,$Cg,$y,$z},$A);$Eg=q#max#;$Fg=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$Gg=bless({$w,$Fg,$y,$z},$A);$Hg=q#maxstr#;$Ig=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$Jg=bless({$w,$Ig,$y,$z},$A);$Kg=q#mean#;$Lg=q#sum(@_) / (@_ || 1)#;$Mg=bless({$w,$Lg,$y,$z},$A);$Ng=q#min#;$Og=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$Pg=bless({$w,$Og,$y,$z},$A);$Qg=q#minstr#;$Rg=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$Sg=bless({$w,$Rg,$y,$z},$A);$Tg=q#outdent#;$Ug=q#my $x = shift;
return $x if ref $x;
$x =~ s/^[ \\t]*|\\s*$//g;
my @lines = split /\\n/, $x;
return $x unless @lines > 1;
my $indent = $lines[1] =~ /^(\\s*)\\S/ ? length $1 : undef;
for (@lines[2..$\#lines]) {
  my $li = /^(\\s*)\\S/ ? length $1 : next;
  $indent = length $1 if !defined($indent) || length $1 < $indent;
}
my $spaces = ' ' x $indent;
s/^$spaces// for @lines;
join "\\n", @lines;#;$Vg=bless({$w,$Ug,$y,$z},$A);$Wg=q#sgr#;$Xg=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$Yg=bless({$w,$Xg,$y,$z},$A);$Zg=q#sr#;$ch=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$dh=bless({$w,$ch,$y,$z},$A);$eh=q#sum#;$fh=q#local $_; my $x = 0; $x += $_ for @_; $x#;$gh=bless({$w,$fh,$y,$z},$A);$hh=q#swap#;$ih=q#@_[0, 1] = @_[1, 0]#;$jh=bless({$w,$ih,$y,$z},$A);$kh={$vg,$xg,$yg,$Ag,$Bg,$Dg,$Eg,$Gg,$Hg,$Jg,$Kg,$Mg,$Ng,$Pg,$Qg,$Sg,$Tg,$Vg,$Wg,$Yg,$Zg,$dh,$eh,$gh,$hh,$jh};$lh=q#/lib/ni_static_util.b#;$mh=bless({$d3,$ug,$E4,$q,$F4,$q,$G4,$kh,$Q,$lh},$f4);$nh={};$oh=q#data#;$ph=q#json_escapes#;$qh=q##;$rh=q#b#;$sh=q#	#;$th=q#t#;$uh=q#
#;$vh=q#n#;$wh=q##;$xh=q#f#;$yh=q##;$zh=q#r#;$Ah=q#"#;$Bh=q#/#;$Ch=q#\\#;$Dh={$qh,$rh,$sh,$th,$uh,$vh,$wh,$xh,$yh,$zh,$Ah,$Ah,$Bh,$Bh,$Ch,$Ch};$Eh=q#json_unescapes#;$Fh={$Ah,$Ah,$Bh,$Bh,$Ch,$Ch,$rh,$qh,$xh,$wh,$vh,$uh,$zh,$yh,$th,$sh};$Gh={$ph,$Dh,$Eh,$Fh};$Hh=q#/lib/json_data.b#;$Ih=bless({$d3,$nh,$oh,$Gh,$Q,$Hh},$P3);$Jh={};$Kh=q#json_decode#;$Lh=[];$Mh=q#local $_;
my @v = [];
for ($_[0] =~ /[][{}]|true|false|null|"(?:[^"\\\\]+|\\\\.)*"|[-+eE\\d.]+/g) {
  if (/^[[{]$/) {
    push @v, [];
  } elsif (/^\\]$/) {
    die "json_decode $_[0]: too many closing brackets" if @v < 2;
    push @{$v[-2]}, $v[-1];
    pop @v;
  } elsif (/^\\}$/) {
    die "json_decode $_[0]: too many closing brackets" if @v < 2;
    push @{$v[-2]}, {@{$v[-1]}};
    pop @v;
  } else {
    push @{$v[-1]}, /^"/      ? json_unescape $_
                  : /^true$/  ? 1
                  : /^false$/ ? 0
                  : /^null$/  ? undef
                  :             0 + $_;
  }
}
my $r = pop @v;
die "json_decode $_[0]: not enough closing brackets" if @v;
wantarray ? @$r : $$r[0];#;$Nh=q#($)#;$Oh=bless({$t,$Lh,$v,$q,$w,$Mh,$y,$Nh},$A);$Ph=q#json_encode#;$Qh=[];$Rh=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Sh=bless({$t,$Qh,$v,$q,$w,$Rh,$y,$Nh},$A);$Th=q#json_encode_pretty#;$Uh=[];$Vh=q#local $_;
my ($v, $indent) = @_;
$indent ||= 0;
my $spaces = ' ' x $indent;
return "\\[\\n$spaces  "
     . join(",\\n$spaces  ", map ni::json_encode_pretty($_, $indent + 2), @$v)
     . "\\n$spaces]" if 'ARRAY' eq ref $v;

return "\\{\\n$spaces  "
     . join(",\\n$spaces  ", map ni::json_escape($_) . ": "
                              . ni::json_encode_pretty($$v{$_}, $indent + 2),
                            sort keys %$v)
     . "\\n$spaces\\}" if 'HASH' eq ref $v;

ni::json_encode($v);#;$Wh=bless({$t,$Uh,$v,$q,$w,$Vh,$y,$z},$A);$Xh=q#json_escape#;$Yh=[];$Zh=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$ci=bless({$t,$Yh,$v,$q,$w,$Zh,$y,$Nh},$A);$di=q#json_unescape#;$ei=[];$fi=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$gi=bless({$t,$ei,$v,$q,$w,$fi,$y,$Nh},$A);$hi=q#json_unescape_one#;$ii=[];$ji=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$ki=bless({$t,$ii,$v,$q,$w,$ji,$y,$Nh},$A);$li={$Kh,$Oh,$Ph,$Sh,$Th,$Wh,$Xh,$ci,$di,$gi,$hi,$ki};$mi=q#/lib/json.b#;$ni=bless({$d3,$Jh,$E4,$q,$F4,$q,$G4,$li,$Q,$mi},$f4);$oi=[$mh,$Ih,$ni];$pi=bless({$d3,$tg,$Q,$sg,$q3,$oi},$t4);$qi=q#Migrate JSON into its own module, e.g. ni::json::. Clearly it deserves
some amount of encapsulation, since all of its member functions already
have the same prefix.#;$ri=[$qi];$si=bless({$c3,$pi,$E,$ri},$H);$ti=[$Z2,$si];$ui=q#Test a "DangerJson" module that does s/// to transform JSON into a valid
Perl expression, then uses eval()#;$vi=[$ui];$wi=bless({$c3,$pi,$E,$vi},$H);$xi=[$Z2,$wi];$yi=[$ti,$xi];$zi=bless({$e,$yi,$Q,$sg},$S);$Ai=q#ni:/class#;$Bi={$r3,1,$s3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$G3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$R3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$e4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$p4,1,$q4,1,$s4,1,$u4,1,$w4,1,$x4,1,$y4,1,$A4,1};$Ci={$r3,1,$s3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$G3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$R3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$e4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$p4,1,$q4,1,$r4,1,$s4,1,$t4,1,$u4,1,$w4,1,$x4,1,$y4,1,$A4,1};$Di=q#/module#;$Ei=q#/lib/perlbranch.b#;$Fi={};$Gi=q#add#;$Hi=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Ii=bless({$w,$Hi,$y,$z},$A);$Ji=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Ki=bless({$w,$Ji,$y,$z},$A);$Li={$Gi,$Ii,$T8,$Ki};$Mi=q#/lib/branch.b#;$Ni=bless({$d3,$Fi,$E4,$q,$F4,$q,$G4,$Li,$Q,$Mi},$f4);$Oi=[$Ni,$R8,$q9,$x9,$E9];$Pi=bless({$Q,$Ei,$q3,$Oi},$h4);$Qi={};$Ri=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$Si=bless({$w,$Ri,$y,$z},$A);$Ti={$U6,$Si};$Ui=q#/lib/class_init.b#;$Vi=bless({$d3,$Qi,$E4,$T4,$F4,$q,$G4,$Ti,$Q,$Ui},$f4);$Wi={$r3,1,$s3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$G3,1,$I3,1,$K3,1,$M3,1,$N3,1,$O3,1,$Q3,1,$R3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$e4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$p4,1,$q4,1,$r4,1,$s4,1,$t4,1,$u4,1,$w4,1,$x4,1,$y4,1,$A4,1};$Xi=q#/lib/definition.b#;$Yi={};$Zi=q#def#;$cj=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$dj=bless({$w,$cj,$y,$z},$A);$ej={$Zi,$dj};$fj=q#/lib/definition_def.b#;$gj=bless({$d3,$Yi,$E4,$q,$F4,$q,$G4,$ej,$Q,$fj},$f4);$hj={};$ij=q#ro#;$jj=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$kj=bless({$w,$jj,$y,$z},$A);$lj=q#rw#;$mj=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$nj=bless({$w,$mj,$y,$z},$A);$oj={$ij,$kj,$lj,$nj};$pj=q#/lib/accessor.b#;$qj=bless({$d3,$hj,$E4,$q,$F4,$q,$G4,$oj,$Q,$pj},$f4);$rj={};$sj=q#(""#;$tj=q#shift->name#;$uj=bless({$w,$tj,$y,$z},$A);$vj={$sj,$uj};$wj=q#/lib/name_as_string.b#;$xj=bless({$d3,$rj,$E4,$q,$F4,$q,$G4,$vj,$Q,$wj},$f4);$yj={};$zj=q#(eq#;$Aj=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Bj=bless({$w,$Aj,$y,$z},$A);$Cj={$zj,$Bj};$Dj=q#/lib/ref_eq.b#;$Ej=bless({$d3,$yj,$E4,$q,$F4,$q,$G4,$Cj,$Q,$Dj},$f4);$Fj={};$Gj=q#defdata#;$Hj=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$Ij=bless({$w,$Hj,$y,$z},$A);$Jj={$Gj,$Ij};$Kj=q#/lib/definition_defdata.b#;$Lj=bless({$d3,$Fj,$E4,$q,$F4,$q,$G4,$Jj,$Q,$Kj},$f4);$Mj={};$Nj=q#instantiate_with_defaults#;$Oj=q#my ($class, @slots) = @_;
my $generator = pop @slots;
$class->def("$$class{name}_init.b",
  instantiate => fc(
    generator => $generator,
    '@slots'  => \\@slots,
    q{
      my %defaults = &$generator(@_);
      my $class    = shift;
      my %args     = @_;
      $defaults{$_} = $args{$_} for @slots;
      \\%defaults;
    }));#;$Pj=bless({$w,$Oj,$y,$z},$A);$Qj={$Nj,$Pj};$Rj=q#/lib/definition_init_with_defaults.b#;$Sj=bless({$d3,$Mj,$E4,$q,$F4,$q,$G4,$Qj,$Q,$Rj},$f4);$Tj=[$gj,$qj,$xj,$Ej,$Lj,$Sj];$Uj=bless({$d3,$Wi,$Q,$Xi,$q3,$Tj},$N3);$Vj=[$Pi,$Vi,$R4,$L8,$Uj];$Wj=bless({$d3,$Ci,$Q,$Di,$q3,$Vj},$u4);$Xj={};$Yj=q#new#;$Zj=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$ck=bless({$w,$Zj,$y,$z},$A);$dk={$Yj,$ck};$ek=q#/lib/instantiable.b#;$fk=bless({$d3,$Xj,$G4,$dk,$Q,$ek},$f4);$gk={};$hk=q#child#;$ik=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$jk=bless({$w,$ik,$y,$z},$A);$kk={$hk,$jk};$lk=q#/lib/subclass.b#;$mk=bless({$d3,$gk,$E4,$q,$F4,$q,$G4,$kk,$Q,$lk},$f4);$nk=[$Wj,$fk,$Vi,$Wj,$mk];$ok=bless({$d3,$Bi,$Q,$R,$q3,$nk},$s3);$pk=q#ni:/class.c#;$qk={$s3,1,$y4,1};$rk=q#/class.c#;$sk={$s3,1,$u4,1,$y4,1};$tk=q#/module.c#;$uk={$s3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$G3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$R3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$e4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$p4,1,$q4,1,$u4,1,$w4,1,$y4,1,$A4,1};$vk=q#/object.c#;$wk=[$ok];$xk=bless({$d3,$uk,$Q,$vk,$q3,$wk},$r4);$yk={$s3,1,$M3,1,$O3,1,$Q3,1,$g4,1,$i4,1,$u4,1,$y4,1};$zk=q#/lib/behavior.c#;$Ak=[$xk];$Bk=bless({$d3,$yk,$Q,$zk,$q3,$Ak},$r4);$Ck=[$xk,$fk,$Bk];$Dk=bless({$d3,$sk,$Q,$tk,$q3,$Ck},$r4);$Ek=[$Dk];$Fk=bless({$d3,$qk,$Q,$rk,$q3,$Ek},$r4);$Gk=q#ni:/fabric#;$Hk=q#ni::/fabric#;$Ik={$Hk,1};$Jk=[];$Kk=bless({$d3,$Ik,$Q,$e1,$q3,$Jk},$t4);$Lk=q#ni:/fabric/native#;$Mk={$t3,1};$Nk=[$R4];$Ok=bless({$d3,$Mk,$Q,$m1,$q3,$Nk},$u3);$Pk=q#ni:/fabric/native.c#;$Qk={$u3,1};$Rk=q#/fabric/native.c#;$Sk=[$xk];$Tk=bless({$d3,$Qk,$Q,$Rk,$q3,$Sk},$r4);$Uk=q#ni:/io#;$Vk=q#ni::/io#;$Wk={$Vk,1};$Xk=[];$Yk=bless({$d3,$Wk,$Q,$s1,$q3,$Xk},$t4);$Zk=q#ni:/io/buffer#;$cl={$e3,1};$dl={};$el=[];$fl=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$gl=bless({$t,$el,$v,$q,$w,$fl,$y,$z},$A);$hl={$U6,$gl};$il=q#/io/buffer_init.b#;$jl=bless({$d3,$dl,$E4,$q,$F4,$q,$G4,$hl,$Q,$il},$f4);$kl={};$ll=[];$ml=q#my $self = shift;
my $rcap = $self->read_capacity;
$! = Errno::EWOULDBLOCK, return undef unless $rcap;

my $length = $_[1];
my $offset = $_[2] || 0;
$length = $rcap if $length > $rcap;

my $capacity   = $$self{capacity};
my $read_index = $$self{read_point} & $capacity - 1;
if ($read_index + $length > $capacity) {
  my $read_size = $capacity - $read_index;
  if ($offset) {
    $_[0] ||= "\\0" x $length;
    substr $_[0], $offset, $read_size,
           substr $$self{data}, $read_index, $read_size;
    substr $_[0], $offset + $read_size, $length - $read_size,
           substr $$self{data}, 0, $length - $read_size;
  } else {
    $_[0] = substr($$self{data}, $read_index, $read_size)
          . substr($$self{data}, 0, $length - $read_size);
  }
  $$self{read_point} += $length;
  return $length;
} else {
  if ($offset) {
    substr $_[0], $offset, $length,
           substr $$self{data}, $read_index, $length;
  } else {
    $_[0] = substr $$self{data}, $read_index, $length;
  }
  $$self{read_point} += $length;
  return $length;
}#;$nl=bless({$t,$ll,$v,$q,$w,$ml,$y,$z},$A);$ol=q#read_capacity#;$pl=[];$ql=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$rl=bless({$t,$pl,$v,$q,$w,$ql,$y,$z},$A);$sl=[];$tl=q#my $self = shift;
my $wcap = $self->write_capacity;
$! = Errno::EWOULDBLOCK, return undef unless $wcap;

my $length = @_ > 1 ? $_[1] : length $_[0];
my $offset = $_[2] || 0;
$length    = $wcap if $length > $wcap;

my $capacity    = $$self{capacity};
my $write_index = $$self{write_point} & $capacity - 1;
if ($write_index + $length > $capacity) {
  \# Two-part write
  my $write_size = $capacity - $write_index;
  substr $$self{data}, $write_index, $write_size,
         substr $_[0], 0, $write_size;
  substr $$self{data}, 0, $length - $write_size,
         substr $_[0], $offset + $write_size, $length - $write_size;
  $$self{write_point} += $length;
  return $length;
} else {
  \# One-part write
  substr $$self{data}, $write_index, $length,
         length $_[0] == $length && !$offset
           ? $_[0]
           : substr $_[0], $offset, $length;
  $$self{write_point} += $length;
  return $length;
}#;$ul=bless({$t,$sl,$v,$q,$w,$tl,$y,$z},$A);$vl=q#write_capacity#;$wl=[];$xl=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$yl=bless({$t,$wl,$v,$q,$w,$xl,$y,$z},$A);$zl={$q7,$nl,$ol,$rl,$u7,$ul,$vl,$yl};$Al=q#/io/buffer_io.b#;$Bl=bless({$d3,$kl,$E4,$q,$F4,$q,$G4,$zl,$Q,$Al},$f4);$Cl=[$s6,$jl,$Bl];$Dl=bless({$d3,$cl,$Q,$F1,$q3,$Cl},$v3);$El=q#ni:/io/buffer.c#;$Fl={$v3,1};$Gl=q#/io/buffer.c#;$Hl={$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1};$Il=q#/io/object.c#;$Jl={};$Kl=q#def_transfer_method#;$Ll=[];$Ml=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Nl=bless({$t,$Ll,$v,$q,$w,$Ml,$y,$z},$A);$Ol={$Kl,$Nl};$Pl=q#/io/object.c_transfer_def.b#;$Ql=bless({$d3,$Jl,$E4,$q,$F4,$q,$G4,$Ol,$Q,$Pl},$f4);$Rl=[$xk,$Ql];$Sl=bless({$d3,$Hl,$Q,$Il,$q3,$Rl},$r4);$Tl=[$Sl];$Ul=bless({$d3,$Fl,$Q,$Gl,$q3,$Tl},$r4);$Vl=q#ni:/io/buffer_init.b#;$Wl=q#ni:/io/buffer_io.b#;$Xl=q#ni:/io/cat#;$Yl={$f3,1};$Zl={};$cm=[];$dm=q#shift; +{fs => [@_]}#;$em=bless({$t,$cm,$v,$q,$w,$dm,$y,$z},$A);$fm={$U6,$em};$gm=q#/io/cat_init.b#;$hm=bless({$d3,$Zl,$E4,$q,$F4,$q,$G4,$fm,$Q,$gm},$f4);$im={};$jm=[];$km=q#my $fs = shift->{fs};
my $length = $_[1];
my $offset = $_[2] || 0;
my $total_read = 0;
my $n;
while (@$fs && $total_read < $length) {
  $n = $$fs[0]->read($_[0], $length - $total_read, $offset + $total_read);
  return $total_read || undef unless defined $n;
  shift @$fs unless $n;
  $total_read += $n;
}
$total_read;#;$lm=bless({$t,$jm,$v,$q,$w,$km,$y,$z},$A);$mm={$q7,$lm};$nm=q#/io/cat_read.b#;$om=bless({$d3,$im,$E4,$q,$F4,$q,$G4,$mm,$Q,$nm},$f4);$pm=[$s6,$hm,$om];$qm=bless({$d3,$Yl,$Q,$S1,$q3,$pm},$w3);$rm=q#ni:/io/cat.c#;$sm={$w3,1};$tm=q#/io/cat.c#;$um=[$Sl];$vm=bless({$d3,$sm,$Q,$tm,$q3,$um},$r4);$wm=q#ni:/io/cat_init.b#;$xm=q#ni:/io/cat_read.b#;$ym=q#ni:/io/exec#;$zm={$g3,1};$Am={};$Bm=q#argv#;$Cm=[];$Dm=q#shift->{'argv'}#;$Em=bless({$t,$Cm,$v,$q,$w,$Dm,$y,$z},$A);$Fm={$Bm,$Em};$Gm=q#/io/exec_ro.b#;$Hm=bless({$d3,$Am,$E4,$q,$F4,$q,$G4,$Fm,$Q,$Gm},$f4);$Im={};$Jm=[];$Km=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Lm=bless({$t,$Jm,$v,$q,$w,$Km,$y,$z},$A);$Mm={$U6,$Lm};$Nm=q#/io/exec_init.b#;$Om=bless({$d3,$Im,$E4,$q,$F4,$q,$G4,$Mm,$Q,$Nm},$f4);$Pm={};$Qm=q#connect#;$Rm=[];$Sm=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Tm=bless({$t,$Rm,$v,$q,$w,$Sm,$y,$z},$A);$Um=q#in_pipe#;$Vm=[];$Wm=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Xm=bless({$t,$Vm,$v,$q,$w,$Wm,$y,$z},$A);$Ym=q#out_pipe#;$Zm=[];$cn=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$dn=bless({$t,$Zm,$v,$q,$w,$cn,$y,$z},$A);$en=q#setup_stdio#;$fn=[];$gn=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$hn=bless({$t,$fn,$v,$q,$w,$gn,$y,$z},$A);$in={$Qm,$Tm,$Um,$Xm,$Ym,$dn,$en,$hn};$jn=q#/io/exec_io_setup.b#;$kn=bless({$d3,$Pm,$E4,$q,$F4,$q,$G4,$in,$Q,$jn},$f4);$ln={};$mn=q#binds_fd#;$nn=[];$on=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$pn=bless({$t,$nn,$v,$q,$w,$on,$y,$z},$A);$qn=[];$rn=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$sn=bless({$t,$qn,$v,$q,$w,$rn,$y,$z},$A);$tn=[];$un=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$vn=bless({$t,$tn,$v,$q,$w,$un,$y,$z},$A);$wn=[];$xn=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$yn=bless({$t,$wn,$v,$q,$w,$xn,$y,$z},$A);$zn=[];$An=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Bn=bless({$t,$zn,$v,$q,$w,$An,$y,$z},$A);$Cn={$mn,$pn,$C7,$sn,$G7,$vn,$K7,$yn,$O7,$Bn};$Dn=q#/io/exec_io_accessors.b#;$En=bless({$d3,$ln,$E4,$q,$F4,$q,$G4,$Cn,$Q,$Dn},$f4);$Fn={};$Gn=q#env#;$Hn=[];$In=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Jn=bless({$t,$Hn,$v,$q,$w,$In,$y,$z},$A);$Kn={$Gn,$Jn};$Ln=q#/io/exec_env.b#;$Mn=bless({$d3,$Fn,$E4,$q,$F4,$q,$G4,$Kn,$Q,$Ln},$f4);$Nn={};$On=q#exec#;$Pn=[];$Qn=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Rn=bless({$t,$Pn,$v,$q,$w,$Qn,$y,$z},$A);$Sn=q#fork#;$Tn=[];$Un=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Vn=bless({$t,$Tn,$v,$q,$w,$Un,$y,$z},$A);$Wn=q#move_fds#;$Xn=[];$Yn=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Zn=bless({$t,$Xn,$v,$q,$w,$Yn,$y,$z},$A);$co={$On,$Rn,$Sn,$Vn,$Wn,$Zn};$do=q#/io/exec_fork.b#;$eo=bless({$d3,$Nn,$E4,$q,$F4,$q,$G4,$co,$Q,$do},$f4);$fo=[$s6,$Hm,$Om,$kn,$En,$Mn,$eo];$go=bless({$d3,$zm,$Q,$h2,$q3,$fo},$x3);$ho=q#ni:/io/exec.c#;$io={$x3,1};$jo=q#/io/exec.c#;$ko=[$Sl];$lo=bless({$d3,$io,$Q,$jo,$q3,$ko},$r4);$mo=q#ni:/io/exec_env.b#;$no=q#ni:/io/exec_fork.b#;$oo=q#ni:/io/exec_init.b#;$po=q#ni:/io/exec_io_accessors.b#;$qo=q#ni:/io/exec_io_setup.b#;$ro=q#ni:/io/exec_ro.b#;$so=q#ni:/io/fd#;$to={$h3,1};$uo=q#read_fd_mask#;$vo={};$wo=[];$xo=q#shift->{'fd'}#;$yo=bless({$t,$wo,$v,$q,$w,$xo,$y,$z},$A);$zo={$C7,$yo};$Ao=q#/io/fd_readers.b#;$Bo=bless({$d3,$vo,$E4,$q,$F4,$q,$G4,$zo,$Q,$Ao},$f4);$Co={};$Do=[];$Eo=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Fo=bless({$t,$Do,$v,$q,$w,$Eo,$y,$z},$A);$Go={$U6,$Fo};$Ho=q#/io/fd_init.b#;$Io=bless({$d3,$Co,$E4,$q,$F4,$q,$G4,$Go,$Q,$Ho},$f4);$Jo={};$Ko=q#be#;$Lo=[];$Mo=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$No=bless({$t,$Lo,$v,$q,$w,$Mo,$y,$z},$A);$Oo={$Ko,$No};$Po=q#/io/fd_shell.b#;$Qo=bless({$d3,$Jo,$E4,$q,$F4,$q,$G4,$Oo,$Q,$Po},$f4);$Ro={};$So=q#cloexec#;$To=[];$Uo=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Vo=bless({$t,$To,$v,$q,$w,$Uo,$y,$z},$A);$Wo=q#fcntl_flag#;$Xo=[];$Yo=q#my ($self, $flag, $value) = @_;
$self->io_check_true(*main::open2, $$self{rfh}, "<&=$$self{fd}")
  unless $$self{rfh};
my $flags = $self->io_check_true(
  *main::fcntl, $$self{rfh}, Fcntl::F_GETFL, 0);
if (@_) {
  if (shift) {$flags |= $flag}
  else       {$flags &= ~$flag}
  $self->io_check_true(*main::fcntl, $$self{rfh}, Fcntl::F_SETFL, $flags);
  $self;
} else {
  !!($flags & $flag);
}#;$Zo=bless({$t,$Xo,$v,$q,$w,$Yo,$y,$z},$A);$cp=q#nonblock#;$dp=[];$ep=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$fp=bless({$t,$dp,$v,$q,$w,$ep,$y,$z},$A);$gp={$So,$Vo,$Wo,$Zo,$cp,$fp};$hp=q#/io/fd_fcntl.b#;$ip=bless({$d3,$Ro,$E4,$q,$F4,$q,$G4,$gp,$Q,$hp},$f4);$jp={};$kp=[];$lp=q#shift->close#;$mp=bless({$t,$kp,$v,$q,$w,$lp,$y,$z},$A);$np=q#close#;$op=[];$pp=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$qp=bless({$t,$op,$v,$q,$w,$pp,$y,$z},$A);$rp={$np,$qp};$sp=q#/io/fd_gc.b#;$tp=bless({$d3,$jp,$E4,$q,$F4,$mp,$G4,$rp,$Q,$sp},$f4);$up={};$vp=q#close_perl_ios#;$wp=[];$xp=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$yp=bless({$t,$wp,$v,$q,$w,$xp,$y,$z},$A);$zp=[];$Ap=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Bp=bless({$t,$zp,$v,$q,$w,$Ap,$y,$z},$A);$Cp=[];$Dp=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Ep=bless({$t,$Cp,$v,$q,$w,$Dp,$y,$z},$A);$Fp={$vp,$yp,$q7,$Bp,$u7,$Ep};$Gp=q#/io/fd_perlio.b#;$Hp=bless({$d3,$up,$E4,$q,$F4,$q,$G4,$Fp,$Q,$Gp},$f4);$Ip=[$s6,$Bo,$Io,$Qo,$ip,$tp,$Hp];$Jp=q#write_fd_mask#;$Kp=bless({$d3,$to,$Q,$u2,$uo,$z,$q3,$Ip,$Jp,$z},$y3);$Lp=[];$Mp=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Np=bless({$t,$Lp,$v,$q,$w,$Mp,$y,$z},$A);$Op=q#ni:/io/fd.c#;$Pp={$y3,1};$Qp=q#/io/fd.c#;$Rp={};$Sp=q#clear_fd#;$Tp=[];$Up=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Vp=bless({$t,$Tp,$v,$q,$w,$Up,$y,$z},$A);$Wp=q#read_fd#;$Xp=[];$Yp=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Zp=bless({$t,$Xp,$v,$q,$w,$Yp,$y,$z},$A);$cq=q#select#;$dq=[];$eq=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$fq=bless({$t,$dq,$v,$q,$w,$eq,$y,$z},$A);$gq=q#write_fd#;$hq=[];$iq=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$jq=bless({$t,$hq,$v,$q,$w,$iq,$y,$z},$A);$kq={$Sp,$Vp,$Wp,$Zp,$cq,$fq,$gq,$jq};$lq=q#/io/fd.c_selector.b#;$mq=bless({$d3,$Rp,$E4,$Np,$F4,$q,$G4,$kq,$Q,$lq},$f4);$nq=[$Sl,$mq];$oq=bless({$d3,$Pp,$Q,$Qp,$q3,$nq},$r4);$pq=q#ni:/io/fd.c_selector.b#;$qq=q#ni:/io/fd_fcntl.b#;$rq=q#ni:/io/fd_gc.b#;$sq=q#ni:/io/fd_init.b#;$tq=q#ni:/io/fd_perlio.b#;$uq=q#ni:/io/fd_readers.b#;$vq=q#ni:/io/fd_shell.b#;$wq=q#ni:/io/file#;$xq={$i3,1};$yq={};$zq=[];$Aq=q#shift->{'name'}#;$Bq=bless({$t,$zq,$v,$q,$w,$Aq,$y,$z},$A);$Cq={$Q,$Bq};$Dq=q#/io/file_readers.b#;$Eq=bless({$d3,$yq,$E4,$q,$F4,$q,$G4,$Cq,$Q,$Dq},$f4);$Fq={};$Gq=q#mode#;$Hq=[];$Iq=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$Jq=bless({$t,$Hq,$v,$q,$w,$Iq,$y,$z},$A);$Kq={$Gq,$Jq};$Lq=q#/io/file_accessors.b#;$Mq=bless({$d3,$Fq,$E4,$q,$F4,$q,$G4,$Kq,$Q,$Lq},$f4);$Nq={};$Oq=[];$Pq=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Qq=bless({$t,$Oq,$v,$q,$w,$Pq,$y,$z},$A);$Rq={$U6,$Qq};$Sq=q#/io/file_init.b#;$Tq=bless({$d3,$Nq,$E4,$q,$F4,$q,$G4,$Rq,$Q,$Sq},$f4);$Uq={};$Vq=q#(-X#;$Wq=[];$Xq=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Yq=bless({$t,$Wq,$v,$q,$w,$Xq,$y,$z},$A);$Zq=q#mv#;$cr=[];$dr=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$er=bless({$t,$cr,$v,$q,$w,$dr,$y,$z},$A);$fr=q#rm#;$gr=[];$hr=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$ir=bless({$t,$gr,$v,$q,$w,$hr,$y,$z},$A);$jr={$Vq,$Yq,$Zq,$er,$fr,$ir};$kr=q#/io/file_fns.b#;$lr=bless({$d3,$Uq,$E4,$q,$F4,$q,$G4,$jr,$Q,$kr},$f4);$mr={};$nr=q#atomic_update#;$or=[];$pr=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$qr=bless({$t,$or,$v,$q,$w,$pr,$y,$z},$A);$rr={$nr,$qr};$sr=q#/io/file_update.b#;$tr=bless({$d3,$mr,$E4,$q,$F4,$q,$G4,$rr,$Q,$sr},$f4);$ur={};$vr=[];$wr=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$xr=bless({$t,$vr,$v,$q,$w,$wr,$y,$z},$A);$yr=[];$zr=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Ar=bless({$t,$yr,$v,$q,$w,$zr,$y,$z},$A);$Br=[];$Cr=q#shift->r->read(@_)#;$Dr=bless({$t,$Br,$v,$q,$w,$Cr,$y,$z},$A);$Er=q#w#;$Fr=[];$Gr=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Hr=bless({$t,$Fr,$v,$q,$w,$Gr,$y,$z},$A);$Ir=[];$Jr=q#shift->w->write(@_)#;$Kr=bless({$t,$Ir,$v,$q,$w,$Jr,$y,$z},$A);$Lr={$np,$xr,$zh,$Ar,$q7,$Dr,$Er,$Hr,$u7,$Kr};$Mr=q#/io/file_io.b#;$Nr=bless({$d3,$ur,$E4,$q,$F4,$q,$G4,$Lr,$Q,$Mr},$f4);$Or=[$s6,$Eq,$Mq,$Tq,$lr,$tr,$Nr];$Pr=bless({$d3,$xq,$Q,$Q2,$q3,$Or},$z3);$Qr=q#ni:/io/file.c#;$Rr={$z3,1};$Sr=q#/io/file.c#;$Tr=[$Sl];$Ur=bless({$d3,$Rr,$Q,$Sr,$q3,$Tr},$r4);$Vr=q#ni:/io/file_accessors.b#;$Wr=q#ni:/io/file_fns.b#;$Xr=q#ni:/io/file_init.b#;$Yr=q#ni:/io/file_io.b#;$Zr=q#ni:/io/file_readers.b#;$cs=q#ni:/io/file_update.b#;$ds=q#ni:/io/file_update_fd#;$es={$j3,1};$fs={};$gs=[];$hs=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$is=bless({$t,$gs,$v,$q,$w,$hs,$y,$z},$A);$js={$U6,$is};$ks=q#/io/file_update_fd_fd_init.b#;$ls=bless({$d3,$fs,$E4,$q,$F4,$q,$G4,$js,$Q,$ks},$f4);$ms={};$ns=[];$os=bless({$t,$ns,$v,$q,$w,$lp,$y,$z},$A);$ps=[];$qs=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$rs=bless({$t,$ps,$v,$q,$w,$qs,$y,$z},$A);$ss={$np,$rs};$ts=q#/io/file_update_fd_fd_gc.b#;$us=bless({$d3,$ms,$E4,$q,$F4,$os,$G4,$ss,$Q,$ts},$f4);$vs=[$s6,$Bo,$ip,$Hp,$ls,$us];$ws=bless({$d3,$es,$Q,$W2,$q3,$vs},$A3);$xs=q#ni:/io/file_update_fd.c#;$ys={$A3,1};$zs=q#/io/file_update_fd.c#;$As=[$Sl];$Bs=bless({$d3,$ys,$Q,$zs,$q3,$As},$r4);$Cs=q#ni:/io/file_update_fd_fd_gc.b#;$Ds=q#ni:/io/file_update_fd_fd_init.b#;$Es=q#ni:/io/named_io_fns.b#;$Fs={};$Gs=q#fcntl#;$Hs=[];$Is=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Js=bless({$t,$Hs,$v,$q,$w,$Is,$y,$z},$A);$Ks=[];$Ls=q#CORE::fork#;$Ms=bless({$t,$Ks,$v,$q,$w,$Ls,$y,$z},$A);$Ns=q#open2#;$Os=[];$Ps=q#CORE::open $_[0], $_[1]#;$Qs=bless({$t,$Os,$v,$q,$w,$Ps,$y,$z},$A);$Rs=q#rename#;$Ss=[];$Ts=q#CORE::rename $_[0], $_[1]#;$Us=bless({$t,$Ss,$v,$q,$w,$Ts,$y,$z},$A);$Vs=q#unlink#;$Ws=[];$Xs=q#CORE::unlink @_#;$Ys=bless({$t,$Ws,$v,$q,$w,$Xs,$y,$z},$A);$Zs=q#waitpid#;$ct=[];$dt=q#CORE::waitpid $_[0], $_[1]#;$et=bless({$t,$ct,$v,$q,$w,$dt,$y,$z},$A);$ft={$Gs,$Js,$Sn,$Ms,$Ns,$Qs,$Rs,$Us,$Vs,$Ys,$Zs,$et};$gt=q#/io/named_io_fns.b#;$ht=bless({$d3,$Fs,$E4,$q,$F4,$q,$G4,$ft,$Q,$gt},$f4);$it=q#main#;$jt=q#ni:/io/null#;$kt={$k3,1};$lt=q#/io/null#;$mt={};$nt=[];$ot=q#+{fd => undef}#;$pt=bless({$t,$nt,$v,$q,$w,$ot,$y,$z},$A);$qt={$U6,$pt};$rt=q#/io/null_init.b#;$st=bless({$d3,$mt,$E4,$q,$F4,$q,$G4,$qt,$Q,$rt},$f4);$tt={};$ut=[];$vt=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$wt=bless({$t,$ut,$v,$q,$w,$vt,$y,$z},$A);$xt=[];$yt=q#shift->fd->read(@_)#;$zt=bless({$t,$xt,$v,$q,$w,$yt,$y,$z},$A);$At=[];$Bt=q#shift->fd->write(@_)#;$Ct=bless({$t,$At,$v,$q,$w,$Bt,$y,$z},$A);$Dt={$C7,$wt,$q7,$zt,$u7,$Ct};$Et=q#/io/null_io.b#;$Ft=bless({$d3,$tt,$E4,$q,$F4,$q,$G4,$Dt,$Q,$Et},$f4);$Gt=[$s6,$st,$Ft];$Ht=bless({$d3,$kt,$Q,$lt,$q3,$Gt},$B3);$It=q#ni:/io/null.c#;$Jt={$B3,1};$Kt=q#/io/null.c#;$Lt=[$Sl];$Mt=bless({$d3,$Jt,$Q,$Kt,$q3,$Lt},$r4);$Nt=q#ni:/io/null_init.b#;$Ot=q#ni:/io/null_io.b#;$Pt=q#ni:/io/object#;$Qt=q#ni:/io/object.c#;$Rt=q#ni:/io/object.c_transfer_def.b#;$St=q#ni:/io/object_checks.b#;$Tt=q#ni:/io/object_constructors.b#;$Ut=q#ni:/io/object_memory.b#;$Vt=q#ni:/io/object_ops.b#;$Wt=q#ni:/io/object_transfer_async.b#;$Xt=q#ni:/io/object_transfer_sync.b#;$Yt=q#ni:/io/pid#;$Zt=q#ni:/io/pid.c#;$cu={$D3,1};$du=q#/io/pid.c#;$eu=[$Sl];$fu=bless({$d3,$cu,$Q,$du,$q3,$eu},$r4);$gu=q#ni:/io/pid_accessors.b#;$hu=q#ni:/io/pid_init.b#;$iu=q#ni:/io/pid_io.b#;$ju=q#ni:/io/pid_readers.b#;$ku=q#ni:/io/pid_wait.b#;$lu=q#ni:/io/str#;$mu={$n3,1};$nu=q#/io/str#;$ou={};$pu=[];$qu=q#shift->{'data'}#;$ru=bless({$t,$pu,$v,$q,$w,$qu,$y,$z},$A);$su=[];$tu=q#shift->{'end'}#;$uu=bless({$t,$su,$v,$q,$w,$tu,$y,$z},$A);$vu=q#start#;$wu=[];$xu=q#shift->{'start'}#;$yu=bless({$t,$wu,$v,$q,$w,$xu,$y,$z},$A);$zu={$oh,$ru,$za,$uu,$vu,$yu};$Au=q#/io/str_ro.b#;$Bu=bless({$d3,$ou,$E4,$q,$F4,$q,$G4,$zu,$Q,$Au},$f4);$Cu={};$Du=[];$Eu=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Fu=bless({$t,$Du,$v,$q,$w,$Eu,$y,$z},$A);$Gu={$U6,$Fu};$Hu=q#/io/str_init.b#;$Iu=bless({$d3,$Cu,$E4,$q,$F4,$q,$G4,$Gu,$Q,$Hu},$f4);$Ju={};$Ku=[];$Lu=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Mu=bless({$t,$Ku,$v,$q,$w,$Lu,$y,$z},$A);$Nu=q#remaining#;$Ou=[];$Pu=q#my $self = shift; $$self{end} - $$self{start}#;$Qu=bless({$t,$Ou,$v,$q,$w,$Pu,$y,$z},$A);$Ru=[];$Su=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Tu=bless({$t,$Ru,$v,$q,$w,$Su,$y,$z},$A);$Uu={$q7,$Mu,$Nu,$Qu,$u7,$Tu};$Vu=q#/io/str_io.b#;$Wu=bless({$d3,$Ju,$E4,$q,$F4,$q,$G4,$Uu,$Q,$Vu},$f4);$Xu=[$s6,$Bu,$Iu,$Wu];$Yu=bless({$d3,$mu,$Q,$nu,$q3,$Xu},$E3);$Zu=q#ni:/io/str.c#;$cv={$E3,1};$dv=q#/io/str.c#;$ev=[$Sl];$fv=bless({$d3,$cv,$Q,$dv,$q3,$ev},$r4);$gv=q#ni:/io/str_init.b#;$hv=q#ni:/io/str_io.b#;$iv=q#ni:/io/str_ro.b#;$jv=q#ni:/io/transfer#;$kv={$F3,1,$H3,1,$J3,1};$lv=q#/io/transfer#;$mv={$F3,1,$H3,1,$J3,1,$z4,1};$nv=q#/semantic/task#;$ov={};$pv=[];$qv=q#shift->{'outcome'}#;$rv=bless({$t,$pv,$v,$q,$w,$qv,$y,$z},$A);$sv={$r,$rv};$tv=q#/semantic/task_ro.b#;$uv=bless({$d3,$ov,$E4,$q,$F4,$q,$G4,$sv,$Q,$tv},$f4);$vv={};$wv=q#failure#;$xv=[];$yv=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$zv=bless({$t,$xv,$v,$q,$w,$yv,$y,$z},$A);$Av=q#success#;$Bv=[];$Cv=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Dv=bless({$t,$Bv,$v,$q,$w,$Cv,$y,$z},$A);$Ev={$wv,$zv,$Av,$Dv};$Fv=q#/semantic/task_outcome.b#;$Gv=bless({$d3,$vv,$E4,$q,$F4,$q,$G4,$Ev,$Q,$Fv},$f4);$Hv=[$R4,$uv,$Gv];$Iv=bless({$d3,$mv,$Q,$nv,$q3,$Hv},$A4);$Jv={};$Kv=[];$Lv=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Mv=bless({$t,$Kv,$v,$q,$w,$Lv,$y,$z},$A);$Nv=[];$Ov=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Pv=bless({$t,$Nv,$v,$q,$w,$Ov,$y,$z},$A);$Qv=[];$Rv=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Sv=bless({$t,$Qv,$v,$q,$w,$Rv,$y,$z},$A);$Tv={$q7,$Pv,$u7,$Sv};$Uv=q#/io/transfer_io_interop.b#;$Vv=bless({$d3,$Jv,$E4,$Mv,$F4,$q,$G4,$Tv,$Q,$Uv},$f4);$Wv={};$Xv=q#pressure#;$Yv=[];$Zv=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$cw=bless({$t,$Yv,$v,$q,$w,$Zv,$y,$z},$A);$dw=q#read_limit_throughput#;$ew=[];$fw=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$gw=bless({$t,$ew,$v,$q,$w,$fw,$y,$z},$A);$hw=q#throughput#;$iw=[];$jw=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$kw=bless({$t,$iw,$v,$q,$w,$jw,$y,$z},$A);$lw=q#write_limit_throughput#;$mw=[];$nw=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$ow=bless({$t,$mw,$v,$q,$w,$nw,$y,$z},$A);$pw={$Xv,$cw,$dw,$gw,$hw,$kw,$lw,$ow};$qw=q#/io/transfer_io_measurement.b#;$rw=bless({$d3,$Wv,$E4,$q,$F4,$q,$G4,$pw,$Q,$qw},$f4);$sw=[$Iv,$Vv,$rw];$tw=bless({$d3,$kv,$Q,$lv,$q3,$sw},$G3);$uw=[];$vw=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$ww=bless({$t,$uw,$v,$q,$w,$vw,$y,$z},$A);$xw=q#ni:/io/transfer.c#;$yw={$G3,1,$I3,1,$K3,1};$zw=q#/io/transfer.c#;$Aw={$G3,1,$I3,1,$K3,1,$A4,1};$Bw=q#/semantic/task.c#;$Cw=[$xk];$Dw=bless({$d3,$Aw,$Q,$Bw,$q3,$Cw},$r4);$Ew={};$Fw={};$Gw=q#/io/transfer.c_into.b#;$Hw=bless({$d3,$Ew,$E4,$ww,$F4,$q,$G4,$Fw,$Q,$Gw},$f4);$Iw=[$Dw,$Hw];$Jw=bless({$d3,$yw,$Q,$zw,$q3,$Iw},$r4);$Kw=q#ni:/io/transfer.c_into.b#;$Lw=q#ni:/io/transfer_async#;$Mw={$H3,1};$Nw=q#/io/transfer_async#;$Ow={};$Pw=q#dest_io#;$Qw=[];$Rw=q#shift->{'dest_io'}#;$Sw=bless({$t,$Qw,$v,$q,$w,$Rw,$y,$z},$A);$Tw=q#id#;$Uw=[];$Vw=q#shift->{'id'}#;$Ww=bless({$t,$Uw,$v,$q,$w,$Vw,$y,$z},$A);$Xw=q#source_io#;$Yw=[];$Zw=q#shift->{'source_io'}#;$cx=bless({$t,$Yw,$v,$q,$w,$Zw,$y,$z},$A);$dx={$Pw,$Sw,$Tw,$Ww,$Xw,$cx};$ex=q#/io/transfer_async_ro.b#;$fx=bless({$d3,$Ow,$E4,$q,$F4,$q,$G4,$dx,$Q,$ex},$f4);$gx={};$hx=[];$ix=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$jx=bless({$t,$hx,$v,$q,$w,$ix,$y,$z},$A);$kx={$U6,$jx};$lx=q#/io/transfer_async_init.b#;$mx=bless({$d3,$gx,$E4,$q,$F4,$q,$G4,$kx,$Q,$lx},$f4);$nx={};$ox=[];$px=q#ni('ni:/io/transfer_async')->track(shift)#;$qx=bless({$t,$ox,$v,$q,$w,$px,$y,$z},$A);$rx=[];$sx=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$tx=bless({$t,$rx,$v,$q,$w,$sx,$y,$z},$A);$ux={};$vx=q#/io/transfer_async_lifecycle.b#;$wx=bless({$d3,$nx,$E4,$qx,$F4,$tx,$G4,$ux,$Q,$vx},$f4);$xx={};$yx=q#run#;$zx=[];$Ax=q#shift#;$Bx=bless({$t,$zx,$v,$q,$w,$Ax,$y,$z},$A);$Cx=q#run_async#;$Dx=[];$Ex=q#my $self = shift;
my $n;

\# Step one: write everything in the pending queue, if possible. Invariant
\# after this if() condition is that $$self{pending} is empty unless there
\# was something preventing IO.
if (length $$self{pending}) {
write_branch:
  $n = 0;
  while ($n < length $$self{pending}) {
    my $x = $self->write($n ? substr($$self{pending}, $n)
                            : $$self{pending});
    last unless defined $x;
    $n += $x;
  }
  $$self{pending} = substr $$self{pending}, $n;
}

\# Step two: load more data into $$self{pending} and, if successful, go
\# back to step one and write it.
unless (length $$self{pending}) {
  goto write_branch if $n = $self->read($$self{pending}, 32768);
  return $self if $!{EINTR} || $!{EAGAIN} || $!{EWOULDBLOCK};
  if (defined $n) {
    $$self{end_time} = time;
    return $self->success;
  } else {
    $self->failure($!);
  }
}

$self;#;$Fx=bless({$t,$Dx,$v,$q,$w,$Ex,$y,$z},$A);$Gx={$yx,$Bx,$Cx,$Fx};$Hx=q#/io/transfer_async_run.b#;$Ix=bless({$d3,$xx,$E4,$q,$F4,$q,$G4,$Gx,$Q,$Hx},$f4);$Jx=[$tw,$fx,$mx,$wx,$Ix];$Kx=q#tracked_transfers#;$Lx={};$Mx=q#transfer_id#;$Nx=bless({$d3,$Mw,$Q,$Nw,$q3,$Jx,$Kx,$Lx,$Mx,0},$I3);$Ox=[];$Px=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Qx=bless({$t,$Ox,$v,$q,$w,$Px,$y,$z},$A);$Rx=q#ni:/io/transfer_async.c#;$Sx={$I3,1};$Tx=q#/io/transfer_async.c#;$Ux={};$Vx=q#new_id#;$Wx=[];$Xx=q#++shift->{transfer_id}#;$Yx=bless({$t,$Wx,$v,$q,$w,$Xx,$y,$z},$A);$Zx=q#track#;$cy=[];$dy=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$ey=bless({$t,$cy,$v,$q,$w,$dy,$y,$z},$A);$fy=q#untrack#;$gy=[];$hy=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$iy=bless({$t,$gy,$v,$q,$w,$hy,$y,$z},$A);$jy={$Vx,$Yx,$Zx,$ey,$fy,$iy};$ky=q#/io/transfer_async.c_tracker.b#;$ly=bless({$d3,$Ux,$E4,$Qx,$F4,$q,$G4,$jy,$Q,$ky},$f4);$my=[$Jw,$ly];$ny=bless({$d3,$Sx,$Q,$Tx,$q3,$my},$r4);$oy=q#ni:/io/transfer_async.c_tracker.b#;$py=q#ni:/io/transfer_async_init.b#;$qy=q#ni:/io/transfer_async_lifecycle.b#;$ry=q#ni:/io/transfer_async_ro.b#;$sy=q#ni:/io/transfer_async_run.b#;$ty=q#ni:/io/transfer_io_interop.b#;$uy=q#ni:/io/transfer_io_measurement.b#;$vy=q#ni:/io/transfer_sync#;$wy={$J3,1};$xy=q#/io/transfer_sync#;$yy={};$zy=[];$Ay=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$By=bless({$t,$zy,$v,$q,$w,$Ay,$y,$z},$A);$Cy={$U6,$By};$Dy=q#/io/transfer_sync_init.b#;$Ey=bless({$d3,$yy,$E4,$q,$F4,$q,$G4,$Cy,$Q,$Dy},$f4);$Fy={};$Gy=[];$Hy=q#my $self = shift;
my $bufsize = $$self{bufsize} || 32768;
my $buf;
my $r;
while (($r = $self->read($buf, $bufsize)) || $!{EINTR}) {
  my $n = $self->write($buf);
  $self->failure($!) unless $n || $!{EINTR};
  while ($n < $r) {
    my $n0 = $self->write($buf, $r - $n, $n);
    $self->failure($!) unless $!{EINTR} || $n0;
    $n += $n0 || 0;
  }
}
$$self{end_time} = time;
$self->success;#;$Iy=bless({$t,$Gy,$v,$q,$w,$Hy,$y,$z},$A);$Jy={$yx,$Iy};$Ky=q#/io/transfer_sync_run.b#;$Ly=bless({$d3,$Fy,$E4,$q,$F4,$q,$G4,$Jy,$Q,$Ky},$f4);$My=[$tw,$Ey,$Ly];$Ny=bless({$d3,$wy,$Q,$xy,$q3,$My},$K3);$Oy=q#ni:/io/transfer_sync.c#;$Py={$K3,1};$Qy=q#/io/transfer_sync.c#;$Ry=[$Jw];$Sy=bless({$d3,$Py,$Q,$Qy,$q3,$Ry},$r4);$Ty=q#ni:/io/transfer_sync_init.b#;$Uy=q#ni:/io/transfer_sync_run.b#;$Vy=q#ni:/lib#;$Wy=q#ni::/lib#;$Xy={$Wy,1};$Yy=[];$Zy=bless({$d3,$Xy,$Q,$x8,$q3,$Yy},$t4);$cz=q#ni:/lib/accessor.b#;$dz=q#ni:/lib/behavior#;$ez=q#ni:/lib/behavior.c#;$fz=q#ni:/lib/branch#;$gz={$N3,1};$hz=q#/lib/branch#;$iz={};$jz=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$kz=bless({$w,$jz,$y,$z},$A);$lz={$U6,$kz};$mz=q#/lib/branch_init.b#;$nz=bless({$d3,$iz,$E4,$q,$F4,$q,$G4,$lz,$Q,$mz},$f4);$oz=[$L8,$R8,$Ni,$nz,$Uj];$pz=bless({$d3,$gz,$Q,$hz,$q3,$oz},$O3);$qz=q#ni:/lib/branch.b#;$rz=q#ni:/lib/branch.c#;$sz={$O3,1};$tz=q#/lib/branch.c#;$uz=[$Bk];$vz=bless({$d3,$sz,$Q,$tz,$q3,$uz},$r4);$wz=q#ni:/lib/branch_init.b#;$xz=q#ni:/lib/class_init.b#;$yz=q#ni:/lib/dataslice#;$zz=q#ni:/lib/dataslice.b#;$Az=q#ni:/lib/dataslice.c#;$Bz={$Q3,1};$Cz=q#/lib/dataslice.c#;$Dz=[$Bk];$Ez=bless({$d3,$Bz,$Q,$Cz,$q3,$Dz},$r4);$Fz=q#ni:/lib/dataslice_init.b#;$Gz=q#ni:/lib/definition.b#;$Hz=q#ni:/lib/definition_def.b#;$Iz=q#ni:/lib/definition_defdata.b#;$Jz=q#ni:/lib/definition_init_with_defaults.b#;$Kz=q#ni:/lib/doc#;$Lz=q#ni:/lib/doc.c#;$Mz={$R3,1};$Nz=q#/lib/doc.c#;$Oz={};$Pz=q#defannotation#;$Qz=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Rz=bless({$w,$Qz,$y,$z},$A);$Sz={$Pz,$Rz};$Tz=q#/lib/doc.c_defannotation.b#;$Uz=bless({$d3,$Oz,$E4,$q,$F4,$q,$G4,$Sz,$Q,$Tz},$f4);$Vz=[$xk,$Uz];$Wz=bless({$d3,$Mz,$Q,$Nz,$q3,$Vz},$r4);$Xz=q#ni:/lib/doc.c_defannotation.b#;$Yz=q#ni:/lib/doc_TODO.b#;$Zz=q#ni:/lib/doc_define.b#;$cA=q#ni:/lib/doc_eg.b#;$dA=q#ni:/lib/doc_end.b#;$eA=q#ni:/lib/doc_init.b#;$fA=q#ni:/lib/doc_namespace.b#;$gA=q#ni:/lib/doc_process.b#;$hA=q#ni:/lib/documentable.b#;$iA=q#ni:/lib/fn#;$jA={$A,1};$kA=q#\# NB: everything here needs to happen in a single method; otherwise JIT
\# compilation will cause infinite recursion.
local ($@, $_);
my $self = shift;
$$self{proto} ||= '';

my $fn_code = qq{sub $$self{proto} {$$self{code}\\n}};

my $closure = $$self{closure};
if ($closure) {
  my $closure_code = join ';',
    map /^([@%])/ ? "my$_=$1\\{\\${\\$_[0]}{'$_'}\\}"
      : /^\\$/     ? "my$_=\\${\\$_[0]}{'$_'}"
      :             "my\\$$_=\\${\\$_[0]}{'$_'}",
    sort keys %$closure;

  my $code   = qq{sub {$closure_code; $fn_code}};
  my $fn_gen = ni::eval $code;
  die "ni:/lib/fn failed to compile $code: $@" if $@;
  return $$self{fn} = &$fn_gen($closure);
} else {
  $$self{fn} = ni::eval $fn_code;
  die "ni:/lib/fn failed to compile $fn_code: $@" if $@;
  return $$self{fn};
}#;$lA=bless({$w,$kA},$A);$mA=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  annotations => [@_]};#;$nA=bless({$w,$mA},$A);$oA=q#ni::/lib/fn::compile#;$pA=q#ni::/lib/fn::instantiate#;$qA={};$rA=q#compile#;$sA={$rA,$lA,$U6,$nA};$tA=q#/lib/fn_init.b#;$uA=bless({$d3,$qA,$E4,$q,$F4,$q,$G4,$sA,$Q,$tA},$f4);$vA={};$wA=[];$xA=q#shift->{'annotations'}#;$yA=bless({$t,$wA,$v,$q,$w,$xA,$y,$z},$A);$zA=[];$AA=q#shift->{'code'}#;$BA=bless({$t,$zA,$v,$q,$w,$AA,$y,$z},$A);$CA=q#fn#;$DA=[];$EA=q#shift->{'fn'}#;$FA=bless({$t,$DA,$v,$q,$w,$EA,$y,$z},$A);$GA={$t,$yA,$w,$BA,$CA,$FA};$HA=q#/lib/fn_ro.b#;$IA=bless({$d3,$vA,$E4,$q,$F4,$q,$G4,$GA,$Q,$HA},$f4);$JA={};$KA=[];$LA=q#if (@_ == 2) {
  $_[0]->{'closure'} = $_[1];
  return $_[0];
} else {
  return shift->{'closure'};
}#;$MA=bless({$t,$KA,$v,$q,$w,$LA,$y,$z},$A);$NA={$v,$MA};$OA=q#/lib/fn_rw.b#;$PA=bless({$d3,$JA,$E4,$q,$F4,$q,$G4,$NA,$Q,$OA},$f4);$QA={};$RA=[];$SA=q#my $self = shift; "fn {$$self{code}}"#;$TA=bless({$t,$RA,$v,$q,$w,$SA,$y,$z},$A);$UA=[];$VA=bless({$t,$UA,$v,$q,$w,$Aj,$y,$z},$A);$WA={$sj,$TA,$zj,$VA};$XA=q#/lib/fn_ops.b#;$YA=bless({$d3,$QA,$E4,$q,$F4,$q,$G4,$WA,$Q,$XA},$f4);$ZA={};$cB=[];$dB=q#local $_;
my ($self, $quote) = @_;
$quote->quote_class(ref $self);

(my $code = $$self{code}) =~ s/^\\s*\\n|\\s*$//g;
my @lines = split /\\n/, $code;
my $spaces = length $code;
for (@lines) {
  $spaces = length $1 if /^([ \\t]*)\\S/ && length $1 < $spaces;
}
$spaces = ' ' x $spaces;
s/^$spaces// for @lines;

my %state = %$self;
delete @state{qw/fn eval_number/};
$state{code} = join "\\n", @lines;
$quote->quote_blessed(\\%state, ref $self);#;$eB=bless({$t,$cB,$v,$q,$w,$dB,$y,$z},$A);$fB={$G9,$eB};$gB=q#/lib/fn_serialize.b#;$hB=bless({$d3,$ZA,$E4,$q,$F4,$q,$G4,$fB,$Q,$gB},$f4);$iB=[$R4,$fk,$uA,$IA,$PA,$YA,$hB];$jB=bless({$d3,$jA,$Q,$Hb,$q3,$iB},$S3);$kB=q#ni:/lib/fn.c#;$lB={$S3,1};$mB=q#/lib/fn.c#;$nB=[$xk];$oB=bless({$d3,$lB,$Q,$mB,$q3,$nB},$r4);$pB=q#ni:/lib/fn_init.b#;$qB=q#ni:/lib/fn_ops.b#;$rB=q#ni:/lib/fn_ro.b#;$sB=q#ni:/lib/fn_rw.b#;$tB=q#ni:/lib/fn_serialize.b#;$uB=q#ni:/lib/future#;$vB={$T3,1};$wB={};$xB=[];$yB=bless({$t,$xB,$v,$q,$w,$qv,$y,$z},$A);$zB=q#parents#;$AB=[];$BB=q#shift->{'parents'}#;$CB=bless({$t,$AB,$v,$q,$w,$BB,$y,$z},$A);$DB={$r,$yB,$zB,$CB};$EB=q#/lib/future_ro.b#;$FB=bless({$d3,$wB,$E4,$q,$F4,$q,$G4,$DB,$Q,$EB},$f4);$GB={};$HB=[];$IB=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$JB=bless({$t,$HB,$v,$q,$w,$IB,$y,$z},$A);$KB={$U6,$JB};$LB=q#/lib/future_init.b#;$MB=bless({$d3,$GB,$E4,$q,$F4,$q,$G4,$KB,$Q,$LB},$f4);$NB={};$OB=q#decide#;$PB=[];$QB=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$RB=bless({$t,$PB,$v,$q,$w,$QB,$y,$z},$A);$SB=q#decided#;$TB=[];$UB=q#shift->{outcome}#;$VB=bless({$t,$TB,$v,$q,$w,$UB,$y,$z},$A);$WB=q#listener#;$XB=[];$YB=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$ZB=bless({$t,$XB,$v,$q,$w,$YB,$y,$z},$A);$cC=q#v#;$dC=[];$eC=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$fC=bless({$t,$dC,$v,$q,$w,$eC,$y,$z},$A);$gC={$OB,$RB,$SB,$VB,$WB,$ZB,$cC,$fC};$hC=q#/lib/future_state.b#;$iC=bless({$d3,$NB,$E4,$q,$F4,$q,$G4,$gC,$Q,$hC},$f4);$jC={};$kC=q#and#;$lC=[];$mC=q#my $self   = $_[0];
my $child  = $self->class->new(grep ref, @_);
my $n      = @{$child->parents};
my $l      = 0;
my @result = map ref($_) ? undef : $_, @_;
for my $i (0..$\#_) {
  $_[$i]->listener(sub {
    $result[$i] = [@_];
    $child->decide(@result) if ++$l == $n;
  }) if ref $_[$i];
}
$child;#;$nC=bless({$t,$lC,$v,$q,$w,$mC,$y,$z},$A);$oC=q#flatmap#;$pC=[];$qC=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$rC=bless({$t,$pC,$v,$q,$w,$qC,$y,$z},$A);$sC=q#map#;$tC=[];$uC=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$vC=bless({$t,$tC,$v,$q,$w,$uC,$y,$z},$A);$wC=q#or#;$xC=[];$yC=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$zC=bless({$t,$xC,$v,$q,$w,$yC,$y,$z},$A);$AC={$kC,$nC,$oC,$rC,$sC,$vC,$wC,$zC};$BC=q#/lib/future_algebra.b#;$CC=bless({$d3,$jC,$E4,$q,$F4,$q,$G4,$AC,$Q,$BC},$f4);$DC=[$R4,$FB,$MB,$iC,$CC];$EC=bless({$d3,$vB,$Q,$Yb,$q3,$DC},$U3);$FC=q#ni:/lib/future.c#;$GC={$U3,1};$HC=q#/lib/future.c#;$IC=[$xk];$JC=bless({$d3,$GC,$Q,$HC,$q3,$IC},$r4);$KC=q#ni:/lib/future_algebra.b#;$LC=q#ni:/lib/future_init.b#;$MC=q#ni:/lib/future_ro.b#;$NC=q#ni:/lib/future_state.b#;$OC=q#ni:/lib/gensym_generator_compact.b#;$PC=q#ni:/lib/global_static_test.b#;$QC={};$RC=[];$SC=q#ni('ni:/lib/test_case')->new(shift)#;$TC=bless({$t,$RC,$v,$q,$w,$SC,$y,$Nh},$A);$UC=q#now#;$VC=[];$WC=q#ni('ni:/lib/test_value')->new(shift)#;$XC=bless({$t,$VC,$v,$q,$w,$WC,$y,$Nh},$A);$YC={$A6,$TC,$UC,$XC};$ZC=q#/lib/global_static_test.b#;$cD=bless({$d3,$QC,$E4,$q,$F4,$q,$G4,$YC,$Q,$ZC},$f4);$dD=q#ni:/lib/image#;$eD=q#ni:/lib/image.c#;$fD={$W3,1};$gD=q#/lib/image.c#;$hD=[$xk];$iD=bless({$d3,$fD,$Q,$gD,$q3,$hD},$r4);$jD=q#ni:/lib/image_init.b#;$kD=q#ni:/lib/image_quoting.b#;$lD=q#ni:/lib/instance.b#;$mD=q#ni:/lib/instantiable.b#;$nD=q#ni:/lib/json.b#;$oD=q#ni:/lib/json_data.b#;$pD=q#ni:/lib/name_as_string.b#;$qD=q#ni:/lib/named.b#;$rD=q#ni:/lib/named_in_ni.b#;$sD=q#ni:/lib/namespaced.b#;$tD=q#ni:/lib/ni#;$uD={$X3,1};$vD={};$wD=q#extend#;$xD=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$yD=bless({$w,$xD,$y,$z},$A);$zD=q#is_mutable#;$AD=q#$0 ne '-' && -w $0#;$BD=bless({$w,$AD,$y,$z},$A);$CD=q#modify#;$DD=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$ED=bless({$w,$DD,$y,$z},$A);$FD={$wD,$yD,$zD,$BD,$CD,$ED};$GD=q#/lib/ni_self.b#;$HD=bless({$d3,$vD,$E4,$q,$F4,$q,$G4,$FD,$Q,$GD},$f4);$ID={};$JD=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$KD=bless({$w,$JD,$y,$z},$A);$LD=q#docs#;$MD=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$ND=bless({$w,$MD,$y,$z},$A);$OD=q#metaclasses#;$PD=q#my $self = shift;
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$QD=bless({$w,$PD,$y,$z},$A);$RD=q#undocumented#;$SD=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$TD=bless({$w,$SD,$y,$z},$A);$UD=q#untested#;$VD=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$WD=bless({$w,$VD,$y,$z},$A);$XD={$K,$KD,$LD,$ND,$OD,$QD,$RD,$TD,$UD,$WD};$YD=q#/lib/ni_dev_introspection.b#;$ZD=bless({$d3,$ID,$E4,$q,$F4,$q,$G4,$XD,$Q,$YD},$f4);$cE={};$dE=q#--internal/+=#;$eE=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$fE=bless({$w,$eE,$y,$z},$A);$gE=q#--internal/dev-state#;$hE=q#my $self = shift;
my @tests = map $_->tests, $self->docs;
$_->run for @tests;
my @failed = grep $_->failed, @tests;

print ni::json_encode_pretty({
  image_size     => length($self->quoted->io->read_all),
  named_entities => scalar(keys %{$$self{named}}),
  classes        => scalar($self->classes),
  metaclasses    => scalar($self->metaclasses),

  undocumented => [map "$_", $self->undocumented],
  untested     => [map "$_", $self->untested],

  tests => {
    total   => scalar(@tests),
    passing => @tests - @failed,
    failed  => (@failed ? [
      map +{
        referent => $_->referent->name,
        assertions => [
          map $_->failed ? +{diff => $_->diff} : +{passed => 1},
              @{$_->assertions}
        ]
      }, @failed
    ] : undef)
  },

  todos => [map +{referent => $_->referent->name,
                  text     => join "\\n", @{$_->todo}},
            map $_->todos, $self->docs],
}), "\\n";
0;#;$iE=bless({$w,$hE,$y,$z},$A);$jE=q#--internal/eval#;$kE=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$lE=bless({$w,$kE,$y,$z},$A);$mE=q#--internal/image#;$nE=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$oE=bless({$w,$nE,$y,$z},$A);$pE=q#--internal/test#;$qE=q#local $| = 1;
my $self   = shift;
my $failed = 0;
my @docs   = $self->docs;
my @todos  = map $_->todos, @docs;

print "\\n" . scalar(@todos) . " TODO item(s)\\n" if @todos;
print "\\n$_\\n" for @todos;

my @tests = map $_->tests, @docs;
$_->run for @tests;

for my $f (grep $_->failed, @tests) {
  ++$failed;
  print "\\nTEST FAIL\\n", $f->test, "\\n";
  print "\\nERROR ", ni('ni:/lib/fn')->resolve_evals($f->error)
    if $f->error;
  print "\\nASSERTIONS\\n";
  print "  $_\\n" for @{$f->assertions};
}

print "\\nSUMMARY\\n";
printf "% 4d test(s) passed\\n", @tests - $failed;
printf "% 4d test(s) failed\\n", $failed;
!!$failed;#;$rE=bless({$w,$qE,$y,$z},$A);$sE=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$tE=bless({$w,$sE,$y,$z},$A);$uE={$dE,$fE,$gE,$iE,$jE,$lE,$mE,$oE,$pE,$rE,$yx,$tE};$vE=q#/lib/ni_main.b#;$wE=bless({$d3,$cE,$E4,$q,$F4,$q,$G4,$uE,$Q,$vE},$f4);$xE={};$yE=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$zE=bless({$w,$yE,$y,$z},$A);$AE=q#resolver_for#;$BE=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$CE=bless({$w,$BE,$y,$z},$A);$DE={$z9,$zE,$AE,$CE};$EE=q#/lib/ni_resolver.b#;$FE=bless({$d3,$xE,$E4,$q,$F4,$q,$G4,$DE,$Q,$EE},$f4);$GE={};$HE=q#exists#;$IE=q#exists $_[0]->{named}{$_[1]}#;$JE=bless({$w,$IE,$y,$z},$A);$KE=q#quoted#;$LE=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$ME=bless({$w,$LE,$y,$z},$A);$NE={$HE,$JE,$KE,$ME};$OE=q#/lib/ni_image.b#;$PE=bless({$d3,$GE,$E4,$q,$F4,$q,$G4,$NE,$Q,$OE},$f4);$QE=[$R4,$HD,$ZD,$wE,$FE,$PE];$RE=bless({$d3,$uD,$Q,$Ye,$q3,$QE},$Y3);$SE=q#ni:/lib/ni.c#;$TE={$Y3,1};$UE=q#/lib/ni.c#;$VE=[$xk];$WE=bless({$d3,$TE,$Q,$UE,$q3,$VE},$r4);$XE=q#ni:/lib/ni_dev_introspection.b#;$YE=q#ni:/lib/ni_image.b#;$ZE=q#ni:/lib/ni_main.b#;$cF=q#ni:/lib/ni_resolver.b#;$dF=q#ni:/lib/ni_self.b#;$eF=q#ni:/lib/ni_static_util.b#;$fF=q#ni:/lib/object_metadata#;$gF={$Z3,1,$C,1,$H,1};$hF=q#/lib/object_metadata#;$iF={};$jF=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$kF=bless({$w,$jF,$y,$z},$A);$lF={$c3,$kF};$mF=q#/lib/object_metadata_rw.b#;$nF=bless({$d3,$iF,$E4,$q,$F4,$q,$G4,$lF,$Q,$mF},$f4);$oF=[$R4,$nF];$pF=bless({$d3,$gF,$Q,$hF,$q3,$oF},$c4);$qF=q#ni:/lib/object_metadata.c#;$rF={$c4,1,$n4,1,$q4,1};$sF=q#/lib/object_metadata.c#;$tF=[$xk];$uF=bless({$d3,$rF,$Q,$sF,$q3,$tF},$r4);$vF=q#ni:/lib/object_metadata_rw.b#;$wF=q#ni:/lib/perlbranch.b#;$xF=q#ni:/lib/quote_circular_addressed.b#;$yF=q#ni:/lib/quote_code_fail.b#;$zF=q#ni:/lib/quote_gensym_identity.b#;$AF=q#ni:/lib/quote_objects.b#;$BF=q#ni:/lib/quote_simple#;$CF={$d4,1};$DF={};$EF=[];$FF=q#+{}#;$GF=bless({$t,$EF,$v,$q,$w,$FF,$y,$z},$A);$HF={$U6,$GF};$IF=q#/lib/quote_simple_init.b#;$JF=bless({$d3,$DF,$E4,$q,$F4,$q,$G4,$HF,$Q,$IF},$f4);$KF={};$LF=[];$MF=bless({$t,$LF,$v,$q,$w,0,$y,$z},$A);$NF=[];$OF=q#shift->quote_value(shift)#;$PF=bless({$t,$NF,$v,$q,$w,$OF,$y,$z},$A);$QF={$Vd,$MF,$re,$PF};$RF=q#/lib/quote_simple_quote.b#;$SF=bless({$d3,$KF,$E4,$q,$F4,$q,$G4,$QF,$Q,$RF},$f4);$TF=[$R4,$JF,$SF,$Vc,$vd,$Ld];$UF=bless({$d3,$CF,$Q,$lf,$q3,$TF},$e4);$VF=q#ni:/lib/quote_simple.c#;$WF={$e4,1};$XF=q#/lib/quote_simple.c#;$YF=[$xk];$ZF=bless({$d3,$WF,$Q,$XF,$q3,$YF},$r4);$cG=q#ni:/lib/quote_simple_init.b#;$dG=q#ni:/lib/quote_simple_quote.b#;$eG=q#ni:/lib/quote_values.b#;$fG=q#ni:/lib/ref_eq.b#;$gG=q#ni:/lib/resolver.b#;$hG=q#ni:/lib/slice#;$iG=q#ni:/lib/slice.b#;$jG=q#ni:/lib/slice.c#;$kG={$g4,1};$lG=q#/lib/slice.c#;$mG=[$Bk];$nG=bless({$d3,$kG,$Q,$lG,$q3,$mG},$r4);$oG=q#ni:/lib/slice_init.b#;$pG=q#ni:/lib/slice_serialize.b#;$qG=q#ni:/lib/static_fn.b#;$rG={};$sG=q#fc#;$tG=[];$uG=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$vG=bless({$t,$tG,$v,$q,$w,$uG,$y,$z},$A);$wG=q#fk#;$xG=[];$yG=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$zG=bless({$t,$xG,$v,$q,$w,$yG,$y,$Nh},$A);$AG=[];$BG=q#ni('ni:/lib/fn')->new(@_)#;$CG=bless({$t,$AG,$v,$q,$w,$BG,$y,$Nh},$A);$DG=q#fp#;$EG=[];$FG=q#($$)#;$GG=bless({$t,$EG,$v,$q,$w,$BG,$y,$FG},$A);$HG={$sG,$vG,$wG,$zG,$CA,$CG,$DG,$GG};$IG=q#/lib/static_fn.b#;$JG=bless({$d3,$rG,$E4,$q,$F4,$q,$G4,$HG,$Q,$IG},$f4);$KG=q#ni:/lib/subclass.b#;$LG=q#ni:/lib/tag#;$MG={$h4,1};$NG=q#/lib/tag#;$OG={};$PG=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$QG=bless({$w,$PG,$y,$z},$A);$RG={$T8,$QG};$SG=q#/lib/tag.b#;$TG=bless({$d3,$OG,$E4,$q,$F4,$q,$G4,$RG,$Q,$SG},$f4);$UG={};$VG=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$WG=bless({$w,$VG,$y,$z},$A);$XG={$U6,$WG};$YG=q#/lib/tag_init.b#;$ZG=bless({$d3,$UG,$E4,$q,$F4,$q,$G4,$XG,$Q,$YG},$f4);$cH=[$L8,$R8,$TG,$ZG];$dH=bless({$d3,$MG,$Q,$NG,$q3,$cH},$i4);$eH=q#ni:/lib/tag.b#;$fH=q#ni:/lib/tag.c#;$gH={$i4,1};$hH=q#/lib/tag.c#;$iH=[$Bk];$jH=bless({$d3,$gH,$Q,$hH,$q3,$iH},$r4);$kH=q#ni:/lib/tag_init.b#;$lH=q#ni:/lib/test_assert_eq#;$mH={$j4,1};$nH=q#/lib/test_assert_eq#;$oH={$j4,1,$l4,1};$pH=q#/lib/test_assertion#;$qH={};$rH=q#commit#;$sH=[];$tH=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$uH=bless({$t,$sH,$v,$q,$w,$tH,$y,$z},$A);$vH={$rH,$uH};$wH=q#/lib/test_assertion_commit.b#;$xH=bless({$d3,$qH,$E4,$q,$F4,$q,$G4,$vH,$Q,$wH},$f4);$yH=[$R4,$xH];$zH=bless({$d3,$oH,$Q,$pH,$q3,$yH},$m4);$AH={};$BH=q#diff#;$CH=[];$DH=q#shift->{'diff'}#;$EH=bless({$t,$CH,$v,$q,$w,$DH,$y,$z},$A);$FH={$BH,$EH};$GH=q#/lib/test_assert_eq_ro.b#;$HH=bless({$d3,$AH,$E4,$q,$F4,$q,$G4,$FH,$Q,$GH},$f4);$IH={};$JH=[];$KH=q#my ($class, $diff) = @_;
+{diff => $diff};#;$LH=bless({$t,$JH,$v,$q,$w,$KH,$y,$z},$A);$MH={$U6,$LH};$NH=q#/lib/test_assert_eq_init.b#;$OH=bless({$d3,$IH,$E4,$q,$F4,$q,$G4,$MH,$Q,$NH},$f4);$PH={};$QH=[];$RH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$SH=bless({$t,$QH,$v,$q,$w,$RH,$y,$z},$A);$TH=q#failed#;$UH=[];$VH=q#defined shift->{diff}#;$WH=bless({$t,$UH,$v,$q,$w,$VH,$y,$z},$A);$XH=q#result#;$YH=[];$ZH=bless({$t,$YH,$v,$q,$w,$Ax,$y,$z},$A);$cI={$sj,$SH,$TH,$WH,$XH,$ZH};$dI=q#/lib/test_assert_eq_result.b#;$eI=bless({$d3,$PH,$E4,$q,$F4,$q,$G4,$cI,$Q,$dI},$f4);$fI=[$zH,$HH,$OH,$eI];$gI=bless({$d3,$mH,$Q,$nH,$q3,$fI},$k4);$hI=q#ni:/lib/test_assert_eq.c#;$iI={$k4,1};$jI=q#/lib/test_assert_eq.c#;$kI={$k4,1,$m4,1};$lI=q#/lib/test_assertion.c#;$mI=[$xk];$nI=bless({$d3,$kI,$Q,$lI,$q3,$mI},$r4);$oI=[$nI];$pI=bless({$d3,$iI,$Q,$jI,$q3,$oI},$r4);$qI=q#ni:/lib/test_assert_eq_init.b#;$rI=q#ni:/lib/test_assert_eq_result.b#;$sI=q#ni:/lib/test_assert_eq_ro.b#;$tI=q#ni:/lib/test_assertion#;$uI=q#ni:/lib/test_assertion.c#;$vI=q#ni:/lib/test_assertion_commit.b#;$wI=q#ni:/lib/test_case#;$xI={$C,1};$yI=q#/lib/test_case#;$zI=q#running_test#;$AI={};$BI=[];$CI=q#shift->{'assertions'}#;$DI=bless({$t,$BI,$v,$q,$w,$CI,$y,$z},$A);$EI=[];$FI=q#shift->{'test'}#;$GI=bless({$t,$EI,$v,$q,$w,$FI,$y,$z},$A);$HI={$n,$DI,$s,$GI};$II=q#/lib/test_case_ro.b#;$JI=bless({$d3,$AI,$E4,$q,$F4,$q,$G4,$HI,$Q,$II},$f4);$KI={};$LI=[];$MI=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$NI=bless({$t,$LI,$v,$q,$w,$MI,$y,$z},$A);$OI={$p,$NI};$PI=q#/lib/test_case_rw.b#;$QI=bless({$d3,$KI,$E4,$q,$F4,$q,$G4,$OI,$Q,$PI},$f4);$RI={};$SI=[];$TI=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$UI=bless({$t,$SI,$v,$q,$w,$TI,$y,$z},$A);$VI={$U6,$UI};$WI=q#/lib/test_case_init.b#;$XI=bless({$d3,$RI,$E4,$q,$F4,$q,$G4,$VI,$Q,$WI},$f4);$YI={};$ZI=[];$cJ=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$dJ=bless({$t,$ZI,$v,$q,$w,$cJ,$y,$z},$A);$eJ=[];$fJ=q#!shift->{outcome}->[0]#;$gJ=bless({$t,$eJ,$v,$q,$w,$fJ,$y,$z},$A);$hJ={$sj,$dJ,$TH,$gJ};$iJ=q#/lib/test_case_metrics.b#;$jJ=bless({$d3,$YI,$E4,$q,$F4,$q,$G4,$hJ,$Q,$iJ},$f4);$kJ={};$lJ=q#done#;$mJ=[];$nJ=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$oJ=bless({$t,$mJ,$v,$q,$w,$nJ,$y,$z},$A);$pJ=[];$qJ=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$rJ=bless({$t,$pJ,$v,$q,$w,$qJ,$y,$z},$A);$sJ={$lJ,$oJ,$yx,$rJ};$tJ=q#/lib/test_case_run.b#;$uJ=bless({$d3,$kJ,$E4,$q,$F4,$q,$G4,$sJ,$Q,$tJ},$f4);$vJ=[$pF,$JI,$QI,$XI,$jJ,$uJ];$wJ=bless({$d3,$xI,$Q,$yI,$zI,$q,$q3,$vJ},$n4);$xJ=[];$yJ=q#shift->{running_test} = undef#;$zJ=bless({$t,$xJ,$v,$q,$w,$yJ,$y,$z},$A);$AJ=q#ni:/lib/test_case.c#;$BJ={$n4,1};$CJ=q#/lib/test_case.c#;$DJ={};$EJ=[];$FJ=q#shift->{'running_test'}#;$GJ=bless({$t,$EJ,$v,$q,$w,$FJ,$y,$z},$A);$HJ={$zI,$GJ};$IJ=q#/lib/test_case.c_test_ro.b#;$JJ=bless({$d3,$DJ,$E4,$q,$F4,$q,$G4,$HJ,$Q,$IJ},$f4);$KJ={};$LJ=q#with_test#;$MJ=[];$NJ=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$OJ=bless({$t,$MJ,$v,$q,$w,$NJ,$y,$z},$A);$PJ={$LJ,$OJ};$QJ=q#/lib/test_case.c_test.b#;$RJ=bless({$d3,$KJ,$E4,$zJ,$F4,$q,$G4,$PJ,$Q,$QJ},$f4);$SJ=[$uF,$JJ,$RJ];$TJ=bless({$d3,$BJ,$Q,$CJ,$q3,$SJ},$r4);$UJ=q#ni:/lib/test_case.c_test.b#;$VJ=q#ni:/lib/test_case.c_test_ro.b#;$WJ=q#ni:/lib/test_case_init.b#;$XJ=q#ni:/lib/test_case_metrics.b#;$YJ=q#ni:/lib/test_case_ro.b#;$ZJ=q#ni:/lib/test_case_run.b#;$cK=q#ni:/lib/test_case_rw.b#;$dK=q#ni:/lib/test_value#;$eK={$o4,1};$fK=q#/lib/test_value#;$gK={};$hK=[];$iK=q#\\$_[1]#;$jK=bless({$t,$hK,$v,$q,$w,$iK,$y,$z},$A);$kK={$U6,$jK};$lK=q#/lib/test_value_init.b#;$mK=bless({$d3,$gK,$E4,$q,$F4,$q,$G4,$kK,$Q,$lK},$f4);$nK={};$oK=q#(==#;$pK=[];$qK=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$rK=bless({$t,$pK,$v,$q,$w,$qK,$y,$z},$A);$sK=q#detailed_scalar_diff#;$tK=[];$uK=q#local $_;
my ($self, $lhs, $rhs) = @_;
my $lpos = 0;
my $rpos = 0;
my @diff;
while ($lpos < length $lhs || $rpos < length $rhs) {
  my $found = index $rhs, substr($lhs, $lpos, 16), $rpos;
  if ($found == -1) {
    if (@diff && $diff[-1]{delete}) {
      $diff[-1]{delete} .= substr($lhs, $lpos, 16);
    } else {
      push @diff, {delete => substr($lhs, $lpos, 16)};
    }
    $lpos += 16;
  } else {
    push @diff, {insert => substr($rhs, $rpos, $found - $rpos)}
      unless $found == $rpos;
    $lpos += 16;
    $rpos = $found + 16;
  }
}
[@diff];#;$vK=bless({$t,$tK,$v,$q,$w,$uK,$y,$z},$A);$wK=[];$xK=q#my ($self, $rhs) = @_;
my $class = $self->class;
my $lhs = $$self;
my $rl = ref $lhs;
my $rr = ref $rhs;
my $realtype = Scalar::Util::reftype($lhs) || "";
return {type_difference => [$rl, $rr]} unless $rl eq $rr;
if ($realtype eq 'HASH') {
  my @left_only  = grep !exists $$rhs{$_}, keys %$lhs;
  my @right_only = grep !exists $$lhs{$_}, keys %$rhs;
  return {hash_key_mismatch => 1,
          object_type       => $rl,
          left_only         => \\@left_only,
          right_only        => \\@right_only}
    if @left_only || @right_only;
  my %diff;
  $diff{$_} = $class->new($$lhs{$_})->diff($$rhs{$_})
    for keys %$lhs;
  delete @diff{grep !defined($diff{$_}), keys %diff};
  return {hash_value_mismatch => 1,
          object_type         => $rl,
          diffs               => \\%diff} if keys %diff;
} elsif ($realtype eq 'ARRAY') {
  return {array_length_mismatch => [scalar(@$lhs), scalar(@$rhs)]}
    unless @$lhs == @$rhs;
  my %diff;
  $diff{$_} = $class->new($$lhs[$_])->diff($$rhs[$_])
    for 0..$\#{$lhs};
  delete @diff{grep !defined($diff{$_}), keys %diff};
  return {array_value_mismatch => 1,
          object_type          => $rl,
          diffs                => \\%diff} if keys %diff;
} elsif ($realtype eq 'SCALAR') {
  return $class->new($$lhs)->diff($$rhs);
} elsif (!$rl) {
  no warnings 'uninitialized';
  return undef if $lhs eq $rhs;
  return {scalar_difference => $self->detailed_scalar_diff($lhs, $rhs)}
    if length($lhs) + length($rhs) > 80;
  return {scalar_difference => [$lhs, $rhs]};
}
return undef;#;$yK=bless({$t,$wK,$v,$q,$w,$xK,$y,$z},$A);$zK={$oK,$rK,$sK,$vK,$BH,$yK};$AK=q#/lib/test_value_eq.b#;$BK=bless({$d3,$nK,$E4,$q,$F4,$q,$G4,$zK,$Q,$AK},$f4);$CK={};$DK=[];$EK=q#ni::json_encode ${$_[0]}#;$FK=bless({$t,$DK,$v,$q,$w,$EK,$y,$z},$A);$GK={$sj,$FK};$HK=q#/lib/test_value_str.b#;$IK=bless({$d3,$CK,$E4,$q,$F4,$q,$G4,$GK,$Q,$HK},$f4);$JK=[$R4,$mK,$BK,$IK];$KK=bless({$d3,$eK,$Q,$fK,$q3,$JK},$p4);$LK=q#ni:/lib/test_value.c#;$MK={$p4,1};$NK=q#/lib/test_value.c#;$OK=[$xk];$PK=bless({$d3,$MK,$Q,$NK,$q3,$OK},$r4);$QK=q#ni:/lib/test_value_eq.b#;$RK=q#ni:/lib/test_value_init.b#;$SK=q#ni:/lib/test_value_str.b#;$TK=q#ni:/lib/todo#;$UK={$H,1};$VK=q#/lib/todo#;$WK={};$XK=q#shift->{'todo'}#;$YK=bless({$w,$XK,$y,$z},$A);$ZK={$E,$YK};$cL=q#/lib/todo_ro.b#;$dL=bless({$d3,$WK,$E4,$q,$F4,$q,$G4,$ZK,$Q,$cL},$f4);$eL={};$fL=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$gL=bless({$w,$fL,$y,$z},$A);$hL={$U6,$gL};$iL=q#/lib/todo_init.b#;$jL=bless({$d3,$eL,$E4,$q,$F4,$q,$G4,$hL,$Q,$iL},$f4);$kL={};$lL=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$mL=bless({$w,$lL,$y,$z},$A);$nL={$sj,$mL};$oL=q#/lib/todo_str.b#;$pL=bless({$d3,$kL,$E4,$q,$F4,$q,$G4,$nL,$Q,$oL},$f4);$qL=[$pF,$dL,$jL,$pL];$rL=bless({$d3,$UK,$Q,$VK,$q3,$qL},$q4);$sL=q#ni:/lib/todo.c#;$tL={$q4,1};$uL=q#/lib/todo.c#;$vL=[$uF];$wL=bless({$d3,$tL,$Q,$uL,$q3,$vL},$r4);$xL=q#ni:/lib/todo_ctor.b#;$yL={};$zL=q#ni('ni:/lib/todo')->new(@_)#;$AL=bless({$w,$zL,$y,$z},$A);$BL={$Z2,$AL};$CL=q#/lib/todo_ctor.b#;$DL=bless({$d3,$yL,$E4,$q,$F4,$q,$G4,$BL,$Q,$CL},$f4);$EL=q#ni:/lib/todo_init.b#;$FL=q#ni:/lib/todo_ro.b#;$GL=q#ni:/lib/todo_str.b#;$HL=q#ni:/metaclass#;$IL={$r4,1};$JL=q#/metaclass#;$KL=[$Pi,$fk,$Vi,$Wj];$LL=bless({$d3,$IL,$Q,$JL,$q3,$KL},$s4);$ML=q#ni:/metaclass.c#;$NL={$s4,1};$OL=q#/metaclass.c#;$PL=[$ok];$QL=bless({$d3,$NL,$Q,$OL,$q3,$PL},$r4);$RL=q#ni:/module#;$SL=q#ni:/module.c#;$TL=q#ni:/object#;$UL=q#ni:/object.c#;$VL=q#ni:/semantic#;$WL=q#ni::/semantic#;$XL={$WL,1};$YL=[];$ZL=bless({$d3,$XL,$Q,$pg,$q3,$YL},$t4);$cM=q#ni:/semantic/dimension#;$dM={$x4,1};$eM=q#/semantic/dimension#;$fM=[$ok];$gM=bless({$d3,$dM,$Q,$eM,$q3,$fM},$y4);$hM=q#ni:/semantic/dimension.c#;$iM={$y4,1};$jM=q#/semantic/dimension.c#;$kM=[$Fk];$lM=bless({$d3,$iM,$Q,$jM,$q3,$kM},$r4);$mM=q#ni:/semantic/task#;$nM=q#ni:/semantic/task.c#;$oM=q#ni:/semantic/task_outcome.b#;$pM=q#ni:/semantic/task_ro.b#;$qM=q#ni:main#;$rM={$it,1};$sM=[$DL,$JG,$cD,$ht];$tM=bless({$d3,$rM,$Q,$it,$q3,$sM},$t4);$uM=q#ni:ni#;$vM={$d,$T,$W,$f1,$g1,$n1,$o1,$t1,$u1,$G1,$H1,$T1,$U1,$i2,$j2,$v2,$w2,$R2,$S2,$X2,$Y2,$y6,$z6,$r8,$s8,$y8,$z8,$U9,$V9,$Ab,$Bb,$Ib,$Jb,$Zb,$cc,$Re,$Se,$Ze,$cf,$mf,$nf,$kg,$lg,$qg,$rg,$zi,$Ai,$ok,$pk,$Fk,$Gk,$Kk,$Lk,$Ok,$Pk,$Tk,$Uk,$Yk,$Zk,$Dl,$El,$Ul,$Vl,$jl,$Wl,$Bl,$Xl,$qm,$rm,$vm,$wm,$hm,$xm,$om,$ym,$go,$ho,$lo,$mo,$Mn,$no,$eo,$oo,$Om,$po,$En,$qo,$kn,$ro,$Hm,$so,$Kp,$Op,$oq,$pq,$mq,$qq,$ip,$rq,$tp,$sq,$Io,$tq,$Hp,$uq,$Bo,$vq,$Qo,$wq,$Pr,$Qr,$Ur,$Vr,$Mq,$Wr,$lr,$Xr,$Tq,$Yr,$Nr,$Zr,$Eq,$cs,$tr,$ds,$ws,$xs,$Bs,$Cs,$us,$Ds,$ls,$Es,$ht,$jt,$Ht,$It,$Mt,$Nt,$st,$Ot,$Ft,$Pt,$s6,$Qt,$Sl,$Rt,$Ql,$St,$w5,$Tt,$E5,$Ut,$Q5,$Vt,$c5,$Wt,$q6,$Xt,$e6,$Yt,$W7,$Zt,$fu,$gu,$U7,$hu,$c7,$iu,$A7,$ju,$P6,$ku,$o7,$lu,$Yu,$Zu,$fv,$gv,$Iu,$hv,$Wu,$iv,$Bu,$jv,$tw,$xw,$Jw,$Kw,$Hw,$Lw,$Nx,$Rx,$ny,$oy,$ly,$py,$mx,$qy,$wx,$ry,$fx,$sy,$Ix,$ty,$Vv,$uy,$rw,$vy,$Ny,$Oy,$Sy,$Ty,$Ey,$Uy,$Ly,$Vy,$Zy,$cz,$qj,$dz,$L8,$ez,$Bk,$fz,$pz,$qz,$Ni,$rz,$vz,$wz,$nz,$xz,$Vi,$yz,$O9,$zz,$d9,$Az,$Ez,$Fz,$j9,$Gz,$Uj,$Hz,$gj,$Iz,$Lj,$Jz,$Sj,$Kz,$gb,$Lz,$Wz,$Xz,$Uz,$Yz,$Ma,$Zz,$xa,$cA,$Sa,$dA,$Ga,$eA,$ha,$fA,$na,$gA,$eb,$hA,$J8,$iA,$jB,$kB,$oB,$pB,$uA,$qB,$YA,$rB,$IA,$sB,$PA,$tB,$hB,$uB,$EC,$FC,$JC,$KC,$CC,$LC,$MB,$MC,$FB,$NC,$iC,$OC,$Fe,$PC,$cD,$dD,$He,$eD,$iD,$jD,$pc,$kD,$Nc,$lD,$P4,$mD,$fk,$nD,$ni,$oD,$Ih,$pD,$xj,$qD,$R8,$rD,$q9,$sD,$x9,$tD,$RE,$SE,$WE,$XE,$ZD,$YE,$PE,$ZE,$wE,$cF,$FE,$dF,$HD,$eF,$mh,$fF,$pF,$qF,$uF,$vF,$nF,$wF,$Pi,$xF,$de,$yF,$Vc,$zF,$xe,$AF,$Ld,$BF,$UF,$VF,$ZF,$cG,$JF,$dG,$SF,$eG,$vd,$fG,$Ej,$gG,$E9,$hG,$Jf,$iG,$Bf,$jG,$nG,$oG,$Hf,$pG,$M9,$qG,$JG,$KG,$mk,$LG,$dH,$eH,$TG,$fH,$jH,$kH,$ZG,$lH,$gI,$hI,$pI,$qI,$OH,$rI,$eI,$sI,$HH,$tI,$zH,$uI,$nI,$vI,$xH,$wI,$wJ,$AJ,$TJ,$UJ,$RJ,$VJ,$JJ,$WJ,$XI,$XJ,$jJ,$YJ,$JI,$ZJ,$uJ,$cK,$QI,$dK,$KK,$LK,$PK,$QK,$BK,$RK,$mK,$SK,$IK,$TK,$rL,$sL,$wL,$xL,$DL,$EL,$jL,$FL,$dL,$GL,$pL,$HL,$LL,$ML,$QL,$RL,$Wj,$SL,$Dk,$TL,$R4,$UL,$xk,$VL,$ZL,$cM,$gM,$hM,$lM,$mM,$Iv,$nM,$Dw,$oM,$Gv,$pM,$uv,$qM,$tM,$uM,$pi};$wM=q#resolvers#;$xM=[];$yM=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$zM=bless({$t,$xM,$v,$q,$w,$yM,$y,$z},$A);$AM=q#file#;$BM=[];$CM=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$DM=bless({$t,$BM,$v,$q,$w,$CM,$y,$z},$A);$EM=q#null#;$FM=[];$GM=q#ni('ni:/io/null')->new#;$HM=bless({$t,$FM,$v,$q,$w,$GM,$y,$z},$A);$IM=q#sh#;$JM=[];$KM=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$LM=bless({$t,$JM,$v,$q,$w,$KM,$y,$z},$A);$MM=q#str#;$NM=[];$OM=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$PM=bless({$t,$NM,$v,$q,$w,$OM,$y,$z},$A);$QM={$C7,$zM,$AM,$DM,$EM,$HM,$IM,$LM,$MM,$PM};$RM=bless({$c,$vM,$wM,$QM},$X3);*$pA=\&$nA;*$oA=\&$lA;*$xf=\&$vf;*$wf=\&$tf;$P4->apply_($r3);$P4->apply_($s3);$P4->apply_($t3);$P4->apply_($u3);$P4->apply_($e3);$P4->apply_($v3);$P4->apply_($f3);$P4->apply_($w3);$P4->apply_($g3);$P4->apply_($x3);$P4->apply_($h3);$P4->apply_($y3);$P4->apply_($i3);$P4->apply_($z3);$P4->apply_($j3);$P4->apply_($A3);$P4->apply_($k3);$P4->apply_($B3);$P4->apply_($l3);$P4->apply_($C3);$P4->apply_($m3);$P4->apply_($D3);$P4->apply_($n3);$P4->apply_($E3);$P4->apply_($F3);$P4->apply_($G3);$P4->apply_($H3);$P4->apply_($I3);$P4->apply_($J3);$P4->apply_($K3);$P4->apply_($L3);$P4->apply_($M3);$P4->apply_($N3);$P4->apply_($O3);$P4->apply_($P3);$P4->apply_($Q3);$P4->apply_($S);$P4->apply_($R3);$P4->apply_($A);$P4->apply_($S3);$P4->apply_($T3);$P4->apply_($U3);$P4->apply_($V3);$P4->apply_($W3);$P4->apply_($X3);$P4->apply_($Y3);$P4->apply_($Z3);$P4->apply_($c4);$P4->apply_($d4);$P4->apply_($e4);$P4->apply_($f4);$P4->apply_($g4);$P4->apply_($h4);$P4->apply_($i4);$P4->apply_($j4);$P4->apply_($k4);$P4->apply_($l4);$P4->apply_($m4);$P4->apply_($C);$P4->apply_($n4);$P4->apply_($o4);$P4->apply_($p4);$P4->apply_($H);$P4->apply_($q4);$P4->apply_($r4);$P4->apply_($s4);$P4->apply_($t4);$P4->apply_($u4);$P4->apply_($v4);$P4->apply_($w4);$P4->apply_($x4);$P4->apply_($y4);$P4->apply_($z4);$P4->apply_($A4);$c5->apply_($e3);$c5->apply_($f3);$c5->apply_($g3);$c5->apply_($h3);$c5->apply_($i3);$c5->apply_($j3);$c5->apply_($k3);$c5->apply_($l3);$c5->apply_($m3);$c5->apply_($n3);$w5->apply_($e3);$w5->apply_($f3);$w5->apply_($g3);$w5->apply_($h3);$w5->apply_($i3);$w5->apply_($j3);$w5->apply_($k3);$w5->apply_($l3);$w5->apply_($m3);$w5->apply_($n3);$E5->apply_($e3);$E5->apply_($f3);$E5->apply_($g3);$E5->apply_($h3);$E5->apply_($i3);$E5->apply_($j3);$E5->apply_($k3);$E5->apply_($l3);$E5->apply_($m3);$E5->apply_($n3);$Q5->apply_($e3);$Q5->apply_($f3);$Q5->apply_($g3);$Q5->apply_($h3);$Q5->apply_($i3);$Q5->apply_($j3);$Q5->apply_($k3);$Q5->apply_($l3);$Q5->apply_($m3);$Q5->apply_($n3);$e6->apply_($e3);$e6->apply_($f3);$e6->apply_($g3);$e6->apply_($h3);$e6->apply_($i3);$e6->apply_($j3);$e6->apply_($k3);$e6->apply_($l3);$e6->apply_($m3);$e6->apply_($n3);$q6->apply_($e3);$q6->apply_($f3);$q6->apply_($g3);$q6->apply_($h3);$q6->apply_($i3);$q6->apply_($j3);$q6->apply_($k3);$q6->apply_($l3);$q6->apply_($m3);$q6->apply_($n3);$P6->apply_($m3);$c7->apply_($m3);$o7->apply_($m3);$A7->apply_($m3);$U7->apply_($m3);$J8->apply_($r3);$J8->apply_($s3);$J8->apply_($u3);$J8->apply_($v3);$J8->apply_($w3);$J8->apply_($x3);$J8->apply_($y3);$J8->apply_($z3);$J8->apply_($A3);$J8->apply_($B3);$J8->apply_($C3);$J8->apply_($D3);$J8->apply_($E3);$J8->apply_($G3);$J8->apply_($I3);$J8->apply_($K3);$J8->apply_($L3);$J8->apply_($M3);$J8->apply_($N3);$J8->apply_($O3);$J8->apply_($P3);$J8->apply_($Q3);$J8->apply_($R3);$J8->apply_($S3);$J8->apply_($U3);$J8->apply_($W3);$J8->apply_($Y3);$J8->apply_($c4);$J8->apply_($e4);$J8->apply_($f4);$J8->apply_($g4);$J8->apply_($h4);$J8->apply_($i4);$J8->apply_($k4);$J8->apply_($m4);$J8->apply_($n4);$J8->apply_($p4);$J8->apply_($q4);$J8->apply_($r4);$J8->apply_($s4);$J8->apply_($t4);$J8->apply_($u4);$J8->apply_($w4);$J8->apply_($x4);$J8->apply_($y4);$J8->apply_($A4);$R8->apply_($r3);$R8->apply_($s3);$R8->apply_($u3);$R8->apply_($v3);$R8->apply_($w3);$R8->apply_($x3);$R8->apply_($y3);$R8->apply_($z3);$R8->apply_($A3);$R8->apply_($B3);$R8->apply_($C3);$R8->apply_($D3);$R8->apply_($E3);$R8->apply_($G3);$R8->apply_($I3);$R8->apply_($K3);$R8->apply_($M3);$R8->apply_($N3);$R8->apply_($O3);$R8->apply_($P3);$R8->apply_($Q3);$R8->apply_($S);$R8->apply_($R3);$R8->apply_($S3);$R8->apply_($U3);$R8->apply_($W3);$R8->apply_($Y3);$R8->apply_($c4);$R8->apply_($e4);$R8->apply_($f4);$R8->apply_($g4);$R8->apply_($h4);$R8->apply_($i4);$R8->apply_($k4);$R8->apply_($m4);$R8->apply_($n4);$R8->apply_($p4);$R8->apply_($q4);$R8->apply_($r4);$R8->apply_($s4);$R8->apply_($t4);$R8->apply_($u4);$R8->apply_($w4);$R8->apply_($x4);$R8->apply_($y4);$R8->apply_($A4);$d9->apply_($P3);$j9->apply_($P3);$q9->apply_($r3);$q9->apply_($s3);$q9->apply_($u3);$q9->apply_($v3);$q9->apply_($w3);$q9->apply_($x3);$q9->apply_($y3);$q9->apply_($z3);$q9->apply_($A3);$q9->apply_($B3);$q9->apply_($C3);$q9->apply_($D3);$q9->apply_($E3);$q9->apply_($G3);$q9->apply_($I3);$q9->apply_($K3);$q9->apply_($M3);$q9->apply_($N3);$q9->apply_($O3);$q9->apply_($P3);$q9->apply_($Q3);$q9->apply_($R3);$q9->apply_($S3);$q9->apply_($U3);$q9->apply_($W3);$q9->apply_($Y3);$q9->apply_($c4);$q9->apply_($e4);$q9->apply_($f4);$q9->apply_($g4);$q9->apply_($h4);$q9->apply_($i4);$q9->apply_($k4);$q9->apply_($m4);$q9->apply_($n4);$q9->apply_($p4);$q9->apply_($q4);$q9->apply_($r4);$q9->apply_($s4);$q9->apply_($t4);$q9->apply_($u4);$q9->apply_($w4);$q9->apply_($x4);$q9->apply_($y4);$q9->apply_($A4);$x9->apply_($r3);$x9->apply_($s3);$x9->apply_($u3);$x9->apply_($v3);$x9->apply_($w3);$x9->apply_($x3);$x9->apply_($y3);$x9->apply_($z3);$x9->apply_($A3);$x9->apply_($B3);$x9->apply_($C3);$x9->apply_($D3);$x9->apply_($E3);$x9->apply_($G3);$x9->apply_($I3);$x9->apply_($K3);$x9->apply_($M3);$x9->apply_($N3);$x9->apply_($O3);$x9->apply_($P3);$x9->apply_($Q3);$x9->apply_($R3);$x9->apply_($S3);$x9->apply_($U3);$x9->apply_($W3);$x9->apply_($Y3);$x9->apply_($c4);$x9->apply_($e4);$x9->apply_($f4);$x9->apply_($g4);$x9->apply_($h4);$x9->apply_($i4);$x9->apply_($k4);$x9->apply_($m4);$x9->apply_($n4);$x9->apply_($p4);$x9->apply_($q4);$x9->apply_($r4);$x9->apply_($s4);$x9->apply_($t4);$x9->apply_($u4);$x9->apply_($w4);$x9->apply_($x4);$x9->apply_($y4);$x9->apply_($A4);$E9->apply_($r3);$E9->apply_($s3);$E9->apply_($u3);$E9->apply_($v3);$E9->apply_($w3);$E9->apply_($x3);$E9->apply_($y3);$E9->apply_($z3);$E9->apply_($A3);$E9->apply_($B3);$E9->apply_($C3);$E9->apply_($D3);$E9->apply_($E3);$E9->apply_($G3);$E9->apply_($I3);$E9->apply_($K3);$E9->apply_($M3);$E9->apply_($N3);$E9->apply_($O3);$E9->apply_($P3);$E9->apply_($Q3);$E9->apply_($R3);$E9->apply_($S3);$E9->apply_($U3);$E9->apply_($W3);$E9->apply_($Y3);$E9->apply_($c4);$E9->apply_($e4);$E9->apply_($g4);$E9->apply_($h4);$E9->apply_($i4);$E9->apply_($k4);$E9->apply_($m4);$E9->apply_($n4);$E9->apply_($p4);$E9->apply_($q4);$E9->apply_($r4);$E9->apply_($s4);$E9->apply_($t4);$E9->apply_($u4);$E9->apply_($w4);$E9->apply_($x4);$E9->apply_($y4);$E9->apply_($A4);$M9->apply_($P3);$M9->apply_($f4);$ha->apply_($S);$na->apply_($S);$xa->apply_($S);$Ga->apply_($S);$Ma->apply_($S);$Sa->apply_($S);$eb->apply_($S);$pc->apply_($V3);$Nc->apply_($V3);$Vc->apply_($V3);$Vc->apply_($d4);$vd->apply_($V3);$vd->apply_($d4);$Ld->apply_($V3);$Ld->apply_($d4);$de->apply_($V3);$xe->apply_($V3);$Fe->apply_($V3);$Bf->apply_($f4);$Hf->apply_($f4);$mh->apply_($sg);$Ih->apply_($sg);$ni->apply_($sg);$Ni->apply_($r3);$Ni->apply_($s3);$Ni->apply_($u3);$Ni->apply_($v3);$Ni->apply_($w3);$Ni->apply_($x3);$Ni->apply_($y3);$Ni->apply_($z3);$Ni->apply_($A3);$Ni->apply_($B3);$Ni->apply_($C3);$Ni->apply_($D3);$Ni->apply_($E3);$Ni->apply_($G3);$Ni->apply_($I3);$Ni->apply_($K3);$Ni->apply_($M3);$Ni->apply_($N3);$Ni->apply_($O3);$Ni->apply_($Q3);$Ni->apply_($R3);$Ni->apply_($S3);$Ni->apply_($U3);$Ni->apply_($W3);$Ni->apply_($Y3);$Ni->apply_($c4);$Ni->apply_($e4);$Ni->apply_($g4);$Ni->apply_($i4);$Ni->apply_($k4);$Ni->apply_($m4);$Ni->apply_($n4);$Ni->apply_($p4);$Ni->apply_($q4);$Ni->apply_($r4);$Ni->apply_($s4);$Ni->apply_($t4);$Ni->apply_($u4);$Ni->apply_($w4);$Ni->apply_($x4);$Ni->apply_($y4);$Ni->apply_($A4);$Vi->apply_($r3);$Vi->apply_($s3);$Vi->apply_($u3);$Vi->apply_($v3);$Vi->apply_($w3);$Vi->apply_($x3);$Vi->apply_($y3);$Vi->apply_($z3);$Vi->apply_($A3);$Vi->apply_($B3);$Vi->apply_($C3);$Vi->apply_($D3);$Vi->apply_($E3);$Vi->apply_($G3);$Vi->apply_($I3);$Vi->apply_($K3);$Vi->apply_($M3);$Vi->apply_($O3);$Vi->apply_($Q3);$Vi->apply_($R3);$Vi->apply_($S3);$Vi->apply_($U3);$Vi->apply_($W3);$Vi->apply_($Y3);$Vi->apply_($c4);$Vi->apply_($e4);$Vi->apply_($g4);$Vi->apply_($i4);$Vi->apply_($k4);$Vi->apply_($m4);$Vi->apply_($n4);$Vi->apply_($p4);$Vi->apply_($q4);$Vi->apply_($r4);$Vi->apply_($s4);$Vi->apply_($t4);$Vi->apply_($u4);$Vi->apply_($w4);$Vi->apply_($x4);$Vi->apply_($y4);$Vi->apply_($A4);$gj->apply_($r3);$gj->apply_($s3);$gj->apply_($u3);$gj->apply_($v3);$gj->apply_($w3);$gj->apply_($x3);$gj->apply_($y3);$gj->apply_($z3);$gj->apply_($A3);$gj->apply_($B3);$gj->apply_($C3);$gj->apply_($D3);$gj->apply_($E3);$gj->apply_($G3);$gj->apply_($I3);$gj->apply_($K3);$gj->apply_($M3);$gj->apply_($N3);$gj->apply_($O3);$gj->apply_($Q3);$gj->apply_($R3);$gj->apply_($S3);$gj->apply_($U3);$gj->apply_($W3);$gj->apply_($Y3);$gj->apply_($c4);$gj->apply_($e4);$gj->apply_($g4);$gj->apply_($i4);$gj->apply_($k4);$gj->apply_($m4);$gj->apply_($n4);$gj->apply_($p4);$gj->apply_($q4);$gj->apply_($r4);$gj->apply_($s4);$gj->apply_($t4);$gj->apply_($u4);$gj->apply_($w4);$gj->apply_($x4);$gj->apply_($y4);$gj->apply_($A4);$qj->apply_($r3);$qj->apply_($s3);$qj->apply_($u3);$qj->apply_($v3);$qj->apply_($w3);$qj->apply_($x3);$qj->apply_($y3);$qj->apply_($z3);$qj->apply_($A3);$qj->apply_($B3);$qj->apply_($C3);$qj->apply_($D3);$qj->apply_($E3);$qj->apply_($G3);$qj->apply_($I3);$qj->apply_($K3);$qj->apply_($M3);$qj->apply_($N3);$qj->apply_($O3);$qj->apply_($Q3);$qj->apply_($R3);$qj->apply_($S3);$qj->apply_($U3);$qj->apply_($W3);$qj->apply_($Y3);$qj->apply_($c4);$qj->apply_($e4);$qj->apply_($g4);$qj->apply_($i4);$qj->apply_($k4);$qj->apply_($m4);$qj->apply_($n4);$qj->apply_($p4);$qj->apply_($q4);$qj->apply_($r4);$qj->apply_($s4);$qj->apply_($t4);$qj->apply_($u4);$qj->apply_($w4);$qj->apply_($x4);$qj->apply_($y4);$qj->apply_($A4);$xj->apply_($r3);$xj->apply_($s3);$xj->apply_($u3);$xj->apply_($v3);$xj->apply_($w3);$xj->apply_($x3);$xj->apply_($y3);$xj->apply_($z3);$xj->apply_($A3);$xj->apply_($B3);$xj->apply_($C3);$xj->apply_($D3);$xj->apply_($E3);$xj->apply_($G3);$xj->apply_($I3);$xj->apply_($K3);$xj->apply_($M3);$xj->apply_($N3);$xj->apply_($O3);$xj->apply_($Q3);$xj->apply_($R3);$xj->apply_($S3);$xj->apply_($U3);$xj->apply_($W3);$xj->apply_($Y3);$xj->apply_($c4);$xj->apply_($e4);$xj->apply_($g4);$xj->apply_($i4);$xj->apply_($k4);$xj->apply_($m4);$xj->apply_($n4);$xj->apply_($p4);$xj->apply_($q4);$xj->apply_($r4);$xj->apply_($s4);$xj->apply_($t4);$xj->apply_($u4);$xj->apply_($w4);$xj->apply_($x4);$xj->apply_($y4);$xj->apply_($A4);$Ej->apply_($r3);$Ej->apply_($s3);$Ej->apply_($u3);$Ej->apply_($v3);$Ej->apply_($w3);$Ej->apply_($x3);$Ej->apply_($y3);$Ej->apply_($z3);$Ej->apply_($A3);$Ej->apply_($B3);$Ej->apply_($C3);$Ej->apply_($D3);$Ej->apply_($E3);$Ej->apply_($G3);$Ej->apply_($I3);$Ej->apply_($K3);$Ej->apply_($M3);$Ej->apply_($N3);$Ej->apply_($O3);$Ej->apply_($Q3);$Ej->apply_($R3);$Ej->apply_($S3);$Ej->apply_($U3);$Ej->apply_($W3);$Ej->apply_($Y3);$Ej->apply_($c4);$Ej->apply_($e4);$Ej->apply_($g4);$Ej->apply_($i4);$Ej->apply_($k4);$Ej->apply_($m4);$Ej->apply_($n4);$Ej->apply_($p4);$Ej->apply_($q4);$Ej->apply_($r4);$Ej->apply_($s4);$Ej->apply_($t4);$Ej->apply_($u4);$Ej->apply_($w4);$Ej->apply_($x4);$Ej->apply_($y4);$Ej->apply_($A4);$Lj->apply_($r3);$Lj->apply_($s3);$Lj->apply_($u3);$Lj->apply_($v3);$Lj->apply_($w3);$Lj->apply_($x3);$Lj->apply_($y3);$Lj->apply_($z3);$Lj->apply_($A3);$Lj->apply_($B3);$Lj->apply_($C3);$Lj->apply_($D3);$Lj->apply_($E3);$Lj->apply_($G3);$Lj->apply_($I3);$Lj->apply_($K3);$Lj->apply_($M3);$Lj->apply_($N3);$Lj->apply_($O3);$Lj->apply_($Q3);$Lj->apply_($R3);$Lj->apply_($S3);$Lj->apply_($U3);$Lj->apply_($W3);$Lj->apply_($Y3);$Lj->apply_($c4);$Lj->apply_($e4);$Lj->apply_($g4);$Lj->apply_($i4);$Lj->apply_($k4);$Lj->apply_($m4);$Lj->apply_($n4);$Lj->apply_($p4);$Lj->apply_($q4);$Lj->apply_($r4);$Lj->apply_($s4);$Lj->apply_($t4);$Lj->apply_($u4);$Lj->apply_($w4);$Lj->apply_($x4);$Lj->apply_($y4);$Lj->apply_($A4);$Sj->apply_($r3);$Sj->apply_($s3);$Sj->apply_($u3);$Sj->apply_($v3);$Sj->apply_($w3);$Sj->apply_($x3);$Sj->apply_($y3);$Sj->apply_($z3);$Sj->apply_($A3);$Sj->apply_($B3);$Sj->apply_($C3);$Sj->apply_($D3);$Sj->apply_($E3);$Sj->apply_($G3);$Sj->apply_($I3);$Sj->apply_($K3);$Sj->apply_($M3);$Sj->apply_($N3);$Sj->apply_($O3);$Sj->apply_($Q3);$Sj->apply_($R3);$Sj->apply_($S3);$Sj->apply_($U3);$Sj->apply_($W3);$Sj->apply_($Y3);$Sj->apply_($c4);$Sj->apply_($e4);$Sj->apply_($g4);$Sj->apply_($i4);$Sj->apply_($k4);$Sj->apply_($m4);$Sj->apply_($n4);$Sj->apply_($p4);$Sj->apply_($q4);$Sj->apply_($r4);$Sj->apply_($s4);$Sj->apply_($t4);$Sj->apply_($u4);$Sj->apply_($w4);$Sj->apply_($x4);$Sj->apply_($y4);$Sj->apply_($A4);$fk->apply_($r3);$fk->apply_($s3);$fk->apply_($u3);$fk->apply_($v3);$fk->apply_($w3);$fk->apply_($x3);$fk->apply_($y3);$fk->apply_($z3);$fk->apply_($A3);$fk->apply_($B3);$fk->apply_($C3);$fk->apply_($D3);$fk->apply_($E3);$fk->apply_($G3);$fk->apply_($I3);$fk->apply_($K3);$fk->apply_($M3);$fk->apply_($O3);$fk->apply_($Q3);$fk->apply_($R3);$fk->apply_($A);$fk->apply_($S3);$fk->apply_($U3);$fk->apply_($W3);$fk->apply_($Y3);$fk->apply_($c4);$fk->apply_($e4);$fk->apply_($f4);$fk->apply_($g4);$fk->apply_($h4);$fk->apply_($i4);$fk->apply_($k4);$fk->apply_($m4);$fk->apply_($n4);$fk->apply_($p4);$fk->apply_($q4);$fk->apply_($r4);$fk->apply_($s4);$fk->apply_($u4);$fk->apply_($w4);$fk->apply_($x4);$fk->apply_($y4);$fk->apply_($A4);$mk->apply_($r3);$mk->apply_($s3);$mk->apply_($u3);$mk->apply_($v3);$mk->apply_($w3);$mk->apply_($x3);$mk->apply_($y3);$mk->apply_($z3);$mk->apply_($A3);$mk->apply_($B3);$mk->apply_($C3);$mk->apply_($D3);$mk->apply_($E3);$mk->apply_($G3);$mk->apply_($I3);$mk->apply_($K3);$mk->apply_($M3);$mk->apply_($O3);$mk->apply_($Q3);$mk->apply_($R3);$mk->apply_($S3);$mk->apply_($U3);$mk->apply_($W3);$mk->apply_($Y3);$mk->apply_($c4);$mk->apply_($e4);$mk->apply_($g4);$mk->apply_($i4);$mk->apply_($k4);$mk->apply_($m4);$mk->apply_($n4);$mk->apply_($p4);$mk->apply_($q4);$mk->apply_($s4);$mk->apply_($u4);$mk->apply_($w4);$mk->apply_($x4);$mk->apply_($y4);$mk->apply_($A4);$jl->apply_($e3);$Bl->apply_($e3);$Ql->apply_($v3);$Ql->apply_($w3);$Ql->apply_($x3);$Ql->apply_($y3);$Ql->apply_($z3);$Ql->apply_($A3);$Ql->apply_($B3);$Ql->apply_($C3);$Ql->apply_($D3);$Ql->apply_($E3);$hm->apply_($f3);$om->apply_($f3);$Hm->apply_($g3);$Om->apply_($g3);$kn->apply_($g3);$En->apply_($g3);$Mn->apply_($g3);$eo->apply_($g3);$Bo->apply_($h3);$Bo->apply_($j3);$Io->apply_($h3);$Qo->apply_($h3);$ip->apply_($h3);$ip->apply_($j3);$tp->apply_($h3);$Hp->apply_($h3);$Hp->apply_($j3);$mq->apply_($y3);$Eq->apply_($i3);$Mq->apply_($i3);$Tq->apply_($i3);$lr->apply_($i3);$tr->apply_($i3);$Nr->apply_($i3);$ls->apply_($j3);$us->apply_($j3);$ht->apply_($it);$st->apply_($k3);$Ft->apply_($k3);$Bu->apply_($n3);$Iu->apply_($n3);$Wu->apply_($n3);$uv->apply_($F3);$uv->apply_($H3);$uv->apply_($J3);$uv->apply_($z4);$Gv->apply_($F3);$Gv->apply_($H3);$Gv->apply_($J3);$Gv->apply_($z4);$Vv->apply_($F3);$Vv->apply_($H3);$Vv->apply_($J3);$rw->apply_($F3);$rw->apply_($H3);$rw->apply_($J3);$Hw->apply_($G3);$Hw->apply_($I3);$Hw->apply_($K3);$fx->apply_($H3);$mx->apply_($H3);$wx->apply_($H3);$Ix->apply_($H3);$ly->apply_($I3);$Ey->apply_($J3);$Ly->apply_($J3);$nz->apply_($N3);$Uz->apply_($R3);$uA->apply_($A);$IA->apply_($A);$PA->apply_($A);$YA->apply_($A);$hB->apply_($A);$FB->apply_($T3);$MB->apply_($T3);$iC->apply_($T3);$CC->apply_($T3);$cD->apply_($it);$HD->apply_($X3);$ZD->apply_($X3);$wE->apply_($X3);$FE->apply_($X3);$PE->apply_($X3);$nF->apply_($Z3);$nF->apply_($C);$nF->apply_($H);$JF->apply_($d4);$SF->apply_($d4);$JG->apply_($it);$TG->apply_($h4);$ZG->apply_($h4);$xH->apply_($j4);$xH->apply_($l4);$HH->apply_($j4);$OH->apply_($j4);$eI->apply_($j4);$JI->apply_($C);$QI->apply_($C);$XI->apply_($C);$jJ->apply_($C);$uJ->apply_($C);$JJ->apply_($n4);$RJ->apply_($n4);$mK->apply_($o4);$BK->apply_($o4);$IK->apply_($o4);$dL->apply_($H);$jL->apply_($H);$pL->apply_($H);$DL->apply_($it);$ni::self=$RM;&$V($T);&$V($f1);&$V($n1);&$V($t1);&$V($G1);&$V($T1);&$V($i2);&$V($v2);&$V($R2);&$V($X2);&$V($P4);&$V($R4);&$T4($R4);&$V($c5);&$V($w5);&$V($E5);&$V($Q5);&$V($e6);&$V($q6);&$V($s6);&$T4($s6);&$V($y6);&$V($P6);&$V($c7);&$V($o7);&$V($A7);&$V($U7);&$V($W7);&$T4($W7);&$V($r8);&$V($y8);&$V($J8);&$V($L8);&$T4($L8);&$V($R8);&$V($d9);&$V($j9);&$V($q9);&$V($x9);&$V($E9);&$V($M9);&$V($O9);&$T4($O9);&$V($U9);&$V($ha);&$V($na);&$V($xa);&$V($Ga);&$V($Ma);&$V($Sa);&$V($eb);&$V($gb);&$T4($gb);&$V($Ab);&$V($Ib);&$V($Zb);&$V($pc);&$V($Nc);&$V($Vc);&$V($vd);&$V($Ld);&$V($de);&$V($xe);&$V($Fe);&$V($He);&$T4($He);&$V($Re);&$V($Ze);&$V($mf);&$V($Bf);&$V($Hf);&$V($Jf);&$T4($Jf);&$V($kg);&$V($qg);&$V($mh);&$V($Ih);&$V($ni);&$V($pi);&$T4($pi);&$V($zi);&$V($Ni);&$V($Pi);&$V($Vi);&$V($gj);&$V($qj);&$V($xj);&$V($Ej);&$V($Lj);&$V($Sj);&$V($Uj);&$V($Wj);&$T4($Wj);&$V($fk);&$V($mk);&$V($ok);&$T4($ok);&$V($xk);&$T4($xk);&$V($Bk);&$T4($Bk);&$V($Dk);&$T4($Dk);&$V($Fk);&$T4($Fk);&$V($Kk);&$T4($Kk);&$V($Ok);&$T4($Ok);&$V($Tk);&$T4($Tk);&$V($Yk);&$T4($Yk);&$V($jl);&$V($Bl);&$V($Dl);&$T4($Dl);&$V($Ql);&$V($Sl);&$T4($Sl);&$V($Ul);&$T4($Ul);&$V($hm);&$V($om);&$V($qm);&$T4($qm);&$V($vm);&$T4($vm);&$V($Hm);&$V($Om);&$V($kn);&$V($En);&$V($Mn);&$V($eo);&$V($go);&$T4($go);&$V($lo);&$T4($lo);&$V($Bo);&$V($Io);&$V($Qo);&$V($ip);&$V($tp);&$V($Hp);&$V($Kp);&$T4($Kp);&$Np($Kp);&$V($mq);&$V($oq);&$T4($oq);&$V($Eq);&$V($Mq);&$V($Tq);&$V($lr);&$V($tr);&$V($Nr);&$V($Pr);&$T4($Pr);&$V($Ur);&$T4($Ur);&$V($ls);&$V($us);&$V($ws);&$T4($ws);&$V($Bs);&$T4($Bs);&$V($ht);&$V($st);&$V($Ft);&$V($Ht);&$T4($Ht);&$V($Mt);&$T4($Mt);&$V($fu);&$T4($fu);&$V($Bu);&$V($Iu);&$V($Wu);&$V($Yu);&$T4($Yu);&$V($fv);&$T4($fv);&$V($uv);&$V($Gv);&$V($Iv);&$T4($Iv);&$V($Vv);&$V($rw);&$V($tw);&$T4($tw);&$ww($tw);&$V($Dw);&$T4($Dw);&$V($Hw);&$V($Jw);&$T4($Jw);&$V($fx);&$V($mx);&$V($wx);&$V($Ix);&$V($Nx);&$T4($Nx);&$ww($Nx);&$Qx($Nx);&$V($ly);&$V($ny);&$T4($ny);&$V($Ey);&$V($Ly);&$V($Ny);&$T4($Ny);&$ww($Ny);&$V($Sy);&$T4($Sy);&$V($Zy);&$T4($Zy);&$V($nz);&$V($pz);&$T4($pz);&$V($vz);&$T4($vz);&$V($Ez);&$T4($Ez);&$V($Uz);&$V($Wz);&$T4($Wz);&$V($uA);&$V($IA);&$V($PA);&$V($YA);&$V($hB);&$V($jB);&$T4($jB);&$V($oB);&$T4($oB);&$V($FB);&$V($MB);&$V($iC);&$V($CC);&$V($EC);&$T4($EC);&$V($JC);&$T4($JC);&$V($cD);&$V($iD);&$T4($iD);&$V($HD);&$V($ZD);&$V($wE);&$V($FE);&$V($PE);&$V($RE);&$T4($RE);&$V($WE);&$T4($WE);&$V($nF);&$V($pF);&$T4($pF);&$V($uF);&$T4($uF);&$V($JF);&$V($SF);&$V($UF);&$T4($UF);&$V($ZF);&$T4($ZF);&$V($nG);&$T4($nG);&$V($JG);&$V($TG);&$V($ZG);&$V($dH);&$T4($dH);&$V($jH);&$T4($jH);&$V($xH);&$V($zH);&$T4($zH);&$V($HH);&$V($OH);&$V($eI);&$V($gI);&$T4($gI);&$V($nI);&$T4($nI);&$V($pI);&$T4($pI);&$V($JI);&$V($QI);&$V($XI);&$V($jJ);&$V($uJ);&$V($wJ);&$T4($wJ);&$zJ($wJ);&$V($JJ);&$V($RJ);&$V($TJ);&$T4($TJ);&$V($mK);&$V($BK);&$V($IK);&$V($KK);&$T4($KK);&$V($PK);&$T4($PK);&$V($dL);&$V($jL);&$V($pL);&$V($rL);&$T4($rL);&$V($wL);&$T4($wL);&$V($DL);&$V($LL);&$T4($LL);&$V($QL);&$T4($QL);&$V($ZL);&$T4($ZL);&$V($gM);&$T4($gM);&$V($lM);&$T4($lM);&$V($tM);&$T4($tM);ni->run(@ARGV);
__DATA__
