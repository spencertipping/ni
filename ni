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
dependencies are indirected through a name table.#;$c1=[$i,$X,$Y,$Z];$d1=[$c1];$e1=q#/fabric#;$f1=bless({$e,$d1,$Q,$e1},$S);$g1=q#ni.doc:/io#;$h1=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$i1=[$i,$h1];$j1=[$i1];$k1=q#/io#;$l1=bless({$e,$j1,$Q,$k1},$S);$m1=q#ni.doc:/io/buffer#;$n1=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$o1=[$f,$n1];$p1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$q1=[];$r1=[];$s1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$t1=bless({$t,$r1,$v,$q,$w,$s1,$y,$z},$A);$u1=bless({$n,$q1,$p,$q,$r,$q,$s,$t1},$C);$v1=[$i,$p1,$u1];$w1=[$o1,$v1];$x1=q#/io/buffer#;$y1=bless({$e,$w1,$Q,$x1},$S);$z1=q#ni.doc:/io/cat#;$A1=q#
my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
my $combined = $io1 + $io2 + $io3;
$combined->into_sync($destination_io);#;$B1=[$f,$A1];$C1=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$D1=[];$E1=[];$F1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$G1=bless({$t,$E1,$v,$q,$w,$F1,$y,$z},$A);$H1=bless({$n,$D1,$p,$q,$r,$q,$s,$G1},$C);$I1=[$i,$C1,$H1];$J1=[$B1,$I1];$K1=q#/io/cat#;$L1=bless({$e,$J1,$Q,$K1},$S);$M1=q#ni.doc:/io/exec#;$N1=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$O1=[$f,$N1];$P1=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$Q1=[];$R1=[];$S1=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$T1=bless({$t,$R1,$v,$q,$w,$S1,$y,$z},$A);$U1=bless({$n,$Q1,$p,$q,$r,$q,$s,$T1},$C);$V1=[$i,$P1,$U1];$W1=[$O1,$V1];$X1=q#/io/exec#;$Y1=bless({$e,$W1,$Q,$X1},$S);$Z1=q#ni.doc:/io/fd#;$c2=q#
open my $fh, ...;
my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
my $fd = ni('ni:/io/fd')->new(0);   \# from number
my $fd = ni('fd:0');                \# same thing
$fd->nonblock(1)->read($_, 100);
$fd->be(10);                        \# move FD number#;$d2=[$f,$c2];$e2=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$f2=[];$g2=[];$h2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$i2=bless({$t,$g2,$v,$q,$w,$h2,$y,$z},$A);$j2=bless({$n,$f2,$p,$q,$r,$q,$s,$i2},$C);$k2=[$i,$e2,$j2];$l2=[$d2,$k2];$m2=q#/io/fd#;$n2=bless({$e,$l2,$Q,$m2},$S);$o2=q#ni.doc:/io/file#;$p2=q#
my $f = ni('ni:/io/file')->new('/etc/passwd');
my $f = ni('file:/etc/passwd');     \# same as above
$f->into_sync(ni('fd:1'));          \# cat to stdout#;$q2=[$f,$p2];$r2=q#warning#;$s2=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$t2=[$r2,$s2];$u2=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$v2=[];$w2=[];$x2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$y2=bless({$t,$w2,$v,$q,$w,$x2,$y,$z},$A);$z2=bless({$n,$v2,$p,$q,$r,$q,$s,$y2},$C);$A2=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$B2=[];$C2=[];$D2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$E2=bless({$t,$C2,$v,$q,$w,$D2,$y,$z},$A);$F2=bless({$n,$B2,$p,$q,$r,$q,$s,$E2},$C);$G2=[$i,$u2,$z2,$A2,$F2];$H2=[$q2,$t2,$G2];$I2=q#/io/file#;$J2=bless({$e,$H2,$Q,$I2},$S);$K2=q#ni.doc:/io/file_update_fd#;$L2=q#A write fd that performs a file rename upon closing.#;$M2=[$i,$L2];$N2=[$M2];$O2=q#/io/file_update_fd#;$P2=bless({$e,$N2,$Q,$O2},$S);$Q2=q#ni.doc:/io/object#;$R2=q#TODO#;$S2=q#referent#;$T2=q#applied_to#;$U2=q#ni::/io/buffer#;$V2=q#ni::/io/cat#;$W2=q#ni::/io/exec#;$X2=q#ni::/io/fd#;$Y2=q#ni::/io/file#;$Z2=q#ni::/io/file_update_fd#;$c3=q#ni::/io/null#;$d3=q#ni::/io/object#;$e3=q#ni::/io/pid#;$f3=q#ni::/io/str#;$g3={$U2,1,$V2,1,$W2,1,$X2,1,$Y2,1,$Z2,1,$c3,1,$d3,1,$e3,1,$f3,1};$h3=q#/io/object#;$i3=q#slices#;$j3=q#ni::/class#;$k3=q#ni::/class.c#;$l3=q#ni::/io/buffer.c#;$m3=q#ni::/io/cat.c#;$n3=q#ni::/io/exec.c#;$o3=q#ni::/io/fd.c#;$p3=q#ni::/io/file.c#;$q3=q#ni::/io/file_update_fd.c#;$r3=q#ni::/io/null.c#;$s3=q#ni::/io/object.c#;$t3=q#ni::/io/pid.c#;$u3=q#ni::/io/str.c#;$v3=q#ni::/io/transfer#;$w3=q#ni::/io/transfer.c#;$x3=q#ni::/io/transfer_async#;$y3=q#ni::/io/transfer_async.c#;$z3=q#ni::/io/transfer_sync#;$A3=q#ni::/io/transfer_sync.c#;$B3=q#ni::/lib/behavior#;$C3=q#ni::/lib/behavior.c#;$D3=q#ni::/lib/branch#;$E3=q#ni::/lib/branch.c#;$F3=q#ni::/lib/dataslice#;$G3=q#ni::/lib/dataslice.c#;$H3=q#ni::/lib/doc.c#;$I3=q#ni::/lib/fn.c#;$J3=q#ni::/lib/future#;$K3=q#ni::/lib/future.c#;$L3=q#ni::/lib/image#;$M3=q#ni::/lib/image.c#;$N3=q#ni::/lib/ni#;$O3=q#ni::/lib/ni.c#;$P3=q#ni::/lib/object_metadata#;$Q3=q#ni::/lib/object_metadata.c#;$R3=q#ni::/lib/quote_simple#;$S3=q#ni::/lib/quote_simple.c#;$T3=q#ni::/lib/slice#;$U3=q#ni::/lib/slice.c#;$V3=q#ni::/lib/tag#;$W3=q#ni::/lib/tag.c#;$X3=q#ni::/lib/test_assert_eq#;$Y3=q#ni::/lib/test_assert_eq.c#;$Z3=q#ni::/lib/test_assertion#;$c4=q#ni::/lib/test_assertion.c#;$d4=q#ni::/lib/test_case.c#;$e4=q#ni::/lib/test_value#;$f4=q#ni::/lib/test_value.c#;$g4=q#ni::/lib/todo.c#;$h4=q#ni::/metaclass#;$i4=q#ni::/metaclass.c#;$j4=q#ni::/module#;$k4=q#ni::/module.c#;$l4=q#ni::/object#;$m4=q#ni::/object.c#;$n4=q#ni::/semantic/dimension#;$o4=q#ni::/semantic/dimension.c#;$p4=q#ni::/semantic/task#;$q4=q#ni::/semantic/task.c#;$r4={$j3,1,$k3,1,$U2,1,$l3,1,$V2,1,$m3,1,$W2,1,$n3,1,$X2,1,$o3,1,$Y2,1,$p3,1,$Z2,1,$q3,1,$c3,1,$r3,1,$d3,1,$s3,1,$e3,1,$t3,1,$f3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$F3,1,$G3,1,$S,1,$H3,1,$A,1,$I3,1,$J3,1,$K3,1,$L3,1,$M3,1,$N3,1,$O3,1,$P3,1,$Q3,1,$R3,1,$S3,1,$T3,1,$U3,1,$V3,1,$W3,1,$X3,1,$Y3,1,$Z3,1,$c4,1,$C,1,$d4,1,$e4,1,$f4,1,$H,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$l4,1,$m4,1,$n4,1,$o4,1,$p4,1,$q4,1};$s4=q#/object#;$t4={};$u4=q#ctor#;$v4=q#dtor#;$w4=q#methods#;$x4=q#DESTROY#;$y4=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$z4=bless({$w,$y4,$y,$z},$A);$A4=q#class#;$B4=q#(my $r = ref shift) =~ s/^ni::/ni:/; ni$r#;$C4=bless({$w,$B4,$y,$z},$A);$D4={$x4,$z4,$A4,$C4};$E4=q#/lib/instance.b#;$F4=bless({$T2,$t4,$u4,$q,$v4,$q,$w4,$D4,$Q,$E4},$T3);$G4=[$F4];$H4=bless({$T2,$r4,$Q,$s4,$i3,$G4},$m4);$I4=q#my $s = shift; $s->apply($s->package)#;$J4=bless({$w,$I4,$y,$z},$A);$K4={};$L4=q#(bool#;$M4=[];$N4=bless({$t,$M4,$v,$q,$w,1,$y,$z},$A);$O4={$L4,$N4};$P4=q#/io/object_ops.b#;$Q4=bless({$T2,$K4,$u4,$q,$v4,$q,$w4,$O4,$Q,$P4},$T3);$R4={};$S4=q#die#;$T4=[];$U4=q#shift; die join " ", @_#;$V4=bless({$t,$T4,$v,$q,$w,$U4,$y,$z},$A);$W4=q#io_check#;$X4=[];$Y4=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$Z4=bless({$t,$X4,$v,$q,$w,$Y4,$y,$z},$A);$c5=q#io_check_defined#;$d5=[];$e5=q#shift->io_check(sub {defined shift}, @_)#;$f5=bless({$t,$d5,$v,$q,$w,$e5,$y,$z},$A);$g5=q#io_check_true#;$h5=[];$i5=q#shift->io_check(sub {shift}, @_)#;$j5=bless({$t,$h5,$v,$q,$w,$i5,$y,$z},$A);$k5={$S4,$V4,$W4,$Z4,$c5,$f5,$g5,$j5};$l5=q#/io/object_checks.b#;$m5=bless({$T2,$R4,$u4,$q,$v4,$q,$w4,$k5,$Q,$l5},$T3);$n5={};$o5=q#(+#;$p5=[];$q5=q#ni('ni:/io/cat')->new(@_[0, 1])#;$r5=bless({$t,$p5,$v,$q,$w,$q5,$y,$z},$A);$s5={$o5,$r5};$t5=q#/io/object_constructors.b#;$u5=bless({$T2,$n5,$u4,$q,$v4,$q,$w4,$s5,$Q,$t5},$T3);$v5={};$w5=q#read_all#;$x5=[];$y5=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$z5=bless({$t,$x5,$v,$q,$w,$y5,$y,$z},$A);$A5=q#write_all#;$B5=[];$C5=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$D5=bless({$t,$B5,$v,$q,$w,$C5,$y,$z},$A);$E5={$w5,$z5,$A5,$D5};$F5=q#/io/object_memory.b#;$G5=bless({$T2,$v5,$u4,$q,$v4,$q,$w4,$E5,$Q,$F5},$T3);$H5={};$I5=q#connect_sync#;$J5=[];$K5=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$L5=bless({$t,$J5,$v,$q,$w,$K5,$y,$z},$A);$M5=q#into_sync#;$N5=[];$O5=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$P5=bless({$t,$N5,$v,$q,$w,$O5,$y,$z},$A);$Q5={$I5,$L5,$M5,$P5};$R5=q#/io/object_transfer_sync.b#;$S5=bless({$T2,$H5,$u4,$q,$v4,$q,$w4,$Q5,$Q,$R5},$T3);$T5={};$U5=q#connect_async#;$V5=[];$W5=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$X5=bless({$t,$V5,$v,$q,$w,$W5,$y,$z},$A);$Y5=q#into_async#;$Z5=[];$c6=q#ni('ni:/io/transfer_async')->new(@_)->run#;$d6=bless({$t,$Z5,$v,$q,$w,$c6,$y,$z},$A);$e6={$U5,$X5,$Y5,$d6};$f6=q#/io/object_transfer_async.b#;$g6=bless({$T2,$T5,$u4,$q,$v4,$q,$w4,$e6,$Q,$f6},$T3);$h6=[$H4,$Q4,$m5,$u5,$G5,$S5,$g6,$g6,$S5,$g6,$S5];$i6=bless({$T2,$g3,$Q,$h3,$i3,$h6},$s3);$j6=q#migrate die() into /lib/ as a base behavior#;$k6=[$j6];$l6=bless({$S2,$i6,$E,$k6},$H);$m6=[$R2,$l6];$n6=[$m6];$o6=bless({$e,$n6,$Q,$h3},$S);$p6=q#ni.doc:/io/pid#;$q6=q#eg#;$r6=[];$s6={$e3,1};$t6=q#/io/pid#;$u6={};$v6=q#pid#;$w6=[];$x6=q#shift->{'pid'}#;$y6=bless({$t,$w6,$v,$q,$w,$x6,$y,$z},$A);$z6=q#status#;$A6=[];$B6=q#shift->{'status'}#;$C6=bless({$t,$A6,$v,$q,$w,$B6,$y,$z},$A);$D6={$v6,$y6,$z6,$C6};$E6=q#/io/pid_readers.b#;$F6=bless({$T2,$u6,$u4,$q,$v4,$q,$w4,$D6,$Q,$E6},$T3);$G6={};$H6=[];$I6=q#shift->await#;$J6=bless({$t,$H6,$v,$q,$w,$I6,$y,$z},$A);$K6=q#instantiate#;$L6=[];$M6=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$N6=bless({$t,$L6,$v,$q,$w,$M6,$y,$z},$A);$O6={$K6,$N6};$P6=q#/io/pid_init.b#;$Q6=bless({$T2,$G6,$u4,$q,$v4,$J6,$w4,$O6,$Q,$P6},$T3);$R6={};$S6=q#await#;$T6=[];$U6=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$V6=bless({$t,$T6,$v,$q,$w,$U6,$y,$z},$A);$W6=q#running#;$X6=[];$Y6=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$Z6=bless({$t,$X6,$v,$q,$w,$Y6,$y,$z},$A);$c7={$S6,$V6,$W6,$Z6};$d7=q#/io/pid_wait.b#;$e7=bless({$T2,$R6,$u4,$q,$v4,$q,$w4,$c7,$Q,$d7},$T3);$f7={};$g7=q#read#;$h7=[];$i7=q#shift->stdout->read(@_)#;$j7=bless({$t,$h7,$v,$q,$w,$i7,$y,$z},$A);$k7=q#write#;$l7=[];$m7=q#shift->stdin->write(@_)#;$n7=bless({$t,$l7,$v,$q,$w,$m7,$y,$z},$A);$o7={$g7,$j7,$k7,$n7};$p7=q#/io/pid_io.b#;$q7=bless({$T2,$f7,$u4,$q,$v4,$q,$w4,$o7,$Q,$p7},$T3);$r7={};$s7=q#fd#;$t7=[];$u7=q#$_[0]->{external_fds}{$_[1]}#;$v7=bless({$t,$t7,$v,$q,$w,$u7,$y,$z},$A);$w7=q#stderr#;$x7=[];$y7=q#shift->fd(2)#;$z7=bless({$t,$x7,$v,$q,$w,$y7,$y,$z},$A);$A7=q#stdin#;$B7=[];$C7=q#shift->fd(0)#;$D7=bless({$t,$B7,$v,$q,$w,$C7,$y,$z},$A);$E7=q#stdout#;$F7=[];$G7=q#shift->fd(1)#;$H7=bless({$t,$F7,$v,$q,$w,$G7,$y,$z},$A);$I7={$s7,$v7,$w7,$z7,$A7,$D7,$E7,$H7};$J7=q#/io/pid_accessors.b#;$K7=bless({$T2,$r7,$u4,$q,$v4,$q,$w4,$I7,$Q,$J7},$T3);$L7=[$i6,$F6,$Q6,$e7,$q7,$K7];$M7=bless({$T2,$s6,$Q,$t6,$i3,$L7},$t3);$N7=[];$O7=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$P7=bless({$t,$N7,$v,$q,$w,$O7,$y,$z},$A);$Q7=bless({$n,$r6,$p,$q,$r,$q,$S2,$M7,$s,$P7},$C);$R7=[$q6,$Q7];$S7=[];$T7=[];$U7=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$V7=bless({$t,$T7,$v,$q,$w,$U7,$y,$z},$A);$W7=bless({$n,$S7,$p,$q,$r,$q,$S2,$M7,$s,$V7},$C);$X7=[$q6,$W7];$Y7=[];$Z7=[];$c8=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$d8=bless({$t,$Z7,$v,$q,$w,$c8,$y,$z},$A);$e8=bless({$n,$Y7,$p,$q,$r,$q,$S2,$M7,$s,$d8},$C);$f8=[$q6,$e8];$g8=[$R7,$X7,$f8];$h8=bless({$e,$g8,$Q,$t6},$S);$i8=q#ni.doc:/lib#;$j8=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$k8=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$l8=[$i,$j8,$k8];$m8=[$l8];$n8=q#/lib#;$o8=bless({$e,$m8,$Q,$n8},$S);$p8=q#ni.doc:/lib/dataslice#;$q8={$F3,1};$r8=q#/lib/dataslice#;$s8={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$F3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$T3,1,$U3,1,$V3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$t8=q#/lib/behavior#;$u8={};$v8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$w8=bless({$w,$v8,$y,$z},$A);$x8={$e,$w8};$y8=q#/lib/documentable.b#;$z8=bless({$T2,$u8,$u4,$q,$v4,$q,$w4,$x8,$Q,$y8},$T3);$A8=[$H4,$z8];$B8=bless({$T2,$s8,$Q,$t8,$i3,$A8},$C3);$C8={};$D8=q#$_[0]->namespace . ":" . $_[0]->{name}#;$E8=bless({$w,$D8,$y,$z},$A);$F8={$Q,$E8};$G8=q#/lib/named.b#;$H8=bless({$T2,$C8,$u4,$V,$v4,$q,$w4,$F8,$Q,$G8},$T3);$I8={};$J8=q#apply#;$K8=q#shift->apply_(@_)#;$L8=bless({$w,$K8,$y,$z},$A);$M8=q#apply_#;$N8=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$O8=bless({$w,$N8,$y,$z},$A);$P8={$J8,$L8,$M8,$O8};$Q8=q#/lib/dataslice.b#;$R8=bless({$T2,$I8,$u4,$q,$v4,$q,$w4,$P8,$Q,$Q8},$T3);$S8={};$T8=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$U8=bless({$w,$T8,$y,$z},$A);$V8={$K6,$U8};$W8=q#/lib/dataslice_init.b#;$X8=bless({$T2,$S8,$u4,$q,$v4,$q,$w4,$V8,$Q,$W8},$T3);$Y8={};$Z8=q#namespace#;$c9=q#'ni'#;$d9=bless({$w,$c9,$y,$z},$A);$e9={$Z8,$d9};$f9=q#/lib/named_in_ni.b#;$g9=bless({$T2,$Y8,$u4,$q,$v4,$q,$w4,$e9,$Q,$f9},$T3);$h9={};$i9=q#package#;$j9=q#my $name = shift->{name};
$name =~ /^\\// ? "ni::$name" : $name;#;$k9=bless({$w,$j9,$y,$z},$A);$l9={$i9,$k9};$m9=q#/lib/namespaced.b#;$n9=bless({$T2,$h9,$u4,$q,$v4,$q,$w4,$l9,$Q,$m9},$T3);$o9={};$p9=q#resolve#;$q9=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$r9=bless({$w,$q9,$y,$z},$A);$s9={$p9,$r9};$t9=q#/lib/resolver.b#;$u9=bless({$T2,$o9,$u4,$q,$v4,$q,$w4,$s9,$Q,$t9},$T3);$v9={};$w9=q#serialize#;$x9=[];$y9=q#local $_;
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
$g;#;$z9=bless({$t,$x9,$v,$q,$w,$y9,$y,$z},$A);$A9={$w9,$z9};$B9=q#/lib/slice_serialize.b#;$C9=bless({$T2,$v9,$u4,$q,$v4,$q,$w4,$A9,$Q,$B9},$T3);$D9=[$B8,$H8,$R8,$X8,$g9,$n9,$u9,$C9];$E9=bless({$T2,$q8,$Q,$r8,$i3,$D9},$G3);$F9=q#Fix serialization for dataslices#;$G9=[$F9];$H9=bless({$S2,$E9,$E,$G9},$H);$I9=[$R2,$H9];$J9=[$I9];$K9=bless({$e,$J9,$Q,$r8},$S);$L9=q#ni.doc:/lib/doc#;$M9=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$N9=[$f,$M9];$O9=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$P9=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$Q9=[];$R9=[];$S9=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$T9=bless({$t,$R9,$v,$q,$w,$S9,$y,$z},$A);$U9=bless({$n,$Q9,$p,$q,$r,$q,$s,$T9},$C);$V9=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$W9=[];$X9=[];$Y9=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$Z9=bless({$t,$X9,$v,$q,$w,$Y9,$y,$z},$A);$ca=bless({$n,$W9,$p,$q,$r,$q,$s,$Z9},$C);$da=[$i,$O9,$P9,$U9,$V9,$ca];$ea=[$N9,$da];$fa=q#/lib/doc#;$ga=bless({$e,$ea,$Q,$fa},$S);$ha=q#ni.doc:/lib/fn#;$ia=q#Give functions a way to name themselves so we can do \#line
reporting#;$ja=[$ia];$ka=bless({$E,$ja},$H);$la=[$i,$ka];$ma=[$la];$na=q#/lib/fn#;$oa=bless({$e,$ma,$Q,$na},$S);$pa=q#ni.doc:/lib/future#;$qa=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$ra=[];$sa=[];$ta=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$ua=bless({$t,$sa,$v,$q,$w,$ta,$y,$z},$A);$va=bless({$n,$ra,$p,$q,$r,$q,$s,$ua},$C);$wa=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$xa=[];$ya=[];$za=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$Aa=bless({$t,$ya,$v,$q,$w,$za,$y,$z},$A);$Ba=bless({$n,$xa,$p,$q,$r,$q,$s,$Aa},$C);$Ca=[$i,$qa,$va,$wa,$Ba];$Da=[$Ca];$Ea=q#/lib/future#;$Fa=bless({$e,$Da,$Q,$Ea},$S);$Ga=q#ni.doc:/lib/image#;$Ha=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$Ia=[$f,$Ha];$Ja=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$Ka=[$i,$Ja];$La={$L3,1};$Ma=q#/lib/image#;$Na={};$Oa=[];$Pa=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Qa=bless({$t,$Oa,$v,$q,$w,$Pa,$y,$z},$A);$Ra={$K6,$Qa};$Sa=q#/lib/image_init.b#;$Ta=bless({$T2,$Na,$u4,$q,$v4,$q,$w4,$Ra,$Q,$Sa},$T3);$Ua={};$Va=q#boot_side_effect#;$Wa=[];$Xa=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ya=bless({$t,$Wa,$v,$q,$w,$Xa,$y,$z},$A);$Za=q#finalizer#;$cb=[];$db=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$eb=bless({$t,$cb,$v,$q,$w,$db,$y,$z},$A);$fb=q#io#;$gb=[];$hb=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$ib=bless({$t,$gb,$v,$q,$w,$hb,$y,$z},$A);$jb=q#reconstruction#;$kb=[];$lb=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$mb=bless({$t,$kb,$v,$q,$w,$lb,$y,$z},$A);$nb=q#side_effect#;$ob=[];$pb=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$qb=bless({$t,$ob,$v,$q,$w,$pb,$y,$z},$A);$rb={$Va,$Ya,$Za,$eb,$fb,$ib,$jb,$mb,$nb,$qb};$sb=q#/lib/image_quoting.b#;$tb=bless({$T2,$Ua,$u4,$q,$v4,$q,$w4,$rb,$Q,$sb},$T3);$ub={};$vb=q#quote_code#;$wb=[];$xb=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$yb=bless({$t,$wb,$v,$q,$w,$xb,$y,$z},$A);$zb={$vb,$yb};$Ab=q#/lib/quote_code_fail.b#;$Bb=bless({$T2,$ub,$u4,$q,$v4,$q,$w4,$zb,$Q,$Ab},$T3);$Cb={};$Db=q#quote_array#;$Eb=[];$Fb=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Gb=bless({$t,$Eb,$v,$q,$w,$Fb,$y,$z},$A);$Hb=q#quote_hash#;$Ib=[];$Jb=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Kb=bless({$t,$Ib,$v,$q,$w,$Jb,$y,$z},$A);$Lb=q#quote_scalar#;$Mb=[];$Nb=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Ob=bless({$t,$Mb,$v,$q,$w,$Nb,$y,$z},$A);$Pb=q#quote_scalar_ref#;$Qb=[];$Rb=q#'\\\\' . shift->quote(${$_[0]})#;$Sb=bless({$t,$Qb,$v,$q,$w,$Rb,$y,$z},$A);$Tb=q#quote_value#;$Ub=[];$Vb=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Wb=bless({$t,$Ub,$v,$q,$w,$Vb,$y,$z},$A);$Xb={$Db,$Gb,$Hb,$Kb,$Lb,$Ob,$Pb,$Sb,$Tb,$Wb};$Yb=q#/lib/quote_values.b#;$Zb=bless({$T2,$Cb,$u4,$q,$v4,$q,$w4,$Xb,$Q,$Yb},$T3);$cc={};$dc=q#quote_blessed#;$ec=[];$fc=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$gc=bless({$t,$ec,$v,$q,$w,$fc,$y,$z},$A);$hc=q#quote_class#;$ic=[];$jc=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$kc=bless({$t,$ic,$v,$q,$w,$jc,$y,$z},$A);$lc=q#quote_object#;$mc=[];$nc=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$oc=bless({$t,$mc,$v,$q,$w,$nc,$y,$z},$A);$pc={$dc,$gc,$hc,$kc,$lc,$oc};$qc=q#/lib/quote_objects.b#;$rc=bless({$T2,$cc,$u4,$q,$v4,$q,$w4,$pc,$Q,$qc},$T3);$sc={};$tc=q#circular_arrayref#;$uc=[];$vc=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$wc=bless({$t,$uc,$v,$q,$w,$vc,$y,$z},$A);$xc=q#circular_hashref#;$yc=[];$zc=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Ac=bless({$t,$yc,$v,$q,$w,$zc,$y,$z},$A);$Bc=q#is_circular#;$Cc=[];$Dc=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Ec=bless({$t,$Cc,$v,$q,$w,$Dc,$y,$z},$A);$Fc={$tc,$wc,$xc,$Ac,$Bc,$Ec};$Gc=q#/lib/quote_circular_addressed.b#;$Hc=bless({$T2,$sc,$u4,$q,$v4,$q,$w4,$Fc,$Q,$Gc},$T3);$Ic={};$Jc=q#address#;$Kc=[];$Lc=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Mc=bless({$t,$Kc,$v,$q,$w,$Lc,$y,$z},$A);$Nc=q#allocate_gensym#;$Oc=[];$Pc=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Qc=bless({$t,$Oc,$v,$q,$w,$Pc,$y,$z},$A);$Rc=q#circular_links#;$Sc=[];$Tc=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Uc=bless({$t,$Sc,$v,$q,$w,$Tc,$y,$z},$A);$Vc=q#quote#;$Wc=[];$Xc=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Yc=bless({$t,$Wc,$v,$q,$w,$Xc,$y,$z},$A);$Zc={$Jc,$Mc,$Nc,$Qc,$Rc,$Uc,$Vc,$Yc};$cd=q#/lib/quote_gensym_identity.b#;$dd=bless({$T2,$Ic,$u4,$q,$v4,$q,$w4,$Zc,$Q,$cd},$T3);$ed={};$fd=q#gensym#;$gd=[];$hd=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$id=bless({$t,$gd,$v,$q,$w,$hd,$y,$z},$A);$jd={$fd,$id};$kd=q#/lib/gensym_generator_compact.b#;$ld=bless({$T2,$ed,$u4,$q,$v4,$q,$w4,$jd,$Q,$kd},$T3);$md=[$H4,$Ta,$tb,$Bb,$Zb,$rc,$Hc,$dd,$ld];$nd=bless({$T2,$La,$Q,$Ma,$i3,$md},$M3);$od=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$pd=[$od];$qd=bless({$S2,$nd,$E,$pd},$H);$rd=[$R2,$qd];$sd=[$Ia,$Ka,$rd];$td=bless({$e,$sd,$Q,$Ma},$S);$ud=q#ni.doc:/lib/ni#;$vd=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$wd=[$f,$vd];$xd=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$yd=[$i,$xd];$zd=[$wd,$yd];$Ad=q#/lib/ni#;$Bd=bless({$e,$zd,$Q,$Ad},$S);$Cd=q#ni.doc:/lib/quote_simple#;$Dd=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$Ed=[];$Fd=[];$Gd=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$Hd=bless({$t,$Fd,$v,$q,$w,$Gd,$y,$z},$A);$Id=bless({$n,$Ed,$p,$q,$r,$q,$s,$Hd},$C);$Jd=[$i,$Dd,$Id];$Kd=[$Jd];$Ld=q#/lib/quote_simple#;$Md=bless({$e,$Kd,$Q,$Ld},$S);$Nd=q#ni.doc:/lib/slice#;$Od=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$Pd=[$f,$Od];$Qd={$T3,1};$Rd=q#/lib/slice#;$Sd=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$Td=bless({$w,$Sd,$y,$z},$A);$Ud=q#local $_;
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
$self;#;$Vd=bless({$w,$Ud,$y,$z},$A);$Wd=q#ni::/lib/slice::apply#;$Xd=q#ni::/lib/slice::apply_#;$Yd={};$Zd={$J8,$Td,$M8,$Vd};$ce=q#/lib/slice.b#;$de=bless({$T2,$Yd,$w4,$Zd,$Q,$ce},$T3);$ee={};$fe=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$ge=bless({$w,$fe,$y,$z},$A);$he={$K6,$ge};$ie=q#/lib/slice_init.b#;$je=bless({$T2,$ee,$w4,$he,$Q,$ie},$T3);$ke=[$B8,$H8,$de,$je,$C9];$le=bless({$T2,$Qd,$Q,$Rd,$i3,$ke},$U3);$me=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$ne=[$me];$oe=bless({$S2,$le,$E,$ne},$H);$pe=[$R2,$oe];$qe=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$re=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$se=[];$te=[];$ue=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$ve=bless({$t,$te,$v,$q,$w,$ue,$y,$z},$A);$we=bless({$n,$se,$p,$q,$r,$q,$s,$ve},$C);$xe=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$ye=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$ze=[];$Ae=[];$Be=q#my $instances = 0;
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
now $instances == 0;#;$Ce=bless({$t,$Ae,$v,$q,$w,$Be,$y,$z},$A);$De=bless({$n,$ze,$p,$q,$r,$q,$s,$Ce},$C);$Ee=[$i,$qe,$re,$we,$xe,$ye,$De];$Fe=[$Pd,$pe,$Ee];$Ge=bless({$e,$Fe,$Q,$Rd},$S);$He=q#ni.doc:/semantic#;$Ie=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$Je=[$i,$Ie];$Ke=[$Je];$Le=q#/semantic#;$Me=bless({$e,$Ke,$Q,$Le},$S);$Ne=q#ni:/class#;$Oe={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Pe={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Qe=q#/module#;$Re=q#/lib/perlbranch.b#;$Se={};$Te=q#add#;$Ue=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Ve=bless({$w,$Ue,$y,$z},$A);$We=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Xe=bless({$w,$We,$y,$z},$A);$Ye={$Te,$Ve,$J8,$Xe};$Ze=q#/lib/branch.b#;$cf=bless({$T2,$Se,$u4,$q,$v4,$q,$w4,$Ye,$Q,$Ze},$T3);$df=[$cf,$H8,$g9,$n9,$u9];$ef=bless({$Q,$Re,$i3,$df},$V3);$ff={};$gf=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$hf=bless({$w,$gf,$y,$z},$A);$if={$K6,$hf};$jf=q#/lib/class_init.b#;$kf=bless({$T2,$ff,$u4,$J4,$v4,$q,$w4,$if,$Q,$jf},$T3);$lf={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$mf=q#/lib/definition.b#;$nf={};$of=q#def#;$pf=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$qf=bless({$w,$pf,$y,$z},$A);$rf={$of,$qf};$sf=q#/lib/definition_def.b#;$tf=bless({$T2,$nf,$u4,$q,$v4,$q,$w4,$rf,$Q,$sf},$T3);$uf={};$vf=q#ro#;$wf=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$xf=bless({$w,$wf,$y,$z},$A);$yf=q#rw#;$zf=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$Af=bless({$w,$zf,$y,$z},$A);$Bf={$vf,$xf,$yf,$Af};$Cf=q#/lib/accessor.b#;$Df=bless({$T2,$uf,$u4,$q,$v4,$q,$w4,$Bf,$Q,$Cf},$T3);$Ef={};$Ff=q#(""#;$Gf=q#shift->name#;$Hf=bless({$w,$Gf,$y,$z},$A);$If={$Ff,$Hf};$Jf=q#/lib/name_as_string.b#;$Kf=bless({$T2,$Ef,$u4,$q,$v4,$q,$w4,$If,$Q,$Jf},$T3);$Lf={};$Mf=q#(eq#;$Nf=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Of=bless({$w,$Nf,$y,$z},$A);$Pf={$Mf,$Of};$Qf=q#/lib/ref_eq.b#;$Rf=bless({$T2,$Lf,$u4,$q,$v4,$q,$w4,$Pf,$Q,$Qf},$T3);$Sf={};$Tf=q#defdata#;$Uf=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$Vf=bless({$w,$Uf,$y,$z},$A);$Wf={$Tf,$Vf};$Xf=q#/lib/definition_defdata.b#;$Yf=bless({$T2,$Sf,$u4,$q,$v4,$q,$w4,$Wf,$Q,$Xf},$T3);$Zf={};$cg=q#instantiate_with_defaults#;$dg=q#my ($class, @slots) = @_;
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
    }));#;$eg=bless({$w,$dg,$y,$z},$A);$fg={$cg,$eg};$gg=q#/lib/definition_init_with_defaults.b#;$hg=bless({$T2,$Zf,$u4,$q,$v4,$q,$w4,$fg,$Q,$gg},$T3);$ig=[$tf,$Df,$Kf,$Rf,$Yf,$hg];$jg=bless({$T2,$lf,$Q,$mf,$i3,$ig},$D3);$kg=[$ef,$kf,$H4,$B8,$jg];$lg=bless({$T2,$Pe,$Q,$Qe,$i3,$kg},$k4);$mg={};$ng=q#new#;$og=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$pg=bless({$w,$og,$y,$z},$A);$qg={$ng,$pg};$rg=q#/lib/instantiable.b#;$sg=bless({$T2,$mg,$w4,$qg,$Q,$rg},$T3);$tg={};$ug=q#child#;$vg=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$wg=bless({$w,$vg,$y,$z},$A);$xg={$ug,$wg};$yg=q#/lib/subclass.b#;$zg=bless({$T2,$tg,$u4,$q,$v4,$q,$w4,$xg,$Q,$yg},$T3);$Ag=[$lg,$sg,$kf,$lg,$zg];$Bg=bless({$T2,$Oe,$Q,$R,$i3,$Ag},$k3);$Cg=q#ni:/class.c#;$Dg={$k3,1,$o4,1};$Eg=q#/class.c#;$Fg={$k3,1,$k4,1,$o4,1};$Gg=q#/module.c#;$Hg={$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$k4,1,$m4,1,$o4,1,$q4,1};$Ig=q#/object.c#;$Jg=[$Bg];$Kg=bless({$T2,$Hg,$Q,$Ig,$i3,$Jg},$h4);$Lg={$k3,1,$C3,1,$E3,1,$G3,1,$U3,1,$W3,1,$k4,1,$o4,1};$Mg=q#/lib/behavior.c#;$Ng=[$Kg];$Og=bless({$T2,$Lg,$Q,$Mg,$i3,$Ng},$h4);$Pg=[$Kg,$sg,$Og];$Qg=bless({$T2,$Fg,$Q,$Gg,$i3,$Pg},$h4);$Rg=[$Qg];$Sg=bless({$T2,$Dg,$Q,$Eg,$i3,$Rg},$h4);$Tg=q#ni:/fabric#;$Ug=q#ni::/fabric#;$Vg={$Ug,1};$Wg=[];$Xg=bless({$T2,$Vg,$Q,$e1,$i3,$Wg},$j4);$Yg=q#ni:/io#;$Zg=q#ni::/io#;$ch={$Zg,1};$dh=[];$eh=bless({$T2,$ch,$Q,$k1,$i3,$dh},$j4);$fh=q#ni:/io/buffer#;$gh={$U2,1};$hh={};$ih=[];$jh=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$kh=bless({$t,$ih,$v,$q,$w,$jh,$y,$z},$A);$lh={$K6,$kh};$mh=q#/io/buffer_init.b#;$nh=bless({$T2,$hh,$u4,$q,$v4,$q,$w4,$lh,$Q,$mh},$T3);$oh={};$ph=[];$qh=q#my $self = shift;
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
}#;$rh=bless({$t,$ph,$v,$q,$w,$qh,$y,$z},$A);$sh=q#read_capacity#;$th=[];$uh=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$vh=bless({$t,$th,$v,$q,$w,$uh,$y,$z},$A);$wh=[];$xh=q#my $self = shift;
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
}#;$yh=bless({$t,$wh,$v,$q,$w,$xh,$y,$z},$A);$zh=q#write_capacity#;$Ah=[];$Bh=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Ch=bless({$t,$Ah,$v,$q,$w,$Bh,$y,$z},$A);$Dh={$g7,$rh,$sh,$vh,$k7,$yh,$zh,$Ch};$Eh=q#/io/buffer_io.b#;$Fh=bless({$T2,$oh,$u4,$q,$v4,$q,$w4,$Dh,$Q,$Eh},$T3);$Gh=[$i6,$nh,$Fh];$Hh=bless({$T2,$gh,$Q,$x1,$i3,$Gh},$l3);$Ih=q#ni:/io/buffer.c#;$Jh={$l3,1};$Kh=q#/io/buffer.c#;$Lh={$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1};$Mh=q#/io/object.c#;$Nh={};$Oh=q#def_transfer_method#;$Ph=[];$Qh=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Rh=bless({$t,$Ph,$v,$q,$w,$Qh,$y,$z},$A);$Sh={$Oh,$Rh};$Th=q#/io/object.c_transfer_def.b#;$Uh=bless({$T2,$Nh,$u4,$q,$v4,$q,$w4,$Sh,$Q,$Th},$T3);$Vh=[$Kg,$Uh];$Wh=bless({$T2,$Lh,$Q,$Mh,$i3,$Vh},$h4);$Xh=[$Wh];$Yh=bless({$T2,$Jh,$Q,$Kh,$i3,$Xh},$h4);$Zh=q#ni:/io/buffer_init.b#;$ci=q#ni:/io/buffer_io.b#;$di=q#ni:/io/cat#;$ei={$V2,1};$fi={};$gi=[];$hi=q#shift; +{fs => [@_]}#;$ii=bless({$t,$gi,$v,$q,$w,$hi,$y,$z},$A);$ji={$K6,$ii};$ki=q#/io/cat_init.b#;$li=bless({$T2,$fi,$u4,$q,$v4,$q,$w4,$ji,$Q,$ki},$T3);$mi={};$ni=[];$oi=q#my $fs = shift->{fs};
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
$total_read;#;$pi=bless({$t,$ni,$v,$q,$w,$oi,$y,$z},$A);$qi={$g7,$pi};$ri=q#/io/cat_read.b#;$si=bless({$T2,$mi,$u4,$q,$v4,$q,$w4,$qi,$Q,$ri},$T3);$ti=[$i6,$li,$si];$ui=bless({$T2,$ei,$Q,$K1,$i3,$ti},$m3);$vi=q#ni:/io/cat.c#;$wi={$m3,1};$xi=q#/io/cat.c#;$yi=[$Wh];$zi=bless({$T2,$wi,$Q,$xi,$i3,$yi},$h4);$Ai=q#ni:/io/cat_init.b#;$Bi=q#ni:/io/cat_read.b#;$Ci=q#ni:/io/exec#;$Di={$W2,1};$Ei={};$Fi=q#argv#;$Gi=[];$Hi=q#shift->{'argv'}#;$Ii=bless({$t,$Gi,$v,$q,$w,$Hi,$y,$z},$A);$Ji={$Fi,$Ii};$Ki=q#/io/exec_ro.b#;$Li=bless({$T2,$Ei,$u4,$q,$v4,$q,$w4,$Ji,$Q,$Ki},$T3);$Mi={};$Ni=[];$Oi=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Pi=bless({$t,$Ni,$v,$q,$w,$Oi,$y,$z},$A);$Qi={$K6,$Pi};$Ri=q#/io/exec_init.b#;$Si=bless({$T2,$Mi,$u4,$q,$v4,$q,$w4,$Qi,$Q,$Ri},$T3);$Ti={};$Ui=q#connect#;$Vi=[];$Wi=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Xi=bless({$t,$Vi,$v,$q,$w,$Wi,$y,$z},$A);$Yi=q#in_pipe#;$Zi=[];$cj=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$dj=bless({$t,$Zi,$v,$q,$w,$cj,$y,$z},$A);$ej=q#out_pipe#;$fj=[];$gj=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$hj=bless({$t,$fj,$v,$q,$w,$gj,$y,$z},$A);$ij=q#setup_stdio#;$jj=[];$kj=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$lj=bless({$t,$jj,$v,$q,$w,$kj,$y,$z},$A);$mj={$Ui,$Xi,$Yi,$dj,$ej,$hj,$ij,$lj};$nj=q#/io/exec_io_setup.b#;$oj=bless({$T2,$Ti,$u4,$q,$v4,$q,$w4,$mj,$Q,$nj},$T3);$pj={};$qj=q#binds_fd#;$rj=[];$sj=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$tj=bless({$t,$rj,$v,$q,$w,$sj,$y,$z},$A);$uj=[];$vj=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$wj=bless({$t,$uj,$v,$q,$w,$vj,$y,$z},$A);$xj=[];$yj=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$zj=bless({$t,$xj,$v,$q,$w,$yj,$y,$z},$A);$Aj=[];$Bj=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Cj=bless({$t,$Aj,$v,$q,$w,$Bj,$y,$z},$A);$Dj=[];$Ej=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Fj=bless({$t,$Dj,$v,$q,$w,$Ej,$y,$z},$A);$Gj={$qj,$tj,$s7,$wj,$w7,$zj,$A7,$Cj,$E7,$Fj};$Hj=q#/io/exec_io_accessors.b#;$Ij=bless({$T2,$pj,$u4,$q,$v4,$q,$w4,$Gj,$Q,$Hj},$T3);$Jj={};$Kj=q#env#;$Lj=[];$Mj=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Nj=bless({$t,$Lj,$v,$q,$w,$Mj,$y,$z},$A);$Oj={$Kj,$Nj};$Pj=q#/io/exec_env.b#;$Qj=bless({$T2,$Jj,$u4,$q,$v4,$q,$w4,$Oj,$Q,$Pj},$T3);$Rj={};$Sj=q#exec#;$Tj=[];$Uj=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Vj=bless({$t,$Tj,$v,$q,$w,$Uj,$y,$z},$A);$Wj=q#fork#;$Xj=[];$Yj=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Zj=bless({$t,$Xj,$v,$q,$w,$Yj,$y,$z},$A);$ck=q#move_fds#;$dk=[];$ek=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$fk=bless({$t,$dk,$v,$q,$w,$ek,$y,$z},$A);$gk={$Sj,$Vj,$Wj,$Zj,$ck,$fk};$hk=q#/io/exec_fork.b#;$ik=bless({$T2,$Rj,$u4,$q,$v4,$q,$w4,$gk,$Q,$hk},$T3);$jk=[$i6,$Li,$Si,$oj,$Ij,$Qj,$ik];$kk=bless({$T2,$Di,$Q,$X1,$i3,$jk},$n3);$lk=q#ni:/io/exec.c#;$mk={$n3,1};$nk=q#/io/exec.c#;$ok=[$Wh];$pk=bless({$T2,$mk,$Q,$nk,$i3,$ok},$h4);$qk=q#ni:/io/exec_env.b#;$rk=q#ni:/io/exec_fork.b#;$sk=q#ni:/io/exec_init.b#;$tk=q#ni:/io/exec_io_accessors.b#;$uk=q#ni:/io/exec_io_setup.b#;$vk=q#ni:/io/exec_ro.b#;$wk=q#ni:/io/fd#;$xk={$X2,1};$yk=q#read_fd_mask#;$zk={};$Ak=[];$Bk=q#shift->{'fd'}#;$Ck=bless({$t,$Ak,$v,$q,$w,$Bk,$y,$z},$A);$Dk={$s7,$Ck};$Ek=q#/io/fd_readers.b#;$Fk=bless({$T2,$zk,$u4,$q,$v4,$q,$w4,$Dk,$Q,$Ek},$T3);$Gk={};$Hk=[];$Ik=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Jk=bless({$t,$Hk,$v,$q,$w,$Ik,$y,$z},$A);$Kk={$K6,$Jk};$Lk=q#/io/fd_init.b#;$Mk=bless({$T2,$Gk,$u4,$q,$v4,$q,$w4,$Kk,$Q,$Lk},$T3);$Nk={};$Ok=q#be#;$Pk=[];$Qk=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Rk=bless({$t,$Pk,$v,$q,$w,$Qk,$y,$z},$A);$Sk={$Ok,$Rk};$Tk=q#/io/fd_shell.b#;$Uk=bless({$T2,$Nk,$u4,$q,$v4,$q,$w4,$Sk,$Q,$Tk},$T3);$Vk={};$Wk=q#cloexec#;$Xk=[];$Yk=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Zk=bless({$t,$Xk,$v,$q,$w,$Yk,$y,$z},$A);$cl=q#fcntl_flag#;$dl=[];$el=q#my ($self, $flag, $value) = @_;
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
}#;$fl=bless({$t,$dl,$v,$q,$w,$el,$y,$z},$A);$gl=q#nonblock#;$hl=[];$il=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$jl=bless({$t,$hl,$v,$q,$w,$il,$y,$z},$A);$kl={$Wk,$Zk,$cl,$fl,$gl,$jl};$ll=q#/io/fd_fcntl.b#;$ml=bless({$T2,$Vk,$u4,$q,$v4,$q,$w4,$kl,$Q,$ll},$T3);$nl={};$ol=[];$pl=q#shift->close#;$ql=bless({$t,$ol,$v,$q,$w,$pl,$y,$z},$A);$rl=q#close#;$sl=[];$tl=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$ul=bless({$t,$sl,$v,$q,$w,$tl,$y,$z},$A);$vl={$rl,$ul};$wl=q#/io/fd_gc.b#;$xl=bless({$T2,$nl,$u4,$q,$v4,$ql,$w4,$vl,$Q,$wl},$T3);$yl={};$zl=q#close_perl_ios#;$Al=[];$Bl=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Cl=bless({$t,$Al,$v,$q,$w,$Bl,$y,$z},$A);$Dl=[];$El=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Fl=bless({$t,$Dl,$v,$q,$w,$El,$y,$z},$A);$Gl=[];$Hl=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Il=bless({$t,$Gl,$v,$q,$w,$Hl,$y,$z},$A);$Jl={$zl,$Cl,$g7,$Fl,$k7,$Il};$Kl=q#/io/fd_perlio.b#;$Ll=bless({$T2,$yl,$u4,$q,$v4,$q,$w4,$Jl,$Q,$Kl},$T3);$Ml=[$i6,$Fk,$Mk,$Uk,$ml,$xl,$Ll];$Nl=q#write_fd_mask#;$Ol=bless({$T2,$xk,$Q,$m2,$yk,$z,$i3,$Ml,$Nl,$z},$o3);$Pl=[];$Ql=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Rl=bless({$t,$Pl,$v,$q,$w,$Ql,$y,$z},$A);$Sl=q#ni:/io/fd.c#;$Tl={$o3,1};$Ul=q#/io/fd.c#;$Vl={};$Wl=q#clear_fd#;$Xl=[];$Yl=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Zl=bless({$t,$Xl,$v,$q,$w,$Yl,$y,$z},$A);$cm=q#read_fd#;$dm=[];$em=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$fm=bless({$t,$dm,$v,$q,$w,$em,$y,$z},$A);$gm=q#select#;$hm=[];$im=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$jm=bless({$t,$hm,$v,$q,$w,$im,$y,$z},$A);$km=q#write_fd#;$lm=[];$mm=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$nm=bless({$t,$lm,$v,$q,$w,$mm,$y,$z},$A);$om={$Wl,$Zl,$cm,$fm,$gm,$jm,$km,$nm};$pm=q#/io/fd.c_selector.b#;$qm=bless({$T2,$Vl,$u4,$Rl,$v4,$q,$w4,$om,$Q,$pm},$T3);$rm=[$Wh,$qm];$sm=bless({$T2,$Tl,$Q,$Ul,$i3,$rm},$h4);$tm=q#ni:/io/fd.c_selector.b#;$um=q#ni:/io/fd_fcntl.b#;$vm=q#ni:/io/fd_gc.b#;$wm=q#ni:/io/fd_init.b#;$xm=q#ni:/io/fd_perlio.b#;$ym=q#ni:/io/fd_readers.b#;$zm=q#ni:/io/fd_shell.b#;$Am=q#ni:/io/file#;$Bm={$Y2,1};$Cm={};$Dm=[];$Em=q#shift->{'name'}#;$Fm=bless({$t,$Dm,$v,$q,$w,$Em,$y,$z},$A);$Gm={$Q,$Fm};$Hm=q#/io/file_readers.b#;$Im=bless({$T2,$Cm,$u4,$q,$v4,$q,$w4,$Gm,$Q,$Hm},$T3);$Jm={};$Km=q#mode#;$Lm=[];$Mm=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$Nm=bless({$t,$Lm,$v,$q,$w,$Mm,$y,$z},$A);$Om={$Km,$Nm};$Pm=q#/io/file_accessors.b#;$Qm=bless({$T2,$Jm,$u4,$q,$v4,$q,$w4,$Om,$Q,$Pm},$T3);$Rm={};$Sm=[];$Tm=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Um=bless({$t,$Sm,$v,$q,$w,$Tm,$y,$z},$A);$Vm={$K6,$Um};$Wm=q#/io/file_init.b#;$Xm=bless({$T2,$Rm,$u4,$q,$v4,$q,$w4,$Vm,$Q,$Wm},$T3);$Ym={};$Zm=q#(-X#;$cn=[];$dn=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$en=bless({$t,$cn,$v,$q,$w,$dn,$y,$z},$A);$fn=q#mv#;$gn=[];$hn=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$in=bless({$t,$gn,$v,$q,$w,$hn,$y,$z},$A);$jn=q#rm#;$kn=[];$ln=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$mn=bless({$t,$kn,$v,$q,$w,$ln,$y,$z},$A);$nn={$Zm,$en,$fn,$in,$jn,$mn};$on=q#/io/file_fns.b#;$pn=bless({$T2,$Ym,$u4,$q,$v4,$q,$w4,$nn,$Q,$on},$T3);$qn={};$rn=q#atomic_update#;$sn=[];$tn=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$un=bless({$t,$sn,$v,$q,$w,$tn,$y,$z},$A);$vn={$rn,$un};$wn=q#/io/file_update.b#;$xn=bless({$T2,$qn,$u4,$q,$v4,$q,$w4,$vn,$Q,$wn},$T3);$yn={};$zn=[];$An=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Bn=bless({$t,$zn,$v,$q,$w,$An,$y,$z},$A);$Cn=q#r#;$Dn=[];$En=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Fn=bless({$t,$Dn,$v,$q,$w,$En,$y,$z},$A);$Gn=[];$Hn=q#shift->r->read(@_)#;$In=bless({$t,$Gn,$v,$q,$w,$Hn,$y,$z},$A);$Jn=q#w#;$Kn=[];$Ln=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Mn=bless({$t,$Kn,$v,$q,$w,$Ln,$y,$z},$A);$Nn=[];$On=q#shift->w->write(@_)#;$Pn=bless({$t,$Nn,$v,$q,$w,$On,$y,$z},$A);$Qn={$rl,$Bn,$Cn,$Fn,$g7,$In,$Jn,$Mn,$k7,$Pn};$Rn=q#/io/file_io.b#;$Sn=bless({$T2,$yn,$u4,$q,$v4,$q,$w4,$Qn,$Q,$Rn},$T3);$Tn=[$i6,$Im,$Qm,$Xm,$pn,$xn,$Sn];$Un=bless({$T2,$Bm,$Q,$I2,$i3,$Tn},$p3);$Vn=q#ni:/io/file.c#;$Wn={$p3,1};$Xn=q#/io/file.c#;$Yn=[$Wh];$Zn=bless({$T2,$Wn,$Q,$Xn,$i3,$Yn},$h4);$co=q#ni:/io/file_accessors.b#;$do=q#ni:/io/file_fns.b#;$eo=q#ni:/io/file_init.b#;$fo=q#ni:/io/file_io.b#;$go=q#ni:/io/file_readers.b#;$ho=q#ni:/io/file_update.b#;$io=q#ni:/io/file_update_fd#;$jo={$Z2,1};$ko={};$lo=[];$mo=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$no=bless({$t,$lo,$v,$q,$w,$mo,$y,$z},$A);$oo={$K6,$no};$po=q#/io/file_update_fd_fd_init.b#;$qo=bless({$T2,$ko,$u4,$q,$v4,$q,$w4,$oo,$Q,$po},$T3);$ro={};$so=[];$to=bless({$t,$so,$v,$q,$w,$pl,$y,$z},$A);$uo=[];$vo=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$wo=bless({$t,$uo,$v,$q,$w,$vo,$y,$z},$A);$xo={$rl,$wo};$yo=q#/io/file_update_fd_fd_gc.b#;$zo=bless({$T2,$ro,$u4,$q,$v4,$to,$w4,$xo,$Q,$yo},$T3);$Ao=[$i6,$Fk,$ml,$Ll,$qo,$zo];$Bo=bless({$T2,$jo,$Q,$O2,$i3,$Ao},$q3);$Co=q#ni:/io/file_update_fd.c#;$Do={$q3,1};$Eo=q#/io/file_update_fd.c#;$Fo=[$Wh];$Go=bless({$T2,$Do,$Q,$Eo,$i3,$Fo},$h4);$Ho=q#ni:/io/file_update_fd_fd_gc.b#;$Io=q#ni:/io/file_update_fd_fd_init.b#;$Jo=q#ni:/io/named_io_fns.b#;$Ko={};$Lo=q#fcntl#;$Mo=[];$No=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Oo=bless({$t,$Mo,$v,$q,$w,$No,$y,$z},$A);$Po=[];$Qo=q#CORE::fork#;$Ro=bless({$t,$Po,$v,$q,$w,$Qo,$y,$z},$A);$So=q#open2#;$To=[];$Uo=q#CORE::open $_[0], $_[1]#;$Vo=bless({$t,$To,$v,$q,$w,$Uo,$y,$z},$A);$Wo=q#rename#;$Xo=[];$Yo=q#CORE::rename $_[0], $_[1]#;$Zo=bless({$t,$Xo,$v,$q,$w,$Yo,$y,$z},$A);$cp=q#unlink#;$dp=[];$ep=q#CORE::unlink @_#;$fp=bless({$t,$dp,$v,$q,$w,$ep,$y,$z},$A);$gp=q#waitpid#;$hp=[];$ip=q#CORE::waitpid $_[0], $_[1]#;$jp=bless({$t,$hp,$v,$q,$w,$ip,$y,$z},$A);$kp={$Lo,$Oo,$Wj,$Ro,$So,$Vo,$Wo,$Zo,$cp,$fp,$gp,$jp};$lp=q#/io/named_io_fns.b#;$mp=bless({$T2,$Ko,$u4,$q,$v4,$q,$w4,$kp,$Q,$lp},$T3);$np=q#main#;$op=q#ni:/io/null#;$pp={$c3,1};$qp=q#/io/null#;$rp={};$sp=[];$tp=q#+{fd => undef}#;$up=bless({$t,$sp,$v,$q,$w,$tp,$y,$z},$A);$vp={$K6,$up};$wp=q#/io/null_init.b#;$xp=bless({$T2,$rp,$u4,$q,$v4,$q,$w4,$vp,$Q,$wp},$T3);$yp={};$zp=[];$Ap=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Bp=bless({$t,$zp,$v,$q,$w,$Ap,$y,$z},$A);$Cp=[];$Dp=q#shift->fd->read(@_)#;$Ep=bless({$t,$Cp,$v,$q,$w,$Dp,$y,$z},$A);$Fp=[];$Gp=q#shift->fd->write(@_)#;$Hp=bless({$t,$Fp,$v,$q,$w,$Gp,$y,$z},$A);$Ip={$s7,$Bp,$g7,$Ep,$k7,$Hp};$Jp=q#/io/null_io.b#;$Kp=bless({$T2,$yp,$u4,$q,$v4,$q,$w4,$Ip,$Q,$Jp},$T3);$Lp=[$i6,$xp,$Kp];$Mp=bless({$T2,$pp,$Q,$qp,$i3,$Lp},$r3);$Np=q#ni:/io/null.c#;$Op={$r3,1};$Pp=q#/io/null.c#;$Qp=[$Wh];$Rp=bless({$T2,$Op,$Q,$Pp,$i3,$Qp},$h4);$Sp=q#ni:/io/null_init.b#;$Tp=q#ni:/io/null_io.b#;$Up=q#ni:/io/object#;$Vp=q#ni:/io/object.c#;$Wp=q#ni:/io/object.c_transfer_def.b#;$Xp=q#ni:/io/object_checks.b#;$Yp=q#ni:/io/object_constructors.b#;$Zp=q#ni:/io/object_memory.b#;$cq=q#ni:/io/object_ops.b#;$dq=q#ni:/io/object_transfer_async.b#;$eq=q#ni:/io/object_transfer_sync.b#;$fq=q#ni:/io/pid#;$gq=q#ni:/io/pid.c#;$hq={$t3,1};$iq=q#/io/pid.c#;$jq=[$Wh];$kq=bless({$T2,$hq,$Q,$iq,$i3,$jq},$h4);$lq=q#ni:/io/pid_accessors.b#;$mq=q#ni:/io/pid_init.b#;$nq=q#ni:/io/pid_io.b#;$oq=q#ni:/io/pid_readers.b#;$pq=q#ni:/io/pid_wait.b#;$qq=q#ni:/io/str#;$rq={$f3,1};$sq=q#/io/str#;$tq={};$uq=q#data#;$vq=[];$wq=q#shift->{'data'}#;$xq=bless({$t,$vq,$v,$q,$w,$wq,$y,$z},$A);$yq=q#end#;$zq=[];$Aq=q#shift->{'end'}#;$Bq=bless({$t,$zq,$v,$q,$w,$Aq,$y,$z},$A);$Cq=q#start#;$Dq=[];$Eq=q#shift->{'start'}#;$Fq=bless({$t,$Dq,$v,$q,$w,$Eq,$y,$z},$A);$Gq={$uq,$xq,$yq,$Bq,$Cq,$Fq};$Hq=q#/io/str_ro.b#;$Iq=bless({$T2,$tq,$u4,$q,$v4,$q,$w4,$Gq,$Q,$Hq},$T3);$Jq={};$Kq=[];$Lq=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Mq=bless({$t,$Kq,$v,$q,$w,$Lq,$y,$z},$A);$Nq={$K6,$Mq};$Oq=q#/io/str_init.b#;$Pq=bless({$T2,$Jq,$u4,$q,$v4,$q,$w4,$Nq,$Q,$Oq},$T3);$Qq={};$Rq=[];$Sq=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Tq=bless({$t,$Rq,$v,$q,$w,$Sq,$y,$z},$A);$Uq=q#remaining#;$Vq=[];$Wq=q#my $self = shift; $$self{end} - $$self{start}#;$Xq=bless({$t,$Vq,$v,$q,$w,$Wq,$y,$z},$A);$Yq=[];$Zq=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$cr=bless({$t,$Yq,$v,$q,$w,$Zq,$y,$z},$A);$dr={$g7,$Tq,$Uq,$Xq,$k7,$cr};$er=q#/io/str_io.b#;$fr=bless({$T2,$Qq,$u4,$q,$v4,$q,$w4,$dr,$Q,$er},$T3);$gr=[$i6,$Iq,$Pq,$fr];$hr=bless({$T2,$rq,$Q,$sq,$i3,$gr},$u3);$ir=q#ni:/io/str.c#;$jr={$u3,1};$kr=q#/io/str.c#;$lr=[$Wh];$mr=bless({$T2,$jr,$Q,$kr,$i3,$lr},$h4);$nr=q#ni:/io/str_init.b#;$or=q#ni:/io/str_io.b#;$pr=q#ni:/io/str_ro.b#;$qr=q#ni:/io/transfer#;$rr={$v3,1,$x3,1,$z3,1};$sr=q#/io/transfer#;$tr={$v3,1,$x3,1,$z3,1,$p4,1};$ur=q#/semantic/task#;$vr={};$wr=[];$xr=q#shift->{'outcome'}#;$yr=bless({$t,$wr,$v,$q,$w,$xr,$y,$z},$A);$zr={$r,$yr};$Ar=q#/semantic/task_ro.b#;$Br=bless({$T2,$vr,$u4,$q,$v4,$q,$w4,$zr,$Q,$Ar},$T3);$Cr={};$Dr=q#failure#;$Er=[];$Fr=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Gr=bless({$t,$Er,$v,$q,$w,$Fr,$y,$z},$A);$Hr=q#success#;$Ir=[];$Jr=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Kr=bless({$t,$Ir,$v,$q,$w,$Jr,$y,$z},$A);$Lr={$Dr,$Gr,$Hr,$Kr};$Mr=q#/semantic/task_outcome.b#;$Nr=bless({$T2,$Cr,$u4,$q,$v4,$q,$w4,$Lr,$Q,$Mr},$T3);$Or=[$H4,$Br,$Nr];$Pr=bless({$T2,$tr,$Q,$ur,$i3,$Or},$q4);$Qr={};$Rr=[];$Sr=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Tr=bless({$t,$Rr,$v,$q,$w,$Sr,$y,$z},$A);$Ur=[];$Vr=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Wr=bless({$t,$Ur,$v,$q,$w,$Vr,$y,$z},$A);$Xr=[];$Yr=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Zr=bless({$t,$Xr,$v,$q,$w,$Yr,$y,$z},$A);$cs={$g7,$Wr,$k7,$Zr};$ds=q#/io/transfer_io_interop.b#;$es=bless({$T2,$Qr,$u4,$Tr,$v4,$q,$w4,$cs,$Q,$ds},$T3);$fs={};$gs=q#pressure#;$hs=[];$is=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$js=bless({$t,$hs,$v,$q,$w,$is,$y,$z},$A);$ks=q#read_limit_throughput#;$ls=[];$ms=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$ns=bless({$t,$ls,$v,$q,$w,$ms,$y,$z},$A);$os=q#throughput#;$ps=[];$qs=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$rs=bless({$t,$ps,$v,$q,$w,$qs,$y,$z},$A);$ss=q#write_limit_throughput#;$ts=[];$us=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$vs=bless({$t,$ts,$v,$q,$w,$us,$y,$z},$A);$ws={$gs,$js,$ks,$ns,$os,$rs,$ss,$vs};$xs=q#/io/transfer_io_measurement.b#;$ys=bless({$T2,$fs,$u4,$q,$v4,$q,$w4,$ws,$Q,$xs},$T3);$zs=[$Pr,$es,$ys];$As=bless({$T2,$rr,$Q,$sr,$i3,$zs},$w3);$Bs=[];$Cs=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Ds=bless({$t,$Bs,$v,$q,$w,$Cs,$y,$z},$A);$Es=q#ni:/io/transfer.c#;$Fs={$w3,1,$y3,1,$A3,1};$Gs=q#/io/transfer.c#;$Hs={$w3,1,$y3,1,$A3,1,$q4,1};$Is=q#/semantic/task.c#;$Js=[$Kg];$Ks=bless({$T2,$Hs,$Q,$Is,$i3,$Js},$h4);$Ls={};$Ms={};$Ns=q#/io/transfer.c_into.b#;$Os=bless({$T2,$Ls,$u4,$Ds,$v4,$q,$w4,$Ms,$Q,$Ns},$T3);$Ps=[$Ks,$Os];$Qs=bless({$T2,$Fs,$Q,$Gs,$i3,$Ps},$h4);$Rs=q#ni:/io/transfer.c_into.b#;$Ss=q#ni:/io/transfer_async#;$Ts={$x3,1};$Us=q#/io/transfer_async#;$Vs={};$Ws=q#dest_io#;$Xs=[];$Ys=q#shift->{'dest_io'}#;$Zs=bless({$t,$Xs,$v,$q,$w,$Ys,$y,$z},$A);$ct=q#id#;$dt=[];$et=q#shift->{'id'}#;$ft=bless({$t,$dt,$v,$q,$w,$et,$y,$z},$A);$gt=q#source_io#;$ht=[];$it=q#shift->{'source_io'}#;$jt=bless({$t,$ht,$v,$q,$w,$it,$y,$z},$A);$kt={$Ws,$Zs,$ct,$ft,$gt,$jt};$lt=q#/io/transfer_async_ro.b#;$mt=bless({$T2,$Vs,$u4,$q,$v4,$q,$w4,$kt,$Q,$lt},$T3);$nt={};$ot=[];$pt=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$qt=bless({$t,$ot,$v,$q,$w,$pt,$y,$z},$A);$rt={$K6,$qt};$st=q#/io/transfer_async_init.b#;$tt=bless({$T2,$nt,$u4,$q,$v4,$q,$w4,$rt,$Q,$st},$T3);$ut={};$vt=[];$wt=q#ni('ni:/io/transfer_async')->track(shift)#;$xt=bless({$t,$vt,$v,$q,$w,$wt,$y,$z},$A);$yt=[];$zt=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$At=bless({$t,$yt,$v,$q,$w,$zt,$y,$z},$A);$Bt={};$Ct=q#/io/transfer_async_lifecycle.b#;$Dt=bless({$T2,$ut,$u4,$xt,$v4,$At,$w4,$Bt,$Q,$Ct},$T3);$Et={};$Ft=q#run#;$Gt=[];$Ht=q#shift#;$It=bless({$t,$Gt,$v,$q,$w,$Ht,$y,$z},$A);$Jt=q#run_async#;$Kt=[];$Lt=q#my $self = shift;
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

$self;#;$Mt=bless({$t,$Kt,$v,$q,$w,$Lt,$y,$z},$A);$Nt={$Ft,$It,$Jt,$Mt};$Ot=q#/io/transfer_async_run.b#;$Pt=bless({$T2,$Et,$u4,$q,$v4,$q,$w4,$Nt,$Q,$Ot},$T3);$Qt=[$As,$mt,$tt,$Dt,$Pt];$Rt=q#tracked_transfers#;$St={};$Tt=q#transfer_id#;$Ut=bless({$T2,$Ts,$Q,$Us,$i3,$Qt,$Rt,$St,$Tt,0},$y3);$Vt=[];$Wt=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Xt=bless({$t,$Vt,$v,$q,$w,$Wt,$y,$z},$A);$Yt=q#ni:/io/transfer_async.c#;$Zt={$y3,1};$cu=q#/io/transfer_async.c#;$du={};$eu=q#new_id#;$fu=[];$gu=q#++shift->{transfer_id}#;$hu=bless({$t,$fu,$v,$q,$w,$gu,$y,$z},$A);$iu=q#track#;$ju=[];$ku=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$lu=bless({$t,$ju,$v,$q,$w,$ku,$y,$z},$A);$mu=q#untrack#;$nu=[];$ou=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$pu=bless({$t,$nu,$v,$q,$w,$ou,$y,$z},$A);$qu={$eu,$hu,$iu,$lu,$mu,$pu};$ru=q#/io/transfer_async.c_tracker.b#;$su=bless({$T2,$du,$u4,$Xt,$v4,$q,$w4,$qu,$Q,$ru},$T3);$tu=[$Qs,$su];$uu=bless({$T2,$Zt,$Q,$cu,$i3,$tu},$h4);$vu=q#ni:/io/transfer_async.c_tracker.b#;$wu=q#ni:/io/transfer_async_init.b#;$xu=q#ni:/io/transfer_async_lifecycle.b#;$yu=q#ni:/io/transfer_async_ro.b#;$zu=q#ni:/io/transfer_async_run.b#;$Au=q#ni:/io/transfer_io_interop.b#;$Bu=q#ni:/io/transfer_io_measurement.b#;$Cu=q#ni:/io/transfer_sync#;$Du={$z3,1};$Eu=q#/io/transfer_sync#;$Fu={};$Gu=[];$Hu=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Iu=bless({$t,$Gu,$v,$q,$w,$Hu,$y,$z},$A);$Ju={$K6,$Iu};$Ku=q#/io/transfer_sync_init.b#;$Lu=bless({$T2,$Fu,$u4,$q,$v4,$q,$w4,$Ju,$Q,$Ku},$T3);$Mu={};$Nu=[];$Ou=q#my $self = shift;
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
$self->success;#;$Pu=bless({$t,$Nu,$v,$q,$w,$Ou,$y,$z},$A);$Qu={$Ft,$Pu};$Ru=q#/io/transfer_sync_run.b#;$Su=bless({$T2,$Mu,$u4,$q,$v4,$q,$w4,$Qu,$Q,$Ru},$T3);$Tu=[$As,$Lu,$Su];$Uu=bless({$T2,$Du,$Q,$Eu,$i3,$Tu},$A3);$Vu=q#ni:/io/transfer_sync.c#;$Wu={$A3,1};$Xu=q#/io/transfer_sync.c#;$Yu=[$Qs];$Zu=bless({$T2,$Wu,$Q,$Xu,$i3,$Yu},$h4);$cv=q#ni:/io/transfer_sync_init.b#;$dv=q#ni:/io/transfer_sync_run.b#;$ev=q#ni:/lib#;$fv=q#ni::/lib#;$gv={$fv,1};$hv=[];$iv=bless({$T2,$gv,$Q,$n8,$i3,$hv},$j4);$jv=q#ni:/lib/accessor.b#;$kv=q#ni:/lib/behavior#;$lv=q#ni:/lib/behavior.c#;$mv=q#ni:/lib/branch#;$nv={$D3,1};$ov=q#/lib/branch#;$pv={};$qv=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$rv=bless({$w,$qv,$y,$z},$A);$sv={$K6,$rv};$tv=q#/lib/branch_init.b#;$uv=bless({$T2,$pv,$u4,$q,$v4,$q,$w4,$sv,$Q,$tv},$T3);$vv=[$B8,$H8,$cf,$uv,$jg];$wv=bless({$T2,$nv,$Q,$ov,$i3,$vv},$E3);$xv=q#ni:/lib/branch.b#;$yv=q#ni:/lib/branch.c#;$zv={$E3,1};$Av=q#/lib/branch.c#;$Bv=[$Og];$Cv=bless({$T2,$zv,$Q,$Av,$i3,$Bv},$h4);$Dv=q#ni:/lib/branch_init.b#;$Ev=q#ni:/lib/class_init.b#;$Fv=q#ni:/lib/dataslice#;$Gv=q#ni:/lib/dataslice.b#;$Hv=q#ni:/lib/dataslice.c#;$Iv={$G3,1};$Jv=q#/lib/dataslice.c#;$Kv=[$Og];$Lv=bless({$T2,$Iv,$Q,$Jv,$i3,$Kv},$h4);$Mv=q#ni:/lib/dataslice_init.b#;$Nv=q#ni:/lib/definition.b#;$Ov=q#ni:/lib/definition_def.b#;$Pv=q#ni:/lib/definition_defdata.b#;$Qv=q#ni:/lib/definition_init_with_defaults.b#;$Rv=q#ni:/lib/doc#;$Sv={$S,1};$Tv={};$Uv=q#shift; +{name => shift, doc => []}#;$Vv=bless({$w,$Uv,$y,$z},$A);$Wv={$K6,$Vv};$Xv=q#/lib/doc_init.b#;$Yv=bless({$T2,$Tv,$u4,$q,$v4,$q,$w4,$Wv,$Q,$Xv},$T3);$Zv={};$cw=q#'ni.doc'#;$dw=bless({$w,$cw,$y,$z},$A);$ew={$Z8,$dw};$fw=q#/lib/doc_namespace.b#;$gw=bless({$T2,$Zv,$u4,$q,$v4,$q,$w4,$ew,$Q,$fw},$T3);$hw={};$iw=q#(@{}#;$jw=q#[map @$_, @{shift->{doc}}]#;$kw=bless({$w,$jw,$y,$z},$A);$lw=q#AUTOLOAD#;$mw=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$nw=bless({$w,$mw,$y,$z},$A);$ow={$iw,$kw,$lw,$nw};$pw=q#/lib/doc_define.b#;$qw=bless({$T2,$hw,$u4,$q,$v4,$q,$w4,$ow,$Q,$pw},$T3);$rw={};$sw=q#shift->referent#;$tw=bless({$w,$sw,$y,$z},$A);$uw=q#ni 'ni:' . shift->{name}#;$vw=bless({$w,$uw,$y,$z},$A);$ww={$yq,$tw,$S2,$vw};$xw=q#/lib/doc_end.b#;$yw=bless({$T2,$rw,$u4,$q,$v4,$q,$w4,$ww,$Q,$xw},$T3);$zw={};$Aw=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Bw=bless({$w,$Aw,$y,$z},$A);$Cw={$R2,$Bw};$Dw=q#/lib/doc_TODO.b#;$Ew=bless({$T2,$zw,$u4,$q,$v4,$q,$w4,$Cw,$Q,$Dw},$T3);$Fw={};$Gw=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Hw=bless({$w,$Gw,$y,$z},$A);$Iw={$q6,$Hw};$Jw=q#/lib/doc_eg.b#;$Kw=bless({$T2,$Fw,$u4,$q,$v4,$q,$w4,$Iw,$Q,$Jw},$T3);$Lw={};$Mw=q#tests#;$Nw=q#my $self = shift;
my $test_case_class = ni('ni:/lib/test_case')->package;
map $_->referent($self->referent), grep ref($_) eq $test_case_class,
    map @$_, @{$$self{doc}};#;$Ow=bless({$w,$Nw,$y,$z},$A);$Pw=q#todos#;$Qw=q#my $self = shift;
my $todo_class = ni('ni:/lib/todo')->package;
map $_->referent($self->referent), grep ref($_) eq $todo_class,
    map @$_, @{$$self{doc}};#;$Rw=bless({$w,$Qw,$y,$z},$A);$Sw={$Mw,$Ow,$Pw,$Rw};$Tw=q#/lib/doc_process.b#;$Uw=bless({$T2,$Lw,$u4,$q,$v4,$q,$w4,$Sw,$Q,$Tw},$T3);$Vw=[$H4,$H8,$Yv,$gw,$qw,$yw,$Ew,$Kw,$Uw];$Ww=bless({$T2,$Sv,$Q,$fa,$i3,$Vw},$H3);$Xw=q#ni:/lib/doc.c#;$Yw={$H3,1};$Zw=q#/lib/doc.c#;$cx={};$dx=q#defannotation#;$ex=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$fx=bless({$w,$ex,$y,$z},$A);$gx={$dx,$fx};$hx=q#/lib/doc.c_defannotation.b#;$ix=bless({$T2,$cx,$u4,$q,$v4,$q,$w4,$gx,$Q,$hx},$T3);$jx=[$Kg,$ix];$kx=bless({$T2,$Yw,$Q,$Zw,$i3,$jx},$h4);$lx=q#ni:/lib/doc.c_defannotation.b#;$mx=q#ni:/lib/doc_TODO.b#;$nx=q#ni:/lib/doc_define.b#;$ox=q#ni:/lib/doc_eg.b#;$px=q#ni:/lib/doc_end.b#;$qx=q#ni:/lib/doc_init.b#;$rx=q#ni:/lib/doc_namespace.b#;$sx=q#ni:/lib/doc_process.b#;$tx=q#ni:/lib/documentable.b#;$ux=q#ni:/lib/fn#;$vx={$A,1};$wx=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$xx=bless({$w,$wx},$A);$yx=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  annotations => [@_]};#;$zx=bless({$w,$yx},$A);$Ax=q#ni::/lib/fn::compile#;$Bx=q#ni::/lib/fn::instantiate#;$Cx={};$Dx=q#compile#;$Ex={$Dx,$xx,$K6,$zx};$Fx=q#/lib/fn_init.b#;$Gx=bless({$T2,$Cx,$u4,$q,$v4,$q,$w4,$Ex,$Q,$Fx},$T3);$Hx={};$Ix=[];$Jx=q#shift->{'annotations'}#;$Kx=bless({$t,$Ix,$v,$q,$w,$Jx,$y,$z},$A);$Lx=[];$Mx=q#shift->{'code'}#;$Nx=bless({$t,$Lx,$v,$q,$w,$Mx,$y,$z},$A);$Ox=q#fn#;$Px=[];$Qx=q#shift->{'fn'}#;$Rx=bless({$t,$Px,$v,$q,$w,$Qx,$y,$z},$A);$Sx={$t,$Kx,$w,$Nx,$Ox,$Rx};$Tx=q#/lib/fn_ro.b#;$Ux=bless({$T2,$Hx,$u4,$q,$v4,$q,$w4,$Sx,$Q,$Tx},$T3);$Vx={};$Wx=[];$Xx=q#if (@_ == 2) {
  $_[0]->{'closure'} = $_[1];
  return $_[0];
} else {
  return shift->{'closure'};
}#;$Yx=bless({$t,$Wx,$v,$q,$w,$Xx,$y,$z},$A);$Zx={$v,$Yx};$cy=q#/lib/fn_rw.b#;$dy=bless({$T2,$Vx,$u4,$q,$v4,$q,$w4,$Zx,$Q,$cy},$T3);$ey={};$fy=[];$gy=q#my $self = shift; "fn {$$self{code}}"#;$hy=bless({$t,$fy,$v,$q,$w,$gy,$y,$z},$A);$iy=[];$jy=bless({$t,$iy,$v,$q,$w,$Nf,$y,$z},$A);$ky={$Ff,$hy,$Mf,$jy};$ly=q#/lib/fn_ops.b#;$my=bless({$T2,$ey,$u4,$q,$v4,$q,$w4,$ky,$Q,$ly},$T3);$ny={};$oy=[];$py=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$qy=bless({$t,$oy,$v,$q,$w,$py,$y,$z},$A);$ry={$w9,$qy};$sy=q#/lib/fn_serialize.b#;$ty=bless({$T2,$ny,$u4,$q,$v4,$q,$w4,$ry,$Q,$sy},$T3);$uy=[$H4,$sg,$Gx,$Ux,$dy,$my,$ty];$vy=bless({$T2,$vx,$Q,$na,$i3,$uy},$I3);$wy=q#ni:/lib/fn.c#;$xy={$I3,1};$yy=q#/lib/fn.c#;$zy=[$Kg];$Ay=bless({$T2,$xy,$Q,$yy,$i3,$zy},$h4);$By=q#ni:/lib/fn_init.b#;$Cy=q#ni:/lib/fn_ops.b#;$Dy=q#ni:/lib/fn_ro.b#;$Ey=q#ni:/lib/fn_rw.b#;$Fy=q#ni:/lib/fn_serialize.b#;$Gy=q#ni:/lib/future#;$Hy={$J3,1};$Iy={};$Jy=[];$Ky=bless({$t,$Jy,$v,$q,$w,$xr,$y,$z},$A);$Ly=q#parents#;$My=[];$Ny=q#shift->{'parents'}#;$Oy=bless({$t,$My,$v,$q,$w,$Ny,$y,$z},$A);$Py={$r,$Ky,$Ly,$Oy};$Qy=q#/lib/future_ro.b#;$Ry=bless({$T2,$Iy,$u4,$q,$v4,$q,$w4,$Py,$Q,$Qy},$T3);$Sy={};$Ty=[];$Uy=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Vy=bless({$t,$Ty,$v,$q,$w,$Uy,$y,$z},$A);$Wy={$K6,$Vy};$Xy=q#/lib/future_init.b#;$Yy=bless({$T2,$Sy,$u4,$q,$v4,$q,$w4,$Wy,$Q,$Xy},$T3);$Zy={};$cz=q#decide#;$dz=[];$ez=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$fz=bless({$t,$dz,$v,$q,$w,$ez,$y,$z},$A);$gz=q#decided#;$hz=[];$iz=q#shift->{outcome}#;$jz=bless({$t,$hz,$v,$q,$w,$iz,$y,$z},$A);$kz=q#listener#;$lz=[];$mz=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$nz=bless({$t,$lz,$v,$q,$w,$mz,$y,$z},$A);$oz=q#v#;$pz=[];$qz=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$rz=bless({$t,$pz,$v,$q,$w,$qz,$y,$z},$A);$sz={$cz,$fz,$gz,$jz,$kz,$nz,$oz,$rz};$tz=q#/lib/future_state.b#;$uz=bless({$T2,$Zy,$u4,$q,$v4,$q,$w4,$sz,$Q,$tz},$T3);$vz={};$wz=q#and#;$xz=[];$yz=q#my $self   = $_[0];
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
$child;#;$zz=bless({$t,$xz,$v,$q,$w,$yz,$y,$z},$A);$Az=q#flatmap#;$Bz=[];$Cz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$Dz=bless({$t,$Bz,$v,$q,$w,$Cz,$y,$z},$A);$Ez=q#map#;$Fz=[];$Gz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$Hz=bless({$t,$Fz,$v,$q,$w,$Gz,$y,$z},$A);$Iz=q#or#;$Jz=[];$Kz=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Lz=bless({$t,$Jz,$v,$q,$w,$Kz,$y,$z},$A);$Mz={$wz,$zz,$Az,$Dz,$Ez,$Hz,$Iz,$Lz};$Nz=q#/lib/future_algebra.b#;$Oz=bless({$T2,$vz,$u4,$q,$v4,$q,$w4,$Mz,$Q,$Nz},$T3);$Pz=[$H4,$Ry,$Yy,$uz,$Oz];$Qz=bless({$T2,$Hy,$Q,$Ea,$i3,$Pz},$K3);$Rz=q#ni:/lib/future.c#;$Sz={$K3,1};$Tz=q#/lib/future.c#;$Uz=[$Kg];$Vz=bless({$T2,$Sz,$Q,$Tz,$i3,$Uz},$h4);$Wz=q#ni:/lib/future_algebra.b#;$Xz=q#ni:/lib/future_init.b#;$Yz=q#ni:/lib/future_ro.b#;$Zz=q#ni:/lib/future_state.b#;$cA=q#ni:/lib/gensym_generator_compact.b#;$dA=q#ni:/lib/global_static_test.b#;$eA={};$fA=[];$gA=q#ni('ni:/lib/test_case')->new(shift)#;$hA=q#($)#;$iA=bless({$t,$fA,$v,$q,$w,$gA,$y,$hA},$A);$jA=q#now#;$kA=[];$lA=q#ni('ni:/lib/test_value')->new(shift)#;$mA=bless({$t,$kA,$v,$q,$w,$lA,$y,$hA},$A);$nA={$q6,$iA,$jA,$mA};$oA=q#/lib/global_static_test.b#;$pA=bless({$T2,$eA,$u4,$q,$v4,$q,$w4,$nA,$Q,$oA},$T3);$qA=q#ni:/lib/image#;$rA=q#ni:/lib/image.c#;$sA={$M3,1};$tA=q#/lib/image.c#;$uA=[$Kg];$vA=bless({$T2,$sA,$Q,$tA,$i3,$uA},$h4);$wA=q#ni:/lib/image_init.b#;$xA=q#ni:/lib/image_quoting.b#;$yA=q#ni:/lib/instance.b#;$zA=q#ni:/lib/instantiable.b#;$AA=q#ni:/lib/json.b#;$BA={};$CA=q#json_decode#;$DA=[];$EA=q#local $_;
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
wantarray ? @$r : $$r[0];#;$FA=bless({$t,$DA,$v,$q,$w,$EA,$y,$hA},$A);$GA=q#json_encode#;$HA=[];$IA=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$JA=bless({$t,$HA,$v,$q,$w,$IA,$y,$hA},$A);$KA=q#json_encode_pretty#;$LA=[];$MA=q#local $_;
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

ni::json_encode($v);#;$NA=bless({$t,$LA,$v,$q,$w,$MA,$y,$z},$A);$OA=q#json_escape#;$PA=[];$QA=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$RA=bless({$t,$PA,$v,$q,$w,$QA,$y,$hA},$A);$SA=q#json_unescape#;$TA=[];$UA=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$VA=bless({$t,$TA,$v,$q,$w,$UA,$y,$hA},$A);$WA=q#json_unescape_one#;$XA=[];$YA=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$ZA=bless({$t,$XA,$v,$q,$w,$YA,$y,$hA},$A);$cB={$CA,$FA,$GA,$JA,$KA,$NA,$OA,$RA,$SA,$VA,$WA,$ZA};$dB=q#/lib/json.b#;$eB=bless({$T2,$BA,$u4,$q,$v4,$q,$w4,$cB,$Q,$dB},$T3);$fB=q#ni#;$gB=q#ni:/lib/json_data.b#;$hB={};$iB=q#json_escapes#;$jB=q##;$kB=q#b#;$lB=q#	#;$mB=q#t#;$nB=q#
#;$oB=q#n#;$pB=q##;$qB=q#f#;$rB=q##;$sB=q#"#;$tB=q#/#;$uB=q#\\#;$vB={$jB,$kB,$lB,$mB,$nB,$oB,$pB,$qB,$rB,$Cn,$sB,$sB,$tB,$tB,$uB,$uB};$wB=q#json_unescapes#;$xB={$sB,$sB,$tB,$tB,$uB,$uB,$kB,$jB,$qB,$pB,$oB,$nB,$Cn,$rB,$mB,$lB};$yB={$iB,$vB,$wB,$xB};$zB=q#/lib/json_data.b#;$AB=bless({$T2,$hB,$uq,$yB,$Q,$zB},$F3);$BB=q#ni:/lib/name_as_string.b#;$CB=q#ni:/lib/named.b#;$DB=q#ni:/lib/named_in_ni.b#;$EB=q#ni:/lib/namespaced.b#;$FB=q#ni:/lib/ni#;$GB={$N3,1};$HB={};$IB=q#extend#;$JB=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$KB=bless({$w,$JB,$y,$z},$A);$LB=q#is_mutable#;$MB=q#$0 ne '-' && -w $0#;$NB=bless({$w,$MB,$y,$z},$A);$OB=q#modify#;$PB=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$QB=bless({$w,$PB,$y,$z},$A);$RB={$IB,$KB,$LB,$NB,$OB,$QB};$SB=q#/lib/ni_self.b#;$TB=bless({$T2,$HB,$u4,$q,$v4,$q,$w4,$RB,$Q,$SB},$T3);$UB={};$VB=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$WB=bless({$w,$VB,$y,$z},$A);$XB=q#docs#;$YB=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$ZB=bless({$w,$YB,$y,$z},$A);$cC=q#metaclasses#;$dC=q#my $self = shift;
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$eC=bless({$w,$dC,$y,$z},$A);$fC=q#undocumented#;$gC=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$hC=bless({$w,$gC,$y,$z},$A);$iC=q#untested#;$jC=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$kC=bless({$w,$jC,$y,$z},$A);$lC={$K,$WB,$XB,$ZB,$cC,$eC,$fC,$hC,$iC,$kC};$mC=q#/lib/ni_dev_introspection.b#;$nC=bless({$T2,$UB,$u4,$q,$v4,$q,$w4,$lC,$Q,$mC},$T3);$oC={};$pC=q#--internal/+=#;$qC=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$rC=bless({$w,$qC,$y,$z},$A);$sC=q#--internal/dev-state#;$tC=q#my $self = shift;
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
0;#;$uC=bless({$w,$tC,$y,$z},$A);$vC=q#--internal/eval#;$wC=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$xC=bless({$w,$wC,$y,$z},$A);$yC=q#--internal/image#;$zC=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$AC=bless({$w,$zC,$y,$z},$A);$BC=q#--internal/test#;$CC=q#local $| = 1;
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
!!$failed;#;$DC=bless({$w,$CC,$y,$z},$A);$EC=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$FC=bless({$w,$EC,$y,$z},$A);$GC={$pC,$rC,$sC,$uC,$vC,$xC,$yC,$AC,$BC,$DC,$Ft,$FC};$HC=q#/lib/ni_main.b#;$IC=bless({$T2,$oC,$u4,$q,$v4,$q,$w4,$GC,$Q,$HC},$T3);$JC={};$KC=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$LC=bless({$w,$KC,$y,$z},$A);$MC=q#resolver_for#;$NC=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$OC=bless({$w,$NC,$y,$z},$A);$PC={$p9,$LC,$MC,$OC};$QC=q#/lib/ni_resolver.b#;$RC=bless({$T2,$JC,$u4,$q,$v4,$q,$w4,$PC,$Q,$QC},$T3);$SC={};$TC=q#exists#;$UC=q#exists $_[0]->{named}{$_[1]}#;$VC=bless({$w,$UC,$y,$z},$A);$WC=q#quoted#;$XC=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$YC=bless({$w,$XC,$y,$z},$A);$ZC={$TC,$VC,$WC,$YC};$cD=q#/lib/ni_image.b#;$dD=bless({$T2,$SC,$u4,$q,$v4,$q,$w4,$ZC,$Q,$cD},$T3);$eD=[$H4,$TB,$nC,$IC,$RC,$dD];$fD=bless({$T2,$GB,$Q,$Ad,$i3,$eD},$O3);$gD=q#ni:/lib/ni.c#;$hD={$O3,1};$iD=q#/lib/ni.c#;$jD=[$Kg];$kD=bless({$T2,$hD,$Q,$iD,$i3,$jD},$h4);$lD=q#ni:/lib/ni_dev_introspection.b#;$mD=q#ni:/lib/ni_image.b#;$nD=q#ni:/lib/ni_main.b#;$oD=q#ni:/lib/ni_resolver.b#;$pD=q#ni:/lib/ni_self.b#;$qD=q#ni:/lib/ni_static_util.b#;$rD={};$sD=q#abbrev#;$tD=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$uD=bless({$w,$tD,$y,$z},$A);$vD=q#dor#;$wD=q#defined $_[0] ? $_[0] : $_[1]#;$xD=bless({$w,$wD,$y,$z},$A);$yD=q#indent#;$zD=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$AD=bless({$w,$zD,$y,$z},$A);$BD=q#max#;$CD=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$DD=bless({$w,$CD,$y,$z},$A);$ED=q#maxstr#;$FD=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$GD=bless({$w,$FD,$y,$z},$A);$HD=q#mean#;$ID=q#sum(@_) / (@_ || 1)#;$JD=bless({$w,$ID,$y,$z},$A);$KD=q#min#;$LD=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$MD=bless({$w,$LD,$y,$z},$A);$ND=q#minstr#;$OD=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$PD=bless({$w,$OD,$y,$z},$A);$QD=q#outdent#;$RD=q#my $x = shift;
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
join "\\n", @lines;#;$SD=bless({$w,$RD,$y,$z},$A);$TD=q#sgr#;$UD=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$VD=bless({$w,$UD,$y,$z},$A);$WD=q#sr#;$XD=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$YD=bless({$w,$XD,$y,$z},$A);$ZD=q#sum#;$cE=q#local $_; my $x = 0; $x += $_ for @_; $x#;$dE=bless({$w,$cE,$y,$z},$A);$eE=q#swap#;$fE=q#@_[0, 1] = @_[1, 0]#;$gE=bless({$w,$fE,$y,$z},$A);$hE={$sD,$uD,$vD,$xD,$yD,$AD,$BD,$DD,$ED,$GD,$HD,$JD,$KD,$MD,$ND,$PD,$QD,$SD,$TD,$VD,$WD,$YD,$ZD,$dE,$eE,$gE};$iE=q#/lib/ni_static_util.b#;$jE=bless({$T2,$rD,$u4,$q,$v4,$q,$w4,$hE,$Q,$iE},$T3);$kE=q#ni:/lib/object_metadata#;$lE={$P3,1,$C,1,$H,1};$mE=q#/lib/object_metadata#;$nE={};$oE=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$pE=bless({$w,$oE,$y,$z},$A);$qE={$S2,$pE};$rE=q#/lib/object_metadata_rw.b#;$sE=bless({$T2,$nE,$u4,$q,$v4,$q,$w4,$qE,$Q,$rE},$T3);$tE=[$H4,$sE];$uE=bless({$T2,$lE,$Q,$mE,$i3,$tE},$Q3);$vE=q#ni:/lib/object_metadata.c#;$wE={$Q3,1,$d4,1,$g4,1};$xE=q#/lib/object_metadata.c#;$yE=[$Kg];$zE=bless({$T2,$wE,$Q,$xE,$i3,$yE},$h4);$AE=q#ni:/lib/object_metadata_rw.b#;$BE=q#ni:/lib/perlbranch.b#;$CE=q#ni:/lib/quote_circular_addressed.b#;$DE=q#ni:/lib/quote_code_fail.b#;$EE=q#ni:/lib/quote_gensym_identity.b#;$FE=q#ni:/lib/quote_objects.b#;$GE=q#ni:/lib/quote_simple#;$HE={$R3,1};$IE={};$JE=[];$KE=q#+{}#;$LE=bless({$t,$JE,$v,$q,$w,$KE,$y,$z},$A);$ME={$K6,$LE};$NE=q#/lib/quote_simple_init.b#;$OE=bless({$T2,$IE,$u4,$q,$v4,$q,$w4,$ME,$Q,$NE},$T3);$PE={};$QE=[];$RE=bless({$t,$QE,$v,$q,$w,0,$y,$z},$A);$SE=[];$TE=q#shift->quote_value(shift)#;$UE=bless({$t,$SE,$v,$q,$w,$TE,$y,$z},$A);$VE={$Bc,$RE,$Vc,$UE};$WE=q#/lib/quote_simple_quote.b#;$XE=bless({$T2,$PE,$u4,$q,$v4,$q,$w4,$VE,$Q,$WE},$T3);$YE=[$H4,$OE,$XE,$Bb,$Zb,$rc];$ZE=bless({$T2,$HE,$Q,$Ld,$i3,$YE},$S3);$cF=q#ni:/lib/quote_simple.c#;$dF={$S3,1};$eF=q#/lib/quote_simple.c#;$fF=[$Kg];$gF=bless({$T2,$dF,$Q,$eF,$i3,$fF},$h4);$hF=q#ni:/lib/quote_simple_init.b#;$iF=q#ni:/lib/quote_simple_quote.b#;$jF=q#ni:/lib/quote_values.b#;$kF=q#ni:/lib/ref_eq.b#;$lF=q#ni:/lib/resolver.b#;$mF=q#ni:/lib/slice#;$nF=q#ni:/lib/slice.b#;$oF=q#ni:/lib/slice.c#;$pF={$U3,1};$qF=q#/lib/slice.c#;$rF=[$Og];$sF=bless({$T2,$pF,$Q,$qF,$i3,$rF},$h4);$tF=q#ni:/lib/slice_init.b#;$uF=q#ni:/lib/slice_serialize.b#;$vF=q#ni:/lib/static_fn.b#;$wF={};$xF=q#fc#;$yF=[];$zF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$AF=bless({$t,$yF,$v,$q,$w,$zF,$y,$z},$A);$BF=q#fk#;$CF=[];$DF=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$EF=bless({$t,$CF,$v,$q,$w,$DF,$y,$hA},$A);$FF=[];$GF=q#ni('ni:/lib/fn')->new(@_)#;$HF=bless({$t,$FF,$v,$q,$w,$GF,$y,$hA},$A);$IF=q#fp#;$JF=[];$KF=q#($$)#;$LF=bless({$t,$JF,$v,$q,$w,$GF,$y,$KF},$A);$MF={$xF,$AF,$BF,$EF,$Ox,$HF,$IF,$LF};$NF=q#/lib/static_fn.b#;$OF=bless({$T2,$wF,$u4,$q,$v4,$q,$w4,$MF,$Q,$NF},$T3);$PF=q#ni:/lib/subclass.b#;$QF=q#ni:/lib/tag#;$RF={$V3,1};$SF=q#/lib/tag#;$TF={};$UF=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$VF=bless({$w,$UF,$y,$z},$A);$WF={$J8,$VF};$XF=q#/lib/tag.b#;$YF=bless({$T2,$TF,$u4,$q,$v4,$q,$w4,$WF,$Q,$XF},$T3);$ZF={};$cG=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$dG=bless({$w,$cG,$y,$z},$A);$eG={$K6,$dG};$fG=q#/lib/tag_init.b#;$gG=bless({$T2,$ZF,$u4,$q,$v4,$q,$w4,$eG,$Q,$fG},$T3);$hG=[$B8,$H8,$YF,$gG];$iG=bless({$T2,$RF,$Q,$SF,$i3,$hG},$W3);$jG=q#ni:/lib/tag.b#;$kG=q#ni:/lib/tag.c#;$lG={$W3,1};$mG=q#/lib/tag.c#;$nG=[$Og];$oG=bless({$T2,$lG,$Q,$mG,$i3,$nG},$h4);$pG=q#ni:/lib/tag_init.b#;$qG=q#ni:/lib/test_assert_eq#;$rG={$X3,1};$sG=q#/lib/test_assert_eq#;$tG={$X3,1,$Z3,1};$uG=q#/lib/test_assertion#;$vG={};$wG=q#commit#;$xG=[];$yG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$zG=bless({$t,$xG,$v,$q,$w,$yG,$y,$z},$A);$AG={$wG,$zG};$BG=q#/lib/test_assertion_commit.b#;$CG=bless({$T2,$vG,$u4,$q,$v4,$q,$w4,$AG,$Q,$BG},$T3);$DG=[$H4,$CG];$EG=bless({$T2,$tG,$Q,$uG,$i3,$DG},$c4);$FG={};$GG=q#diff#;$HG=[];$IG=q#shift->{'diff'}#;$JG=bless({$t,$HG,$v,$q,$w,$IG,$y,$z},$A);$KG={$GG,$JG};$LG=q#/lib/test_assert_eq_ro.b#;$MG=bless({$T2,$FG,$u4,$q,$v4,$q,$w4,$KG,$Q,$LG},$T3);$NG={};$OG=[];$PG=q#my ($class, $diff) = @_;
+{diff => $diff};#;$QG=bless({$t,$OG,$v,$q,$w,$PG,$y,$z},$A);$RG={$K6,$QG};$SG=q#/lib/test_assert_eq_init.b#;$TG=bless({$T2,$NG,$u4,$q,$v4,$q,$w4,$RG,$Q,$SG},$T3);$UG={};$VG=[];$WG=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$XG=bless({$t,$VG,$v,$q,$w,$WG,$y,$z},$A);$YG=q#failed#;$ZG=[];$cH=q#defined shift->{diff}#;$dH=bless({$t,$ZG,$v,$q,$w,$cH,$y,$z},$A);$eH=q#result#;$fH=[];$gH=bless({$t,$fH,$v,$q,$w,$Ht,$y,$z},$A);$hH={$Ff,$XG,$YG,$dH,$eH,$gH};$iH=q#/lib/test_assert_eq_result.b#;$jH=bless({$T2,$UG,$u4,$q,$v4,$q,$w4,$hH,$Q,$iH},$T3);$kH=[$EG,$MG,$TG,$jH];$lH=bless({$T2,$rG,$Q,$sG,$i3,$kH},$Y3);$mH=q#ni:/lib/test_assert_eq.c#;$nH={$Y3,1};$oH=q#/lib/test_assert_eq.c#;$pH={$Y3,1,$c4,1};$qH=q#/lib/test_assertion.c#;$rH=[$Kg];$sH=bless({$T2,$pH,$Q,$qH,$i3,$rH},$h4);$tH=[$sH];$uH=bless({$T2,$nH,$Q,$oH,$i3,$tH},$h4);$vH=q#ni:/lib/test_assert_eq_init.b#;$wH=q#ni:/lib/test_assert_eq_result.b#;$xH=q#ni:/lib/test_assert_eq_ro.b#;$yH=q#ni:/lib/test_assertion#;$zH=q#ni:/lib/test_assertion.c#;$AH=q#ni:/lib/test_assertion_commit.b#;$BH=q#ni:/lib/test_case#;$CH={$C,1};$DH=q#/lib/test_case#;$EH=q#running_test#;$FH={};$GH=[];$HH=q#shift->{'assertions'}#;$IH=bless({$t,$GH,$v,$q,$w,$HH,$y,$z},$A);$JH=[];$KH=q#shift->{'test'}#;$LH=bless({$t,$JH,$v,$q,$w,$KH,$y,$z},$A);$MH={$n,$IH,$s,$LH};$NH=q#/lib/test_case_ro.b#;$OH=bless({$T2,$FH,$u4,$q,$v4,$q,$w4,$MH,$Q,$NH},$T3);$PH={};$QH=[];$RH=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$SH=bless({$t,$QH,$v,$q,$w,$RH,$y,$z},$A);$TH={$p,$SH};$UH=q#/lib/test_case_rw.b#;$VH=bless({$T2,$PH,$u4,$q,$v4,$q,$w4,$TH,$Q,$UH},$T3);$WH={};$XH=[];$YH=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$ZH=bless({$t,$XH,$v,$q,$w,$YH,$y,$z},$A);$cI={$K6,$ZH};$dI=q#/lib/test_case_init.b#;$eI=bless({$T2,$WH,$u4,$q,$v4,$q,$w4,$cI,$Q,$dI},$T3);$fI={};$gI=[];$hI=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$iI=bless({$t,$gI,$v,$q,$w,$hI,$y,$z},$A);$jI=[];$kI=q#!shift->{outcome}->[0]#;$lI=bless({$t,$jI,$v,$q,$w,$kI,$y,$z},$A);$mI={$Ff,$iI,$YG,$lI};$nI=q#/lib/test_case_metrics.b#;$oI=bless({$T2,$fI,$u4,$q,$v4,$q,$w4,$mI,$Q,$nI},$T3);$pI={};$qI=q#done#;$rI=[];$sI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$tI=bless({$t,$rI,$v,$q,$w,$sI,$y,$z},$A);$uI=[];$vI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$wI=bless({$t,$uI,$v,$q,$w,$vI,$y,$z},$A);$xI={$qI,$tI,$Ft,$wI};$yI=q#/lib/test_case_run.b#;$zI=bless({$T2,$pI,$u4,$q,$v4,$q,$w4,$xI,$Q,$yI},$T3);$AI=[$uE,$OH,$VH,$eI,$oI,$zI];$BI=bless({$T2,$CH,$Q,$DH,$EH,$q,$i3,$AI},$d4);$CI=[];$DI=q#shift->{running_test} = undef#;$EI=bless({$t,$CI,$v,$q,$w,$DI,$y,$z},$A);$FI=q#ni:/lib/test_case.c#;$GI={$d4,1};$HI=q#/lib/test_case.c#;$II={};$JI=[];$KI=q#shift->{'running_test'}#;$LI=bless({$t,$JI,$v,$q,$w,$KI,$y,$z},$A);$MI={$EH,$LI};$NI=q#/lib/test_case.c_test_ro.b#;$OI=bless({$T2,$II,$u4,$q,$v4,$q,$w4,$MI,$Q,$NI},$T3);$PI={};$QI=q#with_test#;$RI=[];$SI=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$TI=bless({$t,$RI,$v,$q,$w,$SI,$y,$z},$A);$UI={$QI,$TI};$VI=q#/lib/test_case.c_test.b#;$WI=bless({$T2,$PI,$u4,$EI,$v4,$q,$w4,$UI,$Q,$VI},$T3);$XI=[$zE,$OI,$WI];$YI=bless({$T2,$GI,$Q,$HI,$i3,$XI},$h4);$ZI=q#ni:/lib/test_case.c_test.b#;$cJ=q#ni:/lib/test_case.c_test_ro.b#;$dJ=q#ni:/lib/test_case_init.b#;$eJ=q#ni:/lib/test_case_metrics.b#;$fJ=q#ni:/lib/test_case_ro.b#;$gJ=q#ni:/lib/test_case_run.b#;$hJ=q#ni:/lib/test_case_rw.b#;$iJ=q#ni:/lib/test_value#;$jJ={$e4,1};$kJ=q#/lib/test_value#;$lJ={};$mJ=[];$nJ=q#\\$_[1]#;$oJ=bless({$t,$mJ,$v,$q,$w,$nJ,$y,$z},$A);$pJ={$K6,$oJ};$qJ=q#/lib/test_value_init.b#;$rJ=bless({$T2,$lJ,$u4,$q,$v4,$q,$w4,$pJ,$Q,$qJ},$T3);$sJ={};$tJ=q#(==#;$uJ=[];$vJ=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$wJ=bless({$t,$uJ,$v,$q,$w,$vJ,$y,$z},$A);$xJ=q#detailed_scalar_diff#;$yJ=[];$zJ=q#local $_;
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
[@diff];#;$AJ=bless({$t,$yJ,$v,$q,$w,$zJ,$y,$z},$A);$BJ=[];$CJ=q#my ($self, $rhs) = @_;
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
return undef;#;$DJ=bless({$t,$BJ,$v,$q,$w,$CJ,$y,$z},$A);$EJ={$tJ,$wJ,$xJ,$AJ,$GG,$DJ};$FJ=q#/lib/test_value_eq.b#;$GJ=bless({$T2,$sJ,$u4,$q,$v4,$q,$w4,$EJ,$Q,$FJ},$T3);$HJ={};$IJ=[];$JJ=q#ni::json_encode ${$_[0]}#;$KJ=bless({$t,$IJ,$v,$q,$w,$JJ,$y,$z},$A);$LJ={$Ff,$KJ};$MJ=q#/lib/test_value_str.b#;$NJ=bless({$T2,$HJ,$u4,$q,$v4,$q,$w4,$LJ,$Q,$MJ},$T3);$OJ=[$H4,$rJ,$GJ,$NJ];$PJ=bless({$T2,$jJ,$Q,$kJ,$i3,$OJ},$f4);$QJ=q#ni:/lib/test_value.c#;$RJ={$f4,1};$SJ=q#/lib/test_value.c#;$TJ=[$Kg];$UJ=bless({$T2,$RJ,$Q,$SJ,$i3,$TJ},$h4);$VJ=q#ni:/lib/test_value_eq.b#;$WJ=q#ni:/lib/test_value_init.b#;$XJ=q#ni:/lib/test_value_str.b#;$YJ=q#ni:/lib/todo#;$ZJ={$H,1};$cK=q#/lib/todo#;$dK={};$eK=q#shift->{'todo'}#;$fK=bless({$w,$eK,$y,$z},$A);$gK={$E,$fK};$hK=q#/lib/todo_ro.b#;$iK=bless({$T2,$dK,$u4,$q,$v4,$q,$w4,$gK,$Q,$hK},$T3);$jK={};$kK=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$lK=bless({$w,$kK,$y,$z},$A);$mK={$K6,$lK};$nK=q#/lib/todo_init.b#;$oK=bless({$T2,$jK,$u4,$q,$v4,$q,$w4,$mK,$Q,$nK},$T3);$pK={};$qK=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$rK=bless({$w,$qK,$y,$z},$A);$sK={$Ff,$rK};$tK=q#/lib/todo_str.b#;$uK=bless({$T2,$pK,$u4,$q,$v4,$q,$w4,$sK,$Q,$tK},$T3);$vK=[$uE,$iK,$oK,$uK];$wK=bless({$T2,$ZJ,$Q,$cK,$i3,$vK},$g4);$xK=q#ni:/lib/todo.c#;$yK={$g4,1};$zK=q#/lib/todo.c#;$AK=[$zE];$BK=bless({$T2,$yK,$Q,$zK,$i3,$AK},$h4);$CK=q#ni:/lib/todo_ctor.b#;$DK={};$EK=q#ni('ni:/lib/todo')->new(@_)#;$FK=bless({$w,$EK,$y,$z},$A);$GK={$R2,$FK};$HK=q#/lib/todo_ctor.b#;$IK=bless({$T2,$DK,$u4,$q,$v4,$q,$w4,$GK,$Q,$HK},$T3);$JK=q#ni:/lib/todo_init.b#;$KK=q#ni:/lib/todo_ro.b#;$LK=q#ni:/lib/todo_str.b#;$MK=q#ni:/metaclass#;$NK={$h4,1};$OK=q#/metaclass#;$PK=[$ef,$sg,$kf,$lg];$QK=bless({$T2,$NK,$Q,$OK,$i3,$PK},$i4);$RK=q#ni:/metaclass.c#;$SK={$i4,1};$TK=q#/metaclass.c#;$UK=[$Bg];$VK=bless({$T2,$SK,$Q,$TK,$i3,$UK},$h4);$WK=q#ni:/module#;$XK=q#ni:/module.c#;$YK=q#ni:/object#;$ZK=q#ni:/object.c#;$cL=q#ni:/semantic#;$dL=q#ni::/semantic#;$eL={$dL,1};$fL=[];$gL=bless({$T2,$eL,$Q,$Le,$i3,$fL},$j4);$hL=q#ni:/semantic/dimension#;$iL={$n4,1};$jL=q#/semantic/dimension#;$kL=[$Bg];$lL=bless({$T2,$iL,$Q,$jL,$i3,$kL},$o4);$mL=q#ni:/semantic/dimension.c#;$nL={$o4,1};$oL=q#/semantic/dimension.c#;$pL=[$Sg];$qL=bless({$T2,$nL,$Q,$oL,$i3,$pL},$h4);$rL=q#ni:/semantic/task#;$sL=q#ni:/semantic/task.c#;$tL=q#ni:/semantic/task_outcome.b#;$uL=q#ni:/semantic/task_ro.b#;$vL=q#ni:main#;$wL={$np,1};$xL=[$IK,$OF,$pA,$mp];$yL=bless({$T2,$wL,$Q,$np,$i3,$xL},$j4);$zL=q#ni:ni#;$AL={$fB,1};$BL=[$jE,$AB,$eB];$CL=bless({$T2,$AL,$Q,$fB,$i3,$BL},$j4);$DL={$d,$T,$W,$f1,$g1,$l1,$m1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$J2,$K2,$P2,$Q2,$o6,$p6,$h8,$i8,$o8,$p8,$K9,$L9,$ga,$ha,$oa,$pa,$Fa,$Ga,$td,$ud,$Bd,$Cd,$Md,$Nd,$Ge,$He,$Me,$Ne,$Bg,$Cg,$Sg,$Tg,$Xg,$Yg,$eh,$fh,$Hh,$Ih,$Yh,$Zh,$nh,$ci,$Fh,$di,$ui,$vi,$zi,$Ai,$li,$Bi,$si,$Ci,$kk,$lk,$pk,$qk,$Qj,$rk,$ik,$sk,$Si,$tk,$Ij,$uk,$oj,$vk,$Li,$wk,$Ol,$Sl,$sm,$tm,$qm,$um,$ml,$vm,$xl,$wm,$Mk,$xm,$Ll,$ym,$Fk,$zm,$Uk,$Am,$Un,$Vn,$Zn,$co,$Qm,$do,$pn,$eo,$Xm,$fo,$Sn,$go,$Im,$ho,$xn,$io,$Bo,$Co,$Go,$Ho,$zo,$Io,$qo,$Jo,$mp,$op,$Mp,$Np,$Rp,$Sp,$xp,$Tp,$Kp,$Up,$i6,$Vp,$Wh,$Wp,$Uh,$Xp,$m5,$Yp,$u5,$Zp,$G5,$cq,$Q4,$dq,$g6,$eq,$S5,$fq,$M7,$gq,$kq,$lq,$K7,$mq,$Q6,$nq,$q7,$oq,$F6,$pq,$e7,$qq,$hr,$ir,$mr,$nr,$Pq,$or,$fr,$pr,$Iq,$qr,$As,$Es,$Qs,$Rs,$Os,$Ss,$Ut,$Yt,$uu,$vu,$su,$wu,$tt,$xu,$Dt,$yu,$mt,$zu,$Pt,$Au,$es,$Bu,$ys,$Cu,$Uu,$Vu,$Zu,$cv,$Lu,$dv,$Su,$ev,$iv,$jv,$Df,$kv,$B8,$lv,$Og,$mv,$wv,$xv,$cf,$yv,$Cv,$Dv,$uv,$Ev,$kf,$Fv,$E9,$Gv,$R8,$Hv,$Lv,$Mv,$X8,$Nv,$jg,$Ov,$tf,$Pv,$Yf,$Qv,$hg,$Rv,$Ww,$Xw,$kx,$lx,$ix,$mx,$Ew,$nx,$qw,$ox,$Kw,$px,$yw,$qx,$Yv,$rx,$gw,$sx,$Uw,$tx,$z8,$ux,$vy,$wy,$Ay,$By,$Gx,$Cy,$my,$Dy,$Ux,$Ey,$dy,$Fy,$ty,$Gy,$Qz,$Rz,$Vz,$Wz,$Oz,$Xz,$Yy,$Yz,$Ry,$Zz,$uz,$cA,$ld,$dA,$pA,$qA,$nd,$rA,$vA,$wA,$Ta,$xA,$tb,$yA,$F4,$zA,$sg,$AA,$eB,$gB,$AB,$BB,$Kf,$CB,$H8,$DB,$g9,$EB,$n9,$FB,$fD,$gD,$kD,$lD,$nC,$mD,$dD,$nD,$IC,$oD,$RC,$pD,$TB,$qD,$jE,$kE,$uE,$vE,$zE,$AE,$sE,$BE,$ef,$CE,$Hc,$DE,$Bb,$EE,$dd,$FE,$rc,$GE,$ZE,$cF,$gF,$hF,$OE,$iF,$XE,$jF,$Zb,$kF,$Rf,$lF,$u9,$mF,$le,$nF,$de,$oF,$sF,$tF,$je,$uF,$C9,$vF,$OF,$PF,$zg,$QF,$iG,$jG,$YF,$kG,$oG,$pG,$gG,$qG,$lH,$mH,$uH,$vH,$TG,$wH,$jH,$xH,$MG,$yH,$EG,$zH,$sH,$AH,$CG,$BH,$BI,$FI,$YI,$ZI,$WI,$cJ,$OI,$dJ,$eI,$eJ,$oI,$fJ,$OH,$gJ,$zI,$hJ,$VH,$iJ,$PJ,$QJ,$UJ,$VJ,$GJ,$WJ,$rJ,$XJ,$NJ,$YJ,$wK,$xK,$BK,$CK,$IK,$JK,$oK,$KK,$iK,$LK,$uK,$MK,$QK,$RK,$VK,$WK,$lg,$XK,$Qg,$YK,$H4,$ZK,$Kg,$cL,$gL,$hL,$lL,$mL,$qL,$rL,$Pr,$sL,$Ks,$tL,$Nr,$uL,$Br,$vL,$yL,$zL,$CL};$EL=q#resolvers#;$FL=[];$GL=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$HL=bless({$t,$FL,$v,$q,$w,$GL,$y,$z},$A);$IL=q#file#;$JL=[];$KL=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$LL=bless({$t,$JL,$v,$q,$w,$KL,$y,$z},$A);$ML=q#null#;$NL=[];$OL=q#ni('ni:/io/null')->new#;$PL=bless({$t,$NL,$v,$q,$w,$OL,$y,$z},$A);$QL=q#sh#;$RL=[];$SL=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$TL=bless({$t,$RL,$v,$q,$w,$SL,$y,$z},$A);$UL=q#str#;$VL=[];$WL=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$XL=bless({$t,$VL,$v,$q,$w,$WL,$y,$z},$A);$YL={$s7,$HL,$IL,$LL,$ML,$PL,$QL,$TL,$UL,$XL};$ZL=bless({$c,$DL,$EL,$YL},$N3);*$Bx=\&$zx;*$Ax=\&$xx;*$Xd=\&$Vd;*$Wd=\&$Td;$F4->apply_($j3);$F4->apply_($k3);$F4->apply_($U2);$F4->apply_($l3);$F4->apply_($V2);$F4->apply_($m3);$F4->apply_($W2);$F4->apply_($n3);$F4->apply_($X2);$F4->apply_($o3);$F4->apply_($Y2);$F4->apply_($p3);$F4->apply_($Z2);$F4->apply_($q3);$F4->apply_($c3);$F4->apply_($r3);$F4->apply_($d3);$F4->apply_($s3);$F4->apply_($e3);$F4->apply_($t3);$F4->apply_($f3);$F4->apply_($u3);$F4->apply_($v3);$F4->apply_($w3);$F4->apply_($x3);$F4->apply_($y3);$F4->apply_($z3);$F4->apply_($A3);$F4->apply_($B3);$F4->apply_($C3);$F4->apply_($D3);$F4->apply_($E3);$F4->apply_($F3);$F4->apply_($G3);$F4->apply_($S);$F4->apply_($H3);$F4->apply_($A);$F4->apply_($I3);$F4->apply_($J3);$F4->apply_($K3);$F4->apply_($L3);$F4->apply_($M3);$F4->apply_($N3);$F4->apply_($O3);$F4->apply_($P3);$F4->apply_($Q3);$F4->apply_($R3);$F4->apply_($S3);$F4->apply_($T3);$F4->apply_($U3);$F4->apply_($V3);$F4->apply_($W3);$F4->apply_($X3);$F4->apply_($Y3);$F4->apply_($Z3);$F4->apply_($c4);$F4->apply_($C);$F4->apply_($d4);$F4->apply_($e4);$F4->apply_($f4);$F4->apply_($H);$F4->apply_($g4);$F4->apply_($h4);$F4->apply_($i4);$F4->apply_($j4);$F4->apply_($k4);$F4->apply_($l4);$F4->apply_($m4);$F4->apply_($n4);$F4->apply_($o4);$F4->apply_($p4);$F4->apply_($q4);$Q4->apply_($U2);$Q4->apply_($V2);$Q4->apply_($W2);$Q4->apply_($X2);$Q4->apply_($Y2);$Q4->apply_($Z2);$Q4->apply_($c3);$Q4->apply_($d3);$Q4->apply_($e3);$Q4->apply_($f3);$m5->apply_($U2);$m5->apply_($V2);$m5->apply_($W2);$m5->apply_($X2);$m5->apply_($Y2);$m5->apply_($Z2);$m5->apply_($c3);$m5->apply_($d3);$m5->apply_($e3);$m5->apply_($f3);$u5->apply_($U2);$u5->apply_($V2);$u5->apply_($W2);$u5->apply_($X2);$u5->apply_($Y2);$u5->apply_($Z2);$u5->apply_($c3);$u5->apply_($d3);$u5->apply_($e3);$u5->apply_($f3);$G5->apply_($U2);$G5->apply_($V2);$G5->apply_($W2);$G5->apply_($X2);$G5->apply_($Y2);$G5->apply_($Z2);$G5->apply_($c3);$G5->apply_($d3);$G5->apply_($e3);$G5->apply_($f3);$S5->apply_($U2);$S5->apply_($V2);$S5->apply_($W2);$S5->apply_($X2);$S5->apply_($Y2);$S5->apply_($Z2);$S5->apply_($c3);$S5->apply_($d3);$S5->apply_($e3);$S5->apply_($f3);$g6->apply_($U2);$g6->apply_($V2);$g6->apply_($W2);$g6->apply_($X2);$g6->apply_($Y2);$g6->apply_($Z2);$g6->apply_($c3);$g6->apply_($d3);$g6->apply_($e3);$g6->apply_($f3);$F6->apply_($e3);$Q6->apply_($e3);$e7->apply_($e3);$q7->apply_($e3);$K7->apply_($e3);$z8->apply_($j3);$z8->apply_($k3);$z8->apply_($l3);$z8->apply_($m3);$z8->apply_($n3);$z8->apply_($o3);$z8->apply_($p3);$z8->apply_($q3);$z8->apply_($r3);$z8->apply_($s3);$z8->apply_($t3);$z8->apply_($u3);$z8->apply_($w3);$z8->apply_($y3);$z8->apply_($A3);$z8->apply_($B3);$z8->apply_($C3);$z8->apply_($D3);$z8->apply_($E3);$z8->apply_($F3);$z8->apply_($G3);$z8->apply_($H3);$z8->apply_($I3);$z8->apply_($K3);$z8->apply_($M3);$z8->apply_($O3);$z8->apply_($Q3);$z8->apply_($S3);$z8->apply_($T3);$z8->apply_($U3);$z8->apply_($V3);$z8->apply_($W3);$z8->apply_($Y3);$z8->apply_($c4);$z8->apply_($d4);$z8->apply_($f4);$z8->apply_($g4);$z8->apply_($h4);$z8->apply_($i4);$z8->apply_($j4);$z8->apply_($k4);$z8->apply_($m4);$z8->apply_($n4);$z8->apply_($o4);$z8->apply_($q4);$H8->apply_($j3);$H8->apply_($k3);$H8->apply_($l3);$H8->apply_($m3);$H8->apply_($n3);$H8->apply_($o3);$H8->apply_($p3);$H8->apply_($q3);$H8->apply_($r3);$H8->apply_($s3);$H8->apply_($t3);$H8->apply_($u3);$H8->apply_($w3);$H8->apply_($y3);$H8->apply_($A3);$H8->apply_($C3);$H8->apply_($D3);$H8->apply_($E3);$H8->apply_($F3);$H8->apply_($G3);$H8->apply_($S);$H8->apply_($H3);$H8->apply_($I3);$H8->apply_($K3);$H8->apply_($M3);$H8->apply_($O3);$H8->apply_($Q3);$H8->apply_($S3);$H8->apply_($T3);$H8->apply_($U3);$H8->apply_($V3);$H8->apply_($W3);$H8->apply_($Y3);$H8->apply_($c4);$H8->apply_($d4);$H8->apply_($f4);$H8->apply_($g4);$H8->apply_($h4);$H8->apply_($i4);$H8->apply_($j4);$H8->apply_($k4);$H8->apply_($m4);$H8->apply_($n4);$H8->apply_($o4);$H8->apply_($q4);$R8->apply_($F3);$X8->apply_($F3);$g9->apply_($j3);$g9->apply_($k3);$g9->apply_($l3);$g9->apply_($m3);$g9->apply_($n3);$g9->apply_($o3);$g9->apply_($p3);$g9->apply_($q3);$g9->apply_($r3);$g9->apply_($s3);$g9->apply_($t3);$g9->apply_($u3);$g9->apply_($w3);$g9->apply_($y3);$g9->apply_($A3);$g9->apply_($C3);$g9->apply_($D3);$g9->apply_($E3);$g9->apply_($F3);$g9->apply_($G3);$g9->apply_($H3);$g9->apply_($I3);$g9->apply_($K3);$g9->apply_($M3);$g9->apply_($O3);$g9->apply_($Q3);$g9->apply_($S3);$g9->apply_($T3);$g9->apply_($U3);$g9->apply_($V3);$g9->apply_($W3);$g9->apply_($Y3);$g9->apply_($c4);$g9->apply_($d4);$g9->apply_($f4);$g9->apply_($g4);$g9->apply_($h4);$g9->apply_($i4);$g9->apply_($j4);$g9->apply_($k4);$g9->apply_($m4);$g9->apply_($n4);$g9->apply_($o4);$g9->apply_($q4);$n9->apply_($j3);$n9->apply_($k3);$n9->apply_($l3);$n9->apply_($m3);$n9->apply_($n3);$n9->apply_($o3);$n9->apply_($p3);$n9->apply_($q3);$n9->apply_($r3);$n9->apply_($s3);$n9->apply_($t3);$n9->apply_($u3);$n9->apply_($w3);$n9->apply_($y3);$n9->apply_($A3);$n9->apply_($C3);$n9->apply_($D3);$n9->apply_($E3);$n9->apply_($F3);$n9->apply_($G3);$n9->apply_($H3);$n9->apply_($I3);$n9->apply_($K3);$n9->apply_($M3);$n9->apply_($O3);$n9->apply_($Q3);$n9->apply_($S3);$n9->apply_($T3);$n9->apply_($U3);$n9->apply_($V3);$n9->apply_($W3);$n9->apply_($Y3);$n9->apply_($c4);$n9->apply_($d4);$n9->apply_($f4);$n9->apply_($g4);$n9->apply_($h4);$n9->apply_($i4);$n9->apply_($j4);$n9->apply_($k4);$n9->apply_($m4);$n9->apply_($n4);$n9->apply_($o4);$n9->apply_($q4);$u9->apply_($j3);$u9->apply_($k3);$u9->apply_($l3);$u9->apply_($m3);$u9->apply_($n3);$u9->apply_($o3);$u9->apply_($p3);$u9->apply_($q3);$u9->apply_($r3);$u9->apply_($s3);$u9->apply_($t3);$u9->apply_($u3);$u9->apply_($w3);$u9->apply_($y3);$u9->apply_($A3);$u9->apply_($C3);$u9->apply_($D3);$u9->apply_($E3);$u9->apply_($F3);$u9->apply_($G3);$u9->apply_($H3);$u9->apply_($I3);$u9->apply_($K3);$u9->apply_($M3);$u9->apply_($O3);$u9->apply_($Q3);$u9->apply_($S3);$u9->apply_($U3);$u9->apply_($V3);$u9->apply_($W3);$u9->apply_($Y3);$u9->apply_($c4);$u9->apply_($d4);$u9->apply_($f4);$u9->apply_($g4);$u9->apply_($h4);$u9->apply_($i4);$u9->apply_($j4);$u9->apply_($k4);$u9->apply_($m4);$u9->apply_($n4);$u9->apply_($o4);$u9->apply_($q4);$C9->apply_($F3);$C9->apply_($T3);$Ta->apply_($L3);$tb->apply_($L3);$Bb->apply_($L3);$Bb->apply_($R3);$Zb->apply_($L3);$Zb->apply_($R3);$rc->apply_($L3);$rc->apply_($R3);$Hc->apply_($L3);$dd->apply_($L3);$ld->apply_($L3);$de->apply_($T3);$je->apply_($T3);$cf->apply_($j3);$cf->apply_($k3);$cf->apply_($l3);$cf->apply_($m3);$cf->apply_($n3);$cf->apply_($o3);$cf->apply_($p3);$cf->apply_($q3);$cf->apply_($r3);$cf->apply_($s3);$cf->apply_($t3);$cf->apply_($u3);$cf->apply_($w3);$cf->apply_($y3);$cf->apply_($A3);$cf->apply_($C3);$cf->apply_($D3);$cf->apply_($E3);$cf->apply_($G3);$cf->apply_($H3);$cf->apply_($I3);$cf->apply_($K3);$cf->apply_($M3);$cf->apply_($O3);$cf->apply_($Q3);$cf->apply_($S3);$cf->apply_($U3);$cf->apply_($W3);$cf->apply_($Y3);$cf->apply_($c4);$cf->apply_($d4);$cf->apply_($f4);$cf->apply_($g4);$cf->apply_($h4);$cf->apply_($i4);$cf->apply_($j4);$cf->apply_($k4);$cf->apply_($m4);$cf->apply_($n4);$cf->apply_($o4);$cf->apply_($q4);$kf->apply_($j3);$kf->apply_($k3);$kf->apply_($l3);$kf->apply_($m3);$kf->apply_($n3);$kf->apply_($o3);$kf->apply_($p3);$kf->apply_($q3);$kf->apply_($r3);$kf->apply_($s3);$kf->apply_($t3);$kf->apply_($u3);$kf->apply_($w3);$kf->apply_($y3);$kf->apply_($A3);$kf->apply_($C3);$kf->apply_($E3);$kf->apply_($G3);$kf->apply_($H3);$kf->apply_($I3);$kf->apply_($K3);$kf->apply_($M3);$kf->apply_($O3);$kf->apply_($Q3);$kf->apply_($S3);$kf->apply_($U3);$kf->apply_($W3);$kf->apply_($Y3);$kf->apply_($c4);$kf->apply_($d4);$kf->apply_($f4);$kf->apply_($g4);$kf->apply_($h4);$kf->apply_($i4);$kf->apply_($j4);$kf->apply_($k4);$kf->apply_($m4);$kf->apply_($n4);$kf->apply_($o4);$kf->apply_($q4);$tf->apply_($j3);$tf->apply_($k3);$tf->apply_($l3);$tf->apply_($m3);$tf->apply_($n3);$tf->apply_($o3);$tf->apply_($p3);$tf->apply_($q3);$tf->apply_($r3);$tf->apply_($s3);$tf->apply_($t3);$tf->apply_($u3);$tf->apply_($w3);$tf->apply_($y3);$tf->apply_($A3);$tf->apply_($C3);$tf->apply_($D3);$tf->apply_($E3);$tf->apply_($G3);$tf->apply_($H3);$tf->apply_($I3);$tf->apply_($K3);$tf->apply_($M3);$tf->apply_($O3);$tf->apply_($Q3);$tf->apply_($S3);$tf->apply_($U3);$tf->apply_($W3);$tf->apply_($Y3);$tf->apply_($c4);$tf->apply_($d4);$tf->apply_($f4);$tf->apply_($g4);$tf->apply_($h4);$tf->apply_($i4);$tf->apply_($j4);$tf->apply_($k4);$tf->apply_($m4);$tf->apply_($n4);$tf->apply_($o4);$tf->apply_($q4);$Df->apply_($j3);$Df->apply_($k3);$Df->apply_($l3);$Df->apply_($m3);$Df->apply_($n3);$Df->apply_($o3);$Df->apply_($p3);$Df->apply_($q3);$Df->apply_($r3);$Df->apply_($s3);$Df->apply_($t3);$Df->apply_($u3);$Df->apply_($w3);$Df->apply_($y3);$Df->apply_($A3);$Df->apply_($C3);$Df->apply_($D3);$Df->apply_($E3);$Df->apply_($G3);$Df->apply_($H3);$Df->apply_($I3);$Df->apply_($K3);$Df->apply_($M3);$Df->apply_($O3);$Df->apply_($Q3);$Df->apply_($S3);$Df->apply_($U3);$Df->apply_($W3);$Df->apply_($Y3);$Df->apply_($c4);$Df->apply_($d4);$Df->apply_($f4);$Df->apply_($g4);$Df->apply_($h4);$Df->apply_($i4);$Df->apply_($j4);$Df->apply_($k4);$Df->apply_($m4);$Df->apply_($n4);$Df->apply_($o4);$Df->apply_($q4);$Kf->apply_($j3);$Kf->apply_($k3);$Kf->apply_($l3);$Kf->apply_($m3);$Kf->apply_($n3);$Kf->apply_($o3);$Kf->apply_($p3);$Kf->apply_($q3);$Kf->apply_($r3);$Kf->apply_($s3);$Kf->apply_($t3);$Kf->apply_($u3);$Kf->apply_($w3);$Kf->apply_($y3);$Kf->apply_($A3);$Kf->apply_($C3);$Kf->apply_($D3);$Kf->apply_($E3);$Kf->apply_($G3);$Kf->apply_($H3);$Kf->apply_($I3);$Kf->apply_($K3);$Kf->apply_($M3);$Kf->apply_($O3);$Kf->apply_($Q3);$Kf->apply_($S3);$Kf->apply_($U3);$Kf->apply_($W3);$Kf->apply_($Y3);$Kf->apply_($c4);$Kf->apply_($d4);$Kf->apply_($f4);$Kf->apply_($g4);$Kf->apply_($h4);$Kf->apply_($i4);$Kf->apply_($j4);$Kf->apply_($k4);$Kf->apply_($m4);$Kf->apply_($n4);$Kf->apply_($o4);$Kf->apply_($q4);$Rf->apply_($j3);$Rf->apply_($k3);$Rf->apply_($l3);$Rf->apply_($m3);$Rf->apply_($n3);$Rf->apply_($o3);$Rf->apply_($p3);$Rf->apply_($q3);$Rf->apply_($r3);$Rf->apply_($s3);$Rf->apply_($t3);$Rf->apply_($u3);$Rf->apply_($w3);$Rf->apply_($y3);$Rf->apply_($A3);$Rf->apply_($C3);$Rf->apply_($D3);$Rf->apply_($E3);$Rf->apply_($G3);$Rf->apply_($H3);$Rf->apply_($I3);$Rf->apply_($K3);$Rf->apply_($M3);$Rf->apply_($O3);$Rf->apply_($Q3);$Rf->apply_($S3);$Rf->apply_($U3);$Rf->apply_($W3);$Rf->apply_($Y3);$Rf->apply_($c4);$Rf->apply_($d4);$Rf->apply_($f4);$Rf->apply_($g4);$Rf->apply_($h4);$Rf->apply_($i4);$Rf->apply_($j4);$Rf->apply_($k4);$Rf->apply_($m4);$Rf->apply_($n4);$Rf->apply_($o4);$Rf->apply_($q4);$Yf->apply_($j3);$Yf->apply_($k3);$Yf->apply_($l3);$Yf->apply_($m3);$Yf->apply_($n3);$Yf->apply_($o3);$Yf->apply_($p3);$Yf->apply_($q3);$Yf->apply_($r3);$Yf->apply_($s3);$Yf->apply_($t3);$Yf->apply_($u3);$Yf->apply_($w3);$Yf->apply_($y3);$Yf->apply_($A3);$Yf->apply_($C3);$Yf->apply_($D3);$Yf->apply_($E3);$Yf->apply_($G3);$Yf->apply_($H3);$Yf->apply_($I3);$Yf->apply_($K3);$Yf->apply_($M3);$Yf->apply_($O3);$Yf->apply_($Q3);$Yf->apply_($S3);$Yf->apply_($U3);$Yf->apply_($W3);$Yf->apply_($Y3);$Yf->apply_($c4);$Yf->apply_($d4);$Yf->apply_($f4);$Yf->apply_($g4);$Yf->apply_($h4);$Yf->apply_($i4);$Yf->apply_($j4);$Yf->apply_($k4);$Yf->apply_($m4);$Yf->apply_($n4);$Yf->apply_($o4);$Yf->apply_($q4);$hg->apply_($j3);$hg->apply_($k3);$hg->apply_($l3);$hg->apply_($m3);$hg->apply_($n3);$hg->apply_($o3);$hg->apply_($p3);$hg->apply_($q3);$hg->apply_($r3);$hg->apply_($s3);$hg->apply_($t3);$hg->apply_($u3);$hg->apply_($w3);$hg->apply_($y3);$hg->apply_($A3);$hg->apply_($C3);$hg->apply_($D3);$hg->apply_($E3);$hg->apply_($G3);$hg->apply_($H3);$hg->apply_($I3);$hg->apply_($K3);$hg->apply_($M3);$hg->apply_($O3);$hg->apply_($Q3);$hg->apply_($S3);$hg->apply_($U3);$hg->apply_($W3);$hg->apply_($Y3);$hg->apply_($c4);$hg->apply_($d4);$hg->apply_($f4);$hg->apply_($g4);$hg->apply_($h4);$hg->apply_($i4);$hg->apply_($j4);$hg->apply_($k4);$hg->apply_($m4);$hg->apply_($n4);$hg->apply_($o4);$hg->apply_($q4);$sg->apply_($j3);$sg->apply_($k3);$sg->apply_($l3);$sg->apply_($m3);$sg->apply_($n3);$sg->apply_($o3);$sg->apply_($p3);$sg->apply_($q3);$sg->apply_($r3);$sg->apply_($s3);$sg->apply_($t3);$sg->apply_($u3);$sg->apply_($w3);$sg->apply_($y3);$sg->apply_($A3);$sg->apply_($C3);$sg->apply_($E3);$sg->apply_($G3);$sg->apply_($H3);$sg->apply_($A);$sg->apply_($I3);$sg->apply_($K3);$sg->apply_($M3);$sg->apply_($O3);$sg->apply_($Q3);$sg->apply_($S3);$sg->apply_($T3);$sg->apply_($U3);$sg->apply_($V3);$sg->apply_($W3);$sg->apply_($Y3);$sg->apply_($c4);$sg->apply_($d4);$sg->apply_($f4);$sg->apply_($g4);$sg->apply_($h4);$sg->apply_($i4);$sg->apply_($k4);$sg->apply_($m4);$sg->apply_($n4);$sg->apply_($o4);$sg->apply_($q4);$zg->apply_($j3);$zg->apply_($k3);$zg->apply_($l3);$zg->apply_($m3);$zg->apply_($n3);$zg->apply_($o3);$zg->apply_($p3);$zg->apply_($q3);$zg->apply_($r3);$zg->apply_($s3);$zg->apply_($t3);$zg->apply_($u3);$zg->apply_($w3);$zg->apply_($y3);$zg->apply_($A3);$zg->apply_($C3);$zg->apply_($E3);$zg->apply_($G3);$zg->apply_($H3);$zg->apply_($I3);$zg->apply_($K3);$zg->apply_($M3);$zg->apply_($O3);$zg->apply_($Q3);$zg->apply_($S3);$zg->apply_($U3);$zg->apply_($W3);$zg->apply_($Y3);$zg->apply_($c4);$zg->apply_($d4);$zg->apply_($f4);$zg->apply_($g4);$zg->apply_($i4);$zg->apply_($k4);$zg->apply_($m4);$zg->apply_($n4);$zg->apply_($o4);$zg->apply_($q4);$nh->apply_($U2);$Fh->apply_($U2);$Uh->apply_($l3);$Uh->apply_($m3);$Uh->apply_($n3);$Uh->apply_($o3);$Uh->apply_($p3);$Uh->apply_($q3);$Uh->apply_($r3);$Uh->apply_($s3);$Uh->apply_($t3);$Uh->apply_($u3);$li->apply_($V2);$si->apply_($V2);$Li->apply_($W2);$Si->apply_($W2);$oj->apply_($W2);$Ij->apply_($W2);$Qj->apply_($W2);$ik->apply_($W2);$Fk->apply_($X2);$Fk->apply_($Z2);$Mk->apply_($X2);$Uk->apply_($X2);$ml->apply_($X2);$ml->apply_($Z2);$xl->apply_($X2);$Ll->apply_($X2);$Ll->apply_($Z2);$qm->apply_($o3);$Im->apply_($Y2);$Qm->apply_($Y2);$Xm->apply_($Y2);$pn->apply_($Y2);$xn->apply_($Y2);$Sn->apply_($Y2);$qo->apply_($Z2);$zo->apply_($Z2);$mp->apply_($np);$xp->apply_($c3);$Kp->apply_($c3);$Iq->apply_($f3);$Pq->apply_($f3);$fr->apply_($f3);$Br->apply_($v3);$Br->apply_($x3);$Br->apply_($z3);$Br->apply_($p4);$Nr->apply_($v3);$Nr->apply_($x3);$Nr->apply_($z3);$Nr->apply_($p4);$es->apply_($v3);$es->apply_($x3);$es->apply_($z3);$ys->apply_($v3);$ys->apply_($x3);$ys->apply_($z3);$Os->apply_($w3);$Os->apply_($y3);$Os->apply_($A3);$mt->apply_($x3);$tt->apply_($x3);$Dt->apply_($x3);$Pt->apply_($x3);$su->apply_($y3);$Lu->apply_($z3);$Su->apply_($z3);$uv->apply_($D3);$Yv->apply_($S);$gw->apply_($S);$qw->apply_($S);$yw->apply_($S);$Ew->apply_($S);$Kw->apply_($S);$Uw->apply_($S);$ix->apply_($H3);$Gx->apply_($A);$Ux->apply_($A);$dy->apply_($A);$my->apply_($A);$ty->apply_($A);$Ry->apply_($J3);$Yy->apply_($J3);$uz->apply_($J3);$Oz->apply_($J3);$pA->apply_($np);$eB->apply_($fB);$AB->apply_($fB);$TB->apply_($N3);$nC->apply_($N3);$IC->apply_($N3);$RC->apply_($N3);$dD->apply_($N3);$jE->apply_($fB);$sE->apply_($P3);$sE->apply_($C);$sE->apply_($H);$OE->apply_($R3);$XE->apply_($R3);$OF->apply_($np);$YF->apply_($V3);$gG->apply_($V3);$CG->apply_($X3);$CG->apply_($Z3);$MG->apply_($X3);$TG->apply_($X3);$jH->apply_($X3);$OH->apply_($C);$VH->apply_($C);$eI->apply_($C);$oI->apply_($C);$zI->apply_($C);$OI->apply_($d4);$WI->apply_($d4);$rJ->apply_($e4);$GJ->apply_($e4);$NJ->apply_($e4);$iK->apply_($H);$oK->apply_($H);$uK->apply_($H);$IK->apply_($np);$ni::self=$ZL;&$V($T);&$V($f1);&$V($l1);&$V($y1);&$V($L1);&$V($Y1);&$V($n2);&$V($J2);&$V($P2);&$V($F4);&$V($H4);&$J4($H4);&$V($Q4);&$V($m5);&$V($u5);&$V($G5);&$V($S5);&$V($g6);&$V($i6);&$J4($i6);&$V($o6);&$V($F6);&$V($Q6);&$V($e7);&$V($q7);&$V($K7);&$V($M7);&$J4($M7);&$V($h8);&$V($o8);&$V($z8);&$V($B8);&$J4($B8);&$V($H8);&$V($R8);&$V($X8);&$V($g9);&$V($n9);&$V($u9);&$V($C9);&$V($E9);&$J4($E9);&$V($K9);&$V($ga);&$V($oa);&$V($Fa);&$V($Ta);&$V($tb);&$V($Bb);&$V($Zb);&$V($rc);&$V($Hc);&$V($dd);&$V($ld);&$V($nd);&$J4($nd);&$V($td);&$V($Bd);&$V($Md);&$V($de);&$V($je);&$V($le);&$J4($le);&$V($Ge);&$V($Me);&$V($cf);&$V($ef);&$V($kf);&$V($tf);&$V($Df);&$V($Kf);&$V($Rf);&$V($Yf);&$V($hg);&$V($jg);&$V($lg);&$J4($lg);&$V($sg);&$V($zg);&$V($Bg);&$J4($Bg);&$V($Kg);&$J4($Kg);&$V($Og);&$J4($Og);&$V($Qg);&$J4($Qg);&$V($Sg);&$J4($Sg);&$V($Xg);&$J4($Xg);&$V($eh);&$J4($eh);&$V($nh);&$V($Fh);&$V($Hh);&$J4($Hh);&$V($Uh);&$V($Wh);&$J4($Wh);&$V($Yh);&$J4($Yh);&$V($li);&$V($si);&$V($ui);&$J4($ui);&$V($zi);&$J4($zi);&$V($Li);&$V($Si);&$V($oj);&$V($Ij);&$V($Qj);&$V($ik);&$V($kk);&$J4($kk);&$V($pk);&$J4($pk);&$V($Fk);&$V($Mk);&$V($Uk);&$V($ml);&$V($xl);&$V($Ll);&$V($Ol);&$J4($Ol);&$Rl($Ol);&$V($qm);&$V($sm);&$J4($sm);&$V($Im);&$V($Qm);&$V($Xm);&$V($pn);&$V($xn);&$V($Sn);&$V($Un);&$J4($Un);&$V($Zn);&$J4($Zn);&$V($qo);&$V($zo);&$V($Bo);&$J4($Bo);&$V($Go);&$J4($Go);&$V($mp);&$V($xp);&$V($Kp);&$V($Mp);&$J4($Mp);&$V($Rp);&$J4($Rp);&$V($kq);&$J4($kq);&$V($Iq);&$V($Pq);&$V($fr);&$V($hr);&$J4($hr);&$V($mr);&$J4($mr);&$V($Br);&$V($Nr);&$V($Pr);&$J4($Pr);&$V($es);&$V($ys);&$V($As);&$J4($As);&$Ds($As);&$V($Ks);&$J4($Ks);&$V($Os);&$V($Qs);&$J4($Qs);&$V($mt);&$V($tt);&$V($Dt);&$V($Pt);&$V($Ut);&$J4($Ut);&$Ds($Ut);&$Xt($Ut);&$V($su);&$V($uu);&$J4($uu);&$V($Lu);&$V($Su);&$V($Uu);&$J4($Uu);&$Ds($Uu);&$V($Zu);&$J4($Zu);&$V($iv);&$J4($iv);&$V($uv);&$V($wv);&$J4($wv);&$V($Cv);&$J4($Cv);&$V($Lv);&$J4($Lv);&$V($Yv);&$V($gw);&$V($qw);&$V($yw);&$V($Ew);&$V($Kw);&$V($Uw);&$V($Ww);&$J4($Ww);&$V($ix);&$V($kx);&$J4($kx);&$V($Gx);&$V($Ux);&$V($dy);&$V($my);&$V($ty);&$V($vy);&$J4($vy);&$V($Ay);&$J4($Ay);&$V($Ry);&$V($Yy);&$V($uz);&$V($Oz);&$V($Qz);&$J4($Qz);&$V($Vz);&$J4($Vz);&$V($pA);&$V($vA);&$J4($vA);&$V($eB);&$V($AB);&$V($TB);&$V($nC);&$V($IC);&$V($RC);&$V($dD);&$V($fD);&$J4($fD);&$V($kD);&$J4($kD);&$V($jE);&$V($sE);&$V($uE);&$J4($uE);&$V($zE);&$J4($zE);&$V($OE);&$V($XE);&$V($ZE);&$J4($ZE);&$V($gF);&$J4($gF);&$V($sF);&$J4($sF);&$V($OF);&$V($YF);&$V($gG);&$V($iG);&$J4($iG);&$V($oG);&$J4($oG);&$V($CG);&$V($EG);&$J4($EG);&$V($MG);&$V($TG);&$V($jH);&$V($lH);&$J4($lH);&$V($sH);&$J4($sH);&$V($uH);&$J4($uH);&$V($OH);&$V($VH);&$V($eI);&$V($oI);&$V($zI);&$V($BI);&$J4($BI);&$EI($BI);&$V($OI);&$V($WI);&$V($YI);&$J4($YI);&$V($rJ);&$V($GJ);&$V($NJ);&$V($PJ);&$J4($PJ);&$V($UJ);&$J4($UJ);&$V($iK);&$V($oK);&$V($uK);&$V($wK);&$J4($wK);&$V($BK);&$J4($BK);&$V($IK);&$V($QK);&$J4($QK);&$V($VK);&$J4($VK);&$V($gL);&$J4($gL);&$V($lL);&$J4($lL);&$V($qL);&$J4($qL);&$V($yL);&$J4($yL);&$V($CL);&$J4($CL);ni->run(@ARGV);
__DATA__
