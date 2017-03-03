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
$ni::self = bless {named => {}}, 'lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->resolve($_[0]) : $ni::self}
sub ni::eval {eval shift}
*{'lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'lib/fn::OVERLOAD'} = {};
*{'lib/fn::(bool'} = sub {1};
*{'lib/fn::()'}    = sub {};
*{'lib/fn::(&{}'} = sub {
  my $self = shift;
  $$self{fn} ||= $self->compile;
};
*{'lib/fn::compile'} = sub {
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
now foo->f == 'hi';#;$y=q#proto#;$z=q##;$A=q#lib/fn#;$B=bless({$t,$u,$v,$q,$w,$x,$y,$z},$A);$C=q#lib/test_case#;$D=bless({$n,$o,$p,$q,$r,$q,$s,$B},$C);$E=q#todo#;$F=q#document this further#;$G=[$F];$H=q#lib/todo#;$I=bless({$E,$G},$H);$J=[$l,$m,$D,$I];$K=q#classes#;$L=q#ni implements a Smalltalk 80-style metaclass system with a couple of
differences. First, ni's classes are slice unions and as such don't
support colliding methods; and second, they support multiple inheritance.
These two points are related: method overriding isn't in the picture,
which makes multiple inheritance straightforward to implement.#;$M=[$F];$N=bless({$E,$M},$H);$O=[$K,$L,$N];$P=[$h,$k,$J,$O];$Q=q#name#;$R=q#/class#;$S=q#lib/doc#;$T=bless({$e,$P,$Q,$R},$S);$U=q#my $s = shift; ni->def($s->name, $s)#;$V=bless({$w,$U,$y,$z},$A);$W=q#ni.doc:/fabric#;$X=q#Abstractions to bridge the gaps between separate machines and processes.
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
ni('file:tmp1')->rm;#;$E2=bless({$t,$C2,$v,$q,$w,$D2,$y,$z},$A);$F2=bless({$n,$B2,$p,$q,$r,$q,$s,$E2},$C);$G2=[$i,$u2,$z2,$A2,$F2];$H2=[$q2,$t2,$G2];$I2=q#/io/file#;$J2=bless({$e,$H2,$Q,$I2},$S);$K2=q#ni.doc:/io/file_update_fd#;$L2=q#A write fd that performs a file rename upon closing.#;$M2=[$i,$L2];$N2=[$M2];$O2=q#/io/file_update_fd#;$P2=bless({$e,$N2,$Q,$O2},$S);$Q2=q#ni.doc:/io/object#;$R2=q#TODO#;$S2=q#referent#;$T2=q#applied_to#;$U2=q#io/buffer#;$V2=q#io/cat#;$W2=q#io/exec#;$X2=q#io/fd#;$Y2=q#io/file#;$Z2=q#io/file_update_fd#;$c3=q#io/null#;$d3=q#io/object#;$e3=q#io/pid#;$f3=q#io/str#;$g3={$U2,1,$V2,1,$W2,1,$X2,1,$Y2,1,$Z2,1,$c3,1,$d3,1,$e3,1,$f3,1};$h3=q#/io/object#;$i3=q#slices#;$j3=q#class#;$k3=q#class.c#;$l3=q#io/buffer.c#;$m3=q#io/cat.c#;$n3=q#io/exec.c#;$o3=q#io/fd.c#;$p3=q#io/file.c#;$q3=q#io/file_update_fd.c#;$r3=q#io/null.c#;$s3=q#io/object.c#;$t3=q#io/pid.c#;$u3=q#io/str.c#;$v3=q#io/transfer#;$w3=q#io/transfer.c#;$x3=q#io/transfer_async#;$y3=q#io/transfer_async.c#;$z3=q#io/transfer_sync#;$A3=q#io/transfer_sync.c#;$B3=q#lib/behavior#;$C3=q#lib/behavior.c#;$D3=q#lib/branch#;$E3=q#lib/branch.c#;$F3=q#lib/dataslice#;$G3=q#lib/dataslice.c#;$H3=q#lib/doc.c#;$I3=q#lib/fn.c#;$J3=q#lib/future#;$K3=q#lib/future.c#;$L3=q#lib/image#;$M3=q#lib/image.c#;$N3=q#lib/ni#;$O3=q#lib/ni.c#;$P3=q#lib/object_metadata#;$Q3=q#lib/object_metadata.c#;$R3=q#lib/quote_simple#;$S3=q#lib/quote_simple.c#;$T3=q#lib/slice#;$U3=q#lib/slice.c#;$V3=q#lib/tag#;$W3=q#lib/tag.c#;$X3=q#lib/test_assert_eq#;$Y3=q#lib/test_assert_eq.c#;$Z3=q#lib/test_assertion#;$c4=q#lib/test_assertion.c#;$d4=q#lib/test_case.c#;$e4=q#lib/test_value#;$f4=q#lib/test_value.c#;$g4=q#lib/todo.c#;$h4=q#metaclass#;$i4=q#metaclass.c#;$j4=q#module#;$k4=q#module.c#;$l4=q#object#;$m4=q#object.c#;$n4=q#semantic/dimension#;$o4=q#semantic/dimension.c#;$p4=q#semantic/task#;$q4=q#semantic/task.c#;$r4={$j3,1,$k3,1,$U2,1,$l3,1,$V2,1,$m3,1,$W2,1,$n3,1,$X2,1,$o3,1,$Y2,1,$p3,1,$Z2,1,$q3,1,$c3,1,$r3,1,$d3,1,$s3,1,$e3,1,$t3,1,$f3,1,$u3,1,$v3,1,$w3,1,$x3,1,$y3,1,$z3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$F3,1,$G3,1,$S,1,$H3,1,$A,1,$I3,1,$J3,1,$K3,1,$L3,1,$M3,1,$N3,1,$O3,1,$P3,1,$Q3,1,$R3,1,$S3,1,$T3,1,$U3,1,$V3,1,$W3,1,$X3,1,$Y3,1,$Z3,1,$c4,1,$C,1,$d4,1,$e4,1,$f4,1,$H,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$l4,1,$m4,1,$n4,1,$o4,1,$p4,1,$q4,1};$s4=q#/object#;$t4={};$u4=q#ctor#;$v4=q#dtor#;$w4=q#methods#;$x4=q#DESTROY#;$y4=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$z4=bless({$w,$y4,$y,$z},$A);$A4=q#ni 'ni:/' . ref shift#;$B4=bless({$w,$A4,$y,$z},$A);$C4={$x4,$z4,$j3,$B4};$D4=q#/lib/instance.b#;$E4=bless({$T2,$t4,$u4,$q,$v4,$q,$w4,$C4,$Q,$D4},$T3);$F4=[$E4];$G4=bless({$T2,$r4,$Q,$s4,$i3,$F4},$m4);$H4=q#my $s = shift; $s->apply($s->package)#;$I4=bless({$w,$H4,$y,$z},$A);$J4={};$K4=q#(bool#;$L4=[];$M4=bless({$t,$L4,$v,$q,$w,1,$y,$z},$A);$N4={$K4,$M4};$O4=q#/io/object_ops.b#;$P4=bless({$T2,$J4,$u4,$q,$v4,$q,$w4,$N4,$Q,$O4},$T3);$Q4={};$R4=q#die#;$S4=[];$T4=q#shift; die join " ", @_#;$U4=bless({$t,$S4,$v,$q,$w,$T4,$y,$z},$A);$V4=q#io_check#;$W4=[];$X4=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$Y4=bless({$t,$W4,$v,$q,$w,$X4,$y,$z},$A);$Z4=q#io_check_defined#;$c5=[];$d5=q#shift->io_check(sub {defined shift}, @_)#;$e5=bless({$t,$c5,$v,$q,$w,$d5,$y,$z},$A);$f5=q#io_check_true#;$g5=[];$h5=q#shift->io_check(sub {shift}, @_)#;$i5=bless({$t,$g5,$v,$q,$w,$h5,$y,$z},$A);$j5={$R4,$U4,$V4,$Y4,$Z4,$e5,$f5,$i5};$k5=q#/io/object_checks.b#;$l5=bless({$T2,$Q4,$u4,$q,$v4,$q,$w4,$j5,$Q,$k5},$T3);$m5={};$n5=q#(+#;$o5=[];$p5=q#ni('ni:/io/cat')->new(@_[0, 1])#;$q5=bless({$t,$o5,$v,$q,$w,$p5,$y,$z},$A);$r5={$n5,$q5};$s5=q#/io/object_constructors.b#;$t5=bless({$T2,$m5,$u4,$q,$v4,$q,$w4,$r5,$Q,$s5},$T3);$u5={};$v5=q#read_all#;$w5=[];$x5=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$y5=bless({$t,$w5,$v,$q,$w,$x5,$y,$z},$A);$z5=q#write_all#;$A5=[];$B5=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$C5=bless({$t,$A5,$v,$q,$w,$B5,$y,$z},$A);$D5={$v5,$y5,$z5,$C5};$E5=q#/io/object_memory.b#;$F5=bless({$T2,$u5,$u4,$q,$v4,$q,$w4,$D5,$Q,$E5},$T3);$G5={};$H5=q#connect_sync#;$I5=[];$J5=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$K5=bless({$t,$I5,$v,$q,$w,$J5,$y,$z},$A);$L5=q#into_sync#;$M5=[];$N5=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$O5=bless({$t,$M5,$v,$q,$w,$N5,$y,$z},$A);$P5={$H5,$K5,$L5,$O5};$Q5=q#/io/object_transfer_sync.b#;$R5=bless({$T2,$G5,$u4,$q,$v4,$q,$w4,$P5,$Q,$Q5},$T3);$S5={};$T5=q#connect_async#;$U5=[];$V5=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$W5=bless({$t,$U5,$v,$q,$w,$V5,$y,$z},$A);$X5=q#into_async#;$Y5=[];$Z5=q#ni('ni:/io/transfer_async')->new(@_)->run#;$c6=bless({$t,$Y5,$v,$q,$w,$Z5,$y,$z},$A);$d6={$T5,$W5,$X5,$c6};$e6=q#/io/object_transfer_async.b#;$f6=bless({$T2,$S5,$u4,$q,$v4,$q,$w4,$d6,$Q,$e6},$T3);$g6=[$G4,$P4,$l5,$t5,$F5,$R5,$f6,$f6,$R5,$f6,$R5];$h6=bless({$T2,$g3,$Q,$h3,$i3,$g6},$s3);$i6=q#migrate die() into /lib/ as a base behavior#;$j6=[$i6];$k6=bless({$S2,$h6,$E,$j6},$H);$l6=[$R2,$k6];$m6=[$l6];$n6=bless({$e,$m6,$Q,$h3},$S);$o6=q#ni.doc:/io/pid#;$p6=q#eg#;$q6=[];$r6={$e3,1};$s6=q#/io/pid#;$t6={};$u6=q#pid#;$v6=[];$w6=q#shift->{'pid'}#;$x6=bless({$t,$v6,$v,$q,$w,$w6,$y,$z},$A);$y6=q#status#;$z6=[];$A6=q#shift->{'status'}#;$B6=bless({$t,$z6,$v,$q,$w,$A6,$y,$z},$A);$C6={$u6,$x6,$y6,$B6};$D6=q#/io/pid_readers.b#;$E6=bless({$T2,$t6,$u4,$q,$v4,$q,$w4,$C6,$Q,$D6},$T3);$F6={};$G6=[];$H6=q#shift->await#;$I6=bless({$t,$G6,$v,$q,$w,$H6,$y,$z},$A);$J6=q#instantiate#;$K6=[];$L6=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$M6=bless({$t,$K6,$v,$q,$w,$L6,$y,$z},$A);$N6={$J6,$M6};$O6=q#/io/pid_init.b#;$P6=bless({$T2,$F6,$u4,$q,$v4,$I6,$w4,$N6,$Q,$O6},$T3);$Q6={};$R6=q#await#;$S6=[];$T6=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$U6=bless({$t,$S6,$v,$q,$w,$T6,$y,$z},$A);$V6=q#running#;$W6=[];$X6=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$Y6=bless({$t,$W6,$v,$q,$w,$X6,$y,$z},$A);$Z6={$R6,$U6,$V6,$Y6};$c7=q#/io/pid_wait.b#;$d7=bless({$T2,$Q6,$u4,$q,$v4,$q,$w4,$Z6,$Q,$c7},$T3);$e7={};$f7=q#read#;$g7=[];$h7=q#shift->stdout->read(@_)#;$i7=bless({$t,$g7,$v,$q,$w,$h7,$y,$z},$A);$j7=q#write#;$k7=[];$l7=q#shift->stdin->write(@_)#;$m7=bless({$t,$k7,$v,$q,$w,$l7,$y,$z},$A);$n7={$f7,$i7,$j7,$m7};$o7=q#/io/pid_io.b#;$p7=bless({$T2,$e7,$u4,$q,$v4,$q,$w4,$n7,$Q,$o7},$T3);$q7={};$r7=q#fd#;$s7=[];$t7=q#$_[0]->{external_fds}{$_[1]}#;$u7=bless({$t,$s7,$v,$q,$w,$t7,$y,$z},$A);$v7=q#stderr#;$w7=[];$x7=q#shift->fd(2)#;$y7=bless({$t,$w7,$v,$q,$w,$x7,$y,$z},$A);$z7=q#stdin#;$A7=[];$B7=q#shift->fd(0)#;$C7=bless({$t,$A7,$v,$q,$w,$B7,$y,$z},$A);$D7=q#stdout#;$E7=[];$F7=q#shift->fd(1)#;$G7=bless({$t,$E7,$v,$q,$w,$F7,$y,$z},$A);$H7={$r7,$u7,$v7,$y7,$z7,$C7,$D7,$G7};$I7=q#/io/pid_accessors.b#;$J7=bless({$T2,$q7,$u4,$q,$v4,$q,$w4,$H7,$Q,$I7},$T3);$K7=[$h6,$E6,$P6,$d7,$p7,$J7];$L7=bless({$T2,$r6,$Q,$s6,$i3,$K7},$t3);$M7=[];$N7=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$O7=bless({$t,$M7,$v,$q,$w,$N7,$y,$z},$A);$P7=bless({$n,$q6,$p,$q,$r,$q,$S2,$L7,$s,$O7},$C);$Q7=[$p6,$P7];$R7=[];$S7=[];$T7=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$U7=bless({$t,$S7,$v,$q,$w,$T7,$y,$z},$A);$V7=bless({$n,$R7,$p,$q,$r,$q,$S2,$L7,$s,$U7},$C);$W7=[$p6,$V7];$X7=[];$Y7=[];$Z7=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$c8=bless({$t,$Y7,$v,$q,$w,$Z7,$y,$z},$A);$d8=bless({$n,$X7,$p,$q,$r,$q,$S2,$L7,$s,$c8},$C);$e8=[$p6,$d8];$f8=[$Q7,$W7,$e8];$g8=bless({$e,$f8,$Q,$s6},$S);$h8=q#ni.doc:/lib#;$i8=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$j8=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$k8=[$i,$i8,$j8];$l8=[$k8];$m8=q#/lib#;$n8=bless({$e,$l8,$Q,$m8},$S);$o8=q#ni.doc:/lib/dataslice#;$p8={$F3,1};$q8=q#/lib/dataslice#;$r8={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$B3,1,$C3,1,$D3,1,$E3,1,$F3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$T3,1,$U3,1,$V3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$s8=q#/lib/behavior#;$t8={};$u8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$v8=bless({$w,$u8,$y,$z},$A);$w8={$e,$v8};$x8=q#/lib/documentable.b#;$y8=bless({$T2,$t8,$u4,$q,$v4,$q,$w4,$w8,$Q,$x8},$T3);$z8=[$G4,$y8];$A8=bless({$T2,$r8,$Q,$s8,$i3,$z8},$C3);$B8={};$C8=q#$_[0]->namespace . ":" . $_[0]->{name}#;$D8=bless({$w,$C8,$y,$z},$A);$E8={$Q,$D8};$F8=q#/lib/named.b#;$G8=bless({$T2,$B8,$u4,$V,$v4,$q,$w4,$E8,$Q,$F8},$T3);$H8={};$I8=q#apply#;$J8=q#shift->apply_(@_)#;$K8=bless({$w,$J8,$y,$z},$A);$L8=q#apply_#;$M8=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$N8=bless({$w,$M8,$y,$z},$A);$O8={$I8,$K8,$L8,$N8};$P8=q#/lib/dataslice.b#;$Q8=bless({$T2,$H8,$u4,$q,$v4,$q,$w4,$O8,$Q,$P8},$T3);$R8={};$S8=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$T8=bless({$w,$S8,$y,$z},$A);$U8={$J6,$T8};$V8=q#/lib/dataslice_init.b#;$W8=bless({$T2,$R8,$u4,$q,$v4,$q,$w4,$U8,$Q,$V8},$T3);$X8=q#/lib/named_in_ni.b#;$Y8=q#/lib/namespaced.b#;$Z8=q#/lib/resolver.b#;$c9=q#/lib/slice_serialize.b#;$d9=[$A8,$G8,$Q8,$W8,$X8,$Y8,$Z8,$c9];$e9=bless({$T2,$p8,$Q,$q8,$i3,$d9},$G3);$f9=q#Fix serialization for dataslices#;$g9=[$f9];$h9=bless({$S2,$e9,$E,$g9},$H);$i9=[$R2,$h9];$j9=[$i9];$k9=bless({$e,$j9,$Q,$q8},$S);$l9=q#ni.doc:/lib/doc#;$m9=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$n9=[$f,$m9];$o9=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$p9=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$q9=[];$r9=[];$s9=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$t9=bless({$t,$r9,$v,$q,$w,$s9,$y,$z},$A);$u9=bless({$n,$q9,$p,$q,$r,$q,$s,$t9},$C);$v9=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$w9=[];$x9=[];$y9=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$z9=bless({$t,$x9,$v,$q,$w,$y9,$y,$z},$A);$A9=bless({$n,$w9,$p,$q,$r,$q,$s,$z9},$C);$B9=[$i,$o9,$p9,$u9,$v9,$A9];$C9=[$n9,$B9];$D9=q#/lib/doc#;$E9=bless({$e,$C9,$Q,$D9},$S);$F9=q#ni.doc:/lib/future#;$G9=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$H9=[];$I9=[];$J9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$K9=bless({$t,$I9,$v,$q,$w,$J9,$y,$z},$A);$L9=bless({$n,$H9,$p,$q,$r,$q,$s,$K9},$C);$M9=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$N9=[];$O9=[];$P9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$Q9=bless({$t,$O9,$v,$q,$w,$P9,$y,$z},$A);$R9=bless({$n,$N9,$p,$q,$r,$q,$s,$Q9},$C);$S9=[$i,$G9,$L9,$M9,$R9];$T9=[$S9];$U9=q#/lib/future#;$V9=bless({$e,$T9,$Q,$U9},$S);$W9=q#ni.doc:/lib/image#;$X9=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$Y9=[$f,$X9];$Z9=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$ca=[$i,$Z9];$da={$L3,1};$ea=q#/lib/image#;$fa={};$ga=[];$ha=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$ia=bless({$t,$ga,$v,$q,$w,$ha,$y,$z},$A);$ja={$J6,$ia};$ka=q#/lib/image_init.b#;$la=bless({$T2,$fa,$u4,$q,$v4,$q,$w4,$ja,$Q,$ka},$T3);$ma={};$na=q#boot_side_effect#;$oa=[];$pa=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$qa=bless({$t,$oa,$v,$q,$w,$pa,$y,$z},$A);$ra=q#finalizer#;$sa=[];$ta=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$ua=bless({$t,$sa,$v,$q,$w,$ta,$y,$z},$A);$va=q#io#;$wa=[];$xa=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$ya=bless({$t,$wa,$v,$q,$w,$xa,$y,$z},$A);$za=q#reconstruction#;$Aa=[];$Ba=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Ca=bless({$t,$Aa,$v,$q,$w,$Ba,$y,$z},$A);$Da=q#side_effect#;$Ea=[];$Fa=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ga=bless({$t,$Ea,$v,$q,$w,$Fa,$y,$z},$A);$Ha={$na,$qa,$ra,$ua,$va,$ya,$za,$Ca,$Da,$Ga};$Ia=q#/lib/image_quoting.b#;$Ja=bless({$T2,$ma,$u4,$q,$v4,$q,$w4,$Ha,$Q,$Ia},$T3);$Ka=q#/lib/quote_code_fail.b#;$La=q#/lib/quote_values.b#;$Ma=q#/lib/quote_objects.b#;$Na=q#/lib/quote_circular_addressed.b#;$Oa=q#/lib/quote_gensym_identity.b#;$Pa=q#/lib/gensym_generator_compact.b#;$Qa=[$G4,$la,$Ja,$Ka,$La,$Ma,$Na,$Oa,$Pa];$Ra=bless({$T2,$da,$Q,$ea,$i3,$Qa},$M3);$Sa=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$Ta=[$Sa];$Ua=bless({$S2,$Ra,$E,$Ta},$H);$Va=[$R2,$Ua];$Wa=[$Y9,$ca,$Va];$Xa=bless({$e,$Wa,$Q,$ea},$S);$Ya=q#ni.doc:/lib/ni#;$Za=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$cb=[$f,$Za];$db=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$eb=[$i,$db];$fb=[$cb,$eb];$gb=q#/lib/ni#;$hb=bless({$e,$fb,$Q,$gb},$S);$ib=q#ni.doc:/lib/quote_simple#;$jb=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$kb=[];$lb=[];$mb=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$nb=bless({$t,$lb,$v,$q,$w,$mb,$y,$z},$A);$ob=bless({$n,$kb,$p,$q,$r,$q,$s,$nb},$C);$pb=[$i,$jb,$ob];$qb=[$pb];$rb=q#/lib/quote_simple#;$sb=bless({$e,$qb,$Q,$rb},$S);$tb=q#ni.doc:/lib/slice#;$ub=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$vb=[$f,$ub];$wb={$T3,1};$xb=q#/lib/slice#;$yb=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$zb=bless({$w,$yb,$y,$z},$A);$Ab=q#local $_;
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
$self;#;$Bb=bless({$w,$Ab,$y,$z},$A);$Cb=q#lib/slice::apply#;$Db=q#lib/slice::apply_#;$Eb={};$Fb={$I8,$zb,$L8,$Bb};$Gb=q#/lib/slice.b#;$Hb=bless({$T2,$Eb,$w4,$Fb,$Q,$Gb},$T3);$Ib={};$Jb=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$Kb=bless({$w,$Jb,$y,$z},$A);$Lb={$J6,$Kb};$Mb=q#/lib/slice_init.b#;$Nb=bless({$T2,$Ib,$w4,$Lb,$Q,$Mb},$T3);$Ob={};$Pb=q#serialize#;$Qb=[];$Rb=q#local $_;
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
$g;#;$Sb=bless({$t,$Qb,$v,$q,$w,$Rb,$y,$z},$A);$Tb={$Pb,$Sb};$Ub=bless({$T2,$Ob,$u4,$q,$v4,$q,$w4,$Tb,$Q,$c9},$T3);$Vb=[$A8,$G8,$Hb,$Nb,$Ub];$Wb=bless({$T2,$wb,$Q,$xb,$i3,$Vb},$U3);$Xb=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$Yb=[$Xb];$Zb=bless({$S2,$Wb,$E,$Yb},$H);$cc=[$R2,$Zb];$dc=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$ec=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$fc=[];$gc=[];$hc=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$ic=bless({$t,$gc,$v,$q,$w,$hc,$y,$z},$A);$jc=bless({$n,$fc,$p,$q,$r,$q,$s,$ic},$C);$kc=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$lc=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$mc=[];$nc=[];$oc=q#my $instances = 0;
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
now $instances == 0;#;$pc=bless({$t,$nc,$v,$q,$w,$oc,$y,$z},$A);$qc=bless({$n,$mc,$p,$q,$r,$q,$s,$pc},$C);$rc=[$i,$dc,$ec,$jc,$kc,$lc,$qc];$sc=[$vb,$cc,$rc];$tc=bless({$e,$sc,$Q,$xb},$S);$uc=q#ni.doc:/semantic#;$vc=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$wc=[$i,$vc];$xc=[$wc];$yc=q#/semantic#;$zc=bless({$e,$xc,$Q,$yc},$S);$Ac=q#ni:/class#;$Bc={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Cc={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Dc=q#/module#;$Ec=q#/lib/perlbranch.b#;$Fc=q#/lib/branch.b#;$Gc=[$Fc,$F8,$X8,$Y8,$Z8];$Hc=bless({$Q,$Ec,$i3,$Gc},$V3);$Ic={};$Jc=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$Kc=bless({$w,$Jc,$y,$z},$A);$Lc={$J6,$Kc};$Mc=q#/lib/class_init.b#;$Nc=bless({$T2,$Ic,$u4,$I4,$v4,$q,$w4,$Lc,$Q,$Mc},$T3);$Oc=q#/lib/definition.b#;$Pc=[$Hc,$Nc,$s4,$s8,$Oc];$Qc=bless({$T2,$Cc,$Q,$Dc,$i3,$Pc},$k4);$Rc={};$Sc=q#new#;$Tc=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
&$_($self) for @{ref($self) . "::ctors"};
$self;#;$Uc=bless({$w,$Tc,$y,$z},$A);$Vc={$Sc,$Uc};$Wc=q#/lib/instantiable.b#;$Xc=bless({$T2,$Rc,$w4,$Vc,$Q,$Wc},$T3);$Yc={};$Zc=q#child#;$cd=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$dd=bless({$w,$cd,$y,$z},$A);$ed={$Zc,$dd};$fd=q#/lib/subclass.b#;$gd=bless({$T2,$Yc,$u4,$q,$v4,$q,$w4,$ed,$Q,$fd},$T3);$hd=[$Qc,$Xc,$Nc,$Dc,$gd];$id=bless({$T2,$Bc,$Q,$R,$i3,$hd},$k3);$jd=q#ni:/class.c#;$kd={$k3,1,$o4,1};$ld=q#/class.c#;$md={$k3,1,$k4,1,$o4,1};$nd=q#/module.c#;$od={$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$k4,1,$m4,1,$o4,1,$q4,1};$pd=q#/object.c#;$qd=[$R];$rd=bless({$T2,$od,$Q,$pd,$i3,$qd},$h4);$sd=q#/lib/behavior.c#;$td=[$rd,$Xc,$sd];$ud=bless({$T2,$md,$Q,$nd,$i3,$td},$h4);$vd=[$ud];$wd=bless({$T2,$kd,$Q,$ld,$i3,$vd},$h4);$xd=q#ni:/fabric#;$yd=q#fabric#;$zd={$yd,1};$Ad=[];$Bd=bless({$T2,$zd,$Q,$e1,$i3,$Ad},$j4);$Cd=q#ni:/io#;$Dd={$va,1};$Ed=[];$Fd=bless({$T2,$Dd,$Q,$k1,$i3,$Ed},$j4);$Gd=q#ni:/io/buffer#;$Hd={$U2,1};$Id={};$Jd=[];$Kd=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Ld=bless({$t,$Jd,$v,$q,$w,$Kd,$y,$z},$A);$Md={$J6,$Ld};$Nd=q#/io/buffer_init.b#;$Od=bless({$T2,$Id,$u4,$q,$v4,$q,$w4,$Md,$Q,$Nd},$T3);$Pd={};$Qd=[];$Rd=q#my $self = shift;
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
}#;$Sd=bless({$t,$Qd,$v,$q,$w,$Rd,$y,$z},$A);$Td=q#read_capacity#;$Ud=[];$Vd=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Wd=bless({$t,$Ud,$v,$q,$w,$Vd,$y,$z},$A);$Xd=[];$Yd=q#my $self = shift;
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
}#;$Zd=bless({$t,$Xd,$v,$q,$w,$Yd,$y,$z},$A);$ce=q#write_capacity#;$de=[];$ee=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$fe=bless({$t,$de,$v,$q,$w,$ee,$y,$z},$A);$ge={$f7,$Sd,$Td,$Wd,$j7,$Zd,$ce,$fe};$he=q#/io/buffer_io.b#;$ie=bless({$T2,$Pd,$u4,$q,$v4,$q,$w4,$ge,$Q,$he},$T3);$je=[$h6,$Od,$ie];$ke=bless({$T2,$Hd,$Q,$x1,$i3,$je},$l3);$le=q#ni:/io/buffer.c#;$me={$l3,1};$ne=q#/io/buffer.c#;$oe={$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1};$pe=q#/io/object.c#;$qe={};$re=q#def_transfer_method#;$se=[];$te=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$ue=bless({$t,$se,$v,$q,$w,$te,$y,$z},$A);$ve={$re,$ue};$we=q#/io/object.c_transfer_def.b#;$xe=bless({$T2,$qe,$u4,$q,$v4,$q,$w4,$ve,$Q,$we},$T3);$ye=[$rd,$xe];$ze=bless({$T2,$oe,$Q,$pe,$i3,$ye},$h4);$Ae=[$ze];$Be=bless({$T2,$me,$Q,$ne,$i3,$Ae},$h4);$Ce=q#ni:/io/buffer_init.b#;$De=q#ni:/io/buffer_io.b#;$Ee=q#ni:/io/cat#;$Fe={$V2,1};$Ge={};$He=[];$Ie=q#shift; +{fs => [@_]}#;$Je=bless({$t,$He,$v,$q,$w,$Ie,$y,$z},$A);$Ke={$J6,$Je};$Le=q#/io/cat_init.b#;$Me=bless({$T2,$Ge,$u4,$q,$v4,$q,$w4,$Ke,$Q,$Le},$T3);$Ne={};$Oe=[];$Pe=q#my $fs = shift->{fs};
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
$total_read;#;$Qe=bless({$t,$Oe,$v,$q,$w,$Pe,$y,$z},$A);$Re={$f7,$Qe};$Se=q#/io/cat_read.b#;$Te=bless({$T2,$Ne,$u4,$q,$v4,$q,$w4,$Re,$Q,$Se},$T3);$Ue=[$h6,$Me,$Te];$Ve=bless({$T2,$Fe,$Q,$K1,$i3,$Ue},$m3);$We=q#ni:/io/cat.c#;$Xe={$m3,1};$Ye=q#/io/cat.c#;$Ze=[$ze];$cf=bless({$T2,$Xe,$Q,$Ye,$i3,$Ze},$h4);$df=q#ni:/io/cat_init.b#;$ef=q#ni:/io/cat_read.b#;$ff=q#ni:/io/exec#;$gf={$W2,1};$hf={};$if=q#argv#;$jf=[];$kf=q#shift->{'argv'}#;$lf=bless({$t,$jf,$v,$q,$w,$kf,$y,$z},$A);$mf={$if,$lf};$nf=q#/io/exec_ro.b#;$of=bless({$T2,$hf,$u4,$q,$v4,$q,$w4,$mf,$Q,$nf},$T3);$pf={};$qf=[];$rf=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$sf=bless({$t,$qf,$v,$q,$w,$rf,$y,$z},$A);$tf={$J6,$sf};$uf=q#/io/exec_init.b#;$vf=bless({$T2,$pf,$u4,$q,$v4,$q,$w4,$tf,$Q,$uf},$T3);$wf={};$xf=q#connect#;$yf=[];$zf=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Af=bless({$t,$yf,$v,$q,$w,$zf,$y,$z},$A);$Bf=q#in_pipe#;$Cf=[];$Df=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Ef=bless({$t,$Cf,$v,$q,$w,$Df,$y,$z},$A);$Ff=q#out_pipe#;$Gf=[];$Hf=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$If=bless({$t,$Gf,$v,$q,$w,$Hf,$y,$z},$A);$Jf=q#setup_stdio#;$Kf=[];$Lf=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Mf=bless({$t,$Kf,$v,$q,$w,$Lf,$y,$z},$A);$Nf={$xf,$Af,$Bf,$Ef,$Ff,$If,$Jf,$Mf};$Of=q#/io/exec_io_setup.b#;$Pf=bless({$T2,$wf,$u4,$q,$v4,$q,$w4,$Nf,$Q,$Of},$T3);$Qf={};$Rf=q#binds_fd#;$Sf=[];$Tf=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$Uf=bless({$t,$Sf,$v,$q,$w,$Tf,$y,$z},$A);$Vf=[];$Wf=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Xf=bless({$t,$Vf,$v,$q,$w,$Wf,$y,$z},$A);$Yf=[];$Zf=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$cg=bless({$t,$Yf,$v,$q,$w,$Zf,$y,$z},$A);$dg=[];$eg=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$fg=bless({$t,$dg,$v,$q,$w,$eg,$y,$z},$A);$gg=[];$hg=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$ig=bless({$t,$gg,$v,$q,$w,$hg,$y,$z},$A);$jg={$Rf,$Uf,$r7,$Xf,$v7,$cg,$z7,$fg,$D7,$ig};$kg=q#/io/exec_io_accessors.b#;$lg=bless({$T2,$Qf,$u4,$q,$v4,$q,$w4,$jg,$Q,$kg},$T3);$mg={};$ng=q#env#;$og=[];$pg=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$qg=bless({$t,$og,$v,$q,$w,$pg,$y,$z},$A);$rg={$ng,$qg};$sg=q#/io/exec_env.b#;$tg=bless({$T2,$mg,$u4,$q,$v4,$q,$w4,$rg,$Q,$sg},$T3);$ug={};$vg=q#exec#;$wg=[];$xg=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$yg=bless({$t,$wg,$v,$q,$w,$xg,$y,$z},$A);$zg=q#fork#;$Ag=[];$Bg=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Cg=bless({$t,$Ag,$v,$q,$w,$Bg,$y,$z},$A);$Dg=q#move_fds#;$Eg=[];$Fg=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Gg=bless({$t,$Eg,$v,$q,$w,$Fg,$y,$z},$A);$Hg={$vg,$yg,$zg,$Cg,$Dg,$Gg};$Ig=q#/io/exec_fork.b#;$Jg=bless({$T2,$ug,$u4,$q,$v4,$q,$w4,$Hg,$Q,$Ig},$T3);$Kg=[$h6,$of,$vf,$Pf,$lg,$tg,$Jg];$Lg=bless({$T2,$gf,$Q,$X1,$i3,$Kg},$n3);$Mg=q#ni:/io/exec.c#;$Ng={$n3,1};$Og=q#/io/exec.c#;$Pg=[$ze];$Qg=bless({$T2,$Ng,$Q,$Og,$i3,$Pg},$h4);$Rg=q#ni:/io/exec_env.b#;$Sg=q#ni:/io/exec_fork.b#;$Tg=q#ni:/io/exec_init.b#;$Ug=q#ni:/io/exec_io_accessors.b#;$Vg=q#ni:/io/exec_io_setup.b#;$Wg=q#ni:/io/exec_ro.b#;$Xg=q#ni:/io/fd#;$Yg={$X2,1};$Zg=q#read_fd_mask#;$ch={};$dh=[];$eh=q#shift->{'fd'}#;$fh=bless({$t,$dh,$v,$q,$w,$eh,$y,$z},$A);$gh={$r7,$fh};$hh=q#/io/fd_readers.b#;$ih=bless({$T2,$ch,$u4,$q,$v4,$q,$w4,$gh,$Q,$hh},$T3);$jh={};$kh=[];$lh=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$mh=bless({$t,$kh,$v,$q,$w,$lh,$y,$z},$A);$nh={$J6,$mh};$oh=q#/io/fd_init.b#;$ph=bless({$T2,$jh,$u4,$q,$v4,$q,$w4,$nh,$Q,$oh},$T3);$qh={};$rh=q#be#;$sh=[];$th=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$uh=bless({$t,$sh,$v,$q,$w,$th,$y,$z},$A);$vh={$rh,$uh};$wh=q#/io/fd_shell.b#;$xh=bless({$T2,$qh,$u4,$q,$v4,$q,$w4,$vh,$Q,$wh},$T3);$yh={};$zh=q#cloexec#;$Ah=[];$Bh=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Ch=bless({$t,$Ah,$v,$q,$w,$Bh,$y,$z},$A);$Dh=q#fcntl_flag#;$Eh=[];$Fh=q#my ($self, $flag, $value) = @_;
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
}#;$Gh=bless({$t,$Eh,$v,$q,$w,$Fh,$y,$z},$A);$Hh=q#nonblock#;$Ih=[];$Jh=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Kh=bless({$t,$Ih,$v,$q,$w,$Jh,$y,$z},$A);$Lh={$zh,$Ch,$Dh,$Gh,$Hh,$Kh};$Mh=q#/io/fd_fcntl.b#;$Nh=bless({$T2,$yh,$u4,$q,$v4,$q,$w4,$Lh,$Q,$Mh},$T3);$Oh={};$Ph=[];$Qh=q#shift->close#;$Rh=bless({$t,$Ph,$v,$q,$w,$Qh,$y,$z},$A);$Sh=q#close#;$Th=[];$Uh=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Vh=bless({$t,$Th,$v,$q,$w,$Uh,$y,$z},$A);$Wh={$Sh,$Vh};$Xh=q#/io/fd_gc.b#;$Yh=bless({$T2,$Oh,$u4,$q,$v4,$Rh,$w4,$Wh,$Q,$Xh},$T3);$Zh={};$ci=q#close_perl_ios#;$di=[];$ei=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$fi=bless({$t,$di,$v,$q,$w,$ei,$y,$z},$A);$gi=[];$hi=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$ii=bless({$t,$gi,$v,$q,$w,$hi,$y,$z},$A);$ji=[];$ki=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$li=bless({$t,$ji,$v,$q,$w,$ki,$y,$z},$A);$mi={$ci,$fi,$f7,$ii,$j7,$li};$ni=q#/io/fd_perlio.b#;$oi=bless({$T2,$Zh,$u4,$q,$v4,$q,$w4,$mi,$Q,$ni},$T3);$pi=[$h6,$ih,$ph,$xh,$Nh,$Yh,$oi];$qi=q#write_fd_mask#;$ri=bless({$T2,$Yg,$Q,$m2,$Zg,$z,$i3,$pi,$qi,$z},$o3);$si=[];$ti=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$ui=bless({$t,$si,$v,$q,$w,$ti,$y,$z},$A);$vi=q#ni:/io/fd.c#;$wi={$o3,1};$xi=q#/io/fd.c#;$yi={};$zi=q#clear_fd#;$Ai=[];$Bi=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Ci=bless({$t,$Ai,$v,$q,$w,$Bi,$y,$z},$A);$Di=q#read_fd#;$Ei=[];$Fi=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Gi=bless({$t,$Ei,$v,$q,$w,$Fi,$y,$z},$A);$Hi=q#select#;$Ii=[];$Ji=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Ki=bless({$t,$Ii,$v,$q,$w,$Ji,$y,$z},$A);$Li=q#write_fd#;$Mi=[];$Ni=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Oi=bless({$t,$Mi,$v,$q,$w,$Ni,$y,$z},$A);$Pi={$zi,$Ci,$Di,$Gi,$Hi,$Ki,$Li,$Oi};$Qi=q#/io/fd.c_selector.b#;$Ri=bless({$T2,$yi,$u4,$ui,$v4,$q,$w4,$Pi,$Q,$Qi},$T3);$Si=[$ze,$Ri];$Ti=bless({$T2,$wi,$Q,$xi,$i3,$Si},$h4);$Ui=q#ni:/io/fd.c_selector.b#;$Vi=q#ni:/io/fd_fcntl.b#;$Wi=q#ni:/io/fd_gc.b#;$Xi=q#ni:/io/fd_init.b#;$Yi=q#ni:/io/fd_perlio.b#;$Zi=q#ni:/io/fd_readers.b#;$cj=q#ni:/io/fd_shell.b#;$dj=q#ni:/io/file#;$ej={$Y2,1};$fj={};$gj=[];$hj=q#shift->{'name'}#;$ij=bless({$t,$gj,$v,$q,$w,$hj,$y,$z},$A);$jj={$Q,$ij};$kj=q#/io/file_readers.b#;$lj=bless({$T2,$fj,$u4,$q,$v4,$q,$w4,$jj,$Q,$kj},$T3);$mj={};$nj=q#mode#;$oj=[];$pj=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$qj=bless({$t,$oj,$v,$q,$w,$pj,$y,$z},$A);$rj={$nj,$qj};$sj=q#/io/file_accessors.b#;$tj=bless({$T2,$mj,$u4,$q,$v4,$q,$w4,$rj,$Q,$sj},$T3);$uj={};$vj=[];$wj=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$xj=bless({$t,$vj,$v,$q,$w,$wj,$y,$z},$A);$yj={$J6,$xj};$zj=q#/io/file_init.b#;$Aj=bless({$T2,$uj,$u4,$q,$v4,$q,$w4,$yj,$Q,$zj},$T3);$Bj={};$Cj=q#(-X#;$Dj=[];$Ej=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Fj=bless({$t,$Dj,$v,$q,$w,$Ej,$y,$z},$A);$Gj=q#mv#;$Hj=[];$Ij=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Jj=bless({$t,$Hj,$v,$q,$w,$Ij,$y,$z},$A);$Kj=q#rm#;$Lj=[];$Mj=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Nj=bless({$t,$Lj,$v,$q,$w,$Mj,$y,$z},$A);$Oj={$Cj,$Fj,$Gj,$Jj,$Kj,$Nj};$Pj=q#/io/file_fns.b#;$Qj=bless({$T2,$Bj,$u4,$q,$v4,$q,$w4,$Oj,$Q,$Pj},$T3);$Rj={};$Sj=q#atomic_update#;$Tj=[];$Uj=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Vj=bless({$t,$Tj,$v,$q,$w,$Uj,$y,$z},$A);$Wj={$Sj,$Vj};$Xj=q#/io/file_update.b#;$Yj=bless({$T2,$Rj,$u4,$q,$v4,$q,$w4,$Wj,$Q,$Xj},$T3);$Zj={};$ck=[];$dk=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$ek=bless({$t,$ck,$v,$q,$w,$dk,$y,$z},$A);$fk=q#r#;$gk=[];$hk=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$ik=bless({$t,$gk,$v,$q,$w,$hk,$y,$z},$A);$jk=[];$kk=q#shift->r->read(@_)#;$lk=bless({$t,$jk,$v,$q,$w,$kk,$y,$z},$A);$mk=q#w#;$nk=[];$ok=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$pk=bless({$t,$nk,$v,$q,$w,$ok,$y,$z},$A);$qk=[];$rk=q#shift->w->write(@_)#;$sk=bless({$t,$qk,$v,$q,$w,$rk,$y,$z},$A);$tk={$Sh,$ek,$fk,$ik,$f7,$lk,$mk,$pk,$j7,$sk};$uk=q#/io/file_io.b#;$vk=bless({$T2,$Zj,$u4,$q,$v4,$q,$w4,$tk,$Q,$uk},$T3);$wk=[$h6,$lj,$tj,$Aj,$Qj,$Yj,$vk];$xk=bless({$T2,$ej,$Q,$I2,$i3,$wk},$p3);$yk=q#ni:/io/file.c#;$zk={$p3,1};$Ak=q#/io/file.c#;$Bk=[$ze];$Ck=bless({$T2,$zk,$Q,$Ak,$i3,$Bk},$h4);$Dk=q#ni:/io/file_accessors.b#;$Ek=q#ni:/io/file_fns.b#;$Fk=q#ni:/io/file_init.b#;$Gk=q#ni:/io/file_io.b#;$Hk=q#ni:/io/file_readers.b#;$Ik=q#ni:/io/file_update.b#;$Jk=q#ni:/io/file_update_fd#;$Kk={$Z2,1};$Lk={};$Mk=[];$Nk=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Ok=bless({$t,$Mk,$v,$q,$w,$Nk,$y,$z},$A);$Pk={$J6,$Ok};$Qk=q#/io/file_update_fd_fd_init.b#;$Rk=bless({$T2,$Lk,$u4,$q,$v4,$q,$w4,$Pk,$Q,$Qk},$T3);$Sk={};$Tk=[];$Uk=bless({$t,$Tk,$v,$q,$w,$Qh,$y,$z},$A);$Vk=[];$Wk=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Xk=bless({$t,$Vk,$v,$q,$w,$Wk,$y,$z},$A);$Yk={$Sh,$Xk};$Zk=q#/io/file_update_fd_fd_gc.b#;$cl=bless({$T2,$Sk,$u4,$q,$v4,$Uk,$w4,$Yk,$Q,$Zk},$T3);$dl=[$h6,$hh,$Mh,$ni,$Rk,$cl];$el=bless({$T2,$Kk,$Q,$O2,$i3,$dl},$q3);$fl=q#ni:/io/file_update_fd.c#;$gl={$q3,1};$hl=q#/io/file_update_fd.c#;$il=[$ze];$jl=bless({$T2,$gl,$Q,$hl,$i3,$il},$h4);$kl=q#ni:/io/file_update_fd_fd_gc.b#;$ll=q#ni:/io/file_update_fd_fd_init.b#;$ml=q#ni:/io/named_io_fns.b#;$nl={};$ol=q#fcntl#;$pl=[];$ql=q#CORE::fcntl $_[0], $_[1], $_[2]#;$rl=bless({$t,$pl,$v,$q,$w,$ql,$y,$z},$A);$sl=[];$tl=q#CORE::fork#;$ul=bless({$t,$sl,$v,$q,$w,$tl,$y,$z},$A);$vl=q#open2#;$wl=[];$xl=q#CORE::open $_[0], $_[1]#;$yl=bless({$t,$wl,$v,$q,$w,$xl,$y,$z},$A);$zl=q#rename#;$Al=[];$Bl=q#CORE::rename $_[0], $_[1]#;$Cl=bless({$t,$Al,$v,$q,$w,$Bl,$y,$z},$A);$Dl=q#unlink#;$El=[];$Fl=q#CORE::unlink @_#;$Gl=bless({$t,$El,$v,$q,$w,$Fl,$y,$z},$A);$Hl=q#waitpid#;$Il=[];$Jl=q#CORE::waitpid $_[0], $_[1]#;$Kl=bless({$t,$Il,$v,$q,$w,$Jl,$y,$z},$A);$Ll={$ol,$rl,$zg,$ul,$vl,$yl,$zl,$Cl,$Dl,$Gl,$Hl,$Kl};$Ml=q#/io/named_io_fns.b#;$Nl=bless({$T2,$nl,$u4,$q,$v4,$q,$w4,$Ll,$Q,$Ml},$T3);$Ol=q#main#;$Pl=q#ni:/io/null#;$Ql={$c3,1};$Rl=q#/io/null#;$Sl={};$Tl=[];$Ul=q#+{fd => undef}#;$Vl=bless({$t,$Tl,$v,$q,$w,$Ul,$y,$z},$A);$Wl={$J6,$Vl};$Xl=q#/io/null_init.b#;$Yl=bless({$T2,$Sl,$u4,$q,$v4,$q,$w4,$Wl,$Q,$Xl},$T3);$Zl={};$cm=[];$dm=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$em=bless({$t,$cm,$v,$q,$w,$dm,$y,$z},$A);$fm=[];$gm=q#shift->fd->read(@_)#;$hm=bless({$t,$fm,$v,$q,$w,$gm,$y,$z},$A);$im=[];$jm=q#shift->fd->write(@_)#;$km=bless({$t,$im,$v,$q,$w,$jm,$y,$z},$A);$lm={$r7,$em,$f7,$hm,$j7,$km};$mm=q#/io/null_io.b#;$nm=bless({$T2,$Zl,$u4,$q,$v4,$q,$w4,$lm,$Q,$mm},$T3);$om=[$h6,$Yl,$nm];$pm=bless({$T2,$Ql,$Q,$Rl,$i3,$om},$r3);$qm=q#ni:/io/null.c#;$rm={$r3,1};$sm=q#/io/null.c#;$tm=[$ze];$um=bless({$T2,$rm,$Q,$sm,$i3,$tm},$h4);$vm=q#ni:/io/null_init.b#;$wm=q#ni:/io/null_io.b#;$xm=q#ni:/io/object#;$ym=q#ni:/io/object.c#;$zm=q#ni:/io/object.c_transfer_def.b#;$Am=q#ni:/io/object_checks.b#;$Bm=q#ni:/io/object_constructors.b#;$Cm=q#ni:/io/object_memory.b#;$Dm=q#ni:/io/object_ops.b#;$Em=q#ni:/io/object_transfer_async.b#;$Fm=q#ni:/io/object_transfer_sync.b#;$Gm=q#ni:/io/pid#;$Hm=q#ni:/io/pid.c#;$Im={$t3,1};$Jm=q#/io/pid.c#;$Km=[$ze];$Lm=bless({$T2,$Im,$Q,$Jm,$i3,$Km},$h4);$Mm=q#ni:/io/pid_accessors.b#;$Nm=q#ni:/io/pid_init.b#;$Om=q#ni:/io/pid_io.b#;$Pm=q#ni:/io/pid_readers.b#;$Qm=q#ni:/io/pid_wait.b#;$Rm=q#ni:/io/str#;$Sm={$f3,1};$Tm=q#/io/str#;$Um={};$Vm=q#data#;$Wm=[];$Xm=q#shift->{'data'}#;$Ym=bless({$t,$Wm,$v,$q,$w,$Xm,$y,$z},$A);$Zm=q#end#;$cn=[];$dn=q#shift->{'end'}#;$en=bless({$t,$cn,$v,$q,$w,$dn,$y,$z},$A);$fn=q#start#;$gn=[];$hn=q#shift->{'start'}#;$in=bless({$t,$gn,$v,$q,$w,$hn,$y,$z},$A);$jn={$Vm,$Ym,$Zm,$en,$fn,$in};$kn=q#/io/str_ro.b#;$ln=bless({$T2,$Um,$u4,$q,$v4,$q,$w4,$jn,$Q,$kn},$T3);$mn={};$nn=[];$on=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$pn=bless({$t,$nn,$v,$q,$w,$on,$y,$z},$A);$qn={$J6,$pn};$rn=q#/io/str_init.b#;$sn=bless({$T2,$mn,$u4,$q,$v4,$q,$w4,$qn,$Q,$rn},$T3);$tn={};$un=[];$vn=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$wn=bless({$t,$un,$v,$q,$w,$vn,$y,$z},$A);$xn=q#remaining#;$yn=[];$zn=q#my $self = shift; $$self{end} - $$self{start}#;$An=bless({$t,$yn,$v,$q,$w,$zn,$y,$z},$A);$Bn=[];$Cn=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Dn=bless({$t,$Bn,$v,$q,$w,$Cn,$y,$z},$A);$En={$f7,$wn,$xn,$An,$j7,$Dn};$Fn=q#/io/str_io.b#;$Gn=bless({$T2,$tn,$u4,$q,$v4,$q,$w4,$En,$Q,$Fn},$T3);$Hn=[$h6,$ln,$sn,$Gn];$In=bless({$T2,$Sm,$Q,$Tm,$i3,$Hn},$u3);$Jn=q#ni:/io/str.c#;$Kn={$u3,1};$Ln=q#/io/str.c#;$Mn=[$ze];$Nn=bless({$T2,$Kn,$Q,$Ln,$i3,$Mn},$h4);$On=q#ni:/io/str_init.b#;$Pn=q#ni:/io/str_io.b#;$Qn=q#ni:/io/str_ro.b#;$Rn=q#ni:/io/transfer#;$Sn={$v3,1,$x3,1,$z3,1};$Tn=q#/io/transfer#;$Un={$v3,1,$x3,1,$z3,1,$p4,1};$Vn=q#/semantic/task#;$Wn={};$Xn=[];$Yn=q#shift->{'outcome'}#;$Zn=bless({$t,$Xn,$v,$q,$w,$Yn,$y,$z},$A);$co={$r,$Zn};$do=q#/semantic/task_ro.b#;$eo=bless({$T2,$Wn,$u4,$q,$v4,$q,$w4,$co,$Q,$do},$T3);$fo={};$go=q#failure#;$ho=[];$io=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$jo=bless({$t,$ho,$v,$q,$w,$io,$y,$z},$A);$ko=q#success#;$lo=[];$mo=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$no=bless({$t,$lo,$v,$q,$w,$mo,$y,$z},$A);$oo={$go,$jo,$ko,$no};$po=q#/semantic/task_outcome.b#;$qo=bless({$T2,$fo,$u4,$q,$v4,$q,$w4,$oo,$Q,$po},$T3);$ro=[$G4,$eo,$qo];$so=bless({$T2,$Un,$Q,$Vn,$i3,$ro},$q4);$to={};$uo=[];$vo=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$wo=bless({$t,$uo,$v,$q,$w,$vo,$y,$z},$A);$xo=[];$yo=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$zo=bless({$t,$xo,$v,$q,$w,$yo,$y,$z},$A);$Ao=[];$Bo=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Co=bless({$t,$Ao,$v,$q,$w,$Bo,$y,$z},$A);$Do={$f7,$zo,$j7,$Co};$Eo=q#/io/transfer_io_interop.b#;$Fo=bless({$T2,$to,$u4,$wo,$v4,$q,$w4,$Do,$Q,$Eo},$T3);$Go={};$Ho=q#pressure#;$Io=[];$Jo=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Ko=bless({$t,$Io,$v,$q,$w,$Jo,$y,$z},$A);$Lo=q#read_limit_throughput#;$Mo=[];$No=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Oo=bless({$t,$Mo,$v,$q,$w,$No,$y,$z},$A);$Po=q#throughput#;$Qo=[];$Ro=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$So=bless({$t,$Qo,$v,$q,$w,$Ro,$y,$z},$A);$To=q#write_limit_throughput#;$Uo=[];$Vo=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Wo=bless({$t,$Uo,$v,$q,$w,$Vo,$y,$z},$A);$Xo={$Ho,$Ko,$Lo,$Oo,$Po,$So,$To,$Wo};$Yo=q#/io/transfer_io_measurement.b#;$Zo=bless({$T2,$Go,$u4,$q,$v4,$q,$w4,$Xo,$Q,$Yo},$T3);$cp=[$so,$Fo,$Zo];$dp=bless({$T2,$Sn,$Q,$Tn,$i3,$cp},$w3);$ep=[];$fp=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$gp=bless({$t,$ep,$v,$q,$w,$fp,$y,$z},$A);$hp=q#ni:/io/transfer.c#;$ip={$w3,1,$y3,1,$A3,1};$jp=q#/io/transfer.c#;$kp={$w3,1,$y3,1,$A3,1,$q4,1};$lp=q#/semantic/task.c#;$mp=[$rd];$np=bless({$T2,$kp,$Q,$lp,$i3,$mp},$h4);$op={};$pp={};$qp=q#/io/transfer.c_into.b#;$rp=bless({$T2,$op,$u4,$gp,$v4,$q,$w4,$pp,$Q,$qp},$T3);$sp=[$np,$rp];$tp=bless({$T2,$ip,$Q,$jp,$i3,$sp},$h4);$up=q#ni:/io/transfer.c_into.b#;$vp=q#ni:/io/transfer_async#;$wp={$x3,1};$xp=q#/io/transfer_async#;$yp={};$zp=q#dest_io#;$Ap=[];$Bp=q#shift->{'dest_io'}#;$Cp=bless({$t,$Ap,$v,$q,$w,$Bp,$y,$z},$A);$Dp=q#id#;$Ep=[];$Fp=q#shift->{'id'}#;$Gp=bless({$t,$Ep,$v,$q,$w,$Fp,$y,$z},$A);$Hp=q#source_io#;$Ip=[];$Jp=q#shift->{'source_io'}#;$Kp=bless({$t,$Ip,$v,$q,$w,$Jp,$y,$z},$A);$Lp={$zp,$Cp,$Dp,$Gp,$Hp,$Kp};$Mp=q#/io/transfer_async_ro.b#;$Np=bless({$T2,$yp,$u4,$q,$v4,$q,$w4,$Lp,$Q,$Mp},$T3);$Op={};$Pp=[];$Qp=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Rp=bless({$t,$Pp,$v,$q,$w,$Qp,$y,$z},$A);$Sp={$J6,$Rp};$Tp=q#/io/transfer_async_init.b#;$Up=bless({$T2,$Op,$u4,$q,$v4,$q,$w4,$Sp,$Q,$Tp},$T3);$Vp={};$Wp=[];$Xp=q#ni('ni:/io/transfer_async')->track(shift)#;$Yp=bless({$t,$Wp,$v,$q,$w,$Xp,$y,$z},$A);$Zp=[];$cq=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$dq=bless({$t,$Zp,$v,$q,$w,$cq,$y,$z},$A);$eq={};$fq=q#/io/transfer_async_lifecycle.b#;$gq=bless({$T2,$Vp,$u4,$Yp,$v4,$dq,$w4,$eq,$Q,$fq},$T3);$hq={};$iq=q#run#;$jq=[];$kq=q#shift#;$lq=bless({$t,$jq,$v,$q,$w,$kq,$y,$z},$A);$mq=q#run_async#;$nq=[];$oq=q#my $self = shift;
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

$self;#;$pq=bless({$t,$nq,$v,$q,$w,$oq,$y,$z},$A);$qq={$iq,$lq,$mq,$pq};$rq=q#/io/transfer_async_run.b#;$sq=bless({$T2,$hq,$u4,$q,$v4,$q,$w4,$qq,$Q,$rq},$T3);$tq=[$dp,$Np,$Up,$gq,$sq];$uq=q#tracked_transfers#;$vq={};$wq=q#transfer_id#;$xq=bless({$T2,$wp,$Q,$xp,$i3,$tq,$uq,$vq,$wq,0},$y3);$yq=[];$zq=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Aq=bless({$t,$yq,$v,$q,$w,$zq,$y,$z},$A);$Bq=q#ni:/io/transfer_async.c#;$Cq={$y3,1};$Dq=q#/io/transfer_async.c#;$Eq={};$Fq=q#new_id#;$Gq=[];$Hq=q#++shift->{transfer_id}#;$Iq=bless({$t,$Gq,$v,$q,$w,$Hq,$y,$z},$A);$Jq=q#track#;$Kq=[];$Lq=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Mq=bless({$t,$Kq,$v,$q,$w,$Lq,$y,$z},$A);$Nq=q#untrack#;$Oq=[];$Pq=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Qq=bless({$t,$Oq,$v,$q,$w,$Pq,$y,$z},$A);$Rq={$Fq,$Iq,$Jq,$Mq,$Nq,$Qq};$Sq=q#/io/transfer_async.c_tracker.b#;$Tq=bless({$T2,$Eq,$u4,$Aq,$v4,$q,$w4,$Rq,$Q,$Sq},$T3);$Uq=[$tp,$Tq];$Vq=bless({$T2,$Cq,$Q,$Dq,$i3,$Uq},$h4);$Wq=q#ni:/io/transfer_async.c_tracker.b#;$Xq=q#ni:/io/transfer_async_init.b#;$Yq=q#ni:/io/transfer_async_lifecycle.b#;$Zq=q#ni:/io/transfer_async_ro.b#;$cr=q#ni:/io/transfer_async_run.b#;$dr=q#ni:/io/transfer_io_interop.b#;$er=q#ni:/io/transfer_io_measurement.b#;$fr=q#ni:/io/transfer_sync#;$gr={$z3,1};$hr=q#/io/transfer_sync#;$ir={};$jr=[];$kr=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$lr=bless({$t,$jr,$v,$q,$w,$kr,$y,$z},$A);$mr={$J6,$lr};$nr=q#/io/transfer_sync_init.b#;$or=bless({$T2,$ir,$u4,$q,$v4,$q,$w4,$mr,$Q,$nr},$T3);$pr={};$qr=[];$rr=q#my $self = shift;
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
$self->success;#;$sr=bless({$t,$qr,$v,$q,$w,$rr,$y,$z},$A);$tr={$iq,$sr};$ur=q#/io/transfer_sync_run.b#;$vr=bless({$T2,$pr,$u4,$q,$v4,$q,$w4,$tr,$Q,$ur},$T3);$wr=[$dp,$or,$vr];$xr=bless({$T2,$gr,$Q,$hr,$i3,$wr},$A3);$yr=q#ni:/io/transfer_sync.c#;$zr={$A3,1};$Ar=q#/io/transfer_sync.c#;$Br=[$tp];$Cr=bless({$T2,$zr,$Q,$Ar,$i3,$Br},$h4);$Dr=q#ni:/io/transfer_sync_init.b#;$Er=q#ni:/io/transfer_sync_run.b#;$Fr=q#ni:/lib#;$Gr=q#lib#;$Hr={$Gr,1};$Ir=[];$Jr=bless({$T2,$Hr,$Q,$m8,$i3,$Ir},$j4);$Kr=q#ni:/lib/accessor.b#;$Lr={};$Mr=q#ro#;$Nr=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$Or=bless({$w,$Nr,$y,$z},$A);$Pr=q#rw#;$Qr=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$Rr=bless({$w,$Qr,$y,$z},$A);$Sr={$Mr,$Or,$Pr,$Rr};$Tr=q#/lib/accessor.b#;$Ur=bless({$T2,$Lr,$u4,$q,$v4,$q,$w4,$Sr,$Q,$Tr},$T3);$Vr=q#ni:/lib/behavior#;$Wr=q#ni:/lib/behavior.c#;$Xr={$k3,1,$C3,1,$E3,1,$G3,1,$U3,1,$W3,1,$k4,1,$o4,1};$Yr=[$rd];$Zr=bless({$T2,$Xr,$Q,$sd,$i3,$Yr},$h4);$cs=q#ni:/lib/branch#;$ds={$D3,1};$es=q#/lib/branch#;$fs={};$gs=q#add#;$hs=q#local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {
  $self->resolve($_)->apply($p) for @_;
}
$self;#;$is=bless({$w,$hs,$y,$z},$A);$js=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$self->resolve($_)->apply($p) for @{$$self{slices}};
$self;#;$ks=bless({$w,$js,$y,$z},$A);$ls={$gs,$is,$I8,$ks};$ms=bless({$T2,$fs,$u4,$q,$v4,$q,$w4,$ls,$Q,$Fc},$T3);$ns={};$os=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [@_]};#;$ps=bless({$w,$os,$y,$z},$A);$qs={$J6,$ps};$rs=q#/lib/branch_init.b#;$ss=bless({$T2,$ns,$u4,$q,$v4,$q,$w4,$qs,$Q,$rs},$T3);$ts=[$A8,$G8,$ms,$ss,$Oc];$us=bless({$T2,$ds,$Q,$es,$i3,$ts},$E3);$vs=q#ni:/lib/branch.b#;$ws=q#ni:/lib/branch.c#;$xs={$E3,1};$ys=q#/lib/branch.c#;$zs=[$Zr];$As=bless({$T2,$xs,$Q,$ys,$i3,$zs},$h4);$Bs=q#ni:/lib/branch_init.b#;$Cs=q#ni:/lib/class_init.b#;$Ds=q#ni:/lib/dataslice#;$Es=q#ni:/lib/dataslice.b#;$Fs=q#ni:/lib/dataslice.c#;$Gs={$G3,1};$Hs=q#/lib/dataslice.c#;$Is=[$Zr];$Js=bless({$T2,$Gs,$Q,$Hs,$i3,$Is},$h4);$Ks=q#ni:/lib/dataslice_init.b#;$Ls=q#ni:/lib/definition.b#;$Ms={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Ns={};$Os=q#def#;$Ps=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Qs=bless({$w,$Ps,$y,$z},$A);$Rs={$Os,$Qs};$Ss=q#/lib/definition_def.b#;$Ts=bless({$T2,$Ns,$u4,$q,$v4,$q,$w4,$Rs,$Q,$Ss},$T3);$Us={};$Vs=q#(""#;$Ws=q#shift->name#;$Xs=bless({$w,$Ws,$y,$z},$A);$Ys={$Vs,$Xs};$Zs=q#/lib/name_as_string.b#;$ct=bless({$T2,$Us,$u4,$q,$v4,$q,$w4,$Ys,$Q,$Zs},$T3);$dt={};$et=q#(eq#;$ft=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$gt=bless({$w,$ft,$y,$z},$A);$ht={$et,$gt};$it=q#/lib/ref_eq.b#;$jt=bless({$T2,$dt,$u4,$q,$v4,$q,$w4,$ht,$Q,$it},$T3);$kt={};$lt=q#defdata#;$mt=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$nt=bless({$w,$mt,$y,$z},$A);$ot={$lt,$nt};$pt=q#/lib/definition_defdata.b#;$qt=bless({$T2,$kt,$u4,$q,$v4,$q,$w4,$ot,$Q,$pt},$T3);$rt={};$st=q#instantiate_with_defaults#;$tt=q#my ($class, @slots) = @_;
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
    }));#;$ut=bless({$w,$tt,$y,$z},$A);$vt={$st,$ut};$wt=q#/lib/definition_init_with_defaults.b#;$xt=bless({$T2,$rt,$u4,$q,$v4,$q,$w4,$vt,$Q,$wt},$T3);$yt=[$Ts,$Ur,$ct,$jt,$qt,$xt];$zt=bless({$T2,$Ms,$Q,$Oc,$i3,$yt},$D3);$At=q#ni:/lib/definition_def.b#;$Bt=q#ni:/lib/definition_defdata.b#;$Ct=q#ni:/lib/definition_init_with_defaults.b#;$Dt=q#ni:/lib/doc#;$Et={$S,1};$Ft={};$Gt=q#shift; +{name => shift, doc => []}#;$Ht=bless({$w,$Gt,$y,$z},$A);$It={$J6,$Ht};$Jt=q#/lib/doc_init.b#;$Kt=bless({$T2,$Ft,$u4,$q,$v4,$q,$w4,$It,$Q,$Jt},$T3);$Lt={};$Mt=q#namespace#;$Nt=q#'ni.doc'#;$Ot=bless({$w,$Nt,$y,$z},$A);$Pt={$Mt,$Ot};$Qt=q#/lib/doc_namespace.b#;$Rt=bless({$T2,$Lt,$u4,$q,$v4,$q,$w4,$Pt,$Q,$Qt},$T3);$St={};$Tt=q#(@{}#;$Ut=q#[map @$_, @{shift->{doc}}]#;$Vt=bless({$w,$Ut,$y,$z},$A);$Wt=q#AUTOLOAD#;$Xt=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$Yt=bless({$w,$Xt,$y,$z},$A);$Zt={$Tt,$Vt,$Wt,$Yt};$cu=q#/lib/doc_define.b#;$du=bless({$T2,$St,$u4,$q,$v4,$q,$w4,$Zt,$Q,$cu},$T3);$eu={};$fu=q#shift->referent#;$gu=bless({$w,$fu,$y,$z},$A);$hu=q#ni 'ni:' . shift->{name}#;$iu=bless({$w,$hu,$y,$z},$A);$ju={$Zm,$gu,$S2,$iu};$ku=q#/lib/doc_end.b#;$lu=bless({$T2,$eu,$u4,$q,$v4,$q,$w4,$ju,$Q,$ku},$T3);$mu={};$nu=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$ou=bless({$w,$nu,$y,$z},$A);$pu={$R2,$ou};$qu=q#/lib/doc_TODO.b#;$ru=bless({$T2,$mu,$u4,$q,$v4,$q,$w4,$pu,$Q,$qu},$T3);$su={};$tu=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$uu=bless({$w,$tu,$y,$z},$A);$vu={$p6,$uu};$wu=q#/lib/doc_eg.b#;$xu=bless({$T2,$su,$u4,$q,$v4,$q,$w4,$vu,$Q,$wu},$T3);$yu={};$zu=q#tests#;$Au=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case',
    map @$_, @{$$self{doc}};#;$Bu=bless({$w,$Au,$y,$z},$A);$Cu=q#todos#;$Du=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo',
    map @$_, @{$$self{doc}};#;$Eu=bless({$w,$Du,$y,$z},$A);$Fu={$zu,$Bu,$Cu,$Eu};$Gu=q#/lib/doc_process.b#;$Hu=bless({$T2,$yu,$u4,$q,$v4,$q,$w4,$Fu,$Q,$Gu},$T3);$Iu=[$G4,$G8,$Kt,$Rt,$du,$lu,$ru,$xu,$Hu];$Ju=bless({$T2,$Et,$Q,$D9,$i3,$Iu},$H3);$Ku=q#ni:/lib/doc.c#;$Lu={$H3,1};$Mu=q#/lib/doc.c#;$Nu={};$Ou=q#defannotation#;$Pu=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Qu=bless({$w,$Pu,$y,$z},$A);$Ru={$Ou,$Qu};$Su=q#/lib/doc.c_defannotation.b#;$Tu=bless({$T2,$Nu,$u4,$q,$v4,$q,$w4,$Ru,$Q,$Su},$T3);$Uu=[$rd,$Tu];$Vu=bless({$T2,$Lu,$Q,$Mu,$i3,$Uu},$h4);$Wu=q#ni:/lib/doc.c_defannotation.b#;$Xu=q#ni:/lib/doc_TODO.b#;$Yu=q#ni:/lib/doc_define.b#;$Zu=q#ni:/lib/doc_eg.b#;$cv=q#ni:/lib/doc_end.b#;$dv=q#ni:/lib/doc_init.b#;$ev=q#ni:/lib/doc_namespace.b#;$fv=q#ni:/lib/doc_process.b#;$gv=q#ni:/lib/documentable.b#;$hv=q#ni:/lib/fn#;$iv={$A,1};$jv=q#/lib/fn#;$kv=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$lv=bless({$w,$kv,$y,$z},$A);$mv=q#\# NB: everything here needs to happen in a single method; otherwise JIT
\# compilation will cause infinite recursion.
local ($@, $_);
my $self = shift;
$$self{proto} ||= '';

my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);

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
}#;$nv=bless({$w,$mv},$A);$ov=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$pv=bless({$w,$ov},$A);$qv=q#lib/fn::closure#;$rv=q#lib/fn::compile#;$sv=q#lib/fn::instantiate#;$tv={};$uv=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$vv=bless({$w,$uv,$y,$z},$A);$wv=q#compile#;$xv={$v,$lv,$wv,$nv,$J6,$pv};$yv=q#/lib/fn_init.b#;$zv=bless({$T2,$tv,$u4,$q,$v4,$vv,$w4,$xv,$Q,$yv},$T3);$Av={};$Bv=[];$Cv=q#shift->{'annotations'}#;$Dv=bless({$t,$Bv,$v,$q,$w,$Cv,$y,$z},$A);$Ev=[];$Fv=q#shift->{'code'}#;$Gv=bless({$t,$Ev,$v,$q,$w,$Fv,$y,$z},$A);$Hv=q#eval_number#;$Iv=[];$Jv=q#shift->{'eval_number'}#;$Kv=bless({$t,$Iv,$v,$q,$w,$Jv,$y,$z},$A);$Lv=q#fn#;$Mv=[];$Nv=q#shift->{'fn'}#;$Ov=bless({$t,$Mv,$v,$q,$w,$Nv,$y,$z},$A);$Pv={$t,$Dv,$w,$Gv,$Hv,$Kv,$Lv,$Ov};$Qv=q#/lib/fn_ro.b#;$Rv=bless({$T2,$Av,$u4,$q,$v4,$q,$w4,$Pv,$Q,$Qv},$T3);$Sv={};$Tv=[];$Uv=q#my $self = shift; "fn {$$self{code}}"#;$Vv=bless({$t,$Tv,$v,$q,$w,$Uv,$y,$z},$A);$Wv=[];$Xv=bless({$t,$Wv,$v,$q,$w,$ft,$y,$z},$A);$Yv={$Vs,$Vv,$et,$Xv};$Zv=q#/lib/fn_ops.b#;$cw=bless({$T2,$Sv,$u4,$q,$v4,$q,$w4,$Yv,$Q,$Zv},$T3);$dw={};$ew=[];$fw=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$gw=bless({$t,$ew,$v,$q,$w,$fw,$y,$z},$A);$hw={$Pb,$gw};$iw=q#/lib/fn_serialize.b#;$jw=bless({$T2,$dw,$u4,$q,$v4,$q,$w4,$hw,$Q,$iw},$T3);$kw=[$G4,$Xc,$zv,$Rv,$cw,$jw];$lw=bless({$T2,$iv,$Q,$jv,$i3,$kw},$I3);$mw=[];$nw=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$ow=bless({$t,$mw,$v,$q,$w,$nw,$y,$z},$A);$pw=q#ni:/lib/fn.c#;$qw={$I3,1};$rw=q#/lib/fn.c#;$sw={};$tw=q#resolve_evals#;$uw=[];$vw=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$ww=bless({$t,$uw,$v,$q,$w,$vw,$y,$z},$A);$xw={$tw,$ww};$yw=q#/lib/fn.c_resolve_eval.b#;$zw=bless({$T2,$sw,$u4,$ow,$v4,$q,$w4,$xw,$Q,$yw},$T3);$Aw=[$rd,$zw];$Bw=bless({$T2,$qw,$Q,$rw,$i3,$Aw},$h4);$Cw=q#ni:/lib/fn.c_resolve_eval.b#;$Dw=q#ni:/lib/fn_init.b#;$Ew=q#ni:/lib/fn_ops.b#;$Fw=q#ni:/lib/fn_ro.b#;$Gw=q#ni:/lib/fn_serialize.b#;$Hw=q#ni:/lib/future#;$Iw={$J3,1};$Jw={};$Kw=[];$Lw=bless({$t,$Kw,$v,$q,$w,$Yn,$y,$z},$A);$Mw=q#parents#;$Nw=[];$Ow=q#shift->{'parents'}#;$Pw=bless({$t,$Nw,$v,$q,$w,$Ow,$y,$z},$A);$Qw={$r,$Lw,$Mw,$Pw};$Rw=q#/lib/future_ro.b#;$Sw=bless({$T2,$Jw,$u4,$q,$v4,$q,$w4,$Qw,$Q,$Rw},$T3);$Tw={};$Uw=[];$Vw=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Ww=bless({$t,$Uw,$v,$q,$w,$Vw,$y,$z},$A);$Xw={$J6,$Ww};$Yw=q#/lib/future_init.b#;$Zw=bless({$T2,$Tw,$u4,$q,$v4,$q,$w4,$Xw,$Q,$Yw},$T3);$cx={};$dx=q#decide#;$ex=[];$fx=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$gx=bless({$t,$ex,$v,$q,$w,$fx,$y,$z},$A);$hx=q#decided#;$ix=[];$jx=q#shift->{outcome}#;$kx=bless({$t,$ix,$v,$q,$w,$jx,$y,$z},$A);$lx=q#listener#;$mx=[];$nx=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$ox=bless({$t,$mx,$v,$q,$w,$nx,$y,$z},$A);$px=q#v#;$qx=[];$rx=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$sx=bless({$t,$qx,$v,$q,$w,$rx,$y,$z},$A);$tx={$dx,$gx,$hx,$kx,$lx,$ox,$px,$sx};$ux=q#/lib/future_state.b#;$vx=bless({$T2,$cx,$u4,$q,$v4,$q,$w4,$tx,$Q,$ux},$T3);$wx={};$xx=q#and#;$yx=[];$zx=q#my $self   = $_[0];
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
$child;#;$Ax=bless({$t,$yx,$v,$q,$w,$zx,$y,$z},$A);$Bx=q#flatmap#;$Cx=[];$Dx=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$Ex=bless({$t,$Cx,$v,$q,$w,$Dx,$y,$z},$A);$Fx=q#map#;$Gx=[];$Hx=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$Ix=bless({$t,$Gx,$v,$q,$w,$Hx,$y,$z},$A);$Jx=q#or#;$Kx=[];$Lx=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Mx=bless({$t,$Kx,$v,$q,$w,$Lx,$y,$z},$A);$Nx={$xx,$Ax,$Bx,$Ex,$Fx,$Ix,$Jx,$Mx};$Ox=q#/lib/future_algebra.b#;$Px=bless({$T2,$wx,$u4,$q,$v4,$q,$w4,$Nx,$Q,$Ox},$T3);$Qx=[$G4,$Sw,$Zw,$vx,$Px];$Rx=bless({$T2,$Iw,$Q,$U9,$i3,$Qx},$K3);$Sx=q#ni:/lib/future.c#;$Tx={$K3,1};$Ux=q#/lib/future.c#;$Vx=[$rd];$Wx=bless({$T2,$Tx,$Q,$Ux,$i3,$Vx},$h4);$Xx=q#ni:/lib/future_algebra.b#;$Yx=q#ni:/lib/future_init.b#;$Zx=q#ni:/lib/future_ro.b#;$cy=q#ni:/lib/future_state.b#;$dy=q#ni:/lib/gensym_generator_compact.b#;$ey={};$fy=q#gensym#;$gy=[];$hy=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$iy=bless({$t,$gy,$v,$q,$w,$hy,$y,$z},$A);$jy={$fy,$iy};$ky=bless({$T2,$ey,$u4,$q,$v4,$q,$w4,$jy,$Q,$Pa},$T3);$ly=q#ni:/lib/global_static_test.b#;$my={};$ny=[];$oy=q#ni('ni:/lib/test_case')->new(shift)#;$py=q#($)#;$qy=bless({$t,$ny,$v,$q,$w,$oy,$y,$py},$A);$ry=q#now#;$sy=[];$ty=q#ni('ni:/lib/test_value')->new(shift)#;$uy=bless({$t,$sy,$v,$q,$w,$ty,$y,$py},$A);$vy={$p6,$qy,$ry,$uy};$wy=q#/lib/global_static_test.b#;$xy=bless({$T2,$my,$u4,$q,$v4,$q,$w4,$vy,$Q,$wy},$T3);$yy=q#ni:/lib/image#;$zy=q#ni:/lib/image.c#;$Ay={$M3,1};$By=q#/lib/image.c#;$Cy=[$rd];$Dy=bless({$T2,$Ay,$Q,$By,$i3,$Cy},$h4);$Ey=q#ni:/lib/image_init.b#;$Fy=q#ni:/lib/image_quoting.b#;$Gy=q#ni:/lib/instance.b#;$Hy=q#ni:/lib/instantiable.b#;$Iy=q#ni:/lib/json.b#;$Jy={};$Ky=q#json_decode#;$Ly=[];$My=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Ny=bless({$t,$Ly,$v,$q,$w,$My,$y,$py},$A);$Oy=q#json_encode#;$Py=[];$Qy=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Ry=bless({$t,$Py,$v,$q,$w,$Qy,$y,$py},$A);$Sy=q#json_encode_pretty#;$Ty=[];$Uy=q#local $_;
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

ni::json_encode($v);#;$Vy=bless({$t,$Ty,$v,$q,$w,$Uy,$y,$z},$A);$Wy=q#json_escape#;$Xy=[];$Yy=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$Zy=bless({$t,$Xy,$v,$q,$w,$Yy,$y,$py},$A);$cz=q#json_unescape#;$dz=[];$ez=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$fz=bless({$t,$dz,$v,$q,$w,$ez,$y,$py},$A);$gz=q#json_unescape_one#;$hz=[];$iz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$jz=bless({$t,$hz,$v,$q,$w,$iz,$y,$py},$A);$kz={$Ky,$Ny,$Oy,$Ry,$Sy,$Vy,$Wy,$Zy,$cz,$fz,$gz,$jz};$lz=q#/lib/json.b#;$mz=bless({$T2,$Jy,$u4,$q,$v4,$q,$w4,$kz,$Q,$lz},$T3);$nz=q#ni#;$oz=q#ni:/lib/json_data.b#;$pz={};$qz=q#json_escapes#;$rz=q##;$sz=q#b#;$tz=q#	#;$uz=q#t#;$vz=q#
#;$wz=q#n#;$xz=q##;$yz=q#f#;$zz=q##;$Az=q#"#;$Bz=q#/#;$Cz=q#\\#;$Dz={$rz,$sz,$tz,$uz,$vz,$wz,$xz,$yz,$zz,$fk,$Az,$Az,$Bz,$Bz,$Cz,$Cz};$Ez=q#json_unescapes#;$Fz={$Az,$Az,$Bz,$Bz,$Cz,$Cz,$sz,$rz,$yz,$xz,$wz,$vz,$fk,$zz,$uz,$tz};$Gz={$qz,$Dz,$Ez,$Fz};$Hz=q#/lib/json_data.b#;$Iz=bless({$T2,$pz,$Vm,$Gz,$Q,$Hz},$F3);$Jz=q#ni:/lib/name_as_string.b#;$Kz=q#ni:/lib/named.b#;$Lz=q#ni:/lib/named_in_ni.b#;$Mz={};$Nz=q#'ni'#;$Oz=bless({$w,$Nz,$y,$z},$A);$Pz={$Mt,$Oz};$Qz=bless({$T2,$Mz,$u4,$q,$v4,$q,$w4,$Pz,$Q,$X8},$T3);$Rz=q#ni:/lib/namespaced.b#;$Sz={};$Tz=q#package#;$Uz=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$Vz=bless({$w,$Uz,$y,$z},$A);$Wz={$Tz,$Vz};$Xz=bless({$T2,$Sz,$u4,$q,$v4,$q,$w4,$Wz,$Q,$Y8},$T3);$Yz=q#ni:/lib/ni#;$Zz={$N3,1};$cA={};$dA=q#extend#;$eA=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$fA=bless({$w,$eA,$y,$z},$A);$gA=q#is_mutable#;$hA=q#$0 ne '-' && -w $0#;$iA=bless({$w,$hA,$y,$z},$A);$jA=q#modify#;$kA=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$lA=bless({$w,$kA,$y,$z},$A);$mA={$dA,$fA,$gA,$iA,$jA,$lA};$nA=q#/lib/ni_self.b#;$oA=bless({$T2,$cA,$u4,$q,$v4,$q,$w4,$mA,$Q,$nA},$T3);$pA={};$qA=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$rA=bless({$w,$qA,$y,$z},$A);$sA=q#docs#;$tA=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$uA=bless({$w,$tA,$y,$z},$A);$vA=q#metaclasses#;$wA=q#my $self = shift;
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$xA=bless({$w,$wA,$y,$z},$A);$yA=q#undocumented#;$zA=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$AA=bless({$w,$zA,$y,$z},$A);$BA=q#untested#;$CA=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$DA=bless({$w,$CA,$y,$z},$A);$EA={$K,$rA,$sA,$uA,$vA,$xA,$yA,$AA,$BA,$DA};$FA=q#/lib/ni_dev_introspection.b#;$GA=bless({$T2,$pA,$u4,$q,$v4,$q,$w4,$EA,$Q,$FA},$T3);$HA={};$IA=q#--internal/+=#;$JA=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$KA=bless({$w,$JA,$y,$z},$A);$LA=q#--internal/dev-state#;$MA=q#my $self = shift;
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
0;#;$NA=bless({$w,$MA,$y,$z},$A);$OA=q#--internal/eval#;$PA=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$QA=bless({$w,$PA,$y,$z},$A);$RA=q#--internal/image#;$SA=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$TA=bless({$w,$SA,$y,$z},$A);$UA=q#--internal/test#;$VA=q#local $| = 1;
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
!!$failed;#;$WA=bless({$w,$VA,$y,$z},$A);$XA=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$YA=bless({$w,$XA,$y,$z},$A);$ZA={$IA,$KA,$LA,$NA,$OA,$QA,$RA,$TA,$UA,$WA,$iq,$YA};$cB=q#/lib/ni_main.b#;$dB=bless({$T2,$HA,$u4,$q,$v4,$q,$w4,$ZA,$Q,$cB},$T3);$eB={};$fB=q#resolve#;$gB=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$hB=bless({$w,$gB,$y,$z},$A);$iB=q#resolver_for#;$jB=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$kB=bless({$w,$jB,$y,$z},$A);$lB={$fB,$hB,$iB,$kB};$mB=q#/lib/ni_resolver.b#;$nB=bless({$T2,$eB,$u4,$q,$v4,$q,$w4,$lB,$Q,$mB},$T3);$oB={};$pB=q#exists#;$qB=q#exists $_[0]->{named}{$_[1]}#;$rB=bless({$w,$qB,$y,$z},$A);$sB=q#quoted#;$tB=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$uB=bless({$w,$tB,$y,$z},$A);$vB={$pB,$rB,$sB,$uB};$wB=q#/lib/ni_image.b#;$xB=bless({$T2,$oB,$u4,$q,$v4,$q,$w4,$vB,$Q,$wB},$T3);$yB=[$G4,$oA,$GA,$dB,$nB,$xB];$zB=bless({$T2,$Zz,$Q,$gb,$i3,$yB},$O3);$AB=q#ni:/lib/ni.c#;$BB={$O3,1};$CB=q#/lib/ni.c#;$DB=[$rd];$EB=bless({$T2,$BB,$Q,$CB,$i3,$DB},$h4);$FB=q#ni:/lib/ni_dev_introspection.b#;$GB=q#ni:/lib/ni_image.b#;$HB=q#ni:/lib/ni_main.b#;$IB=q#ni:/lib/ni_resolver.b#;$JB=q#ni:/lib/ni_self.b#;$KB=q#ni:/lib/ni_static_util.b#;$LB={};$MB=q#abbrev#;$NB=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$OB=bless({$w,$NB,$y,$z},$A);$PB=q#dor#;$QB=q#defined $_[0] ? $_[0] : $_[1]#;$RB=bless({$w,$QB,$y,$z},$A);$SB=q#indent#;$TB=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$UB=bless({$w,$TB,$y,$z},$A);$VB=q#max#;$WB=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$XB=bless({$w,$WB,$y,$z},$A);$YB=q#maxstr#;$ZB=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$cC=bless({$w,$ZB,$y,$z},$A);$dC=q#mean#;$eC=q#sum(@_) / (@_ || 1)#;$fC=bless({$w,$eC,$y,$z},$A);$gC=q#min#;$hC=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$iC=bless({$w,$hC,$y,$z},$A);$jC=q#minstr#;$kC=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$lC=bless({$w,$kC,$y,$z},$A);$mC=q#outdent#;$nC=q#my $x = shift;
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
join "\\n", @lines;#;$oC=bless({$w,$nC,$y,$z},$A);$pC=q#sgr#;$qC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$rC=bless({$w,$qC,$y,$z},$A);$sC=q#sr#;$tC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$uC=bless({$w,$tC,$y,$z},$A);$vC=q#sum#;$wC=q#local $_; my $x = 0; $x += $_ for @_; $x#;$xC=bless({$w,$wC,$y,$z},$A);$yC=q#swap#;$zC=q#@_[0, 1] = @_[1, 0]#;$AC=bless({$w,$zC,$y,$z},$A);$BC={$MB,$OB,$PB,$RB,$SB,$UB,$VB,$XB,$YB,$cC,$dC,$fC,$gC,$iC,$jC,$lC,$mC,$oC,$pC,$rC,$sC,$uC,$vC,$xC,$yC,$AC};$CC=q#/lib/ni_static_util.b#;$DC=bless({$T2,$LB,$u4,$q,$v4,$q,$w4,$BC,$Q,$CC},$T3);$EC=q#ni:/lib/object_metadata#;$FC={$P3,1,$C,1,$H,1};$GC=q#/lib/object_metadata#;$HC={};$IC=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$JC=bless({$w,$IC,$y,$z},$A);$KC={$S2,$JC};$LC=q#/lib/object_metadata_rw.b#;$MC=bless({$T2,$HC,$u4,$q,$v4,$q,$w4,$KC,$Q,$LC},$T3);$NC=[$G4,$MC];$OC=bless({$T2,$FC,$Q,$GC,$i3,$NC},$Q3);$PC=q#ni:/lib/object_metadata.c#;$QC={$Q3,1,$d4,1,$g4,1};$RC=q#/lib/object_metadata.c#;$SC=[$rd];$TC=bless({$T2,$QC,$Q,$RC,$i3,$SC},$h4);$UC=q#ni:/lib/object_metadata_rw.b#;$VC=q#ni:/lib/perlbranch.b#;$WC=q#ni:/lib/quote_circular_addressed.b#;$XC={};$YC=q#circular_arrayref#;$ZC=[];$cD=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$dD=bless({$t,$ZC,$v,$q,$w,$cD,$y,$z},$A);$eD=q#circular_hashref#;$fD=[];$gD=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$hD=bless({$t,$fD,$v,$q,$w,$gD,$y,$z},$A);$iD=q#is_circular#;$jD=[];$kD=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$lD=bless({$t,$jD,$v,$q,$w,$kD,$y,$z},$A);$mD={$YC,$dD,$eD,$hD,$iD,$lD};$nD=bless({$T2,$XC,$u4,$q,$v4,$q,$w4,$mD,$Q,$Na},$T3);$oD=q#ni:/lib/quote_code_fail.b#;$pD={};$qD=q#quote_code#;$rD=[];$sD=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$tD=bless({$t,$rD,$v,$q,$w,$sD,$y,$z},$A);$uD={$qD,$tD};$vD=bless({$T2,$pD,$u4,$q,$v4,$q,$w4,$uD,$Q,$Ka},$T3);$wD=q#ni:/lib/quote_gensym_identity.b#;$xD={};$yD=q#address#;$zD=[];$AD=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$BD=bless({$t,$zD,$v,$q,$w,$AD,$y,$z},$A);$CD=q#allocate_gensym#;$DD=[];$ED=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$FD=bless({$t,$DD,$v,$q,$w,$ED,$y,$z},$A);$GD=q#circular_links#;$HD=[];$ID=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$JD=bless({$t,$HD,$v,$q,$w,$ID,$y,$z},$A);$KD=q#quote#;$LD=[];$MD=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$ND=bless({$t,$LD,$v,$q,$w,$MD,$y,$z},$A);$OD={$yD,$BD,$CD,$FD,$GD,$JD,$KD,$ND};$PD=bless({$T2,$xD,$u4,$q,$v4,$q,$w4,$OD,$Q,$Oa},$T3);$QD=q#ni:/lib/quote_objects.b#;$RD={};$SD=q#quote_blessed#;$TD=[];$UD=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$VD=bless({$t,$TD,$v,$q,$w,$UD,$y,$z},$A);$WD=q#quote_class#;$XD=[];$YD=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$ZD=bless({$t,$XD,$v,$q,$w,$YD,$y,$z},$A);$cE=q#quote_object#;$dE=[];$eE=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$fE=bless({$t,$dE,$v,$q,$w,$eE,$y,$z},$A);$gE={$SD,$VD,$WD,$ZD,$cE,$fE};$hE=bless({$T2,$RD,$u4,$q,$v4,$q,$w4,$gE,$Q,$Ma},$T3);$iE=q#ni:/lib/quote_simple#;$jE={$R3,1};$kE={};$lE=[];$mE=q#+{}#;$nE=bless({$t,$lE,$v,$q,$w,$mE,$y,$z},$A);$oE={$J6,$nE};$pE=q#/lib/quote_simple_init.b#;$qE=bless({$T2,$kE,$u4,$q,$v4,$q,$w4,$oE,$Q,$pE},$T3);$rE={};$sE=[];$tE=bless({$t,$sE,$v,$q,$w,0,$y,$z},$A);$uE=[];$vE=q#shift->quote_value(shift)#;$wE=bless({$t,$uE,$v,$q,$w,$vE,$y,$z},$A);$xE={$iD,$tE,$KD,$wE};$yE=q#/lib/quote_simple_quote.b#;$zE=bless({$T2,$rE,$u4,$q,$v4,$q,$w4,$xE,$Q,$yE},$T3);$AE={};$BE=q#quote_array#;$CE=[];$DE=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$EE=bless({$t,$CE,$v,$q,$w,$DE,$y,$z},$A);$FE=q#quote_hash#;$GE=[];$HE=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$IE=bless({$t,$GE,$v,$q,$w,$HE,$y,$z},$A);$JE=q#quote_scalar#;$KE=[];$LE=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$ME=bless({$t,$KE,$v,$q,$w,$LE,$y,$z},$A);$NE=q#quote_scalar_ref#;$OE=[];$PE=q#'\\\\' . shift->quote(${$_[0]})#;$QE=bless({$t,$OE,$v,$q,$w,$PE,$y,$z},$A);$RE=q#quote_value#;$SE=[];$TE=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$UE=bless({$t,$SE,$v,$q,$w,$TE,$y,$z},$A);$VE={$BE,$EE,$FE,$IE,$JE,$ME,$NE,$QE,$RE,$UE};$WE=bless({$T2,$AE,$u4,$q,$v4,$q,$w4,$VE,$Q,$La},$T3);$XE=[$G4,$qE,$zE,$vD,$WE,$hE];$YE=bless({$T2,$jE,$Q,$rb,$i3,$XE},$S3);$ZE=q#ni:/lib/quote_simple.c#;$cF={$S3,1};$dF=q#/lib/quote_simple.c#;$eF=[$rd];$fF=bless({$T2,$cF,$Q,$dF,$i3,$eF},$h4);$gF=q#ni:/lib/quote_simple_init.b#;$hF=q#ni:/lib/quote_simple_quote.b#;$iF=q#ni:/lib/quote_values.b#;$jF=q#ni:/lib/ref_eq.b#;$kF=q#ni:/lib/resolver.b#;$lF={};$mF=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$nF=bless({$w,$mF,$y,$z},$A);$oF={$fB,$nF};$pF=bless({$T2,$lF,$u4,$q,$v4,$q,$w4,$oF,$Q,$Z8},$T3);$qF=q#ni:/lib/slice#;$rF=q#ni:/lib/slice.b#;$sF=q#ni:/lib/slice.c#;$tF={$U3,1};$uF=q#/lib/slice.c#;$vF=[$Zr];$wF=bless({$T2,$tF,$Q,$uF,$i3,$vF},$h4);$xF=q#ni:/lib/slice_init.b#;$yF=q#ni:/lib/slice_serialize.b#;$zF=q#ni:/lib/static_fn.b#;$AF={};$BF=q#fc#;$CF=[];$DF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$EF=bless({$t,$CF,$v,$q,$w,$DF,$y,$z},$A);$FF=q#fk#;$GF=[];$HF=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$IF=bless({$t,$GF,$v,$q,$w,$HF,$y,$py},$A);$JF=[];$KF=q#ni('ni:/lib/fn')->new(@_)#;$LF=bless({$t,$JF,$v,$q,$w,$KF,$y,$py},$A);$MF=q#fp#;$NF=[];$OF=q#($$)#;$PF=bless({$t,$NF,$v,$q,$w,$KF,$y,$OF},$A);$QF={$BF,$EF,$FF,$IF,$Lv,$LF,$MF,$PF};$RF=q#/lib/static_fn.b#;$SF=bless({$T2,$AF,$u4,$q,$v4,$q,$w4,$QF,$Q,$RF},$T3);$TF=q#ni:/lib/subclass.b#;$UF=q#ni:/lib/tag#;$VF={$V3,1};$WF=q#/lib/tag#;$XF={};$YF=q#local $_;
my ($self, $p) = @_;
$self->resolve($_)->apply($p) for @{$$self{slices}};
$self;#;$ZF=bless({$w,$YF,$y,$z},$A);$cG={$I8,$ZF};$dG=q#/lib/tag.b#;$eG=bless({$T2,$XF,$u4,$q,$v4,$q,$w4,$cG,$Q,$dG},$T3);$fG={};$gG=q#local $_;
my $class = shift;
my $name  = shift;
+{name => $name, slices => [@_]};#;$hG=bless({$w,$gG,$y,$z},$A);$iG={$J6,$hG};$jG=q#/lib/tag_init.b#;$kG=bless({$T2,$fG,$u4,$q,$v4,$q,$w4,$iG,$Q,$jG},$T3);$lG=[$A8,$G8,$eG,$kG];$mG=bless({$T2,$VF,$Q,$WF,$i3,$lG},$W3);$nG=q#ni:/lib/tag.b#;$oG=q#ni:/lib/tag.c#;$pG={$W3,1};$qG=q#/lib/tag.c#;$rG=[$Zr];$sG=bless({$T2,$pG,$Q,$qG,$i3,$rG},$h4);$tG=q#ni:/lib/tag_init.b#;$uG=q#ni:/lib/test_assert_eq#;$vG={$X3,1};$wG=q#/lib/test_assert_eq#;$xG={$X3,1,$Z3,1};$yG=q#/lib/test_assertion#;$zG={};$AG=q#commit#;$BG=[];$CG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$DG=bless({$t,$BG,$v,$q,$w,$CG,$y,$z},$A);$EG={$AG,$DG};$FG=q#/lib/test_assertion_commit.b#;$GG=bless({$T2,$zG,$u4,$q,$v4,$q,$w4,$EG,$Q,$FG},$T3);$HG=[$G4,$GG];$IG=bless({$T2,$xG,$Q,$yG,$i3,$HG},$c4);$JG={};$KG=q#diff#;$LG=[];$MG=q#shift->{'diff'}#;$NG=bless({$t,$LG,$v,$q,$w,$MG,$y,$z},$A);$OG={$KG,$NG};$PG=q#/lib/test_assert_eq_ro.b#;$QG=bless({$T2,$JG,$u4,$q,$v4,$q,$w4,$OG,$Q,$PG},$T3);$RG={};$SG=[];$TG=q#my ($class, $diff) = @_;
+{diff => $diff};#;$UG=bless({$t,$SG,$v,$q,$w,$TG,$y,$z},$A);$VG={$J6,$UG};$WG=q#/lib/test_assert_eq_init.b#;$XG=bless({$T2,$RG,$u4,$q,$v4,$q,$w4,$VG,$Q,$WG},$T3);$YG={};$ZG=[];$cH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$dH=bless({$t,$ZG,$v,$q,$w,$cH,$y,$z},$A);$eH=q#failed#;$fH=[];$gH=q#defined shift->{diff}#;$hH=bless({$t,$fH,$v,$q,$w,$gH,$y,$z},$A);$iH=q#result#;$jH=[];$kH=bless({$t,$jH,$v,$q,$w,$kq,$y,$z},$A);$lH={$Vs,$dH,$eH,$hH,$iH,$kH};$mH=q#/lib/test_assert_eq_result.b#;$nH=bless({$T2,$YG,$u4,$q,$v4,$q,$w4,$lH,$Q,$mH},$T3);$oH=[$IG,$QG,$XG,$nH];$pH=bless({$T2,$vG,$Q,$wG,$i3,$oH},$Y3);$qH=q#ni:/lib/test_assert_eq.c#;$rH={$Y3,1};$sH=q#/lib/test_assert_eq.c#;$tH={$Y3,1,$c4,1};$uH=q#/lib/test_assertion.c#;$vH=[$rd];$wH=bless({$T2,$tH,$Q,$uH,$i3,$vH},$h4);$xH=[$wH];$yH=bless({$T2,$rH,$Q,$sH,$i3,$xH},$h4);$zH=q#ni:/lib/test_assert_eq_init.b#;$AH=q#ni:/lib/test_assert_eq_result.b#;$BH=q#ni:/lib/test_assert_eq_ro.b#;$CH=q#ni:/lib/test_assertion#;$DH=q#ni:/lib/test_assertion.c#;$EH=q#ni:/lib/test_assertion_commit.b#;$FH=q#ni:/lib/test_case#;$GH={$C,1};$HH=q#/lib/test_case#;$IH=q#running_test#;$JH={};$KH=[];$LH=q#shift->{'assertions'}#;$MH=bless({$t,$KH,$v,$q,$w,$LH,$y,$z},$A);$NH=[];$OH=q#shift->{'test'}#;$PH=bless({$t,$NH,$v,$q,$w,$OH,$y,$z},$A);$QH={$n,$MH,$s,$PH};$RH=q#/lib/test_case_ro.b#;$SH=bless({$T2,$JH,$u4,$q,$v4,$q,$w4,$QH,$Q,$RH},$T3);$TH={};$UH=[];$VH=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$WH=bless({$t,$UH,$v,$q,$w,$VH,$y,$z},$A);$XH={$p,$WH};$YH=q#/lib/test_case_rw.b#;$ZH=bless({$T2,$TH,$u4,$q,$v4,$q,$w4,$XH,$Q,$YH},$T3);$cI={};$dI=[];$eI=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$fI=bless({$t,$dI,$v,$q,$w,$eI,$y,$z},$A);$gI={$J6,$fI};$hI=q#/lib/test_case_init.b#;$iI=bless({$T2,$cI,$u4,$q,$v4,$q,$w4,$gI,$Q,$hI},$T3);$jI={};$kI=[];$lI=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$mI=bless({$t,$kI,$v,$q,$w,$lI,$y,$z},$A);$nI=[];$oI=q#!shift->{outcome}->[0]#;$pI=bless({$t,$nI,$v,$q,$w,$oI,$y,$z},$A);$qI={$Vs,$mI,$eH,$pI};$rI=q#/lib/test_case_metrics.b#;$sI=bless({$T2,$jI,$u4,$q,$v4,$q,$w4,$qI,$Q,$rI},$T3);$tI={};$uI=q#done#;$vI=[];$wI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$xI=bless({$t,$vI,$v,$q,$w,$wI,$y,$z},$A);$yI=[];$zI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$AI=bless({$t,$yI,$v,$q,$w,$zI,$y,$z},$A);$BI={$uI,$xI,$iq,$AI};$CI=q#/lib/test_case_run.b#;$DI=bless({$T2,$tI,$u4,$q,$v4,$q,$w4,$BI,$Q,$CI},$T3);$EI=[$OC,$SH,$ZH,$iI,$sI,$DI];$FI=bless({$T2,$GH,$Q,$HH,$IH,$q,$i3,$EI},$d4);$GI=[];$HI=q#shift->{running_test} = undef#;$II=bless({$t,$GI,$v,$q,$w,$HI,$y,$z},$A);$JI=q#ni:/lib/test_case.c#;$KI={$d4,1};$LI=q#/lib/test_case.c#;$MI={};$NI=[];$OI=q#shift->{'running_test'}#;$PI=bless({$t,$NI,$v,$q,$w,$OI,$y,$z},$A);$QI={$IH,$PI};$RI=q#/lib/test_case.c_test_ro.b#;$SI=bless({$T2,$MI,$u4,$q,$v4,$q,$w4,$QI,$Q,$RI},$T3);$TI={};$UI=q#with_test#;$VI=[];$WI=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$XI=bless({$t,$VI,$v,$q,$w,$WI,$y,$z},$A);$YI={$UI,$XI};$ZI=q#/lib/test_case.c_test.b#;$cJ=bless({$T2,$TI,$u4,$II,$v4,$q,$w4,$YI,$Q,$ZI},$T3);$dJ=[$TC,$SI,$cJ];$eJ=bless({$T2,$KI,$Q,$LI,$i3,$dJ},$h4);$fJ=q#ni:/lib/test_case.c_test.b#;$gJ=q#ni:/lib/test_case.c_test_ro.b#;$hJ=q#ni:/lib/test_case_init.b#;$iJ=q#ni:/lib/test_case_metrics.b#;$jJ=q#ni:/lib/test_case_ro.b#;$kJ=q#ni:/lib/test_case_run.b#;$lJ=q#ni:/lib/test_case_rw.b#;$mJ=q#ni:/lib/test_value#;$nJ={$e4,1};$oJ=q#/lib/test_value#;$pJ={};$qJ=[];$rJ=q#\\$_[1]#;$sJ=bless({$t,$qJ,$v,$q,$w,$rJ,$y,$z},$A);$tJ={$J6,$sJ};$uJ=q#/lib/test_value_init.b#;$vJ=bless({$T2,$pJ,$u4,$q,$v4,$q,$w4,$tJ,$Q,$uJ},$T3);$wJ={};$xJ=q#(==#;$yJ=[];$zJ=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$AJ=bless({$t,$yJ,$v,$q,$w,$zJ,$y,$z},$A);$BJ=q#detailed_scalar_diff#;$CJ=[];$DJ=q#local $_;
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
[@diff];#;$EJ=bless({$t,$CJ,$v,$q,$w,$DJ,$y,$z},$A);$FJ=[];$GJ=q#my ($self, $rhs) = @_;
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
return undef;#;$HJ=bless({$t,$FJ,$v,$q,$w,$GJ,$y,$z},$A);$IJ={$xJ,$AJ,$BJ,$EJ,$KG,$HJ};$JJ=q#/lib/test_value_eq.b#;$KJ=bless({$T2,$wJ,$u4,$q,$v4,$q,$w4,$IJ,$Q,$JJ},$T3);$LJ={};$MJ=[];$NJ=q#ni::json_encode ${$_[0]}#;$OJ=bless({$t,$MJ,$v,$q,$w,$NJ,$y,$z},$A);$PJ={$Vs,$OJ};$QJ=q#/lib/test_value_str.b#;$RJ=bless({$T2,$LJ,$u4,$q,$v4,$q,$w4,$PJ,$Q,$QJ},$T3);$SJ=[$G4,$vJ,$KJ,$RJ];$TJ=bless({$T2,$nJ,$Q,$oJ,$i3,$SJ},$f4);$UJ=q#ni:/lib/test_value.c#;$VJ={$f4,1};$WJ=q#/lib/test_value.c#;$XJ=[$rd];$YJ=bless({$T2,$VJ,$Q,$WJ,$i3,$XJ},$h4);$ZJ=q#ni:/lib/test_value_eq.b#;$cK=q#ni:/lib/test_value_init.b#;$dK=q#ni:/lib/test_value_str.b#;$eK=q#ni:/lib/todo#;$fK={$H,1};$gK=q#/lib/todo#;$hK={};$iK=q#shift->{'todo'}#;$jK=bless({$w,$iK,$y,$z},$A);$kK={$E,$jK};$lK=q#/lib/todo_ro.b#;$mK=bless({$T2,$hK,$u4,$q,$v4,$q,$w4,$kK,$Q,$lK},$T3);$nK={};$oK=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$pK=bless({$w,$oK,$y,$z},$A);$qK={$J6,$pK};$rK=q#/lib/todo_init.b#;$sK=bless({$T2,$nK,$u4,$q,$v4,$q,$w4,$qK,$Q,$rK},$T3);$tK={};$uK=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$vK=bless({$w,$uK,$y,$z},$A);$wK={$Vs,$vK};$xK=q#/lib/todo_str.b#;$yK=bless({$T2,$tK,$u4,$q,$v4,$q,$w4,$wK,$Q,$xK},$T3);$zK=[$OC,$mK,$sK,$yK];$AK=bless({$T2,$fK,$Q,$gK,$i3,$zK},$g4);$BK=q#ni:/lib/todo.c#;$CK={$g4,1};$DK=q#/lib/todo.c#;$EK=[$TC];$FK=bless({$T2,$CK,$Q,$DK,$i3,$EK},$h4);$GK=q#ni:/lib/todo_ctor.b#;$HK={};$IK=q#ni('ni:/lib/todo')->new(@_)#;$JK=bless({$w,$IK,$y,$z},$A);$KK={$R2,$JK};$LK=q#/lib/todo_ctor.b#;$MK=bless({$T2,$HK,$u4,$q,$v4,$q,$w4,$KK,$Q,$LK},$T3);$NK=q#ni:/lib/todo_init.b#;$OK=q#ni:/lib/todo_ro.b#;$PK=q#ni:/lib/todo_str.b#;$QK=q#ni:/metaclass#;$RK={$h4,1};$SK=q#/metaclass#;$TK=[$Hc,$Xc,$Nc,$Dc];$UK=bless({$T2,$RK,$Q,$SK,$i3,$TK},$i4);$VK=q#ni:/metaclass.c#;$WK={$i4,1};$XK=q#/metaclass.c#;$YK=[$R];$ZK=bless({$T2,$WK,$Q,$XK,$i3,$YK},$h4);$cL=q#ni:/module#;$dL=q#ni:/module.c#;$eL=q#ni:/object#;$fL=q#ni:/object.c#;$gL=q#ni:/semantic#;$hL=q#semantic#;$iL={$hL,1};$jL=[];$kL=bless({$T2,$iL,$Q,$yc,$i3,$jL},$j4);$lL=q#ni:/semantic/dimension#;$mL={$n4,1};$nL=q#/semantic/dimension#;$oL=[$id];$pL=bless({$T2,$mL,$Q,$nL,$i3,$oL},$o4);$qL=q#ni:/semantic/dimension.c#;$rL={$o4,1};$sL=q#/semantic/dimension.c#;$tL=[$wd];$uL=bless({$T2,$rL,$Q,$sL,$i3,$tL},$h4);$vL=q#ni:/semantic/task#;$wL=q#ni:/semantic/task.c#;$xL=q#ni:/semantic/task_outcome.b#;$yL=q#ni:/semantic/task_ro.b#;$zL=q#ni:main#;$AL={$Ol,1};$BL=[$MK,$SF,$xy,$Nl];$CL=bless({$T2,$AL,$Q,$Ol,$i3,$BL},$j4);$DL=q#ni:ni#;$EL={$nz,1};$FL=[$DC,$Iz,$mz];$GL=bless({$T2,$EL,$Q,$nz,$i3,$FL},$j4);$HL={$d,$T,$W,$f1,$g1,$l1,$m1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$J2,$K2,$P2,$Q2,$n6,$o6,$g8,$h8,$n8,$o8,$k9,$l9,$E9,$F9,$V9,$W9,$Xa,$Ya,$hb,$ib,$sb,$tb,$tc,$uc,$zc,$Ac,$id,$jd,$wd,$xd,$Bd,$Cd,$Fd,$Gd,$ke,$le,$Be,$Ce,$Od,$De,$ie,$Ee,$Ve,$We,$cf,$df,$Me,$ef,$Te,$ff,$Lg,$Mg,$Qg,$Rg,$tg,$Sg,$Jg,$Tg,$vf,$Ug,$lg,$Vg,$Pf,$Wg,$of,$Xg,$ri,$vi,$Ti,$Ui,$Ri,$Vi,$Nh,$Wi,$Yh,$Xi,$ph,$Yi,$oi,$Zi,$ih,$cj,$xh,$dj,$xk,$yk,$Ck,$Dk,$tj,$Ek,$Qj,$Fk,$Aj,$Gk,$vk,$Hk,$lj,$Ik,$Yj,$Jk,$el,$fl,$jl,$kl,$cl,$ll,$Rk,$ml,$Nl,$Pl,$pm,$qm,$um,$vm,$Yl,$wm,$nm,$xm,$h6,$ym,$ze,$zm,$xe,$Am,$l5,$Bm,$t5,$Cm,$F5,$Dm,$P4,$Em,$f6,$Fm,$R5,$Gm,$L7,$Hm,$Lm,$Mm,$J7,$Nm,$P6,$Om,$p7,$Pm,$E6,$Qm,$d7,$Rm,$In,$Jn,$Nn,$On,$sn,$Pn,$Gn,$Qn,$ln,$Rn,$dp,$hp,$tp,$up,$rp,$vp,$xq,$Bq,$Vq,$Wq,$Tq,$Xq,$Up,$Yq,$gq,$Zq,$Np,$cr,$sq,$dr,$Fo,$er,$Zo,$fr,$xr,$yr,$Cr,$Dr,$or,$Er,$vr,$Fr,$Jr,$Kr,$Ur,$Vr,$A8,$Wr,$Zr,$cs,$us,$vs,$ms,$ws,$As,$Bs,$ss,$Cs,$Nc,$Ds,$e9,$Es,$Q8,$Fs,$Js,$Ks,$W8,$Ls,$zt,$At,$Ts,$Bt,$qt,$Ct,$xt,$Dt,$Ju,$Ku,$Vu,$Wu,$Tu,$Xu,$ru,$Yu,$du,$Zu,$xu,$cv,$lu,$dv,$Kt,$ev,$Rt,$fv,$Hu,$gv,$y8,$hv,$lw,$pw,$Bw,$Cw,$zw,$Dw,$zv,$Ew,$cw,$Fw,$Rv,$Gw,$jw,$Hw,$Rx,$Sx,$Wx,$Xx,$Px,$Yx,$Zw,$Zx,$Sw,$cy,$vx,$dy,$ky,$ly,$xy,$yy,$Ra,$zy,$Dy,$Ey,$la,$Fy,$Ja,$Gy,$E4,$Hy,$Xc,$Iy,$mz,$oz,$Iz,$Jz,$ct,$Kz,$G8,$Lz,$Qz,$Rz,$Xz,$Yz,$zB,$AB,$EB,$FB,$GA,$GB,$xB,$HB,$dB,$IB,$nB,$JB,$oA,$KB,$DC,$EC,$OC,$PC,$TC,$UC,$MC,$VC,$Hc,$WC,$nD,$oD,$vD,$wD,$PD,$QD,$hE,$iE,$YE,$ZE,$fF,$gF,$qE,$hF,$zE,$iF,$WE,$jF,$jt,$kF,$pF,$qF,$Wb,$rF,$Hb,$sF,$wF,$xF,$Nb,$yF,$Ub,$zF,$SF,$TF,$gd,$UF,$mG,$nG,$eG,$oG,$sG,$tG,$kG,$uG,$pH,$qH,$yH,$zH,$XG,$AH,$nH,$BH,$QG,$CH,$IG,$DH,$wH,$EH,$GG,$FH,$FI,$JI,$eJ,$fJ,$cJ,$gJ,$SI,$hJ,$iI,$iJ,$sI,$jJ,$SH,$kJ,$DI,$lJ,$ZH,$mJ,$TJ,$UJ,$YJ,$ZJ,$KJ,$cK,$vJ,$dK,$RJ,$eK,$AK,$BK,$FK,$GK,$MK,$NK,$sK,$OK,$mK,$PK,$yK,$QK,$UK,$VK,$ZK,$cL,$Qc,$dL,$ud,$eL,$G4,$fL,$rd,$gL,$kL,$lL,$pL,$qL,$uL,$vL,$so,$wL,$np,$xL,$qo,$yL,$eo,$zL,$CL,$DL,$GL};$IL=q#resolvers#;$JL=[];$KL=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$LL=bless({$t,$JL,$v,$q,$w,$KL,$y,$z},$A);$ML=q#file#;$NL=[];$OL=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$PL=bless({$t,$NL,$v,$q,$w,$OL,$y,$z},$A);$QL=q#null#;$RL=[];$SL=q#ni('ni:/io/null')->new#;$TL=bless({$t,$RL,$v,$q,$w,$SL,$y,$z},$A);$UL=q#sh#;$VL=[];$WL=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$XL=bless({$t,$VL,$v,$q,$w,$WL,$y,$z},$A);$YL=q#str#;$ZL=[];$cM=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$dM=bless({$t,$ZL,$v,$q,$w,$cM,$y,$z},$A);$eM={$r7,$LL,$ML,$PL,$QL,$TL,$UL,$XL,$YL,$dM};$fM=bless({$c,$HL,$IL,$eM},$N3);*$sv=\&$pv;*$rv=\&$nv;*$qv=\&$lv;*$Db=\&$Bb;*$Cb=\&$zb;$E4->apply_($j3);$E4->apply_($k3);$E4->apply_($U2);$E4->apply_($l3);$E4->apply_($V2);$E4->apply_($m3);$E4->apply_($W2);$E4->apply_($n3);$E4->apply_($X2);$E4->apply_($o3);$E4->apply_($Y2);$E4->apply_($p3);$E4->apply_($Z2);$E4->apply_($q3);$E4->apply_($c3);$E4->apply_($r3);$E4->apply_($d3);$E4->apply_($s3);$E4->apply_($e3);$E4->apply_($t3);$E4->apply_($f3);$E4->apply_($u3);$E4->apply_($v3);$E4->apply_($w3);$E4->apply_($x3);$E4->apply_($y3);$E4->apply_($z3);$E4->apply_($A3);$E4->apply_($B3);$E4->apply_($C3);$E4->apply_($D3);$E4->apply_($E3);$E4->apply_($F3);$E4->apply_($G3);$E4->apply_($S);$E4->apply_($H3);$E4->apply_($A);$E4->apply_($I3);$E4->apply_($J3);$E4->apply_($K3);$E4->apply_($L3);$E4->apply_($M3);$E4->apply_($N3);$E4->apply_($O3);$E4->apply_($P3);$E4->apply_($Q3);$E4->apply_($R3);$E4->apply_($S3);$E4->apply_($T3);$E4->apply_($U3);$E4->apply_($V3);$E4->apply_($W3);$E4->apply_($X3);$E4->apply_($Y3);$E4->apply_($Z3);$E4->apply_($c4);$E4->apply_($C);$E4->apply_($d4);$E4->apply_($e4);$E4->apply_($f4);$E4->apply_($H);$E4->apply_($g4);$E4->apply_($h4);$E4->apply_($i4);$E4->apply_($j4);$E4->apply_($k4);$E4->apply_($l4);$E4->apply_($m4);$E4->apply_($n4);$E4->apply_($o4);$E4->apply_($p4);$E4->apply_($q4);$P4->apply_($U2);$P4->apply_($V2);$P4->apply_($W2);$P4->apply_($X2);$P4->apply_($Y2);$P4->apply_($Z2);$P4->apply_($c3);$P4->apply_($d3);$P4->apply_($e3);$P4->apply_($f3);$l5->apply_($U2);$l5->apply_($V2);$l5->apply_($W2);$l5->apply_($X2);$l5->apply_($Y2);$l5->apply_($Z2);$l5->apply_($c3);$l5->apply_($d3);$l5->apply_($e3);$l5->apply_($f3);$t5->apply_($U2);$t5->apply_($V2);$t5->apply_($W2);$t5->apply_($X2);$t5->apply_($Y2);$t5->apply_($Z2);$t5->apply_($c3);$t5->apply_($d3);$t5->apply_($e3);$t5->apply_($f3);$F5->apply_($U2);$F5->apply_($V2);$F5->apply_($W2);$F5->apply_($X2);$F5->apply_($Y2);$F5->apply_($Z2);$F5->apply_($c3);$F5->apply_($d3);$F5->apply_($e3);$F5->apply_($f3);$R5->apply_($U2);$R5->apply_($V2);$R5->apply_($W2);$R5->apply_($X2);$R5->apply_($Y2);$R5->apply_($Z2);$R5->apply_($c3);$R5->apply_($d3);$R5->apply_($e3);$R5->apply_($f3);$f6->apply_($U2);$f6->apply_($V2);$f6->apply_($W2);$f6->apply_($X2);$f6->apply_($Y2);$f6->apply_($Z2);$f6->apply_($c3);$f6->apply_($d3);$f6->apply_($e3);$f6->apply_($f3);$E6->apply_($e3);$P6->apply_($e3);$d7->apply_($e3);$p7->apply_($e3);$J7->apply_($e3);$y8->apply_($j3);$y8->apply_($k3);$y8->apply_($l3);$y8->apply_($m3);$y8->apply_($n3);$y8->apply_($o3);$y8->apply_($p3);$y8->apply_($q3);$y8->apply_($r3);$y8->apply_($s3);$y8->apply_($t3);$y8->apply_($u3);$y8->apply_($w3);$y8->apply_($y3);$y8->apply_($A3);$y8->apply_($B3);$y8->apply_($C3);$y8->apply_($D3);$y8->apply_($E3);$y8->apply_($F3);$y8->apply_($G3);$y8->apply_($H3);$y8->apply_($I3);$y8->apply_($K3);$y8->apply_($M3);$y8->apply_($O3);$y8->apply_($Q3);$y8->apply_($S3);$y8->apply_($T3);$y8->apply_($U3);$y8->apply_($V3);$y8->apply_($W3);$y8->apply_($Y3);$y8->apply_($c4);$y8->apply_($d4);$y8->apply_($f4);$y8->apply_($g4);$y8->apply_($h4);$y8->apply_($i4);$y8->apply_($j4);$y8->apply_($k4);$y8->apply_($m4);$y8->apply_($n4);$y8->apply_($o4);$y8->apply_($q4);$G8->apply_($j3);$G8->apply_($k3);$G8->apply_($l3);$G8->apply_($m3);$G8->apply_($n3);$G8->apply_($o3);$G8->apply_($p3);$G8->apply_($q3);$G8->apply_($r3);$G8->apply_($s3);$G8->apply_($t3);$G8->apply_($u3);$G8->apply_($w3);$G8->apply_($y3);$G8->apply_($A3);$G8->apply_($C3);$G8->apply_($D3);$G8->apply_($E3);$G8->apply_($F3);$G8->apply_($G3);$G8->apply_($S);$G8->apply_($H3);$G8->apply_($I3);$G8->apply_($K3);$G8->apply_($M3);$G8->apply_($O3);$G8->apply_($Q3);$G8->apply_($S3);$G8->apply_($T3);$G8->apply_($U3);$G8->apply_($V3);$G8->apply_($W3);$G8->apply_($Y3);$G8->apply_($c4);$G8->apply_($d4);$G8->apply_($f4);$G8->apply_($g4);$G8->apply_($h4);$G8->apply_($i4);$G8->apply_($j4);$G8->apply_($k4);$G8->apply_($m4);$G8->apply_($n4);$G8->apply_($o4);$G8->apply_($q4);$Q8->apply_($F3);$W8->apply_($F3);$la->apply_($L3);$Ja->apply_($L3);$Hb->apply_($T3);$Nb->apply_($T3);$Ub->apply_($F3);$Ub->apply_($T3);$Nc->apply_($j3);$Nc->apply_($k3);$Nc->apply_($l3);$Nc->apply_($m3);$Nc->apply_($n3);$Nc->apply_($o3);$Nc->apply_($p3);$Nc->apply_($q3);$Nc->apply_($r3);$Nc->apply_($s3);$Nc->apply_($t3);$Nc->apply_($u3);$Nc->apply_($w3);$Nc->apply_($y3);$Nc->apply_($A3);$Nc->apply_($C3);$Nc->apply_($E3);$Nc->apply_($G3);$Nc->apply_($H3);$Nc->apply_($I3);$Nc->apply_($K3);$Nc->apply_($M3);$Nc->apply_($O3);$Nc->apply_($Q3);$Nc->apply_($S3);$Nc->apply_($U3);$Nc->apply_($W3);$Nc->apply_($Y3);$Nc->apply_($c4);$Nc->apply_($d4);$Nc->apply_($f4);$Nc->apply_($g4);$Nc->apply_($h4);$Nc->apply_($i4);$Nc->apply_($j4);$Nc->apply_($k4);$Nc->apply_($m4);$Nc->apply_($n4);$Nc->apply_($o4);$Nc->apply_($q4);$Xc->apply_($j3);$Xc->apply_($k3);$Xc->apply_($l3);$Xc->apply_($m3);$Xc->apply_($n3);$Xc->apply_($o3);$Xc->apply_($p3);$Xc->apply_($q3);$Xc->apply_($r3);$Xc->apply_($s3);$Xc->apply_($t3);$Xc->apply_($u3);$Xc->apply_($w3);$Xc->apply_($y3);$Xc->apply_($A3);$Xc->apply_($C3);$Xc->apply_($E3);$Xc->apply_($G3);$Xc->apply_($H3);$Xc->apply_($A);$Xc->apply_($I3);$Xc->apply_($K3);$Xc->apply_($M3);$Xc->apply_($O3);$Xc->apply_($Q3);$Xc->apply_($S3);$Xc->apply_($T3);$Xc->apply_($U3);$Xc->apply_($V3);$Xc->apply_($W3);$Xc->apply_($Y3);$Xc->apply_($c4);$Xc->apply_($d4);$Xc->apply_($f4);$Xc->apply_($g4);$Xc->apply_($h4);$Xc->apply_($i4);$Xc->apply_($k4);$Xc->apply_($m4);$Xc->apply_($n4);$Xc->apply_($o4);$Xc->apply_($q4);$gd->apply_($j3);$gd->apply_($k3);$gd->apply_($l3);$gd->apply_($m3);$gd->apply_($n3);$gd->apply_($o3);$gd->apply_($p3);$gd->apply_($q3);$gd->apply_($r3);$gd->apply_($s3);$gd->apply_($t3);$gd->apply_($u3);$gd->apply_($w3);$gd->apply_($y3);$gd->apply_($A3);$gd->apply_($C3);$gd->apply_($E3);$gd->apply_($G3);$gd->apply_($H3);$gd->apply_($I3);$gd->apply_($K3);$gd->apply_($M3);$gd->apply_($O3);$gd->apply_($Q3);$gd->apply_($S3);$gd->apply_($U3);$gd->apply_($W3);$gd->apply_($Y3);$gd->apply_($c4);$gd->apply_($d4);$gd->apply_($f4);$gd->apply_($g4);$gd->apply_($i4);$gd->apply_($k4);$gd->apply_($m4);$gd->apply_($n4);$gd->apply_($o4);$gd->apply_($q4);$Od->apply_($U2);$ie->apply_($U2);$xe->apply_($l3);$xe->apply_($m3);$xe->apply_($n3);$xe->apply_($o3);$xe->apply_($p3);$xe->apply_($q3);$xe->apply_($r3);$xe->apply_($s3);$xe->apply_($t3);$xe->apply_($u3);$Me->apply_($V2);$Te->apply_($V2);$of->apply_($W2);$vf->apply_($W2);$Pf->apply_($W2);$lg->apply_($W2);$tg->apply_($W2);$Jg->apply_($W2);$ih->apply_($X2);$ih->apply_($Z2);$ph->apply_($X2);$xh->apply_($X2);$Nh->apply_($X2);$Nh->apply_($Z2);$Yh->apply_($X2);$oi->apply_($X2);$oi->apply_($Z2);$Ri->apply_($o3);$lj->apply_($Y2);$tj->apply_($Y2);$Aj->apply_($Y2);$Qj->apply_($Y2);$Yj->apply_($Y2);$vk->apply_($Y2);$Rk->apply_($Z2);$cl->apply_($Z2);$Nl->apply_($Ol);$Yl->apply_($c3);$nm->apply_($c3);$ln->apply_($f3);$sn->apply_($f3);$Gn->apply_($f3);$eo->apply_($v3);$eo->apply_($x3);$eo->apply_($z3);$eo->apply_($p4);$qo->apply_($v3);$qo->apply_($x3);$qo->apply_($z3);$qo->apply_($p4);$Fo->apply_($v3);$Fo->apply_($x3);$Fo->apply_($z3);$Zo->apply_($v3);$Zo->apply_($x3);$Zo->apply_($z3);$rp->apply_($w3);$rp->apply_($y3);$rp->apply_($A3);$Np->apply_($x3);$Up->apply_($x3);$gq->apply_($x3);$sq->apply_($x3);$Tq->apply_($y3);$or->apply_($z3);$vr->apply_($z3);$Ur->apply_($j3);$Ur->apply_($k3);$Ur->apply_($l3);$Ur->apply_($m3);$Ur->apply_($n3);$Ur->apply_($o3);$Ur->apply_($p3);$Ur->apply_($q3);$Ur->apply_($r3);$Ur->apply_($s3);$Ur->apply_($t3);$Ur->apply_($u3);$Ur->apply_($w3);$Ur->apply_($y3);$Ur->apply_($A3);$Ur->apply_($C3);$Ur->apply_($D3);$Ur->apply_($E3);$Ur->apply_($G3);$Ur->apply_($H3);$Ur->apply_($I3);$Ur->apply_($K3);$Ur->apply_($M3);$Ur->apply_($O3);$Ur->apply_($Q3);$Ur->apply_($S3);$Ur->apply_($U3);$Ur->apply_($W3);$Ur->apply_($Y3);$Ur->apply_($c4);$Ur->apply_($d4);$Ur->apply_($f4);$Ur->apply_($g4);$Ur->apply_($h4);$Ur->apply_($i4);$Ur->apply_($j4);$Ur->apply_($k4);$Ur->apply_($m4);$Ur->apply_($n4);$Ur->apply_($o4);$Ur->apply_($q4);$ms->apply_($j3);$ms->apply_($k3);$ms->apply_($l3);$ms->apply_($m3);$ms->apply_($n3);$ms->apply_($o3);$ms->apply_($p3);$ms->apply_($q3);$ms->apply_($r3);$ms->apply_($s3);$ms->apply_($t3);$ms->apply_($u3);$ms->apply_($w3);$ms->apply_($y3);$ms->apply_($A3);$ms->apply_($C3);$ms->apply_($D3);$ms->apply_($E3);$ms->apply_($G3);$ms->apply_($H3);$ms->apply_($I3);$ms->apply_($K3);$ms->apply_($M3);$ms->apply_($O3);$ms->apply_($Q3);$ms->apply_($S3);$ms->apply_($U3);$ms->apply_($W3);$ms->apply_($Y3);$ms->apply_($c4);$ms->apply_($d4);$ms->apply_($f4);$ms->apply_($g4);$ms->apply_($h4);$ms->apply_($i4);$ms->apply_($j4);$ms->apply_($k4);$ms->apply_($m4);$ms->apply_($n4);$ms->apply_($o4);$ms->apply_($q4);$ss->apply_($D3);$Ts->apply_($j3);$Ts->apply_($k3);$Ts->apply_($l3);$Ts->apply_($m3);$Ts->apply_($n3);$Ts->apply_($o3);$Ts->apply_($p3);$Ts->apply_($q3);$Ts->apply_($r3);$Ts->apply_($s3);$Ts->apply_($t3);$Ts->apply_($u3);$Ts->apply_($w3);$Ts->apply_($y3);$Ts->apply_($A3);$Ts->apply_($C3);$Ts->apply_($D3);$Ts->apply_($E3);$Ts->apply_($G3);$Ts->apply_($H3);$Ts->apply_($I3);$Ts->apply_($K3);$Ts->apply_($M3);$Ts->apply_($O3);$Ts->apply_($Q3);$Ts->apply_($S3);$Ts->apply_($U3);$Ts->apply_($W3);$Ts->apply_($Y3);$Ts->apply_($c4);$Ts->apply_($d4);$Ts->apply_($f4);$Ts->apply_($g4);$Ts->apply_($h4);$Ts->apply_($i4);$Ts->apply_($j4);$Ts->apply_($k4);$Ts->apply_($m4);$Ts->apply_($n4);$Ts->apply_($o4);$Ts->apply_($q4);$ct->apply_($j3);$ct->apply_($k3);$ct->apply_($l3);$ct->apply_($m3);$ct->apply_($n3);$ct->apply_($o3);$ct->apply_($p3);$ct->apply_($q3);$ct->apply_($r3);$ct->apply_($s3);$ct->apply_($t3);$ct->apply_($u3);$ct->apply_($w3);$ct->apply_($y3);$ct->apply_($A3);$ct->apply_($C3);$ct->apply_($D3);$ct->apply_($E3);$ct->apply_($G3);$ct->apply_($H3);$ct->apply_($I3);$ct->apply_($K3);$ct->apply_($M3);$ct->apply_($O3);$ct->apply_($Q3);$ct->apply_($S3);$ct->apply_($U3);$ct->apply_($W3);$ct->apply_($Y3);$ct->apply_($c4);$ct->apply_($d4);$ct->apply_($f4);$ct->apply_($g4);$ct->apply_($h4);$ct->apply_($i4);$ct->apply_($j4);$ct->apply_($k4);$ct->apply_($m4);$ct->apply_($n4);$ct->apply_($o4);$ct->apply_($q4);$jt->apply_($j3);$jt->apply_($k3);$jt->apply_($l3);$jt->apply_($m3);$jt->apply_($n3);$jt->apply_($o3);$jt->apply_($p3);$jt->apply_($q3);$jt->apply_($r3);$jt->apply_($s3);$jt->apply_($t3);$jt->apply_($u3);$jt->apply_($w3);$jt->apply_($y3);$jt->apply_($A3);$jt->apply_($C3);$jt->apply_($D3);$jt->apply_($E3);$jt->apply_($G3);$jt->apply_($H3);$jt->apply_($I3);$jt->apply_($K3);$jt->apply_($M3);$jt->apply_($O3);$jt->apply_($Q3);$jt->apply_($S3);$jt->apply_($U3);$jt->apply_($W3);$jt->apply_($Y3);$jt->apply_($c4);$jt->apply_($d4);$jt->apply_($f4);$jt->apply_($g4);$jt->apply_($h4);$jt->apply_($i4);$jt->apply_($j4);$jt->apply_($k4);$jt->apply_($m4);$jt->apply_($n4);$jt->apply_($o4);$jt->apply_($q4);$qt->apply_($j3);$qt->apply_($k3);$qt->apply_($l3);$qt->apply_($m3);$qt->apply_($n3);$qt->apply_($o3);$qt->apply_($p3);$qt->apply_($q3);$qt->apply_($r3);$qt->apply_($s3);$qt->apply_($t3);$qt->apply_($u3);$qt->apply_($w3);$qt->apply_($y3);$qt->apply_($A3);$qt->apply_($C3);$qt->apply_($D3);$qt->apply_($E3);$qt->apply_($G3);$qt->apply_($H3);$qt->apply_($I3);$qt->apply_($K3);$qt->apply_($M3);$qt->apply_($O3);$qt->apply_($Q3);$qt->apply_($S3);$qt->apply_($U3);$qt->apply_($W3);$qt->apply_($Y3);$qt->apply_($c4);$qt->apply_($d4);$qt->apply_($f4);$qt->apply_($g4);$qt->apply_($h4);$qt->apply_($i4);$qt->apply_($j4);$qt->apply_($k4);$qt->apply_($m4);$qt->apply_($n4);$qt->apply_($o4);$qt->apply_($q4);$xt->apply_($j3);$xt->apply_($k3);$xt->apply_($l3);$xt->apply_($m3);$xt->apply_($n3);$xt->apply_($o3);$xt->apply_($p3);$xt->apply_($q3);$xt->apply_($r3);$xt->apply_($s3);$xt->apply_($t3);$xt->apply_($u3);$xt->apply_($w3);$xt->apply_($y3);$xt->apply_($A3);$xt->apply_($C3);$xt->apply_($D3);$xt->apply_($E3);$xt->apply_($G3);$xt->apply_($H3);$xt->apply_($I3);$xt->apply_($K3);$xt->apply_($M3);$xt->apply_($O3);$xt->apply_($Q3);$xt->apply_($S3);$xt->apply_($U3);$xt->apply_($W3);$xt->apply_($Y3);$xt->apply_($c4);$xt->apply_($d4);$xt->apply_($f4);$xt->apply_($g4);$xt->apply_($h4);$xt->apply_($i4);$xt->apply_($j4);$xt->apply_($k4);$xt->apply_($m4);$xt->apply_($n4);$xt->apply_($o4);$xt->apply_($q4);$Kt->apply_($S);$Rt->apply_($S);$du->apply_($S);$lu->apply_($S);$ru->apply_($S);$xu->apply_($S);$Hu->apply_($S);$Tu->apply_($H3);$zv->apply_($A);$Rv->apply_($A);$cw->apply_($A);$jw->apply_($A);$zw->apply_($I3);$Sw->apply_($J3);$Zw->apply_($J3);$vx->apply_($J3);$Px->apply_($J3);$ky->apply_($L3);$xy->apply_($Ol);$mz->apply_($nz);$Iz->apply_($nz);$Qz->apply_($j3);$Qz->apply_($k3);$Qz->apply_($l3);$Qz->apply_($m3);$Qz->apply_($n3);$Qz->apply_($o3);$Qz->apply_($p3);$Qz->apply_($q3);$Qz->apply_($r3);$Qz->apply_($s3);$Qz->apply_($t3);$Qz->apply_($u3);$Qz->apply_($w3);$Qz->apply_($y3);$Qz->apply_($A3);$Qz->apply_($C3);$Qz->apply_($D3);$Qz->apply_($E3);$Qz->apply_($F3);$Qz->apply_($G3);$Qz->apply_($H3);$Qz->apply_($I3);$Qz->apply_($K3);$Qz->apply_($M3);$Qz->apply_($O3);$Qz->apply_($Q3);$Qz->apply_($S3);$Qz->apply_($T3);$Qz->apply_($U3);$Qz->apply_($V3);$Qz->apply_($W3);$Qz->apply_($Y3);$Qz->apply_($c4);$Qz->apply_($d4);$Qz->apply_($f4);$Qz->apply_($g4);$Qz->apply_($h4);$Qz->apply_($i4);$Qz->apply_($j4);$Qz->apply_($k4);$Qz->apply_($m4);$Qz->apply_($n4);$Qz->apply_($o4);$Qz->apply_($q4);$Xz->apply_($j3);$Xz->apply_($k3);$Xz->apply_($l3);$Xz->apply_($m3);$Xz->apply_($n3);$Xz->apply_($o3);$Xz->apply_($p3);$Xz->apply_($q3);$Xz->apply_($r3);$Xz->apply_($s3);$Xz->apply_($t3);$Xz->apply_($u3);$Xz->apply_($w3);$Xz->apply_($y3);$Xz->apply_($A3);$Xz->apply_($C3);$Xz->apply_($D3);$Xz->apply_($E3);$Xz->apply_($F3);$Xz->apply_($G3);$Xz->apply_($H3);$Xz->apply_($I3);$Xz->apply_($K3);$Xz->apply_($M3);$Xz->apply_($O3);$Xz->apply_($Q3);$Xz->apply_($S3);$Xz->apply_($T3);$Xz->apply_($U3);$Xz->apply_($V3);$Xz->apply_($W3);$Xz->apply_($Y3);$Xz->apply_($c4);$Xz->apply_($d4);$Xz->apply_($f4);$Xz->apply_($g4);$Xz->apply_($h4);$Xz->apply_($i4);$Xz->apply_($j4);$Xz->apply_($k4);$Xz->apply_($m4);$Xz->apply_($n4);$Xz->apply_($o4);$Xz->apply_($q4);$oA->apply_($N3);$GA->apply_($N3);$dB->apply_($N3);$nB->apply_($N3);$xB->apply_($N3);$DC->apply_($nz);$MC->apply_($P3);$MC->apply_($C);$MC->apply_($H);$nD->apply_($L3);$vD->apply_($L3);$vD->apply_($R3);$PD->apply_($L3);$hE->apply_($L3);$hE->apply_($R3);$qE->apply_($R3);$zE->apply_($R3);$WE->apply_($L3);$WE->apply_($R3);$pF->apply_($j3);$pF->apply_($k3);$pF->apply_($l3);$pF->apply_($m3);$pF->apply_($n3);$pF->apply_($o3);$pF->apply_($p3);$pF->apply_($q3);$pF->apply_($r3);$pF->apply_($s3);$pF->apply_($t3);$pF->apply_($u3);$pF->apply_($w3);$pF->apply_($y3);$pF->apply_($A3);$pF->apply_($C3);$pF->apply_($D3);$pF->apply_($E3);$pF->apply_($F3);$pF->apply_($G3);$pF->apply_($H3);$pF->apply_($I3);$pF->apply_($K3);$pF->apply_($M3);$pF->apply_($O3);$pF->apply_($Q3);$pF->apply_($S3);$pF->apply_($U3);$pF->apply_($V3);$pF->apply_($W3);$pF->apply_($Y3);$pF->apply_($c4);$pF->apply_($d4);$pF->apply_($f4);$pF->apply_($g4);$pF->apply_($h4);$pF->apply_($i4);$pF->apply_($j4);$pF->apply_($k4);$pF->apply_($m4);$pF->apply_($n4);$pF->apply_($o4);$pF->apply_($q4);$SF->apply_($Ol);$eG->apply_($V3);$kG->apply_($V3);$GG->apply_($X3);$GG->apply_($Z3);$QG->apply_($X3);$XG->apply_($X3);$nH->apply_($X3);$SH->apply_($C);$ZH->apply_($C);$iI->apply_($C);$sI->apply_($C);$DI->apply_($C);$SI->apply_($d4);$cJ->apply_($d4);$vJ->apply_($e4);$KJ->apply_($e4);$RJ->apply_($e4);$mK->apply_($H);$sK->apply_($H);$yK->apply_($H);$MK->apply_($Ol);$ni::self=$fM;&$V($T);&$V($f1);&$V($l1);&$V($y1);&$V($L1);&$V($Y1);&$V($n2);&$V($J2);&$V($P2);&$V($E4);&$V($G4);&$I4($G4);&$V($P4);&$V($l5);&$V($t5);&$V($F5);&$V($R5);&$V($f6);&$V($h6);&$I4($h6);&$V($n6);&$V($E6);&$V($P6);&$V($d7);&$V($p7);&$V($J7);&$V($L7);&$I4($L7);&$V($g8);&$V($n8);&$V($y8);&$V($A8);&$I4($A8);&$V($G8);&$V($Q8);&$V($W8);&$V($e9);&$I4($e9);&$V($k9);&$V($E9);&$V($V9);&$V($la);&$V($Ja);&$V($Ra);&$I4($Ra);&$V($Xa);&$V($hb);&$V($sb);&$V($Hb);&$V($Nb);&$V($Ub);&$V($Wb);&$I4($Wb);&$V($tc);&$V($zc);&$V($Hc);&$V($Nc);&$V($Qc);&$I4($Qc);&$V($Xc);&$V($gd);&$V($id);&$I4($id);&$V($rd);&$I4($rd);&$V($ud);&$I4($ud);&$V($wd);&$I4($wd);&$V($Bd);&$I4($Bd);&$V($Fd);&$I4($Fd);&$V($Od);&$V($ie);&$V($ke);&$I4($ke);&$V($xe);&$V($ze);&$I4($ze);&$V($Be);&$I4($Be);&$V($Me);&$V($Te);&$V($Ve);&$I4($Ve);&$V($cf);&$I4($cf);&$V($of);&$V($vf);&$V($Pf);&$V($lg);&$V($tg);&$V($Jg);&$V($Lg);&$I4($Lg);&$V($Qg);&$I4($Qg);&$V($ih);&$V($ph);&$V($xh);&$V($Nh);&$V($Yh);&$V($oi);&$V($ri);&$I4($ri);&$ui($ri);&$V($Ri);&$V($Ti);&$I4($Ti);&$V($lj);&$V($tj);&$V($Aj);&$V($Qj);&$V($Yj);&$V($vk);&$V($xk);&$I4($xk);&$V($Ck);&$I4($Ck);&$V($Rk);&$V($cl);&$V($el);&$I4($el);&$V($jl);&$I4($jl);&$V($Nl);&$V($Yl);&$V($nm);&$V($pm);&$I4($pm);&$V($um);&$I4($um);&$V($Lm);&$I4($Lm);&$V($ln);&$V($sn);&$V($Gn);&$V($In);&$I4($In);&$V($Nn);&$I4($Nn);&$V($eo);&$V($qo);&$V($so);&$I4($so);&$V($Fo);&$V($Zo);&$V($dp);&$I4($dp);&$gp($dp);&$V($np);&$I4($np);&$V($rp);&$V($tp);&$I4($tp);&$V($Np);&$V($Up);&$V($gq);&$V($sq);&$V($xq);&$I4($xq);&$gp($xq);&$Aq($xq);&$V($Tq);&$V($Vq);&$I4($Vq);&$V($or);&$V($vr);&$V($xr);&$I4($xr);&$gp($xr);&$V($Cr);&$I4($Cr);&$V($Jr);&$I4($Jr);&$V($Ur);&$V($Zr);&$I4($Zr);&$V($ms);&$V($ss);&$V($us);&$I4($us);&$V($As);&$I4($As);&$V($Js);&$I4($Js);&$V($Ts);&$V($ct);&$V($jt);&$V($qt);&$V($xt);&$V($zt);&$V($Kt);&$V($Rt);&$V($du);&$V($lu);&$V($ru);&$V($xu);&$V($Hu);&$V($Ju);&$I4($Ju);&$V($Tu);&$V($Vu);&$I4($Vu);&$V($zv);&$V($Rv);&$V($cw);&$V($jw);&$V($lw);&$I4($lw);&$ow($lw);&$V($zw);&$V($Bw);&$I4($Bw);&$V($Sw);&$V($Zw);&$V($vx);&$V($Px);&$V($Rx);&$I4($Rx);&$V($Wx);&$I4($Wx);&$V($ky);&$V($xy);&$V($Dy);&$I4($Dy);&$V($mz);&$V($Iz);&$V($Qz);&$V($Xz);&$V($oA);&$V($GA);&$V($dB);&$V($nB);&$V($xB);&$V($zB);&$I4($zB);&$V($EB);&$I4($EB);&$V($DC);&$V($MC);&$V($OC);&$I4($OC);&$V($TC);&$I4($TC);&$V($nD);&$V($vD);&$V($PD);&$V($hE);&$V($qE);&$V($zE);&$V($WE);&$V($YE);&$I4($YE);&$V($fF);&$I4($fF);&$V($pF);&$V($wF);&$I4($wF);&$V($SF);&$V($eG);&$V($kG);&$V($mG);&$I4($mG);&$V($sG);&$I4($sG);&$V($GG);&$V($IG);&$I4($IG);&$V($QG);&$V($XG);&$V($nH);&$V($pH);&$I4($pH);&$V($wH);&$I4($wH);&$V($yH);&$I4($yH);&$V($SH);&$V($ZH);&$V($iI);&$V($sI);&$V($DI);&$V($FI);&$I4($FI);&$II($FI);&$V($SI);&$V($cJ);&$V($eJ);&$I4($eJ);&$V($vJ);&$V($KJ);&$V($RJ);&$V($TJ);&$I4($TJ);&$V($YJ);&$I4($YJ);&$V($mK);&$V($sK);&$V($yK);&$V($AK);&$I4($AK);&$V($FK);&$I4($FK);&$V($MK);&$V($UK);&$I4($UK);&$V($ZK);&$I4($ZK);&$V($kL);&$I4($kL);&$V($pL);&$I4($pL);&$V($uL);&$I4($uL);&$V($CL);&$I4($CL);&$V($GL);&$I4($GL);ni->run(@ARGV);
__DATA__
