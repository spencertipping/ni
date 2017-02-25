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
+{name => $name, data => {@_}};#;$T8=bless({$w,$S8,$y,$z},$A);$U8={$J6,$T8};$V8=q#/lib/dataslice_init.b#;$W8=bless({$T2,$R8,$u4,$q,$v4,$q,$w4,$U8,$Q,$V8},$T3);$X8={};$Y8=q#namespace#;$Z8=q#'ni'#;$c9=bless({$w,$Z8,$y,$z},$A);$d9={$Y8,$c9};$e9=q#/lib/named_in_ni.b#;$f9=bless({$T2,$X8,$u4,$q,$v4,$q,$w4,$d9,$Q,$e9},$T3);$g9={};$h9=q#package#;$i9=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$j9=bless({$w,$i9,$y,$z},$A);$k9={$h9,$j9};$l9=q#/lib/namespaced.b#;$m9=bless({$T2,$g9,$u4,$q,$v4,$q,$w4,$k9,$Q,$l9},$T3);$n9={};$o9=q#resolve#;$p9=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$q9=bless({$w,$p9,$y,$z},$A);$r9={$o9,$q9};$s9=q#/lib/resolver.b#;$t9=bless({$T2,$n9,$u4,$q,$v4,$q,$w4,$r9,$Q,$s9},$T3);$u9={};$v9=q#serialize#;$w9=[];$x9=q#local $_;
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
$g;#;$y9=bless({$t,$w9,$v,$q,$w,$x9,$y,$z},$A);$z9={$v9,$y9};$A9=q#/lib/slice_serialize.b#;$B9=bless({$T2,$u9,$u4,$q,$v4,$q,$w4,$z9,$Q,$A9},$T3);$C9=[$A8,$G8,$Q8,$W8,$f9,$m9,$t9,$B9];$D9=bless({$T2,$p8,$Q,$q8,$i3,$C9},$G3);$E9=q#Fix serialization for dataslices#;$F9=[$E9];$G9=bless({$S2,$D9,$E,$F9},$H);$H9=[$R2,$G9];$I9=[$H9];$J9=bless({$e,$I9,$Q,$q8},$S);$K9=q#ni.doc:/lib/doc#;$L9=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$M9=[$f,$L9];$N9=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$O9=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$P9=[];$Q9=[];$R9=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$S9=bless({$t,$Q9,$v,$q,$w,$R9,$y,$z},$A);$T9=bless({$n,$P9,$p,$q,$r,$q,$s,$S9},$C);$U9=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$V9=[];$W9=[];$X9=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$Y9=bless({$t,$W9,$v,$q,$w,$X9,$y,$z},$A);$Z9=bless({$n,$V9,$p,$q,$r,$q,$s,$Y9},$C);$ca=[$i,$N9,$O9,$T9,$U9,$Z9];$da=[$M9,$ca];$ea=q#/lib/doc#;$fa=bless({$e,$da,$Q,$ea},$S);$ga=q#ni.doc:/lib/future#;$ha=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$ia=[];$ja=[];$ka=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$la=bless({$t,$ja,$v,$q,$w,$ka,$y,$z},$A);$ma=bless({$n,$ia,$p,$q,$r,$q,$s,$la},$C);$na=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$oa=[];$pa=[];$qa=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$ra=bless({$t,$pa,$v,$q,$w,$qa,$y,$z},$A);$sa=bless({$n,$oa,$p,$q,$r,$q,$s,$ra},$C);$ta=[$i,$ha,$ma,$na,$sa];$ua=[$ta];$va=q#/lib/future#;$wa=bless({$e,$ua,$Q,$va},$S);$xa=q#ni.doc:/lib/image#;$ya=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$za=[$f,$ya];$Aa=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$Ba=[$i,$Aa];$Ca={$L3,1};$Da=q#/lib/image#;$Ea={};$Fa=[];$Ga=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Ha=bless({$t,$Fa,$v,$q,$w,$Ga,$y,$z},$A);$Ia={$J6,$Ha};$Ja=q#/lib/image_init.b#;$Ka=bless({$T2,$Ea,$u4,$q,$v4,$q,$w4,$Ia,$Q,$Ja},$T3);$La={};$Ma=q#boot_side_effect#;$Na=[];$Oa=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Pa=bless({$t,$Na,$v,$q,$w,$Oa,$y,$z},$A);$Qa=q#finalizer#;$Ra=[];$Sa=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Ta=bless({$t,$Ra,$v,$q,$w,$Sa,$y,$z},$A);$Ua=q#io#;$Va=[];$Wa=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Xa=bless({$t,$Va,$v,$q,$w,$Wa,$y,$z},$A);$Ya=q#reconstruction#;$Za=[];$cb=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$db=bless({$t,$Za,$v,$q,$w,$cb,$y,$z},$A);$eb=q#side_effect#;$fb=[];$gb=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$hb=bless({$t,$fb,$v,$q,$w,$gb,$y,$z},$A);$ib={$Ma,$Pa,$Qa,$Ta,$Ua,$Xa,$Ya,$db,$eb,$hb};$jb=q#/lib/image_quoting.b#;$kb=bless({$T2,$La,$u4,$q,$v4,$q,$w4,$ib,$Q,$jb},$T3);$lb={};$mb=q#quote_code#;$nb=[];$ob=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$pb=bless({$t,$nb,$v,$q,$w,$ob,$y,$z},$A);$qb={$mb,$pb};$rb=q#/lib/quote_code_fail.b#;$sb=bless({$T2,$lb,$u4,$q,$v4,$q,$w4,$qb,$Q,$rb},$T3);$tb={};$ub=q#quote_array#;$vb=[];$wb=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$xb=bless({$t,$vb,$v,$q,$w,$wb,$y,$z},$A);$yb=q#quote_hash#;$zb=[];$Ab=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Bb=bless({$t,$zb,$v,$q,$w,$Ab,$y,$z},$A);$Cb=q#quote_scalar#;$Db=[];$Eb=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Fb=bless({$t,$Db,$v,$q,$w,$Eb,$y,$z},$A);$Gb=q#quote_scalar_ref#;$Hb=[];$Ib=q#'\\\\' . shift->quote(${$_[0]})#;$Jb=bless({$t,$Hb,$v,$q,$w,$Ib,$y,$z},$A);$Kb=q#quote_value#;$Lb=[];$Mb=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Nb=bless({$t,$Lb,$v,$q,$w,$Mb,$y,$z},$A);$Ob={$ub,$xb,$yb,$Bb,$Cb,$Fb,$Gb,$Jb,$Kb,$Nb};$Pb=q#/lib/quote_values.b#;$Qb=bless({$T2,$tb,$u4,$q,$v4,$q,$w4,$Ob,$Q,$Pb},$T3);$Rb={};$Sb=q#quote_blessed#;$Tb=[];$Ub=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Vb=bless({$t,$Tb,$v,$q,$w,$Ub,$y,$z},$A);$Wb=q#quote_class#;$Xb=[];$Yb=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Zb=bless({$t,$Xb,$v,$q,$w,$Yb,$y,$z},$A);$cc=q#quote_object#;$dc=[];$ec=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$fc=bless({$t,$dc,$v,$q,$w,$ec,$y,$z},$A);$gc={$Sb,$Vb,$Wb,$Zb,$cc,$fc};$hc=q#/lib/quote_objects.b#;$ic=bless({$T2,$Rb,$u4,$q,$v4,$q,$w4,$gc,$Q,$hc},$T3);$jc={};$kc=q#circular_arrayref#;$lc=[];$mc=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$nc=bless({$t,$lc,$v,$q,$w,$mc,$y,$z},$A);$oc=q#circular_hashref#;$pc=[];$qc=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$rc=bless({$t,$pc,$v,$q,$w,$qc,$y,$z},$A);$sc=q#is_circular#;$tc=[];$uc=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$vc=bless({$t,$tc,$v,$q,$w,$uc,$y,$z},$A);$wc={$kc,$nc,$oc,$rc,$sc,$vc};$xc=q#/lib/quote_circular_addressed.b#;$yc=bless({$T2,$jc,$u4,$q,$v4,$q,$w4,$wc,$Q,$xc},$T3);$zc={};$Ac=q#address#;$Bc=[];$Cc=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Dc=bless({$t,$Bc,$v,$q,$w,$Cc,$y,$z},$A);$Ec=q#allocate_gensym#;$Fc=[];$Gc=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Hc=bless({$t,$Fc,$v,$q,$w,$Gc,$y,$z},$A);$Ic=q#circular_links#;$Jc=[];$Kc=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Lc=bless({$t,$Jc,$v,$q,$w,$Kc,$y,$z},$A);$Mc=q#quote#;$Nc=[];$Oc=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Pc=bless({$t,$Nc,$v,$q,$w,$Oc,$y,$z},$A);$Qc={$Ac,$Dc,$Ec,$Hc,$Ic,$Lc,$Mc,$Pc};$Rc=q#/lib/quote_gensym_identity.b#;$Sc=bless({$T2,$zc,$u4,$q,$v4,$q,$w4,$Qc,$Q,$Rc},$T3);$Tc={};$Uc=q#gensym#;$Vc=[];$Wc=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Xc=bless({$t,$Vc,$v,$q,$w,$Wc,$y,$z},$A);$Yc={$Uc,$Xc};$Zc=q#/lib/gensym_generator_compact.b#;$cd=bless({$T2,$Tc,$u4,$q,$v4,$q,$w4,$Yc,$Q,$Zc},$T3);$dd=[$G4,$Ka,$kb,$sb,$Qb,$ic,$yc,$Sc,$cd];$ed=bless({$T2,$Ca,$Q,$Da,$i3,$dd},$M3);$fd=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$gd=[$fd];$hd=bless({$S2,$ed,$E,$gd},$H);$id=[$R2,$hd];$jd=[$za,$Ba,$id];$kd=bless({$e,$jd,$Q,$Da},$S);$ld=q#ni.doc:/lib/ni#;$md=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$nd=[$f,$md];$od=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$pd=[$i,$od];$qd=[$nd,$pd];$rd=q#/lib/ni#;$sd=bless({$e,$qd,$Q,$rd},$S);$td=q#ni.doc:/lib/quote_simple#;$ud=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$vd=[];$wd=[];$xd=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$yd=bless({$t,$wd,$v,$q,$w,$xd,$y,$z},$A);$zd=bless({$n,$vd,$p,$q,$r,$q,$s,$yd},$C);$Ad=[$i,$ud,$zd];$Bd=[$Ad];$Cd=q#/lib/quote_simple#;$Dd=bless({$e,$Bd,$Q,$Cd},$S);$Ed=q#ni.doc:/lib/slice#;$Fd=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$Gd=[$f,$Fd];$Hd={$T3,1};$Id=q#/lib/slice#;$Jd=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$Kd=bless({$w,$Jd,$y,$z},$A);$Ld=q#local $_;
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
$self;#;$Md=bless({$w,$Ld,$y,$z},$A);$Nd=q#lib/slice::apply#;$Od=q#lib/slice::apply_#;$Pd={};$Qd={$I8,$Kd,$L8,$Md};$Rd=q#/lib/slice.b#;$Sd=bless({$T2,$Pd,$w4,$Qd,$Q,$Rd},$T3);$Td={};$Ud=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$Vd=bless({$w,$Ud,$y,$z},$A);$Wd={$J6,$Vd};$Xd=q#/lib/slice_init.b#;$Yd=bless({$T2,$Td,$w4,$Wd,$Q,$Xd},$T3);$Zd=[$A8,$G8,$Sd,$Yd,$B9];$ce=bless({$T2,$Hd,$Q,$Id,$i3,$Zd},$U3);$de=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$ee=[$de];$fe=bless({$S2,$ce,$E,$ee},$H);$ge=[$R2,$fe];$he=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$ie=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$je=[];$ke=[];$le=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$me=bless({$t,$ke,$v,$q,$w,$le,$y,$z},$A);$ne=bless({$n,$je,$p,$q,$r,$q,$s,$me},$C);$oe=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$pe=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$qe=[];$re=[];$se=q#my $instances = 0;
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
now $instances == 0;#;$te=bless({$t,$re,$v,$q,$w,$se,$y,$z},$A);$ue=bless({$n,$qe,$p,$q,$r,$q,$s,$te},$C);$ve=[$i,$he,$ie,$ne,$oe,$pe,$ue];$we=[$Gd,$ge,$ve];$xe=bless({$e,$we,$Q,$Id},$S);$ye=q#ni.doc:/semantic#;$ze=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$Ae=[$i,$ze];$Be=[$Ae];$Ce=q#/semantic#;$De=bless({$e,$Be,$Q,$Ce},$S);$Ee=q#ni:/class#;$Fe={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Ge={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$He=q#/module#;$Ie=q#/lib/perlbranch.b#;$Je={};$Ke=q#add#;$Le=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Me=bless({$w,$Le,$y,$z},$A);$Ne=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Oe=bless({$w,$Ne,$y,$z},$A);$Pe={$Ke,$Me,$I8,$Oe};$Qe=q#/lib/branch.b#;$Re=bless({$T2,$Je,$u4,$q,$v4,$q,$w4,$Pe,$Q,$Qe},$T3);$Se=[$Re,$G8,$f9,$m9,$t9];$Te=bless({$Q,$Ie,$i3,$Se},$V3);$Ue={};$Ve=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$We=bless({$w,$Ve,$y,$z},$A);$Xe={$J6,$We};$Ye=q#/lib/class_init.b#;$Ze=bless({$T2,$Ue,$u4,$I4,$v4,$q,$w4,$Xe,$Q,$Ye},$T3);$cf={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$df=q#/lib/definition.b#;$ef={};$ff=q#def#;$gf=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$hf=bless({$w,$gf,$y,$z},$A);$if={$ff,$hf};$jf=q#/lib/definition_def.b#;$kf=bless({$T2,$ef,$u4,$q,$v4,$q,$w4,$if,$Q,$jf},$T3);$lf={};$mf=q#ro#;$nf=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$of=bless({$w,$nf,$y,$z},$A);$pf=q#rw#;$qf=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$rf=bless({$w,$qf,$y,$z},$A);$sf={$mf,$of,$pf,$rf};$tf=q#/lib/accessor.b#;$uf=bless({$T2,$lf,$u4,$q,$v4,$q,$w4,$sf,$Q,$tf},$T3);$vf={};$wf=q#(""#;$xf=q#shift->name#;$yf=bless({$w,$xf,$y,$z},$A);$zf={$wf,$yf};$Af=q#/lib/name_as_string.b#;$Bf=bless({$T2,$vf,$u4,$q,$v4,$q,$w4,$zf,$Q,$Af},$T3);$Cf={};$Df=q#(eq#;$Ef=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Ff=bless({$w,$Ef,$y,$z},$A);$Gf={$Df,$Ff};$Hf=q#/lib/ref_eq.b#;$If=bless({$T2,$Cf,$u4,$q,$v4,$q,$w4,$Gf,$Q,$Hf},$T3);$Jf={};$Kf=q#defdata#;$Lf=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$Mf=bless({$w,$Lf,$y,$z},$A);$Nf={$Kf,$Mf};$Of=q#/lib/definition_defdata.b#;$Pf=bless({$T2,$Jf,$u4,$q,$v4,$q,$w4,$Nf,$Q,$Of},$T3);$Qf={};$Rf=q#instantiate_with_defaults#;$Sf=q#my ($class, @slots) = @_;
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
    }));#;$Tf=bless({$w,$Sf,$y,$z},$A);$Uf={$Rf,$Tf};$Vf=q#/lib/definition_init_with_defaults.b#;$Wf=bless({$T2,$Qf,$u4,$q,$v4,$q,$w4,$Uf,$Q,$Vf},$T3);$Xf=[$kf,$uf,$Bf,$If,$Pf,$Wf];$Yf=bless({$T2,$cf,$Q,$df,$i3,$Xf},$D3);$Zf=[$Te,$Ze,$G4,$A8,$Yf];$cg=bless({$T2,$Ge,$Q,$He,$i3,$Zf},$k4);$dg={};$eg=q#new#;$fg=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$gg=bless({$w,$fg,$y,$z},$A);$hg={$eg,$gg};$ig=q#/lib/instantiable.b#;$jg=bless({$T2,$dg,$w4,$hg,$Q,$ig},$T3);$kg={};$lg=q#child#;$mg=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$ng=bless({$w,$mg,$y,$z},$A);$og={$lg,$ng};$pg=q#/lib/subclass.b#;$qg=bless({$T2,$kg,$u4,$q,$v4,$q,$w4,$og,$Q,$pg},$T3);$rg=[$cg,$jg,$Ze,$cg,$qg];$sg=bless({$T2,$Fe,$Q,$R,$i3,$rg},$k3);$tg=q#ni:/class.c#;$ug={$k3,1,$o4,1};$vg=q#/class.c#;$wg={$k3,1,$k4,1,$o4,1};$xg=q#/module.c#;$yg={$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$k4,1,$m4,1,$o4,1,$q4,1};$zg=q#/object.c#;$Ag=[$sg];$Bg=bless({$T2,$yg,$Q,$zg,$i3,$Ag},$h4);$Cg={$k3,1,$C3,1,$E3,1,$G3,1,$U3,1,$W3,1,$k4,1,$o4,1};$Dg=q#/lib/behavior.c#;$Eg=[$Bg];$Fg=bless({$T2,$Cg,$Q,$Dg,$i3,$Eg},$h4);$Gg=[$Bg,$jg,$Fg];$Hg=bless({$T2,$wg,$Q,$xg,$i3,$Gg},$h4);$Ig=[$Hg];$Jg=bless({$T2,$ug,$Q,$vg,$i3,$Ig},$h4);$Kg=q#ni:/fabric#;$Lg=q#fabric#;$Mg={$Lg,1};$Ng=[];$Og=bless({$T2,$Mg,$Q,$e1,$i3,$Ng},$j4);$Pg=q#ni:/io#;$Qg={$Ua,1};$Rg=[];$Sg=bless({$T2,$Qg,$Q,$k1,$i3,$Rg},$j4);$Tg=q#ni:/io/buffer#;$Ug={$U2,1};$Vg={};$Wg=[];$Xg=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Yg=bless({$t,$Wg,$v,$q,$w,$Xg,$y,$z},$A);$Zg={$J6,$Yg};$ch=q#/io/buffer_init.b#;$dh=bless({$T2,$Vg,$u4,$q,$v4,$q,$w4,$Zg,$Q,$ch},$T3);$eh={};$fh=[];$gh=q#my $self = shift;
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
}#;$hh=bless({$t,$fh,$v,$q,$w,$gh,$y,$z},$A);$ih=q#read_capacity#;$jh=[];$kh=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$lh=bless({$t,$jh,$v,$q,$w,$kh,$y,$z},$A);$mh=[];$nh=q#my $self = shift;
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
}#;$oh=bless({$t,$mh,$v,$q,$w,$nh,$y,$z},$A);$ph=q#write_capacity#;$qh=[];$rh=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$sh=bless({$t,$qh,$v,$q,$w,$rh,$y,$z},$A);$th={$f7,$hh,$ih,$lh,$j7,$oh,$ph,$sh};$uh=q#/io/buffer_io.b#;$vh=bless({$T2,$eh,$u4,$q,$v4,$q,$w4,$th,$Q,$uh},$T3);$wh=[$h6,$dh,$vh];$xh=bless({$T2,$Ug,$Q,$x1,$i3,$wh},$l3);$yh=q#ni:/io/buffer.c#;$zh={$l3,1};$Ah=q#/io/buffer.c#;$Bh={$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1};$Ch=q#/io/object.c#;$Dh={};$Eh=q#def_transfer_method#;$Fh=[];$Gh=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Hh=bless({$t,$Fh,$v,$q,$w,$Gh,$y,$z},$A);$Ih={$Eh,$Hh};$Jh=q#/io/object.c_transfer_def.b#;$Kh=bless({$T2,$Dh,$u4,$q,$v4,$q,$w4,$Ih,$Q,$Jh},$T3);$Lh=[$Bg,$Kh];$Mh=bless({$T2,$Bh,$Q,$Ch,$i3,$Lh},$h4);$Nh=[$Mh];$Oh=bless({$T2,$zh,$Q,$Ah,$i3,$Nh},$h4);$Ph=q#ni:/io/buffer_init.b#;$Qh=q#ni:/io/buffer_io.b#;$Rh=q#ni:/io/cat#;$Sh={$V2,1};$Th={};$Uh=[];$Vh=q#shift; +{fs => [@_]}#;$Wh=bless({$t,$Uh,$v,$q,$w,$Vh,$y,$z},$A);$Xh={$J6,$Wh};$Yh=q#/io/cat_init.b#;$Zh=bless({$T2,$Th,$u4,$q,$v4,$q,$w4,$Xh,$Q,$Yh},$T3);$ci={};$di=[];$ei=q#my $fs = shift->{fs};
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
$total_read;#;$fi=bless({$t,$di,$v,$q,$w,$ei,$y,$z},$A);$gi={$f7,$fi};$hi=q#/io/cat_read.b#;$ii=bless({$T2,$ci,$u4,$q,$v4,$q,$w4,$gi,$Q,$hi},$T3);$ji=[$h6,$Zh,$ii];$ki=bless({$T2,$Sh,$Q,$K1,$i3,$ji},$m3);$li=q#ni:/io/cat.c#;$mi={$m3,1};$ni=q#/io/cat.c#;$oi=[$Mh];$pi=bless({$T2,$mi,$Q,$ni,$i3,$oi},$h4);$qi=q#ni:/io/cat_init.b#;$ri=q#ni:/io/cat_read.b#;$si=q#ni:/io/exec#;$ti={$W2,1};$ui={};$vi=q#argv#;$wi=[];$xi=q#shift->{'argv'}#;$yi=bless({$t,$wi,$v,$q,$w,$xi,$y,$z},$A);$zi={$vi,$yi};$Ai=q#/io/exec_ro.b#;$Bi=bless({$T2,$ui,$u4,$q,$v4,$q,$w4,$zi,$Q,$Ai},$T3);$Ci={};$Di=[];$Ei=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Fi=bless({$t,$Di,$v,$q,$w,$Ei,$y,$z},$A);$Gi={$J6,$Fi};$Hi=q#/io/exec_init.b#;$Ii=bless({$T2,$Ci,$u4,$q,$v4,$q,$w4,$Gi,$Q,$Hi},$T3);$Ji={};$Ki=q#connect#;$Li=[];$Mi=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Ni=bless({$t,$Li,$v,$q,$w,$Mi,$y,$z},$A);$Oi=q#in_pipe#;$Pi=[];$Qi=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Ri=bless({$t,$Pi,$v,$q,$w,$Qi,$y,$z},$A);$Si=q#out_pipe#;$Ti=[];$Ui=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Vi=bless({$t,$Ti,$v,$q,$w,$Ui,$y,$z},$A);$Wi=q#setup_stdio#;$Xi=[];$Yi=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Zi=bless({$t,$Xi,$v,$q,$w,$Yi,$y,$z},$A);$cj={$Ki,$Ni,$Oi,$Ri,$Si,$Vi,$Wi,$Zi};$dj=q#/io/exec_io_setup.b#;$ej=bless({$T2,$Ji,$u4,$q,$v4,$q,$w4,$cj,$Q,$dj},$T3);$fj={};$gj=q#binds_fd#;$hj=[];$ij=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$jj=bless({$t,$hj,$v,$q,$w,$ij,$y,$z},$A);$kj=[];$lj=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$mj=bless({$t,$kj,$v,$q,$w,$lj,$y,$z},$A);$nj=[];$oj=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$pj=bless({$t,$nj,$v,$q,$w,$oj,$y,$z},$A);$qj=[];$rj=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$sj=bless({$t,$qj,$v,$q,$w,$rj,$y,$z},$A);$tj=[];$uj=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$vj=bless({$t,$tj,$v,$q,$w,$uj,$y,$z},$A);$wj={$gj,$jj,$r7,$mj,$v7,$pj,$z7,$sj,$D7,$vj};$xj=q#/io/exec_io_accessors.b#;$yj=bless({$T2,$fj,$u4,$q,$v4,$q,$w4,$wj,$Q,$xj},$T3);$zj={};$Aj=q#env#;$Bj=[];$Cj=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Dj=bless({$t,$Bj,$v,$q,$w,$Cj,$y,$z},$A);$Ej={$Aj,$Dj};$Fj=q#/io/exec_env.b#;$Gj=bless({$T2,$zj,$u4,$q,$v4,$q,$w4,$Ej,$Q,$Fj},$T3);$Hj={};$Ij=q#exec#;$Jj=[];$Kj=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Lj=bless({$t,$Jj,$v,$q,$w,$Kj,$y,$z},$A);$Mj=q#fork#;$Nj=[];$Oj=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Pj=bless({$t,$Nj,$v,$q,$w,$Oj,$y,$z},$A);$Qj=q#move_fds#;$Rj=[];$Sj=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Tj=bless({$t,$Rj,$v,$q,$w,$Sj,$y,$z},$A);$Uj={$Ij,$Lj,$Mj,$Pj,$Qj,$Tj};$Vj=q#/io/exec_fork.b#;$Wj=bless({$T2,$Hj,$u4,$q,$v4,$q,$w4,$Uj,$Q,$Vj},$T3);$Xj=[$h6,$Bi,$Ii,$ej,$yj,$Gj,$Wj];$Yj=bless({$T2,$ti,$Q,$X1,$i3,$Xj},$n3);$Zj=q#ni:/io/exec.c#;$ck={$n3,1};$dk=q#/io/exec.c#;$ek=[$Mh];$fk=bless({$T2,$ck,$Q,$dk,$i3,$ek},$h4);$gk=q#ni:/io/exec_env.b#;$hk=q#ni:/io/exec_fork.b#;$ik=q#ni:/io/exec_init.b#;$jk=q#ni:/io/exec_io_accessors.b#;$kk=q#ni:/io/exec_io_setup.b#;$lk=q#ni:/io/exec_ro.b#;$mk=q#ni:/io/fd#;$nk={$X2,1};$ok=q#read_fd_mask#;$pk={};$qk=[];$rk=q#shift->{'fd'}#;$sk=bless({$t,$qk,$v,$q,$w,$rk,$y,$z},$A);$tk={$r7,$sk};$uk=q#/io/fd_readers.b#;$vk=bless({$T2,$pk,$u4,$q,$v4,$q,$w4,$tk,$Q,$uk},$T3);$wk={};$xk=[];$yk=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$zk=bless({$t,$xk,$v,$q,$w,$yk,$y,$z},$A);$Ak={$J6,$zk};$Bk=q#/io/fd_init.b#;$Ck=bless({$T2,$wk,$u4,$q,$v4,$q,$w4,$Ak,$Q,$Bk},$T3);$Dk={};$Ek=q#be#;$Fk=[];$Gk=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Hk=bless({$t,$Fk,$v,$q,$w,$Gk,$y,$z},$A);$Ik={$Ek,$Hk};$Jk=q#/io/fd_shell.b#;$Kk=bless({$T2,$Dk,$u4,$q,$v4,$q,$w4,$Ik,$Q,$Jk},$T3);$Lk={};$Mk=q#cloexec#;$Nk=[];$Ok=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Pk=bless({$t,$Nk,$v,$q,$w,$Ok,$y,$z},$A);$Qk=q#fcntl_flag#;$Rk=[];$Sk=q#my ($self, $flag, $value) = @_;
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
}#;$Tk=bless({$t,$Rk,$v,$q,$w,$Sk,$y,$z},$A);$Uk=q#nonblock#;$Vk=[];$Wk=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Xk=bless({$t,$Vk,$v,$q,$w,$Wk,$y,$z},$A);$Yk={$Mk,$Pk,$Qk,$Tk,$Uk,$Xk};$Zk=q#/io/fd_fcntl.b#;$cl=bless({$T2,$Lk,$u4,$q,$v4,$q,$w4,$Yk,$Q,$Zk},$T3);$dl={};$el=[];$fl=q#shift->close#;$gl=bless({$t,$el,$v,$q,$w,$fl,$y,$z},$A);$hl=q#close#;$il=[];$jl=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$kl=bless({$t,$il,$v,$q,$w,$jl,$y,$z},$A);$ll={$hl,$kl};$ml=q#/io/fd_gc.b#;$nl=bless({$T2,$dl,$u4,$q,$v4,$gl,$w4,$ll,$Q,$ml},$T3);$ol={};$pl=q#close_perl_ios#;$ql=[];$rl=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$sl=bless({$t,$ql,$v,$q,$w,$rl,$y,$z},$A);$tl=[];$ul=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$vl=bless({$t,$tl,$v,$q,$w,$ul,$y,$z},$A);$wl=[];$xl=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$yl=bless({$t,$wl,$v,$q,$w,$xl,$y,$z},$A);$zl={$pl,$sl,$f7,$vl,$j7,$yl};$Al=q#/io/fd_perlio.b#;$Bl=bless({$T2,$ol,$u4,$q,$v4,$q,$w4,$zl,$Q,$Al},$T3);$Cl=[$h6,$vk,$Ck,$Kk,$cl,$nl,$Bl];$Dl=q#write_fd_mask#;$El=bless({$T2,$nk,$Q,$m2,$ok,$z,$i3,$Cl,$Dl,$z},$o3);$Fl=[];$Gl=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Hl=bless({$t,$Fl,$v,$q,$w,$Gl,$y,$z},$A);$Il=q#ni:/io/fd.c#;$Jl={$o3,1};$Kl=q#/io/fd.c#;$Ll={};$Ml=q#clear_fd#;$Nl=[];$Ol=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Pl=bless({$t,$Nl,$v,$q,$w,$Ol,$y,$z},$A);$Ql=q#read_fd#;$Rl=[];$Sl=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Tl=bless({$t,$Rl,$v,$q,$w,$Sl,$y,$z},$A);$Ul=q#select#;$Vl=[];$Wl=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Xl=bless({$t,$Vl,$v,$q,$w,$Wl,$y,$z},$A);$Yl=q#write_fd#;$Zl=[];$cm=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$dm=bless({$t,$Zl,$v,$q,$w,$cm,$y,$z},$A);$em={$Ml,$Pl,$Ql,$Tl,$Ul,$Xl,$Yl,$dm};$fm=q#/io/fd.c_selector.b#;$gm=bless({$T2,$Ll,$u4,$Hl,$v4,$q,$w4,$em,$Q,$fm},$T3);$hm=[$Mh,$gm];$im=bless({$T2,$Jl,$Q,$Kl,$i3,$hm},$h4);$jm=q#ni:/io/fd.c_selector.b#;$km=q#ni:/io/fd_fcntl.b#;$lm=q#ni:/io/fd_gc.b#;$mm=q#ni:/io/fd_init.b#;$nm=q#ni:/io/fd_perlio.b#;$om=q#ni:/io/fd_readers.b#;$pm=q#ni:/io/fd_shell.b#;$qm=q#ni:/io/file#;$rm={$Y2,1};$sm={};$tm=[];$um=q#shift->{'name'}#;$vm=bless({$t,$tm,$v,$q,$w,$um,$y,$z},$A);$wm={$Q,$vm};$xm=q#/io/file_readers.b#;$ym=bless({$T2,$sm,$u4,$q,$v4,$q,$w4,$wm,$Q,$xm},$T3);$zm={};$Am=q#mode#;$Bm=[];$Cm=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$Dm=bless({$t,$Bm,$v,$q,$w,$Cm,$y,$z},$A);$Em={$Am,$Dm};$Fm=q#/io/file_accessors.b#;$Gm=bless({$T2,$zm,$u4,$q,$v4,$q,$w4,$Em,$Q,$Fm},$T3);$Hm={};$Im=[];$Jm=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Km=bless({$t,$Im,$v,$q,$w,$Jm,$y,$z},$A);$Lm={$J6,$Km};$Mm=q#/io/file_init.b#;$Nm=bless({$T2,$Hm,$u4,$q,$v4,$q,$w4,$Lm,$Q,$Mm},$T3);$Om={};$Pm=q#(-X#;$Qm=[];$Rm=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Sm=bless({$t,$Qm,$v,$q,$w,$Rm,$y,$z},$A);$Tm=q#mv#;$Um=[];$Vm=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Wm=bless({$t,$Um,$v,$q,$w,$Vm,$y,$z},$A);$Xm=q#rm#;$Ym=[];$Zm=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$cn=bless({$t,$Ym,$v,$q,$w,$Zm,$y,$z},$A);$dn={$Pm,$Sm,$Tm,$Wm,$Xm,$cn};$en=q#/io/file_fns.b#;$fn=bless({$T2,$Om,$u4,$q,$v4,$q,$w4,$dn,$Q,$en},$T3);$gn={};$hn=q#atomic_update#;$in=[];$jn=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$kn=bless({$t,$in,$v,$q,$w,$jn,$y,$z},$A);$ln={$hn,$kn};$mn=q#/io/file_update.b#;$nn=bless({$T2,$gn,$u4,$q,$v4,$q,$w4,$ln,$Q,$mn},$T3);$on={};$pn=[];$qn=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$rn=bless({$t,$pn,$v,$q,$w,$qn,$y,$z},$A);$sn=q#r#;$tn=[];$un=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$vn=bless({$t,$tn,$v,$q,$w,$un,$y,$z},$A);$wn=[];$xn=q#shift->r->read(@_)#;$yn=bless({$t,$wn,$v,$q,$w,$xn,$y,$z},$A);$zn=q#w#;$An=[];$Bn=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Cn=bless({$t,$An,$v,$q,$w,$Bn,$y,$z},$A);$Dn=[];$En=q#shift->w->write(@_)#;$Fn=bless({$t,$Dn,$v,$q,$w,$En,$y,$z},$A);$Gn={$hl,$rn,$sn,$vn,$f7,$yn,$zn,$Cn,$j7,$Fn};$Hn=q#/io/file_io.b#;$In=bless({$T2,$on,$u4,$q,$v4,$q,$w4,$Gn,$Q,$Hn},$T3);$Jn=[$h6,$ym,$Gm,$Nm,$fn,$nn,$In];$Kn=bless({$T2,$rm,$Q,$I2,$i3,$Jn},$p3);$Ln=q#ni:/io/file.c#;$Mn={$p3,1};$Nn=q#/io/file.c#;$On=[$Mh];$Pn=bless({$T2,$Mn,$Q,$Nn,$i3,$On},$h4);$Qn=q#ni:/io/file_accessors.b#;$Rn=q#ni:/io/file_fns.b#;$Sn=q#ni:/io/file_init.b#;$Tn=q#ni:/io/file_io.b#;$Un=q#ni:/io/file_readers.b#;$Vn=q#ni:/io/file_update.b#;$Wn=q#ni:/io/file_update_fd#;$Xn={$Z2,1};$Yn={};$Zn=[];$co=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$do=bless({$t,$Zn,$v,$q,$w,$co,$y,$z},$A);$eo={$J6,$do};$fo=q#/io/file_update_fd_fd_init.b#;$go=bless({$T2,$Yn,$u4,$q,$v4,$q,$w4,$eo,$Q,$fo},$T3);$ho={};$io=[];$jo=bless({$t,$io,$v,$q,$w,$fl,$y,$z},$A);$ko=[];$lo=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$mo=bless({$t,$ko,$v,$q,$w,$lo,$y,$z},$A);$no={$hl,$mo};$oo=q#/io/file_update_fd_fd_gc.b#;$po=bless({$T2,$ho,$u4,$q,$v4,$jo,$w4,$no,$Q,$oo},$T3);$qo=[$h6,$vk,$cl,$Bl,$go,$po];$ro=bless({$T2,$Xn,$Q,$O2,$i3,$qo},$q3);$so=q#ni:/io/file_update_fd.c#;$to={$q3,1};$uo=q#/io/file_update_fd.c#;$vo=[$Mh];$wo=bless({$T2,$to,$Q,$uo,$i3,$vo},$h4);$xo=q#ni:/io/file_update_fd_fd_gc.b#;$yo=q#ni:/io/file_update_fd_fd_init.b#;$zo=q#ni:/io/named_io_fns.b#;$Ao={};$Bo=q#fcntl#;$Co=[];$Do=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Eo=bless({$t,$Co,$v,$q,$w,$Do,$y,$z},$A);$Fo=[];$Go=q#CORE::fork#;$Ho=bless({$t,$Fo,$v,$q,$w,$Go,$y,$z},$A);$Io=q#open2#;$Jo=[];$Ko=q#CORE::open $_[0], $_[1]#;$Lo=bless({$t,$Jo,$v,$q,$w,$Ko,$y,$z},$A);$Mo=q#rename#;$No=[];$Oo=q#CORE::rename $_[0], $_[1]#;$Po=bless({$t,$No,$v,$q,$w,$Oo,$y,$z},$A);$Qo=q#unlink#;$Ro=[];$So=q#CORE::unlink @_#;$To=bless({$t,$Ro,$v,$q,$w,$So,$y,$z},$A);$Uo=q#waitpid#;$Vo=[];$Wo=q#CORE::waitpid $_[0], $_[1]#;$Xo=bless({$t,$Vo,$v,$q,$w,$Wo,$y,$z},$A);$Yo={$Bo,$Eo,$Mj,$Ho,$Io,$Lo,$Mo,$Po,$Qo,$To,$Uo,$Xo};$Zo=q#/io/named_io_fns.b#;$cp=bless({$T2,$Ao,$u4,$q,$v4,$q,$w4,$Yo,$Q,$Zo},$T3);$dp=q#main#;$ep=q#ni:/io/null#;$fp={$c3,1};$gp=q#/io/null#;$hp={};$ip=[];$jp=q#+{fd => undef}#;$kp=bless({$t,$ip,$v,$q,$w,$jp,$y,$z},$A);$lp={$J6,$kp};$mp=q#/io/null_init.b#;$np=bless({$T2,$hp,$u4,$q,$v4,$q,$w4,$lp,$Q,$mp},$T3);$op={};$pp=[];$qp=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$rp=bless({$t,$pp,$v,$q,$w,$qp,$y,$z},$A);$sp=[];$tp=q#shift->fd->read(@_)#;$up=bless({$t,$sp,$v,$q,$w,$tp,$y,$z},$A);$vp=[];$wp=q#shift->fd->write(@_)#;$xp=bless({$t,$vp,$v,$q,$w,$wp,$y,$z},$A);$yp={$r7,$rp,$f7,$up,$j7,$xp};$zp=q#/io/null_io.b#;$Ap=bless({$T2,$op,$u4,$q,$v4,$q,$w4,$yp,$Q,$zp},$T3);$Bp=[$h6,$np,$Ap];$Cp=bless({$T2,$fp,$Q,$gp,$i3,$Bp},$r3);$Dp=q#ni:/io/null.c#;$Ep={$r3,1};$Fp=q#/io/null.c#;$Gp=[$Mh];$Hp=bless({$T2,$Ep,$Q,$Fp,$i3,$Gp},$h4);$Ip=q#ni:/io/null_init.b#;$Jp=q#ni:/io/null_io.b#;$Kp=q#ni:/io/object#;$Lp=q#ni:/io/object.c#;$Mp=q#ni:/io/object.c_transfer_def.b#;$Np=q#ni:/io/object_checks.b#;$Op=q#ni:/io/object_constructors.b#;$Pp=q#ni:/io/object_memory.b#;$Qp=q#ni:/io/object_ops.b#;$Rp=q#ni:/io/object_transfer_async.b#;$Sp=q#ni:/io/object_transfer_sync.b#;$Tp=q#ni:/io/pid#;$Up=q#ni:/io/pid.c#;$Vp={$t3,1};$Wp=q#/io/pid.c#;$Xp=[$Mh];$Yp=bless({$T2,$Vp,$Q,$Wp,$i3,$Xp},$h4);$Zp=q#ni:/io/pid_accessors.b#;$cq=q#ni:/io/pid_init.b#;$dq=q#ni:/io/pid_io.b#;$eq=q#ni:/io/pid_readers.b#;$fq=q#ni:/io/pid_wait.b#;$gq=q#ni:/io/str#;$hq={$f3,1};$iq=q#/io/str#;$jq={};$kq=q#data#;$lq=[];$mq=q#shift->{'data'}#;$nq=bless({$t,$lq,$v,$q,$w,$mq,$y,$z},$A);$oq=q#end#;$pq=[];$qq=q#shift->{'end'}#;$rq=bless({$t,$pq,$v,$q,$w,$qq,$y,$z},$A);$sq=q#start#;$tq=[];$uq=q#shift->{'start'}#;$vq=bless({$t,$tq,$v,$q,$w,$uq,$y,$z},$A);$wq={$kq,$nq,$oq,$rq,$sq,$vq};$xq=q#/io/str_ro.b#;$yq=bless({$T2,$jq,$u4,$q,$v4,$q,$w4,$wq,$Q,$xq},$T3);$zq={};$Aq=[];$Bq=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Cq=bless({$t,$Aq,$v,$q,$w,$Bq,$y,$z},$A);$Dq={$J6,$Cq};$Eq=q#/io/str_init.b#;$Fq=bless({$T2,$zq,$u4,$q,$v4,$q,$w4,$Dq,$Q,$Eq},$T3);$Gq={};$Hq=[];$Iq=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Jq=bless({$t,$Hq,$v,$q,$w,$Iq,$y,$z},$A);$Kq=q#remaining#;$Lq=[];$Mq=q#my $self = shift; $$self{end} - $$self{start}#;$Nq=bless({$t,$Lq,$v,$q,$w,$Mq,$y,$z},$A);$Oq=[];$Pq=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Qq=bless({$t,$Oq,$v,$q,$w,$Pq,$y,$z},$A);$Rq={$f7,$Jq,$Kq,$Nq,$j7,$Qq};$Sq=q#/io/str_io.b#;$Tq=bless({$T2,$Gq,$u4,$q,$v4,$q,$w4,$Rq,$Q,$Sq},$T3);$Uq=[$h6,$yq,$Fq,$Tq];$Vq=bless({$T2,$hq,$Q,$iq,$i3,$Uq},$u3);$Wq=q#ni:/io/str.c#;$Xq={$u3,1};$Yq=q#/io/str.c#;$Zq=[$Mh];$cr=bless({$T2,$Xq,$Q,$Yq,$i3,$Zq},$h4);$dr=q#ni:/io/str_init.b#;$er=q#ni:/io/str_io.b#;$fr=q#ni:/io/str_ro.b#;$gr=q#ni:/io/transfer#;$hr={$v3,1,$x3,1,$z3,1};$ir=q#/io/transfer#;$jr={$v3,1,$x3,1,$z3,1,$p4,1};$kr=q#/semantic/task#;$lr={};$mr=[];$nr=q#shift->{'outcome'}#;$or=bless({$t,$mr,$v,$q,$w,$nr,$y,$z},$A);$pr={$r,$or};$qr=q#/semantic/task_ro.b#;$rr=bless({$T2,$lr,$u4,$q,$v4,$q,$w4,$pr,$Q,$qr},$T3);$sr={};$tr=q#failure#;$ur=[];$vr=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$wr=bless({$t,$ur,$v,$q,$w,$vr,$y,$z},$A);$xr=q#success#;$yr=[];$zr=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Ar=bless({$t,$yr,$v,$q,$w,$zr,$y,$z},$A);$Br={$tr,$wr,$xr,$Ar};$Cr=q#/semantic/task_outcome.b#;$Dr=bless({$T2,$sr,$u4,$q,$v4,$q,$w4,$Br,$Q,$Cr},$T3);$Er=[$G4,$rr,$Dr];$Fr=bless({$T2,$jr,$Q,$kr,$i3,$Er},$q4);$Gr={};$Hr=[];$Ir=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Jr=bless({$t,$Hr,$v,$q,$w,$Ir,$y,$z},$A);$Kr=[];$Lr=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Mr=bless({$t,$Kr,$v,$q,$w,$Lr,$y,$z},$A);$Nr=[];$Or=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Pr=bless({$t,$Nr,$v,$q,$w,$Or,$y,$z},$A);$Qr={$f7,$Mr,$j7,$Pr};$Rr=q#/io/transfer_io_interop.b#;$Sr=bless({$T2,$Gr,$u4,$Jr,$v4,$q,$w4,$Qr,$Q,$Rr},$T3);$Tr={};$Ur=q#pressure#;$Vr=[];$Wr=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Xr=bless({$t,$Vr,$v,$q,$w,$Wr,$y,$z},$A);$Yr=q#read_limit_throughput#;$Zr=[];$cs=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$ds=bless({$t,$Zr,$v,$q,$w,$cs,$y,$z},$A);$es=q#throughput#;$fs=[];$gs=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$hs=bless({$t,$fs,$v,$q,$w,$gs,$y,$z},$A);$is=q#write_limit_throughput#;$js=[];$ks=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$ls=bless({$t,$js,$v,$q,$w,$ks,$y,$z},$A);$ms={$Ur,$Xr,$Yr,$ds,$es,$hs,$is,$ls};$ns=q#/io/transfer_io_measurement.b#;$os=bless({$T2,$Tr,$u4,$q,$v4,$q,$w4,$ms,$Q,$ns},$T3);$ps=[$Fr,$Sr,$os];$qs=bless({$T2,$hr,$Q,$ir,$i3,$ps},$w3);$rs=[];$ss=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$ts=bless({$t,$rs,$v,$q,$w,$ss,$y,$z},$A);$us=q#ni:/io/transfer.c#;$vs={$w3,1,$y3,1,$A3,1};$ws=q#/io/transfer.c#;$xs={$w3,1,$y3,1,$A3,1,$q4,1};$ys=q#/semantic/task.c#;$zs=[$Bg];$As=bless({$T2,$xs,$Q,$ys,$i3,$zs},$h4);$Bs={};$Cs={};$Ds=q#/io/transfer.c_into.b#;$Es=bless({$T2,$Bs,$u4,$ts,$v4,$q,$w4,$Cs,$Q,$Ds},$T3);$Fs=[$As,$Es];$Gs=bless({$T2,$vs,$Q,$ws,$i3,$Fs},$h4);$Hs=q#ni:/io/transfer.c_into.b#;$Is=q#ni:/io/transfer_async#;$Js={$x3,1};$Ks=q#/io/transfer_async#;$Ls={};$Ms=q#dest_io#;$Ns=[];$Os=q#shift->{'dest_io'}#;$Ps=bless({$t,$Ns,$v,$q,$w,$Os,$y,$z},$A);$Qs=q#id#;$Rs=[];$Ss=q#shift->{'id'}#;$Ts=bless({$t,$Rs,$v,$q,$w,$Ss,$y,$z},$A);$Us=q#source_io#;$Vs=[];$Ws=q#shift->{'source_io'}#;$Xs=bless({$t,$Vs,$v,$q,$w,$Ws,$y,$z},$A);$Ys={$Ms,$Ps,$Qs,$Ts,$Us,$Xs};$Zs=q#/io/transfer_async_ro.b#;$ct=bless({$T2,$Ls,$u4,$q,$v4,$q,$w4,$Ys,$Q,$Zs},$T3);$dt={};$et=[];$ft=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$gt=bless({$t,$et,$v,$q,$w,$ft,$y,$z},$A);$ht={$J6,$gt};$it=q#/io/transfer_async_init.b#;$jt=bless({$T2,$dt,$u4,$q,$v4,$q,$w4,$ht,$Q,$it},$T3);$kt={};$lt=[];$mt=q#ni('ni:/io/transfer_async')->track(shift)#;$nt=bless({$t,$lt,$v,$q,$w,$mt,$y,$z},$A);$ot=[];$pt=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$qt=bless({$t,$ot,$v,$q,$w,$pt,$y,$z},$A);$rt={};$st=q#/io/transfer_async_lifecycle.b#;$tt=bless({$T2,$kt,$u4,$nt,$v4,$qt,$w4,$rt,$Q,$st},$T3);$ut={};$vt=q#run#;$wt=[];$xt=q#shift#;$yt=bless({$t,$wt,$v,$q,$w,$xt,$y,$z},$A);$zt=q#run_async#;$At=[];$Bt=q#my $self = shift;
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

$self;#;$Ct=bless({$t,$At,$v,$q,$w,$Bt,$y,$z},$A);$Dt={$vt,$yt,$zt,$Ct};$Et=q#/io/transfer_async_run.b#;$Ft=bless({$T2,$ut,$u4,$q,$v4,$q,$w4,$Dt,$Q,$Et},$T3);$Gt=[$qs,$ct,$jt,$tt,$Ft];$Ht=q#tracked_transfers#;$It={};$Jt=q#transfer_id#;$Kt=bless({$T2,$Js,$Q,$Ks,$i3,$Gt,$Ht,$It,$Jt,0},$y3);$Lt=[];$Mt=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Nt=bless({$t,$Lt,$v,$q,$w,$Mt,$y,$z},$A);$Ot=q#ni:/io/transfer_async.c#;$Pt={$y3,1};$Qt=q#/io/transfer_async.c#;$Rt={};$St=q#new_id#;$Tt=[];$Ut=q#++shift->{transfer_id}#;$Vt=bless({$t,$Tt,$v,$q,$w,$Ut,$y,$z},$A);$Wt=q#track#;$Xt=[];$Yt=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Zt=bless({$t,$Xt,$v,$q,$w,$Yt,$y,$z},$A);$cu=q#untrack#;$du=[];$eu=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$fu=bless({$t,$du,$v,$q,$w,$eu,$y,$z},$A);$gu={$St,$Vt,$Wt,$Zt,$cu,$fu};$hu=q#/io/transfer_async.c_tracker.b#;$iu=bless({$T2,$Rt,$u4,$Nt,$v4,$q,$w4,$gu,$Q,$hu},$T3);$ju=[$Gs,$iu];$ku=bless({$T2,$Pt,$Q,$Qt,$i3,$ju},$h4);$lu=q#ni:/io/transfer_async.c_tracker.b#;$mu=q#ni:/io/transfer_async_init.b#;$nu=q#ni:/io/transfer_async_lifecycle.b#;$ou=q#ni:/io/transfer_async_ro.b#;$pu=q#ni:/io/transfer_async_run.b#;$qu=q#ni:/io/transfer_io_interop.b#;$ru=q#ni:/io/transfer_io_measurement.b#;$su=q#ni:/io/transfer_sync#;$tu={$z3,1};$uu=q#/io/transfer_sync#;$vu={};$wu=[];$xu=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$yu=bless({$t,$wu,$v,$q,$w,$xu,$y,$z},$A);$zu={$J6,$yu};$Au=q#/io/transfer_sync_init.b#;$Bu=bless({$T2,$vu,$u4,$q,$v4,$q,$w4,$zu,$Q,$Au},$T3);$Cu={};$Du=[];$Eu=q#my $self = shift;
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
$self->success;#;$Fu=bless({$t,$Du,$v,$q,$w,$Eu,$y,$z},$A);$Gu={$vt,$Fu};$Hu=q#/io/transfer_sync_run.b#;$Iu=bless({$T2,$Cu,$u4,$q,$v4,$q,$w4,$Gu,$Q,$Hu},$T3);$Ju=[$qs,$Bu,$Iu];$Ku=bless({$T2,$tu,$Q,$uu,$i3,$Ju},$A3);$Lu=q#ni:/io/transfer_sync.c#;$Mu={$A3,1};$Nu=q#/io/transfer_sync.c#;$Ou=[$Gs];$Pu=bless({$T2,$Mu,$Q,$Nu,$i3,$Ou},$h4);$Qu=q#ni:/io/transfer_sync_init.b#;$Ru=q#ni:/io/transfer_sync_run.b#;$Su=q#ni:/lib#;$Tu=q#lib#;$Uu={$Tu,1};$Vu=[];$Wu=bless({$T2,$Uu,$Q,$m8,$i3,$Vu},$j4);$Xu=q#ni:/lib/accessor.b#;$Yu=q#ni:/lib/behavior#;$Zu=q#ni:/lib/behavior.c#;$cv=q#ni:/lib/branch#;$dv={$D3,1};$ev=q#/lib/branch#;$fv={};$gv=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$hv=bless({$w,$gv,$y,$z},$A);$iv={$J6,$hv};$jv=q#/lib/branch_init.b#;$kv=bless({$T2,$fv,$u4,$q,$v4,$q,$w4,$iv,$Q,$jv},$T3);$lv=[$A8,$G8,$Re,$kv,$Yf];$mv=bless({$T2,$dv,$Q,$ev,$i3,$lv},$E3);$nv=q#ni:/lib/branch.b#;$ov=q#ni:/lib/branch.c#;$pv={$E3,1};$qv=q#/lib/branch.c#;$rv=[$Fg];$sv=bless({$T2,$pv,$Q,$qv,$i3,$rv},$h4);$tv=q#ni:/lib/branch_init.b#;$uv=q#ni:/lib/class_init.b#;$vv=q#ni:/lib/dataslice#;$wv=q#ni:/lib/dataslice.b#;$xv=q#ni:/lib/dataslice.c#;$yv={$G3,1};$zv=q#/lib/dataslice.c#;$Av=[$Fg];$Bv=bless({$T2,$yv,$Q,$zv,$i3,$Av},$h4);$Cv=q#ni:/lib/dataslice_init.b#;$Dv=q#ni:/lib/definition.b#;$Ev=q#ni:/lib/definition_def.b#;$Fv=q#ni:/lib/definition_defdata.b#;$Gv=q#ni:/lib/definition_init_with_defaults.b#;$Hv=q#ni:/lib/doc#;$Iv={$S,1};$Jv={};$Kv=q#shift; +{name => shift, doc => []}#;$Lv=bless({$w,$Kv,$y,$z},$A);$Mv={$J6,$Lv};$Nv=q#/lib/doc_init.b#;$Ov=bless({$T2,$Jv,$u4,$q,$v4,$q,$w4,$Mv,$Q,$Nv},$T3);$Pv={};$Qv=q#'ni.doc'#;$Rv=bless({$w,$Qv,$y,$z},$A);$Sv={$Y8,$Rv};$Tv=q#/lib/doc_namespace.b#;$Uv=bless({$T2,$Pv,$u4,$q,$v4,$q,$w4,$Sv,$Q,$Tv},$T3);$Vv={};$Wv=q#(@{}#;$Xv=q#[map @$_, @{shift->{doc}}]#;$Yv=bless({$w,$Xv,$y,$z},$A);$Zv=q#AUTOLOAD#;$cw=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$dw=bless({$w,$cw,$y,$z},$A);$ew={$Wv,$Yv,$Zv,$dw};$fw=q#/lib/doc_define.b#;$gw=bless({$T2,$Vv,$u4,$q,$v4,$q,$w4,$ew,$Q,$fw},$T3);$hw={};$iw=q#shift->referent#;$jw=bless({$w,$iw,$y,$z},$A);$kw=q#ni 'ni:' . shift->{name}#;$lw=bless({$w,$kw,$y,$z},$A);$mw={$oq,$jw,$S2,$lw};$nw=q#/lib/doc_end.b#;$ow=bless({$T2,$hw,$u4,$q,$v4,$q,$w4,$mw,$Q,$nw},$T3);$pw={};$qw=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$rw=bless({$w,$qw,$y,$z},$A);$sw={$R2,$rw};$tw=q#/lib/doc_TODO.b#;$uw=bless({$T2,$pw,$u4,$q,$v4,$q,$w4,$sw,$Q,$tw},$T3);$vw={};$ww=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$xw=bless({$w,$ww,$y,$z},$A);$yw={$p6,$xw};$zw=q#/lib/doc_eg.b#;$Aw=bless({$T2,$vw,$u4,$q,$v4,$q,$w4,$yw,$Q,$zw},$T3);$Bw={};$Cw=q#tests#;$Dw=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case', @$self;#;$Ew=bless({$w,$Dw,$y,$z},$A);$Fw=q#todos#;$Gw=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo', @$self;#;$Hw=bless({$w,$Gw,$y,$z},$A);$Iw={$Cw,$Ew,$Fw,$Hw};$Jw=q#/lib/doc_process.b#;$Kw=bless({$T2,$Bw,$u4,$q,$v4,$q,$w4,$Iw,$Q,$Jw},$T3);$Lw=[$G4,$G8,$Ov,$Uv,$gw,$ow,$uw,$Aw,$Kw];$Mw=bless({$T2,$Iv,$Q,$ea,$i3,$Lw},$H3);$Nw=q#ni:/lib/doc.c#;$Ow={$H3,1};$Pw=q#/lib/doc.c#;$Qw={};$Rw=q#defannotation#;$Sw=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Tw=bless({$w,$Sw,$y,$z},$A);$Uw={$Rw,$Tw};$Vw=q#/lib/doc.c_defannotation.b#;$Ww=bless({$T2,$Qw,$u4,$q,$v4,$q,$w4,$Uw,$Q,$Vw},$T3);$Xw=[$Bg,$Ww];$Yw=bless({$T2,$Ow,$Q,$Pw,$i3,$Xw},$h4);$Zw=q#ni:/lib/doc.c_defannotation.b#;$cx=q#ni:/lib/doc_TODO.b#;$dx=q#ni:/lib/doc_define.b#;$ex=q#ni:/lib/doc_eg.b#;$fx=q#ni:/lib/doc_end.b#;$gx=q#ni:/lib/doc_init.b#;$hx=q#ni:/lib/doc_namespace.b#;$ix=q#ni:/lib/doc_process.b#;$jx=q#ni:/lib/documentable.b#;$kx=q#ni:/lib/fn#;$lx={$A,1};$mx=q#/lib/fn#;$nx=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$ox=bless({$w,$nx,$y,$z},$A);$px=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$qx=bless({$w,$px},$A);$rx=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$sx=bless({$w,$rx},$A);$tx=q#lib/fn::closure#;$ux=q#lib/fn::compile#;$vx=q#lib/fn::instantiate#;$wx={};$xx=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$yx=bless({$w,$xx,$y,$z},$A);$zx=q#compile#;$Ax={$v,$ox,$zx,$qx,$J6,$sx};$Bx=q#/lib/fn_init.b#;$Cx=bless({$T2,$wx,$u4,$q,$v4,$yx,$w4,$Ax,$Q,$Bx},$T3);$Dx={};$Ex=[];$Fx=q#shift->{'annotations'}#;$Gx=bless({$t,$Ex,$v,$q,$w,$Fx,$y,$z},$A);$Hx=[];$Ix=q#shift->{'code'}#;$Jx=bless({$t,$Hx,$v,$q,$w,$Ix,$y,$z},$A);$Kx=q#eval_number#;$Lx=[];$Mx=q#shift->{'eval_number'}#;$Nx=bless({$t,$Lx,$v,$q,$w,$Mx,$y,$z},$A);$Ox=q#fn#;$Px=[];$Qx=q#shift->{'fn'}#;$Rx=bless({$t,$Px,$v,$q,$w,$Qx,$y,$z},$A);$Sx={$t,$Gx,$w,$Jx,$Kx,$Nx,$Ox,$Rx};$Tx=q#/lib/fn_ro.b#;$Ux=bless({$T2,$Dx,$u4,$q,$v4,$q,$w4,$Sx,$Q,$Tx},$T3);$Vx={};$Wx=[];$Xx=q#my $self = shift; "fn {$$self{code}}"#;$Yx=bless({$t,$Wx,$v,$q,$w,$Xx,$y,$z},$A);$Zx=[];$cy=bless({$t,$Zx,$v,$q,$w,$Ef,$y,$z},$A);$dy={$wf,$Yx,$Df,$cy};$ey=q#/lib/fn_ops.b#;$fy=bless({$T2,$Vx,$u4,$q,$v4,$q,$w4,$dy,$Q,$ey},$T3);$gy={};$hy=[];$iy=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$jy=bless({$t,$hy,$v,$q,$w,$iy,$y,$z},$A);$ky={$v9,$jy};$ly=q#/lib/fn_serialize.b#;$my=bless({$T2,$gy,$u4,$q,$v4,$q,$w4,$ky,$Q,$ly},$T3);$ny=[$G4,$jg,$Cx,$Ux,$fy,$my];$oy=bless({$T2,$lx,$Q,$mx,$i3,$ny},$I3);$py=[];$qy=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$ry=bless({$t,$py,$v,$q,$w,$qy,$y,$z},$A);$sy=q#ni:/lib/fn.c#;$ty={$I3,1};$uy=q#/lib/fn.c#;$vy={};$wy=q#resolve_evals#;$xy=[];$yy=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$zy=bless({$t,$xy,$v,$q,$w,$yy,$y,$z},$A);$Ay={$wy,$zy};$By=q#/lib/fn.c_resolve_eval.b#;$Cy=bless({$T2,$vy,$u4,$ry,$v4,$q,$w4,$Ay,$Q,$By},$T3);$Dy=[$Bg,$Cy];$Ey=bless({$T2,$ty,$Q,$uy,$i3,$Dy},$h4);$Fy=q#ni:/lib/fn.c_resolve_eval.b#;$Gy=q#ni:/lib/fn_init.b#;$Hy=q#ni:/lib/fn_ops.b#;$Iy=q#ni:/lib/fn_ro.b#;$Jy=q#ni:/lib/fn_serialize.b#;$Ky=q#ni:/lib/future#;$Ly={$J3,1};$My={};$Ny=[];$Oy=bless({$t,$Ny,$v,$q,$w,$nr,$y,$z},$A);$Py=q#parents#;$Qy=[];$Ry=q#shift->{'parents'}#;$Sy=bless({$t,$Qy,$v,$q,$w,$Ry,$y,$z},$A);$Ty={$r,$Oy,$Py,$Sy};$Uy=q#/lib/future_ro.b#;$Vy=bless({$T2,$My,$u4,$q,$v4,$q,$w4,$Ty,$Q,$Uy},$T3);$Wy={};$Xy=[];$Yy=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Zy=bless({$t,$Xy,$v,$q,$w,$Yy,$y,$z},$A);$cz={$J6,$Zy};$dz=q#/lib/future_init.b#;$ez=bless({$T2,$Wy,$u4,$q,$v4,$q,$w4,$cz,$Q,$dz},$T3);$fz={};$gz=q#decide#;$hz=[];$iz=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$jz=bless({$t,$hz,$v,$q,$w,$iz,$y,$z},$A);$kz=q#decided#;$lz=[];$mz=q#shift->{outcome}#;$nz=bless({$t,$lz,$v,$q,$w,$mz,$y,$z},$A);$oz=q#listener#;$pz=[];$qz=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$rz=bless({$t,$pz,$v,$q,$w,$qz,$y,$z},$A);$sz=q#v#;$tz=[];$uz=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$vz=bless({$t,$tz,$v,$q,$w,$uz,$y,$z},$A);$wz={$gz,$jz,$kz,$nz,$oz,$rz,$sz,$vz};$xz=q#/lib/future_state.b#;$yz=bless({$T2,$fz,$u4,$q,$v4,$q,$w4,$wz,$Q,$xz},$T3);$zz={};$Az=q#and#;$Bz=[];$Cz=q#my $self   = $_[0];
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
$child;#;$Dz=bless({$t,$Bz,$v,$q,$w,$Cz,$y,$z},$A);$Ez=q#flatmap#;$Fz=[];$Gz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$Hz=bless({$t,$Fz,$v,$q,$w,$Gz,$y,$z},$A);$Iz=q#map#;$Jz=[];$Kz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$Lz=bless({$t,$Jz,$v,$q,$w,$Kz,$y,$z},$A);$Mz=q#or#;$Nz=[];$Oz=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Pz=bless({$t,$Nz,$v,$q,$w,$Oz,$y,$z},$A);$Qz={$Az,$Dz,$Ez,$Hz,$Iz,$Lz,$Mz,$Pz};$Rz=q#/lib/future_algebra.b#;$Sz=bless({$T2,$zz,$u4,$q,$v4,$q,$w4,$Qz,$Q,$Rz},$T3);$Tz=[$G4,$Vy,$ez,$yz,$Sz];$Uz=bless({$T2,$Ly,$Q,$va,$i3,$Tz},$K3);$Vz=q#ni:/lib/future.c#;$Wz={$K3,1};$Xz=q#/lib/future.c#;$Yz=[$Bg];$Zz=bless({$T2,$Wz,$Q,$Xz,$i3,$Yz},$h4);$cA=q#ni:/lib/future_algebra.b#;$dA=q#ni:/lib/future_init.b#;$eA=q#ni:/lib/future_ro.b#;$fA=q#ni:/lib/future_state.b#;$gA=q#ni:/lib/gensym_generator_compact.b#;$hA=q#ni:/lib/global_static_test.b#;$iA={};$jA=[];$kA=q#ni('ni:/lib/test_case')->new(shift)#;$lA=q#($)#;$mA=bless({$t,$jA,$v,$q,$w,$kA,$y,$lA},$A);$nA=q#now#;$oA=[];$pA=q#ni('ni:/lib/test_value')->new(shift)#;$qA=bless({$t,$oA,$v,$q,$w,$pA,$y,$lA},$A);$rA={$p6,$mA,$nA,$qA};$sA=q#/lib/global_static_test.b#;$tA=bless({$T2,$iA,$u4,$q,$v4,$q,$w4,$rA,$Q,$sA},$T3);$uA=q#ni:/lib/image#;$vA=q#ni:/lib/image.c#;$wA={$M3,1};$xA=q#/lib/image.c#;$yA=[$Bg];$zA=bless({$T2,$wA,$Q,$xA,$i3,$yA},$h4);$AA=q#ni:/lib/image_init.b#;$BA=q#ni:/lib/image_quoting.b#;$CA=q#ni:/lib/instance.b#;$DA=q#ni:/lib/instantiable.b#;$EA=q#ni:/lib/json.b#;$FA={};$GA=q#json_decode#;$HA=[];$IA=q#local $_;
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
wantarray ? @$r : $$r[0];#;$JA=bless({$t,$HA,$v,$q,$w,$IA,$y,$lA},$A);$KA=q#json_encode#;$LA=[];$MA=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$NA=bless({$t,$LA,$v,$q,$w,$MA,$y,$lA},$A);$OA=q#json_encode_pretty#;$PA=[];$QA=q#local $_;
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

ni::json_encode($v);#;$RA=bless({$t,$PA,$v,$q,$w,$QA,$y,$z},$A);$SA=q#json_escape#;$TA=[];$UA=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$VA=bless({$t,$TA,$v,$q,$w,$UA,$y,$lA},$A);$WA=q#json_unescape#;$XA=[];$YA=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$ZA=bless({$t,$XA,$v,$q,$w,$YA,$y,$lA},$A);$cB=q#json_unescape_one#;$dB=[];$eB=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$fB=bless({$t,$dB,$v,$q,$w,$eB,$y,$lA},$A);$gB={$GA,$JA,$KA,$NA,$OA,$RA,$SA,$VA,$WA,$ZA,$cB,$fB};$hB=q#/lib/json.b#;$iB=bless({$T2,$FA,$u4,$q,$v4,$q,$w4,$gB,$Q,$hB},$T3);$jB=q#ni#;$kB=q#ni:/lib/json_data.b#;$lB={};$mB=q#json_escapes#;$nB=q##;$oB=q#b#;$pB=q#	#;$qB=q#t#;$rB=q#
#;$sB=q#n#;$tB=q##;$uB=q#f#;$vB=q##;$wB=q#"#;$xB=q#/#;$yB=q#\\#;$zB={$nB,$oB,$pB,$qB,$rB,$sB,$tB,$uB,$vB,$sn,$wB,$wB,$xB,$xB,$yB,$yB};$AB=q#json_unescapes#;$BB={$wB,$wB,$xB,$xB,$yB,$yB,$oB,$nB,$uB,$tB,$sB,$rB,$sn,$vB,$qB,$pB};$CB={$mB,$zB,$AB,$BB};$DB=q#/lib/json_data.b#;$EB=bless({$T2,$lB,$kq,$CB,$Q,$DB},$F3);$FB=q#ni:/lib/name_as_string.b#;$GB=q#ni:/lib/named.b#;$HB=q#ni:/lib/named_in_ni.b#;$IB=q#ni:/lib/namespaced.b#;$JB=q#ni:/lib/ni#;$KB={$N3,1};$LB={};$MB=q#extend#;$NB=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$OB=bless({$w,$NB,$y,$z},$A);$PB=q#is_mutable#;$QB=q#$0 ne '-' && -w $0#;$RB=bless({$w,$QB,$y,$z},$A);$SB=q#modify#;$TB=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$UB=bless({$w,$TB,$y,$z},$A);$VB={$MB,$OB,$PB,$RB,$SB,$UB};$WB=q#/lib/ni_self.b#;$XB=bless({$T2,$LB,$u4,$q,$v4,$q,$w4,$VB,$Q,$WB},$T3);$YB={};$ZB=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$cC=bless({$w,$ZB,$y,$z},$A);$dC=q#docs#;$eC=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$fC=bless({$w,$eC,$y,$z},$A);$gC=q#metaclasses#;$hC=q#my $self = shift;
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$iC=bless({$w,$hC,$y,$z},$A);$jC=q#undocumented#;$kC=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$lC=bless({$w,$kC,$y,$z},$A);$mC=q#untested#;$nC=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$oC=bless({$w,$nC,$y,$z},$A);$pC={$K,$cC,$dC,$fC,$gC,$iC,$jC,$lC,$mC,$oC};$qC=q#/lib/ni_dev_introspection.b#;$rC=bless({$T2,$YB,$u4,$q,$v4,$q,$w4,$pC,$Q,$qC},$T3);$sC={};$tC=q#--internal/+=#;$uC=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$vC=bless({$w,$uC,$y,$z},$A);$wC=q#--internal/dev-state#;$xC=q#my $self = shift;
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
0;#;$yC=bless({$w,$xC,$y,$z},$A);$zC=q#--internal/eval#;$AC=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$BC=bless({$w,$AC,$y,$z},$A);$CC=q#--internal/image#;$DC=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$EC=bless({$w,$DC,$y,$z},$A);$FC=q#--internal/test#;$GC=q#local $| = 1;
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
!!$failed;#;$HC=bless({$w,$GC,$y,$z},$A);$IC=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$JC=bless({$w,$IC,$y,$z},$A);$KC={$tC,$vC,$wC,$yC,$zC,$BC,$CC,$EC,$FC,$HC,$vt,$JC};$LC=q#/lib/ni_main.b#;$MC=bless({$T2,$sC,$u4,$q,$v4,$q,$w4,$KC,$Q,$LC},$T3);$NC={};$OC=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$PC=bless({$w,$OC,$y,$z},$A);$QC=q#resolver_for#;$RC=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$SC=bless({$w,$RC,$y,$z},$A);$TC={$o9,$PC,$QC,$SC};$UC=q#/lib/ni_resolver.b#;$VC=bless({$T2,$NC,$u4,$q,$v4,$q,$w4,$TC,$Q,$UC},$T3);$WC={};$XC=q#exists#;$YC=q#exists $_[0]->{named}{$_[1]}#;$ZC=bless({$w,$YC,$y,$z},$A);$cD=q#quoted#;$dD=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$eD=bless({$w,$dD,$y,$z},$A);$fD={$XC,$ZC,$cD,$eD};$gD=q#/lib/ni_image.b#;$hD=bless({$T2,$WC,$u4,$q,$v4,$q,$w4,$fD,$Q,$gD},$T3);$iD=[$G4,$XB,$rC,$MC,$VC,$hD];$jD=bless({$T2,$KB,$Q,$rd,$i3,$iD},$O3);$kD=q#ni:/lib/ni.c#;$lD={$O3,1};$mD=q#/lib/ni.c#;$nD=[$Bg];$oD=bless({$T2,$lD,$Q,$mD,$i3,$nD},$h4);$pD=q#ni:/lib/ni_dev_introspection.b#;$qD=q#ni:/lib/ni_image.b#;$rD=q#ni:/lib/ni_main.b#;$sD=q#ni:/lib/ni_resolver.b#;$tD=q#ni:/lib/ni_self.b#;$uD=q#ni:/lib/ni_static_util.b#;$vD={};$wD=q#abbrev#;$xD=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$yD=bless({$w,$xD,$y,$z},$A);$zD=q#dor#;$AD=q#defined $_[0] ? $_[0] : $_[1]#;$BD=bless({$w,$AD,$y,$z},$A);$CD=q#indent#;$DD=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$ED=bless({$w,$DD,$y,$z},$A);$FD=q#max#;$GD=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$HD=bless({$w,$GD,$y,$z},$A);$ID=q#maxstr#;$JD=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$KD=bless({$w,$JD,$y,$z},$A);$LD=q#mean#;$MD=q#sum(@_) / (@_ || 1)#;$ND=bless({$w,$MD,$y,$z},$A);$OD=q#min#;$PD=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$QD=bless({$w,$PD,$y,$z},$A);$RD=q#minstr#;$SD=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$TD=bless({$w,$SD,$y,$z},$A);$UD=q#outdent#;$VD=q#my $x = shift;
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
join "\\n", @lines;#;$WD=bless({$w,$VD,$y,$z},$A);$XD=q#sgr#;$YD=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$ZD=bless({$w,$YD,$y,$z},$A);$cE=q#sr#;$dE=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$eE=bless({$w,$dE,$y,$z},$A);$fE=q#sum#;$gE=q#local $_; my $x = 0; $x += $_ for @_; $x#;$hE=bless({$w,$gE,$y,$z},$A);$iE=q#swap#;$jE=q#@_[0, 1] = @_[1, 0]#;$kE=bless({$w,$jE,$y,$z},$A);$lE={$wD,$yD,$zD,$BD,$CD,$ED,$FD,$HD,$ID,$KD,$LD,$ND,$OD,$QD,$RD,$TD,$UD,$WD,$XD,$ZD,$cE,$eE,$fE,$hE,$iE,$kE};$mE=q#/lib/ni_static_util.b#;$nE=bless({$T2,$vD,$u4,$q,$v4,$q,$w4,$lE,$Q,$mE},$T3);$oE=q#ni:/lib/object_metadata#;$pE={$P3,1,$C,1,$H,1};$qE=q#/lib/object_metadata#;$rE={};$sE=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$tE=bless({$w,$sE,$y,$z},$A);$uE={$S2,$tE};$vE=q#/lib/object_metadata_rw.b#;$wE=bless({$T2,$rE,$u4,$q,$v4,$q,$w4,$uE,$Q,$vE},$T3);$xE=[$G4,$wE];$yE=bless({$T2,$pE,$Q,$qE,$i3,$xE},$Q3);$zE=q#ni:/lib/object_metadata.c#;$AE={$Q3,1,$d4,1,$g4,1};$BE=q#/lib/object_metadata.c#;$CE=[$Bg];$DE=bless({$T2,$AE,$Q,$BE,$i3,$CE},$h4);$EE=q#ni:/lib/object_metadata_rw.b#;$FE=q#ni:/lib/perlbranch.b#;$GE=q#ni:/lib/quote_circular_addressed.b#;$HE=q#ni:/lib/quote_code_fail.b#;$IE=q#ni:/lib/quote_gensym_identity.b#;$JE=q#ni:/lib/quote_objects.b#;$KE=q#ni:/lib/quote_simple#;$LE={$R3,1};$ME={};$NE=[];$OE=q#+{}#;$PE=bless({$t,$NE,$v,$q,$w,$OE,$y,$z},$A);$QE={$J6,$PE};$RE=q#/lib/quote_simple_init.b#;$SE=bless({$T2,$ME,$u4,$q,$v4,$q,$w4,$QE,$Q,$RE},$T3);$TE={};$UE=[];$VE=bless({$t,$UE,$v,$q,$w,0,$y,$z},$A);$WE=[];$XE=q#shift->quote_value(shift)#;$YE=bless({$t,$WE,$v,$q,$w,$XE,$y,$z},$A);$ZE={$sc,$VE,$Mc,$YE};$cF=q#/lib/quote_simple_quote.b#;$dF=bless({$T2,$TE,$u4,$q,$v4,$q,$w4,$ZE,$Q,$cF},$T3);$eF=[$G4,$SE,$dF,$sb,$Qb,$ic];$fF=bless({$T2,$LE,$Q,$Cd,$i3,$eF},$S3);$gF=q#ni:/lib/quote_simple.c#;$hF={$S3,1};$iF=q#/lib/quote_simple.c#;$jF=[$Bg];$kF=bless({$T2,$hF,$Q,$iF,$i3,$jF},$h4);$lF=q#ni:/lib/quote_simple_init.b#;$mF=q#ni:/lib/quote_simple_quote.b#;$nF=q#ni:/lib/quote_values.b#;$oF=q#ni:/lib/ref_eq.b#;$pF=q#ni:/lib/resolver.b#;$qF=q#ni:/lib/slice#;$rF=q#ni:/lib/slice.b#;$sF=q#ni:/lib/slice.c#;$tF={$U3,1};$uF=q#/lib/slice.c#;$vF=[$Fg];$wF=bless({$T2,$tF,$Q,$uF,$i3,$vF},$h4);$xF=q#ni:/lib/slice_init.b#;$yF=q#ni:/lib/slice_serialize.b#;$zF=q#ni:/lib/static_fn.b#;$AF={};$BF=q#fc#;$CF=[];$DF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$EF=bless({$t,$CF,$v,$q,$w,$DF,$y,$z},$A);$FF=q#fk#;$GF=[];$HF=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$IF=bless({$t,$GF,$v,$q,$w,$HF,$y,$lA},$A);$JF=[];$KF=q#ni('ni:/lib/fn')->new(@_)#;$LF=bless({$t,$JF,$v,$q,$w,$KF,$y,$lA},$A);$MF=q#fp#;$NF=[];$OF=q#($$)#;$PF=bless({$t,$NF,$v,$q,$w,$KF,$y,$OF},$A);$QF={$BF,$EF,$FF,$IF,$Ox,$LF,$MF,$PF};$RF=q#/lib/static_fn.b#;$SF=bless({$T2,$AF,$u4,$q,$v4,$q,$w4,$QF,$Q,$RF},$T3);$TF=q#ni:/lib/subclass.b#;$UF=q#ni:/lib/tag#;$VF={$V3,1};$WF=q#/lib/tag#;$XF={};$YF=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$ZF=bless({$w,$YF,$y,$z},$A);$cG={$I8,$ZF};$dG=q#/lib/tag.b#;$eG=bless({$T2,$XF,$u4,$q,$v4,$q,$w4,$cG,$Q,$dG},$T3);$fG={};$gG=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$hG=bless({$w,$gG,$y,$z},$A);$iG={$J6,$hG};$jG=q#/lib/tag_init.b#;$kG=bless({$T2,$fG,$u4,$q,$v4,$q,$w4,$iG,$Q,$jG},$T3);$lG=[$A8,$G8,$eG,$kG];$mG=bless({$T2,$VF,$Q,$WF,$i3,$lG},$W3);$nG=q#ni:/lib/tag.b#;$oG=q#ni:/lib/tag.c#;$pG={$W3,1};$qG=q#/lib/tag.c#;$rG=[$Fg];$sG=bless({$T2,$pG,$Q,$qG,$i3,$rG},$h4);$tG=q#ni:/lib/tag_init.b#;$uG=q#ni:/lib/test_assert_eq#;$vG={$X3,1};$wG=q#/lib/test_assert_eq#;$xG={$X3,1,$Z3,1};$yG=q#/lib/test_assertion#;$zG={};$AG=q#commit#;$BG=[];$CG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$DG=bless({$t,$BG,$v,$q,$w,$CG,$y,$z},$A);$EG={$AG,$DG};$FG=q#/lib/test_assertion_commit.b#;$GG=bless({$T2,$zG,$u4,$q,$v4,$q,$w4,$EG,$Q,$FG},$T3);$HG=[$G4,$GG];$IG=bless({$T2,$xG,$Q,$yG,$i3,$HG},$c4);$JG={};$KG=q#diff#;$LG=[];$MG=q#shift->{'diff'}#;$NG=bless({$t,$LG,$v,$q,$w,$MG,$y,$z},$A);$OG={$KG,$NG};$PG=q#/lib/test_assert_eq_ro.b#;$QG=bless({$T2,$JG,$u4,$q,$v4,$q,$w4,$OG,$Q,$PG},$T3);$RG={};$SG=[];$TG=q#my ($class, $diff) = @_;
+{diff => $diff};#;$UG=bless({$t,$SG,$v,$q,$w,$TG,$y,$z},$A);$VG={$J6,$UG};$WG=q#/lib/test_assert_eq_init.b#;$XG=bless({$T2,$RG,$u4,$q,$v4,$q,$w4,$VG,$Q,$WG},$T3);$YG={};$ZG=[];$cH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$dH=bless({$t,$ZG,$v,$q,$w,$cH,$y,$z},$A);$eH=q#failed#;$fH=[];$gH=q#defined shift->{diff}#;$hH=bless({$t,$fH,$v,$q,$w,$gH,$y,$z},$A);$iH=q#result#;$jH=[];$kH=bless({$t,$jH,$v,$q,$w,$xt,$y,$z},$A);$lH={$wf,$dH,$eH,$hH,$iH,$kH};$mH=q#/lib/test_assert_eq_result.b#;$nH=bless({$T2,$YG,$u4,$q,$v4,$q,$w4,$lH,$Q,$mH},$T3);$oH=[$IG,$QG,$XG,$nH];$pH=bless({$T2,$vG,$Q,$wG,$i3,$oH},$Y3);$qH=q#ni:/lib/test_assert_eq.c#;$rH={$Y3,1};$sH=q#/lib/test_assert_eq.c#;$tH={$Y3,1,$c4,1};$uH=q#/lib/test_assertion.c#;$vH=[$Bg];$wH=bless({$T2,$tH,$Q,$uH,$i3,$vH},$h4);$xH=[$wH];$yH=bless({$T2,$rH,$Q,$sH,$i3,$xH},$h4);$zH=q#ni:/lib/test_assert_eq_init.b#;$AH=q#ni:/lib/test_assert_eq_result.b#;$BH=q#ni:/lib/test_assert_eq_ro.b#;$CH=q#ni:/lib/test_assertion#;$DH=q#ni:/lib/test_assertion.c#;$EH=q#ni:/lib/test_assertion_commit.b#;$FH=q#ni:/lib/test_case#;$GH={$C,1};$HH=q#/lib/test_case#;$IH=q#running_test#;$JH={};$KH=[];$LH=q#shift->{'assertions'}#;$MH=bless({$t,$KH,$v,$q,$w,$LH,$y,$z},$A);$NH=[];$OH=q#shift->{'test'}#;$PH=bless({$t,$NH,$v,$q,$w,$OH,$y,$z},$A);$QH={$n,$MH,$s,$PH};$RH=q#/lib/test_case_ro.b#;$SH=bless({$T2,$JH,$u4,$q,$v4,$q,$w4,$QH,$Q,$RH},$T3);$TH={};$UH=[];$VH=q#if (@_ == 2) {
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
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$mI=bless({$t,$kI,$v,$q,$w,$lI,$y,$z},$A);$nI=[];$oI=q#!shift->{outcome}->[0]#;$pI=bless({$t,$nI,$v,$q,$w,$oI,$y,$z},$A);$qI={$wf,$mI,$eH,$pI};$rI=q#/lib/test_case_metrics.b#;$sI=bless({$T2,$jI,$u4,$q,$v4,$q,$w4,$qI,$Q,$rI},$T3);$tI={};$uI=q#done#;$vI=[];$wI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$xI=bless({$t,$vI,$v,$q,$w,$wI,$y,$z},$A);$yI=[];$zI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$AI=bless({$t,$yI,$v,$q,$w,$zI,$y,$z},$A);$BI={$uI,$xI,$vt,$AI};$CI=q#/lib/test_case_run.b#;$DI=bless({$T2,$tI,$u4,$q,$v4,$q,$w4,$BI,$Q,$CI},$T3);$EI=[$yE,$SH,$ZH,$iI,$sI,$DI];$FI=bless({$T2,$GH,$Q,$HH,$IH,$q,$i3,$EI},$d4);$GI=[];$HI=q#shift->{running_test} = undef#;$II=bless({$t,$GI,$v,$q,$w,$HI,$y,$z},$A);$JI=q#ni:/lib/test_case.c#;$KI={$d4,1};$LI=q#/lib/test_case.c#;$MI={};$NI=[];$OI=q#shift->{'running_test'}#;$PI=bless({$t,$NI,$v,$q,$w,$OI,$y,$z},$A);$QI={$IH,$PI};$RI=q#/lib/test_case.c_test_ro.b#;$SI=bless({$T2,$MI,$u4,$q,$v4,$q,$w4,$QI,$Q,$RI},$T3);$TI={};$UI=q#with_test#;$VI=[];$WI=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$XI=bless({$t,$VI,$v,$q,$w,$WI,$y,$z},$A);$YI={$UI,$XI};$ZI=q#/lib/test_case.c_test.b#;$cJ=bless({$T2,$TI,$u4,$II,$v4,$q,$w4,$YI,$Q,$ZI},$T3);$dJ=[$DE,$SI,$cJ];$eJ=bless({$T2,$KI,$Q,$LI,$i3,$dJ},$h4);$fJ=q#ni:/lib/test_case.c_test.b#;$gJ=q#ni:/lib/test_case.c_test_ro.b#;$hJ=q#ni:/lib/test_case_init.b#;$iJ=q#ni:/lib/test_case_metrics.b#;$jJ=q#ni:/lib/test_case_ro.b#;$kJ=q#ni:/lib/test_case_run.b#;$lJ=q#ni:/lib/test_case_rw.b#;$mJ=q#ni:/lib/test_value#;$nJ={$e4,1};$oJ=q#/lib/test_value#;$pJ={};$qJ=[];$rJ=q#\\$_[1]#;$sJ=bless({$t,$qJ,$v,$q,$w,$rJ,$y,$z},$A);$tJ={$J6,$sJ};$uJ=q#/lib/test_value_init.b#;$vJ=bless({$T2,$pJ,$u4,$q,$v4,$q,$w4,$tJ,$Q,$uJ},$T3);$wJ={};$xJ=q#(==#;$yJ=[];$zJ=q#my ($self, $rhs) = @_;
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
return undef;#;$HJ=bless({$t,$FJ,$v,$q,$w,$GJ,$y,$z},$A);$IJ={$xJ,$AJ,$BJ,$EJ,$KG,$HJ};$JJ=q#/lib/test_value_eq.b#;$KJ=bless({$T2,$wJ,$u4,$q,$v4,$q,$w4,$IJ,$Q,$JJ},$T3);$LJ={};$MJ=[];$NJ=q#ni::json_encode ${$_[0]}#;$OJ=bless({$t,$MJ,$v,$q,$w,$NJ,$y,$z},$A);$PJ={$wf,$OJ};$QJ=q#/lib/test_value_str.b#;$RJ=bless({$T2,$LJ,$u4,$q,$v4,$q,$w4,$PJ,$Q,$QJ},$T3);$SJ=[$G4,$vJ,$KJ,$RJ];$TJ=bless({$T2,$nJ,$Q,$oJ,$i3,$SJ},$f4);$UJ=q#ni:/lib/test_value.c#;$VJ={$f4,1};$WJ=q#/lib/test_value.c#;$XJ=[$Bg];$YJ=bless({$T2,$VJ,$Q,$WJ,$i3,$XJ},$h4);$ZJ=q#ni:/lib/test_value_eq.b#;$cK=q#ni:/lib/test_value_init.b#;$dK=q#ni:/lib/test_value_str.b#;$eK=q#ni:/lib/todo#;$fK={$H,1};$gK=q#/lib/todo#;$hK={};$iK=q#shift->{'todo'}#;$jK=bless({$w,$iK,$y,$z},$A);$kK={$E,$jK};$lK=q#/lib/todo_ro.b#;$mK=bless({$T2,$hK,$u4,$q,$v4,$q,$w4,$kK,$Q,$lK},$T3);$nK={};$oK=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$pK=bless({$w,$oK,$y,$z},$A);$qK={$J6,$pK};$rK=q#/lib/todo_init.b#;$sK=bless({$T2,$nK,$u4,$q,$v4,$q,$w4,$qK,$Q,$rK},$T3);$tK={};$uK=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$vK=bless({$w,$uK,$y,$z},$A);$wK={$wf,$vK};$xK=q#/lib/todo_str.b#;$yK=bless({$T2,$tK,$u4,$q,$v4,$q,$w4,$wK,$Q,$xK},$T3);$zK=[$yE,$mK,$sK,$yK];$AK=bless({$T2,$fK,$Q,$gK,$i3,$zK},$g4);$BK=q#ni:/lib/todo.c#;$CK={$g4,1};$DK=q#/lib/todo.c#;$EK=[$DE];$FK=bless({$T2,$CK,$Q,$DK,$i3,$EK},$h4);$GK=q#ni:/lib/todo_ctor.b#;$HK={};$IK=q#ni('ni:/lib/todo')->new(@_)#;$JK=bless({$w,$IK,$y,$z},$A);$KK={$R2,$JK};$LK=q#/lib/todo_ctor.b#;$MK=bless({$T2,$HK,$u4,$q,$v4,$q,$w4,$KK,$Q,$LK},$T3);$NK=q#ni:/lib/todo_init.b#;$OK=q#ni:/lib/todo_ro.b#;$PK=q#ni:/lib/todo_str.b#;$QK=q#ni:/metaclass#;$RK={$h4,1};$SK=q#/metaclass#;$TK=[$Te,$jg,$Ze,$cg];$UK=bless({$T2,$RK,$Q,$SK,$i3,$TK},$i4);$VK=q#ni:/metaclass.c#;$WK={$i4,1};$XK=q#/metaclass.c#;$YK=[$sg];$ZK=bless({$T2,$WK,$Q,$XK,$i3,$YK},$h4);$cL=q#ni:/module#;$dL=q#ni:/module.c#;$eL=q#ni:/object#;$fL=q#ni:/object.c#;$gL=q#ni:/semantic#;$hL=q#semantic#;$iL={$hL,1};$jL=[];$kL=bless({$T2,$iL,$Q,$Ce,$i3,$jL},$j4);$lL=q#ni:/semantic/dimension#;$mL={$n4,1};$nL=q#/semantic/dimension#;$oL=[$sg];$pL=bless({$T2,$mL,$Q,$nL,$i3,$oL},$o4);$qL=q#ni:/semantic/dimension.c#;$rL={$o4,1};$sL=q#/semantic/dimension.c#;$tL=[$Jg];$uL=bless({$T2,$rL,$Q,$sL,$i3,$tL},$h4);$vL=q#ni:/semantic/task#;$wL=q#ni:/semantic/task.c#;$xL=q#ni:/semantic/task_outcome.b#;$yL=q#ni:/semantic/task_ro.b#;$zL=q#ni:main#;$AL={$dp,1};$BL=[$MK,$SF,$tA,$cp];$CL=bless({$T2,$AL,$Q,$dp,$i3,$BL},$j4);$DL=q#ni:ni#;$EL={$jB,1};$FL=[$nE,$EB,$iB];$GL=bless({$T2,$EL,$Q,$jB,$i3,$FL},$j4);$HL={$d,$T,$W,$f1,$g1,$l1,$m1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$J2,$K2,$P2,$Q2,$n6,$o6,$g8,$h8,$n8,$o8,$J9,$K9,$fa,$ga,$wa,$xa,$kd,$ld,$sd,$td,$Dd,$Ed,$xe,$ye,$De,$Ee,$sg,$tg,$Jg,$Kg,$Og,$Pg,$Sg,$Tg,$xh,$yh,$Oh,$Ph,$dh,$Qh,$vh,$Rh,$ki,$li,$pi,$qi,$Zh,$ri,$ii,$si,$Yj,$Zj,$fk,$gk,$Gj,$hk,$Wj,$ik,$Ii,$jk,$yj,$kk,$ej,$lk,$Bi,$mk,$El,$Il,$im,$jm,$gm,$km,$cl,$lm,$nl,$mm,$Ck,$nm,$Bl,$om,$vk,$pm,$Kk,$qm,$Kn,$Ln,$Pn,$Qn,$Gm,$Rn,$fn,$Sn,$Nm,$Tn,$In,$Un,$ym,$Vn,$nn,$Wn,$ro,$so,$wo,$xo,$po,$yo,$go,$zo,$cp,$ep,$Cp,$Dp,$Hp,$Ip,$np,$Jp,$Ap,$Kp,$h6,$Lp,$Mh,$Mp,$Kh,$Np,$l5,$Op,$t5,$Pp,$F5,$Qp,$P4,$Rp,$f6,$Sp,$R5,$Tp,$L7,$Up,$Yp,$Zp,$J7,$cq,$P6,$dq,$p7,$eq,$E6,$fq,$d7,$gq,$Vq,$Wq,$cr,$dr,$Fq,$er,$Tq,$fr,$yq,$gr,$qs,$us,$Gs,$Hs,$Es,$Is,$Kt,$Ot,$ku,$lu,$iu,$mu,$jt,$nu,$tt,$ou,$ct,$pu,$Ft,$qu,$Sr,$ru,$os,$su,$Ku,$Lu,$Pu,$Qu,$Bu,$Ru,$Iu,$Su,$Wu,$Xu,$uf,$Yu,$A8,$Zu,$Fg,$cv,$mv,$nv,$Re,$ov,$sv,$tv,$kv,$uv,$Ze,$vv,$D9,$wv,$Q8,$xv,$Bv,$Cv,$W8,$Dv,$Yf,$Ev,$kf,$Fv,$Pf,$Gv,$Wf,$Hv,$Mw,$Nw,$Yw,$Zw,$Ww,$cx,$uw,$dx,$gw,$ex,$Aw,$fx,$ow,$gx,$Ov,$hx,$Uv,$ix,$Kw,$jx,$y8,$kx,$oy,$sy,$Ey,$Fy,$Cy,$Gy,$Cx,$Hy,$fy,$Iy,$Ux,$Jy,$my,$Ky,$Uz,$Vz,$Zz,$cA,$Sz,$dA,$ez,$eA,$Vy,$fA,$yz,$gA,$cd,$hA,$tA,$uA,$ed,$vA,$zA,$AA,$Ka,$BA,$kb,$CA,$E4,$DA,$jg,$EA,$iB,$kB,$EB,$FB,$Bf,$GB,$G8,$HB,$f9,$IB,$m9,$JB,$jD,$kD,$oD,$pD,$rC,$qD,$hD,$rD,$MC,$sD,$VC,$tD,$XB,$uD,$nE,$oE,$yE,$zE,$DE,$EE,$wE,$FE,$Te,$GE,$yc,$HE,$sb,$IE,$Sc,$JE,$ic,$KE,$fF,$gF,$kF,$lF,$SE,$mF,$dF,$nF,$Qb,$oF,$If,$pF,$t9,$qF,$ce,$rF,$Sd,$sF,$wF,$xF,$Yd,$yF,$B9,$zF,$SF,$TF,$qg,$UF,$mG,$nG,$eG,$oG,$sG,$tG,$kG,$uG,$pH,$qH,$yH,$zH,$XG,$AH,$nH,$BH,$QG,$CH,$IG,$DH,$wH,$EH,$GG,$FH,$FI,$JI,$eJ,$fJ,$cJ,$gJ,$SI,$hJ,$iI,$iJ,$sI,$jJ,$SH,$kJ,$DI,$lJ,$ZH,$mJ,$TJ,$UJ,$YJ,$ZJ,$KJ,$cK,$vJ,$dK,$RJ,$eK,$AK,$BK,$FK,$GK,$MK,$NK,$sK,$OK,$mK,$PK,$yK,$QK,$UK,$VK,$ZK,$cL,$cg,$dL,$Hg,$eL,$G4,$fL,$Bg,$gL,$kL,$lL,$pL,$qL,$uL,$vL,$Fr,$wL,$As,$xL,$Dr,$yL,$rr,$zL,$CL,$DL,$GL};$IL=q#resolvers#;$JL=[];$KL=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$LL=bless({$t,$JL,$v,$q,$w,$KL,$y,$z},$A);$ML=q#file#;$NL=[];$OL=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$PL=bless({$t,$NL,$v,$q,$w,$OL,$y,$z},$A);$QL=q#null#;$RL=[];$SL=q#ni('ni:/io/null')->new#;$TL=bless({$t,$RL,$v,$q,$w,$SL,$y,$z},$A);$UL=q#sh#;$VL=[];$WL=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$XL=bless({$t,$VL,$v,$q,$w,$WL,$y,$z},$A);$YL=q#str#;$ZL=[];$cM=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$dM=bless({$t,$ZL,$v,$q,$w,$cM,$y,$z},$A);$eM={$r7,$LL,$ML,$PL,$QL,$TL,$UL,$XL,$YL,$dM};$fM=bless({$c,$HL,$IL,$eM},$N3);*$vx=\&$sx;*$ux=\&$qx;*$tx=\&$ox;*$Od=\&$Md;*$Nd=\&$Kd;$E4->apply_($j3);$E4->apply_($k3);$E4->apply_($U2);$E4->apply_($l3);$E4->apply_($V2);$E4->apply_($m3);$E4->apply_($W2);$E4->apply_($n3);$E4->apply_($X2);$E4->apply_($o3);$E4->apply_($Y2);$E4->apply_($p3);$E4->apply_($Z2);$E4->apply_($q3);$E4->apply_($c3);$E4->apply_($r3);$E4->apply_($d3);$E4->apply_($s3);$E4->apply_($e3);$E4->apply_($t3);$E4->apply_($f3);$E4->apply_($u3);$E4->apply_($v3);$E4->apply_($w3);$E4->apply_($x3);$E4->apply_($y3);$E4->apply_($z3);$E4->apply_($A3);$E4->apply_($B3);$E4->apply_($C3);$E4->apply_($D3);$E4->apply_($E3);$E4->apply_($F3);$E4->apply_($G3);$E4->apply_($S);$E4->apply_($H3);$E4->apply_($A);$E4->apply_($I3);$E4->apply_($J3);$E4->apply_($K3);$E4->apply_($L3);$E4->apply_($M3);$E4->apply_($N3);$E4->apply_($O3);$E4->apply_($P3);$E4->apply_($Q3);$E4->apply_($R3);$E4->apply_($S3);$E4->apply_($T3);$E4->apply_($U3);$E4->apply_($V3);$E4->apply_($W3);$E4->apply_($X3);$E4->apply_($Y3);$E4->apply_($Z3);$E4->apply_($c4);$E4->apply_($C);$E4->apply_($d4);$E4->apply_($e4);$E4->apply_($f4);$E4->apply_($H);$E4->apply_($g4);$E4->apply_($h4);$E4->apply_($i4);$E4->apply_($j4);$E4->apply_($k4);$E4->apply_($l4);$E4->apply_($m4);$E4->apply_($n4);$E4->apply_($o4);$E4->apply_($p4);$E4->apply_($q4);$P4->apply_($U2);$P4->apply_($V2);$P4->apply_($W2);$P4->apply_($X2);$P4->apply_($Y2);$P4->apply_($Z2);$P4->apply_($c3);$P4->apply_($d3);$P4->apply_($e3);$P4->apply_($f3);$l5->apply_($U2);$l5->apply_($V2);$l5->apply_($W2);$l5->apply_($X2);$l5->apply_($Y2);$l5->apply_($Z2);$l5->apply_($c3);$l5->apply_($d3);$l5->apply_($e3);$l5->apply_($f3);$t5->apply_($U2);$t5->apply_($V2);$t5->apply_($W2);$t5->apply_($X2);$t5->apply_($Y2);$t5->apply_($Z2);$t5->apply_($c3);$t5->apply_($d3);$t5->apply_($e3);$t5->apply_($f3);$F5->apply_($U2);$F5->apply_($V2);$F5->apply_($W2);$F5->apply_($X2);$F5->apply_($Y2);$F5->apply_($Z2);$F5->apply_($c3);$F5->apply_($d3);$F5->apply_($e3);$F5->apply_($f3);$R5->apply_($U2);$R5->apply_($V2);$R5->apply_($W2);$R5->apply_($X2);$R5->apply_($Y2);$R5->apply_($Z2);$R5->apply_($c3);$R5->apply_($d3);$R5->apply_($e3);$R5->apply_($f3);$f6->apply_($U2);$f6->apply_($V2);$f6->apply_($W2);$f6->apply_($X2);$f6->apply_($Y2);$f6->apply_($Z2);$f6->apply_($c3);$f6->apply_($d3);$f6->apply_($e3);$f6->apply_($f3);$E6->apply_($e3);$P6->apply_($e3);$d7->apply_($e3);$p7->apply_($e3);$J7->apply_($e3);$y8->apply_($j3);$y8->apply_($k3);$y8->apply_($l3);$y8->apply_($m3);$y8->apply_($n3);$y8->apply_($o3);$y8->apply_($p3);$y8->apply_($q3);$y8->apply_($r3);$y8->apply_($s3);$y8->apply_($t3);$y8->apply_($u3);$y8->apply_($w3);$y8->apply_($y3);$y8->apply_($A3);$y8->apply_($B3);$y8->apply_($C3);$y8->apply_($D3);$y8->apply_($E3);$y8->apply_($F3);$y8->apply_($G3);$y8->apply_($H3);$y8->apply_($I3);$y8->apply_($K3);$y8->apply_($M3);$y8->apply_($O3);$y8->apply_($Q3);$y8->apply_($S3);$y8->apply_($T3);$y8->apply_($U3);$y8->apply_($V3);$y8->apply_($W3);$y8->apply_($Y3);$y8->apply_($c4);$y8->apply_($d4);$y8->apply_($f4);$y8->apply_($g4);$y8->apply_($h4);$y8->apply_($i4);$y8->apply_($j4);$y8->apply_($k4);$y8->apply_($m4);$y8->apply_($n4);$y8->apply_($o4);$y8->apply_($q4);$G8->apply_($j3);$G8->apply_($k3);$G8->apply_($l3);$G8->apply_($m3);$G8->apply_($n3);$G8->apply_($o3);$G8->apply_($p3);$G8->apply_($q3);$G8->apply_($r3);$G8->apply_($s3);$G8->apply_($t3);$G8->apply_($u3);$G8->apply_($w3);$G8->apply_($y3);$G8->apply_($A3);$G8->apply_($C3);$G8->apply_($D3);$G8->apply_($E3);$G8->apply_($F3);$G8->apply_($G3);$G8->apply_($S);$G8->apply_($H3);$G8->apply_($I3);$G8->apply_($K3);$G8->apply_($M3);$G8->apply_($O3);$G8->apply_($Q3);$G8->apply_($S3);$G8->apply_($T3);$G8->apply_($U3);$G8->apply_($V3);$G8->apply_($W3);$G8->apply_($Y3);$G8->apply_($c4);$G8->apply_($d4);$G8->apply_($f4);$G8->apply_($g4);$G8->apply_($h4);$G8->apply_($i4);$G8->apply_($j4);$G8->apply_($k4);$G8->apply_($m4);$G8->apply_($n4);$G8->apply_($o4);$G8->apply_($q4);$Q8->apply_($F3);$W8->apply_($F3);$f9->apply_($j3);$f9->apply_($k3);$f9->apply_($l3);$f9->apply_($m3);$f9->apply_($n3);$f9->apply_($o3);$f9->apply_($p3);$f9->apply_($q3);$f9->apply_($r3);$f9->apply_($s3);$f9->apply_($t3);$f9->apply_($u3);$f9->apply_($w3);$f9->apply_($y3);$f9->apply_($A3);$f9->apply_($C3);$f9->apply_($D3);$f9->apply_($E3);$f9->apply_($F3);$f9->apply_($G3);$f9->apply_($H3);$f9->apply_($I3);$f9->apply_($K3);$f9->apply_($M3);$f9->apply_($O3);$f9->apply_($Q3);$f9->apply_($S3);$f9->apply_($T3);$f9->apply_($U3);$f9->apply_($V3);$f9->apply_($W3);$f9->apply_($Y3);$f9->apply_($c4);$f9->apply_($d4);$f9->apply_($f4);$f9->apply_($g4);$f9->apply_($h4);$f9->apply_($i4);$f9->apply_($j4);$f9->apply_($k4);$f9->apply_($m4);$f9->apply_($n4);$f9->apply_($o4);$f9->apply_($q4);$m9->apply_($j3);$m9->apply_($k3);$m9->apply_($l3);$m9->apply_($m3);$m9->apply_($n3);$m9->apply_($o3);$m9->apply_($p3);$m9->apply_($q3);$m9->apply_($r3);$m9->apply_($s3);$m9->apply_($t3);$m9->apply_($u3);$m9->apply_($w3);$m9->apply_($y3);$m9->apply_($A3);$m9->apply_($C3);$m9->apply_($D3);$m9->apply_($E3);$m9->apply_($F3);$m9->apply_($G3);$m9->apply_($H3);$m9->apply_($I3);$m9->apply_($K3);$m9->apply_($M3);$m9->apply_($O3);$m9->apply_($Q3);$m9->apply_($S3);$m9->apply_($T3);$m9->apply_($U3);$m9->apply_($V3);$m9->apply_($W3);$m9->apply_($Y3);$m9->apply_($c4);$m9->apply_($d4);$m9->apply_($f4);$m9->apply_($g4);$m9->apply_($h4);$m9->apply_($i4);$m9->apply_($j4);$m9->apply_($k4);$m9->apply_($m4);$m9->apply_($n4);$m9->apply_($o4);$m9->apply_($q4);$t9->apply_($j3);$t9->apply_($k3);$t9->apply_($l3);$t9->apply_($m3);$t9->apply_($n3);$t9->apply_($o3);$t9->apply_($p3);$t9->apply_($q3);$t9->apply_($r3);$t9->apply_($s3);$t9->apply_($t3);$t9->apply_($u3);$t9->apply_($w3);$t9->apply_($y3);$t9->apply_($A3);$t9->apply_($C3);$t9->apply_($D3);$t9->apply_($E3);$t9->apply_($F3);$t9->apply_($G3);$t9->apply_($H3);$t9->apply_($I3);$t9->apply_($K3);$t9->apply_($M3);$t9->apply_($O3);$t9->apply_($Q3);$t9->apply_($S3);$t9->apply_($U3);$t9->apply_($V3);$t9->apply_($W3);$t9->apply_($Y3);$t9->apply_($c4);$t9->apply_($d4);$t9->apply_($f4);$t9->apply_($g4);$t9->apply_($h4);$t9->apply_($i4);$t9->apply_($j4);$t9->apply_($k4);$t9->apply_($m4);$t9->apply_($n4);$t9->apply_($o4);$t9->apply_($q4);$B9->apply_($F3);$B9->apply_($T3);$Ka->apply_($L3);$kb->apply_($L3);$sb->apply_($L3);$sb->apply_($R3);$Qb->apply_($L3);$Qb->apply_($R3);$ic->apply_($L3);$ic->apply_($R3);$yc->apply_($L3);$Sc->apply_($L3);$cd->apply_($L3);$Sd->apply_($T3);$Yd->apply_($T3);$Re->apply_($j3);$Re->apply_($k3);$Re->apply_($l3);$Re->apply_($m3);$Re->apply_($n3);$Re->apply_($o3);$Re->apply_($p3);$Re->apply_($q3);$Re->apply_($r3);$Re->apply_($s3);$Re->apply_($t3);$Re->apply_($u3);$Re->apply_($w3);$Re->apply_($y3);$Re->apply_($A3);$Re->apply_($C3);$Re->apply_($D3);$Re->apply_($E3);$Re->apply_($G3);$Re->apply_($H3);$Re->apply_($I3);$Re->apply_($K3);$Re->apply_($M3);$Re->apply_($O3);$Re->apply_($Q3);$Re->apply_($S3);$Re->apply_($U3);$Re->apply_($W3);$Re->apply_($Y3);$Re->apply_($c4);$Re->apply_($d4);$Re->apply_($f4);$Re->apply_($g4);$Re->apply_($h4);$Re->apply_($i4);$Re->apply_($j4);$Re->apply_($k4);$Re->apply_($m4);$Re->apply_($n4);$Re->apply_($o4);$Re->apply_($q4);$Ze->apply_($j3);$Ze->apply_($k3);$Ze->apply_($l3);$Ze->apply_($m3);$Ze->apply_($n3);$Ze->apply_($o3);$Ze->apply_($p3);$Ze->apply_($q3);$Ze->apply_($r3);$Ze->apply_($s3);$Ze->apply_($t3);$Ze->apply_($u3);$Ze->apply_($w3);$Ze->apply_($y3);$Ze->apply_($A3);$Ze->apply_($C3);$Ze->apply_($E3);$Ze->apply_($G3);$Ze->apply_($H3);$Ze->apply_($I3);$Ze->apply_($K3);$Ze->apply_($M3);$Ze->apply_($O3);$Ze->apply_($Q3);$Ze->apply_($S3);$Ze->apply_($U3);$Ze->apply_($W3);$Ze->apply_($Y3);$Ze->apply_($c4);$Ze->apply_($d4);$Ze->apply_($f4);$Ze->apply_($g4);$Ze->apply_($h4);$Ze->apply_($i4);$Ze->apply_($j4);$Ze->apply_($k4);$Ze->apply_($m4);$Ze->apply_($n4);$Ze->apply_($o4);$Ze->apply_($q4);$kf->apply_($j3);$kf->apply_($k3);$kf->apply_($l3);$kf->apply_($m3);$kf->apply_($n3);$kf->apply_($o3);$kf->apply_($p3);$kf->apply_($q3);$kf->apply_($r3);$kf->apply_($s3);$kf->apply_($t3);$kf->apply_($u3);$kf->apply_($w3);$kf->apply_($y3);$kf->apply_($A3);$kf->apply_($C3);$kf->apply_($D3);$kf->apply_($E3);$kf->apply_($G3);$kf->apply_($H3);$kf->apply_($I3);$kf->apply_($K3);$kf->apply_($M3);$kf->apply_($O3);$kf->apply_($Q3);$kf->apply_($S3);$kf->apply_($U3);$kf->apply_($W3);$kf->apply_($Y3);$kf->apply_($c4);$kf->apply_($d4);$kf->apply_($f4);$kf->apply_($g4);$kf->apply_($h4);$kf->apply_($i4);$kf->apply_($j4);$kf->apply_($k4);$kf->apply_($m4);$kf->apply_($n4);$kf->apply_($o4);$kf->apply_($q4);$uf->apply_($j3);$uf->apply_($k3);$uf->apply_($l3);$uf->apply_($m3);$uf->apply_($n3);$uf->apply_($o3);$uf->apply_($p3);$uf->apply_($q3);$uf->apply_($r3);$uf->apply_($s3);$uf->apply_($t3);$uf->apply_($u3);$uf->apply_($w3);$uf->apply_($y3);$uf->apply_($A3);$uf->apply_($C3);$uf->apply_($D3);$uf->apply_($E3);$uf->apply_($G3);$uf->apply_($H3);$uf->apply_($I3);$uf->apply_($K3);$uf->apply_($M3);$uf->apply_($O3);$uf->apply_($Q3);$uf->apply_($S3);$uf->apply_($U3);$uf->apply_($W3);$uf->apply_($Y3);$uf->apply_($c4);$uf->apply_($d4);$uf->apply_($f4);$uf->apply_($g4);$uf->apply_($h4);$uf->apply_($i4);$uf->apply_($j4);$uf->apply_($k4);$uf->apply_($m4);$uf->apply_($n4);$uf->apply_($o4);$uf->apply_($q4);$Bf->apply_($j3);$Bf->apply_($k3);$Bf->apply_($l3);$Bf->apply_($m3);$Bf->apply_($n3);$Bf->apply_($o3);$Bf->apply_($p3);$Bf->apply_($q3);$Bf->apply_($r3);$Bf->apply_($s3);$Bf->apply_($t3);$Bf->apply_($u3);$Bf->apply_($w3);$Bf->apply_($y3);$Bf->apply_($A3);$Bf->apply_($C3);$Bf->apply_($D3);$Bf->apply_($E3);$Bf->apply_($G3);$Bf->apply_($H3);$Bf->apply_($I3);$Bf->apply_($K3);$Bf->apply_($M3);$Bf->apply_($O3);$Bf->apply_($Q3);$Bf->apply_($S3);$Bf->apply_($U3);$Bf->apply_($W3);$Bf->apply_($Y3);$Bf->apply_($c4);$Bf->apply_($d4);$Bf->apply_($f4);$Bf->apply_($g4);$Bf->apply_($h4);$Bf->apply_($i4);$Bf->apply_($j4);$Bf->apply_($k4);$Bf->apply_($m4);$Bf->apply_($n4);$Bf->apply_($o4);$Bf->apply_($q4);$If->apply_($j3);$If->apply_($k3);$If->apply_($l3);$If->apply_($m3);$If->apply_($n3);$If->apply_($o3);$If->apply_($p3);$If->apply_($q3);$If->apply_($r3);$If->apply_($s3);$If->apply_($t3);$If->apply_($u3);$If->apply_($w3);$If->apply_($y3);$If->apply_($A3);$If->apply_($C3);$If->apply_($D3);$If->apply_($E3);$If->apply_($G3);$If->apply_($H3);$If->apply_($I3);$If->apply_($K3);$If->apply_($M3);$If->apply_($O3);$If->apply_($Q3);$If->apply_($S3);$If->apply_($U3);$If->apply_($W3);$If->apply_($Y3);$If->apply_($c4);$If->apply_($d4);$If->apply_($f4);$If->apply_($g4);$If->apply_($h4);$If->apply_($i4);$If->apply_($j4);$If->apply_($k4);$If->apply_($m4);$If->apply_($n4);$If->apply_($o4);$If->apply_($q4);$Pf->apply_($j3);$Pf->apply_($k3);$Pf->apply_($l3);$Pf->apply_($m3);$Pf->apply_($n3);$Pf->apply_($o3);$Pf->apply_($p3);$Pf->apply_($q3);$Pf->apply_($r3);$Pf->apply_($s3);$Pf->apply_($t3);$Pf->apply_($u3);$Pf->apply_($w3);$Pf->apply_($y3);$Pf->apply_($A3);$Pf->apply_($C3);$Pf->apply_($D3);$Pf->apply_($E3);$Pf->apply_($G3);$Pf->apply_($H3);$Pf->apply_($I3);$Pf->apply_($K3);$Pf->apply_($M3);$Pf->apply_($O3);$Pf->apply_($Q3);$Pf->apply_($S3);$Pf->apply_($U3);$Pf->apply_($W3);$Pf->apply_($Y3);$Pf->apply_($c4);$Pf->apply_($d4);$Pf->apply_($f4);$Pf->apply_($g4);$Pf->apply_($h4);$Pf->apply_($i4);$Pf->apply_($j4);$Pf->apply_($k4);$Pf->apply_($m4);$Pf->apply_($n4);$Pf->apply_($o4);$Pf->apply_($q4);$Wf->apply_($j3);$Wf->apply_($k3);$Wf->apply_($l3);$Wf->apply_($m3);$Wf->apply_($n3);$Wf->apply_($o3);$Wf->apply_($p3);$Wf->apply_($q3);$Wf->apply_($r3);$Wf->apply_($s3);$Wf->apply_($t3);$Wf->apply_($u3);$Wf->apply_($w3);$Wf->apply_($y3);$Wf->apply_($A3);$Wf->apply_($C3);$Wf->apply_($D3);$Wf->apply_($E3);$Wf->apply_($G3);$Wf->apply_($H3);$Wf->apply_($I3);$Wf->apply_($K3);$Wf->apply_($M3);$Wf->apply_($O3);$Wf->apply_($Q3);$Wf->apply_($S3);$Wf->apply_($U3);$Wf->apply_($W3);$Wf->apply_($Y3);$Wf->apply_($c4);$Wf->apply_($d4);$Wf->apply_($f4);$Wf->apply_($g4);$Wf->apply_($h4);$Wf->apply_($i4);$Wf->apply_($j4);$Wf->apply_($k4);$Wf->apply_($m4);$Wf->apply_($n4);$Wf->apply_($o4);$Wf->apply_($q4);$jg->apply_($j3);$jg->apply_($k3);$jg->apply_($l3);$jg->apply_($m3);$jg->apply_($n3);$jg->apply_($o3);$jg->apply_($p3);$jg->apply_($q3);$jg->apply_($r3);$jg->apply_($s3);$jg->apply_($t3);$jg->apply_($u3);$jg->apply_($w3);$jg->apply_($y3);$jg->apply_($A3);$jg->apply_($C3);$jg->apply_($E3);$jg->apply_($G3);$jg->apply_($H3);$jg->apply_($A);$jg->apply_($I3);$jg->apply_($K3);$jg->apply_($M3);$jg->apply_($O3);$jg->apply_($Q3);$jg->apply_($S3);$jg->apply_($T3);$jg->apply_($U3);$jg->apply_($V3);$jg->apply_($W3);$jg->apply_($Y3);$jg->apply_($c4);$jg->apply_($d4);$jg->apply_($f4);$jg->apply_($g4);$jg->apply_($h4);$jg->apply_($i4);$jg->apply_($k4);$jg->apply_($m4);$jg->apply_($n4);$jg->apply_($o4);$jg->apply_($q4);$qg->apply_($j3);$qg->apply_($k3);$qg->apply_($l3);$qg->apply_($m3);$qg->apply_($n3);$qg->apply_($o3);$qg->apply_($p3);$qg->apply_($q3);$qg->apply_($r3);$qg->apply_($s3);$qg->apply_($t3);$qg->apply_($u3);$qg->apply_($w3);$qg->apply_($y3);$qg->apply_($A3);$qg->apply_($C3);$qg->apply_($E3);$qg->apply_($G3);$qg->apply_($H3);$qg->apply_($I3);$qg->apply_($K3);$qg->apply_($M3);$qg->apply_($O3);$qg->apply_($Q3);$qg->apply_($S3);$qg->apply_($U3);$qg->apply_($W3);$qg->apply_($Y3);$qg->apply_($c4);$qg->apply_($d4);$qg->apply_($f4);$qg->apply_($g4);$qg->apply_($i4);$qg->apply_($k4);$qg->apply_($m4);$qg->apply_($n4);$qg->apply_($o4);$qg->apply_($q4);$dh->apply_($U2);$vh->apply_($U2);$Kh->apply_($l3);$Kh->apply_($m3);$Kh->apply_($n3);$Kh->apply_($o3);$Kh->apply_($p3);$Kh->apply_($q3);$Kh->apply_($r3);$Kh->apply_($s3);$Kh->apply_($t3);$Kh->apply_($u3);$Zh->apply_($V2);$ii->apply_($V2);$Bi->apply_($W2);$Ii->apply_($W2);$ej->apply_($W2);$yj->apply_($W2);$Gj->apply_($W2);$Wj->apply_($W2);$vk->apply_($X2);$vk->apply_($Z2);$Ck->apply_($X2);$Kk->apply_($X2);$cl->apply_($X2);$cl->apply_($Z2);$nl->apply_($X2);$Bl->apply_($X2);$Bl->apply_($Z2);$gm->apply_($o3);$ym->apply_($Y2);$Gm->apply_($Y2);$Nm->apply_($Y2);$fn->apply_($Y2);$nn->apply_($Y2);$In->apply_($Y2);$go->apply_($Z2);$po->apply_($Z2);$cp->apply_($dp);$np->apply_($c3);$Ap->apply_($c3);$yq->apply_($f3);$Fq->apply_($f3);$Tq->apply_($f3);$rr->apply_($v3);$rr->apply_($x3);$rr->apply_($z3);$rr->apply_($p4);$Dr->apply_($v3);$Dr->apply_($x3);$Dr->apply_($z3);$Dr->apply_($p4);$Sr->apply_($v3);$Sr->apply_($x3);$Sr->apply_($z3);$os->apply_($v3);$os->apply_($x3);$os->apply_($z3);$Es->apply_($w3);$Es->apply_($y3);$Es->apply_($A3);$ct->apply_($x3);$jt->apply_($x3);$tt->apply_($x3);$Ft->apply_($x3);$iu->apply_($y3);$Bu->apply_($z3);$Iu->apply_($z3);$kv->apply_($D3);$Ov->apply_($S);$Uv->apply_($S);$gw->apply_($S);$ow->apply_($S);$uw->apply_($S);$Aw->apply_($S);$Kw->apply_($S);$Ww->apply_($H3);$Cx->apply_($A);$Ux->apply_($A);$fy->apply_($A);$my->apply_($A);$Cy->apply_($I3);$Vy->apply_($J3);$ez->apply_($J3);$yz->apply_($J3);$Sz->apply_($J3);$tA->apply_($dp);$iB->apply_($jB);$EB->apply_($jB);$XB->apply_($N3);$rC->apply_($N3);$MC->apply_($N3);$VC->apply_($N3);$hD->apply_($N3);$nE->apply_($jB);$wE->apply_($P3);$wE->apply_($C);$wE->apply_($H);$SE->apply_($R3);$dF->apply_($R3);$SF->apply_($dp);$eG->apply_($V3);$kG->apply_($V3);$GG->apply_($X3);$GG->apply_($Z3);$QG->apply_($X3);$XG->apply_($X3);$nH->apply_($X3);$SH->apply_($C);$ZH->apply_($C);$iI->apply_($C);$sI->apply_($C);$DI->apply_($C);$SI->apply_($d4);$cJ->apply_($d4);$vJ->apply_($e4);$KJ->apply_($e4);$RJ->apply_($e4);$mK->apply_($H);$sK->apply_($H);$yK->apply_($H);$MK->apply_($dp);$ni::self=$fM;&$V($T);&$V($f1);&$V($l1);&$V($y1);&$V($L1);&$V($Y1);&$V($n2);&$V($J2);&$V($P2);&$V($E4);&$V($G4);&$I4($G4);&$V($P4);&$V($l5);&$V($t5);&$V($F5);&$V($R5);&$V($f6);&$V($h6);&$I4($h6);&$V($n6);&$V($E6);&$V($P6);&$V($d7);&$V($p7);&$V($J7);&$V($L7);&$I4($L7);&$V($g8);&$V($n8);&$V($y8);&$V($A8);&$I4($A8);&$V($G8);&$V($Q8);&$V($W8);&$V($f9);&$V($m9);&$V($t9);&$V($B9);&$V($D9);&$I4($D9);&$V($J9);&$V($fa);&$V($wa);&$V($Ka);&$V($kb);&$V($sb);&$V($Qb);&$V($ic);&$V($yc);&$V($Sc);&$V($cd);&$V($ed);&$I4($ed);&$V($kd);&$V($sd);&$V($Dd);&$V($Sd);&$V($Yd);&$V($ce);&$I4($ce);&$V($xe);&$V($De);&$V($Re);&$V($Te);&$V($Ze);&$V($kf);&$V($uf);&$V($Bf);&$V($If);&$V($Pf);&$V($Wf);&$V($Yf);&$V($cg);&$I4($cg);&$V($jg);&$V($qg);&$V($sg);&$I4($sg);&$V($Bg);&$I4($Bg);&$V($Fg);&$I4($Fg);&$V($Hg);&$I4($Hg);&$V($Jg);&$I4($Jg);&$V($Og);&$I4($Og);&$V($Sg);&$I4($Sg);&$V($dh);&$V($vh);&$V($xh);&$I4($xh);&$V($Kh);&$V($Mh);&$I4($Mh);&$V($Oh);&$I4($Oh);&$V($Zh);&$V($ii);&$V($ki);&$I4($ki);&$V($pi);&$I4($pi);&$V($Bi);&$V($Ii);&$V($ej);&$V($yj);&$V($Gj);&$V($Wj);&$V($Yj);&$I4($Yj);&$V($fk);&$I4($fk);&$V($vk);&$V($Ck);&$V($Kk);&$V($cl);&$V($nl);&$V($Bl);&$V($El);&$I4($El);&$Hl($El);&$V($gm);&$V($im);&$I4($im);&$V($ym);&$V($Gm);&$V($Nm);&$V($fn);&$V($nn);&$V($In);&$V($Kn);&$I4($Kn);&$V($Pn);&$I4($Pn);&$V($go);&$V($po);&$V($ro);&$I4($ro);&$V($wo);&$I4($wo);&$V($cp);&$V($np);&$V($Ap);&$V($Cp);&$I4($Cp);&$V($Hp);&$I4($Hp);&$V($Yp);&$I4($Yp);&$V($yq);&$V($Fq);&$V($Tq);&$V($Vq);&$I4($Vq);&$V($cr);&$I4($cr);&$V($rr);&$V($Dr);&$V($Fr);&$I4($Fr);&$V($Sr);&$V($os);&$V($qs);&$I4($qs);&$ts($qs);&$V($As);&$I4($As);&$V($Es);&$V($Gs);&$I4($Gs);&$V($ct);&$V($jt);&$V($tt);&$V($Ft);&$V($Kt);&$I4($Kt);&$ts($Kt);&$Nt($Kt);&$V($iu);&$V($ku);&$I4($ku);&$V($Bu);&$V($Iu);&$V($Ku);&$I4($Ku);&$ts($Ku);&$V($Pu);&$I4($Pu);&$V($Wu);&$I4($Wu);&$V($kv);&$V($mv);&$I4($mv);&$V($sv);&$I4($sv);&$V($Bv);&$I4($Bv);&$V($Ov);&$V($Uv);&$V($gw);&$V($ow);&$V($uw);&$V($Aw);&$V($Kw);&$V($Mw);&$I4($Mw);&$V($Ww);&$V($Yw);&$I4($Yw);&$V($Cx);&$V($Ux);&$V($fy);&$V($my);&$V($oy);&$I4($oy);&$ry($oy);&$V($Cy);&$V($Ey);&$I4($Ey);&$V($Vy);&$V($ez);&$V($yz);&$V($Sz);&$V($Uz);&$I4($Uz);&$V($Zz);&$I4($Zz);&$V($tA);&$V($zA);&$I4($zA);&$V($iB);&$V($EB);&$V($XB);&$V($rC);&$V($MC);&$V($VC);&$V($hD);&$V($jD);&$I4($jD);&$V($oD);&$I4($oD);&$V($nE);&$V($wE);&$V($yE);&$I4($yE);&$V($DE);&$I4($DE);&$V($SE);&$V($dF);&$V($fF);&$I4($fF);&$V($kF);&$I4($kF);&$V($wF);&$I4($wF);&$V($SF);&$V($eG);&$V($kG);&$V($mG);&$I4($mG);&$V($sG);&$I4($sG);&$V($GG);&$V($IG);&$I4($IG);&$V($QG);&$V($XG);&$V($nH);&$V($pH);&$I4($pH);&$V($wH);&$I4($wH);&$V($yH);&$I4($yH);&$V($SH);&$V($ZH);&$V($iI);&$V($sI);&$V($DI);&$V($FI);&$I4($FI);&$II($FI);&$V($SI);&$V($cJ);&$V($eJ);&$I4($eJ);&$V($vJ);&$V($KJ);&$V($RJ);&$V($TJ);&$I4($TJ);&$V($YJ);&$I4($YJ);&$V($mK);&$V($sK);&$V($yK);&$V($AK);&$I4($AK);&$V($FK);&$I4($FK);&$V($MK);&$V($UK);&$I4($UK);&$V($ZK);&$I4($ZK);&$V($kL);&$I4($kL);&$V($pL);&$I4($pL);&$V($uL);&$I4($uL);&$V($CL);&$I4($CL);&$V($GL);&$I4($GL);ni->run(@ARGV);
__DATA__
