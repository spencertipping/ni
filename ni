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
  $_[0];
} else {
  shift->{'mode'};
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
$self->success;#;$sr=bless({$t,$qr,$v,$q,$w,$rr,$y,$z},$A);$tr={$iq,$sr};$ur=q#/io/transfer_sync_run.b#;$vr=bless({$T2,$pr,$u4,$q,$v4,$q,$w4,$tr,$Q,$ur},$T3);$wr=[$dp,$or,$vr];$xr=bless({$T2,$gr,$Q,$hr,$i3,$wr},$A3);$yr=q#ni:/io/transfer_sync.c#;$zr={$A3,1};$Ar=q#/io/transfer_sync.c#;$Br=[$tp];$Cr=bless({$T2,$zr,$Q,$Ar,$i3,$Br},$h4);$Dr=q#ni:/io/transfer_sync_init.b#;$Er=q#ni:/io/transfer_sync_run.b#;$Fr=q#ni:/lib#;$Gr=q#lib#;$Hr={$Gr,1};$Ir=[];$Jr=bless({$T2,$Hr,$Q,$m8,$i3,$Ir},$j4);$Kr=q#ni:/lib/array_encoding.b#;$Lr={};$Mr=q#/lib/array_encoding.b#;$Nr=[];$Or=bless({$T2,$Lr,$Q,$Mr,$i3,$Nr},$D3);$Pr=q#ni:/lib/behavior#;$Qr=q#ni:/lib/behavior.c#;$Rr={$k3,1,$C3,1,$E3,1,$G3,1,$U3,1,$W3,1,$k4,1,$o4,1};$Sr=[$rd];$Tr=bless({$T2,$Rr,$Q,$sd,$i3,$Sr},$h4);$Ur=q#ni:/lib/branch#;$Vr={$D3,1};$Wr=q#/lib/branch#;$Xr={};$Yr=q#add#;$Zr=q#local $_;
my $self = shift;
push @{$$self{slices}}, @_;
for my $p (sort keys %{$$self{applied_to}}) {
  $self->resolve($_)->apply($p) for @_;
}
$self;#;$cs=bless({$w,$Zr,$y,$z},$A);$ds=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$self->resolve($_)->apply($p) for @{$$self{slices}};
$self;#;$es=bless({$w,$ds,$y,$z},$A);$fs={$Yr,$cs,$I8,$es};$gs=bless({$T2,$Xr,$u4,$q,$v4,$q,$w4,$fs,$Q,$Fc},$T3);$hs={};$is=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [@_]};#;$js=bless({$w,$is,$y,$z},$A);$ks={$J6,$js};$ls=q#/lib/branch_init.b#;$ms=bless({$T2,$hs,$u4,$q,$v4,$q,$w4,$ks,$Q,$ls},$T3);$ns=[$A8,$G8,$gs,$ms,$Oc];$os=bless({$T2,$Vr,$Q,$Wr,$i3,$ns},$E3);$ps=q#ni:/lib/branch.b#;$qs=q#ni:/lib/branch.c#;$rs={$E3,1};$ss=q#/lib/branch.c#;$ts=[$Tr];$us=bless({$T2,$rs,$Q,$ss,$i3,$ts},$h4);$vs=q#ni:/lib/branch_init.b#;$ws=q#ni:/lib/class_init.b#;$xs=q#ni:/lib/dataslice#;$ys=q#ni:/lib/dataslice.b#;$zs=q#ni:/lib/dataslice.c#;$As={$G3,1};$Bs=q#/lib/dataslice.c#;$Cs=[$Tr];$Ds=bless({$T2,$As,$Q,$Bs,$i3,$Cs},$h4);$Es=q#ni:/lib/dataslice_init.b#;$Fs=q#ni:/lib/definition.b#;$Gs={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Hs={};$Is=q#def#;$Js=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Ks=bless({$w,$Js,$y,$z},$A);$Ls={$Is,$Ks};$Ms=q#/lib/definition_def.b#;$Ns=bless({$T2,$Hs,$u4,$q,$v4,$q,$w4,$Ls,$Q,$Ms},$T3);$Os=q#/lib/hash_encoding.b#;$Ps={};$Qs=q#(""#;$Rs=q#shift->name#;$Ss=bless({$w,$Rs,$y,$z},$A);$Ts={$Qs,$Ss};$Us=q#/lib/name_as_string.b#;$Vs=bless({$T2,$Ps,$u4,$q,$v4,$q,$w4,$Ts,$Q,$Us},$T3);$Ws={};$Xs=q#(eq#;$Ys=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Zs=bless({$w,$Ys,$y,$z},$A);$ct={$Xs,$Zs};$dt=q#/lib/ref_eq.b#;$et=bless({$T2,$Ws,$u4,$q,$v4,$q,$w4,$ct,$Q,$dt},$T3);$ft={};$gt=q#defdata#;$ht=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$it=bless({$w,$ht,$y,$z},$A);$jt={$gt,$it};$kt=q#/lib/definition_defdata.b#;$lt=bless({$T2,$ft,$u4,$q,$v4,$q,$w4,$jt,$Q,$kt},$T3);$mt=[$Ns,$Os,$Vs,$et,$lt];$nt=bless({$T2,$Gs,$Q,$Oc,$i3,$mt},$D3);$ot=q#ni:/lib/definition_def.b#;$pt=q#ni:/lib/definition_defdata.b#;$qt=q#ni:/lib/doc#;$rt={$S,1};$st={};$tt=q#shift; +{name => shift, doc => []}#;$ut=bless({$w,$tt,$y,$z},$A);$vt={$J6,$ut};$wt=q#/lib/doc_init.b#;$xt=bless({$T2,$st,$u4,$q,$v4,$q,$w4,$vt,$Q,$wt},$T3);$yt={};$zt=q#namespace#;$At=q#'ni.doc'#;$Bt=bless({$w,$At,$y,$z},$A);$Ct={$zt,$Bt};$Dt=q#/lib/doc_namespace.b#;$Et=bless({$T2,$yt,$u4,$q,$v4,$q,$w4,$Ct,$Q,$Dt},$T3);$Ft={};$Gt=q#(@{}#;$Ht=q#[map @$_, @{shift->{doc}}]#;$It=bless({$w,$Ht,$y,$z},$A);$Jt=q#AUTOLOAD#;$Kt=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$Lt=bless({$w,$Kt,$y,$z},$A);$Mt={$Gt,$It,$Jt,$Lt};$Nt=q#/lib/doc_define.b#;$Ot=bless({$T2,$Ft,$u4,$q,$v4,$q,$w4,$Mt,$Q,$Nt},$T3);$Pt={};$Qt=q#shift->referent#;$Rt=bless({$w,$Qt,$y,$z},$A);$St=q#ni 'ni:' . shift->{name}#;$Tt=bless({$w,$St,$y,$z},$A);$Ut={$Zm,$Rt,$S2,$Tt};$Vt=q#/lib/doc_end.b#;$Wt=bless({$T2,$Pt,$u4,$q,$v4,$q,$w4,$Ut,$Q,$Vt},$T3);$Xt={};$Yt=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Zt=bless({$w,$Yt,$y,$z},$A);$cu={$R2,$Zt};$du=q#/lib/doc_TODO.b#;$eu=bless({$T2,$Xt,$u4,$q,$v4,$q,$w4,$cu,$Q,$du},$T3);$fu={};$gu=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$hu=bless({$w,$gu,$y,$z},$A);$iu={$p6,$hu};$ju=q#/lib/doc_eg.b#;$ku=bless({$T2,$fu,$u4,$q,$v4,$q,$w4,$iu,$Q,$ju},$T3);$lu={};$mu=q#tests#;$nu=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case',
    map @$_, @{$$self{doc}};#;$ou=bless({$w,$nu,$y,$z},$A);$pu=q#todos#;$qu=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo',
    map @$_, @{$$self{doc}};#;$ru=bless({$w,$qu,$y,$z},$A);$su={$mu,$ou,$pu,$ru};$tu=q#/lib/doc_process.b#;$uu=bless({$T2,$lu,$u4,$q,$v4,$q,$w4,$su,$Q,$tu},$T3);$vu=[$G4,$G8,$xt,$Et,$Ot,$Wt,$eu,$ku,$uu];$wu=bless({$T2,$rt,$Q,$D9,$i3,$vu},$H3);$xu=q#ni:/lib/doc.c#;$yu={$H3,1};$zu=q#/lib/doc.c#;$Au={};$Bu=q#defannotation#;$Cu=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Du=bless({$w,$Cu,$y,$z},$A);$Eu={$Bu,$Du};$Fu=q#/lib/doc.c_defannotation.b#;$Gu=bless({$T2,$Au,$u4,$q,$v4,$q,$w4,$Eu,$Q,$Fu},$T3);$Hu=[$rd,$Gu];$Iu=bless({$T2,$yu,$Q,$zu,$i3,$Hu},$h4);$Ju=q#ni:/lib/doc.c_defannotation.b#;$Ku=q#ni:/lib/doc_TODO.b#;$Lu=q#ni:/lib/doc_define.b#;$Mu=q#ni:/lib/doc_eg.b#;$Nu=q#ni:/lib/doc_end.b#;$Ou=q#ni:/lib/doc_init.b#;$Pu=q#ni:/lib/doc_namespace.b#;$Qu=q#ni:/lib/doc_process.b#;$Ru=q#ni:/lib/documentable.b#;$Su=q#ni:/lib/fn#;$Tu={$A,1};$Uu=q#/lib/fn#;$Vu=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$Wu=bless({$w,$Vu,$y,$z},$A);$Xu=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$Yu=bless({$w,$Xu},$A);$Zu=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$cv=bless({$w,$Zu},$A);$dv=q#lib/fn::closure#;$ev=q#lib/fn::compile#;$fv=q#lib/fn::instantiate#;$gv={};$hv=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$iv=bless({$w,$hv,$y,$z},$A);$jv=q#compile#;$kv={$v,$Wu,$jv,$Yu,$J6,$cv};$lv=q#/lib/fn_init.b#;$mv=bless({$T2,$gv,$u4,$q,$v4,$iv,$w4,$kv,$Q,$lv},$T3);$nv={};$ov=[];$pv=q#shift->{'annotations'}#;$qv=bless({$t,$ov,$v,$q,$w,$pv,$y,$z},$A);$rv=[];$sv=q#shift->{'code'}#;$tv=bless({$t,$rv,$v,$q,$w,$sv,$y,$z},$A);$uv=q#eval_number#;$vv=[];$wv=q#shift->{'eval_number'}#;$xv=bless({$t,$vv,$v,$q,$w,$wv,$y,$z},$A);$yv=q#fn#;$zv=[];$Av=q#shift->{'fn'}#;$Bv=bless({$t,$zv,$v,$q,$w,$Av,$y,$z},$A);$Cv={$t,$qv,$w,$tv,$uv,$xv,$yv,$Bv};$Dv=q#/lib/fn_ro.b#;$Ev=bless({$T2,$nv,$u4,$q,$v4,$q,$w4,$Cv,$Q,$Dv},$T3);$Fv={};$Gv=[];$Hv=q#my $self = shift; "fn {$$self{code}}"#;$Iv=bless({$t,$Gv,$v,$q,$w,$Hv,$y,$z},$A);$Jv=[];$Kv=bless({$t,$Jv,$v,$q,$w,$Ys,$y,$z},$A);$Lv={$Qs,$Iv,$Xs,$Kv};$Mv=q#/lib/fn_ops.b#;$Nv=bless({$T2,$Fv,$u4,$q,$v4,$q,$w4,$Lv,$Q,$Mv},$T3);$Ov={};$Pv=[];$Qv=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Rv=bless({$t,$Pv,$v,$q,$w,$Qv,$y,$z},$A);$Sv={$Pb,$Rv};$Tv=q#/lib/fn_serialize.b#;$Uv=bless({$T2,$Ov,$u4,$q,$v4,$q,$w4,$Sv,$Q,$Tv},$T3);$Vv=[$G4,$Xc,$mv,$Ev,$Nv,$Uv];$Wv=bless({$T2,$Tu,$Q,$Uu,$i3,$Vv},$I3);$Xv=[];$Yv=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Zv=bless({$t,$Xv,$v,$q,$w,$Yv,$y,$z},$A);$cw=q#ni:/lib/fn.c#;$dw={$I3,1};$ew=q#/lib/fn.c#;$fw={};$gw=q#resolve_evals#;$hw=[];$iw=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$jw=bless({$t,$hw,$v,$q,$w,$iw,$y,$z},$A);$kw={$gw,$jw};$lw=q#/lib/fn.c_resolve_eval.b#;$mw=bless({$T2,$fw,$u4,$Zv,$v4,$q,$w4,$kw,$Q,$lw},$T3);$nw=[$rd,$mw];$ow=bless({$T2,$dw,$Q,$ew,$i3,$nw},$h4);$pw=q#ni:/lib/fn.c_resolve_eval.b#;$qw=q#ni:/lib/fn_init.b#;$rw=q#ni:/lib/fn_ops.b#;$sw=q#ni:/lib/fn_ro.b#;$tw=q#ni:/lib/fn_serialize.b#;$uw=q#ni:/lib/future#;$vw={$J3,1};$ww={};$xw=[];$yw=bless({$t,$xw,$v,$q,$w,$Yn,$y,$z},$A);$zw=q#parents#;$Aw=[];$Bw=q#shift->{'parents'}#;$Cw=bless({$t,$Aw,$v,$q,$w,$Bw,$y,$z},$A);$Dw={$r,$yw,$zw,$Cw};$Ew=q#/lib/future_ro.b#;$Fw=bless({$T2,$ww,$u4,$q,$v4,$q,$w4,$Dw,$Q,$Ew},$T3);$Gw={};$Hw=[];$Iw=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Jw=bless({$t,$Hw,$v,$q,$w,$Iw,$y,$z},$A);$Kw={$J6,$Jw};$Lw=q#/lib/future_init.b#;$Mw=bless({$T2,$Gw,$u4,$q,$v4,$q,$w4,$Kw,$Q,$Lw},$T3);$Nw={};$Ow=q#decide#;$Pw=[];$Qw=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Rw=bless({$t,$Pw,$v,$q,$w,$Qw,$y,$z},$A);$Sw=q#decided#;$Tw=[];$Uw=q#shift->{outcome}#;$Vw=bless({$t,$Tw,$v,$q,$w,$Uw,$y,$z},$A);$Ww=q#listener#;$Xw=[];$Yw=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$Zw=bless({$t,$Xw,$v,$q,$w,$Yw,$y,$z},$A);$cx=q#v#;$dx=[];$ex=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$fx=bless({$t,$dx,$v,$q,$w,$ex,$y,$z},$A);$gx={$Ow,$Rw,$Sw,$Vw,$Ww,$Zw,$cx,$fx};$hx=q#/lib/future_state.b#;$ix=bless({$T2,$Nw,$u4,$q,$v4,$q,$w4,$gx,$Q,$hx},$T3);$jx={};$kx=q#and#;$lx=[];$mx=q#my $self   = $_[0];
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
$child;#;$nx=bless({$t,$lx,$v,$q,$w,$mx,$y,$z},$A);$ox=q#flatmap#;$px=[];$qx=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$rx=bless({$t,$px,$v,$q,$w,$qx,$y,$z},$A);$sx=q#map#;$tx=[];$ux=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$vx=bless({$t,$tx,$v,$q,$w,$ux,$y,$z},$A);$wx=q#or#;$xx=[];$yx=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$zx=bless({$t,$xx,$v,$q,$w,$yx,$y,$z},$A);$Ax={$kx,$nx,$ox,$rx,$sx,$vx,$wx,$zx};$Bx=q#/lib/future_algebra.b#;$Cx=bless({$T2,$jx,$u4,$q,$v4,$q,$w4,$Ax,$Q,$Bx},$T3);$Dx=[$G4,$Fw,$Mw,$ix,$Cx];$Ex=bless({$T2,$vw,$Q,$U9,$i3,$Dx},$K3);$Fx=q#ni:/lib/future.c#;$Gx={$K3,1};$Hx=q#/lib/future.c#;$Ix=[$rd];$Jx=bless({$T2,$Gx,$Q,$Hx,$i3,$Ix},$h4);$Kx=q#ni:/lib/future_algebra.b#;$Lx=q#ni:/lib/future_init.b#;$Mx=q#ni:/lib/future_ro.b#;$Nx=q#ni:/lib/future_state.b#;$Ox=q#ni:/lib/gensym_generator_compact.b#;$Px={};$Qx=q#gensym#;$Rx=[];$Sx=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Tx=bless({$t,$Rx,$v,$q,$w,$Sx,$y,$z},$A);$Ux={$Qx,$Tx};$Vx=bless({$T2,$Px,$u4,$q,$v4,$q,$w4,$Ux,$Q,$Pa},$T3);$Wx=q#ni:/lib/global_static_test.b#;$Xx={};$Yx=[];$Zx=q#ni('ni:/lib/test_case')->new(shift)#;$cy=q#($)#;$dy=bless({$t,$Yx,$v,$q,$w,$Zx,$y,$cy},$A);$ey=q#now#;$fy=[];$gy=q#ni('ni:/lib/test_value')->new(shift)#;$hy=bless({$t,$fy,$v,$q,$w,$gy,$y,$cy},$A);$iy={$p6,$dy,$ey,$hy};$jy=q#/lib/global_static_test.b#;$ky=bless({$T2,$Xx,$u4,$q,$v4,$q,$w4,$iy,$Q,$jy},$T3);$ly=q#ni:/lib/hash_accessor.b#;$my={};$ny=q#ro#;$oy=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$py=bless({$w,$oy,$y,$z},$A);$qy=q#rw#;$ry=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      \\$_[0];
    } else {
      shift->{'$_'};
    }
  }), @as);#;$sy=bless({$w,$ry,$y,$z},$A);$ty={$ny,$py,$qy,$sy};$uy=q#/lib/hash_accessor.b#;$vy=bless({$T2,$my,$u4,$q,$v4,$q,$w4,$ty,$Q,$uy},$T3);$wy=q#ni:/lib/hash_encoding.b#;$xy={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$yy={};$zy=q#instantiate_with_defaults#;$Ay=q#my ($class, @slots) = @_;
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
    }));#;$By=bless({$w,$Ay,$y,$z},$A);$Cy={$zy,$By};$Dy=q#/lib/hash_encoding_init_with_defaults.b#;$Ey=bless({$T2,$yy,$u4,$q,$v4,$q,$w4,$Cy,$Q,$Dy},$T3);$Fy=[$vy,$Ey];$Gy=bless({$T2,$xy,$Q,$Os,$i3,$Fy},$D3);$Hy=q#ni:/lib/hash_encoding_init_with_defaults.b#;$Iy=q#ni:/lib/image#;$Jy=q#ni:/lib/image.c#;$Ky={$M3,1};$Ly=q#/lib/image.c#;$My=[$rd];$Ny=bless({$T2,$Ky,$Q,$Ly,$i3,$My},$h4);$Oy=q#ni:/lib/image_init.b#;$Py=q#ni:/lib/image_quoting.b#;$Qy=q#ni:/lib/instance.b#;$Ry=q#ni:/lib/instantiable.b#;$Sy=q#ni:/lib/json.b#;$Ty={};$Uy=q#json_decode#;$Vy=[];$Wy=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Xy=bless({$t,$Vy,$v,$q,$w,$Wy,$y,$cy},$A);$Yy=q#json_encode#;$Zy=[];$cz=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$dz=bless({$t,$Zy,$v,$q,$w,$cz,$y,$cy},$A);$ez=q#json_encode_pretty#;$fz=[];$gz=q#local $_;
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

ni::json_encode($v);#;$hz=bless({$t,$fz,$v,$q,$w,$gz,$y,$z},$A);$iz=q#json_escape#;$jz=[];$kz=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$lz=bless({$t,$jz,$v,$q,$w,$kz,$y,$cy},$A);$mz=q#json_unescape#;$nz=[];$oz=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$pz=bless({$t,$nz,$v,$q,$w,$oz,$y,$cy},$A);$qz=q#json_unescape_one#;$rz=[];$sz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$tz=bless({$t,$rz,$v,$q,$w,$sz,$y,$cy},$A);$uz={$Uy,$Xy,$Yy,$dz,$ez,$hz,$iz,$lz,$mz,$pz,$qz,$tz};$vz=q#/lib/json.b#;$wz=bless({$T2,$Ty,$u4,$q,$v4,$q,$w4,$uz,$Q,$vz},$T3);$xz=q#ni#;$yz=q#ni:/lib/json_data.b#;$zz={};$Az=q#json_escapes#;$Bz=q##;$Cz=q#b#;$Dz=q#	#;$Ez=q#t#;$Fz=q#
#;$Gz=q#n#;$Hz=q##;$Iz=q#f#;$Jz=q##;$Kz=q#"#;$Lz=q#/#;$Mz=q#\\#;$Nz={$Bz,$Cz,$Dz,$Ez,$Fz,$Gz,$Hz,$Iz,$Jz,$fk,$Kz,$Kz,$Lz,$Lz,$Mz,$Mz};$Oz=q#json_unescapes#;$Pz={$Kz,$Kz,$Lz,$Lz,$Mz,$Mz,$Cz,$Bz,$Iz,$Hz,$Gz,$Fz,$fk,$Jz,$Ez,$Dz};$Qz={$Az,$Nz,$Oz,$Pz};$Rz=q#/lib/json_data.b#;$Sz=bless({$T2,$zz,$Vm,$Qz,$Q,$Rz},$F3);$Tz=q#ni:/lib/name_as_string.b#;$Uz=q#ni:/lib/named.b#;$Vz=q#ni:/lib/named_in_ni.b#;$Wz={};$Xz=q#'ni'#;$Yz=bless({$w,$Xz,$y,$z},$A);$Zz={$zt,$Yz};$cA=bless({$T2,$Wz,$u4,$q,$v4,$q,$w4,$Zz,$Q,$X8},$T3);$dA=q#ni:/lib/namespaced.b#;$eA={};$fA=q#package#;$gA=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$hA=bless({$w,$gA,$y,$z},$A);$iA={$fA,$hA};$jA=bless({$T2,$eA,$u4,$q,$v4,$q,$w4,$iA,$Q,$Y8},$T3);$kA=q#ni:/lib/ni#;$lA={$N3,1};$mA={};$nA=q#extend#;$oA=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$pA=bless({$w,$oA,$y,$z},$A);$qA=q#is_mutable#;$rA=q#$0 ne '-' && -w $0#;$sA=bless({$w,$rA,$y,$z},$A);$tA=q#modify#;$uA=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$vA=bless({$w,$uA,$y,$z},$A);$wA={$nA,$pA,$qA,$sA,$tA,$vA};$xA=q#/lib/ni_self.b#;$yA=bless({$T2,$mA,$u4,$q,$v4,$q,$w4,$wA,$Q,$xA},$T3);$zA={};$AA=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$BA=bless({$w,$AA,$y,$z},$A);$CA=q#docs#;$DA=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$EA=bless({$w,$DA,$y,$z},$A);$FA=q#metaclasses#;$GA=q#my $self = shift;
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$HA=bless({$w,$GA,$y,$z},$A);$IA=q#undocumented#;$JA=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$KA=bless({$w,$JA,$y,$z},$A);$LA=q#untested#;$MA=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$NA=bless({$w,$MA,$y,$z},$A);$OA={$K,$BA,$CA,$EA,$FA,$HA,$IA,$KA,$LA,$NA};$PA=q#/lib/ni_dev_introspection.b#;$QA=bless({$T2,$zA,$u4,$q,$v4,$q,$w4,$OA,$Q,$PA},$T3);$RA={};$SA=q#--internal/+=#;$TA=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$UA=bless({$w,$TA,$y,$z},$A);$VA=q#--internal/dev-state#;$WA=q#my $self = shift;
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
0;#;$XA=bless({$w,$WA,$y,$z},$A);$YA=q#--internal/eval#;$ZA=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$cB=bless({$w,$ZA,$y,$z},$A);$dB=q#--internal/image#;$eB=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$fB=bless({$w,$eB,$y,$z},$A);$gB=q#--internal/test#;$hB=q#local $| = 1;
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
!!$failed;#;$iB=bless({$w,$hB,$y,$z},$A);$jB=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$kB=bless({$w,$jB,$y,$z},$A);$lB={$SA,$UA,$VA,$XA,$YA,$cB,$dB,$fB,$gB,$iB,$iq,$kB};$mB=q#/lib/ni_main.b#;$nB=bless({$T2,$RA,$u4,$q,$v4,$q,$w4,$lB,$Q,$mB},$T3);$oB={};$pB=q#resolve#;$qB=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$rB=bless({$w,$qB,$y,$z},$A);$sB=q#resolver_for#;$tB=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$uB=bless({$w,$tB,$y,$z},$A);$vB={$pB,$rB,$sB,$uB};$wB=q#/lib/ni_resolver.b#;$xB=bless({$T2,$oB,$u4,$q,$v4,$q,$w4,$vB,$Q,$wB},$T3);$yB={};$zB=q#exists#;$AB=q#exists $_[0]->{named}{$_[1]}#;$BB=bless({$w,$AB,$y,$z},$A);$CB=q#quoted#;$DB=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$EB=bless({$w,$DB,$y,$z},$A);$FB={$zB,$BB,$CB,$EB};$GB=q#/lib/ni_image.b#;$HB=bless({$T2,$yB,$u4,$q,$v4,$q,$w4,$FB,$Q,$GB},$T3);$IB=[$G4,$yA,$QA,$nB,$xB,$HB];$JB=bless({$T2,$lA,$Q,$gb,$i3,$IB},$O3);$KB=q#ni:/lib/ni.c#;$LB={$O3,1};$MB=q#/lib/ni.c#;$NB=[$rd];$OB=bless({$T2,$LB,$Q,$MB,$i3,$NB},$h4);$PB=q#ni:/lib/ni_dev_introspection.b#;$QB=q#ni:/lib/ni_image.b#;$RB=q#ni:/lib/ni_main.b#;$SB=q#ni:/lib/ni_resolver.b#;$TB=q#ni:/lib/ni_self.b#;$UB=q#ni:/lib/ni_static_util.b#;$VB={};$WB=q#abbrev#;$XB=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$YB=bless({$w,$XB,$y,$z},$A);$ZB=q#dor#;$cC=q#defined $_[0] ? $_[0] : $_[1]#;$dC=bless({$w,$cC,$y,$z},$A);$eC=q#indent#;$fC=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$gC=bless({$w,$fC,$y,$z},$A);$hC=q#max#;$iC=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$jC=bless({$w,$iC,$y,$z},$A);$kC=q#maxstr#;$lC=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$mC=bless({$w,$lC,$y,$z},$A);$nC=q#mean#;$oC=q#sum(@_) / (@_ || 1)#;$pC=bless({$w,$oC,$y,$z},$A);$qC=q#min#;$rC=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$sC=bless({$w,$rC,$y,$z},$A);$tC=q#minstr#;$uC=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$vC=bless({$w,$uC,$y,$z},$A);$wC=q#outdent#;$xC=q#my $x = shift;
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
join "\\n", @lines;#;$yC=bless({$w,$xC,$y,$z},$A);$zC=q#sgr#;$AC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$BC=bless({$w,$AC,$y,$z},$A);$CC=q#sr#;$DC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$EC=bless({$w,$DC,$y,$z},$A);$FC=q#sum#;$GC=q#local $_; my $x = 0; $x += $_ for @_; $x#;$HC=bless({$w,$GC,$y,$z},$A);$IC=q#swap#;$JC=q#@_[0, 1] = @_[1, 0]#;$KC=bless({$w,$JC,$y,$z},$A);$LC={$WB,$YB,$ZB,$dC,$eC,$gC,$hC,$jC,$kC,$mC,$nC,$pC,$qC,$sC,$tC,$vC,$wC,$yC,$zC,$BC,$CC,$EC,$FC,$HC,$IC,$KC};$MC=q#/lib/ni_static_util.b#;$NC=bless({$T2,$VB,$u4,$q,$v4,$q,$w4,$LC,$Q,$MC},$T3);$OC=q#ni:/lib/object_metadata#;$PC={$P3,1,$C,1,$H,1};$QC=q#/lib/object_metadata#;$RC={};$SC=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  $_[0];
} else {
  shift->{'referent'};
}#;$TC=bless({$w,$SC,$y,$z},$A);$UC={$S2,$TC};$VC=q#/lib/object_metadata_rw.b#;$WC=bless({$T2,$RC,$u4,$q,$v4,$q,$w4,$UC,$Q,$VC},$T3);$XC=[$G4,$WC];$YC=bless({$T2,$PC,$Q,$QC,$i3,$XC},$Q3);$ZC=q#ni:/lib/object_metadata.c#;$cD={$Q3,1,$d4,1,$g4,1};$dD=q#/lib/object_metadata.c#;$eD=[$rd];$fD=bless({$T2,$cD,$Q,$dD,$i3,$eD},$h4);$gD=q#ni:/lib/object_metadata_rw.b#;$hD=q#ni:/lib/packed_encoding.b#;$iD={};$jD=q#/lib/packed_encoding.b#;$kD=[];$lD=bless({$T2,$iD,$Q,$jD,$i3,$kD},$D3);$mD=q#ni:/lib/perlbranch.b#;$nD=q#ni:/lib/quote_circular_addressed.b#;$oD={};$pD=q#circular_arrayref#;$qD=[];$rD=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$sD=bless({$t,$qD,$v,$q,$w,$rD,$y,$z},$A);$tD=q#circular_hashref#;$uD=[];$vD=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$wD=bless({$t,$uD,$v,$q,$w,$vD,$y,$z},$A);$xD=q#is_circular#;$yD=[];$zD=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$AD=bless({$t,$yD,$v,$q,$w,$zD,$y,$z},$A);$BD={$pD,$sD,$tD,$wD,$xD,$AD};$CD=bless({$T2,$oD,$u4,$q,$v4,$q,$w4,$BD,$Q,$Na},$T3);$DD=q#ni:/lib/quote_code_fail.b#;$ED={};$FD=q#quote_code#;$GD=[];$HD=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$ID=bless({$t,$GD,$v,$q,$w,$HD,$y,$z},$A);$JD={$FD,$ID};$KD=bless({$T2,$ED,$u4,$q,$v4,$q,$w4,$JD,$Q,$Ka},$T3);$LD=q#ni:/lib/quote_gensym_identity.b#;$MD={};$ND=q#address#;$OD=[];$PD=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$QD=bless({$t,$OD,$v,$q,$w,$PD,$y,$z},$A);$RD=q#allocate_gensym#;$SD=[];$TD=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$UD=bless({$t,$SD,$v,$q,$w,$TD,$y,$z},$A);$VD=q#circular_links#;$WD=[];$XD=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$YD=bless({$t,$WD,$v,$q,$w,$XD,$y,$z},$A);$ZD=q#quote#;$cE=[];$dE=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$eE=bless({$t,$cE,$v,$q,$w,$dE,$y,$z},$A);$fE={$ND,$QD,$RD,$UD,$VD,$YD,$ZD,$eE};$gE=bless({$T2,$MD,$u4,$q,$v4,$q,$w4,$fE,$Q,$Oa},$T3);$hE=q#ni:/lib/quote_objects.b#;$iE={};$jE=q#quote_blessed#;$kE=[];$lE=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$mE=bless({$t,$kE,$v,$q,$w,$lE,$y,$z},$A);$nE=q#quote_class#;$oE=[];$pE=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$qE=bless({$t,$oE,$v,$q,$w,$pE,$y,$z},$A);$rE=q#quote_object#;$sE=[];$tE=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$uE=bless({$t,$sE,$v,$q,$w,$tE,$y,$z},$A);$vE={$jE,$mE,$nE,$qE,$rE,$uE};$wE=bless({$T2,$iE,$u4,$q,$v4,$q,$w4,$vE,$Q,$Ma},$T3);$xE=q#ni:/lib/quote_simple#;$yE={$R3,1};$zE={};$AE=[];$BE=q#+{}#;$CE=bless({$t,$AE,$v,$q,$w,$BE,$y,$z},$A);$DE={$J6,$CE};$EE=q#/lib/quote_simple_init.b#;$FE=bless({$T2,$zE,$u4,$q,$v4,$q,$w4,$DE,$Q,$EE},$T3);$GE={};$HE=[];$IE=bless({$t,$HE,$v,$q,$w,0,$y,$z},$A);$JE=[];$KE=q#shift->quote_value(shift)#;$LE=bless({$t,$JE,$v,$q,$w,$KE,$y,$z},$A);$ME={$xD,$IE,$ZD,$LE};$NE=q#/lib/quote_simple_quote.b#;$OE=bless({$T2,$GE,$u4,$q,$v4,$q,$w4,$ME,$Q,$NE},$T3);$PE={};$QE=q#quote_array#;$RE=[];$SE=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$TE=bless({$t,$RE,$v,$q,$w,$SE,$y,$z},$A);$UE=q#quote_hash#;$VE=[];$WE=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$XE=bless({$t,$VE,$v,$q,$w,$WE,$y,$z},$A);$YE=q#quote_scalar#;$ZE=[];$cF=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$dF=bless({$t,$ZE,$v,$q,$w,$cF,$y,$z},$A);$eF=q#quote_scalar_ref#;$fF=[];$gF=q#'\\\\' . shift->quote(${$_[0]})#;$hF=bless({$t,$fF,$v,$q,$w,$gF,$y,$z},$A);$iF=q#quote_value#;$jF=[];$kF=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$lF=bless({$t,$jF,$v,$q,$w,$kF,$y,$z},$A);$mF={$QE,$TE,$UE,$XE,$YE,$dF,$eF,$hF,$iF,$lF};$nF=bless({$T2,$PE,$u4,$q,$v4,$q,$w4,$mF,$Q,$La},$T3);$oF=[$G4,$FE,$OE,$KD,$nF,$wE];$pF=bless({$T2,$yE,$Q,$rb,$i3,$oF},$S3);$qF=q#ni:/lib/quote_simple.c#;$rF={$S3,1};$sF=q#/lib/quote_simple.c#;$tF=[$rd];$uF=bless({$T2,$rF,$Q,$sF,$i3,$tF},$h4);$vF=q#ni:/lib/quote_simple_init.b#;$wF=q#ni:/lib/quote_simple_quote.b#;$xF=q#ni:/lib/quote_values.b#;$yF=q#ni:/lib/ref_eq.b#;$zF=q#ni:/lib/resolver.b#;$AF={};$BF=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$CF=bless({$w,$BF,$y,$z},$A);$DF={$pB,$CF};$EF=bless({$T2,$AF,$u4,$q,$v4,$q,$w4,$DF,$Q,$Z8},$T3);$FF=q#ni:/lib/slice#;$GF=q#ni:/lib/slice.b#;$HF=q#ni:/lib/slice.c#;$IF={$U3,1};$JF=q#/lib/slice.c#;$KF=[$Tr];$LF=bless({$T2,$IF,$Q,$JF,$i3,$KF},$h4);$MF=q#ni:/lib/slice_init.b#;$NF=q#ni:/lib/slice_serialize.b#;$OF=q#ni:/lib/static_fn.b#;$PF={};$QF=q#fc#;$RF=[];$SF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$TF=bless({$t,$RF,$v,$q,$w,$SF,$y,$z},$A);$UF=q#fk#;$VF=[];$WF=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$XF=bless({$t,$VF,$v,$q,$w,$WF,$y,$cy},$A);$YF=[];$ZF=q#ni('ni:/lib/fn')->new(@_)#;$cG=bless({$t,$YF,$v,$q,$w,$ZF,$y,$cy},$A);$dG=q#fp#;$eG=[];$fG=q#($$)#;$gG=bless({$t,$eG,$v,$q,$w,$ZF,$y,$fG},$A);$hG={$QF,$TF,$UF,$XF,$yv,$cG,$dG,$gG};$iG=q#/lib/static_fn.b#;$jG=bless({$T2,$PF,$u4,$q,$v4,$q,$w4,$hG,$Q,$iG},$T3);$kG=q#ni:/lib/subclass.b#;$lG=q#ni:/lib/tag#;$mG={$V3,1};$nG=q#/lib/tag#;$oG={};$pG=q#local $_;
my ($self, $p) = @_;
$self->resolve($_)->apply($p) for @{$$self{slices}};
$self;#;$qG=bless({$w,$pG,$y,$z},$A);$rG={$I8,$qG};$sG=q#/lib/tag.b#;$tG=bless({$T2,$oG,$u4,$q,$v4,$q,$w4,$rG,$Q,$sG},$T3);$uG={};$vG=q#local $_;
my $class = shift;
my $name  = shift;
+{name => $name, slices => [@_]};#;$wG=bless({$w,$vG,$y,$z},$A);$xG={$J6,$wG};$yG=q#/lib/tag_init.b#;$zG=bless({$T2,$uG,$u4,$q,$v4,$q,$w4,$xG,$Q,$yG},$T3);$AG=[$A8,$G8,$tG,$zG];$BG=bless({$T2,$mG,$Q,$nG,$i3,$AG},$W3);$CG=q#ni:/lib/tag.b#;$DG=q#ni:/lib/tag.c#;$EG={$W3,1};$FG=q#/lib/tag.c#;$GG=[$Tr];$HG=bless({$T2,$EG,$Q,$FG,$i3,$GG},$h4);$IG=q#ni:/lib/tag_init.b#;$JG=q#ni:/lib/test_assert_eq#;$KG={$X3,1};$LG=q#/lib/test_assert_eq#;$MG={$X3,1,$Z3,1};$NG=q#/lib/test_assertion#;$OG={};$PG=q#commit#;$QG=[];$RG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$SG=bless({$t,$QG,$v,$q,$w,$RG,$y,$z},$A);$TG={$PG,$SG};$UG=q#/lib/test_assertion_commit.b#;$VG=bless({$T2,$OG,$u4,$q,$v4,$q,$w4,$TG,$Q,$UG},$T3);$WG=[$G4,$VG];$XG=bless({$T2,$MG,$Q,$NG,$i3,$WG},$c4);$YG={};$ZG=q#diff#;$cH=[];$dH=q#shift->{'diff'}#;$eH=bless({$t,$cH,$v,$q,$w,$dH,$y,$z},$A);$fH={$ZG,$eH};$gH=q#/lib/test_assert_eq_ro.b#;$hH=bless({$T2,$YG,$u4,$q,$v4,$q,$w4,$fH,$Q,$gH},$T3);$iH={};$jH=[];$kH=q#my ($class, $diff) = @_;
+{diff => $diff};#;$lH=bless({$t,$jH,$v,$q,$w,$kH,$y,$z},$A);$mH={$J6,$lH};$nH=q#/lib/test_assert_eq_init.b#;$oH=bless({$T2,$iH,$u4,$q,$v4,$q,$w4,$mH,$Q,$nH},$T3);$pH={};$qH=[];$rH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$sH=bless({$t,$qH,$v,$q,$w,$rH,$y,$z},$A);$tH=q#failed#;$uH=[];$vH=q#defined shift->{diff}#;$wH=bless({$t,$uH,$v,$q,$w,$vH,$y,$z},$A);$xH=q#result#;$yH=[];$zH=bless({$t,$yH,$v,$q,$w,$kq,$y,$z},$A);$AH={$Qs,$sH,$tH,$wH,$xH,$zH};$BH=q#/lib/test_assert_eq_result.b#;$CH=bless({$T2,$pH,$u4,$q,$v4,$q,$w4,$AH,$Q,$BH},$T3);$DH=[$XG,$hH,$oH,$CH];$EH=bless({$T2,$KG,$Q,$LG,$i3,$DH},$Y3);$FH=q#ni:/lib/test_assert_eq.c#;$GH={$Y3,1};$HH=q#/lib/test_assert_eq.c#;$IH={$Y3,1,$c4,1};$JH=q#/lib/test_assertion.c#;$KH=[$rd];$LH=bless({$T2,$IH,$Q,$JH,$i3,$KH},$h4);$MH=[$LH];$NH=bless({$T2,$GH,$Q,$HH,$i3,$MH},$h4);$OH=q#ni:/lib/test_assert_eq_init.b#;$PH=q#ni:/lib/test_assert_eq_result.b#;$QH=q#ni:/lib/test_assert_eq_ro.b#;$RH=q#ni:/lib/test_assertion#;$SH=q#ni:/lib/test_assertion.c#;$TH=q#ni:/lib/test_assertion_commit.b#;$UH=q#ni:/lib/test_case#;$VH={$C,1};$WH=q#/lib/test_case#;$XH=q#running_test#;$YH={};$ZH=[];$cI=q#shift->{'assertions'}#;$dI=bless({$t,$ZH,$v,$q,$w,$cI,$y,$z},$A);$eI=[];$fI=q#shift->{'test'}#;$gI=bless({$t,$eI,$v,$q,$w,$fI,$y,$z},$A);$hI={$n,$dI,$s,$gI};$iI=q#/lib/test_case_ro.b#;$jI=bless({$T2,$YH,$u4,$q,$v4,$q,$w4,$hI,$Q,$iI},$T3);$kI={};$lI=[];$mI=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  $_[0];
} else {
  shift->{'error'};
}#;$nI=bless({$t,$lI,$v,$q,$w,$mI,$y,$z},$A);$oI={$p,$nI};$pI=q#/lib/test_case_rw.b#;$qI=bless({$T2,$kI,$u4,$q,$v4,$q,$w4,$oI,$Q,$pI},$T3);$rI={};$sI=[];$tI=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$uI=bless({$t,$sI,$v,$q,$w,$tI,$y,$z},$A);$vI={$J6,$uI};$wI=q#/lib/test_case_init.b#;$xI=bless({$T2,$rI,$u4,$q,$v4,$q,$w4,$vI,$Q,$wI},$T3);$yI={};$zI=[];$AI=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$BI=bless({$t,$zI,$v,$q,$w,$AI,$y,$z},$A);$CI=[];$DI=q#!shift->{outcome}->[0]#;$EI=bless({$t,$CI,$v,$q,$w,$DI,$y,$z},$A);$FI={$Qs,$BI,$tH,$EI};$GI=q#/lib/test_case_metrics.b#;$HI=bless({$T2,$yI,$u4,$q,$v4,$q,$w4,$FI,$Q,$GI},$T3);$II={};$JI=q#done#;$KI=[];$LI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$MI=bless({$t,$KI,$v,$q,$w,$LI,$y,$z},$A);$NI=[];$OI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$PI=bless({$t,$NI,$v,$q,$w,$OI,$y,$z},$A);$QI={$JI,$MI,$iq,$PI};$RI=q#/lib/test_case_run.b#;$SI=bless({$T2,$II,$u4,$q,$v4,$q,$w4,$QI,$Q,$RI},$T3);$TI=[$YC,$jI,$qI,$xI,$HI,$SI];$UI=bless({$T2,$VH,$Q,$WH,$XH,$q,$i3,$TI},$d4);$VI=[];$WI=q#shift->{running_test} = undef#;$XI=bless({$t,$VI,$v,$q,$w,$WI,$y,$z},$A);$YI=q#ni:/lib/test_case.c#;$ZI={$d4,1};$cJ=q#/lib/test_case.c#;$dJ={};$eJ=[];$fJ=q#shift->{'running_test'}#;$gJ=bless({$t,$eJ,$v,$q,$w,$fJ,$y,$z},$A);$hJ={$XH,$gJ};$iJ=q#/lib/test_case.c_test_ro.b#;$jJ=bless({$T2,$dJ,$u4,$q,$v4,$q,$w4,$hJ,$Q,$iJ},$T3);$kJ={};$lJ=q#with_test#;$mJ=[];$nJ=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$oJ=bless({$t,$mJ,$v,$q,$w,$nJ,$y,$z},$A);$pJ={$lJ,$oJ};$qJ=q#/lib/test_case.c_test.b#;$rJ=bless({$T2,$kJ,$u4,$XI,$v4,$q,$w4,$pJ,$Q,$qJ},$T3);$sJ=[$fD,$jJ,$rJ];$tJ=bless({$T2,$ZI,$Q,$cJ,$i3,$sJ},$h4);$uJ=q#ni:/lib/test_case.c_test.b#;$vJ=q#ni:/lib/test_case.c_test_ro.b#;$wJ=q#ni:/lib/test_case_init.b#;$xJ=q#ni:/lib/test_case_metrics.b#;$yJ=q#ni:/lib/test_case_ro.b#;$zJ=q#ni:/lib/test_case_run.b#;$AJ=q#ni:/lib/test_case_rw.b#;$BJ=q#ni:/lib/test_value#;$CJ={$e4,1};$DJ=q#/lib/test_value#;$EJ={};$FJ=[];$GJ=q#\\$_[1]#;$HJ=bless({$t,$FJ,$v,$q,$w,$GJ,$y,$z},$A);$IJ={$J6,$HJ};$JJ=q#/lib/test_value_init.b#;$KJ=bless({$T2,$EJ,$u4,$q,$v4,$q,$w4,$IJ,$Q,$JJ},$T3);$LJ={};$MJ=q#(==#;$NJ=[];$OJ=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$PJ=bless({$t,$NJ,$v,$q,$w,$OJ,$y,$z},$A);$QJ=q#detailed_scalar_diff#;$RJ=[];$SJ=q#local $_;
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
[@diff];#;$TJ=bless({$t,$RJ,$v,$q,$w,$SJ,$y,$z},$A);$UJ=[];$VJ=q#my ($self, $rhs) = @_;
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
return undef;#;$WJ=bless({$t,$UJ,$v,$q,$w,$VJ,$y,$z},$A);$XJ={$MJ,$PJ,$QJ,$TJ,$ZG,$WJ};$YJ=q#/lib/test_value_eq.b#;$ZJ=bless({$T2,$LJ,$u4,$q,$v4,$q,$w4,$XJ,$Q,$YJ},$T3);$cK={};$dK=[];$eK=q#ni::json_encode ${$_[0]}#;$fK=bless({$t,$dK,$v,$q,$w,$eK,$y,$z},$A);$gK={$Qs,$fK};$hK=q#/lib/test_value_str.b#;$iK=bless({$T2,$cK,$u4,$q,$v4,$q,$w4,$gK,$Q,$hK},$T3);$jK=[$G4,$KJ,$ZJ,$iK];$kK=bless({$T2,$CJ,$Q,$DJ,$i3,$jK},$f4);$lK=q#ni:/lib/test_value.c#;$mK={$f4,1};$nK=q#/lib/test_value.c#;$oK=[$rd];$pK=bless({$T2,$mK,$Q,$nK,$i3,$oK},$h4);$qK=q#ni:/lib/test_value_eq.b#;$rK=q#ni:/lib/test_value_init.b#;$sK=q#ni:/lib/test_value_str.b#;$tK=q#ni:/lib/todo#;$uK={$H,1};$vK=q#/lib/todo#;$wK={};$xK=q#shift->{'todo'}#;$yK=bless({$w,$xK,$y,$z},$A);$zK={$E,$yK};$AK=q#/lib/todo_ro.b#;$BK=bless({$T2,$wK,$u4,$q,$v4,$q,$w4,$zK,$Q,$AK},$T3);$CK={};$DK=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$EK=bless({$w,$DK,$y,$z},$A);$FK={$J6,$EK};$GK=q#/lib/todo_init.b#;$HK=bless({$T2,$CK,$u4,$q,$v4,$q,$w4,$FK,$Q,$GK},$T3);$IK={};$JK=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$KK=bless({$w,$JK,$y,$z},$A);$LK={$Qs,$KK};$MK=q#/lib/todo_str.b#;$NK=bless({$T2,$IK,$u4,$q,$v4,$q,$w4,$LK,$Q,$MK},$T3);$OK=[$YC,$BK,$HK,$NK];$PK=bless({$T2,$uK,$Q,$vK,$i3,$OK},$g4);$QK=q#ni:/lib/todo.c#;$RK={$g4,1};$SK=q#/lib/todo.c#;$TK=[$fD];$UK=bless({$T2,$RK,$Q,$SK,$i3,$TK},$h4);$VK=q#ni:/lib/todo_ctor.b#;$WK={};$XK=q#ni('ni:/lib/todo')->new(@_)#;$YK=bless({$w,$XK,$y,$z},$A);$ZK={$R2,$YK};$cL=q#/lib/todo_ctor.b#;$dL=bless({$T2,$WK,$u4,$q,$v4,$q,$w4,$ZK,$Q,$cL},$T3);$eL=q#ni:/lib/todo_init.b#;$fL=q#ni:/lib/todo_ro.b#;$gL=q#ni:/lib/todo_str.b#;$hL=q#ni:/metaclass#;$iL={$h4,1};$jL=q#/metaclass#;$kL=[$Hc,$Xc,$Nc,$Dc];$lL=bless({$T2,$iL,$Q,$jL,$i3,$kL},$i4);$mL=q#ni:/metaclass.c#;$nL={$i4,1};$oL=q#/metaclass.c#;$pL=[$R];$qL=bless({$T2,$nL,$Q,$oL,$i3,$pL},$h4);$rL=q#ni:/module#;$sL=q#ni:/module.c#;$tL=q#ni:/object#;$uL=q#ni:/object.c#;$vL=q#ni:/semantic#;$wL=q#semantic#;$xL={$wL,1};$yL=[];$zL=bless({$T2,$xL,$Q,$yc,$i3,$yL},$j4);$AL=q#ni:/semantic/dimension#;$BL={$n4,1};$CL=q#/semantic/dimension#;$DL=[$id];$EL=bless({$T2,$BL,$Q,$CL,$i3,$DL},$o4);$FL=q#ni:/semantic/dimension.c#;$GL={$o4,1};$HL=q#/semantic/dimension.c#;$IL=[$wd];$JL=bless({$T2,$GL,$Q,$HL,$i3,$IL},$h4);$KL=q#ni:/semantic/task#;$LL=q#ni:/semantic/task.c#;$ML=q#ni:/semantic/task_outcome.b#;$NL=q#ni:/semantic/task_ro.b#;$OL=q#ni:main#;$PL={$Ol,1};$QL=[$dL,$jG,$ky,$Nl];$RL=bless({$T2,$PL,$Q,$Ol,$i3,$QL},$j4);$SL=q#ni:ni#;$TL={$xz,1};$UL=[$NC,$Sz,$wz];$VL=bless({$T2,$TL,$Q,$xz,$i3,$UL},$j4);$WL={$d,$T,$W,$f1,$g1,$l1,$m1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$J2,$K2,$P2,$Q2,$n6,$o6,$g8,$h8,$n8,$o8,$k9,$l9,$E9,$F9,$V9,$W9,$Xa,$Ya,$hb,$ib,$sb,$tb,$tc,$uc,$zc,$Ac,$id,$jd,$wd,$xd,$Bd,$Cd,$Fd,$Gd,$ke,$le,$Be,$Ce,$Od,$De,$ie,$Ee,$Ve,$We,$cf,$df,$Me,$ef,$Te,$ff,$Lg,$Mg,$Qg,$Rg,$tg,$Sg,$Jg,$Tg,$vf,$Ug,$lg,$Vg,$Pf,$Wg,$of,$Xg,$ri,$vi,$Ti,$Ui,$Ri,$Vi,$Nh,$Wi,$Yh,$Xi,$ph,$Yi,$oi,$Zi,$ih,$cj,$xh,$dj,$xk,$yk,$Ck,$Dk,$tj,$Ek,$Qj,$Fk,$Aj,$Gk,$vk,$Hk,$lj,$Ik,$Yj,$Jk,$el,$fl,$jl,$kl,$cl,$ll,$Rk,$ml,$Nl,$Pl,$pm,$qm,$um,$vm,$Yl,$wm,$nm,$xm,$h6,$ym,$ze,$zm,$xe,$Am,$l5,$Bm,$t5,$Cm,$F5,$Dm,$P4,$Em,$f6,$Fm,$R5,$Gm,$L7,$Hm,$Lm,$Mm,$J7,$Nm,$P6,$Om,$p7,$Pm,$E6,$Qm,$d7,$Rm,$In,$Jn,$Nn,$On,$sn,$Pn,$Gn,$Qn,$ln,$Rn,$dp,$hp,$tp,$up,$rp,$vp,$xq,$Bq,$Vq,$Wq,$Tq,$Xq,$Up,$Yq,$gq,$Zq,$Np,$cr,$sq,$dr,$Fo,$er,$Zo,$fr,$xr,$yr,$Cr,$Dr,$or,$Er,$vr,$Fr,$Jr,$Kr,$Or,$Pr,$A8,$Qr,$Tr,$Ur,$os,$ps,$gs,$qs,$us,$vs,$ms,$ws,$Nc,$xs,$e9,$ys,$Q8,$zs,$Ds,$Es,$W8,$Fs,$nt,$ot,$Ns,$pt,$lt,$qt,$wu,$xu,$Iu,$Ju,$Gu,$Ku,$eu,$Lu,$Ot,$Mu,$ku,$Nu,$Wt,$Ou,$xt,$Pu,$Et,$Qu,$uu,$Ru,$y8,$Su,$Wv,$cw,$ow,$pw,$mw,$qw,$mv,$rw,$Nv,$sw,$Ev,$tw,$Uv,$uw,$Ex,$Fx,$Jx,$Kx,$Cx,$Lx,$Mw,$Mx,$Fw,$Nx,$ix,$Ox,$Vx,$Wx,$ky,$ly,$vy,$wy,$Gy,$Hy,$Ey,$Iy,$Ra,$Jy,$Ny,$Oy,$la,$Py,$Ja,$Qy,$E4,$Ry,$Xc,$Sy,$wz,$yz,$Sz,$Tz,$Vs,$Uz,$G8,$Vz,$cA,$dA,$jA,$kA,$JB,$KB,$OB,$PB,$QA,$QB,$HB,$RB,$nB,$SB,$xB,$TB,$yA,$UB,$NC,$OC,$YC,$ZC,$fD,$gD,$WC,$hD,$lD,$mD,$Hc,$nD,$CD,$DD,$KD,$LD,$gE,$hE,$wE,$xE,$pF,$qF,$uF,$vF,$FE,$wF,$OE,$xF,$nF,$yF,$et,$zF,$EF,$FF,$Wb,$GF,$Hb,$HF,$LF,$MF,$Nb,$NF,$Ub,$OF,$jG,$kG,$gd,$lG,$BG,$CG,$tG,$DG,$HG,$IG,$zG,$JG,$EH,$FH,$NH,$OH,$oH,$PH,$CH,$QH,$hH,$RH,$XG,$SH,$LH,$TH,$VG,$UH,$UI,$YI,$tJ,$uJ,$rJ,$vJ,$jJ,$wJ,$xI,$xJ,$HI,$yJ,$jI,$zJ,$SI,$AJ,$qI,$BJ,$kK,$lK,$pK,$qK,$ZJ,$rK,$KJ,$sK,$iK,$tK,$PK,$QK,$UK,$VK,$dL,$eL,$HK,$fL,$BK,$gL,$NK,$hL,$lL,$mL,$qL,$rL,$Qc,$sL,$ud,$tL,$G4,$uL,$rd,$vL,$zL,$AL,$EL,$FL,$JL,$KL,$so,$LL,$np,$ML,$qo,$NL,$eo,$OL,$RL,$SL,$VL};$XL=q#resolvers#;$YL=[];$ZL=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$cM=bless({$t,$YL,$v,$q,$w,$ZL,$y,$z},$A);$dM=q#file#;$eM=[];$fM=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$gM=bless({$t,$eM,$v,$q,$w,$fM,$y,$z},$A);$hM=q#null#;$iM=[];$jM=q#ni('ni:/io/null')->new#;$kM=bless({$t,$iM,$v,$q,$w,$jM,$y,$z},$A);$lM=q#sh#;$mM=[];$nM=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$oM=bless({$t,$mM,$v,$q,$w,$nM,$y,$z},$A);$pM=q#str#;$qM=[];$rM=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$sM=bless({$t,$qM,$v,$q,$w,$rM,$y,$z},$A);$tM={$r7,$cM,$dM,$gM,$hM,$kM,$lM,$oM,$pM,$sM};$uM=bless({$c,$WL,$XL,$tM},$N3);*$fv=\&$cv;*$ev=\&$Yu;*$dv=\&$Wu;*$Db=\&$Bb;*$Cb=\&$zb;$E4->apply_($j3);$E4->apply_($k3);$E4->apply_($U2);$E4->apply_($l3);$E4->apply_($V2);$E4->apply_($m3);$E4->apply_($W2);$E4->apply_($n3);$E4->apply_($X2);$E4->apply_($o3);$E4->apply_($Y2);$E4->apply_($p3);$E4->apply_($Z2);$E4->apply_($q3);$E4->apply_($c3);$E4->apply_($r3);$E4->apply_($d3);$E4->apply_($s3);$E4->apply_($e3);$E4->apply_($t3);$E4->apply_($f3);$E4->apply_($u3);$E4->apply_($v3);$E4->apply_($w3);$E4->apply_($x3);$E4->apply_($y3);$E4->apply_($z3);$E4->apply_($A3);$E4->apply_($B3);$E4->apply_($C3);$E4->apply_($D3);$E4->apply_($E3);$E4->apply_($F3);$E4->apply_($G3);$E4->apply_($S);$E4->apply_($H3);$E4->apply_($A);$E4->apply_($I3);$E4->apply_($J3);$E4->apply_($K3);$E4->apply_($L3);$E4->apply_($M3);$E4->apply_($N3);$E4->apply_($O3);$E4->apply_($P3);$E4->apply_($Q3);$E4->apply_($R3);$E4->apply_($S3);$E4->apply_($T3);$E4->apply_($U3);$E4->apply_($V3);$E4->apply_($W3);$E4->apply_($X3);$E4->apply_($Y3);$E4->apply_($Z3);$E4->apply_($c4);$E4->apply_($C);$E4->apply_($d4);$E4->apply_($e4);$E4->apply_($f4);$E4->apply_($H);$E4->apply_($g4);$E4->apply_($h4);$E4->apply_($i4);$E4->apply_($j4);$E4->apply_($k4);$E4->apply_($l4);$E4->apply_($m4);$E4->apply_($n4);$E4->apply_($o4);$E4->apply_($p4);$E4->apply_($q4);$P4->apply_($U2);$P4->apply_($V2);$P4->apply_($W2);$P4->apply_($X2);$P4->apply_($Y2);$P4->apply_($Z2);$P4->apply_($c3);$P4->apply_($d3);$P4->apply_($e3);$P4->apply_($f3);$l5->apply_($U2);$l5->apply_($V2);$l5->apply_($W2);$l5->apply_($X2);$l5->apply_($Y2);$l5->apply_($Z2);$l5->apply_($c3);$l5->apply_($d3);$l5->apply_($e3);$l5->apply_($f3);$t5->apply_($U2);$t5->apply_($V2);$t5->apply_($W2);$t5->apply_($X2);$t5->apply_($Y2);$t5->apply_($Z2);$t5->apply_($c3);$t5->apply_($d3);$t5->apply_($e3);$t5->apply_($f3);$F5->apply_($U2);$F5->apply_($V2);$F5->apply_($W2);$F5->apply_($X2);$F5->apply_($Y2);$F5->apply_($Z2);$F5->apply_($c3);$F5->apply_($d3);$F5->apply_($e3);$F5->apply_($f3);$R5->apply_($U2);$R5->apply_($V2);$R5->apply_($W2);$R5->apply_($X2);$R5->apply_($Y2);$R5->apply_($Z2);$R5->apply_($c3);$R5->apply_($d3);$R5->apply_($e3);$R5->apply_($f3);$f6->apply_($U2);$f6->apply_($V2);$f6->apply_($W2);$f6->apply_($X2);$f6->apply_($Y2);$f6->apply_($Z2);$f6->apply_($c3);$f6->apply_($d3);$f6->apply_($e3);$f6->apply_($f3);$E6->apply_($e3);$P6->apply_($e3);$d7->apply_($e3);$p7->apply_($e3);$J7->apply_($e3);$y8->apply_($j3);$y8->apply_($k3);$y8->apply_($l3);$y8->apply_($m3);$y8->apply_($n3);$y8->apply_($o3);$y8->apply_($p3);$y8->apply_($q3);$y8->apply_($r3);$y8->apply_($s3);$y8->apply_($t3);$y8->apply_($u3);$y8->apply_($w3);$y8->apply_($y3);$y8->apply_($A3);$y8->apply_($B3);$y8->apply_($C3);$y8->apply_($D3);$y8->apply_($E3);$y8->apply_($F3);$y8->apply_($G3);$y8->apply_($H3);$y8->apply_($I3);$y8->apply_($K3);$y8->apply_($M3);$y8->apply_($O3);$y8->apply_($Q3);$y8->apply_($S3);$y8->apply_($T3);$y8->apply_($U3);$y8->apply_($V3);$y8->apply_($W3);$y8->apply_($Y3);$y8->apply_($c4);$y8->apply_($d4);$y8->apply_($f4);$y8->apply_($g4);$y8->apply_($h4);$y8->apply_($i4);$y8->apply_($j4);$y8->apply_($k4);$y8->apply_($m4);$y8->apply_($n4);$y8->apply_($o4);$y8->apply_($q4);$G8->apply_($j3);$G8->apply_($k3);$G8->apply_($l3);$G8->apply_($m3);$G8->apply_($n3);$G8->apply_($o3);$G8->apply_($p3);$G8->apply_($q3);$G8->apply_($r3);$G8->apply_($s3);$G8->apply_($t3);$G8->apply_($u3);$G8->apply_($w3);$G8->apply_($y3);$G8->apply_($A3);$G8->apply_($C3);$G8->apply_($D3);$G8->apply_($E3);$G8->apply_($F3);$G8->apply_($G3);$G8->apply_($S);$G8->apply_($H3);$G8->apply_($I3);$G8->apply_($K3);$G8->apply_($M3);$G8->apply_($O3);$G8->apply_($Q3);$G8->apply_($S3);$G8->apply_($T3);$G8->apply_($U3);$G8->apply_($V3);$G8->apply_($W3);$G8->apply_($Y3);$G8->apply_($c4);$G8->apply_($d4);$G8->apply_($f4);$G8->apply_($g4);$G8->apply_($h4);$G8->apply_($i4);$G8->apply_($j4);$G8->apply_($k4);$G8->apply_($m4);$G8->apply_($n4);$G8->apply_($o4);$G8->apply_($q4);$Q8->apply_($F3);$W8->apply_($F3);$la->apply_($L3);$Ja->apply_($L3);$Hb->apply_($T3);$Nb->apply_($T3);$Ub->apply_($F3);$Ub->apply_($T3);$Nc->apply_($j3);$Nc->apply_($k3);$Nc->apply_($l3);$Nc->apply_($m3);$Nc->apply_($n3);$Nc->apply_($o3);$Nc->apply_($p3);$Nc->apply_($q3);$Nc->apply_($r3);$Nc->apply_($s3);$Nc->apply_($t3);$Nc->apply_($u3);$Nc->apply_($w3);$Nc->apply_($y3);$Nc->apply_($A3);$Nc->apply_($C3);$Nc->apply_($E3);$Nc->apply_($G3);$Nc->apply_($H3);$Nc->apply_($I3);$Nc->apply_($K3);$Nc->apply_($M3);$Nc->apply_($O3);$Nc->apply_($Q3);$Nc->apply_($S3);$Nc->apply_($U3);$Nc->apply_($W3);$Nc->apply_($Y3);$Nc->apply_($c4);$Nc->apply_($d4);$Nc->apply_($f4);$Nc->apply_($g4);$Nc->apply_($h4);$Nc->apply_($i4);$Nc->apply_($j4);$Nc->apply_($k4);$Nc->apply_($m4);$Nc->apply_($n4);$Nc->apply_($o4);$Nc->apply_($q4);$Xc->apply_($j3);$Xc->apply_($k3);$Xc->apply_($l3);$Xc->apply_($m3);$Xc->apply_($n3);$Xc->apply_($o3);$Xc->apply_($p3);$Xc->apply_($q3);$Xc->apply_($r3);$Xc->apply_($s3);$Xc->apply_($t3);$Xc->apply_($u3);$Xc->apply_($w3);$Xc->apply_($y3);$Xc->apply_($A3);$Xc->apply_($C3);$Xc->apply_($E3);$Xc->apply_($G3);$Xc->apply_($H3);$Xc->apply_($A);$Xc->apply_($I3);$Xc->apply_($K3);$Xc->apply_($M3);$Xc->apply_($O3);$Xc->apply_($Q3);$Xc->apply_($S3);$Xc->apply_($T3);$Xc->apply_($U3);$Xc->apply_($V3);$Xc->apply_($W3);$Xc->apply_($Y3);$Xc->apply_($c4);$Xc->apply_($d4);$Xc->apply_($f4);$Xc->apply_($g4);$Xc->apply_($h4);$Xc->apply_($i4);$Xc->apply_($k4);$Xc->apply_($m4);$Xc->apply_($n4);$Xc->apply_($o4);$Xc->apply_($q4);$gd->apply_($j3);$gd->apply_($k3);$gd->apply_($l3);$gd->apply_($m3);$gd->apply_($n3);$gd->apply_($o3);$gd->apply_($p3);$gd->apply_($q3);$gd->apply_($r3);$gd->apply_($s3);$gd->apply_($t3);$gd->apply_($u3);$gd->apply_($w3);$gd->apply_($y3);$gd->apply_($A3);$gd->apply_($C3);$gd->apply_($E3);$gd->apply_($G3);$gd->apply_($H3);$gd->apply_($I3);$gd->apply_($K3);$gd->apply_($M3);$gd->apply_($O3);$gd->apply_($Q3);$gd->apply_($S3);$gd->apply_($U3);$gd->apply_($W3);$gd->apply_($Y3);$gd->apply_($c4);$gd->apply_($d4);$gd->apply_($f4);$gd->apply_($g4);$gd->apply_($i4);$gd->apply_($k4);$gd->apply_($m4);$gd->apply_($n4);$gd->apply_($o4);$gd->apply_($q4);$Od->apply_($U2);$ie->apply_($U2);$xe->apply_($l3);$xe->apply_($m3);$xe->apply_($n3);$xe->apply_($o3);$xe->apply_($p3);$xe->apply_($q3);$xe->apply_($r3);$xe->apply_($s3);$xe->apply_($t3);$xe->apply_($u3);$Me->apply_($V2);$Te->apply_($V2);$of->apply_($W2);$vf->apply_($W2);$Pf->apply_($W2);$lg->apply_($W2);$tg->apply_($W2);$Jg->apply_($W2);$ih->apply_($X2);$ih->apply_($Z2);$ph->apply_($X2);$xh->apply_($X2);$Nh->apply_($X2);$Nh->apply_($Z2);$Yh->apply_($X2);$oi->apply_($X2);$oi->apply_($Z2);$Ri->apply_($o3);$lj->apply_($Y2);$tj->apply_($Y2);$Aj->apply_($Y2);$Qj->apply_($Y2);$Yj->apply_($Y2);$vk->apply_($Y2);$Rk->apply_($Z2);$cl->apply_($Z2);$Nl->apply_($Ol);$Yl->apply_($c3);$nm->apply_($c3);$ln->apply_($f3);$sn->apply_($f3);$Gn->apply_($f3);$eo->apply_($v3);$eo->apply_($x3);$eo->apply_($z3);$eo->apply_($p4);$qo->apply_($v3);$qo->apply_($x3);$qo->apply_($z3);$qo->apply_($p4);$Fo->apply_($v3);$Fo->apply_($x3);$Fo->apply_($z3);$Zo->apply_($v3);$Zo->apply_($x3);$Zo->apply_($z3);$rp->apply_($w3);$rp->apply_($y3);$rp->apply_($A3);$Np->apply_($x3);$Up->apply_($x3);$gq->apply_($x3);$sq->apply_($x3);$Tq->apply_($y3);$or->apply_($z3);$vr->apply_($z3);$gs->apply_($j3);$gs->apply_($k3);$gs->apply_($l3);$gs->apply_($m3);$gs->apply_($n3);$gs->apply_($o3);$gs->apply_($p3);$gs->apply_($q3);$gs->apply_($r3);$gs->apply_($s3);$gs->apply_($t3);$gs->apply_($u3);$gs->apply_($w3);$gs->apply_($y3);$gs->apply_($A3);$gs->apply_($C3);$gs->apply_($D3);$gs->apply_($E3);$gs->apply_($G3);$gs->apply_($H3);$gs->apply_($I3);$gs->apply_($K3);$gs->apply_($M3);$gs->apply_($O3);$gs->apply_($Q3);$gs->apply_($S3);$gs->apply_($U3);$gs->apply_($W3);$gs->apply_($Y3);$gs->apply_($c4);$gs->apply_($d4);$gs->apply_($f4);$gs->apply_($g4);$gs->apply_($h4);$gs->apply_($i4);$gs->apply_($j4);$gs->apply_($k4);$gs->apply_($m4);$gs->apply_($n4);$gs->apply_($o4);$gs->apply_($q4);$ms->apply_($D3);$Ns->apply_($j3);$Ns->apply_($k3);$Ns->apply_($l3);$Ns->apply_($m3);$Ns->apply_($n3);$Ns->apply_($o3);$Ns->apply_($p3);$Ns->apply_($q3);$Ns->apply_($r3);$Ns->apply_($s3);$Ns->apply_($t3);$Ns->apply_($u3);$Ns->apply_($w3);$Ns->apply_($y3);$Ns->apply_($A3);$Ns->apply_($C3);$Ns->apply_($D3);$Ns->apply_($E3);$Ns->apply_($G3);$Ns->apply_($H3);$Ns->apply_($I3);$Ns->apply_($K3);$Ns->apply_($M3);$Ns->apply_($O3);$Ns->apply_($Q3);$Ns->apply_($S3);$Ns->apply_($U3);$Ns->apply_($W3);$Ns->apply_($Y3);$Ns->apply_($c4);$Ns->apply_($d4);$Ns->apply_($f4);$Ns->apply_($g4);$Ns->apply_($h4);$Ns->apply_($i4);$Ns->apply_($j4);$Ns->apply_($k4);$Ns->apply_($m4);$Ns->apply_($n4);$Ns->apply_($o4);$Ns->apply_($q4);$Vs->apply_($j3);$Vs->apply_($k3);$Vs->apply_($l3);$Vs->apply_($m3);$Vs->apply_($n3);$Vs->apply_($o3);$Vs->apply_($p3);$Vs->apply_($q3);$Vs->apply_($r3);$Vs->apply_($s3);$Vs->apply_($t3);$Vs->apply_($u3);$Vs->apply_($w3);$Vs->apply_($y3);$Vs->apply_($A3);$Vs->apply_($C3);$Vs->apply_($D3);$Vs->apply_($E3);$Vs->apply_($G3);$Vs->apply_($H3);$Vs->apply_($I3);$Vs->apply_($K3);$Vs->apply_($M3);$Vs->apply_($O3);$Vs->apply_($Q3);$Vs->apply_($S3);$Vs->apply_($U3);$Vs->apply_($W3);$Vs->apply_($Y3);$Vs->apply_($c4);$Vs->apply_($d4);$Vs->apply_($f4);$Vs->apply_($g4);$Vs->apply_($h4);$Vs->apply_($i4);$Vs->apply_($j4);$Vs->apply_($k4);$Vs->apply_($m4);$Vs->apply_($n4);$Vs->apply_($o4);$Vs->apply_($q4);$et->apply_($j3);$et->apply_($k3);$et->apply_($l3);$et->apply_($m3);$et->apply_($n3);$et->apply_($o3);$et->apply_($p3);$et->apply_($q3);$et->apply_($r3);$et->apply_($s3);$et->apply_($t3);$et->apply_($u3);$et->apply_($w3);$et->apply_($y3);$et->apply_($A3);$et->apply_($C3);$et->apply_($D3);$et->apply_($E3);$et->apply_($G3);$et->apply_($H3);$et->apply_($I3);$et->apply_($K3);$et->apply_($M3);$et->apply_($O3);$et->apply_($Q3);$et->apply_($S3);$et->apply_($U3);$et->apply_($W3);$et->apply_($Y3);$et->apply_($c4);$et->apply_($d4);$et->apply_($f4);$et->apply_($g4);$et->apply_($h4);$et->apply_($i4);$et->apply_($j4);$et->apply_($k4);$et->apply_($m4);$et->apply_($n4);$et->apply_($o4);$et->apply_($q4);$lt->apply_($j3);$lt->apply_($k3);$lt->apply_($l3);$lt->apply_($m3);$lt->apply_($n3);$lt->apply_($o3);$lt->apply_($p3);$lt->apply_($q3);$lt->apply_($r3);$lt->apply_($s3);$lt->apply_($t3);$lt->apply_($u3);$lt->apply_($w3);$lt->apply_($y3);$lt->apply_($A3);$lt->apply_($C3);$lt->apply_($D3);$lt->apply_($E3);$lt->apply_($G3);$lt->apply_($H3);$lt->apply_($I3);$lt->apply_($K3);$lt->apply_($M3);$lt->apply_($O3);$lt->apply_($Q3);$lt->apply_($S3);$lt->apply_($U3);$lt->apply_($W3);$lt->apply_($Y3);$lt->apply_($c4);$lt->apply_($d4);$lt->apply_($f4);$lt->apply_($g4);$lt->apply_($h4);$lt->apply_($i4);$lt->apply_($j4);$lt->apply_($k4);$lt->apply_($m4);$lt->apply_($n4);$lt->apply_($o4);$lt->apply_($q4);$xt->apply_($S);$Et->apply_($S);$Ot->apply_($S);$Wt->apply_($S);$eu->apply_($S);$ku->apply_($S);$uu->apply_($S);$Gu->apply_($H3);$mv->apply_($A);$Ev->apply_($A);$Nv->apply_($A);$Uv->apply_($A);$mw->apply_($I3);$Fw->apply_($J3);$Mw->apply_($J3);$ix->apply_($J3);$Cx->apply_($J3);$Vx->apply_($L3);$ky->apply_($Ol);$vy->apply_($j3);$vy->apply_($k3);$vy->apply_($l3);$vy->apply_($m3);$vy->apply_($n3);$vy->apply_($o3);$vy->apply_($p3);$vy->apply_($q3);$vy->apply_($r3);$vy->apply_($s3);$vy->apply_($t3);$vy->apply_($u3);$vy->apply_($w3);$vy->apply_($y3);$vy->apply_($A3);$vy->apply_($C3);$vy->apply_($D3);$vy->apply_($E3);$vy->apply_($G3);$vy->apply_($H3);$vy->apply_($I3);$vy->apply_($K3);$vy->apply_($M3);$vy->apply_($O3);$vy->apply_($Q3);$vy->apply_($S3);$vy->apply_($U3);$vy->apply_($W3);$vy->apply_($Y3);$vy->apply_($c4);$vy->apply_($d4);$vy->apply_($f4);$vy->apply_($g4);$vy->apply_($h4);$vy->apply_($i4);$vy->apply_($j4);$vy->apply_($k4);$vy->apply_($m4);$vy->apply_($n4);$vy->apply_($o4);$vy->apply_($q4);$Ey->apply_($j3);$Ey->apply_($k3);$Ey->apply_($l3);$Ey->apply_($m3);$Ey->apply_($n3);$Ey->apply_($o3);$Ey->apply_($p3);$Ey->apply_($q3);$Ey->apply_($r3);$Ey->apply_($s3);$Ey->apply_($t3);$Ey->apply_($u3);$Ey->apply_($w3);$Ey->apply_($y3);$Ey->apply_($A3);$Ey->apply_($C3);$Ey->apply_($D3);$Ey->apply_($E3);$Ey->apply_($G3);$Ey->apply_($H3);$Ey->apply_($I3);$Ey->apply_($K3);$Ey->apply_($M3);$Ey->apply_($O3);$Ey->apply_($Q3);$Ey->apply_($S3);$Ey->apply_($U3);$Ey->apply_($W3);$Ey->apply_($Y3);$Ey->apply_($c4);$Ey->apply_($d4);$Ey->apply_($f4);$Ey->apply_($g4);$Ey->apply_($h4);$Ey->apply_($i4);$Ey->apply_($j4);$Ey->apply_($k4);$Ey->apply_($m4);$Ey->apply_($n4);$Ey->apply_($o4);$Ey->apply_($q4);$wz->apply_($xz);$Sz->apply_($xz);$cA->apply_($j3);$cA->apply_($k3);$cA->apply_($l3);$cA->apply_($m3);$cA->apply_($n3);$cA->apply_($o3);$cA->apply_($p3);$cA->apply_($q3);$cA->apply_($r3);$cA->apply_($s3);$cA->apply_($t3);$cA->apply_($u3);$cA->apply_($w3);$cA->apply_($y3);$cA->apply_($A3);$cA->apply_($C3);$cA->apply_($D3);$cA->apply_($E3);$cA->apply_($F3);$cA->apply_($G3);$cA->apply_($H3);$cA->apply_($I3);$cA->apply_($K3);$cA->apply_($M3);$cA->apply_($O3);$cA->apply_($Q3);$cA->apply_($S3);$cA->apply_($T3);$cA->apply_($U3);$cA->apply_($V3);$cA->apply_($W3);$cA->apply_($Y3);$cA->apply_($c4);$cA->apply_($d4);$cA->apply_($f4);$cA->apply_($g4);$cA->apply_($h4);$cA->apply_($i4);$cA->apply_($j4);$cA->apply_($k4);$cA->apply_($m4);$cA->apply_($n4);$cA->apply_($o4);$cA->apply_($q4);$jA->apply_($j3);$jA->apply_($k3);$jA->apply_($l3);$jA->apply_($m3);$jA->apply_($n3);$jA->apply_($o3);$jA->apply_($p3);$jA->apply_($q3);$jA->apply_($r3);$jA->apply_($s3);$jA->apply_($t3);$jA->apply_($u3);$jA->apply_($w3);$jA->apply_($y3);$jA->apply_($A3);$jA->apply_($C3);$jA->apply_($D3);$jA->apply_($E3);$jA->apply_($F3);$jA->apply_($G3);$jA->apply_($H3);$jA->apply_($I3);$jA->apply_($K3);$jA->apply_($M3);$jA->apply_($O3);$jA->apply_($Q3);$jA->apply_($S3);$jA->apply_($T3);$jA->apply_($U3);$jA->apply_($V3);$jA->apply_($W3);$jA->apply_($Y3);$jA->apply_($c4);$jA->apply_($d4);$jA->apply_($f4);$jA->apply_($g4);$jA->apply_($h4);$jA->apply_($i4);$jA->apply_($j4);$jA->apply_($k4);$jA->apply_($m4);$jA->apply_($n4);$jA->apply_($o4);$jA->apply_($q4);$yA->apply_($N3);$QA->apply_($N3);$nB->apply_($N3);$xB->apply_($N3);$HB->apply_($N3);$NC->apply_($xz);$WC->apply_($P3);$WC->apply_($C);$WC->apply_($H);$CD->apply_($L3);$KD->apply_($L3);$KD->apply_($R3);$gE->apply_($L3);$wE->apply_($L3);$wE->apply_($R3);$FE->apply_($R3);$OE->apply_($R3);$nF->apply_($L3);$nF->apply_($R3);$EF->apply_($j3);$EF->apply_($k3);$EF->apply_($l3);$EF->apply_($m3);$EF->apply_($n3);$EF->apply_($o3);$EF->apply_($p3);$EF->apply_($q3);$EF->apply_($r3);$EF->apply_($s3);$EF->apply_($t3);$EF->apply_($u3);$EF->apply_($w3);$EF->apply_($y3);$EF->apply_($A3);$EF->apply_($C3);$EF->apply_($D3);$EF->apply_($E3);$EF->apply_($F3);$EF->apply_($G3);$EF->apply_($H3);$EF->apply_($I3);$EF->apply_($K3);$EF->apply_($M3);$EF->apply_($O3);$EF->apply_($Q3);$EF->apply_($S3);$EF->apply_($U3);$EF->apply_($V3);$EF->apply_($W3);$EF->apply_($Y3);$EF->apply_($c4);$EF->apply_($d4);$EF->apply_($f4);$EF->apply_($g4);$EF->apply_($h4);$EF->apply_($i4);$EF->apply_($j4);$EF->apply_($k4);$EF->apply_($m4);$EF->apply_($n4);$EF->apply_($o4);$EF->apply_($q4);$jG->apply_($Ol);$tG->apply_($V3);$zG->apply_($V3);$VG->apply_($X3);$VG->apply_($Z3);$hH->apply_($X3);$oH->apply_($X3);$CH->apply_($X3);$jI->apply_($C);$qI->apply_($C);$xI->apply_($C);$HI->apply_($C);$SI->apply_($C);$jJ->apply_($d4);$rJ->apply_($d4);$KJ->apply_($e4);$ZJ->apply_($e4);$iK->apply_($e4);$BK->apply_($H);$HK->apply_($H);$NK->apply_($H);$dL->apply_($Ol);$ni::self=$uM;&$V($T);&$V($f1);&$V($l1);&$V($y1);&$V($L1);&$V($Y1);&$V($n2);&$V($J2);&$V($P2);&$V($E4);&$V($G4);&$I4($G4);&$V($P4);&$V($l5);&$V($t5);&$V($F5);&$V($R5);&$V($f6);&$V($h6);&$I4($h6);&$V($n6);&$V($E6);&$V($P6);&$V($d7);&$V($p7);&$V($J7);&$V($L7);&$I4($L7);&$V($g8);&$V($n8);&$V($y8);&$V($A8);&$I4($A8);&$V($G8);&$V($Q8);&$V($W8);&$V($e9);&$I4($e9);&$V($k9);&$V($E9);&$V($V9);&$V($la);&$V($Ja);&$V($Ra);&$I4($Ra);&$V($Xa);&$V($hb);&$V($sb);&$V($Hb);&$V($Nb);&$V($Ub);&$V($Wb);&$I4($Wb);&$V($tc);&$V($zc);&$V($Hc);&$V($Nc);&$V($Qc);&$I4($Qc);&$V($Xc);&$V($gd);&$V($id);&$I4($id);&$V($rd);&$I4($rd);&$V($ud);&$I4($ud);&$V($wd);&$I4($wd);&$V($Bd);&$I4($Bd);&$V($Fd);&$I4($Fd);&$V($Od);&$V($ie);&$V($ke);&$I4($ke);&$V($xe);&$V($ze);&$I4($ze);&$V($Be);&$I4($Be);&$V($Me);&$V($Te);&$V($Ve);&$I4($Ve);&$V($cf);&$I4($cf);&$V($of);&$V($vf);&$V($Pf);&$V($lg);&$V($tg);&$V($Jg);&$V($Lg);&$I4($Lg);&$V($Qg);&$I4($Qg);&$V($ih);&$V($ph);&$V($xh);&$V($Nh);&$V($Yh);&$V($oi);&$V($ri);&$I4($ri);&$ui($ri);&$V($Ri);&$V($Ti);&$I4($Ti);&$V($lj);&$V($tj);&$V($Aj);&$V($Qj);&$V($Yj);&$V($vk);&$V($xk);&$I4($xk);&$V($Ck);&$I4($Ck);&$V($Rk);&$V($cl);&$V($el);&$I4($el);&$V($jl);&$I4($jl);&$V($Nl);&$V($Yl);&$V($nm);&$V($pm);&$I4($pm);&$V($um);&$I4($um);&$V($Lm);&$I4($Lm);&$V($ln);&$V($sn);&$V($Gn);&$V($In);&$I4($In);&$V($Nn);&$I4($Nn);&$V($eo);&$V($qo);&$V($so);&$I4($so);&$V($Fo);&$V($Zo);&$V($dp);&$I4($dp);&$gp($dp);&$V($np);&$I4($np);&$V($rp);&$V($tp);&$I4($tp);&$V($Np);&$V($Up);&$V($gq);&$V($sq);&$V($xq);&$I4($xq);&$gp($xq);&$Aq($xq);&$V($Tq);&$V($Vq);&$I4($Vq);&$V($or);&$V($vr);&$V($xr);&$I4($xr);&$gp($xr);&$V($Cr);&$I4($Cr);&$V($Jr);&$I4($Jr);&$V($Or);&$V($Tr);&$I4($Tr);&$V($gs);&$V($ms);&$V($os);&$I4($os);&$V($us);&$I4($us);&$V($Ds);&$I4($Ds);&$V($Ns);&$V($Vs);&$V($et);&$V($lt);&$V($nt);&$V($xt);&$V($Et);&$V($Ot);&$V($Wt);&$V($eu);&$V($ku);&$V($uu);&$V($wu);&$I4($wu);&$V($Gu);&$V($Iu);&$I4($Iu);&$V($mv);&$V($Ev);&$V($Nv);&$V($Uv);&$V($Wv);&$I4($Wv);&$Zv($Wv);&$V($mw);&$V($ow);&$I4($ow);&$V($Fw);&$V($Mw);&$V($ix);&$V($Cx);&$V($Ex);&$I4($Ex);&$V($Jx);&$I4($Jx);&$V($Vx);&$V($ky);&$V($vy);&$V($Ey);&$V($Gy);&$V($Ny);&$I4($Ny);&$V($wz);&$V($Sz);&$V($cA);&$V($jA);&$V($yA);&$V($QA);&$V($nB);&$V($xB);&$V($HB);&$V($JB);&$I4($JB);&$V($OB);&$I4($OB);&$V($NC);&$V($WC);&$V($YC);&$I4($YC);&$V($fD);&$I4($fD);&$V($lD);&$V($CD);&$V($KD);&$V($gE);&$V($wE);&$V($FE);&$V($OE);&$V($nF);&$V($pF);&$I4($pF);&$V($uF);&$I4($uF);&$V($EF);&$V($LF);&$I4($LF);&$V($jG);&$V($tG);&$V($zG);&$V($BG);&$I4($BG);&$V($HG);&$I4($HG);&$V($VG);&$V($XG);&$I4($XG);&$V($hH);&$V($oH);&$V($CH);&$V($EH);&$I4($EH);&$V($LH);&$I4($LH);&$V($NH);&$I4($NH);&$V($jI);&$V($qI);&$V($xI);&$V($HI);&$V($SI);&$V($UI);&$I4($UI);&$XI($UI);&$V($jJ);&$V($rJ);&$V($tJ);&$I4($tJ);&$V($KJ);&$V($ZJ);&$V($iK);&$V($kK);&$I4($kK);&$V($pK);&$I4($pK);&$V($BK);&$V($HK);&$V($NK);&$V($PK);&$I4($PK);&$V($UK);&$I4($UK);&$V($dL);&$V($lL);&$I4($lL);&$V($qL);&$I4($qL);&$V($zL);&$I4($zL);&$V($EL);&$I4($EL);&$V($JL);&$I4($JL);&$V($RL);&$I4($RL);&$V($VL);&$I4($VL);ni->run(@ARGV);
__DATA__
