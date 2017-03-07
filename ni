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
  ...#;$N9=[$f,$M9];$O9={$S,1};$P9=q#/lib/doc#;$Q9={};$R9=q#shift; +{name => shift, doc => []}#;$S9=bless({$w,$R9,$y,$z},$A);$T9={$K6,$S9};$U9=q#/lib/doc_init.b#;$V9=bless({$T2,$Q9,$u4,$q,$v4,$q,$w4,$T9,$Q,$U9},$T3);$W9={};$X9=q#'ni.doc'#;$Y9=bless({$w,$X9,$y,$z},$A);$Z9={$Z8,$Y9};$ca=q#/lib/doc_namespace.b#;$da=bless({$T2,$W9,$u4,$q,$v4,$q,$w4,$Z9,$Q,$ca},$T3);$ea={};$fa=q#(@{}#;$ga=q#[map @$_, @{shift->{doc}}]#;$ha=bless({$w,$ga,$y,$z},$A);$ia=q#AUTOLOAD#;$ja=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$ka=bless({$w,$ja,$y,$z},$A);$la={$fa,$ha,$ia,$ka};$ma=q#/lib/doc_define.b#;$na=bless({$T2,$ea,$u4,$q,$v4,$q,$w4,$la,$Q,$ma},$T3);$oa={};$pa=q#end#;$qa=q#shift->referent#;$ra=bless({$w,$qa,$y,$z},$A);$sa=q#ni 'ni:' . shift->{name}#;$ta=bless({$w,$sa,$y,$z},$A);$ua={$pa,$ra,$S2,$ta};$va=q#/lib/doc_end.b#;$wa=bless({$T2,$oa,$u4,$q,$v4,$q,$w4,$ua,$Q,$va},$T3);$xa={};$ya=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$za=bless({$w,$ya,$y,$z},$A);$Aa={$R2,$za};$Ba=q#/lib/doc_TODO.b#;$Ca=bless({$T2,$xa,$u4,$q,$v4,$q,$w4,$Aa,$Q,$Ba},$T3);$Da={};$Ea=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Fa=bless({$w,$Ea,$y,$z},$A);$Ga={$q6,$Fa};$Ha=q#/lib/doc_eg.b#;$Ia=bless({$T2,$Da,$u4,$q,$v4,$q,$w4,$Ga,$Q,$Ha},$T3);$Ja={};$Ka=q#tests#;$La=q#my $self = shift;
my $test_case_class = ni('ni:/lib/test_case')->package;
map $_->referent($self->referent), grep ref($_) eq $test_case_class,
    map @$_, @{$$self{doc}};#;$Ma=bless({$w,$La,$y,$z},$A);$Na=q#todos#;$Oa=q#my $self = shift;
my $todo_class = ni('ni:/lib/todo')->package;
map $_->referent($self->referent), grep ref($_) eq $todo_class,
    map @$_, @{$$self{doc}};#;$Pa=bless({$w,$Oa,$y,$z},$A);$Qa={$Ka,$Ma,$Na,$Pa};$Ra=q#/lib/doc_process.b#;$Sa=bless({$T2,$Ja,$u4,$q,$v4,$q,$w4,$Qa,$Q,$Ra},$T3);$Ta=[$H4,$H8,$V9,$da,$na,$wa,$Ca,$Ia,$Sa];$Ua=bless({$T2,$O9,$Q,$P9,$i3,$Ta},$H3);$Va=q#Use a better preprocessor; ni::outdent fixes indentation, but we also
want to unwrap lines unless they're split at paragraphs (like
markdown).#;$Wa=[$Va];$Xa=bless({$S2,$Ua,$E,$Wa},$H);$Ya=[$R2,$Xa];$Za=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$cb=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$db=[];$eb=[];$fb=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$gb=bless({$t,$eb,$v,$q,$w,$fb,$y,$z},$A);$hb=bless({$n,$db,$p,$q,$r,$q,$s,$gb},$C);$ib=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$jb=[];$kb=[];$lb=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$mb=bless({$t,$kb,$v,$q,$w,$lb,$y,$z},$A);$nb=bless({$n,$jb,$p,$q,$r,$q,$s,$mb},$C);$ob=[$i,$Za,$cb,$hb,$ib,$nb];$pb=[$N9,$Ya,$ob];$qb=bless({$e,$pb,$Q,$P9},$S);$rb=q#ni.doc:/lib/fn#;$sb=q#Give functions a way to name themselves so we can do \#line
reporting#;$tb=[$sb];$ub=bless({$E,$tb},$H);$vb=[$i,$ub];$wb=[$vb];$xb=q#/lib/fn#;$yb=bless({$e,$wb,$Q,$xb},$S);$zb=q#ni.doc:/lib/future#;$Ab=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$Bb=[];$Cb=[];$Db=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$Eb=bless({$t,$Cb,$v,$q,$w,$Db,$y,$z},$A);$Fb=bless({$n,$Bb,$p,$q,$r,$q,$s,$Eb},$C);$Gb=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$Hb=[];$Ib=[];$Jb=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$Kb=bless({$t,$Ib,$v,$q,$w,$Jb,$y,$z},$A);$Lb=bless({$n,$Hb,$p,$q,$r,$q,$s,$Kb},$C);$Mb=[$i,$Ab,$Fb,$Gb,$Lb];$Nb=[$Mb];$Ob=q#/lib/future#;$Pb=bless({$e,$Nb,$Q,$Ob},$S);$Qb=q#ni.doc:/lib/image#;$Rb=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$Sb=[$f,$Rb];$Tb=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$Ub=[$i,$Tb];$Vb={$L3,1};$Wb=q#/lib/image#;$Xb={};$Yb=[];$Zb=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$cc=bless({$t,$Yb,$v,$q,$w,$Zb,$y,$z},$A);$dc={$K6,$cc};$ec=q#/lib/image_init.b#;$fc=bless({$T2,$Xb,$u4,$q,$v4,$q,$w4,$dc,$Q,$ec},$T3);$gc={};$hc=q#boot_side_effect#;$ic=[];$jc=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$kc=bless({$t,$ic,$v,$q,$w,$jc,$y,$z},$A);$lc=q#finalizer#;$mc=[];$nc=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$oc=bless({$t,$mc,$v,$q,$w,$nc,$y,$z},$A);$pc=q#io#;$qc=[];$rc=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$sc=bless({$t,$qc,$v,$q,$w,$rc,$y,$z},$A);$tc=q#reconstruction#;$uc=[];$vc=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$wc=bless({$t,$uc,$v,$q,$w,$vc,$y,$z},$A);$xc=q#side_effect#;$yc=[];$zc=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ac=bless({$t,$yc,$v,$q,$w,$zc,$y,$z},$A);$Bc={$hc,$kc,$lc,$oc,$pc,$sc,$tc,$wc,$xc,$Ac};$Cc=q#/lib/image_quoting.b#;$Dc=bless({$T2,$gc,$u4,$q,$v4,$q,$w4,$Bc,$Q,$Cc},$T3);$Ec={};$Fc=q#quote_code#;$Gc=[];$Hc=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Ic=bless({$t,$Gc,$v,$q,$w,$Hc,$y,$z},$A);$Jc={$Fc,$Ic};$Kc=q#/lib/quote_code_fail.b#;$Lc=bless({$T2,$Ec,$u4,$q,$v4,$q,$w4,$Jc,$Q,$Kc},$T3);$Mc={};$Nc=q#quote_array#;$Oc=[];$Pc=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Qc=bless({$t,$Oc,$v,$q,$w,$Pc,$y,$z},$A);$Rc=q#quote_hash#;$Sc=[];$Tc=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Uc=bless({$t,$Sc,$v,$q,$w,$Tc,$y,$z},$A);$Vc=q#quote_scalar#;$Wc=[];$Xc=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Yc=bless({$t,$Wc,$v,$q,$w,$Xc,$y,$z},$A);$Zc=q#quote_scalar_ref#;$cd=[];$dd=q#'\\\\' . shift->quote(${$_[0]})#;$ed=bless({$t,$cd,$v,$q,$w,$dd,$y,$z},$A);$fd=q#quote_value#;$gd=[];$hd=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$id=bless({$t,$gd,$v,$q,$w,$hd,$y,$z},$A);$jd={$Nc,$Qc,$Rc,$Uc,$Vc,$Yc,$Zc,$ed,$fd,$id};$kd=q#/lib/quote_values.b#;$ld=bless({$T2,$Mc,$u4,$q,$v4,$q,$w4,$jd,$Q,$kd},$T3);$md={};$nd=q#quote_blessed#;$od=[];$pd=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$qd=bless({$t,$od,$v,$q,$w,$pd,$y,$z},$A);$rd=q#quote_class#;$sd=[];$td=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$ud=bless({$t,$sd,$v,$q,$w,$td,$y,$z},$A);$vd=q#quote_object#;$wd=[];$xd=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$yd=bless({$t,$wd,$v,$q,$w,$xd,$y,$z},$A);$zd={$nd,$qd,$rd,$ud,$vd,$yd};$Ad=q#/lib/quote_objects.b#;$Bd=bless({$T2,$md,$u4,$q,$v4,$q,$w4,$zd,$Q,$Ad},$T3);$Cd={};$Dd=q#circular_arrayref#;$Ed=[];$Fd=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Gd=bless({$t,$Ed,$v,$q,$w,$Fd,$y,$z},$A);$Hd=q#circular_hashref#;$Id=[];$Jd=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Kd=bless({$t,$Id,$v,$q,$w,$Jd,$y,$z},$A);$Ld=q#is_circular#;$Md=[];$Nd=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Od=bless({$t,$Md,$v,$q,$w,$Nd,$y,$z},$A);$Pd={$Dd,$Gd,$Hd,$Kd,$Ld,$Od};$Qd=q#/lib/quote_circular_addressed.b#;$Rd=bless({$T2,$Cd,$u4,$q,$v4,$q,$w4,$Pd,$Q,$Qd},$T3);$Sd={};$Td=q#address#;$Ud=[];$Vd=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Wd=bless({$t,$Ud,$v,$q,$w,$Vd,$y,$z},$A);$Xd=q#allocate_gensym#;$Yd=[];$Zd=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$ce=bless({$t,$Yd,$v,$q,$w,$Zd,$y,$z},$A);$de=q#circular_links#;$ee=[];$fe=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$ge=bless({$t,$ee,$v,$q,$w,$fe,$y,$z},$A);$he=q#quote#;$ie=[];$je=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$ke=bless({$t,$ie,$v,$q,$w,$je,$y,$z},$A);$le={$Td,$Wd,$Xd,$ce,$de,$ge,$he,$ke};$me=q#/lib/quote_gensym_identity.b#;$ne=bless({$T2,$Sd,$u4,$q,$v4,$q,$w4,$le,$Q,$me},$T3);$oe={};$pe=q#gensym#;$qe=[];$re=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$se=bless({$t,$qe,$v,$q,$w,$re,$y,$z},$A);$te={$pe,$se};$ue=q#/lib/gensym_generator_compact.b#;$ve=bless({$T2,$oe,$u4,$q,$v4,$q,$w4,$te,$Q,$ue},$T3);$we=[$H4,$fc,$Dc,$Lc,$ld,$Bd,$Rd,$ne,$ve];$xe=bless({$T2,$Vb,$Q,$Wb,$i3,$we},$M3);$ye=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$ze=[$ye];$Ae=bless({$S2,$xe,$E,$ze},$H);$Be=[$R2,$Ae];$Ce=q#Use a packed format with BER length encoding (which is portable), and
serialize the apply-journals for behaviors.#;$De=[$Ce];$Ee=bless({$S2,$xe,$E,$De},$H);$Fe=[$R2,$Ee];$Ge=[$Sb,$Ub,$Be,$Fe];$He=bless({$e,$Ge,$Q,$Wb},$S);$Ie=q#ni.doc:/lib/ni#;$Je=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$Ke=[$f,$Je];$Le=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$Me=[$i,$Le];$Ne=[$Ke,$Me];$Oe=q#/lib/ni#;$Pe=bless({$e,$Ne,$Q,$Oe},$S);$Qe=q#ni.doc:/lib/quote_simple#;$Re=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$Se=[];$Te=[];$Ue=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$Ve=bless({$t,$Te,$v,$q,$w,$Ue,$y,$z},$A);$We=bless({$n,$Se,$p,$q,$r,$q,$s,$Ve},$C);$Xe=[$i,$Re,$We];$Ye=[$Xe];$Ze=q#/lib/quote_simple#;$cf=bless({$e,$Ye,$Q,$Ze},$S);$df=q#ni.doc:/lib/slice#;$ef=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$ff=[$f,$ef];$gf={$T3,1};$hf=q#/lib/slice#;$if=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$jf=bless({$w,$if,$y,$z},$A);$kf=q#local $_;
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
$self;#;$lf=bless({$w,$kf,$y,$z},$A);$mf=q#ni::/lib/slice::apply#;$nf=q#ni::/lib/slice::apply_#;$of={};$pf={$J8,$jf,$M8,$lf};$qf=q#/lib/slice.b#;$rf=bless({$T2,$of,$w4,$pf,$Q,$qf},$T3);$sf={};$tf=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$uf=bless({$w,$tf,$y,$z},$A);$vf={$K6,$uf};$wf=q#/lib/slice_init.b#;$xf=bless({$T2,$sf,$w4,$vf,$Q,$wf},$T3);$yf=[$B8,$H8,$rf,$xf,$C9];$zf=bless({$T2,$gf,$Q,$hf,$i3,$yf},$U3);$Af=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$Bf=[$Af];$Cf=bless({$S2,$zf,$E,$Bf},$H);$Df=[$R2,$Cf];$Ef=q#Anything on the Perl definition-side of things needs to be version
controlled. The case we need to handle here is running unit tests inside
a remote; at the end, we should be able to restore the remote state
cleanly (or branch it, or something). We don't want unit test results to
hang around and corrupt the image we then propagate from there.#;$Ff=[$Ef];$Gf=bless({$S2,$zf,$E,$Ff},$H);$Hf=[$R2,$Gf];$If=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$Jf=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$Kf=[];$Lf=[];$Mf=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$Nf=bless({$t,$Lf,$v,$q,$w,$Mf,$y,$z},$A);$Of=bless({$n,$Kf,$p,$q,$r,$q,$s,$Nf},$C);$Pf=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$Qf=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$Rf=[];$Sf=[];$Tf=q#my $instances = 0;
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
now $instances == 0;#;$Uf=bless({$t,$Sf,$v,$q,$w,$Tf,$y,$z},$A);$Vf=bless({$n,$Rf,$p,$q,$r,$q,$s,$Uf},$C);$Wf=[$i,$If,$Jf,$Of,$Pf,$Qf,$Vf];$Xf=[$ff,$Df,$Hf,$Wf];$Yf=bless({$e,$Xf,$Q,$hf},$S);$Zf=q#ni.doc:/semantic#;$cg=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$dg=[$i,$cg];$eg=[$dg];$fg=q#/semantic#;$gg=bless({$e,$eg,$Q,$fg},$S);$hg=q#ni.doc:ni#;$ig=q#ni#;$jg={$ig,1};$kg={};$lg=q#abbrev#;$mg=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$ng=bless({$w,$mg,$y,$z},$A);$og=q#dor#;$pg=q#defined $_[0] ? $_[0] : $_[1]#;$qg=bless({$w,$pg,$y,$z},$A);$rg=q#indent#;$sg=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$tg=bless({$w,$sg,$y,$z},$A);$ug=q#max#;$vg=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$wg=bless({$w,$vg,$y,$z},$A);$xg=q#maxstr#;$yg=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$zg=bless({$w,$yg,$y,$z},$A);$Ag=q#mean#;$Bg=q#sum(@_) / (@_ || 1)#;$Cg=bless({$w,$Bg,$y,$z},$A);$Dg=q#min#;$Eg=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$Fg=bless({$w,$Eg,$y,$z},$A);$Gg=q#minstr#;$Hg=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$Ig=bless({$w,$Hg,$y,$z},$A);$Jg=q#outdent#;$Kg=q#my $x = shift;
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
join "\\n", @lines;#;$Lg=bless({$w,$Kg,$y,$z},$A);$Mg=q#sgr#;$Ng=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$Og=bless({$w,$Ng,$y,$z},$A);$Pg=q#sr#;$Qg=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$Rg=bless({$w,$Qg,$y,$z},$A);$Sg=q#sum#;$Tg=q#local $_; my $x = 0; $x += $_ for @_; $x#;$Ug=bless({$w,$Tg,$y,$z},$A);$Vg=q#swap#;$Wg=q#@_[0, 1] = @_[1, 0]#;$Xg=bless({$w,$Wg,$y,$z},$A);$Yg={$lg,$ng,$og,$qg,$rg,$tg,$ug,$wg,$xg,$zg,$Ag,$Cg,$Dg,$Fg,$Gg,$Ig,$Jg,$Lg,$Mg,$Og,$Pg,$Rg,$Sg,$Ug,$Vg,$Xg};$Zg=q#/lib/ni_static_util.b#;$ch=bless({$T2,$kg,$u4,$q,$v4,$q,$w4,$Yg,$Q,$Zg},$T3);$dh={};$eh=q#data#;$fh=q#json_escapes#;$gh=q##;$hh=q#b#;$ih=q#	#;$jh=q#t#;$kh=q#
#;$lh=q#n#;$mh=q##;$nh=q#f#;$oh=q##;$ph=q#r#;$qh=q#"#;$rh=q#/#;$sh=q#\\#;$th={$gh,$hh,$ih,$jh,$kh,$lh,$mh,$nh,$oh,$ph,$qh,$qh,$rh,$rh,$sh,$sh};$uh=q#json_unescapes#;$vh={$qh,$qh,$rh,$rh,$sh,$sh,$hh,$gh,$nh,$mh,$lh,$kh,$ph,$oh,$jh,$ih};$wh={$fh,$th,$uh,$vh};$xh=q#/lib/json_data.b#;$yh=bless({$T2,$dh,$eh,$wh,$Q,$xh},$F3);$zh={};$Ah=q#json_decode#;$Bh=[];$Ch=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Dh=q#($)#;$Eh=bless({$t,$Bh,$v,$q,$w,$Ch,$y,$Dh},$A);$Fh=q#json_encode#;$Gh=[];$Hh=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Ih=bless({$t,$Gh,$v,$q,$w,$Hh,$y,$Dh},$A);$Jh=q#json_encode_pretty#;$Kh=[];$Lh=q#local $_;
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

ni::json_encode($v);#;$Mh=bless({$t,$Kh,$v,$q,$w,$Lh,$y,$z},$A);$Nh=q#json_escape#;$Oh=[];$Ph=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$Qh=bless({$t,$Oh,$v,$q,$w,$Ph,$y,$Dh},$A);$Rh=q#json_unescape#;$Sh=[];$Th=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$Uh=bless({$t,$Sh,$v,$q,$w,$Th,$y,$Dh},$A);$Vh=q#json_unescape_one#;$Wh=[];$Xh=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$Yh=bless({$t,$Wh,$v,$q,$w,$Xh,$y,$Dh},$A);$Zh={$Ah,$Eh,$Fh,$Ih,$Jh,$Mh,$Nh,$Qh,$Rh,$Uh,$Vh,$Yh};$ci=q#/lib/json.b#;$di=bless({$T2,$zh,$u4,$q,$v4,$q,$w4,$Zh,$Q,$ci},$T3);$ei=[$ch,$yh,$di];$fi=bless({$T2,$jg,$Q,$ig,$i3,$ei},$j4);$gi=q#Migrate JSON into its own module, e.g. ni::json::. Clearly it deserves
some amount of encapsulation, since all of its member functions already
have the same prefix.#;$hi=[$gi];$ii=bless({$S2,$fi,$E,$hi},$H);$ji=[$R2,$ii];$ki=q#Test a "DangerJson" module that does s/// to transform JSON into a valid
Perl expression, then uses eval()#;$li=[$ki];$mi=bless({$S2,$fi,$E,$li},$H);$ni=[$R2,$mi];$oi=[$ji,$ni];$pi=bless({$e,$oi,$Q,$ig},$S);$qi=q#ni:/class#;$ri={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$i4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$si={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$ti=q#/module#;$ui=q#/lib/perlbranch.b#;$vi={};$wi=q#add#;$xi=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$yi=bless({$w,$xi,$y,$z},$A);$zi=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Ai=bless({$w,$zi,$y,$z},$A);$Bi={$wi,$yi,$J8,$Ai};$Ci=q#/lib/branch.b#;$Di=bless({$T2,$vi,$u4,$q,$v4,$q,$w4,$Bi,$Q,$Ci},$T3);$Ei=[$Di,$H8,$g9,$n9,$u9];$Fi=bless({$Q,$ui,$i3,$Ei},$V3);$Gi={};$Hi=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$Ii=bless({$w,$Hi,$y,$z},$A);$Ji={$K6,$Ii};$Ki=q#/lib/class_init.b#;$Li=bless({$T2,$Gi,$u4,$J4,$v4,$q,$w4,$Ji,$Q,$Ki},$T3);$Mi={$j3,1,$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$D3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$h4,1,$i4,1,$j4,1,$k4,1,$m4,1,$n4,1,$o4,1,$q4,1};$Ni=q#/lib/definition.b#;$Oi={};$Pi=q#def#;$Qi=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Ri=bless({$w,$Qi,$y,$z},$A);$Si={$Pi,$Ri};$Ti=q#/lib/definition_def.b#;$Ui=bless({$T2,$Oi,$u4,$q,$v4,$q,$w4,$Si,$Q,$Ti},$T3);$Vi={};$Wi=q#ro#;$Xi=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$Yi=bless({$w,$Xi,$y,$z},$A);$Zi=q#rw#;$cj=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$dj=bless({$w,$cj,$y,$z},$A);$ej={$Wi,$Yi,$Zi,$dj};$fj=q#/lib/accessor.b#;$gj=bless({$T2,$Vi,$u4,$q,$v4,$q,$w4,$ej,$Q,$fj},$T3);$hj={};$ij=q#(""#;$jj=q#shift->name#;$kj=bless({$w,$jj,$y,$z},$A);$lj={$ij,$kj};$mj=q#/lib/name_as_string.b#;$nj=bless({$T2,$hj,$u4,$q,$v4,$q,$w4,$lj,$Q,$mj},$T3);$oj={};$pj=q#(eq#;$qj=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$rj=bless({$w,$qj,$y,$z},$A);$sj={$pj,$rj};$tj=q#/lib/ref_eq.b#;$uj=bless({$T2,$oj,$u4,$q,$v4,$q,$w4,$sj,$Q,$tj},$T3);$vj={};$wj=q#defdata#;$xj=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$yj=bless({$w,$xj,$y,$z},$A);$zj={$wj,$yj};$Aj=q#/lib/definition_defdata.b#;$Bj=bless({$T2,$vj,$u4,$q,$v4,$q,$w4,$zj,$Q,$Aj},$T3);$Cj={};$Dj=q#instantiate_with_defaults#;$Ej=q#my ($class, @slots) = @_;
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
    }));#;$Fj=bless({$w,$Ej,$y,$z},$A);$Gj={$Dj,$Fj};$Hj=q#/lib/definition_init_with_defaults.b#;$Ij=bless({$T2,$Cj,$u4,$q,$v4,$q,$w4,$Gj,$Q,$Hj},$T3);$Jj=[$Ui,$gj,$nj,$uj,$Bj,$Ij];$Kj=bless({$T2,$Mi,$Q,$Ni,$i3,$Jj},$D3);$Lj=[$Fi,$Li,$H4,$B8,$Kj];$Mj=bless({$T2,$si,$Q,$ti,$i3,$Lj},$k4);$Nj={};$Oj=q#new#;$Pj=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$Qj=bless({$w,$Pj,$y,$z},$A);$Rj={$Oj,$Qj};$Sj=q#/lib/instantiable.b#;$Tj=bless({$T2,$Nj,$w4,$Rj,$Q,$Sj},$T3);$Uj={};$Vj=q#child#;$Wj=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$Xj=bless({$w,$Wj,$y,$z},$A);$Yj={$Vj,$Xj};$Zj=q#/lib/subclass.b#;$ck=bless({$T2,$Uj,$u4,$q,$v4,$q,$w4,$Yj,$Q,$Zj},$T3);$dk=[$Mj,$Tj,$Li,$Mj,$ck];$ek=bless({$T2,$ri,$Q,$R,$i3,$dk},$k3);$fk=q#ni:/class.c#;$gk={$k3,1,$o4,1};$hk=q#/class.c#;$ik={$k3,1,$k4,1,$o4,1};$jk=q#/module.c#;$kk={$k3,1,$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1,$w3,1,$y3,1,$A3,1,$C3,1,$E3,1,$G3,1,$H3,1,$I3,1,$K3,1,$M3,1,$O3,1,$Q3,1,$S3,1,$U3,1,$W3,1,$Y3,1,$c4,1,$d4,1,$f4,1,$g4,1,$k4,1,$m4,1,$o4,1,$q4,1};$lk=q#/object.c#;$mk=[$ek];$nk=bless({$T2,$kk,$Q,$lk,$i3,$mk},$h4);$ok={$k3,1,$C3,1,$E3,1,$G3,1,$U3,1,$W3,1,$k4,1,$o4,1};$pk=q#/lib/behavior.c#;$qk=[$nk];$rk=bless({$T2,$ok,$Q,$pk,$i3,$qk},$h4);$sk=[$nk,$Tj,$rk];$tk=bless({$T2,$ik,$Q,$jk,$i3,$sk},$h4);$uk=[$tk];$vk=bless({$T2,$gk,$Q,$hk,$i3,$uk},$h4);$wk=q#ni:/fabric#;$xk=q#ni::/fabric#;$yk={$xk,1};$zk=[];$Ak=bless({$T2,$yk,$Q,$e1,$i3,$zk},$j4);$Bk=q#ni:/io#;$Ck=q#ni::/io#;$Dk={$Ck,1};$Ek=[];$Fk=bless({$T2,$Dk,$Q,$k1,$i3,$Ek},$j4);$Gk=q#ni:/io/buffer#;$Hk={$U2,1};$Ik={};$Jk=[];$Kk=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Lk=bless({$t,$Jk,$v,$q,$w,$Kk,$y,$z},$A);$Mk={$K6,$Lk};$Nk=q#/io/buffer_init.b#;$Ok=bless({$T2,$Ik,$u4,$q,$v4,$q,$w4,$Mk,$Q,$Nk},$T3);$Pk={};$Qk=[];$Rk=q#my $self = shift;
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
}#;$Sk=bless({$t,$Qk,$v,$q,$w,$Rk,$y,$z},$A);$Tk=q#read_capacity#;$Uk=[];$Vk=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Wk=bless({$t,$Uk,$v,$q,$w,$Vk,$y,$z},$A);$Xk=[];$Yk=q#my $self = shift;
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
}#;$Zk=bless({$t,$Xk,$v,$q,$w,$Yk,$y,$z},$A);$cl=q#write_capacity#;$dl=[];$el=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$fl=bless({$t,$dl,$v,$q,$w,$el,$y,$z},$A);$gl={$g7,$Sk,$Tk,$Wk,$k7,$Zk,$cl,$fl};$hl=q#/io/buffer_io.b#;$il=bless({$T2,$Pk,$u4,$q,$v4,$q,$w4,$gl,$Q,$hl},$T3);$jl=[$i6,$Ok,$il];$kl=bless({$T2,$Hk,$Q,$x1,$i3,$jl},$l3);$ll=q#ni:/io/buffer.c#;$ml={$l3,1};$nl=q#/io/buffer.c#;$ol={$l3,1,$m3,1,$n3,1,$o3,1,$p3,1,$q3,1,$r3,1,$s3,1,$t3,1,$u3,1};$pl=q#/io/object.c#;$ql={};$rl=q#def_transfer_method#;$sl=[];$tl=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$ul=bless({$t,$sl,$v,$q,$w,$tl,$y,$z},$A);$vl={$rl,$ul};$wl=q#/io/object.c_transfer_def.b#;$xl=bless({$T2,$ql,$u4,$q,$v4,$q,$w4,$vl,$Q,$wl},$T3);$yl=[$nk,$xl];$zl=bless({$T2,$ol,$Q,$pl,$i3,$yl},$h4);$Al=[$zl];$Bl=bless({$T2,$ml,$Q,$nl,$i3,$Al},$h4);$Cl=q#ni:/io/buffer_init.b#;$Dl=q#ni:/io/buffer_io.b#;$El=q#ni:/io/cat#;$Fl={$V2,1};$Gl={};$Hl=[];$Il=q#shift; +{fs => [@_]}#;$Jl=bless({$t,$Hl,$v,$q,$w,$Il,$y,$z},$A);$Kl={$K6,$Jl};$Ll=q#/io/cat_init.b#;$Ml=bless({$T2,$Gl,$u4,$q,$v4,$q,$w4,$Kl,$Q,$Ll},$T3);$Nl={};$Ol=[];$Pl=q#my $fs = shift->{fs};
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
$total_read;#;$Ql=bless({$t,$Ol,$v,$q,$w,$Pl,$y,$z},$A);$Rl={$g7,$Ql};$Sl=q#/io/cat_read.b#;$Tl=bless({$T2,$Nl,$u4,$q,$v4,$q,$w4,$Rl,$Q,$Sl},$T3);$Ul=[$i6,$Ml,$Tl];$Vl=bless({$T2,$Fl,$Q,$K1,$i3,$Ul},$m3);$Wl=q#ni:/io/cat.c#;$Xl={$m3,1};$Yl=q#/io/cat.c#;$Zl=[$zl];$cm=bless({$T2,$Xl,$Q,$Yl,$i3,$Zl},$h4);$dm=q#ni:/io/cat_init.b#;$em=q#ni:/io/cat_read.b#;$fm=q#ni:/io/exec#;$gm={$W2,1};$hm={};$im=q#argv#;$jm=[];$km=q#shift->{'argv'}#;$lm=bless({$t,$jm,$v,$q,$w,$km,$y,$z},$A);$mm={$im,$lm};$nm=q#/io/exec_ro.b#;$om=bless({$T2,$hm,$u4,$q,$v4,$q,$w4,$mm,$Q,$nm},$T3);$pm={};$qm=[];$rm=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$sm=bless({$t,$qm,$v,$q,$w,$rm,$y,$z},$A);$tm={$K6,$sm};$um=q#/io/exec_init.b#;$vm=bless({$T2,$pm,$u4,$q,$v4,$q,$w4,$tm,$Q,$um},$T3);$wm={};$xm=q#connect#;$ym=[];$zm=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Am=bless({$t,$ym,$v,$q,$w,$zm,$y,$z},$A);$Bm=q#in_pipe#;$Cm=[];$Dm=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Em=bless({$t,$Cm,$v,$q,$w,$Dm,$y,$z},$A);$Fm=q#out_pipe#;$Gm=[];$Hm=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Im=bless({$t,$Gm,$v,$q,$w,$Hm,$y,$z},$A);$Jm=q#setup_stdio#;$Km=[];$Lm=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Mm=bless({$t,$Km,$v,$q,$w,$Lm,$y,$z},$A);$Nm={$xm,$Am,$Bm,$Em,$Fm,$Im,$Jm,$Mm};$Om=q#/io/exec_io_setup.b#;$Pm=bless({$T2,$wm,$u4,$q,$v4,$q,$w4,$Nm,$Q,$Om},$T3);$Qm={};$Rm=q#binds_fd#;$Sm=[];$Tm=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$Um=bless({$t,$Sm,$v,$q,$w,$Tm,$y,$z},$A);$Vm=[];$Wm=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Xm=bless({$t,$Vm,$v,$q,$w,$Wm,$y,$z},$A);$Ym=[];$Zm=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$cn=bless({$t,$Ym,$v,$q,$w,$Zm,$y,$z},$A);$dn=[];$en=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$fn=bless({$t,$dn,$v,$q,$w,$en,$y,$z},$A);$gn=[];$hn=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$in=bless({$t,$gn,$v,$q,$w,$hn,$y,$z},$A);$jn={$Rm,$Um,$s7,$Xm,$w7,$cn,$A7,$fn,$E7,$in};$kn=q#/io/exec_io_accessors.b#;$ln=bless({$T2,$Qm,$u4,$q,$v4,$q,$w4,$jn,$Q,$kn},$T3);$mn={};$nn=q#env#;$on=[];$pn=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$qn=bless({$t,$on,$v,$q,$w,$pn,$y,$z},$A);$rn={$nn,$qn};$sn=q#/io/exec_env.b#;$tn=bless({$T2,$mn,$u4,$q,$v4,$q,$w4,$rn,$Q,$sn},$T3);$un={};$vn=q#exec#;$wn=[];$xn=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$yn=bless({$t,$wn,$v,$q,$w,$xn,$y,$z},$A);$zn=q#fork#;$An=[];$Bn=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Cn=bless({$t,$An,$v,$q,$w,$Bn,$y,$z},$A);$Dn=q#move_fds#;$En=[];$Fn=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Gn=bless({$t,$En,$v,$q,$w,$Fn,$y,$z},$A);$Hn={$vn,$yn,$zn,$Cn,$Dn,$Gn};$In=q#/io/exec_fork.b#;$Jn=bless({$T2,$un,$u4,$q,$v4,$q,$w4,$Hn,$Q,$In},$T3);$Kn=[$i6,$om,$vm,$Pm,$ln,$tn,$Jn];$Ln=bless({$T2,$gm,$Q,$X1,$i3,$Kn},$n3);$Mn=q#ni:/io/exec.c#;$Nn={$n3,1};$On=q#/io/exec.c#;$Pn=[$zl];$Qn=bless({$T2,$Nn,$Q,$On,$i3,$Pn},$h4);$Rn=q#ni:/io/exec_env.b#;$Sn=q#ni:/io/exec_fork.b#;$Tn=q#ni:/io/exec_init.b#;$Un=q#ni:/io/exec_io_accessors.b#;$Vn=q#ni:/io/exec_io_setup.b#;$Wn=q#ni:/io/exec_ro.b#;$Xn=q#ni:/io/fd#;$Yn={$X2,1};$Zn=q#read_fd_mask#;$co={};$do=[];$eo=q#shift->{'fd'}#;$fo=bless({$t,$do,$v,$q,$w,$eo,$y,$z},$A);$go={$s7,$fo};$ho=q#/io/fd_readers.b#;$io=bless({$T2,$co,$u4,$q,$v4,$q,$w4,$go,$Q,$ho},$T3);$jo={};$ko=[];$lo=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$mo=bless({$t,$ko,$v,$q,$w,$lo,$y,$z},$A);$no={$K6,$mo};$oo=q#/io/fd_init.b#;$po=bless({$T2,$jo,$u4,$q,$v4,$q,$w4,$no,$Q,$oo},$T3);$qo={};$ro=q#be#;$so=[];$to=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$uo=bless({$t,$so,$v,$q,$w,$to,$y,$z},$A);$vo={$ro,$uo};$wo=q#/io/fd_shell.b#;$xo=bless({$T2,$qo,$u4,$q,$v4,$q,$w4,$vo,$Q,$wo},$T3);$yo={};$zo=q#cloexec#;$Ao=[];$Bo=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Co=bless({$t,$Ao,$v,$q,$w,$Bo,$y,$z},$A);$Do=q#fcntl_flag#;$Eo=[];$Fo=q#my ($self, $flag, $value) = @_;
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
}#;$Go=bless({$t,$Eo,$v,$q,$w,$Fo,$y,$z},$A);$Ho=q#nonblock#;$Io=[];$Jo=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Ko=bless({$t,$Io,$v,$q,$w,$Jo,$y,$z},$A);$Lo={$zo,$Co,$Do,$Go,$Ho,$Ko};$Mo=q#/io/fd_fcntl.b#;$No=bless({$T2,$yo,$u4,$q,$v4,$q,$w4,$Lo,$Q,$Mo},$T3);$Oo={};$Po=[];$Qo=q#shift->close#;$Ro=bless({$t,$Po,$v,$q,$w,$Qo,$y,$z},$A);$So=q#close#;$To=[];$Uo=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Vo=bless({$t,$To,$v,$q,$w,$Uo,$y,$z},$A);$Wo={$So,$Vo};$Xo=q#/io/fd_gc.b#;$Yo=bless({$T2,$Oo,$u4,$q,$v4,$Ro,$w4,$Wo,$Q,$Xo},$T3);$Zo={};$cp=q#close_perl_ios#;$dp=[];$ep=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$fp=bless({$t,$dp,$v,$q,$w,$ep,$y,$z},$A);$gp=[];$hp=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$ip=bless({$t,$gp,$v,$q,$w,$hp,$y,$z},$A);$jp=[];$kp=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$lp=bless({$t,$jp,$v,$q,$w,$kp,$y,$z},$A);$mp={$cp,$fp,$g7,$ip,$k7,$lp};$np=q#/io/fd_perlio.b#;$op=bless({$T2,$Zo,$u4,$q,$v4,$q,$w4,$mp,$Q,$np},$T3);$pp=[$i6,$io,$po,$xo,$No,$Yo,$op];$qp=q#write_fd_mask#;$rp=bless({$T2,$Yn,$Q,$m2,$Zn,$z,$i3,$pp,$qp,$z},$o3);$sp=[];$tp=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$up=bless({$t,$sp,$v,$q,$w,$tp,$y,$z},$A);$vp=q#ni:/io/fd.c#;$wp={$o3,1};$xp=q#/io/fd.c#;$yp={};$zp=q#clear_fd#;$Ap=[];$Bp=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Cp=bless({$t,$Ap,$v,$q,$w,$Bp,$y,$z},$A);$Dp=q#read_fd#;$Ep=[];$Fp=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Gp=bless({$t,$Ep,$v,$q,$w,$Fp,$y,$z},$A);$Hp=q#select#;$Ip=[];$Jp=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Kp=bless({$t,$Ip,$v,$q,$w,$Jp,$y,$z},$A);$Lp=q#write_fd#;$Mp=[];$Np=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Op=bless({$t,$Mp,$v,$q,$w,$Np,$y,$z},$A);$Pp={$zp,$Cp,$Dp,$Gp,$Hp,$Kp,$Lp,$Op};$Qp=q#/io/fd.c_selector.b#;$Rp=bless({$T2,$yp,$u4,$up,$v4,$q,$w4,$Pp,$Q,$Qp},$T3);$Sp=[$zl,$Rp];$Tp=bless({$T2,$wp,$Q,$xp,$i3,$Sp},$h4);$Up=q#ni:/io/fd.c_selector.b#;$Vp=q#ni:/io/fd_fcntl.b#;$Wp=q#ni:/io/fd_gc.b#;$Xp=q#ni:/io/fd_init.b#;$Yp=q#ni:/io/fd_perlio.b#;$Zp=q#ni:/io/fd_readers.b#;$cq=q#ni:/io/fd_shell.b#;$dq=q#ni:/io/file#;$eq={$Y2,1};$fq={};$gq=[];$hq=q#shift->{'name'}#;$iq=bless({$t,$gq,$v,$q,$w,$hq,$y,$z},$A);$jq={$Q,$iq};$kq=q#/io/file_readers.b#;$lq=bless({$T2,$fq,$u4,$q,$v4,$q,$w4,$jq,$Q,$kq},$T3);$mq={};$nq=q#mode#;$oq=[];$pq=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$qq=bless({$t,$oq,$v,$q,$w,$pq,$y,$z},$A);$rq={$nq,$qq};$sq=q#/io/file_accessors.b#;$tq=bless({$T2,$mq,$u4,$q,$v4,$q,$w4,$rq,$Q,$sq},$T3);$uq={};$vq=[];$wq=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$xq=bless({$t,$vq,$v,$q,$w,$wq,$y,$z},$A);$yq={$K6,$xq};$zq=q#/io/file_init.b#;$Aq=bless({$T2,$uq,$u4,$q,$v4,$q,$w4,$yq,$Q,$zq},$T3);$Bq={};$Cq=q#(-X#;$Dq=[];$Eq=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Fq=bless({$t,$Dq,$v,$q,$w,$Eq,$y,$z},$A);$Gq=q#mv#;$Hq=[];$Iq=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Jq=bless({$t,$Hq,$v,$q,$w,$Iq,$y,$z},$A);$Kq=q#rm#;$Lq=[];$Mq=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Nq=bless({$t,$Lq,$v,$q,$w,$Mq,$y,$z},$A);$Oq={$Cq,$Fq,$Gq,$Jq,$Kq,$Nq};$Pq=q#/io/file_fns.b#;$Qq=bless({$T2,$Bq,$u4,$q,$v4,$q,$w4,$Oq,$Q,$Pq},$T3);$Rq={};$Sq=q#atomic_update#;$Tq=[];$Uq=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Vq=bless({$t,$Tq,$v,$q,$w,$Uq,$y,$z},$A);$Wq={$Sq,$Vq};$Xq=q#/io/file_update.b#;$Yq=bless({$T2,$Rq,$u4,$q,$v4,$q,$w4,$Wq,$Q,$Xq},$T3);$Zq={};$cr=[];$dr=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$er=bless({$t,$cr,$v,$q,$w,$dr,$y,$z},$A);$fr=[];$gr=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$hr=bless({$t,$fr,$v,$q,$w,$gr,$y,$z},$A);$ir=[];$jr=q#shift->r->read(@_)#;$kr=bless({$t,$ir,$v,$q,$w,$jr,$y,$z},$A);$lr=q#w#;$mr=[];$nr=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$or=bless({$t,$mr,$v,$q,$w,$nr,$y,$z},$A);$pr=[];$qr=q#shift->w->write(@_)#;$rr=bless({$t,$pr,$v,$q,$w,$qr,$y,$z},$A);$sr={$So,$er,$ph,$hr,$g7,$kr,$lr,$or,$k7,$rr};$tr=q#/io/file_io.b#;$ur=bless({$T2,$Zq,$u4,$q,$v4,$q,$w4,$sr,$Q,$tr},$T3);$vr=[$i6,$lq,$tq,$Aq,$Qq,$Yq,$ur];$wr=bless({$T2,$eq,$Q,$I2,$i3,$vr},$p3);$xr=q#ni:/io/file.c#;$yr={$p3,1};$zr=q#/io/file.c#;$Ar=[$zl];$Br=bless({$T2,$yr,$Q,$zr,$i3,$Ar},$h4);$Cr=q#ni:/io/file_accessors.b#;$Dr=q#ni:/io/file_fns.b#;$Er=q#ni:/io/file_init.b#;$Fr=q#ni:/io/file_io.b#;$Gr=q#ni:/io/file_readers.b#;$Hr=q#ni:/io/file_update.b#;$Ir=q#ni:/io/file_update_fd#;$Jr={$Z2,1};$Kr={};$Lr=[];$Mr=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Nr=bless({$t,$Lr,$v,$q,$w,$Mr,$y,$z},$A);$Or={$K6,$Nr};$Pr=q#/io/file_update_fd_fd_init.b#;$Qr=bless({$T2,$Kr,$u4,$q,$v4,$q,$w4,$Or,$Q,$Pr},$T3);$Rr={};$Sr=[];$Tr=bless({$t,$Sr,$v,$q,$w,$Qo,$y,$z},$A);$Ur=[];$Vr=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Wr=bless({$t,$Ur,$v,$q,$w,$Vr,$y,$z},$A);$Xr={$So,$Wr};$Yr=q#/io/file_update_fd_fd_gc.b#;$Zr=bless({$T2,$Rr,$u4,$q,$v4,$Tr,$w4,$Xr,$Q,$Yr},$T3);$cs=[$i6,$io,$No,$op,$Qr,$Zr];$ds=bless({$T2,$Jr,$Q,$O2,$i3,$cs},$q3);$es=q#ni:/io/file_update_fd.c#;$fs={$q3,1};$gs=q#/io/file_update_fd.c#;$hs=[$zl];$is=bless({$T2,$fs,$Q,$gs,$i3,$hs},$h4);$js=q#ni:/io/file_update_fd_fd_gc.b#;$ks=q#ni:/io/file_update_fd_fd_init.b#;$ls=q#ni:/io/named_io_fns.b#;$ms={};$ns=q#fcntl#;$os=[];$ps=q#CORE::fcntl $_[0], $_[1], $_[2]#;$qs=bless({$t,$os,$v,$q,$w,$ps,$y,$z},$A);$rs=[];$ss=q#CORE::fork#;$ts=bless({$t,$rs,$v,$q,$w,$ss,$y,$z},$A);$us=q#open2#;$vs=[];$ws=q#CORE::open $_[0], $_[1]#;$xs=bless({$t,$vs,$v,$q,$w,$ws,$y,$z},$A);$ys=q#rename#;$zs=[];$As=q#CORE::rename $_[0], $_[1]#;$Bs=bless({$t,$zs,$v,$q,$w,$As,$y,$z},$A);$Cs=q#unlink#;$Ds=[];$Es=q#CORE::unlink @_#;$Fs=bless({$t,$Ds,$v,$q,$w,$Es,$y,$z},$A);$Gs=q#waitpid#;$Hs=[];$Is=q#CORE::waitpid $_[0], $_[1]#;$Js=bless({$t,$Hs,$v,$q,$w,$Is,$y,$z},$A);$Ks={$ns,$qs,$zn,$ts,$us,$xs,$ys,$Bs,$Cs,$Fs,$Gs,$Js};$Ls=q#/io/named_io_fns.b#;$Ms=bless({$T2,$ms,$u4,$q,$v4,$q,$w4,$Ks,$Q,$Ls},$T3);$Ns=q#main#;$Os=q#ni:/io/null#;$Ps={$c3,1};$Qs=q#/io/null#;$Rs={};$Ss=[];$Ts=q#+{fd => undef}#;$Us=bless({$t,$Ss,$v,$q,$w,$Ts,$y,$z},$A);$Vs={$K6,$Us};$Ws=q#/io/null_init.b#;$Xs=bless({$T2,$Rs,$u4,$q,$v4,$q,$w4,$Vs,$Q,$Ws},$T3);$Ys={};$Zs=[];$ct=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$dt=bless({$t,$Zs,$v,$q,$w,$ct,$y,$z},$A);$et=[];$ft=q#shift->fd->read(@_)#;$gt=bless({$t,$et,$v,$q,$w,$ft,$y,$z},$A);$ht=[];$it=q#shift->fd->write(@_)#;$jt=bless({$t,$ht,$v,$q,$w,$it,$y,$z},$A);$kt={$s7,$dt,$g7,$gt,$k7,$jt};$lt=q#/io/null_io.b#;$mt=bless({$T2,$Ys,$u4,$q,$v4,$q,$w4,$kt,$Q,$lt},$T3);$nt=[$i6,$Xs,$mt];$ot=bless({$T2,$Ps,$Q,$Qs,$i3,$nt},$r3);$pt=q#ni:/io/null.c#;$qt={$r3,1};$rt=q#/io/null.c#;$st=[$zl];$tt=bless({$T2,$qt,$Q,$rt,$i3,$st},$h4);$ut=q#ni:/io/null_init.b#;$vt=q#ni:/io/null_io.b#;$wt=q#ni:/io/object#;$xt=q#ni:/io/object.c#;$yt=q#ni:/io/object.c_transfer_def.b#;$zt=q#ni:/io/object_checks.b#;$At=q#ni:/io/object_constructors.b#;$Bt=q#ni:/io/object_memory.b#;$Ct=q#ni:/io/object_ops.b#;$Dt=q#ni:/io/object_transfer_async.b#;$Et=q#ni:/io/object_transfer_sync.b#;$Ft=q#ni:/io/pid#;$Gt=q#ni:/io/pid.c#;$Ht={$t3,1};$It=q#/io/pid.c#;$Jt=[$zl];$Kt=bless({$T2,$Ht,$Q,$It,$i3,$Jt},$h4);$Lt=q#ni:/io/pid_accessors.b#;$Mt=q#ni:/io/pid_init.b#;$Nt=q#ni:/io/pid_io.b#;$Ot=q#ni:/io/pid_readers.b#;$Pt=q#ni:/io/pid_wait.b#;$Qt=q#ni:/io/str#;$Rt={$f3,1};$St=q#/io/str#;$Tt={};$Ut=[];$Vt=q#shift->{'data'}#;$Wt=bless({$t,$Ut,$v,$q,$w,$Vt,$y,$z},$A);$Xt=[];$Yt=q#shift->{'end'}#;$Zt=bless({$t,$Xt,$v,$q,$w,$Yt,$y,$z},$A);$cu=q#start#;$du=[];$eu=q#shift->{'start'}#;$fu=bless({$t,$du,$v,$q,$w,$eu,$y,$z},$A);$gu={$eh,$Wt,$pa,$Zt,$cu,$fu};$hu=q#/io/str_ro.b#;$iu=bless({$T2,$Tt,$u4,$q,$v4,$q,$w4,$gu,$Q,$hu},$T3);$ju={};$ku=[];$lu=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$mu=bless({$t,$ku,$v,$q,$w,$lu,$y,$z},$A);$nu={$K6,$mu};$ou=q#/io/str_init.b#;$pu=bless({$T2,$ju,$u4,$q,$v4,$q,$w4,$nu,$Q,$ou},$T3);$qu={};$ru=[];$su=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$tu=bless({$t,$ru,$v,$q,$w,$su,$y,$z},$A);$uu=q#remaining#;$vu=[];$wu=q#my $self = shift; $$self{end} - $$self{start}#;$xu=bless({$t,$vu,$v,$q,$w,$wu,$y,$z},$A);$yu=[];$zu=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Au=bless({$t,$yu,$v,$q,$w,$zu,$y,$z},$A);$Bu={$g7,$tu,$uu,$xu,$k7,$Au};$Cu=q#/io/str_io.b#;$Du=bless({$T2,$qu,$u4,$q,$v4,$q,$w4,$Bu,$Q,$Cu},$T3);$Eu=[$i6,$iu,$pu,$Du];$Fu=bless({$T2,$Rt,$Q,$St,$i3,$Eu},$u3);$Gu=q#ni:/io/str.c#;$Hu={$u3,1};$Iu=q#/io/str.c#;$Ju=[$zl];$Ku=bless({$T2,$Hu,$Q,$Iu,$i3,$Ju},$h4);$Lu=q#ni:/io/str_init.b#;$Mu=q#ni:/io/str_io.b#;$Nu=q#ni:/io/str_ro.b#;$Ou=q#ni:/io/transfer#;$Pu={$v3,1,$x3,1,$z3,1};$Qu=q#/io/transfer#;$Ru={$v3,1,$x3,1,$z3,1,$p4,1};$Su=q#/semantic/task#;$Tu={};$Uu=[];$Vu=q#shift->{'outcome'}#;$Wu=bless({$t,$Uu,$v,$q,$w,$Vu,$y,$z},$A);$Xu={$r,$Wu};$Yu=q#/semantic/task_ro.b#;$Zu=bless({$T2,$Tu,$u4,$q,$v4,$q,$w4,$Xu,$Q,$Yu},$T3);$cv={};$dv=q#failure#;$ev=[];$fv=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$gv=bless({$t,$ev,$v,$q,$w,$fv,$y,$z},$A);$hv=q#success#;$iv=[];$jv=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$kv=bless({$t,$iv,$v,$q,$w,$jv,$y,$z},$A);$lv={$dv,$gv,$hv,$kv};$mv=q#/semantic/task_outcome.b#;$nv=bless({$T2,$cv,$u4,$q,$v4,$q,$w4,$lv,$Q,$mv},$T3);$ov=[$H4,$Zu,$nv];$pv=bless({$T2,$Ru,$Q,$Su,$i3,$ov},$q4);$qv={};$rv=[];$sv=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$tv=bless({$t,$rv,$v,$q,$w,$sv,$y,$z},$A);$uv=[];$vv=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$wv=bless({$t,$uv,$v,$q,$w,$vv,$y,$z},$A);$xv=[];$yv=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$zv=bless({$t,$xv,$v,$q,$w,$yv,$y,$z},$A);$Av={$g7,$wv,$k7,$zv};$Bv=q#/io/transfer_io_interop.b#;$Cv=bless({$T2,$qv,$u4,$tv,$v4,$q,$w4,$Av,$Q,$Bv},$T3);$Dv={};$Ev=q#pressure#;$Fv=[];$Gv=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Hv=bless({$t,$Fv,$v,$q,$w,$Gv,$y,$z},$A);$Iv=q#read_limit_throughput#;$Jv=[];$Kv=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Lv=bless({$t,$Jv,$v,$q,$w,$Kv,$y,$z},$A);$Mv=q#throughput#;$Nv=[];$Ov=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$Pv=bless({$t,$Nv,$v,$q,$w,$Ov,$y,$z},$A);$Qv=q#write_limit_throughput#;$Rv=[];$Sv=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Tv=bless({$t,$Rv,$v,$q,$w,$Sv,$y,$z},$A);$Uv={$Ev,$Hv,$Iv,$Lv,$Mv,$Pv,$Qv,$Tv};$Vv=q#/io/transfer_io_measurement.b#;$Wv=bless({$T2,$Dv,$u4,$q,$v4,$q,$w4,$Uv,$Q,$Vv},$T3);$Xv=[$pv,$Cv,$Wv];$Yv=bless({$T2,$Pu,$Q,$Qu,$i3,$Xv},$w3);$Zv=[];$cw=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$dw=bless({$t,$Zv,$v,$q,$w,$cw,$y,$z},$A);$ew=q#ni:/io/transfer.c#;$fw={$w3,1,$y3,1,$A3,1};$gw=q#/io/transfer.c#;$hw={$w3,1,$y3,1,$A3,1,$q4,1};$iw=q#/semantic/task.c#;$jw=[$nk];$kw=bless({$T2,$hw,$Q,$iw,$i3,$jw},$h4);$lw={};$mw={};$nw=q#/io/transfer.c_into.b#;$ow=bless({$T2,$lw,$u4,$dw,$v4,$q,$w4,$mw,$Q,$nw},$T3);$pw=[$kw,$ow];$qw=bless({$T2,$fw,$Q,$gw,$i3,$pw},$h4);$rw=q#ni:/io/transfer.c_into.b#;$sw=q#ni:/io/transfer_async#;$tw={$x3,1};$uw=q#/io/transfer_async#;$vw={};$ww=q#dest_io#;$xw=[];$yw=q#shift->{'dest_io'}#;$zw=bless({$t,$xw,$v,$q,$w,$yw,$y,$z},$A);$Aw=q#id#;$Bw=[];$Cw=q#shift->{'id'}#;$Dw=bless({$t,$Bw,$v,$q,$w,$Cw,$y,$z},$A);$Ew=q#source_io#;$Fw=[];$Gw=q#shift->{'source_io'}#;$Hw=bless({$t,$Fw,$v,$q,$w,$Gw,$y,$z},$A);$Iw={$ww,$zw,$Aw,$Dw,$Ew,$Hw};$Jw=q#/io/transfer_async_ro.b#;$Kw=bless({$T2,$vw,$u4,$q,$v4,$q,$w4,$Iw,$Q,$Jw},$T3);$Lw={};$Mw=[];$Nw=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Ow=bless({$t,$Mw,$v,$q,$w,$Nw,$y,$z},$A);$Pw={$K6,$Ow};$Qw=q#/io/transfer_async_init.b#;$Rw=bless({$T2,$Lw,$u4,$q,$v4,$q,$w4,$Pw,$Q,$Qw},$T3);$Sw={};$Tw=[];$Uw=q#ni('ni:/io/transfer_async')->track(shift)#;$Vw=bless({$t,$Tw,$v,$q,$w,$Uw,$y,$z},$A);$Ww=[];$Xw=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Yw=bless({$t,$Ww,$v,$q,$w,$Xw,$y,$z},$A);$Zw={};$cx=q#/io/transfer_async_lifecycle.b#;$dx=bless({$T2,$Sw,$u4,$Vw,$v4,$Yw,$w4,$Zw,$Q,$cx},$T3);$ex={};$fx=q#run#;$gx=[];$hx=q#shift#;$ix=bless({$t,$gx,$v,$q,$w,$hx,$y,$z},$A);$jx=q#run_async#;$kx=[];$lx=q#my $self = shift;
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

$self;#;$mx=bless({$t,$kx,$v,$q,$w,$lx,$y,$z},$A);$nx={$fx,$ix,$jx,$mx};$ox=q#/io/transfer_async_run.b#;$px=bless({$T2,$ex,$u4,$q,$v4,$q,$w4,$nx,$Q,$ox},$T3);$qx=[$Yv,$Kw,$Rw,$dx,$px];$rx=q#tracked_transfers#;$sx={};$tx=q#transfer_id#;$ux=bless({$T2,$tw,$Q,$uw,$i3,$qx,$rx,$sx,$tx,0},$y3);$vx=[];$wx=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$xx=bless({$t,$vx,$v,$q,$w,$wx,$y,$z},$A);$yx=q#ni:/io/transfer_async.c#;$zx={$y3,1};$Ax=q#/io/transfer_async.c#;$Bx={};$Cx=q#new_id#;$Dx=[];$Ex=q#++shift->{transfer_id}#;$Fx=bless({$t,$Dx,$v,$q,$w,$Ex,$y,$z},$A);$Gx=q#track#;$Hx=[];$Ix=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Jx=bless({$t,$Hx,$v,$q,$w,$Ix,$y,$z},$A);$Kx=q#untrack#;$Lx=[];$Mx=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Nx=bless({$t,$Lx,$v,$q,$w,$Mx,$y,$z},$A);$Ox={$Cx,$Fx,$Gx,$Jx,$Kx,$Nx};$Px=q#/io/transfer_async.c_tracker.b#;$Qx=bless({$T2,$Bx,$u4,$xx,$v4,$q,$w4,$Ox,$Q,$Px},$T3);$Rx=[$qw,$Qx];$Sx=bless({$T2,$zx,$Q,$Ax,$i3,$Rx},$h4);$Tx=q#ni:/io/transfer_async.c_tracker.b#;$Ux=q#ni:/io/transfer_async_init.b#;$Vx=q#ni:/io/transfer_async_lifecycle.b#;$Wx=q#ni:/io/transfer_async_ro.b#;$Xx=q#ni:/io/transfer_async_run.b#;$Yx=q#ni:/io/transfer_io_interop.b#;$Zx=q#ni:/io/transfer_io_measurement.b#;$cy=q#ni:/io/transfer_sync#;$dy={$z3,1};$ey=q#/io/transfer_sync#;$fy={};$gy=[];$hy=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$iy=bless({$t,$gy,$v,$q,$w,$hy,$y,$z},$A);$jy={$K6,$iy};$ky=q#/io/transfer_sync_init.b#;$ly=bless({$T2,$fy,$u4,$q,$v4,$q,$w4,$jy,$Q,$ky},$T3);$my={};$ny=[];$oy=q#my $self = shift;
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
$self->success;#;$py=bless({$t,$ny,$v,$q,$w,$oy,$y,$z},$A);$qy={$fx,$py};$ry=q#/io/transfer_sync_run.b#;$sy=bless({$T2,$my,$u4,$q,$v4,$q,$w4,$qy,$Q,$ry},$T3);$ty=[$Yv,$ly,$sy];$uy=bless({$T2,$dy,$Q,$ey,$i3,$ty},$A3);$vy=q#ni:/io/transfer_sync.c#;$wy={$A3,1};$xy=q#/io/transfer_sync.c#;$yy=[$qw];$zy=bless({$T2,$wy,$Q,$xy,$i3,$yy},$h4);$Ay=q#ni:/io/transfer_sync_init.b#;$By=q#ni:/io/transfer_sync_run.b#;$Cy=q#ni:/lib#;$Dy=q#ni::/lib#;$Ey={$Dy,1};$Fy=[];$Gy=bless({$T2,$Ey,$Q,$n8,$i3,$Fy},$j4);$Hy=q#ni:/lib/accessor.b#;$Iy=q#ni:/lib/behavior#;$Jy=q#ni:/lib/behavior.c#;$Ky=q#ni:/lib/branch#;$Ly={$D3,1};$My=q#/lib/branch#;$Ny={};$Oy=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Py=bless({$w,$Oy,$y,$z},$A);$Qy={$K6,$Py};$Ry=q#/lib/branch_init.b#;$Sy=bless({$T2,$Ny,$u4,$q,$v4,$q,$w4,$Qy,$Q,$Ry},$T3);$Ty=[$B8,$H8,$Di,$Sy,$Kj];$Uy=bless({$T2,$Ly,$Q,$My,$i3,$Ty},$E3);$Vy=q#ni:/lib/branch.b#;$Wy=q#ni:/lib/branch.c#;$Xy={$E3,1};$Yy=q#/lib/branch.c#;$Zy=[$rk];$cz=bless({$T2,$Xy,$Q,$Yy,$i3,$Zy},$h4);$dz=q#ni:/lib/branch_init.b#;$ez=q#ni:/lib/class_init.b#;$fz=q#ni:/lib/dataslice#;$gz=q#ni:/lib/dataslice.b#;$hz=q#ni:/lib/dataslice.c#;$iz={$G3,1};$jz=q#/lib/dataslice.c#;$kz=[$rk];$lz=bless({$T2,$iz,$Q,$jz,$i3,$kz},$h4);$mz=q#ni:/lib/dataslice_init.b#;$nz=q#ni:/lib/definition.b#;$oz=q#ni:/lib/definition_def.b#;$pz=q#ni:/lib/definition_defdata.b#;$qz=q#ni:/lib/definition_init_with_defaults.b#;$rz=q#ni:/lib/doc#;$sz=q#ni:/lib/doc.c#;$tz={$H3,1};$uz=q#/lib/doc.c#;$vz={};$wz=q#defannotation#;$xz=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$yz=bless({$w,$xz,$y,$z},$A);$zz={$wz,$yz};$Az=q#/lib/doc.c_defannotation.b#;$Bz=bless({$T2,$vz,$u4,$q,$v4,$q,$w4,$zz,$Q,$Az},$T3);$Cz=[$nk,$Bz];$Dz=bless({$T2,$tz,$Q,$uz,$i3,$Cz},$h4);$Ez=q#ni:/lib/doc.c_defannotation.b#;$Fz=q#ni:/lib/doc_TODO.b#;$Gz=q#ni:/lib/doc_define.b#;$Hz=q#ni:/lib/doc_eg.b#;$Iz=q#ni:/lib/doc_end.b#;$Jz=q#ni:/lib/doc_init.b#;$Kz=q#ni:/lib/doc_namespace.b#;$Lz=q#ni:/lib/doc_process.b#;$Mz=q#ni:/lib/documentable.b#;$Nz=q#ni:/lib/fn#;$Oz={$A,1};$Pz=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$Qz=bless({$w,$Pz},$A);$Rz=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  annotations => [@_]};#;$Sz=bless({$w,$Rz},$A);$Tz=q#ni::/lib/fn::compile#;$Uz=q#ni::/lib/fn::instantiate#;$Vz={};$Wz=q#compile#;$Xz={$Wz,$Qz,$K6,$Sz};$Yz=q#/lib/fn_init.b#;$Zz=bless({$T2,$Vz,$u4,$q,$v4,$q,$w4,$Xz,$Q,$Yz},$T3);$cA={};$dA=[];$eA=q#shift->{'annotations'}#;$fA=bless({$t,$dA,$v,$q,$w,$eA,$y,$z},$A);$gA=[];$hA=q#shift->{'code'}#;$iA=bless({$t,$gA,$v,$q,$w,$hA,$y,$z},$A);$jA=q#fn#;$kA=[];$lA=q#shift->{'fn'}#;$mA=bless({$t,$kA,$v,$q,$w,$lA,$y,$z},$A);$nA={$t,$fA,$w,$iA,$jA,$mA};$oA=q#/lib/fn_ro.b#;$pA=bless({$T2,$cA,$u4,$q,$v4,$q,$w4,$nA,$Q,$oA},$T3);$qA={};$rA=[];$sA=q#if (@_ == 2) {
  $_[0]->{'closure'} = $_[1];
  return $_[0];
} else {
  return shift->{'closure'};
}#;$tA=bless({$t,$rA,$v,$q,$w,$sA,$y,$z},$A);$uA={$v,$tA};$vA=q#/lib/fn_rw.b#;$wA=bless({$T2,$qA,$u4,$q,$v4,$q,$w4,$uA,$Q,$vA},$T3);$xA={};$yA=[];$zA=q#my $self = shift; "fn {$$self{code}}"#;$AA=bless({$t,$yA,$v,$q,$w,$zA,$y,$z},$A);$BA=[];$CA=bless({$t,$BA,$v,$q,$w,$qj,$y,$z},$A);$DA={$ij,$AA,$pj,$CA};$EA=q#/lib/fn_ops.b#;$FA=bless({$T2,$xA,$u4,$q,$v4,$q,$w4,$DA,$Q,$EA},$T3);$GA={};$HA=[];$IA=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$JA=bless({$t,$HA,$v,$q,$w,$IA,$y,$z},$A);$KA={$w9,$JA};$LA=q#/lib/fn_serialize.b#;$MA=bless({$T2,$GA,$u4,$q,$v4,$q,$w4,$KA,$Q,$LA},$T3);$NA=[$H4,$Tj,$Zz,$pA,$wA,$FA,$MA];$OA=bless({$T2,$Oz,$Q,$xb,$i3,$NA},$I3);$PA=q#ni:/lib/fn.c#;$QA={$I3,1};$RA=q#/lib/fn.c#;$SA=[$nk];$TA=bless({$T2,$QA,$Q,$RA,$i3,$SA},$h4);$UA=q#ni:/lib/fn_init.b#;$VA=q#ni:/lib/fn_ops.b#;$WA=q#ni:/lib/fn_ro.b#;$XA=q#ni:/lib/fn_rw.b#;$YA=q#ni:/lib/fn_serialize.b#;$ZA=q#ni:/lib/future#;$cB={$J3,1};$dB={};$eB=[];$fB=bless({$t,$eB,$v,$q,$w,$Vu,$y,$z},$A);$gB=q#parents#;$hB=[];$iB=q#shift->{'parents'}#;$jB=bless({$t,$hB,$v,$q,$w,$iB,$y,$z},$A);$kB={$r,$fB,$gB,$jB};$lB=q#/lib/future_ro.b#;$mB=bless({$T2,$dB,$u4,$q,$v4,$q,$w4,$kB,$Q,$lB},$T3);$nB={};$oB=[];$pB=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$qB=bless({$t,$oB,$v,$q,$w,$pB,$y,$z},$A);$rB={$K6,$qB};$sB=q#/lib/future_init.b#;$tB=bless({$T2,$nB,$u4,$q,$v4,$q,$w4,$rB,$Q,$sB},$T3);$uB={};$vB=q#decide#;$wB=[];$xB=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$yB=bless({$t,$wB,$v,$q,$w,$xB,$y,$z},$A);$zB=q#decided#;$AB=[];$BB=q#shift->{outcome}#;$CB=bless({$t,$AB,$v,$q,$w,$BB,$y,$z},$A);$DB=q#listener#;$EB=[];$FB=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$GB=bless({$t,$EB,$v,$q,$w,$FB,$y,$z},$A);$HB=q#v#;$IB=[];$JB=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$KB=bless({$t,$IB,$v,$q,$w,$JB,$y,$z},$A);$LB={$vB,$yB,$zB,$CB,$DB,$GB,$HB,$KB};$MB=q#/lib/future_state.b#;$NB=bless({$T2,$uB,$u4,$q,$v4,$q,$w4,$LB,$Q,$MB},$T3);$OB={};$PB=q#and#;$QB=[];$RB=q#my $self   = $_[0];
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
$child;#;$SB=bless({$t,$QB,$v,$q,$w,$RB,$y,$z},$A);$TB=q#flatmap#;$UB=[];$VB=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$WB=bless({$t,$UB,$v,$q,$w,$VB,$y,$z},$A);$XB=q#map#;$YB=[];$ZB=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$cC=bless({$t,$YB,$v,$q,$w,$ZB,$y,$z},$A);$dC=q#or#;$eC=[];$fC=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$gC=bless({$t,$eC,$v,$q,$w,$fC,$y,$z},$A);$hC={$PB,$SB,$TB,$WB,$XB,$cC,$dC,$gC};$iC=q#/lib/future_algebra.b#;$jC=bless({$T2,$OB,$u4,$q,$v4,$q,$w4,$hC,$Q,$iC},$T3);$kC=[$H4,$mB,$tB,$NB,$jC];$lC=bless({$T2,$cB,$Q,$Ob,$i3,$kC},$K3);$mC=q#ni:/lib/future.c#;$nC={$K3,1};$oC=q#/lib/future.c#;$pC=[$nk];$qC=bless({$T2,$nC,$Q,$oC,$i3,$pC},$h4);$rC=q#ni:/lib/future_algebra.b#;$sC=q#ni:/lib/future_init.b#;$tC=q#ni:/lib/future_ro.b#;$uC=q#ni:/lib/future_state.b#;$vC=q#ni:/lib/gensym_generator_compact.b#;$wC=q#ni:/lib/global_static_test.b#;$xC={};$yC=[];$zC=q#ni('ni:/lib/test_case')->new(shift)#;$AC=bless({$t,$yC,$v,$q,$w,$zC,$y,$Dh},$A);$BC=q#now#;$CC=[];$DC=q#ni('ni:/lib/test_value')->new(shift)#;$EC=bless({$t,$CC,$v,$q,$w,$DC,$y,$Dh},$A);$FC={$q6,$AC,$BC,$EC};$GC=q#/lib/global_static_test.b#;$HC=bless({$T2,$xC,$u4,$q,$v4,$q,$w4,$FC,$Q,$GC},$T3);$IC=q#ni:/lib/image#;$JC=q#ni:/lib/image.c#;$KC={$M3,1};$LC=q#/lib/image.c#;$MC=[$nk];$NC=bless({$T2,$KC,$Q,$LC,$i3,$MC},$h4);$OC=q#ni:/lib/image_init.b#;$PC=q#ni:/lib/image_quoting.b#;$QC=q#ni:/lib/instance.b#;$RC=q#ni:/lib/instantiable.b#;$SC=q#ni:/lib/json.b#;$TC=q#ni:/lib/json_data.b#;$UC=q#ni:/lib/name_as_string.b#;$VC=q#ni:/lib/named.b#;$WC=q#ni:/lib/named_in_ni.b#;$XC=q#ni:/lib/namespaced.b#;$YC=q#ni:/lib/ni#;$ZC={$N3,1};$cD={};$dD=q#extend#;$eD=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$fD=bless({$w,$eD,$y,$z},$A);$gD=q#is_mutable#;$hD=q#$0 ne '-' && -w $0#;$iD=bless({$w,$hD,$y,$z},$A);$jD=q#modify#;$kD=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$lD=bless({$w,$kD,$y,$z},$A);$mD={$dD,$fD,$gD,$iD,$jD,$lD};$nD=q#/lib/ni_self.b#;$oD=bless({$T2,$cD,$u4,$q,$v4,$q,$w4,$mD,$Q,$nD},$T3);$pD={};$qD=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$rD=bless({$w,$qD,$y,$z},$A);$sD=q#docs#;$tD=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$uD=bless({$w,$tD,$y,$z},$A);$vD=q#metaclasses#;$wD=q#my $self = shift;
map $self->resolve($_),
    grep $self->exists($_),
    map "ni:" . substr($_, 4),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$xD=bless({$w,$wD,$y,$z},$A);$yD=q#undocumented#;$zD=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$AD=bless({$w,$zD,$y,$z},$A);$BD=q#untested#;$CD=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$DD=bless({$w,$CD,$y,$z},$A);$ED={$K,$rD,$sD,$uD,$vD,$xD,$yD,$AD,$BD,$DD};$FD=q#/lib/ni_dev_introspection.b#;$GD=bless({$T2,$pD,$u4,$q,$v4,$q,$w4,$ED,$Q,$FD},$T3);$HD={};$ID=q#--internal/+=#;$JD=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$KD=bless({$w,$JD,$y,$z},$A);$LD=q#--internal/dev-state#;$MD=q#my $self = shift;
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
0;#;$ND=bless({$w,$MD,$y,$z},$A);$OD=q#--internal/eval#;$PD=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$QD=bless({$w,$PD,$y,$z},$A);$RD=q#--internal/image#;$SD=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$TD=bless({$w,$SD,$y,$z},$A);$UD=q#--internal/test#;$VD=q#local $| = 1;
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
!!$failed;#;$WD=bless({$w,$VD,$y,$z},$A);$XD=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$YD=bless({$w,$XD,$y,$z},$A);$ZD={$ID,$KD,$LD,$ND,$OD,$QD,$RD,$TD,$UD,$WD,$fx,$YD};$cE=q#/lib/ni_main.b#;$dE=bless({$T2,$HD,$u4,$q,$v4,$q,$w4,$ZD,$Q,$cE},$T3);$eE={};$fE=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$gE=bless({$w,$fE,$y,$z},$A);$hE=q#resolver_for#;$iE=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$jE=bless({$w,$iE,$y,$z},$A);$kE={$p9,$gE,$hE,$jE};$lE=q#/lib/ni_resolver.b#;$mE=bless({$T2,$eE,$u4,$q,$v4,$q,$w4,$kE,$Q,$lE},$T3);$nE={};$oE=q#exists#;$pE=q#exists $_[0]->{named}{$_[1]}#;$qE=bless({$w,$pE,$y,$z},$A);$rE=q#quoted#;$sE=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$tE=bless({$w,$sE,$y,$z},$A);$uE={$oE,$qE,$rE,$tE};$vE=q#/lib/ni_image.b#;$wE=bless({$T2,$nE,$u4,$q,$v4,$q,$w4,$uE,$Q,$vE},$T3);$xE=[$H4,$oD,$GD,$dE,$mE,$wE];$yE=bless({$T2,$ZC,$Q,$Oe,$i3,$xE},$O3);$zE=q#ni:/lib/ni.c#;$AE={$O3,1};$BE=q#/lib/ni.c#;$CE=[$nk];$DE=bless({$T2,$AE,$Q,$BE,$i3,$CE},$h4);$EE=q#ni:/lib/ni_dev_introspection.b#;$FE=q#ni:/lib/ni_image.b#;$GE=q#ni:/lib/ni_main.b#;$HE=q#ni:/lib/ni_resolver.b#;$IE=q#ni:/lib/ni_self.b#;$JE=q#ni:/lib/ni_static_util.b#;$KE=q#ni:/lib/object_metadata#;$LE={$P3,1,$C,1,$H,1};$ME=q#/lib/object_metadata#;$NE={};$OE=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$PE=bless({$w,$OE,$y,$z},$A);$QE={$S2,$PE};$RE=q#/lib/object_metadata_rw.b#;$SE=bless({$T2,$NE,$u4,$q,$v4,$q,$w4,$QE,$Q,$RE},$T3);$TE=[$H4,$SE];$UE=bless({$T2,$LE,$Q,$ME,$i3,$TE},$Q3);$VE=q#ni:/lib/object_metadata.c#;$WE={$Q3,1,$d4,1,$g4,1};$XE=q#/lib/object_metadata.c#;$YE=[$nk];$ZE=bless({$T2,$WE,$Q,$XE,$i3,$YE},$h4);$cF=q#ni:/lib/object_metadata_rw.b#;$dF=q#ni:/lib/perlbranch.b#;$eF=q#ni:/lib/quote_circular_addressed.b#;$fF=q#ni:/lib/quote_code_fail.b#;$gF=q#ni:/lib/quote_gensym_identity.b#;$hF=q#ni:/lib/quote_objects.b#;$iF=q#ni:/lib/quote_simple#;$jF={$R3,1};$kF={};$lF=[];$mF=q#+{}#;$nF=bless({$t,$lF,$v,$q,$w,$mF,$y,$z},$A);$oF={$K6,$nF};$pF=q#/lib/quote_simple_init.b#;$qF=bless({$T2,$kF,$u4,$q,$v4,$q,$w4,$oF,$Q,$pF},$T3);$rF={};$sF=[];$tF=bless({$t,$sF,$v,$q,$w,0,$y,$z},$A);$uF=[];$vF=q#shift->quote_value(shift)#;$wF=bless({$t,$uF,$v,$q,$w,$vF,$y,$z},$A);$xF={$Ld,$tF,$he,$wF};$yF=q#/lib/quote_simple_quote.b#;$zF=bless({$T2,$rF,$u4,$q,$v4,$q,$w4,$xF,$Q,$yF},$T3);$AF=[$H4,$qF,$zF,$Lc,$ld,$Bd];$BF=bless({$T2,$jF,$Q,$Ze,$i3,$AF},$S3);$CF=q#ni:/lib/quote_simple.c#;$DF={$S3,1};$EF=q#/lib/quote_simple.c#;$FF=[$nk];$GF=bless({$T2,$DF,$Q,$EF,$i3,$FF},$h4);$HF=q#ni:/lib/quote_simple_init.b#;$IF=q#ni:/lib/quote_simple_quote.b#;$JF=q#ni:/lib/quote_values.b#;$KF=q#ni:/lib/ref_eq.b#;$LF=q#ni:/lib/resolver.b#;$MF=q#ni:/lib/slice#;$NF=q#ni:/lib/slice.b#;$OF=q#ni:/lib/slice.c#;$PF={$U3,1};$QF=q#/lib/slice.c#;$RF=[$rk];$SF=bless({$T2,$PF,$Q,$QF,$i3,$RF},$h4);$TF=q#ni:/lib/slice_init.b#;$UF=q#ni:/lib/slice_serialize.b#;$VF=q#ni:/lib/static_fn.b#;$WF={};$XF=q#fc#;$YF=[];$ZF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$cG=bless({$t,$YF,$v,$q,$w,$ZF,$y,$z},$A);$dG=q#fk#;$eG=[];$fG=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$gG=bless({$t,$eG,$v,$q,$w,$fG,$y,$Dh},$A);$hG=[];$iG=q#ni('ni:/lib/fn')->new(@_)#;$jG=bless({$t,$hG,$v,$q,$w,$iG,$y,$Dh},$A);$kG=q#fp#;$lG=[];$mG=q#($$)#;$nG=bless({$t,$lG,$v,$q,$w,$iG,$y,$mG},$A);$oG={$XF,$cG,$dG,$gG,$jA,$jG,$kG,$nG};$pG=q#/lib/static_fn.b#;$qG=bless({$T2,$WF,$u4,$q,$v4,$q,$w4,$oG,$Q,$pG},$T3);$rG=q#ni:/lib/subclass.b#;$sG=q#ni:/lib/tag#;$tG={$V3,1};$uG=q#/lib/tag#;$vG={};$wG=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$xG=bless({$w,$wG,$y,$z},$A);$yG={$J8,$xG};$zG=q#/lib/tag.b#;$AG=bless({$T2,$vG,$u4,$q,$v4,$q,$w4,$yG,$Q,$zG},$T3);$BG={};$CG=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$DG=bless({$w,$CG,$y,$z},$A);$EG={$K6,$DG};$FG=q#/lib/tag_init.b#;$GG=bless({$T2,$BG,$u4,$q,$v4,$q,$w4,$EG,$Q,$FG},$T3);$HG=[$B8,$H8,$AG,$GG];$IG=bless({$T2,$tG,$Q,$uG,$i3,$HG},$W3);$JG=q#ni:/lib/tag.b#;$KG=q#ni:/lib/tag.c#;$LG={$W3,1};$MG=q#/lib/tag.c#;$NG=[$rk];$OG=bless({$T2,$LG,$Q,$MG,$i3,$NG},$h4);$PG=q#ni:/lib/tag_init.b#;$QG=q#ni:/lib/test_assert_eq#;$RG={$X3,1};$SG=q#/lib/test_assert_eq#;$TG={$X3,1,$Z3,1};$UG=q#/lib/test_assertion#;$VG={};$WG=q#commit#;$XG=[];$YG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$ZG=bless({$t,$XG,$v,$q,$w,$YG,$y,$z},$A);$cH={$WG,$ZG};$dH=q#/lib/test_assertion_commit.b#;$eH=bless({$T2,$VG,$u4,$q,$v4,$q,$w4,$cH,$Q,$dH},$T3);$fH=[$H4,$eH];$gH=bless({$T2,$TG,$Q,$UG,$i3,$fH},$c4);$hH={};$iH=q#diff#;$jH=[];$kH=q#shift->{'diff'}#;$lH=bless({$t,$jH,$v,$q,$w,$kH,$y,$z},$A);$mH={$iH,$lH};$nH=q#/lib/test_assert_eq_ro.b#;$oH=bless({$T2,$hH,$u4,$q,$v4,$q,$w4,$mH,$Q,$nH},$T3);$pH={};$qH=[];$rH=q#my ($class, $diff) = @_;
+{diff => $diff};#;$sH=bless({$t,$qH,$v,$q,$w,$rH,$y,$z},$A);$tH={$K6,$sH};$uH=q#/lib/test_assert_eq_init.b#;$vH=bless({$T2,$pH,$u4,$q,$v4,$q,$w4,$tH,$Q,$uH},$T3);$wH={};$xH=[];$yH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$zH=bless({$t,$xH,$v,$q,$w,$yH,$y,$z},$A);$AH=q#failed#;$BH=[];$CH=q#defined shift->{diff}#;$DH=bless({$t,$BH,$v,$q,$w,$CH,$y,$z},$A);$EH=q#result#;$FH=[];$GH=bless({$t,$FH,$v,$q,$w,$hx,$y,$z},$A);$HH={$ij,$zH,$AH,$DH,$EH,$GH};$IH=q#/lib/test_assert_eq_result.b#;$JH=bless({$T2,$wH,$u4,$q,$v4,$q,$w4,$HH,$Q,$IH},$T3);$KH=[$gH,$oH,$vH,$JH];$LH=bless({$T2,$RG,$Q,$SG,$i3,$KH},$Y3);$MH=q#ni:/lib/test_assert_eq.c#;$NH={$Y3,1};$OH=q#/lib/test_assert_eq.c#;$PH={$Y3,1,$c4,1};$QH=q#/lib/test_assertion.c#;$RH=[$nk];$SH=bless({$T2,$PH,$Q,$QH,$i3,$RH},$h4);$TH=[$SH];$UH=bless({$T2,$NH,$Q,$OH,$i3,$TH},$h4);$VH=q#ni:/lib/test_assert_eq_init.b#;$WH=q#ni:/lib/test_assert_eq_result.b#;$XH=q#ni:/lib/test_assert_eq_ro.b#;$YH=q#ni:/lib/test_assertion#;$ZH=q#ni:/lib/test_assertion.c#;$cI=q#ni:/lib/test_assertion_commit.b#;$dI=q#ni:/lib/test_case#;$eI={$C,1};$fI=q#/lib/test_case#;$gI=q#running_test#;$hI={};$iI=[];$jI=q#shift->{'assertions'}#;$kI=bless({$t,$iI,$v,$q,$w,$jI,$y,$z},$A);$lI=[];$mI=q#shift->{'test'}#;$nI=bless({$t,$lI,$v,$q,$w,$mI,$y,$z},$A);$oI={$n,$kI,$s,$nI};$pI=q#/lib/test_case_ro.b#;$qI=bless({$T2,$hI,$u4,$q,$v4,$q,$w4,$oI,$Q,$pI},$T3);$rI={};$sI=[];$tI=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$uI=bless({$t,$sI,$v,$q,$w,$tI,$y,$z},$A);$vI={$p,$uI};$wI=q#/lib/test_case_rw.b#;$xI=bless({$T2,$rI,$u4,$q,$v4,$q,$w4,$vI,$Q,$wI},$T3);$yI={};$zI=[];$AI=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$BI=bless({$t,$zI,$v,$q,$w,$AI,$y,$z},$A);$CI={$K6,$BI};$DI=q#/lib/test_case_init.b#;$EI=bless({$T2,$yI,$u4,$q,$v4,$q,$w4,$CI,$Q,$DI},$T3);$FI={};$GI=[];$HI=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$II=bless({$t,$GI,$v,$q,$w,$HI,$y,$z},$A);$JI=[];$KI=q#!shift->{outcome}->[0]#;$LI=bless({$t,$JI,$v,$q,$w,$KI,$y,$z},$A);$MI={$ij,$II,$AH,$LI};$NI=q#/lib/test_case_metrics.b#;$OI=bless({$T2,$FI,$u4,$q,$v4,$q,$w4,$MI,$Q,$NI},$T3);$PI={};$QI=q#done#;$RI=[];$SI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$TI=bless({$t,$RI,$v,$q,$w,$SI,$y,$z},$A);$UI=[];$VI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$WI=bless({$t,$UI,$v,$q,$w,$VI,$y,$z},$A);$XI={$QI,$TI,$fx,$WI};$YI=q#/lib/test_case_run.b#;$ZI=bless({$T2,$PI,$u4,$q,$v4,$q,$w4,$XI,$Q,$YI},$T3);$cJ=[$UE,$qI,$xI,$EI,$OI,$ZI];$dJ=bless({$T2,$eI,$Q,$fI,$gI,$q,$i3,$cJ},$d4);$eJ=[];$fJ=q#shift->{running_test} = undef#;$gJ=bless({$t,$eJ,$v,$q,$w,$fJ,$y,$z},$A);$hJ=q#ni:/lib/test_case.c#;$iJ={$d4,1};$jJ=q#/lib/test_case.c#;$kJ={};$lJ=[];$mJ=q#shift->{'running_test'}#;$nJ=bless({$t,$lJ,$v,$q,$w,$mJ,$y,$z},$A);$oJ={$gI,$nJ};$pJ=q#/lib/test_case.c_test_ro.b#;$qJ=bless({$T2,$kJ,$u4,$q,$v4,$q,$w4,$oJ,$Q,$pJ},$T3);$rJ={};$sJ=q#with_test#;$tJ=[];$uJ=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$vJ=bless({$t,$tJ,$v,$q,$w,$uJ,$y,$z},$A);$wJ={$sJ,$vJ};$xJ=q#/lib/test_case.c_test.b#;$yJ=bless({$T2,$rJ,$u4,$gJ,$v4,$q,$w4,$wJ,$Q,$xJ},$T3);$zJ=[$ZE,$qJ,$yJ];$AJ=bless({$T2,$iJ,$Q,$jJ,$i3,$zJ},$h4);$BJ=q#ni:/lib/test_case.c_test.b#;$CJ=q#ni:/lib/test_case.c_test_ro.b#;$DJ=q#ni:/lib/test_case_init.b#;$EJ=q#ni:/lib/test_case_metrics.b#;$FJ=q#ni:/lib/test_case_ro.b#;$GJ=q#ni:/lib/test_case_run.b#;$HJ=q#ni:/lib/test_case_rw.b#;$IJ=q#ni:/lib/test_value#;$JJ={$e4,1};$KJ=q#/lib/test_value#;$LJ={};$MJ=[];$NJ=q#\\$_[1]#;$OJ=bless({$t,$MJ,$v,$q,$w,$NJ,$y,$z},$A);$PJ={$K6,$OJ};$QJ=q#/lib/test_value_init.b#;$RJ=bless({$T2,$LJ,$u4,$q,$v4,$q,$w4,$PJ,$Q,$QJ},$T3);$SJ={};$TJ=q#(==#;$UJ=[];$VJ=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$WJ=bless({$t,$UJ,$v,$q,$w,$VJ,$y,$z},$A);$XJ=q#detailed_scalar_diff#;$YJ=[];$ZJ=q#local $_;
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
[@diff];#;$cK=bless({$t,$YJ,$v,$q,$w,$ZJ,$y,$z},$A);$dK=[];$eK=q#my ($self, $rhs) = @_;
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
return undef;#;$fK=bless({$t,$dK,$v,$q,$w,$eK,$y,$z},$A);$gK={$TJ,$WJ,$XJ,$cK,$iH,$fK};$hK=q#/lib/test_value_eq.b#;$iK=bless({$T2,$SJ,$u4,$q,$v4,$q,$w4,$gK,$Q,$hK},$T3);$jK={};$kK=[];$lK=q#ni::json_encode ${$_[0]}#;$mK=bless({$t,$kK,$v,$q,$w,$lK,$y,$z},$A);$nK={$ij,$mK};$oK=q#/lib/test_value_str.b#;$pK=bless({$T2,$jK,$u4,$q,$v4,$q,$w4,$nK,$Q,$oK},$T3);$qK=[$H4,$RJ,$iK,$pK];$rK=bless({$T2,$JJ,$Q,$KJ,$i3,$qK},$f4);$sK=q#ni:/lib/test_value.c#;$tK={$f4,1};$uK=q#/lib/test_value.c#;$vK=[$nk];$wK=bless({$T2,$tK,$Q,$uK,$i3,$vK},$h4);$xK=q#ni:/lib/test_value_eq.b#;$yK=q#ni:/lib/test_value_init.b#;$zK=q#ni:/lib/test_value_str.b#;$AK=q#ni:/lib/todo#;$BK={$H,1};$CK=q#/lib/todo#;$DK={};$EK=q#shift->{'todo'}#;$FK=bless({$w,$EK,$y,$z},$A);$GK={$E,$FK};$HK=q#/lib/todo_ro.b#;$IK=bless({$T2,$DK,$u4,$q,$v4,$q,$w4,$GK,$Q,$HK},$T3);$JK={};$KK=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$LK=bless({$w,$KK,$y,$z},$A);$MK={$K6,$LK};$NK=q#/lib/todo_init.b#;$OK=bless({$T2,$JK,$u4,$q,$v4,$q,$w4,$MK,$Q,$NK},$T3);$PK={};$QK=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$RK=bless({$w,$QK,$y,$z},$A);$SK={$ij,$RK};$TK=q#/lib/todo_str.b#;$UK=bless({$T2,$PK,$u4,$q,$v4,$q,$w4,$SK,$Q,$TK},$T3);$VK=[$UE,$IK,$OK,$UK];$WK=bless({$T2,$BK,$Q,$CK,$i3,$VK},$g4);$XK=q#ni:/lib/todo.c#;$YK={$g4,1};$ZK=q#/lib/todo.c#;$cL=[$ZE];$dL=bless({$T2,$YK,$Q,$ZK,$i3,$cL},$h4);$eL=q#ni:/lib/todo_ctor.b#;$fL={};$gL=q#ni('ni:/lib/todo')->new(@_)#;$hL=bless({$w,$gL,$y,$z},$A);$iL={$R2,$hL};$jL=q#/lib/todo_ctor.b#;$kL=bless({$T2,$fL,$u4,$q,$v4,$q,$w4,$iL,$Q,$jL},$T3);$lL=q#ni:/lib/todo_init.b#;$mL=q#ni:/lib/todo_ro.b#;$nL=q#ni:/lib/todo_str.b#;$oL=q#ni:/metaclass#;$pL={$h4,1};$qL=q#/metaclass#;$rL=[$Fi,$Tj,$Li,$Mj];$sL=bless({$T2,$pL,$Q,$qL,$i3,$rL},$i4);$tL=q#ni:/metaclass.c#;$uL={$i4,1};$vL=q#/metaclass.c#;$wL=[$ek];$xL=bless({$T2,$uL,$Q,$vL,$i3,$wL},$h4);$yL=q#ni:/module#;$zL=q#ni:/module.c#;$AL=q#ni:/object#;$BL=q#ni:/object.c#;$CL=q#ni:/semantic#;$DL=q#ni::/semantic#;$EL={$DL,1};$FL=[];$GL=bless({$T2,$EL,$Q,$fg,$i3,$FL},$j4);$HL=q#ni:/semantic/dimension#;$IL={$n4,1};$JL=q#/semantic/dimension#;$KL=[$ek];$LL=bless({$T2,$IL,$Q,$JL,$i3,$KL},$o4);$ML=q#ni:/semantic/dimension.c#;$NL={$o4,1};$OL=q#/semantic/dimension.c#;$PL=[$vk];$QL=bless({$T2,$NL,$Q,$OL,$i3,$PL},$h4);$RL=q#ni:/semantic/task#;$SL=q#ni:/semantic/task.c#;$TL=q#ni:/semantic/task_outcome.b#;$UL=q#ni:/semantic/task_ro.b#;$VL=q#ni:main#;$WL={$Ns,1};$XL=[$kL,$qG,$HC,$Ms];$YL=bless({$T2,$WL,$Q,$Ns,$i3,$XL},$j4);$ZL=q#ni:ni#;$cM={$d,$T,$W,$f1,$g1,$l1,$m1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$J2,$K2,$P2,$Q2,$o6,$p6,$h8,$i8,$o8,$p8,$K9,$L9,$qb,$rb,$yb,$zb,$Pb,$Qb,$He,$Ie,$Pe,$Qe,$cf,$df,$Yf,$Zf,$gg,$hg,$pi,$qi,$ek,$fk,$vk,$wk,$Ak,$Bk,$Fk,$Gk,$kl,$ll,$Bl,$Cl,$Ok,$Dl,$il,$El,$Vl,$Wl,$cm,$dm,$Ml,$em,$Tl,$fm,$Ln,$Mn,$Qn,$Rn,$tn,$Sn,$Jn,$Tn,$vm,$Un,$ln,$Vn,$Pm,$Wn,$om,$Xn,$rp,$vp,$Tp,$Up,$Rp,$Vp,$No,$Wp,$Yo,$Xp,$po,$Yp,$op,$Zp,$io,$cq,$xo,$dq,$wr,$xr,$Br,$Cr,$tq,$Dr,$Qq,$Er,$Aq,$Fr,$ur,$Gr,$lq,$Hr,$Yq,$Ir,$ds,$es,$is,$js,$Zr,$ks,$Qr,$ls,$Ms,$Os,$ot,$pt,$tt,$ut,$Xs,$vt,$mt,$wt,$i6,$xt,$zl,$yt,$xl,$zt,$m5,$At,$u5,$Bt,$G5,$Ct,$Q4,$Dt,$g6,$Et,$S5,$Ft,$M7,$Gt,$Kt,$Lt,$K7,$Mt,$Q6,$Nt,$q7,$Ot,$F6,$Pt,$e7,$Qt,$Fu,$Gu,$Ku,$Lu,$pu,$Mu,$Du,$Nu,$iu,$Ou,$Yv,$ew,$qw,$rw,$ow,$sw,$ux,$yx,$Sx,$Tx,$Qx,$Ux,$Rw,$Vx,$dx,$Wx,$Kw,$Xx,$px,$Yx,$Cv,$Zx,$Wv,$cy,$uy,$vy,$zy,$Ay,$ly,$By,$sy,$Cy,$Gy,$Hy,$gj,$Iy,$B8,$Jy,$rk,$Ky,$Uy,$Vy,$Di,$Wy,$cz,$dz,$Sy,$ez,$Li,$fz,$E9,$gz,$R8,$hz,$lz,$mz,$X8,$nz,$Kj,$oz,$Ui,$pz,$Bj,$qz,$Ij,$rz,$Ua,$sz,$Dz,$Ez,$Bz,$Fz,$Ca,$Gz,$na,$Hz,$Ia,$Iz,$wa,$Jz,$V9,$Kz,$da,$Lz,$Sa,$Mz,$z8,$Nz,$OA,$PA,$TA,$UA,$Zz,$VA,$FA,$WA,$pA,$XA,$wA,$YA,$MA,$ZA,$lC,$mC,$qC,$rC,$jC,$sC,$tB,$tC,$mB,$uC,$NB,$vC,$ve,$wC,$HC,$IC,$xe,$JC,$NC,$OC,$fc,$PC,$Dc,$QC,$F4,$RC,$Tj,$SC,$di,$TC,$yh,$UC,$nj,$VC,$H8,$WC,$g9,$XC,$n9,$YC,$yE,$zE,$DE,$EE,$GD,$FE,$wE,$GE,$dE,$HE,$mE,$IE,$oD,$JE,$ch,$KE,$UE,$VE,$ZE,$cF,$SE,$dF,$Fi,$eF,$Rd,$fF,$Lc,$gF,$ne,$hF,$Bd,$iF,$BF,$CF,$GF,$HF,$qF,$IF,$zF,$JF,$ld,$KF,$uj,$LF,$u9,$MF,$zf,$NF,$rf,$OF,$SF,$TF,$xf,$UF,$C9,$VF,$qG,$rG,$ck,$sG,$IG,$JG,$AG,$KG,$OG,$PG,$GG,$QG,$LH,$MH,$UH,$VH,$vH,$WH,$JH,$XH,$oH,$YH,$gH,$ZH,$SH,$cI,$eH,$dI,$dJ,$hJ,$AJ,$BJ,$yJ,$CJ,$qJ,$DJ,$EI,$EJ,$OI,$FJ,$qI,$GJ,$ZI,$HJ,$xI,$IJ,$rK,$sK,$wK,$xK,$iK,$yK,$RJ,$zK,$pK,$AK,$WK,$XK,$dL,$eL,$kL,$lL,$OK,$mL,$IK,$nL,$UK,$oL,$sL,$tL,$xL,$yL,$Mj,$zL,$tk,$AL,$H4,$BL,$nk,$CL,$GL,$HL,$LL,$ML,$QL,$RL,$pv,$SL,$kw,$TL,$nv,$UL,$Zu,$VL,$YL,$ZL,$fi};$dM=q#resolvers#;$eM=[];$fM=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$gM=bless({$t,$eM,$v,$q,$w,$fM,$y,$z},$A);$hM=q#file#;$iM=[];$jM=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$kM=bless({$t,$iM,$v,$q,$w,$jM,$y,$z},$A);$lM=q#null#;$mM=[];$nM=q#ni('ni:/io/null')->new#;$oM=bless({$t,$mM,$v,$q,$w,$nM,$y,$z},$A);$pM=q#sh#;$qM=[];$rM=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$sM=bless({$t,$qM,$v,$q,$w,$rM,$y,$z},$A);$tM=q#str#;$uM=[];$vM=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$wM=bless({$t,$uM,$v,$q,$w,$vM,$y,$z},$A);$xM={$s7,$gM,$hM,$kM,$lM,$oM,$pM,$sM,$tM,$wM};$yM=bless({$c,$cM,$dM,$xM},$N3);*$Uz=\&$Sz;*$Tz=\&$Qz;*$nf=\&$lf;*$mf=\&$jf;$F4->apply_($j3);$F4->apply_($k3);$F4->apply_($U2);$F4->apply_($l3);$F4->apply_($V2);$F4->apply_($m3);$F4->apply_($W2);$F4->apply_($n3);$F4->apply_($X2);$F4->apply_($o3);$F4->apply_($Y2);$F4->apply_($p3);$F4->apply_($Z2);$F4->apply_($q3);$F4->apply_($c3);$F4->apply_($r3);$F4->apply_($d3);$F4->apply_($s3);$F4->apply_($e3);$F4->apply_($t3);$F4->apply_($f3);$F4->apply_($u3);$F4->apply_($v3);$F4->apply_($w3);$F4->apply_($x3);$F4->apply_($y3);$F4->apply_($z3);$F4->apply_($A3);$F4->apply_($B3);$F4->apply_($C3);$F4->apply_($D3);$F4->apply_($E3);$F4->apply_($F3);$F4->apply_($G3);$F4->apply_($S);$F4->apply_($H3);$F4->apply_($A);$F4->apply_($I3);$F4->apply_($J3);$F4->apply_($K3);$F4->apply_($L3);$F4->apply_($M3);$F4->apply_($N3);$F4->apply_($O3);$F4->apply_($P3);$F4->apply_($Q3);$F4->apply_($R3);$F4->apply_($S3);$F4->apply_($T3);$F4->apply_($U3);$F4->apply_($V3);$F4->apply_($W3);$F4->apply_($X3);$F4->apply_($Y3);$F4->apply_($Z3);$F4->apply_($c4);$F4->apply_($C);$F4->apply_($d4);$F4->apply_($e4);$F4->apply_($f4);$F4->apply_($H);$F4->apply_($g4);$F4->apply_($h4);$F4->apply_($i4);$F4->apply_($j4);$F4->apply_($k4);$F4->apply_($l4);$F4->apply_($m4);$F4->apply_($n4);$F4->apply_($o4);$F4->apply_($p4);$F4->apply_($q4);$Q4->apply_($U2);$Q4->apply_($V2);$Q4->apply_($W2);$Q4->apply_($X2);$Q4->apply_($Y2);$Q4->apply_($Z2);$Q4->apply_($c3);$Q4->apply_($d3);$Q4->apply_($e3);$Q4->apply_($f3);$m5->apply_($U2);$m5->apply_($V2);$m5->apply_($W2);$m5->apply_($X2);$m5->apply_($Y2);$m5->apply_($Z2);$m5->apply_($c3);$m5->apply_($d3);$m5->apply_($e3);$m5->apply_($f3);$u5->apply_($U2);$u5->apply_($V2);$u5->apply_($W2);$u5->apply_($X2);$u5->apply_($Y2);$u5->apply_($Z2);$u5->apply_($c3);$u5->apply_($d3);$u5->apply_($e3);$u5->apply_($f3);$G5->apply_($U2);$G5->apply_($V2);$G5->apply_($W2);$G5->apply_($X2);$G5->apply_($Y2);$G5->apply_($Z2);$G5->apply_($c3);$G5->apply_($d3);$G5->apply_($e3);$G5->apply_($f3);$S5->apply_($U2);$S5->apply_($V2);$S5->apply_($W2);$S5->apply_($X2);$S5->apply_($Y2);$S5->apply_($Z2);$S5->apply_($c3);$S5->apply_($d3);$S5->apply_($e3);$S5->apply_($f3);$g6->apply_($U2);$g6->apply_($V2);$g6->apply_($W2);$g6->apply_($X2);$g6->apply_($Y2);$g6->apply_($Z2);$g6->apply_($c3);$g6->apply_($d3);$g6->apply_($e3);$g6->apply_($f3);$F6->apply_($e3);$Q6->apply_($e3);$e7->apply_($e3);$q7->apply_($e3);$K7->apply_($e3);$z8->apply_($j3);$z8->apply_($k3);$z8->apply_($l3);$z8->apply_($m3);$z8->apply_($n3);$z8->apply_($o3);$z8->apply_($p3);$z8->apply_($q3);$z8->apply_($r3);$z8->apply_($s3);$z8->apply_($t3);$z8->apply_($u3);$z8->apply_($w3);$z8->apply_($y3);$z8->apply_($A3);$z8->apply_($B3);$z8->apply_($C3);$z8->apply_($D3);$z8->apply_($E3);$z8->apply_($F3);$z8->apply_($G3);$z8->apply_($H3);$z8->apply_($I3);$z8->apply_($K3);$z8->apply_($M3);$z8->apply_($O3);$z8->apply_($Q3);$z8->apply_($S3);$z8->apply_($T3);$z8->apply_($U3);$z8->apply_($V3);$z8->apply_($W3);$z8->apply_($Y3);$z8->apply_($c4);$z8->apply_($d4);$z8->apply_($f4);$z8->apply_($g4);$z8->apply_($h4);$z8->apply_($i4);$z8->apply_($j4);$z8->apply_($k4);$z8->apply_($m4);$z8->apply_($n4);$z8->apply_($o4);$z8->apply_($q4);$H8->apply_($j3);$H8->apply_($k3);$H8->apply_($l3);$H8->apply_($m3);$H8->apply_($n3);$H8->apply_($o3);$H8->apply_($p3);$H8->apply_($q3);$H8->apply_($r3);$H8->apply_($s3);$H8->apply_($t3);$H8->apply_($u3);$H8->apply_($w3);$H8->apply_($y3);$H8->apply_($A3);$H8->apply_($C3);$H8->apply_($D3);$H8->apply_($E3);$H8->apply_($F3);$H8->apply_($G3);$H8->apply_($S);$H8->apply_($H3);$H8->apply_($I3);$H8->apply_($K3);$H8->apply_($M3);$H8->apply_($O3);$H8->apply_($Q3);$H8->apply_($S3);$H8->apply_($T3);$H8->apply_($U3);$H8->apply_($V3);$H8->apply_($W3);$H8->apply_($Y3);$H8->apply_($c4);$H8->apply_($d4);$H8->apply_($f4);$H8->apply_($g4);$H8->apply_($h4);$H8->apply_($i4);$H8->apply_($j4);$H8->apply_($k4);$H8->apply_($m4);$H8->apply_($n4);$H8->apply_($o4);$H8->apply_($q4);$R8->apply_($F3);$X8->apply_($F3);$g9->apply_($j3);$g9->apply_($k3);$g9->apply_($l3);$g9->apply_($m3);$g9->apply_($n3);$g9->apply_($o3);$g9->apply_($p3);$g9->apply_($q3);$g9->apply_($r3);$g9->apply_($s3);$g9->apply_($t3);$g9->apply_($u3);$g9->apply_($w3);$g9->apply_($y3);$g9->apply_($A3);$g9->apply_($C3);$g9->apply_($D3);$g9->apply_($E3);$g9->apply_($F3);$g9->apply_($G3);$g9->apply_($H3);$g9->apply_($I3);$g9->apply_($K3);$g9->apply_($M3);$g9->apply_($O3);$g9->apply_($Q3);$g9->apply_($S3);$g9->apply_($T3);$g9->apply_($U3);$g9->apply_($V3);$g9->apply_($W3);$g9->apply_($Y3);$g9->apply_($c4);$g9->apply_($d4);$g9->apply_($f4);$g9->apply_($g4);$g9->apply_($h4);$g9->apply_($i4);$g9->apply_($j4);$g9->apply_($k4);$g9->apply_($m4);$g9->apply_($n4);$g9->apply_($o4);$g9->apply_($q4);$n9->apply_($j3);$n9->apply_($k3);$n9->apply_($l3);$n9->apply_($m3);$n9->apply_($n3);$n9->apply_($o3);$n9->apply_($p3);$n9->apply_($q3);$n9->apply_($r3);$n9->apply_($s3);$n9->apply_($t3);$n9->apply_($u3);$n9->apply_($w3);$n9->apply_($y3);$n9->apply_($A3);$n9->apply_($C3);$n9->apply_($D3);$n9->apply_($E3);$n9->apply_($F3);$n9->apply_($G3);$n9->apply_($H3);$n9->apply_($I3);$n9->apply_($K3);$n9->apply_($M3);$n9->apply_($O3);$n9->apply_($Q3);$n9->apply_($S3);$n9->apply_($T3);$n9->apply_($U3);$n9->apply_($V3);$n9->apply_($W3);$n9->apply_($Y3);$n9->apply_($c4);$n9->apply_($d4);$n9->apply_($f4);$n9->apply_($g4);$n9->apply_($h4);$n9->apply_($i4);$n9->apply_($j4);$n9->apply_($k4);$n9->apply_($m4);$n9->apply_($n4);$n9->apply_($o4);$n9->apply_($q4);$u9->apply_($j3);$u9->apply_($k3);$u9->apply_($l3);$u9->apply_($m3);$u9->apply_($n3);$u9->apply_($o3);$u9->apply_($p3);$u9->apply_($q3);$u9->apply_($r3);$u9->apply_($s3);$u9->apply_($t3);$u9->apply_($u3);$u9->apply_($w3);$u9->apply_($y3);$u9->apply_($A3);$u9->apply_($C3);$u9->apply_($D3);$u9->apply_($E3);$u9->apply_($F3);$u9->apply_($G3);$u9->apply_($H3);$u9->apply_($I3);$u9->apply_($K3);$u9->apply_($M3);$u9->apply_($O3);$u9->apply_($Q3);$u9->apply_($S3);$u9->apply_($U3);$u9->apply_($V3);$u9->apply_($W3);$u9->apply_($Y3);$u9->apply_($c4);$u9->apply_($d4);$u9->apply_($f4);$u9->apply_($g4);$u9->apply_($h4);$u9->apply_($i4);$u9->apply_($j4);$u9->apply_($k4);$u9->apply_($m4);$u9->apply_($n4);$u9->apply_($o4);$u9->apply_($q4);$C9->apply_($F3);$C9->apply_($T3);$V9->apply_($S);$da->apply_($S);$na->apply_($S);$wa->apply_($S);$Ca->apply_($S);$Ia->apply_($S);$Sa->apply_($S);$fc->apply_($L3);$Dc->apply_($L3);$Lc->apply_($L3);$Lc->apply_($R3);$ld->apply_($L3);$ld->apply_($R3);$Bd->apply_($L3);$Bd->apply_($R3);$Rd->apply_($L3);$ne->apply_($L3);$ve->apply_($L3);$rf->apply_($T3);$xf->apply_($T3);$ch->apply_($ig);$yh->apply_($ig);$di->apply_($ig);$Di->apply_($j3);$Di->apply_($k3);$Di->apply_($l3);$Di->apply_($m3);$Di->apply_($n3);$Di->apply_($o3);$Di->apply_($p3);$Di->apply_($q3);$Di->apply_($r3);$Di->apply_($s3);$Di->apply_($t3);$Di->apply_($u3);$Di->apply_($w3);$Di->apply_($y3);$Di->apply_($A3);$Di->apply_($C3);$Di->apply_($D3);$Di->apply_($E3);$Di->apply_($G3);$Di->apply_($H3);$Di->apply_($I3);$Di->apply_($K3);$Di->apply_($M3);$Di->apply_($O3);$Di->apply_($Q3);$Di->apply_($S3);$Di->apply_($U3);$Di->apply_($W3);$Di->apply_($Y3);$Di->apply_($c4);$Di->apply_($d4);$Di->apply_($f4);$Di->apply_($g4);$Di->apply_($h4);$Di->apply_($i4);$Di->apply_($j4);$Di->apply_($k4);$Di->apply_($m4);$Di->apply_($n4);$Di->apply_($o4);$Di->apply_($q4);$Li->apply_($j3);$Li->apply_($k3);$Li->apply_($l3);$Li->apply_($m3);$Li->apply_($n3);$Li->apply_($o3);$Li->apply_($p3);$Li->apply_($q3);$Li->apply_($r3);$Li->apply_($s3);$Li->apply_($t3);$Li->apply_($u3);$Li->apply_($w3);$Li->apply_($y3);$Li->apply_($A3);$Li->apply_($C3);$Li->apply_($E3);$Li->apply_($G3);$Li->apply_($H3);$Li->apply_($I3);$Li->apply_($K3);$Li->apply_($M3);$Li->apply_($O3);$Li->apply_($Q3);$Li->apply_($S3);$Li->apply_($U3);$Li->apply_($W3);$Li->apply_($Y3);$Li->apply_($c4);$Li->apply_($d4);$Li->apply_($f4);$Li->apply_($g4);$Li->apply_($h4);$Li->apply_($i4);$Li->apply_($j4);$Li->apply_($k4);$Li->apply_($m4);$Li->apply_($n4);$Li->apply_($o4);$Li->apply_($q4);$Ui->apply_($j3);$Ui->apply_($k3);$Ui->apply_($l3);$Ui->apply_($m3);$Ui->apply_($n3);$Ui->apply_($o3);$Ui->apply_($p3);$Ui->apply_($q3);$Ui->apply_($r3);$Ui->apply_($s3);$Ui->apply_($t3);$Ui->apply_($u3);$Ui->apply_($w3);$Ui->apply_($y3);$Ui->apply_($A3);$Ui->apply_($C3);$Ui->apply_($D3);$Ui->apply_($E3);$Ui->apply_($G3);$Ui->apply_($H3);$Ui->apply_($I3);$Ui->apply_($K3);$Ui->apply_($M3);$Ui->apply_($O3);$Ui->apply_($Q3);$Ui->apply_($S3);$Ui->apply_($U3);$Ui->apply_($W3);$Ui->apply_($Y3);$Ui->apply_($c4);$Ui->apply_($d4);$Ui->apply_($f4);$Ui->apply_($g4);$Ui->apply_($h4);$Ui->apply_($i4);$Ui->apply_($j4);$Ui->apply_($k4);$Ui->apply_($m4);$Ui->apply_($n4);$Ui->apply_($o4);$Ui->apply_($q4);$gj->apply_($j3);$gj->apply_($k3);$gj->apply_($l3);$gj->apply_($m3);$gj->apply_($n3);$gj->apply_($o3);$gj->apply_($p3);$gj->apply_($q3);$gj->apply_($r3);$gj->apply_($s3);$gj->apply_($t3);$gj->apply_($u3);$gj->apply_($w3);$gj->apply_($y3);$gj->apply_($A3);$gj->apply_($C3);$gj->apply_($D3);$gj->apply_($E3);$gj->apply_($G3);$gj->apply_($H3);$gj->apply_($I3);$gj->apply_($K3);$gj->apply_($M3);$gj->apply_($O3);$gj->apply_($Q3);$gj->apply_($S3);$gj->apply_($U3);$gj->apply_($W3);$gj->apply_($Y3);$gj->apply_($c4);$gj->apply_($d4);$gj->apply_($f4);$gj->apply_($g4);$gj->apply_($h4);$gj->apply_($i4);$gj->apply_($j4);$gj->apply_($k4);$gj->apply_($m4);$gj->apply_($n4);$gj->apply_($o4);$gj->apply_($q4);$nj->apply_($j3);$nj->apply_($k3);$nj->apply_($l3);$nj->apply_($m3);$nj->apply_($n3);$nj->apply_($o3);$nj->apply_($p3);$nj->apply_($q3);$nj->apply_($r3);$nj->apply_($s3);$nj->apply_($t3);$nj->apply_($u3);$nj->apply_($w3);$nj->apply_($y3);$nj->apply_($A3);$nj->apply_($C3);$nj->apply_($D3);$nj->apply_($E3);$nj->apply_($G3);$nj->apply_($H3);$nj->apply_($I3);$nj->apply_($K3);$nj->apply_($M3);$nj->apply_($O3);$nj->apply_($Q3);$nj->apply_($S3);$nj->apply_($U3);$nj->apply_($W3);$nj->apply_($Y3);$nj->apply_($c4);$nj->apply_($d4);$nj->apply_($f4);$nj->apply_($g4);$nj->apply_($h4);$nj->apply_($i4);$nj->apply_($j4);$nj->apply_($k4);$nj->apply_($m4);$nj->apply_($n4);$nj->apply_($o4);$nj->apply_($q4);$uj->apply_($j3);$uj->apply_($k3);$uj->apply_($l3);$uj->apply_($m3);$uj->apply_($n3);$uj->apply_($o3);$uj->apply_($p3);$uj->apply_($q3);$uj->apply_($r3);$uj->apply_($s3);$uj->apply_($t3);$uj->apply_($u3);$uj->apply_($w3);$uj->apply_($y3);$uj->apply_($A3);$uj->apply_($C3);$uj->apply_($D3);$uj->apply_($E3);$uj->apply_($G3);$uj->apply_($H3);$uj->apply_($I3);$uj->apply_($K3);$uj->apply_($M3);$uj->apply_($O3);$uj->apply_($Q3);$uj->apply_($S3);$uj->apply_($U3);$uj->apply_($W3);$uj->apply_($Y3);$uj->apply_($c4);$uj->apply_($d4);$uj->apply_($f4);$uj->apply_($g4);$uj->apply_($h4);$uj->apply_($i4);$uj->apply_($j4);$uj->apply_($k4);$uj->apply_($m4);$uj->apply_($n4);$uj->apply_($o4);$uj->apply_($q4);$Bj->apply_($j3);$Bj->apply_($k3);$Bj->apply_($l3);$Bj->apply_($m3);$Bj->apply_($n3);$Bj->apply_($o3);$Bj->apply_($p3);$Bj->apply_($q3);$Bj->apply_($r3);$Bj->apply_($s3);$Bj->apply_($t3);$Bj->apply_($u3);$Bj->apply_($w3);$Bj->apply_($y3);$Bj->apply_($A3);$Bj->apply_($C3);$Bj->apply_($D3);$Bj->apply_($E3);$Bj->apply_($G3);$Bj->apply_($H3);$Bj->apply_($I3);$Bj->apply_($K3);$Bj->apply_($M3);$Bj->apply_($O3);$Bj->apply_($Q3);$Bj->apply_($S3);$Bj->apply_($U3);$Bj->apply_($W3);$Bj->apply_($Y3);$Bj->apply_($c4);$Bj->apply_($d4);$Bj->apply_($f4);$Bj->apply_($g4);$Bj->apply_($h4);$Bj->apply_($i4);$Bj->apply_($j4);$Bj->apply_($k4);$Bj->apply_($m4);$Bj->apply_($n4);$Bj->apply_($o4);$Bj->apply_($q4);$Ij->apply_($j3);$Ij->apply_($k3);$Ij->apply_($l3);$Ij->apply_($m3);$Ij->apply_($n3);$Ij->apply_($o3);$Ij->apply_($p3);$Ij->apply_($q3);$Ij->apply_($r3);$Ij->apply_($s3);$Ij->apply_($t3);$Ij->apply_($u3);$Ij->apply_($w3);$Ij->apply_($y3);$Ij->apply_($A3);$Ij->apply_($C3);$Ij->apply_($D3);$Ij->apply_($E3);$Ij->apply_($G3);$Ij->apply_($H3);$Ij->apply_($I3);$Ij->apply_($K3);$Ij->apply_($M3);$Ij->apply_($O3);$Ij->apply_($Q3);$Ij->apply_($S3);$Ij->apply_($U3);$Ij->apply_($W3);$Ij->apply_($Y3);$Ij->apply_($c4);$Ij->apply_($d4);$Ij->apply_($f4);$Ij->apply_($g4);$Ij->apply_($h4);$Ij->apply_($i4);$Ij->apply_($j4);$Ij->apply_($k4);$Ij->apply_($m4);$Ij->apply_($n4);$Ij->apply_($o4);$Ij->apply_($q4);$Tj->apply_($j3);$Tj->apply_($k3);$Tj->apply_($l3);$Tj->apply_($m3);$Tj->apply_($n3);$Tj->apply_($o3);$Tj->apply_($p3);$Tj->apply_($q3);$Tj->apply_($r3);$Tj->apply_($s3);$Tj->apply_($t3);$Tj->apply_($u3);$Tj->apply_($w3);$Tj->apply_($y3);$Tj->apply_($A3);$Tj->apply_($C3);$Tj->apply_($E3);$Tj->apply_($G3);$Tj->apply_($H3);$Tj->apply_($A);$Tj->apply_($I3);$Tj->apply_($K3);$Tj->apply_($M3);$Tj->apply_($O3);$Tj->apply_($Q3);$Tj->apply_($S3);$Tj->apply_($T3);$Tj->apply_($U3);$Tj->apply_($V3);$Tj->apply_($W3);$Tj->apply_($Y3);$Tj->apply_($c4);$Tj->apply_($d4);$Tj->apply_($f4);$Tj->apply_($g4);$Tj->apply_($h4);$Tj->apply_($i4);$Tj->apply_($k4);$Tj->apply_($m4);$Tj->apply_($n4);$Tj->apply_($o4);$Tj->apply_($q4);$ck->apply_($j3);$ck->apply_($k3);$ck->apply_($l3);$ck->apply_($m3);$ck->apply_($n3);$ck->apply_($o3);$ck->apply_($p3);$ck->apply_($q3);$ck->apply_($r3);$ck->apply_($s3);$ck->apply_($t3);$ck->apply_($u3);$ck->apply_($w3);$ck->apply_($y3);$ck->apply_($A3);$ck->apply_($C3);$ck->apply_($E3);$ck->apply_($G3);$ck->apply_($H3);$ck->apply_($I3);$ck->apply_($K3);$ck->apply_($M3);$ck->apply_($O3);$ck->apply_($Q3);$ck->apply_($S3);$ck->apply_($U3);$ck->apply_($W3);$ck->apply_($Y3);$ck->apply_($c4);$ck->apply_($d4);$ck->apply_($f4);$ck->apply_($g4);$ck->apply_($i4);$ck->apply_($k4);$ck->apply_($m4);$ck->apply_($n4);$ck->apply_($o4);$ck->apply_($q4);$Ok->apply_($U2);$il->apply_($U2);$xl->apply_($l3);$xl->apply_($m3);$xl->apply_($n3);$xl->apply_($o3);$xl->apply_($p3);$xl->apply_($q3);$xl->apply_($r3);$xl->apply_($s3);$xl->apply_($t3);$xl->apply_($u3);$Ml->apply_($V2);$Tl->apply_($V2);$om->apply_($W2);$vm->apply_($W2);$Pm->apply_($W2);$ln->apply_($W2);$tn->apply_($W2);$Jn->apply_($W2);$io->apply_($X2);$io->apply_($Z2);$po->apply_($X2);$xo->apply_($X2);$No->apply_($X2);$No->apply_($Z2);$Yo->apply_($X2);$op->apply_($X2);$op->apply_($Z2);$Rp->apply_($o3);$lq->apply_($Y2);$tq->apply_($Y2);$Aq->apply_($Y2);$Qq->apply_($Y2);$Yq->apply_($Y2);$ur->apply_($Y2);$Qr->apply_($Z2);$Zr->apply_($Z2);$Ms->apply_($Ns);$Xs->apply_($c3);$mt->apply_($c3);$iu->apply_($f3);$pu->apply_($f3);$Du->apply_($f3);$Zu->apply_($v3);$Zu->apply_($x3);$Zu->apply_($z3);$Zu->apply_($p4);$nv->apply_($v3);$nv->apply_($x3);$nv->apply_($z3);$nv->apply_($p4);$Cv->apply_($v3);$Cv->apply_($x3);$Cv->apply_($z3);$Wv->apply_($v3);$Wv->apply_($x3);$Wv->apply_($z3);$ow->apply_($w3);$ow->apply_($y3);$ow->apply_($A3);$Kw->apply_($x3);$Rw->apply_($x3);$dx->apply_($x3);$px->apply_($x3);$Qx->apply_($y3);$ly->apply_($z3);$sy->apply_($z3);$Sy->apply_($D3);$Bz->apply_($H3);$Zz->apply_($A);$pA->apply_($A);$wA->apply_($A);$FA->apply_($A);$MA->apply_($A);$mB->apply_($J3);$tB->apply_($J3);$NB->apply_($J3);$jC->apply_($J3);$HC->apply_($Ns);$oD->apply_($N3);$GD->apply_($N3);$dE->apply_($N3);$mE->apply_($N3);$wE->apply_($N3);$SE->apply_($P3);$SE->apply_($C);$SE->apply_($H);$qF->apply_($R3);$zF->apply_($R3);$qG->apply_($Ns);$AG->apply_($V3);$GG->apply_($V3);$eH->apply_($X3);$eH->apply_($Z3);$oH->apply_($X3);$vH->apply_($X3);$JH->apply_($X3);$qI->apply_($C);$xI->apply_($C);$EI->apply_($C);$OI->apply_($C);$ZI->apply_($C);$qJ->apply_($d4);$yJ->apply_($d4);$RJ->apply_($e4);$iK->apply_($e4);$pK->apply_($e4);$IK->apply_($H);$OK->apply_($H);$UK->apply_($H);$kL->apply_($Ns);$ni::self=$yM;&$V($T);&$V($f1);&$V($l1);&$V($y1);&$V($L1);&$V($Y1);&$V($n2);&$V($J2);&$V($P2);&$V($F4);&$V($H4);&$J4($H4);&$V($Q4);&$V($m5);&$V($u5);&$V($G5);&$V($S5);&$V($g6);&$V($i6);&$J4($i6);&$V($o6);&$V($F6);&$V($Q6);&$V($e7);&$V($q7);&$V($K7);&$V($M7);&$J4($M7);&$V($h8);&$V($o8);&$V($z8);&$V($B8);&$J4($B8);&$V($H8);&$V($R8);&$V($X8);&$V($g9);&$V($n9);&$V($u9);&$V($C9);&$V($E9);&$J4($E9);&$V($K9);&$V($V9);&$V($da);&$V($na);&$V($wa);&$V($Ca);&$V($Ia);&$V($Sa);&$V($Ua);&$J4($Ua);&$V($qb);&$V($yb);&$V($Pb);&$V($fc);&$V($Dc);&$V($Lc);&$V($ld);&$V($Bd);&$V($Rd);&$V($ne);&$V($ve);&$V($xe);&$J4($xe);&$V($He);&$V($Pe);&$V($cf);&$V($rf);&$V($xf);&$V($zf);&$J4($zf);&$V($Yf);&$V($gg);&$V($ch);&$V($yh);&$V($di);&$V($fi);&$J4($fi);&$V($pi);&$V($Di);&$V($Fi);&$V($Li);&$V($Ui);&$V($gj);&$V($nj);&$V($uj);&$V($Bj);&$V($Ij);&$V($Kj);&$V($Mj);&$J4($Mj);&$V($Tj);&$V($ck);&$V($ek);&$J4($ek);&$V($nk);&$J4($nk);&$V($rk);&$J4($rk);&$V($tk);&$J4($tk);&$V($vk);&$J4($vk);&$V($Ak);&$J4($Ak);&$V($Fk);&$J4($Fk);&$V($Ok);&$V($il);&$V($kl);&$J4($kl);&$V($xl);&$V($zl);&$J4($zl);&$V($Bl);&$J4($Bl);&$V($Ml);&$V($Tl);&$V($Vl);&$J4($Vl);&$V($cm);&$J4($cm);&$V($om);&$V($vm);&$V($Pm);&$V($ln);&$V($tn);&$V($Jn);&$V($Ln);&$J4($Ln);&$V($Qn);&$J4($Qn);&$V($io);&$V($po);&$V($xo);&$V($No);&$V($Yo);&$V($op);&$V($rp);&$J4($rp);&$up($rp);&$V($Rp);&$V($Tp);&$J4($Tp);&$V($lq);&$V($tq);&$V($Aq);&$V($Qq);&$V($Yq);&$V($ur);&$V($wr);&$J4($wr);&$V($Br);&$J4($Br);&$V($Qr);&$V($Zr);&$V($ds);&$J4($ds);&$V($is);&$J4($is);&$V($Ms);&$V($Xs);&$V($mt);&$V($ot);&$J4($ot);&$V($tt);&$J4($tt);&$V($Kt);&$J4($Kt);&$V($iu);&$V($pu);&$V($Du);&$V($Fu);&$J4($Fu);&$V($Ku);&$J4($Ku);&$V($Zu);&$V($nv);&$V($pv);&$J4($pv);&$V($Cv);&$V($Wv);&$V($Yv);&$J4($Yv);&$dw($Yv);&$V($kw);&$J4($kw);&$V($ow);&$V($qw);&$J4($qw);&$V($Kw);&$V($Rw);&$V($dx);&$V($px);&$V($ux);&$J4($ux);&$dw($ux);&$xx($ux);&$V($Qx);&$V($Sx);&$J4($Sx);&$V($ly);&$V($sy);&$V($uy);&$J4($uy);&$dw($uy);&$V($zy);&$J4($zy);&$V($Gy);&$J4($Gy);&$V($Sy);&$V($Uy);&$J4($Uy);&$V($cz);&$J4($cz);&$V($lz);&$J4($lz);&$V($Bz);&$V($Dz);&$J4($Dz);&$V($Zz);&$V($pA);&$V($wA);&$V($FA);&$V($MA);&$V($OA);&$J4($OA);&$V($TA);&$J4($TA);&$V($mB);&$V($tB);&$V($NB);&$V($jC);&$V($lC);&$J4($lC);&$V($qC);&$J4($qC);&$V($HC);&$V($NC);&$J4($NC);&$V($oD);&$V($GD);&$V($dE);&$V($mE);&$V($wE);&$V($yE);&$J4($yE);&$V($DE);&$J4($DE);&$V($SE);&$V($UE);&$J4($UE);&$V($ZE);&$J4($ZE);&$V($qF);&$V($zF);&$V($BF);&$J4($BF);&$V($GF);&$J4($GF);&$V($SF);&$J4($SF);&$V($qG);&$V($AG);&$V($GG);&$V($IG);&$J4($IG);&$V($OG);&$J4($OG);&$V($eH);&$V($gH);&$J4($gH);&$V($oH);&$V($vH);&$V($JH);&$V($LH);&$J4($LH);&$V($SH);&$J4($SH);&$V($UH);&$J4($UH);&$V($qI);&$V($xI);&$V($EI);&$V($OI);&$V($ZI);&$V($dJ);&$J4($dJ);&$gJ($dJ);&$V($qJ);&$V($yJ);&$V($AJ);&$J4($AJ);&$V($RJ);&$V($iK);&$V($pK);&$V($rK);&$J4($rK);&$V($wK);&$J4($wK);&$V($IK);&$V($OK);&$V($UK);&$V($WK);&$J4($WK);&$V($dL);&$J4($dL);&$V($kL);&$V($sL);&$J4($sL);&$V($xL);&$J4($xL);&$V($GL);&$J4($GL);&$V($LL);&$J4($LL);&$V($QL);&$J4($QL);&$V($YL);&$J4($YL);ni->run(@ARGV);
__DATA__
