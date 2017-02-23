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
handles multiplexed IO using a single thread.#;$Z=[$i,$X,$Y];$c1=[$Z];$d1=q#/fabric#;$e1=bless({$e,$c1,$Q,$d1},$S);$f1=q#ni.doc:/fabric/remote#;$g1=q#A local proxy for a remote object. All method calls are converted to RMI
wrappers and return futures of results.#;$h1=[$i,$g1];$i1=[$h1];$j1=q#/fabric/remote#;$k1=bless({$e,$i1,$Q,$j1},$S);$l1=q#ni.doc:/fabric/rmi#;$m1=q#An open connection to another ni instance. This module provides the
ability to bootstrap a remote perl interpreter with an async RMI event
loop, and provides options about send/receive data encoding (which is
important for trust/security reasons).#;$n1=q#Method calls are proxied to named objects on the remote, and each one is
sent in a packet that contains the fully-serialized context for the
call.#;$o1=[$i,$m1,$n1];$p1=q#TODO#;$q1=q#referent#;$r1=q#applied_to#;$s1=q#fabric/rmi#;$t1={$s1,1};$u1=q#/fabric/rmi#;$v1=q#slices#;$w1=q#io/buffer#;$x1=q#io/cat#;$y1=q#io/exec#;$z1=q#io/fd#;$A1=q#io/file#;$B1=q#io/file_update_fd#;$C1=q#io/null#;$D1=q#io/object#;$E1=q#io/pid#;$F1=q#io/str#;$G1={$s1,1,$w1,1,$x1,1,$y1,1,$z1,1,$A1,1,$B1,1,$C1,1,$D1,1,$E1,1,$F1,1};$H1=q#/io/object#;$I1=q#class#;$J1=q#class.c#;$K1=q#fabric/remote#;$L1=q#fabric/remote.c#;$M1=q#fabric/rmi.c#;$N1=q#io/buffer.c#;$O1=q#io/cat.c#;$P1=q#io/exec.c#;$Q1=q#io/fd.c#;$R1=q#io/file.c#;$S1=q#io/file_update_fd.c#;$T1=q#io/null.c#;$U1=q#io/object.c#;$V1=q#io/pid.c#;$W1=q#io/str.c#;$X1=q#io/transfer#;$Y1=q#io/transfer.c#;$Z1=q#io/transfer_async#;$c2=q#io/transfer_async.c#;$d2=q#io/transfer_sync#;$e2=q#io/transfer_sync.c#;$f2=q#lib/behavior#;$g2=q#lib/behavior.c#;$h2=q#lib/branch#;$i2=q#lib/branch.c#;$j2=q#lib/dataslice#;$k2=q#lib/dataslice.c#;$l2=q#lib/doc.c#;$m2=q#lib/fn.c#;$n2=q#lib/future#;$o2=q#lib/future.c#;$p2=q#lib/image#;$q2=q#lib/image.c#;$r2=q#lib/ni#;$s2=q#lib/ni.c#;$t2=q#lib/object_metadata#;$u2=q#lib/object_metadata.c#;$v2=q#lib/quote_simple#;$w2=q#lib/quote_simple.c#;$x2=q#lib/slice#;$y2=q#lib/slice.c#;$z2=q#lib/tag#;$A2=q#lib/tag.c#;$B2=q#lib/test_assert_eq#;$C2=q#lib/test_assert_eq.c#;$D2=q#lib/test_assertion#;$E2=q#lib/test_assertion.c#;$F2=q#lib/test_case.c#;$G2=q#lib/test_value#;$H2=q#lib/test_value.c#;$I2=q#lib/todo.c#;$J2=q#metaclass#;$K2=q#metaclass.c#;$L2=q#module#;$M2=q#module.c#;$N2=q#object#;$O2=q#object.c#;$P2=q#semantic/dimension#;$Q2=q#semantic/dimension.c#;$R2=q#semantic/task#;$S2=q#semantic/task.c#;$T2={$I1,1,$J1,1,$K1,1,$L1,1,$s1,1,$M1,1,$w1,1,$N1,1,$x1,1,$O1,1,$y1,1,$P1,1,$z1,1,$Q1,1,$A1,1,$R1,1,$B1,1,$S1,1,$C1,1,$T1,1,$D1,1,$U1,1,$E1,1,$V1,1,$F1,1,$W1,1,$X1,1,$Y1,1,$Z1,1,$c2,1,$d2,1,$e2,1,$f2,1,$g2,1,$h2,1,$i2,1,$j2,1,$k2,1,$S,1,$l2,1,$A,1,$m2,1,$n2,1,$o2,1,$p2,1,$q2,1,$r2,1,$s2,1,$t2,1,$u2,1,$v2,1,$w2,1,$x2,1,$y2,1,$z2,1,$A2,1,$B2,1,$C2,1,$D2,1,$E2,1,$C,1,$F2,1,$G2,1,$H2,1,$H,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$N2,1,$O2,1,$P2,1,$Q2,1,$R2,1,$S2,1};$U2=q#/object#;$V2={};$W2=q#ctor#;$X2=q#dtor#;$Y2=q#methods#;$Z2=q#DESTROY#;$c3=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$d3=bless({$w,$c3,$y,$z},$A);$e3=q#ni 'ni:/' . ref shift#;$f3=bless({$w,$e3,$y,$z},$A);$g3={$Z2,$d3,$I1,$f3};$h3=q#/lib/instance.b#;$i3=bless({$r1,$V2,$W2,$q,$X2,$q,$Y2,$g3,$Q,$h3},$x2);$j3=[$i3];$k3=bless({$r1,$T2,$Q,$U2,$v1,$j3},$O2);$l3=q#my $s = shift; $s->apply($s->package)#;$m3=bless({$w,$l3,$y,$z},$A);$n3={};$o3=q#(bool#;$p3=[];$q3=bless({$t,$p3,$v,$q,$w,1,$y,$z},$A);$r3={$o3,$q3};$s3=q#/io/object_ops.b#;$t3=bless({$r1,$n3,$W2,$q,$X2,$q,$Y2,$r3,$Q,$s3},$x2);$u3={};$v3=q#die#;$w3=[];$x3=q#shift; die join " ", @_#;$y3=bless({$t,$w3,$v,$q,$w,$x3,$y,$z},$A);$z3=q#io_check#;$A3=[];$B3=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$C3=bless({$t,$A3,$v,$q,$w,$B3,$y,$z},$A);$D3=q#io_check_defined#;$E3=[];$F3=q#shift->io_check(sub {defined shift}, @_)#;$G3=bless({$t,$E3,$v,$q,$w,$F3,$y,$z},$A);$H3=q#io_check_true#;$I3=[];$J3=q#shift->io_check(sub {shift}, @_)#;$K3=bless({$t,$I3,$v,$q,$w,$J3,$y,$z},$A);$L3={$v3,$y3,$z3,$C3,$D3,$G3,$H3,$K3};$M3=q#/io/object_checks.b#;$N3=bless({$r1,$u3,$W2,$q,$X2,$q,$Y2,$L3,$Q,$M3},$x2);$O3={};$P3=q#(+#;$Q3=[];$R3=q#ni('ni:/io/cat')->new(@_[0, 1])#;$S3=bless({$t,$Q3,$v,$q,$w,$R3,$y,$z},$A);$T3={$P3,$S3};$U3=q#/io/object_constructors.b#;$V3=bless({$r1,$O3,$W2,$q,$X2,$q,$Y2,$T3,$Q,$U3},$x2);$W3={};$X3=q#read_all#;$Y3=[];$Z3=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$c4=bless({$t,$Y3,$v,$q,$w,$Z3,$y,$z},$A);$d4=q#write_all#;$e4=[];$f4=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$g4=bless({$t,$e4,$v,$q,$w,$f4,$y,$z},$A);$h4={$X3,$c4,$d4,$g4};$i4=q#/io/object_memory.b#;$j4=bless({$r1,$W3,$W2,$q,$X2,$q,$Y2,$h4,$Q,$i4},$x2);$k4={};$l4=q#connect_sync#;$m4=[];$n4=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$o4=bless({$t,$m4,$v,$q,$w,$n4,$y,$z},$A);$p4=q#into_sync#;$q4=[];$r4=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$s4=bless({$t,$q4,$v,$q,$w,$r4,$y,$z},$A);$t4={$l4,$o4,$p4,$s4};$u4=q#/io/object_transfer_sync.b#;$v4=bless({$r1,$k4,$W2,$q,$X2,$q,$Y2,$t4,$Q,$u4},$x2);$w4={};$x4=q#connect_async#;$y4=[];$z4=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$A4=bless({$t,$y4,$v,$q,$w,$z4,$y,$z},$A);$B4=q#into_async#;$C4=[];$D4=q#ni('ni:/io/transfer_async')->new(@_)->run#;$E4=bless({$t,$C4,$v,$q,$w,$D4,$y,$z},$A);$F4={$x4,$A4,$B4,$E4};$G4=q#/io/object_transfer_async.b#;$H4=bless({$r1,$w4,$W2,$q,$X2,$q,$Y2,$F4,$Q,$G4},$x2);$I4=[$k3,$t3,$N3,$V3,$j4,$v4,$H4,$H4,$v4,$H4,$v4];$J4=bless({$r1,$G1,$Q,$H1,$v1,$I4},$U1);$K4={};$L4=q#instantiate#;$M4=[];$N4=q#@slots#;$O4=q#arg_codec#;$P4=q#return_codec#;$Q4=q#image#;$R4=[$O4,$P4,$Q4];$S4=q#generator#;$T4=[];$U4=q#(arg_codec    => ni('ni:/fabric/qperl')->new,
          return_codec => ni('ni:/fabric/qjson')->new,
          image        => 1)#;$V4=bless({$t,$T4,$v,$q,$w,$U4,$y,$z},$A);$W4={$N4,$R4,$S4,$V4};$X4=q#my %defaults = &$generator(@_);
my $class    = shift;
my %args     = @_;
$defaults{$_} = $args{$_} for @slots;
\\%defaults;#;$Y4=bless({$t,$M4,$v,$W4,$w,$X4,$y,$z},$A);$Z4={$L4,$Y4};$c5=q#/fabric/rmi_init.b#;$d5=bless({$r1,$K4,$W2,$q,$X2,$q,$Y2,$Z4,$Q,$c5},$x2);$e5=[$J4,$d5];$f5=bless({$r1,$t1,$Q,$u1,$v1,$e5},$M1);$g5=q#The codec stuff is awful. Nothing about a codec is so global that it
should apply to all method calls; maybe a class has a per-method
codec disposition, or maybe we infer the best one by serialization
performance. Either way, global codecs don't make sense here.#;$h5=[$g5];$i5=bless({$q1,$f5,$E,$h5},$H);$j5=[$p1,$i5];$k5=[$o1,$j5];$l5=bless({$e,$k5,$Q,$u1},$S);$m5=q#ni.doc:/io#;$n5=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$o5=[$i,$n5];$p5=[$o5];$q5=q#/io#;$r5=bless({$e,$p5,$Q,$q5},$S);$s5=q#ni.doc:/io/buffer#;$t5=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$u5=[$f,$t5];$v5=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$w5=[];$x5=[];$y5=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$z5=bless({$t,$x5,$v,$q,$w,$y5,$y,$z},$A);$A5=bless({$n,$w5,$p,$q,$r,$q,$s,$z5},$C);$B5=[$i,$v5,$A5];$C5=[$u5,$B5];$D5=q#/io/buffer#;$E5=bless({$e,$C5,$Q,$D5},$S);$F5=q#ni.doc:/io/cat#;$G5=q#
my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
my $combined = $io1 + $io2 + $io3;
$combined->into_sync($destination_io);#;$H5=[$f,$G5];$I5=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$J5=[];$K5=[];$L5=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$M5=bless({$t,$K5,$v,$q,$w,$L5,$y,$z},$A);$N5=bless({$n,$J5,$p,$q,$r,$q,$s,$M5},$C);$O5=[$i,$I5,$N5];$P5=[$H5,$O5];$Q5=q#/io/cat#;$R5=bless({$e,$P5,$Q,$Q5},$S);$S5=q#ni.doc:/io/exec#;$T5=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$U5=[$f,$T5];$V5=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$W5=[];$X5=[];$Y5=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$Z5=bless({$t,$X5,$v,$q,$w,$Y5,$y,$z},$A);$c6=bless({$n,$W5,$p,$q,$r,$q,$s,$Z5},$C);$d6=[$i,$V5,$c6];$e6=[$U5,$d6];$f6=q#/io/exec#;$g6=bless({$e,$e6,$Q,$f6},$S);$h6=q#ni.doc:/io/fd#;$i6=q#
open my $fh, ...;
my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
my $fd = ni('ni:/io/fd')->new(0);   \# from number
my $fd = ni('fd:0');                \# same thing
$fd->nonblock(1)->read($_, 100);
$fd->be(10);                        \# move FD number#;$j6=[$f,$i6];$k6=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$l6=[];$m6=[];$n6=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$o6=bless({$t,$m6,$v,$q,$w,$n6,$y,$z},$A);$p6=bless({$n,$l6,$p,$q,$r,$q,$s,$o6},$C);$q6=[$i,$k6,$p6];$r6=[$j6,$q6];$s6=q#/io/fd#;$t6=bless({$e,$r6,$Q,$s6},$S);$u6=q#ni.doc:/io/file#;$v6=q#
my $f = ni('ni:/io/file')->new('/etc/passwd');
my $f = ni('file:/etc/passwd');     \# same as above
$f->into_sync(ni('fd:1'));          \# cat to stdout#;$w6=[$f,$v6];$x6=q#warning#;$y6=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$z6=[$x6,$y6];$A6=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$B6=[];$C6=[];$D6=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$E6=bless({$t,$C6,$v,$q,$w,$D6,$y,$z},$A);$F6=bless({$n,$B6,$p,$q,$r,$q,$s,$E6},$C);$G6=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$H6=[];$I6=[];$J6=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$K6=bless({$t,$I6,$v,$q,$w,$J6,$y,$z},$A);$L6=bless({$n,$H6,$p,$q,$r,$q,$s,$K6},$C);$M6=[$i,$A6,$F6,$G6,$L6];$N6=[$w6,$z6,$M6];$O6=q#/io/file#;$P6=bless({$e,$N6,$Q,$O6},$S);$Q6=q#ni.doc:/io/file_update_fd#;$R6=q#A write fd that performs a file rename upon closing.#;$S6=[$i,$R6];$T6=[$S6];$U6=q#/io/file_update_fd#;$V6=bless({$e,$T6,$Q,$U6},$S);$W6=q#ni.doc:/io/object#;$X6=q#migrate die() into /lib/ as a base behavior#;$Y6=[$X6];$Z6=bless({$q1,$J4,$E,$Y6},$H);$c7=[$p1,$Z6];$d7=[$c7];$e7=bless({$e,$d7,$Q,$H1},$S);$f7=q#ni.doc:/io/pid#;$g7=q#eg#;$h7=[];$i7={$E1,1};$j7=q#/io/pid#;$k7={};$l7=q#pid#;$m7=[];$n7=q#shift->{'pid'}#;$o7=bless({$t,$m7,$v,$q,$w,$n7,$y,$z},$A);$p7=q#status#;$q7=[];$r7=q#shift->{'status'}#;$s7=bless({$t,$q7,$v,$q,$w,$r7,$y,$z},$A);$t7={$l7,$o7,$p7,$s7};$u7=q#/io/pid_readers.b#;$v7=bless({$r1,$k7,$W2,$q,$X2,$q,$Y2,$t7,$Q,$u7},$x2);$w7={};$x7=[];$y7=q#shift->await#;$z7=bless({$t,$x7,$v,$q,$w,$y7,$y,$z},$A);$A7=[];$B7=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$C7=bless({$t,$A7,$v,$q,$w,$B7,$y,$z},$A);$D7={$L4,$C7};$E7=q#/io/pid_init.b#;$F7=bless({$r1,$w7,$W2,$q,$X2,$z7,$Y2,$D7,$Q,$E7},$x2);$G7={};$H7=q#await#;$I7=[];$J7=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$K7=bless({$t,$I7,$v,$q,$w,$J7,$y,$z},$A);$L7=q#running#;$M7=[];$N7=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$O7=bless({$t,$M7,$v,$q,$w,$N7,$y,$z},$A);$P7={$H7,$K7,$L7,$O7};$Q7=q#/io/pid_wait.b#;$R7=bless({$r1,$G7,$W2,$q,$X2,$q,$Y2,$P7,$Q,$Q7},$x2);$S7={};$T7=q#read#;$U7=[];$V7=q#shift->stdout->read(@_)#;$W7=bless({$t,$U7,$v,$q,$w,$V7,$y,$z},$A);$X7=q#write#;$Y7=[];$Z7=q#shift->stdin->write(@_)#;$c8=bless({$t,$Y7,$v,$q,$w,$Z7,$y,$z},$A);$d8={$T7,$W7,$X7,$c8};$e8=q#/io/pid_io.b#;$f8=bless({$r1,$S7,$W2,$q,$X2,$q,$Y2,$d8,$Q,$e8},$x2);$g8={};$h8=q#fd#;$i8=[];$j8=q#$_[0]->{external_fds}{$_[1]}#;$k8=bless({$t,$i8,$v,$q,$w,$j8,$y,$z},$A);$l8=q#stderr#;$m8=[];$n8=q#shift->fd(2)#;$o8=bless({$t,$m8,$v,$q,$w,$n8,$y,$z},$A);$p8=q#stdin#;$q8=[];$r8=q#shift->fd(0)#;$s8=bless({$t,$q8,$v,$q,$w,$r8,$y,$z},$A);$t8=q#stdout#;$u8=[];$v8=q#shift->fd(1)#;$w8=bless({$t,$u8,$v,$q,$w,$v8,$y,$z},$A);$x8={$h8,$k8,$l8,$o8,$p8,$s8,$t8,$w8};$y8=q#/io/pid_accessors.b#;$z8=bless({$r1,$g8,$W2,$q,$X2,$q,$Y2,$x8,$Q,$y8},$x2);$A8=[$J4,$v7,$F7,$R7,$f8,$z8];$B8=bless({$r1,$i7,$Q,$j7,$v1,$A8},$V1);$C8=[];$D8=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$E8=bless({$t,$C8,$v,$q,$w,$D8,$y,$z},$A);$F8=bless({$n,$h7,$p,$q,$r,$q,$q1,$B8,$s,$E8},$C);$G8=[$g7,$F8];$H8=[];$I8=[];$J8=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$K8=bless({$t,$I8,$v,$q,$w,$J8,$y,$z},$A);$L8=bless({$n,$H8,$p,$q,$r,$q,$q1,$B8,$s,$K8},$C);$M8=[$g7,$L8];$N8=[];$O8=[];$P8=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$Q8=bless({$t,$O8,$v,$q,$w,$P8,$y,$z},$A);$R8=bless({$n,$N8,$p,$q,$r,$q,$q1,$B8,$s,$Q8},$C);$S8=[$g7,$R8];$T8=[$G8,$M8,$S8];$U8=bless({$e,$T8,$Q,$j7},$S);$V8=q#ni.doc:/lib#;$W8=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$X8=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$Y8=[$i,$W8,$X8];$Z8=[$Y8];$c9=q#/lib#;$d9=bless({$e,$Z8,$Q,$c9},$S);$e9=q#ni.doc:/lib/dataslice#;$f9={$j2,1};$g9=q#/lib/dataslice#;$h9={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$h2,1,$i2,1,$j2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$x2,1,$y2,1,$z2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$i9=q#/lib/behavior#;$j9={};$k9=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$l9=bless({$w,$k9,$y,$z},$A);$m9={$e,$l9};$n9=q#/lib/documentable.b#;$o9=bless({$r1,$j9,$W2,$q,$X2,$q,$Y2,$m9,$Q,$n9},$x2);$p9=[$k3,$o9];$q9=bless({$r1,$h9,$Q,$i9,$v1,$p9},$g2);$r9={};$s9=q#$_[0]->namespace . ":" . $_[0]->{name}#;$t9=bless({$w,$s9,$y,$z},$A);$u9={$Q,$t9};$v9=q#/lib/named.b#;$w9=bless({$r1,$r9,$W2,$V,$X2,$q,$Y2,$u9,$Q,$v9},$x2);$x9={};$y9=q#apply#;$z9=q#shift->apply_(@_)#;$A9=bless({$w,$z9,$y,$z},$A);$B9=q#apply_#;$C9=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$D9=bless({$w,$C9,$y,$z},$A);$E9={$y9,$A9,$B9,$D9};$F9=q#/lib/dataslice.b#;$G9=bless({$r1,$x9,$W2,$q,$X2,$q,$Y2,$E9,$Q,$F9},$x2);$H9={};$I9=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$J9=bless({$w,$I9,$y,$z},$A);$K9={$L4,$J9};$L9=q#/lib/dataslice_init.b#;$M9=bless({$r1,$H9,$W2,$q,$X2,$q,$Y2,$K9,$Q,$L9},$x2);$N9={};$O9=q#namespace#;$P9=q#'ni'#;$Q9=bless({$w,$P9,$y,$z},$A);$R9={$O9,$Q9};$S9=q#/lib/named_in_ni.b#;$T9=bless({$r1,$N9,$W2,$q,$X2,$q,$Y2,$R9,$Q,$S9},$x2);$U9={};$V9=q#package#;$W9=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$X9=bless({$w,$W9,$y,$z},$A);$Y9={$V9,$X9};$Z9=q#/lib/namespaced.b#;$ca=bless({$r1,$U9,$W2,$q,$X2,$q,$Y2,$Y9,$Q,$Z9},$x2);$da={};$ea=q#resolve#;$fa=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$ga=bless({$w,$fa,$y,$z},$A);$ha={$ea,$ga};$ia=q#/lib/resolver.b#;$ja=bless({$r1,$da,$W2,$q,$X2,$q,$Y2,$ha,$Q,$ia},$x2);$ka={};$la=q#serialize#;$ma=[];$na=q#local $_;
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
$g;#;$oa=bless({$t,$ma,$v,$q,$w,$na,$y,$z},$A);$pa={$la,$oa};$qa=q#/lib/slice_serialize.b#;$ra=bless({$r1,$ka,$W2,$q,$X2,$q,$Y2,$pa,$Q,$qa},$x2);$sa=[$q9,$w9,$G9,$M9,$T9,$ca,$ja,$ra];$ta=bless({$r1,$f9,$Q,$g9,$v1,$sa},$k2);$ua=q#Fix serialization for dataslices#;$va=[$ua];$wa=bless({$q1,$ta,$E,$va},$H);$xa=[$p1,$wa];$ya=[$xa];$za=bless({$e,$ya,$Q,$g9},$S);$Aa=q#ni.doc:/lib/doc#;$Ba=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$Ca=[$f,$Ba];$Da=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$Ea=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$Fa=[];$Ga=[];$Ha=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$Ia=bless({$t,$Ga,$v,$q,$w,$Ha,$y,$z},$A);$Ja=bless({$n,$Fa,$p,$q,$r,$q,$s,$Ia},$C);$Ka=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$La=[];$Ma=[];$Na=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$Oa=bless({$t,$Ma,$v,$q,$w,$Na,$y,$z},$A);$Pa=bless({$n,$La,$p,$q,$r,$q,$s,$Oa},$C);$Qa=[$i,$Da,$Ea,$Ja,$Ka,$Pa];$Ra=[$Ca,$Qa];$Sa=q#/lib/doc#;$Ta=bless({$e,$Ra,$Q,$Sa},$S);$Ua=q#ni.doc:/lib/future#;$Va=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$Wa=[];$Xa=[];$Ya=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$Za=bless({$t,$Xa,$v,$q,$w,$Ya,$y,$z},$A);$cb=bless({$n,$Wa,$p,$q,$r,$q,$s,$Za},$C);$db=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$eb=[];$fb=[];$gb=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$hb=bless({$t,$fb,$v,$q,$w,$gb,$y,$z},$A);$ib=bless({$n,$eb,$p,$q,$r,$q,$s,$hb},$C);$jb=[$i,$Va,$cb,$db,$ib];$kb=[$jb];$lb=q#/lib/future#;$mb=bless({$e,$kb,$Q,$lb},$S);$nb=q#ni.doc:/lib/image#;$ob=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$pb=[$f,$ob];$qb=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$rb=[$i,$qb];$sb={$p2,1};$tb=q#/lib/image#;$ub={};$vb=[];$wb=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$xb=bless({$t,$vb,$v,$q,$w,$wb,$y,$z},$A);$yb={$L4,$xb};$zb=q#/lib/image_init.b#;$Ab=bless({$r1,$ub,$W2,$q,$X2,$q,$Y2,$yb,$Q,$zb},$x2);$Bb={};$Cb=q#boot_side_effect#;$Db=[];$Eb=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Fb=bless({$t,$Db,$v,$q,$w,$Eb,$y,$z},$A);$Gb=q#finalizer#;$Hb=[];$Ib=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Jb=bless({$t,$Hb,$v,$q,$w,$Ib,$y,$z},$A);$Kb=q#io#;$Lb=[];$Mb=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Nb=bless({$t,$Lb,$v,$q,$w,$Mb,$y,$z},$A);$Ob=q#reconstruction#;$Pb=[];$Qb=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Rb=bless({$t,$Pb,$v,$q,$w,$Qb,$y,$z},$A);$Sb=q#side_effect#;$Tb=[];$Ub=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Vb=bless({$t,$Tb,$v,$q,$w,$Ub,$y,$z},$A);$Wb={$Cb,$Fb,$Gb,$Jb,$Kb,$Nb,$Ob,$Rb,$Sb,$Vb};$Xb=q#/lib/image_quoting.b#;$Yb=bless({$r1,$Bb,$W2,$q,$X2,$q,$Y2,$Wb,$Q,$Xb},$x2);$Zb={};$cc=q#quote_code#;$dc=[];$ec=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$fc=bless({$t,$dc,$v,$q,$w,$ec,$y,$z},$A);$gc={$cc,$fc};$hc=q#/lib/quote_code_fail.b#;$ic=bless({$r1,$Zb,$W2,$q,$X2,$q,$Y2,$gc,$Q,$hc},$x2);$jc={};$kc=q#quote_array#;$lc=[];$mc=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$nc=bless({$t,$lc,$v,$q,$w,$mc,$y,$z},$A);$oc=q#quote_hash#;$pc=[];$qc=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$rc=bless({$t,$pc,$v,$q,$w,$qc,$y,$z},$A);$sc=q#quote_scalar#;$tc=[];$uc=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$vc=bless({$t,$tc,$v,$q,$w,$uc,$y,$z},$A);$wc=q#quote_scalar_ref#;$xc=[];$yc=q#'\\\\' . shift->quote(${$_[0]})#;$zc=bless({$t,$xc,$v,$q,$w,$yc,$y,$z},$A);$Ac=q#quote_value#;$Bc=[];$Cc=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Dc=bless({$t,$Bc,$v,$q,$w,$Cc,$y,$z},$A);$Ec={$kc,$nc,$oc,$rc,$sc,$vc,$wc,$zc,$Ac,$Dc};$Fc=q#/lib/quote_values.b#;$Gc=bless({$r1,$jc,$W2,$q,$X2,$q,$Y2,$Ec,$Q,$Fc},$x2);$Hc={};$Ic=q#quote_blessed#;$Jc=[];$Kc=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Lc=bless({$t,$Jc,$v,$q,$w,$Kc,$y,$z},$A);$Mc=q#quote_class#;$Nc=[];$Oc=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Pc=bless({$t,$Nc,$v,$q,$w,$Oc,$y,$z},$A);$Qc=q#quote_object#;$Rc=[];$Sc=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$Tc=bless({$t,$Rc,$v,$q,$w,$Sc,$y,$z},$A);$Uc={$Ic,$Lc,$Mc,$Pc,$Qc,$Tc};$Vc=q#/lib/quote_objects.b#;$Wc=bless({$r1,$Hc,$W2,$q,$X2,$q,$Y2,$Uc,$Q,$Vc},$x2);$Xc={};$Yc=q#circular_arrayref#;$Zc=[];$cd=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$dd=bless({$t,$Zc,$v,$q,$w,$cd,$y,$z},$A);$ed=q#circular_hashref#;$fd=[];$gd=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$hd=bless({$t,$fd,$v,$q,$w,$gd,$y,$z},$A);$id=q#is_circular#;$jd=[];$kd=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$ld=bless({$t,$jd,$v,$q,$w,$kd,$y,$z},$A);$md={$Yc,$dd,$ed,$hd,$id,$ld};$nd=q#/lib/quote_circular_addressed.b#;$od=bless({$r1,$Xc,$W2,$q,$X2,$q,$Y2,$md,$Q,$nd},$x2);$pd={};$qd=q#address#;$rd=[];$sd=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$td=bless({$t,$rd,$v,$q,$w,$sd,$y,$z},$A);$ud=q#allocate_gensym#;$vd=[];$wd=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$xd=bless({$t,$vd,$v,$q,$w,$wd,$y,$z},$A);$yd=q#circular_links#;$zd=[];$Ad=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Bd=bless({$t,$zd,$v,$q,$w,$Ad,$y,$z},$A);$Cd=q#quote#;$Dd=[];$Ed=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Fd=bless({$t,$Dd,$v,$q,$w,$Ed,$y,$z},$A);$Gd={$qd,$td,$ud,$xd,$yd,$Bd,$Cd,$Fd};$Hd=q#/lib/quote_gensym_identity.b#;$Id=bless({$r1,$pd,$W2,$q,$X2,$q,$Y2,$Gd,$Q,$Hd},$x2);$Jd={};$Kd=q#gensym#;$Ld=[];$Md=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Nd=bless({$t,$Ld,$v,$q,$w,$Md,$y,$z},$A);$Od={$Kd,$Nd};$Pd=q#/lib/gensym_generator_compact.b#;$Qd=bless({$r1,$Jd,$W2,$q,$X2,$q,$Y2,$Od,$Q,$Pd},$x2);$Rd=[$k3,$Ab,$Yb,$ic,$Gc,$Wc,$od,$Id,$Qd];$Sd=bless({$r1,$sb,$Q,$tb,$v1,$Rd},$q2);$Td=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$Ud=[$Td];$Vd=bless({$q1,$Sd,$E,$Ud},$H);$Wd=[$p1,$Vd];$Xd=[$pb,$rb,$Wd];$Yd=bless({$e,$Xd,$Q,$tb},$S);$Zd=q#ni.doc:/lib/ni#;$ce=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$de=[$f,$ce];$ee=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$fe=[$i,$ee];$ge=[$de,$fe];$he=q#/lib/ni#;$ie=bless({$e,$ge,$Q,$he},$S);$je=q#ni.doc:/lib/quote_simple#;$ke=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$le=[];$me=[];$ne=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$oe=bless({$t,$me,$v,$q,$w,$ne,$y,$z},$A);$pe=bless({$n,$le,$p,$q,$r,$q,$s,$oe},$C);$qe=[$i,$ke,$pe];$re=[$qe];$se=q#/lib/quote_simple#;$te=bless({$e,$re,$Q,$se},$S);$ue=q#ni.doc:/lib/slice#;$ve=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$we=[$f,$ve];$xe={$x2,1};$ye=q#/lib/slice#;$ze=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$Ae=bless({$w,$ze,$y,$z},$A);$Be=q#local $_;
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
$self;#;$Ce=bless({$w,$Be,$y,$z},$A);$De=q#lib/slice::apply#;$Ee=q#lib/slice::apply_#;$Fe={};$Ge={$y9,$Ae,$B9,$Ce};$He=q#/lib/slice.b#;$Ie=bless({$r1,$Fe,$Y2,$Ge,$Q,$He},$x2);$Je={};$Ke=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$Le=bless({$w,$Ke,$y,$z},$A);$Me={$L4,$Le};$Ne=q#/lib/slice_init.b#;$Oe=bless({$r1,$Je,$Y2,$Me,$Q,$Ne},$x2);$Pe=[$q9,$w9,$Ie,$Oe,$ra];$Qe=bless({$r1,$xe,$Q,$ye,$v1,$Pe},$y2);$Re=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$Se=[$Re];$Te=bless({$q1,$Qe,$E,$Se},$H);$Ue=[$p1,$Te];$Ve=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$We=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$Xe=[];$Ye=[];$Ze=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$cf=bless({$t,$Ye,$v,$q,$w,$Ze,$y,$z},$A);$df=bless({$n,$Xe,$p,$q,$r,$q,$s,$cf},$C);$ef=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$ff=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$gf=[];$hf=[];$if=q#my $instances = 0;
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
now $instances == 0;#;$jf=bless({$t,$hf,$v,$q,$w,$if,$y,$z},$A);$kf=bless({$n,$gf,$p,$q,$r,$q,$s,$jf},$C);$lf=[$i,$Ve,$We,$df,$ef,$ff,$kf];$mf=[$we,$Ue,$lf];$nf=bless({$e,$mf,$Q,$ye},$S);$of=q#ni.doc:/semantic#;$pf=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$qf=[$i,$pf];$rf=[$qf];$sf=q#/semantic#;$tf=bless({$e,$rf,$Q,$sf},$S);$uf=q#ni:/class#;$vf={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$K2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$wf={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$xf=q#/module#;$yf=q#/lib/perlbranch.b#;$zf={};$Af=q#add#;$Bf=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Cf=bless({$w,$Bf,$y,$z},$A);$Df=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Ef=bless({$w,$Df,$y,$z},$A);$Ff={$Af,$Cf,$y9,$Ef};$Gf=q#/lib/branch.b#;$Hf=bless({$r1,$zf,$W2,$q,$X2,$q,$Y2,$Ff,$Q,$Gf},$x2);$If=[$Hf,$w9,$T9,$ca,$ja];$Jf=bless({$Q,$yf,$v1,$If},$z2);$Kf={};$Lf=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$Mf=bless({$w,$Lf,$y,$z},$A);$Nf={$L4,$Mf};$Of=q#/lib/class_init.b#;$Pf=bless({$r1,$Kf,$W2,$m3,$X2,$q,$Y2,$Nf,$Q,$Of},$x2);$Qf={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$h2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Rf=q#/lib/definition.b#;$Sf={};$Tf=q#def#;$Uf=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Vf=bless({$w,$Uf,$y,$z},$A);$Wf={$Tf,$Vf};$Xf=q#/lib/definition_def.b#;$Yf=bless({$r1,$Sf,$W2,$q,$X2,$q,$Y2,$Wf,$Q,$Xf},$x2);$Zf={};$cg=q#ro#;$dg=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$eg=bless({$w,$dg,$y,$z},$A);$fg=q#rw#;$gg=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$hg=bless({$w,$gg,$y,$z},$A);$ig={$cg,$eg,$fg,$hg};$jg=q#/lib/accessor.b#;$kg=bless({$r1,$Zf,$W2,$q,$X2,$q,$Y2,$ig,$Q,$jg},$x2);$lg={};$mg=q#(""#;$ng=q#shift->name#;$og=bless({$w,$ng,$y,$z},$A);$pg={$mg,$og};$qg=q#/lib/name_as_string.b#;$rg=bless({$r1,$lg,$W2,$q,$X2,$q,$Y2,$pg,$Q,$qg},$x2);$sg={};$tg=q#(eq#;$ug=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$vg=bless({$w,$ug,$y,$z},$A);$wg={$tg,$vg};$xg=q#/lib/ref_eq.b#;$yg=bless({$r1,$sg,$W2,$q,$X2,$q,$Y2,$wg,$Q,$xg},$x2);$zg={};$Ag=q#defdata#;$Bg=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$Cg=bless({$w,$Bg,$y,$z},$A);$Dg={$Ag,$Cg};$Eg=q#/lib/definition_defdata.b#;$Fg=bless({$r1,$zg,$W2,$q,$X2,$q,$Y2,$Dg,$Q,$Eg},$x2);$Gg={};$Hg=q#instantiate_with_defaults#;$Ig=q#my ($class, @slots) = @_;
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
    }));#;$Jg=bless({$w,$Ig,$y,$z},$A);$Kg={$Hg,$Jg};$Lg=q#/lib/definition_init_with_defaults.b#;$Mg=bless({$r1,$Gg,$W2,$q,$X2,$q,$Y2,$Kg,$Q,$Lg},$x2);$Ng=[$Yf,$kg,$rg,$yg,$Fg,$Mg];$Og=bless({$r1,$Qf,$Q,$Rf,$v1,$Ng},$h2);$Pg=[$Jf,$Pf,$k3,$q9,$Og];$Qg=bless({$r1,$wf,$Q,$xf,$v1,$Pg},$M2);$Rg={};$Sg=q#new#;$Tg=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$Ug=bless({$w,$Tg,$y,$z},$A);$Vg={$Sg,$Ug};$Wg=q#/lib/instantiable.b#;$Xg=bless({$r1,$Rg,$Y2,$Vg,$Q,$Wg},$x2);$Yg={};$Zg=q#child#;$ch=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$dh=bless({$w,$ch,$y,$z},$A);$eh={$Zg,$dh};$fh=q#/lib/subclass.b#;$gh=bless({$r1,$Yg,$W2,$q,$X2,$q,$Y2,$eh,$Q,$fh},$x2);$hh=[$Qg,$Xg,$Pf,$Qg,$gh];$ih=bless({$r1,$vf,$Q,$R,$v1,$hh},$J1);$jh=q#ni:/class.c#;$kh={$J1,1,$Q2,1};$lh=q#/class.c#;$mh={$J1,1,$M2,1,$Q2,1};$nh=q#/module.c#;$oh={$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$M2,1,$O2,1,$Q2,1,$S2,1};$ph=q#/object.c#;$qh=[$ih];$rh=bless({$r1,$oh,$Q,$ph,$v1,$qh},$J2);$sh={$J1,1,$g2,1,$i2,1,$k2,1,$y2,1,$A2,1,$M2,1,$Q2,1};$th=q#/lib/behavior.c#;$uh=[$rh];$vh=bless({$r1,$sh,$Q,$th,$v1,$uh},$J2);$wh=[$rh,$Xg,$vh];$xh=bless({$r1,$mh,$Q,$nh,$v1,$wh},$J2);$yh=[$xh];$zh=bless({$r1,$kh,$Q,$lh,$v1,$yh},$J2);$Ah=q#ni:/fabric#;$Bh=q#fabric#;$Ch={$Bh,1};$Dh=[];$Eh=bless({$r1,$Ch,$Q,$d1,$v1,$Dh},$L2);$Fh=q#ni:/fabric/remote#;$Gh={$K1,1};$Hh={};$Ih=[];$Jh=q#my ($class, $rmi, $name) = @_;
+{rmi  => $rmi,
  name => $name};#;$Kh=bless({$t,$Ih,$v,$q,$w,$Jh,$y,$z},$A);$Lh={$L4,$Kh};$Mh=q#/fabric/remote_init.b#;$Nh=bless({$r1,$Hh,$W2,$q,$X2,$q,$Y2,$Lh,$Q,$Mh},$x2);$Oh={};$Ph=q#AUTOLOAD#;$Qh=[];$Rh=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{rmi}->call($$self{name}, $method, @_);#;$Sh=bless({$t,$Qh,$v,$q,$w,$Rh,$y,$z},$A);$Th={$Ph,$Sh};$Uh=q#/fabric/remote_proxy.b#;$Vh=bless({$r1,$Oh,$W2,$q,$X2,$q,$Y2,$Th,$Q,$Uh},$x2);$Wh=[$k3,$Nh,$Vh];$Xh=bless({$r1,$Gh,$Q,$j1,$v1,$Wh},$L1);$Yh=q#ni:/fabric/remote.c#;$Zh={$L1,1};$ci=q#/fabric/remote.c#;$di=[$rh];$ei=bless({$r1,$Zh,$Q,$ci,$v1,$di},$J2);$fi=q#ni:/fabric/remote_init.b#;$gi=q#ni:/fabric/remote_proxy.b#;$hi=q#ni:/fabric/rmi#;$ii=q#ni:/fabric/rmi.c#;$ji={$M1,1};$ki=q#/fabric/rmi.c#;$li={$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1};$mi=q#/io/object.c#;$ni={};$oi=q#def_transfer_method#;$pi=[];$qi=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$ri=bless({$t,$pi,$v,$q,$w,$qi,$y,$z},$A);$si={$oi,$ri};$ti=q#/io/object.c_transfer_def.b#;$ui=bless({$r1,$ni,$W2,$q,$X2,$q,$Y2,$si,$Q,$ti},$x2);$vi=[$rh,$ui];$wi=bless({$r1,$li,$Q,$mi,$v1,$vi},$J2);$xi=[$wi];$yi=bless({$r1,$ji,$Q,$ki,$v1,$xi},$J2);$zi=q#ni:/fabric/rmi_init.b#;$Ai=q#ni:/io#;$Bi={$Kb,1};$Ci=[];$Di=bless({$r1,$Bi,$Q,$q5,$v1,$Ci},$L2);$Ei=q#ni:/io/buffer#;$Fi={$w1,1};$Gi={};$Hi=[];$Ii=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Ji=bless({$t,$Hi,$v,$q,$w,$Ii,$y,$z},$A);$Ki={$L4,$Ji};$Li=q#/io/buffer_init.b#;$Mi=bless({$r1,$Gi,$W2,$q,$X2,$q,$Y2,$Ki,$Q,$Li},$x2);$Ni={};$Oi=[];$Pi=q#my $self = shift;
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
}#;$Qi=bless({$t,$Oi,$v,$q,$w,$Pi,$y,$z},$A);$Ri=q#read_capacity#;$Si=[];$Ti=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Ui=bless({$t,$Si,$v,$q,$w,$Ti,$y,$z},$A);$Vi=[];$Wi=q#my $self = shift;
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
}#;$Xi=bless({$t,$Vi,$v,$q,$w,$Wi,$y,$z},$A);$Yi=q#write_capacity#;$Zi=[];$cj=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$dj=bless({$t,$Zi,$v,$q,$w,$cj,$y,$z},$A);$ej={$T7,$Qi,$Ri,$Ui,$X7,$Xi,$Yi,$dj};$fj=q#/io/buffer_io.b#;$gj=bless({$r1,$Ni,$W2,$q,$X2,$q,$Y2,$ej,$Q,$fj},$x2);$hj=[$J4,$Mi,$gj];$ij=bless({$r1,$Fi,$Q,$D5,$v1,$hj},$N1);$jj=q#ni:/io/buffer.c#;$kj={$N1,1};$lj=q#/io/buffer.c#;$mj=[$wi];$nj=bless({$r1,$kj,$Q,$lj,$v1,$mj},$J2);$oj=q#ni:/io/buffer_init.b#;$pj=q#ni:/io/buffer_io.b#;$qj=q#ni:/io/cat#;$rj={$x1,1};$sj={};$tj=[];$uj=q#shift; +{fs => [@_]}#;$vj=bless({$t,$tj,$v,$q,$w,$uj,$y,$z},$A);$wj={$L4,$vj};$xj=q#/io/cat_init.b#;$yj=bless({$r1,$sj,$W2,$q,$X2,$q,$Y2,$wj,$Q,$xj},$x2);$zj={};$Aj=[];$Bj=q#my $fs = shift->{fs};
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
$total_read;#;$Cj=bless({$t,$Aj,$v,$q,$w,$Bj,$y,$z},$A);$Dj={$T7,$Cj};$Ej=q#/io/cat_read.b#;$Fj=bless({$r1,$zj,$W2,$q,$X2,$q,$Y2,$Dj,$Q,$Ej},$x2);$Gj=[$J4,$yj,$Fj];$Hj=bless({$r1,$rj,$Q,$Q5,$v1,$Gj},$O1);$Ij=q#ni:/io/cat.c#;$Jj={$O1,1};$Kj=q#/io/cat.c#;$Lj=[$wi];$Mj=bless({$r1,$Jj,$Q,$Kj,$v1,$Lj},$J2);$Nj=q#ni:/io/cat_init.b#;$Oj=q#ni:/io/cat_read.b#;$Pj=q#ni:/io/exec#;$Qj={$y1,1};$Rj={};$Sj=q#argv#;$Tj=[];$Uj=q#shift->{'argv'}#;$Vj=bless({$t,$Tj,$v,$q,$w,$Uj,$y,$z},$A);$Wj={$Sj,$Vj};$Xj=q#/io/exec_ro.b#;$Yj=bless({$r1,$Rj,$W2,$q,$X2,$q,$Y2,$Wj,$Q,$Xj},$x2);$Zj={};$ck=[];$dk=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$ek=bless({$t,$ck,$v,$q,$w,$dk,$y,$z},$A);$fk={$L4,$ek};$gk=q#/io/exec_init.b#;$hk=bless({$r1,$Zj,$W2,$q,$X2,$q,$Y2,$fk,$Q,$gk},$x2);$ik={};$jk=q#connect#;$kk=[];$lk=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$mk=bless({$t,$kk,$v,$q,$w,$lk,$y,$z},$A);$nk=q#in_pipe#;$ok=[];$pk=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$qk=bless({$t,$ok,$v,$q,$w,$pk,$y,$z},$A);$rk=q#out_pipe#;$sk=[];$tk=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$uk=bless({$t,$sk,$v,$q,$w,$tk,$y,$z},$A);$vk=q#setup_stdio#;$wk=[];$xk=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$yk=bless({$t,$wk,$v,$q,$w,$xk,$y,$z},$A);$zk={$jk,$mk,$nk,$qk,$rk,$uk,$vk,$yk};$Ak=q#/io/exec_io_setup.b#;$Bk=bless({$r1,$ik,$W2,$q,$X2,$q,$Y2,$zk,$Q,$Ak},$x2);$Ck={};$Dk=q#binds_fd#;$Ek=[];$Fk=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$Gk=bless({$t,$Ek,$v,$q,$w,$Fk,$y,$z},$A);$Hk=[];$Ik=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Jk=bless({$t,$Hk,$v,$q,$w,$Ik,$y,$z},$A);$Kk=[];$Lk=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Mk=bless({$t,$Kk,$v,$q,$w,$Lk,$y,$z},$A);$Nk=[];$Ok=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Pk=bless({$t,$Nk,$v,$q,$w,$Ok,$y,$z},$A);$Qk=[];$Rk=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Sk=bless({$t,$Qk,$v,$q,$w,$Rk,$y,$z},$A);$Tk={$Dk,$Gk,$h8,$Jk,$l8,$Mk,$p8,$Pk,$t8,$Sk};$Uk=q#/io/exec_io_accessors.b#;$Vk=bless({$r1,$Ck,$W2,$q,$X2,$q,$Y2,$Tk,$Q,$Uk},$x2);$Wk={};$Xk=q#env#;$Yk=[];$Zk=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$cl=bless({$t,$Yk,$v,$q,$w,$Zk,$y,$z},$A);$dl={$Xk,$cl};$el=q#/io/exec_env.b#;$fl=bless({$r1,$Wk,$W2,$q,$X2,$q,$Y2,$dl,$Q,$el},$x2);$gl={};$hl=q#exec#;$il=[];$jl=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$kl=bless({$t,$il,$v,$q,$w,$jl,$y,$z},$A);$ll=q#fork#;$ml=[];$nl=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$ol=bless({$t,$ml,$v,$q,$w,$nl,$y,$z},$A);$pl=q#move_fds#;$ql=[];$rl=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$sl=bless({$t,$ql,$v,$q,$w,$rl,$y,$z},$A);$tl={$hl,$kl,$ll,$ol,$pl,$sl};$ul=q#/io/exec_fork.b#;$vl=bless({$r1,$gl,$W2,$q,$X2,$q,$Y2,$tl,$Q,$ul},$x2);$wl=[$J4,$Yj,$hk,$Bk,$Vk,$fl,$vl];$xl=bless({$r1,$Qj,$Q,$f6,$v1,$wl},$P1);$yl=q#ni:/io/exec.c#;$zl={$P1,1};$Al=q#/io/exec.c#;$Bl=[$wi];$Cl=bless({$r1,$zl,$Q,$Al,$v1,$Bl},$J2);$Dl=q#ni:/io/exec_env.b#;$El=q#ni:/io/exec_fork.b#;$Fl=q#ni:/io/exec_init.b#;$Gl=q#ni:/io/exec_io_accessors.b#;$Hl=q#ni:/io/exec_io_setup.b#;$Il=q#ni:/io/exec_ro.b#;$Jl=q#ni:/io/fd#;$Kl={$z1,1};$Ll=q#read_fd_mask#;$Ml={};$Nl=[];$Ol=q#shift->{'fd'}#;$Pl=bless({$t,$Nl,$v,$q,$w,$Ol,$y,$z},$A);$Ql={$h8,$Pl};$Rl=q#/io/fd_readers.b#;$Sl=bless({$r1,$Ml,$W2,$q,$X2,$q,$Y2,$Ql,$Q,$Rl},$x2);$Tl={};$Ul=[];$Vl=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Wl=bless({$t,$Ul,$v,$q,$w,$Vl,$y,$z},$A);$Xl={$L4,$Wl};$Yl=q#/io/fd_init.b#;$Zl=bless({$r1,$Tl,$W2,$q,$X2,$q,$Y2,$Xl,$Q,$Yl},$x2);$cm={};$dm=q#be#;$em=[];$fm=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$gm=bless({$t,$em,$v,$q,$w,$fm,$y,$z},$A);$hm={$dm,$gm};$im=q#/io/fd_shell.b#;$jm=bless({$r1,$cm,$W2,$q,$X2,$q,$Y2,$hm,$Q,$im},$x2);$km={};$lm=q#cloexec#;$mm=[];$nm=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$om=bless({$t,$mm,$v,$q,$w,$nm,$y,$z},$A);$pm=q#fcntl_flag#;$qm=[];$rm=q#my ($self, $flag, $value) = @_;
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
}#;$sm=bless({$t,$qm,$v,$q,$w,$rm,$y,$z},$A);$tm=q#nonblock#;$um=[];$vm=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$wm=bless({$t,$um,$v,$q,$w,$vm,$y,$z},$A);$xm={$lm,$om,$pm,$sm,$tm,$wm};$ym=q#/io/fd_fcntl.b#;$zm=bless({$r1,$km,$W2,$q,$X2,$q,$Y2,$xm,$Q,$ym},$x2);$Am={};$Bm=[];$Cm=q#shift->close#;$Dm=bless({$t,$Bm,$v,$q,$w,$Cm,$y,$z},$A);$Em=q#close#;$Fm=[];$Gm=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Hm=bless({$t,$Fm,$v,$q,$w,$Gm,$y,$z},$A);$Im={$Em,$Hm};$Jm=q#/io/fd_gc.b#;$Km=bless({$r1,$Am,$W2,$q,$X2,$Dm,$Y2,$Im,$Q,$Jm},$x2);$Lm={};$Mm=q#close_perl_ios#;$Nm=[];$Om=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Pm=bless({$t,$Nm,$v,$q,$w,$Om,$y,$z},$A);$Qm=[];$Rm=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Sm=bless({$t,$Qm,$v,$q,$w,$Rm,$y,$z},$A);$Tm=[];$Um=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Vm=bless({$t,$Tm,$v,$q,$w,$Um,$y,$z},$A);$Wm={$Mm,$Pm,$T7,$Sm,$X7,$Vm};$Xm=q#/io/fd_perlio.b#;$Ym=bless({$r1,$Lm,$W2,$q,$X2,$q,$Y2,$Wm,$Q,$Xm},$x2);$Zm=[$J4,$Sl,$Zl,$jm,$zm,$Km,$Ym];$cn=q#write_fd_mask#;$dn=bless({$r1,$Kl,$Q,$s6,$Ll,$z,$v1,$Zm,$cn,$z},$Q1);$en=[];$fn=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$gn=bless({$t,$en,$v,$q,$w,$fn,$y,$z},$A);$hn=q#ni:/io/fd.c#;$in={$Q1,1};$jn=q#/io/fd.c#;$kn={};$ln=q#clear_fd#;$mn=[];$nn=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$on=bless({$t,$mn,$v,$q,$w,$nn,$y,$z},$A);$pn=q#read_fd#;$qn=[];$rn=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$sn=bless({$t,$qn,$v,$q,$w,$rn,$y,$z},$A);$tn=q#select#;$un=[];$vn=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$wn=bless({$t,$un,$v,$q,$w,$vn,$y,$z},$A);$xn=q#write_fd#;$yn=[];$zn=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$An=bless({$t,$yn,$v,$q,$w,$zn,$y,$z},$A);$Bn={$ln,$on,$pn,$sn,$tn,$wn,$xn,$An};$Cn=q#/io/fd.c_selector.b#;$Dn=bless({$r1,$kn,$W2,$gn,$X2,$q,$Y2,$Bn,$Q,$Cn},$x2);$En=[$wi,$Dn];$Fn=bless({$r1,$in,$Q,$jn,$v1,$En},$J2);$Gn=q#ni:/io/fd.c_selector.b#;$Hn=q#ni:/io/fd_fcntl.b#;$In=q#ni:/io/fd_gc.b#;$Jn=q#ni:/io/fd_init.b#;$Kn=q#ni:/io/fd_perlio.b#;$Ln=q#ni:/io/fd_readers.b#;$Mn=q#ni:/io/fd_shell.b#;$Nn=q#ni:/io/file#;$On={$A1,1};$Pn={};$Qn=[];$Rn=q#shift->{'name'}#;$Sn=bless({$t,$Qn,$v,$q,$w,$Rn,$y,$z},$A);$Tn={$Q,$Sn};$Un=q#/io/file_readers.b#;$Vn=bless({$r1,$Pn,$W2,$q,$X2,$q,$Y2,$Tn,$Q,$Un},$x2);$Wn={};$Xn=q#mode#;$Yn=[];$Zn=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$co=bless({$t,$Yn,$v,$q,$w,$Zn,$y,$z},$A);$do={$Xn,$co};$eo=q#/io/file_accessors.b#;$fo=bless({$r1,$Wn,$W2,$q,$X2,$q,$Y2,$do,$Q,$eo},$x2);$go={};$ho=[];$io=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$jo=bless({$t,$ho,$v,$q,$w,$io,$y,$z},$A);$ko={$L4,$jo};$lo=q#/io/file_init.b#;$mo=bless({$r1,$go,$W2,$q,$X2,$q,$Y2,$ko,$Q,$lo},$x2);$no={};$oo=q#(-X#;$po=[];$qo=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$ro=bless({$t,$po,$v,$q,$w,$qo,$y,$z},$A);$so=q#mv#;$to=[];$uo=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$vo=bless({$t,$to,$v,$q,$w,$uo,$y,$z},$A);$wo=q#rm#;$xo=[];$yo=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$zo=bless({$t,$xo,$v,$q,$w,$yo,$y,$z},$A);$Ao={$oo,$ro,$so,$vo,$wo,$zo};$Bo=q#/io/file_fns.b#;$Co=bless({$r1,$no,$W2,$q,$X2,$q,$Y2,$Ao,$Q,$Bo},$x2);$Do={};$Eo=q#atomic_update#;$Fo=[];$Go=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Ho=bless({$t,$Fo,$v,$q,$w,$Go,$y,$z},$A);$Io={$Eo,$Ho};$Jo=q#/io/file_update.b#;$Ko=bless({$r1,$Do,$W2,$q,$X2,$q,$Y2,$Io,$Q,$Jo},$x2);$Lo={};$Mo=[];$No=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Oo=bless({$t,$Mo,$v,$q,$w,$No,$y,$z},$A);$Po=q#r#;$Qo=[];$Ro=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$So=bless({$t,$Qo,$v,$q,$w,$Ro,$y,$z},$A);$To=[];$Uo=q#shift->r->read(@_)#;$Vo=bless({$t,$To,$v,$q,$w,$Uo,$y,$z},$A);$Wo=q#w#;$Xo=[];$Yo=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Zo=bless({$t,$Xo,$v,$q,$w,$Yo,$y,$z},$A);$cp=[];$dp=q#shift->w->write(@_)#;$ep=bless({$t,$cp,$v,$q,$w,$dp,$y,$z},$A);$fp={$Em,$Oo,$Po,$So,$T7,$Vo,$Wo,$Zo,$X7,$ep};$gp=q#/io/file_io.b#;$hp=bless({$r1,$Lo,$W2,$q,$X2,$q,$Y2,$fp,$Q,$gp},$x2);$ip=[$J4,$Vn,$fo,$mo,$Co,$Ko,$hp];$jp=bless({$r1,$On,$Q,$O6,$v1,$ip},$R1);$kp=q#ni:/io/file.c#;$lp={$R1,1};$mp=q#/io/file.c#;$np=[$wi];$op=bless({$r1,$lp,$Q,$mp,$v1,$np},$J2);$pp=q#ni:/io/file_accessors.b#;$qp=q#ni:/io/file_fns.b#;$rp=q#ni:/io/file_init.b#;$sp=q#ni:/io/file_io.b#;$tp=q#ni:/io/file_readers.b#;$up=q#ni:/io/file_update.b#;$vp=q#ni:/io/file_update_fd#;$wp={$B1,1};$xp={};$yp=[];$zp=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Ap=bless({$t,$yp,$v,$q,$w,$zp,$y,$z},$A);$Bp={$L4,$Ap};$Cp=q#/io/file_update_fd_fd_init.b#;$Dp=bless({$r1,$xp,$W2,$q,$X2,$q,$Y2,$Bp,$Q,$Cp},$x2);$Ep={};$Fp=[];$Gp=bless({$t,$Fp,$v,$q,$w,$Cm,$y,$z},$A);$Hp=[];$Ip=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Jp=bless({$t,$Hp,$v,$q,$w,$Ip,$y,$z},$A);$Kp={$Em,$Jp};$Lp=q#/io/file_update_fd_fd_gc.b#;$Mp=bless({$r1,$Ep,$W2,$q,$X2,$Gp,$Y2,$Kp,$Q,$Lp},$x2);$Np=[$J4,$Sl,$zm,$Ym,$Dp,$Mp];$Op=bless({$r1,$wp,$Q,$U6,$v1,$Np},$S1);$Pp=q#ni:/io/file_update_fd.c#;$Qp={$S1,1};$Rp=q#/io/file_update_fd.c#;$Sp=[$wi];$Tp=bless({$r1,$Qp,$Q,$Rp,$v1,$Sp},$J2);$Up=q#ni:/io/file_update_fd_fd_gc.b#;$Vp=q#ni:/io/file_update_fd_fd_init.b#;$Wp=q#ni:/io/named_io_fns.b#;$Xp={};$Yp=q#fcntl#;$Zp=[];$cq=q#CORE::fcntl $_[0], $_[1], $_[2]#;$dq=bless({$t,$Zp,$v,$q,$w,$cq,$y,$z},$A);$eq=[];$fq=q#CORE::fork#;$gq=bless({$t,$eq,$v,$q,$w,$fq,$y,$z},$A);$hq=q#open2#;$iq=[];$jq=q#CORE::open $_[0], $_[1]#;$kq=bless({$t,$iq,$v,$q,$w,$jq,$y,$z},$A);$lq=q#rename#;$mq=[];$nq=q#CORE::rename $_[0], $_[1]#;$oq=bless({$t,$mq,$v,$q,$w,$nq,$y,$z},$A);$pq=q#unlink#;$qq=[];$rq=q#CORE::unlink @_#;$sq=bless({$t,$qq,$v,$q,$w,$rq,$y,$z},$A);$tq=q#waitpid#;$uq=[];$vq=q#CORE::waitpid $_[0], $_[1]#;$wq=bless({$t,$uq,$v,$q,$w,$vq,$y,$z},$A);$xq={$Yp,$dq,$ll,$gq,$hq,$kq,$lq,$oq,$pq,$sq,$tq,$wq};$yq=q#/io/named_io_fns.b#;$zq=bless({$r1,$Xp,$W2,$q,$X2,$q,$Y2,$xq,$Q,$yq},$x2);$Aq=q#main#;$Bq=q#ni:/io/null#;$Cq={$C1,1};$Dq=q#/io/null#;$Eq={};$Fq=[];$Gq=q#+{fd => undef}#;$Hq=bless({$t,$Fq,$v,$q,$w,$Gq,$y,$z},$A);$Iq={$L4,$Hq};$Jq=q#/io/null_init.b#;$Kq=bless({$r1,$Eq,$W2,$q,$X2,$q,$Y2,$Iq,$Q,$Jq},$x2);$Lq={};$Mq=[];$Nq=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Oq=bless({$t,$Mq,$v,$q,$w,$Nq,$y,$z},$A);$Pq=[];$Qq=q#shift->fd->read(@_)#;$Rq=bless({$t,$Pq,$v,$q,$w,$Qq,$y,$z},$A);$Sq=[];$Tq=q#shift->fd->write(@_)#;$Uq=bless({$t,$Sq,$v,$q,$w,$Tq,$y,$z},$A);$Vq={$h8,$Oq,$T7,$Rq,$X7,$Uq};$Wq=q#/io/null_io.b#;$Xq=bless({$r1,$Lq,$W2,$q,$X2,$q,$Y2,$Vq,$Q,$Wq},$x2);$Yq=[$J4,$Kq,$Xq];$Zq=bless({$r1,$Cq,$Q,$Dq,$v1,$Yq},$T1);$cr=q#ni:/io/null.c#;$dr={$T1,1};$er=q#/io/null.c#;$fr=[$wi];$gr=bless({$r1,$dr,$Q,$er,$v1,$fr},$J2);$hr=q#ni:/io/null_init.b#;$ir=q#ni:/io/null_io.b#;$jr=q#ni:/io/object#;$kr=q#ni:/io/object.c#;$lr=q#ni:/io/object.c_transfer_def.b#;$mr=q#ni:/io/object_checks.b#;$nr=q#ni:/io/object_constructors.b#;$or=q#ni:/io/object_memory.b#;$pr=q#ni:/io/object_ops.b#;$qr=q#ni:/io/object_transfer_async.b#;$rr=q#ni:/io/object_transfer_sync.b#;$sr=q#ni:/io/pid#;$tr=q#ni:/io/pid.c#;$ur={$V1,1};$vr=q#/io/pid.c#;$wr=[$wi];$xr=bless({$r1,$ur,$Q,$vr,$v1,$wr},$J2);$yr=q#ni:/io/pid_accessors.b#;$zr=q#ni:/io/pid_init.b#;$Ar=q#ni:/io/pid_io.b#;$Br=q#ni:/io/pid_readers.b#;$Cr=q#ni:/io/pid_wait.b#;$Dr=q#ni:/io/str#;$Er={$F1,1};$Fr=q#/io/str#;$Gr={};$Hr=q#data#;$Ir=[];$Jr=q#shift->{'data'}#;$Kr=bless({$t,$Ir,$v,$q,$w,$Jr,$y,$z},$A);$Lr=q#end#;$Mr=[];$Nr=q#shift->{'end'}#;$Or=bless({$t,$Mr,$v,$q,$w,$Nr,$y,$z},$A);$Pr=q#start#;$Qr=[];$Rr=q#shift->{'start'}#;$Sr=bless({$t,$Qr,$v,$q,$w,$Rr,$y,$z},$A);$Tr={$Hr,$Kr,$Lr,$Or,$Pr,$Sr};$Ur=q#/io/str_ro.b#;$Vr=bless({$r1,$Gr,$W2,$q,$X2,$q,$Y2,$Tr,$Q,$Ur},$x2);$Wr={};$Xr=[];$Yr=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Zr=bless({$t,$Xr,$v,$q,$w,$Yr,$y,$z},$A);$cs={$L4,$Zr};$ds=q#/io/str_init.b#;$es=bless({$r1,$Wr,$W2,$q,$X2,$q,$Y2,$cs,$Q,$ds},$x2);$fs={};$gs=[];$hs=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$is=bless({$t,$gs,$v,$q,$w,$hs,$y,$z},$A);$js=q#remaining#;$ks=[];$ls=q#my $self = shift; $$self{end} - $$self{start}#;$ms=bless({$t,$ks,$v,$q,$w,$ls,$y,$z},$A);$ns=[];$os=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$ps=bless({$t,$ns,$v,$q,$w,$os,$y,$z},$A);$qs={$T7,$is,$js,$ms,$X7,$ps};$rs=q#/io/str_io.b#;$ss=bless({$r1,$fs,$W2,$q,$X2,$q,$Y2,$qs,$Q,$rs},$x2);$ts=[$J4,$Vr,$es,$ss];$us=bless({$r1,$Er,$Q,$Fr,$v1,$ts},$W1);$vs=q#ni:/io/str.c#;$ws={$W1,1};$xs=q#/io/str.c#;$ys=[$wi];$zs=bless({$r1,$ws,$Q,$xs,$v1,$ys},$J2);$As=q#ni:/io/str_init.b#;$Bs=q#ni:/io/str_io.b#;$Cs=q#ni:/io/str_ro.b#;$Ds=q#ni:/io/transfer#;$Es={$X1,1,$Z1,1,$d2,1};$Fs=q#/io/transfer#;$Gs={$X1,1,$Z1,1,$d2,1,$R2,1};$Hs=q#/semantic/task#;$Is={};$Js=[];$Ks=q#shift->{'outcome'}#;$Ls=bless({$t,$Js,$v,$q,$w,$Ks,$y,$z},$A);$Ms={$r,$Ls};$Ns=q#/semantic/task_ro.b#;$Os=bless({$r1,$Is,$W2,$q,$X2,$q,$Y2,$Ms,$Q,$Ns},$x2);$Ps={};$Qs=q#failure#;$Rs=[];$Ss=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Ts=bless({$t,$Rs,$v,$q,$w,$Ss,$y,$z},$A);$Us=q#success#;$Vs=[];$Ws=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Xs=bless({$t,$Vs,$v,$q,$w,$Ws,$y,$z},$A);$Ys={$Qs,$Ts,$Us,$Xs};$Zs=q#/semantic/task_outcome.b#;$ct=bless({$r1,$Ps,$W2,$q,$X2,$q,$Y2,$Ys,$Q,$Zs},$x2);$dt=[$k3,$Os,$ct];$et=bless({$r1,$Gs,$Q,$Hs,$v1,$dt},$S2);$ft={};$gt=[];$ht=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$it=bless({$t,$gt,$v,$q,$w,$ht,$y,$z},$A);$jt=[];$kt=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$lt=bless({$t,$jt,$v,$q,$w,$kt,$y,$z},$A);$mt=[];$nt=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$ot=bless({$t,$mt,$v,$q,$w,$nt,$y,$z},$A);$pt={$T7,$lt,$X7,$ot};$qt=q#/io/transfer_io_interop.b#;$rt=bless({$r1,$ft,$W2,$it,$X2,$q,$Y2,$pt,$Q,$qt},$x2);$st={};$tt=q#pressure#;$ut=[];$vt=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$wt=bless({$t,$ut,$v,$q,$w,$vt,$y,$z},$A);$xt=q#read_limit_throughput#;$yt=[];$zt=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$At=bless({$t,$yt,$v,$q,$w,$zt,$y,$z},$A);$Bt=q#throughput#;$Ct=[];$Dt=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$Et=bless({$t,$Ct,$v,$q,$w,$Dt,$y,$z},$A);$Ft=q#write_limit_throughput#;$Gt=[];$Ht=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$It=bless({$t,$Gt,$v,$q,$w,$Ht,$y,$z},$A);$Jt={$tt,$wt,$xt,$At,$Bt,$Et,$Ft,$It};$Kt=q#/io/transfer_io_measurement.b#;$Lt=bless({$r1,$st,$W2,$q,$X2,$q,$Y2,$Jt,$Q,$Kt},$x2);$Mt=[$et,$rt,$Lt];$Nt=bless({$r1,$Es,$Q,$Fs,$v1,$Mt},$Y1);$Ot=[];$Pt=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Qt=bless({$t,$Ot,$v,$q,$w,$Pt,$y,$z},$A);$Rt=q#ni:/io/transfer.c#;$St={$Y1,1,$c2,1,$e2,1};$Tt=q#/io/transfer.c#;$Ut={$Y1,1,$c2,1,$e2,1,$S2,1};$Vt=q#/semantic/task.c#;$Wt=[$rh];$Xt=bless({$r1,$Ut,$Q,$Vt,$v1,$Wt},$J2);$Yt={};$Zt={};$cu=q#/io/transfer.c_into.b#;$du=bless({$r1,$Yt,$W2,$Qt,$X2,$q,$Y2,$Zt,$Q,$cu},$x2);$eu=[$Xt,$du];$fu=bless({$r1,$St,$Q,$Tt,$v1,$eu},$J2);$gu=q#ni:/io/transfer.c_into.b#;$hu=q#ni:/io/transfer_async#;$iu={$Z1,1};$ju=q#/io/transfer_async#;$ku={};$lu=q#dest_io#;$mu=[];$nu=q#shift->{'dest_io'}#;$ou=bless({$t,$mu,$v,$q,$w,$nu,$y,$z},$A);$pu=q#id#;$qu=[];$ru=q#shift->{'id'}#;$su=bless({$t,$qu,$v,$q,$w,$ru,$y,$z},$A);$tu=q#source_io#;$uu=[];$vu=q#shift->{'source_io'}#;$wu=bless({$t,$uu,$v,$q,$w,$vu,$y,$z},$A);$xu={$lu,$ou,$pu,$su,$tu,$wu};$yu=q#/io/transfer_async_ro.b#;$zu=bless({$r1,$ku,$W2,$q,$X2,$q,$Y2,$xu,$Q,$yu},$x2);$Au={};$Bu=[];$Cu=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Du=bless({$t,$Bu,$v,$q,$w,$Cu,$y,$z},$A);$Eu={$L4,$Du};$Fu=q#/io/transfer_async_init.b#;$Gu=bless({$r1,$Au,$W2,$q,$X2,$q,$Y2,$Eu,$Q,$Fu},$x2);$Hu={};$Iu=[];$Ju=q#ni('ni:/io/transfer_async')->track(shift)#;$Ku=bless({$t,$Iu,$v,$q,$w,$Ju,$y,$z},$A);$Lu=[];$Mu=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Nu=bless({$t,$Lu,$v,$q,$w,$Mu,$y,$z},$A);$Ou={};$Pu=q#/io/transfer_async_lifecycle.b#;$Qu=bless({$r1,$Hu,$W2,$Ku,$X2,$Nu,$Y2,$Ou,$Q,$Pu},$x2);$Ru={};$Su=q#run#;$Tu=[];$Uu=q#shift#;$Vu=bless({$t,$Tu,$v,$q,$w,$Uu,$y,$z},$A);$Wu=q#run_async#;$Xu=[];$Yu=q#my $self = shift;
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

$self;#;$Zu=bless({$t,$Xu,$v,$q,$w,$Yu,$y,$z},$A);$cv={$Su,$Vu,$Wu,$Zu};$dv=q#/io/transfer_async_run.b#;$ev=bless({$r1,$Ru,$W2,$q,$X2,$q,$Y2,$cv,$Q,$dv},$x2);$fv=[$Nt,$zu,$Gu,$Qu,$ev];$gv=q#tracked_transfers#;$hv={};$iv=q#transfer_id#;$jv=bless({$r1,$iu,$Q,$ju,$v1,$fv,$gv,$hv,$iv,0},$c2);$kv=[];$lv=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$mv=bless({$t,$kv,$v,$q,$w,$lv,$y,$z},$A);$nv=q#ni:/io/transfer_async.c#;$ov={$c2,1};$pv=q#/io/transfer_async.c#;$qv={};$rv=q#new_id#;$sv=[];$tv=q#++shift->{transfer_id}#;$uv=bless({$t,$sv,$v,$q,$w,$tv,$y,$z},$A);$vv=q#track#;$wv=[];$xv=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$yv=bless({$t,$wv,$v,$q,$w,$xv,$y,$z},$A);$zv=q#untrack#;$Av=[];$Bv=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Cv=bless({$t,$Av,$v,$q,$w,$Bv,$y,$z},$A);$Dv={$rv,$uv,$vv,$yv,$zv,$Cv};$Ev=q#/io/transfer_async.c_tracker.b#;$Fv=bless({$r1,$qv,$W2,$mv,$X2,$q,$Y2,$Dv,$Q,$Ev},$x2);$Gv=[$fu,$Fv];$Hv=bless({$r1,$ov,$Q,$pv,$v1,$Gv},$J2);$Iv=q#ni:/io/transfer_async.c_tracker.b#;$Jv=q#ni:/io/transfer_async_init.b#;$Kv=q#ni:/io/transfer_async_lifecycle.b#;$Lv=q#ni:/io/transfer_async_ro.b#;$Mv=q#ni:/io/transfer_async_run.b#;$Nv=q#ni:/io/transfer_io_interop.b#;$Ov=q#ni:/io/transfer_io_measurement.b#;$Pv=q#ni:/io/transfer_sync#;$Qv={$d2,1};$Rv=q#/io/transfer_sync#;$Sv={};$Tv=[];$Uv=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Vv=bless({$t,$Tv,$v,$q,$w,$Uv,$y,$z},$A);$Wv={$L4,$Vv};$Xv=q#/io/transfer_sync_init.b#;$Yv=bless({$r1,$Sv,$W2,$q,$X2,$q,$Y2,$Wv,$Q,$Xv},$x2);$Zv={};$cw=[];$dw=q#my $self = shift;
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
$self->success;#;$ew=bless({$t,$cw,$v,$q,$w,$dw,$y,$z},$A);$fw={$Su,$ew};$gw=q#/io/transfer_sync_run.b#;$hw=bless({$r1,$Zv,$W2,$q,$X2,$q,$Y2,$fw,$Q,$gw},$x2);$iw=[$Nt,$Yv,$hw];$jw=bless({$r1,$Qv,$Q,$Rv,$v1,$iw},$e2);$kw=q#ni:/io/transfer_sync.c#;$lw={$e2,1};$mw=q#/io/transfer_sync.c#;$nw=[$fu];$ow=bless({$r1,$lw,$Q,$mw,$v1,$nw},$J2);$pw=q#ni:/io/transfer_sync_init.b#;$qw=q#ni:/io/transfer_sync_run.b#;$rw=q#ni:/lib#;$sw=q#lib#;$tw={$sw,1};$uw=[];$vw=bless({$r1,$tw,$Q,$c9,$v1,$uw},$L2);$ww=q#ni:/lib/accessor.b#;$xw=q#ni:/lib/behavior#;$yw=q#ni:/lib/behavior.c#;$zw=q#ni:/lib/branch#;$Aw={$h2,1};$Bw=q#/lib/branch#;$Cw={};$Dw=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Ew=bless({$w,$Dw,$y,$z},$A);$Fw={$L4,$Ew};$Gw=q#/lib/branch_init.b#;$Hw=bless({$r1,$Cw,$W2,$q,$X2,$q,$Y2,$Fw,$Q,$Gw},$x2);$Iw=[$q9,$w9,$Hf,$Hw,$Og];$Jw=bless({$r1,$Aw,$Q,$Bw,$v1,$Iw},$i2);$Kw=q#ni:/lib/branch.b#;$Lw=q#ni:/lib/branch.c#;$Mw={$i2,1};$Nw=q#/lib/branch.c#;$Ow=[$vh];$Pw=bless({$r1,$Mw,$Q,$Nw,$v1,$Ow},$J2);$Qw=q#ni:/lib/branch_init.b#;$Rw=q#ni:/lib/class_init.b#;$Sw=q#ni:/lib/dataslice#;$Tw=q#ni:/lib/dataslice.b#;$Uw=q#ni:/lib/dataslice.c#;$Vw={$k2,1};$Ww=q#/lib/dataslice.c#;$Xw=[$vh];$Yw=bless({$r1,$Vw,$Q,$Ww,$v1,$Xw},$J2);$Zw=q#ni:/lib/dataslice_init.b#;$cx=q#ni:/lib/definition.b#;$dx=q#ni:/lib/definition_def.b#;$ex=q#ni:/lib/definition_defdata.b#;$fx=q#ni:/lib/definition_init_with_defaults.b#;$gx=q#ni:/lib/doc#;$hx={$S,1};$ix={};$jx=q#shift; +{name => shift, doc => []}#;$kx=bless({$w,$jx,$y,$z},$A);$lx={$L4,$kx};$mx=q#/lib/doc_init.b#;$nx=bless({$r1,$ix,$W2,$q,$X2,$q,$Y2,$lx,$Q,$mx},$x2);$ox={};$px=q#'ni.doc'#;$qx=bless({$w,$px,$y,$z},$A);$rx={$O9,$qx};$sx=q#/lib/doc_namespace.b#;$tx=bless({$r1,$ox,$W2,$q,$X2,$q,$Y2,$rx,$Q,$sx},$x2);$ux={};$vx=q#(@{}#;$wx=q#[map @$_, @{shift->{doc}}]#;$xx=bless({$w,$wx,$y,$z},$A);$yx=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$zx=bless({$w,$yx,$y,$z},$A);$Ax={$vx,$xx,$Ph,$zx};$Bx=q#/lib/doc_define.b#;$Cx=bless({$r1,$ux,$W2,$q,$X2,$q,$Y2,$Ax,$Q,$Bx},$x2);$Dx={};$Ex=q#shift->referent#;$Fx=bless({$w,$Ex,$y,$z},$A);$Gx=q#ni 'ni:' . shift->{name}#;$Hx=bless({$w,$Gx,$y,$z},$A);$Ix={$Lr,$Fx,$q1,$Hx};$Jx=q#/lib/doc_end.b#;$Kx=bless({$r1,$Dx,$W2,$q,$X2,$q,$Y2,$Ix,$Q,$Jx},$x2);$Lx={};$Mx=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Nx=bless({$w,$Mx,$y,$z},$A);$Ox={$p1,$Nx};$Px=q#/lib/doc_TODO.b#;$Qx=bless({$r1,$Lx,$W2,$q,$X2,$q,$Y2,$Ox,$Q,$Px},$x2);$Rx={};$Sx=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Tx=bless({$w,$Sx,$y,$z},$A);$Ux={$g7,$Tx};$Vx=q#/lib/doc_eg.b#;$Wx=bless({$r1,$Rx,$W2,$q,$X2,$q,$Y2,$Ux,$Q,$Vx},$x2);$Xx={};$Yx=q#tests#;$Zx=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case', @$self;#;$cy=bless({$w,$Zx,$y,$z},$A);$dy=q#todos#;$ey=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo', @$self;#;$fy=bless({$w,$ey,$y,$z},$A);$gy={$Yx,$cy,$dy,$fy};$hy=q#/lib/doc_process.b#;$iy=bless({$r1,$Xx,$W2,$q,$X2,$q,$Y2,$gy,$Q,$hy},$x2);$jy=[$k3,$w9,$nx,$tx,$Cx,$Kx,$Qx,$Wx,$iy];$ky=bless({$r1,$hx,$Q,$Sa,$v1,$jy},$l2);$ly=q#ni:/lib/doc.c#;$my={$l2,1};$ny=q#/lib/doc.c#;$oy={};$py=q#defannotation#;$qy=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$ry=bless({$w,$qy,$y,$z},$A);$sy={$py,$ry};$ty=q#/lib/doc.c_defannotation.b#;$uy=bless({$r1,$oy,$W2,$q,$X2,$q,$Y2,$sy,$Q,$ty},$x2);$vy=[$rh,$uy];$wy=bless({$r1,$my,$Q,$ny,$v1,$vy},$J2);$xy=q#ni:/lib/doc.c_defannotation.b#;$yy=q#ni:/lib/doc_TODO.b#;$zy=q#ni:/lib/doc_define.b#;$Ay=q#ni:/lib/doc_eg.b#;$By=q#ni:/lib/doc_end.b#;$Cy=q#ni:/lib/doc_init.b#;$Dy=q#ni:/lib/doc_namespace.b#;$Ey=q#ni:/lib/doc_process.b#;$Fy=q#ni:/lib/documentable.b#;$Gy=q#ni:/lib/fn#;$Hy={$A,1};$Iy=q#/lib/fn#;$Jy=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$Ky=bless({$w,$Jy,$y,$z},$A);$Ly=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$My=bless({$w,$Ly},$A);$Ny=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$Oy=bless({$w,$Ny},$A);$Py=q#lib/fn::closure#;$Qy=q#lib/fn::compile#;$Ry=q#lib/fn::instantiate#;$Sy={};$Ty=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Uy=bless({$w,$Ty,$y,$z},$A);$Vy=q#compile#;$Wy={$v,$Ky,$Vy,$My,$L4,$Oy};$Xy=q#/lib/fn_init.b#;$Yy=bless({$r1,$Sy,$W2,$q,$X2,$Uy,$Y2,$Wy,$Q,$Xy},$x2);$Zy={};$cz=[];$dz=q#shift->{'annotations'}#;$ez=bless({$t,$cz,$v,$q,$w,$dz,$y,$z},$A);$fz=[];$gz=q#shift->{'code'}#;$hz=bless({$t,$fz,$v,$q,$w,$gz,$y,$z},$A);$iz=q#eval_number#;$jz=[];$kz=q#shift->{'eval_number'}#;$lz=bless({$t,$jz,$v,$q,$w,$kz,$y,$z},$A);$mz=q#fn#;$nz=[];$oz=q#shift->{'fn'}#;$pz=bless({$t,$nz,$v,$q,$w,$oz,$y,$z},$A);$qz={$t,$ez,$w,$hz,$iz,$lz,$mz,$pz};$rz=q#/lib/fn_ro.b#;$sz=bless({$r1,$Zy,$W2,$q,$X2,$q,$Y2,$qz,$Q,$rz},$x2);$tz={};$uz=[];$vz=q#my $self = shift; "fn {$$self{code}}"#;$wz=bless({$t,$uz,$v,$q,$w,$vz,$y,$z},$A);$xz=[];$yz=bless({$t,$xz,$v,$q,$w,$ug,$y,$z},$A);$zz={$mg,$wz,$tg,$yz};$Az=q#/lib/fn_ops.b#;$Bz=bless({$r1,$tz,$W2,$q,$X2,$q,$Y2,$zz,$Q,$Az},$x2);$Cz={};$Dz=[];$Ez=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Fz=bless({$t,$Dz,$v,$q,$w,$Ez,$y,$z},$A);$Gz={$la,$Fz};$Hz=q#/lib/fn_serialize.b#;$Iz=bless({$r1,$Cz,$W2,$q,$X2,$q,$Y2,$Gz,$Q,$Hz},$x2);$Jz=[$k3,$Xg,$Yy,$sz,$Bz,$Iz];$Kz=bless({$r1,$Hy,$Q,$Iy,$v1,$Jz},$m2);$Lz=[];$Mz=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Nz=bless({$t,$Lz,$v,$q,$w,$Mz,$y,$z},$A);$Oz=q#ni:/lib/fn.c#;$Pz={$m2,1};$Qz=q#/lib/fn.c#;$Rz={};$Sz=q#resolve_evals#;$Tz=[];$Uz=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Vz=bless({$t,$Tz,$v,$q,$w,$Uz,$y,$z},$A);$Wz={$Sz,$Vz};$Xz=q#/lib/fn.c_resolve_eval.b#;$Yz=bless({$r1,$Rz,$W2,$Nz,$X2,$q,$Y2,$Wz,$Q,$Xz},$x2);$Zz=[$rh,$Yz];$cA=bless({$r1,$Pz,$Q,$Qz,$v1,$Zz},$J2);$dA=q#ni:/lib/fn.c_resolve_eval.b#;$eA=q#ni:/lib/fn_init.b#;$fA=q#ni:/lib/fn_ops.b#;$gA=q#ni:/lib/fn_ro.b#;$hA=q#ni:/lib/fn_serialize.b#;$iA=q#ni:/lib/future#;$jA={$n2,1};$kA={};$lA=[];$mA=bless({$t,$lA,$v,$q,$w,$Ks,$y,$z},$A);$nA=q#parents#;$oA=[];$pA=q#shift->{'parents'}#;$qA=bless({$t,$oA,$v,$q,$w,$pA,$y,$z},$A);$rA={$r,$mA,$nA,$qA};$sA=q#/lib/future_ro.b#;$tA=bless({$r1,$kA,$W2,$q,$X2,$q,$Y2,$rA,$Q,$sA},$x2);$uA={};$vA=[];$wA=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$xA=bless({$t,$vA,$v,$q,$w,$wA,$y,$z},$A);$yA={$L4,$xA};$zA=q#/lib/future_init.b#;$AA=bless({$r1,$uA,$W2,$q,$X2,$q,$Y2,$yA,$Q,$zA},$x2);$BA={};$CA=q#decide#;$DA=[];$EA=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$FA=bless({$t,$DA,$v,$q,$w,$EA,$y,$z},$A);$GA=q#decided#;$HA=[];$IA=q#shift->{outcome}#;$JA=bless({$t,$HA,$v,$q,$w,$IA,$y,$z},$A);$KA=q#listener#;$LA=[];$MA=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$NA=bless({$t,$LA,$v,$q,$w,$MA,$y,$z},$A);$OA=q#v#;$PA=[];$QA=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$RA=bless({$t,$PA,$v,$q,$w,$QA,$y,$z},$A);$SA={$CA,$FA,$GA,$JA,$KA,$NA,$OA,$RA};$TA=q#/lib/future_state.b#;$UA=bless({$r1,$BA,$W2,$q,$X2,$q,$Y2,$SA,$Q,$TA},$x2);$VA={};$WA=q#and#;$XA=[];$YA=q#my $self   = $_[0];
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
$child;#;$ZA=bless({$t,$XA,$v,$q,$w,$YA,$y,$z},$A);$cB=q#flatmap#;$dB=[];$eB=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$fB=bless({$t,$dB,$v,$q,$w,$eB,$y,$z},$A);$gB=q#map#;$hB=[];$iB=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$jB=bless({$t,$hB,$v,$q,$w,$iB,$y,$z},$A);$kB=q#or#;$lB=[];$mB=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$nB=bless({$t,$lB,$v,$q,$w,$mB,$y,$z},$A);$oB={$WA,$ZA,$cB,$fB,$gB,$jB,$kB,$nB};$pB=q#/lib/future_algebra.b#;$qB=bless({$r1,$VA,$W2,$q,$X2,$q,$Y2,$oB,$Q,$pB},$x2);$rB=[$k3,$tA,$AA,$UA,$qB];$sB=bless({$r1,$jA,$Q,$lb,$v1,$rB},$o2);$tB=q#ni:/lib/future.c#;$uB={$o2,1};$vB=q#/lib/future.c#;$wB=[$rh];$xB=bless({$r1,$uB,$Q,$vB,$v1,$wB},$J2);$yB=q#ni:/lib/future_algebra.b#;$zB=q#ni:/lib/future_init.b#;$AB=q#ni:/lib/future_ro.b#;$BB=q#ni:/lib/future_state.b#;$CB=q#ni:/lib/gensym_generator_compact.b#;$DB=q#ni:/lib/global_static_test.b#;$EB={};$FB=[];$GB=q#ni('ni:/lib/test_case')->new(shift)#;$HB=q#($)#;$IB=bless({$t,$FB,$v,$q,$w,$GB,$y,$HB},$A);$JB=q#now#;$KB=[];$LB=q#ni('ni:/lib/test_value')->new(shift)#;$MB=bless({$t,$KB,$v,$q,$w,$LB,$y,$HB},$A);$NB={$g7,$IB,$JB,$MB};$OB=q#/lib/global_static_test.b#;$PB=bless({$r1,$EB,$W2,$q,$X2,$q,$Y2,$NB,$Q,$OB},$x2);$QB=q#ni:/lib/image#;$RB=q#ni:/lib/image.c#;$SB={$q2,1};$TB=q#/lib/image.c#;$UB=[$rh];$VB=bless({$r1,$SB,$Q,$TB,$v1,$UB},$J2);$WB=q#ni:/lib/image_init.b#;$XB=q#ni:/lib/image_quoting.b#;$YB=q#ni:/lib/instance.b#;$ZB=q#ni:/lib/instantiable.b#;$cC=q#ni:/lib/json.b#;$dC={};$eC=q#json_decode#;$fC=[];$gC=q#local $_;
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
wantarray ? @$r : $$r[0];#;$hC=bless({$t,$fC,$v,$q,$w,$gC,$y,$HB},$A);$iC=q#json_encode#;$jC=[];$kC=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$lC=bless({$t,$jC,$v,$q,$w,$kC,$y,$HB},$A);$mC=q#json_encode_pretty#;$nC=[];$oC=q#local $_;
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

ni::json_encode($v);#;$pC=bless({$t,$nC,$v,$q,$w,$oC,$y,$z},$A);$qC=q#json_escape#;$rC=[];$sC=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$tC=bless({$t,$rC,$v,$q,$w,$sC,$y,$HB},$A);$uC=q#json_unescape#;$vC=[];$wC=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$xC=bless({$t,$vC,$v,$q,$w,$wC,$y,$HB},$A);$yC=q#json_unescape_one#;$zC=[];$AC=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$BC=bless({$t,$zC,$v,$q,$w,$AC,$y,$HB},$A);$CC={$eC,$hC,$iC,$lC,$mC,$pC,$qC,$tC,$uC,$xC,$yC,$BC};$DC=q#/lib/json.b#;$EC=bless({$r1,$dC,$W2,$q,$X2,$q,$Y2,$CC,$Q,$DC},$x2);$FC=q#ni#;$GC=q#ni:/lib/json_data.b#;$HC={};$IC=q#json_escapes#;$JC=q##;$KC=q#b#;$LC=q#	#;$MC=q#t#;$NC=q#
#;$OC=q#n#;$PC=q##;$QC=q#f#;$RC=q##;$SC=q#"#;$TC=q#/#;$UC=q#\\#;$VC={$JC,$KC,$LC,$MC,$NC,$OC,$PC,$QC,$RC,$Po,$SC,$SC,$TC,$TC,$UC,$UC};$WC=q#json_unescapes#;$XC={$SC,$SC,$TC,$TC,$UC,$UC,$KC,$JC,$QC,$PC,$OC,$NC,$Po,$RC,$MC,$LC};$YC={$IC,$VC,$WC,$XC};$ZC=q#/lib/json_data.b#;$cD=bless({$r1,$HC,$Hr,$YC,$Q,$ZC},$j2);$dD=q#ni:/lib/name_as_string.b#;$eD=q#ni:/lib/named.b#;$fD=q#ni:/lib/named_in_ni.b#;$gD=q#ni:/lib/namespaced.b#;$hD=q#ni:/lib/ni#;$iD={$r2,1};$jD={};$kD=q#extend#;$lD=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$mD=bless({$w,$lD,$y,$z},$A);$nD=q#is_mutable#;$oD=q#$0 ne '-' && -w $0#;$pD=bless({$w,$oD,$y,$z},$A);$qD=q#modify#;$rD=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$sD=bless({$w,$rD,$y,$z},$A);$tD={$kD,$mD,$nD,$pD,$qD,$sD};$uD=q#/lib/ni_self.b#;$vD=bless({$r1,$jD,$W2,$q,$X2,$q,$Y2,$tD,$Q,$uD},$x2);$wD={};$xD=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$yD=bless({$w,$xD,$y,$z},$A);$zD=q#docs#;$AD=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$BD=bless({$w,$AD,$y,$z},$A);$CD=q#metaclasses#;$DD=q#my $self = shift;
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$ED=bless({$w,$DD,$y,$z},$A);$FD=q#undocumented#;$GD=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$HD=bless({$w,$GD,$y,$z},$A);$ID=q#untested#;$JD=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$KD=bless({$w,$JD,$y,$z},$A);$LD={$K,$yD,$zD,$BD,$CD,$ED,$FD,$HD,$ID,$KD};$MD=q#/lib/ni_dev_introspection.b#;$ND=bless({$r1,$wD,$W2,$q,$X2,$q,$Y2,$LD,$Q,$MD},$x2);$OD={};$PD=q#--internal/+=#;$QD=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$RD=bless({$w,$QD,$y,$z},$A);$SD=q#--internal/dev-state#;$TD=q#my $self = shift;
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
0;#;$UD=bless({$w,$TD,$y,$z},$A);$VD=q#--internal/eval#;$WD=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$XD=bless({$w,$WD,$y,$z},$A);$YD=q#--internal/image#;$ZD=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$cE=bless({$w,$ZD,$y,$z},$A);$dE=q#--internal/test#;$eE=q#local $| = 1;
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
!!$failed;#;$fE=bless({$w,$eE,$y,$z},$A);$gE=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$hE=bless({$w,$gE,$y,$z},$A);$iE={$PD,$RD,$SD,$UD,$VD,$XD,$YD,$cE,$dE,$fE,$Su,$hE};$jE=q#/lib/ni_main.b#;$kE=bless({$r1,$OD,$W2,$q,$X2,$q,$Y2,$iE,$Q,$jE},$x2);$lE={};$mE=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$nE=bless({$w,$mE,$y,$z},$A);$oE=q#resolver_for#;$pE=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$qE=bless({$w,$pE,$y,$z},$A);$rE={$ea,$nE,$oE,$qE};$sE=q#/lib/ni_resolver.b#;$tE=bless({$r1,$lE,$W2,$q,$X2,$q,$Y2,$rE,$Q,$sE},$x2);$uE={};$vE=q#exists#;$wE=q#exists $_[0]->{named}{$_[1]}#;$xE=bless({$w,$wE,$y,$z},$A);$yE=q#quoted#;$zE=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$AE=bless({$w,$zE,$y,$z},$A);$BE={$vE,$xE,$yE,$AE};$CE=q#/lib/ni_image.b#;$DE=bless({$r1,$uE,$W2,$q,$X2,$q,$Y2,$BE,$Q,$CE},$x2);$EE=[$k3,$vD,$ND,$kE,$tE,$DE];$FE=bless({$r1,$iD,$Q,$he,$v1,$EE},$s2);$GE=q#ni:/lib/ni.c#;$HE={$s2,1};$IE=q#/lib/ni.c#;$JE=[$rh];$KE=bless({$r1,$HE,$Q,$IE,$v1,$JE},$J2);$LE=q#ni:/lib/ni_dev_introspection.b#;$ME=q#ni:/lib/ni_image.b#;$NE=q#ni:/lib/ni_main.b#;$OE=q#ni:/lib/ni_resolver.b#;$PE=q#ni:/lib/ni_self.b#;$QE=q#ni:/lib/ni_static_util.b#;$RE={};$SE=q#abbrev#;$TE=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$UE=bless({$w,$TE,$y,$z},$A);$VE=q#dor#;$WE=q#defined $_[0] ? $_[0] : $_[1]#;$XE=bless({$w,$WE,$y,$z},$A);$YE=q#indent#;$ZE=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$cF=bless({$w,$ZE,$y,$z},$A);$dF=q#max#;$eF=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$fF=bless({$w,$eF,$y,$z},$A);$gF=q#maxstr#;$hF=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$iF=bless({$w,$hF,$y,$z},$A);$jF=q#mean#;$kF=q#sum(@_) / (@_ || 1)#;$lF=bless({$w,$kF,$y,$z},$A);$mF=q#min#;$nF=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$oF=bless({$w,$nF,$y,$z},$A);$pF=q#minstr#;$qF=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$rF=bless({$w,$qF,$y,$z},$A);$sF=q#outdent#;$tF=q#my $x = shift;
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
join "\\n", @lines;#;$uF=bless({$w,$tF,$y,$z},$A);$vF=q#sgr#;$wF=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$xF=bless({$w,$wF,$y,$z},$A);$yF=q#sr#;$zF=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$AF=bless({$w,$zF,$y,$z},$A);$BF=q#sum#;$CF=q#local $_; my $x = 0; $x += $_ for @_; $x#;$DF=bless({$w,$CF,$y,$z},$A);$EF=q#swap#;$FF=q#@_[0, 1] = @_[1, 0]#;$GF=bless({$w,$FF,$y,$z},$A);$HF={$SE,$UE,$VE,$XE,$YE,$cF,$dF,$fF,$gF,$iF,$jF,$lF,$mF,$oF,$pF,$rF,$sF,$uF,$vF,$xF,$yF,$AF,$BF,$DF,$EF,$GF};$IF=q#/lib/ni_static_util.b#;$JF=bless({$r1,$RE,$W2,$q,$X2,$q,$Y2,$HF,$Q,$IF},$x2);$KF=q#ni:/lib/object_metadata#;$LF={$t2,1,$C,1,$H,1};$MF=q#/lib/object_metadata#;$NF={};$OF=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$PF=bless({$w,$OF,$y,$z},$A);$QF={$q1,$PF};$RF=q#/lib/object_metadata_rw.b#;$SF=bless({$r1,$NF,$W2,$q,$X2,$q,$Y2,$QF,$Q,$RF},$x2);$TF=[$k3,$SF];$UF=bless({$r1,$LF,$Q,$MF,$v1,$TF},$u2);$VF=q#ni:/lib/object_metadata.c#;$WF={$u2,1,$F2,1,$I2,1};$XF=q#/lib/object_metadata.c#;$YF=[$rh];$ZF=bless({$r1,$WF,$Q,$XF,$v1,$YF},$J2);$cG=q#ni:/lib/object_metadata_rw.b#;$dG=q#ni:/lib/perlbranch.b#;$eG=q#ni:/lib/quote_circular_addressed.b#;$fG=q#ni:/lib/quote_code_fail.b#;$gG=q#ni:/lib/quote_gensym_identity.b#;$hG=q#ni:/lib/quote_objects.b#;$iG=q#ni:/lib/quote_simple#;$jG={$v2,1};$kG={};$lG=[];$mG=q#+{}#;$nG=bless({$t,$lG,$v,$q,$w,$mG,$y,$z},$A);$oG={$L4,$nG};$pG=q#/lib/quote_simple_init.b#;$qG=bless({$r1,$kG,$W2,$q,$X2,$q,$Y2,$oG,$Q,$pG},$x2);$rG={};$sG=[];$tG=bless({$t,$sG,$v,$q,$w,0,$y,$z},$A);$uG=[];$vG=q#shift->quote_value(shift)#;$wG=bless({$t,$uG,$v,$q,$w,$vG,$y,$z},$A);$xG={$id,$tG,$Cd,$wG};$yG=q#/lib/quote_simple_quote.b#;$zG=bless({$r1,$rG,$W2,$q,$X2,$q,$Y2,$xG,$Q,$yG},$x2);$AG=[$k3,$qG,$zG,$ic,$Gc,$Wc];$BG=bless({$r1,$jG,$Q,$se,$v1,$AG},$w2);$CG=q#ni:/lib/quote_simple.c#;$DG={$w2,1};$EG=q#/lib/quote_simple.c#;$FG=[$rh];$GG=bless({$r1,$DG,$Q,$EG,$v1,$FG},$J2);$HG=q#ni:/lib/quote_simple_init.b#;$IG=q#ni:/lib/quote_simple_quote.b#;$JG=q#ni:/lib/quote_values.b#;$KG=q#ni:/lib/ref_eq.b#;$LG=q#ni:/lib/resolver.b#;$MG=q#ni:/lib/slice#;$NG=q#ni:/lib/slice.b#;$OG=q#ni:/lib/slice.c#;$PG={$y2,1};$QG=q#/lib/slice.c#;$RG=[$vh];$SG=bless({$r1,$PG,$Q,$QG,$v1,$RG},$J2);$TG=q#ni:/lib/slice_init.b#;$UG=q#ni:/lib/slice_serialize.b#;$VG=q#ni:/lib/static_fn.b#;$WG={};$XG=q#fc#;$YG=[];$ZG=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$cH=bless({$t,$YG,$v,$q,$w,$ZG,$y,$z},$A);$dH=q#fk#;$eH=[];$fH=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$gH=bless({$t,$eH,$v,$q,$w,$fH,$y,$HB},$A);$hH=[];$iH=q#ni('ni:/lib/fn')->new(@_)#;$jH=bless({$t,$hH,$v,$q,$w,$iH,$y,$HB},$A);$kH=q#fp#;$lH=[];$mH=q#($$)#;$nH=bless({$t,$lH,$v,$q,$w,$iH,$y,$mH},$A);$oH={$XG,$cH,$dH,$gH,$mz,$jH,$kH,$nH};$pH=q#/lib/static_fn.b#;$qH=bless({$r1,$WG,$W2,$q,$X2,$q,$Y2,$oH,$Q,$pH},$x2);$rH=q#ni:/lib/subclass.b#;$sH=q#ni:/lib/tag#;$tH={$z2,1};$uH=q#/lib/tag#;$vH={};$wH=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$xH=bless({$w,$wH,$y,$z},$A);$yH={$y9,$xH};$zH=q#/lib/tag.b#;$AH=bless({$r1,$vH,$W2,$q,$X2,$q,$Y2,$yH,$Q,$zH},$x2);$BH={};$CH=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$DH=bless({$w,$CH,$y,$z},$A);$EH={$L4,$DH};$FH=q#/lib/tag_init.b#;$GH=bless({$r1,$BH,$W2,$q,$X2,$q,$Y2,$EH,$Q,$FH},$x2);$HH=[$q9,$w9,$AH,$GH];$IH=bless({$r1,$tH,$Q,$uH,$v1,$HH},$A2);$JH=q#ni:/lib/tag.b#;$KH=q#ni:/lib/tag.c#;$LH={$A2,1};$MH=q#/lib/tag.c#;$NH=[$vh];$OH=bless({$r1,$LH,$Q,$MH,$v1,$NH},$J2);$PH=q#ni:/lib/tag_init.b#;$QH=q#ni:/lib/test_assert_eq#;$RH={$B2,1};$SH=q#/lib/test_assert_eq#;$TH={$B2,1,$D2,1};$UH=q#/lib/test_assertion#;$VH={};$WH=q#commit#;$XH=[];$YH=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$ZH=bless({$t,$XH,$v,$q,$w,$YH,$y,$z},$A);$cI={$WH,$ZH};$dI=q#/lib/test_assertion_commit.b#;$eI=bless({$r1,$VH,$W2,$q,$X2,$q,$Y2,$cI,$Q,$dI},$x2);$fI=[$k3,$eI];$gI=bless({$r1,$TH,$Q,$UH,$v1,$fI},$E2);$hI={};$iI=q#diff#;$jI=[];$kI=q#shift->{'diff'}#;$lI=bless({$t,$jI,$v,$q,$w,$kI,$y,$z},$A);$mI={$iI,$lI};$nI=q#/lib/test_assert_eq_ro.b#;$oI=bless({$r1,$hI,$W2,$q,$X2,$q,$Y2,$mI,$Q,$nI},$x2);$pI={};$qI=[];$rI=q#my ($class, $diff) = @_;
+{diff => $diff};#;$sI=bless({$t,$qI,$v,$q,$w,$rI,$y,$z},$A);$tI={$L4,$sI};$uI=q#/lib/test_assert_eq_init.b#;$vI=bless({$r1,$pI,$W2,$q,$X2,$q,$Y2,$tI,$Q,$uI},$x2);$wI={};$xI=[];$yI=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$zI=bless({$t,$xI,$v,$q,$w,$yI,$y,$z},$A);$AI=q#failed#;$BI=[];$CI=q#defined shift->{diff}#;$DI=bless({$t,$BI,$v,$q,$w,$CI,$y,$z},$A);$EI=q#result#;$FI=[];$GI=bless({$t,$FI,$v,$q,$w,$Uu,$y,$z},$A);$HI={$mg,$zI,$AI,$DI,$EI,$GI};$II=q#/lib/test_assert_eq_result.b#;$JI=bless({$r1,$wI,$W2,$q,$X2,$q,$Y2,$HI,$Q,$II},$x2);$KI=[$gI,$oI,$vI,$JI];$LI=bless({$r1,$RH,$Q,$SH,$v1,$KI},$C2);$MI=q#ni:/lib/test_assert_eq.c#;$NI={$C2,1};$OI=q#/lib/test_assert_eq.c#;$PI={$C2,1,$E2,1};$QI=q#/lib/test_assertion.c#;$RI=[$rh];$SI=bless({$r1,$PI,$Q,$QI,$v1,$RI},$J2);$TI=[$SI];$UI=bless({$r1,$NI,$Q,$OI,$v1,$TI},$J2);$VI=q#ni:/lib/test_assert_eq_init.b#;$WI=q#ni:/lib/test_assert_eq_result.b#;$XI=q#ni:/lib/test_assert_eq_ro.b#;$YI=q#ni:/lib/test_assertion#;$ZI=q#ni:/lib/test_assertion.c#;$cJ=q#ni:/lib/test_assertion_commit.b#;$dJ=q#ni:/lib/test_case#;$eJ={$C,1};$fJ=q#/lib/test_case#;$gJ=q#running_test#;$hJ={};$iJ=[];$jJ=q#shift->{'assertions'}#;$kJ=bless({$t,$iJ,$v,$q,$w,$jJ,$y,$z},$A);$lJ=[];$mJ=q#shift->{'test'}#;$nJ=bless({$t,$lJ,$v,$q,$w,$mJ,$y,$z},$A);$oJ={$n,$kJ,$s,$nJ};$pJ=q#/lib/test_case_ro.b#;$qJ=bless({$r1,$hJ,$W2,$q,$X2,$q,$Y2,$oJ,$Q,$pJ},$x2);$rJ={};$sJ=[];$tJ=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$uJ=bless({$t,$sJ,$v,$q,$w,$tJ,$y,$z},$A);$vJ={$p,$uJ};$wJ=q#/lib/test_case_rw.b#;$xJ=bless({$r1,$rJ,$W2,$q,$X2,$q,$Y2,$vJ,$Q,$wJ},$x2);$yJ={};$zJ=[];$AJ=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$BJ=bless({$t,$zJ,$v,$q,$w,$AJ,$y,$z},$A);$CJ={$L4,$BJ};$DJ=q#/lib/test_case_init.b#;$EJ=bless({$r1,$yJ,$W2,$q,$X2,$q,$Y2,$CJ,$Q,$DJ},$x2);$FJ={};$GJ=[];$HJ=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$IJ=bless({$t,$GJ,$v,$q,$w,$HJ,$y,$z},$A);$JJ=[];$KJ=q#!shift->{outcome}->[0]#;$LJ=bless({$t,$JJ,$v,$q,$w,$KJ,$y,$z},$A);$MJ={$mg,$IJ,$AI,$LJ};$NJ=q#/lib/test_case_metrics.b#;$OJ=bless({$r1,$FJ,$W2,$q,$X2,$q,$Y2,$MJ,$Q,$NJ},$x2);$PJ={};$QJ=q#done#;$RJ=[];$SJ=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$TJ=bless({$t,$RJ,$v,$q,$w,$SJ,$y,$z},$A);$UJ=[];$VJ=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$WJ=bless({$t,$UJ,$v,$q,$w,$VJ,$y,$z},$A);$XJ={$QJ,$TJ,$Su,$WJ};$YJ=q#/lib/test_case_run.b#;$ZJ=bless({$r1,$PJ,$W2,$q,$X2,$q,$Y2,$XJ,$Q,$YJ},$x2);$cK=[$UF,$qJ,$xJ,$EJ,$OJ,$ZJ];$dK=bless({$r1,$eJ,$Q,$fJ,$gJ,$q,$v1,$cK},$F2);$eK=[];$fK=q#shift->{running_test} = undef#;$gK=bless({$t,$eK,$v,$q,$w,$fK,$y,$z},$A);$hK=q#ni:/lib/test_case.c#;$iK={$F2,1};$jK=q#/lib/test_case.c#;$kK={};$lK=[];$mK=q#shift->{'running_test'}#;$nK=bless({$t,$lK,$v,$q,$w,$mK,$y,$z},$A);$oK={$gJ,$nK};$pK=q#/lib/test_case.c_test_ro.b#;$qK=bless({$r1,$kK,$W2,$q,$X2,$q,$Y2,$oK,$Q,$pK},$x2);$rK={};$sK=q#with_test#;$tK=[];$uK=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$vK=bless({$t,$tK,$v,$q,$w,$uK,$y,$z},$A);$wK={$sK,$vK};$xK=q#/lib/test_case.c_test.b#;$yK=bless({$r1,$rK,$W2,$gK,$X2,$q,$Y2,$wK,$Q,$xK},$x2);$zK=[$ZF,$qK,$yK];$AK=bless({$r1,$iK,$Q,$jK,$v1,$zK},$J2);$BK=q#ni:/lib/test_case.c_test.b#;$CK=q#ni:/lib/test_case.c_test_ro.b#;$DK=q#ni:/lib/test_case_init.b#;$EK=q#ni:/lib/test_case_metrics.b#;$FK=q#ni:/lib/test_case_ro.b#;$GK=q#ni:/lib/test_case_run.b#;$HK=q#ni:/lib/test_case_rw.b#;$IK=q#ni:/lib/test_value#;$JK={$G2,1};$KK=q#/lib/test_value#;$LK={};$MK=[];$NK=q#\\$_[1]#;$OK=bless({$t,$MK,$v,$q,$w,$NK,$y,$z},$A);$PK={$L4,$OK};$QK=q#/lib/test_value_init.b#;$RK=bless({$r1,$LK,$W2,$q,$X2,$q,$Y2,$PK,$Q,$QK},$x2);$SK={};$TK=q#(==#;$UK=[];$VK=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$WK=bless({$t,$UK,$v,$q,$w,$VK,$y,$z},$A);$XK=q#detailed_scalar_diff#;$YK=[];$ZK=q#local $_;
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
[@diff];#;$cL=bless({$t,$YK,$v,$q,$w,$ZK,$y,$z},$A);$dL=[];$eL=q#my ($self, $rhs) = @_;
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
return undef;#;$fL=bless({$t,$dL,$v,$q,$w,$eL,$y,$z},$A);$gL={$TK,$WK,$XK,$cL,$iI,$fL};$hL=q#/lib/test_value_eq.b#;$iL=bless({$r1,$SK,$W2,$q,$X2,$q,$Y2,$gL,$Q,$hL},$x2);$jL={};$kL=[];$lL=q#ni::json_encode ${$_[0]}#;$mL=bless({$t,$kL,$v,$q,$w,$lL,$y,$z},$A);$nL={$mg,$mL};$oL=q#/lib/test_value_str.b#;$pL=bless({$r1,$jL,$W2,$q,$X2,$q,$Y2,$nL,$Q,$oL},$x2);$qL=[$k3,$RK,$iL,$pL];$rL=bless({$r1,$JK,$Q,$KK,$v1,$qL},$H2);$sL=q#ni:/lib/test_value.c#;$tL={$H2,1};$uL=q#/lib/test_value.c#;$vL=[$rh];$wL=bless({$r1,$tL,$Q,$uL,$v1,$vL},$J2);$xL=q#ni:/lib/test_value_eq.b#;$yL=q#ni:/lib/test_value_init.b#;$zL=q#ni:/lib/test_value_str.b#;$AL=q#ni:/lib/todo#;$BL={$H,1};$CL=q#/lib/todo#;$DL={};$EL=q#shift->{'todo'}#;$FL=bless({$w,$EL,$y,$z},$A);$GL={$E,$FL};$HL=q#/lib/todo_ro.b#;$IL=bless({$r1,$DL,$W2,$q,$X2,$q,$Y2,$GL,$Q,$HL},$x2);$JL={};$KL=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$LL=bless({$w,$KL,$y,$z},$A);$ML={$L4,$LL};$NL=q#/lib/todo_init.b#;$OL=bless({$r1,$JL,$W2,$q,$X2,$q,$Y2,$ML,$Q,$NL},$x2);$PL={};$QL=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$RL=bless({$w,$QL,$y,$z},$A);$SL={$mg,$RL};$TL=q#/lib/todo_str.b#;$UL=bless({$r1,$PL,$W2,$q,$X2,$q,$Y2,$SL,$Q,$TL},$x2);$VL=[$UF,$IL,$OL,$UL];$WL=bless({$r1,$BL,$Q,$CL,$v1,$VL},$I2);$XL=q#ni:/lib/todo.c#;$YL={$I2,1};$ZL=q#/lib/todo.c#;$cM=[$ZF];$dM=bless({$r1,$YL,$Q,$ZL,$v1,$cM},$J2);$eM=q#ni:/lib/todo_ctor.b#;$fM={};$gM=q#ni('ni:/lib/todo')->new(@_)#;$hM=bless({$w,$gM,$y,$z},$A);$iM={$p1,$hM};$jM=q#/lib/todo_ctor.b#;$kM=bless({$r1,$fM,$W2,$q,$X2,$q,$Y2,$iM,$Q,$jM},$x2);$lM=q#ni:/lib/todo_init.b#;$mM=q#ni:/lib/todo_ro.b#;$nM=q#ni:/lib/todo_str.b#;$oM=q#ni:/metaclass#;$pM={$J2,1};$qM=q#/metaclass#;$rM=[$Jf,$Xg,$Pf,$Qg];$sM=bless({$r1,$pM,$Q,$qM,$v1,$rM},$K2);$tM=q#ni:/metaclass.c#;$uM={$K2,1};$vM=q#/metaclass.c#;$wM=[$ih];$xM=bless({$r1,$uM,$Q,$vM,$v1,$wM},$J2);$yM=q#ni:/module#;$zM=q#ni:/module.c#;$AM=q#ni:/object#;$BM=q#ni:/object.c#;$CM=q#ni:/semantic#;$DM=q#semantic#;$EM={$DM,1};$FM=[];$GM=bless({$r1,$EM,$Q,$sf,$v1,$FM},$L2);$HM=q#ni:/semantic/dimension#;$IM={$P2,1};$JM=q#/semantic/dimension#;$KM=[$ih];$LM=bless({$r1,$IM,$Q,$JM,$v1,$KM},$Q2);$MM=q#ni:/semantic/dimension.c#;$NM={$Q2,1};$OM=q#/semantic/dimension.c#;$PM=[$zh];$QM=bless({$r1,$NM,$Q,$OM,$v1,$PM},$J2);$RM=q#ni:/semantic/task#;$SM=q#ni:/semantic/task.c#;$TM=q#ni:/semantic/task_outcome.b#;$UM=q#ni:/semantic/task_ro.b#;$VM=q#ni:main#;$WM={$Aq,1};$XM=[$kM,$qH,$PB,$zq];$YM=bless({$r1,$WM,$Q,$Aq,$v1,$XM},$L2);$ZM=q#ni:ni#;$cN={$FC,1};$dN=[$JF,$cD,$EC];$eN=bless({$r1,$cN,$Q,$FC,$v1,$dN},$L2);$fN={$d,$T,$W,$e1,$f1,$k1,$l1,$l5,$m5,$r5,$s5,$E5,$F5,$R5,$S5,$g6,$h6,$t6,$u6,$P6,$Q6,$V6,$W6,$e7,$f7,$U8,$V8,$d9,$e9,$za,$Aa,$Ta,$Ua,$mb,$nb,$Yd,$Zd,$ie,$je,$te,$ue,$nf,$of,$tf,$uf,$ih,$jh,$zh,$Ah,$Eh,$Fh,$Xh,$Yh,$ei,$fi,$Nh,$gi,$Vh,$hi,$f5,$ii,$yi,$zi,$d5,$Ai,$Di,$Ei,$ij,$jj,$nj,$oj,$Mi,$pj,$gj,$qj,$Hj,$Ij,$Mj,$Nj,$yj,$Oj,$Fj,$Pj,$xl,$yl,$Cl,$Dl,$fl,$El,$vl,$Fl,$hk,$Gl,$Vk,$Hl,$Bk,$Il,$Yj,$Jl,$dn,$hn,$Fn,$Gn,$Dn,$Hn,$zm,$In,$Km,$Jn,$Zl,$Kn,$Ym,$Ln,$Sl,$Mn,$jm,$Nn,$jp,$kp,$op,$pp,$fo,$qp,$Co,$rp,$mo,$sp,$hp,$tp,$Vn,$up,$Ko,$vp,$Op,$Pp,$Tp,$Up,$Mp,$Vp,$Dp,$Wp,$zq,$Bq,$Zq,$cr,$gr,$hr,$Kq,$ir,$Xq,$jr,$J4,$kr,$wi,$lr,$ui,$mr,$N3,$nr,$V3,$or,$j4,$pr,$t3,$qr,$H4,$rr,$v4,$sr,$B8,$tr,$xr,$yr,$z8,$zr,$F7,$Ar,$f8,$Br,$v7,$Cr,$R7,$Dr,$us,$vs,$zs,$As,$es,$Bs,$ss,$Cs,$Vr,$Ds,$Nt,$Rt,$fu,$gu,$du,$hu,$jv,$nv,$Hv,$Iv,$Fv,$Jv,$Gu,$Kv,$Qu,$Lv,$zu,$Mv,$ev,$Nv,$rt,$Ov,$Lt,$Pv,$jw,$kw,$ow,$pw,$Yv,$qw,$hw,$rw,$vw,$ww,$kg,$xw,$q9,$yw,$vh,$zw,$Jw,$Kw,$Hf,$Lw,$Pw,$Qw,$Hw,$Rw,$Pf,$Sw,$ta,$Tw,$G9,$Uw,$Yw,$Zw,$M9,$cx,$Og,$dx,$Yf,$ex,$Fg,$fx,$Mg,$gx,$ky,$ly,$wy,$xy,$uy,$yy,$Qx,$zy,$Cx,$Ay,$Wx,$By,$Kx,$Cy,$nx,$Dy,$tx,$Ey,$iy,$Fy,$o9,$Gy,$Kz,$Oz,$cA,$dA,$Yz,$eA,$Yy,$fA,$Bz,$gA,$sz,$hA,$Iz,$iA,$sB,$tB,$xB,$yB,$qB,$zB,$AA,$AB,$tA,$BB,$UA,$CB,$Qd,$DB,$PB,$QB,$Sd,$RB,$VB,$WB,$Ab,$XB,$Yb,$YB,$i3,$ZB,$Xg,$cC,$EC,$GC,$cD,$dD,$rg,$eD,$w9,$fD,$T9,$gD,$ca,$hD,$FE,$GE,$KE,$LE,$ND,$ME,$DE,$NE,$kE,$OE,$tE,$PE,$vD,$QE,$JF,$KF,$UF,$VF,$ZF,$cG,$SF,$dG,$Jf,$eG,$od,$fG,$ic,$gG,$Id,$hG,$Wc,$iG,$BG,$CG,$GG,$HG,$qG,$IG,$zG,$JG,$Gc,$KG,$yg,$LG,$ja,$MG,$Qe,$NG,$Ie,$OG,$SG,$TG,$Oe,$UG,$ra,$VG,$qH,$rH,$gh,$sH,$IH,$JH,$AH,$KH,$OH,$PH,$GH,$QH,$LI,$MI,$UI,$VI,$vI,$WI,$JI,$XI,$oI,$YI,$gI,$ZI,$SI,$cJ,$eI,$dJ,$dK,$hK,$AK,$BK,$yK,$CK,$qK,$DK,$EJ,$EK,$OJ,$FK,$qJ,$GK,$ZJ,$HK,$xJ,$IK,$rL,$sL,$wL,$xL,$iL,$yL,$RK,$zL,$pL,$AL,$WL,$XL,$dM,$eM,$kM,$lM,$OL,$mM,$IL,$nM,$UL,$oM,$sM,$tM,$xM,$yM,$Qg,$zM,$xh,$AM,$k3,$BM,$rh,$CM,$GM,$HM,$LM,$MM,$QM,$RM,$et,$SM,$Xt,$TM,$ct,$UM,$Os,$VM,$YM,$ZM,$eN};$gN=q#resolvers#;$hN=[];$iN=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$jN=bless({$t,$hN,$v,$q,$w,$iN,$y,$z},$A);$kN=q#file#;$lN=[];$mN=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$nN=bless({$t,$lN,$v,$q,$w,$mN,$y,$z},$A);$oN=q#null#;$pN=[];$qN=q#ni('ni:/io/null')->new#;$rN=bless({$t,$pN,$v,$q,$w,$qN,$y,$z},$A);$sN=q#sh#;$tN=[];$uN=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$vN=bless({$t,$tN,$v,$q,$w,$uN,$y,$z},$A);$wN=q#str#;$xN=[];$yN=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$zN=bless({$t,$xN,$v,$q,$w,$yN,$y,$z},$A);$AN={$h8,$jN,$kN,$nN,$oN,$rN,$sN,$vN,$wN,$zN};$BN=bless({$c,$fN,$gN,$AN},$r2);*$Ry=\&$Oy;*$Qy=\&$My;*$Py=\&$Ky;*$Ee=\&$Ce;*$De=\&$Ae;$i3->apply_($I1);$i3->apply_($J1);$i3->apply_($K1);$i3->apply_($L1);$i3->apply_($s1);$i3->apply_($M1);$i3->apply_($w1);$i3->apply_($N1);$i3->apply_($x1);$i3->apply_($O1);$i3->apply_($y1);$i3->apply_($P1);$i3->apply_($z1);$i3->apply_($Q1);$i3->apply_($A1);$i3->apply_($R1);$i3->apply_($B1);$i3->apply_($S1);$i3->apply_($C1);$i3->apply_($T1);$i3->apply_($D1);$i3->apply_($U1);$i3->apply_($E1);$i3->apply_($V1);$i3->apply_($F1);$i3->apply_($W1);$i3->apply_($X1);$i3->apply_($Y1);$i3->apply_($Z1);$i3->apply_($c2);$i3->apply_($d2);$i3->apply_($e2);$i3->apply_($f2);$i3->apply_($g2);$i3->apply_($h2);$i3->apply_($i2);$i3->apply_($j2);$i3->apply_($k2);$i3->apply_($S);$i3->apply_($l2);$i3->apply_($A);$i3->apply_($m2);$i3->apply_($n2);$i3->apply_($o2);$i3->apply_($p2);$i3->apply_($q2);$i3->apply_($r2);$i3->apply_($s2);$i3->apply_($t2);$i3->apply_($u2);$i3->apply_($v2);$i3->apply_($w2);$i3->apply_($x2);$i3->apply_($y2);$i3->apply_($z2);$i3->apply_($A2);$i3->apply_($B2);$i3->apply_($C2);$i3->apply_($D2);$i3->apply_($E2);$i3->apply_($C);$i3->apply_($F2);$i3->apply_($G2);$i3->apply_($H2);$i3->apply_($H);$i3->apply_($I2);$i3->apply_($J2);$i3->apply_($K2);$i3->apply_($L2);$i3->apply_($M2);$i3->apply_($N2);$i3->apply_($O2);$i3->apply_($P2);$i3->apply_($Q2);$i3->apply_($R2);$i3->apply_($S2);$t3->apply_($s1);$t3->apply_($w1);$t3->apply_($x1);$t3->apply_($y1);$t3->apply_($z1);$t3->apply_($A1);$t3->apply_($B1);$t3->apply_($C1);$t3->apply_($D1);$t3->apply_($E1);$t3->apply_($F1);$N3->apply_($s1);$N3->apply_($w1);$N3->apply_($x1);$N3->apply_($y1);$N3->apply_($z1);$N3->apply_($A1);$N3->apply_($B1);$N3->apply_($C1);$N3->apply_($D1);$N3->apply_($E1);$N3->apply_($F1);$V3->apply_($s1);$V3->apply_($w1);$V3->apply_($x1);$V3->apply_($y1);$V3->apply_($z1);$V3->apply_($A1);$V3->apply_($B1);$V3->apply_($C1);$V3->apply_($D1);$V3->apply_($E1);$V3->apply_($F1);$j4->apply_($s1);$j4->apply_($w1);$j4->apply_($x1);$j4->apply_($y1);$j4->apply_($z1);$j4->apply_($A1);$j4->apply_($B1);$j4->apply_($C1);$j4->apply_($D1);$j4->apply_($E1);$j4->apply_($F1);$v4->apply_($s1);$v4->apply_($w1);$v4->apply_($x1);$v4->apply_($y1);$v4->apply_($z1);$v4->apply_($A1);$v4->apply_($B1);$v4->apply_($C1);$v4->apply_($D1);$v4->apply_($E1);$v4->apply_($F1);$H4->apply_($s1);$H4->apply_($w1);$H4->apply_($x1);$H4->apply_($y1);$H4->apply_($z1);$H4->apply_($A1);$H4->apply_($B1);$H4->apply_($C1);$H4->apply_($D1);$H4->apply_($E1);$H4->apply_($F1);$d5->apply_($s1);$v7->apply_($E1);$F7->apply_($E1);$R7->apply_($E1);$f8->apply_($E1);$z8->apply_($E1);$o9->apply_($I1);$o9->apply_($J1);$o9->apply_($L1);$o9->apply_($M1);$o9->apply_($N1);$o9->apply_($O1);$o9->apply_($P1);$o9->apply_($Q1);$o9->apply_($R1);$o9->apply_($S1);$o9->apply_($T1);$o9->apply_($U1);$o9->apply_($V1);$o9->apply_($W1);$o9->apply_($Y1);$o9->apply_($c2);$o9->apply_($e2);$o9->apply_($f2);$o9->apply_($g2);$o9->apply_($h2);$o9->apply_($i2);$o9->apply_($j2);$o9->apply_($k2);$o9->apply_($l2);$o9->apply_($m2);$o9->apply_($o2);$o9->apply_($q2);$o9->apply_($s2);$o9->apply_($u2);$o9->apply_($w2);$o9->apply_($x2);$o9->apply_($y2);$o9->apply_($z2);$o9->apply_($A2);$o9->apply_($C2);$o9->apply_($E2);$o9->apply_($F2);$o9->apply_($H2);$o9->apply_($I2);$o9->apply_($J2);$o9->apply_($K2);$o9->apply_($L2);$o9->apply_($M2);$o9->apply_($O2);$o9->apply_($P2);$o9->apply_($Q2);$o9->apply_($S2);$w9->apply_($I1);$w9->apply_($J1);$w9->apply_($L1);$w9->apply_($M1);$w9->apply_($N1);$w9->apply_($O1);$w9->apply_($P1);$w9->apply_($Q1);$w9->apply_($R1);$w9->apply_($S1);$w9->apply_($T1);$w9->apply_($U1);$w9->apply_($V1);$w9->apply_($W1);$w9->apply_($Y1);$w9->apply_($c2);$w9->apply_($e2);$w9->apply_($g2);$w9->apply_($h2);$w9->apply_($i2);$w9->apply_($j2);$w9->apply_($k2);$w9->apply_($S);$w9->apply_($l2);$w9->apply_($m2);$w9->apply_($o2);$w9->apply_($q2);$w9->apply_($s2);$w9->apply_($u2);$w9->apply_($w2);$w9->apply_($x2);$w9->apply_($y2);$w9->apply_($z2);$w9->apply_($A2);$w9->apply_($C2);$w9->apply_($E2);$w9->apply_($F2);$w9->apply_($H2);$w9->apply_($I2);$w9->apply_($J2);$w9->apply_($K2);$w9->apply_($L2);$w9->apply_($M2);$w9->apply_($O2);$w9->apply_($P2);$w9->apply_($Q2);$w9->apply_($S2);$G9->apply_($j2);$M9->apply_($j2);$T9->apply_($I1);$T9->apply_($J1);$T9->apply_($L1);$T9->apply_($M1);$T9->apply_($N1);$T9->apply_($O1);$T9->apply_($P1);$T9->apply_($Q1);$T9->apply_($R1);$T9->apply_($S1);$T9->apply_($T1);$T9->apply_($U1);$T9->apply_($V1);$T9->apply_($W1);$T9->apply_($Y1);$T9->apply_($c2);$T9->apply_($e2);$T9->apply_($g2);$T9->apply_($h2);$T9->apply_($i2);$T9->apply_($j2);$T9->apply_($k2);$T9->apply_($l2);$T9->apply_($m2);$T9->apply_($o2);$T9->apply_($q2);$T9->apply_($s2);$T9->apply_($u2);$T9->apply_($w2);$T9->apply_($x2);$T9->apply_($y2);$T9->apply_($z2);$T9->apply_($A2);$T9->apply_($C2);$T9->apply_($E2);$T9->apply_($F2);$T9->apply_($H2);$T9->apply_($I2);$T9->apply_($J2);$T9->apply_($K2);$T9->apply_($L2);$T9->apply_($M2);$T9->apply_($O2);$T9->apply_($P2);$T9->apply_($Q2);$T9->apply_($S2);$ca->apply_($I1);$ca->apply_($J1);$ca->apply_($L1);$ca->apply_($M1);$ca->apply_($N1);$ca->apply_($O1);$ca->apply_($P1);$ca->apply_($Q1);$ca->apply_($R1);$ca->apply_($S1);$ca->apply_($T1);$ca->apply_($U1);$ca->apply_($V1);$ca->apply_($W1);$ca->apply_($Y1);$ca->apply_($c2);$ca->apply_($e2);$ca->apply_($g2);$ca->apply_($h2);$ca->apply_($i2);$ca->apply_($j2);$ca->apply_($k2);$ca->apply_($l2);$ca->apply_($m2);$ca->apply_($o2);$ca->apply_($q2);$ca->apply_($s2);$ca->apply_($u2);$ca->apply_($w2);$ca->apply_($x2);$ca->apply_($y2);$ca->apply_($z2);$ca->apply_($A2);$ca->apply_($C2);$ca->apply_($E2);$ca->apply_($F2);$ca->apply_($H2);$ca->apply_($I2);$ca->apply_($J2);$ca->apply_($K2);$ca->apply_($L2);$ca->apply_($M2);$ca->apply_($O2);$ca->apply_($P2);$ca->apply_($Q2);$ca->apply_($S2);$ja->apply_($I1);$ja->apply_($J1);$ja->apply_($L1);$ja->apply_($M1);$ja->apply_($N1);$ja->apply_($O1);$ja->apply_($P1);$ja->apply_($Q1);$ja->apply_($R1);$ja->apply_($S1);$ja->apply_($T1);$ja->apply_($U1);$ja->apply_($V1);$ja->apply_($W1);$ja->apply_($Y1);$ja->apply_($c2);$ja->apply_($e2);$ja->apply_($g2);$ja->apply_($h2);$ja->apply_($i2);$ja->apply_($j2);$ja->apply_($k2);$ja->apply_($l2);$ja->apply_($m2);$ja->apply_($o2);$ja->apply_($q2);$ja->apply_($s2);$ja->apply_($u2);$ja->apply_($w2);$ja->apply_($y2);$ja->apply_($z2);$ja->apply_($A2);$ja->apply_($C2);$ja->apply_($E2);$ja->apply_($F2);$ja->apply_($H2);$ja->apply_($I2);$ja->apply_($J2);$ja->apply_($K2);$ja->apply_($L2);$ja->apply_($M2);$ja->apply_($O2);$ja->apply_($P2);$ja->apply_($Q2);$ja->apply_($S2);$ra->apply_($j2);$ra->apply_($x2);$Ab->apply_($p2);$Yb->apply_($p2);$ic->apply_($p2);$ic->apply_($v2);$Gc->apply_($p2);$Gc->apply_($v2);$Wc->apply_($p2);$Wc->apply_($v2);$od->apply_($p2);$Id->apply_($p2);$Qd->apply_($p2);$Ie->apply_($x2);$Oe->apply_($x2);$Hf->apply_($I1);$Hf->apply_($J1);$Hf->apply_($L1);$Hf->apply_($M1);$Hf->apply_($N1);$Hf->apply_($O1);$Hf->apply_($P1);$Hf->apply_($Q1);$Hf->apply_($R1);$Hf->apply_($S1);$Hf->apply_($T1);$Hf->apply_($U1);$Hf->apply_($V1);$Hf->apply_($W1);$Hf->apply_($Y1);$Hf->apply_($c2);$Hf->apply_($e2);$Hf->apply_($g2);$Hf->apply_($h2);$Hf->apply_($i2);$Hf->apply_($k2);$Hf->apply_($l2);$Hf->apply_($m2);$Hf->apply_($o2);$Hf->apply_($q2);$Hf->apply_($s2);$Hf->apply_($u2);$Hf->apply_($w2);$Hf->apply_($y2);$Hf->apply_($A2);$Hf->apply_($C2);$Hf->apply_($E2);$Hf->apply_($F2);$Hf->apply_($H2);$Hf->apply_($I2);$Hf->apply_($J2);$Hf->apply_($K2);$Hf->apply_($L2);$Hf->apply_($M2);$Hf->apply_($O2);$Hf->apply_($P2);$Hf->apply_($Q2);$Hf->apply_($S2);$Pf->apply_($I1);$Pf->apply_($J1);$Pf->apply_($L1);$Pf->apply_($M1);$Pf->apply_($N1);$Pf->apply_($O1);$Pf->apply_($P1);$Pf->apply_($Q1);$Pf->apply_($R1);$Pf->apply_($S1);$Pf->apply_($T1);$Pf->apply_($U1);$Pf->apply_($V1);$Pf->apply_($W1);$Pf->apply_($Y1);$Pf->apply_($c2);$Pf->apply_($e2);$Pf->apply_($g2);$Pf->apply_($i2);$Pf->apply_($k2);$Pf->apply_($l2);$Pf->apply_($m2);$Pf->apply_($o2);$Pf->apply_($q2);$Pf->apply_($s2);$Pf->apply_($u2);$Pf->apply_($w2);$Pf->apply_($y2);$Pf->apply_($A2);$Pf->apply_($C2);$Pf->apply_($E2);$Pf->apply_($F2);$Pf->apply_($H2);$Pf->apply_($I2);$Pf->apply_($J2);$Pf->apply_($K2);$Pf->apply_($L2);$Pf->apply_($M2);$Pf->apply_($O2);$Pf->apply_($P2);$Pf->apply_($Q2);$Pf->apply_($S2);$Yf->apply_($I1);$Yf->apply_($J1);$Yf->apply_($L1);$Yf->apply_($M1);$Yf->apply_($N1);$Yf->apply_($O1);$Yf->apply_($P1);$Yf->apply_($Q1);$Yf->apply_($R1);$Yf->apply_($S1);$Yf->apply_($T1);$Yf->apply_($U1);$Yf->apply_($V1);$Yf->apply_($W1);$Yf->apply_($Y1);$Yf->apply_($c2);$Yf->apply_($e2);$Yf->apply_($g2);$Yf->apply_($h2);$Yf->apply_($i2);$Yf->apply_($k2);$Yf->apply_($l2);$Yf->apply_($m2);$Yf->apply_($o2);$Yf->apply_($q2);$Yf->apply_($s2);$Yf->apply_($u2);$Yf->apply_($w2);$Yf->apply_($y2);$Yf->apply_($A2);$Yf->apply_($C2);$Yf->apply_($E2);$Yf->apply_($F2);$Yf->apply_($H2);$Yf->apply_($I2);$Yf->apply_($J2);$Yf->apply_($K2);$Yf->apply_($L2);$Yf->apply_($M2);$Yf->apply_($O2);$Yf->apply_($P2);$Yf->apply_($Q2);$Yf->apply_($S2);$kg->apply_($I1);$kg->apply_($J1);$kg->apply_($L1);$kg->apply_($M1);$kg->apply_($N1);$kg->apply_($O1);$kg->apply_($P1);$kg->apply_($Q1);$kg->apply_($R1);$kg->apply_($S1);$kg->apply_($T1);$kg->apply_($U1);$kg->apply_($V1);$kg->apply_($W1);$kg->apply_($Y1);$kg->apply_($c2);$kg->apply_($e2);$kg->apply_($g2);$kg->apply_($h2);$kg->apply_($i2);$kg->apply_($k2);$kg->apply_($l2);$kg->apply_($m2);$kg->apply_($o2);$kg->apply_($q2);$kg->apply_($s2);$kg->apply_($u2);$kg->apply_($w2);$kg->apply_($y2);$kg->apply_($A2);$kg->apply_($C2);$kg->apply_($E2);$kg->apply_($F2);$kg->apply_($H2);$kg->apply_($I2);$kg->apply_($J2);$kg->apply_($K2);$kg->apply_($L2);$kg->apply_($M2);$kg->apply_($O2);$kg->apply_($P2);$kg->apply_($Q2);$kg->apply_($S2);$rg->apply_($I1);$rg->apply_($J1);$rg->apply_($L1);$rg->apply_($M1);$rg->apply_($N1);$rg->apply_($O1);$rg->apply_($P1);$rg->apply_($Q1);$rg->apply_($R1);$rg->apply_($S1);$rg->apply_($T1);$rg->apply_($U1);$rg->apply_($V1);$rg->apply_($W1);$rg->apply_($Y1);$rg->apply_($c2);$rg->apply_($e2);$rg->apply_($g2);$rg->apply_($h2);$rg->apply_($i2);$rg->apply_($k2);$rg->apply_($l2);$rg->apply_($m2);$rg->apply_($o2);$rg->apply_($q2);$rg->apply_($s2);$rg->apply_($u2);$rg->apply_($w2);$rg->apply_($y2);$rg->apply_($A2);$rg->apply_($C2);$rg->apply_($E2);$rg->apply_($F2);$rg->apply_($H2);$rg->apply_($I2);$rg->apply_($J2);$rg->apply_($K2);$rg->apply_($L2);$rg->apply_($M2);$rg->apply_($O2);$rg->apply_($P2);$rg->apply_($Q2);$rg->apply_($S2);$yg->apply_($I1);$yg->apply_($J1);$yg->apply_($L1);$yg->apply_($M1);$yg->apply_($N1);$yg->apply_($O1);$yg->apply_($P1);$yg->apply_($Q1);$yg->apply_($R1);$yg->apply_($S1);$yg->apply_($T1);$yg->apply_($U1);$yg->apply_($V1);$yg->apply_($W1);$yg->apply_($Y1);$yg->apply_($c2);$yg->apply_($e2);$yg->apply_($g2);$yg->apply_($h2);$yg->apply_($i2);$yg->apply_($k2);$yg->apply_($l2);$yg->apply_($m2);$yg->apply_($o2);$yg->apply_($q2);$yg->apply_($s2);$yg->apply_($u2);$yg->apply_($w2);$yg->apply_($y2);$yg->apply_($A2);$yg->apply_($C2);$yg->apply_($E2);$yg->apply_($F2);$yg->apply_($H2);$yg->apply_($I2);$yg->apply_($J2);$yg->apply_($K2);$yg->apply_($L2);$yg->apply_($M2);$yg->apply_($O2);$yg->apply_($P2);$yg->apply_($Q2);$yg->apply_($S2);$Fg->apply_($I1);$Fg->apply_($J1);$Fg->apply_($L1);$Fg->apply_($M1);$Fg->apply_($N1);$Fg->apply_($O1);$Fg->apply_($P1);$Fg->apply_($Q1);$Fg->apply_($R1);$Fg->apply_($S1);$Fg->apply_($T1);$Fg->apply_($U1);$Fg->apply_($V1);$Fg->apply_($W1);$Fg->apply_($Y1);$Fg->apply_($c2);$Fg->apply_($e2);$Fg->apply_($g2);$Fg->apply_($h2);$Fg->apply_($i2);$Fg->apply_($k2);$Fg->apply_($l2);$Fg->apply_($m2);$Fg->apply_($o2);$Fg->apply_($q2);$Fg->apply_($s2);$Fg->apply_($u2);$Fg->apply_($w2);$Fg->apply_($y2);$Fg->apply_($A2);$Fg->apply_($C2);$Fg->apply_($E2);$Fg->apply_($F2);$Fg->apply_($H2);$Fg->apply_($I2);$Fg->apply_($J2);$Fg->apply_($K2);$Fg->apply_($L2);$Fg->apply_($M2);$Fg->apply_($O2);$Fg->apply_($P2);$Fg->apply_($Q2);$Fg->apply_($S2);$Mg->apply_($I1);$Mg->apply_($J1);$Mg->apply_($L1);$Mg->apply_($M1);$Mg->apply_($N1);$Mg->apply_($O1);$Mg->apply_($P1);$Mg->apply_($Q1);$Mg->apply_($R1);$Mg->apply_($S1);$Mg->apply_($T1);$Mg->apply_($U1);$Mg->apply_($V1);$Mg->apply_($W1);$Mg->apply_($Y1);$Mg->apply_($c2);$Mg->apply_($e2);$Mg->apply_($g2);$Mg->apply_($h2);$Mg->apply_($i2);$Mg->apply_($k2);$Mg->apply_($l2);$Mg->apply_($m2);$Mg->apply_($o2);$Mg->apply_($q2);$Mg->apply_($s2);$Mg->apply_($u2);$Mg->apply_($w2);$Mg->apply_($y2);$Mg->apply_($A2);$Mg->apply_($C2);$Mg->apply_($E2);$Mg->apply_($F2);$Mg->apply_($H2);$Mg->apply_($I2);$Mg->apply_($J2);$Mg->apply_($K2);$Mg->apply_($L2);$Mg->apply_($M2);$Mg->apply_($O2);$Mg->apply_($P2);$Mg->apply_($Q2);$Mg->apply_($S2);$Xg->apply_($I1);$Xg->apply_($J1);$Xg->apply_($L1);$Xg->apply_($M1);$Xg->apply_($N1);$Xg->apply_($O1);$Xg->apply_($P1);$Xg->apply_($Q1);$Xg->apply_($R1);$Xg->apply_($S1);$Xg->apply_($T1);$Xg->apply_($U1);$Xg->apply_($V1);$Xg->apply_($W1);$Xg->apply_($Y1);$Xg->apply_($c2);$Xg->apply_($e2);$Xg->apply_($g2);$Xg->apply_($i2);$Xg->apply_($k2);$Xg->apply_($l2);$Xg->apply_($A);$Xg->apply_($m2);$Xg->apply_($o2);$Xg->apply_($q2);$Xg->apply_($s2);$Xg->apply_($u2);$Xg->apply_($w2);$Xg->apply_($x2);$Xg->apply_($y2);$Xg->apply_($z2);$Xg->apply_($A2);$Xg->apply_($C2);$Xg->apply_($E2);$Xg->apply_($F2);$Xg->apply_($H2);$Xg->apply_($I2);$Xg->apply_($J2);$Xg->apply_($K2);$Xg->apply_($M2);$Xg->apply_($O2);$Xg->apply_($P2);$Xg->apply_($Q2);$Xg->apply_($S2);$gh->apply_($I1);$gh->apply_($J1);$gh->apply_($L1);$gh->apply_($M1);$gh->apply_($N1);$gh->apply_($O1);$gh->apply_($P1);$gh->apply_($Q1);$gh->apply_($R1);$gh->apply_($S1);$gh->apply_($T1);$gh->apply_($U1);$gh->apply_($V1);$gh->apply_($W1);$gh->apply_($Y1);$gh->apply_($c2);$gh->apply_($e2);$gh->apply_($g2);$gh->apply_($i2);$gh->apply_($k2);$gh->apply_($l2);$gh->apply_($m2);$gh->apply_($o2);$gh->apply_($q2);$gh->apply_($s2);$gh->apply_($u2);$gh->apply_($w2);$gh->apply_($y2);$gh->apply_($A2);$gh->apply_($C2);$gh->apply_($E2);$gh->apply_($F2);$gh->apply_($H2);$gh->apply_($I2);$gh->apply_($K2);$gh->apply_($M2);$gh->apply_($O2);$gh->apply_($P2);$gh->apply_($Q2);$gh->apply_($S2);$Nh->apply_($K1);$Vh->apply_($K1);$ui->apply_($M1);$ui->apply_($N1);$ui->apply_($O1);$ui->apply_($P1);$ui->apply_($Q1);$ui->apply_($R1);$ui->apply_($S1);$ui->apply_($T1);$ui->apply_($U1);$ui->apply_($V1);$ui->apply_($W1);$Mi->apply_($w1);$gj->apply_($w1);$yj->apply_($x1);$Fj->apply_($x1);$Yj->apply_($y1);$hk->apply_($y1);$Bk->apply_($y1);$Vk->apply_($y1);$fl->apply_($y1);$vl->apply_($y1);$Sl->apply_($z1);$Sl->apply_($B1);$Zl->apply_($z1);$jm->apply_($z1);$zm->apply_($z1);$zm->apply_($B1);$Km->apply_($z1);$Ym->apply_($z1);$Ym->apply_($B1);$Dn->apply_($Q1);$Vn->apply_($A1);$fo->apply_($A1);$mo->apply_($A1);$Co->apply_($A1);$Ko->apply_($A1);$hp->apply_($A1);$Dp->apply_($B1);$Mp->apply_($B1);$zq->apply_($Aq);$Kq->apply_($C1);$Xq->apply_($C1);$Vr->apply_($F1);$es->apply_($F1);$ss->apply_($F1);$Os->apply_($X1);$Os->apply_($Z1);$Os->apply_($d2);$Os->apply_($R2);$ct->apply_($X1);$ct->apply_($Z1);$ct->apply_($d2);$ct->apply_($R2);$rt->apply_($X1);$rt->apply_($Z1);$rt->apply_($d2);$Lt->apply_($X1);$Lt->apply_($Z1);$Lt->apply_($d2);$du->apply_($Y1);$du->apply_($c2);$du->apply_($e2);$zu->apply_($Z1);$Gu->apply_($Z1);$Qu->apply_($Z1);$ev->apply_($Z1);$Fv->apply_($c2);$Yv->apply_($d2);$hw->apply_($d2);$Hw->apply_($h2);$nx->apply_($S);$tx->apply_($S);$Cx->apply_($S);$Kx->apply_($S);$Qx->apply_($S);$Wx->apply_($S);$iy->apply_($S);$uy->apply_($l2);$Yy->apply_($A);$sz->apply_($A);$Bz->apply_($A);$Iz->apply_($A);$Yz->apply_($m2);$tA->apply_($n2);$AA->apply_($n2);$UA->apply_($n2);$qB->apply_($n2);$PB->apply_($Aq);$EC->apply_($FC);$cD->apply_($FC);$vD->apply_($r2);$ND->apply_($r2);$kE->apply_($r2);$tE->apply_($r2);$DE->apply_($r2);$JF->apply_($FC);$SF->apply_($t2);$SF->apply_($C);$SF->apply_($H);$qG->apply_($v2);$zG->apply_($v2);$qH->apply_($Aq);$AH->apply_($z2);$GH->apply_($z2);$eI->apply_($B2);$eI->apply_($D2);$oI->apply_($B2);$vI->apply_($B2);$JI->apply_($B2);$qJ->apply_($C);$xJ->apply_($C);$EJ->apply_($C);$OJ->apply_($C);$ZJ->apply_($C);$qK->apply_($F2);$yK->apply_($F2);$RK->apply_($G2);$iL->apply_($G2);$pL->apply_($G2);$IL->apply_($H);$OL->apply_($H);$UL->apply_($H);$kM->apply_($Aq);$ni::self=$BN;&$V($T);&$V($e1);&$V($k1);&$V($i3);&$V($k3);&$m3($k3);&$V($t3);&$V($N3);&$V($V3);&$V($j4);&$V($v4);&$V($H4);&$V($J4);&$m3($J4);&$V($d5);&$V($f5);&$m3($f5);&$V($l5);&$V($r5);&$V($E5);&$V($R5);&$V($g6);&$V($t6);&$V($P6);&$V($V6);&$V($e7);&$V($v7);&$V($F7);&$V($R7);&$V($f8);&$V($z8);&$V($B8);&$m3($B8);&$V($U8);&$V($d9);&$V($o9);&$V($q9);&$m3($q9);&$V($w9);&$V($G9);&$V($M9);&$V($T9);&$V($ca);&$V($ja);&$V($ra);&$V($ta);&$m3($ta);&$V($za);&$V($Ta);&$V($mb);&$V($Ab);&$V($Yb);&$V($ic);&$V($Gc);&$V($Wc);&$V($od);&$V($Id);&$V($Qd);&$V($Sd);&$m3($Sd);&$V($Yd);&$V($ie);&$V($te);&$V($Ie);&$V($Oe);&$V($Qe);&$m3($Qe);&$V($nf);&$V($tf);&$V($Hf);&$V($Jf);&$V($Pf);&$V($Yf);&$V($kg);&$V($rg);&$V($yg);&$V($Fg);&$V($Mg);&$V($Og);&$V($Qg);&$m3($Qg);&$V($Xg);&$V($gh);&$V($ih);&$m3($ih);&$V($rh);&$m3($rh);&$V($vh);&$m3($vh);&$V($xh);&$m3($xh);&$V($zh);&$m3($zh);&$V($Eh);&$m3($Eh);&$V($Nh);&$V($Vh);&$V($Xh);&$m3($Xh);&$V($ei);&$m3($ei);&$V($ui);&$V($wi);&$m3($wi);&$V($yi);&$m3($yi);&$V($Di);&$m3($Di);&$V($Mi);&$V($gj);&$V($ij);&$m3($ij);&$V($nj);&$m3($nj);&$V($yj);&$V($Fj);&$V($Hj);&$m3($Hj);&$V($Mj);&$m3($Mj);&$V($Yj);&$V($hk);&$V($Bk);&$V($Vk);&$V($fl);&$V($vl);&$V($xl);&$m3($xl);&$V($Cl);&$m3($Cl);&$V($Sl);&$V($Zl);&$V($jm);&$V($zm);&$V($Km);&$V($Ym);&$V($dn);&$m3($dn);&$gn($dn);&$V($Dn);&$V($Fn);&$m3($Fn);&$V($Vn);&$V($fo);&$V($mo);&$V($Co);&$V($Ko);&$V($hp);&$V($jp);&$m3($jp);&$V($op);&$m3($op);&$V($Dp);&$V($Mp);&$V($Op);&$m3($Op);&$V($Tp);&$m3($Tp);&$V($zq);&$V($Kq);&$V($Xq);&$V($Zq);&$m3($Zq);&$V($gr);&$m3($gr);&$V($xr);&$m3($xr);&$V($Vr);&$V($es);&$V($ss);&$V($us);&$m3($us);&$V($zs);&$m3($zs);&$V($Os);&$V($ct);&$V($et);&$m3($et);&$V($rt);&$V($Lt);&$V($Nt);&$m3($Nt);&$Qt($Nt);&$V($Xt);&$m3($Xt);&$V($du);&$V($fu);&$m3($fu);&$V($zu);&$V($Gu);&$V($Qu);&$V($ev);&$V($jv);&$m3($jv);&$Qt($jv);&$mv($jv);&$V($Fv);&$V($Hv);&$m3($Hv);&$V($Yv);&$V($hw);&$V($jw);&$m3($jw);&$Qt($jw);&$V($ow);&$m3($ow);&$V($vw);&$m3($vw);&$V($Hw);&$V($Jw);&$m3($Jw);&$V($Pw);&$m3($Pw);&$V($Yw);&$m3($Yw);&$V($nx);&$V($tx);&$V($Cx);&$V($Kx);&$V($Qx);&$V($Wx);&$V($iy);&$V($ky);&$m3($ky);&$V($uy);&$V($wy);&$m3($wy);&$V($Yy);&$V($sz);&$V($Bz);&$V($Iz);&$V($Kz);&$m3($Kz);&$Nz($Kz);&$V($Yz);&$V($cA);&$m3($cA);&$V($tA);&$V($AA);&$V($UA);&$V($qB);&$V($sB);&$m3($sB);&$V($xB);&$m3($xB);&$V($PB);&$V($VB);&$m3($VB);&$V($EC);&$V($cD);&$V($vD);&$V($ND);&$V($kE);&$V($tE);&$V($DE);&$V($FE);&$m3($FE);&$V($KE);&$m3($KE);&$V($JF);&$V($SF);&$V($UF);&$m3($UF);&$V($ZF);&$m3($ZF);&$V($qG);&$V($zG);&$V($BG);&$m3($BG);&$V($GG);&$m3($GG);&$V($SG);&$m3($SG);&$V($qH);&$V($AH);&$V($GH);&$V($IH);&$m3($IH);&$V($OH);&$m3($OH);&$V($eI);&$V($gI);&$m3($gI);&$V($oI);&$V($vI);&$V($JI);&$V($LI);&$m3($LI);&$V($SI);&$m3($SI);&$V($UI);&$m3($UI);&$V($qJ);&$V($xJ);&$V($EJ);&$V($OJ);&$V($ZJ);&$V($dK);&$m3($dK);&$gK($dK);&$V($qK);&$V($yK);&$V($AK);&$m3($AK);&$V($RK);&$V($iL);&$V($pL);&$V($rL);&$m3($rL);&$V($wL);&$m3($wL);&$V($IL);&$V($OL);&$V($UL);&$V($WL);&$m3($WL);&$V($dM);&$m3($dM);&$V($kM);&$V($sM);&$m3($sM);&$V($xM);&$m3($xM);&$V($GM);&$m3($GM);&$V($LM);&$m3($LM);&$V($QM);&$m3($QM);&$V($YM);&$m3($YM);&$V($eN);&$m3($eN);ni->run(@ARGV);
__DATA__
