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
written differently from other modules.#;$Y8=[$i,$W8,$X8];$Z8=[$Y8];$c9=q#/lib#;$d9=bless({$e,$Z8,$Q,$c9},$S);$e9=q#ni.doc:/lib/doc#;$f9=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ->TODO(...)
  ...#;$g9=[$f,$f9];$h9=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$i9=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$j9=[];$k9=[];$l9=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$m9=bless({$t,$k9,$v,$q,$w,$l9,$y,$z},$A);$n9=bless({$n,$j9,$p,$q,$r,$q,$s,$m9},$C);$o9=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$p9=[];$q9=[];$r9=q#my $doc = ni("ni:/object")->child("/doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$s9=bless({$t,$q9,$v,$q,$w,$r9,$y,$z},$A);$t9=bless({$n,$p9,$p,$q,$r,$q,$s,$s9},$C);$u9=[$i,$h9,$i9,$n9,$o9,$t9];$v9=[$g9,$u9];$w9=q#/lib/doc#;$x9=bless({$e,$v9,$Q,$w9},$S);$y9=q#ni.doc:/lib/future#;$z9=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$A9=[];$B9=[];$C9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$D9=bless({$t,$B9,$v,$q,$w,$C9,$y,$z},$A);$E9=bless({$n,$A9,$p,$q,$r,$q,$s,$D9},$C);$F9=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$G9=[];$H9=[];$I9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$J9=bless({$t,$H9,$v,$q,$w,$I9,$y,$z},$A);$K9=bless({$n,$G9,$p,$q,$r,$q,$s,$J9},$C);$L9=[$i,$z9,$E9,$F9,$K9];$M9=[$L9];$N9=q#/lib/future#;$O9=bless({$e,$M9,$Q,$N9},$S);$P9=q#ni.doc:/lib/image#;$Q9=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$R9=[$f,$Q9];$S9=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$T9=[$i,$S9];$U9={$p2,1};$V9=q#/lib/image#;$W9={};$X9=[];$Y9=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Z9=bless({$t,$X9,$v,$q,$w,$Y9,$y,$z},$A);$ca={$L4,$Z9};$da=q#/lib/image_init.b#;$ea=bless({$r1,$W9,$W2,$q,$X2,$q,$Y2,$ca,$Q,$da},$x2);$fa={};$ga=q#boot_side_effect#;$ha=[];$ia=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$ja=bless({$t,$ha,$v,$q,$w,$ia,$y,$z},$A);$ka=q#finalizer#;$la=[];$ma=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$na=bless({$t,$la,$v,$q,$w,$ma,$y,$z},$A);$oa=q#io#;$pa=[];$qa=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$ra=bless({$t,$pa,$v,$q,$w,$qa,$y,$z},$A);$sa=q#reconstruction#;$ta=[];$ua=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$va=bless({$t,$ta,$v,$q,$w,$ua,$y,$z},$A);$wa=q#side_effect#;$xa=[];$ya=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$za=bless({$t,$xa,$v,$q,$w,$ya,$y,$z},$A);$Aa={$ga,$ja,$ka,$na,$oa,$ra,$sa,$va,$wa,$za};$Ba=q#/lib/image_quoting.b#;$Ca=bless({$r1,$fa,$W2,$q,$X2,$q,$Y2,$Aa,$Q,$Ba},$x2);$Da={};$Ea=q#quote_code#;$Fa=[];$Ga=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Ha=bless({$t,$Fa,$v,$q,$w,$Ga,$y,$z},$A);$Ia={$Ea,$Ha};$Ja=q#/lib/quote_code_fail.b#;$Ka=bless({$r1,$Da,$W2,$q,$X2,$q,$Y2,$Ia,$Q,$Ja},$x2);$La={};$Ma=q#quote_array#;$Na=[];$Oa=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Pa=bless({$t,$Na,$v,$q,$w,$Oa,$y,$z},$A);$Qa=q#quote_hash#;$Ra=[];$Sa=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Ta=bless({$t,$Ra,$v,$q,$w,$Sa,$y,$z},$A);$Ua=q#quote_scalar#;$Va=[];$Wa=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Xa=bless({$t,$Va,$v,$q,$w,$Wa,$y,$z},$A);$Ya=q#quote_scalar_ref#;$Za=[];$cb=q#'\\\\' . shift->quote(${$_[0]})#;$db=bless({$t,$Za,$v,$q,$w,$cb,$y,$z},$A);$eb=q#quote_value#;$fb=[];$gb=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$hb=bless({$t,$fb,$v,$q,$w,$gb,$y,$z},$A);$ib={$Ma,$Pa,$Qa,$Ta,$Ua,$Xa,$Ya,$db,$eb,$hb};$jb=q#/lib/quote_values.b#;$kb=bless({$r1,$La,$W2,$q,$X2,$q,$Y2,$ib,$Q,$jb},$x2);$lb={};$mb=q#quote_blessed#;$nb=[];$ob=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$pb=bless({$t,$nb,$v,$q,$w,$ob,$y,$z},$A);$qb=q#quote_class#;$rb=[];$sb=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$tb=bless({$t,$rb,$v,$q,$w,$sb,$y,$z},$A);$ub=q#quote_object#;$vb=[];$wb=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$xb=bless({$t,$vb,$v,$q,$w,$wb,$y,$z},$A);$yb={$mb,$pb,$qb,$tb,$ub,$xb};$zb=q#/lib/quote_objects.b#;$Ab=bless({$r1,$lb,$W2,$q,$X2,$q,$Y2,$yb,$Q,$zb},$x2);$Bb={};$Cb=q#circular_arrayref#;$Db=[];$Eb=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Fb=bless({$t,$Db,$v,$q,$w,$Eb,$y,$z},$A);$Gb=q#circular_hashref#;$Hb=[];$Ib=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Jb=bless({$t,$Hb,$v,$q,$w,$Ib,$y,$z},$A);$Kb=q#is_circular#;$Lb=[];$Mb=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Nb=bless({$t,$Lb,$v,$q,$w,$Mb,$y,$z},$A);$Ob={$Cb,$Fb,$Gb,$Jb,$Kb,$Nb};$Pb=q#/lib/quote_circular_addressed.b#;$Qb=bless({$r1,$Bb,$W2,$q,$X2,$q,$Y2,$Ob,$Q,$Pb},$x2);$Rb={};$Sb=q#address#;$Tb=[];$Ub=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Vb=bless({$t,$Tb,$v,$q,$w,$Ub,$y,$z},$A);$Wb=q#allocate_gensym#;$Xb=[];$Yb=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Zb=bless({$t,$Xb,$v,$q,$w,$Yb,$y,$z},$A);$cc=q#circular_links#;$dc=[];$ec=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$fc=bless({$t,$dc,$v,$q,$w,$ec,$y,$z},$A);$gc=q#quote#;$hc=[];$ic=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$jc=bless({$t,$hc,$v,$q,$w,$ic,$y,$z},$A);$kc={$Sb,$Vb,$Wb,$Zb,$cc,$fc,$gc,$jc};$lc=q#/lib/quote_gensym_identity.b#;$mc=bless({$r1,$Rb,$W2,$q,$X2,$q,$Y2,$kc,$Q,$lc},$x2);$nc={};$oc=q#gensym#;$pc=[];$qc=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$rc=bless({$t,$pc,$v,$q,$w,$qc,$y,$z},$A);$sc={$oc,$rc};$tc=q#/lib/gensym_generator_compact.b#;$uc=bless({$r1,$nc,$W2,$q,$X2,$q,$Y2,$sc,$Q,$tc},$x2);$vc=[$k3,$ea,$Ca,$Ka,$kb,$Ab,$Qb,$mc,$uc];$wc=bless({$r1,$U9,$Q,$V9,$v1,$vc},$q2);$xc=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$yc=[$xc];$zc=bless({$q1,$wc,$E,$yc},$H);$Ac=[$p1,$zc];$Bc=[$R9,$T9,$Ac];$Cc=bless({$e,$Bc,$Q,$V9},$S);$Dc=q#ni.doc:/lib/ni#;$Ec=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$Fc=[$f,$Ec];$Gc=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$Hc=[$i,$Gc];$Ic=[$Fc,$Hc];$Jc=q#/lib/ni#;$Kc=bless({$e,$Ic,$Q,$Jc},$S);$Lc=q#ni.doc:/lib/quote_simple#;$Mc=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$Nc=[];$Oc=[];$Pc=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$Qc=bless({$t,$Oc,$v,$q,$w,$Pc,$y,$z},$A);$Rc=bless({$n,$Nc,$p,$q,$r,$q,$s,$Qc},$C);$Sc=[$i,$Mc,$Rc];$Tc=[$Sc];$Uc=q#/lib/quote_simple#;$Vc=bless({$e,$Tc,$Q,$Uc},$S);$Wc=q#ni.doc:/lib/slice#;$Xc=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$Yc=[$f,$Xc];$Zc={$x2,1};$cd=q#/lib/slice#;$dd={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$h2,1,$i2,1,$j2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$x2,1,$y2,1,$z2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$ed=q#/lib/behavior#;$fd={};$gd=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$hd=bless({$w,$gd,$y,$z},$A);$id={$e,$hd};$jd=q#/lib/documentable.b#;$kd=bless({$r1,$fd,$W2,$q,$X2,$q,$Y2,$id,$Q,$jd},$x2);$ld=[$k3,$kd];$md=bless({$r1,$dd,$Q,$ed,$v1,$ld},$g2);$nd={};$od=q#$_[0]->namespace . ":" . $_[0]->{name}#;$pd=bless({$w,$od,$y,$z},$A);$qd={$Q,$pd};$rd=q#/lib/named.b#;$sd=bless({$r1,$nd,$W2,$V,$X2,$q,$Y2,$qd,$Q,$rd},$x2);$td=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$ud=bless({$w,$td,$y,$z},$A);$vd=q#local $_;
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
$self;#;$wd=bless({$w,$vd,$y,$z},$A);$xd=q#lib/slice::apply#;$yd=q#lib/slice::apply_#;$zd={};$Ad=q#apply#;$Bd=q#apply_#;$Cd={$Ad,$ud,$Bd,$wd};$Dd=q#/lib/slice.b#;$Ed=bless({$r1,$zd,$Y2,$Cd,$Q,$Dd},$x2);$Fd={};$Gd=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$Hd=bless({$w,$Gd,$y,$z},$A);$Id={$L4,$Hd};$Jd=q#/lib/slice_init.b#;$Kd=bless({$r1,$Fd,$Y2,$Id,$Q,$Jd},$x2);$Ld={};$Md=q#serialize#;$Nd=[];$Od=q#local $_;
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
$g;#;$Pd=bless({$t,$Nd,$v,$q,$w,$Od,$y,$z},$A);$Qd={$Md,$Pd};$Rd=q#/lib/slice_serialize.b#;$Sd=bless({$r1,$Ld,$W2,$q,$X2,$q,$Y2,$Qd,$Q,$Rd},$x2);$Td=[$md,$sd,$Ed,$Kd,$Sd];$Ud=bless({$r1,$Zc,$Q,$cd,$v1,$Td},$y2);$Vd=q#A bug that takes the cake:
https://searchcode.com/file/109026149/lib/overload.t\#l-1486

Ok, here's the problem. Rebuilt images of ni fail on old perls, but the
boot code works -- the reason is that bless() doesn't commute across
overload magic installation on old perls; references themselves have an
overload flag that's set when the reference is created, not when the
underlying package is modified. So if we want overloading to work, we
need to install overloaded methods before we bless things.#;$Wd=[$Vd];$Xd=bless({$q1,$Ud,$E,$Wd},$H);$Yd=[$p1,$Xd];$Zd=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$ce=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$de=[];$ee=[];$fe=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$ge=bless({$t,$ee,$v,$q,$w,$fe,$y,$z},$A);$he=bless({$n,$de,$p,$q,$r,$q,$s,$ge},$C);$ie=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$je=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$ke=[];$le=[];$me=q#my $instances = 0;
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
now $instances == 0;#;$ne=bless({$t,$le,$v,$q,$w,$me,$y,$z},$A);$oe=bless({$n,$ke,$p,$q,$r,$q,$s,$ne},$C);$pe=[$i,$Zd,$ce,$he,$ie,$je,$oe];$qe=[$Yc,$Yd,$pe];$re=bless({$e,$qe,$Q,$cd},$S);$se=q#ni.doc:/semantic#;$te=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$ue=[$i,$te];$ve=[$ue];$we=q#/semantic#;$xe=bless({$e,$ve,$Q,$we},$S);$ye=q#ni:/class#;$ze={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$K2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Ae={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Be=q#/module#;$Ce=q#/lib/perlbranch.b#;$De={};$Ee=q#add#;$Fe=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Ge=bless({$w,$Fe,$y,$z},$A);$He=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Ie=bless({$w,$He,$y,$z},$A);$Je={$Ee,$Ge,$Ad,$Ie};$Ke=q#/lib/branch.b#;$Le=bless({$r1,$De,$W2,$q,$X2,$q,$Y2,$Je,$Q,$Ke},$x2);$Me={};$Ne=q#namespace#;$Oe=q#'ni'#;$Pe=bless({$w,$Oe,$y,$z},$A);$Qe={$Ne,$Pe};$Re=q#/lib/named_in_ni.b#;$Se=bless({$r1,$Me,$W2,$q,$X2,$q,$Y2,$Qe,$Q,$Re},$x2);$Te={};$Ue=q#package#;$Ve=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$We=bless({$w,$Ve,$y,$z},$A);$Xe={$Ue,$We};$Ye=q#/lib/namespaced.b#;$Ze=bless({$r1,$Te,$W2,$q,$X2,$q,$Y2,$Xe,$Q,$Ye},$x2);$cf={};$df=q#resolve#;$ef=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$ff=bless({$w,$ef,$y,$z},$A);$gf={$df,$ff};$hf=q#/lib/resolver.b#;$if=bless({$r1,$cf,$W2,$q,$X2,$q,$Y2,$gf,$Q,$hf},$x2);$jf=[$Le,$sd,$Se,$Ze,$if];$kf=bless({$Q,$Ce,$v1,$jf},$z2);$lf={};$mf=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$nf=bless({$w,$mf,$y,$z},$A);$of={$L4,$nf};$pf=q#/lib/class_init.b#;$qf=bless({$r1,$lf,$W2,$m3,$X2,$q,$Y2,$of,$Q,$pf},$x2);$rf={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$h2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$sf=q#/lib/definition.b#;$tf={};$uf=q#def#;$vf=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$wf=bless({$w,$vf,$y,$z},$A);$xf={$uf,$wf};$yf=q#/lib/definition_def.b#;$zf=bless({$r1,$tf,$W2,$q,$X2,$q,$Y2,$xf,$Q,$yf},$x2);$Af={};$Bf=q#ro#;$Cf=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$Df=bless({$w,$Cf,$y,$z},$A);$Ef=q#rw#;$Ff=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$Gf=bless({$w,$Ff,$y,$z},$A);$Hf={$Bf,$Df,$Ef,$Gf};$If=q#/lib/accessor.b#;$Jf=bless({$r1,$Af,$W2,$q,$X2,$q,$Y2,$Hf,$Q,$If},$x2);$Kf={};$Lf=q#(""#;$Mf=q#shift->name#;$Nf=bless({$w,$Mf,$y,$z},$A);$Of={$Lf,$Nf};$Pf=q#/lib/name_as_string.b#;$Qf=bless({$r1,$Kf,$W2,$q,$X2,$q,$Y2,$Of,$Q,$Pf},$x2);$Rf={};$Sf=q#(eq#;$Tf=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Uf=bless({$w,$Tf,$y,$z},$A);$Vf={$Sf,$Uf};$Wf=q#/lib/ref_eq.b#;$Xf=bless({$r1,$Rf,$W2,$q,$X2,$q,$Y2,$Vf,$Q,$Wf},$x2);$Yf={};$Zf=q#defdata#;$cg=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$dg=bless({$w,$cg,$y,$z},$A);$eg={$Zf,$dg};$fg=q#/lib/definition_defdata.b#;$gg=bless({$r1,$Yf,$W2,$q,$X2,$q,$Y2,$eg,$Q,$fg},$x2);$hg={};$ig=q#instantiate_with_defaults#;$jg=q#my ($class, @slots) = @_;
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
    }));#;$kg=bless({$w,$jg,$y,$z},$A);$lg={$ig,$kg};$mg=q#/lib/definition_init_with_defaults.b#;$ng=bless({$r1,$hg,$W2,$q,$X2,$q,$Y2,$lg,$Q,$mg},$x2);$og=[$zf,$Jf,$Qf,$Xf,$gg,$ng];$pg=bless({$r1,$rf,$Q,$sf,$v1,$og},$h2);$qg=[$kf,$qf,$k3,$md,$pg];$rg=bless({$r1,$Ae,$Q,$Be,$v1,$qg},$M2);$sg={};$tg=q#new#;$ug=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$vg=bless({$w,$ug,$y,$z},$A);$wg={$tg,$vg};$xg=q#/lib/instantiable.b#;$yg=bless({$r1,$sg,$Y2,$wg,$Q,$xg},$x2);$zg={};$Ag=q#child#;$Bg=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$Cg=bless({$w,$Bg,$y,$z},$A);$Dg={$Ag,$Cg};$Eg=q#/lib/subclass.b#;$Fg=bless({$r1,$zg,$W2,$q,$X2,$q,$Y2,$Dg,$Q,$Eg},$x2);$Gg=[$rg,$yg,$qf,$rg,$Fg];$Hg=bless({$r1,$ze,$Q,$R,$v1,$Gg},$J1);$Ig=q#ni:/class.c#;$Jg={$J1,1,$Q2,1};$Kg=q#/class.c#;$Lg={$J1,1,$M2,1,$Q2,1};$Mg=q#/module.c#;$Ng={$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$M2,1,$O2,1,$Q2,1,$S2,1};$Og=q#/object.c#;$Pg=[$Hg];$Qg=bless({$r1,$Ng,$Q,$Og,$v1,$Pg},$J2);$Rg={$J1,1,$g2,1,$i2,1,$k2,1,$y2,1,$A2,1,$M2,1,$Q2,1};$Sg=q#/lib/behavior.c#;$Tg=[$Qg];$Ug=bless({$r1,$Rg,$Q,$Sg,$v1,$Tg},$J2);$Vg=[$Qg,$yg,$Ug];$Wg=bless({$r1,$Lg,$Q,$Mg,$v1,$Vg},$J2);$Xg=[$Wg];$Yg=bless({$r1,$Jg,$Q,$Kg,$v1,$Xg},$J2);$Zg=q#ni:/fabric#;$ch=q#fabric#;$dh={$ch,1};$eh=[];$fh=bless({$r1,$dh,$Q,$d1,$v1,$eh},$L2);$gh=q#ni:/fabric/remote#;$hh={$K1,1};$ih={};$jh=[];$kh=q#my ($class, $rmi, $name) = @_;
+{rmi  => $rmi,
  name => $name};#;$lh=bless({$t,$jh,$v,$q,$w,$kh,$y,$z},$A);$mh={$L4,$lh};$nh=q#/fabric/remote_init.b#;$oh=bless({$r1,$ih,$W2,$q,$X2,$q,$Y2,$mh,$Q,$nh},$x2);$ph={};$qh=q#AUTOLOAD#;$rh=[];$sh=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{rmi}->call($$self{name}, $method, @_);#;$th=bless({$t,$rh,$v,$q,$w,$sh,$y,$z},$A);$uh={$qh,$th};$vh=q#/fabric/remote_proxy.b#;$wh=bless({$r1,$ph,$W2,$q,$X2,$q,$Y2,$uh,$Q,$vh},$x2);$xh=[$k3,$oh,$wh];$yh=bless({$r1,$hh,$Q,$j1,$v1,$xh},$L1);$zh=q#ni:/fabric/remote.c#;$Ah={$L1,1};$Bh=q#/fabric/remote.c#;$Ch=[$Qg];$Dh=bless({$r1,$Ah,$Q,$Bh,$v1,$Ch},$J2);$Eh=q#ni:/fabric/remote_init.b#;$Fh=q#ni:/fabric/remote_proxy.b#;$Gh=q#ni:/fabric/rmi#;$Hh=q#ni:/fabric/rmi.c#;$Ih={$M1,1};$Jh=q#/fabric/rmi.c#;$Kh={$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1};$Lh=q#/io/object.c#;$Mh={};$Nh=q#def_transfer_method#;$Oh=[];$Ph=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Qh=bless({$t,$Oh,$v,$q,$w,$Ph,$y,$z},$A);$Rh={$Nh,$Qh};$Sh=q#/io/object.c_transfer_def.b#;$Th=bless({$r1,$Mh,$W2,$q,$X2,$q,$Y2,$Rh,$Q,$Sh},$x2);$Uh=[$Qg,$Th];$Vh=bless({$r1,$Kh,$Q,$Lh,$v1,$Uh},$J2);$Wh=[$Vh];$Xh=bless({$r1,$Ih,$Q,$Jh,$v1,$Wh},$J2);$Yh=q#ni:/fabric/rmi_init.b#;$Zh=q#ni:/io#;$ci={$oa,1};$di=[];$ei=bless({$r1,$ci,$Q,$q5,$v1,$di},$L2);$fi=q#ni:/io/buffer#;$gi={$w1,1};$hi={};$ii=[];$ji=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$ki=bless({$t,$ii,$v,$q,$w,$ji,$y,$z},$A);$li={$L4,$ki};$mi=q#/io/buffer_init.b#;$ni=bless({$r1,$hi,$W2,$q,$X2,$q,$Y2,$li,$Q,$mi},$x2);$oi={};$pi=[];$qi=q#my $self = shift;
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
}#;$ri=bless({$t,$pi,$v,$q,$w,$qi,$y,$z},$A);$si=q#read_capacity#;$ti=[];$ui=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$vi=bless({$t,$ti,$v,$q,$w,$ui,$y,$z},$A);$wi=[];$xi=q#my $self = shift;
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
}#;$yi=bless({$t,$wi,$v,$q,$w,$xi,$y,$z},$A);$zi=q#write_capacity#;$Ai=[];$Bi=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Ci=bless({$t,$Ai,$v,$q,$w,$Bi,$y,$z},$A);$Di={$T7,$ri,$si,$vi,$X7,$yi,$zi,$Ci};$Ei=q#/io/buffer_io.b#;$Fi=bless({$r1,$oi,$W2,$q,$X2,$q,$Y2,$Di,$Q,$Ei},$x2);$Gi=[$J4,$ni,$Fi];$Hi=bless({$r1,$gi,$Q,$D5,$v1,$Gi},$N1);$Ii=q#ni:/io/buffer.c#;$Ji={$N1,1};$Ki=q#/io/buffer.c#;$Li=[$Vh];$Mi=bless({$r1,$Ji,$Q,$Ki,$v1,$Li},$J2);$Ni=q#ni:/io/buffer_init.b#;$Oi=q#ni:/io/buffer_io.b#;$Pi=q#ni:/io/cat#;$Qi={$x1,1};$Ri={};$Si=[];$Ti=q#shift; +{fs => [@_]}#;$Ui=bless({$t,$Si,$v,$q,$w,$Ti,$y,$z},$A);$Vi={$L4,$Ui};$Wi=q#/io/cat_init.b#;$Xi=bless({$r1,$Ri,$W2,$q,$X2,$q,$Y2,$Vi,$Q,$Wi},$x2);$Yi={};$Zi=[];$cj=q#my $fs = shift->{fs};
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
$total_read;#;$dj=bless({$t,$Zi,$v,$q,$w,$cj,$y,$z},$A);$ej={$T7,$dj};$fj=q#/io/cat_read.b#;$gj=bless({$r1,$Yi,$W2,$q,$X2,$q,$Y2,$ej,$Q,$fj},$x2);$hj=[$J4,$Xi,$gj];$ij=bless({$r1,$Qi,$Q,$Q5,$v1,$hj},$O1);$jj=q#ni:/io/cat.c#;$kj={$O1,1};$lj=q#/io/cat.c#;$mj=[$Vh];$nj=bless({$r1,$kj,$Q,$lj,$v1,$mj},$J2);$oj=q#ni:/io/cat_init.b#;$pj=q#ni:/io/cat_read.b#;$qj=q#ni:/io/exec#;$rj={$y1,1};$sj={};$tj=q#argv#;$uj=[];$vj=q#shift->{'argv'}#;$wj=bless({$t,$uj,$v,$q,$w,$vj,$y,$z},$A);$xj={$tj,$wj};$yj=q#/io/exec_ro.b#;$zj=bless({$r1,$sj,$W2,$q,$X2,$q,$Y2,$xj,$Q,$yj},$x2);$Aj={};$Bj=[];$Cj=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Dj=bless({$t,$Bj,$v,$q,$w,$Cj,$y,$z},$A);$Ej={$L4,$Dj};$Fj=q#/io/exec_init.b#;$Gj=bless({$r1,$Aj,$W2,$q,$X2,$q,$Y2,$Ej,$Q,$Fj},$x2);$Hj={};$Ij=q#connect#;$Jj=[];$Kj=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Lj=bless({$t,$Jj,$v,$q,$w,$Kj,$y,$z},$A);$Mj=q#in_pipe#;$Nj=[];$Oj=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Pj=bless({$t,$Nj,$v,$q,$w,$Oj,$y,$z},$A);$Qj=q#out_pipe#;$Rj=[];$Sj=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Tj=bless({$t,$Rj,$v,$q,$w,$Sj,$y,$z},$A);$Uj=q#setup_stdio#;$Vj=[];$Wj=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Xj=bless({$t,$Vj,$v,$q,$w,$Wj,$y,$z},$A);$Yj={$Ij,$Lj,$Mj,$Pj,$Qj,$Tj,$Uj,$Xj};$Zj=q#/io/exec_io_setup.b#;$ck=bless({$r1,$Hj,$W2,$q,$X2,$q,$Y2,$Yj,$Q,$Zj},$x2);$dk={};$ek=q#binds_fd#;$fk=[];$gk=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$hk=bless({$t,$fk,$v,$q,$w,$gk,$y,$z},$A);$ik=[];$jk=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$kk=bless({$t,$ik,$v,$q,$w,$jk,$y,$z},$A);$lk=[];$mk=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$nk=bless({$t,$lk,$v,$q,$w,$mk,$y,$z},$A);$ok=[];$pk=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$qk=bless({$t,$ok,$v,$q,$w,$pk,$y,$z},$A);$rk=[];$sk=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$tk=bless({$t,$rk,$v,$q,$w,$sk,$y,$z},$A);$uk={$ek,$hk,$h8,$kk,$l8,$nk,$p8,$qk,$t8,$tk};$vk=q#/io/exec_io_accessors.b#;$wk=bless({$r1,$dk,$W2,$q,$X2,$q,$Y2,$uk,$Q,$vk},$x2);$xk={};$yk=q#env#;$zk=[];$Ak=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Bk=bless({$t,$zk,$v,$q,$w,$Ak,$y,$z},$A);$Ck={$yk,$Bk};$Dk=q#/io/exec_env.b#;$Ek=bless({$r1,$xk,$W2,$q,$X2,$q,$Y2,$Ck,$Q,$Dk},$x2);$Fk={};$Gk=q#exec#;$Hk=[];$Ik=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Jk=bless({$t,$Hk,$v,$q,$w,$Ik,$y,$z},$A);$Kk=q#fork#;$Lk=[];$Mk=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Nk=bless({$t,$Lk,$v,$q,$w,$Mk,$y,$z},$A);$Ok=q#move_fds#;$Pk=[];$Qk=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Rk=bless({$t,$Pk,$v,$q,$w,$Qk,$y,$z},$A);$Sk={$Gk,$Jk,$Kk,$Nk,$Ok,$Rk};$Tk=q#/io/exec_fork.b#;$Uk=bless({$r1,$Fk,$W2,$q,$X2,$q,$Y2,$Sk,$Q,$Tk},$x2);$Vk=[$J4,$zj,$Gj,$ck,$wk,$Ek,$Uk];$Wk=bless({$r1,$rj,$Q,$f6,$v1,$Vk},$P1);$Xk=q#ni:/io/exec.c#;$Yk={$P1,1};$Zk=q#/io/exec.c#;$cl=[$Vh];$dl=bless({$r1,$Yk,$Q,$Zk,$v1,$cl},$J2);$el=q#ni:/io/exec_env.b#;$fl=q#ni:/io/exec_fork.b#;$gl=q#ni:/io/exec_init.b#;$hl=q#ni:/io/exec_io_accessors.b#;$il=q#ni:/io/exec_io_setup.b#;$jl=q#ni:/io/exec_ro.b#;$kl=q#ni:/io/fd#;$ll={$z1,1};$ml=q#read_fd_mask#;$nl={};$ol=[];$pl=q#shift->{'fd'}#;$ql=bless({$t,$ol,$v,$q,$w,$pl,$y,$z},$A);$rl={$h8,$ql};$sl=q#/io/fd_readers.b#;$tl=bless({$r1,$nl,$W2,$q,$X2,$q,$Y2,$rl,$Q,$sl},$x2);$ul={};$vl=[];$wl=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$xl=bless({$t,$vl,$v,$q,$w,$wl,$y,$z},$A);$yl={$L4,$xl};$zl=q#/io/fd_init.b#;$Al=bless({$r1,$ul,$W2,$q,$X2,$q,$Y2,$yl,$Q,$zl},$x2);$Bl={};$Cl=q#be#;$Dl=[];$El=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Fl=bless({$t,$Dl,$v,$q,$w,$El,$y,$z},$A);$Gl={$Cl,$Fl};$Hl=q#/io/fd_shell.b#;$Il=bless({$r1,$Bl,$W2,$q,$X2,$q,$Y2,$Gl,$Q,$Hl},$x2);$Jl={};$Kl=q#cloexec#;$Ll=[];$Ml=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Nl=bless({$t,$Ll,$v,$q,$w,$Ml,$y,$z},$A);$Ol=q#fcntl_flag#;$Pl=[];$Ql=q#my ($self, $flag, $value) = @_;
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
}#;$Rl=bless({$t,$Pl,$v,$q,$w,$Ql,$y,$z},$A);$Sl=q#nonblock#;$Tl=[];$Ul=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Vl=bless({$t,$Tl,$v,$q,$w,$Ul,$y,$z},$A);$Wl={$Kl,$Nl,$Ol,$Rl,$Sl,$Vl};$Xl=q#/io/fd_fcntl.b#;$Yl=bless({$r1,$Jl,$W2,$q,$X2,$q,$Y2,$Wl,$Q,$Xl},$x2);$Zl={};$cm=[];$dm=q#shift->close#;$em=bless({$t,$cm,$v,$q,$w,$dm,$y,$z},$A);$fm=q#close#;$gm=[];$hm=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$im=bless({$t,$gm,$v,$q,$w,$hm,$y,$z},$A);$jm={$fm,$im};$km=q#/io/fd_gc.b#;$lm=bless({$r1,$Zl,$W2,$q,$X2,$em,$Y2,$jm,$Q,$km},$x2);$mm={};$nm=q#close_perl_ios#;$om=[];$pm=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$qm=bless({$t,$om,$v,$q,$w,$pm,$y,$z},$A);$rm=[];$sm=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$tm=bless({$t,$rm,$v,$q,$w,$sm,$y,$z},$A);$um=[];$vm=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$wm=bless({$t,$um,$v,$q,$w,$vm,$y,$z},$A);$xm={$nm,$qm,$T7,$tm,$X7,$wm};$ym=q#/io/fd_perlio.b#;$zm=bless({$r1,$mm,$W2,$q,$X2,$q,$Y2,$xm,$Q,$ym},$x2);$Am=[$J4,$tl,$Al,$Il,$Yl,$lm,$zm];$Bm=q#write_fd_mask#;$Cm=bless({$r1,$ll,$Q,$s6,$ml,$z,$v1,$Am,$Bm,$z},$Q1);$Dm=[];$Em=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Fm=bless({$t,$Dm,$v,$q,$w,$Em,$y,$z},$A);$Gm=q#ni:/io/fd.c#;$Hm={$Q1,1};$Im=q#/io/fd.c#;$Jm={};$Km=q#clear_fd#;$Lm=[];$Mm=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Nm=bless({$t,$Lm,$v,$q,$w,$Mm,$y,$z},$A);$Om=q#read_fd#;$Pm=[];$Qm=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Rm=bless({$t,$Pm,$v,$q,$w,$Qm,$y,$z},$A);$Sm=q#select#;$Tm=[];$Um=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Vm=bless({$t,$Tm,$v,$q,$w,$Um,$y,$z},$A);$Wm=q#write_fd#;$Xm=[];$Ym=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Zm=bless({$t,$Xm,$v,$q,$w,$Ym,$y,$z},$A);$cn={$Km,$Nm,$Om,$Rm,$Sm,$Vm,$Wm,$Zm};$dn=q#/io/fd.c_selector.b#;$en=bless({$r1,$Jm,$W2,$Fm,$X2,$q,$Y2,$cn,$Q,$dn},$x2);$fn=[$Vh,$en];$gn=bless({$r1,$Hm,$Q,$Im,$v1,$fn},$J2);$hn=q#ni:/io/fd.c_selector.b#;$in=q#ni:/io/fd_fcntl.b#;$jn=q#ni:/io/fd_gc.b#;$kn=q#ni:/io/fd_init.b#;$ln=q#ni:/io/fd_perlio.b#;$mn=q#ni:/io/fd_readers.b#;$nn=q#ni:/io/fd_shell.b#;$on=q#ni:/io/file#;$pn={$A1,1};$qn={};$rn=[];$sn=q#shift->{'name'}#;$tn=bless({$t,$rn,$v,$q,$w,$sn,$y,$z},$A);$un={$Q,$tn};$vn=q#/io/file_readers.b#;$wn=bless({$r1,$qn,$W2,$q,$X2,$q,$Y2,$un,$Q,$vn},$x2);$xn={};$yn=q#mode#;$zn=[];$An=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$Bn=bless({$t,$zn,$v,$q,$w,$An,$y,$z},$A);$Cn={$yn,$Bn};$Dn=q#/io/file_accessors.b#;$En=bless({$r1,$xn,$W2,$q,$X2,$q,$Y2,$Cn,$Q,$Dn},$x2);$Fn={};$Gn=[];$Hn=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$In=bless({$t,$Gn,$v,$q,$w,$Hn,$y,$z},$A);$Jn={$L4,$In};$Kn=q#/io/file_init.b#;$Ln=bless({$r1,$Fn,$W2,$q,$X2,$q,$Y2,$Jn,$Q,$Kn},$x2);$Mn={};$Nn=q#(-X#;$On=[];$Pn=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Qn=bless({$t,$On,$v,$q,$w,$Pn,$y,$z},$A);$Rn=q#mv#;$Sn=[];$Tn=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Un=bless({$t,$Sn,$v,$q,$w,$Tn,$y,$z},$A);$Vn=q#rm#;$Wn=[];$Xn=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Yn=bless({$t,$Wn,$v,$q,$w,$Xn,$y,$z},$A);$Zn={$Nn,$Qn,$Rn,$Un,$Vn,$Yn};$co=q#/io/file_fns.b#;$do=bless({$r1,$Mn,$W2,$q,$X2,$q,$Y2,$Zn,$Q,$co},$x2);$eo={};$fo=q#atomic_update#;$go=[];$ho=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$io=bless({$t,$go,$v,$q,$w,$ho,$y,$z},$A);$jo={$fo,$io};$ko=q#/io/file_update.b#;$lo=bless({$r1,$eo,$W2,$q,$X2,$q,$Y2,$jo,$Q,$ko},$x2);$mo={};$no=[];$oo=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$po=bless({$t,$no,$v,$q,$w,$oo,$y,$z},$A);$qo=q#r#;$ro=[];$so=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$to=bless({$t,$ro,$v,$q,$w,$so,$y,$z},$A);$uo=[];$vo=q#shift->r->read(@_)#;$wo=bless({$t,$uo,$v,$q,$w,$vo,$y,$z},$A);$xo=q#w#;$yo=[];$zo=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Ao=bless({$t,$yo,$v,$q,$w,$zo,$y,$z},$A);$Bo=[];$Co=q#shift->w->write(@_)#;$Do=bless({$t,$Bo,$v,$q,$w,$Co,$y,$z},$A);$Eo={$fm,$po,$qo,$to,$T7,$wo,$xo,$Ao,$X7,$Do};$Fo=q#/io/file_io.b#;$Go=bless({$r1,$mo,$W2,$q,$X2,$q,$Y2,$Eo,$Q,$Fo},$x2);$Ho=[$J4,$wn,$En,$Ln,$do,$lo,$Go];$Io=bless({$r1,$pn,$Q,$O6,$v1,$Ho},$R1);$Jo=q#ni:/io/file.c#;$Ko={$R1,1};$Lo=q#/io/file.c#;$Mo=[$Vh];$No=bless({$r1,$Ko,$Q,$Lo,$v1,$Mo},$J2);$Oo=q#ni:/io/file_accessors.b#;$Po=q#ni:/io/file_fns.b#;$Qo=q#ni:/io/file_init.b#;$Ro=q#ni:/io/file_io.b#;$So=q#ni:/io/file_readers.b#;$To=q#ni:/io/file_update.b#;$Uo=q#ni:/io/file_update_fd#;$Vo={$B1,1};$Wo={};$Xo=[];$Yo=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Zo=bless({$t,$Xo,$v,$q,$w,$Yo,$y,$z},$A);$cp={$L4,$Zo};$dp=q#/io/file_update_fd_fd_init.b#;$ep=bless({$r1,$Wo,$W2,$q,$X2,$q,$Y2,$cp,$Q,$dp},$x2);$fp={};$gp=[];$hp=bless({$t,$gp,$v,$q,$w,$dm,$y,$z},$A);$ip=[];$jp=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$kp=bless({$t,$ip,$v,$q,$w,$jp,$y,$z},$A);$lp={$fm,$kp};$mp=q#/io/file_update_fd_fd_gc.b#;$np=bless({$r1,$fp,$W2,$q,$X2,$hp,$Y2,$lp,$Q,$mp},$x2);$op=[$J4,$tl,$Yl,$zm,$ep,$np];$pp=bless({$r1,$Vo,$Q,$U6,$v1,$op},$S1);$qp=q#ni:/io/file_update_fd.c#;$rp={$S1,1};$sp=q#/io/file_update_fd.c#;$tp=[$Vh];$up=bless({$r1,$rp,$Q,$sp,$v1,$tp},$J2);$vp=q#ni:/io/file_update_fd_fd_gc.b#;$wp=q#ni:/io/file_update_fd_fd_init.b#;$xp=q#ni:/io/named_io_fns.b#;$yp={};$zp=q#fcntl#;$Ap=[];$Bp=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Cp=bless({$t,$Ap,$v,$q,$w,$Bp,$y,$z},$A);$Dp=[];$Ep=q#CORE::fork#;$Fp=bless({$t,$Dp,$v,$q,$w,$Ep,$y,$z},$A);$Gp=q#open2#;$Hp=[];$Ip=q#CORE::open $_[0], $_[1]#;$Jp=bless({$t,$Hp,$v,$q,$w,$Ip,$y,$z},$A);$Kp=q#rename#;$Lp=[];$Mp=q#CORE::rename $_[0], $_[1]#;$Np=bless({$t,$Lp,$v,$q,$w,$Mp,$y,$z},$A);$Op=q#unlink#;$Pp=[];$Qp=q#CORE::unlink @_#;$Rp=bless({$t,$Pp,$v,$q,$w,$Qp,$y,$z},$A);$Sp=q#waitpid#;$Tp=[];$Up=q#CORE::waitpid $_[0], $_[1]#;$Vp=bless({$t,$Tp,$v,$q,$w,$Up,$y,$z},$A);$Wp={$zp,$Cp,$Kk,$Fp,$Gp,$Jp,$Kp,$Np,$Op,$Rp,$Sp,$Vp};$Xp=q#/io/named_io_fns.b#;$Yp=bless({$r1,$yp,$W2,$q,$X2,$q,$Y2,$Wp,$Q,$Xp},$x2);$Zp=q#main#;$cq=q#ni:/io/null#;$dq={$C1,1};$eq=q#/io/null#;$fq={};$gq=[];$hq=q#+{fd => undef}#;$iq=bless({$t,$gq,$v,$q,$w,$hq,$y,$z},$A);$jq={$L4,$iq};$kq=q#/io/null_init.b#;$lq=bless({$r1,$fq,$W2,$q,$X2,$q,$Y2,$jq,$Q,$kq},$x2);$mq={};$nq=[];$oq=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$pq=bless({$t,$nq,$v,$q,$w,$oq,$y,$z},$A);$qq=[];$rq=q#shift->fd->read(@_)#;$sq=bless({$t,$qq,$v,$q,$w,$rq,$y,$z},$A);$tq=[];$uq=q#shift->fd->write(@_)#;$vq=bless({$t,$tq,$v,$q,$w,$uq,$y,$z},$A);$wq={$h8,$pq,$T7,$sq,$X7,$vq};$xq=q#/io/null_io.b#;$yq=bless({$r1,$mq,$W2,$q,$X2,$q,$Y2,$wq,$Q,$xq},$x2);$zq=[$J4,$lq,$yq];$Aq=bless({$r1,$dq,$Q,$eq,$v1,$zq},$T1);$Bq=q#ni:/io/null.c#;$Cq={$T1,1};$Dq=q#/io/null.c#;$Eq=[$Vh];$Fq=bless({$r1,$Cq,$Q,$Dq,$v1,$Eq},$J2);$Gq=q#ni:/io/null_init.b#;$Hq=q#ni:/io/null_io.b#;$Iq=q#ni:/io/object#;$Jq=q#ni:/io/object.c#;$Kq=q#ni:/io/object.c_transfer_def.b#;$Lq=q#ni:/io/object_checks.b#;$Mq=q#ni:/io/object_constructors.b#;$Nq=q#ni:/io/object_memory.b#;$Oq=q#ni:/io/object_ops.b#;$Pq=q#ni:/io/object_transfer_async.b#;$Qq=q#ni:/io/object_transfer_sync.b#;$Rq=q#ni:/io/pid#;$Sq=q#ni:/io/pid.c#;$Tq={$V1,1};$Uq=q#/io/pid.c#;$Vq=[$Vh];$Wq=bless({$r1,$Tq,$Q,$Uq,$v1,$Vq},$J2);$Xq=q#ni:/io/pid_accessors.b#;$Yq=q#ni:/io/pid_init.b#;$Zq=q#ni:/io/pid_io.b#;$cr=q#ni:/io/pid_readers.b#;$dr=q#ni:/io/pid_wait.b#;$er=q#ni:/io/str#;$fr={$F1,1};$gr=q#/io/str#;$hr={};$ir=q#data#;$jr=[];$kr=q#shift->{'data'}#;$lr=bless({$t,$jr,$v,$q,$w,$kr,$y,$z},$A);$mr=q#end#;$nr=[];$or=q#shift->{'end'}#;$pr=bless({$t,$nr,$v,$q,$w,$or,$y,$z},$A);$qr=q#start#;$rr=[];$sr=q#shift->{'start'}#;$tr=bless({$t,$rr,$v,$q,$w,$sr,$y,$z},$A);$ur={$ir,$lr,$mr,$pr,$qr,$tr};$vr=q#/io/str_ro.b#;$wr=bless({$r1,$hr,$W2,$q,$X2,$q,$Y2,$ur,$Q,$vr},$x2);$xr={};$yr=[];$zr=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Ar=bless({$t,$yr,$v,$q,$w,$zr,$y,$z},$A);$Br={$L4,$Ar};$Cr=q#/io/str_init.b#;$Dr=bless({$r1,$xr,$W2,$q,$X2,$q,$Y2,$Br,$Q,$Cr},$x2);$Er={};$Fr=[];$Gr=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Hr=bless({$t,$Fr,$v,$q,$w,$Gr,$y,$z},$A);$Ir=q#remaining#;$Jr=[];$Kr=q#my $self = shift; $$self{end} - $$self{start}#;$Lr=bless({$t,$Jr,$v,$q,$w,$Kr,$y,$z},$A);$Mr=[];$Nr=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Or=bless({$t,$Mr,$v,$q,$w,$Nr,$y,$z},$A);$Pr={$T7,$Hr,$Ir,$Lr,$X7,$Or};$Qr=q#/io/str_io.b#;$Rr=bless({$r1,$Er,$W2,$q,$X2,$q,$Y2,$Pr,$Q,$Qr},$x2);$Sr=[$J4,$wr,$Dr,$Rr];$Tr=bless({$r1,$fr,$Q,$gr,$v1,$Sr},$W1);$Ur=q#ni:/io/str.c#;$Vr={$W1,1};$Wr=q#/io/str.c#;$Xr=[$Vh];$Yr=bless({$r1,$Vr,$Q,$Wr,$v1,$Xr},$J2);$Zr=q#ni:/io/str_init.b#;$cs=q#ni:/io/str_io.b#;$ds=q#ni:/io/str_ro.b#;$es=q#ni:/io/transfer#;$fs={$X1,1,$Z1,1,$d2,1};$gs=q#/io/transfer#;$hs={$X1,1,$Z1,1,$d2,1,$R2,1};$is=q#/semantic/task#;$js={};$ks=[];$ls=q#shift->{'outcome'}#;$ms=bless({$t,$ks,$v,$q,$w,$ls,$y,$z},$A);$ns={$r,$ms};$os=q#/semantic/task_ro.b#;$ps=bless({$r1,$js,$W2,$q,$X2,$q,$Y2,$ns,$Q,$os},$x2);$qs={};$rs=q#failure#;$ss=[];$ts=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$us=bless({$t,$ss,$v,$q,$w,$ts,$y,$z},$A);$vs=q#success#;$ws=[];$xs=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$ys=bless({$t,$ws,$v,$q,$w,$xs,$y,$z},$A);$zs={$rs,$us,$vs,$ys};$As=q#/semantic/task_outcome.b#;$Bs=bless({$r1,$qs,$W2,$q,$X2,$q,$Y2,$zs,$Q,$As},$x2);$Cs=[$k3,$ps,$Bs];$Ds=bless({$r1,$hs,$Q,$is,$v1,$Cs},$S2);$Es={};$Fs=[];$Gs=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Hs=bless({$t,$Fs,$v,$q,$w,$Gs,$y,$z},$A);$Is=[];$Js=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Ks=bless({$t,$Is,$v,$q,$w,$Js,$y,$z},$A);$Ls=[];$Ms=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Ns=bless({$t,$Ls,$v,$q,$w,$Ms,$y,$z},$A);$Os={$T7,$Ks,$X7,$Ns};$Ps=q#/io/transfer_io_interop.b#;$Qs=bless({$r1,$Es,$W2,$Hs,$X2,$q,$Y2,$Os,$Q,$Ps},$x2);$Rs={};$Ss=q#pressure#;$Ts=[];$Us=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Vs=bless({$t,$Ts,$v,$q,$w,$Us,$y,$z},$A);$Ws=q#read_limit_throughput#;$Xs=[];$Ys=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Zs=bless({$t,$Xs,$v,$q,$w,$Ys,$y,$z},$A);$ct=q#throughput#;$dt=[];$et=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$ft=bless({$t,$dt,$v,$q,$w,$et,$y,$z},$A);$gt=q#write_limit_throughput#;$ht=[];$it=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$jt=bless({$t,$ht,$v,$q,$w,$it,$y,$z},$A);$kt={$Ss,$Vs,$Ws,$Zs,$ct,$ft,$gt,$jt};$lt=q#/io/transfer_io_measurement.b#;$mt=bless({$r1,$Rs,$W2,$q,$X2,$q,$Y2,$kt,$Q,$lt},$x2);$nt=[$Ds,$Qs,$mt];$ot=bless({$r1,$fs,$Q,$gs,$v1,$nt},$Y1);$pt=[];$qt=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$rt=bless({$t,$pt,$v,$q,$w,$qt,$y,$z},$A);$st=q#ni:/io/transfer.c#;$tt={$Y1,1,$c2,1,$e2,1};$ut=q#/io/transfer.c#;$vt={$Y1,1,$c2,1,$e2,1,$S2,1};$wt=q#/semantic/task.c#;$xt=[$Qg];$yt=bless({$r1,$vt,$Q,$wt,$v1,$xt},$J2);$zt={};$At={};$Bt=q#/io/transfer.c_into.b#;$Ct=bless({$r1,$zt,$W2,$rt,$X2,$q,$Y2,$At,$Q,$Bt},$x2);$Dt=[$yt,$Ct];$Et=bless({$r1,$tt,$Q,$ut,$v1,$Dt},$J2);$Ft=q#ni:/io/transfer.c_into.b#;$Gt=q#ni:/io/transfer_async#;$Ht={$Z1,1};$It=q#/io/transfer_async#;$Jt={};$Kt=q#dest_io#;$Lt=[];$Mt=q#shift->{'dest_io'}#;$Nt=bless({$t,$Lt,$v,$q,$w,$Mt,$y,$z},$A);$Ot=q#id#;$Pt=[];$Qt=q#shift->{'id'}#;$Rt=bless({$t,$Pt,$v,$q,$w,$Qt,$y,$z},$A);$St=q#source_io#;$Tt=[];$Ut=q#shift->{'source_io'}#;$Vt=bless({$t,$Tt,$v,$q,$w,$Ut,$y,$z},$A);$Wt={$Kt,$Nt,$Ot,$Rt,$St,$Vt};$Xt=q#/io/transfer_async_ro.b#;$Yt=bless({$r1,$Jt,$W2,$q,$X2,$q,$Y2,$Wt,$Q,$Xt},$x2);$Zt={};$cu=[];$du=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$eu=bless({$t,$cu,$v,$q,$w,$du,$y,$z},$A);$fu={$L4,$eu};$gu=q#/io/transfer_async_init.b#;$hu=bless({$r1,$Zt,$W2,$q,$X2,$q,$Y2,$fu,$Q,$gu},$x2);$iu={};$ju=[];$ku=q#ni('ni:/io/transfer_async')->track(shift)#;$lu=bless({$t,$ju,$v,$q,$w,$ku,$y,$z},$A);$mu=[];$nu=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$ou=bless({$t,$mu,$v,$q,$w,$nu,$y,$z},$A);$pu={};$qu=q#/io/transfer_async_lifecycle.b#;$ru=bless({$r1,$iu,$W2,$lu,$X2,$ou,$Y2,$pu,$Q,$qu},$x2);$su={};$tu=q#run#;$uu=[];$vu=q#shift#;$wu=bless({$t,$uu,$v,$q,$w,$vu,$y,$z},$A);$xu=q#run_async#;$yu=[];$zu=q#my $self = shift;
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

$self;#;$Au=bless({$t,$yu,$v,$q,$w,$zu,$y,$z},$A);$Bu={$tu,$wu,$xu,$Au};$Cu=q#/io/transfer_async_run.b#;$Du=bless({$r1,$su,$W2,$q,$X2,$q,$Y2,$Bu,$Q,$Cu},$x2);$Eu=[$ot,$Yt,$hu,$ru,$Du];$Fu=q#tracked_transfers#;$Gu={};$Hu=q#transfer_id#;$Iu=bless({$r1,$Ht,$Q,$It,$v1,$Eu,$Fu,$Gu,$Hu,0},$c2);$Ju=[];$Ku=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Lu=bless({$t,$Ju,$v,$q,$w,$Ku,$y,$z},$A);$Mu=q#ni:/io/transfer_async.c#;$Nu={$c2,1};$Ou=q#/io/transfer_async.c#;$Pu={};$Qu=q#new_id#;$Ru=[];$Su=q#++shift->{transfer_id}#;$Tu=bless({$t,$Ru,$v,$q,$w,$Su,$y,$z},$A);$Uu=q#track#;$Vu=[];$Wu=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Xu=bless({$t,$Vu,$v,$q,$w,$Wu,$y,$z},$A);$Yu=q#untrack#;$Zu=[];$cv=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$dv=bless({$t,$Zu,$v,$q,$w,$cv,$y,$z},$A);$ev={$Qu,$Tu,$Uu,$Xu,$Yu,$dv};$fv=q#/io/transfer_async.c_tracker.b#;$gv=bless({$r1,$Pu,$W2,$Lu,$X2,$q,$Y2,$ev,$Q,$fv},$x2);$hv=[$Et,$gv];$iv=bless({$r1,$Nu,$Q,$Ou,$v1,$hv},$J2);$jv=q#ni:/io/transfer_async.c_tracker.b#;$kv=q#ni:/io/transfer_async_init.b#;$lv=q#ni:/io/transfer_async_lifecycle.b#;$mv=q#ni:/io/transfer_async_ro.b#;$nv=q#ni:/io/transfer_async_run.b#;$ov=q#ni:/io/transfer_io_interop.b#;$pv=q#ni:/io/transfer_io_measurement.b#;$qv=q#ni:/io/transfer_sync#;$rv={$d2,1};$sv=q#/io/transfer_sync#;$tv={};$uv=[];$vv=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$wv=bless({$t,$uv,$v,$q,$w,$vv,$y,$z},$A);$xv={$L4,$wv};$yv=q#/io/transfer_sync_init.b#;$zv=bless({$r1,$tv,$W2,$q,$X2,$q,$Y2,$xv,$Q,$yv},$x2);$Av={};$Bv=[];$Cv=q#my $self = shift;
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
$self->success;#;$Dv=bless({$t,$Bv,$v,$q,$w,$Cv,$y,$z},$A);$Ev={$tu,$Dv};$Fv=q#/io/transfer_sync_run.b#;$Gv=bless({$r1,$Av,$W2,$q,$X2,$q,$Y2,$Ev,$Q,$Fv},$x2);$Hv=[$ot,$zv,$Gv];$Iv=bless({$r1,$rv,$Q,$sv,$v1,$Hv},$e2);$Jv=q#ni:/io/transfer_sync.c#;$Kv={$e2,1};$Lv=q#/io/transfer_sync.c#;$Mv=[$Et];$Nv=bless({$r1,$Kv,$Q,$Lv,$v1,$Mv},$J2);$Ov=q#ni:/io/transfer_sync_init.b#;$Pv=q#ni:/io/transfer_sync_run.b#;$Qv=q#ni:/lib#;$Rv=q#lib#;$Sv={$Rv,1};$Tv=[];$Uv=bless({$r1,$Sv,$Q,$c9,$v1,$Tv},$L2);$Vv=q#ni:/lib/accessor.b#;$Wv=q#ni:/lib/behavior#;$Xv=q#ni:/lib/behavior.c#;$Yv=q#ni:/lib/branch#;$Zv={$h2,1};$cw=q#/lib/branch#;$dw={};$ew=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$fw=bless({$w,$ew,$y,$z},$A);$gw={$L4,$fw};$hw=q#/lib/branch_init.b#;$iw=bless({$r1,$dw,$W2,$q,$X2,$q,$Y2,$gw,$Q,$hw},$x2);$jw=[$md,$sd,$Le,$iw,$pg];$kw=bless({$r1,$Zv,$Q,$cw,$v1,$jw},$i2);$lw=q#ni:/lib/branch.b#;$mw=q#ni:/lib/branch.c#;$nw={$i2,1};$ow=q#/lib/branch.c#;$pw=[$Ug];$qw=bless({$r1,$nw,$Q,$ow,$v1,$pw},$J2);$rw=q#ni:/lib/branch_init.b#;$sw=q#ni:/lib/class_init.b#;$tw=q#ni:/lib/dataslice#;$uw={$j2,1};$vw=q#/lib/dataslice#;$ww={};$xw=q#shift->apply_(@_)#;$yw=bless({$w,$xw,$y,$z},$A);$zw=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Aw=bless({$w,$zw,$y,$z},$A);$Bw={$Ad,$yw,$Bd,$Aw};$Cw=q#/lib/dataslice.b#;$Dw=bless({$r1,$ww,$W2,$q,$X2,$q,$Y2,$Bw,$Q,$Cw},$x2);$Ew={};$Fw=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Gw=bless({$w,$Fw,$y,$z},$A);$Hw={$L4,$Gw};$Iw=q#/lib/dataslice_init.b#;$Jw=bless({$r1,$Ew,$W2,$q,$X2,$q,$Y2,$Hw,$Q,$Iw},$x2);$Kw=[$md,$sd,$Dw,$Jw,$Se,$Ze,$if,$Sd];$Lw=bless({$r1,$uw,$Q,$vw,$v1,$Kw},$k2);$Mw=q#ni:/lib/dataslice.b#;$Nw=q#ni:/lib/dataslice.c#;$Ow={$k2,1};$Pw=q#/lib/dataslice.c#;$Qw=[$Ug];$Rw=bless({$r1,$Ow,$Q,$Pw,$v1,$Qw},$J2);$Sw=q#ni:/lib/dataslice_init.b#;$Tw=q#ni:/lib/definition.b#;$Uw=q#ni:/lib/definition_def.b#;$Vw=q#ni:/lib/definition_defdata.b#;$Ww=q#ni:/lib/definition_init_with_defaults.b#;$Xw=q#ni:/lib/doc#;$Yw={$S,1};$Zw={};$cx=q#shift; +{name => shift, doc => []}#;$dx=bless({$w,$cx,$y,$z},$A);$ex={$L4,$dx};$fx=q#/lib/doc_init.b#;$gx=bless({$r1,$Zw,$W2,$q,$X2,$q,$Y2,$ex,$Q,$fx},$x2);$hx={};$ix=q#'ni.doc'#;$jx=bless({$w,$ix,$y,$z},$A);$kx={$Ne,$jx};$lx=q#/lib/doc_namespace.b#;$mx=bless({$r1,$hx,$W2,$q,$X2,$q,$Y2,$kx,$Q,$lx},$x2);$nx={};$ox=q#(@{}#;$px=q#[map @$_, @{shift->{doc}}]#;$qx=bless({$w,$px,$y,$z},$A);$rx=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$sx=bless({$w,$rx,$y,$z},$A);$tx={$ox,$qx,$qh,$sx};$ux=q#/lib/doc_define.b#;$vx=bless({$r1,$nx,$W2,$q,$X2,$q,$Y2,$tx,$Q,$ux},$x2);$wx={};$xx=q#shift->referent#;$yx=bless({$w,$xx,$y,$z},$A);$zx=q#ni 'ni:' . shift->{name}#;$Ax=bless({$w,$zx,$y,$z},$A);$Bx={$mr,$yx,$q1,$Ax};$Cx=q#/lib/doc_end.b#;$Dx=bless({$r1,$wx,$W2,$q,$X2,$q,$Y2,$Bx,$Q,$Cx},$x2);$Ex={};$Fx=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Gx=bless({$w,$Fx,$y,$z},$A);$Hx={$p1,$Gx};$Ix=q#/lib/doc_TODO.b#;$Jx=bless({$r1,$Ex,$W2,$q,$X2,$q,$Y2,$Hx,$Q,$Ix},$x2);$Kx={};$Lx=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Mx=bless({$w,$Lx,$y,$z},$A);$Nx={$g7,$Mx};$Ox=q#/lib/doc_eg.b#;$Px=bless({$r1,$Kx,$W2,$q,$X2,$q,$Y2,$Nx,$Q,$Ox},$x2);$Qx={};$Rx=q#tests#;$Sx=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case', @$self;#;$Tx=bless({$w,$Sx,$y,$z},$A);$Ux=q#todos#;$Vx=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo', @$self;#;$Wx=bless({$w,$Vx,$y,$z},$A);$Xx={$Rx,$Tx,$Ux,$Wx};$Yx=q#/lib/doc_process.b#;$Zx=bless({$r1,$Qx,$W2,$q,$X2,$q,$Y2,$Xx,$Q,$Yx},$x2);$cy=[$k3,$sd,$gx,$mx,$vx,$Dx,$Jx,$Px,$Zx];$dy=bless({$r1,$Yw,$Q,$w9,$v1,$cy},$l2);$ey=q#ni:/lib/doc.c#;$fy={$l2,1};$gy=q#/lib/doc.c#;$hy={};$iy=q#defannotation#;$jy=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$ky=bless({$w,$jy,$y,$z},$A);$ly={$iy,$ky};$my=q#/lib/doc.c_defannotation.b#;$ny=bless({$r1,$hy,$W2,$q,$X2,$q,$Y2,$ly,$Q,$my},$x2);$oy=[$Qg,$ny];$py=bless({$r1,$fy,$Q,$gy,$v1,$oy},$J2);$qy=q#ni:/lib/doc.c_defannotation.b#;$ry=q#ni:/lib/doc_TODO.b#;$sy=q#ni:/lib/doc_define.b#;$ty=q#ni:/lib/doc_eg.b#;$uy=q#ni:/lib/doc_end.b#;$vy=q#ni:/lib/doc_init.b#;$wy=q#ni:/lib/doc_namespace.b#;$xy=q#ni:/lib/doc_process.b#;$yy=q#ni:/lib/documentable.b#;$zy=q#ni:/lib/fn#;$Ay={$A,1};$By=q#/lib/fn#;$Cy=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$Dy=bless({$w,$Cy,$y,$z},$A);$Ey=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$Fy=bless({$w,$Ey},$A);$Gy=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$Hy=bless({$w,$Gy},$A);$Iy=q#lib/fn::closure#;$Jy=q#lib/fn::compile#;$Ky=q#lib/fn::instantiate#;$Ly={};$My=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Ny=bless({$w,$My,$y,$z},$A);$Oy=q#compile#;$Py={$v,$Dy,$Oy,$Fy,$L4,$Hy};$Qy=q#/lib/fn_init.b#;$Ry=bless({$r1,$Ly,$W2,$q,$X2,$Ny,$Y2,$Py,$Q,$Qy},$x2);$Sy={};$Ty=[];$Uy=q#shift->{'annotations'}#;$Vy=bless({$t,$Ty,$v,$q,$w,$Uy,$y,$z},$A);$Wy=[];$Xy=q#shift->{'code'}#;$Yy=bless({$t,$Wy,$v,$q,$w,$Xy,$y,$z},$A);$Zy=q#eval_number#;$cz=[];$dz=q#shift->{'eval_number'}#;$ez=bless({$t,$cz,$v,$q,$w,$dz,$y,$z},$A);$fz=q#fn#;$gz=[];$hz=q#shift->{'fn'}#;$iz=bless({$t,$gz,$v,$q,$w,$hz,$y,$z},$A);$jz={$t,$Vy,$w,$Yy,$Zy,$ez,$fz,$iz};$kz=q#/lib/fn_ro.b#;$lz=bless({$r1,$Sy,$W2,$q,$X2,$q,$Y2,$jz,$Q,$kz},$x2);$mz={};$nz=[];$oz=q#my $self = shift; "fn {$$self{code}}"#;$pz=bless({$t,$nz,$v,$q,$w,$oz,$y,$z},$A);$qz=[];$rz=bless({$t,$qz,$v,$q,$w,$Tf,$y,$z},$A);$sz={$Lf,$pz,$Sf,$rz};$tz=q#/lib/fn_ops.b#;$uz=bless({$r1,$mz,$W2,$q,$X2,$q,$Y2,$sz,$Q,$tz},$x2);$vz={};$wz=[];$xz=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$yz=bless({$t,$wz,$v,$q,$w,$xz,$y,$z},$A);$zz={$Md,$yz};$Az=q#/lib/fn_serialize.b#;$Bz=bless({$r1,$vz,$W2,$q,$X2,$q,$Y2,$zz,$Q,$Az},$x2);$Cz=[$k3,$yg,$Ry,$lz,$uz,$Bz];$Dz=bless({$r1,$Ay,$Q,$By,$v1,$Cz},$m2);$Ez=[];$Fz=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Gz=bless({$t,$Ez,$v,$q,$w,$Fz,$y,$z},$A);$Hz=q#ni:/lib/fn.c#;$Iz={$m2,1};$Jz=q#/lib/fn.c#;$Kz={};$Lz=q#resolve_evals#;$Mz=[];$Nz=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Oz=bless({$t,$Mz,$v,$q,$w,$Nz,$y,$z},$A);$Pz={$Lz,$Oz};$Qz=q#/lib/fn.c_resolve_eval.b#;$Rz=bless({$r1,$Kz,$W2,$Gz,$X2,$q,$Y2,$Pz,$Q,$Qz},$x2);$Sz=[$Qg,$Rz];$Tz=bless({$r1,$Iz,$Q,$Jz,$v1,$Sz},$J2);$Uz=q#ni:/lib/fn.c_resolve_eval.b#;$Vz=q#ni:/lib/fn_init.b#;$Wz=q#ni:/lib/fn_ops.b#;$Xz=q#ni:/lib/fn_ro.b#;$Yz=q#ni:/lib/fn_serialize.b#;$Zz=q#ni:/lib/future#;$cA={$n2,1};$dA={};$eA=[];$fA=bless({$t,$eA,$v,$q,$w,$ls,$y,$z},$A);$gA=q#parents#;$hA=[];$iA=q#shift->{'parents'}#;$jA=bless({$t,$hA,$v,$q,$w,$iA,$y,$z},$A);$kA={$r,$fA,$gA,$jA};$lA=q#/lib/future_ro.b#;$mA=bless({$r1,$dA,$W2,$q,$X2,$q,$Y2,$kA,$Q,$lA},$x2);$nA={};$oA=[];$pA=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$qA=bless({$t,$oA,$v,$q,$w,$pA,$y,$z},$A);$rA={$L4,$qA};$sA=q#/lib/future_init.b#;$tA=bless({$r1,$nA,$W2,$q,$X2,$q,$Y2,$rA,$Q,$sA},$x2);$uA={};$vA=q#decide#;$wA=[];$xA=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$yA=bless({$t,$wA,$v,$q,$w,$xA,$y,$z},$A);$zA=q#decided#;$AA=[];$BA=q#shift->{outcome}#;$CA=bless({$t,$AA,$v,$q,$w,$BA,$y,$z},$A);$DA=q#listener#;$EA=[];$FA=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$GA=bless({$t,$EA,$v,$q,$w,$FA,$y,$z},$A);$HA=q#v#;$IA=[];$JA=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$KA=bless({$t,$IA,$v,$q,$w,$JA,$y,$z},$A);$LA={$vA,$yA,$zA,$CA,$DA,$GA,$HA,$KA};$MA=q#/lib/future_state.b#;$NA=bless({$r1,$uA,$W2,$q,$X2,$q,$Y2,$LA,$Q,$MA},$x2);$OA={};$PA=q#and#;$QA=[];$RA=q#my $self   = $_[0];
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
$child;#;$SA=bless({$t,$QA,$v,$q,$w,$RA,$y,$z},$A);$TA=q#flatmap#;$UA=[];$VA=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$WA=bless({$t,$UA,$v,$q,$w,$VA,$y,$z},$A);$XA=q#map#;$YA=[];$ZA=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$cB=bless({$t,$YA,$v,$q,$w,$ZA,$y,$z},$A);$dB=q#or#;$eB=[];$fB=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$gB=bless({$t,$eB,$v,$q,$w,$fB,$y,$z},$A);$hB={$PA,$SA,$TA,$WA,$XA,$cB,$dB,$gB};$iB=q#/lib/future_algebra.b#;$jB=bless({$r1,$OA,$W2,$q,$X2,$q,$Y2,$hB,$Q,$iB},$x2);$kB=[$k3,$mA,$tA,$NA,$jB];$lB=bless({$r1,$cA,$Q,$N9,$v1,$kB},$o2);$mB=q#ni:/lib/future.c#;$nB={$o2,1};$oB=q#/lib/future.c#;$pB=[$Qg];$qB=bless({$r1,$nB,$Q,$oB,$v1,$pB},$J2);$rB=q#ni:/lib/future_algebra.b#;$sB=q#ni:/lib/future_init.b#;$tB=q#ni:/lib/future_ro.b#;$uB=q#ni:/lib/future_state.b#;$vB=q#ni:/lib/gensym_generator_compact.b#;$wB=q#ni:/lib/global_static_test.b#;$xB={};$yB=[];$zB=q#ni('ni:/lib/test_case')->new(shift)#;$AB=q#($)#;$BB=bless({$t,$yB,$v,$q,$w,$zB,$y,$AB},$A);$CB=q#now#;$DB=[];$EB=q#ni('ni:/lib/test_value')->new(shift)#;$FB=bless({$t,$DB,$v,$q,$w,$EB,$y,$AB},$A);$GB={$g7,$BB,$CB,$FB};$HB=q#/lib/global_static_test.b#;$IB=bless({$r1,$xB,$W2,$q,$X2,$q,$Y2,$GB,$Q,$HB},$x2);$JB=q#ni:/lib/image#;$KB=q#ni:/lib/image.c#;$LB={$q2,1};$MB=q#/lib/image.c#;$NB=[$Qg];$OB=bless({$r1,$LB,$Q,$MB,$v1,$NB},$J2);$PB=q#ni:/lib/image_init.b#;$QB=q#ni:/lib/image_quoting.b#;$RB=q#ni:/lib/instance.b#;$SB=q#ni:/lib/instantiable.b#;$TB=q#ni:/lib/json.b#;$UB={};$VB=q#json_decode#;$WB=[];$XB=q#local $_;
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
wantarray ? @$r : $$r[0];#;$YB=bless({$t,$WB,$v,$q,$w,$XB,$y,$AB},$A);$ZB=q#json_encode#;$cC=[];$dC=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$eC=bless({$t,$cC,$v,$q,$w,$dC,$y,$AB},$A);$fC=q#json_encode_pretty#;$gC=[];$hC=q#local $_;
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

ni::json_encode($v);#;$iC=bless({$t,$gC,$v,$q,$w,$hC,$y,$z},$A);$jC=q#json_escape#;$kC=[];$lC=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/'\\\\' . $ni::json_escapes{$1}/eg;
"\\"$x\\"";#;$mC=bless({$t,$kC,$v,$q,$w,$lC,$y,$AB},$A);$nC=q#json_unescape#;$oC=[];$pC=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$qC=bless({$t,$oC,$v,$q,$w,$pC,$y,$AB},$A);$rC=q#json_unescape_one#;$sC=[];$tC=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$uC=bless({$t,$sC,$v,$q,$w,$tC,$y,$AB},$A);$vC={$VB,$YB,$ZB,$eC,$fC,$iC,$jC,$mC,$nC,$qC,$rC,$uC};$wC=q#/lib/json.b#;$xC=bless({$r1,$UB,$W2,$q,$X2,$q,$Y2,$vC,$Q,$wC},$x2);$yC=q#ni#;$zC=q#ni:/lib/json_data.b#;$AC={};$BC=q#json_escapes#;$CC=q##;$DC=q#b#;$EC=q#	#;$FC=q#t#;$GC=q#
#;$HC=q#n#;$IC=q##;$JC=q#f#;$KC=q##;$LC=q#"#;$MC=q#/#;$NC=q#\\#;$OC={$CC,$DC,$EC,$FC,$GC,$HC,$IC,$JC,$KC,$qo,$LC,$LC,$MC,$MC,$NC,$NC};$PC=q#json_unescapes#;$QC={$LC,$LC,$MC,$MC,$NC,$NC,$DC,$CC,$JC,$IC,$HC,$GC,$qo,$KC,$FC,$EC};$RC={$BC,$OC,$PC,$QC};$SC=q#/lib/json_data.b#;$TC=bless({$r1,$AC,$ir,$RC,$Q,$SC},$j2);$UC=q#ni:/lib/name_as_string.b#;$VC=q#ni:/lib/named.b#;$WC=q#ni:/lib/named_in_ni.b#;$XC=q#ni:/lib/namespaced.b#;$YC=q#ni:/lib/ni#;$ZC={$r2,1};$cD={};$dD=q#extend#;$eD=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$fD=bless({$w,$eD,$y,$z},$A);$gD=q#is_mutable#;$hD=q#$0 ne '-' && -w $0#;$iD=bless({$w,$hD,$y,$z},$A);$jD=q#modify#;$kD=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$lD=bless({$w,$kD,$y,$z},$A);$mD={$dD,$fD,$gD,$iD,$jD,$lD};$nD=q#/lib/ni_self.b#;$oD=bless({$r1,$cD,$W2,$q,$X2,$q,$Y2,$mD,$Q,$nD},$x2);$pD={};$qD=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$rD=bless({$w,$qD,$y,$z},$A);$sD=q#docs#;$tD=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$uD=bless({$w,$tD,$y,$z},$A);$vD=q#metaclasses#;$wD=q#my $self = shift;
map $self->resolve("ni:/$_"),
    grep $self->exists("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$xD=bless({$w,$wD,$y,$z},$A);$yD=q#undocumented#;$zD=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$AD=bless({$w,$zD,$y,$z},$A);$BD=q#untested#;$CD=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$DD=bless({$w,$CD,$y,$z},$A);$ED={$K,$rD,$sD,$uD,$vD,$xD,$yD,$AD,$BD,$DD};$FD=q#/lib/ni_dev_introspection.b#;$GD=bless({$r1,$pD,$W2,$q,$X2,$q,$Y2,$ED,$Q,$FD},$x2);$HD={};$ID=q#--internal/+=#;$JD=q#my $self = shift;
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
exit $self->default(@_);#;$YD=bless({$w,$XD,$y,$z},$A);$ZD={$ID,$KD,$LD,$ND,$OD,$QD,$RD,$TD,$UD,$WD,$tu,$YD};$cE=q#/lib/ni_main.b#;$dE=bless({$r1,$HD,$W2,$q,$X2,$q,$Y2,$ZD,$Q,$cE},$x2);$eE={};$fE=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$gE=bless({$w,$fE,$y,$z},$A);$hE=q#resolver_for#;$iE=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$jE=bless({$w,$iE,$y,$z},$A);$kE={$df,$gE,$hE,$jE};$lE=q#/lib/ni_resolver.b#;$mE=bless({$r1,$eE,$W2,$q,$X2,$q,$Y2,$kE,$Q,$lE},$x2);$nE={};$oE=q#exists#;$pE=q#exists $_[0]->{named}{$_[1]}#;$qE=bless({$w,$pE,$y,$z},$A);$rE=q#quoted#;$sE=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$tE=bless({$w,$sE,$y,$z},$A);$uE={$oE,$qE,$rE,$tE};$vE=q#/lib/ni_image.b#;$wE=bless({$r1,$nE,$W2,$q,$X2,$q,$Y2,$uE,$Q,$vE},$x2);$xE=[$k3,$oD,$GD,$dE,$mE,$wE];$yE=bless({$r1,$ZC,$Q,$Jc,$v1,$xE},$s2);$zE=q#ni:/lib/ni.c#;$AE={$s2,1};$BE=q#/lib/ni.c#;$CE=[$Qg];$DE=bless({$r1,$AE,$Q,$BE,$v1,$CE},$J2);$EE=q#ni:/lib/ni_dev_introspection.b#;$FE=q#ni:/lib/ni_image.b#;$GE=q#ni:/lib/ni_main.b#;$HE=q#ni:/lib/ni_resolver.b#;$IE=q#ni:/lib/ni_self.b#;$JE=q#ni:/lib/ni_static_util.b#;$KE={};$LE=q#abbrev#;$ME=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$NE=bless({$w,$ME,$y,$z},$A);$OE=q#dor#;$PE=q#defined $_[0] ? $_[0] : $_[1]#;$QE=bless({$w,$PE,$y,$z},$A);$RE=q#indent#;$SE=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$TE=bless({$w,$SE,$y,$z},$A);$UE=q#max#;$VE=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$WE=bless({$w,$VE,$y,$z},$A);$XE=q#maxstr#;$YE=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$ZE=bless({$w,$YE,$y,$z},$A);$cF=q#mean#;$dF=q#sum(@_) / (@_ || 1)#;$eF=bless({$w,$dF,$y,$z},$A);$fF=q#min#;$gF=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$hF=bless({$w,$gF,$y,$z},$A);$iF=q#minstr#;$jF=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$kF=bless({$w,$jF,$y,$z},$A);$lF=q#outdent#;$mF=q#my $x = shift;
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
join "\\n", @lines;#;$nF=bless({$w,$mF,$y,$z},$A);$oF=q#sgr#;$pF=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$qF=bless({$w,$pF,$y,$z},$A);$rF=q#sr#;$sF=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$tF=bless({$w,$sF,$y,$z},$A);$uF=q#sum#;$vF=q#local $_; my $x = 0; $x += $_ for @_; $x#;$wF=bless({$w,$vF,$y,$z},$A);$xF=q#swap#;$yF=q#@_[0, 1] = @_[1, 0]#;$zF=bless({$w,$yF,$y,$z},$A);$AF={$LE,$NE,$OE,$QE,$RE,$TE,$UE,$WE,$XE,$ZE,$cF,$eF,$fF,$hF,$iF,$kF,$lF,$nF,$oF,$qF,$rF,$tF,$uF,$wF,$xF,$zF};$BF=q#/lib/ni_static_util.b#;$CF=bless({$r1,$KE,$W2,$q,$X2,$q,$Y2,$AF,$Q,$BF},$x2);$DF=q#ni:/lib/object_metadata#;$EF={$t2,1,$C,1,$H,1};$FF=q#/lib/object_metadata#;$GF={};$HF=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$IF=bless({$w,$HF,$y,$z},$A);$JF={$q1,$IF};$KF=q#/lib/object_metadata_rw.b#;$LF=bless({$r1,$GF,$W2,$q,$X2,$q,$Y2,$JF,$Q,$KF},$x2);$MF=[$k3,$LF];$NF=bless({$r1,$EF,$Q,$FF,$v1,$MF},$u2);$OF=q#ni:/lib/object_metadata.c#;$PF={$u2,1,$F2,1,$I2,1};$QF=q#/lib/object_metadata.c#;$RF=[$Qg];$SF=bless({$r1,$PF,$Q,$QF,$v1,$RF},$J2);$TF=q#ni:/lib/object_metadata_rw.b#;$UF=q#ni:/lib/perlbranch.b#;$VF=q#ni:/lib/quote_circular_addressed.b#;$WF=q#ni:/lib/quote_code_fail.b#;$XF=q#ni:/lib/quote_gensym_identity.b#;$YF=q#ni:/lib/quote_objects.b#;$ZF=q#ni:/lib/quote_simple#;$cG={$v2,1};$dG={};$eG=[];$fG=q#+{}#;$gG=bless({$t,$eG,$v,$q,$w,$fG,$y,$z},$A);$hG={$L4,$gG};$iG=q#/lib/quote_simple_init.b#;$jG=bless({$r1,$dG,$W2,$q,$X2,$q,$Y2,$hG,$Q,$iG},$x2);$kG={};$lG=[];$mG=bless({$t,$lG,$v,$q,$w,0,$y,$z},$A);$nG=[];$oG=q#shift->quote_value(shift)#;$pG=bless({$t,$nG,$v,$q,$w,$oG,$y,$z},$A);$qG={$Kb,$mG,$gc,$pG};$rG=q#/lib/quote_simple_quote.b#;$sG=bless({$r1,$kG,$W2,$q,$X2,$q,$Y2,$qG,$Q,$rG},$x2);$tG=[$k3,$jG,$sG,$Ka,$kb,$Ab];$uG=bless({$r1,$cG,$Q,$Uc,$v1,$tG},$w2);$vG=q#ni:/lib/quote_simple.c#;$wG={$w2,1};$xG=q#/lib/quote_simple.c#;$yG=[$Qg];$zG=bless({$r1,$wG,$Q,$xG,$v1,$yG},$J2);$AG=q#ni:/lib/quote_simple_init.b#;$BG=q#ni:/lib/quote_simple_quote.b#;$CG=q#ni:/lib/quote_values.b#;$DG=q#ni:/lib/ref_eq.b#;$EG=q#ni:/lib/resolver.b#;$FG=q#ni:/lib/slice#;$GG=q#ni:/lib/slice.b#;$HG=q#ni:/lib/slice.c#;$IG={$y2,1};$JG=q#/lib/slice.c#;$KG=[$Ug];$LG=bless({$r1,$IG,$Q,$JG,$v1,$KG},$J2);$MG=q#ni:/lib/slice_init.b#;$NG=q#ni:/lib/slice_serialize.b#;$OG=q#ni:/lib/static_fn.b#;$PG={};$QG=q#fc#;$RG=[];$SG=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$TG=bless({$t,$RG,$v,$q,$w,$SG,$y,$z},$A);$UG=q#fk#;$VG=[];$WG=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$XG=bless({$t,$VG,$v,$q,$w,$WG,$y,$AB},$A);$YG=[];$ZG=q#ni('ni:/lib/fn')->new(@_)#;$cH=bless({$t,$YG,$v,$q,$w,$ZG,$y,$AB},$A);$dH=q#fp#;$eH=[];$fH=q#($$)#;$gH=bless({$t,$eH,$v,$q,$w,$ZG,$y,$fH},$A);$hH={$QG,$TG,$UG,$XG,$fz,$cH,$dH,$gH};$iH=q#/lib/static_fn.b#;$jH=bless({$r1,$PG,$W2,$q,$X2,$q,$Y2,$hH,$Q,$iH},$x2);$kH=q#ni:/lib/subclass.b#;$lH=q#ni:/lib/tag#;$mH={$z2,1};$nH=q#/lib/tag#;$oH={};$pH=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$qH=bless({$w,$pH,$y,$z},$A);$rH={$Ad,$qH};$sH=q#/lib/tag.b#;$tH=bless({$r1,$oH,$W2,$q,$X2,$q,$Y2,$rH,$Q,$sH},$x2);$uH={};$vH=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$wH=bless({$w,$vH,$y,$z},$A);$xH={$L4,$wH};$yH=q#/lib/tag_init.b#;$zH=bless({$r1,$uH,$W2,$q,$X2,$q,$Y2,$xH,$Q,$yH},$x2);$AH=[$md,$sd,$tH,$zH];$BH=bless({$r1,$mH,$Q,$nH,$v1,$AH},$A2);$CH=q#ni:/lib/tag.b#;$DH=q#ni:/lib/tag.c#;$EH={$A2,1};$FH=q#/lib/tag.c#;$GH=[$Ug];$HH=bless({$r1,$EH,$Q,$FH,$v1,$GH},$J2);$IH=q#ni:/lib/tag_init.b#;$JH=q#ni:/lib/test_assert_eq#;$KH={$B2,1};$LH=q#/lib/test_assert_eq#;$MH={$B2,1,$D2,1};$NH=q#/lib/test_assertion#;$OH={};$PH=q#commit#;$QH=[];$RH=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$SH=bless({$t,$QH,$v,$q,$w,$RH,$y,$z},$A);$TH={$PH,$SH};$UH=q#/lib/test_assertion_commit.b#;$VH=bless({$r1,$OH,$W2,$q,$X2,$q,$Y2,$TH,$Q,$UH},$x2);$WH=[$k3,$VH];$XH=bless({$r1,$MH,$Q,$NH,$v1,$WH},$E2);$YH={};$ZH=q#diff#;$cI=[];$dI=q#shift->{'diff'}#;$eI=bless({$t,$cI,$v,$q,$w,$dI,$y,$z},$A);$fI={$ZH,$eI};$gI=q#/lib/test_assert_eq_ro.b#;$hI=bless({$r1,$YH,$W2,$q,$X2,$q,$Y2,$fI,$Q,$gI},$x2);$iI={};$jI=[];$kI=q#my ($class, $diff) = @_;
+{diff => $diff};#;$lI=bless({$t,$jI,$v,$q,$w,$kI,$y,$z},$A);$mI={$L4,$lI};$nI=q#/lib/test_assert_eq_init.b#;$oI=bless({$r1,$iI,$W2,$q,$X2,$q,$Y2,$mI,$Q,$nI},$x2);$pI={};$qI=[];$rI=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$sI=bless({$t,$qI,$v,$q,$w,$rI,$y,$z},$A);$tI=q#failed#;$uI=[];$vI=q#defined shift->{diff}#;$wI=bless({$t,$uI,$v,$q,$w,$vI,$y,$z},$A);$xI=q#result#;$yI=[];$zI=bless({$t,$yI,$v,$q,$w,$vu,$y,$z},$A);$AI={$Lf,$sI,$tI,$wI,$xI,$zI};$BI=q#/lib/test_assert_eq_result.b#;$CI=bless({$r1,$pI,$W2,$q,$X2,$q,$Y2,$AI,$Q,$BI},$x2);$DI=[$XH,$hI,$oI,$CI];$EI=bless({$r1,$KH,$Q,$LH,$v1,$DI},$C2);$FI=q#ni:/lib/test_assert_eq.c#;$GI={$C2,1};$HI=q#/lib/test_assert_eq.c#;$II={$C2,1,$E2,1};$JI=q#/lib/test_assertion.c#;$KI=[$Qg];$LI=bless({$r1,$II,$Q,$JI,$v1,$KI},$J2);$MI=[$LI];$NI=bless({$r1,$GI,$Q,$HI,$v1,$MI},$J2);$OI=q#ni:/lib/test_assert_eq_init.b#;$PI=q#ni:/lib/test_assert_eq_result.b#;$QI=q#ni:/lib/test_assert_eq_ro.b#;$RI=q#ni:/lib/test_assertion#;$SI=q#ni:/lib/test_assertion.c#;$TI=q#ni:/lib/test_assertion_commit.b#;$UI=q#ni:/lib/test_case#;$VI={$C,1};$WI=q#/lib/test_case#;$XI=q#running_test#;$YI={};$ZI=[];$cJ=q#shift->{'assertions'}#;$dJ=bless({$t,$ZI,$v,$q,$w,$cJ,$y,$z},$A);$eJ=[];$fJ=q#shift->{'test'}#;$gJ=bless({$t,$eJ,$v,$q,$w,$fJ,$y,$z},$A);$hJ={$n,$dJ,$s,$gJ};$iJ=q#/lib/test_case_ro.b#;$jJ=bless({$r1,$YI,$W2,$q,$X2,$q,$Y2,$hJ,$Q,$iJ},$x2);$kJ={};$lJ=[];$mJ=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$nJ=bless({$t,$lJ,$v,$q,$w,$mJ,$y,$z},$A);$oJ={$p,$nJ};$pJ=q#/lib/test_case_rw.b#;$qJ=bless({$r1,$kJ,$W2,$q,$X2,$q,$Y2,$oJ,$Q,$pJ},$x2);$rJ={};$sJ=[];$tJ=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$uJ=bless({$t,$sJ,$v,$q,$w,$tJ,$y,$z},$A);$vJ={$L4,$uJ};$wJ=q#/lib/test_case_init.b#;$xJ=bless({$r1,$rJ,$W2,$q,$X2,$q,$Y2,$vJ,$Q,$wJ},$x2);$yJ={};$zJ=[];$AJ=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$BJ=bless({$t,$zJ,$v,$q,$w,$AJ,$y,$z},$A);$CJ=[];$DJ=q#!shift->{outcome}->[0]#;$EJ=bless({$t,$CJ,$v,$q,$w,$DJ,$y,$z},$A);$FJ={$Lf,$BJ,$tI,$EJ};$GJ=q#/lib/test_case_metrics.b#;$HJ=bless({$r1,$yJ,$W2,$q,$X2,$q,$Y2,$FJ,$Q,$GJ},$x2);$IJ={};$JJ=q#done#;$KJ=[];$LJ=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$MJ=bless({$t,$KJ,$v,$q,$w,$LJ,$y,$z},$A);$NJ=[];$OJ=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$PJ=bless({$t,$NJ,$v,$q,$w,$OJ,$y,$z},$A);$QJ={$JJ,$MJ,$tu,$PJ};$RJ=q#/lib/test_case_run.b#;$SJ=bless({$r1,$IJ,$W2,$q,$X2,$q,$Y2,$QJ,$Q,$RJ},$x2);$TJ=[$NF,$jJ,$qJ,$xJ,$HJ,$SJ];$UJ=bless({$r1,$VI,$Q,$WI,$XI,$q,$v1,$TJ},$F2);$VJ=[];$WJ=q#shift->{running_test} = undef#;$XJ=bless({$t,$VJ,$v,$q,$w,$WJ,$y,$z},$A);$YJ=q#ni:/lib/test_case.c#;$ZJ={$F2,1};$cK=q#/lib/test_case.c#;$dK={};$eK=[];$fK=q#shift->{'running_test'}#;$gK=bless({$t,$eK,$v,$q,$w,$fK,$y,$z},$A);$hK={$XI,$gK};$iK=q#/lib/test_case.c_test_ro.b#;$jK=bless({$r1,$dK,$W2,$q,$X2,$q,$Y2,$hK,$Q,$iK},$x2);$kK={};$lK=q#with_test#;$mK=[];$nK=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$oK=bless({$t,$mK,$v,$q,$w,$nK,$y,$z},$A);$pK={$lK,$oK};$qK=q#/lib/test_case.c_test.b#;$rK=bless({$r1,$kK,$W2,$XJ,$X2,$q,$Y2,$pK,$Q,$qK},$x2);$sK=[$SF,$jK,$rK];$tK=bless({$r1,$ZJ,$Q,$cK,$v1,$sK},$J2);$uK=q#ni:/lib/test_case.c_test.b#;$vK=q#ni:/lib/test_case.c_test_ro.b#;$wK=q#ni:/lib/test_case_init.b#;$xK=q#ni:/lib/test_case_metrics.b#;$yK=q#ni:/lib/test_case_ro.b#;$zK=q#ni:/lib/test_case_run.b#;$AK=q#ni:/lib/test_case_rw.b#;$BK=q#ni:/lib/test_value#;$CK={$G2,1};$DK=q#/lib/test_value#;$EK={};$FK=[];$GK=q#\\$_[1]#;$HK=bless({$t,$FK,$v,$q,$w,$GK,$y,$z},$A);$IK={$L4,$HK};$JK=q#/lib/test_value_init.b#;$KK=bless({$r1,$EK,$W2,$q,$X2,$q,$Y2,$IK,$Q,$JK},$x2);$LK={};$MK=q#(==#;$NK=[];$OK=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$PK=bless({$t,$NK,$v,$q,$w,$OK,$y,$z},$A);$QK=q#detailed_scalar_diff#;$RK=[];$SK=q#local $_;
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
[@diff];#;$TK=bless({$t,$RK,$v,$q,$w,$SK,$y,$z},$A);$UK=[];$VK=q#my ($self, $rhs) = @_;
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
return undef;#;$WK=bless({$t,$UK,$v,$q,$w,$VK,$y,$z},$A);$XK={$MK,$PK,$QK,$TK,$ZH,$WK};$YK=q#/lib/test_value_eq.b#;$ZK=bless({$r1,$LK,$W2,$q,$X2,$q,$Y2,$XK,$Q,$YK},$x2);$cL={};$dL=[];$eL=q#ni::json_encode ${$_[0]}#;$fL=bless({$t,$dL,$v,$q,$w,$eL,$y,$z},$A);$gL={$Lf,$fL};$hL=q#/lib/test_value_str.b#;$iL=bless({$r1,$cL,$W2,$q,$X2,$q,$Y2,$gL,$Q,$hL},$x2);$jL=[$k3,$KK,$ZK,$iL];$kL=bless({$r1,$CK,$Q,$DK,$v1,$jL},$H2);$lL=q#ni:/lib/test_value.c#;$mL={$H2,1};$nL=q#/lib/test_value.c#;$oL=[$Qg];$pL=bless({$r1,$mL,$Q,$nL,$v1,$oL},$J2);$qL=q#ni:/lib/test_value_eq.b#;$rL=q#ni:/lib/test_value_init.b#;$sL=q#ni:/lib/test_value_str.b#;$tL=q#ni:/lib/todo#;$uL={$H,1};$vL=q#/lib/todo#;$wL={};$xL=q#shift->{'todo'}#;$yL=bless({$w,$xL,$y,$z},$A);$zL={$E,$yL};$AL=q#/lib/todo_ro.b#;$BL=bless({$r1,$wL,$W2,$q,$X2,$q,$Y2,$zL,$Q,$AL},$x2);$CL={};$DL=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$EL=bless({$w,$DL,$y,$z},$A);$FL={$L4,$EL};$GL=q#/lib/todo_init.b#;$HL=bless({$r1,$CL,$W2,$q,$X2,$q,$Y2,$FL,$Q,$GL},$x2);$IL={};$JL=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$KL=bless({$w,$JL,$y,$z},$A);$LL={$Lf,$KL};$ML=q#/lib/todo_str.b#;$NL=bless({$r1,$IL,$W2,$q,$X2,$q,$Y2,$LL,$Q,$ML},$x2);$OL=[$NF,$BL,$HL,$NL];$PL=bless({$r1,$uL,$Q,$vL,$v1,$OL},$I2);$QL=q#ni:/lib/todo.c#;$RL={$I2,1};$SL=q#/lib/todo.c#;$TL=[$SF];$UL=bless({$r1,$RL,$Q,$SL,$v1,$TL},$J2);$VL=q#ni:/lib/todo_ctor.b#;$WL={};$XL=q#ni('ni:/lib/todo')->new(@_)#;$YL=bless({$w,$XL,$y,$z},$A);$ZL={$p1,$YL};$cM=q#/lib/todo_ctor.b#;$dM=bless({$r1,$WL,$W2,$q,$X2,$q,$Y2,$ZL,$Q,$cM},$x2);$eM=q#ni:/lib/todo_init.b#;$fM=q#ni:/lib/todo_ro.b#;$gM=q#ni:/lib/todo_str.b#;$hM=q#ni:/metaclass#;$iM={$J2,1};$jM=q#/metaclass#;$kM=[$kf,$yg,$qf,$rg];$lM=bless({$r1,$iM,$Q,$jM,$v1,$kM},$K2);$mM=q#ni:/metaclass.c#;$nM={$K2,1};$oM=q#/metaclass.c#;$pM=[$Hg];$qM=bless({$r1,$nM,$Q,$oM,$v1,$pM},$J2);$rM=q#ni:/module#;$sM=q#ni:/module.c#;$tM=q#ni:/object#;$uM=q#ni:/object.c#;$vM=q#ni:/semantic#;$wM=q#semantic#;$xM={$wM,1};$yM=[];$zM=bless({$r1,$xM,$Q,$we,$v1,$yM},$L2);$AM=q#ni:/semantic/dimension#;$BM={$P2,1};$CM=q#/semantic/dimension#;$DM=[$Hg];$EM=bless({$r1,$BM,$Q,$CM,$v1,$DM},$Q2);$FM=q#ni:/semantic/dimension.c#;$GM={$Q2,1};$HM=q#/semantic/dimension.c#;$IM=[$Yg];$JM=bless({$r1,$GM,$Q,$HM,$v1,$IM},$J2);$KM=q#ni:/semantic/task#;$LM=q#ni:/semantic/task.c#;$MM=q#ni:/semantic/task_outcome.b#;$NM=q#ni:/semantic/task_ro.b#;$OM=q#ni:main#;$PM={$Zp,1};$QM=[$dM,$jH,$IB,$Yp];$RM=bless({$r1,$PM,$Q,$Zp,$v1,$QM},$L2);$SM=q#ni:ni#;$TM={$yC,1};$UM=[$CF,$TC,$xC];$VM=bless({$r1,$TM,$Q,$yC,$v1,$UM},$L2);$WM={$d,$T,$W,$e1,$f1,$k1,$l1,$l5,$m5,$r5,$s5,$E5,$F5,$R5,$S5,$g6,$h6,$t6,$u6,$P6,$Q6,$V6,$W6,$e7,$f7,$U8,$V8,$d9,$e9,$x9,$y9,$O9,$P9,$Cc,$Dc,$Kc,$Lc,$Vc,$Wc,$re,$se,$xe,$ye,$Hg,$Ig,$Yg,$Zg,$fh,$gh,$yh,$zh,$Dh,$Eh,$oh,$Fh,$wh,$Gh,$f5,$Hh,$Xh,$Yh,$d5,$Zh,$ei,$fi,$Hi,$Ii,$Mi,$Ni,$ni,$Oi,$Fi,$Pi,$ij,$jj,$nj,$oj,$Xi,$pj,$gj,$qj,$Wk,$Xk,$dl,$el,$Ek,$fl,$Uk,$gl,$Gj,$hl,$wk,$il,$ck,$jl,$zj,$kl,$Cm,$Gm,$gn,$hn,$en,$in,$Yl,$jn,$lm,$kn,$Al,$ln,$zm,$mn,$tl,$nn,$Il,$on,$Io,$Jo,$No,$Oo,$En,$Po,$do,$Qo,$Ln,$Ro,$Go,$So,$wn,$To,$lo,$Uo,$pp,$qp,$up,$vp,$np,$wp,$ep,$xp,$Yp,$cq,$Aq,$Bq,$Fq,$Gq,$lq,$Hq,$yq,$Iq,$J4,$Jq,$Vh,$Kq,$Th,$Lq,$N3,$Mq,$V3,$Nq,$j4,$Oq,$t3,$Pq,$H4,$Qq,$v4,$Rq,$B8,$Sq,$Wq,$Xq,$z8,$Yq,$F7,$Zq,$f8,$cr,$v7,$dr,$R7,$er,$Tr,$Ur,$Yr,$Zr,$Dr,$cs,$Rr,$ds,$wr,$es,$ot,$st,$Et,$Ft,$Ct,$Gt,$Iu,$Mu,$iv,$jv,$gv,$kv,$hu,$lv,$ru,$mv,$Yt,$nv,$Du,$ov,$Qs,$pv,$mt,$qv,$Iv,$Jv,$Nv,$Ov,$zv,$Pv,$Gv,$Qv,$Uv,$Vv,$Jf,$Wv,$md,$Xv,$Ug,$Yv,$kw,$lw,$Le,$mw,$qw,$rw,$iw,$sw,$qf,$tw,$Lw,$Mw,$Dw,$Nw,$Rw,$Sw,$Jw,$Tw,$pg,$Uw,$zf,$Vw,$gg,$Ww,$ng,$Xw,$dy,$ey,$py,$qy,$ny,$ry,$Jx,$sy,$vx,$ty,$Px,$uy,$Dx,$vy,$gx,$wy,$mx,$xy,$Zx,$yy,$kd,$zy,$Dz,$Hz,$Tz,$Uz,$Rz,$Vz,$Ry,$Wz,$uz,$Xz,$lz,$Yz,$Bz,$Zz,$lB,$mB,$qB,$rB,$jB,$sB,$tA,$tB,$mA,$uB,$NA,$vB,$uc,$wB,$IB,$JB,$wc,$KB,$OB,$PB,$ea,$QB,$Ca,$RB,$i3,$SB,$yg,$TB,$xC,$zC,$TC,$UC,$Qf,$VC,$sd,$WC,$Se,$XC,$Ze,$YC,$yE,$zE,$DE,$EE,$GD,$FE,$wE,$GE,$dE,$HE,$mE,$IE,$oD,$JE,$CF,$DF,$NF,$OF,$SF,$TF,$LF,$UF,$kf,$VF,$Qb,$WF,$Ka,$XF,$mc,$YF,$Ab,$ZF,$uG,$vG,$zG,$AG,$jG,$BG,$sG,$CG,$kb,$DG,$Xf,$EG,$if,$FG,$Ud,$GG,$Ed,$HG,$LG,$MG,$Kd,$NG,$Sd,$OG,$jH,$kH,$Fg,$lH,$BH,$CH,$tH,$DH,$HH,$IH,$zH,$JH,$EI,$FI,$NI,$OI,$oI,$PI,$CI,$QI,$hI,$RI,$XH,$SI,$LI,$TI,$VH,$UI,$UJ,$YJ,$tK,$uK,$rK,$vK,$jK,$wK,$xJ,$xK,$HJ,$yK,$jJ,$zK,$SJ,$AK,$qJ,$BK,$kL,$lL,$pL,$qL,$ZK,$rL,$KK,$sL,$iL,$tL,$PL,$QL,$UL,$VL,$dM,$eM,$HL,$fM,$BL,$gM,$NL,$hM,$lM,$mM,$qM,$rM,$rg,$sM,$Wg,$tM,$k3,$uM,$Qg,$vM,$zM,$AM,$EM,$FM,$JM,$KM,$Ds,$LM,$yt,$MM,$Bs,$NM,$ps,$OM,$RM,$SM,$VM};$XM=q#resolvers#;$YM=[];$ZM=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$cN=bless({$t,$YM,$v,$q,$w,$ZM,$y,$z},$A);$dN=q#file#;$eN=[];$fN=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$gN=bless({$t,$eN,$v,$q,$w,$fN,$y,$z},$A);$hN=q#null#;$iN=[];$jN=q#ni('ni:/io/null')->new#;$kN=bless({$t,$iN,$v,$q,$w,$jN,$y,$z},$A);$lN=q#sh#;$mN=[];$nN=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$oN=bless({$t,$mN,$v,$q,$w,$nN,$y,$z},$A);$pN=q#str#;$qN=[];$rN=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$sN=bless({$t,$qN,$v,$q,$w,$rN,$y,$z},$A);$tN={$h8,$cN,$dN,$gN,$hN,$kN,$lN,$oN,$pN,$sN};$uN=bless({$c,$WM,$XM,$tN},$r2);*$Ky=\&$Hy;*$Jy=\&$Fy;*$Iy=\&$Dy;*$yd=\&$wd;*$xd=\&$ud;$i3->apply_($I1);$i3->apply_($J1);$i3->apply_($K1);$i3->apply_($L1);$i3->apply_($s1);$i3->apply_($M1);$i3->apply_($w1);$i3->apply_($N1);$i3->apply_($x1);$i3->apply_($O1);$i3->apply_($y1);$i3->apply_($P1);$i3->apply_($z1);$i3->apply_($Q1);$i3->apply_($A1);$i3->apply_($R1);$i3->apply_($B1);$i3->apply_($S1);$i3->apply_($C1);$i3->apply_($T1);$i3->apply_($D1);$i3->apply_($U1);$i3->apply_($E1);$i3->apply_($V1);$i3->apply_($F1);$i3->apply_($W1);$i3->apply_($X1);$i3->apply_($Y1);$i3->apply_($Z1);$i3->apply_($c2);$i3->apply_($d2);$i3->apply_($e2);$i3->apply_($f2);$i3->apply_($g2);$i3->apply_($h2);$i3->apply_($i2);$i3->apply_($j2);$i3->apply_($k2);$i3->apply_($S);$i3->apply_($l2);$i3->apply_($A);$i3->apply_($m2);$i3->apply_($n2);$i3->apply_($o2);$i3->apply_($p2);$i3->apply_($q2);$i3->apply_($r2);$i3->apply_($s2);$i3->apply_($t2);$i3->apply_($u2);$i3->apply_($v2);$i3->apply_($w2);$i3->apply_($x2);$i3->apply_($y2);$i3->apply_($z2);$i3->apply_($A2);$i3->apply_($B2);$i3->apply_($C2);$i3->apply_($D2);$i3->apply_($E2);$i3->apply_($C);$i3->apply_($F2);$i3->apply_($G2);$i3->apply_($H2);$i3->apply_($H);$i3->apply_($I2);$i3->apply_($J2);$i3->apply_($K2);$i3->apply_($L2);$i3->apply_($M2);$i3->apply_($N2);$i3->apply_($O2);$i3->apply_($P2);$i3->apply_($Q2);$i3->apply_($R2);$i3->apply_($S2);$t3->apply_($s1);$t3->apply_($w1);$t3->apply_($x1);$t3->apply_($y1);$t3->apply_($z1);$t3->apply_($A1);$t3->apply_($B1);$t3->apply_($C1);$t3->apply_($D1);$t3->apply_($E1);$t3->apply_($F1);$N3->apply_($s1);$N3->apply_($w1);$N3->apply_($x1);$N3->apply_($y1);$N3->apply_($z1);$N3->apply_($A1);$N3->apply_($B1);$N3->apply_($C1);$N3->apply_($D1);$N3->apply_($E1);$N3->apply_($F1);$V3->apply_($s1);$V3->apply_($w1);$V3->apply_($x1);$V3->apply_($y1);$V3->apply_($z1);$V3->apply_($A1);$V3->apply_($B1);$V3->apply_($C1);$V3->apply_($D1);$V3->apply_($E1);$V3->apply_($F1);$j4->apply_($s1);$j4->apply_($w1);$j4->apply_($x1);$j4->apply_($y1);$j4->apply_($z1);$j4->apply_($A1);$j4->apply_($B1);$j4->apply_($C1);$j4->apply_($D1);$j4->apply_($E1);$j4->apply_($F1);$v4->apply_($s1);$v4->apply_($w1);$v4->apply_($x1);$v4->apply_($y1);$v4->apply_($z1);$v4->apply_($A1);$v4->apply_($B1);$v4->apply_($C1);$v4->apply_($D1);$v4->apply_($E1);$v4->apply_($F1);$H4->apply_($s1);$H4->apply_($w1);$H4->apply_($x1);$H4->apply_($y1);$H4->apply_($z1);$H4->apply_($A1);$H4->apply_($B1);$H4->apply_($C1);$H4->apply_($D1);$H4->apply_($E1);$H4->apply_($F1);$d5->apply_($s1);$v7->apply_($E1);$F7->apply_($E1);$R7->apply_($E1);$f8->apply_($E1);$z8->apply_($E1);$ea->apply_($p2);$Ca->apply_($p2);$Ka->apply_($p2);$Ka->apply_($v2);$kb->apply_($p2);$kb->apply_($v2);$Ab->apply_($p2);$Ab->apply_($v2);$Qb->apply_($p2);$mc->apply_($p2);$uc->apply_($p2);$kd->apply_($I1);$kd->apply_($J1);$kd->apply_($L1);$kd->apply_($M1);$kd->apply_($N1);$kd->apply_($O1);$kd->apply_($P1);$kd->apply_($Q1);$kd->apply_($R1);$kd->apply_($S1);$kd->apply_($T1);$kd->apply_($U1);$kd->apply_($V1);$kd->apply_($W1);$kd->apply_($Y1);$kd->apply_($c2);$kd->apply_($e2);$kd->apply_($f2);$kd->apply_($g2);$kd->apply_($h2);$kd->apply_($i2);$kd->apply_($j2);$kd->apply_($k2);$kd->apply_($l2);$kd->apply_($m2);$kd->apply_($o2);$kd->apply_($q2);$kd->apply_($s2);$kd->apply_($u2);$kd->apply_($w2);$kd->apply_($x2);$kd->apply_($y2);$kd->apply_($z2);$kd->apply_($A2);$kd->apply_($C2);$kd->apply_($E2);$kd->apply_($F2);$kd->apply_($H2);$kd->apply_($I2);$kd->apply_($J2);$kd->apply_($K2);$kd->apply_($L2);$kd->apply_($M2);$kd->apply_($O2);$kd->apply_($P2);$kd->apply_($Q2);$kd->apply_($S2);$sd->apply_($I1);$sd->apply_($J1);$sd->apply_($L1);$sd->apply_($M1);$sd->apply_($N1);$sd->apply_($O1);$sd->apply_($P1);$sd->apply_($Q1);$sd->apply_($R1);$sd->apply_($S1);$sd->apply_($T1);$sd->apply_($U1);$sd->apply_($V1);$sd->apply_($W1);$sd->apply_($Y1);$sd->apply_($c2);$sd->apply_($e2);$sd->apply_($g2);$sd->apply_($h2);$sd->apply_($i2);$sd->apply_($j2);$sd->apply_($k2);$sd->apply_($S);$sd->apply_($l2);$sd->apply_($m2);$sd->apply_($o2);$sd->apply_($q2);$sd->apply_($s2);$sd->apply_($u2);$sd->apply_($w2);$sd->apply_($x2);$sd->apply_($y2);$sd->apply_($z2);$sd->apply_($A2);$sd->apply_($C2);$sd->apply_($E2);$sd->apply_($F2);$sd->apply_($H2);$sd->apply_($I2);$sd->apply_($J2);$sd->apply_($K2);$sd->apply_($L2);$sd->apply_($M2);$sd->apply_($O2);$sd->apply_($P2);$sd->apply_($Q2);$sd->apply_($S2);$Ed->apply_($x2);$Kd->apply_($x2);$Sd->apply_($j2);$Sd->apply_($x2);$Le->apply_($I1);$Le->apply_($J1);$Le->apply_($L1);$Le->apply_($M1);$Le->apply_($N1);$Le->apply_($O1);$Le->apply_($P1);$Le->apply_($Q1);$Le->apply_($R1);$Le->apply_($S1);$Le->apply_($T1);$Le->apply_($U1);$Le->apply_($V1);$Le->apply_($W1);$Le->apply_($Y1);$Le->apply_($c2);$Le->apply_($e2);$Le->apply_($g2);$Le->apply_($h2);$Le->apply_($i2);$Le->apply_($k2);$Le->apply_($l2);$Le->apply_($m2);$Le->apply_($o2);$Le->apply_($q2);$Le->apply_($s2);$Le->apply_($u2);$Le->apply_($w2);$Le->apply_($y2);$Le->apply_($A2);$Le->apply_($C2);$Le->apply_($E2);$Le->apply_($F2);$Le->apply_($H2);$Le->apply_($I2);$Le->apply_($J2);$Le->apply_($K2);$Le->apply_($L2);$Le->apply_($M2);$Le->apply_($O2);$Le->apply_($P2);$Le->apply_($Q2);$Le->apply_($S2);$Se->apply_($I1);$Se->apply_($J1);$Se->apply_($L1);$Se->apply_($M1);$Se->apply_($N1);$Se->apply_($O1);$Se->apply_($P1);$Se->apply_($Q1);$Se->apply_($R1);$Se->apply_($S1);$Se->apply_($T1);$Se->apply_($U1);$Se->apply_($V1);$Se->apply_($W1);$Se->apply_($Y1);$Se->apply_($c2);$Se->apply_($e2);$Se->apply_($g2);$Se->apply_($h2);$Se->apply_($i2);$Se->apply_($j2);$Se->apply_($k2);$Se->apply_($l2);$Se->apply_($m2);$Se->apply_($o2);$Se->apply_($q2);$Se->apply_($s2);$Se->apply_($u2);$Se->apply_($w2);$Se->apply_($x2);$Se->apply_($y2);$Se->apply_($z2);$Se->apply_($A2);$Se->apply_($C2);$Se->apply_($E2);$Se->apply_($F2);$Se->apply_($H2);$Se->apply_($I2);$Se->apply_($J2);$Se->apply_($K2);$Se->apply_($L2);$Se->apply_($M2);$Se->apply_($O2);$Se->apply_($P2);$Se->apply_($Q2);$Se->apply_($S2);$Ze->apply_($I1);$Ze->apply_($J1);$Ze->apply_($L1);$Ze->apply_($M1);$Ze->apply_($N1);$Ze->apply_($O1);$Ze->apply_($P1);$Ze->apply_($Q1);$Ze->apply_($R1);$Ze->apply_($S1);$Ze->apply_($T1);$Ze->apply_($U1);$Ze->apply_($V1);$Ze->apply_($W1);$Ze->apply_($Y1);$Ze->apply_($c2);$Ze->apply_($e2);$Ze->apply_($g2);$Ze->apply_($h2);$Ze->apply_($i2);$Ze->apply_($j2);$Ze->apply_($k2);$Ze->apply_($l2);$Ze->apply_($m2);$Ze->apply_($o2);$Ze->apply_($q2);$Ze->apply_($s2);$Ze->apply_($u2);$Ze->apply_($w2);$Ze->apply_($x2);$Ze->apply_($y2);$Ze->apply_($z2);$Ze->apply_($A2);$Ze->apply_($C2);$Ze->apply_($E2);$Ze->apply_($F2);$Ze->apply_($H2);$Ze->apply_($I2);$Ze->apply_($J2);$Ze->apply_($K2);$Ze->apply_($L2);$Ze->apply_($M2);$Ze->apply_($O2);$Ze->apply_($P2);$Ze->apply_($Q2);$Ze->apply_($S2);$if->apply_($I1);$if->apply_($J1);$if->apply_($L1);$if->apply_($M1);$if->apply_($N1);$if->apply_($O1);$if->apply_($P1);$if->apply_($Q1);$if->apply_($R1);$if->apply_($S1);$if->apply_($T1);$if->apply_($U1);$if->apply_($V1);$if->apply_($W1);$if->apply_($Y1);$if->apply_($c2);$if->apply_($e2);$if->apply_($g2);$if->apply_($h2);$if->apply_($i2);$if->apply_($j2);$if->apply_($k2);$if->apply_($l2);$if->apply_($m2);$if->apply_($o2);$if->apply_($q2);$if->apply_($s2);$if->apply_($u2);$if->apply_($w2);$if->apply_($y2);$if->apply_($z2);$if->apply_($A2);$if->apply_($C2);$if->apply_($E2);$if->apply_($F2);$if->apply_($H2);$if->apply_($I2);$if->apply_($J2);$if->apply_($K2);$if->apply_($L2);$if->apply_($M2);$if->apply_($O2);$if->apply_($P2);$if->apply_($Q2);$if->apply_($S2);$qf->apply_($I1);$qf->apply_($J1);$qf->apply_($L1);$qf->apply_($M1);$qf->apply_($N1);$qf->apply_($O1);$qf->apply_($P1);$qf->apply_($Q1);$qf->apply_($R1);$qf->apply_($S1);$qf->apply_($T1);$qf->apply_($U1);$qf->apply_($V1);$qf->apply_($W1);$qf->apply_($Y1);$qf->apply_($c2);$qf->apply_($e2);$qf->apply_($g2);$qf->apply_($i2);$qf->apply_($k2);$qf->apply_($l2);$qf->apply_($m2);$qf->apply_($o2);$qf->apply_($q2);$qf->apply_($s2);$qf->apply_($u2);$qf->apply_($w2);$qf->apply_($y2);$qf->apply_($A2);$qf->apply_($C2);$qf->apply_($E2);$qf->apply_($F2);$qf->apply_($H2);$qf->apply_($I2);$qf->apply_($J2);$qf->apply_($K2);$qf->apply_($L2);$qf->apply_($M2);$qf->apply_($O2);$qf->apply_($P2);$qf->apply_($Q2);$qf->apply_($S2);$zf->apply_($I1);$zf->apply_($J1);$zf->apply_($L1);$zf->apply_($M1);$zf->apply_($N1);$zf->apply_($O1);$zf->apply_($P1);$zf->apply_($Q1);$zf->apply_($R1);$zf->apply_($S1);$zf->apply_($T1);$zf->apply_($U1);$zf->apply_($V1);$zf->apply_($W1);$zf->apply_($Y1);$zf->apply_($c2);$zf->apply_($e2);$zf->apply_($g2);$zf->apply_($h2);$zf->apply_($i2);$zf->apply_($k2);$zf->apply_($l2);$zf->apply_($m2);$zf->apply_($o2);$zf->apply_($q2);$zf->apply_($s2);$zf->apply_($u2);$zf->apply_($w2);$zf->apply_($y2);$zf->apply_($A2);$zf->apply_($C2);$zf->apply_($E2);$zf->apply_($F2);$zf->apply_($H2);$zf->apply_($I2);$zf->apply_($J2);$zf->apply_($K2);$zf->apply_($L2);$zf->apply_($M2);$zf->apply_($O2);$zf->apply_($P2);$zf->apply_($Q2);$zf->apply_($S2);$Jf->apply_($I1);$Jf->apply_($J1);$Jf->apply_($L1);$Jf->apply_($M1);$Jf->apply_($N1);$Jf->apply_($O1);$Jf->apply_($P1);$Jf->apply_($Q1);$Jf->apply_($R1);$Jf->apply_($S1);$Jf->apply_($T1);$Jf->apply_($U1);$Jf->apply_($V1);$Jf->apply_($W1);$Jf->apply_($Y1);$Jf->apply_($c2);$Jf->apply_($e2);$Jf->apply_($g2);$Jf->apply_($h2);$Jf->apply_($i2);$Jf->apply_($k2);$Jf->apply_($l2);$Jf->apply_($m2);$Jf->apply_($o2);$Jf->apply_($q2);$Jf->apply_($s2);$Jf->apply_($u2);$Jf->apply_($w2);$Jf->apply_($y2);$Jf->apply_($A2);$Jf->apply_($C2);$Jf->apply_($E2);$Jf->apply_($F2);$Jf->apply_($H2);$Jf->apply_($I2);$Jf->apply_($J2);$Jf->apply_($K2);$Jf->apply_($L2);$Jf->apply_($M2);$Jf->apply_($O2);$Jf->apply_($P2);$Jf->apply_($Q2);$Jf->apply_($S2);$Qf->apply_($I1);$Qf->apply_($J1);$Qf->apply_($L1);$Qf->apply_($M1);$Qf->apply_($N1);$Qf->apply_($O1);$Qf->apply_($P1);$Qf->apply_($Q1);$Qf->apply_($R1);$Qf->apply_($S1);$Qf->apply_($T1);$Qf->apply_($U1);$Qf->apply_($V1);$Qf->apply_($W1);$Qf->apply_($Y1);$Qf->apply_($c2);$Qf->apply_($e2);$Qf->apply_($g2);$Qf->apply_($h2);$Qf->apply_($i2);$Qf->apply_($k2);$Qf->apply_($l2);$Qf->apply_($m2);$Qf->apply_($o2);$Qf->apply_($q2);$Qf->apply_($s2);$Qf->apply_($u2);$Qf->apply_($w2);$Qf->apply_($y2);$Qf->apply_($A2);$Qf->apply_($C2);$Qf->apply_($E2);$Qf->apply_($F2);$Qf->apply_($H2);$Qf->apply_($I2);$Qf->apply_($J2);$Qf->apply_($K2);$Qf->apply_($L2);$Qf->apply_($M2);$Qf->apply_($O2);$Qf->apply_($P2);$Qf->apply_($Q2);$Qf->apply_($S2);$Xf->apply_($I1);$Xf->apply_($J1);$Xf->apply_($L1);$Xf->apply_($M1);$Xf->apply_($N1);$Xf->apply_($O1);$Xf->apply_($P1);$Xf->apply_($Q1);$Xf->apply_($R1);$Xf->apply_($S1);$Xf->apply_($T1);$Xf->apply_($U1);$Xf->apply_($V1);$Xf->apply_($W1);$Xf->apply_($Y1);$Xf->apply_($c2);$Xf->apply_($e2);$Xf->apply_($g2);$Xf->apply_($h2);$Xf->apply_($i2);$Xf->apply_($k2);$Xf->apply_($l2);$Xf->apply_($m2);$Xf->apply_($o2);$Xf->apply_($q2);$Xf->apply_($s2);$Xf->apply_($u2);$Xf->apply_($w2);$Xf->apply_($y2);$Xf->apply_($A2);$Xf->apply_($C2);$Xf->apply_($E2);$Xf->apply_($F2);$Xf->apply_($H2);$Xf->apply_($I2);$Xf->apply_($J2);$Xf->apply_($K2);$Xf->apply_($L2);$Xf->apply_($M2);$Xf->apply_($O2);$Xf->apply_($P2);$Xf->apply_($Q2);$Xf->apply_($S2);$gg->apply_($I1);$gg->apply_($J1);$gg->apply_($L1);$gg->apply_($M1);$gg->apply_($N1);$gg->apply_($O1);$gg->apply_($P1);$gg->apply_($Q1);$gg->apply_($R1);$gg->apply_($S1);$gg->apply_($T1);$gg->apply_($U1);$gg->apply_($V1);$gg->apply_($W1);$gg->apply_($Y1);$gg->apply_($c2);$gg->apply_($e2);$gg->apply_($g2);$gg->apply_($h2);$gg->apply_($i2);$gg->apply_($k2);$gg->apply_($l2);$gg->apply_($m2);$gg->apply_($o2);$gg->apply_($q2);$gg->apply_($s2);$gg->apply_($u2);$gg->apply_($w2);$gg->apply_($y2);$gg->apply_($A2);$gg->apply_($C2);$gg->apply_($E2);$gg->apply_($F2);$gg->apply_($H2);$gg->apply_($I2);$gg->apply_($J2);$gg->apply_($K2);$gg->apply_($L2);$gg->apply_($M2);$gg->apply_($O2);$gg->apply_($P2);$gg->apply_($Q2);$gg->apply_($S2);$ng->apply_($I1);$ng->apply_($J1);$ng->apply_($L1);$ng->apply_($M1);$ng->apply_($N1);$ng->apply_($O1);$ng->apply_($P1);$ng->apply_($Q1);$ng->apply_($R1);$ng->apply_($S1);$ng->apply_($T1);$ng->apply_($U1);$ng->apply_($V1);$ng->apply_($W1);$ng->apply_($Y1);$ng->apply_($c2);$ng->apply_($e2);$ng->apply_($g2);$ng->apply_($h2);$ng->apply_($i2);$ng->apply_($k2);$ng->apply_($l2);$ng->apply_($m2);$ng->apply_($o2);$ng->apply_($q2);$ng->apply_($s2);$ng->apply_($u2);$ng->apply_($w2);$ng->apply_($y2);$ng->apply_($A2);$ng->apply_($C2);$ng->apply_($E2);$ng->apply_($F2);$ng->apply_($H2);$ng->apply_($I2);$ng->apply_($J2);$ng->apply_($K2);$ng->apply_($L2);$ng->apply_($M2);$ng->apply_($O2);$ng->apply_($P2);$ng->apply_($Q2);$ng->apply_($S2);$yg->apply_($I1);$yg->apply_($J1);$yg->apply_($L1);$yg->apply_($M1);$yg->apply_($N1);$yg->apply_($O1);$yg->apply_($P1);$yg->apply_($Q1);$yg->apply_($R1);$yg->apply_($S1);$yg->apply_($T1);$yg->apply_($U1);$yg->apply_($V1);$yg->apply_($W1);$yg->apply_($Y1);$yg->apply_($c2);$yg->apply_($e2);$yg->apply_($g2);$yg->apply_($i2);$yg->apply_($k2);$yg->apply_($l2);$yg->apply_($A);$yg->apply_($m2);$yg->apply_($o2);$yg->apply_($q2);$yg->apply_($s2);$yg->apply_($u2);$yg->apply_($w2);$yg->apply_($x2);$yg->apply_($y2);$yg->apply_($z2);$yg->apply_($A2);$yg->apply_($C2);$yg->apply_($E2);$yg->apply_($F2);$yg->apply_($H2);$yg->apply_($I2);$yg->apply_($J2);$yg->apply_($K2);$yg->apply_($M2);$yg->apply_($O2);$yg->apply_($P2);$yg->apply_($Q2);$yg->apply_($S2);$Fg->apply_($I1);$Fg->apply_($J1);$Fg->apply_($L1);$Fg->apply_($M1);$Fg->apply_($N1);$Fg->apply_($O1);$Fg->apply_($P1);$Fg->apply_($Q1);$Fg->apply_($R1);$Fg->apply_($S1);$Fg->apply_($T1);$Fg->apply_($U1);$Fg->apply_($V1);$Fg->apply_($W1);$Fg->apply_($Y1);$Fg->apply_($c2);$Fg->apply_($e2);$Fg->apply_($g2);$Fg->apply_($i2);$Fg->apply_($k2);$Fg->apply_($l2);$Fg->apply_($m2);$Fg->apply_($o2);$Fg->apply_($q2);$Fg->apply_($s2);$Fg->apply_($u2);$Fg->apply_($w2);$Fg->apply_($y2);$Fg->apply_($A2);$Fg->apply_($C2);$Fg->apply_($E2);$Fg->apply_($F2);$Fg->apply_($H2);$Fg->apply_($I2);$Fg->apply_($K2);$Fg->apply_($M2);$Fg->apply_($O2);$Fg->apply_($P2);$Fg->apply_($Q2);$Fg->apply_($S2);$oh->apply_($K1);$wh->apply_($K1);$Th->apply_($M1);$Th->apply_($N1);$Th->apply_($O1);$Th->apply_($P1);$Th->apply_($Q1);$Th->apply_($R1);$Th->apply_($S1);$Th->apply_($T1);$Th->apply_($U1);$Th->apply_($V1);$Th->apply_($W1);$ni->apply_($w1);$Fi->apply_($w1);$Xi->apply_($x1);$gj->apply_($x1);$zj->apply_($y1);$Gj->apply_($y1);$ck->apply_($y1);$wk->apply_($y1);$Ek->apply_($y1);$Uk->apply_($y1);$tl->apply_($z1);$tl->apply_($B1);$Al->apply_($z1);$Il->apply_($z1);$Yl->apply_($z1);$Yl->apply_($B1);$lm->apply_($z1);$zm->apply_($z1);$zm->apply_($B1);$en->apply_($Q1);$wn->apply_($A1);$En->apply_($A1);$Ln->apply_($A1);$do->apply_($A1);$lo->apply_($A1);$Go->apply_($A1);$ep->apply_($B1);$np->apply_($B1);$Yp->apply_($Zp);$lq->apply_($C1);$yq->apply_($C1);$wr->apply_($F1);$Dr->apply_($F1);$Rr->apply_($F1);$ps->apply_($X1);$ps->apply_($Z1);$ps->apply_($d2);$ps->apply_($R2);$Bs->apply_($X1);$Bs->apply_($Z1);$Bs->apply_($d2);$Bs->apply_($R2);$Qs->apply_($X1);$Qs->apply_($Z1);$Qs->apply_($d2);$mt->apply_($X1);$mt->apply_($Z1);$mt->apply_($d2);$Ct->apply_($Y1);$Ct->apply_($c2);$Ct->apply_($e2);$Yt->apply_($Z1);$hu->apply_($Z1);$ru->apply_($Z1);$Du->apply_($Z1);$gv->apply_($c2);$zv->apply_($d2);$Gv->apply_($d2);$iw->apply_($h2);$Dw->apply_($j2);$Jw->apply_($j2);$gx->apply_($S);$mx->apply_($S);$vx->apply_($S);$Dx->apply_($S);$Jx->apply_($S);$Px->apply_($S);$Zx->apply_($S);$ny->apply_($l2);$Ry->apply_($A);$lz->apply_($A);$uz->apply_($A);$Bz->apply_($A);$Rz->apply_($m2);$mA->apply_($n2);$tA->apply_($n2);$NA->apply_($n2);$jB->apply_($n2);$IB->apply_($Zp);$xC->apply_($yC);$TC->apply_($yC);$oD->apply_($r2);$GD->apply_($r2);$dE->apply_($r2);$mE->apply_($r2);$wE->apply_($r2);$CF->apply_($yC);$LF->apply_($t2);$LF->apply_($C);$LF->apply_($H);$jG->apply_($v2);$sG->apply_($v2);$jH->apply_($Zp);$tH->apply_($z2);$zH->apply_($z2);$VH->apply_($B2);$VH->apply_($D2);$hI->apply_($B2);$oI->apply_($B2);$CI->apply_($B2);$jJ->apply_($C);$qJ->apply_($C);$xJ->apply_($C);$HJ->apply_($C);$SJ->apply_($C);$jK->apply_($F2);$rK->apply_($F2);$KK->apply_($G2);$ZK->apply_($G2);$iL->apply_($G2);$BL->apply_($H);$HL->apply_($H);$NL->apply_($H);$dM->apply_($Zp);$ni::self=$uN;&$V($T);&$V($e1);&$V($k1);&$V($i3);&$V($k3);&$m3($k3);&$V($t3);&$V($N3);&$V($V3);&$V($j4);&$V($v4);&$V($H4);&$V($J4);&$m3($J4);&$V($d5);&$V($f5);&$m3($f5);&$V($l5);&$V($r5);&$V($E5);&$V($R5);&$V($g6);&$V($t6);&$V($P6);&$V($V6);&$V($e7);&$V($v7);&$V($F7);&$V($R7);&$V($f8);&$V($z8);&$V($B8);&$m3($B8);&$V($U8);&$V($d9);&$V($x9);&$V($O9);&$V($ea);&$V($Ca);&$V($Ka);&$V($kb);&$V($Ab);&$V($Qb);&$V($mc);&$V($uc);&$V($wc);&$m3($wc);&$V($Cc);&$V($Kc);&$V($Vc);&$V($kd);&$V($md);&$m3($md);&$V($sd);&$V($Ed);&$V($Kd);&$V($Sd);&$V($Ud);&$m3($Ud);&$V($re);&$V($xe);&$V($Le);&$V($Se);&$V($Ze);&$V($if);&$V($kf);&$V($qf);&$V($zf);&$V($Jf);&$V($Qf);&$V($Xf);&$V($gg);&$V($ng);&$V($pg);&$V($rg);&$m3($rg);&$V($yg);&$V($Fg);&$V($Hg);&$m3($Hg);&$V($Qg);&$m3($Qg);&$V($Ug);&$m3($Ug);&$V($Wg);&$m3($Wg);&$V($Yg);&$m3($Yg);&$V($fh);&$m3($fh);&$V($oh);&$V($wh);&$V($yh);&$m3($yh);&$V($Dh);&$m3($Dh);&$V($Th);&$V($Vh);&$m3($Vh);&$V($Xh);&$m3($Xh);&$V($ei);&$m3($ei);&$V($ni);&$V($Fi);&$V($Hi);&$m3($Hi);&$V($Mi);&$m3($Mi);&$V($Xi);&$V($gj);&$V($ij);&$m3($ij);&$V($nj);&$m3($nj);&$V($zj);&$V($Gj);&$V($ck);&$V($wk);&$V($Ek);&$V($Uk);&$V($Wk);&$m3($Wk);&$V($dl);&$m3($dl);&$V($tl);&$V($Al);&$V($Il);&$V($Yl);&$V($lm);&$V($zm);&$V($Cm);&$m3($Cm);&$Fm($Cm);&$V($en);&$V($gn);&$m3($gn);&$V($wn);&$V($En);&$V($Ln);&$V($do);&$V($lo);&$V($Go);&$V($Io);&$m3($Io);&$V($No);&$m3($No);&$V($ep);&$V($np);&$V($pp);&$m3($pp);&$V($up);&$m3($up);&$V($Yp);&$V($lq);&$V($yq);&$V($Aq);&$m3($Aq);&$V($Fq);&$m3($Fq);&$V($Wq);&$m3($Wq);&$V($wr);&$V($Dr);&$V($Rr);&$V($Tr);&$m3($Tr);&$V($Yr);&$m3($Yr);&$V($ps);&$V($Bs);&$V($Ds);&$m3($Ds);&$V($Qs);&$V($mt);&$V($ot);&$m3($ot);&$rt($ot);&$V($yt);&$m3($yt);&$V($Ct);&$V($Et);&$m3($Et);&$V($Yt);&$V($hu);&$V($ru);&$V($Du);&$V($Iu);&$m3($Iu);&$rt($Iu);&$Lu($Iu);&$V($gv);&$V($iv);&$m3($iv);&$V($zv);&$V($Gv);&$V($Iv);&$m3($Iv);&$rt($Iv);&$V($Nv);&$m3($Nv);&$V($Uv);&$m3($Uv);&$V($iw);&$V($kw);&$m3($kw);&$V($qw);&$m3($qw);&$V($Dw);&$V($Jw);&$V($Lw);&$m3($Lw);&$V($Rw);&$m3($Rw);&$V($gx);&$V($mx);&$V($vx);&$V($Dx);&$V($Jx);&$V($Px);&$V($Zx);&$V($dy);&$m3($dy);&$V($ny);&$V($py);&$m3($py);&$V($Ry);&$V($lz);&$V($uz);&$V($Bz);&$V($Dz);&$m3($Dz);&$Gz($Dz);&$V($Rz);&$V($Tz);&$m3($Tz);&$V($mA);&$V($tA);&$V($NA);&$V($jB);&$V($lB);&$m3($lB);&$V($qB);&$m3($qB);&$V($IB);&$V($OB);&$m3($OB);&$V($xC);&$V($TC);&$V($oD);&$V($GD);&$V($dE);&$V($mE);&$V($wE);&$V($yE);&$m3($yE);&$V($DE);&$m3($DE);&$V($CF);&$V($LF);&$V($NF);&$m3($NF);&$V($SF);&$m3($SF);&$V($jG);&$V($sG);&$V($uG);&$m3($uG);&$V($zG);&$m3($zG);&$V($LG);&$m3($LG);&$V($jH);&$V($tH);&$V($zH);&$V($BH);&$m3($BH);&$V($HH);&$m3($HH);&$V($VH);&$V($XH);&$m3($XH);&$V($hI);&$V($oI);&$V($CI);&$V($EI);&$m3($EI);&$V($LI);&$m3($LI);&$V($NI);&$m3($NI);&$V($jJ);&$V($qJ);&$V($xJ);&$V($HJ);&$V($SJ);&$V($UJ);&$m3($UJ);&$XJ($UJ);&$V($jK);&$V($rK);&$V($tK);&$m3($tK);&$V($KK);&$V($ZK);&$V($iL);&$V($kL);&$m3($kL);&$V($pL);&$m3($pL);&$V($BL);&$V($HL);&$V($NL);&$V($PL);&$m3($PL);&$V($UL);&$m3($UL);&$V($dM);&$V($lM);&$m3($lM);&$V($qM);&$m3($qM);&$V($zM);&$m3($zM);&$V($EM);&$m3($EM);&$V($JM);&$m3($JM);&$V($RM);&$m3($RM);&$V($VM);&$m3($VM);ni->run(@ARGV);
__DATA__
