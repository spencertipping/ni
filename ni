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
  ni('ni:/child')->new('hello world!')->method1;
#;$h=[$f,$g];$i=q#description#;$j=q#ni:class is at the core of ni's object-oriented system, along with core
classes like ni:object and ni:metaclass. There are two layers of
abstraction involved here: Perl packages are modified by behaviors, and
classes encode the higher-level declarative features you'd expect from a
language like Ruby or Smalltalk. This documentation covers both layers.#;$k=[$i,$j];$l=q#behaviors#;$m=q#ni's objects are blessed Perl references, and behaviors are objects
that modify Perl packages in specific ways. The simplest is
ni:/lib/slice, which represents a set of methods you can add to a
package.#;$n=q#assertions#;$o=[];$p=q#error#;$q=undef;$r=q#outcome#;$s=q#test#;$t=q#annotations#;$u=[];$v=q#closure#;$w=q#code#;$x=q#my $fn = fn q{"hi"};
my $slice = ni('ni:/lib/slice')->new('myslice', f => $fn);
$slice->apply('foo');
now foo->f == 'hi';#;$y=q#proto#;$z=q##;$A=q#lib/fn#;$B=bless({$t,$u,$v,$q,$w,$x,$y,$z},$A);$C=q#lib/test_case#;$D=bless({$n,$o,$p,$q,$r,$q,$s,$B},$C);$E=q#TODO...#;$F=[$l,$m,$D,$E];$G=q#classes#;$H=q#ni implements a Smalltalk 80-style metaclass system with a couple of
differences. First, ni's classes are slice unions and as such don't
support colliding methods; and second, they support multiple inheritance.
These two points are related: method overriding isn't in the picture,
which makes multiple inheritance straightforward to implement.#;$I=[$G,$H,$E];$J=[$h,$k,$F,$I];$K=q#name#;$L=q#/class#;$M=q#lib/doc#;$N=bless({$e,$J,$K,$L},$M);$O=q#my $s = shift; ni->def($s->name, $s)#;$P=bless({$w,$O,$y,$z},$A);$Q=q#ni.doc:/fabric#;$R=q#Abstractions to bridge the gaps between separate machines and processes.
This module is designed to make it appear as though all resources are
local, or at least can be referred to locally -- even when they belong to
an external process (e.g. a Hadoop mapper) or another machine (e.g. a
file over SSH). If we can bidirectionally communicate with a remote ni
instance, then we can see its resources.#;$S=q#The fabric layer consists of a couple of things. First, we've got RMI
support code that proxies any method call and return value(s) over a
full-duplex data channel. Second, we have an async event loop that
handles multiplexed IO using a single thread.#;$T=[$i,$R,$S];$U=[$T];$V=q#/fabric#;$W=bless({$e,$U,$K,$V},$M);$X=q#ni.doc:/fabric/remote#;$Y=q#A local proxy for a remote object. All method calls are converted to RMI
wrappers and return futures of results.#;$Z=[$i,$Y];$c1=[$Z];$d1=q#/fabric/remote#;$e1=bless({$e,$c1,$K,$d1},$M);$f1=q#ni.doc:/fabric/rmi#;$g1=q#An open connection to another ni instance. This module provides the
ability to bootstrap a remote perl interpreter with an async RMI event
loop, and provides options about send/receive data encoding (which is
important for trust/security reasons).#;$h1=q#Method calls are proxied to named objects on the remote, and each one is
sent in a packet that contains the fully-serialized context for the
call.#;$i1=[$i,$g1,$h1];$j1=q#TODO#;$k1=q#referent#;$l1=q#applied_to#;$m1=q#fabric/rmi#;$n1={$m1,1};$o1=q#/fabric/rmi#;$p1=q#slices#;$q1=q#io/buffer#;$r1=q#io/cat#;$s1=q#io/exec#;$t1=q#io/fd#;$u1=q#io/file#;$v1=q#io/file_update_fd#;$w1=q#io/null#;$x1=q#io/object#;$y1=q#io/pid#;$z1=q#io/str#;$A1={$m1,1,$q1,1,$r1,1,$s1,1,$t1,1,$u1,1,$v1,1,$w1,1,$x1,1,$y1,1,$z1,1};$B1=q#/io/object#;$C1=q#class#;$D1=q#class.c#;$E1=q#fabric/remote#;$F1=q#fabric/remote.c#;$G1=q#fabric/rmi.c#;$H1=q#io/buffer.c#;$I1=q#io/cat.c#;$J1=q#io/exec.c#;$K1=q#io/fd.c#;$L1=q#io/file.c#;$M1=q#io/file_update_fd.c#;$N1=q#io/null.c#;$O1=q#io/object.c#;$P1=q#io/pid.c#;$Q1=q#io/str.c#;$R1=q#io/transfer#;$S1=q#io/transfer.c#;$T1=q#io/transfer_async#;$U1=q#io/transfer_async.c#;$V1=q#io/transfer_sync#;$W1=q#io/transfer_sync.c#;$X1=q#lib/behavior#;$Y1=q#lib/behavior.c#;$Z1=q#lib/branch#;$c2=q#lib/branch.c#;$d2=q#lib/dataslice#;$e2=q#lib/dataslice.c#;$f2=q#lib/doc.c#;$g2=q#lib/fn.c#;$h2=q#lib/future#;$i2=q#lib/future.c#;$j2=q#lib/image#;$k2=q#lib/image.c#;$l2=q#lib/ni#;$m2=q#lib/ni.c#;$n2=q#lib/object_metadata#;$o2=q#lib/object_metadata.c#;$p2=q#lib/quote_simple#;$q2=q#lib/quote_simple.c#;$r2=q#lib/slice#;$s2=q#lib/slice.c#;$t2=q#lib/tag#;$u2=q#lib/tag.c#;$v2=q#lib/test_assert_eq#;$w2=q#lib/test_assert_eq.c#;$x2=q#lib/test_assertion#;$y2=q#lib/test_assertion.c#;$z2=q#lib/test_case.c#;$A2=q#lib/test_value#;$B2=q#lib/test_value.c#;$C2=q#lib/todo#;$D2=q#lib/todo.c#;$E2=q#metaclass#;$F2=q#metaclass.c#;$G2=q#module#;$H2=q#module.c#;$I2=q#object#;$J2=q#object.c#;$K2=q#semantic/dimension#;$L2=q#semantic/dimension.c#;$M2=q#semantic/task#;$N2=q#semantic/task.c#;$O2={$C1,1,$D1,1,$E1,1,$F1,1,$m1,1,$G1,1,$q1,1,$H1,1,$r1,1,$I1,1,$s1,1,$J1,1,$t1,1,$K1,1,$u1,1,$L1,1,$v1,1,$M1,1,$w1,1,$N1,1,$x1,1,$O1,1,$y1,1,$P1,1,$z1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$X1,1,$Y1,1,$Z1,1,$c2,1,$d2,1,$e2,1,$M,1,$f2,1,$A,1,$g2,1,$h2,1,$i2,1,$j2,1,$k2,1,$l2,1,$m2,1,$n2,1,$o2,1,$p2,1,$q2,1,$r2,1,$s2,1,$t2,1,$u2,1,$v2,1,$w2,1,$x2,1,$y2,1,$C,1,$z2,1,$A2,1,$B2,1,$C2,1,$D2,1,$E2,1,$F2,1,$G2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$N2,1};$P2=q#/object#;$Q2={};$R2=q#ctor#;$S2=q#dtor#;$T2=q#methods#;$U2=q#DESTROY#;$V2=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$W2=bless({$w,$V2,$y,$z},$A);$X2=q#ni 'ni:/' . ref shift#;$Y2=bless({$w,$X2,$y,$z},$A);$Z2={$U2,$W2,$C1,$Y2};$c3=q#/lib/instance.b#;$d3=bless({$l1,$Q2,$R2,$q,$S2,$q,$T2,$Z2,$K,$c3},$r2);$e3=[$d3];$f3=bless({$l1,$O2,$K,$P2,$p1,$e3},$J2);$g3=q#my $s = shift; $s->apply($s->package)#;$h3=bless({$w,$g3,$y,$z},$A);$i3={};$j3=q#(bool#;$k3=[];$l3=bless({$t,$k3,$v,$q,$w,1,$y,$z},$A);$m3={$j3,$l3};$n3=q#/io/object_ops.b#;$o3=bless({$l1,$i3,$R2,$q,$S2,$q,$T2,$m3,$K,$n3},$r2);$p3={};$q3=q#die#;$r3=[];$s3=q#shift; die join " ", @_#;$t3=bless({$t,$r3,$v,$q,$w,$s3,$y,$z},$A);$u3=q#io_check#;$v3=[];$w3=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$x3=bless({$t,$v3,$v,$q,$w,$w3,$y,$z},$A);$y3=q#io_check_defined#;$z3=[];$A3=q#shift->io_check(sub {defined shift}, @_)#;$B3=bless({$t,$z3,$v,$q,$w,$A3,$y,$z},$A);$C3=q#io_check_true#;$D3=[];$E3=q#shift->io_check(sub {shift}, @_)#;$F3=bless({$t,$D3,$v,$q,$w,$E3,$y,$z},$A);$G3={$q3,$t3,$u3,$x3,$y3,$B3,$C3,$F3};$H3=q#/io/object_checks.b#;$I3=bless({$l1,$p3,$R2,$q,$S2,$q,$T2,$G3,$K,$H3},$r2);$J3={};$K3=q#(+#;$L3=[];$M3=q#ni('ni:/io/cat')->new(@_[0, 1])#;$N3=bless({$t,$L3,$v,$q,$w,$M3,$y,$z},$A);$O3={$K3,$N3};$P3=q#/io/object_constructors.b#;$Q3=bless({$l1,$J3,$R2,$q,$S2,$q,$T2,$O3,$K,$P3},$r2);$R3={};$S3=q#read_all#;$T3=[];$U3=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$V3=bless({$t,$T3,$v,$q,$w,$U3,$y,$z},$A);$W3=q#write_all#;$X3=[];$Y3=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$Z3=bless({$t,$X3,$v,$q,$w,$Y3,$y,$z},$A);$c4={$S3,$V3,$W3,$Z3};$d4=q#/io/object_memory.b#;$e4=bless({$l1,$R3,$R2,$q,$S2,$q,$T2,$c4,$K,$d4},$r2);$f4={};$g4=q#connect_sync#;$h4=[];$i4=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$j4=bless({$t,$h4,$v,$q,$w,$i4,$y,$z},$A);$k4=q#into_sync#;$l4=[];$m4=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$n4=bless({$t,$l4,$v,$q,$w,$m4,$y,$z},$A);$o4={$g4,$j4,$k4,$n4};$p4=q#/io/object_transfer_sync.b#;$q4=bless({$l1,$f4,$R2,$q,$S2,$q,$T2,$o4,$K,$p4},$r2);$r4={};$s4=q#connect_async#;$t4=[];$u4=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$v4=bless({$t,$t4,$v,$q,$w,$u4,$y,$z},$A);$w4=q#into_async#;$x4=[];$y4=q#ni('ni:/io/transfer_async')->new(@_)->run#;$z4=bless({$t,$x4,$v,$q,$w,$y4,$y,$z},$A);$A4={$s4,$v4,$w4,$z4};$B4=q#/io/object_transfer_async.b#;$C4=bless({$l1,$r4,$R2,$q,$S2,$q,$T2,$A4,$K,$B4},$r2);$D4=[$f3,$o3,$I3,$Q3,$e4,$q4,$C4,$C4,$q4,$C4,$q4];$E4=bless({$l1,$A1,$K,$B1,$p1,$D4},$O1);$F4={};$G4=q#instantiate#;$H4=[];$I4=q#@slots#;$J4=q#arg_codec#;$K4=q#return_codec#;$L4=q#image#;$M4=[$J4,$K4,$L4];$N4=q#generator#;$O4=[];$P4=q#(arg_codec    => ni('ni:/fabric/qperl')->new,
          return_codec => ni('ni:/fabric/qjson')->new,
          image        => 1)#;$Q4=bless({$t,$O4,$v,$q,$w,$P4,$y,$z},$A);$R4={$I4,$M4,$N4,$Q4};$S4=q#my %defaults = &$generator(@_);
my $class    = shift;
my %args     = @_;
$defaults{$_} = $args{$_} for @slots;
\\%defaults;#;$T4=bless({$t,$H4,$v,$R4,$w,$S4,$y,$z},$A);$U4={$G4,$T4};$V4=q#/fabric/rmi_init.b#;$W4=bless({$l1,$F4,$R2,$q,$S2,$q,$T2,$U4,$K,$V4},$r2);$X4=[$E4,$W4];$Y4=bless({$l1,$n1,$K,$o1,$p1,$X4},$G1);$Z4=q#todo#;$c5=q#The codec stuff is awful. Nothing about a codec is so global that it
should apply to all method calls; maybe a class has a per-method
codec disposition, or maybe we infer the best one by serialization
performance. Either way, global codecs don't make sense here.#;$d5=[$c5];$e5=bless({$k1,$Y4,$Z4,$d5},$C2);$f5=[$j1,$e5];$g5=[$i1,$f5];$h5=bless({$e,$g5,$K,$o1},$M);$i5=q#ni.doc:/io#;$j5=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$k5=[$i,$j5];$l5=[$k5];$m5=q#/io#;$n5=bless({$e,$l5,$K,$m5},$M);$o5=q#ni.doc:/io/buffer#;$p5=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$q5=[$f,$p5];$r5=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$s5=[];$t5=[];$u5=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$v5=bless({$t,$t5,$v,$q,$w,$u5,$y,$z},$A);$w5=bless({$n,$s5,$p,$q,$r,$q,$s,$v5},$C);$x5=[$i,$r5,$w5];$y5=[$q5,$x5];$z5=q#/io/buffer#;$A5=bless({$e,$y5,$K,$z5},$M);$B5=q#ni.doc:/io/cat#;$C5=q#
  my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
  my $combined = $io1 + $io2 + $io3;
  $combined->into_sync($destination_io);
#;$D5=[$f,$C5];$E5=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$F5=[];$G5=[];$H5=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$I5=bless({$t,$G5,$v,$q,$w,$H5,$y,$z},$A);$J5=bless({$n,$F5,$p,$q,$r,$q,$s,$I5},$C);$K5=[$i,$E5,$J5];$L5=[$D5,$K5];$M5=q#/io/cat#;$N5=bless({$e,$L5,$K,$M5},$M);$O5=q#ni.doc:/io/exec#;$P5=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$Q5=[$f,$P5];$R5=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$S5=[];$T5=[];$U5=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$V5=bless({$t,$T5,$v,$q,$w,$U5,$y,$z},$A);$W5=bless({$n,$S5,$p,$q,$r,$q,$s,$V5},$C);$X5=[$i,$R5,$W5];$Y5=[$Q5,$X5];$Z5=q#/io/exec#;$c6=bless({$e,$Y5,$K,$Z5},$M);$d6=q#ni.doc:/io/fd#;$e6=q#
  open my $fh, ...;
  my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
  my $fd = ni('ni:/io/fd')->new(0);   \# from number
  my $fd = ni('fd:0');                \# same thing
  $fd->nonblock(1)->read($_, 100);
  $fd->be(10);                        \# move FD number
#;$f6=[$f,$e6];$g6=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$h6=[];$i6=[];$j6=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$k6=bless({$t,$i6,$v,$q,$w,$j6,$y,$z},$A);$l6=bless({$n,$h6,$p,$q,$r,$q,$s,$k6},$C);$m6=[$i,$g6,$l6];$n6=[$f6,$m6];$o6=q#/io/fd#;$p6=bless({$e,$n6,$K,$o6},$M);$q6=q#ni.doc:/io/file#;$r6=q#
  my $f = ni('ni:/io/file')->new('/etc/passwd');
  my $f = ni('file:/etc/passwd');     \# same as above
  $f->into_sync(ni('fd:1'));          \# cat to stdout
#;$s6=[$f,$r6];$t6=q#warning#;$u6=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$v6=[$t6,$u6];$w6=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$x6=[];$y6=[];$z6=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$A6=bless({$t,$y6,$v,$q,$w,$z6,$y,$z},$A);$B6=bless({$n,$x6,$p,$q,$r,$q,$s,$A6},$C);$C6=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$D6=[];$E6=[];$F6=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$G6=bless({$t,$E6,$v,$q,$w,$F6,$y,$z},$A);$H6=bless({$n,$D6,$p,$q,$r,$q,$s,$G6},$C);$I6=[$i,$w6,$B6,$C6,$H6];$J6=[$s6,$v6,$I6];$K6=q#/io/file#;$L6=bless({$e,$J6,$K,$K6},$M);$M6=q#ni.doc:/io/file_update_fd#;$N6=q#A write fd that performs a file rename upon closing.#;$O6=[$i,$N6];$P6=[$O6];$Q6=q#/io/file_update_fd#;$R6=bless({$e,$P6,$K,$Q6},$M);$S6=q#ni.doc:/io/pid#;$T6=q#eg#;$U6=[];$V6={$y1,1};$W6=q#/io/pid#;$X6={};$Y6=q#pid#;$Z6=[];$c7=q#shift->{'pid'}#;$d7=bless({$t,$Z6,$v,$q,$w,$c7,$y,$z},$A);$e7=q#status#;$f7=[];$g7=q#shift->{'status'}#;$h7=bless({$t,$f7,$v,$q,$w,$g7,$y,$z},$A);$i7={$Y6,$d7,$e7,$h7};$j7=q#/io/pid_readers.b#;$k7=bless({$l1,$X6,$R2,$q,$S2,$q,$T2,$i7,$K,$j7},$r2);$l7={};$m7=[];$n7=q#shift->await#;$o7=bless({$t,$m7,$v,$q,$w,$n7,$y,$z},$A);$p7=[];$q7=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$r7=bless({$t,$p7,$v,$q,$w,$q7,$y,$z},$A);$s7={$G4,$r7};$t7=q#/io/pid_init.b#;$u7=bless({$l1,$l7,$R2,$q,$S2,$o7,$T2,$s7,$K,$t7},$r2);$v7={};$w7=q#await#;$x7=[];$y7=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$z7=bless({$t,$x7,$v,$q,$w,$y7,$y,$z},$A);$A7=q#running#;$B7=[];$C7=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$D7=bless({$t,$B7,$v,$q,$w,$C7,$y,$z},$A);$E7={$w7,$z7,$A7,$D7};$F7=q#/io/pid_wait.b#;$G7=bless({$l1,$v7,$R2,$q,$S2,$q,$T2,$E7,$K,$F7},$r2);$H7={};$I7=q#read#;$J7=[];$K7=q#shift->stdout->read(@_)#;$L7=bless({$t,$J7,$v,$q,$w,$K7,$y,$z},$A);$M7=q#write#;$N7=[];$O7=q#shift->stdin->write(@_)#;$P7=bless({$t,$N7,$v,$q,$w,$O7,$y,$z},$A);$Q7={$I7,$L7,$M7,$P7};$R7=q#/io/pid_io.b#;$S7=bless({$l1,$H7,$R2,$q,$S2,$q,$T2,$Q7,$K,$R7},$r2);$T7={};$U7=q#fd#;$V7=[];$W7=q#$_[0]->{external_fds}{$_[1]}#;$X7=bless({$t,$V7,$v,$q,$w,$W7,$y,$z},$A);$Y7=q#stderr#;$Z7=[];$c8=q#shift->fd(2)#;$d8=bless({$t,$Z7,$v,$q,$w,$c8,$y,$z},$A);$e8=q#stdin#;$f8=[];$g8=q#shift->fd(0)#;$h8=bless({$t,$f8,$v,$q,$w,$g8,$y,$z},$A);$i8=q#stdout#;$j8=[];$k8=q#shift->fd(1)#;$l8=bless({$t,$j8,$v,$q,$w,$k8,$y,$z},$A);$m8={$U7,$X7,$Y7,$d8,$e8,$h8,$i8,$l8};$n8=q#/io/pid_accessors.b#;$o8=bless({$l1,$T7,$R2,$q,$S2,$q,$T2,$m8,$K,$n8},$r2);$p8=[$E4,$k7,$u7,$G7,$S7,$o8];$q8=bless({$l1,$V6,$K,$W6,$p1,$p8},$P1);$r8=[];$s8=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$t8=bless({$t,$r8,$v,$q,$w,$s8,$y,$z},$A);$u8=bless({$n,$U6,$p,$q,$r,$q,$k1,$q8,$s,$t8},$C);$v8=[$T6,$u8];$w8=[];$x8=[];$y8=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$z8=bless({$t,$x8,$v,$q,$w,$y8,$y,$z},$A);$A8=bless({$n,$w8,$p,$q,$r,$q,$k1,$q8,$s,$z8},$C);$B8=[$T6,$A8];$C8=[];$D8=[];$E8=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$F8=bless({$t,$D8,$v,$q,$w,$E8,$y,$z},$A);$G8=bless({$n,$C8,$p,$q,$r,$q,$k1,$q8,$s,$F8},$C);$H8=[$T6,$G8];$I8=[$v8,$B8,$H8];$J8=bless({$e,$I8,$K,$W6},$M);$K8=q#ni.doc:/lib#;$L8=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$M8=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$N8=[$i,$L8,$M8];$O8=[$N8];$P8=q#/lib#;$Q8=bless({$e,$O8,$K,$P8},$M);$R8=q#ni.doc:/lib/doc#;$S8=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ...#;$T8=[$f,$S8];$U8=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$V8=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$W8=[];$X8=[];$Y8=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$Z8=bless({$t,$X8,$v,$q,$w,$Y8,$y,$z},$A);$c9=bless({$n,$W8,$p,$q,$r,$q,$s,$Z8},$C);$d9=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$e9=[];$f9=[];$g9=q#my $doc = ni("ni:/object")->child("doctest_foo")->doc;
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$h9=bless({$t,$f9,$v,$q,$w,$g9,$y,$z},$A);$i9=bless({$n,$e9,$p,$q,$r,$q,$s,$h9},$C);$j9=[$i,$U8,$V8,$c9,$d9,$i9];$k9=[$T8,$j9];$l9=q#/lib/doc#;$m9=bless({$e,$k9,$K,$l9},$M);$n9=q#ni.doc:/lib/future#;$o9=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$p9=[];$q9=[];$r9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$s9=bless({$t,$q9,$v,$q,$w,$r9,$y,$z},$A);$t9=bless({$n,$p9,$p,$q,$r,$q,$s,$s9},$C);$u9=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$v9=[];$w9=[];$x9=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$y9=bless({$t,$w9,$v,$q,$w,$x9,$y,$z},$A);$z9=bless({$n,$v9,$p,$q,$r,$q,$s,$y9},$C);$A9=[$i,$o9,$t9,$u9,$z9];$B9=[$A9];$C9=q#/lib/future#;$D9=bless({$e,$B9,$K,$C9},$M);$E9=q#ni.doc:/lib/image#;$F9=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$G9=[$f,$F9];$H9=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$I9=[$i,$H9];$J9={$j2,1};$K9=q#/lib/image#;$L9={};$M9=[];$N9=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$O9=bless({$t,$M9,$v,$q,$w,$N9,$y,$z},$A);$P9={$G4,$O9};$Q9=q#/lib/image_init.b#;$R9=bless({$l1,$L9,$R2,$q,$S2,$q,$T2,$P9,$K,$Q9},$r2);$S9={};$T9=q#boot_side_effect#;$U9=[];$V9=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$W9=bless({$t,$U9,$v,$q,$w,$V9,$y,$z},$A);$X9=q#finalizer#;$Y9=[];$Z9=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$ca=bless({$t,$Y9,$v,$q,$w,$Z9,$y,$z},$A);$da=q#io#;$ea=[];$fa=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$ga=bless({$t,$ea,$v,$q,$w,$fa,$y,$z},$A);$ha=q#reconstruction#;$ia=[];$ja=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$ka=bless({$t,$ia,$v,$q,$w,$ja,$y,$z},$A);$la=q#side_effect#;$ma=[];$na=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$oa=bless({$t,$ma,$v,$q,$w,$na,$y,$z},$A);$pa={$T9,$W9,$X9,$ca,$da,$ga,$ha,$ka,$la,$oa};$qa=q#/lib/image_quoting.b#;$ra=bless({$l1,$S9,$R2,$q,$S2,$q,$T2,$pa,$K,$qa},$r2);$sa={};$ta=q#quote_code#;$ua=[];$va=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$wa=bless({$t,$ua,$v,$q,$w,$va,$y,$z},$A);$xa={$ta,$wa};$ya=q#/lib/quote_code_fail.b#;$za=bless({$l1,$sa,$R2,$q,$S2,$q,$T2,$xa,$K,$ya},$r2);$Aa={};$Ba=q#quote_array#;$Ca=[];$Da=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Ea=bless({$t,$Ca,$v,$q,$w,$Da,$y,$z},$A);$Fa=q#quote_hash#;$Ga=[];$Ha=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Ia=bless({$t,$Ga,$v,$q,$w,$Ha,$y,$z},$A);$Ja=q#quote_scalar#;$Ka=[];$La=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Ma=bless({$t,$Ka,$v,$q,$w,$La,$y,$z},$A);$Na=q#quote_scalar_ref#;$Oa=[];$Pa=q#'\\\\' . shift->quote(${$_[0]})#;$Qa=bless({$t,$Oa,$v,$q,$w,$Pa,$y,$z},$A);$Ra=q#quote_value#;$Sa=[];$Ta=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Ua=bless({$t,$Sa,$v,$q,$w,$Ta,$y,$z},$A);$Va={$Ba,$Ea,$Fa,$Ia,$Ja,$Ma,$Na,$Qa,$Ra,$Ua};$Wa=q#/lib/quote_values.b#;$Xa=bless({$l1,$Aa,$R2,$q,$S2,$q,$T2,$Va,$K,$Wa},$r2);$Ya={};$Za=q#quote_blessed#;$cb=[];$db=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$eb=bless({$t,$cb,$v,$q,$w,$db,$y,$z},$A);$fb=q#quote_class#;$gb=[];$hb=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$ib=bless({$t,$gb,$v,$q,$w,$hb,$y,$z},$A);$jb=q#quote_object#;$kb=[];$lb=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$mb=bless({$t,$kb,$v,$q,$w,$lb,$y,$z},$A);$nb={$Za,$eb,$fb,$ib,$jb,$mb};$ob=q#/lib/quote_objects.b#;$pb=bless({$l1,$Ya,$R2,$q,$S2,$q,$T2,$nb,$K,$ob},$r2);$qb={};$rb=q#circular_arrayref#;$sb=[];$tb=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$ub=bless({$t,$sb,$v,$q,$w,$tb,$y,$z},$A);$vb=q#circular_hashref#;$wb=[];$xb=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$yb=bless({$t,$wb,$v,$q,$w,$xb,$y,$z},$A);$zb=q#is_circular#;$Ab=[];$Bb=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Cb=bless({$t,$Ab,$v,$q,$w,$Bb,$y,$z},$A);$Db={$rb,$ub,$vb,$yb,$zb,$Cb};$Eb=q#/lib/quote_circular_addressed.b#;$Fb=bless({$l1,$qb,$R2,$q,$S2,$q,$T2,$Db,$K,$Eb},$r2);$Gb={};$Hb=q#address#;$Ib=[];$Jb=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Kb=bless({$t,$Ib,$v,$q,$w,$Jb,$y,$z},$A);$Lb=q#allocate_gensym#;$Mb=[];$Nb=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Ob=bless({$t,$Mb,$v,$q,$w,$Nb,$y,$z},$A);$Pb=q#circular_links#;$Qb=[];$Rb=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Sb=bless({$t,$Qb,$v,$q,$w,$Rb,$y,$z},$A);$Tb=q#quote#;$Ub=[];$Vb=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Wb=bless({$t,$Ub,$v,$q,$w,$Vb,$y,$z},$A);$Xb={$Hb,$Kb,$Lb,$Ob,$Pb,$Sb,$Tb,$Wb};$Yb=q#/lib/quote_gensym_identity.b#;$Zb=bless({$l1,$Gb,$R2,$q,$S2,$q,$T2,$Xb,$K,$Yb},$r2);$cc={};$dc=q#gensym#;$ec=[];$fc=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$gc=bless({$t,$ec,$v,$q,$w,$fc,$y,$z},$A);$hc={$dc,$gc};$ic=q#/lib/gensym_generator_compact.b#;$jc=bless({$l1,$cc,$R2,$q,$S2,$q,$T2,$hc,$K,$ic},$r2);$kc=[$f3,$R9,$ra,$za,$Xa,$pb,$Fb,$Zb,$jc];$lc=bless({$l1,$J9,$K,$K9,$p1,$kc},$k2);$mc=q#The interaction between these ->serialize methods and quotation/image
classes is horrible and even worse, undocumented. Formalize the protocol
for instances to request things like side effects or finalizers, and for
god's sake clean up the side-effect/boot-side-effect crap.#;$nc=[$mc];$oc=bless({$k1,$lc,$Z4,$nc},$C2);$pc=[$j1,$oc];$qc=[$G9,$I9,$pc];$rc=bless({$e,$qc,$K,$K9},$M);$sc=q#ni.doc:/lib/ni#;$tc=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$uc=[$f,$tc];$vc=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$wc=[$i,$vc];$xc=[$uc,$wc];$yc=q#/lib/ni#;$zc=bless({$e,$xc,$K,$yc},$M);$Ac=q#ni.doc:/lib/quote_simple#;$Bc=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$Cc=[];$Dc=[];$Ec=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$Fc=bless({$t,$Dc,$v,$q,$w,$Ec,$y,$z},$A);$Gc=bless({$n,$Cc,$p,$q,$r,$q,$s,$Fc},$C);$Hc=[$i,$Bc,$Gc];$Ic=[$Hc];$Jc=q#/lib/quote_simple#;$Kc=bless({$e,$Ic,$K,$Jc},$M);$Lc=q#ni.doc:/lib/slice#;$Mc=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$Nc=[$f,$Mc];$Oc=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$Pc=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$Qc=[];$Rc=[];$Sc=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$Tc=bless({$t,$Rc,$v,$q,$w,$Sc,$y,$z},$A);$Uc=bless({$n,$Qc,$p,$q,$r,$q,$s,$Tc},$C);$Vc=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$Wc=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$Xc=[];$Yc=[];$Zc=q#my $instances = 0;
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
now $instances == 0;#;$cd=bless({$t,$Yc,$v,$q,$w,$Zc,$y,$z},$A);$dd=bless({$n,$Xc,$p,$q,$r,$q,$s,$cd},$C);$ed=[$i,$Oc,$Pc,$Uc,$Vc,$Wc,$dd];$fd=[$Nc,$ed];$gd=q#/lib/slice#;$hd=bless({$e,$fd,$K,$gd},$M);$id=q#ni.doc:/semantic#;$jd=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$kd=[$i,$jd];$ld=[$kd];$md=q#/semantic#;$nd=bless({$e,$ld,$K,$md},$M);$od=q#ni:/class#;$pd={$C1,1,$D1,1,$F1,1,$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$S1,1,$U1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$i2,1,$k2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$z2,1,$B2,1,$D2,1,$F2,1,$H2,1,$J2,1,$K2,1,$L2,1,$N2,1};$qd={$C1,1,$D1,1,$F1,1,$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$S1,1,$U1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$i2,1,$k2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$z2,1,$B2,1,$D2,1,$E2,1,$F2,1,$G2,1,$H2,1,$J2,1,$K2,1,$L2,1,$N2,1};$rd=q#/module#;$sd=q#/lib/perlbranch.b#;$td={};$ud=q#add#;$vd=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$wd=bless({$w,$vd,$y,$z},$A);$xd=q#apply#;$yd=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$zd=bless({$w,$yd,$y,$z},$A);$Ad={$ud,$wd,$xd,$zd};$Bd=q#/lib/branch.b#;$Cd=bless({$l1,$td,$R2,$q,$S2,$q,$T2,$Ad,$K,$Bd},$r2);$Dd={};$Ed=q#$_[0]->namespace . ":" . $_[0]->{name}#;$Fd=bless({$w,$Ed,$y,$z},$A);$Gd={$K,$Fd};$Hd=q#/lib/named.b#;$Id=bless({$l1,$Dd,$R2,$P,$S2,$q,$T2,$Gd,$K,$Hd},$r2);$Jd={};$Kd=q#namespace#;$Ld=q#'ni'#;$Md=bless({$w,$Ld,$y,$z},$A);$Nd={$Kd,$Md};$Od=q#/lib/named_in_ni.b#;$Pd=bless({$l1,$Jd,$R2,$q,$S2,$q,$T2,$Nd,$K,$Od},$r2);$Qd={};$Rd=q#package#;$Sd=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$Td=bless({$w,$Sd,$y,$z},$A);$Ud={$Rd,$Td};$Vd=q#/lib/namespaced.b#;$Wd=bless({$l1,$Qd,$R2,$q,$S2,$q,$T2,$Ud,$K,$Vd},$r2);$Xd={};$Yd=q#resolve#;$Zd=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$ce=bless({$w,$Zd,$y,$z},$A);$de={$Yd,$ce};$ee=q#/lib/resolver.b#;$fe=bless({$l1,$Xd,$R2,$q,$S2,$q,$T2,$de,$K,$ee},$r2);$ge=[$Cd,$Id,$Pd,$Wd,$fe];$he=bless({$K,$sd,$p1,$ge},$t2);$ie={};$je=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$ke=bless({$w,$je,$y,$z},$A);$le={$G4,$ke};$me=q#/lib/class_init.b#;$ne=bless({$l1,$ie,$R2,$h3,$S2,$q,$T2,$le,$K,$me},$r2);$oe={$C1,1,$D1,1,$F1,1,$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$S1,1,$U1,1,$W1,1,$X1,1,$Y1,1,$Z1,1,$c2,1,$d2,1,$e2,1,$f2,1,$g2,1,$i2,1,$k2,1,$m2,1,$o2,1,$q2,1,$r2,1,$s2,1,$t2,1,$u2,1,$w2,1,$y2,1,$z2,1,$B2,1,$D2,1,$E2,1,$F2,1,$G2,1,$H2,1,$J2,1,$K2,1,$L2,1,$N2,1};$pe=q#/lib/behavior#;$qe={};$re=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$se=bless({$w,$re,$y,$z},$A);$te={$e,$se};$ue=q#/lib/documentable.b#;$ve=bless({$l1,$qe,$R2,$q,$S2,$q,$T2,$te,$K,$ue},$r2);$we=[$f3,$ve];$xe=bless({$l1,$oe,$K,$pe,$p1,$we},$Y1);$ye={$C1,1,$D1,1,$F1,1,$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$S1,1,$U1,1,$W1,1,$Y1,1,$Z1,1,$c2,1,$e2,1,$f2,1,$g2,1,$i2,1,$k2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$z2,1,$B2,1,$D2,1,$E2,1,$F2,1,$G2,1,$H2,1,$J2,1,$K2,1,$L2,1,$N2,1};$ze=q#/lib/definition.b#;$Ae={};$Be=q#def#;$Ce=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$De=bless({$w,$Ce,$y,$z},$A);$Ee={$Be,$De};$Fe=q#/lib/definition_def.b#;$Ge=bless({$l1,$Ae,$R2,$q,$S2,$q,$T2,$Ee,$K,$Fe},$r2);$He={};$Ie=q#ro#;$Je=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$Ke=bless({$w,$Je,$y,$z},$A);$Le=q#rw#;$Me=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$Ne=bless({$w,$Me,$y,$z},$A);$Oe={$Ie,$Ke,$Le,$Ne};$Pe=q#/lib/accessor.b#;$Qe=bless({$l1,$He,$R2,$q,$S2,$q,$T2,$Oe,$K,$Pe},$r2);$Re={};$Se=q#(""#;$Te=q#shift->name#;$Ue=bless({$w,$Te,$y,$z},$A);$Ve={$Se,$Ue};$We=q#/lib/name_as_string.b#;$Xe=bless({$l1,$Re,$R2,$q,$S2,$q,$T2,$Ve,$K,$We},$r2);$Ye={};$Ze=q#(eq#;$cf=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$df=bless({$w,$cf,$y,$z},$A);$ef={$Ze,$df};$ff=q#/lib/ref_eq.b#;$gf=bless({$l1,$Ye,$R2,$q,$S2,$q,$T2,$ef,$K,$ff},$r2);$hf={};$if=q#defdata#;$jf=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$kf=bless({$w,$jf,$y,$z},$A);$lf={$if,$kf};$mf=q#/lib/definition_defdata.b#;$nf=bless({$l1,$hf,$R2,$q,$S2,$q,$T2,$lf,$K,$mf},$r2);$of={};$pf=q#instantiate_with_defaults#;$qf=q#my ($class, @slots) = @_;
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
    }));#;$rf=bless({$w,$qf,$y,$z},$A);$sf={$pf,$rf};$tf=q#/lib/definition_init_with_defaults.b#;$uf=bless({$l1,$of,$R2,$q,$S2,$q,$T2,$sf,$K,$tf},$r2);$vf=[$Ge,$Qe,$Xe,$gf,$nf,$uf];$wf=bless({$l1,$ye,$K,$ze,$p1,$vf},$Z1);$xf=[$he,$ne,$f3,$xe,$wf];$yf=bless({$l1,$qd,$K,$rd,$p1,$xf},$H2);$zf={};$Af=q#new#;$Bf=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$Cf=bless({$w,$Bf,$y,$z},$A);$Df={$Af,$Cf};$Ef=q#/lib/instantiable.b#;$Ff=bless({$l1,$zf,$T2,$Df,$K,$Ef},$r2);$Gf={};$Hf=q#child#;$If=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$Jf=bless({$w,$If,$y,$z},$A);$Kf={$Hf,$Jf};$Lf=q#/lib/subclass.b#;$Mf=bless({$l1,$Gf,$R2,$q,$S2,$q,$T2,$Kf,$K,$Lf},$r2);$Nf=[$yf,$Ff,$ne,$yf,$Mf];$Of=bless({$l1,$pd,$K,$L,$p1,$Nf},$D1);$Pf=q#ni:/class.c#;$Qf={$D1,1,$L2,1};$Rf=q#/class.c#;$Sf={$D1,1,$H2,1,$L2,1};$Tf=q#/module.c#;$Uf={$D1,1,$F1,1,$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$S1,1,$U1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$i2,1,$k2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$z2,1,$B2,1,$D2,1,$H2,1,$J2,1,$L2,1,$N2,1};$Vf=q#/object.c#;$Wf=[$Of];$Xf=bless({$l1,$Uf,$K,$Vf,$p1,$Wf},$E2);$Yf={$D1,1,$Y1,1,$c2,1,$e2,1,$s2,1,$u2,1,$H2,1,$L2,1};$Zf=q#/lib/behavior.c#;$cg=[$Xf];$dg=bless({$l1,$Yf,$K,$Zf,$p1,$cg},$E2);$eg=[$Xf,$Ff,$dg];$fg=bless({$l1,$Sf,$K,$Tf,$p1,$eg},$E2);$gg=[$fg];$hg=bless({$l1,$Qf,$K,$Rf,$p1,$gg},$E2);$ig=q#ni:/fabric/remote#;$jg={$E1,1};$kg={};$lg=[];$mg=q#my ($class, $rmi, $name) = @_;
+{rmi  => $rmi,
  name => $name};#;$ng=bless({$t,$lg,$v,$q,$w,$mg,$y,$z},$A);$og={$G4,$ng};$pg=q#/fabric/remote_init.b#;$qg=bless({$l1,$kg,$R2,$q,$S2,$q,$T2,$og,$K,$pg},$r2);$rg={};$sg=q#AUTOLOAD#;$tg=[];$ug=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{rmi}->call($$self{name}, $method, @_);#;$vg=bless({$t,$tg,$v,$q,$w,$ug,$y,$z},$A);$wg={$sg,$vg};$xg=q#/fabric/remote_proxy.b#;$yg=bless({$l1,$rg,$R2,$q,$S2,$q,$T2,$wg,$K,$xg},$r2);$zg=[$f3,$qg,$yg];$Ag=bless({$l1,$jg,$K,$d1,$p1,$zg},$F1);$Bg=q#ni:/fabric/remote.c#;$Cg={$F1,1};$Dg=q#/fabric/remote.c#;$Eg=[$Xf];$Fg=bless({$l1,$Cg,$K,$Dg,$p1,$Eg},$E2);$Gg=q#ni:/fabric/remote_init.b#;$Hg=q#ni:/fabric/remote_proxy.b#;$Ig=q#ni:/fabric/rmi#;$Jg=q#ni:/fabric/rmi.c#;$Kg={$G1,1};$Lg=q#/fabric/rmi.c#;$Mg={$G1,1,$H1,1,$I1,1,$J1,1,$K1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1};$Ng=q#/io/object.c#;$Og={};$Pg=q#def_transfer_method#;$Qg=[];$Rg=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Sg=bless({$t,$Qg,$v,$q,$w,$Rg,$y,$z},$A);$Tg={$Pg,$Sg};$Ug=q#/io/object.c_transfer_def.b#;$Vg=bless({$l1,$Og,$R2,$q,$S2,$q,$T2,$Tg,$K,$Ug},$r2);$Wg=[$Xf,$Vg];$Xg=bless({$l1,$Mg,$K,$Ng,$p1,$Wg},$E2);$Yg=[$Xg];$Zg=bless({$l1,$Kg,$K,$Lg,$p1,$Yg},$E2);$ch=q#ni:/fabric/rmi_init.b#;$dh=q#ni:/io/buffer#;$eh={$q1,1};$fh={};$gh=[];$hh=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$ih=bless({$t,$gh,$v,$q,$w,$hh,$y,$z},$A);$jh={$G4,$ih};$kh=q#/io/buffer_init.b#;$lh=bless({$l1,$fh,$R2,$q,$S2,$q,$T2,$jh,$K,$kh},$r2);$mh={};$nh=[];$oh=q#my $self = shift;
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
}#;$ph=bless({$t,$nh,$v,$q,$w,$oh,$y,$z},$A);$qh=q#read_capacity#;$rh=[];$sh=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$th=bless({$t,$rh,$v,$q,$w,$sh,$y,$z},$A);$uh=[];$vh=q#my $self = shift;
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
}#;$wh=bless({$t,$uh,$v,$q,$w,$vh,$y,$z},$A);$xh=q#write_capacity#;$yh=[];$zh=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Ah=bless({$t,$yh,$v,$q,$w,$zh,$y,$z},$A);$Bh={$I7,$ph,$qh,$th,$M7,$wh,$xh,$Ah};$Ch=q#/io/buffer_io.b#;$Dh=bless({$l1,$mh,$R2,$q,$S2,$q,$T2,$Bh,$K,$Ch},$r2);$Eh=[$E4,$lh,$Dh];$Fh=bless({$l1,$eh,$K,$z5,$p1,$Eh},$H1);$Gh=q#ni:/io/buffer.c#;$Hh={$H1,1};$Ih=q#/io/buffer.c#;$Jh=[$Xg];$Kh=bless({$l1,$Hh,$K,$Ih,$p1,$Jh},$E2);$Lh=q#ni:/io/buffer_init.b#;$Mh=q#ni:/io/buffer_io.b#;$Nh=q#ni:/io/cat#;$Oh={$r1,1};$Ph={};$Qh=[];$Rh=q#shift; +{fs => [@_]}#;$Sh=bless({$t,$Qh,$v,$q,$w,$Rh,$y,$z},$A);$Th={$G4,$Sh};$Uh=q#/io/cat_init.b#;$Vh=bless({$l1,$Ph,$R2,$q,$S2,$q,$T2,$Th,$K,$Uh},$r2);$Wh={};$Xh=[];$Yh=q#my $fs = shift->{fs};
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
$total_read;#;$Zh=bless({$t,$Xh,$v,$q,$w,$Yh,$y,$z},$A);$ci={$I7,$Zh};$di=q#/io/cat_read.b#;$ei=bless({$l1,$Wh,$R2,$q,$S2,$q,$T2,$ci,$K,$di},$r2);$fi=[$E4,$Vh,$ei];$gi=bless({$l1,$Oh,$K,$M5,$p1,$fi},$I1);$hi=q#ni:/io/cat.c#;$ii={$I1,1};$ji=q#/io/cat.c#;$ki=[$Xg];$li=bless({$l1,$ii,$K,$ji,$p1,$ki},$E2);$mi=q#ni:/io/cat_init.b#;$ni=q#ni:/io/cat_read.b#;$oi=q#ni:/io/exec#;$pi={$s1,1};$qi={};$ri=q#argv#;$si=[];$ti=q#shift->{'argv'}#;$ui=bless({$t,$si,$v,$q,$w,$ti,$y,$z},$A);$vi={$ri,$ui};$wi=q#/io/exec_ro.b#;$xi=bless({$l1,$qi,$R2,$q,$S2,$q,$T2,$vi,$K,$wi},$r2);$yi={};$zi=[];$Ai=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Bi=bless({$t,$zi,$v,$q,$w,$Ai,$y,$z},$A);$Ci={$G4,$Bi};$Di=q#/io/exec_init.b#;$Ei=bless({$l1,$yi,$R2,$q,$S2,$q,$T2,$Ci,$K,$Di},$r2);$Fi={};$Gi=q#connect#;$Hi=[];$Ii=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Ji=bless({$t,$Hi,$v,$q,$w,$Ii,$y,$z},$A);$Ki=q#in_pipe#;$Li=[];$Mi=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Ni=bless({$t,$Li,$v,$q,$w,$Mi,$y,$z},$A);$Oi=q#out_pipe#;$Pi=[];$Qi=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Ri=bless({$t,$Pi,$v,$q,$w,$Qi,$y,$z},$A);$Si=q#setup_stdio#;$Ti=[];$Ui=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Vi=bless({$t,$Ti,$v,$q,$w,$Ui,$y,$z},$A);$Wi={$Gi,$Ji,$Ki,$Ni,$Oi,$Ri,$Si,$Vi};$Xi=q#/io/exec_io_setup.b#;$Yi=bless({$l1,$Fi,$R2,$q,$S2,$q,$T2,$Wi,$K,$Xi},$r2);$Zi={};$cj=q#binds_fd#;$dj=[];$ej=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$fj=bless({$t,$dj,$v,$q,$w,$ej,$y,$z},$A);$gj=[];$hj=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$ij=bless({$t,$gj,$v,$q,$w,$hj,$y,$z},$A);$jj=[];$kj=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$lj=bless({$t,$jj,$v,$q,$w,$kj,$y,$z},$A);$mj=[];$nj=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$oj=bless({$t,$mj,$v,$q,$w,$nj,$y,$z},$A);$pj=[];$qj=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$rj=bless({$t,$pj,$v,$q,$w,$qj,$y,$z},$A);$sj={$cj,$fj,$U7,$ij,$Y7,$lj,$e8,$oj,$i8,$rj};$tj=q#/io/exec_io_accessors.b#;$uj=bless({$l1,$Zi,$R2,$q,$S2,$q,$T2,$sj,$K,$tj},$r2);$vj={};$wj=q#env#;$xj=[];$yj=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$zj=bless({$t,$xj,$v,$q,$w,$yj,$y,$z},$A);$Aj={$wj,$zj};$Bj=q#/io/exec_env.b#;$Cj=bless({$l1,$vj,$R2,$q,$S2,$q,$T2,$Aj,$K,$Bj},$r2);$Dj={};$Ej=q#exec#;$Fj=[];$Gj=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Hj=bless({$t,$Fj,$v,$q,$w,$Gj,$y,$z},$A);$Ij=q#fork#;$Jj=[];$Kj=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Lj=bless({$t,$Jj,$v,$q,$w,$Kj,$y,$z},$A);$Mj=q#move_fds#;$Nj=[];$Oj=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Pj=bless({$t,$Nj,$v,$q,$w,$Oj,$y,$z},$A);$Qj={$Ej,$Hj,$Ij,$Lj,$Mj,$Pj};$Rj=q#/io/exec_fork.b#;$Sj=bless({$l1,$Dj,$R2,$q,$S2,$q,$T2,$Qj,$K,$Rj},$r2);$Tj=[$E4,$xi,$Ei,$Yi,$uj,$Cj,$Sj];$Uj=bless({$l1,$pi,$K,$Z5,$p1,$Tj},$J1);$Vj=q#ni:/io/exec.c#;$Wj={$J1,1};$Xj=q#/io/exec.c#;$Yj=[$Xg];$Zj=bless({$l1,$Wj,$K,$Xj,$p1,$Yj},$E2);$ck=q#ni:/io/exec_env.b#;$dk=q#ni:/io/exec_fork.b#;$ek=q#ni:/io/exec_init.b#;$fk=q#ni:/io/exec_io_accessors.b#;$gk=q#ni:/io/exec_io_setup.b#;$hk=q#ni:/io/exec_ro.b#;$ik=q#ni:/io/fd#;$jk={$t1,1};$kk=q#read_fd_mask#;$lk={};$mk=[];$nk=q#shift->{'fd'}#;$ok=bless({$t,$mk,$v,$q,$w,$nk,$y,$z},$A);$pk={$U7,$ok};$qk=q#/io/fd_readers.b#;$rk=bless({$l1,$lk,$R2,$q,$S2,$q,$T2,$pk,$K,$qk},$r2);$sk={};$tk=[];$uk=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$vk=bless({$t,$tk,$v,$q,$w,$uk,$y,$z},$A);$wk={$G4,$vk};$xk=q#/io/fd_init.b#;$yk=bless({$l1,$sk,$R2,$q,$S2,$q,$T2,$wk,$K,$xk},$r2);$zk={};$Ak=q#be#;$Bk=[];$Ck=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Dk=bless({$t,$Bk,$v,$q,$w,$Ck,$y,$z},$A);$Ek={$Ak,$Dk};$Fk=q#/io/fd_shell.b#;$Gk=bless({$l1,$zk,$R2,$q,$S2,$q,$T2,$Ek,$K,$Fk},$r2);$Hk={};$Ik=q#cloexec#;$Jk=[];$Kk=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Lk=bless({$t,$Jk,$v,$q,$w,$Kk,$y,$z},$A);$Mk=q#fcntl_flag#;$Nk=[];$Ok=q#my ($self, $flag, $value) = @_;
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
}#;$Pk=bless({$t,$Nk,$v,$q,$w,$Ok,$y,$z},$A);$Qk=q#nonblock#;$Rk=[];$Sk=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Tk=bless({$t,$Rk,$v,$q,$w,$Sk,$y,$z},$A);$Uk={$Ik,$Lk,$Mk,$Pk,$Qk,$Tk};$Vk=q#/io/fd_fcntl.b#;$Wk=bless({$l1,$Hk,$R2,$q,$S2,$q,$T2,$Uk,$K,$Vk},$r2);$Xk={};$Yk=[];$Zk=q#shift->close#;$cl=bless({$t,$Yk,$v,$q,$w,$Zk,$y,$z},$A);$dl=q#close#;$el=[];$fl=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$gl=bless({$t,$el,$v,$q,$w,$fl,$y,$z},$A);$hl={$dl,$gl};$il=q#/io/fd_gc.b#;$jl=bless({$l1,$Xk,$R2,$q,$S2,$cl,$T2,$hl,$K,$il},$r2);$kl={};$ll=q#close_perl_ios#;$ml=[];$nl=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$ol=bless({$t,$ml,$v,$q,$w,$nl,$y,$z},$A);$pl=[];$ql=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$rl=bless({$t,$pl,$v,$q,$w,$ql,$y,$z},$A);$sl=[];$tl=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$ul=bless({$t,$sl,$v,$q,$w,$tl,$y,$z},$A);$vl={$ll,$ol,$I7,$rl,$M7,$ul};$wl=q#/io/fd_perlio.b#;$xl=bless({$l1,$kl,$R2,$q,$S2,$q,$T2,$vl,$K,$wl},$r2);$yl=[$E4,$rk,$yk,$Gk,$Wk,$jl,$xl];$zl=q#write_fd_mask#;$Al=bless({$l1,$jk,$K,$o6,$kk,$z,$p1,$yl,$zl,$z},$K1);$Bl=[];$Cl=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Dl=bless({$t,$Bl,$v,$q,$w,$Cl,$y,$z},$A);$El=q#ni:/io/fd.c#;$Fl={$K1,1};$Gl=q#/io/fd.c#;$Hl={};$Il=q#clear_fd#;$Jl=[];$Kl=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$Ll=bless({$t,$Jl,$v,$q,$w,$Kl,$y,$z},$A);$Ml=q#read_fd#;$Nl=[];$Ol=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Pl=bless({$t,$Nl,$v,$q,$w,$Ol,$y,$z},$A);$Ql=q#select#;$Rl=[];$Sl=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Tl=bless({$t,$Rl,$v,$q,$w,$Sl,$y,$z},$A);$Ul=q#write_fd#;$Vl=[];$Wl=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Xl=bless({$t,$Vl,$v,$q,$w,$Wl,$y,$z},$A);$Yl={$Il,$Ll,$Ml,$Pl,$Ql,$Tl,$Ul,$Xl};$Zl=q#/io/fd.c_selector.b#;$cm=bless({$l1,$Hl,$R2,$Dl,$S2,$q,$T2,$Yl,$K,$Zl},$r2);$dm=[$Xg,$cm];$em=bless({$l1,$Fl,$K,$Gl,$p1,$dm},$E2);$fm=q#ni:/io/fd.c_selector.b#;$gm=q#ni:/io/fd_fcntl.b#;$hm=q#ni:/io/fd_gc.b#;$im=q#ni:/io/fd_init.b#;$jm=q#ni:/io/fd_perlio.b#;$km=q#ni:/io/fd_readers.b#;$lm=q#ni:/io/fd_shell.b#;$mm=q#ni:/io/file#;$nm={$u1,1};$om={};$pm=[];$qm=q#shift->{'name'}#;$rm=bless({$t,$pm,$v,$q,$w,$qm,$y,$z},$A);$sm={$K,$rm};$tm=q#/io/file_readers.b#;$um=bless({$l1,$om,$R2,$q,$S2,$q,$T2,$sm,$K,$tm},$r2);$vm={};$wm=q#mode#;$xm=[];$ym=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$zm=bless({$t,$xm,$v,$q,$w,$ym,$y,$z},$A);$Am={$wm,$zm};$Bm=q#/io/file_accessors.b#;$Cm=bless({$l1,$vm,$R2,$q,$S2,$q,$T2,$Am,$K,$Bm},$r2);$Dm={};$Em=[];$Fm=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Gm=bless({$t,$Em,$v,$q,$w,$Fm,$y,$z},$A);$Hm={$G4,$Gm};$Im=q#/io/file_init.b#;$Jm=bless({$l1,$Dm,$R2,$q,$S2,$q,$T2,$Hm,$K,$Im},$r2);$Km={};$Lm=q#(-X#;$Mm=[];$Nm=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Om=bless({$t,$Mm,$v,$q,$w,$Nm,$y,$z},$A);$Pm=q#mv#;$Qm=[];$Rm=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Sm=bless({$t,$Qm,$v,$q,$w,$Rm,$y,$z},$A);$Tm=q#rm#;$Um=[];$Vm=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Wm=bless({$t,$Um,$v,$q,$w,$Vm,$y,$z},$A);$Xm={$Lm,$Om,$Pm,$Sm,$Tm,$Wm};$Ym=q#/io/file_fns.b#;$Zm=bless({$l1,$Km,$R2,$q,$S2,$q,$T2,$Xm,$K,$Ym},$r2);$cn={};$dn=q#atomic_update#;$en=[];$fn=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$gn=bless({$t,$en,$v,$q,$w,$fn,$y,$z},$A);$hn={$dn,$gn};$in=q#/io/file_update.b#;$jn=bless({$l1,$cn,$R2,$q,$S2,$q,$T2,$hn,$K,$in},$r2);$kn={};$ln=[];$mn=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$nn=bless({$t,$ln,$v,$q,$w,$mn,$y,$z},$A);$on=q#r#;$pn=[];$qn=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$rn=bless({$t,$pn,$v,$q,$w,$qn,$y,$z},$A);$sn=[];$tn=q#shift->r->read(@_)#;$un=bless({$t,$sn,$v,$q,$w,$tn,$y,$z},$A);$vn=q#w#;$wn=[];$xn=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$yn=bless({$t,$wn,$v,$q,$w,$xn,$y,$z},$A);$zn=[];$An=q#shift->w->write(@_)#;$Bn=bless({$t,$zn,$v,$q,$w,$An,$y,$z},$A);$Cn={$dl,$nn,$on,$rn,$I7,$un,$vn,$yn,$M7,$Bn};$Dn=q#/io/file_io.b#;$En=bless({$l1,$kn,$R2,$q,$S2,$q,$T2,$Cn,$K,$Dn},$r2);$Fn=[$E4,$um,$Cm,$Jm,$Zm,$jn,$En];$Gn=bless({$l1,$nm,$K,$K6,$p1,$Fn},$L1);$Hn=q#ni:/io/file.c#;$In={$L1,1};$Jn=q#/io/file.c#;$Kn=[$Xg];$Ln=bless({$l1,$In,$K,$Jn,$p1,$Kn},$E2);$Mn=q#ni:/io/file_accessors.b#;$Nn=q#ni:/io/file_fns.b#;$On=q#ni:/io/file_init.b#;$Pn=q#ni:/io/file_io.b#;$Qn=q#ni:/io/file_readers.b#;$Rn=q#ni:/io/file_update.b#;$Sn=q#ni:/io/file_update_fd#;$Tn={$v1,1};$Un={};$Vn=[];$Wn=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Xn=bless({$t,$Vn,$v,$q,$w,$Wn,$y,$z},$A);$Yn={$G4,$Xn};$Zn=q#/io/file_update_fd_fd_init.b#;$co=bless({$l1,$Un,$R2,$q,$S2,$q,$T2,$Yn,$K,$Zn},$r2);$do={};$eo=[];$fo=bless({$t,$eo,$v,$q,$w,$Zk,$y,$z},$A);$go=[];$ho=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$io=bless({$t,$go,$v,$q,$w,$ho,$y,$z},$A);$jo={$dl,$io};$ko=q#/io/file_update_fd_fd_gc.b#;$lo=bless({$l1,$do,$R2,$q,$S2,$fo,$T2,$jo,$K,$ko},$r2);$mo=[$E4,$rk,$Wk,$xl,$co,$lo];$no=bless({$l1,$Tn,$K,$Q6,$p1,$mo},$M1);$oo=q#ni:/io/file_update_fd.c#;$po={$M1,1};$qo=q#/io/file_update_fd.c#;$ro=[$Xg];$so=bless({$l1,$po,$K,$qo,$p1,$ro},$E2);$to=q#ni:/io/file_update_fd_fd_gc.b#;$uo=q#ni:/io/file_update_fd_fd_init.b#;$vo=q#ni:/io/named_io_fns.b#;$wo={};$xo=q#fcntl#;$yo=[];$zo=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Ao=bless({$t,$yo,$v,$q,$w,$zo,$y,$z},$A);$Bo=[];$Co=q#CORE::fork#;$Do=bless({$t,$Bo,$v,$q,$w,$Co,$y,$z},$A);$Eo=q#open2#;$Fo=[];$Go=q#CORE::open $_[0], $_[1]#;$Ho=bless({$t,$Fo,$v,$q,$w,$Go,$y,$z},$A);$Io=q#rename#;$Jo=[];$Ko=q#CORE::rename $_[0], $_[1]#;$Lo=bless({$t,$Jo,$v,$q,$w,$Ko,$y,$z},$A);$Mo=q#unlink#;$No=[];$Oo=q#CORE::unlink @_#;$Po=bless({$t,$No,$v,$q,$w,$Oo,$y,$z},$A);$Qo=q#waitpid#;$Ro=[];$So=q#CORE::waitpid $_[0], $_[1]#;$To=bless({$t,$Ro,$v,$q,$w,$So,$y,$z},$A);$Uo={$xo,$Ao,$Ij,$Do,$Eo,$Ho,$Io,$Lo,$Mo,$Po,$Qo,$To};$Vo=q#/io/named_io_fns.b#;$Wo=bless({$l1,$wo,$R2,$q,$S2,$q,$T2,$Uo,$K,$Vo},$r2);$Xo=q#main#;$Yo=q#ni:/io/null#;$Zo={$w1,1};$cp=q#/io/null#;$dp={};$ep=[];$fp=q#+{fd => undef}#;$gp=bless({$t,$ep,$v,$q,$w,$fp,$y,$z},$A);$hp={$G4,$gp};$ip=q#/io/null_init.b#;$jp=bless({$l1,$dp,$R2,$q,$S2,$q,$T2,$hp,$K,$ip},$r2);$kp={};$lp=[];$mp=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$np=bless({$t,$lp,$v,$q,$w,$mp,$y,$z},$A);$op=[];$pp=q#shift->fd->read(@_)#;$qp=bless({$t,$op,$v,$q,$w,$pp,$y,$z},$A);$rp=[];$sp=q#shift->fd->write(@_)#;$tp=bless({$t,$rp,$v,$q,$w,$sp,$y,$z},$A);$up={$U7,$np,$I7,$qp,$M7,$tp};$vp=q#/io/null_io.b#;$wp=bless({$l1,$kp,$R2,$q,$S2,$q,$T2,$up,$K,$vp},$r2);$xp=[$E4,$jp,$wp];$yp=bless({$l1,$Zo,$K,$cp,$p1,$xp},$N1);$zp=q#ni:/io/null.c#;$Ap={$N1,1};$Bp=q#/io/null.c#;$Cp=[$Xg];$Dp=bless({$l1,$Ap,$K,$Bp,$p1,$Cp},$E2);$Ep=q#ni:/io/null_init.b#;$Fp=q#ni:/io/null_io.b#;$Gp=q#ni:/io/object#;$Hp=q#ni:/io/object.c#;$Ip=q#ni:/io/object.c_transfer_def.b#;$Jp=q#ni:/io/object_checks.b#;$Kp=q#ni:/io/object_constructors.b#;$Lp=q#ni:/io/object_memory.b#;$Mp=q#ni:/io/object_ops.b#;$Np=q#ni:/io/object_transfer_async.b#;$Op=q#ni:/io/object_transfer_sync.b#;$Pp=q#ni:/io/pid#;$Qp=q#ni:/io/pid.c#;$Rp={$P1,1};$Sp=q#/io/pid.c#;$Tp=[$Xg];$Up=bless({$l1,$Rp,$K,$Sp,$p1,$Tp},$E2);$Vp=q#ni:/io/pid_accessors.b#;$Wp=q#ni:/io/pid_init.b#;$Xp=q#ni:/io/pid_io.b#;$Yp=q#ni:/io/pid_readers.b#;$Zp=q#ni:/io/pid_wait.b#;$cq=q#ni:/io/str#;$dq={$z1,1};$eq=q#/io/str#;$fq={};$gq=q#data#;$hq=[];$iq=q#shift->{'data'}#;$jq=bless({$t,$hq,$v,$q,$w,$iq,$y,$z},$A);$kq=q#end#;$lq=[];$mq=q#shift->{'end'}#;$nq=bless({$t,$lq,$v,$q,$w,$mq,$y,$z},$A);$oq=q#start#;$pq=[];$qq=q#shift->{'start'}#;$rq=bless({$t,$pq,$v,$q,$w,$qq,$y,$z},$A);$sq={$gq,$jq,$kq,$nq,$oq,$rq};$tq=q#/io/str_ro.b#;$uq=bless({$l1,$fq,$R2,$q,$S2,$q,$T2,$sq,$K,$tq},$r2);$vq={};$wq=[];$xq=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$yq=bless({$t,$wq,$v,$q,$w,$xq,$y,$z},$A);$zq={$G4,$yq};$Aq=q#/io/str_init.b#;$Bq=bless({$l1,$vq,$R2,$q,$S2,$q,$T2,$zq,$K,$Aq},$r2);$Cq={};$Dq=[];$Eq=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Fq=bless({$t,$Dq,$v,$q,$w,$Eq,$y,$z},$A);$Gq=q#remaining#;$Hq=[];$Iq=q#my $self = shift; $$self{end} - $$self{start}#;$Jq=bless({$t,$Hq,$v,$q,$w,$Iq,$y,$z},$A);$Kq=[];$Lq=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Mq=bless({$t,$Kq,$v,$q,$w,$Lq,$y,$z},$A);$Nq={$I7,$Fq,$Gq,$Jq,$M7,$Mq};$Oq=q#/io/str_io.b#;$Pq=bless({$l1,$Cq,$R2,$q,$S2,$q,$T2,$Nq,$K,$Oq},$r2);$Qq=[$E4,$uq,$Bq,$Pq];$Rq=bless({$l1,$dq,$K,$eq,$p1,$Qq},$Q1);$Sq=q#ni:/io/str.c#;$Tq={$Q1,1};$Uq=q#/io/str.c#;$Vq=[$Xg];$Wq=bless({$l1,$Tq,$K,$Uq,$p1,$Vq},$E2);$Xq=q#ni:/io/str_init.b#;$Yq=q#ni:/io/str_io.b#;$Zq=q#ni:/io/str_ro.b#;$cr=q#ni:/io/transfer#;$dr={$R1,1,$T1,1,$V1,1};$er=q#/io/transfer#;$fr={$R1,1,$T1,1,$V1,1,$M2,1};$gr=q#/semantic/task#;$hr={};$ir=[];$jr=q#shift->{'outcome'}#;$kr=bless({$t,$ir,$v,$q,$w,$jr,$y,$z},$A);$lr={$r,$kr};$mr=q#/semantic/task_ro.b#;$nr=bless({$l1,$hr,$R2,$q,$S2,$q,$T2,$lr,$K,$mr},$r2);$or={};$pr=q#failure#;$qr=[];$rr=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$sr=bless({$t,$qr,$v,$q,$w,$rr,$y,$z},$A);$tr=q#success#;$ur=[];$vr=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$wr=bless({$t,$ur,$v,$q,$w,$vr,$y,$z},$A);$xr={$pr,$sr,$tr,$wr};$yr=q#/semantic/task_outcome.b#;$zr=bless({$l1,$or,$R2,$q,$S2,$q,$T2,$xr,$K,$yr},$r2);$Ar=[$f3,$nr,$zr];$Br=bless({$l1,$fr,$K,$gr,$p1,$Ar},$N2);$Cr={};$Dr=[];$Er=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Fr=bless({$t,$Dr,$v,$q,$w,$Er,$y,$z},$A);$Gr=[];$Hr=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Ir=bless({$t,$Gr,$v,$q,$w,$Hr,$y,$z},$A);$Jr=[];$Kr=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Lr=bless({$t,$Jr,$v,$q,$w,$Kr,$y,$z},$A);$Mr={$I7,$Ir,$M7,$Lr};$Nr=q#/io/transfer_io_interop.b#;$Or=bless({$l1,$Cr,$R2,$Fr,$S2,$q,$T2,$Mr,$K,$Nr},$r2);$Pr={};$Qr=q#pressure#;$Rr=[];$Sr=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Tr=bless({$t,$Rr,$v,$q,$w,$Sr,$y,$z},$A);$Ur=q#read_limit_throughput#;$Vr=[];$Wr=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Xr=bless({$t,$Vr,$v,$q,$w,$Wr,$y,$z},$A);$Yr=q#throughput#;$Zr=[];$cs=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$ds=bless({$t,$Zr,$v,$q,$w,$cs,$y,$z},$A);$es=q#write_limit_throughput#;$fs=[];$gs=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$hs=bless({$t,$fs,$v,$q,$w,$gs,$y,$z},$A);$is={$Qr,$Tr,$Ur,$Xr,$Yr,$ds,$es,$hs};$js=q#/io/transfer_io_measurement.b#;$ks=bless({$l1,$Pr,$R2,$q,$S2,$q,$T2,$is,$K,$js},$r2);$ls=[$Br,$Or,$ks];$ms=bless({$l1,$dr,$K,$er,$p1,$ls},$S1);$ns=[];$os=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$ps=bless({$t,$ns,$v,$q,$w,$os,$y,$z},$A);$qs=q#ni:/io/transfer.c#;$rs={$S1,1,$U1,1,$W1,1};$ss=q#/io/transfer.c#;$ts={$S1,1,$U1,1,$W1,1,$N2,1};$us=q#/semantic/task.c#;$vs=[$Xf];$ws=bless({$l1,$ts,$K,$us,$p1,$vs},$E2);$xs={};$ys={};$zs=q#/io/transfer.c_into.b#;$As=bless({$l1,$xs,$R2,$ps,$S2,$q,$T2,$ys,$K,$zs},$r2);$Bs=[$ws,$As];$Cs=bless({$l1,$rs,$K,$ss,$p1,$Bs},$E2);$Ds=q#ni:/io/transfer.c_into.b#;$Es=q#ni:/io/transfer_async#;$Fs={$T1,1};$Gs=q#/io/transfer_async#;$Hs={};$Is=q#dest_io#;$Js=[];$Ks=q#shift->{'dest_io'}#;$Ls=bless({$t,$Js,$v,$q,$w,$Ks,$y,$z},$A);$Ms=q#id#;$Ns=[];$Os=q#shift->{'id'}#;$Ps=bless({$t,$Ns,$v,$q,$w,$Os,$y,$z},$A);$Qs=q#source_io#;$Rs=[];$Ss=q#shift->{'source_io'}#;$Ts=bless({$t,$Rs,$v,$q,$w,$Ss,$y,$z},$A);$Us={$Is,$Ls,$Ms,$Ps,$Qs,$Ts};$Vs=q#/io/transfer_async_ro.b#;$Ws=bless({$l1,$Hs,$R2,$q,$S2,$q,$T2,$Us,$K,$Vs},$r2);$Xs={};$Ys=[];$Zs=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$ct=bless({$t,$Ys,$v,$q,$w,$Zs,$y,$z},$A);$dt={$G4,$ct};$et=q#/io/transfer_async_init.b#;$ft=bless({$l1,$Xs,$R2,$q,$S2,$q,$T2,$dt,$K,$et},$r2);$gt={};$ht=[];$it=q#ni('ni:/io/transfer_async')->track(shift)#;$jt=bless({$t,$ht,$v,$q,$w,$it,$y,$z},$A);$kt=[];$lt=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$mt=bless({$t,$kt,$v,$q,$w,$lt,$y,$z},$A);$nt={};$ot=q#/io/transfer_async_lifecycle.b#;$pt=bless({$l1,$gt,$R2,$jt,$S2,$mt,$T2,$nt,$K,$ot},$r2);$qt={};$rt=q#run#;$st=[];$tt=q#shift#;$ut=bless({$t,$st,$v,$q,$w,$tt,$y,$z},$A);$vt=q#run_async#;$wt=[];$xt=q#my $self = shift;
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

$self;#;$yt=bless({$t,$wt,$v,$q,$w,$xt,$y,$z},$A);$zt={$rt,$ut,$vt,$yt};$At=q#/io/transfer_async_run.b#;$Bt=bless({$l1,$qt,$R2,$q,$S2,$q,$T2,$zt,$K,$At},$r2);$Ct=[$ms,$Ws,$ft,$pt,$Bt];$Dt=q#tracked_transfers#;$Et={};$Ft=q#transfer_id#;$Gt=bless({$l1,$Fs,$K,$Gs,$p1,$Ct,$Dt,$Et,$Ft,0},$U1);$Ht=[];$It=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Jt=bless({$t,$Ht,$v,$q,$w,$It,$y,$z},$A);$Kt=q#ni:/io/transfer_async.c#;$Lt={$U1,1};$Mt=q#/io/transfer_async.c#;$Nt={};$Ot=q#new_id#;$Pt=[];$Qt=q#++shift->{transfer_id}#;$Rt=bless({$t,$Pt,$v,$q,$w,$Qt,$y,$z},$A);$St=q#track#;$Tt=[];$Ut=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Vt=bless({$t,$Tt,$v,$q,$w,$Ut,$y,$z},$A);$Wt=q#untrack#;$Xt=[];$Yt=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Zt=bless({$t,$Xt,$v,$q,$w,$Yt,$y,$z},$A);$cu={$Ot,$Rt,$St,$Vt,$Wt,$Zt};$du=q#/io/transfer_async.c_tracker.b#;$eu=bless({$l1,$Nt,$R2,$Jt,$S2,$q,$T2,$cu,$K,$du},$r2);$fu=[$Cs,$eu];$gu=bless({$l1,$Lt,$K,$Mt,$p1,$fu},$E2);$hu=q#ni:/io/transfer_async.c_tracker.b#;$iu=q#ni:/io/transfer_async_init.b#;$ju=q#ni:/io/transfer_async_lifecycle.b#;$ku=q#ni:/io/transfer_async_ro.b#;$lu=q#ni:/io/transfer_async_run.b#;$mu=q#ni:/io/transfer_io_interop.b#;$nu=q#ni:/io/transfer_io_measurement.b#;$ou=q#ni:/io/transfer_sync#;$pu={$V1,1};$qu=q#/io/transfer_sync#;$ru={};$su=[];$tu=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$uu=bless({$t,$su,$v,$q,$w,$tu,$y,$z},$A);$vu={$G4,$uu};$wu=q#/io/transfer_sync_init.b#;$xu=bless({$l1,$ru,$R2,$q,$S2,$q,$T2,$vu,$K,$wu},$r2);$yu={};$zu=[];$Au=q#my $self = shift;
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
$self->success;#;$Bu=bless({$t,$zu,$v,$q,$w,$Au,$y,$z},$A);$Cu={$rt,$Bu};$Du=q#/io/transfer_sync_run.b#;$Eu=bless({$l1,$yu,$R2,$q,$S2,$q,$T2,$Cu,$K,$Du},$r2);$Fu=[$ms,$xu,$Eu];$Gu=bless({$l1,$pu,$K,$qu,$p1,$Fu},$W1);$Hu=q#ni:/io/transfer_sync.c#;$Iu={$W1,1};$Ju=q#/io/transfer_sync.c#;$Ku=[$Cs];$Lu=bless({$l1,$Iu,$K,$Ju,$p1,$Ku},$E2);$Mu=q#ni:/io/transfer_sync_init.b#;$Nu=q#ni:/io/transfer_sync_run.b#;$Ou=q#ni:/lib/accessor.b#;$Pu=q#ni:/lib/behavior#;$Qu=q#ni:/lib/behavior.c#;$Ru=q#ni:/lib/branch#;$Su={$Z1,1};$Tu=q#/lib/branch#;$Uu={};$Vu=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Wu=bless({$w,$Vu,$y,$z},$A);$Xu={$G4,$Wu};$Yu=q#/lib/branch_init.b#;$Zu=bless({$l1,$Uu,$R2,$q,$S2,$q,$T2,$Xu,$K,$Yu},$r2);$cv=[$xe,$Id,$Cd,$Zu,$wf];$dv=bless({$l1,$Su,$K,$Tu,$p1,$cv},$c2);$ev=q#ni:/lib/branch.b#;$fv=q#ni:/lib/branch.c#;$gv={$c2,1};$hv=q#/lib/branch.c#;$iv=[$dg];$jv=bless({$l1,$gv,$K,$hv,$p1,$iv},$E2);$kv=q#ni:/lib/branch_init.b#;$lv=q#ni:/lib/class_init.b#;$mv=q#ni:/lib/dataslice#;$nv={$d2,1};$ov=q#/lib/dataslice#;$pv={};$qv=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$rv=bless({$w,$qv,$y,$z},$A);$sv={$G4,$rv};$tv=q#/lib/dataslice_init.b#;$uv=bless({$l1,$pv,$R2,$q,$S2,$q,$T2,$sv,$K,$tv},$r2);$vv={};$wv=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$xv=bless({$w,$wv,$y,$z},$A);$yv={$xd,$xv};$zv=q#/lib/dataslice_apply.b#;$Av=bless({$l1,$vv,$R2,$q,$S2,$q,$T2,$yv,$K,$zv},$r2);$Bv=[$xe,$uv,$Av];$Cv=bless({$l1,$nv,$K,$ov,$p1,$Bv},$e2);$Dv=q#ni:/lib/dataslice.c#;$Ev={$e2,1};$Fv=q#/lib/dataslice.c#;$Gv=[$dg];$Hv=bless({$l1,$Ev,$K,$Fv,$p1,$Gv},$E2);$Iv=q#ni:/lib/dataslice_apply.b#;$Jv=q#ni:/lib/dataslice_init.b#;$Kv=q#ni:/lib/definition.b#;$Lv=q#ni:/lib/definition_def.b#;$Mv=q#ni:/lib/definition_defdata.b#;$Nv=q#ni:/lib/definition_init_with_defaults.b#;$Ov=q#ni:/lib/doc#;$Pv={$M,1};$Qv={};$Rv=q#shift; +{name => shift, doc => []}#;$Sv=bless({$w,$Rv,$y,$z},$A);$Tv={$G4,$Sv};$Uv=q#/lib/doc_init.b#;$Vv=bless({$l1,$Qv,$R2,$q,$S2,$q,$T2,$Tv,$K,$Uv},$r2);$Wv={};$Xv=q#'ni.doc'#;$Yv=bless({$w,$Xv,$y,$z},$A);$Zv={$Kd,$Yv};$cw=q#/lib/doc_namespace.b#;$dw=bless({$l1,$Wv,$R2,$q,$S2,$q,$T2,$Zv,$K,$cw},$r2);$ew={};$fw=q#(@{}#;$gw=q#[map @$_, @{shift->{doc}}]#;$hw=bless({$w,$gw,$y,$z},$A);$iw=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$jw=bless({$w,$iw,$y,$z},$A);$kw={$fw,$hw,$sg,$jw};$lw=q#/lib/doc_define.b#;$mw=bless({$l1,$ew,$R2,$q,$S2,$q,$T2,$kw,$K,$lw},$r2);$nw={};$ow=q#shift->referent#;$pw=bless({$w,$ow,$y,$z},$A);$qw=q#ni 'ni:' . shift->{name}#;$rw=bless({$w,$qw,$y,$z},$A);$sw={$kq,$pw,$k1,$rw};$tw=q#/lib/doc_end.b#;$uw=bless({$l1,$nw,$R2,$q,$S2,$q,$T2,$sw,$K,$tw},$r2);$vw={};$ww=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$xw=bless({$w,$ww,$y,$z},$A);$yw={$j1,$xw};$zw=q#/lib/doc_TODO.b#;$Aw=bless({$l1,$vw,$R2,$q,$S2,$q,$T2,$yw,$K,$zw},$r2);$Bw={};$Cw=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$Dw=bless({$w,$Cw,$y,$z},$A);$Ew={$T6,$Dw};$Fw=q#/lib/doc_eg.b#;$Gw=bless({$l1,$Bw,$R2,$q,$S2,$q,$T2,$Ew,$K,$Fw},$r2);$Hw={};$Iw=q#tests#;$Jw=q#grep ref($_) eq 'lib/test_case', @{$_[0]}#;$Kw=bless({$w,$Jw,$y,$z},$A);$Lw=q#todos#;$Mw=q#grep ref($_) eq 'lib/todo', @{$_[0]}#;$Nw=bless({$w,$Mw,$y,$z},$A);$Ow={$Iw,$Kw,$Lw,$Nw};$Pw=q#/lib/doc_process.b#;$Qw=bless({$l1,$Hw,$R2,$q,$S2,$q,$T2,$Ow,$K,$Pw},$r2);$Rw=[$f3,$Id,$Vv,$dw,$mw,$uw,$Aw,$Gw,$Qw];$Sw=bless({$l1,$Pv,$K,$l9,$p1,$Rw},$f2);$Tw=q#ni:/lib/doc.c#;$Uw={$f2,1};$Vw=q#/lib/doc.c#;$Ww={};$Xw=q#defannotation#;$Yw=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Zw=bless({$w,$Yw,$y,$z},$A);$cx={$Xw,$Zw};$dx=q#/lib/doc.c_defannotation.b#;$ex=bless({$l1,$Ww,$R2,$q,$S2,$q,$T2,$cx,$K,$dx},$r2);$fx=[$Xf,$ex];$gx=bless({$l1,$Uw,$K,$Vw,$p1,$fx},$E2);$hx=q#ni:/lib/doc.c_defannotation.b#;$ix=q#ni:/lib/doc_TODO.b#;$jx=q#ni:/lib/doc_define.b#;$kx=q#ni:/lib/doc_eg.b#;$lx=q#ni:/lib/doc_end.b#;$mx=q#ni:/lib/doc_init.b#;$nx=q#ni:/lib/doc_namespace.b#;$ox=q#ni:/lib/doc_process.b#;$px=q#ni:/lib/documentable.b#;$qx=q#ni:/lib/fn#;$rx={$A,1};$sx=q#/lib/fn#;$tx=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$ux=bless({$w,$tx,$y,$z},$A);$vx=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$wx=bless({$w,$vx},$A);$xx=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$yx=bless({$w,$xx},$A);$zx=q#lib/fn::closure#;$Ax=q#lib/fn::compile#;$Bx=q#lib/fn::instantiate#;$Cx={};$Dx=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Ex=bless({$w,$Dx,$y,$z},$A);$Fx=q#compile#;$Gx={$v,$ux,$Fx,$wx,$G4,$yx};$Hx=q#/lib/fn_init.b#;$Ix=bless({$l1,$Cx,$R2,$q,$S2,$Ex,$T2,$Gx,$K,$Hx},$r2);$Jx={};$Kx=[];$Lx=q#shift->{'annotations'}#;$Mx=bless({$t,$Kx,$v,$q,$w,$Lx,$y,$z},$A);$Nx=[];$Ox=q#shift->{'code'}#;$Px=bless({$t,$Nx,$v,$q,$w,$Ox,$y,$z},$A);$Qx=q#eval_number#;$Rx=[];$Sx=q#shift->{'eval_number'}#;$Tx=bless({$t,$Rx,$v,$q,$w,$Sx,$y,$z},$A);$Ux=q#fn#;$Vx=[];$Wx=q#shift->{'fn'}#;$Xx=bless({$t,$Vx,$v,$q,$w,$Wx,$y,$z},$A);$Yx={$t,$Mx,$w,$Px,$Qx,$Tx,$Ux,$Xx};$Zx=q#/lib/fn_ro.b#;$cy=bless({$l1,$Jx,$R2,$q,$S2,$q,$T2,$Yx,$K,$Zx},$r2);$dy={};$ey=[];$fy=q#my $self = shift; "fn {$$self{code}}"#;$gy=bless({$t,$ey,$v,$q,$w,$fy,$y,$z},$A);$hy=[];$iy=bless({$t,$hy,$v,$q,$w,$cf,$y,$z},$A);$jy={$Se,$gy,$Ze,$iy};$ky=q#/lib/fn_ops.b#;$ly=bless({$l1,$dy,$R2,$q,$S2,$q,$T2,$jy,$K,$ky},$r2);$my={};$ny=q#serialize#;$oy=[];$py=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$qy=bless({$t,$oy,$v,$q,$w,$py,$y,$z},$A);$ry={$ny,$qy};$sy=q#/lib/fn_serialize.b#;$ty=bless({$l1,$my,$R2,$q,$S2,$q,$T2,$ry,$K,$sy},$r2);$uy=[$f3,$Ff,$Ix,$cy,$ly,$ty];$vy=bless({$l1,$rx,$K,$sx,$p1,$uy},$g2);$wy=[];$xy=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$yy=bless({$t,$wy,$v,$q,$w,$xy,$y,$z},$A);$zy=q#ni:/lib/fn.c#;$Ay={$g2,1};$By=q#/lib/fn.c#;$Cy={};$Dy=q#resolve_evals#;$Ey=[];$Fy=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Gy=bless({$t,$Ey,$v,$q,$w,$Fy,$y,$z},$A);$Hy={$Dy,$Gy};$Iy=q#/lib/fn.c_resolve_eval.b#;$Jy=bless({$l1,$Cy,$R2,$yy,$S2,$q,$T2,$Hy,$K,$Iy},$r2);$Ky=[$Xf,$Jy];$Ly=bless({$l1,$Ay,$K,$By,$p1,$Ky},$E2);$My=q#ni:/lib/fn.c_resolve_eval.b#;$Ny=q#ni:/lib/fn_init.b#;$Oy=q#ni:/lib/fn_ops.b#;$Py=q#ni:/lib/fn_ro.b#;$Qy=q#ni:/lib/fn_serialize.b#;$Ry=q#ni:/lib/future#;$Sy={$h2,1};$Ty={};$Uy=[];$Vy=bless({$t,$Uy,$v,$q,$w,$jr,$y,$z},$A);$Wy=q#parents#;$Xy=[];$Yy=q#shift->{'parents'}#;$Zy=bless({$t,$Xy,$v,$q,$w,$Yy,$y,$z},$A);$cz={$r,$Vy,$Wy,$Zy};$dz=q#/lib/future_ro.b#;$ez=bless({$l1,$Ty,$R2,$q,$S2,$q,$T2,$cz,$K,$dz},$r2);$fz={};$gz=[];$hz=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$iz=bless({$t,$gz,$v,$q,$w,$hz,$y,$z},$A);$jz={$G4,$iz};$kz=q#/lib/future_init.b#;$lz=bless({$l1,$fz,$R2,$q,$S2,$q,$T2,$jz,$K,$kz},$r2);$mz={};$nz=q#decide#;$oz=[];$pz=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$qz=bless({$t,$oz,$v,$q,$w,$pz,$y,$z},$A);$rz=q#decided#;$sz=[];$tz=q#shift->{outcome}#;$uz=bless({$t,$sz,$v,$q,$w,$tz,$y,$z},$A);$vz=q#listener#;$wz=[];$xz=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$yz=bless({$t,$wz,$v,$q,$w,$xz,$y,$z},$A);$zz=q#v#;$Az=[];$Bz=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$Cz=bless({$t,$Az,$v,$q,$w,$Bz,$y,$z},$A);$Dz={$nz,$qz,$rz,$uz,$vz,$yz,$zz,$Cz};$Ez=q#/lib/future_state.b#;$Fz=bless({$l1,$mz,$R2,$q,$S2,$q,$T2,$Dz,$K,$Ez},$r2);$Gz={};$Hz=q#and#;$Iz=[];$Jz=q#my $self   = $_[0];
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
$child;#;$Kz=bless({$t,$Iz,$v,$q,$w,$Jz,$y,$z},$A);$Lz=q#flatmap#;$Mz=[];$Nz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$Oz=bless({$t,$Mz,$v,$q,$w,$Nz,$y,$z},$A);$Pz=q#map#;$Qz=[];$Rz=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$Sz=bless({$t,$Qz,$v,$q,$w,$Rz,$y,$z},$A);$Tz=q#or#;$Uz=[];$Vz=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Wz=bless({$t,$Uz,$v,$q,$w,$Vz,$y,$z},$A);$Xz={$Hz,$Kz,$Lz,$Oz,$Pz,$Sz,$Tz,$Wz};$Yz=q#/lib/future_algebra.b#;$Zz=bless({$l1,$Gz,$R2,$q,$S2,$q,$T2,$Xz,$K,$Yz},$r2);$cA=[$f3,$ez,$lz,$Fz,$Zz];$dA=bless({$l1,$Sy,$K,$C9,$p1,$cA},$i2);$eA=q#ni:/lib/future.c#;$fA={$i2,1};$gA=q#/lib/future.c#;$hA=[$Xf];$iA=bless({$l1,$fA,$K,$gA,$p1,$hA},$E2);$jA=q#ni:/lib/future_algebra.b#;$kA=q#ni:/lib/future_init.b#;$lA=q#ni:/lib/future_ro.b#;$mA=q#ni:/lib/future_state.b#;$nA=q#ni:/lib/gensym_generator_compact.b#;$oA=q#ni:/lib/global_static_test.b#;$pA={};$qA=[];$rA=q#ni('ni:/lib/test_case')->new(shift)#;$sA=q#($)#;$tA=bless({$t,$qA,$v,$q,$w,$rA,$y,$sA},$A);$uA=q#now#;$vA=[];$wA=q#ni('ni:/lib/test_value')->new(shift)#;$xA=bless({$t,$vA,$v,$q,$w,$wA,$y,$sA},$A);$yA={$T6,$tA,$uA,$xA};$zA=q#/lib/global_static_test.b#;$AA=bless({$l1,$pA,$R2,$q,$S2,$q,$T2,$yA,$K,$zA},$r2);$BA=q#ni:/lib/image#;$CA=q#ni:/lib/image.c#;$DA={$k2,1};$EA=q#/lib/image.c#;$FA=[$Xf];$GA=bless({$l1,$DA,$K,$EA,$p1,$FA},$E2);$HA=q#ni:/lib/image_init.b#;$IA=q#ni:/lib/image_quoting.b#;$JA=q#ni:/lib/instance.b#;$KA=q#ni:/lib/instantiable.b#;$LA=q#ni:/lib/json.b#;$MA={};$NA=q#json_decode#;$OA=[];$PA=q#local $_;
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
wantarray ? @$r : $$r[0];#;$QA=bless({$t,$OA,$v,$q,$w,$PA,$y,$sA},$A);$RA=q#json_encode#;$SA=[];$TA=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$UA=bless({$t,$SA,$v,$q,$w,$TA,$y,$sA},$A);$VA=q#json_encode_pretty#;$WA=[];$XA=q#local $_;
my ($v, $indent) = @_;
$indent ||= 0;
my $spaces = ' ' x $indent;
return "$spaces\\[\\n"
     . join(",\\n", map ni::json_encode_pretty($_, $indent + 2), @$v)
     . "\\n$spaces]" if 'ARRAY' eq ref $v;

return "$spaces\\{\\n"
     . join(",\\n", map "$spaces  " . ni::json_escape($_) . ":\\n"
                                   . ni::json_encode_pretty($$v{$_}, $indent + 2),
                       sort keys %$v)
     . "\\n$spaces\\}" if 'HASH' eq ref $v;

$spaces . ni::json_encode($v);#;$YA=bless({$t,$WA,$v,$q,$w,$XA,$y,$z},$A);$ZA=q#json_escape#;$cB=[];$dB=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$eB=bless({$t,$cB,$v,$q,$w,$dB,$y,$sA},$A);$fB=q#json_unescape#;$gB=[];$hB=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$iB=bless({$t,$gB,$v,$q,$w,$hB,$y,$sA},$A);$jB=q#json_unescape_one#;$kB=[];$lB=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$mB=bless({$t,$kB,$v,$q,$w,$lB,$y,$sA},$A);$nB={$NA,$QA,$RA,$UA,$VA,$YA,$ZA,$eB,$fB,$iB,$jB,$mB};$oB=q#/lib/json.b#;$pB=bless({$l1,$MA,$R2,$q,$S2,$q,$T2,$nB,$K,$oB},$r2);$qB=q#ni#;$rB=q#ni:/lib/name_as_string.b#;$sB=q#ni:/lib/named.b#;$tB=q#ni:/lib/named_in_ni.b#;$uB=q#ni:/lib/namespaced.b#;$vB=q#ni:/lib/ni#;$wB={$l2,1};$xB={};$yB=q#extend#;$zB=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$AB=bless({$w,$zB,$y,$z},$A);$BB=q#is_mutable#;$CB=q#$0 ne '-' && -w $0#;$DB=bless({$w,$CB,$y,$z},$A);$EB=q#modify#;$FB=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$GB=bless({$w,$FB,$y,$z},$A);$HB={$yB,$AB,$BB,$DB,$EB,$GB};$IB=q#/lib/ni_self.b#;$JB=bless({$l1,$xB,$R2,$q,$S2,$q,$T2,$HB,$K,$IB},$r2);$KB={};$LB=q#--internal/+=#;$MB=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$NB=bless({$w,$MB,$y,$z},$A);$OB=q#--internal/eval#;$PB=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$QB=bless({$w,$PB,$y,$z},$A);$RB=q#--internal/image#;$SB=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$TB=bless({$w,$SB,$y,$z},$A);$UB=q#--internal/test#;$VB=q#local $| = 1;
my $self   = shift;
my $failed = 0;
my @docs   = grep /^ni\\.doc:/, keys %{$$self{named}};
my @todos  = map ni($_)->todos, @docs;

print "\\n" . scalar(@todos) . " TODO item(s)\\n" if @todos;
print "$_\\n\\n" for @todos;

my @tests  = map ni($_)->tests, @docs;
my %named  = %{$$ni::self{named}};
for (@tests) {
  $_->run;
  %{$$ni::self{named}} = %named;
  print '.';
}
print "\\n";
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
!!$failed;#;$WB=bless({$w,$VB,$y,$z},$A);$XB=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$YB=bless({$w,$XB,$y,$z},$A);$ZB={$LB,$NB,$OB,$QB,$RB,$TB,$UB,$WB,$rt,$YB};$cC=q#/lib/ni_main.b#;$dC=bless({$l1,$KB,$R2,$q,$S2,$q,$T2,$ZB,$K,$cC},$r2);$eC={};$fC=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$gC=bless({$w,$fC,$y,$z},$A);$hC=q#resolver_for#;$iC=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$jC=bless({$w,$iC,$y,$z},$A);$kC={$Yd,$gC,$hC,$jC};$lC=q#/lib/ni_resolver.b#;$mC=bless({$l1,$eC,$R2,$q,$S2,$q,$T2,$kC,$K,$lC},$r2);$nC={};$oC=q#exists#;$pC=q#exists $_[0]->{named}{$_[1]}#;$qC=bless({$w,$pC,$y,$z},$A);$rC=q#quoted#;$sC=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$tC=bless({$w,$sC,$y,$z},$A);$uC={$oC,$qC,$rC,$tC};$vC=q#/lib/ni_image.b#;$wC=bless({$l1,$nC,$R2,$q,$S2,$q,$T2,$uC,$K,$vC},$r2);$xC=[$f3,$JB,$dC,$mC,$wC];$yC=bless({$l1,$wB,$K,$yc,$p1,$xC},$m2);$zC=q#ni:/lib/ni.c#;$AC={$m2,1};$BC=q#/lib/ni.c#;$CC=[$Xf];$DC=bless({$l1,$AC,$K,$BC,$p1,$CC},$E2);$EC=q#ni:/lib/ni_image.b#;$FC=q#ni:/lib/ni_main.b#;$GC=q#ni:/lib/ni_resolver.b#;$HC=q#ni:/lib/ni_self.b#;$IC=q#ni:/lib/ni_static_util.b#;$JC={};$KC=q#abbrev#;$LC=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$MC=bless({$w,$LC,$y,$z},$A);$NC=q#dor#;$OC=q#defined $_[0] ? $_[0] : $_[1]#;$PC=bless({$w,$OC,$y,$z},$A);$QC=q#indent#;$RC=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$SC=bless({$w,$RC,$y,$z},$A);$TC=q#max#;$UC=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$VC=bless({$w,$UC,$y,$z},$A);$WC=q#maxstr#;$XC=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$YC=bless({$w,$XC,$y,$z},$A);$ZC=q#mean#;$cD=q#sum(@_) / (@_ || 1)#;$dD=bless({$w,$cD,$y,$z},$A);$eD=q#min#;$fD=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$gD=bless({$w,$fD,$y,$z},$A);$hD=q#minstr#;$iD=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$jD=bless({$w,$iD,$y,$z},$A);$kD=q#outdent#;$lD=q#my $x = shift;
return $x if ref $x;
my @lines = split /\\n/, $x;
return $x unless @lines > 1;
my $indent = $lines[1] =~ /^(\\s*)/ && length $1;
for (@lines[2..$\#lines]) {
  my $li = /^(\\s*)/ && length $1;
  $indent = length $1 if length $1 < $indent;
}
my $spaces = ' ' x $indent;
s/^$spaces// for @lines;
join "\\n", @lines;#;$mD=bless({$w,$lD,$y,$z},$A);$nD=q#sgr#;$oD=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$pD=bless({$w,$oD,$y,$z},$A);$qD=q#sr#;$rD=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$sD=bless({$w,$rD,$y,$z},$A);$tD=q#sum#;$uD=q#local $_; my $x = 0; $x += $_ for @_; $x#;$vD=bless({$w,$uD,$y,$z},$A);$wD=q#swap#;$xD=q#@_[0, 1] = @_[1, 0]#;$yD=bless({$w,$xD,$y,$z},$A);$zD={$KC,$MC,$NC,$PC,$QC,$SC,$TC,$VC,$WC,$YC,$ZC,$dD,$eD,$gD,$hD,$jD,$kD,$mD,$nD,$pD,$qD,$sD,$tD,$vD,$wD,$yD};$AD=q#/lib/ni_static_util.b#;$BD=bless({$l1,$JC,$R2,$q,$S2,$q,$T2,$zD,$K,$AD},$r2);$CD=q#ni:/lib/object_metadata#;$DD={$n2,1,$C,1,$C2,1};$ED=q#/lib/object_metadata#;$FD={};$GD=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$HD=bless({$w,$GD,$y,$z},$A);$ID={$k1,$HD};$JD=q#/lib/object_metadata_rw.b#;$KD=bless({$l1,$FD,$R2,$q,$S2,$q,$T2,$ID,$K,$JD},$r2);$LD=[$f3,$KD];$MD=bless({$l1,$DD,$K,$ED,$p1,$LD},$o2);$ND=q#ni:/lib/object_metadata.c#;$OD={$o2,1,$z2,1,$D2,1};$PD=q#/lib/object_metadata.c#;$QD=[$Xf];$RD=bless({$l1,$OD,$K,$PD,$p1,$QD},$E2);$SD=q#ni:/lib/object_metadata_rw.b#;$TD=q#ni:/lib/perlbranch.b#;$UD=q#ni:/lib/quote_circular_addressed.b#;$VD=q#ni:/lib/quote_code_fail.b#;$WD=q#ni:/lib/quote_gensym_identity.b#;$XD=q#ni:/lib/quote_objects.b#;$YD=q#ni:/lib/quote_simple#;$ZD={$p2,1};$cE={};$dE=[];$eE=q#+{}#;$fE=bless({$t,$dE,$v,$q,$w,$eE,$y,$z},$A);$gE={$G4,$fE};$hE=q#/lib/quote_simple_init.b#;$iE=bless({$l1,$cE,$R2,$q,$S2,$q,$T2,$gE,$K,$hE},$r2);$jE={};$kE=[];$lE=bless({$t,$kE,$v,$q,$w,0,$y,$z},$A);$mE=[];$nE=q#shift->quote_value(shift)#;$oE=bless({$t,$mE,$v,$q,$w,$nE,$y,$z},$A);$pE={$zb,$lE,$Tb,$oE};$qE=q#/lib/quote_simple_quote.b#;$rE=bless({$l1,$jE,$R2,$q,$S2,$q,$T2,$pE,$K,$qE},$r2);$sE=[$f3,$iE,$rE,$za,$Xa,$pb];$tE=bless({$l1,$ZD,$K,$Jc,$p1,$sE},$q2);$uE=q#ni:/lib/quote_simple.c#;$vE={$q2,1};$wE=q#/lib/quote_simple.c#;$xE=[$Xf];$yE=bless({$l1,$vE,$K,$wE,$p1,$xE},$E2);$zE=q#ni:/lib/quote_simple_init.b#;$AE=q#ni:/lib/quote_simple_quote.b#;$BE=q#ni:/lib/quote_values.b#;$CE=q#ni:/lib/ref_eq.b#;$DE=q#ni:/lib/resolver.b#;$EE=q#ni:/lib/slice#;$FE={$r2,1};$GE=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$HE=bless({$w,$GE},$A);$IE=q#local $_;
my ($self, $p) = @_;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
push @{"$p\\::ctors"}, $$self{ctor} if $$self{ctor};
push @{"$p\\::dtors"}, $$self{dtor} if $$self{dtor};
if (grep /^\\(/, keys %{$$self{methods}}) {
  *{"$p\\::()"} = sub {};
  *{"$p\\::OVERLOAD"} = {};
}
*{"$p\\::$_"} = \\&{$$self{methods}{$_}} for keys %{$$self{methods}};
$self;#;$JE=bless({$w,$IE},$A);$KE=q#lib/slice::apply#;$LE=q#lib/slice::apply_#;$ME={};$NE=q#apply_#;$OE={$xd,$HE,$NE,$JE};$PE=q#/lib/slice.b#;$QE=bless({$l1,$ME,$T2,$OE,$K,$PE},$r2);$RE={};$SE=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$TE=bless({$w,$SE,$y,$z},$A);$UE={$G4,$TE};$VE=q#/lib/slice_init.b#;$WE=bless({$l1,$RE,$T2,$UE,$K,$VE},$r2);$XE={};$YE=[];$ZE=q#local $_;
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
$g;#;$cF=bless({$t,$YE,$v,$q,$w,$ZE,$y,$z},$A);$dF={$ny,$cF};$eF=q#/lib/slice_serialize.b#;$fF=bless({$l1,$XE,$R2,$q,$S2,$q,$T2,$dF,$K,$eF},$r2);$gF=[$xe,$Id,$QE,$WE,$fF];$hF=bless({$l1,$FE,$K,$gd,$p1,$gF},$s2);$iF=q#ni:/lib/slice.b#;$jF=q#ni:/lib/slice.c#;$kF={$s2,1};$lF=q#/lib/slice.c#;$mF=[$dg];$nF=bless({$l1,$kF,$K,$lF,$p1,$mF},$E2);$oF=q#ni:/lib/slice_init.b#;$pF=q#ni:/lib/slice_serialize.b#;$qF=q#ni:/lib/static_fn.b#;$rF={};$sF=q#fc#;$tF=[];$uF=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$vF=bless({$t,$tF,$v,$q,$w,$uF,$y,$z},$A);$wF=q#fk#;$xF=[];$yF=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$zF=bless({$t,$xF,$v,$q,$w,$yF,$y,$sA},$A);$AF=[];$BF=q#ni('ni:/lib/fn')->new(@_)#;$CF=bless({$t,$AF,$v,$q,$w,$BF,$y,$sA},$A);$DF=q#fp#;$EF=[];$FF=q#($$)#;$GF=bless({$t,$EF,$v,$q,$w,$BF,$y,$FF},$A);$HF={$sF,$vF,$wF,$zF,$Ux,$CF,$DF,$GF};$IF=q#/lib/static_fn.b#;$JF=bless({$l1,$rF,$R2,$q,$S2,$q,$T2,$HF,$K,$IF},$r2);$KF=q#ni:/lib/subclass.b#;$LF=q#ni:/lib/tag#;$MF={$t2,1};$NF=q#/lib/tag#;$OF={};$PF=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$QF=bless({$w,$PF,$y,$z},$A);$RF={$xd,$QF};$SF=q#/lib/tag.b#;$TF=bless({$l1,$OF,$R2,$q,$S2,$q,$T2,$RF,$K,$SF},$r2);$UF={};$VF=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$WF=bless({$w,$VF,$y,$z},$A);$XF={$G4,$WF};$YF=q#/lib/tag_init.b#;$ZF=bless({$l1,$UF,$R2,$q,$S2,$q,$T2,$XF,$K,$YF},$r2);$cG=[$xe,$Id,$TF,$ZF];$dG=bless({$l1,$MF,$K,$NF,$p1,$cG},$u2);$eG=q#ni:/lib/tag.b#;$fG=q#ni:/lib/tag.c#;$gG={$u2,1};$hG=q#/lib/tag.c#;$iG=[$dg];$jG=bless({$l1,$gG,$K,$hG,$p1,$iG},$E2);$kG=q#ni:/lib/tag_init.b#;$lG=q#ni:/lib/test_assert_eq#;$mG={$v2,1};$nG=q#/lib/test_assert_eq#;$oG={$v2,1,$x2,1};$pG=q#/lib/test_assertion#;$qG={};$rG=q#commit#;$sG=[];$tG=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$uG=bless({$t,$sG,$v,$q,$w,$tG,$y,$z},$A);$vG={$rG,$uG};$wG=q#/lib/test_assertion_commit.b#;$xG=bless({$l1,$qG,$R2,$q,$S2,$q,$T2,$vG,$K,$wG},$r2);$yG=[$f3,$xG];$zG=bless({$l1,$oG,$K,$pG,$p1,$yG},$y2);$AG={};$BG=[];$CG=q#my ($class, $diff) = @_;
+{diff => $diff};#;$DG=bless({$t,$BG,$v,$q,$w,$CG,$y,$z},$A);$EG={$G4,$DG};$FG=q#/lib/test_assert_eq_init.b#;$GG=bless({$l1,$AG,$R2,$q,$S2,$q,$T2,$EG,$K,$FG},$r2);$HG={};$IG=[];$JG=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$KG=bless({$t,$IG,$v,$q,$w,$JG,$y,$z},$A);$LG=q#failed#;$MG=[];$NG=q#defined shift->{diff}#;$OG=bless({$t,$MG,$v,$q,$w,$NG,$y,$z},$A);$PG=q#result#;$QG=[];$RG=bless({$t,$QG,$v,$q,$w,$tt,$y,$z},$A);$SG={$Se,$KG,$LG,$OG,$PG,$RG};$TG=q#/lib/test_assert_eq_result.b#;$UG=bless({$l1,$HG,$R2,$q,$S2,$q,$T2,$SG,$K,$TG},$r2);$VG=[$zG,$GG,$UG];$WG=bless({$l1,$mG,$K,$nG,$p1,$VG},$w2);$XG=q#ni:/lib/test_assert_eq.c#;$YG={$w2,1};$ZG=q#/lib/test_assert_eq.c#;$cH={$w2,1,$y2,1};$dH=q#/lib/test_assertion.c#;$eH=[$Xf];$fH=bless({$l1,$cH,$K,$dH,$p1,$eH},$E2);$gH=[$fH];$hH=bless({$l1,$YG,$K,$ZG,$p1,$gH},$E2);$iH=q#ni:/lib/test_assert_eq_init.b#;$jH=q#ni:/lib/test_assert_eq_result.b#;$kH=q#ni:/lib/test_assertion#;$lH=q#ni:/lib/test_assertion.c#;$mH=q#ni:/lib/test_assertion_commit.b#;$nH=q#ni:/lib/test_case#;$oH={$C,1};$pH=q#/lib/test_case#;$qH=q#running_test#;$rH={};$sH=[];$tH=q#shift->{'assertions'}#;$uH=bless({$t,$sH,$v,$q,$w,$tH,$y,$z},$A);$vH=[];$wH=q#shift->{'test'}#;$xH=bless({$t,$vH,$v,$q,$w,$wH,$y,$z},$A);$yH={$n,$uH,$s,$xH};$zH=q#/lib/test_case_ro.b#;$AH=bless({$l1,$rH,$R2,$q,$S2,$q,$T2,$yH,$K,$zH},$r2);$BH={};$CH=[];$DH=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$EH=bless({$t,$CH,$v,$q,$w,$DH,$y,$z},$A);$FH={$p,$EH};$GH=q#/lib/test_case_rw.b#;$HH=bless({$l1,$BH,$R2,$q,$S2,$q,$T2,$FH,$K,$GH},$r2);$IH={};$JH=[];$KH=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$LH=bless({$t,$JH,$v,$q,$w,$KH,$y,$z},$A);$MH={$G4,$LH};$NH=q#/lib/test_case_init.b#;$OH=bless({$l1,$IH,$R2,$q,$S2,$q,$T2,$MH,$K,$NH},$r2);$PH={};$QH=[];$RH=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$SH=bless({$t,$QH,$v,$q,$w,$RH,$y,$z},$A);$TH=[];$UH=q#!shift->{outcome}->[0]#;$VH=bless({$t,$TH,$v,$q,$w,$UH,$y,$z},$A);$WH={$Se,$SH,$LG,$VH};$XH=q#/lib/test_case_metrics.b#;$YH=bless({$l1,$PH,$R2,$q,$S2,$q,$T2,$WH,$K,$XH},$r2);$ZH={};$cI=q#done#;$dI=[];$eI=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$fI=bless({$t,$dI,$v,$q,$w,$eI,$y,$z},$A);$gI=[];$hI=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$iI=bless({$t,$gI,$v,$q,$w,$hI,$y,$z},$A);$jI={$cI,$fI,$rt,$iI};$kI=q#/lib/test_case_run.b#;$lI=bless({$l1,$ZH,$R2,$q,$S2,$q,$T2,$jI,$K,$kI},$r2);$mI=[$MD,$AH,$HH,$OH,$YH,$lI];$nI=bless({$l1,$oH,$K,$pH,$qH,$q,$p1,$mI},$z2);$oI=[];$pI=q#shift->{running_test} = undef#;$qI=bless({$t,$oI,$v,$q,$w,$pI,$y,$z},$A);$rI=q#ni:/lib/test_case.c#;$sI={$z2,1};$tI=q#/lib/test_case.c#;$uI={};$vI=[];$wI=q#shift->{'running_test'}#;$xI=bless({$t,$vI,$v,$q,$w,$wI,$y,$z},$A);$yI={$qH,$xI};$zI=q#/lib/test_case.c_test_ro.b#;$AI=bless({$l1,$uI,$R2,$q,$S2,$q,$T2,$yI,$K,$zI},$r2);$BI={};$CI=q#with_test#;$DI=[];$EI=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$FI=bless({$t,$DI,$v,$q,$w,$EI,$y,$z},$A);$GI={$CI,$FI};$HI=q#/lib/test_case.c_test.b#;$II=bless({$l1,$BI,$R2,$qI,$S2,$q,$T2,$GI,$K,$HI},$r2);$JI=[$RD,$AI,$II];$KI=bless({$l1,$sI,$K,$tI,$p1,$JI},$E2);$LI=q#ni:/lib/test_case.c_test.b#;$MI=q#ni:/lib/test_case.c_test_ro.b#;$NI=q#ni:/lib/test_case_init.b#;$OI=q#ni:/lib/test_case_metrics.b#;$PI=q#ni:/lib/test_case_ro.b#;$QI=q#ni:/lib/test_case_run.b#;$RI=q#ni:/lib/test_case_rw.b#;$SI=q#ni:/lib/test_value#;$TI={$A2,1};$UI=q#/lib/test_value#;$VI={};$WI=[];$XI=q#\\$_[1]#;$YI=bless({$t,$WI,$v,$q,$w,$XI,$y,$z},$A);$ZI={$G4,$YI};$cJ=q#/lib/test_value_init.b#;$dJ=bless({$l1,$VI,$R2,$q,$S2,$q,$T2,$ZI,$K,$cJ},$r2);$eJ={};$fJ=q#(==#;$gJ=[];$hJ=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$iJ=bless({$t,$gJ,$v,$q,$w,$hJ,$y,$z},$A);$jJ=q#detailed_scalar_diff#;$kJ=[];$lJ=q#local $_;
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
[@diff];#;$mJ=bless({$t,$kJ,$v,$q,$w,$lJ,$y,$z},$A);$nJ=q#diff#;$oJ=[];$pJ=q#my ($self, $rhs) = @_;
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
return undef;#;$qJ=bless({$t,$oJ,$v,$q,$w,$pJ,$y,$z},$A);$rJ={$fJ,$iJ,$jJ,$mJ,$nJ,$qJ};$sJ=q#/lib/test_value_eq.b#;$tJ=bless({$l1,$eJ,$R2,$q,$S2,$q,$T2,$rJ,$K,$sJ},$r2);$uJ={};$vJ=[];$wJ=q#ni::json_encode ${$_[0]}#;$xJ=bless({$t,$vJ,$v,$q,$w,$wJ,$y,$z},$A);$yJ={$Se,$xJ};$zJ=q#/lib/test_value_str.b#;$AJ=bless({$l1,$uJ,$R2,$q,$S2,$q,$T2,$yJ,$K,$zJ},$r2);$BJ=[$f3,$dJ,$tJ,$AJ];$CJ=bless({$l1,$TI,$K,$UI,$p1,$BJ},$B2);$DJ=q#ni:/lib/test_value.c#;$EJ={$B2,1};$FJ=q#/lib/test_value.c#;$GJ=[$Xf];$HJ=bless({$l1,$EJ,$K,$FJ,$p1,$GJ},$E2);$IJ=q#ni:/lib/test_value_eq.b#;$JJ=q#ni:/lib/test_value_init.b#;$KJ=q#ni:/lib/test_value_str.b#;$LJ=q#ni:/lib/todo#;$MJ={$C2,1};$NJ=q#/lib/todo#;$OJ={};$PJ=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$QJ=bless({$w,$PJ,$y,$z},$A);$RJ={$G4,$QJ};$SJ=q#/lib/todo_init.b#;$TJ=bless({$l1,$OJ,$R2,$q,$S2,$q,$T2,$RJ,$K,$SJ},$r2);$UJ={};$VJ=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$WJ=bless({$w,$VJ,$y,$z},$A);$XJ={$Se,$WJ};$YJ=q#/lib/todo_str.b#;$ZJ=bless({$l1,$UJ,$R2,$q,$S2,$q,$T2,$XJ,$K,$YJ},$r2);$cK=[$MD,$TJ,$ZJ];$dK=bless({$l1,$MJ,$K,$NJ,$p1,$cK},$D2);$eK=q#ni:/lib/todo.c#;$fK={$D2,1};$gK=q#/lib/todo.c#;$hK=[$RD];$iK=bless({$l1,$fK,$K,$gK,$p1,$hK},$E2);$jK=q#ni:/lib/todo_ctor.b#;$kK={};$lK=q#ni('ni:/lib/todo')->new(@_)#;$mK=bless({$w,$lK,$y,$z},$A);$nK={$j1,$mK};$oK=q#/lib/todo_ctor.b#;$pK=bless({$l1,$kK,$R2,$q,$S2,$q,$T2,$nK,$K,$oK},$r2);$qK=q#ni:/lib/todo_init.b#;$rK=q#ni:/lib/todo_str.b#;$sK=q#ni:/metaclass#;$tK={$E2,1};$uK=q#/metaclass#;$vK=[$he,$Ff,$ne,$yf];$wK=bless({$l1,$tK,$K,$uK,$p1,$vK},$F2);$xK=q#ni:/metaclass.c#;$yK={$F2,1};$zK=q#/metaclass.c#;$AK=[$Of];$BK=bless({$l1,$yK,$K,$zK,$p1,$AK},$E2);$CK=q#ni:/module#;$DK=q#ni:/module.c#;$EK=q#ni:/object#;$FK=q#ni:/object.c#;$GK=q#ni:/semantic/dimension#;$HK={$K2,1};$IK=q#/semantic/dimension#;$JK=[$Of];$KK=bless({$l1,$HK,$K,$IK,$p1,$JK},$L2);$LK=q#ni:/semantic/dimension.c#;$MK={$L2,1};$NK=q#/semantic/dimension.c#;$OK=[$hg];$PK=bless({$l1,$MK,$K,$NK,$p1,$OK},$E2);$QK=q#ni:/semantic/task#;$RK=q#ni:/semantic/task.c#;$SK=q#ni:/semantic/task_outcome.b#;$TK=q#ni:/semantic/task_ro.b#;$UK=q#ni:main#;$VK={$Xo,1};$WK=[$pK,$JF,$AA,$Wo];$XK=bless({$l1,$VK,$K,$Xo,$p1,$WK},$G2);$YK=q#ni:ni#;$ZK={$qB,1};$cL={$qB,1};$dL=q#json_escapes#;$eL=q##;$fL=q#b#;$gL=q#	#;$hL=q#t#;$iL=q#
#;$jL=q#n#;$kL=q##;$lL=q#"#;$mL=q#/#;$nL=q#\\#;$oL={$eL,$fL,$gL,$hL,$iL,$jL,$kL,$on,$lL,$lL,$mL,$mL,$nL,$nL};$pL=q#json_unescapes#;$qL={$lL,$lL,$mL,$mL,$nL,$nL,$fL,$eL,$jL,$iL,$on,$kL,$hL,$gL};$rL={$dL,$oL,$pL,$qL};$sL=q#/lib/json_data.b#;$tL=bless({$l1,$cL,$gq,$rL,$K,$sL},$d2);$uL=[$BD,$tL,$pB];$vL=bless({$l1,$ZK,$K,$qB,$p1,$uL},$G2);$wL={$d,$N,$Q,$W,$X,$e1,$f1,$h5,$i5,$n5,$o5,$A5,$B5,$N5,$O5,$c6,$d6,$p6,$q6,$L6,$M6,$R6,$S6,$J8,$K8,$Q8,$R8,$m9,$n9,$D9,$E9,$rc,$sc,$zc,$Ac,$Kc,$Lc,$hd,$id,$nd,$od,$Of,$Pf,$hg,$ig,$Ag,$Bg,$Fg,$Gg,$qg,$Hg,$yg,$Ig,$Y4,$Jg,$Zg,$ch,$W4,$dh,$Fh,$Gh,$Kh,$Lh,$lh,$Mh,$Dh,$Nh,$gi,$hi,$li,$mi,$Vh,$ni,$ei,$oi,$Uj,$Vj,$Zj,$ck,$Cj,$dk,$Sj,$ek,$Ei,$fk,$uj,$gk,$Yi,$hk,$xi,$ik,$Al,$El,$em,$fm,$cm,$gm,$Wk,$hm,$jl,$im,$yk,$jm,$xl,$km,$rk,$lm,$Gk,$mm,$Gn,$Hn,$Ln,$Mn,$Cm,$Nn,$Zm,$On,$Jm,$Pn,$En,$Qn,$um,$Rn,$jn,$Sn,$no,$oo,$so,$to,$lo,$uo,$co,$vo,$Wo,$Yo,$yp,$zp,$Dp,$Ep,$jp,$Fp,$wp,$Gp,$E4,$Hp,$Xg,$Ip,$Vg,$Jp,$I3,$Kp,$Q3,$Lp,$e4,$Mp,$o3,$Np,$C4,$Op,$q4,$Pp,$q8,$Qp,$Up,$Vp,$o8,$Wp,$u7,$Xp,$S7,$Yp,$k7,$Zp,$G7,$cq,$Rq,$Sq,$Wq,$Xq,$Bq,$Yq,$Pq,$Zq,$uq,$cr,$ms,$qs,$Cs,$Ds,$As,$Es,$Gt,$Kt,$gu,$hu,$eu,$iu,$ft,$ju,$pt,$ku,$Ws,$lu,$Bt,$mu,$Or,$nu,$ks,$ou,$Gu,$Hu,$Lu,$Mu,$xu,$Nu,$Eu,$Ou,$Qe,$Pu,$xe,$Qu,$dg,$Ru,$dv,$ev,$Cd,$fv,$jv,$kv,$Zu,$lv,$ne,$mv,$Cv,$Dv,$Hv,$Iv,$Av,$Jv,$uv,$Kv,$wf,$Lv,$Ge,$Mv,$nf,$Nv,$uf,$Ov,$Sw,$Tw,$gx,$hx,$ex,$ix,$Aw,$jx,$mw,$kx,$Gw,$lx,$uw,$mx,$Vv,$nx,$dw,$ox,$Qw,$px,$ve,$qx,$vy,$zy,$Ly,$My,$Jy,$Ny,$Ix,$Oy,$ly,$Py,$cy,$Qy,$ty,$Ry,$dA,$eA,$iA,$jA,$Zz,$kA,$lz,$lA,$ez,$mA,$Fz,$nA,$jc,$oA,$AA,$BA,$lc,$CA,$GA,$HA,$R9,$IA,$ra,$JA,$d3,$KA,$Ff,$LA,$pB,$rB,$Xe,$sB,$Id,$tB,$Pd,$uB,$Wd,$vB,$yC,$zC,$DC,$EC,$wC,$FC,$dC,$GC,$mC,$HC,$JB,$IC,$BD,$CD,$MD,$ND,$RD,$SD,$KD,$TD,$he,$UD,$Fb,$VD,$za,$WD,$Zb,$XD,$pb,$YD,$tE,$uE,$yE,$zE,$iE,$AE,$rE,$BE,$Xa,$CE,$gf,$DE,$fe,$EE,$hF,$iF,$QE,$jF,$nF,$oF,$WE,$pF,$fF,$qF,$JF,$KF,$Mf,$LF,$dG,$eG,$TF,$fG,$jG,$kG,$ZF,$lG,$WG,$XG,$hH,$iH,$GG,$jH,$UG,$kH,$zG,$lH,$fH,$mH,$xG,$nH,$nI,$rI,$KI,$LI,$II,$MI,$AI,$NI,$OH,$OI,$YH,$PI,$AH,$QI,$lI,$RI,$HH,$SI,$CJ,$DJ,$HJ,$IJ,$tJ,$JJ,$dJ,$KJ,$AJ,$LJ,$dK,$eK,$iK,$jK,$pK,$qK,$TJ,$rK,$ZJ,$sK,$wK,$xK,$BK,$CK,$yf,$DK,$fg,$EK,$f3,$FK,$Xf,$GK,$KK,$LK,$PK,$QK,$Br,$RK,$ws,$SK,$zr,$TK,$nr,$UK,$XK,$YK,$vL};$xL=q#resolvers#;$yL=[];$zL=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$AL=bless({$t,$yL,$v,$q,$w,$zL,$y,$z},$A);$BL=q#file#;$CL=[];$DL=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$EL=bless({$t,$CL,$v,$q,$w,$DL,$y,$z},$A);$FL=q#null#;$GL=[];$HL=q#ni('ni:/io/null')->new#;$IL=bless({$t,$GL,$v,$q,$w,$HL,$y,$z},$A);$JL=q#sh#;$KL=[];$LL=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$ML=bless({$t,$KL,$v,$q,$w,$LL,$y,$z},$A);$NL=q#str#;$OL=[];$PL=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$QL=bless({$t,$OL,$v,$q,$w,$PL,$y,$z},$A);$RL={$U7,$AL,$BL,$EL,$FL,$IL,$JL,$ML,$NL,$QL};$SL=bless({$c,$wL,$xL,$RL},$l2);*$LE=\&$JE;*$KE=\&$HE;*$Bx=\&$yx;*$Ax=\&$wx;*$zx=\&$ux;$d3->apply_($C1);$d3->apply_($D1);$d3->apply_($E1);$d3->apply_($F1);$d3->apply_($m1);$d3->apply_($G1);$d3->apply_($q1);$d3->apply_($H1);$d3->apply_($r1);$d3->apply_($I1);$d3->apply_($s1);$d3->apply_($J1);$d3->apply_($t1);$d3->apply_($K1);$d3->apply_($u1);$d3->apply_($L1);$d3->apply_($v1);$d3->apply_($M1);$d3->apply_($w1);$d3->apply_($N1);$d3->apply_($x1);$d3->apply_($O1);$d3->apply_($y1);$d3->apply_($P1);$d3->apply_($z1);$d3->apply_($Q1);$d3->apply_($R1);$d3->apply_($S1);$d3->apply_($T1);$d3->apply_($U1);$d3->apply_($V1);$d3->apply_($W1);$d3->apply_($X1);$d3->apply_($Y1);$d3->apply_($Z1);$d3->apply_($c2);$d3->apply_($d2);$d3->apply_($e2);$d3->apply_($M);$d3->apply_($f2);$d3->apply_($A);$d3->apply_($g2);$d3->apply_($h2);$d3->apply_($i2);$d3->apply_($j2);$d3->apply_($k2);$d3->apply_($l2);$d3->apply_($m2);$d3->apply_($n2);$d3->apply_($o2);$d3->apply_($p2);$d3->apply_($q2);$d3->apply_($r2);$d3->apply_($s2);$d3->apply_($t2);$d3->apply_($u2);$d3->apply_($v2);$d3->apply_($w2);$d3->apply_($x2);$d3->apply_($y2);$d3->apply_($C);$d3->apply_($z2);$d3->apply_($A2);$d3->apply_($B2);$d3->apply_($C2);$d3->apply_($D2);$d3->apply_($E2);$d3->apply_($F2);$d3->apply_($G2);$d3->apply_($H2);$d3->apply_($I2);$d3->apply_($J2);$d3->apply_($K2);$d3->apply_($L2);$d3->apply_($M2);$d3->apply_($N2);$o3->apply_($m1);$o3->apply_($q1);$o3->apply_($r1);$o3->apply_($s1);$o3->apply_($t1);$o3->apply_($u1);$o3->apply_($v1);$o3->apply_($w1);$o3->apply_($x1);$o3->apply_($y1);$o3->apply_($z1);$I3->apply_($m1);$I3->apply_($q1);$I3->apply_($r1);$I3->apply_($s1);$I3->apply_($t1);$I3->apply_($u1);$I3->apply_($v1);$I3->apply_($w1);$I3->apply_($x1);$I3->apply_($y1);$I3->apply_($z1);$Q3->apply_($m1);$Q3->apply_($q1);$Q3->apply_($r1);$Q3->apply_($s1);$Q3->apply_($t1);$Q3->apply_($u1);$Q3->apply_($v1);$Q3->apply_($w1);$Q3->apply_($x1);$Q3->apply_($y1);$Q3->apply_($z1);$e4->apply_($m1);$e4->apply_($q1);$e4->apply_($r1);$e4->apply_($s1);$e4->apply_($t1);$e4->apply_($u1);$e4->apply_($v1);$e4->apply_($w1);$e4->apply_($x1);$e4->apply_($y1);$e4->apply_($z1);$q4->apply_($m1);$q4->apply_($q1);$q4->apply_($r1);$q4->apply_($s1);$q4->apply_($t1);$q4->apply_($u1);$q4->apply_($v1);$q4->apply_($w1);$q4->apply_($x1);$q4->apply_($y1);$q4->apply_($z1);$C4->apply_($m1);$C4->apply_($q1);$C4->apply_($r1);$C4->apply_($s1);$C4->apply_($t1);$C4->apply_($u1);$C4->apply_($v1);$C4->apply_($w1);$C4->apply_($x1);$C4->apply_($y1);$C4->apply_($z1);$W4->apply_($m1);$k7->apply_($y1);$u7->apply_($y1);$G7->apply_($y1);$S7->apply_($y1);$o8->apply_($y1);$R9->apply_($j2);$ra->apply_($j2);$za->apply_($j2);$za->apply_($p2);$Xa->apply_($j2);$Xa->apply_($p2);$pb->apply_($j2);$pb->apply_($p2);$Fb->apply_($j2);$Zb->apply_($j2);$jc->apply_($j2);$Cd->apply_($C1);$Cd->apply_($D1);$Cd->apply_($F1);$Cd->apply_($G1);$Cd->apply_($H1);$Cd->apply_($I1);$Cd->apply_($J1);$Cd->apply_($K1);$Cd->apply_($L1);$Cd->apply_($M1);$Cd->apply_($N1);$Cd->apply_($O1);$Cd->apply_($P1);$Cd->apply_($Q1);$Cd->apply_($S1);$Cd->apply_($U1);$Cd->apply_($W1);$Cd->apply_($Y1);$Cd->apply_($Z1);$Cd->apply_($c2);$Cd->apply_($e2);$Cd->apply_($f2);$Cd->apply_($g2);$Cd->apply_($i2);$Cd->apply_($k2);$Cd->apply_($m2);$Cd->apply_($o2);$Cd->apply_($q2);$Cd->apply_($s2);$Cd->apply_($u2);$Cd->apply_($w2);$Cd->apply_($y2);$Cd->apply_($z2);$Cd->apply_($B2);$Cd->apply_($D2);$Cd->apply_($E2);$Cd->apply_($F2);$Cd->apply_($G2);$Cd->apply_($H2);$Cd->apply_($J2);$Cd->apply_($K2);$Cd->apply_($L2);$Cd->apply_($N2);$Id->apply_($C1);$Id->apply_($D1);$Id->apply_($F1);$Id->apply_($G1);$Id->apply_($H1);$Id->apply_($I1);$Id->apply_($J1);$Id->apply_($K1);$Id->apply_($L1);$Id->apply_($M1);$Id->apply_($N1);$Id->apply_($O1);$Id->apply_($P1);$Id->apply_($Q1);$Id->apply_($S1);$Id->apply_($U1);$Id->apply_($W1);$Id->apply_($Y1);$Id->apply_($Z1);$Id->apply_($c2);$Id->apply_($e2);$Id->apply_($M);$Id->apply_($f2);$Id->apply_($g2);$Id->apply_($i2);$Id->apply_($k2);$Id->apply_($m2);$Id->apply_($o2);$Id->apply_($q2);$Id->apply_($r2);$Id->apply_($s2);$Id->apply_($t2);$Id->apply_($u2);$Id->apply_($w2);$Id->apply_($y2);$Id->apply_($z2);$Id->apply_($B2);$Id->apply_($D2);$Id->apply_($E2);$Id->apply_($F2);$Id->apply_($G2);$Id->apply_($H2);$Id->apply_($J2);$Id->apply_($K2);$Id->apply_($L2);$Id->apply_($N2);$Pd->apply_($C1);$Pd->apply_($D1);$Pd->apply_($F1);$Pd->apply_($G1);$Pd->apply_($H1);$Pd->apply_($I1);$Pd->apply_($J1);$Pd->apply_($K1);$Pd->apply_($L1);$Pd->apply_($M1);$Pd->apply_($N1);$Pd->apply_($O1);$Pd->apply_($P1);$Pd->apply_($Q1);$Pd->apply_($S1);$Pd->apply_($U1);$Pd->apply_($W1);$Pd->apply_($Y1);$Pd->apply_($Z1);$Pd->apply_($c2);$Pd->apply_($e2);$Pd->apply_($f2);$Pd->apply_($g2);$Pd->apply_($i2);$Pd->apply_($k2);$Pd->apply_($m2);$Pd->apply_($o2);$Pd->apply_($q2);$Pd->apply_($r2);$Pd->apply_($s2);$Pd->apply_($t2);$Pd->apply_($u2);$Pd->apply_($w2);$Pd->apply_($y2);$Pd->apply_($z2);$Pd->apply_($B2);$Pd->apply_($D2);$Pd->apply_($E2);$Pd->apply_($F2);$Pd->apply_($G2);$Pd->apply_($H2);$Pd->apply_($J2);$Pd->apply_($K2);$Pd->apply_($L2);$Pd->apply_($N2);$Wd->apply_($C1);$Wd->apply_($D1);$Wd->apply_($F1);$Wd->apply_($G1);$Wd->apply_($H1);$Wd->apply_($I1);$Wd->apply_($J1);$Wd->apply_($K1);$Wd->apply_($L1);$Wd->apply_($M1);$Wd->apply_($N1);$Wd->apply_($O1);$Wd->apply_($P1);$Wd->apply_($Q1);$Wd->apply_($S1);$Wd->apply_($U1);$Wd->apply_($W1);$Wd->apply_($Y1);$Wd->apply_($Z1);$Wd->apply_($c2);$Wd->apply_($e2);$Wd->apply_($f2);$Wd->apply_($g2);$Wd->apply_($i2);$Wd->apply_($k2);$Wd->apply_($m2);$Wd->apply_($o2);$Wd->apply_($q2);$Wd->apply_($r2);$Wd->apply_($s2);$Wd->apply_($t2);$Wd->apply_($u2);$Wd->apply_($w2);$Wd->apply_($y2);$Wd->apply_($z2);$Wd->apply_($B2);$Wd->apply_($D2);$Wd->apply_($E2);$Wd->apply_($F2);$Wd->apply_($G2);$Wd->apply_($H2);$Wd->apply_($J2);$Wd->apply_($K2);$Wd->apply_($L2);$Wd->apply_($N2);$fe->apply_($C1);$fe->apply_($D1);$fe->apply_($F1);$fe->apply_($G1);$fe->apply_($H1);$fe->apply_($I1);$fe->apply_($J1);$fe->apply_($K1);$fe->apply_($L1);$fe->apply_($M1);$fe->apply_($N1);$fe->apply_($O1);$fe->apply_($P1);$fe->apply_($Q1);$fe->apply_($S1);$fe->apply_($U1);$fe->apply_($W1);$fe->apply_($Y1);$fe->apply_($Z1);$fe->apply_($c2);$fe->apply_($e2);$fe->apply_($f2);$fe->apply_($g2);$fe->apply_($i2);$fe->apply_($k2);$fe->apply_($m2);$fe->apply_($o2);$fe->apply_($q2);$fe->apply_($s2);$fe->apply_($t2);$fe->apply_($u2);$fe->apply_($w2);$fe->apply_($y2);$fe->apply_($z2);$fe->apply_($B2);$fe->apply_($D2);$fe->apply_($E2);$fe->apply_($F2);$fe->apply_($G2);$fe->apply_($H2);$fe->apply_($J2);$fe->apply_($K2);$fe->apply_($L2);$fe->apply_($N2);$ne->apply_($C1);$ne->apply_($D1);$ne->apply_($F1);$ne->apply_($G1);$ne->apply_($H1);$ne->apply_($I1);$ne->apply_($J1);$ne->apply_($K1);$ne->apply_($L1);$ne->apply_($M1);$ne->apply_($N1);$ne->apply_($O1);$ne->apply_($P1);$ne->apply_($Q1);$ne->apply_($S1);$ne->apply_($U1);$ne->apply_($W1);$ne->apply_($Y1);$ne->apply_($c2);$ne->apply_($e2);$ne->apply_($f2);$ne->apply_($g2);$ne->apply_($i2);$ne->apply_($k2);$ne->apply_($m2);$ne->apply_($o2);$ne->apply_($q2);$ne->apply_($s2);$ne->apply_($u2);$ne->apply_($w2);$ne->apply_($y2);$ne->apply_($z2);$ne->apply_($B2);$ne->apply_($D2);$ne->apply_($E2);$ne->apply_($F2);$ne->apply_($G2);$ne->apply_($H2);$ne->apply_($J2);$ne->apply_($K2);$ne->apply_($L2);$ne->apply_($N2);$ve->apply_($C1);$ve->apply_($D1);$ve->apply_($F1);$ve->apply_($G1);$ve->apply_($H1);$ve->apply_($I1);$ve->apply_($J1);$ve->apply_($K1);$ve->apply_($L1);$ve->apply_($M1);$ve->apply_($N1);$ve->apply_($O1);$ve->apply_($P1);$ve->apply_($Q1);$ve->apply_($S1);$ve->apply_($U1);$ve->apply_($W1);$ve->apply_($X1);$ve->apply_($Y1);$ve->apply_($Z1);$ve->apply_($c2);$ve->apply_($d2);$ve->apply_($e2);$ve->apply_($f2);$ve->apply_($g2);$ve->apply_($i2);$ve->apply_($k2);$ve->apply_($m2);$ve->apply_($o2);$ve->apply_($q2);$ve->apply_($r2);$ve->apply_($s2);$ve->apply_($t2);$ve->apply_($u2);$ve->apply_($w2);$ve->apply_($y2);$ve->apply_($z2);$ve->apply_($B2);$ve->apply_($D2);$ve->apply_($E2);$ve->apply_($F2);$ve->apply_($G2);$ve->apply_($H2);$ve->apply_($J2);$ve->apply_($K2);$ve->apply_($L2);$ve->apply_($N2);$Ge->apply_($C1);$Ge->apply_($D1);$Ge->apply_($F1);$Ge->apply_($G1);$Ge->apply_($H1);$Ge->apply_($I1);$Ge->apply_($J1);$Ge->apply_($K1);$Ge->apply_($L1);$Ge->apply_($M1);$Ge->apply_($N1);$Ge->apply_($O1);$Ge->apply_($P1);$Ge->apply_($Q1);$Ge->apply_($S1);$Ge->apply_($U1);$Ge->apply_($W1);$Ge->apply_($Y1);$Ge->apply_($Z1);$Ge->apply_($c2);$Ge->apply_($e2);$Ge->apply_($f2);$Ge->apply_($g2);$Ge->apply_($i2);$Ge->apply_($k2);$Ge->apply_($m2);$Ge->apply_($o2);$Ge->apply_($q2);$Ge->apply_($s2);$Ge->apply_($u2);$Ge->apply_($w2);$Ge->apply_($y2);$Ge->apply_($z2);$Ge->apply_($B2);$Ge->apply_($D2);$Ge->apply_($E2);$Ge->apply_($F2);$Ge->apply_($G2);$Ge->apply_($H2);$Ge->apply_($J2);$Ge->apply_($K2);$Ge->apply_($L2);$Ge->apply_($N2);$Qe->apply_($C1);$Qe->apply_($D1);$Qe->apply_($F1);$Qe->apply_($G1);$Qe->apply_($H1);$Qe->apply_($I1);$Qe->apply_($J1);$Qe->apply_($K1);$Qe->apply_($L1);$Qe->apply_($M1);$Qe->apply_($N1);$Qe->apply_($O1);$Qe->apply_($P1);$Qe->apply_($Q1);$Qe->apply_($S1);$Qe->apply_($U1);$Qe->apply_($W1);$Qe->apply_($Y1);$Qe->apply_($Z1);$Qe->apply_($c2);$Qe->apply_($e2);$Qe->apply_($f2);$Qe->apply_($g2);$Qe->apply_($i2);$Qe->apply_($k2);$Qe->apply_($m2);$Qe->apply_($o2);$Qe->apply_($q2);$Qe->apply_($s2);$Qe->apply_($u2);$Qe->apply_($w2);$Qe->apply_($y2);$Qe->apply_($z2);$Qe->apply_($B2);$Qe->apply_($D2);$Qe->apply_($E2);$Qe->apply_($F2);$Qe->apply_($G2);$Qe->apply_($H2);$Qe->apply_($J2);$Qe->apply_($K2);$Qe->apply_($L2);$Qe->apply_($N2);$Xe->apply_($C1);$Xe->apply_($D1);$Xe->apply_($F1);$Xe->apply_($G1);$Xe->apply_($H1);$Xe->apply_($I1);$Xe->apply_($J1);$Xe->apply_($K1);$Xe->apply_($L1);$Xe->apply_($M1);$Xe->apply_($N1);$Xe->apply_($O1);$Xe->apply_($P1);$Xe->apply_($Q1);$Xe->apply_($S1);$Xe->apply_($U1);$Xe->apply_($W1);$Xe->apply_($Y1);$Xe->apply_($Z1);$Xe->apply_($c2);$Xe->apply_($e2);$Xe->apply_($f2);$Xe->apply_($g2);$Xe->apply_($i2);$Xe->apply_($k2);$Xe->apply_($m2);$Xe->apply_($o2);$Xe->apply_($q2);$Xe->apply_($s2);$Xe->apply_($u2);$Xe->apply_($w2);$Xe->apply_($y2);$Xe->apply_($z2);$Xe->apply_($B2);$Xe->apply_($D2);$Xe->apply_($E2);$Xe->apply_($F2);$Xe->apply_($G2);$Xe->apply_($H2);$Xe->apply_($J2);$Xe->apply_($K2);$Xe->apply_($L2);$Xe->apply_($N2);$gf->apply_($C1);$gf->apply_($D1);$gf->apply_($F1);$gf->apply_($G1);$gf->apply_($H1);$gf->apply_($I1);$gf->apply_($J1);$gf->apply_($K1);$gf->apply_($L1);$gf->apply_($M1);$gf->apply_($N1);$gf->apply_($O1);$gf->apply_($P1);$gf->apply_($Q1);$gf->apply_($S1);$gf->apply_($U1);$gf->apply_($W1);$gf->apply_($Y1);$gf->apply_($Z1);$gf->apply_($c2);$gf->apply_($e2);$gf->apply_($f2);$gf->apply_($g2);$gf->apply_($i2);$gf->apply_($k2);$gf->apply_($m2);$gf->apply_($o2);$gf->apply_($q2);$gf->apply_($s2);$gf->apply_($u2);$gf->apply_($w2);$gf->apply_($y2);$gf->apply_($z2);$gf->apply_($B2);$gf->apply_($D2);$gf->apply_($E2);$gf->apply_($F2);$gf->apply_($G2);$gf->apply_($H2);$gf->apply_($J2);$gf->apply_($K2);$gf->apply_($L2);$gf->apply_($N2);$nf->apply_($C1);$nf->apply_($D1);$nf->apply_($F1);$nf->apply_($G1);$nf->apply_($H1);$nf->apply_($I1);$nf->apply_($J1);$nf->apply_($K1);$nf->apply_($L1);$nf->apply_($M1);$nf->apply_($N1);$nf->apply_($O1);$nf->apply_($P1);$nf->apply_($Q1);$nf->apply_($S1);$nf->apply_($U1);$nf->apply_($W1);$nf->apply_($Y1);$nf->apply_($Z1);$nf->apply_($c2);$nf->apply_($e2);$nf->apply_($f2);$nf->apply_($g2);$nf->apply_($i2);$nf->apply_($k2);$nf->apply_($m2);$nf->apply_($o2);$nf->apply_($q2);$nf->apply_($s2);$nf->apply_($u2);$nf->apply_($w2);$nf->apply_($y2);$nf->apply_($z2);$nf->apply_($B2);$nf->apply_($D2);$nf->apply_($E2);$nf->apply_($F2);$nf->apply_($G2);$nf->apply_($H2);$nf->apply_($J2);$nf->apply_($K2);$nf->apply_($L2);$nf->apply_($N2);$uf->apply_($C1);$uf->apply_($D1);$uf->apply_($F1);$uf->apply_($G1);$uf->apply_($H1);$uf->apply_($I1);$uf->apply_($J1);$uf->apply_($K1);$uf->apply_($L1);$uf->apply_($M1);$uf->apply_($N1);$uf->apply_($O1);$uf->apply_($P1);$uf->apply_($Q1);$uf->apply_($S1);$uf->apply_($U1);$uf->apply_($W1);$uf->apply_($Y1);$uf->apply_($Z1);$uf->apply_($c2);$uf->apply_($e2);$uf->apply_($f2);$uf->apply_($g2);$uf->apply_($i2);$uf->apply_($k2);$uf->apply_($m2);$uf->apply_($o2);$uf->apply_($q2);$uf->apply_($s2);$uf->apply_($u2);$uf->apply_($w2);$uf->apply_($y2);$uf->apply_($z2);$uf->apply_($B2);$uf->apply_($D2);$uf->apply_($E2);$uf->apply_($F2);$uf->apply_($G2);$uf->apply_($H2);$uf->apply_($J2);$uf->apply_($K2);$uf->apply_($L2);$uf->apply_($N2);$Ff->apply_($C1);$Ff->apply_($D1);$Ff->apply_($F1);$Ff->apply_($G1);$Ff->apply_($H1);$Ff->apply_($I1);$Ff->apply_($J1);$Ff->apply_($K1);$Ff->apply_($L1);$Ff->apply_($M1);$Ff->apply_($N1);$Ff->apply_($O1);$Ff->apply_($P1);$Ff->apply_($Q1);$Ff->apply_($S1);$Ff->apply_($U1);$Ff->apply_($W1);$Ff->apply_($Y1);$Ff->apply_($c2);$Ff->apply_($e2);$Ff->apply_($f2);$Ff->apply_($A);$Ff->apply_($g2);$Ff->apply_($i2);$Ff->apply_($k2);$Ff->apply_($m2);$Ff->apply_($o2);$Ff->apply_($q2);$Ff->apply_($r2);$Ff->apply_($s2);$Ff->apply_($t2);$Ff->apply_($u2);$Ff->apply_($w2);$Ff->apply_($y2);$Ff->apply_($z2);$Ff->apply_($B2);$Ff->apply_($D2);$Ff->apply_($E2);$Ff->apply_($F2);$Ff->apply_($H2);$Ff->apply_($J2);$Ff->apply_($K2);$Ff->apply_($L2);$Ff->apply_($N2);$Mf->apply_($C1);$Mf->apply_($D1);$Mf->apply_($F1);$Mf->apply_($G1);$Mf->apply_($H1);$Mf->apply_($I1);$Mf->apply_($J1);$Mf->apply_($K1);$Mf->apply_($L1);$Mf->apply_($M1);$Mf->apply_($N1);$Mf->apply_($O1);$Mf->apply_($P1);$Mf->apply_($Q1);$Mf->apply_($S1);$Mf->apply_($U1);$Mf->apply_($W1);$Mf->apply_($Y1);$Mf->apply_($c2);$Mf->apply_($e2);$Mf->apply_($f2);$Mf->apply_($g2);$Mf->apply_($i2);$Mf->apply_($k2);$Mf->apply_($m2);$Mf->apply_($o2);$Mf->apply_($q2);$Mf->apply_($s2);$Mf->apply_($u2);$Mf->apply_($w2);$Mf->apply_($y2);$Mf->apply_($z2);$Mf->apply_($B2);$Mf->apply_($D2);$Mf->apply_($F2);$Mf->apply_($H2);$Mf->apply_($J2);$Mf->apply_($K2);$Mf->apply_($L2);$Mf->apply_($N2);$qg->apply_($E1);$yg->apply_($E1);$Vg->apply_($G1);$Vg->apply_($H1);$Vg->apply_($I1);$Vg->apply_($J1);$Vg->apply_($K1);$Vg->apply_($L1);$Vg->apply_($M1);$Vg->apply_($N1);$Vg->apply_($O1);$Vg->apply_($P1);$Vg->apply_($Q1);$lh->apply_($q1);$Dh->apply_($q1);$Vh->apply_($r1);$ei->apply_($r1);$xi->apply_($s1);$Ei->apply_($s1);$Yi->apply_($s1);$uj->apply_($s1);$Cj->apply_($s1);$Sj->apply_($s1);$rk->apply_($t1);$rk->apply_($v1);$yk->apply_($t1);$Gk->apply_($t1);$Wk->apply_($t1);$Wk->apply_($v1);$jl->apply_($t1);$xl->apply_($t1);$xl->apply_($v1);$cm->apply_($K1);$um->apply_($u1);$Cm->apply_($u1);$Jm->apply_($u1);$Zm->apply_($u1);$jn->apply_($u1);$En->apply_($u1);$co->apply_($v1);$lo->apply_($v1);$Wo->apply_($Xo);$jp->apply_($w1);$wp->apply_($w1);$uq->apply_($z1);$Bq->apply_($z1);$Pq->apply_($z1);$nr->apply_($R1);$nr->apply_($T1);$nr->apply_($V1);$nr->apply_($M2);$zr->apply_($R1);$zr->apply_($T1);$zr->apply_($V1);$zr->apply_($M2);$Or->apply_($R1);$Or->apply_($T1);$Or->apply_($V1);$ks->apply_($R1);$ks->apply_($T1);$ks->apply_($V1);$As->apply_($S1);$As->apply_($U1);$As->apply_($W1);$Ws->apply_($T1);$ft->apply_($T1);$pt->apply_($T1);$Bt->apply_($T1);$eu->apply_($U1);$xu->apply_($V1);$Eu->apply_($V1);$Zu->apply_($Z1);$uv->apply_($d2);$Av->apply_($d2);$Vv->apply_($M);$dw->apply_($M);$mw->apply_($M);$uw->apply_($M);$Aw->apply_($M);$Gw->apply_($M);$Qw->apply_($M);$ex->apply_($f2);$Ix->apply_($A);$cy->apply_($A);$ly->apply_($A);$ty->apply_($A);$Jy->apply_($g2);$ez->apply_($h2);$lz->apply_($h2);$Fz->apply_($h2);$Zz->apply_($h2);$AA->apply_($Xo);$pB->apply_($qB);$JB->apply_($l2);$dC->apply_($l2);$mC->apply_($l2);$wC->apply_($l2);$BD->apply_($qB);$KD->apply_($n2);$KD->apply_($C);$KD->apply_($C2);$iE->apply_($p2);$rE->apply_($p2);$QE->apply_($r2);$WE->apply_($r2);$fF->apply_($r2);$JF->apply_($Xo);$TF->apply_($t2);$ZF->apply_($t2);$xG->apply_($v2);$xG->apply_($x2);$GG->apply_($v2);$UG->apply_($v2);$AH->apply_($C);$HH->apply_($C);$OH->apply_($C);$YH->apply_($C);$lI->apply_($C);$AI->apply_($z2);$II->apply_($z2);$dJ->apply_($A2);$tJ->apply_($A2);$AJ->apply_($A2);$TJ->apply_($C2);$ZJ->apply_($C2);$pK->apply_($Xo);$ni::self=$SL;&$P($N);&$P($W);&$P($e1);&$P($d3);&$P($f3);&$h3($f3);&$P($o3);&$P($I3);&$P($Q3);&$P($e4);&$P($q4);&$P($C4);&$P($E4);&$h3($E4);&$P($W4);&$P($Y4);&$h3($Y4);&$P($h5);&$P($n5);&$P($A5);&$P($N5);&$P($c6);&$P($p6);&$P($L6);&$P($R6);&$P($k7);&$P($u7);&$P($G7);&$P($S7);&$P($o8);&$P($q8);&$h3($q8);&$P($J8);&$P($Q8);&$P($m9);&$P($D9);&$P($R9);&$P($ra);&$P($za);&$P($Xa);&$P($pb);&$P($Fb);&$P($Zb);&$P($jc);&$P($lc);&$h3($lc);&$P($rc);&$P($zc);&$P($Kc);&$P($hd);&$P($nd);&$P($Cd);&$P($Id);&$P($Pd);&$P($Wd);&$P($fe);&$P($he);&$P($ne);&$P($ve);&$P($xe);&$h3($xe);&$P($Ge);&$P($Qe);&$P($Xe);&$P($gf);&$P($nf);&$P($uf);&$P($wf);&$P($yf);&$h3($yf);&$P($Ff);&$P($Mf);&$P($Of);&$h3($Of);&$P($Xf);&$h3($Xf);&$P($dg);&$h3($dg);&$P($fg);&$h3($fg);&$P($hg);&$h3($hg);&$P($qg);&$P($yg);&$P($Ag);&$h3($Ag);&$P($Fg);&$h3($Fg);&$P($Vg);&$P($Xg);&$h3($Xg);&$P($Zg);&$h3($Zg);&$P($lh);&$P($Dh);&$P($Fh);&$h3($Fh);&$P($Kh);&$h3($Kh);&$P($Vh);&$P($ei);&$P($gi);&$h3($gi);&$P($li);&$h3($li);&$P($xi);&$P($Ei);&$P($Yi);&$P($uj);&$P($Cj);&$P($Sj);&$P($Uj);&$h3($Uj);&$P($Zj);&$h3($Zj);&$P($rk);&$P($yk);&$P($Gk);&$P($Wk);&$P($jl);&$P($xl);&$P($Al);&$h3($Al);&$Dl($Al);&$P($cm);&$P($em);&$h3($em);&$P($um);&$P($Cm);&$P($Jm);&$P($Zm);&$P($jn);&$P($En);&$P($Gn);&$h3($Gn);&$P($Ln);&$h3($Ln);&$P($co);&$P($lo);&$P($no);&$h3($no);&$P($so);&$h3($so);&$P($Wo);&$P($jp);&$P($wp);&$P($yp);&$h3($yp);&$P($Dp);&$h3($Dp);&$P($Up);&$h3($Up);&$P($uq);&$P($Bq);&$P($Pq);&$P($Rq);&$h3($Rq);&$P($Wq);&$h3($Wq);&$P($nr);&$P($zr);&$P($Br);&$h3($Br);&$P($Or);&$P($ks);&$P($ms);&$h3($ms);&$ps($ms);&$P($ws);&$h3($ws);&$P($As);&$P($Cs);&$h3($Cs);&$P($Ws);&$P($ft);&$P($pt);&$P($Bt);&$P($Gt);&$h3($Gt);&$ps($Gt);&$Jt($Gt);&$P($eu);&$P($gu);&$h3($gu);&$P($xu);&$P($Eu);&$P($Gu);&$h3($Gu);&$ps($Gu);&$P($Lu);&$h3($Lu);&$P($Zu);&$P($dv);&$h3($dv);&$P($jv);&$h3($jv);&$P($uv);&$P($Av);&$P($Cv);&$h3($Cv);&$P($Hv);&$h3($Hv);&$P($Vv);&$P($dw);&$P($mw);&$P($uw);&$P($Aw);&$P($Gw);&$P($Qw);&$P($Sw);&$h3($Sw);&$P($ex);&$P($gx);&$h3($gx);&$P($Ix);&$P($cy);&$P($ly);&$P($ty);&$P($vy);&$h3($vy);&$yy($vy);&$P($Jy);&$P($Ly);&$h3($Ly);&$P($ez);&$P($lz);&$P($Fz);&$P($Zz);&$P($dA);&$h3($dA);&$P($iA);&$h3($iA);&$P($AA);&$P($GA);&$h3($GA);&$P($pB);&$P($JB);&$P($dC);&$P($mC);&$P($wC);&$P($yC);&$h3($yC);&$P($DC);&$h3($DC);&$P($BD);&$P($KD);&$P($MD);&$h3($MD);&$P($RD);&$h3($RD);&$P($iE);&$P($rE);&$P($tE);&$h3($tE);&$P($yE);&$h3($yE);&$P($QE);&$P($WE);&$P($fF);&$P($hF);&$h3($hF);&$P($nF);&$h3($nF);&$P($JF);&$P($TF);&$P($ZF);&$P($dG);&$h3($dG);&$P($jG);&$h3($jG);&$P($xG);&$P($zG);&$h3($zG);&$P($GG);&$P($UG);&$P($WG);&$h3($WG);&$P($fH);&$h3($fH);&$P($hH);&$h3($hH);&$P($AH);&$P($HH);&$P($OH);&$P($YH);&$P($lI);&$P($nI);&$h3($nI);&$qI($nI);&$P($AI);&$P($II);&$P($KI);&$h3($KI);&$P($dJ);&$P($tJ);&$P($AJ);&$P($CJ);&$h3($CJ);&$P($HJ);&$h3($HJ);&$P($TJ);&$P($ZJ);&$P($dK);&$h3($dK);&$P($iK);&$h3($iK);&$P($pK);&$P($wK);&$h3($wK);&$P($BK);&$h3($BK);&$P($KK);&$h3($KK);&$P($PK);&$h3($PK);&$P($XK);&$h3($XK);&$P($vL);&$h3($vL);ni->run(@ARGV);
__DATA__
