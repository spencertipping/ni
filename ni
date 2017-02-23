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
  $combined->into_sync($destination_io);
#;$H5=[$f,$G5];$I5=q#Concatenates multiple IO objects into a single read-only data source.
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
  $fd->be(10);                        \# move FD number
#;$j6=[$f,$i6];$k6=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$l6=[];$m6=[];$n6=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$o6=bless({$t,$m6,$v,$q,$w,$n6,$y,$z},$A);$p6=bless({$n,$l6,$p,$q,$r,$q,$s,$o6},$C);$q6=[$i,$k6,$p6];$r6=[$j6,$q6];$s6=q#/io/fd#;$t6=bless({$e,$r6,$Q,$s6},$S);$u6=q#ni.doc:/io/file#;$v6=q#
  my $f = ni('ni:/io/file')->new('/etc/passwd');
  my $f = ni('file:/etc/passwd');     \# same as above
  $f->into_sync(ni('fd:1'));          \# cat to stdout
#;$w6=[$f,$v6];$x6=q#warning#;$y6=q#Files overload the -X file test operators, but this feature wasn't
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
$some_class->add('/lib/foo');#;$Yc=[$f,$Xc];$Zc=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$cd=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$dd=[];$ed=[];$fd=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$gd=bless({$t,$ed,$v,$q,$w,$fd,$y,$z},$A);$hd=bless({$n,$dd,$p,$q,$r,$q,$s,$gd},$C);$id=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$jd=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$kd=[];$ld=[];$md=q#my $instances = 0;
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
now $instances == 0;#;$nd=bless({$t,$ld,$v,$q,$w,$md,$y,$z},$A);$od=bless({$n,$kd,$p,$q,$r,$q,$s,$nd},$C);$pd=[$i,$Zc,$cd,$hd,$id,$jd,$od];$qd=[$Yc,$pd];$rd=q#/lib/slice#;$sd=bless({$e,$qd,$Q,$rd},$S);$td=q#ni.doc:/semantic#;$ud=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$vd=[$i,$ud];$wd=[$vd];$xd=q#/semantic#;$yd=bless({$e,$wd,$Q,$xd},$S);$zd=q#ni:/class#;$Ad={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$K2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Bd={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Cd=q#/module#;$Dd=q#/lib/perlbranch.b#;$Ed={};$Fd=q#add#;$Gd=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Hd=bless({$w,$Gd,$y,$z},$A);$Id=q#apply#;$Jd=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$Kd=bless({$w,$Jd,$y,$z},$A);$Ld={$Fd,$Hd,$Id,$Kd};$Md=q#/lib/branch.b#;$Nd=bless({$r1,$Ed,$W2,$q,$X2,$q,$Y2,$Ld,$Q,$Md},$x2);$Od={};$Pd=q#$_[0]->namespace . ":" . $_[0]->{name}#;$Qd=bless({$w,$Pd,$y,$z},$A);$Rd={$Q,$Qd};$Sd=q#/lib/named.b#;$Td=bless({$r1,$Od,$W2,$V,$X2,$q,$Y2,$Rd,$Q,$Sd},$x2);$Ud={};$Vd=q#namespace#;$Wd=q#'ni'#;$Xd=bless({$w,$Wd,$y,$z},$A);$Yd={$Vd,$Xd};$Zd=q#/lib/named_in_ni.b#;$ce=bless({$r1,$Ud,$W2,$q,$X2,$q,$Y2,$Yd,$Q,$Zd},$x2);$de={};$ee=q#package#;$fe=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$ge=bless({$w,$fe,$y,$z},$A);$he={$ee,$ge};$ie=q#/lib/namespaced.b#;$je=bless({$r1,$de,$W2,$q,$X2,$q,$Y2,$he,$Q,$ie},$x2);$ke={};$le=q#resolve#;$me=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$ne=bless({$w,$me,$y,$z},$A);$oe={$le,$ne};$pe=q#/lib/resolver.b#;$qe=bless({$r1,$ke,$W2,$q,$X2,$q,$Y2,$oe,$Q,$pe},$x2);$re=[$Nd,$Td,$ce,$je,$qe];$se=bless({$Q,$Dd,$v1,$re},$z2);$te={};$ue=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$ve=bless({$w,$ue,$y,$z},$A);$we={$L4,$ve};$xe=q#/lib/class_init.b#;$ye=bless({$r1,$te,$W2,$m3,$X2,$q,$Y2,$we,$Q,$xe},$x2);$ze={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$f2,1,$g2,1,$h2,1,$i2,1,$j2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$x2,1,$y2,1,$z2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Ae=q#/lib/behavior#;$Be={};$Ce=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$De=bless({$w,$Ce,$y,$z},$A);$Ee={$e,$De};$Fe=q#/lib/documentable.b#;$Ge=bless({$r1,$Be,$W2,$q,$X2,$q,$Y2,$Ee,$Q,$Fe},$x2);$He=[$k3,$Ge];$Ie=bless({$r1,$ze,$Q,$Ae,$v1,$He},$g2);$Je={$I1,1,$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$h2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$J2,1,$K2,1,$L2,1,$M2,1,$O2,1,$P2,1,$Q2,1,$S2,1};$Ke=q#/lib/definition.b#;$Le={};$Me=q#def#;$Ne=q#my $self = shift;
my $name = shift;
$name = "$$self{name}_$name" unless $name =~ /^\\//;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Oe=bless({$w,$Ne,$y,$z},$A);$Pe={$Me,$Oe};$Qe=q#/lib/definition_def.b#;$Re=bless({$r1,$Le,$W2,$q,$X2,$q,$Y2,$Pe,$Q,$Qe},$x2);$Se={};$Te=q#ro#;$Ue=q#my ($self, $slice, @rs) = @_;
$self->def($slice, map +($_ => fn qq{shift->{'$_'}}), @rs);#;$Ve=bless({$w,$Ue,$y,$z},$A);$We=q#rw#;$Xe=q#my ($self, $slice, @as) = @_;
$self->def($slice,
  map +($_ => fn qq{
    if (\\@_ == 2) {
      \\$_[0]->{'$_'} = \\$_[1];
      return \\$_[0];
    } else {
      return shift->{'$_'};
    }
  }), @as);#;$Ye=bless({$w,$Xe,$y,$z},$A);$Ze={$Te,$Ve,$We,$Ye};$cf=q#/lib/accessor.b#;$df=bless({$r1,$Se,$W2,$q,$X2,$q,$Y2,$Ze,$Q,$cf},$x2);$ef={};$ff=q#(""#;$gf=q#shift->name#;$hf=bless({$w,$gf,$y,$z},$A);$if={$ff,$hf};$jf=q#/lib/name_as_string.b#;$kf=bless({$r1,$ef,$W2,$q,$X2,$q,$Y2,$if,$Q,$jf},$x2);$lf={};$mf=q#(eq#;$nf=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$of=bless({$w,$nf,$y,$z},$A);$pf={$mf,$of};$qf=q#/lib/ref_eq.b#;$rf=bless({$r1,$lf,$W2,$q,$X2,$q,$Y2,$pf,$Q,$qf},$x2);$sf={};$tf=q#defdata#;$uf=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$vf=bless({$w,$uf,$y,$z},$A);$wf={$tf,$vf};$xf=q#/lib/definition_defdata.b#;$yf=bless({$r1,$sf,$W2,$q,$X2,$q,$Y2,$wf,$Q,$xf},$x2);$zf={};$Af=q#instantiate_with_defaults#;$Bf=q#my ($class, @slots) = @_;
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
    }));#;$Cf=bless({$w,$Bf,$y,$z},$A);$Df={$Af,$Cf};$Ef=q#/lib/definition_init_with_defaults.b#;$Ff=bless({$r1,$zf,$W2,$q,$X2,$q,$Y2,$Df,$Q,$Ef},$x2);$Gf=[$Re,$df,$kf,$rf,$yf,$Ff];$Hf=bless({$r1,$Je,$Q,$Ke,$v1,$Gf},$h2);$If=[$se,$ye,$k3,$Ie,$Hf];$Jf=bless({$r1,$Bd,$Q,$Cd,$v1,$If},$M2);$Kf={};$Lf=q#new#;$Mf=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$Nf=bless({$w,$Mf,$y,$z},$A);$Of={$Lf,$Nf};$Pf=q#/lib/instantiable.b#;$Qf=bless({$r1,$Kf,$Y2,$Of,$Q,$Pf},$x2);$Rf={};$Sf=q#child#;$Tf=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$Uf=bless({$w,$Tf,$y,$z},$A);$Vf={$Sf,$Uf};$Wf=q#/lib/subclass.b#;$Xf=bless({$r1,$Rf,$W2,$q,$X2,$q,$Y2,$Vf,$Q,$Wf},$x2);$Yf=[$Jf,$Qf,$ye,$Jf,$Xf];$Zf=bless({$r1,$Ad,$Q,$R,$v1,$Yf},$J1);$cg=q#ni:/class.c#;$dg={$J1,1,$Q2,1};$eg=q#/class.c#;$fg={$J1,1,$M2,1,$Q2,1};$gg=q#/module.c#;$hg={$J1,1,$L1,1,$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1,$Y1,1,$c2,1,$e2,1,$g2,1,$i2,1,$k2,1,$l2,1,$m2,1,$o2,1,$q2,1,$s2,1,$u2,1,$w2,1,$y2,1,$A2,1,$C2,1,$E2,1,$F2,1,$H2,1,$I2,1,$M2,1,$O2,1,$Q2,1,$S2,1};$ig=q#/object.c#;$jg=[$Zf];$kg=bless({$r1,$hg,$Q,$ig,$v1,$jg},$J2);$lg={$J1,1,$g2,1,$i2,1,$k2,1,$y2,1,$A2,1,$M2,1,$Q2,1};$mg=q#/lib/behavior.c#;$ng=[$kg];$og=bless({$r1,$lg,$Q,$mg,$v1,$ng},$J2);$pg=[$kg,$Qf,$og];$qg=bless({$r1,$fg,$Q,$gg,$v1,$pg},$J2);$rg=[$qg];$sg=bless({$r1,$dg,$Q,$eg,$v1,$rg},$J2);$tg=q#ni:/fabric#;$ug=q#fabric#;$vg={$ug,1};$wg=[];$xg=bless({$r1,$vg,$Q,$d1,$v1,$wg},$L2);$yg=q#ni:/fabric/remote#;$zg={$K1,1};$Ag={};$Bg=[];$Cg=q#my ($class, $rmi, $name) = @_;
+{rmi  => $rmi,
  name => $name};#;$Dg=bless({$t,$Bg,$v,$q,$w,$Cg,$y,$z},$A);$Eg={$L4,$Dg};$Fg=q#/fabric/remote_init.b#;$Gg=bless({$r1,$Ag,$W2,$q,$X2,$q,$Y2,$Eg,$Q,$Fg},$x2);$Hg={};$Ig=q#AUTOLOAD#;$Jg=[];$Kg=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{rmi}->call($$self{name}, $method, @_);#;$Lg=bless({$t,$Jg,$v,$q,$w,$Kg,$y,$z},$A);$Mg={$Ig,$Lg};$Ng=q#/fabric/remote_proxy.b#;$Og=bless({$r1,$Hg,$W2,$q,$X2,$q,$Y2,$Mg,$Q,$Ng},$x2);$Pg=[$k3,$Gg,$Og];$Qg=bless({$r1,$zg,$Q,$j1,$v1,$Pg},$L1);$Rg=q#ni:/fabric/remote.c#;$Sg={$L1,1};$Tg=q#/fabric/remote.c#;$Ug=[$kg];$Vg=bless({$r1,$Sg,$Q,$Tg,$v1,$Ug},$J2);$Wg=q#ni:/fabric/remote_init.b#;$Xg=q#ni:/fabric/remote_proxy.b#;$Yg=q#ni:/fabric/rmi#;$Zg=q#ni:/fabric/rmi.c#;$ch={$M1,1};$dh=q#/fabric/rmi.c#;$eh={$M1,1,$N1,1,$O1,1,$P1,1,$Q1,1,$R1,1,$S1,1,$T1,1,$U1,1,$V1,1,$W1,1};$fh=q#/io/object.c#;$gh={};$hh=q#def_transfer_method#;$ih=[];$jh=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$kh=bless({$t,$ih,$v,$q,$w,$jh,$y,$z},$A);$lh={$hh,$kh};$mh=q#/io/object.c_transfer_def.b#;$nh=bless({$r1,$gh,$W2,$q,$X2,$q,$Y2,$lh,$Q,$mh},$x2);$oh=[$kg,$nh];$ph=bless({$r1,$eh,$Q,$fh,$v1,$oh},$J2);$qh=[$ph];$rh=bless({$r1,$ch,$Q,$dh,$v1,$qh},$J2);$sh=q#ni:/fabric/rmi_init.b#;$th=q#ni:/io#;$uh={$oa,1};$vh=[];$wh=bless({$r1,$uh,$Q,$q5,$v1,$vh},$L2);$xh=q#ni:/io/buffer#;$yh={$w1,1};$zh={};$Ah=[];$Bh=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Ch=bless({$t,$Ah,$v,$q,$w,$Bh,$y,$z},$A);$Dh={$L4,$Ch};$Eh=q#/io/buffer_init.b#;$Fh=bless({$r1,$zh,$W2,$q,$X2,$q,$Y2,$Dh,$Q,$Eh},$x2);$Gh={};$Hh=[];$Ih=q#my $self = shift;
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
}#;$Jh=bless({$t,$Hh,$v,$q,$w,$Ih,$y,$z},$A);$Kh=q#read_capacity#;$Lh=[];$Mh=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Nh=bless({$t,$Lh,$v,$q,$w,$Mh,$y,$z},$A);$Oh=[];$Ph=q#my $self = shift;
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
}#;$Qh=bless({$t,$Oh,$v,$q,$w,$Ph,$y,$z},$A);$Rh=q#write_capacity#;$Sh=[];$Th=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Uh=bless({$t,$Sh,$v,$q,$w,$Th,$y,$z},$A);$Vh={$T7,$Jh,$Kh,$Nh,$X7,$Qh,$Rh,$Uh};$Wh=q#/io/buffer_io.b#;$Xh=bless({$r1,$Gh,$W2,$q,$X2,$q,$Y2,$Vh,$Q,$Wh},$x2);$Yh=[$J4,$Fh,$Xh];$Zh=bless({$r1,$yh,$Q,$D5,$v1,$Yh},$N1);$ci=q#ni:/io/buffer.c#;$di={$N1,1};$ei=q#/io/buffer.c#;$fi=[$ph];$gi=bless({$r1,$di,$Q,$ei,$v1,$fi},$J2);$hi=q#ni:/io/buffer_init.b#;$ii=q#ni:/io/buffer_io.b#;$ji=q#ni:/io/cat#;$ki={$x1,1};$li={};$mi=[];$ni=q#shift; +{fs => [@_]}#;$oi=bless({$t,$mi,$v,$q,$w,$ni,$y,$z},$A);$pi={$L4,$oi};$qi=q#/io/cat_init.b#;$ri=bless({$r1,$li,$W2,$q,$X2,$q,$Y2,$pi,$Q,$qi},$x2);$si={};$ti=[];$ui=q#my $fs = shift->{fs};
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
$total_read;#;$vi=bless({$t,$ti,$v,$q,$w,$ui,$y,$z},$A);$wi={$T7,$vi};$xi=q#/io/cat_read.b#;$yi=bless({$r1,$si,$W2,$q,$X2,$q,$Y2,$wi,$Q,$xi},$x2);$zi=[$J4,$ri,$yi];$Ai=bless({$r1,$ki,$Q,$Q5,$v1,$zi},$O1);$Bi=q#ni:/io/cat.c#;$Ci={$O1,1};$Di=q#/io/cat.c#;$Ei=[$ph];$Fi=bless({$r1,$Ci,$Q,$Di,$v1,$Ei},$J2);$Gi=q#ni:/io/cat_init.b#;$Hi=q#ni:/io/cat_read.b#;$Ii=q#ni:/io/exec#;$Ji={$y1,1};$Ki={};$Li=q#argv#;$Mi=[];$Ni=q#shift->{'argv'}#;$Oi=bless({$t,$Mi,$v,$q,$w,$Ni,$y,$z},$A);$Pi={$Li,$Oi};$Qi=q#/io/exec_ro.b#;$Ri=bless({$r1,$Ki,$W2,$q,$X2,$q,$Y2,$Pi,$Q,$Qi},$x2);$Si={};$Ti=[];$Ui=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Vi=bless({$t,$Ti,$v,$q,$w,$Ui,$y,$z},$A);$Wi={$L4,$Vi};$Xi=q#/io/exec_init.b#;$Yi=bless({$r1,$Si,$W2,$q,$X2,$q,$Y2,$Wi,$Q,$Xi},$x2);$Zi={};$cj=q#connect#;$dj=[];$ej=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$fj=bless({$t,$dj,$v,$q,$w,$ej,$y,$z},$A);$gj=q#in_pipe#;$hj=[];$ij=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$jj=bless({$t,$hj,$v,$q,$w,$ij,$y,$z},$A);$kj=q#out_pipe#;$lj=[];$mj=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$nj=bless({$t,$lj,$v,$q,$w,$mj,$y,$z},$A);$oj=q#setup_stdio#;$pj=[];$qj=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$rj=bless({$t,$pj,$v,$q,$w,$qj,$y,$z},$A);$sj={$cj,$fj,$gj,$jj,$kj,$nj,$oj,$rj};$tj=q#/io/exec_io_setup.b#;$uj=bless({$r1,$Zi,$W2,$q,$X2,$q,$Y2,$sj,$Q,$tj},$x2);$vj={};$wj=q#binds_fd#;$xj=[];$yj=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$zj=bless({$t,$xj,$v,$q,$w,$yj,$y,$z},$A);$Aj=[];$Bj=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Cj=bless({$t,$Aj,$v,$q,$w,$Bj,$y,$z},$A);$Dj=[];$Ej=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Fj=bless({$t,$Dj,$v,$q,$w,$Ej,$y,$z},$A);$Gj=[];$Hj=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Ij=bless({$t,$Gj,$v,$q,$w,$Hj,$y,$z},$A);$Jj=[];$Kj=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Lj=bless({$t,$Jj,$v,$q,$w,$Kj,$y,$z},$A);$Mj={$wj,$zj,$h8,$Cj,$l8,$Fj,$p8,$Ij,$t8,$Lj};$Nj=q#/io/exec_io_accessors.b#;$Oj=bless({$r1,$vj,$W2,$q,$X2,$q,$Y2,$Mj,$Q,$Nj},$x2);$Pj={};$Qj=q#env#;$Rj=[];$Sj=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Tj=bless({$t,$Rj,$v,$q,$w,$Sj,$y,$z},$A);$Uj={$Qj,$Tj};$Vj=q#/io/exec_env.b#;$Wj=bless({$r1,$Pj,$W2,$q,$X2,$q,$Y2,$Uj,$Q,$Vj},$x2);$Xj={};$Yj=q#exec#;$Zj=[];$ck=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$dk=bless({$t,$Zj,$v,$q,$w,$ck,$y,$z},$A);$ek=q#fork#;$fk=[];$gk=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$hk=bless({$t,$fk,$v,$q,$w,$gk,$y,$z},$A);$ik=q#move_fds#;$jk=[];$kk=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$lk=bless({$t,$jk,$v,$q,$w,$kk,$y,$z},$A);$mk={$Yj,$dk,$ek,$hk,$ik,$lk};$nk=q#/io/exec_fork.b#;$ok=bless({$r1,$Xj,$W2,$q,$X2,$q,$Y2,$mk,$Q,$nk},$x2);$pk=[$J4,$Ri,$Yi,$uj,$Oj,$Wj,$ok];$qk=bless({$r1,$Ji,$Q,$f6,$v1,$pk},$P1);$rk=q#ni:/io/exec.c#;$sk={$P1,1};$tk=q#/io/exec.c#;$uk=[$ph];$vk=bless({$r1,$sk,$Q,$tk,$v1,$uk},$J2);$wk=q#ni:/io/exec_env.b#;$xk=q#ni:/io/exec_fork.b#;$yk=q#ni:/io/exec_init.b#;$zk=q#ni:/io/exec_io_accessors.b#;$Ak=q#ni:/io/exec_io_setup.b#;$Bk=q#ni:/io/exec_ro.b#;$Ck=q#ni:/io/fd#;$Dk={$z1,1};$Ek=q#read_fd_mask#;$Fk={};$Gk=[];$Hk=q#shift->{'fd'}#;$Ik=bless({$t,$Gk,$v,$q,$w,$Hk,$y,$z},$A);$Jk={$h8,$Ik};$Kk=q#/io/fd_readers.b#;$Lk=bless({$r1,$Fk,$W2,$q,$X2,$q,$Y2,$Jk,$Q,$Kk},$x2);$Mk={};$Nk=[];$Ok=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Pk=bless({$t,$Nk,$v,$q,$w,$Ok,$y,$z},$A);$Qk={$L4,$Pk};$Rk=q#/io/fd_init.b#;$Sk=bless({$r1,$Mk,$W2,$q,$X2,$q,$Y2,$Qk,$Q,$Rk},$x2);$Tk={};$Uk=q#be#;$Vk=[];$Wk=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Xk=bless({$t,$Vk,$v,$q,$w,$Wk,$y,$z},$A);$Yk={$Uk,$Xk};$Zk=q#/io/fd_shell.b#;$cl=bless({$r1,$Tk,$W2,$q,$X2,$q,$Y2,$Yk,$Q,$Zk},$x2);$dl={};$el=q#cloexec#;$fl=[];$gl=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$hl=bless({$t,$fl,$v,$q,$w,$gl,$y,$z},$A);$il=q#fcntl_flag#;$jl=[];$kl=q#my ($self, $flag, $value) = @_;
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
}#;$ll=bless({$t,$jl,$v,$q,$w,$kl,$y,$z},$A);$ml=q#nonblock#;$nl=[];$ol=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$pl=bless({$t,$nl,$v,$q,$w,$ol,$y,$z},$A);$ql={$el,$hl,$il,$ll,$ml,$pl};$rl=q#/io/fd_fcntl.b#;$sl=bless({$r1,$dl,$W2,$q,$X2,$q,$Y2,$ql,$Q,$rl},$x2);$tl={};$ul=[];$vl=q#shift->close#;$wl=bless({$t,$ul,$v,$q,$w,$vl,$y,$z},$A);$xl=q#close#;$yl=[];$zl=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Al=bless({$t,$yl,$v,$q,$w,$zl,$y,$z},$A);$Bl={$xl,$Al};$Cl=q#/io/fd_gc.b#;$Dl=bless({$r1,$tl,$W2,$q,$X2,$wl,$Y2,$Bl,$Q,$Cl},$x2);$El={};$Fl=q#close_perl_ios#;$Gl=[];$Hl=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Il=bless({$t,$Gl,$v,$q,$w,$Hl,$y,$z},$A);$Jl=[];$Kl=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Ll=bless({$t,$Jl,$v,$q,$w,$Kl,$y,$z},$A);$Ml=[];$Nl=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Ol=bless({$t,$Ml,$v,$q,$w,$Nl,$y,$z},$A);$Pl={$Fl,$Il,$T7,$Ll,$X7,$Ol};$Ql=q#/io/fd_perlio.b#;$Rl=bless({$r1,$El,$W2,$q,$X2,$q,$Y2,$Pl,$Q,$Ql},$x2);$Sl=[$J4,$Lk,$Sk,$cl,$sl,$Dl,$Rl];$Tl=q#write_fd_mask#;$Ul=bless({$r1,$Dk,$Q,$s6,$Ek,$z,$v1,$Sl,$Tl,$z},$Q1);$Vl=[];$Wl=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Xl=bless({$t,$Vl,$v,$q,$w,$Wl,$y,$z},$A);$Yl=q#ni:/io/fd.c#;$Zl={$Q1,1};$cm=q#/io/fd.c#;$dm={};$em=q#clear_fd#;$fm=[];$gm=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$hm=bless({$t,$fm,$v,$q,$w,$gm,$y,$z},$A);$im=q#read_fd#;$jm=[];$km=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$lm=bless({$t,$jm,$v,$q,$w,$km,$y,$z},$A);$mm=q#select#;$nm=[];$om=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$pm=bless({$t,$nm,$v,$q,$w,$om,$y,$z},$A);$qm=q#write_fd#;$rm=[];$sm=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$tm=bless({$t,$rm,$v,$q,$w,$sm,$y,$z},$A);$um={$em,$hm,$im,$lm,$mm,$pm,$qm,$tm};$vm=q#/io/fd.c_selector.b#;$wm=bless({$r1,$dm,$W2,$Xl,$X2,$q,$Y2,$um,$Q,$vm},$x2);$xm=[$ph,$wm];$ym=bless({$r1,$Zl,$Q,$cm,$v1,$xm},$J2);$zm=q#ni:/io/fd.c_selector.b#;$Am=q#ni:/io/fd_fcntl.b#;$Bm=q#ni:/io/fd_gc.b#;$Cm=q#ni:/io/fd_init.b#;$Dm=q#ni:/io/fd_perlio.b#;$Em=q#ni:/io/fd_readers.b#;$Fm=q#ni:/io/fd_shell.b#;$Gm=q#ni:/io/file#;$Hm={$A1,1};$Im={};$Jm=[];$Km=q#shift->{'name'}#;$Lm=bless({$t,$Jm,$v,$q,$w,$Km,$y,$z},$A);$Mm={$Q,$Lm};$Nm=q#/io/file_readers.b#;$Om=bless({$r1,$Im,$W2,$q,$X2,$q,$Y2,$Mm,$Q,$Nm},$x2);$Pm={};$Qm=q#mode#;$Rm=[];$Sm=q#if (@_ == 2) {
  $_[0]->{'mode'} = $_[1];
  return $_[0];
} else {
  return shift->{'mode'};
}#;$Tm=bless({$t,$Rm,$v,$q,$w,$Sm,$y,$z},$A);$Um={$Qm,$Tm};$Vm=q#/io/file_accessors.b#;$Wm=bless({$r1,$Pm,$W2,$q,$X2,$q,$Y2,$Um,$Q,$Vm},$x2);$Xm={};$Ym=[];$Zm=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$cn=bless({$t,$Ym,$v,$q,$w,$Zm,$y,$z},$A);$dn={$L4,$cn};$en=q#/io/file_init.b#;$fn=bless({$r1,$Xm,$W2,$q,$X2,$q,$Y2,$dn,$Q,$en},$x2);$gn={};$hn=q#(-X#;$in=[];$jn=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$kn=bless({$t,$in,$v,$q,$w,$jn,$y,$z},$A);$ln=q#mv#;$mn=[];$nn=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$on=bless({$t,$mn,$v,$q,$w,$nn,$y,$z},$A);$pn=q#rm#;$qn=[];$rn=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$sn=bless({$t,$qn,$v,$q,$w,$rn,$y,$z},$A);$tn={$hn,$kn,$ln,$on,$pn,$sn};$un=q#/io/file_fns.b#;$vn=bless({$r1,$gn,$W2,$q,$X2,$q,$Y2,$tn,$Q,$un},$x2);$wn={};$xn=q#atomic_update#;$yn=[];$zn=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$An=bless({$t,$yn,$v,$q,$w,$zn,$y,$z},$A);$Bn={$xn,$An};$Cn=q#/io/file_update.b#;$Dn=bless({$r1,$wn,$W2,$q,$X2,$q,$Y2,$Bn,$Q,$Cn},$x2);$En={};$Fn=[];$Gn=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Hn=bless({$t,$Fn,$v,$q,$w,$Gn,$y,$z},$A);$In=q#r#;$Jn=[];$Kn=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Ln=bless({$t,$Jn,$v,$q,$w,$Kn,$y,$z},$A);$Mn=[];$Nn=q#shift->r->read(@_)#;$On=bless({$t,$Mn,$v,$q,$w,$Nn,$y,$z},$A);$Pn=q#w#;$Qn=[];$Rn=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Sn=bless({$t,$Qn,$v,$q,$w,$Rn,$y,$z},$A);$Tn=[];$Un=q#shift->w->write(@_)#;$Vn=bless({$t,$Tn,$v,$q,$w,$Un,$y,$z},$A);$Wn={$xl,$Hn,$In,$Ln,$T7,$On,$Pn,$Sn,$X7,$Vn};$Xn=q#/io/file_io.b#;$Yn=bless({$r1,$En,$W2,$q,$X2,$q,$Y2,$Wn,$Q,$Xn},$x2);$Zn=[$J4,$Om,$Wm,$fn,$vn,$Dn,$Yn];$co=bless({$r1,$Hm,$Q,$O6,$v1,$Zn},$R1);$do=q#ni:/io/file.c#;$eo={$R1,1};$fo=q#/io/file.c#;$go=[$ph];$ho=bless({$r1,$eo,$Q,$fo,$v1,$go},$J2);$io=q#ni:/io/file_accessors.b#;$jo=q#ni:/io/file_fns.b#;$ko=q#ni:/io/file_init.b#;$lo=q#ni:/io/file_io.b#;$mo=q#ni:/io/file_readers.b#;$no=q#ni:/io/file_update.b#;$oo=q#ni:/io/file_update_fd#;$po={$B1,1};$qo={};$ro=[];$so=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$to=bless({$t,$ro,$v,$q,$w,$so,$y,$z},$A);$uo={$L4,$to};$vo=q#/io/file_update_fd_fd_init.b#;$wo=bless({$r1,$qo,$W2,$q,$X2,$q,$Y2,$uo,$Q,$vo},$x2);$xo={};$yo=[];$zo=bless({$t,$yo,$v,$q,$w,$vl,$y,$z},$A);$Ao=[];$Bo=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Co=bless({$t,$Ao,$v,$q,$w,$Bo,$y,$z},$A);$Do={$xl,$Co};$Eo=q#/io/file_update_fd_fd_gc.b#;$Fo=bless({$r1,$xo,$W2,$q,$X2,$zo,$Y2,$Do,$Q,$Eo},$x2);$Go=[$J4,$Lk,$sl,$Rl,$wo,$Fo];$Ho=bless({$r1,$po,$Q,$U6,$v1,$Go},$S1);$Io=q#ni:/io/file_update_fd.c#;$Jo={$S1,1};$Ko=q#/io/file_update_fd.c#;$Lo=[$ph];$Mo=bless({$r1,$Jo,$Q,$Ko,$v1,$Lo},$J2);$No=q#ni:/io/file_update_fd_fd_gc.b#;$Oo=q#ni:/io/file_update_fd_fd_init.b#;$Po=q#ni:/io/named_io_fns.b#;$Qo={};$Ro=q#fcntl#;$So=[];$To=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Uo=bless({$t,$So,$v,$q,$w,$To,$y,$z},$A);$Vo=[];$Wo=q#CORE::fork#;$Xo=bless({$t,$Vo,$v,$q,$w,$Wo,$y,$z},$A);$Yo=q#open2#;$Zo=[];$cp=q#CORE::open $_[0], $_[1]#;$dp=bless({$t,$Zo,$v,$q,$w,$cp,$y,$z},$A);$ep=q#rename#;$fp=[];$gp=q#CORE::rename $_[0], $_[1]#;$hp=bless({$t,$fp,$v,$q,$w,$gp,$y,$z},$A);$ip=q#unlink#;$jp=[];$kp=q#CORE::unlink @_#;$lp=bless({$t,$jp,$v,$q,$w,$kp,$y,$z},$A);$mp=q#waitpid#;$np=[];$op=q#CORE::waitpid $_[0], $_[1]#;$pp=bless({$t,$np,$v,$q,$w,$op,$y,$z},$A);$qp={$Ro,$Uo,$ek,$Xo,$Yo,$dp,$ep,$hp,$ip,$lp,$mp,$pp};$rp=q#/io/named_io_fns.b#;$sp=bless({$r1,$Qo,$W2,$q,$X2,$q,$Y2,$qp,$Q,$rp},$x2);$tp=q#main#;$up=q#ni:/io/null#;$vp={$C1,1};$wp=q#/io/null#;$xp={};$yp=[];$zp=q#+{fd => undef}#;$Ap=bless({$t,$yp,$v,$q,$w,$zp,$y,$z},$A);$Bp={$L4,$Ap};$Cp=q#/io/null_init.b#;$Dp=bless({$r1,$xp,$W2,$q,$X2,$q,$Y2,$Bp,$Q,$Cp},$x2);$Ep={};$Fp=[];$Gp=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Hp=bless({$t,$Fp,$v,$q,$w,$Gp,$y,$z},$A);$Ip=[];$Jp=q#shift->fd->read(@_)#;$Kp=bless({$t,$Ip,$v,$q,$w,$Jp,$y,$z},$A);$Lp=[];$Mp=q#shift->fd->write(@_)#;$Np=bless({$t,$Lp,$v,$q,$w,$Mp,$y,$z},$A);$Op={$h8,$Hp,$T7,$Kp,$X7,$Np};$Pp=q#/io/null_io.b#;$Qp=bless({$r1,$Ep,$W2,$q,$X2,$q,$Y2,$Op,$Q,$Pp},$x2);$Rp=[$J4,$Dp,$Qp];$Sp=bless({$r1,$vp,$Q,$wp,$v1,$Rp},$T1);$Tp=q#ni:/io/null.c#;$Up={$T1,1};$Vp=q#/io/null.c#;$Wp=[$ph];$Xp=bless({$r1,$Up,$Q,$Vp,$v1,$Wp},$J2);$Yp=q#ni:/io/null_init.b#;$Zp=q#ni:/io/null_io.b#;$cq=q#ni:/io/object#;$dq=q#ni:/io/object.c#;$eq=q#ni:/io/object.c_transfer_def.b#;$fq=q#ni:/io/object_checks.b#;$gq=q#ni:/io/object_constructors.b#;$hq=q#ni:/io/object_memory.b#;$iq=q#ni:/io/object_ops.b#;$jq=q#ni:/io/object_transfer_async.b#;$kq=q#ni:/io/object_transfer_sync.b#;$lq=q#ni:/io/pid#;$mq=q#ni:/io/pid.c#;$nq={$V1,1};$oq=q#/io/pid.c#;$pq=[$ph];$qq=bless({$r1,$nq,$Q,$oq,$v1,$pq},$J2);$rq=q#ni:/io/pid_accessors.b#;$sq=q#ni:/io/pid_init.b#;$tq=q#ni:/io/pid_io.b#;$uq=q#ni:/io/pid_readers.b#;$vq=q#ni:/io/pid_wait.b#;$wq=q#ni:/io/str#;$xq={$F1,1};$yq=q#/io/str#;$zq={};$Aq=q#data#;$Bq=[];$Cq=q#shift->{'data'}#;$Dq=bless({$t,$Bq,$v,$q,$w,$Cq,$y,$z},$A);$Eq=q#end#;$Fq=[];$Gq=q#shift->{'end'}#;$Hq=bless({$t,$Fq,$v,$q,$w,$Gq,$y,$z},$A);$Iq=q#start#;$Jq=[];$Kq=q#shift->{'start'}#;$Lq=bless({$t,$Jq,$v,$q,$w,$Kq,$y,$z},$A);$Mq={$Aq,$Dq,$Eq,$Hq,$Iq,$Lq};$Nq=q#/io/str_ro.b#;$Oq=bless({$r1,$zq,$W2,$q,$X2,$q,$Y2,$Mq,$Q,$Nq},$x2);$Pq={};$Qq=[];$Rq=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Sq=bless({$t,$Qq,$v,$q,$w,$Rq,$y,$z},$A);$Tq={$L4,$Sq};$Uq=q#/io/str_init.b#;$Vq=bless({$r1,$Pq,$W2,$q,$X2,$q,$Y2,$Tq,$Q,$Uq},$x2);$Wq={};$Xq=[];$Yq=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Zq=bless({$t,$Xq,$v,$q,$w,$Yq,$y,$z},$A);$cr=q#remaining#;$dr=[];$er=q#my $self = shift; $$self{end} - $$self{start}#;$fr=bless({$t,$dr,$v,$q,$w,$er,$y,$z},$A);$gr=[];$hr=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$ir=bless({$t,$gr,$v,$q,$w,$hr,$y,$z},$A);$jr={$T7,$Zq,$cr,$fr,$X7,$ir};$kr=q#/io/str_io.b#;$lr=bless({$r1,$Wq,$W2,$q,$X2,$q,$Y2,$jr,$Q,$kr},$x2);$mr=[$J4,$Oq,$Vq,$lr];$nr=bless({$r1,$xq,$Q,$yq,$v1,$mr},$W1);$or=q#ni:/io/str.c#;$pr={$W1,1};$qr=q#/io/str.c#;$rr=[$ph];$sr=bless({$r1,$pr,$Q,$qr,$v1,$rr},$J2);$tr=q#ni:/io/str_init.b#;$ur=q#ni:/io/str_io.b#;$vr=q#ni:/io/str_ro.b#;$wr=q#ni:/io/transfer#;$xr={$X1,1,$Z1,1,$d2,1};$yr=q#/io/transfer#;$zr={$X1,1,$Z1,1,$d2,1,$R2,1};$Ar=q#/semantic/task#;$Br={};$Cr=[];$Dr=q#shift->{'outcome'}#;$Er=bless({$t,$Cr,$v,$q,$w,$Dr,$y,$z},$A);$Fr={$r,$Er};$Gr=q#/semantic/task_ro.b#;$Hr=bless({$r1,$Br,$W2,$q,$X2,$q,$Y2,$Fr,$Q,$Gr},$x2);$Ir={};$Jr=q#failure#;$Kr=[];$Lr=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Mr=bless({$t,$Kr,$v,$q,$w,$Lr,$y,$z},$A);$Nr=q#success#;$Or=[];$Pr=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Qr=bless({$t,$Or,$v,$q,$w,$Pr,$y,$z},$A);$Rr={$Jr,$Mr,$Nr,$Qr};$Sr=q#/semantic/task_outcome.b#;$Tr=bless({$r1,$Ir,$W2,$q,$X2,$q,$Y2,$Rr,$Q,$Sr},$x2);$Ur=[$k3,$Hr,$Tr];$Vr=bless({$r1,$zr,$Q,$Ar,$v1,$Ur},$S2);$Wr={};$Xr=[];$Yr=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Zr=bless({$t,$Xr,$v,$q,$w,$Yr,$y,$z},$A);$cs=[];$ds=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$es=bless({$t,$cs,$v,$q,$w,$ds,$y,$z},$A);$fs=[];$gs=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$hs=bless({$t,$fs,$v,$q,$w,$gs,$y,$z},$A);$is={$T7,$es,$X7,$hs};$js=q#/io/transfer_io_interop.b#;$ks=bless({$r1,$Wr,$W2,$Zr,$X2,$q,$Y2,$is,$Q,$js},$x2);$ls={};$ms=q#pressure#;$ns=[];$os=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$ps=bless({$t,$ns,$v,$q,$w,$os,$y,$z},$A);$qs=q#read_limit_throughput#;$rs=[];$ss=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$ts=bless({$t,$rs,$v,$q,$w,$ss,$y,$z},$A);$us=q#throughput#;$vs=[];$ws=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$xs=bless({$t,$vs,$v,$q,$w,$ws,$y,$z},$A);$ys=q#write_limit_throughput#;$zs=[];$As=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Bs=bless({$t,$zs,$v,$q,$w,$As,$y,$z},$A);$Cs={$ms,$ps,$qs,$ts,$us,$xs,$ys,$Bs};$Ds=q#/io/transfer_io_measurement.b#;$Es=bless({$r1,$ls,$W2,$q,$X2,$q,$Y2,$Cs,$Q,$Ds},$x2);$Fs=[$Vr,$ks,$Es];$Gs=bless({$r1,$xr,$Q,$yr,$v1,$Fs},$Y1);$Hs=[];$Is=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Js=bless({$t,$Hs,$v,$q,$w,$Is,$y,$z},$A);$Ks=q#ni:/io/transfer.c#;$Ls={$Y1,1,$c2,1,$e2,1};$Ms=q#/io/transfer.c#;$Ns={$Y1,1,$c2,1,$e2,1,$S2,1};$Os=q#/semantic/task.c#;$Ps=[$kg];$Qs=bless({$r1,$Ns,$Q,$Os,$v1,$Ps},$J2);$Rs={};$Ss={};$Ts=q#/io/transfer.c_into.b#;$Us=bless({$r1,$Rs,$W2,$Js,$X2,$q,$Y2,$Ss,$Q,$Ts},$x2);$Vs=[$Qs,$Us];$Ws=bless({$r1,$Ls,$Q,$Ms,$v1,$Vs},$J2);$Xs=q#ni:/io/transfer.c_into.b#;$Ys=q#ni:/io/transfer_async#;$Zs={$Z1,1};$ct=q#/io/transfer_async#;$dt={};$et=q#dest_io#;$ft=[];$gt=q#shift->{'dest_io'}#;$ht=bless({$t,$ft,$v,$q,$w,$gt,$y,$z},$A);$it=q#id#;$jt=[];$kt=q#shift->{'id'}#;$lt=bless({$t,$jt,$v,$q,$w,$kt,$y,$z},$A);$mt=q#source_io#;$nt=[];$ot=q#shift->{'source_io'}#;$pt=bless({$t,$nt,$v,$q,$w,$ot,$y,$z},$A);$qt={$et,$ht,$it,$lt,$mt,$pt};$rt=q#/io/transfer_async_ro.b#;$st=bless({$r1,$dt,$W2,$q,$X2,$q,$Y2,$qt,$Q,$rt},$x2);$tt={};$ut=[];$vt=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$wt=bless({$t,$ut,$v,$q,$w,$vt,$y,$z},$A);$xt={$L4,$wt};$yt=q#/io/transfer_async_init.b#;$zt=bless({$r1,$tt,$W2,$q,$X2,$q,$Y2,$xt,$Q,$yt},$x2);$At={};$Bt=[];$Ct=q#ni('ni:/io/transfer_async')->track(shift)#;$Dt=bless({$t,$Bt,$v,$q,$w,$Ct,$y,$z},$A);$Et=[];$Ft=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Gt=bless({$t,$Et,$v,$q,$w,$Ft,$y,$z},$A);$Ht={};$It=q#/io/transfer_async_lifecycle.b#;$Jt=bless({$r1,$At,$W2,$Dt,$X2,$Gt,$Y2,$Ht,$Q,$It},$x2);$Kt={};$Lt=q#run#;$Mt=[];$Nt=q#shift#;$Ot=bless({$t,$Mt,$v,$q,$w,$Nt,$y,$z},$A);$Pt=q#run_async#;$Qt=[];$Rt=q#my $self = shift;
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

$self;#;$St=bless({$t,$Qt,$v,$q,$w,$Rt,$y,$z},$A);$Tt={$Lt,$Ot,$Pt,$St};$Ut=q#/io/transfer_async_run.b#;$Vt=bless({$r1,$Kt,$W2,$q,$X2,$q,$Y2,$Tt,$Q,$Ut},$x2);$Wt=[$Gs,$st,$zt,$Jt,$Vt];$Xt=q#tracked_transfers#;$Yt={};$Zt=q#transfer_id#;$cu=bless({$r1,$Zs,$Q,$ct,$v1,$Wt,$Xt,$Yt,$Zt,0},$c2);$du=[];$eu=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$fu=bless({$t,$du,$v,$q,$w,$eu,$y,$z},$A);$gu=q#ni:/io/transfer_async.c#;$hu={$c2,1};$iu=q#/io/transfer_async.c#;$ju={};$ku=q#new_id#;$lu=[];$mu=q#++shift->{transfer_id}#;$nu=bless({$t,$lu,$v,$q,$w,$mu,$y,$z},$A);$ou=q#track#;$pu=[];$qu=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$ru=bless({$t,$pu,$v,$q,$w,$qu,$y,$z},$A);$su=q#untrack#;$tu=[];$uu=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$vu=bless({$t,$tu,$v,$q,$w,$uu,$y,$z},$A);$wu={$ku,$nu,$ou,$ru,$su,$vu};$xu=q#/io/transfer_async.c_tracker.b#;$yu=bless({$r1,$ju,$W2,$fu,$X2,$q,$Y2,$wu,$Q,$xu},$x2);$zu=[$Ws,$yu];$Au=bless({$r1,$hu,$Q,$iu,$v1,$zu},$J2);$Bu=q#ni:/io/transfer_async.c_tracker.b#;$Cu=q#ni:/io/transfer_async_init.b#;$Du=q#ni:/io/transfer_async_lifecycle.b#;$Eu=q#ni:/io/transfer_async_ro.b#;$Fu=q#ni:/io/transfer_async_run.b#;$Gu=q#ni:/io/transfer_io_interop.b#;$Hu=q#ni:/io/transfer_io_measurement.b#;$Iu=q#ni:/io/transfer_sync#;$Ju={$d2,1};$Ku=q#/io/transfer_sync#;$Lu={};$Mu=[];$Nu=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Ou=bless({$t,$Mu,$v,$q,$w,$Nu,$y,$z},$A);$Pu={$L4,$Ou};$Qu=q#/io/transfer_sync_init.b#;$Ru=bless({$r1,$Lu,$W2,$q,$X2,$q,$Y2,$Pu,$Q,$Qu},$x2);$Su={};$Tu=[];$Uu=q#my $self = shift;
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
$self->success;#;$Vu=bless({$t,$Tu,$v,$q,$w,$Uu,$y,$z},$A);$Wu={$Lt,$Vu};$Xu=q#/io/transfer_sync_run.b#;$Yu=bless({$r1,$Su,$W2,$q,$X2,$q,$Y2,$Wu,$Q,$Xu},$x2);$Zu=[$Gs,$Ru,$Yu];$cv=bless({$r1,$Ju,$Q,$Ku,$v1,$Zu},$e2);$dv=q#ni:/io/transfer_sync.c#;$ev={$e2,1};$fv=q#/io/transfer_sync.c#;$gv=[$Ws];$hv=bless({$r1,$ev,$Q,$fv,$v1,$gv},$J2);$iv=q#ni:/io/transfer_sync_init.b#;$jv=q#ni:/io/transfer_sync_run.b#;$kv=q#ni:/lib#;$lv=q#lib#;$mv={$lv,1};$nv=[];$ov=bless({$r1,$mv,$Q,$c9,$v1,$nv},$L2);$pv=q#ni:/lib/accessor.b#;$qv=q#ni:/lib/behavior#;$rv=q#ni:/lib/behavior.c#;$sv=q#ni:/lib/branch#;$tv={$h2,1};$uv=q#/lib/branch#;$vv={};$wv=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$xv=bless({$w,$wv,$y,$z},$A);$yv={$L4,$xv};$zv=q#/lib/branch_init.b#;$Av=bless({$r1,$vv,$W2,$q,$X2,$q,$Y2,$yv,$Q,$zv},$x2);$Bv=[$Ie,$Td,$Nd,$Av,$Hf];$Cv=bless({$r1,$tv,$Q,$uv,$v1,$Bv},$i2);$Dv=q#ni:/lib/branch.b#;$Ev=q#ni:/lib/branch.c#;$Fv={$i2,1};$Gv=q#/lib/branch.c#;$Hv=[$og];$Iv=bless({$r1,$Fv,$Q,$Gv,$v1,$Hv},$J2);$Jv=q#ni:/lib/branch_init.b#;$Kv=q#ni:/lib/class_init.b#;$Lv=q#ni:/lib/dataslice#;$Mv={$j2,1};$Nv=q#/lib/dataslice#;$Ov={};$Pv=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Qv=bless({$w,$Pv,$y,$z},$A);$Rv={$L4,$Qv};$Sv=q#/lib/dataslice_init.b#;$Tv=bless({$r1,$Ov,$W2,$q,$X2,$q,$Y2,$Rv,$Q,$Sv},$x2);$Uv={};$Vv=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Wv=bless({$w,$Vv,$y,$z},$A);$Xv={$Id,$Wv};$Yv=q#/lib/dataslice_apply.b#;$Zv=bless({$r1,$Uv,$W2,$q,$X2,$q,$Y2,$Xv,$Q,$Yv},$x2);$cw=[$Ie,$Tv,$Zv];$dw=bless({$r1,$Mv,$Q,$Nv,$v1,$cw},$k2);$ew=q#ni:/lib/dataslice.c#;$fw={$k2,1};$gw=q#/lib/dataslice.c#;$hw=[$og];$iw=bless({$r1,$fw,$Q,$gw,$v1,$hw},$J2);$jw=q#ni:/lib/dataslice_apply.b#;$kw=q#ni:/lib/dataslice_init.b#;$lw=q#ni:/lib/definition.b#;$mw=q#ni:/lib/definition_def.b#;$nw=q#ni:/lib/definition_defdata.b#;$ow=q#ni:/lib/definition_init_with_defaults.b#;$pw=q#ni:/lib/doc#;$qw={$S,1};$rw={};$sw=q#shift; +{name => shift, doc => []}#;$tw=bless({$w,$sw,$y,$z},$A);$uw={$L4,$tw};$vw=q#/lib/doc_init.b#;$ww=bless({$r1,$rw,$W2,$q,$X2,$q,$Y2,$uw,$Q,$vw},$x2);$xw={};$yw=q#'ni.doc'#;$zw=bless({$w,$yw,$y,$z},$A);$Aw={$Vd,$zw};$Bw=q#/lib/doc_namespace.b#;$Cw=bless({$r1,$xw,$W2,$q,$X2,$q,$Y2,$Aw,$Q,$Bw},$x2);$Dw={};$Ew=q#(@{}#;$Fw=q#[map @$_, @{shift->{doc}}]#;$Gw=bless({$w,$Fw,$y,$z},$A);$Hw=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map ni::outdent($_), @_];
$self;#;$Iw=bless({$w,$Hw,$y,$z},$A);$Jw={$Ew,$Gw,$Ig,$Iw};$Kw=q#/lib/doc_define.b#;$Lw=bless({$r1,$Dw,$W2,$q,$X2,$q,$Y2,$Jw,$Q,$Kw},$x2);$Mw={};$Nw=q#shift->referent#;$Ow=bless({$w,$Nw,$y,$z},$A);$Pw=q#ni 'ni:' . shift->{name}#;$Qw=bless({$w,$Pw,$y,$z},$A);$Rw={$Eq,$Ow,$q1,$Qw};$Sw=q#/lib/doc_end.b#;$Tw=bless({$r1,$Mw,$W2,$q,$X2,$q,$Y2,$Rw,$Q,$Sw},$x2);$Uw={};$Vw=q#my $self = shift;
push @{$$self{doc}},
     [TODO => TODO($_)->referent($self->referent)] for @_;
$self;#;$Ww=bless({$w,$Vw,$y,$z},$A);$Xw={$p1,$Ww};$Yw=q#/lib/doc_TODO.b#;$Zw=bless({$r1,$Uw,$W2,$q,$X2,$q,$Y2,$Xw,$Q,$Yw},$x2);$cx={};$dx=q#my $self = shift;
push @{$$self{doc}},
     [eg => eg($_)->referent($self->referent)] for @_;
$self;#;$ex=bless({$w,$dx,$y,$z},$A);$fx={$g7,$ex};$gx=q#/lib/doc_eg.b#;$hx=bless({$r1,$cx,$W2,$q,$X2,$q,$Y2,$fx,$Q,$gx},$x2);$ix={};$jx=q#tests#;$kx=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/test_case', @$self;#;$lx=bless({$w,$kx,$y,$z},$A);$mx=q#todos#;$nx=q#my $self = shift;
map $_->referent($self->referent), grep ref($_) eq 'lib/todo', @$self;#;$ox=bless({$w,$nx,$y,$z},$A);$px={$jx,$lx,$mx,$ox};$qx=q#/lib/doc_process.b#;$rx=bless({$r1,$ix,$W2,$q,$X2,$q,$Y2,$px,$Q,$qx},$x2);$sx=[$k3,$Td,$ww,$Cw,$Lw,$Tw,$Zw,$hx,$rx];$tx=bless({$r1,$qw,$Q,$w9,$v1,$sx},$l2);$ux=q#ni:/lib/doc.c#;$vx={$l2,1};$wx=q#/lib/doc.c#;$xx={};$yx=q#defannotation#;$zx=q#my $class = shift;
$class->def("$$class{name}_$_.b",
  $_ => fn qq{
    my \\$self = shift;
    push \\@{\\$\\$self{doc}},
         [$_ => $_(\\$_)->referent(\\$self->referent)] for \\@_;
    \\$self;
  }) for @_;
$class;#;$Ax=bless({$w,$zx,$y,$z},$A);$Bx={$yx,$Ax};$Cx=q#/lib/doc.c_defannotation.b#;$Dx=bless({$r1,$xx,$W2,$q,$X2,$q,$Y2,$Bx,$Q,$Cx},$x2);$Ex=[$kg,$Dx];$Fx=bless({$r1,$vx,$Q,$wx,$v1,$Ex},$J2);$Gx=q#ni:/lib/doc.c_defannotation.b#;$Hx=q#ni:/lib/doc_TODO.b#;$Ix=q#ni:/lib/doc_define.b#;$Jx=q#ni:/lib/doc_eg.b#;$Kx=q#ni:/lib/doc_end.b#;$Lx=q#ni:/lib/doc_init.b#;$Mx=q#ni:/lib/doc_namespace.b#;$Nx=q#ni:/lib/doc_process.b#;$Ox=q#ni:/lib/documentable.b#;$Px=q#ni:/lib/fn#;$Qx={$A,1};$Rx=q#/lib/fn#;$Sx=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$Tx=bless({$w,$Sx,$y,$z},$A);$Ux=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
}#;$Vx=bless({$w,$Ux},$A);$Wx=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$Xx=bless({$w,$Wx},$A);$Yx=q#lib/fn::closure#;$Zx=q#lib/fn::compile#;$cy=q#lib/fn::instantiate#;$dy={};$ey=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$fy=bless({$w,$ey,$y,$z},$A);$gy=q#compile#;$hy={$v,$Tx,$gy,$Vx,$L4,$Xx};$iy=q#/lib/fn_init.b#;$jy=bless({$r1,$dy,$W2,$q,$X2,$fy,$Y2,$hy,$Q,$iy},$x2);$ky={};$ly=[];$my=q#shift->{'annotations'}#;$ny=bless({$t,$ly,$v,$q,$w,$my,$y,$z},$A);$oy=[];$py=q#shift->{'code'}#;$qy=bless({$t,$oy,$v,$q,$w,$py,$y,$z},$A);$ry=q#eval_number#;$sy=[];$ty=q#shift->{'eval_number'}#;$uy=bless({$t,$sy,$v,$q,$w,$ty,$y,$z},$A);$vy=q#fn#;$wy=[];$xy=q#shift->{'fn'}#;$yy=bless({$t,$wy,$v,$q,$w,$xy,$y,$z},$A);$zy={$t,$ny,$w,$qy,$ry,$uy,$vy,$yy};$Ay=q#/lib/fn_ro.b#;$By=bless({$r1,$ky,$W2,$q,$X2,$q,$Y2,$zy,$Q,$Ay},$x2);$Cy={};$Dy=[];$Ey=q#my $self = shift; "fn {$$self{code}}"#;$Fy=bless({$t,$Dy,$v,$q,$w,$Ey,$y,$z},$A);$Gy=[];$Hy=bless({$t,$Gy,$v,$q,$w,$nf,$y,$z},$A);$Iy={$ff,$Fy,$mf,$Hy};$Jy=q#/lib/fn_ops.b#;$Ky=bless({$r1,$Cy,$W2,$q,$X2,$q,$Y2,$Iy,$Q,$Jy},$x2);$Ly={};$My=q#serialize#;$Ny=[];$Oy=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Py=bless({$t,$Ny,$v,$q,$w,$Oy,$y,$z},$A);$Qy={$My,$Py};$Ry=q#/lib/fn_serialize.b#;$Sy=bless({$r1,$Ly,$W2,$q,$X2,$q,$Y2,$Qy,$Q,$Ry},$x2);$Ty=[$k3,$Qf,$jy,$By,$Ky,$Sy];$Uy=bless({$r1,$Qx,$Q,$Rx,$v1,$Ty},$m2);$Vy=[];$Wy=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Xy=bless({$t,$Vy,$v,$q,$w,$Wy,$y,$z},$A);$Yy=q#ni:/lib/fn.c#;$Zy={$m2,1};$cz=q#/lib/fn.c#;$dz={};$ez=q#resolve_evals#;$fz=[];$gz=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$hz=bless({$t,$fz,$v,$q,$w,$gz,$y,$z},$A);$iz={$ez,$hz};$jz=q#/lib/fn.c_resolve_eval.b#;$kz=bless({$r1,$dz,$W2,$Xy,$X2,$q,$Y2,$iz,$Q,$jz},$x2);$lz=[$kg,$kz];$mz=bless({$r1,$Zy,$Q,$cz,$v1,$lz},$J2);$nz=q#ni:/lib/fn.c_resolve_eval.b#;$oz=q#ni:/lib/fn_init.b#;$pz=q#ni:/lib/fn_ops.b#;$qz=q#ni:/lib/fn_ro.b#;$rz=q#ni:/lib/fn_serialize.b#;$sz=q#ni:/lib/future#;$tz={$n2,1};$uz={};$vz=[];$wz=bless({$t,$vz,$v,$q,$w,$Dr,$y,$z},$A);$xz=q#parents#;$yz=[];$zz=q#shift->{'parents'}#;$Az=bless({$t,$yz,$v,$q,$w,$zz,$y,$z},$A);$Bz={$r,$wz,$xz,$Az};$Cz=q#/lib/future_ro.b#;$Dz=bless({$r1,$uz,$W2,$q,$X2,$q,$Y2,$Bz,$Q,$Cz},$x2);$Ez={};$Fz=[];$Gz=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Hz=bless({$t,$Fz,$v,$q,$w,$Gz,$y,$z},$A);$Iz={$L4,$Hz};$Jz=q#/lib/future_init.b#;$Kz=bless({$r1,$Ez,$W2,$q,$X2,$q,$Y2,$Iz,$Q,$Jz},$x2);$Lz={};$Mz=q#decide#;$Nz=[];$Oz=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Pz=bless({$t,$Nz,$v,$q,$w,$Oz,$y,$z},$A);$Qz=q#decided#;$Rz=[];$Sz=q#shift->{outcome}#;$Tz=bless({$t,$Rz,$v,$q,$w,$Sz,$y,$z},$A);$Uz=q#listener#;$Vz=[];$Wz=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$Xz=bless({$t,$Vz,$v,$q,$w,$Wz,$y,$z},$A);$Yz=q#v#;$Zz=[];$cA=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$dA=bless({$t,$Zz,$v,$q,$w,$cA,$y,$z},$A);$eA={$Mz,$Pz,$Qz,$Tz,$Uz,$Xz,$Yz,$dA};$fA=q#/lib/future_state.b#;$gA=bless({$r1,$Lz,$W2,$q,$X2,$q,$Y2,$eA,$Q,$fA},$x2);$hA={};$iA=q#and#;$jA=[];$kA=q#my $self   = $_[0];
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
$child;#;$lA=bless({$t,$jA,$v,$q,$w,$kA,$y,$z},$A);$mA=q#flatmap#;$nA=[];$oA=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$pA=bless({$t,$nA,$v,$q,$w,$oA,$y,$z},$A);$qA=q#map#;$rA=[];$sA=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$tA=bless({$t,$rA,$v,$q,$w,$sA,$y,$z},$A);$uA=q#or#;$vA=[];$wA=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$xA=bless({$t,$vA,$v,$q,$w,$wA,$y,$z},$A);$yA={$iA,$lA,$mA,$pA,$qA,$tA,$uA,$xA};$zA=q#/lib/future_algebra.b#;$AA=bless({$r1,$hA,$W2,$q,$X2,$q,$Y2,$yA,$Q,$zA},$x2);$BA=[$k3,$Dz,$Kz,$gA,$AA];$CA=bless({$r1,$tz,$Q,$N9,$v1,$BA},$o2);$DA=q#ni:/lib/future.c#;$EA={$o2,1};$FA=q#/lib/future.c#;$GA=[$kg];$HA=bless({$r1,$EA,$Q,$FA,$v1,$GA},$J2);$IA=q#ni:/lib/future_algebra.b#;$JA=q#ni:/lib/future_init.b#;$KA=q#ni:/lib/future_ro.b#;$LA=q#ni:/lib/future_state.b#;$MA=q#ni:/lib/gensym_generator_compact.b#;$NA=q#ni:/lib/global_static_test.b#;$OA={};$PA=[];$QA=q#ni('ni:/lib/test_case')->new(shift)#;$RA=q#($)#;$SA=bless({$t,$PA,$v,$q,$w,$QA,$y,$RA},$A);$TA=q#now#;$UA=[];$VA=q#ni('ni:/lib/test_value')->new(shift)#;$WA=bless({$t,$UA,$v,$q,$w,$VA,$y,$RA},$A);$XA={$g7,$SA,$TA,$WA};$YA=q#/lib/global_static_test.b#;$ZA=bless({$r1,$OA,$W2,$q,$X2,$q,$Y2,$XA,$Q,$YA},$x2);$cB=q#ni:/lib/image#;$dB=q#ni:/lib/image.c#;$eB={$q2,1};$fB=q#/lib/image.c#;$gB=[$kg];$hB=bless({$r1,$eB,$Q,$fB,$v1,$gB},$J2);$iB=q#ni:/lib/image_init.b#;$jB=q#ni:/lib/image_quoting.b#;$kB=q#ni:/lib/instance.b#;$lB=q#ni:/lib/instantiable.b#;$mB=q#ni:/lib/json.b#;$nB={};$oB=q#json_decode#;$pB=[];$qB=q#local $_;
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
wantarray ? @$r : $$r[0];#;$rB=bless({$t,$pB,$v,$q,$w,$qB,$y,$RA},$A);$sB=q#json_encode#;$tB=[];$uB=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$vB=bless({$t,$tB,$v,$q,$w,$uB,$y,$RA},$A);$wB=q#json_encode_pretty#;$xB=[];$yB=q#local $_;
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

ni::json_encode($v);#;$zB=bless({$t,$xB,$v,$q,$w,$yB,$y,$z},$A);$AB=q#json_escape#;$BB=[];$CB=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$DB=bless({$t,$BB,$v,$q,$w,$CB,$y,$RA},$A);$EB=q#json_unescape#;$FB=[];$GB=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$HB=bless({$t,$FB,$v,$q,$w,$GB,$y,$RA},$A);$IB=q#json_unescape_one#;$JB=[];$KB=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$LB=bless({$t,$JB,$v,$q,$w,$KB,$y,$RA},$A);$MB={$oB,$rB,$sB,$vB,$wB,$zB,$AB,$DB,$EB,$HB,$IB,$LB};$NB=q#/lib/json.b#;$OB=bless({$r1,$nB,$W2,$q,$X2,$q,$Y2,$MB,$Q,$NB},$x2);$PB=q#ni#;$QB=q#ni:/lib/name_as_string.b#;$RB=q#ni:/lib/named.b#;$SB=q#ni:/lib/named_in_ni.b#;$TB=q#ni:/lib/namespaced.b#;$UB=q#ni:/lib/ni#;$VB={$r2,1};$WB={};$XB=q#extend#;$YB=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$ZB=bless({$w,$YB,$y,$z},$A);$cC=q#is_mutable#;$dC=q#$0 ne '-' && -w $0#;$eC=bless({$w,$dC,$y,$z},$A);$fC=q#modify#;$gC=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$hC=bless({$w,$gC,$y,$z},$A);$iC={$XB,$ZB,$cC,$eC,$fC,$hC};$jC=q#/lib/ni_self.b#;$kC=bless({$r1,$WB,$W2,$q,$X2,$q,$Y2,$iC,$Q,$jC},$x2);$lC={};$mC=q#my $self = shift;
my $metaclass = $self->resolve('ni:/object.c');
map $self->resolve("ni:/$_"),
    grep !$$metaclass{applied_to}->{$_},
    sort keys %{$self->resolve('ni:/object')->{applied_to}};#;$nC=bless({$w,$mC,$y,$z},$A);$oC=q#docs#;$pC=q#my $self = shift;
map $self->resolve($_), grep /^ni\\.doc:/, sort keys %{$$self{named}};#;$qC=bless({$w,$pC,$y,$z},$A);$rC=q#metaclasses#;$sC=q#my $self = shift;
map $self->resolve("ni:/$_"),
    sort keys %{$self->resolve('ni:/object.c')->{applied_to}};#;$tC=bless({$w,$sC,$y,$z},$A);$uC=q#undocumented#;$vC=q#my $self = shift;
my %docs = map +($_->referent => 1), $self->docs;
grep !$docs{$_}, $self->classes;#;$wC=bless({$w,$vC,$y,$z},$A);$xC=q#untested#;$yC=q#my $self = shift;
my %tests = map +($_->referent => 1), map $_->tests, $self->docs;
grep !$tests{$_}, $self->classes;#;$zC=bless({$w,$yC,$y,$z},$A);$AC={$K,$nC,$oC,$qC,$rC,$tC,$uC,$wC,$xC,$zC};$BC=q#/lib/ni_dev_introspection.b#;$CC=bless({$r1,$lC,$W2,$q,$X2,$q,$Y2,$AC,$Q,$BC},$x2);$DC={};$EC=q#--internal/+=#;$FC=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$GC=bless({$w,$FC,$y,$z},$A);$HC=q#--internal/dev-state#;$IC=q#my $self = shift;
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
    failed  => [
      map +{
        referent => $_->referent->name,
        assertions => [
          map $_->failed ? +{diff => $_->diff} : +{passed => 1},
              @{$_->assertions}
        ]
      }, @failed
    ]
  },

  todos => [map +{referent => $_->referent->name,
                  text     => join "\\n", @{$_->todo}},
            map $_->todos, $self->docs],
}), "\\n";
0;#;$JC=bless({$w,$IC,$y,$z},$A);$KC=q#--internal/eval#;$LC=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$MC=bless({$w,$LC,$y,$z},$A);$NC=q#--internal/image#;$OC=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$PC=bless({$w,$OC,$y,$z},$A);$QC=q#--internal/test#;$RC=q#local $| = 1;
my $self   = shift;
my $failed = 0;
my @docs   = $self->docs;
my @todos  = map $_->todos, @docs;

print "\\n" . scalar(@todos) . " TODO item(s)\\n" if @todos;
print "$_\\n\\n" for @todos;

my @tests = map $_->tests, @docs;
for (@tests) {
  $_->run;
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
!!$failed;#;$SC=bless({$w,$RC,$y,$z},$A);$TC=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$UC=bless({$w,$TC,$y,$z},$A);$VC={$EC,$GC,$HC,$JC,$KC,$MC,$NC,$PC,$QC,$SC,$Lt,$UC};$WC=q#/lib/ni_main.b#;$XC=bless({$r1,$DC,$W2,$q,$X2,$q,$Y2,$VC,$Q,$WC},$x2);$YC={};$ZC=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$cD=bless({$w,$ZC,$y,$z},$A);$dD=q#resolver_for#;$eD=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$fD=bless({$w,$eD,$y,$z},$A);$gD={$le,$cD,$dD,$fD};$hD=q#/lib/ni_resolver.b#;$iD=bless({$r1,$YC,$W2,$q,$X2,$q,$Y2,$gD,$Q,$hD},$x2);$jD={};$kD=q#exists#;$lD=q#exists $_[0]->{named}{$_[1]}#;$mD=bless({$w,$lD,$y,$z},$A);$nD=q#quoted#;$oD=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$pD=bless({$w,$oD,$y,$z},$A);$qD={$kD,$mD,$nD,$pD};$rD=q#/lib/ni_image.b#;$sD=bless({$r1,$jD,$W2,$q,$X2,$q,$Y2,$qD,$Q,$rD},$x2);$tD=[$k3,$kC,$CC,$XC,$iD,$sD];$uD=bless({$r1,$VB,$Q,$Jc,$v1,$tD},$s2);$vD=q#ni:/lib/ni.c#;$wD={$s2,1};$xD=q#/lib/ni.c#;$yD=[$kg];$zD=bless({$r1,$wD,$Q,$xD,$v1,$yD},$J2);$AD=q#ni:/lib/ni_dev_introspection.b#;$BD=q#ni:/lib/ni_image.b#;$CD=q#ni:/lib/ni_main.b#;$DD=q#ni:/lib/ni_resolver.b#;$ED=q#ni:/lib/ni_self.b#;$FD=q#ni:/lib/ni_static_util.b#;$GD={};$HD=q#abbrev#;$ID=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$JD=bless({$w,$ID,$y,$z},$A);$KD=q#dor#;$LD=q#defined $_[0] ? $_[0] : $_[1]#;$MD=bless({$w,$LD,$y,$z},$A);$ND=q#indent#;$OD=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$PD=bless({$w,$OD,$y,$z},$A);$QD=q#max#;$RD=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$SD=bless({$w,$RD,$y,$z},$A);$TD=q#maxstr#;$UD=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$VD=bless({$w,$UD,$y,$z},$A);$WD=q#mean#;$XD=q#sum(@_) / (@_ || 1)#;$YD=bless({$w,$XD,$y,$z},$A);$ZD=q#min#;$cE=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$dE=bless({$w,$cE,$y,$z},$A);$eE=q#minstr#;$fE=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$gE=bless({$w,$fE,$y,$z},$A);$hE=q#outdent#;$iE=q#my $x = shift;
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
join "\\n", @lines;#;$jE=bless({$w,$iE,$y,$z},$A);$kE=q#sgr#;$lE=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$mE=bless({$w,$lE,$y,$z},$A);$nE=q#sr#;$oE=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$pE=bless({$w,$oE,$y,$z},$A);$qE=q#sum#;$rE=q#local $_; my $x = 0; $x += $_ for @_; $x#;$sE=bless({$w,$rE,$y,$z},$A);$tE=q#swap#;$uE=q#@_[0, 1] = @_[1, 0]#;$vE=bless({$w,$uE,$y,$z},$A);$wE={$HD,$JD,$KD,$MD,$ND,$PD,$QD,$SD,$TD,$VD,$WD,$YD,$ZD,$dE,$eE,$gE,$hE,$jE,$kE,$mE,$nE,$pE,$qE,$sE,$tE,$vE};$xE=q#/lib/ni_static_util.b#;$yE=bless({$r1,$GD,$W2,$q,$X2,$q,$Y2,$wE,$Q,$xE},$x2);$zE=q#ni:/lib/object_metadata#;$AE={$t2,1,$C,1,$H,1};$BE=q#/lib/object_metadata#;$CE={};$DE=q#if (@_ == 2) {
  $_[0]->{'referent'} = $_[1];
  return $_[0];
} else {
  return shift->{'referent'};
}#;$EE=bless({$w,$DE,$y,$z},$A);$FE={$q1,$EE};$GE=q#/lib/object_metadata_rw.b#;$HE=bless({$r1,$CE,$W2,$q,$X2,$q,$Y2,$FE,$Q,$GE},$x2);$IE=[$k3,$HE];$JE=bless({$r1,$AE,$Q,$BE,$v1,$IE},$u2);$KE=q#ni:/lib/object_metadata.c#;$LE={$u2,1,$F2,1,$I2,1};$ME=q#/lib/object_metadata.c#;$NE=[$kg];$OE=bless({$r1,$LE,$Q,$ME,$v1,$NE},$J2);$PE=q#ni:/lib/object_metadata_rw.b#;$QE=q#ni:/lib/perlbranch.b#;$RE=q#ni:/lib/quote_circular_addressed.b#;$SE=q#ni:/lib/quote_code_fail.b#;$TE=q#ni:/lib/quote_gensym_identity.b#;$UE=q#ni:/lib/quote_objects.b#;$VE=q#ni:/lib/quote_simple#;$WE={$v2,1};$XE={};$YE=[];$ZE=q#+{}#;$cF=bless({$t,$YE,$v,$q,$w,$ZE,$y,$z},$A);$dF={$L4,$cF};$eF=q#/lib/quote_simple_init.b#;$fF=bless({$r1,$XE,$W2,$q,$X2,$q,$Y2,$dF,$Q,$eF},$x2);$gF={};$hF=[];$iF=bless({$t,$hF,$v,$q,$w,0,$y,$z},$A);$jF=[];$kF=q#shift->quote_value(shift)#;$lF=bless({$t,$jF,$v,$q,$w,$kF,$y,$z},$A);$mF={$Kb,$iF,$gc,$lF};$nF=q#/lib/quote_simple_quote.b#;$oF=bless({$r1,$gF,$W2,$q,$X2,$q,$Y2,$mF,$Q,$nF},$x2);$pF=[$k3,$fF,$oF,$Ka,$kb,$Ab];$qF=bless({$r1,$WE,$Q,$Uc,$v1,$pF},$w2);$rF=q#ni:/lib/quote_simple.c#;$sF={$w2,1};$tF=q#/lib/quote_simple.c#;$uF=[$kg];$vF=bless({$r1,$sF,$Q,$tF,$v1,$uF},$J2);$wF=q#ni:/lib/quote_simple_init.b#;$xF=q#ni:/lib/quote_simple_quote.b#;$yF=q#ni:/lib/quote_values.b#;$zF=q#ni:/lib/ref_eq.b#;$AF=q#ni:/lib/resolver.b#;$BF=q#ni:/lib/slice#;$CF={$x2,1};$DF=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$EF=bless({$w,$DF},$A);$FF=q#local $_;
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
$self;#;$GF=bless({$w,$FF},$A);$HF=q#lib/slice::apply#;$IF=q#lib/slice::apply_#;$JF={};$KF=q#apply_#;$LF={$Id,$EF,$KF,$GF};$MF=q#/lib/slice.b#;$NF=bless({$r1,$JF,$Y2,$LF,$Q,$MF},$x2);$OF={};$PF=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$QF=bless({$w,$PF,$y,$z},$A);$RF={$L4,$QF};$SF=q#/lib/slice_init.b#;$TF=bless({$r1,$OF,$Y2,$RF,$Q,$SF},$x2);$UF={};$VF=[];$WF=q#local $_;
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
$g;#;$XF=bless({$t,$VF,$v,$q,$w,$WF,$y,$z},$A);$YF={$My,$XF};$ZF=q#/lib/slice_serialize.b#;$cG=bless({$r1,$UF,$W2,$q,$X2,$q,$Y2,$YF,$Q,$ZF},$x2);$dG=[$Ie,$Td,$NF,$TF,$cG];$eG=bless({$r1,$CF,$Q,$rd,$v1,$dG},$y2);$fG=q#ni:/lib/slice.b#;$gG=q#ni:/lib/slice.c#;$hG={$y2,1};$iG=q#/lib/slice.c#;$jG=[$og];$kG=bless({$r1,$hG,$Q,$iG,$v1,$jG},$J2);$lG=q#ni:/lib/slice_init.b#;$mG=q#ni:/lib/slice_serialize.b#;$nG=q#ni:/lib/static_fn.b#;$oG={};$pG=q#fc#;$qG=[];$rG=q#ni('ni:/lib/fn')->new(pop)->closure(@_)#;$sG=bless({$t,$qG,$v,$q,$w,$rG,$y,$z},$A);$tG=q#fk#;$uG=[];$vG=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$wG=bless({$t,$uG,$v,$q,$w,$vG,$y,$RA},$A);$xG=[];$yG=q#ni('ni:/lib/fn')->new(@_)#;$zG=bless({$t,$xG,$v,$q,$w,$yG,$y,$RA},$A);$AG=q#fp#;$BG=[];$CG=q#($$)#;$DG=bless({$t,$BG,$v,$q,$w,$yG,$y,$CG},$A);$EG={$pG,$sG,$tG,$wG,$vy,$zG,$AG,$DG};$FG=q#/lib/static_fn.b#;$GG=bless({$r1,$oG,$W2,$q,$X2,$q,$Y2,$EG,$Q,$FG},$x2);$HG=q#ni:/lib/subclass.b#;$IG=q#ni:/lib/tag#;$JG={$z2,1};$KG=q#/lib/tag#;$LG={};$MG=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$NG=bless({$w,$MG,$y,$z},$A);$OG={$Id,$NG};$PG=q#/lib/tag.b#;$QG=bless({$r1,$LG,$W2,$q,$X2,$q,$Y2,$OG,$Q,$PG},$x2);$RG={};$SG=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$TG=bless({$w,$SG,$y,$z},$A);$UG={$L4,$TG};$VG=q#/lib/tag_init.b#;$WG=bless({$r1,$RG,$W2,$q,$X2,$q,$Y2,$UG,$Q,$VG},$x2);$XG=[$Ie,$Td,$QG,$WG];$YG=bless({$r1,$JG,$Q,$KG,$v1,$XG},$A2);$ZG=q#ni:/lib/tag.b#;$cH=q#ni:/lib/tag.c#;$dH={$A2,1};$eH=q#/lib/tag.c#;$fH=[$og];$gH=bless({$r1,$dH,$Q,$eH,$v1,$fH},$J2);$hH=q#ni:/lib/tag_init.b#;$iH=q#ni:/lib/test_assert_eq#;$jH={$B2,1};$kH=q#/lib/test_assert_eq#;$lH={$B2,1,$D2,1};$mH=q#/lib/test_assertion#;$nH={};$oH=q#commit#;$pH=[];$qH=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$rH=bless({$t,$pH,$v,$q,$w,$qH,$y,$z},$A);$sH={$oH,$rH};$tH=q#/lib/test_assertion_commit.b#;$uH=bless({$r1,$nH,$W2,$q,$X2,$q,$Y2,$sH,$Q,$tH},$x2);$vH=[$k3,$uH];$wH=bless({$r1,$lH,$Q,$mH,$v1,$vH},$E2);$xH={};$yH=q#diff#;$zH=[];$AH=q#shift->{'diff'}#;$BH=bless({$t,$zH,$v,$q,$w,$AH,$y,$z},$A);$CH={$yH,$BH};$DH=q#/lib/test_assert_eq_ro.b#;$EH=bless({$r1,$xH,$W2,$q,$X2,$q,$Y2,$CH,$Q,$DH},$x2);$FH={};$GH=[];$HH=q#my ($class, $diff) = @_;
+{diff => $diff};#;$IH=bless({$t,$GH,$v,$q,$w,$HH,$y,$z},$A);$JH={$L4,$IH};$KH=q#/lib/test_assert_eq_init.b#;$LH=bless({$r1,$FH,$W2,$q,$X2,$q,$Y2,$JH,$Q,$KH},$x2);$MH={};$NH=[];$OH=q#my $self = shift;
$self->failed
  ? "FAIL\\n" . ni::indent(ni::json_encode_pretty($$self{diff}), 2)
  : "PASS";#;$PH=bless({$t,$NH,$v,$q,$w,$OH,$y,$z},$A);$QH=q#failed#;$RH=[];$SH=q#defined shift->{diff}#;$TH=bless({$t,$RH,$v,$q,$w,$SH,$y,$z},$A);$UH=q#result#;$VH=[];$WH=bless({$t,$VH,$v,$q,$w,$Nt,$y,$z},$A);$XH={$ff,$PH,$QH,$TH,$UH,$WH};$YH=q#/lib/test_assert_eq_result.b#;$ZH=bless({$r1,$MH,$W2,$q,$X2,$q,$Y2,$XH,$Q,$YH},$x2);$cI=[$wH,$EH,$LH,$ZH];$dI=bless({$r1,$jH,$Q,$kH,$v1,$cI},$C2);$eI=q#ni:/lib/test_assert_eq.c#;$fI={$C2,1};$gI=q#/lib/test_assert_eq.c#;$hI={$C2,1,$E2,1};$iI=q#/lib/test_assertion.c#;$jI=[$kg];$kI=bless({$r1,$hI,$Q,$iI,$v1,$jI},$J2);$lI=[$kI];$mI=bless({$r1,$fI,$Q,$gI,$v1,$lI},$J2);$nI=q#ni:/lib/test_assert_eq_init.b#;$oI=q#ni:/lib/test_assert_eq_result.b#;$pI=q#ni:/lib/test_assert_eq_ro.b#;$qI=q#ni:/lib/test_assertion#;$rI=q#ni:/lib/test_assertion.c#;$sI=q#ni:/lib/test_assertion_commit.b#;$tI=q#ni:/lib/test_case#;$uI={$C,1};$vI=q#/lib/test_case#;$wI=q#running_test#;$xI={};$yI=[];$zI=q#shift->{'assertions'}#;$AI=bless({$t,$yI,$v,$q,$w,$zI,$y,$z},$A);$BI=[];$CI=q#shift->{'test'}#;$DI=bless({$t,$BI,$v,$q,$w,$CI,$y,$z},$A);$EI={$n,$AI,$s,$DI};$FI=q#/lib/test_case_ro.b#;$GI=bless({$r1,$xI,$W2,$q,$X2,$q,$Y2,$EI,$Q,$FI},$x2);$HI={};$II=[];$JI=q#if (@_ == 2) {
  $_[0]->{'error'} = $_[1];
  return $_[0];
} else {
  return shift->{'error'};
}#;$KI=bless({$t,$II,$v,$q,$w,$JI,$y,$z},$A);$LI={$p,$KI};$MI=q#/lib/test_case_rw.b#;$NI=bless({$r1,$HI,$W2,$q,$X2,$q,$Y2,$LI,$Q,$MI},$x2);$OI={};$PI=[];$QI=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$RI=bless({$t,$PI,$v,$q,$w,$QI,$y,$z},$A);$SI={$L4,$RI};$TI=q#/lib/test_case_init.b#;$UI=bless({$r1,$OI,$W2,$q,$X2,$q,$Y2,$SI,$Q,$TI},$x2);$VI={};$WI=[];$XI=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$YI=bless({$t,$WI,$v,$q,$w,$XI,$y,$z},$A);$ZI=[];$cJ=q#!shift->{outcome}->[0]#;$dJ=bless({$t,$ZI,$v,$q,$w,$cJ,$y,$z},$A);$eJ={$ff,$YI,$QH,$dJ};$fJ=q#/lib/test_case_metrics.b#;$gJ=bless({$r1,$VI,$W2,$q,$X2,$q,$Y2,$eJ,$Q,$fJ},$x2);$hJ={};$iJ=q#done#;$jJ=[];$kJ=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$lJ=bless({$t,$jJ,$v,$q,$w,$kJ,$y,$z},$A);$mJ=[];$nJ=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$oJ=bless({$t,$mJ,$v,$q,$w,$nJ,$y,$z},$A);$pJ={$iJ,$lJ,$Lt,$oJ};$qJ=q#/lib/test_case_run.b#;$rJ=bless({$r1,$hJ,$W2,$q,$X2,$q,$Y2,$pJ,$Q,$qJ},$x2);$sJ=[$JE,$GI,$NI,$UI,$gJ,$rJ];$tJ=bless({$r1,$uI,$Q,$vI,$wI,$q,$v1,$sJ},$F2);$uJ=[];$vJ=q#shift->{running_test} = undef#;$wJ=bless({$t,$uJ,$v,$q,$w,$vJ,$y,$z},$A);$xJ=q#ni:/lib/test_case.c#;$yJ={$F2,1};$zJ=q#/lib/test_case.c#;$AJ={};$BJ=[];$CJ=q#shift->{'running_test'}#;$DJ=bless({$t,$BJ,$v,$q,$w,$CJ,$y,$z},$A);$EJ={$wI,$DJ};$FJ=q#/lib/test_case.c_test_ro.b#;$GJ=bless({$r1,$AJ,$W2,$q,$X2,$q,$Y2,$EJ,$Q,$FJ},$x2);$HJ={};$IJ=q#with_test#;$JJ=[];$KJ=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
my %names = %{$ni::self->{named}};
eval {&$f};
%{$ni::self->{named}} = %names;
$test->error($@) if $@;
$test->done;#;$LJ=bless({$t,$JJ,$v,$q,$w,$KJ,$y,$z},$A);$MJ={$IJ,$LJ};$NJ=q#/lib/test_case.c_test.b#;$OJ=bless({$r1,$HJ,$W2,$wJ,$X2,$q,$Y2,$MJ,$Q,$NJ},$x2);$PJ=[$OE,$GJ,$OJ];$QJ=bless({$r1,$yJ,$Q,$zJ,$v1,$PJ},$J2);$RJ=q#ni:/lib/test_case.c_test.b#;$SJ=q#ni:/lib/test_case.c_test_ro.b#;$TJ=q#ni:/lib/test_case_init.b#;$UJ=q#ni:/lib/test_case_metrics.b#;$VJ=q#ni:/lib/test_case_ro.b#;$WJ=q#ni:/lib/test_case_run.b#;$XJ=q#ni:/lib/test_case_rw.b#;$YJ=q#ni:/lib/test_value#;$ZJ={$G2,1};$cK=q#/lib/test_value#;$dK={};$eK=[];$fK=q#\\$_[1]#;$gK=bless({$t,$eK,$v,$q,$w,$fK,$y,$z},$A);$hK={$L4,$gK};$iK=q#/lib/test_value_init.b#;$jK=bless({$r1,$dK,$W2,$q,$X2,$q,$Y2,$hK,$Q,$iK},$x2);$kK={};$lK=q#(==#;$mK=[];$nK=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$oK=bless({$t,$mK,$v,$q,$w,$nK,$y,$z},$A);$pK=q#detailed_scalar_diff#;$qK=[];$rK=q#local $_;
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
[@diff];#;$sK=bless({$t,$qK,$v,$q,$w,$rK,$y,$z},$A);$tK=[];$uK=q#my ($self, $rhs) = @_;
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
return undef;#;$vK=bless({$t,$tK,$v,$q,$w,$uK,$y,$z},$A);$wK={$lK,$oK,$pK,$sK,$yH,$vK};$xK=q#/lib/test_value_eq.b#;$yK=bless({$r1,$kK,$W2,$q,$X2,$q,$Y2,$wK,$Q,$xK},$x2);$zK={};$AK=[];$BK=q#ni::json_encode ${$_[0]}#;$CK=bless({$t,$AK,$v,$q,$w,$BK,$y,$z},$A);$DK={$ff,$CK};$EK=q#/lib/test_value_str.b#;$FK=bless({$r1,$zK,$W2,$q,$X2,$q,$Y2,$DK,$Q,$EK},$x2);$GK=[$k3,$jK,$yK,$FK];$HK=bless({$r1,$ZJ,$Q,$cK,$v1,$GK},$H2);$IK=q#ni:/lib/test_value.c#;$JK={$H2,1};$KK=q#/lib/test_value.c#;$LK=[$kg];$MK=bless({$r1,$JK,$Q,$KK,$v1,$LK},$J2);$NK=q#ni:/lib/test_value_eq.b#;$OK=q#ni:/lib/test_value_init.b#;$PK=q#ni:/lib/test_value_str.b#;$QK=q#ni:/lib/todo#;$RK={$H,1};$SK=q#/lib/todo#;$TK={};$UK=q#shift->{'todo'}#;$VK=bless({$w,$UK,$y,$z},$A);$WK={$E,$VK};$XK=q#/lib/todo_ro.b#;$YK=bless({$r1,$TK,$W2,$q,$X2,$q,$Y2,$WK,$Q,$XK},$x2);$ZK={};$cL=q#my $class = shift;
+{todo => [map ni::outdent($_), @_]};#;$dL=bless({$w,$cL,$y,$z},$A);$eL={$L4,$dL};$fL=q#/lib/todo_init.b#;$gL=bless({$r1,$ZK,$W2,$q,$X2,$q,$Y2,$eL,$Q,$fL},$x2);$hL={};$iL=q#my $self = shift;
my $referent = $$self{referent} || '(anonymous)';
"TODO $referent\\n" . ni::indent(join("\\n- ", @{$$self{todo}}), 2);#;$jL=bless({$w,$iL,$y,$z},$A);$kL={$ff,$jL};$lL=q#/lib/todo_str.b#;$mL=bless({$r1,$hL,$W2,$q,$X2,$q,$Y2,$kL,$Q,$lL},$x2);$nL=[$JE,$YK,$gL,$mL];$oL=bless({$r1,$RK,$Q,$SK,$v1,$nL},$I2);$pL=q#ni:/lib/todo.c#;$qL={$I2,1};$rL=q#/lib/todo.c#;$sL=[$OE];$tL=bless({$r1,$qL,$Q,$rL,$v1,$sL},$J2);$uL=q#ni:/lib/todo_ctor.b#;$vL={};$wL=q#ni('ni:/lib/todo')->new(@_)#;$xL=bless({$w,$wL,$y,$z},$A);$yL={$p1,$xL};$zL=q#/lib/todo_ctor.b#;$AL=bless({$r1,$vL,$W2,$q,$X2,$q,$Y2,$yL,$Q,$zL},$x2);$BL=q#ni:/lib/todo_init.b#;$CL=q#ni:/lib/todo_ro.b#;$DL=q#ni:/lib/todo_str.b#;$EL=q#ni:/metaclass#;$FL={$J2,1};$GL=q#/metaclass#;$HL=[$se,$Qf,$ye,$Jf];$IL=bless({$r1,$FL,$Q,$GL,$v1,$HL},$K2);$JL=q#ni:/metaclass.c#;$KL={$K2,1};$LL=q#/metaclass.c#;$ML=[$Zf];$NL=bless({$r1,$KL,$Q,$LL,$v1,$ML},$J2);$OL=q#ni:/module#;$PL=q#ni:/module.c#;$QL=q#ni:/object#;$RL=q#ni:/object.c#;$SL=q#ni:/semantic#;$TL=q#semantic#;$UL={$TL,1};$VL=[];$WL=bless({$r1,$UL,$Q,$xd,$v1,$VL},$L2);$XL=q#ni:/semantic/dimension#;$YL={$P2,1};$ZL=q#/semantic/dimension#;$cM=[$Zf];$dM=bless({$r1,$YL,$Q,$ZL,$v1,$cM},$Q2);$eM=q#ni:/semantic/dimension.c#;$fM={$Q2,1};$gM=q#/semantic/dimension.c#;$hM=[$sg];$iM=bless({$r1,$fM,$Q,$gM,$v1,$hM},$J2);$jM=q#ni:/semantic/task#;$kM=q#ni:/semantic/task.c#;$lM=q#ni:/semantic/task_outcome.b#;$mM=q#ni:/semantic/task_ro.b#;$nM=q#ni:main#;$oM={$tp,1};$pM=[$AL,$GG,$ZA,$sp];$qM=bless({$r1,$oM,$Q,$tp,$v1,$pM},$L2);$rM=q#ni:ni#;$sM={$PB,1};$tM={$PB,1};$uM=q#json_escapes#;$vM=q##;$wM=q#b#;$xM=q#	#;$yM=q#t#;$zM=q#
#;$AM=q#n#;$BM=q##;$CM=q#"#;$DM=q#/#;$EM=q#\\#;$FM={$vM,$wM,$xM,$yM,$zM,$AM,$BM,$In,$CM,$CM,$DM,$DM,$EM,$EM};$GM=q#json_unescapes#;$HM={$CM,$CM,$DM,$DM,$EM,$EM,$wM,$vM,$AM,$zM,$In,$BM,$yM,$xM};$IM={$uM,$FM,$GM,$HM};$JM=q#/lib/json_data.b#;$KM=bless({$r1,$tM,$Aq,$IM,$Q,$JM},$j2);$LM=[$yE,$KM,$OB];$MM=bless({$r1,$sM,$Q,$PB,$v1,$LM},$L2);$NM={$d,$T,$W,$e1,$f1,$k1,$l1,$l5,$m5,$r5,$s5,$E5,$F5,$R5,$S5,$g6,$h6,$t6,$u6,$P6,$Q6,$V6,$W6,$e7,$f7,$U8,$V8,$d9,$e9,$x9,$y9,$O9,$P9,$Cc,$Dc,$Kc,$Lc,$Vc,$Wc,$sd,$td,$yd,$zd,$Zf,$cg,$sg,$tg,$xg,$yg,$Qg,$Rg,$Vg,$Wg,$Gg,$Xg,$Og,$Yg,$f5,$Zg,$rh,$sh,$d5,$th,$wh,$xh,$Zh,$ci,$gi,$hi,$Fh,$ii,$Xh,$ji,$Ai,$Bi,$Fi,$Gi,$ri,$Hi,$yi,$Ii,$qk,$rk,$vk,$wk,$Wj,$xk,$ok,$yk,$Yi,$zk,$Oj,$Ak,$uj,$Bk,$Ri,$Ck,$Ul,$Yl,$ym,$zm,$wm,$Am,$sl,$Bm,$Dl,$Cm,$Sk,$Dm,$Rl,$Em,$Lk,$Fm,$cl,$Gm,$co,$do,$ho,$io,$Wm,$jo,$vn,$ko,$fn,$lo,$Yn,$mo,$Om,$no,$Dn,$oo,$Ho,$Io,$Mo,$No,$Fo,$Oo,$wo,$Po,$sp,$up,$Sp,$Tp,$Xp,$Yp,$Dp,$Zp,$Qp,$cq,$J4,$dq,$ph,$eq,$nh,$fq,$N3,$gq,$V3,$hq,$j4,$iq,$t3,$jq,$H4,$kq,$v4,$lq,$B8,$mq,$qq,$rq,$z8,$sq,$F7,$tq,$f8,$uq,$v7,$vq,$R7,$wq,$nr,$or,$sr,$tr,$Vq,$ur,$lr,$vr,$Oq,$wr,$Gs,$Ks,$Ws,$Xs,$Us,$Ys,$cu,$gu,$Au,$Bu,$yu,$Cu,$zt,$Du,$Jt,$Eu,$st,$Fu,$Vt,$Gu,$ks,$Hu,$Es,$Iu,$cv,$dv,$hv,$iv,$Ru,$jv,$Yu,$kv,$ov,$pv,$df,$qv,$Ie,$rv,$og,$sv,$Cv,$Dv,$Nd,$Ev,$Iv,$Jv,$Av,$Kv,$ye,$Lv,$dw,$ew,$iw,$jw,$Zv,$kw,$Tv,$lw,$Hf,$mw,$Re,$nw,$yf,$ow,$Ff,$pw,$tx,$ux,$Fx,$Gx,$Dx,$Hx,$Zw,$Ix,$Lw,$Jx,$hx,$Kx,$Tw,$Lx,$ww,$Mx,$Cw,$Nx,$rx,$Ox,$Ge,$Px,$Uy,$Yy,$mz,$nz,$kz,$oz,$jy,$pz,$Ky,$qz,$By,$rz,$Sy,$sz,$CA,$DA,$HA,$IA,$AA,$JA,$Kz,$KA,$Dz,$LA,$gA,$MA,$uc,$NA,$ZA,$cB,$wc,$dB,$hB,$iB,$ea,$jB,$Ca,$kB,$i3,$lB,$Qf,$mB,$OB,$QB,$kf,$RB,$Td,$SB,$ce,$TB,$je,$UB,$uD,$vD,$zD,$AD,$CC,$BD,$sD,$CD,$XC,$DD,$iD,$ED,$kC,$FD,$yE,$zE,$JE,$KE,$OE,$PE,$HE,$QE,$se,$RE,$Qb,$SE,$Ka,$TE,$mc,$UE,$Ab,$VE,$qF,$rF,$vF,$wF,$fF,$xF,$oF,$yF,$kb,$zF,$rf,$AF,$qe,$BF,$eG,$fG,$NF,$gG,$kG,$lG,$TF,$mG,$cG,$nG,$GG,$HG,$Xf,$IG,$YG,$ZG,$QG,$cH,$gH,$hH,$WG,$iH,$dI,$eI,$mI,$nI,$LH,$oI,$ZH,$pI,$EH,$qI,$wH,$rI,$kI,$sI,$uH,$tI,$tJ,$xJ,$QJ,$RJ,$OJ,$SJ,$GJ,$TJ,$UI,$UJ,$gJ,$VJ,$GI,$WJ,$rJ,$XJ,$NI,$YJ,$HK,$IK,$MK,$NK,$yK,$OK,$jK,$PK,$FK,$QK,$oL,$pL,$tL,$uL,$AL,$BL,$gL,$CL,$YK,$DL,$mL,$EL,$IL,$JL,$NL,$OL,$Jf,$PL,$qg,$QL,$k3,$RL,$kg,$SL,$WL,$XL,$dM,$eM,$iM,$jM,$Vr,$kM,$Qs,$lM,$Tr,$mM,$Hr,$nM,$qM,$rM,$MM};$OM=q#resolvers#;$PM=[];$QM=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$RM=bless({$t,$PM,$v,$q,$w,$QM,$y,$z},$A);$SM=q#file#;$TM=[];$UM=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$VM=bless({$t,$TM,$v,$q,$w,$UM,$y,$z},$A);$WM=q#null#;$XM=[];$YM=q#ni('ni:/io/null')->new#;$ZM=bless({$t,$XM,$v,$q,$w,$YM,$y,$z},$A);$cN=q#sh#;$dN=[];$eN=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$fN=bless({$t,$dN,$v,$q,$w,$eN,$y,$z},$A);$gN=q#str#;$hN=[];$iN=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$jN=bless({$t,$hN,$v,$q,$w,$iN,$y,$z},$A);$kN={$h8,$RM,$SM,$VM,$WM,$ZM,$cN,$fN,$gN,$jN};$lN=bless({$c,$NM,$OM,$kN},$r2);*$IF=\&$GF;*$HF=\&$EF;*$cy=\&$Xx;*$Zx=\&$Vx;*$Yx=\&$Tx;$i3->apply_($I1);$i3->apply_($J1);$i3->apply_($K1);$i3->apply_($L1);$i3->apply_($s1);$i3->apply_($M1);$i3->apply_($w1);$i3->apply_($N1);$i3->apply_($x1);$i3->apply_($O1);$i3->apply_($y1);$i3->apply_($P1);$i3->apply_($z1);$i3->apply_($Q1);$i3->apply_($A1);$i3->apply_($R1);$i3->apply_($B1);$i3->apply_($S1);$i3->apply_($C1);$i3->apply_($T1);$i3->apply_($D1);$i3->apply_($U1);$i3->apply_($E1);$i3->apply_($V1);$i3->apply_($F1);$i3->apply_($W1);$i3->apply_($X1);$i3->apply_($Y1);$i3->apply_($Z1);$i3->apply_($c2);$i3->apply_($d2);$i3->apply_($e2);$i3->apply_($f2);$i3->apply_($g2);$i3->apply_($h2);$i3->apply_($i2);$i3->apply_($j2);$i3->apply_($k2);$i3->apply_($S);$i3->apply_($l2);$i3->apply_($A);$i3->apply_($m2);$i3->apply_($n2);$i3->apply_($o2);$i3->apply_($p2);$i3->apply_($q2);$i3->apply_($r2);$i3->apply_($s2);$i3->apply_($t2);$i3->apply_($u2);$i3->apply_($v2);$i3->apply_($w2);$i3->apply_($x2);$i3->apply_($y2);$i3->apply_($z2);$i3->apply_($A2);$i3->apply_($B2);$i3->apply_($C2);$i3->apply_($D2);$i3->apply_($E2);$i3->apply_($C);$i3->apply_($F2);$i3->apply_($G2);$i3->apply_($H2);$i3->apply_($H);$i3->apply_($I2);$i3->apply_($J2);$i3->apply_($K2);$i3->apply_($L2);$i3->apply_($M2);$i3->apply_($N2);$i3->apply_($O2);$i3->apply_($P2);$i3->apply_($Q2);$i3->apply_($R2);$i3->apply_($S2);$t3->apply_($s1);$t3->apply_($w1);$t3->apply_($x1);$t3->apply_($y1);$t3->apply_($z1);$t3->apply_($A1);$t3->apply_($B1);$t3->apply_($C1);$t3->apply_($D1);$t3->apply_($E1);$t3->apply_($F1);$N3->apply_($s1);$N3->apply_($w1);$N3->apply_($x1);$N3->apply_($y1);$N3->apply_($z1);$N3->apply_($A1);$N3->apply_($B1);$N3->apply_($C1);$N3->apply_($D1);$N3->apply_($E1);$N3->apply_($F1);$V3->apply_($s1);$V3->apply_($w1);$V3->apply_($x1);$V3->apply_($y1);$V3->apply_($z1);$V3->apply_($A1);$V3->apply_($B1);$V3->apply_($C1);$V3->apply_($D1);$V3->apply_($E1);$V3->apply_($F1);$j4->apply_($s1);$j4->apply_($w1);$j4->apply_($x1);$j4->apply_($y1);$j4->apply_($z1);$j4->apply_($A1);$j4->apply_($B1);$j4->apply_($C1);$j4->apply_($D1);$j4->apply_($E1);$j4->apply_($F1);$v4->apply_($s1);$v4->apply_($w1);$v4->apply_($x1);$v4->apply_($y1);$v4->apply_($z1);$v4->apply_($A1);$v4->apply_($B1);$v4->apply_($C1);$v4->apply_($D1);$v4->apply_($E1);$v4->apply_($F1);$H4->apply_($s1);$H4->apply_($w1);$H4->apply_($x1);$H4->apply_($y1);$H4->apply_($z1);$H4->apply_($A1);$H4->apply_($B1);$H4->apply_($C1);$H4->apply_($D1);$H4->apply_($E1);$H4->apply_($F1);$d5->apply_($s1);$v7->apply_($E1);$F7->apply_($E1);$R7->apply_($E1);$f8->apply_($E1);$z8->apply_($E1);$ea->apply_($p2);$Ca->apply_($p2);$Ka->apply_($p2);$Ka->apply_($v2);$kb->apply_($p2);$kb->apply_($v2);$Ab->apply_($p2);$Ab->apply_($v2);$Qb->apply_($p2);$mc->apply_($p2);$uc->apply_($p2);$Nd->apply_($I1);$Nd->apply_($J1);$Nd->apply_($L1);$Nd->apply_($M1);$Nd->apply_($N1);$Nd->apply_($O1);$Nd->apply_($P1);$Nd->apply_($Q1);$Nd->apply_($R1);$Nd->apply_($S1);$Nd->apply_($T1);$Nd->apply_($U1);$Nd->apply_($V1);$Nd->apply_($W1);$Nd->apply_($Y1);$Nd->apply_($c2);$Nd->apply_($e2);$Nd->apply_($g2);$Nd->apply_($h2);$Nd->apply_($i2);$Nd->apply_($k2);$Nd->apply_($l2);$Nd->apply_($m2);$Nd->apply_($o2);$Nd->apply_($q2);$Nd->apply_($s2);$Nd->apply_($u2);$Nd->apply_($w2);$Nd->apply_($y2);$Nd->apply_($A2);$Nd->apply_($C2);$Nd->apply_($E2);$Nd->apply_($F2);$Nd->apply_($H2);$Nd->apply_($I2);$Nd->apply_($J2);$Nd->apply_($K2);$Nd->apply_($L2);$Nd->apply_($M2);$Nd->apply_($O2);$Nd->apply_($P2);$Nd->apply_($Q2);$Nd->apply_($S2);$Td->apply_($I1);$Td->apply_($J1);$Td->apply_($L1);$Td->apply_($M1);$Td->apply_($N1);$Td->apply_($O1);$Td->apply_($P1);$Td->apply_($Q1);$Td->apply_($R1);$Td->apply_($S1);$Td->apply_($T1);$Td->apply_($U1);$Td->apply_($V1);$Td->apply_($W1);$Td->apply_($Y1);$Td->apply_($c2);$Td->apply_($e2);$Td->apply_($g2);$Td->apply_($h2);$Td->apply_($i2);$Td->apply_($k2);$Td->apply_($S);$Td->apply_($l2);$Td->apply_($m2);$Td->apply_($o2);$Td->apply_($q2);$Td->apply_($s2);$Td->apply_($u2);$Td->apply_($w2);$Td->apply_($x2);$Td->apply_($y2);$Td->apply_($z2);$Td->apply_($A2);$Td->apply_($C2);$Td->apply_($E2);$Td->apply_($F2);$Td->apply_($H2);$Td->apply_($I2);$Td->apply_($J2);$Td->apply_($K2);$Td->apply_($L2);$Td->apply_($M2);$Td->apply_($O2);$Td->apply_($P2);$Td->apply_($Q2);$Td->apply_($S2);$ce->apply_($I1);$ce->apply_($J1);$ce->apply_($L1);$ce->apply_($M1);$ce->apply_($N1);$ce->apply_($O1);$ce->apply_($P1);$ce->apply_($Q1);$ce->apply_($R1);$ce->apply_($S1);$ce->apply_($T1);$ce->apply_($U1);$ce->apply_($V1);$ce->apply_($W1);$ce->apply_($Y1);$ce->apply_($c2);$ce->apply_($e2);$ce->apply_($g2);$ce->apply_($h2);$ce->apply_($i2);$ce->apply_($k2);$ce->apply_($l2);$ce->apply_($m2);$ce->apply_($o2);$ce->apply_($q2);$ce->apply_($s2);$ce->apply_($u2);$ce->apply_($w2);$ce->apply_($x2);$ce->apply_($y2);$ce->apply_($z2);$ce->apply_($A2);$ce->apply_($C2);$ce->apply_($E2);$ce->apply_($F2);$ce->apply_($H2);$ce->apply_($I2);$ce->apply_($J2);$ce->apply_($K2);$ce->apply_($L2);$ce->apply_($M2);$ce->apply_($O2);$ce->apply_($P2);$ce->apply_($Q2);$ce->apply_($S2);$je->apply_($I1);$je->apply_($J1);$je->apply_($L1);$je->apply_($M1);$je->apply_($N1);$je->apply_($O1);$je->apply_($P1);$je->apply_($Q1);$je->apply_($R1);$je->apply_($S1);$je->apply_($T1);$je->apply_($U1);$je->apply_($V1);$je->apply_($W1);$je->apply_($Y1);$je->apply_($c2);$je->apply_($e2);$je->apply_($g2);$je->apply_($h2);$je->apply_($i2);$je->apply_($k2);$je->apply_($l2);$je->apply_($m2);$je->apply_($o2);$je->apply_($q2);$je->apply_($s2);$je->apply_($u2);$je->apply_($w2);$je->apply_($x2);$je->apply_($y2);$je->apply_($z2);$je->apply_($A2);$je->apply_($C2);$je->apply_($E2);$je->apply_($F2);$je->apply_($H2);$je->apply_($I2);$je->apply_($J2);$je->apply_($K2);$je->apply_($L2);$je->apply_($M2);$je->apply_($O2);$je->apply_($P2);$je->apply_($Q2);$je->apply_($S2);$qe->apply_($I1);$qe->apply_($J1);$qe->apply_($L1);$qe->apply_($M1);$qe->apply_($N1);$qe->apply_($O1);$qe->apply_($P1);$qe->apply_($Q1);$qe->apply_($R1);$qe->apply_($S1);$qe->apply_($T1);$qe->apply_($U1);$qe->apply_($V1);$qe->apply_($W1);$qe->apply_($Y1);$qe->apply_($c2);$qe->apply_($e2);$qe->apply_($g2);$qe->apply_($h2);$qe->apply_($i2);$qe->apply_($k2);$qe->apply_($l2);$qe->apply_($m2);$qe->apply_($o2);$qe->apply_($q2);$qe->apply_($s2);$qe->apply_($u2);$qe->apply_($w2);$qe->apply_($y2);$qe->apply_($z2);$qe->apply_($A2);$qe->apply_($C2);$qe->apply_($E2);$qe->apply_($F2);$qe->apply_($H2);$qe->apply_($I2);$qe->apply_($J2);$qe->apply_($K2);$qe->apply_($L2);$qe->apply_($M2);$qe->apply_($O2);$qe->apply_($P2);$qe->apply_($Q2);$qe->apply_($S2);$ye->apply_($I1);$ye->apply_($J1);$ye->apply_($L1);$ye->apply_($M1);$ye->apply_($N1);$ye->apply_($O1);$ye->apply_($P1);$ye->apply_($Q1);$ye->apply_($R1);$ye->apply_($S1);$ye->apply_($T1);$ye->apply_($U1);$ye->apply_($V1);$ye->apply_($W1);$ye->apply_($Y1);$ye->apply_($c2);$ye->apply_($e2);$ye->apply_($g2);$ye->apply_($i2);$ye->apply_($k2);$ye->apply_($l2);$ye->apply_($m2);$ye->apply_($o2);$ye->apply_($q2);$ye->apply_($s2);$ye->apply_($u2);$ye->apply_($w2);$ye->apply_($y2);$ye->apply_($A2);$ye->apply_($C2);$ye->apply_($E2);$ye->apply_($F2);$ye->apply_($H2);$ye->apply_($I2);$ye->apply_($J2);$ye->apply_($K2);$ye->apply_($L2);$ye->apply_($M2);$ye->apply_($O2);$ye->apply_($P2);$ye->apply_($Q2);$ye->apply_($S2);$Ge->apply_($I1);$Ge->apply_($J1);$Ge->apply_($L1);$Ge->apply_($M1);$Ge->apply_($N1);$Ge->apply_($O1);$Ge->apply_($P1);$Ge->apply_($Q1);$Ge->apply_($R1);$Ge->apply_($S1);$Ge->apply_($T1);$Ge->apply_($U1);$Ge->apply_($V1);$Ge->apply_($W1);$Ge->apply_($Y1);$Ge->apply_($c2);$Ge->apply_($e2);$Ge->apply_($f2);$Ge->apply_($g2);$Ge->apply_($h2);$Ge->apply_($i2);$Ge->apply_($j2);$Ge->apply_($k2);$Ge->apply_($l2);$Ge->apply_($m2);$Ge->apply_($o2);$Ge->apply_($q2);$Ge->apply_($s2);$Ge->apply_($u2);$Ge->apply_($w2);$Ge->apply_($x2);$Ge->apply_($y2);$Ge->apply_($z2);$Ge->apply_($A2);$Ge->apply_($C2);$Ge->apply_($E2);$Ge->apply_($F2);$Ge->apply_($H2);$Ge->apply_($I2);$Ge->apply_($J2);$Ge->apply_($K2);$Ge->apply_($L2);$Ge->apply_($M2);$Ge->apply_($O2);$Ge->apply_($P2);$Ge->apply_($Q2);$Ge->apply_($S2);$Re->apply_($I1);$Re->apply_($J1);$Re->apply_($L1);$Re->apply_($M1);$Re->apply_($N1);$Re->apply_($O1);$Re->apply_($P1);$Re->apply_($Q1);$Re->apply_($R1);$Re->apply_($S1);$Re->apply_($T1);$Re->apply_($U1);$Re->apply_($V1);$Re->apply_($W1);$Re->apply_($Y1);$Re->apply_($c2);$Re->apply_($e2);$Re->apply_($g2);$Re->apply_($h2);$Re->apply_($i2);$Re->apply_($k2);$Re->apply_($l2);$Re->apply_($m2);$Re->apply_($o2);$Re->apply_($q2);$Re->apply_($s2);$Re->apply_($u2);$Re->apply_($w2);$Re->apply_($y2);$Re->apply_($A2);$Re->apply_($C2);$Re->apply_($E2);$Re->apply_($F2);$Re->apply_($H2);$Re->apply_($I2);$Re->apply_($J2);$Re->apply_($K2);$Re->apply_($L2);$Re->apply_($M2);$Re->apply_($O2);$Re->apply_($P2);$Re->apply_($Q2);$Re->apply_($S2);$df->apply_($I1);$df->apply_($J1);$df->apply_($L1);$df->apply_($M1);$df->apply_($N1);$df->apply_($O1);$df->apply_($P1);$df->apply_($Q1);$df->apply_($R1);$df->apply_($S1);$df->apply_($T1);$df->apply_($U1);$df->apply_($V1);$df->apply_($W1);$df->apply_($Y1);$df->apply_($c2);$df->apply_($e2);$df->apply_($g2);$df->apply_($h2);$df->apply_($i2);$df->apply_($k2);$df->apply_($l2);$df->apply_($m2);$df->apply_($o2);$df->apply_($q2);$df->apply_($s2);$df->apply_($u2);$df->apply_($w2);$df->apply_($y2);$df->apply_($A2);$df->apply_($C2);$df->apply_($E2);$df->apply_($F2);$df->apply_($H2);$df->apply_($I2);$df->apply_($J2);$df->apply_($K2);$df->apply_($L2);$df->apply_($M2);$df->apply_($O2);$df->apply_($P2);$df->apply_($Q2);$df->apply_($S2);$kf->apply_($I1);$kf->apply_($J1);$kf->apply_($L1);$kf->apply_($M1);$kf->apply_($N1);$kf->apply_($O1);$kf->apply_($P1);$kf->apply_($Q1);$kf->apply_($R1);$kf->apply_($S1);$kf->apply_($T1);$kf->apply_($U1);$kf->apply_($V1);$kf->apply_($W1);$kf->apply_($Y1);$kf->apply_($c2);$kf->apply_($e2);$kf->apply_($g2);$kf->apply_($h2);$kf->apply_($i2);$kf->apply_($k2);$kf->apply_($l2);$kf->apply_($m2);$kf->apply_($o2);$kf->apply_($q2);$kf->apply_($s2);$kf->apply_($u2);$kf->apply_($w2);$kf->apply_($y2);$kf->apply_($A2);$kf->apply_($C2);$kf->apply_($E2);$kf->apply_($F2);$kf->apply_($H2);$kf->apply_($I2);$kf->apply_($J2);$kf->apply_($K2);$kf->apply_($L2);$kf->apply_($M2);$kf->apply_($O2);$kf->apply_($P2);$kf->apply_($Q2);$kf->apply_($S2);$rf->apply_($I1);$rf->apply_($J1);$rf->apply_($L1);$rf->apply_($M1);$rf->apply_($N1);$rf->apply_($O1);$rf->apply_($P1);$rf->apply_($Q1);$rf->apply_($R1);$rf->apply_($S1);$rf->apply_($T1);$rf->apply_($U1);$rf->apply_($V1);$rf->apply_($W1);$rf->apply_($Y1);$rf->apply_($c2);$rf->apply_($e2);$rf->apply_($g2);$rf->apply_($h2);$rf->apply_($i2);$rf->apply_($k2);$rf->apply_($l2);$rf->apply_($m2);$rf->apply_($o2);$rf->apply_($q2);$rf->apply_($s2);$rf->apply_($u2);$rf->apply_($w2);$rf->apply_($y2);$rf->apply_($A2);$rf->apply_($C2);$rf->apply_($E2);$rf->apply_($F2);$rf->apply_($H2);$rf->apply_($I2);$rf->apply_($J2);$rf->apply_($K2);$rf->apply_($L2);$rf->apply_($M2);$rf->apply_($O2);$rf->apply_($P2);$rf->apply_($Q2);$rf->apply_($S2);$yf->apply_($I1);$yf->apply_($J1);$yf->apply_($L1);$yf->apply_($M1);$yf->apply_($N1);$yf->apply_($O1);$yf->apply_($P1);$yf->apply_($Q1);$yf->apply_($R1);$yf->apply_($S1);$yf->apply_($T1);$yf->apply_($U1);$yf->apply_($V1);$yf->apply_($W1);$yf->apply_($Y1);$yf->apply_($c2);$yf->apply_($e2);$yf->apply_($g2);$yf->apply_($h2);$yf->apply_($i2);$yf->apply_($k2);$yf->apply_($l2);$yf->apply_($m2);$yf->apply_($o2);$yf->apply_($q2);$yf->apply_($s2);$yf->apply_($u2);$yf->apply_($w2);$yf->apply_($y2);$yf->apply_($A2);$yf->apply_($C2);$yf->apply_($E2);$yf->apply_($F2);$yf->apply_($H2);$yf->apply_($I2);$yf->apply_($J2);$yf->apply_($K2);$yf->apply_($L2);$yf->apply_($M2);$yf->apply_($O2);$yf->apply_($P2);$yf->apply_($Q2);$yf->apply_($S2);$Ff->apply_($I1);$Ff->apply_($J1);$Ff->apply_($L1);$Ff->apply_($M1);$Ff->apply_($N1);$Ff->apply_($O1);$Ff->apply_($P1);$Ff->apply_($Q1);$Ff->apply_($R1);$Ff->apply_($S1);$Ff->apply_($T1);$Ff->apply_($U1);$Ff->apply_($V1);$Ff->apply_($W1);$Ff->apply_($Y1);$Ff->apply_($c2);$Ff->apply_($e2);$Ff->apply_($g2);$Ff->apply_($h2);$Ff->apply_($i2);$Ff->apply_($k2);$Ff->apply_($l2);$Ff->apply_($m2);$Ff->apply_($o2);$Ff->apply_($q2);$Ff->apply_($s2);$Ff->apply_($u2);$Ff->apply_($w2);$Ff->apply_($y2);$Ff->apply_($A2);$Ff->apply_($C2);$Ff->apply_($E2);$Ff->apply_($F2);$Ff->apply_($H2);$Ff->apply_($I2);$Ff->apply_($J2);$Ff->apply_($K2);$Ff->apply_($L2);$Ff->apply_($M2);$Ff->apply_($O2);$Ff->apply_($P2);$Ff->apply_($Q2);$Ff->apply_($S2);$Qf->apply_($I1);$Qf->apply_($J1);$Qf->apply_($L1);$Qf->apply_($M1);$Qf->apply_($N1);$Qf->apply_($O1);$Qf->apply_($P1);$Qf->apply_($Q1);$Qf->apply_($R1);$Qf->apply_($S1);$Qf->apply_($T1);$Qf->apply_($U1);$Qf->apply_($V1);$Qf->apply_($W1);$Qf->apply_($Y1);$Qf->apply_($c2);$Qf->apply_($e2);$Qf->apply_($g2);$Qf->apply_($i2);$Qf->apply_($k2);$Qf->apply_($l2);$Qf->apply_($A);$Qf->apply_($m2);$Qf->apply_($o2);$Qf->apply_($q2);$Qf->apply_($s2);$Qf->apply_($u2);$Qf->apply_($w2);$Qf->apply_($x2);$Qf->apply_($y2);$Qf->apply_($z2);$Qf->apply_($A2);$Qf->apply_($C2);$Qf->apply_($E2);$Qf->apply_($F2);$Qf->apply_($H2);$Qf->apply_($I2);$Qf->apply_($J2);$Qf->apply_($K2);$Qf->apply_($M2);$Qf->apply_($O2);$Qf->apply_($P2);$Qf->apply_($Q2);$Qf->apply_($S2);$Xf->apply_($I1);$Xf->apply_($J1);$Xf->apply_($L1);$Xf->apply_($M1);$Xf->apply_($N1);$Xf->apply_($O1);$Xf->apply_($P1);$Xf->apply_($Q1);$Xf->apply_($R1);$Xf->apply_($S1);$Xf->apply_($T1);$Xf->apply_($U1);$Xf->apply_($V1);$Xf->apply_($W1);$Xf->apply_($Y1);$Xf->apply_($c2);$Xf->apply_($e2);$Xf->apply_($g2);$Xf->apply_($i2);$Xf->apply_($k2);$Xf->apply_($l2);$Xf->apply_($m2);$Xf->apply_($o2);$Xf->apply_($q2);$Xf->apply_($s2);$Xf->apply_($u2);$Xf->apply_($w2);$Xf->apply_($y2);$Xf->apply_($A2);$Xf->apply_($C2);$Xf->apply_($E2);$Xf->apply_($F2);$Xf->apply_($H2);$Xf->apply_($I2);$Xf->apply_($K2);$Xf->apply_($M2);$Xf->apply_($O2);$Xf->apply_($P2);$Xf->apply_($Q2);$Xf->apply_($S2);$Gg->apply_($K1);$Og->apply_($K1);$nh->apply_($M1);$nh->apply_($N1);$nh->apply_($O1);$nh->apply_($P1);$nh->apply_($Q1);$nh->apply_($R1);$nh->apply_($S1);$nh->apply_($T1);$nh->apply_($U1);$nh->apply_($V1);$nh->apply_($W1);$Fh->apply_($w1);$Xh->apply_($w1);$ri->apply_($x1);$yi->apply_($x1);$Ri->apply_($y1);$Yi->apply_($y1);$uj->apply_($y1);$Oj->apply_($y1);$Wj->apply_($y1);$ok->apply_($y1);$Lk->apply_($z1);$Lk->apply_($B1);$Sk->apply_($z1);$cl->apply_($z1);$sl->apply_($z1);$sl->apply_($B1);$Dl->apply_($z1);$Rl->apply_($z1);$Rl->apply_($B1);$wm->apply_($Q1);$Om->apply_($A1);$Wm->apply_($A1);$fn->apply_($A1);$vn->apply_($A1);$Dn->apply_($A1);$Yn->apply_($A1);$wo->apply_($B1);$Fo->apply_($B1);$sp->apply_($tp);$Dp->apply_($C1);$Qp->apply_($C1);$Oq->apply_($F1);$Vq->apply_($F1);$lr->apply_($F1);$Hr->apply_($X1);$Hr->apply_($Z1);$Hr->apply_($d2);$Hr->apply_($R2);$Tr->apply_($X1);$Tr->apply_($Z1);$Tr->apply_($d2);$Tr->apply_($R2);$ks->apply_($X1);$ks->apply_($Z1);$ks->apply_($d2);$Es->apply_($X1);$Es->apply_($Z1);$Es->apply_($d2);$Us->apply_($Y1);$Us->apply_($c2);$Us->apply_($e2);$st->apply_($Z1);$zt->apply_($Z1);$Jt->apply_($Z1);$Vt->apply_($Z1);$yu->apply_($c2);$Ru->apply_($d2);$Yu->apply_($d2);$Av->apply_($h2);$Tv->apply_($j2);$Zv->apply_($j2);$ww->apply_($S);$Cw->apply_($S);$Lw->apply_($S);$Tw->apply_($S);$Zw->apply_($S);$hx->apply_($S);$rx->apply_($S);$Dx->apply_($l2);$jy->apply_($A);$By->apply_($A);$Ky->apply_($A);$Sy->apply_($A);$kz->apply_($m2);$Dz->apply_($n2);$Kz->apply_($n2);$gA->apply_($n2);$AA->apply_($n2);$ZA->apply_($tp);$OB->apply_($PB);$kC->apply_($r2);$CC->apply_($r2);$XC->apply_($r2);$iD->apply_($r2);$sD->apply_($r2);$yE->apply_($PB);$HE->apply_($t2);$HE->apply_($C);$HE->apply_($H);$fF->apply_($v2);$oF->apply_($v2);$NF->apply_($x2);$TF->apply_($x2);$cG->apply_($x2);$GG->apply_($tp);$QG->apply_($z2);$WG->apply_($z2);$uH->apply_($B2);$uH->apply_($D2);$EH->apply_($B2);$LH->apply_($B2);$ZH->apply_($B2);$GI->apply_($C);$NI->apply_($C);$UI->apply_($C);$gJ->apply_($C);$rJ->apply_($C);$GJ->apply_($F2);$OJ->apply_($F2);$jK->apply_($G2);$yK->apply_($G2);$FK->apply_($G2);$YK->apply_($H);$gL->apply_($H);$mL->apply_($H);$AL->apply_($tp);$ni::self=$lN;&$V($T);&$V($e1);&$V($k1);&$V($i3);&$V($k3);&$m3($k3);&$V($t3);&$V($N3);&$V($V3);&$V($j4);&$V($v4);&$V($H4);&$V($J4);&$m3($J4);&$V($d5);&$V($f5);&$m3($f5);&$V($l5);&$V($r5);&$V($E5);&$V($R5);&$V($g6);&$V($t6);&$V($P6);&$V($V6);&$V($e7);&$V($v7);&$V($F7);&$V($R7);&$V($f8);&$V($z8);&$V($B8);&$m3($B8);&$V($U8);&$V($d9);&$V($x9);&$V($O9);&$V($ea);&$V($Ca);&$V($Ka);&$V($kb);&$V($Ab);&$V($Qb);&$V($mc);&$V($uc);&$V($wc);&$m3($wc);&$V($Cc);&$V($Kc);&$V($Vc);&$V($sd);&$V($yd);&$V($Nd);&$V($Td);&$V($ce);&$V($je);&$V($qe);&$V($se);&$V($ye);&$V($Ge);&$V($Ie);&$m3($Ie);&$V($Re);&$V($df);&$V($kf);&$V($rf);&$V($yf);&$V($Ff);&$V($Hf);&$V($Jf);&$m3($Jf);&$V($Qf);&$V($Xf);&$V($Zf);&$m3($Zf);&$V($kg);&$m3($kg);&$V($og);&$m3($og);&$V($qg);&$m3($qg);&$V($sg);&$m3($sg);&$V($xg);&$m3($xg);&$V($Gg);&$V($Og);&$V($Qg);&$m3($Qg);&$V($Vg);&$m3($Vg);&$V($nh);&$V($ph);&$m3($ph);&$V($rh);&$m3($rh);&$V($wh);&$m3($wh);&$V($Fh);&$V($Xh);&$V($Zh);&$m3($Zh);&$V($gi);&$m3($gi);&$V($ri);&$V($yi);&$V($Ai);&$m3($Ai);&$V($Fi);&$m3($Fi);&$V($Ri);&$V($Yi);&$V($uj);&$V($Oj);&$V($Wj);&$V($ok);&$V($qk);&$m3($qk);&$V($vk);&$m3($vk);&$V($Lk);&$V($Sk);&$V($cl);&$V($sl);&$V($Dl);&$V($Rl);&$V($Ul);&$m3($Ul);&$Xl($Ul);&$V($wm);&$V($ym);&$m3($ym);&$V($Om);&$V($Wm);&$V($fn);&$V($vn);&$V($Dn);&$V($Yn);&$V($co);&$m3($co);&$V($ho);&$m3($ho);&$V($wo);&$V($Fo);&$V($Ho);&$m3($Ho);&$V($Mo);&$m3($Mo);&$V($sp);&$V($Dp);&$V($Qp);&$V($Sp);&$m3($Sp);&$V($Xp);&$m3($Xp);&$V($qq);&$m3($qq);&$V($Oq);&$V($Vq);&$V($lr);&$V($nr);&$m3($nr);&$V($sr);&$m3($sr);&$V($Hr);&$V($Tr);&$V($Vr);&$m3($Vr);&$V($ks);&$V($Es);&$V($Gs);&$m3($Gs);&$Js($Gs);&$V($Qs);&$m3($Qs);&$V($Us);&$V($Ws);&$m3($Ws);&$V($st);&$V($zt);&$V($Jt);&$V($Vt);&$V($cu);&$m3($cu);&$Js($cu);&$fu($cu);&$V($yu);&$V($Au);&$m3($Au);&$V($Ru);&$V($Yu);&$V($cv);&$m3($cv);&$Js($cv);&$V($hv);&$m3($hv);&$V($ov);&$m3($ov);&$V($Av);&$V($Cv);&$m3($Cv);&$V($Iv);&$m3($Iv);&$V($Tv);&$V($Zv);&$V($dw);&$m3($dw);&$V($iw);&$m3($iw);&$V($ww);&$V($Cw);&$V($Lw);&$V($Tw);&$V($Zw);&$V($hx);&$V($rx);&$V($tx);&$m3($tx);&$V($Dx);&$V($Fx);&$m3($Fx);&$V($jy);&$V($By);&$V($Ky);&$V($Sy);&$V($Uy);&$m3($Uy);&$Xy($Uy);&$V($kz);&$V($mz);&$m3($mz);&$V($Dz);&$V($Kz);&$V($gA);&$V($AA);&$V($CA);&$m3($CA);&$V($HA);&$m3($HA);&$V($ZA);&$V($hB);&$m3($hB);&$V($OB);&$V($kC);&$V($CC);&$V($XC);&$V($iD);&$V($sD);&$V($uD);&$m3($uD);&$V($zD);&$m3($zD);&$V($yE);&$V($HE);&$V($JE);&$m3($JE);&$V($OE);&$m3($OE);&$V($fF);&$V($oF);&$V($qF);&$m3($qF);&$V($vF);&$m3($vF);&$V($NF);&$V($TF);&$V($cG);&$V($eG);&$m3($eG);&$V($kG);&$m3($kG);&$V($GG);&$V($QG);&$V($WG);&$V($YG);&$m3($YG);&$V($gH);&$m3($gH);&$V($uH);&$V($wH);&$m3($wH);&$V($EH);&$V($LH);&$V($ZH);&$V($dI);&$m3($dI);&$V($kI);&$m3($kI);&$V($mI);&$m3($mI);&$V($GI);&$V($NI);&$V($UI);&$V($gJ);&$V($rJ);&$V($tJ);&$m3($tJ);&$wJ($tJ);&$V($GJ);&$V($OJ);&$V($QJ);&$m3($QJ);&$V($jK);&$V($yK);&$V($FK);&$V($HK);&$m3($HK);&$V($MK);&$m3($MK);&$V($YK);&$V($gL);&$V($mL);&$V($oL);&$m3($oL);&$V($tL);&$m3($tL);&$V($AL);&$V($IL);&$m3($IL);&$V($NL);&$m3($NL);&$V($WL);&$m3($WL);&$V($dM);&$m3($dM);&$V($iM);&$m3($iM);&$V($qM);&$m3($qM);&$V($MM);&$m3($MM);ni->run(@ARGV);
__DATA__
