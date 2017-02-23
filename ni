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
package.#;$n=q#assertions#;$o=[];$p=q#error#;$q=undef;$r=q#outcome#;$s=q#test#;$t=q#annotations#;$u=[];$v=q#code#;$w=q#my $fn = fn q{"hi"};
my $slice = ni('ni:/lib/slice')->new('myslice', f => $fn);
$slice->apply('foo');
now foo->f == 'hi';#;$x=q#proto#;$y=q##;$z=q#lib/fn#;$A=bless({$t,$u,$v,$w,$x,$y},$z);$B=q#lib/test_case#;$C=bless({$n,$o,$p,$q,$r,$q,$s,$A},$B);$D=q#TODO...#;$E=[$l,$m,$C,$D];$F=q#classes#;$G=q#ni implements a Smalltalk 80-style metaclass system with a couple of
differences. First, ni's classes are slice unions and as such don't
support colliding methods; and second, they support multiple inheritance.
These two points are related: method overriding isn't in the picture,
which makes multiple inheritance straightforward to implement.#;$H=[$F,$G,$D];$I=[$h,$k,$E,$H];$J=q#name#;$K=q#/class#;$L=q#lib/doc#;$M=bless({$e,$I,$J,$K},$L);$N=q#my $s = shift; ni->def($s->name, $s)#;$O=bless({$v,$N,$x,$y},$z);$P=q#ni.doc:/fabric#;$Q=q#Abstractions to bridge the gaps between separate machines and processes.
This module is designed to make it appear as though all resources are
local, or at least can be referred to locally -- even when they belong to
an external process (e.g. a Hadoop mapper) or another machine (e.g. a
file over SSH). If we can bidirectionally communicate with a remote ni
instance, then we can see its resources.#;$R=q#At the RMI level, the abstraction involves sending a function to a remote
ni instance; arguments are transported using simple serialization.
#;$S=[$i,$Q,$R];$T=[$S];$U=q#/fabric#;$V=bless({$e,$T,$J,$U},$L);$W=q#ni.doc:/io#;$X=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$Y=[$i,$X];$Z=[$Y];$c1=q#/io#;$d1=bless({$e,$Z,$J,$c1},$L);$e1=q#ni.doc:/io/buffer#;$f1=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$g1=[$f,$f1];$h1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$i1=[];$j1=[];$k1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$l1=bless({$t,$j1,$v,$k1,$x,$y},$z);$m1=bless({$n,$i1,$p,$q,$r,$q,$s,$l1},$B);$n1=[$i,$h1,$m1];$o1=[$g1,$n1];$p1=q#/io/buffer#;$q1=bless({$e,$o1,$J,$p1},$L);$r1=q#ni.doc:/io/cat#;$s1=q#
  my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
  my $combined = $io1 + $io2 + $io3;
  $combined->into_sync($destination_io);
#;$t1=[$f,$s1];$u1=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$v1=[];$w1=[];$x1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$y1=bless({$t,$w1,$v,$x1,$x,$y},$z);$z1=bless({$n,$v1,$p,$q,$r,$q,$s,$y1},$B);$A1=[$i,$u1,$z1];$B1=[$t1,$A1];$C1=q#/io/cat#;$D1=bless({$e,$B1,$J,$C1},$L);$E1=q#ni.doc:/io/exec#;$F1=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$G1=[$f,$F1];$H1=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$I1=[];$J1=[];$K1=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$L1=bless({$t,$J1,$v,$K1,$x,$y},$z);$M1=bless({$n,$I1,$p,$q,$r,$q,$s,$L1},$B);$N1=[$i,$H1,$M1];$O1=[$G1,$N1];$P1=q#/io/exec#;$Q1=bless({$e,$O1,$J,$P1},$L);$R1=q#ni.doc:/io/fd#;$S1=q#
  open my $fh, ...;
  my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
  my $fd = ni('ni:/io/fd')->new(0);   \# from number
  my $fd = ni('fd:0');                \# same thing
  $fd->nonblock(1)->read($_, 100);
  $fd->be(10);                        \# move FD number
#;$T1=[$f,$S1];$U1=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$V1=[];$W1=[];$X1=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$Y1=bless({$t,$W1,$v,$X1,$x,$y},$z);$Z1=bless({$n,$V1,$p,$q,$r,$q,$s,$Y1},$B);$c2=[$i,$U1,$Z1];$d2=[$T1,$c2];$e2=q#/io/fd#;$f2=bless({$e,$d2,$J,$e2},$L);$g2=q#ni.doc:/io/file#;$h2=q#
  my $f = ni('ni:/io/file')->new('/etc/passwd');
  my $f = ni('file:/etc/passwd');     \# same as above
  $f->into_sync(ni('fd:1'));          \# cat to stdout
#;$i2=[$f,$h2];$j2=q#warning#;$k2=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$l2=[$j2,$k2];$m2=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$n2=[];$o2=[];$p2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$q2=bless({$t,$o2,$v,$p2,$x,$y},$z);$r2=bless({$n,$n2,$p,$q,$r,$q,$s,$q2},$B);$s2=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$t2=[];$u2=[];$v2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$w2=bless({$t,$u2,$v,$v2,$x,$y},$z);$x2=bless({$n,$t2,$p,$q,$r,$q,$s,$w2},$B);$y2=[$i,$m2,$r2,$s2,$x2];$z2=[$i2,$l2,$y2];$A2=q#/io/file#;$B2=bless({$e,$z2,$J,$A2},$L);$C2=q#ni.doc:/io/file_update_fd#;$D2=q#A write fd that performs a file rename upon closing.#;$E2=[$i,$D2];$F2=[$E2];$G2=q#/io/file_update_fd#;$H2=bless({$e,$F2,$J,$G2},$L);$I2=q#ni.doc:/io/pid#;$J2=q#eg#;$K2=[];$L2=[];$M2=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$N2=bless({$t,$L2,$v,$M2,$x,$y},$z);$O2=bless({$n,$K2,$p,$q,$r,$q,$s,$N2},$B);$P2=[$J2,$O2];$Q2=[];$R2=[];$S2=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$T2=bless({$t,$R2,$v,$S2,$x,$y},$z);$U2=bless({$n,$Q2,$p,$q,$r,$q,$s,$T2},$B);$V2=[$J2,$U2];$W2=[];$X2=[];$Y2=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$Z2=bless({$t,$X2,$v,$Y2,$x,$y},$z);$c3=bless({$n,$W2,$p,$q,$r,$q,$s,$Z2},$B);$d3=[$J2,$c3];$e3=[$P2,$V2,$d3];$f3=q#/io/pid#;$g3=bless({$e,$e3,$J,$f3},$L);$h3=q#ni.doc:/lib#;$i3=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$j3=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$k3=[$i,$i3,$j3];$l3=[$k3];$m3=q#/lib#;$n3=bless({$e,$l3,$J,$m3},$L);$o3=q#ni.doc:/lib/doc#;$p3=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ...#;$q3=[$f,$p3];$r3=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$s3=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$t3=[];$u3=[];$v3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$w3=bless({$t,$u3,$v,$v3,$x,$y},$z);$x3=bless({$n,$t3,$p,$q,$r,$q,$s,$w3},$B);$y3=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$z3=[];$A3=[];$B3=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$C3=bless({$t,$A3,$v,$B3,$x,$y},$z);$D3=bless({$n,$z3,$p,$q,$r,$q,$s,$C3},$B);$E3=[$i,$r3,$s3,$x3,$y3,$D3];$F3=[$q3,$E3];$G3=q#/lib/doc#;$H3=bless({$e,$F3,$J,$G3},$L);$I3=q#ni.doc:/lib/future#;$J3=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$K3=[];$L3=[];$M3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$N3=bless({$t,$L3,$v,$M3,$x,$y},$z);$O3=bless({$n,$K3,$p,$q,$r,$q,$s,$N3},$B);$P3=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$Q3=[];$R3=[];$S3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$T3=bless({$t,$R3,$v,$S3,$x,$y},$z);$U3=bless({$n,$Q3,$p,$q,$r,$q,$s,$T3},$B);$V3=[$i,$J3,$O3,$P3,$U3];$W3=[$V3];$X3=q#/lib/future#;$Y3=bless({$e,$W3,$J,$X3},$L);$Z3=q#ni.doc:/lib/image#;$c4=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$d4=[$f,$c4];$e4=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$f4=[$i,$e4];$g4=[$d4,$f4];$h4=q#/lib/image#;$i4=bless({$e,$g4,$J,$h4},$L);$j4=q#ni.doc:/lib/ni#;$k4=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$l4=[$f,$k4];$m4=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$n4=[$i,$m4];$o4=[$l4,$n4];$p4=q#/lib/ni#;$q4=bless({$e,$o4,$J,$p4},$L);$r4=q#ni.doc:/lib/quote_simple#;$s4=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$t4=[];$u4=[];$v4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$w4=bless({$t,$u4,$v,$v4,$x,$y},$z);$x4=bless({$n,$t4,$p,$q,$r,$q,$s,$w4},$B);$y4=[$i,$s4,$x4];$z4=[$y4];$A4=q#/lib/quote_simple#;$B4=bless({$e,$z4,$J,$A4},$L);$C4=q#ni.doc:/lib/slice#;$D4=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$E4=[$f,$D4];$F4=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$G4=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$H4=[];$I4=[];$J4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$K4=bless({$t,$I4,$v,$J4,$x,$y},$z);$L4=bless({$n,$H4,$p,$q,$r,$q,$s,$K4},$B);$M4=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$N4=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$O4=[];$P4=[];$Q4=q#my $instances = 0;
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
now $instances == 0;#;$R4=bless({$t,$P4,$v,$Q4,$x,$y},$z);$S4=bless({$n,$O4,$p,$q,$r,$q,$s,$R4},$B);$T4=[$i,$F4,$G4,$L4,$M4,$N4,$S4];$U4=[$E4,$T4];$V4=q#/lib/slice#;$W4=bless({$e,$U4,$J,$V4},$L);$X4=q#ni.doc:/semantic#;$Y4=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$Z4=[$i,$Y4];$c5=[$Z4];$d5=q#/semantic#;$e5=bless({$e,$c5,$J,$d5},$L);$f5=q#ni:/class#;$g5=q#applied_to#;$h5=q#class#;$i5=q#class.c#;$j5=q#io/buffer.c#;$k5=q#io/cat.c#;$l5=q#io/exec.c#;$m5=q#io/fd.c#;$n5=q#io/file.c#;$o5=q#io/file_update_fd.c#;$p5=q#io/null.c#;$q5=q#io/object.c#;$r5=q#io/pid.c#;$s5=q#io/str.c#;$t5=q#io/transfer.c#;$u5=q#io/transfer_async.c#;$v5=q#io/transfer_sync.c#;$w5=q#lib/behavior.c#;$x5=q#lib/branch.c#;$y5=q#lib/dataslice.c#;$z5=q#lib/doc.c#;$A5=q#lib/fn.c#;$B5=q#lib/future.c#;$C5=q#lib/image.c#;$D5=q#lib/ni.c#;$E5=q#lib/quote_simple.c#;$F5=q#lib/slice.c#;$G5=q#lib/tag.c#;$H5=q#lib/test_assert_eq.c#;$I5=q#lib/test_assertion.c#;$J5=q#lib/test_case.c#;$K5=q#lib/test_value.c#;$L5=q#metaclass.c#;$M5=q#module.c#;$N5=q#object.c#;$O5=q#semantic/dimension#;$P5=q#semantic/dimension.c#;$Q5=q#semantic/task.c#;$R5={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$S5=q#slices#;$T5=q#metaclass#;$U5=q#module#;$V5={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$W5=q#/module#;$X5=q#/lib/perlbranch.b#;$Y5={};$Z5=q#ctor#;$c6=q#dtor#;$d6=q#methods#;$e6=q#add#;$f6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$g6=bless({$v,$f6},$z);$h6=q#apply#;$i6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$j6=bless({$v,$i6},$z);$k6={$e6,$g6,$h6,$j6};$l6=q#/lib/branch.b#;$m6=q#lib/slice#;$n6=bless({$g5,$Y5,$Z5,$q,$c6,$q,$d6,$k6,$J,$l6},$m6);$o6=q#lib/branch#;$p6={};$q6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$r6=bless({$v,$q6},$z);$s6={$J,$r6};$t6=q#/lib/named.b#;$u6=bless({$g5,$p6,$Z5,$O,$c6,$q,$d6,$s6,$J,$t6},$m6);$v6=q#lib/tag#;$w6={};$x6=q#namespace#;$y6=q#'ni'#;$z6=bless({$v,$y6},$z);$A6={$x6,$z6};$B6=q#/lib/named_in_ni.b#;$C6=bless({$g5,$w6,$Z5,$q,$c6,$q,$d6,$A6,$J,$B6},$m6);$D6={};$E6=q#package#;$F6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$G6=bless({$v,$F6},$z);$H6={$E6,$G6};$I6=q#/lib/namespaced.b#;$J6=bless({$g5,$D6,$Z5,$q,$c6,$q,$d6,$H6,$J,$I6},$m6);$K6={};$L6=q#resolve#;$M6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$N6=bless({$v,$M6},$z);$O6={$L6,$N6};$P6=q#/lib/resolver.b#;$Q6=bless({$g5,$K6,$Z5,$q,$c6,$q,$d6,$O6,$J,$P6},$m6);$R6=[$n6,$u6,$C6,$J6,$Q6];$S6=bless({$J,$X5,$S5,$R6},$v6);$T6={};$U6=q#my $s = shift; $s->apply($s->package)#;$V6=bless({$v,$U6,$x,$y},$z);$W6=q#instantiate#;$X6=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$Y6=bless({$v,$X6},$z);$Z6={$W6,$Y6};$c7=q#/lib/class_init.b#;$d7=bless({$g5,$T6,$Z5,$V6,$c6,$q,$d6,$Z6,$J,$c7},$m6);$e7=q#io/buffer#;$f7=q#io/cat#;$g7=q#io/exec#;$h7=q#io/fd#;$i7=q#io/file#;$j7=q#io/file_update_fd#;$k7=q#io/null#;$l7=q#io/object#;$m7=q#io/pid#;$n7=q#io/str#;$o7=q#io/transfer#;$p7=q#io/transfer_async#;$q7=q#io/transfer_sync#;$r7=q#lib/behavior#;$s7=q#lib/dataslice#;$t7=q#lib/future#;$u7=q#lib/image#;$v7=q#lib/ni#;$w7=q#lib/quote_simple#;$x7=q#lib/test_assert_eq#;$y7=q#lib/test_assertion#;$z7=q#lib/test_value#;$A7=q#object#;$B7=q#semantic/task#;$C7={$h5,1,$i5,1,$e7,1,$j5,1,$f7,1,$k5,1,$g7,1,$l5,1,$h7,1,$m5,1,$i7,1,$n5,1,$j7,1,$o5,1,$k7,1,$p5,1,$l7,1,$q5,1,$m7,1,$r5,1,$n7,1,$s5,1,$o7,1,$t5,1,$p7,1,$u5,1,$q7,1,$v5,1,$r7,1,$w5,1,$o6,1,$x5,1,$s7,1,$y5,1,$L,1,$z5,1,$z,1,$A5,1,$t7,1,$B5,1,$u7,1,$C5,1,$v7,1,$D5,1,$w7,1,$E5,1,$m6,1,$F5,1,$v6,1,$G5,1,$x7,1,$H5,1,$y7,1,$I5,1,$B,1,$J5,1,$z7,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$A7,1,$N5,1,$O5,1,$P5,1,$B7,1,$Q5,1};$D7=q#/object#;$E7={};$F7=q#DESTROY#;$G7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$H7=bless({$v,$G7},$z);$I7=q#ni 'ni:/' . ref shift#;$J7=bless({$v,$I7},$z);$K7={$F7,$H7,$h5,$J7};$L7=q#/lib/instance.b#;$M7=bless({$g5,$E7,$Z5,$q,$c6,$q,$d6,$K7,$J,$L7},$m6);$N7=[$M7];$O7=bless({$g5,$C7,$J,$D7,$S5,$N7},$N5);$P7={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$r7,1,$w5,1,$o6,1,$x5,1,$s7,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$m6,1,$F5,1,$v6,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$Q7=q#/lib/behavior#;$R7={};$S7=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$T7=bless({$v,$S7},$z);$U7={$e,$T7};$V7=q#/lib/documentable.b#;$W7=bless({$g5,$R7,$Z5,$q,$c6,$q,$d6,$U7,$J,$V7},$m6);$X7=[$O7,$W7];$Y7=bless({$g5,$P7,$J,$Q7,$S5,$X7},$w5);$Z7={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$o6,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$c8=q#/lib/definition.b#;$d8={};$e8=q#def#;$f8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$g8=bless({$v,$f8},$z);$h8={$e8,$g8};$i8=q#/lib/definition_def.b#;$j8=bless({$g5,$d8,$Z5,$q,$c6,$q,$d6,$h8,$J,$i8},$m6);$k8={};$l8=q#ro#;$m8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$n8=bless({$v,$m8},$z);$o8=q#rw#;$p8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$q8=bless({$v,$p8},$z);$r8={$l8,$n8,$o8,$q8};$s8=q#/lib/accessor.b#;$t8=bless({$g5,$k8,$Z5,$q,$c6,$q,$d6,$r8,$J,$s8},$m6);$u8={};$v8=q#(""#;$w8=q#shift->name#;$x8=bless({$v,$w8},$z);$y8={$v8,$x8};$z8=q#/lib/name_as_string.b#;$A8=bless({$g5,$u8,$Z5,$q,$c6,$q,$d6,$y8,$J,$z8},$m6);$B8={};$C8=q#(eq#;$D8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$E8=bless({$v,$D8},$z);$F8={$C8,$E8};$G8=q#/lib/ref_eq.b#;$H8=bless({$g5,$B8,$Z5,$q,$c6,$q,$d6,$F8,$J,$G8},$m6);$I8={};$J8=q#defdata#;$K8=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$L8=bless({$v,$K8},$z);$M8={$J8,$L8};$N8=q#/lib/definition_defdata.b#;$O8=bless({$g5,$I8,$Z5,$q,$c6,$q,$d6,$M8,$J,$N8},$m6);$P8=[$j8,$t8,$A8,$H8,$O8];$Q8=bless({$g5,$Z7,$J,$c8,$S5,$P8},$o6);$R8=[$S6,$d7,$O7,$Y7,$Q8];$S8=bless({$g5,$V5,$J,$W5,$S5,$R8},$M5);$T8={};$U8=q#new#;$V8=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$W8=bless({$v,$V8},$z);$X8={$U8,$W8};$Y8=q#/lib/instantiable.b#;$Z8=bless({$g5,$T8,$d6,$X8,$J,$Y8},$m6);$c9={};$d9=q#child#;$e9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$f9=bless({$v,$e9},$z);$g9={$d9,$f9};$h9=q#/lib/subclass.b#;$i9=bless({$g5,$c9,$Z5,$q,$c6,$q,$d6,$g9,$J,$h9},$m6);$j9=[$S8,$Z8,$d7,$S8,$i9];$k9=bless({$g5,$R5,$J,$K,$S5,$j9},$i5);$l9=q#ni:/class.c#;$m9={$i5,1,$P5,1};$n9=q#/class.c#;$o9={$i5,1,$M5,1,$P5,1};$p9=q#/module.c#;$q9={$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$M5,1,$N5,1,$P5,1,$Q5,1};$r9=q#/object.c#;$s9=[$k9];$t9=bless({$g5,$q9,$J,$r9,$S5,$s9},$T5);$u9={$i5,1,$w5,1,$x5,1,$y5,1,$F5,1,$G5,1,$M5,1,$P5,1};$v9=q#/lib/behavior.c#;$w9=[$t9];$x9=bless({$g5,$u9,$J,$v9,$S5,$w9},$T5);$y9=[$t9,$Z8,$x9];$z9=bless({$g5,$o9,$J,$p9,$S5,$y9},$T5);$A9=[$z9];$B9=bless({$g5,$m9,$J,$n9,$S5,$A9},$T5);$C9=q#ni:/io/buffer#;$D9={$e7,1};$E9={$e7,1,$f7,1,$g7,1,$h7,1,$i7,1,$j7,1,$k7,1,$l7,1,$m7,1,$n7,1};$F9=q#/io/object#;$G9={};$H9=q#(bool#;$I9=[];$J9=bless({$t,$I9,$v,1,$x,$y},$z);$K9={$H9,$J9};$L9=q#/io/object_ops.b#;$M9=bless({$g5,$G9,$Z5,$q,$c6,$q,$d6,$K9,$J,$L9},$m6);$N9={};$O9=q#die#;$P9=[];$Q9=q#shift; die join " ", @_#;$R9=bless({$t,$P9,$v,$Q9,$x,$y},$z);$S9=q#io_check#;$T9=[];$U9=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$V9=bless({$t,$T9,$v,$U9,$x,$y},$z);$W9=q#io_check_defined#;$X9=[];$Y9=q#shift->io_check(sub {defined shift}, @_)#;$Z9=bless({$t,$X9,$v,$Y9,$x,$y},$z);$ca=q#io_check_true#;$da=[];$ea=q#shift->io_check(sub {shift}, @_)#;$fa=bless({$t,$da,$v,$ea,$x,$y},$z);$ga={$O9,$R9,$S9,$V9,$W9,$Z9,$ca,$fa};$ha=q#/io/object_checks.b#;$ia=bless({$g5,$N9,$Z5,$q,$c6,$q,$d6,$ga,$J,$ha},$m6);$ja={};$ka=q#(+#;$la=[];$ma=q#ni('ni:/io/cat')->new(@_[0, 1])#;$na=bless({$t,$la,$v,$ma,$x,$y},$z);$oa={$ka,$na};$pa=q#/io/object_constructors.b#;$qa=bless({$g5,$ja,$Z5,$q,$c6,$q,$d6,$oa,$J,$pa},$m6);$ra={};$sa=q#read_all#;$ta=[];$ua=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$va=bless({$t,$ta,$v,$ua,$x,$y},$z);$wa=q#write_all#;$xa=[];$ya=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$za=bless({$t,$xa,$v,$ya,$x,$y},$z);$Aa={$sa,$va,$wa,$za};$Ba=q#/io/object_memory.b#;$Ca=bless({$g5,$ra,$Z5,$q,$c6,$q,$d6,$Aa,$J,$Ba},$m6);$Da={};$Ea=q#connect_sync#;$Fa=[];$Ga=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$Ha=bless({$t,$Fa,$v,$Ga,$x,$y},$z);$Ia=q#into_sync#;$Ja=[];$Ka=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$La=bless({$t,$Ja,$v,$Ka,$x,$y},$z);$Ma={$Ea,$Ha,$Ia,$La};$Na=q#/io/object_transfer_sync.b#;$Oa=bless({$g5,$Da,$Z5,$q,$c6,$q,$d6,$Ma,$J,$Na},$m6);$Pa={};$Qa=q#connect_async#;$Ra=[];$Sa=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$Ta=bless({$t,$Ra,$v,$Sa,$x,$y},$z);$Ua=q#into_async#;$Va=[];$Wa=q#ni('ni:/io/transfer_async')->new(@_)->run#;$Xa=bless({$t,$Va,$v,$Wa,$x,$y},$z);$Ya={$Qa,$Ta,$Ua,$Xa};$Za=q#/io/object_transfer_async.b#;$cb=bless({$g5,$Pa,$Z5,$q,$c6,$q,$d6,$Ya,$J,$Za},$m6);$db=[$O7,$M9,$ia,$qa,$Ca,$Oa,$cb,$cb,$Oa,$cb,$Oa];$eb=bless({$g5,$E9,$J,$F9,$S5,$db},$q5);$fb={};$gb=[];$hb=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$ib=bless({$t,$gb,$v,$hb,$x,$y},$z);$jb={$W6,$ib};$kb=q#/io/buffer_init.b#;$lb=bless({$g5,$fb,$Z5,$q,$c6,$q,$d6,$jb,$J,$kb},$m6);$mb={};$nb=q#read#;$ob=[];$pb=q#my $self = shift;
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
}#;$qb=bless({$t,$ob,$v,$pb,$x,$y},$z);$rb=q#read_capacity#;$sb=[];$tb=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$ub=bless({$t,$sb,$v,$tb,$x,$y},$z);$vb=q#write#;$wb=[];$xb=q#my $self = shift;
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
}#;$yb=bless({$t,$wb,$v,$xb,$x,$y},$z);$zb=q#write_capacity#;$Ab=[];$Bb=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Cb=bless({$t,$Ab,$v,$Bb,$x,$y},$z);$Db={$nb,$qb,$rb,$ub,$vb,$yb,$zb,$Cb};$Eb=q#/io/buffer_io.b#;$Fb=bless({$g5,$mb,$Z5,$q,$c6,$q,$d6,$Db,$J,$Eb},$m6);$Gb=[$eb,$lb,$Fb];$Hb=bless({$g5,$D9,$J,$p1,$S5,$Gb},$j5);$Ib=q#ni:/io/buffer.c#;$Jb={$j5,1};$Kb=q#/io/buffer.c#;$Lb={$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1};$Mb=q#/io/object.c#;$Nb={};$Ob=q#def_transfer_method#;$Pb=[];$Qb=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Rb=bless({$t,$Pb,$v,$Qb,$x,$y},$z);$Sb={$Ob,$Rb};$Tb=q#/io/object.c_transfer_def.b#;$Ub=bless({$g5,$Nb,$Z5,$q,$c6,$q,$d6,$Sb,$J,$Tb},$m6);$Vb=[$t9,$Ub];$Wb=bless({$g5,$Lb,$J,$Mb,$S5,$Vb},$T5);$Xb=[$Wb];$Yb=bless({$g5,$Jb,$J,$Kb,$S5,$Xb},$T5);$Zb=q#ni:/io/buffer_init.b#;$cc=q#ni:/io/buffer_io.b#;$dc=q#ni:/io/cat#;$ec={$f7,1};$fc={};$gc=[];$hc=q#shift; +{fs => [@_]}#;$ic=bless({$t,$gc,$v,$hc,$x,$y},$z);$jc={$W6,$ic};$kc=q#/io/cat_init.b#;$lc=bless({$g5,$fc,$Z5,$q,$c6,$q,$d6,$jc,$J,$kc},$m6);$mc={};$nc=[];$oc=q#my $fs = shift->{fs};
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
$total_read;#;$pc=bless({$t,$nc,$v,$oc,$x,$y},$z);$qc={$nb,$pc};$rc=q#/io/cat_read.b#;$sc=bless({$g5,$mc,$Z5,$q,$c6,$q,$d6,$qc,$J,$rc},$m6);$tc=[$eb,$lc,$sc];$uc=bless({$g5,$ec,$J,$C1,$S5,$tc},$k5);$vc=q#ni:/io/cat.c#;$wc={$k5,1};$xc=q#/io/cat.c#;$yc=[$Wb];$zc=bless({$g5,$wc,$J,$xc,$S5,$yc},$T5);$Ac=q#ni:/io/cat_init.b#;$Bc=q#ni:/io/cat_read.b#;$Cc=q#ni:/io/exec#;$Dc={$g7,1};$Ec={};$Fc=q#argv#;$Gc=[];$Hc=q#shift->{'argv'}#;$Ic=bless({$t,$Gc,$v,$Hc,$x,$y},$z);$Jc={$Fc,$Ic};$Kc=q#/io/exec_ro.b#;$Lc=bless({$g5,$Ec,$Z5,$q,$c6,$q,$d6,$Jc,$J,$Kc},$m6);$Mc={};$Nc=[];$Oc=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Pc=bless({$t,$Nc,$v,$Oc,$x,$y},$z);$Qc={$W6,$Pc};$Rc=q#/io/exec_init.b#;$Sc=bless({$g5,$Mc,$Z5,$q,$c6,$q,$d6,$Qc,$J,$Rc},$m6);$Tc={};$Uc=q#connect#;$Vc=[];$Wc=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Xc=bless({$t,$Vc,$v,$Wc,$x,$y},$z);$Yc=q#in_pipe#;$Zc=[];$cd=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$dd=bless({$t,$Zc,$v,$cd,$x,$y},$z);$ed=q#out_pipe#;$fd=[];$gd=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$hd=bless({$t,$fd,$v,$gd,$x,$y},$z);$id=q#setup_stdio#;$jd=[];$kd=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$ld=bless({$t,$jd,$v,$kd,$x,$y},$z);$md={$Uc,$Xc,$Yc,$dd,$ed,$hd,$id,$ld};$nd=q#/io/exec_io_setup.b#;$od=bless({$g5,$Tc,$Z5,$q,$c6,$q,$d6,$md,$J,$nd},$m6);$pd={};$qd=q#binds_fd#;$rd=[];$sd=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$td=bless({$t,$rd,$v,$sd,$x,$y},$z);$ud=q#fd#;$vd=[];$wd=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$xd=bless({$t,$vd,$v,$wd,$x,$y},$z);$yd=q#stderr#;$zd=[];$Ad=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Bd=bless({$t,$zd,$v,$Ad,$x,$y},$z);$Cd=q#stdin#;$Dd=[];$Ed=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Fd=bless({$t,$Dd,$v,$Ed,$x,$y},$z);$Gd=q#stdout#;$Hd=[];$Id=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Jd=bless({$t,$Hd,$v,$Id,$x,$y},$z);$Kd={$qd,$td,$ud,$xd,$yd,$Bd,$Cd,$Fd,$Gd,$Jd};$Ld=q#/io/exec_io_accessors.b#;$Md=bless({$g5,$pd,$Z5,$q,$c6,$q,$d6,$Kd,$J,$Ld},$m6);$Nd={};$Od=q#env#;$Pd=[];$Qd=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Rd=bless({$t,$Pd,$v,$Qd,$x,$y},$z);$Sd={$Od,$Rd};$Td=q#/io/exec_env.b#;$Ud=bless({$g5,$Nd,$Z5,$q,$c6,$q,$d6,$Sd,$J,$Td},$m6);$Vd={};$Wd=q#exec#;$Xd=[];$Yd=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Zd=bless({$t,$Xd,$v,$Yd,$x,$y},$z);$ce=q#fork#;$de=[];$ee=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$fe=bless({$t,$de,$v,$ee,$x,$y},$z);$ge=q#move_fds#;$he=[];$ie=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$je=bless({$t,$he,$v,$ie,$x,$y},$z);$ke={$Wd,$Zd,$ce,$fe,$ge,$je};$le=q#/io/exec_fork.b#;$me=bless({$g5,$Vd,$Z5,$q,$c6,$q,$d6,$ke,$J,$le},$m6);$ne=[$eb,$Lc,$Sc,$od,$Md,$Ud,$me];$oe=bless({$g5,$Dc,$J,$P1,$S5,$ne},$l5);$pe=q#ni:/io/exec.c#;$qe={$l5,1};$re=q#/io/exec.c#;$se=[$Wb];$te=bless({$g5,$qe,$J,$re,$S5,$se},$T5);$ue=q#ni:/io/exec_env.b#;$ve=q#ni:/io/exec_fork.b#;$we=q#ni:/io/exec_init.b#;$xe=q#ni:/io/exec_io_accessors.b#;$ye=q#ni:/io/exec_io_setup.b#;$ze=q#ni:/io/exec_ro.b#;$Ae=q#ni:/io/fd#;$Be={$h7,1};$Ce=q#read_fd_mask#;$De={};$Ee=[];$Fe=q#shift->{'fd'}#;$Ge=bless({$t,$Ee,$v,$Fe,$x,$y},$z);$He={$ud,$Ge};$Ie=q#/io/fd_readers.b#;$Je=bless({$g5,$De,$Z5,$q,$c6,$q,$d6,$He,$J,$Ie},$m6);$Ke={};$Le=[];$Me=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Ne=bless({$t,$Le,$v,$Me,$x,$y},$z);$Oe={$W6,$Ne};$Pe=q#/io/fd_init.b#;$Qe=bless({$g5,$Ke,$Z5,$q,$c6,$q,$d6,$Oe,$J,$Pe},$m6);$Re={};$Se=q#be#;$Te=[];$Ue=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Ve=bless({$t,$Te,$v,$Ue,$x,$y},$z);$We={$Se,$Ve};$Xe=q#/io/fd_shell.b#;$Ye=bless({$g5,$Re,$Z5,$q,$c6,$q,$d6,$We,$J,$Xe},$m6);$Ze={};$cf=q#cloexec#;$df=[];$ef=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$ff=bless({$t,$df,$v,$ef,$x,$y},$z);$gf=q#fcntl_flag#;$hf=[];$if=q#my ($self, $flag, $value) = @_;
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
}#;$jf=bless({$t,$hf,$v,$if,$x,$y},$z);$kf=q#nonblock#;$lf=[];$mf=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$nf=bless({$t,$lf,$v,$mf,$x,$y},$z);$of={$cf,$ff,$gf,$jf,$kf,$nf};$pf=q#/io/fd_fcntl.b#;$qf=bless({$g5,$Ze,$Z5,$q,$c6,$q,$d6,$of,$J,$pf},$m6);$rf={};$sf=[];$tf=q#shift->close#;$uf=bless({$t,$sf,$v,$tf,$x,$y},$z);$vf=q#close#;$wf=[];$xf=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$yf=bless({$t,$wf,$v,$xf,$x,$y},$z);$zf={$vf,$yf};$Af=q#/io/fd_gc.b#;$Bf=bless({$g5,$rf,$Z5,$q,$c6,$uf,$d6,$zf,$J,$Af},$m6);$Cf={};$Df=q#close_perl_ios#;$Ef=[];$Ff=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Gf=bless({$t,$Ef,$v,$Ff,$x,$y},$z);$Hf=[];$If=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Jf=bless({$t,$Hf,$v,$If,$x,$y},$z);$Kf=[];$Lf=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Mf=bless({$t,$Kf,$v,$Lf,$x,$y},$z);$Nf={$Df,$Gf,$nb,$Jf,$vb,$Mf};$Of=q#/io/fd_perlio.b#;$Pf=bless({$g5,$Cf,$Z5,$q,$c6,$q,$d6,$Nf,$J,$Of},$m6);$Qf=[$eb,$Je,$Qe,$Ye,$qf,$Bf,$Pf];$Rf=q#write_fd_mask#;$Sf=bless({$g5,$Be,$J,$e2,$Ce,$y,$S5,$Qf,$Rf,$y},$m5);$Tf=[];$Uf=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Vf=bless({$t,$Tf,$v,$Uf,$x,$y},$z);$Wf=q#ni:/io/fd.c#;$Xf={$m5,1};$Yf=q#/io/fd.c#;$Zf={};$cg=q#clear_fd#;$dg=[];$eg=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$fg=bless({$t,$dg,$v,$eg,$x,$y},$z);$gg=q#read_fd#;$hg=[];$ig=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$jg=bless({$t,$hg,$v,$ig,$x,$y},$z);$kg=q#select#;$lg=[];$mg=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$ng=bless({$t,$lg,$v,$mg,$x,$y},$z);$og=q#write_fd#;$pg=[];$qg=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$rg=bless({$t,$pg,$v,$qg,$x,$y},$z);$sg={$cg,$fg,$gg,$jg,$kg,$ng,$og,$rg};$tg=q#/io/fd.c_selector.b#;$ug=bless({$g5,$Zf,$Z5,$Vf,$c6,$q,$d6,$sg,$J,$tg},$m6);$vg=[$Wb,$ug];$wg=bless({$g5,$Xf,$J,$Yf,$S5,$vg},$T5);$xg=q#ni:/io/fd.c_selector.b#;$yg=q#ni:/io/fd_fcntl.b#;$zg=q#ni:/io/fd_gc.b#;$Ag=q#ni:/io/fd_init.b#;$Bg=q#ni:/io/fd_perlio.b#;$Cg=q#ni:/io/fd_readers.b#;$Dg=q#ni:/io/fd_shell.b#;$Eg=q#ni:/io/file#;$Fg={$i7,1};$Gg={};$Hg=[];$Ig=q#shift->{'name'}#;$Jg=bless({$t,$Hg,$v,$Ig,$x,$y},$z);$Kg={$J,$Jg};$Lg=q#/io/file_readers.b#;$Mg=bless({$g5,$Gg,$Z5,$q,$c6,$q,$d6,$Kg,$J,$Lg},$m6);$Ng={};$Og=q#mode#;$Pg=[];$Qg=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$Rg=bless({$t,$Pg,$v,$Qg,$x,$y},$z);$Sg={$Og,$Rg};$Tg=q#/io/file_accessors.b#;$Ug=bless({$g5,$Ng,$Z5,$q,$c6,$q,$d6,$Sg,$J,$Tg},$m6);$Vg={};$Wg=[];$Xg=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Yg=bless({$t,$Wg,$v,$Xg,$x,$y},$z);$Zg={$W6,$Yg};$ch=q#/io/file_init.b#;$dh=bless({$g5,$Vg,$Z5,$q,$c6,$q,$d6,$Zg,$J,$ch},$m6);$eh={};$fh=q#(-X#;$gh=[];$hh=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$ih=bless({$t,$gh,$v,$hh,$x,$y},$z);$jh=q#mv#;$kh=[];$lh=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$mh=bless({$t,$kh,$v,$lh,$x,$y},$z);$nh=q#rm#;$oh=[];$ph=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$qh=bless({$t,$oh,$v,$ph,$x,$y},$z);$rh={$fh,$ih,$jh,$mh,$nh,$qh};$sh=q#/io/file_fns.b#;$th=bless({$g5,$eh,$Z5,$q,$c6,$q,$d6,$rh,$J,$sh},$m6);$uh={};$vh=q#atomic_update#;$wh=[];$xh=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$yh=bless({$t,$wh,$v,$xh,$x,$y},$z);$zh={$vh,$yh};$Ah=q#/io/file_update.b#;$Bh=bless({$g5,$uh,$Z5,$q,$c6,$q,$d6,$zh,$J,$Ah},$m6);$Ch={};$Dh=[];$Eh=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Fh=bless({$t,$Dh,$v,$Eh,$x,$y},$z);$Gh=q#r#;$Hh=[];$Ih=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Jh=bless({$t,$Hh,$v,$Ih,$x,$y},$z);$Kh=[];$Lh=q#shift->r->read(@_)#;$Mh=bless({$t,$Kh,$v,$Lh,$x,$y},$z);$Nh=q#w#;$Oh=[];$Ph=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Qh=bless({$t,$Oh,$v,$Ph,$x,$y},$z);$Rh=[];$Sh=q#shift->w->write(@_)#;$Th=bless({$t,$Rh,$v,$Sh,$x,$y},$z);$Uh={$vf,$Fh,$Gh,$Jh,$nb,$Mh,$Nh,$Qh,$vb,$Th};$Vh=q#/io/file_io.b#;$Wh=bless({$g5,$Ch,$Z5,$q,$c6,$q,$d6,$Uh,$J,$Vh},$m6);$Xh=[$eb,$Mg,$Ug,$dh,$th,$Bh,$Wh];$Yh=bless({$g5,$Fg,$J,$A2,$S5,$Xh},$n5);$Zh=q#ni:/io/file.c#;$ci={$n5,1};$di=q#/io/file.c#;$ei=[$Wb];$fi=bless({$g5,$ci,$J,$di,$S5,$ei},$T5);$gi=q#ni:/io/file_accessors.b#;$hi=q#ni:/io/file_fns.b#;$ii=q#ni:/io/file_init.b#;$ji=q#ni:/io/file_io.b#;$ki=q#ni:/io/file_readers.b#;$li=q#ni:/io/file_update.b#;$mi=q#ni:/io/file_update_fd#;$ni={$j7,1};$oi={};$pi=[];$qi=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$ri=bless({$t,$pi,$v,$qi,$x,$y},$z);$si={$W6,$ri};$ti=q#/io/file_update_fd_init.b#;$ui=bless({$g5,$oi,$Z5,$q,$c6,$q,$d6,$si,$J,$ti},$m6);$vi={};$wi=[];$xi=bless({$t,$wi,$v,$tf,$x,$y},$z);$yi=[];$zi=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Ai=bless({$t,$yi,$v,$zi,$x,$y},$z);$Bi={$vf,$Ai};$Ci=q#/io/file_update_fd_gc.b#;$Di=bless({$g5,$vi,$Z5,$q,$c6,$xi,$d6,$Bi,$J,$Ci},$m6);$Ei=[$eb,$Je,$qf,$Pf,$ui,$Di];$Fi=bless({$g5,$ni,$J,$G2,$S5,$Ei},$o5);$Gi=q#ni:/io/file_update_fd.c#;$Hi={$o5,1};$Ii=q#/io/file_update_fd.c#;$Ji=[$Wb];$Ki=bless({$g5,$Hi,$J,$Ii,$S5,$Ji},$T5);$Li=q#ni:/io/file_update_fd_gc.b#;$Mi=q#ni:/io/file_update_fd_init.b#;$Ni=q#ni:/io/named_io_fns.b#;$Oi={};$Pi=q#fcntl#;$Qi=[];$Ri=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Si=bless({$t,$Qi,$v,$Ri,$x,$y},$z);$Ti=[];$Ui=q#CORE::fork#;$Vi=bless({$t,$Ti,$v,$Ui,$x,$y},$z);$Wi=q#open2#;$Xi=[];$Yi=q#CORE::open $_[0], $_[1]#;$Zi=bless({$t,$Xi,$v,$Yi,$x,$y},$z);$cj=q#rename#;$dj=[];$ej=q#CORE::rename $_[0], $_[1]#;$fj=bless({$t,$dj,$v,$ej,$x,$y},$z);$gj=q#unlink#;$hj=[];$ij=q#CORE::unlink @_#;$jj=bless({$t,$hj,$v,$ij,$x,$y},$z);$kj=q#waitpid#;$lj=[];$mj=q#CORE::waitpid $_[0], $_[1]#;$nj=bless({$t,$lj,$v,$mj,$x,$y},$z);$oj={$Pi,$Si,$ce,$Vi,$Wi,$Zi,$cj,$fj,$gj,$jj,$kj,$nj};$pj=q#/io/named_io_fns.b#;$qj=bless({$g5,$Oi,$Z5,$q,$c6,$q,$d6,$oj,$J,$pj},$m6);$rj=q#main#;$sj=q#ni:/io/null#;$tj={$k7,1};$uj=q#/io/null#;$vj={};$wj=[];$xj=q#+{fd => undef}#;$yj=bless({$t,$wj,$v,$xj,$x,$y},$z);$zj={$W6,$yj};$Aj=q#/io/null_init.b#;$Bj=bless({$g5,$vj,$Z5,$q,$c6,$q,$d6,$zj,$J,$Aj},$m6);$Cj={};$Dj=[];$Ej=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Fj=bless({$t,$Dj,$v,$Ej,$x,$y},$z);$Gj=[];$Hj=q#shift->fd->read(@_)#;$Ij=bless({$t,$Gj,$v,$Hj,$x,$y},$z);$Jj=[];$Kj=q#shift->fd->write(@_)#;$Lj=bless({$t,$Jj,$v,$Kj,$x,$y},$z);$Mj={$ud,$Fj,$nb,$Ij,$vb,$Lj};$Nj=q#/io/null_io.b#;$Oj=bless({$g5,$Cj,$Z5,$q,$c6,$q,$d6,$Mj,$J,$Nj},$m6);$Pj=[$eb,$Bj,$Oj];$Qj=bless({$g5,$tj,$J,$uj,$S5,$Pj},$p5);$Rj=q#ni:/io/null.c#;$Sj={$p5,1};$Tj=q#/io/null.c#;$Uj=[$Wb];$Vj=bless({$g5,$Sj,$J,$Tj,$S5,$Uj},$T5);$Wj=q#ni:/io/null_init.b#;$Xj=q#ni:/io/null_io.b#;$Yj=q#ni:/io/object#;$Zj=q#ni:/io/object.c#;$ck=q#ni:/io/object.c_transfer_def.b#;$dk=q#ni:/io/object_checks.b#;$ek=q#ni:/io/object_constructors.b#;$fk=q#ni:/io/object_memory.b#;$gk=q#ni:/io/object_ops.b#;$hk=q#ni:/io/object_transfer_async.b#;$ik=q#ni:/io/object_transfer_sync.b#;$jk=q#ni:/io/pid#;$kk={$m7,1};$lk={};$mk=q#pid#;$nk=[];$ok=q#shift->{'pid'}#;$pk=bless({$t,$nk,$v,$ok,$x,$y},$z);$qk=q#status#;$rk=[];$sk=q#shift->{'status'}#;$tk=bless({$t,$rk,$v,$sk,$x,$y},$z);$uk={$mk,$pk,$qk,$tk};$vk=q#/io/pid_readers.b#;$wk=bless({$g5,$lk,$Z5,$q,$c6,$q,$d6,$uk,$J,$vk},$m6);$xk={};$yk=[];$zk=q#shift->await#;$Ak=bless({$t,$yk,$v,$zk,$x,$y},$z);$Bk=[];$Ck=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$Dk=bless({$t,$Bk,$v,$Ck,$x,$y},$z);$Ek={$W6,$Dk};$Fk=q#/io/pid_init.b#;$Gk=bless({$g5,$xk,$Z5,$q,$c6,$Ak,$d6,$Ek,$J,$Fk},$m6);$Hk={};$Ik=q#await#;$Jk=[];$Kk=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$Lk=bless({$t,$Jk,$v,$Kk,$x,$y},$z);$Mk=q#running#;$Nk=[];$Ok=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$Pk=bless({$t,$Nk,$v,$Ok,$x,$y},$z);$Qk={$Ik,$Lk,$Mk,$Pk};$Rk=q#/io/pid_wait.b#;$Sk=bless({$g5,$Hk,$Z5,$q,$c6,$q,$d6,$Qk,$J,$Rk},$m6);$Tk={};$Uk=[];$Vk=q#shift->stdout->read(@_)#;$Wk=bless({$t,$Uk,$v,$Vk,$x,$y},$z);$Xk=[];$Yk=q#shift->stdin->write(@_)#;$Zk=bless({$t,$Xk,$v,$Yk,$x,$y},$z);$cl={$nb,$Wk,$vb,$Zk};$dl=q#/io/pid_io.b#;$el=bless({$g5,$Tk,$Z5,$q,$c6,$q,$d6,$cl,$J,$dl},$m6);$fl={};$gl=[];$hl=q#$_[0]->{external_fds}{$_[1]}#;$il=bless({$t,$gl,$v,$hl,$x,$y},$z);$jl=[];$kl=q#shift->fd(2)#;$ll=bless({$t,$jl,$v,$kl,$x,$y},$z);$ml=[];$nl=q#shift->fd(0)#;$ol=bless({$t,$ml,$v,$nl,$x,$y},$z);$pl=[];$ql=q#shift->fd(1)#;$rl=bless({$t,$pl,$v,$ql,$x,$y},$z);$sl={$ud,$il,$yd,$ll,$Cd,$ol,$Gd,$rl};$tl=q#/io/pid_accessors.b#;$ul=bless({$g5,$fl,$Z5,$q,$c6,$q,$d6,$sl,$J,$tl},$m6);$vl=[$eb,$wk,$Gk,$Sk,$el,$ul];$wl=bless({$g5,$kk,$J,$f3,$S5,$vl},$r5);$xl=q#ni:/io/pid.c#;$yl={$r5,1};$zl=q#/io/pid.c#;$Al=[$Wb];$Bl=bless({$g5,$yl,$J,$zl,$S5,$Al},$T5);$Cl=q#ni:/io/pid_accessors.b#;$Dl=q#ni:/io/pid_init.b#;$El=q#ni:/io/pid_io.b#;$Fl=q#ni:/io/pid_readers.b#;$Gl=q#ni:/io/pid_wait.b#;$Hl=q#ni:/io/str#;$Il={$n7,1};$Jl=q#/io/str#;$Kl={};$Ll=q#data#;$Ml=[];$Nl=q#shift->{'data'}#;$Ol=bless({$t,$Ml,$v,$Nl,$x,$y},$z);$Pl=q#end#;$Ql=[];$Rl=q#shift->{'end'}#;$Sl=bless({$t,$Ql,$v,$Rl,$x,$y},$z);$Tl=q#start#;$Ul=[];$Vl=q#shift->{'start'}#;$Wl=bless({$t,$Ul,$v,$Vl,$x,$y},$z);$Xl={$Ll,$Ol,$Pl,$Sl,$Tl,$Wl};$Yl=q#/io/str_ro.b#;$Zl=bless({$g5,$Kl,$Z5,$q,$c6,$q,$d6,$Xl,$J,$Yl},$m6);$cm={};$dm=[];$em=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$fm=bless({$t,$dm,$v,$em,$x,$y},$z);$gm={$W6,$fm};$hm=q#/io/str_init.b#;$im=bless({$g5,$cm,$Z5,$q,$c6,$q,$d6,$gm,$J,$hm},$m6);$jm={};$km=[];$lm=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$mm=bless({$t,$km,$v,$lm,$x,$y},$z);$nm=q#remaining#;$om=[];$pm=q#my $self = shift; $$self{end} - $$self{start}#;$qm=bless({$t,$om,$v,$pm,$x,$y},$z);$rm=[];$sm=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$tm=bless({$t,$rm,$v,$sm,$x,$y},$z);$um={$nb,$mm,$nm,$qm,$vb,$tm};$vm=q#/io/str_io.b#;$wm=bless({$g5,$jm,$Z5,$q,$c6,$q,$d6,$um,$J,$vm},$m6);$xm=[$eb,$Zl,$im,$wm];$ym=bless({$g5,$Il,$J,$Jl,$S5,$xm},$s5);$zm=q#ni:/io/str.c#;$Am={$s5,1};$Bm=q#/io/str.c#;$Cm=[$Wb];$Dm=bless({$g5,$Am,$J,$Bm,$S5,$Cm},$T5);$Em=q#ni:/io/str_init.b#;$Fm=q#ni:/io/str_io.b#;$Gm=q#ni:/io/str_ro.b#;$Hm=q#ni:/io/transfer#;$Im={$o7,1,$p7,1,$q7,1};$Jm=q#/io/transfer#;$Km={$o7,1,$p7,1,$q7,1,$B7,1};$Lm=q#/semantic/task#;$Mm={};$Nm=[];$Om=q#shift->{'outcome'}#;$Pm=bless({$t,$Nm,$v,$Om,$x,$y},$z);$Qm={$r,$Pm};$Rm=q#/semantic/task_ro.b#;$Sm=bless({$g5,$Mm,$Z5,$q,$c6,$q,$d6,$Qm,$J,$Rm},$m6);$Tm={};$Um=q#failure#;$Vm=[];$Wm=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Xm=bless({$t,$Vm,$v,$Wm,$x,$y},$z);$Ym=q#success#;$Zm=[];$cn=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$dn=bless({$t,$Zm,$v,$cn,$x,$y},$z);$en={$Um,$Xm,$Ym,$dn};$fn=q#/semantic/task_outcome.b#;$gn=bless({$g5,$Tm,$Z5,$q,$c6,$q,$d6,$en,$J,$fn},$m6);$hn=[$O7,$Sm,$gn];$in=bless({$g5,$Km,$J,$Lm,$S5,$hn},$Q5);$jn={};$kn=[];$ln=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$mn=bless({$t,$kn,$v,$ln,$x,$y},$z);$nn=[];$on=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$pn=bless({$t,$nn,$v,$on,$x,$y},$z);$qn=[];$rn=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$sn=bless({$t,$qn,$v,$rn,$x,$y},$z);$tn={$nb,$pn,$vb,$sn};$un=q#/io/transfer_io_interop.b#;$vn=bless({$g5,$jn,$Z5,$mn,$c6,$q,$d6,$tn,$J,$un},$m6);$wn={};$xn=q#pressure#;$yn=[];$zn=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$An=bless({$t,$yn,$v,$zn,$x,$y},$z);$Bn=q#read_limit_throughput#;$Cn=[];$Dn=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$En=bless({$t,$Cn,$v,$Dn,$x,$y},$z);$Fn=q#throughput#;$Gn=[];$Hn=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$In=bless({$t,$Gn,$v,$Hn,$x,$y},$z);$Jn=q#write_limit_throughput#;$Kn=[];$Ln=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Mn=bless({$t,$Kn,$v,$Ln,$x,$y},$z);$Nn={$xn,$An,$Bn,$En,$Fn,$In,$Jn,$Mn};$On=q#/io/transfer_io_measurement.b#;$Pn=bless({$g5,$wn,$Z5,$q,$c6,$q,$d6,$Nn,$J,$On},$m6);$Qn=[$in,$vn,$Pn];$Rn=bless({$g5,$Im,$J,$Jm,$S5,$Qn},$t5);$Sn=[];$Tn=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Un=bless({$t,$Sn,$v,$Tn,$x,$y},$z);$Vn=q#ni:/io/transfer.c#;$Wn={$t5,1,$u5,1,$v5,1};$Xn=q#/io/transfer.c#;$Yn={$t5,1,$u5,1,$v5,1,$Q5,1};$Zn=q#/semantic/task.c#;$co=[$t9];$do=bless({$g5,$Yn,$J,$Zn,$S5,$co},$T5);$eo={};$fo={};$go=q#/io/transfer.c_into.b#;$ho=bless({$g5,$eo,$Z5,$Un,$c6,$q,$d6,$fo,$J,$go},$m6);$io=[$do,$ho];$jo=bless({$g5,$Wn,$J,$Xn,$S5,$io},$T5);$ko=q#ni:/io/transfer.c_into.b#;$lo=q#ni:/io/transfer_async#;$mo={$p7,1};$no=q#/io/transfer_async#;$oo={};$po=q#dest_io#;$qo=[];$ro=q#shift->{'dest_io'}#;$so=bless({$t,$qo,$v,$ro,$x,$y},$z);$to=q#id#;$uo=[];$vo=q#shift->{'id'}#;$wo=bless({$t,$uo,$v,$vo,$x,$y},$z);$xo=q#source_io#;$yo=[];$zo=q#shift->{'source_io'}#;$Ao=bless({$t,$yo,$v,$zo,$x,$y},$z);$Bo={$po,$so,$to,$wo,$xo,$Ao};$Co=q#/io/transfer_async_ro.b#;$Do=bless({$g5,$oo,$Z5,$q,$c6,$q,$d6,$Bo,$J,$Co},$m6);$Eo={};$Fo=[];$Go=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Ho=bless({$t,$Fo,$v,$Go,$x,$y},$z);$Io={$W6,$Ho};$Jo=q#/io/transfer_async_init.b#;$Ko=bless({$g5,$Eo,$Z5,$q,$c6,$q,$d6,$Io,$J,$Jo},$m6);$Lo={};$Mo=[];$No=q#ni('ni:/io/transfer_async')->track(shift)#;$Oo=bless({$t,$Mo,$v,$No,$x,$y},$z);$Po=[];$Qo=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Ro=bless({$t,$Po,$v,$Qo,$x,$y},$z);$So={};$To=q#/io/transfer_async_lifecycle.b#;$Uo=bless({$g5,$Lo,$Z5,$Oo,$c6,$Ro,$d6,$So,$J,$To},$m6);$Vo={};$Wo=q#run#;$Xo=[];$Yo=q#shift#;$Zo=bless({$t,$Xo,$v,$Yo,$x,$y},$z);$cp=q#run_async#;$dp=[];$ep=q#my $self = shift;
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

$self;#;$fp=bless({$t,$dp,$v,$ep,$x,$y},$z);$gp={$Wo,$Zo,$cp,$fp};$hp=q#/io/transfer_async_run.b#;$ip=bless({$g5,$Vo,$Z5,$q,$c6,$q,$d6,$gp,$J,$hp},$m6);$jp=[$Rn,$Do,$Ko,$Uo,$ip];$kp=q#tracked_transfers#;$lp={};$mp=q#transfer_id#;$np=bless({$g5,$mo,$J,$no,$S5,$jp,$kp,$lp,$mp,0},$u5);$op=[];$pp=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$qp=bless({$t,$op,$v,$pp,$x,$y},$z);$rp=q#ni:/io/transfer_async.c#;$sp={$u5,1};$tp=q#/io/transfer_async.c#;$up={};$vp=q#new_id#;$wp=[];$xp=q#++shift->{transfer_id}#;$yp=bless({$t,$wp,$v,$xp,$x,$y},$z);$zp=q#track#;$Ap=[];$Bp=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Cp=bless({$t,$Ap,$v,$Bp,$x,$y},$z);$Dp=q#untrack#;$Ep=[];$Fp=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Gp=bless({$t,$Ep,$v,$Fp,$x,$y},$z);$Hp={$vp,$yp,$zp,$Cp,$Dp,$Gp};$Ip=q#/io/transfer_async.c_tracker.b#;$Jp=bless({$g5,$up,$Z5,$qp,$c6,$q,$d6,$Hp,$J,$Ip},$m6);$Kp=[$jo,$Jp];$Lp=bless({$g5,$sp,$J,$tp,$S5,$Kp},$T5);$Mp=q#ni:/io/transfer_async.c_tracker.b#;$Np=q#ni:/io/transfer_async_init.b#;$Op=q#ni:/io/transfer_async_lifecycle.b#;$Pp=q#ni:/io/transfer_async_ro.b#;$Qp=q#ni:/io/transfer_async_run.b#;$Rp=q#ni:/io/transfer_io_interop.b#;$Sp=q#ni:/io/transfer_io_measurement.b#;$Tp=q#ni:/io/transfer_sync#;$Up={$q7,1};$Vp=q#/io/transfer_sync#;$Wp={};$Xp=[];$Yp=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Zp=bless({$t,$Xp,$v,$Yp,$x,$y},$z);$cq={$W6,$Zp};$dq=q#/io/transfer_sync_init.b#;$eq=bless({$g5,$Wp,$Z5,$q,$c6,$q,$d6,$cq,$J,$dq},$m6);$fq={};$gq=[];$hq=q#my $self = shift;
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
$self->success;#;$iq=bless({$t,$gq,$v,$hq,$x,$y},$z);$jq={$Wo,$iq};$kq=q#/io/transfer_sync_run.b#;$lq=bless({$g5,$fq,$Z5,$q,$c6,$q,$d6,$jq,$J,$kq},$m6);$mq=[$Rn,$eq,$lq];$nq=bless({$g5,$Up,$J,$Vp,$S5,$mq},$v5);$oq=q#ni:/io/transfer_sync.c#;$pq={$v5,1};$qq=q#/io/transfer_sync.c#;$rq=[$jo];$sq=bless({$g5,$pq,$J,$qq,$S5,$rq},$T5);$tq=q#ni:/io/transfer_sync_init.b#;$uq=q#ni:/io/transfer_sync_run.b#;$vq=q#ni:/lib/accessor.b#;$wq=q#ni:/lib/behavior#;$xq=q#ni:/lib/behavior.c#;$yq=q#ni:/lib/branch#;$zq={$o6,1};$Aq=q#/lib/branch#;$Bq={};$Cq=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Dq=bless({$v,$Cq},$z);$Eq={$W6,$Dq};$Fq=q#/lib/branch_init.b#;$Gq=bless({$g5,$Bq,$Z5,$q,$c6,$q,$d6,$Eq,$J,$Fq},$m6);$Hq=[$Y7,$u6,$n6,$Gq,$Q8];$Iq=bless({$g5,$zq,$J,$Aq,$S5,$Hq},$x5);$Jq=q#ni:/lib/branch.b#;$Kq=q#ni:/lib/branch.c#;$Lq={$x5,1};$Mq=q#/lib/branch.c#;$Nq=[$x9];$Oq=bless({$g5,$Lq,$J,$Mq,$S5,$Nq},$T5);$Pq=q#ni:/lib/branch_init.b#;$Qq=q#ni:/lib/class_init.b#;$Rq=q#ni:/lib/dataslice#;$Sq={$s7,1};$Tq=q#/lib/dataslice#;$Uq={};$Vq=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Wq=bless({$v,$Vq},$z);$Xq={$W6,$Wq};$Yq=q#/lib/dataslice_init.b#;$Zq=bless({$g5,$Uq,$Z5,$q,$c6,$q,$d6,$Xq,$J,$Yq},$m6);$cr={};$dr=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$er=bless({$v,$dr},$z);$fr={$h6,$er};$gr=q#/lib/dataslice_apply.b#;$hr=bless({$g5,$cr,$Z5,$q,$c6,$q,$d6,$fr,$J,$gr},$m6);$ir=[$Y7,$Zq,$hr];$jr=bless({$g5,$Sq,$J,$Tq,$S5,$ir},$y5);$kr=q#ni:/lib/dataslice.c#;$lr={$y5,1};$mr=q#/lib/dataslice.c#;$nr=[$x9];$or=bless({$g5,$lr,$J,$mr,$S5,$nr},$T5);$pr=q#ni:/lib/dataslice_apply.b#;$qr=q#ni:/lib/dataslice_init.b#;$rr=q#ni:/lib/definition.b#;$sr=q#ni:/lib/definition_def.b#;$tr=q#ni:/lib/definition_defdata.b#;$ur=q#ni:/lib/doc#;$vr={$L,1};$wr={};$xr=q#shift; +{name => shift, doc => []}#;$yr=bless({$v,$xr},$z);$zr={$W6,$yr};$Ar=q#/lib/doc_init.b#;$Br=bless({$g5,$wr,$Z5,$q,$c6,$q,$d6,$zr,$J,$Ar},$m6);$Cr={};$Dr=q#'ni.doc'#;$Er=bless({$v,$Dr},$z);$Fr={$x6,$Er};$Gr=q#/lib/doc_namespace.b#;$Hr=bless({$g5,$Cr,$Z5,$q,$c6,$q,$d6,$Fr,$J,$Gr},$m6);$Ir={};$Jr=q#AUTOLOAD#;$Kr=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map $self->fix_indentation($_), @_];
$self;#;$Lr=bless({$v,$Kr},$z);$Mr=q#fix_indentation#;$Nr=q#my ($self, $x) = @_;
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
join "\\n", @lines;#;$Or=bless({$v,$Nr},$z);$Pr={$Jr,$Lr,$Mr,$Or};$Qr=q#/lib/doc_define.b#;$Rr=bless({$g5,$Ir,$Z5,$q,$c6,$q,$d6,$Pr,$J,$Qr},$m6);$Sr={};$Tr=q#shift->referent#;$Ur=bless({$v,$Tr},$z);$Vr=q#referent#;$Wr=q#ni 'ni:' . shift->{name}#;$Xr=bless({$v,$Wr},$z);$Yr={$Pl,$Ur,$Vr,$Xr};$Zr=q#/lib/doc_end.b#;$cs=bless({$g5,$Sr,$Z5,$q,$c6,$q,$d6,$Yr,$J,$Zr},$m6);$ds={};$es=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$fs=bless({$v,$es},$z);$gs=q#linearized#;$hs=q#map @$_, @{shift->{doc}}#;$is=bless({$v,$hs},$z);$js=q#tests#;$ks=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$ls=bless({$v,$ks},$z);$ms={$J2,$fs,$gs,$is,$js,$ls};$ns=q#/lib/doc_test.b#;$os=bless({$g5,$ds,$Z5,$q,$c6,$q,$d6,$ms,$J,$ns},$m6);$ps=[$O7,$u6,$Br,$Hr,$Rr,$cs,$os];$qs=bless({$g5,$vr,$J,$G3,$S5,$ps},$z5);$rs=q#ni:/lib/doc.c#;$ss={$z5,1};$ts=q#/lib/doc.c#;$us=[$t9];$vs=bless({$g5,$ss,$J,$ts,$S5,$us},$T5);$ws=q#ni:/lib/doc_define.b#;$xs=q#ni:/lib/doc_end.b#;$ys=q#ni:/lib/doc_init.b#;$zs=q#ni:/lib/doc_namespace.b#;$As=q#ni:/lib/doc_test.b#;$Bs=q#ni:/lib/documentable.b#;$Cs=q#ni:/lib/fn#;$Ds={$z,1};$Es=q#/lib/fn#;$Fs={};$Gs=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Hs=bless({$v,$Gs,$x,$y},$z);$Is=q#compile#;$Js=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$Ks=bless({$v,$Js},$z);$Ls=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$Ms=bless({$v,$Ls,$x,$y},$z);$Ns={$Is,$Ks,$W6,$Ms};$Os=q#/lib/fn_init.b#;$Ps=bless({$g5,$Fs,$Z5,$q,$c6,$Hs,$d6,$Ns,$J,$Os},$m6);$Qs={};$Rs=[];$Ss=q#shift->{'annotations'}#;$Ts=bless({$t,$Rs,$v,$Ss,$x,$y},$z);$Us=[];$Vs=q#shift->{'code'}#;$Ws=bless({$t,$Us,$v,$Vs,$x,$y},$z);$Xs=q#eval_number#;$Ys=[];$Zs=q#shift->{'eval_number'}#;$ct=bless({$t,$Ys,$v,$Zs,$x,$y},$z);$dt=q#fn#;$et=[];$ft=q#shift->{'fn'}#;$gt=bless({$t,$et,$v,$ft,$x,$y},$z);$ht={$t,$Ts,$v,$Ws,$Xs,$ct,$dt,$gt};$it=q#/lib/fn_ro.b#;$jt=bless({$g5,$Qs,$Z5,$q,$c6,$q,$d6,$ht,$J,$it},$m6);$kt={};$lt=[];$mt=q#my $self = shift; "fn {$$self{code}}"#;$nt=bless({$t,$lt,$v,$mt,$x,$y},$z);$ot=[];$pt=bless({$t,$ot,$v,$D8,$x,$y},$z);$qt={$v8,$nt,$C8,$pt};$rt=q#/lib/fn_ops.b#;$st=bless({$g5,$kt,$Z5,$q,$c6,$q,$d6,$qt,$J,$rt},$m6);$tt={};$ut=q#serialize#;$vt=[];$wt=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$xt=bless({$t,$vt,$v,$wt,$x,$y},$z);$yt={$ut,$xt};$zt=q#/lib/fn_serialize.b#;$At=bless({$g5,$tt,$Z5,$q,$c6,$q,$d6,$yt,$J,$zt},$m6);$Bt=[$O7,$Z8,$Ps,$jt,$st,$At];$Ct=bless({$g5,$Ds,$J,$Es,$S5,$Bt},$A5);$Dt=[];$Et=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Ft=bless({$t,$Dt,$v,$Et,$x,$y},$z);$Gt=q#ni:/lib/fn.c#;$Ht={$A5,1};$It=q#/lib/fn.c#;$Jt={};$Kt=q#resolve_evals#;$Lt=[];$Mt=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Nt=bless({$t,$Lt,$v,$Mt,$x,$y},$z);$Ot={$Kt,$Nt};$Pt=q#/lib/fn.c_resolve_eval.b#;$Qt=bless({$g5,$Jt,$Z5,$Ft,$c6,$q,$d6,$Ot,$J,$Pt},$m6);$Rt=[$t9,$Qt];$St=bless({$g5,$Ht,$J,$It,$S5,$Rt},$T5);$Tt=q#ni:/lib/fn.c_resolve_eval.b#;$Ut=q#ni:/lib/fn_init.b#;$Vt=q#ni:/lib/fn_ops.b#;$Wt=q#ni:/lib/fn_ro.b#;$Xt=q#ni:/lib/fn_serialize.b#;$Yt=q#ni:/lib/future#;$Zt={$t7,1};$cu={};$du=[];$eu=bless({$t,$du,$v,$Om,$x,$y},$z);$fu=q#parents#;$gu=[];$hu=q#shift->{'parents'}#;$iu=bless({$t,$gu,$v,$hu,$x,$y},$z);$ju={$r,$eu,$fu,$iu};$ku=q#/lib/future_ro.b#;$lu=bless({$g5,$cu,$Z5,$q,$c6,$q,$d6,$ju,$J,$ku},$m6);$mu={};$nu=[];$ou=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$pu=bless({$t,$nu,$v,$ou,$x,$y},$z);$qu={$W6,$pu};$ru=q#/lib/future_init.b#;$su=bless({$g5,$mu,$Z5,$q,$c6,$q,$d6,$qu,$J,$ru},$m6);$tu={};$uu=q#decide#;$vu=[];$wu=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$xu=bless({$t,$vu,$v,$wu,$x,$y},$z);$yu=q#decided#;$zu=[];$Au=q#shift->{outcome}#;$Bu=bless({$t,$zu,$v,$Au,$x,$y},$z);$Cu=q#listener#;$Du=[];$Eu=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$Fu=bless({$t,$Du,$v,$Eu,$x,$y},$z);$Gu=q#v#;$Hu=[];$Iu=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$Ju=bless({$t,$Hu,$v,$Iu,$x,$y},$z);$Ku={$uu,$xu,$yu,$Bu,$Cu,$Fu,$Gu,$Ju};$Lu=q#/lib/future_state.b#;$Mu=bless({$g5,$tu,$Z5,$q,$c6,$q,$d6,$Ku,$J,$Lu},$m6);$Nu={};$Ou=q#and#;$Pu=[];$Qu=q#my $self   = $_[0];
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
$child;#;$Ru=bless({$t,$Pu,$v,$Qu,$x,$y},$z);$Su=q#flatmap#;$Tu=[];$Uu=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$Vu=bless({$t,$Tu,$v,$Uu,$x,$y},$z);$Wu=q#map#;$Xu=[];$Yu=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$Zu=bless({$t,$Xu,$v,$Yu,$x,$y},$z);$cv=q#or#;$dv=[];$ev=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$fv=bless({$t,$dv,$v,$ev,$x,$y},$z);$gv={$Ou,$Ru,$Su,$Vu,$Wu,$Zu,$cv,$fv};$hv=q#/lib/future_algebra.b#;$iv=bless({$g5,$Nu,$Z5,$q,$c6,$q,$d6,$gv,$J,$hv},$m6);$jv=[$O7,$lu,$su,$Mu,$iv];$kv=bless({$g5,$Zt,$J,$X3,$S5,$jv},$B5);$lv=q#ni:/lib/future.c#;$mv={$B5,1};$nv=q#/lib/future.c#;$ov=[$t9];$pv=bless({$g5,$mv,$J,$nv,$S5,$ov},$T5);$qv=q#ni:/lib/future_algebra.b#;$rv=q#ni:/lib/future_init.b#;$sv=q#ni:/lib/future_ro.b#;$tv=q#ni:/lib/future_state.b#;$uv=q#ni:/lib/gensym_generator_compact.b#;$vv={};$wv=q#gensym#;$xv=[];$yv=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$zv=bless({$t,$xv,$v,$yv,$x,$y},$z);$Av={$wv,$zv};$Bv=q#/lib/gensym_generator_compact.b#;$Cv=bless({$g5,$vv,$Z5,$q,$c6,$q,$d6,$Av,$J,$Bv},$m6);$Dv=q#ni:/lib/global_static_test.b#;$Ev={};$Fv=[];$Gv=q#ni('ni:/lib/test_case')->new(shift)#;$Hv=q#($)#;$Iv=bless({$t,$Fv,$v,$Gv,$x,$Hv},$z);$Jv=q#now#;$Kv=[];$Lv=q#ni('ni:/lib/test_value')->new(shift)#;$Mv=bless({$t,$Kv,$v,$Lv,$x,$Hv},$z);$Nv={$J2,$Iv,$Jv,$Mv};$Ov=q#/lib/global_static_test.b#;$Pv=bless({$g5,$Ev,$Z5,$q,$c6,$q,$d6,$Nv,$J,$Ov},$m6);$Qv=q#ni:/lib/image#;$Rv={$u7,1};$Sv={};$Tv=[];$Uv=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Vv=bless({$t,$Tv,$v,$Uv,$x,$y},$z);$Wv={$W6,$Vv};$Xv=q#/lib/image_init.b#;$Yv=bless({$g5,$Sv,$Z5,$q,$c6,$q,$d6,$Wv,$J,$Xv},$m6);$Zv={};$cw=q#boot_side_effect#;$dw=[];$ew=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$fw=bless({$t,$dw,$v,$ew,$x,$y},$z);$gw=q#finalizer#;$hw=[];$iw=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$jw=bless({$t,$hw,$v,$iw,$x,$y},$z);$kw=q#io#;$lw=[];$mw=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$nw=bless({$t,$lw,$v,$mw,$x,$y},$z);$ow=q#reconstruction#;$pw=[];$qw=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$rw=bless({$t,$pw,$v,$qw,$x,$y},$z);$sw=q#side_effect#;$tw=[];$uw=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$vw=bless({$t,$tw,$v,$uw,$x,$y},$z);$ww={$cw,$fw,$gw,$jw,$kw,$nw,$ow,$rw,$sw,$vw};$xw=q#/lib/image_quoting.b#;$yw=bless({$g5,$Zv,$Z5,$q,$c6,$q,$d6,$ww,$J,$xw},$m6);$zw={};$Aw=q#quote_code#;$Bw=[];$Cw=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Dw=bless({$t,$Bw,$v,$Cw,$x,$y},$z);$Ew={$Aw,$Dw};$Fw=q#/lib/quote_code_fail.b#;$Gw=bless({$g5,$zw,$Z5,$q,$c6,$q,$d6,$Ew,$J,$Fw},$m6);$Hw={};$Iw=q#quote_array#;$Jw=[];$Kw=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Lw=bless({$t,$Jw,$v,$Kw,$x,$y},$z);$Mw=q#quote_hash#;$Nw=[];$Ow=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Pw=bless({$t,$Nw,$v,$Ow,$x,$y},$z);$Qw=q#quote_scalar#;$Rw=[];$Sw=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Tw=bless({$t,$Rw,$v,$Sw,$x,$y},$z);$Uw=q#quote_scalar_ref#;$Vw=[];$Ww=q#'\\\\' . shift->quote(${$_[0]})#;$Xw=bless({$t,$Vw,$v,$Ww,$x,$y},$z);$Yw=q#quote_value#;$Zw=[];$cx=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$dx=bless({$t,$Zw,$v,$cx,$x,$y},$z);$ex={$Iw,$Lw,$Mw,$Pw,$Qw,$Tw,$Uw,$Xw,$Yw,$dx};$fx=q#/lib/quote_values.b#;$gx=bless({$g5,$Hw,$Z5,$q,$c6,$q,$d6,$ex,$J,$fx},$m6);$hx={};$ix=q#quote_blessed#;$jx=[];$kx=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$lx=bless({$t,$jx,$v,$kx,$x,$y},$z);$mx=q#quote_class#;$nx=[];$ox=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$px=bless({$t,$nx,$v,$ox,$x,$y},$z);$qx=q#quote_object#;$rx=[];$sx=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$tx=bless({$t,$rx,$v,$sx,$x,$y},$z);$ux={$ix,$lx,$mx,$px,$qx,$tx};$vx=q#/lib/quote_objects.b#;$wx=bless({$g5,$hx,$Z5,$q,$c6,$q,$d6,$ux,$J,$vx},$m6);$xx={};$yx=q#circular_arrayref#;$zx=[];$Ax=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Bx=bless({$t,$zx,$v,$Ax,$x,$y},$z);$Cx=q#circular_hashref#;$Dx=[];$Ex=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Fx=bless({$t,$Dx,$v,$Ex,$x,$y},$z);$Gx=q#is_circular#;$Hx=[];$Ix=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Jx=bless({$t,$Hx,$v,$Ix,$x,$y},$z);$Kx={$yx,$Bx,$Cx,$Fx,$Gx,$Jx};$Lx=q#/lib/quote_circular_addressed.b#;$Mx=bless({$g5,$xx,$Z5,$q,$c6,$q,$d6,$Kx,$J,$Lx},$m6);$Nx={};$Ox=q#address#;$Px=[];$Qx=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Rx=bless({$t,$Px,$v,$Qx,$x,$y},$z);$Sx=q#allocate_gensym#;$Tx=[];$Ux=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Vx=bless({$t,$Tx,$v,$Ux,$x,$y},$z);$Wx=q#circular_links#;$Xx=[];$Yx=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Zx=bless({$t,$Xx,$v,$Yx,$x,$y},$z);$cy=q#quote#;$dy=[];$ey=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$fy=bless({$t,$dy,$v,$ey,$x,$y},$z);$gy={$Ox,$Rx,$Sx,$Vx,$Wx,$Zx,$cy,$fy};$hy=q#/lib/quote_gensym_identity.b#;$iy=bless({$g5,$Nx,$Z5,$q,$c6,$q,$d6,$gy,$J,$hy},$m6);$jy=[$O7,$Yv,$yw,$Gw,$gx,$wx,$Mx,$iy,$Cv];$ky=bless({$g5,$Rv,$J,$h4,$S5,$jy},$C5);$ly=q#ni:/lib/image.c#;$my={$C5,1};$ny=q#/lib/image.c#;$oy=[$t9];$py=bless({$g5,$my,$J,$ny,$S5,$oy},$T5);$qy=q#ni:/lib/image_init.b#;$ry=q#ni:/lib/image_quoting.b#;$sy=q#ni:/lib/instance.b#;$ty=q#ni:/lib/instantiable.b#;$uy=q#ni:/lib/json.b#;$vy={};$wy=q#json_decode#;$xy=[];$yy=q#local $_;
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
wantarray ? @$r : $$r[0];#;$zy=bless({$t,$xy,$v,$yy,$x,$Hv},$z);$Ay=q#json_encode#;$By=[];$Cy=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Dy=bless({$t,$By,$v,$Cy,$x,$Hv},$z);$Ey=q#json_encode_pretty#;$Fy=[];$Gy=q#local $_;
my ($v, $indent) = @_;
$indent ||= 0;
my $spaces = ' ' x $indent;
return "$spaces\\[\\n"
     . join(",\\n", map ni::json_encode_pretty($_, $indent + 2), @$v)
     . "$spaces]" if 'ARRAY' eq ref $v;

return "$spaces\\{\\n"
     . join(",\\n", map "$spaces  " . ni::json_escape($_) . ":\\n"
                                   . ni::json_encode_pretty($$v{$_}, $indent + 2),
                       sort keys %$v)
     . "$spaces\\}" if 'HASH' eq ref $v;

$spaces . ni::json_encode($v);#;$Hy=bless({$t,$Fy,$v,$Gy,$x,$y},$z);$Iy=q#json_escape#;$Jy=[];$Ky=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$Ly=bless({$t,$Jy,$v,$Ky,$x,$Hv},$z);$My=q#json_unescape#;$Ny=[];$Oy=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$Py=bless({$t,$Ny,$v,$Oy,$x,$Hv},$z);$Qy=q#json_unescape_one#;$Ry=[];$Sy=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$Ty=bless({$t,$Ry,$v,$Sy,$x,$Hv},$z);$Uy={$wy,$zy,$Ay,$Dy,$Ey,$Hy,$Iy,$Ly,$My,$Py,$Qy,$Ty};$Vy=q#/lib/json.b#;$Wy=bless({$g5,$vy,$Z5,$q,$c6,$q,$d6,$Uy,$J,$Vy},$m6);$Xy=q#ni#;$Yy=q#ni:/lib/name_as_string.b#;$Zy=q#ni:/lib/named.b#;$cz=q#ni:/lib/named_in_ni.b#;$dz=q#ni:/lib/namespaced.b#;$ez=q#ni:/lib/ni#;$fz={$v7,1};$gz={};$hz=q#extend#;$iz=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$jz=bless({$v,$iz,$x,$y},$z);$kz=q#is_mutable#;$lz=q#$0 ne '-' && -w $0#;$mz=bless({$v,$lz,$x,$y},$z);$nz=q#modify#;$oz=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$pz=bless({$v,$oz,$x,$y},$z);$qz={$hz,$jz,$kz,$mz,$nz,$pz};$rz=q#/lib/ni_self.b#;$sz=bless({$g5,$gz,$Z5,$q,$c6,$q,$d6,$qz,$J,$rz},$m6);$tz={};$uz=q#--internal/+=#;$vz=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$wz=bless({$v,$vz,$x,$y},$z);$xz=q#--internal/eval#;$yz=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$zz=bless({$v,$yz,$x,$y},$z);$Az=q#--internal/image#;$Bz=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$Cz=bless({$v,$Bz,$x,$y},$z);$Dz=q#--internal/test#;$Ez=q#local $| = 1;
my $self   = shift;
my $failed = 0;
my @tests  = map ni($_)->tests, grep /^ni\\.doc:/, keys %{$$self{named}};
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
!!$failed;#;$Fz=bless({$v,$Ez,$x,$y},$z);$Gz=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$Hz=bless({$v,$Gz,$x,$y},$z);$Iz={$uz,$wz,$xz,$zz,$Az,$Cz,$Dz,$Fz,$Wo,$Hz};$Jz=q#/lib/ni_main.b#;$Kz=bless({$g5,$tz,$Z5,$q,$c6,$q,$d6,$Iz,$J,$Jz},$m6);$Lz={};$Mz=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$Nz=bless({$v,$Mz,$x,$y},$z);$Oz=q#resolver_for#;$Pz=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$Qz=bless({$v,$Pz,$x,$y},$z);$Rz={$L6,$Nz,$Oz,$Qz};$Sz=q#/lib/ni_resolver.b#;$Tz=bless({$g5,$Lz,$Z5,$q,$c6,$q,$d6,$Rz,$J,$Sz},$m6);$Uz={};$Vz=q#exists#;$Wz=q#exists $_[0]->{named}{$_[1]}#;$Xz=bless({$v,$Wz,$x,$y},$z);$Yz=q#quoted#;$Zz=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$cA=bless({$v,$Zz,$x,$y},$z);$dA={$Vz,$Xz,$Yz,$cA};$eA=q#/lib/ni_image.b#;$fA=bless({$g5,$Uz,$Z5,$q,$c6,$q,$d6,$dA,$J,$eA},$m6);$gA=[$O7,$sz,$Kz,$Tz,$fA];$hA=bless({$g5,$fz,$J,$p4,$S5,$gA},$D5);$iA=q#ni:/lib/ni.c#;$jA={$D5,1};$kA=q#/lib/ni.c#;$lA=[$t9];$mA=bless({$g5,$jA,$J,$kA,$S5,$lA},$T5);$nA=q#ni:/lib/ni_image.b#;$oA=q#ni:/lib/ni_main.b#;$pA=q#ni:/lib/ni_resolver.b#;$qA=q#ni:/lib/ni_self.b#;$rA=q#ni:/lib/ni_static_util.b#;$sA={};$tA=q#abbrev#;$uA=[];$vA=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$wA=bless({$t,$uA,$v,$vA,$x,$y},$z);$xA=q#dor#;$yA=[];$zA=q#defined $_[0] ? $_[0] : $_[1]#;$AA=bless({$t,$yA,$v,$zA,$x,$y},$z);$BA=q#indent#;$CA=[];$DA=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$EA=bless({$t,$CA,$v,$DA,$x,$y},$z);$FA=q#max#;$GA=[];$HA=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$IA=bless({$t,$GA,$v,$HA,$x,$y},$z);$JA=q#maxstr#;$KA=[];$LA=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$MA=bless({$t,$KA,$v,$LA,$x,$y},$z);$NA=q#mean#;$OA=[];$PA=q#sum(@_) / (@_ || 1)#;$QA=bless({$t,$OA,$v,$PA,$x,$y},$z);$RA=q#min#;$SA=[];$TA=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$UA=bless({$t,$SA,$v,$TA,$x,$y},$z);$VA=q#minstr#;$WA=[];$XA=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$YA=bless({$t,$WA,$v,$XA,$x,$y},$z);$ZA=q#sgr#;$cB=[];$dB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$eB=bless({$t,$cB,$v,$dB,$x,$y},$z);$fB=q#sr#;$gB=[];$hB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$iB=bless({$t,$gB,$v,$hB,$x,$y},$z);$jB=q#sum#;$kB=[];$lB=q#local $_; my $x = 0; $x += $_ for @_; $x#;$mB=bless({$t,$kB,$v,$lB,$x,$y},$z);$nB=q#swap#;$oB=[];$pB=q#@_[0, 1] = @_[1, 0]#;$qB=bless({$t,$oB,$v,$pB,$x,$y},$z);$rB={$tA,$wA,$xA,$AA,$BA,$EA,$FA,$IA,$JA,$MA,$NA,$QA,$RA,$UA,$VA,$YA,$ZA,$eB,$fB,$iB,$jB,$mB,$nB,$qB};$sB=q#/lib/ni_static_util.b#;$tB=bless({$g5,$sA,$Z5,$q,$c6,$q,$d6,$rB,$J,$sB},$m6);$uB=q#ni:/lib/perlbranch.b#;$vB=q#ni:/lib/quote_circular_addressed.b#;$wB=q#ni:/lib/quote_code_fail.b#;$xB=q#ni:/lib/quote_gensym_identity.b#;$yB=q#ni:/lib/quote_objects.b#;$zB=q#ni:/lib/quote_simple#;$AB={$w7,1};$BB={};$CB=[];$DB=q#+{}#;$EB=bless({$t,$CB,$v,$DB,$x,$y},$z);$FB={$W6,$EB};$GB=q#/lib/quote_simple_init.b#;$HB=bless({$g5,$BB,$Z5,$q,$c6,$q,$d6,$FB,$J,$GB},$m6);$IB={};$JB=[];$KB=bless({$t,$JB,$v,0,$x,$y},$z);$LB=[];$MB=q#shift->quote_value(shift)#;$NB=bless({$t,$LB,$v,$MB,$x,$y},$z);$OB={$Gx,$KB,$cy,$NB};$PB=q#/lib/quote_simple_quote.b#;$QB=bless({$g5,$IB,$Z5,$q,$c6,$q,$d6,$OB,$J,$PB},$m6);$RB=[$O7,$HB,$QB,$Gw,$gx,$wx];$SB=bless({$g5,$AB,$J,$A4,$S5,$RB},$E5);$TB=q#ni:/lib/quote_simple.c#;$UB={$E5,1};$VB=q#/lib/quote_simple.c#;$WB=[$t9];$XB=bless({$g5,$UB,$J,$VB,$S5,$WB},$T5);$YB=q#ni:/lib/quote_simple_init.b#;$ZB=q#ni:/lib/quote_simple_quote.b#;$cC=q#ni:/lib/quote_values.b#;$dC=q#ni:/lib/ref_eq.b#;$eC=q#ni:/lib/resolver.b#;$fC=q#ni:/lib/slice#;$gC={$m6,1};$hC=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$iC=bless({$v,$hC},$z);$jC=q#local $_;
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
$self;#;$kC=bless({$v,$jC},$z);$lC=q#lib/slice::apply#;$mC=q#lib/slice::apply_#;$nC={};$oC=q#apply_#;$pC={$h6,$iC,$oC,$kC};$qC=q#/lib/slice.b#;$rC=bless({$g5,$nC,$d6,$pC,$J,$qC},$m6);$sC={};$tC=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$uC=bless({$v,$tC,$x,$y},$z);$vC={$W6,$uC};$wC=q#/lib/slice_init.b#;$xC=bless({$g5,$sC,$d6,$vC,$J,$wC},$m6);$yC={};$zC=[];$AC=q#local $_;
my ($self, $quote) = @_;
my $name = $self->name;
$quote->quote_class(ref $self);

if (defined $name and $name eq 'ni:/lib/slice.b') {
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
$g;#;$BC=bless({$t,$zC,$v,$AC,$x,$y},$z);$CC={$ut,$BC};$DC=q#/lib/slice_serialize.b#;$EC=bless({$g5,$yC,$Z5,$q,$c6,$q,$d6,$CC,$J,$DC},$m6);$FC=[$Y7,$u6,$rC,$xC,$EC];$GC=bless({$g5,$gC,$J,$V4,$S5,$FC},$F5);$HC=q#ni:/lib/slice.b#;$IC=q#ni:/lib/slice.c#;$JC={$F5,1};$KC=q#/lib/slice.c#;$LC=[$x9];$MC=bless({$g5,$JC,$J,$KC,$S5,$LC},$T5);$NC=q#ni:/lib/slice_init.b#;$OC=q#ni:/lib/slice_serialize.b#;$PC=q#ni:/lib/static_fn.b#;$QC={};$RC=[];$SC=q#ni('ni:/lib/fn')->new(@_)#;$TC=bless({$t,$RC,$v,$SC,$x,$Hv},$z);$UC=q#fp#;$VC=[];$WC=q#($$)#;$XC=bless({$t,$VC,$v,$SC,$x,$WC},$z);$YC={$dt,$TC,$UC,$XC};$ZC=q#/lib/static_fn.b#;$cD=bless({$g5,$QC,$Z5,$q,$c6,$q,$d6,$YC,$J,$ZC},$m6);$dD=q#ni:/lib/subclass.b#;$eD=q#ni:/lib/tag#;$fD={$v6,1};$gD=q#/lib/tag#;$hD={};$iD=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$jD=bless({$v,$iD,$x,$y},$z);$kD={$h6,$jD};$lD=q#/lib/tag.b#;$mD=bless({$g5,$hD,$Z5,$q,$c6,$q,$d6,$kD,$J,$lD},$m6);$nD={};$oD=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$pD=bless({$v,$oD,$x,$y},$z);$qD={$W6,$pD};$rD=q#/lib/tag_init.b#;$sD=bless({$g5,$nD,$Z5,$q,$c6,$q,$d6,$qD,$J,$rD},$m6);$tD=[$Y7,$u6,$mD,$sD];$uD=bless({$g5,$fD,$J,$gD,$S5,$tD},$G5);$vD=q#ni:/lib/tag.b#;$wD=q#ni:/lib/tag.c#;$xD={$G5,1};$yD=q#/lib/tag.c#;$zD=[$x9];$AD=bless({$g5,$xD,$J,$yD,$S5,$zD},$T5);$BD=q#ni:/lib/tag_init.b#;$CD=q#ni:/lib/test_assert_eq#;$DD={$x7,1};$ED=q#/lib/test_assert_eq#;$FD={$x7,1,$y7,1};$GD=q#/lib/test_assertion#;$HD={};$ID=q#commit#;$JD=[];$KD=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$LD=bless({$t,$JD,$v,$KD,$x,$y},$z);$MD={$ID,$LD};$ND=q#/lib/test_assertion_commit.b#;$OD=bless({$g5,$HD,$Z5,$q,$c6,$q,$d6,$MD,$J,$ND},$m6);$PD=[$O7,$OD];$QD=bless({$g5,$FD,$J,$GD,$S5,$PD},$I5);$RD={};$SD=[];$TD=q#my ($class, $diff) = @_;
+{diff => $diff};#;$UD=bless({$t,$SD,$v,$TD,$x,$y},$z);$VD={$W6,$UD};$WD=q#/lib/test_assert_eq_init.b#;$XD=bless({$g5,$RD,$Z5,$q,$c6,$q,$d6,$VD,$J,$WD},$m6);$YD={};$ZD=[];$cE=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode_pretty $$self{diff}
              : "PASS";#;$dE=bless({$t,$ZD,$v,$cE,$x,$y},$z);$eE=q#failed#;$fE=[];$gE=q#defined shift->{diff}#;$hE=bless({$t,$fE,$v,$gE,$x,$y},$z);$iE=q#result#;$jE=[];$kE=bless({$t,$jE,$v,$Yo,$x,$y},$z);$lE={$v8,$dE,$eE,$hE,$iE,$kE};$mE=q#/lib/test_assert_eq_result.b#;$nE=bless({$g5,$YD,$Z5,$q,$c6,$q,$d6,$lE,$J,$mE},$m6);$oE=[$QD,$XD,$nE];$pE=bless({$g5,$DD,$J,$ED,$S5,$oE},$H5);$qE=q#ni:/lib/test_assert_eq.c#;$rE={$H5,1};$sE=q#/lib/test_assert_eq.c#;$tE={$H5,1,$I5,1};$uE=q#/lib/test_assertion.c#;$vE=[$t9];$wE=bless({$g5,$tE,$J,$uE,$S5,$vE},$T5);$xE=[$wE];$yE=bless({$g5,$rE,$J,$sE,$S5,$xE},$T5);$zE=q#ni:/lib/test_assert_eq_init.b#;$AE=q#ni:/lib/test_assert_eq_result.b#;$BE=q#ni:/lib/test_assertion#;$CE=q#ni:/lib/test_assertion.c#;$DE=q#ni:/lib/test_assertion_commit.b#;$EE=q#ni:/lib/test_case#;$FE={$B,1};$GE=q#/lib/test_case#;$HE=q#running_test#;$IE={};$JE=[];$KE=q#shift->{'assertions'}#;$LE=bless({$t,$JE,$v,$KE,$x,$y},$z);$ME=[];$NE=q#shift->{'test'}#;$OE=bless({$t,$ME,$v,$NE,$x,$y},$z);$PE={$n,$LE,$s,$OE};$QE=q#/lib/test_case_ro.b#;$RE=bless({$g5,$IE,$Z5,$q,$c6,$q,$d6,$PE,$J,$QE},$m6);$SE={};$TE=[];$UE=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$VE=bless({$t,$TE,$v,$UE,$x,$y},$z);$WE={$p,$VE};$XE=q#/lib/test_case_rw.b#;$YE=bless({$g5,$SE,$Z5,$q,$c6,$q,$d6,$WE,$J,$XE},$m6);$ZE={};$cF=[];$dF=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$eF=bless({$t,$cF,$v,$dF,$x,$y},$z);$fF={$W6,$eF};$gF=q#/lib/test_case_init.b#;$hF=bless({$g5,$ZE,$Z5,$q,$c6,$q,$d6,$fF,$J,$gF},$m6);$iF={};$jF=[];$kF=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$lF=bless({$t,$jF,$v,$kF,$x,$y},$z);$mF=[];$nF=q#!shift->{outcome}->[0]#;$oF=bless({$t,$mF,$v,$nF,$x,$y},$z);$pF={$v8,$lF,$eE,$oF};$qF=q#/lib/test_case_metrics.b#;$rF=bless({$g5,$iF,$Z5,$q,$c6,$q,$d6,$pF,$J,$qF},$m6);$sF={};$tF=q#done#;$uF=[];$vF=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$wF=bless({$t,$uF,$v,$vF,$x,$y},$z);$xF=[];$yF=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$zF=bless({$t,$xF,$v,$yF,$x,$y},$z);$AF={$tF,$wF,$Wo,$zF};$BF=q#/lib/test_case_run.b#;$CF=bless({$g5,$sF,$Z5,$q,$c6,$q,$d6,$AF,$J,$BF},$m6);$DF=[$O7,$RE,$YE,$hF,$rF,$CF];$EF=bless({$g5,$FE,$J,$GE,$HE,$q,$S5,$DF},$J5);$FF=[];$GF=q#shift->{running_test} = undef#;$HF=bless({$t,$FF,$v,$GF,$x,$y},$z);$IF=q#ni:/lib/test_case.c#;$JF={$J5,1};$KF=q#/lib/test_case.c#;$LF={};$MF=[];$NF=q#shift->{'running_test'}#;$OF=bless({$t,$MF,$v,$NF,$x,$y},$z);$PF={$HE,$OF};$QF=q#/lib/test_case.c_test_ro.b#;$RF=bless({$g5,$LF,$Z5,$q,$c6,$q,$d6,$PF,$J,$QF},$m6);$SF={};$TF=q#with_test#;$UF=[];$VF=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$WF=bless({$t,$UF,$v,$VF,$x,$y},$z);$XF={$TF,$WF};$YF=q#/lib/test_case.c_test.b#;$ZF=bless({$g5,$SF,$Z5,$HF,$c6,$q,$d6,$XF,$J,$YF},$m6);$cG=[$t9,$RF,$ZF];$dG=bless({$g5,$JF,$J,$KF,$S5,$cG},$T5);$eG=q#ni:/lib/test_case.c_test.b#;$fG=q#ni:/lib/test_case.c_test_ro.b#;$gG=q#ni:/lib/test_case_init.b#;$hG=q#ni:/lib/test_case_metrics.b#;$iG=q#ni:/lib/test_case_ro.b#;$jG=q#ni:/lib/test_case_run.b#;$kG=q#ni:/lib/test_case_rw.b#;$lG=q#ni:/lib/test_value#;$mG={$z7,1};$nG=q#/lib/test_value#;$oG={};$pG=[];$qG=q#\\$_[1]#;$rG=bless({$t,$pG,$v,$qG,$x,$y},$z);$sG={$W6,$rG};$tG=q#/lib/test_value_init.b#;$uG=bless({$g5,$oG,$Z5,$q,$c6,$q,$d6,$sG,$J,$tG},$m6);$vG={};$wG=q#(==#;$xG=[];$yG=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$zG=bless({$t,$xG,$v,$yG,$x,$y},$z);$AG=q#detailed_scalar_diff#;$BG=[];$CG=q#local $_;
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
[@diff];#;$DG=bless({$t,$BG,$v,$CG,$x,$y},$z);$EG=q#diff#;$FG=[];$GG=q#my ($self, $rhs) = @_;
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
return undef;#;$HG=bless({$t,$FG,$v,$GG,$x,$y},$z);$IG={$wG,$zG,$AG,$DG,$EG,$HG};$JG=q#/lib/test_value_eq.b#;$KG=bless({$g5,$vG,$Z5,$q,$c6,$q,$d6,$IG,$J,$JG},$m6);$LG={};$MG=[];$NG=q#ni::json_encode ${$_[0]}#;$OG=bless({$t,$MG,$v,$NG,$x,$y},$z);$PG={$v8,$OG};$QG=q#/lib/test_value_str.b#;$RG=bless({$g5,$LG,$Z5,$q,$c6,$q,$d6,$PG,$J,$QG},$m6);$SG=[$O7,$uG,$KG,$RG];$TG=bless({$g5,$mG,$J,$nG,$S5,$SG},$K5);$UG=q#ni:/lib/test_value.c#;$VG={$K5,1};$WG=q#/lib/test_value.c#;$XG=[$t9];$YG=bless({$g5,$VG,$J,$WG,$S5,$XG},$T5);$ZG=q#ni:/lib/test_value_eq.b#;$cH=q#ni:/lib/test_value_init.b#;$dH=q#ni:/lib/test_value_str.b#;$eH=q#ni:/metaclass#;$fH={$T5,1};$gH=q#/metaclass#;$hH=[$S6,$Z8,$d7,$S8];$iH=bless({$g5,$fH,$J,$gH,$S5,$hH},$L5);$jH=q#ni:/metaclass.c#;$kH={$L5,1};$lH=q#/metaclass.c#;$mH=[$k9];$nH=bless({$g5,$kH,$J,$lH,$S5,$mH},$T5);$oH=q#ni:/module#;$pH=q#ni:/module.c#;$qH=q#ni:/object#;$rH=q#ni:/object.c#;$sH=q#ni:/semantic/dimension#;$tH={$O5,1};$uH=q#/semantic/dimension#;$vH=[$k9];$wH=bless({$g5,$tH,$J,$uH,$S5,$vH},$P5);$xH=q#ni:/semantic/dimension.c#;$yH={$P5,1};$zH=q#/semantic/dimension.c#;$AH=[$B9];$BH=bless({$g5,$yH,$J,$zH,$S5,$AH},$T5);$CH=q#ni:/semantic/task#;$DH=q#ni:/semantic/task.c#;$EH=q#ni:/semantic/task_outcome.b#;$FH=q#ni:/semantic/task_ro.b#;$GH=q#ni:main#;$HH={$rj,1};$IH=[$cD,$Pv,$qj];$JH=bless({$g5,$HH,$J,$rj,$S5,$IH},$U5);$KH=q#ni:ni#;$LH={$Xy,1};$MH={$Xy,1};$NH=q#json_escapes#;$OH=q##;$PH=q#b#;$QH=q#	#;$RH=q#t#;$SH=q#
#;$TH=q#n#;$UH=q##;$VH=q#"#;$WH=q#/#;$XH=q#\\#;$YH={$OH,$PH,$QH,$RH,$SH,$TH,$UH,$Gh,$VH,$VH,$WH,$WH,$XH,$XH};$ZH=q#json_unescapes#;$cI={$VH,$VH,$WH,$WH,$XH,$XH,$PH,$OH,$TH,$SH,$Gh,$UH,$RH,$QH};$dI={$NH,$YH,$ZH,$cI};$eI=q#/lib/json_data.b#;$fI=bless({$g5,$MH,$Ll,$dI,$J,$eI},$s7);$gI=[$fI,$Wy,$tB];$hI=bless({$g5,$LH,$J,$Xy,$S5,$gI},$U5);$iI={$d,$M,$P,$V,$W,$d1,$e1,$q1,$r1,$D1,$E1,$Q1,$R1,$f2,$g2,$B2,$C2,$H2,$I2,$g3,$h3,$n3,$o3,$H3,$I3,$Y3,$Z3,$i4,$j4,$q4,$r4,$B4,$C4,$W4,$X4,$e5,$f5,$k9,$l9,$B9,$C9,$Hb,$Ib,$Yb,$Zb,$lb,$cc,$Fb,$dc,$uc,$vc,$zc,$Ac,$lc,$Bc,$sc,$Cc,$oe,$pe,$te,$ue,$Ud,$ve,$me,$we,$Sc,$xe,$Md,$ye,$od,$ze,$Lc,$Ae,$Sf,$Wf,$wg,$xg,$ug,$yg,$qf,$zg,$Bf,$Ag,$Qe,$Bg,$Pf,$Cg,$Je,$Dg,$Ye,$Eg,$Yh,$Zh,$fi,$gi,$Ug,$hi,$th,$ii,$dh,$ji,$Wh,$ki,$Mg,$li,$Bh,$mi,$Fi,$Gi,$Ki,$Li,$Di,$Mi,$ui,$Ni,$qj,$sj,$Qj,$Rj,$Vj,$Wj,$Bj,$Xj,$Oj,$Yj,$eb,$Zj,$Wb,$ck,$Ub,$dk,$ia,$ek,$qa,$fk,$Ca,$gk,$M9,$hk,$cb,$ik,$Oa,$jk,$wl,$xl,$Bl,$Cl,$ul,$Dl,$Gk,$El,$el,$Fl,$wk,$Gl,$Sk,$Hl,$ym,$zm,$Dm,$Em,$im,$Fm,$wm,$Gm,$Zl,$Hm,$Rn,$Vn,$jo,$ko,$ho,$lo,$np,$rp,$Lp,$Mp,$Jp,$Np,$Ko,$Op,$Uo,$Pp,$Do,$Qp,$ip,$Rp,$vn,$Sp,$Pn,$Tp,$nq,$oq,$sq,$tq,$eq,$uq,$lq,$vq,$t8,$wq,$Y7,$xq,$x9,$yq,$Iq,$Jq,$n6,$Kq,$Oq,$Pq,$Gq,$Qq,$d7,$Rq,$jr,$kr,$or,$pr,$hr,$qr,$Zq,$rr,$Q8,$sr,$j8,$tr,$O8,$ur,$qs,$rs,$vs,$ws,$Rr,$xs,$cs,$ys,$Br,$zs,$Hr,$As,$os,$Bs,$W7,$Cs,$Ct,$Gt,$St,$Tt,$Qt,$Ut,$Ps,$Vt,$st,$Wt,$jt,$Xt,$At,$Yt,$kv,$lv,$pv,$qv,$iv,$rv,$su,$sv,$lu,$tv,$Mu,$uv,$Cv,$Dv,$Pv,$Qv,$ky,$ly,$py,$qy,$Yv,$ry,$yw,$sy,$M7,$ty,$Z8,$uy,$Wy,$Yy,$A8,$Zy,$u6,$cz,$C6,$dz,$J6,$ez,$hA,$iA,$mA,$nA,$fA,$oA,$Kz,$pA,$Tz,$qA,$sz,$rA,$tB,$uB,$S6,$vB,$Mx,$wB,$Gw,$xB,$iy,$yB,$wx,$zB,$SB,$TB,$XB,$YB,$HB,$ZB,$QB,$cC,$gx,$dC,$H8,$eC,$Q6,$fC,$GC,$HC,$rC,$IC,$MC,$NC,$xC,$OC,$EC,$PC,$cD,$dD,$i9,$eD,$uD,$vD,$mD,$wD,$AD,$BD,$sD,$CD,$pE,$qE,$yE,$zE,$XD,$AE,$nE,$BE,$QD,$CE,$wE,$DE,$OD,$EE,$EF,$IF,$dG,$eG,$ZF,$fG,$RF,$gG,$hF,$hG,$rF,$iG,$RE,$jG,$CF,$kG,$YE,$lG,$TG,$UG,$YG,$ZG,$KG,$cH,$uG,$dH,$RG,$eH,$iH,$jH,$nH,$oH,$S8,$pH,$z9,$qH,$O7,$rH,$t9,$sH,$wH,$xH,$BH,$CH,$in,$DH,$do,$EH,$gn,$FH,$Sm,$GH,$JH,$KH,$hI};$jI=q#resolvers#;$kI=[];$lI=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$mI=bless({$t,$kI,$v,$lI,$x,$y},$z);$nI=q#file#;$oI=[];$pI=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$qI=bless({$t,$oI,$v,$pI,$x,$y},$z);$rI=q#null#;$sI=[];$tI=q#ni('ni:/io/null')->new#;$uI=bless({$t,$sI,$v,$tI,$x,$y},$z);$vI=q#sh#;$wI=[];$xI=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$yI=bless({$t,$wI,$v,$xI,$x,$y},$z);$zI=q#str#;$AI=[];$BI=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$CI=bless({$t,$AI,$v,$BI,$x,$y},$z);$DI={$ud,$mI,$nI,$qI,$rI,$uI,$vI,$yI,$zI,$CI};$EI=bless({$c,$iI,$jI,$DI},$v7);*$mC=\&$kC;*$lC=\&$iC;$n6->apply_($h5);$n6->apply_($i5);$n6->apply_($j5);$n6->apply_($k5);$n6->apply_($l5);$n6->apply_($m5);$n6->apply_($n5);$n6->apply_($o5);$n6->apply_($p5);$n6->apply_($q5);$n6->apply_($r5);$n6->apply_($s5);$n6->apply_($t5);$n6->apply_($u5);$n6->apply_($v5);$n6->apply_($w5);$n6->apply_($o6);$n6->apply_($x5);$n6->apply_($y5);$n6->apply_($z5);$n6->apply_($A5);$n6->apply_($B5);$n6->apply_($C5);$n6->apply_($D5);$n6->apply_($E5);$n6->apply_($F5);$n6->apply_($G5);$n6->apply_($H5);$n6->apply_($I5);$n6->apply_($J5);$n6->apply_($K5);$n6->apply_($T5);$n6->apply_($L5);$n6->apply_($U5);$n6->apply_($M5);$n6->apply_($N5);$n6->apply_($O5);$n6->apply_($P5);$n6->apply_($Q5);$u6->apply_($h5);$u6->apply_($i5);$u6->apply_($j5);$u6->apply_($k5);$u6->apply_($l5);$u6->apply_($m5);$u6->apply_($n5);$u6->apply_($o5);$u6->apply_($p5);$u6->apply_($q5);$u6->apply_($r5);$u6->apply_($s5);$u6->apply_($t5);$u6->apply_($u5);$u6->apply_($v5);$u6->apply_($w5);$u6->apply_($o6);$u6->apply_($x5);$u6->apply_($y5);$u6->apply_($L);$u6->apply_($z5);$u6->apply_($A5);$u6->apply_($B5);$u6->apply_($C5);$u6->apply_($D5);$u6->apply_($E5);$u6->apply_($m6);$u6->apply_($F5);$u6->apply_($v6);$u6->apply_($G5);$u6->apply_($H5);$u6->apply_($I5);$u6->apply_($J5);$u6->apply_($K5);$u6->apply_($T5);$u6->apply_($L5);$u6->apply_($U5);$u6->apply_($M5);$u6->apply_($N5);$u6->apply_($O5);$u6->apply_($P5);$u6->apply_($Q5);$C6->apply_($h5);$C6->apply_($i5);$C6->apply_($j5);$C6->apply_($k5);$C6->apply_($l5);$C6->apply_($m5);$C6->apply_($n5);$C6->apply_($o5);$C6->apply_($p5);$C6->apply_($q5);$C6->apply_($r5);$C6->apply_($s5);$C6->apply_($t5);$C6->apply_($u5);$C6->apply_($v5);$C6->apply_($w5);$C6->apply_($o6);$C6->apply_($x5);$C6->apply_($y5);$C6->apply_($z5);$C6->apply_($A5);$C6->apply_($B5);$C6->apply_($C5);$C6->apply_($D5);$C6->apply_($E5);$C6->apply_($m6);$C6->apply_($F5);$C6->apply_($v6);$C6->apply_($G5);$C6->apply_($H5);$C6->apply_($I5);$C6->apply_($J5);$C6->apply_($K5);$C6->apply_($T5);$C6->apply_($L5);$C6->apply_($U5);$C6->apply_($M5);$C6->apply_($N5);$C6->apply_($O5);$C6->apply_($P5);$C6->apply_($Q5);$J6->apply_($h5);$J6->apply_($i5);$J6->apply_($j5);$J6->apply_($k5);$J6->apply_($l5);$J6->apply_($m5);$J6->apply_($n5);$J6->apply_($o5);$J6->apply_($p5);$J6->apply_($q5);$J6->apply_($r5);$J6->apply_($s5);$J6->apply_($t5);$J6->apply_($u5);$J6->apply_($v5);$J6->apply_($w5);$J6->apply_($o6);$J6->apply_($x5);$J6->apply_($y5);$J6->apply_($z5);$J6->apply_($A5);$J6->apply_($B5);$J6->apply_($C5);$J6->apply_($D5);$J6->apply_($E5);$J6->apply_($m6);$J6->apply_($F5);$J6->apply_($v6);$J6->apply_($G5);$J6->apply_($H5);$J6->apply_($I5);$J6->apply_($J5);$J6->apply_($K5);$J6->apply_($T5);$J6->apply_($L5);$J6->apply_($U5);$J6->apply_($M5);$J6->apply_($N5);$J6->apply_($O5);$J6->apply_($P5);$J6->apply_($Q5);$Q6->apply_($h5);$Q6->apply_($i5);$Q6->apply_($j5);$Q6->apply_($k5);$Q6->apply_($l5);$Q6->apply_($m5);$Q6->apply_($n5);$Q6->apply_($o5);$Q6->apply_($p5);$Q6->apply_($q5);$Q6->apply_($r5);$Q6->apply_($s5);$Q6->apply_($t5);$Q6->apply_($u5);$Q6->apply_($v5);$Q6->apply_($w5);$Q6->apply_($o6);$Q6->apply_($x5);$Q6->apply_($y5);$Q6->apply_($z5);$Q6->apply_($A5);$Q6->apply_($B5);$Q6->apply_($C5);$Q6->apply_($D5);$Q6->apply_($E5);$Q6->apply_($F5);$Q6->apply_($v6);$Q6->apply_($G5);$Q6->apply_($H5);$Q6->apply_($I5);$Q6->apply_($J5);$Q6->apply_($K5);$Q6->apply_($T5);$Q6->apply_($L5);$Q6->apply_($U5);$Q6->apply_($M5);$Q6->apply_($N5);$Q6->apply_($O5);$Q6->apply_($P5);$Q6->apply_($Q5);$d7->apply_($h5);$d7->apply_($i5);$d7->apply_($j5);$d7->apply_($k5);$d7->apply_($l5);$d7->apply_($m5);$d7->apply_($n5);$d7->apply_($o5);$d7->apply_($p5);$d7->apply_($q5);$d7->apply_($r5);$d7->apply_($s5);$d7->apply_($t5);$d7->apply_($u5);$d7->apply_($v5);$d7->apply_($w5);$d7->apply_($x5);$d7->apply_($y5);$d7->apply_($z5);$d7->apply_($A5);$d7->apply_($B5);$d7->apply_($C5);$d7->apply_($D5);$d7->apply_($E5);$d7->apply_($F5);$d7->apply_($G5);$d7->apply_($H5);$d7->apply_($I5);$d7->apply_($J5);$d7->apply_($K5);$d7->apply_($T5);$d7->apply_($L5);$d7->apply_($U5);$d7->apply_($M5);$d7->apply_($N5);$d7->apply_($O5);$d7->apply_($P5);$d7->apply_($Q5);$M7->apply_($h5);$M7->apply_($i5);$M7->apply_($e7);$M7->apply_($j5);$M7->apply_($f7);$M7->apply_($k5);$M7->apply_($g7);$M7->apply_($l5);$M7->apply_($h7);$M7->apply_($m5);$M7->apply_($i7);$M7->apply_($n5);$M7->apply_($j7);$M7->apply_($o5);$M7->apply_($k7);$M7->apply_($p5);$M7->apply_($l7);$M7->apply_($q5);$M7->apply_($m7);$M7->apply_($r5);$M7->apply_($n7);$M7->apply_($s5);$M7->apply_($o7);$M7->apply_($t5);$M7->apply_($p7);$M7->apply_($u5);$M7->apply_($q7);$M7->apply_($v5);$M7->apply_($r7);$M7->apply_($w5);$M7->apply_($o6);$M7->apply_($x5);$M7->apply_($s7);$M7->apply_($y5);$M7->apply_($L);$M7->apply_($z5);$M7->apply_($z);$M7->apply_($A5);$M7->apply_($t7);$M7->apply_($B5);$M7->apply_($u7);$M7->apply_($C5);$M7->apply_($v7);$M7->apply_($D5);$M7->apply_($w7);$M7->apply_($E5);$M7->apply_($m6);$M7->apply_($F5);$M7->apply_($v6);$M7->apply_($G5);$M7->apply_($x7);$M7->apply_($H5);$M7->apply_($y7);$M7->apply_($I5);$M7->apply_($B);$M7->apply_($J5);$M7->apply_($z7);$M7->apply_($K5);$M7->apply_($T5);$M7->apply_($L5);$M7->apply_($U5);$M7->apply_($M5);$M7->apply_($A7);$M7->apply_($N5);$M7->apply_($O5);$M7->apply_($P5);$M7->apply_($B7);$M7->apply_($Q5);$W7->apply_($h5);$W7->apply_($i5);$W7->apply_($j5);$W7->apply_($k5);$W7->apply_($l5);$W7->apply_($m5);$W7->apply_($n5);$W7->apply_($o5);$W7->apply_($p5);$W7->apply_($q5);$W7->apply_($r5);$W7->apply_($s5);$W7->apply_($t5);$W7->apply_($u5);$W7->apply_($v5);$W7->apply_($r7);$W7->apply_($w5);$W7->apply_($o6);$W7->apply_($x5);$W7->apply_($s7);$W7->apply_($y5);$W7->apply_($z5);$W7->apply_($A5);$W7->apply_($B5);$W7->apply_($C5);$W7->apply_($D5);$W7->apply_($E5);$W7->apply_($m6);$W7->apply_($F5);$W7->apply_($v6);$W7->apply_($G5);$W7->apply_($H5);$W7->apply_($I5);$W7->apply_($J5);$W7->apply_($K5);$W7->apply_($T5);$W7->apply_($L5);$W7->apply_($U5);$W7->apply_($M5);$W7->apply_($N5);$W7->apply_($O5);$W7->apply_($P5);$W7->apply_($Q5);$j8->apply_($h5);$j8->apply_($i5);$j8->apply_($j5);$j8->apply_($k5);$j8->apply_($l5);$j8->apply_($m5);$j8->apply_($n5);$j8->apply_($o5);$j8->apply_($p5);$j8->apply_($q5);$j8->apply_($r5);$j8->apply_($s5);$j8->apply_($t5);$j8->apply_($u5);$j8->apply_($v5);$j8->apply_($w5);$j8->apply_($o6);$j8->apply_($x5);$j8->apply_($y5);$j8->apply_($z5);$j8->apply_($A5);$j8->apply_($B5);$j8->apply_($C5);$j8->apply_($D5);$j8->apply_($E5);$j8->apply_($F5);$j8->apply_($G5);$j8->apply_($H5);$j8->apply_($I5);$j8->apply_($J5);$j8->apply_($K5);$j8->apply_($T5);$j8->apply_($L5);$j8->apply_($U5);$j8->apply_($M5);$j8->apply_($N5);$j8->apply_($O5);$j8->apply_($P5);$j8->apply_($Q5);$t8->apply_($h5);$t8->apply_($i5);$t8->apply_($j5);$t8->apply_($k5);$t8->apply_($l5);$t8->apply_($m5);$t8->apply_($n5);$t8->apply_($o5);$t8->apply_($p5);$t8->apply_($q5);$t8->apply_($r5);$t8->apply_($s5);$t8->apply_($t5);$t8->apply_($u5);$t8->apply_($v5);$t8->apply_($w5);$t8->apply_($o6);$t8->apply_($x5);$t8->apply_($y5);$t8->apply_($z5);$t8->apply_($A5);$t8->apply_($B5);$t8->apply_($C5);$t8->apply_($D5);$t8->apply_($E5);$t8->apply_($F5);$t8->apply_($G5);$t8->apply_($H5);$t8->apply_($I5);$t8->apply_($J5);$t8->apply_($K5);$t8->apply_($T5);$t8->apply_($L5);$t8->apply_($U5);$t8->apply_($M5);$t8->apply_($N5);$t8->apply_($O5);$t8->apply_($P5);$t8->apply_($Q5);$A8->apply_($h5);$A8->apply_($i5);$A8->apply_($j5);$A8->apply_($k5);$A8->apply_($l5);$A8->apply_($m5);$A8->apply_($n5);$A8->apply_($o5);$A8->apply_($p5);$A8->apply_($q5);$A8->apply_($r5);$A8->apply_($s5);$A8->apply_($t5);$A8->apply_($u5);$A8->apply_($v5);$A8->apply_($w5);$A8->apply_($o6);$A8->apply_($x5);$A8->apply_($y5);$A8->apply_($z5);$A8->apply_($A5);$A8->apply_($B5);$A8->apply_($C5);$A8->apply_($D5);$A8->apply_($E5);$A8->apply_($F5);$A8->apply_($G5);$A8->apply_($H5);$A8->apply_($I5);$A8->apply_($J5);$A8->apply_($K5);$A8->apply_($T5);$A8->apply_($L5);$A8->apply_($U5);$A8->apply_($M5);$A8->apply_($N5);$A8->apply_($O5);$A8->apply_($P5);$A8->apply_($Q5);$H8->apply_($h5);$H8->apply_($i5);$H8->apply_($j5);$H8->apply_($k5);$H8->apply_($l5);$H8->apply_($m5);$H8->apply_($n5);$H8->apply_($o5);$H8->apply_($p5);$H8->apply_($q5);$H8->apply_($r5);$H8->apply_($s5);$H8->apply_($t5);$H8->apply_($u5);$H8->apply_($v5);$H8->apply_($w5);$H8->apply_($o6);$H8->apply_($x5);$H8->apply_($y5);$H8->apply_($z5);$H8->apply_($A5);$H8->apply_($B5);$H8->apply_($C5);$H8->apply_($D5);$H8->apply_($E5);$H8->apply_($F5);$H8->apply_($G5);$H8->apply_($H5);$H8->apply_($I5);$H8->apply_($J5);$H8->apply_($K5);$H8->apply_($T5);$H8->apply_($L5);$H8->apply_($U5);$H8->apply_($M5);$H8->apply_($N5);$H8->apply_($O5);$H8->apply_($P5);$H8->apply_($Q5);$O8->apply_($h5);$O8->apply_($i5);$O8->apply_($j5);$O8->apply_($k5);$O8->apply_($l5);$O8->apply_($m5);$O8->apply_($n5);$O8->apply_($o5);$O8->apply_($p5);$O8->apply_($q5);$O8->apply_($r5);$O8->apply_($s5);$O8->apply_($t5);$O8->apply_($u5);$O8->apply_($v5);$O8->apply_($w5);$O8->apply_($o6);$O8->apply_($x5);$O8->apply_($y5);$O8->apply_($z5);$O8->apply_($A5);$O8->apply_($B5);$O8->apply_($C5);$O8->apply_($D5);$O8->apply_($E5);$O8->apply_($F5);$O8->apply_($G5);$O8->apply_($H5);$O8->apply_($I5);$O8->apply_($J5);$O8->apply_($K5);$O8->apply_($T5);$O8->apply_($L5);$O8->apply_($U5);$O8->apply_($M5);$O8->apply_($N5);$O8->apply_($O5);$O8->apply_($P5);$O8->apply_($Q5);$Z8->apply_($h5);$Z8->apply_($i5);$Z8->apply_($j5);$Z8->apply_($k5);$Z8->apply_($l5);$Z8->apply_($m5);$Z8->apply_($n5);$Z8->apply_($o5);$Z8->apply_($p5);$Z8->apply_($q5);$Z8->apply_($r5);$Z8->apply_($s5);$Z8->apply_($t5);$Z8->apply_($u5);$Z8->apply_($v5);$Z8->apply_($w5);$Z8->apply_($x5);$Z8->apply_($y5);$Z8->apply_($z5);$Z8->apply_($z);$Z8->apply_($A5);$Z8->apply_($B5);$Z8->apply_($C5);$Z8->apply_($D5);$Z8->apply_($E5);$Z8->apply_($m6);$Z8->apply_($F5);$Z8->apply_($v6);$Z8->apply_($G5);$Z8->apply_($H5);$Z8->apply_($I5);$Z8->apply_($J5);$Z8->apply_($K5);$Z8->apply_($T5);$Z8->apply_($L5);$Z8->apply_($M5);$Z8->apply_($N5);$Z8->apply_($O5);$Z8->apply_($P5);$Z8->apply_($Q5);$i9->apply_($h5);$i9->apply_($i5);$i9->apply_($j5);$i9->apply_($k5);$i9->apply_($l5);$i9->apply_($m5);$i9->apply_($n5);$i9->apply_($o5);$i9->apply_($p5);$i9->apply_($q5);$i9->apply_($r5);$i9->apply_($s5);$i9->apply_($t5);$i9->apply_($u5);$i9->apply_($v5);$i9->apply_($w5);$i9->apply_($x5);$i9->apply_($y5);$i9->apply_($z5);$i9->apply_($A5);$i9->apply_($B5);$i9->apply_($C5);$i9->apply_($D5);$i9->apply_($E5);$i9->apply_($F5);$i9->apply_($G5);$i9->apply_($H5);$i9->apply_($I5);$i9->apply_($J5);$i9->apply_($K5);$i9->apply_($L5);$i9->apply_($M5);$i9->apply_($N5);$i9->apply_($O5);$i9->apply_($P5);$i9->apply_($Q5);$M9->apply_($e7);$M9->apply_($f7);$M9->apply_($g7);$M9->apply_($h7);$M9->apply_($i7);$M9->apply_($j7);$M9->apply_($k7);$M9->apply_($l7);$M9->apply_($m7);$M9->apply_($n7);$ia->apply_($e7);$ia->apply_($f7);$ia->apply_($g7);$ia->apply_($h7);$ia->apply_($i7);$ia->apply_($j7);$ia->apply_($k7);$ia->apply_($l7);$ia->apply_($m7);$ia->apply_($n7);$qa->apply_($e7);$qa->apply_($f7);$qa->apply_($g7);$qa->apply_($h7);$qa->apply_($i7);$qa->apply_($j7);$qa->apply_($k7);$qa->apply_($l7);$qa->apply_($m7);$qa->apply_($n7);$Ca->apply_($e7);$Ca->apply_($f7);$Ca->apply_($g7);$Ca->apply_($h7);$Ca->apply_($i7);$Ca->apply_($j7);$Ca->apply_($k7);$Ca->apply_($l7);$Ca->apply_($m7);$Ca->apply_($n7);$Oa->apply_($e7);$Oa->apply_($f7);$Oa->apply_($g7);$Oa->apply_($h7);$Oa->apply_($i7);$Oa->apply_($j7);$Oa->apply_($k7);$Oa->apply_($l7);$Oa->apply_($m7);$Oa->apply_($n7);$cb->apply_($e7);$cb->apply_($f7);$cb->apply_($g7);$cb->apply_($h7);$cb->apply_($i7);$cb->apply_($j7);$cb->apply_($k7);$cb->apply_($l7);$cb->apply_($m7);$cb->apply_($n7);$lb->apply_($e7);$Fb->apply_($e7);$Ub->apply_($j5);$Ub->apply_($k5);$Ub->apply_($l5);$Ub->apply_($m5);$Ub->apply_($n5);$Ub->apply_($o5);$Ub->apply_($p5);$Ub->apply_($q5);$Ub->apply_($r5);$Ub->apply_($s5);$lc->apply_($f7);$sc->apply_($f7);$Lc->apply_($g7);$Sc->apply_($g7);$od->apply_($g7);$Md->apply_($g7);$Ud->apply_($g7);$me->apply_($g7);$Je->apply_($h7);$Je->apply_($j7);$Qe->apply_($h7);$Ye->apply_($h7);$qf->apply_($h7);$qf->apply_($j7);$Bf->apply_($h7);$Pf->apply_($h7);$Pf->apply_($j7);$ug->apply_($m5);$Mg->apply_($i7);$Ug->apply_($i7);$dh->apply_($i7);$th->apply_($i7);$Bh->apply_($i7);$Wh->apply_($i7);$ui->apply_($j7);$Di->apply_($j7);$qj->apply_($rj);$Bj->apply_($k7);$Oj->apply_($k7);$wk->apply_($m7);$Gk->apply_($m7);$Sk->apply_($m7);$el->apply_($m7);$ul->apply_($m7);$Zl->apply_($n7);$im->apply_($n7);$wm->apply_($n7);$Sm->apply_($o7);$Sm->apply_($p7);$Sm->apply_($q7);$Sm->apply_($B7);$gn->apply_($o7);$gn->apply_($p7);$gn->apply_($q7);$gn->apply_($B7);$vn->apply_($o7);$vn->apply_($p7);$vn->apply_($q7);$Pn->apply_($o7);$Pn->apply_($p7);$Pn->apply_($q7);$ho->apply_($t5);$ho->apply_($u5);$ho->apply_($v5);$Do->apply_($p7);$Ko->apply_($p7);$Uo->apply_($p7);$ip->apply_($p7);$Jp->apply_($u5);$eq->apply_($q7);$lq->apply_($q7);$Gq->apply_($o6);$Zq->apply_($s7);$hr->apply_($s7);$Br->apply_($L);$Hr->apply_($L);$Rr->apply_($L);$cs->apply_($L);$os->apply_($L);$Ps->apply_($z);$jt->apply_($z);$st->apply_($z);$At->apply_($z);$Qt->apply_($A5);$lu->apply_($t7);$su->apply_($t7);$Mu->apply_($t7);$iv->apply_($t7);$Cv->apply_($u7);$Pv->apply_($rj);$Yv->apply_($u7);$yw->apply_($u7);$Gw->apply_($u7);$Gw->apply_($w7);$gx->apply_($u7);$gx->apply_($w7);$wx->apply_($u7);$wx->apply_($w7);$Mx->apply_($u7);$iy->apply_($u7);$Wy->apply_($Xy);$sz->apply_($v7);$Kz->apply_($v7);$Tz->apply_($v7);$fA->apply_($v7);$tB->apply_($Xy);$HB->apply_($w7);$QB->apply_($w7);$rC->apply_($m6);$xC->apply_($m6);$EC->apply_($m6);$cD->apply_($rj);$mD->apply_($v6);$sD->apply_($v6);$OD->apply_($x7);$OD->apply_($y7);$XD->apply_($x7);$nE->apply_($x7);$RE->apply_($B);$YE->apply_($B);$hF->apply_($B);$rF->apply_($B);$CF->apply_($B);$RF->apply_($J5);$ZF->apply_($J5);$uG->apply_($z7);$KG->apply_($z7);$RG->apply_($z7);$ni::self=$EI;&$O($M);&$O($V);&$O($d1);&$O($q1);&$O($D1);&$O($Q1);&$O($f2);&$O($B2);&$O($H2);&$O($g3);&$O($n3);&$O($H3);&$O($Y3);&$O($i4);&$O($q4);&$O($B4);&$O($W4);&$O($e5);&$O($n6);&$O($u6);&$O($C6);&$O($J6);&$O($Q6);&$O($S6);&$O($d7);&$O($M7);&$O($O7);&$V6($O7);&$O($W7);&$O($Y7);&$V6($Y7);&$O($j8);&$O($t8);&$O($A8);&$O($H8);&$O($O8);&$O($Q8);&$O($S8);&$V6($S8);&$O($Z8);&$O($i9);&$O($k9);&$V6($k9);&$O($t9);&$V6($t9);&$O($x9);&$V6($x9);&$O($z9);&$V6($z9);&$O($B9);&$V6($B9);&$O($M9);&$O($ia);&$O($qa);&$O($Ca);&$O($Oa);&$O($cb);&$O($eb);&$V6($eb);&$O($lb);&$O($Fb);&$O($Hb);&$V6($Hb);&$O($Ub);&$O($Wb);&$V6($Wb);&$O($Yb);&$V6($Yb);&$O($lc);&$O($sc);&$O($uc);&$V6($uc);&$O($zc);&$V6($zc);&$O($Lc);&$O($Sc);&$O($od);&$O($Md);&$O($Ud);&$O($me);&$O($oe);&$V6($oe);&$O($te);&$V6($te);&$O($Je);&$O($Qe);&$O($Ye);&$O($qf);&$O($Bf);&$O($Pf);&$O($Sf);&$V6($Sf);&$Vf($Sf);&$O($ug);&$O($wg);&$V6($wg);&$O($Mg);&$O($Ug);&$O($dh);&$O($th);&$O($Bh);&$O($Wh);&$O($Yh);&$V6($Yh);&$O($fi);&$V6($fi);&$O($ui);&$O($Di);&$O($Fi);&$V6($Fi);&$O($Ki);&$V6($Ki);&$O($qj);&$O($Bj);&$O($Oj);&$O($Qj);&$V6($Qj);&$O($Vj);&$V6($Vj);&$O($wk);&$O($Gk);&$O($Sk);&$O($el);&$O($ul);&$O($wl);&$V6($wl);&$O($Bl);&$V6($Bl);&$O($Zl);&$O($im);&$O($wm);&$O($ym);&$V6($ym);&$O($Dm);&$V6($Dm);&$O($Sm);&$O($gn);&$O($in);&$V6($in);&$O($vn);&$O($Pn);&$O($Rn);&$V6($Rn);&$Un($Rn);&$O($do);&$V6($do);&$O($ho);&$O($jo);&$V6($jo);&$O($Do);&$O($Ko);&$O($Uo);&$O($ip);&$O($np);&$V6($np);&$Un($np);&$qp($np);&$O($Jp);&$O($Lp);&$V6($Lp);&$O($eq);&$O($lq);&$O($nq);&$V6($nq);&$Un($nq);&$O($sq);&$V6($sq);&$O($Gq);&$O($Iq);&$V6($Iq);&$O($Oq);&$V6($Oq);&$O($Zq);&$O($hr);&$O($jr);&$V6($jr);&$O($or);&$V6($or);&$O($Br);&$O($Hr);&$O($Rr);&$O($cs);&$O($os);&$O($qs);&$V6($qs);&$O($vs);&$V6($vs);&$O($Ps);&$O($jt);&$O($st);&$O($At);&$O($Ct);&$V6($Ct);&$Ft($Ct);&$O($Qt);&$O($St);&$V6($St);&$O($lu);&$O($su);&$O($Mu);&$O($iv);&$O($kv);&$V6($kv);&$O($pv);&$V6($pv);&$O($Cv);&$O($Pv);&$O($Yv);&$O($yw);&$O($Gw);&$O($gx);&$O($wx);&$O($Mx);&$O($iy);&$O($ky);&$V6($ky);&$O($py);&$V6($py);&$O($Wy);&$O($sz);&$O($Kz);&$O($Tz);&$O($fA);&$O($hA);&$V6($hA);&$O($mA);&$V6($mA);&$O($tB);&$O($HB);&$O($QB);&$O($SB);&$V6($SB);&$O($XB);&$V6($XB);&$O($rC);&$O($xC);&$O($EC);&$O($GC);&$V6($GC);&$O($MC);&$V6($MC);&$O($cD);&$O($mD);&$O($sD);&$O($uD);&$V6($uD);&$O($AD);&$V6($AD);&$O($OD);&$O($QD);&$V6($QD);&$O($XD);&$O($nE);&$O($pE);&$V6($pE);&$O($wE);&$V6($wE);&$O($yE);&$V6($yE);&$O($RE);&$O($YE);&$O($hF);&$O($rF);&$O($CF);&$O($EF);&$V6($EF);&$HF($EF);&$O($RF);&$O($ZF);&$O($dG);&$V6($dG);&$O($uG);&$O($KG);&$O($RG);&$O($TG);&$V6($TG);&$O($YG);&$V6($YG);&$O($iH);&$V6($iH);&$O($nH);&$V6($nH);&$O($wH);&$V6($wH);&$O($BH);&$V6($BH);&$O($JH);&$V6($JH);&$O($hI);&$V6($hI);ni->run(@ARGV);
__DATA__
