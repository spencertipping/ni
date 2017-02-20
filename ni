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
instance, then we can see its resources.#;$R=[$i,$Q];$S=[$R];$T=q#/fabric#;$U=bless({$e,$S,$J,$T},$L);$V=q#ni.doc:/fabric/perl#;$W=q#A perl interpreter running somewhere. In the fabric sense, this means
something that has a name table whose entries can respond to method
calls.#;$X=q#There are some interesting design considerations that go into this.
First, we want to jointly minimize the running transmission size while
also being careful not to leak memory by caching things. A naive setup
like the one ni uses to serialize itself would meet the first requirement
but not the second.#;$Y=q#Second, we don't want to incur parsing overhead for every request we
make.#;$Z=[$i,$W,$X,$Y];$c1=[$Z];$d1=q#/fabric/perl#;$e1=bless({$e,$c1,$J,$d1},$L);$f1=q#ni.doc:/io#;$g1=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$h1=[$i,$g1];$i1=[$h1];$j1=q#/io#;$k1=bless({$e,$i1,$J,$j1},$L);$l1=q#ni.doc:/io/buffer#;$m1=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$n1=[$f,$m1];$o1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$p1=[];$q1=[];$r1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$s1=bless({$t,$q1,$v,$r1,$x,$y},$z);$t1=bless({$n,$p1,$p,$q,$r,$q,$s,$s1},$B);$u1=[$i,$o1,$t1];$v1=[$n1,$u1];$w1=q#/io/buffer#;$x1=bless({$e,$v1,$J,$w1},$L);$y1=q#ni.doc:/io/cat#;$z1=q#
  my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
  my $combined = $io1 + $io2 + $io3;
  $combined->into_sync($destination_io);
#;$A1=[$f,$z1];$B1=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$C1=[];$D1=[];$E1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$F1=bless({$t,$D1,$v,$E1,$x,$y},$z);$G1=bless({$n,$C1,$p,$q,$r,$q,$s,$F1},$B);$H1=[$i,$B1,$G1];$I1=[$A1,$H1];$J1=q#/io/cat#;$K1=bless({$e,$I1,$J,$J1},$L);$L1=q#ni.doc:/io/exec#;$M1=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$N1=[$f,$M1];$O1=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$P1=[];$Q1=[];$R1=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$S1=bless({$t,$Q1,$v,$R1,$x,$y},$z);$T1=bless({$n,$P1,$p,$q,$r,$q,$s,$S1},$B);$U1=[$i,$O1,$T1];$V1=[$N1,$U1];$W1=q#/io/exec#;$X1=bless({$e,$V1,$J,$W1},$L);$Y1=q#ni.doc:/io/fd#;$Z1=q#
  open my $fh, ...;
  my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
  my $fd = ni('ni:/io/fd')->new(0);   \# from number
  my $fd = ni('fd:0');                \# same thing
  $fd->nonblock(1)->read($_, 100);
  $fd->be(10);                        \# move FD number
#;$c2=[$f,$Z1];$d2=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$e2=[];$f2=[];$g2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$h2=bless({$t,$f2,$v,$g2,$x,$y},$z);$i2=bless({$n,$e2,$p,$q,$r,$q,$s,$h2},$B);$j2=[$i,$d2,$i2];$k2=[$c2,$j2];$l2=q#/io/fd#;$m2=bless({$e,$k2,$J,$l2},$L);$n2=q#ni.doc:/io/file#;$o2=q#
  my $f = ni('ni:/io/file')->new('/etc/passwd');
  my $f = ni('file:/etc/passwd');     \# same as above
  $f->into_sync(ni('fd:1'));          \# cat to stdout
#;$p2=[$f,$o2];$q2=q#warning#;$r2=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$s2=[$q2,$r2];$t2=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$u2=[];$v2=[];$w2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$x2=bless({$t,$v2,$v,$w2,$x,$y},$z);$y2=bless({$n,$u2,$p,$q,$r,$q,$s,$x2},$B);$z2=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$A2=[];$B2=[];$C2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$D2=bless({$t,$B2,$v,$C2,$x,$y},$z);$E2=bless({$n,$A2,$p,$q,$r,$q,$s,$D2},$B);$F2=[$i,$t2,$y2,$z2,$E2];$G2=[$p2,$s2,$F2];$H2=q#/io/file#;$I2=bless({$e,$G2,$J,$H2},$L);$J2=q#ni.doc:/io/file_update_fd#;$K2=q#A write fd that performs a file rename upon closing.#;$L2=[$i,$K2];$M2=[$L2];$N2=q#/io/file_update_fd#;$O2=bless({$e,$M2,$J,$N2},$L);$P2=q#ni.doc:/io/pid#;$Q2=q#eg#;$R2=[];$S2=[];$T2=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$U2=bless({$t,$S2,$v,$T2,$x,$y},$z);$V2=bless({$n,$R2,$p,$q,$r,$q,$s,$U2},$B);$W2=[$Q2,$V2];$X2=[];$Y2=[];$Z2=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$c3=bless({$t,$Y2,$v,$Z2,$x,$y},$z);$d3=bless({$n,$X2,$p,$q,$r,$q,$s,$c3},$B);$e3=[$Q2,$d3];$f3=[];$g3=[];$h3=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$i3=bless({$t,$g3,$v,$h3,$x,$y},$z);$j3=bless({$n,$f3,$p,$q,$r,$q,$s,$i3},$B);$k3=[$Q2,$j3];$l3=[$W2,$e3,$k3];$m3=q#/io/pid#;$n3=bless({$e,$l3,$J,$m3},$L);$o3=q#ni.doc:/lib#;$p3=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$q3=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$r3=[$i,$p3,$q3];$s3=[$r3];$t3=q#/lib#;$u3=bless({$e,$s3,$J,$t3},$L);$v3=q#ni.doc:/lib/doc#;$w3=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ...#;$x3=[$f,$w3];$y3=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$z3=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$A3=[];$B3=[];$C3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$D3=bless({$t,$B3,$v,$C3,$x,$y},$z);$E3=bless({$n,$A3,$p,$q,$r,$q,$s,$D3},$B);$F3=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$G3=[];$H3=[];$I3=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$J3=bless({$t,$H3,$v,$I3,$x,$y},$z);$K3=bless({$n,$G3,$p,$q,$r,$q,$s,$J3},$B);$L3=[$i,$y3,$z3,$E3,$F3,$K3];$M3=[$x3,$L3];$N3=q#/lib/doc#;$O3=bless({$e,$M3,$J,$N3},$L);$P3=q#ni.doc:/lib/future#;$Q3=q#A value that doesn't yet exist, but is finalized once it does exist.#;$R3=[];$S3=[];$T3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$U3=bless({$t,$S3,$v,$T3,$x,$y},$z);$V3=bless({$n,$R3,$p,$q,$r,$q,$s,$U3},$B);$W3=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$X3=[];$Y3=[];$Z3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1, 2]];#;$c4=bless({$t,$Y3,$v,$Z3,$x,$y},$z);$d4=bless({$n,$X3,$p,$q,$r,$q,$s,$c4},$B);$e4=[$i,$Q3,$V3,$W3,$d4];$f4=[$e4];$g4=q#/lib/future#;$h4=bless({$e,$f4,$J,$g4},$L);$i4=q#ni.doc:/lib/image#;$j4=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$k4=[$f,$j4];$l4=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$m4=[$i,$l4];$n4=[$k4,$m4];$o4=q#/lib/image#;$p4=bless({$e,$n4,$J,$o4},$L);$q4=q#ni.doc:/lib/ni#;$r4=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$s4=[$f,$r4];$t4=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$u4=[$i,$t4];$v4=[$s4,$u4];$w4=q#/lib/ni#;$x4=bless({$e,$v4,$J,$w4},$L);$y4=q#ni.doc:/lib/quote_simple#;$z4=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$A4=[];$B4=[];$C4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$D4=bless({$t,$B4,$v,$C4,$x,$y},$z);$E4=bless({$n,$A4,$p,$q,$r,$q,$s,$D4},$B);$F4=[$i,$z4,$E4];$G4=[$F4];$H4=q#/lib/quote_simple#;$I4=bless({$e,$G4,$J,$H4},$L);$J4=q#ni.doc:/lib/slice#;$K4=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$L4=[$f,$K4];$M4=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$N4=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$O4=[];$P4=[];$Q4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$R4=bless({$t,$P4,$v,$Q4,$x,$y},$z);$S4=bless({$n,$O4,$p,$q,$r,$q,$s,$R4},$B);$T4=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$U4=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$V4=[];$W4=[];$X4=q#my $instances = 0;
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
now $instances == 0;#;$Y4=bless({$t,$W4,$v,$X4,$x,$y},$z);$Z4=bless({$n,$V4,$p,$q,$r,$q,$s,$Y4},$B);$c5=[$i,$M4,$N4,$S4,$T4,$U4,$Z4];$d5=[$L4,$c5];$e5=q#/lib/slice#;$f5=bless({$e,$d5,$J,$e5},$L);$g5=q#ni.doc:/semantic#;$h5=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$i5=[$i,$h5];$j5=[$i5];$k5=q#/semantic#;$l5=bless({$e,$j5,$J,$k5},$L);$m5=q#ni:/class#;$n5=q#applied_to#;$o5=q#class#;$p5=q#class.c#;$q5=q#fabric/perl.c#;$r5=q#fabric/perl_proxy.c#;$s5=q#io/buffer.c#;$t5=q#io/cat.c#;$u5=q#io/exec.c#;$v5=q#io/fd.c#;$w5=q#io/file.c#;$x5=q#io/file_update_fd.c#;$y5=q#io/null.c#;$z5=q#io/object.c#;$A5=q#io/pid.c#;$B5=q#io/str.c#;$C5=q#io/transfer.c#;$D5=q#io/transfer_async.c#;$E5=q#io/transfer_sync.c#;$F5=q#lib/behavior.c#;$G5=q#lib/branch.c#;$H5=q#lib/dataslice.c#;$I5=q#lib/doc.c#;$J5=q#lib/fn.c#;$K5=q#lib/future.c#;$L5=q#lib/image.c#;$M5=q#lib/ni.c#;$N5=q#lib/quote_simple.c#;$O5=q#lib/slice.c#;$P5=q#lib/tag.c#;$Q5=q#lib/test_assert_eq.c#;$R5=q#lib/test_assertion.c#;$S5=q#lib/test_case.c#;$T5=q#lib/test_value.c#;$U5=q#metaclass.c#;$V5=q#module.c#;$W5=q#object.c#;$X5=q#semantic/dimension#;$Y5=q#semantic/dimension.c#;$Z5=q#semantic/task.c#;$c6={$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1};$d6=q#slices#;$e6=q#metaclass#;$f6=q#module#;$g6={$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$e6,1,$U5,1,$f6,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1};$h6=q#/module#;$i6=q#/lib/perlbranch.b#;$j6={};$k6=q#ctor#;$l6=q#dtor#;$m6=q#methods#;$n6=q#add#;$o6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$p6=bless({$v,$o6},$z);$q6=q#apply#;$r6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$s6=bless({$v,$r6},$z);$t6={$n6,$p6,$q6,$s6};$u6=q#/lib/branch.b#;$v6=q#lib/slice#;$w6=bless({$n5,$j6,$k6,$q,$l6,$q,$m6,$t6,$J,$u6},$v6);$x6=q#lib/branch#;$y6={};$z6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$A6=bless({$v,$z6},$z);$B6={$J,$A6};$C6=q#/lib/named.b#;$D6=bless({$n5,$y6,$k6,$O,$l6,$q,$m6,$B6,$J,$C6},$v6);$E6=q#lib/tag#;$F6={};$G6=q#namespace#;$H6=q#'ni'#;$I6=bless({$v,$H6},$z);$J6={$G6,$I6};$K6=q#/lib/named_in_ni.b#;$L6=bless({$n5,$F6,$k6,$q,$l6,$q,$m6,$J6,$J,$K6},$v6);$M6={};$N6=q#package#;$O6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$P6=bless({$v,$O6},$z);$Q6={$N6,$P6};$R6=q#/lib/namespaced.b#;$S6=bless({$n5,$M6,$k6,$q,$l6,$q,$m6,$Q6,$J,$R6},$v6);$T6={};$U6=q#resolve#;$V6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$W6=bless({$v,$V6},$z);$X6={$U6,$W6};$Y6=q#/lib/resolver.b#;$Z6=bless({$n5,$T6,$k6,$q,$l6,$q,$m6,$X6,$J,$Y6},$v6);$c7=[$w6,$D6,$L6,$S6,$Z6];$d7=bless({$J,$i6,$d6,$c7},$E6);$e7={};$f7=q#my $s = shift; $s->apply($s->package)#;$g7=bless({$v,$f7,$x,$y},$z);$h7=q#instantiate#;$i7=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$j7=bless({$v,$i7},$z);$k7={$h7,$j7};$l7=q#/lib/class_init.b#;$m7=bless({$n5,$e7,$k6,$g7,$l6,$q,$m6,$k7,$J,$l7},$v6);$n7=q#fabric/perl#;$o7=q#fabric/perl_proxy#;$p7=q#io/buffer#;$q7=q#io/cat#;$r7=q#io/exec#;$s7=q#io/fd#;$t7=q#io/file#;$u7=q#io/file_update_fd#;$v7=q#io/null#;$w7=q#io/object#;$x7=q#io/pid#;$y7=q#io/str#;$z7=q#io/transfer#;$A7=q#io/transfer_async#;$B7=q#io/transfer_sync#;$C7=q#lib/behavior#;$D7=q#lib/dataslice#;$E7=q#lib/future#;$F7=q#lib/image#;$G7=q#lib/ni#;$H7=q#lib/quote_simple#;$I7=q#lib/test_assert_eq#;$J7=q#lib/test_assertion#;$K7=q#lib/test_value#;$L7=q#object#;$M7=q#semantic/task#;$N7={$o5,1,$p5,1,$n7,1,$q5,1,$o7,1,$r5,1,$p7,1,$s5,1,$q7,1,$t5,1,$r7,1,$u5,1,$s7,1,$v5,1,$t7,1,$w5,1,$u7,1,$x5,1,$v7,1,$y5,1,$w7,1,$z5,1,$x7,1,$A5,1,$y7,1,$B5,1,$z7,1,$C5,1,$A7,1,$D5,1,$B7,1,$E5,1,$C7,1,$F5,1,$x6,1,$G5,1,$D7,1,$H5,1,$L,1,$I5,1,$z,1,$J5,1,$E7,1,$K5,1,$F7,1,$L5,1,$G7,1,$M5,1,$H7,1,$N5,1,$v6,1,$O5,1,$E6,1,$P5,1,$I7,1,$Q5,1,$J7,1,$R5,1,$B,1,$S5,1,$K7,1,$T5,1,$e6,1,$U5,1,$f6,1,$V5,1,$L7,1,$W5,1,$X5,1,$Y5,1,$M7,1,$Z5,1};$O7=q#/object#;$P7={};$Q7=q#DESTROY#;$R7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$S7=bless({$v,$R7},$z);$T7=q#ni 'ni:/' . ref shift#;$U7=bless({$v,$T7},$z);$V7={$Q7,$S7,$o5,$U7};$W7=q#/lib/instance.b#;$X7=bless({$n5,$P7,$k6,$q,$l6,$q,$m6,$V7,$J,$W7},$v6);$Y7=[$X7];$Z7=bless({$n5,$N7,$J,$O7,$d6,$Y7},$W5);$c8={$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$C7,1,$F5,1,$x6,1,$G5,1,$D7,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$v6,1,$O5,1,$E6,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$e6,1,$U5,1,$f6,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1};$d8=q#/lib/behavior#;$e8={};$f8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$g8=bless({$v,$f8},$z);$h8={$e,$g8};$i8=q#/lib/documentable.b#;$j8=bless({$n5,$e8,$k6,$q,$l6,$q,$m6,$h8,$J,$i8},$v6);$k8=[$Z7,$j8];$l8=bless({$n5,$c8,$J,$d8,$d6,$k8},$F5);$m8={$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$x6,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$e6,1,$U5,1,$f6,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1};$n8=q#/lib/definition.b#;$o8={};$p8=q#def#;$q8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$r8=bless({$v,$q8},$z);$s8={$p8,$r8};$t8=q#/lib/definition_def.b#;$u8=bless({$n5,$o8,$k6,$q,$l6,$q,$m6,$s8,$J,$t8},$v6);$v8={};$w8=q#ro#;$x8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$y8=bless({$v,$x8},$z);$z8=q#rw#;$A8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$B8=bless({$v,$A8},$z);$C8={$w8,$y8,$z8,$B8};$D8=q#/lib/accessor.b#;$E8=bless({$n5,$v8,$k6,$q,$l6,$q,$m6,$C8,$J,$D8},$v6);$F8={};$G8=q#(""#;$H8=q#shift->name#;$I8=bless({$v,$H8},$z);$J8={$G8,$I8};$K8=q#/lib/name_as_string.b#;$L8=bless({$n5,$F8,$k6,$q,$l6,$q,$m6,$J8,$J,$K8},$v6);$M8={};$N8=q#(eq#;$O8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$P8=bless({$v,$O8},$z);$Q8={$N8,$P8};$R8=q#/lib/ref_eq.b#;$S8=bless({$n5,$M8,$k6,$q,$l6,$q,$m6,$Q8,$J,$R8},$v6);$T8={};$U8=q#defdata#;$V8=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$W8=bless({$v,$V8},$z);$X8={$U8,$W8};$Y8=q#/lib/definition_defdata.b#;$Z8=bless({$n5,$T8,$k6,$q,$l6,$q,$m6,$X8,$J,$Y8},$v6);$c9=[$u8,$E8,$L8,$S8,$Z8];$d9=bless({$n5,$m8,$J,$n8,$d6,$c9},$x6);$e9=[$d7,$m7,$Z7,$l8,$d9];$f9=bless({$n5,$g6,$J,$h6,$d6,$e9},$V5);$g9={};$h9=q#new#;$i9=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$j9=bless({$v,$i9},$z);$k9={$h9,$j9};$l9=q#/lib/instantiable.b#;$m9=bless({$n5,$g9,$m6,$k9,$J,$l9},$v6);$n9={};$o9=q#child#;$p9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$q9=bless({$v,$p9},$z);$r9={$o9,$q9};$s9=q#/lib/subclass.b#;$t9=bless({$n5,$n9,$k6,$q,$l6,$q,$m6,$r9,$J,$s9},$v6);$u9=[$f9,$m9,$m7,$f9,$t9];$v9=bless({$n5,$c6,$J,$K,$d6,$u9},$p5);$w9=q#ni:/class.c#;$x9={$p5,1,$Y5,1};$y9=q#/class.c#;$z9={$p5,1,$V5,1,$Y5,1};$A9=q#/module.c#;$B9={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$V5,1,$W5,1,$Y5,1,$Z5,1};$C9=q#/object.c#;$D9=[$v9];$E9=bless({$n5,$B9,$J,$C9,$d6,$D9},$e6);$F9={$p5,1,$F5,1,$G5,1,$H5,1,$O5,1,$P5,1,$V5,1,$Y5,1};$G9=q#/lib/behavior.c#;$H9=[$E9];$I9=bless({$n5,$F9,$J,$G9,$d6,$H9},$e6);$J9=[$E9,$m9,$I9];$K9=bless({$n5,$z9,$J,$A9,$d6,$J9},$e6);$L9=[$K9];$M9=bless({$n5,$x9,$J,$y9,$d6,$L9},$e6);$N9=q#ni:/fabric/perl#;$O9={$n7,1};$P9={};$Q9=[];$R9=q#my ($class, $stdin, $stdout) = @_;
+{stdin   => $stdin,
  stdout  => $stdout,
  pending => {}};#;$S9=bless({$t,$Q9,$v,$R9,$x,$y},$z);$T9={$h7,$S9};$U9=q#/fabric/perl_init.b#;$V9=bless({$n5,$P9,$k6,$q,$l6,$q,$m6,$T9,$J,$U9},$v6);$W9={};$X9=q#ni#;$Y9=[];$Z9=q#ni('/fabric/perl_proxy')->new(@_)#;$ca=bless({$t,$Y9,$v,$Z9,$x,$y},$z);$da={$X9,$ca};$ea=q#/fabric/perl_rmi.b#;$fa=bless({$n5,$W9,$k6,$q,$l6,$q,$m6,$da,$J,$ea},$v6);$ga=[$Z7,$V9,$fa];$ha=bless({$n5,$O9,$J,$d1,$d6,$ga},$q5);$ia=q#ni:/fabric/perl.c#;$ja={$q5,1};$ka=q#/fabric/perl.c#;$la=[$E9];$ma=bless({$n5,$ja,$J,$ka,$d6,$la},$e6);$na=q#ni:/fabric/perl_init.b#;$oa=q#ni:/fabric/perl_proxy#;$pa={$o7,1};$qa=q#/fabric/perl_proxy#;$ra={};$sa=[];$ta=q#my ($class, $perl, $name) = @_;
+{perl => $perl,
  name => $name};#;$ua=bless({$t,$sa,$v,$ta,$x,$y},$z);$va={$h7,$ua};$wa=q#/fabric/perl_proxy_init.b#;$xa=bless({$n5,$ra,$k6,$q,$l6,$q,$m6,$va,$J,$wa},$v6);$ya={};$za=q#AUTOLOAD#;$Aa=[];$Ba=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{perl}->rmi($$self{name}, $method, @_);#;$Ca=bless({$t,$Aa,$v,$Ba,$x,$y},$z);$Da={$za,$Ca};$Ea=q#/fabric/perl_proxy_rmi.b#;$Fa=bless({$n5,$ya,$k6,$q,$l6,$q,$m6,$Da,$J,$Ea},$v6);$Ga=[$Z7,$xa,$Fa];$Ha=bless({$n5,$pa,$J,$qa,$d6,$Ga},$r5);$Ia=q#ni:/fabric/perl_proxy.c#;$Ja={$r5,1};$Ka=q#/fabric/perl_proxy.c#;$La=[$E9];$Ma=bless({$n5,$Ja,$J,$Ka,$d6,$La},$e6);$Na=q#ni:/fabric/perl_proxy_init.b#;$Oa=q#ni:/fabric/perl_proxy_rmi.b#;$Pa=q#ni:/fabric/perl_rmi.b#;$Qa=q#ni:/io/buffer#;$Ra={$p7,1};$Sa={$p7,1,$q7,1,$r7,1,$s7,1,$t7,1,$u7,1,$v7,1,$w7,1,$x7,1,$y7,1};$Ta=q#/io/object#;$Ua={};$Va=q#(bool#;$Wa=[];$Xa=bless({$t,$Wa,$v,1,$x,$y},$z);$Ya={$Va,$Xa};$Za=q#/io/object_ops.b#;$cb=bless({$n5,$Ua,$k6,$q,$l6,$q,$m6,$Ya,$J,$Za},$v6);$db={};$eb=q#die#;$fb=[];$gb=q#shift; die join " ", @_#;$hb=bless({$t,$fb,$v,$gb,$x,$y},$z);$ib=q#io_check#;$jb=[];$kb=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$lb=bless({$t,$jb,$v,$kb,$x,$y},$z);$mb=q#io_check_defined#;$nb=[];$ob=q#shift->io_check(sub {defined shift}, @_)#;$pb=bless({$t,$nb,$v,$ob,$x,$y},$z);$qb=q#io_check_true#;$rb=[];$sb=q#shift->io_check(sub {shift}, @_)#;$tb=bless({$t,$rb,$v,$sb,$x,$y},$z);$ub={$eb,$hb,$ib,$lb,$mb,$pb,$qb,$tb};$vb=q#/io/object_checks.b#;$wb=bless({$n5,$db,$k6,$q,$l6,$q,$m6,$ub,$J,$vb},$v6);$xb={};$yb=q#(+#;$zb=[];$Ab=q#ni('ni:/io/cat')->new(@_[0, 1])#;$Bb=bless({$t,$zb,$v,$Ab,$x,$y},$z);$Cb={$yb,$Bb};$Db=q#/io/object_constructors.b#;$Eb=bless({$n5,$xb,$k6,$q,$l6,$q,$m6,$Cb,$J,$Db},$v6);$Fb={};$Gb=q#read_all#;$Hb=[];$Ib=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$Jb=bless({$t,$Hb,$v,$Ib,$x,$y},$z);$Kb=q#write_all#;$Lb=[];$Mb=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$Nb=bless({$t,$Lb,$v,$Mb,$x,$y},$z);$Ob={$Gb,$Jb,$Kb,$Nb};$Pb=q#/io/object_memory.b#;$Qb=bless({$n5,$Fb,$k6,$q,$l6,$q,$m6,$Ob,$J,$Pb},$v6);$Rb={};$Sb=q#connect_sync#;$Tb=[];$Ub=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$Vb=bless({$t,$Tb,$v,$Ub,$x,$y},$z);$Wb=q#into_sync#;$Xb=[];$Yb=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$Zb=bless({$t,$Xb,$v,$Yb,$x,$y},$z);$cc={$Sb,$Vb,$Wb,$Zb};$dc=q#/io/object_transfer_sync.b#;$ec=bless({$n5,$Rb,$k6,$q,$l6,$q,$m6,$cc,$J,$dc},$v6);$fc={};$gc=q#connect_async#;$hc=[];$ic=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$jc=bless({$t,$hc,$v,$ic,$x,$y},$z);$kc=q#into_async#;$lc=[];$mc=q#ni('ni:/io/transfer_async')->new(@_)->run#;$nc=bless({$t,$lc,$v,$mc,$x,$y},$z);$oc={$gc,$jc,$kc,$nc};$pc=q#/io/object_transfer_async.b#;$qc=bless({$n5,$fc,$k6,$q,$l6,$q,$m6,$oc,$J,$pc},$v6);$rc=[$Z7,$cb,$wb,$Eb,$Qb,$ec,$qc,$qc,$ec,$qc,$ec];$sc=bless({$n5,$Sa,$J,$Ta,$d6,$rc},$z5);$tc={};$uc=[];$vc=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$wc=bless({$t,$uc,$v,$vc,$x,$y},$z);$xc={$h7,$wc};$yc=q#/io/buffer_init.b#;$zc=bless({$n5,$tc,$k6,$q,$l6,$q,$m6,$xc,$J,$yc},$v6);$Ac={};$Bc=q#read#;$Cc=[];$Dc=q#my $self = shift;
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
}#;$Ec=bless({$t,$Cc,$v,$Dc,$x,$y},$z);$Fc=q#read_capacity#;$Gc=[];$Hc=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Ic=bless({$t,$Gc,$v,$Hc,$x,$y},$z);$Jc=q#write#;$Kc=[];$Lc=q#my $self = shift;
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
}#;$Mc=bless({$t,$Kc,$v,$Lc,$x,$y},$z);$Nc=q#write_capacity#;$Oc=[];$Pc=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Qc=bless({$t,$Oc,$v,$Pc,$x,$y},$z);$Rc={$Bc,$Ec,$Fc,$Ic,$Jc,$Mc,$Nc,$Qc};$Sc=q#/io/buffer_io.b#;$Tc=bless({$n5,$Ac,$k6,$q,$l6,$q,$m6,$Rc,$J,$Sc},$v6);$Uc=[$sc,$zc,$Tc];$Vc=bless({$n5,$Ra,$J,$w1,$d6,$Uc},$s5);$Wc=q#ni:/io/buffer.c#;$Xc={$s5,1};$Yc=q#/io/buffer.c#;$Zc={$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1};$cd=q#/io/object.c#;$dd={};$ed=q#def_transfer_method#;$fd=[];$gd=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$hd=bless({$t,$fd,$v,$gd,$x,$y},$z);$id={$ed,$hd};$jd=q#/io/object.c_transfer_def.b#;$kd=bless({$n5,$dd,$k6,$q,$l6,$q,$m6,$id,$J,$jd},$v6);$ld=[$E9,$kd];$md=bless({$n5,$Zc,$J,$cd,$d6,$ld},$e6);$nd=[$md];$od=bless({$n5,$Xc,$J,$Yc,$d6,$nd},$e6);$pd=q#ni:/io/buffer_init.b#;$qd=q#ni:/io/buffer_io.b#;$rd=q#ni:/io/cat#;$sd={$q7,1};$td={};$ud=[];$vd=q#shift; +{fs => [@_]}#;$wd=bless({$t,$ud,$v,$vd,$x,$y},$z);$xd={$h7,$wd};$yd=q#/io/cat_init.b#;$zd=bless({$n5,$td,$k6,$q,$l6,$q,$m6,$xd,$J,$yd},$v6);$Ad={};$Bd=[];$Cd=q#my $fs = shift->{fs};
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
$total_read;#;$Dd=bless({$t,$Bd,$v,$Cd,$x,$y},$z);$Ed={$Bc,$Dd};$Fd=q#/io/cat_read.b#;$Gd=bless({$n5,$Ad,$k6,$q,$l6,$q,$m6,$Ed,$J,$Fd},$v6);$Hd=[$sc,$zd,$Gd];$Id=bless({$n5,$sd,$J,$J1,$d6,$Hd},$t5);$Jd=q#ni:/io/cat.c#;$Kd={$t5,1};$Ld=q#/io/cat.c#;$Md=[$md];$Nd=bless({$n5,$Kd,$J,$Ld,$d6,$Md},$e6);$Od=q#ni:/io/cat_init.b#;$Pd=q#ni:/io/cat_read.b#;$Qd=q#ni:/io/exec#;$Rd={$r7,1};$Sd={};$Td=q#argv#;$Ud=[];$Vd=q#shift->{'argv'}#;$Wd=bless({$t,$Ud,$v,$Vd,$x,$y},$z);$Xd={$Td,$Wd};$Yd=q#/io/exec_ro.b#;$Zd=bless({$n5,$Sd,$k6,$q,$l6,$q,$m6,$Xd,$J,$Yd},$v6);$ce={};$de=[];$ee=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$fe=bless({$t,$de,$v,$ee,$x,$y},$z);$ge={$h7,$fe};$he=q#/io/exec_init.b#;$ie=bless({$n5,$ce,$k6,$q,$l6,$q,$m6,$ge,$J,$he},$v6);$je={};$ke=q#connect#;$le=[];$me=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$ne=bless({$t,$le,$v,$me,$x,$y},$z);$oe=q#in_pipe#;$pe=[];$qe=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$re=bless({$t,$pe,$v,$qe,$x,$y},$z);$se=q#out_pipe#;$te=[];$ue=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$ve=bless({$t,$te,$v,$ue,$x,$y},$z);$we=q#setup_stdio#;$xe=[];$ye=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$ze=bless({$t,$xe,$v,$ye,$x,$y},$z);$Ae={$ke,$ne,$oe,$re,$se,$ve,$we,$ze};$Be=q#/io/exec_io_setup.b#;$Ce=bless({$n5,$je,$k6,$q,$l6,$q,$m6,$Ae,$J,$Be},$v6);$De={};$Ee=q#binds_fd#;$Fe=[];$Ge=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$He=bless({$t,$Fe,$v,$Ge,$x,$y},$z);$Ie=q#fd#;$Je=[];$Ke=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Le=bless({$t,$Je,$v,$Ke,$x,$y},$z);$Me=q#stderr#;$Ne=[];$Oe=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Pe=bless({$t,$Ne,$v,$Oe,$x,$y},$z);$Qe=q#stdin#;$Re=[];$Se=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Te=bless({$t,$Re,$v,$Se,$x,$y},$z);$Ue=q#stdout#;$Ve=[];$We=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Xe=bless({$t,$Ve,$v,$We,$x,$y},$z);$Ye={$Ee,$He,$Ie,$Le,$Me,$Pe,$Qe,$Te,$Ue,$Xe};$Ze=q#/io/exec_io_accessors.b#;$cf=bless({$n5,$De,$k6,$q,$l6,$q,$m6,$Ye,$J,$Ze},$v6);$df={};$ef=q#env#;$ff=[];$gf=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$hf=bless({$t,$ff,$v,$gf,$x,$y},$z);$if={$ef,$hf};$jf=q#/io/exec_env.b#;$kf=bless({$n5,$df,$k6,$q,$l6,$q,$m6,$if,$J,$jf},$v6);$lf={};$mf=q#exec#;$nf=[];$of=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$pf=bless({$t,$nf,$v,$of,$x,$y},$z);$qf=q#fork#;$rf=[];$sf=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$tf=bless({$t,$rf,$v,$sf,$x,$y},$z);$uf=q#move_fds#;$vf=[];$wf=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$xf=bless({$t,$vf,$v,$wf,$x,$y},$z);$yf={$mf,$pf,$qf,$tf,$uf,$xf};$zf=q#/io/exec_fork.b#;$Af=bless({$n5,$lf,$k6,$q,$l6,$q,$m6,$yf,$J,$zf},$v6);$Bf=[$sc,$Zd,$ie,$Ce,$cf,$kf,$Af];$Cf=bless({$n5,$Rd,$J,$W1,$d6,$Bf},$u5);$Df=q#ni:/io/exec.c#;$Ef={$u5,1};$Ff=q#/io/exec.c#;$Gf=[$md];$Hf=bless({$n5,$Ef,$J,$Ff,$d6,$Gf},$e6);$If=q#ni:/io/exec_env.b#;$Jf=q#ni:/io/exec_fork.b#;$Kf=q#ni:/io/exec_init.b#;$Lf=q#ni:/io/exec_io_accessors.b#;$Mf=q#ni:/io/exec_io_setup.b#;$Nf=q#ni:/io/exec_ro.b#;$Of=q#ni:/io/fd#;$Pf={$s7,1};$Qf=q#read_fd_mask#;$Rf={};$Sf=[];$Tf=q#shift->{'fd'}#;$Uf=bless({$t,$Sf,$v,$Tf,$x,$y},$z);$Vf={$Ie,$Uf};$Wf=q#/io/fd_readers.b#;$Xf=bless({$n5,$Rf,$k6,$q,$l6,$q,$m6,$Vf,$J,$Wf},$v6);$Yf={};$Zf=[];$cg=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$dg=bless({$t,$Zf,$v,$cg,$x,$y},$z);$eg={$h7,$dg};$fg=q#/io/fd_init.b#;$gg=bless({$n5,$Yf,$k6,$q,$l6,$q,$m6,$eg,$J,$fg},$v6);$hg={};$ig=q#be#;$jg=[];$kg=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$lg=bless({$t,$jg,$v,$kg,$x,$y},$z);$mg={$ig,$lg};$ng=q#/io/fd_shell.b#;$og=bless({$n5,$hg,$k6,$q,$l6,$q,$m6,$mg,$J,$ng},$v6);$pg={};$qg=q#cloexec#;$rg=[];$sg=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$tg=bless({$t,$rg,$v,$sg,$x,$y},$z);$ug=q#fcntl_flag#;$vg=[];$wg=q#my ($self, $flag, $value) = @_;
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
}#;$xg=bless({$t,$vg,$v,$wg,$x,$y},$z);$yg=q#nonblock#;$zg=[];$Ag=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Bg=bless({$t,$zg,$v,$Ag,$x,$y},$z);$Cg={$qg,$tg,$ug,$xg,$yg,$Bg};$Dg=q#/io/fd_fcntl.b#;$Eg=bless({$n5,$pg,$k6,$q,$l6,$q,$m6,$Cg,$J,$Dg},$v6);$Fg={};$Gg=[];$Hg=q#shift->close#;$Ig=bless({$t,$Gg,$v,$Hg,$x,$y},$z);$Jg=q#close#;$Kg=[];$Lg=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Mg=bless({$t,$Kg,$v,$Lg,$x,$y},$z);$Ng={$Jg,$Mg};$Og=q#/io/fd_gc.b#;$Pg=bless({$n5,$Fg,$k6,$q,$l6,$Ig,$m6,$Ng,$J,$Og},$v6);$Qg={};$Rg=q#close_perl_ios#;$Sg=[];$Tg=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Ug=bless({$t,$Sg,$v,$Tg,$x,$y},$z);$Vg=[];$Wg=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Xg=bless({$t,$Vg,$v,$Wg,$x,$y},$z);$Yg=[];$Zg=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$ch=bless({$t,$Yg,$v,$Zg,$x,$y},$z);$dh={$Rg,$Ug,$Bc,$Xg,$Jc,$ch};$eh=q#/io/fd_perlio.b#;$fh=bless({$n5,$Qg,$k6,$q,$l6,$q,$m6,$dh,$J,$eh},$v6);$gh=[$sc,$Xf,$gg,$og,$Eg,$Pg,$fh];$hh=q#write_fd_mask#;$ih=bless({$n5,$Pf,$J,$l2,$Qf,$y,$d6,$gh,$hh,$y},$v5);$jh=[];$kh=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$lh=bless({$t,$jh,$v,$kh,$x,$y},$z);$mh=q#ni:/io/fd.c#;$nh={$v5,1};$oh=q#/io/fd.c#;$ph={};$qh=q#clear_fd#;$rh=[];$sh=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$th=bless({$t,$rh,$v,$sh,$x,$y},$z);$uh=q#read_fd#;$vh=[];$wh=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$xh=bless({$t,$vh,$v,$wh,$x,$y},$z);$yh=q#select#;$zh=[];$Ah=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Bh=bless({$t,$zh,$v,$Ah,$x,$y},$z);$Ch=q#write_fd#;$Dh=[];$Eh=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Fh=bless({$t,$Dh,$v,$Eh,$x,$y},$z);$Gh={$qh,$th,$uh,$xh,$yh,$Bh,$Ch,$Fh};$Hh=q#/io/fd.c_selector.b#;$Ih=bless({$n5,$ph,$k6,$lh,$l6,$q,$m6,$Gh,$J,$Hh},$v6);$Jh=[$md,$Ih];$Kh=bless({$n5,$nh,$J,$oh,$d6,$Jh},$e6);$Lh=q#ni:/io/fd.c_selector.b#;$Mh=q#ni:/io/fd_fcntl.b#;$Nh=q#ni:/io/fd_gc.b#;$Oh=q#ni:/io/fd_init.b#;$Ph=q#ni:/io/fd_perlio.b#;$Qh=q#ni:/io/fd_readers.b#;$Rh=q#ni:/io/fd_shell.b#;$Sh=q#ni:/io/file#;$Th={$t7,1};$Uh={};$Vh=[];$Wh=q#shift->{'name'}#;$Xh=bless({$t,$Vh,$v,$Wh,$x,$y},$z);$Yh={$J,$Xh};$Zh=q#/io/file_readers.b#;$ci=bless({$n5,$Uh,$k6,$q,$l6,$q,$m6,$Yh,$J,$Zh},$v6);$di={};$ei=q#mode#;$fi=[];$gi=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$hi=bless({$t,$fi,$v,$gi,$x,$y},$z);$ii={$ei,$hi};$ji=q#/io/file_accessors.b#;$ki=bless({$n5,$di,$k6,$q,$l6,$q,$m6,$ii,$J,$ji},$v6);$li={};$mi=[];$ni=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$oi=bless({$t,$mi,$v,$ni,$x,$y},$z);$pi={$h7,$oi};$qi=q#/io/file_init.b#;$ri=bless({$n5,$li,$k6,$q,$l6,$q,$m6,$pi,$J,$qi},$v6);$si={};$ti=q#(-X#;$ui=[];$vi=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$wi=bless({$t,$ui,$v,$vi,$x,$y},$z);$xi=q#mv#;$yi=[];$zi=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Ai=bless({$t,$yi,$v,$zi,$x,$y},$z);$Bi=q#rm#;$Ci=[];$Di=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Ei=bless({$t,$Ci,$v,$Di,$x,$y},$z);$Fi={$ti,$wi,$xi,$Ai,$Bi,$Ei};$Gi=q#/io/file_fns.b#;$Hi=bless({$n5,$si,$k6,$q,$l6,$q,$m6,$Fi,$J,$Gi},$v6);$Ii={};$Ji=q#atomic_update#;$Ki=[];$Li=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Mi=bless({$t,$Ki,$v,$Li,$x,$y},$z);$Ni={$Ji,$Mi};$Oi=q#/io/file_update.b#;$Pi=bless({$n5,$Ii,$k6,$q,$l6,$q,$m6,$Ni,$J,$Oi},$v6);$Qi={};$Ri=[];$Si=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Ti=bless({$t,$Ri,$v,$Si,$x,$y},$z);$Ui=q#r#;$Vi=[];$Wi=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Xi=bless({$t,$Vi,$v,$Wi,$x,$y},$z);$Yi=[];$Zi=q#shift->r->read(@_)#;$cj=bless({$t,$Yi,$v,$Zi,$x,$y},$z);$dj=q#w#;$ej=[];$fj=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$gj=bless({$t,$ej,$v,$fj,$x,$y},$z);$hj=[];$ij=q#shift->w->write(@_)#;$jj=bless({$t,$hj,$v,$ij,$x,$y},$z);$kj={$Jg,$Ti,$Ui,$Xi,$Bc,$cj,$dj,$gj,$Jc,$jj};$lj=q#/io/file_io.b#;$mj=bless({$n5,$Qi,$k6,$q,$l6,$q,$m6,$kj,$J,$lj},$v6);$nj=[$sc,$ci,$ki,$ri,$Hi,$Pi,$mj];$oj=bless({$n5,$Th,$J,$H2,$d6,$nj},$w5);$pj=q#ni:/io/file.c#;$qj={$w5,1};$rj=q#/io/file.c#;$sj=[$md];$tj=bless({$n5,$qj,$J,$rj,$d6,$sj},$e6);$uj=q#ni:/io/file_accessors.b#;$vj=q#ni:/io/file_fns.b#;$wj=q#ni:/io/file_init.b#;$xj=q#ni:/io/file_io.b#;$yj=q#ni:/io/file_readers.b#;$zj=q#ni:/io/file_update.b#;$Aj=q#ni:/io/file_update_fd#;$Bj={$u7,1};$Cj={};$Dj=[];$Ej=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Fj=bless({$t,$Dj,$v,$Ej,$x,$y},$z);$Gj={$h7,$Fj};$Hj=q#/io/file_update_fd_init.b#;$Ij=bless({$n5,$Cj,$k6,$q,$l6,$q,$m6,$Gj,$J,$Hj},$v6);$Jj={};$Kj=[];$Lj=bless({$t,$Kj,$v,$Hg,$x,$y},$z);$Mj=[];$Nj=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Oj=bless({$t,$Mj,$v,$Nj,$x,$y},$z);$Pj={$Jg,$Oj};$Qj=q#/io/file_update_fd_gc.b#;$Rj=bless({$n5,$Jj,$k6,$q,$l6,$Lj,$m6,$Pj,$J,$Qj},$v6);$Sj=[$sc,$Xf,$Eg,$fh,$Ij,$Rj];$Tj=bless({$n5,$Bj,$J,$N2,$d6,$Sj},$x5);$Uj=q#ni:/io/file_update_fd.c#;$Vj={$x5,1};$Wj=q#/io/file_update_fd.c#;$Xj=[$md];$Yj=bless({$n5,$Vj,$J,$Wj,$d6,$Xj},$e6);$Zj=q#ni:/io/file_update_fd_gc.b#;$ck=q#ni:/io/file_update_fd_init.b#;$dk=q#ni:/io/named_io_fns.b#;$ek={};$fk=q#fcntl#;$gk=[];$hk=q#CORE::fcntl $_[0], $_[1], $_[2]#;$ik=bless({$t,$gk,$v,$hk,$x,$y},$z);$jk=[];$kk=q#CORE::fork#;$lk=bless({$t,$jk,$v,$kk,$x,$y},$z);$mk=q#open2#;$nk=[];$ok=q#CORE::open $_[0], $_[1]#;$pk=bless({$t,$nk,$v,$ok,$x,$y},$z);$qk=q#rename#;$rk=[];$sk=q#CORE::rename $_[0], $_[1]#;$tk=bless({$t,$rk,$v,$sk,$x,$y},$z);$uk=q#unlink#;$vk=[];$wk=q#CORE::unlink @_#;$xk=bless({$t,$vk,$v,$wk,$x,$y},$z);$yk=q#waitpid#;$zk=[];$Ak=q#CORE::waitpid $_[0], $_[1]#;$Bk=bless({$t,$zk,$v,$Ak,$x,$y},$z);$Ck={$fk,$ik,$qf,$lk,$mk,$pk,$qk,$tk,$uk,$xk,$yk,$Bk};$Dk=q#/io/named_io_fns.b#;$Ek=bless({$n5,$ek,$k6,$q,$l6,$q,$m6,$Ck,$J,$Dk},$v6);$Fk=q#main#;$Gk=q#ni:/io/null#;$Hk={$v7,1};$Ik=q#/io/null#;$Jk={};$Kk=[];$Lk=q#+{fd => undef}#;$Mk=bless({$t,$Kk,$v,$Lk,$x,$y},$z);$Nk={$h7,$Mk};$Ok=q#/io/null_init.b#;$Pk=bless({$n5,$Jk,$k6,$q,$l6,$q,$m6,$Nk,$J,$Ok},$v6);$Qk={};$Rk=[];$Sk=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Tk=bless({$t,$Rk,$v,$Sk,$x,$y},$z);$Uk=[];$Vk=q#shift->fd->read(@_)#;$Wk=bless({$t,$Uk,$v,$Vk,$x,$y},$z);$Xk=[];$Yk=q#shift->fd->write(@_)#;$Zk=bless({$t,$Xk,$v,$Yk,$x,$y},$z);$cl={$Ie,$Tk,$Bc,$Wk,$Jc,$Zk};$dl=q#/io/null_io.b#;$el=bless({$n5,$Qk,$k6,$q,$l6,$q,$m6,$cl,$J,$dl},$v6);$fl=[$sc,$Pk,$el];$gl=bless({$n5,$Hk,$J,$Ik,$d6,$fl},$y5);$hl=q#ni:/io/null.c#;$il={$y5,1};$jl=q#/io/null.c#;$kl=[$md];$ll=bless({$n5,$il,$J,$jl,$d6,$kl},$e6);$ml=q#ni:/io/null_init.b#;$nl=q#ni:/io/null_io.b#;$ol=q#ni:/io/object#;$pl=q#ni:/io/object.c#;$ql=q#ni:/io/object.c_transfer_def.b#;$rl=q#ni:/io/object_checks.b#;$sl=q#ni:/io/object_constructors.b#;$tl=q#ni:/io/object_memory.b#;$ul=q#ni:/io/object_ops.b#;$vl=q#ni:/io/object_transfer_async.b#;$wl=q#ni:/io/object_transfer_sync.b#;$xl=q#ni:/io/pid#;$yl={$x7,1};$zl={};$Al=q#pid#;$Bl=[];$Cl=q#shift->{'pid'}#;$Dl=bless({$t,$Bl,$v,$Cl,$x,$y},$z);$El=q#status#;$Fl=[];$Gl=q#shift->{'status'}#;$Hl=bless({$t,$Fl,$v,$Gl,$x,$y},$z);$Il={$Al,$Dl,$El,$Hl};$Jl=q#/io/pid_readers.b#;$Kl=bless({$n5,$zl,$k6,$q,$l6,$q,$m6,$Il,$J,$Jl},$v6);$Ll={};$Ml=[];$Nl=q#shift->await#;$Ol=bless({$t,$Ml,$v,$Nl,$x,$y},$z);$Pl=[];$Ql=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$Rl=bless({$t,$Pl,$v,$Ql,$x,$y},$z);$Sl={$h7,$Rl};$Tl=q#/io/pid_init.b#;$Ul=bless({$n5,$Ll,$k6,$q,$l6,$Ol,$m6,$Sl,$J,$Tl},$v6);$Vl={};$Wl=q#await#;$Xl=[];$Yl=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$Zl=bless({$t,$Xl,$v,$Yl,$x,$y},$z);$cm=q#running#;$dm=[];$em=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$fm=bless({$t,$dm,$v,$em,$x,$y},$z);$gm={$Wl,$Zl,$cm,$fm};$hm=q#/io/pid_wait.b#;$im=bless({$n5,$Vl,$k6,$q,$l6,$q,$m6,$gm,$J,$hm},$v6);$jm={};$km=[];$lm=q#shift->stdout->read(@_)#;$mm=bless({$t,$km,$v,$lm,$x,$y},$z);$nm=[];$om=q#shift->stdin->write(@_)#;$pm=bless({$t,$nm,$v,$om,$x,$y},$z);$qm={$Bc,$mm,$Jc,$pm};$rm=q#/io/pid_io.b#;$sm=bless({$n5,$jm,$k6,$q,$l6,$q,$m6,$qm,$J,$rm},$v6);$tm={};$um=[];$vm=q#$_[0]->{external_fds}{$_[1]}#;$wm=bless({$t,$um,$v,$vm,$x,$y},$z);$xm=[];$ym=q#shift->fd(2)#;$zm=bless({$t,$xm,$v,$ym,$x,$y},$z);$Am=[];$Bm=q#shift->fd(0)#;$Cm=bless({$t,$Am,$v,$Bm,$x,$y},$z);$Dm=[];$Em=q#shift->fd(1)#;$Fm=bless({$t,$Dm,$v,$Em,$x,$y},$z);$Gm={$Ie,$wm,$Me,$zm,$Qe,$Cm,$Ue,$Fm};$Hm=q#/io/pid_accessors.b#;$Im=bless({$n5,$tm,$k6,$q,$l6,$q,$m6,$Gm,$J,$Hm},$v6);$Jm=[$sc,$Kl,$Ul,$im,$sm,$Im];$Km=bless({$n5,$yl,$J,$m3,$d6,$Jm},$A5);$Lm=q#ni:/io/pid.c#;$Mm={$A5,1};$Nm=q#/io/pid.c#;$Om=[$md];$Pm=bless({$n5,$Mm,$J,$Nm,$d6,$Om},$e6);$Qm=q#ni:/io/pid_accessors.b#;$Rm=q#ni:/io/pid_init.b#;$Sm=q#ni:/io/pid_io.b#;$Tm=q#ni:/io/pid_readers.b#;$Um=q#ni:/io/pid_wait.b#;$Vm=q#ni:/io/str#;$Wm={$y7,1};$Xm=q#/io/str#;$Ym={};$Zm=q#data#;$cn=[];$dn=q#shift->{'data'}#;$en=bless({$t,$cn,$v,$dn,$x,$y},$z);$fn=q#end#;$gn=[];$hn=q#shift->{'end'}#;$in=bless({$t,$gn,$v,$hn,$x,$y},$z);$jn=q#start#;$kn=[];$ln=q#shift->{'start'}#;$mn=bless({$t,$kn,$v,$ln,$x,$y},$z);$nn={$Zm,$en,$fn,$in,$jn,$mn};$on=q#/io/str_ro.b#;$pn=bless({$n5,$Ym,$k6,$q,$l6,$q,$m6,$nn,$J,$on},$v6);$qn={};$rn=[];$sn=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$tn=bless({$t,$rn,$v,$sn,$x,$y},$z);$un={$h7,$tn};$vn=q#/io/str_init.b#;$wn=bless({$n5,$qn,$k6,$q,$l6,$q,$m6,$un,$J,$vn},$v6);$xn={};$yn=[];$zn=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$An=bless({$t,$yn,$v,$zn,$x,$y},$z);$Bn=q#remaining#;$Cn=[];$Dn=q#my $self = shift; $$self{end} - $$self{start}#;$En=bless({$t,$Cn,$v,$Dn,$x,$y},$z);$Fn=[];$Gn=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Hn=bless({$t,$Fn,$v,$Gn,$x,$y},$z);$In={$Bc,$An,$Bn,$En,$Jc,$Hn};$Jn=q#/io/str_io.b#;$Kn=bless({$n5,$xn,$k6,$q,$l6,$q,$m6,$In,$J,$Jn},$v6);$Ln=[$sc,$pn,$wn,$Kn];$Mn=bless({$n5,$Wm,$J,$Xm,$d6,$Ln},$B5);$Nn=q#ni:/io/str.c#;$On={$B5,1};$Pn=q#/io/str.c#;$Qn=[$md];$Rn=bless({$n5,$On,$J,$Pn,$d6,$Qn},$e6);$Sn=q#ni:/io/str_init.b#;$Tn=q#ni:/io/str_io.b#;$Un=q#ni:/io/str_ro.b#;$Vn=q#ni:/io/transfer#;$Wn={$z7,1,$A7,1,$B7,1};$Xn=q#/io/transfer#;$Yn={$z7,1,$A7,1,$B7,1,$M7,1};$Zn=q#/semantic/task#;$co={};$do=[];$eo=q#shift->{'outcome'}#;$fo=bless({$t,$do,$v,$eo,$x,$y},$z);$go={$r,$fo};$ho=q#/semantic/task_ro.b#;$io=bless({$n5,$co,$k6,$q,$l6,$q,$m6,$go,$J,$ho},$v6);$jo={};$ko=q#failure#;$lo=[];$mo=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$no=bless({$t,$lo,$v,$mo,$x,$y},$z);$oo=q#success#;$po=[];$qo=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$ro=bless({$t,$po,$v,$qo,$x,$y},$z);$so={$ko,$no,$oo,$ro};$to=q#/semantic/task_outcome.b#;$uo=bless({$n5,$jo,$k6,$q,$l6,$q,$m6,$so,$J,$to},$v6);$vo=[$Z7,$io,$uo];$wo=bless({$n5,$Yn,$J,$Zn,$d6,$vo},$Z5);$xo={};$yo=[];$zo=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Ao=bless({$t,$yo,$v,$zo,$x,$y},$z);$Bo=[];$Co=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Do=bless({$t,$Bo,$v,$Co,$x,$y},$z);$Eo=[];$Fo=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Go=bless({$t,$Eo,$v,$Fo,$x,$y},$z);$Ho={$Bc,$Do,$Jc,$Go};$Io=q#/io/transfer_io_interop.b#;$Jo=bless({$n5,$xo,$k6,$Ao,$l6,$q,$m6,$Ho,$J,$Io},$v6);$Ko={};$Lo=q#pressure#;$Mo=[];$No=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Oo=bless({$t,$Mo,$v,$No,$x,$y},$z);$Po=q#read_limit_throughput#;$Qo=[];$Ro=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$So=bless({$t,$Qo,$v,$Ro,$x,$y},$z);$To=q#throughput#;$Uo=[];$Vo=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$Wo=bless({$t,$Uo,$v,$Vo,$x,$y},$z);$Xo=q#write_limit_throughput#;$Yo=[];$Zo=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$cp=bless({$t,$Yo,$v,$Zo,$x,$y},$z);$dp={$Lo,$Oo,$Po,$So,$To,$Wo,$Xo,$cp};$ep=q#/io/transfer_io_measurement.b#;$fp=bless({$n5,$Ko,$k6,$q,$l6,$q,$m6,$dp,$J,$ep},$v6);$gp=[$wo,$Jo,$fp];$hp=bless({$n5,$Wn,$J,$Xn,$d6,$gp},$C5);$ip=[];$jp=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$kp=bless({$t,$ip,$v,$jp,$x,$y},$z);$lp=q#ni:/io/transfer.c#;$mp={$C5,1,$D5,1,$E5,1};$np=q#/io/transfer.c#;$op={$C5,1,$D5,1,$E5,1,$Z5,1};$pp=q#/semantic/task.c#;$qp=[$E9];$rp=bless({$n5,$op,$J,$pp,$d6,$qp},$e6);$sp={};$tp={};$up=q#/io/transfer.c_into.b#;$vp=bless({$n5,$sp,$k6,$kp,$l6,$q,$m6,$tp,$J,$up},$v6);$wp=[$rp,$vp];$xp=bless({$n5,$mp,$J,$np,$d6,$wp},$e6);$yp=q#ni:/io/transfer.c_into.b#;$zp=q#ni:/io/transfer_async#;$Ap={$A7,1};$Bp=q#/io/transfer_async#;$Cp={};$Dp=q#dest_io#;$Ep=[];$Fp=q#shift->{'dest_io'}#;$Gp=bless({$t,$Ep,$v,$Fp,$x,$y},$z);$Hp=q#id#;$Ip=[];$Jp=q#shift->{'id'}#;$Kp=bless({$t,$Ip,$v,$Jp,$x,$y},$z);$Lp=q#source_io#;$Mp=[];$Np=q#shift->{'source_io'}#;$Op=bless({$t,$Mp,$v,$Np,$x,$y},$z);$Pp={$Dp,$Gp,$Hp,$Kp,$Lp,$Op};$Qp=q#/io/transfer_async_ro.b#;$Rp=bless({$n5,$Cp,$k6,$q,$l6,$q,$m6,$Pp,$J,$Qp},$v6);$Sp={};$Tp=[];$Up=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Vp=bless({$t,$Tp,$v,$Up,$x,$y},$z);$Wp={$h7,$Vp};$Xp=q#/io/transfer_async_init.b#;$Yp=bless({$n5,$Sp,$k6,$q,$l6,$q,$m6,$Wp,$J,$Xp},$v6);$Zp={};$cq=[];$dq=q#ni('ni:/io/transfer_async')->track(shift)#;$eq=bless({$t,$cq,$v,$dq,$x,$y},$z);$fq=[];$gq=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$hq=bless({$t,$fq,$v,$gq,$x,$y},$z);$iq={};$jq=q#/io/transfer_async_lifecycle.b#;$kq=bless({$n5,$Zp,$k6,$eq,$l6,$hq,$m6,$iq,$J,$jq},$v6);$lq={};$mq=q#run#;$nq=[];$oq=q#shift#;$pq=bless({$t,$nq,$v,$oq,$x,$y},$z);$qq=q#run_async#;$rq=[];$sq=q#my $self = shift;
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

$self;#;$tq=bless({$t,$rq,$v,$sq,$x,$y},$z);$uq={$mq,$pq,$qq,$tq};$vq=q#/io/transfer_async_run.b#;$wq=bless({$n5,$lq,$k6,$q,$l6,$q,$m6,$uq,$J,$vq},$v6);$xq=[$hp,$Rp,$Yp,$kq,$wq];$yq=q#tracked_transfers#;$zq={};$Aq=q#transfer_id#;$Bq=bless({$n5,$Ap,$J,$Bp,$d6,$xq,$yq,$zq,$Aq,0},$D5);$Cq=[];$Dq=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Eq=bless({$t,$Cq,$v,$Dq,$x,$y},$z);$Fq=q#ni:/io/transfer_async.c#;$Gq={$D5,1};$Hq=q#/io/transfer_async.c#;$Iq={};$Jq=q#new_id#;$Kq=[];$Lq=q#++shift->{transfer_id}#;$Mq=bless({$t,$Kq,$v,$Lq,$x,$y},$z);$Nq=q#track#;$Oq=[];$Pq=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Qq=bless({$t,$Oq,$v,$Pq,$x,$y},$z);$Rq=q#untrack#;$Sq=[];$Tq=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Uq=bless({$t,$Sq,$v,$Tq,$x,$y},$z);$Vq={$Jq,$Mq,$Nq,$Qq,$Rq,$Uq};$Wq=q#/io/transfer_async.c_tracker.b#;$Xq=bless({$n5,$Iq,$k6,$Eq,$l6,$q,$m6,$Vq,$J,$Wq},$v6);$Yq=[$xp,$Xq];$Zq=bless({$n5,$Gq,$J,$Hq,$d6,$Yq},$e6);$cr=q#ni:/io/transfer_async.c_tracker.b#;$dr=q#ni:/io/transfer_async_init.b#;$er=q#ni:/io/transfer_async_lifecycle.b#;$fr=q#ni:/io/transfer_async_ro.b#;$gr=q#ni:/io/transfer_async_run.b#;$hr=q#ni:/io/transfer_io_interop.b#;$ir=q#ni:/io/transfer_io_measurement.b#;$jr=q#ni:/io/transfer_sync#;$kr={$B7,1};$lr=q#/io/transfer_sync#;$mr={};$nr=[];$or=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$pr=bless({$t,$nr,$v,$or,$x,$y},$z);$qr={$h7,$pr};$rr=q#/io/transfer_sync_init.b#;$sr=bless({$n5,$mr,$k6,$q,$l6,$q,$m6,$qr,$J,$rr},$v6);$tr={};$ur=[];$vr=q#my $self = shift;
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
$self->success;#;$wr=bless({$t,$ur,$v,$vr,$x,$y},$z);$xr={$mq,$wr};$yr=q#/io/transfer_sync_run.b#;$zr=bless({$n5,$tr,$k6,$q,$l6,$q,$m6,$xr,$J,$yr},$v6);$Ar=[$hp,$sr,$zr];$Br=bless({$n5,$kr,$J,$lr,$d6,$Ar},$E5);$Cr=q#ni:/io/transfer_sync.c#;$Dr={$E5,1};$Er=q#/io/transfer_sync.c#;$Fr=[$xp];$Gr=bless({$n5,$Dr,$J,$Er,$d6,$Fr},$e6);$Hr=q#ni:/io/transfer_sync_init.b#;$Ir=q#ni:/io/transfer_sync_run.b#;$Jr=q#ni:/lib/accessor.b#;$Kr=q#ni:/lib/behavior#;$Lr=q#ni:/lib/behavior.c#;$Mr=q#ni:/lib/branch#;$Nr={$x6,1};$Or=q#/lib/branch#;$Pr={};$Qr=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Rr=bless({$v,$Qr},$z);$Sr={$h7,$Rr};$Tr=q#/lib/branch_init.b#;$Ur=bless({$n5,$Pr,$k6,$q,$l6,$q,$m6,$Sr,$J,$Tr},$v6);$Vr=[$l8,$D6,$w6,$Ur,$d9];$Wr=bless({$n5,$Nr,$J,$Or,$d6,$Vr},$G5);$Xr=q#ni:/lib/branch.b#;$Yr=q#ni:/lib/branch.c#;$Zr={$G5,1};$cs=q#/lib/branch.c#;$ds=[$I9];$es=bless({$n5,$Zr,$J,$cs,$d6,$ds},$e6);$fs=q#ni:/lib/branch_init.b#;$gs=q#ni:/lib/class_init.b#;$hs=q#ni:/lib/dataslice#;$is={$D7,1};$js=q#/lib/dataslice#;$ks={};$ls=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$ms=bless({$v,$ls},$z);$ns={$h7,$ms};$os=q#/lib/dataslice_init.b#;$ps=bless({$n5,$ks,$k6,$q,$l6,$q,$m6,$ns,$J,$os},$v6);$qs={};$rs=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$ss=bless({$v,$rs},$z);$ts={$q6,$ss};$us=q#/lib/dataslice_apply.b#;$vs=bless({$n5,$qs,$k6,$q,$l6,$q,$m6,$ts,$J,$us},$v6);$ws=[$l8,$ps,$vs];$xs=bless({$n5,$is,$J,$js,$d6,$ws},$H5);$ys=q#ni:/lib/dataslice.c#;$zs={$H5,1};$As=q#/lib/dataslice.c#;$Bs=[$I9];$Cs=bless({$n5,$zs,$J,$As,$d6,$Bs},$e6);$Ds=q#ni:/lib/dataslice_apply.b#;$Es=q#ni:/lib/dataslice_init.b#;$Fs=q#ni:/lib/definition.b#;$Gs=q#ni:/lib/definition_def.b#;$Hs=q#ni:/lib/definition_defdata.b#;$Is=q#ni:/lib/doc#;$Js={$L,1};$Ks={};$Ls=q#shift; +{name => shift, doc => []}#;$Ms=bless({$v,$Ls},$z);$Ns={$h7,$Ms};$Os=q#/lib/doc_init.b#;$Ps=bless({$n5,$Ks,$k6,$q,$l6,$q,$m6,$Ns,$J,$Os},$v6);$Qs={};$Rs=q#'ni.doc'#;$Ss=bless({$v,$Rs},$z);$Ts={$G6,$Ss};$Us=q#/lib/doc_namespace.b#;$Vs=bless({$n5,$Qs,$k6,$q,$l6,$q,$m6,$Ts,$J,$Us},$v6);$Ws={};$Xs=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map $self->fix_indentation($_), @_];
$self;#;$Ys=bless({$v,$Xs},$z);$Zs=q#fix_indentation#;$ct=q#my ($self, $x) = @_;
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
join "\\n", @lines;#;$dt=bless({$v,$ct},$z);$et={$za,$Ys,$Zs,$dt};$ft=q#/lib/doc_define.b#;$gt=bless({$n5,$Ws,$k6,$q,$l6,$q,$m6,$et,$J,$ft},$v6);$ht={};$it=q#shift->referent#;$jt=bless({$v,$it},$z);$kt=q#referent#;$lt=q#ni 'ni:' . shift->{name}#;$mt=bless({$v,$lt},$z);$nt={$fn,$jt,$kt,$mt};$ot=q#/lib/doc_end.b#;$pt=bless({$n5,$ht,$k6,$q,$l6,$q,$m6,$nt,$J,$ot},$v6);$qt={};$rt=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$st=bless({$v,$rt},$z);$tt=q#linearized#;$ut=q#map @$_, @{shift->{doc}}#;$vt=bless({$v,$ut},$z);$wt=q#tests#;$xt=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$yt=bless({$v,$xt},$z);$zt={$Q2,$st,$tt,$vt,$wt,$yt};$At=q#/lib/doc_test.b#;$Bt=bless({$n5,$qt,$k6,$q,$l6,$q,$m6,$zt,$J,$At},$v6);$Ct=[$Z7,$D6,$Ps,$Vs,$gt,$pt,$Bt];$Dt=bless({$n5,$Js,$J,$N3,$d6,$Ct},$I5);$Et=q#ni:/lib/doc.c#;$Ft={$I5,1};$Gt=q#/lib/doc.c#;$Ht=[$E9];$It=bless({$n5,$Ft,$J,$Gt,$d6,$Ht},$e6);$Jt=q#ni:/lib/doc_define.b#;$Kt=q#ni:/lib/doc_end.b#;$Lt=q#ni:/lib/doc_init.b#;$Mt=q#ni:/lib/doc_namespace.b#;$Nt=q#ni:/lib/doc_test.b#;$Ot=q#ni:/lib/documentable.b#;$Pt=q#ni:/lib/fn#;$Qt={$z,1};$Rt=q#/lib/fn#;$St={};$Tt=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Ut=bless({$v,$Tt,$x,$y},$z);$Vt=q#compile#;$Wt=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$Xt=bless({$v,$Wt},$z);$Yt=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$Zt=bless({$v,$Yt,$x,$y},$z);$cu={$Vt,$Xt,$h7,$Zt};$du=q#/lib/fn_init.b#;$eu=bless({$n5,$St,$k6,$q,$l6,$Ut,$m6,$cu,$J,$du},$v6);$fu={};$gu=[];$hu=q#shift->{'annotations'}#;$iu=bless({$t,$gu,$v,$hu,$x,$y},$z);$ju=[];$ku=q#shift->{'code'}#;$lu=bless({$t,$ju,$v,$ku,$x,$y},$z);$mu=q#eval_number#;$nu=[];$ou=q#shift->{'eval_number'}#;$pu=bless({$t,$nu,$v,$ou,$x,$y},$z);$qu=q#fn#;$ru=[];$su=q#shift->{'fn'}#;$tu=bless({$t,$ru,$v,$su,$x,$y},$z);$uu={$t,$iu,$v,$lu,$mu,$pu,$qu,$tu};$vu=q#/lib/fn_ro.b#;$wu=bless({$n5,$fu,$k6,$q,$l6,$q,$m6,$uu,$J,$vu},$v6);$xu={};$yu=[];$zu=q#my $self = shift; "fn {$$self{code}}"#;$Au=bless({$t,$yu,$v,$zu,$x,$y},$z);$Bu=[];$Cu=bless({$t,$Bu,$v,$O8,$x,$y},$z);$Du={$G8,$Au,$N8,$Cu};$Eu=q#/lib/fn_ops.b#;$Fu=bless({$n5,$xu,$k6,$q,$l6,$q,$m6,$Du,$J,$Eu},$v6);$Gu={};$Hu=q#serialize#;$Iu=[];$Ju=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Ku=bless({$t,$Iu,$v,$Ju,$x,$y},$z);$Lu={$Hu,$Ku};$Mu=q#/lib/fn_serialize.b#;$Nu=bless({$n5,$Gu,$k6,$q,$l6,$q,$m6,$Lu,$J,$Mu},$v6);$Ou=[$Z7,$m9,$eu,$wu,$Fu,$Nu];$Pu=bless({$n5,$Qt,$J,$Rt,$d6,$Ou},$J5);$Qu=[];$Ru=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Su=bless({$t,$Qu,$v,$Ru,$x,$y},$z);$Tu=q#ni:/lib/fn.c#;$Uu={$J5,1};$Vu=q#/lib/fn.c#;$Wu={};$Xu=q#resolve_evals#;$Yu=[];$Zu=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$cv=bless({$t,$Yu,$v,$Zu,$x,$y},$z);$dv={$Xu,$cv};$ev=q#/lib/fn.c_resolve_eval.b#;$fv=bless({$n5,$Wu,$k6,$Su,$l6,$q,$m6,$dv,$J,$ev},$v6);$gv=[$E9,$fv];$hv=bless({$n5,$Uu,$J,$Vu,$d6,$gv},$e6);$iv=q#ni:/lib/fn.c_resolve_eval.b#;$jv=q#ni:/lib/fn_init.b#;$kv=q#ni:/lib/fn_ops.b#;$lv=q#ni:/lib/fn_ro.b#;$mv=q#ni:/lib/fn_serialize.b#;$nv=q#ni:/lib/future#;$ov={$E7,1};$pv={};$qv=[];$rv=bless({$t,$qv,$v,$eo,$x,$y},$z);$sv=q#parents#;$tv=[];$uv=q#shift->{'parents'}#;$vv=bless({$t,$tv,$v,$uv,$x,$y},$z);$wv=q#v#;$xv=[];$yv=q#shift->{'v'}#;$zv=bless({$t,$xv,$v,$yv,$x,$y},$z);$Av={$r,$rv,$sv,$vv,$wv,$zv};$Bv=q#/lib/future_ro.b#;$Cv=bless({$n5,$pv,$k6,$q,$l6,$q,$m6,$Av,$J,$Bv},$v6);$Dv={};$Ev=[];$Fv=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Gv=bless({$t,$Ev,$v,$Fv,$x,$y},$z);$Hv={$h7,$Gv};$Iv=q#/lib/future_init.b#;$Jv=bless({$n5,$Dv,$k6,$q,$l6,$q,$m6,$Hv,$J,$Iv},$v6);$Kv={};$Lv=q#decide#;$Mv=[];$Nv=q#local $_;
my ($self, $v) = @_;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Ov=bless({$t,$Mv,$v,$Nv,$x,$y},$z);$Pv=q#decided#;$Qv=[];$Rv=q#shift->{outcome}#;$Sv=bless({$t,$Qv,$v,$Rv,$x,$y},$z);$Tv=q#listener#;$Uv=[];$Vv=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$Wv=bless({$t,$Uv,$v,$Vv,$x,$y},$z);$Xv={$Lv,$Ov,$Pv,$Sv,$Tv,$Wv};$Yv=q#/lib/future_state.b#;$Zv=bless({$n5,$Kv,$k6,$q,$l6,$q,$m6,$Xv,$J,$Yv},$v6);$cw={};$dw=q#and#;$ew=[];$fw=q#my $self   = $_[0];
my $child  = $self->class->new(grep ref, @_);
my $n      = @{$child->parents};
my $l      = 0;
my @result = map ref($_) ? undef : $_, @_;
for my $i (0..$\#_) {
  $_[$i]->listener(sub {
    $result[$i] = shift;
    $child->decide(\\@result) if ++$l == $n;
  }) if ref $_[$i];
}
$child;#;$gw=bless({$t,$ew,$v,$fw,$x,$y},$z);$hw=q#flatmap#;$iw=[];$jw=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$kw=bless({$t,$iw,$v,$jw,$x,$y},$z);$lw=q#map#;$mw=[];$nw=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$ow=bless({$t,$mw,$v,$nw,$x,$y},$z);$pw=q#or#;$qw=[];$rw=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$sw=bless({$t,$qw,$v,$rw,$x,$y},$z);$tw={$dw,$gw,$hw,$kw,$lw,$ow,$pw,$sw};$uw=q#/lib/future_algebra.b#;$vw=bless({$n5,$cw,$k6,$q,$l6,$q,$m6,$tw,$J,$uw},$v6);$ww=[$Z7,$Cv,$Jv,$Zv,$vw];$xw=bless({$n5,$ov,$J,$g4,$d6,$ww},$K5);$yw=q#ni:/lib/future.c#;$zw={$K5,1};$Aw=q#/lib/future.c#;$Bw=[$E9];$Cw=bless({$n5,$zw,$J,$Aw,$d6,$Bw},$e6);$Dw=q#ni:/lib/future_algebra.b#;$Ew=q#ni:/lib/future_init.b#;$Fw=q#ni:/lib/future_ro.b#;$Gw=q#ni:/lib/future_state.b#;$Hw=q#ni:/lib/gensym_generator_compact.b#;$Iw={};$Jw=q#gensym#;$Kw=[];$Lw=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Mw=bless({$t,$Kw,$v,$Lw,$x,$y},$z);$Nw={$Jw,$Mw};$Ow=q#/lib/gensym_generator_compact.b#;$Pw=bless({$n5,$Iw,$k6,$q,$l6,$q,$m6,$Nw,$J,$Ow},$v6);$Qw=q#ni:/lib/global_static_test.b#;$Rw={};$Sw=[];$Tw=q#ni('ni:/lib/test_case')->new(shift)#;$Uw=q#($)#;$Vw=bless({$t,$Sw,$v,$Tw,$x,$Uw},$z);$Ww=q#now#;$Xw=[];$Yw=q#ni('ni:/lib/test_value')->new(shift)#;$Zw=bless({$t,$Xw,$v,$Yw,$x,$Uw},$z);$cx={$Q2,$Vw,$Ww,$Zw};$dx=q#/lib/global_static_test.b#;$ex=bless({$n5,$Rw,$k6,$q,$l6,$q,$m6,$cx,$J,$dx},$v6);$fx=q#ni:/lib/image#;$gx={$F7,1};$hx={};$ix=[];$jx=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$kx=bless({$t,$ix,$v,$jx,$x,$y},$z);$lx={$h7,$kx};$mx=q#/lib/image_init.b#;$nx=bless({$n5,$hx,$k6,$q,$l6,$q,$m6,$lx,$J,$mx},$v6);$ox={};$px=q#boot_side_effect#;$qx=[];$rx=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$sx=bless({$t,$qx,$v,$rx,$x,$y},$z);$tx=q#finalizer#;$ux=[];$vx=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$wx=bless({$t,$ux,$v,$vx,$x,$y},$z);$xx=q#io#;$yx=[];$zx=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Ax=bless({$t,$yx,$v,$zx,$x,$y},$z);$Bx=q#reconstruction#;$Cx=[];$Dx=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Ex=bless({$t,$Cx,$v,$Dx,$x,$y},$z);$Fx=q#side_effect#;$Gx=[];$Hx=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ix=bless({$t,$Gx,$v,$Hx,$x,$y},$z);$Jx={$px,$sx,$tx,$wx,$xx,$Ax,$Bx,$Ex,$Fx,$Ix};$Kx=q#/lib/image_quoting.b#;$Lx=bless({$n5,$ox,$k6,$q,$l6,$q,$m6,$Jx,$J,$Kx},$v6);$Mx={};$Nx=q#quote_code#;$Ox=[];$Px=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Qx=bless({$t,$Ox,$v,$Px,$x,$y},$z);$Rx={$Nx,$Qx};$Sx=q#/lib/quote_code_fail.b#;$Tx=bless({$n5,$Mx,$k6,$q,$l6,$q,$m6,$Rx,$J,$Sx},$v6);$Ux={};$Vx=q#quote_array#;$Wx=[];$Xx=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Yx=bless({$t,$Wx,$v,$Xx,$x,$y},$z);$Zx=q#quote_hash#;$cy=[];$dy=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$ey=bless({$t,$cy,$v,$dy,$x,$y},$z);$fy=q#quote_scalar#;$gy=[];$hy=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$iy=bless({$t,$gy,$v,$hy,$x,$y},$z);$jy=q#quote_scalar_ref#;$ky=[];$ly=q#'\\\\' . shift->quote(${$_[0]})#;$my=bless({$t,$ky,$v,$ly,$x,$y},$z);$ny=q#quote_value#;$oy=[];$py=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$qy=bless({$t,$oy,$v,$py,$x,$y},$z);$ry={$Vx,$Yx,$Zx,$ey,$fy,$iy,$jy,$my,$ny,$qy};$sy=q#/lib/quote_values.b#;$ty=bless({$n5,$Ux,$k6,$q,$l6,$q,$m6,$ry,$J,$sy},$v6);$uy={};$vy=q#quote_blessed#;$wy=[];$xy=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$yy=bless({$t,$wy,$v,$xy,$x,$y},$z);$zy=q#quote_class#;$Ay=[];$By=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Cy=bless({$t,$Ay,$v,$By,$x,$y},$z);$Dy=q#quote_object#;$Ey=[];$Fy=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$Gy=bless({$t,$Ey,$v,$Fy,$x,$y},$z);$Hy={$vy,$yy,$zy,$Cy,$Dy,$Gy};$Iy=q#/lib/quote_objects.b#;$Jy=bless({$n5,$uy,$k6,$q,$l6,$q,$m6,$Hy,$J,$Iy},$v6);$Ky={};$Ly=q#circular_arrayref#;$My=[];$Ny=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Oy=bless({$t,$My,$v,$Ny,$x,$y},$z);$Py=q#circular_hashref#;$Qy=[];$Ry=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Sy=bless({$t,$Qy,$v,$Ry,$x,$y},$z);$Ty=q#is_circular#;$Uy=[];$Vy=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Wy=bless({$t,$Uy,$v,$Vy,$x,$y},$z);$Xy={$Ly,$Oy,$Py,$Sy,$Ty,$Wy};$Yy=q#/lib/quote_circular_addressed.b#;$Zy=bless({$n5,$Ky,$k6,$q,$l6,$q,$m6,$Xy,$J,$Yy},$v6);$cz={};$dz=q#address#;$ez=[];$fz=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$gz=bless({$t,$ez,$v,$fz,$x,$y},$z);$hz=q#allocate_gensym#;$iz=[];$jz=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$kz=bless({$t,$iz,$v,$jz,$x,$y},$z);$lz=q#circular_links#;$mz=[];$nz=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$oz=bless({$t,$mz,$v,$nz,$x,$y},$z);$pz=q#quote#;$qz=[];$rz=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$sz=bless({$t,$qz,$v,$rz,$x,$y},$z);$tz={$dz,$gz,$hz,$kz,$lz,$oz,$pz,$sz};$uz=q#/lib/quote_gensym_identity.b#;$vz=bless({$n5,$cz,$k6,$q,$l6,$q,$m6,$tz,$J,$uz},$v6);$wz=[$Z7,$nx,$Lx,$Tx,$ty,$Jy,$Zy,$vz,$Pw];$xz=bless({$n5,$gx,$J,$o4,$d6,$wz},$L5);$yz=q#ni:/lib/image.c#;$zz={$L5,1};$Az=q#/lib/image.c#;$Bz=[$E9];$Cz=bless({$n5,$zz,$J,$Az,$d6,$Bz},$e6);$Dz=q#ni:/lib/image_init.b#;$Ez=q#ni:/lib/image_quoting.b#;$Fz=q#ni:/lib/instance.b#;$Gz=q#ni:/lib/instantiable.b#;$Hz=q#ni:/lib/json.b#;$Iz={};$Jz=q#json_decode#;$Kz=[];$Lz=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Mz=bless({$t,$Kz,$v,$Lz,$x,$Uw},$z);$Nz=q#json_encode#;$Oz=[];$Pz=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Qz=bless({$t,$Oz,$v,$Pz,$x,$Uw},$z);$Rz=q#json_encode_pretty#;$Sz=[];$Tz=q#local $_;
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

$spaces . ni::json_encode($v);#;$Uz=bless({$t,$Sz,$v,$Tz,$x,$y},$z);$Vz=q#json_escape#;$Wz=[];$Xz=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$Yz=bless({$t,$Wz,$v,$Xz,$x,$Uw},$z);$Zz=q#json_unescape#;$cA=[];$dA=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$eA=bless({$t,$cA,$v,$dA,$x,$Uw},$z);$fA=q#json_unescape_one#;$gA=[];$hA=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$iA=bless({$t,$gA,$v,$hA,$x,$Uw},$z);$jA={$Jz,$Mz,$Nz,$Qz,$Rz,$Uz,$Vz,$Yz,$Zz,$eA,$fA,$iA};$kA=q#/lib/json.b#;$lA=bless({$n5,$Iz,$k6,$q,$l6,$q,$m6,$jA,$J,$kA},$v6);$mA=q#ni:/lib/name_as_string.b#;$nA=q#ni:/lib/named.b#;$oA=q#ni:/lib/named_in_ni.b#;$pA=q#ni:/lib/namespaced.b#;$qA=q#ni:/lib/ni#;$rA={$G7,1};$sA={};$tA=q#extend#;$uA=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$vA=bless({$v,$uA,$x,$y},$z);$wA=q#is_mutable#;$xA=q#$0 ne '-' && -w $0#;$yA=bless({$v,$xA,$x,$y},$z);$zA=q#modify#;$AA=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$BA=bless({$v,$AA,$x,$y},$z);$CA={$tA,$vA,$wA,$yA,$zA,$BA};$DA=q#/lib/ni_self.b#;$EA=bless({$n5,$sA,$k6,$q,$l6,$q,$m6,$CA,$J,$DA},$v6);$FA={};$GA=q#--internal/+=#;$HA=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$IA=bless({$v,$HA,$x,$y},$z);$JA=q#--internal/eval#;$KA=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$LA=bless({$v,$KA,$x,$y},$z);$MA=q#--internal/image#;$NA=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$OA=bless({$v,$NA,$x,$y},$z);$PA=q#--internal/test#;$QA=q#local $| = 1;
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
!!$failed;#;$RA=bless({$v,$QA,$x,$y},$z);$SA=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$TA=bless({$v,$SA,$x,$y},$z);$UA={$GA,$IA,$JA,$LA,$MA,$OA,$PA,$RA,$mq,$TA};$VA=q#/lib/ni_main.b#;$WA=bless({$n5,$FA,$k6,$q,$l6,$q,$m6,$UA,$J,$VA},$v6);$XA={};$YA=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$ZA=bless({$v,$YA,$x,$y},$z);$cB=q#resolver_for#;$dB=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$eB=bless({$v,$dB,$x,$y},$z);$fB={$U6,$ZA,$cB,$eB};$gB=q#/lib/ni_resolver.b#;$hB=bless({$n5,$XA,$k6,$q,$l6,$q,$m6,$fB,$J,$gB},$v6);$iB={};$jB=q#exists#;$kB=q#exists $_[0]->{named}{$_[1]}#;$lB=bless({$v,$kB,$x,$y},$z);$mB=q#quoted#;$nB=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$oB=bless({$v,$nB,$x,$y},$z);$pB={$jB,$lB,$mB,$oB};$qB=q#/lib/ni_image.b#;$rB=bless({$n5,$iB,$k6,$q,$l6,$q,$m6,$pB,$J,$qB},$v6);$sB=[$Z7,$EA,$WA,$hB,$rB];$tB=bless({$n5,$rA,$J,$w4,$d6,$sB},$M5);$uB=q#ni:/lib/ni.c#;$vB={$M5,1};$wB=q#/lib/ni.c#;$xB=[$E9];$yB=bless({$n5,$vB,$J,$wB,$d6,$xB},$e6);$zB=q#ni:/lib/ni_image.b#;$AB=q#ni:/lib/ni_main.b#;$BB=q#ni:/lib/ni_resolver.b#;$CB=q#ni:/lib/ni_self.b#;$DB=q#ni:/lib/ni_static_util.b#;$EB={};$FB=q#abbrev#;$GB=[];$HB=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$IB=bless({$t,$GB,$v,$HB,$x,$y},$z);$JB=q#dor#;$KB=[];$LB=q#defined $_[0] ? $_[0] : $_[1]#;$MB=bless({$t,$KB,$v,$LB,$x,$y},$z);$NB=q#indent#;$OB=[];$PB=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$QB=bless({$t,$OB,$v,$PB,$x,$y},$z);$RB=q#max#;$SB=[];$TB=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$UB=bless({$t,$SB,$v,$TB,$x,$y},$z);$VB=q#maxstr#;$WB=[];$XB=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$YB=bless({$t,$WB,$v,$XB,$x,$y},$z);$ZB=q#mean#;$cC=[];$dC=q#sum(@_) / (@_ || 1)#;$eC=bless({$t,$cC,$v,$dC,$x,$y},$z);$fC=q#min#;$gC=[];$hC=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$iC=bless({$t,$gC,$v,$hC,$x,$y},$z);$jC=q#minstr#;$kC=[];$lC=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$mC=bless({$t,$kC,$v,$lC,$x,$y},$z);$nC=q#sgr#;$oC=[];$pC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$qC=bless({$t,$oC,$v,$pC,$x,$y},$z);$rC=q#sr#;$sC=[];$tC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$uC=bless({$t,$sC,$v,$tC,$x,$y},$z);$vC=q#sum#;$wC=[];$xC=q#local $_; my $x = 0; $x += $_ for @_; $x#;$yC=bless({$t,$wC,$v,$xC,$x,$y},$z);$zC=q#swap#;$AC=[];$BC=q#@_[0, 1] = @_[1, 0]#;$CC=bless({$t,$AC,$v,$BC,$x,$y},$z);$DC={$FB,$IB,$JB,$MB,$NB,$QB,$RB,$UB,$VB,$YB,$ZB,$eC,$fC,$iC,$jC,$mC,$nC,$qC,$rC,$uC,$vC,$yC,$zC,$CC};$EC=q#/lib/ni_static_util.b#;$FC=bless({$n5,$EB,$k6,$q,$l6,$q,$m6,$DC,$J,$EC},$v6);$GC=q#ni:/lib/perlbranch.b#;$HC=q#ni:/lib/quote_circular_addressed.b#;$IC=q#ni:/lib/quote_code_fail.b#;$JC=q#ni:/lib/quote_gensym_identity.b#;$KC=q#ni:/lib/quote_objects.b#;$LC=q#ni:/lib/quote_simple#;$MC={$H7,1};$NC={};$OC=[];$PC=q#+{}#;$QC=bless({$t,$OC,$v,$PC,$x,$y},$z);$RC={$h7,$QC};$SC=q#/lib/quote_simple_init.b#;$TC=bless({$n5,$NC,$k6,$q,$l6,$q,$m6,$RC,$J,$SC},$v6);$UC={};$VC=[];$WC=bless({$t,$VC,$v,0,$x,$y},$z);$XC=[];$YC=q#shift->quote_value(shift)#;$ZC=bless({$t,$XC,$v,$YC,$x,$y},$z);$cD={$Ty,$WC,$pz,$ZC};$dD=q#/lib/quote_simple_quote.b#;$eD=bless({$n5,$UC,$k6,$q,$l6,$q,$m6,$cD,$J,$dD},$v6);$fD=[$Z7,$TC,$eD,$Tx,$ty,$Jy];$gD=bless({$n5,$MC,$J,$H4,$d6,$fD},$N5);$hD=q#ni:/lib/quote_simple.c#;$iD={$N5,1};$jD=q#/lib/quote_simple.c#;$kD=[$E9];$lD=bless({$n5,$iD,$J,$jD,$d6,$kD},$e6);$mD=q#ni:/lib/quote_simple_init.b#;$nD=q#ni:/lib/quote_simple_quote.b#;$oD=q#ni:/lib/quote_values.b#;$pD=q#ni:/lib/ref_eq.b#;$qD=q#ni:/lib/resolver.b#;$rD=q#ni:/lib/slice#;$sD={$v6,1};$tD=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$uD=bless({$v,$tD},$z);$vD=q#local $_;
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
$self;#;$wD=bless({$v,$vD},$z);$xD=q#lib/slice::apply#;$yD=q#lib/slice::apply_#;$zD={};$AD=q#apply_#;$BD={$q6,$uD,$AD,$wD};$CD=q#/lib/slice.b#;$DD=bless({$n5,$zD,$m6,$BD,$J,$CD},$v6);$ED={};$FD=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$GD=bless({$v,$FD,$x,$y},$z);$HD={$h7,$GD};$ID=q#/lib/slice_init.b#;$JD=bless({$n5,$ED,$m6,$HD,$J,$ID},$v6);$KD={};$LD=[];$MD=q#local $_;
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
$g;#;$ND=bless({$t,$LD,$v,$MD,$x,$y},$z);$OD={$Hu,$ND};$PD=q#/lib/slice_serialize.b#;$QD=bless({$n5,$KD,$k6,$q,$l6,$q,$m6,$OD,$J,$PD},$v6);$RD=[$l8,$D6,$DD,$JD,$QD];$SD=bless({$n5,$sD,$J,$e5,$d6,$RD},$O5);$TD=q#ni:/lib/slice.b#;$UD=q#ni:/lib/slice.c#;$VD={$O5,1};$WD=q#/lib/slice.c#;$XD=[$I9];$YD=bless({$n5,$VD,$J,$WD,$d6,$XD},$e6);$ZD=q#ni:/lib/slice_init.b#;$cE=q#ni:/lib/slice_serialize.b#;$dE=q#ni:/lib/static_fn.b#;$eE={};$fE=[];$gE=q#ni('ni:/lib/fn')->new(@_)#;$hE=bless({$t,$fE,$v,$gE,$x,$Uw},$z);$iE=q#fp#;$jE=[];$kE=q#($$)#;$lE=bless({$t,$jE,$v,$gE,$x,$kE},$z);$mE={$qu,$hE,$iE,$lE};$nE=q#/lib/static_fn.b#;$oE=bless({$n5,$eE,$k6,$q,$l6,$q,$m6,$mE,$J,$nE},$v6);$pE=q#ni:/lib/subclass.b#;$qE=q#ni:/lib/tag#;$rE={$E6,1};$sE=q#/lib/tag#;$tE={};$uE=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$vE=bless({$v,$uE,$x,$y},$z);$wE={$q6,$vE};$xE=q#/lib/tag.b#;$yE=bless({$n5,$tE,$k6,$q,$l6,$q,$m6,$wE,$J,$xE},$v6);$zE={};$AE=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$BE=bless({$v,$AE,$x,$y},$z);$CE={$h7,$BE};$DE=q#/lib/tag_init.b#;$EE=bless({$n5,$zE,$k6,$q,$l6,$q,$m6,$CE,$J,$DE},$v6);$FE=[$l8,$D6,$yE,$EE];$GE=bless({$n5,$rE,$J,$sE,$d6,$FE},$P5);$HE=q#ni:/lib/tag.b#;$IE=q#ni:/lib/tag.c#;$JE={$P5,1};$KE=q#/lib/tag.c#;$LE=[$I9];$ME=bless({$n5,$JE,$J,$KE,$d6,$LE},$e6);$NE=q#ni:/lib/tag_init.b#;$OE=q#ni:/lib/test_assert_eq#;$PE={$I7,1};$QE=q#/lib/test_assert_eq#;$RE={$I7,1,$J7,1};$SE=q#/lib/test_assertion#;$TE={};$UE=q#commit#;$VE=[];$WE=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$XE=bless({$t,$VE,$v,$WE,$x,$y},$z);$YE={$UE,$XE};$ZE=q#/lib/test_assertion_commit.b#;$cF=bless({$n5,$TE,$k6,$q,$l6,$q,$m6,$YE,$J,$ZE},$v6);$dF=[$Z7,$cF];$eF=bless({$n5,$RE,$J,$SE,$d6,$dF},$R5);$fF={};$gF=[];$hF=q#my ($class, $diff) = @_;
+{diff => $diff};#;$iF=bless({$t,$gF,$v,$hF,$x,$y},$z);$jF={$h7,$iF};$kF=q#/lib/test_assert_eq_init.b#;$lF=bless({$n5,$fF,$k6,$q,$l6,$q,$m6,$jF,$J,$kF},$v6);$mF={};$nF=[];$oF=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode_pretty $$self{diff}
              : "PASS";#;$pF=bless({$t,$nF,$v,$oF,$x,$y},$z);$qF=q#failed#;$rF=[];$sF=q#defined shift->{diff}#;$tF=bless({$t,$rF,$v,$sF,$x,$y},$z);$uF=q#result#;$vF=[];$wF=bless({$t,$vF,$v,$oq,$x,$y},$z);$xF={$G8,$pF,$qF,$tF,$uF,$wF};$yF=q#/lib/test_assert_eq_result.b#;$zF=bless({$n5,$mF,$k6,$q,$l6,$q,$m6,$xF,$J,$yF},$v6);$AF=[$eF,$lF,$zF];$BF=bless({$n5,$PE,$J,$QE,$d6,$AF},$Q5);$CF=q#ni:/lib/test_assert_eq.c#;$DF={$Q5,1};$EF=q#/lib/test_assert_eq.c#;$FF={$Q5,1,$R5,1};$GF=q#/lib/test_assertion.c#;$HF=[$E9];$IF=bless({$n5,$FF,$J,$GF,$d6,$HF},$e6);$JF=[$IF];$KF=bless({$n5,$DF,$J,$EF,$d6,$JF},$e6);$LF=q#ni:/lib/test_assert_eq_init.b#;$MF=q#ni:/lib/test_assert_eq_result.b#;$NF=q#ni:/lib/test_assertion#;$OF=q#ni:/lib/test_assertion.c#;$PF=q#ni:/lib/test_assertion_commit.b#;$QF=q#ni:/lib/test_case#;$RF={$B,1};$SF=q#/lib/test_case#;$TF=q#running_test#;$UF={};$VF=[];$WF=q#shift->{'assertions'}#;$XF=bless({$t,$VF,$v,$WF,$x,$y},$z);$YF=[];$ZF=q#shift->{'test'}#;$cG=bless({$t,$YF,$v,$ZF,$x,$y},$z);$dG={$n,$XF,$s,$cG};$eG=q#/lib/test_case_ro.b#;$fG=bless({$n5,$UF,$k6,$q,$l6,$q,$m6,$dG,$J,$eG},$v6);$gG={};$hG=[];$iG=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$jG=bless({$t,$hG,$v,$iG,$x,$y},$z);$kG={$p,$jG};$lG=q#/lib/test_case_rw.b#;$mG=bless({$n5,$gG,$k6,$q,$l6,$q,$m6,$kG,$J,$lG},$v6);$nG={};$oG=[];$pG=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$qG=bless({$t,$oG,$v,$pG,$x,$y},$z);$rG={$h7,$qG};$sG=q#/lib/test_case_init.b#;$tG=bless({$n5,$nG,$k6,$q,$l6,$q,$m6,$rG,$J,$sG},$v6);$uG={};$vG=[];$wG=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$xG=bless({$t,$vG,$v,$wG,$x,$y},$z);$yG=[];$zG=q#!shift->{outcome}->[0]#;$AG=bless({$t,$yG,$v,$zG,$x,$y},$z);$BG={$G8,$xG,$qF,$AG};$CG=q#/lib/test_case_metrics.b#;$DG=bless({$n5,$uG,$k6,$q,$l6,$q,$m6,$BG,$J,$CG},$v6);$EG={};$FG=q#done#;$GG=[];$HG=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$IG=bless({$t,$GG,$v,$HG,$x,$y},$z);$JG=[];$KG=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$LG=bless({$t,$JG,$v,$KG,$x,$y},$z);$MG={$FG,$IG,$mq,$LG};$NG=q#/lib/test_case_run.b#;$OG=bless({$n5,$EG,$k6,$q,$l6,$q,$m6,$MG,$J,$NG},$v6);$PG=[$Z7,$fG,$mG,$tG,$DG,$OG];$QG=bless({$n5,$RF,$J,$SF,$TF,$q,$d6,$PG},$S5);$RG=[];$SG=q#shift->{running_test} = undef#;$TG=bless({$t,$RG,$v,$SG,$x,$y},$z);$UG=q#ni:/lib/test_case.c#;$VG={$S5,1};$WG=q#/lib/test_case.c#;$XG={};$YG=[];$ZG=q#shift->{'running_test'}#;$cH=bless({$t,$YG,$v,$ZG,$x,$y},$z);$dH={$TF,$cH};$eH=q#/lib/test_case.c_test_ro.b#;$fH=bless({$n5,$XG,$k6,$q,$l6,$q,$m6,$dH,$J,$eH},$v6);$gH={};$hH=q#with_test#;$iH=[];$jH=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$kH=bless({$t,$iH,$v,$jH,$x,$y},$z);$lH={$hH,$kH};$mH=q#/lib/test_case.c_test.b#;$nH=bless({$n5,$gH,$k6,$TG,$l6,$q,$m6,$lH,$J,$mH},$v6);$oH=[$E9,$fH,$nH];$pH=bless({$n5,$VG,$J,$WG,$d6,$oH},$e6);$qH=q#ni:/lib/test_case.c_test.b#;$rH=q#ni:/lib/test_case.c_test_ro.b#;$sH=q#ni:/lib/test_case_init.b#;$tH=q#ni:/lib/test_case_metrics.b#;$uH=q#ni:/lib/test_case_ro.b#;$vH=q#ni:/lib/test_case_run.b#;$wH=q#ni:/lib/test_case_rw.b#;$xH=q#ni:/lib/test_value#;$yH={$K7,1};$zH=q#/lib/test_value#;$AH={};$BH=[];$CH=q#\\$_[1]#;$DH=bless({$t,$BH,$v,$CH,$x,$y},$z);$EH={$h7,$DH};$FH=q#/lib/test_value_init.b#;$GH=bless({$n5,$AH,$k6,$q,$l6,$q,$m6,$EH,$J,$FH},$v6);$HH={};$IH=q#(==#;$JH=[];$KH=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$LH=bless({$t,$JH,$v,$KH,$x,$y},$z);$MH=q#detailed_scalar_diff#;$NH=[];$OH=q#local $_;
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
[@diff];#;$PH=bless({$t,$NH,$v,$OH,$x,$y},$z);$QH=q#diff#;$RH=[];$SH=q#my ($self, $rhs) = @_;
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
return undef;#;$TH=bless({$t,$RH,$v,$SH,$x,$y},$z);$UH={$IH,$LH,$MH,$PH,$QH,$TH};$VH=q#/lib/test_value_eq.b#;$WH=bless({$n5,$HH,$k6,$q,$l6,$q,$m6,$UH,$J,$VH},$v6);$XH={};$YH=[];$ZH=q#ni::json_encode ${$_[0]}#;$cI=bless({$t,$YH,$v,$ZH,$x,$y},$z);$dI={$G8,$cI};$eI=q#/lib/test_value_str.b#;$fI=bless({$n5,$XH,$k6,$q,$l6,$q,$m6,$dI,$J,$eI},$v6);$gI=[$Z7,$GH,$WH,$fI];$hI=bless({$n5,$yH,$J,$zH,$d6,$gI},$T5);$iI=q#ni:/lib/test_value.c#;$jI={$T5,1};$kI=q#/lib/test_value.c#;$lI=[$E9];$mI=bless({$n5,$jI,$J,$kI,$d6,$lI},$e6);$nI=q#ni:/lib/test_value_eq.b#;$oI=q#ni:/lib/test_value_init.b#;$pI=q#ni:/lib/test_value_str.b#;$qI=q#ni:/metaclass#;$rI={$e6,1};$sI=q#/metaclass#;$tI=[$d7,$m9,$m7,$f9];$uI=bless({$n5,$rI,$J,$sI,$d6,$tI},$U5);$vI=q#ni:/metaclass.c#;$wI={$U5,1};$xI=q#/metaclass.c#;$yI=[$v9];$zI=bless({$n5,$wI,$J,$xI,$d6,$yI},$e6);$AI=q#ni:/module#;$BI=q#ni:/module.c#;$CI=q#ni:/object#;$DI=q#ni:/object.c#;$EI=q#ni:/semantic/dimension#;$FI={$X5,1};$GI=q#/semantic/dimension#;$HI=[$v9];$II=bless({$n5,$FI,$J,$GI,$d6,$HI},$Y5);$JI=q#ni:/semantic/dimension.c#;$KI={$Y5,1};$LI=q#/semantic/dimension.c#;$MI=[$M9];$NI=bless({$n5,$KI,$J,$LI,$d6,$MI},$e6);$OI=q#ni:/semantic/task#;$PI=q#ni:/semantic/task.c#;$QI=q#ni:/semantic/task_outcome.b#;$RI=q#ni:/semantic/task_ro.b#;$SI=q#ni:main#;$TI={$Fk,1};$UI=[$oE,$ex,$Ek];$VI=bless({$n5,$TI,$J,$Fk,$d6,$UI},$f6);$WI=q#ni:ni#;$XI={$X9,1};$YI={$X9,1};$ZI=q#json_escapes#;$cJ=q##;$dJ=q#b#;$eJ=q#	#;$fJ=q#t#;$gJ=q#
#;$hJ=q#n#;$iJ=q##;$jJ=q#"#;$kJ=q#/#;$lJ=q#\\#;$mJ={$cJ,$dJ,$eJ,$fJ,$gJ,$hJ,$iJ,$Ui,$jJ,$jJ,$kJ,$kJ,$lJ,$lJ};$nJ=q#json_unescapes#;$oJ={$jJ,$jJ,$kJ,$kJ,$lJ,$lJ,$dJ,$cJ,$hJ,$gJ,$Ui,$iJ,$fJ,$eJ};$pJ={$ZI,$mJ,$nJ,$oJ};$qJ=q#/lib/json_data.b#;$rJ=bless({$n5,$YI,$Zm,$pJ,$J,$qJ},$D7);$sJ=[$rJ,$lA,$FC];$tJ=bless({$n5,$XI,$J,$X9,$d6,$sJ},$f6);$uJ={$d,$M,$P,$U,$V,$e1,$f1,$k1,$l1,$x1,$y1,$K1,$L1,$X1,$Y1,$m2,$n2,$I2,$J2,$O2,$P2,$n3,$o3,$u3,$v3,$O3,$P3,$h4,$i4,$p4,$q4,$x4,$y4,$I4,$J4,$f5,$g5,$l5,$m5,$v9,$w9,$M9,$N9,$ha,$ia,$ma,$na,$V9,$oa,$Ha,$Ia,$Ma,$Na,$xa,$Oa,$Fa,$Pa,$fa,$Qa,$Vc,$Wc,$od,$pd,$zc,$qd,$Tc,$rd,$Id,$Jd,$Nd,$Od,$zd,$Pd,$Gd,$Qd,$Cf,$Df,$Hf,$If,$kf,$Jf,$Af,$Kf,$ie,$Lf,$cf,$Mf,$Ce,$Nf,$Zd,$Of,$ih,$mh,$Kh,$Lh,$Ih,$Mh,$Eg,$Nh,$Pg,$Oh,$gg,$Ph,$fh,$Qh,$Xf,$Rh,$og,$Sh,$oj,$pj,$tj,$uj,$ki,$vj,$Hi,$wj,$ri,$xj,$mj,$yj,$ci,$zj,$Pi,$Aj,$Tj,$Uj,$Yj,$Zj,$Rj,$ck,$Ij,$dk,$Ek,$Gk,$gl,$hl,$ll,$ml,$Pk,$nl,$el,$ol,$sc,$pl,$md,$ql,$kd,$rl,$wb,$sl,$Eb,$tl,$Qb,$ul,$cb,$vl,$qc,$wl,$ec,$xl,$Km,$Lm,$Pm,$Qm,$Im,$Rm,$Ul,$Sm,$sm,$Tm,$Kl,$Um,$im,$Vm,$Mn,$Nn,$Rn,$Sn,$wn,$Tn,$Kn,$Un,$pn,$Vn,$hp,$lp,$xp,$yp,$vp,$zp,$Bq,$Fq,$Zq,$cr,$Xq,$dr,$Yp,$er,$kq,$fr,$Rp,$gr,$wq,$hr,$Jo,$ir,$fp,$jr,$Br,$Cr,$Gr,$Hr,$sr,$Ir,$zr,$Jr,$E8,$Kr,$l8,$Lr,$I9,$Mr,$Wr,$Xr,$w6,$Yr,$es,$fs,$Ur,$gs,$m7,$hs,$xs,$ys,$Cs,$Ds,$vs,$Es,$ps,$Fs,$d9,$Gs,$u8,$Hs,$Z8,$Is,$Dt,$Et,$It,$Jt,$gt,$Kt,$pt,$Lt,$Ps,$Mt,$Vs,$Nt,$Bt,$Ot,$j8,$Pt,$Pu,$Tu,$hv,$iv,$fv,$jv,$eu,$kv,$Fu,$lv,$wu,$mv,$Nu,$nv,$xw,$yw,$Cw,$Dw,$vw,$Ew,$Jv,$Fw,$Cv,$Gw,$Zv,$Hw,$Pw,$Qw,$ex,$fx,$xz,$yz,$Cz,$Dz,$nx,$Ez,$Lx,$Fz,$X7,$Gz,$m9,$Hz,$lA,$mA,$L8,$nA,$D6,$oA,$L6,$pA,$S6,$qA,$tB,$uB,$yB,$zB,$rB,$AB,$WA,$BB,$hB,$CB,$EA,$DB,$FC,$GC,$d7,$HC,$Zy,$IC,$Tx,$JC,$vz,$KC,$Jy,$LC,$gD,$hD,$lD,$mD,$TC,$nD,$eD,$oD,$ty,$pD,$S8,$qD,$Z6,$rD,$SD,$TD,$DD,$UD,$YD,$ZD,$JD,$cE,$QD,$dE,$oE,$pE,$t9,$qE,$GE,$HE,$yE,$IE,$ME,$NE,$EE,$OE,$BF,$CF,$KF,$LF,$lF,$MF,$zF,$NF,$eF,$OF,$IF,$PF,$cF,$QF,$QG,$UG,$pH,$qH,$nH,$rH,$fH,$sH,$tG,$tH,$DG,$uH,$fG,$vH,$OG,$wH,$mG,$xH,$hI,$iI,$mI,$nI,$WH,$oI,$GH,$pI,$fI,$qI,$uI,$vI,$zI,$AI,$f9,$BI,$K9,$CI,$Z7,$DI,$E9,$EI,$II,$JI,$NI,$OI,$wo,$PI,$rp,$QI,$uo,$RI,$io,$SI,$VI,$WI,$tJ};$vJ=q#resolvers#;$wJ=[];$xJ=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$yJ=bless({$t,$wJ,$v,$xJ,$x,$y},$z);$zJ=q#file#;$AJ=[];$BJ=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$CJ=bless({$t,$AJ,$v,$BJ,$x,$y},$z);$DJ=q#null#;$EJ=[];$FJ=q#ni('ni:/io/null')->new#;$GJ=bless({$t,$EJ,$v,$FJ,$x,$y},$z);$HJ=q#sh#;$IJ=[];$JJ=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$KJ=bless({$t,$IJ,$v,$JJ,$x,$y},$z);$LJ=q#str#;$MJ=[];$NJ=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$OJ=bless({$t,$MJ,$v,$NJ,$x,$y},$z);$PJ={$Ie,$yJ,$zJ,$CJ,$DJ,$GJ,$HJ,$KJ,$LJ,$OJ};$QJ=bless({$c,$uJ,$vJ,$PJ},$G7);*$yD=\&$wD;*$xD=\&$uD;$w6->apply_($o5);$w6->apply_($p5);$w6->apply_($q5);$w6->apply_($r5);$w6->apply_($s5);$w6->apply_($t5);$w6->apply_($u5);$w6->apply_($v5);$w6->apply_($w5);$w6->apply_($x5);$w6->apply_($y5);$w6->apply_($z5);$w6->apply_($A5);$w6->apply_($B5);$w6->apply_($C5);$w6->apply_($D5);$w6->apply_($E5);$w6->apply_($F5);$w6->apply_($x6);$w6->apply_($G5);$w6->apply_($H5);$w6->apply_($I5);$w6->apply_($J5);$w6->apply_($K5);$w6->apply_($L5);$w6->apply_($M5);$w6->apply_($N5);$w6->apply_($O5);$w6->apply_($P5);$w6->apply_($Q5);$w6->apply_($R5);$w6->apply_($S5);$w6->apply_($T5);$w6->apply_($e6);$w6->apply_($U5);$w6->apply_($f6);$w6->apply_($V5);$w6->apply_($W5);$w6->apply_($X5);$w6->apply_($Y5);$w6->apply_($Z5);$D6->apply_($o5);$D6->apply_($p5);$D6->apply_($q5);$D6->apply_($r5);$D6->apply_($s5);$D6->apply_($t5);$D6->apply_($u5);$D6->apply_($v5);$D6->apply_($w5);$D6->apply_($x5);$D6->apply_($y5);$D6->apply_($z5);$D6->apply_($A5);$D6->apply_($B5);$D6->apply_($C5);$D6->apply_($D5);$D6->apply_($E5);$D6->apply_($F5);$D6->apply_($x6);$D6->apply_($G5);$D6->apply_($H5);$D6->apply_($L);$D6->apply_($I5);$D6->apply_($J5);$D6->apply_($K5);$D6->apply_($L5);$D6->apply_($M5);$D6->apply_($N5);$D6->apply_($v6);$D6->apply_($O5);$D6->apply_($E6);$D6->apply_($P5);$D6->apply_($Q5);$D6->apply_($R5);$D6->apply_($S5);$D6->apply_($T5);$D6->apply_($e6);$D6->apply_($U5);$D6->apply_($f6);$D6->apply_($V5);$D6->apply_($W5);$D6->apply_($X5);$D6->apply_($Y5);$D6->apply_($Z5);$L6->apply_($o5);$L6->apply_($p5);$L6->apply_($q5);$L6->apply_($r5);$L6->apply_($s5);$L6->apply_($t5);$L6->apply_($u5);$L6->apply_($v5);$L6->apply_($w5);$L6->apply_($x5);$L6->apply_($y5);$L6->apply_($z5);$L6->apply_($A5);$L6->apply_($B5);$L6->apply_($C5);$L6->apply_($D5);$L6->apply_($E5);$L6->apply_($F5);$L6->apply_($x6);$L6->apply_($G5);$L6->apply_($H5);$L6->apply_($I5);$L6->apply_($J5);$L6->apply_($K5);$L6->apply_($L5);$L6->apply_($M5);$L6->apply_($N5);$L6->apply_($v6);$L6->apply_($O5);$L6->apply_($E6);$L6->apply_($P5);$L6->apply_($Q5);$L6->apply_($R5);$L6->apply_($S5);$L6->apply_($T5);$L6->apply_($e6);$L6->apply_($U5);$L6->apply_($f6);$L6->apply_($V5);$L6->apply_($W5);$L6->apply_($X5);$L6->apply_($Y5);$L6->apply_($Z5);$S6->apply_($o5);$S6->apply_($p5);$S6->apply_($q5);$S6->apply_($r5);$S6->apply_($s5);$S6->apply_($t5);$S6->apply_($u5);$S6->apply_($v5);$S6->apply_($w5);$S6->apply_($x5);$S6->apply_($y5);$S6->apply_($z5);$S6->apply_($A5);$S6->apply_($B5);$S6->apply_($C5);$S6->apply_($D5);$S6->apply_($E5);$S6->apply_($F5);$S6->apply_($x6);$S6->apply_($G5);$S6->apply_($H5);$S6->apply_($I5);$S6->apply_($J5);$S6->apply_($K5);$S6->apply_($L5);$S6->apply_($M5);$S6->apply_($N5);$S6->apply_($v6);$S6->apply_($O5);$S6->apply_($E6);$S6->apply_($P5);$S6->apply_($Q5);$S6->apply_($R5);$S6->apply_($S5);$S6->apply_($T5);$S6->apply_($e6);$S6->apply_($U5);$S6->apply_($f6);$S6->apply_($V5);$S6->apply_($W5);$S6->apply_($X5);$S6->apply_($Y5);$S6->apply_($Z5);$Z6->apply_($o5);$Z6->apply_($p5);$Z6->apply_($q5);$Z6->apply_($r5);$Z6->apply_($s5);$Z6->apply_($t5);$Z6->apply_($u5);$Z6->apply_($v5);$Z6->apply_($w5);$Z6->apply_($x5);$Z6->apply_($y5);$Z6->apply_($z5);$Z6->apply_($A5);$Z6->apply_($B5);$Z6->apply_($C5);$Z6->apply_($D5);$Z6->apply_($E5);$Z6->apply_($F5);$Z6->apply_($x6);$Z6->apply_($G5);$Z6->apply_($H5);$Z6->apply_($I5);$Z6->apply_($J5);$Z6->apply_($K5);$Z6->apply_($L5);$Z6->apply_($M5);$Z6->apply_($N5);$Z6->apply_($O5);$Z6->apply_($E6);$Z6->apply_($P5);$Z6->apply_($Q5);$Z6->apply_($R5);$Z6->apply_($S5);$Z6->apply_($T5);$Z6->apply_($e6);$Z6->apply_($U5);$Z6->apply_($f6);$Z6->apply_($V5);$Z6->apply_($W5);$Z6->apply_($X5);$Z6->apply_($Y5);$Z6->apply_($Z5);$m7->apply_($o5);$m7->apply_($p5);$m7->apply_($q5);$m7->apply_($r5);$m7->apply_($s5);$m7->apply_($t5);$m7->apply_($u5);$m7->apply_($v5);$m7->apply_($w5);$m7->apply_($x5);$m7->apply_($y5);$m7->apply_($z5);$m7->apply_($A5);$m7->apply_($B5);$m7->apply_($C5);$m7->apply_($D5);$m7->apply_($E5);$m7->apply_($F5);$m7->apply_($G5);$m7->apply_($H5);$m7->apply_($I5);$m7->apply_($J5);$m7->apply_($K5);$m7->apply_($L5);$m7->apply_($M5);$m7->apply_($N5);$m7->apply_($O5);$m7->apply_($P5);$m7->apply_($Q5);$m7->apply_($R5);$m7->apply_($S5);$m7->apply_($T5);$m7->apply_($e6);$m7->apply_($U5);$m7->apply_($f6);$m7->apply_($V5);$m7->apply_($W5);$m7->apply_($X5);$m7->apply_($Y5);$m7->apply_($Z5);$X7->apply_($o5);$X7->apply_($p5);$X7->apply_($n7);$X7->apply_($q5);$X7->apply_($o7);$X7->apply_($r5);$X7->apply_($p7);$X7->apply_($s5);$X7->apply_($q7);$X7->apply_($t5);$X7->apply_($r7);$X7->apply_($u5);$X7->apply_($s7);$X7->apply_($v5);$X7->apply_($t7);$X7->apply_($w5);$X7->apply_($u7);$X7->apply_($x5);$X7->apply_($v7);$X7->apply_($y5);$X7->apply_($w7);$X7->apply_($z5);$X7->apply_($x7);$X7->apply_($A5);$X7->apply_($y7);$X7->apply_($B5);$X7->apply_($z7);$X7->apply_($C5);$X7->apply_($A7);$X7->apply_($D5);$X7->apply_($B7);$X7->apply_($E5);$X7->apply_($C7);$X7->apply_($F5);$X7->apply_($x6);$X7->apply_($G5);$X7->apply_($D7);$X7->apply_($H5);$X7->apply_($L);$X7->apply_($I5);$X7->apply_($z);$X7->apply_($J5);$X7->apply_($E7);$X7->apply_($K5);$X7->apply_($F7);$X7->apply_($L5);$X7->apply_($G7);$X7->apply_($M5);$X7->apply_($H7);$X7->apply_($N5);$X7->apply_($v6);$X7->apply_($O5);$X7->apply_($E6);$X7->apply_($P5);$X7->apply_($I7);$X7->apply_($Q5);$X7->apply_($J7);$X7->apply_($R5);$X7->apply_($B);$X7->apply_($S5);$X7->apply_($K7);$X7->apply_($T5);$X7->apply_($e6);$X7->apply_($U5);$X7->apply_($f6);$X7->apply_($V5);$X7->apply_($L7);$X7->apply_($W5);$X7->apply_($X5);$X7->apply_($Y5);$X7->apply_($M7);$X7->apply_($Z5);$j8->apply_($o5);$j8->apply_($p5);$j8->apply_($q5);$j8->apply_($r5);$j8->apply_($s5);$j8->apply_($t5);$j8->apply_($u5);$j8->apply_($v5);$j8->apply_($w5);$j8->apply_($x5);$j8->apply_($y5);$j8->apply_($z5);$j8->apply_($A5);$j8->apply_($B5);$j8->apply_($C5);$j8->apply_($D5);$j8->apply_($E5);$j8->apply_($C7);$j8->apply_($F5);$j8->apply_($x6);$j8->apply_($G5);$j8->apply_($D7);$j8->apply_($H5);$j8->apply_($I5);$j8->apply_($J5);$j8->apply_($K5);$j8->apply_($L5);$j8->apply_($M5);$j8->apply_($N5);$j8->apply_($v6);$j8->apply_($O5);$j8->apply_($E6);$j8->apply_($P5);$j8->apply_($Q5);$j8->apply_($R5);$j8->apply_($S5);$j8->apply_($T5);$j8->apply_($e6);$j8->apply_($U5);$j8->apply_($f6);$j8->apply_($V5);$j8->apply_($W5);$j8->apply_($X5);$j8->apply_($Y5);$j8->apply_($Z5);$u8->apply_($o5);$u8->apply_($p5);$u8->apply_($q5);$u8->apply_($r5);$u8->apply_($s5);$u8->apply_($t5);$u8->apply_($u5);$u8->apply_($v5);$u8->apply_($w5);$u8->apply_($x5);$u8->apply_($y5);$u8->apply_($z5);$u8->apply_($A5);$u8->apply_($B5);$u8->apply_($C5);$u8->apply_($D5);$u8->apply_($E5);$u8->apply_($F5);$u8->apply_($x6);$u8->apply_($G5);$u8->apply_($H5);$u8->apply_($I5);$u8->apply_($J5);$u8->apply_($K5);$u8->apply_($L5);$u8->apply_($M5);$u8->apply_($N5);$u8->apply_($O5);$u8->apply_($P5);$u8->apply_($Q5);$u8->apply_($R5);$u8->apply_($S5);$u8->apply_($T5);$u8->apply_($e6);$u8->apply_($U5);$u8->apply_($f6);$u8->apply_($V5);$u8->apply_($W5);$u8->apply_($X5);$u8->apply_($Y5);$u8->apply_($Z5);$E8->apply_($o5);$E8->apply_($p5);$E8->apply_($q5);$E8->apply_($r5);$E8->apply_($s5);$E8->apply_($t5);$E8->apply_($u5);$E8->apply_($v5);$E8->apply_($w5);$E8->apply_($x5);$E8->apply_($y5);$E8->apply_($z5);$E8->apply_($A5);$E8->apply_($B5);$E8->apply_($C5);$E8->apply_($D5);$E8->apply_($E5);$E8->apply_($F5);$E8->apply_($x6);$E8->apply_($G5);$E8->apply_($H5);$E8->apply_($I5);$E8->apply_($J5);$E8->apply_($K5);$E8->apply_($L5);$E8->apply_($M5);$E8->apply_($N5);$E8->apply_($O5);$E8->apply_($P5);$E8->apply_($Q5);$E8->apply_($R5);$E8->apply_($S5);$E8->apply_($T5);$E8->apply_($e6);$E8->apply_($U5);$E8->apply_($f6);$E8->apply_($V5);$E8->apply_($W5);$E8->apply_($X5);$E8->apply_($Y5);$E8->apply_($Z5);$L8->apply_($o5);$L8->apply_($p5);$L8->apply_($q5);$L8->apply_($r5);$L8->apply_($s5);$L8->apply_($t5);$L8->apply_($u5);$L8->apply_($v5);$L8->apply_($w5);$L8->apply_($x5);$L8->apply_($y5);$L8->apply_($z5);$L8->apply_($A5);$L8->apply_($B5);$L8->apply_($C5);$L8->apply_($D5);$L8->apply_($E5);$L8->apply_($F5);$L8->apply_($x6);$L8->apply_($G5);$L8->apply_($H5);$L8->apply_($I5);$L8->apply_($J5);$L8->apply_($K5);$L8->apply_($L5);$L8->apply_($M5);$L8->apply_($N5);$L8->apply_($O5);$L8->apply_($P5);$L8->apply_($Q5);$L8->apply_($R5);$L8->apply_($S5);$L8->apply_($T5);$L8->apply_($e6);$L8->apply_($U5);$L8->apply_($f6);$L8->apply_($V5);$L8->apply_($W5);$L8->apply_($X5);$L8->apply_($Y5);$L8->apply_($Z5);$S8->apply_($o5);$S8->apply_($p5);$S8->apply_($q5);$S8->apply_($r5);$S8->apply_($s5);$S8->apply_($t5);$S8->apply_($u5);$S8->apply_($v5);$S8->apply_($w5);$S8->apply_($x5);$S8->apply_($y5);$S8->apply_($z5);$S8->apply_($A5);$S8->apply_($B5);$S8->apply_($C5);$S8->apply_($D5);$S8->apply_($E5);$S8->apply_($F5);$S8->apply_($x6);$S8->apply_($G5);$S8->apply_($H5);$S8->apply_($I5);$S8->apply_($J5);$S8->apply_($K5);$S8->apply_($L5);$S8->apply_($M5);$S8->apply_($N5);$S8->apply_($O5);$S8->apply_($P5);$S8->apply_($Q5);$S8->apply_($R5);$S8->apply_($S5);$S8->apply_($T5);$S8->apply_($e6);$S8->apply_($U5);$S8->apply_($f6);$S8->apply_($V5);$S8->apply_($W5);$S8->apply_($X5);$S8->apply_($Y5);$S8->apply_($Z5);$Z8->apply_($o5);$Z8->apply_($p5);$Z8->apply_($q5);$Z8->apply_($r5);$Z8->apply_($s5);$Z8->apply_($t5);$Z8->apply_($u5);$Z8->apply_($v5);$Z8->apply_($w5);$Z8->apply_($x5);$Z8->apply_($y5);$Z8->apply_($z5);$Z8->apply_($A5);$Z8->apply_($B5);$Z8->apply_($C5);$Z8->apply_($D5);$Z8->apply_($E5);$Z8->apply_($F5);$Z8->apply_($x6);$Z8->apply_($G5);$Z8->apply_($H5);$Z8->apply_($I5);$Z8->apply_($J5);$Z8->apply_($K5);$Z8->apply_($L5);$Z8->apply_($M5);$Z8->apply_($N5);$Z8->apply_($O5);$Z8->apply_($P5);$Z8->apply_($Q5);$Z8->apply_($R5);$Z8->apply_($S5);$Z8->apply_($T5);$Z8->apply_($e6);$Z8->apply_($U5);$Z8->apply_($f6);$Z8->apply_($V5);$Z8->apply_($W5);$Z8->apply_($X5);$Z8->apply_($Y5);$Z8->apply_($Z5);$m9->apply_($o5);$m9->apply_($p5);$m9->apply_($q5);$m9->apply_($r5);$m9->apply_($s5);$m9->apply_($t5);$m9->apply_($u5);$m9->apply_($v5);$m9->apply_($w5);$m9->apply_($x5);$m9->apply_($y5);$m9->apply_($z5);$m9->apply_($A5);$m9->apply_($B5);$m9->apply_($C5);$m9->apply_($D5);$m9->apply_($E5);$m9->apply_($F5);$m9->apply_($G5);$m9->apply_($H5);$m9->apply_($I5);$m9->apply_($z);$m9->apply_($J5);$m9->apply_($K5);$m9->apply_($L5);$m9->apply_($M5);$m9->apply_($N5);$m9->apply_($v6);$m9->apply_($O5);$m9->apply_($E6);$m9->apply_($P5);$m9->apply_($Q5);$m9->apply_($R5);$m9->apply_($S5);$m9->apply_($T5);$m9->apply_($e6);$m9->apply_($U5);$m9->apply_($V5);$m9->apply_($W5);$m9->apply_($X5);$m9->apply_($Y5);$m9->apply_($Z5);$t9->apply_($o5);$t9->apply_($p5);$t9->apply_($q5);$t9->apply_($r5);$t9->apply_($s5);$t9->apply_($t5);$t9->apply_($u5);$t9->apply_($v5);$t9->apply_($w5);$t9->apply_($x5);$t9->apply_($y5);$t9->apply_($z5);$t9->apply_($A5);$t9->apply_($B5);$t9->apply_($C5);$t9->apply_($D5);$t9->apply_($E5);$t9->apply_($F5);$t9->apply_($G5);$t9->apply_($H5);$t9->apply_($I5);$t9->apply_($J5);$t9->apply_($K5);$t9->apply_($L5);$t9->apply_($M5);$t9->apply_($N5);$t9->apply_($O5);$t9->apply_($P5);$t9->apply_($Q5);$t9->apply_($R5);$t9->apply_($S5);$t9->apply_($T5);$t9->apply_($U5);$t9->apply_($V5);$t9->apply_($W5);$t9->apply_($X5);$t9->apply_($Y5);$t9->apply_($Z5);$V9->apply_($n7);$fa->apply_($n7);$xa->apply_($o7);$Fa->apply_($o7);$cb->apply_($p7);$cb->apply_($q7);$cb->apply_($r7);$cb->apply_($s7);$cb->apply_($t7);$cb->apply_($u7);$cb->apply_($v7);$cb->apply_($w7);$cb->apply_($x7);$cb->apply_($y7);$wb->apply_($p7);$wb->apply_($q7);$wb->apply_($r7);$wb->apply_($s7);$wb->apply_($t7);$wb->apply_($u7);$wb->apply_($v7);$wb->apply_($w7);$wb->apply_($x7);$wb->apply_($y7);$Eb->apply_($p7);$Eb->apply_($q7);$Eb->apply_($r7);$Eb->apply_($s7);$Eb->apply_($t7);$Eb->apply_($u7);$Eb->apply_($v7);$Eb->apply_($w7);$Eb->apply_($x7);$Eb->apply_($y7);$Qb->apply_($p7);$Qb->apply_($q7);$Qb->apply_($r7);$Qb->apply_($s7);$Qb->apply_($t7);$Qb->apply_($u7);$Qb->apply_($v7);$Qb->apply_($w7);$Qb->apply_($x7);$Qb->apply_($y7);$ec->apply_($p7);$ec->apply_($q7);$ec->apply_($r7);$ec->apply_($s7);$ec->apply_($t7);$ec->apply_($u7);$ec->apply_($v7);$ec->apply_($w7);$ec->apply_($x7);$ec->apply_($y7);$qc->apply_($p7);$qc->apply_($q7);$qc->apply_($r7);$qc->apply_($s7);$qc->apply_($t7);$qc->apply_($u7);$qc->apply_($v7);$qc->apply_($w7);$qc->apply_($x7);$qc->apply_($y7);$zc->apply_($p7);$Tc->apply_($p7);$kd->apply_($s5);$kd->apply_($t5);$kd->apply_($u5);$kd->apply_($v5);$kd->apply_($w5);$kd->apply_($x5);$kd->apply_($y5);$kd->apply_($z5);$kd->apply_($A5);$kd->apply_($B5);$zd->apply_($q7);$Gd->apply_($q7);$Zd->apply_($r7);$ie->apply_($r7);$Ce->apply_($r7);$cf->apply_($r7);$kf->apply_($r7);$Af->apply_($r7);$Xf->apply_($s7);$Xf->apply_($u7);$gg->apply_($s7);$og->apply_($s7);$Eg->apply_($s7);$Eg->apply_($u7);$Pg->apply_($s7);$fh->apply_($s7);$fh->apply_($u7);$Ih->apply_($v5);$ci->apply_($t7);$ki->apply_($t7);$ri->apply_($t7);$Hi->apply_($t7);$Pi->apply_($t7);$mj->apply_($t7);$Ij->apply_($u7);$Rj->apply_($u7);$Ek->apply_($Fk);$Pk->apply_($v7);$el->apply_($v7);$Kl->apply_($x7);$Ul->apply_($x7);$im->apply_($x7);$sm->apply_($x7);$Im->apply_($x7);$pn->apply_($y7);$wn->apply_($y7);$Kn->apply_($y7);$io->apply_($z7);$io->apply_($A7);$io->apply_($B7);$io->apply_($M7);$uo->apply_($z7);$uo->apply_($A7);$uo->apply_($B7);$uo->apply_($M7);$Jo->apply_($z7);$Jo->apply_($A7);$Jo->apply_($B7);$fp->apply_($z7);$fp->apply_($A7);$fp->apply_($B7);$vp->apply_($C5);$vp->apply_($D5);$vp->apply_($E5);$Rp->apply_($A7);$Yp->apply_($A7);$kq->apply_($A7);$wq->apply_($A7);$Xq->apply_($D5);$sr->apply_($B7);$zr->apply_($B7);$Ur->apply_($x6);$ps->apply_($D7);$vs->apply_($D7);$Ps->apply_($L);$Vs->apply_($L);$gt->apply_($L);$pt->apply_($L);$Bt->apply_($L);$eu->apply_($z);$wu->apply_($z);$Fu->apply_($z);$Nu->apply_($z);$fv->apply_($J5);$Cv->apply_($E7);$Jv->apply_($E7);$Zv->apply_($E7);$vw->apply_($E7);$Pw->apply_($F7);$ex->apply_($Fk);$nx->apply_($F7);$Lx->apply_($F7);$Tx->apply_($F7);$Tx->apply_($H7);$ty->apply_($F7);$ty->apply_($H7);$Jy->apply_($F7);$Jy->apply_($H7);$Zy->apply_($F7);$vz->apply_($F7);$lA->apply_($X9);$EA->apply_($G7);$WA->apply_($G7);$hB->apply_($G7);$rB->apply_($G7);$FC->apply_($X9);$TC->apply_($H7);$eD->apply_($H7);$DD->apply_($v6);$JD->apply_($v6);$QD->apply_($v6);$oE->apply_($Fk);$yE->apply_($E6);$EE->apply_($E6);$cF->apply_($I7);$cF->apply_($J7);$lF->apply_($I7);$zF->apply_($I7);$fG->apply_($B);$mG->apply_($B);$tG->apply_($B);$DG->apply_($B);$OG->apply_($B);$fH->apply_($S5);$nH->apply_($S5);$GH->apply_($K7);$WH->apply_($K7);$fI->apply_($K7);$ni::self=$QJ;&$O($M);&$O($U);&$O($e1);&$O($k1);&$O($x1);&$O($K1);&$O($X1);&$O($m2);&$O($I2);&$O($O2);&$O($n3);&$O($u3);&$O($O3);&$O($h4);&$O($p4);&$O($x4);&$O($I4);&$O($f5);&$O($l5);&$O($w6);&$O($D6);&$O($L6);&$O($S6);&$O($Z6);&$O($d7);&$O($m7);&$O($X7);&$O($Z7);&$g7($Z7);&$O($j8);&$O($l8);&$g7($l8);&$O($u8);&$O($E8);&$O($L8);&$O($S8);&$O($Z8);&$O($d9);&$O($f9);&$g7($f9);&$O($m9);&$O($t9);&$O($v9);&$g7($v9);&$O($E9);&$g7($E9);&$O($I9);&$g7($I9);&$O($K9);&$g7($K9);&$O($M9);&$g7($M9);&$O($V9);&$O($fa);&$O($ha);&$g7($ha);&$O($ma);&$g7($ma);&$O($xa);&$O($Fa);&$O($Ha);&$g7($Ha);&$O($Ma);&$g7($Ma);&$O($cb);&$O($wb);&$O($Eb);&$O($Qb);&$O($ec);&$O($qc);&$O($sc);&$g7($sc);&$O($zc);&$O($Tc);&$O($Vc);&$g7($Vc);&$O($kd);&$O($md);&$g7($md);&$O($od);&$g7($od);&$O($zd);&$O($Gd);&$O($Id);&$g7($Id);&$O($Nd);&$g7($Nd);&$O($Zd);&$O($ie);&$O($Ce);&$O($cf);&$O($kf);&$O($Af);&$O($Cf);&$g7($Cf);&$O($Hf);&$g7($Hf);&$O($Xf);&$O($gg);&$O($og);&$O($Eg);&$O($Pg);&$O($fh);&$O($ih);&$g7($ih);&$lh($ih);&$O($Ih);&$O($Kh);&$g7($Kh);&$O($ci);&$O($ki);&$O($ri);&$O($Hi);&$O($Pi);&$O($mj);&$O($oj);&$g7($oj);&$O($tj);&$g7($tj);&$O($Ij);&$O($Rj);&$O($Tj);&$g7($Tj);&$O($Yj);&$g7($Yj);&$O($Ek);&$O($Pk);&$O($el);&$O($gl);&$g7($gl);&$O($ll);&$g7($ll);&$O($Kl);&$O($Ul);&$O($im);&$O($sm);&$O($Im);&$O($Km);&$g7($Km);&$O($Pm);&$g7($Pm);&$O($pn);&$O($wn);&$O($Kn);&$O($Mn);&$g7($Mn);&$O($Rn);&$g7($Rn);&$O($io);&$O($uo);&$O($wo);&$g7($wo);&$O($Jo);&$O($fp);&$O($hp);&$g7($hp);&$kp($hp);&$O($rp);&$g7($rp);&$O($vp);&$O($xp);&$g7($xp);&$O($Rp);&$O($Yp);&$O($kq);&$O($wq);&$O($Bq);&$g7($Bq);&$kp($Bq);&$Eq($Bq);&$O($Xq);&$O($Zq);&$g7($Zq);&$O($sr);&$O($zr);&$O($Br);&$g7($Br);&$kp($Br);&$O($Gr);&$g7($Gr);&$O($Ur);&$O($Wr);&$g7($Wr);&$O($es);&$g7($es);&$O($ps);&$O($vs);&$O($xs);&$g7($xs);&$O($Cs);&$g7($Cs);&$O($Ps);&$O($Vs);&$O($gt);&$O($pt);&$O($Bt);&$O($Dt);&$g7($Dt);&$O($It);&$g7($It);&$O($eu);&$O($wu);&$O($Fu);&$O($Nu);&$O($Pu);&$g7($Pu);&$Su($Pu);&$O($fv);&$O($hv);&$g7($hv);&$O($Cv);&$O($Jv);&$O($Zv);&$O($vw);&$O($xw);&$g7($xw);&$O($Cw);&$g7($Cw);&$O($Pw);&$O($ex);&$O($nx);&$O($Lx);&$O($Tx);&$O($ty);&$O($Jy);&$O($Zy);&$O($vz);&$O($xz);&$g7($xz);&$O($Cz);&$g7($Cz);&$O($lA);&$O($EA);&$O($WA);&$O($hB);&$O($rB);&$O($tB);&$g7($tB);&$O($yB);&$g7($yB);&$O($FC);&$O($TC);&$O($eD);&$O($gD);&$g7($gD);&$O($lD);&$g7($lD);&$O($DD);&$O($JD);&$O($QD);&$O($SD);&$g7($SD);&$O($YD);&$g7($YD);&$O($oE);&$O($yE);&$O($EE);&$O($GE);&$g7($GE);&$O($ME);&$g7($ME);&$O($cF);&$O($eF);&$g7($eF);&$O($lF);&$O($zF);&$O($BF);&$g7($BF);&$O($IF);&$g7($IF);&$O($KF);&$g7($KF);&$O($fG);&$O($mG);&$O($tG);&$O($DG);&$O($OG);&$O($QG);&$g7($QG);&$TG($QG);&$O($fH);&$O($nH);&$O($pH);&$g7($pH);&$O($GH);&$O($WH);&$O($fI);&$O($hI);&$g7($hI);&$O($mI);&$g7($mI);&$O($uI);&$g7($uI);&$O($zI);&$g7($zI);&$O($II);&$g7($II);&$O($NI);&$g7($NI);&$O($VI);&$g7($VI);&$O($tJ);&$g7($tJ);ni->run(@ARGV);
__DATA__
