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
a replica of the runtime object-oriented state.#;$m4=q#ni ends up being a quine:#;$n4=[];$o4=[];$p4=q#if (-r $0) {
  my $contents = ni("file:$0")->read_all;
  my $replica  = ni->quoted->io->read_all;
  now $replica == $contents;
}#;$q4=bless({$t,$o4,$v,$p4,$x,$y},$z);$r4=bless({$n,$n4,$p,$q,$r,$q,$s,$q4},$B);$s4=[$i,$l4,$m4,$r4];$t4=[$k4,$s4];$u4=q#/lib/image#;$v4=bless({$e,$t4,$J,$u4},$L);$w4=q#ni.doc:/lib/ni#;$x4=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$y4=[$f,$x4];$z4=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$A4=[$i,$z4];$B4=[$y4,$A4];$C4=q#/lib/ni#;$D4=bless({$e,$B4,$J,$C4},$L);$E4=q#ni.doc:/lib/quote_simple#;$F4=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$G4=[];$H4=[];$I4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$J4=bless({$t,$H4,$v,$I4,$x,$y},$z);$K4=bless({$n,$G4,$p,$q,$r,$q,$s,$J4},$B);$L4=[$i,$F4,$K4];$M4=[$L4];$N4=q#/lib/quote_simple#;$O4=bless({$e,$M4,$J,$N4},$L);$P4=q#ni.doc:/lib/slice#;$Q4=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$R4=[$f,$Q4];$S4=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$T4=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$U4=[];$V4=[];$W4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$X4=bless({$t,$V4,$v,$W4,$x,$y},$z);$Y4=bless({$n,$U4,$p,$q,$r,$q,$s,$X4},$B);$Z4=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$c5=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$d5=[];$e5=[];$f5=q#my $instances = 0;
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
now $instances == 0;#;$g5=bless({$t,$e5,$v,$f5,$x,$y},$z);$h5=bless({$n,$d5,$p,$q,$r,$q,$s,$g5},$B);$i5=[$i,$S4,$T4,$Y4,$Z4,$c5,$h5];$j5=[$R4,$i5];$k5=q#/lib/slice#;$l5=bless({$e,$j5,$J,$k5},$L);$m5=q#ni.doc:/semantic#;$n5=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$o5=[$i,$n5];$p5=[$o5];$q5=q#/semantic#;$r5=bless({$e,$p5,$J,$q5},$L);$s5=q#ni:/class#;$t5=q#applied_to#;$u5=q#class#;$v5=q#class.c#;$w5=q#fabric/perl.c#;$x5=q#fabric/perl_proxy.c#;$y5=q#io/buffer.c#;$z5=q#io/cat.c#;$A5=q#io/exec.c#;$B5=q#io/fd.c#;$C5=q#io/file.c#;$D5=q#io/file_update_fd.c#;$E5=q#io/null.c#;$F5=q#io/object.c#;$G5=q#io/pid.c#;$H5=q#io/str.c#;$I5=q#io/transfer.c#;$J5=q#io/transfer_async.c#;$K5=q#io/transfer_sync.c#;$L5=q#lib/behavior.c#;$M5=q#lib/branch.c#;$N5=q#lib/dataslice.c#;$O5=q#lib/doc.c#;$P5=q#lib/fn.c#;$Q5=q#lib/future.c#;$R5=q#lib/image.c#;$S5=q#lib/ni.c#;$T5=q#lib/quote_simple.c#;$U5=q#lib/slice.c#;$V5=q#lib/tag.c#;$W5=q#lib/test_assert_eq.c#;$X5=q#lib/test_assertion.c#;$Y5=q#lib/test_case.c#;$Z5=q#lib/test_value.c#;$c6=q#metaclass.c#;$d6=q#module.c#;$e6=q#object.c#;$f6=q#semantic/dimension#;$g6=q#semantic/dimension.c#;$h6=q#semantic/task.c#;$i6={$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$c6,1,$d6,1,$e6,1,$f6,1,$g6,1,$h6,1};$j6=q#slices#;$k6=q#metaclass#;$l6=q#module#;$m6={$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$k6,1,$c6,1,$l6,1,$d6,1,$e6,1,$f6,1,$g6,1,$h6,1};$n6=q#/module#;$o6=q#/lib/perlbranch.b#;$p6={};$q6=q#ctor#;$r6=q#dtor#;$s6=q#methods#;$t6=q#add#;$u6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$v6=bless({$v,$u6},$z);$w6=q#apply#;$x6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$y6=bless({$v,$x6},$z);$z6={$t6,$v6,$w6,$y6};$A6=q#/lib/branch.b#;$B6=q#lib/slice#;$C6=bless({$t5,$p6,$q6,$q,$r6,$q,$s6,$z6,$J,$A6},$B6);$D6=q#lib/branch#;$E6={};$F6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$G6=bless({$v,$F6},$z);$H6={$J,$G6};$I6=q#/lib/named.b#;$J6=bless({$t5,$E6,$q6,$O,$r6,$q,$s6,$H6,$J,$I6},$B6);$K6=q#lib/tag#;$L6={};$M6=q#namespace#;$N6=q#'ni'#;$O6=bless({$v,$N6},$z);$P6={$M6,$O6};$Q6=q#/lib/named_in_ni.b#;$R6=bless({$t5,$L6,$q6,$q,$r6,$q,$s6,$P6,$J,$Q6},$B6);$S6={};$T6=q#package#;$U6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$V6=bless({$v,$U6},$z);$W6={$T6,$V6};$X6=q#/lib/namespaced.b#;$Y6=bless({$t5,$S6,$q6,$q,$r6,$q,$s6,$W6,$J,$X6},$B6);$Z6={};$c7=q#resolve#;$d7=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$e7=bless({$v,$d7},$z);$f7={$c7,$e7};$g7=q#/lib/resolver.b#;$h7=bless({$t5,$Z6,$q6,$q,$r6,$q,$s6,$f7,$J,$g7},$B6);$i7=[$C6,$J6,$R6,$Y6,$h7];$j7=bless({$J,$o6,$j6,$i7},$K6);$k7={};$l7=q#my $s = shift; $s->apply($s->package)#;$m7=bless({$v,$l7,$x,$y},$z);$n7=q#instantiate#;$o7=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$p7=bless({$v,$o7},$z);$q7={$n7,$p7};$r7=q#/lib/class_init.b#;$s7=bless({$t5,$k7,$q6,$m7,$r6,$q,$s6,$q7,$J,$r7},$B6);$t7=q#fabric/perl#;$u7=q#fabric/perl_proxy#;$v7=q#io/buffer#;$w7=q#io/cat#;$x7=q#io/exec#;$y7=q#io/fd#;$z7=q#io/file#;$A7=q#io/file_update_fd#;$B7=q#io/null#;$C7=q#io/object#;$D7=q#io/pid#;$E7=q#io/str#;$F7=q#io/transfer#;$G7=q#io/transfer_async#;$H7=q#io/transfer_sync#;$I7=q#lib/behavior#;$J7=q#lib/dataslice#;$K7=q#lib/future#;$L7=q#lib/image#;$M7=q#lib/ni#;$N7=q#lib/quote_simple#;$O7=q#lib/test_assert_eq#;$P7=q#lib/test_assertion#;$Q7=q#lib/test_value#;$R7=q#object#;$S7=q#semantic/task#;$T7={$u5,1,$v5,1,$t7,1,$w5,1,$u7,1,$x5,1,$v7,1,$y5,1,$w7,1,$z5,1,$x7,1,$A5,1,$y7,1,$B5,1,$z7,1,$C5,1,$A7,1,$D5,1,$B7,1,$E5,1,$C7,1,$F5,1,$D7,1,$G5,1,$E7,1,$H5,1,$F7,1,$I5,1,$G7,1,$J5,1,$H7,1,$K5,1,$I7,1,$L5,1,$D6,1,$M5,1,$J7,1,$N5,1,$L,1,$O5,1,$z,1,$P5,1,$K7,1,$Q5,1,$L7,1,$R5,1,$M7,1,$S5,1,$N7,1,$T5,1,$B6,1,$U5,1,$K6,1,$V5,1,$O7,1,$W5,1,$P7,1,$X5,1,$B,1,$Y5,1,$Q7,1,$Z5,1,$k6,1,$c6,1,$l6,1,$d6,1,$R7,1,$e6,1,$f6,1,$g6,1,$S7,1,$h6,1};$U7=q#/object#;$V7={};$W7=q#DESTROY#;$X7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$Y7=bless({$v,$X7},$z);$Z7=q#ni 'ni:/' . ref shift#;$c8=bless({$v,$Z7},$z);$d8={$W7,$Y7,$u5,$c8};$e8=q#/lib/instance.b#;$f8=bless({$t5,$V7,$q6,$q,$r6,$q,$s6,$d8,$J,$e8},$B6);$g8=[$f8];$h8=bless({$t5,$T7,$J,$U7,$j6,$g8},$e6);$i8={$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$I7,1,$L5,1,$D6,1,$M5,1,$J7,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$B6,1,$U5,1,$K6,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$k6,1,$c6,1,$l6,1,$d6,1,$e6,1,$f6,1,$g6,1,$h6,1};$j8=q#/lib/behavior#;$k8={};$l8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$m8=bless({$v,$l8},$z);$n8={$e,$m8};$o8=q#/lib/documentable.b#;$p8=bless({$t5,$k8,$q6,$q,$r6,$q,$s6,$n8,$J,$o8},$B6);$q8=[$h8,$p8];$r8=bless({$t5,$i8,$J,$j8,$j6,$q8},$L5);$s8={$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$D6,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$k6,1,$c6,1,$l6,1,$d6,1,$e6,1,$f6,1,$g6,1,$h6,1};$t8=q#/lib/definition.b#;$u8={};$v8=q#def#;$w8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$x8=bless({$v,$w8},$z);$y8={$v8,$x8};$z8=q#/lib/definition_def.b#;$A8=bless({$t5,$u8,$q6,$q,$r6,$q,$s6,$y8,$J,$z8},$B6);$B8={};$C8=q#ro#;$D8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$E8=bless({$v,$D8},$z);$F8=q#rw#;$G8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$H8=bless({$v,$G8},$z);$I8={$C8,$E8,$F8,$H8};$J8=q#/lib/accessor.b#;$K8=bless({$t5,$B8,$q6,$q,$r6,$q,$s6,$I8,$J,$J8},$B6);$L8={};$M8=q#(""#;$N8=q#shift->name#;$O8=bless({$v,$N8},$z);$P8={$M8,$O8};$Q8=q#/lib/name_as_string.b#;$R8=bless({$t5,$L8,$q6,$q,$r6,$q,$s6,$P8,$J,$Q8},$B6);$S8={};$T8=q#(eq#;$U8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$V8=bless({$v,$U8},$z);$W8={$T8,$V8};$X8=q#/lib/ref_eq.b#;$Y8=bless({$t5,$S8,$q6,$q,$r6,$q,$s6,$W8,$J,$X8},$B6);$Z8={};$c9=q#defdata#;$d9=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$e9=bless({$v,$d9},$z);$f9={$c9,$e9};$g9=q#/lib/definition_defdata.b#;$h9=bless({$t5,$Z8,$q6,$q,$r6,$q,$s6,$f9,$J,$g9},$B6);$i9=[$A8,$K8,$R8,$Y8,$h9];$j9=bless({$t5,$s8,$J,$t8,$j6,$i9},$D6);$k9=[$j7,$s7,$h8,$r8,$j9];$l9=bless({$t5,$m6,$J,$n6,$j6,$k9},$d6);$m9={};$n9=q#new#;$o9=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$p9=bless({$v,$o9},$z);$q9={$n9,$p9};$r9=q#/lib/instantiable.b#;$s9=bless({$t5,$m9,$s6,$q9,$J,$r9},$B6);$t9={};$u9=q#child#;$v9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$w9=bless({$v,$v9},$z);$x9={$u9,$w9};$y9=q#/lib/subclass.b#;$z9=bless({$t5,$t9,$q6,$q,$r6,$q,$s6,$x9,$J,$y9},$B6);$A9=[$l9,$s9,$s7,$l9,$z9];$B9=bless({$t5,$i6,$J,$K,$j6,$A9},$v5);$C9=q#ni:/class.c#;$D9={$v5,1,$g6,1};$E9=q#/class.c#;$F9={$v5,1,$d6,1,$g6,1};$G9=q#/module.c#;$H9={$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$d6,1,$e6,1,$g6,1,$h6,1};$I9=q#/object.c#;$J9=[$B9];$K9=bless({$t5,$H9,$J,$I9,$j6,$J9},$k6);$L9={$v5,1,$L5,1,$M5,1,$N5,1,$U5,1,$V5,1,$d6,1,$g6,1};$M9=q#/lib/behavior.c#;$N9=[$K9];$O9=bless({$t5,$L9,$J,$M9,$j6,$N9},$k6);$P9=[$K9,$s9,$O9];$Q9=bless({$t5,$F9,$J,$G9,$j6,$P9},$k6);$R9=[$Q9];$S9=bless({$t5,$D9,$J,$E9,$j6,$R9},$k6);$T9=q#ni:/fabric/perl#;$U9={$t7,1};$V9={};$W9=[];$X9=q#my ($class, $stdin, $stdout) = @_;
+{stdin   => $stdin,
  stdout  => $stdout,
  pending => {}};#;$Y9=bless({$t,$W9,$v,$X9,$x,$y},$z);$Z9={$n7,$Y9};$ca=q#/fabric/perl_init.b#;$da=bless({$t5,$V9,$q6,$q,$r6,$q,$s6,$Z9,$J,$ca},$B6);$ea={};$fa=q#ni#;$ga=[];$ha=q#ni('/fabric/perl_proxy')->new(@_)#;$ia=bless({$t,$ga,$v,$ha,$x,$y},$z);$ja={$fa,$ia};$ka=q#/fabric/perl_rmi.b#;$la=bless({$t5,$ea,$q6,$q,$r6,$q,$s6,$ja,$J,$ka},$B6);$ma=[$h8,$da,$la];$na=bless({$t5,$U9,$J,$d1,$j6,$ma},$w5);$oa=q#ni:/fabric/perl.c#;$pa={$w5,1};$qa=q#/fabric/perl.c#;$ra=[$K9];$sa=bless({$t5,$pa,$J,$qa,$j6,$ra},$k6);$ta=q#ni:/fabric/perl_init.b#;$ua=q#ni:/fabric/perl_proxy#;$va={$u7,1};$wa=q#/fabric/perl_proxy#;$xa={};$ya=[];$za=q#my ($class, $perl, $name) = @_;
+{perl => $perl,
  name => $name};#;$Aa=bless({$t,$ya,$v,$za,$x,$y},$z);$Ba={$n7,$Aa};$Ca=q#/fabric/perl_proxy_init.b#;$Da=bless({$t5,$xa,$q6,$q,$r6,$q,$s6,$Ba,$J,$Ca},$B6);$Ea={};$Fa=q#AUTOLOAD#;$Ga=[];$Ha=q#my $self = shift;
my $method = ${__PACKAGE__ . '::AUTOLOAD'};
$$self{perl}->rmi($$self{name}, $method, @_);#;$Ia=bless({$t,$Ga,$v,$Ha,$x,$y},$z);$Ja={$Fa,$Ia};$Ka=q#/fabric/perl_proxy_rmi.b#;$La=bless({$t5,$Ea,$q6,$q,$r6,$q,$s6,$Ja,$J,$Ka},$B6);$Ma=[$h8,$Da,$La];$Na=bless({$t5,$va,$J,$wa,$j6,$Ma},$x5);$Oa=q#ni:/fabric/perl_proxy.c#;$Pa={$x5,1};$Qa=q#/fabric/perl_proxy.c#;$Ra=[$K9];$Sa=bless({$t5,$Pa,$J,$Qa,$j6,$Ra},$k6);$Ta=q#ni:/fabric/perl_proxy_init.b#;$Ua=q#ni:/fabric/perl_proxy_rmi.b#;$Va=q#ni:/fabric/perl_rmi.b#;$Wa=q#ni:/io/buffer#;$Xa={$v7,1};$Ya={$v7,1,$w7,1,$x7,1,$y7,1,$z7,1,$A7,1,$B7,1,$C7,1,$D7,1,$E7,1};$Za=q#/io/object#;$cb={};$db=q#(bool#;$eb=[];$fb=bless({$t,$eb,$v,1,$x,$y},$z);$gb={$db,$fb};$hb=q#/io/object_ops.b#;$ib=bless({$t5,$cb,$q6,$q,$r6,$q,$s6,$gb,$J,$hb},$B6);$jb={};$kb=q#die#;$lb=[];$mb=q#shift; die join " ", @_#;$nb=bless({$t,$lb,$v,$mb,$x,$y},$z);$ob=q#io_check#;$pb=[];$qb=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$rb=bless({$t,$pb,$v,$qb,$x,$y},$z);$sb=q#io_check_defined#;$tb=[];$ub=q#shift->io_check(sub {defined shift}, @_)#;$vb=bless({$t,$tb,$v,$ub,$x,$y},$z);$wb=q#io_check_true#;$xb=[];$yb=q#shift->io_check(sub {shift}, @_)#;$zb=bless({$t,$xb,$v,$yb,$x,$y},$z);$Ab={$kb,$nb,$ob,$rb,$sb,$vb,$wb,$zb};$Bb=q#/io/object_checks.b#;$Cb=bless({$t5,$jb,$q6,$q,$r6,$q,$s6,$Ab,$J,$Bb},$B6);$Db={};$Eb=q#(+#;$Fb=[];$Gb=q#ni('ni:/io/cat')->new(@_[0, 1])#;$Hb=bless({$t,$Fb,$v,$Gb,$x,$y},$z);$Ib={$Eb,$Hb};$Jb=q#/io/object_constructors.b#;$Kb=bless({$t5,$Db,$q6,$q,$r6,$q,$s6,$Ib,$J,$Jb},$B6);$Lb={};$Mb=q#read_all#;$Nb=[];$Ob=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$Pb=bless({$t,$Nb,$v,$Ob,$x,$y},$z);$Qb=q#write_all#;$Rb=[];$Sb=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$Tb=bless({$t,$Rb,$v,$Sb,$x,$y},$z);$Ub={$Mb,$Pb,$Qb,$Tb};$Vb=q#/io/object_memory.b#;$Wb=bless({$t5,$Lb,$q6,$q,$r6,$q,$s6,$Ub,$J,$Vb},$B6);$Xb={};$Yb=q#connect_sync#;$Zb=[];$cc=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$dc=bless({$t,$Zb,$v,$cc,$x,$y},$z);$ec=q#into_sync#;$fc=[];$gc=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$hc=bless({$t,$fc,$v,$gc,$x,$y},$z);$ic={$Yb,$dc,$ec,$hc};$jc=q#/io/object_transfer_sync.b#;$kc=bless({$t5,$Xb,$q6,$q,$r6,$q,$s6,$ic,$J,$jc},$B6);$lc={};$mc=q#connect_async#;$nc=[];$oc=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$pc=bless({$t,$nc,$v,$oc,$x,$y},$z);$qc=q#into_async#;$rc=[];$sc=q#ni('ni:/io/transfer_async')->new(@_)->run#;$tc=bless({$t,$rc,$v,$sc,$x,$y},$z);$uc={$mc,$pc,$qc,$tc};$vc=q#/io/object_transfer_async.b#;$wc=bless({$t5,$lc,$q6,$q,$r6,$q,$s6,$uc,$J,$vc},$B6);$xc=[$h8,$ib,$Cb,$Kb,$Wb,$kc,$wc,$wc,$kc,$wc,$kc];$yc=bless({$t5,$Ya,$J,$Za,$j6,$xc},$F5);$zc={};$Ac=[];$Bc=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Cc=bless({$t,$Ac,$v,$Bc,$x,$y},$z);$Dc={$n7,$Cc};$Ec=q#/io/buffer_init.b#;$Fc=bless({$t5,$zc,$q6,$q,$r6,$q,$s6,$Dc,$J,$Ec},$B6);$Gc={};$Hc=q#read#;$Ic=[];$Jc=q#my $self = shift;
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
}#;$Kc=bless({$t,$Ic,$v,$Jc,$x,$y},$z);$Lc=q#read_capacity#;$Mc=[];$Nc=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Oc=bless({$t,$Mc,$v,$Nc,$x,$y},$z);$Pc=q#write#;$Qc=[];$Rc=q#my $self = shift;
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
}#;$Sc=bless({$t,$Qc,$v,$Rc,$x,$y},$z);$Tc=q#write_capacity#;$Uc=[];$Vc=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Wc=bless({$t,$Uc,$v,$Vc,$x,$y},$z);$Xc={$Hc,$Kc,$Lc,$Oc,$Pc,$Sc,$Tc,$Wc};$Yc=q#/io/buffer_io.b#;$Zc=bless({$t5,$Gc,$q6,$q,$r6,$q,$s6,$Xc,$J,$Yc},$B6);$cd=[$yc,$Fc,$Zc];$dd=bless({$t5,$Xa,$J,$w1,$j6,$cd},$y5);$ed=q#ni:/io/buffer.c#;$fd={$y5,1};$gd=q#/io/buffer.c#;$hd={$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1};$id=q#/io/object.c#;$jd={};$kd=q#def_transfer_method#;$ld=[];$md=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$nd=bless({$t,$ld,$v,$md,$x,$y},$z);$od={$kd,$nd};$pd=q#/io/object.c_transfer_def.b#;$qd=bless({$t5,$jd,$q6,$q,$r6,$q,$s6,$od,$J,$pd},$B6);$rd=[$K9,$qd];$sd=bless({$t5,$hd,$J,$id,$j6,$rd},$k6);$td=[$sd];$ud=bless({$t5,$fd,$J,$gd,$j6,$td},$k6);$vd=q#ni:/io/buffer_init.b#;$wd=q#ni:/io/buffer_io.b#;$xd=q#ni:/io/cat#;$yd={$w7,1};$zd={};$Ad=[];$Bd=q#shift; +{fs => [@_]}#;$Cd=bless({$t,$Ad,$v,$Bd,$x,$y},$z);$Dd={$n7,$Cd};$Ed=q#/io/cat_init.b#;$Fd=bless({$t5,$zd,$q6,$q,$r6,$q,$s6,$Dd,$J,$Ed},$B6);$Gd={};$Hd=[];$Id=q#my $fs = shift->{fs};
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
$total_read;#;$Jd=bless({$t,$Hd,$v,$Id,$x,$y},$z);$Kd={$Hc,$Jd};$Ld=q#/io/cat_read.b#;$Md=bless({$t5,$Gd,$q6,$q,$r6,$q,$s6,$Kd,$J,$Ld},$B6);$Nd=[$yc,$Fd,$Md];$Od=bless({$t5,$yd,$J,$J1,$j6,$Nd},$z5);$Pd=q#ni:/io/cat.c#;$Qd={$z5,1};$Rd=q#/io/cat.c#;$Sd=[$sd];$Td=bless({$t5,$Qd,$J,$Rd,$j6,$Sd},$k6);$Ud=q#ni:/io/cat_init.b#;$Vd=q#ni:/io/cat_read.b#;$Wd=q#ni:/io/exec#;$Xd={$x7,1};$Yd={};$Zd=q#argv#;$ce=[];$de=q#shift->{'argv'}#;$ee=bless({$t,$ce,$v,$de,$x,$y},$z);$fe={$Zd,$ee};$ge=q#/io/exec_ro.b#;$he=bless({$t5,$Yd,$q6,$q,$r6,$q,$s6,$fe,$J,$ge},$B6);$ie={};$je=[];$ke=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$le=bless({$t,$je,$v,$ke,$x,$y},$z);$me={$n7,$le};$ne=q#/io/exec_init.b#;$oe=bless({$t5,$ie,$q6,$q,$r6,$q,$s6,$me,$J,$ne},$B6);$pe={};$qe=q#connect#;$re=[];$se=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$te=bless({$t,$re,$v,$se,$x,$y},$z);$ue=q#in_pipe#;$ve=[];$we=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$xe=bless({$t,$ve,$v,$we,$x,$y},$z);$ye=q#out_pipe#;$ze=[];$Ae=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Be=bless({$t,$ze,$v,$Ae,$x,$y},$z);$Ce=q#setup_stdio#;$De=[];$Ee=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Fe=bless({$t,$De,$v,$Ee,$x,$y},$z);$Ge={$qe,$te,$ue,$xe,$ye,$Be,$Ce,$Fe};$He=q#/io/exec_io_setup.b#;$Ie=bless({$t5,$pe,$q6,$q,$r6,$q,$s6,$Ge,$J,$He},$B6);$Je={};$Ke=q#binds_fd#;$Le=[];$Me=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$Ne=bless({$t,$Le,$v,$Me,$x,$y},$z);$Oe=q#fd#;$Pe=[];$Qe=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Re=bless({$t,$Pe,$v,$Qe,$x,$y},$z);$Se=q#stderr#;$Te=[];$Ue=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Ve=bless({$t,$Te,$v,$Ue,$x,$y},$z);$We=q#stdin#;$Xe=[];$Ye=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Ze=bless({$t,$Xe,$v,$Ye,$x,$y},$z);$cf=q#stdout#;$df=[];$ef=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$ff=bless({$t,$df,$v,$ef,$x,$y},$z);$gf={$Ke,$Ne,$Oe,$Re,$Se,$Ve,$We,$Ze,$cf,$ff};$hf=q#/io/exec_io_accessors.b#;$if=bless({$t5,$Je,$q6,$q,$r6,$q,$s6,$gf,$J,$hf},$B6);$jf={};$kf=q#env#;$lf=[];$mf=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$nf=bless({$t,$lf,$v,$mf,$x,$y},$z);$of={$kf,$nf};$pf=q#/io/exec_env.b#;$qf=bless({$t5,$jf,$q6,$q,$r6,$q,$s6,$of,$J,$pf},$B6);$rf={};$sf=q#exec#;$tf=[];$uf=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$vf=bless({$t,$tf,$v,$uf,$x,$y},$z);$wf=q#fork#;$xf=[];$yf=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$zf=bless({$t,$xf,$v,$yf,$x,$y},$z);$Af=q#move_fds#;$Bf=[];$Cf=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Df=bless({$t,$Bf,$v,$Cf,$x,$y},$z);$Ef={$sf,$vf,$wf,$zf,$Af,$Df};$Ff=q#/io/exec_fork.b#;$Gf=bless({$t5,$rf,$q6,$q,$r6,$q,$s6,$Ef,$J,$Ff},$B6);$Hf=[$yc,$he,$oe,$Ie,$if,$qf,$Gf];$If=bless({$t5,$Xd,$J,$W1,$j6,$Hf},$A5);$Jf=q#ni:/io/exec.c#;$Kf={$A5,1};$Lf=q#/io/exec.c#;$Mf=[$sd];$Nf=bless({$t5,$Kf,$J,$Lf,$j6,$Mf},$k6);$Of=q#ni:/io/exec_env.b#;$Pf=q#ni:/io/exec_fork.b#;$Qf=q#ni:/io/exec_init.b#;$Rf=q#ni:/io/exec_io_accessors.b#;$Sf=q#ni:/io/exec_io_setup.b#;$Tf=q#ni:/io/exec_ro.b#;$Uf=q#ni:/io/fd#;$Vf={$y7,1};$Wf=q#read_fd_mask#;$Xf={};$Yf=[];$Zf=q#shift->{'fd'}#;$cg=bless({$t,$Yf,$v,$Zf,$x,$y},$z);$dg={$Oe,$cg};$eg=q#/io/fd_readers.b#;$fg=bless({$t5,$Xf,$q6,$q,$r6,$q,$s6,$dg,$J,$eg},$B6);$gg={};$hg=[];$ig=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$jg=bless({$t,$hg,$v,$ig,$x,$y},$z);$kg={$n7,$jg};$lg=q#/io/fd_init.b#;$mg=bless({$t5,$gg,$q6,$q,$r6,$q,$s6,$kg,$J,$lg},$B6);$ng={};$og=q#be#;$pg=[];$qg=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$rg=bless({$t,$pg,$v,$qg,$x,$y},$z);$sg={$og,$rg};$tg=q#/io/fd_shell.b#;$ug=bless({$t5,$ng,$q6,$q,$r6,$q,$s6,$sg,$J,$tg},$B6);$vg={};$wg=q#cloexec#;$xg=[];$yg=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$zg=bless({$t,$xg,$v,$yg,$x,$y},$z);$Ag=q#fcntl_flag#;$Bg=[];$Cg=q#my ($self, $flag, $value) = @_;
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
}#;$Dg=bless({$t,$Bg,$v,$Cg,$x,$y},$z);$Eg=q#nonblock#;$Fg=[];$Gg=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Hg=bless({$t,$Fg,$v,$Gg,$x,$y},$z);$Ig={$wg,$zg,$Ag,$Dg,$Eg,$Hg};$Jg=q#/io/fd_fcntl.b#;$Kg=bless({$t5,$vg,$q6,$q,$r6,$q,$s6,$Ig,$J,$Jg},$B6);$Lg={};$Mg=[];$Ng=q#shift->close#;$Og=bless({$t,$Mg,$v,$Ng,$x,$y},$z);$Pg=q#close#;$Qg=[];$Rg=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$Sg=bless({$t,$Qg,$v,$Rg,$x,$y},$z);$Tg={$Pg,$Sg};$Ug=q#/io/fd_gc.b#;$Vg=bless({$t5,$Lg,$q6,$q,$r6,$Og,$s6,$Tg,$J,$Ug},$B6);$Wg={};$Xg=q#close_perl_ios#;$Yg=[];$Zg=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$ch=bless({$t,$Yg,$v,$Zg,$x,$y},$z);$dh=[];$eh=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$fh=bless({$t,$dh,$v,$eh,$x,$y},$z);$gh=[];$hh=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$ih=bless({$t,$gh,$v,$hh,$x,$y},$z);$jh={$Xg,$ch,$Hc,$fh,$Pc,$ih};$kh=q#/io/fd_perlio.b#;$lh=bless({$t5,$Wg,$q6,$q,$r6,$q,$s6,$jh,$J,$kh},$B6);$mh=[$yc,$fg,$mg,$ug,$Kg,$Vg,$lh];$nh=q#write_fd_mask#;$oh=bless({$t5,$Vf,$J,$l2,$Wf,$y,$j6,$mh,$nh,$y},$B5);$ph=[];$qh=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$rh=bless({$t,$ph,$v,$qh,$x,$y},$z);$sh=q#ni:/io/fd.c#;$th={$B5,1};$uh=q#/io/fd.c#;$vh={};$wh=q#clear_fd#;$xh=[];$yh=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$zh=bless({$t,$xh,$v,$yh,$x,$y},$z);$Ah=q#read_fd#;$Bh=[];$Ch=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$Dh=bless({$t,$Bh,$v,$Ch,$x,$y},$z);$Eh=q#select#;$Fh=[];$Gh=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$Hh=bless({$t,$Fh,$v,$Gh,$x,$y},$z);$Ih=q#write_fd#;$Jh=[];$Kh=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$Lh=bless({$t,$Jh,$v,$Kh,$x,$y},$z);$Mh={$wh,$zh,$Ah,$Dh,$Eh,$Hh,$Ih,$Lh};$Nh=q#/io/fd.c_selector.b#;$Oh=bless({$t5,$vh,$q6,$rh,$r6,$q,$s6,$Mh,$J,$Nh},$B6);$Ph=[$sd,$Oh];$Qh=bless({$t5,$th,$J,$uh,$j6,$Ph},$k6);$Rh=q#ni:/io/fd.c_selector.b#;$Sh=q#ni:/io/fd_fcntl.b#;$Th=q#ni:/io/fd_gc.b#;$Uh=q#ni:/io/fd_init.b#;$Vh=q#ni:/io/fd_perlio.b#;$Wh=q#ni:/io/fd_readers.b#;$Xh=q#ni:/io/fd_shell.b#;$Yh=q#ni:/io/file#;$Zh={$z7,1};$ci={};$di=[];$ei=q#shift->{'name'}#;$fi=bless({$t,$di,$v,$ei,$x,$y},$z);$gi={$J,$fi};$hi=q#/io/file_readers.b#;$ii=bless({$t5,$ci,$q6,$q,$r6,$q,$s6,$gi,$J,$hi},$B6);$ji={};$ki=q#mode#;$li=[];$mi=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$ni=bless({$t,$li,$v,$mi,$x,$y},$z);$oi={$ki,$ni};$pi=q#/io/file_accessors.b#;$qi=bless({$t5,$ji,$q6,$q,$r6,$q,$s6,$oi,$J,$pi},$B6);$ri={};$si=[];$ti=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$ui=bless({$t,$si,$v,$ti,$x,$y},$z);$vi={$n7,$ui};$wi=q#/io/file_init.b#;$xi=bless({$t5,$ri,$q6,$q,$r6,$q,$s6,$vi,$J,$wi},$B6);$yi={};$zi=q#(-X#;$Ai=[];$Bi=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Ci=bless({$t,$Ai,$v,$Bi,$x,$y},$z);$Di=q#mv#;$Ei=[];$Fi=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Gi=bless({$t,$Ei,$v,$Fi,$x,$y},$z);$Hi=q#rm#;$Ii=[];$Ji=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Ki=bless({$t,$Ii,$v,$Ji,$x,$y},$z);$Li={$zi,$Ci,$Di,$Gi,$Hi,$Ki};$Mi=q#/io/file_fns.b#;$Ni=bless({$t5,$yi,$q6,$q,$r6,$q,$s6,$Li,$J,$Mi},$B6);$Oi={};$Pi=q#atomic_update#;$Qi=[];$Ri=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Si=bless({$t,$Qi,$v,$Ri,$x,$y},$z);$Ti={$Pi,$Si};$Ui=q#/io/file_update.b#;$Vi=bless({$t5,$Oi,$q6,$q,$r6,$q,$s6,$Ti,$J,$Ui},$B6);$Wi={};$Xi=[];$Yi=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Zi=bless({$t,$Xi,$v,$Yi,$x,$y},$z);$cj=q#r#;$dj=[];$ej=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$fj=bless({$t,$dj,$v,$ej,$x,$y},$z);$gj=[];$hj=q#shift->r->read(@_)#;$ij=bless({$t,$gj,$v,$hj,$x,$y},$z);$jj=q#w#;$kj=[];$lj=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$mj=bless({$t,$kj,$v,$lj,$x,$y},$z);$nj=[];$oj=q#shift->w->write(@_)#;$pj=bless({$t,$nj,$v,$oj,$x,$y},$z);$qj={$Pg,$Zi,$cj,$fj,$Hc,$ij,$jj,$mj,$Pc,$pj};$rj=q#/io/file_io.b#;$sj=bless({$t5,$Wi,$q6,$q,$r6,$q,$s6,$qj,$J,$rj},$B6);$tj=[$yc,$ii,$qi,$xi,$Ni,$Vi,$sj];$uj=bless({$t5,$Zh,$J,$H2,$j6,$tj},$C5);$vj=q#ni:/io/file.c#;$wj={$C5,1};$xj=q#/io/file.c#;$yj=[$sd];$zj=bless({$t5,$wj,$J,$xj,$j6,$yj},$k6);$Aj=q#ni:/io/file_accessors.b#;$Bj=q#ni:/io/file_fns.b#;$Cj=q#ni:/io/file_init.b#;$Dj=q#ni:/io/file_io.b#;$Ej=q#ni:/io/file_readers.b#;$Fj=q#ni:/io/file_update.b#;$Gj=q#ni:/io/file_update_fd#;$Hj={$A7,1};$Ij={};$Jj=[];$Kj=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Lj=bless({$t,$Jj,$v,$Kj,$x,$y},$z);$Mj={$n7,$Lj};$Nj=q#/io/file_update_fd_init.b#;$Oj=bless({$t5,$Ij,$q6,$q,$r6,$q,$s6,$Mj,$J,$Nj},$B6);$Pj={};$Qj=[];$Rj=bless({$t,$Qj,$v,$Ng,$x,$y},$z);$Sj=[];$Tj=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Uj=bless({$t,$Sj,$v,$Tj,$x,$y},$z);$Vj={$Pg,$Uj};$Wj=q#/io/file_update_fd_gc.b#;$Xj=bless({$t5,$Pj,$q6,$q,$r6,$Rj,$s6,$Vj,$J,$Wj},$B6);$Yj=[$yc,$fg,$Kg,$lh,$Oj,$Xj];$Zj=bless({$t5,$Hj,$J,$N2,$j6,$Yj},$D5);$ck=q#ni:/io/file_update_fd.c#;$dk={$D5,1};$ek=q#/io/file_update_fd.c#;$fk=[$sd];$gk=bless({$t5,$dk,$J,$ek,$j6,$fk},$k6);$hk=q#ni:/io/file_update_fd_gc.b#;$ik=q#ni:/io/file_update_fd_init.b#;$jk=q#ni:/io/named_io_fns.b#;$kk={};$lk=q#fcntl#;$mk=[];$nk=q#CORE::fcntl $_[0], $_[1], $_[2]#;$ok=bless({$t,$mk,$v,$nk,$x,$y},$z);$pk=[];$qk=q#CORE::fork#;$rk=bless({$t,$pk,$v,$qk,$x,$y},$z);$sk=q#open2#;$tk=[];$uk=q#CORE::open $_[0], $_[1]#;$vk=bless({$t,$tk,$v,$uk,$x,$y},$z);$wk=q#rename#;$xk=[];$yk=q#CORE::rename $_[0], $_[1]#;$zk=bless({$t,$xk,$v,$yk,$x,$y},$z);$Ak=q#unlink#;$Bk=[];$Ck=q#CORE::unlink @_#;$Dk=bless({$t,$Bk,$v,$Ck,$x,$y},$z);$Ek=q#waitpid#;$Fk=[];$Gk=q#CORE::waitpid $_[0], $_[1]#;$Hk=bless({$t,$Fk,$v,$Gk,$x,$y},$z);$Ik={$lk,$ok,$wf,$rk,$sk,$vk,$wk,$zk,$Ak,$Dk,$Ek,$Hk};$Jk=q#/io/named_io_fns.b#;$Kk=bless({$t5,$kk,$q6,$q,$r6,$q,$s6,$Ik,$J,$Jk},$B6);$Lk=q#main#;$Mk=q#ni:/io/null#;$Nk={$B7,1};$Ok=q#/io/null#;$Pk={};$Qk=[];$Rk=q#+{fd => undef}#;$Sk=bless({$t,$Qk,$v,$Rk,$x,$y},$z);$Tk={$n7,$Sk};$Uk=q#/io/null_init.b#;$Vk=bless({$t5,$Pk,$q6,$q,$r6,$q,$s6,$Tk,$J,$Uk},$B6);$Wk={};$Xk=[];$Yk=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Zk=bless({$t,$Xk,$v,$Yk,$x,$y},$z);$cl=[];$dl=q#shift->fd->read(@_)#;$el=bless({$t,$cl,$v,$dl,$x,$y},$z);$fl=[];$gl=q#shift->fd->write(@_)#;$hl=bless({$t,$fl,$v,$gl,$x,$y},$z);$il={$Oe,$Zk,$Hc,$el,$Pc,$hl};$jl=q#/io/null_io.b#;$kl=bless({$t5,$Wk,$q6,$q,$r6,$q,$s6,$il,$J,$jl},$B6);$ll=[$yc,$Vk,$kl];$ml=bless({$t5,$Nk,$J,$Ok,$j6,$ll},$E5);$nl=q#ni:/io/null.c#;$ol={$E5,1};$pl=q#/io/null.c#;$ql=[$sd];$rl=bless({$t5,$ol,$J,$pl,$j6,$ql},$k6);$sl=q#ni:/io/null_init.b#;$tl=q#ni:/io/null_io.b#;$ul=q#ni:/io/object#;$vl=q#ni:/io/object.c#;$wl=q#ni:/io/object.c_transfer_def.b#;$xl=q#ni:/io/object_checks.b#;$yl=q#ni:/io/object_constructors.b#;$zl=q#ni:/io/object_memory.b#;$Al=q#ni:/io/object_ops.b#;$Bl=q#ni:/io/object_transfer_async.b#;$Cl=q#ni:/io/object_transfer_sync.b#;$Dl=q#ni:/io/pid#;$El={$D7,1};$Fl={};$Gl=q#pid#;$Hl=[];$Il=q#shift->{'pid'}#;$Jl=bless({$t,$Hl,$v,$Il,$x,$y},$z);$Kl=q#status#;$Ll=[];$Ml=q#shift->{'status'}#;$Nl=bless({$t,$Ll,$v,$Ml,$x,$y},$z);$Ol={$Gl,$Jl,$Kl,$Nl};$Pl=q#/io/pid_readers.b#;$Ql=bless({$t5,$Fl,$q6,$q,$r6,$q,$s6,$Ol,$J,$Pl},$B6);$Rl={};$Sl=[];$Tl=q#shift->await#;$Ul=bless({$t,$Sl,$v,$Tl,$x,$y},$z);$Vl=[];$Wl=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$Xl=bless({$t,$Vl,$v,$Wl,$x,$y},$z);$Yl={$n7,$Xl};$Zl=q#/io/pid_init.b#;$cm=bless({$t5,$Rl,$q6,$q,$r6,$Ul,$s6,$Yl,$J,$Zl},$B6);$dm={};$em=q#await#;$fm=[];$gm=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$hm=bless({$t,$fm,$v,$gm,$x,$y},$z);$im=q#running#;$jm=[];$km=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$lm=bless({$t,$jm,$v,$km,$x,$y},$z);$mm={$em,$hm,$im,$lm};$nm=q#/io/pid_wait.b#;$om=bless({$t5,$dm,$q6,$q,$r6,$q,$s6,$mm,$J,$nm},$B6);$pm={};$qm=[];$rm=q#shift->stdout->read(@_)#;$sm=bless({$t,$qm,$v,$rm,$x,$y},$z);$tm=[];$um=q#shift->stdin->write(@_)#;$vm=bless({$t,$tm,$v,$um,$x,$y},$z);$wm={$Hc,$sm,$Pc,$vm};$xm=q#/io/pid_io.b#;$ym=bless({$t5,$pm,$q6,$q,$r6,$q,$s6,$wm,$J,$xm},$B6);$zm={};$Am=[];$Bm=q#$_[0]->{external_fds}{$_[1]}#;$Cm=bless({$t,$Am,$v,$Bm,$x,$y},$z);$Dm=[];$Em=q#shift->fd(2)#;$Fm=bless({$t,$Dm,$v,$Em,$x,$y},$z);$Gm=[];$Hm=q#shift->fd(0)#;$Im=bless({$t,$Gm,$v,$Hm,$x,$y},$z);$Jm=[];$Km=q#shift->fd(1)#;$Lm=bless({$t,$Jm,$v,$Km,$x,$y},$z);$Mm={$Oe,$Cm,$Se,$Fm,$We,$Im,$cf,$Lm};$Nm=q#/io/pid_accessors.b#;$Om=bless({$t5,$zm,$q6,$q,$r6,$q,$s6,$Mm,$J,$Nm},$B6);$Pm=[$yc,$Ql,$cm,$om,$ym,$Om];$Qm=bless({$t5,$El,$J,$m3,$j6,$Pm},$G5);$Rm=q#ni:/io/pid.c#;$Sm={$G5,1};$Tm=q#/io/pid.c#;$Um=[$sd];$Vm=bless({$t5,$Sm,$J,$Tm,$j6,$Um},$k6);$Wm=q#ni:/io/pid_accessors.b#;$Xm=q#ni:/io/pid_init.b#;$Ym=q#ni:/io/pid_io.b#;$Zm=q#ni:/io/pid_readers.b#;$cn=q#ni:/io/pid_wait.b#;$dn=q#ni:/io/str#;$en={$E7,1};$fn=q#/io/str#;$gn={};$hn=q#data#;$in=[];$jn=q#shift->{'data'}#;$kn=bless({$t,$in,$v,$jn,$x,$y},$z);$ln=q#end#;$mn=[];$nn=q#shift->{'end'}#;$on=bless({$t,$mn,$v,$nn,$x,$y},$z);$pn=q#start#;$qn=[];$rn=q#shift->{'start'}#;$sn=bless({$t,$qn,$v,$rn,$x,$y},$z);$tn={$hn,$kn,$ln,$on,$pn,$sn};$un=q#/io/str_ro.b#;$vn=bless({$t5,$gn,$q6,$q,$r6,$q,$s6,$tn,$J,$un},$B6);$wn={};$xn=[];$yn=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$zn=bless({$t,$xn,$v,$yn,$x,$y},$z);$An={$n7,$zn};$Bn=q#/io/str_init.b#;$Cn=bless({$t5,$wn,$q6,$q,$r6,$q,$s6,$An,$J,$Bn},$B6);$Dn={};$En=[];$Fn=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Gn=bless({$t,$En,$v,$Fn,$x,$y},$z);$Hn=q#remaining#;$In=[];$Jn=q#my $self = shift; $$self{end} - $$self{start}#;$Kn=bless({$t,$In,$v,$Jn,$x,$y},$z);$Ln=[];$Mn=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Nn=bless({$t,$Ln,$v,$Mn,$x,$y},$z);$On={$Hc,$Gn,$Hn,$Kn,$Pc,$Nn};$Pn=q#/io/str_io.b#;$Qn=bless({$t5,$Dn,$q6,$q,$r6,$q,$s6,$On,$J,$Pn},$B6);$Rn=[$yc,$vn,$Cn,$Qn];$Sn=bless({$t5,$en,$J,$fn,$j6,$Rn},$H5);$Tn=q#ni:/io/str.c#;$Un={$H5,1};$Vn=q#/io/str.c#;$Wn=[$sd];$Xn=bless({$t5,$Un,$J,$Vn,$j6,$Wn},$k6);$Yn=q#ni:/io/str_init.b#;$Zn=q#ni:/io/str_io.b#;$co=q#ni:/io/str_ro.b#;$do=q#ni:/io/transfer#;$eo={$F7,1,$G7,1,$H7,1};$fo=q#/io/transfer#;$go={$F7,1,$G7,1,$H7,1,$S7,1};$ho=q#/semantic/task#;$io={};$jo=[];$ko=q#shift->{'outcome'}#;$lo=bless({$t,$jo,$v,$ko,$x,$y},$z);$mo={$r,$lo};$no=q#/semantic/task_ro.b#;$oo=bless({$t5,$io,$q6,$q,$r6,$q,$s6,$mo,$J,$no},$B6);$po={};$qo=q#failure#;$ro=[];$so=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$to=bless({$t,$ro,$v,$so,$x,$y},$z);$uo=q#success#;$vo=[];$wo=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$xo=bless({$t,$vo,$v,$wo,$x,$y},$z);$yo={$qo,$to,$uo,$xo};$zo=q#/semantic/task_outcome.b#;$Ao=bless({$t5,$po,$q6,$q,$r6,$q,$s6,$yo,$J,$zo},$B6);$Bo=[$h8,$oo,$Ao];$Co=bless({$t5,$go,$J,$ho,$j6,$Bo},$h6);$Do={};$Eo=[];$Fo=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Go=bless({$t,$Eo,$v,$Fo,$x,$y},$z);$Ho=[];$Io=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Jo=bless({$t,$Ho,$v,$Io,$x,$y},$z);$Ko=[];$Lo=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Mo=bless({$t,$Ko,$v,$Lo,$x,$y},$z);$No={$Hc,$Jo,$Pc,$Mo};$Oo=q#/io/transfer_io_interop.b#;$Po=bless({$t5,$Do,$q6,$Go,$r6,$q,$s6,$No,$J,$Oo},$B6);$Qo={};$Ro=q#pressure#;$So=[];$To=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Uo=bless({$t,$So,$v,$To,$x,$y},$z);$Vo=q#read_limit_throughput#;$Wo=[];$Xo=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Yo=bless({$t,$Wo,$v,$Xo,$x,$y},$z);$Zo=q#throughput#;$cp=[];$dp=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$ep=bless({$t,$cp,$v,$dp,$x,$y},$z);$fp=q#write_limit_throughput#;$gp=[];$hp=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$ip=bless({$t,$gp,$v,$hp,$x,$y},$z);$jp={$Ro,$Uo,$Vo,$Yo,$Zo,$ep,$fp,$ip};$kp=q#/io/transfer_io_measurement.b#;$lp=bless({$t5,$Qo,$q6,$q,$r6,$q,$s6,$jp,$J,$kp},$B6);$mp=[$Co,$Po,$lp];$np=bless({$t5,$eo,$J,$fo,$j6,$mp},$I5);$op=[];$pp=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$qp=bless({$t,$op,$v,$pp,$x,$y},$z);$rp=q#ni:/io/transfer.c#;$sp={$I5,1,$J5,1,$K5,1};$tp=q#/io/transfer.c#;$up={$I5,1,$J5,1,$K5,1,$h6,1};$vp=q#/semantic/task.c#;$wp=[$K9];$xp=bless({$t5,$up,$J,$vp,$j6,$wp},$k6);$yp={};$zp={};$Ap=q#/io/transfer.c_into.b#;$Bp=bless({$t5,$yp,$q6,$qp,$r6,$q,$s6,$zp,$J,$Ap},$B6);$Cp=[$xp,$Bp];$Dp=bless({$t5,$sp,$J,$tp,$j6,$Cp},$k6);$Ep=q#ni:/io/transfer.c_into.b#;$Fp=q#ni:/io/transfer_async#;$Gp={$G7,1};$Hp=q#/io/transfer_async#;$Ip={};$Jp=q#dest_io#;$Kp=[];$Lp=q#shift->{'dest_io'}#;$Mp=bless({$t,$Kp,$v,$Lp,$x,$y},$z);$Np=q#id#;$Op=[];$Pp=q#shift->{'id'}#;$Qp=bless({$t,$Op,$v,$Pp,$x,$y},$z);$Rp=q#source_io#;$Sp=[];$Tp=q#shift->{'source_io'}#;$Up=bless({$t,$Sp,$v,$Tp,$x,$y},$z);$Vp={$Jp,$Mp,$Np,$Qp,$Rp,$Up};$Wp=q#/io/transfer_async_ro.b#;$Xp=bless({$t5,$Ip,$q6,$q,$r6,$q,$s6,$Vp,$J,$Wp},$B6);$Yp={};$Zp=[];$cq=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$dq=bless({$t,$Zp,$v,$cq,$x,$y},$z);$eq={$n7,$dq};$fq=q#/io/transfer_async_init.b#;$gq=bless({$t5,$Yp,$q6,$q,$r6,$q,$s6,$eq,$J,$fq},$B6);$hq={};$iq=[];$jq=q#ni('ni:/io/transfer_async')->track(shift)#;$kq=bless({$t,$iq,$v,$jq,$x,$y},$z);$lq=[];$mq=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$nq=bless({$t,$lq,$v,$mq,$x,$y},$z);$oq={};$pq=q#/io/transfer_async_lifecycle.b#;$qq=bless({$t5,$hq,$q6,$kq,$r6,$nq,$s6,$oq,$J,$pq},$B6);$rq={};$sq=q#run#;$tq=[];$uq=q#shift#;$vq=bless({$t,$tq,$v,$uq,$x,$y},$z);$wq=q#run_async#;$xq=[];$yq=q#my $self = shift;
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

$self;#;$zq=bless({$t,$xq,$v,$yq,$x,$y},$z);$Aq={$sq,$vq,$wq,$zq};$Bq=q#/io/transfer_async_run.b#;$Cq=bless({$t5,$rq,$q6,$q,$r6,$q,$s6,$Aq,$J,$Bq},$B6);$Dq=[$np,$Xp,$gq,$qq,$Cq];$Eq=q#tracked_transfers#;$Fq={};$Gq=q#transfer_id#;$Hq=bless({$t5,$Gp,$J,$Hp,$j6,$Dq,$Eq,$Fq,$Gq,0},$J5);$Iq=[];$Jq=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Kq=bless({$t,$Iq,$v,$Jq,$x,$y},$z);$Lq=q#ni:/io/transfer_async.c#;$Mq={$J5,1};$Nq=q#/io/transfer_async.c#;$Oq={};$Pq=q#new_id#;$Qq=[];$Rq=q#++shift->{transfer_id}#;$Sq=bless({$t,$Qq,$v,$Rq,$x,$y},$z);$Tq=q#track#;$Uq=[];$Vq=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Wq=bless({$t,$Uq,$v,$Vq,$x,$y},$z);$Xq=q#untrack#;$Yq=[];$Zq=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$cr=bless({$t,$Yq,$v,$Zq,$x,$y},$z);$dr={$Pq,$Sq,$Tq,$Wq,$Xq,$cr};$er=q#/io/transfer_async.c_tracker.b#;$fr=bless({$t5,$Oq,$q6,$Kq,$r6,$q,$s6,$dr,$J,$er},$B6);$gr=[$Dp,$fr];$hr=bless({$t5,$Mq,$J,$Nq,$j6,$gr},$k6);$ir=q#ni:/io/transfer_async.c_tracker.b#;$jr=q#ni:/io/transfer_async_init.b#;$kr=q#ni:/io/transfer_async_lifecycle.b#;$lr=q#ni:/io/transfer_async_ro.b#;$mr=q#ni:/io/transfer_async_run.b#;$nr=q#ni:/io/transfer_io_interop.b#;$or=q#ni:/io/transfer_io_measurement.b#;$pr=q#ni:/io/transfer_sync#;$qr={$H7,1};$rr=q#/io/transfer_sync#;$sr={};$tr=[];$ur=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$vr=bless({$t,$tr,$v,$ur,$x,$y},$z);$wr={$n7,$vr};$xr=q#/io/transfer_sync_init.b#;$yr=bless({$t5,$sr,$q6,$q,$r6,$q,$s6,$wr,$J,$xr},$B6);$zr={};$Ar=[];$Br=q#my $self = shift;
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
$self->success;#;$Cr=bless({$t,$Ar,$v,$Br,$x,$y},$z);$Dr={$sq,$Cr};$Er=q#/io/transfer_sync_run.b#;$Fr=bless({$t5,$zr,$q6,$q,$r6,$q,$s6,$Dr,$J,$Er},$B6);$Gr=[$np,$yr,$Fr];$Hr=bless({$t5,$qr,$J,$rr,$j6,$Gr},$K5);$Ir=q#ni:/io/transfer_sync.c#;$Jr={$K5,1};$Kr=q#/io/transfer_sync.c#;$Lr=[$Dp];$Mr=bless({$t5,$Jr,$J,$Kr,$j6,$Lr},$k6);$Nr=q#ni:/io/transfer_sync_init.b#;$Or=q#ni:/io/transfer_sync_run.b#;$Pr=q#ni:/lib/accessor.b#;$Qr=q#ni:/lib/behavior#;$Rr=q#ni:/lib/behavior.c#;$Sr=q#ni:/lib/branch#;$Tr={$D6,1};$Ur=q#/lib/branch#;$Vr={};$Wr=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Xr=bless({$v,$Wr},$z);$Yr={$n7,$Xr};$Zr=q#/lib/branch_init.b#;$cs=bless({$t5,$Vr,$q6,$q,$r6,$q,$s6,$Yr,$J,$Zr},$B6);$ds=[$r8,$J6,$C6,$cs,$j9];$es=bless({$t5,$Tr,$J,$Ur,$j6,$ds},$M5);$fs=q#ni:/lib/branch.b#;$gs=q#ni:/lib/branch.c#;$hs={$M5,1};$is=q#/lib/branch.c#;$js=[$O9];$ks=bless({$t5,$hs,$J,$is,$j6,$js},$k6);$ls=q#ni:/lib/branch_init.b#;$ms=q#ni:/lib/class_init.b#;$ns=q#ni:/lib/dataslice#;$os={$J7,1};$ps=q#/lib/dataslice#;$qs={};$rs=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$ss=bless({$v,$rs},$z);$ts={$n7,$ss};$us=q#/lib/dataslice_init.b#;$vs=bless({$t5,$qs,$q6,$q,$r6,$q,$s6,$ts,$J,$us},$B6);$ws={};$xs=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$ys=bless({$v,$xs},$z);$zs={$w6,$ys};$As=q#/lib/dataslice_apply.b#;$Bs=bless({$t5,$ws,$q6,$q,$r6,$q,$s6,$zs,$J,$As},$B6);$Cs=[$r8,$vs,$Bs];$Ds=bless({$t5,$os,$J,$ps,$j6,$Cs},$N5);$Es=q#ni:/lib/dataslice.c#;$Fs={$N5,1};$Gs=q#/lib/dataslice.c#;$Hs=[$O9];$Is=bless({$t5,$Fs,$J,$Gs,$j6,$Hs},$k6);$Js=q#ni:/lib/dataslice_apply.b#;$Ks=q#ni:/lib/dataslice_init.b#;$Ls=q#ni:/lib/definition.b#;$Ms=q#ni:/lib/definition_def.b#;$Ns=q#ni:/lib/definition_defdata.b#;$Os=q#ni:/lib/doc#;$Ps={$L,1};$Qs={};$Rs=q#shift; +{name => shift, doc => []}#;$Ss=bless({$v,$Rs},$z);$Ts={$n7,$Ss};$Us=q#/lib/doc_init.b#;$Vs=bless({$t5,$Qs,$q6,$q,$r6,$q,$s6,$Ts,$J,$Us},$B6);$Ws={};$Xs=q#'ni.doc'#;$Ys=bless({$v,$Xs},$z);$Zs={$M6,$Ys};$ct=q#/lib/doc_namespace.b#;$dt=bless({$t5,$Ws,$q6,$q,$r6,$q,$s6,$Zs,$J,$ct},$B6);$et={};$ft=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map $self->fix_indentation($_), @_];
$self;#;$gt=bless({$v,$ft},$z);$ht=q#fix_indentation#;$it=q#my ($self, $x) = @_;
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
join "\\n", @lines;#;$jt=bless({$v,$it},$z);$kt={$Fa,$gt,$ht,$jt};$lt=q#/lib/doc_define.b#;$mt=bless({$t5,$et,$q6,$q,$r6,$q,$s6,$kt,$J,$lt},$B6);$nt={};$ot=q#shift->referent#;$pt=bless({$v,$ot},$z);$qt=q#referent#;$rt=q#ni 'ni:' . shift->{name}#;$st=bless({$v,$rt},$z);$tt={$ln,$pt,$qt,$st};$ut=q#/lib/doc_end.b#;$vt=bless({$t5,$nt,$q6,$q,$r6,$q,$s6,$tt,$J,$ut},$B6);$wt={};$xt=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$yt=bless({$v,$xt},$z);$zt=q#linearized#;$At=q#map @$_, @{shift->{doc}}#;$Bt=bless({$v,$At},$z);$Ct=q#tests#;$Dt=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$Et=bless({$v,$Dt},$z);$Ft={$Q2,$yt,$zt,$Bt,$Ct,$Et};$Gt=q#/lib/doc_test.b#;$Ht=bless({$t5,$wt,$q6,$q,$r6,$q,$s6,$Ft,$J,$Gt},$B6);$It=[$h8,$J6,$Vs,$dt,$mt,$vt,$Ht];$Jt=bless({$t5,$Ps,$J,$N3,$j6,$It},$O5);$Kt=q#ni:/lib/doc.c#;$Lt={$O5,1};$Mt=q#/lib/doc.c#;$Nt=[$K9];$Ot=bless({$t5,$Lt,$J,$Mt,$j6,$Nt},$k6);$Pt=q#ni:/lib/doc_define.b#;$Qt=q#ni:/lib/doc_end.b#;$Rt=q#ni:/lib/doc_init.b#;$St=q#ni:/lib/doc_namespace.b#;$Tt=q#ni:/lib/doc_test.b#;$Ut=q#ni:/lib/documentable.b#;$Vt=q#ni:/lib/fn#;$Wt={$z,1};$Xt=q#/lib/fn#;$Yt={};$Zt=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$cu=bless({$v,$Zt,$x,$y},$z);$du=q#compile#;$eu=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$fu=bless({$v,$eu},$z);$gu=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$hu=bless({$v,$gu,$x,$y},$z);$iu={$du,$fu,$n7,$hu};$ju=q#/lib/fn_init.b#;$ku=bless({$t5,$Yt,$q6,$q,$r6,$cu,$s6,$iu,$J,$ju},$B6);$lu={};$mu=[];$nu=q#shift->{'annotations'}#;$ou=bless({$t,$mu,$v,$nu,$x,$y},$z);$pu=[];$qu=q#shift->{'code'}#;$ru=bless({$t,$pu,$v,$qu,$x,$y},$z);$su=q#eval_number#;$tu=[];$uu=q#shift->{'eval_number'}#;$vu=bless({$t,$tu,$v,$uu,$x,$y},$z);$wu=q#fn#;$xu=[];$yu=q#shift->{'fn'}#;$zu=bless({$t,$xu,$v,$yu,$x,$y},$z);$Au={$t,$ou,$v,$ru,$su,$vu,$wu,$zu};$Bu=q#/lib/fn_ro.b#;$Cu=bless({$t5,$lu,$q6,$q,$r6,$q,$s6,$Au,$J,$Bu},$B6);$Du={};$Eu=[];$Fu=q#my $self = shift; "fn {$$self{code}}"#;$Gu=bless({$t,$Eu,$v,$Fu,$x,$y},$z);$Hu=[];$Iu=bless({$t,$Hu,$v,$U8,$x,$y},$z);$Ju={$M8,$Gu,$T8,$Iu};$Ku=q#/lib/fn_ops.b#;$Lu=bless({$t5,$Du,$q6,$q,$r6,$q,$s6,$Ju,$J,$Ku},$B6);$Mu={};$Nu=q#serialize#;$Ou=[];$Pu=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Qu=bless({$t,$Ou,$v,$Pu,$x,$y},$z);$Ru={$Nu,$Qu};$Su=q#/lib/fn_serialize.b#;$Tu=bless({$t5,$Mu,$q6,$q,$r6,$q,$s6,$Ru,$J,$Su},$B6);$Uu=[$h8,$s9,$ku,$Cu,$Lu,$Tu];$Vu=bless({$t5,$Wt,$J,$Xt,$j6,$Uu},$P5);$Wu=[];$Xu=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Yu=bless({$t,$Wu,$v,$Xu,$x,$y},$z);$Zu=q#ni:/lib/fn.c#;$cv={$P5,1};$dv=q#/lib/fn.c#;$ev={};$fv=q#resolve_evals#;$gv=[];$hv=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$iv=bless({$t,$gv,$v,$hv,$x,$y},$z);$jv={$fv,$iv};$kv=q#/lib/fn.c_resolve_eval.b#;$lv=bless({$t5,$ev,$q6,$Yu,$r6,$q,$s6,$jv,$J,$kv},$B6);$mv=[$K9,$lv];$nv=bless({$t5,$cv,$J,$dv,$j6,$mv},$k6);$ov=q#ni:/lib/fn.c_resolve_eval.b#;$pv=q#ni:/lib/fn_init.b#;$qv=q#ni:/lib/fn_ops.b#;$rv=q#ni:/lib/fn_ro.b#;$sv=q#ni:/lib/fn_serialize.b#;$tv=q#ni:/lib/future#;$uv={$K7,1};$vv={};$wv=[];$xv=bless({$t,$wv,$v,$ko,$x,$y},$z);$yv=q#parents#;$zv=[];$Av=q#shift->{'parents'}#;$Bv=bless({$t,$zv,$v,$Av,$x,$y},$z);$Cv=q#v#;$Dv=[];$Ev=q#shift->{'v'}#;$Fv=bless({$t,$Dv,$v,$Ev,$x,$y},$z);$Gv={$r,$xv,$yv,$Bv,$Cv,$Fv};$Hv=q#/lib/future_ro.b#;$Iv=bless({$t5,$vv,$q6,$q,$r6,$q,$s6,$Gv,$J,$Hv},$B6);$Jv={};$Kv=[];$Lv=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Mv=bless({$t,$Kv,$v,$Lv,$x,$y},$z);$Nv={$n7,$Mv};$Ov=q#/lib/future_init.b#;$Pv=bless({$t5,$Jv,$q6,$q,$r6,$q,$s6,$Nv,$J,$Ov},$B6);$Qv={};$Rv=q#decide#;$Sv=[];$Tv=q#local $_;
my ($self, $v) = @_;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Uv=bless({$t,$Sv,$v,$Tv,$x,$y},$z);$Vv=q#decided#;$Wv=[];$Xv=q#shift->{outcome}#;$Yv=bless({$t,$Wv,$v,$Xv,$x,$y},$z);$Zv=q#listener#;$cw=[];$dw=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$ew=bless({$t,$cw,$v,$dw,$x,$y},$z);$fw={$Rv,$Uv,$Vv,$Yv,$Zv,$ew};$gw=q#/lib/future_state.b#;$hw=bless({$t5,$Qv,$q6,$q,$r6,$q,$s6,$fw,$J,$gw},$B6);$iw={};$jw=q#and#;$kw=[];$lw=q#my $self   = $_[0];
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
$child;#;$mw=bless({$t,$kw,$v,$lw,$x,$y},$z);$nw=q#flatmap#;$ow=[];$pw=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$qw=bless({$t,$ow,$v,$pw,$x,$y},$z);$rw=q#map#;$sw=[];$tw=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$uw=bless({$t,$sw,$v,$tw,$x,$y},$z);$vw=q#or#;$ww=[];$xw=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$yw=bless({$t,$ww,$v,$xw,$x,$y},$z);$zw={$jw,$mw,$nw,$qw,$rw,$uw,$vw,$yw};$Aw=q#/lib/future_algebra.b#;$Bw=bless({$t5,$iw,$q6,$q,$r6,$q,$s6,$zw,$J,$Aw},$B6);$Cw=[$h8,$Iv,$Pv,$hw,$Bw];$Dw=bless({$t5,$uv,$J,$g4,$j6,$Cw},$Q5);$Ew=q#ni:/lib/future.c#;$Fw={$Q5,1};$Gw=q#/lib/future.c#;$Hw=[$K9];$Iw=bless({$t5,$Fw,$J,$Gw,$j6,$Hw},$k6);$Jw=q#ni:/lib/future_algebra.b#;$Kw=q#ni:/lib/future_init.b#;$Lw=q#ni:/lib/future_ro.b#;$Mw=q#ni:/lib/future_state.b#;$Nw=q#ni:/lib/gensym_generator_compact.b#;$Ow={};$Pw=q#gensym#;$Qw=[];$Rw=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Sw=bless({$t,$Qw,$v,$Rw,$x,$y},$z);$Tw={$Pw,$Sw};$Uw=q#/lib/gensym_generator_compact.b#;$Vw=bless({$t5,$Ow,$q6,$q,$r6,$q,$s6,$Tw,$J,$Uw},$B6);$Ww=q#ni:/lib/global_static_test.b#;$Xw={};$Yw=[];$Zw=q#ni('ni:/lib/test_case')->new(shift)#;$cx=q#($)#;$dx=bless({$t,$Yw,$v,$Zw,$x,$cx},$z);$ex=q#now#;$fx=[];$gx=q#ni('ni:/lib/test_value')->new(shift)#;$hx=bless({$t,$fx,$v,$gx,$x,$cx},$z);$ix={$Q2,$dx,$ex,$hx};$jx=q#/lib/global_static_test.b#;$kx=bless({$t5,$Xw,$q6,$q,$r6,$q,$s6,$ix,$J,$jx},$B6);$lx=q#ni:/lib/image#;$mx={$L7,1};$nx={};$ox=[];$px=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$qx=bless({$t,$ox,$v,$px,$x,$y},$z);$rx={$n7,$qx};$sx=q#/lib/image_init.b#;$tx=bless({$t5,$nx,$q6,$q,$r6,$q,$s6,$rx,$J,$sx},$B6);$ux={};$vx=q#boot_side_effect#;$wx=[];$xx=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$yx=bless({$t,$wx,$v,$xx,$x,$y},$z);$zx=q#finalizer#;$Ax=[];$Bx=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Cx=bless({$t,$Ax,$v,$Bx,$x,$y},$z);$Dx=q#io#;$Ex=[];$Fx=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Gx=bless({$t,$Ex,$v,$Fx,$x,$y},$z);$Hx=q#reconstruction#;$Ix=[];$Jx=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Kx=bless({$t,$Ix,$v,$Jx,$x,$y},$z);$Lx=q#side_effect#;$Mx=[];$Nx=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ox=bless({$t,$Mx,$v,$Nx,$x,$y},$z);$Px={$vx,$yx,$zx,$Cx,$Dx,$Gx,$Hx,$Kx,$Lx,$Ox};$Qx=q#/lib/image_quoting.b#;$Rx=bless({$t5,$ux,$q6,$q,$r6,$q,$s6,$Px,$J,$Qx},$B6);$Sx={};$Tx=q#quote_code#;$Ux=[];$Vx=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Wx=bless({$t,$Ux,$v,$Vx,$x,$y},$z);$Xx={$Tx,$Wx};$Yx=q#/lib/quote_code_fail.b#;$Zx=bless({$t5,$Sx,$q6,$q,$r6,$q,$s6,$Xx,$J,$Yx},$B6);$cy={};$dy=q#quote_array#;$ey=[];$fy=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$gy=bless({$t,$ey,$v,$fy,$x,$y},$z);$hy=q#quote_hash#;$iy=[];$jy=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$ky=bless({$t,$iy,$v,$jy,$x,$y},$z);$ly=q#quote_scalar#;$my=[];$ny=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$oy=bless({$t,$my,$v,$ny,$x,$y},$z);$py=q#quote_scalar_ref#;$qy=[];$ry=q#'\\\\' . shift->quote(${$_[0]})#;$sy=bless({$t,$qy,$v,$ry,$x,$y},$z);$ty=q#quote_value#;$uy=[];$vy=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$wy=bless({$t,$uy,$v,$vy,$x,$y},$z);$xy={$dy,$gy,$hy,$ky,$ly,$oy,$py,$sy,$ty,$wy};$yy=q#/lib/quote_values.b#;$zy=bless({$t5,$cy,$q6,$q,$r6,$q,$s6,$xy,$J,$yy},$B6);$Ay={};$By=q#quote_blessed#;$Cy=[];$Dy=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Ey=bless({$t,$Cy,$v,$Dy,$x,$y},$z);$Fy=q#quote_class#;$Gy=[];$Hy=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Iy=bless({$t,$Gy,$v,$Hy,$x,$y},$z);$Jy=q#quote_object#;$Ky=[];$Ly=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$My=bless({$t,$Ky,$v,$Ly,$x,$y},$z);$Ny={$By,$Ey,$Fy,$Iy,$Jy,$My};$Oy=q#/lib/quote_objects.b#;$Py=bless({$t5,$Ay,$q6,$q,$r6,$q,$s6,$Ny,$J,$Oy},$B6);$Qy={};$Ry=q#circular_arrayref#;$Sy=[];$Ty=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Uy=bless({$t,$Sy,$v,$Ty,$x,$y},$z);$Vy=q#circular_hashref#;$Wy=[];$Xy=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Yy=bless({$t,$Wy,$v,$Xy,$x,$y},$z);$Zy=q#is_circular#;$cz=[];$dz=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$ez=bless({$t,$cz,$v,$dz,$x,$y},$z);$fz={$Ry,$Uy,$Vy,$Yy,$Zy,$ez};$gz=q#/lib/quote_circular_addressed.b#;$hz=bless({$t5,$Qy,$q6,$q,$r6,$q,$s6,$fz,$J,$gz},$B6);$iz={};$jz=q#address#;$kz=[];$lz=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$mz=bless({$t,$kz,$v,$lz,$x,$y},$z);$nz=q#allocate_gensym#;$oz=[];$pz=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$qz=bless({$t,$oz,$v,$pz,$x,$y},$z);$rz=q#circular_links#;$sz=[];$tz=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$uz=bless({$t,$sz,$v,$tz,$x,$y},$z);$vz=q#quote#;$wz=[];$xz=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$yz=bless({$t,$wz,$v,$xz,$x,$y},$z);$zz={$jz,$mz,$nz,$qz,$rz,$uz,$vz,$yz};$Az=q#/lib/quote_gensym_identity.b#;$Bz=bless({$t5,$iz,$q6,$q,$r6,$q,$s6,$zz,$J,$Az},$B6);$Cz=[$h8,$tx,$Rx,$Zx,$zy,$Py,$hz,$Bz,$Vw];$Dz=bless({$t5,$mx,$J,$u4,$j6,$Cz},$R5);$Ez=q#ni:/lib/image.c#;$Fz={$R5,1};$Gz=q#/lib/image.c#;$Hz=[$K9];$Iz=bless({$t5,$Fz,$J,$Gz,$j6,$Hz},$k6);$Jz=q#ni:/lib/image_init.b#;$Kz=q#ni:/lib/image_quoting.b#;$Lz=q#ni:/lib/instance.b#;$Mz=q#ni:/lib/instantiable.b#;$Nz=q#ni:/lib/json.b#;$Oz={};$Pz=q#json_decode#;$Qz=[];$Rz=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Sz=bless({$t,$Qz,$v,$Rz,$x,$cx},$z);$Tz=q#json_encode#;$Uz=[];$Vz=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Wz=bless({$t,$Uz,$v,$Vz,$x,$cx},$z);$Xz=q#json_encode_pretty#;$Yz=[];$Zz=q#local $_;
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

$spaces . ni::json_encode($v);#;$cA=bless({$t,$Yz,$v,$Zz,$x,$y},$z);$dA=q#json_escape#;$eA=[];$fA=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$gA=bless({$t,$eA,$v,$fA,$x,$cx},$z);$hA=q#json_unescape#;$iA=[];$jA=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$kA=bless({$t,$iA,$v,$jA,$x,$cx},$z);$lA=q#json_unescape_one#;$mA=[];$nA=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$oA=bless({$t,$mA,$v,$nA,$x,$cx},$z);$pA={$Pz,$Sz,$Tz,$Wz,$Xz,$cA,$dA,$gA,$hA,$kA,$lA,$oA};$qA=q#/lib/json.b#;$rA=bless({$t5,$Oz,$q6,$q,$r6,$q,$s6,$pA,$J,$qA},$B6);$sA=q#ni:/lib/name_as_string.b#;$tA=q#ni:/lib/named.b#;$uA=q#ni:/lib/named_in_ni.b#;$vA=q#ni:/lib/namespaced.b#;$wA=q#ni:/lib/ni#;$xA={$M7,1};$yA={};$zA=q#extend#;$AA=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$BA=bless({$v,$AA,$x,$y},$z);$CA=q#is_mutable#;$DA=q#$0 ne '-' && -w $0#;$EA=bless({$v,$DA,$x,$y},$z);$FA=q#modify#;$GA=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$HA=bless({$v,$GA,$x,$y},$z);$IA={$zA,$BA,$CA,$EA,$FA,$HA};$JA=q#/lib/ni_self.b#;$KA=bless({$t5,$yA,$q6,$q,$r6,$q,$s6,$IA,$J,$JA},$B6);$LA={};$MA=q#--internal/+=#;$NA=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$OA=bless({$v,$NA,$x,$y},$z);$PA=q#--internal/eval#;$QA=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$RA=bless({$v,$QA,$x,$y},$z);$SA=q#--internal/image#;$TA=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$UA=bless({$v,$TA,$x,$y},$z);$VA=q#--internal/test#;$WA=q#local $| = 1;
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
!!$failed;#;$XA=bless({$v,$WA,$x,$y},$z);$YA=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$ZA=bless({$v,$YA,$x,$y},$z);$cB={$MA,$OA,$PA,$RA,$SA,$UA,$VA,$XA,$sq,$ZA};$dB=q#/lib/ni_main.b#;$eB=bless({$t5,$LA,$q6,$q,$r6,$q,$s6,$cB,$J,$dB},$B6);$fB={};$gB=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$hB=bless({$v,$gB,$x,$y},$z);$iB=q#resolver_for#;$jB=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$kB=bless({$v,$jB,$x,$y},$z);$lB={$c7,$hB,$iB,$kB};$mB=q#/lib/ni_resolver.b#;$nB=bless({$t5,$fB,$q6,$q,$r6,$q,$s6,$lB,$J,$mB},$B6);$oB={};$pB=q#exists#;$qB=q#exists $_[0]->{named}{$_[1]}#;$rB=bless({$v,$qB,$x,$y},$z);$sB=q#quoted#;$tB=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$uB=bless({$v,$tB,$x,$y},$z);$vB={$pB,$rB,$sB,$uB};$wB=q#/lib/ni_image.b#;$xB=bless({$t5,$oB,$q6,$q,$r6,$q,$s6,$vB,$J,$wB},$B6);$yB=[$h8,$KA,$eB,$nB,$xB];$zB=bless({$t5,$xA,$J,$C4,$j6,$yB},$S5);$AB=q#ni:/lib/ni.c#;$BB={$S5,1};$CB=q#/lib/ni.c#;$DB=[$K9];$EB=bless({$t5,$BB,$J,$CB,$j6,$DB},$k6);$FB=q#ni:/lib/ni_image.b#;$GB=q#ni:/lib/ni_main.b#;$HB=q#ni:/lib/ni_resolver.b#;$IB=q#ni:/lib/ni_self.b#;$JB=q#ni:/lib/ni_static_util.b#;$KB={};$LB=q#abbrev#;$MB=[];$NB=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$OB=bless({$t,$MB,$v,$NB,$x,$y},$z);$PB=q#dor#;$QB=[];$RB=q#defined $_[0] ? $_[0] : $_[1]#;$SB=bless({$t,$QB,$v,$RB,$x,$y},$z);$TB=q#indent#;$UB=[];$VB=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$WB=bless({$t,$UB,$v,$VB,$x,$y},$z);$XB=q#max#;$YB=[];$ZB=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$cC=bless({$t,$YB,$v,$ZB,$x,$y},$z);$dC=q#maxstr#;$eC=[];$fC=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$gC=bless({$t,$eC,$v,$fC,$x,$y},$z);$hC=q#mean#;$iC=[];$jC=q#sum(@_) / (@_ || 1)#;$kC=bless({$t,$iC,$v,$jC,$x,$y},$z);$lC=q#min#;$mC=[];$nC=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$oC=bless({$t,$mC,$v,$nC,$x,$y},$z);$pC=q#minstr#;$qC=[];$rC=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$sC=bless({$t,$qC,$v,$rC,$x,$y},$z);$tC=q#sgr#;$uC=[];$vC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$wC=bless({$t,$uC,$v,$vC,$x,$y},$z);$xC=q#sr#;$yC=[];$zC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$AC=bless({$t,$yC,$v,$zC,$x,$y},$z);$BC=q#sum#;$CC=[];$DC=q#local $_; my $x = 0; $x += $_ for @_; $x#;$EC=bless({$t,$CC,$v,$DC,$x,$y},$z);$FC=q#swap#;$GC=[];$HC=q#@_[0, 1] = @_[1, 0]#;$IC=bless({$t,$GC,$v,$HC,$x,$y},$z);$JC={$LB,$OB,$PB,$SB,$TB,$WB,$XB,$cC,$dC,$gC,$hC,$kC,$lC,$oC,$pC,$sC,$tC,$wC,$xC,$AC,$BC,$EC,$FC,$IC};$KC=q#/lib/ni_static_util.b#;$LC=bless({$t5,$KB,$q6,$q,$r6,$q,$s6,$JC,$J,$KC},$B6);$MC=q#ni:/lib/perlbranch.b#;$NC=q#ni:/lib/quote_circular_addressed.b#;$OC=q#ni:/lib/quote_code_fail.b#;$PC=q#ni:/lib/quote_gensym_identity.b#;$QC=q#ni:/lib/quote_objects.b#;$RC=q#ni:/lib/quote_simple#;$SC={$N7,1};$TC={};$UC=[];$VC=q#+{}#;$WC=bless({$t,$UC,$v,$VC,$x,$y},$z);$XC={$n7,$WC};$YC=q#/lib/quote_simple_init.b#;$ZC=bless({$t5,$TC,$q6,$q,$r6,$q,$s6,$XC,$J,$YC},$B6);$cD={};$dD=[];$eD=bless({$t,$dD,$v,0,$x,$y},$z);$fD=[];$gD=q#shift->quote_value(shift)#;$hD=bless({$t,$fD,$v,$gD,$x,$y},$z);$iD={$Zy,$eD,$vz,$hD};$jD=q#/lib/quote_simple_quote.b#;$kD=bless({$t5,$cD,$q6,$q,$r6,$q,$s6,$iD,$J,$jD},$B6);$lD=[$h8,$ZC,$kD,$Zx,$zy,$Py];$mD=bless({$t5,$SC,$J,$N4,$j6,$lD},$T5);$nD=q#ni:/lib/quote_simple.c#;$oD={$T5,1};$pD=q#/lib/quote_simple.c#;$qD=[$K9];$rD=bless({$t5,$oD,$J,$pD,$j6,$qD},$k6);$sD=q#ni:/lib/quote_simple_init.b#;$tD=q#ni:/lib/quote_simple_quote.b#;$uD=q#ni:/lib/quote_values.b#;$vD=q#ni:/lib/ref_eq.b#;$wD=q#ni:/lib/resolver.b#;$xD=q#ni:/lib/slice#;$yD={$B6,1};$zD=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$AD=bless({$v,$zD},$z);$BD=q#local $_;
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
$self;#;$CD=bless({$v,$BD},$z);$DD=q#lib/slice::apply#;$ED=q#lib/slice::apply_#;$FD={};$GD=q#apply_#;$HD={$w6,$AD,$GD,$CD};$ID=q#/lib/slice.b#;$JD=bless({$t5,$FD,$s6,$HD,$J,$ID},$B6);$KD={};$LD=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$MD=bless({$v,$LD,$x,$y},$z);$ND={$n7,$MD};$OD=q#/lib/slice_init.b#;$PD=bless({$t5,$KD,$s6,$ND,$J,$OD},$B6);$QD={};$RD=[];$SD=q#local $_;
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
$g;#;$TD=bless({$t,$RD,$v,$SD,$x,$y},$z);$UD={$Nu,$TD};$VD=q#/lib/slice_serialize.b#;$WD=bless({$t5,$QD,$q6,$q,$r6,$q,$s6,$UD,$J,$VD},$B6);$XD=[$r8,$J6,$JD,$PD,$WD];$YD=bless({$t5,$yD,$J,$k5,$j6,$XD},$U5);$ZD=q#ni:/lib/slice.b#;$cE=q#ni:/lib/slice.c#;$dE={$U5,1};$eE=q#/lib/slice.c#;$fE=[$O9];$gE=bless({$t5,$dE,$J,$eE,$j6,$fE},$k6);$hE=q#ni:/lib/slice_init.b#;$iE=q#ni:/lib/slice_serialize.b#;$jE=q#ni:/lib/static_fn.b#;$kE={};$lE=[];$mE=q#ni('ni:/lib/fn')->new(@_)#;$nE=bless({$t,$lE,$v,$mE,$x,$cx},$z);$oE=q#fp#;$pE=[];$qE=q#($$)#;$rE=bless({$t,$pE,$v,$mE,$x,$qE},$z);$sE={$wu,$nE,$oE,$rE};$tE=q#/lib/static_fn.b#;$uE=bless({$t5,$kE,$q6,$q,$r6,$q,$s6,$sE,$J,$tE},$B6);$vE=q#ni:/lib/subclass.b#;$wE=q#ni:/lib/tag#;$xE={$K6,1};$yE=q#/lib/tag#;$zE={};$AE=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$BE=bless({$v,$AE,$x,$y},$z);$CE={$w6,$BE};$DE=q#/lib/tag.b#;$EE=bless({$t5,$zE,$q6,$q,$r6,$q,$s6,$CE,$J,$DE},$B6);$FE={};$GE=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$HE=bless({$v,$GE,$x,$y},$z);$IE={$n7,$HE};$JE=q#/lib/tag_init.b#;$KE=bless({$t5,$FE,$q6,$q,$r6,$q,$s6,$IE,$J,$JE},$B6);$LE=[$r8,$J6,$EE,$KE];$ME=bless({$t5,$xE,$J,$yE,$j6,$LE},$V5);$NE=q#ni:/lib/tag.b#;$OE=q#ni:/lib/tag.c#;$PE={$V5,1};$QE=q#/lib/tag.c#;$RE=[$O9];$SE=bless({$t5,$PE,$J,$QE,$j6,$RE},$k6);$TE=q#ni:/lib/tag_init.b#;$UE=q#ni:/lib/test_assert_eq#;$VE={$O7,1};$WE=q#/lib/test_assert_eq#;$XE={$O7,1,$P7,1};$YE=q#/lib/test_assertion#;$ZE={};$cF=q#commit#;$dF=[];$eF=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$fF=bless({$t,$dF,$v,$eF,$x,$y},$z);$gF={$cF,$fF};$hF=q#/lib/test_assertion_commit.b#;$iF=bless({$t5,$ZE,$q6,$q,$r6,$q,$s6,$gF,$J,$hF},$B6);$jF=[$h8,$iF];$kF=bless({$t5,$XE,$J,$YE,$j6,$jF},$X5);$lF={};$mF=[];$nF=q#my ($class, $diff) = @_;
+{diff => $diff};#;$oF=bless({$t,$mF,$v,$nF,$x,$y},$z);$pF={$n7,$oF};$qF=q#/lib/test_assert_eq_init.b#;$rF=bless({$t5,$lF,$q6,$q,$r6,$q,$s6,$pF,$J,$qF},$B6);$sF={};$tF=[];$uF=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode_pretty $$self{diff}
              : "PASS";#;$vF=bless({$t,$tF,$v,$uF,$x,$y},$z);$wF=q#failed#;$xF=[];$yF=q#defined shift->{diff}#;$zF=bless({$t,$xF,$v,$yF,$x,$y},$z);$AF=q#result#;$BF=[];$CF=bless({$t,$BF,$v,$uq,$x,$y},$z);$DF={$M8,$vF,$wF,$zF,$AF,$CF};$EF=q#/lib/test_assert_eq_result.b#;$FF=bless({$t5,$sF,$q6,$q,$r6,$q,$s6,$DF,$J,$EF},$B6);$GF=[$kF,$rF,$FF];$HF=bless({$t5,$VE,$J,$WE,$j6,$GF},$W5);$IF=q#ni:/lib/test_assert_eq.c#;$JF={$W5,1};$KF=q#/lib/test_assert_eq.c#;$LF={$W5,1,$X5,1};$MF=q#/lib/test_assertion.c#;$NF=[$K9];$OF=bless({$t5,$LF,$J,$MF,$j6,$NF},$k6);$PF=[$OF];$QF=bless({$t5,$JF,$J,$KF,$j6,$PF},$k6);$RF=q#ni:/lib/test_assert_eq_init.b#;$SF=q#ni:/lib/test_assert_eq_result.b#;$TF=q#ni:/lib/test_assertion#;$UF=q#ni:/lib/test_assertion.c#;$VF=q#ni:/lib/test_assertion_commit.b#;$WF=q#ni:/lib/test_case#;$XF={$B,1};$YF=q#/lib/test_case#;$ZF=q#running_test#;$cG={};$dG=[];$eG=q#shift->{'assertions'}#;$fG=bless({$t,$dG,$v,$eG,$x,$y},$z);$gG=[];$hG=q#shift->{'test'}#;$iG=bless({$t,$gG,$v,$hG,$x,$y},$z);$jG={$n,$fG,$s,$iG};$kG=q#/lib/test_case_ro.b#;$lG=bless({$t5,$cG,$q6,$q,$r6,$q,$s6,$jG,$J,$kG},$B6);$mG={};$nG=[];$oG=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$pG=bless({$t,$nG,$v,$oG,$x,$y},$z);$qG={$p,$pG};$rG=q#/lib/test_case_rw.b#;$sG=bless({$t5,$mG,$q6,$q,$r6,$q,$s6,$qG,$J,$rG},$B6);$tG={};$uG=[];$vG=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$wG=bless({$t,$uG,$v,$vG,$x,$y},$z);$xG={$n7,$wG};$yG=q#/lib/test_case_init.b#;$zG=bless({$t5,$tG,$q6,$q,$r6,$q,$s6,$xG,$J,$yG},$B6);$AG={};$BG=[];$CG=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$DG=bless({$t,$BG,$v,$CG,$x,$y},$z);$EG=[];$FG=q#!shift->{outcome}->[0]#;$GG=bless({$t,$EG,$v,$FG,$x,$y},$z);$HG={$M8,$DG,$wF,$GG};$IG=q#/lib/test_case_metrics.b#;$JG=bless({$t5,$AG,$q6,$q,$r6,$q,$s6,$HG,$J,$IG},$B6);$KG={};$LG=q#done#;$MG=[];$NG=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$OG=bless({$t,$MG,$v,$NG,$x,$y},$z);$PG=[];$QG=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$RG=bless({$t,$PG,$v,$QG,$x,$y},$z);$SG={$LG,$OG,$sq,$RG};$TG=q#/lib/test_case_run.b#;$UG=bless({$t5,$KG,$q6,$q,$r6,$q,$s6,$SG,$J,$TG},$B6);$VG=[$h8,$lG,$sG,$zG,$JG,$UG];$WG=bless({$t5,$XF,$J,$YF,$ZF,$q,$j6,$VG},$Y5);$XG=[];$YG=q#shift->{running_test} = undef#;$ZG=bless({$t,$XG,$v,$YG,$x,$y},$z);$cH=q#ni:/lib/test_case.c#;$dH={$Y5,1};$eH=q#/lib/test_case.c#;$fH={};$gH=[];$hH=q#shift->{'running_test'}#;$iH=bless({$t,$gH,$v,$hH,$x,$y},$z);$jH={$ZF,$iH};$kH=q#/lib/test_case.c_test_ro.b#;$lH=bless({$t5,$fH,$q6,$q,$r6,$q,$s6,$jH,$J,$kH},$B6);$mH={};$nH=q#with_test#;$oH=[];$pH=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$qH=bless({$t,$oH,$v,$pH,$x,$y},$z);$rH={$nH,$qH};$sH=q#/lib/test_case.c_test.b#;$tH=bless({$t5,$mH,$q6,$ZG,$r6,$q,$s6,$rH,$J,$sH},$B6);$uH=[$K9,$lH,$tH];$vH=bless({$t5,$dH,$J,$eH,$j6,$uH},$k6);$wH=q#ni:/lib/test_case.c_test.b#;$xH=q#ni:/lib/test_case.c_test_ro.b#;$yH=q#ni:/lib/test_case_init.b#;$zH=q#ni:/lib/test_case_metrics.b#;$AH=q#ni:/lib/test_case_ro.b#;$BH=q#ni:/lib/test_case_run.b#;$CH=q#ni:/lib/test_case_rw.b#;$DH=q#ni:/lib/test_value#;$EH={$Q7,1};$FH=q#/lib/test_value#;$GH={};$HH=[];$IH=q#\\$_[1]#;$JH=bless({$t,$HH,$v,$IH,$x,$y},$z);$KH={$n7,$JH};$LH=q#/lib/test_value_init.b#;$MH=bless({$t5,$GH,$q6,$q,$r6,$q,$s6,$KH,$J,$LH},$B6);$NH={};$OH=q#(==#;$PH=[];$QH=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$RH=bless({$t,$PH,$v,$QH,$x,$y},$z);$SH=q#detailed_scalar_diff#;$TH=[];$UH=q#local $_;
my ($self, $lhs, $rhs, $lpos, $rpos) = @_;
my ($prefix) = ($lhs ^ $rhs) =~ /^(\\0*)/;

my $l = length $prefix;
return $self->detailed_scalar_diff(substr($lhs, $l), substr($rhs, $l),
                                   $lpos + $l,       $rpos + $l)
  if $l;

my $minlength = ni::min(length $lhs, length $rhs);
my ($suffix) = (  substr($lhs, length($lhs) - $minlength)
                ^ substr($rhs, length($rhs) - $minlength)) =~ /(\\0*)$/;
return $self->detailed_scalar_diff(substr($lhs, 0, length($lhs) - $l),
                                   substr($rhs, 0, length($rhs) - $l),
                                   $lpos, $rpos)
  if $l = length $suffix;

my $d = length($rhs) - length($lhs);
my $best_offset = 0;
my $best_match  = 0;
for (0..abs $d) {
  my $diff = substr($d < 0 ? $lhs : $rhs, $_, $minlength)
           ^ ($d < 0 ? $rhs : $lhs);
  my @matching = $diff =~ /\\0\\0\\0\\0\\0\\0\\0\\0+/g;
  my $match = ni::sum(map length, @matching);
  if ($match > $best_match) {
    $best_offset = $_;
    $best_match  = $match;
  }
}

\# TODO: rewrite this

return [] unless $lhs ne $rhs;
+{lpos  => $lpos,
  rpos  => $rpos,
  ltext => $lhs,
  rtext => $rhs}#;$VH=bless({$t,$TH,$v,$UH,$x,$y},$z);$WH=q#diff#;$XH=[];$YH=q#my ($self, $rhs) = @_;
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
return undef;#;$ZH=bless({$t,$XH,$v,$YH,$x,$y},$z);$cI={$OH,$RH,$SH,$VH,$WH,$ZH};$dI=q#/lib/test_value_eq.b#;$eI=bless({$t5,$NH,$q6,$q,$r6,$q,$s6,$cI,$J,$dI},$B6);$fI={};$gI=[];$hI=q#ni::json_encode ${$_[0]}#;$iI=bless({$t,$gI,$v,$hI,$x,$y},$z);$jI={$M8,$iI};$kI=q#/lib/test_value_str.b#;$lI=bless({$t5,$fI,$q6,$q,$r6,$q,$s6,$jI,$J,$kI},$B6);$mI=[$h8,$MH,$eI,$lI];$nI=bless({$t5,$EH,$J,$FH,$j6,$mI},$Z5);$oI=q#ni:/lib/test_value.c#;$pI={$Z5,1};$qI=q#/lib/test_value.c#;$rI=[$K9];$sI=bless({$t5,$pI,$J,$qI,$j6,$rI},$k6);$tI=q#ni:/lib/test_value_eq.b#;$uI=q#ni:/lib/test_value_init.b#;$vI=q#ni:/lib/test_value_str.b#;$wI=q#ni:/metaclass#;$xI={$k6,1};$yI=q#/metaclass#;$zI=[$j7,$s9,$s7,$l9];$AI=bless({$t5,$xI,$J,$yI,$j6,$zI},$c6);$BI=q#ni:/metaclass.c#;$CI={$c6,1};$DI=q#/metaclass.c#;$EI=[$B9];$FI=bless({$t5,$CI,$J,$DI,$j6,$EI},$k6);$GI=q#ni:/module#;$HI=q#ni:/module.c#;$II=q#ni:/object#;$JI=q#ni:/object.c#;$KI=q#ni:/semantic/dimension#;$LI={$f6,1};$MI=q#/semantic/dimension#;$NI=[$B9];$OI=bless({$t5,$LI,$J,$MI,$j6,$NI},$g6);$PI=q#ni:/semantic/dimension.c#;$QI={$g6,1};$RI=q#/semantic/dimension.c#;$SI=[$S9];$TI=bless({$t5,$QI,$J,$RI,$j6,$SI},$k6);$UI=q#ni:/semantic/task#;$VI=q#ni:/semantic/task.c#;$WI=q#ni:/semantic/task_outcome.b#;$XI=q#ni:/semantic/task_ro.b#;$YI=q#ni:main#;$ZI={$Lk,1};$cJ=[$uE,$kx,$Kk];$dJ=bless({$t5,$ZI,$J,$Lk,$j6,$cJ},$l6);$eJ=q#ni:ni#;$fJ={$fa,1};$gJ={$fa,1};$hJ=q#json_escapes#;$iJ=q##;$jJ=q#b#;$kJ=q#	#;$lJ=q#t#;$mJ=q#
#;$nJ=q#n#;$oJ=q##;$pJ=q#"#;$qJ=q#/#;$rJ=q#\\#;$sJ={$iJ,$jJ,$kJ,$lJ,$mJ,$nJ,$oJ,$cj,$pJ,$pJ,$qJ,$qJ,$rJ,$rJ};$tJ=q#json_unescapes#;$uJ={$pJ,$pJ,$qJ,$qJ,$rJ,$rJ,$jJ,$iJ,$nJ,$mJ,$cj,$oJ,$lJ,$kJ};$vJ={$hJ,$sJ,$tJ,$uJ};$wJ=q#/lib/json_data.b#;$xJ=bless({$t5,$gJ,$hn,$vJ,$J,$wJ},$J7);$yJ=[$xJ,$rA,$LC];$zJ=bless({$t5,$fJ,$J,$fa,$j6,$yJ},$l6);$AJ={$d,$M,$P,$U,$V,$e1,$f1,$k1,$l1,$x1,$y1,$K1,$L1,$X1,$Y1,$m2,$n2,$I2,$J2,$O2,$P2,$n3,$o3,$u3,$v3,$O3,$P3,$h4,$i4,$v4,$w4,$D4,$E4,$O4,$P4,$l5,$m5,$r5,$s5,$B9,$C9,$S9,$T9,$na,$oa,$sa,$ta,$da,$ua,$Na,$Oa,$Sa,$Ta,$Da,$Ua,$La,$Va,$la,$Wa,$dd,$ed,$ud,$vd,$Fc,$wd,$Zc,$xd,$Od,$Pd,$Td,$Ud,$Fd,$Vd,$Md,$Wd,$If,$Jf,$Nf,$Of,$qf,$Pf,$Gf,$Qf,$oe,$Rf,$if,$Sf,$Ie,$Tf,$he,$Uf,$oh,$sh,$Qh,$Rh,$Oh,$Sh,$Kg,$Th,$Vg,$Uh,$mg,$Vh,$lh,$Wh,$fg,$Xh,$ug,$Yh,$uj,$vj,$zj,$Aj,$qi,$Bj,$Ni,$Cj,$xi,$Dj,$sj,$Ej,$ii,$Fj,$Vi,$Gj,$Zj,$ck,$gk,$hk,$Xj,$ik,$Oj,$jk,$Kk,$Mk,$ml,$nl,$rl,$sl,$Vk,$tl,$kl,$ul,$yc,$vl,$sd,$wl,$qd,$xl,$Cb,$yl,$Kb,$zl,$Wb,$Al,$ib,$Bl,$wc,$Cl,$kc,$Dl,$Qm,$Rm,$Vm,$Wm,$Om,$Xm,$cm,$Ym,$ym,$Zm,$Ql,$cn,$om,$dn,$Sn,$Tn,$Xn,$Yn,$Cn,$Zn,$Qn,$co,$vn,$do,$np,$rp,$Dp,$Ep,$Bp,$Fp,$Hq,$Lq,$hr,$ir,$fr,$jr,$gq,$kr,$qq,$lr,$Xp,$mr,$Cq,$nr,$Po,$or,$lp,$pr,$Hr,$Ir,$Mr,$Nr,$yr,$Or,$Fr,$Pr,$K8,$Qr,$r8,$Rr,$O9,$Sr,$es,$fs,$C6,$gs,$ks,$ls,$cs,$ms,$s7,$ns,$Ds,$Es,$Is,$Js,$Bs,$Ks,$vs,$Ls,$j9,$Ms,$A8,$Ns,$h9,$Os,$Jt,$Kt,$Ot,$Pt,$mt,$Qt,$vt,$Rt,$Vs,$St,$dt,$Tt,$Ht,$Ut,$p8,$Vt,$Vu,$Zu,$nv,$ov,$lv,$pv,$ku,$qv,$Lu,$rv,$Cu,$sv,$Tu,$tv,$Dw,$Ew,$Iw,$Jw,$Bw,$Kw,$Pv,$Lw,$Iv,$Mw,$hw,$Nw,$Vw,$Ww,$kx,$lx,$Dz,$Ez,$Iz,$Jz,$tx,$Kz,$Rx,$Lz,$f8,$Mz,$s9,$Nz,$rA,$sA,$R8,$tA,$J6,$uA,$R6,$vA,$Y6,$wA,$zB,$AB,$EB,$FB,$xB,$GB,$eB,$HB,$nB,$IB,$KA,$JB,$LC,$MC,$j7,$NC,$hz,$OC,$Zx,$PC,$Bz,$QC,$Py,$RC,$mD,$nD,$rD,$sD,$ZC,$tD,$kD,$uD,$zy,$vD,$Y8,$wD,$h7,$xD,$YD,$ZD,$JD,$cE,$gE,$hE,$PD,$iE,$WD,$jE,$uE,$vE,$z9,$wE,$ME,$NE,$EE,$OE,$SE,$TE,$KE,$UE,$HF,$IF,$QF,$RF,$rF,$SF,$FF,$TF,$kF,$UF,$OF,$VF,$iF,$WF,$WG,$cH,$vH,$wH,$tH,$xH,$lH,$yH,$zG,$zH,$JG,$AH,$lG,$BH,$UG,$CH,$sG,$DH,$nI,$oI,$sI,$tI,$eI,$uI,$MH,$vI,$lI,$wI,$AI,$BI,$FI,$GI,$l9,$HI,$Q9,$II,$h8,$JI,$K9,$KI,$OI,$PI,$TI,$UI,$Co,$VI,$xp,$WI,$Ao,$XI,$oo,$YI,$dJ,$eJ,$zJ};$BJ=q#resolvers#;$CJ=[];$DJ=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$EJ=bless({$t,$CJ,$v,$DJ,$x,$y},$z);$FJ=q#file#;$GJ=[];$HJ=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$IJ=bless({$t,$GJ,$v,$HJ,$x,$y},$z);$JJ=q#null#;$KJ=[];$LJ=q#ni('ni:/io/null')->new#;$MJ=bless({$t,$KJ,$v,$LJ,$x,$y},$z);$NJ=q#sh#;$OJ=[];$PJ=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$QJ=bless({$t,$OJ,$v,$PJ,$x,$y},$z);$RJ=q#str#;$SJ=[];$TJ=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$UJ=bless({$t,$SJ,$v,$TJ,$x,$y},$z);$VJ={$Oe,$EJ,$FJ,$IJ,$JJ,$MJ,$NJ,$QJ,$RJ,$UJ};$WJ=bless({$c,$AJ,$BJ,$VJ},$M7);*$ED=\&$CD;*$DD=\&$AD;$C6->apply_($u5);$C6->apply_($v5);$C6->apply_($w5);$C6->apply_($x5);$C6->apply_($y5);$C6->apply_($z5);$C6->apply_($A5);$C6->apply_($B5);$C6->apply_($C5);$C6->apply_($D5);$C6->apply_($E5);$C6->apply_($F5);$C6->apply_($G5);$C6->apply_($H5);$C6->apply_($I5);$C6->apply_($J5);$C6->apply_($K5);$C6->apply_($L5);$C6->apply_($D6);$C6->apply_($M5);$C6->apply_($N5);$C6->apply_($O5);$C6->apply_($P5);$C6->apply_($Q5);$C6->apply_($R5);$C6->apply_($S5);$C6->apply_($T5);$C6->apply_($U5);$C6->apply_($V5);$C6->apply_($W5);$C6->apply_($X5);$C6->apply_($Y5);$C6->apply_($Z5);$C6->apply_($k6);$C6->apply_($c6);$C6->apply_($l6);$C6->apply_($d6);$C6->apply_($e6);$C6->apply_($f6);$C6->apply_($g6);$C6->apply_($h6);$J6->apply_($u5);$J6->apply_($v5);$J6->apply_($w5);$J6->apply_($x5);$J6->apply_($y5);$J6->apply_($z5);$J6->apply_($A5);$J6->apply_($B5);$J6->apply_($C5);$J6->apply_($D5);$J6->apply_($E5);$J6->apply_($F5);$J6->apply_($G5);$J6->apply_($H5);$J6->apply_($I5);$J6->apply_($J5);$J6->apply_($K5);$J6->apply_($L5);$J6->apply_($D6);$J6->apply_($M5);$J6->apply_($N5);$J6->apply_($L);$J6->apply_($O5);$J6->apply_($P5);$J6->apply_($Q5);$J6->apply_($R5);$J6->apply_($S5);$J6->apply_($T5);$J6->apply_($B6);$J6->apply_($U5);$J6->apply_($K6);$J6->apply_($V5);$J6->apply_($W5);$J6->apply_($X5);$J6->apply_($Y5);$J6->apply_($Z5);$J6->apply_($k6);$J6->apply_($c6);$J6->apply_($l6);$J6->apply_($d6);$J6->apply_($e6);$J6->apply_($f6);$J6->apply_($g6);$J6->apply_($h6);$R6->apply_($u5);$R6->apply_($v5);$R6->apply_($w5);$R6->apply_($x5);$R6->apply_($y5);$R6->apply_($z5);$R6->apply_($A5);$R6->apply_($B5);$R6->apply_($C5);$R6->apply_($D5);$R6->apply_($E5);$R6->apply_($F5);$R6->apply_($G5);$R6->apply_($H5);$R6->apply_($I5);$R6->apply_($J5);$R6->apply_($K5);$R6->apply_($L5);$R6->apply_($D6);$R6->apply_($M5);$R6->apply_($N5);$R6->apply_($O5);$R6->apply_($P5);$R6->apply_($Q5);$R6->apply_($R5);$R6->apply_($S5);$R6->apply_($T5);$R6->apply_($B6);$R6->apply_($U5);$R6->apply_($K6);$R6->apply_($V5);$R6->apply_($W5);$R6->apply_($X5);$R6->apply_($Y5);$R6->apply_($Z5);$R6->apply_($k6);$R6->apply_($c6);$R6->apply_($l6);$R6->apply_($d6);$R6->apply_($e6);$R6->apply_($f6);$R6->apply_($g6);$R6->apply_($h6);$Y6->apply_($u5);$Y6->apply_($v5);$Y6->apply_($w5);$Y6->apply_($x5);$Y6->apply_($y5);$Y6->apply_($z5);$Y6->apply_($A5);$Y6->apply_($B5);$Y6->apply_($C5);$Y6->apply_($D5);$Y6->apply_($E5);$Y6->apply_($F5);$Y6->apply_($G5);$Y6->apply_($H5);$Y6->apply_($I5);$Y6->apply_($J5);$Y6->apply_($K5);$Y6->apply_($L5);$Y6->apply_($D6);$Y6->apply_($M5);$Y6->apply_($N5);$Y6->apply_($O5);$Y6->apply_($P5);$Y6->apply_($Q5);$Y6->apply_($R5);$Y6->apply_($S5);$Y6->apply_($T5);$Y6->apply_($B6);$Y6->apply_($U5);$Y6->apply_($K6);$Y6->apply_($V5);$Y6->apply_($W5);$Y6->apply_($X5);$Y6->apply_($Y5);$Y6->apply_($Z5);$Y6->apply_($k6);$Y6->apply_($c6);$Y6->apply_($l6);$Y6->apply_($d6);$Y6->apply_($e6);$Y6->apply_($f6);$Y6->apply_($g6);$Y6->apply_($h6);$h7->apply_($u5);$h7->apply_($v5);$h7->apply_($w5);$h7->apply_($x5);$h7->apply_($y5);$h7->apply_($z5);$h7->apply_($A5);$h7->apply_($B5);$h7->apply_($C5);$h7->apply_($D5);$h7->apply_($E5);$h7->apply_($F5);$h7->apply_($G5);$h7->apply_($H5);$h7->apply_($I5);$h7->apply_($J5);$h7->apply_($K5);$h7->apply_($L5);$h7->apply_($D6);$h7->apply_($M5);$h7->apply_($N5);$h7->apply_($O5);$h7->apply_($P5);$h7->apply_($Q5);$h7->apply_($R5);$h7->apply_($S5);$h7->apply_($T5);$h7->apply_($U5);$h7->apply_($K6);$h7->apply_($V5);$h7->apply_($W5);$h7->apply_($X5);$h7->apply_($Y5);$h7->apply_($Z5);$h7->apply_($k6);$h7->apply_($c6);$h7->apply_($l6);$h7->apply_($d6);$h7->apply_($e6);$h7->apply_($f6);$h7->apply_($g6);$h7->apply_($h6);$s7->apply_($u5);$s7->apply_($v5);$s7->apply_($w5);$s7->apply_($x5);$s7->apply_($y5);$s7->apply_($z5);$s7->apply_($A5);$s7->apply_($B5);$s7->apply_($C5);$s7->apply_($D5);$s7->apply_($E5);$s7->apply_($F5);$s7->apply_($G5);$s7->apply_($H5);$s7->apply_($I5);$s7->apply_($J5);$s7->apply_($K5);$s7->apply_($L5);$s7->apply_($M5);$s7->apply_($N5);$s7->apply_($O5);$s7->apply_($P5);$s7->apply_($Q5);$s7->apply_($R5);$s7->apply_($S5);$s7->apply_($T5);$s7->apply_($U5);$s7->apply_($V5);$s7->apply_($W5);$s7->apply_($X5);$s7->apply_($Y5);$s7->apply_($Z5);$s7->apply_($k6);$s7->apply_($c6);$s7->apply_($l6);$s7->apply_($d6);$s7->apply_($e6);$s7->apply_($f6);$s7->apply_($g6);$s7->apply_($h6);$f8->apply_($u5);$f8->apply_($v5);$f8->apply_($t7);$f8->apply_($w5);$f8->apply_($u7);$f8->apply_($x5);$f8->apply_($v7);$f8->apply_($y5);$f8->apply_($w7);$f8->apply_($z5);$f8->apply_($x7);$f8->apply_($A5);$f8->apply_($y7);$f8->apply_($B5);$f8->apply_($z7);$f8->apply_($C5);$f8->apply_($A7);$f8->apply_($D5);$f8->apply_($B7);$f8->apply_($E5);$f8->apply_($C7);$f8->apply_($F5);$f8->apply_($D7);$f8->apply_($G5);$f8->apply_($E7);$f8->apply_($H5);$f8->apply_($F7);$f8->apply_($I5);$f8->apply_($G7);$f8->apply_($J5);$f8->apply_($H7);$f8->apply_($K5);$f8->apply_($I7);$f8->apply_($L5);$f8->apply_($D6);$f8->apply_($M5);$f8->apply_($J7);$f8->apply_($N5);$f8->apply_($L);$f8->apply_($O5);$f8->apply_($z);$f8->apply_($P5);$f8->apply_($K7);$f8->apply_($Q5);$f8->apply_($L7);$f8->apply_($R5);$f8->apply_($M7);$f8->apply_($S5);$f8->apply_($N7);$f8->apply_($T5);$f8->apply_($B6);$f8->apply_($U5);$f8->apply_($K6);$f8->apply_($V5);$f8->apply_($O7);$f8->apply_($W5);$f8->apply_($P7);$f8->apply_($X5);$f8->apply_($B);$f8->apply_($Y5);$f8->apply_($Q7);$f8->apply_($Z5);$f8->apply_($k6);$f8->apply_($c6);$f8->apply_($l6);$f8->apply_($d6);$f8->apply_($R7);$f8->apply_($e6);$f8->apply_($f6);$f8->apply_($g6);$f8->apply_($S7);$f8->apply_($h6);$p8->apply_($u5);$p8->apply_($v5);$p8->apply_($w5);$p8->apply_($x5);$p8->apply_($y5);$p8->apply_($z5);$p8->apply_($A5);$p8->apply_($B5);$p8->apply_($C5);$p8->apply_($D5);$p8->apply_($E5);$p8->apply_($F5);$p8->apply_($G5);$p8->apply_($H5);$p8->apply_($I5);$p8->apply_($J5);$p8->apply_($K5);$p8->apply_($I7);$p8->apply_($L5);$p8->apply_($D6);$p8->apply_($M5);$p8->apply_($J7);$p8->apply_($N5);$p8->apply_($O5);$p8->apply_($P5);$p8->apply_($Q5);$p8->apply_($R5);$p8->apply_($S5);$p8->apply_($T5);$p8->apply_($B6);$p8->apply_($U5);$p8->apply_($K6);$p8->apply_($V5);$p8->apply_($W5);$p8->apply_($X5);$p8->apply_($Y5);$p8->apply_($Z5);$p8->apply_($k6);$p8->apply_($c6);$p8->apply_($l6);$p8->apply_($d6);$p8->apply_($e6);$p8->apply_($f6);$p8->apply_($g6);$p8->apply_($h6);$A8->apply_($u5);$A8->apply_($v5);$A8->apply_($w5);$A8->apply_($x5);$A8->apply_($y5);$A8->apply_($z5);$A8->apply_($A5);$A8->apply_($B5);$A8->apply_($C5);$A8->apply_($D5);$A8->apply_($E5);$A8->apply_($F5);$A8->apply_($G5);$A8->apply_($H5);$A8->apply_($I5);$A8->apply_($J5);$A8->apply_($K5);$A8->apply_($L5);$A8->apply_($D6);$A8->apply_($M5);$A8->apply_($N5);$A8->apply_($O5);$A8->apply_($P5);$A8->apply_($Q5);$A8->apply_($R5);$A8->apply_($S5);$A8->apply_($T5);$A8->apply_($U5);$A8->apply_($V5);$A8->apply_($W5);$A8->apply_($X5);$A8->apply_($Y5);$A8->apply_($Z5);$A8->apply_($k6);$A8->apply_($c6);$A8->apply_($l6);$A8->apply_($d6);$A8->apply_($e6);$A8->apply_($f6);$A8->apply_($g6);$A8->apply_($h6);$K8->apply_($u5);$K8->apply_($v5);$K8->apply_($w5);$K8->apply_($x5);$K8->apply_($y5);$K8->apply_($z5);$K8->apply_($A5);$K8->apply_($B5);$K8->apply_($C5);$K8->apply_($D5);$K8->apply_($E5);$K8->apply_($F5);$K8->apply_($G5);$K8->apply_($H5);$K8->apply_($I5);$K8->apply_($J5);$K8->apply_($K5);$K8->apply_($L5);$K8->apply_($D6);$K8->apply_($M5);$K8->apply_($N5);$K8->apply_($O5);$K8->apply_($P5);$K8->apply_($Q5);$K8->apply_($R5);$K8->apply_($S5);$K8->apply_($T5);$K8->apply_($U5);$K8->apply_($V5);$K8->apply_($W5);$K8->apply_($X5);$K8->apply_($Y5);$K8->apply_($Z5);$K8->apply_($k6);$K8->apply_($c6);$K8->apply_($l6);$K8->apply_($d6);$K8->apply_($e6);$K8->apply_($f6);$K8->apply_($g6);$K8->apply_($h6);$R8->apply_($u5);$R8->apply_($v5);$R8->apply_($w5);$R8->apply_($x5);$R8->apply_($y5);$R8->apply_($z5);$R8->apply_($A5);$R8->apply_($B5);$R8->apply_($C5);$R8->apply_($D5);$R8->apply_($E5);$R8->apply_($F5);$R8->apply_($G5);$R8->apply_($H5);$R8->apply_($I5);$R8->apply_($J5);$R8->apply_($K5);$R8->apply_($L5);$R8->apply_($D6);$R8->apply_($M5);$R8->apply_($N5);$R8->apply_($O5);$R8->apply_($P5);$R8->apply_($Q5);$R8->apply_($R5);$R8->apply_($S5);$R8->apply_($T5);$R8->apply_($U5);$R8->apply_($V5);$R8->apply_($W5);$R8->apply_($X5);$R8->apply_($Y5);$R8->apply_($Z5);$R8->apply_($k6);$R8->apply_($c6);$R8->apply_($l6);$R8->apply_($d6);$R8->apply_($e6);$R8->apply_($f6);$R8->apply_($g6);$R8->apply_($h6);$Y8->apply_($u5);$Y8->apply_($v5);$Y8->apply_($w5);$Y8->apply_($x5);$Y8->apply_($y5);$Y8->apply_($z5);$Y8->apply_($A5);$Y8->apply_($B5);$Y8->apply_($C5);$Y8->apply_($D5);$Y8->apply_($E5);$Y8->apply_($F5);$Y8->apply_($G5);$Y8->apply_($H5);$Y8->apply_($I5);$Y8->apply_($J5);$Y8->apply_($K5);$Y8->apply_($L5);$Y8->apply_($D6);$Y8->apply_($M5);$Y8->apply_($N5);$Y8->apply_($O5);$Y8->apply_($P5);$Y8->apply_($Q5);$Y8->apply_($R5);$Y8->apply_($S5);$Y8->apply_($T5);$Y8->apply_($U5);$Y8->apply_($V5);$Y8->apply_($W5);$Y8->apply_($X5);$Y8->apply_($Y5);$Y8->apply_($Z5);$Y8->apply_($k6);$Y8->apply_($c6);$Y8->apply_($l6);$Y8->apply_($d6);$Y8->apply_($e6);$Y8->apply_($f6);$Y8->apply_($g6);$Y8->apply_($h6);$h9->apply_($u5);$h9->apply_($v5);$h9->apply_($w5);$h9->apply_($x5);$h9->apply_($y5);$h9->apply_($z5);$h9->apply_($A5);$h9->apply_($B5);$h9->apply_($C5);$h9->apply_($D5);$h9->apply_($E5);$h9->apply_($F5);$h9->apply_($G5);$h9->apply_($H5);$h9->apply_($I5);$h9->apply_($J5);$h9->apply_($K5);$h9->apply_($L5);$h9->apply_($D6);$h9->apply_($M5);$h9->apply_($N5);$h9->apply_($O5);$h9->apply_($P5);$h9->apply_($Q5);$h9->apply_($R5);$h9->apply_($S5);$h9->apply_($T5);$h9->apply_($U5);$h9->apply_($V5);$h9->apply_($W5);$h9->apply_($X5);$h9->apply_($Y5);$h9->apply_($Z5);$h9->apply_($k6);$h9->apply_($c6);$h9->apply_($l6);$h9->apply_($d6);$h9->apply_($e6);$h9->apply_($f6);$h9->apply_($g6);$h9->apply_($h6);$s9->apply_($u5);$s9->apply_($v5);$s9->apply_($w5);$s9->apply_($x5);$s9->apply_($y5);$s9->apply_($z5);$s9->apply_($A5);$s9->apply_($B5);$s9->apply_($C5);$s9->apply_($D5);$s9->apply_($E5);$s9->apply_($F5);$s9->apply_($G5);$s9->apply_($H5);$s9->apply_($I5);$s9->apply_($J5);$s9->apply_($K5);$s9->apply_($L5);$s9->apply_($M5);$s9->apply_($N5);$s9->apply_($O5);$s9->apply_($z);$s9->apply_($P5);$s9->apply_($Q5);$s9->apply_($R5);$s9->apply_($S5);$s9->apply_($T5);$s9->apply_($B6);$s9->apply_($U5);$s9->apply_($K6);$s9->apply_($V5);$s9->apply_($W5);$s9->apply_($X5);$s9->apply_($Y5);$s9->apply_($Z5);$s9->apply_($k6);$s9->apply_($c6);$s9->apply_($d6);$s9->apply_($e6);$s9->apply_($f6);$s9->apply_($g6);$s9->apply_($h6);$z9->apply_($u5);$z9->apply_($v5);$z9->apply_($w5);$z9->apply_($x5);$z9->apply_($y5);$z9->apply_($z5);$z9->apply_($A5);$z9->apply_($B5);$z9->apply_($C5);$z9->apply_($D5);$z9->apply_($E5);$z9->apply_($F5);$z9->apply_($G5);$z9->apply_($H5);$z9->apply_($I5);$z9->apply_($J5);$z9->apply_($K5);$z9->apply_($L5);$z9->apply_($M5);$z9->apply_($N5);$z9->apply_($O5);$z9->apply_($P5);$z9->apply_($Q5);$z9->apply_($R5);$z9->apply_($S5);$z9->apply_($T5);$z9->apply_($U5);$z9->apply_($V5);$z9->apply_($W5);$z9->apply_($X5);$z9->apply_($Y5);$z9->apply_($Z5);$z9->apply_($c6);$z9->apply_($d6);$z9->apply_($e6);$z9->apply_($f6);$z9->apply_($g6);$z9->apply_($h6);$da->apply_($t7);$la->apply_($t7);$Da->apply_($u7);$La->apply_($u7);$ib->apply_($v7);$ib->apply_($w7);$ib->apply_($x7);$ib->apply_($y7);$ib->apply_($z7);$ib->apply_($A7);$ib->apply_($B7);$ib->apply_($C7);$ib->apply_($D7);$ib->apply_($E7);$Cb->apply_($v7);$Cb->apply_($w7);$Cb->apply_($x7);$Cb->apply_($y7);$Cb->apply_($z7);$Cb->apply_($A7);$Cb->apply_($B7);$Cb->apply_($C7);$Cb->apply_($D7);$Cb->apply_($E7);$Kb->apply_($v7);$Kb->apply_($w7);$Kb->apply_($x7);$Kb->apply_($y7);$Kb->apply_($z7);$Kb->apply_($A7);$Kb->apply_($B7);$Kb->apply_($C7);$Kb->apply_($D7);$Kb->apply_($E7);$Wb->apply_($v7);$Wb->apply_($w7);$Wb->apply_($x7);$Wb->apply_($y7);$Wb->apply_($z7);$Wb->apply_($A7);$Wb->apply_($B7);$Wb->apply_($C7);$Wb->apply_($D7);$Wb->apply_($E7);$kc->apply_($v7);$kc->apply_($w7);$kc->apply_($x7);$kc->apply_($y7);$kc->apply_($z7);$kc->apply_($A7);$kc->apply_($B7);$kc->apply_($C7);$kc->apply_($D7);$kc->apply_($E7);$wc->apply_($v7);$wc->apply_($w7);$wc->apply_($x7);$wc->apply_($y7);$wc->apply_($z7);$wc->apply_($A7);$wc->apply_($B7);$wc->apply_($C7);$wc->apply_($D7);$wc->apply_($E7);$Fc->apply_($v7);$Zc->apply_($v7);$qd->apply_($y5);$qd->apply_($z5);$qd->apply_($A5);$qd->apply_($B5);$qd->apply_($C5);$qd->apply_($D5);$qd->apply_($E5);$qd->apply_($F5);$qd->apply_($G5);$qd->apply_($H5);$Fd->apply_($w7);$Md->apply_($w7);$he->apply_($x7);$oe->apply_($x7);$Ie->apply_($x7);$if->apply_($x7);$qf->apply_($x7);$Gf->apply_($x7);$fg->apply_($y7);$fg->apply_($A7);$mg->apply_($y7);$ug->apply_($y7);$Kg->apply_($y7);$Kg->apply_($A7);$Vg->apply_($y7);$lh->apply_($y7);$lh->apply_($A7);$Oh->apply_($B5);$ii->apply_($z7);$qi->apply_($z7);$xi->apply_($z7);$Ni->apply_($z7);$Vi->apply_($z7);$sj->apply_($z7);$Oj->apply_($A7);$Xj->apply_($A7);$Kk->apply_($Lk);$Vk->apply_($B7);$kl->apply_($B7);$Ql->apply_($D7);$cm->apply_($D7);$om->apply_($D7);$ym->apply_($D7);$Om->apply_($D7);$vn->apply_($E7);$Cn->apply_($E7);$Qn->apply_($E7);$oo->apply_($F7);$oo->apply_($G7);$oo->apply_($H7);$oo->apply_($S7);$Ao->apply_($F7);$Ao->apply_($G7);$Ao->apply_($H7);$Ao->apply_($S7);$Po->apply_($F7);$Po->apply_($G7);$Po->apply_($H7);$lp->apply_($F7);$lp->apply_($G7);$lp->apply_($H7);$Bp->apply_($I5);$Bp->apply_($J5);$Bp->apply_($K5);$Xp->apply_($G7);$gq->apply_($G7);$qq->apply_($G7);$Cq->apply_($G7);$fr->apply_($J5);$yr->apply_($H7);$Fr->apply_($H7);$cs->apply_($D6);$vs->apply_($J7);$Bs->apply_($J7);$Vs->apply_($L);$dt->apply_($L);$mt->apply_($L);$vt->apply_($L);$Ht->apply_($L);$ku->apply_($z);$Cu->apply_($z);$Lu->apply_($z);$Tu->apply_($z);$lv->apply_($P5);$Iv->apply_($K7);$Pv->apply_($K7);$hw->apply_($K7);$Bw->apply_($K7);$Vw->apply_($L7);$kx->apply_($Lk);$tx->apply_($L7);$Rx->apply_($L7);$Zx->apply_($L7);$Zx->apply_($N7);$zy->apply_($L7);$zy->apply_($N7);$Py->apply_($L7);$Py->apply_($N7);$hz->apply_($L7);$Bz->apply_($L7);$rA->apply_($fa);$KA->apply_($M7);$eB->apply_($M7);$nB->apply_($M7);$xB->apply_($M7);$LC->apply_($fa);$ZC->apply_($N7);$kD->apply_($N7);$JD->apply_($B6);$PD->apply_($B6);$WD->apply_($B6);$uE->apply_($Lk);$EE->apply_($K6);$KE->apply_($K6);$iF->apply_($O7);$iF->apply_($P7);$rF->apply_($O7);$FF->apply_($O7);$lG->apply_($B);$sG->apply_($B);$zG->apply_($B);$JG->apply_($B);$UG->apply_($B);$lH->apply_($Y5);$tH->apply_($Y5);$MH->apply_($Q7);$eI->apply_($Q7);$lI->apply_($Q7);$ni::self=$WJ;&$O($M);&$O($U);&$O($e1);&$O($k1);&$O($x1);&$O($K1);&$O($X1);&$O($m2);&$O($I2);&$O($O2);&$O($n3);&$O($u3);&$O($O3);&$O($h4);&$O($v4);&$O($D4);&$O($O4);&$O($l5);&$O($r5);&$O($C6);&$O($J6);&$O($R6);&$O($Y6);&$O($h7);&$O($j7);&$O($s7);&$O($f8);&$O($h8);&$m7($h8);&$O($p8);&$O($r8);&$m7($r8);&$O($A8);&$O($K8);&$O($R8);&$O($Y8);&$O($h9);&$O($j9);&$O($l9);&$m7($l9);&$O($s9);&$O($z9);&$O($B9);&$m7($B9);&$O($K9);&$m7($K9);&$O($O9);&$m7($O9);&$O($Q9);&$m7($Q9);&$O($S9);&$m7($S9);&$O($da);&$O($la);&$O($na);&$m7($na);&$O($sa);&$m7($sa);&$O($Da);&$O($La);&$O($Na);&$m7($Na);&$O($Sa);&$m7($Sa);&$O($ib);&$O($Cb);&$O($Kb);&$O($Wb);&$O($kc);&$O($wc);&$O($yc);&$m7($yc);&$O($Fc);&$O($Zc);&$O($dd);&$m7($dd);&$O($qd);&$O($sd);&$m7($sd);&$O($ud);&$m7($ud);&$O($Fd);&$O($Md);&$O($Od);&$m7($Od);&$O($Td);&$m7($Td);&$O($he);&$O($oe);&$O($Ie);&$O($if);&$O($qf);&$O($Gf);&$O($If);&$m7($If);&$O($Nf);&$m7($Nf);&$O($fg);&$O($mg);&$O($ug);&$O($Kg);&$O($Vg);&$O($lh);&$O($oh);&$m7($oh);&$rh($oh);&$O($Oh);&$O($Qh);&$m7($Qh);&$O($ii);&$O($qi);&$O($xi);&$O($Ni);&$O($Vi);&$O($sj);&$O($uj);&$m7($uj);&$O($zj);&$m7($zj);&$O($Oj);&$O($Xj);&$O($Zj);&$m7($Zj);&$O($gk);&$m7($gk);&$O($Kk);&$O($Vk);&$O($kl);&$O($ml);&$m7($ml);&$O($rl);&$m7($rl);&$O($Ql);&$O($cm);&$O($om);&$O($ym);&$O($Om);&$O($Qm);&$m7($Qm);&$O($Vm);&$m7($Vm);&$O($vn);&$O($Cn);&$O($Qn);&$O($Sn);&$m7($Sn);&$O($Xn);&$m7($Xn);&$O($oo);&$O($Ao);&$O($Co);&$m7($Co);&$O($Po);&$O($lp);&$O($np);&$m7($np);&$qp($np);&$O($xp);&$m7($xp);&$O($Bp);&$O($Dp);&$m7($Dp);&$O($Xp);&$O($gq);&$O($qq);&$O($Cq);&$O($Hq);&$m7($Hq);&$qp($Hq);&$Kq($Hq);&$O($fr);&$O($hr);&$m7($hr);&$O($yr);&$O($Fr);&$O($Hr);&$m7($Hr);&$qp($Hr);&$O($Mr);&$m7($Mr);&$O($cs);&$O($es);&$m7($es);&$O($ks);&$m7($ks);&$O($vs);&$O($Bs);&$O($Ds);&$m7($Ds);&$O($Is);&$m7($Is);&$O($Vs);&$O($dt);&$O($mt);&$O($vt);&$O($Ht);&$O($Jt);&$m7($Jt);&$O($Ot);&$m7($Ot);&$O($ku);&$O($Cu);&$O($Lu);&$O($Tu);&$O($Vu);&$m7($Vu);&$Yu($Vu);&$O($lv);&$O($nv);&$m7($nv);&$O($Iv);&$O($Pv);&$O($hw);&$O($Bw);&$O($Dw);&$m7($Dw);&$O($Iw);&$m7($Iw);&$O($Vw);&$O($kx);&$O($tx);&$O($Rx);&$O($Zx);&$O($zy);&$O($Py);&$O($hz);&$O($Bz);&$O($Dz);&$m7($Dz);&$O($Iz);&$m7($Iz);&$O($rA);&$O($KA);&$O($eB);&$O($nB);&$O($xB);&$O($zB);&$m7($zB);&$O($EB);&$m7($EB);&$O($LC);&$O($ZC);&$O($kD);&$O($mD);&$m7($mD);&$O($rD);&$m7($rD);&$O($JD);&$O($PD);&$O($WD);&$O($YD);&$m7($YD);&$O($gE);&$m7($gE);&$O($uE);&$O($EE);&$O($KE);&$O($ME);&$m7($ME);&$O($SE);&$m7($SE);&$O($iF);&$O($kF);&$m7($kF);&$O($rF);&$O($FF);&$O($HF);&$m7($HF);&$O($OF);&$m7($OF);&$O($QF);&$m7($QF);&$O($lG);&$O($sG);&$O($zG);&$O($JG);&$O($UG);&$O($WG);&$m7($WG);&$ZG($WG);&$O($lH);&$O($tH);&$O($vH);&$m7($vH);&$O($MH);&$O($eI);&$O($lI);&$O($nI);&$m7($nI);&$O($sI);&$m7($sI);&$O($AI);&$m7($AI);&$O($FI);&$m7($FI);&$O($OI);&$m7($OI);&$O($TI);&$m7($TI);&$O($dJ);&$m7($dJ);&$O($zJ);&$m7($zJ);ni->run(@ARGV);
__DATA__
