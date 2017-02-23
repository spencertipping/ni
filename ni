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
sub fn(;$);
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
which makes multiple inheritance straightforward to implement.#;$I=[$G,$H,$E];$J=[$h,$k,$F,$I];$K=q#name#;$L=q#/class#;$M=q#lib/doc#;$N=bless({$e,$J,$K,$L},$M);$O=q#my $s = shift; ni->def($s->name, $s)#;$P=bless({$w,$O,$y,$z},$A);$Q=q#ni.doc:/io#;$R=q#An implementation of IO in terms of system-level FDs. We need this for a
few reasons, three of them being that (1) old versions of Perl don't
correctly handle interrupted system calls, (2) we want tighter control
over which FDs are closed at what times, and (3) we want to be able to
"unread" things -- push back against the read buffer (or use a custom
read format in general).#;$S=[$i,$R];$T=[$S];$U=q#/io#;$V=bless({$e,$T,$K,$U},$M);$W=q#ni.doc:/io/buffer#;$X=q#
my $buf = ni("ni:/io/buffer")->new(8192);
$buf->write("foo");
$buf->read($_, 256);        \# reads "foo"#;$Y=[$f,$X];$Z=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
behaves like a nonblocking socket and sets errno accordingly.#;$c1=[];$d1=[];$e1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$f1=bless({$t,$d1,$v,$q,$w,$e1,$y,$z},$A);$g1=bless({$n,$c1,$p,$q,$r,$q,$s,$f1},$C);$h1=[$i,$Z,$g1];$i1=[$Y,$h1];$j1=q#/io/buffer#;$k1=bless({$e,$i1,$K,$j1},$M);$l1=q#ni.doc:/io/cat#;$m1=q#
  my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
  my $combined = $io1 + $io2 + $io3;
  $combined->into_sync($destination_io);
#;$n1=[$f,$m1];$o1=q#Concatenates multiple IO objects into a single read-only data source.
This is a mutable object that consumes its inputs and then loses its
references to them as quickly as possible, allowing their resources to be
freed. Once fully consumed, the cat object holds no references.#;$p1=[];$q1=[];$r1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$s1=bless({$t,$q1,$v,$q,$w,$r1,$y,$z},$A);$t1=bless({$n,$p1,$p,$q,$r,$q,$s,$s1},$C);$u1=[$i,$o1,$t1];$v1=[$n1,$u1];$w1=q#/io/cat#;$x1=bless({$e,$v1,$K,$w1},$M);$y1=q#ni.doc:/io/exec#;$z1=q#
my $pid = ni("ni:/io/exec")->new("ls", "-l")
  ->connect(1 => ni("file:foo")->w)
  ->env(ENV_VAR => "value", ENV2 => "val2")
  ->fork;
$? = $pid->await or die "ls -l failed: $?";#;$A1=[$f,$z1];$B1=q#An object that represents a fork+exec operation that hasn't yet happened.
It allows you to incrementally specify the context of the process,
including environment variables and file descriptor mappings. It is also
an IO object and will set up pipes to stdin/out if you use it this way.#;$C1=[];$D1=[];$E1=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$F1=bless({$t,$D1,$v,$q,$w,$E1,$y,$z},$A);$G1=bless({$n,$C1,$p,$q,$r,$q,$s,$F1},$C);$H1=[$i,$B1,$G1];$I1=[$A1,$H1];$J1=q#/io/exec#;$K1=bless({$e,$I1,$K,$J1},$M);$L1=q#ni.doc:/io/fd#;$M1=q#
  open my $fh, ...;
  my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
  my $fd = ni('ni:/io/fd')->new(0);   \# from number
  my $fd = ni('fd:0');                \# same thing
  $fd->nonblock(1)->read($_, 100);
  $fd->be(10);                        \# move FD number
#;$N1=[$f,$M1];$O1=q#Represents a file descriptor as a child of /io/object (so the usual IO
methods like into_async are available), and provides some convenience
functions for things like setting up FDs for child processes. FDs are
closed when destroyed.#;$P1=[];$Q1=[];$R1=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$S1=bless({$t,$Q1,$v,$q,$w,$R1,$y,$z},$A);$T1=bless({$n,$P1,$p,$q,$r,$q,$s,$S1},$C);$U1=[$i,$O1,$T1];$V1=[$N1,$U1];$W1=q#/io/fd#;$X1=bless({$e,$V1,$K,$W1},$M);$Y1=q#ni.doc:/io/file#;$Z1=q#
  my $f = ni('ni:/io/file')->new('/etc/passwd');
  my $f = ni('file:/etc/passwd');     \# same as above
  $f->into_sync(ni('fd:1'));          \# cat to stdout
#;$c2=[$f,$Z1];$d2=q#warning#;$e2=q#Files overload the -X file test operators, but this feature wasn't
introduced until Perl 5.12 -- prior versions won't recognize this
overload. That means that using this overload in ni's base code will
reduce its portability and cause tests to fail.#;$f2=[$d2,$e2];$g2=q#Represents a file that may or may not exist, and stores/constructs file
descriptors for reading/writing. /io/files are one-shot objects: once
you've consumed them for reading or written to them, you should destroy
the object and start over (or close the file) if you want to operate on
the file further -- put differently, /io/file objects own the FDs they
create.#;$h2=[];$i2=[];$j2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$k2=bless({$t,$i2,$v,$q,$w,$j2,$y,$z},$A);$l2=bless({$n,$h2,$p,$q,$r,$q,$s,$k2},$C);$m2=q#File objects also provide some useful functions like atomic-updating.
This lets you write a stream slowly into a tempfile, then rename over the
original once the tempfile is closed. ni uses this to update itself to
avoid race conditions.#;$n2=[];$o2=[];$p2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$q2=bless({$t,$o2,$v,$q,$w,$p2,$y,$z},$A);$r2=bless({$n,$n2,$p,$q,$r,$q,$s,$q2},$C);$s2=[$i,$g2,$l2,$m2,$r2];$t2=[$c2,$f2,$s2];$u2=q#/io/file#;$v2=bless({$e,$t2,$K,$u2},$M);$w2=q#ni.doc:/io/file_update_fd#;$x2=q#A write fd that performs a file rename upon closing.#;$y2=[$i,$x2];$z2=[$y2];$A2=q#/io/file_update_fd#;$B2=bless({$e,$z2,$K,$A2},$M);$C2=q#ni.doc:/io/pid#;$D2=q#eg#;$E2=[];$F2=[];$G2=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$H2=bless({$t,$F2,$v,$q,$w,$G2,$y,$z},$A);$I2=bless({$n,$E2,$p,$q,$r,$q,$s,$H2},$C);$J2=[$D2,$I2];$K2=[];$L2=[];$M2=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$N2=bless({$t,$L2,$v,$q,$w,$M2,$y,$z},$A);$O2=bless({$n,$K2,$p,$q,$r,$q,$s,$N2},$C);$P2=[$D2,$O2];$Q2=[];$R2=[];$S2=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$T2=bless({$t,$R2,$v,$q,$w,$S2,$y,$z},$A);$U2=bless({$n,$Q2,$p,$q,$r,$q,$s,$T2},$C);$V2=[$D2,$U2];$W2=[$J2,$P2,$V2];$X2=q#/io/pid#;$Y2=bless({$e,$W2,$K,$X2},$M);$Z2=q#ni.doc:/lib#;$c3=q#Bootstrapping code for the core abstractions in ni, and almost everything
about its introspection. This includes definitions for documentation,
unit tests, classes, support for basic image generation, etc -- and when
possible, it's written with some awareness of downstream use cases (for
instance, image serialization and RMI share logic).#;$d3=q#/lib is the place where things don't quite work yet, so the code here is
written differently from other modules.#;$e3=[$i,$c3,$d3];$f3=[$e3];$g3=q#/lib#;$h3=bless({$e,$f3,$K,$g3},$M);$i3=q#ni.doc:/lib/doc#;$j3=q#
ni("ni:/some/class")->doc
  ->synopsis(...)
  ->description(...)
  ->eg(...)
  ...#;$k3=[$f,$j3];$l3=q#Associate documentation with the specified class. Documentation is stored
separately and in the "ni.doc" namespace; this way you can serialize
instances of the class and the class's code without bringing along all of
its documentation and unit tests.#;$m3=q#Documentation objects are internally represented as arrays of quoted
method calls:#;$n3=[];$o3=[];$p3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$q3=bless({$t,$o3,$v,$q,$w,$p3,$y,$z},$A);$r3=bless({$n,$n3,$p,$q,$r,$q,$s,$q3},$C);$s3=q#This documentation can later be compiled into things like manpages,
markdown, or HTML by target-specific conversion functions. Documentation
also stores unit tests, which are specified using "eg":#;$t3=[];$u3=[];$v3=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$w3=bless({$t,$u3,$v,$q,$w,$v3,$y,$z},$A);$x3=bless({$n,$t3,$p,$q,$r,$q,$s,$w3},$C);$y3=[$i,$l3,$m3,$r3,$s3,$x3];$z3=[$k3,$y3];$A3=q#/lib/doc#;$B3=bless({$e,$z3,$K,$A3},$M);$C3=q#ni.doc:/lib/future#;$D3=q#An expression that doesn't yet exist, but is finalized once it does
exist.#;$E3=[];$F3=[];$G3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$H3=bless({$t,$F3,$v,$q,$w,$G3,$y,$z},$A);$I3=bless({$n,$E3,$p,$q,$r,$q,$s,$H3},$C);$J3=q#You can combine multiple futures in different ways depending on what
you're trying to do.#;$K3=[];$L3=[];$M3=q#my $f1 = ni('ni:/lib/future')->new;
my $f2 = ni('ni:/lib/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1], [2]];#;$N3=bless({$t,$L3,$v,$q,$w,$M3,$y,$z},$A);$O3=bless({$n,$K3,$p,$q,$r,$q,$s,$N3},$C);$P3=[$i,$D3,$I3,$J3,$O3];$Q3=[$P3];$R3=q#/lib/future#;$S3=bless({$e,$Q3,$K,$R3},$M);$T3=q#ni.doc:/lib/image#;$U3=q#
my $image = ni("ni:/lib/image")->new;
my $gensym = $image->quote($value);
$image->io->into_sync($a_file);#;$V3=[$f,$U3];$W3=q#Generates Perl code that reconstructs the state of objects at the
behavior/slice level. Since classes are self-describing, this results in
a replica of the runtime object-oriented state.#;$X3=[$i,$W3];$Y3=[$V3,$X3];$Z3=q#/lib/image#;$c4=bless({$e,$Y3,$K,$Z3},$M);$d4=q#ni.doc:/lib/ni#;$e4=q#my $value = ni->resolve($name);
my $value = ni($name);   \# alias for ni->resolve($name)
my $self  = ni;#;$f4=[$f,$e4];$g4=q#The class for the currently-running ni instance. This includes all
instance state, the table of named objects, and a bit of logic to update
ni in place, for instance when adding extensions.#;$h4=[$i,$g4];$i4=[$f4,$h4];$j4=q#/lib/ni#;$k4=bless({$e,$i4,$K,$j4},$M);$l4=q#ni.doc:/lib/quote_simple#;$m4=q#A stateless object that serializes values with direct quotation; that
is, the serialization contains no variables. If your objects have
circular or shared references, you should probably use
/lib/quote_circular or similar.#;$n4=[];$o4=[];$p4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$q4=bless({$t,$o4,$v,$q,$w,$p4,$y,$z},$A);$r4=bless({$n,$n4,$p,$q,$r,$q,$s,$q4},$C);$s4=[$i,$m4,$r4];$t4=[$s4];$u4=q#/lib/quote_simple#;$v4=bless({$e,$t4,$K,$u4},$M);$w4=q#ni.doc:/lib/slice#;$x4=q#
ni('ni:/lib/slice')->new('/lib/foo',
  ctor => fn q{shift->say_hi},
  say_hi => fn q{print "hi from " . shift->name . "\\n"});
$some_class->add('/lib/foo');#;$y4=[$f,$x4];$z4=q#A slice of methods encoding some aspect of an object's behavior. Slices
are combined using tags and branches, and the set of slices used to
construct a class must be disjoint except for constructors and
destructors.#;$A4=q#Slices are objects that provide an ->apply method, which installs their
methods + ctors + dtors into a Perl package.#;$B4=[];$C4=[];$D4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$E4=bless({$t,$C4,$v,$q,$w,$D4,$y,$z},$A);$F4=bless({$n,$B4,$p,$q,$r,$q,$s,$E4},$C);$G4=q#Slices automatically do the equivalent of using Perl's "overload" module
if any methods begin with an open-paren.#;$H4=q#Classes automatically incorporate some special low-level slices that are
used by others; one of these is /lib/instantiable.b, which implements
->new and ->DESTROY. These methods then call into the lists of
constructors and destructors implemented when slices are added to a
package.#;$I4=[];$J4=[];$K4=q#my $instances = 0;
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
now $instances == 0;#;$L4=bless({$t,$J4,$v,$q,$w,$K4,$y,$z},$A);$M4=bless({$n,$I4,$p,$q,$r,$q,$s,$L4},$C);$N4=[$i,$z4,$A4,$F4,$G4,$H4,$M4];$O4=[$y4,$N4];$P4=q#/lib/slice#;$Q4=bless({$e,$O4,$K,$P4},$M);$R4=q#ni.doc:/semantic#;$S4=q#Opportunities to assign real-world semantics to objects. This is a
collection of behaviors that don't necessarily imply a Perl-level
protocol, but which may end up meaning something at some point.#;$T4=[$i,$S4];$U4=[$T4];$V4=q#/semantic#;$W4=bless({$e,$U4,$K,$V4},$M);$X4=q#ni:/class#;$Y4=q#applied_to#;$Z4=q#class#;$c5=q#class.c#;$d5=q#io/buffer.c#;$e5=q#io/cat.c#;$f5=q#io/exec.c#;$g5=q#io/fd.c#;$h5=q#io/file.c#;$i5=q#io/file_update_fd.c#;$j5=q#io/null.c#;$k5=q#io/object.c#;$l5=q#io/pid.c#;$m5=q#io/str.c#;$n5=q#io/transfer.c#;$o5=q#io/transfer_async.c#;$p5=q#io/transfer_sync.c#;$q5=q#lib/behavior.c#;$r5=q#lib/branch.c#;$s5=q#lib/dataslice.c#;$t5=q#lib/doc.c#;$u5=q#lib/fn.c#;$v5=q#lib/future.c#;$w5=q#lib/image.c#;$x5=q#lib/ni.c#;$y5=q#lib/quote_simple.c#;$z5=q#lib/slice.c#;$A5=q#lib/tag.c#;$B5=q#lib/test_assert_eq.c#;$C5=q#lib/test_assertion.c#;$D5=q#lib/test_case.c#;$E5=q#lib/test_value.c#;$F5=q#metaclass.c#;$G5=q#module.c#;$H5=q#object.c#;$I5=q#semantic/dimension#;$J5=q#semantic/dimension.c#;$K5=q#semantic/task.c#;$L5={$Z4,1,$c5,1,$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1};$M5=q#slices#;$N5=q#metaclass#;$O5=q#module#;$P5={$Z4,1,$c5,1,$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$N5,1,$F5,1,$O5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1};$Q5=q#/module#;$R5=q#/lib/perlbranch.b#;$S5={};$T5=q#ctor#;$U5=q#dtor#;$V5=q#methods#;$W5=q#add#;$X5=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$Y5=bless({$w,$X5},$A);$Z5=q#apply#;$c6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$d6=bless({$w,$c6},$A);$e6={$W5,$Y5,$Z5,$d6};$f6=q#/lib/branch.b#;$g6=q#lib/slice#;$h6=bless({$Y4,$S5,$T5,$q,$U5,$q,$V5,$e6,$K,$f6},$g6);$i6=q#lib/branch#;$j6={};$k6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$l6=bless({$w,$k6},$A);$m6={$K,$l6};$n6=q#/lib/named.b#;$o6=bless({$Y4,$j6,$T5,$P,$U5,$q,$V5,$m6,$K,$n6},$g6);$p6=q#lib/tag#;$q6={};$r6=q#namespace#;$s6=q#'ni'#;$t6=bless({$w,$s6},$A);$u6={$r6,$t6};$v6=q#/lib/named_in_ni.b#;$w6=bless({$Y4,$q6,$T5,$q,$U5,$q,$V5,$u6,$K,$v6},$g6);$x6={};$y6=q#package#;$z6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$A6=bless({$w,$z6},$A);$B6={$y6,$A6};$C6=q#/lib/namespaced.b#;$D6=bless({$Y4,$x6,$T5,$q,$U5,$q,$V5,$B6,$K,$C6},$g6);$E6={};$F6=q#resolve#;$G6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$H6=bless({$w,$G6},$A);$I6={$F6,$H6};$J6=q#/lib/resolver.b#;$K6=bless({$Y4,$E6,$T5,$q,$U5,$q,$V5,$I6,$K,$J6},$g6);$L6=[$h6,$o6,$w6,$D6,$K6];$M6=bless({$K,$R5,$M5,$L6},$p6);$N6={};$O6=q#my $s = shift; $s->apply($s->package)#;$P6=bless({$w,$O6,$y,$z},$A);$Q6=q#instantiate#;$R6=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$S6=bless({$w,$R6},$A);$T6={$Q6,$S6};$U6=q#/lib/class_init.b#;$V6=bless({$Y4,$N6,$T5,$P6,$U5,$q,$V5,$T6,$K,$U6},$g6);$W6=q#io/buffer#;$X6=q#io/cat#;$Y6=q#io/exec#;$Z6=q#io/fd#;$c7=q#io/file#;$d7=q#io/file_update_fd#;$e7=q#io/null#;$f7=q#io/object#;$g7=q#io/pid#;$h7=q#io/str#;$i7=q#io/transfer#;$j7=q#io/transfer_async#;$k7=q#io/transfer_sync#;$l7=q#lib/behavior#;$m7=q#lib/dataslice#;$n7=q#lib/future#;$o7=q#lib/image#;$p7=q#lib/ni#;$q7=q#lib/quote_simple#;$r7=q#lib/test_assert_eq#;$s7=q#lib/test_assertion#;$t7=q#lib/test_value#;$u7=q#object#;$v7=q#semantic/task#;$w7={$Z4,1,$c5,1,$W6,1,$d5,1,$X6,1,$e5,1,$Y6,1,$f5,1,$Z6,1,$g5,1,$c7,1,$h5,1,$d7,1,$i5,1,$e7,1,$j5,1,$f7,1,$k5,1,$g7,1,$l5,1,$h7,1,$m5,1,$i7,1,$n5,1,$j7,1,$o5,1,$k7,1,$p5,1,$l7,1,$q5,1,$i6,1,$r5,1,$m7,1,$s5,1,$M,1,$t5,1,$A,1,$u5,1,$n7,1,$v5,1,$o7,1,$w5,1,$p7,1,$x5,1,$q7,1,$y5,1,$g6,1,$z5,1,$p6,1,$A5,1,$r7,1,$B5,1,$s7,1,$C5,1,$C,1,$D5,1,$t7,1,$E5,1,$N5,1,$F5,1,$O5,1,$G5,1,$u7,1,$H5,1,$I5,1,$J5,1,$v7,1,$K5,1};$x7=q#/object#;$y7={};$z7=q#DESTROY#;$A7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$B7=bless({$w,$A7},$A);$C7=q#ni 'ni:/' . ref shift#;$D7=bless({$w,$C7},$A);$E7={$z7,$B7,$Z4,$D7};$F7=q#/lib/instance.b#;$G7=bless({$Y4,$y7,$T5,$q,$U5,$q,$V5,$E7,$K,$F7},$g6);$H7=[$G7];$I7=bless({$Y4,$w7,$K,$x7,$M5,$H7},$H5);$J7={$Z4,1,$c5,1,$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$l7,1,$q5,1,$i6,1,$r5,1,$m7,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$g6,1,$z5,1,$p6,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$N5,1,$F5,1,$O5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1};$K7=q#/lib/behavior#;$L7={};$M7=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$N7=bless({$w,$M7},$A);$O7={$e,$N7};$P7=q#/lib/documentable.b#;$Q7=bless({$Y4,$L7,$T5,$q,$U5,$q,$V5,$O7,$K,$P7},$g6);$R7=[$I7,$Q7];$S7=bless({$Y4,$J7,$K,$K7,$M5,$R7},$q5);$T7={$Z4,1,$c5,1,$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$i6,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$N5,1,$F5,1,$O5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1};$U7=q#/lib/definition.b#;$V7={};$W7=q#def#;$X7=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$Y7=bless({$w,$X7},$A);$Z7={$W7,$Y7};$c8=q#/lib/definition_def.b#;$d8=bless({$Y4,$V7,$T5,$q,$U5,$q,$V5,$Z7,$K,$c8},$g6);$e8={};$f8=q#ro#;$g8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$h8=bless({$w,$g8},$A);$i8=q#rw#;$j8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$k8=bless({$w,$j8},$A);$l8={$f8,$h8,$i8,$k8};$m8=q#/lib/accessor.b#;$n8=bless({$Y4,$e8,$T5,$q,$U5,$q,$V5,$l8,$K,$m8},$g6);$o8={};$p8=q#(""#;$q8=q#shift->name#;$r8=bless({$w,$q8},$A);$s8={$p8,$r8};$t8=q#/lib/name_as_string.b#;$u8=bless({$Y4,$o8,$T5,$q,$U5,$q,$V5,$s8,$K,$t8},$g6);$v8={};$w8=q#(eq#;$x8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$y8=bless({$w,$x8},$A);$z8={$w8,$y8};$A8=q#/lib/ref_eq.b#;$B8=bless({$Y4,$v8,$T5,$q,$U5,$q,$V5,$z8,$K,$A8},$g6);$C8={};$D8=q#defdata#;$E8=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$F8=bless({$w,$E8},$A);$G8={$D8,$F8};$H8=q#/lib/definition_defdata.b#;$I8=bless({$Y4,$C8,$T5,$q,$U5,$q,$V5,$G8,$K,$H8},$g6);$J8=[$d8,$n8,$u8,$B8,$I8];$K8=bless({$Y4,$T7,$K,$U7,$M5,$J8},$i6);$L8=[$M6,$V6,$I7,$S7,$K8];$M8=bless({$Y4,$P5,$K,$Q5,$M5,$L8},$G5);$N8={};$O8=q#new#;$P8=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$Q8=bless({$w,$P8},$A);$R8={$O8,$Q8};$S8=q#/lib/instantiable.b#;$T8=bless({$Y4,$N8,$V5,$R8,$K,$S8},$g6);$U8={};$V8=q#child#;$W8=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$X8=bless({$w,$W8},$A);$Y8={$V8,$X8};$Z8=q#/lib/subclass.b#;$c9=bless({$Y4,$U8,$T5,$q,$U5,$q,$V5,$Y8,$K,$Z8},$g6);$d9=[$M8,$T8,$V6,$M8,$c9];$e9=bless({$Y4,$L5,$K,$L,$M5,$d9},$c5);$f9=q#ni:/class.c#;$g9={$c5,1,$J5,1};$h9=q#/class.c#;$i9={$c5,1,$G5,1,$J5,1};$j9=q#/module.c#;$k9={$c5,1,$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$G5,1,$H5,1,$J5,1,$K5,1};$l9=q#/object.c#;$m9={};$n9=q#instantiate_with_defaults#;$o9=q#my ($class, @slots) = @_;
my $generator = pop @slots;
$class->def("$$class{name}_init.b",
  instantiate => fn->closure(
    generator => $generator,
    '@slots'  => \\@slots,
    q{
      my %defaults = &$generator(@_);
      my $class    = shift;
      my %args     = @_;
      $defaults{$_} = $args{$_} for @slots;
      \\%defaults;
    }));#;$p9=bless({$w,$o9},$A);$q9={$n9,$p9};$r9=q#/lib/definition_init_meta.b#;$s9=bless({$Y4,$m9,$T5,$q,$U5,$q,$V5,$q9,$K,$r9},$g6);$t9=[$e9,$s9];$u9=bless({$Y4,$k9,$K,$l9,$M5,$t9},$N5);$v9={$c5,1,$q5,1,$r5,1,$s5,1,$z5,1,$A5,1,$G5,1,$J5,1};$w9=q#/lib/behavior.c#;$x9=[$u9];$y9=bless({$Y4,$v9,$K,$w9,$M5,$x9},$N5);$z9=[$u9,$T8,$y9];$A9=bless({$Y4,$i9,$K,$j9,$M5,$z9},$N5);$B9=[$A9];$C9=bless({$Y4,$g9,$K,$h9,$M5,$B9},$N5);$D9=q#ni:/io/buffer#;$E9={$W6,1};$F9={$W6,1,$X6,1,$Y6,1,$Z6,1,$c7,1,$d7,1,$e7,1,$f7,1,$g7,1,$h7,1};$G9=q#/io/object#;$H9={};$I9=q#(bool#;$J9=[];$K9=bless({$t,$J9,$v,$q,$w,1,$y,$z},$A);$L9={$I9,$K9};$M9=q#/io/object_ops.b#;$N9=bless({$Y4,$H9,$T5,$q,$U5,$q,$V5,$L9,$K,$M9},$g6);$O9={};$P9=q#die#;$Q9=[];$R9=q#shift; die join " ", @_#;$S9=bless({$t,$Q9,$v,$q,$w,$R9,$y,$z},$A);$T9=q#io_check#;$U9=[];$V9=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$W9=bless({$t,$U9,$v,$q,$w,$V9,$y,$z},$A);$X9=q#io_check_defined#;$Y9=[];$Z9=q#shift->io_check(sub {defined shift}, @_)#;$ca=bless({$t,$Y9,$v,$q,$w,$Z9,$y,$z},$A);$da=q#io_check_true#;$ea=[];$fa=q#shift->io_check(sub {shift}, @_)#;$ga=bless({$t,$ea,$v,$q,$w,$fa,$y,$z},$A);$ha={$P9,$S9,$T9,$W9,$X9,$ca,$da,$ga};$ia=q#/io/object_checks.b#;$ja=bless({$Y4,$O9,$T5,$q,$U5,$q,$V5,$ha,$K,$ia},$g6);$ka={};$la=q#(+#;$ma=[];$na=q#ni('ni:/io/cat')->new(@_[0, 1])#;$oa=bless({$t,$ma,$v,$q,$w,$na,$y,$z},$A);$pa={$la,$oa};$qa=q#/io/object_constructors.b#;$ra=bless({$Y4,$ka,$T5,$q,$U5,$q,$V5,$pa,$K,$qa},$g6);$sa={};$ta=q#read_all#;$ua=[];$va=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$wa=bless({$t,$ua,$v,$q,$w,$va,$y,$z},$A);$xa=q#write_all#;$ya=[];$za=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$Aa=bless({$t,$ya,$v,$q,$w,$za,$y,$z},$A);$Ba={$ta,$wa,$xa,$Aa};$Ca=q#/io/object_memory.b#;$Da=bless({$Y4,$sa,$T5,$q,$U5,$q,$V5,$Ba,$K,$Ca},$g6);$Ea={};$Fa=q#connect_sync#;$Ga=[];$Ha=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$Ia=bless({$t,$Ga,$v,$q,$w,$Ha,$y,$z},$A);$Ja=q#into_sync#;$Ka=[];$La=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$Ma=bless({$t,$Ka,$v,$q,$w,$La,$y,$z},$A);$Na={$Fa,$Ia,$Ja,$Ma};$Oa=q#/io/object_transfer_sync.b#;$Pa=bless({$Y4,$Ea,$T5,$q,$U5,$q,$V5,$Na,$K,$Oa},$g6);$Qa={};$Ra=q#connect_async#;$Sa=[];$Ta=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$Ua=bless({$t,$Sa,$v,$q,$w,$Ta,$y,$z},$A);$Va=q#into_async#;$Wa=[];$Xa=q#ni('ni:/io/transfer_async')->new(@_)->run#;$Ya=bless({$t,$Wa,$v,$q,$w,$Xa,$y,$z},$A);$Za={$Ra,$Ua,$Va,$Ya};$cb=q#/io/object_transfer_async.b#;$db=bless({$Y4,$Qa,$T5,$q,$U5,$q,$V5,$Za,$K,$cb},$g6);$eb=[$I7,$N9,$ja,$ra,$Da,$Pa,$db,$db,$Pa];$fb=bless({$Y4,$F9,$K,$G9,$M5,$eb},$k5);$gb={};$hb=[];$ib=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$jb=bless({$t,$hb,$v,$q,$w,$ib,$y,$z},$A);$kb={$Q6,$jb};$lb=q#/io/buffer_init.b#;$mb=bless({$Y4,$gb,$T5,$q,$U5,$q,$V5,$kb,$K,$lb},$g6);$nb={};$ob=q#read#;$pb=[];$qb=q#my $self = shift;
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
}#;$rb=bless({$t,$pb,$v,$q,$w,$qb,$y,$z},$A);$sb=q#read_capacity#;$tb=[];$ub=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$vb=bless({$t,$tb,$v,$q,$w,$ub,$y,$z},$A);$wb=q#write#;$xb=[];$yb=q#my $self = shift;
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
}#;$zb=bless({$t,$xb,$v,$q,$w,$yb,$y,$z},$A);$Ab=q#write_capacity#;$Bb=[];$Cb=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Db=bless({$t,$Bb,$v,$q,$w,$Cb,$y,$z},$A);$Eb={$ob,$rb,$sb,$vb,$wb,$zb,$Ab,$Db};$Fb=q#/io/buffer_io.b#;$Gb=bless({$Y4,$nb,$T5,$q,$U5,$q,$V5,$Eb,$K,$Fb},$g6);$Hb=[$fb,$mb,$Gb];$Ib=bless({$Y4,$E9,$K,$j1,$M5,$Hb},$d5);$Jb=q#ni:/io/buffer.c#;$Kb={$d5,1};$Lb=q#/io/buffer.c#;$Mb={$d5,1,$e5,1,$f5,1,$g5,1,$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1};$Nb=q#/io/object.c#;$Ob={};$Pb=q#def_transfer_method#;$Qb=[];$Rb=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Sb=bless({$t,$Qb,$v,$q,$w,$Rb,$y,$z},$A);$Tb={$Pb,$Sb};$Ub=q#/io/object.c_transfer_def.b#;$Vb=bless({$Y4,$Ob,$T5,$q,$U5,$q,$V5,$Tb,$K,$Ub},$g6);$Wb=[$u9,$Vb];$Xb=bless({$Y4,$Mb,$K,$Nb,$M5,$Wb},$N5);$Yb=[$Xb];$Zb=bless({$Y4,$Kb,$K,$Lb,$M5,$Yb},$N5);$cc=q#ni:/io/buffer_init.b#;$dc=q#ni:/io/buffer_io.b#;$ec=q#ni:/io/cat#;$fc={$X6,1};$gc={};$hc=[];$ic=q#shift; +{fs => [@_]}#;$jc=bless({$t,$hc,$v,$q,$w,$ic,$y,$z},$A);$kc={$Q6,$jc};$lc=q#/io/cat_init.b#;$mc=bless({$Y4,$gc,$T5,$q,$U5,$q,$V5,$kc,$K,$lc},$g6);$nc={};$oc=[];$pc=q#my $fs = shift->{fs};
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
$total_read;#;$qc=bless({$t,$oc,$v,$q,$w,$pc,$y,$z},$A);$rc={$ob,$qc};$sc=q#/io/cat_read.b#;$tc=bless({$Y4,$nc,$T5,$q,$U5,$q,$V5,$rc,$K,$sc},$g6);$uc=[$fb,$mc,$tc];$vc=bless({$Y4,$fc,$K,$w1,$M5,$uc},$e5);$wc=q#ni:/io/cat.c#;$xc={$e5,1};$yc=q#/io/cat.c#;$zc=[$Xb];$Ac=bless({$Y4,$xc,$K,$yc,$M5,$zc},$N5);$Bc=q#ni:/io/cat_init.b#;$Cc=q#ni:/io/cat_read.b#;$Dc=q#ni:/io/exec#;$Ec={$Y6,1};$Fc={};$Gc=q#argv#;$Hc=[];$Ic=q#shift->{'argv'}#;$Jc=bless({$t,$Hc,$v,$q,$w,$Ic,$y,$z},$A);$Kc={$Gc,$Jc};$Lc=q#/io/exec_ro.b#;$Mc=bless({$Y4,$Fc,$T5,$q,$U5,$q,$V5,$Kc,$K,$Lc},$g6);$Nc={};$Oc=[];$Pc=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Qc=bless({$t,$Oc,$v,$q,$w,$Pc,$y,$z},$A);$Rc={$Q6,$Qc};$Sc=q#/io/exec_init.b#;$Tc=bless({$Y4,$Nc,$T5,$q,$U5,$q,$V5,$Rc,$K,$Sc},$g6);$Uc={};$Vc=q#connect#;$Wc=[];$Xc=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Yc=bless({$t,$Wc,$v,$q,$w,$Xc,$y,$z},$A);$Zc=q#in_pipe#;$cd=[];$dd=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$ed=bless({$t,$cd,$v,$q,$w,$dd,$y,$z},$A);$fd=q#out_pipe#;$gd=[];$hd=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$id=bless({$t,$gd,$v,$q,$w,$hd,$y,$z},$A);$jd=q#setup_stdio#;$kd=[];$ld=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$md=bless({$t,$kd,$v,$q,$w,$ld,$y,$z},$A);$nd={$Vc,$Yc,$Zc,$ed,$fd,$id,$jd,$md};$od=q#/io/exec_io_setup.b#;$pd=bless({$Y4,$Uc,$T5,$q,$U5,$q,$V5,$nd,$K,$od},$g6);$qd={};$rd=q#binds_fd#;$sd=[];$td=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$ud=bless({$t,$sd,$v,$q,$w,$td,$y,$z},$A);$vd=q#fd#;$wd=[];$xd=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$yd=bless({$t,$wd,$v,$q,$w,$xd,$y,$z},$A);$zd=q#stderr#;$Ad=[];$Bd=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Cd=bless({$t,$Ad,$v,$q,$w,$Bd,$y,$z},$A);$Dd=q#stdin#;$Ed=[];$Fd=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Gd=bless({$t,$Ed,$v,$q,$w,$Fd,$y,$z},$A);$Hd=q#stdout#;$Id=[];$Jd=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Kd=bless({$t,$Id,$v,$q,$w,$Jd,$y,$z},$A);$Ld={$rd,$ud,$vd,$yd,$zd,$Cd,$Dd,$Gd,$Hd,$Kd};$Md=q#/io/exec_io_accessors.b#;$Nd=bless({$Y4,$qd,$T5,$q,$U5,$q,$V5,$Ld,$K,$Md},$g6);$Od={};$Pd=q#env#;$Qd=[];$Rd=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Sd=bless({$t,$Qd,$v,$q,$w,$Rd,$y,$z},$A);$Td={$Pd,$Sd};$Ud=q#/io/exec_env.b#;$Vd=bless({$Y4,$Od,$T5,$q,$U5,$q,$V5,$Td,$K,$Ud},$g6);$Wd={};$Xd=q#exec#;$Yd=[];$Zd=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$ce=bless({$t,$Yd,$v,$q,$w,$Zd,$y,$z},$A);$de=q#fork#;$ee=[];$fe=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$ge=bless({$t,$ee,$v,$q,$w,$fe,$y,$z},$A);$he=q#move_fds#;$ie=[];$je=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$ke=bless({$t,$ie,$v,$q,$w,$je,$y,$z},$A);$le={$Xd,$ce,$de,$ge,$he,$ke};$me=q#/io/exec_fork.b#;$ne=bless({$Y4,$Wd,$T5,$q,$U5,$q,$V5,$le,$K,$me},$g6);$oe=[$fb,$Mc,$Tc,$pd,$Nd,$Vd,$ne];$pe=bless({$Y4,$Ec,$K,$J1,$M5,$oe},$f5);$qe=q#ni:/io/exec.c#;$re={$f5,1};$se=q#/io/exec.c#;$te=[$Xb];$ue=bless({$Y4,$re,$K,$se,$M5,$te},$N5);$ve=q#ni:/io/exec_env.b#;$we=q#ni:/io/exec_fork.b#;$xe=q#ni:/io/exec_init.b#;$ye=q#ni:/io/exec_io_accessors.b#;$ze=q#ni:/io/exec_io_setup.b#;$Ae=q#ni:/io/exec_ro.b#;$Be=q#ni:/io/fd#;$Ce={$Z6,1};$De=q#read_fd_mask#;$Ee={};$Fe=[];$Ge=q#shift->{'fd'}#;$He=bless({$t,$Fe,$v,$q,$w,$Ge,$y,$z},$A);$Ie={$vd,$He};$Je=q#/io/fd_readers.b#;$Ke=bless({$Y4,$Ee,$T5,$q,$U5,$q,$V5,$Ie,$K,$Je},$g6);$Le={};$Me=[];$Ne=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Oe=bless({$t,$Me,$v,$q,$w,$Ne,$y,$z},$A);$Pe={$Q6,$Oe};$Qe=q#/io/fd_init.b#;$Re=bless({$Y4,$Le,$T5,$q,$U5,$q,$V5,$Pe,$K,$Qe},$g6);$Se={};$Te=q#be#;$Ue=[];$Ve=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$self->close_perl_ios;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$We=bless({$t,$Ue,$v,$q,$w,$Ve,$y,$z},$A);$Xe={$Te,$We};$Ye=q#/io/fd_shell.b#;$Ze=bless({$Y4,$Se,$T5,$q,$U5,$q,$V5,$Xe,$K,$Ye},$g6);$cf={};$df=q#cloexec#;$ef=[];$ff=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$gf=bless({$t,$ef,$v,$q,$w,$ff,$y,$z},$A);$hf=q#fcntl_flag#;$if=[];$jf=q#my ($self, $flag, $value) = @_;
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
}#;$kf=bless({$t,$if,$v,$q,$w,$jf,$y,$z},$A);$lf=q#nonblock#;$mf=[];$nf=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$of=bless({$t,$mf,$v,$q,$w,$nf,$y,$z},$A);$pf={$df,$gf,$hf,$kf,$lf,$of};$qf=q#/io/fd_fcntl.b#;$rf=bless({$Y4,$cf,$T5,$q,$U5,$q,$V5,$pf,$K,$qf},$g6);$sf={};$tf=[];$uf=q#shift->close#;$vf=bless({$t,$tf,$v,$q,$w,$uf,$y,$z},$A);$wf=q#close#;$xf=[];$yf=q#my $self = shift;
if (defined $$self{fd}) {
  $self->close_perl_ios;
  POSIX::close $$self{fd};
  $$self{fd} = undef;
}
$self;#;$zf=bless({$t,$xf,$v,$q,$w,$yf,$y,$z},$A);$Af={$wf,$zf};$Bf=q#/io/fd_gc.b#;$Cf=bless({$Y4,$sf,$T5,$q,$U5,$vf,$V5,$Af,$K,$Bf},$g6);$Df={};$Ef=q#close_perl_ios#;$Ff=[];$Gf=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
ni('ni:/io/fd')->clear_fd($$self{fd});
$$self{rfh} = $$self{wfh} = undef;
$self;#;$Hf=bless({$t,$Ff,$v,$q,$w,$Gf,$y,$z},$A);$If=[];$Jf=q#my $self = shift;
unless ($$self{rfh}) {
  open $$self{rfh}, "<&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->read_fd($$self{fd});
}
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Kf=bless({$t,$If,$v,$q,$w,$Jf,$y,$z},$A);$Lf=[];$Mf=q#my $self = shift;
unless ($$self{wfh}) {
  open $$self{wfh}, ">&=$$self{fd}" or return undef;
  ni('ni:/io/fd')->write_fd($$self{fd});
}
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Nf=bless({$t,$Lf,$v,$q,$w,$Mf,$y,$z},$A);$Of={$Ef,$Hf,$ob,$Kf,$wb,$Nf};$Pf=q#/io/fd_perlio.b#;$Qf=bless({$Y4,$Df,$T5,$q,$U5,$q,$V5,$Of,$K,$Pf},$g6);$Rf=[$fb,$Ke,$Re,$Ze,$rf,$Cf,$Qf];$Sf=q#write_fd_mask#;$Tf=bless({$Y4,$Ce,$K,$W1,$De,$z,$M5,$Rf,$Sf,$z},$g5);$Uf=[];$Vf=q#my $self = shift;
$$self{read_fd_mask} = '';
$$self{write_fd_mask} = '';#;$Wf=bless({$t,$Uf,$v,$q,$w,$Vf,$y,$z},$A);$Xf=q#ni:/io/fd.c#;$Yf={$g5,1};$Zf=q#/io/fd.c#;$cg={};$dg=q#clear_fd#;$eg=[];$fg=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 0;
vec($$self{write_fd_mask}, $fd, 1) = 0;#;$gg=bless({$t,$eg,$v,$q,$w,$fg,$y,$z},$A);$hg=q#read_fd#;$ig=[];$jg=q#my ($self, $fd) = @_;
vec($$self{read_fd_mask}, $fd, 1) = 1;#;$kg=bless({$t,$ig,$v,$q,$w,$jg,$y,$z},$A);$lg=q#select#;$mg=[];$ng=q#my ($self, $timeout) = @_;
my $n = select my $rbits = $$self{read_fd_mask},
               my $wbits = $$self{write_fd_mask},
               my $ebits = $$self{read_fd_mask} | $$self{write_fd_mask},
               $timeout || 0;
wantarray ? ($n, $rbits, $wbits) : $n;#;$og=bless({$t,$mg,$v,$q,$w,$ng,$y,$z},$A);$pg=q#write_fd#;$qg=[];$rg=q#my ($self, $fd) = @_;
vec($$self{write_fd_mask}, $fd, 1) = 1;#;$sg=bless({$t,$qg,$v,$q,$w,$rg,$y,$z},$A);$tg={$dg,$gg,$hg,$kg,$lg,$og,$pg,$sg};$ug=q#/io/fd.c_selector.b#;$vg=bless({$Y4,$cg,$T5,$Wf,$U5,$q,$V5,$tg,$K,$ug},$g6);$wg=[$Xb,$vg];$xg=bless({$Y4,$Yf,$K,$Zf,$M5,$wg},$N5);$yg=q#ni:/io/fd.c_selector.b#;$zg=q#ni:/io/fd_fcntl.b#;$Ag=q#ni:/io/fd_gc.b#;$Bg=q#ni:/io/fd_init.b#;$Cg=q#ni:/io/fd_perlio.b#;$Dg=q#ni:/io/fd_readers.b#;$Eg=q#ni:/io/fd_shell.b#;$Fg=q#ni:/io/file#;$Gg={$c7,1};$Hg={};$Ig=[];$Jg=q#shift->{'name'}#;$Kg=bless({$t,$Ig,$v,$q,$w,$Jg,$y,$z},$A);$Lg={$K,$Kg};$Mg=q#/io/file_readers.b#;$Ng=bless({$Y4,$Hg,$T5,$q,$U5,$q,$V5,$Lg,$K,$Mg},$g6);$Og={};$Pg=q#mode#;$Qg=[];$Rg=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$Sg=bless({$t,$Qg,$v,$q,$w,$Rg,$y,$z},$A);$Tg={$Pg,$Sg};$Ug=q#/io/file_accessors.b#;$Vg=bless({$Y4,$Og,$T5,$q,$U5,$q,$V5,$Tg,$K,$Ug},$g6);$Wg={};$Xg=[];$Yg=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Zg=bless({$t,$Xg,$v,$q,$w,$Yg,$y,$z},$A);$ch={$Q6,$Zg};$dh=q#/io/file_init.b#;$eh=bless({$Y4,$Wg,$T5,$q,$U5,$q,$V5,$ch,$K,$dh},$g6);$fh={};$gh=q#(-X#;$hh=[];$ih=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$jh=bless({$t,$hh,$v,$q,$w,$ih,$y,$z},$A);$kh=q#mv#;$lh=[];$mh=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$nh=bless({$t,$lh,$v,$q,$w,$mh,$y,$z},$A);$oh=q#rm#;$ph=[];$qh=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$rh=bless({$t,$ph,$v,$q,$w,$qh,$y,$z},$A);$sh={$gh,$jh,$kh,$nh,$oh,$rh};$th=q#/io/file_fns.b#;$uh=bless({$Y4,$fh,$T5,$q,$U5,$q,$V5,$sh,$K,$th},$g6);$vh={};$wh=q#atomic_update#;$xh=[];$yh=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$zh=bless({$t,$xh,$v,$q,$w,$yh,$y,$z},$A);$Ah={$wh,$zh};$Bh=q#/io/file_update.b#;$Ch=bless({$Y4,$vh,$T5,$q,$U5,$q,$V5,$Ah,$K,$Bh},$g6);$Dh={};$Eh=[];$Fh=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Gh=bless({$t,$Eh,$v,$q,$w,$Fh,$y,$z},$A);$Hh=q#r#;$Ih=[];$Jh=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Kh=bless({$t,$Ih,$v,$q,$w,$Jh,$y,$z},$A);$Lh=[];$Mh=q#shift->r->read(@_)#;$Nh=bless({$t,$Lh,$v,$q,$w,$Mh,$y,$z},$A);$Oh=q#w#;$Ph=[];$Qh=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Rh=bless({$t,$Ph,$v,$q,$w,$Qh,$y,$z},$A);$Sh=[];$Th=q#shift->w->write(@_)#;$Uh=bless({$t,$Sh,$v,$q,$w,$Th,$y,$z},$A);$Vh={$wf,$Gh,$Hh,$Kh,$ob,$Nh,$Oh,$Rh,$wb,$Uh};$Wh=q#/io/file_io.b#;$Xh=bless({$Y4,$Dh,$T5,$q,$U5,$q,$V5,$Vh,$K,$Wh},$g6);$Yh=[$fb,$Ng,$Vg,$eh,$uh,$Ch,$Xh];$Zh=bless({$Y4,$Gg,$K,$u2,$M5,$Yh},$h5);$ci=q#ni:/io/file.c#;$di={$h5,1};$ei=q#/io/file.c#;$fi=[$Xb];$gi=bless({$Y4,$di,$K,$ei,$M5,$fi},$N5);$hi=q#ni:/io/file_accessors.b#;$ii=q#ni:/io/file_fns.b#;$ji=q#ni:/io/file_init.b#;$ki=q#ni:/io/file_io.b#;$li=q#ni:/io/file_readers.b#;$mi=q#ni:/io/file_update.b#;$ni=q#ni:/io/file_update_fd#;$oi={$d7,1};$pi={};$qi=[];$ri=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$si=bless({$t,$qi,$v,$q,$w,$ri,$y,$z},$A);$ti={$Q6,$si};$ui=q#/io/file_update_fd_init.b#;$vi=bless({$Y4,$pi,$T5,$q,$U5,$q,$V5,$ti,$K,$ui},$g6);$wi={};$xi=[];$yi=bless({$t,$xi,$v,$q,$w,$uf,$y,$z},$A);$zi=[];$Ai=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Bi=bless({$t,$zi,$v,$q,$w,$Ai,$y,$z},$A);$Ci={$wf,$Bi};$Di=q#/io/file_update_fd_gc.b#;$Ei=bless({$Y4,$wi,$T5,$q,$U5,$yi,$V5,$Ci,$K,$Di},$g6);$Fi=[$fb,$Ke,$rf,$Qf,$vi,$Ei];$Gi=bless({$Y4,$oi,$K,$A2,$M5,$Fi},$i5);$Hi=q#ni:/io/file_update_fd.c#;$Ii={$i5,1};$Ji=q#/io/file_update_fd.c#;$Ki=[$Xb];$Li=bless({$Y4,$Ii,$K,$Ji,$M5,$Ki},$N5);$Mi=q#ni:/io/file_update_fd_gc.b#;$Ni=q#ni:/io/file_update_fd_init.b#;$Oi=q#ni:/io/named_io_fns.b#;$Pi={};$Qi=q#fcntl#;$Ri=[];$Si=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Ti=bless({$t,$Ri,$v,$q,$w,$Si,$y,$z},$A);$Ui=[];$Vi=q#CORE::fork#;$Wi=bless({$t,$Ui,$v,$q,$w,$Vi,$y,$z},$A);$Xi=q#open2#;$Yi=[];$Zi=q#CORE::open $_[0], $_[1]#;$cj=bless({$t,$Yi,$v,$q,$w,$Zi,$y,$z},$A);$dj=q#rename#;$ej=[];$fj=q#CORE::rename $_[0], $_[1]#;$gj=bless({$t,$ej,$v,$q,$w,$fj,$y,$z},$A);$hj=q#unlink#;$ij=[];$jj=q#CORE::unlink @_#;$kj=bless({$t,$ij,$v,$q,$w,$jj,$y,$z},$A);$lj=q#waitpid#;$mj=[];$nj=q#CORE::waitpid $_[0], $_[1]#;$oj=bless({$t,$mj,$v,$q,$w,$nj,$y,$z},$A);$pj={$Qi,$Ti,$de,$Wi,$Xi,$cj,$dj,$gj,$hj,$kj,$lj,$oj};$qj=q#/io/named_io_fns.b#;$rj=bless({$Y4,$Pi,$T5,$q,$U5,$q,$V5,$pj,$K,$qj},$g6);$sj=q#main#;$tj=q#ni:/io/null#;$uj={$e7,1};$vj=q#/io/null#;$wj={};$xj=[];$yj=q#+{fd => undef}#;$zj=bless({$t,$xj,$v,$q,$w,$yj,$y,$z},$A);$Aj={$Q6,$zj};$Bj=q#/io/null_init.b#;$Cj=bless({$Y4,$wj,$T5,$q,$U5,$q,$V5,$Aj,$K,$Bj},$g6);$Dj={};$Ej=[];$Fj=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Gj=bless({$t,$Ej,$v,$q,$w,$Fj,$y,$z},$A);$Hj=[];$Ij=q#shift->fd->read(@_)#;$Jj=bless({$t,$Hj,$v,$q,$w,$Ij,$y,$z},$A);$Kj=[];$Lj=q#shift->fd->write(@_)#;$Mj=bless({$t,$Kj,$v,$q,$w,$Lj,$y,$z},$A);$Nj={$vd,$Gj,$ob,$Jj,$wb,$Mj};$Oj=q#/io/null_io.b#;$Pj=bless({$Y4,$Dj,$T5,$q,$U5,$q,$V5,$Nj,$K,$Oj},$g6);$Qj=[$fb,$Cj,$Pj];$Rj=bless({$Y4,$uj,$K,$vj,$M5,$Qj},$j5);$Sj=q#ni:/io/null.c#;$Tj={$j5,1};$Uj=q#/io/null.c#;$Vj=[$Xb];$Wj=bless({$Y4,$Tj,$K,$Uj,$M5,$Vj},$N5);$Xj=q#ni:/io/null_init.b#;$Yj=q#ni:/io/null_io.b#;$Zj=q#ni:/io/object#;$ck=q#ni:/io/object.c#;$dk=q#ni:/io/object.c_transfer_def.b#;$ek=q#ni:/io/object_checks.b#;$fk=q#ni:/io/object_constructors.b#;$gk=q#ni:/io/object_memory.b#;$hk=q#ni:/io/object_ops.b#;$ik=q#ni:/io/object_transfer_async.b#;$jk=q#ni:/io/object_transfer_sync.b#;$kk=q#ni:/io/pid#;$lk={$g7,1};$mk={};$nk=q#pid#;$ok=[];$pk=q#shift->{'pid'}#;$qk=bless({$t,$ok,$v,$q,$w,$pk,$y,$z},$A);$rk=q#status#;$sk=[];$tk=q#shift->{'status'}#;$uk=bless({$t,$sk,$v,$q,$w,$tk,$y,$z},$A);$vk={$nk,$qk,$rk,$uk};$wk=q#/io/pid_readers.b#;$xk=bless({$Y4,$mk,$T5,$q,$U5,$q,$V5,$vk,$K,$wk},$g6);$yk={};$zk=[];$Ak=q#shift->await#;$Bk=bless({$t,$zk,$v,$q,$w,$Ak,$y,$z},$A);$Ck=[];$Dk=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$Ek=bless({$t,$Ck,$v,$q,$w,$Dk,$y,$z},$A);$Fk={$Q6,$Ek};$Gk=q#/io/pid_init.b#;$Hk=bless({$Y4,$yk,$T5,$q,$U5,$Bk,$V5,$Fk,$K,$Gk},$g6);$Ik={};$Jk=q#await#;$Kk=[];$Lk=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$Mk=bless({$t,$Kk,$v,$q,$w,$Lk,$y,$z},$A);$Nk=q#running#;$Ok=[];$Pk=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$Qk=bless({$t,$Ok,$v,$q,$w,$Pk,$y,$z},$A);$Rk={$Jk,$Mk,$Nk,$Qk};$Sk=q#/io/pid_wait.b#;$Tk=bless({$Y4,$Ik,$T5,$q,$U5,$q,$V5,$Rk,$K,$Sk},$g6);$Uk={};$Vk=[];$Wk=q#shift->stdout->read(@_)#;$Xk=bless({$t,$Vk,$v,$q,$w,$Wk,$y,$z},$A);$Yk=[];$Zk=q#shift->stdin->write(@_)#;$cl=bless({$t,$Yk,$v,$q,$w,$Zk,$y,$z},$A);$dl={$ob,$Xk,$wb,$cl};$el=q#/io/pid_io.b#;$fl=bless({$Y4,$Uk,$T5,$q,$U5,$q,$V5,$dl,$K,$el},$g6);$gl={};$hl=[];$il=q#$_[0]->{external_fds}{$_[1]}#;$jl=bless({$t,$hl,$v,$q,$w,$il,$y,$z},$A);$kl=[];$ll=q#shift->fd(2)#;$ml=bless({$t,$kl,$v,$q,$w,$ll,$y,$z},$A);$nl=[];$ol=q#shift->fd(0)#;$pl=bless({$t,$nl,$v,$q,$w,$ol,$y,$z},$A);$ql=[];$rl=q#shift->fd(1)#;$sl=bless({$t,$ql,$v,$q,$w,$rl,$y,$z},$A);$tl={$vd,$jl,$zd,$ml,$Dd,$pl,$Hd,$sl};$ul=q#/io/pid_accessors.b#;$vl=bless({$Y4,$gl,$T5,$q,$U5,$q,$V5,$tl,$K,$ul},$g6);$wl=[$fb,$xk,$Hk,$Tk,$fl,$vl];$xl=bless({$Y4,$lk,$K,$X2,$M5,$wl},$l5);$yl=q#ni:/io/pid.c#;$zl={$l5,1};$Al=q#/io/pid.c#;$Bl=[$Xb];$Cl=bless({$Y4,$zl,$K,$Al,$M5,$Bl},$N5);$Dl=q#ni:/io/pid_accessors.b#;$El=q#ni:/io/pid_init.b#;$Fl=q#ni:/io/pid_io.b#;$Gl=q#ni:/io/pid_readers.b#;$Hl=q#ni:/io/pid_wait.b#;$Il=q#ni:/io/str#;$Jl={$h7,1};$Kl=q#/io/str#;$Ll={};$Ml=q#data#;$Nl=[];$Ol=q#shift->{'data'}#;$Pl=bless({$t,$Nl,$v,$q,$w,$Ol,$y,$z},$A);$Ql=q#end#;$Rl=[];$Sl=q#shift->{'end'}#;$Tl=bless({$t,$Rl,$v,$q,$w,$Sl,$y,$z},$A);$Ul=q#start#;$Vl=[];$Wl=q#shift->{'start'}#;$Xl=bless({$t,$Vl,$v,$q,$w,$Wl,$y,$z},$A);$Yl={$Ml,$Pl,$Ql,$Tl,$Ul,$Xl};$Zl=q#/io/str_ro.b#;$cm=bless({$Y4,$Ll,$T5,$q,$U5,$q,$V5,$Yl,$K,$Zl},$g6);$dm={};$em=[];$fm=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$gm=bless({$t,$em,$v,$q,$w,$fm,$y,$z},$A);$hm={$Q6,$gm};$im=q#/io/str_init.b#;$jm=bless({$Y4,$dm,$T5,$q,$U5,$q,$V5,$hm,$K,$im},$g6);$km={};$lm=[];$mm=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$nm=bless({$t,$lm,$v,$q,$w,$mm,$y,$z},$A);$om=q#remaining#;$pm=[];$qm=q#my $self = shift; $$self{end} - $$self{start}#;$rm=bless({$t,$pm,$v,$q,$w,$qm,$y,$z},$A);$sm=[];$tm=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$um=bless({$t,$sm,$v,$q,$w,$tm,$y,$z},$A);$vm={$ob,$nm,$om,$rm,$wb,$um};$wm=q#/io/str_io.b#;$xm=bless({$Y4,$km,$T5,$q,$U5,$q,$V5,$vm,$K,$wm},$g6);$ym=[$fb,$cm,$jm,$xm];$zm=bless({$Y4,$Jl,$K,$Kl,$M5,$ym},$m5);$Am=q#ni:/io/str.c#;$Bm={$m5,1};$Cm=q#/io/str.c#;$Dm=[$Xb];$Em=bless({$Y4,$Bm,$K,$Cm,$M5,$Dm},$N5);$Fm=q#ni:/io/str_init.b#;$Gm=q#ni:/io/str_io.b#;$Hm=q#ni:/io/str_ro.b#;$Im=q#ni:/io/transfer#;$Jm={$i7,1,$j7,1,$k7,1};$Km=q#/io/transfer#;$Lm={$i7,1,$j7,1,$k7,1,$v7,1};$Mm=q#/semantic/task#;$Nm={};$Om=[];$Pm=q#shift->{'outcome'}#;$Qm=bless({$t,$Om,$v,$q,$w,$Pm,$y,$z},$A);$Rm={$r,$Qm};$Sm=q#/semantic/task_ro.b#;$Tm=bless({$Y4,$Nm,$T5,$q,$U5,$q,$V5,$Rm,$K,$Sm},$g6);$Um={};$Vm=q#failure#;$Wm=[];$Xm=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Ym=bless({$t,$Wm,$v,$q,$w,$Xm,$y,$z},$A);$Zm=q#success#;$cn=[];$dn=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$en=bless({$t,$cn,$v,$q,$w,$dn,$y,$z},$A);$fn={$Vm,$Ym,$Zm,$en};$gn=q#/semantic/task_outcome.b#;$hn=bless({$Y4,$Um,$T5,$q,$U5,$q,$V5,$fn,$K,$gn},$g6);$in=[$I7,$Tm,$hn];$jn=bless({$Y4,$Lm,$K,$Mm,$M5,$in},$K5);$kn={};$ln=[];$mn=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$nn=bless({$t,$ln,$v,$q,$w,$mn,$y,$z},$A);$on=[];$pn=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$qn=bless({$t,$on,$v,$q,$w,$pn,$y,$z},$A);$rn=[];$sn=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$tn=bless({$t,$rn,$v,$q,$w,$sn,$y,$z},$A);$un={$ob,$qn,$wb,$tn};$vn=q#/io/transfer_io_interop.b#;$wn=bless({$Y4,$kn,$T5,$nn,$U5,$q,$V5,$un,$K,$vn},$g6);$xn={};$yn=q#pressure#;$zn=[];$An=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Bn=bless({$t,$zn,$v,$q,$w,$An,$y,$z},$A);$Cn=q#read_limit_throughput#;$Dn=[];$En=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Fn=bless({$t,$Dn,$v,$q,$w,$En,$y,$z},$A);$Gn=q#throughput#;$Hn=[];$In=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$Jn=bless({$t,$Hn,$v,$q,$w,$In,$y,$z},$A);$Kn=q#write_limit_throughput#;$Ln=[];$Mn=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Nn=bless({$t,$Ln,$v,$q,$w,$Mn,$y,$z},$A);$On={$yn,$Bn,$Cn,$Fn,$Gn,$Jn,$Kn,$Nn};$Pn=q#/io/transfer_io_measurement.b#;$Qn=bless({$Y4,$xn,$T5,$q,$U5,$q,$V5,$On,$K,$Pn},$g6);$Rn=[$jn,$wn,$Qn];$Sn=bless({$Y4,$Jm,$K,$Km,$M5,$Rn},$n5);$Tn=[];$Un=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Vn=bless({$t,$Tn,$v,$q,$w,$Un,$y,$z},$A);$Wn=q#ni:/io/transfer.c#;$Xn={$n5,1,$o5,1,$p5,1};$Yn=q#/io/transfer.c#;$Zn={$n5,1,$o5,1,$p5,1,$K5,1};$co=q#/semantic/task.c#;$do=[$u9];$eo=bless({$Y4,$Zn,$K,$co,$M5,$do},$N5);$fo={};$go={};$ho=q#/io/transfer.c_into.b#;$io=bless({$Y4,$fo,$T5,$Vn,$U5,$q,$V5,$go,$K,$ho},$g6);$jo=[$eo,$io];$ko=bless({$Y4,$Xn,$K,$Yn,$M5,$jo},$N5);$lo=q#ni:/io/transfer.c_into.b#;$mo=q#ni:/io/transfer_async#;$no={$j7,1};$oo=q#/io/transfer_async#;$po={};$qo=q#dest_io#;$ro=[];$so=q#shift->{'dest_io'}#;$to=bless({$t,$ro,$v,$q,$w,$so,$y,$z},$A);$uo=q#id#;$vo=[];$wo=q#shift->{'id'}#;$xo=bless({$t,$vo,$v,$q,$w,$wo,$y,$z},$A);$yo=q#source_io#;$zo=[];$Ao=q#shift->{'source_io'}#;$Bo=bless({$t,$zo,$v,$q,$w,$Ao,$y,$z},$A);$Co={$qo,$to,$uo,$xo,$yo,$Bo};$Do=q#/io/transfer_async_ro.b#;$Eo=bless({$Y4,$po,$T5,$q,$U5,$q,$V5,$Co,$K,$Do},$g6);$Fo={};$Go=[];$Ho=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Io=bless({$t,$Go,$v,$q,$w,$Ho,$y,$z},$A);$Jo={$Q6,$Io};$Ko=q#/io/transfer_async_init.b#;$Lo=bless({$Y4,$Fo,$T5,$q,$U5,$q,$V5,$Jo,$K,$Ko},$g6);$Mo={};$No=[];$Oo=q#ni('ni:/io/transfer_async')->track(shift)#;$Po=bless({$t,$No,$v,$q,$w,$Oo,$y,$z},$A);$Qo=[];$Ro=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$So=bless({$t,$Qo,$v,$q,$w,$Ro,$y,$z},$A);$To={};$Uo=q#/io/transfer_async_lifecycle.b#;$Vo=bless({$Y4,$Mo,$T5,$Po,$U5,$So,$V5,$To,$K,$Uo},$g6);$Wo={};$Xo=q#run#;$Yo=[];$Zo=q#shift#;$cp=bless({$t,$Yo,$v,$q,$w,$Zo,$y,$z},$A);$dp=q#run_async#;$ep=[];$fp=q#my $self = shift;
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

$self;#;$gp=bless({$t,$ep,$v,$q,$w,$fp,$y,$z},$A);$hp={$Xo,$cp,$dp,$gp};$ip=q#/io/transfer_async_run.b#;$jp=bless({$Y4,$Wo,$T5,$q,$U5,$q,$V5,$hp,$K,$ip},$g6);$kp=[$Sn,$Eo,$Lo,$Vo,$jp];$lp=q#tracked_transfers#;$mp={};$np=q#transfer_id#;$op=bless({$Y4,$no,$K,$oo,$M5,$kp,$lp,$mp,$np,0},$o5);$pp=[];$qp=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$rp=bless({$t,$pp,$v,$q,$w,$qp,$y,$z},$A);$sp=q#ni:/io/transfer_async.c#;$tp={$o5,1};$up=q#/io/transfer_async.c#;$vp={};$wp=q#new_id#;$xp=[];$yp=q#++shift->{transfer_id}#;$zp=bless({$t,$xp,$v,$q,$w,$yp,$y,$z},$A);$Ap=q#track#;$Bp=[];$Cp=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Dp=bless({$t,$Bp,$v,$q,$w,$Cp,$y,$z},$A);$Ep=q#untrack#;$Fp=[];$Gp=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Hp=bless({$t,$Fp,$v,$q,$w,$Gp,$y,$z},$A);$Ip={$wp,$zp,$Ap,$Dp,$Ep,$Hp};$Jp=q#/io/transfer_async.c_tracker.b#;$Kp=bless({$Y4,$vp,$T5,$rp,$U5,$q,$V5,$Ip,$K,$Jp},$g6);$Lp=[$ko,$Kp];$Mp=bless({$Y4,$tp,$K,$up,$M5,$Lp},$N5);$Np=q#ni:/io/transfer_async.c_tracker.b#;$Op=q#ni:/io/transfer_async_init.b#;$Pp=q#ni:/io/transfer_async_lifecycle.b#;$Qp=q#ni:/io/transfer_async_ro.b#;$Rp=q#ni:/io/transfer_async_run.b#;$Sp=q#ni:/io/transfer_io_interop.b#;$Tp=q#ni:/io/transfer_io_measurement.b#;$Up=q#ni:/io/transfer_sync#;$Vp={$k7,1};$Wp=q#/io/transfer_sync#;$Xp={};$Yp=[];$Zp=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$cq=bless({$t,$Yp,$v,$q,$w,$Zp,$y,$z},$A);$dq={$Q6,$cq};$eq=q#/io/transfer_sync_init.b#;$fq=bless({$Y4,$Xp,$T5,$q,$U5,$q,$V5,$dq,$K,$eq},$g6);$gq={};$hq=[];$iq=q#my $self = shift;
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
$self->success;#;$jq=bless({$t,$hq,$v,$q,$w,$iq,$y,$z},$A);$kq={$Xo,$jq};$lq=q#/io/transfer_sync_run.b#;$mq=bless({$Y4,$gq,$T5,$q,$U5,$q,$V5,$kq,$K,$lq},$g6);$nq=[$Sn,$fq,$mq];$oq=bless({$Y4,$Vp,$K,$Wp,$M5,$nq},$p5);$pq=q#ni:/io/transfer_sync.c#;$qq={$p5,1};$rq=q#/io/transfer_sync.c#;$sq=[$ko];$tq=bless({$Y4,$qq,$K,$rq,$M5,$sq},$N5);$uq=q#ni:/io/transfer_sync_init.b#;$vq=q#ni:/io/transfer_sync_run.b#;$wq=q#ni:/lib/accessor.b#;$xq=q#ni:/lib/behavior#;$yq=q#ni:/lib/behavior.c#;$zq=q#ni:/lib/branch#;$Aq={$i6,1};$Bq=q#/lib/branch#;$Cq={};$Dq=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Eq=bless({$w,$Dq},$A);$Fq={$Q6,$Eq};$Gq=q#/lib/branch_init.b#;$Hq=bless({$Y4,$Cq,$T5,$q,$U5,$q,$V5,$Fq,$K,$Gq},$g6);$Iq=[$S7,$o6,$h6,$Hq,$K8];$Jq=bless({$Y4,$Aq,$K,$Bq,$M5,$Iq},$r5);$Kq=q#ni:/lib/branch.b#;$Lq=q#ni:/lib/branch.c#;$Mq={$r5,1};$Nq=q#/lib/branch.c#;$Oq=[$y9];$Pq=bless({$Y4,$Mq,$K,$Nq,$M5,$Oq},$N5);$Qq=q#ni:/lib/branch_init.b#;$Rq=q#ni:/lib/class_init.b#;$Sq=q#ni:/lib/dataslice#;$Tq={$m7,1};$Uq=q#/lib/dataslice#;$Vq={};$Wq=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Xq=bless({$w,$Wq},$A);$Yq={$Q6,$Xq};$Zq=q#/lib/dataslice_init.b#;$cr=bless({$Y4,$Vq,$T5,$q,$U5,$q,$V5,$Yq,$K,$Zq},$g6);$dr={};$er=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$fr=bless({$w,$er},$A);$gr={$Z5,$fr};$hr=q#/lib/dataslice_apply.b#;$ir=bless({$Y4,$dr,$T5,$q,$U5,$q,$V5,$gr,$K,$hr},$g6);$jr=[$S7,$cr,$ir];$kr=bless({$Y4,$Tq,$K,$Uq,$M5,$jr},$s5);$lr=q#ni:/lib/dataslice.c#;$mr={$s5,1};$nr=q#/lib/dataslice.c#;$or=[$y9];$pr=bless({$Y4,$mr,$K,$nr,$M5,$or},$N5);$qr=q#ni:/lib/dataslice_apply.b#;$rr=q#ni:/lib/dataslice_init.b#;$sr=q#ni:/lib/definition.b#;$tr=q#ni:/lib/definition_def.b#;$ur=q#ni:/lib/definition_defdata.b#;$vr=q#ni:/lib/definition_init_meta.b#;$wr=q#ni:/lib/doc#;$xr={$M,1};$yr={};$zr=q#shift; +{name => shift, doc => []}#;$Ar=bless({$w,$zr},$A);$Br={$Q6,$Ar};$Cr=q#/lib/doc_init.b#;$Dr=bless({$Y4,$yr,$T5,$q,$U5,$q,$V5,$Br,$K,$Cr},$g6);$Er={};$Fr=q#'ni.doc'#;$Gr=bless({$w,$Fr},$A);$Hr={$r6,$Gr};$Ir=q#/lib/doc_namespace.b#;$Jr=bless({$Y4,$Er,$T5,$q,$U5,$q,$V5,$Hr,$K,$Ir},$g6);$Kr={};$Lr=q#AUTOLOAD#;$Mr=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, map $self->fix_indentation($_), @_];
$self;#;$Nr=bless({$w,$Mr},$A);$Or=q#fix_indentation#;$Pr=q#my ($self, $x) = @_;
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
join "\\n", @lines;#;$Qr=bless({$w,$Pr},$A);$Rr={$Lr,$Nr,$Or,$Qr};$Sr=q#/lib/doc_define.b#;$Tr=bless({$Y4,$Kr,$T5,$q,$U5,$q,$V5,$Rr,$K,$Sr},$g6);$Ur={};$Vr=q#shift->referent#;$Wr=bless({$w,$Vr},$A);$Xr=q#referent#;$Yr=q#ni 'ni:' . shift->{name}#;$Zr=bless({$w,$Yr},$A);$cs={$Ql,$Wr,$Xr,$Zr};$ds=q#/lib/doc_end.b#;$es=bless({$Y4,$Ur,$T5,$q,$U5,$q,$V5,$cs,$K,$ds},$g6);$fs={};$gs=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$hs=bless({$w,$gs},$A);$is=q#linearized#;$js=q#map @$_, @{shift->{doc}}#;$ks=bless({$w,$js},$A);$ls=q#tests#;$ms=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$ns=bless({$w,$ms},$A);$os={$D2,$hs,$is,$ks,$ls,$ns};$ps=q#/lib/doc_test.b#;$qs=bless({$Y4,$fs,$T5,$q,$U5,$q,$V5,$os,$K,$ps},$g6);$rs=[$I7,$o6,$Dr,$Jr,$Tr,$es,$qs];$ss=bless({$Y4,$xr,$K,$A3,$M5,$rs},$t5);$ts=q#ni:/lib/doc.c#;$us={$t5,1};$vs=q#/lib/doc.c#;$ws=[$u9];$xs=bless({$Y4,$us,$K,$vs,$M5,$ws},$N5);$ys=q#ni:/lib/doc_define.b#;$zs=q#ni:/lib/doc_end.b#;$As=q#ni:/lib/doc_init.b#;$Bs=q#ni:/lib/doc_namespace.b#;$Cs=q#ni:/lib/doc_test.b#;$Ds=q#ni:/lib/documentable.b#;$Es=q#ni:/lib/fn#;$Fs={$A,1};$Gs=q#/lib/fn#;$Hs={};$Is=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Js=bless({$w,$Is,$y,$z},$A);$Ks=q#my $self = shift;
return $$self{closure} unless @_;
$$self{closure} = {@_};
$self;#;$Ls=bless({$w,$Ks},$A);$Ms=q#compile#;$Ns=q#\# NB: everything here needs to happen in a single method; otherwise JIT
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
  print STDERR ref($closure), "\\n";
  my $closure_code = join ';',
    map /^([@%])/ ? "my$_=$1{\\${\\$_[0]}{'$_'}}"
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
}#;$Os=bless({$w,$Ns},$A);$Ps=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  closure     => undef,
  eval_number => undef,
  annotations => [@_]};#;$Qs=bless({$w,$Ps},$A);$Rs={$v,$Ls,$Ms,$Os,$Q6,$Qs};$Ss=q#/lib/fn_init.b#;$Ts=bless({$Y4,$Hs,$T5,$q,$U5,$Js,$V5,$Rs,$K,$Ss},$g6);$Us={};$Vs=[];$Ws=q#shift->{'annotations'}#;$Xs=bless({$t,$Vs,$v,$q,$w,$Ws,$y,$z},$A);$Ys=[];$Zs=q#shift->{'code'}#;$ct=bless({$t,$Ys,$v,$q,$w,$Zs,$y,$z},$A);$dt=q#eval_number#;$et=[];$ft=q#shift->{'eval_number'}#;$gt=bless({$t,$et,$v,$q,$w,$ft,$y,$z},$A);$ht=q#fn#;$it=[];$jt=q#shift->{'fn'}#;$kt=bless({$t,$it,$v,$q,$w,$jt,$y,$z},$A);$lt={$t,$Xs,$w,$ct,$dt,$gt,$ht,$kt};$mt=q#/lib/fn_ro.b#;$nt=bless({$Y4,$Us,$T5,$q,$U5,$q,$V5,$lt,$K,$mt},$g6);$ot={};$pt=[];$qt=q#my $self = shift; "fn {$$self{code}}"#;$rt=bless({$t,$pt,$v,$q,$w,$qt,$y,$z},$A);$st=[];$tt=bless({$t,$st,$v,$q,$w,$x8,$y,$z},$A);$ut={$p8,$rt,$w8,$tt};$vt=q#/lib/fn_ops.b#;$wt=bless({$Y4,$ot,$T5,$q,$U5,$q,$V5,$ut,$K,$vt},$g6);$xt={};$yt=q#serialize#;$zt=[];$At=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Bt=bless({$t,$zt,$v,$q,$w,$At,$y,$z},$A);$Ct={$yt,$Bt};$Dt=q#/lib/fn_serialize.b#;$Et=bless({$Y4,$xt,$T5,$q,$U5,$q,$V5,$Ct,$K,$Dt},$g6);$Ft=[$I7,$T8,$Ts,$nt,$wt,$Et];$Gt=bless({$Y4,$Fs,$K,$Gs,$M5,$Ft},$u5);$Ht=[];$It=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Jt=bless({$t,$Ht,$v,$q,$w,$It,$y,$z},$A);$Kt=q#ni:/lib/fn.c#;$Lt={$u5,1};$Mt=q#/lib/fn.c#;$Nt={};$Ot=[];$Pt=q#my $class = shift;
my $body  = pop;
$class->new($body)->closure(@_);#;$Qt=bless({$t,$Ot,$v,$q,$w,$Pt,$y,$z},$A);$Rt={$v,$Qt};$St=q#/lib/fn.c_closure.b#;$Tt=bless({$Y4,$Nt,$T5,$q,$U5,$q,$V5,$Rt,$K,$St},$g6);$Ut={};$Vt=q#resolve_evals#;$Wt=[];$Xt=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Yt=bless({$t,$Wt,$v,$q,$w,$Xt,$y,$z},$A);$Zt={$Vt,$Yt};$cu=q#/lib/fn.c_resolve_eval.b#;$du=bless({$Y4,$Ut,$T5,$Jt,$U5,$q,$V5,$Zt,$K,$cu},$g6);$eu=[$u9,$Tt,$du];$fu=bless({$Y4,$Lt,$K,$Mt,$M5,$eu},$N5);$gu=q#ni:/lib/fn.c_closure.b#;$hu=q#ni:/lib/fn.c_resolve_eval.b#;$iu=q#ni:/lib/fn_init.b#;$ju=q#ni:/lib/fn_ops.b#;$ku=q#ni:/lib/fn_ro.b#;$lu=q#ni:/lib/fn_serialize.b#;$mu=q#ni:/lib/future#;$nu={$n7,1};$ou={};$pu=[];$qu=bless({$t,$pu,$v,$q,$w,$Pm,$y,$z},$A);$ru=q#parents#;$su=[];$tu=q#shift->{'parents'}#;$uu=bless({$t,$su,$v,$q,$w,$tu,$y,$z},$A);$vu={$r,$qu,$ru,$uu};$wu=q#/lib/future_ro.b#;$xu=bless({$Y4,$ou,$T5,$q,$U5,$q,$V5,$vu,$K,$wu},$g6);$yu={};$zu=[];$Au=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Bu=bless({$t,$zu,$v,$q,$w,$Au,$y,$z},$A);$Cu={$Q6,$Bu};$Du=q#/lib/future_init.b#;$Eu=bless({$Y4,$yu,$T5,$q,$U5,$q,$V5,$Cu,$K,$Du},$g6);$Fu={};$Gu=q#decide#;$Hu=[];$Iu=q#local $_;
my $self = shift;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, @_];
$$self{v} = [@_];
defined && &$_(@_) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Ju=bless({$t,$Hu,$v,$q,$w,$Iu,$y,$z},$A);$Ku=q#decided#;$Lu=[];$Mu=q#shift->{outcome}#;$Nu=bless({$t,$Lu,$v,$q,$w,$Mu,$y,$z},$A);$Ou=q#listener#;$Pu=[];$Qu=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l(@{$$self{v}})
  : push @{$$self{listeners}}, $l;
$self;#;$Ru=bless({$t,$Pu,$v,$q,$w,$Qu,$y,$z},$A);$Su=q#v#;$Tu=[];$Uu=q#my $v = shift->{v};
return undef unless $v;
@$v;#;$Vu=bless({$t,$Tu,$v,$q,$w,$Uu,$y,$z},$A);$Wu={$Gu,$Ju,$Ku,$Nu,$Ou,$Ru,$Su,$Vu};$Xu=q#/lib/future_state.b#;$Yu=bless({$Y4,$Fu,$T5,$q,$U5,$q,$V5,$Wu,$K,$Xu},$g6);$Zu={};$cv=q#and#;$dv=[];$ev=q#my $self   = $_[0];
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
$child;#;$fv=bless({$t,$dv,$v,$q,$w,$ev,$y,$z},$A);$gv=q#flatmap#;$hv=[];$iv=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(@_)->listener(sub {$child->decide(@_)})});
$child;#;$jv=bless({$t,$hv,$v,$q,$w,$iv,$y,$z},$A);$kv=q#map#;$lv=[];$mv=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(@_))});
$child;#;$nv=bless({$t,$lv,$v,$q,$w,$mv,$y,$z},$A);$ov=q#or#;$pv=[];$qv=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(@_) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$rv=bless({$t,$pv,$v,$q,$w,$qv,$y,$z},$A);$sv={$cv,$fv,$gv,$jv,$kv,$nv,$ov,$rv};$tv=q#/lib/future_algebra.b#;$uv=bless({$Y4,$Zu,$T5,$q,$U5,$q,$V5,$sv,$K,$tv},$g6);$vv=[$I7,$xu,$Eu,$Yu,$uv];$wv=bless({$Y4,$nu,$K,$R3,$M5,$vv},$v5);$xv=q#ni:/lib/future.c#;$yv={$v5,1};$zv=q#/lib/future.c#;$Av=[$u9];$Bv=bless({$Y4,$yv,$K,$zv,$M5,$Av},$N5);$Cv=q#ni:/lib/future_algebra.b#;$Dv=q#ni:/lib/future_init.b#;$Ev=q#ni:/lib/future_ro.b#;$Fv=q#ni:/lib/future_state.b#;$Gv=q#ni:/lib/gensym_generator_compact.b#;$Hv={};$Iv=q#gensym#;$Jv=[];$Kv=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Lv=bless({$t,$Jv,$v,$q,$w,$Kv,$y,$z},$A);$Mv={$Iv,$Lv};$Nv=q#/lib/gensym_generator_compact.b#;$Ov=bless({$Y4,$Hv,$T5,$q,$U5,$q,$V5,$Mv,$K,$Nv},$g6);$Pv=q#ni:/lib/global_static_test.b#;$Qv={};$Rv=[];$Sv=q#ni('ni:/lib/test_case')->new(shift)#;$Tv=q#($)#;$Uv=bless({$t,$Rv,$v,$q,$w,$Sv,$y,$Tv},$A);$Vv=q#now#;$Wv=[];$Xv=q#ni('ni:/lib/test_value')->new(shift)#;$Yv=bless({$t,$Wv,$v,$q,$w,$Xv,$y,$Tv},$A);$Zv={$D2,$Uv,$Vv,$Yv};$cw=q#/lib/global_static_test.b#;$dw=bless({$Y4,$Qv,$T5,$q,$U5,$q,$V5,$Zv,$K,$cw},$g6);$ew=q#ni:/lib/image#;$fw={$o7,1};$gw={};$hw=[];$iw=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$jw=bless({$t,$hw,$v,$q,$w,$iw,$y,$z},$A);$kw={$Q6,$jw};$lw=q#/lib/image_init.b#;$mw=bless({$Y4,$gw,$T5,$q,$U5,$q,$V5,$kw,$K,$lw},$g6);$nw={};$ow=q#boot_side_effect#;$pw=[];$qw=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$rw=bless({$t,$pw,$v,$q,$w,$qw,$y,$z},$A);$sw=q#finalizer#;$tw=[];$uw=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$vw=bless({$t,$tw,$v,$q,$w,$uw,$y,$z},$A);$ww=q#io#;$xw=[];$yw=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$zw=bless({$t,$xw,$v,$q,$w,$yw,$y,$z},$A);$Aw=q#reconstruction#;$Bw=[];$Cw=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Dw=bless({$t,$Bw,$v,$q,$w,$Cw,$y,$z},$A);$Ew=q#side_effect#;$Fw=[];$Gw=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Hw=bless({$t,$Fw,$v,$q,$w,$Gw,$y,$z},$A);$Iw={$ow,$rw,$sw,$vw,$ww,$zw,$Aw,$Dw,$Ew,$Hw};$Jw=q#/lib/image_quoting.b#;$Kw=bless({$Y4,$nw,$T5,$q,$U5,$q,$V5,$Iw,$K,$Jw},$g6);$Lw={};$Mw=q#quote_code#;$Nw=[];$Ow=q#my ($self, $code) = @_;
my $message;
eval {
  require B::Deparse;
  $message = "can't quote perl native fn: "
           . B::Deparse->new->coderef2text($code);
};
die $message || "can't quote perl CODE refs (make B::Deparse available "
              . "for more info)";#;$Pw=bless({$t,$Nw,$v,$q,$w,$Ow,$y,$z},$A);$Qw={$Mw,$Pw};$Rw=q#/lib/quote_code_fail.b#;$Sw=bless({$Y4,$Lw,$T5,$q,$U5,$q,$V5,$Qw,$K,$Rw},$g6);$Tw={};$Uw=q#quote_array#;$Vw=[];$Ww=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Xw=bless({$t,$Vw,$v,$q,$w,$Ww,$y,$z},$A);$Yw=q#quote_hash#;$Zw=[];$cx=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$dx=bless({$t,$Zw,$v,$q,$w,$cx,$y,$z},$A);$ex=q#quote_scalar#;$fx=[];$gx=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$hx=bless({$t,$fx,$v,$q,$w,$gx,$y,$z},$A);$ix=q#quote_scalar_ref#;$jx=[];$kx=q#'\\\\' . shift->quote(${$_[0]})#;$lx=bless({$t,$jx,$v,$q,$w,$kx,$y,$z},$A);$mx=q#quote_value#;$nx=[];$ox=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$px=bless({$t,$nx,$v,$q,$w,$ox,$y,$z},$A);$qx={$Uw,$Xw,$Yw,$dx,$ex,$hx,$ix,$lx,$mx,$px};$rx=q#/lib/quote_values.b#;$sx=bless({$Y4,$Tw,$T5,$q,$U5,$q,$V5,$qx,$K,$rx},$g6);$tx={};$ux=q#quote_blessed#;$vx=[];$wx=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$xx=bless({$t,$vx,$v,$q,$w,$wx,$y,$z},$A);$yx=q#quote_class#;$zx=[];$Ax=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Bx=bless({$t,$zx,$v,$q,$w,$Ax,$y,$z},$A);$Cx=q#quote_object#;$Dx=[];$Ex=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$Fx=bless({$t,$Dx,$v,$q,$w,$Ex,$y,$z},$A);$Gx={$ux,$xx,$yx,$Bx,$Cx,$Fx};$Hx=q#/lib/quote_objects.b#;$Ix=bless({$Y4,$tx,$T5,$q,$U5,$q,$V5,$Gx,$K,$Hx},$g6);$Jx={};$Kx=q#circular_arrayref#;$Lx=[];$Mx=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Nx=bless({$t,$Lx,$v,$q,$w,$Mx,$y,$z},$A);$Ox=q#circular_hashref#;$Px=[];$Qx=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Rx=bless({$t,$Px,$v,$q,$w,$Qx,$y,$z},$A);$Sx=q#is_circular#;$Tx=[];$Ux=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Vx=bless({$t,$Tx,$v,$q,$w,$Ux,$y,$z},$A);$Wx={$Kx,$Nx,$Ox,$Rx,$Sx,$Vx};$Xx=q#/lib/quote_circular_addressed.b#;$Yx=bless({$Y4,$Jx,$T5,$q,$U5,$q,$V5,$Wx,$K,$Xx},$g6);$Zx={};$cy=q#address#;$dy=[];$ey=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$fy=bless({$t,$dy,$v,$q,$w,$ey,$y,$z},$A);$gy=q#allocate_gensym#;$hy=[];$iy=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$jy=bless({$t,$hy,$v,$q,$w,$iy,$y,$z},$A);$ky=q#circular_links#;$ly=[];$my=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$ny=bless({$t,$ly,$v,$q,$w,$my,$y,$z},$A);$oy=q#quote#;$py=[];$qy=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$ry=bless({$t,$py,$v,$q,$w,$qy,$y,$z},$A);$sy={$cy,$fy,$gy,$jy,$ky,$ny,$oy,$ry};$ty=q#/lib/quote_gensym_identity.b#;$uy=bless({$Y4,$Zx,$T5,$q,$U5,$q,$V5,$sy,$K,$ty},$g6);$vy=[$I7,$mw,$Kw,$Sw,$sx,$Ix,$Yx,$uy,$Ov];$wy=bless({$Y4,$fw,$K,$Z3,$M5,$vy},$w5);$xy=q#ni:/lib/image.c#;$yy={$w5,1};$zy=q#/lib/image.c#;$Ay=[$u9];$By=bless({$Y4,$yy,$K,$zy,$M5,$Ay},$N5);$Cy=q#ni:/lib/image_init.b#;$Dy=q#ni:/lib/image_quoting.b#;$Ey=q#ni:/lib/instance.b#;$Fy=q#ni:/lib/instantiable.b#;$Gy=q#ni:/lib/json.b#;$Hy={};$Iy=q#json_decode#;$Jy=[];$Ky=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Ly=bless({$t,$Jy,$v,$q,$w,$Ky,$y,$Tv},$A);$My=q#json_encode#;$Ny=[];$Oy=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Py=bless({$t,$Ny,$v,$q,$w,$Oy,$y,$Tv},$A);$Qy=q#json_encode_pretty#;$Ry=[];$Sy=q#local $_;
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

$spaces . ni::json_encode($v);#;$Ty=bless({$t,$Ry,$v,$q,$w,$Sy,$y,$z},$A);$Uy=q#json_escape#;$Vy=[];$Wy=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$Xy=bless({$t,$Vy,$v,$q,$w,$Wy,$y,$Tv},$A);$Yy=q#json_unescape#;$Zy=[];$cz=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$dz=bless({$t,$Zy,$v,$q,$w,$cz,$y,$Tv},$A);$ez=q#json_unescape_one#;$fz=[];$gz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$hz=bless({$t,$fz,$v,$q,$w,$gz,$y,$Tv},$A);$iz={$Iy,$Ly,$My,$Py,$Qy,$Ty,$Uy,$Xy,$Yy,$dz,$ez,$hz};$jz=q#/lib/json.b#;$kz=bless({$Y4,$Hy,$T5,$q,$U5,$q,$V5,$iz,$K,$jz},$g6);$lz=q#ni#;$mz=q#ni:/lib/name_as_string.b#;$nz=q#ni:/lib/named.b#;$oz=q#ni:/lib/named_in_ni.b#;$pz=q#ni:/lib/namespaced.b#;$qz=q#ni:/lib/ni#;$rz={$p7,1};$sz={};$tz=q#extend#;$uz=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$vz=bless({$w,$uz,$y,$z},$A);$wz=q#is_mutable#;$xz=q#$0 ne '-' && -w $0#;$yz=bless({$w,$xz,$y,$z},$A);$zz=q#modify#;$Az=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$Bz=bless({$w,$Az,$y,$z},$A);$Cz={$tz,$vz,$wz,$yz,$zz,$Bz};$Dz=q#/lib/ni_self.b#;$Ez=bless({$Y4,$sz,$T5,$q,$U5,$q,$V5,$Cz,$K,$Dz},$g6);$Fz={};$Gz=q#--internal/+=#;$Hz=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted;
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$Iz=bless({$w,$Hz,$y,$z},$A);$Jz=q#--internal/eval#;$Kz=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$Lz=bless({$w,$Kz,$y,$z},$A);$Mz=q#--internal/image#;$Nz=q#shift->quoted->io->into_sync(ni"fd:1");
0;#;$Oz=bless({$w,$Nz,$y,$z},$A);$Pz=q#--internal/test#;$Qz=q#local $| = 1;
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
!!$failed;#;$Rz=bless({$w,$Qz,$y,$z},$A);$Sz=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$Tz=bless({$w,$Sz,$y,$z},$A);$Uz={$Gz,$Iz,$Jz,$Lz,$Mz,$Oz,$Pz,$Rz,$Xo,$Tz};$Vz=q#/lib/ni_main.b#;$Wz=bless({$Y4,$Fz,$T5,$q,$U5,$q,$V5,$Uz,$K,$Vz},$g6);$Xz={};$Yz=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$Zz=bless({$w,$Yz,$y,$z},$A);$cA=q#resolver_for#;$dA=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$eA=bless({$w,$dA,$y,$z},$A);$fA={$F6,$Zz,$cA,$eA};$gA=q#/lib/ni_resolver.b#;$hA=bless({$Y4,$Xz,$T5,$q,$U5,$q,$V5,$fA,$K,$gA},$g6);$iA={};$jA=q#exists#;$kA=q#exists $_[0]->{named}{$_[1]}#;$lA=bless({$w,$kA,$y,$z},$A);$mA=q#quoted#;$nA=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$oA=bless({$w,$nA,$y,$z},$A);$pA={$jA,$lA,$mA,$oA};$qA=q#/lib/ni_image.b#;$rA=bless({$Y4,$iA,$T5,$q,$U5,$q,$V5,$pA,$K,$qA},$g6);$sA=[$I7,$Ez,$Wz,$hA,$rA];$tA=bless({$Y4,$rz,$K,$j4,$M5,$sA},$x5);$uA=q#ni:/lib/ni.c#;$vA={$x5,1};$wA=q#/lib/ni.c#;$xA=[$u9];$yA=bless({$Y4,$vA,$K,$wA,$M5,$xA},$N5);$zA=q#ni:/lib/ni_image.b#;$AA=q#ni:/lib/ni_main.b#;$BA=q#ni:/lib/ni_resolver.b#;$CA=q#ni:/lib/ni_self.b#;$DA=q#ni:/lib/ni_static_util.b#;$EA={};$FA=q#abbrev#;$GA=[];$HA=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$IA=bless({$t,$GA,$v,$q,$w,$HA,$y,$z},$A);$JA=q#dor#;$KA=[];$LA=q#defined $_[0] ? $_[0] : $_[1]#;$MA=bless({$t,$KA,$v,$q,$w,$LA,$y,$z},$A);$NA=q#indent#;$OA=[];$PA=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$QA=bless({$t,$OA,$v,$q,$w,$PA,$y,$z},$A);$RA=q#max#;$SA=[];$TA=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$UA=bless({$t,$SA,$v,$q,$w,$TA,$y,$z},$A);$VA=q#maxstr#;$WA=[];$XA=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$YA=bless({$t,$WA,$v,$q,$w,$XA,$y,$z},$A);$ZA=q#mean#;$cB=[];$dB=q#sum(@_) / (@_ || 1)#;$eB=bless({$t,$cB,$v,$q,$w,$dB,$y,$z},$A);$fB=q#min#;$gB=[];$hB=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$iB=bless({$t,$gB,$v,$q,$w,$hB,$y,$z},$A);$jB=q#minstr#;$kB=[];$lB=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$mB=bless({$t,$kB,$v,$q,$w,$lB,$y,$z},$A);$nB=q#sgr#;$oB=[];$pB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$qB=bless({$t,$oB,$v,$q,$w,$pB,$y,$z},$A);$rB=q#sr#;$sB=[];$tB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$uB=bless({$t,$sB,$v,$q,$w,$tB,$y,$z},$A);$vB=q#sum#;$wB=[];$xB=q#local $_; my $x = 0; $x += $_ for @_; $x#;$yB=bless({$t,$wB,$v,$q,$w,$xB,$y,$z},$A);$zB=q#swap#;$AB=[];$BB=q#@_[0, 1] = @_[1, 0]#;$CB=bless({$t,$AB,$v,$q,$w,$BB,$y,$z},$A);$DB={$FA,$IA,$JA,$MA,$NA,$QA,$RA,$UA,$VA,$YA,$ZA,$eB,$fB,$iB,$jB,$mB,$nB,$qB,$rB,$uB,$vB,$yB,$zB,$CB};$EB=q#/lib/ni_static_util.b#;$FB=bless({$Y4,$EA,$T5,$q,$U5,$q,$V5,$DB,$K,$EB},$g6);$GB=q#ni:/lib/perlbranch.b#;$HB=q#ni:/lib/quote_circular_addressed.b#;$IB=q#ni:/lib/quote_code_fail.b#;$JB=q#ni:/lib/quote_gensym_identity.b#;$KB=q#ni:/lib/quote_objects.b#;$LB=q#ni:/lib/quote_simple#;$MB={$q7,1};$NB={};$OB=[];$PB=q#+{}#;$QB=bless({$t,$OB,$v,$q,$w,$PB,$y,$z},$A);$RB={$Q6,$QB};$SB=q#/lib/quote_simple_init.b#;$TB=bless({$Y4,$NB,$T5,$q,$U5,$q,$V5,$RB,$K,$SB},$g6);$UB={};$VB=[];$WB=bless({$t,$VB,$v,$q,$w,0,$y,$z},$A);$XB=[];$YB=q#shift->quote_value(shift)#;$ZB=bless({$t,$XB,$v,$q,$w,$YB,$y,$z},$A);$cC={$Sx,$WB,$oy,$ZB};$dC=q#/lib/quote_simple_quote.b#;$eC=bless({$Y4,$UB,$T5,$q,$U5,$q,$V5,$cC,$K,$dC},$g6);$fC=[$I7,$TB,$eC,$Sw,$sx,$Ix];$gC=bless({$Y4,$MB,$K,$u4,$M5,$fC},$y5);$hC=q#ni:/lib/quote_simple.c#;$iC={$y5,1};$jC=q#/lib/quote_simple.c#;$kC=[$u9];$lC=bless({$Y4,$iC,$K,$jC,$M5,$kC},$N5);$mC=q#ni:/lib/quote_simple_init.b#;$nC=q#ni:/lib/quote_simple_quote.b#;$oC=q#ni:/lib/quote_values.b#;$pC=q#ni:/lib/ref_eq.b#;$qC=q#ni:/lib/resolver.b#;$rC=q#ni:/lib/slice#;$sC={$g6,1};$tC=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$uC=bless({$w,$tC},$A);$vC=q#local $_;
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
$self;#;$wC=bless({$w,$vC},$A);$xC=q#lib/slice::apply#;$yC=q#lib/slice::apply_#;$zC={};$AC=q#apply_#;$BC={$Z5,$uC,$AC,$wC};$CC=q#/lib/slice.b#;$DC=bless({$Y4,$zC,$V5,$BC,$K,$CC},$g6);$EC={};$FC=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$GC=bless({$w,$FC,$y,$z},$A);$HC={$Q6,$GC};$IC=q#/lib/slice_init.b#;$JC=bless({$Y4,$EC,$V5,$HC,$K,$IC},$g6);$KC={};$LC=[];$MC=q#local $_;
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
$g;#;$NC=bless({$t,$LC,$v,$q,$w,$MC,$y,$z},$A);$OC={$yt,$NC};$PC=q#/lib/slice_serialize.b#;$QC=bless({$Y4,$KC,$T5,$q,$U5,$q,$V5,$OC,$K,$PC},$g6);$RC=[$S7,$o6,$DC,$JC,$QC];$SC=bless({$Y4,$sC,$K,$P4,$M5,$RC},$z5);$TC=q#ni:/lib/slice.b#;$UC=q#ni:/lib/slice.c#;$VC={$z5,1};$WC=q#/lib/slice.c#;$XC=[$y9];$YC=bless({$Y4,$VC,$K,$WC,$M5,$XC},$N5);$ZC=q#ni:/lib/slice_init.b#;$cD=q#ni:/lib/slice_serialize.b#;$dD=q#ni:/lib/static_fn.b#;$eD={};$fD=q#fk#;$gD=[];$hD=q#ni('ni:/lib/fn')->closure('@x' => [@_], q{wantarray ? @x : $x[0]});#;$iD=bless({$t,$gD,$v,$q,$w,$hD,$y,$Tv},$A);$jD=[];$kD=q#@_ ? ni('ni:/lib/fn')->new(@_) : ni('ni:/lib/fn');#;$lD=q#(;$)#;$mD=bless({$t,$jD,$v,$q,$w,$kD,$y,$lD},$A);$nD=q#fp#;$oD=[];$pD=q#ni('ni:/lib/fn')->new(@_);#;$qD=q#($$)#;$rD=bless({$t,$oD,$v,$q,$w,$pD,$y,$qD},$A);$sD={$fD,$iD,$ht,$mD,$nD,$rD};$tD=q#/lib/static_fn.b#;$uD=bless({$Y4,$eD,$T5,$q,$U5,$q,$V5,$sD,$K,$tD},$g6);$vD=q#ni:/lib/subclass.b#;$wD=q#ni:/lib/tag#;$xD={$p6,1};$yD=q#/lib/tag#;$zD={};$AD=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$BD=bless({$w,$AD,$y,$z},$A);$CD={$Z5,$BD};$DD=q#/lib/tag.b#;$ED=bless({$Y4,$zD,$T5,$q,$U5,$q,$V5,$CD,$K,$DD},$g6);$FD={};$GD=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$HD=bless({$w,$GD,$y,$z},$A);$ID={$Q6,$HD};$JD=q#/lib/tag_init.b#;$KD=bless({$Y4,$FD,$T5,$q,$U5,$q,$V5,$ID,$K,$JD},$g6);$LD=[$S7,$o6,$ED,$KD];$MD=bless({$Y4,$xD,$K,$yD,$M5,$LD},$A5);$ND=q#ni:/lib/tag.b#;$OD=q#ni:/lib/tag.c#;$PD={$A5,1};$QD=q#/lib/tag.c#;$RD=[$y9];$SD=bless({$Y4,$PD,$K,$QD,$M5,$RD},$N5);$TD=q#ni:/lib/tag_init.b#;$UD=q#ni:/lib/test_assert_eq#;$VD={$r7,1};$WD=q#/lib/test_assert_eq#;$XD={$r7,1,$s7,1};$YD=q#/lib/test_assertion#;$ZD={};$cE=q#commit#;$dE=[];$eE=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$fE=bless({$t,$dE,$v,$q,$w,$eE,$y,$z},$A);$gE={$cE,$fE};$hE=q#/lib/test_assertion_commit.b#;$iE=bless({$Y4,$ZD,$T5,$q,$U5,$q,$V5,$gE,$K,$hE},$g6);$jE=[$I7,$iE];$kE=bless({$Y4,$XD,$K,$YD,$M5,$jE},$C5);$lE={};$mE=[];$nE=q#my ($class, $diff) = @_;
+{diff => $diff};#;$oE=bless({$t,$mE,$v,$q,$w,$nE,$y,$z},$A);$pE={$Q6,$oE};$qE=q#/lib/test_assert_eq_init.b#;$rE=bless({$Y4,$lE,$T5,$q,$U5,$q,$V5,$pE,$K,$qE},$g6);$sE={};$tE=[];$uE=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode_pretty $$self{diff}
              : "PASS";#;$vE=bless({$t,$tE,$v,$q,$w,$uE,$y,$z},$A);$wE=q#failed#;$xE=[];$yE=q#defined shift->{diff}#;$zE=bless({$t,$xE,$v,$q,$w,$yE,$y,$z},$A);$AE=q#result#;$BE=[];$CE=bless({$t,$BE,$v,$q,$w,$Zo,$y,$z},$A);$DE={$p8,$vE,$wE,$zE,$AE,$CE};$EE=q#/lib/test_assert_eq_result.b#;$FE=bless({$Y4,$sE,$T5,$q,$U5,$q,$V5,$DE,$K,$EE},$g6);$GE=[$kE,$rE,$FE];$HE=bless({$Y4,$VD,$K,$WD,$M5,$GE},$B5);$IE=q#ni:/lib/test_assert_eq.c#;$JE={$B5,1};$KE=q#/lib/test_assert_eq.c#;$LE={$B5,1,$C5,1};$ME=q#/lib/test_assertion.c#;$NE=[$u9];$OE=bless({$Y4,$LE,$K,$ME,$M5,$NE},$N5);$PE=[$OE];$QE=bless({$Y4,$JE,$K,$KE,$M5,$PE},$N5);$RE=q#ni:/lib/test_assert_eq_init.b#;$SE=q#ni:/lib/test_assert_eq_result.b#;$TE=q#ni:/lib/test_assertion#;$UE=q#ni:/lib/test_assertion.c#;$VE=q#ni:/lib/test_assertion_commit.b#;$WE=q#ni:/lib/test_case#;$XE={$C,1};$YE=q#/lib/test_case#;$ZE=q#running_test#;$cF={};$dF=[];$eF=q#shift->{'assertions'}#;$fF=bless({$t,$dF,$v,$q,$w,$eF,$y,$z},$A);$gF=[];$hF=q#shift->{'test'}#;$iF=bless({$t,$gF,$v,$q,$w,$hF,$y,$z},$A);$jF={$n,$fF,$s,$iF};$kF=q#/lib/test_case_ro.b#;$lF=bless({$Y4,$cF,$T5,$q,$U5,$q,$V5,$jF,$K,$kF},$g6);$mF={};$nF=[];$oF=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$pF=bless({$t,$nF,$v,$q,$w,$oF,$y,$z},$A);$qF={$p,$pF};$rF=q#/lib/test_case_rw.b#;$sF=bless({$Y4,$mF,$T5,$q,$U5,$q,$V5,$qF,$K,$rF},$g6);$tF={};$uF=[];$vF=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$wF=bless({$t,$uF,$v,$q,$w,$vF,$y,$z},$A);$xF={$Q6,$wF};$yF=q#/lib/test_case_init.b#;$zF=bless({$Y4,$tF,$T5,$q,$U5,$q,$V5,$xF,$K,$yF},$g6);$AF={};$BF=[];$CF=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$DF=bless({$t,$BF,$v,$q,$w,$CF,$y,$z},$A);$EF=[];$FF=q#!shift->{outcome}->[0]#;$GF=bless({$t,$EF,$v,$q,$w,$FF,$y,$z},$A);$HF={$p8,$DF,$wE,$GF};$IF=q#/lib/test_case_metrics.b#;$JF=bless({$Y4,$AF,$T5,$q,$U5,$q,$V5,$HF,$K,$IF},$g6);$KF={};$LF=q#done#;$MF=[];$NF=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$OF=bless({$t,$MF,$v,$q,$w,$NF,$y,$z},$A);$PF=[];$QF=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$RF=bless({$t,$PF,$v,$q,$w,$QF,$y,$z},$A);$SF={$LF,$OF,$Xo,$RF};$TF=q#/lib/test_case_run.b#;$UF=bless({$Y4,$KF,$T5,$q,$U5,$q,$V5,$SF,$K,$TF},$g6);$VF=[$I7,$lF,$sF,$zF,$JF,$UF];$WF=bless({$Y4,$XE,$K,$YE,$ZE,$q,$M5,$VF},$D5);$XF=[];$YF=q#shift->{running_test} = undef#;$ZF=bless({$t,$XF,$v,$q,$w,$YF,$y,$z},$A);$cG=q#ni:/lib/test_case.c#;$dG={$D5,1};$eG=q#/lib/test_case.c#;$fG={};$gG=[];$hG=q#shift->{'running_test'}#;$iG=bless({$t,$gG,$v,$q,$w,$hG,$y,$z},$A);$jG={$ZE,$iG};$kG=q#/lib/test_case.c_test_ro.b#;$lG=bless({$Y4,$fG,$T5,$q,$U5,$q,$V5,$jG,$K,$kG},$g6);$mG={};$nG=q#with_test#;$oG=[];$pG=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$qG=bless({$t,$oG,$v,$q,$w,$pG,$y,$z},$A);$rG={$nG,$qG};$sG=q#/lib/test_case.c_test.b#;$tG=bless({$Y4,$mG,$T5,$ZF,$U5,$q,$V5,$rG,$K,$sG},$g6);$uG=[$u9,$lG,$tG];$vG=bless({$Y4,$dG,$K,$eG,$M5,$uG},$N5);$wG=q#ni:/lib/test_case.c_test.b#;$xG=q#ni:/lib/test_case.c_test_ro.b#;$yG=q#ni:/lib/test_case_init.b#;$zG=q#ni:/lib/test_case_metrics.b#;$AG=q#ni:/lib/test_case_ro.b#;$BG=q#ni:/lib/test_case_run.b#;$CG=q#ni:/lib/test_case_rw.b#;$DG=q#ni:/lib/test_value#;$EG={$t7,1};$FG=q#/lib/test_value#;$GG={};$HG=[];$IG=q#\\$_[1]#;$JG=bless({$t,$HG,$v,$q,$w,$IG,$y,$z},$A);$KG={$Q6,$JG};$LG=q#/lib/test_value_init.b#;$MG=bless({$Y4,$GG,$T5,$q,$U5,$q,$V5,$KG,$K,$LG},$g6);$NG={};$OG=q#(==#;$PG=[];$QG=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$RG=bless({$t,$PG,$v,$q,$w,$QG,$y,$z},$A);$SG=q#detailed_scalar_diff#;$TG=[];$UG=q#local $_;
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
[@diff];#;$VG=bless({$t,$TG,$v,$q,$w,$UG,$y,$z},$A);$WG=q#diff#;$XG=[];$YG=q#my ($self, $rhs) = @_;
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
return undef;#;$ZG=bless({$t,$XG,$v,$q,$w,$YG,$y,$z},$A);$cH={$OG,$RG,$SG,$VG,$WG,$ZG};$dH=q#/lib/test_value_eq.b#;$eH=bless({$Y4,$NG,$T5,$q,$U5,$q,$V5,$cH,$K,$dH},$g6);$fH={};$gH=[];$hH=q#ni::json_encode ${$_[0]}#;$iH=bless({$t,$gH,$v,$q,$w,$hH,$y,$z},$A);$jH={$p8,$iH};$kH=q#/lib/test_value_str.b#;$lH=bless({$Y4,$fH,$T5,$q,$U5,$q,$V5,$jH,$K,$kH},$g6);$mH=[$I7,$MG,$eH,$lH];$nH=bless({$Y4,$EG,$K,$FG,$M5,$mH},$E5);$oH=q#ni:/lib/test_value.c#;$pH={$E5,1};$qH=q#/lib/test_value.c#;$rH=[$u9];$sH=bless({$Y4,$pH,$K,$qH,$M5,$rH},$N5);$tH=q#ni:/lib/test_value_eq.b#;$uH=q#ni:/lib/test_value_init.b#;$vH=q#ni:/lib/test_value_str.b#;$wH=q#ni:/metaclass#;$xH={$N5,1};$yH=q#/metaclass#;$zH=[$M6,$T8,$V6,$M8];$AH=bless({$Y4,$xH,$K,$yH,$M5,$zH},$F5);$BH=q#ni:/metaclass.c#;$CH={$F5,1};$DH=q#/metaclass.c#;$EH=[$e9];$FH=bless({$Y4,$CH,$K,$DH,$M5,$EH},$N5);$GH=q#ni:/module#;$HH=q#ni:/module.c#;$IH=q#ni:/object#;$JH=q#ni:/object.c#;$KH=q#ni:/semantic/dimension#;$LH={$I5,1};$MH=q#/semantic/dimension#;$NH=[$e9];$OH=bless({$Y4,$LH,$K,$MH,$M5,$NH},$J5);$PH=q#ni:/semantic/dimension.c#;$QH={$J5,1};$RH=q#/semantic/dimension.c#;$SH=[$C9];$TH=bless({$Y4,$QH,$K,$RH,$M5,$SH},$N5);$UH=q#ni:/semantic/task#;$VH=q#ni:/semantic/task.c#;$WH=q#ni:/semantic/task_outcome.b#;$XH=q#ni:/semantic/task_ro.b#;$YH=q#ni:main#;$ZH={$sj,1};$cI=[$uD,$dw,$rj];$dI=bless({$Y4,$ZH,$K,$sj,$M5,$cI},$O5);$eI=q#ni:ni#;$fI={$lz,1};$gI={$lz,1};$hI=q#json_escapes#;$iI=q##;$jI=q#b#;$kI=q#	#;$lI=q#t#;$mI=q#
#;$nI=q#n#;$oI=q##;$pI=q#"#;$qI=q#/#;$rI=q#\\#;$sI={$iI,$jI,$kI,$lI,$mI,$nI,$oI,$Hh,$pI,$pI,$qI,$qI,$rI,$rI};$tI=q#json_unescapes#;$uI={$pI,$pI,$qI,$qI,$rI,$rI,$jI,$iI,$nI,$mI,$Hh,$oI,$lI,$kI};$vI={$hI,$sI,$tI,$uI};$wI=q#/lib/json_data.b#;$xI=bless({$Y4,$gI,$Ml,$vI,$K,$wI},$m7);$yI=[$xI,$kz,$FB];$zI=bless({$Y4,$fI,$K,$lz,$M5,$yI},$O5);$AI={$d,$N,$Q,$V,$W,$k1,$l1,$x1,$y1,$K1,$L1,$X1,$Y1,$v2,$w2,$B2,$C2,$Y2,$Z2,$h3,$i3,$B3,$C3,$S3,$T3,$c4,$d4,$k4,$l4,$v4,$w4,$Q4,$R4,$W4,$X4,$e9,$f9,$C9,$D9,$Ib,$Jb,$Zb,$cc,$mb,$dc,$Gb,$ec,$vc,$wc,$Ac,$Bc,$mc,$Cc,$tc,$Dc,$pe,$qe,$ue,$ve,$Vd,$we,$ne,$xe,$Tc,$ye,$Nd,$ze,$pd,$Ae,$Mc,$Be,$Tf,$Xf,$xg,$yg,$vg,$zg,$rf,$Ag,$Cf,$Bg,$Re,$Cg,$Qf,$Dg,$Ke,$Eg,$Ze,$Fg,$Zh,$ci,$gi,$hi,$Vg,$ii,$uh,$ji,$eh,$ki,$Xh,$li,$Ng,$mi,$Ch,$ni,$Gi,$Hi,$Li,$Mi,$Ei,$Ni,$vi,$Oi,$rj,$tj,$Rj,$Sj,$Wj,$Xj,$Cj,$Yj,$Pj,$Zj,$fb,$ck,$Xb,$dk,$Vb,$ek,$ja,$fk,$ra,$gk,$Da,$hk,$N9,$ik,$db,$jk,$Pa,$kk,$xl,$yl,$Cl,$Dl,$vl,$El,$Hk,$Fl,$fl,$Gl,$xk,$Hl,$Tk,$Il,$zm,$Am,$Em,$Fm,$jm,$Gm,$xm,$Hm,$cm,$Im,$Sn,$Wn,$ko,$lo,$io,$mo,$op,$sp,$Mp,$Np,$Kp,$Op,$Lo,$Pp,$Vo,$Qp,$Eo,$Rp,$jp,$Sp,$wn,$Tp,$Qn,$Up,$oq,$pq,$tq,$uq,$fq,$vq,$mq,$wq,$n8,$xq,$S7,$yq,$y9,$zq,$Jq,$Kq,$h6,$Lq,$Pq,$Qq,$Hq,$Rq,$V6,$Sq,$kr,$lr,$pr,$qr,$ir,$rr,$cr,$sr,$K8,$tr,$d8,$ur,$I8,$vr,$s9,$wr,$ss,$ts,$xs,$ys,$Tr,$zs,$es,$As,$Dr,$Bs,$Jr,$Cs,$qs,$Ds,$Q7,$Es,$Gt,$Kt,$fu,$gu,$Tt,$hu,$du,$iu,$Ts,$ju,$wt,$ku,$nt,$lu,$Et,$mu,$wv,$xv,$Bv,$Cv,$uv,$Dv,$Eu,$Ev,$xu,$Fv,$Yu,$Gv,$Ov,$Pv,$dw,$ew,$wy,$xy,$By,$Cy,$mw,$Dy,$Kw,$Ey,$G7,$Fy,$T8,$Gy,$kz,$mz,$u8,$nz,$o6,$oz,$w6,$pz,$D6,$qz,$tA,$uA,$yA,$zA,$rA,$AA,$Wz,$BA,$hA,$CA,$Ez,$DA,$FB,$GB,$M6,$HB,$Yx,$IB,$Sw,$JB,$uy,$KB,$Ix,$LB,$gC,$hC,$lC,$mC,$TB,$nC,$eC,$oC,$sx,$pC,$B8,$qC,$K6,$rC,$SC,$TC,$DC,$UC,$YC,$ZC,$JC,$cD,$QC,$dD,$uD,$vD,$c9,$wD,$MD,$ND,$ED,$OD,$SD,$TD,$KD,$UD,$HE,$IE,$QE,$RE,$rE,$SE,$FE,$TE,$kE,$UE,$OE,$VE,$iE,$WE,$WF,$cG,$vG,$wG,$tG,$xG,$lG,$yG,$zF,$zG,$JF,$AG,$lF,$BG,$UF,$CG,$sF,$DG,$nH,$oH,$sH,$tH,$eH,$uH,$MG,$vH,$lH,$wH,$AH,$BH,$FH,$GH,$M8,$HH,$A9,$IH,$I7,$JH,$u9,$KH,$OH,$PH,$TH,$UH,$jn,$VH,$eo,$WH,$hn,$XH,$Tm,$YH,$dI,$eI,$zI};$BI=q#resolvers#;$CI=[];$DI=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$EI=bless({$t,$CI,$v,$q,$w,$DI,$y,$z},$A);$FI=q#file#;$GI=[];$HI=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$II=bless({$t,$GI,$v,$q,$w,$HI,$y,$z},$A);$JI=q#null#;$KI=[];$LI=q#ni('ni:/io/null')->new#;$MI=bless({$t,$KI,$v,$q,$w,$LI,$y,$z},$A);$NI=q#sh#;$OI=[];$PI=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$QI=bless({$t,$OI,$v,$q,$w,$PI,$y,$z},$A);$RI=q#str#;$SI=[];$TI=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$UI=bless({$t,$SI,$v,$q,$w,$TI,$y,$z},$A);$VI={$vd,$EI,$FI,$II,$JI,$MI,$NI,$QI,$RI,$UI};$WI=bless({$c,$AI,$BI,$VI},$p7);*$yC=\&$wC;*$xC=\&$uC;$h6->apply_($Z4);$h6->apply_($c5);$h6->apply_($d5);$h6->apply_($e5);$h6->apply_($f5);$h6->apply_($g5);$h6->apply_($h5);$h6->apply_($i5);$h6->apply_($j5);$h6->apply_($k5);$h6->apply_($l5);$h6->apply_($m5);$h6->apply_($n5);$h6->apply_($o5);$h6->apply_($p5);$h6->apply_($q5);$h6->apply_($i6);$h6->apply_($r5);$h6->apply_($s5);$h6->apply_($t5);$h6->apply_($u5);$h6->apply_($v5);$h6->apply_($w5);$h6->apply_($x5);$h6->apply_($y5);$h6->apply_($z5);$h6->apply_($A5);$h6->apply_($B5);$h6->apply_($C5);$h6->apply_($D5);$h6->apply_($E5);$h6->apply_($N5);$h6->apply_($F5);$h6->apply_($O5);$h6->apply_($G5);$h6->apply_($H5);$h6->apply_($I5);$h6->apply_($J5);$h6->apply_($K5);$o6->apply_($Z4);$o6->apply_($c5);$o6->apply_($d5);$o6->apply_($e5);$o6->apply_($f5);$o6->apply_($g5);$o6->apply_($h5);$o6->apply_($i5);$o6->apply_($j5);$o6->apply_($k5);$o6->apply_($l5);$o6->apply_($m5);$o6->apply_($n5);$o6->apply_($o5);$o6->apply_($p5);$o6->apply_($q5);$o6->apply_($i6);$o6->apply_($r5);$o6->apply_($s5);$o6->apply_($M);$o6->apply_($t5);$o6->apply_($u5);$o6->apply_($v5);$o6->apply_($w5);$o6->apply_($x5);$o6->apply_($y5);$o6->apply_($g6);$o6->apply_($z5);$o6->apply_($p6);$o6->apply_($A5);$o6->apply_($B5);$o6->apply_($C5);$o6->apply_($D5);$o6->apply_($E5);$o6->apply_($N5);$o6->apply_($F5);$o6->apply_($O5);$o6->apply_($G5);$o6->apply_($H5);$o6->apply_($I5);$o6->apply_($J5);$o6->apply_($K5);$w6->apply_($Z4);$w6->apply_($c5);$w6->apply_($d5);$w6->apply_($e5);$w6->apply_($f5);$w6->apply_($g5);$w6->apply_($h5);$w6->apply_($i5);$w6->apply_($j5);$w6->apply_($k5);$w6->apply_($l5);$w6->apply_($m5);$w6->apply_($n5);$w6->apply_($o5);$w6->apply_($p5);$w6->apply_($q5);$w6->apply_($i6);$w6->apply_($r5);$w6->apply_($s5);$w6->apply_($t5);$w6->apply_($u5);$w6->apply_($v5);$w6->apply_($w5);$w6->apply_($x5);$w6->apply_($y5);$w6->apply_($g6);$w6->apply_($z5);$w6->apply_($p6);$w6->apply_($A5);$w6->apply_($B5);$w6->apply_($C5);$w6->apply_($D5);$w6->apply_($E5);$w6->apply_($N5);$w6->apply_($F5);$w6->apply_($O5);$w6->apply_($G5);$w6->apply_($H5);$w6->apply_($I5);$w6->apply_($J5);$w6->apply_($K5);$D6->apply_($Z4);$D6->apply_($c5);$D6->apply_($d5);$D6->apply_($e5);$D6->apply_($f5);$D6->apply_($g5);$D6->apply_($h5);$D6->apply_($i5);$D6->apply_($j5);$D6->apply_($k5);$D6->apply_($l5);$D6->apply_($m5);$D6->apply_($n5);$D6->apply_($o5);$D6->apply_($p5);$D6->apply_($q5);$D6->apply_($i6);$D6->apply_($r5);$D6->apply_($s5);$D6->apply_($t5);$D6->apply_($u5);$D6->apply_($v5);$D6->apply_($w5);$D6->apply_($x5);$D6->apply_($y5);$D6->apply_($g6);$D6->apply_($z5);$D6->apply_($p6);$D6->apply_($A5);$D6->apply_($B5);$D6->apply_($C5);$D6->apply_($D5);$D6->apply_($E5);$D6->apply_($N5);$D6->apply_($F5);$D6->apply_($O5);$D6->apply_($G5);$D6->apply_($H5);$D6->apply_($I5);$D6->apply_($J5);$D6->apply_($K5);$K6->apply_($Z4);$K6->apply_($c5);$K6->apply_($d5);$K6->apply_($e5);$K6->apply_($f5);$K6->apply_($g5);$K6->apply_($h5);$K6->apply_($i5);$K6->apply_($j5);$K6->apply_($k5);$K6->apply_($l5);$K6->apply_($m5);$K6->apply_($n5);$K6->apply_($o5);$K6->apply_($p5);$K6->apply_($q5);$K6->apply_($i6);$K6->apply_($r5);$K6->apply_($s5);$K6->apply_($t5);$K6->apply_($u5);$K6->apply_($v5);$K6->apply_($w5);$K6->apply_($x5);$K6->apply_($y5);$K6->apply_($z5);$K6->apply_($p6);$K6->apply_($A5);$K6->apply_($B5);$K6->apply_($C5);$K6->apply_($D5);$K6->apply_($E5);$K6->apply_($N5);$K6->apply_($F5);$K6->apply_($O5);$K6->apply_($G5);$K6->apply_($H5);$K6->apply_($I5);$K6->apply_($J5);$K6->apply_($K5);$V6->apply_($Z4);$V6->apply_($c5);$V6->apply_($d5);$V6->apply_($e5);$V6->apply_($f5);$V6->apply_($g5);$V6->apply_($h5);$V6->apply_($i5);$V6->apply_($j5);$V6->apply_($k5);$V6->apply_($l5);$V6->apply_($m5);$V6->apply_($n5);$V6->apply_($o5);$V6->apply_($p5);$V6->apply_($q5);$V6->apply_($r5);$V6->apply_($s5);$V6->apply_($t5);$V6->apply_($u5);$V6->apply_($v5);$V6->apply_($w5);$V6->apply_($x5);$V6->apply_($y5);$V6->apply_($z5);$V6->apply_($A5);$V6->apply_($B5);$V6->apply_($C5);$V6->apply_($D5);$V6->apply_($E5);$V6->apply_($N5);$V6->apply_($F5);$V6->apply_($O5);$V6->apply_($G5);$V6->apply_($H5);$V6->apply_($I5);$V6->apply_($J5);$V6->apply_($K5);$G7->apply_($Z4);$G7->apply_($c5);$G7->apply_($W6);$G7->apply_($d5);$G7->apply_($X6);$G7->apply_($e5);$G7->apply_($Y6);$G7->apply_($f5);$G7->apply_($Z6);$G7->apply_($g5);$G7->apply_($c7);$G7->apply_($h5);$G7->apply_($d7);$G7->apply_($i5);$G7->apply_($e7);$G7->apply_($j5);$G7->apply_($f7);$G7->apply_($k5);$G7->apply_($g7);$G7->apply_($l5);$G7->apply_($h7);$G7->apply_($m5);$G7->apply_($i7);$G7->apply_($n5);$G7->apply_($j7);$G7->apply_($o5);$G7->apply_($k7);$G7->apply_($p5);$G7->apply_($l7);$G7->apply_($q5);$G7->apply_($i6);$G7->apply_($r5);$G7->apply_($m7);$G7->apply_($s5);$G7->apply_($M);$G7->apply_($t5);$G7->apply_($A);$G7->apply_($u5);$G7->apply_($n7);$G7->apply_($v5);$G7->apply_($o7);$G7->apply_($w5);$G7->apply_($p7);$G7->apply_($x5);$G7->apply_($q7);$G7->apply_($y5);$G7->apply_($g6);$G7->apply_($z5);$G7->apply_($p6);$G7->apply_($A5);$G7->apply_($r7);$G7->apply_($B5);$G7->apply_($s7);$G7->apply_($C5);$G7->apply_($C);$G7->apply_($D5);$G7->apply_($t7);$G7->apply_($E5);$G7->apply_($N5);$G7->apply_($F5);$G7->apply_($O5);$G7->apply_($G5);$G7->apply_($u7);$G7->apply_($H5);$G7->apply_($I5);$G7->apply_($J5);$G7->apply_($v7);$G7->apply_($K5);$Q7->apply_($Z4);$Q7->apply_($c5);$Q7->apply_($d5);$Q7->apply_($e5);$Q7->apply_($f5);$Q7->apply_($g5);$Q7->apply_($h5);$Q7->apply_($i5);$Q7->apply_($j5);$Q7->apply_($k5);$Q7->apply_($l5);$Q7->apply_($m5);$Q7->apply_($n5);$Q7->apply_($o5);$Q7->apply_($p5);$Q7->apply_($l7);$Q7->apply_($q5);$Q7->apply_($i6);$Q7->apply_($r5);$Q7->apply_($m7);$Q7->apply_($s5);$Q7->apply_($t5);$Q7->apply_($u5);$Q7->apply_($v5);$Q7->apply_($w5);$Q7->apply_($x5);$Q7->apply_($y5);$Q7->apply_($g6);$Q7->apply_($z5);$Q7->apply_($p6);$Q7->apply_($A5);$Q7->apply_($B5);$Q7->apply_($C5);$Q7->apply_($D5);$Q7->apply_($E5);$Q7->apply_($N5);$Q7->apply_($F5);$Q7->apply_($O5);$Q7->apply_($G5);$Q7->apply_($H5);$Q7->apply_($I5);$Q7->apply_($J5);$Q7->apply_($K5);$d8->apply_($Z4);$d8->apply_($c5);$d8->apply_($d5);$d8->apply_($e5);$d8->apply_($f5);$d8->apply_($g5);$d8->apply_($h5);$d8->apply_($i5);$d8->apply_($j5);$d8->apply_($k5);$d8->apply_($l5);$d8->apply_($m5);$d8->apply_($n5);$d8->apply_($o5);$d8->apply_($p5);$d8->apply_($q5);$d8->apply_($i6);$d8->apply_($r5);$d8->apply_($s5);$d8->apply_($t5);$d8->apply_($u5);$d8->apply_($v5);$d8->apply_($w5);$d8->apply_($x5);$d8->apply_($y5);$d8->apply_($z5);$d8->apply_($A5);$d8->apply_($B5);$d8->apply_($C5);$d8->apply_($D5);$d8->apply_($E5);$d8->apply_($N5);$d8->apply_($F5);$d8->apply_($O5);$d8->apply_($G5);$d8->apply_($H5);$d8->apply_($I5);$d8->apply_($J5);$d8->apply_($K5);$n8->apply_($Z4);$n8->apply_($c5);$n8->apply_($d5);$n8->apply_($e5);$n8->apply_($f5);$n8->apply_($g5);$n8->apply_($h5);$n8->apply_($i5);$n8->apply_($j5);$n8->apply_($k5);$n8->apply_($l5);$n8->apply_($m5);$n8->apply_($n5);$n8->apply_($o5);$n8->apply_($p5);$n8->apply_($q5);$n8->apply_($i6);$n8->apply_($r5);$n8->apply_($s5);$n8->apply_($t5);$n8->apply_($u5);$n8->apply_($v5);$n8->apply_($w5);$n8->apply_($x5);$n8->apply_($y5);$n8->apply_($z5);$n8->apply_($A5);$n8->apply_($B5);$n8->apply_($C5);$n8->apply_($D5);$n8->apply_($E5);$n8->apply_($N5);$n8->apply_($F5);$n8->apply_($O5);$n8->apply_($G5);$n8->apply_($H5);$n8->apply_($I5);$n8->apply_($J5);$n8->apply_($K5);$u8->apply_($Z4);$u8->apply_($c5);$u8->apply_($d5);$u8->apply_($e5);$u8->apply_($f5);$u8->apply_($g5);$u8->apply_($h5);$u8->apply_($i5);$u8->apply_($j5);$u8->apply_($k5);$u8->apply_($l5);$u8->apply_($m5);$u8->apply_($n5);$u8->apply_($o5);$u8->apply_($p5);$u8->apply_($q5);$u8->apply_($i6);$u8->apply_($r5);$u8->apply_($s5);$u8->apply_($t5);$u8->apply_($u5);$u8->apply_($v5);$u8->apply_($w5);$u8->apply_($x5);$u8->apply_($y5);$u8->apply_($z5);$u8->apply_($A5);$u8->apply_($B5);$u8->apply_($C5);$u8->apply_($D5);$u8->apply_($E5);$u8->apply_($N5);$u8->apply_($F5);$u8->apply_($O5);$u8->apply_($G5);$u8->apply_($H5);$u8->apply_($I5);$u8->apply_($J5);$u8->apply_($K5);$B8->apply_($Z4);$B8->apply_($c5);$B8->apply_($d5);$B8->apply_($e5);$B8->apply_($f5);$B8->apply_($g5);$B8->apply_($h5);$B8->apply_($i5);$B8->apply_($j5);$B8->apply_($k5);$B8->apply_($l5);$B8->apply_($m5);$B8->apply_($n5);$B8->apply_($o5);$B8->apply_($p5);$B8->apply_($q5);$B8->apply_($i6);$B8->apply_($r5);$B8->apply_($s5);$B8->apply_($t5);$B8->apply_($u5);$B8->apply_($v5);$B8->apply_($w5);$B8->apply_($x5);$B8->apply_($y5);$B8->apply_($z5);$B8->apply_($A5);$B8->apply_($B5);$B8->apply_($C5);$B8->apply_($D5);$B8->apply_($E5);$B8->apply_($N5);$B8->apply_($F5);$B8->apply_($O5);$B8->apply_($G5);$B8->apply_($H5);$B8->apply_($I5);$B8->apply_($J5);$B8->apply_($K5);$I8->apply_($Z4);$I8->apply_($c5);$I8->apply_($d5);$I8->apply_($e5);$I8->apply_($f5);$I8->apply_($g5);$I8->apply_($h5);$I8->apply_($i5);$I8->apply_($j5);$I8->apply_($k5);$I8->apply_($l5);$I8->apply_($m5);$I8->apply_($n5);$I8->apply_($o5);$I8->apply_($p5);$I8->apply_($q5);$I8->apply_($i6);$I8->apply_($r5);$I8->apply_($s5);$I8->apply_($t5);$I8->apply_($u5);$I8->apply_($v5);$I8->apply_($w5);$I8->apply_($x5);$I8->apply_($y5);$I8->apply_($z5);$I8->apply_($A5);$I8->apply_($B5);$I8->apply_($C5);$I8->apply_($D5);$I8->apply_($E5);$I8->apply_($N5);$I8->apply_($F5);$I8->apply_($O5);$I8->apply_($G5);$I8->apply_($H5);$I8->apply_($I5);$I8->apply_($J5);$I8->apply_($K5);$T8->apply_($Z4);$T8->apply_($c5);$T8->apply_($d5);$T8->apply_($e5);$T8->apply_($f5);$T8->apply_($g5);$T8->apply_($h5);$T8->apply_($i5);$T8->apply_($j5);$T8->apply_($k5);$T8->apply_($l5);$T8->apply_($m5);$T8->apply_($n5);$T8->apply_($o5);$T8->apply_($p5);$T8->apply_($q5);$T8->apply_($r5);$T8->apply_($s5);$T8->apply_($t5);$T8->apply_($A);$T8->apply_($u5);$T8->apply_($v5);$T8->apply_($w5);$T8->apply_($x5);$T8->apply_($y5);$T8->apply_($g6);$T8->apply_($z5);$T8->apply_($p6);$T8->apply_($A5);$T8->apply_($B5);$T8->apply_($C5);$T8->apply_($D5);$T8->apply_($E5);$T8->apply_($N5);$T8->apply_($F5);$T8->apply_($G5);$T8->apply_($H5);$T8->apply_($I5);$T8->apply_($J5);$T8->apply_($K5);$c9->apply_($Z4);$c9->apply_($c5);$c9->apply_($d5);$c9->apply_($e5);$c9->apply_($f5);$c9->apply_($g5);$c9->apply_($h5);$c9->apply_($i5);$c9->apply_($j5);$c9->apply_($k5);$c9->apply_($l5);$c9->apply_($m5);$c9->apply_($n5);$c9->apply_($o5);$c9->apply_($p5);$c9->apply_($q5);$c9->apply_($r5);$c9->apply_($s5);$c9->apply_($t5);$c9->apply_($u5);$c9->apply_($v5);$c9->apply_($w5);$c9->apply_($x5);$c9->apply_($y5);$c9->apply_($z5);$c9->apply_($A5);$c9->apply_($B5);$c9->apply_($C5);$c9->apply_($D5);$c9->apply_($E5);$c9->apply_($F5);$c9->apply_($G5);$c9->apply_($H5);$c9->apply_($I5);$c9->apply_($J5);$c9->apply_($K5);$s9->apply_($c5);$s9->apply_($d5);$s9->apply_($e5);$s9->apply_($f5);$s9->apply_($g5);$s9->apply_($h5);$s9->apply_($i5);$s9->apply_($j5);$s9->apply_($k5);$s9->apply_($l5);$s9->apply_($m5);$s9->apply_($n5);$s9->apply_($o5);$s9->apply_($p5);$s9->apply_($q5);$s9->apply_($r5);$s9->apply_($s5);$s9->apply_($t5);$s9->apply_($u5);$s9->apply_($v5);$s9->apply_($w5);$s9->apply_($x5);$s9->apply_($y5);$s9->apply_($z5);$s9->apply_($A5);$s9->apply_($B5);$s9->apply_($C5);$s9->apply_($D5);$s9->apply_($E5);$s9->apply_($G5);$s9->apply_($H5);$s9->apply_($J5);$s9->apply_($K5);$N9->apply_($W6);$N9->apply_($X6);$N9->apply_($Y6);$N9->apply_($Z6);$N9->apply_($c7);$N9->apply_($d7);$N9->apply_($e7);$N9->apply_($f7);$N9->apply_($g7);$N9->apply_($h7);$ja->apply_($W6);$ja->apply_($X6);$ja->apply_($Y6);$ja->apply_($Z6);$ja->apply_($c7);$ja->apply_($d7);$ja->apply_($e7);$ja->apply_($f7);$ja->apply_($g7);$ja->apply_($h7);$ra->apply_($W6);$ra->apply_($X6);$ra->apply_($Y6);$ra->apply_($Z6);$ra->apply_($c7);$ra->apply_($d7);$ra->apply_($e7);$ra->apply_($f7);$ra->apply_($g7);$ra->apply_($h7);$Da->apply_($W6);$Da->apply_($X6);$Da->apply_($Y6);$Da->apply_($Z6);$Da->apply_($c7);$Da->apply_($d7);$Da->apply_($e7);$Da->apply_($f7);$Da->apply_($g7);$Da->apply_($h7);$Pa->apply_($W6);$Pa->apply_($X6);$Pa->apply_($Y6);$Pa->apply_($Z6);$Pa->apply_($c7);$Pa->apply_($d7);$Pa->apply_($e7);$Pa->apply_($f7);$Pa->apply_($g7);$Pa->apply_($h7);$db->apply_($W6);$db->apply_($X6);$db->apply_($Y6);$db->apply_($Z6);$db->apply_($c7);$db->apply_($d7);$db->apply_($e7);$db->apply_($f7);$db->apply_($g7);$db->apply_($h7);$mb->apply_($W6);$Gb->apply_($W6);$Vb->apply_($d5);$Vb->apply_($e5);$Vb->apply_($f5);$Vb->apply_($g5);$Vb->apply_($h5);$Vb->apply_($i5);$Vb->apply_($j5);$Vb->apply_($k5);$Vb->apply_($l5);$Vb->apply_($m5);$mc->apply_($X6);$tc->apply_($X6);$Mc->apply_($Y6);$Tc->apply_($Y6);$pd->apply_($Y6);$Nd->apply_($Y6);$Vd->apply_($Y6);$ne->apply_($Y6);$Ke->apply_($Z6);$Ke->apply_($d7);$Re->apply_($Z6);$Ze->apply_($Z6);$rf->apply_($Z6);$rf->apply_($d7);$Cf->apply_($Z6);$Qf->apply_($Z6);$Qf->apply_($d7);$vg->apply_($g5);$Ng->apply_($c7);$Vg->apply_($c7);$eh->apply_($c7);$uh->apply_($c7);$Ch->apply_($c7);$Xh->apply_($c7);$vi->apply_($d7);$Ei->apply_($d7);$rj->apply_($sj);$Cj->apply_($e7);$Pj->apply_($e7);$xk->apply_($g7);$Hk->apply_($g7);$Tk->apply_($g7);$fl->apply_($g7);$vl->apply_($g7);$cm->apply_($h7);$jm->apply_($h7);$xm->apply_($h7);$Tm->apply_($i7);$Tm->apply_($j7);$Tm->apply_($k7);$Tm->apply_($v7);$hn->apply_($i7);$hn->apply_($j7);$hn->apply_($k7);$hn->apply_($v7);$wn->apply_($i7);$wn->apply_($j7);$wn->apply_($k7);$Qn->apply_($i7);$Qn->apply_($j7);$Qn->apply_($k7);$io->apply_($n5);$io->apply_($o5);$io->apply_($p5);$Eo->apply_($j7);$Lo->apply_($j7);$Vo->apply_($j7);$jp->apply_($j7);$Kp->apply_($o5);$fq->apply_($k7);$mq->apply_($k7);$Hq->apply_($i6);$cr->apply_($m7);$ir->apply_($m7);$Dr->apply_($M);$Jr->apply_($M);$Tr->apply_($M);$es->apply_($M);$qs->apply_($M);$Ts->apply_($A);$nt->apply_($A);$wt->apply_($A);$Et->apply_($A);$Tt->apply_($u5);$du->apply_($u5);$xu->apply_($n7);$Eu->apply_($n7);$Yu->apply_($n7);$uv->apply_($n7);$Ov->apply_($o7);$dw->apply_($sj);$mw->apply_($o7);$Kw->apply_($o7);$Sw->apply_($o7);$Sw->apply_($q7);$sx->apply_($o7);$sx->apply_($q7);$Ix->apply_($o7);$Ix->apply_($q7);$Yx->apply_($o7);$uy->apply_($o7);$kz->apply_($lz);$Ez->apply_($p7);$Wz->apply_($p7);$hA->apply_($p7);$rA->apply_($p7);$FB->apply_($lz);$TB->apply_($q7);$eC->apply_($q7);$DC->apply_($g6);$JC->apply_($g6);$QC->apply_($g6);$uD->apply_($sj);$ED->apply_($p6);$KD->apply_($p6);$iE->apply_($r7);$iE->apply_($s7);$rE->apply_($r7);$FE->apply_($r7);$lF->apply_($C);$sF->apply_($C);$zF->apply_($C);$JF->apply_($C);$UF->apply_($C);$lG->apply_($D5);$tG->apply_($D5);$MG->apply_($t7);$eH->apply_($t7);$lH->apply_($t7);$ni::self=$WI;&$P($N);&$P($V);&$P($k1);&$P($x1);&$P($K1);&$P($X1);&$P($v2);&$P($B2);&$P($Y2);&$P($h3);&$P($B3);&$P($S3);&$P($c4);&$P($k4);&$P($v4);&$P($Q4);&$P($W4);&$P($h6);&$P($o6);&$P($w6);&$P($D6);&$P($K6);&$P($M6);&$P($V6);&$P($G7);&$P($I7);&$P6($I7);&$P($Q7);&$P($S7);&$P6($S7);&$P($d8);&$P($n8);&$P($u8);&$P($B8);&$P($I8);&$P($K8);&$P($M8);&$P6($M8);&$P($T8);&$P($c9);&$P($e9);&$P6($e9);&$P($s9);&$P($u9);&$P6($u9);&$P($y9);&$P6($y9);&$P($A9);&$P6($A9);&$P($C9);&$P6($C9);&$P($N9);&$P($ja);&$P($ra);&$P($Da);&$P($Pa);&$P($db);&$P($fb);&$P6($fb);&$P($mb);&$P($Gb);&$P($Ib);&$P6($Ib);&$P($Vb);&$P($Xb);&$P6($Xb);&$P($Zb);&$P6($Zb);&$P($mc);&$P($tc);&$P($vc);&$P6($vc);&$P($Ac);&$P6($Ac);&$P($Mc);&$P($Tc);&$P($pd);&$P($Nd);&$P($Vd);&$P($ne);&$P($pe);&$P6($pe);&$P($ue);&$P6($ue);&$P($Ke);&$P($Re);&$P($Ze);&$P($rf);&$P($Cf);&$P($Qf);&$P($Tf);&$P6($Tf);&$Wf($Tf);&$P($vg);&$P($xg);&$P6($xg);&$P($Ng);&$P($Vg);&$P($eh);&$P($uh);&$P($Ch);&$P($Xh);&$P($Zh);&$P6($Zh);&$P($gi);&$P6($gi);&$P($vi);&$P($Ei);&$P($Gi);&$P6($Gi);&$P($Li);&$P6($Li);&$P($rj);&$P($Cj);&$P($Pj);&$P($Rj);&$P6($Rj);&$P($Wj);&$P6($Wj);&$P($xk);&$P($Hk);&$P($Tk);&$P($fl);&$P($vl);&$P($xl);&$P6($xl);&$P($Cl);&$P6($Cl);&$P($cm);&$P($jm);&$P($xm);&$P($zm);&$P6($zm);&$P($Em);&$P6($Em);&$P($Tm);&$P($hn);&$P($jn);&$P6($jn);&$P($wn);&$P($Qn);&$P($Sn);&$P6($Sn);&$Vn($Sn);&$P($eo);&$P6($eo);&$P($io);&$P($ko);&$P6($ko);&$P($Eo);&$P($Lo);&$P($Vo);&$P($jp);&$P($op);&$P6($op);&$Vn($op);&$rp($op);&$P($Kp);&$P($Mp);&$P6($Mp);&$P($fq);&$P($mq);&$P($oq);&$P6($oq);&$Vn($oq);&$P($tq);&$P6($tq);&$P($Hq);&$P($Jq);&$P6($Jq);&$P($Pq);&$P6($Pq);&$P($cr);&$P($ir);&$P($kr);&$P6($kr);&$P($pr);&$P6($pr);&$P($Dr);&$P($Jr);&$P($Tr);&$P($es);&$P($qs);&$P($ss);&$P6($ss);&$P($xs);&$P6($xs);&$P($Ts);&$P($nt);&$P($wt);&$P($Et);&$P($Gt);&$P6($Gt);&$Jt($Gt);&$P($Tt);&$P($du);&$P($fu);&$P6($fu);&$P($xu);&$P($Eu);&$P($Yu);&$P($uv);&$P($wv);&$P6($wv);&$P($Bv);&$P6($Bv);&$P($Ov);&$P($dw);&$P($mw);&$P($Kw);&$P($Sw);&$P($sx);&$P($Ix);&$P($Yx);&$P($uy);&$P($wy);&$P6($wy);&$P($By);&$P6($By);&$P($kz);&$P($Ez);&$P($Wz);&$P($hA);&$P($rA);&$P($tA);&$P6($tA);&$P($yA);&$P6($yA);&$P($FB);&$P($TB);&$P($eC);&$P($gC);&$P6($gC);&$P($lC);&$P6($lC);&$P($DC);&$P($JC);&$P($QC);&$P($SC);&$P6($SC);&$P($YC);&$P6($YC);&$P($uD);&$P($ED);&$P($KD);&$P($MD);&$P6($MD);&$P($SD);&$P6($SD);&$P($iE);&$P($kE);&$P6($kE);&$P($rE);&$P($FE);&$P($HE);&$P6($HE);&$P($OE);&$P6($OE);&$P($QE);&$P6($QE);&$P($lF);&$P($sF);&$P($zF);&$P($JF);&$P($UF);&$P($WF);&$P6($WF);&$ZF($WF);&$P($lG);&$P($tG);&$P($vG);&$P6($vG);&$P($MG);&$P($eH);&$P($lH);&$P($nH);&$P6($nH);&$P($sH);&$P6($sH);&$P($AH);&$P6($AH);&$P($FH);&$P6($FH);&$P($OH);&$P6($OH);&$P($TH);&$P6($TH);&$P($dI);&$P6($dI);&$P($zI);&$P6($zI);ni->run(@ARGV);
__DATA__
