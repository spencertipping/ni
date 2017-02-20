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
my $method = ${__PACKAGE__ . 'AUTOLOAD'};
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
$self;#;$xf=bless({$t,$vf,$v,$wf,$x,$y},$z);$yf={$mf,$pf,$qf,$tf,$uf,$xf};$zf=q#/io/exec_fork.b#;$Af=bless({$n5,$lf,$k6,$q,$l6,$q,$m6,$yf,$J,$zf},$v6);$Bf=[$sc,$Zd,$ie,$Ce,$cf,$kf,$Af];$Cf=bless({$n5,$Rd,$J,$W1,$d6,$Bf},$u5);$Df=q#ni:/io/exec.c#;$Ef={$u5,1};$Ff=q#/io/exec.c#;$Gf=[$md];$Hf=bless({$n5,$Ef,$J,$Ff,$d6,$Gf},$e6);$If=q#ni:/io/exec_env.b#;$Jf=q#ni:/io/exec_fork.b#;$Kf=q#ni:/io/exec_init.b#;$Lf=q#ni:/io/exec_io_accessors.b#;$Mf=q#ni:/io/exec_io_setup.b#;$Nf=q#ni:/io/exec_ro.b#;$Of=q#ni:/io/fd#;$Pf={$s7,1};$Qf={};$Rf=[];$Sf=q#shift->{'fd'}#;$Tf=bless({$t,$Rf,$v,$Sf,$x,$y},$z);$Uf={$Ie,$Tf};$Vf=q#/io/fd_readers.b#;$Wf=bless({$n5,$Qf,$k6,$q,$l6,$q,$m6,$Uf,$J,$Vf},$v6);$Xf={};$Yf=[];$Zf=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$cg=bless({$t,$Yf,$v,$Zf,$x,$y},$z);$dg={$h7,$cg};$eg=q#/io/fd_init.b#;$fg=bless({$n5,$Xf,$k6,$q,$l6,$q,$m6,$dg,$J,$eg},$v6);$gg={};$hg=q#be#;$ig=[];$jg=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$$self{rfh} = $$self{wfh} = undef;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$kg=bless({$t,$ig,$v,$jg,$x,$y},$z);$lg={$hg,$kg};$mg=q#/io/fd_shell.b#;$ng=bless({$n5,$gg,$k6,$q,$l6,$q,$m6,$lg,$J,$mg},$v6);$og={};$pg=q#cloexec#;$qg=[];$rg=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$sg=bless({$t,$qg,$v,$rg,$x,$y},$z);$tg=q#fcntl_flag#;$ug=[];$vg=q#my ($self, $flag, $value) = @_;
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
}#;$wg=bless({$t,$ug,$v,$vg,$x,$y},$z);$xg=q#nonblock#;$yg=[];$zg=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Ag=bless({$t,$yg,$v,$zg,$x,$y},$z);$Bg={$pg,$sg,$tg,$wg,$xg,$Ag};$Cg=q#/io/fd_fcntl.b#;$Dg=bless({$n5,$og,$k6,$q,$l6,$q,$m6,$Bg,$J,$Cg},$v6);$Eg={};$Fg=[];$Gg=q#shift->close#;$Hg=bless({$t,$Fg,$v,$Gg,$x,$y},$z);$Ig=q#close#;$Jg=[];$Kg=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
POSIX::close $$self{fd} if defined $$self{fd};
$$self{fd} = $$self{rfh} = $$self{wfh} = undef;
$self;#;$Lg=bless({$t,$Jg,$v,$Kg,$x,$y},$z);$Mg={$Ig,$Lg};$Ng=q#/io/fd_gc.b#;$Og=bless({$n5,$Eg,$k6,$q,$l6,$Hg,$m6,$Mg,$J,$Ng},$v6);$Pg={};$Qg=[];$Rg=q#my $self = shift;
open $$self{rfh}, "<&=$$self{fd}" or return undef unless $$self{rfh};
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Sg=bless({$t,$Qg,$v,$Rg,$x,$y},$z);$Tg=[];$Ug=q#my $self = shift;
open $$self{wfh}, ">&=$$self{fd}" or return undef unless $$self{wfh};
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Vg=bless({$t,$Tg,$v,$Ug,$x,$y},$z);$Wg={$Bc,$Sg,$Jc,$Vg};$Xg=q#/io/fd_perlio.b#;$Yg=bless({$n5,$Pg,$k6,$q,$l6,$q,$m6,$Wg,$J,$Xg},$v6);$Zg=[$sc,$Wf,$fg,$ng,$Dg,$Og,$Yg];$ch=bless({$n5,$Pf,$J,$l2,$d6,$Zg},$v5);$dh=q#ni:/io/fd.c#;$eh={$v5,1};$fh=q#/io/fd.c#;$gh=[$md];$hh=bless({$n5,$eh,$J,$fh,$d6,$gh},$e6);$ih=q#ni:/io/fd_fcntl.b#;$jh=q#ni:/io/fd_gc.b#;$kh=q#ni:/io/fd_init.b#;$lh=q#ni:/io/fd_perlio.b#;$mh=q#ni:/io/fd_readers.b#;$nh=q#ni:/io/fd_shell.b#;$oh=q#ni:/io/file#;$ph={$t7,1};$qh={};$rh=[];$sh=q#shift->{'name'}#;$th=bless({$t,$rh,$v,$sh,$x,$y},$z);$uh={$J,$th};$vh=q#/io/file_readers.b#;$wh=bless({$n5,$qh,$k6,$q,$l6,$q,$m6,$uh,$J,$vh},$v6);$xh={};$yh=q#mode#;$zh=[];$Ah=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$Bh=bless({$t,$zh,$v,$Ah,$x,$y},$z);$Ch={$yh,$Bh};$Dh=q#/io/file_accessors.b#;$Eh=bless({$n5,$xh,$k6,$q,$l6,$q,$m6,$Ch,$J,$Dh},$v6);$Fh={};$Gh=[];$Hh=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Ih=bless({$t,$Gh,$v,$Hh,$x,$y},$z);$Jh={$h7,$Ih};$Kh=q#/io/file_init.b#;$Lh=bless({$n5,$Fh,$k6,$q,$l6,$q,$m6,$Jh,$J,$Kh},$v6);$Mh={};$Nh=q#(-X#;$Oh=[];$Ph=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Qh=bless({$t,$Oh,$v,$Ph,$x,$y},$z);$Rh=q#mv#;$Sh=[];$Th=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Uh=bless({$t,$Sh,$v,$Th,$x,$y},$z);$Vh=q#rm#;$Wh=[];$Xh=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Yh=bless({$t,$Wh,$v,$Xh,$x,$y},$z);$Zh={$Nh,$Qh,$Rh,$Uh,$Vh,$Yh};$ci=q#/io/file_fns.b#;$di=bless({$n5,$Mh,$k6,$q,$l6,$q,$m6,$Zh,$J,$ci},$v6);$ei={};$fi=q#atomic_update#;$gi=[];$hi=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$ii=bless({$t,$gi,$v,$hi,$x,$y},$z);$ji={$fi,$ii};$ki=q#/io/file_update.b#;$li=bless({$n5,$ei,$k6,$q,$l6,$q,$m6,$ji,$J,$ki},$v6);$mi={};$ni=[];$oi=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$pi=bless({$t,$ni,$v,$oi,$x,$y},$z);$qi=q#r#;$ri=[];$si=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$ti=bless({$t,$ri,$v,$si,$x,$y},$z);$ui=[];$vi=q#shift->r->read(@_)#;$wi=bless({$t,$ui,$v,$vi,$x,$y},$z);$xi=q#w#;$yi=[];$zi=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Ai=bless({$t,$yi,$v,$zi,$x,$y},$z);$Bi=[];$Ci=q#shift->w->write(@_)#;$Di=bless({$t,$Bi,$v,$Ci,$x,$y},$z);$Ei={$Ig,$pi,$qi,$ti,$Bc,$wi,$xi,$Ai,$Jc,$Di};$Fi=q#/io/file_io.b#;$Gi=bless({$n5,$mi,$k6,$q,$l6,$q,$m6,$Ei,$J,$Fi},$v6);$Hi=[$sc,$wh,$Eh,$Lh,$di,$li,$Gi];$Ii=bless({$n5,$ph,$J,$H2,$d6,$Hi},$w5);$Ji=q#ni:/io/file.c#;$Ki={$w5,1};$Li=q#/io/file.c#;$Mi=[$md];$Ni=bless({$n5,$Ki,$J,$Li,$d6,$Mi},$e6);$Oi=q#ni:/io/file_accessors.b#;$Pi=q#ni:/io/file_fns.b#;$Qi=q#ni:/io/file_init.b#;$Ri=q#ni:/io/file_io.b#;$Si=q#ni:/io/file_readers.b#;$Ti=q#ni:/io/file_update.b#;$Ui=q#ni:/io/file_update_fd#;$Vi={$u7,1};$Wi={};$Xi=[];$Yi=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Zi=bless({$t,$Xi,$v,$Yi,$x,$y},$z);$cj={$h7,$Zi};$dj=q#/io/file_update_fd_init.b#;$ej=bless({$n5,$Wi,$k6,$q,$l6,$q,$m6,$cj,$J,$dj},$v6);$fj={};$gj=[];$hj=bless({$t,$gj,$v,$Gg,$x,$y},$z);$ij=[];$jj=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$kj=bless({$t,$ij,$v,$jj,$x,$y},$z);$lj={$Ig,$kj};$mj=q#/io/file_update_fd_gc.b#;$nj=bless({$n5,$fj,$k6,$q,$l6,$hj,$m6,$lj,$J,$mj},$v6);$oj=[$sc,$Wf,$Dg,$Yg,$ej,$nj];$pj=bless({$n5,$Vi,$J,$N2,$d6,$oj},$x5);$qj=q#ni:/io/file_update_fd.c#;$rj={$x5,1};$sj=q#/io/file_update_fd.c#;$tj=[$md];$uj=bless({$n5,$rj,$J,$sj,$d6,$tj},$e6);$vj=q#ni:/io/file_update_fd_gc.b#;$wj=q#ni:/io/file_update_fd_init.b#;$xj=q#ni:/io/named_io_fns.b#;$yj={};$zj=q#fcntl#;$Aj=[];$Bj=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Cj=bless({$t,$Aj,$v,$Bj,$x,$y},$z);$Dj=[];$Ej=q#CORE::fork#;$Fj=bless({$t,$Dj,$v,$Ej,$x,$y},$z);$Gj=q#open2#;$Hj=[];$Ij=q#CORE::open $_[0], $_[1]#;$Jj=bless({$t,$Hj,$v,$Ij,$x,$y},$z);$Kj=q#rename#;$Lj=[];$Mj=q#CORE::rename $_[0], $_[1]#;$Nj=bless({$t,$Lj,$v,$Mj,$x,$y},$z);$Oj=q#unlink#;$Pj=[];$Qj=q#CORE::unlink @_#;$Rj=bless({$t,$Pj,$v,$Qj,$x,$y},$z);$Sj=q#waitpid#;$Tj=[];$Uj=q#CORE::waitpid $_[0], $_[1]#;$Vj=bless({$t,$Tj,$v,$Uj,$x,$y},$z);$Wj={$zj,$Cj,$qf,$Fj,$Gj,$Jj,$Kj,$Nj,$Oj,$Rj,$Sj,$Vj};$Xj=q#/io/named_io_fns.b#;$Yj=bless({$n5,$yj,$k6,$q,$l6,$q,$m6,$Wj,$J,$Xj},$v6);$Zj=q#main#;$ck=q#ni:/io/null#;$dk={$v7,1};$ek=q#/io/null#;$fk={};$gk=[];$hk=q#+{fd => undef}#;$ik=bless({$t,$gk,$v,$hk,$x,$y},$z);$jk={$h7,$ik};$kk=q#/io/null_init.b#;$lk=bless({$n5,$fk,$k6,$q,$l6,$q,$m6,$jk,$J,$kk},$v6);$mk={};$nk=[];$ok=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$pk=bless({$t,$nk,$v,$ok,$x,$y},$z);$qk=[];$rk=q#shift->fd->read(@_)#;$sk=bless({$t,$qk,$v,$rk,$x,$y},$z);$tk=[];$uk=q#shift->fd->write(@_)#;$vk=bless({$t,$tk,$v,$uk,$x,$y},$z);$wk={$Ie,$pk,$Bc,$sk,$Jc,$vk};$xk=q#/io/null_io.b#;$yk=bless({$n5,$mk,$k6,$q,$l6,$q,$m6,$wk,$J,$xk},$v6);$zk=[$sc,$lk,$yk];$Ak=bless({$n5,$dk,$J,$ek,$d6,$zk},$y5);$Bk=q#ni:/io/null.c#;$Ck={$y5,1};$Dk=q#/io/null.c#;$Ek=[$md];$Fk=bless({$n5,$Ck,$J,$Dk,$d6,$Ek},$e6);$Gk=q#ni:/io/null_init.b#;$Hk=q#ni:/io/null_io.b#;$Ik=q#ni:/io/object#;$Jk=q#ni:/io/object.c#;$Kk=q#ni:/io/object.c_transfer_def.b#;$Lk=q#ni:/io/object_checks.b#;$Mk=q#ni:/io/object_constructors.b#;$Nk=q#ni:/io/object_memory.b#;$Ok=q#ni:/io/object_ops.b#;$Pk=q#ni:/io/object_transfer_async.b#;$Qk=q#ni:/io/object_transfer_sync.b#;$Rk=q#ni:/io/pid#;$Sk={$x7,1};$Tk={};$Uk=q#pid#;$Vk=[];$Wk=q#shift->{'pid'}#;$Xk=bless({$t,$Vk,$v,$Wk,$x,$y},$z);$Yk=q#status#;$Zk=[];$cl=q#shift->{'status'}#;$dl=bless({$t,$Zk,$v,$cl,$x,$y},$z);$el={$Uk,$Xk,$Yk,$dl};$fl=q#/io/pid_readers.b#;$gl=bless({$n5,$Tk,$k6,$q,$l6,$q,$m6,$el,$J,$fl},$v6);$hl={};$il=[];$jl=q#shift->await#;$kl=bless({$t,$il,$v,$jl,$x,$y},$z);$ll=[];$ml=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$nl=bless({$t,$ll,$v,$ml,$x,$y},$z);$ol={$h7,$nl};$pl=q#/io/pid_init.b#;$ql=bless({$n5,$hl,$k6,$q,$l6,$kl,$m6,$ol,$J,$pl},$v6);$rl={};$sl=q#await#;$tl=[];$ul=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$vl=bless({$t,$tl,$v,$ul,$x,$y},$z);$wl=q#running#;$xl=[];$yl=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$zl=bless({$t,$xl,$v,$yl,$x,$y},$z);$Al={$sl,$vl,$wl,$zl};$Bl=q#/io/pid_wait.b#;$Cl=bless({$n5,$rl,$k6,$q,$l6,$q,$m6,$Al,$J,$Bl},$v6);$Dl={};$El=[];$Fl=q#shift->stdout->read(@_)#;$Gl=bless({$t,$El,$v,$Fl,$x,$y},$z);$Hl=[];$Il=q#shift->stdin->write(@_)#;$Jl=bless({$t,$Hl,$v,$Il,$x,$y},$z);$Kl={$Bc,$Gl,$Jc,$Jl};$Ll=q#/io/pid_io.b#;$Ml=bless({$n5,$Dl,$k6,$q,$l6,$q,$m6,$Kl,$J,$Ll},$v6);$Nl={};$Ol=[];$Pl=q#$_[0]->{external_fds}{$_[1]}#;$Ql=bless({$t,$Ol,$v,$Pl,$x,$y},$z);$Rl=[];$Sl=q#shift->fd(2)#;$Tl=bless({$t,$Rl,$v,$Sl,$x,$y},$z);$Ul=[];$Vl=q#shift->fd(0)#;$Wl=bless({$t,$Ul,$v,$Vl,$x,$y},$z);$Xl=[];$Yl=q#shift->fd(1)#;$Zl=bless({$t,$Xl,$v,$Yl,$x,$y},$z);$cm={$Ie,$Ql,$Me,$Tl,$Qe,$Wl,$Ue,$Zl};$dm=q#/io/pid_accessors.b#;$em=bless({$n5,$Nl,$k6,$q,$l6,$q,$m6,$cm,$J,$dm},$v6);$fm=[$sc,$gl,$ql,$Cl,$Ml,$em];$gm=bless({$n5,$Sk,$J,$m3,$d6,$fm},$A5);$hm=q#ni:/io/pid.c#;$im={$A5,1};$jm=q#/io/pid.c#;$km=[$md];$lm=bless({$n5,$im,$J,$jm,$d6,$km},$e6);$mm=q#ni:/io/pid_accessors.b#;$nm=q#ni:/io/pid_init.b#;$om=q#ni:/io/pid_io.b#;$pm=q#ni:/io/pid_readers.b#;$qm=q#ni:/io/pid_wait.b#;$rm=q#ni:/io/str#;$sm={$y7,1};$tm=q#/io/str#;$um={};$vm=q#data#;$wm=[];$xm=q#shift->{'data'}#;$ym=bless({$t,$wm,$v,$xm,$x,$y},$z);$zm=q#end#;$Am=[];$Bm=q#shift->{'end'}#;$Cm=bless({$t,$Am,$v,$Bm,$x,$y},$z);$Dm=q#start#;$Em=[];$Fm=q#shift->{'start'}#;$Gm=bless({$t,$Em,$v,$Fm,$x,$y},$z);$Hm={$vm,$ym,$zm,$Cm,$Dm,$Gm};$Im=q#/io/str_ro.b#;$Jm=bless({$n5,$um,$k6,$q,$l6,$q,$m6,$Hm,$J,$Im},$v6);$Km={};$Lm=[];$Mm=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Nm=bless({$t,$Lm,$v,$Mm,$x,$y},$z);$Om={$h7,$Nm};$Pm=q#/io/str_init.b#;$Qm=bless({$n5,$Km,$k6,$q,$l6,$q,$m6,$Om,$J,$Pm},$v6);$Rm={};$Sm=[];$Tm=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Um=bless({$t,$Sm,$v,$Tm,$x,$y},$z);$Vm=q#remaining#;$Wm=[];$Xm=q#my $self = shift; $$self{end} - $$self{start}#;$Ym=bless({$t,$Wm,$v,$Xm,$x,$y},$z);$Zm=[];$cn=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$dn=bless({$t,$Zm,$v,$cn,$x,$y},$z);$en={$Bc,$Um,$Vm,$Ym,$Jc,$dn};$fn=q#/io/str_io.b#;$gn=bless({$n5,$Rm,$k6,$q,$l6,$q,$m6,$en,$J,$fn},$v6);$hn=[$sc,$Jm,$Qm,$gn];$in=bless({$n5,$sm,$J,$tm,$d6,$hn},$B5);$jn=q#ni:/io/str.c#;$kn={$B5,1};$ln=q#/io/str.c#;$mn=[$md];$nn=bless({$n5,$kn,$J,$ln,$d6,$mn},$e6);$on=q#ni:/io/str_init.b#;$pn=q#ni:/io/str_io.b#;$qn=q#ni:/io/str_ro.b#;$rn=q#ni:/io/transfer#;$sn={$z7,1,$A7,1,$B7,1};$tn=q#/io/transfer#;$un={$z7,1,$A7,1,$B7,1,$M7,1};$vn=q#/semantic/task#;$wn={};$xn=[];$yn=q#shift->{'outcome'}#;$zn=bless({$t,$xn,$v,$yn,$x,$y},$z);$An={$r,$zn};$Bn=q#/semantic/task_ro.b#;$Cn=bless({$n5,$wn,$k6,$q,$l6,$q,$m6,$An,$J,$Bn},$v6);$Dn={};$En=q#failure#;$Fn=[];$Gn=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Hn=bless({$t,$Fn,$v,$Gn,$x,$y},$z);$In=q#success#;$Jn=[];$Kn=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Ln=bless({$t,$Jn,$v,$Kn,$x,$y},$z);$Mn={$En,$Hn,$In,$Ln};$Nn=q#/semantic/task_outcome.b#;$On=bless({$n5,$Dn,$k6,$q,$l6,$q,$m6,$Mn,$J,$Nn},$v6);$Pn=[$Z7,$Cn,$On];$Qn=bless({$n5,$un,$J,$vn,$d6,$Pn},$Z5);$Rn={};$Sn=[];$Tn=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Un=bless({$t,$Sn,$v,$Tn,$x,$y},$z);$Vn=[];$Wn=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Xn=bless({$t,$Vn,$v,$Wn,$x,$y},$z);$Yn=[];$Zn=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$co=bless({$t,$Yn,$v,$Zn,$x,$y},$z);$do={$Bc,$Xn,$Jc,$co};$eo=q#/io/transfer_io_interop.b#;$fo=bless({$n5,$Rn,$k6,$Un,$l6,$q,$m6,$do,$J,$eo},$v6);$go={};$ho=q#pressure#;$io=[];$jo=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$ko=bless({$t,$io,$v,$jo,$x,$y},$z);$lo=q#read_limit_throughput#;$mo=[];$no=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$oo=bless({$t,$mo,$v,$no,$x,$y},$z);$po=q#throughput#;$qo=[];$ro=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$so=bless({$t,$qo,$v,$ro,$x,$y},$z);$to=q#write_limit_throughput#;$uo=[];$vo=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$wo=bless({$t,$uo,$v,$vo,$x,$y},$z);$xo={$ho,$ko,$lo,$oo,$po,$so,$to,$wo};$yo=q#/io/transfer_io_measurement.b#;$zo=bless({$n5,$go,$k6,$q,$l6,$q,$m6,$xo,$J,$yo},$v6);$Ao=[$Qn,$fo,$zo];$Bo=bless({$n5,$sn,$J,$tn,$d6,$Ao},$C5);$Co=[];$Do=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Eo=bless({$t,$Co,$v,$Do,$x,$y},$z);$Fo=q#ni:/io/transfer.c#;$Go={$C5,1,$D5,1,$E5,1};$Ho=q#/io/transfer.c#;$Io={$C5,1,$D5,1,$E5,1,$Z5,1};$Jo=q#/semantic/task.c#;$Ko=[$E9];$Lo=bless({$n5,$Io,$J,$Jo,$d6,$Ko},$e6);$Mo={};$No={};$Oo=q#/io/transfer.c_into.b#;$Po=bless({$n5,$Mo,$k6,$Eo,$l6,$q,$m6,$No,$J,$Oo},$v6);$Qo=[$Lo,$Po];$Ro=bless({$n5,$Go,$J,$Ho,$d6,$Qo},$e6);$So=q#ni:/io/transfer.c_into.b#;$To=q#ni:/io/transfer_async#;$Uo={$A7,1};$Vo=q#/io/transfer_async#;$Wo={};$Xo=q#dest_io#;$Yo=[];$Zo=q#shift->{'dest_io'}#;$cp=bless({$t,$Yo,$v,$Zo,$x,$y},$z);$dp=q#id#;$ep=[];$fp=q#shift->{'id'}#;$gp=bless({$t,$ep,$v,$fp,$x,$y},$z);$hp=q#source_io#;$ip=[];$jp=q#shift->{'source_io'}#;$kp=bless({$t,$ip,$v,$jp,$x,$y},$z);$lp={$Xo,$cp,$dp,$gp,$hp,$kp};$mp=q#/io/transfer_async_ro.b#;$np=bless({$n5,$Wo,$k6,$q,$l6,$q,$m6,$lp,$J,$mp},$v6);$op={};$pp=[];$qp=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$rp=bless({$t,$pp,$v,$qp,$x,$y},$z);$sp={$h7,$rp};$tp=q#/io/transfer_async_init.b#;$up=bless({$n5,$op,$k6,$q,$l6,$q,$m6,$sp,$J,$tp},$v6);$vp={};$wp=[];$xp=q#ni('ni:/io/transfer_async')->track(shift)#;$yp=bless({$t,$wp,$v,$xp,$x,$y},$z);$zp=[];$Ap=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Bp=bless({$t,$zp,$v,$Ap,$x,$y},$z);$Cp={};$Dp=q#/io/transfer_async_lifecycle.b#;$Ep=bless({$n5,$vp,$k6,$yp,$l6,$Bp,$m6,$Cp,$J,$Dp},$v6);$Fp={};$Gp=q#run#;$Hp=[];$Ip=q#shift#;$Jp=bless({$t,$Hp,$v,$Ip,$x,$y},$z);$Kp=q#run_async#;$Lp=[];$Mp=q#my $self = shift;
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

$self;#;$Np=bless({$t,$Lp,$v,$Mp,$x,$y},$z);$Op={$Gp,$Jp,$Kp,$Np};$Pp=q#/io/transfer_async_run.b#;$Qp=bless({$n5,$Fp,$k6,$q,$l6,$q,$m6,$Op,$J,$Pp},$v6);$Rp=[$Bo,$np,$up,$Ep,$Qp];$Sp=q#tracked_transfers#;$Tp={};$Up=q#transfer_id#;$Vp=bless({$n5,$Uo,$J,$Vo,$d6,$Rp,$Sp,$Tp,$Up,0},$D5);$Wp=[];$Xp=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Yp=bless({$t,$Wp,$v,$Xp,$x,$y},$z);$Zp=q#ni:/io/transfer_async.c#;$cq={$D5,1};$dq=q#/io/transfer_async.c#;$eq={};$fq=q#new_id#;$gq=[];$hq=q#++shift->{transfer_id}#;$iq=bless({$t,$gq,$v,$hq,$x,$y},$z);$jq=q#track#;$kq=[];$lq=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$mq=bless({$t,$kq,$v,$lq,$x,$y},$z);$nq=q#untrack#;$oq=[];$pq=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$qq=bless({$t,$oq,$v,$pq,$x,$y},$z);$rq={$fq,$iq,$jq,$mq,$nq,$qq};$sq=q#/io/transfer_async.c_tracker.b#;$tq=bless({$n5,$eq,$k6,$Yp,$l6,$q,$m6,$rq,$J,$sq},$v6);$uq=[$Ro,$tq];$vq=bless({$n5,$cq,$J,$dq,$d6,$uq},$e6);$wq=q#ni:/io/transfer_async.c_tracker.b#;$xq=q#ni:/io/transfer_async_init.b#;$yq=q#ni:/io/transfer_async_lifecycle.b#;$zq=q#ni:/io/transfer_async_ro.b#;$Aq=q#ni:/io/transfer_async_run.b#;$Bq=q#ni:/io/transfer_io_interop.b#;$Cq=q#ni:/io/transfer_io_measurement.b#;$Dq=q#ni:/io/transfer_sync#;$Eq={$B7,1};$Fq=q#/io/transfer_sync#;$Gq={};$Hq=[];$Iq=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Jq=bless({$t,$Hq,$v,$Iq,$x,$y},$z);$Kq={$h7,$Jq};$Lq=q#/io/transfer_sync_init.b#;$Mq=bless({$n5,$Gq,$k6,$q,$l6,$q,$m6,$Kq,$J,$Lq},$v6);$Nq={};$Oq=[];$Pq=q#my $self = shift;
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
$self->success;#;$Qq=bless({$t,$Oq,$v,$Pq,$x,$y},$z);$Rq={$Gp,$Qq};$Sq=q#/io/transfer_sync_run.b#;$Tq=bless({$n5,$Nq,$k6,$q,$l6,$q,$m6,$Rq,$J,$Sq},$v6);$Uq=[$Bo,$Mq,$Tq];$Vq=bless({$n5,$Eq,$J,$Fq,$d6,$Uq},$E5);$Wq=q#ni:/io/transfer_sync.c#;$Xq={$E5,1};$Yq=q#/io/transfer_sync.c#;$Zq=[$Ro];$cr=bless({$n5,$Xq,$J,$Yq,$d6,$Zq},$e6);$dr=q#ni:/io/transfer_sync_init.b#;$er=q#ni:/io/transfer_sync_run.b#;$fr=q#ni:/lib/accessor.b#;$gr=q#ni:/lib/behavior#;$hr=q#ni:/lib/behavior.c#;$ir=q#ni:/lib/branch#;$jr={$x6,1};$kr=q#/lib/branch#;$lr={};$mr=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$nr=bless({$v,$mr},$z);$or={$h7,$nr};$pr=q#/lib/branch_init.b#;$qr=bless({$n5,$lr,$k6,$q,$l6,$q,$m6,$or,$J,$pr},$v6);$rr=[$l8,$D6,$w6,$qr,$d9];$sr=bless({$n5,$jr,$J,$kr,$d6,$rr},$G5);$tr=q#ni:/lib/branch.b#;$ur=q#ni:/lib/branch.c#;$vr={$G5,1};$wr=q#/lib/branch.c#;$xr=[$I9];$yr=bless({$n5,$vr,$J,$wr,$d6,$xr},$e6);$zr=q#ni:/lib/branch_init.b#;$Ar=q#ni:/lib/class_init.b#;$Br=q#ni:/lib/dataslice#;$Cr={$D7,1};$Dr=q#/lib/dataslice#;$Er={};$Fr=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Gr=bless({$v,$Fr},$z);$Hr={$h7,$Gr};$Ir=q#/lib/dataslice_init.b#;$Jr=bless({$n5,$Er,$k6,$q,$l6,$q,$m6,$Hr,$J,$Ir},$v6);$Kr={};$Lr=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Mr=bless({$v,$Lr},$z);$Nr={$q6,$Mr};$Or=q#/lib/dataslice_apply.b#;$Pr=bless({$n5,$Kr,$k6,$q,$l6,$q,$m6,$Nr,$J,$Or},$v6);$Qr=[$l8,$Jr,$Pr];$Rr=bless({$n5,$Cr,$J,$Dr,$d6,$Qr},$H5);$Sr=q#ni:/lib/dataslice.c#;$Tr={$H5,1};$Ur=q#/lib/dataslice.c#;$Vr=[$I9];$Wr=bless({$n5,$Tr,$J,$Ur,$d6,$Vr},$e6);$Xr=q#ni:/lib/dataslice_apply.b#;$Yr=q#ni:/lib/dataslice_init.b#;$Zr=q#ni:/lib/definition.b#;$cs=q#ni:/lib/definition_def.b#;$ds=q#ni:/lib/definition_defdata.b#;$es=q#ni:/lib/doc#;$fs={$L,1};$gs={};$hs=q#shift; +{name => shift, doc => []}#;$is=bless({$v,$hs},$z);$js={$h7,$is};$ks=q#/lib/doc_init.b#;$ls=bless({$n5,$gs,$k6,$q,$l6,$q,$m6,$js,$J,$ks},$v6);$ms={};$ns=q#'ni.doc'#;$os=bless({$v,$ns},$z);$ps={$G6,$os};$qs=q#/lib/doc_namespace.b#;$rs=bless({$n5,$ms,$k6,$q,$l6,$q,$m6,$ps,$J,$qs},$v6);$ss={};$ts=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;#;$us=bless({$v,$ts},$z);$vs={$za,$us};$ws=q#/lib/doc_define.b#;$xs=bless({$n5,$ss,$k6,$q,$l6,$q,$m6,$vs,$J,$ws},$v6);$ys={};$zs=q#shift->referent#;$As=bless({$v,$zs},$z);$Bs=q#referent#;$Cs=q#ni 'ni:' . shift->{name}#;$Ds=bless({$v,$Cs},$z);$Es={$zm,$As,$Bs,$Ds};$Fs=q#/lib/doc_end.b#;$Gs=bless({$n5,$ys,$k6,$q,$l6,$q,$m6,$Es,$J,$Fs},$v6);$Hs={};$Is=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$Js=bless({$v,$Is},$z);$Ks=q#linearized#;$Ls=q#map @$_, @{shift->{doc}}#;$Ms=bless({$v,$Ls},$z);$Ns=q#tests#;$Os=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$Ps=bless({$v,$Os},$z);$Qs={$Q2,$Js,$Ks,$Ms,$Ns,$Ps};$Rs=q#/lib/doc_test.b#;$Ss=bless({$n5,$Hs,$k6,$q,$l6,$q,$m6,$Qs,$J,$Rs},$v6);$Ts=[$Z7,$D6,$ls,$rs,$xs,$Gs,$Ss];$Us=bless({$n5,$fs,$J,$N3,$d6,$Ts},$I5);$Vs=q#ni:/lib/doc.c#;$Ws={$I5,1};$Xs=q#/lib/doc.c#;$Ys=[$E9];$Zs=bless({$n5,$Ws,$J,$Xs,$d6,$Ys},$e6);$ct=q#ni:/lib/doc_define.b#;$dt=q#ni:/lib/doc_end.b#;$et=q#ni:/lib/doc_init.b#;$ft=q#ni:/lib/doc_namespace.b#;$gt=q#ni:/lib/doc_test.b#;$ht=q#ni:/lib/documentable.b#;$it=q#ni:/lib/fn#;$jt={$z,1};$kt=q#/lib/fn#;$lt={};$mt=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$nt=bless({$v,$mt,$x,$y},$z);$ot=q#compile#;$pt=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$qt=bless({$v,$pt},$z);$rt=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$st=bless({$v,$rt,$x,$y},$z);$tt={$ot,$qt,$h7,$st};$ut=q#/lib/fn_init.b#;$vt=bless({$n5,$lt,$k6,$q,$l6,$nt,$m6,$tt,$J,$ut},$v6);$wt={};$xt=[];$yt=q#shift->{'annotations'}#;$zt=bless({$t,$xt,$v,$yt,$x,$y},$z);$At=[];$Bt=q#shift->{'code'}#;$Ct=bless({$t,$At,$v,$Bt,$x,$y},$z);$Dt=q#eval_number#;$Et=[];$Ft=q#shift->{'eval_number'}#;$Gt=bless({$t,$Et,$v,$Ft,$x,$y},$z);$Ht=q#fn#;$It=[];$Jt=q#shift->{'fn'}#;$Kt=bless({$t,$It,$v,$Jt,$x,$y},$z);$Lt={$t,$zt,$v,$Ct,$Dt,$Gt,$Ht,$Kt};$Mt=q#/lib/fn_ro.b#;$Nt=bless({$n5,$wt,$k6,$q,$l6,$q,$m6,$Lt,$J,$Mt},$v6);$Ot={};$Pt=[];$Qt=q#my $self = shift; "fn {$$self{code}}"#;$Rt=bless({$t,$Pt,$v,$Qt,$x,$y},$z);$St=[];$Tt=bless({$t,$St,$v,$O8,$x,$y},$z);$Ut={$G8,$Rt,$N8,$Tt};$Vt=q#/lib/fn_ops.b#;$Wt=bless({$n5,$Ot,$k6,$q,$l6,$q,$m6,$Ut,$J,$Vt},$v6);$Xt={};$Yt=q#serialize#;$Zt=[];$cu=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$du=bless({$t,$Zt,$v,$cu,$x,$y},$z);$eu={$Yt,$du};$fu=q#/lib/fn_serialize.b#;$gu=bless({$n5,$Xt,$k6,$q,$l6,$q,$m6,$eu,$J,$fu},$v6);$hu=[$Z7,$m9,$vt,$Nt,$Wt,$gu];$iu=bless({$n5,$jt,$J,$kt,$d6,$hu},$J5);$ju=[];$ku=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$lu=bless({$t,$ju,$v,$ku,$x,$y},$z);$mu=q#ni:/lib/fn.c#;$nu={$J5,1};$ou=q#/lib/fn.c#;$pu={};$qu=q#resolve_evals#;$ru=[];$su=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$tu=bless({$t,$ru,$v,$su,$x,$y},$z);$uu={$qu,$tu};$vu=q#/lib/fn.c_resolve_eval.b#;$wu=bless({$n5,$pu,$k6,$lu,$l6,$q,$m6,$uu,$J,$vu},$v6);$xu=[$E9,$wu];$yu=bless({$n5,$nu,$J,$ou,$d6,$xu},$e6);$zu=q#ni:/lib/fn.c_resolve_eval.b#;$Au=q#ni:/lib/fn_init.b#;$Bu=q#ni:/lib/fn_ops.b#;$Cu=q#ni:/lib/fn_ro.b#;$Du=q#ni:/lib/fn_serialize.b#;$Eu=q#ni:/lib/future#;$Fu={$E7,1};$Gu={};$Hu=[];$Iu=bless({$t,$Hu,$v,$yn,$x,$y},$z);$Ju=q#parents#;$Ku=[];$Lu=q#shift->{'parents'}#;$Mu=bless({$t,$Ku,$v,$Lu,$x,$y},$z);$Nu=q#v#;$Ou=[];$Pu=q#shift->{'v'}#;$Qu=bless({$t,$Ou,$v,$Pu,$x,$y},$z);$Ru={$r,$Iu,$Ju,$Mu,$Nu,$Qu};$Su=q#/lib/future_ro.b#;$Tu=bless({$n5,$Gu,$k6,$q,$l6,$q,$m6,$Ru,$J,$Su},$v6);$Uu={};$Vu=[];$Wu=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$Xu=bless({$t,$Vu,$v,$Wu,$x,$y},$z);$Yu={$h7,$Xu};$Zu=q#/lib/future_init.b#;$cv=bless({$n5,$Uu,$k6,$q,$l6,$q,$m6,$Yu,$J,$Zu},$v6);$dv={};$ev=q#decide#;$fv=[];$gv=q#local $_;
my ($self, $v) = @_;
die "ni:/lib/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$hv=bless({$t,$fv,$v,$gv,$x,$y},$z);$iv=q#decided#;$jv=[];$kv=q#shift->{outcome}#;$lv=bless({$t,$jv,$v,$kv,$x,$y},$z);$mv=q#listener#;$nv=[];$ov=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$pv=bless({$t,$nv,$v,$ov,$x,$y},$z);$qv={$ev,$hv,$iv,$lv,$mv,$pv};$rv=q#/lib/future_state.b#;$sv=bless({$n5,$dv,$k6,$q,$l6,$q,$m6,$qv,$J,$rv},$v6);$tv={};$uv=q#and#;$vv=[];$wv=q#my $self   = $_[0];
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
$child;#;$xv=bless({$t,$vv,$v,$wv,$x,$y},$z);$yv=q#flatmap#;$zv=[];$Av=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$Bv=bless({$t,$zv,$v,$Av,$x,$y},$z);$Cv=q#map#;$Dv=[];$Ev=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$Fv=bless({$t,$Dv,$v,$Ev,$x,$y},$z);$Gv=q#or#;$Hv=[];$Iv=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Jv=bless({$t,$Hv,$v,$Iv,$x,$y},$z);$Kv={$uv,$xv,$yv,$Bv,$Cv,$Fv,$Gv,$Jv};$Lv=q#/lib/future_algebra.b#;$Mv=bless({$n5,$tv,$k6,$q,$l6,$q,$m6,$Kv,$J,$Lv},$v6);$Nv=[$Z7,$Tu,$cv,$sv,$Mv];$Ov=bless({$n5,$Fu,$J,$g4,$d6,$Nv},$K5);$Pv=q#ni:/lib/future.c#;$Qv={$K5,1};$Rv=q#/lib/future.c#;$Sv=[$E9];$Tv=bless({$n5,$Qv,$J,$Rv,$d6,$Sv},$e6);$Uv=q#ni:/lib/future_algebra.b#;$Vv=q#ni:/lib/future_init.b#;$Wv=q#ni:/lib/future_ro.b#;$Xv=q#ni:/lib/future_state.b#;$Yv=q#ni:/lib/gensym_generator_compact.b#;$Zv={};$cw=q#gensym#;$dw=[];$ew=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$fw=bless({$t,$dw,$v,$ew,$x,$y},$z);$gw={$cw,$fw};$hw=q#/lib/gensym_generator_compact.b#;$iw=bless({$n5,$Zv,$k6,$q,$l6,$q,$m6,$gw,$J,$hw},$v6);$jw=q#ni:/lib/global_static_test.b#;$kw={};$lw=[];$mw=q#ni('ni:/lib/test_case')->new(shift)#;$nw=q#($)#;$ow=bless({$t,$lw,$v,$mw,$x,$nw},$z);$pw=q#now#;$qw=[];$rw=q#ni('ni:/lib/test_value')->new(shift)#;$sw=bless({$t,$qw,$v,$rw,$x,$nw},$z);$tw={$Q2,$ow,$pw,$sw};$uw=q#/lib/global_static_test.b#;$vw=bless({$n5,$kw,$k6,$q,$l6,$q,$m6,$tw,$J,$uw},$v6);$ww=q#ni:/lib/image#;$xw={$F7,1};$yw={};$zw=[];$Aw=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Bw=bless({$t,$zw,$v,$Aw,$x,$y},$z);$Cw={$h7,$Bw};$Dw=q#/lib/image_init.b#;$Ew=bless({$n5,$yw,$k6,$q,$l6,$q,$m6,$Cw,$J,$Dw},$v6);$Fw={};$Gw=q#boot_side_effect#;$Hw=[];$Iw=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Jw=bless({$t,$Hw,$v,$Iw,$x,$y},$z);$Kw=q#finalizer#;$Lw=[];$Mw=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Nw=bless({$t,$Lw,$v,$Mw,$x,$y},$z);$Ow=q#io#;$Pw=[];$Qw=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Rw=bless({$t,$Pw,$v,$Qw,$x,$y},$z);$Sw=q#reconstruction#;$Tw=[];$Uw=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Vw=bless({$t,$Tw,$v,$Uw,$x,$y},$z);$Ww=q#side_effect#;$Xw=[];$Yw=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Zw=bless({$t,$Xw,$v,$Yw,$x,$y},$z);$cx={$Gw,$Jw,$Kw,$Nw,$Ow,$Rw,$Sw,$Vw,$Ww,$Zw};$dx=q#/lib/image_quoting.b#;$ex=bless({$n5,$Fw,$k6,$q,$l6,$q,$m6,$cx,$J,$dx},$v6);$fx={};$gx=q#quote_code#;$hx=[];$ix=q#shift->die('cannot quote perl CODE refs', shift)#;$jx=bless({$t,$hx,$v,$ix,$x,$y},$z);$kx={$gx,$jx};$lx=q#/lib/quote_code_fail.b#;$mx=bless({$n5,$fx,$k6,$q,$l6,$q,$m6,$kx,$J,$lx},$v6);$nx={};$ox=q#quote_array#;$px=[];$qx=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$rx=bless({$t,$px,$v,$qx,$x,$y},$z);$sx=q#quote_hash#;$tx=[];$ux=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$vx=bless({$t,$tx,$v,$ux,$x,$y},$z);$wx=q#quote_scalar#;$xx=[];$yx=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$zx=bless({$t,$xx,$v,$yx,$x,$y},$z);$Ax=q#quote_scalar_ref#;$Bx=[];$Cx=q#'\\\\' . shift->quote(${$_[0]})#;$Dx=bless({$t,$Bx,$v,$Cx,$x,$y},$z);$Ex=q#quote_value#;$Fx=[];$Gx=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Hx=bless({$t,$Fx,$v,$Gx,$x,$y},$z);$Ix={$ox,$rx,$sx,$vx,$wx,$zx,$Ax,$Dx,$Ex,$Hx};$Jx=q#/lib/quote_values.b#;$Kx=bless({$n5,$nx,$k6,$q,$l6,$q,$m6,$Ix,$J,$Jx},$v6);$Lx={};$Mx=q#quote_blessed#;$Nx=[];$Ox=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Px=bless({$t,$Nx,$v,$Ox,$x,$y},$z);$Qx=q#quote_class#;$Rx=[];$Sx=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if ni->exists("ni:$class");#;$Tx=bless({$t,$Rx,$v,$Sx,$x,$y},$z);$Ux=q#quote_object#;$Vx=[];$Wx=q#local $_;
my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer('&' . $self->quote($_) . "($q);")
  for @{ref($_[0]) . '::ctors'};
$q;#;$Xx=bless({$t,$Vx,$v,$Wx,$x,$y},$z);$Yx={$Mx,$Px,$Qx,$Tx,$Ux,$Xx};$Zx=q#/lib/quote_objects.b#;$cy=bless({$n5,$Lx,$k6,$q,$l6,$q,$m6,$Yx,$J,$Zx},$v6);$dy={};$ey=q#circular_arrayref#;$fy=[];$gy=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$hy=bless({$t,$fy,$v,$gy,$x,$y},$z);$iy=q#circular_hashref#;$jy=[];$ky=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$ly=bless({$t,$jy,$v,$ky,$x,$y},$z);$my=q#is_circular#;$ny=[];$oy=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$py=bless({$t,$ny,$v,$oy,$x,$y},$z);$qy={$ey,$hy,$iy,$ly,$my,$py};$ry=q#/lib/quote_circular_addressed.b#;$sy=bless({$n5,$dy,$k6,$q,$l6,$q,$m6,$qy,$J,$ry},$v6);$ty={};$uy=q#address#;$vy=[];$wy=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$xy=bless({$t,$vy,$v,$wy,$x,$y},$z);$yy=q#allocate_gensym#;$zy=[];$Ay=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$By=bless({$t,$zy,$v,$Ay,$x,$y},$z);$Cy=q#circular_links#;$Dy=[];$Ey=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Fy=bless({$t,$Dy,$v,$Ey,$x,$y},$z);$Gy=q#quote#;$Hy=[];$Iy=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Jy=bless({$t,$Hy,$v,$Iy,$x,$y},$z);$Ky={$uy,$xy,$yy,$By,$Cy,$Fy,$Gy,$Jy};$Ly=q#/lib/quote_gensym_identity.b#;$My=bless({$n5,$ty,$k6,$q,$l6,$q,$m6,$Ky,$J,$Ly},$v6);$Ny=[$Z7,$Ew,$ex,$mx,$Kx,$cy,$sy,$My,$iw];$Oy=bless({$n5,$xw,$J,$o4,$d6,$Ny},$L5);$Py=q#ni:/lib/image.c#;$Qy={$L5,1};$Ry=q#/lib/image.c#;$Sy=[$E9];$Ty=bless({$n5,$Qy,$J,$Ry,$d6,$Sy},$e6);$Uy=q#ni:/lib/image_init.b#;$Vy=q#ni:/lib/image_quoting.b#;$Wy=q#ni:/lib/instance.b#;$Xy=q#ni:/lib/instantiable.b#;$Yy=q#ni:/lib/json.b#;$Zy={};$cz=q#json_decode#;$dz=[];$ez=q#local $_;
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
wantarray ? @$r : $$r[0];#;$fz=bless({$t,$dz,$v,$ez,$x,$nw},$z);$gz=q#json_encode#;$hz=[];$iz=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$jz=bless({$t,$hz,$v,$iz,$x,$nw},$z);$kz=q#json_escape#;$lz=[];$mz=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$nz=bless({$t,$lz,$v,$mz,$x,$nw},$z);$oz=q#json_unescape#;$pz=[];$qz=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$rz=bless({$t,$pz,$v,$qz,$x,$nw},$z);$sz=q#json_unescape_one#;$tz=[];$uz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$vz=bless({$t,$tz,$v,$uz,$x,$nw},$z);$wz={$cz,$fz,$gz,$jz,$kz,$nz,$oz,$rz,$sz,$vz};$xz=q#/lib/json.b#;$yz=bless({$n5,$Zy,$k6,$q,$l6,$q,$m6,$wz,$J,$xz},$v6);$zz=q#ni:/lib/name_as_string.b#;$Az=q#ni:/lib/named.b#;$Bz=q#ni:/lib/named_in_ni.b#;$Cz=q#ni:/lib/namespaced.b#;$Dz=q#ni:/lib/ni#;$Ez={$G7,1};$Fz={};$Gz=q#extend#;$Hz=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$Iz=bless({$v,$Hz,$x,$y},$z);$Jz=q#is_mutable#;$Kz=q#$0 ne '-' && -w $0#;$Lz=bless({$v,$Kz,$x,$y},$z);$Mz=q#modify#;$Nz=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$Oz=bless({$v,$Nz,$x,$y},$z);$Pz={$Gz,$Iz,$Jz,$Lz,$Mz,$Oz};$Qz=q#/lib/ni_self.b#;$Rz=bless({$n5,$Fz,$k6,$q,$l6,$q,$m6,$Pz,$J,$Qz},$v6);$Sz={};$Tz=q#--internal/+=#;$Uz=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted(use_newlines => 1);
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$Vz=bless({$v,$Uz,$x,$y},$z);$Wz=q#--internal/eval#;$Xz=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$Yz=bless({$v,$Xz,$x,$y},$z);$Zz=q#--internal/image#;$cA=q#shift->quoted(use_newlines => 1)->io->into_sync(ni"fd:1");
0;#;$dA=bless({$v,$cA,$x,$y},$z);$eA=q#--internal/test#;$fA=q#local $| = 1;
my $self   = shift;
my $failed = 0;
my @tests  = map ni($_)->tests, grep /^ni\\.doc:/, keys %{$$self{named}};
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
!!$failed;#;$gA=bless({$v,$fA,$x,$y},$z);$hA=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$iA=bless({$v,$hA,$x,$y},$z);$jA={$Tz,$Vz,$Wz,$Yz,$Zz,$dA,$eA,$gA,$Gp,$iA};$kA=q#/lib/ni_main.b#;$lA=bless({$n5,$Sz,$k6,$q,$l6,$q,$m6,$jA,$J,$kA},$v6);$mA={};$nA=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$oA=bless({$v,$nA,$x,$y},$z);$pA=q#resolver_for#;$qA=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$rA=bless({$v,$qA,$x,$y},$z);$sA={$U6,$oA,$pA,$rA};$tA=q#/lib/ni_resolver.b#;$uA=bless({$n5,$mA,$k6,$q,$l6,$q,$m6,$sA,$J,$tA},$v6);$vA={};$wA=q#exists#;$xA=q#exists $_[0]->{named}{$_[1]}#;$yA=bless({$v,$xA,$x,$y},$z);$zA=q#quoted#;$AA=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$BA=bless({$v,$AA,$x,$y},$z);$CA={$wA,$yA,$zA,$BA};$DA=q#/lib/ni_image.b#;$EA=bless({$n5,$vA,$k6,$q,$l6,$q,$m6,$CA,$J,$DA},$v6);$FA=[$Z7,$Rz,$lA,$uA,$EA];$GA=bless({$n5,$Ez,$J,$w4,$d6,$FA},$M5);$HA=q#ni:/lib/ni.c#;$IA={$M5,1};$JA=q#/lib/ni.c#;$KA=[$E9];$LA=bless({$n5,$IA,$J,$JA,$d6,$KA},$e6);$MA=q#ni:/lib/ni_image.b#;$NA=q#ni:/lib/ni_main.b#;$OA=q#ni:/lib/ni_resolver.b#;$PA=q#ni:/lib/ni_self.b#;$QA=q#ni:/lib/ni_static_util.b#;$RA={};$SA=q#abbrev#;$TA=[];$UA=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$VA=bless({$t,$TA,$v,$UA,$x,$y},$z);$WA=q#dor#;$XA=[];$YA=q#defined $_[0] ? $_[0] : $_[1]#;$ZA=bless({$t,$XA,$v,$YA,$x,$y},$z);$cB=q#indent#;$dB=[];$eB=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$fB=bless({$t,$dB,$v,$eB,$x,$y},$z);$gB=q#max#;$hB=[];$iB=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$jB=bless({$t,$hB,$v,$iB,$x,$y},$z);$kB=q#maxstr#;$lB=[];$mB=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$nB=bless({$t,$lB,$v,$mB,$x,$y},$z);$oB=q#mean#;$pB=[];$qB=q#sum(@_) / (@_ || 1)#;$rB=bless({$t,$pB,$v,$qB,$x,$y},$z);$sB=q#min#;$tB=[];$uB=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$vB=bless({$t,$tB,$v,$uB,$x,$y},$z);$wB=q#minstr#;$xB=[];$yB=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$zB=bless({$t,$xB,$v,$yB,$x,$y},$z);$AB=q#sgr#;$BB=[];$CB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$DB=bless({$t,$BB,$v,$CB,$x,$y},$z);$EB=q#sr#;$FB=[];$GB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$HB=bless({$t,$FB,$v,$GB,$x,$y},$z);$IB=q#sum#;$JB=[];$KB=q#local $_; my $x = 0; $x += $_ for @_; $x#;$LB=bless({$t,$JB,$v,$KB,$x,$y},$z);$MB=q#swap#;$NB=[];$OB=q#@_[0, 1] = @_[1, 0]#;$PB=bless({$t,$NB,$v,$OB,$x,$y},$z);$QB={$SA,$VA,$WA,$ZA,$cB,$fB,$gB,$jB,$kB,$nB,$oB,$rB,$sB,$vB,$wB,$zB,$AB,$DB,$EB,$HB,$IB,$LB,$MB,$PB};$RB=q#/lib/ni_static_util.b#;$SB=bless({$n5,$RA,$k6,$q,$l6,$q,$m6,$QB,$J,$RB},$v6);$TB=q#ni:/lib/perlbranch.b#;$UB=q#ni:/lib/quote_circular_addressed.b#;$VB=q#ni:/lib/quote_code_fail.b#;$WB=q#ni:/lib/quote_gensym_identity.b#;$XB=q#ni:/lib/quote_objects.b#;$YB=q#ni:/lib/quote_simple#;$ZB={$H7,1};$cC={};$dC=[];$eC=q#+{}#;$fC=bless({$t,$dC,$v,$eC,$x,$y},$z);$gC={$h7,$fC};$hC=q#/lib/quote_simple_init.b#;$iC=bless({$n5,$cC,$k6,$q,$l6,$q,$m6,$gC,$J,$hC},$v6);$jC={};$kC=[];$lC=bless({$t,$kC,$v,0,$x,$y},$z);$mC=[];$nC=q#shift->quote_value(shift)#;$oC=bless({$t,$mC,$v,$nC,$x,$y},$z);$pC={$my,$lC,$Gy,$oC};$qC=q#/lib/quote_simple_quote.b#;$rC=bless({$n5,$jC,$k6,$q,$l6,$q,$m6,$pC,$J,$qC},$v6);$sC=[$Z7,$iC,$rC,$mx,$Kx,$cy];$tC=bless({$n5,$ZB,$J,$H4,$d6,$sC},$N5);$uC=q#ni:/lib/quote_simple.c#;$vC={$N5,1};$wC=q#/lib/quote_simple.c#;$xC=[$E9];$yC=bless({$n5,$vC,$J,$wC,$d6,$xC},$e6);$zC=q#ni:/lib/quote_simple_init.b#;$AC=q#ni:/lib/quote_simple_quote.b#;$BC=q#ni:/lib/quote_values.b#;$CC=q#ni:/lib/ref_eq.b#;$DC=q#ni:/lib/resolver.b#;$EC=q#ni:/lib/slice#;$FC={$v6,1};$GC=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_($p);#;$HC=bless({$v,$GC},$z);$IC=q#local $_;
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
$self;#;$JC=bless({$v,$IC},$z);$KC=q#lib/slice::apply#;$LC=q#lib/slice::apply_#;$MC={};$NC=q#apply_#;$OC={$q6,$HC,$NC,$JC};$PC=q#/lib/slice.b#;$QC=bless({$n5,$MC,$m6,$OC,$J,$PC},$v6);$RC={};$SC=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$TC=bless({$v,$SC,$x,$y},$z);$UC={$h7,$TC};$VC=q#/lib/slice_init.b#;$WC=bless({$n5,$RC,$m6,$UC,$J,$VC},$v6);$XC={};$YC=[];$ZC=q#local $_;
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
$g;#;$cD=bless({$t,$YC,$v,$ZC,$x,$y},$z);$dD={$Yt,$cD};$eD=q#/lib/slice_serialize.b#;$fD=bless({$n5,$XC,$k6,$q,$l6,$q,$m6,$dD,$J,$eD},$v6);$gD=[$l8,$D6,$QC,$WC,$fD];$hD=bless({$n5,$FC,$J,$e5,$d6,$gD},$O5);$iD=q#ni:/lib/slice.b#;$jD=q#ni:/lib/slice.c#;$kD={$O5,1};$lD=q#/lib/slice.c#;$mD=[$I9];$nD=bless({$n5,$kD,$J,$lD,$d6,$mD},$e6);$oD=q#ni:/lib/slice_init.b#;$pD=q#ni:/lib/slice_serialize.b#;$qD=q#ni:/lib/static_fn.b#;$rD={};$sD=[];$tD=q#ni('ni:/lib/fn')->new(@_)#;$uD=bless({$t,$sD,$v,$tD,$x,$nw},$z);$vD=q#fp#;$wD=[];$xD=q#($$)#;$yD=bless({$t,$wD,$v,$tD,$x,$xD},$z);$zD={$Ht,$uD,$vD,$yD};$AD=q#/lib/static_fn.b#;$BD=bless({$n5,$rD,$k6,$q,$l6,$q,$m6,$zD,$J,$AD},$v6);$CD=q#ni:/lib/subclass.b#;$DD=q#ni:/lib/tag#;$ED={$E6,1};$FD=q#/lib/tag#;$GD={};$HD=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$ID=bless({$v,$HD,$x,$y},$z);$JD={$q6,$ID};$KD=q#/lib/tag.b#;$LD=bless({$n5,$GD,$k6,$q,$l6,$q,$m6,$JD,$J,$KD},$v6);$MD={};$ND=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$OD=bless({$v,$ND,$x,$y},$z);$PD={$h7,$OD};$QD=q#/lib/tag_init.b#;$RD=bless({$n5,$MD,$k6,$q,$l6,$q,$m6,$PD,$J,$QD},$v6);$SD=[$l8,$D6,$LD,$RD];$TD=bless({$n5,$ED,$J,$FD,$d6,$SD},$P5);$UD=q#ni:/lib/tag.b#;$VD=q#ni:/lib/tag.c#;$WD={$P5,1};$XD=q#/lib/tag.c#;$YD=[$I9];$ZD=bless({$n5,$WD,$J,$XD,$d6,$YD},$e6);$cE=q#ni:/lib/tag_init.b#;$dE=q#ni:/lib/test_assert_eq#;$eE={$I7,1};$fE=q#/lib/test_assert_eq#;$gE={$I7,1,$J7,1};$hE=q#/lib/test_assertion#;$iE={};$jE=q#commit#;$kE=[];$lE=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$mE=bless({$t,$kE,$v,$lE,$x,$y},$z);$nE={$jE,$mE};$oE=q#/lib/test_assertion_commit.b#;$pE=bless({$n5,$iE,$k6,$q,$l6,$q,$m6,$nE,$J,$oE},$v6);$qE=[$Z7,$pE];$rE=bless({$n5,$gE,$J,$hE,$d6,$qE},$R5);$sE={};$tE=[];$uE=q#my ($class, $diff) = @_;
+{diff => $diff};#;$vE=bless({$t,$tE,$v,$uE,$x,$y},$z);$wE={$h7,$vE};$xE=q#/lib/test_assert_eq_init.b#;$yE=bless({$n5,$sE,$k6,$q,$l6,$q,$m6,$wE,$J,$xE},$v6);$zE={};$AE=[];$BE=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode $$self{diff}
              : "PASS";#;$CE=bless({$t,$AE,$v,$BE,$x,$y},$z);$DE=q#failed#;$EE=[];$FE=q#defined shift->{diff}#;$GE=bless({$t,$EE,$v,$FE,$x,$y},$z);$HE=q#result#;$IE=[];$JE=bless({$t,$IE,$v,$Ip,$x,$y},$z);$KE={$G8,$CE,$DE,$GE,$HE,$JE};$LE=q#/lib/test_assert_eq_result.b#;$ME=bless({$n5,$zE,$k6,$q,$l6,$q,$m6,$KE,$J,$LE},$v6);$NE=[$rE,$yE,$ME];$OE=bless({$n5,$eE,$J,$fE,$d6,$NE},$Q5);$PE=q#ni:/lib/test_assert_eq.c#;$QE={$Q5,1};$RE=q#/lib/test_assert_eq.c#;$SE={$Q5,1,$R5,1};$TE=q#/lib/test_assertion.c#;$UE=[$E9];$VE=bless({$n5,$SE,$J,$TE,$d6,$UE},$e6);$WE=[$VE];$XE=bless({$n5,$QE,$J,$RE,$d6,$WE},$e6);$YE=q#ni:/lib/test_assert_eq_init.b#;$ZE=q#ni:/lib/test_assert_eq_result.b#;$cF=q#ni:/lib/test_assertion#;$dF=q#ni:/lib/test_assertion.c#;$eF=q#ni:/lib/test_assertion_commit.b#;$fF=q#ni:/lib/test_case#;$gF={$B,1};$hF=q#/lib/test_case#;$iF=q#running_test#;$jF={};$kF=[];$lF=q#shift->{'assertions'}#;$mF=bless({$t,$kF,$v,$lF,$x,$y},$z);$nF=[];$oF=q#shift->{'test'}#;$pF=bless({$t,$nF,$v,$oF,$x,$y},$z);$qF={$n,$mF,$s,$pF};$rF=q#/lib/test_case_ro.b#;$sF=bless({$n5,$jF,$k6,$q,$l6,$q,$m6,$qF,$J,$rF},$v6);$tF={};$uF=[];$vF=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$wF=bless({$t,$uF,$v,$vF,$x,$y},$z);$xF={$p,$wF};$yF=q#/lib/test_case_rw.b#;$zF=bless({$n5,$tF,$k6,$q,$l6,$q,$m6,$xF,$J,$yF},$v6);$AF={};$BF=[];$CF=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$DF=bless({$t,$BF,$v,$CF,$x,$y},$z);$EF={$h7,$DF};$FF=q#/lib/test_case_init.b#;$GF=bless({$n5,$AF,$k6,$q,$l6,$q,$m6,$EF,$J,$FF},$v6);$HF={};$IF=[];$JF=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$KF=bless({$t,$IF,$v,$JF,$x,$y},$z);$LF=[];$MF=q#!shift->{outcome}->[0]#;$NF=bless({$t,$LF,$v,$MF,$x,$y},$z);$OF={$G8,$KF,$DE,$NF};$PF=q#/lib/test_case_metrics.b#;$QF=bless({$n5,$HF,$k6,$q,$l6,$q,$m6,$OF,$J,$PF},$v6);$RF={};$SF=q#done#;$TF=[];$UF=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$VF=bless({$t,$TF,$v,$UF,$x,$y},$z);$WF=[];$XF=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$YF=bless({$t,$WF,$v,$XF,$x,$y},$z);$ZF={$SF,$VF,$Gp,$YF};$cG=q#/lib/test_case_run.b#;$dG=bless({$n5,$RF,$k6,$q,$l6,$q,$m6,$ZF,$J,$cG},$v6);$eG=[$Z7,$sF,$zF,$GF,$QF,$dG];$fG=bless({$n5,$gF,$J,$hF,$iF,$q,$d6,$eG},$S5);$gG=[];$hG=q#shift->{running_test} = undef#;$iG=bless({$t,$gG,$v,$hG,$x,$y},$z);$jG=q#ni:/lib/test_case.c#;$kG={$S5,1};$lG=q#/lib/test_case.c#;$mG={};$nG=[];$oG=q#shift->{'running_test'}#;$pG=bless({$t,$nG,$v,$oG,$x,$y},$z);$qG={$iF,$pG};$rG=q#/lib/test_case.c_test_ro.b#;$sG=bless({$n5,$mG,$k6,$q,$l6,$q,$m6,$qG,$J,$rG},$v6);$tG={};$uG=q#with_test#;$vG=[];$wG=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$xG=bless({$t,$vG,$v,$wG,$x,$y},$z);$yG={$uG,$xG};$zG=q#/lib/test_case.c_test.b#;$AG=bless({$n5,$tG,$k6,$iG,$l6,$q,$m6,$yG,$J,$zG},$v6);$BG=[$E9,$sG,$AG];$CG=bless({$n5,$kG,$J,$lG,$d6,$BG},$e6);$DG=q#ni:/lib/test_case.c_test.b#;$EG=q#ni:/lib/test_case.c_test_ro.b#;$FG=q#ni:/lib/test_case_init.b#;$GG=q#ni:/lib/test_case_metrics.b#;$HG=q#ni:/lib/test_case_ro.b#;$IG=q#ni:/lib/test_case_run.b#;$JG=q#ni:/lib/test_case_rw.b#;$KG=q#ni:/lib/test_value#;$LG={$K7,1};$MG=q#/lib/test_value#;$NG={};$OG=[];$PG=q#\\$_[1]#;$QG=bless({$t,$OG,$v,$PG,$x,$y},$z);$RG={$h7,$QG};$SG=q#/lib/test_value_init.b#;$TG=bless({$n5,$NG,$k6,$q,$l6,$q,$m6,$RG,$J,$SG},$v6);$UG={};$VG=q#(==#;$WG=[];$XG=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$YG=bless({$t,$WG,$v,$XG,$x,$y},$z);$ZG=q#diff#;$cH=[];$dH=q#my ($self, $rhs) = @_;
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
  return {scalar_difference => [$lhs, $rhs]} if $lhs ne $rhs;
}
return undef;#;$eH=bless({$t,$cH,$v,$dH,$x,$y},$z);$fH={$VG,$YG,$ZG,$eH};$gH=q#/lib/test_value_eq.b#;$hH=bless({$n5,$UG,$k6,$q,$l6,$q,$m6,$fH,$J,$gH},$v6);$iH={};$jH=[];$kH=q#ni::json_encode ${$_[0]}#;$lH=bless({$t,$jH,$v,$kH,$x,$y},$z);$mH={$G8,$lH};$nH=q#/lib/test_value_str.b#;$oH=bless({$n5,$iH,$k6,$q,$l6,$q,$m6,$mH,$J,$nH},$v6);$pH=[$Z7,$TG,$hH,$oH];$qH=bless({$n5,$LG,$J,$MG,$d6,$pH},$T5);$rH=q#ni:/lib/test_value.c#;$sH={$T5,1};$tH=q#/lib/test_value.c#;$uH=[$E9];$vH=bless({$n5,$sH,$J,$tH,$d6,$uH},$e6);$wH=q#ni:/lib/test_value_eq.b#;$xH=q#ni:/lib/test_value_init.b#;$yH=q#ni:/lib/test_value_str.b#;$zH=q#ni:/metaclass#;$AH={$e6,1};$BH=q#/metaclass#;$CH=[$d7,$m9,$m7,$f9];$DH=bless({$n5,$AH,$J,$BH,$d6,$CH},$U5);$EH=q#ni:/metaclass.c#;$FH={$U5,1};$GH=q#/metaclass.c#;$HH=[$v9];$IH=bless({$n5,$FH,$J,$GH,$d6,$HH},$e6);$JH=q#ni:/module#;$KH=q#ni:/module.c#;$LH=q#ni:/object#;$MH=q#ni:/object.c#;$NH=q#ni:/semantic/dimension#;$OH={$X5,1};$PH=q#/semantic/dimension#;$QH=[$v9];$RH=bless({$n5,$OH,$J,$PH,$d6,$QH},$Y5);$SH=q#ni:/semantic/dimension.c#;$TH={$Y5,1};$UH=q#/semantic/dimension.c#;$VH=[$M9];$WH=bless({$n5,$TH,$J,$UH,$d6,$VH},$e6);$XH=q#ni:/semantic/task#;$YH=q#ni:/semantic/task.c#;$ZH=q#ni:/semantic/task_outcome.b#;$cI=q#ni:/semantic/task_ro.b#;$dI=q#ni:main#;$eI={$Zj,1};$fI=[$BD,$vw,$Yj];$gI=bless({$n5,$eI,$J,$Zj,$d6,$fI},$f6);$hI=q#ni:ni#;$iI={$X9,1};$jI={$X9,1};$kI=q#json_escapes#;$lI=q##;$mI=q#b#;$nI=q#	#;$oI=q#t#;$pI=q#
#;$qI=q#n#;$rI=q##;$sI=q#"#;$tI=q#/#;$uI=q#\\#;$vI={$lI,$mI,$nI,$oI,$pI,$qI,$rI,$qi,$sI,$sI,$tI,$tI,$uI,$uI};$wI=q#json_unescapes#;$xI={$sI,$sI,$tI,$tI,$uI,$uI,$mI,$lI,$qI,$pI,$qi,$rI,$oI,$nI};$yI={$kI,$vI,$wI,$xI};$zI=q#/lib/json_data.b#;$AI=bless({$n5,$jI,$vm,$yI,$J,$zI},$D7);$BI=[$AI,$yz,$SB];$CI=bless({$n5,$iI,$J,$X9,$d6,$BI},$f6);$DI={$d,$M,$P,$U,$V,$e1,$f1,$k1,$l1,$x1,$y1,$K1,$L1,$X1,$Y1,$m2,$n2,$I2,$J2,$O2,$P2,$n3,$o3,$u3,$v3,$O3,$P3,$h4,$i4,$p4,$q4,$x4,$y4,$I4,$J4,$f5,$g5,$l5,$m5,$v9,$w9,$M9,$N9,$ha,$ia,$ma,$na,$V9,$oa,$Ha,$Ia,$Ma,$Na,$xa,$Oa,$Fa,$Pa,$fa,$Qa,$Vc,$Wc,$od,$pd,$zc,$qd,$Tc,$rd,$Id,$Jd,$Nd,$Od,$zd,$Pd,$Gd,$Qd,$Cf,$Df,$Hf,$If,$kf,$Jf,$Af,$Kf,$ie,$Lf,$cf,$Mf,$Ce,$Nf,$Zd,$Of,$ch,$dh,$hh,$ih,$Dg,$jh,$Og,$kh,$fg,$lh,$Yg,$mh,$Wf,$nh,$ng,$oh,$Ii,$Ji,$Ni,$Oi,$Eh,$Pi,$di,$Qi,$Lh,$Ri,$Gi,$Si,$wh,$Ti,$li,$Ui,$pj,$qj,$uj,$vj,$nj,$wj,$ej,$xj,$Yj,$ck,$Ak,$Bk,$Fk,$Gk,$lk,$Hk,$yk,$Ik,$sc,$Jk,$md,$Kk,$kd,$Lk,$wb,$Mk,$Eb,$Nk,$Qb,$Ok,$cb,$Pk,$qc,$Qk,$ec,$Rk,$gm,$hm,$lm,$mm,$em,$nm,$ql,$om,$Ml,$pm,$gl,$qm,$Cl,$rm,$in,$jn,$nn,$on,$Qm,$pn,$gn,$qn,$Jm,$rn,$Bo,$Fo,$Ro,$So,$Po,$To,$Vp,$Zp,$vq,$wq,$tq,$xq,$up,$yq,$Ep,$zq,$np,$Aq,$Qp,$Bq,$fo,$Cq,$zo,$Dq,$Vq,$Wq,$cr,$dr,$Mq,$er,$Tq,$fr,$E8,$gr,$l8,$hr,$I9,$ir,$sr,$tr,$w6,$ur,$yr,$zr,$qr,$Ar,$m7,$Br,$Rr,$Sr,$Wr,$Xr,$Pr,$Yr,$Jr,$Zr,$d9,$cs,$u8,$ds,$Z8,$es,$Us,$Vs,$Zs,$ct,$xs,$dt,$Gs,$et,$ls,$ft,$rs,$gt,$Ss,$ht,$j8,$it,$iu,$mu,$yu,$zu,$wu,$Au,$vt,$Bu,$Wt,$Cu,$Nt,$Du,$gu,$Eu,$Ov,$Pv,$Tv,$Uv,$Mv,$Vv,$cv,$Wv,$Tu,$Xv,$sv,$Yv,$iw,$jw,$vw,$ww,$Oy,$Py,$Ty,$Uy,$Ew,$Vy,$ex,$Wy,$X7,$Xy,$m9,$Yy,$yz,$zz,$L8,$Az,$D6,$Bz,$L6,$Cz,$S6,$Dz,$GA,$HA,$LA,$MA,$EA,$NA,$lA,$OA,$uA,$PA,$Rz,$QA,$SB,$TB,$d7,$UB,$sy,$VB,$mx,$WB,$My,$XB,$cy,$YB,$tC,$uC,$yC,$zC,$iC,$AC,$rC,$BC,$Kx,$CC,$S8,$DC,$Z6,$EC,$hD,$iD,$QC,$jD,$nD,$oD,$WC,$pD,$fD,$qD,$BD,$CD,$t9,$DD,$TD,$UD,$LD,$VD,$ZD,$cE,$RD,$dE,$OE,$PE,$XE,$YE,$yE,$ZE,$ME,$cF,$rE,$dF,$VE,$eF,$pE,$fF,$fG,$jG,$CG,$DG,$AG,$EG,$sG,$FG,$GF,$GG,$QF,$HG,$sF,$IG,$dG,$JG,$zF,$KG,$qH,$rH,$vH,$wH,$hH,$xH,$TG,$yH,$oH,$zH,$DH,$EH,$IH,$JH,$f9,$KH,$K9,$LH,$Z7,$MH,$E9,$NH,$RH,$SH,$WH,$XH,$Qn,$YH,$Lo,$ZH,$On,$cI,$Cn,$dI,$gI,$hI,$CI};$EI=q#resolvers#;$FI=[];$GI=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$HI=bless({$t,$FI,$v,$GI,$x,$y},$z);$II=q#file#;$JI=[];$KI=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$LI=bless({$t,$JI,$v,$KI,$x,$y},$z);$MI=q#null#;$NI=[];$OI=q#ni('ni:/io/null')->new#;$PI=bless({$t,$NI,$v,$OI,$x,$y},$z);$QI=q#sh#;$RI=[];$SI=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$TI=bless({$t,$RI,$v,$SI,$x,$y},$z);$UI=q#str#;$VI=[];$WI=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$XI=bless({$t,$VI,$v,$WI,$x,$y},$z);$YI={$Ie,$HI,$II,$LI,$MI,$PI,$QI,$TI,$UI,$XI};$ZI=bless({$c,$DI,$EI,$YI},$G7);*$LC=\&$JC;*$KC=\&$HC;$w6->apply_($o5);$w6->apply_($p5);$w6->apply_($q5);$w6->apply_($r5);$w6->apply_($s5);$w6->apply_($t5);$w6->apply_($u5);$w6->apply_($v5);$w6->apply_($w5);$w6->apply_($x5);$w6->apply_($y5);$w6->apply_($z5);$w6->apply_($A5);$w6->apply_($B5);$w6->apply_($C5);$w6->apply_($D5);$w6->apply_($E5);$w6->apply_($F5);$w6->apply_($x6);$w6->apply_($G5);$w6->apply_($H5);$w6->apply_($I5);$w6->apply_($J5);$w6->apply_($K5);$w6->apply_($L5);$w6->apply_($M5);$w6->apply_($N5);$w6->apply_($O5);$w6->apply_($P5);$w6->apply_($Q5);$w6->apply_($R5);$w6->apply_($S5);$w6->apply_($T5);$w6->apply_($e6);$w6->apply_($U5);$w6->apply_($f6);$w6->apply_($V5);$w6->apply_($W5);$w6->apply_($X5);$w6->apply_($Y5);$w6->apply_($Z5);$D6->apply_($o5);$D6->apply_($p5);$D6->apply_($q5);$D6->apply_($r5);$D6->apply_($s5);$D6->apply_($t5);$D6->apply_($u5);$D6->apply_($v5);$D6->apply_($w5);$D6->apply_($x5);$D6->apply_($y5);$D6->apply_($z5);$D6->apply_($A5);$D6->apply_($B5);$D6->apply_($C5);$D6->apply_($D5);$D6->apply_($E5);$D6->apply_($F5);$D6->apply_($x6);$D6->apply_($G5);$D6->apply_($H5);$D6->apply_($L);$D6->apply_($I5);$D6->apply_($J5);$D6->apply_($K5);$D6->apply_($L5);$D6->apply_($M5);$D6->apply_($N5);$D6->apply_($v6);$D6->apply_($O5);$D6->apply_($E6);$D6->apply_($P5);$D6->apply_($Q5);$D6->apply_($R5);$D6->apply_($S5);$D6->apply_($T5);$D6->apply_($e6);$D6->apply_($U5);$D6->apply_($f6);$D6->apply_($V5);$D6->apply_($W5);$D6->apply_($X5);$D6->apply_($Y5);$D6->apply_($Z5);$L6->apply_($o5);$L6->apply_($p5);$L6->apply_($q5);$L6->apply_($r5);$L6->apply_($s5);$L6->apply_($t5);$L6->apply_($u5);$L6->apply_($v5);$L6->apply_($w5);$L6->apply_($x5);$L6->apply_($y5);$L6->apply_($z5);$L6->apply_($A5);$L6->apply_($B5);$L6->apply_($C5);$L6->apply_($D5);$L6->apply_($E5);$L6->apply_($F5);$L6->apply_($x6);$L6->apply_($G5);$L6->apply_($H5);$L6->apply_($I5);$L6->apply_($J5);$L6->apply_($K5);$L6->apply_($L5);$L6->apply_($M5);$L6->apply_($N5);$L6->apply_($v6);$L6->apply_($O5);$L6->apply_($E6);$L6->apply_($P5);$L6->apply_($Q5);$L6->apply_($R5);$L6->apply_($S5);$L6->apply_($T5);$L6->apply_($e6);$L6->apply_($U5);$L6->apply_($f6);$L6->apply_($V5);$L6->apply_($W5);$L6->apply_($X5);$L6->apply_($Y5);$L6->apply_($Z5);$S6->apply_($o5);$S6->apply_($p5);$S6->apply_($q5);$S6->apply_($r5);$S6->apply_($s5);$S6->apply_($t5);$S6->apply_($u5);$S6->apply_($v5);$S6->apply_($w5);$S6->apply_($x5);$S6->apply_($y5);$S6->apply_($z5);$S6->apply_($A5);$S6->apply_($B5);$S6->apply_($C5);$S6->apply_($D5);$S6->apply_($E5);$S6->apply_($F5);$S6->apply_($x6);$S6->apply_($G5);$S6->apply_($H5);$S6->apply_($I5);$S6->apply_($J5);$S6->apply_($K5);$S6->apply_($L5);$S6->apply_($M5);$S6->apply_($N5);$S6->apply_($v6);$S6->apply_($O5);$S6->apply_($E6);$S6->apply_($P5);$S6->apply_($Q5);$S6->apply_($R5);$S6->apply_($S5);$S6->apply_($T5);$S6->apply_($e6);$S6->apply_($U5);$S6->apply_($f6);$S6->apply_($V5);$S6->apply_($W5);$S6->apply_($X5);$S6->apply_($Y5);$S6->apply_($Z5);$Z6->apply_($o5);$Z6->apply_($p5);$Z6->apply_($q5);$Z6->apply_($r5);$Z6->apply_($s5);$Z6->apply_($t5);$Z6->apply_($u5);$Z6->apply_($v5);$Z6->apply_($w5);$Z6->apply_($x5);$Z6->apply_($y5);$Z6->apply_($z5);$Z6->apply_($A5);$Z6->apply_($B5);$Z6->apply_($C5);$Z6->apply_($D5);$Z6->apply_($E5);$Z6->apply_($F5);$Z6->apply_($x6);$Z6->apply_($G5);$Z6->apply_($H5);$Z6->apply_($I5);$Z6->apply_($J5);$Z6->apply_($K5);$Z6->apply_($L5);$Z6->apply_($M5);$Z6->apply_($N5);$Z6->apply_($O5);$Z6->apply_($E6);$Z6->apply_($P5);$Z6->apply_($Q5);$Z6->apply_($R5);$Z6->apply_($S5);$Z6->apply_($T5);$Z6->apply_($e6);$Z6->apply_($U5);$Z6->apply_($f6);$Z6->apply_($V5);$Z6->apply_($W5);$Z6->apply_($X5);$Z6->apply_($Y5);$Z6->apply_($Z5);$m7->apply_($o5);$m7->apply_($p5);$m7->apply_($q5);$m7->apply_($r5);$m7->apply_($s5);$m7->apply_($t5);$m7->apply_($u5);$m7->apply_($v5);$m7->apply_($w5);$m7->apply_($x5);$m7->apply_($y5);$m7->apply_($z5);$m7->apply_($A5);$m7->apply_($B5);$m7->apply_($C5);$m7->apply_($D5);$m7->apply_($E5);$m7->apply_($F5);$m7->apply_($G5);$m7->apply_($H5);$m7->apply_($I5);$m7->apply_($J5);$m7->apply_($K5);$m7->apply_($L5);$m7->apply_($M5);$m7->apply_($N5);$m7->apply_($O5);$m7->apply_($P5);$m7->apply_($Q5);$m7->apply_($R5);$m7->apply_($S5);$m7->apply_($T5);$m7->apply_($e6);$m7->apply_($U5);$m7->apply_($f6);$m7->apply_($V5);$m7->apply_($W5);$m7->apply_($X5);$m7->apply_($Y5);$m7->apply_($Z5);$X7->apply_($o5);$X7->apply_($p5);$X7->apply_($n7);$X7->apply_($q5);$X7->apply_($o7);$X7->apply_($r5);$X7->apply_($p7);$X7->apply_($s5);$X7->apply_($q7);$X7->apply_($t5);$X7->apply_($r7);$X7->apply_($u5);$X7->apply_($s7);$X7->apply_($v5);$X7->apply_($t7);$X7->apply_($w5);$X7->apply_($u7);$X7->apply_($x5);$X7->apply_($v7);$X7->apply_($y5);$X7->apply_($w7);$X7->apply_($z5);$X7->apply_($x7);$X7->apply_($A5);$X7->apply_($y7);$X7->apply_($B5);$X7->apply_($z7);$X7->apply_($C5);$X7->apply_($A7);$X7->apply_($D5);$X7->apply_($B7);$X7->apply_($E5);$X7->apply_($C7);$X7->apply_($F5);$X7->apply_($x6);$X7->apply_($G5);$X7->apply_($D7);$X7->apply_($H5);$X7->apply_($L);$X7->apply_($I5);$X7->apply_($z);$X7->apply_($J5);$X7->apply_($E7);$X7->apply_($K5);$X7->apply_($F7);$X7->apply_($L5);$X7->apply_($G7);$X7->apply_($M5);$X7->apply_($H7);$X7->apply_($N5);$X7->apply_($v6);$X7->apply_($O5);$X7->apply_($E6);$X7->apply_($P5);$X7->apply_($I7);$X7->apply_($Q5);$X7->apply_($J7);$X7->apply_($R5);$X7->apply_($B);$X7->apply_($S5);$X7->apply_($K7);$X7->apply_($T5);$X7->apply_($e6);$X7->apply_($U5);$X7->apply_($f6);$X7->apply_($V5);$X7->apply_($L7);$X7->apply_($W5);$X7->apply_($X5);$X7->apply_($Y5);$X7->apply_($M7);$X7->apply_($Z5);$j8->apply_($o5);$j8->apply_($p5);$j8->apply_($q5);$j8->apply_($r5);$j8->apply_($s5);$j8->apply_($t5);$j8->apply_($u5);$j8->apply_($v5);$j8->apply_($w5);$j8->apply_($x5);$j8->apply_($y5);$j8->apply_($z5);$j8->apply_($A5);$j8->apply_($B5);$j8->apply_($C5);$j8->apply_($D5);$j8->apply_($E5);$j8->apply_($C7);$j8->apply_($F5);$j8->apply_($x6);$j8->apply_($G5);$j8->apply_($D7);$j8->apply_($H5);$j8->apply_($I5);$j8->apply_($J5);$j8->apply_($K5);$j8->apply_($L5);$j8->apply_($M5);$j8->apply_($N5);$j8->apply_($v6);$j8->apply_($O5);$j8->apply_($E6);$j8->apply_($P5);$j8->apply_($Q5);$j8->apply_($R5);$j8->apply_($S5);$j8->apply_($T5);$j8->apply_($e6);$j8->apply_($U5);$j8->apply_($f6);$j8->apply_($V5);$j8->apply_($W5);$j8->apply_($X5);$j8->apply_($Y5);$j8->apply_($Z5);$u8->apply_($o5);$u8->apply_($p5);$u8->apply_($q5);$u8->apply_($r5);$u8->apply_($s5);$u8->apply_($t5);$u8->apply_($u5);$u8->apply_($v5);$u8->apply_($w5);$u8->apply_($x5);$u8->apply_($y5);$u8->apply_($z5);$u8->apply_($A5);$u8->apply_($B5);$u8->apply_($C5);$u8->apply_($D5);$u8->apply_($E5);$u8->apply_($F5);$u8->apply_($x6);$u8->apply_($G5);$u8->apply_($H5);$u8->apply_($I5);$u8->apply_($J5);$u8->apply_($K5);$u8->apply_($L5);$u8->apply_($M5);$u8->apply_($N5);$u8->apply_($O5);$u8->apply_($P5);$u8->apply_($Q5);$u8->apply_($R5);$u8->apply_($S5);$u8->apply_($T5);$u8->apply_($e6);$u8->apply_($U5);$u8->apply_($f6);$u8->apply_($V5);$u8->apply_($W5);$u8->apply_($X5);$u8->apply_($Y5);$u8->apply_($Z5);$E8->apply_($o5);$E8->apply_($p5);$E8->apply_($q5);$E8->apply_($r5);$E8->apply_($s5);$E8->apply_($t5);$E8->apply_($u5);$E8->apply_($v5);$E8->apply_($w5);$E8->apply_($x5);$E8->apply_($y5);$E8->apply_($z5);$E8->apply_($A5);$E8->apply_($B5);$E8->apply_($C5);$E8->apply_($D5);$E8->apply_($E5);$E8->apply_($F5);$E8->apply_($x6);$E8->apply_($G5);$E8->apply_($H5);$E8->apply_($I5);$E8->apply_($J5);$E8->apply_($K5);$E8->apply_($L5);$E8->apply_($M5);$E8->apply_($N5);$E8->apply_($O5);$E8->apply_($P5);$E8->apply_($Q5);$E8->apply_($R5);$E8->apply_($S5);$E8->apply_($T5);$E8->apply_($e6);$E8->apply_($U5);$E8->apply_($f6);$E8->apply_($V5);$E8->apply_($W5);$E8->apply_($X5);$E8->apply_($Y5);$E8->apply_($Z5);$L8->apply_($o5);$L8->apply_($p5);$L8->apply_($q5);$L8->apply_($r5);$L8->apply_($s5);$L8->apply_($t5);$L8->apply_($u5);$L8->apply_($v5);$L8->apply_($w5);$L8->apply_($x5);$L8->apply_($y5);$L8->apply_($z5);$L8->apply_($A5);$L8->apply_($B5);$L8->apply_($C5);$L8->apply_($D5);$L8->apply_($E5);$L8->apply_($F5);$L8->apply_($x6);$L8->apply_($G5);$L8->apply_($H5);$L8->apply_($I5);$L8->apply_($J5);$L8->apply_($K5);$L8->apply_($L5);$L8->apply_($M5);$L8->apply_($N5);$L8->apply_($O5);$L8->apply_($P5);$L8->apply_($Q5);$L8->apply_($R5);$L8->apply_($S5);$L8->apply_($T5);$L8->apply_($e6);$L8->apply_($U5);$L8->apply_($f6);$L8->apply_($V5);$L8->apply_($W5);$L8->apply_($X5);$L8->apply_($Y5);$L8->apply_($Z5);$S8->apply_($o5);$S8->apply_($p5);$S8->apply_($q5);$S8->apply_($r5);$S8->apply_($s5);$S8->apply_($t5);$S8->apply_($u5);$S8->apply_($v5);$S8->apply_($w5);$S8->apply_($x5);$S8->apply_($y5);$S8->apply_($z5);$S8->apply_($A5);$S8->apply_($B5);$S8->apply_($C5);$S8->apply_($D5);$S8->apply_($E5);$S8->apply_($F5);$S8->apply_($x6);$S8->apply_($G5);$S8->apply_($H5);$S8->apply_($I5);$S8->apply_($J5);$S8->apply_($K5);$S8->apply_($L5);$S8->apply_($M5);$S8->apply_($N5);$S8->apply_($O5);$S8->apply_($P5);$S8->apply_($Q5);$S8->apply_($R5);$S8->apply_($S5);$S8->apply_($T5);$S8->apply_($e6);$S8->apply_($U5);$S8->apply_($f6);$S8->apply_($V5);$S8->apply_($W5);$S8->apply_($X5);$S8->apply_($Y5);$S8->apply_($Z5);$Z8->apply_($o5);$Z8->apply_($p5);$Z8->apply_($q5);$Z8->apply_($r5);$Z8->apply_($s5);$Z8->apply_($t5);$Z8->apply_($u5);$Z8->apply_($v5);$Z8->apply_($w5);$Z8->apply_($x5);$Z8->apply_($y5);$Z8->apply_($z5);$Z8->apply_($A5);$Z8->apply_($B5);$Z8->apply_($C5);$Z8->apply_($D5);$Z8->apply_($E5);$Z8->apply_($F5);$Z8->apply_($x6);$Z8->apply_($G5);$Z8->apply_($H5);$Z8->apply_($I5);$Z8->apply_($J5);$Z8->apply_($K5);$Z8->apply_($L5);$Z8->apply_($M5);$Z8->apply_($N5);$Z8->apply_($O5);$Z8->apply_($P5);$Z8->apply_($Q5);$Z8->apply_($R5);$Z8->apply_($S5);$Z8->apply_($T5);$Z8->apply_($e6);$Z8->apply_($U5);$Z8->apply_($f6);$Z8->apply_($V5);$Z8->apply_($W5);$Z8->apply_($X5);$Z8->apply_($Y5);$Z8->apply_($Z5);$m9->apply_($o5);$m9->apply_($p5);$m9->apply_($q5);$m9->apply_($r5);$m9->apply_($s5);$m9->apply_($t5);$m9->apply_($u5);$m9->apply_($v5);$m9->apply_($w5);$m9->apply_($x5);$m9->apply_($y5);$m9->apply_($z5);$m9->apply_($A5);$m9->apply_($B5);$m9->apply_($C5);$m9->apply_($D5);$m9->apply_($E5);$m9->apply_($F5);$m9->apply_($G5);$m9->apply_($H5);$m9->apply_($I5);$m9->apply_($z);$m9->apply_($J5);$m9->apply_($K5);$m9->apply_($L5);$m9->apply_($M5);$m9->apply_($N5);$m9->apply_($v6);$m9->apply_($O5);$m9->apply_($E6);$m9->apply_($P5);$m9->apply_($Q5);$m9->apply_($R5);$m9->apply_($S5);$m9->apply_($T5);$m9->apply_($e6);$m9->apply_($U5);$m9->apply_($V5);$m9->apply_($W5);$m9->apply_($X5);$m9->apply_($Y5);$m9->apply_($Z5);$t9->apply_($o5);$t9->apply_($p5);$t9->apply_($q5);$t9->apply_($r5);$t9->apply_($s5);$t9->apply_($t5);$t9->apply_($u5);$t9->apply_($v5);$t9->apply_($w5);$t9->apply_($x5);$t9->apply_($y5);$t9->apply_($z5);$t9->apply_($A5);$t9->apply_($B5);$t9->apply_($C5);$t9->apply_($D5);$t9->apply_($E5);$t9->apply_($F5);$t9->apply_($G5);$t9->apply_($H5);$t9->apply_($I5);$t9->apply_($J5);$t9->apply_($K5);$t9->apply_($L5);$t9->apply_($M5);$t9->apply_($N5);$t9->apply_($O5);$t9->apply_($P5);$t9->apply_($Q5);$t9->apply_($R5);$t9->apply_($S5);$t9->apply_($T5);$t9->apply_($U5);$t9->apply_($V5);$t9->apply_($W5);$t9->apply_($X5);$t9->apply_($Y5);$t9->apply_($Z5);$V9->apply_($n7);$fa->apply_($n7);$xa->apply_($o7);$Fa->apply_($o7);$cb->apply_($p7);$cb->apply_($q7);$cb->apply_($r7);$cb->apply_($s7);$cb->apply_($t7);$cb->apply_($u7);$cb->apply_($v7);$cb->apply_($w7);$cb->apply_($x7);$cb->apply_($y7);$wb->apply_($p7);$wb->apply_($q7);$wb->apply_($r7);$wb->apply_($s7);$wb->apply_($t7);$wb->apply_($u7);$wb->apply_($v7);$wb->apply_($w7);$wb->apply_($x7);$wb->apply_($y7);$Eb->apply_($p7);$Eb->apply_($q7);$Eb->apply_($r7);$Eb->apply_($s7);$Eb->apply_($t7);$Eb->apply_($u7);$Eb->apply_($v7);$Eb->apply_($w7);$Eb->apply_($x7);$Eb->apply_($y7);$Qb->apply_($p7);$Qb->apply_($q7);$Qb->apply_($r7);$Qb->apply_($s7);$Qb->apply_($t7);$Qb->apply_($u7);$Qb->apply_($v7);$Qb->apply_($w7);$Qb->apply_($x7);$Qb->apply_($y7);$ec->apply_($p7);$ec->apply_($q7);$ec->apply_($r7);$ec->apply_($s7);$ec->apply_($t7);$ec->apply_($u7);$ec->apply_($v7);$ec->apply_($w7);$ec->apply_($x7);$ec->apply_($y7);$qc->apply_($p7);$qc->apply_($q7);$qc->apply_($r7);$qc->apply_($s7);$qc->apply_($t7);$qc->apply_($u7);$qc->apply_($v7);$qc->apply_($w7);$qc->apply_($x7);$qc->apply_($y7);$zc->apply_($p7);$Tc->apply_($p7);$kd->apply_($s5);$kd->apply_($t5);$kd->apply_($u5);$kd->apply_($v5);$kd->apply_($w5);$kd->apply_($x5);$kd->apply_($y5);$kd->apply_($z5);$kd->apply_($A5);$kd->apply_($B5);$zd->apply_($q7);$Gd->apply_($q7);$Zd->apply_($r7);$ie->apply_($r7);$Ce->apply_($r7);$cf->apply_($r7);$kf->apply_($r7);$Af->apply_($r7);$Wf->apply_($s7);$Wf->apply_($u7);$fg->apply_($s7);$ng->apply_($s7);$Dg->apply_($s7);$Dg->apply_($u7);$Og->apply_($s7);$Yg->apply_($s7);$Yg->apply_($u7);$wh->apply_($t7);$Eh->apply_($t7);$Lh->apply_($t7);$di->apply_($t7);$li->apply_($t7);$Gi->apply_($t7);$ej->apply_($u7);$nj->apply_($u7);$Yj->apply_($Zj);$lk->apply_($v7);$yk->apply_($v7);$gl->apply_($x7);$ql->apply_($x7);$Cl->apply_($x7);$Ml->apply_($x7);$em->apply_($x7);$Jm->apply_($y7);$Qm->apply_($y7);$gn->apply_($y7);$Cn->apply_($z7);$Cn->apply_($A7);$Cn->apply_($B7);$Cn->apply_($M7);$On->apply_($z7);$On->apply_($A7);$On->apply_($B7);$On->apply_($M7);$fo->apply_($z7);$fo->apply_($A7);$fo->apply_($B7);$zo->apply_($z7);$zo->apply_($A7);$zo->apply_($B7);$Po->apply_($C5);$Po->apply_($D5);$Po->apply_($E5);$np->apply_($A7);$up->apply_($A7);$Ep->apply_($A7);$Qp->apply_($A7);$tq->apply_($D5);$Mq->apply_($B7);$Tq->apply_($B7);$qr->apply_($x6);$Jr->apply_($D7);$Pr->apply_($D7);$ls->apply_($L);$rs->apply_($L);$xs->apply_($L);$Gs->apply_($L);$Ss->apply_($L);$vt->apply_($z);$Nt->apply_($z);$Wt->apply_($z);$gu->apply_($z);$wu->apply_($J5);$Tu->apply_($E7);$cv->apply_($E7);$sv->apply_($E7);$Mv->apply_($E7);$iw->apply_($F7);$vw->apply_($Zj);$Ew->apply_($F7);$ex->apply_($F7);$mx->apply_($F7);$mx->apply_($H7);$Kx->apply_($F7);$Kx->apply_($H7);$cy->apply_($F7);$cy->apply_($H7);$sy->apply_($F7);$My->apply_($F7);$yz->apply_($X9);$Rz->apply_($G7);$lA->apply_($G7);$uA->apply_($G7);$EA->apply_($G7);$SB->apply_($X9);$iC->apply_($H7);$rC->apply_($H7);$QC->apply_($v6);$WC->apply_($v6);$fD->apply_($v6);$BD->apply_($Zj);$LD->apply_($E6);$RD->apply_($E6);$pE->apply_($I7);$pE->apply_($J7);$yE->apply_($I7);$ME->apply_($I7);$sF->apply_($B);$zF->apply_($B);$GF->apply_($B);$QF->apply_($B);$dG->apply_($B);$sG->apply_($S5);$AG->apply_($S5);$TG->apply_($K7);$hH->apply_($K7);$oH->apply_($K7);$ni::self=$ZI;&$O($M);&$O($U);&$O($e1);&$O($k1);&$O($x1);&$O($K1);&$O($X1);&$O($m2);&$O($I2);&$O($O2);&$O($n3);&$O($u3);&$O($O3);&$O($h4);&$O($p4);&$O($x4);&$O($I4);&$O($f5);&$O($l5);&$O($w6);&$O($D6);&$O($L6);&$O($S6);&$O($Z6);&$O($d7);&$O($m7);&$O($X7);&$O($Z7);&$g7($Z7);&$O($j8);&$O($l8);&$g7($l8);&$O($u8);&$O($E8);&$O($L8);&$O($S8);&$O($Z8);&$O($d9);&$O($f9);&$g7($f9);&$O($m9);&$O($t9);&$O($v9);&$g7($v9);&$O($E9);&$g7($E9);&$O($I9);&$g7($I9);&$O($K9);&$g7($K9);&$O($M9);&$g7($M9);&$O($V9);&$O($fa);&$O($ha);&$g7($ha);&$O($ma);&$g7($ma);&$O($xa);&$O($Fa);&$O($Ha);&$g7($Ha);&$O($Ma);&$g7($Ma);&$O($cb);&$O($wb);&$O($Eb);&$O($Qb);&$O($ec);&$O($qc);&$O($sc);&$g7($sc);&$O($zc);&$O($Tc);&$O($Vc);&$g7($Vc);&$O($kd);&$O($md);&$g7($md);&$O($od);&$g7($od);&$O($zd);&$O($Gd);&$O($Id);&$g7($Id);&$O($Nd);&$g7($Nd);&$O($Zd);&$O($ie);&$O($Ce);&$O($cf);&$O($kf);&$O($Af);&$O($Cf);&$g7($Cf);&$O($Hf);&$g7($Hf);&$O($Wf);&$O($fg);&$O($ng);&$O($Dg);&$O($Og);&$O($Yg);&$O($ch);&$g7($ch);&$O($hh);&$g7($hh);&$O($wh);&$O($Eh);&$O($Lh);&$O($di);&$O($li);&$O($Gi);&$O($Ii);&$g7($Ii);&$O($Ni);&$g7($Ni);&$O($ej);&$O($nj);&$O($pj);&$g7($pj);&$O($uj);&$g7($uj);&$O($Yj);&$O($lk);&$O($yk);&$O($Ak);&$g7($Ak);&$O($Fk);&$g7($Fk);&$O($gl);&$O($ql);&$O($Cl);&$O($Ml);&$O($em);&$O($gm);&$g7($gm);&$O($lm);&$g7($lm);&$O($Jm);&$O($Qm);&$O($gn);&$O($in);&$g7($in);&$O($nn);&$g7($nn);&$O($Cn);&$O($On);&$O($Qn);&$g7($Qn);&$O($fo);&$O($zo);&$O($Bo);&$g7($Bo);&$Eo($Bo);&$O($Lo);&$g7($Lo);&$O($Po);&$O($Ro);&$g7($Ro);&$O($np);&$O($up);&$O($Ep);&$O($Qp);&$O($Vp);&$g7($Vp);&$Eo($Vp);&$Yp($Vp);&$O($tq);&$O($vq);&$g7($vq);&$O($Mq);&$O($Tq);&$O($Vq);&$g7($Vq);&$Eo($Vq);&$O($cr);&$g7($cr);&$O($qr);&$O($sr);&$g7($sr);&$O($yr);&$g7($yr);&$O($Jr);&$O($Pr);&$O($Rr);&$g7($Rr);&$O($Wr);&$g7($Wr);&$O($ls);&$O($rs);&$O($xs);&$O($Gs);&$O($Ss);&$O($Us);&$g7($Us);&$O($Zs);&$g7($Zs);&$O($vt);&$O($Nt);&$O($Wt);&$O($gu);&$O($iu);&$g7($iu);&$lu($iu);&$O($wu);&$O($yu);&$g7($yu);&$O($Tu);&$O($cv);&$O($sv);&$O($Mv);&$O($Ov);&$g7($Ov);&$O($Tv);&$g7($Tv);&$O($iw);&$O($vw);&$O($Ew);&$O($ex);&$O($mx);&$O($Kx);&$O($cy);&$O($sy);&$O($My);&$O($Oy);&$g7($Oy);&$O($Ty);&$g7($Ty);&$O($yz);&$O($Rz);&$O($lA);&$O($uA);&$O($EA);&$O($GA);&$g7($GA);&$O($LA);&$g7($LA);&$O($SB);&$O($iC);&$O($rC);&$O($tC);&$g7($tC);&$O($yC);&$g7($yC);&$O($QC);&$O($WC);&$O($fD);&$O($hD);&$g7($hD);&$O($nD);&$g7($nD);&$O($BD);&$O($LD);&$O($RD);&$O($TD);&$g7($TD);&$O($ZD);&$g7($ZD);&$O($pE);&$O($rE);&$g7($rE);&$O($yE);&$O($ME);&$O($OE);&$g7($OE);&$O($VE);&$g7($VE);&$O($XE);&$g7($XE);&$O($sF);&$O($zF);&$O($GF);&$O($QF);&$O($dG);&$O($fG);&$g7($fG);&$iG($fG);&$O($sG);&$O($AG);&$O($CG);&$g7($CG);&$O($TG);&$O($hH);&$O($oH);&$O($qH);&$g7($qH);&$O($vH);&$g7($vH);&$O($DH);&$g7($DH);&$O($IH);&$g7($IH);&$O($RH);&$g7($RH);&$O($WH);&$g7($WH);&$O($gI);&$g7($gI);&$O($CI);&$g7($CI);ni->run(@ARGV);
__DATA__
