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
*{'lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
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
now foo->f == 'hi';#;$x=q#eval_number#;$y=q#proto#;$z=q##;$A=q#lib/fn#;$B=bless({$t,$u,$v,$w,$x,492,$y,$z},$A);$C=q#lib/fn::ctors#;$D=q#lib/test_case#;$E=bless({$n,$o,$p,$q,$r,$q,$s,$B},$D);$F=q#TODO...#;$G=[$l,$m,$E,$F];$H=q#classes#;$I=q#ni implements a Smalltalk 80-style metaclass system with a couple of
      differences. First, ni's classes are slice unions and as such don't
      support colliding methods; and second, they support multiple inheritance.
      These two points are related: method overriding isn't in the picture,
      which makes multiple inheritance straightforward to implement.#;$J=[$H,$I,$F];$K=[$h,$k,$G,$J];$L=q#name#;$M=q#/class#;$N=q#lib/doc#;$O=bless({$e,$K,$L,$M},$N);$P=q#lib/doc::ctors#;$Q=q#ni.doc:/fabric#;$R=q#Abstractions to bridge the gaps between separate machines and processes.
      This module is designed to make it appear as though all resources are
      local, or at least can be referred to locally -- even when they belong to
      an external process (e.g. a Hadoop mapper) or another machine (e.g. a
      file over SSH). If we can bidirectionally communicate with a remote ni
      instance, then we can see its resources.#;$S=[$i,$R];$T=[$S];$U=q#/fabric#;$V=bless({$e,$T,$L,$U},$N);$W=q#ni.doc:/fabric/future#;$X=q#A value that doesn't yet exist, but is finalized once it does exist.#;$Y=[];$Z=[];$c1=q#my $f1 = ni('ni:/fabric/future')->new;
my $f2 = $f1->map(fn q{$_[0] + 1});
now [$f1->v, $f2->v] == [undef, undef];
$f1->decide(5);
now [$f1->v, $f2->v] == [5, 6];#;$d1=bless({$t,$Z,$v,$c1,$x,496,$y,$z},$A);$e1=bless({$n,$Y,$p,$q,$r,$q,$s,$d1},$D);$f1=q#You can combine multiple futures in different ways depending on what
      you're trying to do.#;$g1=[];$h1=[];$i1=q#my $f1 = ni('ni:/fabric/future')->new;
my $f2 = ni('ni:/fabric/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1, 2]];#;$j1=bless({$t,$h1,$v,$i1,$x,498,$y,$z},$A);$k1=bless({$n,$g1,$p,$q,$r,$q,$s,$j1},$D);$l1=[$i,$X,$e1,$f1,$k1];$m1=[$l1];$n1=q#/fabric/future#;$o1=bless({$e,$m1,$L,$n1},$N);$p1=q#ni.doc:/fabric/rmi#;$q1=[];$r1=q#/fabric/rmi#;$s1=bless({$e,$q1,$L,$r1},$N);$t1=q#ni.doc:/io#;$u1=q#An implementation of IO in terms of system-level FDs. We need this for a
      few reasons, three of them being that (1) old versions of Perl don't
      correctly handle interrupted system calls, (2) we want tighter control
      over which FDs are closed at what times, and (3) we want to be able to
      "unread" things -- push back against the read buffer (or use a custom
      read format in general).#;$v1=[$i,$u1];$w1=[$v1];$x1=q#/io#;$y1=bless({$e,$w1,$L,$x1},$N);$z1=q#ni.doc:/io/buffer#;$A1=q#
    my $buf = ni("ni:/io/buffer")->new(8192);
    $buf->write("foo");
    $buf->read($_, 256);        \# reads "foo"#;$B1=[$f,$A1];$C1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
      behaves like a nonblocking socket and sets errno accordingly.#;$D1=[];$E1=[];$F1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$G1=bless({$t,$E1,$v,$F1,$x,500,$y,$z},$A);$H1=bless({$n,$D1,$p,$q,$r,$q,$s,$G1},$D);$I1=[$i,$C1,$H1];$J1=[$B1,$I1];$K1=q#/io/buffer#;$L1=bless({$e,$J1,$L,$K1},$N);$M1=q#ni.doc:/io/cat#;$N1=q#
    my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into_sync($destination_io);
  #;$O1=[$f,$N1];$P1=q#Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.#;$Q1=[];$R1=[];$S1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$T1=bless({$t,$R1,$v,$S1,$x,502,$y,$z},$A);$U1=bless({$n,$Q1,$p,$q,$r,$q,$s,$T1},$D);$V1=[$i,$P1,$U1];$W1=[$O1,$V1];$X1=q#/io/cat#;$Y1=bless({$e,$W1,$L,$X1},$N);$Z1=q#ni.doc:/io/exec#;$c2=q#
    my $pid = ni("ni:/io/exec")->new("ls", "-l")
      ->connect(1 => ni("file:foo")->w)
      ->env(ENV_VAR => "value", ENV2 => "val2")
      ->fork;
    $? = $pid->await or die "ls -l failed: $?";#;$d2=[$f,$c2];$e2=q#An object that represents a fork+exec operation that hasn't yet happened.
      It allows you to incrementally specify the context of the process,
      including environment variables and file descriptor mappings. It is also
      an IO object and will set up pipes to stdin/out if you use it this way.#;$f2=[];$g2=[];$h2=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$i2=bless({$t,$g2,$v,$h2,$x,504,$y,$z},$A);$j2=bless({$n,$f2,$p,$q,$r,$q,$s,$i2},$D);$k2=[$i,$e2,$j2];$l2=[$d2,$k2];$m2=q#/io/exec#;$n2=bless({$e,$l2,$L,$m2},$N);$o2=q#ni.doc:/io/fd#;$p2=q#
    open my $fh, ...;
    my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
    my $fd = ni('ni:/io/fd')->new(0);   \# from number
    my $fd = ni('fd:0');                \# same thing
    $fd->nonblock(1)->read($_, 100);
    $fd->be(10);                        \# move FD number
  #;$q2=[$f,$p2];$r2=q#Represents a file descriptor as a child of /io/object (so the usual IO
      methods like into_async are available), and provides some convenience
      functions for things like setting up FDs for child processes. FDs are
      closed when destroyed.#;$s2=[];$t2=[];$u2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$v2=bless({$t,$t2,$v,$u2,$x,506,$y,$z},$A);$w2=bless({$n,$s2,$p,$q,$r,$q,$s,$v2},$D);$x2=[$i,$r2,$w2];$y2=[$q2,$x2];$z2=q#/io/fd#;$A2=bless({$e,$y2,$L,$z2},$N);$B2=q#ni.doc:/io/file#;$C2=q#
    my $f = ni('ni:/io/file')->new('/etc/passwd');
    my $f = ni('file:/etc/passwd');     \# same as above
    $f->into_sync(ni('fd:1'));          \# cat to stdout
  #;$D2=[$f,$C2];$E2=q#warning#;$F2=q#Files overload the -X file test operators, but this feature wasn't
      introduced until Perl 5.12 -- prior versions won't recognize this
      overload. That means that using this overload in ni's base code will
      reduce its portability and cause tests to fail.#;$G2=[$E2,$F2];$H2=q#Represents a file that may or may not exist, and stores/constructs file
      descriptors for reading/writing. /io/files are one-shot objects: once
      you've consumed them for reading or written to them, you should destroy
      the object and start over (or close the file) if you want to operate on
      the file further -- put differently, /io/file objects own the FDs they
      create.#;$I2=[];$J2=[];$K2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$L2=bless({$t,$J2,$v,$K2,$x,508,$y,$z},$A);$M2=bless({$n,$I2,$p,$q,$r,$q,$s,$L2},$D);$N2=q#File objects also provide some useful functions like atomic-updating.
      This lets you write a stream slowly into a tempfile, then rename over the
      original once the tempfile is closed. ni uses this to update itself to
      avoid race conditions.#;$O2=[];$P2=[];$Q2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$R2=bless({$t,$P2,$v,$Q2,$x,510,$y,$z},$A);$S2=bless({$n,$O2,$p,$q,$r,$q,$s,$R2},$D);$T2=[$i,$H2,$M2,$N2,$S2];$U2=[$D2,$G2,$T2];$V2=q#/io/file#;$W2=bless({$e,$U2,$L,$V2},$N);$X2=q#ni.doc:/io/file_update_fd#;$Y2=q#A write fd that performs a file rename upon closing.#;$Z2=[$i,$Y2];$c3=[$Z2];$d3=q#/io/file_update_fd#;$e3=bless({$e,$c3,$L,$d3},$N);$f3=q#ni.doc:/io/pid#;$g3=q#eg#;$h3=[];$i3=[];$j3=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$k3=bless({$t,$i3,$v,$j3,$x,512,$y,$z},$A);$l3=bless({$n,$h3,$p,$q,$r,$q,$s,$k3},$D);$m3=[$g3,$l3];$n3=[];$o3=[];$p3=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$q3=bless({$t,$o3,$v,$p3,$x,514,$y,$z},$A);$r3=bless({$n,$n3,$p,$q,$r,$q,$s,$q3},$D);$s3=[$g3,$r3];$t3=[];$u3=[];$v3=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$w3=bless({$t,$u3,$v,$v3,$x,516,$y,$z},$A);$x3=bless({$n,$t3,$p,$q,$r,$q,$s,$w3},$D);$y3=[$g3,$x3];$z3=[$m3,$s3,$y3];$A3=q#/io/pid#;$B3=bless({$e,$z3,$L,$A3},$N);$C3=q#ni.doc:/lib#;$D3=q#Bootstrapping code for the core abstractions in ni, and almost everything
      about its introspection. This includes definitions for documentation,
      unit tests, classes, support for basic image generation, etc -- and when
      possible, it's written with some awareness of downstream use cases (for
      instance, image serialization and RMI share logic).#;$E3=q#/lib is the place where things don't quite work yet, so the code here is
      written differently from other modules.#;$F3=[$i,$D3,$E3];$G3=[$F3];$H3=q#/lib#;$I3=bless({$e,$G3,$L,$H3},$N);$J3=q#ni.doc:/lib/doc#;$K3=q#
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...#;$L3=[$f,$K3];$M3=q#Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class's code without bringing along all of
      its documentation and unit tests.#;$N3=q#Documentation objects are internally represented as arrays of quoted
      method calls:#;$O3=[];$P3=[];$Q3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$R3=bless({$t,$P3,$v,$Q3,$x,518,$y,$z},$A);$S3=bless({$n,$O3,$p,$q,$r,$q,$s,$R3},$D);$T3=q#This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":#;$U3=[];$V3=[];$W3=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$X3=bless({$t,$V3,$v,$W3,$x,520,$y,$z},$A);$Y3=bless({$n,$U3,$p,$q,$r,$q,$s,$X3},$D);$Z3=[$i,$M3,$N3,$S3,$T3,$Y3];$c4=[$L3,$Z3];$d4=q#/lib/doc#;$e4=bless({$e,$c4,$L,$d4},$N);$f4=q#ni.doc:/lib/image#;$g4=q#
    my $image = ni("ni:/lib/image")->new;
    my $gensym = $image->quote($value);
    $image->io->into_sync($a_file);#;$h4=[$f,$g4];$i4=q#Generates Perl code that reconstructs the state of objects at the
      behavior/slice level. Since classes are self-describing, this results in
      a replica of the runtime object-oriented state.#;$j4=[$i,$i4];$k4=[$h4,$j4];$l4=q#/lib/image#;$m4=bless({$e,$k4,$L,$l4},$N);$n4=q#ni.doc:/lib/ni#;$o4=q#my $value = ni->resolve($name);
               my $value = ni($name);   \# alias for ni->resolve($name)
               my $self  = ni;#;$p4=[$f,$o4];$q4=q#The class for the currently-running ni instance. This includes all
      instance state, the table of named objects, and a bit of logic to update
      ni in place, for instance when adding extensions.#;$r4=[$i,$q4];$s4=[$p4,$r4];$t4=q#/lib/ni#;$u4=bless({$e,$s4,$L,$t4},$N);$v4=q#ni.doc:/lib/quote_simple#;$w4=q#A stateless object that serializes values with direct quotation; that
      is, the serialization contains no variables. If your objects have
      circular or shared references, you should probably use
      /lib/quote_circular or similar.#;$x4=[];$y4=[];$z4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$A4=bless({$t,$y4,$v,$z4,$x,522,$y,$z},$A);$B4=bless({$n,$x4,$p,$q,$r,$q,$s,$A4},$D);$C4=[$i,$w4,$B4];$D4=[$C4];$E4=q#/lib/quote_simple#;$F4=bless({$e,$D4,$L,$E4},$N);$G4=q#ni.doc:/lib/slice#;$H4=q#
    ni('ni:/lib/slice')->new('/lib/foo',
      ctor => fn q{shift->say_hi},
      say_hi => fn q{print "hi from " . shift->name . "\\n"});
    $some_class->add('/lib/foo');#;$I4=[$f,$H4];$J4=q#A slice of methods encoding some aspect of an object's behavior. Slices
      are combined using tags and branches, and the set of slices used to
      construct a class must be disjoint except for constructors and
      destructors.#;$K4=q#Slices are objects that provide an ->apply method, which installs their
      methods + ctors + dtors into a Perl package.#;$L4=[];$M4=[];$N4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$O4=bless({$t,$M4,$v,$N4,$x,524,$y,$z},$A);$P4=bless({$n,$L4,$p,$q,$r,$q,$s,$O4},$D);$Q4=q#Slices automatically do the equivalent of using Perl's "overload" module
      if any methods begin with an open-paren.#;$R4=q#Classes automatically incorporate some special low-level slices that are
      used by others; one of these is /lib/instantiable.b, which implements
      ->new and ->DESTROY. These methods then call into the lists of
      constructors and destructors implemented when slices are added to a
      package.#;$S4=[];$T4=[];$U4=q#my $instances = 0;
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
now $instances == 0;#;$V4=bless({$t,$T4,$v,$U4,$x,526,$y,$z},$A);$W4=bless({$n,$S4,$p,$q,$r,$q,$s,$V4},$D);$X4=[$i,$J4,$K4,$P4,$Q4,$R4,$W4];$Y4=[$I4,$X4];$Z4=q#/lib/slice#;$c5=bless({$e,$Y4,$L,$Z4},$N);$d5=q#ni.doc:/semantic#;$e5=q#Opportunities to assign real-world semantics to objects. This is a
      collection of behaviors that don't necessarily imply a Perl-level
      protocol, but which may end up meaning something at some point.#;$f5=[$i,$e5];$g5=[$f5];$h5=q#/semantic#;$i5=bless({$e,$g5,$L,$h5},$N);$j5=q#ni:/class#;$k5=q#applied_to#;$l5=q#class#;$m5=q#class.c#;$n5=q#fabric/future.c#;$o5=q#fabric/rmi.c#;$p5=q#io/buffer.c#;$q5=q#io/cat.c#;$r5=q#io/exec.c#;$s5=q#io/fd.c#;$t5=q#io/file.c#;$u5=q#io/file_update_fd.c#;$v5=q#io/null.c#;$w5=q#io/object.c#;$x5=q#io/pid.c#;$y5=q#io/str.c#;$z5=q#io/transfer.c#;$A5=q#io/transfer_async.c#;$B5=q#io/transfer_sync.c#;$C5=q#lib/behavior.c#;$D5=q#lib/branch.c#;$E5=q#lib/dataslice.c#;$F5=q#lib/doc.c#;$G5=q#lib/fn.c#;$H5=q#lib/image.c#;$I5=q#lib/ni.c#;$J5=q#lib/quote_simple.c#;$K5=q#lib/slice.c#;$L5=q#lib/tag.c#;$M5=q#lib/test_assert_eq.c#;$N5=q#lib/test_assertion.c#;$O5=q#lib/test_case.c#;$P5=q#lib/test_value.c#;$Q5=q#metaclass.c#;$R5=q#module.c#;$S5=q#object.c#;$T5=q#semantic/dimension#;$U5=q#semantic/dimension.c#;$V5=q#semantic/task.c#;$W5={$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1};$X5=q#slices#;$Y5=q#metaclass#;$Z5=q#module#;$c6={$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Y5,1,$Q5,1,$Z5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1};$d6=q#/module#;$e6=q#/lib/perlbranch.b#;$f6={};$g6=q#ctor#;$h6=q#dtor#;$i6=q#methods#;$j6=q#add#;$k6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$l6=bless({$v,$k6,$x,528,$y,$z},$A);$m6=q#apply#;$n6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$o6=bless({$v,$n6,$x,530,$y,$z},$A);$p6={$j6,$l6,$m6,$o6};$q6=q#/lib/branch.b#;$r6=q#lib/slice#;$s6=bless({$k5,$f6,$g6,$q,$h6,$q,$i6,$p6,$L,$q6},$r6);$t6=q#lib/branch#;$u6=q#lib/slice::ctors#;$v6={};$w6=q#my $s = shift; ni->def($s->name, $s)#;$x6=bless({$v,$w6,$x,532,$y,$z},$A);$y6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$z6=bless({$v,$y6,$x,534,$y,$z},$A);$A6={$L,$z6};$B6=q#/lib/named.b#;$C6=bless({$k5,$v6,$g6,$x6,$h6,$q,$i6,$A6,$L,$B6},$r6);$D6=q#lib/tag#;$E6={};$F6=q#namespace#;$G6=q#'ni'#;$H6=bless({$v,$G6,$x,536,$y,$z},$A);$I6={$F6,$H6};$J6=q#/lib/named_in_ni.b#;$K6=bless({$k5,$E6,$g6,$q,$h6,$q,$i6,$I6,$L,$J6},$r6);$L6={};$M6=q#package#;$N6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$O6=bless({$v,$N6,$x,538,$y,$z},$A);$P6={$M6,$O6};$Q6=q#/lib/namespaced.b#;$R6=bless({$k5,$L6,$g6,$q,$h6,$q,$i6,$P6,$L,$Q6},$r6);$S6={};$T6=q#resolve#;$U6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$V6=bless({$v,$U6,$x,540,$y,$z},$A);$W6={$T6,$V6};$X6=q#/lib/resolver.b#;$Y6=bless({$k5,$S6,$g6,$q,$h6,$q,$i6,$W6,$L,$X6},$r6);$Z6=[$s6,$C6,$K6,$R6,$Y6];$c7=bless({$L,$e6,$X5,$Z6},$D6);$d7=q#lib/tag::ctors#;$e7={};$f7=q#my $s = shift; $s->apply($s->package)#;$g7=bless({$v,$f7,$x,542,$y,$z},$A);$h7=q#instantiate#;$i7=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$j7=bless({$v,$i7,$x,544,$y,$z},$A);$k7={$h7,$j7};$l7=q#/lib/class_init.b#;$m7=bless({$k5,$e7,$g6,$g7,$h6,$q,$i6,$k7,$L,$l7},$r6);$n7=q#fabric/future#;$o7=q#fabric/rmi#;$p7=q#io/buffer#;$q7=q#io/cat#;$r7=q#io/exec#;$s7=q#io/fd#;$t7=q#io/file#;$u7=q#io/file_update_fd#;$v7=q#io/null#;$w7=q#io/object#;$x7=q#io/pid#;$y7=q#io/str#;$z7=q#io/transfer#;$A7=q#io/transfer_async#;$B7=q#io/transfer_sync#;$C7=q#lib/behavior#;$D7=q#lib/dataslice#;$E7=q#lib/image#;$F7=q#lib/ni#;$G7=q#lib/quote_simple#;$H7=q#lib/test_assert_eq#;$I7=q#lib/test_assertion#;$J7=q#lib/test_value#;$K7=q#object#;$L7=q#semantic/task#;$M7={$l5,1,$m5,1,$n7,1,$n5,1,$o7,1,$o5,1,$p7,1,$p5,1,$q7,1,$q5,1,$r7,1,$r5,1,$s7,1,$s5,1,$t7,1,$t5,1,$u7,1,$u5,1,$v7,1,$v5,1,$w7,1,$w5,1,$x7,1,$x5,1,$y7,1,$y5,1,$z7,1,$z5,1,$A7,1,$A5,1,$B7,1,$B5,1,$C7,1,$C5,1,$t6,1,$D5,1,$D7,1,$E5,1,$N,1,$F5,1,$A,1,$G5,1,$E7,1,$H5,1,$F7,1,$I5,1,$G7,1,$J5,1,$r6,1,$K5,1,$D6,1,$L5,1,$H7,1,$M5,1,$I7,1,$N5,1,$D,1,$O5,1,$J7,1,$P5,1,$Y5,1,$Q5,1,$Z5,1,$R5,1,$K7,1,$S5,1,$T5,1,$U5,1,$L7,1,$V5,1};$N7=q#/object#;$O7={};$P7=q#DESTROY#;$Q7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$R7=bless({$v,$Q7,$x,546,$y,$z},$A);$S7=q#ni 'ni:/' . ref shift#;$T7=bless({$v,$S7,$x,548,$y,$z},$A);$U7={$P7,$R7,$l5,$T7};$V7=q#/lib/instance.b#;$W7=bless({$k5,$O7,$g6,$q,$h6,$q,$i6,$U7,$L,$V7},$r6);$X7=[$W7];$Y7=bless({$k5,$M7,$L,$N7,$X5,$X7},$S5);$Z7=q#object.c::ctors#;$c8={$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C7,1,$C5,1,$t6,1,$D5,1,$D7,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$r6,1,$K5,1,$D6,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Y5,1,$Q5,1,$Z5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1};$d8=q#/lib/behavior#;$e8={};$f8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$g8=bless({$v,$f8,$x,550,$y,$z},$A);$h8={$e,$g8};$i8=q#/lib/documentable.b#;$j8=bless({$k5,$e8,$g6,$q,$h6,$q,$i6,$h8,$L,$i8},$r6);$k8=[$Y7,$j8];$l8=bless({$k5,$c8,$L,$d8,$X5,$k8},$C5);$m8=q#lib/behavior.c::ctors#;$n8={$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$t6,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Y5,1,$Q5,1,$Z5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1};$o8=q#/lib/definition.b#;$p8={};$q8=q#def#;$r8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$s8=bless({$v,$r8,$x,552,$y,$z},$A);$t8={$q8,$s8};$u8=q#/lib/definition_def.b#;$v8=bless({$k5,$p8,$g6,$q,$h6,$q,$i6,$t8,$L,$u8},$r6);$w8={};$x8=q#ro#;$y8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$z8=bless({$v,$y8,$x,554,$y,$z},$A);$A8=q#rw#;$B8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$C8=bless({$v,$B8,$x,556,$y,$z},$A);$D8={$x8,$z8,$A8,$C8};$E8=q#/lib/accessor.b#;$F8=bless({$k5,$w8,$g6,$q,$h6,$q,$i6,$D8,$L,$E8},$r6);$G8={};$H8=q#(""#;$I8=q#shift->name#;$J8=bless({$v,$I8,$x,558,$y,$z},$A);$K8={$H8,$J8};$L8=q#/lib/name_as_string.b#;$M8=bless({$k5,$G8,$g6,$q,$h6,$q,$i6,$K8,$L,$L8},$r6);$N8={};$O8=q#(eq#;$P8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$Q8=bless({$v,$P8,$x,560,$y,$z},$A);$R8={$O8,$Q8};$S8=q#/lib/ref_eq.b#;$T8=bless({$k5,$N8,$g6,$q,$h6,$q,$i6,$R8,$L,$S8},$r6);$U8={};$V8=q#defdata#;$W8=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$X8=bless({$v,$W8,$x,562,$y,$z},$A);$Y8={$V8,$X8};$Z8=q#/lib/definition_defdata.b#;$c9=bless({$k5,$U8,$g6,$q,$h6,$q,$i6,$Y8,$L,$Z8},$r6);$d9=[$v8,$F8,$M8,$T8,$c9];$e9=bless({$k5,$n8,$L,$o8,$X5,$d9},$t6);$f9=q#lib/branch::ctors#;$g9=[$c7,$m7,$Y7,$l8,$e9];$h9=bless({$k5,$c6,$L,$d6,$X5,$g9},$R5);$i9=q#module.c::ctors#;$j9={};$k9=q#new#;$l9=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$m9=bless({$v,$l9,$x,564,$y,$z},$A);$n9={$k9,$m9};$o9=q#/lib/instantiable.b#;$p9=bless({$k5,$j9,$i6,$n9,$L,$o9},$r6);$q9={};$r9=q#child#;$s9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$t9=bless({$v,$s9,$x,566,$y,$z},$A);$u9={$r9,$t9};$v9=q#/lib/subclass.b#;$w9=bless({$k5,$q9,$g6,$q,$h6,$q,$i6,$u9,$L,$v9},$r6);$x9=[$h9,$p9,$m7,$h9,$w9];$y9=bless({$k5,$W5,$L,$M,$X5,$x9},$m5);$z9=q#class.c::ctors#;$A9=q#ni:/class.c#;$B9={$m5,1,$U5,1};$C9=q#/class.c#;$D9={$m5,1,$R5,1,$U5,1};$E9=q#/module.c#;$F9={$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$R5,1,$S5,1,$U5,1,$V5,1};$G9=q#/object.c#;$H9=[$y9];$I9=bless({$k5,$F9,$L,$G9,$X5,$H9},$Y5);$J9=q#metaclass::ctors#;$K9={$m5,1,$C5,1,$D5,1,$E5,1,$K5,1,$L5,1,$R5,1,$U5,1};$L9=q#/lib/behavior.c#;$M9=[$I9];$N9=bless({$k5,$K9,$L,$L9,$X5,$M9},$Y5);$O9=[$I9,$p9,$N9];$P9=bless({$k5,$D9,$L,$E9,$X5,$O9},$Y5);$Q9=[$P9];$R9=bless({$k5,$B9,$L,$C9,$X5,$Q9},$Y5);$S9=q#ni:/fabric/future#;$T9={$n7,1};$U9={};$V9=[];$W9=q#shift->{'outcome'}#;$X9=bless({$t,$V9,$v,$W9,$x,568,$y,$z},$A);$Y9=q#parents#;$Z9=[];$ca=q#shift->{'parents'}#;$da=bless({$t,$Z9,$v,$ca,$x,570,$y,$z},$A);$ea=q#v#;$fa=[];$ga=q#shift->{'v'}#;$ha=bless({$t,$fa,$v,$ga,$x,572,$y,$z},$A);$ia={$r,$X9,$Y9,$da,$ea,$ha};$ja=q#/fabric/future_ro.b#;$ka=bless({$k5,$U9,$g6,$q,$h6,$q,$i6,$ia,$L,$ja},$r6);$la={};$ma=[];$na=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$oa=bless({$t,$ma,$v,$na,$x,574,$y,$z},$A);$pa={$h7,$oa};$qa=q#/fabric/future_init.b#;$ra=bless({$k5,$la,$g6,$q,$h6,$q,$i6,$pa,$L,$qa},$r6);$sa={};$ta=q#decide#;$ua=[];$va=q#local $_;
my ($self, $v) = @_;
die "ni:/fabric/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$wa=bless({$t,$ua,$v,$va,$x,576,$y,$z},$A);$xa=q#decided#;$ya=[];$za=q#shift->{outcome}#;$Aa=bless({$t,$ya,$v,$za,$x,578,$y,$z},$A);$Ba=q#listener#;$Ca=[];$Da=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$Ea=bless({$t,$Ca,$v,$Da,$x,580,$y,$z},$A);$Fa={$ta,$wa,$xa,$Aa,$Ba,$Ea};$Ga=q#/fabric/future_state.b#;$Ha=bless({$k5,$sa,$g6,$q,$h6,$q,$i6,$Fa,$L,$Ga},$r6);$Ia={};$Ja=q#and#;$Ka=[];$La=q#my $self   = $_[0];
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
$child;#;$Ma=bless({$t,$Ka,$v,$La,$x,582,$y,$z},$A);$Na=q#flatmap#;$Oa=[];$Pa=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$Qa=bless({$t,$Oa,$v,$Pa,$x,584,$y,$z},$A);$Ra=q#map#;$Sa=[];$Ta=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$Ua=bless({$t,$Sa,$v,$Ta,$x,586,$y,$z},$A);$Va=q#or#;$Wa=[];$Xa=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Ya=bless({$t,$Wa,$v,$Xa,$x,588,$y,$z},$A);$Za={$Ja,$Ma,$Na,$Qa,$Ra,$Ua,$Va,$Ya};$cb=q#/fabric/future_algebra.b#;$db=bless({$k5,$Ia,$g6,$q,$h6,$q,$i6,$Za,$L,$cb},$r6);$eb=[$Y7,$ka,$ra,$Ha,$db];$fb=bless({$k5,$T9,$L,$n1,$X5,$eb},$n5);$gb=q#fabric/future.c::ctors#;$hb=q#ni:/fabric/future.c#;$ib={$n5,1};$jb=q#/fabric/future.c#;$kb=[$I9];$lb=bless({$k5,$ib,$L,$jb,$X5,$kb},$Y5);$mb=q#ni:/fabric/future_algebra.b#;$nb=q#ni:/fabric/future_init.b#;$ob=q#ni:/fabric/future_ro.b#;$pb=q#ni:/fabric/future_state.b#;$qb=q#ni:/fabric/rmi#;$rb={$o7,1};$sb={};$tb=[];$ub=q#my ($class, $io, $quote) = @_;#;$vb=bless({$t,$tb,$v,$ub,$x,590,$y,$z},$A);$wb={$h7,$vb};$xb=q#/fabric/rmi_init.b#;$yb=bless({$k5,$sb,$g6,$q,$h6,$q,$i6,$wb,$L,$xb},$r6);$zb=[$Y7,$yb];$Ab=bless({$k5,$rb,$L,$r1,$X5,$zb},$o5);$Bb=q#fabric/rmi.c::ctors#;$Cb=q#ni:/fabric/rmi.c#;$Db={$o5,1};$Eb=q#/fabric/rmi.c#;$Fb=[$I9];$Gb=bless({$k5,$Db,$L,$Eb,$X5,$Fb},$Y5);$Hb=q#ni:/fabric/rmi_init.b#;$Ib=q#ni:/io/buffer#;$Jb={$p7,1};$Kb={$p7,1,$q7,1,$r7,1,$s7,1,$t7,1,$u7,1,$v7,1,$w7,1,$x7,1,$y7,1};$Lb=q#/io/object#;$Mb={};$Nb=q#(bool#;$Ob=[];$Pb=bless({$t,$Ob,$v,1,$x,592,$y,$z},$A);$Qb={$Nb,$Pb};$Rb=q#/io/object_ops.b#;$Sb=bless({$k5,$Mb,$g6,$q,$h6,$q,$i6,$Qb,$L,$Rb},$r6);$Tb={};$Ub=q#die#;$Vb=[];$Wb=q#shift; die join " ", @_#;$Xb=bless({$t,$Vb,$v,$Wb,$x,594,$y,$z},$A);$Yb=q#io_check#;$Zb=[];$cc=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$dc=bless({$t,$Zb,$v,$cc,$x,596,$y,$z},$A);$ec=q#io_check_defined#;$fc=[];$gc=q#shift->io_check(sub {defined shift}, @_)#;$hc=bless({$t,$fc,$v,$gc,$x,598,$y,$z},$A);$ic=q#io_check_true#;$jc=[];$kc=q#shift->io_check(sub {shift}, @_)#;$lc=bless({$t,$jc,$v,$kc,$x,600,$y,$z},$A);$mc={$Ub,$Xb,$Yb,$dc,$ec,$hc,$ic,$lc};$nc=q#/io/object_checks.b#;$oc=bless({$k5,$Tb,$g6,$q,$h6,$q,$i6,$mc,$L,$nc},$r6);$pc={};$qc=q#(+#;$rc=[];$sc=q#ni('ni:/io/cat')->new(@_[0, 1])#;$tc=bless({$t,$rc,$v,$sc,$x,602,$y,$z},$A);$uc={$qc,$tc};$vc=q#/io/object_constructors.b#;$wc=bless({$k5,$pc,$g6,$q,$h6,$q,$i6,$uc,$L,$vc},$r6);$xc={};$yc=q#read_all#;$zc=[];$Ac=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$Bc=bless({$t,$zc,$v,$Ac,$x,604,$y,$z},$A);$Cc=q#write_all#;$Dc=[];$Ec=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$Fc=bless({$t,$Dc,$v,$Ec,$x,606,$y,$z},$A);$Gc={$yc,$Bc,$Cc,$Fc};$Hc=q#/io/object_memory.b#;$Ic=bless({$k5,$xc,$g6,$q,$h6,$q,$i6,$Gc,$L,$Hc},$r6);$Jc={};$Kc=q#connect_sync#;$Lc=[];$Mc=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$Nc=bless({$t,$Lc,$v,$Mc,$x,608,$y,$z},$A);$Oc=q#into_sync#;$Pc=[];$Qc=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$Rc=bless({$t,$Pc,$v,$Qc,$x,610,$y,$z},$A);$Sc={$Kc,$Nc,$Oc,$Rc};$Tc=q#/io/object_transfer_sync.b#;$Uc=bless({$k5,$Jc,$g6,$q,$h6,$q,$i6,$Sc,$L,$Tc},$r6);$Vc={};$Wc=q#connect_async#;$Xc=[];$Yc=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$Zc=bless({$t,$Xc,$v,$Yc,$x,612,$y,$z},$A);$cd=q#into_async#;$dd=[];$ed=q#ni('ni:/io/transfer_async')->new(@_)->run#;$fd=bless({$t,$dd,$v,$ed,$x,614,$y,$z},$A);$gd={$Wc,$Zc,$cd,$fd};$hd=q#/io/object_transfer_async.b#;$id=bless({$k5,$Vc,$g6,$q,$h6,$q,$i6,$gd,$L,$hd},$r6);$jd=[$Y7,$Sb,$oc,$wc,$Ic,$Uc,$id,$id,$Uc,$id,$Uc];$kd=bless({$k5,$Kb,$L,$Lb,$X5,$jd},$w5);$ld=q#io/object.c::ctors#;$md={};$nd=[];$od=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$pd=bless({$t,$nd,$v,$od,$x,616,$y,$z},$A);$qd={$h7,$pd};$rd=q#/io/buffer_init.b#;$sd=bless({$k5,$md,$g6,$q,$h6,$q,$i6,$qd,$L,$rd},$r6);$td={};$ud=q#read#;$vd=[];$wd=q#my $self = shift;
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
}#;$xd=bless({$t,$vd,$v,$wd,$x,618,$y,$z},$A);$yd=q#read_capacity#;$zd=[];$Ad=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$Bd=bless({$t,$zd,$v,$Ad,$x,620,$y,$z},$A);$Cd=q#write#;$Dd=[];$Ed=q#my $self = shift;
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
}#;$Fd=bless({$t,$Dd,$v,$Ed,$x,622,$y,$z},$A);$Gd=q#write_capacity#;$Hd=[];$Id=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Jd=bless({$t,$Hd,$v,$Id,$x,624,$y,$z},$A);$Kd={$ud,$xd,$yd,$Bd,$Cd,$Fd,$Gd,$Jd};$Ld=q#/io/buffer_io.b#;$Md=bless({$k5,$td,$g6,$q,$h6,$q,$i6,$Kd,$L,$Ld},$r6);$Nd=[$kd,$sd,$Md];$Od=bless({$k5,$Jb,$L,$K1,$X5,$Nd},$p5);$Pd=q#io/buffer.c::ctors#;$Qd=q#ni:/io/buffer.c#;$Rd={$p5,1};$Sd=q#/io/buffer.c#;$Td={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1};$Ud=q#/io/object.c#;$Vd={};$Wd=q#def_transfer_method#;$Xd=[];$Yd=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Zd=bless({$t,$Xd,$v,$Yd,$x,626,$y,$z},$A);$ce={$Wd,$Zd};$de=q#/io/object.c_transfer_def.b#;$ee=bless({$k5,$Vd,$g6,$q,$h6,$q,$i6,$ce,$L,$de},$r6);$fe=[$I9,$ee];$ge=bless({$k5,$Td,$L,$Ud,$X5,$fe},$Y5);$he=[$ge];$ie=bless({$k5,$Rd,$L,$Sd,$X5,$he},$Y5);$je=q#ni:/io/buffer_init.b#;$ke=q#ni:/io/buffer_io.b#;$le=q#ni:/io/cat#;$me={$q7,1};$ne={};$oe=[];$pe=q#shift; +{fs => [@_]}#;$qe=bless({$t,$oe,$v,$pe,$x,628,$y,$z},$A);$re={$h7,$qe};$se=q#/io/cat_init.b#;$te=bless({$k5,$ne,$g6,$q,$h6,$q,$i6,$re,$L,$se},$r6);$ue={};$ve=[];$we=q#my $fs = shift->{fs};
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
$total_read;#;$xe=bless({$t,$ve,$v,$we,$x,630,$y,$z},$A);$ye={$ud,$xe};$ze=q#/io/cat_read.b#;$Ae=bless({$k5,$ue,$g6,$q,$h6,$q,$i6,$ye,$L,$ze},$r6);$Be=[$kd,$te,$Ae];$Ce=bless({$k5,$me,$L,$X1,$X5,$Be},$q5);$De=q#io/cat.c::ctors#;$Ee=q#ni:/io/cat.c#;$Fe={$q5,1};$Ge=q#/io/cat.c#;$He=[$ge];$Ie=bless({$k5,$Fe,$L,$Ge,$X5,$He},$Y5);$Je=q#ni:/io/cat_init.b#;$Ke=q#ni:/io/cat_read.b#;$Le=q#ni:/io/exec#;$Me={$r7,1};$Ne={};$Oe=q#argv#;$Pe=[];$Qe=q#shift->{'argv'}#;$Re=bless({$t,$Pe,$v,$Qe,$x,632,$y,$z},$A);$Se={$Oe,$Re};$Te=q#/io/exec_ro.b#;$Ue=bless({$k5,$Ne,$g6,$q,$h6,$q,$i6,$Se,$L,$Te},$r6);$Ve={};$We=[];$Xe=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Ye=bless({$t,$We,$v,$Xe,$x,634,$y,$z},$A);$Ze={$h7,$Ye};$cf=q#/io/exec_init.b#;$df=bless({$k5,$Ve,$g6,$q,$h6,$q,$i6,$Ze,$L,$cf},$r6);$ef={};$ff=q#connect#;$gf=[];$hf=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$if=bless({$t,$gf,$v,$hf,$x,636,$y,$z},$A);$jf=q#in_pipe#;$kf=[];$lf=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$mf=bless({$t,$kf,$v,$lf,$x,638,$y,$z},$A);$nf=q#out_pipe#;$of=[];$pf=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$qf=bless({$t,$of,$v,$pf,$x,640,$y,$z},$A);$rf=q#setup_stdio#;$sf=[];$tf=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$uf=bless({$t,$sf,$v,$tf,$x,642,$y,$z},$A);$vf={$ff,$if,$jf,$mf,$nf,$qf,$rf,$uf};$wf=q#/io/exec_io_setup.b#;$xf=bless({$k5,$ef,$g6,$q,$h6,$q,$i6,$vf,$L,$wf},$r6);$yf={};$zf=q#binds_fd#;$Af=[];$Bf=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$Cf=bless({$t,$Af,$v,$Bf,$x,644,$y,$z},$A);$Df=q#fd#;$Ef=[];$Ff=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$Gf=bless({$t,$Ef,$v,$Ff,$x,646,$y,$z},$A);$Hf=q#stderr#;$If=[];$Jf=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Kf=bless({$t,$If,$v,$Jf,$x,648,$y,$z},$A);$Lf=q#stdin#;$Mf=[];$Nf=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Of=bless({$t,$Mf,$v,$Nf,$x,650,$y,$z},$A);$Pf=q#stdout#;$Qf=[];$Rf=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Sf=bless({$t,$Qf,$v,$Rf,$x,652,$y,$z},$A);$Tf={$zf,$Cf,$Df,$Gf,$Hf,$Kf,$Lf,$Of,$Pf,$Sf};$Uf=q#/io/exec_io_accessors.b#;$Vf=bless({$k5,$yf,$g6,$q,$h6,$q,$i6,$Tf,$L,$Uf},$r6);$Wf={};$Xf=q#env#;$Yf=[];$Zf=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$cg=bless({$t,$Yf,$v,$Zf,$x,654,$y,$z},$A);$dg={$Xf,$cg};$eg=q#/io/exec_env.b#;$fg=bless({$k5,$Wf,$g6,$q,$h6,$q,$i6,$dg,$L,$eg},$r6);$gg={};$hg=q#exec#;$ig=[];$jg=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$kg=bless({$t,$ig,$v,$jg,$x,656,$y,$z},$A);$lg=q#fork#;$mg=[];$ng=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$og=bless({$t,$mg,$v,$ng,$x,658,$y,$z},$A);$pg=q#move_fds#;$qg=[];$rg=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$sg=bless({$t,$qg,$v,$rg,$x,660,$y,$z},$A);$tg={$hg,$kg,$lg,$og,$pg,$sg};$ug=q#/io/exec_fork.b#;$vg=bless({$k5,$gg,$g6,$q,$h6,$q,$i6,$tg,$L,$ug},$r6);$wg=[$kd,$Ue,$df,$xf,$Vf,$fg,$vg];$xg=bless({$k5,$Me,$L,$m2,$X5,$wg},$r5);$yg=q#io/exec.c::ctors#;$zg=q#ni:/io/exec.c#;$Ag={$r5,1};$Bg=q#/io/exec.c#;$Cg=[$ge];$Dg=bless({$k5,$Ag,$L,$Bg,$X5,$Cg},$Y5);$Eg=q#ni:/io/exec_env.b#;$Fg=q#ni:/io/exec_fork.b#;$Gg=q#ni:/io/exec_init.b#;$Hg=q#ni:/io/exec_io_accessors.b#;$Ig=q#ni:/io/exec_io_setup.b#;$Jg=q#ni:/io/exec_ro.b#;$Kg=q#ni:/io/fd#;$Lg={$s7,1};$Mg={};$Ng=[];$Og=q#shift->{'fd'}#;$Pg=bless({$t,$Ng,$v,$Og,$x,662,$y,$z},$A);$Qg={$Df,$Pg};$Rg=q#/io/fd_readers.b#;$Sg=bless({$k5,$Mg,$g6,$q,$h6,$q,$i6,$Qg,$L,$Rg},$r6);$Tg={};$Ug=[];$Vg=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Wg=bless({$t,$Ug,$v,$Vg,$x,664,$y,$z},$A);$Xg={$h7,$Wg};$Yg=q#/io/fd_init.b#;$Zg=bless({$k5,$Tg,$g6,$q,$h6,$q,$i6,$Xg,$L,$Yg},$r6);$ch={};$dh=q#be#;$eh=[];$fh=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$$self{rfh} = $$self{wfh} = undef;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$gh=bless({$t,$eh,$v,$fh,$x,666,$y,$z},$A);$hh={$dh,$gh};$ih=q#/io/fd_shell.b#;$jh=bless({$k5,$ch,$g6,$q,$h6,$q,$i6,$hh,$L,$ih},$r6);$kh={};$lh=q#cloexec#;$mh=[];$nh=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$oh=bless({$t,$mh,$v,$nh,$x,668,$y,$z},$A);$ph=q#fcntl_flag#;$qh=[];$rh=q#my ($self, $flag, $value) = @_;
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
}#;$sh=bless({$t,$qh,$v,$rh,$x,670,$y,$z},$A);$th=q#nonblock#;$uh=[];$vh=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$wh=bless({$t,$uh,$v,$vh,$x,672,$y,$z},$A);$xh={$lh,$oh,$ph,$sh,$th,$wh};$yh=q#/io/fd_fcntl.b#;$zh=bless({$k5,$kh,$g6,$q,$h6,$q,$i6,$xh,$L,$yh},$r6);$Ah={};$Bh=[];$Ch=q#shift->close#;$Dh=bless({$t,$Bh,$v,$Ch,$x,674,$y,$z},$A);$Eh=q#close#;$Fh=[];$Gh=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
POSIX::close $$self{fd} if defined $$self{fd};
$$self{fd} = $$self{rfh} = $$self{wfh} = undef;
$self;#;$Hh=bless({$t,$Fh,$v,$Gh,$x,676,$y,$z},$A);$Ih={$Eh,$Hh};$Jh=q#/io/fd_gc.b#;$Kh=bless({$k5,$Ah,$g6,$q,$h6,$Dh,$i6,$Ih,$L,$Jh},$r6);$Lh={};$Mh=[];$Nh=q#my $self = shift;
open $$self{rfh}, "<&=$$self{fd}" or return undef unless $$self{rfh};
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Oh=bless({$t,$Mh,$v,$Nh,$x,678,$y,$z},$A);$Ph=[];$Qh=q#my $self = shift;
open $$self{wfh}, ">&=$$self{fd}" or return undef unless $$self{wfh};
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Rh=bless({$t,$Ph,$v,$Qh,$x,680,$y,$z},$A);$Sh={$ud,$Oh,$Cd,$Rh};$Th=q#/io/fd_perlio.b#;$Uh=bless({$k5,$Lh,$g6,$q,$h6,$q,$i6,$Sh,$L,$Th},$r6);$Vh=[$kd,$Sg,$Zg,$jh,$zh,$Kh,$Uh];$Wh=bless({$k5,$Lg,$L,$z2,$X5,$Vh},$s5);$Xh=q#io/fd.c::ctors#;$Yh=q#ni:/io/fd.c#;$Zh={$s5,1};$ci=q#/io/fd.c#;$di=[$ge];$ei=bless({$k5,$Zh,$L,$ci,$X5,$di},$Y5);$fi=q#ni:/io/fd_fcntl.b#;$gi=q#ni:/io/fd_gc.b#;$hi=q#ni:/io/fd_init.b#;$ii=q#ni:/io/fd_perlio.b#;$ji=q#ni:/io/fd_readers.b#;$ki=q#ni:/io/fd_shell.b#;$li=q#ni:/io/file#;$mi={$t7,1};$ni={};$oi=[];$pi=q#shift->{'name'}#;$qi=bless({$t,$oi,$v,$pi,$x,682,$y,$z},$A);$ri={$L,$qi};$si=q#/io/file_readers.b#;$ti=bless({$k5,$ni,$g6,$q,$h6,$q,$i6,$ri,$L,$si},$r6);$ui={};$vi=q#mode#;$wi=[];$xi=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$yi=bless({$t,$wi,$v,$xi,$x,684,$y,$z},$A);$zi={$vi,$yi};$Ai=q#/io/file_accessors.b#;$Bi=bless({$k5,$ui,$g6,$q,$h6,$q,$i6,$zi,$L,$Ai},$r6);$Ci={};$Di=[];$Ei=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$Fi=bless({$t,$Di,$v,$Ei,$x,686,$y,$z},$A);$Gi={$h7,$Fi};$Hi=q#/io/file_init.b#;$Ii=bless({$k5,$Ci,$g6,$q,$h6,$q,$i6,$Gi,$L,$Hi},$r6);$Ji={};$Ki=q#(-X#;$Li=[];$Mi=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Ni=bless({$t,$Li,$v,$Mi,$x,688,$y,$z},$A);$Oi=q#mv#;$Pi=[];$Qi=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Ri=bless({$t,$Pi,$v,$Qi,$x,690,$y,$z},$A);$Si=q#rm#;$Ti=[];$Ui=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Vi=bless({$t,$Ti,$v,$Ui,$x,692,$y,$z},$A);$Wi={$Ki,$Ni,$Oi,$Ri,$Si,$Vi};$Xi=q#/io/file_fns.b#;$Yi=bless({$k5,$Ji,$g6,$q,$h6,$q,$i6,$Wi,$L,$Xi},$r6);$Zi={};$cj=q#atomic_update#;$dj=[];$ej=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$fj=bless({$t,$dj,$v,$ej,$x,694,$y,$z},$A);$gj={$cj,$fj};$hj=q#/io/file_update.b#;$ij=bless({$k5,$Zi,$g6,$q,$h6,$q,$i6,$gj,$L,$hj},$r6);$jj={};$kj=[];$lj=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$mj=bless({$t,$kj,$v,$lj,$x,696,$y,$z},$A);$nj=q#r#;$oj=[];$pj=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$qj=bless({$t,$oj,$v,$pj,$x,698,$y,$z},$A);$rj=[];$sj=q#shift->r->read(@_)#;$tj=bless({$t,$rj,$v,$sj,$x,700,$y,$z},$A);$uj=q#w#;$vj=[];$wj=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$xj=bless({$t,$vj,$v,$wj,$x,702,$y,$z},$A);$yj=[];$zj=q#shift->w->write(@_)#;$Aj=bless({$t,$yj,$v,$zj,$x,704,$y,$z},$A);$Bj={$Eh,$mj,$nj,$qj,$ud,$tj,$uj,$xj,$Cd,$Aj};$Cj=q#/io/file_io.b#;$Dj=bless({$k5,$jj,$g6,$q,$h6,$q,$i6,$Bj,$L,$Cj},$r6);$Ej=[$kd,$ti,$Bi,$Ii,$Yi,$ij,$Dj];$Fj=bless({$k5,$mi,$L,$V2,$X5,$Ej},$t5);$Gj=q#io/file.c::ctors#;$Hj=q#ni:/io/file.c#;$Ij={$t5,1};$Jj=q#/io/file.c#;$Kj=[$ge];$Lj=bless({$k5,$Ij,$L,$Jj,$X5,$Kj},$Y5);$Mj=q#ni:/io/file_accessors.b#;$Nj=q#ni:/io/file_fns.b#;$Oj=q#ni:/io/file_init.b#;$Pj=q#ni:/io/file_io.b#;$Qj=q#ni:/io/file_readers.b#;$Rj=q#ni:/io/file_update.b#;$Sj=q#ni:/io/file_update_fd#;$Tj={$u7,1};$Uj={};$Vj=[];$Wj=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Xj=bless({$t,$Vj,$v,$Wj,$x,706,$y,$z},$A);$Yj={$h7,$Xj};$Zj=q#/io/file_update_fd_init.b#;$ck=bless({$k5,$Uj,$g6,$q,$h6,$q,$i6,$Yj,$L,$Zj},$r6);$dk={};$ek=[];$fk=bless({$t,$ek,$v,$Ch,$x,708,$y,$z},$A);$gk=[];$hk=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$ik=bless({$t,$gk,$v,$hk,$x,710,$y,$z},$A);$jk={$Eh,$ik};$kk=q#/io/file_update_fd_gc.b#;$lk=bless({$k5,$dk,$g6,$q,$h6,$fk,$i6,$jk,$L,$kk},$r6);$mk=[$kd,$Sg,$zh,$Uh,$ck,$lk];$nk=bless({$k5,$Tj,$L,$d3,$X5,$mk},$u5);$ok=q#io/file_update_fd.c::ctors#;$pk=q#ni:/io/file_update_fd.c#;$qk={$u5,1};$rk=q#/io/file_update_fd.c#;$sk=[$ge];$tk=bless({$k5,$qk,$L,$rk,$X5,$sk},$Y5);$uk=q#ni:/io/file_update_fd_gc.b#;$vk=q#ni:/io/file_update_fd_init.b#;$wk=q#ni:/io/named_io_fns.b#;$xk={};$yk=q#fcntl#;$zk=[];$Ak=q#CORE::fcntl $_[0], $_[1], $_[2]#;$Bk=bless({$t,$zk,$v,$Ak,$x,712,$y,$z},$A);$Ck=[];$Dk=q#CORE::fork#;$Ek=bless({$t,$Ck,$v,$Dk,$x,714,$y,$z},$A);$Fk=q#open2#;$Gk=[];$Hk=q#CORE::open $_[0], $_[1]#;$Ik=bless({$t,$Gk,$v,$Hk,$x,716,$y,$z},$A);$Jk=q#rename#;$Kk=[];$Lk=q#CORE::rename $_[0], $_[1]#;$Mk=bless({$t,$Kk,$v,$Lk,$x,718,$y,$z},$A);$Nk=q#unlink#;$Ok=[];$Pk=q#CORE::unlink @_#;$Qk=bless({$t,$Ok,$v,$Pk,$x,720,$y,$z},$A);$Rk=q#waitpid#;$Sk=[];$Tk=q#CORE::waitpid $_[0], $_[1]#;$Uk=bless({$t,$Sk,$v,$Tk,$x,722,$y,$z},$A);$Vk={$yk,$Bk,$lg,$Ek,$Fk,$Ik,$Jk,$Mk,$Nk,$Qk,$Rk,$Uk};$Wk=q#/io/named_io_fns.b#;$Xk=bless({$k5,$xk,$g6,$q,$h6,$q,$i6,$Vk,$L,$Wk},$r6);$Yk=q#main#;$Zk=q#ni:/io/null#;$cl={$v7,1};$dl=q#/io/null#;$el={};$fl=[];$gl=q#+{fd => undef}#;$hl=bless({$t,$fl,$v,$gl,$x,724,$y,$z},$A);$il={$h7,$hl};$jl=q#/io/null_init.b#;$kl=bless({$k5,$el,$g6,$q,$h6,$q,$i6,$il,$L,$jl},$r6);$ll={};$ml=[];$nl=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$ol=bless({$t,$ml,$v,$nl,$x,726,$y,$z},$A);$pl=[];$ql=q#shift->fd->read(@_)#;$rl=bless({$t,$pl,$v,$ql,$x,728,$y,$z},$A);$sl=[];$tl=q#shift->fd->write(@_)#;$ul=bless({$t,$sl,$v,$tl,$x,730,$y,$z},$A);$vl={$Df,$ol,$ud,$rl,$Cd,$ul};$wl=q#/io/null_io.b#;$xl=bless({$k5,$ll,$g6,$q,$h6,$q,$i6,$vl,$L,$wl},$r6);$yl=[$kd,$kl,$xl];$zl=bless({$k5,$cl,$L,$dl,$X5,$yl},$v5);$Al=q#io/null.c::ctors#;$Bl=q#ni:/io/null.c#;$Cl={$v5,1};$Dl=q#/io/null.c#;$El=[$ge];$Fl=bless({$k5,$Cl,$L,$Dl,$X5,$El},$Y5);$Gl=q#ni:/io/null_init.b#;$Hl=q#ni:/io/null_io.b#;$Il=q#ni:/io/object#;$Jl=q#ni:/io/object.c#;$Kl=q#ni:/io/object.c_transfer_def.b#;$Ll=q#ni:/io/object_checks.b#;$Ml=q#ni:/io/object_constructors.b#;$Nl=q#ni:/io/object_memory.b#;$Ol=q#ni:/io/object_ops.b#;$Pl=q#ni:/io/object_transfer_async.b#;$Ql=q#ni:/io/object_transfer_sync.b#;$Rl=q#ni:/io/pid#;$Sl={$x7,1};$Tl={};$Ul=q#pid#;$Vl=[];$Wl=q#shift->{'pid'}#;$Xl=bless({$t,$Vl,$v,$Wl,$x,732,$y,$z},$A);$Yl=q#status#;$Zl=[];$cm=q#shift->{'status'}#;$dm=bless({$t,$Zl,$v,$cm,$x,734,$y,$z},$A);$em={$Ul,$Xl,$Yl,$dm};$fm=q#/io/pid_readers.b#;$gm=bless({$k5,$Tl,$g6,$q,$h6,$q,$i6,$em,$L,$fm},$r6);$hm={};$im=[];$jm=q#shift->await#;$km=bless({$t,$im,$v,$jm,$x,736,$y,$z},$A);$lm=[];$mm=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$nm=bless({$t,$lm,$v,$mm,$x,738,$y,$z},$A);$om={$h7,$nm};$pm=q#/io/pid_init.b#;$qm=bless({$k5,$hm,$g6,$q,$h6,$km,$i6,$om,$L,$pm},$r6);$rm={};$sm=q#await#;$tm=[];$um=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$vm=bless({$t,$tm,$v,$um,$x,740,$y,$z},$A);$wm=q#running#;$xm=[];$ym=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$zm=bless({$t,$xm,$v,$ym,$x,742,$y,$z},$A);$Am={$sm,$vm,$wm,$zm};$Bm=q#/io/pid_wait.b#;$Cm=bless({$k5,$rm,$g6,$q,$h6,$q,$i6,$Am,$L,$Bm},$r6);$Dm={};$Em=[];$Fm=q#shift->stdout->read(@_)#;$Gm=bless({$t,$Em,$v,$Fm,$x,744,$y,$z},$A);$Hm=[];$Im=q#shift->stdin->write(@_)#;$Jm=bless({$t,$Hm,$v,$Im,$x,746,$y,$z},$A);$Km={$ud,$Gm,$Cd,$Jm};$Lm=q#/io/pid_io.b#;$Mm=bless({$k5,$Dm,$g6,$q,$h6,$q,$i6,$Km,$L,$Lm},$r6);$Nm={};$Om=[];$Pm=q#$_[0]->{external_fds}{$_[1]}#;$Qm=bless({$t,$Om,$v,$Pm,$x,748,$y,$z},$A);$Rm=[];$Sm=q#shift->fd(2)#;$Tm=bless({$t,$Rm,$v,$Sm,$x,750,$y,$z},$A);$Um=[];$Vm=q#shift->fd(0)#;$Wm=bless({$t,$Um,$v,$Vm,$x,752,$y,$z},$A);$Xm=[];$Ym=q#shift->fd(1)#;$Zm=bless({$t,$Xm,$v,$Ym,$x,754,$y,$z},$A);$cn={$Df,$Qm,$Hf,$Tm,$Lf,$Wm,$Pf,$Zm};$dn=q#/io/pid_accessors.b#;$en=bless({$k5,$Nm,$g6,$q,$h6,$q,$i6,$cn,$L,$dn},$r6);$fn=[$kd,$gm,$qm,$Cm,$Mm,$en];$gn=bless({$k5,$Sl,$L,$A3,$X5,$fn},$x5);$hn=q#io/pid.c::ctors#;$in=q#ni:/io/pid.c#;$jn={$x5,1};$kn=q#/io/pid.c#;$ln=[$ge];$mn=bless({$k5,$jn,$L,$kn,$X5,$ln},$Y5);$nn=q#ni:/io/pid_accessors.b#;$on=q#ni:/io/pid_init.b#;$pn=q#ni:/io/pid_io.b#;$qn=q#ni:/io/pid_readers.b#;$rn=q#ni:/io/pid_wait.b#;$sn=q#ni:/io/str#;$tn={$y7,1};$un=q#/io/str#;$vn={};$wn=q#data#;$xn=[];$yn=q#shift->{'data'}#;$zn=bless({$t,$xn,$v,$yn,$x,756,$y,$z},$A);$An=q#end#;$Bn=[];$Cn=q#shift->{'end'}#;$Dn=bless({$t,$Bn,$v,$Cn,$x,758,$y,$z},$A);$En=q#start#;$Fn=[];$Gn=q#shift->{'start'}#;$Hn=bless({$t,$Fn,$v,$Gn,$x,760,$y,$z},$A);$In={$wn,$zn,$An,$Dn,$En,$Hn};$Jn=q#/io/str_ro.b#;$Kn=bless({$k5,$vn,$g6,$q,$h6,$q,$i6,$In,$L,$Jn},$r6);$Ln={};$Mn=[];$Nn=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$On=bless({$t,$Mn,$v,$Nn,$x,762,$y,$z},$A);$Pn={$h7,$On};$Qn=q#/io/str_init.b#;$Rn=bless({$k5,$Ln,$g6,$q,$h6,$q,$i6,$Pn,$L,$Qn},$r6);$Sn={};$Tn=[];$Un=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Vn=bless({$t,$Tn,$v,$Un,$x,764,$y,$z},$A);$Wn=q#remaining#;$Xn=[];$Yn=q#my $self = shift; $$self{end} - $$self{start}#;$Zn=bless({$t,$Xn,$v,$Yn,$x,766,$y,$z},$A);$co=[];$do=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$eo=bless({$t,$co,$v,$do,$x,768,$y,$z},$A);$fo={$ud,$Vn,$Wn,$Zn,$Cd,$eo};$go=q#/io/str_io.b#;$ho=bless({$k5,$Sn,$g6,$q,$h6,$q,$i6,$fo,$L,$go},$r6);$io=[$kd,$Kn,$Rn,$ho];$jo=bless({$k5,$tn,$L,$un,$X5,$io},$y5);$ko=q#io/str.c::ctors#;$lo=q#ni:/io/str.c#;$mo={$y5,1};$no=q#/io/str.c#;$oo=[$ge];$po=bless({$k5,$mo,$L,$no,$X5,$oo},$Y5);$qo=q#ni:/io/str_init.b#;$ro=q#ni:/io/str_io.b#;$so=q#ni:/io/str_ro.b#;$to=q#ni:/io/transfer#;$uo={$z7,1,$A7,1,$B7,1};$vo=q#/io/transfer#;$wo={$z7,1,$A7,1,$B7,1,$L7,1};$xo=q#/semantic/task#;$yo={};$zo=[];$Ao=bless({$t,$zo,$v,$W9,$x,770,$y,$z},$A);$Bo={$r,$Ao};$Co=q#/semantic/task_ro.b#;$Do=bless({$k5,$yo,$g6,$q,$h6,$q,$i6,$Bo,$L,$Co},$r6);$Eo={};$Fo=q#failure#;$Go=[];$Ho=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Io=bless({$t,$Go,$v,$Ho,$x,772,$y,$z},$A);$Jo=q#success#;$Ko=[];$Lo=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Mo=bless({$t,$Ko,$v,$Lo,$x,774,$y,$z},$A);$No={$Fo,$Io,$Jo,$Mo};$Oo=q#/semantic/task_outcome.b#;$Po=bless({$k5,$Eo,$g6,$q,$h6,$q,$i6,$No,$L,$Oo},$r6);$Qo=[$Y7,$Do,$Po];$Ro=bless({$k5,$wo,$L,$xo,$X5,$Qo},$V5);$So=q#semantic/task.c::ctors#;$To={};$Uo=[];$Vo=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Wo=bless({$t,$Uo,$v,$Vo,$x,776,$y,$z},$A);$Xo=[];$Yo=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Zo=bless({$t,$Xo,$v,$Yo,$x,778,$y,$z},$A);$cp=[];$dp=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$ep=bless({$t,$cp,$v,$dp,$x,780,$y,$z},$A);$fp={$ud,$Zo,$Cd,$ep};$gp=q#/io/transfer_io_interop.b#;$hp=bless({$k5,$To,$g6,$Wo,$h6,$q,$i6,$fp,$L,$gp},$r6);$ip={};$jp=q#pressure#;$kp=[];$lp=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$mp=bless({$t,$kp,$v,$lp,$x,782,$y,$z},$A);$np=q#read_limit_throughput#;$op=[];$pp=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$qp=bless({$t,$op,$v,$pp,$x,784,$y,$z},$A);$rp=q#throughput#;$sp=[];$tp=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$up=bless({$t,$sp,$v,$tp,$x,786,$y,$z},$A);$vp=q#write_limit_throughput#;$wp=[];$xp=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$yp=bless({$t,$wp,$v,$xp,$x,788,$y,$z},$A);$zp={$jp,$mp,$np,$qp,$rp,$up,$vp,$yp};$Ap=q#/io/transfer_io_measurement.b#;$Bp=bless({$k5,$ip,$g6,$q,$h6,$q,$i6,$zp,$L,$Ap},$r6);$Cp=[$Ro,$hp,$Bp];$Dp=bless({$k5,$uo,$L,$vo,$X5,$Cp},$z5);$Ep=q#io/transfer.c::ctors#;$Fp=q#ni:/io/transfer.c#;$Gp={$z5,1,$A5,1,$B5,1};$Hp=q#/io/transfer.c#;$Ip={$z5,1,$A5,1,$B5,1,$V5,1};$Jp=q#/semantic/task.c#;$Kp=[$I9];$Lp=bless({$k5,$Ip,$L,$Jp,$X5,$Kp},$Y5);$Mp={};$Np=[];$Op=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Pp=bless({$t,$Np,$v,$Op,$x,792,$y,$z},$A);$Qp={};$Rp=q#/io/transfer.c_into.b#;$Sp=bless({$k5,$Mp,$g6,$Pp,$h6,$q,$i6,$Qp,$L,$Rp},$r6);$Tp=[$Lp,$Sp];$Up=bless({$k5,$Gp,$L,$Hp,$X5,$Tp},$Y5);$Vp=q#ni:/io/transfer.c_into.b#;$Wp=q#ni:/io/transfer_async#;$Xp={$A7,1};$Yp=q#/io/transfer_async#;$Zp={};$cq=q#dest_io#;$dq=[];$eq=q#shift->{'dest_io'}#;$fq=bless({$t,$dq,$v,$eq,$x,794,$y,$z},$A);$gq=q#id#;$hq=[];$iq=q#shift->{'id'}#;$jq=bless({$t,$hq,$v,$iq,$x,796,$y,$z},$A);$kq=q#source_io#;$lq=[];$mq=q#shift->{'source_io'}#;$nq=bless({$t,$lq,$v,$mq,$x,798,$y,$z},$A);$oq={$cq,$fq,$gq,$jq,$kq,$nq};$pq=q#/io/transfer_async_ro.b#;$qq=bless({$k5,$Zp,$g6,$q,$h6,$q,$i6,$oq,$L,$pq},$r6);$rq={};$sq=[];$tq=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$uq=bless({$t,$sq,$v,$tq,$x,800,$y,$z},$A);$vq={$h7,$uq};$wq=q#/io/transfer_async_init.b#;$xq=bless({$k5,$rq,$g6,$q,$h6,$q,$i6,$vq,$L,$wq},$r6);$yq={};$zq=[];$Aq=q#ni('ni:/io/transfer_async')->track(shift)#;$Bq=bless({$t,$zq,$v,$Aq,$x,802,$y,$z},$A);$Cq=[];$Dq=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$Eq=bless({$t,$Cq,$v,$Dq,$x,804,$y,$z},$A);$Fq={};$Gq=q#/io/transfer_async_lifecycle.b#;$Hq=bless({$k5,$yq,$g6,$Bq,$h6,$Eq,$i6,$Fq,$L,$Gq},$r6);$Iq={};$Jq=q#run#;$Kq=[];$Lq=q#shift#;$Mq=bless({$t,$Kq,$v,$Lq,$x,806,$y,$z},$A);$Nq=q#run_async#;$Oq=[];$Pq=q#my $self = shift;
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

$self;#;$Qq=bless({$t,$Oq,$v,$Pq,$x,808,$y,$z},$A);$Rq={$Jq,$Mq,$Nq,$Qq};$Sq=q#/io/transfer_async_run.b#;$Tq=bless({$k5,$Iq,$g6,$q,$h6,$q,$i6,$Rq,$L,$Sq},$r6);$Uq=[$Dp,$qq,$xq,$Hq,$Tq];$Vq=q#tracked_transfers#;$Wq={};$Xq=q#transfer_id#;$Yq=bless({$k5,$Xp,$L,$Yp,$X5,$Uq,$Vq,$Wq,$Xq,0},$A5);$Zq=q#io/transfer_async.c::ctors#;$cr=q#ni:/io/transfer_async.c#;$dr={$A5,1};$er=q#/io/transfer_async.c#;$fr={};$gr=[];$hr=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$ir=bless({$t,$gr,$v,$hr,$x,818,$y,$z},$A);$jr=q#new_id#;$kr=[];$lr=q#++shift->{transfer_id}#;$mr=bless({$t,$kr,$v,$lr,$x,820,$y,$z},$A);$nr=q#track#;$or=[];$pr=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$qr=bless({$t,$or,$v,$pr,$x,822,$y,$z},$A);$rr=q#untrack#;$sr=[];$tr=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$ur=bless({$t,$sr,$v,$tr,$x,824,$y,$z},$A);$vr={$jr,$mr,$nr,$qr,$rr,$ur};$wr=q#/io/transfer_async.c_tracker.b#;$xr=bless({$k5,$fr,$g6,$ir,$h6,$q,$i6,$vr,$L,$wr},$r6);$yr=[$Up,$xr];$zr=bless({$k5,$dr,$L,$er,$X5,$yr},$Y5);$Ar=q#ni:/io/transfer_async.c_tracker.b#;$Br=q#ni:/io/transfer_async_init.b#;$Cr=q#ni:/io/transfer_async_lifecycle.b#;$Dr=q#ni:/io/transfer_async_ro.b#;$Er=q#ni:/io/transfer_async_run.b#;$Fr=q#ni:/io/transfer_io_interop.b#;$Gr=q#ni:/io/transfer_io_measurement.b#;$Hr=q#ni:/io/transfer_sync#;$Ir={$B7,1};$Jr=q#/io/transfer_sync#;$Kr={};$Lr=[];$Mr=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Nr=bless({$t,$Lr,$v,$Mr,$x,826,$y,$z},$A);$Or={$h7,$Nr};$Pr=q#/io/transfer_sync_init.b#;$Qr=bless({$k5,$Kr,$g6,$q,$h6,$q,$i6,$Or,$L,$Pr},$r6);$Rr={};$Sr=[];$Tr=q#my $self = shift;
my $buf;
my $r;
while (($r = $self->read($buf, 32768)) || $!{EINTR}) {
  my $n = $self->write($buf);
  $self->failure($!) unless $n || $!{EINTR};
  while ($n < $r) {
    my $n0 = $self->write($buf, $r - $n, $n);
    $self->failure($!) unless $!{EINTR} || $n0;
    $n += $n0 || 0;
  }
}
$$self{end_time} = time;
$self->success;#;$Ur=bless({$t,$Sr,$v,$Tr,$x,828,$y,$z},$A);$Vr={$Jq,$Ur};$Wr=q#/io/transfer_sync_run.b#;$Xr=bless({$k5,$Rr,$g6,$q,$h6,$q,$i6,$Vr,$L,$Wr},$r6);$Yr=[$Dp,$Qr,$Xr];$Zr=bless({$k5,$Ir,$L,$Jr,$X5,$Yr},$B5);$cs=q#io/transfer_sync.c::ctors#;$ds=q#ni:/io/transfer_sync.c#;$es={$B5,1};$fs=q#/io/transfer_sync.c#;$gs=[$Up];$hs=bless({$k5,$es,$L,$fs,$X5,$gs},$Y5);$is=q#ni:/io/transfer_sync_init.b#;$js=q#ni:/io/transfer_sync_run.b#;$ks=q#ni:/lib/accessor.b#;$ls=q#ni:/lib/behavior#;$ms=q#ni:/lib/behavior.c#;$ns=q#ni:/lib/branch#;$os={$t6,1};$ps=q#/lib/branch#;$qs={};$rs=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$ss=bless({$v,$rs,$x,834,$y,$z},$A);$ts={$h7,$ss};$us=q#/lib/branch_init.b#;$vs=bless({$k5,$qs,$g6,$q,$h6,$q,$i6,$ts,$L,$us},$r6);$ws=[$l8,$C6,$s6,$vs,$e9];$xs=bless({$k5,$os,$L,$ps,$X5,$ws},$D5);$ys=q#lib/branch.c::ctors#;$zs=q#ni:/lib/branch.b#;$As=q#ni:/lib/branch.c#;$Bs={$D5,1};$Cs=q#/lib/branch.c#;$Ds=[$N9];$Es=bless({$k5,$Bs,$L,$Cs,$X5,$Ds},$Y5);$Fs=q#ni:/lib/branch_init.b#;$Gs=q#ni:/lib/class_init.b#;$Hs=q#ni:/lib/dataslice#;$Is={$D7,1};$Js=q#/lib/dataslice#;$Ks={};$Ls=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Ms=bless({$v,$Ls,$x,836,$y,$z},$A);$Ns={$h7,$Ms};$Os=q#/lib/dataslice_init.b#;$Ps=bless({$k5,$Ks,$g6,$q,$h6,$q,$i6,$Ns,$L,$Os},$r6);$Qs={};$Rs=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Ss=bless({$v,$Rs,$x,838,$y,$z},$A);$Ts={$m6,$Ss};$Us=q#/lib/dataslice_apply.b#;$Vs=bless({$k5,$Qs,$g6,$q,$h6,$q,$i6,$Ts,$L,$Us},$r6);$Ws=[$l8,$Ps,$Vs];$Xs=bless({$k5,$Is,$L,$Js,$X5,$Ws},$E5);$Ys=q#lib/dataslice.c::ctors#;$Zs=q#ni:/lib/dataslice.c#;$ct={$E5,1};$dt=q#/lib/dataslice.c#;$et=[$N9];$ft=bless({$k5,$ct,$L,$dt,$X5,$et},$Y5);$gt=q#ni:/lib/dataslice_apply.b#;$ht=q#ni:/lib/dataslice_init.b#;$it=q#ni:/lib/definition.b#;$jt=q#ni:/lib/definition_def.b#;$kt=q#ni:/lib/definition_defdata.b#;$lt=q#ni:/lib/doc#;$mt={$N,1};$nt={};$ot=q#shift; +{name => shift, doc => []}#;$pt=bless({$v,$ot,$x,840,$y,$z},$A);$qt={$h7,$pt};$rt=q#/lib/doc_init.b#;$st=bless({$k5,$nt,$g6,$q,$h6,$q,$i6,$qt,$L,$rt},$r6);$tt={};$ut=q#'ni.doc'#;$vt=bless({$v,$ut,$x,842,$y,$z},$A);$wt={$F6,$vt};$xt=q#/lib/doc_namespace.b#;$yt=bless({$k5,$tt,$g6,$q,$h6,$q,$i6,$wt,$L,$xt},$r6);$zt={};$At=q#AUTOLOAD#;$Bt=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;#;$Ct=bless({$v,$Bt,$x,844,$y,$z},$A);$Dt={$At,$Ct};$Et=q#/lib/doc_define.b#;$Ft=bless({$k5,$zt,$g6,$q,$h6,$q,$i6,$Dt,$L,$Et},$r6);$Gt={};$Ht=q#shift->referent#;$It=bless({$v,$Ht,$x,846,$y,$z},$A);$Jt=q#referent#;$Kt=q#ni 'ni:' . shift->{name}#;$Lt=bless({$v,$Kt,$x,848,$y,$z},$A);$Mt={$An,$It,$Jt,$Lt};$Nt=q#/lib/doc_end.b#;$Ot=bless({$k5,$Gt,$g6,$q,$h6,$q,$i6,$Mt,$L,$Nt},$r6);$Pt={};$Qt=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$Rt=bless({$v,$Qt,$x,850,$y,$z},$A);$St=q#linearized#;$Tt=q#map @$_, @{shift->{doc}}#;$Ut=bless({$v,$Tt,$x,852,$y,$z},$A);$Vt=q#tests#;$Wt=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$Xt=bless({$v,$Wt,$x,854,$y,$z},$A);$Yt={$g3,$Rt,$St,$Ut,$Vt,$Xt};$Zt=q#/lib/doc_test.b#;$cu=bless({$k5,$Pt,$g6,$q,$h6,$q,$i6,$Yt,$L,$Zt},$r6);$du=[$Y7,$C6,$st,$yt,$Ft,$Ot,$cu];$eu=bless({$k5,$mt,$L,$d4,$X5,$du},$F5);$fu=q#lib/doc.c::ctors#;$gu=q#ni:/lib/doc.c#;$hu={$F5,1};$iu=q#/lib/doc.c#;$ju=[$I9];$ku=bless({$k5,$hu,$L,$iu,$X5,$ju},$Y5);$lu=q#ni:/lib/doc_define.b#;$mu=q#ni:/lib/doc_end.b#;$nu=q#ni:/lib/doc_init.b#;$ou=q#ni:/lib/doc_namespace.b#;$pu=q#ni:/lib/doc_test.b#;$qu=q#ni:/lib/documentable.b#;$ru=q#ni:/lib/fn#;$su={$A,1};$tu=q#/lib/fn#;$uu={};$vu=q#shift->compile#;$wu=bless({$v,$vu,$x,856,$y,$z},$A);$xu=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$yu=bless({$v,$xu,$x,858,$y,$z},$A);$zu=q#compile#;$Au=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$Bu=bless({$v,$Au,$x,860,$y,$z},$A);$Cu=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$Du=bless({$v,$Cu,$x,862,$y,$z},$A);$Eu={$zu,$Bu,$h7,$Du};$Fu=q#/lib/fn_init.b#;$Gu=bless({$k5,$uu,$g6,$wu,$h6,$yu,$i6,$Eu,$L,$Fu},$r6);$Hu={};$Iu=[];$Ju=q#shift->{'annotations'}#;$Ku=bless({$t,$Iu,$v,$Ju,$x,864,$y,$z},$A);$Lu=[];$Mu=q#shift->{'code'}#;$Nu=bless({$t,$Lu,$v,$Mu,$x,866,$y,$z},$A);$Ou=[];$Pu=q#shift->{'eval_number'}#;$Qu=bless({$t,$Ou,$v,$Pu,$x,868,$y,$z},$A);$Ru=q#fn#;$Su=[];$Tu=q#shift->{'fn'}#;$Uu=bless({$t,$Su,$v,$Tu,$x,870,$y,$z},$A);$Vu={$t,$Ku,$v,$Nu,$x,$Qu,$Ru,$Uu};$Wu=q#/lib/fn_ro.b#;$Xu=bless({$k5,$Hu,$g6,$q,$h6,$q,$i6,$Vu,$L,$Wu},$r6);$Yu={};$Zu=[];$cv=q#my $self = shift; "fn {$$self{code}}"#;$dv=bless({$t,$Zu,$v,$cv,$x,872,$y,$z},$A);$ev=[];$fv=bless({$t,$ev,$v,$P8,$x,874,$y,$z},$A);$gv={$H8,$dv,$O8,$fv};$hv=q#/lib/fn_ops.b#;$iv=bless({$k5,$Yu,$g6,$q,$h6,$q,$i6,$gv,$L,$hv},$r6);$jv={};$kv=q#serialize#;$lv=[];$mv=q#local $_;
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
delete $state{fn};
$state{code} = join "\\n", @lines;
$quote->quote_blessed(\\%state, ref $self);#;$nv=bless({$t,$lv,$v,$mv,$x,876,$y,$z},$A);$ov={$kv,$nv};$pv=q#/lib/fn_serialize.b#;$qv=bless({$k5,$jv,$g6,$q,$h6,$q,$i6,$ov,$L,$pv},$r6);$rv=[$Y7,$p9,$Gu,$Xu,$iv,$qv];$sv=bless({$k5,$su,$L,$tu,$X5,$rv},$G5);$tv=q#lib/fn.c::ctors#;$uv=q#ni:/lib/fn.c#;$vv={$G5,1};$wv=q#/lib/fn.c#;$xv={};$yv=[];$zv=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$Av=bless({$t,$yv,$v,$zv,$x,880,$y,$z},$A);$Bv=q#resolve_evals#;$Cv=[];$Dv=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$Ev=bless({$t,$Cv,$v,$Dv,$x,882,$y,$z},$A);$Fv={$Bv,$Ev};$Gv=q#/lib/fn.c_resolve_eval.b#;$Hv=bless({$k5,$xv,$g6,$Av,$h6,$q,$i6,$Fv,$L,$Gv},$r6);$Iv=[$I9,$Hv];$Jv=bless({$k5,$vv,$L,$wv,$X5,$Iv},$Y5);$Kv=q#ni:/lib/fn.c_resolve_eval.b#;$Lv=q#ni:/lib/fn_init.b#;$Mv=q#ni:/lib/fn_ops.b#;$Nv=q#ni:/lib/fn_ro.b#;$Ov=q#ni:/lib/fn_serialize.b#;$Pv=q#ni:/lib/gensym_generator_compact.b#;$Qv={};$Rv=q#gensym#;$Sv=[];$Tv=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Uv=bless({$t,$Sv,$v,$Tv,$x,884,$y,$z},$A);$Vv={$Rv,$Uv};$Wv=q#/lib/gensym_generator_compact.b#;$Xv=bless({$k5,$Qv,$g6,$q,$h6,$q,$i6,$Vv,$L,$Wv},$r6);$Yv=q#ni:/lib/global_static_test.b#;$Zv={};$cw=[];$dw=q#ni('ni:/lib/test_case')->new(shift)#;$ew=q#($)#;$fw=bless({$t,$cw,$v,$dw,$x,886,$y,$ew},$A);$gw=q#now#;$hw=[];$iw=q#ni('ni:/lib/test_value')->new(shift)#;$jw=bless({$t,$hw,$v,$iw,$x,888,$y,$ew},$A);$kw={$g3,$fw,$gw,$jw};$lw=q#/lib/global_static_test.b#;$mw=bless({$k5,$Zv,$g6,$q,$h6,$q,$i6,$kw,$L,$lw},$r6);$nw=q#ni:/lib/image#;$ow={$E7,1};$pw={};$qw=[];$rw=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$sw=bless({$t,$qw,$v,$rw,$x,890,$y,$z},$A);$tw={$h7,$sw};$uw=q#/lib/image_init.b#;$vw=bless({$k5,$pw,$g6,$q,$h6,$q,$i6,$tw,$L,$uw},$r6);$ww={};$xw=q#address#;$yw=[];$zw=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$Aw=bless({$t,$yw,$v,$zw,$x,892,$y,$z},$A);$Bw=q#allocate_gensym#;$Cw=[];$Dw=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$Ew=bless({$t,$Cw,$v,$Dw,$x,894,$y,$z},$A);$Fw=q#boot_side_effect#;$Gw=[];$Hw=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Iw=bless({$t,$Gw,$v,$Hw,$x,896,$y,$z},$A);$Jw=q#circular_links#;$Kw=[];$Lw=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Mw=bless({$t,$Kw,$v,$Lw,$x,898,$y,$z},$A);$Nw=q#finalizer#;$Ow=[];$Pw=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Qw=bless({$t,$Ow,$v,$Pw,$x,900,$y,$z},$A);$Rw=q#io#;$Sw=[];$Tw=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Uw=bless({$t,$Sw,$v,$Tw,$x,902,$y,$z},$A);$Vw=q#quote#;$Ww=[];$Xw=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Yw=bless({$t,$Ww,$v,$Xw,$x,904,$y,$z},$A);$Zw=q#reconstruction#;$cx=[];$dx=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$ex=bless({$t,$cx,$v,$dx,$x,906,$y,$z},$A);$fx=q#side_effect#;$gx=[];$hx=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$ix=bless({$t,$gx,$v,$hx,$x,908,$y,$z},$A);$jx={$xw,$Aw,$Bw,$Ew,$Fw,$Iw,$Jw,$Mw,$Nw,$Qw,$Rw,$Uw,$Vw,$Yw,$Zw,$ex,$fx,$ix};$kx=q#/lib/image_quoting.b#;$lx=bless({$k5,$ww,$g6,$q,$h6,$q,$i6,$jx,$L,$kx},$r6);$mx={};$nx=q#quote_code#;$ox=[];$px=q#shift->die('cannot quote perl CODE refs', shift)#;$qx=bless({$t,$ox,$v,$px,$x,910,$y,$z},$A);$rx={$nx,$qx};$sx=q#/lib/quote_code_fail.b#;$tx=bless({$k5,$mx,$g6,$q,$h6,$q,$i6,$rx,$L,$sx},$r6);$ux={};$vx=q#quote_array#;$wx=[];$xx=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$yx=bless({$t,$wx,$v,$xx,$x,912,$y,$z},$A);$zx=q#quote_hash#;$Ax=[];$Bx=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$Cx=bless({$t,$Ax,$v,$Bx,$x,914,$y,$z},$A);$Dx=q#quote_scalar#;$Ex=[];$Fx=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$Gx=bless({$t,$Ex,$v,$Fx,$x,916,$y,$z},$A);$Hx=q#quote_scalar_ref#;$Ix=[];$Jx=q#'\\\\' . shift->quote(${$_[0]})#;$Kx=bless({$t,$Ix,$v,$Jx,$x,918,$y,$z},$A);$Lx=q#quote_value#;$Mx=[];$Nx=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Ox=bless({$t,$Mx,$v,$Nx,$x,920,$y,$z},$A);$Px={$vx,$yx,$zx,$Cx,$Dx,$Gx,$Hx,$Kx,$Lx,$Ox};$Qx=q#/lib/quote_values.b#;$Rx=bless({$k5,$ux,$g6,$q,$h6,$q,$i6,$Px,$L,$Qx},$r6);$Sx={};$Tx=q#quote_blessed#;$Ux=[];$Vx=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Wx=bless({$t,$Ux,$v,$Vx,$x,922,$y,$z},$A);$Xx=q#quote_class#;$Yx=[];$Zx=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");#;$cy=bless({$t,$Yx,$v,$Zx,$x,924,$y,$z},$A);$dy=q#quote_object#;$ey=[];$fy=q#my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . '::ctors') . ';')
  if @{ref($_[0]) . '::ctors'};
$q;#;$gy=bless({$t,$ey,$v,$fy,$x,926,$y,$z},$A);$hy={$Tx,$Wx,$Xx,$cy,$dy,$gy};$iy=q#/lib/quote_objects.b#;$jy=bless({$k5,$Sx,$g6,$q,$h6,$q,$i6,$hy,$L,$iy},$r6);$ky={};$ly=q#circular_arrayref#;$my=[];$ny=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$oy=bless({$t,$my,$v,$ny,$x,928,$y,$z},$A);$py=q#circular_hashref#;$qy=[];$ry=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$sy=bless({$t,$qy,$v,$ry,$x,930,$y,$z},$A);$ty=q#is_circular#;$uy=[];$vy=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$wy=bless({$t,$uy,$v,$vy,$x,932,$y,$z},$A);$xy={$ly,$oy,$py,$sy,$ty,$wy};$yy=q#/lib/quote_circular_addressed.b#;$zy=bless({$k5,$ky,$g6,$q,$h6,$q,$i6,$xy,$L,$yy},$r6);$Ay=[$Y7,$vw,$lx,$tx,$Rx,$jy,$zy,$Xv];$By=bless({$k5,$ow,$L,$l4,$X5,$Ay},$H5);$Cy=q#lib/image.c::ctors#;$Dy=q#ni:/lib/image.c#;$Ey={$H5,1};$Fy=q#/lib/image.c#;$Gy=[$I9];$Hy=bless({$k5,$Ey,$L,$Fy,$X5,$Gy},$Y5);$Iy=q#ni:/lib/image_init.b#;$Jy=q#ni:/lib/image_quoting.b#;$Ky=q#ni:/lib/instance.b#;$Ly=q#ni:/lib/instantiable.b#;$My=q#ni:/lib/json.b#;$Ny={};$Oy=q#json_decode#;$Py=[];$Qy=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Ry=bless({$t,$Py,$v,$Qy,$x,934,$y,$ew},$A);$Sy=q#json_encode#;$Ty=[];$Uy=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Vy=bless({$t,$Ty,$v,$Uy,$x,936,$y,$ew},$A);$Wy=q#json_escape#;$Xy=[];$Yy=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$Zy=bless({$t,$Xy,$v,$Yy,$x,938,$y,$ew},$A);$cz=q#json_unescape#;$dz=[];$ez=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$fz=bless({$t,$dz,$v,$ez,$x,940,$y,$ew},$A);$gz=q#json_unescape_one#;$hz=[];$iz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$jz=bless({$t,$hz,$v,$iz,$x,942,$y,$ew},$A);$kz={$Oy,$Ry,$Sy,$Vy,$Wy,$Zy,$cz,$fz,$gz,$jz};$lz=q#/lib/json.b#;$mz=bless({$k5,$Ny,$g6,$q,$h6,$q,$i6,$kz,$L,$lz},$r6);$nz=q#ni#;$oz=q#ni:/lib/name_as_string.b#;$pz=q#ni:/lib/named.b#;$qz=q#ni:/lib/named_in_ni.b#;$rz=q#ni:/lib/namespaced.b#;$sz=q#ni:/lib/ni#;$tz={$F7,1};$uz={};$vz=q#extend#;$wz=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$xz=bless({$v,$wz,$x,944,$y,$z},$A);$yz=q#is_mutable#;$zz=q#$0 ne '-' && -w $0#;$Az=bless({$v,$zz,$x,946,$y,$z},$A);$Bz=q#modify#;$Cz=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$Dz=bless({$v,$Cz,$x,948,$y,$z},$A);$Ez={$vz,$xz,$yz,$Az,$Bz,$Dz};$Fz=q#/lib/ni_self.b#;$Gz=bless({$k5,$uz,$g6,$q,$h6,$q,$i6,$Ez,$L,$Fz},$r6);$Hz={};$Iz=q#--internal/+=#;$Jz=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted(use_newlines => 1);
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$Kz=bless({$v,$Jz,$x,950,$y,$z},$A);$Lz=q#--internal/eval#;$Mz=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$Nz=bless({$v,$Mz,$x,952,$y,$z},$A);$Oz=q#--internal/image#;$Pz=q#shift->quoted(use_newlines => 1)->io->into_sync(ni"fd:1");
0;#;$Qz=bless({$v,$Pz,$x,954,$y,$z},$A);$Rz=q#--internal/test#;$Sz=q#my $self   = shift;
my $failed = 0;
my @tests  = map ni($_)->tests, grep /^ni\\.doc:/, keys %{$$self{named}};
for (@tests) {
  $_->run;
  print "$_\\n";
}
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
!!$failed;#;$Tz=bless({$v,$Sz,$x,956,$y,$z},$A);$Uz=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$Vz=bless({$v,$Uz,$x,958,$y,$z},$A);$Wz={$Iz,$Kz,$Lz,$Nz,$Oz,$Qz,$Rz,$Tz,$Jq,$Vz};$Xz=q#/lib/ni_main.b#;$Yz=bless({$k5,$Hz,$g6,$q,$h6,$q,$i6,$Wz,$L,$Xz},$r6);$Zz={};$cA=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$dA=bless({$v,$cA,$x,960,$y,$z},$A);$eA=q#resolver_for#;$fA=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$gA=bless({$v,$fA,$x,962,$y,$z},$A);$hA={$T6,$dA,$eA,$gA};$iA=q#/lib/ni_resolver.b#;$jA=bless({$k5,$Zz,$g6,$q,$h6,$q,$i6,$hA,$L,$iA},$r6);$kA={};$lA=q#exists#;$mA=q#exists $_[0]->{named}{$_[1]}#;$nA=bless({$v,$mA,$x,964,$y,$z},$A);$oA=q#quoted#;$pA=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$qA=bless({$v,$pA,$x,966,$y,$z},$A);$rA={$lA,$nA,$oA,$qA};$sA=q#/lib/ni_image.b#;$tA=bless({$k5,$kA,$g6,$q,$h6,$q,$i6,$rA,$L,$sA},$r6);$uA=[$Y7,$Gz,$Yz,$jA,$tA];$vA=bless({$k5,$tz,$L,$t4,$X5,$uA},$I5);$wA=q#lib/ni.c::ctors#;$xA=q#ni:/lib/ni.c#;$yA={$I5,1};$zA=q#/lib/ni.c#;$AA=[$I9];$BA=bless({$k5,$yA,$L,$zA,$X5,$AA},$Y5);$CA=q#ni:/lib/ni_image.b#;$DA=q#ni:/lib/ni_main.b#;$EA=q#ni:/lib/ni_resolver.b#;$FA=q#ni:/lib/ni_self.b#;$GA=q#ni:/lib/ni_static_util.b#;$HA={};$IA=q#abbrev#;$JA=[];$KA=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$LA=bless({$t,$JA,$v,$KA,$x,968,$y,$z},$A);$MA=q#dor#;$NA=[];$OA=q#defined $_[0] ? $_[0] : $_[1]#;$PA=bless({$t,$NA,$v,$OA,$x,970,$y,$z},$A);$QA=q#indent#;$RA=[];$SA=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$TA=bless({$t,$RA,$v,$SA,$x,972,$y,$z},$A);$UA=q#max#;$VA=[];$WA=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$XA=bless({$t,$VA,$v,$WA,$x,974,$y,$z},$A);$YA=q#maxstr#;$ZA=[];$cB=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$dB=bless({$t,$ZA,$v,$cB,$x,976,$y,$z},$A);$eB=q#mean#;$fB=[];$gB=q#sum(@_) / (@_ || 1)#;$hB=bless({$t,$fB,$v,$gB,$x,978,$y,$z},$A);$iB=q#min#;$jB=[];$kB=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$lB=bless({$t,$jB,$v,$kB,$x,980,$y,$z},$A);$mB=q#minstr#;$nB=[];$oB=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$pB=bless({$t,$nB,$v,$oB,$x,982,$y,$z},$A);$qB=q#sgr#;$rB=[];$sB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$tB=bless({$t,$rB,$v,$sB,$x,984,$y,$z},$A);$uB=q#sr#;$vB=[];$wB=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$xB=bless({$t,$vB,$v,$wB,$x,986,$y,$z},$A);$yB=q#sum#;$zB=[];$AB=q#local $_; my $x = 0; $x += $_ for @_; $x#;$BB=bless({$t,$zB,$v,$AB,$x,988,$y,$z},$A);$CB=q#swap#;$DB=[];$EB=q#@_[0, 1] = @_[1, 0]#;$FB=bless({$t,$DB,$v,$EB,$x,990,$y,$z},$A);$GB={$IA,$LA,$MA,$PA,$QA,$TA,$UA,$XA,$YA,$dB,$eB,$hB,$iB,$lB,$mB,$pB,$qB,$tB,$uB,$xB,$yB,$BB,$CB,$FB};$HB=q#/lib/ni_static_util.b#;$IB=bless({$k5,$HA,$g6,$q,$h6,$q,$i6,$GB,$L,$HB},$r6);$JB=q#ni:/lib/perlbranch.b#;$KB=q#ni:/lib/quote_circular_addressed.b#;$LB=q#ni:/lib/quote_code_fail.b#;$MB=q#ni:/lib/quote_objects.b#;$NB=q#ni:/lib/quote_simple#;$OB={$G7,1};$PB={};$QB=[];$RB=q#+{}#;$SB=bless({$t,$QB,$v,$RB,$x,992,$y,$z},$A);$TB={$h7,$SB};$UB=q#/lib/quote_simple_init.b#;$VB=bless({$k5,$PB,$g6,$q,$h6,$q,$i6,$TB,$L,$UB},$r6);$WB={};$XB=[];$YB=bless({$t,$XB,$v,0,$x,994,$y,$z},$A);$ZB=[];$cC=q#shift->quote_value(shift)#;$dC=bless({$t,$ZB,$v,$cC,$x,996,$y,$z},$A);$eC={$ty,$YB,$Vw,$dC};$fC=q#/lib/quote_simple_quote.b#;$gC=bless({$k5,$WB,$g6,$q,$h6,$q,$i6,$eC,$L,$fC},$r6);$hC=[$Y7,$VB,$gC,$tx,$Rx,$jy];$iC=bless({$k5,$OB,$L,$E4,$X5,$hC},$J5);$jC=q#lib/quote_simple.c::ctors#;$kC=q#ni:/lib/quote_simple.c#;$lC={$J5,1};$mC=q#/lib/quote_simple.c#;$nC=[$I9];$oC=bless({$k5,$lC,$L,$mC,$X5,$nC},$Y5);$pC=q#ni:/lib/quote_simple_init.b#;$qC=q#ni:/lib/quote_simple_quote.b#;$rC=q#ni:/lib/quote_values.b#;$sC=q#ni:/lib/ref_eq.b#;$tC=q#ni:/lib/resolver.b#;$uC=q#ni:/lib/slice#;$vC={$r6,1};$wC=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);#;$xC=bless({$v,$wC,$x,998,$y,$z},$A);$yC=q#local $_;
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
$self;#;$zC=bless({$v,$yC,$x,1000,$y,$z},$A);$AC=q#lib/slice::apply#;$BC=q#lib/slice::apply_unsafe#;$CC={};$DC=q#apply_unsafe#;$EC={$m6,$xC,$DC,$zC};$FC=q#/lib/slice.b#;$GC=bless({$k5,$CC,$i6,$EC,$L,$FC},$r6);$HC={};$IC=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$JC=bless({$v,$IC,$x,1002,$y,$z},$A);$KC={$h7,$JC};$LC=q#/lib/slice_init.b#;$MC=bless({$k5,$HC,$i6,$KC,$L,$LC},$r6);$NC={};$OC=[];$PC=q#local $_;
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
$quote->side_effect("$g\\->apply_unsafe(" . $quote->quote($_) . ");")
  for sort keys %{$$self{applied_to}};
$g;#;$QC=bless({$t,$OC,$v,$PC,$x,1004,$y,$z},$A);$RC={$kv,$QC};$SC=q#/lib/slice_serialize.b#;$TC=bless({$k5,$NC,$g6,$q,$h6,$q,$i6,$RC,$L,$SC},$r6);$UC=[$l8,$C6,$GC,$MC,$TC];$VC=bless({$k5,$vC,$L,$Z4,$X5,$UC},$K5);$WC=q#lib/slice.c::ctors#;$XC=q#ni:/lib/slice.b#;$YC=q#ni:/lib/slice.c#;$ZC={$K5,1};$cD=q#/lib/slice.c#;$dD=[$N9];$eD=bless({$k5,$ZC,$L,$cD,$X5,$dD},$Y5);$fD=q#ni:/lib/slice_init.b#;$gD=q#ni:/lib/slice_serialize.b#;$hD=q#ni:/lib/static_fn.b#;$iD={};$jD=[];$kD=q#ni('ni:/lib/fn')->new(@_)#;$lD=bless({$t,$jD,$v,$kD,$x,1006,$y,$ew},$A);$mD=q#fp#;$nD=[];$oD=q#($$)#;$pD=bless({$t,$nD,$v,$kD,$x,1008,$y,$oD},$A);$qD={$Ru,$lD,$mD,$pD};$rD=q#/lib/static_fn.b#;$sD=bless({$k5,$iD,$g6,$q,$h6,$q,$i6,$qD,$L,$rD},$r6);$tD=q#ni:/lib/subclass.b#;$uD=q#ni:/lib/tag#;$vD={$D6,1};$wD=q#/lib/tag#;$xD={};$yD=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$zD=bless({$v,$yD,$x,1010,$y,$z},$A);$AD={$m6,$zD};$BD=q#/lib/tag.b#;$CD=bless({$k5,$xD,$g6,$q,$h6,$q,$i6,$AD,$L,$BD},$r6);$DD={};$ED=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$FD=bless({$v,$ED,$x,1012,$y,$z},$A);$GD={$h7,$FD};$HD=q#/lib/tag_init.b#;$ID=bless({$k5,$DD,$g6,$q,$h6,$q,$i6,$GD,$L,$HD},$r6);$JD=[$l8,$C6,$CD,$ID];$KD=bless({$k5,$vD,$L,$wD,$X5,$JD},$L5);$LD=q#lib/tag.c::ctors#;$MD=q#ni:/lib/tag.b#;$ND=q#ni:/lib/tag.c#;$OD={$L5,1};$PD=q#/lib/tag.c#;$QD=[$N9];$RD=bless({$k5,$OD,$L,$PD,$X5,$QD},$Y5);$SD=q#ni:/lib/tag_init.b#;$TD=q#ni:/lib/test_assert_eq#;$UD={$H7,1};$VD=q#/lib/test_assert_eq#;$WD={$H7,1,$I7,1};$XD=q#/lib/test_assertion#;$YD={};$ZD=q#commit#;$cE=[];$dE=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$eE=bless({$t,$cE,$v,$dE,$x,1014,$y,$z},$A);$fE={$ZD,$eE};$gE=q#/lib/test_assertion_commit.b#;$hE=bless({$k5,$YD,$g6,$q,$h6,$q,$i6,$fE,$L,$gE},$r6);$iE=[$Y7,$hE];$jE=bless({$k5,$WD,$L,$XD,$X5,$iE},$N5);$kE=q#lib/test_assertion.c::ctors#;$lE={};$mE=[];$nE=q#my ($class, $diff) = @_;
+{diff => $diff};#;$oE=bless({$t,$mE,$v,$nE,$x,1016,$y,$z},$A);$pE={$h7,$oE};$qE=q#/lib/test_assert_eq_init.b#;$rE=bless({$k5,$lE,$g6,$q,$h6,$q,$i6,$pE,$L,$qE},$r6);$sE={};$tE=[];$uE=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode $$self{diff}
              : "PASS";#;$vE=bless({$t,$tE,$v,$uE,$x,1018,$y,$z},$A);$wE=q#failed#;$xE=[];$yE=q#defined shift->{diff}#;$zE=bless({$t,$xE,$v,$yE,$x,1020,$y,$z},$A);$AE=q#result#;$BE=[];$CE=bless({$t,$BE,$v,$Lq,$x,1022,$y,$z},$A);$DE={$H8,$vE,$wE,$zE,$AE,$CE};$EE=q#/lib/test_assert_eq_result.b#;$FE=bless({$k5,$sE,$g6,$q,$h6,$q,$i6,$DE,$L,$EE},$r6);$GE=[$jE,$rE,$FE];$HE=bless({$k5,$UD,$L,$VD,$X5,$GE},$M5);$IE=q#lib/test_assert_eq.c::ctors#;$JE=q#ni:/lib/test_assert_eq.c#;$KE={$M5,1};$LE=q#/lib/test_assert_eq.c#;$ME={$M5,1,$N5,1};$NE=q#/lib/test_assertion.c#;$OE=[$I9];$PE=bless({$k5,$ME,$L,$NE,$X5,$OE},$Y5);$QE=[$PE];$RE=bless({$k5,$KE,$L,$LE,$X5,$QE},$Y5);$SE=q#ni:/lib/test_assert_eq_init.b#;$TE=q#ni:/lib/test_assert_eq_result.b#;$UE=q#ni:/lib/test_assertion#;$VE=q#ni:/lib/test_assertion.c#;$WE=q#ni:/lib/test_assertion_commit.b#;$XE=q#ni:/lib/test_case#;$YE={$D,1};$ZE=q#/lib/test_case#;$cF=q#running_test#;$dF={};$eF=[];$fF=q#shift->{'assertions'}#;$gF=bless({$t,$eF,$v,$fF,$x,1024,$y,$z},$A);$hF=[];$iF=q#shift->{'test'}#;$jF=bless({$t,$hF,$v,$iF,$x,1026,$y,$z},$A);$kF={$n,$gF,$s,$jF};$lF=q#/lib/test_case_ro.b#;$mF=bless({$k5,$dF,$g6,$q,$h6,$q,$i6,$kF,$L,$lF},$r6);$nF={};$oF=[];$pF=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$qF=bless({$t,$oF,$v,$pF,$x,1028,$y,$z},$A);$rF={$p,$qF};$sF=q#/lib/test_case_rw.b#;$tF=bless({$k5,$nF,$g6,$q,$h6,$q,$i6,$rF,$L,$sF},$r6);$uF={};$vF=[];$wF=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$xF=bless({$t,$vF,$v,$wF,$x,1030,$y,$z},$A);$yF={$h7,$xF};$zF=q#/lib/test_case_init.b#;$AF=bless({$k5,$uF,$g6,$q,$h6,$q,$i6,$yF,$L,$zF},$r6);$BF={};$CF=[];$DF=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$EF=bless({$t,$CF,$v,$DF,$x,1032,$y,$z},$A);$FF=[];$GF=q#!shift->{outcome}->[0]#;$HF=bless({$t,$FF,$v,$GF,$x,1034,$y,$z},$A);$IF={$H8,$EF,$wE,$HF};$JF=q#/lib/test_case_metrics.b#;$KF=bless({$k5,$BF,$g6,$q,$h6,$q,$i6,$IF,$L,$JF},$r6);$LF={};$MF=q#done#;$NF=[];$OF=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$PF=bless({$t,$NF,$v,$OF,$x,1036,$y,$z},$A);$QF=[];$RF=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$SF=bless({$t,$QF,$v,$RF,$x,1038,$y,$z},$A);$TF={$MF,$PF,$Jq,$SF};$UF=q#/lib/test_case_run.b#;$VF=bless({$k5,$LF,$g6,$q,$h6,$q,$i6,$TF,$L,$UF},$r6);$WF=[$Y7,$mF,$tF,$AF,$KF,$VF];$XF=bless({$k5,$YE,$L,$ZE,$cF,$q,$X5,$WF},$O5);$YF=q#lib/test_case.c::ctors#;$ZF=q#ni:/lib/test_case.c#;$cG={$O5,1};$dG=q#/lib/test_case.c#;$eG={};$fG=[];$gG=q#shift->{'running_test'}#;$hG=bless({$t,$fG,$v,$gG,$x,1042,$y,$z},$A);$iG={$cF,$hG};$jG=q#/lib/test_case.c_test_ro.b#;$kG=bless({$k5,$eG,$g6,$q,$h6,$q,$i6,$iG,$L,$jG},$r6);$lG={};$mG=[];$nG=q#shift->{running_test} = undef#;$oG=bless({$t,$mG,$v,$nG,$x,1044,$y,$z},$A);$pG=q#with_test#;$qG=[];$rG=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$sG=bless({$t,$qG,$v,$rG,$x,1046,$y,$z},$A);$tG={$pG,$sG};$uG=q#/lib/test_case.c_test.b#;$vG=bless({$k5,$lG,$g6,$oG,$h6,$q,$i6,$tG,$L,$uG},$r6);$wG=[$I9,$kG,$vG];$xG=bless({$k5,$cG,$L,$dG,$X5,$wG},$Y5);$yG=q#ni:/lib/test_case.c_test.b#;$zG=q#ni:/lib/test_case.c_test_ro.b#;$AG=q#ni:/lib/test_case_init.b#;$BG=q#ni:/lib/test_case_metrics.b#;$CG=q#ni:/lib/test_case_ro.b#;$DG=q#ni:/lib/test_case_run.b#;$EG=q#ni:/lib/test_case_rw.b#;$FG=q#ni:/lib/test_value#;$GG={$J7,1};$HG=q#/lib/test_value#;$IG={};$JG=[];$KG=q#\\$_[1]#;$LG=bless({$t,$JG,$v,$KG,$x,1048,$y,$z},$A);$MG={$h7,$LG};$NG=q#/lib/test_value_init.b#;$OG=bless({$k5,$IG,$g6,$q,$h6,$q,$i6,$MG,$L,$NG},$r6);$PG={};$QG=q#(==#;$RG=[];$SG=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$TG=bless({$t,$RG,$v,$SG,$x,1050,$y,$z},$A);$UG=q#diff#;$VG=[];$WG=q#my ($self, $rhs) = @_;
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
return undef;#;$XG=bless({$t,$VG,$v,$WG,$x,1052,$y,$z},$A);$YG={$QG,$TG,$UG,$XG};$ZG=q#/lib/test_value_eq.b#;$cH=bless({$k5,$PG,$g6,$q,$h6,$q,$i6,$YG,$L,$ZG},$r6);$dH={};$eH=[];$fH=q#ni::json_encode ${$_[0]}#;$gH=bless({$t,$eH,$v,$fH,$x,1054,$y,$z},$A);$hH={$H8,$gH};$iH=q#/lib/test_value_str.b#;$jH=bless({$k5,$dH,$g6,$q,$h6,$q,$i6,$hH,$L,$iH},$r6);$kH=[$Y7,$OG,$cH,$jH];$lH=bless({$k5,$GG,$L,$HG,$X5,$kH},$P5);$mH=q#lib/test_value.c::ctors#;$nH=q#ni:/lib/test_value.c#;$oH={$P5,1};$pH=q#/lib/test_value.c#;$qH=[$I9];$rH=bless({$k5,$oH,$L,$pH,$X5,$qH},$Y5);$sH=q#ni:/lib/test_value_eq.b#;$tH=q#ni:/lib/test_value_init.b#;$uH=q#ni:/lib/test_value_str.b#;$vH=q#ni:/metaclass#;$wH={$Y5,1};$xH=q#/metaclass#;$yH=[$c7,$p9,$m7,$h9];$zH=bless({$k5,$wH,$L,$xH,$X5,$yH},$Q5);$AH=q#metaclass.c::ctors#;$BH=q#ni:/metaclass.c#;$CH={$Q5,1};$DH=q#/metaclass.c#;$EH=[$y9];$FH=bless({$k5,$CH,$L,$DH,$X5,$EH},$Y5);$GH=q#ni:/module#;$HH=q#ni:/module.c#;$IH=q#ni:/object#;$JH=q#ni:/object.c#;$KH=q#ni:/semantic/dimension#;$LH={$T5,1};$MH=q#/semantic/dimension#;$NH=[$y9];$OH=bless({$k5,$LH,$L,$MH,$X5,$NH},$U5);$PH=q#semantic/dimension.c::ctors#;$QH=q#ni:/semantic/dimension.c#;$RH={$U5,1};$SH=q#/semantic/dimension.c#;$TH=[$R9];$UH=bless({$k5,$RH,$L,$SH,$X5,$TH},$Y5);$VH=q#ni:/semantic/task#;$WH=q#ni:/semantic/task.c#;$XH=q#ni:/semantic/task_outcome.b#;$YH=q#ni:/semantic/task_ro.b#;$ZH=q#ni:main#;$cI={$Yk,1};$dI=[$sD,$mw,$Xk];$eI=bless({$k5,$cI,$L,$Yk,$X5,$dI},$Z5);$fI=q#module::ctors#;$gI=q#ni:ni#;$hI={$nz,1};$iI={$nz,1};$jI=q#json_escapes#;$kI=q##;$lI=q#b#;$mI=q#	#;$nI=q#t#;$oI=q#
#;$pI=q#n#;$qI=q##;$rI=q#"#;$sI=q#/#;$tI=q#\\#;$uI={$kI,$lI,$mI,$nI,$oI,$pI,$qI,$nj,$rI,$rI,$sI,$sI,$tI,$tI};$vI=q#json_unescapes#;$wI={$rI,$rI,$sI,$sI,$tI,$tI,$lI,$kI,$pI,$oI,$nj,$qI,$nI,$mI};$xI={$jI,$uI,$vI,$wI};$yI=q#/lib/json_data.b#;$zI=bless({$k5,$iI,$wn,$xI,$L,$yI},$D7);$AI=[$zI,$mz,$IB];$BI=bless({$k5,$hI,$L,$nz,$X5,$AI},$Z5);$CI={$d,$O,$Q,$V,$W,$o1,$p1,$s1,$t1,$y1,$z1,$L1,$M1,$Y1,$Z1,$n2,$o2,$A2,$B2,$W2,$X2,$e3,$f3,$B3,$C3,$I3,$J3,$e4,$f4,$m4,$n4,$u4,$v4,$F4,$G4,$c5,$d5,$i5,$j5,$y9,$A9,$R9,$S9,$fb,$hb,$lb,$mb,$db,$nb,$ra,$ob,$ka,$pb,$Ha,$qb,$Ab,$Cb,$Gb,$Hb,$yb,$Ib,$Od,$Qd,$ie,$je,$sd,$ke,$Md,$le,$Ce,$Ee,$Ie,$Je,$te,$Ke,$Ae,$Le,$xg,$zg,$Dg,$Eg,$fg,$Fg,$vg,$Gg,$df,$Hg,$Vf,$Ig,$xf,$Jg,$Ue,$Kg,$Wh,$Yh,$ei,$fi,$zh,$gi,$Kh,$hi,$Zg,$ii,$Uh,$ji,$Sg,$ki,$jh,$li,$Fj,$Hj,$Lj,$Mj,$Bi,$Nj,$Yi,$Oj,$Ii,$Pj,$Dj,$Qj,$ti,$Rj,$ij,$Sj,$nk,$pk,$tk,$uk,$lk,$vk,$ck,$wk,$Xk,$Zk,$zl,$Bl,$Fl,$Gl,$kl,$Hl,$xl,$Il,$kd,$Jl,$ge,$Kl,$ee,$Ll,$oc,$Ml,$wc,$Nl,$Ic,$Ol,$Sb,$Pl,$id,$Ql,$Uc,$Rl,$gn,$in,$mn,$nn,$en,$on,$qm,$pn,$Mm,$qn,$gm,$rn,$Cm,$sn,$jo,$lo,$po,$qo,$Rn,$ro,$ho,$so,$Kn,$to,$Dp,$Fp,$Up,$Vp,$Sp,$Wp,$Yq,$cr,$zr,$Ar,$xr,$Br,$xq,$Cr,$Hq,$Dr,$qq,$Er,$Tq,$Fr,$hp,$Gr,$Bp,$Hr,$Zr,$ds,$hs,$is,$Qr,$js,$Xr,$ks,$F8,$ls,$l8,$ms,$N9,$ns,$xs,$zs,$s6,$As,$Es,$Fs,$vs,$Gs,$m7,$Hs,$Xs,$Zs,$ft,$gt,$Vs,$ht,$Ps,$it,$e9,$jt,$v8,$kt,$c9,$lt,$eu,$gu,$ku,$lu,$Ft,$mu,$Ot,$nu,$st,$ou,$yt,$pu,$cu,$qu,$j8,$ru,$sv,$uv,$Jv,$Kv,$Hv,$Lv,$Gu,$Mv,$iv,$Nv,$Xu,$Ov,$qv,$Pv,$Xv,$Yv,$mw,$nw,$By,$Dy,$Hy,$Iy,$vw,$Jy,$lx,$Ky,$W7,$Ly,$p9,$My,$mz,$oz,$M8,$pz,$C6,$qz,$K6,$rz,$R6,$sz,$vA,$xA,$BA,$CA,$tA,$DA,$Yz,$EA,$jA,$FA,$Gz,$GA,$IB,$JB,$c7,$KB,$zy,$LB,$tx,$MB,$jy,$NB,$iC,$kC,$oC,$pC,$VB,$qC,$gC,$rC,$Rx,$sC,$T8,$tC,$Y6,$uC,$VC,$XC,$GC,$YC,$eD,$fD,$MC,$gD,$TC,$hD,$sD,$tD,$w9,$uD,$KD,$MD,$CD,$ND,$RD,$SD,$ID,$TD,$HE,$JE,$RE,$SE,$rE,$TE,$FE,$UE,$jE,$VE,$PE,$WE,$hE,$XE,$XF,$ZF,$xG,$yG,$vG,$zG,$kG,$AG,$AF,$BG,$KF,$CG,$mF,$DG,$VF,$EG,$tF,$FG,$lH,$nH,$rH,$sH,$cH,$tH,$OG,$uH,$jH,$vH,$zH,$BH,$FH,$GH,$h9,$HH,$P9,$IH,$Y7,$JH,$I9,$KH,$OH,$QH,$UH,$VH,$Ro,$WH,$Lp,$XH,$Po,$YH,$Do,$ZH,$eI,$gI,$BI};$DI=q#resolvers#;$EI=[];$FI=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$GI=bless({$t,$EI,$v,$FI,$x,1056,$y,$z},$A);$HI=q#file#;$II=[];$JI=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$KI=bless({$t,$II,$v,$JI,$x,1058,$y,$z},$A);$LI=q#null#;$MI=[];$NI=q#ni('ni:/io/null')->new#;$OI=bless({$t,$MI,$v,$NI,$x,1060,$y,$z},$A);$PI=q#sh#;$QI=[];$RI=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$SI=bless({$t,$QI,$v,$RI,$x,1062,$y,$z},$A);$TI=q#str#;$UI=[];$VI=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$WI=bless({$t,$UI,$v,$VI,$x,1064,$y,$z},$A);$XI={$Df,$GI,$HI,$KI,$LI,$OI,$PI,$SI,$TI,$WI};$YI=bless({$c,$CI,$DI,$XI},$F7);*$BC=\&$zC;*$AC=\&$xC;$s6->apply_unsafe($l5);$s6->apply_unsafe($m5);$s6->apply_unsafe($n5);$s6->apply_unsafe($o5);$s6->apply_unsafe($p5);$s6->apply_unsafe($q5);$s6->apply_unsafe($r5);$s6->apply_unsafe($s5);$s6->apply_unsafe($t5);$s6->apply_unsafe($u5);$s6->apply_unsafe($v5);$s6->apply_unsafe($w5);$s6->apply_unsafe($x5);$s6->apply_unsafe($y5);$s6->apply_unsafe($z5);$s6->apply_unsafe($A5);$s6->apply_unsafe($B5);$s6->apply_unsafe($C5);$s6->apply_unsafe($t6);$s6->apply_unsafe($D5);$s6->apply_unsafe($E5);$s6->apply_unsafe($F5);$s6->apply_unsafe($G5);$s6->apply_unsafe($H5);$s6->apply_unsafe($I5);$s6->apply_unsafe($J5);$s6->apply_unsafe($K5);$s6->apply_unsafe($L5);$s6->apply_unsafe($M5);$s6->apply_unsafe($N5);$s6->apply_unsafe($O5);$s6->apply_unsafe($P5);$s6->apply_unsafe($Y5);$s6->apply_unsafe($Q5);$s6->apply_unsafe($Z5);$s6->apply_unsafe($R5);$s6->apply_unsafe($S5);$s6->apply_unsafe($T5);$s6->apply_unsafe($U5);$s6->apply_unsafe($V5);$C6->apply_unsafe($l5);$C6->apply_unsafe($m5);$C6->apply_unsafe($n5);$C6->apply_unsafe($o5);$C6->apply_unsafe($p5);$C6->apply_unsafe($q5);$C6->apply_unsafe($r5);$C6->apply_unsafe($s5);$C6->apply_unsafe($t5);$C6->apply_unsafe($u5);$C6->apply_unsafe($v5);$C6->apply_unsafe($w5);$C6->apply_unsafe($x5);$C6->apply_unsafe($y5);$C6->apply_unsafe($z5);$C6->apply_unsafe($A5);$C6->apply_unsafe($B5);$C6->apply_unsafe($C5);$C6->apply_unsafe($t6);$C6->apply_unsafe($D5);$C6->apply_unsafe($E5);$C6->apply_unsafe($N);$C6->apply_unsafe($F5);$C6->apply_unsafe($G5);$C6->apply_unsafe($H5);$C6->apply_unsafe($I5);$C6->apply_unsafe($J5);$C6->apply_unsafe($r6);$C6->apply_unsafe($K5);$C6->apply_unsafe($D6);$C6->apply_unsafe($L5);$C6->apply_unsafe($M5);$C6->apply_unsafe($N5);$C6->apply_unsafe($O5);$C6->apply_unsafe($P5);$C6->apply_unsafe($Y5);$C6->apply_unsafe($Q5);$C6->apply_unsafe($Z5);$C6->apply_unsafe($R5);$C6->apply_unsafe($S5);$C6->apply_unsafe($T5);$C6->apply_unsafe($U5);$C6->apply_unsafe($V5);$K6->apply_unsafe($l5);$K6->apply_unsafe($m5);$K6->apply_unsafe($n5);$K6->apply_unsafe($o5);$K6->apply_unsafe($p5);$K6->apply_unsafe($q5);$K6->apply_unsafe($r5);$K6->apply_unsafe($s5);$K6->apply_unsafe($t5);$K6->apply_unsafe($u5);$K6->apply_unsafe($v5);$K6->apply_unsafe($w5);$K6->apply_unsafe($x5);$K6->apply_unsafe($y5);$K6->apply_unsafe($z5);$K6->apply_unsafe($A5);$K6->apply_unsafe($B5);$K6->apply_unsafe($C5);$K6->apply_unsafe($t6);$K6->apply_unsafe($D5);$K6->apply_unsafe($E5);$K6->apply_unsafe($F5);$K6->apply_unsafe($G5);$K6->apply_unsafe($H5);$K6->apply_unsafe($I5);$K6->apply_unsafe($J5);$K6->apply_unsafe($r6);$K6->apply_unsafe($K5);$K6->apply_unsafe($D6);$K6->apply_unsafe($L5);$K6->apply_unsafe($M5);$K6->apply_unsafe($N5);$K6->apply_unsafe($O5);$K6->apply_unsafe($P5);$K6->apply_unsafe($Y5);$K6->apply_unsafe($Q5);$K6->apply_unsafe($Z5);$K6->apply_unsafe($R5);$K6->apply_unsafe($S5);$K6->apply_unsafe($T5);$K6->apply_unsafe($U5);$K6->apply_unsafe($V5);$R6->apply_unsafe($l5);$R6->apply_unsafe($m5);$R6->apply_unsafe($n5);$R6->apply_unsafe($o5);$R6->apply_unsafe($p5);$R6->apply_unsafe($q5);$R6->apply_unsafe($r5);$R6->apply_unsafe($s5);$R6->apply_unsafe($t5);$R6->apply_unsafe($u5);$R6->apply_unsafe($v5);$R6->apply_unsafe($w5);$R6->apply_unsafe($x5);$R6->apply_unsafe($y5);$R6->apply_unsafe($z5);$R6->apply_unsafe($A5);$R6->apply_unsafe($B5);$R6->apply_unsafe($C5);$R6->apply_unsafe($t6);$R6->apply_unsafe($D5);$R6->apply_unsafe($E5);$R6->apply_unsafe($F5);$R6->apply_unsafe($G5);$R6->apply_unsafe($H5);$R6->apply_unsafe($I5);$R6->apply_unsafe($J5);$R6->apply_unsafe($r6);$R6->apply_unsafe($K5);$R6->apply_unsafe($D6);$R6->apply_unsafe($L5);$R6->apply_unsafe($M5);$R6->apply_unsafe($N5);$R6->apply_unsafe($O5);$R6->apply_unsafe($P5);$R6->apply_unsafe($Y5);$R6->apply_unsafe($Q5);$R6->apply_unsafe($Z5);$R6->apply_unsafe($R5);$R6->apply_unsafe($S5);$R6->apply_unsafe($T5);$R6->apply_unsafe($U5);$R6->apply_unsafe($V5);$Y6->apply_unsafe($l5);$Y6->apply_unsafe($m5);$Y6->apply_unsafe($n5);$Y6->apply_unsafe($o5);$Y6->apply_unsafe($p5);$Y6->apply_unsafe($q5);$Y6->apply_unsafe($r5);$Y6->apply_unsafe($s5);$Y6->apply_unsafe($t5);$Y6->apply_unsafe($u5);$Y6->apply_unsafe($v5);$Y6->apply_unsafe($w5);$Y6->apply_unsafe($x5);$Y6->apply_unsafe($y5);$Y6->apply_unsafe($z5);$Y6->apply_unsafe($A5);$Y6->apply_unsafe($B5);$Y6->apply_unsafe($C5);$Y6->apply_unsafe($t6);$Y6->apply_unsafe($D5);$Y6->apply_unsafe($E5);$Y6->apply_unsafe($F5);$Y6->apply_unsafe($G5);$Y6->apply_unsafe($H5);$Y6->apply_unsafe($I5);$Y6->apply_unsafe($J5);$Y6->apply_unsafe($K5);$Y6->apply_unsafe($D6);$Y6->apply_unsafe($L5);$Y6->apply_unsafe($M5);$Y6->apply_unsafe($N5);$Y6->apply_unsafe($O5);$Y6->apply_unsafe($P5);$Y6->apply_unsafe($Y5);$Y6->apply_unsafe($Q5);$Y6->apply_unsafe($Z5);$Y6->apply_unsafe($R5);$Y6->apply_unsafe($S5);$Y6->apply_unsafe($T5);$Y6->apply_unsafe($U5);$Y6->apply_unsafe($V5);$m7->apply_unsafe($l5);$m7->apply_unsafe($m5);$m7->apply_unsafe($n5);$m7->apply_unsafe($o5);$m7->apply_unsafe($p5);$m7->apply_unsafe($q5);$m7->apply_unsafe($r5);$m7->apply_unsafe($s5);$m7->apply_unsafe($t5);$m7->apply_unsafe($u5);$m7->apply_unsafe($v5);$m7->apply_unsafe($w5);$m7->apply_unsafe($x5);$m7->apply_unsafe($y5);$m7->apply_unsafe($z5);$m7->apply_unsafe($A5);$m7->apply_unsafe($B5);$m7->apply_unsafe($C5);$m7->apply_unsafe($D5);$m7->apply_unsafe($E5);$m7->apply_unsafe($F5);$m7->apply_unsafe($G5);$m7->apply_unsafe($H5);$m7->apply_unsafe($I5);$m7->apply_unsafe($J5);$m7->apply_unsafe($K5);$m7->apply_unsafe($L5);$m7->apply_unsafe($M5);$m7->apply_unsafe($N5);$m7->apply_unsafe($O5);$m7->apply_unsafe($P5);$m7->apply_unsafe($Y5);$m7->apply_unsafe($Q5);$m7->apply_unsafe($Z5);$m7->apply_unsafe($R5);$m7->apply_unsafe($S5);$m7->apply_unsafe($T5);$m7->apply_unsafe($U5);$m7->apply_unsafe($V5);$W7->apply_unsafe($l5);$W7->apply_unsafe($m5);$W7->apply_unsafe($n7);$W7->apply_unsafe($n5);$W7->apply_unsafe($o7);$W7->apply_unsafe($o5);$W7->apply_unsafe($p7);$W7->apply_unsafe($p5);$W7->apply_unsafe($q7);$W7->apply_unsafe($q5);$W7->apply_unsafe($r7);$W7->apply_unsafe($r5);$W7->apply_unsafe($s7);$W7->apply_unsafe($s5);$W7->apply_unsafe($t7);$W7->apply_unsafe($t5);$W7->apply_unsafe($u7);$W7->apply_unsafe($u5);$W7->apply_unsafe($v7);$W7->apply_unsafe($v5);$W7->apply_unsafe($w7);$W7->apply_unsafe($w5);$W7->apply_unsafe($x7);$W7->apply_unsafe($x5);$W7->apply_unsafe($y7);$W7->apply_unsafe($y5);$W7->apply_unsafe($z7);$W7->apply_unsafe($z5);$W7->apply_unsafe($A7);$W7->apply_unsafe($A5);$W7->apply_unsafe($B7);$W7->apply_unsafe($B5);$W7->apply_unsafe($C7);$W7->apply_unsafe($C5);$W7->apply_unsafe($t6);$W7->apply_unsafe($D5);$W7->apply_unsafe($D7);$W7->apply_unsafe($E5);$W7->apply_unsafe($N);$W7->apply_unsafe($F5);$W7->apply_unsafe($A);$W7->apply_unsafe($G5);$W7->apply_unsafe($E7);$W7->apply_unsafe($H5);$W7->apply_unsafe($F7);$W7->apply_unsafe($I5);$W7->apply_unsafe($G7);$W7->apply_unsafe($J5);$W7->apply_unsafe($r6);$W7->apply_unsafe($K5);$W7->apply_unsafe($D6);$W7->apply_unsafe($L5);$W7->apply_unsafe($H7);$W7->apply_unsafe($M5);$W7->apply_unsafe($I7);$W7->apply_unsafe($N5);$W7->apply_unsafe($D);$W7->apply_unsafe($O5);$W7->apply_unsafe($J7);$W7->apply_unsafe($P5);$W7->apply_unsafe($Y5);$W7->apply_unsafe($Q5);$W7->apply_unsafe($Z5);$W7->apply_unsafe($R5);$W7->apply_unsafe($K7);$W7->apply_unsafe($S5);$W7->apply_unsafe($T5);$W7->apply_unsafe($U5);$W7->apply_unsafe($L7);$W7->apply_unsafe($V5);$j8->apply_unsafe($l5);$j8->apply_unsafe($m5);$j8->apply_unsafe($n5);$j8->apply_unsafe($o5);$j8->apply_unsafe($p5);$j8->apply_unsafe($q5);$j8->apply_unsafe($r5);$j8->apply_unsafe($s5);$j8->apply_unsafe($t5);$j8->apply_unsafe($u5);$j8->apply_unsafe($v5);$j8->apply_unsafe($w5);$j8->apply_unsafe($x5);$j8->apply_unsafe($y5);$j8->apply_unsafe($z5);$j8->apply_unsafe($A5);$j8->apply_unsafe($B5);$j8->apply_unsafe($C7);$j8->apply_unsafe($C5);$j8->apply_unsafe($t6);$j8->apply_unsafe($D5);$j8->apply_unsafe($D7);$j8->apply_unsafe($E5);$j8->apply_unsafe($F5);$j8->apply_unsafe($G5);$j8->apply_unsafe($H5);$j8->apply_unsafe($I5);$j8->apply_unsafe($J5);$j8->apply_unsafe($r6);$j8->apply_unsafe($K5);$j8->apply_unsafe($D6);$j8->apply_unsafe($L5);$j8->apply_unsafe($M5);$j8->apply_unsafe($N5);$j8->apply_unsafe($O5);$j8->apply_unsafe($P5);$j8->apply_unsafe($Y5);$j8->apply_unsafe($Q5);$j8->apply_unsafe($Z5);$j8->apply_unsafe($R5);$j8->apply_unsafe($S5);$j8->apply_unsafe($T5);$j8->apply_unsafe($U5);$j8->apply_unsafe($V5);$v8->apply_unsafe($l5);$v8->apply_unsafe($m5);$v8->apply_unsafe($n5);$v8->apply_unsafe($o5);$v8->apply_unsafe($p5);$v8->apply_unsafe($q5);$v8->apply_unsafe($r5);$v8->apply_unsafe($s5);$v8->apply_unsafe($t5);$v8->apply_unsafe($u5);$v8->apply_unsafe($v5);$v8->apply_unsafe($w5);$v8->apply_unsafe($x5);$v8->apply_unsafe($y5);$v8->apply_unsafe($z5);$v8->apply_unsafe($A5);$v8->apply_unsafe($B5);$v8->apply_unsafe($C5);$v8->apply_unsafe($t6);$v8->apply_unsafe($D5);$v8->apply_unsafe($E5);$v8->apply_unsafe($F5);$v8->apply_unsafe($G5);$v8->apply_unsafe($H5);$v8->apply_unsafe($I5);$v8->apply_unsafe($J5);$v8->apply_unsafe($K5);$v8->apply_unsafe($L5);$v8->apply_unsafe($M5);$v8->apply_unsafe($N5);$v8->apply_unsafe($O5);$v8->apply_unsafe($P5);$v8->apply_unsafe($Y5);$v8->apply_unsafe($Q5);$v8->apply_unsafe($Z5);$v8->apply_unsafe($R5);$v8->apply_unsafe($S5);$v8->apply_unsafe($T5);$v8->apply_unsafe($U5);$v8->apply_unsafe($V5);$F8->apply_unsafe($l5);$F8->apply_unsafe($m5);$F8->apply_unsafe($n5);$F8->apply_unsafe($o5);$F8->apply_unsafe($p5);$F8->apply_unsafe($q5);$F8->apply_unsafe($r5);$F8->apply_unsafe($s5);$F8->apply_unsafe($t5);$F8->apply_unsafe($u5);$F8->apply_unsafe($v5);$F8->apply_unsafe($w5);$F8->apply_unsafe($x5);$F8->apply_unsafe($y5);$F8->apply_unsafe($z5);$F8->apply_unsafe($A5);$F8->apply_unsafe($B5);$F8->apply_unsafe($C5);$F8->apply_unsafe($t6);$F8->apply_unsafe($D5);$F8->apply_unsafe($E5);$F8->apply_unsafe($F5);$F8->apply_unsafe($G5);$F8->apply_unsafe($H5);$F8->apply_unsafe($I5);$F8->apply_unsafe($J5);$F8->apply_unsafe($K5);$F8->apply_unsafe($L5);$F8->apply_unsafe($M5);$F8->apply_unsafe($N5);$F8->apply_unsafe($O5);$F8->apply_unsafe($P5);$F8->apply_unsafe($Y5);$F8->apply_unsafe($Q5);$F8->apply_unsafe($Z5);$F8->apply_unsafe($R5);$F8->apply_unsafe($S5);$F8->apply_unsafe($T5);$F8->apply_unsafe($U5);$F8->apply_unsafe($V5);$M8->apply_unsafe($l5);$M8->apply_unsafe($m5);$M8->apply_unsafe($n5);$M8->apply_unsafe($o5);$M8->apply_unsafe($p5);$M8->apply_unsafe($q5);$M8->apply_unsafe($r5);$M8->apply_unsafe($s5);$M8->apply_unsafe($t5);$M8->apply_unsafe($u5);$M8->apply_unsafe($v5);$M8->apply_unsafe($w5);$M8->apply_unsafe($x5);$M8->apply_unsafe($y5);$M8->apply_unsafe($z5);$M8->apply_unsafe($A5);$M8->apply_unsafe($B5);$M8->apply_unsafe($C5);$M8->apply_unsafe($t6);$M8->apply_unsafe($D5);$M8->apply_unsafe($E5);$M8->apply_unsafe($F5);$M8->apply_unsafe($G5);$M8->apply_unsafe($H5);$M8->apply_unsafe($I5);$M8->apply_unsafe($J5);$M8->apply_unsafe($K5);$M8->apply_unsafe($L5);$M8->apply_unsafe($M5);$M8->apply_unsafe($N5);$M8->apply_unsafe($O5);$M8->apply_unsafe($P5);$M8->apply_unsafe($Y5);$M8->apply_unsafe($Q5);$M8->apply_unsafe($Z5);$M8->apply_unsafe($R5);$M8->apply_unsafe($S5);$M8->apply_unsafe($T5);$M8->apply_unsafe($U5);$M8->apply_unsafe($V5);$T8->apply_unsafe($l5);$T8->apply_unsafe($m5);$T8->apply_unsafe($n5);$T8->apply_unsafe($o5);$T8->apply_unsafe($p5);$T8->apply_unsafe($q5);$T8->apply_unsafe($r5);$T8->apply_unsafe($s5);$T8->apply_unsafe($t5);$T8->apply_unsafe($u5);$T8->apply_unsafe($v5);$T8->apply_unsafe($w5);$T8->apply_unsafe($x5);$T8->apply_unsafe($y5);$T8->apply_unsafe($z5);$T8->apply_unsafe($A5);$T8->apply_unsafe($B5);$T8->apply_unsafe($C5);$T8->apply_unsafe($t6);$T8->apply_unsafe($D5);$T8->apply_unsafe($E5);$T8->apply_unsafe($F5);$T8->apply_unsafe($G5);$T8->apply_unsafe($H5);$T8->apply_unsafe($I5);$T8->apply_unsafe($J5);$T8->apply_unsafe($K5);$T8->apply_unsafe($L5);$T8->apply_unsafe($M5);$T8->apply_unsafe($N5);$T8->apply_unsafe($O5);$T8->apply_unsafe($P5);$T8->apply_unsafe($Y5);$T8->apply_unsafe($Q5);$T8->apply_unsafe($Z5);$T8->apply_unsafe($R5);$T8->apply_unsafe($S5);$T8->apply_unsafe($T5);$T8->apply_unsafe($U5);$T8->apply_unsafe($V5);$c9->apply_unsafe($l5);$c9->apply_unsafe($m5);$c9->apply_unsafe($n5);$c9->apply_unsafe($o5);$c9->apply_unsafe($p5);$c9->apply_unsafe($q5);$c9->apply_unsafe($r5);$c9->apply_unsafe($s5);$c9->apply_unsafe($t5);$c9->apply_unsafe($u5);$c9->apply_unsafe($v5);$c9->apply_unsafe($w5);$c9->apply_unsafe($x5);$c9->apply_unsafe($y5);$c9->apply_unsafe($z5);$c9->apply_unsafe($A5);$c9->apply_unsafe($B5);$c9->apply_unsafe($C5);$c9->apply_unsafe($t6);$c9->apply_unsafe($D5);$c9->apply_unsafe($E5);$c9->apply_unsafe($F5);$c9->apply_unsafe($G5);$c9->apply_unsafe($H5);$c9->apply_unsafe($I5);$c9->apply_unsafe($J5);$c9->apply_unsafe($K5);$c9->apply_unsafe($L5);$c9->apply_unsafe($M5);$c9->apply_unsafe($N5);$c9->apply_unsafe($O5);$c9->apply_unsafe($P5);$c9->apply_unsafe($Y5);$c9->apply_unsafe($Q5);$c9->apply_unsafe($Z5);$c9->apply_unsafe($R5);$c9->apply_unsafe($S5);$c9->apply_unsafe($T5);$c9->apply_unsafe($U5);$c9->apply_unsafe($V5);$p9->apply_unsafe($l5);$p9->apply_unsafe($m5);$p9->apply_unsafe($n5);$p9->apply_unsafe($o5);$p9->apply_unsafe($p5);$p9->apply_unsafe($q5);$p9->apply_unsafe($r5);$p9->apply_unsafe($s5);$p9->apply_unsafe($t5);$p9->apply_unsafe($u5);$p9->apply_unsafe($v5);$p9->apply_unsafe($w5);$p9->apply_unsafe($x5);$p9->apply_unsafe($y5);$p9->apply_unsafe($z5);$p9->apply_unsafe($A5);$p9->apply_unsafe($B5);$p9->apply_unsafe($C5);$p9->apply_unsafe($D5);$p9->apply_unsafe($E5);$p9->apply_unsafe($F5);$p9->apply_unsafe($A);$p9->apply_unsafe($G5);$p9->apply_unsafe($H5);$p9->apply_unsafe($I5);$p9->apply_unsafe($J5);$p9->apply_unsafe($r6);$p9->apply_unsafe($K5);$p9->apply_unsafe($D6);$p9->apply_unsafe($L5);$p9->apply_unsafe($M5);$p9->apply_unsafe($N5);$p9->apply_unsafe($O5);$p9->apply_unsafe($P5);$p9->apply_unsafe($Y5);$p9->apply_unsafe($Q5);$p9->apply_unsafe($R5);$p9->apply_unsafe($S5);$p9->apply_unsafe($T5);$p9->apply_unsafe($U5);$p9->apply_unsafe($V5);$w9->apply_unsafe($l5);$w9->apply_unsafe($m5);$w9->apply_unsafe($n5);$w9->apply_unsafe($o5);$w9->apply_unsafe($p5);$w9->apply_unsafe($q5);$w9->apply_unsafe($r5);$w9->apply_unsafe($s5);$w9->apply_unsafe($t5);$w9->apply_unsafe($u5);$w9->apply_unsafe($v5);$w9->apply_unsafe($w5);$w9->apply_unsafe($x5);$w9->apply_unsafe($y5);$w9->apply_unsafe($z5);$w9->apply_unsafe($A5);$w9->apply_unsafe($B5);$w9->apply_unsafe($C5);$w9->apply_unsafe($D5);$w9->apply_unsafe($E5);$w9->apply_unsafe($F5);$w9->apply_unsafe($G5);$w9->apply_unsafe($H5);$w9->apply_unsafe($I5);$w9->apply_unsafe($J5);$w9->apply_unsafe($K5);$w9->apply_unsafe($L5);$w9->apply_unsafe($M5);$w9->apply_unsafe($N5);$w9->apply_unsafe($O5);$w9->apply_unsafe($P5);$w9->apply_unsafe($Q5);$w9->apply_unsafe($R5);$w9->apply_unsafe($S5);$w9->apply_unsafe($T5);$w9->apply_unsafe($U5);$w9->apply_unsafe($V5);$ka->apply_unsafe($n7);$ra->apply_unsafe($n7);$Ha->apply_unsafe($n7);$db->apply_unsafe($n7);$yb->apply_unsafe($o7);$Sb->apply_unsafe($p7);$Sb->apply_unsafe($q7);$Sb->apply_unsafe($r7);$Sb->apply_unsafe($s7);$Sb->apply_unsafe($t7);$Sb->apply_unsafe($u7);$Sb->apply_unsafe($v7);$Sb->apply_unsafe($w7);$Sb->apply_unsafe($x7);$Sb->apply_unsafe($y7);$oc->apply_unsafe($p7);$oc->apply_unsafe($q7);$oc->apply_unsafe($r7);$oc->apply_unsafe($s7);$oc->apply_unsafe($t7);$oc->apply_unsafe($u7);$oc->apply_unsafe($v7);$oc->apply_unsafe($w7);$oc->apply_unsafe($x7);$oc->apply_unsafe($y7);$wc->apply_unsafe($p7);$wc->apply_unsafe($q7);$wc->apply_unsafe($r7);$wc->apply_unsafe($s7);$wc->apply_unsafe($t7);$wc->apply_unsafe($u7);$wc->apply_unsafe($v7);$wc->apply_unsafe($w7);$wc->apply_unsafe($x7);$wc->apply_unsafe($y7);$Ic->apply_unsafe($p7);$Ic->apply_unsafe($q7);$Ic->apply_unsafe($r7);$Ic->apply_unsafe($s7);$Ic->apply_unsafe($t7);$Ic->apply_unsafe($u7);$Ic->apply_unsafe($v7);$Ic->apply_unsafe($w7);$Ic->apply_unsafe($x7);$Ic->apply_unsafe($y7);$Uc->apply_unsafe($p7);$Uc->apply_unsafe($q7);$Uc->apply_unsafe($r7);$Uc->apply_unsafe($s7);$Uc->apply_unsafe($t7);$Uc->apply_unsafe($u7);$Uc->apply_unsafe($v7);$Uc->apply_unsafe($w7);$Uc->apply_unsafe($x7);$Uc->apply_unsafe($y7);$id->apply_unsafe($p7);$id->apply_unsafe($q7);$id->apply_unsafe($r7);$id->apply_unsafe($s7);$id->apply_unsafe($t7);$id->apply_unsafe($u7);$id->apply_unsafe($v7);$id->apply_unsafe($w7);$id->apply_unsafe($x7);$id->apply_unsafe($y7);$sd->apply_unsafe($p7);$Md->apply_unsafe($p7);$ee->apply_unsafe($p5);$ee->apply_unsafe($q5);$ee->apply_unsafe($r5);$ee->apply_unsafe($s5);$ee->apply_unsafe($t5);$ee->apply_unsafe($u5);$ee->apply_unsafe($v5);$ee->apply_unsafe($w5);$ee->apply_unsafe($x5);$ee->apply_unsafe($y5);$te->apply_unsafe($q7);$Ae->apply_unsafe($q7);$Ue->apply_unsafe($r7);$df->apply_unsafe($r7);$xf->apply_unsafe($r7);$Vf->apply_unsafe($r7);$fg->apply_unsafe($r7);$vg->apply_unsafe($r7);$Sg->apply_unsafe($s7);$Sg->apply_unsafe($u7);$Zg->apply_unsafe($s7);$jh->apply_unsafe($s7);$zh->apply_unsafe($s7);$zh->apply_unsafe($u7);$Kh->apply_unsafe($s7);$Uh->apply_unsafe($s7);$Uh->apply_unsafe($u7);$ti->apply_unsafe($t7);$Bi->apply_unsafe($t7);$Ii->apply_unsafe($t7);$Yi->apply_unsafe($t7);$ij->apply_unsafe($t7);$Dj->apply_unsafe($t7);$ck->apply_unsafe($u7);$lk->apply_unsafe($u7);$Xk->apply_unsafe($Yk);$kl->apply_unsafe($v7);$xl->apply_unsafe($v7);$gm->apply_unsafe($x7);$qm->apply_unsafe($x7);$Cm->apply_unsafe($x7);$Mm->apply_unsafe($x7);$en->apply_unsafe($x7);$Kn->apply_unsafe($y7);$Rn->apply_unsafe($y7);$ho->apply_unsafe($y7);$Do->apply_unsafe($z7);$Do->apply_unsafe($A7);$Do->apply_unsafe($B7);$Do->apply_unsafe($L7);$Po->apply_unsafe($z7);$Po->apply_unsafe($A7);$Po->apply_unsafe($B7);$Po->apply_unsafe($L7);$hp->apply_unsafe($z7);$hp->apply_unsafe($A7);$hp->apply_unsafe($B7);$Bp->apply_unsafe($z7);$Bp->apply_unsafe($A7);$Bp->apply_unsafe($B7);$Sp->apply_unsafe($z5);$Sp->apply_unsafe($A5);$Sp->apply_unsafe($B5);$qq->apply_unsafe($A7);$xq->apply_unsafe($A7);$Hq->apply_unsafe($A7);$Tq->apply_unsafe($A7);$xr->apply_unsafe($A5);$Qr->apply_unsafe($B7);$Xr->apply_unsafe($B7);$vs->apply_unsafe($t6);$Ps->apply_unsafe($D7);$Vs->apply_unsafe($D7);$st->apply_unsafe($N);$yt->apply_unsafe($N);$Ft->apply_unsafe($N);$Ot->apply_unsafe($N);$cu->apply_unsafe($N);$Gu->apply_unsafe($A);$Xu->apply_unsafe($A);$iv->apply_unsafe($A);$qv->apply_unsafe($A);$Hv->apply_unsafe($G5);$Xv->apply_unsafe($E7);$mw->apply_unsafe($Yk);$vw->apply_unsafe($E7);$lx->apply_unsafe($E7);$tx->apply_unsafe($E7);$tx->apply_unsafe($G7);$Rx->apply_unsafe($E7);$Rx->apply_unsafe($G7);$jy->apply_unsafe($E7);$jy->apply_unsafe($G7);$zy->apply_unsafe($E7);$mz->apply_unsafe($nz);$Gz->apply_unsafe($F7);$Yz->apply_unsafe($F7);$jA->apply_unsafe($F7);$tA->apply_unsafe($F7);$IB->apply_unsafe($nz);$VB->apply_unsafe($G7);$gC->apply_unsafe($G7);$GC->apply_unsafe($r6);$MC->apply_unsafe($r6);$TC->apply_unsafe($r6);$sD->apply_unsafe($Yk);$CD->apply_unsafe($D6);$ID->apply_unsafe($D6);$hE->apply_unsafe($H7);$hE->apply_unsafe($I7);$rE->apply_unsafe($H7);$FE->apply_unsafe($H7);$mF->apply_unsafe($D);$tF->apply_unsafe($D);$AF->apply_unsafe($D);$KF->apply_unsafe($D);$VF->apply_unsafe($D);$kG->apply_unsafe($O5);$vG->apply_unsafe($O5);$OG->apply_unsafe($J7);$cH->apply_unsafe($J7);$jH->apply_unsafe($J7);$ni::self=$YI;&$_($B)for@$C;&$_($O)for@$P;&$_($V)for@$P;&$_($d1)for@$C;&$_($j1)for@$C;&$_($o1)for@$P;&$_($s1)for@$P;&$_($y1)for@$P;&$_($G1)for@$C;&$_($L1)for@$P;&$_($T1)for@$C;&$_($Y1)for@$P;&$_($i2)for@$C;&$_($n2)for@$P;&$_($v2)for@$C;&$_($A2)for@$P;&$_($L2)for@$C;&$_($R2)for@$C;&$_($W2)for@$P;&$_($e3)for@$P;&$_($k3)for@$C;&$_($q3)for@$C;&$_($w3)for@$C;&$_($B3)for@$P;&$_($I3)for@$P;&$_($R3)for@$C;&$_($X3)for@$C;&$_($e4)for@$P;&$_($m4)for@$P;&$_($u4)for@$P;&$_($A4)for@$C;&$_($F4)for@$P;&$_($O4)for@$C;&$_($V4)for@$C;&$_($c5)for@$P;&$_($i5)for@$P;&$_($l6)for@$C;&$_($o6)for@$C;&$_($s6)for@$u6;&$_($x6)for@$C;&$_($z6)for@$C;&$_($C6)for@$u6;&$_($H6)for@$C;&$_($K6)for@$u6;&$_($O6)for@$C;&$_($R6)for@$u6;&$_($V6)for@$C;&$_($Y6)for@$u6;&$_($c7)for@$d7;&$_($g7)for@$C;&$_($j7)for@$C;&$_($m7)for@$u6;&$_($R7)for@$C;&$_($T7)for@$C;&$_($W7)for@$u6;&$_($Y7)for@$Z7;&$_($g8)for@$C;&$_($j8)for@$u6;&$_($l8)for@$m8;&$_($s8)for@$C;&$_($v8)for@$u6;&$_($z8)for@$C;&$_($C8)for@$C;&$_($F8)for@$u6;&$_($J8)for@$C;&$_($M8)for@$u6;&$_($Q8)for@$C;&$_($T8)for@$u6;&$_($X8)for@$C;&$_($c9)for@$u6;&$_($e9)for@$f9;&$_($h9)for@$i9;&$_($m9)for@$C;&$_($p9)for@$u6;&$_($t9)for@$C;&$_($w9)for@$u6;&$_($y9)for@$z9;&$_($I9)for@$J9;&$_($N9)for@$J9;&$_($P9)for@$J9;&$_($R9)for@$J9;&$_($X9)for@$C;&$_($da)for@$C;&$_($ha)for@$C;&$_($ka)for@$u6;&$_($oa)for@$C;&$_($ra)for@$u6;&$_($wa)for@$C;&$_($Aa)for@$C;&$_($Ea)for@$C;&$_($Ha)for@$u6;&$_($Ma)for@$C;&$_($Qa)for@$C;&$_($Ua)for@$C;&$_($Ya)for@$C;&$_($db)for@$u6;&$_($fb)for@$gb;&$_($lb)for@$J9;&$_($vb)for@$C;&$_($yb)for@$u6;&$_($Ab)for@$Bb;&$_($Gb)for@$J9;&$_($Pb)for@$C;&$_($Sb)for@$u6;&$_($Xb)for@$C;&$_($dc)for@$C;&$_($hc)for@$C;&$_($lc)for@$C;&$_($oc)for@$u6;&$_($tc)for@$C;&$_($wc)for@$u6;&$_($Bc)for@$C;&$_($Fc)for@$C;&$_($Ic)for@$u6;&$_($Nc)for@$C;&$_($Rc)for@$C;&$_($Uc)for@$u6;&$_($Zc)for@$C;&$_($fd)for@$C;&$_($id)for@$u6;&$_($kd)for@$ld;&$_($pd)for@$C;&$_($sd)for@$u6;&$_($xd)for@$C;&$_($Bd)for@$C;&$_($Fd)for@$C;&$_($Jd)for@$C;&$_($Md)for@$u6;&$_($Od)for@$Pd;&$_($Zd)for@$C;&$_($ee)for@$u6;&$_($ge)for@$J9;&$_($ie)for@$J9;&$_($qe)for@$C;&$_($te)for@$u6;&$_($xe)for@$C;&$_($Ae)for@$u6;&$_($Ce)for@$De;&$_($Ie)for@$J9;&$_($Re)for@$C;&$_($Ue)for@$u6;&$_($Ye)for@$C;&$_($df)for@$u6;&$_($if)for@$C;&$_($mf)for@$C;&$_($qf)for@$C;&$_($uf)for@$C;&$_($xf)for@$u6;&$_($Cf)for@$C;&$_($Gf)for@$C;&$_($Kf)for@$C;&$_($Of)for@$C;&$_($Sf)for@$C;&$_($Vf)for@$u6;&$_($cg)for@$C;&$_($fg)for@$u6;&$_($kg)for@$C;&$_($og)for@$C;&$_($sg)for@$C;&$_($vg)for@$u6;&$_($xg)for@$yg;&$_($Dg)for@$J9;&$_($Pg)for@$C;&$_($Sg)for@$u6;&$_($Wg)for@$C;&$_($Zg)for@$u6;&$_($gh)for@$C;&$_($jh)for@$u6;&$_($oh)for@$C;&$_($sh)for@$C;&$_($wh)for@$C;&$_($zh)for@$u6;&$_($Dh)for@$C;&$_($Hh)for@$C;&$_($Kh)for@$u6;&$_($Oh)for@$C;&$_($Rh)for@$C;&$_($Uh)for@$u6;&$_($Wh)for@$Xh;&$_($ei)for@$J9;&$_($qi)for@$C;&$_($ti)for@$u6;&$_($yi)for@$C;&$_($Bi)for@$u6;&$_($Fi)for@$C;&$_($Ii)for@$u6;&$_($Ni)for@$C;&$_($Ri)for@$C;&$_($Vi)for@$C;&$_($Yi)for@$u6;&$_($fj)for@$C;&$_($ij)for@$u6;&$_($mj)for@$C;&$_($qj)for@$C;&$_($tj)for@$C;&$_($xj)for@$C;&$_($Aj)for@$C;&$_($Dj)for@$u6;&$_($Fj)for@$Gj;&$_($Lj)for@$J9;&$_($Xj)for@$C;&$_($ck)for@$u6;&$_($fk)for@$C;&$_($ik)for@$C;&$_($lk)for@$u6;&$_($nk)for@$ok;&$_($tk)for@$J9;&$_($Bk)for@$C;&$_($Ek)for@$C;&$_($Ik)for@$C;&$_($Mk)for@$C;&$_($Qk)for@$C;&$_($Uk)for@$C;&$_($Xk)for@$u6;&$_($hl)for@$C;&$_($kl)for@$u6;&$_($ol)for@$C;&$_($rl)for@$C;&$_($ul)for@$C;&$_($xl)for@$u6;&$_($zl)for@$Al;&$_($Fl)for@$J9;&$_($Xl)for@$C;&$_($dm)for@$C;&$_($gm)for@$u6;&$_($km)for@$C;&$_($nm)for@$C;&$_($qm)for@$u6;&$_($vm)for@$C;&$_($zm)for@$C;&$_($Cm)for@$u6;&$_($Gm)for@$C;&$_($Jm)for@$C;&$_($Mm)for@$u6;&$_($Qm)for@$C;&$_($Tm)for@$C;&$_($Wm)for@$C;&$_($Zm)for@$C;&$_($en)for@$u6;&$_($gn)for@$hn;&$_($mn)for@$J9;&$_($zn)for@$C;&$_($Dn)for@$C;&$_($Hn)for@$C;&$_($Kn)for@$u6;&$_($On)for@$C;&$_($Rn)for@$u6;&$_($Vn)for@$C;&$_($Zn)for@$C;&$_($eo)for@$C;&$_($ho)for@$u6;&$_($jo)for@$ko;&$_($po)for@$J9;&$_($Ao)for@$C;&$_($Do)for@$u6;&$_($Io)for@$C;&$_($Mo)for@$C;&$_($Po)for@$u6;&$_($Ro)for@$So;&$_($Wo)for@$C;&$_($Zo)for@$C;&$_($ep)for@$C;&$_($hp)for@$u6;&$_($mp)for@$C;&$_($qp)for@$C;&$_($up)for@$C;&$_($yp)for@$C;&$_($Bp)for@$u6;&$_($Dp)for@$Ep;&$_($Lp)for@$J9;&$_($Pp)for@$C;&$_($Sp)for@$u6;&$_($Up)for@$J9;&$_($fq)for@$C;&$_($jq)for@$C;&$_($nq)for@$C;&$_($qq)for@$u6;&$_($uq)for@$C;&$_($xq)for@$u6;&$_($Bq)for@$C;&$_($Eq)for@$C;&$_($Hq)for@$u6;&$_($Mq)for@$C;&$_($Qq)for@$C;&$_($Tq)for@$u6;&$_($Yq)for@$Zq;&$_($ir)for@$C;&$_($mr)for@$C;&$_($qr)for@$C;&$_($ur)for@$C;&$_($xr)for@$u6;&$_($zr)for@$J9;&$_($Nr)for@$C;&$_($Qr)for@$u6;&$_($Ur)for@$C;&$_($Xr)for@$u6;&$_($Zr)for@$cs;&$_($hs)for@$J9;&$_($ss)for@$C;&$_($vs)for@$u6;&$_($xs)for@$ys;&$_($Es)for@$J9;&$_($Ms)for@$C;&$_($Ps)for@$u6;&$_($Ss)for@$C;&$_($Vs)for@$u6;&$_($Xs)for@$Ys;&$_($ft)for@$J9;&$_($pt)for@$C;&$_($st)for@$u6;&$_($vt)for@$C;&$_($yt)for@$u6;&$_($Ct)for@$C;&$_($Ft)for@$u6;&$_($It)for@$C;&$_($Lt)for@$C;&$_($Ot)for@$u6;&$_($Rt)for@$C;&$_($Ut)for@$C;&$_($Xt)for@$C;&$_($cu)for@$u6;&$_($eu)for@$fu;&$_($ku)for@$J9;&$_($wu)for@$C;&$_($yu)for@$C;&$_($Bu)for@$C;&$_($Du)for@$C;&$_($Gu)for@$u6;&$_($Ku)for@$C;&$_($Nu)for@$C;&$_($Qu)for@$C;&$_($Uu)for@$C;&$_($Xu)for@$u6;&$_($dv)for@$C;&$_($fv)for@$C;&$_($iv)for@$u6;&$_($nv)for@$C;&$_($qv)for@$u6;&$_($sv)for@$tv;&$_($Av)for@$C;&$_($Ev)for@$C;&$_($Hv)for@$u6;&$_($Jv)for@$J9;&$_($Uv)for@$C;&$_($Xv)for@$u6;&$_($fw)for@$C;&$_($jw)for@$C;&$_($mw)for@$u6;&$_($sw)for@$C;&$_($vw)for@$u6;&$_($Aw)for@$C;&$_($Ew)for@$C;&$_($Iw)for@$C;&$_($Mw)for@$C;&$_($Qw)for@$C;&$_($Uw)for@$C;&$_($Yw)for@$C;&$_($ex)for@$C;&$_($ix)for@$C;&$_($lx)for@$u6;&$_($qx)for@$C;&$_($tx)for@$u6;&$_($yx)for@$C;&$_($Cx)for@$C;&$_($Gx)for@$C;&$_($Kx)for@$C;&$_($Ox)for@$C;&$_($Rx)for@$u6;&$_($Wx)for@$C;&$_($cy)for@$C;&$_($gy)for@$C;&$_($jy)for@$u6;&$_($oy)for@$C;&$_($sy)for@$C;&$_($wy)for@$C;&$_($zy)for@$u6;&$_($By)for@$Cy;&$_($Hy)for@$J9;&$_($Ry)for@$C;&$_($Vy)for@$C;&$_($Zy)for@$C;&$_($fz)for@$C;&$_($jz)for@$C;&$_($mz)for@$u6;&$_($xz)for@$C;&$_($Az)for@$C;&$_($Dz)for@$C;&$_($Gz)for@$u6;&$_($Kz)for@$C;&$_($Nz)for@$C;&$_($Qz)for@$C;&$_($Tz)for@$C;&$_($Vz)for@$C;&$_($Yz)for@$u6;&$_($dA)for@$C;&$_($gA)for@$C;&$_($jA)for@$u6;&$_($nA)for@$C;&$_($qA)for@$C;&$_($tA)for@$u6;&$_($vA)for@$wA;&$_($BA)for@$J9;&$_($LA)for@$C;&$_($PA)for@$C;&$_($TA)for@$C;&$_($XA)for@$C;&$_($dB)for@$C;&$_($hB)for@$C;&$_($lB)for@$C;&$_($pB)for@$C;&$_($tB)for@$C;&$_($xB)for@$C;&$_($BB)for@$C;&$_($FB)for@$C;&$_($IB)for@$u6;&$_($SB)for@$C;&$_($VB)for@$u6;&$_($YB)for@$C;&$_($dC)for@$C;&$_($gC)for@$u6;&$_($iC)for@$jC;&$_($oC)for@$J9;&$_($xC)for@$C;&$_($zC)for@$C;&$_($GC)for@$u6;&$_($JC)for@$C;&$_($MC)for@$u6;&$_($QC)for@$C;&$_($TC)for@$u6;&$_($VC)for@$WC;&$_($eD)for@$J9;&$_($lD)for@$C;&$_($pD)for@$C;&$_($sD)for@$u6;&$_($zD)for@$C;&$_($CD)for@$u6;&$_($FD)for@$C;&$_($ID)for@$u6;&$_($KD)for@$LD;&$_($RD)for@$J9;&$_($eE)for@$C;&$_($hE)for@$u6;&$_($jE)for@$kE;&$_($oE)for@$C;&$_($rE)for@$u6;&$_($vE)for@$C;&$_($zE)for@$C;&$_($CE)for@$C;&$_($FE)for@$u6;&$_($HE)for@$IE;&$_($PE)for@$J9;&$_($RE)for@$J9;&$_($gF)for@$C;&$_($jF)for@$C;&$_($mF)for@$u6;&$_($qF)for@$C;&$_($tF)for@$u6;&$_($xF)for@$C;&$_($AF)for@$u6;&$_($EF)for@$C;&$_($HF)for@$C;&$_($KF)for@$u6;&$_($PF)for@$C;&$_($SF)for@$C;&$_($VF)for@$u6;&$_($XF)for@$YF;&$_($hG)for@$C;&$_($kG)for@$u6;&$_($oG)for@$C;&$_($sG)for@$C;&$_($vG)for@$u6;&$_($xG)for@$J9;&$_($LG)for@$C;&$_($OG)for@$u6;&$_($TG)for@$C;&$_($XG)for@$C;&$_($cH)for@$u6;&$_($gH)for@$C;&$_($jH)for@$u6;&$_($lH)for@$mH;&$_($rH)for@$J9;&$_($zH)for@$AH;&$_($FH)for@$J9;&$_($OH)for@$PH;&$_($UH)for@$J9;&$_($eI)for@$fI;&$_($BI)for@$fI;&$_($GI)for@$C;&$_($KI)for@$C;&$_($OI)for@$C;&$_($SI)for@$C;&$_($WI)for@$C;ni->run(@ARGV);
__DATA__
