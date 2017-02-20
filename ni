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
now foo->f == 'hi';#;$x=q#eval_number#;$y=q#proto#;$z=q##;$A=q#lib/fn#;$B=bless({$t,$u,$v,$w,$x,490,$y,$z},$A);$C=q#lib/fn::ctors#;$D=q#lib/test_case#;$E=bless({$n,$o,$p,$q,$r,$q,$s,$B},$D);$F=q#TODO...#;$G=[$l,$m,$E,$F];$H=q#classes#;$I=q#ni implements a Smalltalk 80-style metaclass system with a couple of
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
now [$f1->v, $f2->v] == [5, 6];#;$d1=bless({$t,$Z,$v,$c1,$x,494,$y,$z},$A);$e1=bless({$n,$Y,$p,$q,$r,$q,$s,$d1},$D);$f1=q#You can combine multiple futures in different ways depending on what
      you're trying to do.#;$g1=[];$h1=[];$i1=q#my $f1 = ni('ni:/fabric/future')->new;
my $f2 = ni('ni:/fabric/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1, 2]];#;$j1=bless({$t,$h1,$v,$i1,$x,496,$y,$z},$A);$k1=bless({$n,$g1,$p,$q,$r,$q,$s,$j1},$D);$l1=[$i,$X,$e1,$f1,$k1];$m1=[$l1];$n1=q#/fabric/future#;$o1=bless({$e,$m1,$L,$n1},$N);$p1=q#ni.doc:/io#;$q1=q#An implementation of IO in terms of system-level FDs. We need this for a
      few reasons, three of them being that (1) old versions of Perl don't
      correctly handle interrupted system calls, (2) we want tighter control
      over which FDs are closed at what times, and (3) we want to be able to
      "unread" things -- push back against the read buffer (or use a custom
      read format in general).#;$r1=[$i,$q1];$s1=[$r1];$t1=q#/io#;$u1=bless({$e,$s1,$L,$t1},$N);$v1=q#ni.doc:/io/buffer#;$w1=q#
    my $buf = ni("ni:/io/buffer")->new(8192);
    $buf->write("foo");
    $buf->read($_, 256);        \# reads "foo"#;$x1=[$f,$w1];$y1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
      behaves like a nonblocking socket and sets errno accordingly.#;$z1=[];$A1=[];$B1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$C1=bless({$t,$A1,$v,$B1,$x,498,$y,$z},$A);$D1=bless({$n,$z1,$p,$q,$r,$q,$s,$C1},$D);$E1=[$i,$y1,$D1];$F1=[$x1,$E1];$G1=q#/io/buffer#;$H1=bless({$e,$F1,$L,$G1},$N);$I1=q#ni.doc:/io/cat#;$J1=q#
    my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into_sync($destination_io);
  #;$K1=[$f,$J1];$L1=q#Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.#;$M1=[];$N1=[];$O1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$P1=bless({$t,$N1,$v,$O1,$x,500,$y,$z},$A);$Q1=bless({$n,$M1,$p,$q,$r,$q,$s,$P1},$D);$R1=[$i,$L1,$Q1];$S1=[$K1,$R1];$T1=q#/io/cat#;$U1=bless({$e,$S1,$L,$T1},$N);$V1=q#ni.doc:/io/exec#;$W1=q#
    my $pid = ni("ni:/io/exec")->new("ls", "-l")
      ->connect(1 => ni("file:foo")->w)
      ->env(ENV_VAR => "value", ENV2 => "val2")
      ->fork;
    $? = $pid->await or die "ls -l failed: $?";#;$X1=[$f,$W1];$Y1=q#An object that represents a fork+exec operation that hasn't yet happened.
      It allows you to incrementally specify the context of the process,
      including environment variables and file descriptor mappings. It is also
      an IO object and will set up pipes to stdin/out if you use it this way.#;$Z1=[];$c2=[];$d2=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$e2=bless({$t,$c2,$v,$d2,$x,502,$y,$z},$A);$f2=bless({$n,$Z1,$p,$q,$r,$q,$s,$e2},$D);$g2=[$i,$Y1,$f2];$h2=[$X1,$g2];$i2=q#/io/exec#;$j2=bless({$e,$h2,$L,$i2},$N);$k2=q#ni.doc:/io/fd#;$l2=q#
    open my $fh, ...;
    my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
    my $fd = ni('ni:/io/fd')->new(0);   \# from number
    my $fd = ni('fd:0');                \# same thing
    $fd->nonblock(1)->read($_, 100);
    $fd->be(10);                        \# move FD number
  #;$m2=[$f,$l2];$n2=q#Represents a file descriptor as a child of /io/object (so the usual IO
      methods like into_async are available), and provides some convenience
      functions for things like setting up FDs for child processes. FDs are
      closed when destroyed.#;$o2=[];$p2=[];$q2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$r2=bless({$t,$p2,$v,$q2,$x,504,$y,$z},$A);$s2=bless({$n,$o2,$p,$q,$r,$q,$s,$r2},$D);$t2=[$i,$n2,$s2];$u2=[$m2,$t2];$v2=q#/io/fd#;$w2=bless({$e,$u2,$L,$v2},$N);$x2=q#ni.doc:/io/file#;$y2=q#
    my $f = ni('ni:/io/file')->new('/etc/passwd');
    my $f = ni('file:/etc/passwd');     \# same as above
    $f->into_sync(ni('fd:1'));          \# cat to stdout
  #;$z2=[$f,$y2];$A2=q#warning#;$B2=q#Files overload the -X file test operators, but this feature wasn't
      introduced until Perl 5.12 -- prior versions won't recognize this
      overload. That means that using this overload in ni's base code will
      reduce its portability and cause tests to fail.#;$C2=[$A2,$B2];$D2=q#Represents a file that may or may not exist, and stores/constructs file
      descriptors for reading/writing. /io/files are one-shot objects: once
      you've consumed them for reading or written to them, you should destroy
      the object and start over (or close the file) if you want to operate on
      the file further -- put differently, /io/file objects own the FDs they
      create.#;$E2=[];$F2=[];$G2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$H2=bless({$t,$F2,$v,$G2,$x,506,$y,$z},$A);$I2=bless({$n,$E2,$p,$q,$r,$q,$s,$H2},$D);$J2=q#File objects also provide some useful functions like atomic-updating.
      This lets you write a stream slowly into a tempfile, then rename over the
      original once the tempfile is closed. ni uses this to update itself to
      avoid race conditions.#;$K2=[];$L2=[];$M2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$N2=bless({$t,$L2,$v,$M2,$x,508,$y,$z},$A);$O2=bless({$n,$K2,$p,$q,$r,$q,$s,$N2},$D);$P2=[$i,$D2,$I2,$J2,$O2];$Q2=[$z2,$C2,$P2];$R2=q#/io/file#;$S2=bless({$e,$Q2,$L,$R2},$N);$T2=q#ni.doc:/io/file_update_fd#;$U2=q#A write fd that performs a file rename upon closing.#;$V2=[$i,$U2];$W2=[$V2];$X2=q#/io/file_update_fd#;$Y2=bless({$e,$W2,$L,$X2},$N);$Z2=q#ni.doc:/io/pid#;$c3=q#eg#;$d3=[];$e3=[];$f3=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$g3=bless({$t,$e3,$v,$f3,$x,510,$y,$z},$A);$h3=bless({$n,$d3,$p,$q,$r,$q,$s,$g3},$D);$i3=[$c3,$h3];$j3=[];$k3=[];$l3=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$m3=bless({$t,$k3,$v,$l3,$x,512,$y,$z},$A);$n3=bless({$n,$j3,$p,$q,$r,$q,$s,$m3},$D);$o3=[$c3,$n3];$p3=[];$q3=[];$r3=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$s3=bless({$t,$q3,$v,$r3,$x,514,$y,$z},$A);$t3=bless({$n,$p3,$p,$q,$r,$q,$s,$s3},$D);$u3=[$c3,$t3];$v3=[$i3,$o3,$u3];$w3=q#/io/pid#;$x3=bless({$e,$v3,$L,$w3},$N);$y3=q#ni.doc:/lib#;$z3=q#Bootstrapping code for the core abstractions in ni, and almost everything
      about its introspection. This includes definitions for documentation,
      unit tests, classes, support for basic image generation, etc -- and when
      possible, it's written with some awareness of downstream use cases (for
      instance, image serialization and RMI share logic).#;$A3=q#/lib is the place where things don't quite work yet, so the code here is
      written differently from other modules.#;$B3=[$i,$z3,$A3];$C3=[$B3];$D3=q#/lib#;$E3=bless({$e,$C3,$L,$D3},$N);$F3=q#ni.doc:/lib/doc#;$G3=q#
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...#;$H3=[$f,$G3];$I3=q#Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class's code without bringing along all of
      its documentation and unit tests.#;$J3=q#Documentation objects are internally represented as arrays of quoted
      method calls:#;$K3=[];$L3=[];$M3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$N3=bless({$t,$L3,$v,$M3,$x,516,$y,$z},$A);$O3=bless({$n,$K3,$p,$q,$r,$q,$s,$N3},$D);$P3=q#This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":#;$Q3=[];$R3=[];$S3=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$T3=bless({$t,$R3,$v,$S3,$x,518,$y,$z},$A);$U3=bless({$n,$Q3,$p,$q,$r,$q,$s,$T3},$D);$V3=[$i,$I3,$J3,$O3,$P3,$U3];$W3=[$H3,$V3];$X3=q#/lib/doc#;$Y3=bless({$e,$W3,$L,$X3},$N);$Z3=q#ni.doc:/lib/image#;$c4=q#
    my $image = ni("ni:/lib/image")->new;
    my $gensym = $image->quote($value);
    $image->io->into_sync($a_file);#;$d4=[$f,$c4];$e4=q#Generates Perl code that reconstructs the state of objects at the
      behavior/slice level. Since classes are self-describing, this results in
      a replica of the runtime object-oriented state.#;$f4=[$i,$e4];$g4=[$d4,$f4];$h4=q#/lib/image#;$i4=bless({$e,$g4,$L,$h4},$N);$j4=q#ni.doc:/lib/ni#;$k4=q#my $value = ni->resolve($name);
               my $value = ni($name);   \# alias for ni->resolve($name)
               my $self  = ni;#;$l4=[$f,$k4];$m4=q#The class for the currently-running ni instance. This includes all
      instance state, the table of named objects, and a bit of logic to update
      ni in place, for instance when adding extensions.#;$n4=[$i,$m4];$o4=[$l4,$n4];$p4=q#/lib/ni#;$q4=bless({$e,$o4,$L,$p4},$N);$r4=q#ni.doc:/lib/quote_simple#;$s4=q#A stateless object that serializes values with direct quotation; that
      is, the serialization contains no variables. If your objects have
      circular or shared references, you should probably use
      /lib/quote_circular or similar.#;$t4=[];$u4=[];$v4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$w4=bless({$t,$u4,$v,$v4,$x,520,$y,$z},$A);$x4=bless({$n,$t4,$p,$q,$r,$q,$s,$w4},$D);$y4=[$i,$s4,$x4];$z4=[$y4];$A4=q#/lib/quote_simple#;$B4=bless({$e,$z4,$L,$A4},$N);$C4=q#ni.doc:/lib/slice#;$D4=q#
    ni('ni:/lib/slice')->new('/lib/foo',
      ctor => fn q{shift->say_hi},
      say_hi => fn q{print "hi from " . shift->name . "\\n"});
    $some_class->add('/lib/foo');#;$E4=[$f,$D4];$F4=q#A slice of methods encoding some aspect of an object's behavior. Slices
      are combined using tags and branches, and the set of slices used to
      construct a class must be disjoint except for constructors and
      destructors.#;$G4=q#Slices are objects that provide an ->apply method, which installs their
      methods + ctors + dtors into a Perl package.#;$H4=[];$I4=[];$J4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$K4=bless({$t,$I4,$v,$J4,$x,522,$y,$z},$A);$L4=bless({$n,$H4,$p,$q,$r,$q,$s,$K4},$D);$M4=q#Slices automatically do the equivalent of using Perl's "overload" module
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
now $instances == 0;#;$R4=bless({$t,$P4,$v,$Q4,$x,524,$y,$z},$A);$S4=bless({$n,$O4,$p,$q,$r,$q,$s,$R4},$D);$T4=[$i,$F4,$G4,$L4,$M4,$N4,$S4];$U4=[$E4,$T4];$V4=q#/lib/slice#;$W4=bless({$e,$U4,$L,$V4},$N);$X4=q#ni.doc:/semantic#;$Y4=q#Opportunities to assign real-world semantics to objects. This is a
      collection of behaviors that don't necessarily imply a Perl-level
      protocol, but which may end up meaning something at some point.#;$Z4=[$i,$Y4];$c5=[$Z4];$d5=q#/semantic#;$e5=bless({$e,$c5,$L,$d5},$N);$f5=q#ni:/class#;$g5=q#applied_to#;$h5=q#class#;$i5=q#class.c#;$j5=q#fabric/future.c#;$k5=q#io/buffer.c#;$l5=q#io/cat.c#;$m5=q#io/exec.c#;$n5=q#io/fd.c#;$o5=q#io/file.c#;$p5=q#io/file_update_fd.c#;$q5=q#io/null.c#;$r5=q#io/object.c#;$s5=q#io/pid.c#;$t5=q#io/str.c#;$u5=q#io/transfer.c#;$v5=q#io/transfer_async.c#;$w5=q#io/transfer_sync.c#;$x5=q#lib/behavior.c#;$y5=q#lib/branch.c#;$z5=q#lib/dataslice.c#;$A5=q#lib/doc.c#;$B5=q#lib/fn.c#;$C5=q#lib/image.c#;$D5=q#lib/ni.c#;$E5=q#lib/quote_simple.c#;$F5=q#lib/slice.c#;$G5=q#lib/tag.c#;$H5=q#lib/test_assert_eq.c#;$I5=q#lib/test_assertion.c#;$J5=q#lib/test_case.c#;$K5=q#lib/test_value.c#;$L5=q#metaclass.c#;$M5=q#module.c#;$N5=q#object.c#;$O5=q#semantic/dimension#;$P5=q#semantic/dimension.c#;$Q5=q#semantic/task.c#;$R5={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$S5=q#slices#;$T5=q#metaclass#;$U5=q#module#;$V5={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$W5=q#/module#;$X5=q#/lib/perlbranch.b#;$Y5={};$Z5=q#ctor#;$c6=q#dtor#;$d6=q#methods#;$e6=q#add#;$f6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$g6=bless({$v,$f6,$x,526,$y,$z},$A);$h6=q#apply#;$i6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$j6=bless({$v,$i6,$x,528,$y,$z},$A);$k6={$e6,$g6,$h6,$j6};$l6=q#/lib/branch.b#;$m6=q#lib/slice#;$n6=bless({$g5,$Y5,$Z5,$q,$c6,$q,$d6,$k6,$L,$l6},$m6);$o6=q#lib/branch#;$p6=q#lib/slice::ctors#;$q6={};$r6=q#my $s = shift; ni->def($s->name, $s)#;$s6=bless({$v,$r6,$x,530,$y,$z},$A);$t6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$u6=bless({$v,$t6,$x,532,$y,$z},$A);$v6={$L,$u6};$w6=q#/lib/named.b#;$x6=bless({$g5,$q6,$Z5,$s6,$c6,$q,$d6,$v6,$L,$w6},$m6);$y6=q#lib/tag#;$z6={};$A6=q#namespace#;$B6=q#'ni'#;$C6=bless({$v,$B6,$x,534,$y,$z},$A);$D6={$A6,$C6};$E6=q#/lib/named_in_ni.b#;$F6=bless({$g5,$z6,$Z5,$q,$c6,$q,$d6,$D6,$L,$E6},$m6);$G6={};$H6=q#package#;$I6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$J6=bless({$v,$I6,$x,536,$y,$z},$A);$K6={$H6,$J6};$L6=q#/lib/namespaced.b#;$M6=bless({$g5,$G6,$Z5,$q,$c6,$q,$d6,$K6,$L,$L6},$m6);$N6={};$O6=q#resolve#;$P6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$Q6=bless({$v,$P6,$x,538,$y,$z},$A);$R6={$O6,$Q6};$S6=q#/lib/resolver.b#;$T6=bless({$g5,$N6,$Z5,$q,$c6,$q,$d6,$R6,$L,$S6},$m6);$U6=[$n6,$x6,$F6,$M6,$T6];$V6=bless({$L,$X5,$S5,$U6},$y6);$W6=q#lib/tag::ctors#;$X6={};$Y6=q#my $s = shift; $s->apply($s->package)#;$Z6=bless({$v,$Y6,$x,540,$y,$z},$A);$c7=q#instantiate#;$d7=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$e7=bless({$v,$d7,$x,542,$y,$z},$A);$f7={$c7,$e7};$g7=q#/lib/class_init.b#;$h7=bless({$g5,$X6,$Z5,$Z6,$c6,$q,$d6,$f7,$L,$g7},$m6);$i7=q#fabric/future#;$j7=q#io/buffer#;$k7=q#io/cat#;$l7=q#io/exec#;$m7=q#io/fd#;$n7=q#io/file#;$o7=q#io/file_update_fd#;$p7=q#io/null#;$q7=q#io/object#;$r7=q#io/pid#;$s7=q#io/str#;$t7=q#io/transfer#;$u7=q#io/transfer_async#;$v7=q#io/transfer_sync#;$w7=q#lib/behavior#;$x7=q#lib/dataslice#;$y7=q#lib/image#;$z7=q#lib/ni#;$A7=q#lib/quote_simple#;$B7=q#lib/test_assert_eq#;$C7=q#lib/test_assertion#;$D7=q#lib/test_value#;$E7=q#object#;$F7=q#semantic/task#;$G7={$h5,1,$i5,1,$i7,1,$j5,1,$j7,1,$k5,1,$k7,1,$l5,1,$l7,1,$m5,1,$m7,1,$n5,1,$n7,1,$o5,1,$o7,1,$p5,1,$p7,1,$q5,1,$q7,1,$r5,1,$r7,1,$s5,1,$s7,1,$t5,1,$t7,1,$u5,1,$u7,1,$v5,1,$v7,1,$w5,1,$w7,1,$x5,1,$o6,1,$y5,1,$x7,1,$z5,1,$N,1,$A5,1,$A,1,$B5,1,$y7,1,$C5,1,$z7,1,$D5,1,$A7,1,$E5,1,$m6,1,$F5,1,$y6,1,$G5,1,$B7,1,$H5,1,$C7,1,$I5,1,$D,1,$J5,1,$D7,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$E7,1,$N5,1,$O5,1,$P5,1,$F7,1,$Q5,1};$H7=q#/object#;$I7={};$J7=q#DESTROY#;$K7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$L7=bless({$v,$K7,$x,544,$y,$z},$A);$M7=q#ni 'ni:/' . ref shift#;$N7=bless({$v,$M7,$x,546,$y,$z},$A);$O7={$J7,$L7,$h5,$N7};$P7=q#/lib/instance.b#;$Q7=bless({$g5,$I7,$Z5,$q,$c6,$q,$d6,$O7,$L,$P7},$m6);$R7=[$Q7];$S7=bless({$g5,$G7,$L,$H7,$S5,$R7},$N5);$T7=q#object.c::ctors#;$U7={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$w7,1,$x5,1,$o6,1,$y5,1,$x7,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$m6,1,$F5,1,$y6,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$V7=q#/lib/behavior#;$W7={};$X7=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$Y7=bless({$v,$X7,$x,548,$y,$z},$A);$Z7={$e,$Y7};$c8=q#/lib/documentable.b#;$d8=bless({$g5,$W7,$Z5,$q,$c6,$q,$d6,$Z7,$L,$c8},$m6);$e8=[$S7,$d8];$f8=bless({$g5,$U7,$L,$V7,$S5,$e8},$x5);$g8=q#lib/behavior.c::ctors#;$h8={$h5,1,$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$o6,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$T5,1,$L5,1,$U5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1};$i8=q#/lib/definition.b#;$j8={};$k8=q#def#;$l8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$m8=bless({$v,$l8,$x,550,$y,$z},$A);$n8={$k8,$m8};$o8=q#/lib/definition_def.b#;$p8=bless({$g5,$j8,$Z5,$q,$c6,$q,$d6,$n8,$L,$o8},$m6);$q8={};$r8=q#ro#;$s8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$t8=bless({$v,$s8,$x,552,$y,$z},$A);$u8=q#rw#;$v8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$w8=bless({$v,$v8,$x,554,$y,$z},$A);$x8={$r8,$t8,$u8,$w8};$y8=q#/lib/accessor.b#;$z8=bless({$g5,$q8,$Z5,$q,$c6,$q,$d6,$x8,$L,$y8},$m6);$A8={};$B8=q#(""#;$C8=q#shift->name#;$D8=bless({$v,$C8,$x,556,$y,$z},$A);$E8={$B8,$D8};$F8=q#/lib/name_as_string.b#;$G8=bless({$g5,$A8,$Z5,$q,$c6,$q,$d6,$E8,$L,$F8},$m6);$H8={};$I8=q#(eq#;$J8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$K8=bless({$v,$J8,$x,558,$y,$z},$A);$L8={$I8,$K8};$M8=q#/lib/ref_eq.b#;$N8=bless({$g5,$H8,$Z5,$q,$c6,$q,$d6,$L8,$L,$M8},$m6);$O8={};$P8=q#defdata#;$Q8=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$R8=bless({$v,$Q8,$x,560,$y,$z},$A);$S8={$P8,$R8};$T8=q#/lib/definition_defdata.b#;$U8=bless({$g5,$O8,$Z5,$q,$c6,$q,$d6,$S8,$L,$T8},$m6);$V8=[$p8,$z8,$G8,$N8,$U8];$W8=bless({$g5,$h8,$L,$i8,$S5,$V8},$o6);$X8=q#lib/branch::ctors#;$Y8=[$V6,$h7,$S7,$f8,$W8];$Z8=bless({$g5,$V5,$L,$W5,$S5,$Y8},$M5);$c9=q#module.c::ctors#;$d9={};$e9=q#new#;$f9=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$g9=bless({$v,$f9,$x,562,$y,$z},$A);$h9={$e9,$g9};$i9=q#/lib/instantiable.b#;$j9=bless({$g5,$d9,$d6,$h9,$L,$i9},$m6);$k9={};$l9=q#child#;$m9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$n9=bless({$v,$m9,$x,564,$y,$z},$A);$o9={$l9,$n9};$p9=q#/lib/subclass.b#;$q9=bless({$g5,$k9,$Z5,$q,$c6,$q,$d6,$o9,$L,$p9},$m6);$r9=[$Z8,$j9,$h7,$Z8,$q9];$s9=bless({$g5,$R5,$L,$M,$S5,$r9},$i5);$t9=q#class.c::ctors#;$u9=q#ni:/class.c#;$v9={$i5,1,$P5,1};$w9=q#/class.c#;$x9={$i5,1,$M5,1,$P5,1};$y9=q#/module.c#;$z9={$i5,1,$j5,1,$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$M5,1,$N5,1,$P5,1,$Q5,1};$A9=q#/object.c#;$B9=[$s9];$C9=bless({$g5,$z9,$L,$A9,$S5,$B9},$T5);$D9=q#metaclass::ctors#;$E9={$i5,1,$x5,1,$y5,1,$z5,1,$F5,1,$G5,1,$M5,1,$P5,1};$F9=q#/lib/behavior.c#;$G9=[$C9];$H9=bless({$g5,$E9,$L,$F9,$S5,$G9},$T5);$I9=[$C9,$j9,$H9];$J9=bless({$g5,$x9,$L,$y9,$S5,$I9},$T5);$K9=[$J9];$L9=bless({$g5,$v9,$L,$w9,$S5,$K9},$T5);$M9=q#ni:/fabric/future#;$N9={$i7,1};$O9={};$P9=[];$Q9=q#shift->{'outcome'}#;$R9=bless({$t,$P9,$v,$Q9,$x,566,$y,$z},$A);$S9=q#parents#;$T9=[];$U9=q#shift->{'parents'}#;$V9=bless({$t,$T9,$v,$U9,$x,568,$y,$z},$A);$W9=q#v#;$X9=[];$Y9=q#shift->{'v'}#;$Z9=bless({$t,$X9,$v,$Y9,$x,570,$y,$z},$A);$ca={$r,$R9,$S9,$V9,$W9,$Z9};$da=q#/fabric/future_ro.b#;$ea=bless({$g5,$O9,$Z5,$q,$c6,$q,$d6,$ca,$L,$da},$m6);$fa={};$ga=[];$ha=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$ia=bless({$t,$ga,$v,$ha,$x,572,$y,$z},$A);$ja={$c7,$ia};$ka=q#/fabric/future_init.b#;$la=bless({$g5,$fa,$Z5,$q,$c6,$q,$d6,$ja,$L,$ka},$m6);$ma={};$na=q#decide#;$oa=[];$pa=q#local $_;
my ($self, $v) = @_;
die "ni:/fabric/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$qa=bless({$t,$oa,$v,$pa,$x,574,$y,$z},$A);$ra=q#decided#;$sa=[];$ta=q#shift->{outcome}#;$ua=bless({$t,$sa,$v,$ta,$x,576,$y,$z},$A);$va=q#listener#;$wa=[];$xa=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$ya=bless({$t,$wa,$v,$xa,$x,578,$y,$z},$A);$za={$na,$qa,$ra,$ua,$va,$ya};$Aa=q#/fabric/future_state.b#;$Ba=bless({$g5,$ma,$Z5,$q,$c6,$q,$d6,$za,$L,$Aa},$m6);$Ca={};$Da=q#and#;$Ea=[];$Fa=q#my $self   = $_[0];
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
$child;#;$Ga=bless({$t,$Ea,$v,$Fa,$x,580,$y,$z},$A);$Ha=q#flatmap#;$Ia=[];$Ja=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$Ka=bless({$t,$Ia,$v,$Ja,$x,582,$y,$z},$A);$La=q#map#;$Ma=[];$Na=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$Oa=bless({$t,$Ma,$v,$Na,$x,584,$y,$z},$A);$Pa=q#or#;$Qa=[];$Ra=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$Sa=bless({$t,$Qa,$v,$Ra,$x,586,$y,$z},$A);$Ta={$Da,$Ga,$Ha,$Ka,$La,$Oa,$Pa,$Sa};$Ua=q#/fabric/future_algebra.b#;$Va=bless({$g5,$Ca,$Z5,$q,$c6,$q,$d6,$Ta,$L,$Ua},$m6);$Wa=[$S7,$ea,$la,$Ba,$Va];$Xa=bless({$g5,$N9,$L,$n1,$S5,$Wa},$j5);$Ya=q#fabric/future.c::ctors#;$Za=q#ni:/fabric/future.c#;$cb={$j5,1};$db=q#/fabric/future.c#;$eb=[$C9];$fb=bless({$g5,$cb,$L,$db,$S5,$eb},$T5);$gb=q#ni:/fabric/future_algebra.b#;$hb=q#ni:/fabric/future_init.b#;$ib=q#ni:/fabric/future_ro.b#;$jb=q#ni:/fabric/future_state.b#;$kb=q#ni:/io/buffer#;$lb={$j7,1};$mb={$j7,1,$k7,1,$l7,1,$m7,1,$n7,1,$o7,1,$p7,1,$q7,1,$r7,1,$s7,1};$nb=q#/io/object#;$ob={};$pb=q#(bool#;$qb=[];$rb=bless({$t,$qb,$v,1,$x,588,$y,$z},$A);$sb={$pb,$rb};$tb=q#/io/object_ops.b#;$ub=bless({$g5,$ob,$Z5,$q,$c6,$q,$d6,$sb,$L,$tb},$m6);$vb={};$wb=q#die#;$xb=[];$yb=q#shift; die join " ", @_#;$zb=bless({$t,$xb,$v,$yb,$x,590,$y,$z},$A);$Ab=q#io_check#;$Bb=[];$Cb=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$Db=bless({$t,$Bb,$v,$Cb,$x,592,$y,$z},$A);$Eb=q#io_check_defined#;$Fb=[];$Gb=q#shift->io_check(sub {defined shift}, @_)#;$Hb=bless({$t,$Fb,$v,$Gb,$x,594,$y,$z},$A);$Ib=q#io_check_true#;$Jb=[];$Kb=q#shift->io_check(sub {shift}, @_)#;$Lb=bless({$t,$Jb,$v,$Kb,$x,596,$y,$z},$A);$Mb={$wb,$zb,$Ab,$Db,$Eb,$Hb,$Ib,$Lb};$Nb=q#/io/object_checks.b#;$Ob=bless({$g5,$vb,$Z5,$q,$c6,$q,$d6,$Mb,$L,$Nb},$m6);$Pb={};$Qb=q#(+#;$Rb=[];$Sb=q#ni('ni:/io/cat')->new(@_[0, 1])#;$Tb=bless({$t,$Rb,$v,$Sb,$x,598,$y,$z},$A);$Ub={$Qb,$Tb};$Vb=q#/io/object_constructors.b#;$Wb=bless({$g5,$Pb,$Z5,$q,$c6,$q,$d6,$Ub,$L,$Vb},$m6);$Xb={};$Yb=q#read_all#;$Zb=[];$cc=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$dc=bless({$t,$Zb,$v,$cc,$x,600,$y,$z},$A);$ec=q#write_all#;$fc=[];$gc=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$hc=bless({$t,$fc,$v,$gc,$x,602,$y,$z},$A);$ic={$Yb,$dc,$ec,$hc};$jc=q#/io/object_memory.b#;$kc=bless({$g5,$Xb,$Z5,$q,$c6,$q,$d6,$ic,$L,$jc},$m6);$lc={};$mc=q#connect_sync#;$nc=[];$oc=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$pc=bless({$t,$nc,$v,$oc,$x,604,$y,$z},$A);$qc=q#into_sync#;$rc=[];$sc=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$tc=bless({$t,$rc,$v,$sc,$x,606,$y,$z},$A);$uc={$mc,$pc,$qc,$tc};$vc=q#/io/object_transfer_sync.b#;$wc=bless({$g5,$lc,$Z5,$q,$c6,$q,$d6,$uc,$L,$vc},$m6);$xc={};$yc=q#connect_async#;$zc=[];$Ac=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$Bc=bless({$t,$zc,$v,$Ac,$x,608,$y,$z},$A);$Cc=q#into_async#;$Dc=[];$Ec=q#ni('ni:/io/transfer_async')->new(@_)->run#;$Fc=bless({$t,$Dc,$v,$Ec,$x,610,$y,$z},$A);$Gc={$yc,$Bc,$Cc,$Fc};$Hc=q#/io/object_transfer_async.b#;$Ic=bless({$g5,$xc,$Z5,$q,$c6,$q,$d6,$Gc,$L,$Hc},$m6);$Jc=[$S7,$ub,$Ob,$Wb,$kc,$wc,$Ic,$Ic,$wc,$Ic,$wc];$Kc=bless({$g5,$mb,$L,$nb,$S5,$Jc},$r5);$Lc=q#io/object.c::ctors#;$Mc={};$Nc=[];$Oc=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$Pc=bless({$t,$Nc,$v,$Oc,$x,612,$y,$z},$A);$Qc={$c7,$Pc};$Rc=q#/io/buffer_init.b#;$Sc=bless({$g5,$Mc,$Z5,$q,$c6,$q,$d6,$Qc,$L,$Rc},$m6);$Tc={};$Uc=q#read#;$Vc=[];$Wc=q#my $self = shift;
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
}#;$Xc=bless({$t,$Vc,$v,$Wc,$x,614,$y,$z},$A);$Yc=q#read_capacity#;$Zc=[];$cd=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$dd=bless({$t,$Zc,$v,$cd,$x,616,$y,$z},$A);$ed=q#write#;$fd=[];$gd=q#my $self = shift;
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
}#;$hd=bless({$t,$fd,$v,$gd,$x,618,$y,$z},$A);$id=q#write_capacity#;$jd=[];$kd=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$ld=bless({$t,$jd,$v,$kd,$x,620,$y,$z},$A);$md={$Uc,$Xc,$Yc,$dd,$ed,$hd,$id,$ld};$nd=q#/io/buffer_io.b#;$od=bless({$g5,$Tc,$Z5,$q,$c6,$q,$d6,$md,$L,$nd},$m6);$pd=[$Kc,$Sc,$od];$qd=bless({$g5,$lb,$L,$G1,$S5,$pd},$k5);$rd=q#io/buffer.c::ctors#;$sd=q#ni:/io/buffer.c#;$td={$k5,1};$ud=q#/io/buffer.c#;$vd={$k5,1,$l5,1,$m5,1,$n5,1,$o5,1,$p5,1,$q5,1,$r5,1,$s5,1,$t5,1};$wd=q#/io/object.c#;$xd={};$yd=q#def_transfer_method#;$zd=[];$Ad=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Bd=bless({$t,$zd,$v,$Ad,$x,622,$y,$z},$A);$Cd={$yd,$Bd};$Dd=q#/io/object.c_transfer_def.b#;$Ed=bless({$g5,$xd,$Z5,$q,$c6,$q,$d6,$Cd,$L,$Dd},$m6);$Fd=[$C9,$Ed];$Gd=bless({$g5,$vd,$L,$wd,$S5,$Fd},$T5);$Hd=[$Gd];$Id=bless({$g5,$td,$L,$ud,$S5,$Hd},$T5);$Jd=q#ni:/io/buffer_init.b#;$Kd=q#ni:/io/buffer_io.b#;$Ld=q#ni:/io/cat#;$Md={$k7,1};$Nd={};$Od=[];$Pd=q#shift; +{fs => [@_]}#;$Qd=bless({$t,$Od,$v,$Pd,$x,624,$y,$z},$A);$Rd={$c7,$Qd};$Sd=q#/io/cat_init.b#;$Td=bless({$g5,$Nd,$Z5,$q,$c6,$q,$d6,$Rd,$L,$Sd},$m6);$Ud={};$Vd=[];$Wd=q#my $fs = shift->{fs};
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
$total_read;#;$Xd=bless({$t,$Vd,$v,$Wd,$x,626,$y,$z},$A);$Yd={$Uc,$Xd};$Zd=q#/io/cat_read.b#;$ce=bless({$g5,$Ud,$Z5,$q,$c6,$q,$d6,$Yd,$L,$Zd},$m6);$de=[$Kc,$Td,$ce];$ee=bless({$g5,$Md,$L,$T1,$S5,$de},$l5);$fe=q#io/cat.c::ctors#;$ge=q#ni:/io/cat.c#;$he={$l5,1};$ie=q#/io/cat.c#;$je=[$Gd];$ke=bless({$g5,$he,$L,$ie,$S5,$je},$T5);$le=q#ni:/io/cat_init.b#;$me=q#ni:/io/cat_read.b#;$ne=q#ni:/io/exec#;$oe={$l7,1};$pe={};$qe=q#argv#;$re=[];$se=q#shift->{'argv'}#;$te=bless({$t,$re,$v,$se,$x,628,$y,$z},$A);$ue={$qe,$te};$ve=q#/io/exec_ro.b#;$we=bless({$g5,$pe,$Z5,$q,$c6,$q,$d6,$ue,$L,$ve},$m6);$xe={};$ye=[];$ze=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Ae=bless({$t,$ye,$v,$ze,$x,630,$y,$z},$A);$Be={$c7,$Ae};$Ce=q#/io/exec_init.b#;$De=bless({$g5,$xe,$Z5,$q,$c6,$q,$d6,$Be,$L,$Ce},$m6);$Ee={};$Fe=q#connect#;$Ge=[];$He=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Ie=bless({$t,$Ge,$v,$He,$x,632,$y,$z},$A);$Je=q#in_pipe#;$Ke=[];$Le=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$Me=bless({$t,$Ke,$v,$Le,$x,634,$y,$z},$A);$Ne=q#out_pipe#;$Oe=[];$Pe=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$Qe=bless({$t,$Oe,$v,$Pe,$x,636,$y,$z},$A);$Re=q#setup_stdio#;$Se=[];$Te=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$Ue=bless({$t,$Se,$v,$Te,$x,638,$y,$z},$A);$Ve={$Fe,$Ie,$Je,$Me,$Ne,$Qe,$Re,$Ue};$We=q#/io/exec_io_setup.b#;$Xe=bless({$g5,$Ee,$Z5,$q,$c6,$q,$d6,$Ve,$L,$We},$m6);$Ye={};$Ze=q#binds_fd#;$cf=[];$df=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$ef=bless({$t,$cf,$v,$df,$x,640,$y,$z},$A);$ff=q#fd#;$gf=[];$hf=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$if=bless({$t,$gf,$v,$hf,$x,642,$y,$z},$A);$jf=q#stderr#;$kf=[];$lf=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$mf=bless({$t,$kf,$v,$lf,$x,644,$y,$z},$A);$nf=q#stdin#;$of=[];$pf=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$qf=bless({$t,$of,$v,$pf,$x,646,$y,$z},$A);$rf=q#stdout#;$sf=[];$tf=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$uf=bless({$t,$sf,$v,$tf,$x,648,$y,$z},$A);$vf={$Ze,$ef,$ff,$if,$jf,$mf,$nf,$qf,$rf,$uf};$wf=q#/io/exec_io_accessors.b#;$xf=bless({$g5,$Ye,$Z5,$q,$c6,$q,$d6,$vf,$L,$wf},$m6);$yf={};$zf=q#env#;$Af=[];$Bf=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Cf=bless({$t,$Af,$v,$Bf,$x,650,$y,$z},$A);$Df={$zf,$Cf};$Ef=q#/io/exec_env.b#;$Ff=bless({$g5,$yf,$Z5,$q,$c6,$q,$d6,$Df,$L,$Ef},$m6);$Gf={};$Hf=q#exec#;$If=[];$Jf=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$Kf=bless({$t,$If,$v,$Jf,$x,652,$y,$z},$A);$Lf=q#fork#;$Mf=[];$Nf=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$Of=bless({$t,$Mf,$v,$Nf,$x,654,$y,$z},$A);$Pf=q#move_fds#;$Qf=[];$Rf=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$Sf=bless({$t,$Qf,$v,$Rf,$x,656,$y,$z},$A);$Tf={$Hf,$Kf,$Lf,$Of,$Pf,$Sf};$Uf=q#/io/exec_fork.b#;$Vf=bless({$g5,$Gf,$Z5,$q,$c6,$q,$d6,$Tf,$L,$Uf},$m6);$Wf=[$Kc,$we,$De,$Xe,$xf,$Ff,$Vf];$Xf=bless({$g5,$oe,$L,$i2,$S5,$Wf},$m5);$Yf=q#io/exec.c::ctors#;$Zf=q#ni:/io/exec.c#;$cg={$m5,1};$dg=q#/io/exec.c#;$eg=[$Gd];$fg=bless({$g5,$cg,$L,$dg,$S5,$eg},$T5);$gg=q#ni:/io/exec_env.b#;$hg=q#ni:/io/exec_fork.b#;$ig=q#ni:/io/exec_init.b#;$jg=q#ni:/io/exec_io_accessors.b#;$kg=q#ni:/io/exec_io_setup.b#;$lg=q#ni:/io/exec_ro.b#;$mg=q#ni:/io/fd#;$ng={$m7,1};$og={};$pg=[];$qg=q#shift->{'fd'}#;$rg=bless({$t,$pg,$v,$qg,$x,658,$y,$z},$A);$sg={$ff,$rg};$tg=q#/io/fd_readers.b#;$ug=bless({$g5,$og,$Z5,$q,$c6,$q,$d6,$sg,$L,$tg},$m6);$vg={};$wg=[];$xg=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$yg=bless({$t,$wg,$v,$xg,$x,660,$y,$z},$A);$zg={$c7,$yg};$Ag=q#/io/fd_init.b#;$Bg=bless({$g5,$vg,$Z5,$q,$c6,$q,$d6,$zg,$L,$Ag},$m6);$Cg={};$Dg=q#be#;$Eg=[];$Fg=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$$self{rfh} = $$self{wfh} = undef;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Gg=bless({$t,$Eg,$v,$Fg,$x,662,$y,$z},$A);$Hg={$Dg,$Gg};$Ig=q#/io/fd_shell.b#;$Jg=bless({$g5,$Cg,$Z5,$q,$c6,$q,$d6,$Hg,$L,$Ig},$m6);$Kg={};$Lg=q#cloexec#;$Mg=[];$Ng=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$Og=bless({$t,$Mg,$v,$Ng,$x,664,$y,$z},$A);$Pg=q#fcntl_flag#;$Qg=[];$Rg=q#my ($self, $flag, $value) = @_;
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
}#;$Sg=bless({$t,$Qg,$v,$Rg,$x,666,$y,$z},$A);$Tg=q#nonblock#;$Ug=[];$Vg=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$Wg=bless({$t,$Ug,$v,$Vg,$x,668,$y,$z},$A);$Xg={$Lg,$Og,$Pg,$Sg,$Tg,$Wg};$Yg=q#/io/fd_fcntl.b#;$Zg=bless({$g5,$Kg,$Z5,$q,$c6,$q,$d6,$Xg,$L,$Yg},$m6);$ch={};$dh=[];$eh=q#shift->close#;$fh=bless({$t,$dh,$v,$eh,$x,670,$y,$z},$A);$gh=q#close#;$hh=[];$ih=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
POSIX::close $$self{fd} if defined $$self{fd};
$$self{fd} = $$self{rfh} = $$self{wfh} = undef;
$self;#;$jh=bless({$t,$hh,$v,$ih,$x,672,$y,$z},$A);$kh={$gh,$jh};$lh=q#/io/fd_gc.b#;$mh=bless({$g5,$ch,$Z5,$q,$c6,$fh,$d6,$kh,$L,$lh},$m6);$nh={};$oh=[];$ph=q#my $self = shift;
open $$self{rfh}, "<&=$$self{fd}" or return undef unless $$self{rfh};
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$qh=bless({$t,$oh,$v,$ph,$x,674,$y,$z},$A);$rh=[];$sh=q#my $self = shift;
open $$self{wfh}, ">&=$$self{fd}" or return undef unless $$self{wfh};
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$th=bless({$t,$rh,$v,$sh,$x,676,$y,$z},$A);$uh={$Uc,$qh,$ed,$th};$vh=q#/io/fd_perlio.b#;$wh=bless({$g5,$nh,$Z5,$q,$c6,$q,$d6,$uh,$L,$vh},$m6);$xh=[$Kc,$ug,$Bg,$Jg,$Zg,$mh,$wh];$yh=bless({$g5,$ng,$L,$v2,$S5,$xh},$n5);$zh=q#io/fd.c::ctors#;$Ah=q#ni:/io/fd.c#;$Bh={$n5,1};$Ch=q#/io/fd.c#;$Dh=[$Gd];$Eh=bless({$g5,$Bh,$L,$Ch,$S5,$Dh},$T5);$Fh=q#ni:/io/fd_fcntl.b#;$Gh=q#ni:/io/fd_gc.b#;$Hh=q#ni:/io/fd_init.b#;$Ih=q#ni:/io/fd_perlio.b#;$Jh=q#ni:/io/fd_readers.b#;$Kh=q#ni:/io/fd_shell.b#;$Lh=q#ni:/io/file#;$Mh={$n7,1};$Nh={};$Oh=[];$Ph=q#shift->{'name'}#;$Qh=bless({$t,$Oh,$v,$Ph,$x,678,$y,$z},$A);$Rh={$L,$Qh};$Sh=q#/io/file_readers.b#;$Th=bless({$g5,$Nh,$Z5,$q,$c6,$q,$d6,$Rh,$L,$Sh},$m6);$Uh={};$Vh=q#mode#;$Wh=[];$Xh=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$Yh=bless({$t,$Wh,$v,$Xh,$x,680,$y,$z},$A);$Zh={$Vh,$Yh};$ci=q#/io/file_accessors.b#;$di=bless({$g5,$Uh,$Z5,$q,$c6,$q,$d6,$Zh,$L,$ci},$m6);$ei={};$fi=[];$gi=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$hi=bless({$t,$fi,$v,$gi,$x,682,$y,$z},$A);$ii={$c7,$hi};$ji=q#/io/file_init.b#;$ki=bless({$g5,$ei,$Z5,$q,$c6,$q,$d6,$ii,$L,$ji},$m6);$li={};$mi=q#(-X#;$ni=[];$oi=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$pi=bless({$t,$ni,$v,$oi,$x,684,$y,$z},$A);$qi=q#mv#;$ri=[];$si=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$ti=bless({$t,$ri,$v,$si,$x,686,$y,$z},$A);$ui=q#rm#;$vi=[];$wi=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$xi=bless({$t,$vi,$v,$wi,$x,688,$y,$z},$A);$yi={$mi,$pi,$qi,$ti,$ui,$xi};$zi=q#/io/file_fns.b#;$Ai=bless({$g5,$li,$Z5,$q,$c6,$q,$d6,$yi,$L,$zi},$m6);$Bi={};$Ci=q#atomic_update#;$Di=[];$Ei=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Fi=bless({$t,$Di,$v,$Ei,$x,690,$y,$z},$A);$Gi={$Ci,$Fi};$Hi=q#/io/file_update.b#;$Ii=bless({$g5,$Bi,$Z5,$q,$c6,$q,$d6,$Gi,$L,$Hi},$m6);$Ji={};$Ki=[];$Li=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$Mi=bless({$t,$Ki,$v,$Li,$x,692,$y,$z},$A);$Ni=q#r#;$Oi=[];$Pi=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$Qi=bless({$t,$Oi,$v,$Pi,$x,694,$y,$z},$A);$Ri=[];$Si=q#shift->r->read(@_)#;$Ti=bless({$t,$Ri,$v,$Si,$x,696,$y,$z},$A);$Ui=q#w#;$Vi=[];$Wi=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$Xi=bless({$t,$Vi,$v,$Wi,$x,698,$y,$z},$A);$Yi=[];$Zi=q#shift->w->write(@_)#;$cj=bless({$t,$Yi,$v,$Zi,$x,700,$y,$z},$A);$dj={$gh,$Mi,$Ni,$Qi,$Uc,$Ti,$Ui,$Xi,$ed,$cj};$ej=q#/io/file_io.b#;$fj=bless({$g5,$Ji,$Z5,$q,$c6,$q,$d6,$dj,$L,$ej},$m6);$gj=[$Kc,$Th,$di,$ki,$Ai,$Ii,$fj];$hj=bless({$g5,$Mh,$L,$R2,$S5,$gj},$o5);$ij=q#io/file.c::ctors#;$jj=q#ni:/io/file.c#;$kj={$o5,1};$lj=q#/io/file.c#;$mj=[$Gd];$nj=bless({$g5,$kj,$L,$lj,$S5,$mj},$T5);$oj=q#ni:/io/file_accessors.b#;$pj=q#ni:/io/file_fns.b#;$qj=q#ni:/io/file_init.b#;$rj=q#ni:/io/file_io.b#;$sj=q#ni:/io/file_readers.b#;$tj=q#ni:/io/file_update.b#;$uj=q#ni:/io/file_update_fd#;$vj={$o7,1};$wj={};$xj=[];$yj=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$zj=bless({$t,$xj,$v,$yj,$x,702,$y,$z},$A);$Aj={$c7,$zj};$Bj=q#/io/file_update_fd_init.b#;$Cj=bless({$g5,$wj,$Z5,$q,$c6,$q,$d6,$Aj,$L,$Bj},$m6);$Dj={};$Ej=[];$Fj=bless({$t,$Ej,$v,$eh,$x,704,$y,$z},$A);$Gj=[];$Hj=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Ij=bless({$t,$Gj,$v,$Hj,$x,706,$y,$z},$A);$Jj={$gh,$Ij};$Kj=q#/io/file_update_fd_gc.b#;$Lj=bless({$g5,$Dj,$Z5,$q,$c6,$Fj,$d6,$Jj,$L,$Kj},$m6);$Mj=[$Kc,$ug,$Zg,$wh,$Cj,$Lj];$Nj=bless({$g5,$vj,$L,$X2,$S5,$Mj},$p5);$Oj=q#io/file_update_fd.c::ctors#;$Pj=q#ni:/io/file_update_fd.c#;$Qj={$p5,1};$Rj=q#/io/file_update_fd.c#;$Sj=[$Gd];$Tj=bless({$g5,$Qj,$L,$Rj,$S5,$Sj},$T5);$Uj=q#ni:/io/file_update_fd_gc.b#;$Vj=q#ni:/io/file_update_fd_init.b#;$Wj=q#ni:/io/named_io_fns.b#;$Xj={};$Yj=q#fcntl#;$Zj=[];$ck=q#CORE::fcntl $_[0], $_[1], $_[2]#;$dk=bless({$t,$Zj,$v,$ck,$x,708,$y,$z},$A);$ek=[];$fk=q#CORE::fork#;$gk=bless({$t,$ek,$v,$fk,$x,710,$y,$z},$A);$hk=q#open2#;$ik=[];$jk=q#CORE::open $_[0], $_[1]#;$kk=bless({$t,$ik,$v,$jk,$x,712,$y,$z},$A);$lk=q#rename#;$mk=[];$nk=q#CORE::rename $_[0], $_[1]#;$ok=bless({$t,$mk,$v,$nk,$x,714,$y,$z},$A);$pk=q#unlink#;$qk=[];$rk=q#CORE::unlink @_#;$sk=bless({$t,$qk,$v,$rk,$x,716,$y,$z},$A);$tk=q#waitpid#;$uk=[];$vk=q#CORE::waitpid $_[0], $_[1]#;$wk=bless({$t,$uk,$v,$vk,$x,718,$y,$z},$A);$xk={$Yj,$dk,$Lf,$gk,$hk,$kk,$lk,$ok,$pk,$sk,$tk,$wk};$yk=q#/io/named_io_fns.b#;$zk=bless({$g5,$Xj,$Z5,$q,$c6,$q,$d6,$xk,$L,$yk},$m6);$Ak=q#main#;$Bk=q#ni:/io/null#;$Ck={$p7,1};$Dk=q#/io/null#;$Ek={};$Fk=[];$Gk=q#+{fd => undef}#;$Hk=bless({$t,$Fk,$v,$Gk,$x,720,$y,$z},$A);$Ik={$c7,$Hk};$Jk=q#/io/null_init.b#;$Kk=bless({$g5,$Ek,$Z5,$q,$c6,$q,$d6,$Ik,$L,$Jk},$m6);$Lk={};$Mk=[];$Nk=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$Ok=bless({$t,$Mk,$v,$Nk,$x,722,$y,$z},$A);$Pk=[];$Qk=q#shift->fd->read(@_)#;$Rk=bless({$t,$Pk,$v,$Qk,$x,724,$y,$z},$A);$Sk=[];$Tk=q#shift->fd->write(@_)#;$Uk=bless({$t,$Sk,$v,$Tk,$x,726,$y,$z},$A);$Vk={$ff,$Ok,$Uc,$Rk,$ed,$Uk};$Wk=q#/io/null_io.b#;$Xk=bless({$g5,$Lk,$Z5,$q,$c6,$q,$d6,$Vk,$L,$Wk},$m6);$Yk=[$Kc,$Kk,$Xk];$Zk=bless({$g5,$Ck,$L,$Dk,$S5,$Yk},$q5);$cl=q#io/null.c::ctors#;$dl=q#ni:/io/null.c#;$el={$q5,1};$fl=q#/io/null.c#;$gl=[$Gd];$hl=bless({$g5,$el,$L,$fl,$S5,$gl},$T5);$il=q#ni:/io/null_init.b#;$jl=q#ni:/io/null_io.b#;$kl=q#ni:/io/object#;$ll=q#ni:/io/object.c#;$ml=q#ni:/io/object.c_transfer_def.b#;$nl=q#ni:/io/object_checks.b#;$ol=q#ni:/io/object_constructors.b#;$pl=q#ni:/io/object_memory.b#;$ql=q#ni:/io/object_ops.b#;$rl=q#ni:/io/object_transfer_async.b#;$sl=q#ni:/io/object_transfer_sync.b#;$tl=q#ni:/io/pid#;$ul={$r7,1};$vl={};$wl=q#pid#;$xl=[];$yl=q#shift->{'pid'}#;$zl=bless({$t,$xl,$v,$yl,$x,728,$y,$z},$A);$Al=q#status#;$Bl=[];$Cl=q#shift->{'status'}#;$Dl=bless({$t,$Bl,$v,$Cl,$x,730,$y,$z},$A);$El={$wl,$zl,$Al,$Dl};$Fl=q#/io/pid_readers.b#;$Gl=bless({$g5,$vl,$Z5,$q,$c6,$q,$d6,$El,$L,$Fl},$m6);$Hl={};$Il=[];$Jl=q#shift->await#;$Kl=bless({$t,$Il,$v,$Jl,$x,732,$y,$z},$A);$Ll=[];$Ml=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$Nl=bless({$t,$Ll,$v,$Ml,$x,734,$y,$z},$A);$Ol={$c7,$Nl};$Pl=q#/io/pid_init.b#;$Ql=bless({$g5,$Hl,$Z5,$q,$c6,$Kl,$d6,$Ol,$L,$Pl},$m6);$Rl={};$Sl=q#await#;$Tl=[];$Ul=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$Vl=bless({$t,$Tl,$v,$Ul,$x,736,$y,$z},$A);$Wl=q#running#;$Xl=[];$Yl=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$Zl=bless({$t,$Xl,$v,$Yl,$x,738,$y,$z},$A);$cm={$Sl,$Vl,$Wl,$Zl};$dm=q#/io/pid_wait.b#;$em=bless({$g5,$Rl,$Z5,$q,$c6,$q,$d6,$cm,$L,$dm},$m6);$fm={};$gm=[];$hm=q#shift->stdout->read(@_)#;$im=bless({$t,$gm,$v,$hm,$x,740,$y,$z},$A);$jm=[];$km=q#shift->stdin->write(@_)#;$lm=bless({$t,$jm,$v,$km,$x,742,$y,$z},$A);$mm={$Uc,$im,$ed,$lm};$nm=q#/io/pid_io.b#;$om=bless({$g5,$fm,$Z5,$q,$c6,$q,$d6,$mm,$L,$nm},$m6);$pm={};$qm=[];$rm=q#$_[0]->{external_fds}{$_[1]}#;$sm=bless({$t,$qm,$v,$rm,$x,744,$y,$z},$A);$tm=[];$um=q#shift->fd(2)#;$vm=bless({$t,$tm,$v,$um,$x,746,$y,$z},$A);$wm=[];$xm=q#shift->fd(0)#;$ym=bless({$t,$wm,$v,$xm,$x,748,$y,$z},$A);$zm=[];$Am=q#shift->fd(1)#;$Bm=bless({$t,$zm,$v,$Am,$x,750,$y,$z},$A);$Cm={$ff,$sm,$jf,$vm,$nf,$ym,$rf,$Bm};$Dm=q#/io/pid_accessors.b#;$Em=bless({$g5,$pm,$Z5,$q,$c6,$q,$d6,$Cm,$L,$Dm},$m6);$Fm=[$Kc,$Gl,$Ql,$em,$om,$Em];$Gm=bless({$g5,$ul,$L,$w3,$S5,$Fm},$s5);$Hm=q#io/pid.c::ctors#;$Im=q#ni:/io/pid.c#;$Jm={$s5,1};$Km=q#/io/pid.c#;$Lm=[$Gd];$Mm=bless({$g5,$Jm,$L,$Km,$S5,$Lm},$T5);$Nm=q#ni:/io/pid_accessors.b#;$Om=q#ni:/io/pid_init.b#;$Pm=q#ni:/io/pid_io.b#;$Qm=q#ni:/io/pid_readers.b#;$Rm=q#ni:/io/pid_wait.b#;$Sm=q#ni:/io/str#;$Tm={$s7,1};$Um=q#/io/str#;$Vm={};$Wm=q#data#;$Xm=[];$Ym=q#shift->{'data'}#;$Zm=bless({$t,$Xm,$v,$Ym,$x,752,$y,$z},$A);$cn=q#end#;$dn=[];$en=q#shift->{'end'}#;$fn=bless({$t,$dn,$v,$en,$x,754,$y,$z},$A);$gn=q#start#;$hn=[];$in=q#shift->{'start'}#;$jn=bless({$t,$hn,$v,$in,$x,756,$y,$z},$A);$kn={$Wm,$Zm,$cn,$fn,$gn,$jn};$ln=q#/io/str_ro.b#;$mn=bless({$g5,$Vm,$Z5,$q,$c6,$q,$d6,$kn,$L,$ln},$m6);$nn={};$on=[];$pn=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$qn=bless({$t,$on,$v,$pn,$x,758,$y,$z},$A);$rn={$c7,$qn};$sn=q#/io/str_init.b#;$tn=bless({$g5,$nn,$Z5,$q,$c6,$q,$d6,$rn,$L,$sn},$m6);$un={};$vn=[];$wn=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$xn=bless({$t,$vn,$v,$wn,$x,760,$y,$z},$A);$yn=q#remaining#;$zn=[];$An=q#my $self = shift; $$self{end} - $$self{start}#;$Bn=bless({$t,$zn,$v,$An,$x,762,$y,$z},$A);$Cn=[];$Dn=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$En=bless({$t,$Cn,$v,$Dn,$x,764,$y,$z},$A);$Fn={$Uc,$xn,$yn,$Bn,$ed,$En};$Gn=q#/io/str_io.b#;$Hn=bless({$g5,$un,$Z5,$q,$c6,$q,$d6,$Fn,$L,$Gn},$m6);$In=[$Kc,$mn,$tn,$Hn];$Jn=bless({$g5,$Tm,$L,$Um,$S5,$In},$t5);$Kn=q#io/str.c::ctors#;$Ln=q#ni:/io/str.c#;$Mn={$t5,1};$Nn=q#/io/str.c#;$On=[$Gd];$Pn=bless({$g5,$Mn,$L,$Nn,$S5,$On},$T5);$Qn=q#ni:/io/str_init.b#;$Rn=q#ni:/io/str_io.b#;$Sn=q#ni:/io/str_ro.b#;$Tn=q#ni:/io/transfer#;$Un={$t7,1,$u7,1,$v7,1};$Vn=q#/io/transfer#;$Wn={$t7,1,$u7,1,$v7,1,$F7,1};$Xn=q#/semantic/task#;$Yn={};$Zn=[];$co=bless({$t,$Zn,$v,$Q9,$x,766,$y,$z},$A);$do={$r,$co};$eo=q#/semantic/task_ro.b#;$fo=bless({$g5,$Yn,$Z5,$q,$c6,$q,$d6,$do,$L,$eo},$m6);$go={};$ho=q#failure#;$io=[];$jo=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$ko=bless({$t,$io,$v,$jo,$x,768,$y,$z},$A);$lo=q#success#;$mo=[];$no=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$oo=bless({$t,$mo,$v,$no,$x,770,$y,$z},$A);$po={$ho,$ko,$lo,$oo};$qo=q#/semantic/task_outcome.b#;$ro=bless({$g5,$go,$Z5,$q,$c6,$q,$d6,$po,$L,$qo},$m6);$so=[$S7,$fo,$ro];$to=bless({$g5,$Wn,$L,$Xn,$S5,$so},$Q5);$uo=q#semantic/task.c::ctors#;$vo={};$wo=[];$xo=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$yo=bless({$t,$wo,$v,$xo,$x,772,$y,$z},$A);$zo=[];$Ao=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Bo=bless({$t,$zo,$v,$Ao,$x,774,$y,$z},$A);$Co=[];$Do=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Eo=bless({$t,$Co,$v,$Do,$x,776,$y,$z},$A);$Fo={$Uc,$Bo,$ed,$Eo};$Go=q#/io/transfer_io_interop.b#;$Ho=bless({$g5,$vo,$Z5,$yo,$c6,$q,$d6,$Fo,$L,$Go},$m6);$Io={};$Jo=q#pressure#;$Ko=[];$Lo=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$Mo=bless({$t,$Ko,$v,$Lo,$x,778,$y,$z},$A);$No=q#read_limit_throughput#;$Oo=[];$Po=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$Qo=bless({$t,$Oo,$v,$Po,$x,780,$y,$z},$A);$Ro=q#throughput#;$So=[];$To=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$Uo=bless({$t,$So,$v,$To,$x,782,$y,$z},$A);$Vo=q#write_limit_throughput#;$Wo=[];$Xo=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$Yo=bless({$t,$Wo,$v,$Xo,$x,784,$y,$z},$A);$Zo={$Jo,$Mo,$No,$Qo,$Ro,$Uo,$Vo,$Yo};$cp=q#/io/transfer_io_measurement.b#;$dp=bless({$g5,$Io,$Z5,$q,$c6,$q,$d6,$Zo,$L,$cp},$m6);$ep=[$to,$Ho,$dp];$fp=bless({$g5,$Un,$L,$Vn,$S5,$ep},$u5);$gp=q#io/transfer.c::ctors#;$hp=q#ni:/io/transfer.c#;$ip={$u5,1,$v5,1,$w5,1};$jp=q#/io/transfer.c#;$kp={$u5,1,$v5,1,$w5,1,$Q5,1};$lp=q#/semantic/task.c#;$mp=[$C9];$np=bless({$g5,$kp,$L,$lp,$S5,$mp},$T5);$op={};$pp=[];$qp=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$rp=bless({$t,$pp,$v,$qp,$x,788,$y,$z},$A);$sp={};$tp=q#/io/transfer.c_into.b#;$up=bless({$g5,$op,$Z5,$rp,$c6,$q,$d6,$sp,$L,$tp},$m6);$vp=[$np,$up];$wp=bless({$g5,$ip,$L,$jp,$S5,$vp},$T5);$xp=q#ni:/io/transfer.c_into.b#;$yp=q#ni:/io/transfer_async#;$zp={$u7,1};$Ap=q#/io/transfer_async#;$Bp={};$Cp=q#dest_io#;$Dp=[];$Ep=q#shift->{'dest_io'}#;$Fp=bless({$t,$Dp,$v,$Ep,$x,790,$y,$z},$A);$Gp=q#id#;$Hp=[];$Ip=q#shift->{'id'}#;$Jp=bless({$t,$Hp,$v,$Ip,$x,792,$y,$z},$A);$Kp=q#source_io#;$Lp=[];$Mp=q#shift->{'source_io'}#;$Np=bless({$t,$Lp,$v,$Mp,$x,794,$y,$z},$A);$Op={$Cp,$Fp,$Gp,$Jp,$Kp,$Np};$Pp=q#/io/transfer_async_ro.b#;$Qp=bless({$g5,$Bp,$Z5,$q,$c6,$q,$d6,$Op,$L,$Pp},$m6);$Rp={};$Sp=[];$Tp=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$Up=bless({$t,$Sp,$v,$Tp,$x,796,$y,$z},$A);$Vp={$c7,$Up};$Wp=q#/io/transfer_async_init.b#;$Xp=bless({$g5,$Rp,$Z5,$q,$c6,$q,$d6,$Vp,$L,$Wp},$m6);$Yp={};$Zp=[];$cq=q#ni('ni:/io/transfer_async')->track(shift)#;$dq=bless({$t,$Zp,$v,$cq,$x,798,$y,$z},$A);$eq=[];$fq=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$gq=bless({$t,$eq,$v,$fq,$x,800,$y,$z},$A);$hq={};$iq=q#/io/transfer_async_lifecycle.b#;$jq=bless({$g5,$Yp,$Z5,$dq,$c6,$gq,$d6,$hq,$L,$iq},$m6);$kq={};$lq=q#run#;$mq=[];$nq=q#shift#;$oq=bless({$t,$mq,$v,$nq,$x,802,$y,$z},$A);$pq=q#run_async#;$qq=[];$rq=q#my $self = shift;
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

$self;#;$sq=bless({$t,$qq,$v,$rq,$x,804,$y,$z},$A);$tq={$lq,$oq,$pq,$sq};$uq=q#/io/transfer_async_run.b#;$vq=bless({$g5,$kq,$Z5,$q,$c6,$q,$d6,$tq,$L,$uq},$m6);$wq=[$fp,$Qp,$Xp,$jq,$vq];$xq=q#tracked_transfers#;$yq={};$zq=q#transfer_id#;$Aq=bless({$g5,$zp,$L,$Ap,$S5,$wq,$xq,$yq,$zq,0},$v5);$Bq=q#io/transfer_async.c::ctors#;$Cq=q#ni:/io/transfer_async.c#;$Dq={$v5,1};$Eq=q#/io/transfer_async.c#;$Fq={};$Gq=[];$Hq=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Iq=bless({$t,$Gq,$v,$Hq,$x,814,$y,$z},$A);$Jq=q#new_id#;$Kq=[];$Lq=q#++shift->{transfer_id}#;$Mq=bless({$t,$Kq,$v,$Lq,$x,816,$y,$z},$A);$Nq=q#track#;$Oq=[];$Pq=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$Qq=bless({$t,$Oq,$v,$Pq,$x,818,$y,$z},$A);$Rq=q#untrack#;$Sq=[];$Tq=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$Uq=bless({$t,$Sq,$v,$Tq,$x,820,$y,$z},$A);$Vq={$Jq,$Mq,$Nq,$Qq,$Rq,$Uq};$Wq=q#/io/transfer_async.c_tracker.b#;$Xq=bless({$g5,$Fq,$Z5,$Iq,$c6,$q,$d6,$Vq,$L,$Wq},$m6);$Yq=[$wp,$Xq];$Zq=bless({$g5,$Dq,$L,$Eq,$S5,$Yq},$T5);$cr=q#ni:/io/transfer_async.c_tracker.b#;$dr=q#ni:/io/transfer_async_init.b#;$er=q#ni:/io/transfer_async_lifecycle.b#;$fr=q#ni:/io/transfer_async_ro.b#;$gr=q#ni:/io/transfer_async_run.b#;$hr=q#ni:/io/transfer_io_interop.b#;$ir=q#ni:/io/transfer_io_measurement.b#;$jr=q#ni:/io/transfer_sync#;$kr={$v7,1};$lr=q#/io/transfer_sync#;$mr={};$nr=[];$or=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$pr=bless({$t,$nr,$v,$or,$x,822,$y,$z},$A);$qr={$c7,$pr};$rr=q#/io/transfer_sync_init.b#;$sr=bless({$g5,$mr,$Z5,$q,$c6,$q,$d6,$qr,$L,$rr},$m6);$tr={};$ur=[];$vr=q#my $self = shift;
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
$self->success;#;$wr=bless({$t,$ur,$v,$vr,$x,824,$y,$z},$A);$xr={$lq,$wr};$yr=q#/io/transfer_sync_run.b#;$zr=bless({$g5,$tr,$Z5,$q,$c6,$q,$d6,$xr,$L,$yr},$m6);$Ar=[$fp,$sr,$zr];$Br=bless({$g5,$kr,$L,$lr,$S5,$Ar},$w5);$Cr=q#io/transfer_sync.c::ctors#;$Dr=q#ni:/io/transfer_sync.c#;$Er={$w5,1};$Fr=q#/io/transfer_sync.c#;$Gr=[$wp];$Hr=bless({$g5,$Er,$L,$Fr,$S5,$Gr},$T5);$Ir=q#ni:/io/transfer_sync_init.b#;$Jr=q#ni:/io/transfer_sync_run.b#;$Kr=q#ni:/lib/accessor.b#;$Lr=q#ni:/lib/behavior#;$Mr=q#ni:/lib/behavior.c#;$Nr=q#ni:/lib/branch#;$Or={$o6,1};$Pr=q#/lib/branch#;$Qr={};$Rr=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$Sr=bless({$v,$Rr,$x,830,$y,$z},$A);$Tr={$c7,$Sr};$Ur=q#/lib/branch_init.b#;$Vr=bless({$g5,$Qr,$Z5,$q,$c6,$q,$d6,$Tr,$L,$Ur},$m6);$Wr=[$f8,$x6,$n6,$Vr,$W8];$Xr=bless({$g5,$Or,$L,$Pr,$S5,$Wr},$y5);$Yr=q#lib/branch.c::ctors#;$Zr=q#ni:/lib/branch.b#;$cs=q#ni:/lib/branch.c#;$ds={$y5,1};$es=q#/lib/branch.c#;$fs=[$H9];$gs=bless({$g5,$ds,$L,$es,$S5,$fs},$T5);$hs=q#ni:/lib/branch_init.b#;$is=q#ni:/lib/class_init.b#;$js=q#ni:/lib/dataslice#;$ks={$x7,1};$ls=q#/lib/dataslice#;$ms={};$ns=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$os=bless({$v,$ns,$x,832,$y,$z},$A);$ps={$c7,$os};$qs=q#/lib/dataslice_init.b#;$rs=bless({$g5,$ms,$Z5,$q,$c6,$q,$d6,$ps,$L,$qs},$m6);$ss={};$ts=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$us=bless({$v,$ts,$x,834,$y,$z},$A);$vs={$h6,$us};$ws=q#/lib/dataslice_apply.b#;$xs=bless({$g5,$ss,$Z5,$q,$c6,$q,$d6,$vs,$L,$ws},$m6);$ys=[$f8,$rs,$xs];$zs=bless({$g5,$ks,$L,$ls,$S5,$ys},$z5);$As=q#lib/dataslice.c::ctors#;$Bs=q#ni:/lib/dataslice.c#;$Cs={$z5,1};$Ds=q#/lib/dataslice.c#;$Es=[$H9];$Fs=bless({$g5,$Cs,$L,$Ds,$S5,$Es},$T5);$Gs=q#ni:/lib/dataslice_apply.b#;$Hs=q#ni:/lib/dataslice_init.b#;$Is=q#ni:/lib/definition.b#;$Js=q#ni:/lib/definition_def.b#;$Ks=q#ni:/lib/definition_defdata.b#;$Ls=q#ni:/lib/doc#;$Ms={$N,1};$Ns={};$Os=q#shift; +{name => shift, doc => []}#;$Ps=bless({$v,$Os,$x,836,$y,$z},$A);$Qs={$c7,$Ps};$Rs=q#/lib/doc_init.b#;$Ss=bless({$g5,$Ns,$Z5,$q,$c6,$q,$d6,$Qs,$L,$Rs},$m6);$Ts={};$Us=q#'ni.doc'#;$Vs=bless({$v,$Us,$x,838,$y,$z},$A);$Ws={$A6,$Vs};$Xs=q#/lib/doc_namespace.b#;$Ys=bless({$g5,$Ts,$Z5,$q,$c6,$q,$d6,$Ws,$L,$Xs},$m6);$Zs={};$ct=q#AUTOLOAD#;$dt=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;#;$et=bless({$v,$dt,$x,840,$y,$z},$A);$ft={$ct,$et};$gt=q#/lib/doc_define.b#;$ht=bless({$g5,$Zs,$Z5,$q,$c6,$q,$d6,$ft,$L,$gt},$m6);$it={};$jt=q#shift->referent#;$kt=bless({$v,$jt,$x,842,$y,$z},$A);$lt=q#referent#;$mt=q#ni 'ni:' . shift->{name}#;$nt=bless({$v,$mt,$x,844,$y,$z},$A);$ot={$cn,$kt,$lt,$nt};$pt=q#/lib/doc_end.b#;$qt=bless({$g5,$it,$Z5,$q,$c6,$q,$d6,$ot,$L,$pt},$m6);$rt={};$st=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$tt=bless({$v,$st,$x,846,$y,$z},$A);$ut=q#linearized#;$vt=q#map @$_, @{shift->{doc}}#;$wt=bless({$v,$vt,$x,848,$y,$z},$A);$xt=q#tests#;$yt=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$zt=bless({$v,$yt,$x,850,$y,$z},$A);$At={$c3,$tt,$ut,$wt,$xt,$zt};$Bt=q#/lib/doc_test.b#;$Ct=bless({$g5,$rt,$Z5,$q,$c6,$q,$d6,$At,$L,$Bt},$m6);$Dt=[$S7,$x6,$Ss,$Ys,$ht,$qt,$Ct];$Et=bless({$g5,$Ms,$L,$X3,$S5,$Dt},$A5);$Ft=q#lib/doc.c::ctors#;$Gt=q#ni:/lib/doc.c#;$Ht={$A5,1};$It=q#/lib/doc.c#;$Jt=[$C9];$Kt=bless({$g5,$Ht,$L,$It,$S5,$Jt},$T5);$Lt=q#ni:/lib/doc_define.b#;$Mt=q#ni:/lib/doc_end.b#;$Nt=q#ni:/lib/doc_init.b#;$Ot=q#ni:/lib/doc_namespace.b#;$Pt=q#ni:/lib/doc_test.b#;$Qt=q#ni:/lib/documentable.b#;$Rt=q#ni:/lib/fn#;$St={$A,1};$Tt=q#/lib/fn#;$Ut={};$Vt=q#shift->compile#;$Wt=bless({$v,$Vt,$x,852,$y,$z},$A);$Xt=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$Yt=bless({$v,$Xt,$x,854,$y,$z},$A);$Zt=q#compile#;$cu=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$du=bless({$v,$cu,$x,856,$y,$z},$A);$eu=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$fu=bless({$v,$eu,$x,858,$y,$z},$A);$gu={$Zt,$du,$c7,$fu};$hu=q#/lib/fn_init.b#;$iu=bless({$g5,$Ut,$Z5,$Wt,$c6,$Yt,$d6,$gu,$L,$hu},$m6);$ju={};$ku=[];$lu=q#shift->{'annotations'}#;$mu=bless({$t,$ku,$v,$lu,$x,860,$y,$z},$A);$nu=[];$ou=q#shift->{'code'}#;$pu=bless({$t,$nu,$v,$ou,$x,862,$y,$z},$A);$qu=[];$ru=q#shift->{'eval_number'}#;$su=bless({$t,$qu,$v,$ru,$x,864,$y,$z},$A);$tu=q#fn#;$uu=[];$vu=q#shift->{'fn'}#;$wu=bless({$t,$uu,$v,$vu,$x,866,$y,$z},$A);$xu={$t,$mu,$v,$pu,$x,$su,$tu,$wu};$yu=q#/lib/fn_ro.b#;$zu=bless({$g5,$ju,$Z5,$q,$c6,$q,$d6,$xu,$L,$yu},$m6);$Au={};$Bu=[];$Cu=q#my $self = shift; "fn {$$self{code}}"#;$Du=bless({$t,$Bu,$v,$Cu,$x,868,$y,$z},$A);$Eu=[];$Fu=bless({$t,$Eu,$v,$J8,$x,870,$y,$z},$A);$Gu={$B8,$Du,$I8,$Fu};$Hu=q#/lib/fn_ops.b#;$Iu=bless({$g5,$Au,$Z5,$q,$c6,$q,$d6,$Gu,$L,$Hu},$m6);$Ju={};$Ku=q#serialize#;$Lu=[];$Mu=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$Nu=bless({$t,$Lu,$v,$Mu,$x,872,$y,$z},$A);$Ou={$Ku,$Nu};$Pu=q#/lib/fn_serialize.b#;$Qu=bless({$g5,$Ju,$Z5,$q,$c6,$q,$d6,$Ou,$L,$Pu},$m6);$Ru=[$S7,$j9,$iu,$zu,$Iu,$Qu];$Su=bless({$g5,$St,$L,$Tt,$S5,$Ru},$B5);$Tu=q#lib/fn.c::ctors#;$Uu=q#ni:/lib/fn.c#;$Vu={$B5,1};$Wu=q#/lib/fn.c#;$Xu={};$Yu=[];$Zu=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$cv=bless({$t,$Yu,$v,$Zu,$x,876,$y,$z},$A);$dv=q#resolve_evals#;$ev=[];$fv=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$gv=bless({$t,$ev,$v,$fv,$x,878,$y,$z},$A);$hv={$dv,$gv};$iv=q#/lib/fn.c_resolve_eval.b#;$jv=bless({$g5,$Xu,$Z5,$cv,$c6,$q,$d6,$hv,$L,$iv},$m6);$kv=[$C9,$jv];$lv=bless({$g5,$Vu,$L,$Wu,$S5,$kv},$T5);$mv=q#ni:/lib/fn.c_resolve_eval.b#;$nv=q#ni:/lib/fn_init.b#;$ov=q#ni:/lib/fn_ops.b#;$pv=q#ni:/lib/fn_ro.b#;$qv=q#ni:/lib/fn_serialize.b#;$rv=q#ni:/lib/gensym_generator_compact.b#;$sv={};$tv=q#gensym#;$uv=[];$vv=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$wv=bless({$t,$uv,$v,$vv,$x,880,$y,$z},$A);$xv={$tv,$wv};$yv=q#/lib/gensym_generator_compact.b#;$zv=bless({$g5,$sv,$Z5,$q,$c6,$q,$d6,$xv,$L,$yv},$m6);$Av=q#ni:/lib/global_static_test.b#;$Bv={};$Cv=[];$Dv=q#ni('ni:/lib/test_case')->new(shift)#;$Ev=q#($)#;$Fv=bless({$t,$Cv,$v,$Dv,$x,882,$y,$Ev},$A);$Gv=q#now#;$Hv=[];$Iv=q#ni('ni:/lib/test_value')->new(shift)#;$Jv=bless({$t,$Hv,$v,$Iv,$x,884,$y,$Ev},$A);$Kv={$c3,$Fv,$Gv,$Jv};$Lv=q#/lib/global_static_test.b#;$Mv=bless({$g5,$Bv,$Z5,$q,$c6,$q,$d6,$Kv,$L,$Lv},$m6);$Nv=q#ni:/lib/image#;$Ov={$y7,1};$Pv={};$Qv=[];$Rv=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$Sv=bless({$t,$Qv,$v,$Rv,$x,886,$y,$z},$A);$Tv={$c7,$Sv};$Uv=q#/lib/image_init.b#;$Vv=bless({$g5,$Pv,$Z5,$q,$c6,$q,$d6,$Tv,$L,$Uv},$m6);$Wv={};$Xv=q#address#;$Yv=[];$Zv=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$cw=bless({$t,$Yv,$v,$Zv,$x,888,$y,$z},$A);$dw=q#allocate_gensym#;$ew=[];$fw=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$gw=bless({$t,$ew,$v,$fw,$x,890,$y,$z},$A);$hw=q#boot_side_effect#;$iw=[];$jw=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$kw=bless({$t,$iw,$v,$jw,$x,892,$y,$z},$A);$lw=q#circular_links#;$mw=[];$nw=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$ow=bless({$t,$mw,$v,$nw,$x,894,$y,$z},$A);$pw=q#finalizer#;$qw=[];$rw=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$sw=bless({$t,$qw,$v,$rw,$x,896,$y,$z},$A);$tw=q#io#;$uw=[];$vw=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$ww=bless({$t,$uw,$v,$vw,$x,898,$y,$z},$A);$xw=q#quote#;$yw=[];$zw=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Aw=bless({$t,$yw,$v,$zw,$x,900,$y,$z},$A);$Bw=q#reconstruction#;$Cw=[];$Dw=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Ew=bless({$t,$Cw,$v,$Dw,$x,902,$y,$z},$A);$Fw=q#side_effect#;$Gw=[];$Hw=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Iw=bless({$t,$Gw,$v,$Hw,$x,904,$y,$z},$A);$Jw={$Xv,$cw,$dw,$gw,$hw,$kw,$lw,$ow,$pw,$sw,$tw,$ww,$xw,$Aw,$Bw,$Ew,$Fw,$Iw};$Kw=q#/lib/image_quoting.b#;$Lw=bless({$g5,$Wv,$Z5,$q,$c6,$q,$d6,$Jw,$L,$Kw},$m6);$Mw={};$Nw=q#quote_code#;$Ow=[];$Pw=q#shift->die('cannot quote perl CODE refs', shift)#;$Qw=bless({$t,$Ow,$v,$Pw,$x,906,$y,$z},$A);$Rw={$Nw,$Qw};$Sw=q#/lib/quote_code_fail.b#;$Tw=bless({$g5,$Mw,$Z5,$q,$c6,$q,$d6,$Rw,$L,$Sw},$m6);$Uw={};$Vw=q#quote_array#;$Ww=[];$Xw=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$Yw=bless({$t,$Ww,$v,$Xw,$x,908,$y,$z},$A);$Zw=q#quote_hash#;$cx=[];$dx=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$ex=bless({$t,$cx,$v,$dx,$x,910,$y,$z},$A);$fx=q#quote_scalar#;$gx=[];$hx=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$ix=bless({$t,$gx,$v,$hx,$x,912,$y,$z},$A);$jx=q#quote_scalar_ref#;$kx=[];$lx=q#'\\\\' . shift->quote(${$_[0]})#;$mx=bless({$t,$kx,$v,$lx,$x,914,$y,$z},$A);$nx=q#quote_value#;$ox=[];$px=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$qx=bless({$t,$ox,$v,$px,$x,916,$y,$z},$A);$rx={$Vw,$Yw,$Zw,$ex,$fx,$ix,$jx,$mx,$nx,$qx};$sx=q#/lib/quote_values.b#;$tx=bless({$g5,$Uw,$Z5,$q,$c6,$q,$d6,$rx,$L,$sx},$m6);$ux={};$vx=q#quote_blessed#;$wx=[];$xx=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$yx=bless({$t,$wx,$v,$xx,$x,918,$y,$z},$A);$zx=q#quote_class#;$Ax=[];$Bx=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");#;$Cx=bless({$t,$Ax,$v,$Bx,$x,920,$y,$z},$A);$Dx=q#quote_object#;$Ex=[];$Fx=q#my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . '::ctors') . ';')
  if @{ref($_[0]) . '::ctors'};
$q;#;$Gx=bless({$t,$Ex,$v,$Fx,$x,922,$y,$z},$A);$Hx={$vx,$yx,$zx,$Cx,$Dx,$Gx};$Ix=q#/lib/quote_objects.b#;$Jx=bless({$g5,$ux,$Z5,$q,$c6,$q,$d6,$Hx,$L,$Ix},$m6);$Kx={};$Lx=q#circular_arrayref#;$Mx=[];$Nx=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$Ox=bless({$t,$Mx,$v,$Nx,$x,924,$y,$z},$A);$Px=q#circular_hashref#;$Qx=[];$Rx=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$Sx=bless({$t,$Qx,$v,$Rx,$x,926,$y,$z},$A);$Tx=q#is_circular#;$Ux=[];$Vx=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$Wx=bless({$t,$Ux,$v,$Vx,$x,928,$y,$z},$A);$Xx={$Lx,$Ox,$Px,$Sx,$Tx,$Wx};$Yx=q#/lib/quote_circular_addressed.b#;$Zx=bless({$g5,$Kx,$Z5,$q,$c6,$q,$d6,$Xx,$L,$Yx},$m6);$cy=[$S7,$Vv,$Lw,$Tw,$tx,$Jx,$Zx,$zv];$dy=bless({$g5,$Ov,$L,$h4,$S5,$cy},$C5);$ey=q#lib/image.c::ctors#;$fy=q#ni:/lib/image.c#;$gy={$C5,1};$hy=q#/lib/image.c#;$iy=[$C9];$jy=bless({$g5,$gy,$L,$hy,$S5,$iy},$T5);$ky=q#ni:/lib/image_init.b#;$ly=q#ni:/lib/image_quoting.b#;$my=q#ni:/lib/instance.b#;$ny=q#ni:/lib/instantiable.b#;$oy=q#ni:/lib/json.b#;$py={};$qy=q#json_decode#;$ry=[];$sy=q#local $_;
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
wantarray ? @$r : $$r[0];#;$ty=bless({$t,$ry,$v,$sy,$x,930,$y,$Ev},$A);$uy=q#json_encode#;$vy=[];$wy=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$xy=bless({$t,$vy,$v,$wy,$x,932,$y,$Ev},$A);$yy=q#json_escape#;$zy=[];$Ay=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$By=bless({$t,$zy,$v,$Ay,$x,934,$y,$Ev},$A);$Cy=q#json_unescape#;$Dy=[];$Ey=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$Fy=bless({$t,$Dy,$v,$Ey,$x,936,$y,$Ev},$A);$Gy=q#json_unescape_one#;$Hy=[];$Iy=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$Jy=bless({$t,$Hy,$v,$Iy,$x,938,$y,$Ev},$A);$Ky={$qy,$ty,$uy,$xy,$yy,$By,$Cy,$Fy,$Gy,$Jy};$Ly=q#/lib/json.b#;$My=bless({$g5,$py,$Z5,$q,$c6,$q,$d6,$Ky,$L,$Ly},$m6);$Ny=q#ni#;$Oy=q#ni:/lib/name_as_string.b#;$Py=q#ni:/lib/named.b#;$Qy=q#ni:/lib/named_in_ni.b#;$Ry=q#ni:/lib/namespaced.b#;$Sy=q#ni:/lib/ni#;$Ty={$z7,1};$Uy={};$Vy=q#extend#;$Wy=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$Xy=bless({$v,$Wy,$x,940,$y,$z},$A);$Yy=q#is_mutable#;$Zy=q#$0 ne '-' && -w $0#;$cz=bless({$v,$Zy,$x,942,$y,$z},$A);$dz=q#modify#;$ez=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$fz=bless({$v,$ez,$x,944,$y,$z},$A);$gz={$Vy,$Xy,$Yy,$cz,$dz,$fz};$hz=q#/lib/ni_self.b#;$iz=bless({$g5,$Uy,$Z5,$q,$c6,$q,$d6,$gz,$L,$hz},$m6);$jz={};$kz=q#--internal/+=#;$lz=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted(use_newlines => 1);
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$mz=bless({$v,$lz,$x,946,$y,$z},$A);$nz=q#--internal/eval#;$oz=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$pz=bless({$v,$oz,$x,948,$y,$z},$A);$qz=q#--internal/image#;$rz=q#shift->quoted(use_newlines => 1)->io->into_sync(ni"fd:1");
0;#;$sz=bless({$v,$rz,$x,950,$y,$z},$A);$tz=q#--internal/test#;$uz=q#my $self   = shift;
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
!!$failed;#;$vz=bless({$v,$uz,$x,952,$y,$z},$A);$wz=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$xz=bless({$v,$wz,$x,954,$y,$z},$A);$yz={$kz,$mz,$nz,$pz,$qz,$sz,$tz,$vz,$lq,$xz};$zz=q#/lib/ni_main.b#;$Az=bless({$g5,$jz,$Z5,$q,$c6,$q,$d6,$yz,$L,$zz},$m6);$Bz={};$Cz=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$Dz=bless({$v,$Cz,$x,956,$y,$z},$A);$Ez=q#resolver_for#;$Fz=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$Gz=bless({$v,$Fz,$x,958,$y,$z},$A);$Hz={$O6,$Dz,$Ez,$Gz};$Iz=q#/lib/ni_resolver.b#;$Jz=bless({$g5,$Bz,$Z5,$q,$c6,$q,$d6,$Hz,$L,$Iz},$m6);$Kz={};$Lz=q#exists#;$Mz=q#exists $_[0]->{named}{$_[1]}#;$Nz=bless({$v,$Mz,$x,960,$y,$z},$A);$Oz=q#quoted#;$Pz=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$Qz=bless({$v,$Pz,$x,962,$y,$z},$A);$Rz={$Lz,$Nz,$Oz,$Qz};$Sz=q#/lib/ni_image.b#;$Tz=bless({$g5,$Kz,$Z5,$q,$c6,$q,$d6,$Rz,$L,$Sz},$m6);$Uz=[$S7,$iz,$Az,$Jz,$Tz];$Vz=bless({$g5,$Ty,$L,$p4,$S5,$Uz},$D5);$Wz=q#lib/ni.c::ctors#;$Xz=q#ni:/lib/ni.c#;$Yz={$D5,1};$Zz=q#/lib/ni.c#;$cA=[$C9];$dA=bless({$g5,$Yz,$L,$Zz,$S5,$cA},$T5);$eA=q#ni:/lib/ni_image.b#;$fA=q#ni:/lib/ni_main.b#;$gA=q#ni:/lib/ni_resolver.b#;$hA=q#ni:/lib/ni_self.b#;$iA=q#ni:/lib/ni_static_util.b#;$jA={};$kA=q#abbrev#;$lA=[];$mA=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$nA=bless({$t,$lA,$v,$mA,$x,964,$y,$z},$A);$oA=q#dor#;$pA=[];$qA=q#defined $_[0] ? $_[0] : $_[1]#;$rA=bless({$t,$pA,$v,$qA,$x,966,$y,$z},$A);$sA=q#indent#;$tA=[];$uA=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$vA=bless({$t,$tA,$v,$uA,$x,968,$y,$z},$A);$wA=q#max#;$xA=[];$yA=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$zA=bless({$t,$xA,$v,$yA,$x,970,$y,$z},$A);$AA=q#maxstr#;$BA=[];$CA=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$DA=bless({$t,$BA,$v,$CA,$x,972,$y,$z},$A);$EA=q#mean#;$FA=[];$GA=q#sum(@_) / (@_ || 1)#;$HA=bless({$t,$FA,$v,$GA,$x,974,$y,$z},$A);$IA=q#min#;$JA=[];$KA=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$LA=bless({$t,$JA,$v,$KA,$x,976,$y,$z},$A);$MA=q#minstr#;$NA=[];$OA=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$PA=bless({$t,$NA,$v,$OA,$x,978,$y,$z},$A);$QA=q#sgr#;$RA=[];$SA=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$TA=bless({$t,$RA,$v,$SA,$x,980,$y,$z},$A);$UA=q#sr#;$VA=[];$WA=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$XA=bless({$t,$VA,$v,$WA,$x,982,$y,$z},$A);$YA=q#sum#;$ZA=[];$cB=q#local $_; my $x = 0; $x += $_ for @_; $x#;$dB=bless({$t,$ZA,$v,$cB,$x,984,$y,$z},$A);$eB=q#swap#;$fB=[];$gB=q#@_[0, 1] = @_[1, 0]#;$hB=bless({$t,$fB,$v,$gB,$x,986,$y,$z},$A);$iB={$kA,$nA,$oA,$rA,$sA,$vA,$wA,$zA,$AA,$DA,$EA,$HA,$IA,$LA,$MA,$PA,$QA,$TA,$UA,$XA,$YA,$dB,$eB,$hB};$jB=q#/lib/ni_static_util.b#;$kB=bless({$g5,$jA,$Z5,$q,$c6,$q,$d6,$iB,$L,$jB},$m6);$lB=q#ni:/lib/perlbranch.b#;$mB=q#ni:/lib/quote_circular_addressed.b#;$nB=q#ni:/lib/quote_code_fail.b#;$oB=q#ni:/lib/quote_objects.b#;$pB=q#ni:/lib/quote_simple#;$qB={$A7,1};$rB={};$sB=[];$tB=q#+{}#;$uB=bless({$t,$sB,$v,$tB,$x,988,$y,$z},$A);$vB={$c7,$uB};$wB=q#/lib/quote_simple_init.b#;$xB=bless({$g5,$rB,$Z5,$q,$c6,$q,$d6,$vB,$L,$wB},$m6);$yB={};$zB=[];$AB=bless({$t,$zB,$v,0,$x,990,$y,$z},$A);$BB=[];$CB=q#shift->quote_value(shift)#;$DB=bless({$t,$BB,$v,$CB,$x,992,$y,$z},$A);$EB={$Tx,$AB,$xw,$DB};$FB=q#/lib/quote_simple_quote.b#;$GB=bless({$g5,$yB,$Z5,$q,$c6,$q,$d6,$EB,$L,$FB},$m6);$HB=[$S7,$xB,$GB,$Tw,$tx,$Jx];$IB=bless({$g5,$qB,$L,$A4,$S5,$HB},$E5);$JB=q#lib/quote_simple.c::ctors#;$KB=q#ni:/lib/quote_simple.c#;$LB={$E5,1};$MB=q#/lib/quote_simple.c#;$NB=[$C9];$OB=bless({$g5,$LB,$L,$MB,$S5,$NB},$T5);$PB=q#ni:/lib/quote_simple_init.b#;$QB=q#ni:/lib/quote_simple_quote.b#;$RB=q#ni:/lib/quote_values.b#;$SB=q#ni:/lib/ref_eq.b#;$TB=q#ni:/lib/resolver.b#;$UB=q#ni:/lib/slice#;$VB={$m6,1};$WB=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);#;$XB=bless({$v,$WB,$x,994,$y,$z},$A);$YB=q#local $_;
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
$self;#;$ZB=bless({$v,$YB,$x,996,$y,$z},$A);$cC=q#lib/slice::apply#;$dC=q#lib/slice::apply_unsafe#;$eC={};$fC=q#apply_unsafe#;$gC={$h6,$XB,$fC,$ZB};$hC=q#/lib/slice.b#;$iC=bless({$g5,$eC,$d6,$gC,$L,$hC},$m6);$jC={};$kC=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$lC=bless({$v,$kC,$x,998,$y,$z},$A);$mC={$c7,$lC};$nC=q#/lib/slice_init.b#;$oC=bless({$g5,$jC,$d6,$mC,$L,$nC},$m6);$pC={};$qC=[];$rC=q#local $_;
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
$g;#;$sC=bless({$t,$qC,$v,$rC,$x,1000,$y,$z},$A);$tC={$Ku,$sC};$uC=q#/lib/slice_serialize.b#;$vC=bless({$g5,$pC,$Z5,$q,$c6,$q,$d6,$tC,$L,$uC},$m6);$wC=[$f8,$x6,$iC,$oC,$vC];$xC=bless({$g5,$VB,$L,$V4,$S5,$wC},$F5);$yC=q#lib/slice.c::ctors#;$zC=q#ni:/lib/slice.b#;$AC=q#ni:/lib/slice.c#;$BC={$F5,1};$CC=q#/lib/slice.c#;$DC=[$H9];$EC=bless({$g5,$BC,$L,$CC,$S5,$DC},$T5);$FC=q#ni:/lib/slice_init.b#;$GC=q#ni:/lib/slice_serialize.b#;$HC=q#ni:/lib/static_fn.b#;$IC={};$JC=[];$KC=q#ni('ni:/lib/fn')->new(@_)#;$LC=bless({$t,$JC,$v,$KC,$x,1002,$y,$Ev},$A);$MC=q#fp#;$NC=[];$OC=q#($$)#;$PC=bless({$t,$NC,$v,$KC,$x,1004,$y,$OC},$A);$QC={$tu,$LC,$MC,$PC};$RC=q#/lib/static_fn.b#;$SC=bless({$g5,$IC,$Z5,$q,$c6,$q,$d6,$QC,$L,$RC},$m6);$TC=q#ni:/lib/subclass.b#;$UC=q#ni:/lib/tag#;$VC={$y6,1};$WC=q#/lib/tag#;$XC={};$YC=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$ZC=bless({$v,$YC,$x,1006,$y,$z},$A);$cD={$h6,$ZC};$dD=q#/lib/tag.b#;$eD=bless({$g5,$XC,$Z5,$q,$c6,$q,$d6,$cD,$L,$dD},$m6);$fD={};$gD=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$hD=bless({$v,$gD,$x,1008,$y,$z},$A);$iD={$c7,$hD};$jD=q#/lib/tag_init.b#;$kD=bless({$g5,$fD,$Z5,$q,$c6,$q,$d6,$iD,$L,$jD},$m6);$lD=[$f8,$x6,$eD,$kD];$mD=bless({$g5,$VC,$L,$WC,$S5,$lD},$G5);$nD=q#lib/tag.c::ctors#;$oD=q#ni:/lib/tag.b#;$pD=q#ni:/lib/tag.c#;$qD={$G5,1};$rD=q#/lib/tag.c#;$sD=[$H9];$tD=bless({$g5,$qD,$L,$rD,$S5,$sD},$T5);$uD=q#ni:/lib/tag_init.b#;$vD=q#ni:/lib/test_assert_eq#;$wD={$B7,1};$xD=q#/lib/test_assert_eq#;$yD={$B7,1,$C7,1};$zD=q#/lib/test_assertion#;$AD={};$BD=q#commit#;$CD=[];$DD=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$ED=bless({$t,$CD,$v,$DD,$x,1010,$y,$z},$A);$FD={$BD,$ED};$GD=q#/lib/test_assertion_commit.b#;$HD=bless({$g5,$AD,$Z5,$q,$c6,$q,$d6,$FD,$L,$GD},$m6);$ID=[$S7,$HD];$JD=bless({$g5,$yD,$L,$zD,$S5,$ID},$I5);$KD=q#lib/test_assertion.c::ctors#;$LD={};$MD=[];$ND=q#my ($class, $diff) = @_;
+{diff => $diff};#;$OD=bless({$t,$MD,$v,$ND,$x,1012,$y,$z},$A);$PD={$c7,$OD};$QD=q#/lib/test_assert_eq_init.b#;$RD=bless({$g5,$LD,$Z5,$q,$c6,$q,$d6,$PD,$L,$QD},$m6);$SD={};$TD=[];$UD=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode $$self{diff}
              : "PASS";#;$VD=bless({$t,$TD,$v,$UD,$x,1014,$y,$z},$A);$WD=q#failed#;$XD=[];$YD=q#defined shift->{diff}#;$ZD=bless({$t,$XD,$v,$YD,$x,1016,$y,$z},$A);$cE=q#result#;$dE=[];$eE=bless({$t,$dE,$v,$nq,$x,1018,$y,$z},$A);$fE={$B8,$VD,$WD,$ZD,$cE,$eE};$gE=q#/lib/test_assert_eq_result.b#;$hE=bless({$g5,$SD,$Z5,$q,$c6,$q,$d6,$fE,$L,$gE},$m6);$iE=[$JD,$RD,$hE];$jE=bless({$g5,$wD,$L,$xD,$S5,$iE},$H5);$kE=q#lib/test_assert_eq.c::ctors#;$lE=q#ni:/lib/test_assert_eq.c#;$mE={$H5,1};$nE=q#/lib/test_assert_eq.c#;$oE={$H5,1,$I5,1};$pE=q#/lib/test_assertion.c#;$qE=[$C9];$rE=bless({$g5,$oE,$L,$pE,$S5,$qE},$T5);$sE=[$rE];$tE=bless({$g5,$mE,$L,$nE,$S5,$sE},$T5);$uE=q#ni:/lib/test_assert_eq_init.b#;$vE=q#ni:/lib/test_assert_eq_result.b#;$wE=q#ni:/lib/test_assertion#;$xE=q#ni:/lib/test_assertion.c#;$yE=q#ni:/lib/test_assertion_commit.b#;$zE=q#ni:/lib/test_case#;$AE={$D,1};$BE=q#/lib/test_case#;$CE=q#running_test#;$DE={};$EE=[];$FE=q#shift->{'assertions'}#;$GE=bless({$t,$EE,$v,$FE,$x,1020,$y,$z},$A);$HE=[];$IE=q#shift->{'test'}#;$JE=bless({$t,$HE,$v,$IE,$x,1022,$y,$z},$A);$KE={$n,$GE,$s,$JE};$LE=q#/lib/test_case_ro.b#;$ME=bless({$g5,$DE,$Z5,$q,$c6,$q,$d6,$KE,$L,$LE},$m6);$NE={};$OE=[];$PE=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$QE=bless({$t,$OE,$v,$PE,$x,1024,$y,$z},$A);$RE={$p,$QE};$SE=q#/lib/test_case_rw.b#;$TE=bless({$g5,$NE,$Z5,$q,$c6,$q,$d6,$RE,$L,$SE},$m6);$UE={};$VE=[];$WE=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$XE=bless({$t,$VE,$v,$WE,$x,1026,$y,$z},$A);$YE={$c7,$XE};$ZE=q#/lib/test_case_init.b#;$cF=bless({$g5,$UE,$Z5,$q,$c6,$q,$d6,$YE,$L,$ZE},$m6);$dF={};$eF=[];$fF=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$gF=bless({$t,$eF,$v,$fF,$x,1028,$y,$z},$A);$hF=[];$iF=q#!shift->{outcome}->[0]#;$jF=bless({$t,$hF,$v,$iF,$x,1030,$y,$z},$A);$kF={$B8,$gF,$WD,$jF};$lF=q#/lib/test_case_metrics.b#;$mF=bless({$g5,$dF,$Z5,$q,$c6,$q,$d6,$kF,$L,$lF},$m6);$nF={};$oF=q#done#;$pF=[];$qF=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$rF=bless({$t,$pF,$v,$qF,$x,1032,$y,$z},$A);$sF=[];$tF=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$uF=bless({$t,$sF,$v,$tF,$x,1034,$y,$z},$A);$vF={$oF,$rF,$lq,$uF};$wF=q#/lib/test_case_run.b#;$xF=bless({$g5,$nF,$Z5,$q,$c6,$q,$d6,$vF,$L,$wF},$m6);$yF=[$S7,$ME,$TE,$cF,$mF,$xF];$zF=bless({$g5,$AE,$L,$BE,$CE,$q,$S5,$yF},$J5);$AF=q#lib/test_case.c::ctors#;$BF=q#ni:/lib/test_case.c#;$CF={$J5,1};$DF=q#/lib/test_case.c#;$EF={};$FF=[];$GF=q#shift->{'running_test'}#;$HF=bless({$t,$FF,$v,$GF,$x,1038,$y,$z},$A);$IF={$CE,$HF};$JF=q#/lib/test_case.c_test_ro.b#;$KF=bless({$g5,$EF,$Z5,$q,$c6,$q,$d6,$IF,$L,$JF},$m6);$LF={};$MF=[];$NF=q#shift->{running_test} = undef#;$OF=bless({$t,$MF,$v,$NF,$x,1040,$y,$z},$A);$PF=q#with_test#;$QF=[];$RF=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$SF=bless({$t,$QF,$v,$RF,$x,1042,$y,$z},$A);$TF={$PF,$SF};$UF=q#/lib/test_case.c_test.b#;$VF=bless({$g5,$LF,$Z5,$OF,$c6,$q,$d6,$TF,$L,$UF},$m6);$WF=[$C9,$KF,$VF];$XF=bless({$g5,$CF,$L,$DF,$S5,$WF},$T5);$YF=q#ni:/lib/test_case.c_test.b#;$ZF=q#ni:/lib/test_case.c_test_ro.b#;$cG=q#ni:/lib/test_case_init.b#;$dG=q#ni:/lib/test_case_metrics.b#;$eG=q#ni:/lib/test_case_ro.b#;$fG=q#ni:/lib/test_case_run.b#;$gG=q#ni:/lib/test_case_rw.b#;$hG=q#ni:/lib/test_value#;$iG={$D7,1};$jG=q#/lib/test_value#;$kG={};$lG=[];$mG=q#\\$_[1]#;$nG=bless({$t,$lG,$v,$mG,$x,1044,$y,$z},$A);$oG={$c7,$nG};$pG=q#/lib/test_value_init.b#;$qG=bless({$g5,$kG,$Z5,$q,$c6,$q,$d6,$oG,$L,$pG},$m6);$rG={};$sG=q#(==#;$tG=[];$uG=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$vG=bless({$t,$tG,$v,$uG,$x,1046,$y,$z},$A);$wG=q#diff#;$xG=[];$yG=q#my ($self, $rhs) = @_;
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
return undef;#;$zG=bless({$t,$xG,$v,$yG,$x,1048,$y,$z},$A);$AG={$sG,$vG,$wG,$zG};$BG=q#/lib/test_value_eq.b#;$CG=bless({$g5,$rG,$Z5,$q,$c6,$q,$d6,$AG,$L,$BG},$m6);$DG={};$EG=[];$FG=q#ni::json_encode ${$_[0]}#;$GG=bless({$t,$EG,$v,$FG,$x,1050,$y,$z},$A);$HG={$B8,$GG};$IG=q#/lib/test_value_str.b#;$JG=bless({$g5,$DG,$Z5,$q,$c6,$q,$d6,$HG,$L,$IG},$m6);$KG=[$S7,$qG,$CG,$JG];$LG=bless({$g5,$iG,$L,$jG,$S5,$KG},$K5);$MG=q#lib/test_value.c::ctors#;$NG=q#ni:/lib/test_value.c#;$OG={$K5,1};$PG=q#/lib/test_value.c#;$QG=[$C9];$RG=bless({$g5,$OG,$L,$PG,$S5,$QG},$T5);$SG=q#ni:/lib/test_value_eq.b#;$TG=q#ni:/lib/test_value_init.b#;$UG=q#ni:/lib/test_value_str.b#;$VG=q#ni:/metaclass#;$WG={$T5,1};$XG=q#/metaclass#;$YG=[$V6,$j9,$h7,$Z8];$ZG=bless({$g5,$WG,$L,$XG,$S5,$YG},$L5);$cH=q#metaclass.c::ctors#;$dH=q#ni:/metaclass.c#;$eH={$L5,1};$fH=q#/metaclass.c#;$gH=[$s9];$hH=bless({$g5,$eH,$L,$fH,$S5,$gH},$T5);$iH=q#ni:/module#;$jH=q#ni:/module.c#;$kH=q#ni:/object#;$lH=q#ni:/object.c#;$mH=q#ni:/semantic/dimension#;$nH={$O5,1};$oH=q#/semantic/dimension#;$pH=[$s9];$qH=bless({$g5,$nH,$L,$oH,$S5,$pH},$P5);$rH=q#semantic/dimension.c::ctors#;$sH=q#ni:/semantic/dimension.c#;$tH={$P5,1};$uH=q#/semantic/dimension.c#;$vH=[$L9];$wH=bless({$g5,$tH,$L,$uH,$S5,$vH},$T5);$xH=q#ni:/semantic/task#;$yH=q#ni:/semantic/task.c#;$zH=q#ni:/semantic/task_outcome.b#;$AH=q#ni:/semantic/task_ro.b#;$BH=q#ni:main#;$CH={$Ak,1};$DH=[$SC,$Mv,$zk];$EH=bless({$g5,$CH,$L,$Ak,$S5,$DH},$U5);$FH=q#module::ctors#;$GH=q#ni:ni#;$HH={$Ny,1};$IH={$Ny,1};$JH=q#json_escapes#;$KH=q##;$LH=q#b#;$MH=q#	#;$NH=q#t#;$OH=q#
#;$PH=q#n#;$QH=q##;$RH=q#"#;$SH=q#/#;$TH=q#\\#;$UH={$KH,$LH,$MH,$NH,$OH,$PH,$QH,$Ni,$RH,$RH,$SH,$SH,$TH,$TH};$VH=q#json_unescapes#;$WH={$RH,$RH,$SH,$SH,$TH,$TH,$LH,$KH,$PH,$OH,$Ni,$QH,$NH,$MH};$XH={$JH,$UH,$VH,$WH};$YH=q#/lib/json_data.b#;$ZH=bless({$g5,$IH,$Wm,$XH,$L,$YH},$x7);$cI=[$ZH,$My,$kB];$dI=bless({$g5,$HH,$L,$Ny,$S5,$cI},$U5);$eI={$d,$O,$Q,$V,$W,$o1,$p1,$u1,$v1,$H1,$I1,$U1,$V1,$j2,$k2,$w2,$x2,$S2,$T2,$Y2,$Z2,$x3,$y3,$E3,$F3,$Y3,$Z3,$i4,$j4,$q4,$r4,$B4,$C4,$W4,$X4,$e5,$f5,$s9,$u9,$L9,$M9,$Xa,$Za,$fb,$gb,$Va,$hb,$la,$ib,$ea,$jb,$Ba,$kb,$qd,$sd,$Id,$Jd,$Sc,$Kd,$od,$Ld,$ee,$ge,$ke,$le,$Td,$me,$ce,$ne,$Xf,$Zf,$fg,$gg,$Ff,$hg,$Vf,$ig,$De,$jg,$xf,$kg,$Xe,$lg,$we,$mg,$yh,$Ah,$Eh,$Fh,$Zg,$Gh,$mh,$Hh,$Bg,$Ih,$wh,$Jh,$ug,$Kh,$Jg,$Lh,$hj,$jj,$nj,$oj,$di,$pj,$Ai,$qj,$ki,$rj,$fj,$sj,$Th,$tj,$Ii,$uj,$Nj,$Pj,$Tj,$Uj,$Lj,$Vj,$Cj,$Wj,$zk,$Bk,$Zk,$dl,$hl,$il,$Kk,$jl,$Xk,$kl,$Kc,$ll,$Gd,$ml,$Ed,$nl,$Ob,$ol,$Wb,$pl,$kc,$ql,$ub,$rl,$Ic,$sl,$wc,$tl,$Gm,$Im,$Mm,$Nm,$Em,$Om,$Ql,$Pm,$om,$Qm,$Gl,$Rm,$em,$Sm,$Jn,$Ln,$Pn,$Qn,$tn,$Rn,$Hn,$Sn,$mn,$Tn,$fp,$hp,$wp,$xp,$up,$yp,$Aq,$Cq,$Zq,$cr,$Xq,$dr,$Xp,$er,$jq,$fr,$Qp,$gr,$vq,$hr,$Ho,$ir,$dp,$jr,$Br,$Dr,$Hr,$Ir,$sr,$Jr,$zr,$Kr,$z8,$Lr,$f8,$Mr,$H9,$Nr,$Xr,$Zr,$n6,$cs,$gs,$hs,$Vr,$is,$h7,$js,$zs,$Bs,$Fs,$Gs,$xs,$Hs,$rs,$Is,$W8,$Js,$p8,$Ks,$U8,$Ls,$Et,$Gt,$Kt,$Lt,$ht,$Mt,$qt,$Nt,$Ss,$Ot,$Ys,$Pt,$Ct,$Qt,$d8,$Rt,$Su,$Uu,$lv,$mv,$jv,$nv,$iu,$ov,$Iu,$pv,$zu,$qv,$Qu,$rv,$zv,$Av,$Mv,$Nv,$dy,$fy,$jy,$ky,$Vv,$ly,$Lw,$my,$Q7,$ny,$j9,$oy,$My,$Oy,$G8,$Py,$x6,$Qy,$F6,$Ry,$M6,$Sy,$Vz,$Xz,$dA,$eA,$Tz,$fA,$Az,$gA,$Jz,$hA,$iz,$iA,$kB,$lB,$V6,$mB,$Zx,$nB,$Tw,$oB,$Jx,$pB,$IB,$KB,$OB,$PB,$xB,$QB,$GB,$RB,$tx,$SB,$N8,$TB,$T6,$UB,$xC,$zC,$iC,$AC,$EC,$FC,$oC,$GC,$vC,$HC,$SC,$TC,$q9,$UC,$mD,$oD,$eD,$pD,$tD,$uD,$kD,$vD,$jE,$lE,$tE,$uE,$RD,$vE,$hE,$wE,$JD,$xE,$rE,$yE,$HD,$zE,$zF,$BF,$XF,$YF,$VF,$ZF,$KF,$cG,$cF,$dG,$mF,$eG,$ME,$fG,$xF,$gG,$TE,$hG,$LG,$NG,$RG,$SG,$CG,$TG,$qG,$UG,$JG,$VG,$ZG,$dH,$hH,$iH,$Z8,$jH,$J9,$kH,$S7,$lH,$C9,$mH,$qH,$sH,$wH,$xH,$to,$yH,$np,$zH,$ro,$AH,$fo,$BH,$EH,$GH,$dI};$fI=q#resolvers#;$gI=[];$hI=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$iI=bless({$t,$gI,$v,$hI,$x,1052,$y,$z},$A);$jI=q#file#;$kI=[];$lI=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$mI=bless({$t,$kI,$v,$lI,$x,1054,$y,$z},$A);$nI=q#null#;$oI=[];$pI=q#ni('ni:/io/null')->new#;$qI=bless({$t,$oI,$v,$pI,$x,1056,$y,$z},$A);$rI=q#sh#;$sI=[];$tI=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$uI=bless({$t,$sI,$v,$tI,$x,1058,$y,$z},$A);$vI=q#str#;$wI=[];$xI=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$yI=bless({$t,$wI,$v,$xI,$x,1060,$y,$z},$A);$zI={$ff,$iI,$jI,$mI,$nI,$qI,$rI,$uI,$vI,$yI};$AI=bless({$c,$eI,$fI,$zI},$z7);*$dC=\&$ZB;*$cC=\&$XB;$n6->apply_unsafe($h5);$n6->apply_unsafe($i5);$n6->apply_unsafe($j5);$n6->apply_unsafe($k5);$n6->apply_unsafe($l5);$n6->apply_unsafe($m5);$n6->apply_unsafe($n5);$n6->apply_unsafe($o5);$n6->apply_unsafe($p5);$n6->apply_unsafe($q5);$n6->apply_unsafe($r5);$n6->apply_unsafe($s5);$n6->apply_unsafe($t5);$n6->apply_unsafe($u5);$n6->apply_unsafe($v5);$n6->apply_unsafe($w5);$n6->apply_unsafe($x5);$n6->apply_unsafe($o6);$n6->apply_unsafe($y5);$n6->apply_unsafe($z5);$n6->apply_unsafe($A5);$n6->apply_unsafe($B5);$n6->apply_unsafe($C5);$n6->apply_unsafe($D5);$n6->apply_unsafe($E5);$n6->apply_unsafe($F5);$n6->apply_unsafe($G5);$n6->apply_unsafe($H5);$n6->apply_unsafe($I5);$n6->apply_unsafe($J5);$n6->apply_unsafe($K5);$n6->apply_unsafe($T5);$n6->apply_unsafe($L5);$n6->apply_unsafe($U5);$n6->apply_unsafe($M5);$n6->apply_unsafe($N5);$n6->apply_unsafe($O5);$n6->apply_unsafe($P5);$n6->apply_unsafe($Q5);$x6->apply_unsafe($h5);$x6->apply_unsafe($i5);$x6->apply_unsafe($j5);$x6->apply_unsafe($k5);$x6->apply_unsafe($l5);$x6->apply_unsafe($m5);$x6->apply_unsafe($n5);$x6->apply_unsafe($o5);$x6->apply_unsafe($p5);$x6->apply_unsafe($q5);$x6->apply_unsafe($r5);$x6->apply_unsafe($s5);$x6->apply_unsafe($t5);$x6->apply_unsafe($u5);$x6->apply_unsafe($v5);$x6->apply_unsafe($w5);$x6->apply_unsafe($x5);$x6->apply_unsafe($o6);$x6->apply_unsafe($y5);$x6->apply_unsafe($z5);$x6->apply_unsafe($N);$x6->apply_unsafe($A5);$x6->apply_unsafe($B5);$x6->apply_unsafe($C5);$x6->apply_unsafe($D5);$x6->apply_unsafe($E5);$x6->apply_unsafe($m6);$x6->apply_unsafe($F5);$x6->apply_unsafe($y6);$x6->apply_unsafe($G5);$x6->apply_unsafe($H5);$x6->apply_unsafe($I5);$x6->apply_unsafe($J5);$x6->apply_unsafe($K5);$x6->apply_unsafe($T5);$x6->apply_unsafe($L5);$x6->apply_unsafe($U5);$x6->apply_unsafe($M5);$x6->apply_unsafe($N5);$x6->apply_unsafe($O5);$x6->apply_unsafe($P5);$x6->apply_unsafe($Q5);$F6->apply_unsafe($h5);$F6->apply_unsafe($i5);$F6->apply_unsafe($j5);$F6->apply_unsafe($k5);$F6->apply_unsafe($l5);$F6->apply_unsafe($m5);$F6->apply_unsafe($n5);$F6->apply_unsafe($o5);$F6->apply_unsafe($p5);$F6->apply_unsafe($q5);$F6->apply_unsafe($r5);$F6->apply_unsafe($s5);$F6->apply_unsafe($t5);$F6->apply_unsafe($u5);$F6->apply_unsafe($v5);$F6->apply_unsafe($w5);$F6->apply_unsafe($x5);$F6->apply_unsafe($o6);$F6->apply_unsafe($y5);$F6->apply_unsafe($z5);$F6->apply_unsafe($A5);$F6->apply_unsafe($B5);$F6->apply_unsafe($C5);$F6->apply_unsafe($D5);$F6->apply_unsafe($E5);$F6->apply_unsafe($m6);$F6->apply_unsafe($F5);$F6->apply_unsafe($y6);$F6->apply_unsafe($G5);$F6->apply_unsafe($H5);$F6->apply_unsafe($I5);$F6->apply_unsafe($J5);$F6->apply_unsafe($K5);$F6->apply_unsafe($T5);$F6->apply_unsafe($L5);$F6->apply_unsafe($U5);$F6->apply_unsafe($M5);$F6->apply_unsafe($N5);$F6->apply_unsafe($O5);$F6->apply_unsafe($P5);$F6->apply_unsafe($Q5);$M6->apply_unsafe($h5);$M6->apply_unsafe($i5);$M6->apply_unsafe($j5);$M6->apply_unsafe($k5);$M6->apply_unsafe($l5);$M6->apply_unsafe($m5);$M6->apply_unsafe($n5);$M6->apply_unsafe($o5);$M6->apply_unsafe($p5);$M6->apply_unsafe($q5);$M6->apply_unsafe($r5);$M6->apply_unsafe($s5);$M6->apply_unsafe($t5);$M6->apply_unsafe($u5);$M6->apply_unsafe($v5);$M6->apply_unsafe($w5);$M6->apply_unsafe($x5);$M6->apply_unsafe($o6);$M6->apply_unsafe($y5);$M6->apply_unsafe($z5);$M6->apply_unsafe($A5);$M6->apply_unsafe($B5);$M6->apply_unsafe($C5);$M6->apply_unsafe($D5);$M6->apply_unsafe($E5);$M6->apply_unsafe($m6);$M6->apply_unsafe($F5);$M6->apply_unsafe($y6);$M6->apply_unsafe($G5);$M6->apply_unsafe($H5);$M6->apply_unsafe($I5);$M6->apply_unsafe($J5);$M6->apply_unsafe($K5);$M6->apply_unsafe($T5);$M6->apply_unsafe($L5);$M6->apply_unsafe($U5);$M6->apply_unsafe($M5);$M6->apply_unsafe($N5);$M6->apply_unsafe($O5);$M6->apply_unsafe($P5);$M6->apply_unsafe($Q5);$T6->apply_unsafe($h5);$T6->apply_unsafe($i5);$T6->apply_unsafe($j5);$T6->apply_unsafe($k5);$T6->apply_unsafe($l5);$T6->apply_unsafe($m5);$T6->apply_unsafe($n5);$T6->apply_unsafe($o5);$T6->apply_unsafe($p5);$T6->apply_unsafe($q5);$T6->apply_unsafe($r5);$T6->apply_unsafe($s5);$T6->apply_unsafe($t5);$T6->apply_unsafe($u5);$T6->apply_unsafe($v5);$T6->apply_unsafe($w5);$T6->apply_unsafe($x5);$T6->apply_unsafe($o6);$T6->apply_unsafe($y5);$T6->apply_unsafe($z5);$T6->apply_unsafe($A5);$T6->apply_unsafe($B5);$T6->apply_unsafe($C5);$T6->apply_unsafe($D5);$T6->apply_unsafe($E5);$T6->apply_unsafe($F5);$T6->apply_unsafe($y6);$T6->apply_unsafe($G5);$T6->apply_unsafe($H5);$T6->apply_unsafe($I5);$T6->apply_unsafe($J5);$T6->apply_unsafe($K5);$T6->apply_unsafe($T5);$T6->apply_unsafe($L5);$T6->apply_unsafe($U5);$T6->apply_unsafe($M5);$T6->apply_unsafe($N5);$T6->apply_unsafe($O5);$T6->apply_unsafe($P5);$T6->apply_unsafe($Q5);$h7->apply_unsafe($h5);$h7->apply_unsafe($i5);$h7->apply_unsafe($j5);$h7->apply_unsafe($k5);$h7->apply_unsafe($l5);$h7->apply_unsafe($m5);$h7->apply_unsafe($n5);$h7->apply_unsafe($o5);$h7->apply_unsafe($p5);$h7->apply_unsafe($q5);$h7->apply_unsafe($r5);$h7->apply_unsafe($s5);$h7->apply_unsafe($t5);$h7->apply_unsafe($u5);$h7->apply_unsafe($v5);$h7->apply_unsafe($w5);$h7->apply_unsafe($x5);$h7->apply_unsafe($y5);$h7->apply_unsafe($z5);$h7->apply_unsafe($A5);$h7->apply_unsafe($B5);$h7->apply_unsafe($C5);$h7->apply_unsafe($D5);$h7->apply_unsafe($E5);$h7->apply_unsafe($F5);$h7->apply_unsafe($G5);$h7->apply_unsafe($H5);$h7->apply_unsafe($I5);$h7->apply_unsafe($J5);$h7->apply_unsafe($K5);$h7->apply_unsafe($T5);$h7->apply_unsafe($L5);$h7->apply_unsafe($U5);$h7->apply_unsafe($M5);$h7->apply_unsafe($N5);$h7->apply_unsafe($O5);$h7->apply_unsafe($P5);$h7->apply_unsafe($Q5);$Q7->apply_unsafe($h5);$Q7->apply_unsafe($i5);$Q7->apply_unsafe($i7);$Q7->apply_unsafe($j5);$Q7->apply_unsafe($j7);$Q7->apply_unsafe($k5);$Q7->apply_unsafe($k7);$Q7->apply_unsafe($l5);$Q7->apply_unsafe($l7);$Q7->apply_unsafe($m5);$Q7->apply_unsafe($m7);$Q7->apply_unsafe($n5);$Q7->apply_unsafe($n7);$Q7->apply_unsafe($o5);$Q7->apply_unsafe($o7);$Q7->apply_unsafe($p5);$Q7->apply_unsafe($p7);$Q7->apply_unsafe($q5);$Q7->apply_unsafe($q7);$Q7->apply_unsafe($r5);$Q7->apply_unsafe($r7);$Q7->apply_unsafe($s5);$Q7->apply_unsafe($s7);$Q7->apply_unsafe($t5);$Q7->apply_unsafe($t7);$Q7->apply_unsafe($u5);$Q7->apply_unsafe($u7);$Q7->apply_unsafe($v5);$Q7->apply_unsafe($v7);$Q7->apply_unsafe($w5);$Q7->apply_unsafe($w7);$Q7->apply_unsafe($x5);$Q7->apply_unsafe($o6);$Q7->apply_unsafe($y5);$Q7->apply_unsafe($x7);$Q7->apply_unsafe($z5);$Q7->apply_unsafe($N);$Q7->apply_unsafe($A5);$Q7->apply_unsafe($A);$Q7->apply_unsafe($B5);$Q7->apply_unsafe($y7);$Q7->apply_unsafe($C5);$Q7->apply_unsafe($z7);$Q7->apply_unsafe($D5);$Q7->apply_unsafe($A7);$Q7->apply_unsafe($E5);$Q7->apply_unsafe($m6);$Q7->apply_unsafe($F5);$Q7->apply_unsafe($y6);$Q7->apply_unsafe($G5);$Q7->apply_unsafe($B7);$Q7->apply_unsafe($H5);$Q7->apply_unsafe($C7);$Q7->apply_unsafe($I5);$Q7->apply_unsafe($D);$Q7->apply_unsafe($J5);$Q7->apply_unsafe($D7);$Q7->apply_unsafe($K5);$Q7->apply_unsafe($T5);$Q7->apply_unsafe($L5);$Q7->apply_unsafe($U5);$Q7->apply_unsafe($M5);$Q7->apply_unsafe($E7);$Q7->apply_unsafe($N5);$Q7->apply_unsafe($O5);$Q7->apply_unsafe($P5);$Q7->apply_unsafe($F7);$Q7->apply_unsafe($Q5);$d8->apply_unsafe($h5);$d8->apply_unsafe($i5);$d8->apply_unsafe($j5);$d8->apply_unsafe($k5);$d8->apply_unsafe($l5);$d8->apply_unsafe($m5);$d8->apply_unsafe($n5);$d8->apply_unsafe($o5);$d8->apply_unsafe($p5);$d8->apply_unsafe($q5);$d8->apply_unsafe($r5);$d8->apply_unsafe($s5);$d8->apply_unsafe($t5);$d8->apply_unsafe($u5);$d8->apply_unsafe($v5);$d8->apply_unsafe($w5);$d8->apply_unsafe($w7);$d8->apply_unsafe($x5);$d8->apply_unsafe($o6);$d8->apply_unsafe($y5);$d8->apply_unsafe($x7);$d8->apply_unsafe($z5);$d8->apply_unsafe($A5);$d8->apply_unsafe($B5);$d8->apply_unsafe($C5);$d8->apply_unsafe($D5);$d8->apply_unsafe($E5);$d8->apply_unsafe($m6);$d8->apply_unsafe($F5);$d8->apply_unsafe($y6);$d8->apply_unsafe($G5);$d8->apply_unsafe($H5);$d8->apply_unsafe($I5);$d8->apply_unsafe($J5);$d8->apply_unsafe($K5);$d8->apply_unsafe($T5);$d8->apply_unsafe($L5);$d8->apply_unsafe($U5);$d8->apply_unsafe($M5);$d8->apply_unsafe($N5);$d8->apply_unsafe($O5);$d8->apply_unsafe($P5);$d8->apply_unsafe($Q5);$p8->apply_unsafe($h5);$p8->apply_unsafe($i5);$p8->apply_unsafe($j5);$p8->apply_unsafe($k5);$p8->apply_unsafe($l5);$p8->apply_unsafe($m5);$p8->apply_unsafe($n5);$p8->apply_unsafe($o5);$p8->apply_unsafe($p5);$p8->apply_unsafe($q5);$p8->apply_unsafe($r5);$p8->apply_unsafe($s5);$p8->apply_unsafe($t5);$p8->apply_unsafe($u5);$p8->apply_unsafe($v5);$p8->apply_unsafe($w5);$p8->apply_unsafe($x5);$p8->apply_unsafe($o6);$p8->apply_unsafe($y5);$p8->apply_unsafe($z5);$p8->apply_unsafe($A5);$p8->apply_unsafe($B5);$p8->apply_unsafe($C5);$p8->apply_unsafe($D5);$p8->apply_unsafe($E5);$p8->apply_unsafe($F5);$p8->apply_unsafe($G5);$p8->apply_unsafe($H5);$p8->apply_unsafe($I5);$p8->apply_unsafe($J5);$p8->apply_unsafe($K5);$p8->apply_unsafe($T5);$p8->apply_unsafe($L5);$p8->apply_unsafe($U5);$p8->apply_unsafe($M5);$p8->apply_unsafe($N5);$p8->apply_unsafe($O5);$p8->apply_unsafe($P5);$p8->apply_unsafe($Q5);$z8->apply_unsafe($h5);$z8->apply_unsafe($i5);$z8->apply_unsafe($j5);$z8->apply_unsafe($k5);$z8->apply_unsafe($l5);$z8->apply_unsafe($m5);$z8->apply_unsafe($n5);$z8->apply_unsafe($o5);$z8->apply_unsafe($p5);$z8->apply_unsafe($q5);$z8->apply_unsafe($r5);$z8->apply_unsafe($s5);$z8->apply_unsafe($t5);$z8->apply_unsafe($u5);$z8->apply_unsafe($v5);$z8->apply_unsafe($w5);$z8->apply_unsafe($x5);$z8->apply_unsafe($o6);$z8->apply_unsafe($y5);$z8->apply_unsafe($z5);$z8->apply_unsafe($A5);$z8->apply_unsafe($B5);$z8->apply_unsafe($C5);$z8->apply_unsafe($D5);$z8->apply_unsafe($E5);$z8->apply_unsafe($F5);$z8->apply_unsafe($G5);$z8->apply_unsafe($H5);$z8->apply_unsafe($I5);$z8->apply_unsafe($J5);$z8->apply_unsafe($K5);$z8->apply_unsafe($T5);$z8->apply_unsafe($L5);$z8->apply_unsafe($U5);$z8->apply_unsafe($M5);$z8->apply_unsafe($N5);$z8->apply_unsafe($O5);$z8->apply_unsafe($P5);$z8->apply_unsafe($Q5);$G8->apply_unsafe($h5);$G8->apply_unsafe($i5);$G8->apply_unsafe($j5);$G8->apply_unsafe($k5);$G8->apply_unsafe($l5);$G8->apply_unsafe($m5);$G8->apply_unsafe($n5);$G8->apply_unsafe($o5);$G8->apply_unsafe($p5);$G8->apply_unsafe($q5);$G8->apply_unsafe($r5);$G8->apply_unsafe($s5);$G8->apply_unsafe($t5);$G8->apply_unsafe($u5);$G8->apply_unsafe($v5);$G8->apply_unsafe($w5);$G8->apply_unsafe($x5);$G8->apply_unsafe($o6);$G8->apply_unsafe($y5);$G8->apply_unsafe($z5);$G8->apply_unsafe($A5);$G8->apply_unsafe($B5);$G8->apply_unsafe($C5);$G8->apply_unsafe($D5);$G8->apply_unsafe($E5);$G8->apply_unsafe($F5);$G8->apply_unsafe($G5);$G8->apply_unsafe($H5);$G8->apply_unsafe($I5);$G8->apply_unsafe($J5);$G8->apply_unsafe($K5);$G8->apply_unsafe($T5);$G8->apply_unsafe($L5);$G8->apply_unsafe($U5);$G8->apply_unsafe($M5);$G8->apply_unsafe($N5);$G8->apply_unsafe($O5);$G8->apply_unsafe($P5);$G8->apply_unsafe($Q5);$N8->apply_unsafe($h5);$N8->apply_unsafe($i5);$N8->apply_unsafe($j5);$N8->apply_unsafe($k5);$N8->apply_unsafe($l5);$N8->apply_unsafe($m5);$N8->apply_unsafe($n5);$N8->apply_unsafe($o5);$N8->apply_unsafe($p5);$N8->apply_unsafe($q5);$N8->apply_unsafe($r5);$N8->apply_unsafe($s5);$N8->apply_unsafe($t5);$N8->apply_unsafe($u5);$N8->apply_unsafe($v5);$N8->apply_unsafe($w5);$N8->apply_unsafe($x5);$N8->apply_unsafe($o6);$N8->apply_unsafe($y5);$N8->apply_unsafe($z5);$N8->apply_unsafe($A5);$N8->apply_unsafe($B5);$N8->apply_unsafe($C5);$N8->apply_unsafe($D5);$N8->apply_unsafe($E5);$N8->apply_unsafe($F5);$N8->apply_unsafe($G5);$N8->apply_unsafe($H5);$N8->apply_unsafe($I5);$N8->apply_unsafe($J5);$N8->apply_unsafe($K5);$N8->apply_unsafe($T5);$N8->apply_unsafe($L5);$N8->apply_unsafe($U5);$N8->apply_unsafe($M5);$N8->apply_unsafe($N5);$N8->apply_unsafe($O5);$N8->apply_unsafe($P5);$N8->apply_unsafe($Q5);$U8->apply_unsafe($h5);$U8->apply_unsafe($i5);$U8->apply_unsafe($j5);$U8->apply_unsafe($k5);$U8->apply_unsafe($l5);$U8->apply_unsafe($m5);$U8->apply_unsafe($n5);$U8->apply_unsafe($o5);$U8->apply_unsafe($p5);$U8->apply_unsafe($q5);$U8->apply_unsafe($r5);$U8->apply_unsafe($s5);$U8->apply_unsafe($t5);$U8->apply_unsafe($u5);$U8->apply_unsafe($v5);$U8->apply_unsafe($w5);$U8->apply_unsafe($x5);$U8->apply_unsafe($o6);$U8->apply_unsafe($y5);$U8->apply_unsafe($z5);$U8->apply_unsafe($A5);$U8->apply_unsafe($B5);$U8->apply_unsafe($C5);$U8->apply_unsafe($D5);$U8->apply_unsafe($E5);$U8->apply_unsafe($F5);$U8->apply_unsafe($G5);$U8->apply_unsafe($H5);$U8->apply_unsafe($I5);$U8->apply_unsafe($J5);$U8->apply_unsafe($K5);$U8->apply_unsafe($T5);$U8->apply_unsafe($L5);$U8->apply_unsafe($U5);$U8->apply_unsafe($M5);$U8->apply_unsafe($N5);$U8->apply_unsafe($O5);$U8->apply_unsafe($P5);$U8->apply_unsafe($Q5);$j9->apply_unsafe($h5);$j9->apply_unsafe($i5);$j9->apply_unsafe($j5);$j9->apply_unsafe($k5);$j9->apply_unsafe($l5);$j9->apply_unsafe($m5);$j9->apply_unsafe($n5);$j9->apply_unsafe($o5);$j9->apply_unsafe($p5);$j9->apply_unsafe($q5);$j9->apply_unsafe($r5);$j9->apply_unsafe($s5);$j9->apply_unsafe($t5);$j9->apply_unsafe($u5);$j9->apply_unsafe($v5);$j9->apply_unsafe($w5);$j9->apply_unsafe($x5);$j9->apply_unsafe($y5);$j9->apply_unsafe($z5);$j9->apply_unsafe($A5);$j9->apply_unsafe($A);$j9->apply_unsafe($B5);$j9->apply_unsafe($C5);$j9->apply_unsafe($D5);$j9->apply_unsafe($E5);$j9->apply_unsafe($m6);$j9->apply_unsafe($F5);$j9->apply_unsafe($y6);$j9->apply_unsafe($G5);$j9->apply_unsafe($H5);$j9->apply_unsafe($I5);$j9->apply_unsafe($J5);$j9->apply_unsafe($K5);$j9->apply_unsafe($T5);$j9->apply_unsafe($L5);$j9->apply_unsafe($M5);$j9->apply_unsafe($N5);$j9->apply_unsafe($O5);$j9->apply_unsafe($P5);$j9->apply_unsafe($Q5);$q9->apply_unsafe($h5);$q9->apply_unsafe($i5);$q9->apply_unsafe($j5);$q9->apply_unsafe($k5);$q9->apply_unsafe($l5);$q9->apply_unsafe($m5);$q9->apply_unsafe($n5);$q9->apply_unsafe($o5);$q9->apply_unsafe($p5);$q9->apply_unsafe($q5);$q9->apply_unsafe($r5);$q9->apply_unsafe($s5);$q9->apply_unsafe($t5);$q9->apply_unsafe($u5);$q9->apply_unsafe($v5);$q9->apply_unsafe($w5);$q9->apply_unsafe($x5);$q9->apply_unsafe($y5);$q9->apply_unsafe($z5);$q9->apply_unsafe($A5);$q9->apply_unsafe($B5);$q9->apply_unsafe($C5);$q9->apply_unsafe($D5);$q9->apply_unsafe($E5);$q9->apply_unsafe($F5);$q9->apply_unsafe($G5);$q9->apply_unsafe($H5);$q9->apply_unsafe($I5);$q9->apply_unsafe($J5);$q9->apply_unsafe($K5);$q9->apply_unsafe($L5);$q9->apply_unsafe($M5);$q9->apply_unsafe($N5);$q9->apply_unsafe($O5);$q9->apply_unsafe($P5);$q9->apply_unsafe($Q5);$ea->apply_unsafe($i7);$la->apply_unsafe($i7);$Ba->apply_unsafe($i7);$Va->apply_unsafe($i7);$ub->apply_unsafe($j7);$ub->apply_unsafe($k7);$ub->apply_unsafe($l7);$ub->apply_unsafe($m7);$ub->apply_unsafe($n7);$ub->apply_unsafe($o7);$ub->apply_unsafe($p7);$ub->apply_unsafe($q7);$ub->apply_unsafe($r7);$ub->apply_unsafe($s7);$Ob->apply_unsafe($j7);$Ob->apply_unsafe($k7);$Ob->apply_unsafe($l7);$Ob->apply_unsafe($m7);$Ob->apply_unsafe($n7);$Ob->apply_unsafe($o7);$Ob->apply_unsafe($p7);$Ob->apply_unsafe($q7);$Ob->apply_unsafe($r7);$Ob->apply_unsafe($s7);$Wb->apply_unsafe($j7);$Wb->apply_unsafe($k7);$Wb->apply_unsafe($l7);$Wb->apply_unsafe($m7);$Wb->apply_unsafe($n7);$Wb->apply_unsafe($o7);$Wb->apply_unsafe($p7);$Wb->apply_unsafe($q7);$Wb->apply_unsafe($r7);$Wb->apply_unsafe($s7);$kc->apply_unsafe($j7);$kc->apply_unsafe($k7);$kc->apply_unsafe($l7);$kc->apply_unsafe($m7);$kc->apply_unsafe($n7);$kc->apply_unsafe($o7);$kc->apply_unsafe($p7);$kc->apply_unsafe($q7);$kc->apply_unsafe($r7);$kc->apply_unsafe($s7);$wc->apply_unsafe($j7);$wc->apply_unsafe($k7);$wc->apply_unsafe($l7);$wc->apply_unsafe($m7);$wc->apply_unsafe($n7);$wc->apply_unsafe($o7);$wc->apply_unsafe($p7);$wc->apply_unsafe($q7);$wc->apply_unsafe($r7);$wc->apply_unsafe($s7);$Ic->apply_unsafe($j7);$Ic->apply_unsafe($k7);$Ic->apply_unsafe($l7);$Ic->apply_unsafe($m7);$Ic->apply_unsafe($n7);$Ic->apply_unsafe($o7);$Ic->apply_unsafe($p7);$Ic->apply_unsafe($q7);$Ic->apply_unsafe($r7);$Ic->apply_unsafe($s7);$Sc->apply_unsafe($j7);$od->apply_unsafe($j7);$Ed->apply_unsafe($k5);$Ed->apply_unsafe($l5);$Ed->apply_unsafe($m5);$Ed->apply_unsafe($n5);$Ed->apply_unsafe($o5);$Ed->apply_unsafe($p5);$Ed->apply_unsafe($q5);$Ed->apply_unsafe($r5);$Ed->apply_unsafe($s5);$Ed->apply_unsafe($t5);$Td->apply_unsafe($k7);$ce->apply_unsafe($k7);$we->apply_unsafe($l7);$De->apply_unsafe($l7);$Xe->apply_unsafe($l7);$xf->apply_unsafe($l7);$Ff->apply_unsafe($l7);$Vf->apply_unsafe($l7);$ug->apply_unsafe($m7);$ug->apply_unsafe($o7);$Bg->apply_unsafe($m7);$Jg->apply_unsafe($m7);$Zg->apply_unsafe($m7);$Zg->apply_unsafe($o7);$mh->apply_unsafe($m7);$wh->apply_unsafe($m7);$wh->apply_unsafe($o7);$Th->apply_unsafe($n7);$di->apply_unsafe($n7);$ki->apply_unsafe($n7);$Ai->apply_unsafe($n7);$Ii->apply_unsafe($n7);$fj->apply_unsafe($n7);$Cj->apply_unsafe($o7);$Lj->apply_unsafe($o7);$zk->apply_unsafe($Ak);$Kk->apply_unsafe($p7);$Xk->apply_unsafe($p7);$Gl->apply_unsafe($r7);$Ql->apply_unsafe($r7);$em->apply_unsafe($r7);$om->apply_unsafe($r7);$Em->apply_unsafe($r7);$mn->apply_unsafe($s7);$tn->apply_unsafe($s7);$Hn->apply_unsafe($s7);$fo->apply_unsafe($t7);$fo->apply_unsafe($u7);$fo->apply_unsafe($v7);$fo->apply_unsafe($F7);$ro->apply_unsafe($t7);$ro->apply_unsafe($u7);$ro->apply_unsafe($v7);$ro->apply_unsafe($F7);$Ho->apply_unsafe($t7);$Ho->apply_unsafe($u7);$Ho->apply_unsafe($v7);$dp->apply_unsafe($t7);$dp->apply_unsafe($u7);$dp->apply_unsafe($v7);$up->apply_unsafe($u5);$up->apply_unsafe($v5);$up->apply_unsafe($w5);$Qp->apply_unsafe($u7);$Xp->apply_unsafe($u7);$jq->apply_unsafe($u7);$vq->apply_unsafe($u7);$Xq->apply_unsafe($v5);$sr->apply_unsafe($v7);$zr->apply_unsafe($v7);$Vr->apply_unsafe($o6);$rs->apply_unsafe($x7);$xs->apply_unsafe($x7);$Ss->apply_unsafe($N);$Ys->apply_unsafe($N);$ht->apply_unsafe($N);$qt->apply_unsafe($N);$Ct->apply_unsafe($N);$iu->apply_unsafe($A);$zu->apply_unsafe($A);$Iu->apply_unsafe($A);$Qu->apply_unsafe($A);$jv->apply_unsafe($B5);$zv->apply_unsafe($y7);$Mv->apply_unsafe($Ak);$Vv->apply_unsafe($y7);$Lw->apply_unsafe($y7);$Tw->apply_unsafe($y7);$Tw->apply_unsafe($A7);$tx->apply_unsafe($y7);$tx->apply_unsafe($A7);$Jx->apply_unsafe($y7);$Jx->apply_unsafe($A7);$Zx->apply_unsafe($y7);$My->apply_unsafe($Ny);$iz->apply_unsafe($z7);$Az->apply_unsafe($z7);$Jz->apply_unsafe($z7);$Tz->apply_unsafe($z7);$kB->apply_unsafe($Ny);$xB->apply_unsafe($A7);$GB->apply_unsafe($A7);$iC->apply_unsafe($m6);$oC->apply_unsafe($m6);$vC->apply_unsafe($m6);$SC->apply_unsafe($Ak);$eD->apply_unsafe($y6);$kD->apply_unsafe($y6);$HD->apply_unsafe($B7);$HD->apply_unsafe($C7);$RD->apply_unsafe($B7);$hE->apply_unsafe($B7);$ME->apply_unsafe($D);$TE->apply_unsafe($D);$cF->apply_unsafe($D);$mF->apply_unsafe($D);$xF->apply_unsafe($D);$KF->apply_unsafe($J5);$VF->apply_unsafe($J5);$qG->apply_unsafe($D7);$CG->apply_unsafe($D7);$JG->apply_unsafe($D7);$ni::self=$AI;&$_($B)for@$C;&$_($O)for@$P;&$_($V)for@$P;&$_($d1)for@$C;&$_($j1)for@$C;&$_($o1)for@$P;&$_($u1)for@$P;&$_($C1)for@$C;&$_($H1)for@$P;&$_($P1)for@$C;&$_($U1)for@$P;&$_($e2)for@$C;&$_($j2)for@$P;&$_($r2)for@$C;&$_($w2)for@$P;&$_($H2)for@$C;&$_($N2)for@$C;&$_($S2)for@$P;&$_($Y2)for@$P;&$_($g3)for@$C;&$_($m3)for@$C;&$_($s3)for@$C;&$_($x3)for@$P;&$_($E3)for@$P;&$_($N3)for@$C;&$_($T3)for@$C;&$_($Y3)for@$P;&$_($i4)for@$P;&$_($q4)for@$P;&$_($w4)for@$C;&$_($B4)for@$P;&$_($K4)for@$C;&$_($R4)for@$C;&$_($W4)for@$P;&$_($e5)for@$P;&$_($g6)for@$C;&$_($j6)for@$C;&$_($n6)for@$p6;&$_($s6)for@$C;&$_($u6)for@$C;&$_($x6)for@$p6;&$_($C6)for@$C;&$_($F6)for@$p6;&$_($J6)for@$C;&$_($M6)for@$p6;&$_($Q6)for@$C;&$_($T6)for@$p6;&$_($V6)for@$W6;&$_($Z6)for@$C;&$_($e7)for@$C;&$_($h7)for@$p6;&$_($L7)for@$C;&$_($N7)for@$C;&$_($Q7)for@$p6;&$_($S7)for@$T7;&$_($Y7)for@$C;&$_($d8)for@$p6;&$_($f8)for@$g8;&$_($m8)for@$C;&$_($p8)for@$p6;&$_($t8)for@$C;&$_($w8)for@$C;&$_($z8)for@$p6;&$_($D8)for@$C;&$_($G8)for@$p6;&$_($K8)for@$C;&$_($N8)for@$p6;&$_($R8)for@$C;&$_($U8)for@$p6;&$_($W8)for@$X8;&$_($Z8)for@$c9;&$_($g9)for@$C;&$_($j9)for@$p6;&$_($n9)for@$C;&$_($q9)for@$p6;&$_($s9)for@$t9;&$_($C9)for@$D9;&$_($H9)for@$D9;&$_($J9)for@$D9;&$_($L9)for@$D9;&$_($R9)for@$C;&$_($V9)for@$C;&$_($Z9)for@$C;&$_($ea)for@$p6;&$_($ia)for@$C;&$_($la)for@$p6;&$_($qa)for@$C;&$_($ua)for@$C;&$_($ya)for@$C;&$_($Ba)for@$p6;&$_($Ga)for@$C;&$_($Ka)for@$C;&$_($Oa)for@$C;&$_($Sa)for@$C;&$_($Va)for@$p6;&$_($Xa)for@$Ya;&$_($fb)for@$D9;&$_($rb)for@$C;&$_($ub)for@$p6;&$_($zb)for@$C;&$_($Db)for@$C;&$_($Hb)for@$C;&$_($Lb)for@$C;&$_($Ob)for@$p6;&$_($Tb)for@$C;&$_($Wb)for@$p6;&$_($dc)for@$C;&$_($hc)for@$C;&$_($kc)for@$p6;&$_($pc)for@$C;&$_($tc)for@$C;&$_($wc)for@$p6;&$_($Bc)for@$C;&$_($Fc)for@$C;&$_($Ic)for@$p6;&$_($Kc)for@$Lc;&$_($Pc)for@$C;&$_($Sc)for@$p6;&$_($Xc)for@$C;&$_($dd)for@$C;&$_($hd)for@$C;&$_($ld)for@$C;&$_($od)for@$p6;&$_($qd)for@$rd;&$_($Bd)for@$C;&$_($Ed)for@$p6;&$_($Gd)for@$D9;&$_($Id)for@$D9;&$_($Qd)for@$C;&$_($Td)for@$p6;&$_($Xd)for@$C;&$_($ce)for@$p6;&$_($ee)for@$fe;&$_($ke)for@$D9;&$_($te)for@$C;&$_($we)for@$p6;&$_($Ae)for@$C;&$_($De)for@$p6;&$_($Ie)for@$C;&$_($Me)for@$C;&$_($Qe)for@$C;&$_($Ue)for@$C;&$_($Xe)for@$p6;&$_($ef)for@$C;&$_($if)for@$C;&$_($mf)for@$C;&$_($qf)for@$C;&$_($uf)for@$C;&$_($xf)for@$p6;&$_($Cf)for@$C;&$_($Ff)for@$p6;&$_($Kf)for@$C;&$_($Of)for@$C;&$_($Sf)for@$C;&$_($Vf)for@$p6;&$_($Xf)for@$Yf;&$_($fg)for@$D9;&$_($rg)for@$C;&$_($ug)for@$p6;&$_($yg)for@$C;&$_($Bg)for@$p6;&$_($Gg)for@$C;&$_($Jg)for@$p6;&$_($Og)for@$C;&$_($Sg)for@$C;&$_($Wg)for@$C;&$_($Zg)for@$p6;&$_($fh)for@$C;&$_($jh)for@$C;&$_($mh)for@$p6;&$_($qh)for@$C;&$_($th)for@$C;&$_($wh)for@$p6;&$_($yh)for@$zh;&$_($Eh)for@$D9;&$_($Qh)for@$C;&$_($Th)for@$p6;&$_($Yh)for@$C;&$_($di)for@$p6;&$_($hi)for@$C;&$_($ki)for@$p6;&$_($pi)for@$C;&$_($ti)for@$C;&$_($xi)for@$C;&$_($Ai)for@$p6;&$_($Fi)for@$C;&$_($Ii)for@$p6;&$_($Mi)for@$C;&$_($Qi)for@$C;&$_($Ti)for@$C;&$_($Xi)for@$C;&$_($cj)for@$C;&$_($fj)for@$p6;&$_($hj)for@$ij;&$_($nj)for@$D9;&$_($zj)for@$C;&$_($Cj)for@$p6;&$_($Fj)for@$C;&$_($Ij)for@$C;&$_($Lj)for@$p6;&$_($Nj)for@$Oj;&$_($Tj)for@$D9;&$_($dk)for@$C;&$_($gk)for@$C;&$_($kk)for@$C;&$_($ok)for@$C;&$_($sk)for@$C;&$_($wk)for@$C;&$_($zk)for@$p6;&$_($Hk)for@$C;&$_($Kk)for@$p6;&$_($Ok)for@$C;&$_($Rk)for@$C;&$_($Uk)for@$C;&$_($Xk)for@$p6;&$_($Zk)for@$cl;&$_($hl)for@$D9;&$_($zl)for@$C;&$_($Dl)for@$C;&$_($Gl)for@$p6;&$_($Kl)for@$C;&$_($Nl)for@$C;&$_($Ql)for@$p6;&$_($Vl)for@$C;&$_($Zl)for@$C;&$_($em)for@$p6;&$_($im)for@$C;&$_($lm)for@$C;&$_($om)for@$p6;&$_($sm)for@$C;&$_($vm)for@$C;&$_($ym)for@$C;&$_($Bm)for@$C;&$_($Em)for@$p6;&$_($Gm)for@$Hm;&$_($Mm)for@$D9;&$_($Zm)for@$C;&$_($fn)for@$C;&$_($jn)for@$C;&$_($mn)for@$p6;&$_($qn)for@$C;&$_($tn)for@$p6;&$_($xn)for@$C;&$_($Bn)for@$C;&$_($En)for@$C;&$_($Hn)for@$p6;&$_($Jn)for@$Kn;&$_($Pn)for@$D9;&$_($co)for@$C;&$_($fo)for@$p6;&$_($ko)for@$C;&$_($oo)for@$C;&$_($ro)for@$p6;&$_($to)for@$uo;&$_($yo)for@$C;&$_($Bo)for@$C;&$_($Eo)for@$C;&$_($Ho)for@$p6;&$_($Mo)for@$C;&$_($Qo)for@$C;&$_($Uo)for@$C;&$_($Yo)for@$C;&$_($dp)for@$p6;&$_($fp)for@$gp;&$_($np)for@$D9;&$_($rp)for@$C;&$_($up)for@$p6;&$_($wp)for@$D9;&$_($Fp)for@$C;&$_($Jp)for@$C;&$_($Np)for@$C;&$_($Qp)for@$p6;&$_($Up)for@$C;&$_($Xp)for@$p6;&$_($dq)for@$C;&$_($gq)for@$C;&$_($jq)for@$p6;&$_($oq)for@$C;&$_($sq)for@$C;&$_($vq)for@$p6;&$_($Aq)for@$Bq;&$_($Iq)for@$C;&$_($Mq)for@$C;&$_($Qq)for@$C;&$_($Uq)for@$C;&$_($Xq)for@$p6;&$_($Zq)for@$D9;&$_($pr)for@$C;&$_($sr)for@$p6;&$_($wr)for@$C;&$_($zr)for@$p6;&$_($Br)for@$Cr;&$_($Hr)for@$D9;&$_($Sr)for@$C;&$_($Vr)for@$p6;&$_($Xr)for@$Yr;&$_($gs)for@$D9;&$_($os)for@$C;&$_($rs)for@$p6;&$_($us)for@$C;&$_($xs)for@$p6;&$_($zs)for@$As;&$_($Fs)for@$D9;&$_($Ps)for@$C;&$_($Ss)for@$p6;&$_($Vs)for@$C;&$_($Ys)for@$p6;&$_($et)for@$C;&$_($ht)for@$p6;&$_($kt)for@$C;&$_($nt)for@$C;&$_($qt)for@$p6;&$_($tt)for@$C;&$_($wt)for@$C;&$_($zt)for@$C;&$_($Ct)for@$p6;&$_($Et)for@$Ft;&$_($Kt)for@$D9;&$_($Wt)for@$C;&$_($Yt)for@$C;&$_($du)for@$C;&$_($fu)for@$C;&$_($iu)for@$p6;&$_($mu)for@$C;&$_($pu)for@$C;&$_($su)for@$C;&$_($wu)for@$C;&$_($zu)for@$p6;&$_($Du)for@$C;&$_($Fu)for@$C;&$_($Iu)for@$p6;&$_($Nu)for@$C;&$_($Qu)for@$p6;&$_($Su)for@$Tu;&$_($cv)for@$C;&$_($gv)for@$C;&$_($jv)for@$p6;&$_($lv)for@$D9;&$_($wv)for@$C;&$_($zv)for@$p6;&$_($Fv)for@$C;&$_($Jv)for@$C;&$_($Mv)for@$p6;&$_($Sv)for@$C;&$_($Vv)for@$p6;&$_($cw)for@$C;&$_($gw)for@$C;&$_($kw)for@$C;&$_($ow)for@$C;&$_($sw)for@$C;&$_($ww)for@$C;&$_($Aw)for@$C;&$_($Ew)for@$C;&$_($Iw)for@$C;&$_($Lw)for@$p6;&$_($Qw)for@$C;&$_($Tw)for@$p6;&$_($Yw)for@$C;&$_($ex)for@$C;&$_($ix)for@$C;&$_($mx)for@$C;&$_($qx)for@$C;&$_($tx)for@$p6;&$_($yx)for@$C;&$_($Cx)for@$C;&$_($Gx)for@$C;&$_($Jx)for@$p6;&$_($Ox)for@$C;&$_($Sx)for@$C;&$_($Wx)for@$C;&$_($Zx)for@$p6;&$_($dy)for@$ey;&$_($jy)for@$D9;&$_($ty)for@$C;&$_($xy)for@$C;&$_($By)for@$C;&$_($Fy)for@$C;&$_($Jy)for@$C;&$_($My)for@$p6;&$_($Xy)for@$C;&$_($cz)for@$C;&$_($fz)for@$C;&$_($iz)for@$p6;&$_($mz)for@$C;&$_($pz)for@$C;&$_($sz)for@$C;&$_($vz)for@$C;&$_($xz)for@$C;&$_($Az)for@$p6;&$_($Dz)for@$C;&$_($Gz)for@$C;&$_($Jz)for@$p6;&$_($Nz)for@$C;&$_($Qz)for@$C;&$_($Tz)for@$p6;&$_($Vz)for@$Wz;&$_($dA)for@$D9;&$_($nA)for@$C;&$_($rA)for@$C;&$_($vA)for@$C;&$_($zA)for@$C;&$_($DA)for@$C;&$_($HA)for@$C;&$_($LA)for@$C;&$_($PA)for@$C;&$_($TA)for@$C;&$_($XA)for@$C;&$_($dB)for@$C;&$_($hB)for@$C;&$_($kB)for@$p6;&$_($uB)for@$C;&$_($xB)for@$p6;&$_($AB)for@$C;&$_($DB)for@$C;&$_($GB)for@$p6;&$_($IB)for@$JB;&$_($OB)for@$D9;&$_($XB)for@$C;&$_($ZB)for@$C;&$_($iC)for@$p6;&$_($lC)for@$C;&$_($oC)for@$p6;&$_($sC)for@$C;&$_($vC)for@$p6;&$_($xC)for@$yC;&$_($EC)for@$D9;&$_($LC)for@$C;&$_($PC)for@$C;&$_($SC)for@$p6;&$_($ZC)for@$C;&$_($eD)for@$p6;&$_($hD)for@$C;&$_($kD)for@$p6;&$_($mD)for@$nD;&$_($tD)for@$D9;&$_($ED)for@$C;&$_($HD)for@$p6;&$_($JD)for@$KD;&$_($OD)for@$C;&$_($RD)for@$p6;&$_($VD)for@$C;&$_($ZD)for@$C;&$_($eE)for@$C;&$_($hE)for@$p6;&$_($jE)for@$kE;&$_($rE)for@$D9;&$_($tE)for@$D9;&$_($GE)for@$C;&$_($JE)for@$C;&$_($ME)for@$p6;&$_($QE)for@$C;&$_($TE)for@$p6;&$_($XE)for@$C;&$_($cF)for@$p6;&$_($gF)for@$C;&$_($jF)for@$C;&$_($mF)for@$p6;&$_($rF)for@$C;&$_($uF)for@$C;&$_($xF)for@$p6;&$_($zF)for@$AF;&$_($HF)for@$C;&$_($KF)for@$p6;&$_($OF)for@$C;&$_($SF)for@$C;&$_($VF)for@$p6;&$_($XF)for@$D9;&$_($nG)for@$C;&$_($qG)for@$p6;&$_($vG)for@$C;&$_($zG)for@$C;&$_($CG)for@$p6;&$_($GG)for@$C;&$_($JG)for@$p6;&$_($LG)for@$MG;&$_($RG)for@$D9;&$_($ZG)for@$cH;&$_($hH)for@$D9;&$_($qH)for@$rH;&$_($wH)for@$D9;&$_($EH)for@$FH;&$_($dI)for@$FH;&$_($iI)for@$C;&$_($mI)for@$C;&$_($qI)for@$C;&$_($uI)for@$C;&$_($yI)for@$C;ni->run(@ARGV);
__DATA__
