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
now foo->f == 'hi';#;$x=q#eval_number#;$y=q#proto#;$z=q##;$A=q#lib/fn#;$B=bless({$t,$u,$v,$w,$x,498,$y,$z},$A);$C=q#lib/fn::ctors#;$D=q#lib/test_case#;$E=bless({$n,$o,$p,$q,$r,$q,$s,$B},$D);$F=q#TODO...#;$G=[$l,$m,$E,$F];$H=q#classes#;$I=q#ni implements a Smalltalk 80-style metaclass system with a couple of
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
now [$f1->v, $f2->v] == [5, 6];#;$d1=bless({$t,$Z,$v,$c1,$x,502,$y,$z},$A);$e1=bless({$n,$Y,$p,$q,$r,$q,$s,$d1},$D);$f1=q#You can combine multiple futures in different ways depending on what
      you're trying to do.#;$g1=[];$h1=[];$i1=q#my $f1 = ni('ni:/fabric/future')->new;
my $f2 = ni('ni:/fabric/future')->new;
my $f3 = $f1->or($f2);
my $f4 = $f1->and($f2);
$f1->decide(1);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, undef, 1, undef];
$f2->decide(2);
now [$f1->v, $f2->v, $f3->v, $f4->v] == [1, 2, 1, [1, 2]];#;$j1=bless({$t,$h1,$v,$i1,$x,504,$y,$z},$A);$k1=bless({$n,$g1,$p,$q,$r,$q,$s,$j1},$D);$l1=[$i,$X,$e1,$f1,$k1];$m1=[$l1];$n1=q#/fabric/future#;$o1=bless({$e,$m1,$L,$n1},$N);$p1=q#ni.doc:/fabric/perl#;$q1=q#A perl interpreter running somewhere. In the fabric sense, this means
      something that has a name table whose entries can respond to method
      calls.#;$r1=q#There are some interesting design considerations that go into this.
      First, we want to jointly minimize the running transmission size while
      also being careful not to leak memory by caching things. A naive setup
      like the one ni uses to serialize itself would meet the first requirement
      but not the second.#;$s1=q#Second, we don't want to incur parsing overhead for every request we
      make.#;$t1=[$i,$q1,$r1,$s1];$u1=[$t1];$v1=q#/fabric/perl#;$w1=bless({$e,$u1,$L,$v1},$N);$x1=q#ni.doc:/io#;$y1=q#An implementation of IO in terms of system-level FDs. We need this for a
      few reasons, three of them being that (1) old versions of Perl don't
      correctly handle interrupted system calls, (2) we want tighter control
      over which FDs are closed at what times, and (3) we want to be able to
      "unread" things -- push back against the read buffer (or use a custom
      read format in general).#;$z1=[$i,$y1];$A1=[$z1];$B1=q#/io#;$C1=bless({$e,$A1,$L,$B1},$N);$D1=q#ni.doc:/io/buffer#;$E1=q#
    my $buf = ni("ni:/io/buffer")->new(8192);
    $buf->write("foo");
    $buf->read($_, 256);        \# reads "foo"#;$F1=[$f,$E1];$G1=q#A bounded, memory-backed FIFO data queue. In IO terms, this object
      behaves like a nonblocking socket and sets errno accordingly.#;$H1=[];$I1=[];$J1=q#my $buf = ni('ni:/io/buffer')->new(128);
now [$buf->read_capacity, $buf->write_capacity] == [0, 128];
now $buf->write("foobarbif") == 9;
now [$buf->read_capacity, $buf->write_capacity] == [9, 119];
now [$buf->read($_, 5), $_] == [5, "fooba"];

now $buf->write(" " x 125) == 124;
now $buf->read($_, 120) == 120;
now [$buf->read($_, 100), $_, $buf->read_capacity] == [8, " " x 8, 0];
now [$buf->read($_, 100), 0 + $!] == [undef, Errno::EWOULDBLOCK];#;$K1=bless({$t,$I1,$v,$J1,$x,506,$y,$z},$A);$L1=bless({$n,$H1,$p,$q,$r,$q,$s,$K1},$D);$M1=[$i,$G1,$L1];$N1=[$F1,$M1];$O1=q#/io/buffer#;$P1=bless({$e,$N1,$L,$O1},$N);$Q1=q#ni.doc:/io/cat#;$R1=q#
    my $combined = ni('ni:/io/cat')->new($io1, $io2, ...);
    my $combined = $io1 + $io2 + $io3;
    $combined->into_sync($destination_io);
  #;$S1=[$f,$R1];$T1=q#Concatenates multiple IO objects into a single read-only data source.
      This is a mutable object that consumes its inputs and then loses its
      references to them as quickly as possible, allowing their resources to be
      freed. Once fully consumed, the cat object holds no references.#;$U1=[];$V1=[];$W1=q#my $cat = ni('str:foo') + ni('str:bar');
now [$cat->read($_, 16), $_] == [8, "foo\\nbar\\n"];#;$X1=bless({$t,$V1,$v,$W1,$x,508,$y,$z},$A);$Y1=bless({$n,$U1,$p,$q,$r,$q,$s,$X1},$D);$Z1=[$i,$T1,$Y1];$c2=[$S1,$Z1];$d2=q#/io/cat#;$e2=bless({$e,$c2,$L,$d2},$N);$f2=q#ni.doc:/io/exec#;$g2=q#
    my $pid = ni("ni:/io/exec")->new("ls", "-l")
      ->connect(1 => ni("file:foo")->w)
      ->env(ENV_VAR => "value", ENV2 => "val2")
      ->fork;
    $? = $pid->await or die "ls -l failed: $?";#;$h2=[$f,$g2];$i2=q#An object that represents a fork+exec operation that hasn't yet happened.
      It allows you to incrementally specify the context of the process,
      including environment variables and file descriptor mappings. It is also
      an IO object and will set up pipes to stdin/out if you use it this way.#;$j2=[];$k2=[];$l2=q#my $e   = ni('ni:/io/exec')->new('echo', 'hi');
my $out = $e->stdout;
my $pid = $e->fork;
now [$out->read_all, $pid->await] == ["hi\\n", 0];#;$m2=bless({$t,$k2,$v,$l2,$x,510,$y,$z},$A);$n2=bless({$n,$j2,$p,$q,$r,$q,$s,$m2},$D);$o2=[$i,$i2,$n2];$p2=[$h2,$o2];$q2=q#/io/exec#;$r2=bless({$e,$p2,$L,$q2},$N);$s2=q#ni.doc:/io/fd#;$t2=q#
    open my $fh, ...;
    my $fd = ni('ni:/io/fd')->new($fh); \# from perl FH
    my $fd = ni('ni:/io/fd')->new(0);   \# from number
    my $fd = ni('fd:0');                \# same thing
    $fd->nonblock(1)->read($_, 100);
    $fd->be(10);                        \# move FD number
  #;$u2=[$f,$t2];$v2=q#Represents a file descriptor as a child of /io/object (so the usual IO
      methods like into_async are available), and provides some convenience
      functions for things like setting up FDs for child processes. FDs are
      closed when destroyed.#;$w2=[];$x2=[];$y2=q#my ($r, $w) = POSIX::pipe;
{
  my $fd = ni('ni:/io/fd')->new($r);
}
ni('ni:/io/fd')->new($w)->be($r);   \# fails unless $r was GC-closed#;$z2=bless({$t,$x2,$v,$y2,$x,512,$y,$z},$A);$A2=bless({$n,$w2,$p,$q,$r,$q,$s,$z2},$D);$B2=[$i,$v2,$A2];$C2=[$u2,$B2];$D2=q#/io/fd#;$E2=bless({$e,$C2,$L,$D2},$N);$F2=q#ni.doc:/io/file#;$G2=q#
    my $f = ni('ni:/io/file')->new('/etc/passwd');
    my $f = ni('file:/etc/passwd');     \# same as above
    $f->into_sync(ni('fd:1'));          \# cat to stdout
  #;$H2=[$f,$G2];$I2=q#warning#;$J2=q#Files overload the -X file test operators, but this feature wasn't
      introduced until Perl 5.12 -- prior versions won't recognize this
      overload. That means that using this overload in ni's base code will
      reduce its portability and cause tests to fail.#;$K2=[$I2,$J2];$L2=q#Represents a file that may or may not exist, and stores/constructs file
      descriptors for reading/writing. /io/files are one-shot objects: once
      you've consumed them for reading or written to them, you should destroy
      the object and start over (or close the file) if you want to operate on
      the file further -- put differently, /io/file objects own the FDs they
      create.#;$M2=[];$N2=[];$O2=q#my $ni = ni('file:/dev/zero');
now [$ni->read($_, 8), $_] == [8, "\\0" x 8];#;$P2=bless({$t,$N2,$v,$O2,$x,514,$y,$z},$A);$Q2=bless({$n,$M2,$p,$q,$r,$q,$s,$P2},$D);$R2=q#File objects also provide some useful functions like atomic-updating.
      This lets you write a stream slowly into a tempfile, then rename over the
      original once the tempfile is closed. ni uses this to update itself to
      avoid race conditions.#;$S2=[];$T2=[];$U2=q#ni('file:tmp1')->write_all("original contents");
{
  my $updater = ni('file:tmp1')->atomic_update;
  $updater->write_all('foo bar');
  now ni('file:tmp1')->read_all == "original contents";
}
now ni('file:tmp1')->read_all == "foo bar";
ni('file:tmp1')->rm;#;$V2=bless({$t,$T2,$v,$U2,$x,516,$y,$z},$A);$W2=bless({$n,$S2,$p,$q,$r,$q,$s,$V2},$D);$X2=[$i,$L2,$Q2,$R2,$W2];$Y2=[$H2,$K2,$X2];$Z2=q#/io/file#;$c3=bless({$e,$Y2,$L,$Z2},$N);$d3=q#ni.doc:/io/file_update_fd#;$e3=q#A write fd that performs a file rename upon closing.#;$f3=[$i,$e3];$g3=[$f3];$h3=q#/io/file_update_fd#;$i3=bless({$e,$g3,$L,$h3},$N);$j3=q#ni.doc:/io/pid#;$k3=q#eg#;$l3=[];$m3=[];$n3=q#now [ni('sh:true')->await, ni('sh:false')->await] == [0, 1 << 8];#;$o3=bless({$t,$m3,$v,$n3,$x,518,$y,$z},$A);$p3=bless({$n,$l3,$p,$q,$r,$q,$s,$o3},$D);$q3=[$k3,$p3];$r3=[];$s3=[];$t3=q#my $pid = ni('sh:seq 4');
my $data = $pid->read_all;
now [$pid->await, $data] == [0, "1\\n2\\n3\\n4\\n"];#;$u3=bless({$t,$s3,$v,$t3,$x,520,$y,$z},$A);$v3=bless({$n,$r3,$p,$q,$r,$q,$s,$u3},$D);$w3=[$k3,$v3];$x3=[];$y3=[];$z3=q#my $seq = ni('sh:seq 10');
my $grep = ni('ni:/io/exec')->new('egrep', '[13579]$')
  ->connect(0 => $seq->stdout)
  ->fork;
now [$grep->read_all, $seq->await, $grep->await]
 == ["1\\n3\\n5\\n7\\n9\\n", 0, 0];#;$A3=bless({$t,$y3,$v,$z3,$x,522,$y,$z},$A);$B3=bless({$n,$x3,$p,$q,$r,$q,$s,$A3},$D);$C3=[$k3,$B3];$D3=[$q3,$w3,$C3];$E3=q#/io/pid#;$F3=bless({$e,$D3,$L,$E3},$N);$G3=q#ni.doc:/lib#;$H3=q#Bootstrapping code for the core abstractions in ni, and almost everything
      about its introspection. This includes definitions for documentation,
      unit tests, classes, support for basic image generation, etc -- and when
      possible, it's written with some awareness of downstream use cases (for
      instance, image serialization and RMI share logic).#;$I3=q#/lib is the place where things don't quite work yet, so the code here is
      written differently from other modules.#;$J3=[$i,$H3,$I3];$K3=[$J3];$L3=q#/lib#;$M3=bless({$e,$K3,$L,$L3},$N);$N3=q#ni.doc:/lib/doc#;$O3=q#
    ni("ni:/some/class")->doc
      ->synopsis(...)
      ->description(...)
      ->eg(...)
      ...#;$P3=[$f,$O3];$Q3=q#Associate documentation with the specified class. Documentation is stored
      separately and in the "ni.doc" namespace; this way you can serialize
      instances of the class and the class's code without bringing along all of
      its documentation and unit tests.#;$R3=q#Documentation objects are internally represented as arrays of quoted
      method calls:#;$S3=[];$T3=[];$U3=q#my $doc = ni("ni:/lib/doc")->new("foo");
now $doc->{doc} == [];
$doc->foo("bar bif baz");
now $doc->{doc} == [["foo", "bar bif baz"]];#;$V3=bless({$t,$T3,$v,$U3,$x,524,$y,$z},$A);$W3=bless({$n,$S3,$p,$q,$r,$q,$s,$V3},$D);$X3=q#This documentation can later be compiled into things like manpages,
      markdown, or HTML by target-specific conversion functions. Documentation
      also stores unit tests, which are specified using "eg":#;$Y3=[];$Z3=[];$c4=q#my $doc = ni("ni:/lib/doc")->new("foo");
my $passing_test = q{return 1};
my $failing_test = q{return 0};
$doc->eg($passing_test)
    ->description(q[Foo objects are contrived examples.],
                  eg $failing_test,
                  q[So there.]);
now scalar($doc->tests) == 2;#;$d4=bless({$t,$Z3,$v,$c4,$x,526,$y,$z},$A);$e4=bless({$n,$Y3,$p,$q,$r,$q,$s,$d4},$D);$f4=[$i,$Q3,$R3,$W3,$X3,$e4];$g4=[$P3,$f4];$h4=q#/lib/doc#;$i4=bless({$e,$g4,$L,$h4},$N);$j4=q#ni.doc:/lib/image#;$k4=q#
    my $image = ni("ni:/lib/image")->new;
    my $gensym = $image->quote($value);
    $image->io->into_sync($a_file);#;$l4=[$f,$k4];$m4=q#Generates Perl code that reconstructs the state of objects at the
      behavior/slice level. Since classes are self-describing, this results in
      a replica of the runtime object-oriented state.#;$n4=[$i,$m4];$o4=[$l4,$n4];$p4=q#/lib/image#;$q4=bless({$e,$o4,$L,$p4},$N);$r4=q#ni.doc:/lib/ni#;$s4=q#my $value = ni->resolve($name);
               my $value = ni($name);   \# alias for ni->resolve($name)
               my $self  = ni;#;$t4=[$f,$s4];$u4=q#The class for the currently-running ni instance. This includes all
      instance state, the table of named objects, and a bit of logic to update
      ni in place, for instance when adding extensions.#;$v4=[$i,$u4];$w4=[$t4,$v4];$x4=q#/lib/ni#;$y4=bless({$e,$w4,$L,$x4},$N);$z4=q#ni.doc:/lib/quote_simple#;$A4=q#A stateless object that serializes values with direct quotation; that
      is, the serialization contains no variables. If your objects have
      circular or shared references, you should probably use
      /lib/quote_circular or similar.#;$B4=[];$C4=[];$D4=q#my $q = ni('ni:/lib/quote_simple')->new;
now $q->quote([1,2,3]) == "[1,2,3]";
now $q->quote({foo => 1, bar => [1, 2]}) == "{q\#bar\#,[1,2],q\#foo\#,1}";#;$E4=bless({$t,$C4,$v,$D4,$x,528,$y,$z},$A);$F4=bless({$n,$B4,$p,$q,$r,$q,$s,$E4},$D);$G4=[$i,$A4,$F4];$H4=[$G4];$I4=q#/lib/quote_simple#;$J4=bless({$e,$H4,$L,$I4},$N);$K4=q#ni.doc:/lib/slice#;$L4=q#
    ni('ni:/lib/slice')->new('/lib/foo',
      ctor => fn q{shift->say_hi},
      say_hi => fn q{print "hi from " . shift->name . "\\n"});
    $some_class->add('/lib/foo');#;$M4=[$f,$L4];$N4=q#A slice of methods encoding some aspect of an object's behavior. Slices
      are combined using tags and branches, and the set of slices used to
      construct a class must be disjoint except for constructors and
      destructors.#;$O4=q#Slices are objects that provide an ->apply method, which installs their
      methods + ctors + dtors into a Perl package.#;$P4=[];$Q4=[];$R4=q#my $slice = ni('ni:/lib/slice')->new('foo1', foo => fn q{"bar"});
$slice->apply('test::foo1');
now bless({}, 'test::foo1')->foo == 'bar';#;$S4=bless({$t,$Q4,$v,$R4,$x,530,$y,$z},$A);$T4=bless({$n,$P4,$p,$q,$r,$q,$s,$S4},$D);$U4=q#Slices automatically do the equivalent of using Perl's "overload" module
      if any methods begin with an open-paren.#;$V4=q#Classes automatically incorporate some special low-level slices that are
      used by others; one of these is /lib/instantiable.b, which implements
      ->new and ->DESTROY. These methods then call into the lists of
      constructors and destructors implemented when slices are added to a
      package.#;$W4=[];$X4=[];$Y4=q#my $instances = 0;
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
now $instances == 0;#;$Z4=bless({$t,$X4,$v,$Y4,$x,532,$y,$z},$A);$c5=bless({$n,$W4,$p,$q,$r,$q,$s,$Z4},$D);$d5=[$i,$N4,$O4,$T4,$U4,$V4,$c5];$e5=[$M4,$d5];$f5=q#/lib/slice#;$g5=bless({$e,$e5,$L,$f5},$N);$h5=q#ni.doc:/semantic#;$i5=q#Opportunities to assign real-world semantics to objects. This is a
      collection of behaviors that don't necessarily imply a Perl-level
      protocol, but which may end up meaning something at some point.#;$j5=[$i,$i5];$k5=[$j5];$l5=q#/semantic#;$m5=bless({$e,$k5,$L,$l5},$N);$n5=q#ni:/class#;$o5=q#applied_to#;$p5=q#class#;$q5=q#class.c#;$r5=q#fabric/future.c#;$s5=q#fabric/perl.c#;$t5=q#fabric/perl_proxy.c#;$u5=q#io/buffer.c#;$v5=q#io/cat.c#;$w5=q#io/exec.c#;$x5=q#io/fd.c#;$y5=q#io/file.c#;$z5=q#io/file_update_fd.c#;$A5=q#io/null.c#;$B5=q#io/object.c#;$C5=q#io/pid.c#;$D5=q#io/str.c#;$E5=q#io/transfer.c#;$F5=q#io/transfer_async.c#;$G5=q#io/transfer_sync.c#;$H5=q#lib/behavior.c#;$I5=q#lib/branch.c#;$J5=q#lib/dataslice.c#;$K5=q#lib/doc.c#;$L5=q#lib/fn.c#;$M5=q#lib/image.c#;$N5=q#lib/ni.c#;$O5=q#lib/quote_simple.c#;$P5=q#lib/slice.c#;$Q5=q#lib/tag.c#;$R5=q#lib/test_assert_eq.c#;$S5=q#lib/test_assertion.c#;$T5=q#lib/test_case.c#;$U5=q#lib/test_value.c#;$V5=q#metaclass.c#;$W5=q#module.c#;$X5=q#object.c#;$Y5=q#semantic/dimension#;$Z5=q#semantic/dimension.c#;$c6=q#semantic/task.c#;$d6={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$V5,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$c6,1};$e6=q#slices#;$f6=q#metaclass#;$g6=q#module#;$h6={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$f6,1,$V5,1,$g6,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$c6,1};$i6=q#/module#;$j6=q#/lib/perlbranch.b#;$k6={};$l6=q#ctor#;$m6=q#dtor#;$n6=q#methods#;$o6=q#add#;$p6=q#local $_;
my $self = shift;
my @s = map $self->resolve($_), @_;
push @{$$self{slices}}, @s;
for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
$self;#;$q6=bless({$v,$p6,$x,534,$y,$z},$A);$r6=q#apply#;$s6=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
$$self{applied_to}{$p} = 1;
$_->apply($p) for @{$$self{slices}};
$self;#;$t6=bless({$v,$s6,$x,536,$y,$z},$A);$u6={$o6,$q6,$r6,$t6};$v6=q#/lib/branch.b#;$w6=q#lib/slice#;$x6=bless({$o5,$k6,$l6,$q,$m6,$q,$n6,$u6,$L,$v6},$w6);$y6=q#lib/branch#;$z6=q#lib/slice::ctors#;$A6={};$B6=q#my $s = shift; ni->def($s->name, $s)#;$C6=bless({$v,$B6,$x,538,$y,$z},$A);$D6=q#$_[0]->namespace . ":" . $_[0]->{name}#;$E6=bless({$v,$D6,$x,540,$y,$z},$A);$F6={$L,$E6};$G6=q#/lib/named.b#;$H6=bless({$o5,$A6,$l6,$C6,$m6,$q,$n6,$F6,$L,$G6},$w6);$I6=q#lib/tag#;$J6={};$K6=q#namespace#;$L6=q#'ni'#;$M6=bless({$v,$L6,$x,542,$y,$z},$A);$N6={$K6,$M6};$O6=q#/lib/named_in_ni.b#;$P6=bless({$o5,$J6,$l6,$q,$m6,$q,$n6,$N6,$L,$O6},$w6);$Q6={};$R6=q#package#;$S6=q#(my $name = shift->{name}) =~ s/^\\///; $name#;$T6=bless({$v,$S6,$x,544,$y,$z},$A);$U6={$R6,$T6};$V6=q#/lib/namespaced.b#;$W6=bless({$o5,$Q6,$l6,$q,$m6,$q,$n6,$U6,$L,$V6},$w6);$X6={};$Y6=q#resolve#;$Z6=q#ref $_[1] ? $_[1] : ni"ni:$_[1]"#;$c7=bless({$v,$Z6,$x,546,$y,$z},$A);$d7={$Y6,$c7};$e7=q#/lib/resolver.b#;$f7=bless({$o5,$X6,$l6,$q,$m6,$q,$n6,$d7,$L,$e7},$w6);$g7=[$x6,$H6,$P6,$W6,$f7];$h7=bless({$L,$j6,$e6,$g7},$I6);$i7=q#lib/tag::ctors#;$j7={};$k7=q#my $s = shift; $s->apply($s->package)#;$l7=bless({$v,$k7,$x,548,$y,$z},$A);$m7=q#instantiate#;$n7=q#local $_;
my ($class, $name, @slices) = @_;
+{name   => $name,
  slices => [map $class->resolve($_), @slices]};#;$o7=bless({$v,$n7,$x,550,$y,$z},$A);$p7={$m7,$o7};$q7=q#/lib/class_init.b#;$r7=bless({$o5,$j7,$l6,$l7,$m6,$q,$n6,$p7,$L,$q7},$w6);$s7=q#fabric/future#;$t7=q#fabric/perl#;$u7=q#fabric/perl_proxy#;$v7=q#io/buffer#;$w7=q#io/cat#;$x7=q#io/exec#;$y7=q#io/fd#;$z7=q#io/file#;$A7=q#io/file_update_fd#;$B7=q#io/null#;$C7=q#io/object#;$D7=q#io/pid#;$E7=q#io/str#;$F7=q#io/transfer#;$G7=q#io/transfer_async#;$H7=q#io/transfer_sync#;$I7=q#lib/behavior#;$J7=q#lib/dataslice#;$K7=q#lib/image#;$L7=q#lib/ni#;$M7=q#lib/quote_simple#;$N7=q#lib/test_assert_eq#;$O7=q#lib/test_assertion#;$P7=q#lib/test_value#;$Q7=q#object#;$R7=q#semantic/task#;$S7={$p5,1,$q5,1,$s7,1,$r5,1,$t7,1,$s5,1,$u7,1,$t5,1,$v7,1,$u5,1,$w7,1,$v5,1,$x7,1,$w5,1,$y7,1,$x5,1,$z7,1,$y5,1,$A7,1,$z5,1,$B7,1,$A5,1,$C7,1,$B5,1,$D7,1,$C5,1,$E7,1,$D5,1,$F7,1,$E5,1,$G7,1,$F5,1,$H7,1,$G5,1,$I7,1,$H5,1,$y6,1,$I5,1,$J7,1,$J5,1,$N,1,$K5,1,$A,1,$L5,1,$K7,1,$M5,1,$L7,1,$N5,1,$M7,1,$O5,1,$w6,1,$P5,1,$I6,1,$Q5,1,$N7,1,$R5,1,$O7,1,$S5,1,$D,1,$T5,1,$P7,1,$U5,1,$f6,1,$V5,1,$g6,1,$W5,1,$Q7,1,$X5,1,$Y5,1,$Z5,1,$R7,1,$c6,1};$T7=q#/object#;$U7={};$V7=q#DESTROY#;$W7=q#local $_;
my $self = shift;
defined($_) && $_->($self) for @{ref($self) . '::dtors'};#;$X7=bless({$v,$W7,$x,552,$y,$z},$A);$Y7=q#ni 'ni:/' . ref shift#;$Z7=bless({$v,$Y7,$x,554,$y,$z},$A);$c8={$V7,$X7,$p5,$Z7};$d8=q#/lib/instance.b#;$e8=bless({$o5,$U7,$l6,$q,$m6,$q,$n6,$c8,$L,$d8},$w6);$f8=[$e8];$g8=bless({$o5,$S7,$L,$T7,$e6,$f8},$X5);$h8=q#object.c::ctors#;$i8={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$I7,1,$H5,1,$y6,1,$I5,1,$J7,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$w6,1,$P5,1,$I6,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$f6,1,$V5,1,$g6,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$c6,1};$j8=q#/lib/behavior#;$k8={};$l8=q#my $self = shift;
(my $name = $self->name) =~ s/^[^:]*://;
return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
ni('ni:/lib/doc')->new($name);#;$m8=bless({$v,$l8,$x,556,$y,$z},$A);$n8={$e,$m8};$o8=q#/lib/documentable.b#;$p8=bless({$o5,$k8,$l6,$q,$m6,$q,$n6,$n8,$L,$o8},$w6);$q8=[$g8,$p8];$r8=bless({$o5,$i8,$L,$j8,$e6,$q8},$H5);$s8=q#lib/behavior.c::ctors#;$t8={$p5,1,$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$y6,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$f6,1,$V5,1,$g6,1,$W5,1,$X5,1,$Y5,1,$Z5,1,$c6,1};$u8=q#/lib/definition.b#;$v8={};$w8=q#def#;$x8=q#my $self = shift;
my $name = shift;
$self->add(ni->exists("ni:$name")
  ? ni"ni:$name"
  : ni('ni:/lib/slice')->new($name, @_));
$self;#;$y8=bless({$v,$x8,$x,558,$y,$z},$A);$z8={$w8,$y8};$A8=q#/lib/definition_def.b#;$B8=bless({$o5,$v8,$l6,$q,$m6,$q,$n6,$z8,$L,$A8},$w6);$C8={};$D8=q#ro#;$E8=q#my ($self, $slice, @rs) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{shift->{'$_'}}), @rs));#;$F8=bless({$v,$E8,$x,560,$y,$z},$A);$G8=q#rw#;$H8=q#my ($self, $slice, @as) = @_;
$self->add(ni('ni:/lib/slice')->new(
  $slice,
  map +($_ => fn qq{\\@_ == 2 ? \\$_[0]->{'$_'} = \\$_[1] : shift->{'$_'}}), @as));#;$I8=bless({$v,$H8,$x,562,$y,$z},$A);$J8={$D8,$F8,$G8,$I8};$K8=q#/lib/accessor.b#;$L8=bless({$o5,$C8,$l6,$q,$m6,$q,$n6,$J8,$L,$K8},$w6);$M8={};$N8=q#(""#;$O8=q#shift->name#;$P8=bless({$v,$O8,$x,564,$y,$z},$A);$Q8={$N8,$P8};$R8=q#/lib/name_as_string.b#;$S8=bless({$o5,$M8,$l6,$q,$m6,$q,$n6,$Q8,$L,$R8},$w6);$T8={};$U8=q#(eq#;$V8=q#ref($_[0]) eq ref($_[1])
  and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);#;$W8=bless({$v,$V8,$x,566,$y,$z},$A);$X8={$U8,$W8};$Y8=q#/lib/ref_eq.b#;$Z8=bless({$o5,$T8,$l6,$q,$m6,$q,$n6,$X8,$L,$Y8},$w6);$c9={};$d9=q#defdata#;$e9=q#shift->add(ni('ni:/lib/dataslice')->new(@_))#;$f9=bless({$v,$e9,$x,568,$y,$z},$A);$g9={$d9,$f9};$h9=q#/lib/definition_defdata.b#;$i9=bless({$o5,$c9,$l6,$q,$m6,$q,$n6,$g9,$L,$h9},$w6);$j9=[$B8,$L8,$S8,$Z8,$i9];$k9=bless({$o5,$t8,$L,$u8,$e6,$j9},$y6);$l9=q#lib/branch::ctors#;$m9=[$h7,$r7,$g8,$r8,$k9];$n9=bless({$o5,$h6,$L,$i6,$e6,$m9},$W5);$o9=q#module.c::ctors#;$p9={};$q9=q#new#;$r9=q#local $_;
my $class   = shift;
my $package = ref $class ? $class->package : $class;
my $self    = bless &{"$package\\::instantiate"}($class, @_), $package;
$_->($self) for @{ref($self) . "::ctors"};
$self;#;$s9=bless({$v,$r9,$x,570,$y,$z},$A);$t9={$q9,$s9};$u9=q#/lib/instantiable.b#;$v9=bless({$o5,$p9,$n6,$t9,$L,$u9},$w6);$w9={};$x9=q#child#;$y9=q#my ($self, $name, @slices) = @_;
ni("ni:/metaclass")->new("$name.c", $self->class)
->new($name, $self, @slices);#;$z9=bless({$v,$y9,$x,572,$y,$z},$A);$A9={$x9,$z9};$B9=q#/lib/subclass.b#;$C9=bless({$o5,$w9,$l6,$q,$m6,$q,$n6,$A9,$L,$B9},$w6);$D9=[$n9,$v9,$r7,$n9,$C9];$E9=bless({$o5,$d6,$L,$M,$e6,$D9},$q5);$F9=q#class.c::ctors#;$G9=q#ni:/class.c#;$H9={$q5,1,$Z5,1};$I9=q#/class.c#;$J9={$q5,1,$W5,1,$Z5,1};$K9=q#/module.c#;$L9={$q5,1,$r5,1,$s5,1,$t5,1,$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1,$E5,1,$F5,1,$G5,1,$H5,1,$I5,1,$J5,1,$K5,1,$L5,1,$M5,1,$N5,1,$O5,1,$P5,1,$Q5,1,$R5,1,$S5,1,$T5,1,$U5,1,$W5,1,$X5,1,$Z5,1,$c6,1};$M9=q#/object.c#;$N9=[$E9];$O9=bless({$o5,$L9,$L,$M9,$e6,$N9},$f6);$P9=q#metaclass::ctors#;$Q9={$q5,1,$H5,1,$I5,1,$J5,1,$P5,1,$Q5,1,$W5,1,$Z5,1};$R9=q#/lib/behavior.c#;$S9=[$O9];$T9=bless({$o5,$Q9,$L,$R9,$e6,$S9},$f6);$U9=[$O9,$v9,$T9];$V9=bless({$o5,$J9,$L,$K9,$e6,$U9},$f6);$W9=[$V9];$X9=bless({$o5,$H9,$L,$I9,$e6,$W9},$f6);$Y9=q#ni:/fabric/future#;$Z9={$s7,1};$ca={};$da=[];$ea=q#shift->{'outcome'}#;$fa=bless({$t,$da,$v,$ea,$x,574,$y,$z},$A);$ga=q#parents#;$ha=[];$ia=q#shift->{'parents'}#;$ja=bless({$t,$ha,$v,$ia,$x,576,$y,$z},$A);$ka=q#v#;$la=[];$ma=q#shift->{'v'}#;$na=bless({$t,$la,$v,$ma,$x,578,$y,$z},$A);$oa={$r,$fa,$ga,$ja,$ka,$na};$pa=q#/fabric/future_ro.b#;$qa=bless({$o5,$ca,$l6,$q,$m6,$q,$n6,$oa,$L,$pa},$w6);$ra={};$sa=[];$ta=q#my $class = shift;
+{v         => undef,
  parents   => [@_],
  listeners => [],
  outcome   => undef};#;$ua=bless({$t,$sa,$v,$ta,$x,580,$y,$z},$A);$va={$m7,$ua};$wa=q#/fabric/future_init.b#;$xa=bless({$o5,$ra,$l6,$q,$m6,$q,$n6,$va,$L,$wa},$w6);$ya={};$za=q#decide#;$Aa=[];$Ba=q#local $_;
my ($self, $v) = @_;
die "ni:/fabric/future: cannot change a decided future"
  if $$self{outcome};
$$self{outcome} = [1, $v];
$$self{v} = $v;
defined && &$_($v) for @{$$self{listeners}};
$$self{parents} = $$self{listeners} = undef;
$self;#;$Ca=bless({$t,$Aa,$v,$Ba,$x,582,$y,$z},$A);$Da=q#decided#;$Ea=[];$Fa=q#shift->{outcome}#;$Ga=bless({$t,$Ea,$v,$Fa,$x,584,$y,$z},$A);$Ha=q#listener#;$Ia=[];$Ja=q#my ($self, $l) = @_;
$$self{outcome}
  ? &$l($$self{v})
  : push @{$$self{listeners}}, $l;
$self;#;$Ka=bless({$t,$Ia,$v,$Ja,$x,586,$y,$z},$A);$La={$za,$Ca,$Da,$Ga,$Ha,$Ka};$Ma=q#/fabric/future_state.b#;$Na=bless({$o5,$ya,$l6,$q,$m6,$q,$n6,$La,$L,$Ma},$w6);$Oa={};$Pa=q#and#;$Qa=[];$Ra=q#my $self   = $_[0];
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
$child;#;$Sa=bless({$t,$Qa,$v,$Ra,$x,588,$y,$z},$A);$Ta=q#flatmap#;$Ua=[];$Va=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {&$f(shift)->listener(sub {$child->decide(shift)})});
$child;#;$Wa=bless({$t,$Ua,$v,$Va,$x,590,$y,$z},$A);$Xa=q#map#;$Ya=[];$Za=q#my ($self, $f) = @_;
my $child = $self->class->new($self);
$self->listener(sub {$child->decide(&$f(shift))});
$child;#;$cb=bless({$t,$Ya,$v,$Za,$x,592,$y,$z},$A);$db=q#or#;$eb=[];$fb=q#local $_;
my $self    = $_[0];
my $child   = $self->class->new(@_);
my $trigger = sub {$child->decide(shift) unless $child->decided};
$_->listener($trigger) for @_;
$child;#;$gb=bless({$t,$eb,$v,$fb,$x,594,$y,$z},$A);$hb={$Pa,$Sa,$Ta,$Wa,$Xa,$cb,$db,$gb};$ib=q#/fabric/future_algebra.b#;$jb=bless({$o5,$Oa,$l6,$q,$m6,$q,$n6,$hb,$L,$ib},$w6);$kb=[$g8,$qa,$xa,$Na,$jb];$lb=bless({$o5,$Z9,$L,$n1,$e6,$kb},$r5);$mb=q#fabric/future.c::ctors#;$nb=q#ni:/fabric/future.c#;$ob={$r5,1};$pb=q#/fabric/future.c#;$qb=[$O9];$rb=bless({$o5,$ob,$L,$pb,$e6,$qb},$f6);$sb=q#ni:/fabric/future_algebra.b#;$tb=q#ni:/fabric/future_init.b#;$ub=q#ni:/fabric/future_ro.b#;$vb=q#ni:/fabric/future_state.b#;$wb=q#ni:/fabric/perl#;$xb={$t7,1};$yb={};$zb=[];$Ab=q#my ($class, $stdin, $stdout) = @_;
+{stdin   => $stdin,
  stdout  => $stdout,
  pending => {}};#;$Bb=bless({$t,$zb,$v,$Ab,$x,596,$y,$z},$A);$Cb={$m7,$Bb};$Db=q#/fabric/perl_init.b#;$Eb=bless({$o5,$yb,$l6,$q,$m6,$q,$n6,$Cb,$L,$Db},$w6);$Fb={};$Gb=q#ni#;$Hb=[];$Ib=q#ni('/fabric/perl_proxy')->new(@_)#;$Jb=bless({$t,$Hb,$v,$Ib,$x,598,$y,$z},$A);$Kb={$Gb,$Jb};$Lb=q#/fabric/perl_rmi.b#;$Mb=bless({$o5,$Fb,$l6,$q,$m6,$q,$n6,$Kb,$L,$Lb},$w6);$Nb=[$g8,$Eb,$Mb];$Ob=bless({$o5,$xb,$L,$v1,$e6,$Nb},$s5);$Pb=q#fabric/perl.c::ctors#;$Qb=q#ni:/fabric/perl.c#;$Rb={$s5,1};$Sb=q#/fabric/perl.c#;$Tb=[$O9];$Ub=bless({$o5,$Rb,$L,$Sb,$e6,$Tb},$f6);$Vb=q#ni:/fabric/perl_init.b#;$Wb=q#ni:/fabric/perl_proxy#;$Xb={$u7,1};$Yb=q#/fabric/perl_proxy#;$Zb={};$cc=[];$dc=q#my ($class, $perl, $name) = @_;
+{perl => $perl,
  name => $name};#;$ec=bless({$t,$cc,$v,$dc,$x,600,$y,$z},$A);$fc={$m7,$ec};$gc=q#/fabric/perl_proxy_init.b#;$hc=bless({$o5,$Zb,$l6,$q,$m6,$q,$n6,$fc,$L,$gc},$w6);$ic={};$jc=q#AUTOLOAD#;$kc=[];$lc=q#my $self = shift;
my $method = ${__PACKAGE__ . 'AUTOLOAD'};
$$self{perl}->rmi($$self{name}, $method, @_);#;$mc=bless({$t,$kc,$v,$lc,$x,602,$y,$z},$A);$nc={$jc,$mc};$oc=q#/fabric/perl_proxy_rmi.b#;$pc=bless({$o5,$ic,$l6,$q,$m6,$q,$n6,$nc,$L,$oc},$w6);$qc=[$g8,$hc,$pc];$rc=bless({$o5,$Xb,$L,$Yb,$e6,$qc},$t5);$sc=q#fabric/perl_proxy.c::ctors#;$tc=q#ni:/fabric/perl_proxy.c#;$uc={$t5,1};$vc=q#/fabric/perl_proxy.c#;$wc=[$O9];$xc=bless({$o5,$uc,$L,$vc,$e6,$wc},$f6);$yc=q#ni:/fabric/perl_proxy_init.b#;$zc=q#ni:/fabric/perl_proxy_rmi.b#;$Ac=q#ni:/fabric/perl_rmi.b#;$Bc=q#ni:/io/buffer#;$Cc={$v7,1};$Dc={$v7,1,$w7,1,$x7,1,$y7,1,$z7,1,$A7,1,$B7,1,$C7,1,$D7,1,$E7,1};$Ec=q#/io/object#;$Fc={};$Gc=q#(bool#;$Hc=[];$Ic=bless({$t,$Hc,$v,1,$x,604,$y,$z},$A);$Jc={$Gc,$Ic};$Kc=q#/io/object_ops.b#;$Lc=bless({$o5,$Fc,$l6,$q,$m6,$q,$n6,$Jc,$L,$Kc},$w6);$Mc={};$Nc=q#die#;$Oc=[];$Pc=q#shift; die join " ", @_#;$Qc=bless({$t,$Oc,$v,$Pc,$x,606,$y,$z},$A);$Rc=q#io_check#;$Sc=[];$Tc=q#my $self  = shift;
my $check = shift;
my $fn    = shift;
my $r     = &$fn(@_);
$self->die($fn, $!) unless &$check($r);
$r;#;$Uc=bless({$t,$Sc,$v,$Tc,$x,608,$y,$z},$A);$Vc=q#io_check_defined#;$Wc=[];$Xc=q#shift->io_check(sub {defined shift}, @_)#;$Yc=bless({$t,$Wc,$v,$Xc,$x,610,$y,$z},$A);$Zc=q#io_check_true#;$cd=[];$dd=q#shift->io_check(sub {shift}, @_)#;$ed=bless({$t,$cd,$v,$dd,$x,612,$y,$z},$A);$fd={$Nc,$Qc,$Rc,$Uc,$Vc,$Yc,$Zc,$ed};$gd=q#/io/object_checks.b#;$hd=bless({$o5,$Mc,$l6,$q,$m6,$q,$n6,$fd,$L,$gd},$w6);$id={};$jd=q#(+#;$kd=[];$ld=q#ni('ni:/io/cat')->new(@_[0, 1])#;$md=bless({$t,$kd,$v,$ld,$x,614,$y,$z},$A);$nd={$jd,$md};$od=q#/io/object_constructors.b#;$pd=bless({$o5,$id,$l6,$q,$m6,$q,$n6,$nd,$L,$od},$w6);$qd={};$rd=q#read_all#;$sd=[];$td=q#shift->into_sync(ni('ni:/io/str')->new(my $data = ''));
$data;#;$ud=bless({$t,$sd,$v,$td,$x,616,$y,$z},$A);$vd=q#write_all#;$wd=[];$xd=q#my $self = shift;
ni('ni:/io/str')->new($_[0])->into_sync($self);#;$yd=bless({$t,$wd,$v,$xd,$x,618,$y,$z},$A);$zd={$rd,$ud,$vd,$yd};$Ad=q#/io/object_memory.b#;$Bd=bless({$o5,$qd,$l6,$q,$m6,$q,$n6,$zd,$L,$Ad},$w6);$Cd={};$Dd=q#connect_sync#;$Ed=[];$Fd=q#my ($self, $rhs) = @_;
($self->into_sync($rhs),
 $rhs->into_sync($self));#;$Gd=bless({$t,$Ed,$v,$Fd,$x,620,$y,$z},$A);$Hd=q#into_sync#;$Id=[];$Jd=q#ni('ni:/io/transfer_sync')->new(@_)->run#;$Kd=bless({$t,$Id,$v,$Jd,$x,622,$y,$z},$A);$Ld={$Dd,$Gd,$Hd,$Kd};$Md=q#/io/object_transfer_sync.b#;$Nd=bless({$o5,$Cd,$l6,$q,$m6,$q,$n6,$Ld,$L,$Md},$w6);$Od={};$Pd=q#connect_async#;$Qd=[];$Rd=q#my ($self, $rhs) = @_;
($self->into_async($rhs),
 $rhs->into_async($self));#;$Sd=bless({$t,$Qd,$v,$Rd,$x,624,$y,$z},$A);$Td=q#into_async#;$Ud=[];$Vd=q#ni('ni:/io/transfer_async')->new(@_)->run#;$Wd=bless({$t,$Ud,$v,$Vd,$x,626,$y,$z},$A);$Xd={$Pd,$Sd,$Td,$Wd};$Yd=q#/io/object_transfer_async.b#;$Zd=bless({$o5,$Od,$l6,$q,$m6,$q,$n6,$Xd,$L,$Yd},$w6);$ce=[$g8,$Lc,$hd,$pd,$Bd,$Nd,$Zd,$Zd,$Nd,$Zd,$Nd];$de=bless({$o5,$Dc,$L,$Ec,$e6,$ce},$B5);$ee=q#io/object.c::ctors#;$fe={};$ge=[];$he=q#my ($class, $capacity) = @_;
$capacity ||= 65536;
$class->die("buffer capacity must be a power of two (got $capacity)")
  if $capacity & $capacity - 1;
+{capacity    => $capacity,
  data        => "\\0" x $capacity,
  read_point  => 0,
  write_point => 0};#;$ie=bless({$t,$ge,$v,$he,$x,628,$y,$z},$A);$je={$m7,$ie};$ke=q#/io/buffer_init.b#;$le=bless({$o5,$fe,$l6,$q,$m6,$q,$n6,$je,$L,$ke},$w6);$me={};$ne=q#read#;$oe=[];$pe=q#my $self = shift;
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
}#;$qe=bless({$t,$oe,$v,$pe,$x,630,$y,$z},$A);$re=q#read_capacity#;$se=[];$te=q#my $self = shift;
$$self{write_point} - $$self{read_point};#;$ue=bless({$t,$se,$v,$te,$x,632,$y,$z},$A);$ve=q#write#;$we=[];$xe=q#my $self = shift;
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
}#;$ye=bless({$t,$we,$v,$xe,$x,634,$y,$z},$A);$ze=q#write_capacity#;$Ae=[];$Be=q#my $self = shift;
$$self{capacity} - $$self{write_point} + $$self{read_point};#;$Ce=bless({$t,$Ae,$v,$Be,$x,636,$y,$z},$A);$De={$ne,$qe,$re,$ue,$ve,$ye,$ze,$Ce};$Ee=q#/io/buffer_io.b#;$Fe=bless({$o5,$me,$l6,$q,$m6,$q,$n6,$De,$L,$Ee},$w6);$Ge=[$de,$le,$Fe];$He=bless({$o5,$Cc,$L,$O1,$e6,$Ge},$u5);$Ie=q#io/buffer.c::ctors#;$Je=q#ni:/io/buffer.c#;$Ke={$u5,1};$Le=q#/io/buffer.c#;$Me={$u5,1,$v5,1,$w5,1,$x5,1,$y5,1,$z5,1,$A5,1,$B5,1,$C5,1,$D5,1};$Ne=q#/io/object.c#;$Oe={};$Pe=q#def_transfer_method#;$Qe=[];$Re=q#my ($class, $transfer_class, $method_name) = @_;
my $transfer_name = $transfer_class->name;
$class->def("/io/object_transfer_$method_name.b",
  "into_$method_name" => fn qq{ni('$transfer_name')->new(\\@_)->run},
  "connect_$method_name" => fn qq{
    my (\\$self, \\$rhs) = \\@_;
    (\\$self->into_$method_name(\\$rhs),
     \\$rhs->into_$method_name(\\$self));
  });#;$Se=bless({$t,$Qe,$v,$Re,$x,638,$y,$z},$A);$Te={$Pe,$Se};$Ue=q#/io/object.c_transfer_def.b#;$Ve=bless({$o5,$Oe,$l6,$q,$m6,$q,$n6,$Te,$L,$Ue},$w6);$We=[$O9,$Ve];$Xe=bless({$o5,$Me,$L,$Ne,$e6,$We},$f6);$Ye=[$Xe];$Ze=bless({$o5,$Ke,$L,$Le,$e6,$Ye},$f6);$cf=q#ni:/io/buffer_init.b#;$df=q#ni:/io/buffer_io.b#;$ef=q#ni:/io/cat#;$ff={$w7,1};$gf={};$hf=[];$if=q#shift; +{fs => [@_]}#;$jf=bless({$t,$hf,$v,$if,$x,640,$y,$z},$A);$kf={$m7,$jf};$lf=q#/io/cat_init.b#;$mf=bless({$o5,$gf,$l6,$q,$m6,$q,$n6,$kf,$L,$lf},$w6);$nf={};$of=[];$pf=q#my $fs = shift->{fs};
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
$total_read;#;$qf=bless({$t,$of,$v,$pf,$x,642,$y,$z},$A);$rf={$ne,$qf};$sf=q#/io/cat_read.b#;$tf=bless({$o5,$nf,$l6,$q,$m6,$q,$n6,$rf,$L,$sf},$w6);$uf=[$de,$mf,$tf];$vf=bless({$o5,$ff,$L,$d2,$e6,$uf},$v5);$wf=q#io/cat.c::ctors#;$xf=q#ni:/io/cat.c#;$yf={$v5,1};$zf=q#/io/cat.c#;$Af=[$Xe];$Bf=bless({$o5,$yf,$L,$zf,$e6,$Af},$f6);$Cf=q#ni:/io/cat_init.b#;$Df=q#ni:/io/cat_read.b#;$Ef=q#ni:/io/exec#;$Ff={$x7,1};$Gf={};$Hf=q#argv#;$If=[];$Jf=q#shift->{'argv'}#;$Kf=bless({$t,$If,$v,$Jf,$x,644,$y,$z},$A);$Lf={$Hf,$Kf};$Mf=q#/io/exec_ro.b#;$Nf=bless({$o5,$Gf,$l6,$q,$m6,$q,$n6,$Lf,$L,$Mf},$w6);$Of={};$Pf=[];$Qf=q#my ($class, @argv) = @_;
+{argv         => \\@argv,
  env          => {%ENV},
  internal_fds => {},
  external_fds => {}};#;$Rf=bless({$t,$Pf,$v,$Qf,$x,646,$y,$z},$A);$Sf={$m7,$Rf};$Tf=q#/io/exec_init.b#;$Uf=bless({$o5,$Of,$l6,$q,$m6,$q,$n6,$Sf,$L,$Tf},$w6);$Vf={};$Wf=q#connect#;$Xf=[];$Yf=q#my ($self, %fds) = @_;
@{$$self{internal_fds}}{keys %fds} = values %fds;
$self;#;$Zf=bless({$t,$Xf,$v,$Yf,$x,648,$y,$z},$A);$cg=q#in_pipe#;$dg=[];$eg=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(1);
}
$self;#;$fg=bless({$t,$dg,$v,$eg,$x,650,$y,$z},$A);$gg=q#out_pipe#;$hg=[];$ig=q#local $_;
my $self = shift;
for (@_) {
  my ($r, $w) = POSIX::pipe;
  $self->die($!) unless defined $r;
  $$self{internal_fds}{$_} = ni('ni:/io/fd')->new($w)->cloexec(0);
  $$self{external_fds}{$_} = ni('ni:/io/fd')->new($r)->cloexec(1);
}
$self;#;$jg=bless({$t,$hg,$v,$ig,$x,652,$y,$z},$A);$kg=q#setup_stdio#;$lg=[];$mg=q#my $self = shift;
$self->connect(0 => ni('null:')->fd->cloexec(0)) unless $self->binds_fd(0);
$self->out_pipe(1) unless $self->binds_fd(1);
$self->out_pipe(2) unless $self->binds_fd(2);
$self;#;$ng=bless({$t,$lg,$v,$mg,$x,654,$y,$z},$A);$og={$Wf,$Zf,$cg,$fg,$gg,$jg,$kg,$ng};$pg=q#/io/exec_io_setup.b#;$qg=bless({$o5,$Vf,$l6,$q,$m6,$q,$n6,$og,$L,$pg},$w6);$rg={};$sg=q#binds_fd#;$tg=[];$ug=q#my ($self, $fd) = @_;
$$self{internal_fds}{$fd};#;$vg=bless({$t,$tg,$v,$ug,$x,656,$y,$z},$A);$wg=q#fd#;$xg=[];$yg=q#my ($self, $fd) = @_;
$$self{external_fds}{$fd};#;$zg=bless({$t,$xg,$v,$yg,$x,658,$y,$z},$A);$Ag=q#stderr#;$Bg=[];$Cg=q#my $self = shift;
$self->binds_fd(2) ? $self->fd(2) : $self->out_pipe(2)->fd(2);#;$Dg=bless({$t,$Bg,$v,$Cg,$x,660,$y,$z},$A);$Eg=q#stdin#;$Fg=[];$Gg=q#my $self = shift;
$self->binds_fd(0) ? $self->fd(0) : $self->in_pipe(0)->fd(0);#;$Hg=bless({$t,$Fg,$v,$Gg,$x,662,$y,$z},$A);$Ig=q#stdout#;$Jg=[];$Kg=q#my $self = shift;
$self->binds_fd(1) ? $self->fd(1) : $self->out_pipe(1)->fd(1);#;$Lg=bless({$t,$Jg,$v,$Kg,$x,664,$y,$z},$A);$Mg={$sg,$vg,$wg,$zg,$Ag,$Dg,$Eg,$Hg,$Ig,$Lg};$Ng=q#/io/exec_io_accessors.b#;$Og=bless({$o5,$rg,$l6,$q,$m6,$q,$n6,$Mg,$L,$Ng},$w6);$Pg={};$Qg=q#env#;$Rg=[];$Sg=q#my ($self, %env) = @_;
return $$self{env} unless keys %env;
@{$$self{env}}{keys %env} = values %env;
$self;#;$Tg=bless({$t,$Rg,$v,$Sg,$x,666,$y,$z},$A);$Ug={$Qg,$Tg};$Vg=q#/io/exec_env.b#;$Wg=bless({$o5,$Pg,$l6,$q,$m6,$q,$n6,$Ug,$L,$Vg},$w6);$Xg={};$Yg=q#exec#;$Zg=[];$ch=q#my $self = shift->setup_stdio->move_fds;
my @argv = (@{$$self{argv}}, @_);
$_->close for values %{$$self{external_fds}};
local %ENV = %{$$self{env}};
{ exec @argv };
$self->stderr("exec failed", $!);
1;#;$dh=bless({$t,$Zg,$v,$ch,$x,668,$y,$z},$A);$eh=q#fork#;$fh=[];$gh=q#my $self = shift->setup_stdio;
my $pid  = $self->io_check_defined(*main::fork);
exit $self->exec(@_) unless $pid;
$_->close for values %{$$self{internal_fds}};
delete $$self{internal_fds};
ni('ni:/io/pid')->new(
  $pid,
  [@{$$self{argv}}, @_],
  $$self{env},
  %{$$self{external_fds}});#;$hh=bless({$t,$fh,$v,$gh,$x,670,$y,$z},$A);$ih=q#move_fds#;$jh=[];$kh=q#my $self = shift;
$$self{internal_fds}{$_}->be($_) for keys %{$$self{internal_fds}};
$self;#;$lh=bless({$t,$jh,$v,$kh,$x,672,$y,$z},$A);$mh={$Yg,$dh,$eh,$hh,$ih,$lh};$nh=q#/io/exec_fork.b#;$oh=bless({$o5,$Xg,$l6,$q,$m6,$q,$n6,$mh,$L,$nh},$w6);$ph=[$de,$Nf,$Uf,$qg,$Og,$Wg,$oh];$qh=bless({$o5,$Ff,$L,$q2,$e6,$ph},$w5);$rh=q#io/exec.c::ctors#;$sh=q#ni:/io/exec.c#;$th={$w5,1};$uh=q#/io/exec.c#;$vh=[$Xe];$wh=bless({$o5,$th,$L,$uh,$e6,$vh},$f6);$xh=q#ni:/io/exec_env.b#;$yh=q#ni:/io/exec_fork.b#;$zh=q#ni:/io/exec_init.b#;$Ah=q#ni:/io/exec_io_accessors.b#;$Bh=q#ni:/io/exec_io_setup.b#;$Ch=q#ni:/io/exec_ro.b#;$Dh=q#ni:/io/fd#;$Eh={$y7,1};$Fh={};$Gh=[];$Hh=q#shift->{'fd'}#;$Ih=bless({$t,$Gh,$v,$Hh,$x,674,$y,$z},$A);$Jh={$wg,$Ih};$Kh=q#/io/fd_readers.b#;$Lh=bless({$o5,$Fh,$l6,$q,$m6,$q,$n6,$Jh,$L,$Kh},$w6);$Mh={};$Nh=[];$Oh=q#my ($class, $fd) = @_;
+{fd  => ref $fd ? fileno $fd : $fd,
  rfh => undef,
  wfh => undef};#;$Ph=bless({$t,$Nh,$v,$Oh,$x,676,$y,$z},$A);$Qh={$m7,$Ph};$Rh=q#/io/fd_init.b#;$Sh=bless({$o5,$Mh,$l6,$q,$m6,$q,$n6,$Qh,$L,$Rh},$w6);$Th={};$Uh=q#be#;$Vh=[];$Wh=q#my ($self, $new) = @_;
return $self if $new == $$self{fd};
$self->io_check_defined(*POSIX::dup2, $$self{fd}, $new);
$$self{rfh} = $$self{wfh} = undef;
POSIX::close $$self{fd};
$$self{fd} = $new;
$self;#;$Xh=bless({$t,$Vh,$v,$Wh,$x,678,$y,$z},$A);$Yh={$Uh,$Xh};$Zh=q#/io/fd_shell.b#;$ci=bless({$o5,$Th,$l6,$q,$m6,$q,$n6,$Yh,$L,$Zh},$w6);$di={};$ei=q#cloexec#;$fi=[];$gi=q#shift->fcntl_flag(Fcntl::FD_CLOEXEC, @_)#;$hi=bless({$t,$fi,$v,$gi,$x,680,$y,$z},$A);$ii=q#fcntl_flag#;$ji=[];$ki=q#my ($self, $flag, $value) = @_;
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
}#;$li=bless({$t,$ji,$v,$ki,$x,682,$y,$z},$A);$mi=q#nonblock#;$ni=[];$oi=q#shift->fcntl_flag(Fcntl::O_NONBLOCK, @_)#;$pi=bless({$t,$ni,$v,$oi,$x,684,$y,$z},$A);$qi={$ei,$hi,$ii,$li,$mi,$pi};$ri=q#/io/fd_fcntl.b#;$si=bless({$o5,$di,$l6,$q,$m6,$q,$n6,$qi,$L,$ri},$w6);$ti={};$ui=[];$vi=q#shift->close#;$wi=bless({$t,$ui,$v,$vi,$x,686,$y,$z},$A);$xi=q#close#;$yi=[];$zi=q#my $self = shift;
close $$self{rfh} if $$self{rfh};
close $$self{wfh} if $$self{wfh};
POSIX::close $$self{fd} if defined $$self{fd};
$$self{fd} = $$self{rfh} = $$self{wfh} = undef;
$self;#;$Ai=bless({$t,$yi,$v,$zi,$x,688,$y,$z},$A);$Bi={$xi,$Ai};$Ci=q#/io/fd_gc.b#;$Di=bless({$o5,$ti,$l6,$q,$m6,$wi,$n6,$Bi,$L,$Ci},$w6);$Ei={};$Fi=[];$Gi=q#my $self = shift;
open $$self{rfh}, "<&=$$self{fd}" or return undef unless $$self{rfh};
sysread $$self{rfh}, $_[0], $_[1], $_[2] || 0;#;$Hi=bless({$t,$Fi,$v,$Gi,$x,690,$y,$z},$A);$Ii=[];$Ji=q#my $self = shift;
open $$self{wfh}, ">&=$$self{fd}" or return undef unless $$self{wfh};
syswrite $$self{wfh}, $_[0], $_[1] || length $_[0], $_[2] || 0;#;$Ki=bless({$t,$Ii,$v,$Ji,$x,692,$y,$z},$A);$Li={$ne,$Hi,$ve,$Ki};$Mi=q#/io/fd_perlio.b#;$Ni=bless({$o5,$Ei,$l6,$q,$m6,$q,$n6,$Li,$L,$Mi},$w6);$Oi=[$de,$Lh,$Sh,$ci,$si,$Di,$Ni];$Pi=bless({$o5,$Eh,$L,$D2,$e6,$Oi},$x5);$Qi=q#io/fd.c::ctors#;$Ri=q#ni:/io/fd.c#;$Si={$x5,1};$Ti=q#/io/fd.c#;$Ui=[$Xe];$Vi=bless({$o5,$Si,$L,$Ti,$e6,$Ui},$f6);$Wi=q#ni:/io/fd_fcntl.b#;$Xi=q#ni:/io/fd_gc.b#;$Yi=q#ni:/io/fd_init.b#;$Zi=q#ni:/io/fd_perlio.b#;$cj=q#ni:/io/fd_readers.b#;$dj=q#ni:/io/fd_shell.b#;$ej=q#ni:/io/file#;$fj={$z7,1};$gj={};$hj=[];$ij=q#shift->{'name'}#;$jj=bless({$t,$hj,$v,$ij,$x,694,$y,$z},$A);$kj={$L,$jj};$lj=q#/io/file_readers.b#;$mj=bless({$o5,$gj,$l6,$q,$m6,$q,$n6,$kj,$L,$lj},$w6);$nj={};$oj=q#mode#;$pj=[];$qj=q#@_ == 2 ? $_[0]->{'mode'} = $_[1] : shift->{'mode'}#;$rj=bless({$t,$pj,$v,$qj,$x,696,$y,$z},$A);$sj={$oj,$rj};$tj=q#/io/file_accessors.b#;$uj=bless({$o5,$nj,$l6,$q,$m6,$q,$n6,$sj,$L,$tj},$w6);$vj={};$wj=[];$xj=q#my ($class, $name, $mode) = @_;
+{name => $name,
  mode => $mode || 0644,
  r    => undef,
  w    => undef};#;$yj=bless({$t,$wj,$v,$xj,$x,698,$y,$z},$A);$zj={$m7,$yj};$Aj=q#/io/file_init.b#;$Bj=bless({$o5,$vj,$l6,$q,$m6,$q,$n6,$zj,$L,$Aj},$w6);$Cj={};$Dj=q#(-X#;$Ej=[];$Fj=q#my ($self, $test) = @_;
&{"-$test"}($$self{name});#;$Gj=bless({$t,$Ej,$v,$Fj,$x,700,$y,$z},$A);$Hj=q#mv#;$Ij=[];$Jj=q#my ($self, $dest) = @_;
$dest = $dest->name if ref $dest;
$self->io_check_true(*main::rename, $self->name, $dest);
$$self{name} = $dest;
$self;#;$Kj=bless({$t,$Ij,$v,$Jj,$x,702,$y,$z},$A);$Lj=q#rm#;$Mj=[];$Nj=q#my $self = shift;
$self->io_check_true(*main::unlink, $self->name);
$self;#;$Oj=bless({$t,$Mj,$v,$Nj,$x,704,$y,$z},$A);$Pj={$Dj,$Gj,$Hj,$Kj,$Lj,$Oj};$Qj=q#/io/file_fns.b#;$Rj=bless({$o5,$Cj,$l6,$q,$m6,$q,$n6,$Pj,$L,$Qj},$w6);$Sj={};$Tj=q#atomic_update#;$Uj=[];$Vj=q#my $self = shift;
my $suffix = 0;
++$suffix while -e $self->name . ".$suffix";
ni('ni:/io/file_update_fd')->new(
  $self,
  $self->class->new($self->name . ".$suffix", $self->mode));#;$Wj=bless({$t,$Uj,$v,$Vj,$x,706,$y,$z},$A);$Xj={$Tj,$Wj};$Yj=q#/io/file_update.b#;$Zj=bless({$o5,$Sj,$l6,$q,$m6,$q,$n6,$Xj,$L,$Yj},$w6);$ck={};$dk=[];$ek=q#my $self = shift;
$$self{r}->close if $$self{r};
$$self{w}->close if $$self{w};
$$self{r} = $$self{w} = undef;
$self;#;$fk=bless({$t,$dk,$v,$ek,$x,708,$y,$z},$A);$gk=q#r#;$hk=[];$ik=q#my $self = shift;
$$self{r} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name}, POSIX::O_RDONLY));#;$jk=bless({$t,$hk,$v,$ik,$x,710,$y,$z},$A);$kk=[];$lk=q#shift->r->read(@_)#;$mk=bless({$t,$kk,$v,$lk,$x,712,$y,$z},$A);$nk=q#w#;$ok=[];$pk=q#my $self = shift;
$$self{w} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(
    *POSIX::open, $$self{name},
      POSIX::O_WRONLY | POSIX::O_TRUNC | POSIX::O_CREAT,
      $$self{mode}));#;$qk=bless({$t,$ok,$v,$pk,$x,714,$y,$z},$A);$rk=[];$sk=q#shift->w->write(@_)#;$tk=bless({$t,$rk,$v,$sk,$x,716,$y,$z},$A);$uk={$xi,$fk,$gk,$jk,$ne,$mk,$nk,$qk,$ve,$tk};$vk=q#/io/file_io.b#;$wk=bless({$o5,$ck,$l6,$q,$m6,$q,$n6,$uk,$L,$vk},$w6);$xk=[$de,$mj,$uj,$Bj,$Rj,$Zj,$wk];$yk=bless({$o5,$fj,$L,$Z2,$e6,$xk},$y5);$zk=q#io/file.c::ctors#;$Ak=q#ni:/io/file.c#;$Bk={$y5,1};$Ck=q#/io/file.c#;$Dk=[$Xe];$Ek=bless({$o5,$Bk,$L,$Ck,$e6,$Dk},$f6);$Fk=q#ni:/io/file_accessors.b#;$Gk=q#ni:/io/file_fns.b#;$Hk=q#ni:/io/file_init.b#;$Ik=q#ni:/io/file_io.b#;$Jk=q#ni:/io/file_readers.b#;$Kk=q#ni:/io/file_update.b#;$Lk=q#ni:/io/file_update_fd#;$Mk={$A7,1};$Nk={};$Ok=[];$Pk=q#my ($class, $file, $tempfile) = @_;
my $fd = $tempfile->w;
+{writer   => $fd,
  fd       => $fd->fd,
  wfh      => undef,
  tempfile => $tempfile,
  file     => $file};#;$Qk=bless({$t,$Ok,$v,$Pk,$x,718,$y,$z},$A);$Rk={$m7,$Qk};$Sk=q#/io/file_update_fd_init.b#;$Tk=bless({$o5,$Nk,$l6,$q,$m6,$q,$n6,$Rk,$L,$Sk},$w6);$Uk={};$Vk=[];$Wk=bless({$t,$Vk,$v,$vi,$x,720,$y,$z},$A);$Xk=[];$Yk=q#my $self = shift;
close $$self{wfh} if $$self{wfh};
$$self{writer} = undef if $$self{writer};
if ($$self{tempfile}) {
  $$self{tempfile}->mv($$self{file}->name);
  $$self{tempfile} = undef;
}
$self;#;$Zk=bless({$t,$Xk,$v,$Yk,$x,722,$y,$z},$A);$cl={$xi,$Zk};$dl=q#/io/file_update_fd_gc.b#;$el=bless({$o5,$Uk,$l6,$q,$m6,$Wk,$n6,$cl,$L,$dl},$w6);$fl=[$de,$Lh,$si,$Ni,$Tk,$el];$gl=bless({$o5,$Mk,$L,$h3,$e6,$fl},$z5);$hl=q#io/file_update_fd.c::ctors#;$il=q#ni:/io/file_update_fd.c#;$jl={$z5,1};$kl=q#/io/file_update_fd.c#;$ll=[$Xe];$ml=bless({$o5,$jl,$L,$kl,$e6,$ll},$f6);$nl=q#ni:/io/file_update_fd_gc.b#;$ol=q#ni:/io/file_update_fd_init.b#;$pl=q#ni:/io/named_io_fns.b#;$ql={};$rl=q#fcntl#;$sl=[];$tl=q#CORE::fcntl $_[0], $_[1], $_[2]#;$ul=bless({$t,$sl,$v,$tl,$x,724,$y,$z},$A);$vl=[];$wl=q#CORE::fork#;$xl=bless({$t,$vl,$v,$wl,$x,726,$y,$z},$A);$yl=q#open2#;$zl=[];$Al=q#CORE::open $_[0], $_[1]#;$Bl=bless({$t,$zl,$v,$Al,$x,728,$y,$z},$A);$Cl=q#rename#;$Dl=[];$El=q#CORE::rename $_[0], $_[1]#;$Fl=bless({$t,$Dl,$v,$El,$x,730,$y,$z},$A);$Gl=q#unlink#;$Hl=[];$Il=q#CORE::unlink @_#;$Jl=bless({$t,$Hl,$v,$Il,$x,732,$y,$z},$A);$Kl=q#waitpid#;$Ll=[];$Ml=q#CORE::waitpid $_[0], $_[1]#;$Nl=bless({$t,$Ll,$v,$Ml,$x,734,$y,$z},$A);$Ol={$rl,$ul,$eh,$xl,$yl,$Bl,$Cl,$Fl,$Gl,$Jl,$Kl,$Nl};$Pl=q#/io/named_io_fns.b#;$Ql=bless({$o5,$ql,$l6,$q,$m6,$q,$n6,$Ol,$L,$Pl},$w6);$Rl=q#main#;$Sl=q#ni:/io/null#;$Tl={$B7,1};$Ul=q#/io/null#;$Vl={};$Wl=[];$Xl=q#+{fd => undef}#;$Yl=bless({$t,$Wl,$v,$Xl,$x,736,$y,$z},$A);$Zl={$m7,$Yl};$cm=q#/io/null_init.b#;$dm=bless({$o5,$Vl,$l6,$q,$m6,$q,$n6,$Zl,$L,$cm},$w6);$em={};$fm=[];$gm=q#my $self = shift;
$$self{fd} ||= ni('ni:/io/fd')->new(
  $self->io_check_defined(*POSIX::open, '/dev/null', POSIX::O_RDWR));#;$hm=bless({$t,$fm,$v,$gm,$x,738,$y,$z},$A);$im=[];$jm=q#shift->fd->read(@_)#;$km=bless({$t,$im,$v,$jm,$x,740,$y,$z},$A);$lm=[];$mm=q#shift->fd->write(@_)#;$nm=bless({$t,$lm,$v,$mm,$x,742,$y,$z},$A);$om={$wg,$hm,$ne,$km,$ve,$nm};$pm=q#/io/null_io.b#;$qm=bless({$o5,$em,$l6,$q,$m6,$q,$n6,$om,$L,$pm},$w6);$rm=[$de,$dm,$qm];$sm=bless({$o5,$Tl,$L,$Ul,$e6,$rm},$A5);$tm=q#io/null.c::ctors#;$um=q#ni:/io/null.c#;$vm={$A5,1};$wm=q#/io/null.c#;$xm=[$Xe];$ym=bless({$o5,$vm,$L,$wm,$e6,$xm},$f6);$zm=q#ni:/io/null_init.b#;$Am=q#ni:/io/null_io.b#;$Bm=q#ni:/io/object#;$Cm=q#ni:/io/object.c#;$Dm=q#ni:/io/object.c_transfer_def.b#;$Em=q#ni:/io/object_checks.b#;$Fm=q#ni:/io/object_constructors.b#;$Gm=q#ni:/io/object_memory.b#;$Hm=q#ni:/io/object_ops.b#;$Im=q#ni:/io/object_transfer_async.b#;$Jm=q#ni:/io/object_transfer_sync.b#;$Km=q#ni:/io/pid#;$Lm={$D7,1};$Mm={};$Nm=q#pid#;$Om=[];$Pm=q#shift->{'pid'}#;$Qm=bless({$t,$Om,$v,$Pm,$x,744,$y,$z},$A);$Rm=q#status#;$Sm=[];$Tm=q#shift->{'status'}#;$Um=bless({$t,$Sm,$v,$Tm,$x,746,$y,$z},$A);$Vm={$Nm,$Qm,$Rm,$Um};$Wm=q#/io/pid_readers.b#;$Xm=bless({$o5,$Mm,$l6,$q,$m6,$q,$n6,$Vm,$L,$Wm},$w6);$Ym={};$Zm=[];$cn=q#shift->await#;$dn=bless({$t,$Zm,$v,$cn,$x,748,$y,$z},$A);$en=[];$fn=q#my ($class, $pid, $argv, $env, %external_fds) = @_;
+{pid          => $pid,
  argv         => $argv,
  env          => $env,
  external_fds => \\%external_fds,
  status       => undef};#;$gn=bless({$t,$en,$v,$fn,$x,750,$y,$z},$A);$hn={$m7,$gn};$in=q#/io/pid_init.b#;$jn=bless({$o5,$Ym,$l6,$q,$m6,$dn,$n6,$hn,$L,$in},$w6);$kn={};$ln=q#await#;$mn=[];$nn=q#my $self = shift;
return $$self{status} if defined $$self{status};
$self->io_check_defined(*main::waitpid, $$self{pid}, 0);
$$self{status} = $?;#;$on=bless({$t,$mn,$v,$nn,$x,752,$y,$z},$A);$pn=q#running#;$qn=[];$rn=q#not defined $_[0]->{status} and kill 0, $_[0]->{pid}#;$sn=bless({$t,$qn,$v,$rn,$x,754,$y,$z},$A);$tn={$ln,$on,$pn,$sn};$un=q#/io/pid_wait.b#;$vn=bless({$o5,$kn,$l6,$q,$m6,$q,$n6,$tn,$L,$un},$w6);$wn={};$xn=[];$yn=q#shift->stdout->read(@_)#;$zn=bless({$t,$xn,$v,$yn,$x,756,$y,$z},$A);$An=[];$Bn=q#shift->stdin->write(@_)#;$Cn=bless({$t,$An,$v,$Bn,$x,758,$y,$z},$A);$Dn={$ne,$zn,$ve,$Cn};$En=q#/io/pid_io.b#;$Fn=bless({$o5,$wn,$l6,$q,$m6,$q,$n6,$Dn,$L,$En},$w6);$Gn={};$Hn=[];$In=q#$_[0]->{external_fds}{$_[1]}#;$Jn=bless({$t,$Hn,$v,$In,$x,760,$y,$z},$A);$Kn=[];$Ln=q#shift->fd(2)#;$Mn=bless({$t,$Kn,$v,$Ln,$x,762,$y,$z},$A);$Nn=[];$On=q#shift->fd(0)#;$Pn=bless({$t,$Nn,$v,$On,$x,764,$y,$z},$A);$Qn=[];$Rn=q#shift->fd(1)#;$Sn=bless({$t,$Qn,$v,$Rn,$x,766,$y,$z},$A);$Tn={$wg,$Jn,$Ag,$Mn,$Eg,$Pn,$Ig,$Sn};$Un=q#/io/pid_accessors.b#;$Vn=bless({$o5,$Gn,$l6,$q,$m6,$q,$n6,$Tn,$L,$Un},$w6);$Wn=[$de,$Xm,$jn,$vn,$Fn,$Vn];$Xn=bless({$o5,$Lm,$L,$E3,$e6,$Wn},$C5);$Yn=q#io/pid.c::ctors#;$Zn=q#ni:/io/pid.c#;$co={$C5,1};$do=q#/io/pid.c#;$eo=[$Xe];$fo=bless({$o5,$co,$L,$do,$e6,$eo},$f6);$go=q#ni:/io/pid_accessors.b#;$ho=q#ni:/io/pid_init.b#;$io=q#ni:/io/pid_io.b#;$jo=q#ni:/io/pid_readers.b#;$ko=q#ni:/io/pid_wait.b#;$lo=q#ni:/io/str#;$mo={$E7,1};$no=q#/io/str#;$oo={};$po=q#data#;$qo=[];$ro=q#shift->{'data'}#;$so=bless({$t,$qo,$v,$ro,$x,768,$y,$z},$A);$to=q#end#;$uo=[];$vo=q#shift->{'end'}#;$wo=bless({$t,$uo,$v,$vo,$x,770,$y,$z},$A);$xo=q#start#;$yo=[];$zo=q#shift->{'start'}#;$Ao=bless({$t,$yo,$v,$zo,$x,772,$y,$z},$A);$Bo={$po,$so,$to,$wo,$xo,$Ao};$Co=q#/io/str_ro.b#;$Do=bless({$o5,$oo,$l6,$q,$m6,$q,$n6,$Bo,$L,$Co},$w6);$Eo={};$Fo=[];$Go=q#my $class = shift;
+{data  => \\$_[0],
  start => $_[1] || 0,
  end   => $_[2] || length $_[0]};#;$Ho=bless({$t,$Fo,$v,$Go,$x,774,$y,$z},$A);$Io={$m7,$Ho};$Jo=q#/io/str_init.b#;$Ko=bless({$o5,$Eo,$l6,$q,$m6,$q,$n6,$Io,$L,$Jo},$w6);$Lo={};$Mo=[];$No=q#my $self = shift;
my $l    = ni::min($$self{end} - $$self{start}, $_[1]);
return 0 unless $l;
if ($_[2]) {
  substr $_[0], $_[2], $l, substr ${$$self{data}}, $$self{start}, $l;
} else {
  $_[0] = substr ${$$self{data}}, $$self{start}, $l;
}
$$self{start} += $l;
$l;#;$Oo=bless({$t,$Mo,$v,$No,$x,776,$y,$z},$A);$Po=q#remaining#;$Qo=[];$Ro=q#my $self = shift; $$self{end} - $$self{start}#;$So=bless({$t,$Qo,$v,$Ro,$x,778,$y,$z},$A);$To=[];$Uo=q#my $self = shift;
${$$self{data}} .= $_[0];
$$self{start} += length $_[0];
$$self{end} = length ${$$self{data}};
length $_[0];#;$Vo=bless({$t,$To,$v,$Uo,$x,780,$y,$z},$A);$Wo={$ne,$Oo,$Po,$So,$ve,$Vo};$Xo=q#/io/str_io.b#;$Yo=bless({$o5,$Lo,$l6,$q,$m6,$q,$n6,$Wo,$L,$Xo},$w6);$Zo=[$de,$Do,$Ko,$Yo];$cp=bless({$o5,$mo,$L,$no,$e6,$Zo},$D5);$dp=q#io/str.c::ctors#;$ep=q#ni:/io/str.c#;$fp={$D5,1};$gp=q#/io/str.c#;$hp=[$Xe];$ip=bless({$o5,$fp,$L,$gp,$e6,$hp},$f6);$jp=q#ni:/io/str_init.b#;$kp=q#ni:/io/str_io.b#;$lp=q#ni:/io/str_ro.b#;$mp=q#ni:/io/transfer#;$np={$F7,1,$G7,1,$H7,1};$op=q#/io/transfer#;$pp={$F7,1,$G7,1,$H7,1,$R7,1};$qp=q#/semantic/task#;$rp={};$sp=[];$tp=bless({$t,$sp,$v,$ea,$x,782,$y,$z},$A);$up={$r,$tp};$vp=q#/semantic/task_ro.b#;$wp=bless({$o5,$rp,$l6,$q,$m6,$q,$n6,$up,$L,$vp},$w6);$xp={};$yp=q#failure#;$zp=[];$Ap=q#my $self = shift;
$$self{outcome} = [0, @_];
$self->die($_[0]);#;$Bp=bless({$t,$zp,$v,$Ap,$x,784,$y,$z},$A);$Cp=q#success#;$Dp=[];$Ep=q#my $self = shift;
$$self{outcome} = [1, @_];
$self;#;$Fp=bless({$t,$Dp,$v,$Ep,$x,786,$y,$z},$A);$Gp={$yp,$Bp,$Cp,$Fp};$Hp=q#/semantic/task_outcome.b#;$Ip=bless({$o5,$xp,$l6,$q,$m6,$q,$n6,$Gp,$L,$Hp},$w6);$Jp=[$g8,$wp,$Ip];$Kp=bless({$o5,$pp,$L,$qp,$e6,$Jp},$c6);$Lp=q#semantic/task.c::ctors#;$Mp={};$Np=[];$Op=q#my $self = shift;
@$self{qw/read_bytes read_time write_bytes write_time/} = (0, 0, 0, 0);#;$Pp=bless({$t,$Np,$v,$Op,$x,788,$y,$z},$A);$Qp=[];$Rp=q#my $self = shift;
my $start_time = time;
my $n = $$self{source_io}->read(@_);
my $end_time = time;
$$self{start_time} ||= $start_time;
$$self{read_bytes} += $n if defined $n;
$$self{read_time} += $end_time - $start_time;
$n;#;$Sp=bless({$t,$Qp,$v,$Rp,$x,790,$y,$z},$A);$Tp=[];$Up=q#my $self = shift;
my $start_time = time;
my $n = $$self{dest_io}->write(@_);
my $end_time = time;
$$self{write_bytes} += $n if defined $n;
$$self{write_time} += $end_time - $start_time;
$n;#;$Vp=bless({$t,$Tp,$v,$Up,$x,792,$y,$z},$A);$Wp={$ne,$Sp,$ve,$Vp};$Xp=q#/io/transfer_io_interop.b#;$Yp=bless({$o5,$Mp,$l6,$Pp,$m6,$q,$n6,$Wp,$L,$Xp},$w6);$Zp={};$cq=q#pressure#;$dq=[];$eq=q#my $self = shift;
my $in_impedance  = log($$self{read_time}  || 1);
my $out_impedance = log($$self{write_time} || 1);
($out_impedance - $in_impedance) / log 20;#;$fq=bless({$t,$dq,$v,$eq,$x,794,$y,$z},$A);$gq=q#read_limit_throughput#;$hq=[];$iq=q#my $self = shift;
$$self{read_bytes} / ($$self{read_time} || 1);#;$jq=bless({$t,$hq,$v,$iq,$x,796,$y,$z},$A);$kq=q#throughput#;$lq=[];$mq=q#my $self = shift;
my $end_time = $$self{end_time} || time;
my $dt       = $end_time - $$self{start_time} || 1;
$$self{write_bytes} / $dt;#;$nq=bless({$t,$lq,$v,$mq,$x,798,$y,$z},$A);$oq=q#write_limit_throughput#;$pq=[];$qq=q#my $self = shift;
$$self{write_bytes} / ($$self{write_time} || 1);#;$rq=bless({$t,$pq,$v,$qq,$x,800,$y,$z},$A);$sq={$cq,$fq,$gq,$jq,$kq,$nq,$oq,$rq};$tq=q#/io/transfer_io_measurement.b#;$uq=bless({$o5,$Zp,$l6,$q,$m6,$q,$n6,$sq,$L,$tq},$w6);$vq=[$Kp,$Yp,$uq];$wq=bless({$o5,$np,$L,$op,$e6,$vq},$E5);$xq=q#io/transfer.c::ctors#;$yq=q#ni:/io/transfer.c#;$zq={$E5,1,$F5,1,$G5,1};$Aq=q#/io/transfer.c#;$Bq={$E5,1,$F5,1,$G5,1,$c6,1};$Cq=q#/semantic/task.c#;$Dq=[$O9];$Eq=bless({$o5,$Bq,$L,$Cq,$e6,$Dq},$f6);$Fq={};$Gq=[];$Hq=q#my $self = shift;
ni('ni:/io/object')->def_transfer_method($self, $1)
  if $self->name =~ /transfer_(\\w+)$/;#;$Iq=bless({$t,$Gq,$v,$Hq,$x,804,$y,$z},$A);$Jq={};$Kq=q#/io/transfer.c_into.b#;$Lq=bless({$o5,$Fq,$l6,$Iq,$m6,$q,$n6,$Jq,$L,$Kq},$w6);$Mq=[$Eq,$Lq];$Nq=bless({$o5,$zq,$L,$Aq,$e6,$Mq},$f6);$Oq=q#ni:/io/transfer.c_into.b#;$Pq=q#ni:/io/transfer_async#;$Qq={$G7,1};$Rq=q#/io/transfer_async#;$Sq={};$Tq=q#dest_io#;$Uq=[];$Vq=q#shift->{'dest_io'}#;$Wq=bless({$t,$Uq,$v,$Vq,$x,806,$y,$z},$A);$Xq=q#id#;$Yq=[];$Zq=q#shift->{'id'}#;$cr=bless({$t,$Yq,$v,$Zq,$x,808,$y,$z},$A);$dr=q#source_io#;$er=[];$fr=q#shift->{'source_io'}#;$gr=bless({$t,$er,$v,$fr,$x,810,$y,$z},$A);$hr={$Tq,$Wq,$Xq,$cr,$dr,$gr};$ir=q#/io/transfer_async_ro.b#;$jr=bless({$o5,$Sq,$l6,$q,$m6,$q,$n6,$hr,$L,$ir},$w6);$kr={};$lr=[];$mr=q#my ($class, $source, $dest) = @_;
$source->nonblock(1) if $source->can('nonblock');
$dest  ->nonblock(1) if $dest  ->can('nonblock');
+{source_io => $source,
  dest_io   => $dest,
  pending   => '',
  outcome   => undef,
  id        => $class->new_id};#;$nr=bless({$t,$lr,$v,$mr,$x,812,$y,$z},$A);$or={$m7,$nr};$pr=q#/io/transfer_async_init.b#;$qr=bless({$o5,$kr,$l6,$q,$m6,$q,$n6,$or,$L,$pr},$w6);$rr={};$sr=[];$tr=q#ni('ni:/io/transfer_async')->track(shift)#;$ur=bless({$t,$sr,$v,$tr,$x,814,$y,$z},$A);$vr=[];$wr=q#ni('ni:/io/transfer_async')->untrack(shift->{id})#;$xr=bless({$t,$vr,$v,$wr,$x,816,$y,$z},$A);$yr={};$zr=q#/io/transfer_async_lifecycle.b#;$Ar=bless({$o5,$rr,$l6,$ur,$m6,$xr,$n6,$yr,$L,$zr},$w6);$Br={};$Cr=q#run#;$Dr=[];$Er=q#shift#;$Fr=bless({$t,$Dr,$v,$Er,$x,818,$y,$z},$A);$Gr=q#run_async#;$Hr=[];$Ir=q#my $self = shift;
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

$self;#;$Jr=bless({$t,$Hr,$v,$Ir,$x,820,$y,$z},$A);$Kr={$Cr,$Fr,$Gr,$Jr};$Lr=q#/io/transfer_async_run.b#;$Mr=bless({$o5,$Br,$l6,$q,$m6,$q,$n6,$Kr,$L,$Lr},$w6);$Nr=[$wq,$jr,$qr,$Ar,$Mr];$Or=q#tracked_transfers#;$Pr={};$Qr=q#transfer_id#;$Rr=bless({$o5,$Qq,$L,$Rq,$e6,$Nr,$Or,$Pr,$Qr,0},$F5);$Sr=q#io/transfer_async.c::ctors#;$Tr=q#ni:/io/transfer_async.c#;$Ur={$F5,1};$Vr=q#/io/transfer_async.c#;$Wr={};$Xr=[];$Yr=q#my $self = shift;
$$self{tracked_transfers} = {};
$$self{transfer_id}       = 0;#;$Zr=bless({$t,$Xr,$v,$Yr,$x,830,$y,$z},$A);$cs=q#new_id#;$ds=[];$es=q#++shift->{transfer_id}#;$fs=bless({$t,$ds,$v,$es,$x,832,$y,$z},$A);$gs=q#track#;$hs=[];$is=q#my ($self, $transfer) = @_;
Scalar::Util::weaken($$self{tracked_transfers}{$transfer->id} = $transfer);
$self;#;$js=bless({$t,$hs,$v,$is,$x,834,$y,$z},$A);$ks=q#untrack#;$ls=[];$ms=q#my ($self, $id) = @_;
delete $$self{tracked_transfers}{$id};
$self;#;$ns=bless({$t,$ls,$v,$ms,$x,836,$y,$z},$A);$os={$cs,$fs,$gs,$js,$ks,$ns};$ps=q#/io/transfer_async.c_tracker.b#;$qs=bless({$o5,$Wr,$l6,$Zr,$m6,$q,$n6,$os,$L,$ps},$w6);$rs=[$Nq,$qs];$ss=bless({$o5,$Ur,$L,$Vr,$e6,$rs},$f6);$ts=q#ni:/io/transfer_async.c_tracker.b#;$us=q#ni:/io/transfer_async_init.b#;$vs=q#ni:/io/transfer_async_lifecycle.b#;$ws=q#ni:/io/transfer_async_ro.b#;$xs=q#ni:/io/transfer_async_run.b#;$ys=q#ni:/io/transfer_io_interop.b#;$zs=q#ni:/io/transfer_io_measurement.b#;$As=q#ni:/io/transfer_sync#;$Bs={$H7,1};$Cs=q#/io/transfer_sync#;$Ds={};$Es=[];$Fs=q#my ($class, $source, $dest) = @_;
+{source_io => $source,
  dest_io   => $dest};#;$Gs=bless({$t,$Es,$v,$Fs,$x,838,$y,$z},$A);$Hs={$m7,$Gs};$Is=q#/io/transfer_sync_init.b#;$Js=bless({$o5,$Ds,$l6,$q,$m6,$q,$n6,$Hs,$L,$Is},$w6);$Ks={};$Ls=[];$Ms=q#my $self = shift;
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
$self->success;#;$Ns=bless({$t,$Ls,$v,$Ms,$x,840,$y,$z},$A);$Os={$Cr,$Ns};$Ps=q#/io/transfer_sync_run.b#;$Qs=bless({$o5,$Ks,$l6,$q,$m6,$q,$n6,$Os,$L,$Ps},$w6);$Rs=[$wq,$Js,$Qs];$Ss=bless({$o5,$Bs,$L,$Cs,$e6,$Rs},$G5);$Ts=q#io/transfer_sync.c::ctors#;$Us=q#ni:/io/transfer_sync.c#;$Vs={$G5,1};$Ws=q#/io/transfer_sync.c#;$Xs=[$Nq];$Ys=bless({$o5,$Vs,$L,$Ws,$e6,$Xs},$f6);$Zs=q#ni:/io/transfer_sync_init.b#;$ct=q#ni:/io/transfer_sync_run.b#;$dt=q#ni:/lib/accessor.b#;$et=q#ni:/lib/behavior#;$ft=q#ni:/lib/behavior.c#;$gt=q#ni:/lib/branch#;$ht={$y6,1};$it=q#/lib/branch#;$jt={};$kt=q#local $_;
my $class = shift;
my $name  = shift;
+{name       => $name,
  applied_to => {},
  slices     => [map $class->resolve($_), @_]};#;$lt=bless({$v,$kt,$x,846,$y,$z},$A);$mt={$m7,$lt};$nt=q#/lib/branch_init.b#;$ot=bless({$o5,$jt,$l6,$q,$m6,$q,$n6,$mt,$L,$nt},$w6);$pt=[$r8,$H6,$x6,$ot,$k9];$qt=bless({$o5,$ht,$L,$it,$e6,$pt},$I5);$rt=q#lib/branch.c::ctors#;$st=q#ni:/lib/branch.b#;$tt=q#ni:/lib/branch.c#;$ut={$I5,1};$vt=q#/lib/branch.c#;$wt=[$T9];$xt=bless({$o5,$ut,$L,$vt,$e6,$wt},$f6);$yt=q#ni:/lib/branch_init.b#;$zt=q#ni:/lib/class_init.b#;$At=q#ni:/lib/dataslice#;$Bt={$J7,1};$Ct=q#/lib/dataslice#;$Dt={};$Et=q#my $class = shift;
my $name = shift;
+{name => $name, data => {@_}};#;$Ft=bless({$v,$Et,$x,848,$y,$z},$A);$Gt={$m7,$Ft};$Ht=q#/lib/dataslice_init.b#;$It=bless({$o5,$Dt,$l6,$q,$m6,$q,$n6,$Gt,$L,$Ht},$w6);$Jt={};$Kt=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
return if $$self{applied_to}{$p};
$$self{applied_to}{$p} = 1;
*{"$p\\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
$self;#;$Lt=bless({$v,$Kt,$x,850,$y,$z},$A);$Mt={$r6,$Lt};$Nt=q#/lib/dataslice_apply.b#;$Ot=bless({$o5,$Jt,$l6,$q,$m6,$q,$n6,$Mt,$L,$Nt},$w6);$Pt=[$r8,$It,$Ot];$Qt=bless({$o5,$Bt,$L,$Ct,$e6,$Pt},$J5);$Rt=q#lib/dataslice.c::ctors#;$St=q#ni:/lib/dataslice.c#;$Tt={$J5,1};$Ut=q#/lib/dataslice.c#;$Vt=[$T9];$Wt=bless({$o5,$Tt,$L,$Ut,$e6,$Vt},$f6);$Xt=q#ni:/lib/dataslice_apply.b#;$Yt=q#ni:/lib/dataslice_init.b#;$Zt=q#ni:/lib/definition.b#;$cu=q#ni:/lib/definition_def.b#;$du=q#ni:/lib/definition_defdata.b#;$eu=q#ni:/lib/doc#;$fu={$N,1};$gu={};$hu=q#shift; +{name => shift, doc => []}#;$iu=bless({$v,$hu,$x,852,$y,$z},$A);$ju={$m7,$iu};$ku=q#/lib/doc_init.b#;$lu=bless({$o5,$gu,$l6,$q,$m6,$q,$n6,$ju,$L,$ku},$w6);$mu={};$nu=q#'ni.doc'#;$ou=bless({$v,$nu,$x,854,$y,$z},$A);$pu={$K6,$ou};$qu=q#/lib/doc_namespace.b#;$ru=bless({$o5,$mu,$l6,$q,$m6,$q,$n6,$pu,$L,$qu},$w6);$su={};$tu=q#my $self = shift;
(my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
push @{$$self{doc}}, [$method, @_];
$self;#;$uu=bless({$v,$tu,$x,856,$y,$z},$A);$vu={$jc,$uu};$wu=q#/lib/doc_define.b#;$xu=bless({$o5,$su,$l6,$q,$m6,$q,$n6,$vu,$L,$wu},$w6);$yu={};$zu=q#shift->referent#;$Au=bless({$v,$zu,$x,858,$y,$z},$A);$Bu=q#referent#;$Cu=q#ni 'ni:' . shift->{name}#;$Du=bless({$v,$Cu,$x,860,$y,$z},$A);$Eu={$to,$Au,$Bu,$Du};$Fu=q#/lib/doc_end.b#;$Gu=bless({$o5,$yu,$l6,$q,$m6,$q,$n6,$Eu,$L,$Fu},$w6);$Hu={};$Iu=q#my $self = shift;
push @{$$self{doc}}, [eg => eg($_)] for @_;
$self;#;$Ju=bless({$v,$Iu,$x,862,$y,$z},$A);$Ku=q#linearized#;$Lu=q#map @$_, @{shift->{doc}}#;$Mu=bless({$v,$Lu,$x,864,$y,$z},$A);$Nu=q#tests#;$Ou=q#my $self = shift;
grep ref($_) eq 'lib/test_case', $self->linearized;#;$Pu=bless({$v,$Ou,$x,866,$y,$z},$A);$Qu={$k3,$Ju,$Ku,$Mu,$Nu,$Pu};$Ru=q#/lib/doc_test.b#;$Su=bless({$o5,$Hu,$l6,$q,$m6,$q,$n6,$Qu,$L,$Ru},$w6);$Tu=[$g8,$H6,$lu,$ru,$xu,$Gu,$Su];$Uu=bless({$o5,$fu,$L,$h4,$e6,$Tu},$K5);$Vu=q#lib/doc.c::ctors#;$Wu=q#ni:/lib/doc.c#;$Xu={$K5,1};$Yu=q#/lib/doc.c#;$Zu=[$O9];$cv=bless({$o5,$Xu,$L,$Yu,$e6,$Zu},$f6);$dv=q#ni:/lib/doc_define.b#;$ev=q#ni:/lib/doc_end.b#;$fv=q#ni:/lib/doc_init.b#;$gv=q#ni:/lib/doc_namespace.b#;$hv=q#ni:/lib/doc_test.b#;$iv=q#ni:/lib/documentable.b#;$jv=q#ni:/lib/fn#;$kv={$A,1};$lv=q#/lib/fn#;$mv={};$nv=q#shift->compile#;$ov=bless({$v,$nv,$x,868,$y,$z},$A);$pv=q#my $self = shift;
delete ${'lib/fn::evals'}{$$self{eval_number}}
  if defined $$self{eval_number};#;$qv=bless({$v,$pv,$x,870,$y,$z},$A);$rv=q#compile#;$sv=q#local $@;
my $self = shift;
$$self{proto} ||= '';
my $code = "sub $$self{proto} {$$self{code}\\n}";
my ($en) = ni::eval('__FILE__') =~ /eval (\\d+)/;
$$self{eval_number} = ++$en;
Scalar::Util::weaken(${'lib/fn::evals'}{$en} = $self);
$$self{fn} = ni::eval $code;
die "ni:/lib/fn failed to compile $code: $@" if $@;
$$self{fn};#;$tv=bless({$v,$sv,$x,872,$y,$z},$A);$uv=q#my $class = shift;
my $code  = pop;
my $proto = @_ && $_[-1] =~ /^\\(/ ? pop : '';
+{code        => $code,
  proto       => $proto,
  eval_number => undef,
  annotations => [@_]};#;$vv=bless({$v,$uv,$x,874,$y,$z},$A);$wv={$rv,$tv,$m7,$vv};$xv=q#/lib/fn_init.b#;$yv=bless({$o5,$mv,$l6,$ov,$m6,$qv,$n6,$wv,$L,$xv},$w6);$zv={};$Av=[];$Bv=q#shift->{'annotations'}#;$Cv=bless({$t,$Av,$v,$Bv,$x,876,$y,$z},$A);$Dv=[];$Ev=q#shift->{'code'}#;$Fv=bless({$t,$Dv,$v,$Ev,$x,878,$y,$z},$A);$Gv=[];$Hv=q#shift->{'eval_number'}#;$Iv=bless({$t,$Gv,$v,$Hv,$x,880,$y,$z},$A);$Jv=q#fn#;$Kv=[];$Lv=q#shift->{'fn'}#;$Mv=bless({$t,$Kv,$v,$Lv,$x,882,$y,$z},$A);$Nv={$t,$Cv,$v,$Fv,$x,$Iv,$Jv,$Mv};$Ov=q#/lib/fn_ro.b#;$Pv=bless({$o5,$zv,$l6,$q,$m6,$q,$n6,$Nv,$L,$Ov},$w6);$Qv={};$Rv=[];$Sv=q#my $self = shift; "fn {$$self{code}}"#;$Tv=bless({$t,$Rv,$v,$Sv,$x,884,$y,$z},$A);$Uv=[];$Vv=bless({$t,$Uv,$v,$V8,$x,886,$y,$z},$A);$Wv={$N8,$Tv,$U8,$Vv};$Xv=q#/lib/fn_ops.b#;$Yv=bless({$o5,$Qv,$l6,$q,$m6,$q,$n6,$Wv,$L,$Xv},$w6);$Zv={};$cw=q#serialize#;$dw=[];$ew=q#local $_;
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
$quote->quote_blessed(\\%state, ref $self);#;$fw=bless({$t,$dw,$v,$ew,$x,888,$y,$z},$A);$gw={$cw,$fw};$hw=q#/lib/fn_serialize.b#;$iw=bless({$o5,$Zv,$l6,$q,$m6,$q,$n6,$gw,$L,$hw},$w6);$jw=[$g8,$v9,$yv,$Pv,$Yv,$iw];$kw=bless({$o5,$kv,$L,$lv,$e6,$jw},$L5);$lw=q#lib/fn.c::ctors#;$mw=q#ni:/lib/fn.c#;$nw={$L5,1};$ow=q#/lib/fn.c#;$pw={};$qw=[];$rw=q#my $self = shift;
$SIG{__WARN__} = sub {warn $self->resolve_evals(shift), @_};
$SIG{__DIE__}  = sub {die  $self->resolve_evals(shift), @_};#;$sw=bless({$t,$qw,$v,$rw,$x,892,$y,$z},$A);$tw=q#resolve_evals#;$uw=[];$vw=q#my ($self, $trace) = @_;
1 while $trace =~ s\#\\(eval (\\d+)\\)\#
  ${'lib/fn::evals'}{$1}{code} || "(anonymous eval $1)"\#eg;
$trace;#;$ww=bless({$t,$uw,$v,$vw,$x,894,$y,$z},$A);$xw={$tw,$ww};$yw=q#/lib/fn.c_resolve_eval.b#;$zw=bless({$o5,$pw,$l6,$sw,$m6,$q,$n6,$xw,$L,$yw},$w6);$Aw=[$O9,$zw];$Bw=bless({$o5,$nw,$L,$ow,$e6,$Aw},$f6);$Cw=q#ni:/lib/fn.c_resolve_eval.b#;$Dw=q#ni:/lib/fn_init.b#;$Ew=q#ni:/lib/fn_ops.b#;$Fw=q#ni:/lib/fn_ro.b#;$Gw=q#ni:/lib/fn_serialize.b#;$Hw=q#ni:/lib/gensym_generator_compact.b#;$Iw={};$Jw=q#gensym#;$Kw=[];$Lw=q#my $n = shift->{gensym_n}++;
my $s = '$' .
  substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
         $n % 50, 1;
$n = int $n / 50;
while ($n) {
  $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
               $n % 63, 1;
  $n = int $n / 63;
}
$s;#;$Mw=bless({$t,$Kw,$v,$Lw,$x,896,$y,$z},$A);$Nw={$Jw,$Mw};$Ow=q#/lib/gensym_generator_compact.b#;$Pw=bless({$o5,$Iw,$l6,$q,$m6,$q,$n6,$Nw,$L,$Ow},$w6);$Qw=q#ni:/lib/global_static_test.b#;$Rw={};$Sw=[];$Tw=q#ni('ni:/lib/test_case')->new(shift)#;$Uw=q#($)#;$Vw=bless({$t,$Sw,$v,$Tw,$x,898,$y,$Uw},$A);$Ww=q#now#;$Xw=[];$Yw=q#ni('ni:/lib/test_value')->new(shift)#;$Zw=bless({$t,$Xw,$v,$Yw,$x,900,$y,$Uw},$A);$cx={$k3,$Vw,$Ww,$Zw};$dx=q#/lib/global_static_test.b#;$ex=bless({$o5,$Rw,$l6,$q,$m6,$q,$n6,$cx,$L,$dx},$w6);$fx=q#ni:/lib/image#;$gx={$K7,1};$hx={};$ix=[];$jx=q#+{gensym_n     => 0,
  circular     => [],
  definitions  => {},
  objects      => {},
  side_effects => [],
  finalizers   => [],
  visited      => {},
  ordering     => []};#;$kx=bless({$t,$ix,$v,$jx,$x,902,$y,$z},$A);$lx={$m7,$kx};$mx=q#/lib/image_init.b#;$nx=bless({$o5,$hx,$l6,$q,$m6,$q,$n6,$lx,$L,$mx},$w6);$ox={};$px=q#address#;$qx=[];$rx=q#return 'undef' unless defined $_[1];
return "id:$_[1]" if !ref $_[1] && length $_[1] < 256;
"addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \\$_[1]);#;$sx=bless({$t,$qx,$v,$rx,$x,904,$y,$z},$A);$tx=q#allocate_gensym#;$ux=[];$vx=q#my $self = shift;
my $a = $self->address(shift);
return $$self{visited}{$a} if $_[0] =~ /^\\$\\w+$/;
my $g = $$self{visited}{$a} = $self->gensym;
$$self{definitions}{$g} = "$g=$_[0];";
push @{$$self{ordering}}, $g;
$g;#;$wx=bless({$t,$ux,$v,$vx,$x,906,$y,$z},$A);$xx=q#boot_side_effect#;$yx=[];$zx=q#unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Ax=bless({$t,$yx,$v,$zx,$x,908,$y,$z},$A);$Bx=q#circular_links#;$Cx=[];$Dx=q#local $_;
my $self = shift;
map "\\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
    @{$$self{circular}};#;$Ex=bless({$t,$Cx,$v,$Dx,$x,910,$y,$z},$A);$Fx=q#finalizer#;$Gx=[];$Hx=q#push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]#;$Ix=bless({$t,$Gx,$v,$Hx,$x,912,$y,$z},$A);$Jx=q#io#;$Kx=[];$Lx=q#local $_;
my $self = shift;
ni('ni:/io/str')->new(join '',
  "\#!/usr/bin/env perl\\n",
  "chomp(\\$ni::license=<<'_');\\n", $ni::license, "\\n_\\n",
  "BEGIN{eval(\\$ni::boot=<<'_')}\\n", $ni::boot, "\\n_\\n",
  $self->reconstruction,
  "ni->run(\\@ARGV);",
  "\\n__DATA__\\n");#;$Mx=bless({$t,$Kx,$v,$Lx,$x,914,$y,$z},$A);$Nx=q#quote#;$Ox=[];$Px=q#my $self = shift;
return $self->quote_scalar($_[0])
  if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
my $a = $self->address($_[0]);
$$self{objects}{$a} = \\$_[0];
my $v = $$self{visited}{$a};
return ref $v ? '0' : $v if defined $v;
$$self{visited}{$a} = \\'undef';
$self->allocate_gensym($_[0], $self->quote_value($_[0]));#;$Qx=bless({$t,$Ox,$v,$Px,$x,916,$y,$z},$A);$Rx=q#reconstruction#;$Sx=[];$Tx=q#my $self = shift;
(@{$$self{definitions}}{@{$$self{ordering}}},
 $self->circular_links,
 @{$$self{side_effects}},
 @{$$self{finalizers}});#;$Ux=bless({$t,$Sx,$v,$Tx,$x,918,$y,$z},$A);$Vx=q#side_effect#;$Wx=[];$Xx=q#push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]#;$Yx=bless({$t,$Wx,$v,$Xx,$x,920,$y,$z},$A);$Zx={$px,$sx,$tx,$wx,$xx,$Ax,$Bx,$Ex,$Fx,$Ix,$Jx,$Mx,$Nx,$Qx,$Rx,$Ux,$Vx,$Yx};$cy=q#/lib/image_quoting.b#;$dy=bless({$o5,$ox,$l6,$q,$m6,$q,$n6,$Zx,$L,$cy},$w6);$ey={};$fy=q#quote_code#;$gy=[];$hy=q#shift->die('cannot quote perl CODE refs', shift)#;$iy=bless({$t,$gy,$v,$hy,$x,922,$y,$z},$A);$jy={$fy,$iy};$ky=q#/lib/quote_code_fail.b#;$ly=bless({$o5,$ey,$l6,$q,$m6,$q,$n6,$jy,$L,$ky},$w6);$my={};$ny=q#quote_array#;$oy=[];$py=q#local $_;
my ($self, $v) = @_;
$self->is_circular($$v[$_]) && $self->circular_arrayref($v, $_, $$v[$_])
  for 0..$\#{$v};
'[' . join(',', map $self->quote($_), @$v) . ']';#;$qy=bless({$t,$oy,$v,$py,$x,924,$y,$z},$A);$ry=q#quote_hash#;$sy=[];$ty=q#local $_;
my ($self, $v) = @_;
my @ks = sort keys %$v;
my @qs;
for my $k (@ks) {
  $self->circular_hashref($v, $k, $$v{$k})
    if $self->is_circular($$v{$k});
  push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
}
'{' . join(",", @qs) . '}';#;$uy=bless({$t,$sy,$v,$ty,$x,926,$y,$z},$A);$vy=q#quote_scalar#;$wy=[];$xy=q#my $v = $_[1];
return 'undef' unless defined $v;
return $v if Scalar::Util::looks_like_number $v;
$v =~ s/([\\\\\#])/\\\\$1/g;
"q\#$v\#";#;$yy=bless({$t,$wy,$v,$xy,$x,928,$y,$z},$A);$zy=q#quote_scalar_ref#;$Ay=[];$By=q#'\\\\' . shift->quote(${$_[0]})#;$Cy=bless({$t,$Ay,$v,$By,$x,930,$y,$z},$A);$Dy=q#quote_value#;$Ey=[];$Fy=q#my $self = shift;
return $self->quote_scalar($_[0])     unless ref $_[0];
return $self->quote_scalar_ref($_[0]) if 'SCALAR' eq ref $_[0];
return $self->quote_array($_[0])      if 'ARRAY'  eq ref $_[0];
return $self->quote_hash($_[0])       if 'HASH'   eq ref $_[0];
return $self->quote_code($_[0])       if 'CODE'   eq ref $_[0];
$self->quote_object($_[0]);#;$Gy=bless({$t,$Ey,$v,$Fy,$x,932,$y,$z},$A);$Hy={$ny,$qy,$ry,$uy,$vy,$yy,$zy,$Cy,$Dy,$Gy};$Iy=q#/lib/quote_values.b#;$Jy=bless({$o5,$my,$l6,$q,$m6,$q,$n6,$Hy,$L,$Iy},$w6);$Ky={};$Ly=q#quote_blessed#;$My=[];$Ny=q#my ($self, $x, $r) = @_;
$r ||= ref $x;
$self->quote_class($r);
my $t = Scalar::Util::reftype $x;
my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
"bless($quoted," . $self->quote($r) . ")";#;$Oy=bless({$t,$My,$v,$Ny,$x,934,$y,$z},$A);$Py=q#quote_class#;$Qy=[];$Ry=q#my ($self, $class) = @_;
$self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");#;$Sy=bless({$t,$Qy,$v,$Ry,$x,936,$y,$z},$A);$Ty=q#quote_object#;$Uy=[];$Vy=q#my $self = shift;
my $q = $self->allocate_gensym($_[0],
  $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
$self->finalizer("&\\$_($q)for\\@" . $self->quote(ref($_[0]) . '::ctors') . ';')
  if @{ref($_[0]) . '::ctors'};
$q;#;$Wy=bless({$t,$Uy,$v,$Vy,$x,938,$y,$z},$A);$Xy={$Ly,$Oy,$Py,$Sy,$Ty,$Wy};$Yy=q#/lib/quote_objects.b#;$Zy=bless({$o5,$Ky,$l6,$q,$m6,$q,$n6,$Xy,$L,$Yy},$w6);$cz={};$dz=q#circular_arrayref#;$ez=[];$fz=q#my $self          = shift;
my $address       = $self->address(shift);
my $index         = shift;
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "[$index]", $value_address];
$self;#;$gz=bless({$t,$ez,$v,$fz,$x,940,$y,$z},$A);$hz=q#circular_hashref#;$iz=[];$jz=q#my $self          = shift;
my $address       = $self->address(shift);
my $quoted_key    = $self->quote(shift);
my $value_address = $self->address(shift);
push @{$$self{circular}}, [$address, "{$quoted_key}", $value_address];
$self;#;$kz=bless({$t,$iz,$v,$jz,$x,942,$y,$z},$A);$lz=q#is_circular#;$mz=[];$nz=q#my $self = shift;
ref $$self{visited}{$self->address(shift)};#;$oz=bless({$t,$mz,$v,$nz,$x,944,$y,$z},$A);$pz={$dz,$gz,$hz,$kz,$lz,$oz};$qz=q#/lib/quote_circular_addressed.b#;$rz=bless({$o5,$cz,$l6,$q,$m6,$q,$n6,$pz,$L,$qz},$w6);$sz=[$g8,$nx,$dy,$ly,$Jy,$Zy,$rz,$Pw];$tz=bless({$o5,$gx,$L,$p4,$e6,$sz},$M5);$uz=q#lib/image.c::ctors#;$vz=q#ni:/lib/image.c#;$wz={$M5,1};$xz=q#/lib/image.c#;$yz=[$O9];$zz=bless({$o5,$wz,$L,$xz,$e6,$yz},$f6);$Az=q#ni:/lib/image_init.b#;$Bz=q#ni:/lib/image_quoting.b#;$Cz=q#ni:/lib/instance.b#;$Dz=q#ni:/lib/instantiable.b#;$Ez=q#ni:/lib/json.b#;$Fz={};$Gz=q#json_decode#;$Hz=[];$Iz=q#local $_;
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
wantarray ? @$r : $$r[0];#;$Jz=bless({$t,$Hz,$v,$Iz,$x,946,$y,$Uw},$A);$Kz=q#json_encode#;$Lz=[];$Mz=q#local $_;
my ($v) = @_;
return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                           sort keys %$v) . "}" if 'HASH' eq ref $v;
Scalar::Util::looks_like_number $v
  ? $v
  : defined $v ? ni::json_escape($v) : 'null';#;$Nz=bless({$t,$Lz,$v,$Mz,$x,948,$y,$Uw},$A);$Oz=q#json_escape#;$Pz=[];$Qz=q#(my $x = shift) =~ s/([\\b\\f\\n\\r\\t"\\\\])/"\\\\" . ($ni::json_escapes{$1} || "")/eg;
"\\"$x\\"";#;$Rz=bless({$t,$Pz,$v,$Qz,$x,950,$y,$Uw},$A);$Sz=q#json_unescape#;$Tz=[];$Uz=q#my $x = substr shift, 1, -1;
$x =~ s/\\\\(["\\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
$x;#;$Vz=bless({$t,$Tz,$v,$Uz,$x,952,$y,$Uw},$A);$Wz=q#json_unescape_one#;$Xz=[];$Yz=q#$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1#;$Zz=bless({$t,$Xz,$v,$Yz,$x,954,$y,$Uw},$A);$cA={$Gz,$Jz,$Kz,$Nz,$Oz,$Rz,$Sz,$Vz,$Wz,$Zz};$dA=q#/lib/json.b#;$eA=bless({$o5,$Fz,$l6,$q,$m6,$q,$n6,$cA,$L,$dA},$w6);$fA=q#ni:/lib/name_as_string.b#;$gA=q#ni:/lib/named.b#;$hA=q#ni:/lib/named_in_ni.b#;$iA=q#ni:/lib/namespaced.b#;$jA=q#ni:/lib/ni#;$kA={$L7,1};$lA={};$mA=q#extend#;$nA=q#my $self = shift;
for (@_) {
  my $r = do $_;
  die "ni: failed to parse $_: $@" if $@;
  die "ni: failed to execute $_: $!" unless defined $r;
}
$self;#;$oA=bless({$v,$nA,$x,956,$y,$z},$A);$pA=q#is_mutable#;$qA=q#$0 ne '-' && -w $0#;$rA=bless({$v,$qA,$x,958,$y,$z},$A);$sA=q#modify#;$tA=q#my ($self, $fn) = @_;
die "ni: cannot modify immutable instance $0" unless $self->is_mutable;
my (undef, undef, $mode) = stat $0;
&$fn(ni('ni:/io/file')->new($0, $mode)->atomic_update);#;$uA=bless({$v,$tA,$x,960,$y,$z},$A);$vA={$mA,$oA,$pA,$rA,$sA,$uA};$wA=q#/lib/ni_self.b#;$xA=bless({$o5,$lA,$l6,$q,$m6,$q,$n6,$vA,$L,$wA},$w6);$yA={};$zA=q#--internal/+=#;$AA=q#my $self = shift;
$self->extend($_) for @_;
my $q = $self->quoted(use_newlines => 1);
$self->modify(sub {$q->io->into_sync(shift)});
0;#;$BA=bless({$v,$AA,$x,962,$y,$z},$A);$CA=q#--internal/eval#;$DA=q#my $self = shift;
for (@_) {
  my $r = ni::eval($_);
  print $@ ? "ERROR $@\\n" : "$r\\n";
}
0;#;$EA=bless({$v,$DA,$x,964,$y,$z},$A);$FA=q#--internal/image#;$GA=q#shift->quoted(use_newlines => 1)->io->into_sync(ni"fd:1");
0;#;$HA=bless({$v,$GA,$x,966,$y,$z},$A);$IA=q#--internal/test#;$JA=q#my $self   = shift;
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
!!$failed;#;$KA=bless({$v,$JA,$x,968,$y,$z},$A);$LA=q#my $self = shift;
shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
exit $self->default(@_);#;$MA=bless({$v,$LA,$x,970,$y,$z},$A);$NA={$zA,$BA,$CA,$EA,$FA,$HA,$IA,$KA,$Cr,$MA};$OA=q#/lib/ni_main.b#;$PA=bless({$o5,$yA,$l6,$q,$m6,$q,$n6,$NA,$L,$OA},$w6);$QA={};$RA=q#my $self = shift;
return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
return $$self{resolvers}{$1}->($_[0]) if
  $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
die "ni:self failed to resolve $_[0]";#;$SA=bless({$v,$RA,$x,972,$y,$z},$A);$TA=q#resolver_for#;$UA=q#my $self = shift;
${$$self{resolvers}}{$_[0]} = $_[1];
$self;#;$VA=bless({$v,$UA,$x,974,$y,$z},$A);$WA={$Y6,$SA,$TA,$VA};$XA=q#/lib/ni_resolver.b#;$YA=bless({$o5,$QA,$l6,$q,$m6,$q,$n6,$WA,$L,$XA},$w6);$ZA={};$cB=q#exists#;$dB=q#exists $_[0]->{named}{$_[1]}#;$eB=bless({$v,$dB,$x,976,$y,$z},$A);$fB=q#quoted#;$gB=q#my $self = shift;
my $q = ni('ni:/lib/image')->new(@_);
my $gs = $q->quote($self);
$q->side_effect("\\$ni::self=$gs;");
$q;#;$hB=bless({$v,$gB,$x,978,$y,$z},$A);$iB={$cB,$eB,$fB,$hB};$jB=q#/lib/ni_image.b#;$kB=bless({$o5,$ZA,$l6,$q,$m6,$q,$n6,$iB,$L,$jB},$w6);$lB=[$g8,$xA,$PA,$YA,$kB];$mB=bless({$o5,$kA,$L,$x4,$e6,$lB},$N5);$nB=q#lib/ni.c::ctors#;$oB=q#ni:/lib/ni.c#;$pB={$N5,1};$qB=q#/lib/ni.c#;$rB=[$O9];$sB=bless({$o5,$pB,$L,$qB,$e6,$rB},$f6);$tB=q#ni:/lib/ni_image.b#;$uB=q#ni:/lib/ni_main.b#;$vB=q#ni:/lib/ni_resolver.b#;$wB=q#ni:/lib/ni_self.b#;$xB=q#ni:/lib/ni_static_util.b#;$yB={};$zB=q#abbrev#;$AB=[];$BB=q#length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'#;$CB=bless({$t,$AB,$v,$BB,$x,980,$y,$z},$A);$DB=q#dor#;$EB=[];$FB=q#defined $_[0] ? $_[0] : $_[1]#;$GB=bless({$t,$EB,$v,$FB,$x,982,$y,$z},$A);$HB=q#indent#;$IB=[];$JB=q#my ($s, $indent) = (@_, 2);
join "\\n", map ' ' x $indent . $_, split /\\n/, $s;#;$KB=bless({$t,$IB,$v,$JB,$x,984,$y,$z},$A);$LB=q#max#;$MB=[];$NB=q#local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m#;$OB=bless({$t,$MB,$v,$NB,$x,986,$y,$z},$A);$PB=q#maxstr#;$QB=[];$RB=q#local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m#;$SB=bless({$t,$QB,$v,$RB,$x,988,$y,$z},$A);$TB=q#mean#;$UB=[];$VB=q#sum(@_) / (@_ || 1)#;$WB=bless({$t,$UB,$v,$VB,$x,990,$y,$z},$A);$XB=q#min#;$YB=[];$ZB=q#local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m#;$cC=bless({$t,$YB,$v,$ZB,$x,992,$y,$z},$A);$dC=q#minstr#;$eC=[];$fC=q#local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m#;$gC=bless({$t,$eC,$v,$fC,$x,994,$y,$z},$A);$hC=q#sgr#;$iC=[];$jC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x#;$kC=bless({$t,$iC,$v,$jC,$x,996,$y,$z},$A);$lC=q#sr#;$mC=[];$nC=q#(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x#;$oC=bless({$t,$mC,$v,$nC,$x,998,$y,$z},$A);$pC=q#sum#;$qC=[];$rC=q#local $_; my $x = 0; $x += $_ for @_; $x#;$sC=bless({$t,$qC,$v,$rC,$x,1000,$y,$z},$A);$tC=q#swap#;$uC=[];$vC=q#@_[0, 1] = @_[1, 0]#;$wC=bless({$t,$uC,$v,$vC,$x,1002,$y,$z},$A);$xC={$zB,$CB,$DB,$GB,$HB,$KB,$LB,$OB,$PB,$SB,$TB,$WB,$XB,$cC,$dC,$gC,$hC,$kC,$lC,$oC,$pC,$sC,$tC,$wC};$yC=q#/lib/ni_static_util.b#;$zC=bless({$o5,$yB,$l6,$q,$m6,$q,$n6,$xC,$L,$yC},$w6);$AC=q#ni:/lib/perlbranch.b#;$BC=q#ni:/lib/quote_circular_addressed.b#;$CC=q#ni:/lib/quote_code_fail.b#;$DC=q#ni:/lib/quote_objects.b#;$EC=q#ni:/lib/quote_simple#;$FC={$M7,1};$GC={};$HC=[];$IC=q#+{}#;$JC=bless({$t,$HC,$v,$IC,$x,1004,$y,$z},$A);$KC={$m7,$JC};$LC=q#/lib/quote_simple_init.b#;$MC=bless({$o5,$GC,$l6,$q,$m6,$q,$n6,$KC,$L,$LC},$w6);$NC={};$OC=[];$PC=bless({$t,$OC,$v,0,$x,1006,$y,$z},$A);$QC=[];$RC=q#shift->quote_value(shift)#;$SC=bless({$t,$QC,$v,$RC,$x,1008,$y,$z},$A);$TC={$lz,$PC,$Nx,$SC};$UC=q#/lib/quote_simple_quote.b#;$VC=bless({$o5,$NC,$l6,$q,$m6,$q,$n6,$TC,$L,$UC},$w6);$WC=[$g8,$MC,$VC,$ly,$Jy,$Zy];$XC=bless({$o5,$FC,$L,$I4,$e6,$WC},$O5);$YC=q#lib/quote_simple.c::ctors#;$ZC=q#ni:/lib/quote_simple.c#;$cD={$O5,1};$dD=q#/lib/quote_simple.c#;$eD=[$O9];$fD=bless({$o5,$cD,$L,$dD,$e6,$eD},$f6);$gD=q#ni:/lib/quote_simple_init.b#;$hD=q#ni:/lib/quote_simple_quote.b#;$iD=q#ni:/lib/quote_values.b#;$jD=q#ni:/lib/ref_eq.b#;$kD=q#ni:/lib/resolver.b#;$lD=q#ni:/lib/slice#;$mD={$w6,1};$nD=q#local $_;
my ($self, $p) = @_;
$p = $p->package if ref $p;
die "/lib/slice.b->apply('$p'): perl packages don't start with "
  . "slashes (this fails on older versions of perl)" if $p =~ /^\\//;
return if $$self{applied_to}{$p};
for (keys %{$$self{methods}}) {
  die "$self: overlapping method $p\\::$_" if defined *{"$p\\::$_"}{CODE};
}
$self->apply_unsafe($p);#;$oD=bless({$v,$nD,$x,1010,$y,$z},$A);$pD=q#local $_;
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
$self;#;$qD=bless({$v,$pD,$x,1012,$y,$z},$A);$rD=q#lib/slice::apply#;$sD=q#lib/slice::apply_unsafe#;$tD={};$uD=q#apply_unsafe#;$vD={$r6,$oD,$uD,$qD};$wD=q#/lib/slice.b#;$xD=bless({$o5,$tD,$n6,$vD,$L,$wD},$w6);$yD={};$zD=q#my $class = shift;
my $name  = shift;
my %args  = @_;
+{ctor       => delete($args{ctor}),
  dtor       => delete($args{dtor}),
  applied_to => delete($args{applied_to}),
  name       => $name,
  methods    => \\%args};#;$AD=bless({$v,$zD,$x,1014,$y,$z},$A);$BD={$m7,$AD};$CD=q#/lib/slice_init.b#;$DD=bless({$o5,$yD,$n6,$BD,$L,$CD},$w6);$ED={};$FD=[];$GD=q#local $_;
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
$g;#;$HD=bless({$t,$FD,$v,$GD,$x,1016,$y,$z},$A);$ID={$cw,$HD};$JD=q#/lib/slice_serialize.b#;$KD=bless({$o5,$ED,$l6,$q,$m6,$q,$n6,$ID,$L,$JD},$w6);$LD=[$r8,$H6,$xD,$DD,$KD];$MD=bless({$o5,$mD,$L,$f5,$e6,$LD},$P5);$ND=q#lib/slice.c::ctors#;$OD=q#ni:/lib/slice.b#;$PD=q#ni:/lib/slice.c#;$QD={$P5,1};$RD=q#/lib/slice.c#;$SD=[$T9];$TD=bless({$o5,$QD,$L,$RD,$e6,$SD},$f6);$UD=q#ni:/lib/slice_init.b#;$VD=q#ni:/lib/slice_serialize.b#;$WD=q#ni:/lib/static_fn.b#;$XD={};$YD=[];$ZD=q#ni('ni:/lib/fn')->new(@_)#;$cE=bless({$t,$YD,$v,$ZD,$x,1018,$y,$Uw},$A);$dE=q#fp#;$eE=[];$fE=q#($$)#;$gE=bless({$t,$eE,$v,$ZD,$x,1020,$y,$fE},$A);$hE={$Jv,$cE,$dE,$gE};$iE=q#/lib/static_fn.b#;$jE=bless({$o5,$XD,$l6,$q,$m6,$q,$n6,$hE,$L,$iE},$w6);$kE=q#ni:/lib/subclass.b#;$lE=q#ni:/lib/tag#;$mE={$I6,1};$nE=q#/lib/tag#;$oE={};$pE=q#local $_;
my ($self, $p) = @_;
$_->apply($p) for @{$$self{slices}};
$self;#;$qE=bless({$v,$pE,$x,1022,$y,$z},$A);$rE={$r6,$qE};$sE=q#/lib/tag.b#;$tE=bless({$o5,$oE,$l6,$q,$m6,$q,$n6,$rE,$L,$sE},$w6);$uE={};$vE=q#local $_;
my $class = shift;
my $name  = shift;
+{name   => $name,
  slices => [map $class->resolve($_), @_]};#;$wE=bless({$v,$vE,$x,1024,$y,$z},$A);$xE={$m7,$wE};$yE=q#/lib/tag_init.b#;$zE=bless({$o5,$uE,$l6,$q,$m6,$q,$n6,$xE,$L,$yE},$w6);$AE=[$r8,$H6,$tE,$zE];$BE=bless({$o5,$mE,$L,$nE,$e6,$AE},$Q5);$CE=q#lib/tag.c::ctors#;$DE=q#ni:/lib/tag.b#;$EE=q#ni:/lib/tag.c#;$FE={$Q5,1};$GE=q#/lib/tag.c#;$HE=[$T9];$IE=bless({$o5,$FE,$L,$GE,$e6,$HE},$f6);$JE=q#ni:/lib/tag_init.b#;$KE=q#ni:/lib/test_assert_eq#;$LE={$N7,1};$ME=q#/lib/test_assert_eq#;$NE={$N7,1,$O7,1};$OE=q#/lib/test_assertion#;$PE={};$QE=q#commit#;$RE=[];$SE=q#my $self = shift;
my $test = ni('ni:/lib/test_case')->running_test;
push @{$test->assertions}, $self->result;
$self;#;$TE=bless({$t,$RE,$v,$SE,$x,1026,$y,$z},$A);$UE={$QE,$TE};$VE=q#/lib/test_assertion_commit.b#;$WE=bless({$o5,$PE,$l6,$q,$m6,$q,$n6,$UE,$L,$VE},$w6);$XE=[$g8,$WE];$YE=bless({$o5,$NE,$L,$OE,$e6,$XE},$S5);$ZE=q#lib/test_assertion.c::ctors#;$cF={};$dF=[];$eF=q#my ($class, $diff) = @_;
+{diff => $diff};#;$fF=bless({$t,$dF,$v,$eF,$x,1028,$y,$z},$A);$gF={$m7,$fF};$hF=q#/lib/test_assert_eq_init.b#;$iF=bless({$o5,$cF,$l6,$q,$m6,$q,$n6,$gF,$L,$hF},$w6);$jF={};$kF=[];$lF=q#my $self = shift;
$self->failed ? "FAIL " . ni::json_encode $$self{diff}
              : "PASS";#;$mF=bless({$t,$kF,$v,$lF,$x,1030,$y,$z},$A);$nF=q#failed#;$oF=[];$pF=q#defined shift->{diff}#;$qF=bless({$t,$oF,$v,$pF,$x,1032,$y,$z},$A);$rF=q#result#;$sF=[];$tF=bless({$t,$sF,$v,$Er,$x,1034,$y,$z},$A);$uF={$N8,$mF,$nF,$qF,$rF,$tF};$vF=q#/lib/test_assert_eq_result.b#;$wF=bless({$o5,$jF,$l6,$q,$m6,$q,$n6,$uF,$L,$vF},$w6);$xF=[$YE,$iF,$wF];$yF=bless({$o5,$LE,$L,$ME,$e6,$xF},$R5);$zF=q#lib/test_assert_eq.c::ctors#;$AF=q#ni:/lib/test_assert_eq.c#;$BF={$R5,1};$CF=q#/lib/test_assert_eq.c#;$DF={$R5,1,$S5,1};$EF=q#/lib/test_assertion.c#;$FF=[$O9];$GF=bless({$o5,$DF,$L,$EF,$e6,$FF},$f6);$HF=[$GF];$IF=bless({$o5,$BF,$L,$CF,$e6,$HF},$f6);$JF=q#ni:/lib/test_assert_eq_init.b#;$KF=q#ni:/lib/test_assert_eq_result.b#;$LF=q#ni:/lib/test_assertion#;$MF=q#ni:/lib/test_assertion.c#;$NF=q#ni:/lib/test_assertion_commit.b#;$OF=q#ni:/lib/test_case#;$PF={$D,1};$QF=q#/lib/test_case#;$RF=q#running_test#;$SF={};$TF=[];$UF=q#shift->{'assertions'}#;$VF=bless({$t,$TF,$v,$UF,$x,1036,$y,$z},$A);$WF=[];$XF=q#shift->{'test'}#;$YF=bless({$t,$WF,$v,$XF,$x,1038,$y,$z},$A);$ZF={$n,$VF,$s,$YF};$cG=q#/lib/test_case_ro.b#;$dG=bless({$o5,$SF,$l6,$q,$m6,$q,$n6,$ZF,$L,$cG},$w6);$eG={};$fG=[];$gG=q#@_ == 2 ? $_[0]->{'error'} = $_[1] : shift->{'error'}#;$hG=bless({$t,$fG,$v,$gG,$x,1040,$y,$z},$A);$iG={$p,$hG};$jG=q#/lib/test_case_rw.b#;$kG=bless({$o5,$eG,$l6,$q,$m6,$q,$n6,$iG,$L,$jG},$w6);$lG={};$mG=[];$nG=q#my $class = shift;
my $test  = fn shift;
+{test       => $test,
  assertions => [],
  error      => undef,
  outcome    => undef};#;$oG=bless({$t,$mG,$v,$nG,$x,1042,$y,$z},$A);$pG={$m7,$oG};$qG=q#/lib/test_case_init.b#;$rG=bless({$o5,$lG,$l6,$q,$m6,$q,$n6,$pG,$L,$qG},$w6);$sG={};$tG=[];$uG=q#my $self = shift;
join '', $self->failed  ? 'FAIL ' : 'PASS ',
         $self->error   ? 'E'     : ':',
         map $_->failed ? 'X'     : '.', @{$$self{assertions}};#;$vG=bless({$t,$tG,$v,$uG,$x,1044,$y,$z},$A);$wG=[];$xG=q#!shift->{outcome}->[0]#;$yG=bless({$t,$wG,$v,$xG,$x,1046,$y,$z},$A);$zG={$N8,$vG,$nF,$yG};$AG=q#/lib/test_case_metrics.b#;$BG=bless({$o5,$sG,$l6,$q,$m6,$q,$n6,$zG,$L,$AG},$w6);$CG={};$DG=q#done#;$EG=[];$FG=q#my $self = shift;
my @failed = grep $_->failed, @{$$self{assertions}};
my $any_failed = @failed || defined $$self{error};
$$self{outcome} = [!$any_failed, $$self{error}, @failed];#;$GG=bless({$t,$EG,$v,$FG,$x,1048,$y,$z},$A);$HG=[];$IG=q#local $_;
my $self = shift;
$self->class->with_test($self, \\&{$$self{test}});
$self;#;$JG=bless({$t,$HG,$v,$IG,$x,1050,$y,$z},$A);$KG={$DG,$GG,$Cr,$JG};$LG=q#/lib/test_case_run.b#;$MG=bless({$o5,$CG,$l6,$q,$m6,$q,$n6,$KG,$L,$LG},$w6);$NG=[$g8,$dG,$kG,$rG,$BG,$MG];$OG=bless({$o5,$PF,$L,$QF,$RF,$q,$e6,$NG},$T5);$PG=q#lib/test_case.c::ctors#;$QG=q#ni:/lib/test_case.c#;$RG={$T5,1};$SG=q#/lib/test_case.c#;$TG={};$UG=[];$VG=q#shift->{'running_test'}#;$WG=bless({$t,$UG,$v,$VG,$x,1054,$y,$z},$A);$XG={$RF,$WG};$YG=q#/lib/test_case.c_test_ro.b#;$ZG=bless({$o5,$TG,$l6,$q,$m6,$q,$n6,$XG,$L,$YG},$w6);$cH={};$dH=[];$eH=q#shift->{running_test} = undef#;$fH=bless({$t,$dH,$v,$eH,$x,1056,$y,$z},$A);$gH=q#with_test#;$hH=[];$iH=q#my ($self, $test, $f) = @_;
local $$self{running_test} = $test;
eval {&$f};
$test->error($@) if $@;
$test->done;#;$jH=bless({$t,$hH,$v,$iH,$x,1058,$y,$z},$A);$kH={$gH,$jH};$lH=q#/lib/test_case.c_test.b#;$mH=bless({$o5,$cH,$l6,$fH,$m6,$q,$n6,$kH,$L,$lH},$w6);$nH=[$O9,$ZG,$mH];$oH=bless({$o5,$RG,$L,$SG,$e6,$nH},$f6);$pH=q#ni:/lib/test_case.c_test.b#;$qH=q#ni:/lib/test_case.c_test_ro.b#;$rH=q#ni:/lib/test_case_init.b#;$sH=q#ni:/lib/test_case_metrics.b#;$tH=q#ni:/lib/test_case_ro.b#;$uH=q#ni:/lib/test_case_run.b#;$vH=q#ni:/lib/test_case_rw.b#;$wH=q#ni:/lib/test_value#;$xH={$P7,1};$yH=q#/lib/test_value#;$zH={};$AH=[];$BH=q#\\$_[1]#;$CH=bless({$t,$AH,$v,$BH,$x,1060,$y,$z},$A);$DH={$m7,$CH};$EH=q#/lib/test_value_init.b#;$FH=bless({$o5,$zH,$l6,$q,$m6,$q,$n6,$DH,$L,$EH},$w6);$GH={};$HH=q#(==#;$IH=[];$JH=q#my ($self, $rhs) = @_;
ni('ni:/lib/test_assert_eq')
  ->new($self->diff($rhs))
  ->commit;#;$KH=bless({$t,$IH,$v,$JH,$x,1062,$y,$z},$A);$LH=q#diff#;$MH=[];$NH=q#my ($self, $rhs) = @_;
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
return undef;#;$OH=bless({$t,$MH,$v,$NH,$x,1064,$y,$z},$A);$PH={$HH,$KH,$LH,$OH};$QH=q#/lib/test_value_eq.b#;$RH=bless({$o5,$GH,$l6,$q,$m6,$q,$n6,$PH,$L,$QH},$w6);$SH={};$TH=[];$UH=q#ni::json_encode ${$_[0]}#;$VH=bless({$t,$TH,$v,$UH,$x,1066,$y,$z},$A);$WH={$N8,$VH};$XH=q#/lib/test_value_str.b#;$YH=bless({$o5,$SH,$l6,$q,$m6,$q,$n6,$WH,$L,$XH},$w6);$ZH=[$g8,$FH,$RH,$YH];$cI=bless({$o5,$xH,$L,$yH,$e6,$ZH},$U5);$dI=q#lib/test_value.c::ctors#;$eI=q#ni:/lib/test_value.c#;$fI={$U5,1};$gI=q#/lib/test_value.c#;$hI=[$O9];$iI=bless({$o5,$fI,$L,$gI,$e6,$hI},$f6);$jI=q#ni:/lib/test_value_eq.b#;$kI=q#ni:/lib/test_value_init.b#;$lI=q#ni:/lib/test_value_str.b#;$mI=q#ni:/metaclass#;$nI={$f6,1};$oI=q#/metaclass#;$pI=[$h7,$v9,$r7,$n9];$qI=bless({$o5,$nI,$L,$oI,$e6,$pI},$V5);$rI=q#metaclass.c::ctors#;$sI=q#ni:/metaclass.c#;$tI={$V5,1};$uI=q#/metaclass.c#;$vI=[$E9];$wI=bless({$o5,$tI,$L,$uI,$e6,$vI},$f6);$xI=q#ni:/module#;$yI=q#ni:/module.c#;$zI=q#ni:/object#;$AI=q#ni:/object.c#;$BI=q#ni:/semantic/dimension#;$CI={$Y5,1};$DI=q#/semantic/dimension#;$EI=[$E9];$FI=bless({$o5,$CI,$L,$DI,$e6,$EI},$Z5);$GI=q#semantic/dimension.c::ctors#;$HI=q#ni:/semantic/dimension.c#;$II={$Z5,1};$JI=q#/semantic/dimension.c#;$KI=[$X9];$LI=bless({$o5,$II,$L,$JI,$e6,$KI},$f6);$MI=q#ni:/semantic/task#;$NI=q#ni:/semantic/task.c#;$OI=q#ni:/semantic/task_outcome.b#;$PI=q#ni:/semantic/task_ro.b#;$QI=q#ni:main#;$RI={$Rl,1};$SI=[$jE,$ex,$Ql];$TI=bless({$o5,$RI,$L,$Rl,$e6,$SI},$g6);$UI=q#module::ctors#;$VI=q#ni:ni#;$WI={$Gb,1};$XI={$Gb,1};$YI=q#json_escapes#;$ZI=q##;$cJ=q#b#;$dJ=q#	#;$eJ=q#t#;$fJ=q#
#;$gJ=q#n#;$hJ=q##;$iJ=q#"#;$jJ=q#/#;$kJ=q#\\#;$lJ={$ZI,$cJ,$dJ,$eJ,$fJ,$gJ,$hJ,$gk,$iJ,$iJ,$jJ,$jJ,$kJ,$kJ};$mJ=q#json_unescapes#;$nJ={$iJ,$iJ,$jJ,$jJ,$kJ,$kJ,$cJ,$ZI,$gJ,$fJ,$gk,$hJ,$eJ,$dJ};$oJ={$YI,$lJ,$mJ,$nJ};$pJ=q#/lib/json_data.b#;$qJ=bless({$o5,$XI,$po,$oJ,$L,$pJ},$J7);$rJ=[$qJ,$eA,$zC];$sJ=bless({$o5,$WI,$L,$Gb,$e6,$rJ},$g6);$tJ={$d,$O,$Q,$V,$W,$o1,$p1,$w1,$x1,$C1,$D1,$P1,$Q1,$e2,$f2,$r2,$s2,$E2,$F2,$c3,$d3,$i3,$j3,$F3,$G3,$M3,$N3,$i4,$j4,$q4,$r4,$y4,$z4,$J4,$K4,$g5,$h5,$m5,$n5,$E9,$G9,$X9,$Y9,$lb,$nb,$rb,$sb,$jb,$tb,$xa,$ub,$qa,$vb,$Na,$wb,$Ob,$Qb,$Ub,$Vb,$Eb,$Wb,$rc,$tc,$xc,$yc,$hc,$zc,$pc,$Ac,$Mb,$Bc,$He,$Je,$Ze,$cf,$le,$df,$Fe,$ef,$vf,$xf,$Bf,$Cf,$mf,$Df,$tf,$Ef,$qh,$sh,$wh,$xh,$Wg,$yh,$oh,$zh,$Uf,$Ah,$Og,$Bh,$qg,$Ch,$Nf,$Dh,$Pi,$Ri,$Vi,$Wi,$si,$Xi,$Di,$Yi,$Sh,$Zi,$Ni,$cj,$Lh,$dj,$ci,$ej,$yk,$Ak,$Ek,$Fk,$uj,$Gk,$Rj,$Hk,$Bj,$Ik,$wk,$Jk,$mj,$Kk,$Zj,$Lk,$gl,$il,$ml,$nl,$el,$ol,$Tk,$pl,$Ql,$Sl,$sm,$um,$ym,$zm,$dm,$Am,$qm,$Bm,$de,$Cm,$Xe,$Dm,$Ve,$Em,$hd,$Fm,$pd,$Gm,$Bd,$Hm,$Lc,$Im,$Zd,$Jm,$Nd,$Km,$Xn,$Zn,$fo,$go,$Vn,$ho,$jn,$io,$Fn,$jo,$Xm,$ko,$vn,$lo,$cp,$ep,$ip,$jp,$Ko,$kp,$Yo,$lp,$Do,$mp,$wq,$yq,$Nq,$Oq,$Lq,$Pq,$Rr,$Tr,$ss,$ts,$qs,$us,$qr,$vs,$Ar,$ws,$jr,$xs,$Mr,$ys,$Yp,$zs,$uq,$As,$Ss,$Us,$Ys,$Zs,$Js,$ct,$Qs,$dt,$L8,$et,$r8,$ft,$T9,$gt,$qt,$st,$x6,$tt,$xt,$yt,$ot,$zt,$r7,$At,$Qt,$St,$Wt,$Xt,$Ot,$Yt,$It,$Zt,$k9,$cu,$B8,$du,$i9,$eu,$Uu,$Wu,$cv,$dv,$xu,$ev,$Gu,$fv,$lu,$gv,$ru,$hv,$Su,$iv,$p8,$jv,$kw,$mw,$Bw,$Cw,$zw,$Dw,$yv,$Ew,$Yv,$Fw,$Pv,$Gw,$iw,$Hw,$Pw,$Qw,$ex,$fx,$tz,$vz,$zz,$Az,$nx,$Bz,$dy,$Cz,$e8,$Dz,$v9,$Ez,$eA,$fA,$S8,$gA,$H6,$hA,$P6,$iA,$W6,$jA,$mB,$oB,$sB,$tB,$kB,$uB,$PA,$vB,$YA,$wB,$xA,$xB,$zC,$AC,$h7,$BC,$rz,$CC,$ly,$DC,$Zy,$EC,$XC,$ZC,$fD,$gD,$MC,$hD,$VC,$iD,$Jy,$jD,$Z8,$kD,$f7,$lD,$MD,$OD,$xD,$PD,$TD,$UD,$DD,$VD,$KD,$WD,$jE,$kE,$C9,$lE,$BE,$DE,$tE,$EE,$IE,$JE,$zE,$KE,$yF,$AF,$IF,$JF,$iF,$KF,$wF,$LF,$YE,$MF,$GF,$NF,$WE,$OF,$OG,$QG,$oH,$pH,$mH,$qH,$ZG,$rH,$rG,$sH,$BG,$tH,$dG,$uH,$MG,$vH,$kG,$wH,$cI,$eI,$iI,$jI,$RH,$kI,$FH,$lI,$YH,$mI,$qI,$sI,$wI,$xI,$n9,$yI,$V9,$zI,$g8,$AI,$O9,$BI,$FI,$HI,$LI,$MI,$Kp,$NI,$Eq,$OI,$Ip,$PI,$wp,$QI,$TI,$VI,$sJ};$uJ=q#resolvers#;$vJ=[];$wJ=q#ni('ni:/io/fd')->new(0 + substr shift, 3)#;$xJ=bless({$t,$vJ,$v,$wJ,$x,1068,$y,$z},$A);$yJ=q#file#;$zJ=[];$AJ=q#my $f = shift;
$f =~ s/^file:(?:\\/\\/)?//;
ni('ni:/io/file')->new($f);#;$BJ=bless({$t,$zJ,$v,$AJ,$x,1070,$y,$z},$A);$CJ=q#null#;$DJ=[];$EJ=q#ni('ni:/io/null')->new#;$FJ=bless({$t,$DJ,$v,$EJ,$x,1072,$y,$z},$A);$GJ=q#sh#;$HJ=[];$IJ=q#ni('ni:/io/exec')->new('/bin/sh', '-c', substr shift, 3)->fork#;$JJ=bless({$t,$HJ,$v,$IJ,$x,1074,$y,$z},$A);$KJ=q#str#;$LJ=[];$MJ=q#my $s = shift;
ni('ni:/io/str')->new(substr($s, 4) . "\\n");#;$NJ=bless({$t,$LJ,$v,$MJ,$x,1076,$y,$z},$A);$OJ={$wg,$xJ,$yJ,$BJ,$CJ,$FJ,$GJ,$JJ,$KJ,$NJ};$PJ=bless({$c,$tJ,$uJ,$OJ},$L7);*$sD=\&$qD;*$rD=\&$oD;$x6->apply_unsafe($p5);$x6->apply_unsafe($q5);$x6->apply_unsafe($r5);$x6->apply_unsafe($s5);$x6->apply_unsafe($t5);$x6->apply_unsafe($u5);$x6->apply_unsafe($v5);$x6->apply_unsafe($w5);$x6->apply_unsafe($x5);$x6->apply_unsafe($y5);$x6->apply_unsafe($z5);$x6->apply_unsafe($A5);$x6->apply_unsafe($B5);$x6->apply_unsafe($C5);$x6->apply_unsafe($D5);$x6->apply_unsafe($E5);$x6->apply_unsafe($F5);$x6->apply_unsafe($G5);$x6->apply_unsafe($H5);$x6->apply_unsafe($y6);$x6->apply_unsafe($I5);$x6->apply_unsafe($J5);$x6->apply_unsafe($K5);$x6->apply_unsafe($L5);$x6->apply_unsafe($M5);$x6->apply_unsafe($N5);$x6->apply_unsafe($O5);$x6->apply_unsafe($P5);$x6->apply_unsafe($Q5);$x6->apply_unsafe($R5);$x6->apply_unsafe($S5);$x6->apply_unsafe($T5);$x6->apply_unsafe($U5);$x6->apply_unsafe($f6);$x6->apply_unsafe($V5);$x6->apply_unsafe($g6);$x6->apply_unsafe($W5);$x6->apply_unsafe($X5);$x6->apply_unsafe($Y5);$x6->apply_unsafe($Z5);$x6->apply_unsafe($c6);$H6->apply_unsafe($p5);$H6->apply_unsafe($q5);$H6->apply_unsafe($r5);$H6->apply_unsafe($s5);$H6->apply_unsafe($t5);$H6->apply_unsafe($u5);$H6->apply_unsafe($v5);$H6->apply_unsafe($w5);$H6->apply_unsafe($x5);$H6->apply_unsafe($y5);$H6->apply_unsafe($z5);$H6->apply_unsafe($A5);$H6->apply_unsafe($B5);$H6->apply_unsafe($C5);$H6->apply_unsafe($D5);$H6->apply_unsafe($E5);$H6->apply_unsafe($F5);$H6->apply_unsafe($G5);$H6->apply_unsafe($H5);$H6->apply_unsafe($y6);$H6->apply_unsafe($I5);$H6->apply_unsafe($J5);$H6->apply_unsafe($N);$H6->apply_unsafe($K5);$H6->apply_unsafe($L5);$H6->apply_unsafe($M5);$H6->apply_unsafe($N5);$H6->apply_unsafe($O5);$H6->apply_unsafe($w6);$H6->apply_unsafe($P5);$H6->apply_unsafe($I6);$H6->apply_unsafe($Q5);$H6->apply_unsafe($R5);$H6->apply_unsafe($S5);$H6->apply_unsafe($T5);$H6->apply_unsafe($U5);$H6->apply_unsafe($f6);$H6->apply_unsafe($V5);$H6->apply_unsafe($g6);$H6->apply_unsafe($W5);$H6->apply_unsafe($X5);$H6->apply_unsafe($Y5);$H6->apply_unsafe($Z5);$H6->apply_unsafe($c6);$P6->apply_unsafe($p5);$P6->apply_unsafe($q5);$P6->apply_unsafe($r5);$P6->apply_unsafe($s5);$P6->apply_unsafe($t5);$P6->apply_unsafe($u5);$P6->apply_unsafe($v5);$P6->apply_unsafe($w5);$P6->apply_unsafe($x5);$P6->apply_unsafe($y5);$P6->apply_unsafe($z5);$P6->apply_unsafe($A5);$P6->apply_unsafe($B5);$P6->apply_unsafe($C5);$P6->apply_unsafe($D5);$P6->apply_unsafe($E5);$P6->apply_unsafe($F5);$P6->apply_unsafe($G5);$P6->apply_unsafe($H5);$P6->apply_unsafe($y6);$P6->apply_unsafe($I5);$P6->apply_unsafe($J5);$P6->apply_unsafe($K5);$P6->apply_unsafe($L5);$P6->apply_unsafe($M5);$P6->apply_unsafe($N5);$P6->apply_unsafe($O5);$P6->apply_unsafe($w6);$P6->apply_unsafe($P5);$P6->apply_unsafe($I6);$P6->apply_unsafe($Q5);$P6->apply_unsafe($R5);$P6->apply_unsafe($S5);$P6->apply_unsafe($T5);$P6->apply_unsafe($U5);$P6->apply_unsafe($f6);$P6->apply_unsafe($V5);$P6->apply_unsafe($g6);$P6->apply_unsafe($W5);$P6->apply_unsafe($X5);$P6->apply_unsafe($Y5);$P6->apply_unsafe($Z5);$P6->apply_unsafe($c6);$W6->apply_unsafe($p5);$W6->apply_unsafe($q5);$W6->apply_unsafe($r5);$W6->apply_unsafe($s5);$W6->apply_unsafe($t5);$W6->apply_unsafe($u5);$W6->apply_unsafe($v5);$W6->apply_unsafe($w5);$W6->apply_unsafe($x5);$W6->apply_unsafe($y5);$W6->apply_unsafe($z5);$W6->apply_unsafe($A5);$W6->apply_unsafe($B5);$W6->apply_unsafe($C5);$W6->apply_unsafe($D5);$W6->apply_unsafe($E5);$W6->apply_unsafe($F5);$W6->apply_unsafe($G5);$W6->apply_unsafe($H5);$W6->apply_unsafe($y6);$W6->apply_unsafe($I5);$W6->apply_unsafe($J5);$W6->apply_unsafe($K5);$W6->apply_unsafe($L5);$W6->apply_unsafe($M5);$W6->apply_unsafe($N5);$W6->apply_unsafe($O5);$W6->apply_unsafe($w6);$W6->apply_unsafe($P5);$W6->apply_unsafe($I6);$W6->apply_unsafe($Q5);$W6->apply_unsafe($R5);$W6->apply_unsafe($S5);$W6->apply_unsafe($T5);$W6->apply_unsafe($U5);$W6->apply_unsafe($f6);$W6->apply_unsafe($V5);$W6->apply_unsafe($g6);$W6->apply_unsafe($W5);$W6->apply_unsafe($X5);$W6->apply_unsafe($Y5);$W6->apply_unsafe($Z5);$W6->apply_unsafe($c6);$f7->apply_unsafe($p5);$f7->apply_unsafe($q5);$f7->apply_unsafe($r5);$f7->apply_unsafe($s5);$f7->apply_unsafe($t5);$f7->apply_unsafe($u5);$f7->apply_unsafe($v5);$f7->apply_unsafe($w5);$f7->apply_unsafe($x5);$f7->apply_unsafe($y5);$f7->apply_unsafe($z5);$f7->apply_unsafe($A5);$f7->apply_unsafe($B5);$f7->apply_unsafe($C5);$f7->apply_unsafe($D5);$f7->apply_unsafe($E5);$f7->apply_unsafe($F5);$f7->apply_unsafe($G5);$f7->apply_unsafe($H5);$f7->apply_unsafe($y6);$f7->apply_unsafe($I5);$f7->apply_unsafe($J5);$f7->apply_unsafe($K5);$f7->apply_unsafe($L5);$f7->apply_unsafe($M5);$f7->apply_unsafe($N5);$f7->apply_unsafe($O5);$f7->apply_unsafe($P5);$f7->apply_unsafe($I6);$f7->apply_unsafe($Q5);$f7->apply_unsafe($R5);$f7->apply_unsafe($S5);$f7->apply_unsafe($T5);$f7->apply_unsafe($U5);$f7->apply_unsafe($f6);$f7->apply_unsafe($V5);$f7->apply_unsafe($g6);$f7->apply_unsafe($W5);$f7->apply_unsafe($X5);$f7->apply_unsafe($Y5);$f7->apply_unsafe($Z5);$f7->apply_unsafe($c6);$r7->apply_unsafe($p5);$r7->apply_unsafe($q5);$r7->apply_unsafe($r5);$r7->apply_unsafe($s5);$r7->apply_unsafe($t5);$r7->apply_unsafe($u5);$r7->apply_unsafe($v5);$r7->apply_unsafe($w5);$r7->apply_unsafe($x5);$r7->apply_unsafe($y5);$r7->apply_unsafe($z5);$r7->apply_unsafe($A5);$r7->apply_unsafe($B5);$r7->apply_unsafe($C5);$r7->apply_unsafe($D5);$r7->apply_unsafe($E5);$r7->apply_unsafe($F5);$r7->apply_unsafe($G5);$r7->apply_unsafe($H5);$r7->apply_unsafe($I5);$r7->apply_unsafe($J5);$r7->apply_unsafe($K5);$r7->apply_unsafe($L5);$r7->apply_unsafe($M5);$r7->apply_unsafe($N5);$r7->apply_unsafe($O5);$r7->apply_unsafe($P5);$r7->apply_unsafe($Q5);$r7->apply_unsafe($R5);$r7->apply_unsafe($S5);$r7->apply_unsafe($T5);$r7->apply_unsafe($U5);$r7->apply_unsafe($f6);$r7->apply_unsafe($V5);$r7->apply_unsafe($g6);$r7->apply_unsafe($W5);$r7->apply_unsafe($X5);$r7->apply_unsafe($Y5);$r7->apply_unsafe($Z5);$r7->apply_unsafe($c6);$e8->apply_unsafe($p5);$e8->apply_unsafe($q5);$e8->apply_unsafe($s7);$e8->apply_unsafe($r5);$e8->apply_unsafe($t7);$e8->apply_unsafe($s5);$e8->apply_unsafe($u7);$e8->apply_unsafe($t5);$e8->apply_unsafe($v7);$e8->apply_unsafe($u5);$e8->apply_unsafe($w7);$e8->apply_unsafe($v5);$e8->apply_unsafe($x7);$e8->apply_unsafe($w5);$e8->apply_unsafe($y7);$e8->apply_unsafe($x5);$e8->apply_unsafe($z7);$e8->apply_unsafe($y5);$e8->apply_unsafe($A7);$e8->apply_unsafe($z5);$e8->apply_unsafe($B7);$e8->apply_unsafe($A5);$e8->apply_unsafe($C7);$e8->apply_unsafe($B5);$e8->apply_unsafe($D7);$e8->apply_unsafe($C5);$e8->apply_unsafe($E7);$e8->apply_unsafe($D5);$e8->apply_unsafe($F7);$e8->apply_unsafe($E5);$e8->apply_unsafe($G7);$e8->apply_unsafe($F5);$e8->apply_unsafe($H7);$e8->apply_unsafe($G5);$e8->apply_unsafe($I7);$e8->apply_unsafe($H5);$e8->apply_unsafe($y6);$e8->apply_unsafe($I5);$e8->apply_unsafe($J7);$e8->apply_unsafe($J5);$e8->apply_unsafe($N);$e8->apply_unsafe($K5);$e8->apply_unsafe($A);$e8->apply_unsafe($L5);$e8->apply_unsafe($K7);$e8->apply_unsafe($M5);$e8->apply_unsafe($L7);$e8->apply_unsafe($N5);$e8->apply_unsafe($M7);$e8->apply_unsafe($O5);$e8->apply_unsafe($w6);$e8->apply_unsafe($P5);$e8->apply_unsafe($I6);$e8->apply_unsafe($Q5);$e8->apply_unsafe($N7);$e8->apply_unsafe($R5);$e8->apply_unsafe($O7);$e8->apply_unsafe($S5);$e8->apply_unsafe($D);$e8->apply_unsafe($T5);$e8->apply_unsafe($P7);$e8->apply_unsafe($U5);$e8->apply_unsafe($f6);$e8->apply_unsafe($V5);$e8->apply_unsafe($g6);$e8->apply_unsafe($W5);$e8->apply_unsafe($Q7);$e8->apply_unsafe($X5);$e8->apply_unsafe($Y5);$e8->apply_unsafe($Z5);$e8->apply_unsafe($R7);$e8->apply_unsafe($c6);$p8->apply_unsafe($p5);$p8->apply_unsafe($q5);$p8->apply_unsafe($r5);$p8->apply_unsafe($s5);$p8->apply_unsafe($t5);$p8->apply_unsafe($u5);$p8->apply_unsafe($v5);$p8->apply_unsafe($w5);$p8->apply_unsafe($x5);$p8->apply_unsafe($y5);$p8->apply_unsafe($z5);$p8->apply_unsafe($A5);$p8->apply_unsafe($B5);$p8->apply_unsafe($C5);$p8->apply_unsafe($D5);$p8->apply_unsafe($E5);$p8->apply_unsafe($F5);$p8->apply_unsafe($G5);$p8->apply_unsafe($I7);$p8->apply_unsafe($H5);$p8->apply_unsafe($y6);$p8->apply_unsafe($I5);$p8->apply_unsafe($J7);$p8->apply_unsafe($J5);$p8->apply_unsafe($K5);$p8->apply_unsafe($L5);$p8->apply_unsafe($M5);$p8->apply_unsafe($N5);$p8->apply_unsafe($O5);$p8->apply_unsafe($w6);$p8->apply_unsafe($P5);$p8->apply_unsafe($I6);$p8->apply_unsafe($Q5);$p8->apply_unsafe($R5);$p8->apply_unsafe($S5);$p8->apply_unsafe($T5);$p8->apply_unsafe($U5);$p8->apply_unsafe($f6);$p8->apply_unsafe($V5);$p8->apply_unsafe($g6);$p8->apply_unsafe($W5);$p8->apply_unsafe($X5);$p8->apply_unsafe($Y5);$p8->apply_unsafe($Z5);$p8->apply_unsafe($c6);$B8->apply_unsafe($p5);$B8->apply_unsafe($q5);$B8->apply_unsafe($r5);$B8->apply_unsafe($s5);$B8->apply_unsafe($t5);$B8->apply_unsafe($u5);$B8->apply_unsafe($v5);$B8->apply_unsafe($w5);$B8->apply_unsafe($x5);$B8->apply_unsafe($y5);$B8->apply_unsafe($z5);$B8->apply_unsafe($A5);$B8->apply_unsafe($B5);$B8->apply_unsafe($C5);$B8->apply_unsafe($D5);$B8->apply_unsafe($E5);$B8->apply_unsafe($F5);$B8->apply_unsafe($G5);$B8->apply_unsafe($H5);$B8->apply_unsafe($y6);$B8->apply_unsafe($I5);$B8->apply_unsafe($J5);$B8->apply_unsafe($K5);$B8->apply_unsafe($L5);$B8->apply_unsafe($M5);$B8->apply_unsafe($N5);$B8->apply_unsafe($O5);$B8->apply_unsafe($P5);$B8->apply_unsafe($Q5);$B8->apply_unsafe($R5);$B8->apply_unsafe($S5);$B8->apply_unsafe($T5);$B8->apply_unsafe($U5);$B8->apply_unsafe($f6);$B8->apply_unsafe($V5);$B8->apply_unsafe($g6);$B8->apply_unsafe($W5);$B8->apply_unsafe($X5);$B8->apply_unsafe($Y5);$B8->apply_unsafe($Z5);$B8->apply_unsafe($c6);$L8->apply_unsafe($p5);$L8->apply_unsafe($q5);$L8->apply_unsafe($r5);$L8->apply_unsafe($s5);$L8->apply_unsafe($t5);$L8->apply_unsafe($u5);$L8->apply_unsafe($v5);$L8->apply_unsafe($w5);$L8->apply_unsafe($x5);$L8->apply_unsafe($y5);$L8->apply_unsafe($z5);$L8->apply_unsafe($A5);$L8->apply_unsafe($B5);$L8->apply_unsafe($C5);$L8->apply_unsafe($D5);$L8->apply_unsafe($E5);$L8->apply_unsafe($F5);$L8->apply_unsafe($G5);$L8->apply_unsafe($H5);$L8->apply_unsafe($y6);$L8->apply_unsafe($I5);$L8->apply_unsafe($J5);$L8->apply_unsafe($K5);$L8->apply_unsafe($L5);$L8->apply_unsafe($M5);$L8->apply_unsafe($N5);$L8->apply_unsafe($O5);$L8->apply_unsafe($P5);$L8->apply_unsafe($Q5);$L8->apply_unsafe($R5);$L8->apply_unsafe($S5);$L8->apply_unsafe($T5);$L8->apply_unsafe($U5);$L8->apply_unsafe($f6);$L8->apply_unsafe($V5);$L8->apply_unsafe($g6);$L8->apply_unsafe($W5);$L8->apply_unsafe($X5);$L8->apply_unsafe($Y5);$L8->apply_unsafe($Z5);$L8->apply_unsafe($c6);$S8->apply_unsafe($p5);$S8->apply_unsafe($q5);$S8->apply_unsafe($r5);$S8->apply_unsafe($s5);$S8->apply_unsafe($t5);$S8->apply_unsafe($u5);$S8->apply_unsafe($v5);$S8->apply_unsafe($w5);$S8->apply_unsafe($x5);$S8->apply_unsafe($y5);$S8->apply_unsafe($z5);$S8->apply_unsafe($A5);$S8->apply_unsafe($B5);$S8->apply_unsafe($C5);$S8->apply_unsafe($D5);$S8->apply_unsafe($E5);$S8->apply_unsafe($F5);$S8->apply_unsafe($G5);$S8->apply_unsafe($H5);$S8->apply_unsafe($y6);$S8->apply_unsafe($I5);$S8->apply_unsafe($J5);$S8->apply_unsafe($K5);$S8->apply_unsafe($L5);$S8->apply_unsafe($M5);$S8->apply_unsafe($N5);$S8->apply_unsafe($O5);$S8->apply_unsafe($P5);$S8->apply_unsafe($Q5);$S8->apply_unsafe($R5);$S8->apply_unsafe($S5);$S8->apply_unsafe($T5);$S8->apply_unsafe($U5);$S8->apply_unsafe($f6);$S8->apply_unsafe($V5);$S8->apply_unsafe($g6);$S8->apply_unsafe($W5);$S8->apply_unsafe($X5);$S8->apply_unsafe($Y5);$S8->apply_unsafe($Z5);$S8->apply_unsafe($c6);$Z8->apply_unsafe($p5);$Z8->apply_unsafe($q5);$Z8->apply_unsafe($r5);$Z8->apply_unsafe($s5);$Z8->apply_unsafe($t5);$Z8->apply_unsafe($u5);$Z8->apply_unsafe($v5);$Z8->apply_unsafe($w5);$Z8->apply_unsafe($x5);$Z8->apply_unsafe($y5);$Z8->apply_unsafe($z5);$Z8->apply_unsafe($A5);$Z8->apply_unsafe($B5);$Z8->apply_unsafe($C5);$Z8->apply_unsafe($D5);$Z8->apply_unsafe($E5);$Z8->apply_unsafe($F5);$Z8->apply_unsafe($G5);$Z8->apply_unsafe($H5);$Z8->apply_unsafe($y6);$Z8->apply_unsafe($I5);$Z8->apply_unsafe($J5);$Z8->apply_unsafe($K5);$Z8->apply_unsafe($L5);$Z8->apply_unsafe($M5);$Z8->apply_unsafe($N5);$Z8->apply_unsafe($O5);$Z8->apply_unsafe($P5);$Z8->apply_unsafe($Q5);$Z8->apply_unsafe($R5);$Z8->apply_unsafe($S5);$Z8->apply_unsafe($T5);$Z8->apply_unsafe($U5);$Z8->apply_unsafe($f6);$Z8->apply_unsafe($V5);$Z8->apply_unsafe($g6);$Z8->apply_unsafe($W5);$Z8->apply_unsafe($X5);$Z8->apply_unsafe($Y5);$Z8->apply_unsafe($Z5);$Z8->apply_unsafe($c6);$i9->apply_unsafe($p5);$i9->apply_unsafe($q5);$i9->apply_unsafe($r5);$i9->apply_unsafe($s5);$i9->apply_unsafe($t5);$i9->apply_unsafe($u5);$i9->apply_unsafe($v5);$i9->apply_unsafe($w5);$i9->apply_unsafe($x5);$i9->apply_unsafe($y5);$i9->apply_unsafe($z5);$i9->apply_unsafe($A5);$i9->apply_unsafe($B5);$i9->apply_unsafe($C5);$i9->apply_unsafe($D5);$i9->apply_unsafe($E5);$i9->apply_unsafe($F5);$i9->apply_unsafe($G5);$i9->apply_unsafe($H5);$i9->apply_unsafe($y6);$i9->apply_unsafe($I5);$i9->apply_unsafe($J5);$i9->apply_unsafe($K5);$i9->apply_unsafe($L5);$i9->apply_unsafe($M5);$i9->apply_unsafe($N5);$i9->apply_unsafe($O5);$i9->apply_unsafe($P5);$i9->apply_unsafe($Q5);$i9->apply_unsafe($R5);$i9->apply_unsafe($S5);$i9->apply_unsafe($T5);$i9->apply_unsafe($U5);$i9->apply_unsafe($f6);$i9->apply_unsafe($V5);$i9->apply_unsafe($g6);$i9->apply_unsafe($W5);$i9->apply_unsafe($X5);$i9->apply_unsafe($Y5);$i9->apply_unsafe($Z5);$i9->apply_unsafe($c6);$v9->apply_unsafe($p5);$v9->apply_unsafe($q5);$v9->apply_unsafe($r5);$v9->apply_unsafe($s5);$v9->apply_unsafe($t5);$v9->apply_unsafe($u5);$v9->apply_unsafe($v5);$v9->apply_unsafe($w5);$v9->apply_unsafe($x5);$v9->apply_unsafe($y5);$v9->apply_unsafe($z5);$v9->apply_unsafe($A5);$v9->apply_unsafe($B5);$v9->apply_unsafe($C5);$v9->apply_unsafe($D5);$v9->apply_unsafe($E5);$v9->apply_unsafe($F5);$v9->apply_unsafe($G5);$v9->apply_unsafe($H5);$v9->apply_unsafe($I5);$v9->apply_unsafe($J5);$v9->apply_unsafe($K5);$v9->apply_unsafe($A);$v9->apply_unsafe($L5);$v9->apply_unsafe($M5);$v9->apply_unsafe($N5);$v9->apply_unsafe($O5);$v9->apply_unsafe($w6);$v9->apply_unsafe($P5);$v9->apply_unsafe($I6);$v9->apply_unsafe($Q5);$v9->apply_unsafe($R5);$v9->apply_unsafe($S5);$v9->apply_unsafe($T5);$v9->apply_unsafe($U5);$v9->apply_unsafe($f6);$v9->apply_unsafe($V5);$v9->apply_unsafe($W5);$v9->apply_unsafe($X5);$v9->apply_unsafe($Y5);$v9->apply_unsafe($Z5);$v9->apply_unsafe($c6);$C9->apply_unsafe($p5);$C9->apply_unsafe($q5);$C9->apply_unsafe($r5);$C9->apply_unsafe($s5);$C9->apply_unsafe($t5);$C9->apply_unsafe($u5);$C9->apply_unsafe($v5);$C9->apply_unsafe($w5);$C9->apply_unsafe($x5);$C9->apply_unsafe($y5);$C9->apply_unsafe($z5);$C9->apply_unsafe($A5);$C9->apply_unsafe($B5);$C9->apply_unsafe($C5);$C9->apply_unsafe($D5);$C9->apply_unsafe($E5);$C9->apply_unsafe($F5);$C9->apply_unsafe($G5);$C9->apply_unsafe($H5);$C9->apply_unsafe($I5);$C9->apply_unsafe($J5);$C9->apply_unsafe($K5);$C9->apply_unsafe($L5);$C9->apply_unsafe($M5);$C9->apply_unsafe($N5);$C9->apply_unsafe($O5);$C9->apply_unsafe($P5);$C9->apply_unsafe($Q5);$C9->apply_unsafe($R5);$C9->apply_unsafe($S5);$C9->apply_unsafe($T5);$C9->apply_unsafe($U5);$C9->apply_unsafe($V5);$C9->apply_unsafe($W5);$C9->apply_unsafe($X5);$C9->apply_unsafe($Y5);$C9->apply_unsafe($Z5);$C9->apply_unsafe($c6);$qa->apply_unsafe($s7);$xa->apply_unsafe($s7);$Na->apply_unsafe($s7);$jb->apply_unsafe($s7);$Eb->apply_unsafe($t7);$Mb->apply_unsafe($t7);$hc->apply_unsafe($u7);$pc->apply_unsafe($u7);$Lc->apply_unsafe($v7);$Lc->apply_unsafe($w7);$Lc->apply_unsafe($x7);$Lc->apply_unsafe($y7);$Lc->apply_unsafe($z7);$Lc->apply_unsafe($A7);$Lc->apply_unsafe($B7);$Lc->apply_unsafe($C7);$Lc->apply_unsafe($D7);$Lc->apply_unsafe($E7);$hd->apply_unsafe($v7);$hd->apply_unsafe($w7);$hd->apply_unsafe($x7);$hd->apply_unsafe($y7);$hd->apply_unsafe($z7);$hd->apply_unsafe($A7);$hd->apply_unsafe($B7);$hd->apply_unsafe($C7);$hd->apply_unsafe($D7);$hd->apply_unsafe($E7);$pd->apply_unsafe($v7);$pd->apply_unsafe($w7);$pd->apply_unsafe($x7);$pd->apply_unsafe($y7);$pd->apply_unsafe($z7);$pd->apply_unsafe($A7);$pd->apply_unsafe($B7);$pd->apply_unsafe($C7);$pd->apply_unsafe($D7);$pd->apply_unsafe($E7);$Bd->apply_unsafe($v7);$Bd->apply_unsafe($w7);$Bd->apply_unsafe($x7);$Bd->apply_unsafe($y7);$Bd->apply_unsafe($z7);$Bd->apply_unsafe($A7);$Bd->apply_unsafe($B7);$Bd->apply_unsafe($C7);$Bd->apply_unsafe($D7);$Bd->apply_unsafe($E7);$Nd->apply_unsafe($v7);$Nd->apply_unsafe($w7);$Nd->apply_unsafe($x7);$Nd->apply_unsafe($y7);$Nd->apply_unsafe($z7);$Nd->apply_unsafe($A7);$Nd->apply_unsafe($B7);$Nd->apply_unsafe($C7);$Nd->apply_unsafe($D7);$Nd->apply_unsafe($E7);$Zd->apply_unsafe($v7);$Zd->apply_unsafe($w7);$Zd->apply_unsafe($x7);$Zd->apply_unsafe($y7);$Zd->apply_unsafe($z7);$Zd->apply_unsafe($A7);$Zd->apply_unsafe($B7);$Zd->apply_unsafe($C7);$Zd->apply_unsafe($D7);$Zd->apply_unsafe($E7);$le->apply_unsafe($v7);$Fe->apply_unsafe($v7);$Ve->apply_unsafe($u5);$Ve->apply_unsafe($v5);$Ve->apply_unsafe($w5);$Ve->apply_unsafe($x5);$Ve->apply_unsafe($y5);$Ve->apply_unsafe($z5);$Ve->apply_unsafe($A5);$Ve->apply_unsafe($B5);$Ve->apply_unsafe($C5);$Ve->apply_unsafe($D5);$mf->apply_unsafe($w7);$tf->apply_unsafe($w7);$Nf->apply_unsafe($x7);$Uf->apply_unsafe($x7);$qg->apply_unsafe($x7);$Og->apply_unsafe($x7);$Wg->apply_unsafe($x7);$oh->apply_unsafe($x7);$Lh->apply_unsafe($y7);$Lh->apply_unsafe($A7);$Sh->apply_unsafe($y7);$ci->apply_unsafe($y7);$si->apply_unsafe($y7);$si->apply_unsafe($A7);$Di->apply_unsafe($y7);$Ni->apply_unsafe($y7);$Ni->apply_unsafe($A7);$mj->apply_unsafe($z7);$uj->apply_unsafe($z7);$Bj->apply_unsafe($z7);$Rj->apply_unsafe($z7);$Zj->apply_unsafe($z7);$wk->apply_unsafe($z7);$Tk->apply_unsafe($A7);$el->apply_unsafe($A7);$Ql->apply_unsafe($Rl);$dm->apply_unsafe($B7);$qm->apply_unsafe($B7);$Xm->apply_unsafe($D7);$jn->apply_unsafe($D7);$vn->apply_unsafe($D7);$Fn->apply_unsafe($D7);$Vn->apply_unsafe($D7);$Do->apply_unsafe($E7);$Ko->apply_unsafe($E7);$Yo->apply_unsafe($E7);$wp->apply_unsafe($F7);$wp->apply_unsafe($G7);$wp->apply_unsafe($H7);$wp->apply_unsafe($R7);$Ip->apply_unsafe($F7);$Ip->apply_unsafe($G7);$Ip->apply_unsafe($H7);$Ip->apply_unsafe($R7);$Yp->apply_unsafe($F7);$Yp->apply_unsafe($G7);$Yp->apply_unsafe($H7);$uq->apply_unsafe($F7);$uq->apply_unsafe($G7);$uq->apply_unsafe($H7);$Lq->apply_unsafe($E5);$Lq->apply_unsafe($F5);$Lq->apply_unsafe($G5);$jr->apply_unsafe($G7);$qr->apply_unsafe($G7);$Ar->apply_unsafe($G7);$Mr->apply_unsafe($G7);$qs->apply_unsafe($F5);$Js->apply_unsafe($H7);$Qs->apply_unsafe($H7);$ot->apply_unsafe($y6);$It->apply_unsafe($J7);$Ot->apply_unsafe($J7);$lu->apply_unsafe($N);$ru->apply_unsafe($N);$xu->apply_unsafe($N);$Gu->apply_unsafe($N);$Su->apply_unsafe($N);$yv->apply_unsafe($A);$Pv->apply_unsafe($A);$Yv->apply_unsafe($A);$iw->apply_unsafe($A);$zw->apply_unsafe($L5);$Pw->apply_unsafe($K7);$ex->apply_unsafe($Rl);$nx->apply_unsafe($K7);$dy->apply_unsafe($K7);$ly->apply_unsafe($K7);$ly->apply_unsafe($M7);$Jy->apply_unsafe($K7);$Jy->apply_unsafe($M7);$Zy->apply_unsafe($K7);$Zy->apply_unsafe($M7);$rz->apply_unsafe($K7);$eA->apply_unsafe($Gb);$xA->apply_unsafe($L7);$PA->apply_unsafe($L7);$YA->apply_unsafe($L7);$kB->apply_unsafe($L7);$zC->apply_unsafe($Gb);$MC->apply_unsafe($M7);$VC->apply_unsafe($M7);$xD->apply_unsafe($w6);$DD->apply_unsafe($w6);$KD->apply_unsafe($w6);$jE->apply_unsafe($Rl);$tE->apply_unsafe($I6);$zE->apply_unsafe($I6);$WE->apply_unsafe($N7);$WE->apply_unsafe($O7);$iF->apply_unsafe($N7);$wF->apply_unsafe($N7);$dG->apply_unsafe($D);$kG->apply_unsafe($D);$rG->apply_unsafe($D);$BG->apply_unsafe($D);$MG->apply_unsafe($D);$ZG->apply_unsafe($T5);$mH->apply_unsafe($T5);$FH->apply_unsafe($P7);$RH->apply_unsafe($P7);$YH->apply_unsafe($P7);$ni::self=$PJ;&$_($B)for@$C;&$_($O)for@$P;&$_($V)for@$P;&$_($d1)for@$C;&$_($j1)for@$C;&$_($o1)for@$P;&$_($w1)for@$P;&$_($C1)for@$P;&$_($K1)for@$C;&$_($P1)for@$P;&$_($X1)for@$C;&$_($e2)for@$P;&$_($m2)for@$C;&$_($r2)for@$P;&$_($z2)for@$C;&$_($E2)for@$P;&$_($P2)for@$C;&$_($V2)for@$C;&$_($c3)for@$P;&$_($i3)for@$P;&$_($o3)for@$C;&$_($u3)for@$C;&$_($A3)for@$C;&$_($F3)for@$P;&$_($M3)for@$P;&$_($V3)for@$C;&$_($d4)for@$C;&$_($i4)for@$P;&$_($q4)for@$P;&$_($y4)for@$P;&$_($E4)for@$C;&$_($J4)for@$P;&$_($S4)for@$C;&$_($Z4)for@$C;&$_($g5)for@$P;&$_($m5)for@$P;&$_($q6)for@$C;&$_($t6)for@$C;&$_($x6)for@$z6;&$_($C6)for@$C;&$_($E6)for@$C;&$_($H6)for@$z6;&$_($M6)for@$C;&$_($P6)for@$z6;&$_($T6)for@$C;&$_($W6)for@$z6;&$_($c7)for@$C;&$_($f7)for@$z6;&$_($h7)for@$i7;&$_($l7)for@$C;&$_($o7)for@$C;&$_($r7)for@$z6;&$_($X7)for@$C;&$_($Z7)for@$C;&$_($e8)for@$z6;&$_($g8)for@$h8;&$_($m8)for@$C;&$_($p8)for@$z6;&$_($r8)for@$s8;&$_($y8)for@$C;&$_($B8)for@$z6;&$_($F8)for@$C;&$_($I8)for@$C;&$_($L8)for@$z6;&$_($P8)for@$C;&$_($S8)for@$z6;&$_($W8)for@$C;&$_($Z8)for@$z6;&$_($f9)for@$C;&$_($i9)for@$z6;&$_($k9)for@$l9;&$_($n9)for@$o9;&$_($s9)for@$C;&$_($v9)for@$z6;&$_($z9)for@$C;&$_($C9)for@$z6;&$_($E9)for@$F9;&$_($O9)for@$P9;&$_($T9)for@$P9;&$_($V9)for@$P9;&$_($X9)for@$P9;&$_($fa)for@$C;&$_($ja)for@$C;&$_($na)for@$C;&$_($qa)for@$z6;&$_($ua)for@$C;&$_($xa)for@$z6;&$_($Ca)for@$C;&$_($Ga)for@$C;&$_($Ka)for@$C;&$_($Na)for@$z6;&$_($Sa)for@$C;&$_($Wa)for@$C;&$_($cb)for@$C;&$_($gb)for@$C;&$_($jb)for@$z6;&$_($lb)for@$mb;&$_($rb)for@$P9;&$_($Bb)for@$C;&$_($Eb)for@$z6;&$_($Jb)for@$C;&$_($Mb)for@$z6;&$_($Ob)for@$Pb;&$_($Ub)for@$P9;&$_($ec)for@$C;&$_($hc)for@$z6;&$_($mc)for@$C;&$_($pc)for@$z6;&$_($rc)for@$sc;&$_($xc)for@$P9;&$_($Ic)for@$C;&$_($Lc)for@$z6;&$_($Qc)for@$C;&$_($Uc)for@$C;&$_($Yc)for@$C;&$_($ed)for@$C;&$_($hd)for@$z6;&$_($md)for@$C;&$_($pd)for@$z6;&$_($ud)for@$C;&$_($yd)for@$C;&$_($Bd)for@$z6;&$_($Gd)for@$C;&$_($Kd)for@$C;&$_($Nd)for@$z6;&$_($Sd)for@$C;&$_($Wd)for@$C;&$_($Zd)for@$z6;&$_($de)for@$ee;&$_($ie)for@$C;&$_($le)for@$z6;&$_($qe)for@$C;&$_($ue)for@$C;&$_($ye)for@$C;&$_($Ce)for@$C;&$_($Fe)for@$z6;&$_($He)for@$Ie;&$_($Se)for@$C;&$_($Ve)for@$z6;&$_($Xe)for@$P9;&$_($Ze)for@$P9;&$_($jf)for@$C;&$_($mf)for@$z6;&$_($qf)for@$C;&$_($tf)for@$z6;&$_($vf)for@$wf;&$_($Bf)for@$P9;&$_($Kf)for@$C;&$_($Nf)for@$z6;&$_($Rf)for@$C;&$_($Uf)for@$z6;&$_($Zf)for@$C;&$_($fg)for@$C;&$_($jg)for@$C;&$_($ng)for@$C;&$_($qg)for@$z6;&$_($vg)for@$C;&$_($zg)for@$C;&$_($Dg)for@$C;&$_($Hg)for@$C;&$_($Lg)for@$C;&$_($Og)for@$z6;&$_($Tg)for@$C;&$_($Wg)for@$z6;&$_($dh)for@$C;&$_($hh)for@$C;&$_($lh)for@$C;&$_($oh)for@$z6;&$_($qh)for@$rh;&$_($wh)for@$P9;&$_($Ih)for@$C;&$_($Lh)for@$z6;&$_($Ph)for@$C;&$_($Sh)for@$z6;&$_($Xh)for@$C;&$_($ci)for@$z6;&$_($hi)for@$C;&$_($li)for@$C;&$_($pi)for@$C;&$_($si)for@$z6;&$_($wi)for@$C;&$_($Ai)for@$C;&$_($Di)for@$z6;&$_($Hi)for@$C;&$_($Ki)for@$C;&$_($Ni)for@$z6;&$_($Pi)for@$Qi;&$_($Vi)for@$P9;&$_($jj)for@$C;&$_($mj)for@$z6;&$_($rj)for@$C;&$_($uj)for@$z6;&$_($yj)for@$C;&$_($Bj)for@$z6;&$_($Gj)for@$C;&$_($Kj)for@$C;&$_($Oj)for@$C;&$_($Rj)for@$z6;&$_($Wj)for@$C;&$_($Zj)for@$z6;&$_($fk)for@$C;&$_($jk)for@$C;&$_($mk)for@$C;&$_($qk)for@$C;&$_($tk)for@$C;&$_($wk)for@$z6;&$_($yk)for@$zk;&$_($Ek)for@$P9;&$_($Qk)for@$C;&$_($Tk)for@$z6;&$_($Wk)for@$C;&$_($Zk)for@$C;&$_($el)for@$z6;&$_($gl)for@$hl;&$_($ml)for@$P9;&$_($ul)for@$C;&$_($xl)for@$C;&$_($Bl)for@$C;&$_($Fl)for@$C;&$_($Jl)for@$C;&$_($Nl)for@$C;&$_($Ql)for@$z6;&$_($Yl)for@$C;&$_($dm)for@$z6;&$_($hm)for@$C;&$_($km)for@$C;&$_($nm)for@$C;&$_($qm)for@$z6;&$_($sm)for@$tm;&$_($ym)for@$P9;&$_($Qm)for@$C;&$_($Um)for@$C;&$_($Xm)for@$z6;&$_($dn)for@$C;&$_($gn)for@$C;&$_($jn)for@$z6;&$_($on)for@$C;&$_($sn)for@$C;&$_($vn)for@$z6;&$_($zn)for@$C;&$_($Cn)for@$C;&$_($Fn)for@$z6;&$_($Jn)for@$C;&$_($Mn)for@$C;&$_($Pn)for@$C;&$_($Sn)for@$C;&$_($Vn)for@$z6;&$_($Xn)for@$Yn;&$_($fo)for@$P9;&$_($so)for@$C;&$_($wo)for@$C;&$_($Ao)for@$C;&$_($Do)for@$z6;&$_($Ho)for@$C;&$_($Ko)for@$z6;&$_($Oo)for@$C;&$_($So)for@$C;&$_($Vo)for@$C;&$_($Yo)for@$z6;&$_($cp)for@$dp;&$_($ip)for@$P9;&$_($tp)for@$C;&$_($wp)for@$z6;&$_($Bp)for@$C;&$_($Fp)for@$C;&$_($Ip)for@$z6;&$_($Kp)for@$Lp;&$_($Pp)for@$C;&$_($Sp)for@$C;&$_($Vp)for@$C;&$_($Yp)for@$z6;&$_($fq)for@$C;&$_($jq)for@$C;&$_($nq)for@$C;&$_($rq)for@$C;&$_($uq)for@$z6;&$_($wq)for@$xq;&$_($Eq)for@$P9;&$_($Iq)for@$C;&$_($Lq)for@$z6;&$_($Nq)for@$P9;&$_($Wq)for@$C;&$_($cr)for@$C;&$_($gr)for@$C;&$_($jr)for@$z6;&$_($nr)for@$C;&$_($qr)for@$z6;&$_($ur)for@$C;&$_($xr)for@$C;&$_($Ar)for@$z6;&$_($Fr)for@$C;&$_($Jr)for@$C;&$_($Mr)for@$z6;&$_($Rr)for@$Sr;&$_($Zr)for@$C;&$_($fs)for@$C;&$_($js)for@$C;&$_($ns)for@$C;&$_($qs)for@$z6;&$_($ss)for@$P9;&$_($Gs)for@$C;&$_($Js)for@$z6;&$_($Ns)for@$C;&$_($Qs)for@$z6;&$_($Ss)for@$Ts;&$_($Ys)for@$P9;&$_($lt)for@$C;&$_($ot)for@$z6;&$_($qt)for@$rt;&$_($xt)for@$P9;&$_($Ft)for@$C;&$_($It)for@$z6;&$_($Lt)for@$C;&$_($Ot)for@$z6;&$_($Qt)for@$Rt;&$_($Wt)for@$P9;&$_($iu)for@$C;&$_($lu)for@$z6;&$_($ou)for@$C;&$_($ru)for@$z6;&$_($uu)for@$C;&$_($xu)for@$z6;&$_($Au)for@$C;&$_($Du)for@$C;&$_($Gu)for@$z6;&$_($Ju)for@$C;&$_($Mu)for@$C;&$_($Pu)for@$C;&$_($Su)for@$z6;&$_($Uu)for@$Vu;&$_($cv)for@$P9;&$_($ov)for@$C;&$_($qv)for@$C;&$_($tv)for@$C;&$_($vv)for@$C;&$_($yv)for@$z6;&$_($Cv)for@$C;&$_($Fv)for@$C;&$_($Iv)for@$C;&$_($Mv)for@$C;&$_($Pv)for@$z6;&$_($Tv)for@$C;&$_($Vv)for@$C;&$_($Yv)for@$z6;&$_($fw)for@$C;&$_($iw)for@$z6;&$_($kw)for@$lw;&$_($sw)for@$C;&$_($ww)for@$C;&$_($zw)for@$z6;&$_($Bw)for@$P9;&$_($Mw)for@$C;&$_($Pw)for@$z6;&$_($Vw)for@$C;&$_($Zw)for@$C;&$_($ex)for@$z6;&$_($kx)for@$C;&$_($nx)for@$z6;&$_($sx)for@$C;&$_($wx)for@$C;&$_($Ax)for@$C;&$_($Ex)for@$C;&$_($Ix)for@$C;&$_($Mx)for@$C;&$_($Qx)for@$C;&$_($Ux)for@$C;&$_($Yx)for@$C;&$_($dy)for@$z6;&$_($iy)for@$C;&$_($ly)for@$z6;&$_($qy)for@$C;&$_($uy)for@$C;&$_($yy)for@$C;&$_($Cy)for@$C;&$_($Gy)for@$C;&$_($Jy)for@$z6;&$_($Oy)for@$C;&$_($Sy)for@$C;&$_($Wy)for@$C;&$_($Zy)for@$z6;&$_($gz)for@$C;&$_($kz)for@$C;&$_($oz)for@$C;&$_($rz)for@$z6;&$_($tz)for@$uz;&$_($zz)for@$P9;&$_($Jz)for@$C;&$_($Nz)for@$C;&$_($Rz)for@$C;&$_($Vz)for@$C;&$_($Zz)for@$C;&$_($eA)for@$z6;&$_($oA)for@$C;&$_($rA)for@$C;&$_($uA)for@$C;&$_($xA)for@$z6;&$_($BA)for@$C;&$_($EA)for@$C;&$_($HA)for@$C;&$_($KA)for@$C;&$_($MA)for@$C;&$_($PA)for@$z6;&$_($SA)for@$C;&$_($VA)for@$C;&$_($YA)for@$z6;&$_($eB)for@$C;&$_($hB)for@$C;&$_($kB)for@$z6;&$_($mB)for@$nB;&$_($sB)for@$P9;&$_($CB)for@$C;&$_($GB)for@$C;&$_($KB)for@$C;&$_($OB)for@$C;&$_($SB)for@$C;&$_($WB)for@$C;&$_($cC)for@$C;&$_($gC)for@$C;&$_($kC)for@$C;&$_($oC)for@$C;&$_($sC)for@$C;&$_($wC)for@$C;&$_($zC)for@$z6;&$_($JC)for@$C;&$_($MC)for@$z6;&$_($PC)for@$C;&$_($SC)for@$C;&$_($VC)for@$z6;&$_($XC)for@$YC;&$_($fD)for@$P9;&$_($oD)for@$C;&$_($qD)for@$C;&$_($xD)for@$z6;&$_($AD)for@$C;&$_($DD)for@$z6;&$_($HD)for@$C;&$_($KD)for@$z6;&$_($MD)for@$ND;&$_($TD)for@$P9;&$_($cE)for@$C;&$_($gE)for@$C;&$_($jE)for@$z6;&$_($qE)for@$C;&$_($tE)for@$z6;&$_($wE)for@$C;&$_($zE)for@$z6;&$_($BE)for@$CE;&$_($IE)for@$P9;&$_($TE)for@$C;&$_($WE)for@$z6;&$_($YE)for@$ZE;&$_($fF)for@$C;&$_($iF)for@$z6;&$_($mF)for@$C;&$_($qF)for@$C;&$_($tF)for@$C;&$_($wF)for@$z6;&$_($yF)for@$zF;&$_($GF)for@$P9;&$_($IF)for@$P9;&$_($VF)for@$C;&$_($YF)for@$C;&$_($dG)for@$z6;&$_($hG)for@$C;&$_($kG)for@$z6;&$_($oG)for@$C;&$_($rG)for@$z6;&$_($vG)for@$C;&$_($yG)for@$C;&$_($BG)for@$z6;&$_($GG)for@$C;&$_($JG)for@$C;&$_($MG)for@$z6;&$_($OG)for@$PG;&$_($WG)for@$C;&$_($ZG)for@$z6;&$_($fH)for@$C;&$_($jH)for@$C;&$_($mH)for@$z6;&$_($oH)for@$P9;&$_($CH)for@$C;&$_($FH)for@$z6;&$_($KH)for@$C;&$_($OH)for@$C;&$_($RH)for@$z6;&$_($VH)for@$C;&$_($YH)for@$z6;&$_($cI)for@$dI;&$_($iI)for@$P9;&$_($qI)for@$rI;&$_($wI)for@$P9;&$_($FI)for@$GI;&$_($LI)for@$P9;&$_($TI)for@$UI;&$_($sJ)for@$UI;&$_($xJ)for@$C;&$_($BJ)for@$C;&$_($FJ)for@$C;&$_($JJ)for@$C;&$_($NJ)for@$C;ni->run(@ARGV);
__DATA__
