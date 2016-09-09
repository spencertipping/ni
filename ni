#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
Copyright (c) 2016 Spencer Tipping | MIT license

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
eval($ni::self{ni} = <<'_');
sub ni::boot_header
{ join "\n", '#!/usr/bin/env perl',
             "\$ni::self{license} = <<'_';\n$ni::self{license}_",
             "eval(\$ni::self{ni} = <<'_');\n$ni::self{ni}_",
             "die \$@ if \$@",
             "__DATA__" }

sub ni::unsdoc
{ join '', grep !/^\s*[|A-Z]/ + s/^\s*c\n//, split /\n(\s*\n)+/, $_[0] }

sub ni::eval($$)
{ @ni::evals{eval('__FILE__') =~ /\(eval (\d+)\)/} = ($_[1]);
  my @r = eval "package ni;$_[0]";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1 - 1}/eg, die $@ if $@;
  @r }

sub ni::set
{ chomp($ni::self{$_[0]} = $_[1]);
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc$/;
  ni::eval $_[1], $_[0]                           if $_[0] =~ /\.pl$/ }

ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\s*(\d+)\s+(.*?)(\.sdoc)?$/;
ni::eval 'exit main @ARGV', 'main';
_
die $@ if $@
__DATA__
30 ni.map.sdoc
Resource layout map.
ni is assembled by following the instructions here. This script is also
included in the ni image itself so it can rebuild accordingly. The filenames
referenced from this file correspond to SDoc-processed entries in src/.

Note that these are just the entries for the core image. ni modifies itself
during the build process to include more extensions, each of which lives in a
subdirectory of src/.

bootcode
resource ni.map.sdoc

resource util.pl.sdoc
resource dev.pl.sdoc
resource parse.pl.sdoc
resource common.pl.sdoc
resource cli.pl.sdoc
resource op.pl.sdoc
resource self.pl.sdoc
resource main.pl.sdoc
lib core/stream
lib core/meta
lib core/checkpoint
lib core/gen
lib core/col
lib core/row
lib core/facet
lib core/pl
lib core/lisp
lib doc
45 util.pl.sdoc
Utility functions.
Generally useful stuff, some of which makes up for the old versions of Perl we
need to support.

sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}

sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}

sub rf  {open my $fh, "< $_[0]" or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]" or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

Module loading.
ni can include .pm files in its resource stream, which contain Perl code but
aren't evaluated by default. This function is used to eval them into the
current runtime.

sub load($) {ni::eval $self{$_[0]}, $_[0]}

Quoted function support.
Functions that store their code in string form. This is useful for two
purposes: first, it enables you to recompile things, e.g. for dynamic inlining;
and second, it makes functions self-documenting, particularly in the context of
parser combinators.

{
package ni::fn;
use overload qw/ &{} f "" source /;
sub new {
  my ($class, $code) = @_;
  bless {f => ni::eval("sub {$code\n}", "anon sub{$code}"), code => $code},
        $class;
}

sub f($)      {${$_[0]}{f}}
sub source($) {${$_[0]}{code}}
}

sub fn($) {ref($_[0]) ? $_[0] : ni::fn->new($_[0])}
29 dev.pl.sdoc
Development functions.
Utilities helpful for debugging and developing ni.

sub dev_inspect($;\%) {
  local $_;
  my ($x, $refs) = (@_, {});
  return "<circular $x>" if exists $$refs{$x};

  $$refs{$x} = $x;
  my $r = 'ARRAY' eq ref $x ? '[' . join(', ', map dev_inspect($_, $refs), @$x) . ']'
        : 'HASH'  eq ref $x ? '{' . join(', ', map "$_ => " . dev_inspect($$x{$_}, $refs), keys %$x) . '}'
        : "" . $x;
  delete $$refs{$x};
  $r;
}

sub dev_trace($) {
  my ($fname) = @_;
  my $f = \&{$fname};
  my $indent = '';
  *{$fname} = sub {
    printf STDERR "$indent$fname %s ...\n", dev_inspect [@_];
    $indent .= "  ";
    my @r = &$f(@_);
    $indent =~ s/  $//;
    printf STDERR "$indent$fname %s = %s\n", dev_inspect([@_]), dev_inspect [@r];
    @r;
  };
}
102 parse.pl.sdoc
Parser combinators.
List-structured combinators. These work like normal parser combinators, but are
indirected through data structures to make them much easier to inspect. This
allows ni to build an operator mapping table.

use strict 'refs';

our %parsers;
sub defparser($$$) {
  my ($name, $proto, $f) = @_;
  $parsers{$name} = fn $f;
  eval "sub p$name($proto) {['$name', \@_]}";
}

sub parse {
  my ($p, @args) = @{$_[0]};
  my $f = $parsers{$p} or die "ni: no such parser: $p";
  &$f(@_);
}

Base parsers.
Stuff for dealing with some base cases.

c
BEGIN {
  defparser 'end',   '',  sub {@_ > 1       ? () : (0)};
  defparser 'empty', '',  sub {length $_[1] ? () : (0, @_[2..$#_])};
  defparser 'k',     '$', sub {(${$_[0]}[1], @_[1..$#_])};
  defparser 'none',  '',  sub {(undef,       @_[1..$#_])};
}

Basic combinators.
Sequence, alternation, etc. 'alt' implies a sequence of alternatives; 'dsp' is
a dispatch on specified prefixes. The 'r' suffix means that the parser
combinator takes a reference to a collection; this allows you to modify the
collection later on to add more alternatives.

c
BEGIN {
  defparser 'altr', '\@',
    sub {my ($self, @xs, @ps, @r) = @_;
         @r = parse $_, @xs and return @r for @ps = @{$$self[1]}; ()};

  defparser 'dspr', '\%',
    sub {my ($self, $x, @xs, $k, @ys, %ls) = @_;
         my (undef, $ps) = @$self;
         ++$ls{length $_} for keys %$ps;
         for my $l (sort {$b <=> $a} keys %ls) {
           return (@ys = parse $$ps{$c}, substr($x, $l), @xs) ? @ys : ()
           if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
         }
         ()};
}

sub palt(@) {my @ps = @_; paltr @ps}
sub pdsp(%) {my %ps = @_; pdspr %ps}

c
BEGIN {
  defparser 'seq', '@',
    sub {my ($self, @is, $x, @xs, @ys) = @_;
         my (undef, @ps) = @$self;
         (($x, @is) = parse $_, @is) ? push @xs, $x : return () for @ps;
         (\@xs, @is)};

  defparser 'rep', '$;$',
    sub {my ($self, @is, @c, @r) = @_;
         my (undef, $p, $n) = (@$self, 0);
         push @r, $_ while ($_, @is) = parse $p, (@c = @is);
         @r >= $n ? (\@r, @c) : ()};

  defparser 'opt', '$',
    sub {my ($self, @is) = @_;
         my @xs = parse $$self[1], @is; @xs ? @xs : (undef, @is)};

  defparser 'map', '$$',
    sub {my ($self, @is) = @_;
         my (undef, $f, $p) = @$self;
         $f = fn $f;
         my @xs = parse $p, @is; @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()};

  defparser 'cond', '$$',
    sub {my ($self, @is) = @_;
         my (undef, $f, $p) = @$self;
         $f = fn $f;
         my @xs = parse $p, @is; @xs && &$f($_ = $xs[0]) ? @xs : ()};
}

sub pn($@) {my ($n, @ps) = @_; pmap fn "\$\$_[$n]", pseq @ps}

Regex parsing.
Consumes the match, returning either the matched text or the first match group
you specify. Always matches from the beginning of a string.

c
BEGIN {
  defparser 'rx', '$',
    sub {my ($self, $x, @xs) = @_;
         $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()};
}

sub prc($) {pn 0, prx qr/$_[0]/, popt pempty}
90 common.pl.sdoc
Regex parsing.
Sometimes we'll have an operator that takes a regex, which is subject to the
CLI reader problem the same way code arguments are. Rather than try to infer
brackets the same way, we just require that regexes are terminated with /
(which should be ok because that's also how they typically start).

use constant regex => pmap q{s/\/$//; $_}, prx qr{^(?:[^\\/]+|\\.)*/};

Generic code parser.
Counts brackets outside quoted strings, which in our case are '' and "".
Doesn't look for regular expressions because these vary by language; but this
parser should be able to handle most straightforward languages with quoted
string literals and backslash escapes.

defparser 'generic_code', '',
  sub {my ($self, $code, @xs) = @_;
       return ($code, @xs) unless $code =~ /\]$/;
       (my $tcode = $code) =~ s/"([^"\\]+|\\.)"|'([^'\\]+|\\.)'//g;
       my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
       $balance ? (substr($code, 0, $balance), substr($code, $balance))
                : ($code, @xs)};

Code parsing.
This is nontrivial due to the CLI reader problem. The idea is that we need to
figure out how many closing brackets belong to the code, vs how many close a
lambda. Depending on the language, the only way to do this may be to shell out
to an interpreter.

defparser 'rbcode', '',
  sub {return @_[1..$#_] unless $_[1] =~ /\]$/;
       my ($self, $code, @xs, $x, $qcode) = @_;
       ($qcode = $code) =~ s/'/'\\''/g;
       $x .= ']' while $_ = system("ruby -ce '$qcode' >/dev/null 2>&1")
                       and ($qcode =~ s/\]$//, $code =~ s/\]$//);
       $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

Perl code is similar to Ruby, but we need to explicitly disable any BEGIN{}
blocks to avoid executing side effects. We can guarantee that nothing will run
(beyond `use` statements, which we assume are safe) by removing any
occurrences of the string `BEGIN` and replacing them with something
syntactically equivalent but less volatile -- in this case, `END`.

defparser 'plcode', '',
  sub {return @_[1..$#_] unless $_[0] =~ /\]$/;
       my ($self, $code, @xs, $x, $qcode) = @_;
       ($qcode = $code) =~ s/'/'\\''/g;

       my $begin_warning = $qcode =~ s/BEGIN/END/g;
       $x .= ']' while $_ = system("perl -ce '$qcode' >/dev/null 2>&1")
                       and ($qcode =~ s/\]$//, $code =~ s/\]$//);

       print STDERR <<EOF if $_ && $begin_warning;
ni: failed to get closing bracket count for perl code "$_[0]", possibly
    because BEGIN-block metaprogramming is disabled when ni tries to figure
    this out. To avoid this, bypass bracket inference by terminating your code
    with a single space, e.g:

    p'[[some code]]'            # this fails due to bracket inference
    p'[[some code]] '           # this works by bypassing it
EOF
       $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

Basic CLI types.
Some common argument formats for various commands, sometimes transformed for
specific cases. These are documented somewhere in `doc/`.

use constant neval   => pmap q{eval}, prx '=([^]]+)';
use constant integer => palt pmap(q{int},       neval),
                             pmap(q{10 ** $_},  prx 'E(-?\d+)'),
                             pmap(q{1 << $_},   prx 'B(\d+)'),
                             pmap(q{0 + "0$_"}, prx 'x[0-9a-fA-F]+'),
                             pmap(q{0 + $_},    prx '\d+');
use constant float   => pmap q{0 + $_}, prx '-?\d*(?:\.\d+)?(?:[eE][-+]?\d+)?';
use constant number  => palt neval, integer, float;

use constant colspec1 => prx '[A-Z]';
use constant colspec  => prx '[-A-Z.]+';

Filenames, in general.
Typically filenames won't include bracket characters, though they might include
just about everything else. Two possibilities there: if we need special stuff,
there's the `file:` prefix; otherwise we assume the non-bracket interpretation.

use constant tmpdir   => dor $ENV{TMPDIR}, '/tmp';
use constant tempfile => pmap q{tmpdir . "/ni-$$-$_"}, prx '^@:(\w*)';

use constant filename => palt prc '^file:(.+)', tempfile,
                              pcond q{-e}, prx '^[^][]+';

use constant nefilename => palt filename, prx '^[^][]+';
46 cli.pl.sdoc
CLI grammar.
ni's command line grammar uses some patterns on top of the parser combinator
primitives defined in parse.pl.sdoc. Probably the most important one to know
about is the long/short option dispatch, which looks like this:

| option = alt @longs, dsp %shorts

our %contexts;
our %long_refs;
our %short_refs;

sub defcontext($) {
  $short_refs{$_[0]} = {};
  $long_refs{$_[0]}  = [pdspr %{$short_refs{$_[0]}}];
  $contexts{$_[0]}   = paltr @{$long_refs{$_[0]}};
}

sub defshort($$) {
  my ($context, $dsp) = split /\//, $_[0];
  die "ni: defshort cannot be used to redefine '$_[0]' (use rmshort first)"
    if exists $short_refs{$context}{$dsp};
  $short_refs{$context}{$dsp} = $_[1];
}

sub deflong($$) {
  my ($context, $name) = split /\//, $_[0];
  unshift @{$long_refs{$context}}, $_[1];
}

sub rmshort($) {
  my ($context, $dsp) = split /\//, $_[0];
  delete $short_refs{$context}{$dsp};
}

CLI grammar elements.
Generators for various syntactic constructs given a context. Here's what they
represent:

| pseries(context): a chain of consecutive operators in the context
  plambda(context): a lambda-list: [ chain ]
  pcli(context):    a complete command-line within the context

sub pseries($)    {prep pn 1, popt pempty, $contexts{$_[0]}, popt pempty}
sub plambda($)    {pn 1, prc qr/\[/, pseries $_[0], prc qr/\]/}
sub pcli($)       {pn 0, pseries $_[0], pend}
sub pcli_debug($) {pseries $_[0]}
24 op.pl.sdoc
Operator definition.
Like ni's parser combinators, operators are indirected using names. This
provides an intermediate representation that can be inspected and serialized.

our %operators;
sub defoperator($$) {
  my ($name, $f) = @_;
  $operators{$name} = fn $f;
  ni::eval "sub ${name}_op(@) {['$name', \@_]}", "defoperator $name";
  ni::eval "sub ${name}_run(@) {\$operators{\$name}->(\@_)}",
           "defoperator $name ($f)";
}

sub operate {
  my ($name, @args) = @_;
  die "ni operate: undefined operator: $name" unless exists $operators{$name};
  $operators{$name}->(@args);
}

sub flatten_operators($) {
  my ($name) = @{$_[0]};
  return $_[0] unless ref $name;
  map flatten_operators($_), @{$_[0]};
}
39 self.pl.sdoc
Image functions.
ni needs to be able to reconstruct itself from a map. These functions implement
the map commands required to do this.

sub quote_resource {map sprintf("%d %s\n%s", scalar(split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub quote_library  {map {my $l = $_;
                         quote_resource "$_/lib", map "$l/$_", split /\n/, $self{"$_/lib"}} @_}

sub read_map {join '', map "$_\n",
                       (map {my ($c, @a) = split /\s+/;
                               $c eq 'bootcode'    ? ni::boot_header
                             : $c eq 'resource'    ? quote_resource @a
                             : $c =~ /^lib$|^ext$/ ? quote_library @a
                             : die "ni: unknown map command+args: $c @a"}
                        grep {s/#.*//g; length}
                        map split(/\n/), @self{@_}), "__END__"}

sub intern_lib($) {
  my ($l) = @_;
  set "$l/$_", rfc "$l/$_"
    for grep length, split /\n/, ($self{"$l/lib"} = rfc "$l/lib");
}

sub modify_self($) {
  die "ni: not a modifiable instance: $0" unless -w $0;
  open my $fh, "> $0" or die "ni: failed to open self: $!";
  print $fh read_map $_[0];
  close $fh;
}

sub extend_self($$) {
  my ($type, $lib) = @_;
  intern_lib $lib;
  set 'ni.map.sdoc', "$self{'ni.map.sdoc'}\n$type $lib"
    unless grep /^(lib|ext)\s+$lib$/, split /\n/, $self{'ni.map'};
  modify_self 'ni.map';
}

sub image {read_map 'ni.map'}
66 main.pl.sdoc
CLI entry point.
Some custom toplevel option handlers and the main function that ni uses to
parse CLI options and execute the data pipeline.

use strict;
use POSIX qw/:sys_wait_h/;

sub sigchld_handler {
  local ($!, $?);
  1 while 0 < waitpid -1, WNOHANG;
  $SIG{CHLD} = \&sigchld_handler;
};
$SIG{CHLD} = \&sigchld_handler;

our %cli_special;
sub defclispecial($$) {$cli_special{$_[0]} = fn $_[1]}

Development options.
Things useful for developing ni.

defclispecial '--dev/self', q{print "$ni::self{$_[0]}\n"};
defclispecial '--dev/parse', q{
  dev_trace 'ni::parse';
  parse pcli '', @_;
};

Extensions.
Options to extend and modify the ni image.

defclispecial '--internal/lib', q{extend_self 'lib', $_[0]};

Documentation.

defclispecial '--explain', q{
  my ($r) = parse pcli '', @_;
  print join("\t", map dev_inspect($_), @$_), "\n" for @$r;
};

Root CLI context.
This is used by extensions that define long and short options.

defcontext '';

Main stuff.
sub main() is called by the ni boot header on @ARGV. I've separated
$main_operator so it can be extended to handle various cases; for instance, ni
launches a pager when its output is connected to a terminal, etc. This is
handled by core/stream.

our $main_operator = sub {operate @$_ for @_};

sub main {
  my ($cmd, @args) = @_;
  return $cli_special{$cmd}->(@args) if exists $cli_special{$cmd};

  @_ = ('//help')
    if -t STDIN and -t STDOUT and !@_ || $_[0] =~ /^-h$|^-\?$|^--help$/;

  my ($r) = parse pcli '', @_;
  return &$main_operator(flatten_operators $r) if ref $r;

  my (undef, @rest) = parse pcli_debug '', @_;
  print STDERR "ni: failed to parse starting here (ni --dev/parse to trace):\n";
  print STDERR "  @rest\n";
  exit 1;
}
2 core/stream/lib
pipeline.pl.sdoc
ops.pl.sdoc
184 core/stream/pipeline.pl.sdoc
Pipeline construction.
A way to build a shell pipeline in-process by consing a transformation onto
this process's standard input. This will cause a fork to happen, and the forked
PID is returned.

I define some system functions with a `c` prefix: these are checked system
calls that will die with a helpful message if anything fails.

use Errno qw/EINTR/;
use POSIX qw/dup2/;

sub cdup2 {dup2 @_ or die "ni: dup2(@_) failed: $!"}
sub cpipe {pipe $_[0], $_[1] or die "ni: pipe failed: $!"}
sub cfork {my $pid = fork; die "ni: fork failed: $!" unless defined $pid; $pid}

sub move_fd($$) {
  my ($old, $new) = @_;
  return if $old == $new;
  close $new;
  cdup2 $old, $new;
  close $old;
}

Safe reads/writes.
This is required because older versions of Perl don't automatically retry
interrupted reads/writes. We run the risk of interruption because we have a
SIGCHLD handler. nfu lost data on older versions of Perl because it failed to
handle this case properly.

sub saferead($$$) {
  my $n = undef;
  do {
    return $n if defined($n = sysread $_[0], $_[1], $_[2]);
  } while $!{EINTR};
  return undef;
}

sub safewrite($$) {
  my $n = undef;
  do {
    return $n if defined($n = syswrite $_[0], $_[1]);
  } while $!{EINTR};
  return undef;
}

Process construction.
A few functions, depending on what you want to do:

| siproc(&): fork into block, return pipe FH to block's STDIN.
  soproc(&): fork into block, return pipe FH from block's STDOUT.
  sicons(&): fork into block, connect its STDOUT to our STDIN.
  socons(&): fork into block, connect our STDOUT to its STDIN.

NOTE: forkopen does something strange and noteworthy. You'll notice that it's
reopening STDIN and STDOUT from FDs, which seems redundant. This is required
because Perl filehandles aren't the same as OS-level file descriptors, and ni
deals with both in different ways.

In particular, ni closes STDIN (the filehandle) if the input comes from a
terminal, since presumably the user doesn't intend to type their input in
manually. This needs to happen before any exec() from a forked filter process.
But this creates a problem: if we later reactivate fd 0, which we do by moving
file descriptors from a pipe. We have to do this at the fd level so exec()
works correctly (since exec doesn't know anything about Perl filehandles, just
fds). Anyway, despite the fact that fd 0 is newly activated by an sicons {}
operation, Perl's STDIN filehandle will think it's closed and return no data.

So that's why we do these redundant open STDIN and STDOUT operations. At some
point I might bypass Perl's IO layer altogether and use POSIX calls, but at the
moment that seems like more trouble than it's worth.

sub forkopen($$) {
  my ($fd, $f) = @_;
  cpipe my $r, my $w;
  my ($ret, $child) = ($r, $w)[$fd ^ 1, $fd];
  if (cfork) {
    close $child;
    return $ret;
  } else {
    close $ret;
    move_fd fileno $child, $fd;
    open STDIN,  '<&=0' if $fd == 0;
    open STDOUT, '>&=1' if $fd == 1;
    &$f;
    exit;
  }
}

sub siproc(&) {forkopen 0, $_[0]}
sub soproc(&) {forkopen 1, $_[0]}
sub sicons(&) {my ($f) = @_; move_fd fileno soproc {&$f}, 0; open STDIN,  '<&=0'}
sub socons(&) {my ($f) = @_; move_fd fileno siproc {&$f}, 1; open STDOUT, '>&=1'}

Stream functions.
These are called by pipelines to simplify things. For example, a common
operation is to append the output of some data-producing command:

| $ ni . .              # lists current directory twice

If you do this, ni will create a pipeline that uses stream wrappers to
concatenate the second `ls` output (despite the fact that technically it's a
shell pipe).

sub sforward($$) {local $_; safewrite $_[1], $_ while saferead $_[0], $_, 8192}
sub stee($$$)    {local $_; safewrite($_[1], $_), safewrite($_[2], $_) while saferead $_[0], $_, 8192}
sub sappend(&)   {sforward \*STDIN, \*STDOUT; &{$_[0]}}
sub sprepend(&)  {&{$_[0]}; sforward \*STDIN, \*STDOUT}

sub srfile($) {open my $fh, '<', $_[0] or die "ni: srfile $_[0]: $!"; $fh}
sub swfile($) {open my $fh, '>', $_[0] or die "ni: swfile $_[0]: $!"; $fh}

Compressed stream support.
This provides a stdin filter you can use to read the contents of a compressed
stream as though it weren't compressed. It's implemented as a filter process so
we don't need to rely on file extensions.

We detect the following file formats:

| gzip:  1f 8b
  bzip2: BZh\0
  lzo:   89 4c 5a 4f
  lz4:   04 22 4d 18
  xz:    fd 37 7a 58 5a

Decoding works by reading enough to decode the magic, then forwarding data
into the appropriate decoding process (or doing nothing if we don't know what
the data is).

sub sdecode(;$) {
  local $_;
  return unless saferead \*STDIN, $_, 8192;

  my $decoder = /^\x1f\x8b/             ? "gzip -dc"
              : /^BZh\0/                ? "bzip2 -dc"
              : /^\x89\x4c\x5a\x4f/     ? "lzop -dc"
              : /^\x04\x22\x4d\x18/     ? "lz4 -dc"
              : /^\xfd\x37\x7a\x58\x5a/ ? "xz -dc" : undef;

  if (defined $decoder) {
    open my $o, "| $decoder" or die "ni_decode: failed to open '$decoder': $!";
    safewrite $o, $_;
    sforward \*STDIN, $o;
  } else {
    safewrite \*STDOUT, $_;
    sforward \*STDIN, \*STDOUT;
  }
}

File/directory cat.
cat exists to turn filesystem objects into text. Files are emitted and
directories are turned into readable listings. Files are automatically
decompressed.

sub scat {
  for my $f (@_) {
    if (-d $f) {
      $| = 1;
      opendir my $d, $f or die "ni_cat: failed to opendir $f: $!";
      print "$f/$_\n" for sort grep !/^\.\.?$/, readdir $d;
      closedir $d;
    } else {
      sforward srfile $f, siproc {sdecode};
    }
  }
}

Self invocation.
You can run ni and read from the resulting file descriptor; this gives you a
way to evaluate lambda expressions (this is how checkpoints work, for example).
If you do this, the ni subprocess won't receive anything on its standard input.

sub shell_quote {join ' ', map /[^-A-Za-z_0-9\/:@.]/
                                 ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                                 : $_,
                           map 'ARRAY' eq ref($_) ? shell_quote(@$_) : $_, @_}

sub sni(@) {
  my $q = shell_quote @_;
  soproc {
    open my $fh, '| perl - ' . $q
      or die "ni: sni @args failed to fork to perl: $!";
    syswrite $fh, image;
  };
}
83 core/stream/ops.pl.sdoc
Streaming data sources.
Common ways to read data, most notably from files and directories. Also
included are numeric generators, shell commands, etc.

$main_operator = sub {
  -t STDIN ? close STDIN : sicons {sdecode};
  @$_ && sicons {operate @$_} for @_;
  exec 'less' or exec 'more' if -t STDOUT;
  sforward \*STDIN, \*STDOUT;
};

use constant shell_command => prc '.*';

defoperator cat  => q{my ($f) = @_; sappend {scat $f}};
defoperator echo => q{my ($x) = @_; sappend {print "$x\n"}};
defoperator sh   => q{my ($c) = @_; sappend {exec $c}};

Note that we generate numbers internally rather than shelling out to `seq`
(which is ~20x faster than Perl for the purpose, incidentally). This is
deliberate: certain versions of `seq` generate floating-point numbers after a
point, which can cause unexpected results and loss of precision.

defoperator n => q{
  my ($l, $u) = @_;
  sappend {for (my $i = $l; $i < $u; ++$i) {print "$i\n"}};
};

defshort '/n',   pmap q{n_op 1, $_ + 1}, number;
defshort '/n0',  pmap q{n_op 0, $_}, number;
defshort '/id:', pmap q{echo_op $_}, prc '.*';

defshort '/$:',  pmap q{sh_op $_}, shell_command;
defshort '/sh:', pmap q{sh_op $_}, shell_command;

deflong '/fs', pmap q{cat_op $_}, filename;

Shell transformation.
Pipe through a shell command. We also define a command to duplicate a stream
through a shell command.

defoperator pipe => q{exec $_[0] or die "ni: failed to exec $_[0]: $!"};
defshort '/$=', pmap q{pipe_op $_}, shell_command;

defoperator tee => q{
  my ($cmd) = @_;
  open my $fh, "| $cmd" or die "ni: tee $cmd failed: $!";
  stee \*STDIN, $fh, \*STDOUT;
};
defshort '/$^', pmap q{tee_op $_}, shell_command;

Sinking.
We can sink data into a file just as easily as we can read from it. This is
done with the `>` operator, which is typically written as `\>`.

defoperator file_tee   => q{stee \*STDIN, swfile($_[0]), \*STDOUT};
defoperator file_write => q{sforward \*STDIN, swfile $_[0]; print "$_[0]\n"};
defoperator file_read  => q{chomp, scat $_ while <STDIN>};

defshort '/>%', pmap q{file_tee_op $_},   nefilename;
defshort '/>',  pmap q{file_write_op $_}, nefilename;
defshort '/<',  pmap q{file_read_op},     pnone;

Compression and decoding.
Sometimes you want to emit compressed data, which you can do with the `Z`
operator. It defaults to gzip, but you can also specify xz, lzo, lz4, or bzip2
by adding a suffix. You can decode a stream in any of these formats using `ZD`
(though in most cases ni will automatically decode compressed formats).

our %compressors = qw/ g gzip  x xz  o lzop  4 lz4  b bzip2 /;

use constant compressor_name => prx '[gxo4b]';
use constant compressor_spec =>
  pmap q{my ($c, $level) = @$_;
         $c = $compressors{$c || 'g'};
         defined $level ? pipe_op "$c -$level" : pipe_op $c},
  pseq popt compressor_name, popt integer;

defoperator sink_null => q{1 while saferead \*STDIN, $_, 8192};
defoperator decode    => q{sdecode};

defshort '/Z',  compressor_spec;
defshort '/ZN', pk sink_null_op();
defshort '/ZD', pk decode_op();
1 core/meta/lib
meta.pl.sdoc
32 core/meta/meta.pl.sdoc
Image-related data sources.
Long options to access ni's internal state. Also the ability to instantiate ni
within a shell process.

defoperator 'meta_image', q{sappend {print image, "\n"}};
defoperator 'meta_keys',  q{sappend {print "$_\n" for @keys}};
defoperator 'meta_key',   q{sappend {print "$_\n" for @self{@_}}};

defoperator 'meta_help', q{
  my ($topic) = @_;
  $topic = 'tutorial' unless length $topic;
  sappend {print $self{"doc/$topic.md"}, "\n"};
};

deflong '/meta_self', pmap q{meta_image_op},  prx '//ni';
deflong '/meta_keys', pmap q{meta_keys_op},   prx '//ni/';
deflong '/meta_get',  pmap q{meta_key_op $_}, prx '//ni/([^]]+)';

Documentation options.
These are listed under the `//help` prefix. This isn't a toplevel option
because it's more straightforward to model these as data sources.

deflong '/meta_help', pmap q{meta_help_op $_}, prx '^//help/?(.*)';

defoperator 'meta_options', q{
  for my $c (sort keys %contexts) {
    printf "%s\tshort\t%s\t%s\n", $c, $_, sgr dev_inspect($short_refs{$c}->{$_}), qr/\n/, ' ' for sort keys %{$short_refs{$c}};
    printf "%s\tlong\t%s\t%s\n",  $c,     sgr dev_inspect($_),                    qr/\n/, ' ' for           @{$long_refs{$c}};
  }
};

deflong '/meta_options', pmap q{meta_options_op}, prx '//ni/options';
1 core/checkpoint/lib
checkpoint.pl.sdoc
17 core/checkpoint/checkpoint.pl.sdoc
Checkpoint files.
You can break a long pipeline into a series of smaller files using
checkpointing, whose operator is `:`. The idea is to cache intermediate
results. A checkpoint specifies a file and a lambda whose output it should
capture.

sub checkpoint_create($$) {
  stee sni(@{$_[1]}), swfile "$_[0].part", \*STDOUT;
  rename "$_[0].part", $_[0];
}

defoperator 'checkpoint', q{
  my ($file, $generator) = @_;
  sappend {-r $file ? scat $file : checkpoint_create $file, $generator};
};

defshort '/:', pmap q{checkpoint_op $$_[0], $$_[1]}, pseq nefilename, plambda '';
1 core/gen/lib
gen.pl.sdoc
34 core/gen/gen.pl.sdoc
Code generator.
A general-purpose interface to do code-generation stuff. This is used when
you've got a task that's mostly boilerplate of some kind, but you've got
variable regions. For example, if you wanted to generalize JVM-hosted
command-line filters:

| my $java_linefilter = gen q{
    import java.io.*;
    public class %classname {
      public static void main(String[] args) {
        BufferedReader stdin = <the ridiculous crap required to do this>;
        String %line;
        while ((%line = stdin.readLine()) != null) {
          %body;
        }
      }
    }
  };
  my $code = &$java_linefilter(classname => 'Foo',
                               line      => 'line',
                               body      => 'System.out.println(line);');

our $gensym_index = 0;
sub gensym {join '_', '_gensym', ++$gensym_index, @_}

sub gen($) {
  my @pieces = split /(%\w+)/, $_[0];
  sub {
    my %vars = @_;
    my @r = @pieces;
    $r[$_] = $vars{substr $pieces[$_], 1} for grep $_ & 1, 0..$#pieces;
    join '', @r;
  };
}
1 core/col/lib
col.pl.sdoc
73 core/col/col.pl.sdoc
Column manipulation operators.
In root context, ni interprets columns as being tab-delimited.

Column selection.
Normally perl is fast at text manipulation, but on most UNIX systems
`/usr/bin/cut` is at least an order of magnitude faster. We can use it if two
conditions are met:

| 1. All addressed columns are at index 8 (9 if one-based) or lower.
  2. The addressed columns are in ascending order.

sub col_cut {
  my ($floor, $rest, @fs) = @_;
  exec 'cut', '-f', join ',', $rest ? (@fs, "$floor-") : @fs;
}

our $cut_gen = gen q{chomp; @_ = split /\t/; print join("\t", @_[%is]), "\n"};

defoperator cols => q{
  # TODO: this function shouldn't be parsing column specs
  my $ind   = grep /[^A-I.]/, @_;
  my $asc   = join('', @_) eq join('', sort @_);
  my @cols  = map /^\.$/ ? -1 : ord($_) - 65, @_;
  my $floor = (sort {$b <=> $a} @cols)[0] + 1;
  return col_cut $floor, scalar(grep $_ eq '.', @_), @cols if $ind && $asc;

  my $body = $cut_gen->(is => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cols);
  eval qq{while (<STDIN>) {$body}};
};

our @col_alt = pmap q{cols_op split //, $_}, colspec;

defshort '/f', paltr @col_alt;

sub defcolalt($) {unshift @col_alt, $_[0]}

Column swapping.
This is such a common thing to do that it gets its own operator `x`. The idea
is that you're swapping the specified column(s) into the first N position(s).

sub ni_colswap(@) {
  # TODO after we do the colspec parsing refactor
}

Column splitting.
Adapters for input formats that don't have tab delimiters. Common ones are,
with their split-spec mnemonics:

| commas:       C
  pipes:        P
  whitespace:   S
  non-words:    W

You can also field-split on arbitrary regexes, or extend the %split_chalt hash
to add custom split operators.

defoperator split_chr   => q{exec 'perl', '-lnpe', "y/$_[0]/\\t/"};
defoperator split_regex => q{exec 'perl', '-lnpe', "s/$_[0]/\$1\\t/g"};
defoperator scan_regex  => q{exec 'perl', '-lne',  'print join "\t", /' . "$_[0]/g"};

our %split_dsp = (
  'C' => pmap(q{split_chr_op   ','},               pnone),
  'P' => pmap(q{split_chr_op   '|'},               pnone),
  'S' => pmap(q{split_regex_op qr/\s+/},           pnone),
  'W' => pmap(q{split_regex_op qr/[^\w\n]+/},      pnone),
  '/' => pmap(q{split_regex_op $_},                regex),
  ':' => pmap(q{split_chr_op   $_},                prx '^.'),
  'm' => pn(1, prx '^/', pmap q{scan_regex_op $_}, regex),
);

defshort '/F', pdspr %split_dsp;

sub defsplitalt($$) {$split_dsp{$_[0]} = $_[1]}
1 core/row/lib
row.pl.sdoc
76 core/row/row.pl.sdoc
Row-level operations.
These reorder/drop/create entire rows without really looking at fields.

defoperator head => q{exec 'head', @_};
defoperator tail => q{exec 'tail', @_};

defoperator row_every => q{$. % $_[0] || print while <STDIN>};
defoperator row_match => q{$\ = "\n"; chomp, /$_[0]/o && print while <STDIN>};
defoperator row_sample => q{
  srand($ENV{NI_SEED} || 42);
  while (<STDIN>) {
    print, $. -= -log(1 - rand()) / $_[0] if $. >= 0;
  }
};

defoperator row_sort => q{exec 'sort', @_};

our @row_alt = (
  pmap(q{tail_op '-n', $_},             pn 1, prx '\+', integer),
  pmap(q{tail_op '-n', '+' . ($_ + 1)}, pn 1, prx '-',  integer),
  pmap(q{row_every_op  $_},             pn 1, prx 'x',  number),
  pmap(q{row_match_op  $_},             pn 1, prx '/',  regex),
  pmap(q{row_sample_op $_},                   prx '\.\d+'),
  pmap(q{head_op '-n', $_},             integer));

defshort '/r', paltr @row_alt;

sub defrowalt($) {unshift @row_alt, $_[0]}

Sorting.
ni has four sorting operators, each of which can take modifiers:

| g     group: sort by byte ordering
  G     groupuniq: sort + uniq by byte ordering
  o     order: sort numeric ascending
  O     rorder: sort numeric descending

Modifiers follow the operator and dictate the column index and, optionally, the
type of sort to perform on that column (though a lot of this is already
specified by which sort operator you use). Columns are specified as A-Z, and
modifiers, which are optional, are any of these:

| g     general numeric sort (not available for all 'sort' versions)
  n     numeric sort
  r     reverse

use constant sortspec => prep pseq colspec1, popt prx '[gnr]+';

sub sort_args {'-t', "\t",
               map {my $i = ord($$_[0]) - 64;
                    my $m = defined $$_[1] ? $$_[1] : '';
                    ('-k', "$i$m,$i")} @_}

defshort '/g', pmap q{row_sort_op        sort_args @$_}, sortspec;
defshort '/G', pmap q{row_sort_op '-u',  sort_args @$_}, sortspec;
defshort '/o', pmap q{row_sort_op '-n',  sort_args @$_}, sortspec;
defshort '/O', pmap q{row_sort_op '-rn', sort_args @$_}, sortspec;

Counting.
Sorted and unsorted streaming counts.

defoperator count => q{
  my ($n, $last) = (0, undef);
  while (<STDIN>) {
    if ($_ ne $last) {
      print "$n\t$_" if defined $last;
      $n = 0;
      $last = $_;
    }
    ++$n;
  }
  print "$n\t$last" if defined $last;
};

defshort '/c', pmap q{count_op},                pnone;
defshort '/C', pmap q{[row_sort_op, count_op]}, pnone;
1 core/facet/lib
facet.pl.sdoc
20 core/facet/facet.pl.sdoc
Faceting support.
A compound operation that prepends a new facet-value column, sorts by that
value, and supports streaming aggregation within facet groups. It's specified
this way for two reasons. First, it's convenient; but second, and more
importantly, not all backends will have the right set of primitives to let you
specify a facet the same way.

The general syntax is like this:

| $ ni n:100 @p'a % 10' 'r a, fr {$_ + b} 0'    # sum each group

This generalizes easily to other programming environments, e.g. SQL GROUP BY:

| $ ni ... [@'mod(x, 10)' 'sum(y)']

our %facet_dsp;

defshort '/@', pdspr %facet_dsp;

sub deffacetalt($$) {$facet_dsp{$_[0]} = $_[1]}
5 core/pl/lib
util.pm.sdoc
math.pm.sdoc
stream.pm.sdoc
reducers.pm.sdoc
pl.pl.sdoc
60 core/pl/util.pm.sdoc
Utility library functions.
Mostly inherited from nfu. This is all loaded inline before any Perl mapper
code. Note that List::Util, the usual solution to a lot of these problems, is
introduced in v5.7.3, so we can't rely on it being there.

sub ceval {eval $_[0]; die $@ if $@}

sub sum  {local $_; my $s = 0; $s += $_ for @_; $s}
sub prod {local $_; my $p = 1; $p *= $_ for @_; $p}

sub mean {scalar @_ && sum(@_) / @_}

sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

sub argmax(&@) {
  local $_;
  my ($f, $m, @xs) = @_;
  my $fm = &$f($m);
  for my $x (@xs) {
    ($m, $fm) = ($x, $fx) if (my $fx = &$f($x)) > $fm;
  }
  $m;
}

sub argmin(&@) {
  local $_;
  my ($f, $m, @xs) = @_;
  my $fm = &$f($m);
  for my $x (@xs) {
    ($m, $fm) = ($x, $fx) if (my $fx = &$f($x)) < $fm;
  }
  $m;
}

sub any(&@) {local $_; my ($f, @xs) = @_; &$f($_) && return 1 for @_; 0}
sub all(&@) {local $_; my ($f, @xs) = @_; &$f($_) || return 0 for @_; 1}

sub uniq  {local $_; my(%seen, @xs); $seen{$_}++ or push @xs, $_ for @_; @xs}
sub freqs {local $_; my %fs; ++$fs{$_} for @_; \%fs}

sub reduce(&$@) {local $_; my ($f, $x, @xs) = @_; $x = &$f($x, $_) for @xs; $x}
sub reductions(&$@) {
  local $_;
  my ($f, $x, @xs, @ys) = @_;
  push @ys, $x = &$f($x, $_) for @xs;
  @ys;
}

sub cart {
  local $_;
  return () unless @_;
  return map [$_], @{$_[0]} if @_ == 1;
  my @ns     = map scalar(@$_), @_;
  my @shifts = reverse reductions {$_[0] * $_[1]} 1 / $ns[0], reverse @ns;
  map {my $i = $_; [map $_[$_][int($i / $shifts[$_]) % $ns[$_]], 0..$#_]}
      0..prod(@ns) - 1;
}
33 core/pl/math.pm.sdoc
Math utility functions.
Mostly geometric and statistical stuff.

use constant tau => 2 * 3.14159265358979323846264;

use constant LOG2  => log 2;
use constant LOG2R => 1 / LOG2;

sub log2 {LOG2R * log $_[0]}
sub quant {my ($x, $q) = @_; $q ||= 1;
           my $s = $x < 0 ? -1 : 1; int(abs($x) / $q + 0.5) * $q * $s}

sub dot {local $_; my ($u, $v) = @_;
         sum map $$u[$_] * $$v[$_], 0..min $#{$u}, $#{$v}}

sub l1norm {local $_; sum map abs($_), @_}
sub l2norm {local $_; sqrt sum map $_*$_, @_}

sub rdeg($) {$_[0] * 360 / tau}
sub drad($) {$_[0] / 360 * tau}

sub prec {($_[0] * sin drad $_[1], $_[0] * cos drad $_[1])}
sub rpol {(l2norm(@_), rdeg atan2($_[0], $_[1]))}

if (eval {require Math::Trig}) {
  sub haversine {
    local $_;
    my ($th1, $ph1, $th2, $ph2) = map drad $_, @_;
    my ($dt, $dp) = ($th2 - $th1, $ph2 - $ph1);
    my $a = sin($dp / 2)**2 + cos($p1) * cos($p2) * sin($dt / 2)**2;
    2 * atan2(sqrt($a), sqrt(1 - $a));
  }
}
75 core/pl/stream.pm.sdoc
Perl stream-related functions.
Utilities to parse and emit streams of data. Handles the following use cases:

| $ ni n:10p'a + a'             # emit single value
  $ ni n:10p'a, a * a'          # emit multiple values vertically
  $ ni n:10p'r a, a * a'        # emit multiple values horizontally

The 'pr' function can bypass split /\t/, which is useful in high-throughput
situations. For example:

| $ ni n:10p'pr "$_\tfoo"'      # append a new field without splitting

Uppercase letters are quoted fields: A == 'a'. This is useful when defining
lazy facets (see facet.pm.sdoc).

Lowercase letters followed by underscores are field-extractors that can take an
array of lines and return an array of field values. These are useful in
conjunction with the line-reading functions `rw`, `ru`, and `re`.

our @q;
our @F;
our $l;

sub rl()   {$l = $_ = @q ? shift @q : <STDIN>; @F = (); $_}
sub F_(@)  {chomp $l, @F = split /\t/, $l unless @F; @_ ? @F[@_] : @F}
sub r(@)   {(my $l = join "\t", @_) =~ s/\n//g; print "$l\n"; ()}
sub pr(;$) {(my $l = @_ ? $_[0] : $_) =~ s/\n//g; print "$l\n"; ()}
BEGIN {ceval sprintf 'sub %s() {F_ %d}', $_, ord($_) - 97 for 'b'..'q';
       ceval sprintf 'sub %s() {"%s"}', uc, $_ for 'a'..'q';
       ceval sprintf 'sub %s_  {local $_; map((split /\t/)[%d], @_)}',
                     $_, ord($_) - 97 for 'a'..'q'}

Optimize access to the first field; in particular, no need to fully populate @F
since no seeking needs to happen. This should improve performance for faceting
workflows.

sub a() {@F ? $F[0] : substr $l, 0, index $l, "\t"}

Seeking functions.
It's possible to read downwards (i.e. future lines), which returns an array and
sends the after-rejected line into the lookahead queue to be used by the next
iteration. Mnemonics:

| rw: read while condition
  ru: read until condition
  re: read while equal

These functions all read things into memory. If you want to stream stuff, you
can do it in two ways. One is to use control flow with the 'rl' (read line)
function:

| do_stuff until rl =~ /<\//;           # iterate until closing XML tag
  push @q, $_;                          # important: stash rejected line

The other is to use the faceting functions defined in facet.pm.

sub rw(&) {my @r = ($l); push @r, $l while  defined rl && &{$_[0]}; push @q, $l if defined $l; @r}
sub ru(&) {my @r = ($l); push @r, $l until !defined rl || &{$_[0]}; push @q, $l if defined $l; @r}
sub re(&) {my ($f, $i) = ($_[0], &{$_[0]}); rw {&$f eq $i}}
BEGIN {eval sprintf 'sub re%s() {re {%s}}', $_, $_ for 'a'..'q'}

Streaming aggregations.
These functions are like the ones above, but designed to work in constant
space:

| se<column>: streaming reduce while column is equal
  sr: streaming reduce all data

sub se(&$@) {my ($f, $e, @xs) = @_; my $k = &$e;
             @xs = &$f(@xs), rl while defined and &$e eq $k;
             push @q, $_ if defined; @xs}
BEGIN {ceval sprintf 'sub se%s(&@) {my ($f, @xs) = @_; se {&$f(@_)} \&%s, @xs}',
                     $_, $_ for 'a'..'q'}

sub sr(&@) {my ($f, @xs) = @_; @xs = &$f(@xs), rl while defined; @xs}
68 core/pl/reducers.pm.sdoc
Compound reductions.
Suppose you want to calculate, in parallel, the sum of one column and the mean
of another. You can't use two separate `sr` calls since the first one will
force the whole stream. Instead, you need a way to build a single compound
function that maintains the two separate state elements. That's what `rc`
(reduce compound) is all about.

In fast languages we could probably get away with some nice combinatory stuff
here, but this is performance-critical and Perl isn't fast. So I'm making some
epic use of codegen and `eval` to help Perl be all it can be. We end up
compiling into a single function body for a `cr` call, which is then mapped
through a finalizer to eliminate intermediate states.

sub rsum($)  {+{reduce => gen "%1 + ($_[0])",
                init   => [0],
                end    => gen '%1'}}

sub rmean($) {+{reduce => gen "%1 + ($_[0]), %2 + 1",
                init   => [0, 0],
                end    => gen '%1 / (%2 || 1)'}}

sub rmin($)  {+{reduce => gen "defined %1 ? min %1, ($_[0]) : ($_[0])",
                init   => [undef],
                end    => gen '%1'}}

sub rmax($)  {+{reduce => gen "defined %1 ? max %1, ($_[0]) : ($_[0])",
                init   => [undef],
                end    => gen '%1'}}

sub rarr($)  {+{reduce => gen "[\@{%1}, ($_[0])]",
                init   => [[]],
                end    => gen '%1'}}

sub rfn($$)  {+{reduce => gen $_[0],
                init   => [@_[1..$#_]],
                end    => gen join ', ', map "%$_", 1..$#_}}

sub compound_reducer(@) {
  local $_;
  my $slots = 0;
  my @indexes = map {my $n = @{$$_{init}}; $slots += $n; $slots - $n} @_;
  my @mapping = map {my $i = $_;
                     [map {;$_ => sprintf "\$_[%d]", $indexes[$i] + $_ - 1}
                          1..@{$_[$i]{init}}]} 0..$#_;
  (init   => [map @{$$_{init}}, @_],
   reduce => join(', ', map $_[$_]{reduce}->(@{$mapping[$_]}), 0..$#_),
   end    => join(', ', map $_[$_]{end}->(@{$mapping[$_]}),    0..$#_));
}

Reduce compound function.
Executes a compound reduction using the specified stream reducer function.
Typical usage would be like this:

| ($sum, $mean) = rc \&sea, fsum A, fmean B;

sub rc {
  my ($f, @rs) = @_;
  my %c        = compound_reducer @rs;
  my $reduce   = eval "sub{\n($c{reduce})\n}" or die "sc: '$c{reduce}': $@";
  my $end      = eval "sub{\n($c{end})\n}"    or die "sc: '$c{end}': $@";
  &$end(&$f($reduce, @{$c{init}}));
}

Just like for `se` functions, we define shorthands such as `rca ...` = `rc
\&sea, ...`.

c
BEGIN {ceval sprintf 'sub rc%s {rc \&se%s, @_}', $_, $_ for 'a'..'q'}
50 core/pl/pl.pl.sdoc
Perl wrapper.
Defines the `p` operator, which can be modified in a few different ways to do
different things. By default it functions as a one-in, many-out row
transformer.

use constant perl_mapgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3' or die "ni: failed to open fd 3: $!";
  sub row {
    %body
  }
  while (defined rl) {
    %each
  }
};

sub perl_prefix() {join "\n", @self{qw| core/pl/util.pm
                                        core/pl/math.pm
                                        core/pl/stream.pm
                                        core/gen/gen.pl
                                        core/pl/reducers.pm |}}

c
BEGIN {
  defoperator perl_code => q{
    my ($body, $each) = @_;
    move_fd 0, 3;
    safewrite siproc {exec 'perl', '-'},
              perl_mapgen->(prefix => perl_prefix,
                            body   => $body,
                            each   => $each);
  };
}

sub perl_mapper($)  {perl_code_op $_[0], 'pr for row'}
sub perl_grepper($) {perl_code_op $_[0], 'pr if row'}
sub perl_facet($)   {perl_code_op $_[0], 'pr row . "\t$_"'}

our @perl_alt = pmap q{perl_mapper $_}, pplcode;

defshort '/p', paltr @perl_alt;

sub defperlalt($) {unshift @perl_alt, $_[0]}

defrowalt pmap q{perl_grepper $_}, pn 1, prx 'p', pplcode;

deffacetalt 'p', pmap q{[perl_facet $$_[0],
                         row_sort_op('-k1b,1'),
                         perl_mapper $$_[1]]}, pseq pplcode, pplcode;
2 core/lisp/lib
prefix.lisp
lisp.pl.sdoc
163 core/lisp/prefix.lisp
(declaim (optimize (speed 3) (safety 0)))
(setf *read-default-float-format* 'double-float)

;;; utility functions from wu-sugar

(defun str (&rest values)
  (with-output-to-string (s)
    (dolist (val values)
      (princ val s))))

(defun join (separator &rest strings)
  "Concatenates STRINGS, joining them by SEPARATOR."
  (when (characterp separator)
    (setf separator (string separator)))
  (if strings
      (reduce (lambda (a b) (str a separator b)) strings)
      ""))

(defun split (string &rest delimiter-chars)
  "Splits STRING by one or more delimiter characters, returning a list."
  (let ((current-pos 0)
	result)
    (loop
       (push (subseq string 
		     current-pos 
		     (setf current-pos (position-if (lambda (c) (member c delimiter-chars)) 
						    string 
						    :start current-pos)))
	     result)
       (unless current-pos (return))
       (incf current-pos))
    (nreverse result)))

(defun starts-with-p (seq subseq)
  (let ((subseq-len (length subseq))) 
    (if (<= subseq-len (length seq)) 
	(search subseq seq :end2 subseq-len)
	nil)))

(defun ends-with-p (seq subseq)
  (let ((seq-len (length seq))
	(subseq-len (length subseq))) 
    (if (<= (length subseq) (length seq)) 
	(search subseq seq :from-end t :start2 (- seq-len subseq-len))
	nil)))

(defun partial (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))


(defvar *l*)
(defvar *cols*)

(defun read-col (text)
  (when text
    (let* ((*read-eval* nil)
           (r (read-from-string text)))
      (etypecase r
        (symbol text)
        (number r)))))

(defun %r (&rest values)
  (apply #'join #\Tab values))

(defmacro r (&rest values)
  `(multiple-value-call #'%r ,@values))

(defvar *line-q* (make-array 10 :adjustable t :fill-pointer 0))

(declaim (inline next-line))
(defun next-line ()
  (or (when (plusp (length *line-q*))
        (vector-pop *line-q*))
      (read-line *standard-input* nil)))

(defun %sr (&rest reducefn_inputfn_current)
  (loop for l = (next-line) while l do
       (let ((*cols* (split l #\Tab)))
         (loop for (reducefn inputfn current) in reducefn_inputfn_current
              for ric in reducefn_inputfn_current do
              (setf (third ric)
                    (funcall reducefn current (funcall inputfn))))))
  (apply #'values (mapcar #'third reducefn_inputfn_current)))

(defmacro sr (&rest reducer_input_initial)
  `(let* ((bindings (list ,@(loop for (reducer input initial) in reducer_input_initial append
                                 (list reducer `(lambda () ,input) initial))))
          (fixed-bindings (loop for (reducefn inputfn initial) on bindings by #'cdddr collect
                               (list reducefn inputfn (if initial                                                          
                                                          (funcall reducefn initial (funcall inputfn))
                                                          (funcall inputfn))))))
     (apply #'%sr fixed-bindings)))

(defun %se (reducefn inputfn continuefn current)
  (loop for l = (next-line) while l do
       (let ((*cols* (split l #\Tab)))
         (if (funcall continuefn)
             (setf current (funcall reducefn current (funcall inputfn)))
             (progn
               (vector-push-extend l *line-q*)
               (return)))))
  current)

(defmacro se (reducer input partition &optional (initial nil initial-supplied-p))
  `(let* ((inputfn (lambda () ,input))
          (partfn (lambda () ,partition))
          (first-partfn-result (funcall partfn))
          (continuefn (lambda () (equal first-partfn-result (funcall partfn))))
          (reducefn ,reducer))
     (%se reducefn inputfn continuefn ,(if initial-supplied-p               
                                          `(funcall reducefn ,initial (funcall inputfn))
                                          `(funcall inputfn)))))

(defun %rw (continue-fn)
  (let ((result (list *l*)))
    (loop for *l* = (next-line) while *l* do
         (let ((*cols* (split *l* #\Tab)))
           (if (funcall continue-fn)
               (push *l* result)
               (progn
                 (vector-push-extend *l* *line-q*)
                 (return)))))
    (apply #'values (nreverse result))))

(defmacro rw (condition)
  `(let ((continue-fn (lambda () ,condition)))
     (%rw continue-fn)))

(defmacro ru (condition)
  `(let ((continue-fn (lambda () (not ,condition))))
     (%rw continue-fn)))

;; TODO: convert row strings to numbers
;; (defun %mean (&rest values)
;;   (/ (apply #'+ values)
;;      (length values)))

;; (defmacro mean (form)
;;   `(multiple-value-call #'%mean ,form))

(defun output-rows (&rest rows)
  (dolist (row rows)
    (princ row)
    (terpri)))

(defmacro with-ni-env (filter-p &body body)
  (let ((l-var '*l* ;;(gensym "L")
         ))
    `(let ((*standard-input* (sb-sys:make-fd-stream 3)))
       (symbol-macrolet ,(loop for colname in '(a b c d e f g h i j k l m n o p q)
                            for index from 0
                            collect (list colname `(read-col (nth ,index *cols*))))     
         (loop for ,l-var = (next-line) while ,l-var do
              (let ((*cols* (split ,l-var #\Tab)))
                ,(if filter-p
                     `(when (progn ,@body)
                        (write-string ,l-var)
                        (terpri))
                     `(progn
                        ,@(loop for form in body collect
                               `(multiple-value-call #'output-rows ,form)
                        )))))))))
52 core/lisp/lisp.pl.sdoc
Lisp backend.
A super simple SBCL operator. The first thing we want to do is to define the
code template that we send to Lisp via stdin (using a heredoc). So ni ends up
generating a pipeline element like this:

| ... | sbcl --noinform --script 3<&0 <<'EOF' | ...
        (prefix lisp code)
        (line mapping code)
        EOF

use constant lisp_mapgen => gen q{
  %prefix
  (with-ni-env nil
    %body)
};

use constant lisp_grepgen => gen q{
  %prefix
  (with-ni-env t
    %body)
};

Now we specify which files get loaded into the prefix. The ni build script
preprocesses files with an sdoc extension (though you're not required to use
it; normal files are passed straight through), and file paths become keys in
the %self hash after having the src/ prefix replaced with core/.

sub lisp_prefix() {join "\n", @self{qw| core/lisp/prefix.lisp |}}

Finally we define the toplevel operator. 'root' is the operator context, 'L' is
the operator name, and pmap {...} mrc '...' is the parsing expression that
consumes the operator's arguments (in this case a single argument of just some
Lisp code) and returns a shell command. (See src/sh.pl.sdoc for details about
how shell commands are represented.)

use constant lispcode => prc '.*[^]]+';

defoperator lisp_code => q{
  my $code = $_[0];
  move_fd 0, 3;
  safewrite siproc {exec qw| sbcl --noinform --noprint --eval |,
                         '(load *standard-input* :verbose nil :print nil)'},
            $code;
};

defshort '/l', pmap q{lisp_code_op lisp_mapgen->(prefix => lisp_prefix,
                                                 body   => $_)},
               lispcode;

defrowalt pmap q{lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
                                             body   => $_)},
          pn 1, prx 'l', lispcode;
12 doc/lib
col.md
examples.md
extend.md
facet.md
libraries.md
lisp.md
options.md
perl.md
row.md
sql.md
stream.md
tutorial.md
143 doc/col.md
# Column operations
ni models incoming data as a tab-delimited spreadsheet and provides some
operators that allow you to manipulate the columns in a stream accordingly. The
two important ones are `f[columns...]` to rearrange columns, and `F[delimiter]`
to create new ones.

ni always refers to columns using letters: `A` to `Z`.

## Reordering
First let's generate some data, in this case an 8x8 multiplication table:

```bash
$ ni n8p'r map a*$_, 1..8' > mult-table
$ ni mult-table
1	2	3	4	5	6	7	8
2	4	6	8	10	12	14	16
3	6	9	12	15	18	21	24
4	8	12	16	20	24	28	32
5	10	15	20	25	30	35	40
6	12	18	24	30	36	42	48
7	14	21	28	35	42	49	56
8	16	24	32	40	48	56	64
```

The `f` operator takes a multi-column spec and reorders, duplicates, or deletes
columns accordingly.

```bash
$ ni mult-table fA      # the first column
1
2
3
4
5
6
7
8
$ ni mult-table fDC     # fourth, then third column
4	3
8	6
12	9
16	12
20	15
24	18
28	21
32	24
$ ni mult-table fAA     # first column, duplicated
1	1
2	2
3	3
4	4
5	5
6	6
7	7
8	8
```

You can also choose "the rest of the columns" using `.` within your column
spec. This selects everything to the right of the rightmost column you've
mentioned.

```bash
$ ni mult-table fDA.    # fourth, first, "and the rest (i.e. 5-8)"
4	1	5	6	7	8
8	2	10	12	14	16
12	3	15	18	21	24
16	4	20	24	28	32
20	5	25	30	35	40
24	6	30	36	42	48
28	7	35	42	49	56
32	8	40	48	56	64
$ ni mult-table fBA.    # an easy way to swap first two columns
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
6	3	9	12	15	18	21	24
8	4	12	16	20	24	28	32
10	5	15	20	25	30	35	40
12	6	18	24	30	36	42	48
14	7	21	28	35	42	49	56
16	8	24	32	40	48	56	64
```

## Splitting
The `F` operator gives you a way to convert non-tab-delimited data into TSV.
For example, if you're parsing `/etc/passwd`, you'd turn colons into tabs
first.

`F` has the following uses:

- `F:<char>`: split on character
- `F/regex/`: split on occurrences of regex. If present, the first capture
  group will be included before a tab is appended to a field.
- `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
- `FC`: split on commas
- `FS`: split on runs of horizontal whitespace
- `FW`: split on runs of non-word characters
- `FP`: split on pipe symbols

Note that `FC` isn't a proper CSV parser; it just transliterates all commas
into tabs.

### Examples
```bash
$ ni /etc/passwd r2F::          # F: followed by :, which is the split char
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
```

```bash
$ ni //ni r3                            # some data
#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
```

```bash
$ ni //ni r3F/\\//                      # split on forward slashes
#!	usr	bin	env perl
$ni::self{license} = <<'_';
ni: https:		github.com	spencertipping	ni
```

```bash
$ ni //ni r3FW                          # split on non-words
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
```

```bash
$ ni //ni r3FS                          # split on whitespace
#!/usr/bin/env	perl
$ni::self{license}	=	<<'_';
ni:	https://github.com/spencertipping/ni
```

```bash
$ ni //ni r3Fm'/\/\w+/'                 # words beginning with a slash
/usr	/bin	/env

/github	/spencertipping	/ni
```
4 doc/examples.md
# General ni examples
Things that might give you ideas to be more dangerous.

**TODO**
17 doc/extend.md
# Extending ni
You can extend ni by writing a library. For example, suppose we want a new
operator `N` that counts lines by shelling out to `wc -l`:

```bash
$ mkdir my-library
$ echo my-lib.pl > my-library/lib
$ cat > my-library/my-lib.pl <<'EOF'
defoperator count_lines => q{exec 'wc', '-l'};
defshort '/N', pmap q{count_lines_op}, pnone;
EOF
$ ni --lib my-library n100N
100
```

Most ni extensions are about defining a new operator, which involves extending
ni's command-line grammar.
69 doc/facet.md
# Faceting
ni supports an operator that facets rows: that is, it groups them by some
function and aggregates within each group. How this is implemented depends on
the backend; map/reduce workflows do this automatically with the shuffle step,
whereas multiple POSIX tools are required. The facet operator handles this
appropriately in each context.

The basic format of the facet operator is like this:

```sh
$ ni ... @<language><key-expr> <reducer>
```

For example, here's a Perl facet to implement word count:

```bash
$ ni @pa 'r a, rca rsum 1' <<'EOF'
foo
bar
foo
bif
EOF
bar	1
bif	1
foo	2
```

Structurally, here's what's going on:

- `@p`: facet with Perl code
- `a`: use the first column value as the faceting key (this is Perl code)
- `r ...`: make a TSV row from an array of values...
  - `a`: ...the first of which is the faceting key (which is always `a` because
    the facet column is prepended to the data)
  - `rca ...`: return an array of streaming reductions within each facet on
    column A
    - `rsum 1`: the first (and only) of which is a sum of the constant 1 for
      each reduced item

## Perl
See [perl.md](perl.md) (`ni //help/perl`) for information about the libraries
ni provides for Perl code.

To get a list of users faceted by login shell:

```bash
$ ni /etc/passwd F::@pg 'r a, @{rca rarr B}'
/bin/bash	root
/bin/false	syslog
/bin/sh	backup	bin	daemon	games	gnats	irc	libuuid	list	lp	mail	man	news	nobody	proxy	sys	uucp	www-data
/bin/sync	sync
```

Here, `rarr B` means "collect faceted values into an array reference", and in
this case the value is column B. (`B` is in uppercase because `rarr` takes a
string argument rather than a code block; this is for performance reasons, and
[perl.md](perl.md) (`ni //help/perl`) discusses the details behind it.)

A lot of faceting workflows can be more easily expressed as a sort/reduce in
code, particularly if you're not computing a new value for the key. For
example, the above query can be written more concisely this way:

```bash
$ ni /etc/passwd F::gGp'r g, a_ reg'
/bin/bash	root
/bin/false	syslog
/bin/sh	backup	bin	daemon	games	gnats	irc	libuuid	list	lp	mail	man	news	nobody	proxy	sys	uucp	www-data
/bin/sync	sync
```
28 doc/libraries.md
# Libraries
A library is just a directory with a `lib` file in it. `lib` lists the names of
files to be included within that library, one per line, and in doing so
specifies the order of inclusion (which sometimes matters if you're defining
stuff in Perl). Most libraries will include at least one Perl file, which ni
evaluates in the `ni` package. ni will assume that any file ending in `.pl`
should be evaluated when the library is loaded. (Importantly, libraries are
loaded _before_ the main CLI arguments are parsed, which is why it's possible
to add new syntax.)

ni has two library-loading options:

- `ni --lib X ...`: load the `X` library before executing the pipeline
- `ni --extend X [...]`: load the `X` library and rewrite yourself to include
  it in the future (and then execute the pipeline if you got one)

`--extend` is useful in a scripted context when you're building a site-specific
ni executable, e.g.:

```sh
#!/bin/bash
[[ -x /bin/ni ]] || get_ni_from_somewhere
ni --extend site-lib1           # this modifies ni in place
ni --extend site-lib2
```

`--extend` is idempotent, and you can use it to install a newer version of an
already-included library.
88 doc/lisp.md
# Common Lisp driver
ni supports Common Lisp via SBCL, which is available using the `l` and `L`
operators. For example:

```bash
$ ni n4l'(+ a 2)'
3
4
5
6
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `(r ...)` is a function that takes a list of
values and prints a tab-delimited row. For example:

```bash
$ ni n4l'(r a (1+ a))'                  # generate two columns
1	2
2	3
3	4
4	5
$ ni n4l'(r a (1+ a))' l'(r (+ a b))'   # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `p'code'` operator; otherwise ni
will assume that everything following your quoted code is also Perl.

It is possible to omit `r` altogether; then you're returning one or more
values, each of which will become a row of output:

```bash
$ ni n2l'a (+ a 100)'                   # return without "r"
1
101
2
102
```

## Streaming lookahead
This is implemented in terms of reducers, and gives you the ability to reduce
arbitrarily many rows in constant space. There are two parts to this. First,
the streaming reduce functions `se` and `sr`; and second, compound reducers
(very useful, and explained in the next section).

### `sr`
Reduces the entire data stream:

```
sr (reducer value [initial-value])* => reduced-value*
```

For example, to sum arbitrarily many numbers in constant space:

```bash
$ ni n10000l"(sr ('+ a))"
50005000
```

Or to reduce multiple columns into a row:

```bash
$ ni n4fAA l"(r (sr ('+ a) ('* b)))"
10	24
```

### `se`
Reduces over a contiguous group of rows for which the partition function
remains equal. (Mnemonic is "stream while equal".)

```
se function value-form partition-form [initial-value] => reduced-value
```

For example, to naively get a comma-delimited list of users by login shell:

```bash
$ ni /etc/passwd F::gG l"(r g (se (partial #'join #\,) a g))"
/bin/bash	root
/bin/false	syslog
/bin/sh	backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	sync
```
2 doc/options.md
# Complete ni operator listing
## 
324 doc/perl.md
# Perl interface
**NOTE:** This documentation covers ni's Perl data transformer, not the
internal libraries you use to extend ni. For the latter, see
[extend.md](extend.md) (`ni //help/extend`).

ni provides the `p` operator to execute a Perl line processor on the current
data stream. For example:

```bash
$ ni n5p'a * a'                 # square some numbers
1
4
9
16
25
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `r(...)` is a function that takes a list of
values and prints a tab-delimited row. For example:

```bash
$ ni n4p'r a, a + 1'                    # generate two columns
1	2
2	3
3	4
4	5
$ ni n4p'r a, a + 1' p'r a + b'         # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `p'code'` operator; otherwise ni
will assume that everything following your quoted code is also Perl.

Internally, `a`, `b`, etc are defined like this, which I explain below:

```pl
sub a() {F_ 0}
sub b() {F_ 1}
...
```

### `F_`: the array of fields
The Perl code given to `p` is invoked on each line of input, which is stored
both in `$l` and, for convenience, in `$_`. ni doesn't split `$l` into fields
until you call `F_`, at which point the split happens and the fields are
cached for efficiency.

`F_(...)` takes one or more column indexes (as zero-based integers) and returns
the field values. If you don't pass in anything, it returns all of the fields
for the line. For example:

```bash
$ ni /etc/passwd F::r3
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3p'r F_ 0..3'
root	x	0	0
daemon	x	1	1
bin	x	2	2
$ ni /etc/passwd F::r3p'r scalar F_'            # number of fields
7
7
7
```

### `r`, multiple rows, and return values
`p` executes your Perl code using essentially this template:

```pl
sub row {
  # your code goes here
}
while (<STDIN>) {
  $l = $_;
  print "$_\n" for row();
}
```

Counterintuitively, `r(...)` doesn't return a value; it returns an empty list
and side-effectfully prints a row. It works this way because that allows you to
generate arbitrarily many rows in constant space.

This design is also what makes it possible to omit `r` altogether; then you're
returning one or more values, each of which will become a row of output:

```bash
$ ni n2p'a, a + 100'                    # return without "r"
1
101
2
102
$ ni n2p'r a, a + 100'                  # use "r" for side effect, return ()
1	101
2	102
$ ni n3p'r $_ for 1..a; ()'             # use r imperatively, explicit return
1
1
2
1
2
3
$ ni n3p'r $_ for 1..a'                 # use r imperatively, implicit return
1

1
2

1
2
3

```

The last example has blank lines because Perl's `for` construct returns a
single empty scalar. You can suppress any implicit returns using `;()` at the
end of your mapper code.

Whether you use `r` or implicit returns, ni will remove newlines from every
string you give it. This makes it easier to use `qx` without any filtering.

## Buffered readahead
`p` code can read forwards in the input stream. This is trivially possible by
calling `rl()` ("read line"), which destructively advances to the next line and
returns it; but more likely you'd use one of these instead:

- `@lines = rw {condition}`: read lines while a condition is met
- `@lines = ru {condition}`: read lines until a condition is met
- `@lines = re {a + b}`: read lines while `a + b` is equal

**NOTE:** `rw`, `ru`, and `re` are destructive operators in that they consume
lines and destroy the context for `a`, `b`, etc.

```bash
$ ni n10p'r ru {a%4 == 0}'              # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

The line array returned by `ru` is just an array of flat, tab-delimited strings
(verbatim lines from standard input), but you can extract fields using the
column-accessor functions `a_`, `b_`, etc:

```bash
$ ni n10p'r map a*$_, 1..10' | tee mult-table
1	2	3	4	5	6	7	8	9	10
2	4	6	8	10	12	14	16	18	20
3	6	9	12	15	18	21	24	27	30
4	8	12	16	20	24	28	32	36	40
5	10	15	20	25	30	35	40	45	50
6	12	18	24	30	36	42	48	54	60
7	14	21	28	35	42	49	56	63	70
8	16	24	32	40	48	56	64	72	80
9	18	27	36	45	54	63	72	81	90
10	20	30	40	50	60	70	80	90	100
$ ni mult-table p'r g_ ru {a%4 == 0}'   # extract seventh column from each line
7	14	21
28	35	42	49
56	63	70
```

`a_` etc are defined like this:

```pl
sub a_ {local $_; map((split /\t/)[0], @_)}
sub b_ {local $_; map((split /\t/)[1], @_)}
...
```

## Utility functions
ni predefines some stuff you may find useful:

- `sum(@)`, `prod(@)`, `mean(@)`
- `max(@)`, `min(@)`, `maxstr(@)`, `minstr(@)`, `argmax(&@)`, `argmin(&@)`
- `any(&@)`, `all(&@)`, `uniq(@)`, `%f = %{freqs(@)}`
- `reduce {f} $init, @xs`
- `reductions {f} $init, @xs`
- `cart([a1, a2, a3], [b1, b2, b3], ...) = [a1, b1, ...], [a1, b2, ...], ...`:
  Cartesian product

```bash
$ ni n100p'sum rw {1}'
5050
$ ni n10p'prod rw {1}'
3628800
$ ni n100p'mean rw {1}'
50.5
```

## Streaming lookahead
This is implemented in terms of reducers, and gives you the ability to reduce
arbitrarily many rows in constant space. There are two parts to this. First,
the streaming reduce functions `se` and `sr`; and second, compound reducers
(very useful, and explained in the next section).

### `sr`
Reduces the entire data stream:

```pl
($x, $y, ...) = sr {reducer} $x0, $y0, ...
```

For example, to sum arbitrarily many numbers in constant space:

```bash
$ ni n10000p'sr {$_[0] + a} 0'
50005000
```

### `se`
Reduces over a contiguous group of rows for which the partition function
remains equal. (Mnemonic is "stream while equal".)

```pl
@final_state = se {reducer} \&partition_fn, @init_state
```

For example, to naively get a comma-delimited list of users by login shell:

```bash
$ ni /etc/passwd F::gGp'r g, se {"$_[0]," . a} \&g, ""'
/bin/bash	,root
/bin/false	,syslog
/bin/sh	,backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	,sync
```

`se` has shorthands for the first 17 columns: `sea`, `seb`, ..., `seq`.

## Compound reducers
If you want to do something like calculating the sum of one column, the average
of another one, and the min/max of a third, you'll end up writing some awkward
reducer code. ni provides a facility called compound reduction to deal with
this. For example, here's the hard way:

```bash
$ ni n100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[2], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
5050	50.5	1	100
```

And here's the easy way, using `rc`:

```bash
$ ni n100p'r rc \&sr, rsum A, rmean A, rmin A, rmax A'
5050	50.5	1	100
```

### What's going on here
`rsum`, `rmean`, etc, return compound reducers, which are hash references with
keys that indicate (1) the initial state, (2) the reduction function **as a
string**, and (3) the finalizer (which is why `rmean` can return a single
number despite its intermediate state being the separated sum and number).

Compound reducers are compiled functions, which means their arguments are
expressed as strings representing quoted code. This is why we use `A` rather
than `a` in the example above: `A` evaluates to the string `'a'`, which is
spliced into a function body along with the other reducer expressions and
compiled. The result is a very efficient reducer function that ends up looking
like this:

```pl
sub {($_[0] + a, $_[1] + a, $_[2] + 1, min($_[3], a), max($_[4], a))}
```

`rc` hands this function to `sr` and then uses the finalizer functions to
return individual values, one per initial reducer.

### Custom compound reducers
You can easily create your own compound reducer using `rfn`. For example, let's
count the frequency of each lowercase letter in a file:

```bash
$ ni /etc/passwd FWpsplit// r/[a-z]/ \
     p'my %freqs = %{rc \&sr, rfn q{ ++${%1}{a()} && %1 }, {}};
       map r($_, $freqs{$_}), sort keys %freqs'
a	39
b	36
c	14
d	13
e	17
f	1
g	11
h	20
i	46
k	3
l	19
m	14
n	50
o	25
p	15
r	24
s	51
t	15
u	17
v	12
w	12
x	23
y	12
```

Here's what's going on.

- `FW`: split into words
- `psplit//`: the same as `p'split //'`: split the line into individual chars,
  returned as an array so we end up with each on its own output line
- `r/[a-z]/`: keep all lowercase letters from those rows
- `p'...'`: apply perl to rows
  - `rfn q{ ++${%1}{a()} && %1 }, {}`: a reducer whose initial state is the
    empty hash `{}`, and whose reducer function does the following:
    - `q{ ++${%1}{a()} ...}`: `%1` is the reduced quantity, and `a()` is the
      first column of the line. We're incrementing the entry within the `%{%1}`
      hash.
    - `&& %1` is a way to return the hash reference as the reduced value (since
      we're modifying it in place). We need to do this without using a comma
      because each reducer function is evaluated in list context.
181 doc/row.md
# Row operations
These are fairly well-optimized operations that operate on rows as units, which
basically means that ni can just scan for newlines and doesn't have to parse
anything else. They include:

- Take first/last N
- Take uniform-random or periodic sample
- Rows matching regex
- Rows satisfying code
- Reorder rows
- Count identical rows

## First/last
Shorthands for UNIX `head` and `tail`.

```bash
$ ni n10r3                      # take first 3
1
2
3
$ ni n10r+3                     # take last 3
8
9
10
$ ni n10r-7                     # drop first 7
8
9
10
```

## Sampling
```bash
$ ni n10000rx4000               # take every 4000th row
4000
8000
$ ni n10000r.0002               # sample uniformly, P(row) = 0.0002
1
6823
8921
9509
```

It's worth noting that uniform sampling, though random, is also deterministic;
by default ni seeds the RNG with 42 every time (though you can change this by
exporting `NI_SEED`). ni also uses an optimized Poisson process to sample rows,
which minimizes calls to `rand()`.

## Regex matching
```bash
$ ni n10000r/[42]000$/
2000
4000
$ ni n1000r/[^1]$/r3
2
3
4
```

These regexes are evaluated by Perl, which is likely to be faster than `grep`
for nontrivial patterns.

## Code
`rp` means "select rows for which this Perl expression returns true".

```bash
$ ni n10000rp'$_ % 100 == 42' r3
42
142
242
```

The expression has access to column accessors and everything else described in
[perl.md](perl.md) (`ni //help/perl`).

Note that whitespace is always required after quoted code.

**TODO:** other languages

## Sorting
ni has four operators that shell out to the UNIX sort command. Two are
alpha-sorts:

```bash
$ ni n100n10gr4                 # g = 'group'
1
1
10
10
$ ni n100n100Gr4                # G = 'group uniq'
1
10
100
11
```

The idea behind `g` as `group` is that this is what you do prior to an
aggregation; i.e. to group related rows together so you can stream into a
reducer (covered in more detail in [facet.md](facet.md) (`ni //help/facet`)).

ni also has two `order` operators that sort numerically:

```bash
$ ni n100or3                    # o = 'order': sort numeric ascending
1
2
3
$ ni n100Or3                    # O = 'reverse order'
100
99
98
```

### Specifying sort columns
When used without options, the sort operators sort by a whole row; but you can
append one or more column specifications to change this. I'll generate some
multicolumn data to demonstrate this (see [perl.md](perl.md) (`ni //help/perl`)
for an explanation of the `p` operator).

```bash
$ ni n100p'r a, sin(a), log(a)' > data          # generate multicolumn data
$ ni data r4
1	0.841470984807897	0
2	0.909297426825682	0.693147180559945
3	0.141120008059867	1.09861228866811
4	-0.756802495307928	1.38629436111989
```

Now we can sort by the second column, which ni refers to as `B` (in general, ni
uses spreadsheet notation: columns are letters, rows are numbers):

```bash
$ ni data oB r4
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
```

This is an example of required whitespace between `oB` and `r4`; columns can be
suffixed with `g`, `n`, and/or `r` modifiers to modify how they are sorted
(these behave as described for `sort`'s `-k` option), and ni prefers this
interpretation:

```bash
$ ni data oBr r4                # r suffix = reverse sort
33	0.999911860107267	3.49650756146648
77	0.999520158580731	4.34380542185368
58	0.992872648084537	4.06044301054642
14	0.99060735569487	2.63905732961526
```

## Counting
ni gives you the `c` and `C` operators to count runs of identical rows (just
like `uniq -c`). The `C` operator first sorts the input, whereas `c` is a
streaming count.

```bash
$ ni //ni FWpF_ r500 > word-list
$ ni word-list cr10             # unsorted count
1	usr
1	bin
1	env
1	perl
1	
1	ni
1	self
1	license
1	_
1	ni
$ ni word-list Cr10             # sort first to group words
15	0
8	1
8	2
1	2016
1	3
1	30
1	45
1	5
1	A
2	ACTION
```
29 doc/sql.md
# SQL interop
ni defines a parsing context that translates command-line syntax into SQL
queries. We'll need to define a SQL connection profile in order to use it:

```bash
$ mkdir sqlite-profile
$ echo sqlite.pl > sqlite-profile/lib
$ cat > sqlite-profile/sqlite.pl <<'EOF'
$sql_profiles{S} = pmap {sh "sqlite", "-separator", "\t", $$_[0], $$_[1]}
                        seq mrc '^.*', $sql_query;
EOF
```

Now we can create a test database and use this library to access it.

```bash
$ sqlite test.db <<'EOF'
CREATE TABLE foo(x int, y int);
INSERT INTO foo(x, y) VALUES (1, 2);
INSERT INTO foo(x, y) VALUES (3, 4);
INSERT INTO foo(x, y) VALUES (5, 6);
EOF
$ ni --lib sqlite-profile QStest.db foo[wx=3]
3	4
$ ni --lib sqlite-profile QStest.db foo[Ox]
5	6
3	4
1	2
```
251 doc/stream.md
# Stream operations
```bash
$ echo test > foo
$ ni foo
test
$ ni foo foo
test
test
```

ni transparently decompresses common formats, regardless of file extension:

```bash
$ echo test | gzip > fooz
$ ni fooz
test
$ cat fooz | ni
test
```

## Data sources
In addition to files, ni can generate data in a few ways:

```bash
$ ni $:'seq 4'                  # shell command stdout
1
2
3
4
$ ni n4                         # integer generator
1
2
3
4
$ ni n04                        # integer generator, zero-based
0
1
2
3
$ ni id:foo                     # literal text
foo
```

## Transformation
ni can stream data through a shell process, which is often shorter than
shelling out separately:

```bash
$ ni n3 | sort
1
2
3
$ ni n3 $=sort                  # $= filters through a command
1
2
3
$ ni n3 $='sort -r'
3
2
1
```

And, of course, ni has shorthands for doing all of the above:

```bash
$ ni n3 g       # g = sort
1
2
3
$ ni n3g        # no need for whitespace
1
2
3
$ ni n3gAr      # reverse-sort by first field
3
2
1
$ ni n3O        # NOTE: capital O, not zero; more typical reverse numeric sort
3
2
1
```

Notice that ni typically doesn't require whitespace between commands. The only
case where it does is when the parse would be ambiguous without it (and
figuring out when this happens requires some knowledge about how the shell
quotes things, since ni sees post-quoted arguments). ni will complain if it
can't parse something, though.

See [row.md](row.md) (`ni //help/row`) for details about row-reordering
operators like sorting.

## Writing files
You can write a file in two ways. One is, of course, using shell redirection:

```bash
$ ni n3 >file                   # nothing goes to the terminal
$ ni file
1
2
3
```

The other way is to use one of ni's two file-writing operators:

```bash
$ ni n3 \>file2                 # writes the filename to the terminal
file2
$ ni file2
1
2
3
$ ni n3 \>%file3                # duplicates output
1
2
3
$ ni file3
1
2
3
```

The `<` operator inverts `>` by reading files; it's conceptually equivalent to
`xargs cat`:

```bash
$ ni n4 \>file3 \<
1
2
3
4
```

If you want to write a compressed file, you can use the `Z` operator:

```bash
$ ni n3Z >file3.gz
$ zcat file3.gz
1
2
3
```

`Z` lets you specify which compressor you want to use; for example:

```bash
$ ni id:gzip Z | gzip -dc               # gzip by default
gzip
$ ni id:gzip Zg | gzip -dc              # explicitly specify
gzip
$ ni id:gzip Zg9 | gzip -dc             # specify compression level
gzip
$ ni id:xz Zx | xz -dc
xz
$ ni id:lzo Zo | lzop -dc
lzo
$ ni id:bzip2 Zb | bzip2 -dc
bzip2
```

```sh
# this one isn't a unit test because not all test docker images have a
# straightforward LZ4 install (some are too old)
$ ni id:lz4 Z4 | lz4 -dc
lz4
```

ni also provides a universal decompression operator `ZD`, though you'll rarely
need it because any external data will be decoded automatically. `ZD` has no
effect if the data isn't compressed.

```bash
$ ni n4 Z ZD
1
2
3
4
$ ni n4 ZD
1
2
3
4
```

Finally, ni provides the ultimate lossy compressor, `ZN`, which achieves 100%
compression by writing data to `/dev/null`:

```bash
$ ni n4 ZN | wc -c
0
```

## Checkpoints
Checkpoints let you cache intermediate outputs in a pipeline. This can avoid
expensive recomputation. For example, let's expensively get some numbers:

```bash
$ ni n1000000gr4
1
10
100
1000
```

If we wanted to iterate on the pipeline from this point onwards, we could do
this quickly by checkpointing the result:

```bash
$ ni :numbers[n1000000gr4]
1
10
100
1000
```

Now this data will be reused if we rerun it:

```bash
$ ni :numbers[n1000000gr4]O
1000
100
10
1
```

ni isn't rerunning the process at all; we can see this by modifying the
checkpoint file:

```bash
$ echo 'checkpointed' > numbers
$ ni :numbers[n1000000gr4]O
checkpointed
```

You can write compressed data into a checkpoint. The checkpointing operator
itself will decode any compressed data you feed into it; for example:

```bash
$ ni :biglist[n100000Z]r5
1
2
3
4
5
$ ni :biglist[n100000Z]r5
1
2
3
4
5
```
31 doc/tutorial.md
# ni tutorial
You can access this tutorial by running `ni //help` or `ni //help/tutorial`.

ni parses its command arguments to build and run a shell pipeline. You can get
started by using it like a version of `less` that knows how to decompress
things, but it can do a lot more; see the topics below.

```sh
$ ni data-source.gz                     # works like less
$ cat data-source.gz | ni               # same here
$ ni //help/stream                      # view a help topic
```

## Basics
- [stream.md](stream.md) (`ni //help/stream`): intro to ni grammar and data
- [row.md](row.md)       (`ni //help/row`):    row-level operators
- [col.md](col.md)       (`ni //help/col`):    column-level operators
- [perl.md](perl.md)     (`ni //help/perl`):   ni's Perl library
- [lisp.md](lisp.md)     (`ni //help/lisp`):   ni's Common Lisp library
- [ruby.md](ruby.md)     (`ni //help/ruby`):   ni's Ruby library
- [facet.md](facet.md)   (`ni //help/facet`):  the faceting operator

## Reference
- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md)       (`ni //help/extend`):    how to write a ni
  extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library
__END__
