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

sub ni::eval($;$)
{ @ni::evals{eval('__FILE__') =~ /\(eval (\d+)\)/} = ($_[1] || "anon {$_[0]}");
  my @r = eval "package ni;$_[0]";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1 - 1}/eg, die $@ if $@;
  @r }

sub ni::set
{ chomp($ni::self{$_[0]} = $_[1]);
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc$/;
  ni::eval $_[1], $_[0]                           if $_[0] =~ /\.pl$/ }

ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\s*(\d+)\s+(.*?)(\.sdoc)?$/;
$ni::data = \*DATA;
ni::eval 'exit main @ARGV', 'main';
_
die $@ if $@
__DATA__
43 ni.map.sdoc
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
lib core/deps
lib core/checkpoint
lib core/gen
lib core/json
lib core/net
lib core/col
lib core/row
lib core/cell
lib core/pl
lib core/rb
lib core/lisp
lib core/sql
lib core/python
lib core/gnuplot
lib core/http
lib core/caterwaul
lib core/jsplot
lib core/docker
lib core/hadoop
lib core/pyspark
lib doc
83 util.pl.sdoc
Utility functions.
Generally useful stuff, some of which makes up for the old versions of Perl we
need to support.

sub weval($) {my @r = eval "package ni;$_[0]"; print STDERR $@ if $@; @r}

sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub swap($$) {@_[0, 1] = @_[1, 0]}

sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}

sub rf  {open my $fh, "< $_[0]" or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]" or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

use constant noise_chars => '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ+';
sub noise_char() {substr noise_chars, rand(length noise_chars), 1}
sub noise_str($) {join '', map noise_char, 1..$_[0]}

sub abbrev($$) {length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'}

Module loading.
ni can include .pm files in its resource stream, which contain Perl code but
aren't evaluated by default. This function is used to eval them into the
current runtime.

sub load($) {ni::eval $self{$_[0]}, $_[0]}

Shell quoting/unquoting.
Useful for two cases. One is when you have a structured list and want a shell
string that will send those arguments to a command; the other is when you have
a single string and want to get the ARGV list a shell would pass to a command
(modulo dollar substitution, which we don't do).

sub shell_quote {
  local $_;
  join ' ', map /[^-A-Za-z_0-9\/:@.]/
                  ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                  : $_,
            map 'ARRAY' eq ref($_) ? shell_quote(@$_) : $_, @_;
}

sub shell_unquote_one($) {
  my ($x) = @_;
  $x =~ s/\\(["\\])/$1/g, return substr $x, 1, -1 if $x =~ /^"/;
  return                         substr $x, 1, -1 if $x =~ /^'/;
  return                         substr $x, 1     if $x =~ /^\\/;
  $x;
}

sub shell_unquote($) {
  local $_;
  my @ps = $_[0] =~ /"(?:[^"\\]+|\\[\s\S])*"|'[^']*'|\\[\s\S]|[^\s"'\\]+|\s+/g;
  my @s  = (-1, grep($ps[$_] =~ /^\s/, 0..$#ps), scalar @ps);
  map join('', map shell_unquote_one $_, @ps[$s[$_]+1 .. $s[$_+1]-1]), 0..$#s-1;
}

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
31 dev.pl.sdoc
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

sub dev_inspect_nonl($) {(my $r = dev_inspect $_[0]) =~ s/\s+/ /g; $r}

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
110 parse.pl.sdoc
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

sub pn($@)
{ my ($n, @ps) = @_;
  'ARRAY' eq ref $n ? pmap fn "[\@\$_[" . join(',', @$n) . "]]", pseq @ps
                    : pmap fn "\$\$_[$n]", pseq @ps }

sub pc($) {pn 0, $_[0], popt pempty}

Regex parsing.
Consumes the match, returning either the matched text or the first match group
you specify. Always matches from the beginning of a string.

c
BEGIN {
  defparser 'rx', '$',
    sub {my ($self, $x, @xs) = @_;
         $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()};

  defparser 'nx', '$',
    sub {my ($self, $x, @xs) = @_; $x =~ /^(?:$$self[1])/ ? () : ($x, @xs)};
}

sub prc($) {pn 0, prx qr/$_[0]/, popt pempty}
64 common.pl.sdoc
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

Basic CLI types.
Some common argument formats for various commands, sometimes transformed for
specific cases. These are documented somewhere in `doc/`.

A parsed column spec is an N-element array: [floor, cols...]. `floor` indicates
the first column that would be selected by a `.` ("the rest").

use constant neval   => pmap q{eval}, prx '=([^]=]+)';
use constant integer => palt pmap(q{int},       neval),
                             pmap(q{10 ** $_},  prx 'E(-?\d+)'),
                             pmap(q{1 << $_},   prx 'B(\d+)'),
                             pmap(q{0 + "0$_"}, prx 'x[0-9a-fA-F]+'),
                             pmap(q{0 + $_},    prx '[1-9]\d*(?:[eE]\d+)?');
use constant float   => pmap q{0 + $_}, prx '-?\d*(?:\.\d+)?(?:[eE][-+]?\d+)?';
use constant number  => palt neval, integer, float;

use constant colspec1      => pmap q{ord() - 65}, prx '[A-Z]';
use constant colspec_rest  => pmap q{-1}, prx '\.';
use constant colspec_range => pmap q{[$$_[0] .. $$_[2]]},
                              pseq colspec1, prx '-', colspec1;

use constant colspec_fixed => pmap q{[max(@$_) + 1, @$_]},
                              pmap q{[map ref() ? @$_ : $_, @$_]},
                              prep palt(colspec_range, colspec1), 1;

use constant colspec => pmap q{[max(@$_) + 1, @$_]},
                        pmap q{[map ref() ? @$_ : $_, @$_]},
                        prep palt(colspec_range, colspec1, colspec_rest), 1;

Filenames, in general.
Typically filenames won't include bracket characters, though they might include
just about everything else. Two possibilities there: if we need special stuff,
there's the `file:` prefix; otherwise we assume the non-bracket interpretation.

use constant tmpdir   => dor $ENV{TMPDIR}, '/tmp';
use constant tempfile => pmap q{tmpdir . "/ni-$<-$_"}, prx '@:(\w*)';
use constant filename => palt prc 'file:(.+)',
                              prc '\.?/(?:[^/]|$)[^]]*',
                              tempfile,
                              pcond q{-e}, prc '[^][]+';

use constant nefilename => palt filename, prc '[^][]+';
54 cli.pl.sdoc
CLI grammar.
ni's command line grammar uses some patterns on top of the parser combinator
primitives defined in parse.pl.sdoc. Probably the most important one to know
about is the long/short option dispatch, which looks like this:

| option = alt @longs, dsp %shorts

our %contexts;
our %long_names;
our %long_refs;
our %short_refs;

sub defcontext($) {
  $short_refs{$_[0]} = {};
  $long_refs{$_[0]}  = [pdspr %{$short_refs{$_[0]}}];
  $long_names{$_[0]} = ['<short dispatch>'];
  $contexts{$_[0]}   = paltr @{$long_refs{$_[0]}};
}

sub defshort($$) {
  my ($context, $dsp) = split /\//, $_[0], 2;
  die "ni: defshort cannot be used to redefine '$_[0]' (use rmshort first)"
    if exists $short_refs{$context}{$dsp};
  $short_refs{$context}{$dsp} = $_[1];
}

sub deflong($$) {
  my ($context, $name) = split /\//, $_[0], 2;
  unshift @{$long_names{$context}}, $name;
  unshift @{$long_refs{$context}}, $_[1];
}

sub rmshort($) {
  my ($context, $dsp) = split /\//, $_[0], 2;
  delete $short_refs{$context}{$dsp};
}

CLI grammar elements.
Generators for various syntactic constructs given a context. Here's what they
represent:

| pseries(context): a chain of consecutive operators in the context
  plambda(context): a lambda-list: [ chain ]
  pcli(context):    a complete command-line within the context

sub psuffix($)    {prep $contexts{$_[0]}}
sub pseries($)    {prep pn 1, popt pempty, $contexts{$_[0]}, popt pempty}
sub plambda($)    {pn 1, prc qr/\[/, pseries $_[0], prx qr/\]/}
sub pqfn($)       {palt plambda $_[0], psuffix $_[0]}

sub pcli($)       {pn 0, pseries $_[0], pend}
sub pcli_debug($) {pseries $_[0]}

sub cli(@) {my ($r) = parse pcli '', @_; $r}
25 op.pl.sdoc
Operator definition.
Like ni's parser combinators, operators are indirected using names. This
provides an intermediate representation that can be inspected and serialized.

our %operators;
sub defoperator($$) {
  my ($name, $f) = @_;
  die "ni: cannot redefine operator $name" if exists $operators{$name};
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
48 self.pl.sdoc
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
sub image_with(%) {
  my %old_self = %self;
  my %h        = @_;
  $self{'ni.map'} .= join '', map "\nresource $_", keys %h;
  @self{keys %h} = values %h;
  my $i = image;
  %self = %old_self;
  $i;
}
62 main.pl.sdoc
CLI entry point.
Some custom toplevel option handlers and the main function that ni uses to
parse CLI options and execute the data pipeline.

our %cli_special;
sub defclispecial($$) {$cli_special{$_[0]} = fn $_[1]}

Development options.
Things useful for developing ni.

defclispecial '--dev/eval', q{print ni::eval($_[0], "anon $_[0]"), "\n"};
defclispecial '--dev/parse', q{
  dev_trace 'ni::parse';
  parse pcli '', @_;
};

defclispecial '--dev/parse-one', q{
  dev_trace 'ni::parse';
  parse ni::eval($_[0]), @_[1..$#_];
};

Extensions.
Options to extend and modify the ni image.

defclispecial '--internal/lib', q{extend_self 'lib', $_[0]};
defclispecial '--lib', q{intern_lib shift; goto \&main};

Documentation.

defclispecial '--explain', q{
  my ($r) = parse pcli '', @_;
  print json_encode($_), "\n" for @$r;
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

  my ($r) = cli @_;
  return &$main_operator(flatten_operators $r) if ref $r;

  my (undef, @rest) = parse pcli_debug '', @_;
  print STDERR "ni: failed to parse starting here (ni --dev/parse to trace):\n";
  print STDERR "  @rest\n";
  exit 1;
}
3 core/stream/lib
procfh.pl.sdoc
pipeline.pl.sdoc
ops.pl.sdoc
85 core/stream/procfh.pl.sdoc
Process + filehandle combination.
We can't use Perl's process-aware FHs because they block-wait for the process
on close. There are some situations where we care about the exit code and
others where we don't, and this class supports both cases.

package ni::procfh;

use strict;
use warnings;
use POSIX qw/:sys_wait_h/;

Global child collector.
Collect children regardless of whether anyone is listening for them. If we have
an interested party, notify them.

our %child_owners;

sub await_children {
  local ($!, $?, $_);
  while (0 < ($_ = waitpid -1, WNOHANG)) {
    $child_owners{$_}->child_exited($?) if defined $child_owners{$_};
  }
  $SIG{CHLD} = \&await_children;
};
$SIG{CHLD} = \&await_children;

Proc-filehandle class.
Overloading *{} makes it possible for this to act like a real filehandle in
every sense: fileno() works, syswrite() works, etc. The constructor takes care
of numeric fds by promoting them into Perl fh references.

use overload qw/*{} fh "" str/;
sub new($$$) {
  my ($class, $fd, $pid) = @_;
  my $fh = ref($fd) ? $fd : undef;
  open $fh, "<&=$fd" or die "ni: procfh($fd, $pid) failed: $!"
    unless defined $fh;
  $child_owners{$pid} = bless {fh => $fh, pid => $pid, status => undef}, $class;
}

sub DESTROY   {
  my ($self) = @_;
  delete $child_owners{$$self{pid}} unless defined $$self{status};
}

sub fh($)     {my ($self) = @_; $$self{fh}}
sub pid($)    {my ($self) = @_; $$self{pid}}
sub status($) {my ($self) = @_; $$self{status}}

sub kill($$) {
  my ($self, $sig) = @_;
  kill $sig, $$self{pid} unless defined $$self{status};
}

sub str($)
{ my ($self) = @_;
  sprintf "<fd %d, pid %d, status %s>",
          fileno $$self{fh}, $$self{pid}, $$self{status} || 'none' }

Child await.
We have to stop the SIGCHLD handler while we wait for the child in question.
Otherwise we run the risk of waitpid() blocking forever or catching the wrong
process. This ends up being fine because this process can't create more
children while waitpid() is waiting, so we might have some resource delays but
we won't have a leak.

We also don't have to worry about multithreading: only one await() call can
happen per process.

sub await($) {
  local ($?, $SIG{CHLD});
  my ($self) = @_;
  return $$self{status} if defined $$self{status};
  $SIG{CHLD} = 'DEFAULT';
  return $$self{status} if defined $$self{status};
  my $pid = waitpid $$self{pid}, 0;
  $self->child_exited($pid <= 0 ? "-1 [waitpid: $!]" : $?);
  $$self{status};
}

sub child_exited($$) {
  my ($self, $status) = @_;
  $$self{status} = $status;
  delete $child_owners{$$self{pid}};
}
218 core/stream/pipeline.pl.sdoc
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

sub sh($) {exec 'sh', '-c', $_[0]}

Safe reads/writes.
This is required because older versions of Perl don't automatically retry
interrupted reads/writes. We run the risk of interruption because we have a
SIGCHLD handler. nfu lost data on older versions of Perl because it failed to
handle this case properly.

sub saferead($$$;$) {
  my $n;
  do {
    return $n if defined($n = sysread $_[0], $_[1], $_[2], $_[3]);
  } while $!{EINTR};
  return undef;
}

sub safewrite($$) {
  my $n;
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
  my $pid;
  if ($pid = cfork) {
    close $child;
    return ni::procfh->new($ret, $pid);
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

sub sicons(&) {
  my ($f) = @_;
  my $fh = soproc {&$f};
  move_fd fileno $fh, 0;
  open STDIN, '<&=0';
  $fh;
}

sub socons(&) {
  my ($f) = @_;
  my $fh = siproc {&$f};
  move_fd fileno $fh, 1;
  open STDOUT, '>&=1';
  $fh;
}

Stream functions.
These are called by pipelines to simplify things. For example, a common
operation is to append the output of some data-producing command:

| $ ni . .              # lists current directory twice

If you do this, ni will create a pipeline that uses stream wrappers to
concatenate the second `ls` output (despite the fact that technically it's a
shell pipe).

sub sforward($$) {local $_; safewrite $_[1], $_ while saferead $_[0], $_, 8192}
sub stee($$$)    {local $_; safewrite($_[1], $_), safewrite($_[2], $_) while saferead $_[0], $_, 8192}
sub sio()        {sforward \*STDIN, \*STDOUT}

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
    my $o = siproc {sh $decoder};
    safewrite $o, $_;
    sforward \*STDIN, $o;
    close $o;
    $o->await;
  } else {
    safewrite \*STDOUT, $_;
    sio;
  }
}

File/directory cat.
cat exists to turn filesystem objects into text. Files are emitted and
directories are turned into readable listings. Files are automatically
decompressed.

sub scat {
  local $| = 1;
  for my $f (@_) {
    if (-d $f) {
      opendir my $d, $f or die "ni_cat: failed to opendir $f: $!";
      print "$f/$_\n" for sort grep !/^\.\.?$/, readdir $d;
      closedir $d;
    } else {
      my $d = siproc {sdecode};
      sforward srfile $f, $d;
      close $d->fh;
      $d->await;
    }
  }
}

Self invocation.
You can run ni and read from the resulting file descriptor; this gives you a
way to evaluate lambda expressions (this is how checkpoints work, for example).
If you do this, ni's standard input will come from a continuation of __DATA__.

defclispecial '--internal/operate', q{
  my ($k) = @_;
  my $fh = siproc {&$main_operator(flatten_operators json_decode($self{$k}))};
  print $fh $_ while <$data>;
  close $fh;
  $fh->await;
};

sub sni_exec_list(@) {
  my $stdin = image_with 'transient/op' => json_encode([@_]);
  ($stdin, qw|perl - --internal/operate transient/op|);
}

sub exec_ni(@) {
  my ($stdin, @argv) = sni_exec_list @_;
  my $fh = siproc {exec @argv};
  safewrite $fh, $stdin;
  sforward \*STDIN, $fh;
  close $fh;
  exit $fh->await;
}

sub sni(@) {
  my @args = @_;
  soproc {close STDIN; close 0; exec_ni @args};
}
125 core/stream/ops.pl.sdoc
Streaming data sources.
Common ways to read data, most notably from files and directories. Also
included are numeric generators, shell commands, etc.

$main_operator = sub {
  -t STDIN ? close STDIN : sicons {sdecode};
  @$_ && sicons {operate @$_} for @_;
  exec 'less' or exec 'more' if -t STDOUT;
  sio;
  0;
};

use constant shell_lambda    => pn 1, prx '\[',  prep(prc '.*[^]]'), prx '\]$';
use constant shell_lambda_ws => pn 1, prc '\[$', prep(pnx '\]$'),    prc '\]$';
use constant shell_command   => palt pmap(q{shell_quote @$_}, shell_lambda),
                                     pmap(q{shell_quote @$_}, shell_lambda_ws),
                                     prx '.*';

defoperator cat  => q{my ($f) = @_; sio; scat $f};
defoperator echo => q{my ($x) = @_; sio; print "$x\n"};
defoperator sh   => q{my ($c) = @_; sh $c};

Note that we generate numbers internally rather than shelling out to `seq`
(which is ~20x faster than Perl for the purpose, incidentally). This is
deliberate: certain versions of `seq` generate floating-point numbers after a
point, which can cause unexpected results and loss of precision.

defoperator n => q{
  my ($l, $u) = @_;
  sio; for (my $i = $l; $i < $u; ++$i) {print "$i\n"};
};

defshort '/n',   pmap q{n_op 1, $_ + 1}, number;
defshort '/n0',  pmap q{n_op 0, $_}, number;
defshort '/id:', pmap q{echo_op $_}, prc '.*';

defshort '/e', pmap q{sh_op $_}, shell_command;

deflong '/fs', pmap q{cat_op $_}, filename;

Stream mixing/forking.
Append, prepend, duplicate, divert.

defoperator append => q{
  my @xs = @_;
  sio;
  my $fh = siproc {exec_ni @xs};
  close $fh;
  $fh->await;
};

defoperator prepend => q{
  my @xs = @_;
  my $fh = siproc {exec_ni @xs};
  close $fh;
  $fh->await;
  sio;
};

defoperator duplicate => q{
  my @xs = @_;
  my $fh = siproc {exec_ni @xs};
  stee \*STDIN, $fh, \*STDOUT;
  close $fh;
  $fh->await;
};

defoperator sink_null => q{1 while saferead \*STDIN, $_, 8192};
defoperator divert => q{
  my @xs = @_;
  my $fh = siproc {close STDOUT; exec_ni @xs, sink_null_op};
  stee \*STDIN, $fh, \*STDOUT;
  close $fh;
  $fh->await;
};

defshort '/+', pmap q{append_op    @$_}, pqfn '';
defshort '/^', pmap q{prepend_op   @$_}, pqfn '';
defshort '/%', pmap q{duplicate_op @$_}, pqfn '';
defshort '/=', pmap q{divert_op    @$_}, pqfn '';

Sinking.
We can sink data into a file just as easily as we can read from it. This is
done with the `>` operator, which is typically written as `\>`. The difference
between this and the shell's > operator is that \> outputs the filename; this
lets you invert the operation with the nullary \< operator.

use constant tmpdir => $ENV{TMPDIR} || "/tmp";
sub tempfile_name() {
  my $r = '/';
  $r = tmpdir . "/ni-$$-" . noise_str 8 while -e $r;
  $r;
}

defoperator file_read  => q{chomp, weval q{scat $_} while <STDIN>};
defoperator file_write => q{
  my ($file) = @_;
  $file = tempfile_name unless defined $file;
  sforward \*STDIN, swfile $file;
  print "$file\n";
};

defshort '/>', pmap q{file_write_op $_}, nefilename;
defshort '/<', pmap q{file_read_op},     pnone;

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
         defined $level ? sh_op "$c -$level" : sh_op $c},
  pseq popt compressor_name, popt integer;

defoperator decode => q{sdecode};

defshort '/z',  compressor_spec;
defshort '/zn', pk sink_null_op();
defshort '/zd', pk decode_op();
2 core/meta/lib
meta.pl.sdoc
map.pl.sdoc
44 core/meta/meta.pl.sdoc
Image-related data sources.
Long options to access ni's internal state. Also the ability to instantiate ni
within a shell process.

defoperator meta_image => q{sio; print image, "\n"};
defoperator meta_keys  => q{sio; print "$_\n" for sort keys %self};
defoperator meta_key   => q{my @ks = @_; sio; print "$_\n" for @self{@ks}};

defoperator meta_help => q{
  my ($topic) = @_;
  $topic = 'tutorial' unless length $topic;
  sio; print $self{"doc/$topic.md"} . "\n";
};

defshort '//',         pmap q{meta_key_op $_}, prc '[^][]+$';
defshort '///ni',      pmap q{meta_image_op},  pnone;
defshort '///ni/keys', pmap q{meta_keys_op},   pnone;

Documentation options.
These are listed under the `//help` prefix. This isn't a toplevel option
because it's more straightforward to model these as data sources.

sub meta_context_name($) {$_[0] || '<root>'}

defshort '///help', pmap q{meta_help_op $_}, popt prx '/(.*)';

defoperator meta_options => q{
  sio;
  for my $c (sort keys %contexts) {
    printf "%s\tlong\t%s\t%s\n",  meta_context_name $c, $long_names{$c}[$_], abbrev dev_inspect_nonl $long_refs{$c}[$_],  40 for       0..$#{$long_refs{$c}};
    printf "%s\tshort\t%s\t%s\n", meta_context_name $c, $_,                  abbrev dev_inspect_nonl $short_refs{$c}{$_}, 40 for sort keys %{$short_refs{$c}};
  }
};

defshort '///options', pmap q{meta_options_op}, pnone;

Inspection.
This lets you get details about specific operators or parsing contexts.

defoperator meta_op  => q{sio; print "sub {$operators{$_[0]}}\n"};
defoperator meta_ops => q{sio; print "$_\n" for sort keys %operators};

defshort '///op/', pmap q{meta_op_op $_}, prc '(.+)';
defshort '///ops', pmap q{meta_ops_op},   pnone;
24 core/meta/map.pl.sdoc
Syntax mapping.
We can inspect the parser dispatch tables within various contexts to get a
character-level map of prefixes and to indicate which characters are available
for additional operators.

use constant qwerty_prefixes => 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@%=$+_,.:';
use constant qwerty_effort =>   '02200011000021011000011212244222332222432332222334344444455544565565223';

defoperator meta_short_availability => q{
  sio;
  print "--------" . qwerty_prefixes . "\n";
  for my $c (sort keys %contexts) {
    my $s = $short_refs{$c};
    my %multi;
    ++$multi{substr $_, 0, 1} for grep 1 < length, keys %$s;

    print substr(meta_context_name $c, 0, 7) . "\t"
        . join('', map $multi{$_} ? '.' : $$s{$_} ? '|' : ' ',
                       split //, qwerty_prefixes)
        . "\n";
  }
};

defshort '///map/short', pmap q{meta_short_availability}, pnone;
1 core/deps/lib
sha1.pm
1031 core/deps/sha1.pm
package Digest::SHA::PurePerl;

require 5.003000;

use strict;
use warnings;
use vars qw($VERSION @ISA @EXPORT_OK);
use Fcntl qw(O_RDONLY);
use integer;
use Carp qw(croak);

$VERSION = '5.96';

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = ();		# see "SHA and HMAC-SHA functions" below

# Inherit from Digest::base if possible

eval {
	require Digest::base;
	push(@ISA, 'Digest::base');
};

# ref. src/sha.c and sha/sha64bit.c from Digest::SHA

my $MAX32 = 0xffffffff;

my $uses64bit = (((1 << 16) << 16) << 16) << 15;

my @H01 = (			# SHA-1 initial hash value
	0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476,
	0xc3d2e1f0
);

my @H0224 = (			# SHA-224 initial hash value
	0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939,
	0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4
);

my @H0256 = (			# SHA-256 initial hash value
	0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
	0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
);

my(@H0384, @H0512, @H0512224, @H0512256);  # filled in later if $uses64bit

# Routines with a "_c_" prefix return Perl code-fragments which are
# eval'ed at initialization.  This technique emulates the behavior
# of the C preprocessor, allowing the optimized transform code from
# Digest::SHA to be more easily translated into Perl.

sub _c_SL32 {			# code to shift $x left by $n bits
	my($x, $n) = @_;
	"($x << $n)";		# even works for 64-bit integers
				# since the upper 32 bits are
				# eventually discarded in _digcpy
}

sub _c_SR32 {			# code to shift $x right by $n bits
	my($x, $n) = @_;
	my $mask = (1 << (32 - $n)) - 1;
	"(($x >> $n) & $mask)";		# "use integer" does arithmetic
					# shift, so clear upper bits
}

sub _c_Ch { my($x, $y, $z) = @_; "($z ^ ($x & ($y ^ $z)))" }
sub _c_Pa { my($x, $y, $z) = @_; "($x ^ $y ^ $z)" }
sub _c_Ma { my($x, $y, $z) = @_; "(($x & $y) | ($z & ($x | $y)))" }

sub _c_ROTR {			# code to rotate $x right by $n bits
	my($x, $n) = @_;
	"(" . _c_SR32($x, $n) . " | " . _c_SL32($x, 32 - $n) . ")";
}

sub _c_ROTL {			# code to rotate $x left by $n bits
	my($x, $n) = @_;
	"(" . _c_SL32($x, $n) . " | " . _c_SR32($x, 32 - $n) . ")";
}

sub _c_SIGMA0 {			# ref. NIST SHA standard
	my($x) = @_;
	"(" . _c_ROTR($x,  2) . " ^ " . _c_ROTR($x, 13) . " ^ " .
		_c_ROTR($x, 22) . ")";
}

sub _c_SIGMA1 {
	my($x) = @_;
	"(" . _c_ROTR($x,  6) . " ^ " . _c_ROTR($x, 11) . " ^ " .
		_c_ROTR($x, 25) . ")";
}

sub _c_sigma0 {
	my($x) = @_;
	"(" . _c_ROTR($x,  7) . " ^ " . _c_ROTR($x, 18) . " ^ " .
		_c_SR32($x,  3) . ")";
}

sub _c_sigma1 {
	my($x) = @_;
	"(" . _c_ROTR($x, 17) . " ^ " . _c_ROTR($x, 19) . " ^ " .
		_c_SR32($x, 10) . ")";
}

sub _c_M1Ch {			# ref. Digest::SHA sha.c (sha1 routine)
	my($a, $b, $c, $d, $e, $k, $w) = @_;
	"$e += " . _c_ROTL($a, 5) . " + " . _c_Ch($b, $c, $d) .
		" + $k + $w; $b = " . _c_ROTL($b, 30) . ";\n";
}

sub _c_M1Pa {
	my($a, $b, $c, $d, $e, $k, $w) = @_;
	"$e += " . _c_ROTL($a, 5) . " + " . _c_Pa($b, $c, $d) .
		" + $k + $w; $b = " . _c_ROTL($b, 30) . ";\n";
}

sub _c_M1Ma {
	my($a, $b, $c, $d, $e, $k, $w) = @_;
	"$e += " . _c_ROTL($a, 5) . " + " . _c_Ma($b, $c, $d) .
		" + $k + $w; $b = " . _c_ROTL($b, 30) . ";\n";
}

sub _c_M11Ch { my($k, $w) = @_; _c_M1Ch('$a', '$b', '$c', '$d', '$e', $k, $w) }
sub _c_M11Pa { my($k, $w) = @_; _c_M1Pa('$a', '$b', '$c', '$d', '$e', $k, $w) }
sub _c_M11Ma { my($k, $w) = @_; _c_M1Ma('$a', '$b', '$c', '$d', '$e', $k, $w) }
sub _c_M12Ch { my($k, $w) = @_; _c_M1Ch('$e', '$a', '$b', '$c', '$d', $k, $w) }
sub _c_M12Pa { my($k, $w) = @_; _c_M1Pa('$e', '$a', '$b', '$c', '$d', $k, $w) }
sub _c_M12Ma { my($k, $w) = @_; _c_M1Ma('$e', '$a', '$b', '$c', '$d', $k, $w) }
sub _c_M13Ch { my($k, $w) = @_; _c_M1Ch('$d', '$e', '$a', '$b', '$c', $k, $w) }
sub _c_M13Pa { my($k, $w) = @_; _c_M1Pa('$d', '$e', '$a', '$b', '$c', $k, $w) }
sub _c_M13Ma { my($k, $w) = @_; _c_M1Ma('$d', '$e', '$a', '$b', '$c', $k, $w) }
sub _c_M14Ch { my($k, $w) = @_; _c_M1Ch('$c', '$d', '$e', '$a', '$b', $k, $w) }
sub _c_M14Pa { my($k, $w) = @_; _c_M1Pa('$c', '$d', '$e', '$a', '$b', $k, $w) }
sub _c_M14Ma { my($k, $w) = @_; _c_M1Ma('$c', '$d', '$e', '$a', '$b', $k, $w) }
sub _c_M15Ch { my($k, $w) = @_; _c_M1Ch('$b', '$c', '$d', '$e', '$a', $k, $w) }
sub _c_M15Pa { my($k, $w) = @_; _c_M1Pa('$b', '$c', '$d', '$e', '$a', $k, $w) }
sub _c_M15Ma { my($k, $w) = @_; _c_M1Ma('$b', '$c', '$d', '$e', '$a', $k, $w) }

sub _c_W11 { my($s) = @_; '$W[' . (($s +  0) & 0xf) . ']' }
sub _c_W12 { my($s) = @_; '$W[' . (($s + 13) & 0xf) . ']' }
sub _c_W13 { my($s) = @_; '$W[' . (($s +  8) & 0xf) . ']' }
sub _c_W14 { my($s) = @_; '$W[' . (($s +  2) & 0xf) . ']' }

sub _c_A1 {
	my($s) = @_;
	my $tmp = _c_W11($s) . " ^ " . _c_W12($s) . " ^ " .
		_c_W13($s) . " ^ " . _c_W14($s);
	"((\$tmp = $tmp), (" . _c_W11($s) . " = " . _c_ROTL('$tmp', 1) . "))";
}

# The following code emulates the "sha1" routine from Digest::SHA sha.c

my $sha1_code = '

my($K1, $K2, $K3, $K4) = (	# SHA-1 constants
	0x5a827999, 0x6ed9eba1, 0x8f1bbcdc, 0xca62c1d6
);

sub _sha1 {
	my($self, $block) = @_;
	my(@W, $a, $b, $c, $d, $e, $tmp);

	@W = unpack("N16", $block);
	($a, $b, $c, $d, $e) = @{$self->{H}};
' .
	_c_M11Ch('$K1', '$W[ 0]'  ) . _c_M12Ch('$K1', '$W[ 1]'  ) .
	_c_M13Ch('$K1', '$W[ 2]'  ) . _c_M14Ch('$K1', '$W[ 3]'  ) .
	_c_M15Ch('$K1', '$W[ 4]'  ) . _c_M11Ch('$K1', '$W[ 5]'  ) .
	_c_M12Ch('$K1', '$W[ 6]'  ) . _c_M13Ch('$K1', '$W[ 7]'  ) .
	_c_M14Ch('$K1', '$W[ 8]'  ) . _c_M15Ch('$K1', '$W[ 9]'  ) .
	_c_M11Ch('$K1', '$W[10]'  ) . _c_M12Ch('$K1', '$W[11]'  ) .
	_c_M13Ch('$K1', '$W[12]'  ) . _c_M14Ch('$K1', '$W[13]'  ) .
	_c_M15Ch('$K1', '$W[14]'  ) . _c_M11Ch('$K1', '$W[15]'  ) .
	_c_M12Ch('$K1', _c_A1( 0) ) . _c_M13Ch('$K1', _c_A1( 1) ) .
	_c_M14Ch('$K1', _c_A1( 2) ) . _c_M15Ch('$K1', _c_A1( 3) ) .
	_c_M11Pa('$K2', _c_A1( 4) ) . _c_M12Pa('$K2', _c_A1( 5) ) .
	_c_M13Pa('$K2', _c_A1( 6) ) . _c_M14Pa('$K2', _c_A1( 7) ) .
	_c_M15Pa('$K2', _c_A1( 8) ) . _c_M11Pa('$K2', _c_A1( 9) ) .
	_c_M12Pa('$K2', _c_A1(10) ) . _c_M13Pa('$K2', _c_A1(11) ) .
	_c_M14Pa('$K2', _c_A1(12) ) . _c_M15Pa('$K2', _c_A1(13) ) .
	_c_M11Pa('$K2', _c_A1(14) ) . _c_M12Pa('$K2', _c_A1(15) ) .
	_c_M13Pa('$K2', _c_A1( 0) ) . _c_M14Pa('$K2', _c_A1( 1) ) .
	_c_M15Pa('$K2', _c_A1( 2) ) . _c_M11Pa('$K2', _c_A1( 3) ) .
	_c_M12Pa('$K2', _c_A1( 4) ) . _c_M13Pa('$K2', _c_A1( 5) ) .
	_c_M14Pa('$K2', _c_A1( 6) ) . _c_M15Pa('$K2', _c_A1( 7) ) .
	_c_M11Ma('$K3', _c_A1( 8) ) . _c_M12Ma('$K3', _c_A1( 9) ) .
	_c_M13Ma('$K3', _c_A1(10) ) . _c_M14Ma('$K3', _c_A1(11) ) .
	_c_M15Ma('$K3', _c_A1(12) ) . _c_M11Ma('$K3', _c_A1(13) ) .
	_c_M12Ma('$K3', _c_A1(14) ) . _c_M13Ma('$K3', _c_A1(15) ) .
	_c_M14Ma('$K3', _c_A1( 0) ) . _c_M15Ma('$K3', _c_A1( 1) ) .
	_c_M11Ma('$K3', _c_A1( 2) ) . _c_M12Ma('$K3', _c_A1( 3) ) .
	_c_M13Ma('$K3', _c_A1( 4) ) . _c_M14Ma('$K3', _c_A1( 5) ) .
	_c_M15Ma('$K3', _c_A1( 6) ) . _c_M11Ma('$K3', _c_A1( 7) ) .
	_c_M12Ma('$K3', _c_A1( 8) ) . _c_M13Ma('$K3', _c_A1( 9) ) .
	_c_M14Ma('$K3', _c_A1(10) ) . _c_M15Ma('$K3', _c_A1(11) ) .
	_c_M11Pa('$K4', _c_A1(12) ) . _c_M12Pa('$K4', _c_A1(13) ) .
	_c_M13Pa('$K4', _c_A1(14) ) . _c_M14Pa('$K4', _c_A1(15) ) .
	_c_M15Pa('$K4', _c_A1( 0) ) . _c_M11Pa('$K4', _c_A1( 1) ) .
	_c_M12Pa('$K4', _c_A1( 2) ) . _c_M13Pa('$K4', _c_A1( 3) ) .
	_c_M14Pa('$K4', _c_A1( 4) ) . _c_M15Pa('$K4', _c_A1( 5) ) .
	_c_M11Pa('$K4', _c_A1( 6) ) . _c_M12Pa('$K4', _c_A1( 7) ) .
	_c_M13Pa('$K4', _c_A1( 8) ) . _c_M14Pa('$K4', _c_A1( 9) ) .
	_c_M15Pa('$K4', _c_A1(10) ) . _c_M11Pa('$K4', _c_A1(11) ) .
	_c_M12Pa('$K4', _c_A1(12) ) . _c_M13Pa('$K4', _c_A1(13) ) .
	_c_M14Pa('$K4', _c_A1(14) ) . _c_M15Pa('$K4', _c_A1(15) ) .

'	$self->{H}->[0] += $a; $self->{H}->[1] += $b; $self->{H}->[2] += $c;
	$self->{H}->[3] += $d; $self->{H}->[4] += $e;
}
';

eval($sha1_code);

sub _c_M2 {			# ref. Digest::SHA sha.c (sha256 routine)
	my($a, $b, $c, $d, $e, $f, $g, $h, $w) = @_;
	"\$T1 = $h + " . _c_SIGMA1($e) . " + " . _c_Ch($e, $f, $g) .
		" + \$K256[\$i++] + $w; $h = \$T1 + " . _c_SIGMA0($a) .
		" + " . _c_Ma($a, $b, $c) . "; $d += \$T1;\n";
}

sub _c_M21 { _c_M2('$a', '$b', '$c', '$d', '$e', '$f', '$g', '$h', $_[0]) }
sub _c_M22 { _c_M2('$h', '$a', '$b', '$c', '$d', '$e', '$f', '$g', $_[0]) }
sub _c_M23 { _c_M2('$g', '$h', '$a', '$b', '$c', '$d', '$e', '$f', $_[0]) }
sub _c_M24 { _c_M2('$f', '$g', '$h', '$a', '$b', '$c', '$d', '$e', $_[0]) }
sub _c_M25 { _c_M2('$e', '$f', '$g', '$h', '$a', '$b', '$c', '$d', $_[0]) }
sub _c_M26 { _c_M2('$d', '$e', '$f', '$g', '$h', '$a', '$b', '$c', $_[0]) }
sub _c_M27 { _c_M2('$c', '$d', '$e', '$f', '$g', '$h', '$a', '$b', $_[0]) }
sub _c_M28 { _c_M2('$b', '$c', '$d', '$e', '$f', '$g', '$h', '$a', $_[0]) }

sub _c_W21 { my($s) = @_; '$W[' . (($s +  0) & 0xf) . ']' }
sub _c_W22 { my($s) = @_; '$W[' . (($s + 14) & 0xf) . ']' }
sub _c_W23 { my($s) = @_; '$W[' . (($s +  9) & 0xf) . ']' }
sub _c_W24 { my($s) = @_; '$W[' . (($s +  1) & 0xf) . ']' }

sub _c_A2 {
	my($s) = @_;
	"(" . _c_W21($s) . " += " . _c_sigma1(_c_W22($s)) . " + " .
		_c_W23($s) . " + " . _c_sigma0(_c_W24($s)) . ")";
}

# The following code emulates the "sha256" routine from Digest::SHA sha.c

my $sha256_code = '

my @K256 = (			# SHA-224/256 constants
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
	0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
	0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
	0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
	0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
	0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
);

sub _sha256 {
	my($self, $block) = @_;
	my(@W, $a, $b, $c, $d, $e, $f, $g, $h, $i, $T1);

	@W = unpack("N16", $block);
	($a, $b, $c, $d, $e, $f, $g, $h) = @{$self->{H}};
' .
	_c_M21('$W[ 0]' ) . _c_M22('$W[ 1]' ) . _c_M23('$W[ 2]' ) .
	_c_M24('$W[ 3]' ) . _c_M25('$W[ 4]' ) . _c_M26('$W[ 5]' ) .
	_c_M27('$W[ 6]' ) . _c_M28('$W[ 7]' ) . _c_M21('$W[ 8]' ) .
	_c_M22('$W[ 9]' ) . _c_M23('$W[10]' ) . _c_M24('$W[11]' ) .
	_c_M25('$W[12]' ) . _c_M26('$W[13]' ) . _c_M27('$W[14]' ) .
	_c_M28('$W[15]' ) .
	_c_M21(_c_A2( 0)) . _c_M22(_c_A2( 1)) . _c_M23(_c_A2( 2)) .
	_c_M24(_c_A2( 3)) . _c_M25(_c_A2( 4)) . _c_M26(_c_A2( 5)) .
	_c_M27(_c_A2( 6)) . _c_M28(_c_A2( 7)) . _c_M21(_c_A2( 8)) .
	_c_M22(_c_A2( 9)) . _c_M23(_c_A2(10)) . _c_M24(_c_A2(11)) .
	_c_M25(_c_A2(12)) . _c_M26(_c_A2(13)) . _c_M27(_c_A2(14)) .
	_c_M28(_c_A2(15)) . _c_M21(_c_A2( 0)) . _c_M22(_c_A2( 1)) .
	_c_M23(_c_A2( 2)) . _c_M24(_c_A2( 3)) . _c_M25(_c_A2( 4)) .
	_c_M26(_c_A2( 5)) . _c_M27(_c_A2( 6)) . _c_M28(_c_A2( 7)) .
	_c_M21(_c_A2( 8)) . _c_M22(_c_A2( 9)) . _c_M23(_c_A2(10)) .
	_c_M24(_c_A2(11)) . _c_M25(_c_A2(12)) . _c_M26(_c_A2(13)) .
	_c_M27(_c_A2(14)) . _c_M28(_c_A2(15)) . _c_M21(_c_A2( 0)) .
	_c_M22(_c_A2( 1)) . _c_M23(_c_A2( 2)) . _c_M24(_c_A2( 3)) .
	_c_M25(_c_A2( 4)) . _c_M26(_c_A2( 5)) . _c_M27(_c_A2( 6)) .
	_c_M28(_c_A2( 7)) . _c_M21(_c_A2( 8)) . _c_M22(_c_A2( 9)) .
	_c_M23(_c_A2(10)) . _c_M24(_c_A2(11)) . _c_M25(_c_A2(12)) .
	_c_M26(_c_A2(13)) . _c_M27(_c_A2(14)) . _c_M28(_c_A2(15)) .

'	$self->{H}->[0] += $a; $self->{H}->[1] += $b; $self->{H}->[2] += $c;
	$self->{H}->[3] += $d; $self->{H}->[4] += $e; $self->{H}->[5] += $f;
	$self->{H}->[6] += $g; $self->{H}->[7] += $h;
}
';

eval($sha256_code);

sub _sha512_placeholder { return }
my $sha512 = \&_sha512_placeholder;

my $_64bit_code = '

no warnings qw(portable);

my @K512 = (
	0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f,
	0xe9b5dba58189dbbc, 0x3956c25bf348b538, 0x59f111f1b605d019,
	0x923f82a4af194f9b, 0xab1c5ed5da6d8118, 0xd807aa98a3030242,
	0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
	0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235,
	0xc19bf174cf692694, 0xe49b69c19ef14ad2, 0xefbe4786384f25e3,
	0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65, 0x2de92c6f592b0275,
	0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
	0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f,
	0xbf597fc7beef0ee4, 0xc6e00bf33da88fc2, 0xd5a79147930aa725,
	0x06ca6351e003826f, 0x142929670a0e6e70, 0x27b70a8546d22ffc,
	0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
	0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6,
	0x92722c851482353b, 0xa2bfe8a14cf10364, 0xa81a664bbc423001,
	0xc24b8b70d0f89791, 0xc76c51a30654be30, 0xd192e819d6ef5218,
	0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
	0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99,
	0x34b0bcb5e19b48a8, 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb,
	0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3, 0x748f82ee5defb2fc,
	0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
	0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915,
	0xc67178f2e372532b, 0xca273eceea26619c, 0xd186b8c721c0c207,
	0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178, 0x06f067aa72176fba,
	0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
	0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc,
	0x431d67c49c100d4c, 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a,
	0x5fcb6fab3ad6faec, 0x6c44198c4a475817);

@H0384 = (
	0xcbbb9d5dc1059ed8, 0x629a292a367cd507, 0x9159015a3070dd17,
	0x152fecd8f70e5939, 0x67332667ffc00b31, 0x8eb44a8768581511,
	0xdb0c2e0d64f98fa7, 0x47b5481dbefa4fa4);

@H0512 = (
	0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b,
	0xa54ff53a5f1d36f1, 0x510e527fade682d1, 0x9b05688c2b3e6c1f,
	0x1f83d9abfb41bd6b, 0x5be0cd19137e2179);

@H0512224 = (
	0x8c3d37c819544da2, 0x73e1996689dcd4d6, 0x1dfab7ae32ff9c82,
	0x679dd514582f9fcf, 0x0f6d2b697bd44da8, 0x77e36f7304c48942,
	0x3f9d85a86a1d36c8, 0x1112e6ad91d692a1);

@H0512256 = (
	0x22312194fc2bf72c, 0x9f555fa3c84c64c2, 0x2393b86b6f53b151,
	0x963877195940eabd, 0x96283ee2a88effe3, 0xbe5e1e2553863992,
	0x2b0199fc2c85b8aa, 0x0eb72ddc81c52ca2);

use warnings;

sub _c_SL64 { my($x, $n) = @_; "($x << $n)" }

sub _c_SR64 {
	my($x, $n) = @_;
	my $mask = (1 << (64 - $n)) - 1;
	"(($x >> $n) & $mask)";
}

sub _c_ROTRQ {
	my($x, $n) = @_;
	"(" . _c_SR64($x, $n) . " | " . _c_SL64($x, 64 - $n) . ")";
}

sub _c_SIGMAQ0 {
	my($x) = @_;
	"(" . _c_ROTRQ($x, 28) . " ^ " .  _c_ROTRQ($x, 34) . " ^ " .
		_c_ROTRQ($x, 39) . ")";
}

sub _c_SIGMAQ1 {
	my($x) = @_;
	"(" . _c_ROTRQ($x, 14) . " ^ " .  _c_ROTRQ($x, 18) . " ^ " .
		_c_ROTRQ($x, 41) . ")";
}

sub _c_sigmaQ0 {
	my($x) = @_;
	"(" . _c_ROTRQ($x, 1) . " ^ " .  _c_ROTRQ($x, 8) . " ^ " .
		_c_SR64($x, 7) . ")";
}

sub _c_sigmaQ1 {
	my($x) = @_;
	"(" . _c_ROTRQ($x, 19) . " ^ " .  _c_ROTRQ($x, 61) . " ^ " .
		_c_SR64($x, 6) . ")";
}

my $sha512_code = q/
sub _sha512 {
	my($self, $block) = @_;
	my(@N, @W, $a, $b, $c, $d, $e, $f, $g, $h, $T1, $T2);

	@N = unpack("N32", $block);
	($a, $b, $c, $d, $e, $f, $g, $h) = @{$self->{H}};
	for ( 0 .. 15) { $W[$_] = (($N[2*$_] << 16) << 16) | $N[2*$_+1] }
	for (16 .. 79) { $W[$_] = / .
		_c_sigmaQ1(q/$W[$_- 2]/) . q/ + $W[$_- 7] + / .
		_c_sigmaQ0(q/$W[$_-15]/) . q/ + $W[$_-16] }
	for ( 0 .. 79) {
		$T1 = $h + / . _c_SIGMAQ1(q/$e/) .
			q/ + (($g) ^ (($e) & (($f) ^ ($g)))) +
				$K512[$_] + $W[$_];
		$T2 = / . _c_SIGMAQ0(q/$a/) .
			q/ + ((($a) & ($b)) | (($c) & (($a) | ($b))));
		$h = $g; $g = $f; $f = $e; $e = $d + $T1;
		$d = $c; $c = $b; $b = $a; $a = $T1 + $T2;
	}
	$self->{H}->[0] += $a; $self->{H}->[1] += $b; $self->{H}->[2] += $c;
	$self->{H}->[3] += $d; $self->{H}->[4] += $e; $self->{H}->[5] += $f;
	$self->{H}->[6] += $g; $self->{H}->[7] += $h;
}
/;

eval($sha512_code);
$sha512 = \&_sha512;

';

eval($_64bit_code) if $uses64bit;

sub _SETBIT {
	my($self, $pos) = @_;
	my @c = unpack("C*", $self->{block});
	$c[$pos >> 3] = 0x00 unless defined $c[$pos >> 3];
	$c[$pos >> 3] |= (0x01 << (7 - $pos % 8));
	$self->{block} = pack("C*", @c);
}

sub _CLRBIT {
	my($self, $pos) = @_;
	my @c = unpack("C*", $self->{block});
	$c[$pos >> 3] = 0x00 unless defined $c[$pos >> 3];
	$c[$pos >> 3] &= ~(0x01 << (7 - $pos % 8));
	$self->{block} = pack("C*", @c);
}

sub _BYTECNT {
	my($bitcnt) = @_;
	$bitcnt > 0 ? 1 + (($bitcnt - 1) >> 3) : 0;
}

sub _digcpy {
	my($self) = @_;
	my @dig;
	for (@{$self->{H}}) {
		push(@dig, (($_>>16)>>16) & $MAX32) if $self->{alg} >= 384;
		push(@dig, $_ & $MAX32);
	}
	$self->{digest} = pack("N" . ($self->{digestlen}>>2), @dig);
}

sub _sharewind {
	my($self) = @_;
	my $alg = $self->{alg};
	$self->{block} = ""; $self->{blockcnt} = 0;
	$self->{blocksize} = $alg <= 256 ? 512 : 1024;
	for (qw(lenll lenlh lenhl lenhh)) { $self->{$_} = 0 }
	$self->{digestlen} = $alg == 1 ? 20 : ($alg % 1000)/8;
	if    ($alg == 1)   { $self->{sha} = \&_sha1;   $self->{H} = [@H01]   }
	elsif ($alg == 224) { $self->{sha} = \&_sha256; $self->{H} = [@H0224] }
	elsif ($alg == 256) { $self->{sha} = \&_sha256; $self->{H} = [@H0256] }
	elsif ($alg == 384) { $self->{sha} = $sha512;   $self->{H} = [@H0384] }
	elsif ($alg == 512) { $self->{sha} = $sha512;   $self->{H} = [@H0512] }
	elsif ($alg == 512224) { $self->{sha}=$sha512; $self->{H}=[@H0512224] }
	elsif ($alg == 512256) { $self->{sha}=$sha512; $self->{H}=[@H0512256] }
	push(@{$self->{H}}, 0) while scalar(@{$self->{H}}) < 8;
	$self;
}

sub _shaopen {
	my($alg) = @_;
	my($self);
	return unless grep { $alg == $_ } (1,224,256,384,512,512224,512256);
	return if ($alg >= 384 && !$uses64bit);
	$self->{alg} = $alg;
	_sharewind($self);
}

sub _shadirect {
	my($bitstr, $bitcnt, $self) = @_;
	my $savecnt = $bitcnt;
	my $offset = 0;
	my $blockbytes = $self->{blocksize} >> 3;
	while ($bitcnt >= $self->{blocksize}) {
		&{$self->{sha}}($self, substr($bitstr, $offset, $blockbytes));
		$offset += $blockbytes;
		$bitcnt -= $self->{blocksize};
	}
	if ($bitcnt > 0) {
		$self->{block} = substr($bitstr, $offset, _BYTECNT($bitcnt));
		$self->{blockcnt} = $bitcnt;
	}
	$savecnt;
}

sub _shabytes {
	my($bitstr, $bitcnt, $self) = @_;
	my($numbits);
	my $savecnt = $bitcnt;
	if ($self->{blockcnt} + $bitcnt >= $self->{blocksize}) {
		$numbits = $self->{blocksize} - $self->{blockcnt};
		$self->{block} .= substr($bitstr, 0, $numbits >> 3);
		$bitcnt -= $numbits;
		$bitstr = substr($bitstr, $numbits >> 3, _BYTECNT($bitcnt));
		&{$self->{sha}}($self, $self->{block});
		$self->{block} = "";
		$self->{blockcnt} = 0;
		_shadirect($bitstr, $bitcnt, $self);
	}
	else {
		$self->{block} .= substr($bitstr, 0, _BYTECNT($bitcnt));
		$self->{blockcnt} += $bitcnt;
	}
	$savecnt;
}

sub _shabits {
	my($bitstr, $bitcnt, $self) = @_;
	my($i, @buf);
	my $numbytes = _BYTECNT($bitcnt);
	my $savecnt = $bitcnt;
	my $gap = 8 - $self->{blockcnt} % 8;
	my @c = unpack("C*", $self->{block});
	my @b = unpack("C" . $numbytes, $bitstr);
	$c[$self->{blockcnt}>>3] &= (~0 << $gap);
	$c[$self->{blockcnt}>>3] |= $b[0] >> (8 - $gap);
	$self->{block} = pack("C*", @c);
	$self->{blockcnt} += ($bitcnt < $gap) ? $bitcnt : $gap;
	return($savecnt) if $bitcnt < $gap;
	if ($self->{blockcnt} == $self->{blocksize}) {
		&{$self->{sha}}($self, $self->{block});
		$self->{block} = "";
		$self->{blockcnt} = 0;
	}
	return($savecnt) if ($bitcnt -= $gap) == 0;
	for ($i = 0; $i < $numbytes - 1; $i++) {
		$buf[$i] = (($b[$i] << $gap) & 0xff) | ($b[$i+1] >> (8 - $gap));
	}
	$buf[$numbytes-1] = ($b[$numbytes-1] << $gap) & 0xff;
	_shabytes(pack("C*", @buf), $bitcnt, $self);
	$savecnt;
}

sub _shawrite {
	my($bitstr, $bitcnt, $self) = @_;
	return(0) unless $bitcnt > 0;
	no integer;
	my $TWO32 = 4294967296;
	if (($self->{lenll} += $bitcnt) >= $TWO32) {
		$self->{lenll} -= $TWO32;
		if (++$self->{lenlh} >= $TWO32) {
			$self->{lenlh} -= $TWO32;
			if (++$self->{lenhl} >= $TWO32) {
				$self->{lenhl} -= $TWO32;
				if (++$self->{lenhh} >= $TWO32) {
					$self->{lenhh} -= $TWO32;
				}
			}
		}
	}
	use integer;
	my $blockcnt = $self->{blockcnt};
	return(_shadirect($bitstr, $bitcnt, $self)) if $blockcnt == 0;
	return(_shabytes ($bitstr, $bitcnt, $self)) if $blockcnt % 8 == 0;
	return(_shabits  ($bitstr, $bitcnt, $self));
}

my $no_downgrade = 'sub utf8::downgrade { 1 }';

my $pp_downgrade = q {
	sub utf8::downgrade {

		# No need to downgrade if character and byte
		# semantics are equivalent.  But this might
		# leave the UTF-8 flag set, harmlessly.

		require bytes;
		return 1 if length($_[0]) == bytes::length($_[0]);

		use utf8;
		return 0 if $_[0] =~ /[^\x00-\xff]/;
		$_[0] = pack('C*', unpack('U*', $_[0]));
		return 1;
	}
};

{
	no integer;

	if    ($] < 5.006)	{ eval $no_downgrade }
	elsif ($] < 5.008)	{ eval $pp_downgrade }
}

my $WSE = 'Wide character in subroutine entry';
my $MWS = 16384;

sub _shaWrite {
	my($bytestr_r, $bytecnt, $self) = @_;
	return(0) unless $bytecnt > 0;
	croak $WSE unless utf8::downgrade($$bytestr_r, 1);
	return(_shawrite($$bytestr_r, $bytecnt<<3, $self)) if $bytecnt <= $MWS;
	my $offset = 0;
	while ($bytecnt > $MWS) {
		_shawrite(substr($$bytestr_r, $offset, $MWS), $MWS<<3, $self);
		$offset  += $MWS;
		$bytecnt -= $MWS;
	}
	_shawrite(substr($$bytestr_r, $offset, $bytecnt), $bytecnt<<3, $self);
}

sub _shafinish {
	my($self) = @_;
	my $LENPOS = $self->{alg} <= 256 ? 448 : 896;
	_SETBIT($self, $self->{blockcnt}++);
	while ($self->{blockcnt} > $LENPOS) {
		if ($self->{blockcnt} < $self->{blocksize}) {
			_CLRBIT($self, $self->{blockcnt}++);
		}
		else {
			&{$self->{sha}}($self, $self->{block});
			$self->{block} = "";
			$self->{blockcnt} = 0;
		}
	}
	while ($self->{blockcnt} < $LENPOS) {
		_CLRBIT($self, $self->{blockcnt}++);
	}
	if ($self->{blocksize} > 512) {
		$self->{block} .= pack("N", $self->{lenhh} & $MAX32);
		$self->{block} .= pack("N", $self->{lenhl} & $MAX32);
	}
	$self->{block} .= pack("N", $self->{lenlh} & $MAX32);
	$self->{block} .= pack("N", $self->{lenll} & $MAX32);
	&{$self->{sha}}($self, $self->{block});
}

sub _shadigest { my($self) = @_; _digcpy($self); $self->{digest} }

sub _shahex {
	my($self) = @_;
	_digcpy($self);
	join("", unpack("H*", $self->{digest}));
}

sub _shabase64 {
	my($self) = @_;
	_digcpy($self);
	my $b64 = pack("u", $self->{digest});
	$b64 =~ s/^.//mg;
	$b64 =~ s/\n//g;
	$b64 =~ tr|` -_|AA-Za-z0-9+/|;
	my $numpads = (3 - length($self->{digest}) % 3) % 3;
	$b64 =~ s/.{$numpads}$// if $numpads;
	$b64;
}

sub _shadsize { my($self) = @_; $self->{digestlen} }

sub _shacpy {
	my($to, $from) = @_;
	$to->{alg} = $from->{alg};
	$to->{sha} = $from->{sha};
	$to->{H} = [@{$from->{H}}];
	$to->{block} = $from->{block};
	$to->{blockcnt} = $from->{blockcnt};
	$to->{blocksize} = $from->{blocksize};
	for (qw(lenhh lenhl lenlh lenll)) { $to->{$_} = $from->{$_} }
	$to->{digestlen} = $from->{digestlen};
	$to;
}

sub _shadup { my($self) = @_; my($copy); _shacpy($copy, $self) }

sub _shadump {
	my $self = shift;
	for (qw(alg H block blockcnt lenhh lenhl lenlh lenll)) {
		return unless defined $self->{$_};
	}

	my @state = ();
	my $fmt = ($self->{alg} <= 256 ? "%08x" : "%016x");

	push(@state, "alg:" . $self->{alg});

	my @H = map { $self->{alg} <= 256 ? $_ & $MAX32 : $_ } @{$self->{H}};
	push(@state, "H:" . join(":", map { sprintf($fmt, $_) } @H));

	my @c = unpack("C*", $self->{block});
	push(@c, 0x00) while scalar(@c) < ($self->{blocksize} >> 3);
	push(@state, "block:" . join(":", map {sprintf("%02x", $_)} @c));
	push(@state, "blockcnt:" . $self->{blockcnt});

	push(@state, "lenhh:" . $self->{lenhh});
	push(@state, "lenhl:" . $self->{lenhl});
	push(@state, "lenlh:" . $self->{lenlh});
	push(@state, "lenll:" . $self->{lenll});
	join("\n", @state) . "\n";
}

sub _shaload {
	my $state = shift;

	my %s = ();
	for (split(/\n/, $state)) {
		s/^\s+//;
		s/\s+$//;
		next if (/^(#|$)/);
		my @f = split(/[:\s]+/);
		my $tag = shift(@f);
		$s{$tag} = join('', @f);
	}

	# H and block may contain arbitrary values, but check everything else
	grep { $_ == $s{alg} } (1,224,256,384,512,512224,512256) or return;
	length($s{H}) == ($s{alg} <= 256 ? 64 : 128) or return;
	length($s{block}) == ($s{alg} <= 256 ? 128 : 256) or return;
	{
		no integer;
		for (qw(blockcnt lenhh lenhl lenlh lenll)) {
			0 <= $s{$_} or return;
			$s{$_} <= 4294967295 or return;
		}
		$s{blockcnt} < ($s{alg} <= 256 ? 512 : 1024) or return;
	}

	my $self = _shaopen($s{alg}) or return;

	my @h = $s{H} =~ /(.{8})/g;
	for (@{$self->{H}}) {
		$_ = hex(shift @h);
		if ($self->{alg} > 256) {
			$_ = (($_ << 16) << 16) | hex(shift @h);
		}
	}

	$self->{blockcnt} = $s{blockcnt};
	$self->{block} = pack("H*", $s{block});
	$self->{block} = substr($self->{block},0,_BYTECNT($self->{blockcnt}));

	$self->{lenhh} = $s{lenhh};
	$self->{lenhl} = $s{lenhl};
	$self->{lenlh} = $s{lenlh};
	$self->{lenll} = $s{lenll};

	$self;
}

# ref. src/hmac.c from Digest::SHA

sub _hmacopen {
	my($alg, $key) = @_;
	my($self);
	$self->{isha} = _shaopen($alg) or return;
	$self->{osha} = _shaopen($alg) or return;
	croak $WSE unless utf8::downgrade($key, 1);
	if (length($key) > $self->{osha}->{blocksize} >> 3) {
		$self->{ksha} = _shaopen($alg) or return;
		_shawrite($key, length($key) << 3, $self->{ksha});
		_shafinish($self->{ksha});
		$key = _shadigest($self->{ksha});
	}
	$key .= chr(0x00)
		while length($key) < $self->{osha}->{blocksize} >> 3;
	my @k = unpack("C*", $key);
	for (@k) { $_ ^= 0x5c }
	_shawrite(pack("C*", @k), $self->{osha}->{blocksize}, $self->{osha});
	for (@k) { $_ ^= (0x5c ^ 0x36) }
	_shawrite(pack("C*", @k), $self->{isha}->{blocksize}, $self->{isha});
	$self;
}

sub _hmacWrite {
	my($bytestr_r, $bytecnt, $self) = @_;
	_shaWrite($bytestr_r, $bytecnt, $self->{isha});
}

sub _hmacfinish {
	my($self) = @_;
	_shafinish($self->{isha});
	_shawrite(_shadigest($self->{isha}),
			$self->{isha}->{digestlen} << 3, $self->{osha});
	_shafinish($self->{osha});
}

sub _hmacdigest { my($self) = @_; _shadigest($self->{osha}) }
sub _hmachex    { my($self) = @_; _shahex($self->{osha})    }
sub _hmacbase64 { my($self) = @_; _shabase64($self->{osha}) }

# SHA and HMAC-SHA functions

my @suffix_extern = ("", "_hex", "_base64");
my @suffix_intern = ("digest", "hex", "base64");

my($i, $alg);
for $alg (1, 224, 256, 384, 512, 512224, 512256) {
	for $i (0 .. 2) {
		my $fcn = 'sub sha' . $alg . $suffix_extern[$i] . ' {
			my $state = _shaopen(' . $alg . ') or return;
			for (@_) { _shaWrite(\$_, length($_), $state) }
			_shafinish($state);
			_sha' . $suffix_intern[$i] . '($state);
		}';
		eval($fcn);
		push(@EXPORT_OK, 'sha' . $alg . $suffix_extern[$i]);
		$fcn = 'sub hmac_sha' . $alg . $suffix_extern[$i] . ' {
			my $state = _hmacopen(' . $alg . ', pop(@_)) or return;
			for (@_) { _hmacWrite(\$_, length($_), $state) }
			_hmacfinish($state);
			_hmac' . $suffix_intern[$i] . '($state);
		}';
		eval($fcn);
		push(@EXPORT_OK, 'hmac_sha' . $alg . $suffix_extern[$i]);
	}
}

# OOP methods

sub hashsize  { my $self = shift; _shadsize($self) << 3 }
sub algorithm { my $self = shift; $self->{alg} }

sub add {
	my $self = shift;
	for (@_) { _shaWrite(\$_, length($_), $self) }
	$self;
}

sub digest {
	my $self = shift;
	_shafinish($self);
	my $rsp = _shadigest($self);
	_sharewind($self);
	$rsp;
}

sub hexdigest {
	my $self = shift;
	_shafinish($self);
	my $rsp = _shahex($self);
	_sharewind($self);
	$rsp;
}

sub b64digest {
	my $self = shift;
	_shafinish($self);
	my $rsp = _shabase64($self);
	_sharewind($self);
	$rsp;
}

sub new {
	my($class, $alg) = @_;
	$alg =~ s/\D+//g if defined $alg;
	if (ref($class)) {	# instance method
		if (!defined($alg) || ($alg == $class->algorithm)) {
			_sharewind($class);
			return($class);
		}
		my $self = _shaopen($alg) or return;
		return(_shacpy($class, $self));
	}
	$alg = 1 unless defined $alg;
	my $self = _shaopen($alg) or return;
	bless($self, $class);
	$self;
}

sub clone {
	my $self = shift;
	my $copy = _shadup($self) or return;
	bless($copy, ref($self));
}

BEGIN { *reset = \&new }

sub add_bits {
	my($self, $data, $nbits) = @_;
	unless (defined $nbits) {
		$nbits = length($data);
		$data = pack("B*", $data);
	}
	$nbits = length($data) * 8 if $nbits > length($data) * 8;
	_shawrite($data, $nbits, $self);
	return($self);
}

sub _bail {
	my $msg = shift;

	$msg .= ": $!";
	croak $msg;
}

sub _addfile {
	my ($self, $handle) = @_;

	my $n;
	my $buf = "";

	while (($n = read($handle, $buf, 4096))) {
		$self->add($buf);
	}
	_bail("Read failed") unless defined $n;

	$self;
}

{
	my $_can_T_filehandle;

	sub _istext {
		local *FH = shift;
		my $file = shift;

		if (! defined $_can_T_filehandle) {
			local $^W = 0;
			my $istext = eval { -T FH };
			$_can_T_filehandle = $@ ? 0 : 1;
			return $_can_T_filehandle ? $istext : -T $file;
		}
		return $_can_T_filehandle ? -T FH : -T $file;
	}
}

sub addfile {
	my ($self, $file, $mode) = @_;

	return(_addfile($self, $file)) unless ref(\$file) eq 'SCALAR';

	$mode = defined($mode) ? $mode : "";
	my ($binary, $UNIVERSAL, $BITS, $portable) =
		map { $_ eq $mode } ("b", "U", "0", "p");

		## Always interpret "-" to mean STDIN; otherwise use
		## sysopen to handle full range of POSIX file names

	local *FH;
	$file eq '-' and open(FH, '< -')
		or sysopen(FH, $file, O_RDONLY)
			or _bail('Open failed');

	if ($BITS) {
		my ($n, $buf) = (0, "");
		while (($n = read(FH, $buf, 4096))) {
			$buf =~ s/[^01]//g;
			$self->add_bits($buf);
		}
		_bail("Read failed") unless defined $n;
		close(FH);
		return($self);
	}

	binmode(FH) if $binary || $portable || $UNIVERSAL;
	if ($UNIVERSAL && _istext(*FH, $file)) {
		while (<FH>) {
			s/\015\012/\012/g;	# DOS/Windows
			s/\015/\012/g;		# early MacOS
			$self->add($_);
		}
	}
	elsif ($portable && _istext(*FH, $file)) {
		while (<FH>) {
			s/\015?\015\012/\012/g;
			s/\015/\012/g;
			$self->add($_);
		}
	}
	else { $self->_addfile(*FH) }
	close(FH);

	$self;
}

sub getstate {
	my $self = shift;

	return _shadump($self);
}

sub putstate {
	my $class = shift;
	my $state = shift;

	if (ref($class)) {	# instance method
		my $self = _shaload($state) or return;
		return(_shacpy($class, $self));
	}
	my $self = _shaload($state) or return;
	bless($self, $class);
	return($self);
}

sub dump {
	my $self = shift;
	my $file = shift;

	my $state = $self->getstate or return;
	$file = "-" if (!defined($file) || $file eq "");

	local *FH;
	open(FH, "> $file") or return;
	print FH $state;
	close(FH);

	return($self);
}

sub load {
	my $class = shift;
	my $file = shift;

	$file = "-" if (!defined($file) || $file eq "");
	
	local *FH;
	open(FH, "< $file") or return;
	my $str = join('', <FH>);
	close(FH);

	$class->putstate($str);
}

1;
1 core/checkpoint/lib
checkpoint.pl.sdoc
17 core/checkpoint/checkpoint.pl.sdoc
Checkpoint files.
You can break a long pipeline into a series of smaller files using
checkpointing, whose operator is `:`. The idea is to cache intermediate
results. A checkpoint specifies a file and a lambda whose output it should
capture.

sub checkpoint_create($$) {
  stee sni(@{$_[1]}), swfile "$_[0].part", siproc {sdecode};
  rename "$_[0].part", $_[0];
}

defoperator 'checkpoint', q{
  my ($file, $generator) = @_;
  sio; -r $file ? scat $file : checkpoint_create $file, $generator;
};

defshort '/:', pmap q{checkpoint_op $$_[0], $$_[1]}, pseq nefilename, pqfn '';
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
2 core/json/lib
json.pl.sdoc
extract.pl.sdoc
70 core/json/json.pl.sdoc
JSON parser/generator.
Perl has native JSON libraries available in CPAN, but we can't assume those are
installed locally. The pure-perl library is unusably slow, and even it isn't
always there. So I'm implementing an optimized pure-Perl library here to
address this. Note that this library doesn't parse all valid JSON, but it does
come close -- and I've never seen a real-world use case of JSON that it would
fail to parse.

use Scalar::Util qw/looks_like_number/;

our %json_unescapes =
  ("\\" => "\\", "/" => "/", "\"" => "\"", b => "\b", n => "\n", r => "\r",
   t => "\t");

sub json_unescape_one($) {$json_unescapes{$_[0]} || chr hex substr $_[0], 1}
sub json_unescape($) {
  my $x = substr $_[0], 1, -1;
  $x =~ s/\\(["\\\/bfnrt]|u[0-9a-fA-F]{4})/json_unescape_one $1/eg;
  $x;
}

Fully decode a string of JSON. Unless you need to extract everything, this is
probably the slowest option; targeted attribute extraction should be much
faster.

sub json_decode($) {
  local $_;
  my @v = [];
  for ($_[0] =~ /[][{}]|true|false|null|"(?:[^"\\]+|\\.)*"|[-+eE\d.]+/g) {
    if (/^[[{]$/) {
      push @v, [];
    } elsif (/^\]$/) {
      die "json_decode $_[0]: too many closing brackets" if @v < 2;
      push @{$v[-2]}, $v[-1];
      pop @v;
    } elsif (/^\}$/) {
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
  wantarray ? @$r : $$r[0];
}

Encode a string of JSON from a structured value. TODO: add the ability to
generate true/false values.

our %json_escapes = map {;$json_unescapes{$_} => $_} keys %json_unescapes;

sub json_escape($) {
  (my $x = $_[0]) =~ s/([\b\f\n\r\t"\\])/"\\" . $json_escapes{$1}/eg;
  "\"$x\"";
}

sub json_encode($) {
  local $_;
  my ($v) = @_;
  return "[" . join(',', map json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
  return "{" . join(',', map json_escape($_) . ":" . json_encode($$v{$_}),
                             sort keys %$v) . "}" if 'HASH' eq ref $v;
  looks_like_number $v ? $v : json_escape $v;
}
49 core/json/extract.pl.sdoc
Targeted extraction.
ni gives you ways to decode JSON, but you aren't likely to have data stored as
JSON objects in the middle of data pipelines. It's more of an archival format,
so the goal is to unpack stuff quickly. ni gives you a way of doing this that
is usually much faster than running the full decoder. (And also requires less
typing.)

The set of operations is basically this:

| .foo          # direct object key access (not very fast)
  ..foo         # multiple indirect object key access (fast-ish)
  :foo          # single indirect object key access (very fast)
  [0]           # array access (slow)
  []            # array flatten (slow)

Operations compose by juxtaposition: `.foo[0]:bar` means "give me the value of
every 'bar' key within the first element of the 'foo' field of the root
object".

Extracted values are flattened into a single array and returned. They're
optimized for strings, numeric, and true/false/null; you can return other
values, but it will be slower.

# TODO: replace all of this

use constant json_si_gen => gen q#
  (/"%k":\s*/g ? /\G("[^\\\\"]*")/            ? json_unescape $1
               : /\G("(?:[^\\\\"]+|\\\\.)*")/ ? json_unescape $1
               : /\G([^][{},]+)/              ? "" . $1
               : undef
               : undef) #;

sub json_extractor($) {
  my @pieces = split /\s*,\s*/, $_[0];
  die "ni: json_extractor is not really written yet"
    if grep !/^:\w+$/, @pieces;

  my @compiled = map json_si_gen->(k => qr/\Q$_\E/),
                 map sr($_, qr/^:/, ''), @pieces;
  join ',', @compiled;
}

defoperator destructure => q{
  ni::eval gen(q{binmode STDOUT, ":encoding(utf-8)";
                 while (<STDIN>) {print join("\t", %e), "\n"}})
            ->(e => json_extractor $_[0]);
};

defshort '/D', pmap q{destructure_op $_}, pgeneric_code;
1 core/net/lib
net.pl.sdoc
18 core/net/net.pl.sdoc
Networking stuff.
SSH tunneling to other hosts. Allows you to run a ni lambda elsewhere. ni does
not need to be installed on the remote system, nor does its filesystem need to
be writable.

defoperator ssh => q{
  my ($host, $lambda) = @_;
  my ($stdin, @exec) = sni_exec_list @$lambda;
  my $fh = siproc {exec 'ssh', $host, shell_quote @exec};
  safewrite $fh, $stdin;
  sforward \*STDIN, $fh;
  close $fh;
  $fh->await;
};

use constant ssh_host => prc '[^][/,]+';

defshort '/s', pmap q{ssh_op $$_[0], $$_[1]}, pseq ssh_host, pqfn '';
1 core/col/lib
col.pl.sdoc
99 core/col/col.pl.sdoc
Column manipulation operators.
In root context, ni interprets columns as being tab-delimited.

Column selection.
Normally perl is fast at text manipulation, but on most UNIX systems
`/usr/bin/cut` is at least an order of magnitude faster. We can use it if the
column access order is strictly ascending and has no duplicates.

sub col_cut {
  my ($floor, $rest, @fs) = @_;
  exec 'cut', '-f', join ',', $rest ? (@fs, "$floor-") : @fs;
}

TODO optimization: limit the number of elements returned by split(). We can do
this because we have the full column list available, though it may be more
trouble than it's worth if we have to contend with stray tabs in the output.

use constant cols_gen =>
  gen q{@_ = split /\t/, $_, %limit; print join "\t", @_[%is]};

defoperator cols => q{
  my ($floor, @cs) = @_;
  my $asc = join('', @cs) eq join('', sort @cs);
  my %dup; ++$dup{$_} for @cs;
  return col_cut $floor + 1, scalar(grep $_ == -1, @cs), map $_ + 1, @cs
    if $asc && !grep $_ > 1, values %dup;
  exec 'perl', '-lne',
       cols_gen->(limit => $floor + 1,
                  is    => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cs);
};

our @col_alt = pmap q{cols_op @$_}, colspec;

defshort '/f', paltr @col_alt;

sub defcolalt($) {unshift @col_alt, $_[0]}

Column swapping.
This is such a common thing to do that it gets its own operator `x`. The idea
is that you're swapping the specified column(s) into the first N position(s).

defoperator colswap => q{
  my ($floor, @cs) = @_;
  my %cs; ++$cs{$_} for @cs;
  die "ni colswap: . doesn't make sense"    if grep $_ == -1, @cs;
  die "ni colswap: can't duplicate columns" if grep $_ > 1, values %cs;
  my $n = 0;
  my @cols = 0..$floor-1;
  swap $cols[$n++], $cols[$_] for @cs;
  exec 'perl', '-lne', cols_gen->(limit => $floor + 1,
                                  is    => join ',', @cols, "$floor..\$#_");
};

defshort '/x', pmap q{ref $_ ? colswap_op @$_ : colswap_op 2, 1}, popt colspec;

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

Juxtaposition.
You can juxtapose two data sources horizontally by using `w` for `with`.

defoperator with => q{
  my $fh = sni @_;
  my $l;
  while (<STDIN>) {
    chomp;
    return unless defined($l = <$fh>);
    print "$_\t$l";
  }
};

defshort '/w', pmap q{with_op @$_}, pqfn '';
1 core/row/lib
row.pl.sdoc
118 core/row/row.pl.sdoc
Row-level operations.
These reorder/drop/create entire rows without really looking at fields.

defoperator head => q{exec 'head', @_};
defoperator tail => q{exec 'tail', $_[0], join "", @_[1..$#_]};

defoperator row_every => q{$. % $_[0] || print while <STDIN>};
defoperator row_match => q{$\ = "\n"; chomp, /$_[0]/o && print while <STDIN>};
defoperator row_sample => q{
  srand($ENV{NI_SEED} || 42);
  $. = 0;
  while (<STDIN>) {
    print, $. -= -log(1 - rand()) / $_[0] if $. >= 0;
  }
};

defoperator row_cols_defined => q{
  my ($floor, @cs) = @_;
  my $limit = $floor + 1;
  my $line;
  line:
  while (defined($line = <STDIN>)) {
    chomp $line;
    $_ || next line for (split /\t/, $line, $limit)[@cs];
    print $line . "\n";
  }
};

our @row_alt = (
  pmap(q{tail_op '-n', '',  $_},       pn 1, prx '\+', integer),
  pmap(q{tail_op '-n', '+', ($_ + 1)}, pn 1, prx '-',  integer),
  pmap(q{row_every_op  $_},            pn 1, prx 'x',  number),
  pmap(q{row_match_op  $_},            pn 1, prx '/',  regex),
  pmap(q{row_sample_op $_},                  prx '\.\d+'),
  pmap(q{head_op '-n', $_},            integer),
  pmap(q{row_cols_defined_op @$_},     colspec_fixed));

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
  -     reverse (I would use 'r', but it conflicts with the row operator)

use constant sortspec => prep pseq colspec1, popt prx '[-gn]+';

sub sort_args {'-t', "\t",
               map {my $i = $$_[0] + 1;
                    (my $m = defined $$_[1] ? $$_[1] : '') =~ s/-/r/g;
                    ('-k', "$i$m,$i")} @_}

Compatibility detection.
GNU coreutils sort supports some useful options like `--buffer-size` and
`--compress-program`. We should use these if they exist because they can make a
huge difference when processing large datasets.

Note that we localize compatibility detection down to the operator -- we don't
do it system-wide or at parse time. The reason is that parameterized operators
can be moved, potentially across machines; this really is the only way to do it
reliably.

sub sort_supports(@) {
  my $args = shell_quote @_;
  my $p    = siproc {sh "sort $args >/dev/null 2>&1"};
  close $p;
  return !$p->await;
}

sub sort_extra_args(@) {
  my @r;
  sort_supports @r, $_ and push @r, $_ for @_;
  @r;
}

defoperator row_sort => q{
  # TODO: support customization
  exec 'sort', sort_extra_args('--compress-program=gzip',
                               '--buffer-size=64M',
                               '--parallel=4'), @_};

defshort '/g', pmap q{row_sort_op        sort_args @$_}, sortspec;
defshort '/o', pmap q{row_sort_op '-n',  sort_args @$_}, sortspec;
defshort '/O', pmap q{row_sort_op '-rn', sort_args @$_}, sortspec;

Counting.
Sorted and unsorted streaming counts.

defoperator count => q{
  my ($n, $last) = (0, undef);
  while (<STDIN>) {
    if ($_ ne $last) {
      print "$n\t$last" if defined $last;
      $n = 0;
      $last = $_;
    }
    ++$n;
  }
  print "$n\t$last" if defined $last;
};

defoperator uniq => q{exec 'uniq'};

defshort '/c', pmap q{count_op}, pnone;
defshort '/u', pmap q{uniq_op},  pnone;
2 core/cell/lib
murmurhash.pl.sdoc
cell.pl.sdoc
28 core/cell/murmurhash.pl.sdoc
Pure-Perl MurmurHash3_32 implementation.
This is used by some intification operators and based on the Wikipedia
implementation. It's limited to 32 bits because otherwise ni will fail on 32-bit
machines.

use constant murmur_c1 => 0xcc9e2d51;
use constant murmur_c2 => 0x1b873593;
use constant murmur_n  => 0xe6546b64;

sub murmurhash3($;$) {
  use integer;
  local $_;
  my $h = $_[1] || 0;

  for (unpack 'L*', $_[0]) {
    $_ *= murmur_c1;
    $h ^= ($_ << 15 | $_ >> 17 & 0x7fff) * murmur_c2 & 0xffffffff;
    $h  = ($h << 13 | $h >> 19 & 0x1fff) * 5 + murmur_n;
  }

  my ($r) = unpack 'L<', substr($_[0], ~3 & length $_[0]) . "\0\0\0\0";
  $r *= murmur_c1;
  $h ^= ($r << 15 | $r >> 17 & 0x7fff) * murmur_c2 & 0xffffffff ^ length $_[0];
  $h &= 0xffffffff;
  $h  = ($h ^ $h >> 16) * 0x85ebca6b & 0xffffffff;
  $h  = ($h ^ $h >> 13) * 0xc2b2ae35 & 0xffffffff;
  return $h ^ $h >> 16;
}
115 core/cell/cell.pl.sdoc
Cell-level operators.
Cell-specific transformations that are often much shorter than the equivalent Perl
code. They're also optimized for performance.

defcontext 'cell';
defshort '/,', pqfn 'cell';

use constant cellspec       => pmap q{$_ || [1, 0]}, popt colspec;
use constant cellspec_fixed => pmap q{$_ || [1, 0]}, popt colspec_fixed;

Codegen.
Most of these have exactly the same format and take a column spec.

use constant cell_op_gen => gen q{
  my ($cs, %args) = @_;
  my ($floor, @cols) = @$cs;
  my $limit = $floor + 1;
  %begin;
  while (<STDIN>) {
    chomp;
    my @xs = split /\t/, $_, $limit;
    %each_line
    %each for @cols;
    print join("\t", @xs) . "\n";
  }
  %end
};

sub cell_eval($@) {
  my ($h, @args) = @_;
  fn(cell_op_gen->(%$h))->(@args);
}

Intification.
Strategies to turn each distinct entry into a number. Particularly useful in a
plotting context.

defoperator intify_compact => q{
  cell_eval {args  => 'undef',
             begin => 'my %ids; my $n = 0',
             each  => '$xs[$_] = ($ids{$xs[$_]} ||= ++$n)'}, @_;
};

defoperator intify_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = murmurhash3 $xs[$_], $seed'}, @_;
};

defshort 'cell/z', pmap q{intify_compact_op $_},  cellspec_fixed;
defshort 'cell/h', pmap q{intify_hash_op    @$_}, pseq cellspec_fixed, popt integer;

Numerical transformations.
Trivial stuff that applies to each cell individually.

use constant log_base => pmap q{$_ || exp 1}, popt number;

defoperator cell_log => q{
  my ($cs, $base) = @_;
  my $lb = 1 / log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = log(max 1e-16, \$xs[\$_]) * $lb"}, $cs;
};

defoperator cell_exp => q{
  my ($cs, $base) = @_;
  my $eb = log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = $eb * exp \$xs[\$_]"}, $cs;
};

defshort 'cell/l', pmap q{cell_log_op @$_}, pseq cellspec_fixed, log_base;
defshort 'cell/e', pmap q{cell_exp_op @$_}, pseq cellspec_fixed, log_base;

use constant jitter_bias => pmap q{dor $_, 0}, popt number;
use constant jitter_mag  => pmap q{$_ || 1},   palt pmap(q{0.9}, prx ','),
                                                    popt pc number;

defoperator jitter_uniform => q{
  my ($cs, $mag, $bias) = @_;
  my $adjust = $bias - $mag / 2;
  cell_eval {args => 'undef', each => "\$xs[\$_] += rand() * $mag + $adjust"}, $cs;
};

defshort 'cell/j', pmap q{jitter_uniform_op @$_},
                   pseq cellspec_fixed, jitter_mag, jitter_bias;

use constant quant_spec => pmap q{$_ || 1}, popt number;

defoperator quantize => q{
  my ($cs, $q) = @_;
  my $iq = 1 / $q;
  cell_eval {args => 'undef',
             each => "\$xs[\$_] = $q * int(0.5 + $iq * \$xs[\$_])"}, $cs;
};

defshort 'cell/q', pmap q{quantize_op @$_}, pseq cellspec_fixed, quant_spec;

Streaming numeric transformations.
Sum, delta, average, variance, entropy, etc. Arguably these are column operators and
not cell operators, but in practice you tend to use them in the same context as
things like log scaling.

defoperator col_sum => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols',
             each  => '$xs[$_] = $ns[$_] += $xs[$_]'}, @_;
};

defoperator col_delta => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols',
             each  => '$xs[$_] -= $ns[$_], $ns[$_] += $xs[$_]'}, @_;
};

defshort 'cell/s', pmap q{col_sum_op   $_}, cellspec_fixed;
defshort 'cell/d', pmap q{col_delta_op $_}, cellspec_fixed;
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

sub ceval {eval $_[0]; die "error evaluating $_[0]: $@" if $@}

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
66 core/pl/stream.pm.sdoc
Perl stream-related functions.
Utilities to parse and emit streams of data. Handles the following use cases:

| $ ni n:10p'a + a'             # emit single value
  $ ni n:10p'a, a * a'          # emit multiple values vertically
  $ ni n:10p'r a, a * a'        # emit multiple values horizontally

The 'pr' function can bypass split /\t/, which is useful in high-throughput
situations. For example:

| $ ni n:10p'pr "$_\tfoo"'      # append a new field without splitting

Lowercase letters followed by underscores are field-extractors that can take an
array of lines and return an array of field values. These are useful in
conjunction with the line-reading functions `rw`, `ru`, and `re`.

our @q;
our @F;

sub rl():lvalue   {chomp($_ = @q ? shift @q : <STDIN>); @F = split /\t/; $_}
sub pl($):lvalue  {chomp, push @q, $_ until !defined($_ = <STDIN>) || @q >= $_[0]; @q[0..$_[0]-1]}
sub F_(@):lvalue  {@_ ? @F[@_] : @F}
sub FM()          {$#F}
sub FR($):lvalue  {@F[$_[0]..$#F]}
sub r(@)          {my $l = join "\t", @_; print $l, "\n"; ()}
BEGIN {ceval sprintf 'sub %s():lvalue {@F[%d]}', $_, ord($_) - 97 for 'a'..'l';
       ceval sprintf 'sub %s_ {local $_; map((split /\t/)[%d], @_)}',
                     $_, ord($_) - 97 for 'a'..'l'}

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

sub rw(&) {my @r = ($_); push @r, $_ while  defined rl && &{$_[0]}; push @q, $_ if defined $_; @r}
sub ru(&) {my @r = ($_); push @r, $_ until !defined rl || &{$_[0]}; push @q, $_ if defined $_; @r}
sub re(&) {my ($f, $i) = ($_[0], &{$_[0]}); rw {&$f eq $i}}
BEGIN {ceval sprintf 'sub re%s() {re {%s}}', $_, $_ for 'a'..'l'}

Streaming aggregations.
These functions are like the ones above, but designed to work in constant
space:

| se<column>: streaming reduce while column is equal
  sr: streaming reduce all data

sub se(&$@) {my ($f, $e, @xs) = @_; my $k = &$e;
             @xs = &$f(@xs), rl while defined and &$e eq $k;
             push @q, $_ if defined; @xs}
BEGIN {ceval sprintf 'sub se%s(&@) {my ($f, @xs) = @_; se {&$f(@_)} \&%s, @xs}',
                     $_, $_ for 'a'..'l'}

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
97 core/pl/pl.pl.sdoc
Perl parse element.
A way to figure out where some Perl code ends, in most cases. This works
because appending closing brackets to valid Perl code will always make it
invalid. The same property holds across most code-to-code functions. This is
how ni figures out whether a closing bracket is a lambda-terminator or a part
of some row mapping code.

sub syntax_check($$) {
  my ($check_cmd, $code) = @_;
  my $fh = siproc {sh "$check_cmd >/dev/null 2>&1"};
  safewrite $fh, $code;
  close $fh;
  $fh->await;
}

We need to explicitly disable any BEGIN{} blocks to avoid executing side
effects. We can guarantee that nothing will run (beyond `use` statements, which
we assume are safe) by removing any occurrences of the string `BEGIN` and
replacing them with something syntactically equivalent but less volatile -- in
this case, `END`.

c
BEGIN {
defparser 'plcode', '$', sub {
  return @_[1..$#_] unless $_[1] =~ /\]$/;
  my ($self, $code, @xs) = @_;
  my $safecode      = $code;
  my $begin_warning = $safecode =~ s/BEGIN/ END /g;
  my $codegen       = $$self[1];
  my $status        = 0;
  my $x             = '';
  $x .= ']' while $status = syntax_check 'perl -c -', &$codegen($safecode)
                  and ($safecode =~ s/\]$//, $code =~ s/\]$//);

  die <<EOF if $status;
ni: failed to get closing bracket count for perl code "$code$x", possibly
    because BEGIN-block metaprogramming is disabled when ni tries to figure
    this out. To avoid this, make sure the shell argument containing your code
    ends with something that isn't a closing bracket; e.g:

    p'[[some code]]'            # this may fail due to bracket inference
    p'[[some code]] '           # this works by bypassing it
    [p'[some code] ' ]          # this works for ni lambdas
EOF

  length $x ? ($code, $x, @xs) : ($code, @xs);
};
}

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

use constant perl_prefix =>
  join "\n", @self{qw| core/pl/util.pm
                       core/pl/math.pm
                       core/pl/stream.pm
                       core/gen/gen.pl
                       core/json/json.pl
                       core/pl/reducers.pm |};

sub stdin_to_perl($) {
  move_fd 0, 3;
  safewrite siproc {exec 'perl', '-'}, $_[0];
}

sub perl_code($$) {perl_mapgen->(prefix => perl_prefix,
                                 body   => $_[0],
                                 each   => $_[1])}

sub perl_mapper($)  {perl_code $_[0], 'print "$_\n" for row'}
sub perl_grepper($) {perl_code $_[0], 'print "$_\n" if row'}

defoperator perl_mapper  => q{stdin_to_perl perl_mapper  $_[0]};
defoperator perl_grepper => q{stdin_to_perl perl_grepper $_[0]};

our @perl_alt = pmap q{perl_mapper_op $_}, pplcode \&perl_mapper;

defshort '/p', paltr @perl_alt;

sub defperlalt($) {unshift @perl_alt, $_[0]}

defrowalt pmap q{perl_grepper_op $_},
          pn 1, prx 'p', pplcode \&perl_grepper;
2 core/rb/lib
prefix.rb
rb.pl.sdoc
96 core/rb/prefix.rb
# ni ruby driver prefix
# This is loaded prior to the short codegen segment in rb.pl.sdoc.

$have_json = true
begin
  require 'json'
rescue ScriptError
  $have_json = false
end

# Portability stuff.
# Old versions of Ruby have "a"[0] == 97, new versions have "a"[0] == "a". This
# makes it always safe to say "a"[0].ord.
class Numeric
  def ord; self; end
end

# Add to_proc conversion to symbols, which makes it possible to write
# map(&:foo).
class Symbol
  def to_proc
    x = self
    proc {|v| v.send(x)}
  end
end

class Line
  attr_reader :fields

  def initialize s
    @fields = s.split /\t/
  end

  def [] *x
    fields[*x]
  end

  def to_s
    fields.join "\t"
  end
end

# Some metaprogramming to get letter accessors
Line.class_eval do
  ('a'..'q').each do |l|
    index = l[0].ord - 97
    define_method    l   .to_sym, proc {fields[index]}
    define_method "#{l}s".to_sym, proc {fields[index].to_s}
    define_method "#{l}i".to_sym, proc {fields[index].to_i}
    define_method "#{l}f".to_sym, proc {fields[index].to_f}
  end
end

Enumerable.class_eval do
  ('a'..'q').each do |l|
    index = l[0].ord - 97
    define_method    l   .to_sym, proc {map {|x| x.fields[index]}}
    define_method "#{l}s".to_sym, proc {map {|x| fields[index].to_s}}
    define_method "#{l}i".to_sym, proc {map {|x| fields[index].to_i}}
    define_method "#{l}f".to_sym, proc {map {|x| fields[index].to_f}}
  end
end

def r *xs
  xs.join("\t")
end

# Readahead support
$q = []
$l = nil

def next_line
  return $q.shift unless $q.empty?
  Line.new($in.readline.chomp!) rescue nil
end

def rw
  r = [$l]
  l = nil
  r << l while l = next_line and yield l
  $q << l if l
  r
end

def ru
  r = [$l]
  l = nil
  r << l until !(l = next_line) or yield l
  $q << l if l
  r
end

def re &f
  v = f.call $l
  rw {|l| f.call(l) == v}
end
74 core/rb/rb.pl.sdoc
Ruby code element.
This works just like the Perl code parser but is slightly less involved because
there's no `BEGIN/END` substitution. We also don't need to take a code
transform because no amount of wrapping will change whether an expression can
be parsed.

c
BEGIN {
defparser 'rbcode', '', sub {
  return @_[1..$#_] unless $_[1] =~ /\]$/;
  my ($self, $code, @xs) = @_;
  my ($x, $status) = ('', 0);
  $x .= ']' while $status = syntax_check 'ruby -c -', $code and $code =~ s/\]$//;
  die <<EOF if $status;
ni: failed to get closing bracket count for ruby code "$code$x"; this means
    your code has a syntax error.
EOF
  length $x ? ($code, $x, @xs) : ($code, @xs);
};
}

Ruby wrapper.

use constant ruby_mapgen => gen q{
  %prefix
  STDIN.close
  $in = IO.new(3)
  class Line
    def row
      %body
    end
  end

  def map_mode! x
    if x.is_a? Enumerable
      x.each do |v|
        v = r *v if v.is_a? Enumerable
        puts v
      end
    elsif !x.nil?
      puts x
    end
  end

  while $l = next_line
    x = $l.row
    %each
  end
};

use constant ruby_prefix => join "\n", @self{qw| core/rb/prefix.rb |};

sub stdin_to_ruby($) {
  move_fd 0, 3;
  safewrite siproc {exec 'ruby', '-'}, $_[0];
}

sub ruby_code($$) {ruby_mapgen->(prefix => ruby_prefix,
                                 body   => $_[0],
                                 each   => $_[1])}

sub ruby_mapper($)  {ruby_code $_[0], 'map_mode! x'}
sub ruby_grepper($) {ruby_code $_[0], 'puts $l if x'}

defoperator ruby_mapper  => q{stdin_to_ruby ruby_mapper  $_[0]};
defoperator ruby_grepper => q{stdin_to_ruby ruby_grepper $_[0]};

our @ruby_alt = pmap q{ruby_mapper_op $_}, prbcode;

defshort '/m', paltr @ruby_alt;

sub defrubyalt($) {unshift @ruby_alt, $_[0]}

defrowalt pmap q{perl_grepper_op $_}, pn 1, prx 'm', prbcode;
2 core/lisp/lib
prefix.lisp
lisp.pl.sdoc
166 core/lisp/prefix.lisp
;;;;;;;;
;; NB: don't delete the line of semicolons above; SBCL throws away the first few
;; bytes on CentOS.
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
  my ($code) = @_;
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
1 core/sql/lib
sql.pl.sdoc
132 core/sql/sql.pl.sdoc
SQL parsing context.
Translates ni CLI grammar to a SELECT query. This is a little interesting
because SQL has a weird structure to it; to help with this I've also got a
'sqlgen' abstraction that figures out when we need to drop into a subquery.

sub sqlgen($) {bless {from => $_[0]}, 'ni::sqlgen'}

sub ni::sqlgen::render {
  local $_;
  my ($self) = @_;
  return $$self{from} if 1 == keys %$self;

  my $select = ni::dor $$self{select}, '*';
  my @others;

  for (qw/from where order_by group_by limit union intersect except
          inner_join left_join right_join full_join natural_join/) {
    next unless exists $$self{$_};
    (my $k = $_) =~ y/a-z_/A-Z /;
    push @others, "$k $$self{$_}";
  }

  ni::gen('SELECT %distinct %stuff %others')
       ->(stuff    => $select,
          distinct => $$self{uniq} ? 'DISTINCT' : '',
          others   => join ' ', @others);
}

sub ni::sqlgen::modify_where {join ' AND ', @_}

sub ni::sqlgen::modify {
  my ($self, %kvs) = @_;
  while (my ($k, $v) = each %kvs) {
    if (exists $$self{$k}) {
      if (exists ${'ni::sqlgen::'}{"modify_$k"}) {
        $v = &{"ni::sqlgen::modify_$k"}($$self{$k}, $v);
      } else {
        $self = ni::sqlgen "($self->render)";
      }
    }
    $$self{$k} = $v;
  }
  $self;
}

sub ni::sqlgen::map        {$_[0]->modify(select => $_[1])}
sub ni::sqlgen::filter     {$_[0]->modify(where =>  $_[1])}
sub ni::sqlgen::take       {$_[0]->modify(limit =>  $_[1])}
sub ni::sqlgen::sample     {$_[0]->modify(where =>  "random() < $_[1]")}

sub ni::sqlgen::ijoin      {$_[0]->modify(join => 1, inner_join   => $_[1])}
sub ni::sqlgen::ljoin      {$_[0]->modify(join => 1, left_join    => $_[1])}
sub ni::sqlgen::rjoin      {$_[0]->modify(join => 1, right_join   => $_[1])}
sub ni::sqlgen::njoin      {$_[0]->modify(join => 1, natural_join => $_[1])}

sub ni::sqlgen::order_by   {$_[0]->modify(order_by => $_[1])}

sub ni::sqlgen::uniq       {${$_[0]}{uniq} = 1; $_[0]}

sub ni::sqlgen::union      {$_[0]->modify(setop => 1, union     => $_[1])}
sub ni::sqlgen::intersect  {$_[0]->modify(setop => 1, intersect => $_[1])}
sub ni::sqlgen::difference {$_[0]->modify(setop => 1, except    => $_[1])}

SQL code parse element.
Counts brackets outside quoted strings.

use constant sqlcode => pgeneric_code;

Code compilation.
Parser elements can generate one of two things: [method, @args] or
{%modifications}. Compiling code is just starting with a SQL context and
left-reducing method calls.

sub sql_compile {
  local $_;
  my ($g, @ms) = @_;
  for (@ms) {
    if (ref($_) eq 'ARRAY') {
      my ($m, @args) = @$_;
      $g = $g->$m(@args);
    } else {
      $g = $g->modify(%$_);
    }
  }
  $g->render;
}

SQL operator mapping.
For the most part we model SQL operations the same way that we address Spark
RDDs, though the mnemonics are a mix of ni and SQL abbreviations.

defcontext 'sql';

use constant sql_table => pmap q{sqlgen $_}, prc '^[^][]*';

our $sql_query = pmap q{sql_compile $$_[0], @{$$_[1]}},
                 pseq sql_table, popt pqfn 'sql';

our @sql_row_alt = (
  pmap(q{['take',   $_]}, integer),
  pmap(q{['filter', $_]}, sqlcode),
);

our @sql_join_alt = (
  pmap(q{['ljoin', $_]}, pn 1, prx 'L', $sql_query),
  pmap(q{['rjoin', $_]}, pn 1, prx 'R', $sql_query),
  pmap(q{['njoin', $_]}, pn 1, prx 'N', $sql_query),
  pmap(q{['ijoin', $_]}, $sql_query),
);

defshort 'sql/m', pmap q{['map', $_]}, sqlcode;
defshort 'sql/r', paltr @sql_row_alt;
defshort 'sql/j', paltr @sql_join_alt;
defshort 'sql/u', pk ['uniq'];

defshort 'sql/g', pmap q{['order_by', $_]},        sqlcode;
defshort 'sql/o', pmap q{['order_by', "$_ ASC"]},  sqlcode;
defshort 'sql/O', pmap q{['order_by', "$_ DESC"]}, sqlcode;

defshort 'sql/+', pmap q{['union',      $_]}, $sql_query;
defshort 'sql/*', pmap q{['intersect',  $_]}, $sql_query;
defshort 'sql/-', pmap q{['difference', $_]}, $sql_query;

Global operator.
SQL stuff is accessed using Q, which delegates to a sub-parser that handles
configuration/connections.

our %sql_profiles;

defshort '/Q', pdspr %sql_profiles;

sub defsqlprofile($$) {$sql_profiles{$_[0]} = $_[1]}
1 core/python/lib
python.pl.sdoc
46 core/python/python.pl.sdoc
Python stuff.
A context for processing stuff in Python, as well as various functions to
handle the peculiarities of Python code.

Indentation fixing.
This is useful in any context where code is artificially indented, e.g. when
you've got a multiline quotation and the first line appears outdented because
the quote opener has taken up space:

| my $python_code = q{import numpy as np
                      print np};
  # -----------------| <- this indentation is misleading

In this case, we want to have the second line indented at zero, not at the
apparent indentation. The pydent function does this transformation for you, and
correctly handles Python block constructs:

| my $python_code = pydent q{if True:
                               print "well that's good"};

sub pydent($) {
  my @lines   = split /\n/, $_[0];
  my @indents = map length(sr $_, qr/\S.*$/, ''), @lines;
  my $indent  = @lines > 1 ? $indents[1] - $indents[0] : 0;

  $indent = min $indent - 1, @indents[2..$#indents]
    if $lines[0] =~ /:\s*(#.*)?$/ && @lines > 2;

  my $spaces = ' ' x $indent;
  $lines[$_] =~ s/^$spaces// for 1..$#lines;
  join "\n", @lines;
}

sub indent($;$) {
  my ($code, $indent) = (@_, 2);
  join "\n", map ' ' x $indent . $_, split /\n/, $code;
}

sub pyquote($) {"'" . sgr(sgr($_[0], qr/\\/, '\\\\'), qr/'/, '\\\'') . "'"}

Python code parse element.
Counts brackets, excluding those inside quoted strings. This is more efficient
and less accurate than Ruby/Perl, but the upside is that errors are not
particularly common.

use constant pycode => pmap q{pydent $_}, generic_code;
1 core/gnuplot/lib
gnuplot.pl.sdoc
12 core/gnuplot/gnuplot.pl.sdoc
Gnuplot interop.
An operator that tees output to a gnuplot process.

defcontext 'gnuplot';
defshort 'gnuplot/d', pk 'plot "-" with dots';

defoperator gnuplot => q{
  my ($args) = @_;
  exec 'gnuplot', '-persist', '-e', $args;
};

defshort '/G', $contexts{gnuplot};
2 core/http/lib
ws.pm.sdoc
http.pl.sdoc
32 core/http/ws.pm.sdoc
WebSocket encoding functions.
We just encode text messages; no binary or other protocols are defined yet.

c
BEGIN {
  eval 'use Digest::SHA qw/sha1_base64/';
  load 'core/deps/sha1.pm',
    Digest::SHA::PurePerl->import(qw/sha1_base64/) if $@;
}

use constant ws_guid => '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

sub ws_header($) {
  my ($client_key) = $_[0] =~ /Sec-WebSocket-Key:\s*(\S+)/i;
  my ($protocol)   = $_[0] =~ /Sec-WebSocket-Protocol:\s*(\S+)/i;
  my $hash = sha1_base64 $client_key . ws_guid;
  join "\n", "HTTP/1.1 101 Switching Protocols",
             "Upgrade: websocket",
             "Connection: upgrade",
             "Sec-WebSocket-Accept: $hash=",
             "Sec-WebSocket-Protocol: $protocol",
             '', '';
}

sub ws_length_encode($) {
  my ($n) = @_;
  return pack 'C',        $n if $n < 126;
  return pack 'Cn',  126, $n if $n < 65536;
  return pack 'CNN', 127, $n >> 32, $n;
}

sub ws_encode($) {"\x81" . ws_length_encode(length $_[0]) . $_[0]}
65 core/http/http.pl.sdoc
HTTP server.
A very simple HTTP server that can be used to serve a stream's contents. This is used
by other libraries to serve things like JSPlot.

use Socket;
use Errno qw/EINTR/;

sub http_reply($$$%) {
  my ($fh, $code, $body, %headers) = @_;
  $fh->print(join "\n", "HTTP/1.1 $code NI",
                        map("$_: $headers{$_}", sort keys %headers),
                        "Content-Length: " . length($body),
                        '',
                        $body);
}

sub uri_decode(@) {(my $u = $_[0]) =~ s/%([0-9A-Fa-f]{2})/chr hex $1/eg; $u}

sub safeaccept($$) {
  my $r;
  1 until $r = accept $_[0], $_[1] or !$!{EINTR};
  $r;
}

sub http($$) {
  my (undef, $f) = @_;
  my ($server, $client);
  $f = fn $f;

  socket $server, PF_INET, SOCK_STREAM, getprotobyname 'tcp'
    or die "ni http: socket() failed: $!";
  setsockopt $server, SOL_SOCKET, SO_REUSEADDR, pack 'l', 1
    or die "ni http: setsockopt() failed: $!";

  ++$_[0] > 65535 && die "ni http: bind() failed: $!"
    until bind $server, sockaddr_in $_[0], INADDR_LOOPBACK;

  listen $server, SOMAXCONN or die "ni http: listen() failed: $!";

  &$f;
  for (; $_ = '', safeaccept $client, $server; close $client) {
    next if cfork;
    my $n = 1;
    close $server;
    $n = saferead $client, $_, 8192, length until /\r?\n\r?\n/ || !$n;
    &$f(uri_decode(/^GET (.*) HTTP\//), $_, $client);
    exit;
  }
}

Websocket operators.
This is used to stream a data source to the browser. See `core/jsplot` for details.

defoperator http_websocket_encode => q{
  load 'core/http/ws.pm';
  safewrite \*STDOUT, ws_encode($_) while <STDIN>;
};

defoperator http_websocket_encode_batch => q{
  load 'core/http/ws.pm';
  safewrite \*STDOUT, ws_encode($_) while saferead \*STDIN, $_, $_[0] || 8192;
};

deflong '/http/wse',       pmap q{http_websocket_encode_op},                prc '--http/wse$';
deflong '/http/wse-batch', pmap q{http_websocket_encode_batch_op $_}, pn 1, prc '--http/wse-batch$', popt integer;
2 core/caterwaul/lib
caterwaul.min.js
caterwaul.std.min.js
138 core/caterwaul/caterwaul.min.js
(function(f){return f(f)})(function(initializer,key,undefined){(function(f){return f(f)})(function(initializer){var calls_init=function(){var f=function(){return f.init.apply(f,arguments)
};return f},original_global=typeof caterwaul==="undefined"?undefined:caterwaul,caterwaul_global=calls_init();caterwaul_global.deglobalize=function(){caterwaul=original_global;
return caterwaul_global};caterwaul_global.core_initializer=initializer;caterwaul_global.context=this;caterwaul_global.merge=(function(o){for(var k in o){if(o.hasOwnProperty(k)){return true
}}})({toString:true})?function(o){for(var i=1,l=arguments.length,_;i<l;++i){if(_=arguments[i]){for(var k in _){if(Object.prototype.hasOwnProperty.call(_,k)){o[k]=_[k]
}}}}return o}:function(o){for(var i=1,l=arguments.length,_;i<l;++i){if(_=arguments[i]){for(var k in _){if(Object.prototype.hasOwnProperty.call(_,k)){o[k]=_[k]}}if(_.toString&&!/\[native code\]/.test(_.toString.toString())){o.toString=_.toString
}}}return o},caterwaul_global.modules=[];caterwaul_global.module=function(name,transform,f){if(arguments.length===1){return caterwaul_global[name+"_initializer"]
}name+"_initializer" in caterwaul_global||caterwaul_global.modules.push(name);f||(f=transform,transform=null);(caterwaul_global[name+"_initializer"]=transform?caterwaul_global(transform)(f):f)(caterwaul_global);
return caterwaul_global};return caterwaul=caterwaul_global});var qw=function(x){return x.split(/\s+/)},se=function(x,f){return f&&f.call(x,x)||x},fail=function(m){throw new Error(m)
},unique=key||(function(){for(var xs=[],d="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$_",i=21,n;i>=0;--i){xs.push(d.charAt(Math.random()*64>>>0))
}return xs.join("")})(),gensym=(function(c){return function(name){return[name||"",(++c).toString(36),unique].join("_")}})(0),is_gensym=function(s){return s.substr(s.length-22)===unique
},bind=function(f,t){return function(){return f.apply(t,arguments)}},map=function(f,xs){for(var i=0,ys=[],l=xs.length;i<l;++i){ys.push(f(xs[i],i))}return ys},rmap=function(f,xs){return map(function(x){return x instanceof Array?rmap(f,x):f(x)
})},hash=function(s){for(var i=0,xs=qw(s),o={},l=xs.length;i<l;++i){o[xs[i]]=true}return annotate_keys(o)},max_length_key=gensym("hash"),annotate_keys=function(o){var max=0;
for(var k in o){own.call(o,k)&&(max=k.length>max?k.length:max)}o[max_length_key]=max;return o},has=function(o,p){return p!=null&&!(p.length>o[max_length_key])&&own.call(o,p)
},own=Object.prototype.hasOwnProperty,caterwaul_global=caterwaul.merge(caterwaul,{map:map,rmap:rmap,gensym:gensym,is_gensym:is_gensym,gensym_entropy:function(){return unique
}}),lex_op=hash(". new ++ -- u++ u-- u+ u- typeof void u~ u! ! * / % + - << >> >>> < > <= >= instanceof in == != === !== & ^ | && || ? = += -= *= /= %= &= |= ^= <<= >>= >>>= : , return throw case var const break continue else u; ;"),lex_table=function(s){for(var i=0,xs=[false];
i<8;++i){xs.push.apply(xs,xs)}for(var i=0,l=s.length;i<l;++i){xs[s.charCodeAt(i)]=true}return xs},lex_float=lex_table(".0123456789"),lex_decimal=lex_table("0123456789"),lex_integer=lex_table("0123456789abcdefABCDEFx"),lex_exp=lex_table("eE"),lex_space=lex_table(" \n\r\t"),lex_bracket=lex_table("()[]{}?:"),lex_opener=lex_table("([{?:"),lex_punct=lex_table("+-*/%&|^!~=<>?:;.,"),lex_eol=lex_table("\n\r"),lex_regexp_suffix=lex_table("gims"),lex_quote=lex_table("'\"/"),lex_slash="/".charCodeAt(0),lex_zero="0".charCodeAt(0),lex_postfix_unary=hash("++ --"),lex_ident=lex_table("@$_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),lex_star="*".charCodeAt(0),lex_back="\\".charCodeAt(0),lex_x="x".charCodeAt(0),lex_dot=".".charCodeAt(0),lex_hash="#".charCodeAt(0),parse_reduce_order=map(hash,["function","( [ . [] ()","new delete void","u++ u-- ++ -- typeof u~ u! u+ u-","* / %","+ -","<< >> >>>","< > <= >= instanceof in","== != === !==","::",":::","&","^","|","&&","||","-> =>","case","? = += -= *= /= %= &= |= ^= <<= >>= >>>= &&= ||=",":",",","return throw break continue","var const","if else try catch finally for switch with while do",";"]),parse_associates_right=hash("= += -= *= /= %= &= ^= |= <<= >>= >>>= &&= ||= :: ::: -> => ~ ! new typeof void u+ u- -- ++ u-- u++ ? if else function try catch finally for switch case with while do"),parse_inverse_order=(function(xs){for(var o={},i=0,l=xs.length;
i<l;++i){for(var k in xs[i]){has(xs[i],k)&&(o[k]=i)}}return annotate_keys(o)})(parse_reduce_order),parse_index_forward=(function(rs){for(var xs=[],i=0,l=rs.length,_=null;
_=rs[i],xs[i]=true,i<l;++i){for(var k in _){if(has(_,k)&&(xs[i]=xs[i]&&!has(parse_associates_right,k))){break}}}return xs})(parse_reduce_order),parse_lr=hash("[] . () * / % + - << >> >>> < > <= >= instanceof in == != === !== & ^ | && || -> => = += -= *= /= %= &= |= ^= <<= >>= >>>= &&= ||= , : ;"),parse_r_until_block=annotate_keys({"function":2,"if":1,"do":1,"catch":1,"try":1,"for":1,"while":1,"with":1,"switch":1}),parse_accepts=annotate_keys({"if":"else","do":"while","catch":"finally","try":"catch"}),parse_invocation=hash("[] ()"),parse_r_optional=hash("return throw break continue else"),parse_r=hash("u+ u- u! u~ u++ u-- new typeof finally case var const void delete"),parse_block=hash("; {"),parse_invisible=hash("i;"),parse_l=hash("++ --"),parse_group=annotate_keys({"(":")","[":"]","{":"}","?":":"}),parse_ambiguous_group=hash("[ ("),parse_ternary=hash("?"),parse_not_a_value=hash("function if for while with catch void delete new typeof in instanceof"),parse_also_expression=hash("function"),parse_misleading_postfix=hash(":"),syntax_common=caterwaul_global.syntax_common={_replace:function(n){return(n.l=this.l)&&(this.l.r=n),(n.r=this.r)&&(this.r.l=n),this
},_append_to:function(n){return n&&n._append(this),this},_reparent:function(n){return this.p&&this.p[0]===this&&(this.p[0]=n),this},_fold_l:function(n){return this._append(this.l&&this.l._unlink(this)||empty)
},_append:function(n){return(this[this.length++]=n)&&(n.p=this),this},_fold_r:function(n){return this._append(this.r&&this.r._unlink(this)||empty)},_sibling:function(n){return n.p=this.p,(this.r=n).l=this
},_fold_lr:function(){return this._fold_l()._fold_r()},_fold_rr:function(){return this._fold_r()._fold_r()},_wrap:function(n){return n.p=this._replace(n).p,this._reparent(n),delete this.l,delete this.r,this._append_to(n)
},_unlink:function(n){return this.l&&(this.l.r=this.r),this.r&&(this.r.l=this.l),delete this.l,delete this.r,this._reparent(n)},pop:function(){return --this.length,this
},push:function(x){return this[this.length++]=caterwaul_global.syntax.promote(x||empty),this},id:function(){var id=gensym("id");return(this.id=function(){return id
})()},is_caterwaul_syntax:true,each:function(f){for(var i=0,l=this.length;i<l;++i){f(this[i],i)}return this},map:function(f){for(var n=new this.constructor(this),i=0,l=this.length;
i<l;++i){n.push(f(this[i],i)||this[i])}return n},reach:function(f){f(this);for(var i=0,l=this.length;i<l;++i){this[i].reach(f)}return this},rmap:function(f){var r=f(this);
return !r||r===this?this.map(function(n){return n.rmap(f)}):r===true?this:r.rmap===undefined?new this.constructor(r):r},peach:function(f){for(var i=0,l=this.length;
i<l;++i){this[i].peach(f)}f(this);return this},pmap:function(f){var t=this.map(function(n){return n.pmap(f)});return f(t)},clone:function(){return this.rmap(function(){return false
})},collect:function(p){var ns=[];this.reach(function(n){p(n)&&ns.push(n)});return ns},replace:function(rs){var r;return own.call(rs,this.data)&&(r=rs[this.data])?r.constructor===String?se(this.map(function(n){return n.replace(rs)
}),function(){this.data=r}):r:this.map(function(n){return n.replace(rs)})},thin_clone:function(){return this.map(function(){return false})},repopulated_with:function(xs){return new this.constructor(this.data,xs)
},with_data:function(d){return new this.constructor(d,Array.prototype.slice.call(this))},change:function(i,x){return se(new this.constructor(this.data,Array.prototype.slice.call(this)),function(n){n[i]=x
})},compose_single:function(i,f){return this.change(i,f(this[i]))},slice:function(x1,x2){return new this.constructor(this.data,Array.prototype.slice.call(this,x1,x2))
},traverse:function(f){f({entering:this});f({exiting:this.each(function(n){n.traverse(f)})});return this},flatten:function(d){d=d||this.data;return d!==this.data?this.as(d):!(has(parse_lr,d)&&this.length)?this:has(parse_associates_right,d)?se(new this.constructor(d),bind(function(n){for(var i=this;
i&&i.data===d;i=i[1]){n.push(i[0])}n.push(i)},this)):se(new this.constructor(d),bind(function(n){for(var i=this,ns=[];i.data===d;i=i[0]){i[1]&&ns.push(i[1])}ns.push(i);
for(i=ns.length-1;i>=0;--i){n.push(ns[i])}},this))},unflatten:function(){var t=this,right=has(parse_associates_right,this.data);return this.length<=2?this:se(new this.constructor(this.data),function(n){if(right){for(var i=0,l=t.length-1;
i<l;++i){n=n.push(t[i]).push(i<l-2?t.data:t[i])[1]}}else{for(var i=t.length-1;i>=1;--i){n=n.push(i>1?t.data:t[0]).push(t[i])[0]}}})},as:function(d){return this.data===d?this:new caterwaul_global.syntax(d).push(this)
},bindings:function(hash){var result=hash||{};this.reach(function(n){n.add_bindings_to(result)});return result},expressions:function(hash){var result=hash||{};this.reach(function(n){n.add_expressions_to(result)
});return result},add_bindings_to:function(hash){},add_expressions_to:function(hash){},resolve:function(){return this},reduce:function(){return this},prefix:function(d){return this.prefixes().push(d),this
},prefixes:function(){return this.prefix_data||(this.prefix_data=[])},infix:function(d){return this.infixes().push(d),this},infixes:function(){return this.infix_data||(this.infix_data=[])
},suffix:function(d){return this.suffixes().push(d),this},suffixes:function(){return this.suffix_data||(this.suffix_data=[])},contains:function(f){var result=f(this);
if(result){return result}for(var i=0,l=this.length;i<l;++i){if(result=this[i].contains(f)){return result}}},match:function(target,variables){target=target.constructor===String?caterwaul_global.parse(target):target;
variables||(variables={_:target});if(this.is_wildcard()&&(!this.leaf_nodes_only()||!this.length)){return variables[this.without_metadata()]=target,variables}else{if(this.length===target.length&&this.data===target.data){for(var i=0,l=this.length;
i<l;++i){if(!this[i].match(target[i],variables)){return null}}return variables}}},toString:function(depth){var xs=[""];this.serialize(xs,depth||-1);return xs.join("")
},structure:function(){if(this.length){return"("+['"'+this.data+'"'].concat(map(function(x){return x.structure()},this)).join(" ")+")"}else{return this.data}}};caterwaul_global.syntax_subclasses=[];
caterwaul_global.syntax_subclass=function(ctor){var extensions=Array.prototype.slice.call(arguments,1),proxy=function(){return ctor.apply(this,arguments)};caterwaul_global.merge.apply(this,[proxy.prototype,syntax_common].concat(extensions));
caterwaul_global.syntax_subclasses.push(proxy);proxy.prototype.constructor=proxy;return proxy};caterwaul_global.syntax_extend=function(){for(var i=0,l=caterwaul_global.syntax_subclasses.length,es=Array.prototype.slice.call(arguments);
i<l;++i){caterwaul_global.merge.apply(this,[caterwaul_global.syntax_subclasses[i].prototype].concat(es))}caterwaul_global.merge.apply(this,[syntax_common].concat(es));
return caterwaul_global};var parse_hex=caterwaul_global.parse_hex=function(digits){for(var result=0,i=0,l=digits.length,d;i<l;++i){result*=16,result+=(d=digits.charCodeAt(i))<=58?d-48:(d&95)-55
}return result},parse_octal=caterwaul_global.parse_octal=function(digits){for(var result=0,i=0,l=digits.length;i<l;++i){result*=8,result+=digits.charCodeAt(i)-48
}return result},unescape_string=caterwaul_global.unescape_string=function(s){for(var i=0,c,l=s.length,result=[],is_escaped=false;i<l;++i){if(is_escaped){is_escaped=false,result.push((c=s.charAt(i))==="\\"?"\\":c==="n"?"\n":c==="r"?"\r":c==="b"?"\b":c==="f"?"\f":c==="0"?"\u0000":c==="t"?"\t":c==="v"?"\v":c==='"'||c==="'"?c:c==="x"?String.fromCharCode(parse_hex(s.substring(i,++i+1))):c==="u"?String.fromCharCode(parse_hex(s.substring(i,(i+=3)+1))):String.fromCharCode(parse_octal(s.substring(i,(i+=2)+1))))
}else{if((c=s.charAt(i))==="\\"){is_escaped=true}else{result.push(c)}}}return result.join("")};caterwaul_global.javascript_tree_type_methods={is_string:function(){return/['"]/.test(this.data.charAt(0))
},as_escaped_string:function(){return this.data.substr(1,this.data.length-2)},is_number:function(){return/^-?(0x|\d|\.\d+)/.test(this.data)},as_number:function(){return Number(this.data)
},is_boolean:function(){return this.data==="true"||this.data==="false"},as_boolean:function(){return this.data==="true"},is_regexp:function(){return/^\/./.test(this.data)
},as_escaped_regexp:function(){return this.data.substring(1,this.data.lastIndexOf("/"))},is_array:function(){return this.data==="["},as_unescaped_string:function(){return unescape_string(this.as_escaped_string())
},could_be_identifier:function(){return/^[A-Za-z_$@][A-Za-z0-9$_@]*$/.test(this.data)},is_identifier:function(){return this.length===0&&this.could_be_identifier()&&!this.is_boolean()&&!this.is_null_or_undefined()&&!has(lex_op,this.data)
},has_grouped_block:function(){return has(parse_r_until_block,this.data)},is_block:function(){return has(parse_block,this.data)},is_blockless_keyword:function(){return has(parse_r_optional,this.data)
},is_null_or_undefined:function(){return this.data==="null"||this.data==="undefined"},is_constant:function(){return this.is_number()||this.is_string()||this.is_boolean()||this.is_regexp()||this.is_null_or_undefined()
},left_is_lvalue:function(){return/=$/.test(this.data)||/\+\+$/.test(this.data)||/--$/.test(this.data)},is_empty:function(){return !this.length},has_parameter_list:function(){return this.data==="function"||this.data==="catch"
},has_lvalue_list:function(){return this.data==="var"||this.data==="const"},is_dereference:function(){return this.data==="."||this.data==="[]"},is_invocation:function(){return this.data==="()"
},is_contextualized_invocation:function(){return this.is_invocation()&&this[0].is_dereference()},is_invisible:function(){return has(parse_invisible,this.data)},is_binary_operator:function(){return has(parse_lr,this.data)
},is_prefix_unary_operator:function(){return has(parse_r,this.data)},is_postfix_unary_operator:function(){return has(parse_l,this.data)},is_unary_operator:function(){return this.is_prefix_unary_operator()||this.is_postfix_unary_operator()
},precedence:function(){return parse_inverse_order[this.data]},is_right_associative:function(){return has(parse_associates_right,this.data)},is_associative:function(){return/^[,;]$/.test(this.data)
},is_group:function(){return/^[(\[{][)\]]?$/.test(this.data)},accepts:function(e){return has(parse_accepts,this.data)&&parse_accepts[this.data]===(e.data||e)}};caterwaul_global.javascript_tree_metadata_methods={could_have_metadata:function(){return this.could_be_identifier()
},without_metadata:function(){return this.data.replace(/@.*$/g,"")},is_wildcard:function(){return this.data.charCodeAt(0)===95},leaf_nodes_only:function(){return/@0/.test(this.data)
},is_opaque:function(){return this.data.charCodeAt(0)===64}};caterwaul_global.javascript_tree_serialization_methods={ends_with_block:function(){var block=this[this.length-1];
if(block&&block.data===parse_accepts[this.data]){block=block[0]}return this.data==="{"||has(parse_r_until_block,this.data)&&(this.data!=="function"||this.length===3)&&block&&block.ends_with_block()
},never_guarded:function(){return this.is_group()||this.precedence()>parse_inverse_order[","]},guarded:function(p){var this_p=this.never_guarded()?undefined:this.precedence(),associative=this.is_associative(),right=this.is_right_associative(),result=this.map(function(x,i){return x.guarded(this_p-(!associative&&!right&&!!i))
});return this_p>p?result.as("("):result},serialize:function(xs,depth){var ep=function(x){e(p||x&&lex_ident[xs[xs.length-1].charCodeAt(0)]===lex_ident[x.charCodeAt(0)]?" ":""),x&&e(x)
},e=function(x){x&&xs.push(x)},p=this.prefix_data&&this.prefix_data.join(""),l=this.length,d=this.data,d1=depth-1,i=this.infix_data&&this.infix_data.join(""),s=this.suffix_data&&this.suffix_data.join("");
if(depth===0&&(l||d.length>32)){return e("...")}switch(l){case 0:if(has(parse_r_optional,d)){return ep(d.replace(/^u/,"")),e(s)}else{if(has(parse_group,d)){return ep(d),e(i),e(parse_group[d]),e(s)
}else{return ep(d),e(s)}}case 1:if(has(parse_r,d)||has(parse_r_optional,d)){return ep(d.replace(/^u/,"")),this[0].serialize(xs,d1),e(s)}else{if(has(parse_misleading_postfix,d)){return this[0].serialize(xs,d1),ep(d),e(s)
}else{if(has(parse_group,d)){return ep(d),this[0].serialize(xs,d1),e(i),e(parse_group[d]),e(s)}else{if(has(parse_lr,d)){return ep(),this[0].serialize(xs,d1),e(s)
}else{return this[0].serialize(xs,d1),ep(d),e(s)}}}}case 2:if(has(parse_invocation,d)){return this[0].serialize(xs,d1),ep(d.charAt(0)),this[1].serialize(xs,d1),e(i),e(d.charAt(1)),e(s)
}else{if(has(parse_r_until_block,d)){return ep(d),this[0].serialize(xs,d1),this[1].serialize(xs,d1),e(s)}else{if(has(parse_invisible,d)){return this[0].serialize(xs,d1),this[1].serialize(xs,d1),e(s)
}else{return this[0].serialize(xs,d1),ep(d),this[1].serialize(xs,d1),e(s)}}}default:if(has(parse_ternary,d)){return this[0].serialize(xs,d1),ep(d),this[1].precedence()>this.precedence()?(this[1].as("(").serialize(xs,d1),e(i),e(":"),this[2].serialize(xs,d1),e(s)):(this[1].serialize(xs,d1),e(i),e(":"),this[2].serialize(xs,d1),e(s))
}else{if(has(parse_r_until_block,d)){return this.accepts(this[2])&&!this[1].ends_with_block()?(ep(d),this[0].serialize(xs,d1),this[1].serialize(xs,d1),e(";"),this[2].serialize(xs,d1),e(s)):(ep(d),this[0].serialize(xs,d1),this[1].serialize(xs,d1),this[2].serialize(xs,d1),e(s))
}else{return ep(),this.unflatten().serialize(xs,d1),e(s)}}}}};caterwaul_global.ref_common=caterwaul_global.merge({},caterwaul_global.javascript_tree_type_methods,caterwaul_global.javascript_tree_metadata_methods,caterwaul_global.javascript_tree_serialization_methods,{replace:function(replacements){var r;
return own.call(replacements,this.data)&&(r=replacements[this.data])?r.constructor===String?se(new this.constructor(this.value),function(){this.data=r}):r:this},length:0});
caterwaul_global.ref=caterwaul_global.syntax_subclass(function(value,name){if(value instanceof this.constructor){this.value=value.value,this.data=value.data}else{this.value=value,this.data=gensym(name&&name.constructor===String?name:"ref")
}},caterwaul_global.ref_common,{add_bindings_to:function(hash){hash[this.data]=this.value}});caterwaul_global.expression_ref=caterwaul_global.syntax_subclass(function(e,name){if(e instanceof this.constructor){this.e=e.e,this.data=e.data
}else{this.e=e,this.data=gensym(name&&name.constructor===String?name:"e")}},caterwaul_global.ref_common,{add_expressions_to:function(hash){hash[this.data]=this.e
}});caterwaul_global.metadata_node=caterwaul_global.syntax_subclass(function(d,name){if(d instanceof this.constructor){this.metadata=d.metadata,this.data=d.data}else{this.metadata=d,this.data="@"+(name||"")
}},caterwaul_global.ref_common);caterwaul_global.opaque_tree=caterwaul_global.syntax_subclass(function(code,expression_refs){if(code instanceof this.constructor){this.data=code.data,this.expression_refs=code.expression_refs
}else{this.data=code.toString(),this.expression_refs=expression_refs||code.caterwaul_expression_ref_table}var rs=this.expression_refs;for(var k in rs){own.call(rs,k)&&rs[k].constructor===String&&(rs[k]=new caterwaul_global.opaque_tree(rs[k]))
}},{resolve:function(){return this.expression_refs?caterwaul_global.late_bound_tree(new this.constructor(this.data),this.expression_refs):this},reduce:function(){return this.expression_refs?caterwaul_global.late_bound_tree(this.parse(),this.expression_refs):this.parse()
},guarded:function(){return this},serialize:function(xs){return xs.push(this.data),xs},parse:function(){return caterwaul_global.parse(this.data)}});caterwaul_global.syntax=se(caterwaul_global.syntax_subclass(function(data){if(data instanceof this.constructor){this.data=data.data,this.length=0,this.prefix_data=data.prefix_data,this.infix_data=data.infix_data,this.suffix_data=data.suffix_data
}else{this.data=data&&data.toString();this.length=0;for(var i=1,l=arguments.length,_;_=arguments[i],i<l;++i){for(var j=0,lj=_.length,it,c;_ instanceof Array?(it=_[j],j<lj):(it=_,!j);
++j){this._append(caterwaul_global.syntax.promote(it))}}}},caterwaul_global.javascript_tree_type_methods,caterwaul_global.javascript_tree_metadata_methods,caterwaul_global.javascript_tree_serialization_methods),function(){this.from_string=function(s){return new caterwaul_global.syntax('"'+s.replace(/\\/g,"\\\\").replace(/"/g,'\\"').replace(/\n/g,"\\n")+'"')
};this.from_array=function(xs){for(var i=0,c=new caterwaul_global.syntax(","),l=xs.length;i<l;++i){c.push(xs[i])}return new caterwaul_global.syntax("[",c.length?c.unflatten():[])
};this.from_object=function(o){var comma=new caterwaul_global.syntax(",");for(var k in o){if(own.call(o,k)){comma.push(new caterwaul_global.syntax(":",/^[$_A-Za-z][A-Za-z0-9$_]*$/.test(k)?k:caterwaul_global.syntax.from_string(k),o[k].as("(")))
}}return new caterwaul_global.syntax("{",comma.length?comma.unflatten():[])}});caterwaul_global.syntax.promote=function(v){var c=v.constructor;return c===String||c===Number||c===Boolean?new caterwaul_global.syntax(v):v
};var empty=caterwaul_global.empty=new caterwaul_global.syntax("");caterwaul_global.parse=function(input){if(input==null){return input}if(input.constructor===caterwaul_global.syntax){return input
}var s=input.toString().replace(/^\s*|\s*$/g,""),mark=0,c=0,re=true,esc=false,dot=false,exp=false,close=0,t="",i=0,l=s.length,cs=function(i){return s.charCodeAt(i)
},grouping_stack=[],gs_top=null,head=null,parent=null,indexes=map(function(){return[]},parse_reduce_order),invocation_nodes=[],all_nodes=[empty],new_node=function(n){return all_nodes.push(n),n
},push=function(n){return head?head._sibling(head=n):(head=n._append_to(parent)),new_node(n)},syntax_node=this.syntax,groups=[],ternaries=[],prefix=[],shift_prefix=function(){var k=prefix;
prefix=[];return k},prefixed_node=function(n){n.prefix_data=shift_prefix();return new_node(n)};if(l===0){return empty}while((mark=i)<l){esc=exp=dot=t=0;if(lex_space[c=cs(i)]){while(++i<l&&lex_space[cs(i)]){}}else{if(lex_bracket[c]){++i,t=1,re=lex_opener[c]
}else{if(c===lex_slash&&cs(i+1)===lex_star&&(i+=2)){while(++i<=l&&(cs(i-1)!==lex_slash||cs(i-2)!==lex_star)){}}else{if(c===lex_slash&&cs(i+1)===lex_slash){while(++i<=l&&!lex_eol[cs(i-1)]){}}else{if(c===lex_hash){while(++i<=l&&!lex_eol[cs(i-1)]){}}else{if(lex_quote[c]&&(close=c)&&re&&!(re=!(t=s.charAt(i)))){while(++i<l&&(c=cs(i))!==close||esc){esc=!esc&&c===lex_back
}while(++i<l&&lex_regexp_suffix[cs(i)]){}t=1}else{if(c===lex_zero&&lex_integer[cs(i+1)]){while(++i<l&&lex_integer[cs(i)]){}re=!(t=1)}else{if(lex_float[c]&&(c!==lex_dot||lex_decimal[cs(i+1)])){while(++i<l&&(lex_decimal[c=cs(i)]||dot^(dot|=c===lex_dot)||exp^(exp|=lex_exp[c]&&++i))){}while(i<l&&lex_decimal[cs(i)]){++i
}re=!(t=1)}else{if(lex_punct[c]&&(t=re?"u":"",re=true)){while(i<l&&lex_punct[cs(i)]&&has(lex_op,t+s.charAt(i))){t+=s.charAt(i++)}re=!has(lex_postfix_unary,t)}else{while(++i<l&&(lex_ident[c=cs(i)]||c>127)){}re=has(lex_op,t=s.substring(mark,i))
}}}}}}}}}if(i===mark){throw new Error('Caterwaul lex error at "'+s.substr(mark,80)+'" with leading context "'+s.substr(mark-80,80)+'" (probably a Caterwaul bug)')
}if(t===0){prefix.push(s.substring(mark,i));continue}t=t===1?s.substring(mark,i):t==="u;"?";":t;t===gs_top?(grouping_stack.pop(),gs_top=grouping_stack[grouping_stack.length-1],(head||parent).infix_data=shift_prefix(),head=head?head.p:parent,parent=null):(has(parse_group,t)?(grouping_stack.push(gs_top=parse_group[t]),parent=push(prefixed_node(new syntax_node(t))),groups.push(parent),head=null):push(prefixed_node(new syntax_node(t))),has(parse_inverse_order,t)&&indexes[parse_inverse_order[t]].push(head||parent));
re|=t===")"&&head.l&&has(parse_r_until_block,head.l.data)}for(var i=0,l=indexes.length,forward,_;_=indexes[i],forward=parse_index_forward[i],i<l;++i){for(var j=forward?0:_.length-1,lj=_.length,inc=forward?1:-1,node,data,ll;
forward?j<lj:j>=0;j+=inc){if(has(parse_lr,data=(node=_[j]).data)){if(data===":"&&parse_inverse_order[node.r.data]>i){node._fold_l()}else{node._fold_lr()}}else{if(has(parse_ambiguous_group,data)&&node.l&&!((ll=node.l.l)&&has(parse_r_until_block,ll.data))&&(node.l.data==="."||(node.l.data==="function"&&node.l.length===2)||!(has(lex_op,node.l.data)||has(parse_not_a_value,node.l.data)))){invocation_nodes.push(node.l._wrap(new_node(new syntax_node(data+parse_group[data]))).p._fold_r())
}else{if(has(parse_l,data)){node._fold_l()}else{if(has(parse_r,data)){node._fold_r()}else{if(has(parse_ternary,data)){node._fold_lr(),ternaries.push(node)}else{if(has(parse_r_until_block,data)&&node.r&&node.r.data!==":"){for(var count=0,limit=parse_r_until_block[data];
count<limit&&node.r&&!has(parse_block,node.r.data);++count){node._fold_r()}node.r&&(node.r.data===";"?node.push(empty):node._fold_r());if(has(parse_accepts,data)&&parse_accepts[data]===(node.r&&node.r.r&&node.r.r.data)){node._fold_r().pop()._fold_r()
}else{if(has(parse_accepts,data)&&parse_accepts[data]===(node.r&&node.r.data)){node._fold_r()}}}else{if(has(parse_r_optional,data)){node.r&&node.r.data!==";"&&node._fold_r()
}}}}}}}}}for(var i=all_nodes.length-1,_;i>=0;--i){(_=all_nodes[i]).r&&_._wrap(new_node(new syntax_node("i;"))).p._fold_r()}for(var i=0,l=invocation_nodes.length,_,child;
i<l;++i){(child=(_=invocation_nodes[i])[1]=_[1][0]||empty)&&(child.p=_)}for(var i=0,l=groups.length,_;i<l;++i){(_=groups[i]).length||_.push(empty)}for(var i=0,l=ternaries.length,_,n,temp;
i<l;++i){n=(_=ternaries[i]).length,temp=_[0],_[0]=_[n-2],_[1]=temp,_[2]=_[n-1],_.length=3}while(head.p){head=head.p}for(var i=all_nodes.length-1,_;i>=0;--i){delete (_=all_nodes[i]).p,delete _.l,delete _.r
}head.suffix_data=prefix;return head};var bound_expression_template=caterwaul_global.parse("var _bindings; return(_expression)"),binding_template=caterwaul_global.parse("_variable = _base._variable"),undefined_binding=caterwaul_global.parse("undefined = void(0)"),late_bound_template=caterwaul_global.parse("(function (_bindings) {var _result=(_body);_result_init;return(_result)}).call(this, _expressions)"),late_bound_ref_table_template=caterwaul_global.parse("_result.caterwaul_expression_ref_table = _expression_ref_table");
caterwaul_global.compile=function(tree,environment,options){options=caterwaul_global.merge({gensym_renaming:true,transparent_errors:false,unbound_closure:false,guard:true},options);
tree=caterwaul_global.late_bound_tree(tree,null,options);if(options.guard){tree=tree.guarded()}var bindings=caterwaul_global.merge({},this._environment,environment,tree.bindings()),variables=[undefined_binding],s=gensym("base");
for(var k in bindings){if(own.call(bindings,k)&&k!=="this"){variables.push(binding_template.replace({_variable:k,_base:s}))}}var variable_definitions=new this.syntax(",",variables).unflatten(),function_body=bound_expression_template.replace({_bindings:variable_definitions,_expression:tree});
if(options.gensym_renaming){var renaming_table=this.gensym_rename_table(function_body);for(var k in bindings){own.call(bindings,k)&&(bindings[renaming_table[k]||k]=bindings[k])
}function_body=function_body.replace(renaming_table);s=renaming_table[s]}var code=function_body.toString(),closure=(function(){if(options.transparent_errors){return new Function(s,code)
}else{try{return new Function(s,code)}catch(e){throw new Error((e.message||e)+" while compiling "+code)}}})();return options.unbound_closure?closure:closure.call(bindings["this"],bindings)
};var trivial_node_template=caterwaul_global.parse("new caterwaul.syntax(_data)"),nontrivial_node_template=caterwaul_global.parse("new caterwaul.syntax(_data, _xs)"),node_prefix_template=caterwaul_global.parse("_x.prefix(_y)"),node_infix_template=caterwaul_global.parse("_x.infix(_y)"),node_suffix_template=caterwaul_global.parse("_x.suffix(_y)");
caterwaul_global.node_padding_annotations=function(node,node_expression){for(var xs=node.prefixes(),i=0,l=xs.length;i<l;++i){node_expression=node_prefix_template.replace({_x:node_expression,_y:caterwaul_global.syntax.from_string(xs[i])})
}for(var xs=node.infixes(),i=0,l=xs.length;i<l;++i){node_expression=node_infix_template.replace({_x:node_expression,_y:caterwaul_global.syntax.from_string(xs[i])})
}for(var xs=node.suffixes(),i=0,l=xs.length;i<l;++i){node_expression=node_suffix_template.replace({_x:node_expression,_y:caterwaul_global.syntax.from_string(xs[i])})
}return node_expression};caterwaul_global.syntax_to_expression=function(tree){if(tree.length){for(var comma=new caterwaul_global.syntax(","),i=0,l=tree.length;i<l;
++i){comma.push(caterwaul_global.syntax_to_expression(tree[i]))}return caterwaul_global.node_padding_annotations(tree,nontrivial_node_template.replace({_data:caterwaul_global.syntax.from_string(tree.data),_xs:comma.unflatten()}))
}else{return caterwaul_global.node_padding_annotations(tree,trivial_node_template.replace({_data:caterwaul_global.syntax.from_string(tree.data)}))}};caterwaul_global.late_bound_tree=function(tree,environment,options){options=caterwaul_global.merge({expression_ref_table:true},options);
tree=tree.rmap(function(node){return node.resolve()});var bindings=caterwaul_global.merge({},environment,tree.expressions()),variables=new caterwaul_global.syntax(","),expressions=new caterwaul_global.syntax(","),table={};
for(var k in bindings){if(own.call(bindings,k)){variables.push(new caterwaul_global.syntax(k)),expressions.push(bindings[k]),table[k]=caterwaul_global.syntax.from_string(bindings[k].toString())
}}var result_gensym=caterwaul_global.gensym("result"),result_initializer=options.expression_ref_table?late_bound_ref_table_template.replace({_result:result_gensym,_expression_ref_table:caterwaul_global.syntax.from_object(table)}):caterwaul_global.empty;
return variables.length?late_bound_template.replace({_bindings:variables.unflatten(),_expressions:expressions.unflatten(),_result:result_gensym,_result_init:result_initializer,_body:tree}):tree
};caterwaul_global.gensym_rename_table=function(tree){var names={},gensyms=[];tree.reach(function(node){var d=node.data;if(is_gensym(d)){names[d]||gensyms.push(d)
}names[d]=d.replace(/^(.*)_[a-z0-9]+_.{22}$/,"$1")||"anon"});var unseen_count={},next_unseen=function(name){if(!(name in names)){return name}var n=unseen_count[name]||0;
while(names[name+(++n).toString(36)]){}return name+(unseen_count[name]=n).toString(36)};for(var renamed={},i=0,l=gensyms.length,g;i<l;++i){renamed[g=gensyms[i]]||(names[renamed[g]=next_unseen(names[g])]=true)
}return renamed};var invoke_caterwaul_methods=function(methods){/^:/.test(methods)&&(methods=caterwaul_global[methods.substr(1)]);methods.constructor===String&&(methods=methods.split(/\s+/));
for(var i=1,l=methods.length,r=caterwaul_global[methods[0]]();i<l;++i){r=caterwaul_global[methods[i]](r)}return r};caterwaul_global.init=function(macroexpander){macroexpander||(macroexpander=function(x){return true
});return macroexpander.constructor===Function?se((function(){var result=function(f,environment,options){return typeof f==="function"||f.constructor===String?caterwaul_global.compile(result.call(result,caterwaul_global.parse(f)),environment,options):f.rmap(function(node){return macroexpander.call(result,node,environment,options)
})};return result})(),function(){this.global=caterwaul_global,this.macroexpander=macroexpander}):invoke_caterwaul_methods(macroexpander)};caterwaul_global.initializer=initializer;
caterwaul_global.clone=function(){return se(initializer(initializer,unique).deglobalize(),function(){for(var k in caterwaul_global){this[k]||(this[k]=caterwaul_global[k])
}})};var w_template=caterwaul_global.parse("(function (f) {return f(f)})(_x)"),module_template=caterwaul_global.parse("module(_name, _f)");caterwaul_global.replicator=function(options){if(options&&options.minimal_core_only){return w_template.replace({_x:new this.opaque_tree(this.core_initializer)})
}if(options&&options.core_only){return w_template.replace({_x:new this.opaque_tree(this.initializer)})}for(var i=0,ms=options&&options.modules||this.modules,c=[],l=ms.length;
i<l;++i){c.push(module_template.replace({_name:this.syntax.from_string(ms[i]),_f:new this.opaque_tree(this.module(ms[i]))}))}for(var i=0,l=c.length,result=new this.syntax(".",w_template.replace({_x:new this.opaque_tree(this.initializer)}));
i<l;++i){result.push(c[i])}return this.late_bound_tree(result.unflatten())};return caterwaul});
79 core/caterwaul/caterwaul.std.min.js
caterwaul.module("std.all-bundle",function($){$.all=[]});caterwaul.module("std.macro",function($){var syntax_manipulator=function(base_case){var result=function(x){if(x.constructor===Array){for(var i=0,l=x.length,ys=[];
i<l;++i){ys.push(result(x[i]))}return function(tree){for(var i=ys.length-1,r;i>=0;--i){if(r=ys[i].call(this,tree)){return r}}}}else{return x.constructor===String?result($.parse(x)):x.constructor===$.syntax?base_case.call(this,x):x
}};return result};$.pattern=syntax_manipulator(function(pattern){return function(tree){return pattern.match(tree)}});$.expander=syntax_manipulator(function(expander){return function(match){return expander.replace(match)
}});$.alternatives=syntax_manipulator(function(alternative){throw new Error("must use replacer functions with caterwaul.alternatives()")});$.reexpander=function(expander){var e=$.expander(expander);
return function(match){var r=e.call(this,match);return r&&this(r)}};var composer=function(expander_base_case){return function(pattern,expander){var new_pattern=$.pattern(pattern),new_expander=expander_base_case(expander);
return function(tree){var match=new_pattern.call(this,tree);return match&&new_expander.call(this,match)}}};$.replacer=composer($.expander);$.rereplacer=composer($.reexpander);
$.macroexpand=function(tree){return $($.alternatives(Array.prototype.slice.call(arguments,1)))(tree)}});caterwaul.module("std.anon",function($){$.anonymizer=function(){var xs=arguments;
return(function(){var table=(function(o){for(var r={},i=0,l=o.length,x;i<l;++i){x=o[i],r[x[0]]=x[1]}return r}).call(this,((function(xs){var x,x0,xi,xl,xr;for(var xr=new xs.constructor(),xi=0,xl=xs.length;
xi<xl;++xi){x=xs[xi],xr.push(([x,$.gensym(x)]))}return xr}).call(this,(function(xs){var x,x0,xi,xl,xr;for(var xr=new xs.constructor(),xi=0,xl=xs.length;xi<xl;++xi){x=xs[xi],xr.push.apply(xr,Array.prototype.slice.call((x.constructor===Array?x:x.split(" "))))
}return xr}).call(this,Array.prototype.slice.call((xs))))));return function(_){return(($).parse(_)).replace(table)}}).call(this)}});caterwaul.module("std.js",(function(qs,qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9,qsa,qsb,qsc,qsd,qse,qsf,qsg,qsh,qsi,qsj,qsk,qsl,qsm,qsn){var result1=(function($){$.js=function(macroexpander){var string_interpolator=function(node){var s=node.data,q=s.charAt(0),syntax=$.syntax;
if(q!=="'"&&q!=='"'||!/#\{[^\}]+\}/.test(s)){return false}for(var pieces=[],is_code=[],i=1,l=s.length-1,brace_depth=0,got_hash=false,start=1,c;i<l;++i){if(brace_depth){if((c=s.charAt(i))==="}"){--brace_depth||(pieces.push(s.substring(start,i)),is_code.push(true))&&(start=i+1),got_hash=false
}else{brace_depth+=c==="{"}}else{if((c=s.charAt(i))==="#"){got_hash=true}else{if(c==="{"&&got_hash){pieces.push(s.substring(start,i-1)),is_code.push(false),start=i+1,++brace_depth
}else{got_hash=false}}}}pieces.push(s.substring(start,l)),is_code.push(false);for(var quoted=new RegExp("\\\\"+q,"g"),i=0,l=pieces.length;i<l;++i){pieces[i]=is_code[i]?this($.parse(pieces[i].replace(quoted,q)).as("(")):new syntax(q+pieces[i]+q)
}return new syntax("+",pieces).unflatten().as("(")};var function_local_template=qs,function_bind_pattern=qs1,function_result_pattern=qs2,function_with_afters=qs3,function_without_afters=qs4,function_assignment_template=qs5,function_is_result=function(n){return n.is_empty()&&n.data==="result"
},function_destructure=$.rereplacer(qs6,function(match){for(var formals=[],befores=[],afters=[],ps=match._xs.flatten(","),i=0,l=ps.length,p;i<l;++i){p=this(ps[i]),(afters.length||p.contains(function_is_result)?afters:befores.length||p.length?befores:formals).push(p)
}for(var contains_locals=[befores,afters],i=0,l=contains_locals.length;i<l;++i){for(var xs=contains_locals[i],j=0,lj=xs.length,m;j<lj;++j){xs[j]=(m=function_bind_pattern.match(xs[j]))&&m._x.is_empty()?function_local_template.replace(m):xs[j].as("(")
}}var new_formals=formals.length?new $.syntax(",",formals).unflatten():$.empty,new_befores=befores.length?new $.syntax(";",befores).unflatten():$.empty,new_afters=afters.length?new $.syntax(";",afters).unflatten():$.empty,template=function_assignment_template.replace({_f:match._f,_x:afters.length?function_with_afters:function_without_afters});
return template.replace({_formals:new_formals,_befores:new_befores,_afters:new_afters,_result:match._y})});var tuple_template=qs7,tuple_constructor=qs8,tuple_assignment=qs9,tuple_destructure=$.rereplacer(qsa,function(match){for(var formals=match._xs.flatten(","),assignments=new $.syntax(";"),i=0,l=formals.length;
i<l;++i){assignments.push(tuple_assignment.replace({_name:formals[i]}))}return tuple_template.replace({_f:match._f,_g:$.gensym("tuple_ctor"),_ctor:tuple_constructor.replace({_formals:formals,_assignments:assignments.unflatten()}),_prototype:match._y})
});var infix_function=function(node){var d=node.data,left,fn;if((d==="/"||d==="|")&&(left=node[0]).data===d&&left[1]&&left[1].data==="u-"&&(fn=left[1][0])){return new $.syntax("()",fn,this(left[0]).flatten(d).push(this(node[1])).with_data(",").unflatten())
}};var infix_method=function(node){var d=node.data,left,fn;if((d==="/"||d==="|")&&(left=node[0]).data===d&&left[1]&&left[1].data==="u~"&&(fn=left[1][0])){var xs=[].slice.call(this(node[0][0]).flatten(d)),object=xs.shift();
return new $.syntax("()",new $.syntax(".",new $.syntax("(",object),fn),new $.syntax(",",xs,this(node[1])).unflatten())}};var postfix_function_template=qsb,postfix_function=$.rereplacer(qsc,function(match){return postfix_function_template.replace({_f:match._f,_x:this(match._x).flatten("/").with_data(",").unflatten()})
});var modified_literal_form=$.pattern(qsd),lookup_literal_modifier=function(caterwaul,type,modifier){var hash=caterwaul.literal_modifiers[type];return hash.hasOwnProperty(modifier)&&hash[modifier]
},literal_modifier=function(node){var modified_literal=modified_literal_form.call(this,node),literal,expander;if(modified_literal&&(literal=modified_literal._literal)&&(expander=literal.is_identifier()?lookup_literal_modifier(this,"identifier",modified_literal._modifier.data):literal.is_array()?lookup_literal_modifier(this,"array",modified_literal._modifier.data):literal.is_regexp()?lookup_literal_modifier(this,"regexp",modified_literal._modifier.data):literal.is_number()?lookup_literal_modifier(this,"number",modified_literal._modifier.data):literal.is_string()?lookup_literal_modifier(this,"string",modified_literal._modifier.data):null)){return expander.call(this,literal)
}};var bracket_modifier_form=$.pattern(qse),slash_modifier_form=$.pattern(qsf),minus_modifier_form=$.pattern(qsg),in_modifier_form=$.pattern(qsh),pipe_modifier_form=$.pattern(qsi),comma_modifier_form=$.pattern(qsj),dot_parameters=$.pattern(qsk),bracket_parameters=$.pattern(qsl),parameterized_wickets=$.pattern(qsm),parameterized_minus=$.pattern(qsn),modifier=function(node){var modifier,parameterized_match=parameterized_wickets.call(this,node)||parameterized_minus.call(this,node);
if(parameterized_match&&this.parameterized_modifiers.hasOwnProperty(modifier=parameterized_match._modifier.data)){var r=this.parameterized_modifiers[modifier].call(this,parameterized_match);
if(r){return r}}var regular_match=bracket_modifier_form.call(this,node)||slash_modifier_form.call(this,node)||minus_modifier_form.call(this,node)||in_modifier_form.call(this,node)||pipe_modifier_form.call(this,node)||comma_modifier_form.call(this,node);
if(regular_match){var parameter_match=dot_parameters.call(this,regular_match._modifier)||bracket_parameters.call(this,regular_match._modifier);if(parameter_match){regular_match._modifier=parameter_match._modifier;
regular_match._parameters=parameter_match._parameters;return this.parameterized_modifiers.hasOwnProperty(modifier=regular_match._modifier.data)&&this.parameterized_modifiers[modifier].call(this,regular_match)
}else{return this.modifiers.hasOwnProperty(modifier=regular_match._modifier.data)&&this.modifiers[modifier].call(this,regular_match)}}};var each_node=function(node){if(node.prefixes){var p=(function(xs1){var x,x0,xi,xl,xr;
for(var x,xi=0,xl=xs1.length,x1;xi<xl;++xi){x=xs1[xi];if(x1=(/^#/.test(x))){return x1}}return false}).call(this,node.prefixes()),i=(function(xs1){var x,x0,xi,xl,xr;
for(var x,xi=0,xl=xs1.length,x1;xi<xl;++xi){x=xs1[xi];if(x1=(/^#/.test(x))){return x1}}return false}).call(this,node.infixes()),s=(function(xs1){var x,x0,xi,xl,xr;
for(var x,xi=0,xl=xs1.length,x1;xi<xl;++xi){x=xs1[xi];if(x1=(/^#/.test(x))){return x1}}return false}).call(this,node.suffixes());(p||i||s)&&(node=node.thin_clone()),p&&(node.prefix_data=(function(xs1){var x,x0,xi,xl,xr;
for(var xr=new xs1.constructor(),xi=0,xl=xs1.length;xi<xl;++xi){x=xs1[xi],(/^#/.test(x))||xr.push(x)}return xr}).call(this,node.prefix_data)),i&&(node.infix_data=(function(xs1){var x,x0,xi,xl,xr;
for(var xr=new xs1.constructor(),xi=0,xl=xs1.length;xi<xl;++xi){x=xs1[xi],(/^#/.test(x))||xr.push(x)}return xr}).call(this,node.infix_data)),s&&(node.suffix_data=(function(xs1){var x,x0,xi,xl,xr;
for(var xr=new xs1.constructor(),xi=0,xl=xs1.length;xi<xl;++xi){x=xs1[xi],(/^#/.test(x))||xr.push(x)}return xr}).call(this,node.suffix_data))}return string_interpolator.call(this,node)||literal_modifier.call(this,node)||node.length&&(modifier.call(this,node)||function_destructure.call(this,node)||tuple_destructure.call(this,node)||infix_function.call(this,node)||infix_method.call(this,node)||postfix_function.call(this,node))
},result=macroexpander?$(function(node){return macroexpander.call(this,node)||each_node.call(this,node)}):$(each_node);result.modifiers={};result.parameterized_modifiers={};
result.literal_modifiers={regexp:{},array:{},string:{},number:{},identifier:{}};return result}});result1.caterwaul_expression_ref_table={qs:('new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " "))'),qs1:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " ")'),qs2:('new caterwaul.syntax( "result")'),qs3:('new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_formals")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "_befores") ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "result") .prefix( " ") ,new caterwaul.syntax( "_result") .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "_afters") .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "result") .prefix( " ")) .prefix( " "))) .prefix( " "))'),qs4:('new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_formals")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "_befores") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_result") .prefix( " ")) .prefix( " "))) .prefix( " "))'),qs5:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "_x") .prefix( " ")) .prefix( " ")'),qs6:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "_xs")) ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " ")'),qs7:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_g") .prefix( " ") ,new caterwaul.syntax( "_ctor") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_g") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "_prototype") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_g") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( "_g") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_g") .prefix( " ")) .prefix( " "))) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "this"))) .prefix( " ")'),qs8:('new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_formals")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "_assignments")) .prefix( " "))'),qs9:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "_name")) ,new caterwaul.syntax( "_name") .prefix( " ")) .prefix( " ")'),qsa:('new caterwaul.syntax( "*=" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "_xs")) ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " ")'),qsb:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "_x"))'),qsc:('new caterwaul.syntax( "/" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "_f"))) .prefix( " ")'),qsd:('new caterwaul.syntax( "." ,new caterwaul.syntax( "_literal") ,new caterwaul.syntax( "_modifier"))'),qse:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_modifier") ,new caterwaul.syntax( "_expression"))'),qsf:('new caterwaul.syntax( "/" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier")) .prefix( " ")'),qsg:('new caterwaul.syntax( "-" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier")) .prefix( " ")'),qsh:('new caterwaul.syntax( "in" ,new caterwaul.syntax( "_modifier") ,new caterwaul.syntax( "_expression") .prefix( " ")) .prefix( " ")'),qsi:('new caterwaul.syntax( "|" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier")) .prefix( " ")'),qsj:('new caterwaul.syntax( "," ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier") .prefix( " "))'),qsk:('new caterwaul.syntax( "." ,new caterwaul.syntax( "_modifier") ,new caterwaul.syntax( "_parameters"))'),qsl:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_modifier") ,new caterwaul.syntax( "_parameters"))'),qsm:('new caterwaul.syntax( ">" ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier")) .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " "))'),qsn:('new caterwaul.syntax( "-" ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_modifier")) .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " "))')};
return(result1)}).call(this,new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",new caterwaul.syntax("_x"),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" "),new caterwaul.syntax("result"),new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_formals"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("_befores"),(new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("result")).prefix(" "),(new caterwaul.syntax("_result")).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("_afters")).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("result")).prefix(" "))).prefix(" ")))).prefix(" ")),new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_formals"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax("_befores"),(new caterwaul.syntax("return",(new caterwaul.syntax("_result")).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("=",new caterwaul.syntax("_f"),(new caterwaul.syntax("_x")).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("()",new caterwaul.syntax("_f"),new caterwaul.syntax("_xs")),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("_f"),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_g")).prefix(" "),(new caterwaul.syntax("_ctor")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",new caterwaul.syntax(".",(new caterwaul.syntax("_g")).prefix(" "),new caterwaul.syntax("prototype")),(new caterwaul.syntax("_prototype")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("_g")).prefix(" "),new caterwaul.syntax("prototype")),new caterwaul.syntax("constructor")),(new caterwaul.syntax("_g")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_g")).prefix(" "))).prefix(" ")))).prefix(" ")))).prefix(" "),new caterwaul.syntax("call")),new caterwaul.syntax("this")))).prefix(" "),new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_formals"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("_assignments"))).prefix(" ")),(new caterwaul.syntax("=",new caterwaul.syntax(".",new caterwaul.syntax("this"),new caterwaul.syntax("_name")),(new caterwaul.syntax("_name")).prefix(" "))).prefix(" "),(new caterwaul.syntax("*=",new caterwaul.syntax("()",new caterwaul.syntax("_f"),new caterwaul.syntax("_xs")),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax("_f"),new caterwaul.syntax("_x")),(new caterwaul.syntax("/",new caterwaul.syntax("_x"),new caterwaul.syntax("u!",new caterwaul.syntax("_f")))).prefix(" "),new caterwaul.syntax(".",new caterwaul.syntax("_literal"),new caterwaul.syntax("_modifier")),new caterwaul.syntax("[]",new caterwaul.syntax("_modifier"),new caterwaul.syntax("_expression")),(new caterwaul.syntax("/",new caterwaul.syntax("_expression"),new caterwaul.syntax("_modifier"))).prefix(" "),(new caterwaul.syntax("-",new caterwaul.syntax("_expression"),new caterwaul.syntax("_modifier"))).prefix(" "),(new caterwaul.syntax("in",new caterwaul.syntax("_modifier"),(new caterwaul.syntax("_expression")).prefix(" "))).prefix(" "),(new caterwaul.syntax("|",new caterwaul.syntax("_expression"),new caterwaul.syntax("_modifier"))).prefix(" "),new caterwaul.syntax(",",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_modifier")).prefix(" ")),new caterwaul.syntax(".",new caterwaul.syntax("_modifier"),new caterwaul.syntax("_parameters")),new caterwaul.syntax("[]",new caterwaul.syntax("_modifier"),new caterwaul.syntax("_parameters")),new caterwaul.syntax(">",(new caterwaul.syntax("<",new caterwaul.syntax("_expression"),new caterwaul.syntax("_modifier"))).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" ")),new caterwaul.syntax("-",(new caterwaul.syntax("-",new caterwaul.syntax("_expression"),new caterwaul.syntax("_modifier"))).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" "))));
caterwaul.module("std.js-literals",(function(qs1,qs2){var result=(function($){$.js_literals=function(caterwaul_function){var function_template=qs1;(function(r){r.x=$.reexpander(function(node){return node.with_data(node.data.replace(/\s+/g,""))
});var call_exec_template=qs2;r.qf=function(node){return function_template.replace({_body:call_exec_template.replace({_regexp:node})})}})(caterwaul_function.literal_modifiers.regexp);
(function(s){s.qw=$.reexpander(function(node){for(var array_node=new $.syntax("["),comma=new $.syntax(","),delimiter=node.data.charAt(0),pieces=node.as_escaped_string().split(/\s+/),i=0,l=pieces.length;
i<l;++i){comma.push(new $.syntax(delimiter+pieces[i]+delimiter))}return array_node.push(comma.unflatten())});s.qh=$.reexpander(function(node){for(var hash_node=new $.syntax("{"),comma=new $.syntax(","),delimiter=node.data.charAt(0),pieces=node.as_escaped_string().split(/\s+/),i=0,l=pieces.length;
i<l;i+=2){comma.push(new $.syntax(":",new $.syntax(delimiter+pieces[i]+delimiter),new $.syntax(delimiter+pieces[i+1]+delimiter)))}return hash_node.push(comma.unflatten())
});s.qr=$.reexpander(function(node){return node.with_data("/"+node.as_escaped_string().replace(/\//g,"\\/")+"/")});s.qs=function(node){return new $.expression_ref($.syntax_to_expression($.parse(node.as_unescaped_string())),"qs")
};s.qse=function(node){return new $.expression_ref($.syntax_to_expression(this.call(this,$.parse(node.as_unescaped_string()))),"qse")};s.qf=$.reexpander(function(node){return function_template.replace({_body:$.parse(node.as_unescaped_string())})
})})(caterwaul_function.literal_modifiers.string);return caterwaul_function}});result.caterwaul_expression_ref_table={qs1:('new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_body") .prefix( " "))) .prefix( " "))'),qs2:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_regexp") ,new caterwaul.syntax( "exec")) ,new caterwaul.syntax( "_"))')};
return(result)}).call(this,new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_body")).prefix(" ")))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("_regexp"),new caterwaul.syntax("exec")),new caterwaul.syntax("_"))));
caterwaul.module("std.words",(function(qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9,qsa,qsb,qsc,qsd,qsf,qsg,qsh,qsi,qsj){var result=(function($){(function(){var scope_template=qs1;
return $.words=function(caterwaul_function){return($.merge(caterwaul_function.modifiers,$.words.modifiers),$.merge(caterwaul_function.parameterized_modifiers,$.words.parameterized_modifiers),caterwaul_function)
},$.words.modifiers={qs:function(match){return new $.expression_ref($.syntax_to_expression(match._expression),"qs")},qse:function(match){return new $.expression_ref($.syntax_to_expression(this(match._expression)),"qse")
},qc:function(match){return $.compile(this(match._expression))},qce:function(match){return this($.compile(this(match._expression)))},reexpand:function(match){return this(this(match._expression))
},noexpand:function(match){return match._expression},raise:$.reexpander(qs2),eval:function(match){return new $.ref($.compile(this(match._expression)),"eval")},ahead:function(match){return new $.expression_ref(this(match._expression),"ahead")
},capture:function(match){for(var comma=new $.syntax(","),bindings=match._expression.flatten(","),i=0,l=bindings.length;i<l;++i){comma.push(this(bindings[i]).with_data(":"))
}return new $.syntax("{",comma.unflatten())},wcapture:function(match){for(var e=this(match._expression),comma=new $.syntax(","),bindings=e.flatten(","),node,i=0,l=bindings.length;
i<l;++i){(node=this(bindings[i]))[1]=node[0],comma.push(node.with_data(":"))}return scope_template.replace({_variables:e,_expression:new $.syntax("{",comma.unflatten())})
}},$.words.parameterized_modifiers={given:$.reexpander(qs3),bgiven:$.reexpander(qs4),rescue:$.reexpander(qs5),se:$.reexpander(qs6),re:$.reexpander(qs7),then:$.reexpander(qs8),eq:$.reexpander(qs9),ocq:$.reexpander(qsa),dcq:$.reexpander(qsb),acq:$.reexpander(qsc),ncq:$.reexpander(qsd),where:$.reexpander(qsf),using:$.reexpander(function(match){var m=this(match._parameters),o=$.compile(m),comma=new $.syntax(","),expression_ref=new $.expression_ref(m);
for(var k in o){Object.prototype.hasOwnProperty.call(o,k)&&/^[_$a-zA-Z][_$0-9a-zA-Z]*$/.test(k)&&!this.modifiers.hasOwnProperty(k)&&!this.parameterized_modifiers.hasOwnProperty(k)&&comma.push(new $.syntax("=",k,new $.syntax(".",expression_ref,k)))
}return scope_template.replace({_variables:comma.unflatten(),_expression:match._expression})}),when:$.reexpander(qsg),and:$.reexpander(qsh),unless:$.reexpander(qsi),or:$.reexpander(qsj)}
}).call(this)});result.caterwaul_expression_ref_table={qs1:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "_variables") .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_expression") .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "this"))'),qs2:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "throw" ,new caterwaul.syntax( "_expression") .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "this"))'),qs3:('new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_parameters")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_expression") .prefix( " "))) .prefix( " ")))'),qs4:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "t") ,new caterwaul.syntax( "f") .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "f") .prefix( " ") ,new caterwaul.syntax( "apply")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "t") ,new caterwaul.syntax( "arguments") .prefix( " "))))) .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_parameters")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_expression") .prefix( " "))) .prefix( " ")) .prefix( " ")))'),qs5:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "try" ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_expression") .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "catch" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "e")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_parameters") .prefix( " "))) .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "this"))'),qs6:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "it")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_parameters") .prefix( " ") ,new caterwaul.syntax( "it") .prefix( " ")))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_expression")) .prefix( " ")))'),qs7:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "it")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_parameters") .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_expression")) .prefix( " ")))'),qs8:('new caterwaul.syntax( "(" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_parameters") .prefix( " ")))'),qs9:('new caterwaul.syntax( "=" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")'),qsa:('new caterwaul.syntax( "?" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_expression") .prefix( " ") .infix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_expression") .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")) .prefix( " ")'),qsb:('new caterwaul.syntax( "?" ,new caterwaul.syntax( "!==" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "void" ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "_expression") .prefix( " ") .infix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_expression") .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")) .prefix( " ")'),qsc:('new caterwaul.syntax( "?" ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "_expression")) ,new caterwaul.syntax( "_expression") .prefix( " ") .infix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_expression") .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")) .prefix( " ")'),qsd:('new caterwaul.syntax( "?" ,new caterwaul.syntax( "!=" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "void" ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( "  ")) .prefix( " ") ,new caterwaul.syntax( "_expression") .prefix( " ") .infix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_expression") .prefix( " ") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")) .prefix( " ")'),qsf:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "_parameters") .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_expression") .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "this"))'),qsg:('new caterwaul.syntax( "&&" ,new caterwaul.syntax( "_parameters") ,new caterwaul.syntax( "_expression") .prefix( " ")) .prefix( " ")'),qsh:('new caterwaul.syntax( "&&" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")'),qsi:('new caterwaul.syntax( "&&" ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "_parameters") .prefix( " ")) ,new caterwaul.syntax( "_expression") .prefix( " ")) .prefix( " ")'),qsj:('new caterwaul.syntax( "||" ,new caterwaul.syntax( "_expression") ,new caterwaul.syntax( "_parameters") .prefix( " ")) .prefix( " ")')};
return(result)}).call(this,new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("_variables")).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_expression")).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax("this")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("throw",(new caterwaul.syntax("_expression")).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax("this")),new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_parameters"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_expression")).prefix(" ")))).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(",",new caterwaul.syntax("t"),(new caterwaul.syntax("f")).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("f")).prefix(" "),new caterwaul.syntax("apply")),new caterwaul.syntax(",",new caterwaul.syntax("t"),(new caterwaul.syntax("arguments")).prefix(" ")))))).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax(",",new caterwaul.syntax("this"),(new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_parameters"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_expression")).prefix(" ")))).prefix(" "))).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("try",(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_expression")).prefix(" ")))).prefix(" "),(new caterwaul.syntax("catch",(new caterwaul.syntax("(",new caterwaul.syntax("e"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_parameters")).prefix(" ")))).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax("this")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("it"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",new caterwaul.syntax(",",(new caterwaul.syntax("_parameters")).prefix(" "),(new caterwaul.syntax("it")).prefix(" "))))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("this"),(new caterwaul.syntax("(",new caterwaul.syntax("_expression"))).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("it"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("return",(new caterwaul.syntax("_parameters")).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("this"),(new caterwaul.syntax("(",new caterwaul.syntax("_expression"))).prefix(" "))),new caterwaul.syntax("(",new caterwaul.syntax(",",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_parameters")).prefix(" "))),(new caterwaul.syntax("=",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "),(new caterwaul.syntax("?",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_expression")).prefix(" ").infix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_expression")).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("?",(new caterwaul.syntax("!==",new caterwaul.syntax("_expression"),(new caterwaul.syntax("void",(new caterwaul.syntax("0")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("_expression")).prefix(" ").infix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_expression")).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("?",new caterwaul.syntax("u!",new caterwaul.syntax("_expression")),(new caterwaul.syntax("_expression")).prefix(" ").infix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_expression")).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("?",(new caterwaul.syntax("!=",new caterwaul.syntax("_expression"),(new caterwaul.syntax("void",(new caterwaul.syntax("0")).prefix(" "))).prefix("  "))).prefix(" "),(new caterwaul.syntax("_expression")).prefix(" ").infix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_expression")).prefix(" "),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(""))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("_parameters")).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_expression")).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax("this")),(new caterwaul.syntax("&&",new caterwaul.syntax("_parameters"),(new caterwaul.syntax("_expression")).prefix(" "))).prefix(" "),(new caterwaul.syntax("&&",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" "),(new caterwaul.syntax("&&",new caterwaul.syntax("u!",(new caterwaul.syntax("_parameters")).prefix(" ")),(new caterwaul.syntax("_expression")).prefix(" "))).prefix(" "),(new caterwaul.syntax("||",new caterwaul.syntax("_expression"),(new caterwaul.syntax("_parameters")).prefix(" "))).prefix(" ")));
caterwaul.module("std.grammar",(function(qs){var result=(function($){$.grammar=function(anonymous_symbols,options,rule_cc){return(function(){var default_options={fix:true,descend:true,initial:qs},settings=$.merge({},default_options,options),anon=$.anonymizer(anonymous_symbols),anon_pattern=anon(settings.initial),rule=function(p,e){return $[settings.fix?"rereplacer":"replacer"](anon(p),e.constructor===$.syntax?anon(e):e)
},expand=(function(it){return settings.descend?$(it):it}).call(this,($.alternatives(rule_cc(rule,anon))));return function(_){return(function(it){return this.constructor===Function?it&&this(it):it
}).call(this,(expand.call(expand,(anon_pattern).replace(_))))}}).call(this)}});result.caterwaul_expression_ref_table={qs:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_expression"))')};
return(result)}).call(this,new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_expression"))));caterwaul.module("std.seq",(function(qs,qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9,qsa,qsb,qsc,qsd,qse,qsf,qsg,qsh,qsi,qsj,qsk,qsl,qsm,qsn,qso,qsp,qsq,qsr,qss,qst,qsu,qsv,qsw,qsx,qsy,qsz,qs10,qs11,qs12,qs13,qs14,qs15,qs16,qs17,qs18,qs19,qs1a,qs1b,qs1c,qs1d,qs1e,qs1f,qs1g,qs1h,qs1i,qs1j,qs1k,qs1l,qs1m,qs1n,qs1o,qs1p,qs1q,qs1r,qs1s,qs1t,qs1u,qs1v,qs1w,qs1x,qs1y,qs1z,qs20,qs21,qs22,qs23,qs24,qs25,qs26,qs27,qs28,qs29,qs2a,qs2b,qs2c,qs2d,qs2e,qs2f,qs2g,qs2h,qs2i,qs2j,qs2k,qs2l,qs2m,qs2n,qs2o,qs2p){var result=(function($){$.seq=function(caterwaul_function){return(function(it){return it.modifiers.seq=$.grammar("S",{initial:qs},(function(rule,anon){return(function(){var operator_macros=(function(){var loop_anon=$.anonymizer("x","y","i","j","l","lj","r","o","k"),scope=anon(qs1),scoped=function(t){return(scope).replace({_body:t})
},form=function(x){return(function(it){return it.uses_x0=/_x0\s*=/.test(x.toString()),it}).call(this,(loop_anon(scoped(anon(x)))))},map=form(qs2),each=form(qs3),flatmap=form(qs4),iterate=form(qs5),filter=form(qs6),filter_not=form(qs7),map_filter=form(qs8),imap_filter=form(qs9),foldl=form(qsa),foldr=form(qsb),unfold=form(qsc),ifoldl=form(qsd),ifoldr=form(qse),iunfold=form(qsf),exists=form(qsg),not_exists=form(qsh),r_exists=form(qsi),iexists=form(qsj),ir_exists=form(qsk),concat=anon(qsl),kmap=form(qsm),keach=form(qsn),kfilter=form(qso),kfilter_not=form(qsp),kmap_filter=form(qsq),vmap=form(qsr),veach=form(qss),vfilter=form(qst),vfilter_not=form(qsu),vmap_filter=form(qsv);
return(function(){var operator_case=function(forms){return function(match){return(function(){var use=function(form,iform){return function(body){return render_form(match._xs,body,form,iform)
}};return parse_modifiers(match._thing,use(forms.normal,forms.inormal),use(forms.bang,forms.ibang),use(forms.tbang,forms.itbang))}).call(this)}},map_forms=operator_case({normal:map,bang:each,tbang:flatmap,itbang:iterate}),filter_forms=operator_case({normal:filter,bang:filter_not,tbang:map_filter,itbang:imap_filter}),fold_forms=operator_case({normal:foldl,bang:foldr,tbang:unfold,inormal:ifoldl,ibang:ifoldr,itbang:iunfold}),kmap_forms=operator_case({normal:kmap,bang:keach}),kfilter_forms=operator_case({normal:kfilter,bang:kfilter_not,tbang:kmap_filter}),vmap_forms=operator_case({normal:vmap,bang:veach}),vfilter_forms=operator_case({normal:vfilter,bang:vfilter_not,tbang:vmap_filter}),exists_forms=operator_case({normal:exists,bang:not_exists,tbang:r_exists,inormal:iexists,itbang:ir_exists}),parse_modifiers=function(tree,n,b,tb){return(function(){var r=null;
return((r=qsw.match(tree))?tb(r._x):(r=qsx.match(tree))?b(r._x):n(tree))}).call(this)},render_form=function(xs,body,form,iform){return(function(){var r=null,use=function(f,match){return f.replace($.merge({_f:match._x,_init:match._init,_s:xs},names_for(match._var)))
},promote=function(f,body){return((f).replace({_f:(f.uses_x0?qsy:qsz).replace($.merge({_f:body},gensym_names)),_s:xs})).replace(gensym_names)};return((r=qs10.match(body)||qs11.match(body))?use(iform,r):(r=qs12.match(body)||qs13.match(body))?use(form,r):promote(form,body))
}).call(this)},names_for=function(p){return p?{_x:p,_x0:(""+(p)+"0"),_xi:(""+(p)+"i"),_xl:(""+(p)+"l"),_xs:(""+(p)+"s"),_xr:(""+(p)+"r")}:{_x:"x",_x0:"x0",_xi:"xi",_xl:"xl",_xs:"xs",_xr:"xr"}
},gensym_names=(function(xs1){var x1,x0,xi,xl,xr;var xr=new xs1.constructor();for(var k in xs1){if(Object.prototype.hasOwnProperty.call(xs1,k)){x1=xs1[k],xr[k]=($.gensym(x1))
}}return xr}).call(this,names_for(null));return[rule(qs14,qs15),rule(qs16,concat),rule(qs17,qs18),rule(qs19,qs1a),rule(qs1b,qs1c),rule(qs1d,qs1e),rule(qs1f,qs1g),rule(qs1h,qs1i),rule(qs1j,qs1k),rule(qs1l,qs1m),rule(qs1n,qs1o),rule(qs1p,qs1q),rule(qs1r,qs1s),rule(qs1t,qs1u),rule(qs1v,filter_forms),rule(qs1w,map_forms),rule(qs1x,fold_forms),rule(qs1y,exists_forms),rule(qs1z,kmap_forms),rule(qs20,vmap_forms),rule(qs21,kfilter_forms),rule(qs22,vfilter_forms)]
}).call(this)}).call(this),word_macros=(function(){var n=function(match){return n_pattern.replace($.merge({_l:"0",_step:"1"},match))},ni=function(match){return ni_pattern.replace($.merge({_l:"0",_step:"1"},match))
},scope=anon(qs23),scoped=function(t){return scope.replace({_body:t})},form=function(p){return(function(){var tree=scoped(anon(p));return function(_){return tree.replace(_)
}}).call(this)},n_pattern=anon(qs24),ni_pattern=anon(qs25),keys=form(qs26),values=form(qs27),pairs=form(qs28),object=form(qs29),mobject=form(qs2a);return[rule(qs2b,n),rule(qs2c,ni),rule(qs2d,n),rule(qs2e,ni),rule(qs2f,n),rule(qs2g,ni),rule(qs2h,keys),rule(qs2i,object),rule(qs2j,mobject),rule(qs2k,values),rule(qs2l,object),rule(qs2m,mobject),rule(qs2n,pairs),rule(qs2o,object),rule(qs2p,mobject)]
}).call(this);return(operator_macros).concat(word_macros)}).call(this)})),it}).call(this,(caterwaul_function))}});result.caterwaul_expression_ref_table={qs:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_expression"))'),qs1:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_xs")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_x0") .prefix( " ")) ,new caterwaul.syntax( "_xi") .prefix( " ")) ,new caterwaul.syntax( "_xl") .prefix( " ")) ,new caterwaul.syntax( "_xr") .prefix( " "))) ,new caterwaul.syntax( "_body") .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") .prefix( " ") ,new caterwaul.syntax( "_s"))))'),qs2:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f"))))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( "                                        "))'),qs3:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( "                              ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( "                                                  "))'),qs4:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "apply")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xr") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Array") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "slice")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f"))))))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qs5:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "_x0") .prefix( " ")) ,new caterwaul.syntax( "_xl") .prefix( " "))) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_x") .prefix( " ")) .prefix( " "))'),qs6:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( "    ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "_x"))) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( "      "))'),qs7:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( "    ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "||" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "_x"))) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( "      "))'),qs8:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "y") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "y") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "y"))) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qs9:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "_x0") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "_f"))) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsa:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "0"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "1") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( "            ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_x0") .prefix( " ")) .prefix( " "))'),qsb:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "2") .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_xl") ,new caterwaul.syntax( "1") .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u--" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_x0") .prefix( " ")) .prefix( " "))'),qsc:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "!==" ,new caterwaul.syntax( "_x") .prefix( "                      ") ,new caterwaul.syntax( "null") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( "   "))'),qsd:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( "      ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_x0") .prefix( " ")) .prefix( "      "))'),qse:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length")) ,new caterwaul.syntax( "1") .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u--" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_x0") .prefix( " ")) .prefix( "      "))'),qsf:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "_x0") .prefix( " "))) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") .prefix( "          ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( "        "))'),qsg:('new caterwaul.syntax( "i;" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "x") .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "false") .prefix( " ")) .prefix( " "))'),qsh:('new caterwaul.syntax( "i;" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "false") .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "true") .prefix( " ")) .prefix( " "))'),qsi:('new caterwaul.syntax( "i;" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "1") .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "_x") .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u--" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "x") .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "false") .prefix( " ")) .prefix( " "))'),qsj:('new caterwaul.syntax( "i;" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "_xl") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_f") .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "false") .prefix( " ")) .prefix( " "))'),qsk:('new caterwaul.syntax( "i;" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_xl") .prefix( " ") ,new caterwaul.syntax( "1") .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "_x") .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "_xi") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u--" ,new caterwaul.syntax( "_xi")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_xi"))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x0") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_init")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_f") .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "false") .prefix( " ")) .prefix( " "))'),qsl:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_xs"))) ,new caterwaul.syntax( "concat")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_ys"))))'),qsm:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_x") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "_f")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_x"))) .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsn:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_x") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "_f") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( "                "))'),qso:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_x") .prefix( " "))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( "      ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") .prefix( "  ") ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_x"))) .prefix( " ")) .prefix( " ")) .prefix( "    ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsp:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_x") .prefix( " "))) ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( "    ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") .prefix( "  ") ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_x"))) .prefix( " ")) .prefix( " ")) .prefix( "    ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsq:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_x") .prefix( " "))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "_x"))) .prefix( "  ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsr:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( "  ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "k"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "k")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " "))) .prefix( " ")) .prefix( "    ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qss:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( "  ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "k"))) .prefix( " ") ,new caterwaul.syntax( "_f") .prefix( " "))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( "            "))'),qst:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( "  ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "k"))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( "        ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") ,new caterwaul.syntax( "k")) ,new caterwaul.syntax( "_x") .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) .prefix( "    ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsu:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( "  ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "k"))) .prefix( " ") ,new caterwaul.syntax( "||" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( "        ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") ,new caterwaul.syntax( "k")) ,new caterwaul.syntax( "_x") .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) .prefix( "    ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsv:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_xr") .prefix( " ") ,new caterwaul.syntax( "new" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "constructor")) ,new caterwaul.syntax( ""))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( "  ") ,new caterwaul.syntax( "_xs") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k") .prefix( " ")))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xs") .prefix( " ") ,new caterwaul.syntax( "k"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_f")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "x") .prefix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_xr") ,new caterwaul.syntax( "k")) ,new caterwaul.syntax( "x") .prefix( "  ")) .prefix( " ")) .prefix( " ")) .prefix( " "))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_xr") .prefix( " ")) .prefix( " "))'),qsw:('new caterwaul.syntax( "u~" ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "_x")))'),qsx:('new caterwaul.syntax( "u!" ,new caterwaul.syntax( "_x"))'),qsy:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_x0")))'),qsz:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "_f") ,new caterwaul.syntax( "_x"))'),qs10:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_var@0") ,new caterwaul.syntax( "_init")) ,new caterwaul.syntax( "_x"))'),qs11:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "_init")) ,new caterwaul.syntax( "_x"))'),qs12:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_var@0") ,new caterwaul.syntax( "_x"))'),qs13:('new caterwaul.syntax( "[" ,new caterwaul.syntax( "_x"))'),qs14:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x"))'),qs15:('new caterwaul.syntax( "_x")'),qs16:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_ys") .prefix( " ")) .prefix( " "))'),qs17:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_x")))'),qs18:('new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x")))'),qs19:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y")))'),qs1a:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "_y"))'),qs1b:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_ys")))'),qs1c:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_xs")) ,new caterwaul.syntax( "_ys"))'),qs1d:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "_x")))'),qs1e:('new caterwaul.syntax( "[" ,new caterwaul.syntax( "_x"))'),qs1f:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y") .prefix( " ")))'),qs1g:('new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") .prefix( " ") ,new caterwaul.syntax( "_y")))'),qs1h:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_p")))'),qs1i:('new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_xs")) ,new caterwaul.syntax( "_p"))'),qs1j:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "u~" ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "_x"))))'),qs1k:('new caterwaul.syntax( "[" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x")))'),qs1l:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "u~" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_ys"))))'),qs1m:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_xs")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_ys")))'),qs1n:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "?" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y") .prefix( " ") .infix( " ") ,new caterwaul.syntax( "_z") .prefix( " ")) .prefix( " "))'),qs1o:('new caterwaul.syntax( "?" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x"))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_y"))) .prefix( " ") .infix( " ") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_z"))) .prefix( " ")) .prefix( " ")'),qs1p:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " "))'),qs1q:('new caterwaul.syntax( "&&" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x"))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_y"))) .prefix( " ")) .prefix( " ")'),qs1r:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "||" ,new caterwaul.syntax( "_x") ,new caterwaul.syntax( "_y") .prefix( " ")) .prefix( " "))'),qs1s:('new caterwaul.syntax( "||" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_x"))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_y"))) .prefix( " ")) .prefix( " ")'),qs1t:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "u+" ,new caterwaul.syntax( "_xs")))'),qs1u:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Array") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "slice")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_xs"))))'),qs1v:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_thing")) .prefix( " "))'),qs1w:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_thing")) .prefix( " "))'),qs1x:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_thing")) .prefix( " "))'),qs1y:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "|" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "_thing")) .prefix( " "))'),qs1z:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k")) .prefix( " ") ,new caterwaul.syntax( "_thing")))'),qs20:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "v")) .prefix( " ") ,new caterwaul.syntax( "_thing")))'),qs21:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "k")) .prefix( " ") ,new caterwaul.syntax( "_thing")))'),qs22:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_xs") ,new caterwaul.syntax( "v")) .prefix( " ") ,new caterwaul.syntax( "_thing")))'),qs23:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "o")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "_body")) .prefix( " "))) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "this") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_o"))) .prefix( " ")))'),qs24:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "i") ,new caterwaul.syntax( "u") .prefix( " ")) ,new caterwaul.syntax( "s") .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "<=" ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "u") ,new caterwaul.syntax( "i") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "s") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( "      ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "d") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "u") .prefix( " ") ,new caterwaul.syntax( "i") .prefix( " ")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "?" ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "d") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "u") .prefix( "  ") .infix( " ")) .prefix( " ") ,new caterwaul.syntax( ">" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "u") .prefix( "  ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "+=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "s") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "i"))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "r") .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_l")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_u")) .prefix( " ")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_step")) .prefix( " ")))'),qs25:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "i") ,new caterwaul.syntax( "u") .prefix( " ")) ,new caterwaul.syntax( "s") .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "if" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "||" ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "u") ,new caterwaul.syntax( "i") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "s") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "s")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "d") .prefix( " ") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "u") .prefix( " ") ,new caterwaul.syntax( "i") .prefix( " ")) .prefix( " ")) .prefix( " "))) ,new caterwaul.syntax( "?" ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "d") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "<=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "u") .prefix( " ") .infix( " ")) .prefix( " ") ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "u") .prefix( " ")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "+=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "s") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "i"))) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "r") .prefix( " ")) .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_l")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_u")) .prefix( " ")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_step")) .prefix( " ")))'),qs26:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "ks") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( " ") ,new caterwaul.syntax( "o") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "o") ,new caterwaul.syntax( "k") .prefix( " "))) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "ks") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "k"))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "ks") .prefix( " ")) .prefix( " "))'),qs27:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "vs") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( " ") ,new caterwaul.syntax( "o") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "o") ,new caterwaul.syntax( "k") .prefix( " "))) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "vs") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "o") ,new caterwaul.syntax( "k")))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "vs") .prefix( " ")) .prefix( " "))'),qs28:('new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "ps") .prefix( " ") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "in" ,new caterwaul.syntax( "k") .prefix( " ") ,new caterwaul.syntax( "o") .prefix( " ")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "&&" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Object") .prefix( " ") ,new caterwaul.syntax( "prototype")) ,new caterwaul.syntax( "hasOwnProperty")) ,new caterwaul.syntax( "call")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "o") ,new caterwaul.syntax( "k") .prefix( " "))) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "ps") .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "k") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "o") .prefix( " ") ,new caterwaul.syntax( "k")))))) .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "ps") .prefix( " ")) .prefix( " "))'),qs29:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "l") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "o") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "l") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "i")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "o") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "0"))) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "x") .prefix( " ") ,new caterwaul.syntax( "1"))) .prefix( " "))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "r") .prefix( " ")) .prefix( " "))'),qs2a:('new caterwaul.syntax( ";" ,new caterwaul.syntax( "for" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( ";" ,new caterwaul.syntax( "var" ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "r") .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "0") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "l") .prefix( " ") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "o") .prefix( " ") ,new caterwaul.syntax( "length"))) .prefix( " ")) ,new caterwaul.syntax( "x") .prefix( " "))) ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "i") .prefix( " ") ,new caterwaul.syntax( "l") .prefix( " ")) .prefix( " ")) ,new caterwaul.syntax( "u++" ,new caterwaul.syntax( "i")) .prefix( " "))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "x") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "o") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "||" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "r") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "0"))) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "r") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "0"))) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "")) .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "push")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "1"))))) ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "r") .prefix( " ")) .prefix( " "))'),qs2b:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "n") ,new caterwaul.syntax( "_u")))'),qs2c:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "ni") ,new caterwaul.syntax( "_u")))'),qs2d:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "n") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_l") ,new caterwaul.syntax( "_u") .prefix( " "))))'),qs2e:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "ni") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_l") ,new caterwaul.syntax( "_u") .prefix( " "))))'),qs2f:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "n") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_l") ,new caterwaul.syntax( "_u") .prefix( " ")) ,new caterwaul.syntax( "_step") .prefix( " "))))'),qs2g:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "ni") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_l") ,new caterwaul.syntax( "_u") .prefix( " ")) ,new caterwaul.syntax( "_step") .prefix( " "))))'),qs2h:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "keys")) .prefix( " "))'),qs2i:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "|" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "object")) .prefix( " "))'),qs2j:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "mobject")) .prefix( " "))'),qs2k:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "values")) .prefix( " "))'),qs2l:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "object")) .prefix( " "))'),qs2m:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "mobject")) .prefix( " "))'),qs2n:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "pairs")) .prefix( " "))'),qs2o:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "object")) .prefix( " "))'),qs2p:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "|" ,new caterwaul.syntax( "_o") ,new caterwaul.syntax( "mobject")) .prefix( " "))')};
return(result)}).call(this,new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_expression")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("_xs"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_x0")).prefix(" ")),(new caterwaul.syntax("_xi")).prefix(" ")),(new caterwaul.syntax("_xl")).prefix(" ")),(new caterwaul.syntax("_xr")).prefix(" "))),(new caterwaul.syntax("_body")).prefix(" ")))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("this"),new caterwaul.syntax("[]",(new caterwaul.syntax("S")).prefix(" "),new caterwaul.syntax("_s")))),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("(",new caterwaul.syntax("_f"))))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix("                                        ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix("                              "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xs")).prefix(" "))).prefix("                                                  ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("apply")),new caterwaul.syntax(",",new caterwaul.syntax("_xr"),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("Array")).prefix(" "),new caterwaul.syntax("prototype")),new caterwaul.syntax("slice")),new caterwaul.syntax("call")),new caterwaul.syntax("(",new caterwaul.syntax("_f"))))))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("_x0")).prefix(" ")),(new caterwaul.syntax("_xl")).prefix(" "))),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_x")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix("    "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("&&",(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("_x")))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix("      ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix("    "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("||",(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("_x")))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix("      ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("y")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("&&",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("y"),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("y")))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("_x0")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("&&",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("_x0"),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" "))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("_f")))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("0")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("1")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix("            "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_x0")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("-",(new caterwaul.syntax("_xl")).prefix(" "),(new caterwaul.syntax("2")).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),(new caterwaul.syntax("-",new caterwaul.syntax("_xl"),(new caterwaul.syntax("1")).prefix(" "))).prefix(" ")))).prefix(" "))),(new caterwaul.syntax(">=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u--",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_x0")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "))),(new caterwaul.syntax("!==",(new caterwaul.syntax("_x")).prefix("                      "),(new caterwaul.syntax("null")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("_x")),(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix("   ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix("      "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_x0")).prefix(" "))).prefix("      ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),(new caterwaul.syntax("-",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")),(new caterwaul.syntax("1")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax(">=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u--",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_x0")).prefix(" "))).prefix("      ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("_x0")).prefix(" "))),(new caterwaul.syntax("=",(new caterwaul.syntax("_x0")).prefix("          "),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("_x")),(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix("        ")),new caterwaul.syntax("i;",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",(new caterwaul.syntax("=",new caterwaul.syntax("_x"),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("x"),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("x")).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("false")).prefix(" "))).prefix(" ")),new caterwaul.syntax("i;",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",(new caterwaul.syntax("=",new caterwaul.syntax("_x"),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("x"),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("false")).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("true")).prefix(" "))).prefix(" ")),new caterwaul.syntax("i;",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("-",(new caterwaul.syntax("_xl")).prefix(" "),(new caterwaul.syntax("1")).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("_x")).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax(">=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u--",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",(new caterwaul.syntax("=",new caterwaul.syntax("_x"),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("x"),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("x")).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("false")).prefix(" "))).prefix(" ")),new caterwaul.syntax("i;",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("_xl")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",(new caterwaul.syntax("=",new caterwaul.syntax("_x"),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("_x0"),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("_f")).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("false")).prefix(" "))).prefix(" ")),new caterwaul.syntax("i;",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xl")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("length")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("-",(new caterwaul.syntax("_xl")).prefix(" "),(new caterwaul.syntax("1")).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("_x")).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax(">=",(new caterwaul.syntax("_xi")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u--",new caterwaul.syntax("_xi"))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",(new caterwaul.syntax("=",new caterwaul.syntax("_x"),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_xi")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("_x0"),(new caterwaul.syntax("(",new caterwaul.syntax("_init"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("_f")).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("false")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_xs"))),new caterwaul.syntax("concat")),new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_ys")))),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_x")).prefix(" "))))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("_f")),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_x")))).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_x")).prefix(" "))))).prefix(" "),(new caterwaul.syntax("_f")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xs")).prefix(" "))).prefix("                ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_x")).prefix(" "))),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix("      "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("_xr")).prefix("  "),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_x")))).prefix(" "))).prefix(" "))).prefix("    ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_x")).prefix(" "))),(new caterwaul.syntax("u!",(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix("    "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("_xr")).prefix("  "),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_x")))).prefix(" "))).prefix(" "))).prefix("    ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("_x")).prefix(" "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_x")).prefix(" "))),(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("x"),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("x")),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("_x")))).prefix("  "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix("  "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("k")).prefix(" "))))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("k")))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("_xr")).prefix(" "),new caterwaul.syntax("k")),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" ")))).prefix(" "))).prefix("    ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix("  "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("k")).prefix(" "))))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("k")))).prefix(" "),(new caterwaul.syntax("_f")).prefix(" ")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xs")).prefix(" "))).prefix("            ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix("  "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("k")).prefix(" "))))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("k")))).prefix(" "),(new caterwaul.syntax("&&",(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix("        "),(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("[]",new caterwaul.syntax("_xr"),new caterwaul.syntax("k")),(new caterwaul.syntax("_x")).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" "))).prefix("    ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix("  "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("k")).prefix(" "))))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("k")))).prefix(" "),(new caterwaul.syntax("||",(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix("        "),(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("[]",new caterwaul.syntax("_xr"),new caterwaul.syntax("k")),(new caterwaul.syntax("_x")).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" "))).prefix("    ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_xr")).prefix(" "),(new caterwaul.syntax("new",new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("constructor")),new caterwaul.syntax("")))).prefix(" "))).prefix(" "),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix("  "),(new caterwaul.syntax("_xs")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("if",(new caterwaul.syntax("(",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Object"),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("_xs"),(new caterwaul.syntax("k")).prefix(" "))))).prefix(" "),new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("_x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("_xs")).prefix(" "),new caterwaul.syntax("k")))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("x")).prefix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("_f"))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("&&",(new caterwaul.syntax("x")).prefix(" "),(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("[]",new caterwaul.syntax("_xr"),new caterwaul.syntax("k")),(new caterwaul.syntax("x")).prefix("  "))).prefix(" "))).prefix(" "))).prefix(" ")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("_xr")).prefix(" "))).prefix(" ")),new caterwaul.syntax("u~",new caterwaul.syntax("u!",new caterwaul.syntax("_x"))),new caterwaul.syntax("u!",new caterwaul.syntax("_x")),new caterwaul.syntax("()",new caterwaul.syntax("_f"),new caterwaul.syntax(",",new caterwaul.syntax("_x"),new caterwaul.syntax("_x0"))),new caterwaul.syntax("()",new caterwaul.syntax("_f"),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",new caterwaul.syntax("[]",new caterwaul.syntax("_var@0"),new caterwaul.syntax("_init")),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",new caterwaul.syntax("[",new caterwaul.syntax("_init")),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",new caterwaul.syntax("_var@0"),new caterwaul.syntax("_x")),new caterwaul.syntax("[",new caterwaul.syntax("_x")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x")),new caterwaul.syntax("_x"),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("+",new caterwaul.syntax("_xs"),(new caterwaul.syntax("_ys")).prefix(" "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("(",new caterwaul.syntax("_x"))),new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("_x"),new caterwaul.syntax("_y"))),new caterwaul.syntax("[]",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x")),new caterwaul.syntax("_y")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("()",new caterwaul.syntax("_xs"),new caterwaul.syntax("_ys"))),new caterwaul.syntax("()",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_xs")),new caterwaul.syntax("_ys")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[",new caterwaul.syntax("_x"))),new caterwaul.syntax("[",new caterwaul.syntax("_x")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax(",",new caterwaul.syntax("_x"),(new caterwaul.syntax("_y")).prefix(" "))),new caterwaul.syntax(",",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x")),new caterwaul.syntax("[]",(new caterwaul.syntax("S")).prefix(" "),new caterwaul.syntax("_y"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax(".",new caterwaul.syntax("_xs"),new caterwaul.syntax("_p"))),new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_xs")),new caterwaul.syntax("_p")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("u~",new caterwaul.syntax("[",new caterwaul.syntax("_x")))),new caterwaul.syntax("[",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("u~",new caterwaul.syntax("()",new caterwaul.syntax("_xs"),new caterwaul.syntax("_ys")))),new caterwaul.syntax("()",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_xs")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_ys"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("?",new caterwaul.syntax("_x"),(new caterwaul.syntax("_y")).prefix(" ").infix(" "),(new caterwaul.syntax("_z")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("?",new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x"))),(new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_y")))).prefix(" ").infix(" "),(new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_z")))).prefix(" "))).prefix(" "),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("&&",new caterwaul.syntax("_x"),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("&&",new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x"))),(new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_y")))).prefix(" "))).prefix(" "),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("||",new caterwaul.syntax("_x"),(new caterwaul.syntax("_y")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("||",new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_x"))),(new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_y")))).prefix(" "))).prefix(" "),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("u+",new caterwaul.syntax("_xs"))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax("Array"),new caterwaul.syntax("prototype")),new caterwaul.syntax("slice")),new caterwaul.syntax("call")),new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_xs")))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("%",new caterwaul.syntax("_xs"),new caterwaul.syntax("_thing"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("*",new caterwaul.syntax("_xs"),new caterwaul.syntax("_thing"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_xs"),new caterwaul.syntax("_thing"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("|",new caterwaul.syntax("_xs"),new caterwaul.syntax("_thing"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("*",(new caterwaul.syntax("%",new caterwaul.syntax("_xs"),new caterwaul.syntax("k"))).prefix(" "),new caterwaul.syntax("_thing"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("*",(new caterwaul.syntax("%",new caterwaul.syntax("_xs"),new caterwaul.syntax("v"))).prefix(" "),new caterwaul.syntax("_thing"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("%",(new caterwaul.syntax("%",new caterwaul.syntax("_xs"),new caterwaul.syntax("k"))).prefix(" "),new caterwaul.syntax("_thing"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("%",(new caterwaul.syntax("%",new caterwaul.syntax("_xs"),new caterwaul.syntax("v"))).prefix(" "),new caterwaul.syntax("_thing"))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax("o"))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax("_body"))).prefix(" "))),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("this"),(new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_o")))).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("i"),(new caterwaul.syntax("u")).prefix(" ")),(new caterwaul.syntax("s")).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("<=",(new caterwaul.syntax("*",new caterwaul.syntax("(",(new caterwaul.syntax("-",new caterwaul.syntax("u"),(new caterwaul.syntax("i")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("s")).prefix(" "))).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix("      ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("r")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("d")).prefix(" "),(new caterwaul.syntax("-",(new caterwaul.syntax("u")).prefix(" "),(new caterwaul.syntax("i")).prefix(" "))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("?",(new caterwaul.syntax(">=",(new caterwaul.syntax("d")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "),(new caterwaul.syntax("<",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("u")).prefix("  ").infix(" "))).prefix(" "),(new caterwaul.syntax(">",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("u")).prefix("  "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("+=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("s")).prefix(" "))).prefix(" ")))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("r")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("i")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("r")).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("(",new caterwaul.syntax("_l")),(new caterwaul.syntax("(",new caterwaul.syntax("_u"))).prefix(" ")),(new caterwaul.syntax("(",new caterwaul.syntax("_step"))).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax("(",new caterwaul.syntax("function",(new caterwaul.syntax("(",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("i"),(new caterwaul.syntax("u")).prefix(" ")),(new caterwaul.syntax("s")).prefix(" ")))).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("if",(new caterwaul.syntax("(",(new caterwaul.syntax("||",(new caterwaul.syntax("<",(new caterwaul.syntax("*",new caterwaul.syntax("(",(new caterwaul.syntax("-",new caterwaul.syntax("u"),(new caterwaul.syntax("i")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("s")).prefix(" "))).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "),(new caterwaul.syntax("u!",new caterwaul.syntax("s"))).prefix(" "))).prefix(" "))).prefix(" "),(new caterwaul.syntax("return",(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("r")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("d")).prefix(" "),(new caterwaul.syntax("-",(new caterwaul.syntax("u")).prefix(" "),(new caterwaul.syntax("i")).prefix(" "))).prefix(" "))).prefix(" "))),(new caterwaul.syntax("?",(new caterwaul.syntax(">=",(new caterwaul.syntax("d")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" "),(new caterwaul.syntax("<=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("u")).prefix(" ").infix(" "))).prefix(" "),(new caterwaul.syntax(">=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("u")).prefix(" "))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("+=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("s")).prefix(" "))).prefix(" ")))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("r")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("i")))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("r")).prefix(" "))).prefix(" ")))).prefix(" "))),new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("(",new caterwaul.syntax("_l")),(new caterwaul.syntax("(",new caterwaul.syntax("_u"))).prefix(" ")),(new caterwaul.syntax("(",new caterwaul.syntax("_step"))).prefix(" "))),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("ks")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix(" "),(new caterwaul.syntax("o")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("Object")).prefix(" "),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("o"),(new caterwaul.syntax("k")).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("ks")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("k")))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("ks")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("vs")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix(" "),(new caterwaul.syntax("o")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("Object")).prefix(" "),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("o"),(new caterwaul.syntax("k")).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("vs")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("[]",new caterwaul.syntax("o"),new caterwaul.syntax("k"))))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("vs")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",(new caterwaul.syntax("=",(new caterwaul.syntax("ps")).prefix(" "),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax("var",(new caterwaul.syntax("in",(new caterwaul.syntax("k")).prefix(" "),(new caterwaul.syntax("o")).prefix(" "))).prefix(" ")))).prefix(" "),(new caterwaul.syntax("&&",new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax(".",new caterwaul.syntax(".",(new caterwaul.syntax("Object")).prefix(" "),new caterwaul.syntax("prototype")),new caterwaul.syntax("hasOwnProperty")),new caterwaul.syntax("call")),new caterwaul.syntax(",",new caterwaul.syntax("o"),(new caterwaul.syntax("k")).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("ps")).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("[",new caterwaul.syntax(",",new caterwaul.syntax("k"),new caterwaul.syntax("[]",(new caterwaul.syntax("o")).prefix(" "),new caterwaul.syntax("k"))))))).prefix(" "))).prefix(" ")),(new caterwaul.syntax("return",(new caterwaul.syntax("ps")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("r")).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("l")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("o")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("l")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("i"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("o")).prefix(" "),new caterwaul.syntax("i")))).prefix(" "),(new caterwaul.syntax("=",new caterwaul.syntax("[]",(new caterwaul.syntax("r")).prefix(" "),new caterwaul.syntax("[]",new caterwaul.syntax("x"),new caterwaul.syntax("0"))),new caterwaul.syntax("[]",(new caterwaul.syntax("x")).prefix(" "),new caterwaul.syntax("1")))).prefix(" "))),(new caterwaul.syntax("return",(new caterwaul.syntax("r")).prefix(" "))).prefix(" ")),new caterwaul.syntax(";",new caterwaul.syntax("for",(new caterwaul.syntax("(",new caterwaul.syntax(";",new caterwaul.syntax(";",new caterwaul.syntax("var",new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("r")).prefix(" "),(new caterwaul.syntax("{",new caterwaul.syntax(""))).prefix(" "))).prefix(" "),(new caterwaul.syntax("=",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("0")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("=",(new caterwaul.syntax("l")).prefix(" "),new caterwaul.syntax(".",(new caterwaul.syntax("o")).prefix(" "),new caterwaul.syntax("length")))).prefix(" ")),(new caterwaul.syntax("x")).prefix(" "))),(new caterwaul.syntax("<",(new caterwaul.syntax("i")).prefix(" "),(new caterwaul.syntax("l")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("u++",new caterwaul.syntax("i"))).prefix(" ")))).prefix(" "),new caterwaul.syntax(",",(new caterwaul.syntax("=",(new caterwaul.syntax("x")).prefix(" "),new caterwaul.syntax("[]",(new caterwaul.syntax("o")).prefix(" "),new caterwaul.syntax("i")))).prefix(" "),new caterwaul.syntax("()",new caterwaul.syntax(".",(new caterwaul.syntax("(",(new caterwaul.syntax("||",new caterwaul.syntax("[]",new caterwaul.syntax("r"),new caterwaul.syntax("[]",new caterwaul.syntax("x"),new caterwaul.syntax("0"))),(new caterwaul.syntax("(",(new caterwaul.syntax("=",new caterwaul.syntax("[]",new caterwaul.syntax("r"),new caterwaul.syntax("[]",new caterwaul.syntax("x"),new caterwaul.syntax("0"))),(new caterwaul.syntax("[",new caterwaul.syntax(""))).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" "))).prefix(" "),new caterwaul.syntax("push")),new caterwaul.syntax("[]",new caterwaul.syntax("x"),new caterwaul.syntax("1"))))),(new caterwaul.syntax("return",(new caterwaul.syntax("r")).prefix(" "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("n"),new caterwaul.syntax("_u"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("ni"),new caterwaul.syntax("_u"))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("n"),new caterwaul.syntax(",",new caterwaul.syntax("_l"),(new caterwaul.syntax("_u")).prefix(" ")))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("ni"),new caterwaul.syntax(",",new caterwaul.syntax("_l"),(new caterwaul.syntax("_u")).prefix(" ")))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("n"),new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("_l"),(new caterwaul.syntax("_u")).prefix(" ")),(new caterwaul.syntax("_step")).prefix(" ")))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("[]",new caterwaul.syntax("ni"),new caterwaul.syntax(",",new caterwaul.syntax(",",new caterwaul.syntax("_l"),(new caterwaul.syntax("_u")).prefix(" ")),(new caterwaul.syntax("_step")).prefix(" ")))),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_o"),new caterwaul.syntax("keys"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("|",new caterwaul.syntax("_o"),new caterwaul.syntax("object"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_o"),new caterwaul.syntax("mobject"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_o"),new caterwaul.syntax("values"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("-",new caterwaul.syntax("_o"),new caterwaul.syntax("object"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("-",new caterwaul.syntax("_o"),new caterwaul.syntax("mobject"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_o"),new caterwaul.syntax("pairs"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("/",new caterwaul.syntax("_o"),new caterwaul.syntax("object"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),(new caterwaul.syntax("|",new caterwaul.syntax("_o"),new caterwaul.syntax("mobject"))).prefix(" "))));
caterwaul.module("std",function($){$.js_all=function(){return this("js js_literals words seq")};$.all.push("js_all")});
11 core/jsplot/lib
jquery.min.js
jquery.mousewheel.min.js
axis.waul.sdoc
matrix.waul.sdoc
socket.waul.sdoc
render.waul.sdoc
interface.waul.sdoc
css
html
jsplot.js.sdoc
jsplot.pl.sdoc
4 core/jsplot/jquery.min.js
/*! jQuery v1.11.1 | (c) 2005, 2014 jQuery Foundation, Inc. | jquery.org/license */
!function(a,b){"object"==typeof module&&"object"==typeof module.exports?module.exports=a.document?b(a,!0):function(a){if(!a.document)throw new Error("jQuery requires a window with a document");return b(a)}:b(a)}("undefined"!=typeof window?window:this,function(a,b){var c=[],d=c.slice,e=c.concat,f=c.push,g=c.indexOf,h={},i=h.toString,j=h.hasOwnProperty,k={},l="1.11.1",m=function(a,b){return new m.fn.init(a,b)},n=/^[\s\uFEFF\xA0]+|[\s\uFEFF\xA0]+$/g,o=/^-ms-/,p=/-([\da-z])/gi,q=function(a,b){return b.toUpperCase()};m.fn=m.prototype={jquery:l,constructor:m,selector:"",length:0,toArray:function(){return d.call(this)},get:function(a){return null!=a?0>a?this[a+this.length]:this[a]:d.call(this)},pushStack:function(a){var b=m.merge(this.constructor(),a);return b.prevObject=this,b.context=this.context,b},each:function(a,b){return m.each(this,a,b)},map:function(a){return this.pushStack(m.map(this,function(b,c){return a.call(b,c,b)}))},slice:function(){return this.pushStack(d.apply(this,arguments))},first:function(){return this.eq(0)},last:function(){return this.eq(-1)},eq:function(a){var b=this.length,c=+a+(0>a?b:0);return this.pushStack(c>=0&&b>c?[this[c]]:[])},end:function(){return this.prevObject||this.constructor(null)},push:f,sort:c.sort,splice:c.splice},m.extend=m.fn.extend=function(){var a,b,c,d,e,f,g=arguments[0]||{},h=1,i=arguments.length,j=!1;for("boolean"==typeof g&&(j=g,g=arguments[h]||{},h++),"object"==typeof g||m.isFunction(g)||(g={}),h===i&&(g=this,h--);i>h;h++)if(null!=(e=arguments[h]))for(d in e)a=g[d],c=e[d],g!==c&&(j&&c&&(m.isPlainObject(c)||(b=m.isArray(c)))?(b?(b=!1,f=a&&m.isArray(a)?a:[]):f=a&&m.isPlainObject(a)?a:{},g[d]=m.extend(j,f,c)):void 0!==c&&(g[d]=c));return g},m.extend({expando:"jQuery"+(l+Math.random()).replace(/\D/g,""),isReady:!0,error:function(a){throw new Error(a)},noop:function(){},isFunction:function(a){return"function"===m.type(a)},isArray:Array.isArray||function(a){return"array"===m.type(a)},isWindow:function(a){return null!=a&&a==a.window},isNumeric:function(a){return!m.isArray(a)&&a-parseFloat(a)>=0},isEmptyObject:function(a){var b;for(b in a)return!1;return!0},isPlainObject:function(a){var b;if(!a||"object"!==m.type(a)||a.nodeType||m.isWindow(a))return!1;try{if(a.constructor&&!j.call(a,"constructor")&&!j.call(a.constructor.prototype,"isPrototypeOf"))return!1}catch(c){return!1}if(k.ownLast)for(b in a)return j.call(a,b);for(b in a);return void 0===b||j.call(a,b)},type:function(a){return null==a?a+"":"object"==typeof a||"function"==typeof a?h[i.call(a)]||"object":typeof a},globalEval:function(b){b&&m.trim(b)&&(a.execScript||function(b){a.eval.call(a,b)})(b)},camelCase:function(a){return a.replace(o,"ms-").replace(p,q)},nodeName:function(a,b){return a.nodeName&&a.nodeName.toLowerCase()===b.toLowerCase()},each:function(a,b,c){var d,e=0,f=a.length,g=r(a);if(c){if(g){for(;f>e;e++)if(d=b.apply(a[e],c),d===!1)break}else for(e in a)if(d=b.apply(a[e],c),d===!1)break}else if(g){for(;f>e;e++)if(d=b.call(a[e],e,a[e]),d===!1)break}else for(e in a)if(d=b.call(a[e],e,a[e]),d===!1)break;return a},trim:function(a){return null==a?"":(a+"").replace(n,"")},makeArray:function(a,b){var c=b||[];return null!=a&&(r(Object(a))?m.merge(c,"string"==typeof a?[a]:a):f.call(c,a)),c},inArray:function(a,b,c){var d;if(b){if(g)return g.call(b,a,c);for(d=b.length,c=c?0>c?Math.max(0,d+c):c:0;d>c;c++)if(c in b&&b[c]===a)return c}return-1},merge:function(a,b){var c=+b.length,d=0,e=a.length;while(c>d)a[e++]=b[d++];if(c!==c)while(void 0!==b[d])a[e++]=b[d++];return a.length=e,a},grep:function(a,b,c){for(var d,e=[],f=0,g=a.length,h=!c;g>f;f++)d=!b(a[f],f),d!==h&&e.push(a[f]);return e},map:function(a,b,c){var d,f=0,g=a.length,h=r(a),i=[];if(h)for(;g>f;f++)d=b(a[f],f,c),null!=d&&i.push(d);else for(f in a)d=b(a[f],f,c),null!=d&&i.push(d);return e.apply([],i)},guid:1,proxy:function(a,b){var c,e,f;return"string"==typeof b&&(f=a[b],b=a,a=f),m.isFunction(a)?(c=d.call(arguments,2),e=function(){return a.apply(b||this,c.concat(d.call(arguments)))},e.guid=a.guid=a.guid||m.guid++,e):void 0},now:function(){return+new Date},support:k}),m.each("Boolean Number String Function Array Date RegExp Object Error".split(" "),function(a,b){h["[object "+b+"]"]=b.toLowerCase()});function r(a){var b=a.length,c=m.type(a);return"function"===c||m.isWindow(a)?!1:1===a.nodeType&&b?!0:"array"===c||0===b||"number"==typeof b&&b>0&&b-1 in a}var s=function(a){var b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u="sizzle"+-new Date,v=a.document,w=0,x=0,y=gb(),z=gb(),A=gb(),B=function(a,b){return a===b&&(l=!0),0},C="undefined",D=1<<31,E={}.hasOwnProperty,F=[],G=F.pop,H=F.push,I=F.push,J=F.slice,K=F.indexOf||function(a){for(var b=0,c=this.length;c>b;b++)if(this[b]===a)return b;return-1},L="checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",M="[\\x20\\t\\r\\n\\f]",N="(?:\\\\.|[\\w-]|[^\\x00-\\xa0])+",O=N.replace("w","w#"),P="\\["+M+"*("+N+")(?:"+M+"*([*^$|!~]?=)"+M+"*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|("+O+"))|)"+M+"*\\]",Q=":("+N+")(?:\\((('((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\")|((?:\\\\.|[^\\\\()[\\]]|"+P+")*)|.*)\\)|)",R=new RegExp("^"+M+"+|((?:^|[^\\\\])(?:\\\\.)*)"+M+"+$","g"),S=new RegExp("^"+M+"*,"+M+"*"),T=new RegExp("^"+M+"*([>+~]|"+M+")"+M+"*"),U=new RegExp("="+M+"*([^\\]'\"]*?)"+M+"*\\]","g"),V=new RegExp(Q),W=new RegExp("^"+O+"$"),X={ID:new RegExp("^#("+N+")"),CLASS:new RegExp("^\\.("+N+")"),TAG:new RegExp("^("+N.replace("w","w*")+")"),ATTR:new RegExp("^"+P),PSEUDO:new RegExp("^"+Q),CHILD:new RegExp("^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\("+M+"*(even|odd|(([+-]|)(\\d*)n|)"+M+"*(?:([+-]|)"+M+"*(\\d+)|))"+M+"*\\)|)","i"),bool:new RegExp("^(?:"+L+")$","i"),needsContext:new RegExp("^"+M+"*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\("+M+"*((?:-\\d)?\\d*)"+M+"*\\)|)(?=[^-]|$)","i")},Y=/^(?:input|select|textarea|button)$/i,Z=/^h\d$/i,$=/^[^{]+\{\s*\[native \w/,_=/^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,ab=/[+~]/,bb=/'|\\/g,cb=new RegExp("\\\\([\\da-f]{1,6}"+M+"?|("+M+")|.)","ig"),db=function(a,b,c){var d="0x"+b-65536;return d!==d||c?b:0>d?String.fromCharCode(d+65536):String.fromCharCode(d>>10|55296,1023&d|56320)};try{I.apply(F=J.call(v.childNodes),v.childNodes),F[v.childNodes.length].nodeType}catch(eb){I={apply:F.length?function(a,b){H.apply(a,J.call(b))}:function(a,b){var c=a.length,d=0;while(a[c++]=b[d++]);a.length=c-1}}}function fb(a,b,d,e){var f,h,j,k,l,o,r,s,w,x;if((b?b.ownerDocument||b:v)!==n&&m(b),b=b||n,d=d||[],!a||"string"!=typeof a)return d;if(1!==(k=b.nodeType)&&9!==k)return[];if(p&&!e){if(f=_.exec(a))if(j=f[1]){if(9===k){if(h=b.getElementById(j),!h||!h.parentNode)return d;if(h.id===j)return d.push(h),d}else if(b.ownerDocument&&(h=b.ownerDocument.getElementById(j))&&t(b,h)&&h.id===j)return d.push(h),d}else{if(f[2])return I.apply(d,b.getElementsByTagName(a)),d;if((j=f[3])&&c.getElementsByClassName&&b.getElementsByClassName)return I.apply(d,b.getElementsByClassName(j)),d}if(c.qsa&&(!q||!q.test(a))){if(s=r=u,w=b,x=9===k&&a,1===k&&"object"!==b.nodeName.toLowerCase()){o=g(a),(r=b.getAttribute("id"))?s=r.replace(bb,"\\$&"):b.setAttribute("id",s),s="[id='"+s+"'] ",l=o.length;while(l--)o[l]=s+qb(o[l]);w=ab.test(a)&&ob(b.parentNode)||b,x=o.join(",")}if(x)try{return I.apply(d,w.querySelectorAll(x)),d}catch(y){}finally{r||b.removeAttribute("id")}}}return i(a.replace(R,"$1"),b,d,e)}function gb(){var a=[];function b(c,e){return a.push(c+" ")>d.cacheLength&&delete b[a.shift()],b[c+" "]=e}return b}function hb(a){return a[u]=!0,a}function ib(a){var b=n.createElement("div");try{return!!a(b)}catch(c){return!1}finally{b.parentNode&&b.parentNode.removeChild(b),b=null}}function jb(a,b){var c=a.split("|"),e=a.length;while(e--)d.attrHandle[c[e]]=b}function kb(a,b){var c=b&&a,d=c&&1===a.nodeType&&1===b.nodeType&&(~b.sourceIndex||D)-(~a.sourceIndex||D);if(d)return d;if(c)while(c=c.nextSibling)if(c===b)return-1;return a?1:-1}function lb(a){return function(b){var c=b.nodeName.toLowerCase();return"input"===c&&b.type===a}}function mb(a){return function(b){var c=b.nodeName.toLowerCase();return("input"===c||"button"===c)&&b.type===a}}function nb(a){return hb(function(b){return b=+b,hb(function(c,d){var e,f=a([],c.length,b),g=f.length;while(g--)c[e=f[g]]&&(c[e]=!(d[e]=c[e]))})})}function ob(a){return a&&typeof a.getElementsByTagName!==C&&a}c=fb.support={},f=fb.isXML=function(a){var b=a&&(a.ownerDocument||a).documentElement;return b?"HTML"!==b.nodeName:!1},m=fb.setDocument=function(a){var b,e=a?a.ownerDocument||a:v,g=e.defaultView;return e!==n&&9===e.nodeType&&e.documentElement?(n=e,o=e.documentElement,p=!f(e),g&&g!==g.top&&(g.addEventListener?g.addEventListener("unload",function(){m()},!1):g.attachEvent&&g.attachEvent("onunload",function(){m()})),c.attributes=ib(function(a){return a.className="i",!a.getAttribute("className")}),c.getElementsByTagName=ib(function(a){return a.appendChild(e.createComment("")),!a.getElementsByTagName("*").length}),c.getElementsByClassName=$.test(e.getElementsByClassName)&&ib(function(a){return a.innerHTML="<div class='a'></div><div class='a i'></div>",a.firstChild.className="i",2===a.getElementsByClassName("i").length}),c.getById=ib(function(a){return o.appendChild(a).id=u,!e.getElementsByName||!e.getElementsByName(u).length}),c.getById?(d.find.ID=function(a,b){if(typeof b.getElementById!==C&&p){var c=b.getElementById(a);return c&&c.parentNode?[c]:[]}},d.filter.ID=function(a){var b=a.replace(cb,db);return function(a){return a.getAttribute("id")===b}}):(delete d.find.ID,d.filter.ID=function(a){var b=a.replace(cb,db);return function(a){var c=typeof a.getAttributeNode!==C&&a.getAttributeNode("id");return c&&c.value===b}}),d.find.TAG=c.getElementsByTagName?function(a,b){return typeof b.getElementsByTagName!==C?b.getElementsByTagName(a):void 0}:function(a,b){var c,d=[],e=0,f=b.getElementsByTagName(a);if("*"===a){while(c=f[e++])1===c.nodeType&&d.push(c);return d}return f},d.find.CLASS=c.getElementsByClassName&&function(a,b){return typeof b.getElementsByClassName!==C&&p?b.getElementsByClassName(a):void 0},r=[],q=[],(c.qsa=$.test(e.querySelectorAll))&&(ib(function(a){a.innerHTML="<select msallowclip=''><option selected=''></option></select>",a.querySelectorAll("[msallowclip^='']").length&&q.push("[*^$]="+M+"*(?:''|\"\")"),a.querySelectorAll("[selected]").length||q.push("\\["+M+"*(?:value|"+L+")"),a.querySelectorAll(":checked").length||q.push(":checked")}),ib(function(a){var b=e.createElement("input");b.setAttribute("type","hidden"),a.appendChild(b).setAttribute("name","D"),a.querySelectorAll("[name=d]").length&&q.push("name"+M+"*[*^$|!~]?="),a.querySelectorAll(":enabled").length||q.push(":enabled",":disabled"),a.querySelectorAll("*,:x"),q.push(",.*:")})),(c.matchesSelector=$.test(s=o.matches||o.webkitMatchesSelector||o.mozMatchesSelector||o.oMatchesSelector||o.msMatchesSelector))&&ib(function(a){c.disconnectedMatch=s.call(a,"div"),s.call(a,"[s!='']:x"),r.push("!=",Q)}),q=q.length&&new RegExp(q.join("|")),r=r.length&&new RegExp(r.join("|")),b=$.test(o.compareDocumentPosition),t=b||$.test(o.contains)?function(a,b){var c=9===a.nodeType?a.documentElement:a,d=b&&b.parentNode;return a===d||!(!d||1!==d.nodeType||!(c.contains?c.contains(d):a.compareDocumentPosition&&16&a.compareDocumentPosition(d)))}:function(a,b){if(b)while(b=b.parentNode)if(b===a)return!0;return!1},B=b?function(a,b){if(a===b)return l=!0,0;var d=!a.compareDocumentPosition-!b.compareDocumentPosition;return d?d:(d=(a.ownerDocument||a)===(b.ownerDocument||b)?a.compareDocumentPosition(b):1,1&d||!c.sortDetached&&b.compareDocumentPosition(a)===d?a===e||a.ownerDocument===v&&t(v,a)?-1:b===e||b.ownerDocument===v&&t(v,b)?1:k?K.call(k,a)-K.call(k,b):0:4&d?-1:1)}:function(a,b){if(a===b)return l=!0,0;var c,d=0,f=a.parentNode,g=b.parentNode,h=[a],i=[b];if(!f||!g)return a===e?-1:b===e?1:f?-1:g?1:k?K.call(k,a)-K.call(k,b):0;if(f===g)return kb(a,b);c=a;while(c=c.parentNode)h.unshift(c);c=b;while(c=c.parentNode)i.unshift(c);while(h[d]===i[d])d++;return d?kb(h[d],i[d]):h[d]===v?-1:i[d]===v?1:0},e):n},fb.matches=function(a,b){return fb(a,null,null,b)},fb.matchesSelector=function(a,b){if((a.ownerDocument||a)!==n&&m(a),b=b.replace(U,"='$1']"),!(!c.matchesSelector||!p||r&&r.test(b)||q&&q.test(b)))try{var d=s.call(a,b);if(d||c.disconnectedMatch||a.document&&11!==a.document.nodeType)return d}catch(e){}return fb(b,n,null,[a]).length>0},fb.contains=function(a,b){return(a.ownerDocument||a)!==n&&m(a),t(a,b)},fb.attr=function(a,b){(a.ownerDocument||a)!==n&&m(a);var e=d.attrHandle[b.toLowerCase()],f=e&&E.call(d.attrHandle,b.toLowerCase())?e(a,b,!p):void 0;return void 0!==f?f:c.attributes||!p?a.getAttribute(b):(f=a.getAttributeNode(b))&&f.specified?f.value:null},fb.error=function(a){throw new Error("Syntax error, unrecognized expression: "+a)},fb.uniqueSort=function(a){var b,d=[],e=0,f=0;if(l=!c.detectDuplicates,k=!c.sortStable&&a.slice(0),a.sort(B),l){while(b=a[f++])b===a[f]&&(e=d.push(f));while(e--)a.splice(d[e],1)}return k=null,a},e=fb.getText=function(a){var b,c="",d=0,f=a.nodeType;if(f){if(1===f||9===f||11===f){if("string"==typeof a.textContent)return a.textContent;for(a=a.firstChild;a;a=a.nextSibling)c+=e(a)}else if(3===f||4===f)return a.nodeValue}else while(b=a[d++])c+=e(b);return c},d=fb.selectors={cacheLength:50,createPseudo:hb,match:X,attrHandle:{},find:{},relative:{">":{dir:"parentNode",first:!0}," ":{dir:"parentNode"},"+":{dir:"previousSibling",first:!0},"~":{dir:"previousSibling"}},preFilter:{ATTR:function(a){return a[1]=a[1].replace(cb,db),a[3]=(a[3]||a[4]||a[5]||"").replace(cb,db),"~="===a[2]&&(a[3]=" "+a[3]+" "),a.slice(0,4)},CHILD:function(a){return a[1]=a[1].toLowerCase(),"nth"===a[1].slice(0,3)?(a[3]||fb.error(a[0]),a[4]=+(a[4]?a[5]+(a[6]||1):2*("even"===a[3]||"odd"===a[3])),a[5]=+(a[7]+a[8]||"odd"===a[3])):a[3]&&fb.error(a[0]),a},PSEUDO:function(a){var b,c=!a[6]&&a[2];return X.CHILD.test(a[0])?null:(a[3]?a[2]=a[4]||a[5]||"":c&&V.test(c)&&(b=g(c,!0))&&(b=c.indexOf(")",c.length-b)-c.length)&&(a[0]=a[0].slice(0,b),a[2]=c.slice(0,b)),a.slice(0,3))}},filter:{TAG:function(a){var b=a.replace(cb,db).toLowerCase();return"*"===a?function(){return!0}:function(a){return a.nodeName&&a.nodeName.toLowerCase()===b}},CLASS:function(a){var b=y[a+" "];return b||(b=new RegExp("(^|"+M+")"+a+"("+M+"|$)"))&&y(a,function(a){return b.test("string"==typeof a.className&&a.className||typeof a.getAttribute!==C&&a.getAttribute("class")||"")})},ATTR:function(a,b,c){return function(d){var e=fb.attr(d,a);return null==e?"!="===b:b?(e+="","="===b?e===c:"!="===b?e!==c:"^="===b?c&&0===e.indexOf(c):"*="===b?c&&e.indexOf(c)>-1:"$="===b?c&&e.slice(-c.length)===c:"~="===b?(" "+e+" ").indexOf(c)>-1:"|="===b?e===c||e.slice(0,c.length+1)===c+"-":!1):!0}},CHILD:function(a,b,c,d,e){var f="nth"!==a.slice(0,3),g="last"!==a.slice(-4),h="of-type"===b;return 1===d&&0===e?function(a){return!!a.parentNode}:function(b,c,i){var j,k,l,m,n,o,p=f!==g?"nextSibling":"previousSibling",q=b.parentNode,r=h&&b.nodeName.toLowerCase(),s=!i&&!h;if(q){if(f){while(p){l=b;while(l=l[p])if(h?l.nodeName.toLowerCase()===r:1===l.nodeType)return!1;o=p="only"===a&&!o&&"nextSibling"}return!0}if(o=[g?q.firstChild:q.lastChild],g&&s){k=q[u]||(q[u]={}),j=k[a]||[],n=j[0]===w&&j[1],m=j[0]===w&&j[2],l=n&&q.childNodes[n];while(l=++n&&l&&l[p]||(m=n=0)||o.pop())if(1===l.nodeType&&++m&&l===b){k[a]=[w,n,m];break}}else if(s&&(j=(b[u]||(b[u]={}))[a])&&j[0]===w)m=j[1];else while(l=++n&&l&&l[p]||(m=n=0)||o.pop())if((h?l.nodeName.toLowerCase()===r:1===l.nodeType)&&++m&&(s&&((l[u]||(l[u]={}))[a]=[w,m]),l===b))break;return m-=e,m===d||m%d===0&&m/d>=0}}},PSEUDO:function(a,b){var c,e=d.pseudos[a]||d.setFilters[a.toLowerCase()]||fb.error("unsupported pseudo: "+a);return e[u]?e(b):e.length>1?(c=[a,a,"",b],d.setFilters.hasOwnProperty(a.toLowerCase())?hb(function(a,c){var d,f=e(a,b),g=f.length;while(g--)d=K.call(a,f[g]),a[d]=!(c[d]=f[g])}):function(a){return e(a,0,c)}):e}},pseudos:{not:hb(function(a){var b=[],c=[],d=h(a.replace(R,"$1"));return d[u]?hb(function(a,b,c,e){var f,g=d(a,null,e,[]),h=a.length;while(h--)(f=g[h])&&(a[h]=!(b[h]=f))}):function(a,e,f){return b[0]=a,d(b,null,f,c),!c.pop()}}),has:hb(function(a){return function(b){return fb(a,b).length>0}}),contains:hb(function(a){return function(b){return(b.textContent||b.innerText||e(b)).indexOf(a)>-1}}),lang:hb(function(a){return W.test(a||"")||fb.error("unsupported lang: "+a),a=a.replace(cb,db).toLowerCase(),function(b){var c;do if(c=p?b.lang:b.getAttribute("xml:lang")||b.getAttribute("lang"))return c=c.toLowerCase(),c===a||0===c.indexOf(a+"-");while((b=b.parentNode)&&1===b.nodeType);return!1}}),target:function(b){var c=a.location&&a.location.hash;return c&&c.slice(1)===b.id},root:function(a){return a===o},focus:function(a){return a===n.activeElement&&(!n.hasFocus||n.hasFocus())&&!!(a.type||a.href||~a.tabIndex)},enabled:function(a){return a.disabled===!1},disabled:function(a){return a.disabled===!0},checked:function(a){var b=a.nodeName.toLowerCase();return"input"===b&&!!a.checked||"option"===b&&!!a.selected},selected:function(a){return a.parentNode&&a.parentNode.selectedIndex,a.selected===!0},empty:function(a){for(a=a.firstChild;a;a=a.nextSibling)if(a.nodeType<6)return!1;return!0},parent:function(a){return!d.pseudos.empty(a)},header:function(a){return Z.test(a.nodeName)},input:function(a){return Y.test(a.nodeName)},button:function(a){var b=a.nodeName.toLowerCase();return"input"===b&&"button"===a.type||"button"===b},text:function(a){var b;return"input"===a.nodeName.toLowerCase()&&"text"===a.type&&(null==(b=a.getAttribute("type"))||"text"===b.toLowerCase())},first:nb(function(){return[0]}),last:nb(function(a,b){return[b-1]}),eq:nb(function(a,b,c){return[0>c?c+b:c]}),even:nb(function(a,b){for(var c=0;b>c;c+=2)a.push(c);return a}),odd:nb(function(a,b){for(var c=1;b>c;c+=2)a.push(c);return a}),lt:nb(function(a,b,c){for(var d=0>c?c+b:c;--d>=0;)a.push(d);return a}),gt:nb(function(a,b,c){for(var d=0>c?c+b:c;++d<b;)a.push(d);return a})}},d.pseudos.nth=d.pseudos.eq;for(b in{radio:!0,checkbox:!0,file:!0,password:!0,image:!0})d.pseudos[b]=lb(b);for(b in{submit:!0,reset:!0})d.pseudos[b]=mb(b);function pb(){}pb.prototype=d.filters=d.pseudos,d.setFilters=new pb,g=fb.tokenize=function(a,b){var c,e,f,g,h,i,j,k=z[a+" "];if(k)return b?0:k.slice(0);h=a,i=[],j=d.preFilter;while(h){(!c||(e=S.exec(h)))&&(e&&(h=h.slice(e[0].length)||h),i.push(f=[])),c=!1,(e=T.exec(h))&&(c=e.shift(),f.push({value:c,type:e[0].replace(R," ")}),h=h.slice(c.length));for(g in d.filter)!(e=X[g].exec(h))||j[g]&&!(e=j[g](e))||(c=e.shift(),f.push({value:c,type:g,matches:e}),h=h.slice(c.length));if(!c)break}return b?h.length:h?fb.error(a):z(a,i).slice(0)};function qb(a){for(var b=0,c=a.length,d="";c>b;b++)d+=a[b].value;return d}function rb(a,b,c){var d=b.dir,e=c&&"parentNode"===d,f=x++;return b.first?function(b,c,f){while(b=b[d])if(1===b.nodeType||e)return a(b,c,f)}:function(b,c,g){var h,i,j=[w,f];if(g){while(b=b[d])if((1===b.nodeType||e)&&a(b,c,g))return!0}else while(b=b[d])if(1===b.nodeType||e){if(i=b[u]||(b[u]={}),(h=i[d])&&h[0]===w&&h[1]===f)return j[2]=h[2];if(i[d]=j,j[2]=a(b,c,g))return!0}}}function sb(a){return a.length>1?function(b,c,d){var e=a.length;while(e--)if(!a[e](b,c,d))return!1;return!0}:a[0]}function tb(a,b,c){for(var d=0,e=b.length;e>d;d++)fb(a,b[d],c);return c}function ub(a,b,c,d,e){for(var f,g=[],h=0,i=a.length,j=null!=b;i>h;h++)(f=a[h])&&(!c||c(f,d,e))&&(g.push(f),j&&b.push(h));return g}function vb(a,b,c,d,e,f){return d&&!d[u]&&(d=vb(d)),e&&!e[u]&&(e=vb(e,f)),hb(function(f,g,h,i){var j,k,l,m=[],n=[],o=g.length,p=f||tb(b||"*",h.nodeType?[h]:h,[]),q=!a||!f&&b?p:ub(p,m,a,h,i),r=c?e||(f?a:o||d)?[]:g:q;if(c&&c(q,r,h,i),d){j=ub(r,n),d(j,[],h,i),k=j.length;while(k--)(l=j[k])&&(r[n[k]]=!(q[n[k]]=l))}if(f){if(e||a){if(e){j=[],k=r.length;while(k--)(l=r[k])&&j.push(q[k]=l);e(null,r=[],j,i)}k=r.length;while(k--)(l=r[k])&&(j=e?K.call(f,l):m[k])>-1&&(f[j]=!(g[j]=l))}}else r=ub(r===g?r.splice(o,r.length):r),e?e(null,g,r,i):I.apply(g,r)})}function wb(a){for(var b,c,e,f=a.length,g=d.relative[a[0].type],h=g||d.relative[" "],i=g?1:0,k=rb(function(a){return a===b},h,!0),l=rb(function(a){return K.call(b,a)>-1},h,!0),m=[function(a,c,d){return!g&&(d||c!==j)||((b=c).nodeType?k(a,c,d):l(a,c,d))}];f>i;i++)if(c=d.relative[a[i].type])m=[rb(sb(m),c)];else{if(c=d.filter[a[i].type].apply(null,a[i].matches),c[u]){for(e=++i;f>e;e++)if(d.relative[a[e].type])break;return vb(i>1&&sb(m),i>1&&qb(a.slice(0,i-1).concat({value:" "===a[i-2].type?"*":""})).replace(R,"$1"),c,e>i&&wb(a.slice(i,e)),f>e&&wb(a=a.slice(e)),f>e&&qb(a))}m.push(c)}return sb(m)}function xb(a,b){var c=b.length>0,e=a.length>0,f=function(f,g,h,i,k){var l,m,o,p=0,q="0",r=f&&[],s=[],t=j,u=f||e&&d.find.TAG("*",k),v=w+=null==t?1:Math.random()||.1,x=u.length;for(k&&(j=g!==n&&g);q!==x&&null!=(l=u[q]);q++){if(e&&l){m=0;while(o=a[m++])if(o(l,g,h)){i.push(l);break}k&&(w=v)}c&&((l=!o&&l)&&p--,f&&r.push(l))}if(p+=q,c&&q!==p){m=0;while(o=b[m++])o(r,s,g,h);if(f){if(p>0)while(q--)r[q]||s[q]||(s[q]=G.call(i));s=ub(s)}I.apply(i,s),k&&!f&&s.length>0&&p+b.length>1&&fb.uniqueSort(i)}return k&&(w=v,j=t),r};return c?hb(f):f}return h=fb.compile=function(a,b){var c,d=[],e=[],f=A[a+" "];if(!f){b||(b=g(a)),c=b.length;while(c--)f=wb(b[c]),f[u]?d.push(f):e.push(f);f=A(a,xb(e,d)),f.selector=a}return f},i=fb.select=function(a,b,e,f){var i,j,k,l,m,n="function"==typeof a&&a,o=!f&&g(a=n.selector||a);if(e=e||[],1===o.length){if(j=o[0]=o[0].slice(0),j.length>2&&"ID"===(k=j[0]).type&&c.getById&&9===b.nodeType&&p&&d.relative[j[1].type]){if(b=(d.find.ID(k.matches[0].replace(cb,db),b)||[])[0],!b)return e;n&&(b=b.parentNode),a=a.slice(j.shift().value.length)}i=X.needsContext.test(a)?0:j.length;while(i--){if(k=j[i],d.relative[l=k.type])break;if((m=d.find[l])&&(f=m(k.matches[0].replace(cb,db),ab.test(j[0].type)&&ob(b.parentNode)||b))){if(j.splice(i,1),a=f.length&&qb(j),!a)return I.apply(e,f),e;break}}}return(n||h(a,o))(f,b,!p,e,ab.test(a)&&ob(b.parentNode)||b),e},c.sortStable=u.split("").sort(B).join("")===u,c.detectDuplicates=!!l,m(),c.sortDetached=ib(function(a){return 1&a.compareDocumentPosition(n.createElement("div"))}),ib(function(a){return a.innerHTML="<a href='#'></a>","#"===a.firstChild.getAttribute("href")})||jb("type|href|height|width",function(a,b,c){return c?void 0:a.getAttribute(b,"type"===b.toLowerCase()?1:2)}),c.attributes&&ib(function(a){return a.innerHTML="<input/>",a.firstChild.setAttribute("value",""),""===a.firstChild.getAttribute("value")})||jb("value",function(a,b,c){return c||"input"!==a.nodeName.toLowerCase()?void 0:a.defaultValue}),ib(function(a){return null==a.getAttribute("disabled")})||jb(L,function(a,b,c){var d;return c?void 0:a[b]===!0?b.toLowerCase():(d=a.getAttributeNode(b))&&d.specified?d.value:null}),fb}(a);m.find=s,m.expr=s.selectors,m.expr[":"]=m.expr.pseudos,m.unique=s.uniqueSort,m.text=s.getText,m.isXMLDoc=s.isXML,m.contains=s.contains;var t=m.expr.match.needsContext,u=/^<(\w+)\s*\/?>(?:<\/\1>|)$/,v=/^.[^:#\[\.,]*$/;function w(a,b,c){if(m.isFunction(b))return m.grep(a,function(a,d){return!!b.call(a,d,a)!==c});if(b.nodeType)return m.grep(a,function(a){return a===b!==c});if("string"==typeof b){if(v.test(b))return m.filter(b,a,c);b=m.filter(b,a)}return m.grep(a,function(a){return m.inArray(a,b)>=0!==c})}m.filter=function(a,b,c){var d=b[0];return c&&(a=":not("+a+")"),1===b.length&&1===d.nodeType?m.find.matchesSelector(d,a)?[d]:[]:m.find.matches(a,m.grep(b,function(a){return 1===a.nodeType}))},m.fn.extend({find:function(a){var b,c=[],d=this,e=d.length;if("string"!=typeof a)return this.pushStack(m(a).filter(function(){for(b=0;e>b;b++)if(m.contains(d[b],this))return!0}));for(b=0;e>b;b++)m.find(a,d[b],c);return c=this.pushStack(e>1?m.unique(c):c),c.selector=this.selector?this.selector+" "+a:a,c},filter:function(a){return this.pushStack(w(this,a||[],!1))},not:function(a){return this.pushStack(w(this,a||[],!0))},is:function(a){return!!w(this,"string"==typeof a&&t.test(a)?m(a):a||[],!1).length}});var x,y=a.document,z=/^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]*))$/,A=m.fn.init=function(a,b){var c,d;if(!a)return this;if("string"==typeof a){if(c="<"===a.charAt(0)&&">"===a.charAt(a.length-1)&&a.length>=3?[null,a,null]:z.exec(a),!c||!c[1]&&b)return!b||b.jquery?(b||x).find(a):this.constructor(b).find(a);if(c[1]){if(b=b instanceof m?b[0]:b,m.merge(this,m.parseHTML(c[1],b&&b.nodeType?b.ownerDocument||b:y,!0)),u.test(c[1])&&m.isPlainObject(b))for(c in b)m.isFunction(this[c])?this[c](b[c]):this.attr(c,b[c]);return this}if(d=y.getElementById(c[2]),d&&d.parentNode){if(d.id!==c[2])return x.find(a);this.length=1,this[0]=d}return this.context=y,this.selector=a,this}return a.nodeType?(this.context=this[0]=a,this.length=1,this):m.isFunction(a)?"undefined"!=typeof x.ready?x.ready(a):a(m):(void 0!==a.selector&&(this.selector=a.selector,this.context=a.context),m.makeArray(a,this))};A.prototype=m.fn,x=m(y);var B=/^(?:parents|prev(?:Until|All))/,C={children:!0,contents:!0,next:!0,prev:!0};m.extend({dir:function(a,b,c){var d=[],e=a[b];while(e&&9!==e.nodeType&&(void 0===c||1!==e.nodeType||!m(e).is(c)))1===e.nodeType&&d.push(e),e=e[b];return d},sibling:function(a,b){for(var c=[];a;a=a.nextSibling)1===a.nodeType&&a!==b&&c.push(a);return c}}),m.fn.extend({has:function(a){var b,c=m(a,this),d=c.length;return this.filter(function(){for(b=0;d>b;b++)if(m.contains(this,c[b]))return!0})},closest:function(a,b){for(var c,d=0,e=this.length,f=[],g=t.test(a)||"string"!=typeof a?m(a,b||this.context):0;e>d;d++)for(c=this[d];c&&c!==b;c=c.parentNode)if(c.nodeType<11&&(g?g.index(c)>-1:1===c.nodeType&&m.find.matchesSelector(c,a))){f.push(c);break}return this.pushStack(f.length>1?m.unique(f):f)},index:function(a){return a?"string"==typeof a?m.inArray(this[0],m(a)):m.inArray(a.jquery?a[0]:a,this):this[0]&&this[0].parentNode?this.first().prevAll().length:-1},add:function(a,b){return this.pushStack(m.unique(m.merge(this.get(),m(a,b))))},addBack:function(a){return this.add(null==a?this.prevObject:this.prevObject.filter(a))}});function D(a,b){do a=a[b];while(a&&1!==a.nodeType);return a}m.each({parent:function(a){var b=a.parentNode;return b&&11!==b.nodeType?b:null},parents:function(a){return m.dir(a,"parentNode")},parentsUntil:function(a,b,c){return m.dir(a,"parentNode",c)},next:function(a){return D(a,"nextSibling")},prev:function(a){return D(a,"previousSibling")},nextAll:function(a){return m.dir(a,"nextSibling")},prevAll:function(a){return m.dir(a,"previousSibling")},nextUntil:function(a,b,c){return m.dir(a,"nextSibling",c)},prevUntil:function(a,b,c){return m.dir(a,"previousSibling",c)},siblings:function(a){return m.sibling((a.parentNode||{}).firstChild,a)},children:function(a){return m.sibling(a.firstChild)},contents:function(a){return m.nodeName(a,"iframe")?a.contentDocument||a.contentWindow.document:m.merge([],a.childNodes)}},function(a,b){m.fn[a]=function(c,d){var e=m.map(this,b,c);return"Until"!==a.slice(-5)&&(d=c),d&&"string"==typeof d&&(e=m.filter(d,e)),this.length>1&&(C[a]||(e=m.unique(e)),B.test(a)&&(e=e.reverse())),this.pushStack(e)}});var E=/\S+/g,F={};function G(a){var b=F[a]={};return m.each(a.match(E)||[],function(a,c){b[c]=!0}),b}m.Callbacks=function(a){a="string"==typeof a?F[a]||G(a):m.extend({},a);var b,c,d,e,f,g,h=[],i=!a.once&&[],j=function(l){for(c=a.memory&&l,d=!0,f=g||0,g=0,e=h.length,b=!0;h&&e>f;f++)if(h[f].apply(l[0],l[1])===!1&&a.stopOnFalse){c=!1;break}b=!1,h&&(i?i.length&&j(i.shift()):c?h=[]:k.disable())},k={add:function(){if(h){var d=h.length;!function f(b){m.each(b,function(b,c){var d=m.type(c);"function"===d?a.unique&&k.has(c)||h.push(c):c&&c.length&&"string"!==d&&f(c)})}(arguments),b?e=h.length:c&&(g=d,j(c))}return this},remove:function(){return h&&m.each(arguments,function(a,c){var d;while((d=m.inArray(c,h,d))>-1)h.splice(d,1),b&&(e>=d&&e--,f>=d&&f--)}),this},has:function(a){return a?m.inArray(a,h)>-1:!(!h||!h.length)},empty:function(){return h=[],e=0,this},disable:function(){return h=i=c=void 0,this},disabled:function(){return!h},lock:function(){return i=void 0,c||k.disable(),this},locked:function(){return!i},fireWith:function(a,c){return!h||d&&!i||(c=c||[],c=[a,c.slice?c.slice():c],b?i.push(c):j(c)),this},fire:function(){return k.fireWith(this,arguments),this},fired:function(){return!!d}};return k},m.extend({Deferred:function(a){var b=[["resolve","done",m.Callbacks("once memory"),"resolved"],["reject","fail",m.Callbacks("once memory"),"rejected"],["notify","progress",m.Callbacks("memory")]],c="pending",d={state:function(){return c},always:function(){return e.done(arguments).fail(arguments),this},then:function(){var a=arguments;return m.Deferred(function(c){m.each(b,function(b,f){var g=m.isFunction(a[b])&&a[b];e[f[1]](function(){var a=g&&g.apply(this,arguments);a&&m.isFunction(a.promise)?a.promise().done(c.resolve).fail(c.reject).progress(c.notify):c[f[0]+"With"](this===d?c.promise():this,g?[a]:arguments)})}),a=null}).promise()},promise:function(a){return null!=a?m.extend(a,d):d}},e={};return d.pipe=d.then,m.each(b,function(a,f){var g=f[2],h=f[3];d[f[1]]=g.add,h&&g.add(function(){c=h},b[1^a][2].disable,b[2][2].lock),e[f[0]]=function(){return e[f[0]+"With"](this===e?d:this,arguments),this},e[f[0]+"With"]=g.fireWith}),d.promise(e),a&&a.call(e,e),e},when:function(a){var b=0,c=d.call(arguments),e=c.length,f=1!==e||a&&m.isFunction(a.promise)?e:0,g=1===f?a:m.Deferred(),h=function(a,b,c){return function(e){b[a]=this,c[a]=arguments.length>1?d.call(arguments):e,c===i?g.notifyWith(b,c):--f||g.resolveWith(b,c)}},i,j,k;if(e>1)for(i=new Array(e),j=new Array(e),k=new Array(e);e>b;b++)c[b]&&m.isFunction(c[b].promise)?c[b].promise().done(h(b,k,c)).fail(g.reject).progress(h(b,j,i)):--f;return f||g.resolveWith(k,c),g.promise()}});var H;m.fn.ready=function(a){return m.ready.promise().done(a),this},m.extend({isReady:!1,readyWait:1,holdReady:function(a){a?m.readyWait++:m.ready(!0)},ready:function(a){if(a===!0?!--m.readyWait:!m.isReady){if(!y.body)return setTimeout(m.ready);m.isReady=!0,a!==!0&&--m.readyWait>0||(H.resolveWith(y,[m]),m.fn.triggerHandler&&(m(y).triggerHandler("ready"),m(y).off("ready")))}}});function I(){y.addEventListener?(y.removeEventListener("DOMContentLoaded",J,!1),a.removeEventListener("load",J,!1)):(y.detachEvent("onreadystatechange",J),a.detachEvent("onload",J))}function J(){(y.addEventListener||"load"===event.type||"complete"===y.readyState)&&(I(),m.ready())}m.ready.promise=function(b){if(!H)if(H=m.Deferred(),"complete"===y.readyState)setTimeout(m.ready);else if(y.addEventListener)y.addEventListener("DOMContentLoaded",J,!1),a.addEventListener("load",J,!1);else{y.attachEvent("onreadystatechange",J),a.attachEvent("onload",J);var c=!1;try{c=null==a.frameElement&&y.documentElement}catch(d){}c&&c.doScroll&&!function e(){if(!m.isReady){try{c.doScroll("left")}catch(a){return setTimeout(e,50)}I(),m.ready()}}()}return H.promise(b)};var K="undefined",L;for(L in m(k))break;k.ownLast="0"!==L,k.inlineBlockNeedsLayout=!1,m(function(){var a,b,c,d;c=y.getElementsByTagName("body")[0],c&&c.style&&(b=y.createElement("div"),d=y.createElement("div"),d.style.cssText="position:absolute;border:0;width:0;height:0;top:0;left:-9999px",c.appendChild(d).appendChild(b),typeof b.style.zoom!==K&&(b.style.cssText="display:inline;margin:0;border:0;padding:1px;width:1px;zoom:1",k.inlineBlockNeedsLayout=a=3===b.offsetWidth,a&&(c.style.zoom=1)),c.removeChild(d))}),function(){var a=y.createElement("div");if(null==k.deleteExpando){k.deleteExpando=!0;try{delete a.test}catch(b){k.deleteExpando=!1}}a=null}(),m.acceptData=function(a){var b=m.noData[(a.nodeName+" ").toLowerCase()],c=+a.nodeType||1;return 1!==c&&9!==c?!1:!b||b!==!0&&a.getAttribute("classid")===b};var M=/^(?:\{[\w\W]*\}|\[[\w\W]*\])$/,N=/([A-Z])/g;function O(a,b,c){if(void 0===c&&1===a.nodeType){var d="data-"+b.replace(N,"-$1").toLowerCase();if(c=a.getAttribute(d),"string"==typeof c){try{c="true"===c?!0:"false"===c?!1:"null"===c?null:+c+""===c?+c:M.test(c)?m.parseJSON(c):c}catch(e){}m.data(a,b,c)}else c=void 0}return c}function P(a){var b;for(b in a)if(("data"!==b||!m.isEmptyObject(a[b]))&&"toJSON"!==b)return!1;return!0}function Q(a,b,d,e){if(m.acceptData(a)){var f,g,h=m.expando,i=a.nodeType,j=i?m.cache:a,k=i?a[h]:a[h]&&h;
if(k&&j[k]&&(e||j[k].data)||void 0!==d||"string"!=typeof b)return k||(k=i?a[h]=c.pop()||m.guid++:h),j[k]||(j[k]=i?{}:{toJSON:m.noop}),("object"==typeof b||"function"==typeof b)&&(e?j[k]=m.extend(j[k],b):j[k].data=m.extend(j[k].data,b)),g=j[k],e||(g.data||(g.data={}),g=g.data),void 0!==d&&(g[m.camelCase(b)]=d),"string"==typeof b?(f=g[b],null==f&&(f=g[m.camelCase(b)])):f=g,f}}function R(a,b,c){if(m.acceptData(a)){var d,e,f=a.nodeType,g=f?m.cache:a,h=f?a[m.expando]:m.expando;if(g[h]){if(b&&(d=c?g[h]:g[h].data)){m.isArray(b)?b=b.concat(m.map(b,m.camelCase)):b in d?b=[b]:(b=m.camelCase(b),b=b in d?[b]:b.split(" ")),e=b.length;while(e--)delete d[b[e]];if(c?!P(d):!m.isEmptyObject(d))return}(c||(delete g[h].data,P(g[h])))&&(f?m.cleanData([a],!0):k.deleteExpando||g!=g.window?delete g[h]:g[h]=null)}}}m.extend({cache:{},noData:{"applet ":!0,"embed ":!0,"object ":"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"},hasData:function(a){return a=a.nodeType?m.cache[a[m.expando]]:a[m.expando],!!a&&!P(a)},data:function(a,b,c){return Q(a,b,c)},removeData:function(a,b){return R(a,b)},_data:function(a,b,c){return Q(a,b,c,!0)},_removeData:function(a,b){return R(a,b,!0)}}),m.fn.extend({data:function(a,b){var c,d,e,f=this[0],g=f&&f.attributes;if(void 0===a){if(this.length&&(e=m.data(f),1===f.nodeType&&!m._data(f,"parsedAttrs"))){c=g.length;while(c--)g[c]&&(d=g[c].name,0===d.indexOf("data-")&&(d=m.camelCase(d.slice(5)),O(f,d,e[d])));m._data(f,"parsedAttrs",!0)}return e}return"object"==typeof a?this.each(function(){m.data(this,a)}):arguments.length>1?this.each(function(){m.data(this,a,b)}):f?O(f,a,m.data(f,a)):void 0},removeData:function(a){return this.each(function(){m.removeData(this,a)})}}),m.extend({queue:function(a,b,c){var d;return a?(b=(b||"fx")+"queue",d=m._data(a,b),c&&(!d||m.isArray(c)?d=m._data(a,b,m.makeArray(c)):d.push(c)),d||[]):void 0},dequeue:function(a,b){b=b||"fx";var c=m.queue(a,b),d=c.length,e=c.shift(),f=m._queueHooks(a,b),g=function(){m.dequeue(a,b)};"inprogress"===e&&(e=c.shift(),d--),e&&("fx"===b&&c.unshift("inprogress"),delete f.stop,e.call(a,g,f)),!d&&f&&f.empty.fire()},_queueHooks:function(a,b){var c=b+"queueHooks";return m._data(a,c)||m._data(a,c,{empty:m.Callbacks("once memory").add(function(){m._removeData(a,b+"queue"),m._removeData(a,c)})})}}),m.fn.extend({queue:function(a,b){var c=2;return"string"!=typeof a&&(b=a,a="fx",c--),arguments.length<c?m.queue(this[0],a):void 0===b?this:this.each(function(){var c=m.queue(this,a,b);m._queueHooks(this,a),"fx"===a&&"inprogress"!==c[0]&&m.dequeue(this,a)})},dequeue:function(a){return this.each(function(){m.dequeue(this,a)})},clearQueue:function(a){return this.queue(a||"fx",[])},promise:function(a,b){var c,d=1,e=m.Deferred(),f=this,g=this.length,h=function(){--d||e.resolveWith(f,[f])};"string"!=typeof a&&(b=a,a=void 0),a=a||"fx";while(g--)c=m._data(f[g],a+"queueHooks"),c&&c.empty&&(d++,c.empty.add(h));return h(),e.promise(b)}});var S=/[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,T=["Top","Right","Bottom","Left"],U=function(a,b){return a=b||a,"none"===m.css(a,"display")||!m.contains(a.ownerDocument,a)},V=m.access=function(a,b,c,d,e,f,g){var h=0,i=a.length,j=null==c;if("object"===m.type(c)){e=!0;for(h in c)m.access(a,b,h,c[h],!0,f,g)}else if(void 0!==d&&(e=!0,m.isFunction(d)||(g=!0),j&&(g?(b.call(a,d),b=null):(j=b,b=function(a,b,c){return j.call(m(a),c)})),b))for(;i>h;h++)b(a[h],c,g?d:d.call(a[h],h,b(a[h],c)));return e?a:j?b.call(a):i?b(a[0],c):f},W=/^(?:checkbox|radio)$/i;!function(){var a=y.createElement("input"),b=y.createElement("div"),c=y.createDocumentFragment();if(b.innerHTML="  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>",k.leadingWhitespace=3===b.firstChild.nodeType,k.tbody=!b.getElementsByTagName("tbody").length,k.htmlSerialize=!!b.getElementsByTagName("link").length,k.html5Clone="<:nav></:nav>"!==y.createElement("nav").cloneNode(!0).outerHTML,a.type="checkbox",a.checked=!0,c.appendChild(a),k.appendChecked=a.checked,b.innerHTML="<textarea>x</textarea>",k.noCloneChecked=!!b.cloneNode(!0).lastChild.defaultValue,c.appendChild(b),b.innerHTML="<input type='radio' checked='checked' name='t'/>",k.checkClone=b.cloneNode(!0).cloneNode(!0).lastChild.checked,k.noCloneEvent=!0,b.attachEvent&&(b.attachEvent("onclick",function(){k.noCloneEvent=!1}),b.cloneNode(!0).click()),null==k.deleteExpando){k.deleteExpando=!0;try{delete b.test}catch(d){k.deleteExpando=!1}}}(),function(){var b,c,d=y.createElement("div");for(b in{submit:!0,change:!0,focusin:!0})c="on"+b,(k[b+"Bubbles"]=c in a)||(d.setAttribute(c,"t"),k[b+"Bubbles"]=d.attributes[c].expando===!1);d=null}();var X=/^(?:input|select|textarea)$/i,Y=/^key/,Z=/^(?:mouse|pointer|contextmenu)|click/,$=/^(?:focusinfocus|focusoutblur)$/,_=/^([^.]*)(?:\.(.+)|)$/;function ab(){return!0}function bb(){return!1}function cb(){try{return y.activeElement}catch(a){}}m.event={global:{},add:function(a,b,c,d,e){var f,g,h,i,j,k,l,n,o,p,q,r=m._data(a);if(r){c.handler&&(i=c,c=i.handler,e=i.selector),c.guid||(c.guid=m.guid++),(g=r.events)||(g=r.events={}),(k=r.handle)||(k=r.handle=function(a){return typeof m===K||a&&m.event.triggered===a.type?void 0:m.event.dispatch.apply(k.elem,arguments)},k.elem=a),b=(b||"").match(E)||[""],h=b.length;while(h--)f=_.exec(b[h])||[],o=q=f[1],p=(f[2]||"").split(".").sort(),o&&(j=m.event.special[o]||{},o=(e?j.delegateType:j.bindType)||o,j=m.event.special[o]||{},l=m.extend({type:o,origType:q,data:d,handler:c,guid:c.guid,selector:e,needsContext:e&&m.expr.match.needsContext.test(e),namespace:p.join(".")},i),(n=g[o])||(n=g[o]=[],n.delegateCount=0,j.setup&&j.setup.call(a,d,p,k)!==!1||(a.addEventListener?a.addEventListener(o,k,!1):a.attachEvent&&a.attachEvent("on"+o,k))),j.add&&(j.add.call(a,l),l.handler.guid||(l.handler.guid=c.guid)),e?n.splice(n.delegateCount++,0,l):n.push(l),m.event.global[o]=!0);a=null}},remove:function(a,b,c,d,e){var f,g,h,i,j,k,l,n,o,p,q,r=m.hasData(a)&&m._data(a);if(r&&(k=r.events)){b=(b||"").match(E)||[""],j=b.length;while(j--)if(h=_.exec(b[j])||[],o=q=h[1],p=(h[2]||"").split(".").sort(),o){l=m.event.special[o]||{},o=(d?l.delegateType:l.bindType)||o,n=k[o]||[],h=h[2]&&new RegExp("(^|\\.)"+p.join("\\.(?:.*\\.|)")+"(\\.|$)"),i=f=n.length;while(f--)g=n[f],!e&&q!==g.origType||c&&c.guid!==g.guid||h&&!h.test(g.namespace)||d&&d!==g.selector&&("**"!==d||!g.selector)||(n.splice(f,1),g.selector&&n.delegateCount--,l.remove&&l.remove.call(a,g));i&&!n.length&&(l.teardown&&l.teardown.call(a,p,r.handle)!==!1||m.removeEvent(a,o,r.handle),delete k[o])}else for(o in k)m.event.remove(a,o+b[j],c,d,!0);m.isEmptyObject(k)&&(delete r.handle,m._removeData(a,"events"))}},trigger:function(b,c,d,e){var f,g,h,i,k,l,n,o=[d||y],p=j.call(b,"type")?b.type:b,q=j.call(b,"namespace")?b.namespace.split("."):[];if(h=l=d=d||y,3!==d.nodeType&&8!==d.nodeType&&!$.test(p+m.event.triggered)&&(p.indexOf(".")>=0&&(q=p.split("."),p=q.shift(),q.sort()),g=p.indexOf(":")<0&&"on"+p,b=b[m.expando]?b:new m.Event(p,"object"==typeof b&&b),b.isTrigger=e?2:3,b.namespace=q.join("."),b.namespace_re=b.namespace?new RegExp("(^|\\.)"+q.join("\\.(?:.*\\.|)")+"(\\.|$)"):null,b.result=void 0,b.target||(b.target=d),c=null==c?[b]:m.makeArray(c,[b]),k=m.event.special[p]||{},e||!k.trigger||k.trigger.apply(d,c)!==!1)){if(!e&&!k.noBubble&&!m.isWindow(d)){for(i=k.delegateType||p,$.test(i+p)||(h=h.parentNode);h;h=h.parentNode)o.push(h),l=h;l===(d.ownerDocument||y)&&o.push(l.defaultView||l.parentWindow||a)}n=0;while((h=o[n++])&&!b.isPropagationStopped())b.type=n>1?i:k.bindType||p,f=(m._data(h,"events")||{})[b.type]&&m._data(h,"handle"),f&&f.apply(h,c),f=g&&h[g],f&&f.apply&&m.acceptData(h)&&(b.result=f.apply(h,c),b.result===!1&&b.preventDefault());if(b.type=p,!e&&!b.isDefaultPrevented()&&(!k._default||k._default.apply(o.pop(),c)===!1)&&m.acceptData(d)&&g&&d[p]&&!m.isWindow(d)){l=d[g],l&&(d[g]=null),m.event.triggered=p;try{d[p]()}catch(r){}m.event.triggered=void 0,l&&(d[g]=l)}return b.result}},dispatch:function(a){a=m.event.fix(a);var b,c,e,f,g,h=[],i=d.call(arguments),j=(m._data(this,"events")||{})[a.type]||[],k=m.event.special[a.type]||{};if(i[0]=a,a.delegateTarget=this,!k.preDispatch||k.preDispatch.call(this,a)!==!1){h=m.event.handlers.call(this,a,j),b=0;while((f=h[b++])&&!a.isPropagationStopped()){a.currentTarget=f.elem,g=0;while((e=f.handlers[g++])&&!a.isImmediatePropagationStopped())(!a.namespace_re||a.namespace_re.test(e.namespace))&&(a.handleObj=e,a.data=e.data,c=((m.event.special[e.origType]||{}).handle||e.handler).apply(f.elem,i),void 0!==c&&(a.result=c)===!1&&(a.preventDefault(),a.stopPropagation()))}return k.postDispatch&&k.postDispatch.call(this,a),a.result}},handlers:function(a,b){var c,d,e,f,g=[],h=b.delegateCount,i=a.target;if(h&&i.nodeType&&(!a.button||"click"!==a.type))for(;i!=this;i=i.parentNode||this)if(1===i.nodeType&&(i.disabled!==!0||"click"!==a.type)){for(e=[],f=0;h>f;f++)d=b[f],c=d.selector+" ",void 0===e[c]&&(e[c]=d.needsContext?m(c,this).index(i)>=0:m.find(c,this,null,[i]).length),e[c]&&e.push(d);e.length&&g.push({elem:i,handlers:e})}return h<b.length&&g.push({elem:this,handlers:b.slice(h)}),g},fix:function(a){if(a[m.expando])return a;var b,c,d,e=a.type,f=a,g=this.fixHooks[e];g||(this.fixHooks[e]=g=Z.test(e)?this.mouseHooks:Y.test(e)?this.keyHooks:{}),d=g.props?this.props.concat(g.props):this.props,a=new m.Event(f),b=d.length;while(b--)c=d[b],a[c]=f[c];return a.target||(a.target=f.srcElement||y),3===a.target.nodeType&&(a.target=a.target.parentNode),a.metaKey=!!a.metaKey,g.filter?g.filter(a,f):a},props:"altKey bubbles cancelable ctrlKey currentTarget eventPhase metaKey relatedTarget shiftKey target timeStamp view which".split(" "),fixHooks:{},keyHooks:{props:"char charCode key keyCode".split(" "),filter:function(a,b){return null==a.which&&(a.which=null!=b.charCode?b.charCode:b.keyCode),a}},mouseHooks:{props:"button buttons clientX clientY fromElement offsetX offsetY pageX pageY screenX screenY toElement".split(" "),filter:function(a,b){var c,d,e,f=b.button,g=b.fromElement;return null==a.pageX&&null!=b.clientX&&(d=a.target.ownerDocument||y,e=d.documentElement,c=d.body,a.pageX=b.clientX+(e&&e.scrollLeft||c&&c.scrollLeft||0)-(e&&e.clientLeft||c&&c.clientLeft||0),a.pageY=b.clientY+(e&&e.scrollTop||c&&c.scrollTop||0)-(e&&e.clientTop||c&&c.clientTop||0)),!a.relatedTarget&&g&&(a.relatedTarget=g===a.target?b.toElement:g),a.which||void 0===f||(a.which=1&f?1:2&f?3:4&f?2:0),a}},special:{load:{noBubble:!0},focus:{trigger:function(){if(this!==cb()&&this.focus)try{return this.focus(),!1}catch(a){}},delegateType:"focusin"},blur:{trigger:function(){return this===cb()&&this.blur?(this.blur(),!1):void 0},delegateType:"focusout"},click:{trigger:function(){return m.nodeName(this,"input")&&"checkbox"===this.type&&this.click?(this.click(),!1):void 0},_default:function(a){return m.nodeName(a.target,"a")}},beforeunload:{postDispatch:function(a){void 0!==a.result&&a.originalEvent&&(a.originalEvent.returnValue=a.result)}}},simulate:function(a,b,c,d){var e=m.extend(new m.Event,c,{type:a,isSimulated:!0,originalEvent:{}});d?m.event.trigger(e,null,b):m.event.dispatch.call(b,e),e.isDefaultPrevented()&&c.preventDefault()}},m.removeEvent=y.removeEventListener?function(a,b,c){a.removeEventListener&&a.removeEventListener(b,c,!1)}:function(a,b,c){var d="on"+b;a.detachEvent&&(typeof a[d]===K&&(a[d]=null),a.detachEvent(d,c))},m.Event=function(a,b){return this instanceof m.Event?(a&&a.type?(this.originalEvent=a,this.type=a.type,this.isDefaultPrevented=a.defaultPrevented||void 0===a.defaultPrevented&&a.returnValue===!1?ab:bb):this.type=a,b&&m.extend(this,b),this.timeStamp=a&&a.timeStamp||m.now(),void(this[m.expando]=!0)):new m.Event(a,b)},m.Event.prototype={isDefaultPrevented:bb,isPropagationStopped:bb,isImmediatePropagationStopped:bb,preventDefault:function(){var a=this.originalEvent;this.isDefaultPrevented=ab,a&&(a.preventDefault?a.preventDefault():a.returnValue=!1)},stopPropagation:function(){var a=this.originalEvent;this.isPropagationStopped=ab,a&&(a.stopPropagation&&a.stopPropagation(),a.cancelBubble=!0)},stopImmediatePropagation:function(){var a=this.originalEvent;this.isImmediatePropagationStopped=ab,a&&a.stopImmediatePropagation&&a.stopImmediatePropagation(),this.stopPropagation()}},m.each({mouseenter:"mouseover",mouseleave:"mouseout",pointerenter:"pointerover",pointerleave:"pointerout"},function(a,b){m.event.special[a]={delegateType:b,bindType:b,handle:function(a){var c,d=this,e=a.relatedTarget,f=a.handleObj;return(!e||e!==d&&!m.contains(d,e))&&(a.type=f.origType,c=f.handler.apply(this,arguments),a.type=b),c}}}),k.submitBubbles||(m.event.special.submit={setup:function(){return m.nodeName(this,"form")?!1:void m.event.add(this,"click._submit keypress._submit",function(a){var b=a.target,c=m.nodeName(b,"input")||m.nodeName(b,"button")?b.form:void 0;c&&!m._data(c,"submitBubbles")&&(m.event.add(c,"submit._submit",function(a){a._submit_bubble=!0}),m._data(c,"submitBubbles",!0))})},postDispatch:function(a){a._submit_bubble&&(delete a._submit_bubble,this.parentNode&&!a.isTrigger&&m.event.simulate("submit",this.parentNode,a,!0))},teardown:function(){return m.nodeName(this,"form")?!1:void m.event.remove(this,"._submit")}}),k.changeBubbles||(m.event.special.change={setup:function(){return X.test(this.nodeName)?(("checkbox"===this.type||"radio"===this.type)&&(m.event.add(this,"propertychange._change",function(a){"checked"===a.originalEvent.propertyName&&(this._just_changed=!0)}),m.event.add(this,"click._change",function(a){this._just_changed&&!a.isTrigger&&(this._just_changed=!1),m.event.simulate("change",this,a,!0)})),!1):void m.event.add(this,"beforeactivate._change",function(a){var b=a.target;X.test(b.nodeName)&&!m._data(b,"changeBubbles")&&(m.event.add(b,"change._change",function(a){!this.parentNode||a.isSimulated||a.isTrigger||m.event.simulate("change",this.parentNode,a,!0)}),m._data(b,"changeBubbles",!0))})},handle:function(a){var b=a.target;return this!==b||a.isSimulated||a.isTrigger||"radio"!==b.type&&"checkbox"!==b.type?a.handleObj.handler.apply(this,arguments):void 0},teardown:function(){return m.event.remove(this,"._change"),!X.test(this.nodeName)}}),k.focusinBubbles||m.each({focus:"focusin",blur:"focusout"},function(a,b){var c=function(a){m.event.simulate(b,a.target,m.event.fix(a),!0)};m.event.special[b]={setup:function(){var d=this.ownerDocument||this,e=m._data(d,b);e||d.addEventListener(a,c,!0),m._data(d,b,(e||0)+1)},teardown:function(){var d=this.ownerDocument||this,e=m._data(d,b)-1;e?m._data(d,b,e):(d.removeEventListener(a,c,!0),m._removeData(d,b))}}}),m.fn.extend({on:function(a,b,c,d,e){var f,g;if("object"==typeof a){"string"!=typeof b&&(c=c||b,b=void 0);for(f in a)this.on(f,b,c,a[f],e);return this}if(null==c&&null==d?(d=b,c=b=void 0):null==d&&("string"==typeof b?(d=c,c=void 0):(d=c,c=b,b=void 0)),d===!1)d=bb;else if(!d)return this;return 1===e&&(g=d,d=function(a){return m().off(a),g.apply(this,arguments)},d.guid=g.guid||(g.guid=m.guid++)),this.each(function(){m.event.add(this,a,d,c,b)})},one:function(a,b,c,d){return this.on(a,b,c,d,1)},off:function(a,b,c){var d,e;if(a&&a.preventDefault&&a.handleObj)return d=a.handleObj,m(a.delegateTarget).off(d.namespace?d.origType+"."+d.namespace:d.origType,d.selector,d.handler),this;if("object"==typeof a){for(e in a)this.off(e,b,a[e]);return this}return(b===!1||"function"==typeof b)&&(c=b,b=void 0),c===!1&&(c=bb),this.each(function(){m.event.remove(this,a,c,b)})},trigger:function(a,b){return this.each(function(){m.event.trigger(a,b,this)})},triggerHandler:function(a,b){var c=this[0];return c?m.event.trigger(a,b,c,!0):void 0}});function db(a){var b=eb.split("|"),c=a.createDocumentFragment();if(c.createElement)while(b.length)c.createElement(b.pop());return c}var eb="abbr|article|aside|audio|bdi|canvas|data|datalist|details|figcaption|figure|footer|header|hgroup|mark|meter|nav|output|progress|section|summary|time|video",fb=/ jQuery\d+="(?:null|\d+)"/g,gb=new RegExp("<(?:"+eb+")[\\s/>]","i"),hb=/^\s+/,ib=/<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:]+)[^>]*)\/>/gi,jb=/<([\w:]+)/,kb=/<tbody/i,lb=/<|&#?\w+;/,mb=/<(?:script|style|link)/i,nb=/checked\s*(?:[^=]|=\s*.checked.)/i,ob=/^$|\/(?:java|ecma)script/i,pb=/^true\/(.*)/,qb=/^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g,rb={option:[1,"<select multiple='multiple'>","</select>"],legend:[1,"<fieldset>","</fieldset>"],area:[1,"<map>","</map>"],param:[1,"<object>","</object>"],thead:[1,"<table>","</table>"],tr:[2,"<table><tbody>","</tbody></table>"],col:[2,"<table><tbody></tbody><colgroup>","</colgroup></table>"],td:[3,"<table><tbody><tr>","</tr></tbody></table>"],_default:k.htmlSerialize?[0,"",""]:[1,"X<div>","</div>"]},sb=db(y),tb=sb.appendChild(y.createElement("div"));rb.optgroup=rb.option,rb.tbody=rb.tfoot=rb.colgroup=rb.caption=rb.thead,rb.th=rb.td;function ub(a,b){var c,d,e=0,f=typeof a.getElementsByTagName!==K?a.getElementsByTagName(b||"*"):typeof a.querySelectorAll!==K?a.querySelectorAll(b||"*"):void 0;if(!f)for(f=[],c=a.childNodes||a;null!=(d=c[e]);e++)!b||m.nodeName(d,b)?f.push(d):m.merge(f,ub(d,b));return void 0===b||b&&m.nodeName(a,b)?m.merge([a],f):f}function vb(a){W.test(a.type)&&(a.defaultChecked=a.checked)}function wb(a,b){return m.nodeName(a,"table")&&m.nodeName(11!==b.nodeType?b:b.firstChild,"tr")?a.getElementsByTagName("tbody")[0]||a.appendChild(a.ownerDocument.createElement("tbody")):a}function xb(a){return a.type=(null!==m.find.attr(a,"type"))+"/"+a.type,a}function yb(a){var b=pb.exec(a.type);return b?a.type=b[1]:a.removeAttribute("type"),a}function zb(a,b){for(var c,d=0;null!=(c=a[d]);d++)m._data(c,"globalEval",!b||m._data(b[d],"globalEval"))}function Ab(a,b){if(1===b.nodeType&&m.hasData(a)){var c,d,e,f=m._data(a),g=m._data(b,f),h=f.events;if(h){delete g.handle,g.events={};for(c in h)for(d=0,e=h[c].length;e>d;d++)m.event.add(b,c,h[c][d])}g.data&&(g.data=m.extend({},g.data))}}function Bb(a,b){var c,d,e;if(1===b.nodeType){if(c=b.nodeName.toLowerCase(),!k.noCloneEvent&&b[m.expando]){e=m._data(b);for(d in e.events)m.removeEvent(b,d,e.handle);b.removeAttribute(m.expando)}"script"===c&&b.text!==a.text?(xb(b).text=a.text,yb(b)):"object"===c?(b.parentNode&&(b.outerHTML=a.outerHTML),k.html5Clone&&a.innerHTML&&!m.trim(b.innerHTML)&&(b.innerHTML=a.innerHTML)):"input"===c&&W.test(a.type)?(b.defaultChecked=b.checked=a.checked,b.value!==a.value&&(b.value=a.value)):"option"===c?b.defaultSelected=b.selected=a.defaultSelected:("input"===c||"textarea"===c)&&(b.defaultValue=a.defaultValue)}}m.extend({clone:function(a,b,c){var d,e,f,g,h,i=m.contains(a.ownerDocument,a);if(k.html5Clone||m.isXMLDoc(a)||!gb.test("<"+a.nodeName+">")?f=a.cloneNode(!0):(tb.innerHTML=a.outerHTML,tb.removeChild(f=tb.firstChild)),!(k.noCloneEvent&&k.noCloneChecked||1!==a.nodeType&&11!==a.nodeType||m.isXMLDoc(a)))for(d=ub(f),h=ub(a),g=0;null!=(e=h[g]);++g)d[g]&&Bb(e,d[g]);if(b)if(c)for(h=h||ub(a),d=d||ub(f),g=0;null!=(e=h[g]);g++)Ab(e,d[g]);else Ab(a,f);return d=ub(f,"script"),d.length>0&&zb(d,!i&&ub(a,"script")),d=h=e=null,f},buildFragment:function(a,b,c,d){for(var e,f,g,h,i,j,l,n=a.length,o=db(b),p=[],q=0;n>q;q++)if(f=a[q],f||0===f)if("object"===m.type(f))m.merge(p,f.nodeType?[f]:f);else if(lb.test(f)){h=h||o.appendChild(b.createElement("div")),i=(jb.exec(f)||["",""])[1].toLowerCase(),l=rb[i]||rb._default,h.innerHTML=l[1]+f.replace(ib,"<$1></$2>")+l[2],e=l[0];while(e--)h=h.lastChild;if(!k.leadingWhitespace&&hb.test(f)&&p.push(b.createTextNode(hb.exec(f)[0])),!k.tbody){f="table"!==i||kb.test(f)?"<table>"!==l[1]||kb.test(f)?0:h:h.firstChild,e=f&&f.childNodes.length;while(e--)m.nodeName(j=f.childNodes[e],"tbody")&&!j.childNodes.length&&f.removeChild(j)}m.merge(p,h.childNodes),h.textContent="";while(h.firstChild)h.removeChild(h.firstChild);h=o.lastChild}else p.push(b.createTextNode(f));h&&o.removeChild(h),k.appendChecked||m.grep(ub(p,"input"),vb),q=0;while(f=p[q++])if((!d||-1===m.inArray(f,d))&&(g=m.contains(f.ownerDocument,f),h=ub(o.appendChild(f),"script"),g&&zb(h),c)){e=0;while(f=h[e++])ob.test(f.type||"")&&c.push(f)}return h=null,o},cleanData:function(a,b){for(var d,e,f,g,h=0,i=m.expando,j=m.cache,l=k.deleteExpando,n=m.event.special;null!=(d=a[h]);h++)if((b||m.acceptData(d))&&(f=d[i],g=f&&j[f])){if(g.events)for(e in g.events)n[e]?m.event.remove(d,e):m.removeEvent(d,e,g.handle);j[f]&&(delete j[f],l?delete d[i]:typeof d.removeAttribute!==K?d.removeAttribute(i):d[i]=null,c.push(f))}}}),m.fn.extend({text:function(a){return V(this,function(a){return void 0===a?m.text(this):this.empty().append((this[0]&&this[0].ownerDocument||y).createTextNode(a))},null,a,arguments.length)},append:function(){return this.domManip(arguments,function(a){if(1===this.nodeType||11===this.nodeType||9===this.nodeType){var b=wb(this,a);b.appendChild(a)}})},prepend:function(){return this.domManip(arguments,function(a){if(1===this.nodeType||11===this.nodeType||9===this.nodeType){var b=wb(this,a);b.insertBefore(a,b.firstChild)}})},before:function(){return this.domManip(arguments,function(a){this.parentNode&&this.parentNode.insertBefore(a,this)})},after:function(){return this.domManip(arguments,function(a){this.parentNode&&this.parentNode.insertBefore(a,this.nextSibling)})},remove:function(a,b){for(var c,d=a?m.filter(a,this):this,e=0;null!=(c=d[e]);e++)b||1!==c.nodeType||m.cleanData(ub(c)),c.parentNode&&(b&&m.contains(c.ownerDocument,c)&&zb(ub(c,"script")),c.parentNode.removeChild(c));return this},empty:function(){for(var a,b=0;null!=(a=this[b]);b++){1===a.nodeType&&m.cleanData(ub(a,!1));while(a.firstChild)a.removeChild(a.firstChild);a.options&&m.nodeName(a,"select")&&(a.options.length=0)}return this},clone:function(a,b){return a=null==a?!1:a,b=null==b?a:b,this.map(function(){return m.clone(this,a,b)})},html:function(a){return V(this,function(a){var b=this[0]||{},c=0,d=this.length;if(void 0===a)return 1===b.nodeType?b.innerHTML.replace(fb,""):void 0;if(!("string"!=typeof a||mb.test(a)||!k.htmlSerialize&&gb.test(a)||!k.leadingWhitespace&&hb.test(a)||rb[(jb.exec(a)||["",""])[1].toLowerCase()])){a=a.replace(ib,"<$1></$2>");try{for(;d>c;c++)b=this[c]||{},1===b.nodeType&&(m.cleanData(ub(b,!1)),b.innerHTML=a);b=0}catch(e){}}b&&this.empty().append(a)},null,a,arguments.length)},replaceWith:function(){var a=arguments[0];return this.domManip(arguments,function(b){a=this.parentNode,m.cleanData(ub(this)),a&&a.replaceChild(b,this)}),a&&(a.length||a.nodeType)?this:this.remove()},detach:function(a){return this.remove(a,!0)},domManip:function(a,b){a=e.apply([],a);var c,d,f,g,h,i,j=0,l=this.length,n=this,o=l-1,p=a[0],q=m.isFunction(p);if(q||l>1&&"string"==typeof p&&!k.checkClone&&nb.test(p))return this.each(function(c){var d=n.eq(c);q&&(a[0]=p.call(this,c,d.html())),d.domManip(a,b)});if(l&&(i=m.buildFragment(a,this[0].ownerDocument,!1,this),c=i.firstChild,1===i.childNodes.length&&(i=c),c)){for(g=m.map(ub(i,"script"),xb),f=g.length;l>j;j++)d=i,j!==o&&(d=m.clone(d,!0,!0),f&&m.merge(g,ub(d,"script"))),b.call(this[j],d,j);if(f)for(h=g[g.length-1].ownerDocument,m.map(g,yb),j=0;f>j;j++)d=g[j],ob.test(d.type||"")&&!m._data(d,"globalEval")&&m.contains(h,d)&&(d.src?m._evalUrl&&m._evalUrl(d.src):m.globalEval((d.text||d.textContent||d.innerHTML||"").replace(qb,"")));i=c=null}return this}}),m.each({appendTo:"append",prependTo:"prepend",insertBefore:"before",insertAfter:"after",replaceAll:"replaceWith"},function(a,b){m.fn[a]=function(a){for(var c,d=0,e=[],g=m(a),h=g.length-1;h>=d;d++)c=d===h?this:this.clone(!0),m(g[d])[b](c),f.apply(e,c.get());return this.pushStack(e)}});var Cb,Db={};function Eb(b,c){var d,e=m(c.createElement(b)).appendTo(c.body),f=a.getDefaultComputedStyle&&(d=a.getDefaultComputedStyle(e[0]))?d.display:m.css(e[0],"display");return e.detach(),f}function Fb(a){var b=y,c=Db[a];return c||(c=Eb(a,b),"none"!==c&&c||(Cb=(Cb||m("<iframe frameborder='0' width='0' height='0'/>")).appendTo(b.documentElement),b=(Cb[0].contentWindow||Cb[0].contentDocument).document,b.write(),b.close(),c=Eb(a,b),Cb.detach()),Db[a]=c),c}!function(){var a;k.shrinkWrapBlocks=function(){if(null!=a)return a;a=!1;var b,c,d;return c=y.getElementsByTagName("body")[0],c&&c.style?(b=y.createElement("div"),d=y.createElement("div"),d.style.cssText="position:absolute;border:0;width:0;height:0;top:0;left:-9999px",c.appendChild(d).appendChild(b),typeof b.style.zoom!==K&&(b.style.cssText="-webkit-box-sizing:content-box;-moz-box-sizing:content-box;box-sizing:content-box;display:block;margin:0;border:0;padding:1px;width:1px;zoom:1",b.appendChild(y.createElement("div")).style.width="5px",a=3!==b.offsetWidth),c.removeChild(d),a):void 0}}();var Gb=/^margin/,Hb=new RegExp("^("+S+")(?!px)[a-z%]+$","i"),Ib,Jb,Kb=/^(top|right|bottom|left)$/;a.getComputedStyle?(Ib=function(a){return a.ownerDocument.defaultView.getComputedStyle(a,null)},Jb=function(a,b,c){var d,e,f,g,h=a.style;return c=c||Ib(a),g=c?c.getPropertyValue(b)||c[b]:void 0,c&&(""!==g||m.contains(a.ownerDocument,a)||(g=m.style(a,b)),Hb.test(g)&&Gb.test(b)&&(d=h.width,e=h.minWidth,f=h.maxWidth,h.minWidth=h.maxWidth=h.width=g,g=c.width,h.width=d,h.minWidth=e,h.maxWidth=f)),void 0===g?g:g+""}):y.documentElement.currentStyle&&(Ib=function(a){return a.currentStyle},Jb=function(a,b,c){var d,e,f,g,h=a.style;return c=c||Ib(a),g=c?c[b]:void 0,null==g&&h&&h[b]&&(g=h[b]),Hb.test(g)&&!Kb.test(b)&&(d=h.left,e=a.runtimeStyle,f=e&&e.left,f&&(e.left=a.currentStyle.left),h.left="fontSize"===b?"1em":g,g=h.pixelLeft+"px",h.left=d,f&&(e.left=f)),void 0===g?g:g+""||"auto"});function Lb(a,b){return{get:function(){var c=a();if(null!=c)return c?void delete this.get:(this.get=b).apply(this,arguments)}}}!function(){var b,c,d,e,f,g,h;if(b=y.createElement("div"),b.innerHTML="  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>",d=b.getElementsByTagName("a")[0],c=d&&d.style){c.cssText="float:left;opacity:.5",k.opacity="0.5"===c.opacity,k.cssFloat=!!c.cssFloat,b.style.backgroundClip="content-box",b.cloneNode(!0).style.backgroundClip="",k.clearCloneStyle="content-box"===b.style.backgroundClip,k.boxSizing=""===c.boxSizing||""===c.MozBoxSizing||""===c.WebkitBoxSizing,m.extend(k,{reliableHiddenOffsets:function(){return null==g&&i(),g},boxSizingReliable:function(){return null==f&&i(),f},pixelPosition:function(){return null==e&&i(),e},reliableMarginRight:function(){return null==h&&i(),h}});function i(){var b,c,d,i;c=y.getElementsByTagName("body")[0],c&&c.style&&(b=y.createElement("div"),d=y.createElement("div"),d.style.cssText="position:absolute;border:0;width:0;height:0;top:0;left:-9999px",c.appendChild(d).appendChild(b),b.style.cssText="-webkit-box-sizing:border-box;-moz-box-sizing:border-box;box-sizing:border-box;display:block;margin-top:1%;top:1%;border:1px;padding:1px;width:4px;position:absolute",e=f=!1,h=!0,a.getComputedStyle&&(e="1%"!==(a.getComputedStyle(b,null)||{}).top,f="4px"===(a.getComputedStyle(b,null)||{width:"4px"}).width,i=b.appendChild(y.createElement("div")),i.style.cssText=b.style.cssText="-webkit-box-sizing:content-box;-moz-box-sizing:content-box;box-sizing:content-box;display:block;margin:0;border:0;padding:0",i.style.marginRight=i.style.width="0",b.style.width="1px",h=!parseFloat((a.getComputedStyle(i,null)||{}).marginRight)),b.innerHTML="<table><tr><td></td><td>t</td></tr></table>",i=b.getElementsByTagName("td"),i[0].style.cssText="margin:0;border:0;padding:0;display:none",g=0===i[0].offsetHeight,g&&(i[0].style.display="",i[1].style.display="none",g=0===i[0].offsetHeight),c.removeChild(d))}}}(),m.swap=function(a,b,c,d){var e,f,g={};for(f in b)g[f]=a.style[f],a.style[f]=b[f];e=c.apply(a,d||[]);for(f in b)a.style[f]=g[f];return e};var Mb=/alpha\([^)]*\)/i,Nb=/opacity\s*=\s*([^)]*)/,Ob=/^(none|table(?!-c[ea]).+)/,Pb=new RegExp("^("+S+")(.*)$","i"),Qb=new RegExp("^([+-])=("+S+")","i"),Rb={position:"absolute",visibility:"hidden",display:"block"},Sb={letterSpacing:"0",fontWeight:"400"},Tb=["Webkit","O","Moz","ms"];function Ub(a,b){if(b in a)return b;var c=b.charAt(0).toUpperCase()+b.slice(1),d=b,e=Tb.length;while(e--)if(b=Tb[e]+c,b in a)return b;return d}function Vb(a,b){for(var c,d,e,f=[],g=0,h=a.length;h>g;g++)d=a[g],d.style&&(f[g]=m._data(d,"olddisplay"),c=d.style.display,b?(f[g]||"none"!==c||(d.style.display=""),""===d.style.display&&U(d)&&(f[g]=m._data(d,"olddisplay",Fb(d.nodeName)))):(e=U(d),(c&&"none"!==c||!e)&&m._data(d,"olddisplay",e?c:m.css(d,"display"))));for(g=0;h>g;g++)d=a[g],d.style&&(b&&"none"!==d.style.display&&""!==d.style.display||(d.style.display=b?f[g]||"":"none"));return a}function Wb(a,b,c){var d=Pb.exec(b);return d?Math.max(0,d[1]-(c||0))+(d[2]||"px"):b}function Xb(a,b,c,d,e){for(var f=c===(d?"border":"content")?4:"width"===b?1:0,g=0;4>f;f+=2)"margin"===c&&(g+=m.css(a,c+T[f],!0,e)),d?("content"===c&&(g-=m.css(a,"padding"+T[f],!0,e)),"margin"!==c&&(g-=m.css(a,"border"+T[f]+"Width",!0,e))):(g+=m.css(a,"padding"+T[f],!0,e),"padding"!==c&&(g+=m.css(a,"border"+T[f]+"Width",!0,e)));return g}function Yb(a,b,c){var d=!0,e="width"===b?a.offsetWidth:a.offsetHeight,f=Ib(a),g=k.boxSizing&&"border-box"===m.css(a,"boxSizing",!1,f);if(0>=e||null==e){if(e=Jb(a,b,f),(0>e||null==e)&&(e=a.style[b]),Hb.test(e))return e;d=g&&(k.boxSizingReliable()||e===a.style[b]),e=parseFloat(e)||0}return e+Xb(a,b,c||(g?"border":"content"),d,f)+"px"}m.extend({cssHooks:{opacity:{get:function(a,b){if(b){var c=Jb(a,"opacity");return""===c?"1":c}}}},cssNumber:{columnCount:!0,fillOpacity:!0,flexGrow:!0,flexShrink:!0,fontWeight:!0,lineHeight:!0,opacity:!0,order:!0,orphans:!0,widows:!0,zIndex:!0,zoom:!0},cssProps:{"float":k.cssFloat?"cssFloat":"styleFloat"},style:function(a,b,c,d){if(a&&3!==a.nodeType&&8!==a.nodeType&&a.style){var e,f,g,h=m.camelCase(b),i=a.style;if(b=m.cssProps[h]||(m.cssProps[h]=Ub(i,h)),g=m.cssHooks[b]||m.cssHooks[h],void 0===c)return g&&"get"in g&&void 0!==(e=g.get(a,!1,d))?e:i[b];if(f=typeof c,"string"===f&&(e=Qb.exec(c))&&(c=(e[1]+1)*e[2]+parseFloat(m.css(a,b)),f="number"),null!=c&&c===c&&("number"!==f||m.cssNumber[h]||(c+="px"),k.clearCloneStyle||""!==c||0!==b.indexOf("background")||(i[b]="inherit"),!(g&&"set"in g&&void 0===(c=g.set(a,c,d)))))try{i[b]=c}catch(j){}}},css:function(a,b,c,d){var e,f,g,h=m.camelCase(b);return b=m.cssProps[h]||(m.cssProps[h]=Ub(a.style,h)),g=m.cssHooks[b]||m.cssHooks[h],g&&"get"in g&&(f=g.get(a,!0,c)),void 0===f&&(f=Jb(a,b,d)),"normal"===f&&b in Sb&&(f=Sb[b]),""===c||c?(e=parseFloat(f),c===!0||m.isNumeric(e)?e||0:f):f}}),m.each(["height","width"],function(a,b){m.cssHooks[b]={get:function(a,c,d){return c?Ob.test(m.css(a,"display"))&&0===a.offsetWidth?m.swap(a,Rb,function(){return Yb(a,b,d)}):Yb(a,b,d):void 0},set:function(a,c,d){var e=d&&Ib(a);return Wb(a,c,d?Xb(a,b,d,k.boxSizing&&"border-box"===m.css(a,"boxSizing",!1,e),e):0)}}}),k.opacity||(m.cssHooks.opacity={get:function(a,b){return Nb.test((b&&a.currentStyle?a.currentStyle.filter:a.style.filter)||"")?.01*parseFloat(RegExp.$1)+"":b?"1":""},set:function(a,b){var c=a.style,d=a.currentStyle,e=m.isNumeric(b)?"alpha(opacity="+100*b+")":"",f=d&&d.filter||c.filter||"";c.zoom=1,(b>=1||""===b)&&""===m.trim(f.replace(Mb,""))&&c.removeAttribute&&(c.removeAttribute("filter"),""===b||d&&!d.filter)||(c.filter=Mb.test(f)?f.replace(Mb,e):f+" "+e)}}),m.cssHooks.marginRight=Lb(k.reliableMarginRight,function(a,b){return b?m.swap(a,{display:"inline-block"},Jb,[a,"marginRight"]):void 0}),m.each({margin:"",padding:"",border:"Width"},function(a,b){m.cssHooks[a+b]={expand:function(c){for(var d=0,e={},f="string"==typeof c?c.split(" "):[c];4>d;d++)e[a+T[d]+b]=f[d]||f[d-2]||f[0];return e}},Gb.test(a)||(m.cssHooks[a+b].set=Wb)}),m.fn.extend({css:function(a,b){return V(this,function(a,b,c){var d,e,f={},g=0;if(m.isArray(b)){for(d=Ib(a),e=b.length;e>g;g++)f[b[g]]=m.css(a,b[g],!1,d);return f}return void 0!==c?m.style(a,b,c):m.css(a,b)},a,b,arguments.length>1)},show:function(){return Vb(this,!0)},hide:function(){return Vb(this)},toggle:function(a){return"boolean"==typeof a?a?this.show():this.hide():this.each(function(){U(this)?m(this).show():m(this).hide()})}});function Zb(a,b,c,d,e){return new Zb.prototype.init(a,b,c,d,e)}m.Tween=Zb,Zb.prototype={constructor:Zb,init:function(a,b,c,d,e,f){this.elem=a,this.prop=c,this.easing=e||"swing",this.options=b,this.start=this.now=this.cur(),this.end=d,this.unit=f||(m.cssNumber[c]?"":"px")
},cur:function(){var a=Zb.propHooks[this.prop];return a&&a.get?a.get(this):Zb.propHooks._default.get(this)},run:function(a){var b,c=Zb.propHooks[this.prop];return this.pos=b=this.options.duration?m.easing[this.easing](a,this.options.duration*a,0,1,this.options.duration):a,this.now=(this.end-this.start)*b+this.start,this.options.step&&this.options.step.call(this.elem,this.now,this),c&&c.set?c.set(this):Zb.propHooks._default.set(this),this}},Zb.prototype.init.prototype=Zb.prototype,Zb.propHooks={_default:{get:function(a){var b;return null==a.elem[a.prop]||a.elem.style&&null!=a.elem.style[a.prop]?(b=m.css(a.elem,a.prop,""),b&&"auto"!==b?b:0):a.elem[a.prop]},set:function(a){m.fx.step[a.prop]?m.fx.step[a.prop](a):a.elem.style&&(null!=a.elem.style[m.cssProps[a.prop]]||m.cssHooks[a.prop])?m.style(a.elem,a.prop,a.now+a.unit):a.elem[a.prop]=a.now}}},Zb.propHooks.scrollTop=Zb.propHooks.scrollLeft={set:function(a){a.elem.nodeType&&a.elem.parentNode&&(a.elem[a.prop]=a.now)}},m.easing={linear:function(a){return a},swing:function(a){return.5-Math.cos(a*Math.PI)/2}},m.fx=Zb.prototype.init,m.fx.step={};var $b,_b,ac=/^(?:toggle|show|hide)$/,bc=new RegExp("^(?:([+-])=|)("+S+")([a-z%]*)$","i"),cc=/queueHooks$/,dc=[ic],ec={"*":[function(a,b){var c=this.createTween(a,b),d=c.cur(),e=bc.exec(b),f=e&&e[3]||(m.cssNumber[a]?"":"px"),g=(m.cssNumber[a]||"px"!==f&&+d)&&bc.exec(m.css(c.elem,a)),h=1,i=20;if(g&&g[3]!==f){f=f||g[3],e=e||[],g=+d||1;do h=h||".5",g/=h,m.style(c.elem,a,g+f);while(h!==(h=c.cur()/d)&&1!==h&&--i)}return e&&(g=c.start=+g||+d||0,c.unit=f,c.end=e[1]?g+(e[1]+1)*e[2]:+e[2]),c}]};function fc(){return setTimeout(function(){$b=void 0}),$b=m.now()}function gc(a,b){var c,d={height:a},e=0;for(b=b?1:0;4>e;e+=2-b)c=T[e],d["margin"+c]=d["padding"+c]=a;return b&&(d.opacity=d.width=a),d}function hc(a,b,c){for(var d,e=(ec[b]||[]).concat(ec["*"]),f=0,g=e.length;g>f;f++)if(d=e[f].call(c,b,a))return d}function ic(a,b,c){var d,e,f,g,h,i,j,l,n=this,o={},p=a.style,q=a.nodeType&&U(a),r=m._data(a,"fxshow");c.queue||(h=m._queueHooks(a,"fx"),null==h.unqueued&&(h.unqueued=0,i=h.empty.fire,h.empty.fire=function(){h.unqueued||i()}),h.unqueued++,n.always(function(){n.always(function(){h.unqueued--,m.queue(a,"fx").length||h.empty.fire()})})),1===a.nodeType&&("height"in b||"width"in b)&&(c.overflow=[p.overflow,p.overflowX,p.overflowY],j=m.css(a,"display"),l="none"===j?m._data(a,"olddisplay")||Fb(a.nodeName):j,"inline"===l&&"none"===m.css(a,"float")&&(k.inlineBlockNeedsLayout&&"inline"!==Fb(a.nodeName)?p.zoom=1:p.display="inline-block")),c.overflow&&(p.overflow="hidden",k.shrinkWrapBlocks()||n.always(function(){p.overflow=c.overflow[0],p.overflowX=c.overflow[1],p.overflowY=c.overflow[2]}));for(d in b)if(e=b[d],ac.exec(e)){if(delete b[d],f=f||"toggle"===e,e===(q?"hide":"show")){if("show"!==e||!r||void 0===r[d])continue;q=!0}o[d]=r&&r[d]||m.style(a,d)}else j=void 0;if(m.isEmptyObject(o))"inline"===("none"===j?Fb(a.nodeName):j)&&(p.display=j);else{r?"hidden"in r&&(q=r.hidden):r=m._data(a,"fxshow",{}),f&&(r.hidden=!q),q?m(a).show():n.done(function(){m(a).hide()}),n.done(function(){var b;m._removeData(a,"fxshow");for(b in o)m.style(a,b,o[b])});for(d in o)g=hc(q?r[d]:0,d,n),d in r||(r[d]=g.start,q&&(g.end=g.start,g.start="width"===d||"height"===d?1:0))}}function jc(a,b){var c,d,e,f,g;for(c in a)if(d=m.camelCase(c),e=b[d],f=a[c],m.isArray(f)&&(e=f[1],f=a[c]=f[0]),c!==d&&(a[d]=f,delete a[c]),g=m.cssHooks[d],g&&"expand"in g){f=g.expand(f),delete a[d];for(c in f)c in a||(a[c]=f[c],b[c]=e)}else b[d]=e}function kc(a,b,c){var d,e,f=0,g=dc.length,h=m.Deferred().always(function(){delete i.elem}),i=function(){if(e)return!1;for(var b=$b||fc(),c=Math.max(0,j.startTime+j.duration-b),d=c/j.duration||0,f=1-d,g=0,i=j.tweens.length;i>g;g++)j.tweens[g].run(f);return h.notifyWith(a,[j,f,c]),1>f&&i?c:(h.resolveWith(a,[j]),!1)},j=h.promise({elem:a,props:m.extend({},b),opts:m.extend(!0,{specialEasing:{}},c),originalProperties:b,originalOptions:c,startTime:$b||fc(),duration:c.duration,tweens:[],createTween:function(b,c){var d=m.Tween(a,j.opts,b,c,j.opts.specialEasing[b]||j.opts.easing);return j.tweens.push(d),d},stop:function(b){var c=0,d=b?j.tweens.length:0;if(e)return this;for(e=!0;d>c;c++)j.tweens[c].run(1);return b?h.resolveWith(a,[j,b]):h.rejectWith(a,[j,b]),this}}),k=j.props;for(jc(k,j.opts.specialEasing);g>f;f++)if(d=dc[f].call(j,a,k,j.opts))return d;return m.map(k,hc,j),m.isFunction(j.opts.start)&&j.opts.start.call(a,j),m.fx.timer(m.extend(i,{elem:a,anim:j,queue:j.opts.queue})),j.progress(j.opts.progress).done(j.opts.done,j.opts.complete).fail(j.opts.fail).always(j.opts.always)}m.Animation=m.extend(kc,{tweener:function(a,b){m.isFunction(a)?(b=a,a=["*"]):a=a.split(" ");for(var c,d=0,e=a.length;e>d;d++)c=a[d],ec[c]=ec[c]||[],ec[c].unshift(b)},prefilter:function(a,b){b?dc.unshift(a):dc.push(a)}}),m.speed=function(a,b,c){var d=a&&"object"==typeof a?m.extend({},a):{complete:c||!c&&b||m.isFunction(a)&&a,duration:a,easing:c&&b||b&&!m.isFunction(b)&&b};return d.duration=m.fx.off?0:"number"==typeof d.duration?d.duration:d.duration in m.fx.speeds?m.fx.speeds[d.duration]:m.fx.speeds._default,(null==d.queue||d.queue===!0)&&(d.queue="fx"),d.old=d.complete,d.complete=function(){m.isFunction(d.old)&&d.old.call(this),d.queue&&m.dequeue(this,d.queue)},d},m.fn.extend({fadeTo:function(a,b,c,d){return this.filter(U).css("opacity",0).show().end().animate({opacity:b},a,c,d)},animate:function(a,b,c,d){var e=m.isEmptyObject(a),f=m.speed(b,c,d),g=function(){var b=kc(this,m.extend({},a),f);(e||m._data(this,"finish"))&&b.stop(!0)};return g.finish=g,e||f.queue===!1?this.each(g):this.queue(f.queue,g)},stop:function(a,b,c){var d=function(a){var b=a.stop;delete a.stop,b(c)};return"string"!=typeof a&&(c=b,b=a,a=void 0),b&&a!==!1&&this.queue(a||"fx",[]),this.each(function(){var b=!0,e=null!=a&&a+"queueHooks",f=m.timers,g=m._data(this);if(e)g[e]&&g[e].stop&&d(g[e]);else for(e in g)g[e]&&g[e].stop&&cc.test(e)&&d(g[e]);for(e=f.length;e--;)f[e].elem!==this||null!=a&&f[e].queue!==a||(f[e].anim.stop(c),b=!1,f.splice(e,1));(b||!c)&&m.dequeue(this,a)})},finish:function(a){return a!==!1&&(a=a||"fx"),this.each(function(){var b,c=m._data(this),d=c[a+"queue"],e=c[a+"queueHooks"],f=m.timers,g=d?d.length:0;for(c.finish=!0,m.queue(this,a,[]),e&&e.stop&&e.stop.call(this,!0),b=f.length;b--;)f[b].elem===this&&f[b].queue===a&&(f[b].anim.stop(!0),f.splice(b,1));for(b=0;g>b;b++)d[b]&&d[b].finish&&d[b].finish.call(this);delete c.finish})}}),m.each(["toggle","show","hide"],function(a,b){var c=m.fn[b];m.fn[b]=function(a,d,e){return null==a||"boolean"==typeof a?c.apply(this,arguments):this.animate(gc(b,!0),a,d,e)}}),m.each({slideDown:gc("show"),slideUp:gc("hide"),slideToggle:gc("toggle"),fadeIn:{opacity:"show"},fadeOut:{opacity:"hide"},fadeToggle:{opacity:"toggle"}},function(a,b){m.fn[a]=function(a,c,d){return this.animate(b,a,c,d)}}),m.timers=[],m.fx.tick=function(){var a,b=m.timers,c=0;for($b=m.now();c<b.length;c++)a=b[c],a()||b[c]!==a||b.splice(c--,1);b.length||m.fx.stop(),$b=void 0},m.fx.timer=function(a){m.timers.push(a),a()?m.fx.start():m.timers.pop()},m.fx.interval=13,m.fx.start=function(){_b||(_b=setInterval(m.fx.tick,m.fx.interval))},m.fx.stop=function(){clearInterval(_b),_b=null},m.fx.speeds={slow:600,fast:200,_default:400},m.fn.delay=function(a,b){return a=m.fx?m.fx.speeds[a]||a:a,b=b||"fx",this.queue(b,function(b,c){var d=setTimeout(b,a);c.stop=function(){clearTimeout(d)}})},function(){var a,b,c,d,e;b=y.createElement("div"),b.setAttribute("className","t"),b.innerHTML="  <link/><table></table><a href='/a'>a</a><input type='checkbox'/>",d=b.getElementsByTagName("a")[0],c=y.createElement("select"),e=c.appendChild(y.createElement("option")),a=b.getElementsByTagName("input")[0],d.style.cssText="top:1px",k.getSetAttribute="t"!==b.className,k.style=/top/.test(d.getAttribute("style")),k.hrefNormalized="/a"===d.getAttribute("href"),k.checkOn=!!a.value,k.optSelected=e.selected,k.enctype=!!y.createElement("form").enctype,c.disabled=!0,k.optDisabled=!e.disabled,a=y.createElement("input"),a.setAttribute("value",""),k.input=""===a.getAttribute("value"),a.value="t",a.setAttribute("type","radio"),k.radioValue="t"===a.value}();var lc=/\r/g;m.fn.extend({val:function(a){var b,c,d,e=this[0];{if(arguments.length)return d=m.isFunction(a),this.each(function(c){var e;1===this.nodeType&&(e=d?a.call(this,c,m(this).val()):a,null==e?e="":"number"==typeof e?e+="":m.isArray(e)&&(e=m.map(e,function(a){return null==a?"":a+""})),b=m.valHooks[this.type]||m.valHooks[this.nodeName.toLowerCase()],b&&"set"in b&&void 0!==b.set(this,e,"value")||(this.value=e))});if(e)return b=m.valHooks[e.type]||m.valHooks[e.nodeName.toLowerCase()],b&&"get"in b&&void 0!==(c=b.get(e,"value"))?c:(c=e.value,"string"==typeof c?c.replace(lc,""):null==c?"":c)}}}),m.extend({valHooks:{option:{get:function(a){var b=m.find.attr(a,"value");return null!=b?b:m.trim(m.text(a))}},select:{get:function(a){for(var b,c,d=a.options,e=a.selectedIndex,f="select-one"===a.type||0>e,g=f?null:[],h=f?e+1:d.length,i=0>e?h:f?e:0;h>i;i++)if(c=d[i],!(!c.selected&&i!==e||(k.optDisabled?c.disabled:null!==c.getAttribute("disabled"))||c.parentNode.disabled&&m.nodeName(c.parentNode,"optgroup"))){if(b=m(c).val(),f)return b;g.push(b)}return g},set:function(a,b){var c,d,e=a.options,f=m.makeArray(b),g=e.length;while(g--)if(d=e[g],m.inArray(m.valHooks.option.get(d),f)>=0)try{d.selected=c=!0}catch(h){d.scrollHeight}else d.selected=!1;return c||(a.selectedIndex=-1),e}}}}),m.each(["radio","checkbox"],function(){m.valHooks[this]={set:function(a,b){return m.isArray(b)?a.checked=m.inArray(m(a).val(),b)>=0:void 0}},k.checkOn||(m.valHooks[this].get=function(a){return null===a.getAttribute("value")?"on":a.value})});var mc,nc,oc=m.expr.attrHandle,pc=/^(?:checked|selected)$/i,qc=k.getSetAttribute,rc=k.input;m.fn.extend({attr:function(a,b){return V(this,m.attr,a,b,arguments.length>1)},removeAttr:function(a){return this.each(function(){m.removeAttr(this,a)})}}),m.extend({attr:function(a,b,c){var d,e,f=a.nodeType;if(a&&3!==f&&8!==f&&2!==f)return typeof a.getAttribute===K?m.prop(a,b,c):(1===f&&m.isXMLDoc(a)||(b=b.toLowerCase(),d=m.attrHooks[b]||(m.expr.match.bool.test(b)?nc:mc)),void 0===c?d&&"get"in d&&null!==(e=d.get(a,b))?e:(e=m.find.attr(a,b),null==e?void 0:e):null!==c?d&&"set"in d&&void 0!==(e=d.set(a,c,b))?e:(a.setAttribute(b,c+""),c):void m.removeAttr(a,b))},removeAttr:function(a,b){var c,d,e=0,f=b&&b.match(E);if(f&&1===a.nodeType)while(c=f[e++])d=m.propFix[c]||c,m.expr.match.bool.test(c)?rc&&qc||!pc.test(c)?a[d]=!1:a[m.camelCase("default-"+c)]=a[d]=!1:m.attr(a,c,""),a.removeAttribute(qc?c:d)},attrHooks:{type:{set:function(a,b){if(!k.radioValue&&"radio"===b&&m.nodeName(a,"input")){var c=a.value;return a.setAttribute("type",b),c&&(a.value=c),b}}}}}),nc={set:function(a,b,c){return b===!1?m.removeAttr(a,c):rc&&qc||!pc.test(c)?a.setAttribute(!qc&&m.propFix[c]||c,c):a[m.camelCase("default-"+c)]=a[c]=!0,c}},m.each(m.expr.match.bool.source.match(/\w+/g),function(a,b){var c=oc[b]||m.find.attr;oc[b]=rc&&qc||!pc.test(b)?function(a,b,d){var e,f;return d||(f=oc[b],oc[b]=e,e=null!=c(a,b,d)?b.toLowerCase():null,oc[b]=f),e}:function(a,b,c){return c?void 0:a[m.camelCase("default-"+b)]?b.toLowerCase():null}}),rc&&qc||(m.attrHooks.value={set:function(a,b,c){return m.nodeName(a,"input")?void(a.defaultValue=b):mc&&mc.set(a,b,c)}}),qc||(mc={set:function(a,b,c){var d=a.getAttributeNode(c);return d||a.setAttributeNode(d=a.ownerDocument.createAttribute(c)),d.value=b+="","value"===c||b===a.getAttribute(c)?b:void 0}},oc.id=oc.name=oc.coords=function(a,b,c){var d;return c?void 0:(d=a.getAttributeNode(b))&&""!==d.value?d.value:null},m.valHooks.button={get:function(a,b){var c=a.getAttributeNode(b);return c&&c.specified?c.value:void 0},set:mc.set},m.attrHooks.contenteditable={set:function(a,b,c){mc.set(a,""===b?!1:b,c)}},m.each(["width","height"],function(a,b){m.attrHooks[b]={set:function(a,c){return""===c?(a.setAttribute(b,"auto"),c):void 0}}})),k.style||(m.attrHooks.style={get:function(a){return a.style.cssText||void 0},set:function(a,b){return a.style.cssText=b+""}});var sc=/^(?:input|select|textarea|button|object)$/i,tc=/^(?:a|area)$/i;m.fn.extend({prop:function(a,b){return V(this,m.prop,a,b,arguments.length>1)},removeProp:function(a){return a=m.propFix[a]||a,this.each(function(){try{this[a]=void 0,delete this[a]}catch(b){}})}}),m.extend({propFix:{"for":"htmlFor","class":"className"},prop:function(a,b,c){var d,e,f,g=a.nodeType;if(a&&3!==g&&8!==g&&2!==g)return f=1!==g||!m.isXMLDoc(a),f&&(b=m.propFix[b]||b,e=m.propHooks[b]),void 0!==c?e&&"set"in e&&void 0!==(d=e.set(a,c,b))?d:a[b]=c:e&&"get"in e&&null!==(d=e.get(a,b))?d:a[b]},propHooks:{tabIndex:{get:function(a){var b=m.find.attr(a,"tabindex");return b?parseInt(b,10):sc.test(a.nodeName)||tc.test(a.nodeName)&&a.href?0:-1}}}}),k.hrefNormalized||m.each(["href","src"],function(a,b){m.propHooks[b]={get:function(a){return a.getAttribute(b,4)}}}),k.optSelected||(m.propHooks.selected={get:function(a){var b=a.parentNode;return b&&(b.selectedIndex,b.parentNode&&b.parentNode.selectedIndex),null}}),m.each(["tabIndex","readOnly","maxLength","cellSpacing","cellPadding","rowSpan","colSpan","useMap","frameBorder","contentEditable"],function(){m.propFix[this.toLowerCase()]=this}),k.enctype||(m.propFix.enctype="encoding");var uc=/[\t\r\n\f]/g;m.fn.extend({addClass:function(a){var b,c,d,e,f,g,h=0,i=this.length,j="string"==typeof a&&a;if(m.isFunction(a))return this.each(function(b){m(this).addClass(a.call(this,b,this.className))});if(j)for(b=(a||"").match(E)||[];i>h;h++)if(c=this[h],d=1===c.nodeType&&(c.className?(" "+c.className+" ").replace(uc," "):" ")){f=0;while(e=b[f++])d.indexOf(" "+e+" ")<0&&(d+=e+" ");g=m.trim(d),c.className!==g&&(c.className=g)}return this},removeClass:function(a){var b,c,d,e,f,g,h=0,i=this.length,j=0===arguments.length||"string"==typeof a&&a;if(m.isFunction(a))return this.each(function(b){m(this).removeClass(a.call(this,b,this.className))});if(j)for(b=(a||"").match(E)||[];i>h;h++)if(c=this[h],d=1===c.nodeType&&(c.className?(" "+c.className+" ").replace(uc," "):"")){f=0;while(e=b[f++])while(d.indexOf(" "+e+" ")>=0)d=d.replace(" "+e+" "," ");g=a?m.trim(d):"",c.className!==g&&(c.className=g)}return this},toggleClass:function(a,b){var c=typeof a;return"boolean"==typeof b&&"string"===c?b?this.addClass(a):this.removeClass(a):this.each(m.isFunction(a)?function(c){m(this).toggleClass(a.call(this,c,this.className,b),b)}:function(){if("string"===c){var b,d=0,e=m(this),f=a.match(E)||[];while(b=f[d++])e.hasClass(b)?e.removeClass(b):e.addClass(b)}else(c===K||"boolean"===c)&&(this.className&&m._data(this,"__className__",this.className),this.className=this.className||a===!1?"":m._data(this,"__className__")||"")})},hasClass:function(a){for(var b=" "+a+" ",c=0,d=this.length;d>c;c++)if(1===this[c].nodeType&&(" "+this[c].className+" ").replace(uc," ").indexOf(b)>=0)return!0;return!1}}),m.each("blur focus focusin focusout load resize scroll unload click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup error contextmenu".split(" "),function(a,b){m.fn[b]=function(a,c){return arguments.length>0?this.on(b,null,a,c):this.trigger(b)}}),m.fn.extend({hover:function(a,b){return this.mouseenter(a).mouseleave(b||a)},bind:function(a,b,c){return this.on(a,null,b,c)},unbind:function(a,b){return this.off(a,null,b)},delegate:function(a,b,c,d){return this.on(b,a,c,d)},undelegate:function(a,b,c){return 1===arguments.length?this.off(a,"**"):this.off(b,a||"**",c)}});var vc=m.now(),wc=/\?/,xc=/(,)|(\[|{)|(}|])|"(?:[^"\\\r\n]|\\["\\\/bfnrt]|\\u[\da-fA-F]{4})*"\s*:?|true|false|null|-?(?!0\d)\d+(?:\.\d+|)(?:[eE][+-]?\d+|)/g;m.parseJSON=function(b){if(a.JSON&&a.JSON.parse)return a.JSON.parse(b+"");var c,d=null,e=m.trim(b+"");return e&&!m.trim(e.replace(xc,function(a,b,e,f){return c&&b&&(d=0),0===d?a:(c=e||b,d+=!f-!e,"")}))?Function("return "+e)():m.error("Invalid JSON: "+b)},m.parseXML=function(b){var c,d;if(!b||"string"!=typeof b)return null;try{a.DOMParser?(d=new DOMParser,c=d.parseFromString(b,"text/xml")):(c=new ActiveXObject("Microsoft.XMLDOM"),c.async="false",c.loadXML(b))}catch(e){c=void 0}return c&&c.documentElement&&!c.getElementsByTagName("parsererror").length||m.error("Invalid XML: "+b),c};var yc,zc,Ac=/#.*$/,Bc=/([?&])_=[^&]*/,Cc=/^(.*?):[ \t]*([^\r\n]*)\r?$/gm,Dc=/^(?:about|app|app-storage|.+-extension|file|res|widget):$/,Ec=/^(?:GET|HEAD)$/,Fc=/^\/\//,Gc=/^([\w.+-]+:)(?:\/\/(?:[^\/?#]*@|)([^\/?#:]*)(?::(\d+)|)|)/,Hc={},Ic={},Jc="*/".concat("*");try{zc=location.href}catch(Kc){zc=y.createElement("a"),zc.href="",zc=zc.href}yc=Gc.exec(zc.toLowerCase())||[];function Lc(a){return function(b,c){"string"!=typeof b&&(c=b,b="*");var d,e=0,f=b.toLowerCase().match(E)||[];if(m.isFunction(c))while(d=f[e++])"+"===d.charAt(0)?(d=d.slice(1)||"*",(a[d]=a[d]||[]).unshift(c)):(a[d]=a[d]||[]).push(c)}}function Mc(a,b,c,d){var e={},f=a===Ic;function g(h){var i;return e[h]=!0,m.each(a[h]||[],function(a,h){var j=h(b,c,d);return"string"!=typeof j||f||e[j]?f?!(i=j):void 0:(b.dataTypes.unshift(j),g(j),!1)}),i}return g(b.dataTypes[0])||!e["*"]&&g("*")}function Nc(a,b){var c,d,e=m.ajaxSettings.flatOptions||{};for(d in b)void 0!==b[d]&&((e[d]?a:c||(c={}))[d]=b[d]);return c&&m.extend(!0,a,c),a}function Oc(a,b,c){var d,e,f,g,h=a.contents,i=a.dataTypes;while("*"===i[0])i.shift(),void 0===e&&(e=a.mimeType||b.getResponseHeader("Content-Type"));if(e)for(g in h)if(h[g]&&h[g].test(e)){i.unshift(g);break}if(i[0]in c)f=i[0];else{for(g in c){if(!i[0]||a.converters[g+" "+i[0]]){f=g;break}d||(d=g)}f=f||d}return f?(f!==i[0]&&i.unshift(f),c[f]):void 0}function Pc(a,b,c,d){var e,f,g,h,i,j={},k=a.dataTypes.slice();if(k[1])for(g in a.converters)j[g.toLowerCase()]=a.converters[g];f=k.shift();while(f)if(a.responseFields[f]&&(c[a.responseFields[f]]=b),!i&&d&&a.dataFilter&&(b=a.dataFilter(b,a.dataType)),i=f,f=k.shift())if("*"===f)f=i;else if("*"!==i&&i!==f){if(g=j[i+" "+f]||j["* "+f],!g)for(e in j)if(h=e.split(" "),h[1]===f&&(g=j[i+" "+h[0]]||j["* "+h[0]])){g===!0?g=j[e]:j[e]!==!0&&(f=h[0],k.unshift(h[1]));break}if(g!==!0)if(g&&a["throws"])b=g(b);else try{b=g(b)}catch(l){return{state:"parsererror",error:g?l:"No conversion from "+i+" to "+f}}}return{state:"success",data:b}}m.extend({active:0,lastModified:{},etag:{},ajaxSettings:{url:zc,type:"GET",isLocal:Dc.test(yc[1]),global:!0,processData:!0,async:!0,contentType:"application/x-www-form-urlencoded; charset=UTF-8",accepts:{"*":Jc,text:"text/plain",html:"text/html",xml:"application/xml, text/xml",json:"application/json, text/javascript"},contents:{xml:/xml/,html:/html/,json:/json/},responseFields:{xml:"responseXML",text:"responseText",json:"responseJSON"},converters:{"* text":String,"text html":!0,"text json":m.parseJSON,"text xml":m.parseXML},flatOptions:{url:!0,context:!0}},ajaxSetup:function(a,b){return b?Nc(Nc(a,m.ajaxSettings),b):Nc(m.ajaxSettings,a)},ajaxPrefilter:Lc(Hc),ajaxTransport:Lc(Ic),ajax:function(a,b){"object"==typeof a&&(b=a,a=void 0),b=b||{};var c,d,e,f,g,h,i,j,k=m.ajaxSetup({},b),l=k.context||k,n=k.context&&(l.nodeType||l.jquery)?m(l):m.event,o=m.Deferred(),p=m.Callbacks("once memory"),q=k.statusCode||{},r={},s={},t=0,u="canceled",v={readyState:0,getResponseHeader:function(a){var b;if(2===t){if(!j){j={};while(b=Cc.exec(f))j[b[1].toLowerCase()]=b[2]}b=j[a.toLowerCase()]}return null==b?null:b},getAllResponseHeaders:function(){return 2===t?f:null},setRequestHeader:function(a,b){var c=a.toLowerCase();return t||(a=s[c]=s[c]||a,r[a]=b),this},overrideMimeType:function(a){return t||(k.mimeType=a),this},statusCode:function(a){var b;if(a)if(2>t)for(b in a)q[b]=[q[b],a[b]];else v.always(a[v.status]);return this},abort:function(a){var b=a||u;return i&&i.abort(b),x(0,b),this}};if(o.promise(v).complete=p.add,v.success=v.done,v.error=v.fail,k.url=((a||k.url||zc)+"").replace(Ac,"").replace(Fc,yc[1]+"//"),k.type=b.method||b.type||k.method||k.type,k.dataTypes=m.trim(k.dataType||"*").toLowerCase().match(E)||[""],null==k.crossDomain&&(c=Gc.exec(k.url.toLowerCase()),k.crossDomain=!(!c||c[1]===yc[1]&&c[2]===yc[2]&&(c[3]||("http:"===c[1]?"80":"443"))===(yc[3]||("http:"===yc[1]?"80":"443")))),k.data&&k.processData&&"string"!=typeof k.data&&(k.data=m.param(k.data,k.traditional)),Mc(Hc,k,b,v),2===t)return v;h=k.global,h&&0===m.active++&&m.event.trigger("ajaxStart"),k.type=k.type.toUpperCase(),k.hasContent=!Ec.test(k.type),e=k.url,k.hasContent||(k.data&&(e=k.url+=(wc.test(e)?"&":"?")+k.data,delete k.data),k.cache===!1&&(k.url=Bc.test(e)?e.replace(Bc,"$1_="+vc++):e+(wc.test(e)?"&":"?")+"_="+vc++)),k.ifModified&&(m.lastModified[e]&&v.setRequestHeader("If-Modified-Since",m.lastModified[e]),m.etag[e]&&v.setRequestHeader("If-None-Match",m.etag[e])),(k.data&&k.hasContent&&k.contentType!==!1||b.contentType)&&v.setRequestHeader("Content-Type",k.contentType),v.setRequestHeader("Accept",k.dataTypes[0]&&k.accepts[k.dataTypes[0]]?k.accepts[k.dataTypes[0]]+("*"!==k.dataTypes[0]?", "+Jc+"; q=0.01":""):k.accepts["*"]);for(d in k.headers)v.setRequestHeader(d,k.headers[d]);if(k.beforeSend&&(k.beforeSend.call(l,v,k)===!1||2===t))return v.abort();u="abort";for(d in{success:1,error:1,complete:1})v[d](k[d]);if(i=Mc(Ic,k,b,v)){v.readyState=1,h&&n.trigger("ajaxSend",[v,k]),k.async&&k.timeout>0&&(g=setTimeout(function(){v.abort("timeout")},k.timeout));try{t=1,i.send(r,x)}catch(w){if(!(2>t))throw w;x(-1,w)}}else x(-1,"No Transport");function x(a,b,c,d){var j,r,s,u,w,x=b;2!==t&&(t=2,g&&clearTimeout(g),i=void 0,f=d||"",v.readyState=a>0?4:0,j=a>=200&&300>a||304===a,c&&(u=Oc(k,v,c)),u=Pc(k,u,v,j),j?(k.ifModified&&(w=v.getResponseHeader("Last-Modified"),w&&(m.lastModified[e]=w),w=v.getResponseHeader("etag"),w&&(m.etag[e]=w)),204===a||"HEAD"===k.type?x="nocontent":304===a?x="notmodified":(x=u.state,r=u.data,s=u.error,j=!s)):(s=x,(a||!x)&&(x="error",0>a&&(a=0))),v.status=a,v.statusText=(b||x)+"",j?o.resolveWith(l,[r,x,v]):o.rejectWith(l,[v,x,s]),v.statusCode(q),q=void 0,h&&n.trigger(j?"ajaxSuccess":"ajaxError",[v,k,j?r:s]),p.fireWith(l,[v,x]),h&&(n.trigger("ajaxComplete",[v,k]),--m.active||m.event.trigger("ajaxStop")))}return v},getJSON:function(a,b,c){return m.get(a,b,c,"json")},getScript:function(a,b){return m.get(a,void 0,b,"script")}}),m.each(["get","post"],function(a,b){m[b]=function(a,c,d,e){return m.isFunction(c)&&(e=e||d,d=c,c=void 0),m.ajax({url:a,type:b,dataType:e,data:c,success:d})}}),m.each(["ajaxStart","ajaxStop","ajaxComplete","ajaxError","ajaxSuccess","ajaxSend"],function(a,b){m.fn[b]=function(a){return this.on(b,a)}}),m._evalUrl=function(a){return m.ajax({url:a,type:"GET",dataType:"script",async:!1,global:!1,"throws":!0})},m.fn.extend({wrapAll:function(a){if(m.isFunction(a))return this.each(function(b){m(this).wrapAll(a.call(this,b))});if(this[0]){var b=m(a,this[0].ownerDocument).eq(0).clone(!0);this[0].parentNode&&b.insertBefore(this[0]),b.map(function(){var a=this;while(a.firstChild&&1===a.firstChild.nodeType)a=a.firstChild;return a}).append(this)}return this},wrapInner:function(a){return this.each(m.isFunction(a)?function(b){m(this).wrapInner(a.call(this,b))}:function(){var b=m(this),c=b.contents();c.length?c.wrapAll(a):b.append(a)})},wrap:function(a){var b=m.isFunction(a);return this.each(function(c){m(this).wrapAll(b?a.call(this,c):a)})},unwrap:function(){return this.parent().each(function(){m.nodeName(this,"body")||m(this).replaceWith(this.childNodes)}).end()}}),m.expr.filters.hidden=function(a){return a.offsetWidth<=0&&a.offsetHeight<=0||!k.reliableHiddenOffsets()&&"none"===(a.style&&a.style.display||m.css(a,"display"))},m.expr.filters.visible=function(a){return!m.expr.filters.hidden(a)};var Qc=/%20/g,Rc=/\[\]$/,Sc=/\r?\n/g,Tc=/^(?:submit|button|image|reset|file)$/i,Uc=/^(?:input|select|textarea|keygen)/i;function Vc(a,b,c,d){var e;if(m.isArray(b))m.each(b,function(b,e){c||Rc.test(a)?d(a,e):Vc(a+"["+("object"==typeof e?b:"")+"]",e,c,d)});else if(c||"object"!==m.type(b))d(a,b);else for(e in b)Vc(a+"["+e+"]",b[e],c,d)}m.param=function(a,b){var c,d=[],e=function(a,b){b=m.isFunction(b)?b():null==b?"":b,d[d.length]=encodeURIComponent(a)+"="+encodeURIComponent(b)};if(void 0===b&&(b=m.ajaxSettings&&m.ajaxSettings.traditional),m.isArray(a)||a.jquery&&!m.isPlainObject(a))m.each(a,function(){e(this.name,this.value)});else for(c in a)Vc(c,a[c],b,e);return d.join("&").replace(Qc,"+")},m.fn.extend({serialize:function(){return m.param(this.serializeArray())},serializeArray:function(){return this.map(function(){var a=m.prop(this,"elements");return a?m.makeArray(a):this}).filter(function(){var a=this.type;return this.name&&!m(this).is(":disabled")&&Uc.test(this.nodeName)&&!Tc.test(a)&&(this.checked||!W.test(a))}).map(function(a,b){var c=m(this).val();return null==c?null:m.isArray(c)?m.map(c,function(a){return{name:b.name,value:a.replace(Sc,"\r\n")}}):{name:b.name,value:c.replace(Sc,"\r\n")}}).get()}}),m.ajaxSettings.xhr=void 0!==a.ActiveXObject?function(){return!this.isLocal&&/^(get|post|head|put|delete|options)$/i.test(this.type)&&Zc()||$c()}:Zc;var Wc=0,Xc={},Yc=m.ajaxSettings.xhr();a.ActiveXObject&&m(a).on("unload",function(){for(var a in Xc)Xc[a](void 0,!0)}),k.cors=!!Yc&&"withCredentials"in Yc,Yc=k.ajax=!!Yc,Yc&&m.ajaxTransport(function(a){if(!a.crossDomain||k.cors){var b;return{send:function(c,d){var e,f=a.xhr(),g=++Wc;if(f.open(a.type,a.url,a.async,a.username,a.password),a.xhrFields)for(e in a.xhrFields)f[e]=a.xhrFields[e];a.mimeType&&f.overrideMimeType&&f.overrideMimeType(a.mimeType),a.crossDomain||c["X-Requested-With"]||(c["X-Requested-With"]="XMLHttpRequest");for(e in c)void 0!==c[e]&&f.setRequestHeader(e,c[e]+"");f.send(a.hasContent&&a.data||null),b=function(c,e){var h,i,j;if(b&&(e||4===f.readyState))if(delete Xc[g],b=void 0,f.onreadystatechange=m.noop,e)4!==f.readyState&&f.abort();else{j={},h=f.status,"string"==typeof f.responseText&&(j.text=f.responseText);try{i=f.statusText}catch(k){i=""}h||!a.isLocal||a.crossDomain?1223===h&&(h=204):h=j.text?200:404}j&&d(h,i,j,f.getAllResponseHeaders())},a.async?4===f.readyState?setTimeout(b):f.onreadystatechange=Xc[g]=b:b()},abort:function(){b&&b(void 0,!0)}}}});function Zc(){try{return new a.XMLHttpRequest}catch(b){}}function $c(){try{return new a.ActiveXObject("Microsoft.XMLHTTP")}catch(b){}}m.ajaxSetup({accepts:{script:"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript"},contents:{script:/(?:java|ecma)script/},converters:{"text script":function(a){return m.globalEval(a),a}}}),m.ajaxPrefilter("script",function(a){void 0===a.cache&&(a.cache=!1),a.crossDomain&&(a.type="GET",a.global=!1)}),m.ajaxTransport("script",function(a){if(a.crossDomain){var b,c=y.head||m("head")[0]||y.documentElement;return{send:function(d,e){b=y.createElement("script"),b.async=!0,a.scriptCharset&&(b.charset=a.scriptCharset),b.src=a.url,b.onload=b.onreadystatechange=function(a,c){(c||!b.readyState||/loaded|complete/.test(b.readyState))&&(b.onload=b.onreadystatechange=null,b.parentNode&&b.parentNode.removeChild(b),b=null,c||e(200,"success"))},c.insertBefore(b,c.firstChild)},abort:function(){b&&b.onload(void 0,!0)}}}});var _c=[],ad=/(=)\?(?=&|$)|\?\?/;m.ajaxSetup({jsonp:"callback",jsonpCallback:function(){var a=_c.pop()||m.expando+"_"+vc++;return this[a]=!0,a}}),m.ajaxPrefilter("json jsonp",function(b,c,d){var e,f,g,h=b.jsonp!==!1&&(ad.test(b.url)?"url":"string"==typeof b.data&&!(b.contentType||"").indexOf("application/x-www-form-urlencoded")&&ad.test(b.data)&&"data");return h||"jsonp"===b.dataTypes[0]?(e=b.jsonpCallback=m.isFunction(b.jsonpCallback)?b.jsonpCallback():b.jsonpCallback,h?b[h]=b[h].replace(ad,"$1"+e):b.jsonp!==!1&&(b.url+=(wc.test(b.url)?"&":"?")+b.jsonp+"="+e),b.converters["script json"]=function(){return g||m.error(e+" was not called"),g[0]},b.dataTypes[0]="json",f=a[e],a[e]=function(){g=arguments},d.always(function(){a[e]=f,b[e]&&(b.jsonpCallback=c.jsonpCallback,_c.push(e)),g&&m.isFunction(f)&&f(g[0]),g=f=void 0}),"script"):void 0}),m.parseHTML=function(a,b,c){if(!a||"string"!=typeof a)return null;"boolean"==typeof b&&(c=b,b=!1),b=b||y;var d=u.exec(a),e=!c&&[];return d?[b.createElement(d[1])]:(d=m.buildFragment([a],b,e),e&&e.length&&m(e).remove(),m.merge([],d.childNodes))};var bd=m.fn.load;m.fn.load=function(a,b,c){if("string"!=typeof a&&bd)return bd.apply(this,arguments);var d,e,f,g=this,h=a.indexOf(" ");return h>=0&&(d=m.trim(a.slice(h,a.length)),a=a.slice(0,h)),m.isFunction(b)?(c=b,b=void 0):b&&"object"==typeof b&&(f="POST"),g.length>0&&m.ajax({url:a,type:f,dataType:"html",data:b}).done(function(a){e=arguments,g.html(d?m("<div>").append(m.parseHTML(a)).find(d):a)}).complete(c&&function(a,b){g.each(c,e||[a.responseText,b,a])}),this},m.expr.filters.animated=function(a){return m.grep(m.timers,function(b){return a===b.elem}).length};var cd=a.document.documentElement;function dd(a){return m.isWindow(a)?a:9===a.nodeType?a.defaultView||a.parentWindow:!1}m.offset={setOffset:function(a,b,c){var d,e,f,g,h,i,j,k=m.css(a,"position"),l=m(a),n={};"static"===k&&(a.style.position="relative"),h=l.offset(),f=m.css(a,"top"),i=m.css(a,"left"),j=("absolute"===k||"fixed"===k)&&m.inArray("auto",[f,i])>-1,j?(d=l.position(),g=d.top,e=d.left):(g=parseFloat(f)||0,e=parseFloat(i)||0),m.isFunction(b)&&(b=b.call(a,c,h)),null!=b.top&&(n.top=b.top-h.top+g),null!=b.left&&(n.left=b.left-h.left+e),"using"in b?b.using.call(a,n):l.css(n)}},m.fn.extend({offset:function(a){if(arguments.length)return void 0===a?this:this.each(function(b){m.offset.setOffset(this,a,b)});var b,c,d={top:0,left:0},e=this[0],f=e&&e.ownerDocument;if(f)return b=f.documentElement,m.contains(b,e)?(typeof e.getBoundingClientRect!==K&&(d=e.getBoundingClientRect()),c=dd(f),{top:d.top+(c.pageYOffset||b.scrollTop)-(b.clientTop||0),left:d.left+(c.pageXOffset||b.scrollLeft)-(b.clientLeft||0)}):d},position:function(){if(this[0]){var a,b,c={top:0,left:0},d=this[0];return"fixed"===m.css(d,"position")?b=d.getBoundingClientRect():(a=this.offsetParent(),b=this.offset(),m.nodeName(a[0],"html")||(c=a.offset()),c.top+=m.css(a[0],"borderTopWidth",!0),c.left+=m.css(a[0],"borderLeftWidth",!0)),{top:b.top-c.top-m.css(d,"marginTop",!0),left:b.left-c.left-m.css(d,"marginLeft",!0)}}},offsetParent:function(){return this.map(function(){var a=this.offsetParent||cd;while(a&&!m.nodeName(a,"html")&&"static"===m.css(a,"position"))a=a.offsetParent;return a||cd})}}),m.each({scrollLeft:"pageXOffset",scrollTop:"pageYOffset"},function(a,b){var c=/Y/.test(b);m.fn[a]=function(d){return V(this,function(a,d,e){var f=dd(a);return void 0===e?f?b in f?f[b]:f.document.documentElement[d]:a[d]:void(f?f.scrollTo(c?m(f).scrollLeft():e,c?e:m(f).scrollTop()):a[d]=e)},a,d,arguments.length,null)}}),m.each(["top","left"],function(a,b){m.cssHooks[b]=Lb(k.pixelPosition,function(a,c){return c?(c=Jb(a,b),Hb.test(c)?m(a).position()[b]+"px":c):void 0})}),m.each({Height:"height",Width:"width"},function(a,b){m.each({padding:"inner"+a,content:b,"":"outer"+a},function(c,d){m.fn[d]=function(d,e){var f=arguments.length&&(c||"boolean"!=typeof d),g=c||(d===!0||e===!0?"margin":"border");return V(this,function(b,c,d){var e;return m.isWindow(b)?b.document.documentElement["client"+a]:9===b.nodeType?(e=b.documentElement,Math.max(b.body["scroll"+a],e["scroll"+a],b.body["offset"+a],e["offset"+a],e["client"+a])):void 0===d?m.css(b,c,g):m.style(b,c,d,g)},b,f?d:void 0,f,null)}})}),m.fn.size=function(){return this.length},m.fn.andSelf=m.fn.addBack,"function"==typeof define&&define.amd&&define("jquery",[],function(){return m});var ed=a.jQuery,fd=a.$;return m.noConflict=function(b){return a.$===m&&(a.$=fd),b&&a.jQuery===m&&(a.jQuery=ed),m},typeof b===K&&(a.jQuery=a.$=m),m});
8 core/jsplot/jquery.mousewheel.min.js
/*!
 * jQuery Mousewheel 3.1.13
 *
 * Copyright 2015 jQuery Foundation and other contributors
 * Released under the MIT license.
 * http://jquery.org/license
 */
!function(a){"function"==typeof define&&define.amd?define(["jquery"],a):"object"==typeof exports?module.exports=a:a(jQuery)}(function(a){function b(b){var g=b||window.event,h=i.call(arguments,1),j=0,l=0,m=0,n=0,o=0,p=0;if(b=a.event.fix(g),b.type="mousewheel","detail"in g&&(m=-1*g.detail),"wheelDelta"in g&&(m=g.wheelDelta),"wheelDeltaY"in g&&(m=g.wheelDeltaY),"wheelDeltaX"in g&&(l=-1*g.wheelDeltaX),"axis"in g&&g.axis===g.HORIZONTAL_AXIS&&(l=-1*m,m=0),j=0===m?l:m,"deltaY"in g&&(m=-1*g.deltaY,j=m),"deltaX"in g&&(l=g.deltaX,0===m&&(j=-1*l)),0!==m||0!==l){if(1===g.deltaMode){var q=a.data(this,"mousewheel-line-height");j*=q,m*=q,l*=q}else if(2===g.deltaMode){var r=a.data(this,"mousewheel-page-height");j*=r,m*=r,l*=r}if(n=Math.max(Math.abs(m),Math.abs(l)),(!f||f>n)&&(f=n,d(g,n)&&(f/=40)),d(g,n)&&(j/=40,l/=40,m/=40),j=Math[j>=1?"floor":"ceil"](j/f),l=Math[l>=1?"floor":"ceil"](l/f),m=Math[m>=1?"floor":"ceil"](m/f),k.settings.normalizeOffset&&this.getBoundingClientRect){var s=this.getBoundingClientRect();o=b.clientX-s.left,p=b.clientY-s.top}return b.deltaX=l,b.deltaY=m,b.deltaFactor=f,b.offsetX=o,b.offsetY=p,b.deltaMode=0,h.unshift(b,j,l,m),e&&clearTimeout(e),e=setTimeout(c,200),(a.event.dispatch||a.event.handle).apply(this,h)}}function c(){f=null}function d(a,b){return k.settings.adjustOldDeltas&&"mousewheel"===a.type&&b%120===0}var e,f,g=["wheel","mousewheel","DOMMouseScroll","MozMousePixelScroll"],h="onwheel"in document||document.documentMode>=9?["wheel"]:["mousewheel","DomMouseScroll","MozMousePixelScroll"],i=Array.prototype.slice;if(a.event.fixHooks)for(var j=g.length;j;)a.event.fixHooks[g[--j]]=a.event.mouseHooks;var k=a.event.special.mousewheel={version:"3.1.12",setup:function(){if(this.addEventListener)for(var c=h.length;c;)this.addEventListener(h[--c],b,!1);else this.onmousewheel=b;a.data(this,"mousewheel-line-height",k.getLineHeight(this)),a.data(this,"mousewheel-page-height",k.getPageHeight(this))},teardown:function(){if(this.removeEventListener)for(var c=h.length;c;)this.removeEventListener(h[--c],b,!1);else this.onmousewheel=null;a.removeData(this,"mousewheel-line-height"),a.removeData(this,"mousewheel-page-height")},getLineHeight:function(b){var c=a(b),d=c["offsetParent"in a.fn?"offsetParent":"parent"]();return d.length||(d=a("body")),parseInt(d.css("fontSize"),10)||parseInt(c.css("fontSize"),10)||16},getPageHeight:function(b){return a(b).height()},settings:{adjustOldDeltas:!0,normalizeOffset:!0}};a.fn.extend({mousewheel:function(a){return a?this.bind("mousewheel",a):this.trigger("mousewheel")},unmousewheel:function(a){return this.unbind("mousewheel",a)}})});
32 core/jsplot/axis.waul.sdoc
A column vector of numeric data.
Stores a single axis of the data we want to render. It has a fixed capacity and represents a uniform sample if it overflows (i.e. it kicks data points out
evenly). Because multiple axes need to be coordinated, all random numbers used for sampling are parameters rather than generated here.

If you want focused nonuniform sampling, you can do it like this:

| var r = Math.random();
  r *= axis.focus(data_point, focus_center, focus_scale);
  // same for other axes
  axis.push(data_point, r);

Focusing biases the probability of accepting points so that data closer to the focal plane(s) is preferred.

caterwaul(':all')(function () {
  axis(capacity) = {data: new Float64Array(capacity), max: null, min: null, n: 0, c: capacity} /-caterwaul.merge/ axis_methods,
  axis /-caterwaul.merge/ static_methods,

  where[static_methods = capture[focus(x, c, s)     = Math.abs(x - c) / s],

        axis_methods   = capture[reset()            = this -se [this.n = 0, this.min = this.max = null],
                                 set(i, x)          = this -se [this.min = this.min == null ? x : this.min /-Math.min/ x,
                                                                this.max = this.max == null ? x : this.max /-Math.max/ x,
                                                                this.data[i] = x],

                                 offset()           = (this.max + this.min) / 2,
                                 range()            = this.max - this.min,
                                 at(x)              = this.min + x * this.range(),
                                 end()              = this.n /-Math.min/ this.c,
                                 p(i)               = this.data[i],

                                 push(x, r)         = this.n < this.c ? this.set(this.n++, x) : this /x /~uniform_push/ r,
                                 uniform_push(x, r) = this.set(r * this.n | 0, x) -when [r * this.n < this.c]]]})();
20 core/jsplot/matrix.waul.sdoc
Matrices.
Not a complete library; just enough stuff to get 3D linear transformation and projection. This library also generates compiled functions for fast axis-specific
transformation.

caterwaul(':all')(function () {
  matrix(x) = (x ? +x -seq : n[16] *[+((x>>2) == (x&3))] -seq) -se- it /-caterwaul.merge/ matrix_methods,
  matrix /-caterwaul.merge/ static_methods,

  where[static_methods = capture[translate(x, y, z)   = matrix() -se [it[3] = x, it[7] = y, it[11] = z],
                                 scale(x, y, z)       = matrix() -se [it[0] = x, it[5] = y, it[10] = z],
                                 rotate_x(t)          = matrix() -se [it[5] = it[10] = c, it[6] = -(it[9] = -s), where [s = t /!Math.sin, c = t /!Math.cos]],
                                 rotate_y(t)          = matrix() -se [it[0] = it[10] = c, it[2] = -(it[8] = -s), where [s = t /!Math.sin, c = t /!Math.cos]],
                                 prod(xs = arguments) = xs /[x0 /~dot/ x] -seq],

        compile        = caterwaul(':all'),
        matrix_methods = capture[dot             (b, a=this) = a *[n[4] /y[0][y0 + a[xi&12 | yi] * b[yi<<2 | xi&3]] -seq] /seq /!matrix,
                                 transform       (v, a=this) = v *[n[4] /s[0][s0 + a[xi<<2|s]*v[s]] -seq] -seq,
                                 transformer_form(d, a=this) = qs[given[x, y, z] in _a*x + _b*y + _c*z + _d]
                                                               /~replace/ {_a: '#{a[d<<2|0]}', _b: '#{a[d<<2|1]}', _c: '#{a[d<<2|2]}', _d: '#{a[d<<2|3]}'},
                                 transformer     (d, a=this) = this.transformer_form(d).toString() /!compile]]})();
11 core/jsplot/socket.waul.sdoc
Web socket interface.
Manages downloads from the server, tracks state, invokes a callback for each batch of new data.

caterwaul(':all')(function () {
  ni_ws(cmd, cb) = cancel_existing() -then- ws_connect(cmd, cb),

  where[existing_connection         = null,
        cancel_existing()           = existing_connection /~send/ '' -rescue- null -then- existing_connection.close() -when.existing_connection,
        ni_url(cmd)                 = "#{document.location.href.replace(/^http:/, 'ws:').replace(/#.*/, '')}ni/#{cmd /!encodeURIComponent}",
        ws_connect(cmd, f)          = existing_connection = new WebSocket(cmd /!ni_url, 'data') -se [it.onmessage = f /!message_wrapper],
        message_wrapper(f, k='')(e) = k -eq[lines.pop()] -then- f(lines) -where[m = k + e.data, lines = m.split(/\n/)]]})();
27 core/jsplot/render.waul.sdoc
Rendering support.
Rendering is treated like an asynchronous operation against the axis buffers. It ends up being re-entrant so we don't lock the browser thread, but those
details are all hidden inside a render request.

caterwaul(':all')(function () {
  render(axes, vm, ctx, w, h, cb) = start_rendering(axes, vm, ctx, w, h)

  -where[state                                = capture[ctx = null, w = 0, h = 0, i = 17, a = null, vm = null, cb = null, vt = null, id = null],
         start_rendering(axes, vm, ctx, w, h) = state /eq[{a: axes, vm: vm, ctx: ctx, w: w, h: h, i: 17, vt: n[4] *[vm.transformer(x)] -seq,
                                                           id: ctx.clearRect(0, 0, w, h) -re- ctx.getImageData(0, 0, w, h)}]
                                              -then- reset_alphas(ctx, w, h) -then- render_part /!requestAnimationFrame,

         reset_alphas = function (c, w, h) {for (var i = 0, id = state.id.data; i < w*h; ++i) id[i<<2|3] = 192; c.putImageData(state.id, 0, 0)},
         render_part  = function () {var t  = +new Date, cx = state.w>>1, cy = state.h>>1, ax=state.a[0], ay=state.a[1], xt=state.vt[0], yt=state.vt[1],
                                         id = state.id.data,                               az=state.a[2], aw=state.a[3], zt=state.vt[2], wt=state.vt[3],
                                         s  = state.w /-Math.min/ state.h >> 1, n = state.a[0].end(), use_hue = !!aw, width = state.w, height = state.h;
                                     for (var i = state.i; (i &= 0xff) && +new Date - t < 20; i += 17) for (; i < n; i += 256)
                                     { var w  = aw ? i /!aw.p : 0, x  = ax ? i /!ax.p : 0, y  = ay ? i /!ay.p : 0, z  = az ? i /!az.p : 0,
                                           wi = 1 / wt(x, y, z),   xp = wi * xt(x, y, z),  yp = wi * yt(x, y, z),  zp = wi * zt(x, y, z);
                                       if (zp > 0) {var r  = use_hue ? 0 |-Math.max| 1 |-Math.min| 1 - 3*(1/3 - Math.abs(.5  - w)) : 1,
                                                        g  = use_hue ? 0 |-Math.max| 1 |-Math.min| 3*(1/3 - Math.abs(1/3 - w)) : 1,
                                                        b  = use_hue ? 0 |-Math.max| 1 |-Math.min| 3*(1/3 - Math.abs(2/3 - w)) : 1,
                                                        sx = cx + xp/zp*s | 0, sy = cy - yp/zp*s | 0, pi = sy*width + sx << 2;
                                         if (sx >= 0 && sx < width && sy >= 0 && sy < height)
                                         { var a = 255^id[pi|3]>>>1; id[pi|0] += a*r|0; id[pi|1] += a*g|0; id[pi|2] += a*b|0; id[pi|3] = 255^a|128 }}}
                                     state.ctx.putImageData(state.id, 0, 0);
                                     if (state.i = i) render_part /!requestAnimationFrame; else state.id = null}]})();
56 core/jsplot/interface.waul.sdoc
Page driver.

$(caterwaul(':all')(function ($) {
  setup_event_handlers(),
  where[tau = Math.PI * 2, screen  = $('#screen'),    sc = screen[0]  /~getContext/ '2d',
        w   = $(window),   overlay = $('#overlay'),   oc = overlay[0] /~getContext/ '2d',
                           tr      = $('#transform'), lw = 0, mx = null, ms = false, lme = 0,
                           status  = $('#status'),    lh = 0, my = null, mc = false,
                           preview = $('#preview'),

        default_settings       = {ni: "n1000p'r a, sin(a/400), cos(a/300)'", r: [0, 0], s: [1, 1, 1], c: [0, 0, 0], f: [0, 0, 0], d: 1.4},
        settings(x)            = x ? document.location.hash /eq[x /!JSON.stringify /!encodeURIComponent]
                                   : document.location.hash.substr(1) /!decodeURIComponent /!JSON.parse -rescue- {} /-$.extend/ default_settings,
        set(k, v)              = settings() /-$.extend/ ({} -se- it[k] /eq.v) /!settings,

        drag(dx, dy, s, c)     = c ? 'd' /-set/ (settings().d * Math.exp(2 * dy / lh))
                               : s ? 'r' /-set/ [r[0] + dx / lw, r[1] - dy / lh]       -where [r = settings().r]
                               :     'c' /-set/ [c[0] + dx / lw, c[1] - dy / lh, c[2]] -where [c = settings().c],

        size_changed()         = (lw !== cw || lh !== ch) -se [lw = cw, lh = ch] -where [cw = w.width(), ch = w.height()],
        setup_event_handlers() = tr /~keydown/ given.e [e.which === 13 && !e.shiftKey ? visualize(tr.val()) -then- false : true]
                                      /~keyup/ given.e ['ni' /-set/ tr.val()]
                                        /~val/ settings().ni
                          -then- overlay     /~mousedown/ given.e [mx = e.pageX, my = e.pageY, ms = e.shiftKey, mc = e.ctrlKey]
                                            /~mousewheel/ given.e ['d' /-set/ (settings().d * Math.exp(e.deltaY * -0.01)) -then- update_screen()]
                          -then- $(document) /~mousemove/ given.e [drag(x - mx, y - my, ms, mc), mx = x, my = y, update_screen(), lme = +new Date,
                                                                   where [x = e.pageX, y = e.pageY], when [mx != null && +new Date - lme > 30]]
                                               /~mouseup/ given.e [mx = null, update_screen()]
                          -then- $('canvas').attr('unselectable', 'on').css('user-select', 'none').on('selectstart', false)
                          -then- given.e [overlay.add(screen) /~attr/ {width: lw, height: lh}
                                   -then- tr /~css/ {height: 0} /~css/ {width: lw, height: tr[0].scrollHeight - 2} -when- size_changed()] /-setInterval/ 50
                          -then- tr.val() /!visualize,

        data_state           = {axes: null, last_render: 0, preview: ''},
        reset_data_state()   = data_state = {axes: null, last_render: 0, preview: ''} -se- preview /~text/ '',

        data_was_revised(ls) = update_screen() /when[+new Date - data_state.last_render > 1000]
                      -then- preview /~text/ data_state.preview /when[data_state.preview.length < 65536 && (data_state.preview += ls.join("\n"))],

        visualize(cmd)     = reset_data_state() -then- ni_ws(cmd, handle_data)
                           -where [infer_n_axes(ls)   = ls /[0][x0 /-Math.max/ x.length] -seq |-Math.min| 4,
                                   update_n_axes(ls)  = data_state.axes /eq[n[ls /!infer_n_axes] *[1048576*4 /!axis] -seq] -unless- data_state.axes,
                                   handle_data(lines) = lines *![x.split(/\t/) /!populate_axes] -seq -then- data_was_revised(lines),
                                   populate_axes(l)   = l /!update_n_axes -then- data_state.axes *!a[a.push(+l[ai] || 0, r)] /seq -where [r = Math.random()]],

        object_matrix()    = matrix.prod(matrix.translate(c[0], c[1], c[2]), matrix.rotate_x(-r[1]*tau), matrix.rotate_y(-r[0]*tau),
                                         matrix.scale(s[0], s[1], s[2]),
                                         matrix.translate(f[0], f[1], f[2])) -where [st = settings(), c = st.c, r = st.r, s = st.s, f = st.f],

        normalize_matrix() = matrix.scale(1/sx, 1/sy, 1/sz) /~dot/ matrix.translate(-cx, -cy, -cz)
                      -where[as = data_state.axes, sx = as[0] && as[0].range() || 1, sy = as[1] && as[1].range() || 1, sz = as[2] && as[2].range() || 1,
                                                   cx = as[0] ? as[0].offset() : 0,  cy = as[1] ? as[1].offset() : 0,  cz = as[2] ? as[2].offset() : 0],

        camera_matrix()    = matrix.translate(0, 0, settings().d) /~dot/ object_matrix() /~dot/ normalize_matrix(),
        update_screen()    = render(data_state.axes, camera_matrix(), sc, screen.width(), screen.height()) -then- data_state.last_render /eq[+new Date]
                      -when- data_state.axes]}));
37 core/jsplot/css
body {margin:0; color:#eee; background:#111; font-size:10pt;
      font-family:monospace; overflow: hidden}

#screen, #overlay {position:absolute}

::-webkit-scrollbar {width:12px; height:4px}
::-webkit-scrollbar-track {background:rgba(255,255,255,0.1)}
::-webkit-scrollbar-thumb {background:rgba(255,255,255,0.5)}

*:focus, *:hover, .pinned, .active {opacity:1 !important}

#status {position:absolute; right:2px; bottom:2px; width:300px;
         opacity:0.2; text-align:right; z-index:9}

#transform {background:none; margin:0; color:#eee; position:absolute;
            left:0; top:0; border:none; outline:none; padding:1px 0;
            border-bottom:solid 1px transparent; font-family:monospace;
            z-index:9}

#transform:focus,
#transform:hover {background:rgba(17,17,17,0.75)}
#transform:focus {border-bottom:solid 1px #f60}

#preview {z-index:9; color:transparent; max-width:1px; overflow-x:auto;
          position:absolute; left:0; padding-right:12px; overflow-y:show;
          margin:0; background:rgba(17,17,17,0.75); cursor:default;
          font-family:monospace}

#preview:hover, #preview.pinned {color:#eee; max-width:100%}
#preview.pinned {border-right:solid 1px #f60}

#controls {position:absolute; width:12px; right:0; bottom:14pt; z-index:9;
           background:rgba(17,17,17,0.75)}
#controls canvas {display:block; height:48px; width:600px; margin-left:4px}

#controls:hover, #controls.pinned {width:600px}
#controls.pinned {border-left:solid 1px #f60}
22 core/jsplot/html
<!doctype html>
<html>
<head>
<title>ni/jsplot</title>
<style>%css</style>
<script>%js</script>
</head>
<body>
<textarea spellcheck='false' id='transform'></textarea>
<pre id='preview' class='autohide'></pre>
<canvas id='screen'></canvas>
<canvas id='overlay'></canvas>
<div id='controls' class='autohide'>
  <canvas id='xrange'></canvas>
  <canvas id='yrange'></canvas>
  <canvas id='zrange'></canvas>
</div>
<div id='status'>
  <label id='sizelabel'></label>
</div>
</body>
</html>
472 core/jsplot/jsplot.js.sdoc
JSPlot.
A plotting library that allows you to live-transform the data, and that
supports incremental rendering in not much space.

$(function () {
var tau = 2 * Math.PI;

var w  = $(window);
var c  = $('#screen');
var o  = $('#overlay');
var t  = $('#transform');
var st = $('#status');
var pr = $('#preview');

var cs = $('#controls');
var xr = $('#xrange');
var yr = $('#yrange');
var zr = $('#zrange');

var size_label = $('#sizelabel');

var ctx = c[0].getContext('2d');
var otx = o[0].getContext('2d');
var xrc = xr[0].getContext('2d');
var yrc = yr[0].getContext('2d');
var zrc = zr[0].getContext('2d');

$('canvas').attr('unselectable', 'on').css('user-select', 'none').on('selectstart', false);
$('.autohide').click(function () {$(this).toggleClass('pinned')});

xr.mousemove(function (e) {
  overlay_x = ((e.pageX - $(this).offset().left) / $(this).width() - 0.5) / settings.s[0] - settings.f[0];
  render_overlay();
}).mouseout(function () {overlay_x = null; render_overlay()});

yr.mousemove(function (e) {
  overlay_y = ((e.pageX - $(this).offset().left) / $(this).width() - 0.5) / settings.s[1] - settings.f[1];
  render_overlay();
}).mouseout(function () {overlay_y = null; render_overlay()});

zr.mousemove(function (e) {
  overlay_z = ((e.pageX - $(this).offset().left) / $(this).width() - 0.5) / settings.s[2] - settings.f[2];
  render_overlay();
}).mouseout(function () {overlay_z = null; render_overlay()});

var settings;
var save_settings = function () {
  document.location.hash = encodeURIComponent(JSON.stringify(settings));
};

var bookmark = decodeURIComponent(document.location.hash.substr(1));
try {
  settings = JSON.parse(bookmark);
} catch (e) {
  settings = {ni: bookmark || '',
              r:  [0, 0],
              s:  [1, 1, 1],
              c:  [0, 0, 0],
              f:  [0, 0, 0],
              d:  1.4};
  save_settings();
}
t.val(settings.ni);

var wwl, whl;
var resize_stuff = function () {
  var ww = w.width() - 2, wh = w.height() - 2;
  var th = t.height() + 2;
  pr.css({top: th + 1, height: wh - th - 2});
  t .css({height: 0});
  t .css({width: ww, height: t[0].scrollHeight - 2});
  if (ww !== wwl || wh !== whl) {
    c.attr({width: wwl = ww, height: whl = wh});
    o.attr({width: wwl = ww, height: whl = wh});
    xr.attr({width: xr.width(), height: xr.height()});
    yr.attr({width: yr.width(), height: yr.height()});
    zr.attr({width: zr.width(), height: zr.height()});
    anything_new = true;
  }
};

View coordinates.
Stored as a transformation matrix + a base viewport distance.

var matrix    = function () {return [1, 0, 0, 0,
                                     0, 1, 0, 0,
                                     0, 0, 1, 0,
                                     0, 0, 0, 1]};

var translate = function (x, y, z) {var r = matrix(); r[3] = x; r[7] = y; r[11] = z; return r};
var scale     = function (x, y, z) {var r = matrix(); r[0] = x; r[5] = y; r[10] = z; return r};

var rotate_x = function (t) {
  var r = matrix(), s = Math.sin(t), c = Math.cos(t);
  r[5] = r[10] = c;
  r[6] = s;
  r[9] = -s;
  return r;
};

var rotate_y = function (t) {
  var r = matrix(), s = Math.sin(t), c = Math.cos(t);
  r[0] = r[10] = c;
  r[2] = s;
  r[8] = -s;
  return r;
};

var mm = function (a, b) {
  var r = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  for (var i = 0; i < 4; ++i)
    for (var j = 0; j < 4; ++j)
      for (var k = 0; k < 4; ++k)
        r[i*4 + j] += a[i*4 + k] * b[k*4 + j];
  return r;
};

var mx = null, my = null, mshift = false, mctrl = false;

var view_matrix = function () {
  return mm(mm(translate(settings.c[0], settings.c[1], settings.c[2]),
               mm(rotate_x(-settings.r[1] * tau),
                  rotate_y(-settings.r[0] * tau))),
            mm(scale(settings.s[0], settings.s[1], settings.s[2]),
               translate(settings.f[0], settings.f[1], settings.f[2])));
};

o.mousedown(function (e) {
  mx     = e.pageX;
  my     = e.pageY;
  mshift = e.shiftKey;
  mctrl  = e.ctrlKey || e.metaKey;
});

o.mousewheel(function (e) {
  settings.d *= Math.exp(e.deltaY * -0.01);
  partial = 0;
  anything_new = true;
  save_settings();
});

xr.mousewheel(function (e) {
  var d = (e.pageX - $(this).offset().left) / $(this).width() - 0.5;

  settings.f[0] -= d / settings.s[0];
  settings.s[0] *= Math.exp(e.deltaY * 0.01);
  settings.f[0] += d / settings.s[0];

  partial = 0;
  anything_new = true;
  render_overlay();
  save_settings();
});

yr.mousewheel(function (e) {
  var d = (e.pageX - $(this).offset().left) / $(this).width() - 0.5;

  settings.f[1] -= d / settings.s[1];
  settings.s[1] *= Math.exp(e.deltaY * 0.01);
  settings.f[1] += d / settings.s[1];

  partial = 0;
  anything_new = true;
  render_overlay();
  save_settings();
});

zr.mousewheel(function (e) {
  var d = (e.pageX - $(this).offset().left) / $(this).width() - 0.5;

  settings.f[2] -= d / settings.s[2];
  settings.s[2] *= Math.exp(e.deltaY * 0.01);
  settings.f[2] += d / settings.s[2];

  partial = 0;
  anything_new = true;
  render_overlay();
  save_settings();
});

var last_mousemove = 0;
$(document).mousemove(function (e) {
  if (mx === null) return;
  if (+new Date - last_mousemove < 30) return;
  last_mousemove = +new Date;

  var hh = wwl / 2;
  var vh = whl / 2;

  if (mctrl)
    settings.d *= Math.exp(2 * (e.pageY - my) / whl);
  else if (mshift)
    settings.r = [settings.r[0] + (e.pageX - mx) / whl,
                  settings.r[1] - (e.pageY - my) / wwl];
  else
    settings.c = [settings.c[0] + (e.pageX - mx) / wwl,
                  settings.c[1] - (e.pageY - my) / whl,
                  settings.c[2]];

  mx = e.pageX;
  my = e.pageY;
  partial = 0;
  anything_new = true;
  save_settings();
});

$(document).mouseup(function (e) {
  if (mx === null) return;
  mx = my = null;
  partial = 0;
  anything_new = true;
});

AJAX data requests.
Data comes down in small pieces and is rendered as it arrives. We do this for
two reasons: first, to decrease view latency; and second, to run in constant
space. (The client does store some data, but there's an upper bound on how
much.)

t.keydown(function (e) {if (e.which === 13 && !e.shiftKey) {refresh(); return false}});
t.keyup  (function (e) {settings.ni = t.val(); save_settings()});

var anything_new = false;
var ws = null;
var refresh = function () {
  var ws_url = document.location.href.replace(/^http:/, 'ws:').replace(/#.*/, '') + 'ni/';
  if (ws != null) {
    try {ws.send('')} catch (e) {}
    ws.close();
  }
  ws = new WebSocket(ws_url + encodeURIComponent(t.val()), 'data');
  incoming = '';
  n_points = 0;
  min_x = 0, max_x = 0, min_y = 0, max_y = 0,
  min_z = 0, max_z = 0, min_w = 0, max_w = 0;

  var continuation = '';
  var size = 0;
  var incoming = '';
  var last_data_time = +new Date - 9000;
  var dim_status_timeout = null;

  ws.onmessage = function (e) {
    var m = continuation + e.data;
    if (incoming.length < 65536)
      pr.text(incoming += e.data.substr(0, 65536));

    if (+new Date - last_data_time > 10000) {
      anything_new = true;
      partial = 0;
      last_data_time = +new Date;
    }

    var lines = m.split(/\n/);
    continuation = lines.pop();
    for (var i = 0; i < lines.length; ++i)
      collect_point(lines[i].split(/\t/));

    size_label.text(n_points + " : " + ((size += e.data.length) >>> 10) + "K");
    st.addClass('active');
    dim_status_timeout == null || clearTimeout(dim_status_timeout);
    dim_status_timeout = setTimeout(function () {
      st.removeClass('active');
    }, 500);
  };

  ws.onclose = function () {
    anything_new = true;
    partial = 0;
  };
};

Rendering logic.
Super simple: [x, y, z] if present. We sample points into a cache and render
everything in timed batches.

var n_points = 0;
var partial  = 0;
var parsed   = new Float64Array(4 * 1048576);

var min_x = 0, max_x = 0, min_y = 0, max_y = 0,
    min_z = 0, max_z = 0, min_w = 0, max_w = 0;

var collect_point = function (p) {
  if (isNaN(p[0] = +p[0])) p[0] = min_x;
  if (isNaN(p[1] = +p[1])) p[1] = min_y;
  if (isNaN(p[2] = +p[2])) p[2] = min_z;
  if (isNaN(p[3] = +p[3])) p[3] = min_w;

  if (!n_points) {
    min_x = max_x = +p[0];
    min_y = max_y = +p[1];
    min_z = max_z = +p[2];
    min_w = max_w = +p[3];
  }

  min_x = Math.min(min_x, p[0]);
  max_x = Math.max(max_x, p[0]);
  min_y = Math.min(min_y, p[1]);
  max_y = Math.max(max_y, p[1]);
  min_z = Math.min(min_z, p[2]);
  max_z = Math.max(max_z, p[2]);
  min_w = Math.min(min_w, p[3]);
  max_w = Math.max(max_w, p[3]);

  if (++n_points * 4 + 4 < parsed.length) {
    parsed[n_points * 4 + 0] = p[0];
    parsed[n_points * 4 + 1] = p[1];
    parsed[n_points * 4 + 2] = p[2];
    parsed[n_points * 4 + 3] = p[3];
  } else {
    var i = Math.random() * n_points | 0;
    if (i * 4 + 4 < parsed.length) {
      parsed[i * 4 + 0] = p[0];
      parsed[i * 4 + 1] = p[1];
      parsed[i * 4 + 2] = p[2];
      parsed[i * 4 + 3] = p[3];
    }
  }
};

var project = function (x, y, z, cm) {
  var wp = 1 / (cm[12]*x + cm[13]*y + cm[14]*z + cm[15]);
  var xp = wp * (cm[0]*x + cm[1] *y + cm[2] *z + cm[3]);
  var yp = wp * (cm[4]*x + cm[5] *y + cm[6] *z + cm[7]);
  var zp = wp * (cm[8]*x + cm[9] *y + cm[10]*z + cm[11]);
  if (zp > 0) {
    zp = 1 / zp;
    return [xp*zp, yp*zp, zp];
  }
};

var overlay_x = null;
var overlay_y = null;
var overlay_z = null;

var render_scale = function () {return Math.min(whl, wwl)};

var camera_matrix = function () {
  var sx = 1 / (max_x - min_x || 1);
  var sy = 1 / (max_y - min_y || 1);
  var sz = 1 / (max_z - min_z || 1);

  var cx = (max_x + min_x) / 2;
  var cy = (max_y + min_y) / 2;
  var cz = (max_z + min_z) / 2;

  return mm(translate(0, 0, settings.d),
            mm(view_matrix(), mm(scale(sx, sy, sz),
                                 translate(-cx, -cy, -cz))));
};

var overlay_line = function (scale, p1, p2, cm) {
  var pp1 = project(p1[0], p1[1], p1[2], cm);
  var pp2 = project(p2[0], p2[1], p2[2], cm);
  if (pp1 && pp2) {
    otx.beginPath();
    otx.moveTo(wwl/2 + scale*pp1[0], whl/2 - scale*pp1[1]);
    otx.lineTo(wwl/2 + scale*pp2[0], whl/2 - scale*pp2[1]);
    otx.stroke();
  }
};

var render_overlay = function () {
  otx.clearRect(0, 0, wwl, whl);
  otx.fillStyle = otx.strokeStyle = 'rgba(255, 96, 0, 0.5)';

  var vsize       = render_scale();
  var grid_points = 100;
  var cm          = mm(translate(0, 0, settings.d), view_matrix());

  for (var i = 0; i <= grid_points; ++i) {
    var a = -0.5;
    var b =  0.5;
    var c = -0.5 + i / grid_points;

    if (overlay_x != null) {
      overlay_line(vsize, [overlay_x, a, c], [overlay_x, b, c], cm);
      overlay_line(vsize, [overlay_x, c, a], [overlay_x, c, b], cm);
    }

    if (overlay_y != null) {
      overlay_line(vsize, [a, overlay_y, c], [b, overlay_y, c], cm);
      overlay_line(vsize, [c, overlay_y, a], [c, overlay_y, b], cm);
    }

    if (overlay_z != null) {
      overlay_line(vsize, [a, c, overlay_z], [b, c, overlay_z], cm);
      overlay_line(vsize, [c, a, overlay_z], [c, b, overlay_z], cm);
    }
  }
};

var render = function () {
  requestAnimationFrame(render);
  resize_stuff();

  if (!anything_new) return;

  var t = +new Date;
  if (!partial) {
    ctx.clearRect(0, 0, wwl, whl);
    xrc.clearRect(0, 0, xr.width(), xr.height());
    yrc.clearRect(0, 0, yr.width(), yr.height());
    zrc.clearRect(0, 0, zr.width(), zr.height());
  }
  ctx.fillStyle = 'rgba(255, 255, 255, 0.5)';
  xrc.fillStyle = 'rgba(255, 255, 255, 0.1)';
  yrc.fillStyle = 'rgba(255, 255, 255, 0.1)';
  zrc.fillStyle = 'rgba(255, 255, 255, 0.1)';

  var sx = 1 / (max_x - min_x || 1);
  var sy = 1 / (max_y - min_y || 1);
  var sz = 1 / (max_z - min_z || 1);

  var osx = settings.s[0];
  var osy = settings.s[1];
  var osz = settings.s[2];

  var ocx = settings.f[0];
  var ocy = settings.f[1];
  var ocz = settings.f[2];

  var cm = camera_matrix();
  var m00 = cm[0],  m01 = cm[1],  m02 = cm[2],  m03 = cm[3],
      m10 = cm[4],  m11 = cm[5],  m12 = cm[6],  m13 = cm[7],
      m20 = cm[8],  m21 = cm[9],  m22 = cm[10], m23 = cm[11],
      m30 = cm[12], m31 = cm[13], m32 = cm[14], m33 = cm[15];

  var step = 17;
  var end  = 255 * step & 0xff;

  var vsize = render_scale();

  var range_w = xr.width();
  var range_h = xr.height();

  var n = Math.min(parsed.length / 4, n_points);
  for (var i = partial; (i &= 0xff) != end && +new Date - t < 10; i += step) {
    for (; i < n; i += 256) {
      var x = parsed[i * 4 + 0];
      var y = parsed[i * 4 + 1];
      var z = parsed[i * 4 + 2];

      xrc.fillRect((((x - min_x) * sx - 0.5 + ocx) * osx + 0.5) * range_w, Math.random() * range_h, 1, 1);
      yrc.fillRect((((y - min_y) * sy - 0.5 + ocy) * osy + 0.5) * range_w, Math.random() * range_h, 1, 1);
      zrc.fillRect((((z - min_z) * sz - 0.5 + ocz) * osz + 0.5) * range_w, Math.random() * range_h, 1, 1);

      var wp = 1 / (m30*x + m31*y + m32*z + m33);
      var xp = wp * (m00*x + m01*y + m02*z + m03);
      var yp = wp * (m10*x + m11*y + m12*z + m13);
      var zp = wp * (m20*x + m21*y + m22*z + m23);

      if (zp > 0) {
        zp = 1 / zp;
        var ps = Math.min(Math.max(zp, 0.5), 1);
        ctx.fillRect(wwl/2 + xp * zp * vsize,
                     whl/2 - yp * zp * vsize, ps, ps);
      }
    }
  }

  partial = i === end ? 0 : i;
  anything_new = i !== end;
};

requestAnimationFrame(render);

refresh();
t.focus();

});
78 core/jsplot/jsplot.pl.sdoc
JSPlot interop.
JSPlot is served over HTTP as a portable web interface. It requests data via
AJAX, and may request the same data multiple times to save browser memory. The
JSPlot driver buffers the data to disk to make it repeatable.

use constant jsplot_gen => gen $self{'core/jsplot/html'};

use constant jsplot_html =>
  jsplot_gen->(css => $self{'core/jsplot/css'},
               js  => join '', @self{qw| core/caterwaul/caterwaul.min.js
                                         core/caterwaul/caterwaul.std.min.js
                                         core/jsplot/jquery.min.js
                                         core/jsplot/jquery.mousewheel.min.js
                                         core/jsplot/axis.waul
                                         core/jsplot/matrix.waul
                                         core/jsplot/socket.waul
                                         core/jsplot/render.waul
                                         core/jsplot/interface.waul |});

JSPlot data streaming.
This is the websocket connection that ni uses to stream data to the client. Any
data we receive from the client indicates that the client is canceling the
websocket request, so we need to break the pipe and kill off subprocesses.

sub jsplot_log($@) {printf STDERR "ni js[$$]: $_[0]", @_[1..$#_]}

sub jsplot_stream($$@) {
  local $_;
  my ($reply, $req, @ni_args) = @_;
  my ($ops) = cli @ni_args;
  unless (defined $ops) {
    my (undef, @rest) = parse pcli_debug '', @ni_args;
    die "ni: jsplot failed to parse starting at @rest";
  }

  jsplot_log "running %s\n", json_encode $ops;

  safewrite $reply, ws_header($req);
  my $ni_pipe = sni @$ops, http_websocket_encode_batch_op 65536;

  my $incoming;
  my $rmask   = '';
  vec($rmask, fileno $reply, 1) = 1;

  my $n;
  my $total = 0;
  while ($n = saferead $ni_pipe, $_, 65536) {
    jsplot_log "% 8dKiB\r", ($total += $n) >> 10;
    if (select my $rout = $rmask, undef, undef, 0) {
      saferead $reply, $incoming, 8192;
      if ($incoming =~ /^\x81\x80/) {
        jsplot_log "SIGTERM to worker\n";
        $ni_pipe->kill('TERM');
        jsplot_log "awaiting worker exit\n";
        jsplot_log "worker exited with %d\n", $ni_pipe->await;
        return;
      }
    }
    safewrite $reply, $_;
  }
  jsplot_log "done transferring data\n";
  $ni_pipe->await;
  jsplot_log "worker exited with %d\n";
}

sub jsplot_server {
  my ($port) = @_;
  load 'core/http/ws.pm';
  http $port, sub {
    my ($url, $req, $reply) = @_;
    return print "http://localhost:$port/\n"             unless defined $reply;
    return http_reply $reply, 200, jsplot_html           if $url eq '/';
    return jsplot_stream($reply, $req, shell_unquote $1) if $url =~ /^\/ni\/(.*)/;
    return http_reply $reply, 404, $url;
  };
}

defclispecial '--js', q{jsplot_server $_[0] || 8090};
1 core/docker/lib
docker.pl.sdoc
26 core/docker/docker.pl.sdoc
Pipeline dockerization.
Creates a transient container to execute a part of your pipeline. The image you
specify needs to have Perl installed, but that's about it.

our @docker_alt;
defshort '/C', paltr @docker_alt;

sub defdockeralt($) {unshift @docker_alt, $_[0]}

defoperator docker_run_image => q{
  my ($image, @f) = @_;
  my ($stdin, @args) = sni_exec_list @f;
  my $fh = siproc {exec qw|docker run --rm -i|, $image, @args};
  safewrite $fh, $stdin;
  sforward \*STDIN, $fh;
  close $fh;
  $fh->await;
};

Prebuilt image case.
This is what happens by default, and looks like `ni Cubuntu[g]`.

use constant docker_image_name => prx '[^][]+';

defdockeralt pmap q{docker_run_image_op $$_[0], @{$$_[1]}},
                  pseq pc docker_image_name, pqfn '';
1 core/hadoop/lib
hadoop.pl.sdoc
1 core/hadoop/hadoop.pl.sdoc
Hadoop contexts.
1 core/pyspark/lib
pyspark.pl.sdoc
61 core/pyspark/pyspark.pl.sdoc
Pyspark interop.
We need to define a context for CLI arguments so we can convert ni pipelines
into pyspark code. This ends up being fairly straightforward because Spark
provides so many high-level operators.

There are two things going on here. First, we define the codegen for Spark
jobs; this is fairly configuration-independent since the API is stable. Second,
we define a configuration system that lets the user specify the Spark execution
profile. This governs everything from `spark-submit` CLI options to
SparkContext init.

Pyspark operators.
These exist in their own parsing context, which we hook in below by using
contexts->{pyspark}{...}. Rather than compiling directly to Python code, we
generate a series of gens, each of which refers to a '%v' quantity that
signifies the value being transformed.

sub pyspark_compile {my $v = shift; $v = $_->(v => $v) for @_; $v}
sub pyspark_lambda($) {$_[0]}

defcontext 'pyspark';

use constant pyspark_fn => pmap q{pyspark_lambda $_}, pycode;

our $pyspark_rdd = pmap q{pyspark_compile 'sc', @$_}, pqfn 'pyspark';

our @pyspark_row_alt = (
  (pmap q{gen "%v.sample(False, $_)"}, integer),
  (pmap q{gen "%v.takeSample(False, $_)"}, prx '\.(\d+)'),
  (pmap q{gen "%v.filter($_)"}, pyspark_fn));

deflong 'pyspark/stream/n',
  pmap q{gen "sc.parallelize(range($_))"}, pn 1, prx 'n', number;

deflong 'pyspark/stream/pipe',
  pmap q{gen "%v.pipe(" . pyquote($_) . ")"}, prx '\$=([^]]+)';

defshort 'pyspark/p', pmap q{gen "%v.map(lambda x: $_)"}, pyspark_fn;
defshort 'pyspark/r', paltr @pyspark_row_alt;
defshort 'pyspark/G', pk gen "%v.distinct()";
defshort 'pyspark/g', pk gen "%v.sortByKey()";

defshort 'pyspark/+', pmap q{gen "%v.union($_)"}, $pyspark_rdd;
defshort 'pyspark/*', pmap q{gen "%v.intersect($_)"}, $pyspark_rdd;

Configuration management.
A profile contains the code required to initialize the SparkContext and any
other variables relevant to the process. Each is referenced by a single
character and stored in the %spark_profiles table.

our %spark_profiles = (
  L => pk gen pydent q{from pyspark import SparkContext
                       sc = SparkContext("local", "%name")
                       %body});

sub defsparkprofile($$) {$spark_profiles{$_[0]} = $_[1]}

defoperator pyspark => q{print STDERR "TODO: pyspark\n"};

defshort '/P', pmap q{pyspark_op @$_},
               pseq pdspr(%spark_profiles), $pyspark_rdd;
11 doc/lib
col.md
examples.md
extend.md
libraries.md
lisp.md
options.md
perl.md
row.md
sql.md
stream.md
tutorial.md
176 doc/col.md
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
$ ni mult-table fA-D    # first four columns
1	2	3	4
2	4	6	8
3	6	9	12
4	8	12	16
5	10	15	20
6	12	18	24
7	14	21	28
8	16	24	32
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
$ ni mult-table x       # even easier (see below)
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
6	3	9	12	15	18	21	24
8	4	12	16	20	24	28	32
10	5	15	20	25	30	35	40
12	6	18	24	30	36	42	48
14	7	21	28	35	42	49	56
16	8	24	32	40	48	56	64
```

## Exchanging
You can swap columns into leading positions using the `x` operator:

```bash
$ ni mult-table xC r2   # swap third column into first position
3	2	1	4	5	6	7	8
6	4	2	8	10	12	14	16
$ ni mult-table xGHr2   # swap seventh, eighth columns into first two
7	8	3	4	5	6	1	2
14	16	6	8	10	12	2	4
$ ni mult-table xr2     # swap first two columns
2	1	3	4	5	6	7	8
4	2	6	8	10	12	14	16
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
241 doc/examples.md
# Examples of ni misuse
All of these use `ni --js` (see [visual.md](visual.md) for a brief overview).
If you're working through these on the command line, you can use `ni
--explain` to help figure out what's going on:

```sh
$ ni --explain //ni FWpF_ plc gc
["meta_image"]
["split_regex","(?^:[^\\w\\n]+)"]
["perl_mapper","F_"]
["perl_mapper","lc"]
["row_sort","-t","\t"]
["count"]
```

**NOTE:** Some of the screenshots here are from older versions of ni, so some
details may have changed since.

## Simple 2D letter/letter co-occurrence matrix
![img](http://spencertipping.com/ni-example-letter-cooccurrence.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fdict%2Fwords%20plc%20pr%2F%5Ba-z%5D%2Fg%20pcart%5BF_%5D%2C%5BF_%5D%20p'r%20map%20ord%2C%20F_'%20%2CjAB%2C%22%2C%22vm%22%3A%5B1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.1348179443582629%7D
```

Equivalent ni command:

```sh
$ ni /usr/share/dict/words plc pr/[a-z]/g pcart[F_],[F_] p'r map ord, F_'
```

### How it works
- `/usr/share/dict/words`: cat the file; we get one word per line
- `plc`: short for `p'lc $_'`: lowercase each line
- `pr/[a-z]/g`: short for `p'r /[a-z]/g'`:
  - `/[a-z]/g`: in list context, return every occurrence of a lowercase letter
  - `r(@list)`: join with tabs and print an output line
- `pcart[F_],[F_]`: short for `p'cart [F_], [F_]'`:
  - `F_`: return a list of tab-delimited fields (the lowercase letters from
    `pr/[a-z]/g`)
  - `cart [@xs], [@ys], ...`: Cartesian product of N arrays: in this case,
    returns `[$xi, $yi]` pairs, which represent every combination of lowercase
    letters within this word
  - `cart` returns array references, which ni automatically joins with tabs to
    convert to output rows.

At this point we have each pair of co-occurring letters on a single line,
tab-delimited. The last step is to convert to ASCII:

- `p'r map ord, F_`: convert each tab-delimited field to its ASCII value:
  - `F_`: the list of fields
  - `map ord, @list`: short for `map ord($_), @list`: convert each list element
    to ASCII
  - `r @list`: tab-join and write output line

This gives us a long stream of integer pairs. If we plot them now, we'll get a
bunch of dots on the screen:

![img](http://spencertipping.com/ni-example-letter-cooccurrence-dots.png)

If, however, we move each dot by a random vector chosen from a 0.9x0.9
rectangle, we'll end up with stochastically-shaded squares with visible
boundaries. This is a common thing to do when dot plotting, so ni provides the
"jitter" cell operator.

- `,`: enter cell-transform context (essentially a new namespace for letters)
  - `jAB,`: jitter cells in columns A and B uniformly;
    - `,`: ...by 0.9. ni provides this shorthand because it's common to do
      this. (The alternative would be `,jAB.9`, but that's extra typing with a
      lot of right-hand travel.)

## Simple 3D sine wave
![img](http://spencertipping.com/ni-example-simple-3dsine.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22nE3p'r%20a%2C%20%24_%20for%200..999'%20r'%2F0(%5C%5Ct%7C%24)%2F'%20p'r%20a%2C%20sin(a%2F100)*sin(b%2F100)%2C%20b'%20%2B%5Bid%3A0%2C4%2C0%20id%3A0%2C-4%2C0%20FC%5D%22%2C%22vm%22%3A%5B0.9900140741662761%2C0%2C0.14096855306306652%2C0.038856304985337244%2C-0.043561678568934545%2C0.9510565162951593%2C0.30593117358776106%2C0.03263707571801566%2C-0.13406906098332957%2C-0.3090169943749491%2C0.9415593364597573%2C0%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A0.8750031755957807%7D
```

Equivalent ni command:

```sh
$ ni nE3p'r a, $_ for 0..999' r'/0(\t|$)/' p'r a, sin(a/100)*sin(b/100), b'
```

### How it works
- `nE3`: generate integers from 1 to 1000, inclusive. `E3` is shorthand for
  `1e3`, which is 1000.
- `p'r a, $_ for 0..999'`: for each row, emit 1000 rows, one for each value in
  the second column. This emits the plane of input points for which we'll
  evaluate the product of two sines.
- `r'/0(\t|$)/'`: punches holes in the plane to form a grid. I'll explain this
  below and show what it looks like if we omit it.
- `p'r a, sin(a/100)*sin(b/100), b'`: the plane coordinates are `a` and `b`, so
  for each plane point we emit a 3D point at `<a, f(a, b), b>`.
- `@[id:0,4,0 id:0,-4,0 FC]`: two extra data points to expand the min/max along
  the Y axis. This results in flatter output.

### `r'/0(\t|$)/'` for the grid
If we preview just `nE3p'r a, $_ for 0..999'`, we'll see a uniform plane:

![img](http://spencertipping.com/ni-example-simple-3dsine-plane.png)

We'd have a grid if every point were divisible by 10 along either the X or the
Y axis. We could use Perl like this: `rp'(a%10==0) || (b%10==0)'`, but an
equivalent assertion is that either of the two numbers ends in a zero. The
regex `/0(\t|$)/` detects this case: the first number is followed by a tab, the
second number by the end of the line.

![img](http://spencertipping.com/ni-example-simple-3dsine-grid.png)

### View scaling
This uses a sub-ni instance to append some data to the stream. We just need two
more data points whose Y coordinates expand the range to [-4, 4] so the
plotting interface scales the sine wave down vertically.

- `+[...]`: append a new stream:
  - `id:0,4,0`: append the literal text `0,4,0`
  - `id:0,-4,0`: append the literal text `0,-4,0`
  - `FC`: fieldsplit on commas: this turns commas into tabs so the two points
    occupy three columns each.

## Simple ASCII co-occurrence of ni source code
![img](http://spencertipping.com/ni-example-ascii-cooccurrence.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2F%2Fni%20psplit%2F%2F%20pord%20p'r%20pl%203'%20%2CjABC.5%22%2C%22vm%22%3A%5B0.3052176877315164%2C0%2C-0.9522826067380442%2C0.025659824046920767%2C0.15198698116928847%2C0.9871813119337595%2C0.04871360101460337%2C-0.00521898518107074%2C0.9400755930513635%2C-0.15960281128089038%2C0.30130519740018513%2C-0.060644007110305605%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.4140702339178333%7D
```

Equivalent ni command:

```sh
$ ni //ni psplit// pord p'r pl 3'
```

### How it works
- `//ni`: append ni's source code verbatim
- `psplit//`: a compact form of `p'split //'`, which will return each character
  on a separate line.
- `pord`: a compact form of `p'ord $_'`, which will transform each line into
  its ASCII value.
- `p'r pl 3'`: non-destructively read the next three lines and place them on a
  single row. `pl($n)` peeks `n` lines ahead, returning them as an array.

At this point we have triples of ASCII values. The web UI provides some
shading to handle point collisions, but it can only offer so much dynamic
range; typically if you're relying on shading to tell you something, you'll
want to jitter each data point a little. (See the 2D letter/letter
co-occurrence from `/usr/share/dict/words` for an example.)

- `,`: cell transformation context
  - `jABC.5`: jitter columns A, B, and C by a random value whose range is
    centered at 0 and is 0.5 across. ASCII values are integers, so jittering
    each one by 0.5 preserves its identity while adding some spatial resolution
    and better shading to the plot.

## Co-occurrence of manpage words in quasi-donut form
![img](http://spencertipping.com/ni-example-cooccurrence-quasidonut.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fman%2Fman1%20%5C%5C%3CFWp'r%20F_(%24_-2..%24_)%20for%202..FM'%20%2ChABzC%20p'r%20prec((a%20%26%200xffff)%20%2B%200xffff%2C%20int((b%20%26%200xffff)%20%2F%200xffff*270))%2C%20c'%20fACB%22%2C%22vm%22%3A%5B-0.9841092569237042%2C0%2C-0.17756398969608223%2C0.07311827956989259%2C0.03298935921795422%2C0.9825897456859218%2C-0.18283624873452747%2C0.4633668730031738%2C0.17447255547845722%2C-0.185788567121162%2C-0.9669756644878278%2C-0.06165651783795804%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A0.6482182956356948%7D
```

Equivalent ni command:

```sh
$ ni /usr/share/man/man1 \<FWp'r F_($_-2..$_) for 2..FM' \
  ,hABzC p'r prec((a & 0xffff) + 0xffff, int((b & 0xffff) / 0xffff*270)), c' \
  fACB
```

### How it works
- `/usr/share/man/man1` produces a list of filenames (all manpages in `man1`)
- `\<` cats each file in a stream of filenames (all manpage text in `man1`)
- `FW` splits on non-word characters
- `p'r F_($_-2..$_) for 2..FM'` is a 3-wide sliding window of words per line:
  - `2..FM` generates the index of the last word in each window
  - `$_-2..$_` generates the indexes of all words in the current window
  - `r F_(@indexes)` outputs a row of fields at numerically-specified
    `@indexes`

A preview of the output at this point:

```sh
$ ni /usr/share/man/man1 \<FWp'r F_($_-2..$_) for 2..FM' r10
        DO      NOT
DO      NOT     MODIFY
NOT     MODIFY  THIS
MODIFY  THIS    FILE
THIS    FILE    It
FILE    It      was
It      was     generated
was     generated       by
generated       by      help2man
by      help2man        1
```

Now we use cell operators to transform things into numbers.

- `,`: the cell operator prefix, which continues until the next CLI argument:
  - `hAB`: 32-bit murmurhash each cell in columns A and B (unbiased)
  - `zC`: assign successive integers to distinct values in column C (biased)

At this point we have a cube of word cooccurrence:

![img](http://spencertipping.com/ni-example-cooccurrence-hhz-cube.png)

```
http://localhost:8090/#%7B%22ni%22%3A%22%2Fusr%2Fshare%2Fman%2Fman1%20%5C%5C%3CFWp'r%20F_(%24_-2..%24_)%20for%202..FM'%20%2ChABzC%22%2C%22vm%22%3A%5B-0.1455274825751139%2C0%2C-0.9893542094796254%2C-0.04019689987431912%2C0.4626508778259456%2C0.8839247529105698%2C-0.06805289441946762%2C-0.01019337018835886%2C0.874514675155316%2C-0.46762915990349385%2C-0.12863534407689978%2C-0.08768024724094528%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.6594267918484475%7D
```

I haven't implemented axes and range display yet, but `A` and `B` take on the
full range of 32-bit integer values and `C` contains positive integers up to
the number of distinct words we have -- we can calculate it easily, in this
case by just taking the maximum:

```sh
$ ni /usr/share/man/man1 \<FWpF_ ,z Or1
84480
```

Alternatively, we can reverse-sort within the UI and use the preview window:

![img](http://spencertipping.com/ni-example-cooccurrence-hhz-preview.png)

ni provides a quasidonut constructor library in the form of two functions,
`prec(rho, theta) = (x, y)` and `rpol(x, y) = (rho, theta)`. These convert
between rectangular and polar coordinates. In this case we want `prec`, and
here's what we're doing with it:

```pl
(a & 0xffff)            # low 16 bits of the hash (just to truncate; could be pretty much anything)
(a & 0xffff) + 0xffff   # push to the upper half of the range [0, 0x1ffff]: this increases the radius and creates a ring

(b & 0xffff) / 0xffff   # truncate b to the range [0, 1]
... / 0xffff * 270      # now extend to the range [0, 270]: 3/4 of a donut

r prec(rho, theta), c   # output x and y as points on circles, z is preserved
```

From there we just flip into the viewport coordinate system, where Z points
away from the camera.
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
69 doc/options.md
# Complete ni operator listing
Operator | Example      | Description
---------|--------------|------------
`+`      | `+p'foo'`    | Appends a data source evaluated with no input
`^`      | `^file`      | Prepends a data source
`%`      | `%[\>f K]`   | Duplicate stream, interleaving fork output
`=`      | `=\>f`       | Duplicate stream, ignoring fork output
`\>`     | `\>file`     | Sinks stream into resource, emits resource name
`\<`     | `\<`         | Opposite of `\>`
`-`      |              |
`,`      | `,jAB`       | Enter cell context
`.`      |              |
`:`      | `:foo[nE8z]` | Checkpointed stream
`@`      | `@foo[\>@a]` | Enter named-gensym context
`\##`    | `\>foo \##`  | Cat **and then obliterate** named resource(s)
         |              |
`a`      |              |
`b`      | `bL40`       | Block-read and unpack binary data
`c`      | `c`          | `uniq -c`, but emits proper TSV format
`d`      |              |
`e`      | `e[tac]`     | Exec shell command
`f`      | `fACB`       | Reorder, duplicate, or drop fields by column
`g`      | `gA`         | Sort by all or selected columns
`h`      |              |
`i`      |              |
`j`      | `j foo`      | Join sorted streams on field values
`k`      |              |
`l`      | `l'(1+ a)'`  | Map over rows using Common Lisp
`m`      | `m'a + 1'`   | Map over rows using Ruby
`n`      | `n10`        | Generate or prepend line numbers
`o`      | `oC`         | Numeric sort ascending
`p`      | `p'a + 1'`   | Map over rows using Perl
`q`      |              |
`r`      | `r10`        | Select rows by criterion
`s`      | `sfoo[n10]`  | Evaluate a lambda on another machine using SSH
`t`      |              |
`u`      | `u`          | Just like `uniq`
`v`      | `vCplc`      | Vertically transform a range of columns
`w`      |              |
`x`      | `xC`         | Exchange first fields with others
`y`      |              |
`z`      | `z4`         | Compress or decompress
         |              |
`A`      |              |
`B`      |              |
`C`      | `Cubuntu[g]` | Containerize a pipeline with Docker
`D`      | `D.foo`      | Destructure structured text data (JSON/XML)
`E`      |              |
`F`      | `FC`         | Parse data into fields
`G`      |              |
`H`      | `H c`        | Send named files through hadoop
`I`      |              |
`J`      |              |
`K`      |              |
`L`      | `L'(1+ a)'`  | (Reserved for Lisp driver)
`M`      | `M'svd(x)'`  | Faceted Octave matrix interop
`N`      | `N'svd(x)'`  | Faceted NumPy matrix interop
`O`      | `OD`         | Numeric sort descending
`P`      | `Plg`        | Evaluate Pyspark lambda context
`Q`      |              |
`R`      | `R'a+1'`     | Faceted R matrix interop
`S`      |              |
`T`      |              |
`U`      |              |
`V`      | `VB`         | Pivot and collect on field B
`W`      |              |
`X`      |              |
`Y`      |              |
`Z`      |              |
387 doc/perl.md
# Perl interface
**NOTE:** This documentation covers ni's Perl data transformer, not the
internal libraries you use to extend ni. For the latter, see
[extend.md](extend.md) (`ni //help/extend`).

For a data transformer you're more likely to use and like, see
[ruby.md](ruby.md).

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

`p` does a decent job of figuring out where a chunk of code ends where lambdas
are concerned:

```bash
$ ni :plfoo[n4p'a*a']
1
4
9
16
```

## Basic stuff
`a` to `l` are one-letter functions that return the first 12 tab-delimited
values from the current line. (12 wasn't chosen arbitrarily; the letter `m` is
Perl syntax for regular expressions, so we can't use it.) `r(...)` is a
function that takes a list of values and prints a tab-delimited row. For
example:

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
$ ni /etc/passwd F::r3p'r F_ 1..3'
x	0	0
x	1	1
x	2	2
$ ni /etc/passwd F::r3p'r scalar F_'            # number of fields
7
7
7
```

If you want to select field ranges to the end of the line (like Perl's
`@_[1..$#_]` construct), `FM` is analogous to `$#_`:

```bash
$ ni /etc/passwd F::r3p'r F_ 3..FM'
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3p'r FR 3'         # FR(n) == F_(n..FM)
0	root	/root	/bin/bash
1	daemon	/usr/sbin	/bin/sh
2	bin	/bin	/bin/sh
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
$ ni n10p'r map a*$_, 1..10' =\>mult-table
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
$ ni n100p'r rc \&sr, rsum "a", rmean "a", rmin "a", rmax "a"'
5050	50.5	1	100
```

### What's going on here
`rsum`, `rmean`, etc, return compound reducers, which are hash references with
keys that indicate (1) the initial state, (2) the reduction function **as a
string**, and (3) the finalizer (which is why `rmean` can return a single
number despite its intermediate state being the separated sum and number).

Compound reducers are compiled functions, which means their arguments are
expressed as strings representing quoted code. This is why we use `"a"` rather
than `a` in the example above: the string `'a' is spliced into a function body
along with the other reducer expressions and compiled. The result is a very
efficient reducer function that ends up looking like this:

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
      first column of the line. (We wouldn't normally need parens, but if we
      omit them here Perl will assume `a` itself is the key.) We're
      incrementing the entry within the `%{%1}` hash.
    - `&& %1` is a way to return the hash reference as the reduced value (since
      we're modifying it in place). We need to do this without using a comma
      because each reducer function is evaluated in list context.

Of course, in this case it's a lot easier to use the streaming count operator:

```bash
$ ni /etc/passwd FWpsplit// r/[a-z]/gcx
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
195 doc/row.md
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
$ ni n100n100gur4               # u = 'uniq'
1
10
100
11
```

The idea behind `g` as `group` is that this is what you do prior to an
aggregation; i.e. to group related rows together so you can stream into a
reducer.

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
$ ni data oBr4
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
```

Columns can be suffixed with `g`, `n`, and/or `-` modifiers to modify how they
are sorted (these behave as described for `sort`'s `-k` option), and ni prefers
this interpretation:

```bash
$ ni data oBg r4                # 'g' is a modifier of B, not another sort
11	-0.999990206550703	2.39789527279837
55	-0.99975517335862	4.00733318523247
99	-0.999206834186354	4.59511985013459
80	-0.993888653923375	4.38202663467388
$ ni data oB g r4               # 'g' is a sorting operator
1	0.841470984807897	0
10	-0.54402111088937	2.30258509299405
100	-0.506365641109759	4.60517018598809
11	-0.999990206550703	2.39789527279837
```

## Counting
ni gives you the `c` operator to count runs of identical rows (just
like `uniq -c`).

```bash
$ ni //ni FWpF_ r/^\\D/r500 > word-list
$ ni word-list cr10             # unsorted count
1	usr
1	bin
1	env
1	perl
1	ni
1	self
1	license
1	_
1	ni
1	https
$ ni word-list gcr10            # sort first to group words
2	A
1	ACTION
1	AN
1	AND
2	ANY
1	ARGV
1	ARISING
1	AS
1	AUTHORS
1	BE
$ ni word-list gcOr10           # by descending count
30	ni
23	lib
23	core
21	_
13	sdoc
12	the
11	pl
9	to
9	resource
9	eval
```
35 doc/sql.md
# SQL interop
ni defines a parsing context that translates command-line syntax into SQL
queries. We'll need to define a SQL connection profile in order to use it:

```bash
$ mkdir sqlite-profile
$ echo sqlite.pl > sqlite-profile/lib
$ cat > sqlite-profile/sqlite.pl <<'EOF'
defoperator sqlite => q{
  my ($db, $query) = @_;
  exec 'sqlite', '-separator', "\t", $db, $query;
};
defsqlprofile S => pmap q{sqlite_op $$_[0], $$_[1]},
                        pseq filename, $sql_query;
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
$ ni --lib sqlite-profile QStest.db foo[rx=3]
3	4
$ ni --lib sqlite-profile QStest.db foo rx=3
3	4
$ ni --lib sqlite-profile QStest.db foo Ox
5	6
3	4
1	2
```
367 doc/stream.md
# Stream operations
`bash` and `ni` are both pipeline constructors: they string processes together
by connecting one's stdin to another's stdout. For example, here's word count
implemented first in bash, then in ni (with the corresponding options
vertically aligned):

```
$ cat file |perl -lne 'print for split /\W+/' |sort |uniq -c
$ ni  file FW pF_                             g     c
```

ni's pipe symbols are implied: `ni g c` is the same as `ni g | ni c`.

```sh
$ ni --explain FW pF_ g c
["split_regex","(?^:[^\\w\\n]+)"]
["perl_mapper","F_"]
["row_sort","-t","\t"]
["count"]
```

## Files
Files append themselves to the data stream (the bash equivalent would be
`|cat - file`):

```bash
$ echo test > foo
$ ni foo
test
$ ni foo foo
test
test
```

ni automatically decompresses common formats (gz, lzo, lz4, xz, bzip2),
regardless of file extension:

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
$ ni +e'seq 4'                  # append output of shell command "seq 4"
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
$ ni n3 e'sort'                 # without +, e acts as a filter
1
2
3
$ ni n3e'sort -r'
3
2
1
$ ni n3e[ sort -r ]             # easy way to quote arguments
3
2
1
$ ni n3e[sort -r]
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
$ ni n3gA-      # reverse-sort by first field
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

## Stream combiners
ni has four operators that combine streams:

- `+`: append a stream to this one
- `^`: prepend a stream to this one
- `%`: duplicate this stream through a process, and include output
- `=`: duplicate this stream through a process, discarding its output

Visually, here's what these stream combiners do:

```
$ ni n10 n5 g           # stdin --> n10 --> n5 --> g --> stdout


                        #                  /dev/null --> n5 --> g
                        #                  ----------------------
                        #                           |
$ ni n10 +[n5 g]        # ni stdin --> n10 -------append--> ni stdout


                        #                        ni stdin --> n10
                        #                        ----------------
                        #                             |
$ ni n10 +[n5 g]        # /dev/null --> n5 --> g ---append---> ni stdout


                        #                  n5 --> g
                        #                 /        \
$ ni n10 %[n5 g]        # ni stdin --> n10 ---------+--> ni stdout


                        #                  n5 --> g --> /dev/null
                        #                 /
$ ni n10 =[n5 g]        # ni stdin --> n10 -----------> ni stdout
```

```bash
$ { echo hello; echo world; } > hw
$ ni n3 +hw
1
2
3
hello
world
$ ni n3 ^hw
hello
world
1
2
3
$ ni hw %e[wc -l]               # output from 'wc -l' is included
hello
world
2
$ ni hw =e[wc -l]               # output from 'wc -l' is gone
hello
world
```

## Writing files
You can write a file in two ways. One is, of course, using shell redirection:

```bash
$ ni n3 >file                   # nothing goes to the terminal
$ ni file
1
2
3
```

The other way is to use ni's `\>` operator:

```bash
$ ni n3 \>file2                 # writes the filename to the terminal
file2
$ ni file2
1
2
3
$ ni n3 =\>file3                # eats the filename because \> happens inside =
1
2
3
$ ni file3
1
2
3
```

The `\<` operator inverts `\>` by reading files; it's conceptually equivalent
to `xargs cat`:

```bash
$ ni n4 \>file3 \<
1
2
3
4
```

If you want to write a compressed file, you can use the `z` operator:

```bash
$ ni n3z >file3.gz
$ zcat file3.gz
1
2
3
```

`z` lets you specify which compressor you want to use; for example:

```bash
$ ni id:gzip z | gzip -dc               # gzip by default
gzip
$ ni id:gzip zg | gzip -dc              # explicitly specify
gzip
$ ni id:gzip zg9 | gzip -dc             # specify compression level
gzip
$ ni id:xz zx | xz -dc
xz
$ ni id:lzo zo | lzop -dc
lzo
$ ni id:bzip2 zb | bzip2 -dc
bzip2
```

```sh
# this one isn't a unit test because not all test docker images have a
# straightforward LZ4 install (some are too old)
$ ni id:lz4 z4 | lz4 -dc
lz4
```

ni also provides a universal decompression operator `zd`, though you'll rarely
need it because any external data will be decoded automatically. `zd` has no
effect if the data isn't compressed.

```bash
$ ni n4 z zd
1
2
3
4
$ ni n4 zd
1
2
3
4
```

Not to be outdone, ni provides the ultimate lossy compressor, `zn`, which
achieves 100% compression by writing data to `/dev/null`:

```bash
$ ni n4 zn | wc -c
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
$ ni :biglist[n100000z]r5
1
2
3
4
5
$ ni :biglist[n100000z]r5
1
2
3
4
5
```

Checkpointing, like most operators that accept lambda expressions, can also be
written with the lambda implicit. In this case the lambda ends when you break
the operators using whitespace:

```bash
$ ni :biglist n100000z r5
1
2
3
4
5
```

You can use `ni --explain` to see how ni parses something; in this case the
lambda is contained within the `checkpoint` array:

```bash
$ ni --explain :biglist n100000z r5
["checkpoint","biglist",[["n",1,100001],["sh","gzip"]]]
["head","-n",5]
$ ni --explain :biglist n100000zr5
["checkpoint","biglist",[["n",1,100001],["sh","gzip"],["head","-n",5]]]
```
37 doc/tutorial.md
# ni tutorial
You can access this tutorial by running `ni //help` or `ni //help/tutorial`.

ni parses its command arguments to build and run a data pipeline. You can get
started by using it like a version of `less` that knows how to decompress
things, but it can do a lot more; see the topics below.

```sh
$ ni data-source.gz                     # works like less
$ cat data-source.gz | ni               # same here
$ ni //help/stream                      # view a help topic
```

## The deep end
- [examples.md](examples.md) (`ni //help/examples`)

## Basics
- [stream.md](stream.md) (`ni //help/stream`): intro to ni grammar and data
- [row.md](row.md)       (`ni //help/row`):    row-level operators
- [col.md](col.md)       (`ni //help/col`):    column-level operators
- [perl.md](perl.md)     (`ni //help/perl`):   ni's Perl library
- [lisp.md](lisp.md)     (`ni //help/lisp`):   ni's Common Lisp library
- [ruby.md](ruby.md)     (`ni //help/ruby`):   ni's Ruby library
- [visual.md](visual.md) (`ni //help/visual`): visualizing data

## Reference
You can use `ni //options` to get a list of all parsing rules ni applies to the
command line. The output format is a TSV of `context long/short name parser`.

- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md)       (`ni //help/extend`):    how to write a ni
  extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library
__END__
