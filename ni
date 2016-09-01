#!/usr/bin/env perl
# ni: https://github.com/spencertipping/ni
# Copyright (c) 2016 Spencer Tipping
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
use 5.006_000;
BEGIN {
eval($ni::self{'boot/sdoc'} = q{
sub ni::unsdoc
{join '', grep !/^\h*[|A-Z]/ + s/^\h*c\n//, split /\n(\h*\n)+/, $_[0]}});
eval($ni::self{'boot/self'} = q{
sub ni::eval($$)
{ @ni::evals{eval('__FILE__') =~ /\(eval (\d+)\)/} = ($_[1]);
  eval "package ni;$_[0]\n;1" or die "$@ evaluating $_[1]";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1}/eg, die $@ if $@ }
sub ni::set
{ chomp($ni::self{$_[0]} = $_[1]);
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc/;
  ni::eval $_[1], $_[0] if $_[0] =~ /\.pl$/ }})}
push(@ni::keys, $2), ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\h*(\d+)\h+(.*?)(\.sdoc)?$/;
ni::eval 'exit main @ARGV', 'main';
__DATA__
43 ni.sdoc
#!/usr/bin/env perl
# ni: https://github.com/spencertipping/ni
# Copyright (c) 2016 Spencer Tipping
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

use 5.006_000;
BEGIN {
eval($ni::self{'boot/sdoc'} = q{
sub ni::unsdoc
{join '', grep !/^\h*[|A-Z]/ + s/^\h*c\n//, split /\n(\h*\n)+/, $_[0]}});

eval($ni::self{'boot/self'} = q{
sub ni::eval($$)
{ @ni::evals{eval('__FILE__') =~ /\(eval (\d+)\)/} = ($_[1]);
  eval "package ni;$_[0]\n;1" or die "$@ evaluating $_[1]";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1}/eg, die $@ if $@ }

sub ni::set
{ chomp($ni::self{$_[0]} = $_[1]);
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc/;
  ni::eval $_[1], $_[0] if $_[0] =~ /\.pl$/ }})}

push(@ni::keys, $2), ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\h*(\d+)\h+(.*?)(\.sdoc)?$/;
ni::eval 'exit main @ARGV', 'main';
__DATA__
37 ni.map.sdoc
Resource layout map.
ni is assembled by following the instructions here. This script is also
included in the ni image itself so it can rebuild accordingly. The filenames
referenced from this file correspond to SDoc-processed entries in src/.

Note that these are just the entries for the core image. ni modifies itself
during the build process to include more extensions, each of which lives in a
subdirectory of src/.

unquote ni
resource ni.sdoc
resource ni.map.sdoc

resource util.pl.sdoc
resource self.pl.sdoc
resource cli.pl.sdoc
resource sh.pl.sdoc

resource main.pl.sdoc
lib core/common
lib core/gen
lib core/stream
lib core/meta
lib core/checkpoint
lib core/col
lib core/row
lib core/facet
lib core/pl
lib core/python
lib core/sql
lib core/java
lib core/lisp
lib core/http
lib core/hadoop
lib core/pyspark
lib core/gnuplot
lib doc
17 util.pl.sdoc
Utility functions.
Generally useful stuff, some of which makes up for the old versions of Perl we
need to support.

sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}

sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}

sub rf  {open my $fh, "< $_[0]" or die $!; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]" or die $!; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}
42 self.pl.sdoc
Image functions.
ni needs to be able to reconstruct itself from a map. These functions implement
the map commands required to do this.

sub map_u {@self{@_}}
sub map_r {map sprintf("%d %s\n%s", scalar(split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub map_l {map {my $l = $_;
                map_r "$_/lib", map "$l/$_", split /\n/, $self{"$_/lib"}} @_}

sub read_map {join '', map "$_\n",
                       (map {my ($c, @a) = split /\s+/;
                               $c eq 'unquote'     ? map_u @a
                             : $c eq 'resource'    ? map_r @a
                             : $c =~ /^lib$|^ext$/ ? map_l @a
                             : die "ni: unknown map command+args: $c @a"}
                        grep {s/#.*//g; length}
                        map split(/\n/), @self{@_}), "__END__"}

sub intern_lib {
  for my $l (@_) {
    for (grep length, split /\n/, ($self{"$l/lib"} = rfc "$l/lib")) {
      my $c = $self{"$l/$_"} = rfc "$l/$_";
      eval "package ni;$c", $@ && die "$@ evaluating $l/$_" if /\.pl$/;
    }
  }
}

sub modify_self($) {
  die "ni: not a modifiable instance: $0" unless -w $0;
  open my $fh, "> $0" or die "ni: failed to open self: $!";
  print $fh read_map $_[0];
  close $fh;
}

sub extend_self($$) {
  intern_lib $_[0];
  set 'ni.map.sdoc', "$self{'ni.map.sdoc'}\n$_[1] $_[0]"
    unless grep /^(lib|ext) $_[0]$/, split /\n/, $self{'ni.map'};
  modify_self 'ni.map';
}

sub image {read_map 'ni.map'}
175 cli.pl.sdoc
Command-line option parser.
A context-aware command line parser, which in a Canard-powered world works as
the reader. Certain static symbols, despite being resolved at runtime, have
read-time parse semantics that make it possible for ni syntax to be as
expressive as (and often much more than) nfu. See design/cli.md for details.

A parse state is just a modified copy of @ARGV, including string
transformations. For example:

| $ ni foo m'r a + b' T4
  # initial parse state is ("foo", "mr a + b", "T4")
  # quasifile parse step consumes "foo", so we then have ("mr a + b", "T4")
  # quasifile parse step rejects ("mr a + b", "T4") by returning ()
  # long option parsers all reject ("mr a + b", "T4")
  # short option parser "m" happens:
  #   m -> ruby-code
  #   ruby-code is run on ("r a + b", "T4") and returns (code, "T4")
  #     (had the code ended in extra ] characters, the parser would have
  #      returned those separately, e.g. (code, "]", "T4"))
  # ...

This might seem like it would be slow, but most of the overhead lives in
high-throughput native functions that are unlikely to take up much time in
practice.

use constant end_of_argv  => sub {@_           ? () : (0)};
use constant consumed_opt => sub {length $_[0] ? () : @_};
use constant none         => sub {(undef, @_)};

sub k($) {my ($v) = @_; sub {($v, @_)}}

sub seqr(\@) {my ($ps) = @_;
         sub {my ($x, @xs, @ys, @ps);
              (($x, @_) = &$_(@_)) ? push @xs, $x : return () for @ps = @$ps;
              (\@xs, @_)}}

sub altr(\@) {my ($ps) = @_;
         sub {my @ps, @r; @r = &$_(@_) and return @r for @ps = @$ps; ()}}

sub seq(@) {ref $_ or die "non-ref to seq(): $_" for @_; seqr @_}
sub alt(@) {ref $_ or die "non-ref to alt(): $_" for @_; altr @_}

sub rep($;$) {my ($p, $n) = (@_, 0);
         sub {my (@c, @r);
              push @r, $_ while ($_, @_) = &$p(@c = @_);
              @r >= $n ? (\@r, @c) : ()}}

sub maybe($) {my ($p) = @_;
         sub {my @xs = &$p(@_); @xs ? @xs : (undef, @_)}};

sub pmap(&$) {my ($f, $p) = @_;
         sub {my @xs = &$p(@_); @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()}}

sub pif(&$) {my ($f, $p) = @_;
        sub {my @xs = &$p(@_); @xs && &$f($_ = $xs[0]) ? @xs : ()}}

sub ptag(@) {my (@xs, $p) = @_; $p = pop @xs; pmap {[@xs, $_]} $p}
sub pn($@)  {my ($n, @ps) = @_; pmap {$$_[$n]} seq @ps}

Match/consume regex.
Consumes the match, returning either the matched text or the first match group
you specify. Always matches from the beginning of a string.

sub mr($) {my $r = qr/$_[0]/;
      sub {my ($x, @xs) = @_; $x =~ s/^($r)// ? ($2 || $1, $x, @xs) : ()}}

sub mrc($) {pn 0, mr $_[0], maybe consumed_opt}

Character dispatch.
This is just a way to bypass a lot of the alt() overhead that would otherwise
result to decode a high-entropy stream of text. The most obvious case is short
option parsing.

| chalt(a => seq(...), b => ..., ...)
  # functionally the same as alt(pn(1, mr('^a', seq(...))),
  #                              pn(1, mr('^b', ...)),
  #                              ...)

Note that the dispatch character itself isn't encoded into the result.

sub chaltr(\%) {my ($ps) = @_;
           sub {my ($x, @xs, $k, @ys, %ls) = @_;
                ++$ls{length $_} for keys %$ps;
                for my $l (sort {$b <=> $a} keys %ls) {
                  return (@ys = $$ps{$c}(substr($x, $l), @xs))
                    ? ($ys[0], @ys[1..$#ys])
                    : ()
                  if exists $$ps{$c = substr $x, 0, $l};
                }
                ()}}

sub chalt(%) {my %h = @_; chaltr %h}

Regex parsing.
Sometimes we'll have an operator that takes a regex, which is subject to the
CLI reader problem the same way code arguments are. Rather than try to infer
brackets the same way, we just require that regexes are terminated with /
(which should be ok because that's also how they typically start).

use constant regex => pmap {s/\/$//; $_} mr qr/^(?:[^\\\/]+|\\.)*\//;

Code parsing.
This is nontrivial due to the CLI reader problem. The idea is that we need to
figure out how many closing brackets belong to the code, vs how many close a
lambda. Depending on the language, the only way to do this may be to shell out
to an interpreter.

use constant rbcode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs, $x, $qcode) = @_;
  ($qcode = $code) =~ s/'/'\\''/g;
  $x .= ']' while $_ = system("ruby -ce '$qcode' >/dev/null 2>&1")
                  and ($qcode =~ s/\]$//, $code =~ s/\]$//);
  $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

Perl code is similar to Ruby, but we need to explicitly disable any BEGIN{}
blocks to avoid executing side effects. We can guarantee that nothing will run
(beyond `use` statements, which we assume are safe) by removing any
occurrences of the string `BEGIN` and replacing them with something
syntactically equivalent but less volatile -- in this case, `END`.

use constant plcode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs, $x, $qcode) = @_;
  ($qcode = $code) =~ s/'/'\\''/g;

  my $begin_warning = $qcode =~ s/BEGIN/END/g;
  $x .= ']' while $_ = system("perl -ce '$qcode' >/dev/null 2>&1")
                  and ($qcode =~ s/\]$//, $code =~ s/\]$//);

  print STDERR <<EOF if $_ && $begin_warning;
ni: failed to get closing bracket count for perl code "$_[0]", possibly
    because BEGIN-block metaprogramming is disabled when ni tries to figure
    this out.
    https://github.com/spencertipping/ni/tree/master/design/cli-reader-problem.md
EOF
  $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

CLI parser generator.
ni's CLI grammar is interesting because it maintains the same basic structure
across a number of different interpretation contexts. For example, the main CLI
arguments are interpreted as local/POSIX stuff, but if you create a JVM Spark
context then the same grammar will host a series of modified operators that
compile into Java/Scala code instead. The invariant parts of this are factored
into a function that produces a CLI grammar with mutable long/short mappings
(which can be modified using 'deflong' and 'defshort' below).

our %contexts;

sub context($) {my ($c, $p) = split /\//, $_[0]; $contexts{$c}{$p}}

sub defcontext($) {
  my $short = {};
  my $long  = [];
  my $r = $contexts{$_[0]} = {};
  $$r{ops} = sub {$$r{ops}->(@_)};

  $$r{longs}  = $long;
  $$r{shorts} = $short;

  $$r{long}   = altr @$long;
  $$r{short}  = chaltr %$short;
  $$r{lambda} = alt mr '_', pn 1, mrc '\[', $$r{ops}, mr '\]';
  $$r{thing}  = alt $$r{lambda}, $$r{long}, $$r{short};
  $$r{suffix} = rep $$r{thing}, 1;
  $$r{op}     = pn 1, rep(consumed_opt), $$r{thing}, rep(consumed_opt);
  $$r{ops}    = rep $$r{op};
  $$r{cli}    = pn 0, $$r{ops}, end_of_argv;
  $$r{cli_d}  = $$r{ops};
}

defcontext 'root';

sub defshort($$$) {$contexts{$_[0]}{shorts}{$_[1]} = $_[2]}
sub deflong($$$)  {unshift @{$contexts{$_[0]}{longs}}, $_[2]}
103 sh.pl.sdoc
Self-contained POSIX shell tasks.
Compiling a shell pipeline is not entirely trivial for a couple of reasons. One
is that we have to be parsimonious about argv lengths to avoid hitting the
upper limit (as low as 4k, potentially, though realistically at least 20k). The
other is that some commands require heredocs, which in turn require unique
terminators and some creative stdin redirection. Rather than managing these
details by hand, you can instead use the `sh` function:

| my $seq      = sh 'seq', 10;
  my $perl_cat = sh ['perl', '-n'], stdin => <<EOF
    open my $fh, "<&=", 3;              # old stdin is now redirected to fd 3
    print while <$fh>;
  EOF
  my $pipeline = pipeline $seq, $perl_cat;

Now you can write `$pipeline` into a `/bin/sh` process to execute it.

NOTE: unlike system(), exec(), etc, if you say sh('ls -lh'), this will try to
execute the "ls -lh" command rather than interpreting "-lh" as an argument.

sub quote {join ' ', map /[^-A-Za-z_0-9\/:@.]/
                           ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                           : $_,
                     map 'ARRAY' eq ref($_) ? quote(@$_) : $_, @_}

Shell commands can include some context, as shown in the example above. In
addition to "stdin", which redirects fd 0 to fd 3, you can specify a hash of
prefixes. These will be evaluated before any command in the pipeline, so you
can do things like define shell functions. For example:

| my $wot = sh ['ni_wot'], prefix => {ni_wot => 'ni_wot() { echo wot; }'};

The keys in the prefixes hash are just used for deduplication. You can nest
shell commands and their prefixes will be combined:

| sub two {sh ['two', @_], prefix => {two => 'two() { eval "$1"; eval "$2"; }'}}
  my $both = two $wot, $wot;

In this case the 'two' shell function will receive two copies of the
shell-quoted invocation of 'wot', and the 'ni_wot' function will be defined
once before the pipeline.

sub collect_nested_invocations {
  local $_;
  my ($options, @xs) = @_;
  map {
    my $c = $_;
    if ('HASH' eq ref $c) {
      if (exists $$c{stdin}) {
        die "ni sh: only one stdin redirection is allowed for a subquoted "
          . "command: " . quote(@{$$c{exec}}) if exists $$options{stdin};
        $$options{stdin} = $$c{stdin};
      }
      if (exists $$c{prefix}) {
        my $p = $$c{prefix};
        $$options{prefix}{$_} = $$p{$_} for keys %$p;
      }
      [collect_nested_invocations($options, @{$$c{exec}})];
    } elsif ('ARRAY' eq ref $c) {
      [collect_nested_invocations($options, @$c)];
    } else {
      $c;
    }
  } @xs;
}

sub sh {
  return {exec => [@_]} unless ref $_[0];
  my ($c, %o) = @_;
  my ($exec) = collect_nested_invocations \%o, $c;
  +{exec => $exec, %o};
}

sub heredoc_for {my $n = 0; ++$n while $_[0] =~ /^_$n$/m; "_$n"}

sub sh_prefix() {join "\n", @self{@sh_libs}}

sub flatten {map 'ARRAY' eq ref($_) ? flatten(@$_) : $_, @_}

sub pipeline {
  my %ps;
  my @cs;
  my @hs;
  for (flatten @_) {
    my $c = quote @{$$_{exec}};
    $c .= " $$_{magic}" if exists $$_{magic};
    if (exists $$_{stdin}) {
      my $h = heredoc_for $$_{stdin};
      push @cs, "$c 3<&0 <<'$h'";
      push @hs, "$$_{stdin}\n$h";
    } else {
      push @cs, $c;
    }

    if (exists $$_{prefix}) {
      my $p = $$_{prefix};
      $ps{$_} = $$p{$_} for keys %$p;
    }
  }
  join '', map "$_\n", values %ps,
                       join("\\\n| ", @cs),
                       @hs;
}
107 main.pl.sdoc
Main function.
ni can be invoked as a stream processor, but it can also do some toplevel
things besides. This main function knows how to handle these cases.

use POSIX qw/dup dup2/;

use constant exit_success      => 0;
use constant exit_run_error    => 1;
use constant exit_nop          => 2;
use constant exit_sigchld_fail => 3;

our %option_handlers;

our @pipeline_prefix = sh 'true';
our @pipeline_suffix = ();

sub parse_ops {
  return () unless @_;
  my ($parsed) = context('root/cli')->(@_);
  return @$parsed if ref $parsed && @$parsed;
  my (undef, @rest) = context('root/cli_d')->(@_);
  die "failed to parse " . join ' ', @rest;
}

sub sh_code {pipeline @pipeline_prefix, parse_ops(@_), @pipeline_suffix}

FD redirection stuff.
ni compiles a shell pipeline by sending the pipeline code to a shell process as
standard input. The shell then executes the pipeline, which reads from fd 3 to
pull stdin (we have to do this because the shell's stdin is used for the
pipeline code, and /bin/sh doesn't reliably differentiate between "more code"
and "stdin to the program").

We can't use Perl's open() function here because it only gives us a way to
forward one file descriptor. Instead, we need to manually fork/exec and get
everything set up using pipe() and dup2.

sub run_sh {
  pipe my $r, $w;

  if (my $child = fork) {
    close $r;
    close STDIN;
    close STDOUT;
    syswrite $w, $_[0] or die "ni: failed to write pipeline to shell: $!";
    close STDERR;
    close $w;
    waitpid $child, 0;
    $?;
  } else {
    close $w;
    if (fileno $r == 3) {
      defined(my $fd = dup fileno $r) or die "ni: failed to dup temp fd: $!";
      close $r;
      $r = $fd;
    }
    dup2 0, 3 or die "ni: failed to redirect stdin to shell: $!"
      unless -t STDIN;
    close STDIN;
    dup2 fileno $r, 0 or die "ni: failed to redirect command to shell: $!";
    close $r;

    exec 'sh' or exec 'ash' or exec 'dash' or exec 'bash'
      or die "ni: failed to run any POSIX sh: $!";
  }
}

Internal and debugging options.
A handful of things useful for ni development. These are also present in the
final image.

$option_handlers{'internal/eval'}
  = sub {eval "package ni; $_[0]"; die $@ if $@; exit_success};

$option_handlers{'internal/lib'} = sub {extend_self $_[0], 'lib'};

$option_handlers{'internal/parse'}
  = sub {eval("package ni; sub {print join(' | ', ($_[0])->(\@_)), '\n'}")
           ->(@_[1..$#_]); die $@ if $@; exit_success};

Extensions.
User-facing options to modify or live-extend ni. --lib is used to load code but
not change the ni image, --extend causes ni to self-modify to include the
specified extension.

$option_handlers{lib} = sub {intern_lib shift; goto \&main};
$option_handlers{run} = sub {eval 'package ni;' . shift;
                             die $@ if $@;
                             goto \&main};

$option_handlers{extend} = sub {extend_self $_[0], 'ext'};

Documentation options.
Options for the end user that can't otherwise be parsed as pipelines.

$option_handlers{help} = sub {@_ = ("//help/" . (@_ ? $_[0] : '')); goto \&main};

$option_handlers{explain} = sub {TODO()};
$option_handlers{compile} = sub {print sh_code @_; exit_nop};

sub main {
  my ($command, @args) = @_;
  $command = '--help' if $command eq '-h' or !@_ && -t STDIN && -t STDOUT;
  my $h = $command =~ s/^--// && $option_handlers{$command};
  return &$h(@args) if $h;
  run_sh sh_code @_;
}
1 core/common/lib
common.pl.sdoc
35 core/common/common.pl.sdoc
Basic CLI types.
Some common argument formats for various commands, sometimes transformed for
specific cases. These are documented somewhere in `doc/`.

use constant neval   => pmap {eval} mr '^=([^]]+)';
use constant integer => alt pmap(sub {10 ** $_},  mr '^E(-?\d+)'),
                            pmap(sub {1 << $_},   mr '^B(\d+)'),
                            pmap(sub {0 + "0$_"}, mr '^x[0-9a-fA-F]+'),
                            pmap(sub {0 + $_},    mr '^\d+');
use constant float   => pmap {0 + $_} mr '^-?\d*(?:\.\d+)?(?:[eE][-+]?\d+)?';
use constant number  => alt neval, integer, float;

use constant colspec1 => mr '^[A-Z]';
use constant colspec  => mr '^[-A-Z.]+';

Generic code parser.
Counts brackets outside quoted strings, which in our case are '' and "".
Doesn't look for regular expressions because these vary by language; but this
parser should be able to handle most straightforward languages with quoted
string literals and backslash escapes.

use constant generic_code => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs) = @_;
  (my $tcode = $code) =~ s/"([^"\\]+|\\.)"|'([^'\\]+|\\.)'//g;
  my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
  $balance ? (substr($code, 0, $balance), substr($code, $balance))
           : ($code, @xs)};

Filenames, in general.
Typically filenames won't include bracket characters, though they might include
just about everything else. Two possibilities there: if we need special stuff,
there's the `file:` prefix; otherwise we assume the non-bracket interpretation.

use constant filename => alt mrc '^file:(.+)', mr '^[^][]+';
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
4 core/stream/lib
stream.sh.sdoc
cat.pm.sdoc
decode.pm.sdoc
stream.pl.sdoc
31 core/stream/stream.sh.sdoc
Stream shell functions.
These are called by pipelines to simplify things. For example, a common
operation is to append the output of some data-producing command:

| $ ni . .              # lists current directory twice

If you do this, ni will compile a pipeline that uses stream wrappers to
concatenate the second `ls` output (despite the fact that technically it's a
shell pipe).

ni_append()  { cat; "$@"; }
ni_prepend() { "$@"; cat; }

ni_append_hd()  { cat <&3; "$@"; }
ni_prepend_hd() { "$@"; cat <&3; }

ni_pipe() { eval "$1" | eval "$2"; }

ni_seq() { perl -e \
  'for (my $i = $ARGV[0]; $i <= $ARGV[1]; ++$i) {print "$i\n"}' "$@"; }

ni_tee() { { perl -e 'open FH, ">&=3" or die $!;
                      while (sysread STDIN, $_, 8192) {print FH; print}' \
               | "$@"; } 3>&1; }

Pager handling.
A wrapper around various programs to preview long streams of data. We might
not have any, but if we do, we make them available under the 'ni_pager'
function.

ni_pager() { ${NI_PAGER:-less} || more || cat; }
17 core/stream/cat.pm.sdoc
Perl code for the ni_cat function.
ni_cat exists to turn filesystem objects into text. Files are emitted and
directories are turned into listings. This code isn't evaluated into ni;
instead, it's used as a library and inserted via -e into a perl interpreter.

while (@ARGV) {
  my $f = shift @ARGV;
  if (-d $f) {
    opendir my $d, $f or die "ni_cat: failed to opendir $f: $!";
    print "$f/$_\n" for sort grep $_ ne '.' && $_ ne '..', readdir $d;
    closedir $d;
  } else {
    open F, '<', $f or die "ni_cat: failed to open $f: $!";
    syswrite STDOUT, $_ while sysread F, $_, 8192;
    close F;
  }
}
41 core/stream/decode.pm.sdoc
Compressed stream support.
This provides a perl command you can use to read the contents of a compressed
stream as though it weren't compressed. It's implemented as a filter process so
we don't need to rely on file extensions.

We detect the following file formats:

| gzip:  1f 8b
  bzip2: BZh\0
  7z:    37 7A BC AF 27 1C
  lzop:  89 4c 5a 4f
  lz4:   04 22 4d 18
  xz:    fd 37 7a 58 5a

Decoding works by reading enough to decode the magic, then forwarding data
into the appropriate decoding process (or doing nothing if we don't know what
the data is).

my ($fd) = @ARGV;
if (defined $fd) {
  close STDIN;
  open STDIN, "<&=$fd" or die "ni_decode: failed to open fd $fd: $!";
}

sysread STDIN, $_, 8192;

my $decoder = /^\x1f\x8b/             ? "gzip -dc"
            : /^BZh\0/                ? "bzip2 -dc"
            : /^\x89\x4c\x5a\x4f/     ? "lzop -dc"
            : /^\x04\x22\x4d\x18/     ? "lz4 -dc"
            : /^\xfd\x37\x7a\x58\x5a/ ? "xz -dc" : undef;

if (defined $decoder) {
  open FH, "| $decoder" or die "ni_decode: failed to open '$decoder': $!";
  syswrite FH, $_;
  syswrite FH, $_ while sysread STDIN, $_, 8192;
  close FH;
} else {
  syswrite STDOUT, $_;
  syswrite STDOUT, $_ while sysread STDIN, $_, 8192;
}
90 core/stream/stream.pl.sdoc
Streaming data sources.
Common ways to read data, most notably from files and directories. Also
included are numeric generators, shell commands, etc. Most of the functionality
here is implemented by stream.sh.

use constant stream_sh => {stream_sh => $self{'core/stream/stream.sh'}};
use constant perl_fn   => gen '%name() { perl -e %code "$@"; }';
use constant perl_ifn  => gen "%name() { perl - \"\$@\" <<'%hd'; }\n%code\n%hd";

sub perl_fn_dep($$)
{+{$_[0] => perl_fn->(name => $_[0], code => quote $self{$_[1]})}}

sub perl_stdin_fn_dep($$)
{+{$_[0] => perl_ifn->(name => $_[0], code => $_[1], hd => heredoc_for $_[1])}}

sub ni_cat($)     {sh ['ni_cat', $_[0]], prefix => perl_fn_dep 'ni_cat',    'core/stream/cat.pm'}
sub ni_decode(;$) {sh ['ni_decode', @_], prefix => perl_fn_dep 'ni_decode', 'core/stream/decode.pm'}

sub ni_pager {sh ['ni_pager'], prefix => stream_sh}

sub ni_pipe {@_ == 1 ? $_[0] : sh ['ni_pipe', $_[0], ni_pipe(@_[1..$#_])],
                                  prefix => stream_sh}

sub ni_append  {sh ['ni_append', @_], prefix => stream_sh}
sub ni_verb($) {sh ['ni_append_hd', 'cat'], stdin => $_[0],
                                            prefix => stream_sh}

TTY handling.
Reset the default pipeline prefix/suffix depending on what ni is connected to.
We don't want to interact with a terminal directly; we use less for output, and
terminal input is ignored.

@pipeline_prefix = -t STDIN  ? ()       : ni_decode 3;
@pipeline_suffix = -t STDOUT ? ni_pager : ();

Data sources.
Various useful sources including shell commands, number generators, filesystem
accessors, and verbatim text.

sub ni_file($) {ni_append 'eval', ni_pipe ni_cat $_[0], ni_decode}

deflong 'root', 'stream/sh', pmap {ni_append qw/sh -c/, $_}
                             mrc '^(?:sh|\$):(.*)';

deflong 'root', 'stream/fs', pmap {ni_file $_}
                             alt mrc '^file:(.+)', pif {-e} mrc '^[^][]+';

deflong 'root', 'stream/n',  pmap {ni_append 'ni_seq', 1, $_}     pn 1, mr '^n:',  number;
deflong 'root', 'stream/n0', pmap {ni_append 'ni_seq', 0, $_ - 1} pn 1, mr '^n0:', number;

deflong 'root', 'stream/id', pmap {ni_append 'echo', $_} mrc '^id:(.*)';

Shell transformation.
Pipe through a shell command. We also define a command to duplicate a stream
through a shell command.

deflong 'root', 'stream/pipe', pmap {sh 'sh', '-c', $_} mrc '^\$=(.*)';
deflong 'root', 'stream/tee',  pmap {sh 'ni_tee', 'sh', '-c', $_}
                               mrc '^\$\^(.*)';

Sinking.
We can sink data into a file just as easily as we can read from it. This is
done with the `>` operator, which is typically written as `\>`.

defshort 'root', '>', pmap {sh 'tee', $_} filename;

Compression and decoding.
Sometimes you want to emit compressed data, which you can do with the `Z`
operator. It defaults to gzip, but you can also specify xz, lzo, lz4, or bzip2
by adding a suffix. You can decode a stream in any of these formats using `ZD`
(though in most cases ni will automatically decode compressed formats).

our %compressors;

defshort 'root', 'Z', pmap {dor $_, sh 'gzip'} maybe chaltr %compressors;

sub defcompressor($$) {$compressors{$_[0]} = $_[1]}
sub defnormalcompressor($$) {
  my ($alt, $comp) = @_;
  defcompressor $alt, pmap {defined $_ ? sh $comp, "-$_" : sh $comp}
                      maybe integer;
}

defnormalcompressor 'g', 'gzip';
defnormalcompressor 'x', 'xz';
defnormalcompressor 'o', 'lzop';
defnormalcompressor '4', 'lz4';
defnormalcompressor 'b', 'bzip2';

defcompressor 'D', k ni_decode;
1 core/meta/lib
meta.pl.sdoc
28 core/meta/meta.pl.sdoc
Image-related data sources.
Long options to access ni's internal state. Also the ability to instantiate ni
within a shell process.

deflong 'root', 'meta/self', pmap {ni_verb image}            mr '^//ni';
deflong 'root', 'meta/keys', pmap {ni_verb join "\n", @keys} mr '^//ni/';
deflong 'root', 'meta/get',  pmap {ni_verb $self{$_}}        mr '^//ni/([^]]+)';

sub ni {sh ['ni_self', @_], prefix => perl_stdin_fn_dep 'ni_self', image}

deflong 'root', 'meta/ni', pmap {ni @$_} pn 1, mr '^@', context 'root/lambda';

Documentation options.
These are listed under the `//help` prefix. This isn't a toplevel option
because it's more straightforward to model these as data sources.

deflong 'root', 'meta/help',
  pmap {$_ = 'README' if !length or /^tutorial$/;
        die "ni: unknown help topic: $_" unless exists $self{"doc/$_.md"};
        ni_verb $self{"doc/$_.md"}}
  pn 1, mr '^//help/?', mrc '^.*';

Misc utility options.
Mostly around ni-specific oddities like SDoc.

deflong 'root', 'meta/sdoc',
  pmap {sh qw/perl -e/, "$self{'boot/sdoc'}; print ni::unsdoc join '', <>"}
  mrc '^--sdoc';
2 core/checkpoint/lib
checkpoint.sh.sdoc
checkpoint.pl.sdoc
5 core/checkpoint/checkpoint.sh.sdoc
Checkpoint shell functions.
We need a function that generates a checkpoint file atomically; that is, it
becomes its final name only when all of the content has been written.

ni_checkpoint() { sh | tee "$1.part" && mv "$1.part" "$1"; }
16 core/checkpoint/checkpoint.pl.sdoc
Checkpoint files.
You can break a long pipeline into a series of smaller files using
checkpointing, whose operator is `:`. The idea is to cache intermediate
results. A checkpoint specifies a file and a lambda whose output it should
capture.

use constant checkpoint_prefix =>
  {checkpoint_sh => $self{'core/checkpoint/checkpoint.sh'},
   %{stream_sh()}};

defshort 'root', ':',
  pmap {-r $$_[0] ? ni_file $$_[0]
                  : [sh(['ni_append_hd', 'ni_checkpoint', $$_[0]],
                        stdin  => pipeline($$_[1]),
                        prefix => checkpoint_prefix), ni_decode]}
  seq filename, context 'root/lambda';
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
  sh 'cut', '-f', join ',', $rest ? (@fs, "$floor-") : @fs;
}

our $cut_gen = gen q{chomp; @_ = split /\t/; print join("\t", @_[%is]), "\n"};

sub ni_cols(@) {
  # TODO: this function shouldn't be parsing column specs
  my $ind   = grep /[^A-I.]/, @_;
  my $asc   = join('', @_) eq join('', sort @_);
  my @cols  = map /^\.$/ ? -1 : ord($_) - 65, @_;
  my $floor = (sort {$b <=> $a} @cols)[0] + 1;
  return col_cut $floor, scalar(grep $_ eq '.', @_), @cols if $ind && $asc;

  sh ['perl', '-ne',
      $cut_gen->(is => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cols)];
}

our @col_alt = (pmap {ni_cols split //, $_} colspec);

defshort 'root', 'f', altr @col_alt;

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

sub ni_split_chr($)   {sh 'perl', '-lnpe', "y/$_[0]/\\t/"}
sub ni_split_regex($) {sh 'perl', '-lnpe', "s/$_[0]/\$1\\t/g"}
sub ni_scan_regex($)  {sh 'perl', '-lne',  'print join "\t", /' . "$_[0]/g"}

our %split_chalt = (
  'C' => (pmap {ni_split_chr   ','}              none),
  'P' => (pmap {ni_split_chr   '|'}              none),
  'S' => (pmap {ni_split_regex qr/\h+/}          none),
  'W' => (pmap {ni_split_regex qr/[^\w\n]+/}     none),
  '/' => (pmap {ni_split_regex $_}               regex),
  ':' => (pmap {ni_split_chr   $_}               mr '^.'),
  'm' => (pn 1, mr '^/', pmap {ni_scan_regex $_} regex),
);

defshort 'root', 'F', chaltr %split_chalt;

sub defsplitalt($$) {$split_chalt{$_[0]} = $_[1]}
2 core/row/lib
row.sh.sdoc
row.pl.sdoc
25 core/row/row.sh.sdoc
Row-selection functions.
Wrappers to select specific rows from a stream. This implements the `r`
command. Note that it's faster (and more powerful) to use perl than grep for
regex matching, at least last time I tested it.

ni_revery()  { perl -ne 'print unless $. % '"$1"; }
ni_rmatch()  { perl -lne 'print if /'"$1"/; }

Row sampling is done using a Poisson process, which takes rand() out of the
line of fire. This is particularly useful for sparse samplings.

ni_rsample() { perl -ne '
  BEGIN {srand($ENV{NI_SEED} || 42)}
  if ($. >= 0) {print; $. -= -log(1 - rand()) / '"$1"'}'; }

Sorting.
The biggest question here is whether we have a version of `sort` that can
handle options like `--compress-program`. If so, we want to use these
extensions because they accelerate things substantially. We have to detect this
in shell rather than inside the ni codegen because the compiled code might be
sent elsewhere.

ni_sort() {
  # TODO: --compress-program etc
  sort "$@"; }
57 core/row/row.pl.sdoc
Row-level operations.
These reorder/drop/create entire rows without really looking at fields.

use constant row_pre => {row_sh => $self{'core/row/row.sh'}};

our @row_alt = (
  (pmap {sh 'tail', '-n', $_}                      pn 1, mr '^\+', number),
  (pmap {sh 'tail', '-n', '+' . ($_ + 1)}          pn 1, mr '^-',  number),
  (pmap {sh ['ni_revery',  $_], prefix => row_pre} pn 1, mr '^x',  number),
  (pmap {sh ['ni_rmatch',  $_], prefix => row_pre} pn 1, mr '^/',  regex),
  (pmap {sh ['ni_rsample', $_], prefix => row_pre} mr '^\.\d+'),
  (pmap {sh 'head', '-n', $_}                      alt neval, integer));

defshort 'root', 'r', altr @row_alt;

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

use constant sortspec => rep seq colspec1, maybe mr '^[gnr]+';

sub sort_args {'-t', "\t",
               map {my $i = ord($$_[0]) - 64;
                    my $m = defined $$_[1] ? $$_[1] : '';
                    ('-k', "$i$m,$i")} @_}

sub ni_sort(@) {sh ['ni_sort', @_], prefix => row_pre}

defshort 'root', 'g', pmap {ni_sort        sort_args @$_} sortspec;
defshort 'root', 'G', pmap {ni_sort '-u',  sort_args @$_} sortspec;
defshort 'root', 'o', pmap {ni_sort '-n',  sort_args @$_} sortspec;
defshort 'root', 'O', pmap {ni_sort '-rn', sort_args @$_} sortspec;

Counting.
Sorted and unsorted streaming counts.

sub ni_count {sh 'perl', '-ne',
  q{if ($_ ne $last) {print "$n\t$_" if defined $last; $n = 0; $last = $_} ++$n;
    END {print "$n\t$last" if defined $last}}}

defshort 'root', 'c', k ni_count;
defshort 'root', 'C', k [ni_sort, ni_count];
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

our %facet_chalt;

defshort 'root', '@', chaltr %facet_chalt;

sub deffacetalt($$) {$facet_chalt{$_[0]} = $_[1]}
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
43 core/pl/pl.pl.sdoc
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

sub perl_gen($$) {sh [qw/perl -/],
  stdin => perl_mapgen->(prefix => perl_prefix,
                         body   => $_[0],
                         each   => $_[1])}

sub perl_mapper($)  {perl_gen $_[0], 'pr for row'}
sub perl_grepper($) {perl_gen $_[0], 'pr if row'}
sub perl_facet($)   {perl_gen $_[0], 'pr row . "\t$_"'}

our @perl_alt = (pmap {perl_mapper $_} plcode);

defshort 'root', 'p', altr @perl_alt;

sub defperlalt($) {unshift @perl_alt, $_[0]}

defrowalt pmap {perl_grepper $_} pn 1, mr '^p', plcode;

deffacetalt 'p', pmap {[perl_facet $$_[0],
                        sh(['ni_sort', '-k1,1'], prefix => row_pre),
                        perl_mapper $$_[1]]} seq plcode, plcode;
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

use constant pycode => pmap {pydent $_} generic_code;
1 core/sql/lib
sql.pl.sdoc
131 core/sql/sql.pl.sdoc
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

use constant sqlcode => generic_code;

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

use constant sql_table => pmap {sqlgen $_} mrc '^[^][]*';

our $sql_query = pmap {sql_compile $$_[0], @{$$_[1]}}
                 seq sql_table, maybe alt context 'sql/lambda',
                                          context 'sql/suffix';

our @sql_row_alt;
our @sql_join_alt = (
  (pmap {['ljoin', $_]} pn 1, mr '^L', $sql_query),
  (pmap {['rjoin', $_]} pn 1, mr '^R', $sql_query),
  (pmap {['njoin', $_]} pn 1, mr '^N', $sql_query),
  (pmap {['ijoin', $_]} $sql_query),
);

defshort 'sql', 's', pmap {['map',    $_]} sqlcode;
defshort 'sql', 'w', pmap {['filter', $_]} sqlcode;
defshort 'sql', 'r', altr @sql_row_alt;
defshort 'sql', 'j', altr @sql_join_alt;
defshort 'sql', 'G', k ['uniq'];

defshort 'sql', 'g', pmap {['order_by', $_]}        sqlcode;
defshort 'sql', 'o', pmap {['order_by', "$_ ASC"]}  sqlcode;
defshort 'sql', 'O', pmap {['order_by', "$_ DESC"]} sqlcode;

defshort 'sql', '+', pmap {['union',      $_]} $sql_query;
defshort 'sql', '*', pmap {['intersect',  $_]} $sql_query;
defshort 'sql', '-', pmap {['difference', $_]} $sql_query;

defshort 'sql', '@', pmap {+{select => $$_[1], group_by => $$_[0]}}
                     seq sqlcode, sqlcode;

Global operator.
SQL stuff is accessed using Q, which delegates to a sub-parser that handles
configuration/connections.

our %sql_profiles;

defshort 'root', 'Q', chaltr %sql_profiles;
1 core/java/lib
java.pl.sdoc
4 core/java/java.pl.sdoc
Java op context.
A context that translates ni CLI operators into Java control flow.

defcontext 'java/cf';
2 core/lisp/lib
prefix.lisp
lisp.pl.sdoc
75 core/lisp/prefix.lisp
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

(defun read-col (text)
  (when text
    (let* ((*read-eval* nil)
           (r (read-from-string text)))
      (etypecase r
        (symbol text)
        (number r)))))

(defun r (&rest values)
  (apply #'join #\Tab values))

(defmacro with-ni-env (filter-p &body body)
  (let ((l-var (gensym "L"))
        (cols-var (gensym "COLS")))
    `(let ((*standard-input* (sb-sys:make-fd-stream 3)))
       (symbol-macrolet ,(loop for colname in '(a b c d e f g h i j k l m n o p q)
                            for index from 0
                            collect (list colname `(read-col (nth ,index ,cols-var))))     
         (loop for ,l-var = (read-line *standard-input* nil) while ,l-var do
              (let ((,cols-var (split ,l-var #\Tab)))
                ,(if filter-p
                     `(when (progn ,@body)
                        (write-string ,l-var)
                        (terpri))
                     `(progn
                        ,@(loop for form in body collect
                               `(progn
                                  (princ ,form)
                                  (terpri)))))))))))
46 core/lisp/lisp.pl.sdoc
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

use constant lispcode => mrc '^.*[^]]+';

defshort 'root', 'l', pmap {sh [qw/sbcl --noinform --script/],
                               stdin => lisp_mapgen->(prefix => lisp_prefix,
                                                      body   => $_)}
                           lispcode;

defrowalt pmap {sh [qw/sbcl --noinform --script/],
                   stdin => lisp_grepgen->(prefix => lisp_prefix,
                                           body   => $_)}
          pn 1, mr '^l', lispcode;
2 core/http/lib
http.pm.sdoc
http.pl.sdoc
42 core/http/http.pm.sdoc
HTTP server.
A very simple HTTP server that can be used to serve a stream's contents. The
server is defined solely in terms of a function that takes a URL and returns
data, optionally specifying the content type. The HTTP server returns when it
receives EPIPE while writing to stdout.

The code to drive the HTTP server arrives as stdin, and the optional hostname
and port are specified as @ARGV. If omitted, they default to 127.0.0.1 and
random.

use Socket;

my ($hostname, $port) = @ARGV;
$hostname = '127.0.0.1' unless defined $hostname;

socket S, PF_INET, SOCK_STREAM, getprotobyname 'tcp'
  or die "ni_http: socket() failed: $!";

setsockopt S, SOL_SOCKET, SO_REUSEADDR, pack 'l', 1
  or die "ni_http: setsockopt() failed: $!";

my $tries = 0;
until (defined $port) {
  $port = 1024 + int rand(65535 - 1024);
  bind S, sockaddr_in $port, INADDR_ANY or $port = undef;
  die "ni_http: bind() failed on over 4096 random ports: $!" if ++$tries > 4096;
}

listen S, SOMAXCONN or die "ni_http: listen() failed: $!";

print "http://$hostname:$port/\n";
close STDOUT;

sub http(&) {
  my ($f) = @_;
  for (; accept C, S; close C) {
    $_ = ''; sysread C, $_, 8192, length until /\n\r?\n\r?$/;
    *STDOUT = C;
    print "HTTP/1.1 200 OK\n\n";
    pr for &$f(/^GET (.*) HTTP\//);
  }
}
19 core/http/http.pl.sdoc
HTTP server.
A trivial server that gets to decide what to do with the input stream. This
blocks stream processing until a request comes in, at which point the function
is called with `$url` set to the request URL.

use constant perl_httpgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3' or die "ni_http: failed to open fd 3: $!";
  http {
    %body
  };
};

defshort 'root', 'H', pmap {sh [qw/perl -/, defined $$_[0] ? ($$_[0]) : ()],
  stdin => perl_httpgen->(prefix => join("\n", perl_prefix,
                                               $self{'core/http/http.pm'}),
                          body   => $$_[1])}
  seq maybe number, plcode;
1 core/hadoop/lib
hadoop.pl.sdoc
2 core/hadoop/hadoop.pl.sdoc
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

use constant pyspark_fn => pmap {pyspark_lambda $_} pycode;

our $pyspark_rdd = pmap {pyspark_compile 'sc', @$_}
                   alt context 'pyspark/lambda',
                       context 'pyspark/suffix';

our @pyspark_row_alt = (
  (pmap {gen "%v.sample(False, $_)"} alt neval, integer),
  (pmap {gen "%v.takeSample(False, $_)"} mr '^\.(\d+)'),
  (pmap {gen "%v.filter($_)"} pyspark_fn));

deflong 'pyspark', 'stream/n',
  pmap {gen "sc.parallelize(range($_))"} pn 1, mr '^n:', number;

deflong 'pyspark', 'stream/pipe',
  pmap {gen "%v.pipe(" . pyquote($_) . ")"} mr '^\$=([^]]+)';

defshort 'pyspark', 'p', pmap {gen "%v.map(lambda x: $_)"} pyspark_fn;
defshort 'pyspark', 'r', altr @pyspark_row_alt;
defshort 'pyspark', 'G', k gen "%v.distinct()";
defshort 'pyspark', 'g', k gen "%v.sortByKey()";

defshort 'pyspark', '+', pmap {gen "%v.union($_)"} $pyspark_rdd;
defshort 'pyspark', '*', pmap {gen "%v.intersect($_)"} $pyspark_rdd;

Configuration management.
A profile contains the code required to initialize the SparkContext and any
other variables relevant to the process. Each is referenced by a single
character and stored in the %spark_profiles table.

our %spark_profiles = (
  L => k gen pydent q{from pyspark import SparkContext
                      sc = SparkContext("local", "%name")
                      %body});

sub ni_pyspark {sh ['echo', 'TODO: pyspark', @_]}

defshort 'root', 'P', pmap {ni_pyspark @$_}
                      seq chaltr(%spark_profiles), $pyspark_rdd;
1 core/gnuplot/lib
gnuplot.pl.sdoc
9 core/gnuplot/gnuplot.pl.sdoc
Gnuplot interop.
An operator that tees output to a gnuplot process.

defcontext 'gnuplot';

sub compile_gnuplot {sh ['ni_tee', qw/gnuplot -persist -e/, @_]}

defshort 'root', 'P', pmap {compile_gnuplot $_} context 'gnuplot/op';
defshort 'gnuplot', 'd', k 'plot "-" with dots';
13 doc/lib
README.md
col.md
examples.md
extend.md
facet.md
internals.md
libraries.md
lisp.md
options.md
perl.md
row.md
sql.md
stream.md
22 doc/README.md
# ni tutorial
You can access this tutorial by running `ni //help` or `ni //help/tutorial`.

ni parses its command arguments to build and run a shell pipeline. Help topics
include:

## Basics
- [stream.md](stream.md) (`ni //help/stream`): intro to ni grammar and data
- [row.md](row.md) (`ni //help/row`): row-level operators
- [col.md](col.md) (`ni //help/col`): column-level operators
- [perl.md](perl.md) (`ni //help/perl`): ni's Perl library
- [ruby.md](ruby.md) (`ni //help/ruby`): ni's Ruby library
- [facet.md](facet.md) (`ni //help/facet`): the faceting operator

## Reference
- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md) (`ni //help/extend`): how to write a ni extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library
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
$ ni n:8p'r map a*$_, 1..8' > mult-table
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
# ni: https://github.com/spencertipping/ni
# Copyright (c) 2016 Spencer Tipping
```

```bash
$ ni //ni r3F/\\//                      # split on forward slashes
#!	usr	bin	env perl
# ni: https:		github.com	spencertipping	ni
# Copyright (c) 2016 Spencer Tipping
```

```bash
$ ni //ni r3FW                          # split on non-words
	usr	bin	env	perl
	ni	https	github	com	spencertipping	ni
	Copyright	c	2016	Spencer	Tipping
```

```bash
$ ni //ni r3FS                          # split on whitespace
#!/usr/bin/env	perl
#	ni:	https://github.com/spencertipping/ni
#	Copyright	(c)	2016	Spencer	Tipping
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
14 doc/extend.md
# Extending ni
You can extend ni by writing a library. For example, suppose we want a new
operator `N` that counts lines by shelling out to `wc -l`:

```bash
$ mkdir my-library
$ echo my-lib.pl > my-library/lib
$ echo "defshort 'root', 'N', k sh ['wc', '-l'];" > my-library/my-lib.pl
$ ni --lib my-library n:100N
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
10 doc/internals.md
# Internal options
ni provides some internal debugging options that you may find useful if you're
writing extensions.

```bash
$ ni --internal/parse generic_code [foo]
[foo]
$ ni --internal/parse generic_code [foo]]]
[foo] | ]]
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
43 doc/lisp.md
# Common Lisp driver
ni supports Common Lisp via SBCL, which is available using the `l` and `L`
operators. For example:

```bash
$ ni n:4l'(+ a 2)'
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
$ ni n:4l'(r a (1+ a))'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n:4l'(r a (1+ a))' l'(r (+ a b))'        # ... and sum them
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
$ ni n:2l'a (+ a 100)'                   # return without "r"
1
101
2
102
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
$ ni n:5p'a * a'                # square some numbers
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
$ ni n:4p'r a, a + 1'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n:4p'r a, a + 1' p'r a + b'        # ... and sum them
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
$ ni n:2p'a, a + 100'                   # return without "r"
1
101
2
102
$ ni n:2p'r a, a + 100'                 # use "r" for side effect, return ()
1	101
2	102
$ ni n:3p'r $_ for 1..a; ()'            # use r imperatively, explicit return
1
1
2
1
2
3
$ ni n:3p'r $_ for 1..a'                # use r imperatively, implicit return
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
$ ni n:10p'r ru {a%4 == 0}'             # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

The line array returned by `ru` is just an array of flat, tab-delimited strings
(verbatim lines from standard input), but you can extract fields using the
column-accessor functions `a_`, `b_`, etc:

```bash
$ ni n:10p'r map a*$_, 1..10' | tee mult-table
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
$ ni n:100p'sum rw {1}'
5050
$ ni n:10p'prod rw {1}'
3628800
$ ni n:100p'mean rw {1}'
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
$ ni n:10000p'sr {$_[0] + a} 0'
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
$ ni n:100p'my ($sum, $n, $min, $max) = sr {$_[0] + a, $_[1] + 1,
                                            min($_[2], a), max($_[2], a)}
                                           0, 0, a, a;
            r $sum, $sum / $n, $min, $max'
5050	50.5	1	100
```

And here's the easy way, using `rc`:

```bash
$ ni n:100p'r rc \&sr, rsum A, rmean A, rmin A, rmax A'
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
$ ni n:10r3                     # take first 3
1
2
3
$ ni n:10r+3                    # take last 3
8
9
10
$ ni n:10r-7                    # drop first 7
8
9
10
```

## Sampling
```bash
$ ni n:10000rx4000              # take every 4000th row
4000
8000
$ ni n:10000r.0002              # sample uniformly, P(row) = 0.0002
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
$ ni n:10000r/[42]000$/
2000
4000
$ ni n:1000r/[^1]$/r3
2
3
4
```

These regexes are evaluated by Perl, which is likely to be faster than `grep`
for nontrivial patterns.

## Code
`rp` means "select rows for which this Perl expression returns true".

```bash
$ ni n:10000rp'$_ % 100 == 42' r3
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
$ ni n:100n:10gr4               # g = 'group'
1
1
10
10
$ ni n:100n:100Gr4              # G = 'group uniq'
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
$ ni n:100or3                   # o = 'order': sort numeric ascending
1
2
3
$ ni n:100Or3                   # O = 'reverse order'
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
$ ni n:100p'r a, sin(a), log(a)' > data         # generate multicolumn data
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
1	https
1	github
1	com
1	spencertipping
$ ni word-list Cr10             # sort first to group words
41	0
8	006_000
1	1
9	2
2	2016
2	3
1	43
1	5
2	A
3	ACTION
```
29 doc/sql.md
# SQL interop
ni defines a parsing context that translates command-line syntax into SQL
queries. We'll need to define a SQL connection profile in order to use it:

```bash
$ mkdir sqlite-profile
$ echo sqlite.pl > sqlite-profile/lib
$ cat > sqlite-profile/sqlite.pl <<'EOF'
$sql_profiles{S} = pmap {sh "sqlite3", "-separator", "\t", $$_[0], $$_[1]}
                        seq mrc '^.*', $sql_query;
EOF
```

Now we can create a test database and use this library to access it.

```bash
$ sqlite3 test.db <<'EOF'
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
219 doc/stream.md
# Stream operations
Streams are made of text, and ni can do a few different things with them. The
simplest involve stuff that bash utilities already handle (though more
verbosely):

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
$ ni n:4                        # integer generator
1
2
3
4
$ ni n0:4                       # integer generator, zero-based
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
$ ni n:3 | sort
1
2
3
$ ni n:3 $=sort                 # $= filters through a command
1
2
3
$ ni n:3 $='sort -r'
3
2
1
```

And, of course, ni has shorthands for doing all of the above:

```bash
$ ni n:3 g      # g = sort
1
2
3
$ ni n:3g       # no need for whitespace
1
2
3
$ ni n:3gAr     # reverse-sort by first field
3
2
1
$ ni n:3O       # NOTE: capital O, not zero; more typical reverse numeric sort
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
$ ni n:3 >file
$ ni file
1
2
3
```

The other way is to copy the stream to a file:

```bash
$ ni n:3 \>file2
1
2
3
$ ni file2
1
2
3
```

If you want to write a compressed file, you can use the `Z` operator:

```bash
$ ni n:3Z >file3.gz
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
$ ni id:lz4 Z4 | lz4 -dc
lz4
$ ni id:bzip2 Zb | bzip2 -dc
bzip2
```

ni also provides a decompression operator `ZD`, though you'll rarely need it
because any external data will be decoded automatically:

```bash
$ ni n:4 Z ZD
1
2
3
4
```

## Checkpoints
Checkpoints let you cache intermediate outputs in a pipeline. This can avoid
expensive recomputation. For example, let's expensively get some numbers:

```bash
$ ni n:1000000gr4
1
10
100
1000
```

If we wanted to iterate on the pipeline from this point onwards, we could do
this quickly by checkpointing the result:

```bash
$ ni :numbers[n:1000000gr4]
1
10
100
1000
```

Now this data will be reused if we rerun it:

```bash
$ ni :numbers[n:1000000gr4]O
1000
100
10
1
```

ni isn't rerunning the process at all; we can see this by modifying the
checkpoint file:

```bash
$ echo 'checkpointed' > numbers
$ ni :numbers[n:1000000gr4]O
checkpointed
```

You can write compressed data into a checkpoint. The checkpointing operator
itself will decode any compressed data you feed into it; for example:

```bash
$ ni :biglist[n:100000Z]r5
1
2
3
4
5
$ ni :biglist[n:100000Z]r5
1
2
3
4
5
```
__END__
