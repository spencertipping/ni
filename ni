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
  eval "package ni;$_[0]\n;1";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1 - 1}/eg, die $@ if $@ }

sub ni::set
{ chomp($ni::self{$_[0]} = $_[1]);
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc/;
  ni::eval $_[1], $_[0] if $_[0] =~ /\.pl$/ }

push(@ni::keys, $2), ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\s*(\d+)\s+(.*?)(\.sdoc)?$/;
ni::eval 'exit main @ARGV', 'main';
_
die $@ if $@
__DATA__
39 ni.map.sdoc
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
resource self.pl.sdoc
resource cli.pl.sdoc
resource sh.pl.sdoc

resource main.pl.sdoc
lib core/common
lib core/gen
lib core/stream
lib core/meta
lib core/checkpoint
lib core/deps
lib core/col
lib core/row
lib core/facet
lib core/pl
lib core/python
lib core/sql
lib core/java
lib core/lisp
lib core/http
lib core/plot
lib core/hadoop
lib core/pyspark
lib core/gnuplot
lib core/jsplot
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
41 self.pl.sdoc
Image functions.
ni needs to be able to reconstruct itself from a map. These functions implement
the map commands required to do this.

sub map_r {map sprintf("%d %s\n%s", scalar(split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub map_l {map {my $l = $_;
                map_r "$_/lib", map "$l/$_", split /\n/, $self{"$_/lib"}} @_}

sub read_map {join '', map "$_\n",
                       (map {my ($c, @a) = split /\s+/;
                               $c eq 'bootcode'    ? ni::boot_header
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
182 cli.pl.sdoc
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
                  if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
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

sub defshort($$$) {
  die "ni: redefining existing short operator $_[1] (use rmshort first)"
    if exists $contexts{$_[0]}{shorts}{$_[1]};
  $contexts{$_[0]}{shorts}{$_[1]} = $_[2];
}

sub rmshort($) {delete $contexts{$_[0]}{shorts}{$_[1]}}

sub deflong($$$) {unshift @{$contexts{$_[0]}{longs}}, $_[2]}
104 sh.pl.sdoc
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

sub heredoc_for($)   {my $n = 0; ++$n while $_[0] =~ /^_$n$/m; "_$n"}
sub heredoc_wrap($$) {my $hd = heredoc_for $_[0]; "<<'$hd'$_[1]\n$_[0]\n$hd\n"}

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
      push @hs, $$_{stdin} =~ /\n$/ ? "$$_{stdin}$h" : "$$_{stdin}\n$h";
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
109 main.pl.sdoc
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
      my $fd = dup fileno $r;
      defined $fd or die "ni: failed to dup temp fd " . fileno($r) . ": $!";
      close $r;
      open $r, "<&=$fd" or die "ni: failed to open dup fd $fd: $!";
    }
    dup2 0, 3 or die "ni: failed to redirect stdin to shell: $!"
      unless -t STDIN;
    close STDIN;
    dup2 fileno $r, 0 or die "ni: failed to redirect command to shell "
                           . fileno($r) . " -> 0): $!";
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
40 core/common/common.pl.sdoc
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

use constant ea       => mrc '^';

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

use constant tmpdir   => dor $ENV{TMPDIR}, '/tmp';
use constant tempfile => pmap {tmpdir . "/ni-$$-$_"} mr '^@(\w*)';

use constant filename => alt mrc '^file:(.+)', tempfile, mr '^[^][]+';
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
46 core/stream/stream.sh.sdoc
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

ni_null() { perl -e '1 while sysread STDIN, $_, 8192'; }

ni_seq() { perl -e \
  'for (my $i = $ARGV[0]; $i <= $ARGV[1]; ++$i) {print "$i\n"}' "$@"; }

ni_tee() { { perl -e 'open FH, ">&=3" or die $!;
                      while (sysread STDIN, $_, 8192) {print FH; print}' \
               | "$@"; } 3>&1; }

ni_read() { while read f; do cat f; done; }
ni_write() { cat > "$1"; echo "$1"; }

ni_write_tempfile() {
  ni_tempfile_prefix="${TMPDIR:-/tmp}/ni-$$-"
  ni_tempfile_entropy=
  while [ -e "$ni_tempfile_prefix-$ni_tempfile_entropy" ]; do
    ni_tempfile_entropy="$(perl -e 'print join "", map chr(65 + rand 32), 0..12')"
  done
  cat > "$ni_tempfile_prefix-$ni_tempfile_entropy"
  echo "$ni_tempfile_prefix-$ni_tempfile_entropy"
}

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
100 core/stream/stream.pl.sdoc
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
sub ni_null  {sh ['ni_null'],  prefix => stream_sh}

sub ni_pipe {@_ == 1 ? $_[0] : sh ['ni_pipe', $_[0], ni_pipe(@_[1..$#_])],
                                  prefix => stream_sh}

sub ni_append  {sh ['ni_append', @_], prefix => stream_sh}
sub ni_verb($) {sh ['ni_append_hd', 'cat'], stdin => $_[0],
                                            prefix => stream_sh}

sub ni_write_tempfile {sh ['ni_write_tempfile'], prefix => stream_sh}

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

TODO: mixed-format reads; there's no good reason to think that each entry being
read has the same encoding. This is a big refactor because it means we need to
be able to send arbitrary portions of ni to the compiled process.

defshort 'root', '>%', pmap {sh 'tee', $_} filename;
defshort 'root', '>',  pmap {sh ['ni_write', $_], prefix => sh_prefix} filename;
defshort 'root', '<', k sh ['ni_read'], prefix => sh_prefix;

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

defcompressor 'N', k ni_null;
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
  pmap {$_ = 'tutorial' unless length;
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
1 core/deps/lib
sha1.pl
1031 core/deps/sha1.pl
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
  'S' => (pmap {ni_split_regex qr/\s+/}          none),
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
106 core/lisp/prefix.lisp
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

(declaim (inline next-line))
(defun next-line ()
  (read-line *standard-input* nil))

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

(defun output-rows (&rest rows)
  (dolist (row rows)
    (princ row)
    (terpri)))

(defmacro with-ni-env (filter-p &body body)
  (let ((l-var (gensym "L")))
    `(let ((*standard-input* (sb-sys:make-fd-stream 3)))
       (symbol-macrolet ,(loop for colname in '(a b c d e f g h i j k l m n o p q)
                            for index from 0
                            collect (list colname `(read-col (nth ,index *cols*))))     
         (loop for ,l-var = (read-line *standard-input* nil) while ,l-var do
              (let ((*cols* (split ,l-var #\Tab)))
                ,(if filter-p
                     `(when (progn ,@body)
                        (write-string ,l-var)
                        (terpri))
                     `(progn
                        ,@(loop for form in body collect
                               `(multiple-value-call #'output-rows ,form)
                        )))))))))
48 core/lisp/lisp.pl.sdoc
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

sub lisp_sh($) {sh [qw| sbcl --noinform --noprint --eval |,
                    '(load *standard-input* :verbose nil :print nil)'],
                   stdin => $_[0]}

defshort 'root', 'l', pmap {lisp_sh lisp_mapgen->(prefix => lisp_prefix,
                                                  body   => $_)}
                      lispcode;

defrowalt pmap {lisp_sh lisp_grepgen->(prefix => lisp_prefix,
                                       body   => $_)}
          pn 1, mr '^l', lispcode;
4 core/http/lib
http.pm.sdoc
ws.pm.sdoc
http.pl.sdoc
ws.pl.sdoc
50 core/http/http.pm.sdoc
HTTP server.
A very simple HTTP server that can be used to serve a stream's contents. The
server is defined solely in terms of a function that takes a URL and returns
data, optionally specifying the content type.

The code to drive the HTTP server arrives as stdin, and the optional hostname
and port are specified as @ARGV. If omitted, they default to 127.0.0.1 and
random.

use Socket;

socket S, PF_INET, SOCK_STREAM, getprotobyname 'tcp'
  or die "ni_http: socket() failed: $!";

setsockopt S, SOL_SOCKET, SO_REUSEADDR, pack 'l', 1
  or die "ni_http: setsockopt() failed: $!";

my ($port) = (@ARGV, 1024);
@ARGV = ();
++$port > 65535 && die "ni_http: bind() failed: $!"
  until bind S, sockaddr_in $port, INADDR_ANY;

listen S, SOMAXCONN or die "ni_http: listen() failed: $!";

sub http_reply($$%) {
  my ($code, $body, %headers) = @_;
  print "HTTP/1.1 $code NI\n";
  print "$_: $headers{$_}\n" for sort keys %headers;
  print "Content-Length: " . length($body) . "\n\n";
  print $body;
}

sub uri_decode(@) {
  my ($u) = @_;
  $u =~ s/%(..)/chr hex $1/eg;
  $u;
}

sub http(&) {
  my ($f) = @_;
  for (; $_ = '', accept C, S; close C) {
    1 while 0 < waitpid -1, WNOHANG;
    next if fork;
    close S;
    sysread C, $_, 8192, length until /\r?\n\r?\n$/;
    *STDOUT = C;
    pr for &$f(uri_decode(/^GET (.*) HTTP\//), $_);
    exit;
  }
}
31 core/http/ws.pm.sdoc
WebSocket encoding functions.
We just encode text messages; no binary or other protocols are defined yet.

c
BEGIN {
  eval 'use Digest::SHA qw/sha1_base64/';
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
38 core/http/http.pl.sdoc
HTTP server.
A trivial server that gets to decide what to do with the input stream. This
blocks stream processing until a request comes in, at which point the function
is called with `$_[0]` set to the URL.

use constant perl_httpgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3' or die "ni_http: failed to open fd 3: $!";
  %body
};

use constant perl_http_opgen => gen q{
  print "http://localhost:$port/\n";
  close STDOUT;
  http {
    print "HTTP/1.1 200 OK\n\n";
    %body
  };
};

use constant http_prefix => join "\n", "BEGIN {$self{'core/deps/sha1.pl'}\n}",
                                       $self{'core/http/http.pm'},
                                       $self{'core/http/ws.pm'};

sub ni_http_server {
  my ($body, @args) = grep defined, @_;
  sh [qw/perl -/, @args], stdin => perl_httpgen->(prefix => http_prefix,
                                                  body   => $body);
}

This HTTP server function is more for testing than anything especially
practical. However, the HTTP library is used to serve the JS plotting interface
and stream the data over a websocket.

deflong 'root', 'http/url',
  pmap {ni_http_server perl_http_opgen->(body => perl_prefix . "\n$$_[2]"), $$_[1]}
  seq mrc '^--http$', maybe pn(0, number, maybe ea), plcode;
16 core/http/ws.pl.sdoc
WebSocket serialization.
Converts an arbitrary data stream to a series of WebSocket messages. ni uses
this to stream data into the client for JSPlot visualization. All data is sent
as a "text" message.

use constant ws_perl => $self{'core/http/ws.pm'};
use constant wse_gen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3' or die 'ni_http: failed to open fd 3';
  print ws_encode $_ while <STDIN>;
};

deflong 'root', 'http/wse',
  pmap {sh [qw/perl -/], stdin => wse_gen->(prefix => ws_perl)}
  mrc '^--http/wse$';
1 core/plot/lib
plot.pl.sdoc
9 core/plot/plot.pl.sdoc
Plot operator.
This delegates to other operators using a chalt. The mnemonic is "v" for
"visualize."

our %plot_chalt;

defshort 'root', 'v', chaltr %plot_chalt;

sub defplotalt($$) {$plot_chalt{$_[0]} = $_[1]}
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
defshort 'gnuplot', 'd', k 'plot "-" with dots';

sub compile_gnuplot {sh ['ni_tee', qw/gnuplot -persist -e/, @_]}

defplotalt 'g', pmap {compile_gnuplot $_} context 'gnuplot/op';
4 core/jsplot/lib
jsplot.css.sdoc
jsplot.js.sdoc
jsplot.pm.sdoc
jsplot.pl.sdoc
7 core/jsplot/jsplot.css.sdoc
body {margin: 0; color: #eee; background: #111; font-family: sans-serif;
      overflow: hidden}

#i {opacity:0.25; background:rgba(255, 255, 255, 0.1); font-size:16pt;
    color:#eee; font-family:monospace; border:none; outline:none;
    position:absolute; left:8px; top:8px }
#i:focus {opacity: 1}
70 core/jsplot/jsplot.js.sdoc
JSPlot.
A plotting library that allows you to live-transform the data, and that
supports incremental rendering in not much space.

$(function () {
var w  = $(window);
var c  = $('#c');
var t  = $('#i');
var cx = c[0].getContext('2d');
var wwl, whl;
setInterval(function () {
  var ww = w.width(), wh = w.height();
  if (ww !== wwl || wh !== whl) {
    t.css({width: ww - 2 * t.offset().left});
    c.attr({width: wwl = ww, height: whl = wh});
    refresh();
  }
}, 50);

View coordinates.
vr = view rotation, vp = view position, vs = view scaling. Everything is done
in 3D whether we're looking at 2D or 3D data.

var vr = [0, 0, 0];
var vp = [0, 0, 0];
var vs = [0, 0, 0];

AJAX data requests.
Data comes down in small pieces and is rendered as it arrives. We do this for
two reasons: first, to decrease view latency; and second, to run in constant
space. (The client does store some data, but there's an upper bound on how
much.)

t.keydown(function (e) {if (e.which === 13) refresh()});

var ws = null;
var refresh = function () {
  var ws_url = document.location.href.replace(/^http:/, 'ws:') + 'ni/';
  if (ws != null) ws.close();
  ws = new WebSocket(ws_url + t.val(), 'data');
  cache = [];
  cx.clearRect(0, 0, wwl, whl);
  ws.onmessage = function (e) {collect_point(e.data.split(/\t/))};
};

Rendering logic.
Super simple: [x, y, z] if present. We sample points into a cache and render
everything in timed batches.

var n_points        = 0;
var points_to_cache = 65536;
var cache           = [];
var buffer          = [];
var collect_point = function (p) {
  buffer.push(p);
  ++n_points;
  var i = Math.random() * n_points | 0;
  if (i < points_to_cache) cache[i] = p;
};

setInterval(function () {
  cx.fillStyle = 'rgba(255, 255, 255, 0.5)';
  for (var i = 0; i < buffer.length; ++i)
    cx.fillRect(+buffer[i][0] + wwl/2, +buffer[i][1] + whl/2, 1, 1);
  buffer = [];
}, 50);

refresh();

});
29 core/jsplot/jsplot.pm.sdoc
JSPlot data streaming functions.
This ends up being called from the quoted code block in jsplot.pl.sdoc. This is
the websocket connection that ni uses to stream data to the client. The
template defines a constant called 'self' that contains the ni image.

use POSIX qw/dup2 :sys_wait_h/;

sub quote {join ' ', map /[^-A-Za-z_0-9\/:@.]/
                           ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                           : $_,
                     map 'ARRAY' eq ref($_) ? quote(@$_) : $_, @_}

sub ni($) {
  if (1 != fileno STDOUT) {
    POSIX::close 1;
    dup2 fileno(STDOUT), 1 or die "ni_jsplot: failed to dup to stdout: $!";
    close STDOUT;
  }

  my $process = "perl - $_[0]";
  open PL, "| $process" or die "ni_jsplot: failed to open | $process: $!";
  syswrite PL, self;
}

sub ni_stream_data($$@) {
  my ($req, $opts, @fs) = @_;
  print ws_header $req;
  ni quote(@fs) . " $opts --http/wse";
}
47 core/jsplot/jsplot.pl.sdoc
JSPlot interop.
JSPlot is served over HTTP as a portable web interface. It requests data via
AJAX, and may request the same data multiple times to save browser memory. The
JSPlot driver buffers the data to disk to make it repeatable.

use constant jsplot_gen => gen <<'EOF';
<!doctype html>
<html>
<head>
<title>ni/jsplot</title>
<style>%css</style>
<script src='https://code.jquery.com/jquery.min.js'></script>
<script>%js</script>
</head>
<body>
<input id='i'></input>
<canvas id='c'></canvas>
</body>
</html>
EOF

use constant jsplot_html => jsplot_gen->(css => $self{'core/jsplot/jsplot.css'},
                                         js  => $self{'core/jsplot/jsplot.js'});

use constant jsplot_server_gen => gen q{
  use constant self => %self_hd
  %prefix
  print "http://localhost:$port/";
  close STDOUT;
  use constant html => q{%html};
  chomp(our @data = <>);
  http {
    my ($url, $req) = @_;
    http_reply 200, html if $url eq '/';
    ni_stream_data $req, $1, @data if $url =~ /^\/ni\/(.*)/;
    http_reply 404, $url;
  };
};

use constant jsplot_prefix => join "\n", @self{qw| util.pl
                                                   core/jsplot/jsplot.pm |};

defplotalt 'j',
  k [ni_write_tempfile,
     ni_http_server jsplot_server_gen->(prefix  => jsplot_prefix,
                                        self_hd => heredoc_wrap(image, ';'),
                                        html    => jsplot_html)];
13 doc/lib
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
70 doc/lisp.md
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
$ ni n:10000l"(sr ('+ a))"
50005000
```

Or to reduce multiple columns into a row:

```bash
$ ni n:4fAA l"(r (sr ('+ a) ('* b)))"
10	24
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
1	self
1	license
1	_
1	ni
$ ni word-list Cr10             # sort first to group words
14	0
8	1
9	2
2	2016
1	3
1	39
1	5
1	A
2	ACTION
1	AN
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
240 doc/stream.md
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
$ ni n:3 >file                  # nothing goes to the terminal
$ ni file
1
2
3
```

The other way is to use one of ni's two file-writing operators:

```bash
$ ni n:3 \>file2                # writes the filename to the terminal
file2
$ ni file2
1
2
3
$ ni n:3 \>%file3               # duplicates output
1
2
3
$ ni file3
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
$ ni n:4 Z ZD
1
2
3
4
$ ni n:4 ZD
1
2
3
4
```

Finally, ni provides the ultimate lossy compressor, `ZN`, which achieves 100%
compression by writing data to `/dev/null`:

```bash
$ ni n:4 ZN | wc -c
0
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
