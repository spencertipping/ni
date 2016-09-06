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
die $@;
__DATA__
19 ni.map.sdoc
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
resource pipeline.pl.sdoc
resource main.pl.sdoc
38 util.pl.sdoc
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
24 dev.pl.sdoc
Development functions.
Utilities helpful for debugging and developing ni.

sub dev_inspect($) {
  local $_;
  my $x = $_[0];
  return '[' . join(', ', map dev_inspect($_), @$x) . ']' if 'ARRAY' eq ref $x;
  return '{' . join(', ', map "$_ => " . dev_inspect($$x{$_}), keys %$x) . '}' if 'HASH' eq ref $x;
  "" . $x;
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
    printf STDERR "$indent$fname %s = %s\n", dev_inspect [@_], dev_inspect [@r];
    @r;
  };
}
101 parse.pl.sdoc
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

  defparser 'cond', '&$',
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
61 common.pl.sdoc
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
48 cli.pl.sdoc
CLI grammar.
ni's command line grammar uses some patterns on top of the parser combinator
primitives defined in parse.pl.sdoc. Probably the most important one to know
about is the long/short option dispatch, which looks like this:

| option = alt @longs, dsp %shorts

This compound dispatch pattern is called a "context" because it represents the
full range of stuff a ni parser will end up resolving.

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

sub pseries($) {prep pn 1, popt pempty, $contexts{$_[0]}, popt pempty}
sub plambda($) {pn 1, prc qr/\[/, pseries $_[0], prc qr/\]/}
sub pcli($)    {pn 0, pseries $_[0], pend}
50 pipeline.pl.sdoc
Pipeline construction.
A way to build a shell pipeline in-process by consing a transformation onto
this process's standard input. This will cause a fork to happen, and the forked
PID is returned.

use POSIX qw/dup2/;

sub cdup2 {dup2 @_ or die "ni: dup2(@_) failed: $!"}
sub cfork {my $pid = fork; die "ni: fork failed: $!" unless defined $pid; $pid}
sub cpipe {pipe $_[0], $_[1] or die "ni: pipe failed: $!"}

sub move_fd($$) {
  my ($old, $new) = @_;
  return if $old == $new;
  close $new;
  cdup2 $old, $new;
  close $old;
}

sub with_stdin(&$) {
  cpipe my $r, my $w;
  my $pid = cfork;
  if ($pid) {
    close $r;
    syswrite $w, $_[1];
    close $w;
    $pid;
  } else {
    move_fd 0, 3;
    move_fd fileno $r, 0;
    close $w;
    &{$_[0]}();
    exit;
  }
}

sub stdin_cons(&@) {
  cpipe my $r, my $w;
  my $pid = cfork;
  if ($pid) {
    close $w;
    move_fd fileno $r, 0;
    return $pid;
  } else {
    close $r;
    move_fd fileno $w, 1;
    &{$_[0]}(@_[1..$#_]);
    exit;
  }
}
30 main.pl.sdoc
CLI entry point.
Some custom toplevel option handlers and the main function that ni uses to
parse CLI options and execute the data pipeline.

our %cli_special;
sub defclispecial($$) {$cli_special{$_[0]} = fn $_[1]}

Development options.
Things useful for developing ni.

defclispecial '--dev/self', q{print "$ni::self{$_[0]}\n"};
defclispecial '--dev/parse', q{
  dev_trace 'ni::parse';
  parse pcli '', @_;
};

Root CLI context.

defcontext '';

defshort '/x', pk 'x';
defshort '/y', pk 'y';

sub main {
  my ($cmd, @args) = @_;
  return $cli_special{$cmd}->(@args) if exists $cli_special{$cmd};
  my @r = parse pcli '', @_;
  print scalar(@r), "\n";
  print "$_\n" for @r;
}
