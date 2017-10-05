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
use strict;
$ni::data = \*DATA;

sub ni::boot_header
{ join "\n", '#!/usr/bin/env perl',
             "\$ni::self{license} = <<'_';\n$ni::self{license}_",
             "eval(\$ni::self{ni} = <<'_');\n$ni::self{ni}_",
             "die \$@ if \$@",
             "__DATA__" }

sub ni::eval($;$)
{ @ni::evals{eval('__FILE__') =~ /\(eval (\d+)\)/} = ($_[1] || "anon {$_[0]}");
  my @r = eval "package ni;$_[0]";
  $@ =~ s/\(eval (\d+)\)/$ni::evals{$1 - 1}/eg, die $@ if $@;
  @r }

sub ni::set
{ my $k = $_[0];
  chomp($ni::self{$k} = $_[1]);
  ni::eval $_[1], $k if $k =~ /\.pl$/ }

ni::set $2, join '', map $_ = <DATA>, 1..$1
while defined($_ = <DATA>) && /^\s*(\d+)\s+(.*)$/;
ni::eval 'exit main @ARGV', 'main';
_
die $@ if $@
__DATA__
53 core/boot/ni.map
# Resource layout map.
# ni is assembled by following the instructions here. This script is also
# included in the ni image itself so it can rebuild accordingly.

bootcode
resource core/boot/ni.map

resource core/boot/util.pl
resource core/boot/doc.pl
resource core/boot/dev.pl
resource core/boot/parse.pl
resource core/boot/common.pl
resource core/boot/cli.pl
resource core/boot/op.pl
resource core/boot/self.pl
resource core/boot/main.pl
lib core/gen
lib core/json
lib core/deps
lib core/conf
lib core/stream
lib core/meta
lib core/monitor
lib core/uri
lib core/fn
lib core/closure
lib core/destructure
lib core/checkpoint
lib core/net
lib core/buffer
lib core/script
lib core/assert
lib core/col
lib core/row
lib core/pl
lib core/bloom
lib core/cell
lib core/rb
lib core/lisp
lib core/sql
lib core/python
lib core/binary
lib core/matrix
lib core/gnuplot
lib core/image
lib core/http
lib core/caterwaul
lib core/jsplot
lib core/mapomatic
lib core/docker
lib core/hadoop
lib core/pyspark
lib doc
105 core/boot/util.pl
# Utility functions.
# Generally useful stuff, some of which makes up for the old versions of Perl we
# need to support.

sub weval($) {my @r = eval "package ni;$_[0]"; print STDERR $@ if $@; @r}

sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub swap($$) {@_[0, 1] = @_[1, 0]}
sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}

sub sum {local $_; my $x = 0; $x += $_ for @_; $x}

sub rf  {open my $fh, "< $_[0]" or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]" or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub dirbase($)  {my @xs = $_[0] =~ /^(.*)\/+([^\/]+)\/*$/; @xs ? @xs : ('', $_[0])}
sub basename($) {(dirbase $_[0])[1]}
sub dirname($)  {(dirbase $_[0])[0]}

sub mkdir_p {-d $_[0] or !length $_[0] or mkdir_p(dirname $_[0]) && mkdir $_[0]}

sub wf {
  mkdir_p dirname $_[0];
  open my $fh, "> $_[0]" or die "wf $_[0]: $!";
  print $fh $_[1];
  close $fh;
  $_[0];
}

sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

use constant noise_chars => '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
sub noise_char() {substr noise_chars, rand(length noise_chars), 1}
sub noise_str($) {join '', map noise_char, 1..$_[0]}

sub abbrev($$) {length($_[0]) < $_[1] ? $_[0] : substr($_[0], 0, $_[1] - 3) . '...'}

sub indent($;$) {
  my ($s, $indent) = (@_, 2);
  join "\n", map ' ' x $indent . $_, split /\n/, $s;
}

# Module loading.
# ni can include .pm files in its resource stream, which contain Perl code but
# aren't evaluated by default. This function is used to eval them into the
# current runtime.

sub load($) {ni::eval $ni::self{$_[0]}, $_[0]}

# Shell quoting/unquoting.
# Useful for two cases. One is when you have a structured list and want a shell
# string that will send those arguments to a command; the other is when you have
# a single string and want to get the ARGV list a shell would pass to a command
# (modulo dollar substitution, which we don't do).

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
  (my $c = $_[0]) =~ s/\\\n//g;
  1 while $c =~ s/^\s+|\s+$//g || $c =~ s/(?:^|\s)#.*/$1/gm;
  my @ps = $c =~ /"(?:[^"\\]+|\\[\s\S])*"|'[^']*'|\\[\s\S]|[^\s"'\\]+|\s+/g;
  my @s  = (-1, grep($ps[$_] =~ /^\s/, 0..$#ps), scalar @ps);
  map join('', map shell_unquote_one $_, @ps[$s[$_]+1 .. $s[$_+1]-1]), 0..$#s-1;
}

# Quoted function support.
# Functions that store their code in string form. This is useful for two
# purposes: first, it enables you to recompile things, e.g. for dynamic inlining;
# and second, it makes functions self-documenting, particularly in the context of
# parser combinators.

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
30 core/boot/doc.pl
# Documentation generator.
# Produces documentation by inspecting data structures. For the most part this is
# delegated.

our %doc;
our %doc_fns;
our %doc_entities;

sub documentation_for($$) {
  my ($type, $thing) = @_;
  $doc_fns{$type}->($thing, $doc{$type}{$thing}, $type);
}
sub default_doc_fn($$) {$_[1]}

sub doc_sections(@) {
  my %sections      = @_;
  my @section_order = map $_[$_ * 2], 0..$#_ >> 1;
  join "\n", map {('', ($_ ? '## ' : '# ') . $section_order[$_],
                   map "\t$_", split /\n/, $sections{$section_order[$_]})}
             0..$#section_order;
}

sub defdocumentable($$;$) {
  my ($name, $global_hash, $fn) = @_;
  $doc{$name}          = {};
  $doc_fns{$name}      = defined $fn ? fn $fn : \&default_doc_fn;
  $doc_entities{$name} = $global_hash;
  ni::eval "sub doc$name(\$\$) {\$ni::doc{'$name'}{\$_[0]} = \$_[1]}";
  ni::eval "sub ${name}_doc(\$) {documentation_for '$name', \$_[0]}";
}
42 core/boot/dev.pl
# Development functions.
# Utilities helpful for debugging and developing ni.

sub try_to_resolve_coderef($) {
  for my $f (keys %{ni::}) {
    return "ni::$f" if \&{"ni::$f"} eq $_[0];
  }
  return '<opaque code reference>';
}

sub dev_inspect($;\%);
sub dev_inspect($;\%) {
  local $_;
  my ($x, $refs) = (@_, {});
  return "<circular $x>" if defined $x && exists $$refs{$x};

  $$refs{$x} = $x if defined $x;
  my $r = 'ARRAY' eq ref $x ? '[' . join(', ', map dev_inspect($_, %$refs), @$x) . ']'
        : 'HASH'  eq ref $x ? '{' . join(', ', map "$_ => " . dev_inspect($$x{$_}, %$refs), keys %$x) . '}'
        : 'CODE'  eq ref $x ? try_to_resolve_coderef($x)
        : defined $x        ? "" . $x
        :                     'undef';
  delete $$refs{$x} if defined $x;
  $r;
}

sub dev_inspect_nonl($) {(my $r = dev_inspect $_[0]) =~ s/\s+/ /g; $r}

sub dev_trace($) {
  no strict 'refs';
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
227 core/boot/parse.pl
# Parser combinators.
# List-structured combinators. These work like normal parser combinators, but are
# indirected through data structures to make them much easier to inspect. This
# allows ni to build an operator mapping table.

our %parsers;
BEGIN {defdocumentable 'parser', \%parsers, q{
  my ($p, $doc) = @_;
  if (ref $p) {
    my ($name, @args) = @$p;
    return $ni::doc{parser}{$name}->($p) if ref $ni::doc{parser}{$name};
    parser_doc($name);
  } else {
    doc_sections
      "PARSER $p"  => $doc,
      "DEFINITION" => parser_ebnf($ni::parsers{$p});
  }
}}

our %parser_ebnf;
sub defparserebnf($$) {$parser_ebnf{$_[0]} = fn $_[1]}
sub parser_ebnf($) {
  my ($p) = @_;
  return "<$p>" unless ref $p;
  return "<core parser {\n" . indent($p, 2) . "\n}>" unless 'ARRAY' eq ref $p;
  my ($name, @args) = @$p;
  return "<" . join(' ', $name, map dev_inspect($_), @args) . ">" unless exists $parser_ebnf{$name};
  $parser_ebnf{$name}->($p);
}

sub parser($);
sub parser($) {
  return $_[0] if ref $_[0];
  die "ni: parser $_[0] is not defined" unless exists $parsers{$_[0]};
  parser $parsers{$_[0]};
}

sub defparser($$$) {
  my ($name, $proto, $f) = @_;
  (my $code_name = $name) =~ s/\W+/_/g;
  die "ni: defparser cannot redefine $name" if exists $parsers{$name};
  $parsers{$name} = fn $f;
  eval "sub $code_name($proto) {['$name', \@_]}";
}

sub defparseralias($$) {
  my ($name, $alias) = @_;
  (my $code_name = $name) =~ s/\W+/_/g;
  die "ni: defparseralias cannot redefine $name" if exists $parsers{$name};
  $parsers{$name} = $alias;
  eval "sub $code_name() {['$name']}" unless exists ${ni::}{$code_name};
}

# Parse function.
# ($result, @remaining) = parse($parser, @inputs). $parser can take one of three
# forms:

# | ['parser-name', @args]: $parsers{'parser-name'} is a function
#   ['parser-name']: $parsers{'parser-name'} could be a function or an array
#   'parser-name': $parsers{'parser-name'} is an array

our $recursion_level = 0;
sub parse;
sub parse {
  local $_;
  local $recursion_level = $recursion_level + 1;
  my ($p, @args) = @{parser $_[0]};
  die "ni: runaway parse of $p [@args] on @_[1..$#_] ($recursion_level levels)"
    if $recursion_level > 1024;
  my $f = $parsers{$p};
  'ARRAY' eq ref $f ? parse $f, @_[1..$#_] : &$f([$p, @args], @_[1..$#_]);
}

# Base parsers.
# Stuff for dealing with some base cases.

BEGIN {
  defparser 'pend',   '',  q{@_ > 1                        ? () : (0)};
  defparser 'pempty', '',  q{defined $_[1] && length $_[1] ? () : (0, @_[2..$#_])};
  defparser 'pk',     '$', q{(${$_[0]}[1], @_[1..$#_])};
  defparser 'pnone',  '',  q{(undef,       @_[1..$#_])};
}

defparserebnf pend   => q{'<argv_end>'};
defparserebnf pempty => q{'<empty>'};
defparserebnf pk     => q{"<'', evaluate as " . dev_inspect($_[0][1]) . ">"};
defparserebnf pnone  => q{"''"};

# Basic combinators.
# Sequence, alternation, etc. 'alt' implies a sequence of alternatives; 'dsp' is
# a dispatch on specified prefixes. The 'r' suffix means that the parser
# combinator takes a reference to a collection; this allows you to modify the
# collection later on to add more alternatives.

BEGIN {
  defparser 'paltr', '\@',
    q{my ($self, @xs, @ps, @r) = @_;
      @r = parse $_, @xs and return @r for @ps = @{parser $$self[1]}; ()};

  defparser 'pdspr', '\%',
    q{my ($self, $x, @xs, $k, @ys, %ls, $c) = @_;
      my (undef, $ps) = @$self;
      return () unless defined $x;
      ++$ls{length $_} for keys %$ps;
      for my $l (sort {$b <=> $a} keys %ls) {
        return (@ys = parse $$ps{$c}, substr($x, $l), @xs) ? @ys : ()
        if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
      }
      ()};
}

sub palt(@) {my @ps = @_; paltr @ps}
sub pdsp(%) {my %ps = @_; pdspr %ps}

defparserebnf paltr => fn q{
  my ($self, $alt) = @{$_[0]};
  my @docs = map "| " . sr(indent(parser_ebnf $_, 2), qr/^  /, ''), @$alt;
  "(\n" . join("\n", @docs) . "\n)";
};

defparserebnf pdspr => fn q{
  my ($self, $dsp) = @{$_[0]};
  my @docs = map "| '$_' " . sr(indent(parser_ebnf $$dsp{$_}, 2), qr/^  /, ''),
             sort keys %$dsp;
  "(\n" . join("\n", @docs) . "\n)";
};

BEGIN {
  defparser 'pseq', '@',
    q{my ($self, @is, $x, @xs, @ys) = @_;
      my (undef, @ps) = @$self;
      (($x, @is) = parse $_, @is) ? push @xs, $x : return () for @ps;
      (\@xs, @is)};

  defparser 'prep', '$;$',
    q{my ($self, @is, @c, @r) = @_;
      my (undef, $p, $n) = (@$self, 0);
      push @r, $_ while ($_, @is) = parse $p, (@c = @is);
      @r >= $n ? (\@r, @c) : ()};

  defparser 'popt', '$',
    q{my ($self, @is) = @_;
      my @xs = parse $$self[1], @is; @xs ? @xs : (undef, @is)};

  defparser 'pmap', '$$',
    q{my ($self, @is) = @_;
      my (undef, $f, $p) = @$self;
      $f = fn $f;
      my @xs = parse $p, @is; @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()};

  defparser 'pcond', '$$',
    q{my ($self, @is) = @_;
      my (undef, $f, $p) = @$self;
      $f = fn $f;
      my @xs = parse $p, @is; @xs && &$f($_ = $xs[0]) ? @xs : ()};
}

defparserebnf pseq => fn q{
  my ($self, @ps) = @{$_[0]};
  "(\n" . join("\n", map indent(parser_ebnf $_, 2), @ps) . "\n)";
};

defparserebnf prep => fn q{
  my ($self, $p, $n) = (@{$_[0]}, 0);
  my $rep_symbol = $n == 0 ? '*' : $n == 1 ? '+' : "{$n+ times}";
  parser_ebnf($p) . "$rep_symbol";
};

defparserebnf popt => fn q{
  my ($self, $p) = @{$_[0]};
  parser_ebnf($p) . '?';
};

defparserebnf pmap => fn q{
  my ($self, $f, $p) = @{$_[0]};
  parser_ebnf($p) . " -> {$f}";
};

defparserebnf pcond => fn q{
  my ($self, $f, $p) = @{$_[0]};
  parser_ebnf($p) . " such that {$f}";
};

sub pn($@)
{ my ($n, @ps) = @_;
  'ARRAY' eq ref $n ? pmap fn "[\@\$_[" . join(',', @$n) . "]]", pseq @ps
                    : pmap fn "\$\$_[$n]", pseq @ps }

sub pc($) {pn 0, $_[0], popt pempty}

# Regex parsing.
# Consumes the match, returning either the matched text or the first match group
# you specify. Always matches from the beginning of a string.

BEGIN {
  defparser 'prx', '$',
    q{my ($self, $x, @xs) = @_;
      defined $x && $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()};

  defparser 'pstr', '$',
    q{my ($self, $x, @xs) = @_;
      defined $x && index($x, $$self[1]) == 0
        ? ($$self[1], substr($x, length $$self[1]), @xs)
        : ()};

  defparser 'pnx', '$',
    q{my ($self, $x, @xs) = @_;
      !defined $x || $x =~ /^(?:$$self[1])/ ? () : ($x, @xs)};
}

sub prc($)  {pn 0, prx  $_[0], popt pempty}
sub pstc($) {pn 0, pstr $_[0], popt pempty}

defparserebnf pstr => fn q{
  my ($self, $s) = @{$_[0]};
  $s =~ /'/ ? json_encode($s) : "'$s'";
};

defparserebnf prx => fn q{
  my ($self, $r) = @{$_[0]};
  "/$r/";
};

defparserebnf pnx => fn q{
  my ($self, $r) = @{$_[0]};
  "!/$r/";
};
84 core/boot/common.pl
# Regex parsing.
# Sometimes we'll have an operator that takes a regex, which is subject to the
# CLI reader problem the same way code arguments are. Rather than try to infer
# brackets the same way, we just require that regexes are terminated with /
# (which should be ok because that's also how they typically start).

BEGIN {defparseralias regex => pmap q{s/\/$//; $_}, prx qr{^(?:[^\\/]+|\\.)*/}}

docparser regex => q{Regular expression, delimited by slashes};


docparser generic_code => <<'_';
Counts brackets outside quoted strings, which in our case are '' and "".
Doesn't look for regular expressions because these vary by language; but this
parser should be able to handle most straightforward languages with quoted
string literals and backslash escapes.
_

defparser 'generic_code', '',
  q{my ($self, $code, @xs) = @_;
    return ($code, '', @xs) unless $code =~ /\]$/;
    (my $tcode = $code) =~ s/"([^"\\\\]+|\\\\.)"|'([^'\\\\]+|\\\\.)'//g;
    my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
    $balance ? (substr($code, 0, $balance), substr($code, $balance), @xs)
             : ($code, '', @xs)};


# Basic CLI types.
# Some common argument formats for various commands, sometimes transformed for
# specific cases. These are documented somewhere in `doc/`.

# A parsed column spec is an N-element array: [floor, cols...]. `floor` indicates
# the first column that would be selected by a `.` ("the rest").

BEGIN {defparseralias neval => pmap q{eval}, prx '=([^]=]+)'}
BEGIN {defparseralias integer => palt pmap(q{int},       neval),
                                      pmap(q{10 ** $_},  prx 'E(-?\d+)'),
                                      pmap(q{1 << $_},   prx 'B(\d+)'),
                                      pmap(q{0 + "0$_"}, prx 'x[0-9a-fA-F]+'),
                                      pmap(q{0 + $_},    prx '-?[1-9]\d*(?:[eE]\d+)?'),
                                                         pstr '0'}
BEGIN {defparseralias float => pmap q{0 + $_},
                               pcond q{length},
                               prx '-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:[eE][-+]?\d+)?'}
BEGIN {defparseralias number => palt neval, float, integer}

BEGIN {defparseralias colspec1      => palt pn(1, pstr '#', integer),
                                            pmap q{ord() - 65}, prx '[A-Z]';
       defparseralias colspec_rest  => pmap q{-1}, pstr '.'}
BEGIN {defparseralias colspec_range => pmap q{[$$_[0] .. $$_[2]]},
                                       pseq colspec1, pstr '-', colspec1}
BEGIN {defparseralias colspec_fixed => pmap q{[max(@$_) + 1, @$_]},
                                       pmap q{[map ref() ? @$_ : $_, @$_]},
                                       prep pn(1, popt pstr ',',
                                                  palt(colspec_range, colspec1)), 1}
BEGIN {defparseralias colspec => pmap q{[max(@$_) + 1, @$_]},
                                 pmap q{[map ref() ? @$_ : $_, @$_]},
                                 prep pn(1, popt pstr ',',
                                            palt(colspec_range, colspec1, colspec_rest)), 1}

docparser neval => q{An expression evaluated by Perl; e.g. =3+4 for 7};
docparser colspec1 => q{A way to identify a single column; either A-Z or #N};
docparser colspec_rest => <<'_';
"The rest of the columns": everything to the right of the rightmost
explicitly-specified column
_

docparser colspec_range => q{A range of columns, e.g. A-Q or #10-#20};
docparser colspec_fixed => q{A set of definite columns; disallows '.' ("the rest")};
docparser colspec => q{A set of columns, possibly including '.' ("the rest")};

# Filenames, in general.
# Typically filenames won't include bracket characters, though they might include
# just about everything else. Two possibilities there: if we need special stuff,
# there's the `file://` prefix; otherwise we assume the non-bracket
# interpretation.

BEGIN {defparseralias filename   => palt prx 'file://(.+)',
                                         prx '\.?/(?:[^/]|$)[^]]*',
                                         pcond q{-e}, prx '[^][]+'}
BEGIN {defparseralias nefilename => palt filename, prx '[^][]+'}

docparser filename   => q{The name of an existing file};
docparser nefilename => q{The name of a possibly-nonexisting file};
130 core/boot/cli.pl
# CLI grammar.
# ni's command line grammar uses some patterns on top of the parser combinator
# primitives defined in parse.pl.sdoc. Probably the most important one to know
# about is the long/short option dispatch, which looks like this:

# | option = alt @longs, dsp %shorts

our %contexts;
our %shorts;
our %longs;
our %long_refs;
our %short_refs;
our %dsps;
our %alts;

BEGIN {
  defdocumentable context => \%contexts, q{
    my ($c, $doc) = @_;
    doc_sections
      "CONTEXT $c" => $doc,
      "LONG OPERATORS (ni --doc/long X)"   => join("\n", sort grep /^$c\//, keys %ni::longs),
      "SHORT OPERATORS (ni --doc/short X)" => join("\n", sort grep /^$c\//, keys %ni::shorts);
  };

  defdocumentable short => \%shorts, q{
    my ($s, $doc) = @_;
    doc_sections
      "SHORT OPERATOR $s" => $doc,
      "SYNTAX" => parser_ebnf $ni::shorts{$s};
  };

  defdocumentable long => \%longs, q{
    my ($l, $doc) = @_;
    doc_sections
      "LONG OPERATOR $l" => $doc,
      "SYNTAX" => parser_ebnf $ni::longs{$l};
  };

  defdocumentable dsp => \%dsps, q{
    my ($d, $doc) = @_;
    doc_sections
      "EXTENSIBLE DISPATCH TABLE $d" => $doc,
      "OPTIONS" => parser_ebnf parser "dsp/$d";
  };

  defdocumentable alt => \%alts, q{
    my ($a, $doc) = @_;
    doc_sections
      "EXTENSIBLE LIST $a" => $doc,
      "OPTIONS" => parser_ebnf parser "alt/$a";
  };
}

sub defcontext($$) {
  my ($c, $doc) = @_;
  $short_refs{$c} = {};
  $long_refs{$c}  = ["$c/short"];
  $contexts{$c}   = paltr @{$long_refs{$c}};

  doccontext $c, $doc;

  defparseralias "$c/short",  pdspr %{$short_refs{$c}};
  defparseralias "$c/op",     $contexts{$c};
  defparseralias "$c/suffix", prep "$c/op";
  defparseralias "$c/series", prep pn 1, popt pempty, "$c/op", popt pempty;
  defparseralias "$c/lambda", pn 1, pstc '[', "$c/series", pstr ']';
  defparseralias "$c/qfn",    palt "$c/lambda", "$c/suffix";

  docparser "$c/short" => qq{Dispatch table for short options in context '$c'};
  docparser "$c/op" => qq{A single operator in the context '$c'};
  docparser "$c/suffix" => qq{A string of operators unbroken by whitespace};
  docparser "$c/series" => qq{A string of operators, possibly including whitespace};
  docparser "$c/lambda" => qq{A bracketed lambda function in context '$c'};
  docparser "$c/qfn" => qq{Operators that are interpreted as a lambda, whether bracketed or written as a suffix};
}

sub defshort($$) {
  my ($context, $dsp) = split /\//, $_[0], 2;
  warn "ni: defshort is redefining '$_[0]' (use rmshort to avoid this warning)"
    if exists $short_refs{$context}{$dsp};
  $shorts{$_[0]} = $short_refs{$context}{$dsp} = $_[1];
}

sub deflong($$) {
  my ($context, $name) = split /\//, $_[0], 2;
  unshift @{$long_refs{$context}}, $longs{$_[0]} = $_[1];
}

sub rmshort($) {
  my ($context, $dsp) = split /\//, $_[0], 2;
  delete $shorts{$_[0]};
  delete $short_refs{$context}{$dsp};
}

sub cli_parse(@) {parse parser '/series', @_}
sub cli(@) {
  my ($r, @rest) = cli_parse @_;
  die "ni: failed to parse starting here:\n  @rest" if @rest;
  $r;
}

# Extensible parse elements.
# These patterns come up a lot, and it's worth being able to autogenerate their
# documentation.

sub defalt($$@) {
  no strict 'refs';
  my ($name, $doc, @entries) = @_;
  my $vname = __PACKAGE__ . "::$name";
  docalt $name, $doc;
  @{$vname} = @entries;
  $alts{$name} = \@{$vname};
  *{__PACKAGE__ . "::def$name"} = sub ($) {unshift @{$vname}, $_[0]};
  my $r = paltr @{$vname};
  defparseralias "alt/$name" => $r;
  $r;
}

sub defdsp($$%) {
  no strict 'refs';
  my ($name, $doc, %entries) = @_;
  my $vname = __PACKAGE__ . "::$name";
  docdsp $name, $doc;
  %{$vname} = %entries;
  $dsps{$name} = \%{$vname};
  *{__PACKAGE__ . "::def$name"} = sub ($$) {${$vname}{$_[0]} = $_[1]};
  my $r = pdspr %{$vname};
  defparseralias "dsp/$name" => $r;
  $r;
}
81 core/boot/op.pl
# Operator definition.
# Like ni's parser combinators, operators are indirected using names. This
# provides an intermediate representation that can be inspected and serialized.

# Meta operators are applied before any pipeline forking happens, and are at
# liberty to modify anything to their left. (Or anything at all really, but it's
# counterintuitive for them to act rightwards.)

# Note that only toplevel meta operators are evaluated pre-pipeline. Meta
# operators inside lambdas are applied when the lambdas are evaluated (this is a
# feature).

our %operators;
our %meta_operators;

defdocumentable operator => \%operators, q{
  my ($op, $doc) = @_;
  doc_sections
    "OPERATOR $op"   => $doc,
    "IMPLEMENTATION" => $ni::operators{$op};
};

defdocumentable meta_operator => \%meta_operators, q{
  my ($mop, $doc) = @_;
  doc_sections
    "META OPERATOR $mop" => $doc,
    "IMPLEMENTATION" => $ni::meta_operators{$mop};
};

sub defmetaoperator($$) {
  my ($name, $f) = @_;
  die "ni: cannot redefine meta operator $name" if exists $meta_operators{$name};
  warn "ni: overlapping op/meta definition $name" if exists $operators{$name};
  $meta_operators{$name} = fn $f;
  ni::eval "sub ${name}_op(@) {['$name', \@_]}", "defmetaoperator $name";
}

sub meta_operate($$$) {
  my ($operator, $position, $context) = @_;
  my ($op, @args) = @$operator;
  my ($left, $right) = ([$position ? @$context[0..$position-1] : ()],
                        [@$context[$position+1..$#{$context}]]);
  my ($new_left, $new_right) = $meta_operators{$op}->([@args], $left, $right);
  (@{$new_left || $left}, @{$new_right || $right});
}

sub flatten_operators($);
sub flatten_operators($) {
  my ($name) = @{$_[0]};
  return $_[0] unless ref $name;
  map flatten_operators $_, @{$_[0]};
}

sub apply_meta_operators;
sub apply_meta_operators(@) {
  local $_;
  exists $meta_operators{$_[$_]->[0]}
    and return apply_meta_operators meta_operate $_[$_], $_, [@_]
    for 0..$#_;
  @_;
}

# Regular operators.
# Each of these is a filter process that is forked and piped against standard
# input. Operators act independently of one another.

sub defoperator($$) {
  my ($name, $f) = @_;
  die "ni: cannot redefine operator $name" if exists $operators{$name};
  warn "ni: overlapping op/meta definition $name" if exists $meta_operators{$name};
  $operators{$name} = fn $f;
  ni::eval "sub ${name}_op(@) {['$name', \@_]}", "defoperator $name";
  ni::eval "sub ${name}_run(@) {\$ni::operators{$name}->(\@_)}",
           "defoperator $name ($f)";
}

sub operate {
  my ($name, @args) = @_;
  die "ni operate: undefined operator: $name" unless exists $operators{$name};
  $operators{$name}->(@args);
}
61 core/boot/self.pl
# Image functions.
# ni needs to be able to reconstruct itself from a map. These functions implement
# the map commands required to do this.

our %self;

our $ni_map = 'core/boot/ni.map';

sub self_append_resource($$) {
  my ($k, $v) = @_;
  $self{$ni_map} .= "\nresource $k";
  $self{$k} = $v;
}

sub lib_entries($$) {
  local $_;
  my ($name, $text) = @_;
  map "$name/$_", grep {s/#.*//; length} split /\n/, $text;
}

sub quote_resource {my @xs; map sprintf("%d %s\n%s", scalar(@xs = split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub quote_library  {map quote_resource("$_/lib", lib_entries $_, $self{"$_/lib"}), @_}

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
  set $_, rfc $_ for lib_entries $l, ($self{"$l/lib"} = rfc "$l/lib");
}

sub modify_self() {
  die "ni: not a modifiable instance: $0" unless -w $0;
  open my $fh, "> $0" or die "ni: failed to open self: $!";
  print $fh read_map $ni_map;
  close $fh;
}

sub extend_self($$) {
  my ($type, $lib) = @_;
  intern_lib $lib;
  set $ni_map, "$self{$ni_map}\n$type $lib"
    unless grep /^(lib|ext)\s+$lib$/, split /\n/, $self{$ni_map};
}

sub image {read_map $ni_map}
sub image_with(%) {
  my %old_self = %self;
  my %h        = @_;
  $self{$ni_map} .= join '', map "\nresource $_", keys %h;
  @self{keys %h} = values %h;
  my $i = image;
  %self = %old_self;
  $i;
}
165 core/boot/main.pl
# CLI entry point.
# Some custom toplevel option handlers and the main function that ni uses to
# parse CLI options and execute the data pipeline.

our %cli_special;
BEGIN {defdocumentable 'clispecial', \%cli_special}

sub defclispecial($$$) {
  $cli_special{$_[0]} = fn $_[1];
  docclispecial $_[0], $_[2];
}

# Development options.
# Things useful for developing ni.

defclispecial '--dev/eval', q{print ni::eval($_[0], "anon $_[0]"), "\n"}, <<'_';
Development option: evaluate an expression within the `ni::` package, and print
the result. For example:

$ ni --dev/eval '3 + 4'
7
_

defclispecial '--dev/parse', q{dev_trace 'ni::parse'; cli_parse @_}, <<'_';
Development option: trace the ni::parse function and attempt to parse the
command line. Mostly useful for debugging.
_

defclispecial '--dev/parse-one', q{
  dev_trace 'ni::parse';
  parse ni::eval($_[0]), @_[1..$#_];
}, <<'_';
Development option: trace the ni::parse function and evaluate the specified
parser on the rest of the command line arguments. For example:

$ ni --dev/parse-one 'parser "/qfn"' [gc]
_

defclispecial '--dev/doc-check' => q{
  my $undocumented = 0;
  for my $type (sort keys %ni::doc_entities) {
    exists $ni::doc{$type}{$_} or ++$undocumented && print "$type\t$_\n"
      for sort keys %{$ni::doc_entities{$type}};
  }
  print "$undocumented undocumented thing(s)\n";
  !!$undocumented;
}, <<'_';
Development option: find things (parsers, ops, etc) that have been defined but
have no documentation associated with them.
_

# Extensions.
# Options to extend and modify the ni image.

defclispecial '--internal/lib', q{
  extend_self 'lib', $_ for @_;
  modify_self;
}, <<'_';
Usage: ni --internal/lib lib1 lib2 ... libN
Modifies the ni image in-place to include the specified libraries. See ni
//help/libraries for more information.
_

defclispecial '--lib', q{intern_lib shift; goto \&main}, <<'_';
Usage: ni --lib lib-dir normal-ni-options...
Preloads a library before running normally. ni does not self-modify on disk if
you do this, though the library will be available in remote contexts.
_

defclispecial '--run', q{
  $ni::self{"transient/eval"} .= "\n$_[0]\n";
  ni::eval $_[0], "--run $_[0]";
  shift;
  goto \&main;
}, <<'_';
Usage: ni --run 'perl code' normal-ni-options...
Runs code before running normally.
_

# Documentation.

defclispecial '--explain', q{
  my ($r, @rest) = cli_parse @_;
  print "UNPARSED: @rest\n" if @rest;
  if (ref $r) {
    print json_encode($_), "\n" for flatten_operators $r;
  }
}, <<'_';
Usage: ni --explain normal-ni-options...
Describes the operators and meta-operators produced from the specified command
line. Meta-operators are unevaluated in this form.
_

defclispecial '--explain-meta', q{
  my ($r, @rest) = cli_parse @_;
  print "UNPARSED: @rest\n" if @rest;
  if (ref $r) {
    print json_encode($_), "\n" for apply_meta_operators flatten_operators $r;
  }
}, <<'_';
Usage: ni --explain-meta normal-ni-options...
Describes the operators produced from the specified command line, after
evaluating all meta-operators. Each operator in the output corresponds to a
forked process in the pipeline.
_

defclispecial "--doc/$_", qq{
  if (\@_) {
    eval {print ${_}_doc \$_, "\\n"}, \$@ && warn \$@ for \@_;
  } else {
    print "\$_\\n" for sort keys \%{\$ni::doc_entities{'$_'}};
  }
}, <<_
Usage: ni --doc/$_ [<${_}-name>]
Print documentation for <${_}-name> if specified; otherwise list all ${_}s.
_
for keys %ni::doc;

defclispecial '--doc', q{print "--doc/$_\n" for sort keys %ni::doc}, <<'_';
Usage: ni --doc
Prints a list of all documentable types of things. For example, "parser" is one
such thing, and from there you can run `ni --doc/parser X`, where `X` is the
name of a parser. (`ni --doc/parser` will list all of them.)
_

# Root CLI context.
# This is used by extensions that define long and short options.

defcontext '', q{toplevel CLI context};

# Main stuff.
# sub main() is called by the ni boot header on @ARGV. I've separated
# $main_operator so it can be extended to handle various cases; for instance, ni
# launches a pager when its output is connected to a terminal, etc. This is
# handled by core/stream.

our $main_operator = sub {die "ni: no main operator defined (your ni is broken)"};

sub main {
  my ($cmd, @args) = @_;
  $ni::is_toplevel = 1;

  @_ = ('//help', @_[1..$#_])
    if -t STDIN and -t STDOUT and !@_ || $_[0] =~ /^-h$|^-\?$|^--help$/;

  if (exists $ENV{HOME} && !exists $ENV{NI_NO_HOME} && -d "$ENV{HOME}/.ni") {
    eval {intern_lib "$ENV{HOME}/.ni"};
    if ($@) {
      print STDERR "ni: note: failed to load ~/.ni as a library: $@\n";
      print STDERR "    (export NI_NO_HOME to disable ~/.ni autoloading,\n";
      print STDERR "     or run `ni //help/libraries` for details about libraries)\n";
    }
  }

  return $cli_special{$cmd}->(@args) if defined $cmd && exists $cli_special{$cmd};

  my ($r, @rest) = cli_parse @_;
  return &$main_operator(flatten_operators $r) if !@rest && ref $r;

  print STDERR "ni: failed to parse starting here (ni --dev/parse to trace):\n";
  print STDERR "  @rest\n";
  print STDERR "If ni is failing to parse a filename, start it with /, ./,\n";
  print STDERR "or file:// to qualify it.\n";
  exit 1;
}
1 core/gen/lib
gen.pl
34 core/gen/gen.pl
# Code generator.
# A general-purpose interface to do code-generation stuff. This is used when
# you've got a task that's mostly boilerplate of some kind, but you've got
# variable regions. For example, if you wanted to generalize JVM-hosted
# command-line filters:

# | my $java_linefilter = gen q{
#     import java.io.*;
#     public class %classname {
#       public static void main(String[] args) {
#         BufferedReader stdin = <the ridiculous crap required to do this>;
#         String %line;
#         while ((%line = stdin.readLine()) != null) {
#           %body;
#         }
#       }
#     }
#   };
#   my $code = &$java_linefilter(classname => 'Foo',
#                                line      => 'line',
#                                body      => 'System.out.println(line);');

our $gensym_index = 0;
sub gensym {join '_', '_gensym', ++$gensym_index, @_}

sub gen($) {
  my @pieces = split /(%\w+)/, $_[0];
  sub {
    my %vars = @_;
    my @r = @pieces;
    $r[$_] = $vars{substr $pieces[$_], 1} for grep $_ & 1, 0..$#pieces;
    join '', map defined $_ ? $_ : '', @r;
  };
}
1 core/json/lib
json.pl
77 core/json/json.pl
# JSON parser/generator.
# Perl has native JSON libraries available in CPAN, but we can't assume those are
# installed locally. The pure-perl library is unusably slow, and even it isn't
# always there. So I'm implementing an optimized pure-Perl library here to
# address this. Note that this library doesn't parse all valid JSON, but it does
# come close -- and I've never seen a real-world use case of JSON that it would
# fail to parse.

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

# Fully decode a string of JSON. Unless you need to extract everything, this is
# probably the slowest option; targeted attribute extraction should be much
# faster.

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

# Encode a string of JSON from a structured value. TODO: add the ability to
# generate true/false values.

our %json_escapes = map {;$json_unescapes{$_} => $_} keys %json_unescapes;

sub json_escape($) {
  (my $x = $_[0]) =~ s/([\b\f\n\r\t"\\])/"\\" . $json_escapes{$1}/eg;
  "\"$x\"";
}

sub json_encode($);
sub json_encode($) {
  local $_;
  my ($v) = @_;
  return "[" . join(',', map json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
  return "{" . join(',', map json_escape($_) . ":" . json_encode($$v{$_}),
                             sort keys %$v) . "}" if 'HASH' eq ref $v;
  return json_escape $$v if 'SCALAR' eq ref $v;   # force string
  looks_like_number $v ? $v : defined $v ? json_escape $v : 'null';
}

if (__PACKAGE__ eq 'ni::pl') {
  *je = \&json_encode;
  *jd = \&json_decode;
}
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
1 core/conf/lib
conf.pl
57 core/conf/conf.pl
# Configuration variables.
# These can be specified as environment vars or overridden locally for specific
# operations.

our %conf_variables;
our %conf_defaults;

sub conf($) {
  die "ni: nonexistent configuration variable $_[0] (ni //ni/conf to list)"
    unless exists $conf_variables{$_[0]};
  dor $conf_variables{$_[0]}->(), $conf_defaults{$_[0]};
}

sub conf_set($$) {
  die "ni: nonexistent configuration variable $_[0] (ni //ni/conf to list)"
    unless exists $conf_variables{$_[0]};
  $conf_variables{$_[0]}->($_[1]);
}

defoperator conf_get => q{
  my ($name) = @_;
  sio();
  print conf $name, "\n";
};

sub defconf($$) {
  $conf_variables{$_[0]} = fn $_[1];
  defshort '/$' . $_[0], pmap qq{conf_get_op '$_[0]'}, pnone;
}

sub conf_env {
  my ($name, $v) = @_;
  $ENV{$name} = $v if @_ == 2;
  $ENV{$name};
}

sub defconfenv($$$) {
  my ($name, $env, $v) = @_;
  defconf $name, qq{conf_env '$env', \@_};
  $conf_defaults{$name} = $v;
}

defoperator configure => q{
  my ($vars, $f) = @_;
  conf_set $_, $$vars{$_} for keys %$vars;
  &$ni::main_operator(@$f);
};

BEGIN {defparseralias config_map_key   => prx '[^=]+';
       defparseralias config_map_value => prc '.*[^}]+|'}
BEGIN {defparseralias config_map_kv    => pn [0, 2], config_map_key, pstr '=',
                                                     config_map_value}
BEGIN {defparseralias config_option_map
         => pmap q{my %h; $h{$$_[0]} = $$_[1] for @{$_[0]}; \%h},
            pn 0, prep(config_map_kv), prc '}'}

defshort '/^{', pmap q{configure_op @$_}, pseq config_option_map, _qfn;
6 core/stream/lib
fh.pl
procfh.pl
pipeline.pl
self.pl
ops.pl
main.pl
9 core/stream/fh.pl
# Filehandle functions.

use Fcntl qw/:DEFAULT/;

sub fh_nonblock($) {
  my ($fh) = @_;
  my $flags = fcntl $fh, F_GETFL, 0       or die "ni: fcntl get $fh: $!";
  fcntl $fh, F_SETFL, $flags | O_NONBLOCK or die "ni: fcntl set $fh: $!";
}
98 core/stream/procfh.pl
# Process + filehandle combination.
# We can't use Perl's process-aware FHs because they block-wait for the process
# on close. There are some situations where we care about the exit code and
# others where we don't, and this class supports both cases.

package ni::procfh;

use POSIX qw/:sys_wait_h/;

# Global child collector.
# Collect children regardless of whether anyone is listening for them. If we have
# an interested party, notify them.

our %child_owners;

sub await_children {
  local ($!, $?, $_);
  while (0 < ($_ = waitpid -1, WNOHANG)) {
    $child_owners{$_}->child_exited($?) if defined $child_owners{$_};
  }
  $SIG{CHLD} = \&await_children;
};
$SIG{CHLD} = \&await_children;

# Signal forwarding.
# Propagate termination signals to children and run finalizers. This makes it so
# that non-writing pipelines like `ni n100000000gzn` still die out without
# waiting for the SIGPIPE.

sub kill_children($) {kill $_[0], keys %child_owners}

# Proc-filehandle class.
# Overloading *{} makes it possible for this to act like a real filehandle in
# every sense: fileno() works, syswrite() works, etc. The constructor takes care
# of numeric fds by promoting them into Perl fh references.

use overload qw/*{} fh "" str/;
sub new($$$) {
  my ($class, $fd, $pid) = @_;
  my $result = bless {pid => $pid, status => undef}, $class;
  $child_owners{$pid} = $result;
  my $fh = ref($fd) ? $fd : undef;
  open $fh, "<&=$fd" or die "ni: procfh($fd, $pid) failed: $!"
    unless defined $fh;
  $$result{fh} = $fh;
  $result;
}

sub DESTROY {
  my ($self) = @_;
  close $$self{fh};
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

# Child await.
# We have to stop the SIGCHLD handler while we wait for the child in question.
# Otherwise we run the risk of waitpid() blocking forever or catching the wrong
# process. This ends up being fine because this process can't create more
# children while waitpid() is waiting, so we might have some resource delays but
# we won't have a leak.

# We also don't have to worry about multithreading: only one await() call can
# happen per process.

sub await($) {
  local ($?, $SIG{CHLD});
  my ($self) = @_;
  return $$self{status} if defined $$self{status};
  $SIG{CHLD} = 'DEFAULT';
  return $$self{status} if defined $$self{status};
  if (kill 'ZERO', $$self{pid}) {
    my $pid = waitpid $$self{pid}, 0;
    $self->child_exited($pid <= 0 ? "-1 [waitpid: $!]" : $?);
  } else {
    $self->child_exited("-1 [child already collected]");
  }
  $$self{status};
}

sub child_exited($$) {
  my ($self, $status) = @_;
  $$self{status} = $status;
  delete $child_owners{$$self{pid}};
}
234 core/stream/pipeline.pl
# Pipeline construction.
# A way to build a shell pipeline in-process by consing a transformation onto
# this process's standard input. This will cause a fork to happen, and the forked
# PID is returned.

# I define some system functions with a `c` prefix: these are checked system
# calls that will die with a helpful message if anything fails.

no warnings 'io';

use Errno qw/EINTR/;
use POSIX qw/dup2/;

sub cdup2 {defined dup2 $_[0], $_[1] or die "ni: dup2(@_) failed: $!"}
sub cpipe {pipe $_[0], $_[1] or die "ni: pipe failed: $!"}
sub cfork {my $pid = fork; die "ni: fork failed: $!" unless defined $pid; $pid}

sub sh($) {exec 'sh', '-c', $_[0]}

sub nuke_stdin() {
  open STDIN, '</dev/null';
  if (fileno STDIN) {
    cdup2 fileno STDIN, 0;
    open STDIN, '<&=0';
  }
}

# Safe reads/writes.
# This is required because older versions of Perl don't automatically retry
# interrupted reads/writes. We run the risk of interruption because we have a
# SIGCHLD handler. nfu lost data on older versions of Perl because it failed to
# handle this case properly.

sub saferead($$$;$) {
  my $n;
  do {
    return $n if defined($n = sysread $_[0], $_[1], $_[2], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

sub safewrite($$;$$) {
  my $n;
  do {
    return $n if defined($n = syswrite $_[0], $_[1], $_[2] || length $_[1], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

sub saferead_exactly($$$;$) {
  my ($r, $n) = (0, 0);
  while ($r < $_[2]) {
    return undef unless $n = saferead $_[0], $_[1], $_[2] - $r, ($_[3] || 0) + $r;
    $r += $n;
  }
  $r;
}

sub safewrite_exactly($$) {
  my ($w, $n) = (0, 0);
  while ($w < length $_[1]) {
    return undef unless $n = safewrite $_[0], $_[1], length($_[1]) - $w, $w;
    $w += $n;
  }
  $w;
}

# Process construction.
# A few functions, depending on what you want to do:

# | siproc(&): fork into block, return pipe FH to block's STDIN.
#   soproc(&): fork into block, return pipe FH from block's STDOUT.
#   sicons(&): fork into block, connect its STDOUT to our STDIN.
#   socons(&): fork into block, connect our STDOUT to its STDIN.

# NOTE: forkopen does something strange and noteworthy. You'll notice that it's
# reopening STDIN and STDOUT from FDs, which seems redundant. This is required
# because Perl filehandles aren't the same as OS-level file descriptors, and ni
# deals with both in different ways.

# In particular, ni closes STDIN (the filehandle) if the input comes from a
# terminal, since presumably the user doesn't intend to type their input in
# manually. This needs to happen before any exec() from a forked filter process.
# But this creates a problem: if we later reactivate fd 0, which we do by moving
# file descriptors from a pipe. We have to do this at the fd level so exec()
# works correctly (since exec doesn't know anything about Perl filehandles, just
# fds). Anyway, despite the fact that fd 0 is newly activated by an sicons {}
# operation, Perl's STDIN filehandle will think it's closed and return no data.

# So that's why we do these redundant open STDIN and STDOUT operations. At some
# point I might bypass Perl's IO layer altogether and use POSIX calls, but at the
# moment that seems like more trouble than it's worth.

sub forkopen {
  my ($fd, $f, @args) = @_;
  cpipe my $r, my $w;
  my ($ret, $child) = ($r, $w)[$fd ^ 1, $fd];
  my $pid;
  if ($pid = cfork) {
    close $child;
    return ni::procfh->new($ret, $pid);
  } else {
    close $ret;
    cdup2 fileno $child, $fd;
    close $child;
    open STDIN,  '<&=0' if $fd == 0;
    open STDOUT, '>&=1' if $fd == 1;
    &$f(@args);
    exit;
  }
}

sub sioproc(&@) {
  my ($f, @args) = @_;
  cpipe my $proc_in, my $w;
  cpipe my $r, my $proc_out;
  my $pid;
  if ($pid = cfork) {
    close $proc_in;
    close $proc_out;
    return ($w, ni::procfh->new($r, $pid));
  } else {
    close $w;
    close $r;
    cdup2 fileno $proc_in, 0;
    cdup2 fileno $proc_out, 1;
    close $proc_in;
    close $proc_out;
    open STDIN,  '<&=0';
    open STDOUT, '>&=1';
    &$f(@args);
    exit;
  }
}

sub siproc(&@) {forkopen 0, @_}
sub soproc(&@) {forkopen 1, @_}

sub sicons(&@) {
  my ($f, @args) = @_;
  my $fh = soproc {&$f(@args)};
  cdup2 fileno $fh, 0;
  close $fh;
  open STDIN, '<&=0';
  $fh;
}

sub socons(&@) {
  my ($f, @args) = @_;
  my $fh = siproc {&$f(@args)};
  cdup2 fileno $fh, 1;
  close $fh;
  open STDOUT, '>&=1';
  $fh;
}

# Stream functions.
# These are called by pipelines to simplify things. For example, a common
# operation is to append the output of some data-producing command:

# | $ ni . .              # lists current directory twice

# If you do this, ni will create a pipeline that uses stream wrappers to
# concatenate the second `ls` output (despite the fact that technically it's a
# shell pipe).

sub sforward($$) {local $_; safewrite_exactly $_[1], $_ while saferead $_[0], $_, 8192}
sub stee($$$)    {local $_; safewrite_exactly($_[1], $_), safewrite_exactly($_[2], $_) while saferead $_[0], $_, 8192}
sub sio()        {sforward \*STDIN, \*STDOUT}

sub srfile($) {open my $fh, '<', $_[0] or die "ni: srfile $_[0]: $!"; $fh}
sub swfile($) {open my $fh, '>', $_[0] or die "ni: swfile $_[0]: $!"; $fh}

# Compressed stream support.
# This provides a stdin filter you can use to read the contents of a compressed
# stream as though it weren't compressed. It's implemented as a filter process so
# we don't need to rely on file extensions.

# We detect the following file formats:

# | gzip:  1f 8b
#   bzip2: BZh[1-9]    (sometimes \0 instead of the digit)
#   lzo:   89 4c 5a 4f
#   lz4:   04 22 4d 18
#   xz:    fd 37 7a 58 5a

# Decoding works by reading enough to decode the magic, then forwarding data
# into the appropriate decoding process (or doing nothing if we don't know what
# the data is).

sub sdecode(;$) {
  local $_;
  return unless saferead \*STDIN, $_, 8192;

  my $decoder = /^\x1f\x8b/             ? "gzip -dc || cat"
              : /^BZh[1-9\0]/           ? "pbzip2 -dc || bzip2 -dc || cat"
              : /^\x89\x4c\x5a\x4f/     ? "lzop -dc || cat"
              : /^\x04\x22\x4d\x18/     ? "lz4 -dc || cat"
              : /^\xfd\x37\x7a\x58\x5a/ ? "xz -dc || cat" : undef;

  if (defined $decoder) {
    my $o = siproc {exec $decoder};
    safewrite_exactly $o, $_;
    sforward \*STDIN, $o;
    close $o;
    $o->await;
  } else {
    safewrite_exactly \*STDOUT, $_;
    sio;
  }
}

# File/directory cat.
# cat exists to turn filesystem objects into text. Files are emitted and
# directories are turned into readable listings. Files are automatically
# decompressed and globs are automatically deglobbed.

sub glob_expand($) {-e($_[0]) ? $_[0] : glob $_[0]}

sub scat {
  local $| = 1;
  for my $f (map glob_expand($_), @_) {
    if (-d $f) {
      opendir my $d, $f or die "ni_cat: failed to opendir $f: $!";
      print "$f/$_\n" for sort grep !/^\.\.?$/, readdir $d;
      closedir $d;
    } else {
      my $d = siproc {sdecode};
      sforward srfile $f, $d;
      close $d;
      $d->await;
    }
  }
}
101 core/stream/self.pl
# Self invocation.
# You can run ni and read from the resulting file descriptor; this gives you a
# way to evaluate lambda expressions (this is how checkpoints work, for example).
# If you do this, ni's standard input will come from a continuation of __DATA__.

use Errno qw/EINTR/;

our @quoted_resources;
our %non_propagated_env_vars;

sub defnonpropagatedenv {@non_propagated_env_vars{@_} = map 1, @_}

# TMPDIR is notoriously non-portable and shouldn't be forwarded at all.
defnonpropagatedenv 'TMPDIR';

sub quoted_resources()     {@quoted_resources}
sub add_quoted_resource($) {push @quoted_resources, $_[0]}

sub safereadbuf($$$;$) {
  my $n;
  do {
    return $n if defined($n = read $_[0], $_[1], $_[2], $_[3] || 0);
  } while $!{EINTR};
  return undef;
}

sub safereadbuf_exactly($$$;$) {
  my ($r, $n) = (0, 0);
  while ($r < $_[2]) {
    return undef unless $n = safereadbuf $_[0], $_[1], $_[2] - $r, ($_[3] || 0) + $r;
    $r += $n;
  }
  $r;
}

defclispecial '--internal/operate-quoted', q{
  my $parent_env = json_decode $ni::self{'quoted/env'};
  exists $ENV{$_} or $ENV{$_} = $$parent_env{$_} for keys %$parent_env;

  sforward_buf_unquoted $ni::data, resource_write($_)
    for @{json_decode $ni::self{'quoted/resources'}};

  $ni::is_toplevel = 0;
  my $fh = siproc {
    &$ni::main_operator(flatten_operators json_decode $ni::self{'quoted/op'});
  };
  safewrite $fh, $_ while safereadbuf $ni::data, $_, 8192;
  close $fh;
  $fh->await;
}, <<'_';
Internal option: causes ni to parse keys within its image and use those as the
list of operators to run. This is how all of ni's remoting is done; the
indirection prevents us from hitting any size limits on ARGV or ENV.
_

sub sforward_quoted($$) {
  my ($n, $b);
  safewrite $_[1], pack 'na*', $n, $b while $n = saferead $_[0], $b, 8192;
  safewrite $_[1], pack 'n', 0;
}

sub sforward_buf_unquoted($$) {
  my ($n, $nb, $b, $eof) = (0, '', '', 0);
  while (!$eof and safereadbuf_exactly $_[0], $nb, 2 and ($n) = unpack 'n', $nb) {
    $b = '';
    $eof ||= !safereadbuf $_[0], $b, $n - length($b), length $b
      until $eof or length($b) >= $n;
    safewrite $_[1], $b;
  }
}

sub ni_quoted_exec_args() {qw|perl - --internal/operate-quoted|}

sub ni_quoted_image($@) {
  my ($include_quoted_resources, @args) = @_;
  my @env_keys = grep !$non_propagated_env_vars{$_}, keys %ENV;
  my %reduced_env;
  @reduced_env{@env_keys} = @ENV{@env_keys};
  image_with
    'quoted/op'        => json_encode [@args],
    'quoted/env'       => json_encode {%reduced_env},
    'quoted/resources' => json_encode($include_quoted_resources
                                        ? [@quoted_resources]
                                        : []);
}

sub quote_ni_into($@) {
  my ($fh, @args) = @_;
  safewrite $fh, ni_quoted_image 1, @args;
  sforward_quoted resource_read($_), $fh for @quoted_resources;
  sforward \*STDIN, $fh;
  close $fh;
  $fh->await;
}

sub exec_ni(@) {
  my $ni = siproc {exec ni_quoted_exec_args};
  quote_ni_into $ni, @_;
}

sub sni(@) {soproc {nuke_stdin; exec_ni @_} @_}
265 core/stream/ops.pl
# Streaming data sources.
# Common ways to read data, most notably from files and directories. Also
# included are numeric generators, shell commands, etc.

BEGIN {
  defparseralias multiword    => pn 1, prx '\[',  prep(prc '[\s\S]*[^]]', 1), prx '\]';
  defparseralias multiword_ws => pn 1, prc '\[$', prep(pnx '\]$',         1), prx '\]$';

  defparser 'super_brackets', '', q{
    my ($self, @xs) = @_;
    return () unless $xs[0] =~ s/^(\^[^[]*)\[//;
    my $superness = $1;
    my @r;
    push @r, shift @xs while @xs && $xs[0] !~ s/^(\Q$superness\E)\]//;
    $1 eq $superness ? (\@r, @xs) : ();
  };
}
BEGIN {
  defparseralias shell_command => palt pmap(q{shell_quote @$_}, super_brackets),
                                       pmap(q{shell_quote @$_}, multiword_ws),
                                       pmap(q{shell_quote @$_}, multiword),
                                       prx '[^][]+';
  defparseralias id_text => palt pmap(q{join "\t", @$_}, super_brackets),
                                 pmap(q{join "\t", @$_}, multiword_ws),
                                 pmap(q{join "\t", @$_}, multiword),
                                 prx '[^][]+';
}

defoperator echo => q{my ($x) = @_; sio; print "$x\n"};
defoperator sh   => q{my ($c) = @_; sh $c};

defshort '/e', pmap q{sh_op $_}, shell_command;

# Cat meta-operator.
# We don't want 'cat' to be a regular operator because of how shell wildcards
# work. If you say something like `ni *`, it's going to get a bunch of filenames,
# each of which will fork out to another ni process. Most of these ni processes
# will just be copying stdin to stdout, a huge waste if there are a lot of files.

# We get around this by making `cat` a meta-operator that merges adjacent cat
# operations into a single `cat_multi`.

defoperator cat_multi => q{sio; scat $_ for @_};

defmetaoperator cat => q{
  my ($args, $left, $right) = @_;
  my ($f) = @$args;
  my $i = -1;
  ++$i while $i+1 < @$right && $$right[$i+1][0] eq 'cat';
  ($left, [cat_multi_op($f, $i > -1 ? map $$_[1], @$right[0..$i] : ()),
           @$right[$i+1..$#{$right}]]);
};

docparser multiword => <<'_';
A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
metacharacters within the arguments won't be expanded). If you use this form,
no ARGV entry can end in a closing bracket; otherwise ni will assume you wanted
to close the list.
_

docparser multiword_ws => <<'_';
A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
metacharacters within the arguments won't be expanded). Whitespace is required
around both brackets.
_

docparser shell_command => q{A quoted or bracketed shell command};

docoperator cat  => q{Append contents of a file or resource};
docoperator echo => q{Append text verbatim};
docoperator sh   => q{Filter stream through a shell command};

# Note that we generate numbers internally rather than shelling out to `seq`
# (which is ~20x faster than Perl for the purpose, incidentally). This is
# deliberate: certain versions of `seq` generate floating-point numbers after a
# point, which can cause unexpected results and loss of precision.

defoperator n => q{
  my ($l, $u) = @_;
  sio; for (my $i = $l; $u < 0 || $i < $u; ++$i) {print "$i\n"};
};

docoperator n => q{Append consecutive integers within a range};

defshort '/n',  pmap q{n_op 1, defined $_ ? $_ + 1 : -1}, popt number;
defshort '/n0', pmap q{n_op 0, defined $_ ? $_ : -1}, popt number;
defshort '/i',  pmap q{echo_op $_}, id_text;

defshort '/1', pmap q{n_op 1, 2}, pnone;

deflong '/fs', pmap q{cat_op $_}, filename;

docshort '/n' => q{Append integers 1..N, or 1..infinity if N is unspecified};
docshort '/n0' => q{Append integers 0..N-1, or 0..infinity if N is unspecified};
docshort '/i' => q{Identity: append literal text};
docshort '/e' => q{Exec shell command as a filter for the current stream};

docshort '/1' => q{Alias for 'n1'};

doclong '/fs' => q{Append things that appear to be files};

# Stream mixing/forking.
# Append, prepend, divert.

defoperator append => q{my @xs = @_; sio; exec_ni @xs};
docoperator append => q{Append another ni stream to this one};

defoperator prepend => q{
  my @xs = @_;
  close(my $fh = siproc {exec_ni @xs});
  $fh->await;
  sio;
};
docoperator prepend => q{Prepend a ni stream to this one};

defoperator sink_null => q{1 while saferead \*STDIN, $_, 8192};
docoperator sink_null => q{Consume stream and produce nothing};

defoperator divert => q{
  my @xs = @_;
  my $fh = siproc {close STDOUT; exec_ni @xs, sink_null_op};
  stee \*STDIN, $fh, \*STDOUT;
  close $fh;
  $fh->await;
};
docoperator divert => q{Duplicate this stream into a ni pipeline, discarding that pipeline's output};

defshort '/+', pmap q{append_op    @$_}, _qfn;
defshort '/^', pmap q{prepend_op   @$_}, _qfn;
defshort '/=', pmap q{divert_op    @$_}, _qfn;

# Interleaving.
# Append/prepend will block one of the two data sources until the other
# completes. Sometimes, though, you want to stream both at once. Interleaving
# makes that possible, and you can optionally specify the mixture ratio, which is
# the number of interleaved rows per input row. (Negative numbers are interpreted
# as reciprocals, so -2 means two stdin rows for every interleaved.)

defoperator interleave => q{
  my ($ratio, $lambda) = @_;
  my $fh = soproc {close STDIN; exec_ni @$lambda};

  if ($ratio) {
    $ratio = 1/-$ratio if $ratio < 0;
    my ($n1, $n2) = (0, 0);
    while (1) {
      ++$n1, defined($_ = <STDIN>) || goto done, print while $n1 <= $n2 * $ratio;
      ++$n2, defined($_ = <$fh>)   || goto done, print while $n1 >= $n2 * $ratio;
    }
  } else {
    my $rmask;
    my ($stdin_ok,  $ni_ok) = (1, 1);
    my ($stdin_buf, $ni_buf);
    while ($stdin_ok || $ni_ok) {
      vec($rmask, fileno STDIN, 1) = $stdin_ok;
      vec($rmask, fileno $fh,   1) = $ni_ok;
      my $n = select my $rout = $rmask, undef, undef, 0.01;
      if (vec $rout, fileno STDIN, 1) {
        $stdin_ok = !!saferead \*STDIN, $stdin_buf, 1048576, length $stdin_buf;
        my $i = 1 + rindex $stdin_buf, "\n";
        if ($i) {
          safewrite \*STDOUT, substr $stdin_buf, 0, $i;
          $stdin_buf = substr $stdin_buf, $i;
        }
      }
      if (vec $rout, fileno $fh, 1) {
        $ni_ok = !!saferead $fh, $ni_buf, 1048576, length $ni_buf;
        my $i = 1 + rindex $ni_buf, "\n";
        if ($i) {
          safewrite \*STDOUT, substr $ni_buf, 0, $i;
          $ni_buf = substr $ni_buf, $i;
        }
      }
    }
  }

  done:
  close $fh;
  $fh->await;
};

defshort '/%', pmap q{interleave_op @$_}, pseq popt number, _qfn;

# Sinking.
# We can sink data into a file just as easily as we can read from it. This is
# done with the `>` operator, which is typically written as `\>`. The difference
# between this and the shell's > operator is that \> outputs the filename; this
# lets you invert the operation with the nullary \< operator.

defoperator file_read  => q{chomp, weval q{scat $_} while <STDIN>};
defoperator file_write => q{
  my ($file) = @_;
  $file = resource_tmp('file://') unless defined $file;
  sforward \*STDIN, swfile $file;
  print "$file\n";
};

defshort '/>', pmap q{file_write_op $_}, nefilename;
defshort '/<', pmap q{file_read_op},     pnone;

defoperator file_prepend_name_read => q{
  my $file;
  while (defined($file = <STDIN>))
  {
    chomp $file;
    my $fh = srfile $file;
    print "$file\t$_" while <$fh>;
  }
};

defshort '/W<', pmap q{file_prepend_name_read_op}, pnone;

defoperator file_prepend_name_write => q{
  my $file = undef;
  my $fh   = undef;
  while (<STDIN>)
  {
    my ($fname, $l) = split /\t/, $_, 2;
    defined($file) && print("$file\n"), $fh = swfile($file = $fname)
      if !defined($file) or $fname ne $file;
    print $fh $l;
  }
  print "$file\n";
};

defshort '/W>', pmap q{file_prepend_name_write_op}, pnone;

# Resource stream encoding.
# This makes it possible to serialize a directory structure into a single stream.
# ni uses this format internally to store its k/v state.

defoperator encode_resource_stream => q{
  my @xs;
  while (<STDIN>) {
    chomp;
    my $s = rfc $_;
    my $line_count = @xs = split /\n/, "$s ";
    print "$line_count $_\n", $s, "\n";
  }
};

defshort '/>\'R', pmap q{encode_resource_stream_op}, pnone;

# Compression and decoding.
# Sometimes you want to emit compressed data, which you can do with the `Z`
# operator. It defaults to gzip, but you can also specify xz, lzo, lz4, or bzip2
# by adding a suffix. You can decode a stream in any of these formats using `ZD`
# (though in most cases ni will automatically decode compressed formats).

our %compressors = qw/ g gzip  x xz  o lzop  4 lz4  b bzip2 /;

BEGIN {defparseralias compressor_name => prx '[gxo4b]'}
BEGIN {
  defparseralias compressor_spec =>
    pmap q{my ($c, $level) = @$_;
           $c = $ni::compressors{$c || 'g'};
           defined $level ? sh_op "$c -$level" : sh_op $c},
    pseq popt compressor_name, popt integer;
}

defoperator decode => q{sdecode};

defshort '/z',  compressor_spec;
defshort '/zn', pk sink_null_op();
defshort '/zd', pk decode_op();
86 core/stream/main.pl
use POSIX ();

our $pager_fh;
our $handling_pipe;

defconfenv 'pager', NI_PAGER => 'less -u';

sub child_status_ok($) {
  $_[0] == 0                    # ok exit status
    or ($_[0] & 127) == 13      # killed by sigpipe
    or ($_[0] & 127) == 15      # killed by sigterm (probably from us)
}

$ni::main_operator = sub {
  my @children;

  # Awkward TMPDIR fix for mac OSX
  delete $ENV{TMPDIR} unless -d $ENV{TMPDIR} and -w $ENV{TMPDIR};

  if (-t STDIN) {
    nuke_stdin;
  } else {
    # Fix for bugs/2016.0918.replicated-garbage.md: forcibly flush the STDIN
    # buffer so no child process gets bogus data.
    cdup2 0, 3;
    close STDIN;
    cdup2 3, 0;
    POSIX::close 3;
    open STDIN, '<&=0' or die "ni: failed to reopen STDIN: $!";

    push @children, sicons {sdecode};
  }

  my @ops = apply_meta_operators @_;
  @$_ and push @children, sicons {operate @$_} for @ops;

  if (-t STDOUT) {
    $pager_fh = siproc {exec conf 'pager' or
                        exec 'less' or exec 'more' or sio};
    sforward \*STDIN, $pager_fh;
    close $pager_fh;
    $pager_fh->await;
    ni::procfh::kill_children 'TERM';
  } else {
    sio;
  }

  my $exit_status = 0;
  for (@children) {
    my $status = $_->await;
    next if child_status_ok $status;
    print STDERR "ni: nonzero exit status for child process $_\n";
    $exit_status = 1;
  }
  $handling_pipe ? 0 : $exit_status;
};

# Pagers and kill signals.
# `less` resets a number of terminal settings, including character buffering and
# no-echo. If we kill it directly with a signal, it will exit without restoring a
# shell-friendly terminal state, requiring the user to run `reset` to fix it. So
# in an interrupt context we try to give the pager a chance to exit gracefully by
# closing its input stream and having the user use `q` or similar.

$SIG{PIPE} = sub {
  $handling_pipe = 1;
  close $pager_fh if $pager_fh;
  ni::procfh::kill_children 'TERM';
  exit 0;
};

$SIG{TERM} =
$SIG{HUP}  = sub {
  close $pager_fh if $pager_fh;
  ni::procfh::kill_children 'TERM';
  exit 1;
};

$SIG{INT} = sub {
  if ($pager_fh) {
    close $pager_fh;
    $pager_fh->await;
  }
  ni::procfh::kill_children 'TERM';
  exit 1;
};
2 core/meta/lib
meta.pl
map.pl
76 core/meta/meta.pl
# Image-related data sources.
# Long options to access ni's internal state. Also the ability to instantiate ni
# within a shell process.

defoperator meta_image => q{sio; print image, "\n"};
defoperator meta_keys  => q{sio; print "$_\n" for sort keys %ni::self};
defoperator meta_key   => q{my @ks = @_; sio; print "$_\n" for @ni::self{@ks}};

defoperator meta_help => q{
  my ($topic) = @_;
  $topic = 'tutorial' unless length $topic;
  sio; print $ni::self{"doc/$topic.md"}, "\n";
};

defshort '///ni/',     pmap q{meta_key_op $_}, prc '[^][]+$';
defshort '///ni',      pmap q{meta_image_op},  pnone;
defshort '///ni/keys', pmap q{meta_keys_op},   pnone;

defoperator meta_eval_number => q{sio; print $ni::evals{$_[0] - 1}, "\n"};
defshort '///ni/eval/', pmap q{meta_eval_number_op $_}, integer;

# Documentation options.
# These are listed under the `//help` prefix. This isn't a toplevel option
# because it's more straightforward to model these as data sources.

sub meta_context_name($) {$_[0] || '<root>'}

defshort '///help', pmap q{meta_help_op $_}, popt prx '/(.*)';

defoperator meta_options => q{
  sio;
  for my $c (sort keys %ni::contexts) {
    printf "%s\tlong\t%s\t%s\n",  meta_context_name $c, $ni::long_names{$c}[$_], abbrev dev_inspect_nonl $ni::long_refs{$c}[$_],  40 for       0..$#{$ni::long_refs{$c}};
    printf "%s\tshort\t%s\t%s\n", meta_context_name $c, $_,                      abbrev dev_inspect_nonl $ni::short_refs{$c}{$_}, 40 for sort keys %{$ni::short_refs{$c}};
  }
};

defshort '///ni/options', pmap q{meta_options_op}, pnone;

defshort '///license', pmap q{meta_key_op 'license'}, pnone;

defoperator meta_conf => q{
  sio;
  print "$_\t" . conf($_) . "\t$ni::conf_variables{$_}\n" for sort keys %ni::conf_variables;
};

defshort '///ni/conf', pmap q{meta_conf_op}, pnone;

# Inspection.
# This lets you get details about specific operators or parsing contexts.

defoperator meta_op  => q{sio; print "sub {$ni::operators{$_[0]}}\n"};
defoperator meta_ops => q{sio; print "$_\n" for sort keys %ni::operators};
defshort '///ni/op/', pmap q{meta_op_op $_}, prc '.+';
defshort '///ni/ops', pmap q{meta_ops_op},   pnone;

defoperator meta_parser  => q{sio; print json_encode(parser $_[0]), "\n"};
defoperator meta_parsers => q{sio; print "$_\t" . json_encode(parser $_) . "\n" for sort keys %ni::parsers};
defshort '///ni/parser/', pmap q{meta_parser_op $_}, prc '.+';
defshort '///ni/parsers', pmap q{meta_parsers_op}, pnone;

# The backdoor.
# Motivated by `bugs/2016.0918-replicated-garbage`. Lets you eval arbitrary Perl
# code within this process, and behaves like a normal streaming operator.

defoperator dev_backdoor => q{ni::eval $_[0]};
defshort '/--dev/backdoor', pmap q{dev_backdoor_op $_}, prx '.*';

# Used for regression testing
defoperator dev_local_operate => q{
  my ($lambda) = @_;
  my $fh = siproc {exec ni_quoted_exec_args};
  quote_ni_into $fh, @$lambda;
};

defshort '/--dev/local-operate', pmap q{dev_local_operate_op $_}, _qfn;
24 core/meta/map.pl
# Syntax mapping.
# We can inspect the parser dispatch tables within various contexts to get a
# character-level map of prefixes and to indicate which characters are available
# for additional operators.

use constant qwerty_prefixes => 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@%=$+_,.:';
use constant qwerty_effort =>   '02200011000021011000011212244222332222432332222334344444455544565565223';

defoperator meta_short_availability => q{
  sio;
  print "--------" . qwerty_prefixes . "\n";
  for my $c (sort keys %ni::contexts) {
    my $s = $ni::short_refs{$c};
    my %multi;
    ++$multi{substr $_, 0, 1} for grep 1 < length, keys %$s;

    print substr(meta_context_name $c, 0, 7) . "\t"
        . join('', map $multi{$_} ? '.' : $$s{$_} ? '|' : ' ',
                       split //, qwerty_prefixes)
        . "\n";
  }
};

defshort '///ni/map/short', pmap q{meta_short_availability_op}, pnone;
1 core/monitor/lib
monitor.pl
70 core/monitor/monitor.pl
# Pipeline monitoring.
# nfu provided a simple throughput/data count for each pipeline stage. ni can do
# much more, for instance determining the cause of a bottleneck and previewing
# data.

sub unit_bytes($) {
  return $_[0] >> 10, "K" if $_[0] >> 10 <= 99999;
  return $_[0] >> 20, "M" if $_[0] >> 20 <= 99999;
  return $_[0] >> 30, "G" if $_[0] >> 30 <= 99999;
  return $_[0] >> 40, "T" if $_[0] >> 40 <= 99999;
  return $_[0] >> 50, "P";
}

defconfenv 'monitor', NI_MONITOR => 'yes';

defmetaoperator stderr_monitor_transform => q{
  my ($args, $left) = @_;
  my ($interval) = @$args;
  [map {;$$left[$_], stderr_monitor_op($_, json_encode $$left[$_], $interval)}
        0..$#{$left}];
};

defoperator stderr_monitor => q{
  BEGIN {eval {require Time::HiRes; Time::HiRes->import('time')}}
  my ($monitor_id, $monitor_name, $update_rate) = (@_, 1);
  my ($itime, $otime, $bytes) = (0, 0, 0);
  my $last_update = 0;
  my $start_time = time;
  my ($stdin, $stdout) = (\*STDIN, \*STDOUT);
  while (1) {
    my $t1 = time; $bytes += my $n = saferead $stdin, $_, 65536;
                   last unless $n;
    my $t2 = time; safewrite_exactly $stdout, $_;
    my $t3 = time;

    $itime += $t2 - $t1;
    $otime += $t3 - $t2;

    if ($t3 - $last_update > $update_rate && $t3 - $start_time > 2) {
      $last_update = $t3;
      my $runtime = $t3 - $start_time || 1;
      my $width   = $ENV{COLUMNS} || 80;
      my $preview;
      if ($t3 & 3 && /\n(.*)\n/) {
        ($preview = substr $1, 0, $width - 20) =~ s/\t/  /g;
        $preview =~ s/[[:cntrl:]]/./g;
        $preview = substr $preview, 0, $width - 20;
      } else {
        $preview = substr $monitor_name, 0, $width - 20;
      }

      my $factor_log = log(($otime || 1) / ($itime || 1)) / log 2;

      safewrite \*STDERR,
        sprintf "\033[%d;1H%d \r\033[K%5d%s %5d%s/s% 4d %s\n",
          $monitor_id + 1,
          int($t3),
          unit_bytes $bytes,
          unit_bytes $bytes / $runtime,
          $factor_log * 10,
          $preview;
    }
  }
};

my $original_main_operator = $ni::main_operator;
$ni::main_operator = sub {
  return &$original_main_operator(@_) if conf 'monitor' ne 'yes';
  &$original_main_operator(@_, stderr_monitor_transform_op(0.1));
};
1 core/uri/lib
uri.pl
111 core/uri/uri.pl
# Resources identified by URI.
# A way for ni to interface with URIs. URIs are self-appending like files; to
# quote them you should use the `\'` prefix or the `i` operator:

# | ni http://google.com          # prints contents of google.com
#   ni \'http://google.com        # prints "http://google.com"
#   ni ihttp://google.com         # prints "http://google.com"

# If you've got a lot of resources, you can use `\'` with a lambda to quote all
# of them:

# | ni \'[ http://foo.com http://bar.com ]

sub uri_scheme($) {my ($scheme) = $_[0] =~ /^([^:]+):/; $scheme}
sub uri_path($)   {my $s = uri_scheme $_[0]; sr $_[0], qr|^\Q$s://\E|, ''}

BEGIN {
  no strict 'refs';

  deflong '/resource', defdsp 'resourcealt', 'dispatch table for URI prefixes';

  for my $op (qw/read write exists tmp nuke/) {
    %{"ni::resource_$op"} = ();
    *{"ni::resource_$op"} = sub ($) {
      my ($r) = @_;
      my $scheme = uri_scheme $r;
      my $f = ${"ni::resource_$op"}{$scheme} or
        die "ni: $scheme resources don't support the $op operation";
      &$f($r, uri_path $r);
    };
  }
}

our %nuke_on_exit;
sub nuke_on_exit($) {$nuke_on_exit{$_[0]} = $$}

END {$nuke_on_exit{$_} eq $$ and resource_nuke $_ for keys %nuke_on_exit}

our %resource_read;
our %resource_write;
our %resource_exists;
our %resource_tmp;
our %resource_nuke;

defoperator resource_quote => q{sio; print "$_[0]\n"};
defoperator resource_append => q{
  sio;
  my $decoder = siproc {sdecode};
  sforward resource_read $_[0], $decoder;
  close $decoder;
  $decoder->await;
};

defoperator resource_quote_many => q{sio; print "$_\n" for @_};

defshort "/'", pmap q{resource_quote_many_op @$_},
  pn 1, prc qr/\[/, prep(prc '[^]].*'), prc qr/\]/;

sub defresource($%) {
  my ($scheme, %opts) = @_;
  defresourcealt("'$scheme://",
    pmap qq{resource_quote_op "$scheme://\$_"}, prx '.*');
  defresourcealt("$scheme://",
    pmap qq{resource_append_op "$scheme://\$_"}, prx '.*');

  $resource_read{$scheme}   = fn $opts{read}   if exists $opts{read};
  $resource_write{$scheme}  = fn $opts{write}  if exists $opts{write};
  $resource_exists{$scheme} = fn $opts{exists} if exists $opts{exists};
  $resource_tmp{$scheme}    = fn $opts{tmp}    if exists $opts{tmp};
  $resource_nuke{$scheme}   = fn $opts{nuke}   if exists $opts{nuke};
}

# Stream function extensions.
# Add resource support to srfile and swfile.

my $original_srfile = \&srfile;
my $original_swfile = \&swfile;
my $original_glob_expand = \&glob_expand;

sub is_uri($) {$_[0] =~ /^[^:\/]+:\/\//}

{
  no warnings 'redefine';
  *glob_expand = sub($) {
    return $_[0] if is_uri $_[0] or -e $_[0];
    glob $_[0];
  };

  *srfile = sub($) {
    return resource_read $_[0] if is_uri $_[0];
    &$original_srfile($_[0]);
  };

  *swfile = sub($) {
    return resource_write $_[0] if is_uri $_[0];
    &$original_swfile($_[0]);
  };
}

# Filesystem resources.
# Things that behave like files: local files, HDFS, S3, sftp, etc.

sub uri_temp_noise() {"ni." . getpwuid($<) . "." . noise_str 32}
defconfenv 'tmpdir', TMPDIR => '/tmp';

defresource 'file',
  read   => q{srfile $_[1]},
  write  => q{swfile $_[1]},
  exists => q{-e $_[1]},
  tmp    => q{"file://" . conf('tmpdir') . "/" . uri_temp_noise},
  nuke   => q{unlink $_[1]};
2 core/fn/lib
fn.pl
op-rewrite.pl
89 core/fn/fn.pl
# Operator->operator functions.
# This provides a mechanism for ni to implement aliases and other shorthands.
# Internally we do this by defining a "rewrite parser" that modifies the unparsed
# elements in front of it. Rewriting is namespaced: if you define a lambda in the
# root context, it won't apply in other contexts.

# Here's an example of a lambda that looks for manpages matching the given
# filename pattern:

# | defn ['/manpages', 'pattern'],
#        qw[e[find /usr/share/man -name $pattern*] \<];

# After this definition, ni will perform substitutions like the following:

# | $ ni manpages ls
#   # ni e[find /usr/share/man -name ls*] \<

# Other types of definitions.
# A parameter like 'pattern' will just consume a single unparsed argument, but
# sometimes you want to apply parsing structure to things. You can do that by
# type-tagging the parameters:

# | defexpander ['/manpages-matching', condition => plcode],
#               qw[e[find /usr/share/man -type f] rp$condition];

sub evaluate_fn_expansion(\%@) {
  local $_;
  my ($args, @expansion) = @_;
  return &{$expansion[0]}(%$args) if ref $expansion[0];

  return @expansion unless keys %$args;

  my @result;
  my $scalar_args = join '|', map qr/\$\Q$_\E/, keys %$args;
  $scalar_args = qr/$scalar_args/;
  my $array_args = join '|', map qr/@\Q$_\E/, keys %$args;
  $array_args = qr/$array_args/;

  for (@expansion) {
    if (/^($array_args)$/) {
      push @result, @$args{substr $1, 1};
    } else {
      my @pieces = split /($scalar_args)/;
      $_ & 1 and $pieces[$_] = $$args{substr $pieces[$_], 1}
        for 0..$#pieces;
      push @result, join '', @pieces;
    }
  }
  @result;
}

BEGIN {
  defparser fn_expander => '$$$$',
    q{
      my ($self, @xs) = @_;
      my (undef, $context, $formals, $positions, $expansion) = @$self;
      my ($parsed_formals, @rest) = parse pn(1, popt pempty, $formals), @xs;
      return () unless defined $parsed_formals;

      my %args;
      $args{$_} = $$parsed_formals[$$positions{$_}] for keys %$positions;
      parse parser "$context/op",
            evaluate_fn_expansion(%args, @$expansion), @rest;
    };
}

sub defexpander($@) {
  my ($fn, @expansion) = @_;
  my ($short_spec, @args) = ref $fn ? @$fn : ($fn);
  my ($context, $name) = split /\//, $short_spec, 2;

  my @arg_parsers;
  my %arg_positions;
  if (@args > 1 && ref $args[1]) {
    for (my $i = 0; $i < @args; $i += 2) {
      my ($k, $v) = @args[$i, $i + 1];
      $arg_positions{$k} = $i >> 1;
      push @arg_parsers, $v;
    }
  } elsif (@args) {
    $arg_positions{@args[0..$#args]} = 0..$#args;
    @arg_parsers = map prc '.*', @args;
  }

  defshort $short_spec,
    fn_expander $context, pseq(map pc $_, @arg_parsers),
                          \%arg_positions,
                          \@expansion;
}
50 core/fn/op-rewrite.pl
# Operator-level text substitution.
# WARNING: This is a hack, but possibly a useful one.

sub rewrite_atoms_in {
  my ($op, $fn) = @_;
  return $fn->($op) unless ref $op;
  return [map rewrite_atoms_in($_, $fn), @$op] if ref $op eq 'ARRAY';
  return {map rewrite_atoms_in($_, $fn), %$op} if ref $op eq 'HASH';
  die "rewrite_atoms_in: not sure how to rewrite $op of type " . ref($op);
}

defmetaoperator op_let => q{
  my ($args, $left, $right) = @_;
  my ($bindings, $ops) = @$args;
  my @keys = map $$_[0], @$bindings;
  my %replacements = map @$_, @$bindings;
  my $rewritten = rewrite_atoms_in $ops, sub {
    my $a = shift;
    $a =~ s/\Q$_\E/$replacements{$_}/g for @keys;
    $a;
  };
  ($left, [@$rewritten, @$right]);
};

defoperator op_fn => q{
  my ($bindings, $ops) = @_;
  while (<STDIN>) {
    chomp;
    my @vars = split /\t/;
    my %replacements;
    @replacements{@$bindings} = @vars;
    my $rewritten = rewrite_atoms_in $ops, sub {
      my $a = shift;
      $a =~ s/\Q$_\E/$replacements{$_}/g for @$bindings;
      $a;
    };
    close(my $fh = siproc {exec_ni @$rewritten});
    $fh->await;
  }
};

BEGIN {defparseralias fn_bindings => pn 0, prep(prc qr/[^:=]+/), prc qr/:/}
BEGIN {defparseralias let_binding => pn [0, 2], prx qr/[^:=]+/, pstr '=', prc '[\s\S]+'}
BEGIN {defparseralias let_bindings => pn 0, prep(let_binding), prc qr/:/}

defshort '/l[' => pmap q{op_let_op @$_},
                  pn [1, 2], popt pempty, let_bindings, '/series', pstr ']';

defshort '/f[' => pmap q{op_fn_op @$_},
                  pn [1, 2], popt pempty, fn_bindings, '/series', pstr ']';
2 core/closure/lib
closure.pl
file.pl
31 core/closure/closure.pl
# Data closures.
# Data closures are a way to ship data along with a process, for example over
# hadoop or SSH. The idea is to make your data as portable as ni is.

sub add_closure_key($$) {
  # TODO: use a lib for all data closures
  my ($k, $v) = @_;
  self_append_resource "transient/closure/$k", pack 'u', $v;
}

sub closure_keys()  {grep s/^transient\/closure\///, keys %ni::self}
sub closure_data($) {unpack 'u', $ni::self{"transient/closure/$_[0]"}}

defmetaoperator memory_data_closure => q{
  my ($name, $f) = @{$_[0]};
  my $data;
  my $fh = sni @$f;
  1 while saferead $fh, $data, 8192, length $data;
  close $fh;
  $fh->await;
  add_closure_key $name, $data;
  ();
};

defoperator memory_closure_append => q{sio; print closure_data $_[0]};

BEGIN {defparseralias closure_name => prx '[^][]+'}

defshort '///:', pmap q{memory_closure_append_op $_}, pc closure_name;
defshort '/::',  pmap q{memory_data_closure_op @$_},
                 pseq pc closure_name, _qfn;
43 core/closure/file.pl
# File-backed data closures.
# Sometimes you have data that's too large to store in-memory in the ni image,
# but you still want it to be forwarded automatically. To handle this case, you
# can use a file-backed data closure: the data is streamed after ni's image state
# and is written directly to disk, never stored in memory. (All that's stored in
# memory is the name of the file.)

# A point of subtlety about the way file closures are handled. I'm doing this
# through URIs because tempdirs might not be portable between machines: on a
# Linux machine all tempfiles might be in /tmp, but on Mac they might be
# somewhere else. So we make sure that the name of the tempfile is computed in
# the same place that it's written, minimizing the likelihood that we'll hit
# permission errors.

defresource 'file-closure',
  read  => q{resource_read closure_data $_[1]},
  write => q{my $tmp = resource_tmp 'file://';
             add_closure_key $_[1], $tmp;
             resource_write $tmp},
  nuke  => q{resource_nuke closure_data $_[1]};

defmetaoperator file_data_closure => q{
  my ($name, $f) = @{$_[0]};
  my $c    = "file-closure://$name";
  my $file = resource_write $c;
  my $fh   = sni @$f;
  sforward $fh, $file;
  close $file;
  close $fh;
  $fh->await;
  nuke_on_exit $c;
  add_quoted_resource $c;
  ();
};

defoperator file_closure_append => q{
  sio;
  sforward resource_read(closure_data $_[0]), \*STDOUT;
};

defshort '///@', pmap q{file_closure_append_op $_}, pc closure_name;
defshort '/:@',  pmap q{file_data_closure_op @$_},
                 pseq pc closure_name, _qfn;
1 core/destructure/lib
destructure.pl
45 core/destructure/destructure.pl
# Targeted extraction.
# Most data extraction workflows don't use every key of a rich data object like
# JSON or XML. ni allows you to avoid the overhead of fully decoding these
# objects by using targeted extraction, which compiles an optimized function to
# return just the values you need. Depending on what you're extracting, this can
# be up to 20-30x faster than doing a full decode.



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

  my $matchers = join "\n",
                 map '/^/g; push @result, ' . json_si_gen->(k => qr/\Q$_\E/) . ';',
                 map sr($_, qr/^:/, ''), @pieces;
  qq{
    my \@result;
    $matchers;
    print join("\\t", \@result) . "\\n";
  };
}

defoperator destructure => q{
  ni::eval gen(q{
    no warnings 'uninitialized';
    eval {binmode STDOUT, ":encoding(utf-8)"};
    print STDERR "ni: warning: your perl might not handle utf-8 correctly\n" if $@;
    while (<STDIN>) {
      %e;
    }
  })->(e => json_extractor $_[0]);
};

defshort '/D', pmap q{destructure_op $_}, generic_code;
1 core/checkpoint/lib
checkpoint.pl
29 core/checkpoint/checkpoint.pl
# Checkpoint files.
# You can break a long pipeline into a series of smaller files using
# checkpointing, whose operator is `:`. The idea is to cache intermediate
# results. A checkpoint specifies a file.

# Checkpoints are fully buffered before emitting output.

sub checkpoint_create($$) {
  my $name = "/";
  $name = "$_[0].part." . noise_str 8 while -e $name;
  my $fh = swfile $name;
  sforward sni(@{$_[1]}), $fh;
  rename $name, $_[0];
}

defoperator checkpoint => q{
  my ($file, $generator) = @_;
  sio;
  checkpoint_create $file, $generator unless -r $file;
  scat $file;
};

defmetaoperator inline_checkpoint => q{
  my ($args, $left, $right) = @_;
  my ($file) = @$args;
  ([], [checkpoint_op($file, $left), @$right]);
};

defshort '/:', pmap q{inline_checkpoint_op $_}, pc nefilename;
1 core/net/lib
net.pl
32 core/net/net.pl
# Networking stuff.
# SSH tunneling to other hosts. Allows you to run a ni lambda elsewhere. ni does
# not need to be installed on the remote system, nor does its filesystem need to
# be writable.

BEGIN {defparseralias ssh_host => prx '[^][/,]+'}

defoperator ssh => q{
  my ($host, $lambda) = @_;
  my $ssh_pipe = siproc {exec 'ssh', @$host, shell_quote ni_quoted_exec_args};
  quote_ni_into $ssh_pipe, @$lambda;
};

defshort '/s', pmap q{ssh_op @$_},
  pseq palt(pc pmap(q{[$_]}, ssh_host), pc multiword), _qfn;

# Network resources.

defresource 'http', read  => q{soproc {exec 'curl', '-sS', $_[0]} @_},
                    write => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'https', read  => q{soproc {exec 'curl', '-sS', $_[0]} @_},
                     write => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'sftp',
  read   => q{my ($host, $path) = $_[1] =~ m|^([^:/]+):?(.*)|;
              soproc {exec 'ssh', $host, 'cat', $path}};

defresource 's3cmd',
  read   => q{soproc {exec 's3cmd', '--no-progress', '--stop-on-error', 'get', "s3://$_[1]", '-'} @_},
  write  => q{siproc {exec 's3cmd', 'put', '-', "s3://$_[1]"} @_};
  # TODO
1 core/buffer/lib
buffer.pl
14 core/buffer/buffer.pl
# Buffering operators.
# Buffering a stream causes it to be forced in its entirety. Buffering does not
# imply, however, that the stream will be consumed at any particular rate; some
# buffers may be size-limited, at which point writes will block until there's
# space available.

# Null buffer.
# Forwards data 1:1, but ignores pipe signals on its output.

defoperator buffer_null => q{local $SIG{PIPE} = 'IGNORE'; sio};

defshort '/B',
  defdsp 'bufferalt', 'dispatch table for /B buffer operator',
    n => pmap q{buffer_null_op}, pnone;
1 core/script/lib
script.pl
35 core/script/script.pl
# Scripting support.
# This lets you define a library of scripts or other random utilities (possibly
# with dependent files) and gives you a way to invoke those scripts from a custom
# operator. See doc/script.md for details about how this works.

sub export_lib_to_path {
  local $_;
  my ($lib, $path) = @_;
  $path ||= uri_path resource_tmp 'file://';
  mkdir $path or die "ni: could not create $path/ for library export: $!";
  my @l     = lib_entries $lib, $ni::self{"$lib/lib"};
  my @files = map sr($_, qr|^\Q$lib/\E|, "$path/"), @l;
  wf "$path/ni", image;
  wf $files[$_], $ni::self{$l[$_]} for 0..$#l;
  chmod 0755, "$path/ni", @files;
  $path;
}

sub rm_rf($) {
  local $SIG{CHLD} = 'DEFAULT';
  system 'rm', '-rf', $_[0];
}

defoperator script => q{
  my ($lib, $cmd) = @_;
  my $tmpdir = export_lib_to_path $lib;
  my $runner = siproc {
    chdir $tmpdir;
    sh $cmd;
  };
  sforward \*STDIN, $runner;
  close $runner;
  $runner->await;
  rm_rf $tmpdir;
};
1 core/assert/lib
assert.pl
6 core/assert/assert.pl
# Assertions: die horribly unless something is true.
# Defines the `!` operator, which will deliberately nuke your entire process
# unless its condition is true.

defshort '/!',
  defdsp 'assertdsp', 'dispatch table for the ! assertion operator';
1 core/col/lib
col.pl
188 core/col/col.pl
# Column manipulation operators.
# In root context, ni interprets columns as being tab-delimited.

# Column selection.
# Normally perl is fast at text manipulation, but on most UNIX systems
# `/usr/bin/cut` is at least an order of magnitude faster. We can use it if the
# column access order is strictly ascending and has no duplicates.

sub col_cut {
  my ($floor, $rest, @fs) = @_;
  exec 'cut', '-f', join ',', $rest ? (@fs, "$floor-") : @fs;
}

use constant cols_gen =>
  gen q{@_ = split /\t/, $_, %limit; print join "\t", @_[%is]};

defoperator cols => q{
  my ($floor, @cs) = @_;
  my $asc = join('', @cs) eq join('', sort {$a <=> $b} @cs);
  my %dup; ++$dup{$_} for @cs;
  return col_cut $floor + 1, scalar(grep $_ == -1, @cs), map $_ + 1, @cs
    if $asc && !grep $_ > 1, values %dup;
  exec 'perl', '-lne',
       cols_gen->(limit => $floor + 1,
                  is    => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cs);
};

defshort '/f',
  defalt 'colalt', 'list of alternatives for /f field-select operator',
    pmap q{cols_op @$_}, colspec;

# Column swapping.
# This is such a common thing to do that it gets its own operator `x`. The idea
# is that you're swapping the specified column(s) into the first N position(s).

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

# Column splitting.
# Adapters for input formats that don't have tab delimiters. Common ones are,
# with their split-spec mnemonics:

# | commas:       C
#   slashes:      D
#   "proper CSV": V
#   pipes:        P
#   whitespace:   S
#   non-words:    W

# You can also field-split on arbitrary regexes, or extend the splitalt dsp to
# add custom split operators.

defoperator split_chr   => q{exec 'perl', '-lnpe', $_[0] =~ /\// ? "y#$_[0]#\\t#" : "y/$_[0]/\\t/"};
defoperator split_regex => q{my $r = qr/$_[0]/; exec 'perl', '-lnpe', "s/$r/\$1\\t/g"};
defoperator scan_regex  => q{exec 'perl', '-lne',  'print join "\t", /' . "$_[0]/g"};

# TODO: collapse multiline fields
defoperator split_proper_csv => q{
  while (<STDIN>) {
    my @fields = /\G([^,"\n]*|"(?:[^"]+|"")*")(?:,|$)/g;
    s/\t/        /g, s/^"|"$//g, s/""/"/g for @fields;
    pop @fields;
    print join("\t", @fields), "\n";
  }
};

defshort '/F',
  defdsp 'splitalt', 'dispatch table for /F split operator',
    'C' => pmap(q{split_chr_op   ','},               pnone),
    'D' => pmap(q{split_chr_op   '\/'},              pnone),
    'V' => pmap(q{split_proper_csv_op},              pnone),
    'P' => pmap(q{split_chr_op   '|'},               pnone),
    'S' => pmap(q{split_regex_op '\s+'},             pnone),
    'W' => pmap(q{split_regex_op '[^\w\n]+'},        pnone),
    '/' => pmap(q{split_regex_op $_},                regex),
    ':' => pmap(q{split_chr_op   $_},                prx '.'),
    'm' => pn(1, pstr '/', pmap q{scan_regex_op $_}, regex);

# Juxtaposition.
# You can juxtapose two data sources horizontally by using `w` for `with`.

defoperator with_right => q{
  my $fh = sni @_;
  my $l;
  while (<STDIN>) {
    chomp;
    return unless defined($l = <$fh>);
    print "$_\t$l";
  }
};

defoperator with_left => q{
  my $fh = sni @_;
  my $l;
  while (<STDIN>) {
    return unless defined($l = <$fh>);
    chomp $l;
    print "$l\t$_";
  }
};

defshort '/w', pmap q{with_right_op @$_}, _qfn;
defshort '/W', pmap q{with_left_op  @$_}, _qfn;

# Vertical transformation.
# This is useful when you want to apply a streaming transformation to a specific
# set of columns. For example, if you have five columns and want to lowercase the
# middle one:

# | ni vCplc              # lowercase column C

# WARNING: your process needs to output exactly one line per input. If the driver
# is forced to buffer too much memory it will hang waiting for the process to
# catch up.

# TODO: optimize this. Right now it's horrendously slow.

defoperator vertical_apply => q{
  my ($colspec, $lambda) = @_;
  my ($limit, @cols) = @$colspec;
  my ($i, $o) = sioproc {exec ni_quoted_exec_args};
  safewrite $i, ni_quoted_image 0, @$lambda;

  vec(my $rbits = '', fileno $o, 1) = 1;
  vec(my $wbits = '', fileno $i, 1) = 1;
  fh_nonblock $i;

  my $read_buf = '';
  my $write_buf = '';
  my @queued;
  my @awaiting_completion;
  my $stdin_ok = my $proc_ok = 1;
  while ($stdin_ok || $proc_ok) {
    my $l = sum map length, @queued;
    $_ = '';
    chomp, push @queued, $_ while ($l += length) <= 1048576
                              and $stdin_ok &&= defined($_ = <STDIN>);

    while (@queued && sum(map length, @awaiting_completion) < 1048576
                   && select undef, my $wout=$wbits, undef, 0) {
      my $n = 0;
      my @chopped;
      push @chopped, join "\t", (split /\t/, $queued[$n++], $limit)[@cols]
        while $n < @queued && 8192 > sum map 1 + length, @chopped;
      ++$n unless $n;
      push @awaiting_completion, @queued[0..$n-1];
      @queued = @queued[$n..$#queued];
      my $s  = $write_buf . join '', map "$_\n", @chopped;
      my $sn = safewrite $i, $s;
      $write_buf = substr $s, $sn;
    }

    close $i if !@queued && !$stdin_ok;

    $proc_ok &&= saferead $o, $read_buf, 8192, length $read_buf
      while $proc_ok && select my $rout=$rbits, undef, undef, 0;

    my @lines = split /\n/, $read_buf . " ";
    $proc_ok ? $read_buf = substr pop(@lines), 0, -1 : pop @lines;
    for (@lines) {
      die "ni: vertical apply's process emitted too many lines: $_"
        unless @awaiting_completion;
      my @fs = split /\t/, shift @awaiting_completion;
      @fs[@cols] = my @cs = split /\t/;
      print join("\t", @fs, @cs[@fs..$#cs]), "\n";
    }
  }

  die "ni: vertical apply's process ultimately lost "
    . scalar(@awaiting_completion) . " line(s)"
  if @awaiting_completion;

  close $o;
  $o->await;
};

defshort '/v', pmap q{vertical_apply_op @$_}, pseq colspec_fixed, _qfn;
3 core/row/lib
row.pl
scale.pl
join.pl
187 core/row/row.pl
# Row-level operations.
# These reorder/drop/create entire rows without really looking at fields.

defoperator head => q{exec 'head', @_};
defoperator tail => q{exec 'tail', $_[0], join "", @_[1..$#_]};

defoperator safe_head => q{$. <= $_[0] && print while <STDIN>};

defconfenv 'row/seed', NI_ROW_SEED => 42;

defoperator row_every => q{($. -1) % $_[0] || print while <STDIN>};
defoperator row_match => q{$\ = "\n"; chomp, /$_[0]/o && print while <STDIN>};
defoperator row_sample => q{
  srand conf 'row/seed';
  $. = 0;
  while (<STDIN>) {
    print, $. -= -log(1 - rand()) / $_[0] if $. >= 0;
  }
};

defoperator row_cols_defined => q{
  my ($floor, @cs) = @_;
  my @pieces = ('[^\t\n]*') x $floor;
  $pieces[$_] = '[^\t\n]+' for @cs;
  my $r = join '\t', @pieces;
  $r = qr/^$r/;
  /$r/ and print while <STDIN>;
};

defoperator row_include_or_exclude_exact => q{
  my ($include_mode, $col, $lambda) = @_;
  my %set;
  my $fh = sni @$lambda;
  chomp, ++$set{$_} while <$fh>;
  close $fh;
  $fh->await;
  while (<STDIN>) {
    chomp;
    my @fs = split /\t/, $_, $col + 2;
    print "$_\n" if !$include_mode == !$set{$fs[$col]};
  }
};

defshort '/r',
  defalt 'rowalt', 'alternatives for the /r row operator',
    pmap(q{tail_op '-n', '',  $_},       pn 1, prx '[+~]', integer),
    pmap(q{tail_op '-n', '+', ($_ + 1)}, pn 1, prx '-',    integer),
    pmap(q{safe_head_op  $_},            pn 1, prx 's',    number),
    pmap(q{row_every_op  $_},            pn 1, prx 'x',    number),
    pmap(q{row_match_op  $_},            pn 1, prx '/',    regex),
    pmap(q{row_sample_op $_},                  prx '\.\d+'),
    pmap(q{head_op '-n', 0 + $_},        integer),
    pmap(q{row_include_or_exclude_exact_op 1, @$_}, pn [1, 2], pstr 'i', colspec1, _qfn),
    pmap(q{row_include_or_exclude_exact_op 0, @$_}, pn [1, 2], pstr 'I', colspec1, _qfn),
    pmap(q{row_cols_defined_op @$_},     colspec_fixed);

# Sorting.
# ni has four sorting operators, each of which can take modifiers:

# | g     group: sort by byte ordering
#   G     groupuniq: sort + uniq by byte ordering
#   o     order: sort numeric ascending
#   O     rorder: sort numeric descending

# Modifiers follow the operator and dictate the column index and, optionally, the
# type of sort to perform on that column (though a lot of this is already
# specified by which sort operator you use). Columns are specified as A-Z, and
# modifiers, which are optional, are any of these:

# | g     general numeric sort (not available for all 'sort' versions)
#   n     numeric sort
#   -     reverse (I would use 'r', but it conflicts with the row operator)

BEGIN {defparseralias sortspec => prep pseq colspec1, popt prx '[-gn]+'}

sub sort_args {'-t', "\t",
               map {my $i = $$_[0] + 1;
                    (my $m = defined $$_[1] ? $$_[1] : '') =~ s/-/r/g;
                    ('-k', "$i$m,$i")} @_}

# Compatibility detection.
# GNU coreutils sort supports some useful options like `--buffer-size` and
# `--compress-program`. We should use these if they exist because they can make a
# huge difference when processing large datasets.

# Note that we localize compatibility detection down to the operator -- we don't
# do it system-wide or at parse time. The reason is that parameterized operators
# can be moved, potentially across machines; this really is the only way to do it
# reliably.

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

defconfenv 'row/sort-compress', NI_ROW_SORT_COMPRESS => '';
defconfenv 'row/sort-buffer',   NI_ROW_SORT_BUFFER   => '64M';
defconfenv 'row/sort-parallel', NI_ROW_SORT_PARALLEL => '4';

defoperator row_sort => q{
  exec 'sort', sort_extra_args(
    length(conf 'row/sort-compress')
      ? ('--compress-program=' . conf 'row/sort-compress') : (),
    '--buffer-size=' . conf 'row/sort-buffer',
    '--parallel='    . conf 'row/sort-parallel'), @_};

defoperator partial_sort => q{
  my $sort_size = shift;
  my @buff = ();
  while (<STDIN>) {
    push @buff, $_;
    if (@buff == $sort_size) {
      print sort(@buff);
      @buff = ();
    }
  }
  print sort(@buff) if @buff;
};

defshort '/g',
  defalt 'sortalt', 'alternatives for the /g row operator',
    pmap(q{partial_sort_op               $_}, pn 1, prx '_', integer),
    pmap(q{row_sort_op        sort_args @$_}, sortspec);
defshort '/o', pmap q{row_sort_op '-n',  sort_args @$_}, sortspec;
defshort '/O', pmap q{row_sort_op '-rn', sort_args @$_}, sortspec;

defoperator row_grouped_sort => q{
  my ($key_col, $sort_cols) = @_;
  my $key_expr = $key_col
    ? qq{(split /\\t/)[$key_col]}
    : qq{/^([^\\t\\n]*)/};

  my $sort_expr = join ' || ',
    map {my $sort_op = $$_[1] =~ /[gn]/ ? '<=>' : 'cmp';
         $$_[1] =~ /-/ ? qq{\$b[$$_[0]] $sort_op \$a[$$_[0]]}
                       : qq{\$a[$$_[0]] $sort_op \$b[$$_[0]]}} @$sort_cols;

  ni::eval gen(q{
    my $k;
    my @group;
    push @group, $_ = <STDIN>;
    ($k) = %key_expr;
    while (<STDIN>) {
      my ($rk) = %key_expr;
      if ($rk ne $k) {
        print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
        @group = $_;
        $k = $rk;
      } else {
        push @group, $_;
      }
    }
    print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
  })->(key_expr => $key_expr, sort_expr => $sort_expr);
};

defshort '/gg', pmap q{row_grouped_sort_op @$_}, pseq colspec1, sortspec;

# Counting.
# Sorted and unsorted streaming counts.

defoperator count => q{
  my ($n, $last) = (0, undef);
  while (<STDIN>) {
    if (!defined $last or $_ ne $last) {
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

189 core/row/scale.pl
# Row-based process scaling.
# Allows you to bypass process bottlenecks by distributing rows across multiple
# workers.

BEGIN {defshort '/S', defalt 'scalealt', 'row scaling alternation list'}

# Fixed scaling.
# The simplest option: specify N workers and a lambda, and the lambda will be
# replicated that many times. Incoming data is broken into chunks of rows and
# written to any worker that's available.

# Implementation-wise here's how this works. We're distributing a single stream
# of data to potentially a lot of subprocesses, each of which might be quite
# fast. So we need to optimize this aggressively, which in this case consists of
# the following:

# | 1. We maintain an input queue for each worker for reasons described below.
#   2. We minimize the overhead involved in line-splitting blocks, avoiding it
#      entirely for most of the input data.
#   3. We keep writes to workers small enough that they won't block.

# Visually, here's what we're doing:

# | worker 1 <- [block 1] [block 2] [block 3] [block 4] [part of block 5]
#   worker 2 <- [rest of part 5] [block 6] [block 7] [block 8] [part of block 9]
#   worker 3 <- ...

# The key here is that we can avoid line-splitting for any two consecutive blocks
# we send to the same worker, so we want every queue-fill operation to consume a
# large number of blocks. This leads to a possibly counterintuitive heuristic: we
# deliberately let queues run low before refilling them.

# Reading from the workers' stdout is exactly the same: we enqueue a bunch of
# blocks and then line-merge once the queues are full enough.

# A note about blocking: the scale operator goes to some lengths to avoid
# blocking on the workers, but it's fine and expected for it to block on its own
# stdin and stdout. The only consideration there is that we try to interleave
# worker and stdin/stdout blocking; this increases the likelihood that we'll
# saturate source and/or sink processes.

# TODO: refactor this to make the pieces available elsewhere. Should probably end
# up with various "combine streams vertically/horizontally/etc" library
# functions.

defoperator row_fixed_scale => q{
  use constant buf_size => 32768;

  sub new_ref() {\(my $x = '')}

  my ($n, $f) = @_;
  conf_set monitor => 0;

  my ($iqueue, $oqueue) = (64, 64);

  my (@wi, @wo);
  my ($wb, $rb, $w, $r);
  my ($ib, $ob, $ibtmp, $obtmp);
  for (1..$n) {
    my ($i, $o) = sioproc {
      setpriority 0, 0, $n >> 2;
      &$ni::main_operator(flatten_operators $f);
      exit;
    };
    push @wi, $i;
    push @wo, $o;
    vec($wb, fileno $i, 1) = 1;
    vec($rb, fileno $o, 1) = 1;
  }

  vec($ib, fileno STDIN,  1) = 1;
  vec($ob, fileno STDOUT, 1) = 1;

  my $stdout_reader = siproc {
    my @bufs;
    my $buf_limit = $oqueue * $n;
    my @stdout = map [], @wo;
    my @outqueue;
    my $b;
    my $stdout = \*STDOUT;

    close $_ for @wi;

    while ($n) {
      until (@outqueue < $oqueue * $n) {
        safewrite $stdout, ${$b = shift @outqueue};
        push @bufs, $b unless @bufs >= $buf_limit;
      }

      select $r = $rb, undef, undef, undef;
      for my $i (0..$#wo) {
        next unless defined $wo[$i];
        next unless vec $r, fileno $wo[$i], 1;

        while (@outqueue and select undef, $obtmp = $ob, undef, 0) {
          safewrite $stdout, ${$b = shift @outqueue};
          push @bufs, $b unless @bufs >= $buf_limit;
        }
        my $so = $stdout[$i];
        if (saferead $wo[$i], ${$b = pop(@bufs) || new_ref}, buf_size) {
          push @$so, $b;
          my $np;
          if (@$so >= $oqueue and 0 <= ($np = rindex $$b, "\n")) {
            push @outqueue, @$so[0..$#{$so} - 1];
            push @outqueue, \(my $x = substr $$b, 0, $np + 1);
            $$b = substr $$b, $np + 1;
            @$so = ($b);
          }
        } else {
          --$n;
          vec($rb, fileno $wo[$i], 1) = 0;
          close $wo[$i];
          push @outqueue, @$so;
          $stdout[$i] = $wo[$i] = undef;
        }
      }
    }

    safewrite $stdout, $$_ for @outqueue;
  };

  close $stdout_reader;
  close $_ for @wo;

  {
    my @bufs;
    my $buf_limit = $iqueue * $n;
    my @stdin = map [], @wi;
    my @queue;
    my $eof;
    my $b;
    my $stdin = \*STDIN;

    until (!@queue && $eof) {
      select undef, $w = $wb, undef, undef;
      for my $i (0..$#wi) {
        next unless vec $w, fileno $wi[$i], 1;

        my $si = $stdin[$i];
        if (@$si * 4 < $iqueue) {
          # Commit to refilling this stdin queue, which means we need to write
          # exclusively to this one until we find a line break.
          push @$si, shift @queue while @$si < $iqueue and @queue;
          while (@queue or not $eof) {
            unless ($b = $queue[0]) {
              last if $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, buf_size;
              push @queue, $b;
            }

            my $np;
            if (0 <= ($np = rindex $$b, "\n")) {
              push @$si, \(my $x = substr $$b, 0, $np + 1);
              $$b = substr $$b, $np + 1;
              last;
            } else {
              push @$si, shift @queue;
            }
          }
        }

        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, buf_size
        or push @queue, $b
          while @queue < $iqueue * $n and !$eof
            and select $ibtmp = $ib, undef, undef, 0;

        if (@$si) {
          safewrite $wi[$i], ${$b = shift @$si};
          push @bufs, $b unless @bufs >= $buf_limit;
        }

        $eof ||= !saferead $stdin, ${$b = pop(@bufs) || new_ref}, buf_size
        or push @queue, $b
          while @queue < $iqueue * $n and !$eof
            and select $ibtmp = $ib, undef, undef, 0;
      }
    }

    # Run out the individual queues.
    for my $i (0..$#wi) {
      safewrite $wi[$i], $$_ for @{$stdin[$i]};
      close $wi[$i];
    }
  }

  $_->await for @wo;
  $stdout_reader->await;
};

defscalealt pmap q{row_fixed_scale_op @$_}, pseq integer, _qfn;
32 core/row/join.pl
# Streaming joins.
# The UNIX `join` command does this, but rearranges fields in the process. ni
# implements its own operators as a workaround.

defoperator join => q{
  my ($left_cols, $right_cols, $f) = @_;
  my $fh = sni @$f;
  my ($leof, $reof) = (0, 0);
  my ($llimit, @lcols) = @$left_cols;
  my ($rlimit, @rcols) = @$right_cols;
  while (!$leof && !$reof) {
    chomp(my $lkey = join "\t", (split /\t/, my $lrow = <STDIN>, $llimit + 1)[@lcols]);
    chomp(my $rkey = join "\t", (split /\t/, my $rrow = <$fh>,   $rlimit + 1)[@rcols]);
    $reof ||= !defined $rrow;
    $leof ||= !defined $lrow;

    until ($lkey eq $rkey or $leof or $reof) {
      chomp($rkey = join "\t", (split /\t/, $rrow = <$fh>, $llimit + 1)[@lcols]),
        $reof ||= !defined $rrow until $reof or $rkey ge $lkey;
      chomp($lkey = join "\t", (split /\t/, $lrow = <STDIN>, $rlimit + 1)[@rcols]),
        $leof ||= !defined $lrow until $leof or $lkey ge $rkey;
    }

    if ($lkey eq $rkey and !$leof && !$reof) {
      chomp $lrow;
      print "$lrow\t$rrow";
    }
  }
};

defshort '/j', pmap q{join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]},
               pseq popt colspec, _qfn;
9 core/pl/lib
json_util.pm
hash_util.pm
util.pm
math.pm
stream.pm
reducers.pm
time.pm
geohash.pl
pl.pl
19 core/pl/json_util.pm
# JSON utils 

# for extracting a small number of fields from
# complex JSON

sub get_array {
  my @raw_data = $_[0] =~ /"$_[1]":\[([^]]+)/;
  return map {eval $_} map {split /,/, $_} @raw_data;
}

sub get_scalar {
  my ($output_val,) = $_[0] =~ /"$_[1]":("[^"]*"|-?\d+.?\d*)/;
  return eval $output_val;
}

sub get_flat_hash {
  my @raw_data = $_[0] =~ /"$_[1]":({[^}]*})/;
  return @raw_data;
}
150 core/pl/hash_util.pm
# Hash utilities

# Key-By-Value ascending and descending
sub kbv_dsc { my %h = @_; sort { $h{$b} <=> $h{$a} } keys %h }
sub kbv_asc { my %h = @_; sort { $h{$a} <=> $h{$b} } keys %h }

sub dump_array {
  my $r = shift;
  my $indent = $_[0] ? $_[0] : 0;
  print "\t" x $indent, "[\n";
  for my $el (@$r) {
    if( ref $el eq "HASH" ) { 
        dump_hash( $el, $indent + 1 );
    } elsif( ref $el eq "ARRAY") {
        dump_array( $el, $indent + 1);
    } else { 
        print "\t" x ($indent + 1), "$el\n";
    } 
  } 
  print "\t" x $indent, "]\n";
}

sub dump_hash { 
  my $h = shift; 
  my $indent = $_[0] ? $_[0] : 0;
  foreach my $key (keys %$h) { 
    print "\t" x $indent, "$key\t=>";
    if( ref $h->{$key} eq "HASH" ) { 
      print "\n";
      dump_hash( $h->{$key}, $indent + 1);
    } elsif( ref $h->{$key} eq "ARRAY") {
      print "\n";
      dump_array( $h->{$key}, $indent + 1);
    } else { 
      print "\t", $h->{$key}, "\n";
    } 
  }   
}

sub dump_data {
  $dumpme = pop @_;
  print join "\t", @_, "\n";
  if(ref($dumpme) eq "HASH") {
    dump_hash($dumpme);
  } elsif(ref($dumpme) eq "ARRAY") {
    dump_array($dumpme);
  } else {
    print "$dumpme\n";
  }
}

sub merge_hash_values($$) {
  my ($val1, $val2) = @_;
  return $val1 unless defined $val2;
  return $val2 unless defined $val1;

  my $ref1 = ref($val1);
  my $ref2 = ref($val2);
  my $output;
  if ($ref1 eq "" and $ref2 eq "") {
    $output = $val1 || $val2;
  } elsif($ref1 eq "ARRAY" and $ref2 eq "ARRAY") {
    my @output = @$val1;
    push @output, @$val2;
    $output = \@output;
  } elsif($ref1 eq "HASH" and $ref2 eq "HASH") {
    $output = merge_hashes($val1, $val2);
  } else {
    die "cannot merge different types of values value 1: $val1, value 2: $val2\n";
  }
  $output
}

sub sum_two_hashes($$) {
  my ($href1, $href2) = @_;
  for my $key(keys %{$href2}) {
    my $val = $href2->{$key};
    if(ref($val) eq "") {
       $href1->{$key} += $val;
    } elsif(ref($val) eq "HASH") {
      $href1->{$key} = %{$val} ? sum_two_hashes($href1->{$key}, $val ) 
                               : $href1->{$key} ? $href1->{$key} : {};
    } else { die "bad structure" }
  }
  $href1;
}

sub accumulate_two_hashes($$) {
  my ($href1, $href2) = @_;
  for my $key (keys %{$href2}) {
    $href1->{$key} = {} if not exists $href1->{$key};
    my $val = $href2->{$key};
    if(ref($val) eq "") {
      $href1->{$key}->{$val} += 1;
    } elsif(ref($val) eq "ARRAY") {
      for (@{$val}) {$href1->{$key}->{$_} += 1;}
    } elsif(ref($val) eq "HASH") {
      $href1->{$key} = accumulate_two_hashes($href1->{$key}, $val );
    } else {
      die "accumulating went bad";
    }
  }
  $href1;
}

sub merge_two_hashes($$) {
  my ($href1, $href2) = @_;
  my %h1 = %$href1;
  my %h2 = %$href2;
  my @keys = uniq keys %h1, keys %h2; 
  my %h;
  for my $key(@keys) {
    my $val1 = $h1{$key};
    my $val2 = $h2{$key};
    $h{$key} = merge_hash_values($val1, $val2); 
  }
  \%h;
}

sub sum_hashes {
  my $href = shift;
  for(@_) {
    $href = sum_two_hashes($href, $_);
  }
  $href;
}

sub merge_hashes {
  my $href = shift;
  for(@_) {
    $href = merge_two_hashes($href, $_);
  }
  $href;
}

sub accumulate_hashes {
  my $href = {};
  for(@_) {
    $href = accumulate_two_hashes($href, $_);
  }
  $href;
}

# JSON decoding is very slow; if you know that 
# your data has no shared (top-level) keys, 
# you can go from string json directly to string json.
sub string_merge_hashes {
  my @hash_vals = map {substr $_, 1, -1} @_;
  "{" . join(",", @hash_vals) . "}";
}
284 core/pl/util.pm
# Utility library functions.
# Mostly inherited from nfu. This is all loaded inline before any Perl mapper
# code. Note that List::Util, the usual solution to a lot of these problems, is
# introduced in v5.7.3, so we can't rely on it being there.

sub ceval {eval $_[0]; die "error evaluating $_[0]: $@" if $@}

sub first  {$_[0]}
sub final  {$_[$#_]}
sub rando  {$_[int(rand($#_ + 1))]}
sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

sub take($@) {my ($n, @xs) = @_; @xs[0..($n-1)]}
sub drop($@) {my ($n, @xs) = @_; @xs[$n..$#xs]} 

sub take_while(&@) {
  local $_;
  my ($f, @xs) = @_; 
  my @out; 
  for (@xs) { if(&$f($_)) {push @out, $_} else {last} }
  @out;
}

sub drop_while(&@) {
  local $_;
  my ($f, @xs) = @_;
  my @out;
  my $count = 0;
  for (@xs) { if(!&$f($_)) {return drop $count, @xs} $count++;}
  ();
} 

sub take_every($$@) {
  my ($every, $start, @r) = @_;
  @r[grep { ($_ - $start) % $every == 0 } 0..$#r];
}

sub take_even(@) { take_every(2, 0, @_); }
sub take_odd(@) { take_every(2, 1, @_); }

sub deltas {local $_; return () unless @_ > 1; map $_[$_] - $_[$_ - 1], 1..$#_}
sub totals {local $_; my ($x, @xs) = 0; push @xs, $x += $_ for @_; @xs}

sub argmax(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) > $fm;
  }
  $m;
}

sub argmin(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) < $fm;
  }
  $m;
}

sub any(&@) {local $_; my ($f, @xs) = @_; &$f($_) && return 1 for @xs; 0}
sub all(&@) {local $_; my ($f, @xs) = @_; &$f($_) || return 0 for @xs; 1}

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
  use integer;
  local $_;
  return () unless @_;
  @$_ or return () for @_;
  return map [$_], @{$_[0]} if @_ == 1;
  my @ns = map scalar(@$_), @_;
  map {
    my ($i, $xs) = ($_ - 1, []);
    for (reverse 0..$#ns) {
      unshift @$xs, ${$_[$_]}[$i % $ns[$_]];
      $i /= $ns[$_];
    }
    $xs;
  } 1..prod(@ns);
}

sub clip {
  local $_;
  my ($lower, $upper, @xs) = @_;
  wantarray ? map min($upper, max $lower, $_), @xs
            : min $upper, max $lower, $xs[0];
}

sub within {
  local $_;
  my ($lower, $upper, @xs) = @_;
  not grep $_ < $lower || $_ > $upper, @xs;
}

sub rf  {open my $fh, "< $_[0]" or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rfl {open my $fh, "< $_[0]" or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub dirbase($)  {my @xs = $_[0] =~ /^(.*)\/+([^\/]+)\/*$/; @xs ? @xs : ('', $_[0])}
sub basename($) {(dirbase $_[0])[1]}
sub dirname($)  {(dirbase $_[0])[0]}

sub mkdir_p {-d $_[0] or !length $_[0] or mkdir_p(dirname $_[0]) && mkdir $_[0]}

sub wf {
  local $_;
  my $f = shift;
  my $fh;
  if ($f =~ /^\|/) {
    open $fh, $f or die "wf $f: $!";
  } else {
    mkdir_p dirname $f;
    open $fh, "> $f" or die "wf $f: $!";
  }
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

sub af {
  local $_;
  my $f = shift;
  mkdir_p dirname $f;
  open my $fh, ">> $f" or die "af $f: $!";
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

sub testpath {
  $_ =~ s/-\*/-0000\*/;
  $_;
}

our $base64_digits = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/!#$%&()[]*@?|;<>';
our @base64_digits = split //, $base64_digits; 
our $base64_ext_digits = substr($base64_digits, -16);
our @base64_ext_digits = split //, $base64_ext_digits;
our %base64_ext_decode = map(($base64_ext_digits[$_], 1), 0..$#base64_ext_digits);
our %base64_decode = map(($base64_digits[$_], ($_ % 64)), 0..$#base64_digits);

sub hex2extbase64($) {
  my $hex_num = hex $_[0];
  my $n_hex_chars = length $_[0];
  if ($n_hex_chars == 3) {
    $base64_digits[$hex_num >> 6] . $base64_digits[$hex_num % 64];
  } elsif ($n_hex_chars == 2) {
    $base64_digits[$hex_num >> 2] . $base64_ext_digits[$hex_num % 4];
  } else {
    $base64_ext_digits[$hex_num];
  }
}

sub hex2base64($) {
  $_[0] =~ tr/\-//d;
  my @hex_strs = unpack("(A3)*", $_[0]); 
  my $last_hex_str = pop @hex_strs;
  my @hex_nums = map {hex $_} @hex_strs;
  my @b64_strs = map { $base64_digits[$_ >> 6] . $base64_digits[$_ % 64] } @hex_nums;
  my $b64_str = join("", @b64_strs);
  my $last_chars = hex2extbase64 $last_hex_str;
  $b64_str . $last_chars;
}

sub extbase642hex ($) {
  my $n_hex_digits = length($_[0]) + 1 - sum(map { $base64_ext_decode{$_} } (split //, $_[0]));
  my $fmt_str = "%0" . $n_hex_digits . "x";
  my $value;
  if ($n_hex_digits > 1) {
    $shift_amt = $n_hex_digits == 2 ? 2 : 6;
    $value = ($base64_decode{substr($_[0], 0, 1)} << $shift_amt) + $base64_decode{substr($_[0], 1, 1)}; 
  } else {
    $value = $base64_decode{$_[0]}; 
  }
  sprintf $fmt_str, $value;
}

sub base642hex($) {
  my @b64_strs = unpack("(A2)*", $_[0]); 
  my $last_b64 = pop @b64_strs;
  my $last_hex_str = extbase642hex $last_b64;
  my @b64_nums = map { ($base64_decode{substr($_, 0, 1)} << 6) + $base64_decode{substr($_, 1, 1)}} @b64_strs;
  my @hex_strs = map {sprintf "%03x", $_} @b64_nums;
  my $output_str = join("", @hex_strs) .  $last_hex_str;
  $output_str
}

sub hyphenate_uuid($) {
  join("-", substr($_[0], 0, 8), substr($_[0], 8, 4), 
            substr($_[0], 12, 4), substr($_[0], 16, 4),
            substr($_[0], 20))
}

sub startswith($$) {
  my $affix_length = length($_[1]);
  substr($_[0], 0, $affix_length) eq $_[1]
}

sub endswith($$) {
  my $affix_length = length($_[1]);
  substr($_[0], -$affix_length) eq $_[1]
}

# Indexed Hash Methods
#

sub ihash_get {
  my @raw_output = ihash_all(@_);
  map {first grep defined, @$_} @raw_output;
}

sub ihash_def {
  my @raw_output = ihash_all(@_);
  map {my @def_out = grep defined, @$_; \@def_out;} @raw_output;
}

sub ihash_uniq {
  my @raw_output = ihash_all(@_);
  map {my @uniq_out = uniq grep defined, @$_; \@uniq_out;} @raw_output;
}

sub ihash_freqs {
  my @raw_output = ihash_all(@_);
  map {my %h = %{freqs grep defined, @$_}; \%h; } @raw_output;
} 


# OK, let's solve this problem: You want to look up the 
# same key in multiple hashes that would be exceedingly large
# if you stored them in memory.
# These are special hashes!
# Each hash has a large number of entries but a small
# number of unique but long keys, so you store the keys in an array
# and store indexes of that array as the values of a hash.  
# I'll call that pair of hash with indices as values with the array
# an indexed hash, or ihash for short.
# ihash_all takes as input a reference to an array of keys 
# (or single key which is cast to an array of one key, $ks_ref;
# a minimum key length $min_key_length, and an array of ihashes.
# ihash_all looks up each of the keys in @$ksref in each of 
# the ihashes, and then returns all matches as an array of arrays.

sub ihash_all {
  my ($ks_ref, $min_key_length, @hash_and_val_refs) = @_;
  $ks_ref = [$ks] unless ref $ks_ref; 
  my @index_hash_refs = take_even @hash_and_val_refs;
  my @hash_val_refs = take_odd @hash_and_val_refs;
  my @potential_keys = 
    map{ my $k = $_; my @r = $min_key_length..length($k);
         map {substr($k, 0, $_)} @r } @$ks_ref; 
  my @output;
  for (0..$#hash_val_refs) {
    my %index_hash = %{$index_hash_refs[$_]};
    my @hash_vals = @{$hash_val_refs[$_]};
    my @val_indices = @index_hash{@potential_keys};
    my @output_vals = @hash_vals[@val_indices];
    push @output, \@output_vals;
  }
  @output;
}

sub alph($) {chr($_[0] + 64)}


BEGIN {
  *h2b64 = \&hex2base64;
  *b642h = \&base642hex;
}
74 core/pl/math.pm
# Math utility functions.
# Mostly geometric and statistical stuff.

use constant tau => 2 * 3.14159265358979323846264;
use constant tau2 => tau/2;
use constant tau4 => tau/4;

use constant LOG2  => log 2;
use constant LOG2R => 1 / LOG2;

sub sum  {local $_; my $s = 0; $s += $_ for @_; $s}
sub prod {local $_; my $p = 1; $p *= $_ for @_; $p}
sub mean {scalar @_ && sum(@_) / @_}

sub log2($) {LOG2R * log $_[0]}
sub quant {my ($x, $q) = @_; $q ||= 1;
           my $s = $x < 0 ? -1 : 1; int(abs($x) / $q + 0.5) * $q * $s}

sub dot($$) {local $_; my ($u, $v) = @_;
             sum map $$u[$_] * $$v[$_], 0..min $#{$u}, $#{$v}}

sub l1norm {local $_; sum map abs($_), @_}
sub l2norm {local $_; sqrt sum map $_*$_, @_}

sub interp {
  my $f = shift;
  my @r;
  while (@_) {
    push @r, $_[0] * (1 - $f) + $_[1] * $f;
    shift; shift;
  }
  @r;
}

sub proj($$)
{ local $_; my ($a, $b) = @_;
  my $f = dot($a, $b) / dot($b, $b);
  map $f * $_, @$b }

sub orth($$)
{ local $_; my ($a, $b) = @_;
  my @proj = proj $a, $b;
  map $$a[$_] - $proj[$_], 0..$#{$a} }

sub cross($$)
{ my ($x1, $y1, $z1, $x2, $y2, $z2) = (@{$_[0]}, @{$_[1]});
  ($y1*$z2 - $z1*$y2, $z1*$x2 - $x1*$z2, $x1*$y2 - $y1*$x2) }

sub rdeg($) {$_[0] * 360 / tau}
sub drad($) {$_[0] / 360 * tau}

sub prec {($_[0] * sin drad $_[1], $_[0] * cos drad $_[1])}
sub rpol {(l2norm(@_), rdeg atan2($_[0], $_[1]))}

sub entropy {
  local $_;
  my $sum = sum @_;
  my $t = 0;
  $t += $_ / $sum * ($_ > 0 ? -log2($_ / $sum) : 0) for @_;
  $t;
}

BEGIN {
if (eval {require Math::Trig}) {
  Math::Trig->import('!sec');   # sec() conflicts with stream reducers
  sub haversine {
    local $_;
    my ($t1, $p1, $t2, $p2) = map drad $_, @_;
    my ($dt, $dp) = ($t2 - $t1, $p2 - $p1);
    my $a = clip 0, 1, sin($dp / 2)**2 + cos($p1) * cos($p2) * sin($dt / 2)**2;
    2 * atan2(sqrt($a), sqrt(1 - $a));
  }
}
}
127 core/pl/stream.pm
# Perl stream-related functions.
# Utilities to parse and emit streams of data. Handles the following use cases:

# | $ ni n:10p'a + a'             # emit single value
#   $ ni n:10p'a, a * a'          # emit multiple values vertically
#   $ ni n:10p'r a, a * a'        # emit multiple values horizontally

# The 'pr' function can bypass split /\t/, which is useful in high-throughput
# situations. For example:

# | $ ni n:10p'pr "$_\tfoo"'      # append a new field without splitting

# Lowercase letters followed by underscores are field-extractors that can take an
# array of lines and return an array of field values. These are useful in
# conjunction with the line-reading functions `rw`, `ru`, and `re`.

our @q;
our @F;

sub rl(;$) {return ($_, map rl(), 2..$_[0]) if @_;
            chomp($_ = @q ? shift @q : <STDIN>); @F = split /\t/; $_}
sub pl($)  {chomp, push @q, $_ until @q >= $_[0] || !defined($_ = <STDIN>); @q[0..$_[0]-1]}
sub F_(@)  {@_ ? @F[@_] : @F}
sub FM()   {$#F}
sub FR($)  {@F[$_[0]..$#F]}
sub FT($)  {@F[0..($_[0]-1)]}
sub r(@)   {(my $l = join "\t", @_) =~ s/\n//g; print $l, "\n"; ()}
BEGIN {ceval sprintf 'sub %s():lvalue {@F[%d]}', $_, ord($_) - 97 for 'a'..'l';
       ceval sprintf 'sub %s_ {local $_; map((split /\t/)[%d] || "", map split(/\n/), @_)}',
                     $_, ord($_) - 97, ord($_) - 97 for 'a'..'l'}
BEGIN {ceval sprintf 'sub %s__ {my @r; foreach my $line(@_) 
                                      {my @line_arr = split /\t/, $line; 
                                       my @short = @line_arr[%d..$#line_arr];
                                       push @r, @short} @r }', $_, ord($_) - 97 for 'a'..'l'}

sub cols(@) {
  local $_;
  for my $i (0..min map $#{$_}, @_) {
    r map $_[$_][$i], 0..$#_;
  }
  ();
}

# Hash constructors.
# Pairs of letters you can use to index one column against another. For example,
# `%h = ab_ @lines` is the same as `@h{a_ @lines} = b_ @lines`.

BEGIN {for my $x ('a'..'l') {
         ceval sprintf 'sub %s%s_ {my %r; @r{%s_ @_} = %s_ @_; %r}',
                       $x, $_, $x, $_ for 'a'..'l'}}
BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
         ceval sprintf 'sub %s%sS {my %r; my @key_arr = %s_ @_; my @val_arr = %s_ @_;
                                    $r{$key_arr[$_]} += $val_arr[$_] for 0..$#key_arr; %r}',
                       $x, $y, $x, $y }}}
BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
         ceval sprintf 'sub %s%sSNN {my %r; my @key_arr = %s_ @_; my @val_arr = %s_ @_;
                                    $r{$key_arr[$_]} += $val_arr[$_] for 0..$#key_arr;
                                    my %r_output; @filtered_keys = grep {$_ ne ""} keys %r;
                                    @r_output{@filtered_keys} = @r{@filtered_keys}; %r_output}',
                       $x, $y, $x, $y }}}

BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
          ceval sprintf 'sub %s%sC {my %r; my @key_arr = %s_ @_; my @val_arr = %s_ @_; 
                                    for (0..$#key_arr) { my @keys = split /,/, $key_arr[$_]; 
                                                        my $val = $val_arr[$_];
                                                        $r{$_} = $val for @keys;} 
                                    %r}',
                             $x, $y, $x, $y }}}

# For this, we return a pair consisting of a hash reference and a 
# reference to the lookups of the values of the hash, since
# we'll always want to use them together. We add 1 so that the
# 0 value will be the empty string; this will catch all of the
# invalid lookups.
BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
          ceval sprintf 'sub %s%sc {my %r; my @key_arr = %s_ @_; my @val_arr = %s_ @_; 
                                    for (0..$#key_arr) { my @keys = split /,/, $key_arr[$_]; 
                                                        my $val = $_;
                                                        $r{$_} = $val + 1 for @keys;} 
                                    unshift @val_arr, undef;
                                    \%r, \@val_arr}',
                             $x, $y, $x, $y}}}
## Seeking functions.
# It's possible to read downwards (i.e. future lines), which returns an array and
# sends the after-rejected line into the lookahead queue to be used by the next
# iteration. Mnemonics:

# | rw: read while condition
#   ru: read until condition
#   re: read while equal

# These functions all read things into memory. If you want to stream stuff, you
# can do it in two ways. One is to use control flow with the 'rl' (read line)
# function:

# | do_stuff until rl =~ /<\//;           # iterate until closing XML tag
#   push @q, $_;                          # important: stash rejected line

# The other is to use the faceting functions defined in facet.pm.

sub rw(&) {my @r = ($_); push @r, $_ while  defined rl && &{$_[0]}; push @q, $_ if defined $_; @r}
sub ru(&) {my @r = ($_); push @r, $_ until !defined rl || &{$_[0]}; push @q, $_ if defined $_; @r}
sub re(&) {my ($f, $i) = ($_[0], &{$_[0]}); rw {&$f eq $i}}
sub rea() {re {a}}
BEGIN {ceval sprintf 'sub re%s() {re {join "\t", @F[0..%d]}}',
                     $_, ord($_) - 97 for 'b'..'l'}

# Streaming aggregations.
# These functions are like the ones above, but designed to work in constant
# space:

# | se<column>: streaming reduce while everything up to column is equal
#   sr: streaming reduce all data

sub se(&$@) {my ($f, $e, @xs) = @_; my $k = &$e;
             @xs = &$f(@xs), rl while defined and &$e eq $k;
             push @q, $_ if defined; @xs}
BEGIN {ceval sprintf 'sub se%s(&$@) {
                        my ($f, @xs) = @_;
                        se {&$f(@_)} sub {join "\t", @F[0..%d]}, @xs;
                      }', $_, ord($_) - 97 for 'a'..'l'}

sub sr(&@) {my ($f, @xs) = @_; @xs = &$f(@xs), rl while defined; @xs}
67 core/pl/reducers.pm
# Compound reductions.
# Suppose you want to calculate, in parallel, the sum of one column and the mean
# of another. You can't use two separate `sr` calls since the first one will
# force the whole stream. Instead, you need a way to build a single compound
# function that maintains the two separate state elements. That's what `rc`
# (reduce compound) is all about.

# In fast languages we could probably get away with some nice combinatory stuff
# here, but this is performance-critical and Perl isn't fast. So I'm making some
# epic use of codegen and `eval` to help Perl be all it can be. We end up
# compiling into a single function body for a `cr` call, which is then mapped
# through a finalizer to eliminate intermediate states.

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

# Reduce compound function.
# Executes a compound reduction using the specified stream reducer function.
# Typical usage would be like this:

# | ($sum, $mean) = rc \&sea, fsum A, fmean B;

sub rc {
  my ($f, @rs) = @_;
  my %c        = compound_reducer @rs;
  my $reduce   = eval "sub{\n($c{reduce})\n}" or die "sc: '$c{reduce}': $@";
  my $end      = eval "sub{\n($c{end})\n}"    or die "sc: '$c{end}': $@";
  &$end(&$f($reduce, @{$c{init}}));
}

# Just like for `se` functions, we define shorthands such as `rca ...` = `rc
# \&sea, ...`.

BEGIN {ceval sprintf 'sub rc%s {rc \&se%s, @_}', $_, $_ for 'a'..'q'}
187 core/pl/time.pm
# Time conversion functions.
# Dependency-free functions that do various time-conversion tasks for you in a
# standardized way. They include:

# | @parts = tep($elements, $epoch): convert an epoch to specified pieces
#   $epoch = tpe($elements, @values): convert values to an epoch

# Everything always happens in UTC. If you want a different timezone, you'll need
# to shift your epochs by some multiple of 3600.

use POSIX ();

use constant time_pieces => 'SMHdmYwjDN';

our $mktime_error = 0;          # bugfix for OSX

sub time_element_indexes($) {map index(time_pieces, $_), split //, $_[0]}

sub time_epoch_pieces($;$) {
  local $_;
  my ($es, $t) = $_[0] =~ /^[SMHdmYwjDN]+$/ ? @_ : ('YmdHMS', @_);
  my @pieces = gmtime $t;
  push @pieces, int(1_000_000_000 * ($t - int $t));
  $pieces[5] += 1900;
  $pieces[4]++;
  @pieces[time_element_indexes $es];
}

sub time_pieces_epoch {
  local $_;
  my ($es, @ps) = $_[0] =~ /^[SMHdmYwjDN]+$/ ? @_ : ('YmdHMS', @_);
  my @tvs = (0, 0, 0, 1, 1, 1970, 0, 0, -1, 0);
  @tvs[time_element_indexes $es] = @ps;
  $tvs[5] -= 1900;
  $tvs[4]--;
  POSIX::mktime(@tvs[0..5]) + $tvs[9] / 1_000_000_000 - $mktime_error;
}

# Day of Week and Hour of Day.
# These methods are for converting timestamps in GMT; if you have data from another location on the globe (and you probably do), you'll need to use a timezone shift as described above.

our @days = qw(Thu Fri Sat Sun Mon Tue Wed);
sub day_of_week($) {
  my $ts = $_[0];
  my $weekday = int(($ts % 604800)/86400);
  @days[$weekday];
}

sub hour_of_day($) {
  my $ts = $_[0];
  int(($ts %86400)/3600);
}

sub hour_of_week($) {
  my $ts = $_[0];
  my $dow = day_of_week($ts);
  my $hod = sprintf "%02d", hour_of_day($ts);
  $dow . "_" . $hod;
}

sub year_month($) {
  my @year_month = tep('Ym', $_[0]);
  my $year = @year_month[0];
  my $month = sprintf "%02d", @year_month[1];
  $year . "_" . $month;
}

# Round to day/hour/quarter-hour/minute.

BEGIN {for my $x ('day', 'hour', 'quarter_hour', 'minute') {
         my $dur = $x eq 'day' ? 86400 : $x eq 'hour' ? 3600 : 
                    $x eq 'quarter_hour' ? 900 : $x eq 'minute' ? 60 : 0; 
         ceval sprintf 'sub truncate_to_%s($) {my $ts = $_[0]; %d * int($ts/%d)}',
                       $x, $dur, $dur}}

# Approximate timezone shifts by lat/lng.
# Uses the Bilow-Steinmetz approximation to quickly calculate a timezone offset
# (in seconds, which can be added to a GMT epoch) for a given latitude/longitude.
# It may be off by a few hours but is generally unbiased.

sub timezone_seconds {
  my ($lat, $lng) = @_;
  240 * int($lng + 7);
}

sub gh60_localtime($$) {
  my ($ts, $gh) = @_;
  my ($lat, $lng) = ghd $gh, 60;
  $ts + timezone_seconds($lat, $lng);
}

sub gh_localtime($$) {
  my ($ts, $gh) = @_;
  my ($lat, $lng) = ghd $gh;
  $ts + timezone_seconds($lat, $lng);
}

{
  my $t = time;
  $mktime_error = time_pieces_epoch(time_epoch_pieces $t) - $t;
}


# ISO 8601 is a standard format for time data; it looks like: 
# 2017-06-24T07:58:59+00:00 or 2017-06-24T07:58:59Z
# There's also a form with no colons or dashes that's supported:
# 20170624T075859Z
# And also a form with a space between the time and the date:
# 2017-06-24 07:58:59.729
# The added or subtracted amount at the end corresponds to the
# local timezone.

sub iso_8601_epoch {
  my $iso_time = $_[0];
  my ($date_part, $time_part) = split /[\sT]/, $iso_time;
  my $y, $m, $d;
  if ($date_part !~ /^\d{4}-/) {
    ($y, $m, $d) = /^(\d{4})(\d{2})(\d{2})/;
  } else {
    ($y, $m, $d) = split /-/, $date_part;
  }

  return time_pieces_epoch($y, $m, $d) unless $time_part;

  my ($h, $min, $s, $tz_part) = ($time_part =~ /^(\d{2}):?(\d{2}):?([0-9.]{2,})([Z+-].*)?$/);
  my $raw_ts = time_pieces_epoch($y, $m, $d, $h, $min, $s);
  return $raw_ts unless $tz_part;
  
  my ($offset_type, $offset_hr, $offset_min) = ($tz_part =~ /([+-])(\d{2}):?(\d{2})?/);

  my $offset_amt = $offset_type eq "-" ? 1 : -1; 
  my $offset = $offset_amt * (3600 * $offset_hr + 60 * $offset_min); 
  $raw_ts + $offset;
}

# Converts an epoch timestamp to the corresponding 
# time zone; gives local time when a second argument
# corresponding to the local timezone is given.

sub epoch_iso_8601($;$) {
  my ($epoch, $tz_raw) = $#_ == 2 ? @_ : (@_ , "Z");
  my $epoch_offset;
  my $tz_str;
  if ($tz_raw =~ /^-?\d+\.?\d*$/) {
    die("badly formatted ISO timezone: $tz_raw\n") if abs $tz_raw > 12;
    $epoch_offset = $tz_raw*3600;
    my $tz_hr = int($tz_raw);
    my $tz_min = abs int(($tz_raw - $tz_hr)*60);
    my $tz_sign = $tz_raw < 0 ? "-" : "+";
    $tz_str = sprintf "%s%02d:%02d", $tz_sign, abs $tz_hr, $tz_min;
  } elsif ($tz_raw =~ /^[+-]\d{1,2}:?(\d{2})?$/) {
    my $tz_sign = substr($tz_raw, 0, 1);
    my ($tz_hr, $tz_min) = ($tz_raw =~ /^[+-](\d{1,2}):?(\d{2})?$/);
    $tz_str = sprintf "%s%02d:%02d", $tz_sign, $tz_hr, $tz_min || 0;
    my $offset_amt = 3600 * $tz_hr + 60 * ($tz_min || 0);
    $epoch_offset = $tz_sign eq "+"? $offset_amt : -$offset_amt;
  } elsif ($tz_raw eq "Z") {
    $epoch_offset = 0;
    $tz_str = $tz_raw;
  } else {
    die("badly formatted ISO 8601 timestamp: $tz_raw");
  }
  $epoch += $epoch_offset;
  my ($y, $m, $d, $h, $min, $s) = time_epoch_pieces($epoch);
  my $iso_time = sprintf "%d-%02d-%02dT%02d:%02d:%02d%s", $y, $m, $d, $h, $min, $s, $tz_str;
  $iso_time;
}


BEGIN {
  *tep  = \&time_epoch_pieces;
  *tpe  = \&time_pieces_epoch;
  *tsec = \&timezone_seconds;
  *ghl = \&gh_localtime;
  *gh6l = \&gh60_localtime;
  *dow = \&day_of_week;
  *hod = \&hour_of_day;
  *how = \&hour_of_week;
  *ym = \&year_month;
  *ttd = \&truncate_to_day;
  *tth = \&truncate_to_hour;
  *tt15 = \&truncate_to_quarter_hour;
  *ttm = \&truncate_to_minute;
  *i2e = \&iso_8601_epoch;
  *e2i = \&epoch_iso_8601;
}

170 core/pl/geohash.pl
# Fast, portable geohash encoder.
# A port of https://www.factual.com/blog/how-geohashes-work that works on 32-bit
# Perl builds.

BEGIN {

use Scalar::Util qw/looks_like_number/;

our @geohash_alphabet = split //, '0123456789bcdefghjkmnpqrstuvwxyz';
our %geohash_decode   = map(($geohash_alphabet[$_], $_), 0..$#geohash_alphabet);

if (1 << 32) {
  *morton_gap = sub($) {
    my ($x) = @_;
    $x |= $x << 16; $x &= 0x0000ffff0000ffff;
    $x |= $x << 8;  $x &= 0x00ff00ff00ff00ff;
    $x |= $x << 4;  $x &= 0x0f0f0f0f0f0f0f0f;
    $x |= $x << 2;  $x &= 0x3333333333333333;
    return ($x | $x << 1) & 0x5555555555555555;
  };

  *morton_ungap = sub($) {
    my ($x) = @_;  $x &= 0x5555555555555555;
    $x ^= $x >> 1; $x &= 0x3333333333333333;
    $x ^= $x >> 2; $x &= 0x0f0f0f0f0f0f0f0f;
    $x ^= $x >> 4; $x &= 0x00ff00ff00ff00ff;
    $x ^= $x >> 8; $x &= 0x0000ffff0000ffff;
    return ($x ^ $x >> 16) & 0x00000000ffffffff;
  };

  *geohash_encode = sub {
    local $_;
    my ($lat, $lng, $precision) = @_;
    $precision ||= 12;
    my $bits = $precision > 0 ? $precision * 5 : -$precision;
    my $gh   = (morton_gap(int(($lat +  90) / 180 * 0x40000000)) |
                morton_gap(int(($lng + 180) / 360 * 0x40000000)) << 1)
               >> 60 - $bits;

    $precision > 0 ? join '', reverse map $geohash_alphabet[$gh >> $_ * 5 & 31],
                                          0 .. $precision - 1
                   : $gh;
  };

  *geohash_tagged_precision = sub {
    my ($gh) = @_;
    $gh &= 0x3fff_ffff_ffff_ffff;
    my $bits = 0;
    for (my $b = 32; $b; $b >>= 1) {
      $bits |= $b if ($gh & ~(-1 << ($bits | $b))) != $gh;
    }
    $bits;
  };

  *geohash_decode_tagged = sub {
    my ($gh) = @_;
    $gh &= 0x3fff_ffff_ffff_ffff;
    my $tag_bit_index = geohash_tagged_precision($gh);
    geohash_decode($gh & ~(-1 << $tag_bit_index), $tag_bit_index);
  };

  *geohash_decode = sub {
    local $_;
    my ($gh, $bits) = @_;
    return geohash_decode_tagged($gh)
      if looks_like_number $gh and $gh & 0x4000_0000_0000_0000;
    unless (defined $bits) {
      # Decode gh from base-32
      $bits = length($gh) * 5;
      my $n = 0;
      $n = $n << 5 | $geohash_decode{lc $_} for split //, $gh;
      $gh = $n;
    }
    $gh <<= 60 - $bits;
    return (morton_ungap($gh)      / 0x40000000 * 180 -  90,
            morton_ungap($gh >> 1) / 0x40000000 * 360 - 180);
  };
} else {
  *morton_gap = sub($) {
    my ($x) = @_;
    $x |= $x << 8;  $x &= 0x00ff00ff;
    $x |= $x << 4;  $x &= 0x0f0f0f0f;
    $x |= $x << 2;  $x &= 0x33333333;
    return ($x | $x << 1) & 0x55555555;
  };

  *morton_ungap = sub($) {
    my ($x) = @_;  $x &= 0x55555555;
    $x ^= $x >> 1; $x &= 0x33333333;
    $x ^= $x >> 2; $x &= 0x0f0f0f0f;
    $x ^= $x >> 4; $x &= 0x00ff00ff;
    return ($x ^= $x >> 8) & 0x0000ffff;
  };

  *geohash_encode = sub {
    local $_;
    my ($lat, $lng, $precision) = (@_, 12);
    my $unit_lat = ($lat + 90)  / 180;
    my $unit_lng = ($lng + 180) / 360;
    my $high_30  = morton_gap($unit_lat * 0x8000)
                 | morton_gap($unit_lng * 0x8000) << 1;
    my $low_30   = morton_gap($unit_lat * 0x40000000 & 0x7fff)
                 | morton_gap($unit_lng * 0x40000000 & 0x7fff) << 1;

    my $gh12 = join '', map($geohash_alphabet[$high_30 >> 30 - 5*$_ & 31], 1..6),
                        map($geohash_alphabet[$low_30  >> 30 - 5*$_ & 31], 1..6);
    substr $gh12, 0, $precision;
  };

  *geohash_decode = sub {
    local $_;
    my $gh12 = "$_[0]s" . "0" x 11;
    my ($low_30, $high_30) = (0, 0);
    for (0..5) {
      $low_30  = $low_30  << 5 | $geohash_decode{lc substr $gh12, $_ + 6, 1};
      $high_30 = $high_30 << 5 | $geohash_decode{lc substr $gh12, $_    , 1};
    }
    my $lat_int = morton_ungap($low_30)      | morton_ungap($high_30)      << 15;
    my $lng_int = morton_ungap($low_30 >> 1) | morton_ungap($high_30 >> 1) << 15;
    ($lat_int / 0x40000000 * 180 - 90, $lng_int / 0x40000000 * 360 - 180);
  };
}

*ghe = \&geohash_encode;
*ghd = \&geohash_decode;

*gh_box = sub {
  local $_;
  my $gh = shift;
  my $northeast_corner = substr($gh . "z" x 12, 0, 12);
  my $southwest_corner = substr($gh . "0" x 12, 0, 12);
  my ($north, $east) = ghd($northeast_corner);
  my ($south, $west) = ghd($southwest_corner);
  ($north, $south, $east, $west);
  };

*ghb = \&gh_box;
}

sub to_radians {
  3.1415926535897943284626 * $_[0]/180.0;
}

sub earth_radius_in_units {
  if ($_[0] eq "km") {
    6371} elsif ($_[0] eq "mi") {
    3959} elsif ($_[0] eq "m") {
    6371E3} elsif ($_[0] eq "ft") {
    20903520 }  #3959 * 5280
     else { -1 }
}

sub lat_lon_dist {
  my $units = @_ == 4 ? "km" : pop;
  my ($lat1, $lon1, $lat2, $lon2) = @_;
  my $earth_radius = earth_radius_in_units $units;
  my $phi1 = to_radians $lat1;
  my $phi2 = to_radians $lat2;
  my $d_phi = $phi1 - $phi2;
  my $d_lambda = to_radians ($lon1 - $lon2);
  my $a = sin($d_phi/2)**2 + cos($phi1)*cos($phi2)*(sin($d_lambda/2)**2);
  my $c = 2 * atan2(sqrt($a), sqrt(1-$a));
  $earth_radius * $c
}

sub gh_dist {
  my @lat_lons;
  push @lat_lons, ghd($_[0]), ghd($_[1]), ($_[2] || "km");
  lat_lon_dist @lat_lons;
}
167 core/pl/pl.pl
# Perl parse element.
# A way to figure out where some Perl code ends, in most cases. This works
# because appending closing brackets to valid Perl code will always make it
# invalid. The same property holds across most code-to-code functions. This is
# how ni figures out whether a closing bracket is a lambda-terminator or a part
# of some row mapping code.

use POSIX ();

sub syntax_check($$) {
  my ($check_cmd, $code) = @_;
  my $fh = siproc {sh "$check_cmd >/dev/null 2>&1"};
  safewrite $fh, $code;
  close $fh;
  $fh->await;
}

# We need to explicitly disable any BEGIN{} blocks to avoid executing side
# effects. We can guarantee that nothing will run (beyond `use` statements, which
# we assume are safe) by removing any occurrences of the string `BEGIN` and
# replacing them with something syntactically equivalent but less volatile -- in
# this case, `END`.

BEGIN {
defparser 'plcode', '$', q{
  return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
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

  ($code, $x, @xs);
};
}

# Perl wrapper.
# Defines the `p` operator, which can be modified in a few different ways to do
# different things. By default it functions as a one-in, many-out row
# transformer.

use constant perl_mapgen => gen q{
  package ni::pl;
  %prefix
  %closures
  close STDIN;
  open STDIN, '<&=3' or die "ni: failed to open fd 3: $!";
  sub row {
    %body
  }
  while (defined rl) {
    %each
  }
};

our @perl_prefix_keys = qw| core/pl/util.pm
                            core/pl/json_util.pm
                            core/pl/hash_util.pm
                            core/pl/math.pm
                            core/pl/stream.pm
                            core/pl/geohash.pl
                            core/pl/time.pm
                            core/cell/murmurhash.pl
                            core/bloom/bloomfilter.pl
                            core/gen/gen.pl
                            core/json/json.pl
                            core/pl/reducers.pm |;

sub defperlprefix($) {push @perl_prefix_keys, $_[0]}

sub perl_prefix() {join "\n", @ni::self{@perl_prefix_keys}}

sub perl_quote($)    {(my $x = $_[0]) =~ s/([\\'])/\\$1/g; "'$x'"}
sub perl_closure($$) {"use constant $_[0] => " . perl_quote($_[1]) . ";"}
sub perl_closures()
{join "\n", map perl_closure($_, closure_data $_), closure_keys}

sub perl_expand_begin($) {sr $_[0], qr/^\^/, 'BEGIN'}

sub stdin_to_perl($) {
  eval {cdup2 0, 3; POSIX::close 0};
  die "ni: perl driver failed to move FD 0 to 3 ($!)\n"
    . "    this usually means you're running in a context with no STDIN"
  if $@;
  safewrite siproc {exec 'perl', '-'}, $_[0];
}

sub perl_code($$) {perl_mapgen->(prefix   => perl_prefix,
                                 closures => perl_closures,
                                 body     => $_[0],
                                 each     => $_[1])}

sub perl_mapper($)   {perl_code perl_expand_begin $_[0], 'ref $_ ? r(@$_) : length && print $_, "\n" for row'}
sub perl_grepper($)  {perl_code perl_expand_begin $_[0], 'print $_, "\n" if row'}
sub perl_asserter($) {perl_code perl_expand_begin $_[0], q(
  print $_, "\n";
  unless (row) {
    sleep 1;
    die "\r\033[KASSERTION " . q#) . $_[0] . q(# . " FAILED at line $.: $_";
  }
)}

defoperator perl_mapper  => q{stdin_to_perl perl_mapper   $_[0]};
defoperator perl_grepper => q{stdin_to_perl perl_grepper  $_[0]};
defoperator perl_assert  => q{stdin_to_perl perl_asserter $_[0]};

defoperator perl_cell_transformer => q{
  my ($colspec, $code) = @_;
  my ($limit, @cols) = @$colspec;
  my $gen = gen q{
    for my $fi (%cols) {
      $_ = $F[$fi];
      $F[$fi] = row;
    }
    r @F;
  };
  stdin_to_perl perl_mapgen->(
    prefix   => perl_prefix,
    closures => perl_closures,
    body     => perl_expand_begin $code,
    each     => $gen->(cols => @cols ? join ',', @cols : '0..$#F'));
};

defmetaoperator perl_require => q{
  my ($args, $left, $right) = @_;
  my $code_fh = sni @$args;
  my $code    = 'BEGIN{' . join('', <$code_fh>) . "\n}";
  my $key     = "core/pl/require/" . gensym;
  self_append_resource $key, $code;
  push @ni::perl_prefix_keys, $key;
  ($left, $right);
};

BEGIN {
  defparseralias perl_mapper_code         => plcode \&perl_mapper;
  defparseralias perl_grepper_code        => plcode \&perl_grepper;
  defparseralias perl_asserter_code       => plcode \&perl_asserter;
  defparseralias perl_cell_transform_code => plcode \&perl_mapper;      # [sic]
}

defshort '/p',
  defalt 'perlalt', 'alternatives for /p perl operator',
    pmap q{perl_mapper_op $_}, perl_mapper_code;

defshort '/pR', pmap q{perl_require_op @$_}, _qfn;

defrowalt pmap q{perl_grepper_op $_},
          pn 1, pstr 'p', perl_grepper_code;

defassertdsp 'p', pmap q{perl_assert_op $_}, perl_asserter_code;

defshort 'cell/p', pmap q{perl_cell_transformer_op @$_},
                   pseq colspec, perl_cell_transform_code;
2 core/bloom/lib
bloomfilter.pl
bloom.pl
92 core/bloom/bloomfilter.pl
# Bloom filter library.
# A simple pure-Perl implementation of Bloom filters.

eval {require Digest::MD5; Digest::MD5->import('md5')};

# Swiped from https://hur.st/bloomfilter
sub bloom_args($$) {
  my ($n, $p) = @_;
  my $m = int 1 + $n * -log($p) / log(2 ** log 2);
  my $k = int 0.5 + log(2) * $m / $n;
  ($m, $k);
}

sub bloom_new($$) {
  my ($m, $k) = @_;
  ($m, $k) = bloom_args($m, $k) if $k < 1;
  pack("NN", $m, $k) . "\0" x ($m + 7 >> 3);
}

sub bloom_from_hex($) {
  pack 'H*', $_[0];
}

sub multihash($$) {
  my @hs;
  push @hs, unpack "N4", md5($_[0] . scalar @hs) until @hs >= $_[1];
  @hs[0..$_[1]-1];
}

# Destructively adds an element to the filter and returns the filter.
sub bloom_add($$) {
  my ($m, $k) = unpack "NN", $_[0];
  vec($_[0], $_ % $m + 64, 1) = 1 for multihash $_[1], $k;
  $_[0];
}

sub bloom_contains($$) {
  my ($m, $k) = unpack "NN", $_[0];
  vec($_[0], $_ % $m + 64, 1) || return 0 for multihash $_[1], $k;
  1;
}

# Prehashing variants
# prehash($m, $k, $element) -> "prehash_string"
# bloom_add_prehashed($filter, "prehash_string")
sub bloom_prehash($$$) {
  my ($m, $k) = @_;
  unpack "H*", pack "N*", map $_ % $m + 64, multihash $_[2], $k;
}

sub bloom_add_prehashed($$) {
  vec($_[0], $_, 1) = 1 for unpack "N*", pack "H*", $_[1];
  $_[0];
}

sub bloom_contains_prehashed($$) {
  vec($_[0], $_, 1) || return 0 for unpack "N*", pack "H*", $_[1];
  1;
}

# Set operations
sub bloom_intersect {
  local $_;
  my ($n, $k, $filter) = unpack "NNa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "NN", $_;
    die "cannot intersect two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter &= unpack "x8 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_union {
  local $_;
  my ($n, $k, $filter) = unpack "NNa*", shift;
  for (@_) {
    my ($rn, $rk) = unpack "NN", $_;
    die "cannot union two bloomfilters of differing parameters "
      . "($n, $k) vs ($rn, $rk)"
      unless $n == $rn && $k == $rk;
    $filter |= unpack "x8 a*", $_;
  }
  pack("NN", $n, $k) . $filter;
}

sub bloom_count($) {
  my ($m, $k, $bits) = unpack "NN %32b*", $_[0];
  return -1 if $bits >= $m;     # overflow case (> if %32b runs past end maybe)
  $m * -log(1 - $bits/$m) / $k;
}
52 core/bloom/bloom.pl
# Bloom filter operators.
# Operators to construct and query bloom filters. The bloom constructor is a
# sub-operator of `z` (compress), and querying is done using `rb`.
#
# Notation is two digits: power of ten #elements, and power of ten
# false-positive probability. So `zB74` means "create a filter designed to
# store 10^7 (== 10M) elements with a 10^-4 likelihood of false positives."

BEGIN {
  defparseralias bloom_size_spec => pmap q{10 **  $_}, prx qr/\d/;
  defparseralias bloom_fp_spec   => pmap q{10 ** -$_}, prx qr/\d/;
}

defoperator bloomify => q{
  my ($n, $p) = @_;
  my $f = bloom_new $n, $p;
  chomp, bloom_add $f, $_ while <STDIN>;
  print $f;
};

defoperator bloomify_hex => q{
  my ($n, $p) = @_;
  my $f = bloom_new $n, $p;
  chomp, bloom_add $f, $_ while <STDIN>;
  print unpack("H*", $f), "\n";
};

defoperator bloomify_prehashed => q{
  my ($n, $p) = @_;
  my $f = bloom_new $n, $p;
  chomp, bloom_add_prehashed $f, $_ while <STDIN>;
  print $f;
};

defshort '/zB',  pmap q{bloomify_op @$_},           pseq bloom_size_spec, bloom_fp_spec;
defshort '/zBH', pmap q{bloomify_hex_op @$_},       pseq bloom_size_spec, bloom_fp_spec;
defshort '/zBP', pmap q{bloomify_prehashed_op @$_}, pseq bloom_size_spec, bloom_fp_spec;

defoperator bloom_rows => q{
  my ($include_mode, $col, $bloom_lambda) = @_;
  my $bloom;
  my $r = sni @$bloom_lambda;
  1 while read $r, $bloom, 65536, length $bloom;
  $r->await;
  while (<STDIN>) {
    chomp(my @cols = split /\t/, $_, $col + 2);
    print if !$include_mode == !bloom_contains $bloom, $cols[$col];
  }
};

defrowalt pmap q{bloom_rows_op 1, @$_}, pn [1, 2], pstr 'b', colspec1, _qfn;
defrowalt pmap q{bloom_rows_op 0, @$_}, pn [1, 2], pstr 'B', colspec1, _qfn;
2 core/cell/lib
murmurhash.pl
cell.pl
28 core/cell/murmurhash.pl
# Pure-Perl MurmurHash3_32 implementation.
# This is used by some intification operators and based on the Wikipedia
# implementation. It's limited to 32 bits because otherwise ni will fail on 32-bit
# machines.

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

  my ($r) = unpack 'V', substr($_[0], ~3 & length $_[0]) . "\0\0\0\0";
  $r *= murmur_c1;
  $h ^= ($r << 15 | $r >> 17 & 0x7fff) * murmur_c2 & 0xffffffff ^ length $_[0];
  $h &= 0xffffffff;
  $h  = ($h ^ $h >> 16) * 0x85ebca6b & 0xffffffff;
  $h  = ($h ^ $h >> 13) * 0xc2b2ae35 & 0xffffffff;
  return $h ^ $h >> 16;
}
162 core/cell/cell.pl
# Cell-level operators.
# Cell-specific transformations that are often much shorter than the equivalent
# Perl code. They're also optimized for performance.

defcontext 'cell', q{cell operator context};
defshort '/,', parser 'cell/qfn';

BEGIN {
  defparseralias cellspec       => pmap q{$_ || [1, 0]}, popt colspec;
  defparseralias cellspec_fixed => pmap q{$_ || [1, 0]}, popt colspec_fixed;
}

# Codegen.
# Most of these have exactly the same format and take a column spec.

use constant cell_op_gen => gen q{
  BEGIN {eval {require Digest::MD5; Digest::MD5->import(qw/md5 md5_hex/)}}
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

# Intification.
# Strategies to turn each distinct entry into a number. Particularly useful in a
# plotting context.

defoperator intify_compact => q{
  cell_eval {args  => 'undef',
             begin => 'my %ids; my $n = 0',
             each  => '$xs[$_] = ($ids{$xs[$_]} ||= ++$n) - 1'}, @_;
};

defoperator intify_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = unpack "N", md5 $xs[$_] . $seed'}, @_;
};

defoperator real_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = unpack("N", md5 $xs[$_] . $seed) / (1<<32)'}, @_;
};

defoperator md5 => q{
  cell_eval {args  => 'undef',
             begin => '',
             each  => '$xs[$_] = md5_hex $xs[$_]'}, @_;
};

defshort 'cell/z', pmap q{intify_compact_op $_},  cellspec_fixed;
defshort 'cell/h', pmap q{intify_hash_op    @$_}, pseq cellspec_fixed, popt integer;
defshort 'cell/H', pmap q{real_hash_op      @$_}, pseq cellspec_fixed, popt integer;

defshort 'cell/m', pmap q{md5_op $_}, cellspec_fixed;

defoperator bloom_prehash => q{
  cell_eval {args  => '$m, $k',
             begin => '($m, $k) = bloom_args $m, $k',
             each  => '$xs[$_] = bloom_prehash $m, $k, $xs[$_]'}, @_;
};

defshort 'cell/BP', pmap q{bloom_prehash_op @$_},
  pseq cellspec_fixed, bloom_size_spec, bloom_fp_spec;

# Numerical transformations.
# Trivial stuff that applies to each cell individually.

BEGIN {
  defparseralias quant_spec  => pmap q{$_ || 1}, popt number;
  defparseralias log_base    => pmap q{$_ || exp 1}, popt number;
  defparseralias jitter_bias => pmap q{dor $_, 0}, popt number;
  defparseralias jitter_mag  => pmap q{$_ || 1},   palt pmap(q{0.9}, prx ','),
                                                        popt number;
}

defoperator cell_log => q{
  my ($cs, $base) = @_;
  my $lb = 1 / log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = log(max 1e-16, \$xs[\$_]) * $lb"}, $cs;
};

defoperator cell_exp => q{
  my ($cs, $base) = @_;
  my $eb = log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = exp $eb * \$xs[\$_]"}, $cs;
};

defshort 'cell/l', pmap q{cell_log_op @$_}, pseq cellspec_fixed, log_base;
defshort 'cell/e', pmap q{cell_exp_op @$_}, pseq cellspec_fixed, log_base;

# Log-progression that preserves sign of the values. Everything is shifted
# upwards by one in log-space, so signed_log(0) == log(1) == 0.
defoperator cell_signed_log => q{
  my ($cs, $base) = @_;
  my $lb = 1 / log $base;
  cell_eval {
    args => 'undef',
    each => "\$xs[\$_] = (\$xs[\$_] > 0 ? $lb : -$lb) * log(1 + abs \$xs[\$_])"}, $cs;
};

defshort 'cell/L', pmap q{cell_signed_log_op @$_}, pseq cellspec_fixed, log_base;

defoperator jitter_uniform => q{
  my ($cs, $mag, $bias) = @_;
  my $adjust = $bias - $mag / 2;
  cell_eval {args => 'undef', each => "\$xs[\$_] += rand() * $mag + $adjust"}, $cs;
};

defshort 'cell/j', pmap q{jitter_uniform_op @$_},
                   pseq cellspec_fixed, jitter_mag, jitter_bias;


defoperator quantize => q{
  my ($cs, $q) = @_;
  my $iq = 1 / $q;
  cell_eval {args => 'undef',
             each => "\$xs[\$_] = $q * int(0.5 + $iq * \$xs[\$_])"}, $cs;
};

defshort 'cell/q', pmap q{quantize_op @$_}, pseq cellspec_fixed, quant_spec;

# Streaming numeric transformations.
# Sum, delta, average, variance, entropy, etc. Arguably these are column operators and
# not cell operators, but in practice you tend to use them in the same context as
# things like log scaling.

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

defoperator col_average => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols; $. = 0',
             each  => '$xs[$_] = ($ns[$_] += $xs[$_]) / $.'}, @_;
};

defshort 'cell/a', pmap q{col_average_op $_}, cellspec_fixed;
defshort 'cell/s', pmap q{col_sum_op     $_}, cellspec_fixed;
defshort 'cell/d', pmap q{col_delta_op   $_}, cellspec_fixed;
2 core/rb/lib
prefix.rb
rb.pl
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
75 core/rb/rb.pl
# Ruby code element.
# This works just like the Perl code parser but is slightly less involved because
# there's no `BEGIN/END` substitution. We also don't need to take a code
# transform because no amount of wrapping will change whether an expression can
# be parsed.

use POSIX ();

BEGIN {
defparser 'rbcode', '', q{
  return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
  my ($self, $code, @xs) = @_;
  my ($x, $status) = ('', 0);
  $x .= ']' while $status = syntax_check 'ruby -c -', $code and $code =~ s/\]$//;
  die <<EOF if $status;
ni: failed to get closing bracket count for ruby code "$code$x"; this means
    your code has a syntax error.
EOF
  ($code, $x, @xs);
};
}

# Ruby wrapper.

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
  exit 0
};

use constant ruby_prefix => join "\n", @ni::self{qw| core/rb/prefix.rb |};

sub stdin_to_ruby($) {
  cdup2 0, 3;
  POSIX::close 0;
  safewrite siproc {exec 'ruby', '-'}, $_[0];
}

sub ruby_code($$) {ruby_mapgen->(prefix => ruby_prefix,
                                 body   => $_[0],
                                 each   => $_[1])}

sub ruby_mapper($)  {ruby_code $_[0], 'map_mode! x'}
sub ruby_grepper($) {ruby_code $_[0], 'puts $l if x'}

defoperator ruby_mapper  => q{stdin_to_ruby ruby_mapper  $_[0]};
defoperator ruby_grepper => q{stdin_to_ruby ruby_grepper $_[0]};

defshort '/m',
  defalt 'rubyalt', 'alternatives for the /m ruby operator',
    pmap q{ruby_mapper_op $_}, rbcode;

defrowalt pmap q{ruby_grepper_op $_}, pn 1, pstr 'm', rbcode;
2 core/lisp/lib
prefix.lisp
lisp.pl
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
53 core/lisp/lisp.pl
# Lisp backend.
# A super simple SBCL operator. The first thing we want to do is to define the
# code template that we send to Lisp via stdin (using a heredoc). So ni ends up
# generating a pipeline element like this:

# | ... | sbcl --noinform --script 3<&0 <<'EOF' | ...
#         (prefix lisp code)
#         (line mapping code)
#         EOF

use POSIX ();

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

# Now we specify which files get loaded into the prefix. File paths become keys
# in the %self hash.

sub lisp_prefix() {join "\n", @ni::self{qw| core/lisp/prefix.lisp |}}

# Finally we define the toplevel operator. 'root' is the operator context, 'L' is
# the operator name, and pmap {...} mrc '...' is the parsing expression that
# consumes the operator's arguments (in this case a single argument of just some
# Lisp code) and returns a shell command. (See src/sh.pl.sdoc for details about
# how shell commands are represented.)

BEGIN {defparseralias lispcode => prc '.*[^]]+'}

defoperator lisp_code => q{
  my ($code) = @_;
  cdup2 0, 3;
  POSIX::close 0;
  safewrite siproc {exec qw| sbcl --noinform --noprint --eval |,
                         '(load *standard-input* :verbose nil :print nil)'},
            $code;
};

defshort '/l', pmap q{lisp_code_op lisp_mapgen->(prefix => lisp_prefix,
                                                 body   => $_)},
               lispcode;

defrowalt pmap q{lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
                                             body   => $_)},
          pn 1, pstr 'l', lispcode;
1 core/sql/lib
sql.pl
129 core/sql/sql.pl
# SQL parsing context.
# Translates ni CLI grammar to a SELECT query. This is a little interesting
# because SQL has a weird structure to it; to help with this I've also got a
# 'sqlgen' abstraction that figures out when we need to drop into a subquery.

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

# SQL code parse element.
# Counts brackets outside quoted strings.

BEGIN {defparseralias sqlcode => generic_code}

# Code compilation.
# Parser elements can generate one of two things: [method, @args] or
# {%modifications}. Compiling code is just starting with a SQL context and
# left-reducing method calls.

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

# SQL operator mapping.
# For the most part we model SQL operations the same way that we address Spark
# RDDs, though the mnemonics are a mix of ni and SQL abbreviations.

BEGIN {defcontext 'sql', q{SQL generator context}}
BEGIN {defparseralias sql_table => pmap q{sqlgen $_}, prc '^[^][]*'}
BEGIN {defparseralias sql_query => pmap q{sql_compile $$_[0], @{$$_[1]}},
                                   pseq sql_table, popt parser 'sql/qfn'}

defshort 'sql/m', pmap q{['map', $_]}, sqlcode;
defshort 'sql/u', pk ['uniq'];

defshort 'sql/r',
  defalt 'sqlrowalt', 'alternatives for sql/r row operator',
    pmap(q{['take',   $_]}, integer),
    pmap(q{['filter', $_]}, sqlcode);

defshort 'sql/j',
  defalt 'sqljoinalt', 'alternatives for sql/j join operator',
    pmap(q{['ljoin', $_]}, pn 1, pstr 'L', sql_query),
    pmap(q{['rjoin', $_]}, pn 1, pstr 'R', sql_query),
    pmap(q{['njoin', $_]}, pn 1, pstr 'N', sql_query),
    pmap(q{['ijoin', $_]}, sql_query);

defshort 'sql/g', pmap q{['order_by', $_]},        sqlcode;
defshort 'sql/o', pmap q{['order_by', "$_ ASC"]},  sqlcode;
defshort 'sql/O', pmap q{['order_by', "$_ DESC"]}, sqlcode;

defshort 'sql/+', pmap q{['union',      $_]}, sql_query;
defshort 'sql/*', pmap q{['intersect',  $_]}, sql_query;
defshort 'sql/-', pmap q{['difference', $_]}, sql_query;

# Global operator.
# SQL stuff is accessed using Q, which delegates to a sub-parser that handles
# configuration/connections. The dev/compile delegate is provided so you can see
# the SQL code being generated.

defoperator sql_preview => q{sio; print "$_[0]\n"};

defshort '/Q',
  defdsp 'sqlprofile', 'dispatch for SQL profiles',
    'dev/compile' => pmap q{sql_preview_op($_[0])}, sql_query;
1 core/python/lib
python.pl
41 core/python/python.pl
# Python stuff.
# A context for processing stuff in Python, as well as various functions to
# handle the peculiarities of Python code.

# Indentation fixing.
# This is useful in any context where code is artificially indented, e.g. when
# you've got a multiline quotation and the first line appears outdented because
# the quote opener has taken up space:

# | my $python_code = q{import numpy as np
#                       print np};
#   # -----------------| <- this indentation is misleading

# In this case, we want to have the second line indented at zero, not at the
# apparent indentation. The pydent function does this transformation for you, and
# correctly handles Python block constructs:

# | my $python_code = pydent q{if True:
#                                print "well that's good"};

sub pydent($) {
  my @lines   = split /\n/, $_[0];
  my @indents = map length(sr $_, qr/\S.*$/, ''), @lines;
  my $indent  = @lines > 1 ? $indents[1] - $indents[0] : 0;

  $indent = min $indent - 1, @indents[2..$#indents]
    if $lines[0] =~ /:\s*(#.*)?$/ && @lines >= 2;

  my $spaces = ' ' x $indent;
  $lines[$_] =~ s/^$spaces// for 1..$#lines;
  join "\n", @lines;
}

sub pyquote($) {"'" . sgr(sgr($_[0], qr/\\/, '\\\\'), qr/'/, '\\\'') . "'"}

# Python code parse element.
# Counts brackets, excluding those inside quoted strings. This is more efficient
# and less accurate than Ruby/Perl, but the upside is that errors are not
# particularly common.

defparseralias pycode => pmap q{pydent $_}, generic_code;
3 core/binary/lib
bytestream.pm
bytewriter.pm
binary.pl
29 core/binary/bytestream.pm
# Binary byte stream driver.
# Functions that read data in blocks. The lookahead is 8192 bytes by default, but
# you can peek further using the 'pb' function.

our $stdin_ok = 1;
our $offset = 0;
our $binary = '';

sub bi() {$offset}

sub pb($) {
  use bytes;
  $stdin_ok &&= sysread STDIN, $binary, $_[0], length $binary
    while $stdin_ok && length($binary) < $_[0];
  substr $binary, 0, $_[0];
}

sub available() {length pb 1}

# TODO: optimize
sub rb($) {
  use bytes;
  my $r = pb $_[0];
  $binary = substr $binary, $_[0];
  $offset += $_[0];
  $r;
}

sub rp($) {unpack $_[0], rb length pack $_[0], unpack $_[0], $binary}
7 core/binary/bytewriter.pm
# Byte writer.
# Convenience functions that make it easier to write binary data to standard out.
# This library gets added to the perl prefix, so these functions are available in
# perl mappers.

sub ws($)  {print $_[0]; ()}
sub wp($@) {ws pack $_[0], @_[1..$#_]}
50 core/binary/binary.pl
# Binary import operator.
# An operator that reads data in terms of bytes rather than lines. This is done
# in a Perl context with functions that manage a queue of data in `$_`.

use constant binary_perlgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3';
  while (available) {
    %body
  }
};

defperlprefix 'core/binary/bytewriter.pm';

our @binary_perl_prefix_keys = qw| core/binary/bytestream.pm |;

sub binary_perl_prefix() {join "\n", perl_prefix,
                                     @ni::self{@binary_perl_prefix_keys}}

sub defbinaryperlprefix($) {push @binary_perl_prefix_keys, $_[0]}

sub binary_perl_mapper($) {binary_perlgen->(prefix => binary_perl_prefix,
                                            body   => perl_expand_begin $_[0])}

defoperator binary_perl => q{stdin_to_perl binary_perl_mapper $_[0]};

defoperator binary_fixed => q{
  use bytes;
  my ($pack_template) = @_;
  my @packed = unpack $pack_template, "\0" x 65536;
  my $length = length pack $pack_template, @packed;
  my $offset = 0;
  die "ni: binary_fixed template consumes no data" unless $length;
  my $buf = $length;
  $buf <<= 1 until $buf >= 65536;
  while (1) {
    read STDIN, $_, $buf - length, length or return until length >= $length;
    my @vs = unpack "($pack_template)*", $_;
    for (my $n = 0; $n + @packed < @vs; $n += @packed) {
      print join("\t", @vs[$n..$n+$#packed]), "\n";
    }
    $_ = length() % $length ? substr($_, $length * @vs / @packed) : '';
  }
};

defshort '/b',
  defdsp 'binaryalt', 'dispatch table for the /b binary operator',
    f => pmap(q{binary_fixed_op $_}, generic_code),
    p => pmap q{binary_perl_op $_}, plcode \&binary_perl_mapper;
1 core/matrix/lib
matrix.pl
156 core/matrix/matrix.pl
# Matrix conversions.
# Dense to sparse creates a (row, column, value) stream from your data. Sparse to
# dense inverts that. You can specify where the matrix data begins using a column
# identifier; this is useful when your matrices are prefixed with keys.

sub matrix_cell_combine($$) {
  return $_[0] = $_[1] unless defined $_[0];
  $_[0] += $_[1];
}

defoperator dense_to_sparse => q{
  my ($col) = @_;
  $col ||= 0;
  my @q;
  my $n = 0;
  while (defined($_ = @q ? shift @q : <STDIN>)) {
    chomp(my @fs = split /\t/);
    if ($col) {
      $n = 0;
      my $k  = join "\t", @fs[0..$col-1];
      my $kr = qr/\Q$k\E/;
      print join("\t", $k, $n, $_ - $col, $fs[$_]), "\n" for $col..$#fs;
      my $l;
      while (defined($l = <STDIN>) && $l =~ /^$kr\t/) {
        ++$n;
        chomp(@fs = split /\t/, $l);
        print join("\t", $k, $n, $_ - $col, $fs[$_]), "\n" for $col..$#fs;
      }
      push @q, $l if defined $l;
    } else {
      print join("\t", $n, $_, $fs[$_]), "\n" for 0..$#fs;
      ++$n;
    }
  }
};

defoperator sparse_to_dense => q{
  my ($col) = @_;
  $col ||= 0;
  my $n = 0;
  my @q;
  my $row = -1;
  while (defined($_ = @q ? shift @q : <STDIN>)) {
    ++$row;
    chomp;
    my @r = split /\t/, $_, $col + 3;
    my $k = join "\t", @r[0..$col];
    my $kr = qr/\Q$k\E/;
    my @fs = $col ? @r[0..$col-1] : ();
    if ($col < @r) {
      no warnings 'numeric';
      ++$row, print "\n" until $row >= $r[$col];
    }
    matrix_cell_combine $fs[$col + $r[$col+1]], $r[$col+2];
    matrix_cell_combine $fs[$col + $1], $2
      while defined($_ = <STDIN>) && /^$kr\t([^\t]+)\t(.*)/;
    push @q, $_ if defined;
    print join("\t", map defined() ? $_ : '', @fs), "\n";
  }
};

defoperator unflatten => q{
  my ($n_cols) = @_;
  my @row = ();
  while(<STDIN>) {
    chomp;
    push @row, split /\t/, $_;
    while(@row >= $n_cols) {
      my @emit_vals = splice(@row, 0, $n_cols);
      print(join("\t", @emit_vals). "\n"); 
      }
    }
  if (@row > 0) {
    while(@row > 0) {
      my @emit_vals = splice(@row, 0, $n_cols);
      print(join("\t", @emit_vals). "\n");
    }
  } 
};

defshort '/X', pmap q{sparse_to_dense_op $_}, popt colspec1;
defshort '/Y', pmap q{dense_to_sparse_op $_}, popt colspec1;
defshort '/Z', pmap q{unflatten_op 0 + $_}, integer;

# NumPy interop.
# Partitioned by the first row value and sent in as dense matrices.

use constant numpy_gen => gen pydent q{
  from numpy import *
  from sys   import stdin, stdout, stderr
  try:
    stdin = stdin.buffer
    stdout = stdout.buffer
  except:
    pass
  while True:
    try:
      dimensions = fromstring(stdin.read(8), dtype=">u4", count=2)
    except:
      exit()
    x = fromstring(stdin.read(8*dimensions[0]*dimensions[1]),
                   dtype="d",
                   count=dimensions[0]*dimensions[1]) \
        .reshape(dimensions)
  %body
    if type(x) != ndarray: x = array(x)
    if len(x.shape) != 2: x = reshape(x, (-1, 1))
    stdout.write(array(x.shape).astype(">u4").tostring())
    stdout.write(x.astype("d").tostring())
    stdout.flush()};

defoperator numpy_dense => q{
  my ($col, $f) = @_;
  $col ||= 0;
  my ($i, $o) = sioproc {
    exec 'python', '-c', numpy_gen->(body => indent $f, 2)
      or die "ni: failed to execute python: $!"};

  my @q;
  my ($rows, $cols);
  while (defined($_ = @q ? shift @q : <STDIN>)) {
    chomp;
    my @r = split /\t/;
    my $k = $col ? join("\t", @r[0..$col-1]) : '';
    $rows = 1;
    my @m = [@r[$col..$#r]];
    my $kr = qr/\Q$k\E/;
    ++$rows, push @m, [split /\t/, $col ? substr $_, length $1 : $_]
      while defined($_ = <STDIN>) and !$col || /^($kr\t)/;
    push @q, $_ if defined;

    $cols = max map scalar(@$_), @m;
    safewrite $i, pack "NNF*", $rows, $cols,
      map $_ || 0,
      map {(@$_, (0) x ($cols - @$_))} @m;

    saferead $o, $_, 8;
    ($rows, $cols) = unpack "NN", $_;

    $_ = '';
    saferead $o, $_, $rows*$cols*8 - length(), length
      until length == $rows*$cols*8;

    # TODO: optimize this. Right now it's horrifically slow and for no purpose
    # (everything's getting read into memory either way).
    for my $r (0..$rows-1) {
      print join("\t", $col ? ($k) : (), unpack "F$cols", substr $_, $r*$cols*8), "\n";
    }
  }

  close $i;
  close $o;
  $o->await;
};

defshort '/N', pmap q{numpy_dense_op @$_}, pseq popt colspec1, pycode;
1 core/gnuplot/lib
gnuplot.pl
66 core/gnuplot/gnuplot.pl
# Gnuplot interop.
# An operator that sends output to a gnuplot process.

BEGIN {defdsp gnuplot_code_prefixalt => 'prefixes for gnuplot code';
       defparseralias gnuplot_colspec => palt colspec1, pmap q{undef}, pstr ':'}
BEGIN {defparseralias gnuplot_code =>
         pmap q{join "", map ref($_) ? @$_ : $_, @$_},
              pseq prep('dsp/gnuplot_code_prefixalt'),
                   popt generic_code}

defoperator stream_to_gnuplot => q{
  my ($col, $command) = @_;
  exec 'gnuplot', '-e', $command unless defined $col;
  my ($k, $fh) = (undef, undef);
  while (<STDIN>) {
    chomp;
    my @fs = split /\t/, $_, $col + 2;
    my $rk = join "\t", @fs[0..$col];
    if (!defined $k or $k ne $rk) {
      if (defined $fh) {
        close $fh;
        $fh->await;
      }
      $k  = $rk;
      $fh = siproc {exec 'gnuplot', '-e', "KEY='$k';$command"};
    }
    print $fh join("\t", @fs[$col+1..$#fs]) . "\n";
  }
};

defshort '/G', pmap q{stream_to_gnuplot_op @$_},
               pseq gnuplot_colspec, gnuplot_code;

# Some convenient shorthands for gnuplot -- things like interactive plotting,
# setting up JPEG export, etc.

BEGIN {defparseralias gnuplot_terminal_size =>
         pmap q{defined $_ ? "size " . join ',', @$_ : ""},
         popt pn [0, 2], integer, prx('[x,]'), integer}

defgnuplot_code_prefixalt J  => pmap q{"set terminal jpeg $_;"}, gnuplot_terminal_size;
defgnuplot_code_prefixalt PC => pmap q{"set terminal pngcairo $_;"}, gnuplot_terminal_size;
defgnuplot_code_prefixalt P  => pmap q{"set terminal png $_;"}, gnuplot_terminal_size;

defgnuplot_code_prefixalt XP => pk "set terminal x11 persist;";
defgnuplot_code_prefixalt QP => pk "set terminal qt persist;";
defgnuplot_code_prefixalt WP => pk "set terminal wx persist;";

defgnuplot_code_prefixalt '%l' => pk 'plot "-" with lines ';
defgnuplot_code_prefixalt '%d' => pk 'plot "-" with dots ';
defgnuplot_code_prefixalt '%i' => pk 'plot "-" with impulses ';
defgnuplot_code_prefixalt '%v' => pk 'plot "-" with vectors ';

defgnuplot_code_prefixalt '%t' => pmap q{"title '$_'"}, generic_code;
defgnuplot_code_prefixalt '%u' => pmap q{"using $_"},   generic_code;

# FFMPEG movie assembly.
# You can use the companion operator `GF` to take a stream of jpeg images from a
# partitioned gnuplot process and assemble a movie. `GF` accepts shell arguments
# for ffmpeg to follow `-f image2pipe -i -`.
#
# GF^ inverts GF, outputting PNG images from a video specified on stdin.

defshort '/GF',  pmap q{sh_op "ffmpeg -f image2pipe -i - $_"}, shell_command;
defshort '/GF^', pmap q{sh_op "ffmpeg -i - $_ -f image2pipe -c:v png -"},
                      shell_command;
1 core/image/lib
image.pl
129 core/image/image.pl
# Image compositing and processing
# Operators that loop over concatenated PNG images within a stream. This is
# useful for compositing workflows in a streaming context, e.g. between a
# gnuplot loop and ffmpeg.

# Image traversal functions
# simage() will read a single image from stdin, copying it to stdout, then
# return. This allows you to perform a distinct action for each of a series of
# images concatenated into a stream.
#
# OK .... so this is a lot more complicated than it should be, for all kinds of
# fun reasons. Here's what's up.
#
# For PNG it's simple: there are multiple sections, each length-prefixed
# because its designers were sober, well-adjusted individuals. This includes
# the zero-length IEND marker. So we can do small reads to get the length+type,
# then do custom-sized reads to skip sections.
#
# Contrarily, JFIF/JPEG exemplifies the ambiguity between malice and
# incompetence. JFIF sections are tagged with lengths, but the image data
# itself is a binary format with the equivalent of backslash-escapes around the
# ff byte. The idea is that because each ff in the image is replaced with ff00,
# you can safely identify the image size by looking for the ffd9 EOI marker.
# But that's a bit of a wrench for this use case because it means we need to
# push bytes back into the data stream.
#
# So ... what do we do? We have `simage` maintain a "leftovers" buffer so we
# can still do big-ish block reads and keep track of unconsumed data.
#
# TODO: for now only PNG and BMP are supported.

sub simage_png {
  my ($into) = @_;
  return undef unless saferead_exactly \*STDIN, $_, 6;
  safewrite_exactly $into, $_;

  my ($l, $t) = (0, '');
  while ($t ne 'IEND') {
    ($l, $t) = unpack 'Na4', $_ if saferead_exactly \*STDIN, $_, 8;
    saferead_exactly \*STDIN, $_, $l + 4, 8;
    safewrite_exactly $into, $_;
  }
  close $into;
  $into->await;
}

sub simage_bmp {
  my ($into) = @_;
  return undef unless saferead_exactly \*STDIN, $_, 4;
  safewrite_exactly $into, $_;

  # Super easy: bytes 2-6 store the total size as a little-endian int.
  saferead_exactly \*STDIN, $_, unpack('V', $_) - 2;
  safewrite_exactly $into, $_;
  close $into;
  $into->await;
}

sub simage_jfif {
  die "ni simage jfif: TODO: this is complicated and unimplemented at the moment";
}

sub simage_into(&) {
  # Reads exactly one image from stdin, outputting to the stdin of the
  # specified forked function if an image can be read. Returns the fork's exit
  # code on success, sets $! and returns undef on failure. Dies if you're
  # working with an unsupported type of image.
  my ($fn) = @_;
  local $_;
  my $n;
  return undef unless $n = saferead_exactly \*STDIN, $_, 2;
  my $into = siproc {&$fn};
  safewrite_exactly $into, $_;
  return simage_png  $into if /^\x89P$/;
  return simage_jfif $into if /^\xff\xd8$/;
  return simage_bmp  $into if /^BM$/;
  die "ni simage: unsupported image type (unrecognized magic in $_)";
}

# Image splitting operators
# The simplest of these executes a pipeline separately for each of a series of
# images.

defoperator each_image => q{
  my ($lambda) = @_;
  $ENV{KEY} = 0;
  1 while ++$ENV{KEY} && defined simage_into {exec_ni @$lambda};
};

defshort '/I' => pmap q{each_image_op $_}, _qfn;

# Streaming compositing pipelines
# ImageMagick's "convert" command lets you composite images and select regions.
# This isn't an especially cheap strategy and it involves a lot of disk IO, but
# it ends up being a very flexible way to implement a multi-frame compositing
# setup.

BEGIN {defparseralias image_command => palt pmap(q{''}, pstr ':'), shell_command}

defconfenv image_command => 'NI_IMAGE_COMMAND', 'convert';

sub image_sync_sh($) {
  my $fh = siproc {close STDIN; sh $_[0]} $_[0];
  close $fh;
  $fh->await;
}

defoperator composite_images => q{
  my ($init, $reducer, $emitter) = @_;
  my $ic = conf 'image_command';
  my $reduced_image = substr(resource_tmp 'file://', 7) . '.png';
  my $temp_image    = substr(resource_tmp 'file://', 7) . '.png';

  my $reduced_q = shell_quote $reduced_image;
  my $temp_q    = shell_quote $temp_image;

  if (defined simage_into {sh "$ic - $init $reduced_q"}) {
    image_sync_sh "$ic $reduced_q $emitter png:-";
    image_sync_sh "$ic $reduced_q $emitter png:-"
      while defined simage_into {sh "$ic $reduced_q $reducer $temp_q; mv $temp_q $reduced_q"};
  }

  unlink $reduced_image;
};

defshort '/IC' => pmap q{composite_images_op @$_},
                  pseq pc image_command, pc image_command, pc image_command;

defshort '/IJ' => pmap q{each_image_op [sh_op "convert - jpg:-"]}, pnone;
2 core/http/lib
ws.pm
http.pl
41 core/http/ws.pm
# WebSocket encoding functions.
# We just encode text messages; no binary or other protocols are defined yet.

BEGIN {
  eval 'use Digest::SHA qw/sha1_base64/';
  load 'core/deps/sha1.pm',
    Digest::SHA::PurePerl->import(qw/sha1_base64/) if $@;

  eval 'use Encode qw/encode/';
  if ($@) {
    warn 'ni: websockets will fail for utf-8 data on this machine '
       . '(no Encode module)';
    *encode = sub {$_[1]};
  }
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

sub ws_encode($) {
  my $e = encode 'utf8', $_[0];
  "\x81" . ws_length_encode(length $e) . $e;
}
65 core/http/http.pl
# HTTP server.
# A very simple HTTP server that can be used to serve a stream's contents. This is used
# by other libraries to serve things like JSPlot.

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

# Websocket operators.
# This is used to stream a data source to the browser. See `core/jsplot` for details.

defoperator http_websocket_encode => q{
  load 'core/http/ws.pm';
  safewrite \*STDOUT, ws_encode($_) while <STDIN>;
};

defoperator http_websocket_encode_batch => q{
  load 'core/http/ws.pm';
  safewrite \*STDOUT, ws_encode($_) while saferead \*STDIN, $_, $_[0] || 8192;
};

defshort '/--http/wse',       pmap q{http_websocket_encode_op}, pnone;
defshort '/--http/wse-batch', pmap q{http_websocket_encode_batch_op $_}, popt integer;
3 core/caterwaul/lib
caterwaul.min.js
caterwaul.std.min.js
caterwaul.ui.min.js
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
14 core/caterwaul/caterwaul.ui.min.js
caterwaul.module("ui.jquery",(function(qs,qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9,qsa,qsb,qsc,qsd,qse,qsf,qsg,qsh,qsi,qsj,qsk,qsl,qsm,qsn,qso,qsp,qsq,qsr,qss,qst,qsu,qsv,qsw,qsx,qsy,qsz,qs10,qs11,qs12,qs13,qs14,qs15,qs16,qs17,qs18,qs19,qs1a,qs1b,qs1c,qs1d,qs1e,qs1f,qs1g,qs1h,qs1i,qs1j,qs1k,qs1l,qs1m,qs1n){var result=(function($){$.jquery=function(caterwaul_function){return(function(it){return it.modifiers.jquery=$.grammar("J",{initial:qs},(function(rule,anon){return(function(){var jq=qs1,hyphenate=function(s){return s.replace(/_/g,"-")
},p=(function(){var p_pattern=anon(qs2);return(function(node){return p_pattern.replace({_thing:node})})}).call(this),jquery_macros=(function(){var dom_node_template=anon((""+(jq)+"(TS[_element])")),jquery_template=anon((""+(jq)+'("<span>" + (_element) + "</span>")')),become_dom_node=function(match){return dom_node_template.replace(match)
},wrap_in_jquery=function(match){return jquery_template.replace(match)};return[rule(qs3,(function(match){return match._element.is_constant()||match._element.length?wrap_in_jquery(match):become_dom_node(match)
})),rule(qs4,qs5),rule(qs6,qs7),rule(qs8,qs9),rule(qsa,qsb),rule(qsc,qsd),rule(qse,qsf),rule(qsg,qsh),rule(qsi,qsj),rule(qsk,qsl),rule(qsm,qsn),rule(qso,qsp),rule(qsq,qsr),rule(qss,qst),rule(qsu,qsv),rule(qsw,qsx),rule(qsy,qsz),rule(qs10,qs11),rule(qs12,qs13),rule(qs14,qs15),rule(qs16,qs17)]
}).call(this),string_macros=(function(){var string=function(s){return new $.syntax('"'+s.replace(/\\/g,"\\\\").replace(/"/g,'\\"')+'"')};return[rule(qs18,(function(match){return string(("<"+(hyphenate(match._identifier.data))+">"))
})),rule(qs19,(function(match){return string(hyphenate(match._identifier.data))})),rule(qs1a,(function(match){return string(expand(p(match._identifier)).data)}))]
}).call(this),search_macros=(function(){var interpolated=function(node){return("("+(node.toString())+').replace(/(\\)/g, "$1$1").replace(/(")/g, "\\$1")')},binary=function(op){return function(match){return new $.syntax((""+(expand(p(match._element1)).data)+""+(op)+""+(expand(p(match._element2)).data)+""))
}};return[rule(qs1b,(function(match){return new $.syntax(hyphenate((function(it){return it==="_"?"*":it}).call(this,(match._element.data))))})),rule(qs1c,(function(match){return new $.syntax((""+(this(p(match._element)).data)+"."+(hyphenate(match._class.data))+""))
})),rule(qs1d,(function(match){return new $.syntax((""+(this(p(match._element)).data)+"["+(this(p(match._attributes)))+"]"))})),rule(qs1e,(function(match){return new $.syntax((""+(this(p(match._attribute)).data)+'="')+interpolated(match._value)+'}"')
})),rule(qs1f,"P[_element]"),rule(qs1g,binary(", ")),rule(qs1h,binary(", ")),rule(qs1i,binary(" ")),rule(qs1j,binary(" ")),rule(qs1k,binary(" > ")),rule(qs1l,binary(" > ")),rule(qs1m,(function(match){return new $.syntax((""+(expand(p(match._element)).data)+":"+(hyphenate(match._selector.data))+""))
})),rule(qs1n,(function(match){return new $.syntax((""+(expand(p(match._element)).data)+":"+(hyphenate(match._selector.data))+'("#')+"{"+interpolated(match._value)+'}")')
}))]}).call(this);return(jquery_macros).concat(string_macros)}).call(this)})),it}).call(this,(caterwaul_function))}});result.caterwaul_expression_ref_table={qs:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_expression"))'),qs1:('new caterwaul.syntax( "jQuery")'),qs2:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "_thing"))'),qs3:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element"))'),qs4:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_class")))'),qs5:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "addClass")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_class")))'),qs6:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_attr") ,new caterwaul.syntax( "_val"))) .prefix( " "))'),qs7:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "attr")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_attr")) ,new caterwaul.syntax( "_val") .prefix( " ")))'),qs8:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_name") ,new caterwaul.syntax( "_val")))) .prefix( " "))'),qs9:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "data")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_name")) ,new caterwaul.syntax( "_val") .prefix( " ")))'),qsa:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_method") ,new caterwaul.syntax( "_args"))) .prefix( " "))'),qsb:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "_method")) ,new caterwaul.syntax( "_args"))'),qsc:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "u!" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_event") ,new caterwaul.syntax( "_args")))) .prefix( " "))'),qsd:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "bind")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_event")) ,new caterwaul.syntax( "_args") .prefix( " ")))'),qse:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "%" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_function")) .prefix( " "))'),qsf:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "_function") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")))'),qsg:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_children")))'),qsh:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "append")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_children")))'),qsi:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_children")))'),qsj:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "append")) ,new caterwaul.syntax( "_children"))'),qsk:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "<" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_tree") .prefix( " ")) .prefix( " "))'),qsl:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "append")) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_tree")) ,new caterwaul.syntax( "toString")) ,new caterwaul.syntax( "")))'),qsm:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( ">" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_child") .prefix( " ")) .prefix( " "))'),qsn:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "append")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_child")))'),qso:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( ">=" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_child") .prefix( " ")) .prefix( " "))'),qsp:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "append")) ,new caterwaul.syntax( "_child"))'),qsq:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( " ")))'),qsr:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element1")) ,new caterwaul.syntax( "add")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element2")))'),qss:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( " ")) .prefix( " "))'),qst:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element1")) ,new caterwaul.syntax( "add")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element2")))'),qsu:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( " ")) .prefix( " "))'),qsv:('new caterwaul.syntax( "-" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element1")) ,new caterwaul.syntax( "_element2") .prefix( " ")) .prefix( " ")'),qsw:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( ">>" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_pattern") .prefix( " ")) .prefix( " "))'),qsx:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "filter")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "PS") ,new caterwaul.syntax( "_pattern")))'),qsy:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( ">>>" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_pattern") .prefix( " ")) .prefix( " "))'),qsz:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "find")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "PS") ,new caterwaul.syntax( "_pattern")))'),qs10:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "<<" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_pattern") .prefix( " ")) .prefix( " "))'),qs11:('new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")) ,new caterwaul.syntax( "parents")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "PS") ,new caterwaul.syntax( "_pattern")))'),qs12:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_element")))'),qs13:('new caterwaul.syntax( "(" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")))'),qs14:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "_element")))'),qs15:('new caterwaul.syntax( "[" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "_element")))'),qs16:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "J") ,new caterwaul.syntax( "u+" ,new caterwaul.syntax( "_expression")))'),qs17:('new caterwaul.syntax( "_expression")'),qs18:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "TS") ,new caterwaul.syntax( "_identifier"))'),qs19:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "S") ,new caterwaul.syntax( "_identifier"))'),qs1a:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "PS") ,new caterwaul.syntax( "_identifier"))'),qs1b:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "_element"))'),qs1c:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "." ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_class")))'),qs1d:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_attributes")))'),qs1e:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "=" ,new caterwaul.syntax( "_attribute") ,new caterwaul.syntax( "_value") .prefix( " ")) .prefix( " "))'),qs1f:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_element")))'),qs1g:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( "   ")) .prefix( " "))'),qs1h:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( "    ")))'),qs1i:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( ">>" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( "  ")) .prefix( " "))'),qs1j:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( ">>>" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( " ")) .prefix( " "))'),qs1k:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( ">" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2") .prefix( "   ")) .prefix( " "))'),qs1l:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_element1") ,new caterwaul.syntax( "_element2")))'),qs1m:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "_selector")) .prefix( " "))'),qs1n:('new caterwaul.syntax( "[]" ,new caterwaul.syntax( "P") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "_element") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "_selector") ,new caterwaul.syntax( "_value"))) .prefix( " "))')};
return(result)}).call(this,new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_expression")),new caterwaul.syntax("jQuery"),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax("_thing")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax(".",new caterwaul.syntax("_element"),new caterwaul.syntax("_class"))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("addClass")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_class"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("*",new caterwaul.syntax("_element"),new caterwaul.syntax("()",new caterwaul.syntax("_attr"),new caterwaul.syntax("_val")))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("attr")),new caterwaul.syntax(",",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_attr")),(new caterwaul.syntax("_val")).prefix(" "))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("*",new caterwaul.syntax("_element"),new caterwaul.syntax("u!",new caterwaul.syntax("()",new caterwaul.syntax("_name"),new caterwaul.syntax("_val"))))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("data")),new caterwaul.syntax(",",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_name")),(new caterwaul.syntax("_val")).prefix(" "))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("/",new caterwaul.syntax("_element"),new caterwaul.syntax("()",new caterwaul.syntax("_method"),new caterwaul.syntax("_args")))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("_method")),new caterwaul.syntax("_args")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("/",new caterwaul.syntax("_element"),new caterwaul.syntax("u!",new caterwaul.syntax("()",new caterwaul.syntax("_event"),new caterwaul.syntax("_args"))))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("bind")),new caterwaul.syntax(",",new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_event")),(new caterwaul.syntax("_args")).prefix(" "))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("%",new caterwaul.syntax("_element"),new caterwaul.syntax("_function"))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax("_function"),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("()",new caterwaul.syntax("_element"),new caterwaul.syntax("_children"))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("append")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_children"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("[]",new caterwaul.syntax("_element"),new caterwaul.syntax("_children"))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("append")),new caterwaul.syntax("_children")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("<",new caterwaul.syntax("_element"),(new caterwaul.syntax("_tree")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("append")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("(",new caterwaul.syntax("_tree")),new caterwaul.syntax("toString")),new caterwaul.syntax(""))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax(">",new caterwaul.syntax("_element"),(new caterwaul.syntax("_child")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("append")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_child"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax(">=",new caterwaul.syntax("_element"),(new caterwaul.syntax("_child")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("append")),new caterwaul.syntax("_child")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax(",",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix(" "))),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element1")),new caterwaul.syntax("add")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element2"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("+",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element1")),new caterwaul.syntax("add")),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element2"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("-",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix(" "))).prefix(" ")),(new caterwaul.syntax("-",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element1")),(new caterwaul.syntax("_element2")).prefix(" "))).prefix(" "),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax(">>",new caterwaul.syntax("_element"),(new caterwaul.syntax("_pattern")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("filter")),new caterwaul.syntax("[]",new caterwaul.syntax("PS"),new caterwaul.syntax("_pattern"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax(">>>",new caterwaul.syntax("_element"),(new caterwaul.syntax("_pattern")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("find")),new caterwaul.syntax("[]",new caterwaul.syntax("PS"),new caterwaul.syntax("_pattern"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),(new caterwaul.syntax("<<",new caterwaul.syntax("_element"),(new caterwaul.syntax("_pattern")).prefix(" "))).prefix(" ")),new caterwaul.syntax("()",new caterwaul.syntax(".",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element")),new caterwaul.syntax("parents")),new caterwaul.syntax("[]",new caterwaul.syntax("PS"),new caterwaul.syntax("_pattern"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("(",new caterwaul.syntax("_element"))),new caterwaul.syntax("(",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("[",new caterwaul.syntax("_element"))),new caterwaul.syntax("[",new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("_element"))),new caterwaul.syntax("[]",new caterwaul.syntax("J"),new caterwaul.syntax("u+",new caterwaul.syntax("_expression"))),new caterwaul.syntax("_expression"),new caterwaul.syntax("[]",new caterwaul.syntax("TS"),new caterwaul.syntax("_identifier")),new caterwaul.syntax("[]",new caterwaul.syntax("S"),new caterwaul.syntax("_identifier")),new caterwaul.syntax("[]",new caterwaul.syntax("PS"),new caterwaul.syntax("_identifier")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax("_element")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax(".",new caterwaul.syntax("_element"),new caterwaul.syntax("_class"))),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax("[]",new caterwaul.syntax("_element"),new caterwaul.syntax("_attributes"))),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax("=",new caterwaul.syntax("_attribute"),(new caterwaul.syntax("_value")).prefix(" "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax("(",new caterwaul.syntax("_element"))),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax("+",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix("   "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax(",",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix("    "))),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax(">>",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix("  "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax(">>>",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix(" "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax(">",new caterwaul.syntax("_element1"),(new caterwaul.syntax("_element2")).prefix("   "))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),new caterwaul.syntax("()",new caterwaul.syntax("_element1"),new caterwaul.syntax("_element2"))),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax("/",new caterwaul.syntax("_element"),new caterwaul.syntax("_selector"))).prefix(" ")),new caterwaul.syntax("[]",new caterwaul.syntax("P"),(new caterwaul.syntax("/",new caterwaul.syntax("_element"),new caterwaul.syntax("()",new caterwaul.syntax("_selector"),new caterwaul.syntax("_value")))).prefix(" "))));
caterwaul.module("ui",function($){$.all.push("jquery")});
19 core/jsplot/lib
jquery.min.js
jquery.mousewheel.min.js
modus.js
vector.js

murmurhash3.js

axis.waul
label.waul
dataframe.waul
matrix.waul
socket.waul
render.waul
camera.waul
interface.waul

css
html
jsplot.pl
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
1 core/jsplot/modus.js
caterwaul.module( 'modus' , function ($) { $ = jQuery; var original_jquery_val = $.fn.val; (function () {var use_named_combinator =function (receiver, args) { ; return $.modus[args[0]] .apply(receiver, Array.prototype.slice.call(args, 1))} ; return $.fn.val =function () {var args = arguments; return(function (it) {return it ? args.length ? it.setter.apply(this, args): it.getter.call(this): original_jquery_val.apply(this, args)}) .call(this, ( this.data( 'modus')))} , $.fn.modus =function (getter, setter) { ; return getter.constructor === String ? use_named_combinator(this, arguments): this.data( 'modus' , {getter: getter, setter: setter})}}) .call(this) , $.modus = { util: {} , val:function () { ; return original_jquery_val.apply(this, arguments)} , delegate:function (getter, setter) { ; return{first:function () { ; return this} , val:function () { ; return arguments.length ? ( setter.apply(this, arguments) , this): getter.apply(this, arguments)}}} , proxy:function (element) { ; return this.modus(function (_) {return find(this, element) .val()} ,function (_) {return(find(this, element) .first() .val(_) , this)})} , list:function (new_element) { ; return this.modus(function (_) {return(function (xs) {var x, x0, xi, xl, xr;for (var xr = new xs.constructor() , xi = 0, xl = xs.length; xi < xl; ++xi) x = xs[xi] , xr.push( ($(x) .val())) ; return xr}) .call(this,Array.prototype.slice.call( (this.children())))} ,function (_) {return(function (it) {return(function (xs) {var x, x0, xi, xl, xr;for (var xi = 0, xl = xs.length; xi < xl; ++xi) x = xs[xi] , (it.append(new_element(x, xi) .val(x))) ; return xs}) .call(this, _) , it}) .call(this, (this.empty()))})} , composite:function (paths) { ; return this.modus(function (_) {return(function (xs) {var x, x0, xi, xl, xr;var xr = new xs.constructor() ; for (var k in xs) if (Object.prototype.hasOwnProperty.call(xs, k)) x = xs[k] , xr[k] = (find(this, x) .first() .val()) ; return xr}) .call(this,paths)} ,function (_) {return( (function (xs) {var x, x0, xi, xl, xr;for (var x in xs) if (Object.prototype.hasOwnProperty.call(xs, x))find(this, paths[x]) .first() .val(_[x]) ; return xs}) .call(this,paths) ,this)})} , where:find =function (container, path) { ; return path.constructor === String ? ( container.filter(path)) .add( container.find(path)): path.constructor === Function ? path(container): path.constructor === jQuery ? path: (function () {throw new Error( ( 'invalid modus path: ' + (path) + ''))}) .call(this)}}}) ;
1 core/jsplot/vector.js
 caterwaul.module( 'vector' , (function (qs,qs1,qs2,qs3,qs4,qs5,qs6,qs7,qs8,qs9,qsa,qsb,qsc,qsd,qse,qsf,qsg,qsh,qsi,qsj,qsk,qsl,qsm,qsn,qso,qsp,qsq,qsr,qss,qst,qsu,qsv,qsw,qsx,qsy,qsz,qs10,qs11,qs12,qs13,qs14,qs15,qs16,qs17,qs18,qs19,qs1a,qs1b,qs1c,qs1d,qs1e,qs1f,qs1g,qs1h,qs1i) {var result= ( function ($) { (function () {var generator =function (base, composite) { ; return function (n, prefix) { ; return(function () {var compiled_base = base(n) ; return rename($.merge( {} ,compiled_base, composite(compiled_base)) , prefix)}) .call(this)}} , rename =function (o, prefix) { ; return(function (xs) {var x, x0, xi, xl, xr;var xr = new xs.constructor() ; for (var x in xs) if (Object.prototype.hasOwnProperty.call(xs, x)) xr[ ( '' + (prefix || "") + '' + (x) + '')] = xs[x] ; return xr}) .call(this, o)} , compile_function =function (xs, e) { ; return(function () {var tree = (qs) .replace( {_formals: xs, _e: e}) ; return(function (it) {return it.tree =tree, it}) .call(this, ($.compile( tree)))}) .call(this)} , base_v =function (n) { ; return(function () {var r =function (n, formals, wrap, fold, each) { ; return(function () {var body = ( wrap) .replace( {x: (function (xs) {var x, x0, xi, xl, xr;for (var x0 = xs[0] , xi = 1, xl = xs.length; xi < xl; ++xi) x = xs[xi] , x0 = ( (fold) .replace( {x: x0, y: x})) ; return x0}) .call(this, (function (xs) {var x, x0, xi, xl, xr;for (var xr = new xs.constructor() , xi = 0, xl = xs.length; xi < xl; ++xi) x = xs[xi] , xr.push( ( (each) .replace( {i: ( '' + (x) + '')}))) ; return xr}) .call(this, (function (i, u, s) {if ( (u - i) * s <= 0) return [] ; for (var r = [] , d = u - i; d >= 0 ? i < u: i > u; i += s) r.push(i) ; return r}) ( (0) , (n) , (1))))}) ; return compile_function( formals, body)}) .call(this)} ; return{plus: r(n,qs1,qs2,qs3,qs4) , times: r(n,qs5,qs6,qs7,qs8) , minus: r(n,qs9,qsa,qsb,qsc) , scale: r(n,qsd,qse,qsf,qsg) , dot: r(n,qsh,qsi,qsj,qsk) , norm: r(n,qsl,qsm,qsn,qso) , min: r(n,qsp,qsq,qsr,qss) , macv: r(n,qst,qsu,qsv,qsw) , max: r(n,qsx,qsy,qsz,qs10) , macs: r(n,qs11,qs12,qs13,qs14) , mixv: r(n,qs15,qs16,qs17,qs18) , mixs: r(n,qs19,qs1a,qs1b,qs1c)}}) .call(this)} , composite_v =function (base) { ; return(function () {var ref_compile =function (functions, formals, body) { ; return(function () {var new_body = ( body) .replace( (function (xs) {var x, x0, xi, xl, xr;var xr = new xs.constructor() ; for (var k in xs) if (Object.prototype.hasOwnProperty.call(xs, k)) x = xs[k] , xr[k] = (new $.expression_ref(x.tree)) ; return xr}) .call(this, functions)) ; return compile_function( formals, new_body)}) .call(this)} ; return{unit: ref_compile(base,qs1d,qs1e) , proj: ref_compile(base,qs1f,qs1g) , orth: ref_compile(base,qs1h,qs1i)}}) .call(this)} ; return $.vector = generator(base_v, composite_v)}) .call(this)}) ;result.caterwaul_expression_ref_table = {qs: ( " new caterwaul.syntax( \"(\" ,new caterwaul.syntax( \"function\" ,new caterwaul.syntax( \"(\" ,new caterwaul.syntax( \"_formals\")) .prefix( \" \") ,new caterwaul.syntax( \"{\" ,new caterwaul.syntax( \"return\" ,new caterwaul.syntax( \"_e\") .prefix( \" \"))) .prefix( \" \")))") ,qs1: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qs2: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qs3: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs4: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")") ,qs5: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qs6: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qs7: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs8: ( " new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")") ,qs9: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qsa: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qsb: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qsc: ( " new caterwaul.syntax( \"-\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")") ,qsd: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qse: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qsf: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qsg: ( " new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"b\") .prefix( \" \")) .prefix( \" \")") ,qsh: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qsi: ( " new caterwaul.syntax( \"x\")") ,qsj: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \")) .prefix( \" \")") ,qsk: ( " new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")") ,qsl: ( " new caterwaul.syntax( \"a\")") ,qsm: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \".\" ,new caterwaul.syntax( \"Math\") ,new caterwaul.syntax( \"sqrt\")) ,new caterwaul.syntax( \"x\"))") ,qsn: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \")) .prefix( \" \")") ,qso: ( " new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")") ,qsp: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qsq: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qsr: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qss: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \".\" ,new caterwaul.syntax( \"Math\") ,new caterwaul.syntax( \"min\")) ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))))") ,qst: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \")) ,new caterwaul.syntax( \"c\") .prefix( \" \"))") ,qsu: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qsv: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qsw: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"c\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")) .prefix( \" \")") ,qsx: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qsy: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qsz: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs10: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \".\" ,new caterwaul.syntax( \"Math\") ,new caterwaul.syntax( \"max\")) ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))))") ,qs11: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \")) ,new caterwaul.syntax( \"c\") .prefix( \" \"))") ,qs12: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qs13: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs14: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"c\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")) .prefix( \" \")") ,qs15: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \")) ,new caterwaul.syntax( \"c\") .prefix( \" \"))") ,qs16: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qs17: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs18: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"(\" ,new caterwaul.syntax( \"-\" ,new caterwaul.syntax( \"1\") ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")) .prefix( \" \")) .prefix( \" \") ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"c\") .prefix( \" \") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"b\") .prefix( \" \") ,new caterwaul.syntax( \"i\"))) .prefix( \" \")) .prefix( \" \")") ,qs19: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \")) ,new caterwaul.syntax( \"c\") .prefix( \" \"))") ,qs1a: ( " new caterwaul.syntax( \"[\" ,new caterwaul.syntax( \"x\"))") ,qs1b: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"x\") ,new caterwaul.syntax( \"y\") .prefix( \" \"))") ,qs1c: ( " new caterwaul.syntax( \"+\" ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"(\" ,new caterwaul.syntax( \"-\" ,new caterwaul.syntax( \"1\") ,new caterwaul.syntax( \"b\") .prefix( \" \")) .prefix( \" \")) .prefix( \" \")) .prefix( \" \") ,new caterwaul.syntax( \"*\" ,new caterwaul.syntax( \"[]\" ,new caterwaul.syntax( \"c\") .prefix( \" \") ,new caterwaul.syntax( \"i\")) ,new caterwaul.syntax( \"b\") .prefix( \" \")) .prefix( \" \")) .prefix( \" \")") ,qs1d: ( " new caterwaul.syntax( \"a\")") ,qs1e: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"scale\") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"/\" ,new caterwaul.syntax( \"1.0\") .prefix( \" \") ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"norm\") .prefix( \" \") ,new caterwaul.syntax( \"a\"))) .prefix( \" \")))") ,qs1f: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qs1g: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"scale\") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"b\") ,new caterwaul.syntax( \"/\" ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"dot\") .prefix( \" \") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))) ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"dot\") .prefix( \" \") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"b\") ,new caterwaul.syntax( \"b\") .prefix( \" \")))) .prefix( \" \")))") ,qs1h: ( " new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))") ,qs1i: ( " new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"minus\") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"scale\") .prefix( \" \") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"b\") ,new caterwaul.syntax( \"/\" ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"dot\") .prefix( \" \") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"a\") ,new caterwaul.syntax( \"b\") .prefix( \" \"))) ,new caterwaul.syntax( \"()\" ,new caterwaul.syntax( \"dot\") .prefix( \" \") ,new caterwaul.syntax( \",\" ,new caterwaul.syntax( \"b\") ,new caterwaul.syntax( \"b\") .prefix( \" \")))) .prefix( \" \")))))")} ;return(result)}) .call(this,new caterwaul.syntax( "(" ,new caterwaul.syntax( "function" ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "_formals")) .prefix( " ") ,new caterwaul.syntax( "{" ,new caterwaul.syntax( "return" ,new caterwaul.syntax( "_e") .prefix( " "))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "b") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Math") ,new caterwaul.syntax( "sqrt")) ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Math") ,new caterwaul.syntax( "min")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i")))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "c") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "c") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "." ,new caterwaul.syntax( "Math") ,new caterwaul.syntax( "max")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i")))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "c") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "c") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "c") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "1") ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "c") .prefix( " ") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "b") .prefix( " ") ,new caterwaul.syntax( "i"))) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "c") .prefix( " ")) ,new caterwaul.syntax( "[" ,new caterwaul.syntax( "x")) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "x") ,new caterwaul.syntax( "y") .prefix( " ")) ,new caterwaul.syntax( "+" ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "(" ,new caterwaul.syntax( "-" ,new caterwaul.syntax( "1") ,new caterwaul.syntax( "b") .prefix( " ")) .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "*" ,new caterwaul.syntax( "[]" ,new caterwaul.syntax( "c") .prefix( " ") ,new caterwaul.syntax( "i")) ,new caterwaul.syntax( "b") .prefix( " ")) .prefix( " ")) .prefix( " ") ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "scale") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "1.0") .prefix( " ") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "norm") .prefix( " ") ,new caterwaul.syntax( "a"))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "scale") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "b") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "dot") .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " "))) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "dot") .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "b") ,new caterwaul.syntax( "b") .prefix( " ")))) .prefix( " "))) ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " ")) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "minus") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "scale") .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "b") ,new caterwaul.syntax( "/" ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "dot") .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "a") ,new caterwaul.syntax( "b") .prefix( " "))) ,new caterwaul.syntax( "()" ,new caterwaul.syntax( "dot") .prefix( " ") ,new caterwaul.syntax( "," ,new caterwaul.syntax( "b") ,new caterwaul.syntax( "b") .prefix( " ")))) .prefix( " "))))))) ;
65 core/jsplot/murmurhash3.js
/**
 * JS Implementation of MurmurHash3 (r136) (as of May 20, 2011)
 * 
 * @author <a href="mailto:gary.court@gmail.com">Gary Court</a>
 * @see http://github.com/garycourt/murmurhash-js
 * @author <a href="mailto:aappleby@gmail.com">Austin Appleby</a>
 * @see http://sites.google.com/site/murmurhash/
 * 
 * @param {string} key ASCII only
 * @param {number} seed Positive integer only
 * @return {number} 32-bit positive integer hash 
 */

function murmurhash3_32(key, seed) {
	var remainder, bytes, h1, h1b, c1, c1b, c2, c2b, k1, i;
	
        if (key == null) key = '';
	remainder = key.length & 3; // key.length % 4
	bytes = key.length - remainder;
	h1 = seed;
	c1 = 0xcc9e2d51;
	c2 = 0x1b873593;
	i = 0;
	
	while (i < bytes) {
	  	k1 = 
	  	  ((key.charCodeAt(i) & 0xff)) |
	  	  ((key.charCodeAt(++i) & 0xff) << 8) |
	  	  ((key.charCodeAt(++i) & 0xff) << 16) |
	  	  ((key.charCodeAt(++i) & 0xff) << 24);
		++i;
		
		k1 = ((((k1 & 0xffff) * c1) + ((((k1 >>> 16) * c1) & 0xffff) << 16))) & 0xffffffff;
		k1 = (k1 << 15) | (k1 >>> 17);
		k1 = ((((k1 & 0xffff) * c2) + ((((k1 >>> 16) * c2) & 0xffff) << 16))) & 0xffffffff;

		h1 ^= k1;
        h1 = (h1 << 13) | (h1 >>> 19);
		h1b = ((((h1 & 0xffff) * 5) + ((((h1 >>> 16) * 5) & 0xffff) << 16))) & 0xffffffff;
		h1 = (((h1b & 0xffff) + 0x6b64) + ((((h1b >>> 16) + 0xe654) & 0xffff) << 16));
	}
	
	k1 = 0;
	
	switch (remainder) {
		case 3: k1 ^= (key.charCodeAt(i + 2) & 0xff) << 16;
		case 2: k1 ^= (key.charCodeAt(i + 1) & 0xff) << 8;
		case 1: k1 ^= (key.charCodeAt(i) & 0xff);
		
		k1 = (((k1 & 0xffff) * c1) + ((((k1 >>> 16) * c1) & 0xffff) << 16)) & 0xffffffff;
		k1 = (k1 << 15) | (k1 >>> 17);
		k1 = (((k1 & 0xffff) * c2) + ((((k1 >>> 16) * c2) & 0xffff) << 16)) & 0xffffffff;
		h1 ^= k1;
	}
	
	h1 ^= key.length;

	h1 ^= h1 >>> 16;
	h1 = (((h1 & 0xffff) * 0x85ebca6b) + ((((h1 >>> 16) * 0x85ebca6b) & 0xffff) << 16)) & 0xffffffff;
	h1 ^= h1 >>> 13;
	h1 = ((((h1 & 0xffff) * 0xc2b2ae35) + ((((h1 >>> 16) * 0xc2b2ae35) & 0xffff) << 16))) & 0xffffffff;
	h1 ^= h1 >>> 16;

	return h1 >>> 0;
}
34 core/jsplot/axis.waul
// A column vector of numeric data.
// Stores a single axis of the data we want to render. It has a fixed capacity and represents a uniform sample if it overflows (i.e. it kicks data points out
// evenly). Because multiple axes need to be coordinated, all random numbers used for sampling are parameters rather than generated here.

// If you want focused nonuniform sampling, you can do it like this:

// | var r = Math.random();
//   r *= axis.focus(data_point, focus_center, focus_scale);
//   // same for other axes
//   axis.push(data_point, r);

// Focusing biases the probability of accepting points so that data closer to the focal plane(s) is preferred.

caterwaul(':all')(function () {
  axis(capacity) = this /-caterwaul.merge/ {data: new Float64Array(capacity), max: null, min: null, n: 0, c: capacity} -re- void 0,
  axis.prototype /-caterwaul.merge/ axis_methods,
  axis           /-caterwaul.merge/ static_methods,

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
                                 pnorm(i)           = (this.data[i] - this.offset()) / this.range() + 0.5,

                                 push(x, r)         = this.n++ < this.c ? this.set(this.n, +x) : this /+x /~uniform_push/ r,
                                 uniform_push(x, r) = this.set(r * this.n | 0, x) -when [r * this.n < this.c]]]})();
32 core/jsplot/label.waul
// A column vector of labeled data.
// Stores a column of hashed labels for a field, along with a sample of unique string values for that field. The sample's purpose is to provide a catalog of
// human-readable labels, ideally covering a large fraction of the points in question. This means we want its bias to echo the points' bias.

// The way we achieve this is simple: the sample array is indexed by the low N bits of each entry's murmurhash. Collisions are eagerly replaced, and we monitor
// the total string length, changing the number of bits and collapsing to remain within the memory limits.

caterwaul(':all')(function () {
  label(capacity) = this /-caterwaul.merge/ {hashes: new Uint32Array(capacity), sample: new label_sample(capacity), n: 0, c: capacity} -re- void 0,
  label.prototype /-caterwaul.merge/ label_methods,

  label_sample(capacity) = this /-caterwaul.merge/ {s: n[1 << 16] *[''] -seq, bits: 16, size: 0, n: 0, c: capacity} -re- void 0,
  label_sample.prototype /-caterwaul.merge/ label_sample_methods,

  where[label_methods = capture[reset()            = this -se [this.n = 0, this.sample = new label_sample(this.c)],
                                set(i, x)          = this -se [this.hashes[i] = x /-murmurhash3_32/ 0, this.sample /~push/ x],
                                pnorm(i)           = this.hashes[i] / 0x100000000,
                                p(i)               = this.pnorm(i),
                                h(i)               = this.hashes[i],
                                end()              = this.n /-Math.min/ this.c,
                                push(x, r)         = this.n++ < this.c ? this.set(this.n - 1, x) : this /x /~uniform_push/ r,
                                uniform_push(x, r) = this.set(r * this.n | 0, x) -when [r * this.n < this.c]],

        label_sample_methods = capture [reset()      = this -se [this.s = n[1 << 16] *[''] -seq, this.n = 0, this.size = 0, this.bits = 16],
                                        set(i, x)    = this -se [this.size += x.length - this.s[i].length, this.s[i] = x, unless [x == null]],
                                        push(x)      = this -se [this.set(x /-murmurhash3_32/ 0 & ~(-1 << this.bits), x), ++this.n, this.check_size()],

                                        check_size() = this -se [this.size * 2 > 4 * this.capacity ? this.collapse()
                                                               : this.size * 4 < 4 * this.capacity ? this.expand() : 0],
                                        collapse()   = this -se [this.s = n[1 << --this.bits] *[this.s[x] || this.s[x + 1 << this.bits]] -seq],
                                        expand()     = this -se [this.s = n[1 << ++this.bits] *[''] /seq -se-
                                                                          this.s *![it[murmurhash3_32(x, 0) & ~(-1 << this.bits)] = x] /seq]]]})();
24 core/jsplot/dataframe.waul
// Data frame: a collection of typed axes.
// Manages axis allocation, memory bounding, and input conversion. Also provides a layer of customization around coordinate mappings.

caterwaul(':all')(function () {
  dataframe(memory_limit) = this /-caterwaul.merge/ {axes: null, axis_types: null, n: 0, preview_lines: [], memory_limit: memory_limit} -re- void 0,
  dataframe.prototype /-caterwaul.merge/ dataframe_methods,

  where [dataframe_methods = capture [push(l)       = ++this.n <= 1024 ? this.preview_lines /~push/ l.split(/\t/) : this /~axis_push/ l.split(/\t/),
                                      axis_push(vs) = this.force_axes() -unless [this.axes] -then- this.axes *![x /vs[xi] /~push/ r] /seq
                                                                                            -where [r = Math.random()],
                                      eof()         = this.force_axes() -unless [this.axes],
                                      axes()        = this.axes ? this.axes.length : 0,
                                      axis(i)       = this.axes[i],
                                      capacity()    = this.axes ? this.memory_limit / this.axes.length >>> 3 : 0,

                                      force_axes()  = this.axes /eq[axis_types *[new window[x](axis_capacity)] -seq]
                                                      -then- this.axis_types /eq.axis_types
                                                      -then- this.preview_lines *!this.axis_push /seq
                                                      -where [n_axes        = this.preview_lines /[0][x0 /-Math.max/ x.length] -seq || 1,
                                                              axis_capacity = this.memory_limit / n_axes >>> 3,
                                                              self          = this,
                                                              is_numeric(x) = !isNaN(+x),
                                                              axis_type(i)  = self.preview_lines /[0][x0 + x[i] /!is_numeric] -seq,
                                                              axis_types    = n[n_axes] *[axis_type(x) > self.n/2 ? 'axis' : 'label'] -seq]]]})();
34 core/jsplot/matrix.waul
// Matrices.
// Not a complete library; just enough stuff to get 3D linear transformation and projection. This library also generates compiled functions for fast axis-specific
// transformation.

caterwaul(':all')(function () {
  matrix(x) = (x ? +x -seq : n[16] *[+((x>>2) == (x&3))] -seq) -se- it /-caterwaul.merge/ matrix_methods,
  matrix /-caterwaul.merge/ static_methods,

  where[static_methods = capture[translate(v)         = matrix() -se [it[3] = v[0], it[7] = v[1], it[11] = v[2]],
                                 scale(v)             = matrix() -se [it[0] = v[0], it[5] = v[1], it[10] = v[2]],
                                 rotate_x(t)          = matrix() -se [it[5] = it[10] = c, it[6] = -(it[9] = -s), where [s = t /!Math.sin, c = t /!Math.cos]],
                                 rotate_y(t)          = matrix() -se [it[0] = it[10] = c, it[2] = -(it[8] = -s), where [s = t /!Math.sin, c = t /!Math.cos]],
                                 prod(xs = arguments) = xs /[x0 /~dot/ x] -seq],

        matrix_methods = capture[dot             (b, a=this) = a *[n[4] /y[0][y0 + a[xi&12 | yi] * b[yi<<2 | xi&3]] -seq] /seq /!matrix,
                                 plus            (b, a=this) = a *[b[xi] + x]                                             /seq /!matrix,
                                 trace              (a=this) = a[0] + a[5] + a[10] + a[15],
                                 det                (a=this) = this.det_ -dcq [(t1 - t2 + t3 + t4 - t5) / 24
                                                                       -where [tr1 = a.trace(),  t1 = tr1 /-Math.pow/ 4,
                                                            a2 = a  /~dot/ a,  tr2 = a2.trace(), t2 = 6 * tr2 * tr1*tr1,
                                                            a3 = a2 /~dot/ a,  tr3 = a3.trace(), t3 = 3 * tr2*tr2,
                                                            a4 = a2 /~dot/ a2, tr4 = a4.trace(), t4 = 8 * tr3*tr1, t5 = 6 * tr4]],

                                 scaled_by       (y, a=this) = a *[x * y] /seq /!matrix,
                                 inv                (a=this) = this.inv_ -dcq [(matrix().scaled_by(1/6*t1) |~plus| a.scaled_by(-0.5*t2)
                                                                                  |~plus| a2.scaled_by(t3) |~plus| a3.scaled_by(-1)) /~scaled_by/ (1/a.det())
                                                                       -where [a2 = a /~dot/ a,    a3 = a2 /~dot/ a,
                                        tr1 = a.trace(),                      tr2 = a2.trace(),   tr3 = a3.trace(),
                                        t1  = tr1*tr1*tr1 - 3*tr1*tr2 + 2*tr3, t2 = tr1*tr1 - tr2, t3 = tr1]],

                                 transform       (v, a=this) = v *[n[4] /s[0][s0 + a[xi<<2|s]*v[s]] -seq] -seq,
                                 transformer_form(d, a=this) = qse[given[x, y, z] in _a*x + _b*y + _c*z + _d]
                                                               /~replace/ {_a: '#{a[d<<2|0]}', _b: '#{a[d<<2|1]}', _c: '#{a[d<<2|2]}', _d: '#{a[d<<2|3]}'},
                                 transformer     (d, a=this) = this.transformer_form(d) /!caterwaul.compile]]})();
12 core/jsplot/socket.waul
// Web socket interface.
// Manages downloads from the server, tracks state, invokes a callback for each batch of new data.

caterwaul(':all')(function () {
  ni_ws(cmd, cb) = cancel_existing() -then- ws_connect(cmd, cb),

  where[existing_connection         = null,
        cancel_existing()           = existing_connection /~send/ '' -rescue- null -then- existing_connection.close() -when.existing_connection,
        ni_url(cmd)                 = "#{document.location.href.replace(/^http:/, 'ws:').replace(/#.*/, '')}ni/#{cmd /!encodeURIComponent}",
        ws_connect(cmd, f)          = existing_connection = new WebSocket(cmd /!ni_url, 'data') -se [it.onmessage = f /!message_wrapper],
        message_wrapper(f, k='')(e) = e.data.constructor === Blob ? f() -then- cancel_existing()
                                                                  : k -eq[lines.pop()] -then- f(lines) -where[m = k + e.data, lines = m.split(/\n/)]]})();
118 core/jsplot/render.waul
// Rendering support.
// Rendering is treated like an asynchronous operation against the axis buffers. It ends up being re-entrant so we don't lock the browser thread, but those
// details are all hidden inside a render request.

caterwaul(':all')(function () {
  render(state = null, last_render = 0, frames_requested = 0)
        (axes, vm, l0, limit, sr, ctx, w, h, cb) = state /eq[{a: axes, vm: vm, ctx: ctx, w: w, h: h, i: 0, vt: n[4] *[vm.transformer(x)] -seq, l: 0, l0: l0,
                                                              total_shade: 0, saturation_rate: sr /!Math.exp, limit: limit, cb: cb,
                                                              id: state && state.id && state.ctx === ctx && state.w === w && state.h === h
                                                                    ? state.id
                                                                    : ctx.getImageData(0, 0, w, h)}]
                                                   -then- request_frame()

// Render function.
// This is kind of subtle, so I'll explain how it all works. My liberal use of scare quotes is indicative of the amount of duplicity going on around here.

// Rendering happens "asynchronously" -- that is, it's a time-bounded, reentrant process. The outermost loop governing the whole rendering process is driven by
// requestAnimationFrame(), which calls into render_part(). render_part() runs for about 20ms, rendering 1/4096th "random" slices of the data until it runs out of
// time. The assumption here is that 1/4096th of the data takes under 20ms, which empirically has been true on my machine.

// The renderer works by modifying RGBA values in an ImageData object, which is a little tricky because all of the drawing operations need to be translucent:
// conceptually we're rendering a volume rather than an opaque object. We also log-scale(-ish) the light output on the screen, so brightness(pixel) ~ log(volume
// depth). This is all done without storing any extra per-pixel state. Here's a full list of the shader properties we want:

// | 1. Singly-shaded pixels should be visible: i.e. rendered at rgb >= 64 (except for obvious counter-cases like slice focusing or distant points).
//   2. Multiply-shaded pixels should converge to full luminosity _and preserve color_, with luminosity ~ log(total volume depth).
//   3. Pixels should eventually saturate to white, losing color resolution as they gain luminosity. This should approximately double the dynamic range.
//   4. The algorithm should be properly additive so that antialiasing works (we want 2x2 subpixel rendering).
//   5. A bunch of very dim points should converge to the right color average (i.e. we can't lose too much detail to 8-bit quantization).
//   6. The total onscreen brightness should remain about the same regardless of how many points are on the screen (otherwise zooming would dim the display).

// So here's roughly how this all works. The RGB channels store full-value color all the time: this is a brightness-weighted average of the colors drawn into that
// pixel. Brightness is stored in the alpha channel and converges to 255 by an exponential series if all points are rendered at the same intensity.

// Point intensity is randomized for every pixel shading operation. We do this to break through the quantization artifacts we'd get if the intensity were
// constantly low.

  -where[slice_size      = 4096,
         slices          = n[slice_size] -seq -re- it.sort("Math.random() - 0.5".qf),
         request_frame() = render_part /!requestAnimationFrame -then- ++frames_requested -unless.frames_requested,
         render_part     = function () {

// Render state.
// Local variables for the axes and coordinate transformations, some misc stuff, and our luminosity adjustment and shade tracking. The luminosity adjustment is
// modified each iteration as we figure out how dense our shading is empirically; state.l0, the "target luminosity", is measured in full shades per screen pixel.
// Here's the calculation:

// | initial_l = target_shade_per_pixel * pixels / data_points;
//   next_l    = target_shade_per_pixel * pixels / (actual_shading_so_far / layers_so_far * total_layers);

    --frames_requested;
    var ax = state.a[0], ay = state.a[1],                  xt = state.vt[0], yt = state.vt[1], width  = state.id.width,
        az = state.a[2], aw = state.a[3], aq = state.a[4], zt = state.vt[2], wt = state.vt[3], height = state.id.height,
        id = state.id.data, n = state.a[0].end(), use_hue = !!aw, cx = width >> 1, cy = height >> 1,
        l  = state.l || state.l0 * (width*height) / n, total_shade = state.total_shade, s = width /-Math.min/ height >> 1,
        sr = state.saturation_rate, ss = state.limit || slice_size;

    if (state.cb)      state.cb(state.i, ss);
    if (state.i < ss)  request_frame();
    if (state.i === 0) id.fill(0);

    var t = +new Date;
    for (; state.i < ss && +new Date - t < 30; ++state.i) {
      for (var j = slices[state.i]; j < n; j += slice_size) {
        var w  = aw ? j /!aw.pnorm : 0, x  = ax ? j /!ax.p : 0, y  = ay ? j /!ay.p : 0, z  = az ? j /!az.p : 0,
            wi = 1 / wt(x, y, z),       xp = wi * xt(x, y, z),  yp = wi * yt(x, y, z),  zp = wi * zt(x, y, z),
            q  = aq ? j /!aq.pnorm : 1;

        if (zp > 0) {
          w *= 0.8;
          var r  = use_hue ? 1 - 2*(1/2 - Math.abs(.5  - w)) |-Math.min| 1 |-Math.max| 0.1 : 1,
              g  = use_hue ?     2*(1/2 - Math.abs(1/3 - w)) |-Math.min| 1 |-Math.max| 0.1 : 1,
              b  = use_hue ?     2*(1/2 - Math.abs(2/3 - w)) |-Math.min| 1 |-Math.max| 0.1 : 1,
              zi = 1/zp, tx = cx + xp*zi*s, ty = cy - yp*zi*s, sx = tx|0, sy = ty|0;

          if (sx >= 0 && sx < width-1 && sy >= 0 && sy < height-1) {
            tx -= sx; ty -= sy;
            if (zi > 1)
              for (var dx = 0; dx <= 1; ++dx)
                for (var dy = 0; dy <= 1; ++dy) {
                  var pi = (sy+dy)*width + sx+dx << 2,
                      op = (1 - Math.abs(dx-tx)) * (1 - Math.abs(dy-ty)),
                      lp = id[pi|3] || 64,
                      ci = l * op * (256 - lp) * q,
                      li = ci * zi*zi,
                      d  = sr / (ci + lp);

                  total_shade += li;
                  id[pi|3] += li;
                  id[pi|0] = (id[pi|0] * lp + r * 256 * ci) * d;
                  id[pi|1] = (id[pi|1] * lp + g * 256 * ci) * d;
                  id[pi|2] = (id[pi|2] * lp + b * 256 * ci) * d;
                }
            else {
              var pi = sy*width + sx << 2,
                  lp = id[pi|3] || 64,
                  ci = l * (256 - lp) * q,
                  li = ci * zi*zi,
                  d  = sr / (ci + lp);

              total_shade += li;
              id[pi|3] += li;
              id[pi|0] = (id[pi|0] * lp + r * 256 * ci) * d;
              id[pi|1] = (id[pi|1] * lp + g * 256 * ci) * d;
              id[pi|2] = (id[pi|2] * lp + b * 256 * ci) * d;
            }
          }
        }
      }

      if (total_shade) l = state.l0 * width * height / (total_shade / (state.i + 1) * ss);
    }

    state.l           = l;
    state.total_shade = total_shade;
    state.ctx.putImageData(state.id, 0, 0);
    last_render = +new Date;
  }]})();
57 core/jsplot/camera.waul
// Camera state, geometry, and UI.
// The camera contains an object matrix, a view matrix, and some render settings.

caterwaul(':all')(function ($) {
  camera(v) = jquery[div.camera /modus('composite', {br: '.brightness .number', ot: '.object-translation', os: '.object-scale',
                                                     sa: '.saturation .number', cr: '.camera-rotation',    cd: '.distance .number',
                                                     axes: '.axis-mapping input'})
                     >  div.vector.axis_mapping[axis_mapping() /~attr/ {title: 'Axis mapping'}]
                     >  div.vector.brightness  [log_number()   /~attr/ {title: 'View brightness'}]
                     >  div.vector.saturation  [log_number()   /~attr/ {title: 'White-saturation rate'}]
                     >  div.vector.distance    [log_number()   /~attr/ {title: 'Camera distance'}]
                     >= translation_ui() /~addClass/ 'object-translation' /~attr/ {title: 'Object translation'}
                     >= scale_ui()       /~addClass/ 'object-scale'       /~attr/ {title: 'Object scale'}
                     >= rotation_ui()    /~addClass/ 'camera-rotation'    /~attr/ {title: 'Camera rotation'}] -se- it.val(v) /when.v,

  camera /-$.extend/ wcapture [object_matrix(o)       = matrix.prod(o.os /!matrix.scale, o.ot /!matrix.translate),
                               camera_matrix(o)       = matrix.prod([0, 0, 1]                /!matrix.translate,
                                                                    [1/o.cd, 1/o.cd, 1/o.cd] /!matrix.scale,
                                                                    o.cr[0] / 360 * tau      /!matrix.rotate_x,
                                                                    o.cr[1] / 360 * tau      /!matrix.rotate_y),
                               norm(v)                = v[3] ? v |-v4scale| 1/v[3] : v,
                               m(o)                   = camera_matrix(o) /~dot/ object_matrix(o),

                               plane_lock(v)          = v *[xi === mi ? 0 : x] -seq -where [mi = n[3] /[v[x] /!Math.abs < v[x0] /!Math.abs ? x : x0] -seq],
                               axis_lock(v)           = v *[xi === mi ? x : 0] -seq -where [mi = n[3] /[v[x] /!Math.abs > v[x0] /!Math.abs ? x : x0] -seq],

                               iv_obj_locked(f)(o, v) = object_matrix(o).inv() /~transform/ f(camera_matrix(o).inv() /~transform/ v) /!norm,
                               iv_obj(o, v)           = camera_matrix(o).inv() /~transform/ v /!norm,
                               iv(o, v)               = m(o).inv()             /~transform/ v /!norm],

  $(given.e in $(document).on('mousewheel', '.log-number', given.e in $(this).val($(this).val() * Math.exp(e.deltaY * 0.01)).change())
                          .on('mousewheel', '.rotation',   given.e in $(this).val($(this).val() /-v2plus/ [e.deltaY * 0.1, e.deltaX * 0.1]).change())
                          .on('change',     '.number',     given.e in $(this).val($(this).val()))
                          .on('keydown',    '.number',     given.e[e.which === 38 ? $(this).val($(this).val() + 1).change() :
                                                                   e.which === 40 ? $(this).val($(this).val() - 1).change() : true])
                          .on('focus',      '.number',     given.e in $(this).select())),

  where[tagged(f, c)(v) = f(v) /~addClass/ c,
        nan_protect(x)  = x /!isNaN ? 0 : x,
        number_ui(n)(v) = jquery[input.number /modus(g, s) /val(v)] -where [g()  = this.modus('val') /!+eval /!n /!nan_protect,
                                                                            s(v) = this.modus('val', +v /!n /!nan_protect)],

        axis_mapping()  = jquery[input /modus(g, s)] -where [g()  = this.modus('val').split('') *[x.charCodeAt(0) - 65] -seq,
                                                             s(v) = this.modus('val', (v || 'ABCDE') *[String.fromCharCode(65 + x)] -seq -re- it.join(''))],

        log_number      = "_".qf       /!number_ui /-tagged/ 'log-number',
        linear_number   = "_".qf       /!number_ui /-tagged/ 'linear-number',
        angle_number    = "_ % 360".qf /!number_ui /-tagged/ 'linear-number',
        vector_ui(c)(v) = jquery[div.vector /modus('list', c)],

        translation_ui  = vector_ui(linear_number) /-tagged/ 'translation',
        scale_ui        = vector_ui(log_number)    /-tagged/ 'scale',
        rotation_ui     = vector_ui(angle_number)  /-tagged/ 'rotation',

        tau             = Math.PI * 2],

  using[caterwaul.merge(caterwaul.vector(2, 'v2'), caterwaul.vector(3, 'v3'), caterwaul.vector(4, 'v4'))]})(jQuery);
159 core/jsplot/interface.waul
// Page driver.

$(caterwaul(':all')(function ($) {
  setup_event_handlers(),

  $.fn.activate() = $(this) /~addClass/ 'active' -se [it /~data/ 'activation-timeout' /!clearTimeout,
                                                      it |'activation-timeout' |~data| "it /~removeClass/ 'active'".qf /-setTimeout/ 500],

  where[screen   = $('#screen'),    sc = screen[0]  /~getContext/ '2d',
        overlay  = $('#overlay'),   oc = overlay[0] /~getContext/ '2d',
        tr       = $('#transform'), lw = 0, mx = null, ms = false,
        status   = $('#status'),    lh = 0, my = null, mc = false,
        preview  = $('#preview'),
        explain  = $('#explain'),
        controls = $('#controls').modus('proxy', '.camera') /~addClass/ 'camera-mode noshift',
        w        = $(window).modus('composite', {ni: tr, v: controls}),

        default_settings()     = {ni: "//ni psplit// pord p'r pl 3' ,jABC.5 S8p'r prec(a+50, c*3.5+a*a/500), b, sin(a/100) + sin(b/100)' "
                                      + "S8,qABCD0.01 p'r a, - c, b, d'",
                                  v: {cr: [0, 0], os: [1, 1, 1], ot: [0, 0, 0], cd: 100, br: 1, sa: 0.03, axes: n[4] -seq}},

        size_changed()         = (lw !== cw || lh !== ch) -se [lw = cw, lh = ch] -where [cw = w.width(), ch = w.height()],
        resize_canvases()      = overlay.add(screen) /~attr/ {width: lw, height: lh} -then- update_screen(),
        resize_other_stuff()   = tr      /~css/ {height: 0} /~css/ {height: tr[0].scrollHeight - 2, width: lw-2}
                          -then- preview /~css/ {top: tr.height() + 3, bottom: 1}
                          -then- explain /~css/ {top: tr.height() + 3, left: preview.width() + 12},
        handle_resizes()       = resize_canvases() -then- resize_other_stuff() -when- size_changed(),

        update_status(t)       = status.children('#sizelabel').text(t).activate(),

        object_mode            = false,
        toggle_object_mode()   = controls.toggleClass('object-mode', object_mode = !object_mode)
                                         .toggleClass('camera-mode', !object_mode),

        view_change(k, f, v)   = w.val(w.val() -se [it.v[k] = it.v[k] /-f/ v]),

        data_lock_vector()     = data_state.frame.axes.length >= 3 ? [1, 1, 1] : [1, 1, 0],
        screen_scale()         = (lw /-Math.min/ lh) / 2,
        drag(dx, dy, s)        = s ? 'cr' /v2plus /-view_change/ [dy * 180 / screen_scale(), -dx * 180 / screen_scale()]
                                   : w.val() /se    [it.v.ot = it.v.ot /-v3plus/ modify(it.v, [dx / screen_scale(), -dy / screen_scale(), 0, 0])
                                                                       /-v3times/ data_lock_vector()]
                                             /where [modify = object_mode ? camera.iv_obj_locked(camera.axis_lock) : camera.iv]
                                             /!w.val,

        wheel(dx, dy, s)       = object_mode ? 'os' |v3times |-view_change| [Math.exp(sx * 0.01 * (d[0] >= d[2])),
                                                                             Math.exp(sy * 0.01),
                                                                             Math.exp(sx * 0.01 * (d[2] >= d[0]))]
                                                                     -where [d = camera.iv_obj(w.val().v, [1, 0, 0, 0]) *Math.abs -seq,
                                                                             sx = s ? dy || dx : dx,
                                                                             sy = s ? 0        : dy]
                                             : 'cd' |a*b -given[a, b] |-view_change| Math.exp(dy * -0.01),

        check_syntax(v)        = $.getJSON('/parse/#{v /!encodeURIComponent}', update_explain)
                         -where [update_explain(r)  = explain.empty() /~append/ explanation_for(r.ops)
                                                                      /~prepend/ errors_for(r.unparsed),
                                 explanation_for(x) = jquery[pre /text(s)] -where [s = x *JSON.stringify /seq -re- it.join("\n")],
                                 errors_for(u)      = u.length ? jquery[div.errors > code /text(u /~join/ ' ')] : []],

        setup_event_handlers() = tr /~keydown/ given.e [e.which === 13 && !e.shiftKey ? w.val().ni /!visualize -then- false : true]
                                      /~keyup/ given.e [$(this).change() -then- w.val().ni /!check_syntax]
                                      /~focus/ given.e [explain.show()]
                                       /~blur/ given.e [explain.hide()]
                          -then- overlay     /~mousedown/ given.e [mx = e.pageX, my = e.pageY, ms = e.shiftKey, true]
                                            /~mousewheel/ given.e [wheel(e.deltaX, e.deltaY, e.shiftKey), update_screen_fast()]
                          -then- $(document) /~mousemove/ given.e [drag(x - mx, y - my, ms), mx = x, my = y, ms = e.shiftKey, update_screen_fast(),
                                                                   where [x = e.pageX, y = e.pageY], when.mx]
                                               /~mouseup/ given.e [mx = null, update_screen(), when.mx]
                                               /~keydown/ given.e [e.which === 9 ? toggle_object_mode() -then- false
                                                                 : e.which === 16 ? controls /~addClass/    'shift' : true]
                                                 /~keyup/ given.e [e.which === 16 ? controls /~removeClass/ 'shift' : true]
                          -then- w /~resize/ handle_resizes
                          -then- controls /~append/ camera().change(update_screen_fast)
                          -then- $('#object-mode, #camera-mode') /~click/ toggle_object_mode
                          -then- $('canvas').attr('unselectable', 'on').css('user-select', 'none').on('selectstart', false)
                          -then- $('.autohide') /~click/ "$(this) /~toggleClass/ 'pinned'".qf
                          -then- $('#search input') /~change/ "update_selected($(this).val())".qf
                                                     /~keyup/ search_complete /~focus/ "$('#search-auto').show()".qf
                                                                              /~blur/  "$('#search-auto').hide()".qf
                          -then- $('#search-auto').on('mousedown', '.option', "$('#search input') /~val/ $(this).text()".qf)
                                                  .on('mouseover', '.option', "update_selected($(this).text()) -then- update_overlay()".qf)

                          -then- handle_resizes /-setTimeout/ 10
                          -then- "document.location.hash = $(this).val() /!JSON.stringify /!encodeURI".qf /-setInterval/ 50
                          -then- w /~val/ $.extend(default_settings(), document.location.hash.substr(1) /!decodeURIComponent /!JSON.parse -rescue- {})
                          -then- tr.val() /!visualize,

        reset_data_state()   = data_state = {frame: new dataframe(128 * 1048576), preview_done: false, bytes: 0, last_render: 0} -se- preview /~text/ '',
        data_state           = null -se- reset_data_state(),

        data_was_revised(ls) = update_screen() /when[+new Date - data_state.last_render > data_state.frame.axes[0].end() / 100]
                      -then- '#{ats} / #{data_state.frame.axes[0].n}[#{data_state.frame.capacity()}] / #{kb /!Math.round}K'
                             /!update_status
                             /where [ats = data_state.frame.axis_types *[x.substr(0, 1)] -seq -re- it.join(''),
                                     kb  = (data_state.bytes += ls /[0][x0 + x.length + 1] -seq + 0.0) / 1024]
                      -when [data_state.frame.axes && data_state.frame.axes[0]]
                      -then- preview.text(data_state.frame.preview_lines *[x /~join/ '\t'] -seq -re- it.join('\n').substr(0, 65536))
                             /then[data_state.preview_done = data_state.frame.preview_lines.length >= 1024]
                             /unless[data_state.preview_done],

        visualize(cmd)     = reset_data_state() -then- ni_ws(cmd, handle_data)
                      -where [handle_data(ls) = ls ? ls *!data_state.frame.push -seq -then- data_was_revised(ls)
                                                   : data_state.frame.eof()          -then- data_was_revised([])],

        label_axes()       = data_state.frame.axes ? data_state.frame.axes %[x.constructor === label] -seq : [],
        search_complete()  = $('#search-auto').empty() |~append| options *[jquery[div.option /text(x)]] -seq
                             -where [v           = $('#search input').val().toLowerCase(),
                                     option_keys = {} -se [label_axes() *![x.sample.s
                                                                        *![it[x] /eq.it -when [x.length && x.toLowerCase() /~indexOf/ v !== -1]] -seq] -seq],
                                     options     = option_keys /keys -seq -re- it.sort().slice(0, 1024)],

        selected_points    = [],
        update_selected(s) = selected_points /eq.new_selected -then- w.val().v /!update_overlay
                     -where [h            = s /-murmurhash3_32/ 0,
                             new_selected = (function () {
                                              var r = [], las = label_axes();
                                              for (var li = 0; li < las.length; ++li)
                                                for (var i = 0, a = las[li]; i < a.end(); ++i)
                                                  if (a.h(i) === h)
                                                    r.push(i);
                                              return r;
                                            })()],

        update_overlay(v)  = oc.clearRect(0, 0, lw, lh) -then- outline_points('#f60', selected_points)
                     -where [scale          = lw /-Math.min/ lh >>> 1,
                             cx             = lw >>> 1,
                             cy             = lh >>> 1,
                             axes           = data_state.frame.axes /!axis_map,
                             m              = v /!camera.m,
                             outline_points = function (c, is) {
                               oc.strokeStyle = c;
                               var t = +new Date;
                               for (var i = 0; i < is.length && +new Date - t < 20; ++i) {
                                 var pi = is[i];
                                 var p  = m.transform([axes[0] ? axes[0].p(pi) : 0,
                                                       axes[1] ? axes[1].p(pi) : 0,
                                                       axes[2] ? axes[2].p(pi) : 0, 1] /!camera.norm);
                                 if (p[2] > 0) oc.strokeRect(cx + scale*p[0]/p[2] - 2, cy - scale*p[1]/p[2] - 2, 5, 5);
                               }
                             }],

        axis_map(as)         = w.val().v.axes *[as[x]] -seq,
        renderer             = render(),
        full_render_tmout    = null,
        update_screen_fast() = renderer(data_state.frame.axes /!axis_map, v /!camera.m, v.br, preview_slices, v.sa, sc, screen.width(), screen.height())
                               /where [preview_factor = Math.min(1, data_state.frame.n / data_state.frame.capacity()),
                                       preview_slices = Math.min(4096, 128 / preview_factor | 0)]
                        -then- full_render_tmout /!clearTimeout
                        -then- full_render_tmout /eq[update_screen /-setTimeout/ 50]
                        -where [v = w.val().v],

        update_render_status(i, m) = $('#render-bar-inner') /~css/ {width: $('#render-bar').width() * i/m} -then- status.activate(),
        update_screen()            = full_render_tmout /!clearTimeout
                             -then-  renderer(data_state.frame.axes /!axis_map, v /!camera.m, v.br, 0, v.sa, sc, screen.width(), screen.height(), update_render_status)
                             -then-  update_overlay(v)
                             -then-  data_state.last_render /eq[+new Date]
                             -when  [data_state.frame.axes && +new Date - data_state.last_render > 50]
                             -where [v = w.val().v]],

  using[caterwaul.merge({}, caterwaul.vector(2, 'v2'), caterwaul.vector(3, 'v3'), caterwaul.vector(4, 'v4'))]}));
78 core/jsplot/css
body {margin:0; color:#eee; background:black; font-size:10pt; font-family:monospace; overflow: hidden}

#screen, #overlay {position:absolute}

::-webkit-scrollbar {width:12px; height:4px}
::-webkit-scrollbar-track {background:rgba(255,255,255,0.1)}
::-webkit-scrollbar-thumb {background:rgba(255,255,255,0.5)}

*:focus, *:hover, .pinned, .active {opacity:1 !important}

#status {position:absolute; right:2px; bottom:2px; width:400px; opacity:0.2; text-align:right; z-index:9}
#render-bar {display:inline-block; width:40px; height:4px; border:solid 1px rgba(255,255,255,0.5)}
#render-bar-inner {background:rgba(255,255,255,0.5); height:4px}

#search {position:absolute; right:400px; bottom:0; width:400px; z-index:9}
#search input {width:400px; font-family:monospace; color:#eee; border:none; outline:none; background:transparent; border-top:solid 1px transparent}

#search input:focus {border-top:solid 1px #f60}
#search input:hover, #search input:focus {background:rgba(0,0,0,0.75)}

#search-auto .option {cursor:pointer}
#search-auto .option:hover {background:rgba(96,96,96,0.75)}
#search-auto {max-height:700px; width:400px; overflow:auto}
#search-auto:hover {background:rgba(0,0,0,0.75)}

#transform {background:none; margin:0; color:#eee; position:absolute; left:0; top:0; border:none; outline:none; padding:1px 0;
            border-bottom:solid 1px transparent; font-family:monospace; z-index:9}

#transform:focus,
#transform:hover {background:rgba(0,0,0,0.75)}
#transform:focus {border-bottom:solid 1px #f60}

#explain {z-index:9; position:absolute; padding:4px}
#explain > .errors {color:#f20}
#explain > .errors::before {color:#f20; content:'error> '}

#preview {z-index:9; color:transparent; max-width:1px; overflow-x:auto; position:absolute; left:0; padding-right:12px; overflow-y:show; margin:0;
          background:rgba(0,0,0,0.75); cursor:default; font-family:monospace}
#preview:hover, #preview.pinned {color:#eee; max-width:100%}
#preview.pinned {border-right:solid 1px #f60}

#controls {position:absolute; width:192px; right:-184px; bottom:14pt; z-index:9; background:rgba(0,0,0,0.75); border-top:solid 1px transparent}
#controls:hover, #controls.pinned {right:0}
#controls.pinned {border-top:solid 1px #f60}

.vector input {background:none; border:none; margin:0; color:#eee; padding:4px; font-family:sans-serif; font-size:14pt; width:132px; cursor:default}

.vector     {display:inline-block; padding:4px; border-left:solid 8px rgba(96,96,96,0.5); background:rgba(64,64,64,0.25); margin:2px 0}
.vector > * {display:inline-block}

.object-translation::before {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'+'; padding:4px}
.object-scale::before       {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'x'; padding:4px}
.camera-rotation::before    {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'R'; padding:4px}
.distance::before           {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'D'; padding:4px}
.brightness::before         {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'B'; padding:4px}
.saturation::before         {float:right; font-size:14pt; color:rgba(192,192,192,0.5); content:'S'; padding:4px}

#controls > label {font-family:sans-serif; font-size:14pt; color:rgba(96,96,96,0.5)}
#controls > label::before {color:rgba(192,192,192,0.5); content:' [ '}
#controls > label::after  {color:rgba(192,192,192,0.5); content:' ] '}
#controls.camera-mode > #camera-mode {color:#f60}
#controls.object-mode > #object-mode {color:#f60}

#controls.camera-mode.noshift .vector.object-translation {border-left:solid 8px #f60}
#controls.camera-mode.noshift .distance                  {border-left:solid 8px #f60}

#controls.camera-mode.shift .vector.object-translation {border-left:solid 8px rgba(96,96,96,0.5)}
#controls.camera-mode.shift .vector.camera-rotation    {border-left:solid 8px #f60}
#controls.camera-mode.shift .distance                  {border-left:solid 8px rgba(96,96,96,0.5)}

#controls.object-mode.noshift .vector.object-translation {border-left:solid 8px #f60}
#controls.object-mode.noshift .vector.object-scale       {border-left:solid 8px #f60}
#controls.object-mode.noshift .distance                  {border-left:solid 8px rgba(96,96,96,0.5)}

#controls.object-mode.shift .vector.object-translation {border-left:solid 8px rgba(96,96,96,0.5)}
#controls.object-mode.shift .vector.object-scale       {border-left:solid 8px #f60}
#controls.object-mode.shift .vector.camera-rotation    {border-left:solid 8px #f60}
#controls.object-mode.shift .distance                  {border-left:solid 8px rgba(96,96,96,0.5)}
27 core/jsplot/html
<!doctype html>
<html>
<head>
<title>ni/jsplot</title>
<style>%css</style>
<script>%js</script>
</head>
<body>
<textarea spellcheck='false' id='transform'></textarea>
<div id='explain'></div>
<pre id='preview' class='autohide'></pre>
<canvas id='screen'></canvas>
<canvas id='overlay'></canvas>
<div id='controls' class='autohide'>
  <label id='camera-mode'>camera</label>
  <label id='object-mode'>object</label>
</div>
<div id='search'>
  <div id='search-auto'></div>
  <input id='search-text'></input>
</div>
<div id='status'>
  <div id='render-bar'><div id='render-bar-inner'></div></div>
  <label id='sizelabel'></label>
</div>
</body>
</html>
93 core/jsplot/jsplot.pl
# JSPlot interop.
# JSPlot is served over HTTP as a portable web interface. It requests data via
# AJAX, and may request the same data multiple times to save browser memory. The
# JSPlot driver buffers the data to disk to make it repeatable.

use constant jsplot_gen => gen $ni::self{'core/jsplot/html'};

use constant jsplot_html =>
  jsplot_gen->(css => $ni::self{'core/jsplot/css'},
               js  => join '', @ni::self{qw| core/jsplot/jquery.min.js
                                             core/jsplot/jquery.mousewheel.min.js
                                             core/caterwaul/caterwaul.min.js
                                             core/caterwaul/caterwaul.std.min.js
                                             core/caterwaul/caterwaul.ui.min.js
                                             core/jsplot/murmurhash3.js
                                             core/jsplot/modus.js
                                             core/jsplot/vector.js
                                             core/jsplot/axis.waul
                                             core/jsplot/label.waul
                                             core/jsplot/dataframe.waul
                                             core/jsplot/matrix.waul
                                             core/jsplot/socket.waul
                                             core/jsplot/render.waul
                                             core/jsplot/camera.waul
                                             core/jsplot/interface.waul |});

# Parsing.
# This entry point provides a realtime CLI parse for the UI.

sub jsplot_parse($$@) {
  my ($reply, $req, @ni_args) = @_;
  my ($ops, @rest) = cli @ni_args;
  http_reply $reply, 200, json_encode {ops => $ops, unparsed => [@rest]};
}

# JSPlot data streaming.
# This is the websocket connection that ni uses to stream data to the client. Any
# data we receive from the client indicates that the client is canceling the
# websocket request, so we need to break the pipe and kill off subprocesses.

sub jsplot_log($@) {printf STDERR "ni js[$$]: $_[0]", @_[1..$#_]}

sub jsplot_stream($$@) {
  local $_;
  my ($reply, $req, @ni_args) = @_;
  my ($ops, @rest) = cli @ni_args;
  die "ni: jsplot failed to parse starting at @rest" unless defined $ops;

  jsplot_log "running %s\n", json_encode $ops;

  safewrite $reply, ws_header($req);
  my $ni_pipe = sni @$ops, http_websocket_encode_batch_op 65536;

  my $incoming;
  my $rmask = '';
  vec($rmask, fileno $reply, 1) = 1;

  while (saferead $ni_pipe, $_, 65536) {
    if (select my $rout = $rmask, undef, undef, 0) {
      saferead $reply, $incoming, 8192;
      if (length $incoming) {
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
    return jsplot_parse($reply, $req, shell_unquote $1)  if $url =~ /^\/parse\/([\s\S]*)/;
    return jsplot_stream($reply, $req, shell_unquote $1) if $url =~ /^\/ni\/([\s\S]*)/;
    return http_reply $reply, 404, $url;
  };
}

defclispecial '--js', q{jsplot_server $_[0] || 8090}, <<'_';
Usage: ni --js [port=8090]
Runs a web interface that allows you to visualize data streams. See ni
//help/visual for details.
_
1 core/mapomatic/lib
mapomatic.pl
60 core/mapomatic/mapomatic.pl
# Map-O-Matic
# Generates a data: url that maps points on a page using Leaflet.

sub mapomatic_compress {
  local $_ = shift;
  s/^\s+//mg; s/\v+//g; s/\s+/ /g;
  $_;
}

use constant mapomatic_header => <<'EOF';
<html>
<head>
  <link rel="stylesheet"href="http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.3/leaflet.css"/>
  <script src="http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.3/leaflet.js"></script>
  <script src="http://code.jquery.com/jquery-1.11.3.min.js"></script>
  <style>body {margin: 0}</style>
</head>
<body id='map'>
  <script type='geodata'>
EOF

use constant mapomatic_footer => <<'EOF';
  </script>
  <script>
  $(function () {
    var rf = function () {
      if ($('#map').height() !== $(window).height())
        $('#map').height($(window).height());
    };
    $(window).resize(rf);
    setTimeout(rf, 100);
    var m = L.map('map');
    L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png',
                {attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'}).addTo(m);
    var sy = 0, sx = 0;
    var ls = $('script[type="geodata"]').text().replace(/^\s*|\s*$/g, '').split(/~/);
    for (var i = 0, l = ls.length; i < l; ++i) {
      var ps = ls[i].split(/\s+/);
      var ll = [+ps[0], +ps[1]];
      L.marker(ll).addTo(m).bindPopup(ps.slice(2).join(' '));
      sy += ll[0];
      sx += ll[1];
    }
    m.setView([sy / ls.length, sx / ls.length], 4);
  });
  </script>
</body>
</html>
EOF

defoperator mapomatic => q{
  eval {require MIME::Base64};
  my $encoded_points = join "~", map mapomatic_compress($_), <STDIN>;
  print "data:text/html;base64," . MIME::Base64::encode_base64(
    mapomatic_compress(mapomatic_header)
      . $encoded_points
      . mapomatic_compress(mapomatic_footer)) . "\n\0";
};

defshort '/MM',  pmap q{mapomatic_op}, pnone;
1 core/docker/lib
docker.pl
88 core/docker/docker.pl
# Pipeline dockerization.
# Creates a transient container to execute a part of your pipeline. The image you
# specify needs to have Perl installed, but that's about it.

# Prebuilt image case.
# This is what happens by default, and looks like `ni Cubuntu[g]`.

use constant docker_image_name => prx '[^][]+';

defoperator docker_run_image => q{
  my ($image, @f) = @_;
  my $fh = siproc {exec qw|docker run --rm -i|, $image, ni_quoted_exec_args};
  quote_ni_into $fh, @f;
};

# On-demand images.
# These are untagged images built on the fly. NB: workaround here for old/buggy
# versions of the docker client; here's what's going on.

# Normally you'd use `docker build -q` and get an image ID, but some older docker
# clients ignore the `-q` option and emit full verbose output (and, in
# unexpectedly barbaric fashion, they also emit this verbosity to standard out,
# not standard error). So we instead go through the silliness of tagging and then
# untagging the image.

defoperator docker_run_dynamic => q{
  my ($dockerfile, @f) = @_;
  my $fh = siproc {
    my $quoted_dockerfile = shell_quote 'printf', '%s', $dockerfile;
    my $quoted_args       = shell_quote ni_quoted_exec_args;
    my $image_name        = "ni-tmp-" . lc noise_str 32;
    sh qq{image_name=\`$quoted_dockerfile | docker build -q -\`
          if [ \${#image_name} -gt 80 ]; then \\
            $quoted_dockerfile | docker build -q -t $image_name - >&2
            image_name=$image_name
            docker run --rm -i \$image_name $quoted_args
            docker rmi --no-prune=true \$image_name
          else
            docker run --rm -i \$image_name $quoted_args
          fi};
  };
  quote_ni_into $fh, @f;
};

sub alpine_dockerfile {
  join "\n", 'FROM alpine',
             q{RUN echo '@edge http://nl.alpinelinux.org/alpine/edge/main' \
                   >> /etc/apk/repositories \
                && echo '@testing http://nl.alpinelinux.org/alpine/edge/testing' \
                   >> /etc/apk/repositories \
                && echo '@community http://nl.alpinelinux.org/alpine/edge/community' \
                   >> /etc/apk/repositories \
                && apk update \
                && apk add perl},
             map "RUN apk add $_", @_;
}

sub ubuntu_dockerfile {
  join "\n", 'FROM ubuntu',
             'RUN apt-get update',
             map "RUN apt-get install -y $_", @_;
}

use constant docker_package_list => pmap q{[/\+([^][+]+)/g]}, prx '[^][]+';

defshort '/C',
  defalt 'dockeralt', 'alternatives for the /C containerize operator',
    pmap(q{docker_run_dynamic_op alpine_dockerfile(@{$$_[0]}), @{$$_[1]}},
         pseq pn(1, prc 'A', pc docker_package_list), _qfn),
    pmap(q{docker_run_dynamic_op ubuntu_dockerfile(@{$$_[0]}), @{$$_[1]}},
         pseq pn(1, prc 'U', pc docker_package_list), _qfn),
    pmap(q{docker_run_image_op $$_[0], @{$$_[1]}},
         pseq pc docker_image_name, _qfn);

# Execution within existing containers.
# Same idea as running a new Docker, but creates a process within an existing
# container.

use constant docker_container_name => docker_image_name;

defoperator docker_exec => q{
  my ($container, @f) = @_;
  my $fh = siproc {exec qw|docker exec -i|, $container, ni_quoted_exec_args};
  quote_ni_into $fh, @f;
};

defshort '/E', pmap q{docker_exec_op $$_[0], @{$$_[1]}},
               pseq pc docker_container_name, _qfn;
2 core/hadoop/lib
hadoop-conf.pl
hadoop.pl
361 core/hadoop/hadoop-conf.pl
# MapReduce configuration is a huge pain;
# we aim to make it a little easier.

# Derived from https://github.com/Yelp/mrjob/blob/master/mrjob/compat.py
 
our %mr_generics = (
'Hdb',      'dfs.blocksize',
'Hdbpc',    'dfs.bytes-per-checksum',
'Hdcwps',   'dfs.client-write-packet-size',
'Hdchkr',   'dfs.client.https.keystore.resource',
'Hdchna',   'dfs.client.https.need-auth',
'Hdcrps',   'dfs.client.read.prefetch.size',
'Hdcst',    'dfs.client.socket-timeout',
'Hddns',    'dfs.datanode.StorageId',
'Hddnbb',   'dfs.datanode.balance.bandwidthPerSec',
'Hddndd',   'dfs.datanode.data.dir',
'Hddnh',    'dfs.datanode.hostname',
'Hddnmtt',  'dfs.datanode.max.transfer.threads',
'Hdmsi',    'dfs.metrics.session-id',
'Hdnnap',   'dfs.namenode.accesstime.precision',
'Hdnnba',   'dfs.namenode.backup.address',
'Hdnnbha',  'dfs.namenode.backup.http-address',
'Hdnncd',   'dfs.namenode.checkpoint.dir',
'Hdnnced',  'dfs.namenode.checkpoint.edits.dir',
'Hdnncp',   'dfs.namenode.checkpoint.period',
'Hdnned',   'dfs.namenode.edits.dir',
'Hdnnhri',  'dfs.namenode.heartbeat.recheck-interval',
'Hdnnhttp', 'dfs.namenode.http-address',
'Hdnnhttps','dfs.namenode.https-address',
'Hdnnmo',   'dfs.namenode.max.objects',
'Hdnnnd',   'dfs.namenode.name.dir',
'Hdnnndr',  'dfs.namenode.name.dir.restore',
'Hdnnrc',   'dfs.namenode.replication.considerLoad',
'Hdnnri',   'dfs.namenode.replication.interval',
'Hdnnrms',  'dfs.namenode.replication.max-streams',
'Hdnnrm',   'dfs.namenode.replication.min',
'Hdnnrpts', 'dfs.namenode.replication.pending.timeout-sec',
'Hdnnse',   'dfs.namenode.safemode.extension',
'Hdnnstp',  'dfs.namenode.safemode.threshold-pct',
'Hdnnsha',  'dfs.namenode.secondary.http-address',
'Hdnnup',   'dfs.namenode.upgrade.permission',
'Hdpe',     'dfs.permissions.enabled',
'Hdps',     'dfs.permissions.superusergroup',
'Hfcbd',    'fs.client.buffer.dir',
'Hfd',      'fs.defaultFS',
'Hfdi',     'fs.df.interval',
'Hinla',    'io.native.lib.available',
'Hccp',     'mapreduce.client.completion.pollinterval',
'Hcgu',     'mapreduce.client.genericoptionsparser.used',
'Hcof',     'mapreduce.client.output.filter',
'Hcpp',     'mapreduce.client.progressmonitor.pollinterval',
'Hcsfr',    'mapreduce.client.submit.file.replication',
'Hcae',     'mapreduce.cluster.acls.enabled',
'Hcld',     'mapreduce.cluster.local.dir',
'Hcmm',     'mapreduce.cluster.mapmemory.mb',
'Hcps',     'mapreduce.cluster.permissions.supergroup',
'Hcrm',     'mapreduce.cluster.reducememory.mb',
'Hctd',     'mapreduce.cluster.temp.dir',
'Hfdfs',    'mapreduce.fieldsel.data.field.separator',
'Hfmokvfs', 'mapreduce.fieldsel.map.output.key.value.fields.spec',
'Hfrokvfs', 'mapreduce.fieldsel.reduce.output.key.value.fields.spec',
'Hifi',     'mapreduce.input.fileinputformat.inputdir',
'Hifsmax',  'mapreduce.input.fileinputformat.split.maxsize',
'Hifsmin',  'mapreduce.input.fileinputformat.split.minsize',
'Hifsmpn',  'mapreduce.input.fileinputformat.split.minsize.per.node',
'Hifsmpr',  'mapreduce.input.fileinputformat.split.minsize.per.rack',
'Hikkvs',   'mapreduce.input.keyvaluelinerecordreader.key.value.separator',
'Hill',     'mapreduce.input.lineinputformat.linespermap',
'Hillm',    'mapreduce.input.linerecordreader.line.maxlength',
'Himdf',    'mapreduce.input.multipleinputs.dir.formats',
'Himdm',    'mapreduce.input.multipleinputs.dir.mappers',
'Hipc',     'mapreduce.input.pathFilter.class',
'Hisc',     'mapreduce.input.sequencefileinputfilter.class',
'Hisf',     'mapreduce.input.sequencefileinputfilter.frequency',
'Hisr',     'mapreduce.input.sequencefileinputfilter.regex',
'Hjca',     'mapreduce.job.cache.archives',
'Hjcat',    'mapreduce.job.cache.archives.timestamps',
'Hjcf',     'mapreduce.job.cache.files',
'Hjcft',    'mapreduce.job.cache.files.timestamps',
'Hjcla',    'mapreduce.job.cache.local.archives',
'Hjclf',    'mapreduce.job.cache.local.files',
'Hjcsc',    'mapreduce.job.cache.symlink.create',
'Hjcpa',    'mapreduce.job.classpath.archives',
'Hjcpf',    'mapreduce.job.classpath.files',
'Hjcc',     'mapreduce.job.combine.class',
'Hjcscn',   'mapreduce.job.committer.setup.cleanup.needed',
'Hjenra',   'mapreduce.job.end-notification.retry.attempts',
'Hjenri',   'mapreduce.job.end-notification.retry.interval',
'Hjenu',    'mapreduce.job.end-notification.url',
'Hji',      'mapreduce.job.id',
'Hjic',     'mapreduce.job.inputformat.class',
'Hjj',      'mapreduce.job.jar',
'Hjjn',     'mapreduce.job.jvm.numtasks',
'Hjld',     'mapreduce.job.local.dir',
'Hjmc',     'mapreduce.job.map.class',
'Hjm',      'mapreduce.job.maps',
'Hjmpt',    'mapreduce.job.maxtaskfailures.per.tracker',
'Hjn',      'mapreduce.job.name',
'Hjogcc',   'mapreduce.job.output.group.comparator.class',
'Hjokc',    'mapreduce.job.output.key.class',
'Hjokcc',   'mapreduce.job.output.key.comparator.class',
'Hjovc',    'mapreduce.job.output.value.class',
'Hjoc',     'mapreduce.job.outputformat.class',
'Hjpc',     'mapreduce.job.partitioner.class',
'Hjp',      'mapreduce.job.priority',
'Hjq',      'mapreduce.job.queuename',
'Hjrc',     'mapreduce.job.reduce.class',
'Hjrsc',    'mapreduce.job.reduce.slowstart.completedmaps',
'Hjr',      'mapreduce.job.reduces',
'Hjso',     'mapreduce.job.skip.outdir',
'Hjs',      'mapreduce.job.skiprecords',
'Hjssnt',   'mapreduce.job.speculative.slownodethreshold',
'Hjsstt',   'mapreduce.job.speculative.slowtaskthreshold',
'Hjssc',    'mapreduce.job.speculative.speculativecap',
'Hjun',     'mapreduce.job.user.name',
'Hjurh',    'mapreduce.job.userlog.retain.hours',
'Hjwd',     'mapreduce.job.working.dir',
'Hjcci',    'mapreduce.jobcontrol.createdir.ifnotexist',
'Hjta',     'mapreduce.jobtracker.address',
'Hjtbat',   'mapreduce.jobtracker.blacklist.average.threshold',
'Hjteti',   'mapreduce.jobtracker.expire.trackers.interval',
'Hjthc',    'mapreduce.jobtracker.handler.count',
'Hjthis',   'mapreduce.jobtracker.heartbeats.in.second',
'Hjthef',   'mapreduce.jobtracker.hosts.exclude.filename',
'Hjthf',    'mapreduce.jobtracker.hosts.filename',
'Hjtha',    'mapreduce.jobtracker.http.address',
'Hjti',     'mapreduce.jobtracker.instrumentation',
'Hjtjbs',   'mapreduce.jobtracker.jobhistory.block.size',
'Hjtjcl',   'mapreduce.jobtracker.jobhistory.completed.location',
'Hjtjl',    'mapreduce.jobtracker.jobhistory.location',
'Hjtjlcs',  'mapreduce.jobtracker.jobhistory.lru.cache.size',
'Hjtjt',    'mapreduce.jobtracker.jobinit.threads',
'Hjtmmm',   'mapreduce.jobtracker.maxmapmemory.mb',
'Hjtmrm',   'mapreduce.jobtracker.maxreducememory.mb',
'Hjtmp',    'mapreduce.jobtracker.maxtasks.perjob',
'Hjtpja',   'mapreduce.jobtracker.persist.jobstatus.active',
'Hjtpjd',   'mapreduce.jobtracker.persist.jobstatus.dir',
'Hjtpjh',   'mapreduce.jobtracker.persist.jobstatus.hours',
'Hjtrr',    'mapreduce.jobtracker.restart.recover',
'Hjtrcs',   'mapreduce.jobtracker.retiredjobs.cache.size',
'Hjtr',     'mapreduce.jobtracker.retirejobs',
'Hjtsd',    'mapreduce.jobtracker.system.dir',
'Hjttl',    'mapreduce.jobtracker.taskcache.levels',
'Hjtt',     'mapreduce.jobtracker.taskscheduler',
'Hjttmp',   'mapreduce.jobtracker.taskscheduler.maxrunningtasks.perjob',
'Hjtttc',   'mapreduce.jobtracker.taskscheduler.taskalloc.capacitypad',
'Hjtttm',   'mapreduce.jobtracker.tasktracker.maxblacklists',
'Hjtwt',    'mapreduce.jobtracker.webinterface.trusted',
'Hje',      'mapreduce.join.expr',
'Hjk',      'mapreduce.join.keycomparator',
'Hmcm',     'mapreduce.map.combine.minspills',
'Hmds',     'mapreduce.map.debug.script',
'Hme',      'mapreduce.map.env',
'Hmfm',     'mapreduce.map.failures.maxpercent',
'Hmif',     'mapreduce.map.input.file',
'Hmil',     'mapreduce.map.input.length',
'Hmis',     'mapreduce.map.input.start',
'Hmjo',     'mapreduce.map.java.opts',
'Hmll',     'mapreduce.map.log.level',
'Hmm',      'mapreduce.map.maxattempts',
'Hmmm',     'mapreduce.map.memory.mb',
'Hmoc',     'mapreduce.map.output.compress',
'Hmokfs',   'mapreduce.map.output.key.field.separator',
'Hmovc',    'mapreduce.map.output.value.class',
'Hmsm',     'mapreduce.map.skip.maxrecords',
'Hmspcai',  'mapreduce.map.skip.proc-count.auto-incr',
'Hmssp',    'mapreduce.map.sort.spill.percent',
'Hms',      'mapreduce.map.speculative',
'Hmr',      'mapreduce.mapper.regex',
'Hmrg',     'mapreduce.mapper.regexmapper..group',
'Hofc',     'mapreduce.output.fileoutputformat.compress',
'Hofcc',    'mapreduce.output.fileoutputformat.compress.codec',
'Hofct',    'mapreduce.output.fileoutputformat.compress.type',
'Hofo',     'mapreduce.output.fileoutputformat.outputdir',
'Holo',     'mapreduce.output.lazyoutputformat.outputformat',
'Hoskc',    'mapreduce.output.seqbinaryoutputformat.key.class',
'Hosvc',    'mapreduce.output.seqbinaryoutputformat.value.class',
'Hots',     'mapreduce.output.textoutputformat.separator',
'Hpblo',    'mapreduce.partition.binarypartitioner.left.offset',
'Hpbro',    'mapreduce.partition.binarypartitioner.right.offset',
'Hpkco',    'mapreduce.partition.keycomparator.options',
'Hpkpo',    'mapreduce.partition.keypartitioner.options',
'Hpcp',     'mapreduce.pipes.commandfile.preserve',
'Hpe',      'mapreduce.pipes.executable',
'Hpei',     'mapreduce.pipes.executable.interpretor',
'Hpif',     'mapreduce.pipes.inputformat',
'Hpijm',    'mapreduce.pipes.isjavamapper',
'Hpijrr',   'mapreduce.pipes.isjavarecordreader',
'Hpijrw',   'mapreduce.pipes.isjavarecordwriter',
'Hpijr',    'mapreduce.pipes.isjavareducer',
'Hpp',      'mapreduce.pipes.partitioner',
'Hrds',     'mapreduce.reduce.debug.script',
'Hre',      'mapreduce.reduce.env',
'Hrfm',     'mapreduce.reduce.failures.maxpercent',
'Hribp',    'mapreduce.reduce.input.buffer.percent',
'Hrjo',     'mapreduce.reduce.java.opts',
'Hrll',     'mapreduce.reduce.log.level',
'Hrmbp',    'mapreduce.reduce.markreset.buffer.percent',
'Hrm',      'mapreduce.reduce.maxattempts',
'Hrmm',     'mapreduce.reduce.memory.mb',
'Hrmt',     'mapreduce.reduce.memory.totalbytes',
'Hrmit',    'mapreduce.reduce.merge.inmem.threshold',
'Hrsct',    'mapreduce.reduce.shuffle.connect.timeout',
'Hrsibp',   'mapreduce.reduce.shuffle.input.buffer.percent',
'Hrsmp',    'mapreduce.reduce.shuffle.merge.percent',
'Hrsp',     'mapreduce.reduce.shuffle.parallelcopies',
'Hrsrt',    'mapreduce.reduce.shuffle.read.timeout',
'Hrsm',     'mapreduce.reduce.skip.maxgroups',
'Hrspcai',  'mapreduce.reduce.skip.proc-count.auto-incr',
'Hrs',      'mapreduce.reduce.speculative',
'Htai',     'mapreduce.task.attempt.id',
'Htdl',     'mapreduce.task.debugout.lines',
'Htfpft',   'mapreduce.task.files.preserve.failedtasks',
'Htfpfp',   'mapreduce.task.files.preserve.filepattern',
'Htid',     'mapreduce.task.id',
'Htisf',    'mapreduce.task.io.sort.factor',
'Htism',    'mapreduce.task.io.sort.mb',
'Htim',     'mapreduce.task.ismap',
'Htmpr',    'mapreduce.task.merge.progress.records',
'Htod',     'mapreduce.task.output.dir',
'Htpart',   'mapreduce.task.partition',
'Htprof',   'mapreduce.task.profile',
'Htpm',     'mapreduce.task.profile.maps',
'Htpp',     'mapreduce.task.profile.params',
'Htpr',     'mapreduce.task.profile.reduces',
'Htssa',    'mapreduce.task.skip.start.attempts',
'Htt',      'mapreduce.task.timeout',
'Httd',     'mapreduce.task.tmp.dir',
'Htulk',    'mapreduce.task.userlog.limit.kb',
'Httcls',   'mapreduce.tasktracker.cache.local.size',
'Httct',    'mapreduce.tasktracker.contention.tracking',
'Httdi',    'mapreduce.tasktracker.dns.interface',
'Httdn',    'mapreduce.tasktracker.dns.nameserver',
'Htteb',    'mapreduce.tasktracker.events.batchsize',
'Htthi',    'mapreduce.tasktracker.healthchecker.interval',
'Htthsa',   'mapreduce.tasktracker.healthchecker.script.args',
'Htthsp',   'mapreduce.tasktracker.healthchecker.script.path',
'Htthst',   'mapreduce.tasktracker.healthchecker.script.timeout',
'Htthn',    'mapreduce.tasktracker.host.name',
'Httha',    'mapreduce.tasktracker.http.address',
'Httht',    'mapreduce.tasktracker.http.threads',
'Httim',    'mapreduce.tasktracker.indexcache.mb',
'Htti',     'mapreduce.tasktracker.instrumentation',
'Httldmsk', 'mapreduce.tasktracker.local.dir.minspacekill',
'Httldmss', 'mapreduce.tasktracker.local.dir.minspacestart',
'Httmtm',   'mapreduce.tasktracker.map.tasks.maximum',
'Httnsr',   'mapreduce.tasktracker.net.static.resolutions',
'Httrtm',   'mapreduce.tasktracker.reduce.tasks.maximum',
'Httra',    'mapreduce.tasktracker.report.address',
'Httr',     'mapreduce.tasktracker.resourcecalculatorplugin',
'Httt',     'mapreduce.tasktracker.taskcontroller',
'Htttm',    'mapreduce.tasktracker.taskmemorymanager.monitoringinterval',
'Httts',    'mapreduce.tasktracker.tasks.sleeptimebeforesigkill',
'Hntcnm',   'net.topology.configured.node.mapping',
'Hntnsmi',  'net.topology.node.switch.mapping.impl',
'Hntsfn',   'net.topology.script.file.name',
'Hntsna',   'net.topology.script.number.args',
'Hsjcpa',   'security.job.client.protocol.acl',
'Hsjtpa',   'security.job.task.protocol.acl',
'Hfieldsep','stream.map.output.field.separator',
'Hnfields', 'stream.num.map.output.key.fields',
);

our %mr_conf_abbrevs = reverse %mr_generics;

our %compression_abbrevs = (
  "gz", "org.apache.hadoop.io.compress.GzipCodec",
  "lzo", "org.apache.hadoop.io.compress.DefaultCodec",
  "bz", "org.apache.hadoop.io.compress.BZip2Codec",
  "snappy", "org.apache.hadoop.io.compress.SnappyCodec"
);

sub sortconf($) {
  my $spec = $_[0];
  return undef unless substr($spec, 0, 1) eq "g";
  join "", map {my $col = ord($_) - 64; $_ eq "n" ? "n" : $_ eq "-" ? "r" : " -k$col,$col" }
   split //, substr $spec, 1;
}

sub partconf($) {
  my $spec = $_[0];
  return undef unless substr($spec, 0, 1) eq "f";
  "-k" . join ",", map { ord($_) - 64 } split //, substr $spec, 1;
}

sub translate_mr_conf_var($$) {
  my ($k, $v) = @_;
  if ($k eq "Hofcc") {return $compression_abbrevs{$v} || $v;}
  if ($k eq "Hpkco") {return sortconf($v) || $v;}
  if ($k eq "Hpkpo") {return partconf($v) || $v;}
  $v;
}

# These must be first in a hadoop command for... reasons.
our @priority_hadoop_opts = ("stream.num.map.output.key.fields",
                             "stream.map.output.field.separator",
                             "mapreduce.partition.keypartitioner.options",
                             "mapreduce.partition.keycomparator.options"); 
our @priority_hadoop_opt_abbrevs = map {$mr_conf_abbrevs{$_}} @priority_hadoop_opts;

sub priority_jobconf(@) {
  # Apparently some of the Hadoop options must be in order;
  # I have no idea what that order is exactly, but I follow
  # the convention laid out here:
  # https://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Comparator+Class
  # and here:
  # http://ischoolreview.com/iSR_Grav/entries/entry-2
  # Upshots: you need to use the stream.map.output.num.fields if 
  # you use comparators 
  my %input_jobconf = @_;

  my @high_priority_jobconf = ();

  my @field_based_opts = grep defined, @input_jobconf{'Hpkpo', 'Hpkco'};
  if(@field_based_opts) {
    my $max_field = max map {split /\D+/} @field_based_opts;
    push @high_priority_jobconf, 
      -D => "stream.num.map.output.key.fields=$max_field";
  }

  push @high_priority_jobconf, 
    -D => "mapreduce.job.output.key.comparator.class=" . 
          "org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedComparator"
    if grep {$_ eq 'Hpkco'} keys %input_jobconf;

  push @high_priority_jobconf, 
    map { -D => $mr_generics{$_} . "=" . $input_jobconf{$_}} 
      grep { defined($input_jobconf{$_}) } @priority_hadoop_opt_abbrevs; 

  delete @input_jobconf{@priority_hadoop_opt_abbrevs};
  \@high_priority_jobconf, \%input_jobconf;
}

sub hadoop_generic_options(@) {
  my @jobconf = @_;
  my %jobconf = map {split /=/, $_, 2} @jobconf;
    %jobconf = map {$mr_conf_abbrevs{$_}, $jobconf{$_}} keys %jobconf;

  my %raw = map {$_, dor(conf $_, $jobconf{$_})} keys %mr_generics;
  my %clean_jobconf = map {$_, $raw{$_}} grep {defined $raw{$_}} keys %raw;
  my %clean_jobconf = map {$_, translate_mr_conf_var($_, $clean_jobconf{$_})} keys %clean_jobconf;
  $clean_jobconf{'Hpkpo'} = "-k1,1" if exists($clean_jobconf{'Hpkco'}) and !exists($clean_jobconf{'Hpkpo'});
  my $needs_partitioner = grep {$_ eq 'Hpkpo'} keys %clean_jobconf; 

  my ($high_priority_jobconf_ref, $low_priority_jobconf_ref) = priority_jobconf(%clean_jobconf);
  %jobconf = %$low_priority_jobconf_ref;
  my @output_jobconf = @$high_priority_jobconf_ref;

  my @low_priority_options = map {$mr_generics{$_} . "=" . $clean_jobconf{$_}} keys %jobconf;
  my @low_priority_jobconf = map((-D => $_), @low_priority_options);

  # -partitioner is actually not a generic option
  # so it must follow the lowest priority generic option.
  push @low_priority_jobconf, 
    -partitioner => "org.apache.hadoop.mapred.lib.KeyFieldBasedPartitioner" 
    if $needs_partitioner;

  push @output_jobconf, @low_priority_jobconf;

  @output_jobconf;
}
260 core/hadoop/hadoop.pl
# Hadoop operator.
# The entry point for running various kinds of Hadoop jobs.

BEGIN {defshort '/H', defdsp 'hadoopalt', 'hadoop job dispatch table'}

defperlprefix "core/hadoop/hadoop-conf.pl";

our %mr_generics;

for (keys %mr_generics) {
  my $var_name = $mr_generics{$_};
  $var_name =~ tr/[a-z]\-./[A-Z]__/;
  my $env_var_name = 'NI_HADOOP_' . $var_name;
  defconfenv $_, $env_var_name => undef;
}

defconfenv 'hadoop/name',          NI_HADOOP               => 'hadoop';
defconfenv 'hadoop/streaming-jar', NI_HADOOP_STREAMING_JAR => undef;
defconfenv 'hadoop/jobconf', NI_HADOOP_JOBCONF => undef;
defconfenv 'hdfs/tmpdir', NI_HDFS_TMPDIR => '/tmp';

defresource 'hdfs',
  read   => q{soproc {exec conf 'hadoop/name', 'fs', '-cat', $_[1]} @_},
  write  => q{siproc {sh conf('hadoop/name') . " fs -put - " . shell_quote($_[1]) . " 1>&2"} @_},
  exists => q{local $_;
              my $fh = soproc {exec conf 'hadoop/name', 'fs', '-stat', $_[1]} @_;
              saferead $fh, $_, 8192;
              close $fh;
              !$fh->await},
  tmp    => q{"hdfs://" . conf('hdfs/tmpdir') . "/" . uri_temp_noise},
  nuke   => q{sh conf('hadoop/name') . ' fs -rm -r ' . shell_quote($_[1]) . " 1>&2"};

defresource 'hdfst',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    my $path = shell_quote $_[1];
                    sh qq{$hadoop_name fs -text $path 2>/dev/null || $hadoop_name fs -text $path"/part*" 2>/dev/null}} @_},
  nuke => q{sh conf('hadoop/name') . ' fs -rm -r ' . shell_quote($_[1]) . " 1>&2"};

defresource 'hdfsrm',
  read => q{soproc {my $o = resource_read "hdfst://$_[1]";
                    sforward $o, \*STDOUT;
                    $o->await;
                    resource_nuke "hdfst://$_[1]"} @_},
  nuke => q{resource_nuke "hdfst://$_[1]"};

# Streaming.
# We need to be able to find the Streaming jar, which is slightly nontrivial. The
# hadoop docs suggest that $HADOOP_HOME has something to do with it, but I've
# seen installations that had no such environment variable and everything worked
# fine. Here's what we can do:

# | 1. Use $NI_HADOOP_STREAMING_JAR if it's set
#   2. Use `locate hadoop-streaming*.jar` if we have `locate`
#   3. Use `find /usr /opt -name hadoop-streaming*.jar`, see if it's there

# If those don't work, then we are officially SOL and you'll have to set
# NI_HADOOP_STREAMING_JAR.

sub hadoop_streaming_jar {
  local $SIG{CHLD} = 'DEFAULT';
  conf 'hadoop/streaming-jar'
  || (split /\n/, `locate 'hadoop-streaming*.jar' \\
                   || find /usr -name 'hadoop-streaming*.jar' \\
                   || find /opt -name 'hadoop-streaming*.jar'`)[0]
  || die "ni: cannot find hadoop streaming jar "
       . "(you can fix this by setting \$NI_HADOOP_STREAMING_JAR)";
}

# Input type autodetection.
# Technically, hadoop operators take one or more HFDS input paths on stdin -- but
# of course ni isn't going to just give up if we appear to have something else.
# If we have something that obviously isn't an HDFS path, we upload that stream
# into a temporary HDFS location and run against that.

sub hdfs_input_path {
  local $_;
  my $n;
  die "ni: hdfs_input_path: no data" unless $n = saferead \*STDIN, $_, 8192;
  if (/^\w+:\/\//) {
    $n = saferead \*STDIN, $_, 8192, length while $n;
    s/hdfst:\/\//hdfs:\/\//g;
    (0, map [split /\t/], grep length, split /\n/);
  } else {
    my $hdfs_tmp    = resource_tmp 'hdfs://';
    my $hdfs_writer = resource_write $hdfs_tmp;
    safewrite $hdfs_writer, $_;
    safewrite $hdfs_writer, $_ while saferead \*STDIN, $_, 8192;
    close $hdfs_writer;
    $hdfs_writer->await;
    (1, [$hdfs_tmp]);
  }
}

sub hadoop_lambda_file($$) {
  my ($name, $lambda) = @_;
  my $tmp = resource_tmp('file://') . $name;
  my $w   = resource_write $tmp;
  conf_set monitor => 0;
  safewrite $w, ni_quoted_image 1, @$lambda;
  sforward_quoted resource_read($_), $w for quoted_resources;
  close $w;
  ($tmp, ni_quoted_exec_args);
}

sub hadoop_embedded_cmd($@) {
  "sh -c " . shell_quote("cat " . shell_quote($_[0]) . " - | " . shell_quote(@_[1..$#_]));
}

sub hadoop_cmd_setup(@) {
  my ($map, $combine, $reduce) = @_;
  my ($nuke_inputs, @ipath) = hdfs_input_path;

  my ($mapper, @map_cmd) = hadoop_lambda_file 'mapper', $map;
  my ($combiner, @combine_cmd) = $combine
    ? hadoop_lambda_file 'combiner', $combine : ();
  my ($reducer, @reduce_cmd) = $reduce
    ? hadoop_lambda_file 'reducer', $reduce : ();

  my $streaming_jar = hadoop_streaming_jar;

  $mapper, \@map_cmd, $combiner, \@combine_cmd, 
    $reducer, \@reduce_cmd, $nuke_inputs, \@ipath, $streaming_jar;
}


sub make_hadoop_cmd($$$$$$$$$) {
  my ($mapper, $map_cmd_ref, $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref, $streaming_jar, $ipaths, $opath) = @_;
  $mapper   =~ s|^file://||;
  $combiner =~ s|^file://|| if $combiner;
  $reducer  =~ s|^file://|| if $reducer;

  my @map_cmd = @$map_cmd_ref;
  my @combine_cmd = @$combine_cmd_ref;
  my @reduce_cmd = @$reduce_cmd_ref;

  (my $mapper_file   = $mapper)         =~ s|.*/||;
  (my $combiner_file = $combiner || '') =~ s|.*/||;
  (my $reducer_file  = $reducer  || '') =~ s|.*/||;

  my @jobconf =
    grep $reducer || !/reduce/,             # HACK
    grep length, split /\s+/, dor conf 'hadoop/jobconf', '';

  my $job_name = "ni @$ipaths -> $opath";
  my $n_addl_paths = $#$ipaths; 
  my $first_path = $$ipaths[0];
  $job_name = "ni $first_path and $n_addl_paths others" .
              " -> $opath" if $n_addl_paths > 0;
  push @jobconf, "mapreduce.job.name=" . $job_name;
  
  my @ordered_jobconf = hadoop_generic_options(@jobconf);
 
  my $cmd = shell_quote
    conf 'hadoop/name',
    jar => $streaming_jar,
    -files  => join(",", grep defined, ($mapper, $combiner, $reducer)),
    @ordered_jobconf,
    map((-input => $_), @$ipaths),
    -output => $opath,
    -mapper => hadoop_embedded_cmd($mapper_file, @map_cmd),
    (defined $combiner
      ? (-combiner => hadoop_embedded_cmd($combiner_file, @combine_cmd))
      : ()),
    (defined $reducer
      ? (-reducer => hadoop_embedded_cmd($reducer_file, @reduce_cmd))
      : (-reducer => 'NONE'));
    
  $cmd;
}


defoperator hadoop_streaming => q{
  my ($mapper, $map_cmd_ref, 
      $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref,
      $nuke_inputs, $ipath_ref,
      $streaming_jar) = hadoop_cmd_setup @_;

  for my $ipaths (@$ipath_ref) {
    my $opath = resource_tmp "hdfs://";
    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref,
                              $combiner, $combine_cmd_ref,
                              $reducer, $reduce_cmd_ref, 
                              $streaming_jar, $ipaths, $opath);
    die "hadoop command text too long" if length $cmd > 125_000;
    my $hadoop_fh = siproc {
     sh "$cmd 1>&2";
    };

    close $hadoop_fh;
    warn "ni: hadoop streaming failed" if $hadoop_fh->await;

    /^hdfsrm:/ && resource_nuke($_) for @$ipaths;

    (my $result_path = $opath) =~ s/^hdfs:/hdfst:/;
    print "$result_path/part-*\n";
  }

  if ($nuke_inputs) {resource_nuke $_ for map @$_, @$ipath_ref}

  resource_nuke $mapper;
  resource_nuke $combiner if defined $combiner;
  resource_nuke $reducer  if defined $reducer;
};

defoperator hadoop_make_nukeable =>
  q{print sr $_, qr/^hdfst?/, 'hdfsrm' while <STDIN>};

BEGIN {defparseralias hadoop_streaming_lambda => palt pmap(q{undef}, prc '_'),
                                                      pmap(q{[]},    prc ':'),
                                                      _qfn}

defhadoopalt S => pmap q{hadoop_streaming_op @$_},
                  pseq pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda;

defhadoopalt '#' => pmap q{hadoop_make_nukeable_op}, pnone;

defhadoopalt DS => pmap q{my ($m, $c, $r) = @$_;
                          my @cr =
                            (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
                             defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
                          [@$m, @cr]},
                   pseq pc hadoop_streaming_lambda,
                        pc hadoop_streaming_lambda,
                        pc hadoop_streaming_lambda;

defhadoopalt R =>
  pmap q{configure_op {'Hjr' => "$_"},
                      [hadoop_streaming_op [], undef, []]},
  pc number;

#Hadoop quick configuration.
#This will be useful for spinning up more customizable jobs once I
#figure out exactly how configure_op works.

defoperator hadoop_test => q{
  my ($mapper, $map_cmd_ref, 
      $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref,
      $nuke_inputs, $ipath_ref,
      $streaming_jar) = hadoop_cmd_setup @_;

  for my $ipaths (@$ipath_ref) {
    my $opath = resource_tmp "hdfs://";
    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref, 
                              $combiner, $combine_cmd_ref,
                              $reducer, $reduce_cmd_ref,
                              $streaming_jar, $ipaths, $opath);
    print "$cmd\n";
  }
};

defhadoopalt T => pmap q{hadoop_test_op @$_},
                  pseq pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda;

2 core/pyspark/lib
pyspark.pl
local.pl
54 core/pyspark/pyspark.pl
# Pyspark interop.
# We need to define a context for CLI arguments so we can convert ni pipelines
# into pyspark code. This ends up being fairly straightforward because Spark
# provides so many high-level operators.

# There are two things going on here. First, we define the codegen for Spark
# jobs; this is fairly configuration-independent since the API is stable. Second,
# we define a configuration system that lets the user specify the Spark execution
# profile. This governs everything from `spark-submit` CLI options to
# SparkContext init.

# Pyspark operators.
# These exist in their own parsing context. Rather than compiling directly to
# Python code, we generate a series of gens, each of which refers to a '%v'
# quantity that signifies the value being transformed.

sub pyspark_compile {my $v = shift; $v = $_->(v => $v) for @_; $v}
sub pyspark_create_lambda($) {$_[0]}

BEGIN {defcontext 'pyspark', q{PySpark compilation context}}
BEGIN {
  defparseralias pyspark_fn  => pmap q{pyspark_create_lambda $_}, pycode;
  defparseralias pyspark_rdd => pmap q{pyspark_compile 'input', @$_}, pyspark_qfn;
}

defshort 'pyspark/n',  pmap q{gen "%v.union(sc.parallelize(range(1, 1+$_)))"}, integer;
defshort 'pyspark/n0', pmap q{gen "%v.union(sc.parallelize(range($_)))"}, integer;

defshort 'pyspark/e',
  pmap q{TODO(); gen "%v.pipe(" . pyquote($_) . ")"}, prx '([^]]+)';

defshort 'pyspark/m', pmap q{gen "%v.map(lambda x: $_)"}, pyspark_fn;
defshort 'pyspark/r',
  defalt 'pysparkrowalt', 'alternatives for pyspark/r row operator',
    pmap(q{gen "%v.sample(False, $_)"},     integer),
    pmap(q{gen "%v.takeSample(False, $_)"}, prx '\.(\d+)'),
    pmap(q{gen "%v.filter($_)"},            pyspark_fn);

defshort 'pyspark/g', pk gen "%v.sortByKey()";
defshort 'pyspark/u', pk gen "%v.distinct()";

defshort 'pyspark/+', pmap q{gen "%v.union($_)"},     pyspark_rdd;
defshort 'pyspark/*', pmap q{gen "%v.intersect($_)"}, pyspark_rdd;

# Configuration management.
# A profile contains the code required to initialize the SparkContext and any
# other variables relevant to the process. Each is referenced by a single
# character and stored in the %spark_profiles table.

defoperator pyspark_preview => q{sio; print "$_[0]\n"};

defshort '/P',
  defdsp 'sparkprofile', 'dispatch for pyspark profiles',
    'dev/compile' => pmap q{pyspark_preview_op $_}, pyspark_rdd;
36 core/pyspark/local.pl
# Local PySpark profile.
# Provides a way to stream data into and out of a context.

use constant pyspark_text_io_gen => gen pydent q{
  from pyspark import SparkConf, SparkContext
  %prefix
  conf = SparkConf().setAppName(%name).setMaster(%master)
  sc = SparkContext(conf=conf)
  if len(%input_path) > 0:
    input = sc.textFile(%input_path)
  else:
    input = sc.parallelize([])
  output = %body
  output.saveAsTextFile(%output_path)
};

defoperator pyspark_local_text => q{
  my ($fn) = @_;
  my $inpath   = join ',', map sr("file://$_", qr/\n$/, ''), <STDIN>;
  my $outpath  = "/tmp/ni-$$-out";
  my $tempfile = "/tmp/ni-$$-temp.py";
  safewrite swfile($tempfile),
    pyspark_text_io_gen->(
      master      => pyquote 'local[*]',
      name        => pyquote "ni $inpath -> $outpath",
      input_path  => pyquote $inpath,
      output_path => pyquote "file://$outpath",
      body        => $fn);
  local $SIG{CHLD} = 'DEFAULT';
  die "ni: pyspark failed with $_" if $_ = system 'spark-submit', $tempfile;
  print "$outpath\n";
};

defsparkprofile L => pmap q{[pyspark_local_text_op($_),
                             file_read_op,
                             row_match_op '/part-']}, pyspark_rdd;
26 doc/lib
binary.md
closure.md
col.md
container.md
examples.md
extend.md
fn.md
hadoop.md
json.md
libraries.md
lisp.md
matrix.md
monitor.md
net.md
options.md
perl.md
pyspark.md
row.md
ruby.md
scale.md
script.md
sql.md
stream.md
tutorial.md
visual.md
warnings.md
83 doc/binary.md
# Binary decoding
ni's row transform operators won't work on binary data because they seek to the
nearest newline. If you want to parse binary data you should use the `b`
operator, which does block reads and provides a byte cursor.

## Generating binary data
You can't use `r()` to emit binary unless you want newlines, but you can print
bytes directly to standard output using `wp` (write-packed) or `ws` (write
string). For example, let's synthesize a one-second wav file:

```bash
$ ni n1p'wp "A4VA8VvvVVvvA4V",
         qw|RIFF 176436 WAVEfmt 16 1 2 44100 176400 4 16 data 176400|' \
     +n44100p'my $v = 32767 * sin a*440*tau/44100;
              wp "ss", $v, $v' \
  > test.wav
```

Note that normally you'd specify the endian-ness of `s` by saying `s<` instead.
However, old versions of Perl don't support endian modifiers so I have to leave
it out of the examples here.

## Perl decoder
The decoding interface consists of four functions:

- `$offset = bi`: absolute offset of the next byte
- `$bytes = pb(n)`: peek and return N bytes
- `$bytes = rb(n)`: consume and return N bytes
- `@value = rp("packstring")`: read, unpack, and consume bytes by pack pattern

For example, we can decode samples from the WAV file above. `bp` means "binary
read with perl":

```bash
$ ni test.wav bp'rp "A4VA8VvvVVvvA4V" if bi == 0;       # skip the header
                 r rp"ss"' r10
2052	2052
4097	4097
6126	6126
8130	8130
10103	10103
12036	12036
13921	13921
15752	15752
17521	17521
19222	19222
```

A faster approach is to use the `bf` operator to read fixed-length packed
records (internally it uses more efficient logic to manage the queue of
incoming bytes):

**NOTE:** The following behaves nondeterministically on Alpine for reasons I
don't understand. I assume it has something to do with libc differences, but
you should assume `bf` doesn't work until I get this sorted out (the test below
is disabled for now).

```sh
$ ni test.wav bf'ss' r-15r10
2052	2052
4097	4097
6126	6126
8130	8130
10103	10103
12036	12036
13921	13921
15752	15752
17521	17521
19222	19222
```

If we wanted to find the dominant frequencies, for example (the `,qB.01`
quantizes the magnitudes so the test case doesn't depend on float rounding):

```bash
$ ni test.wav bp'bi?r rp "ss":rb 44' fA N'x = fft.fft(x, axis=0).real' \
     Wn rp'a <= 22050' OB r5,qB.01
441	45263289.95
14941	755.22
7341	745.63
8461	667.75
12181	620.78
```
112 doc/closure.md
# Data closures
Sometimes it's useful to bundle data with ni so that it's available on a
different filesystem. Data closures do exactly this.

There are two kinds of data closures, memory-resident and disk-backed, with the
obvious implications. Both types of data closures are accessible to scripts you
write inside ni; for example:

```bash
$ ni n5                         # some data
1
2
3
4
5
$ ni ::foo[n5]                  # ...in a memory-resident closure
```

Once you have a closure, it's visible to mappers, row filters, and any other
embedded script as a constant. It also becomes accessible as its own data
stream:

```
$ ni ::foo[n5] //:foo                   # closures are streams
1
2
3
4
5
$ ni ::foo[n5] n1p'r split /\n/, foo'   # and strings inside languages
1	2	3	4	5
```

The operative feature of closures is that they travel with ni, for instance
into a docker container:

```lazytest
if ! [[ $SKIP_DOCKER ]]; then
```

```bash
$ ni ::foo[n5] Cubuntu[//:foo]
1
2
3
4
5
$ ni ::foo[n5] Cubuntu[n1p'r split /\n/, foo']
1	2	3	4	5
```

```lazytest
fi                      # $SKIP_DOCKER
```

Disk-backed closures have almost exactly the same semantics, and are
automatically deleted when ni exits:

```bash
$ rm -r /tmp/* || :
$ ni :@foo[n10] //@foo e[wc -l]         # disk-backed data closure
10
$ ls /tmp | wc -l
0
$ ni :@foo[nE5] :@bar[nE4] //@foo //@bar gr9
1
1
10
10
100
100
1000
1000
10000
```

Disk-backed closures turn into file URIs inside language environments.

```bash
$ ni :@foo[nE6] \
      n1p'open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
          print while <$fh>;()' e[wc -c]
6888896
```

They also travel with ni into tempfiles on remote systems, and ni maps the
names accordingly:

```lazytest
if ! [[ $SKIP_DOCKER ]]; then
```

```bash
$ ni :@foo[nE5] :@bar[nE4] Cubuntu[//@foo //@bar gr9]
1
1
10
10
100
100
1000
1000
10000
$ ni :@foo[nE6] Cubuntu[ \
    n1p'open my $fh, "<", foo =~ /^file:\/\/(.*)/ or die $!;
        print while <$fh>;()'] e[wc -c]
6888896
```

```lazytest
fi                      # $SKIP_DOCKER
```
238 doc/col.md
# Column operations
ni models incoming data as a tab-delimited spreadsheet and provides some
operators that allow you to manipulate the columns in a stream accordingly. The
two important ones are `f[columns...]` to rearrange columns, and `F[delimiter]`
to create new ones.

ni refers to columns using letters: `A` to `Z`, though you can also use the
form `#N` to address the Nth column, where `#0` == `A` and `#25` == `Z`. This
second form can be useful for large data files.

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
`F` has the following uses:

- `F:<char>`: split on character
- `F/regex/`: split on occurrences of regex. If present, the first capture
  group will be included before a tab is appended to a field.
- `Fm/regex/`: don't split; instead, look for matches of regex and use those as
  the field values.
- `FC`: split on commas (doesn't handle special CSV cases)
- `FV`: parse CSV "correctly", up to newlines in fields
- `FS`: split on runs of horizontal whitespace
- `FW`: split on runs of non-word characters
- `FP`: split on pipe symbols

### Examples

`/etc/passwd` split on colons:
```bash
$ ni /etc/passwd r2F::          # F: followed by :, which is the split char
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
```

The first three lines of ni's source code:
```bash
$ ni //ni r3                            # some data
#!/usr/bin/env perl
$ni::self{license} = <<'_';
ni: https://github.com/spencertipping/ni
```

The first three lines of ni's source code, split on forward slashes:
```bash
$ ni //ni r3F/\\//                      # split on forward slashes
#!	usr	bin	env perl
$ni::self{license} = <<'_';
ni: https:		github.com	spencertipping	ni
```


The first three lines of ni's source code, split on non-words:
```bash
$ ni //ni r3FW                          # split on non-words
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
```

The first three lines of ni's source code, split on whitespace:
```bash
$ ni //ni r3FS                          # split on whitespace
#!/usr/bin/env	perl
$ni::self{license}	=	<<'_';
ni:	https://github.com/spencertipping/ni
```

The first three lines of ni's source code, split on words beginning with a slash:
```bash
$ ni //ni r3Fm'/\/\w+/'                 # words beginning with a slash
/usr	/bin	/env

/github	/spencertipping	/ni
```

## Vertical operator application
A situation that comes up a lot in real-world workflows is that you want to
apply some mapper code to a specific column. For example, if we want to
uppercase the third column of a dataset, we can do it like this:

```bash
$ ni //ni r3FW p'r a, b, uc(c), FR 3'
	usr	BIN	env	perl
	ni	SELF	license	_
ni	https	GITHUB	com	spencertipping	ni
```

But that requires a lot of keystrokes. More concise is to use `v` to pipe
column C to a separate ni process:

```bash
$ ni //ni r3FW vCpuc
	usr	BIN	env	perl
	ni	SELF	license	_
ni	https	GITHUB	com	spencertipping	ni
```

## Left/right juxtaposition
ni has the `+` and `^` operators to join streams vertically, but you can also
join them horizontally, row by row. This is done using `w` and `W` (for
"with"):

```bash
$ ni //ni r3FWfB
usr
ni
https
$ ni //ni r3FWfB wn100          # right-join numbers
usr	1
ni	2
https	3
$ ni //ni r3FWfB Wn100          # left-join numbers
1	usr
2	ni
3	https
```

As shown above, the output stream is only as long as the shorter input. This is
useful in conjunction with infinite generators like `n`; for example, you can
prepend line numbers to an arbitrarily long data stream like this:

```bash
$ ni //license Wn r~3
19	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
20	SOFTWARE.
21	
$ ni nE5p'a*a' Wn r~3
99998	9999600004
99999	9999800001
100000	10000000000
```
85 doc/container.md
# Containerized pipelines
```lazytest
# These tests only get run in environments where docker is installed
# (centos 5 uses i386 libraries and doesn't support docker, for example).
if ! [[ $SKIP_DOCKER ]]; then
```

Some ni operators depend on tools you may not want to install on your machine.
If you want to use those operators anyway, though, you can run them inside a
Docker container with the tools installed. ni provides the `C` operator to
containerize a stream transformation:

```bash
$ ni nE4 Cubuntu[gr4] O
1000
100
10
1
```

The above is executed roughly like this, except that ni pipes itself into Perl
rather than invoking itself by name:

```sh
$ ni nE4 \
  | docker run --rm -i ubuntu ni gr4 \
  | ni O
```

A common use case is for something like NumPy:

```bash
$ docker build -q -t ni-test/numpy - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y python-numpy
CMD /bin/bash
EOF
$ ni n100 Cni-test/numpy[N'x = x + 1'] r4
2
3
4
5
```

## Dynamic images
The examples above are a little awkward in that (1) they require you to tag the
docker images, and (2) they require a separate command to build them in the
first place. To get around this, ni lets you define image generators and
includes predefined ones for Ubuntu and Alpine:

```bash
$ ni n100 CU+python-numpy+sbcl[N'x = x + 1' l'(1+ a)'] r4
3
4
5
6
$ ni n100 CA+py-numpy@community+sbcl@testing[N'x = x + 1' l'(1+ a)'] r4
3
4
5
6
```

## Running in an existing container
ni can run `docker exec` and do the same interop it does when it creates a new
container.

```bash
$ docker run --detach -i --name ni-test-container ubuntu >/dev/null
$ ni Eni-test-container[n100g =\>/tmp/in-container Bn] r4
1
10
100
11
$ [[ -e /tmp/in-container ]] || echo 'file not in host (good)'
file not in host (good)
$ ni Eni-test-container[/tmp/in-container] | wc -l
100
$ docker rm -f ni-test-container >/dev/null
```

```lazytest
fi                      # $SKIP_DOCKER (lazytest condition)
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
- `@[i0,4,0 i0,-4,0 FC]`: two extra data points to expand the min/max along
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
  - `i0,4,0`: append the literal text `0,4,0`
  - `i0,-4,0`: append the literal text `0,-4,0`
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
operator `wc` that counts lines by shelling out to `wc -l`:

```bash
$ mkdir my-library
$ echo my-lib.pl > my-library/lib
$ cat > my-library/my-lib.pl <<'EOF'
defoperator count_lines => q{exec 'wc', '-l'};
defshort '/wc', pmap q{count_lines_op}, pnone;
EOF
$ ni --lib my-library n100wc
100
```

Most ni extensions are about defining a new operator, which involves extending
ni's command-line grammar.
77 doc/fn.md
# Function definitions
ni lets you define aliases using the `defexpander` internal function. These
support arguments of arbitrary parse types and provide basic substitution. For
example, let's define a shorthand for fractional numbers:

```bash
$ mkdir fractional
$ echo fractional.pl > fractional/lib
$ cat >fractional/fractional.pl <<'EOF'
defexpander ['/frac', n => pc integer, step => pc number],
            'n$n', 'pa * $step';
EOF
```

Now we can load the library and use the new operator:

```bash
$ ni --lib fractional frac 10 .5
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
$ ni --lib fractional frac4.5
0.5
1
1.5
2
```

You can also define a nullary function, which is just a regular shorthand:

```bash
$ ni --run 'defexpander "/license-words", qw[//license FWpF_]' \
     license-words r10
ni
https
github
com
spencertipping
ni
Copyright
c
2016
Spencer
```

## Using Perl functions
You can use an arbitrary Perl expression instead of an expansion template:

```bash
$ mkdir fractional2
$ echo fractional2.pl > fractional2/lib
$ cat >fractional2/fractional2.pl <<'EOF'
defexpander ['/frac', n => pc integer, step => pc number],
  sub {
    my %args = @_;
    ("+n$args{n}", "pa * $args{step}");
  };
EOF
$ ni --lib fractional2 frac 10 .5
0.5
1
1.5
2
2.5
3
3.5
4
4.5
5
```
119 doc/hadoop.md
# Hadoop operator
The `H` operator runs a Hadoop job. For example, here's what it looks like to
use Hadoop Streaming (in this case, inside a `sequenceiq/hadoop-docker`
container):

```sh
$ docker run --detach -i -m 2G --name ni-test-hadoop \
    sequenceiq/hadoop-docker \
    /etc/bootstrap.sh -bash
```

```lazytest
if ! [[ $SKIP_DOCKER ]]; then
```

Let's start up a container and use `HS` to run a Streaming job. `H` is a
delegator, meaning that you always specify a profile (in this case `S` for
Streaming) before any command arguments.

(Note: The `until docker exec` silliness below is because we have to wait for
the hadoop container to boot up correctly. Sometimes the container gets into a
bad state and doesn't become available, in which case we nuke it and start
over. This is all just for unit testing; you won't have to worry about this
stuff if you're using ni to run hadoop jobs.)

```lazytest
start_time=0;
until docker exec -i ni-test-hadoop \
      /usr/local/hadoop/bin/hadoop fs -mkdir /test-dir; do
  if (( $(date +%s) - start_time > 60 )); then
    docker rm -f ni-test-hadoop >&2
    docker run --detach -i -m 2G --name ni-test-hadoop \
      sequenceiq/hadoop-docker \
      /etc/bootstrap.sh -bash >&2
    start_time=$(date +%s)
  fi
done
```

`S` takes three lambdas: the mapper, the combiner, and the reducer. There are
two special cases you can use instead of a normal bracketed lambda:

- `_`: skip this stage of the pipeline. If you use this for the combiner and
  reducer, then your job will be map-only.
- `:`: identity lambda. `HS:_:` is a simple way to repartition your input data,
  for example.

Hadoop input paths are specified using the input stream to a hadoop command. If
you specify something that doesn't start with `hdfs://` or `hdfst://`, the
stream will be uploaded to an HDFS tempfile and used as the job input. Notice
that we use `\<` after the hadoop command: `H` emits the output path, but not
the output data -- so if we want the data we need to read it.

```bash
$ NI_HADOOP=/usr/local/hadoop/bin/hadoop \
  ni n5 Eni-test-hadoop [HS[p'r a, a*a'] _ _ \<]
1	1
2	4
3	9
4	16
5	25
```

With a reducer:

```bash
$ ni n5 ^{hadoop/name=/usr/local/hadoop/bin/hadoop} \
          Eni-test-hadoop [HS[p'r a, a*a'] _ [p'r a, b+1'] \<] o
1	2
2	5
3	10
4	17
5	26
```

Now let's get a word count for `ni //license`:

```bash
$ ni //license ^{hadoop/name=/usr/local/hadoop/bin/hadoop} \
                 Eni-test-hadoop [HS[FW pF_] _ [fAcx] \<] r10
2016	1
A	1
ACTION	1
AN	1
AND	1
ANY	2
ARISING	1
AS	1
AUTHORS	1
BE	1
```

## Jobconf
You can pass in jobconf options using the `hadoop/jobconf` variable or by
setting `NI_HADOOP_JOBCONF` (note the different output; if you use multiple
reducers, you'll see the shard boundaries):

```bash
$ ni //license ^{hadoop/name=/usr/local/hadoop/bin/hadoop \
                 hadoop/jobconf='mapred.map.tasks=10
                                 mapred.reduce.tasks=4'} \
                 Eni-test-hadoop [HSFWpF_ _ fAcx \<] r10
2016	1
A	1
BE	1
BUT	1
FOR	2
INCLUDING	1
LIABILITY	1
LIABLE	1
OF	4
OR	7
```

```lazytest
docker rm -f ni-test-hadoop >&2

fi                      # $SKIP_DOCKER (lazytest condition)
```
73 doc/json.md
# JSON operators
## Background
Perl has a standard JSON library since 5.14, but it's written in pure Perl and
decodes at about 1MiB/s (vs the ~60MiB/s for the native version, but that isn't
installed by default). Older versions of Perl don't include any JSON support at
all.

To work around this, ni implements a medium-speed pure Perl JSON parser
(~5MiB/s) and some very fast alternatives for cases where you don't need to do
a full object parse.

Internally, ni uses its own JSON library for pipeline serialization and to
generate the output of `--explain`.

## Full encode/decode
### Perl driver
```bash
$ ni //license FWp'json_encode [F_]' r4
["ni","https","github","com","spencertipping","ni"]
["Copyright","c",2016,"Spencer","Tipping","MIT","license"]
[]
["Permission","is","hereby","granted","free","of","charge","to","any","person","obtaining","a","copy"]
```

`json_decode` inverts and always returns a reference:

```bash
$ ni //license FWp'json_encode [F_]' p'r @{json_decode a}' r4
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
```

### Ruby driver
**TODO**: this is necessary because older Rubies don't provide any JSON
support.

## Sparse extraction
It's uncommon to need every field in a JSON object, particularly when you're
testing specific hypotheses on rich data. This makes it likely that JSON
decoding will be pipeline bottleneck simply due to the ratio of bytes
pre-vs-post-decode. ni helps to mitigate this by providing a very fast
destructuring operator that works like `jq` (but 2-3x faster):

```bash
$ ni //license FWpF_ p'r pl 3' \
     p'json_encode {type    => 'trigram',
                    context => {w1 => a, w2 => b},
                    word    => c}' \>jsons
jsons
$ ni jsons r5
{"context":{"w1":"https","w2":"github"},"type":"trigram","word":"com"}
{"context":{"w1":"github","w2":"com"},"type":"trigram","word":"spencertipping"}
{"context":{"w1":"com","w2":"spencertipping"},"type":"trigram","word":"ni"}
{"context":{"w1":"spencertipping","w2":"ni"},"type":"trigram","word":"Copyright"}
{"context":{"w1":"ni","w2":"Copyright"},"type":"trigram","word":"c"}
```

A destructuring specification consists of a comma-delimited list of extractors:

```bash
$ ni jsons D:w1,:w2,:word r5
https	github	com
github	com	spencertipping
com	spencertipping	ni
spencertipping	ni	Copyright
ni	Copyright	c
```

### Types of extractors
The example above used the `:field` extractor, which means "find the first
occurrence of a field called `field`, and return its value."
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
113 doc/lisp.md
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

If you don't have SBCL installed locally, you can use the `C` (containerize)
operator to run a Docker image:

```lazytest
if ! [[ $SKIP_DOCKER ]]; then
```

```bash
$ docker build -q -t ni-test/sbcl - <<EOF > /dev/null
FROM ubuntu
RUN apt-get update
RUN apt-get install -y sbcl
CMD /bin/bash
EOF
$ ni Cni-test/sbcl[n4l'(+ a 2)']
3
4
5
6
```

```lazytest
fi                      # $HAVE_DOCKER
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

Note that whitespace is required after every `l'code'` operator; otherwise ni
will assume that everything following your quoted code is also Lisp.

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
252 doc/matrix.md
# Matrix operations

## Sparse and Dense Matrix Operations (`X` and `Y`)
ni provides a handful of operations that make it easy to work with sparse and dense matrices. The first two are `Y` (dense to sparse) and `X` (sparse to dense), which work like this:

```bash
$ ni //ni FWr10
	usr	bin	env	perl
	ni	self	license	_	
ni	https	github	com	spencertipping	ni
Copyright	c	2016	Spencer	Tipping	MIT	license

Permission	is	hereby	granted	free	of	charge	to	any	person	obtaining	a	copy
of	this	software	and	associated	documentation	files	the	Software	to	deal
in	the	Software	without	restriction	including	without	limitation	the	rights
to	use	copy	modify	merge	publish	distribute	sublicense	and	or	sell
copies	of	the	Software	and	to	permit	persons	to	whom	the	Software	is
```

A sparse matrix is represented as a series of `row col value` tuples:

```bash
$ ni //ni FW Yr10
0	0	
0	1	usr
0	2	bin
0	3	env
0	4	perl
1	0	
1	1	ni
1	2	self
1	3	license
1	4	_
```

`X` inverts `Y` exactly:

```bash
$ ni //ni FW fABCD Y X r10
	usr	bin	env
	ni	self	license
ni	https	github	com
Copyright	c	2016	Spencer

Permission	is	hereby	granted
of	this	software	and
in	the	Software	without
to	use	copy	modify
copies	of	the	Software
```

`X` is also additive in the event of cell collisions; this makes it useful as a
reducer:

```bash
$ ni n010p'r 0, a%3, 1' X
4	3	3
```

## 1-D Matrix Operations (`Z`)
Data in row form can be flattened (lengthened?) into a column via `pF_`.

```bash
$ ni i[a b] i[c d] pF_
a
b
c
d
```

Inverting that operation, converting a column to a row with a specified number of fields is done using `Z`, which takes the number of fields as a parameter. 

```bash
$ ni i[a b] i[c d] pF_ Z2
a	b
c	d
```

In fact, `pF_` can be replaced effectively with `Z1`
 

## NumPy interop
You can transform dense matrices with NumPy using the `N` operator. Your code is evaluated in an imperative context and side-effectfully transforms the input matrix, which is called `x`.

```bash
$ ni n10p'r map a*$_, 1..10'
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
$ ni n10p'r map a*$_, 1..10' N'x = x + 1'
2	3	4	5	6	7	8	9	10	11
3	5	7	9	11	13	15	17	19	21
4	7	10	13	16	19	22	25	28	31
5	9	13	17	21	25	29	33	37	41
6	11	16	21	26	31	36	41	46	51
7	13	19	25	31	37	43	49	55	61
8	15	22	29	36	43	50	57	64	71
9	17	25	33	41	49	57	65	73	81
10	19	28	37	46	55	64	73	82	91
11	21	31	41	51	61	71	81	91	101
```

`N` gets the full input stream unless you use a partition:

```bash
$ ni n4N'x = x.T'
1	2	3	4
```

This example raises an important issue: ni always imports NumPy arrays as 2D objects, never 1D (even if it's just a column of numbers). It will, however, promote any 1D arrays into column vectors.

```bash
$ ni n4N'x = reshape(x, (-1))'
1
2
3
4
```

## Partitioned matrices
The operations above caused the entire input stream to be read into memory, which is not very scalable. Each matrix operator allows you to specify that the first N columns represent a partition identifier: if you indicate this, then
each matrix ends when the partition fields change.

For example, suppose we've got a bunch of words and we want to partition our analysis by the first letter. We start by splitting that into its own column:

```bash
$ ni //license plc FW Z1 p'r/(.)(.*)/' g r10
2	016
a	
a	
a	bove
a	ction
a	ll
a	n
a	nd
a	nd
a	nd
```

Now we can apply matrix operators with the `B` qualifier, indicating that matrices start at column B and everything left of that is the partition ID. Let's form letter occurrence matrices by expanding into sparse form.

```bash
$ ni //license plc FWpF_ p'r split//' g r10
2	0	1	6
a
a
a	b	o	v	e
a	c	t	i	o	n
a	l	l
a	n
a	n	d
a	n	d
a	n	d
$ ni //license plc FWpF_ p'r split//' g YB r10
2	0	0	0
2	0	1	1
2	0	2	6
a	1	0	b
a	1	1	o
a	1	2	v
a	1	3	e
a	2	0	c
a	2	1	t
a	2	2	i
$ ni //license plc FWpF_ p'r split//' gYB fABD gcfBCDA r10
2	0	6	1
a			2
a	b	v	1
a	c	i	1
a	l		1
a	n		9
a	r	s	1
a	s		1
a	s	o	1
a	u	h	1
```

At this point we have partitioned sparse matrices, and each row has the form
`first-letter row-number subsequent-letter frequency`. We can convert these to
dense matrices by using `,z` to assign a number to each subsequent letter (so
that each gets a unique column index), then sorting and using `X`.

```bash
$ ni //license plc FWpF_ p'r split//' \
      gYBfABDgcfBCDA ,zC o XB r10
a		2
a			1
a				1
a		1
a		9
a					1
a		1				1
a							1
b		2
b		1
```

Now the matrix is in a form that NumPy can process. The `N` operator automatically zero-fills to the right to make sure the matrix is rectangular (as opposed to the ragged edges we have above).

```bash
$ ni //license plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'x *= 2' YB,qD.01XB r10
a	0	4	0	0	0	0	0
a	0	0	2	0	0	0	0
a	0	0	0	2	0	0	0
a	0	2	0	0	0	0	0
a	0	18	0	0	0	0	0
a	0	0	0	0	2	0	0
a	0	2	0	0	0	2	0
a	0	0	0	0	0	0	2
b	0	4
b	0	2
```

You can use multiline code with Python and ni will fix the indentation so everything works. For example:

```bash
$ ni //license plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'x *= 2
        x += 1' r10
a	1	5	1	1	1	1	1
a	1	1	3	1	1	1	1
a	1	1	1	3	1	1	1
a	1	3	1	1	1	1	1
a	1	19	1	1	1	1	1
a	1	1	1	1	3	1	1
a	1	3	1	1	1	3	1
a	1	1	1	1	1	1	3
b	1	5
b	1	3
```

It also works with blocks that require indentation:

```bash
$ ni //license plc FWpF_ p'r split//' \
     gYBfABDgcfBCDA,zCo XB \
     NB'if True:
          x = x + 1' r3
a	1	3	1	1	1	1	1
a	1	1	2	1	1	1	1
a	1	1	1	2	1	1	1
```
107 doc/monitor.md
# Monitors
If you run a pipeline that takes longer than a couple of seconds, ni will emit
monitor output to standard error. It looks like this:

```sh
$ ni nE9S8p'sin(rand())' rp'a > 0.1' ,q0.01 czn
 6106K  2085K/s  -8 905930
 4223K  1427K/s  24 0.735911990151528
 3672K  1251K/s  16 0.3174551373783
  985K   339K/s -19 0.45
 1329K   456K/s -32 ["count"]
```

Each row corresponds to the output side of one pipeline operation:

```sh
$ ni --explain nE9S8p'sin(rand())' rp'a > 0.1' ,q0.01 czn
["n",1,1000000001]
["row_fixed_scale",8,[["perl_mapper","sin(rand())"]]]
["perl_grepper","a > 0.1"]
[["quantize",[1,0],0.01]]
["count"]
["sink_null"]
```

(`sink_null` produces no output, so its monitor will never appear.)

There are four columns:

```
     +------------------------- total output data (from this operator)
     |        +---------------- output data rate
     |        |   +------------ output data "pressure" (more below)
     |        |   |      +----- data preview, or --explain for the operator
     |        |   |      |        (the display alternates between the two)
 6106K  2085K/s  -8 905930
```

## Throughput and data pressure
Data pipeline performance isn't entirely straightforward, particularly when
buffering operators are involved. Most ni operators, however, are written to
stream data quickly from input to output -- in which case the slowest operator
will govern the overall pipeline speed. For example, we can write an
artificially slow map function to reduce the throughput everywhere:

```sh
$ ni nE7 pa+1 zn                        # a few seconds
$ ni nE7 p'sleep 0.1; a + 1' zn         # quite a lot longer
```

Throughput and output data won't help you identify the pipeline bottleneck in
these situations because the pipeline is effectively moving in lock-step. This
is where the concept of data pressure comes in.

To understand how ni computes data pressure, it is helpful to envision each part
of the pipeline as having a monitor that sits in between it and the next
operator, accepting output from the former and sending it to the latter:

```
         +----------+                     +----------+
         |          |                     |          |
         | operator |     +---------+     | operator |
... ==>  |    X     | ==> | monitor | ==> |    Y     | ==> ...
         |          |     +---------+     |          |
         +----------+                     +----------+
```

The output data pressure of process X is `10 * log2(output_time / input_time)`,
where `output_time` is the amount of time the monitor spends waiting for output
from X and `input_time` is the amount of time the monitor spends waiting for
process Y to accept input.

Going back to our two examples above, if you run the second one, you'll see
monitor output like this:

```
 2815K    90K/s 115 ["n",1,10000001]
 2752K    88K/s-116 ["perl_mapper","sleep 0.1; a + 1"]
```

The output pressure from `nE7` is 115, and the output pressure of the Perl
command is -116, meaning `n` was producing data about 3000 times as fast as the
Perl process was consuming it (and the Perl process was producing data about
3000 times _slower_ than `sink_null` was willing to consume it). Intuitively
this makes sense: the Perl command is creating backpressure against `n`, and
`sink_null` is, relatively to the Perl command, creating the opposite (i.e. it
appears to have no backpressure at all, acting like a vacuum).

## Dealing with bottlenecks
The easiest way to solve a bottleneck is to [horizontally scale](scale.md) the
slow sections of the pipeline. This isn't always possible, for example if you
need to sort things, but when it is possible it should produce significant
speedups. For example:

```sh
$ ni nE7 S8p'sleep 0.1; a + 1' zn
33014K   645K/s  49 4356592
21735K   433K/s-117 ["row_fixed_scale",8,[["perl_mapper","sleep 0.1; a + 1"]]]
```

Now the Perl function appears to be only ~32x slower than `n` -- unexpectedly
fast for two reasons. First, `S` maintains large memory buffers that it fills
quickly at pipeline startup; that will distort initial readings, though it will
ultimately even out. Second, `n` produces rows whose length increases as we
read more input; so the `sleep 0.1` in the Perl command has less of an impact
the further we get into the stream. (This is an example of data-dependent
throughput, which happens a lot in practice.)
46 doc/net.md
# Network operators
## HTTP
ni uses `curl` to retrieve HTTP/HTTPS urls. For example:

```bash
$ nc -l 1400 <<'EOF' > /dev/null &      # an enterprise-grade web server
HTTP/1.1 200 OK
Content-length: 12

Hello world
EOF
$ sleep 1; ni http://localhost:1400 n3
Hello world
1
2
3
```

(The `sleep 1` before the ni command is to give the webserver's J2EE stack time
to boot up, which helps the tests work consistently.)

## SSH
You can move a part of your pipeline to another machine via SSH. ni will run
itself on that machine by sending itself over STDIN to `perl`, so it works
whether or not ni exists on the remote system, and whether or not the remote
disk is writable.

The SSH operator is `s` and looks like this:

```sh
$ ni //license shostname[gc FW p'r a, length b'] r10
```

Conceptually, here's how ni executes the above:

```
$ ni //license \
  | ssh hostname "ni gc FW p'r a, length b'" \
  | ni r10
```

You can, of course, nest SSH operators:

```sh
$ ni //license shost1[shost2[gc]] r10
```
98 doc/options.md
# Complete ni operator listing
Implementation status:
- T: implemented and automatically tested
- M: implemented and manually tested (so it might be broken)
- P: partially implemented
- I: implemented
- U: unimplemented

## Resources
Scheme     | Status | Example
-----------|--------|--------
`file://`  | M      | `file:///usr/share/dict/words`
`hdfs://`  | M      | `hdfs:///user/x/foobar`
`http://`  | T      | `http://localhost:8090`
`https://` | M      | `https://en.wikipedia.org`
`s3cmd://` | I      | `s3cmd://some/path/to/object`

## Stream operators
Operator | Status | Example      | Description
---------|--------|--------------|------------
`+`      | T      | `+p'foo'`    | Appends a data source evaluated with no input
`^`      | T      | `^file`      | Prepends a data source
`=`      | T      | `=\>f`       | Duplicate stream, ignoring fork output
`\>`     | T      | `\>file`     | Sinks stream into resource, emits resource name
`\>\'R`  | M      | `\>\'R`      | Converts a stream of resource names into a packed resource stream
`\<`     | T      | `\<`         | Opposite of `\>`
`%`      | I      | `%n100`      | Interleave lines, optionally with a bias ratio
`-`      |        |              |
`_`      |        |              |
`\!`     | I      | `\!p'F_==3'` | Assert a condition
`,`      | T      | `,jAB`       | Enter cell context
`:`      | T      | `:foo[nE8z]` | Checkpointed stream
`::`     | M      | `::x[n100]`  | Create an in-memory data closure
`//:`    | M      | `//:x`       | Append closure data
`@`      | U      | `@foo[\>@a]` | Enter named-gensym context
`\##`    | U      | `\>foo \##`  | Cat **and then obliterate** named resource(s)
`1`      | M      | `1p'"hi"'`   | `1` is an alias for `n1`
`a`      |        |              |
`b`      | T      | `bL40`       | Binary operators
`c`      | T      | `c`          | `uniq -c`, but emits proper TSV format
`d`      |        |              |
`e`      | T      | `e[tac]`     | Exec shell command
`f`      | T      | `fACB`       | Reorder, duplicate, or drop fields by column
`g`      | T      | `gA`         | Sort by all or selected columns
`h`      |        |              |
`i`      | T      | `ifoo`       | Append literal text `foo`
`j`      | U      | `j foo`      | Join sorted streams on field values
`k`      |        |              |
`l`      | T      | `l'(1+ a)'`  | Map over rows using Common Lisp
`m`      | T      | `m'a + 1'`   | Map over rows using Ruby
`n`      | T      | `n10`        | Generate or prepend line numbers
`o`      | T      | `oC`         | Numeric sort ascending
`p`      | T      | `p'a + 1'`   | Map over rows using Perl
`q`      |        |              |
`r`      | T      | `r10`        | Select rows by criterion
`s`      | M      | `sfoo[n10]`  | Evaluate a lambda on another machine using SSH
`t`      |        |              |
`u`      | T      | `u`          | Just like `uniq`
`v`      | T      | `vCplc`      | Vertically transform a range of columns
`w`      | T      | `wn100`      | "With": join a stream rightwards
`x`      | T      | `xC`         | Exchange first fields with others
`y`      |        |              |
`z`      | T      | `z4`         | Compress or decompress
`A`      |        |              |
`B`      | T      | `Bn`         | Buffer a stream
`C`      | T      | `Cubuntu[g]` | Containerize a pipeline with Docker
`D`      | PT     | `D:foo`      | Destructure structured text data (JSON/XML)
`E`      | T      | `Efoo[g]`    | Execute a pipeline in an existing Docker
`F`      | T      | `FC`         | Parse data into fields
`G`      | T      | `GA...`      | Gnuplot prefix
`H`      | T      | `HS:::`      | Send named files through hadoop
`I`      | I      | `I[...]`     | Process concatenated image files
`J`      |        |              |
`K`      |        |              |
`L`      | U      | `L'(1+ a)'`  | (Reserved for Lisp driver)
`M`      | U      | `M'svd(x)'`  | Faceted Octave matrix interop
`MM`     | I      | `MM`         | [Map-o-matic](https://github.com/spencertipping/mapomatic)
`N`      | T      | `N'svd(x)'`  | Faceted NumPy matrix interop
`O`      | T      | `OD`         | Numeric sort descending
`P`      | T      | `PLg`        | Evaluate Pyspark lambda context
`Q`      |        |              |
`R`      | U      | `R'a+1'`     | Faceted R matrix interop
`S`      | T      | `S16[rx100]` | Scale across multiple processes
`T`      |        |              |
`U`      |        |              |
`V`      | U      | `VB`         | Pivot and collect on field B
`W`      | T      | `Wn100`      | "With": join a stream leftwards
`X`      | T      | `X`          | Sparse to dense matrix conversion
`Y`      | T      | `Y`          | Dense to sparse matrix conversion
`Z`      | T      | `Z2`         | Fold/unfold stream to specified width


## Cell operators
Operator | Status | Example | Description
---------|--------|---------|------------
`h`      | T      | `,z`    | Turns each unique value into a hash.
`H`      | T      | `,HAB`  | Turns each unique value into a unique number between 0 and 1.
`z`      | T      | `,h`    | Turns each unique value into an integer.
506 doc/perl.md
# Perl interface
**NOTE:** This documentation covers ni's Perl data transformer, not the
internal libraries you use to extend ni. For the latter, see
[extend.md](extend.md) (`ni //help/extend`).

ni also grudgingly supports Ruby: [ruby.md](ruby.md). However, as far as I know
nobody uses this code and I'm likely to cannibalize the `m` operator at some
point.

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
$ ni ::plfoo[n4p'a*a'] //:plfoo
1
4
9
16
```

**NOTE:** The Perl driver won't execute your code at all if the input stream is
empty. If you're using the Perl driver to generate data, you need to feed it a
row using, e.g. `n1` (for which a shorthand is just `1`):

```bash
$ ni +p'1..5'                   # nothing happens here
$ ni +n1p'1..5'                 # the single input row causes `p` to run
1
2
3
4
5
$ ni 1p'"hello"'                # 1 == n1
hello
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
The Perl code given to `p` is invoked on each line of input, which is stored in
`$_`. `F_(...)` takes one or more column indexes (as zero-based integers) and
returns the field values. If you don't pass in anything, it returns all of the
fields for the line. For example:

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
  defined $_ && print "$_\n" for row();
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
$ ni n3p'r $_ for 1..a'                 # use r imperatively, implicit return
1
1
2
1
2
3
```

As a shorthand, any array references you return will become rows:

```bash
$ ni n3p'[a, a+1, a+2]'
1	2	3
2	3	4
3	4	5
```

### `pR`: `require` external source for perl context
Sometimes you want to have access to external functions inside your perl
mappers/greppers/etc; ni gives you the `pR` operator to do this. For example:

```bash
$ cat > functions.pm <<'EOF'
package ni::pl;       # important! this is the package where your perl code runs
sub normalize
{
  my $sum = sum @_;   # required code can call back into ni functions
  map $_ / ($sum || 1), @_;
}
EOF
$ ni pRfunctions.pm 1p'r normalize 1, 2, 5'
0.125	0.25	0.625
```

**NOTE:** If you're using `pR` and you have code that depends on that `pR` to
parse, ni may fail to parse closing brackets. You can either explicitly wrap
subroutines with `()`, or you can use the workarounds suggested by the
perl-parse error message if you run into this. It isn't possible for ni to
figure this out for you because it parses the command line before executing
meta-operators. (And it isn't possible to do this differently because the `pR`
might be inside `s` or similar.)

As usual with ni, any source you require with `pR` will be shipped over SSH and
Docker connections, bundled into Hadoop jars, and generally will be available
wherever your pipeline goes. There are some limitations, though; in particular,
ni doesn't track any files your code _itself_ requires -- so if `functions.pm`
had `require`d `foo.pm`, for instance, ni would only know to ship
`functions.pm` and not `foo.pm`.

Normally this is a feature; it makes it possible for you to `use` or `require`
Perl standard modules without propagating your local version onto
possibly-incompatible remotes (e.g. if you have v5.22 locally but the remote
runs v5.10, it's possible that your local standard libraries will break on the
remote perl).

ni can probably infer sub-inclusions, but this requires some invasive
modification to Perl's `require` mechanism through the use of hooks, and I
haven't implemented it yet. For the moment, sub-inclusions aren't really
handled; although you could `pR` each one, this won't set `%INC` keys and any
sub-`require` statements in your source is likely to fail as a result (since
the files won't exist on the remote end). This is lame and will be fixed at
some point.

`pR` takes ni lambdas, which makes it possible to load source from nonstandard
locations like HTTP:

```bash
$ nc -l 3001 <<'EOF' > /dev/null &
HTTP/1.1 200 OK
Content-Length: 54

package ni::pl;
sub this_worked { r "it worked", @_ }
EOF
$ sleep 1
$ ni pRhttp://localhost:3001 1p'this_worked 5, 6, 7'
it worked	5	6	7
```

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

`a_` etc are defined like this, with an exception for the scalar return case:

```pl
sub a_ {local $_; map((split /\t/)[0], map split(/\n/), @_)}
sub b_ {local $_; map((split /\t/)[1], map split(/\n/), @_)}
...
```

The line split enables more idiomatic handling of data closures:

```bash
$ ni ::squares[n100p'100 - a' p'r a, a*a'] \
     n5p'^{@sq{a_ squares} = b_ squares} $sq{a()}'
1
4
9
16
25
```

## Hash constructors
The above code can be condensed a bit with hash constructors, which are pairs
of columns:

```bash
$ ni ::squares[n100p'100 - a' p'r a, a*a'] n5p'^{%sq = ab_ squares} $sq{a()}'
1
4
9
16
25
```

## Utility functions
ni predefines some stuff you may find useful:

- `sum(@)`, `prod(@)`, `mean(@)`
- `first(@)`, `last(@)`
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

The Cartesian product function works, which is a considerable improvement over
its prior behavior:

```bash
$ ni n1p'cart [1,2], [1,2,3], ["a","b"]'
1	1	a
1	1	b
1	2	a
1	2	b
1	3	a
1	3	b
2	1	a
2	1	b
2	2	a
2	2	b
2	3	a
2	3	b
```

## Streaming lookahead
This is implemented in terms of reducers, and gives you the ability to reduce
arbitrarily many rows in constant space. There are two parts to this. First,
the streaming reduce functions `se` and `sr`; and second, compound reducers
(very useful, and explained in the next section).

### `sr`
Reduces the entire data stream:

```pl
@final_state = sr {reducer} @init_state
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

**NOTE**: There is _no comma_ between the reducer and the partition function.


For example, to naively get a comma-delimited list of users by login shell:

```bash
$ ni /etc/passwd F::gGp'r g, se {"$_[0]," . a} \&g, ""'
/bin/bash	,root
/bin/false	,syslog
/bin/sh	,backup,bin,daemon,games,gnats,irc,libuuid,list,lp,mail,man,news,nobody,proxy,sys,uucp,www-data
/bin/sync	,sync
```

`se` has shorthands for the first 17 columns: `sea`, `seb`, ..., `seq`; they are called as:

```pl
@final_state = sea {reducer} @init_state
```


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
than `a` in the example above: the string `'a'` is spliced into a function body
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
28 doc/pyspark.md
# PySpark interop
```lazytest
# All of these tests require Docker, so skip if we don't have it
if ! [[ $SKIP_DOCKER ]]; then
```

ni's `P` operator compiles a series of stream operators into PySpark. It takes
a context as its first argument, then a lambda of stream operations. Like other
ni commands, data is streamed into and out of the PySpark context. For example,
using the `L` (for "local") Spark profile:

```bash
$ ni Cgettyimages/spark[PL[n10] \<o]
1
2
3
4
5
6
7
8
9
10
```

```lazytest
fi              # $SKIP_DOCKER
```
352 doc/row.md
# Row operations
These are fairly well-optimized operations that operate on rows as units, which
basically means that ni can just scan for newlines and doesn't have to parse
anything else. They include:

- Take first/last N
- Take uniform-random or periodic sample
- Rows matching regex
- Rows containing specified fields
- Rows satisfying code
- Reorder rows
- Count identical rows
- Joining sorted rows

## First/last
Shorthands for UNIX `head` and `tail`.

```bash
$ ni n10r3                      # take first 3
1
2
3
$ ni n10r~3                     # take last 3
8
9
10
$ ni n10r-7                     # drop first 7
8
9
10
```

Using commands like `r10` can break pipes and cause you to get less data than
you might expect; see "pipe signals" in [warnings.md](warnings.md) (`ni
//help/warnings`) for details.

A "safe" version of `ni`'s "take first N" command exists, called `rs`. `rsN` will have the same result as `rN`, but `rs` will be much slower and should only be used when a pipe signal must be avoided, for example during a [Hadoop Streaming](hadoop.md) job.

```bash
$ ni n10 rs3
1
2
3
```

## Sampling
```bash
$ ni n10000rx4000               # take the 1st of every 4000 rows
1
4001
8001
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

**NOTE:** If you're sampling from a large dataset, you will often get some
speedup by parallelizing the row operator. For example:

```sh
$ ni /path/to/large/data rx100          # ~300MB/s
$ ni /path/to/large/data S8rx100        # ~800MB/s
```

See [scale.md](scale.md) for details about horizontal scaling.

## Row matching

There are several ways you can keep only rows matching a certain criterion.

The simplest way is to use a regex:
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

You can also row match using code in any language that ni supports. At current,
it supports Perl, Ruby, and Common Lisp.

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

## Column assertion
In real-world data pipelines it's common to have cases where you have missing
data. To remove those, you can select only rows which provide a nonempty value
for one or more columns:

```bash
$ ni n1000p'r/(.)(.*)/' r15     # until 10, the second field is empty
1	
2	
3	
4	
5	
6	
7	
8	
9	
1	0
1	1
1	2
1	3
1	4
1	5
$ ni n1000p'r/(.)(.*)/' rB r8   # rB = "rows for which field B exists"
1	0
1	1
1	2
1	3
1	4
1	5
1	6
1	7
```

```bash
$ ni n10rB | wc -l              # no field B here, so no output
0
```

```bash
$ ni n10p'r a; ""' rA | wc -l   # remove blank lines
10
```

### Set membership
You can also assert that a column's value is one of a set of things. If you
want to do this for a very large set, you should consider using [bloom
filters](bloom.md).

```bash
$ ni n100 riA[n4 p'a*2']        # intersect column A with values from n4 p'a*2'
2
4
6
8
```

An uppercase `I` does the opposite, rejecting specified values:

```bash
$ ni n10 rIAn5
6
7
8
9
10
```

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

### Grouped sort
The `gg` operator sorts key-grouped bundles of rows; this is a piecewise sort.
For example, to sort words within lengths:

```bash
$ ni i{foo,bar,bif,baz,quux,uber,bake} p'r length, a' ggAB
3	bar
3	baz
3	bif
3	foo
4	bake
4	quux
4	uber
```

The first column specifies the grouping key, and subsequent columns are
interpreted as for the `g` operator.

```bash
$ ni i{foo,bar,bif,baz,quux,uber,bake} p'r length, a' ggAB-
3	foo
3	bif
3	baz
3	bar
4	uber
4	quux
4	bake
$ ni n10p'r "a", a' ggABn- r4
a	10
a	9
a	8
a	7
```


## Counting
ni gives you the `c` operator to count runs of identical rows (just
like `uniq -c`).

```bash
$ ni //license FWpF_ > word-list
$ ni word-list cr10             # unsorted count
1	ni
1	https
1	github
1	com
1	spencertipping
1	ni
1	Copyright
1	c
1	2016
1	Spencer
$ ni word-list gcr10            # sort first to group words
1	2016
1	A
1	ACTION
1	AN
1	AND
2	ANY
1	ARISING
1	AS
1	AUTHORS
1	BE
$ ni word-list gcOr10           # by descending count
7	to
7	the
7	OR
6	THE
5	Software
4	of
4	and
4	OF
4	IN
3	SOFTWARE
```

## Joining
You can use the `j` operator to inner-join two streams on the first column
value. ni assumes both streams are sorted already, e.g. using the `g` operator.

```bash
$ ni word-list p'r a, length a' > word-lengths
$ ni word-list gj[word-lengths g] r10
2016	2016	4
A	A	1
ACTION	ACTION	6
AN	AN	2
AND	AND	3
ANY	ANY	3
ANY	ANY	3
ARISING	ARISING	7
AS	AS	2
AUTHORS	AUTHORS	7
```

As shown above, joins are strictly line-sequential: every output consumes a
line from each stream. This is unlike the UNIX `join` command, which outputs a
Cartesian product.
143 doc/ruby.md
# Ruby interface
The `m` operator (for "map") lets you use a Ruby line processor to transform
data. The Ruby and [Perl](perl.md) drivers work differently, in part to reflect
the cultural differences between the two languages. It also doesn't rely on new
Ruby features like `Enumerable::Lazy`, because it's tested to be compatible
back to Ruby 1.8.7.

```bash
$ ni n5m'ai * ai'               # square some numbers
1
4
9
16
25
```

Like `p`, `m` has lambda-end detection using Ruby syntax checking:

```bash
$ ni ::rbfoo[n4m'ai*ai'] //:rbfoo
1
4
9
16
```

## Basic stuff
`a` to `q` are one-letter functions that return the first 17 tab-delimited
values from the current line. `r(...)` is a function that takes a list of
values and returns a tab-delimited row. You can suffix a value to coerce it:

- `f`: `to_f`
- `i`: `to_i`
- `s`: `to_s`

```bash
$ ni n4m'r a, ai + 1'                   # generate two columns
1	2
2	3
3	4
4	5
$ ni n4m'r a, ai + 1' m'r ai + bi'      # ... and sum them
3
5
7
9
```

Note that whitespace is required after every `m'code'` operator; otherwise ni
will assume that everything following your quoted code is also Ruby.

### `fields`: the array of fields
The Ruby code given to `m` is invoked on each line of input, which is stored in
`$l` and is the receiver. `$l` isn't split into fields until you call `fields`,
at which point the split happens and the fields are cached for efficiency.

```bash
$ ni /etc/passwd F::r3
root	x	0	0	root	/root	/bin/bash
daemon	x	1	1	daemon	/usr/sbin	/bin/sh
bin	x	2	2	bin	/bin	/bin/sh
$ ni /etc/passwd F::r3m'r fields[0..3]'
root	x	0	0
daemon	x	1	1
bin	x	2	2
$ ni /etc/passwd F::r3m'r fields[1..3]'
x	0	0
x	1	1
x	2	2
$ ni /etc/passwd F::r3m'r fields.size'
7
7
7
```

### `r`, multiple rows, and return values
If your code returns an Enumerable, you'll get an output line for each entry.
If it returns a string, you'll have a single output line. `r()` is a function
that joins on tabs and returns the result (unlike the `r` function in the Perl
driver):

```bash
$ ni n2m'[a, ai + 100]'                 # multiple lines
1
101
2
102
$ ni n2m'r a, ai + 100'                 # multiple columns
1	101
2	102
```

ni will remove newlines from every string you give it. This makes it easier to
use backticks without any filtering.

## Buffered readahead
`m` code can read forwards in the input stream:

- `lines = rw {|l| condition}`: read lines while a condition is met
- `lines = ru {|l| condition}`: read lines until a condition is met
- `lines = re {|l| l.ai + l.bi}`: read lines while `ai + bi` is equal

```bash
$ ni n10m'r ru {|l| l.ai%4 == 0}'       # read forward until a multiple of 4
1	2	3
4	5	6	7
8	9	10
```

`ru`, etc return arrays of Line objects, and there are two ways to break them
into columns. Let's generate some columnar data:

```bash
$ ni n10m'r (1..10).map {|x| ai*x}' =\>mult-table
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
```

The first way is to access the fields on each line individually:

```bash
$ ni mult-table m'r ru {|l| l.ai%4 == 0}.map(&:g)'      # access column G
7	14	21
28	35	42	49
56	63	70
```

The other way is to invoke field accessor methods on the whole array:

```bash
$ ni mult-table m'r ru {|l| l.ai%4 == 0}.g'
7	14	21
28	35	42	49
56	63	70
```
25 doc/scale.md
# Pipeline scaling
The scaling operator `S` lets you work around bottlenecks by adding horizontal
scale to a section of your pipeline. For example, suppose we have this:

```bash
$ ni nE6 p'sin(a/100)' rp'a >= 0' =\>slow-sine-table e[wc -l]
500141
```

On a multicore machine, this will run more slowly than it should because
`p` processes data serially. We can parallelize it across four processes like
this:

```bash
$ ni nE6 S4[p'sin(a/100)' rp'a >= 0'] =\>parallel-sine-table e[wc -l]
500141
$ diff <(ni slow-sine-table o) <(ni parallel-sine-table o) | head -n10
```

`S` works by reading blocks of input and finding line boundaries. It then sends
groups of lines to the child processes as their input fds become available.

Scaling makes no guarantees about the ordering of the output rows, nor where
exactly the input will be split. In practice the input splits tend to be large
for performance reasons.
55 doc/script.md
# Scripting interface
It's common to have a shell script you'd like to integrate into a ni pipeline,
and possibly also into the ni image itself. This allows you to do something
more graceful than `ni e[sh my-script args...]`.

ni's scripting interface makes this possible. All you need to do is write a
normal library and include it into the ni image. For example, here's a
scripting extension that echoes its command-line arguments:

```bash
$ mkdir echo-script
$ echo echo.pl >> echo-script/lib
$ echo echo.sh >> echo-script/lib
$ cat > echo-script/echo.sh <<'EOF'
#!/bin/sh
echo "$@"
EOF
$ cat > echo-script/echo.pl <<'EOF'
defshort '/echo' => pmap q{script_op 'echo-script', "./echo.sh $_"},
                    shell_command;
EOF
```

Now let's use the library:

```bash
$ ni --lib echo-script echo[1 2 3]
1 2 3
```

Script libraries can also include subdirectories; for example:

```bash
$ mkdir -p echo2/bin
$ { echo echo2.pl; echo bin/echo2; } > echo2/lib
$ cat > echo2/echo2.pl <<'EOF'
defshort '/echo2' => pmap q{script_op 'echo2', "bin/echo2 $_"},
                     shell_command;
EOF
$ cat > echo2/bin/echo2 <<'EOF'
#!/bin/sh
echo "$# argument(s)"
echo "$@"
EOF
```

Usage is exactly as before. Note, however, that if the operator name collides
with a directory, as it does here, you need to use `'foo bar'` rather than
`[foo bar]` to disambiguate.

```bash
$ ni --lib echo2 echo2'foo bar'
2 argument(s)
foo bar
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
                        pseq pc filename, sql_query;
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
517 doc/stream.md
# Stream operations
## Files
ni accepts file names and opens their contents in less.

```bash
$ echo test > foo
$ ni foo
test
```

Files append themselves to the data stream:

```bash
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

## Stream Generation
In addition to reading files, ni can generate data:

### Integer Streams

The `n` operator generates a stream of integers of a given length; if the
length is not given, `ni n` will generate an infinite stream.

```bash
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
```

```sh
$ ni n                          # infinite stream of ints
1
2
3
4
5
.
.
.
```

### Pulse Stream
Many operators, for example the perl operator `p'...'` and the numpy operator
`N'...'` require an input stream. In some cases, you want to generate a dummy
stream. For this purpose you can use `n1` or its shorthand `1` to cause the
operator in question to execute.


```bash
$ ni ::word[1p'pretty'] n3 w[np'r word']
1	pretty
2	pretty
3	pretty
```

### Literal Text

The `i` operator puts whitespace-delimited literal text into the stream.

```bash
$ ni ifoo                       # literal text
foo
$ ni i[foo bar]                 # literal two-column text
foo	bar
$ ni i[ foo[] [bar] ]           # literal two-column text with brackets
foo[]	[bar]
```

The example above can be written equivalently as:

```bash
$ ni ::word[ipretty] n3 w[np'r word']
1	pretty
2	pretty
3	pretty
```

### bash commands
```bash
$ ni e'seq 4'                  # output of shell command "seq 4"
1
2
3
4
```

### Whitespace

```bash
$ ni 1p'hi' +1p'there'
hi
there
```

Note that the whitespace between `hi` and `there`. If this is missing, the `ni`
parser will interpret it as follows:


```bash
$ ni 1p'hi'1p'there'
hi1pthere
```

This is important to keep in mind for `ni` in general, where commands are often
very compact: sometimes whitespace (or a lack thereof) is needed for clarity.
See [debugging.md](debugging.md) for some of the common cases where whitespace
(or lack thereof) is important.

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

### Important note about `e`
`e'sort -r'` and `e[sort -r]` are not quite identical; the difference comes in
when you use shell metacharacters:

```bash
$ mkdir test-dir
$ touch test-dir/{a,b,c}
$ ni e'ls test-dir/*'                   # e'' sends its command through sh -c
test-dir/a
test-dir/b
test-dir/c
$ ni e[ls test-dir/*] 2>/dev/null || :  # e[] uses exec() directly; no wildcard expansion
$ ni e[ ls test-dir/* ]                 # using whitespace avoids this problem
test-dir/a
test-dir/b
test-dir/c
```

## Stream combiners
ni has four operators that combine streams:

- `+`: append a stream to this one
- `^`: prepend a stream to this one
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
$ ni n10 ^[n5 g]        # /dev/null --> n5 --> g ---append---> ni stdout


                        #                  n5 --> g --> /dev/null
                        #                 /
$ ni n10 =[n5 g]        # ni stdin --> n10 -----------> ni stdout
```

Usage examples:

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
$ ni hw =e[wc -l]               # output from 'wc -l' is gone
hello
world
```

## Data generation
In addition to files, ni can generate data in a few ways:

```bash
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
$ ni ifoo                       # literal text
foo
```

## Using a shell process
ni can stream data through a shell process using the `e` command, which is often
shorter than shelling out separately.

The following commands are equivalent to `ni n3 | sort`:

```bash
$ ni n3 e'sort'
1
2
3
$ ni n3e'sort -r'
3
2
1
$ ni n3e[sort -r]
3
2
1
```

By default, `e` acts as a filter. To keep your current stream and append the
result of the shell command, use the `+` operator.

```
$ ni n2 +e'seq 3 5'                  # append output of shell command "seq 4"
1
2
3
4
5
```

### Important note about `e`
`e'sort -r'` and `e[sort -r]` are not quite identical; the difference comes in
when you use shell metacharacters:

```bash
$ mkdir test-dir
$ touch test-dir/{a,b,c}
$ ni e'ls test-dir/*'                   # e'' sends its command through sh -c
test-dir/a
test-dir/b
test-dir/c
$ ni e[ls test-dir/*] 2>/dev/null || :  # e[] uses exec() directly; no wildcard expansion
$ ni e[ ls test-dir/* ]                 # using whitespace avoids this problem
test-dir/a
test-dir/b
test-dir/c
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

## Reading/writing multiple files
`\<` will already handle multiple files, behaving like `xargs cat`. But
sometimes you want to know which file each line came from; for that you can use
`W\<` (mnemonic: "prepend and read"; see [col.md](col.md) under
"juxtaposition" for `W`). For example:

```bash
$ { echo foo; echo bar; } > file1
$ echo bif > file2
$ ni ifile1 ifile2 \<       # regular file-read on multiple files
foo
bar
bif
$ ni ifile1 ifile2 W\<      # prepend-file read on multiple files
file1	foo
file1	bar
file2	bif
```

The inverse is `W\>`, which takes the stream produced by `W\<` and converts it
back into files. For example, we can add a `.txt` extension to each one:

```bash
$ ni ifile1 ifile2 W\< p'r a.".txt", b' W\>
file1.txt
file2.txt
$ cat file1.txt
foo
bar
$ cat file2.txt
bif
```

## Compression
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
$ ni igzip z | gzip -dc                 # gzip by default
gzip
$ ni igzip zg | gzip -dc                # explicitly specify
gzip
$ ni igzip zg9 | gzip -dc               # specify compression level
gzip
$ ni ixz zx | xz -dc
xz
$ ni ilzo zo | lzop -dc
lzo
$ ni ibzip2 zb | bzip2 -dc
bzip2
```

```sh
# this one isn't a unit test because not all test docker images have a
# straightforward LZ4 install (some are too old)
$ ni ilz4 z4 | lz4 -dc
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
this quickly by checkpointing the stream at that point:

```bash
$ ni n1000000gr4 :numbers
1
10
100
1000
```

Now this data will be reused if we rerun it:

```bash
$ ni n1000000gr4 :numbers O
1000
100
10
1
```

ni isn't rerunning the process at all; we can see this by modifying the
checkpoint file:

```bash
$ echo 'checkpointed' > numbers
$ ni n1000000gr4 :numbers O
checkpointed
```

You can write compressed data into a checkpoint. The checkpointing operator
itself will decode any compressed data you feed into it; for example:

```bash
$ ni n100000z :biglist r+5
99996
99997
99998
99999
100000
$ ni n100000z :biglist r+5
99996
99997
99998
99999
100000
```
46 doc/tutorial.md
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

## Essentials
- [stream.md](stream.md)   (`ni //help/stream`):  intro to ni grammar and data
- [row.md](row.md)         (`ni //help/row`):     row-level operators
- [col.md](col.md)         (`ni //help/col`):     column-level operators
- [perl.md](perl.md)       (`ni //help/perl`):    ni's Perl library
- [lisp.md](lisp.md)       (`ni //help/lisp`):    ni's Common Lisp library
- [ruby.md](ruby.md)       (`ni //help/ruby`):    ni's Ruby library
- [monitor.md](monitor.md) (`ni //help/monitor`): pipeline monitoring

## Stuff worth knowing about
- [net.md](net.md)             (`ni //help/net`):       HTTP/SSH/etc
- [scale.md](scale.md)         (`ni //help/scale`):     parallelizing stuff
- [closure.md](closure.md)     (`ni //help/closure`):   data closures
- [hadoop.md](hadoop.md)       (`ni //help/hadoop`);    Hadoop interop
- [visual.md](visual.md)       (`ni //help/visual`):    visualizing data
- [matrix.md](matrix.md)       (`ni //help/matrix`):    dense/sparse matrices
- [binary.md](binary.md)       (`ni //help/binary`):    decoding binary streams
- [container.md](container.md) (`ni //help/container`): Dockerizing stuff
- [json.md](json.md)           (`ni //help/json`):      working with JSON
- [warnings.md](warnings.md)   (`ni //help/warnings`):  things to look out for

## Reference
- [options.md](options.md) (`ni //help/options`): every CLI option and
  operator, each with example usage

## Extending ni
- [extend.md](extend.md)       (`ni //help/extend`):    how to write a ni
  extension
- [libraries.md](libraries.md) (`ni //help/libraries`): how to load/use a
  library
30 doc/visual.md
# Visualizing data
See also [examples.md](examples.md).

```sh
$ ni --js
http://localhost:8090
```

## Using the web interface
You can view the example in the screenshots by opening the following link on
your system while running `ni --js`:

```
http://localhost:8090/#%7B%22ni%22%3A%22n2E2p'r%20%24_%2C%20a%20for%200..199'%20p'r(a*10%20%2B%20%24_%2C%20b*10)%2C%20r(a*10%2C%20b*10%20%2B%20%24_)%20for%200..9'%20p'r%20a%2C%20sin(1%20%2B%20a%20%2F%20340)%20*%20cos(b*b%20%2F%2030000)%20%2B%20sin((a%20%2B%2050)*b%20%2F%20120000)%2C%20b'%22%2C%22vm%22%3A%5B1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%2C0%2C0%2C0%2C0%2C1%5D%2C%22d%22%3A1.4%7D
```

![web ui](http://spencertipping.com/ni-jsplot-sinewave.png)

The UI consists of three main components: the ni command editor (top), the plot
(center), and a data preview (left). The data preview is normally hidden, but
you can hover over the left side of the screen to pop it out (click it to
toggle locking):

![data preview](http://spencertipping.com/ni-jsplot-sinewave2.png)

The view angle can be panned, rotated, and zoomed:

- **mouse drag:** pan
- **shift + drag:** 3D rotate
- **ctrl + drag, alt + drag, mousewheel:** zoom
74 doc/warnings.md
# Things to look out for
![img](http://spencertipping.com/ni.png)

![img](http://spencertipping.com/ni2.png)

## `r` and pipe signals
Internally, commands like `r10` will read ten rows and then break the input
pipe, causing the writing process to receive a `SIGPIPE` and terminate.
Normally this is what you want to happen, particularly when the process would
otherwise generate data forever. But there are some situations where it causes
problems; for example:

```bash
$ ni n1000000 =\>not-a-million-things r5
1
2
3
4
5
$ echo $(( $(cat not-a-million-things 2>/dev/null | wc -l) < 1000000 ))
1
```

(The output file might not even get created, depending on when the pipe signal
arrives.)

Here's what I got the last time I tried it:

```sh
$ wc -l not-a-million-things
3100 not-a-million-things
```

This happens because the ni process implementing `=` does basically this:

```
while data = read stdin:
  write data to stdout          # connected to r5
  write data to child process   # connected to \>not-a-million-things
```

Once `r5` (which is implemented with `head -n5`) receives enough data, it
exits, closing its input stream. This causes the next write to generate a pipe
signal and kill the process.

### Workaround
`r` is designed to break pipes, so you need to prepend an operator that will
either buffer or consume the stream first. Any sorting operator will do this
automatically:

```bash
$ ni n1000000 =\>a-million-things gr5
1
10
100
1000
10000
$ wc -l < a-million-things
1000000
```

More idiomatic, though, is to convert `=\>` to a checkpoint, which is fully
buffered:

```bash
$ ni n1000000 :a-million-things-2 r5
1
2
3
4
5
$ wc -l < a-million-things-2
1000000
```
__END__
