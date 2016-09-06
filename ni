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
  ni::set(substr($_[0], 0, -5), ni::unsdoc $_[1]) if $_[0] =~ /\.sdoc$/;
  ni::eval $_[1], $_[0] if $_[0] =~ /\.pl$/ }

push(@ni::keys, $2), ni::set "$2$3", join '', map $_ = <DATA>, 1..$1
while <DATA> =~ /^\s*(\d+)\s+(.*?)(\.sdoc)?$/;
ni::eval 'exit main @ARGV', 'main';
_
die $@ if $@;
__DATA__
15 ni.map.sdoc
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
resource parse.pl.sdoc
resource pipeline.pl.sdoc
33 util.pl.sdoc
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

{
package ni::fn;
use overload qw/ &{} apply ${} source /;
sub new {
  my ($class, $code) = @_;
  my $compiled = eval "sub {$code\n}";
  die "ni: error compiling $code: $@" if $@;
  bless {f => eval "sub {$code\n}", code => $code}, $class;
}

sub apply($@) {${$_[0]}{f}->(@_[1..$#_])}
sub source($) {\${$_[0]}{code}}
}

sub fn($) {ni::fn->new($_[0])}
94 parse.pl.sdoc
Parser combinators.
List-structured combinators. These work like normal parser combinators, but are
indirected through data structures to make them much easier to inspect. This
allows ni to build an operator mapping table.

our %parsers;
sub defparser($$&) {
  my ($name, $proto, $f) = @_;
  $parsers{$name} = $f;
  eval "sub p$name($proto) {['$name', \@_]}";
}

sub parse($@) {$parsers{${$_[0]}[0]}->(@_)}

Base parsers.
Stuff for dealing with some base cases.

c
BEGIN {
defparser 'end',   '',  sub {@_           ? () : (0)};
defparser 'empty', '',  sub {length $_[1] ? () : @_};
defparser 'k',     '$', sub {(${$_[0]}[1], @_)};
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
  sub {my ($self, $x, @xs, $k, @xs, %ls) = @_;
       my (undef, $ps) = @$self;
       ++$ls{length $_} for keys %$ps;
       for my $l (sort {$b <=> $a} keys %ls) {
         return (@ys = parse $ps{$c}, substr($x, $l), @xs) ? @ys : ()
         if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
       }
       ()};
}

sub palt(@) {my @ps = @_; paltr(@ps)}
sub pdsp(%) {my %ps = @_; pdspr(%ps)}

c
BEGIN {
defparser 'seq', '@',
  sub {my ($self, @is, $x, @xs, @ys, @ps) = @_;
       (($x, @is) = parse $_, @is) ? push @xs, $x : return ()
         for @ps = @{$$self[1]};
       (\@xs, @_)};

defparser 'rep', '$;$',
  sub {my ($self, @is, @c, @r) = @_;
       my (undef, $p, $n) = (@$self, 0);
       push @r, $_ while ($_, @_) = parse $p, (@c = @_);
       @r >= $n ? (\@r, @c) : ()};

defparser 'opt', '$',
  sub {my ($self, @is) = @_;
       my @xs = &{$$self[1]}(@is); @xs ? @xs : (undef, @is)};

defparser 'map', '&$',
  sub {my ($self, @is) = @_;
       my (undef, $f, $p) = @$self;
       my @xs = &$p(@is); @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()};

defparser 'cond', '&$',
  sub {my ($self, @is) = @_;
       my (undef, $f, $p) = @$self;
       my @xs = &$p(@is); @xs && &$f($_ = $xs[0]) ? @xs : ()};
}

sub pn($@) {my ($n, @ps) = @_;
            pmap(fn "\$\$_[$n]", pseq(@ps))}

Regex parsing.
Consumes the match, returning either the matched text or the first match group
you specify. Always matches from the beginning of a string.

c
BEGIN {
defparser 'prx', '$',
  sub {my ($self, $x, @xs) = @_;
       $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()};
}

sub prc($) {pn(0, prx(qr/$_[0]/), popt(pempty()))}
50 pipeline.pl.sdoc
Pipeline construction.
A way to build a shell pipeline in-process by consing a transformation onto
this process's standard input. This will cause a fork to happen, and the forked
PID is returned.

use POSIX qw/dup2/;

sub cdup2 {dup2 @_ or die "ni: dup2(@_) failed: $!"}
sub cfork {my $pid = fork; die "ni: fork failed: $!" unless defined $pid; $pid}
sub cpipe {pipe @_ or die "ni: pipe failed: $!"}

sub move_fd($$) {
  my ($old, $new) = @_;
  return if $old == $new;
  close $new;
  cdup2 $old, $new
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
