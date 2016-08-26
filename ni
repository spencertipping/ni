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
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
26 ni
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
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
23 ni.map


unquote ni
resource ni
resource ni.map
resource util.pl
resource self.pl
resource cli.pl
resource sh.pl
resource main.pl
lib core/common
lib core/gen
lib core/stream
lib core/meta
lib core/col
lib core/row
lib core/pl
lib core/python
lib core/sql
lib core/java
lib core/hadoop
lib core/pyspark
lib doc
12 util.pl

package ni;
sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}
sub rf  {open my $fh, "< $_[0]"; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}
sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}
30 self.pl

package ni;
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
sub intern_lib($) {$self{"$_[0]/$_"} = rfc "$_[0]/$_",
                   $_ =~ /\.pl$/ && eval $self{"$_[0]/$_"}
                   for grep length,
                       split /\n/, ($self{"$_[0]/lib"} = rfc "$_[0]/lib")}
sub modify_self($) {
  die "ni: not a modifiable instance: $0" unless -w $0;
  open my $fh, "> $0" or die "ni: failed to open self: $!";
  print $fh read_map $_[0];
  close $fh;
}
sub extend_self($) {
  $self{'ni.map'} .= "\next $_[0]";
  intern_lib $_[0];
  modify_self 'ni.map';
}
sub image {read_map 'ni.map'}
95 cli.pl

package ni;



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

sub mr($) {my $r = qr/$_[0]/;
      sub {my ($x, @xs) = @_; $x =~ s/^($r)// ? ($2 || $1, $x, @xs) : ()}}
sub mrc($) {pn 0, mr $_[0], maybe consumed_opt}



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

use constant regex => pmap {s/\/$//; $_} mr '^(?:[^\\/]|\\.)*/';

use constant rbcode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs, $x, $qcode) = @_;
  ($qcode = $code) =~ s/'/'\\''/g;
  $x .= ']' while $_ = system("ruby -ce '$qcode' >/dev/null 2>&1")
                  and ($qcode =~ s/\]$//, $code =~ s/\]$//);
  $_ ? () : length $x ? ($code, $x, @xs) : ($code, @xs)};

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
68 sh.pl




package ni;
sub quote {join ' ', map /[^-A-Za-z_0-9\/:@.]/
                           ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                           : $_,
                     map 'ARRAY' eq ref $_ ? quote(@$_) : $_, @_}





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
sub pipeline {
  my %ps;
  my @cs;
  my @hs;
  for (@_) {
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
69 main.pl

package ni;
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

$option_handlers{'internal/eval'}
  = sub {eval "package ni; $_[0]"; die $@ if $@; exit_success};
$option_handlers{'internal/lib'}
  = sub {intern_lib $_[0]; $self{'ni.map'} .= "\nlib $_[0]";
         modify_self 'ni.map'};

$option_handlers{lib} = sub {intern_lib shift; goto \&main};
$option_handlers{extend}
  = sub {intern_lib $_[0]; $self{'ni.map'} .= "\next $_[0]";
         modify_self 'ni.map'};

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
common.pl
11 core/common/common.pl

package ni;
use constant neval   => pmap {eval} mr '^=([^]]+)';
use constant integer => alt pmap(sub {10 ** $_},  mr '^E(-?\d+)'),
                            pmap(sub {1 << $_},   mr '^B(\d+)'),
                            pmap(sub {0 + "0$_"}, mr '^x[0-9a-fA-F]+'),
                            pmap(sub {0 + $_},    mr '\d+');
use constant float   => pmap {0 + $_} mr '^-?\d*(?:\.\d+)?(?:[eE][-+]?\d+)?';
use constant number  => alt neval, integer, float;
use constant colspec1 => mr '^[A-Z0-9]';
use constant colspec  => mr '^[-A-Z0-9.]+';
1 core/gen/lib
gen.pl
15 core/gen/gen.pl


package ni;
our %gen_cache;
our $gensym_index = 0;
sub gensym {join '_', '_gensym', ++$gensym_index, @_}
sub gen {
  my @pieces = split /(%\w+)/, $_[0];
  sub {
    my %vars = @_;
    my @r = @pieces;
    $r[$_] = $vars{substr $pieces[$_], 1} for grep $_ & 1, 0..$#pieces;
    join '', @r;
  };
}
4 core/stream/lib
cat.pm
decode.pm
stream.pl
stream.sh
13 core/stream/cat.pm

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
24 core/stream/decode.pm




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
28 core/stream/stream.pl

package ni;
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
@pipeline_prefix = -t STDIN  ? ()       : ni_decode 3;
@pipeline_suffix = -t STDOUT ? ni_pager : ();
deflong 'root', 'stream/sh', pmap {ni_append qw/sh -c/, $_}
                             mrc '^(?:sh|\$):(.*)';
deflong 'root', 'stream/fs',
  pmap {ni_append 'eval', ni_pipe ni_cat $_, ni_decode}
  alt mrc '^file:(.+)', pif {-e} mrc '^[^]]+';
deflong 'root', 'stream/n',  pmap {ni_append 'seq',    $_}     pn 1, mr '^n:',  number;
deflong 'root', 'stream/n0', pmap {ni_append 'seq', 0, $_ - 1} pn 1, mr '^n0:', number;
deflong 'root', 'stream/id', pmap {ni_append 'echo', $_} mrc '^id:(.*)';
deflong 'root', 'stream/pipe', pmap {sh 'sh', '-c', $_} mrc '^\$=(.*)';
10 core/stream/stream.sh



ni_append()  { cat; "$@"; }
ni_prepend() { "$@"; cat; }
ni_append_hd()  { cat <&3; "$@"; }
ni_prepend_hd() { "$@"; cat <&3; }
ni_pipe() { eval "$1" | eval "$2"; }

ni_pager() { ${NI_PAGER:-less} || more || cat; }
1 core/meta/lib
meta.pl
13 core/meta/meta.pl

package ni;
deflong 'root', 'meta/self', pmap {ni_verb image}            mr '^//ni';
deflong 'root', 'meta/keys', pmap {ni_verb join "\n", @keys} mr '^//ni/';
deflong 'root', 'meta/get',  pmap {ni_verb $self{$_}}        mr '^//ni/([^]]+)';
sub ni {sh ['ni_self', @_], prefix => perl_stdin_fn_dep 'ni_self', image}
deflong 'root', 'meta/ni', pmap {ni @$_} pn 1, mr '^@', context 'root/lambda';

deflong 'root', 'meta/help',
  pmap {$_ = 'tutorial' unless length;
        die "ni: unknown help topic: $_" unless exists $self{"doc/$_.md"};
        ni_verb $self{"doc/$_.md"}}
  pn 1, mr '^//help/?', mrc '^.*';
1 core/col/lib
col.pl
32 core/col/col.pl

package ni;


sub col_cut(@) {
  sh 'cut', '-f', join ',', TODO();
}
sub ni_cols(@) {
  my $ind = grep /[^A-I.]/, @_;
  my $asc = join('', @_) eq join('', sort @_);
  # TODO: optimize using col_cut
  my @cols    = map /^\.$/ ? -1 : ord($_) - 65, @_;
  my $cut_gen = gen q{chomp; @_ = split /\t/; print join("\t", @_[%is]), "\n"};
  my $floor   = (sort {$b <=> $a} @cols)[0] + 1;
  sh ['perl', '-ne',
      $cut_gen->(is => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cols)];
}
our @col_alt = (pmap {ni_cols split //, $_} colspec);
defshort 'root', 'f', altr @col_alt;



sub ni_split_chr($)   {sh 'perl', '-npe', "y/$_[0]/\\t/"}
sub ni_split_regex($) {sh 'perl', '-npe', "s/$_[0]/\\t/g"}
our %split_chalt = (
  'C' => (pmap {ni_split_chr   ','}          none),
  'P' => (pmap {ni_split_chr   '|'}          none),
  'S' => (pmap {ni_split_regex qr/\h+/}      none),
  'W' => (pmap {ni_split_regex qr/[^\w\n]+/} none),
  '/' => (pmap {ni_split_regex $_}           regex),
);
defshort 'root', 'F', chaltr %split_chalt;
2 core/row/lib
row.pl
row.sh
25 core/row/row.pl

package ni;
use constant row_pre => {row_sh => $self{'core/row/row.sh'}};
our @row_alt = (
  (pmap {sh 'tail', '-n', $_}                      pn 1, mr '^\+', number),
  (pmap {sh 'tail', '-n', '+' . ($_ + 1)}          pn 1, mr '^-',  number),
  (pmap {sh ['ni_revery',  $_], prefix => row_pre} pn 1, mr '^x',  number),
  (pmap {sh ['ni_rmatch',  $_], prefix => row_pre} pn 1, mr '^/',  regex),
  (pmap {sh ['ni_rsample', $_], prefix => row_pre} mr '^\.\d+'),
  (pmap {sh 'head', '-n', $_}                      alt neval, integer));
defshort 'root', 'r', altr @row_alt;




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
11 core/row/row.sh

ni_revery()  { perl -ne 'print unless $. % '"$1"; }
ni_rmatch()  { perl -ne 'print if /'"$1"/; }

ni_rsample() { perl -ne '
  BEGIN {srand($ENV{NI_SEED} || 42)}
  if ($. >= 0) {print; $. -= -log(1 - rand()) / '"$1"'}'; }

ni_sort() {
  # TODO: --compress-program etc
  sort "$@"; }
4 core/pl/lib
pl.pl
util.pm
math.pm
stream.pm
28 core/pl/pl.pl

package ni;
use constant perl_mapgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3' or die "ni: failed to open fd 3: $!";
  sub row {
    %body
  }
  while (<STDIN>) {
    @F = ();
    %each
  }
};
sub perl_prefix() {join "\n", @self{qw| core/pl/util.pm
                                        core/pl/math.pm
                                        core/pl/stream.pm |}}
sub perl_mapper($) {sh [qw/perl -/],
  stdin => perl_mapgen->(prefix => perl_prefix,
                         body   => $_[0],
                         each   => 'pr for row')}
sub perl_grepper($) {sh [qw/perl -/],
  stdin => perl_mapgen->(prefix => perl_prefix,
                         body   => $_[0],
                         each   => 'pr if row')}
our @perl_alt = (pmap {perl_mapper $_} plcode);
defshort 'root', 'p', altr @perl_alt;
unshift @row_alt, pmap {perl_grepper $_} pn 1, mr '^p', plcode;
48 core/pl/util.pm

sub sr($$$)  {(my $x = $_[2]) =~ s/$_[0]/$_[1]/;  $x}
sub sgr($$$) {(my $x = $_[2]) =~ s/$_[0]/$_[1]/g; $x}
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
sub uniq  {local $_; my %seen, @xs; $seen{$_}++ or push @xs, $_ for @_; @xs}
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
23 core/pl/math.pm

use Math::Trig;
use constant tau => 2 * pi;
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
sub haversine {
  local $_;
  my ($th1, $ph1, $th2, $ph2) = map drad $_, @_;
  my ($dt, $dp) = ($th2 - $th1, $ph2 - $ph1);
  my $a = sin($dp / 2)**2 + cos($p1) * cos($p2) * sin($dt / 2)**2;
  2 * atan2(sqrt($a), sqrt(1 - $a));
}
10 core/pl/stream.pm




our @F;
sub F(@)    {chomp, @F = split /\t/ unless @F; @_ ? @F[@_] : @F}
sub r(@)    {(my $l = join "\t", @_) =~ s/\n//g; print "$l\n"; ()}
sub pr(;$)  {(my $l = @_ ? $_[0] : $_) =~ s/\n//g; print "$l\n"; ()}
sub grab($) {$_ .= <STDIN> until /$_[0]/}
BEGIN {eval sprintf "sub %s() {F %d}", $_, ord($_) - 97 for 'a'..'q'}
1 core/python/lib
python.pl
29 core/python/python.pl

package ni;




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

use constant pycode => sub {
  return @_ unless $_[0] =~ /\]$/;
  my ($code, @xs) = @_;
  (my $tcode = $code) =~ s/"([^"\\]+|\\.)*"|'([^'\\]+|\\.)*'//g;
  my $balance = sr($tcode, qr/[^[]/, '') - sr($tcode, qr/[^]]/, '');
  $balance ? (pydent substr($code, 0, $balance), substr($code, $balance))
           : (pydent $code, @xs)};
1 core/sql/lib
sql.pl
40 core/sql/sql.pl

package ni;
defcontext 'sql';
sub sqlgen($) {bless {from => $_[0]}, 'ni::sqlgen'}
sub ni::sqlgen::render {
  local $_;
  my ($self) = @_;
  my $select = ni::dor $$self{select}, '*';
  my @others;
  for (qw/from where order_by group_by limit union intersect except
          inner_join left_join right_join full_join/) {
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
  my ($self, $k, $v) = @_;
  defined \&{"ni::sqlgen::modify_$k"}
    ? $$self{$k} = &{"ni::sqlgen::modify_$k"}($$self{$k}, $v)
    : $self = ni::sqlgen "($self->render)" if exists $$self{$k};
  $$self{$k} = $v;
  $self;
}
sub ni::sqlgen::map        {${$_[0]}->modify('select',     $_[1])}
sub ni::sqlgen::filter     {${$_[0]}->modify('where',      $_[1])}
sub ni::sqlgen::ijoin      {${$_[0]}->modify('inner_join', $_[1])}
sub ni::sqlgen::ljoin      {${$_[0]}->modify('left_join',  $_[1])}
sub ni::sqlgen::rjoin      {${$_[0]}->modify('right_join', $_[1])}
sub ni::sqlgen::uniq       {${$_[0]}{uniq} = 1; $_[0]}
sub ni::sqlgen::union      {${$_[0]}->modify('union',      $_[1])}
sub ni::sqlgen::intersect  {${$_[0]}->modify('intersect',  $_[1])}
sub ni::sqlgen::difference {${$_[0]}->modify('except',     $_[1])}
sub ni::sqlgen::take       {${$_[0]}->modify('limit',      $_[1])}
sub ni::sqlgen::sample     {${$_[0]}->modify('where',      "random() < $_[1]")}
1 core/java/lib
java.pl
3 core/java/java.pl

package ni;
defcontext 'java/cf';
1 core/hadoop/lib
hadoop.pl
1 core/hadoop/hadoop.pl

1 core/pyspark/lib
pyspark.pl
33 core/pyspark/pyspark.pl


package ni;

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

our %spark_profiles = (
  L => k gen pydent q{from pyspark import SparkContext
                      sc = SparkContext("local", "%name")
                      %body});
sub ni_pyspark {sh ['echo', 'TODO: pyspark', @_]}
defshort 'root', 'P', pmap {ni_pyspark @$_}
                      seq chaltr(%spark_profiles), $pyspark_rdd;
2 doc/lib
tutorial.md
stream.md
10 doc/tutorial.md
# ni tutorial
You can access this tutorial by running `ni //help`, or `ni //help/tutorial`.

ni parses its command arguments to build and run a shell pipeline. Help topics
include:

- `//help/stream`: an explanation of the basic way ni handles CLI options and
  data
- `//help/row`: row-level operators
- `//help/col`: column-level operators
89 doc/stream.md
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
$ ni n:3 g                      # g = sort
1
2
3
$ ni n:3g                       # no need for whitespace
1
2
3
$ ni n:3gAr                     # reverse-sort by first field
3
2
1
$ ni n:3O                       # more typical reverse numeric sort
3
2
1
```

See `ni --tutorial row` for details about row-reordering operators like
sorting.
__END__
