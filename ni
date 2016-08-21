#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
$ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1
  while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_"
  for grep /\.pl$/i, @ni::keys;
close DATA;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
10 ni
#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
$ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1
  while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_"
  for grep /\.pl$/i, @ni::keys;
close DATA;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
18 ni.map
gen ni                        # "eval" the bootstrap code
resource ni                   # ... and now store so ni can regenerate it
resource ni.map
resource util.pl
resource sh.pl cli.pl ops.pl

resource ops/sort.pl
resource ops/rows.pl
resource ops/ruby.pl
resource ops/perl.pl
resource ops/hadoop.pl

resource main.pl

# Library resources
lib sh
lib pl
lib rb
5 util.pl

package ni;
sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub lib($) {join "\n", @self{map "$_[0]/$_", split /\n/, $self{"$_[0]/lib"}}}
52 sh.pl




package ni;

sub shell_quote;
sub shell_quote {join ' ', map /[^-A-Za-z_0-9\/.]/
                                 ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                                 : $_,
                           map 'ARRAY' eq ref $_ ? shell_quote @$_ : $_, @_}
sub sh {+{id => shell_quote(@_), exec => [@_]}}



use constant none => {};
sub heredoc_terminator {
  my $id = 0;
  ++$id while $_[1] =~ /^_$_[0]_$id$/m;
  "_$_[0]_$id";
}
sub compile_pipeline {
  my %file_inits;
  my @commands;
  my @heredocs;
  for my $i (0..$#_) {
    my $c = $_[$i];
    $c = {id => $c, exec => [$c]} unless ref $c;
    exists $c->{$_} or die "compile_pipeline: must specify $_" for qw/exec id/;
    my $redirections;
    if (exists $c->{stdin}) {
      my $h = heredoc_terminator $i, $c->{stdin};
      push @heredocs, "$c->{stdin}\n$h";
      $redirections = "3<&0 <<\'$h\'";
    }
    $file_inits{$_} = $c->{files}{$_} for keys %{$c->{files} || none};
    my @env = map "$_=" . shell_quote($c->{env}{$_}), keys %{$c->{env} || none};
    push @commands, join " ", @env, shell_quote(@{$c->{exec}}), $redirections;
  }
  my $mkdirs = keys %file_inits
               ? 'mkdir -p ' . shell_quote map /^(.*)\/[^\/]*$/,
                                           keys %file_inits
               : '';
  my $cats   = join "\n", map {
                 my $h = heredoc_terminator '', $file_inits{$_};
                 "cat > " . shell_quote($_) . " <<'$h'\n$file_inits{$_}\n$h"
               } keys %file_inits;
  my $libs   = lib 'sh';
  my $pipe   = join "\\\n  | ", @commands;
  my $docs   = join "\n", @heredocs;
  join "\n", $libs, $mkdirs, $cats, $pipe, $docs;
}
95 cli.pl

package ni;



use constant end_of_argv  => sub {@_           ? () : (0)};
use constant consumed_opt => sub {length $_[0] ? () : @_};
sub seqr(\@) {my ($ps) = @_;
         sub {my ($x, @xs, @ys, @ps);
              (($x, @_) = &$_(@_)) ? push @xs, $x : return () for @ps = @$ps;
              (\@xs, @_)}}
sub altr(\@) {my ($ps) = @_;
         sub {my @ps, @r; @r = &$_(@_) and return @r for @ps = @$ps; ()}}
sub seq(@) {seqr @_}
sub alt(@) {altr @_}
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
sub ptag($$) {my ($t, $p)  = @_; pmap {+{$t => $_}} $p}
sub pn($@)   {my ($n, @ps) = @_; pmap {$$_[$n]} seq @ps}

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



our %short;
our @long;
sub defshort($$) {$short{$_[0]} = $_[1]}
sub deflong($$)  {unshift @long, $_[1]}








sub ops() {sub {ops()->(@_)}}
use constant long   => altr @long;
use constant short  => chaltr %short;
use constant lambda => alt mr '_', pn 1, mrc '\[', ops, mr '\]';
use constant thing  => alt lambda, long, short;
use constant suffix => rep thing, 1;
use constant op     => pn 1, rep(consumed_opt), thing, rep(consumed_opt);
use constant ops    => rep op;
use constant cli    => pn 0, ops, end_of_argv;
use constant cli_d  => ops;
24 ops.pl

package ni;
sub psh {my (@sh) = @_; pmap {sh @sh, ref $_ ? @$_ : ($_)} pop @sh}

use constant neval   => pmap {eval} mr '^=([^]]+)';
use constant integer => alt pmap(sub {10 ** $_},  mr '^E(-?\d+)'),
                            pmap(sub {1 << $_},   mr '^B(\d+)'),
                            pmap(sub {0 + "0$_"}, mr '^x[0-9a-fA-F]+'),
                            pmap(sub {0 + $_},    mr '\d+');
use constant float   => pmap {0 + $_} mr '^-?\d*(?:\.\d+)?(?:[eE][-+]?\d+)?';
use constant number  => alt neval, integer, float;
use constant colspec  => mr '^[-A-Z0-9.]+';
use constant colspec1 => mr '^[A-Z0-9]';
use constant vmapspec => alt ptag('hash', mr '^%'),
                             ptag('cval', mr '^+'),
                             ptag('row',  mr '^r?');
use constant idspec   => seq maybe vmapspec, maybe colspec;
use constant valspec  => alt mrc '^=(.*)', mr '^.';
deflong 'file',  psh 'append', 'decode',         pif {-e} mrc '^[^]]*';
deflong 'dir',   psh 'append', 'directory_list', pif {-d} mrc '^[^]]*';
deflong 'shell', psh 'append', 'sh', '-c',       mrc '^\$:([^]]+)$';
deflong 'n',     psh qw/append seq/,   pn 1, mr '^n:',  alt neval, integer;
deflong 'n0',    psh qw/append seq 0/, pn 1, mr '^n0:', alt neval, integer;
deflong 'id',    psh qw/append echo/,        mr '^id:([^]]*)';
10 ops/sort.pl

package ni;
sub sort_colspec {"-t", "\t", map {("-k", $_ + 1)}
                              map /[A-Z]/ ? ord $_ - ord 'A' : $_,
                                  split //, $_[0]}
use constant sort_args => pmap {[sort_colspec $_]} maybe colspec;
defshort 'g', psh qw/sort/,     sort_args;
defshort 'G', psh qw/sort -u/,  sort_args;
defshort 'o', psh qw/sort -n/,  sort_args;
defshort 'O', psh qw/sort -nr/, sort_args;
10 ops/rows.pl

package ni;
use constant rowspec => alt psh('tail', '-n', pn 1, mr '^\+', number),
                            psh('tail', '-n', pmap {'+' . ($_ + 1)}
                                              pn 1, mr '^-',  number),
                            psh('every',      pn 1, mr '^x',  number),
                            psh('match',      pn 1, mr '^/',  regex),
                            psh('sample',     mr '^\.\d+'),
                            psh('head', '-n', alt neval, integer);
defshort 'r', rowspec;
5 ops/ruby.pl

package ni;
defshort 'm', pmap {+{id    => "ruby map $_",
                      exec  => ['ruby', '-e', 'Kernel.eval $stdin.read', $_],
                      stdin => lib 'rb'}} rbcode;
5 ops/perl.pl

package ni;
defshort 'p', pmap {+{id    => "perl map $_",
                      exec  => ['perl', '-e', 'eval join "", <STDIN>', $_],
                      stdin => lib 'pl'}} plcode;
15 ops/hadoop.pl

package ni;
use constant hadoop_lambda => alt suffix,
                                  mr '_',
                                  pn 1, maybe(consumed_opt), lambda;
use constant hadoop_m   => pmap {[map     => $_]}     hadoop_lambda;
use constant hadoop_mr  => pmap {[map     => $$_[0],
                                  reduce  => $$_[1]]} rep hadoop_lambda, 2;
use constant hadoop_mcr => pmap {[map     => $$_[0],
                                  combine => $$_[1],
                                  reduce  => $$_[2]]} rep hadoop_lambda, 3;
use constant hadoop_lambdas => alt hadoop_mcr, hadoop_mr, hadoop_m;
deflong 'hdfs', psh 'echo', mrc 'hdfs:[^]]+';
defshort 'h', psh 'local_hadoop', hadoop_lambdas;
defshort 'H', psh 'real_hadoop',  hadoop_lambdas;
36 main.pl

package ni;
use constant exit_success      => 0;
use constant exit_run_error    => 1;
use constant exit_nop          => 2;
use constant exit_sigchld_fail => 3;
sub je($);
sub real_pipeline {compile_pipeline @_, -t STDOUT ? ('pager') : ()}
sub usage() {print STDERR $self{'doc/usage'}; exit_nop}
sub explain {print STDERR je [@_], "\n";      exit_nop}
sub compile {print STDERR real_pipeline @_;   exit_nop}
sub parse {
  my ($parsed) = cli->(@_);
  return @$parsed if ref $parsed && @$parsed;
  my (undef, @rest) = cli_d->(@_);
  die "failed to parse " . join ' ', @rest;
}
sub shell {
  open SH, '| sh'   or
  open SH, '| dash' or
  open SH, '| bash' or die "ni: could not open any POSIX sh: $!";
  syswrite SH, $_[0]
    or die "ni: could not write compiled pipeline to shell process: $!";
  unless (-t STDIN) {
    syswrite SH, $_ while sysread STDIN, $_, 32768;
  }
  close STDIN;
  close SH;
  0;
}
sub main {
  return usage if !@_ || $_[0] eq '-h' || $_[0] eq '--help';
  return explain parse @_[1..$#_] if $_[0] eq '--explain';
  return compile parse @_[1..$#_] if $_[0] eq '--compile';
  return shell real_pipeline parse @_;
}
5 sh/lib
stream.sh
compressed.sh
directory.sh
rows.sh
pager.sh
5 sh/stream.sh



append()  { cat; "$@"; }
prepend() { "$@"; cat; }
31 sh/compressed.sh





decode() {
  perl -e '
    sysread STDIN, $_, 8192;
    my $decoder = /^\x1f\x8b/             ? "gzip -dc"
                : /^BZh\0/                ? "bzip2 -dc"
                : /^\x89\x4c\x5a\x4f/     ? "lzop -dc"
                : /^\x04\x22\x4d\x18/     ? "lz4 -dc"
                : /^\xfd\x37\x7a\x58\x5a/ ? "xz -dc" : undef;
    if (defined $decoder) {
      open FH, "| $decoder" or die "decode: failed to open $decoder";
      syswrite FH, $_;
      syswrite FH, $_ while sysread STDIN, $_, 8192;
      close STDIN;
      close FH;
      exit 0;
    }
    my $archiver = /^\x50\x4b\x03\x04/              ? "zip"
                 : /^[\s\S]{257}ustar(\x0000|  \0)/ ? "tar" : undef;
    if (defined $archiver) {
      ...;
      exit 0;
    }
    syswrite STDOUT, $_;
    syswrite STDOUT, $_ while sysread STDIN, $_, 8192;
  ' < "$1"
}
5 sh/directory.sh

directory_list() {
  ls "$1" | perl -ne 'BEGIN {($prefix = shift @ARGV) =~ s/\/+$//}
                      print "$prefix/$_"' "$1"
}
6 sh/rows.sh

every()  { perl -ne 'print unless $. % '"$1"; }
match()  { perl -ne 'print if /'"$1"/; }

sample() { perl -ne 'BEGIN {srand(42)}
                     if ($. >= 0) {print; $. -= -log(1 - rand()) / '"$1"'}'; }
2 sh/pager.sh

pager() { less || more || cat; }
2 pl/lib
util.pm
math.pm
46 pl/util.pm

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
23 pl/math.pm

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
0 rb/lib
