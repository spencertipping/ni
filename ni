#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
7 ni
#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
14 ni.map


unquote ni
resource ni
resource ni.map
resource util.pl
resource self.pl
resource cli.pl
resource sh.pl
resource main.pl
lib core/gen
lib core/stream
lib core/meta
lib core/pl
7 util.pl

package ni;
sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub rf  {open my $fh, "< $_[0]"; my $r = join '', <$fh>; close $fh; $r}
sub rl  {open my $fh, "< $_[0]"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}
27 self.pl

package ni;
sub map_u {@self{@_}}
sub map_r {map sprintf("%d %s\n%s", scalar(split /\n/, "$self{$_} "), $_, $self{$_}), @_}
sub map_l {map {my $l = $_;
                map_r "$_/lib", map "$l/$_", split /\n/, $self{"$_/lib"}} @_}
sub read_map {join "\n", (map {my ($c, @a) = split /\s+/;
                                 $c eq 'unquote'     ? map_u @a
                               : $c eq 'resource'    ? map_r @a
                               : $c =~ /^lib$|^ext$/ ? map_l @a
                               : die "ni: unknown map command+args: $c @a"}
                          grep {s/#.*//g; length}
                          map split(/\n/), @self{@_}), "__END__"}
sub intern_lib($) {$self{"$_[0]/$_"} = rfc "$_[0]/$_"
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
sub psh {my (@sh) = @_; pmap {sh @sh, ref $_ ? @$_ : ($_)} pop @sh}
sub heredoc_for {my $n = 0; ++$n while $_[0] =~ /^_$n$/m; "_$n"}
sub sh_prefix() {join "\n", @self{@sh_libs}}
sub pipeline {
  my %ps;
  my @cs;
  my @hs;
  for (@_) {
    my $c = quote @{$$_{exec}};
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
50 main.pl

package ni;
use constant exit_success      => 0;
use constant exit_run_error    => 1;
use constant exit_nop          => 2;
use constant exit_sigchld_fail => 3;
our %option_handlers;
our @pipeline_prefix = sh 'true';
our @pipeline_suffix = ();
sub parse_ops {
  return () unless @_;
  my ($parsed) = cli->(@_);
  return @$parsed if ref $parsed && @$parsed;
  my (undef, @rest) = cli_d->(@_);
  die "failed to parse " . join ' ', @rest;
}
sub sh_code {pipeline @pipeline_prefix, parse_ops(@_), @pipeline_suffix}
sub run_sh {
  open SH, '| sh'   or
  open SH, '| ash'  or
  open SH, '| dash' or
  open SH, '| bash' or die "ni: could not run any POSIX sh: $!";
  syswrite SH, $_[0]
    or die "ni: could not write compiled pipeline to shell process: $!";
  unless (-t STDIN) {
    syswrite SH, $_ while sysread STDIN, $_, 8192;
  }
  close STDIN;
  close SH;
  0;
}

$option_handlers{'internal/eval'}
  = sub {eval "package ni; $_[0]"; die $@ if $@; exit_success};
$option_handlers{'internal/lib'}
  = sub {intern_lib $_[0]; $self{'ni.map'} .= "\nlib $_[0]";
         modify_self 'ni.map'};

$option_handlers{usage} = sub {print $help_topics{usage}, "\n"; exit_nop};
$option_handlers{help}
  = sub {print $help_topics{@_ ? $_[0] : 'ni'}, "\n"; exit_nop};
$option_handlers{explain} = sub {TODO()};
$option_handlers{compile} = sub {print sh_code @_; exit_nop};
sub main {
  my ($command, @args) = @ARGV;
  $command = '--help' if $command eq '-h';
  my $h = $command =~ s/^--// && $option_handlers{$command};
  return &$h(@args) if $h;
  run_sh sh_code @ARGV;
}
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
19 core/stream/decode.pm




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
19 core/stream/stream.pl

package ni;
use constant stream_sh => {stream_sh => $self{'core/stream/stream.sh'}};
use constant perl_fn   => gen '%name() { perl -e %code "$@"; }';
sub perl_fn_dep($$) {+{$_[0] => perl_fn->(name => $_[0],
                                          code => quote $self{$_[1]})}}
sub ni_cat($) {sh ['ni_cat', $_[0]], prefix => perl_fn_dep('ni_cat', 'core/stream/cat.pm')}
sub ni_decode {sh ['ni_decode'],     prefix => perl_fn_dep('ni_decode', 'core/stream/decode.pm')}
sub ni_pager {sh ['ni_pager'], prefix => stream_sh}
sub ni_pipe {@_ == 1 ? $_[0] : sh ['ni_pipe', $_[0], ni_pipe(@_[1..$#_])],
                                  prefix => stream_sh}
sub ni_append  {sh ['ni_append', @_], prefix => stream_sh}
sub ni_verb($) {sh ['ni_append_hd', 'cat'], stdin => $_[0],
                                            prefix => stream_sh}
@pipeline_prefix = ni_decode;
@pipeline_suffix = ni_pager;
deflong 'stream/sh', pmap {ni_append qw/sh -c/, $_} mrc '^(?:sh|\$):(.*)';
deflong 'stream/fs', pmap {ni_append 'eval', ni_pipe ni_cat $_, ni_decode}
                          alt mrc '^file:(.+)', pif {-e} mrc '^[^]]+';
10 core/stream/stream.sh



ni_append()  { cat; "$@"; }
ni_prepend() { "$@"; cat; }
ni_append_hd()  { cat <&3; "$@"; }
ni_prepend_hd() { "$@"; cat <&3; }
ni_pipe() { eval "$1" | eval "$2"; }

ni_pager() { ${NI_PAGER:-less} || more || cat; }
1 core/meta/lib
meta.pl
5 core/meta/meta.pl

package ni;
deflong 'meta/self', pmap {ni_verb read_map 'ni.map'} mr '^//ni';
deflong 'meta/keys', pmap {ni_verb join "\n", @keys}  mr '^//ni/';
deflong 'meta/get',  pmap {ni_verb $self{$_}}         mr '^//ni/([^]]+)';
2 core/pl/lib
util.pm
math.pm
46 core/pl/util.pm

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
__END__