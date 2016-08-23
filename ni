#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
close DATA;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
8 ni
#!/usr/bin/env perl
# https://github.com/spencertipping/ni; MIT license
use 5.006_000;
chomp($ni::self{push(@ni::keys, $2) && $2} = join '', map $_ = <DATA>, 1..$1) while <DATA> =~ /^(\d+)\s+(.*)$/;
close DATA;
push(@ni::evals, $_), eval $ni::self{$_}, $@ && die "$@ evaluating $_" for grep /\.pl$/i, @ni::keys;
eval {exit ni::main(@ARGV)}; $@ =~ s/\(eval (\d+)\)/$ni::evals[$1-1]/g; die $@;
__DATA__
11 ni.map


unquote ni
resource ni
resource ni.map
resource util.pl
resource self.pl
resource cli.pl
resource main.pl
lib core/stream
lib core/gen
10 util.pl

package ni;
sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub shell_quote {join ' ', map /[^-A-Za-z_0-9\/:@.]/
                                 ? "'" . sgr($_, qr/'/, "'\\''") . "'"
                                 : $_,
                           map 'ARRAY' eq ref $_ ? shell_quote(@$_) : $_, @_}
sub rf {open my $fh, "< $_[0]"; my $r = join '', <$fh>; close $fh; $r}
sub rl {open my $fh, "< $_[0]"; my @r =          <$fh>; close $fh; @r}
26 self.pl

package ni;
sub map_u {@self{@_}}
sub map_r {map sprintf("%d %s\n%s", scalar(split /\n/, $self{$_}), $_, $self{$_}), @_}
sub map_l {map {my $l = $_;
                map_r "$_/lib", map "$l/$_", split /\n/, $self{"$_/lib"}} @_}
sub read_map {join "\n", map {my ($c, @a) = split /\s+/;
                                $c eq 'unquote'     ? map_u @a
                              : $c eq 'resource'    ? map_r @a
                              : $c =~ /^lib$|^ext$/ ? map_l @a
                              : die "ni: unknown map command+args: $c @a"}
                         grep {s/#.*//g; length}
                         map split(/\n/), @self{@_}}
sub intern_lib($) {$self{"$_[0]/$_"} = rf "$_[0]/$_"
                   for split /\n/, ($self{"$_[0]/lib"} = rf "$_[0]/lib")}
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
46 main.pl

package ni;
use constant exit_success      => 0;
use constant exit_run_error    => 1;
use constant exit_nop          => 2;
use constant exit_sigchld_fail => 3;
our %option_handlers;
sub parse_ops {
  my ($parsed) = cli->(@_);
  return @$parsed if ref $parsed && @$parsed;
  my (undef, @rest) = cli_d->(@_);
  die "failed to parse " . join ' ', @rest;
}
sub run_sh {
  open SH, '| sh'   or
  open SH, '| ash'  or
  open SH, '| dash' or
  open SH, '| bash' or die "ni: could not run any POSIX sh: $!";
  syswrite SH, $_[0]
    or die "ni: could not write compiled pipeline to shell process: $!";
  unless (-t STDIN) {
    syswrite SH, $_ while sysread STDIN, $_, 32768;
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
$option_handlers{compile} = sub {print pipeline(parse_ops @_), "\n"; exit_nop};
sub main {
  my ($command, @args) = @ARGV;
  $command = '--help' if $command eq '-h';
  my $h = $command =~ s/^--// && $option_handlers{$command};
  return &$h(@args) if $h;
  run_sh pipeline(parse_ops @ARGV);
}
1 core/stream/lib
stream.sh
0 core/stream/stream.sh

2 core/gen/lib
gen.pl
sh.pl

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

22 core/gen/sh.pl




package ni;
sub sh {ref $_[0] ? {exec => $_[0], @_[1..$#_]} : {exec => [@_]}}
sub heredoc_for {my $n = 0; ++$n while $_[0] =~ /^_$n$/m; "_$n"}
sub pipeline {
  my @cs;
  my @hs;
  for (@_) {
    my $c = shell_quote @{$$_{exec}};
    if (exists $$_{stdin}) {
      my $h = heredoc_for $$_{stdin};
      push @cs, "$c 3<&0 <<'$h'";
      push @hs, "$$_{stdin}\n$h";
    } else {
      push @cs, $c;
    }
  }
  join("\\\n\t| ", @cs) . "\n" . join("\n", @hs);
}
