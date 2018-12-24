# Utility functions.
# Generally useful stuff, some of which makes up for the old versions of Perl we
# need to support.

sub weval($) {my @r = eval "package ni;$_[0]"; print STDERR $@ if $@; @r}

sub sgr($$$) {(my $x = $_[0]) =~ s/$_[1]/$_[2]/g; $x}
sub sr($$$)  {(my $x = $_[0]) =~ s/$_[1]/$_[2]/;  $x}
sub swap($$) {@_[0, 1] = @_[1, 0]}
sub dor($$)  {defined $_[0] ? $_[0] : $_[1]}

sub sum {local $_; my $x = 0; $x += $_ for @_; $x}

sub srfile($) { open my $fh, "< $_[0]" or die "srfile $_[0]: $!"; $fh }
sub swfile($) { open my $fh, "> $_[0]" or die "swfile $_[0]: $!"; $fh }

sub rf  {my $fh = srfile shift; my $r = join '', <$fh>; close $fh; $r}
sub rl  {my $fh = srfile shift; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub dirbase($)  {my @xs = $_[0] =~ /^(.*)\/+([^\/]+)\/*$/; @xs ? @xs : ('', $_[0])}
sub basename($) {(dirbase $_[0])[1]}
sub dirname($)  {(dirbase $_[0])[0]}

sub mkdir_p {-d $_[0] or !length $_[0] or mkdir_p(dirname $_[0]) && mkdir $_[0]}

sub wf {
  my $f = shift;
  mkdir_p dirname $f;
  my $fh = swfile $f;
  print $fh @_;
  close $fh;
  $f;
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
