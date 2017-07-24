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
