# Targeted extraction.
# Most data extraction workflows don't use every key of a rich data object like
# JSON or XML. ni allows you to avoid the overhead of fully decoding these
# objects by using targeted extraction, which compiles an optimized function to
# return just the values you need. Depending on what you're extracting, this can
# be up to 20-30x faster than doing a full decode.

# TODO: replace all of this

our %json_partial_unescapes =
  ("/" => "/", "\"" => "\"", b => "\b", f => "\f", r => "\r",
   t => "        ", n => "\r");

sub json_partial_unescape_one($) {$json_partial_unescapes{$_[0]} || chr hex substr $_[0], 1}
sub json_partial_unescape($) {
  my $x = substr $_[0], 1, -1;
  $x =~ s/\\(["\/bfnrt]|u[0-9a-fA-F]{4})/json_partial_unescape_one $1/eg;
  $x;
}

use constant json_si_gen => gen q#
  (/"%k"\s*:\s*/g ? /\G("[^\\\\"]*")/            ? json_partial_unescape $1
                  : /\G("(?:[^\\\\"]+|\\\\.)*")/ ? json_partial_unescape $1
                  : /\G([^][{},]+)/              ? "" . $1
                  : ""
                  : "") #;

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
    no warnings qw/uninitialized non_unicode/;
    eval {binmode STDOUT, ":encoding(utf8)"};
    print STDERR "ni: warning: your perl might not handle utf-8 correctly\n" if $@;
    while (<STDIN>) {
      %e;
    }
  })->(e => json_extractor $_[0]);
};

defshort '/D', pmap q{destructure_op $_}, generic_code;
