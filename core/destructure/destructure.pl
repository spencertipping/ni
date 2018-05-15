# Targeted extraction.
# Most data extraction workflows don't use every key of a rich data object like
# JSON or XML. ni allows you to avoid the overhead of fully decoding these
# objects by using targeted extraction, which compiles an optimized function to
# return just the values you need. Depending on what you're extracting, this can
# be up to 20-30x faster than doing a full decode.



# TODO: replace all of this

use constant json_si_gen => gen q#
  (/"%k"\s*:\s*/g ? /\G("[^\\\\"]*")/            ? json_unescape $1
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
