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
                            core/pl/hash_util.pm
                            core/pl/math.pm
                            core/pl/stream.pm
                            core/pl/geohash.pm
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

BEGIN {
  defparseralias perl_mapper_code         => plcode \&perl_mapper;
  defparseralias perl_grepper_code        => plcode \&perl_grepper;
  defparseralias perl_asserter_code       => plcode \&perl_asserter;
  defparseralias perl_cell_transform_code => plcode \&perl_mapper;      # [sic]
}

defshort '/p',
  defalt 'perlalt', 'alternatives for /p perl operator',
    pmap q{perl_mapper_op $_}, perl_mapper_code;

defrowalt pmap q{perl_grepper_op $_},
          pn 1, pstr 'p', perl_grepper_code;

defassertdsp 'p', pmap q{perl_assert_op $_}, perl_asserter_code;

defshort 'cell/p', pmap q{perl_cell_transformer_op @$_},
                   pseq colspec, perl_cell_transform_code;
