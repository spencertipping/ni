# Binary import operator.
# An operator that reads data in terms of bytes rather than lines. This is done
# in a Perl context with functions that manage a queue of data in `$_`.

use constant binary_perlgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3';
  while (available) {
    %body
  }
};

defperlprefix 'core/binary/bytewriter.pm';

our @binary_perl_prefix_keys = qw| core/binary/bytestream.pm |;

sub binary_perl_prefix() {join "\n", perl_prefix,
                                     @ni::self{@binary_perl_prefix_keys}}

sub defbinaryperlprefix($) {push @binary_perl_prefix_keys, $_[0]}

sub binary_perl_mapper($) {binary_perlgen->(prefix => binary_perl_prefix,
                                            body   => perl_expand_begin $_[0])}

defoperator binary_perl => q{stdin_to_perl binary_perl_mapper $_[0]};

defoperator binary_fixed => q{
  use bytes;
  my ($pack_template) = @_;
  my $length = length pack $pack_template, unpack $pack_template, "\0" x 65536;
  die "ni: binary_fixed template consumes no data" unless $length;
  my $bufsize = $length;
  $bufsize <<= 1 until $bufsize >= 65536;
  my $buf = '';
  while (1)
  {
    read STDIN, $buf, $bufsize - length($buf), length($buf) or return
      until length($buf) >= $length;
    my $o = 0;
    for (; $o + $length <= length($buf); $o += $length)
    {
      print join("\t", unpack $pack_template, substr $buf, $o, $length), "\n";
    }
    $buf = substr $buf, $o;
  }
};

defshort '/b',
  defdsp 'binaryalt', 'dispatch table for the /b binary operator',
    f => pmap(q{binary_fixed_op $_}, generic_code),
    p => pmap q{binary_perl_op $_}, plcode \&binary_perl_mapper;
