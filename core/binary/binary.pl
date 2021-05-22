# Binary import operator.
# An operator that reads data in terms of bytes rather than lines. This is done
# in a Perl context with functions that manage a queue of data in `$_`.
#
# For Python, we just make the entirety of stdin available. This is useful when
# you're doing NumPy stuff that addresses TO BE CONTINUED! (this is an inside
# joke from a derp moment during code review, for anyone who finds it later)
#
# PREVIOUSLY, IN BINARY.PL... addresses... but addresses what??? Large blocks
# of memory at a time, of course, e.g. outputs from
# ffmpeg -f image2pipe -c:v ppm, perhaps then piped through a PPM header
# trimmer like
# bp'rb length("P6\n$width $height\n225\n"); ws rb $width*$height*3'

use constant binary_perlgen => gen q{
  %prefix
  close STDIN;
  open STDIN, '<&=3';
  while (available) {
    %body
  }
};

use constant binary_pythongen => gen pydent q{
  import os
  import sys
  sys.stdin.close()
  stdin = os.fdopen(3, 'r')
  %prefix
  def go():
  %body
  while len(stdin.buffer.peek(1)):
    try:
      go()
    except EOFError:
      sys.exit(0)
};

defperlprefix 'core/binary/bytewriter.pm';
defperlprefix 'core/binary/search.pm';

our @binary_perl_prefix_keys = qw| core/binary/bytestream.pm
                                   core/binary/formats.pm |;

our @binary_python_prefix_keys = ();

sub binary_perl_prefix() {join "\n", perl_prefix,
                                     @ni::self{@binary_perl_prefix_keys}}

sub binary_python_prefix() {join "\n", python_prefix,
                                 @ni::self{@binary_python_prefix_keys}}

sub defbinaryperlprefix($)   {push @binary_perl_prefix_keys,   $_[0]}
sub defbinarypythonprefix($) {push @binary_python_prefix_keys, $_[0]}

sub binary_perl_mapper($) {binary_perlgen->(prefix => binary_perl_prefix,
                                            body   => perl_expand_begin $_[0])}

sub binary_python_mapper($) {binary_pythongen->(prefix => binary_python_prefix,
                                                body   => indent(pydent $_[0], 2))}

defoperator binary_perl   => q{stdin_to_perl   binary_perl_mapper   $_[0]};
defoperator binary_python => q{stdin_to_python binary_python_mapper $_[0]};

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

defoperator binary_invert_fixed => q{
  use bytes;
  my ($pack_template) = @_;
  while (<STDIN>)
  {
    chomp;
    print pack($pack_template, split /\t/);
  }
};

defshort '/bf',  pmap q{binary_fixed_op $_},        generic_code;
defshort '/bf^', pmap q{binary_invert_fixed_op $_}, generic_code;

defshort '/bp', pmap q{binary_perl_op $_},   plcode \&binary_perl_mapper;
defshort '/by', pmap q{binary_python_op $_}, pycode \&binary_python_mapper;
