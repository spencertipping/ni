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
#line 1 "binary perl code context"
    %body
  }
};

use constant binary_pythongen => gen pydent q{
  import os
  import sys
  sys.stdin.close()
  sys.stdin = os.fdopen(3, 'r')
  %prefix
  class runner:
    def go(self):
  %body
  each = runner()
  while len(sys.stdin.buffer.peek(1)):
    try:
      each.go()
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
                                                body   => indent(pydent $_[0], 4))}

defoperator binary_perl   => q{stdin_to_perl   binary_perl_mapper   $_[0]};
defoperator binary_python => q{stdin_to_python binary_python_mapper $_[0]};

defconfenv 'binary/fixed-buffer', NI_BINARY_FIXED_BUFFER => 65536;

defoperator binary_fixed => q{
  use bytes;
  my $fixed_buffer = conf('binary/fixed-buffer');

  my ($pack_template) = @_;
  my $pack_count = scalar(() = unpack $pack_template, "\0" x $fixed_buffer);
  my $pack_length = length pack $pack_template,
                           unpack $pack_template, "\0" x $fixed_buffer;
  die "ni: binary_fixed template consumes no data" unless $pack_length;

  my $bufsize = $pack_length;
  $bufsize <<= 1 until $bufsize >= $fixed_buffer;
  my $buf = '';

  # There are two strategies we can take to unpacking fixed-width binary data.
  # One involves handing unpack() a big string and saying "unpack all of this;
  # I'll sort it out later"; the other is to make a bunch of calls to unpack()
  # and substr() so we don't build a large array.
  #
  # It seems like the first option should be faster, but it often isn't. I
  # suspect it depends on the data and the pack pattern though. Because it's
  # unknown and possibly data-dependent, we measure each until it's seen 8MB of
  # data and then commit to the faster one.

  my $decision = 0;
  my $next_branch = 0;
  my ($t1, $n1) = (0, 0);
  my ($t2, $n2) = (0, 0);

  my @xs;

  while (1)
  {
    read STDIN, $buf, $bufsize - length($buf), length($buf) or return
      until length($buf) >= $pack_length;

    unless ($decision)
    {
      # Important: randomize so we don't mis-measure due to some type of
      # periodicity in output impedance (it's unlikely, but you never know).
      $next_branch = rand() < 0.5;
      $decision = $t1 / $n1 < $t2 / $n2 ? 1 : 2
        if $n1 >= 8 << 20 and $n2 >= 8 << 20;
    }

    if ($decision == 1 or !$decision && $next_branch == 0)
    {
      my $bl = length $buf;
      unless ($decision) { $t1 -= time; $n1 += $bl }
      my $n = int($bl / $pack_length);
      my $r = $bl % $pack_length;

      @xs  = unpack "($pack_template)$n a$r", $buf;
      $buf = pop @xs;

      if ($pack_count == 1) { print join("\n", @xs), "\n" }
      else
      { print join("\t", @xs[$_*$pack_count .. ($_+1)*$pack_count-1]), "\n"
          for 0..$n-1 }

      unless ($decision) { $n1 -= length $buf; $t1 += time }
    }
    else
    {
      unless ($decision) { $t2 -= time; $n2 += length $buf }
      my $o = 0;
      for (; $o + $pack_length <= length($buf); $o += $pack_length)
      {
        print join("\t", unpack $pack_template, substr $buf, $o, $pack_length), "\n";
      }
      $buf = substr $buf, $o;
      unless ($decision) { $n2 -= length $buf; $t2 += time }
    }
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
