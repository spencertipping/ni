# Image compositing and processing
# Operators that loop over concatenated PNG or JPG images within a stream. This
# is useful for compositing workflows in a streaming context, e.g. between a
# gnuplot loop and ffmpeg.

# Image traversal functions
# simage() will read a single image from stdin, copying it to stdout, then
# return. This allows you to perform a distinct action for each of a series of
# images concatenated into a stream.
#
# OK .... so this is a lot more complicated than it should be, for all kinds of
# fun reasons. Here's what's up.
#
# For PNG it's simple: there are multiple sections, each length-prefixed
# because its designers were sober, well-adjusted individuals. This includes
# the zero-length IEND marker. So we can do small reads to get the length+type,
# then do custom-sized reads to skip sections.
#
# Contrarily, JFIF/JPEG exemplifies the ambiguity between malice and
# incompetence. JFIF sections are tagged with lengths, but the image data
# itself is a binary format with the equivalent of backslash-escapes around the
# ff byte. The idea is that because each ff in the image is replaced with ff00,
# you can safely identify the image size by looking for the ffd9 EOI marker.
# But that's a bit of a wrench for this use case because it means we need to
# push bytes back into the data stream.
#
# So ... what do we do? We have `simage` maintain a "leftovers" buffer so we
# can still do big-ish block reads and keep track of unconsumed data. (TODO)

sub simage_png {
  my ($into) = @_;
  safewrite $into, $_;
  return undef unless saferead_exactly \*STDIN, $_, 6;
  safewrite $into, $_;

  my ($l, $t) = (0, '');
  while ($t ne 'IEND') {
    ($l, $t) = unpack 'Na4' if saferead_exactly \*STDIN, $_, 8;
    saferead_exactly \*STDIN, $_, $l + 4, 8;
    safewrite $into, $_;
  }
}

sub simage_jfif {
  # TODO: rewrite this.
  # FFDA is beginning-of-stream, but this data is entropy coded and has no
  # length prefix. We need to look for FFD9 as a string-search, then capture
  # leftovers for the next iteration. Probably worth some kind of binary stream
  # abstraction.
  my ($into) = @_;
  safewrite $into, $_;

  my ($t, $l) = ('', 0);
  while (1) {
    saferead_exactly \*STDIN, $t, 2;
    safewrite $into, $t;
    return 0 if $t eq "\xff\xd9";
    die sprintf("ni simage: invalid JFIF chunk prefix: %s", unpack "H4", $t)
      unless $t =~ /^\xff/;

    $l = unpack 'n' if saferead_exactly \*STDIN, $_, 2;
    printf STDERR "jfif chunk: %s [%d]\n", unpack("H4", $t), $l;
    saferead_exactly \*STDIN, $_, $l - 2, 2;
    safewrite $into, $_;
  }
}

sub simage_into {
  # Reads exactly one image from stdin, outputting to the specified ni lambda.
  # Returns the lambda's exit code on success, sets $! and returns undef on
  # failure. Dies if you're working with an unsupported type of image.
  local $_;
  my ($lambda) = @_;
  my $into = siproc {exec_ni @$lambda};
  return undef unless saferead_exactly \*STDIN, $_, 2;
  return simage_png $into  if /^\x89P$/;
  return simage_jfif $into if /^\xff\xd8$/;
  die "ni simage: unsupported image type (unrecognized magic in $_)";
}

# Image splitting operators
# The simplest of these executes a pipeline separately for each of a series of
# images.

defoperator each_image => q{1 while defined simage_into $_};

defshort '/I' => pmap q{each_image_op $_}, _qfn;
