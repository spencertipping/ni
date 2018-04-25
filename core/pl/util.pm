# Utility library functions.

sub ceval {eval $_[0]; die "error evaluating $_[0]: $@" if $@}

sub clip {
  local $_;
  my ($lower, $upper, @xs) = @_;
  wantarray ? map min($upper, max $lower, $_), @xs
            : min $upper, max $lower, $xs[0];
}

sub within {
  local $_;
  my ($lower, $upper, @xs) = @_;
  not grep $_ < $lower || $_ > $upper, @xs;
}

# Binary repacking
# It's common to pack(), then unpack() immediately; for instance, to parse WKB
# we'd pack hex and then unpack doubles. This transcoding can be more concisely
# represented using two functions, pu and up, each of which uses a colon to
# split the templates.
#
# For example, up("H*:Q*", 1, 2, 3) would give you a bunch of hex digits to
# encode the quadwords 1, 2, and 3.

sub pu { my ($p, $u) = split /:/, shift; pack $p, unpack $u, @_ }
sub up { my ($u, $p) = split /:/, shift; unpack $u, pack $p, @_ }

