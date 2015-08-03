#!/usr/bin/env perl
use 5.000_000;
use strict;
use warnings;

use Benchmark qw/:all/;

# Unmodified binary format, little-endian here but endian-detected in the
# decoder.
our $record_le = join '', map chr(hex),
  qw/ 4E 49 21 00 08 00 00 00 04 00 00 00
      20 00 00 00
      28 00 00 00
      31 00 00 00
      3c 00 00 00 00 00 00 00
      05 00 00 00 00 00 00 00
      ff ff ff ff ff ff ff ff
      00 00 00 00 00 00 f0 3f
      03 00 00 00 66 6f 6f 00 /;

our $text_record = join "\t", 5, -1, 1.0, "foo";
our (undef, $g_l8, @g_fs) = unpack '(LLL/L)<', $record_le;

die "oops" unless length($record_le) eq 64;

our @field_unpacks = qw[
  Q<
  d
  L</a
  LLL/L
];

my $results = timethese 4000000, {
  hunpack_detect_endian => sub {
    my (undef, $ei) = unpack 'SS', $record_le;
    if ($ei == 0x21) {
      # Little-endian, which we know
      my ($l8, @fs) = unpack '(LL/L)<', substr($record_le, 4);
      return $l8 + $fs[0];
    } else {
      # Big-endian; do nothing
    }
  },

  hunpack_assume_le => sub {
    my (undef, $l8, @fs) = unpack('(LLL/L)<', $record_le);
    return $l8 + $fs[0];
  },

  hunpack_assume_le_rf0_array => sub {
    my (undef, $l8, @fs) = unpack('(LLL/L)<', $record_le);
    return unpack $field_unpacks[$fs[0] & 7],
                  substr $record_le, $fs[0] & ~7;
  },

  hunpack_assume_le_rf1_array => sub {
    my (undef, $l8, @fs) = unpack('(LLL/L)<', $record_le);
    return unpack $field_unpacks[$fs[1] & 7],
                  substr $record_le, $fs[1] & ~7;
  },

  hunpack_assume_le_rf2_array => sub {
    my (undef, $l8, @fs) = unpack('(LLL/L)<', $record_le);
    return unpack $field_unpacks[$fs[2] & 7],
                  substr $record_le, $fs[2] & ~7;
  },

  funpack_assume_le_rf0_array => sub {
    return unpack $field_unpacks[$g_fs[0] & 7],
                  substr $record_le, $g_fs[0] & ~7;
  },

  funpack_assume_le_rf1_array => sub {
    return unpack $field_unpacks[$g_fs[1] & 7],
                  substr $record_le, $g_fs[1] & ~7;
  },

  funpack_assume_le_rf2_array => sub {
    return unpack $field_unpacks[$g_fs[2] & 7],
                  substr $record_le, $g_fs[2] & ~7;
  },

  split_rf0 => sub {
    my @fs = split /\t/, $text_record;
    return 0 + $fs[0];
  },

  split_rf1 => sub {
    my @fs = split /\t/, $text_record;
    return 0 + $fs[1];
  },

  split_rf2 => sub {
    my @fs = split /\t/, $text_record;
    return 0 + $fs[2];
  },

};

cmpthese $results;
