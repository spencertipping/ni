# High-level pipe operations, each of which corresponds to a command-line
# option. They can also be used from compiled code.

our %op_shorthand_lookups;      # keyed by short
our %op_shorthands;             # keyed by long
our %op_formats;                # ditto
our %op_usage;                  # ditto
our %op_fns;                    # ditto

sub defop {
  my ($long, $short, $format, $usage, $fn) = @_;
  if (defined $short) {
    $op_shorthands{$long}         = $short;
    $op_shorthand_lookups{$short} = "--$long";
  }
  $op_formats{$long} = $format;
  $op_usage{$long}   = $usage;
  $op_fns{$long}     = $fn;

  die "operator $long already exists (possibly as a method rather than an op)"
    if exists $ni::io::{$long};

  *{"ni::io::$long"} = $fn;     # programmatic access
}

our %format_matchers = (
  a => qr/^[a-zA-Z]+$/,
  d => qr/^[-+\.0-9]+$/,
  s => qr/^.*$/,
  v => qr/^[^-].*$/,
);

sub apply_format {
  my ($format, @args) = @_;
  my @format = split //, $format;
  my @parsed;

  for (@format) {
    die "too few arguments for $format" if !@args && !/[A-Z]/;
    my $a = shift @args;
    if ($a =~ /$format_matchers{lc $_}/) {
      push @parsed, $a;
    } else {
      die "failed to match format $format" unless /[A-Z]/;
      push @parsed, undef;
    }
  }

  [@parsed], @args;
}

sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map ['plus', $_], @_ if $o eq '--';
    if ($o =~ /^--/) {
      my $c = $o =~ s/^--//r;
      die "unknown long command: $o" unless exists $op_fns{$c};
      my ($args, @rest) = apply_format $op_formats{$c}, @_;
      push @parsed, [$c, @$args];
      @_ = @rest;
    } elsif ($o =~ s/^-//) {
      unshift @_, map $op_shorthand_lookups{$_} // $_,
                      $o =~ /([:+^=%\/]?[a-zA-Z]|[-+\.0-9]+)/g;
    } else {
      push @parsed, ['plus', $o];
    }
  }
  @parsed;
}

# Record transformation
# Most of the transforms within ni are in-process, so it's possible for one
# operator to append a data element that would be interpreted differently if we
# later added an external command like a sort. We want to avoid this issue as
# much as possible, which we can do by using some default idioms.

sub ::row {
  my $s = join "\t", @_;
  $s =~ s/\n//g;
  $s;
}

sub record_transformer {
  # Look at the function and figure out what we need. If the function refers to
  # @_ as an array, then we'll need to parse into columns. Otherwise we may be
  # able to just get away with providing $_ as the un-chomped line.
  my ($code) = @_;
  my $parse_prefix = $code =~ /\@_/ || $code =~ /\$_\[/ || $code =~ /\%\d+/
    ? 'chomp; @_ = split /\t/, $_;'
    : '';

  my $f = compile qq{
    local \$_ = \$_[0];
    $parse_prefix;
    $code;
  };

  sub {
    my @result;
    for ($f->(@_)) {
      if (/^[^\n]*\n$/) {
        push @result, $_;
      } else {
        push @result, map "$_\n", split /\n/;
      }
    }
    @result;
  };
}
