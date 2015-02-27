# High-level pipe operations, each of which corresponds to a command-line
# option. They can also be used from compiled code.

our %op_shorthand_lookups;      # keyed by short
our %op_shorthands;             # keyed by long
our %op_formats;                # ditto
our %op_usage;                  # ditto
our %op_fns;                    # ditto

sub long_op_method  { "--$_[0]" =~ s/-/_/gr }
sub short_op_method { "_$_[0]" }

sub shell_quote { join ' ', map /[^-\/\w]/ ? "'" . s/(['\\])/'\\$1'/gr . "'"
                              : length $_  ? $_
                              :              "''", @_ }

sub self_pipe { ni_process(shell_quote('perl', '-', @_),
                           ni_memory(self)->reader_fh,
                           undef) }

sub defop {
  my ($long, $short, $format, $usage, $fn) = @_;
  if (defined $short) {
    $op_shorthands{$long}         = $short;
    $op_shorthand_lookups{$short} = "--$long";
  }
  $op_formats{$long} = $format;
  $op_usage{$long}   = $usage;
  $op_fns{$long}     = $fn;

  my $long_method_name = long_op_method $long;
  my $short_method_name =
    defined $short ? short_op_method $short : undef;

  die "operator $long already exists (possibly as a method rather than an op)"
    if exists $ni::io::{$long_method_name}
    or defined $short_method_name && exists $ni::io::{$short_method_name};

  # Enable programmatic access
  *{"ni::io::$short_method_name"} = $fn if defined $short_method_name;
  *{"ni::io::$long_method_name"}  = $fn;
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
      unshift @args, $a;
    }
  }

  [@parsed], @args;
}

sub file_opt { ['plus', ni $_[0]] }
sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map file_opt($_), @_ if $o eq '--';

    # Special cases
    if (ref($o) =~ /\[$/) {
      # Lambda-invocation of ni on the specified options.
      push @parsed, ['plus', self_pipe @$o];
    } elsif ($o =~ /^--/) {
      my $c = $o =~ s/^--//r;
      die "unknown long command: $o" unless exists $op_fns{$c};
      my ($args, @rest) = apply_format $op_formats{$c}, @_;
      push @parsed, [$c, @$args];
      @_ = @rest;
    } elsif ($o =~ s/^-//) {
      my ($op, @stuff) = grep length,
                         split /([:+^=%\/]?[a-zA-Z]|[-+\.0-9]+)/, $o;
      die "undefined short op: $op" unless exists $op_shorthand_lookups{$op};
      unshift @_, map $op_shorthand_lookups{$_} // $_, $op, @stuff;
    } else {
      push @parsed, file_opt $o;
    }
  }
  @parsed;
}
