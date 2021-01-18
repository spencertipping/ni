# Cell-level operators.
# Cell-specific transformations that are often much shorter than the equivalent
# Perl code. They're also optimized for performance.

defcontext 'cell', q{cell operator context};
defshort '/,', parser 'cell/qfn';

BEGIN {
  defparseralias cellspec       => pmap q{$_ || [1, 0]}, popt colspec;
  defparseralias cellspec_fixed => pmap q{$_ || [1, 0]}, popt colspec_fixed;
}

# Codegen.
# Most of these have exactly the same format and take a column spec.

use constant cell_op_gen => gen q{
  BEGIN {eval {require Digest::MD5; Digest::MD5->import(qw/md5 md5_hex/)}}
  my ($cs, %args) = @_;
  my ($floor, @cols) = @$cs;
  my $limit = $floor + 1;
  %begin;
  while (<STDIN>) {
    chomp;
    my @xs = split /\t/, $_, $limit;
    %each_line
    for (@cols) {
      %each
    }
    print join("\t", @xs) . "\n";
  }
  %end
};

sub cell_eval($@) {
  my ($h, @args) = @_;
  fn(cell_op_gen->(%$h))->(@args);
}


# Data cleaning.
# Remove characters that fall outside of specified classes; e.g. non-numerics,
# non-word, non-alpha-or-space.

defoperator cell_clean_regex => q{
  cell_eval {args  => '$qre',
             begin => '',
             each  => '$xs[$_] =~ s/$qre//g'}, @_;
};

defshort 'cell/C',
  defdsp 'cleanalt', 'dispatch table for cell/C clean operator',
    d => pmap(q{cell_clean_regex_op $_, qr/[^-0-9]/},       cellspec_fixed),
    f => pmap(q{cell_clean_regex_op $_, qr/[^-+eE.0-9]/},   cellspec_fixed),
    w => pmap(q{cell_clean_regex_op $_, qr/\W/},            cellspec_fixed),
    x => pmap(q{cell_clean_regex_op $_, qr/[^-0-9a-fA-F]/}, cellspec_fixed);


# Intification.
# Strategies to turn each distinct entry into a number. Particularly useful in a
# plotting context.

defoperator intify_compact => q{
  cell_eval {args  => 'undef',
             begin => 'my %ids; my $n = 0',
             each  => '$xs[$_] = ($ids{$xs[$_]} ||= ++$n) - 1'}, @_;
};

defoperator intify_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = unpack "N", md5 $xs[$_] . $seed'}, @_;
};

defoperator real_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = unpack("N", md5 $xs[$_] . $seed) / (1<<32)'}, @_;
};

defoperator md5 => q{
  cell_eval {args  => 'undef',
             begin => '',
             each  => '$xs[$_] = md5_hex $xs[$_]'}, @_;
};

defshort 'cell/z', pmap q{intify_compact_op $_},  cellspec_fixed;
defshort 'cell/h', pmap q{intify_hash_op    @$_}, pseq cellspec_fixed, popt integer;
defshort 'cell/H', pmap q{real_hash_op      @$_}, pseq cellspec_fixed, popt integer;

defshort 'cell/m', pmap q{md5_op $_}, cellspec_fixed;

defoperator bloom_prehash => q{
  cell_eval {args  => '$m, $k',
             begin => '($m, $k) = bloom_args $m, $k',
             each  => '$xs[$_] = bloom_prehash $m, $k, $xs[$_]'}, @_;
};

defshort 'cell/BP', pmap q{bloom_prehash_op @$_},
  pseq cellspec_fixed, bloom_size_spec, bloom_fp_spec;

# Count-changes.
# A constant-space way to get a new integer for each new value within a column.
# You can specify an optional modulus to get cycling values suitable for input
# to S\> or \*[].

defoperator count_changes => q{
  my ($colspec, $mod) = @_;

  if ("1,0" eq join",", @$colspec)
  {
    # Optimize the common case by bypassing most of the cell machinery.
    my $last = undef;
    my $n = 0;
    while (<STDIN>)
    {
      my $i = index $_, "\t";
      $i = length if $i < 0;
      my $k = substr $_, 0, $i;
      if ($k ne $last)
      {
        $last = $k;
        ++$n;
        $n %= $mod if defined $mod;
      }
      print "$n\t", substr $_, $i + 1;
    }
  }
  else
  {
    cell_eval {args  => '$mod',
               begin => 'my (@n, @last) = ()',
               each  => 'no warnings "uninitialized";
                         if ($xs[$_] ne $last[$_]) {
                           $last[$_] = $xs[$_];
                           ++$n[$_];
                           $n[$_] %= $mod if defined $mod;
                           $xs[$_] = $n[$_];
                         } else {
                           $xs[$_] = $n[$_];
                         }'}, @_;
  }
};

defshort 'cell/Z', pmap q{count_changes_op @$_},
                   pseq cellspec_fixed, popt integer;

# Numerical transformations.
# Trivial stuff that applies to each cell individually.

BEGIN {
  defparseralias quant_spec  => pmap q{$_ || 1}, popt number;
  defparseralias log_base    => pmap q{$_ || exp 1}, popt number;
  defparseralias jitter_bias => pmap q{dor $_, 0}, popt number;
  defparseralias jitter_mag  => pmap q{$_ || 1},   palt pmap(q{0.9}, prx ','),
                                                        popt number;
}

defoperator cell_log => q{
  my ($cs, $base) = @_;
  my $lb = 1 / log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = log(max 1e-16, \$xs[\$_]) * $lb"}, $cs;
};

defoperator cell_exp => q{
  my ($cs, $base) = @_;
  my $eb = log $base;
  cell_eval {args => 'undef', each => "\$xs[\$_] = exp $eb * \$xs[\$_]"}, $cs;
};

defshort 'cell/l', pmap q{cell_log_op @$_}, pseq cellspec_fixed, log_base;
defshort 'cell/e', pmap q{cell_exp_op @$_}, pseq cellspec_fixed, log_base;

# Log-progression that preserves sign of the values. Everything is shifted
# upwards by one in log-space, so signed_log(0) == log(1) == 0.
defoperator cell_signed_log => q{
  my ($cs, $base) = @_;
  my $lb = 1 / log $base;
  cell_eval {
    args => 'undef',
    each => "\$xs[\$_] = (\$xs[\$_] > 0 ? $lb : -$lb) * log(1 + abs \$xs[\$_])"}, $cs;
};

defshort 'cell/L', pmap q{cell_signed_log_op @$_}, pseq cellspec_fixed, log_base;

defoperator jitter_uniform => q{
  my ($cs, $mag, $bias) = @_;
  my $adjust = $bias - $mag / 2;
  cell_eval {args => 'undef', each => "\$xs[\$_] += rand() * $mag + $adjust"}, $cs;
};

defshort 'cell/j', pmap q{jitter_uniform_op @$_},
                   pseq cellspec_fixed, jitter_mag, jitter_bias;


defoperator jitter_gaussian => q{
  my ($cs, $mag) = @_;
  cell_eval {
    args => 'undef',
    each => "\$xs[\$_] += $mag * sqrt(-2 * log(max 1e-16, rand()))
                                         * cos(6.28318530717959 * rand())"},
    $cs;
};

defshort 'cell/J', pmap q{jitter_gaussian_op @$_},
                   pseq cellspec_fixed, jitter_mag;


defoperator quantize => q{
  my ($cs, $q) = @_;
  my $iq = 1 / $q;
  cell_eval {args => 'undef',
             each => "\$xs[\$_] = $q * int(0.5 + $iq * \$xs[\$_])"}, $cs;
};

defshort 'cell/q', pmap q{quantize_op @$_}, pseq cellspec_fixed, quant_spec;

# Cellular quantization (for visualization): quantize axes to cell centers, then
# uniformly jitter them by 0.9 * that amount. This will produce visually
# distinct but uniformly shaded cells.
defshort 'cell/Q',
  pmap q{ my ($cellspec, $quantum) = @$_;
          [quantize_op($cellspec, $quantum),
           jitter_uniform_op($cellspec, $quantum * 0.9)] },
  pseq cellspec_fixed, quant_spec;


# Random value attenuation (for visualization): attenuate by 1-rand()**n, where
# you can specify n. This results in values casting a shadow downwards.
BEGIN
{ defparseralias attenuate_spec => pmap q{$_ || 4}, popt number }

defoperator attenuate => q{
  my ($cs, $power) = @_;
  my $rand_code = $power == int $power
    ? join"*", ("rand()") x $power
    : "rand() ** $power";
  cell_eval {args => 'undef',
             each => "\$xs[\$_] *= (1 - $rand_code)"}, $cs;
};

defshort 'cell/A',
  pmap q{attenuate_op @$_},
  pseq cellspec_fixed, attenuate_spec;


# Streaming numeric transformations.
# Sum, delta, average, variance, entropy, etc. Arguably these are column operators and
# not cell operators, but in practice you tend to use them in the same context as
# things like log scaling.

defoperator col_sum => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols',
             each  => '$xs[$_] = $ns[$_] += $xs[$_]'}, @_;
};

defoperator col_delta => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols',
             each  => '$xs[$_] -= $ns[$_], $ns[$_] += $xs[$_]'}, @_;
};

defoperator col_average => q{
  cell_eval {args  => 'undef',
             begin => 'my @ns = map 0, @cols; $. = 0',
             each  => '$xs[$_] = ($ns[$_] += $xs[$_]) / $.'}, @_;
};

defshort 'cell/a', pmap q{col_average_op $_}, cellspec_fixed;
defshort 'cell/s', pmap q{col_sum_op     $_}, cellspec_fixed;
defshort 'cell/d', pmap q{col_delta_op   $_}, cellspec_fixed;


defoperator col_windowed_average => q{
  cell_eval {args  => '$wsize',
             begin => 'my @ws = map [], @cols;
                       my @t = map 0, @cols',
             each  => 'push @{$ws[$_]}, $xs[$_];
                       $t[$_] += $xs[$_];
                       $t[$_] -= shift @{$ws[$_]} if @{$ws[$_]} > $wsize;
                       $xs[$_] = $t[$_] / @{$ws[$_]}'}, @_;
};

defshort 'cell/aw', pmap q{col_windowed_average_op @$_},
                         pseq cellspec_fixed, integer;


# Grouped sum/average.
# This is to save you the indignity of writing something like
# "p'r a, sum b_ rea'", which is a common and keystroke-heavy thing to do.
# Instead, you can write the much-suaver ",sgA": sum values grouped ending with
# column A (so the values are in col B). The same goes for averaging.
#
# We're overloading the syntax against "g", the geohash encoding operator,
# which you would never use immediately after a non-colspec ,s or ,a -- so this
# will never collide in practice.

defshort 'cell/ag', pmap
  q{
    my $col  = $_;
    my $fs   = "0..$col";
    my $se   = "se" . ("A".."Z")[$col];
    my $next = ("a".."z")[$col + 1];
    perl_mapper_op "
      my \@fs = F_($fs);
      my (\$t, \$n) = $se { (\$_[0] + $next, \$_[1] + 1) } 0, 0;
      r \@fs, \$t / (\$n || 1)";
  }, colspec1;

defshort 'cell/sg', pmap
  q{
    my $col  = $_;
    my $fs   = "0..$col";
    my $se   = "se" . ("A".."Z")[$col];
    my $next = ("a".."z")[$col + 1];
    perl_mapper_op "r F_($fs), $se { \$_[0] + $next } 0";
  }, colspec1;


# Time conversions.

defoperator epoch_to_formatted => q{
  cell_eval {args => 'undef',
             each => q{$xs[$_] = sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ",
                                         time_epoch_pieces $xs[$_]}}, @_;
};

defshort 'cell/t', pmap q{epoch_to_formatted_op $_}, cellspec_fixed;

# Geohash conversions.
# These can be parameterized by a precision spec, which takes the same form as
# the one you normally use with `ghe` and `ghd`.

defoperator geohash_encode => q{
  cell_eval {args => '@precision',
             each => q{$xs[$_] = geohash_encode split(/,/, $xs[$_]), @precision}}, @_;
};

defoperator geohash_decode => q{
  cell_eval {args => '@precision',
             each => q{$xs[$_] = join",", geohash_decode $xs[$_], @precision}}, @_;
};

defshort 'cell/g', pmap q{geohash_encode_op @$_}, pseq cellspec_fixed, palt integer, pk 12;
defshort 'cell/G', pmap q{geohash_decode_op @$_}, pseq cellspec_fixed, popt integer;
