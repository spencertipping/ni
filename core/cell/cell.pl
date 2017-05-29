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
  my ($cs, %args) = @_;
  my ($floor, @cols) = @$cs;
  my $limit = $floor + 1;
  %begin;
  while (<STDIN>) {
    chomp;
    my @xs = split /\t/, $_, $limit;
    %each_line
    %each for @cols;
    print join("\t", @xs) . "\n";
  }
  %end
};

sub cell_eval($@) {
  my ($h, @args) = @_;
  fn(cell_op_gen->(%$h))->(@args);
}

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
             each  => '$xs[$_] = murmurhash3 $xs[$_], $seed'}, @_;
};

defoperator real_hash => q{
  cell_eval {args  => '$seed',
             begin => '$seed ||= 0',
             each  => '$xs[$_] = murmurhash3($xs[$_], $seed) / (1<<32)'}, @_;
};

defshort 'cell/z', pmap q{intify_compact_op $_},  cellspec_fixed;
defshort 'cell/h', pmap q{intify_hash_op    @$_}, pseq cellspec_fixed, popt integer;
defshort 'cell/H', pmap q{real_hash_op      @$_}, pseq cellspec_fixed, popt integer;

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


defoperator quantize => q{
  my ($cs, $q) = @_;
  my $iq = 1 / $q;
  cell_eval {args => 'undef',
             each => "\$xs[\$_] = $q * int(0.5 + $iq * \$xs[\$_])"}, $cs;
};

defshort 'cell/q', pmap q{quantize_op @$_}, pseq cellspec_fixed, quant_spec;

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
