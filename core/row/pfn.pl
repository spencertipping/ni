# Parallel function interface (using xargs under the hood)
#
# ni ... fx24[ %var1 %var2 : lambda ]
#
# Does the same thing as f[], but in parallel using xargs. This is functionally
# equivalent to SX..., but is more ni-idiomatic.

# Super-devious thing happening here...
# row_xargs_scale_op uses xargs, which uses sh -c, to rewrite args into the
# inner ni. In doing so, it calls shell_quote to protect data. All of this is
# janky but usually works.
#
# The tricky part is that $xargs_arg will initially be an innocuous identifier
# as produced by noise_str -- but xargs will then transform it into an arbitrary
# string. That means it needs to be form-preserving within sh -c, which
# obviously can't work: there is no safe context.
#
# We fix this by hex-encoding all data prior to xargs, then decoding it within
# the xargs child.

defmetaoperator xargs_fn => q{
  my ($args, $left, $right) = @_;
  my ($n, $bindings, $fnbody) = @$args;
  my $xargs_arg = noise_str 32;
  conf_set 'xargs/arg' => $xargs_arg;

  my $xargs_op = row_xargs_scale_op
    $n, "1p'pack \"H*\", \"$xargs_arg\"'", ":",
    [op_fn_op $bindings, $fnbody];

  my $safe_op = perl_mapper_op('unpack "H*"');

  ($left, [$safe_op, $xargs_op, @$right]);
};

defshort '/fx', pmap q{xargs_fn_op @$_},
                     pn [1, 3, 4], popt pempty,
                     pc integer, pc pstr '[', fn_bindings, '/series',
                                 pc pstr ']';
