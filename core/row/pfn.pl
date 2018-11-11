# Parallel function interface (using xargs under the hood)
#
# ni ... fx24[ %var1 %var2 : lambda ]
#
# Does the same thing as f[], but in parallel using xargs. This is functionally
# equivalent to SX..., but is more ni-idiomatic.

defmetaoperator xargs_fn => q{
  my ($args, $left, $right) = @_;
  my ($n, $bindings, $fnbody) = @$args;
  my $xargs_arg = noise_str 32;
  conf_set 'xargs/arg' => $xargs_arg;

  my $xargs_op = row_xargs_scale_op $n, "i$xargs_arg", ":",
                                    [op_fn_op $bindings, $fnbody];
  ($left, [$xargs_op, @$right]);
};

defshort '/fx', pmap q{xargs_fn_op @$_},
                     pn [1, 3, 4], popt pempty,
                     pc integer, pc pstr '[', fn_bindings, '/series',
                                 pc pstr ']';
