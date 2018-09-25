# Row-based process scaling, powered by xargs.
# Similar to S8, but SX8 will use xargs -P8. xargs has every process write to
# the same output fd, so we have to redirect each output into a file in order to
# prevent rows from getting spliced.

# Overall usage looks like this:
#
#   ni indir SX4 {} z\>output/{}.gz [ ni commands to distribute ]
#
# {} and z\>output/{}.gz are both parsed as _strings_ -- i.e. as shell commands
# -- because they get passed straight through to xargs. The above is
# semantically equivalent to this:
#
#   ni indir e[ xargs -P4 -I{} /tmp/ni {} --internal/lambda z\>output/{}.gz ]
#
# We write our current state, closures and all, to a tempfile so the xargs
# indirection doesn't lose anything.

defconfenv 'xargs/arg', NI_XARGS_ARG => '{}';

defmetaoperator run_quoted_lambda => q{
  my ($args, $left, $right) = @_;
  my ($name) = @$args;
  my $ops = json_decode $ni::self{"quoted/$name"};
  ($left, [@$ops, @$right]);
};

defshort '/--internal/lambda' => pmap q{run_quoted_lambda_op $_}, prc '.*';

defoperator row_xargs_scale => q{
  my ($n, $inform, $outform, $lambda) = @_;
  my $arg = conf 'xargs/arg';
  my $tmp_ni = uri_path resource_tmp "file://";
  wf $tmp_ni, image_with "quoted/$$-lambda" => json_encode $lambda;
  chmod 0700, $tmp_ni;
  my $cmd = shell_quote "xargs", "-P$n", "-I$arg", "sh", "-c",
              "$tmp_ni $inform --internal/lambda$$-lambda $outform";
  system $cmd and die "ni SX$n: $cmd failed; temporary ni in $tmp_ni";
  unlink $tmp_ni;
};

defscalealt pmap q{row_xargs_scale_op @$_},
            pn 1, pstr"X",
                  pseq pc integer, pc shell_arg, pc shell_arg, _qfn;
