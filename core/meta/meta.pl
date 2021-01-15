# Image-related data sources.
# Long options to access ni's internal state. Also the ability to instantiate ni
# within a shell process.

defoperator meta_image => q{sio; print image, "\n"};
defoperator meta_keys  => q{sio; print "$_\n" for sort keys %ni::self};
defoperator meta_key   => q{my @ks = @_; sio; print "$_\n" for @ni::self{@ks}};

defoperator meta_help => q{
  my ($topic) = @_;
  $topic = 'tutorial' unless length $topic;
  $topic =~ s/^ex(\d+)$/ni_by_example_$1.md/;
  sio; print dor($ni::self{"doc/$topic.md"}, $ni::self{"doc/$topic"}), "\n";
};

defshort '///ni/',     pmap q{meta_key_op $_}, prc '[^][]+$';
defshort '///ni',      pmap q{meta_image_op},  pnone;
defshort '///你',      pmap q{meta_image_op},  pnone;
defshort '///ni/keys', pmap q{meta_keys_op},   pnone;

defoperator meta_eval_number => q{sio; print $ni::evals{$_[0] - 1}, "\n"};
defshort '///ni/eval/', pmap q{meta_eval_number_op $_}, integer;

# Documentation options.
# These are listed under the `//help` prefix. This isn't a toplevel option
# because it's more straightforward to model these as data sources.

sub meta_context_name($) {$_[0] || '<root>'}

defshort '///help', pmap q{meta_help_op $_}, popt prx '/(.*)';

defoperator meta_options => q{
  sio;
  for my $c (sort keys %ni::contexts) {
    printf "%s\tlong\t%s\t%s\n",  meta_context_name $c, $ni::long_names{$c}[$_], abbrev dev_inspect_nonl $ni::long_refs{$c}[$_],  40 for       0..$#{$ni::long_refs{$c}};
    printf "%s\tshort\t%s\t%s\n", meta_context_name $c, $_,                      abbrev dev_inspect_nonl $ni::short_refs{$c}{$_}, 40 for sort keys %{$ni::short_refs{$c}};
  }
};

defshort '///ni/options', pmap q{meta_options_op}, pnone;

defshort '///license', pmap q{meta_key_op 'license'}, pnone;
defshort '/--license', pmap q{meta_key_op 'license'}, pnone;

defoperator meta_conf => q{
  sio;
  print "$_\t" . conf($_) . "\t$ni::conf_variables{$_}\n" for sort keys %ni::conf_variables;
};

defshort '///ni/conf', pmap q{meta_conf_op}, pnone;

# Inspection.
# This lets you get details about specific operators or parsing contexts.

defoperator meta_op  => q{sio; print "sub {$ni::operators{$_[0]}}\n"};
defoperator meta_ops => q{sio; print "$_\n" for sort keys %ni::operators};
defshort '///ni/op/', pmap q{meta_op_op $_}, prc '.+';
defshort '///ni/ops', pmap q{meta_ops_op},   pnone;

defoperator meta_parser  => q{sio; print json_encode(parser $_[0]), "\n"};
defoperator meta_parsers => q{sio; print "$_\t" . json_encode(parser $_) . "\n" for sort keys %ni::parsers};
defshort '///ni/parser/', pmap q{meta_parser_op $_}, prc '.+';
defshort '///ni/parsers', pmap q{meta_parsers_op}, pnone;

# The backdoor.
# Motivated by `bugs/2016.0918-replicated-garbage`. Lets you eval arbitrary Perl
# code within this process, and behaves like a normal streaming operator.

defoperator dev_backdoor => q{ni::eval $_[0]};
defshort '/--dev/backdoor', pmap q{dev_backdoor_op $_}, prc '.*';

# Used for regression testing
defoperator dev_local_operate => q{
  my ($lambda) = @_;
  my $fh = siproc {exec ni_quoted_exec_args};
  quote_ni_into $fh, @$lambda;
};

defshort '/--dev/local-operate', pmap q{dev_local_operate_op $_}, _qfn;
