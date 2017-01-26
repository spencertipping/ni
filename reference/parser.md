
# PARSER /lambda
	A bracketed lambda function in context ''

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  </series>
	  ']'
	) -> {$$_[1]}

# PARSER /op
	A single operator in the context ''

## DEFINITION
	(
	| (
	  | ''file-closure://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "file-closure://$_"}
	  | ''file://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "file://$_"}
	  | ''hdfs://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "hdfs://$_"}
	  | ''hdfst://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "hdfst://$_"}
	  | ''http://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "http://$_"}
	  | ''https://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "https://$_"}
	  | ''s3cmd://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "s3cmd://$_"}
	  | ''sftp://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_quote_op "sftp://$_"}
	  | 'file-closure://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "file-closure://$_"}
	  | 'file://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "file://$_"}
	  | 'hdfs://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "hdfs://$_"}
	  | 'hdfst://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "hdfst://$_"}
	  | 'http://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "http://$_"}
	  | 'https://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "https://$_"}
	  | 's3cmd://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "s3cmd://$_"}
	  | 'sftp://' (
	      /.*/
	      <empty>?
	    ) -> {$$_[0]} -> {resource_append_op "sftp://$_"}
	  )
	| <filename> -> {cat_op $_}
	| </short>
	)

# PARSER /qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| </lambda>
	| </suffix>
	)

# PARSER /series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  </op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER /short
	Dispatch table for short options in context ''

## DEFINITION
	(
	| '!' (
	  | 'p' <perl_asserter_code> -> {perl_assert_op $_}
	  )
	| '$hadoop/jobconf' '' -> {conf_get_op 'hadoop/jobconf'}
	| '$hadoop/jobname' '' -> {conf_get_op 'hadoop/jobname'}
	| '$hadoop/name' '' -> {conf_get_op 'hadoop/name'}
	| '$hadoop/streaming-jar' '' -> {conf_get_op 'hadoop/streaming-jar'}
	| '$hdfs/tmpdir' '' -> {conf_get_op 'hdfs/tmpdir'}
	| '$monitor' '' -> {conf_get_op 'monitor'}
	| '$pager' '' -> {conf_get_op 'pager'}
	| '$row/seed' '' -> {conf_get_op 'row/seed'}
	| '$row/sort-buffer' '' -> {conf_get_op 'row/sort-buffer'}
	| '$row/sort-compress' '' -> {conf_get_op 'row/sort-compress'}
	| '$row/sort-parallel' '' -> {conf_get_op 'row/sort-parallel'}
	| '$tmpdir' '' -> {conf_get_op 'tmpdir'}
	| '%' (
	    <number>?
	    </qfn>
	  ) -> {interleave_op @$_}
	| ''' (
	    (
	      /(?^:\[)/
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      /[^]].*/
	      <empty>?
	    ) -> {$$_[0]}*
	    (
	      /(?^:\])/
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {$$_[1]} -> {resource_quote_many_op @$_}
	| '+' </qfn> -> {append_op    @$_}
	| ',' (
	  | <cell/lambda>
	  | <cell/suffix>
	  )
	| '--dev/backdoor' /.*/ -> {dev_backdoor_op $_}
	| '--dev/local-operate' </qfn> -> {dev_local_operate_op $_}
	| '--http/wse' '' -> {http_websocket_encode_op}
	| '--http/wse-batch' <integer>? -> {http_websocket_encode_batch_op $_}
	| '//:' (
	    <closure_name>
	    <empty>?
	  ) -> {$$_[0]} -> {memory_closure_append_op $_}
	| '//@' (
	    <closure_name>
	    <empty>?
	  ) -> {$$_[0]} -> {file_closure_append_op $_}
	| '//help' //(.*)/? -> {meta_help_op $_}
	| '//license' '' -> {meta_key_op 'license'}
	| '//ni' '' -> {meta_image_op}
	| '//ni/' (
	    /[^][]+$/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_key_op $_}
	| '//ni/conf' '' -> {meta_conf_op}
	| '//ni/eval/' <integer> -> {meta_eval_number_op $_}
	| '//ni/keys' '' -> {meta_keys_op}
	| '//ni/map/short' '' -> {meta_short_availability_op}
	| '//ni/op/' (
	    /.+/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_op_op $_}
	| '//ni/ops' '' -> {meta_ops_op}
	| '//ni/options' '' -> {meta_options_op}
	| '//ni/parser/' (
	    /.+/
	    <empty>?
	  ) -> {$$_[0]} -> {meta_parser_op $_}
	| '//ni/parsers' '' -> {meta_parsers_op}
	| '1' '' -> {n_op 1, 2}
	| ':' (
	    <nefilename>
	    <empty>?
	  ) -> {$$_[0]} -> {inline_checkpoint_op $_}
	| '::' (
	    (
	      <closure_name>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {memory_data_closure_op @$_}
	| ':@' (
	    (
	      <closure_name>
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {file_data_closure_op @$_}
	| '<' '' -> {file_read_op}
	| '=' </qfn> -> {divert_op    @$_}
	| '>' <nefilename> -> {file_write_op $_}
	| '>'R' '' -> {encode_resource_stream_op}
	| 'B' (
	  | 'n' '' -> {buffer_null_op}
	  )
	| 'C' (
	  | (
	      (
	        (
	          /A/
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          /[^][]+/ -> {[/\+([^][+]+)/g]}
	          <empty>?
	        ) -> {$$_[0]}
	      ) -> {$$_[1]}
	      </qfn>
	    ) -> {docker_run_dynamic_op alpine_dockerfile(@{$$_[0]}), @{$$_[1]}}
	  | (
	      (
	        (
	          /U/
	          <empty>?
	        ) -> {$$_[0]}
	        (
	          /[^][]+/ -> {[/\+([^][+]+)/g]}
	          <empty>?
	        ) -> {$$_[0]}
	      ) -> {$$_[1]}
	      </qfn>
	    ) -> {docker_run_dynamic_op ubuntu_dockerfile(@{$$_[0]}), @{$$_[1]}}
	  | (
	      (
	        /[^][]+/
	        <empty>?
	      ) -> {$$_[0]}
	      </qfn>
	    ) -> {docker_run_image_op $$_[0], @{$$_[1]}}
	  )
	| 'D' <generic_code> -> {destructure_op $_}
	| 'E' (
	    (
	      /[^][]+/
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {docker_exec_op $$_[0], @{$$_[1]}}
	| 'F' (
	  | '/' <regex> -> {split_regex_op $_}
	  | ':' /./ -> {split_chr_op   $_}
	  | 'C' '' -> {split_chr_op   ','}
	  | 'P' '' -> {split_chr_op   '|'}
	  | 'S' '' -> {split_regex_op '\s+'}
	  | 'V' '' -> {split_proper_csv_op}
	  | 'W' '' -> {split_regex_op '[^\w\n]+'}
	  | 'm' (
	      '/'
	      <regex> -> {scan_regex_op $_}
	    ) -> {$$_[1]}
	  )
	| 'G' (
	  | <gnuplot/lambda>
	  | <gnuplot/suffix>
	  ) -> {stream_to_gnuplot_op $_}
	| 'H' (
	  | 'DS' (
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {my ($m, $c, $r) = @$_;
	                              my @cr =
	                                (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
	                                 defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
	                              [@$m, @cr]}
	  | 'S' (
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        <hadoop_streaming_lambda>
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {hadoop_streaming_op @$_}
	  )
	| 'N' (
	    <colspec1>?
	    <pycode>
	  ) -> {numpy_dense_op @$_}
	| 'O' <sortspec> -> {row_sort_op '-rn', sort_args @$_}
	| 'P' (
	  | 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                                 file_read_op,
	                                 row_match_op '/part-']}
	  | 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	  )
	| 'Q' (
	  | 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	  )
	| 'S' (
	  | (
	      <integer>
	      </qfn>
	    ) -> {row_fixed_scale_op @$_}
	  )
	| 'W' </qfn> -> {with_left_op  @$_}
	| 'X' <colspec1>? -> {sparse_to_dense_op $_}
	| 'Y' <colspec1>? -> {dense_to_sparse_op $_}
	| '^' </qfn> -> {prepend_op   @$_}
	| '^{' (
	    <config_option_map>
	    </qfn>
	  ) -> {configure_op @$_}
	| 'b' (
	  | 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	  )
	| 'c' '' -> {count_op}
	| 'e' <shell_command> -> {sh_op $_}
	| 'f' (
	  | <colspec> -> {cols_op @$_}
	  )
	| 'g' <sortspec> -> {row_sort_op        sort_args @$_}
	| 'i' <id_text> -> {echo_op $_}
	| 'j' (
	    <colspec>?
	    </qfn>
	  ) -> {join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]}
	| 'l' <lispcode> -> {lisp_code_op lisp_mapgen->(prefix => lisp_prefix,
	                                                   body   => $_)}
	| 'm' (
	  | <rbcode> -> {ruby_mapper_op $_}
	  )
	| 'n' <number>? -> {n_op 1, defined $_ ? $_ + 1 : -1}
	| 'n0' <number>? -> {n_op 0, defined $_ ? $_ : -1}
	| 'o' <sortspec> -> {row_sort_op '-n',  sort_args @$_}
	| 'p' (
	  | <perl_mapper_code> -> {perl_mapper_op $_}
	  )
	| 'r' (
	  | (
	      'l'
	      <lispcode>
	    ) -> {$$_[1]} -> {lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
	                                                 body   => $_)}
	  | (
	      'm'
	      <rbcode>
	    ) -> {$$_[1]} -> {ruby_grepper_op $_}
	  | (
	      'p'
	      <perl_grepper_code>
	    ) -> {$$_[1]} -> {perl_grepper_op $_}
	  | (
	      /[+~]/
	      <integer>
	    ) -> {$$_[1]} -> {tail_op '-n', '',  $_}
	  | (
	      /-/
	      <integer>
	    ) -> {$$_[1]} -> {tail_op '-n', '+', ($_ + 1)}
	  | (
	      /x/
	      <number>
	    ) -> {$$_[1]} -> {row_every_op  $_}
	  | (
	      ///
	      <regex>
	    ) -> {$$_[1]} -> {row_match_op  $_}
	  | /\.\d+/ -> {row_sample_op $_}
	  | <integer> -> {head_op '-n', 0 + $_}
	  | <colspec_fixed> -> {row_cols_defined_op @$_}
	  )
	| 's' (
	    (
	    | (
	        <ssh_host> -> {[$_]}
	        <empty>?
	      ) -> {$$_[0]}
	    | (
	        <multiword>
	        <empty>?
	      ) -> {$$_[0]}
	    )
	    </qfn>
	  ) -> {ssh_op @$_}
	| 'u' '' -> {uniq_op}
	| 'v' (
	    <colspec_fixed>
	    </qfn>
	  ) -> {vertical_apply_op @$_}
	| 'w' </qfn> -> {with_right_op @$_}
	| 'x' <colspec>? -> {ref $_ ? colswap_op @$_ : colswap_op 2, 1}
	| 'z' <compressor_spec>
	| 'zd' <'', evaluate as [decode]>
	| 'zn' <'', evaluate as [sink_null]>
	)

# PARSER /suffix
	A string of operators unbroken by whitespace

## DEFINITION
	</op>*

# PARSER alt/colalt

## DEFINITION
	(
	| <colspec> -> {cols_op @$_}
	)

# PARSER alt/dockeralt

## DEFINITION
	(
	| (
	    (
	      (
	        /A/
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        /[^][]+/ -> {[/\+([^][+]+)/g]}
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {$$_[1]}
	    </qfn>
	  ) -> {docker_run_dynamic_op alpine_dockerfile(@{$$_[0]}), @{$$_[1]}}
	| (
	    (
	      (
	        /U/
	        <empty>?
	      ) -> {$$_[0]}
	      (
	        /[^][]+/ -> {[/\+([^][+]+)/g]}
	        <empty>?
	      ) -> {$$_[0]}
	    ) -> {$$_[1]}
	    </qfn>
	  ) -> {docker_run_dynamic_op ubuntu_dockerfile(@{$$_[0]}), @{$$_[1]}}
	| (
	    (
	      /[^][]+/
	      <empty>?
	    ) -> {$$_[0]}
	    </qfn>
	  ) -> {docker_run_image_op $$_[0], @{$$_[1]}}
	)

# PARSER alt/perlalt

## DEFINITION
	(
	| <perl_mapper_code> -> {perl_mapper_op $_}
	)

# PARSER alt/pysparkrowalt

## DEFINITION
	(
	| <integer> -> {gen "%v.sample(False, $_)"}
	| /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	| <pyspark_fn> -> {gen "%v.filter($_)"}
	)

# PARSER alt/rowalt

## DEFINITION
	(
	| (
	    'l'
	    <lispcode>
	  ) -> {$$_[1]} -> {lisp_code_op lisp_grepgen->(prefix => lisp_prefix,
	                                               body   => $_)}
	| (
	    'm'
	    <rbcode>
	  ) -> {$$_[1]} -> {ruby_grepper_op $_}
	| (
	    'p'
	    <perl_grepper_code>
	  ) -> {$$_[1]} -> {perl_grepper_op $_}
	| (
	    /[+~]/
	    <integer>
	  ) -> {$$_[1]} -> {tail_op '-n', '',  $_}
	| (
	    /-/
	    <integer>
	  ) -> {$$_[1]} -> {tail_op '-n', '+', ($_ + 1)}
	| (
	    /x/
	    <number>
	  ) -> {$$_[1]} -> {row_every_op  $_}
	| (
	    ///
	    <regex>
	  ) -> {$$_[1]} -> {row_match_op  $_}
	| /\.\d+/ -> {row_sample_op $_}
	| <integer> -> {head_op '-n', 0 + $_}
	| <colspec_fixed> -> {row_cols_defined_op @$_}
	)

# PARSER alt/rubyalt

## DEFINITION
	(
	| <rbcode> -> {ruby_mapper_op $_}
	)

# PARSER alt/scalealt

## DEFINITION
	(
	| (
	    <integer>
	    </qfn>
	  ) -> {row_fixed_scale_op @$_}
	)

# PARSER alt/sqljoinalt

## DEFINITION
	(
	| (
	    'L'
	    <sql_query>
	  ) -> {$$_[1]} -> {['ljoin', $_]}
	| (
	    'R'
	    <sql_query>
	  ) -> {$$_[1]} -> {['rjoin', $_]}
	| (
	    'N'
	    <sql_query>
	  ) -> {$$_[1]} -> {['njoin', $_]}
	| <sql_query> -> {['ijoin', $_]}
	)

# PARSER alt/sqlrowalt

## DEFINITION
	(
	| <integer> -> {['take',   $_]}
	| <sqlcode> -> {['filter', $_]}
	)

# PARSER cell/lambda
	A bracketed lambda function in context 'cell'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <cell/series>
	  ']'
	) -> {$$_[1]}

# PARSER cell/op
	A single operator in the context 'cell'

## DEFINITION
	(
	| <cell/short>
	)

# PARSER cell/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <cell/lambda>
	| <cell/suffix>
	)

# PARSER cell/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <cell/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER cell/short
	Dispatch table for short options in context 'cell'

## DEFINITION
	(
	| 'H' (
	    <cellspec_fixed>
	    <integer>?
	  ) -> {real_hash_op      @$_}
	| 'a' <cellspec_fixed> -> {col_average_op $_}
	| 'd' <cellspec_fixed> -> {col_delta_op   $_}
	| 'e' (
	    <cellspec_fixed>
	    <log_base>
	  ) -> {cell_exp_op @$_}
	| 'h' (
	    <cellspec_fixed>
	    <integer>?
	  ) -> {intify_hash_op    @$_}
	| 'j' (
	    <cellspec_fixed>
	    <jitter_mag>
	    <jitter_bias>
	  ) -> {jitter_uniform_op @$_}
	| 'l' (
	    <cellspec_fixed>
	    <log_base>
	  ) -> {cell_log_op @$_}
	| 'p' (
	    <colspec>
	    <perl_cell_transform_code>
	  ) -> {perl_cell_transformer_op @$_}
	| 'q' (
	    <cellspec_fixed>
	    <quant_spec>
	  ) -> {quantize_op @$_}
	| 's' <cellspec_fixed> -> {col_sum_op     $_}
	| 'z' <cellspec_fixed> -> {intify_compact_op $_}
	)

# PARSER cell/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<cell/op>*

# PARSER cellspec

## DEFINITION
	<colspec>? -> {$_ || [1, 0]}

# PARSER cellspec_fixed

## DEFINITION
	<colspec_fixed>? -> {$_ || [1, 0]}

# PARSER closure_name

## DEFINITION
	/[^][]+/

# PARSER colspec
	A set of columns, possibly including '.' ("the rest")

## DEFINITION
	(
	  ','?
	  (
	  | <colspec_range>
	  | <colspec1>
	  | <colspec_rest>
	  )
	) -> {$$_[1]}+ -> {[map ref() ? @$_ : $_, @$_]} -> {[max(@$_) + 1, @$_]}

# PARSER colspec1
	A way to identify a single column; either A-Z or #N

## DEFINITION
	(
	| (
	    '#'
	    <integer>
	  ) -> {$$_[1]}
	| /[A-Z]/ -> {ord() - 65}
	)

# PARSER colspec_fixed
	A set of definite columns; disallows '.' ("the rest")

## DEFINITION
	(
	  ','?
	  (
	  | <colspec_range>
	  | <colspec1>
	  )
	) -> {$$_[1]}+ -> {[map ref() ? @$_ : $_, @$_]} -> {[max(@$_) + 1, @$_]}

# PARSER colspec_range
	A range of columns, e.g. A-Q or #10-#20

## DEFINITION
	(
	  <colspec1>
	  '-'
	  <colspec1>
	) -> {[$$_[0] .. $$_[2]]}

# PARSER colspec_rest
	"The rest of the columns": everything to the right of the rightmost
	explicitly-specified column

## DEFINITION
	'.' -> {-1}

# PARSER compressor_name

## DEFINITION
	/[gxo4b]/

# PARSER compressor_spec

## DEFINITION
	(
	  <compressor_name>?
	  <integer>?
	) -> {my ($c, $level) = @$_;
	           $c = $ni::compressors{$c || 'g'};
	           defined $level ? sh_op "$c -$level" : sh_op $c}

# PARSER config_map_key

## DEFINITION
	/[^=]+/

# PARSER config_map_kv

## DEFINITION
	(
	  <config_map_key>
	  '='
	  <config_map_value>
	) -> {[@$_[0,2]]}

# PARSER config_map_value

## DEFINITION
	(
	  /.*[^}]+|/
	  <empty>?
	) -> {$$_[0]}

# PARSER config_option_map

## DEFINITION
	(
	  <config_map_kv>*
	  (
	    /}/
	    <empty>?
	  ) -> {$$_[0]}
	) -> {$$_[0]} -> {my %h; $h{$$_[0]} = $$_[1] for @{$_[0]}; \%h}

# PARSER dsp/assertdsp

## DEFINITION
	(
	| 'p' <perl_asserter_code> -> {perl_assert_op $_}
	)

# PARSER dsp/binaryalt

## DEFINITION
	(
	| 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	)

# PARSER dsp/bufferalt

## DEFINITION
	(
	| 'n' '' -> {buffer_null_op}
	)

# PARSER dsp/hadoopalt

## DEFINITION
	(
	| 'DS' (
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {my ($m, $c, $r) = @$_;
	                            my @cr =
	                              (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
	                               defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
	                            [@$m, @cr]}
	| 'S' (
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	    (
	      <hadoop_streaming_lambda>
	      <empty>?
	    ) -> {$$_[0]}
	  ) -> {hadoop_streaming_op @$_}
	)

# PARSER dsp/resourcealt

## DEFINITION
	(
	| ''file-closure://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "file-closure://$_"}
	| ''file://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "file://$_"}
	| ''hdfs://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "hdfs://$_"}
	| ''hdfst://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "hdfst://$_"}
	| ''http://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "http://$_"}
	| ''https://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "https://$_"}
	| ''s3cmd://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "s3cmd://$_"}
	| ''sftp://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "sftp://$_"}
	| 'file-closure://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "file-closure://$_"}
	| 'file://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "file://$_"}
	| 'hdfs://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "hdfs://$_"}
	| 'hdfst://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "hdfst://$_"}
	| 'http://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "http://$_"}
	| 'https://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "https://$_"}
	| 's3cmd://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "s3cmd://$_"}
	| 'sftp://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "sftp://$_"}
	)

# PARSER dsp/sparkprofile

## DEFINITION
	(
	| 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                               file_read_op,
	                               row_match_op '/part-']}
	| 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	)

# PARSER dsp/splitalt

## DEFINITION
	(
	| '/' <regex> -> {split_regex_op $_}
	| ':' /./ -> {split_chr_op   $_}
	| 'C' '' -> {split_chr_op   ','}
	| 'P' '' -> {split_chr_op   '|'}
	| 'S' '' -> {split_regex_op '\s+'}
	| 'V' '' -> {split_proper_csv_op}
	| 'W' '' -> {split_regex_op '[^\w\n]+'}
	| 'm' (
	    '/'
	    <regex> -> {scan_regex_op $_}
	  ) -> {$$_[1]}
	)

# PARSER dsp/sqlprofile

## DEFINITION
	(
	| 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	)

# PARSER filename
	The name of an existing file

## DEFINITION
	(
	| /file://(.+)/
	| /\.?/(?:[^/]|$)[^]]*/
	| /[^][]+/ such that {-e}
	)

# PARSER float

## DEFINITION
	/-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:[eE][-+]?\d+)?/ such that {length} -> {0 + $_}

# PARSER fn_expander

## DEFINITION
	<core parser {
	  
	        my ($self, @xs) = @_;
	        my (undef, $context, $formals, $positions, $expansion) = @$self;
	        my ($parsed_formals, @rest) = parse pn(1, popt pempty, $formals), @xs;
	        return () unless defined $parsed_formals;
	        my %args;
	        $args{$_} = $$parsed_formals[$$positions{$_}] for keys %$positions;
	        parse parser "$context/op",
	              evaluate_fn_expansion(%args, @$expansion), @rest;
	      
	}>

# PARSER generic_code
	Counts brackets outside quoted strings, which in our case are '' and "".
	Doesn't look for regular expressions because these vary by language; but this
	parser should be able to handle most straightforward languages with quoted
	string literals and backslash escapes.

## DEFINITION
	<core parser {
	  my ($self, $code, @xs) = @_;
	      return ($code, '', @xs) unless $code =~ /\]$/;
	      (my $tcode = $code) =~ s/"([^"\\]+|\\.)"|'([^'\\]+|\\.)'//g;
	      my $balance = length(sgr $tcode, qr/[^[]/, '') - length(sgr $tcode, qr/[^]]/, '');
	      $balance ? (substr($code, 0, $balance), substr($code, $balance), @xs)
	               : ($code, '', @xs)
	}>

# PARSER gnuplot/lambda
	A bracketed lambda function in context 'gnuplot'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <gnuplot/series>
	  ']'
	) -> {$$_[1]}

# PARSER gnuplot/op
	A single operator in the context 'gnuplot'

## DEFINITION
	(
	| <gnuplot/short>
	)

# PARSER gnuplot/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <gnuplot/lambda>
	| <gnuplot/suffix>
	)

# PARSER gnuplot/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <gnuplot/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER gnuplot/short
	Dispatch table for short options in context 'gnuplot'

## DEFINITION
	(
	| 'd' <'', evaluate as plot "-" with dots>
	)

# PARSER gnuplot/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<gnuplot/op>*

# PARSER hadoop_streaming_lambda

## DEFINITION
	(
	| (
	    /_/
	    <empty>?
	  ) -> {$$_[0]} -> {undef}
	| (
	    /:/
	    <empty>?
	  ) -> {$$_[0]} -> {[]}
	| </qfn>
	)

# PARSER id_text

## DEFINITION
	(
	| <multiword_ws> -> {join "\t", @$_}
	| <multiword> -> {join "\t", @$_}
	| /[^][]+/
	)

# PARSER integer

## DEFINITION
	(
	| <neval> -> {int}
	| /E(-?\d+)/ -> {10 ** $_}
	| /B(\d+)/ -> {1 << $_}
	| /x[0-9a-fA-F]+/ -> {0 + "0$_"}
	| /-?[1-9]\d*(?:[eE]\d+)?/ -> {0 + $_}
	| '0'
	)

# PARSER jitter_bias

## DEFINITION
	<number>? -> {dor $_, 0}

# PARSER jitter_mag

## DEFINITION
	(
	| /,/ -> {0.9}
	| <number>?
	) -> {$_ || 1}

# PARSER lispcode

## DEFINITION
	(
	  /.*[^]]+/
	  <empty>?
	) -> {$$_[0]}

# PARSER log_base

## DEFINITION
	<number>? -> {$_ || exp 1}

# PARSER multiword
	A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
	metacharacters within the arguments won't be expanded). If you use this form,
	no ARGV entry can end in a closing bracket; otherwise ni will assume you wanted
	to close the list.

## DEFINITION
	(
	  /\[/
	  (
	    /.*[^]]/
	    <empty>?
	  ) -> {$$_[0]}+
	  /\]/
	) -> {$$_[1]}

# PARSER multiword_ws
	A bracketed list of arguments to exec(), interpreted verbatim (i.e. shell
	metacharacters within the arguments won't be expanded). Whitespace is required
	around both brackets.

## DEFINITION
	(
	  (
	    /\[$/
	    <empty>?
	  ) -> {$$_[0]}
	  !/\]$/+
	  /\]$/
	) -> {$$_[1]}

# PARSER nefilename
	The name of a possibly-nonexisting file

## DEFINITION
	(
	| <filename>
	| /[^][]+/
	)

# PARSER neval
	An expression evaluated by Perl; e.g. =3+4 for 7

## DEFINITION
	/=([^]=]+)/ -> {eval}

# PARSER number

## DEFINITION
	(
	| <neval>
	| <float>
	| <integer>
	)

# PARSER paltr

## DEFINITION
	<core parser {
	  my ($self, @xs, @ps, @r) = @_;
	        @r = parse $_, @xs and return @r for @ps = @{parser $$self[1]}; ()
	}>

# PARSER pcond

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my (undef, $f, $p) = @$self;
	        $f = fn $f;
	        my @xs = parse $p, @is; @xs && &$f($_ = $xs[0]) ? @xs : ()
	}>

# PARSER pdspr

## DEFINITION
	<core parser {
	  my ($self, $x, @xs, $k, @ys, %ls, $c) = @_;
	        my (undef, $ps) = @$self;
	        return () unless defined $x;
	        ++$ls{length $_} for keys %$ps;
	        for my $l (sort {$b <=> $a} keys %ls) {
	          return (@ys = parse $$ps{$c}, substr($x, $l), @xs) ? @ys : ()
	          if exists $$ps{$c = substr $x, 0, $l} and $l <= length $x;
	        }
	        ()
	}>

# PARSER pempty

## DEFINITION
	<core parser {
	  defined $_[1] && length $_[1] ? () : (0, @_[2..$#_])
	}>

# PARSER pend

## DEFINITION
	<core parser {
	  @_ > 1                        ? () : (0)
	}>

# PARSER perl_asserter_code

## DEFINITION
	<plcode ni::perl_asserter>

# PARSER perl_cell_transform_code

## DEFINITION
	<plcode ni::perl_mapper>

# PARSER perl_grepper_code

## DEFINITION
	<plcode ni::perl_grepper>

# PARSER perl_mapper_code

## DEFINITION
	<plcode ni::perl_mapper>

# PARSER pk

## DEFINITION
	<core parser {
	  (${$_[0]}[1], @_[1..$#_])
	}>

# PARSER plcode

## DEFINITION
	<core parser {
	  
	    return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
	    my ($self, $code, @xs) = @_;
	    my $safecode      = $code;
	    my $begin_warning = $safecode =~ s/BEGIN/ END /g;
	    my $codegen       = $$self[1];
	    my $status        = 0;
	    my $x             = '';
	    $x .= ']' while $status = syntax_check 'perl -c -', &$codegen($safecode)
	                    and ($safecode =~ s/\]$//, $code =~ s/\]$//);
	    die <<EOF if $status;
	  ni: failed to get closing bracket count for perl code "$code$x", possibly
	      because BEGIN-block metaprogramming is disabled when ni tries to figure
	      this out. To avoid this, make sure the shell argument containing your code
	      ends with something that isn't a closing bracket; e.g:
	      p'[[some code]]'            # this may fail due to bracket inference
	      p'[[some code]] '           # this works by bypassing it
	      [p'[some code] ' ]          # this works for ni lambdas
	  EOF
	    ($code, $x, @xs);
	}>

# PARSER pmap

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my (undef, $f, $p) = @$self;
	        $f = fn $f;
	        my @xs = parse $p, @is; @xs ? (&$f($_ = $xs[0]), @xs[1..$#xs]) : ()
	}>

# PARSER pnone

## DEFINITION
	<core parser {
	  (undef,       @_[1..$#_])
	}>

# PARSER pnx

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        !defined $x || $x =~ /^(?:$$self[1])/ ? () : ($x, @xs)
	}>

# PARSER popt

## DEFINITION
	<core parser {
	  my ($self, @is) = @_;
	        my @xs = parse $$self[1], @is; @xs ? @xs : (undef, @is)
	}>

# PARSER prep

## DEFINITION
	<core parser {
	  my ($self, @is, @c, @r) = @_;
	        my (undef, $p, $n) = (@$self, 0);
	        push @r, $_ while ($_, @is) = parse $p, (@c = @is);
	        @r >= $n ? (\@r, @c) : ()
	}>

# PARSER prx

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        defined $x && $x =~ s/^($$self[1])// ? (dor($2, $1), $x, @xs) : ()
	}>

# PARSER pseq

## DEFINITION
	<core parser {
	  my ($self, @is, $x, @xs, @ys) = @_;
	        my (undef, @ps) = @$self;
	        (($x, @is) = parse $_, @is) ? push @xs, $x : return () for @ps;
	        (\@xs, @is)
	}>

# PARSER pstr

## DEFINITION
	<core parser {
	  my ($self, $x, @xs) = @_;
	        defined $x && index($x, $$self[1]) == 0
	          ? ($$self[1], substr($x, length $$self[1]), @xs)
	          : ()
	}>

# PARSER pycode

## DEFINITION
	<generic_code> -> {pydent $_}

# PARSER pyspark/lambda
	A bracketed lambda function in context 'pyspark'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <pyspark/series>
	  ']'
	) -> {$$_[1]}

# PARSER pyspark/op
	A single operator in the context 'pyspark'

## DEFINITION
	(
	| <pyspark/short>
	)

# PARSER pyspark/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <pyspark/lambda>
	| <pyspark/suffix>
	)

# PARSER pyspark/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <pyspark/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER pyspark/short
	Dispatch table for short options in context 'pyspark'

## DEFINITION
	(
	| '*' <pyspark_rdd> -> {gen "%v.intersect($_)"}
	| '+' <pyspark_rdd> -> {gen "%v.union($_)"}
	| 'e' /([^]]+)/ -> {TODO(); gen "%v.pipe(" . pyquote($_) . ")"}
	| 'g' <'', evaluate as <opaque code reference>>
	| 'm' <pyspark_fn> -> {gen "%v.map(lambda x: $_)"}
	| 'n' <integer> -> {gen "%v.union(sc.parallelize(range(1, 1+$_)))"}
	| 'n0' <integer> -> {gen "%v.union(sc.parallelize(range($_)))"}
	| 'r' (
	  | <integer> -> {gen "%v.sample(False, $_)"}
	  | /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	  | <pyspark_fn> -> {gen "%v.filter($_)"}
	  )
	| 'u' <'', evaluate as <opaque code reference>>
	)

# PARSER pyspark/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<pyspark/op>*

# PARSER pyspark_fn

## DEFINITION
	<pycode> -> {pyspark_create_lambda $_}

# PARSER pyspark_rdd

## DEFINITION
	<pyspark/qfn> -> {pyspark_compile 'input', @$_}

# PARSER quant_spec

## DEFINITION
	<number>? -> {$_ || 1}

# PARSER rbcode

## DEFINITION
	<core parser {
	  
	    return $_[1], '', @_[2..$#_] unless $_[1] =~ /\]$/;
	    my ($self, $code, @xs) = @_;
	    my ($x, $status) = ('', 0);
	    $x .= ']' while $status = syntax_check 'ruby -c -', $code and $code =~ s/\]$//;
	    die <<EOF if $status;
	  ni: failed to get closing bracket count for ruby code "$code$x"; this means
	      your code has a syntax error.
	  EOF
	    ($code, $x, @xs);
	}>

# PARSER regex
	Regular expression, delimited by slashes

## DEFINITION
	/(?^:^(?:[^\\/]+|\\.)*/)/ -> {s/\/$//; $_}

# PARSER shell_command
	A quoted or bracketed shell command

## DEFINITION
	(
	| <multiword_ws> -> {shell_quote @$_}
	| <multiword> -> {shell_quote @$_}
	| /[^][]+/
	)

# PARSER sortspec

## DEFINITION
	(
	  <colspec1>
	  /[-gn]+/?
	)*

# PARSER sql/lambda
	A bracketed lambda function in context 'sql'

## DEFINITION
	(
	  (
	    '['
	    <empty>?
	  ) -> {$$_[0]}
	  <sql/series>
	  ']'
	) -> {$$_[1]}

# PARSER sql/op
	A single operator in the context 'sql'

## DEFINITION
	(
	| <sql/short>
	)

# PARSER sql/qfn
	Operators that are interpreted as a lambda, whether bracketed or written as a suffix

## DEFINITION
	(
	| <sql/lambda>
	| <sql/suffix>
	)

# PARSER sql/series
	A string of operators, possibly including whitespace

## DEFINITION
	(
	  <empty>?
	  <sql/op>
	  <empty>?
	) -> {$$_[1]}*

# PARSER sql/short
	Dispatch table for short options in context 'sql'

## DEFINITION
	(
	| '*' <sql_query> -> {['intersect',  $_]}
	| '+' <sql_query> -> {['union',      $_]}
	| '-' <sql_query> -> {['difference', $_]}
	| 'O' <sqlcode> -> {['order_by', "$_ DESC"]}
	| 'g' <sqlcode> -> {['order_by', $_]}
	| 'j' (
	  | (
	      'L'
	      <sql_query>
	    ) -> {$$_[1]} -> {['ljoin', $_]}
	  | (
	      'R'
	      <sql_query>
	    ) -> {$$_[1]} -> {['rjoin', $_]}
	  | (
	      'N'
	      <sql_query>
	    ) -> {$$_[1]} -> {['njoin', $_]}
	  | <sql_query> -> {['ijoin', $_]}
	  )
	| 'm' <sqlcode> -> {['map', $_]}
	| 'o' <sqlcode> -> {['order_by', "$_ ASC"]}
	| 'r' (
	  | <integer> -> {['take',   $_]}
	  | <sqlcode> -> {['filter', $_]}
	  )
	| 'u' <'', evaluate as [uniq]>
	)

# PARSER sql/suffix
	A string of operators unbroken by whitespace

## DEFINITION
	<sql/op>*

# PARSER sql_query

## DEFINITION
	(
	  <sql_table>
	  (
	  | <sql/lambda>
	  | <sql/suffix>
	  )?
	) -> {sql_compile $$_[0], @{$$_[1]}}

# PARSER sql_table

## DEFINITION
	(
	  /^[^][]*/
	  <empty>?
	) -> {$$_[0]} -> {sqlgen $_}

# PARSER sqlcode

## DEFINITION
	<generic_code>

# PARSER ssh_host

## DEFINITION
	/[^][/,]+/
