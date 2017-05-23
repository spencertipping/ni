
# SHORT OPERATOR /!

## SYNTAX
	(
	| 'p' <perl_asserter_code> -> {perl_assert_op $_}
	)

# SHORT OPERATOR /$hadoop/jobconf

## SYNTAX
	'' -> {conf_get_op 'hadoop/jobconf'}

# SHORT OPERATOR /$hadoop/jobname

## SYNTAX
	'' -> {conf_get_op 'hadoop/jobname'}

# SHORT OPERATOR /$hadoop/name

## SYNTAX
	'' -> {conf_get_op 'hadoop/name'}

# SHORT OPERATOR /$hadoop/streaming-jar

## SYNTAX
	'' -> {conf_get_op 'hadoop/streaming-jar'}

# SHORT OPERATOR /$hdfs/tmpdir

## SYNTAX
	'' -> {conf_get_op 'hdfs/tmpdir'}

# SHORT OPERATOR /$image_command

## SYNTAX
	'' -> {conf_get_op 'image_command'}

# SHORT OPERATOR /$monitor

## SYNTAX
	'' -> {conf_get_op 'monitor'}

# SHORT OPERATOR /$pager

## SYNTAX
	'' -> {conf_get_op 'pager'}

# SHORT OPERATOR /$row/seed

## SYNTAX
	'' -> {conf_get_op 'row/seed'}

# SHORT OPERATOR /$row/sort-buffer

## SYNTAX
	'' -> {conf_get_op 'row/sort-buffer'}

# SHORT OPERATOR /$row/sort-compress

## SYNTAX
	'' -> {conf_get_op 'row/sort-compress'}

# SHORT OPERATOR /$row/sort-parallel

## SYNTAX
	'' -> {conf_get_op 'row/sort-parallel'}

# SHORT OPERATOR /$tmpdir

## SYNTAX
	'' -> {conf_get_op 'tmpdir'}

# SHORT OPERATOR /%

## SYNTAX
	(
	  <number>?
	  </qfn>
	) -> {interleave_op @$_}

# SHORT OPERATOR /'

## SYNTAX
	(
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

# SHORT OPERATOR /+

## SYNTAX
	</qfn> -> {append_op    @$_}

# SHORT OPERATOR /,

## SYNTAX
	(
	| <cell/lambda>
	| <cell/suffix>
	)

# SHORT OPERATOR /--dev/backdoor

## SYNTAX
	/.*/ -> {dev_backdoor_op $_}

# SHORT OPERATOR /--dev/local-operate

## SYNTAX
	</qfn> -> {dev_local_operate_op $_}

# SHORT OPERATOR /--http/wse

## SYNTAX
	'' -> {http_websocket_encode_op}

# SHORT OPERATOR /--http/wse-batch

## SYNTAX
	<integer>? -> {http_websocket_encode_batch_op $_}

# SHORT OPERATOR ///:

## SYNTAX
	(
	  <closure_name>
	  <empty>?
	) -> {$$_[0]} -> {memory_closure_append_op $_}

# SHORT OPERATOR ///@

## SYNTAX
	(
	  <closure_name>
	  <empty>?
	) -> {$$_[0]} -> {file_closure_append_op $_}

# SHORT OPERATOR ///help

## SYNTAX
	//(.*)/? -> {meta_help_op $_}

# SHORT OPERATOR ///license

## SYNTAX
	'' -> {meta_key_op 'license'}

# SHORT OPERATOR ///ni

## SYNTAX
	'' -> {meta_image_op}

# SHORT OPERATOR ///ni/

## SYNTAX
	(
	  /[^][]+$/
	  <empty>?
	) -> {$$_[0]} -> {meta_key_op $_}

# SHORT OPERATOR ///ni/conf

## SYNTAX
	'' -> {meta_conf_op}

# SHORT OPERATOR ///ni/eval/

## SYNTAX
	<integer> -> {meta_eval_number_op $_}

# SHORT OPERATOR ///ni/keys

## SYNTAX
	'' -> {meta_keys_op}

# SHORT OPERATOR ///ni/map/short

## SYNTAX
	'' -> {meta_short_availability_op}

# SHORT OPERATOR ///ni/op/

## SYNTAX
	(
	  /.+/
	  <empty>?
	) -> {$$_[0]} -> {meta_op_op $_}

# SHORT OPERATOR ///ni/ops

## SYNTAX
	'' -> {meta_ops_op}

# SHORT OPERATOR ///ni/options

## SYNTAX
	'' -> {meta_options_op}

# SHORT OPERATOR ///ni/parser/

## SYNTAX
	(
	  /.+/
	  <empty>?
	) -> {$$_[0]} -> {meta_parser_op $_}

# SHORT OPERATOR ///ni/parsers

## SYNTAX
	'' -> {meta_parsers_op}

# SHORT OPERATOR /1
	Alias for 'n1'

## SYNTAX
	'' -> {n_op 1, 2}

# SHORT OPERATOR /:

## SYNTAX
	(
	  <nefilename>
	  <empty>?
	) -> {$$_[0]} -> {inline_checkpoint_op $_}

# SHORT OPERATOR /::

## SYNTAX
	(
	  (
	    <closure_name>
	    <empty>?
	  ) -> {$$_[0]}
	  </qfn>
	) -> {memory_data_closure_op @$_}

# SHORT OPERATOR /:@

## SYNTAX
	(
	  (
	    <closure_name>
	    <empty>?
	  ) -> {$$_[0]}
	  </qfn>
	) -> {file_data_closure_op @$_}

# SHORT OPERATOR /<

## SYNTAX
	'' -> {file_read_op}

# SHORT OPERATOR /=

## SYNTAX
	</qfn> -> {divert_op    @$_}

# SHORT OPERATOR />

## SYNTAX
	<nefilename> -> {file_write_op $_}

# SHORT OPERATOR />'R

## SYNTAX
	'' -> {encode_resource_stream_op}

# SHORT OPERATOR /B

## SYNTAX
	(
	| 'n' '' -> {buffer_null_op}
	)

# SHORT OPERATOR /C

## SYNTAX
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

# SHORT OPERATOR /D

## SYNTAX
	<generic_code> -> {destructure_op $_}

# SHORT OPERATOR /E

## SYNTAX
	(
	  (
	    /[^][]+/
	    <empty>?
	  ) -> {$$_[0]}
	  </qfn>
	) -> {docker_exec_op $$_[0], @{$$_[1]}}

# SHORT OPERATOR /F

## SYNTAX
	(
	| '/' <regex> -> {split_regex_op $_}
	| ':' /./ -> {split_chr_op   $_}
	| 'C' '' -> {split_chr_op   ','}
	| 'D' '' -> {split_chr_op   '\/'}
	| 'P' '' -> {split_chr_op   '|'}
	| 'S' '' -> {split_regex_op '\s+'}
	| 'V' '' -> {split_proper_csv_op}
	| 'W' '' -> {split_regex_op '[^\w\n]+'}
	| 'm' (
	    '/'
	    <regex> -> {scan_regex_op $_}
	  ) -> {$$_[1]}
	)

# SHORT OPERATOR /G

## SYNTAX
	(
	  <gnuplot_colspec>
	  <gnuplot_code>
	) -> {stream_to_gnuplot_op @$_}

# SHORT OPERATOR /GF

## SYNTAX
	<shell_command> -> {sh_op "ffmpeg -f image2pipe -i - $_"}

# SHORT OPERATOR /H

## SYNTAX
	(
	| '#' '' -> {hadoop_make_nukeable_op}
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
	| 'R' (
	    <number>
	    <empty>?
	  ) -> {$$_[0]} -> {configure_op {'hadoop/jobconf' => "mapred.reduce.tasks=$_"},
	                        [hadoop_streaming_op [], undef, []]}
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

# SHORT OPERATOR /I

## SYNTAX
	</qfn> -> {each_image_op $_}

# SHORT OPERATOR /IC

## SYNTAX
	(
	  (
	    <image_command>
	    <empty>?
	  ) -> {$$_[0]}
	  (
	    <image_command>
	    <empty>?
	  ) -> {$$_[0]}
	  (
	    <image_command>
	    <empty>?
	  ) -> {$$_[0]}
	) -> {composite_images_op @$_}

# SHORT OPERATOR /IJ

## SYNTAX
	'' -> {each_image_op [sh_op "convert - jpg:-"]}

# SHORT OPERATOR /N

## SYNTAX
	(
	  <colspec1>?
	  <pycode>
	) -> {numpy_dense_op @$_}

# SHORT OPERATOR /O

## SYNTAX
	<sortspec> -> {row_sort_op '-rn', sort_args @$_}

# SHORT OPERATOR /P

## SYNTAX
	(
	| 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                               file_read_op,
	                               row_match_op '/part-']}
	| 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	)

# SHORT OPERATOR /Q

## SYNTAX
	(
	| 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	)

# SHORT OPERATOR /S

## SYNTAX
	(
	| (
	    <integer>
	    </qfn>
	  ) -> {row_fixed_scale_op @$_}
	)

# SHORT OPERATOR /W

## SYNTAX
	</qfn> -> {with_left_op  @$_}

# SHORT OPERATOR /X

## SYNTAX
	<colspec1>? -> {sparse_to_dense_op $_}

# SHORT OPERATOR /Y

## SYNTAX
	<colspec1>? -> {dense_to_sparse_op $_}

# SHORT OPERATOR /Z

## SYNTAX
	<integer> -> {unflatten_op 0 + $_}

# SHORT OPERATOR /^

## SYNTAX
	</qfn> -> {prepend_op   @$_}

# SHORT OPERATOR /^{

## SYNTAX
	(
	  <config_option_map>
	  </qfn>
	) -> {configure_op @$_}

# SHORT OPERATOR /b

## SYNTAX
	(
	| 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	)

# SHORT OPERATOR /c

## SYNTAX
	'' -> {count_op}

# SHORT OPERATOR /e
	Exec shell command as a filter for the current stream

## SYNTAX
	<shell_command> -> {sh_op $_}

# SHORT OPERATOR /f

## SYNTAX
	(
	| <colspec> -> {cols_op @$_}
	)

# SHORT OPERATOR /f[

## SYNTAX
	(
	  <empty>?
	  <fn_bindings>
	  </series>
	  ']'
	) -> {[@$_[1,2]]} -> {op_fn_op @$_}

# SHORT OPERATOR /g

## SYNTAX
	(
	| (
	    /_/
	    <integer>
	  ) -> {$$_[1]} -> {partial_sort_op               $_}
	| <sortspec> -> {row_sort_op        sort_args @$_}
	)

# SHORT OPERATOR /gg

## SYNTAX
	(
	  <colspec1>
	  <sortspec>
	) -> {row_grouped_sort_op @$_}

# SHORT OPERATOR /i
	Identity: append literal text

## SYNTAX
	<id_text> -> {echo_op $_}

# SHORT OPERATOR /j

## SYNTAX
	(
	  <colspec>?
	  </qfn>
	) -> {join_op $$_[0] || [1, 0], $$_[0] || [1, 0], $$_[1]}

# SHORT OPERATOR /l

## SYNTAX
	<lispcode> -> {lisp_code_op lisp_mapgen->(prefix => lisp_prefix,
	                                                 body   => $_)}

# SHORT OPERATOR /l[

## SYNTAX
	(
	  <empty>?
	  <let_bindings>
	  </series>
	  ']'
	) -> {[@$_[1,2]]} -> {op_let_op @$_}

# SHORT OPERATOR /m

## SYNTAX
	(
	| <rbcode> -> {ruby_mapper_op $_}
	)

# SHORT OPERATOR /n
	Append integers 1..N, or 1..infinity if N is unspecified

## SYNTAX
	<number>? -> {n_op 1, defined $_ ? $_ + 1 : -1}

# SHORT OPERATOR /n0
	Append integers 0..N-1, or 0..infinity if N is unspecified

## SYNTAX
	<number>? -> {n_op 0, defined $_ ? $_ : -1}

# SHORT OPERATOR /o

## SYNTAX
	<sortspec> -> {row_sort_op '-n',  sort_args @$_}

# SHORT OPERATOR /p

## SYNTAX
	(
	| <perl_mapper_code> -> {perl_mapper_op $_}
	)

# SHORT OPERATOR /r

## SYNTAX
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

# SHORT OPERATOR /s

## SYNTAX
	(
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

# SHORT OPERATOR /u

## SYNTAX
	'' -> {uniq_op}

# SHORT OPERATOR /v

## SYNTAX
	(
	  <colspec_fixed>
	  </qfn>
	) -> {vertical_apply_op @$_}

# SHORT OPERATOR /w

## SYNTAX
	</qfn> -> {with_right_op @$_}

# SHORT OPERATOR /x

## SYNTAX
	<colspec>? -> {ref $_ ? colswap_op @$_ : colswap_op 2, 1}

# SHORT OPERATOR /z

## SYNTAX
	<compressor_spec>

# SHORT OPERATOR /zd

## SYNTAX
	<'', evaluate as [decode]>

# SHORT OPERATOR /zn

## SYNTAX
	<'', evaluate as [sink_null]>

# SHORT OPERATOR cell/H

## SYNTAX
	(
	  <cellspec_fixed>
	  <integer>?
	) -> {real_hash_op      @$_}

# SHORT OPERATOR cell/a

## SYNTAX
	<cellspec_fixed> -> {col_average_op $_}

# SHORT OPERATOR cell/d

## SYNTAX
	<cellspec_fixed> -> {col_delta_op   $_}

# SHORT OPERATOR cell/e

## SYNTAX
	(
	  <cellspec_fixed>
	  <log_base>
	) -> {cell_exp_op @$_}

# SHORT OPERATOR cell/h

## SYNTAX
	(
	  <cellspec_fixed>
	  <integer>?
	) -> {intify_hash_op    @$_}

# SHORT OPERATOR cell/j

## SYNTAX
	(
	  <cellspec_fixed>
	  <jitter_mag>
	  <jitter_bias>
	) -> {jitter_uniform_op @$_}

# SHORT OPERATOR cell/l

## SYNTAX
	(
	  <cellspec_fixed>
	  <log_base>
	) -> {cell_log_op @$_}

# SHORT OPERATOR cell/p

## SYNTAX
	(
	  <colspec>
	  <perl_cell_transform_code>
	) -> {perl_cell_transformer_op @$_}

# SHORT OPERATOR cell/q

## SYNTAX
	(
	  <cellspec_fixed>
	  <quant_spec>
	) -> {quantize_op @$_}

# SHORT OPERATOR cell/s

## SYNTAX
	<cellspec_fixed> -> {col_sum_op     $_}

# SHORT OPERATOR cell/z

## SYNTAX
	<cellspec_fixed> -> {intify_compact_op $_}

# SHORT OPERATOR pyspark/*

## SYNTAX
	<pyspark_rdd> -> {gen "%v.intersect($_)"}

# SHORT OPERATOR pyspark/+

## SYNTAX
	<pyspark_rdd> -> {gen "%v.union($_)"}

# SHORT OPERATOR pyspark/e

## SYNTAX
	/([^]]+)/ -> {TODO(); gen "%v.pipe(" . pyquote($_) . ")"}

# SHORT OPERATOR pyspark/g

## SYNTAX
	<'', evaluate as <opaque code reference>>

# SHORT OPERATOR pyspark/m

## SYNTAX
	<pyspark_fn> -> {gen "%v.map(lambda x: $_)"}

# SHORT OPERATOR pyspark/n

## SYNTAX
	<integer> -> {gen "%v.union(sc.parallelize(range(1, 1+$_)))"}

# SHORT OPERATOR pyspark/n0

## SYNTAX
	<integer> -> {gen "%v.union(sc.parallelize(range($_)))"}

# SHORT OPERATOR pyspark/r

## SYNTAX
	(
	| <integer> -> {gen "%v.sample(False, $_)"}
	| /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	| <pyspark_fn> -> {gen "%v.filter($_)"}
	)

# SHORT OPERATOR pyspark/u

## SYNTAX
	<'', evaluate as <opaque code reference>>

# SHORT OPERATOR sql/*

## SYNTAX
	<sql_query> -> {['intersect',  $_]}

# SHORT OPERATOR sql/+

## SYNTAX
	<sql_query> -> {['union',      $_]}

# SHORT OPERATOR sql/-

## SYNTAX
	<sql_query> -> {['difference', $_]}

# SHORT OPERATOR sql/O

## SYNTAX
	<sqlcode> -> {['order_by', "$_ DESC"]}

# SHORT OPERATOR sql/g

## SYNTAX
	<sqlcode> -> {['order_by', $_]}

# SHORT OPERATOR sql/j

## SYNTAX
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

# SHORT OPERATOR sql/m

## SYNTAX
	<sqlcode> -> {['map', $_]}

# SHORT OPERATOR sql/o

## SYNTAX
	<sqlcode> -> {['order_by', "$_ ASC"]}

# SHORT OPERATOR sql/r

## SYNTAX
	(
	| <integer> -> {['take',   $_]}
	| <sqlcode> -> {['filter', $_]}
	)

# SHORT OPERATOR sql/u

## SYNTAX
	<'', evaluate as [uniq]>
