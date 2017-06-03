
# EXTENSIBLE DISPATCH TABLE assertdsp
	dispatch table for the ! assertion operator

## OPTIONS
	(
	| 'p' <perl_asserter_code> -> {perl_assert_op $_}
	)

# EXTENSIBLE DISPATCH TABLE binaryalt
	dispatch table for the /b binary operator

## OPTIONS
	(
	| 'f' <generic_code> -> {binary_fixed_op $_}
	| 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	)

# EXTENSIBLE DISPATCH TABLE bufferalt
	dispatch table for /B buffer operator

## OPTIONS
	(
	| 'n' '' -> {buffer_null_op}
	)

# EXTENSIBLE DISPATCH TABLE gnuplot_code_prefixalt
	prefixes for gnuplot code

## OPTIONS
	(
	| '%d' <'', evaluate as plot "-" with dots >
	| '%i' <'', evaluate as plot "-" with impulses >
	| '%l' <'', evaluate as plot "-" with lines >
	| '%t' <generic_code> -> {"title '$_'"}
	| '%u' <generic_code> -> {"using $_"}
	| '%v' <'', evaluate as plot "-" with vectors >
	| 'J' <gnuplot_terminal_size> -> {"set terminal jpeg $_;"}
	| 'P' <gnuplot_terminal_size> -> {"set terminal png $_;"}
	| 'PC' <gnuplot_terminal_size> -> {"set terminal pngcairo $_;"}
	| 'QP' <'', evaluate as set terminal qt persist;>
	| 'WP' <'', evaluate as set terminal wx persist;>
	| 'XP' <'', evaluate as set terminal x11 persist;>
	)

# EXTENSIBLE DISPATCH TABLE hadoopalt
	hadoop job dispatch table

## OPTIONS
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

# EXTENSIBLE DISPATCH TABLE resourcealt
	dispatch table for URI prefixes

## OPTIONS
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
	| ''hdfsrm://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_quote_op "hdfsrm://$_"}
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
	| 'hdfsrm://' (
	    /.*/
	    <empty>?
	  ) -> {$$_[0]} -> {resource_append_op "hdfsrm://$_"}
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

# EXTENSIBLE DISPATCH TABLE sparkprofile
	dispatch for pyspark profiles

## OPTIONS
	(
	| 'L' <pyspark_rdd> -> {[pyspark_local_text_op($_),
	                               file_read_op,
	                               row_match_op '/part-']}
	| 'dev/compile' <pyspark_rdd> -> {pyspark_preview_op $_}
	)

# EXTENSIBLE DISPATCH TABLE splitalt
	dispatch table for /F split operator

## OPTIONS
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

# EXTENSIBLE DISPATCH TABLE sqlprofile
	dispatch for SQL profiles

## OPTIONS
	(
	| 'dev/compile' <sql_query> -> {sql_preview_op($_[0])}
	)
