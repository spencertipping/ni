
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

# EXTENSIBLE DISPATCH TABLE combinealt
	dispatch table for /F^ combine operator

## OPTIONS
	(
	| ':' /./ -> {sh_op 'tr "\t" "'.$_.'"'}
	| 'C' '' -> {sh_op 'tr "\t" ,'}
	| 'P' '' -> {sh_op 'tr "\t" "|"'}
	| 'S' '' -> {sh_op 'tr "\t" " "'}
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
	| 'Q' 'P'? -> {"set terminal qt persist;"}
	| 'W' 'P'? -> {"set terminal wx persist;"}
	| 'X' 'P'? -> {"set terminal x11 persist;"}
	)

# EXTENSIBLE DISPATCH TABLE hadoopalt
	hadoop job dispatch table

## OPTIONS
	(
	| '#' '' -> {hadoop_make_nukeable_op}
	| 'DS' (
	    <empty>?
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
	  ) -> {my (undef, $m, $c, $r) = @$_;
	                            my @cr =
	                              (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
	                               defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
	                            [@$m, @cr]}
	| 'R' (
	    <number>
	    <empty>?
	  ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                        [hadoop_streaming_op [], undef, []]}
	| 'RR' (
	    <number>
	    <empty>?
	  ) -> {$$_[0]} -> {configure_op {'Hjr' => "$_"},
	                        [hadoop_streaming_op
	                          [perl_mapper_op 'print "$.\t$_\n";()'],
	                          undef,
	                          [cols_op 2, 1, -1]]}
	| 'S' (
	    <empty>?
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
	  ) -> {hadoop_streaming_op @$_[1..$#$_]}
	| 'T' (
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
	  ) -> {hadoop_test_op @$_}
	)

# EXTENSIBLE DISPATCH TABLE resourcealt
	dispatch table for URI prefixes

## OPTIONS
	(
	| ''7z://' /.*/ -> {resource_quote_op "7z://$_"}
	| ''7zentry://' /.*/ -> {resource_quote_op "7zentry://$_"}
	| ''file-closure://' /.*/ -> {resource_quote_op "file-closure://$_"}
	| ''file://' /.*/ -> {resource_quote_op "file://$_"}
	| ''git://' /.*/ -> {resource_quote_op "git://$_"}
	| ''gitblob://' /.*/ -> {resource_quote_op "gitblob://$_"}
	| ''gitcommit://' /.*/ -> {resource_quote_op "gitcommit://$_"}
	| ''gitcommitmeta://' /.*/ -> {resource_quote_op "gitcommitmeta://$_"}
	| ''gitdiff://' /.*/ -> {resource_quote_op "gitdiff://$_"}
	| ''githistory://' /.*/ -> {resource_quote_op "githistory://$_"}
	| ''gitnmhistory://' /.*/ -> {resource_quote_op "gitnmhistory://$_"}
	| ''gitpdiff://' /.*/ -> {resource_quote_op "gitpdiff://$_"}
	| ''gitsnap://' /.*/ -> {resource_quote_op "gitsnap://$_"}
	| ''gittree://' /.*/ -> {resource_quote_op "gittree://$_"}
	| ''hdfs://' /.*/ -> {resource_quote_op "hdfs://$_"}
	| ''hdfsc://' /.*/ -> {resource_quote_op "hdfsc://$_"}
	| ''hdfscname://' /.*/ -> {resource_quote_op "hdfscname://$_"}
	| ''hdfsj://' /.*/ -> {resource_quote_op "hdfsj://$_"}
	| ''hdfsjname://' /.*/ -> {resource_quote_op "hdfsjname://$_"}
	| ''hdfsrm://' /.*/ -> {resource_quote_op "hdfsrm://$_"}
	| ''hdfst://' /.*/ -> {resource_quote_op "hdfst://$_"}
	| ''http://' /.*/ -> {resource_quote_op "http://$_"}
	| ''https://' /.*/ -> {resource_quote_op "https://$_"}
	| ''s3cmd://' /.*/ -> {resource_quote_op "s3cmd://$_"}
	| ''sftp://' /.*/ -> {resource_quote_op "sftp://$_"}
	| ''sqlite://' /.*/ -> {resource_quote_op "sqlite://$_"}
	| ''sqliteq://' /.*/ -> {resource_quote_op "sqliteq://$_"}
	| ''sqlites://' /.*/ -> {resource_quote_op "sqlites://$_"}
	| ''sqlitet://' /.*/ -> {resource_quote_op "sqlitet://$_"}
	| ''tar://' /.*/ -> {resource_quote_op "tar://$_"}
	| ''tarentry://' /.*/ -> {resource_quote_op "tarentry://$_"}
	| ''xlsx://' /.*/ -> {resource_quote_op "xlsx://$_"}
	| ''xlsxsheet://' /.*/ -> {resource_quote_op "xlsxsheet://$_"}
	| ''zip://' /.*/ -> {resource_quote_op "zip://$_"}
	| ''zipentry://' /.*/ -> {resource_quote_op "zipentry://$_"}
	| '7z://' /.*/ -> {resource_append_op "7z://$_"}
	| '7zentry://' /.*/ -> {resource_append_op "7zentry://$_"}
	| 'file-closure://' /.*/ -> {resource_append_op "file-closure://$_"}
	| 'file://' /.*/ -> {resource_append_op "file://$_"}
	| 'git://' /.*/ -> {resource_append_op "git://$_"}
	| 'gitblob://' /.*/ -> {resource_append_op "gitblob://$_"}
	| 'gitcommit://' /.*/ -> {resource_append_op "gitcommit://$_"}
	| 'gitcommitmeta://' /.*/ -> {resource_append_op "gitcommitmeta://$_"}
	| 'gitdiff://' /.*/ -> {resource_append_op "gitdiff://$_"}
	| 'githistory://' /.*/ -> {resource_append_op "githistory://$_"}
	| 'gitnmhistory://' /.*/ -> {resource_append_op "gitnmhistory://$_"}
	| 'gitpdiff://' /.*/ -> {resource_append_op "gitpdiff://$_"}
	| 'gitsnap://' /.*/ -> {resource_append_op "gitsnap://$_"}
	| 'gittree://' /.*/ -> {resource_append_op "gittree://$_"}
	| 'hdfs://' /.*/ -> {resource_append_op "hdfs://$_"}
	| 'hdfsc://' /.*/ -> {resource_append_op "hdfsc://$_"}
	| 'hdfscname://' /.*/ -> {resource_append_op "hdfscname://$_"}
	| 'hdfsj://' /.*/ -> {resource_append_op "hdfsj://$_"}
	| 'hdfsjname://' /.*/ -> {resource_append_op "hdfsjname://$_"}
	| 'hdfsrm://' /.*/ -> {resource_append_op "hdfsrm://$_"}
	| 'hdfst://' /.*/ -> {resource_append_op "hdfst://$_"}
	| 'http://' /.*/ -> {resource_append_op "http://$_"}
	| 'https://' /.*/ -> {resource_append_op "https://$_"}
	| 's3cmd://' /.*/ -> {resource_append_op "s3cmd://$_"}
	| 'sftp://' /.*/ -> {resource_append_op "sftp://$_"}
	| 'sqlite://' /.*/ -> {resource_append_op "sqlite://$_"}
	| 'sqliteq://' /.*/ -> {resource_append_op "sqliteq://$_"}
	| 'sqlites://' /.*/ -> {resource_append_op "sqlites://$_"}
	| 'sqlitet://' /.*/ -> {resource_append_op "sqlitet://$_"}
	| 'tar://' /.*/ -> {resource_append_op "tar://$_"}
	| 'tarentry://' /.*/ -> {resource_append_op "tarentry://$_"}
	| 'xlsx://' /.*/ -> {resource_append_op "xlsx://$_"}
	| 'xlsxsheet://' /.*/ -> {resource_append_op "xlsxsheet://$_"}
	| 'zip://' /.*/ -> {resource_append_op "zip://$_"}
	| 'zipentry://' /.*/ -> {resource_append_op "zipentry://$_"}
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
