
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
	| 'p' <plcode ni::binary_perl_mapper> -> {binary_perl_op $_}
	)

# EXTENSIBLE DISPATCH TABLE bufferalt
	dispatch table for /B buffer operator

## OPTIONS
	(
	| 'n' '' -> {buffer_null_op}
	)

# EXTENSIBLE DISPATCH TABLE hadoopalt
	hadoop job dispatch table

## OPTIONS
	(
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
