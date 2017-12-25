
# LONG OPERATOR /fs
	Append things that appear to be files

## SYNTAX
	<filename> -> {cat_op $_}

# LONG OPERATOR /resource

## SYNTAX
	(
	| ''file-closure://' /.*/ -> {resource_quote_op "file-closure://$_"}
	| ''file://' /.*/ -> {resource_quote_op "file://$_"}
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
	| 'file-closure://' /.*/ -> {resource_append_op "file-closure://$_"}
	| 'file://' /.*/ -> {resource_append_op "file://$_"}
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
	)
