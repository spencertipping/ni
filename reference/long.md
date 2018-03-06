
# LONG OPERATOR /fs
	Append things that appear to be files

## SYNTAX
	<filename> -> {cat_op $_}

# LONG OPERATOR /resource

## SYNTAX
	(
	| ''file-closure://' /.*/ -> {resource_quote_op "file-closure://$_"}
	| ''file://' /.*/ -> {resource_quote_op "file://$_"}
	| ''git://' /.*/ -> {resource_quote_op "git://$_"}
	| ''gitblob://' /.*/ -> {resource_quote_op "gitblob://$_"}
	| ''gitcommit://' /.*/ -> {resource_quote_op "gitcommit://$_"}
	| ''gitcommitmeta://' /.*/ -> {resource_quote_op "gitcommitmeta://$_"}
	| ''gitdiff://' /.*/ -> {resource_quote_op "gitdiff://$_"}
	| ''githistory://' /.*/ -> {resource_quote_op "githistory://$_"}
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
	| 'file-closure://' /.*/ -> {resource_append_op "file-closure://$_"}
	| 'file://' /.*/ -> {resource_append_op "file://$_"}
	| 'git://' /.*/ -> {resource_append_op "git://$_"}
	| 'gitblob://' /.*/ -> {resource_append_op "gitblob://$_"}
	| 'gitcommit://' /.*/ -> {resource_append_op "gitcommit://$_"}
	| 'gitcommitmeta://' /.*/ -> {resource_append_op "gitcommitmeta://$_"}
	| 'gitdiff://' /.*/ -> {resource_append_op "gitdiff://$_"}
	| 'githistory://' /.*/ -> {resource_append_op "githistory://$_"}
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
	)
