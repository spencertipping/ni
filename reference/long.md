
# LONG OPERATOR /fs
	Append things that appear to be files

## SYNTAX
	<filename> -> {cat_op $_}

# LONG OPERATOR /resource

## SYNTAX
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
	| 'sqlitet://' /.*/ -> {resource_append_op "sqlitet://$_"}
	| 'tar://' /.*/ -> {resource_append_op "tar://$_"}
	| 'tarentry://' /.*/ -> {resource_append_op "tarentry://$_"}
	| 'xlsx://' /.*/ -> {resource_append_op "xlsx://$_"}
	| 'xlsxsheet://' /.*/ -> {resource_append_op "xlsxsheet://$_"}
	| 'zip://' /.*/ -> {resource_append_op "zip://$_"}
	| 'zipentry://' /.*/ -> {resource_append_op "zipentry://$_"}
	)
