
# LONG OPERATOR /fs
	Append things that appear to be files

## SYNTAX
	<filename> -> {cat_op $_}

# LONG OPERATOR /resource

## SYNTAX
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
