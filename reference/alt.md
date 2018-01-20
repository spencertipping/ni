
# EXTENSIBLE LIST colalt
	list of alternatives for /f field-select operator

## OPTIONS
	(
	| <colspec> -> {cols_op @$_}
	)

# EXTENSIBLE LIST dockeralt
	alternatives for the /C containerize operator

## OPTIONS
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

# EXTENSIBLE LIST perlalt
	alternatives for /p perl operator

## OPTIONS
	(
	| <perl_mapper_code> -> {perl_mapper_op $_}
	)

# EXTENSIBLE LIST pysparkrowalt
	alternatives for pyspark/r row operator

## OPTIONS
	(
	| <integer> -> {gen "%v.sample(False, $_)"}
	| /\.(\d+)/ -> {gen "%v.takeSample(False, $_)"}
	| <pyspark_fn> -> {gen "%v.filter($_)"}
	)

# EXTENSIBLE LIST rowalt
	alternatives for the /r row operator

## OPTIONS
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
	    '^b'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {bloom_rows_op 0, @$_}
	| (
	    'b'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {bloom_rows_op 1, @$_}
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
	    /s/
	    <number>
	  ) -> {$$_[1]} -> {safe_head_op  $_}
	| (
	    /x/
	    <number>
	  ) -> {$$_[1]} -> {row_every_op  $_}
	| (
	    /x/
	    <colspec1>
	  ) -> {$$_[1]} -> {row_repeat_op $_}
	| (
	    ///
	    <regex>
	  ) -> {$$_[1]} -> {row_match_op  $_}
	| /\.\d+/ -> {row_sample_op $_}
	| <integer> -> {head_op '-n', 0 + $_}
	| (
	    'i'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 1, @$_}
	| (
	    'I'
	    <colspec1>
	    </qfn>
	  ) -> {[@$_[1,2]]} -> {row_include_or_exclude_exact_op 0, @$_}
	| <colspec_fixed> -> {row_cols_defined_op @$_}
	)

# EXTENSIBLE LIST rubyalt
	alternatives for the /m ruby operator

## OPTIONS
	(
	| <rbcode> -> {ruby_mapper_op $_}
	)

# EXTENSIBLE LIST scalealt
	row scaling alternation list

## OPTIONS
	(
	| (
	    <integer>
	    </qfn>
	  ) -> {row_fixed_scale_op @$_}
	)

# EXTENSIBLE LIST sortalt
	alternatives for the /g row operator

## OPTIONS
	(
	| (
	    /_/
	    <integer>
	  ) -> {$$_[1]} -> {partial_sort_op               $_}
	| <sortspec> -> {row_sort_op        sort_args @$_}
	)

# EXTENSIBLE LIST sqljoinalt
	alternatives for sql/j join operator

## OPTIONS
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

# EXTENSIBLE LIST sqlrowalt
	alternatives for sql/r row operator

## OPTIONS
	(
	| <integer> -> {['take',   $_]}
	| <sqlcode> -> {['filter', $_]}
	)
