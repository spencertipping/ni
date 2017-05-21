# Pyspark interop.
# We need to define a context for CLI arguments so we can convert ni pipelines
# into pyspark code. This ends up being fairly straightforward because Spark
# provides so many high-level operators.

# There are two things going on here. First, we define the codegen for Spark
# jobs; this is fairly configuration-independent since the API is stable. Second,
# we define a configuration system that lets the user specify the Spark execution
# profile. This governs everything from `spark-submit` CLI options to
# SparkContext init.

# Pyspark operators.
# These exist in their own parsing context. Rather than compiling directly to
# Python code, we generate a series of gens, each of which refers to a '%v'
# quantity that signifies the value being transformed.

sub pyspark_compile {my $v = shift; $v = $_->(v => $v) for @_; $v}
sub pyspark_create_lambda($) {$_[0]}

BEGIN {defcontext 'pyspark', q{PySpark compilation context}}
BEGIN {
  defparseralias pyspark_fn  => pmap q{pyspark_create_lambda $_}, pycode;
  defparseralias pyspark_rdd => pmap q{pyspark_compile 'input', @$_}, pyspark_qfn;
}

defshort 'pyspark/n',  pmap q{gen "%v.union(sc.parallelize(range(1, 1+$_)))"}, integer;
defshort 'pyspark/n0', pmap q{gen "%v.union(sc.parallelize(range($_)))"}, integer;

defshort 'pyspark/e',
  pmap q{TODO(); gen "%v.pipe(" . pyquote($_) . ")"}, prx '([^]]+)';

defshort 'pyspark/m', pmap q{gen "%v.map(lambda x: $_)"}, pyspark_fn;
defshort 'pyspark/r',
  defalt 'pysparkrowalt', 'alternatives for pyspark/r row operator',
    pmap(q{gen "%v.sample(False, $_)"},     integer),
    pmap(q{gen "%v.takeSample(False, $_)"}, prx '\.(\d+)'),
    pmap(q{gen "%v.filter($_)"},            pyspark_fn);

defshort 'pyspark/g', pk gen "%v.sortByKey()";
defshort 'pyspark/u', pk gen "%v.distinct()";

defshort 'pyspark/+', pmap q{gen "%v.union($_)"},     pyspark_rdd;
defshort 'pyspark/*', pmap q{gen "%v.intersect($_)"}, pyspark_rdd;

# Configuration management.
# A profile contains the code required to initialize the SparkContext and any
# other variables relevant to the process. Each is referenced by a single
# character and stored in the %spark_profiles table.

defoperator pyspark_preview => q{sio; print "$_[0]\n"};

defshort '/P',
  defdsp 'sparkprofile', 'dispatch for pyspark profiles',
    'dev/compile' => pmap q{pyspark_preview_op $_}, pyspark_rdd;
