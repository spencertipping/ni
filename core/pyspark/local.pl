# Local PySpark profile.
# Provides a way to stream data into and out of a context.

use constant pyspark_text_io_gen => gen pydent q{
  from pyspark import SparkConf, SparkContext
  %prefix
  conf = SparkConf().setAppName(%name).setMaster(%master)
  sc = SparkContext(conf=conf)
  if len(%input_path) > 0:
    input = sc.textFile(%input_path)
  else:
    input = sc.parallelize([])
  output = %body
  output.saveAsTextFile(%output_path)
};

defoperator pyspark_local_text => q{
  my ($fn) = @_;
  my $inpath   = join ',', map sr("file://$_", qr/\n$/, ''), <STDIN>;
  my $outpath  = "/tmp/ni-$$-out";
  my $tempfile = "/tmp/ni-$$-temp.py";
  safewrite swfile($tempfile),
    pyspark_text_io_gen->(
      master      => pyquote 'local[*]',
      name        => pyquote "ni $inpath -> $outpath",
      input_path  => pyquote $inpath,
      output_path => pyquote "file://$outpath",
      body        => $fn);
  local $SIG{CHLD} = 'DEFAULT';
  die "ni: pyspark failed with $_" if $_ = system 'spark-submit', $tempfile;
  print "$outpath\n";
};

defsparkprofile L => pmap q{[pyspark_local_text_op($_),
                             file_read_op,
                             row_match_op '/part-']}, pyspark_rdd;
