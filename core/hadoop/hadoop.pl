# Hadoop operator.
# The entry point for running various kinds of Hadoop jobs.

BEGIN {defshort '/H', defdsp 'hadoopalt', 'hadoop job dispatch table'}

defperlprefix "core/hadoop/hadoop-conf.pl";

our %mr_generics;

for (keys %mr_generics) {
  my $var_name = $mr_generics{$_};
  $var_name =~ tr/[a-z]\-./[A-Z]__/;
  my $env_var_name = 'NI_HADOOP_' . $var_name;
  defconfenv $_, $env_var_name => undef;
}

defconfenv 'hadoop/name',          NI_HADOOP               => 'hadoop';
defconfenv 'hadoop/streaming-jar', NI_HADOOP_STREAMING_JAR => undef;
defconfenv 'hadoop/jobconf', NI_HADOOP_JOBCONF => undef;
defconfenv 'hdfs/tmpdir', NI_HDFS_TMPDIR => '/tmp';

defresource 'hdfs',
  read   => q{soproc {exec conf 'hadoop/name', 'fs', '-cat', $_[1]} @_},
  write  => q{siproc {sh conf('hadoop/name') . " fs -put - " . shell_quote($_[1]) . " 1>&2"} @_},
  exists => q{local $_;
              my $fh = soproc {exec conf 'hadoop/name', 'fs', '-stat', $_[1]} @_;
              saferead $fh, $_, 8192;
              close $fh;
              !$fh->await},
  tmp    => q{"hdfs://" . conf('hdfs/tmpdir') . "/" . uri_temp_noise},
  nuke   => q{sh conf('hadoop/name') . ' fs -rm -r ' . shell_quote($_[1]) . " 1>&2"};

defresource 'hdfst',
  read => q{soproc {my $hadoop_name = conf 'hadoop/name';
                    my $path = shell_quote $_[1];
                    sh qq{$hadoop_name fs -text $path 2>/dev/null || $hadoop_name fs -text $path"/part*" 2>/dev/null}} @_},
  nuke => q{sh conf('hadoop/name') . ' fs -rm -r ' . shell_quote($_[1]) . " 1>&2"};

defresource 'hdfsrm',
  read => q{soproc {my $o = resource_read "hdfst://$_[1]";
                    sforward $o, \*STDOUT;
                    $o->await;
                    resource_nuke "hdfst://$_[1]"} @_},
  nuke => q{resource_nuke "hdfst://$_[1]"};

# Streaming.
# We need to be able to find the Streaming jar, which is slightly nontrivial. The
# hadoop docs suggest that $HADOOP_HOME has something to do with it, but I've
# seen installations that had no such environment variable and everything worked
# fine. Here's what we can do:

# | 1. Use $NI_HADOOP_STREAMING_JAR if it's set
#   2. Use `locate hadoop-streaming*.jar` if we have `locate`
#   3. Use `find /usr /opt -name hadoop-streaming*.jar`, see if it's there

# If those don't work, then we are officially SOL and you'll have to set
# NI_HADOOP_STREAMING_JAR.

sub hadoop_streaming_jar {
  local $SIG{CHLD} = 'DEFAULT';
  conf 'hadoop/streaming-jar'
  || (split /\n/, `locate 'hadoop-streaming*.jar' \\
                   || find /usr -name 'hadoop-streaming*.jar' \\
                   || find /opt -name 'hadoop-streaming*.jar'`)[0]
  || die "ni: cannot find hadoop streaming jar "
       . "(you can fix this by setting \$NI_HADOOP_STREAMING_JAR)";
}

# Input type autodetection.
# Technically, hadoop operators take one or more HFDS input paths on stdin -- but
# of course ni isn't going to just give up if we appear to have something else.
# If we have something that obviously isn't an HDFS path, we upload that stream
# into a temporary HDFS location and run against that.

sub hdfs_input_path {
  local $_;
  my $n;
  die "ni: hdfs_input_path: no data" unless $n = saferead \*STDIN, $_, 8192;
  if (/^\w+:\/\//) {
    $n = saferead \*STDIN, $_, 8192, length while $n;
    s/hdfst:\/\//hdfs:\/\//g;
    (0, map [split /\t/], grep length, split /\n/);
  } else {
    my $hdfs_tmp    = resource_tmp 'hdfs://';
    my $hdfs_writer = resource_write $hdfs_tmp;
    safewrite $hdfs_writer, $_;
    safewrite $hdfs_writer, $_ while saferead \*STDIN, $_, 8192;
    close $hdfs_writer;
    $hdfs_writer->await;
    (1, [$hdfs_tmp]);
  }
}

sub hadoop_lambda_file($$) {
  my ($name, $lambda) = @_;
  my $tmp = resource_tmp('file://') . $name;
  my $w   = resource_write $tmp;
  conf_set monitor => 0;
  safewrite $w, ni_quoted_image 1, @$lambda;
  sforward_quoted resource_read($_), $w for quoted_resources;
  close $w;
  ($tmp, ni_quoted_exec_args);
}

sub hadoop_embedded_cmd($@) {
  "sh -c " . shell_quote("cat " . shell_quote($_[0]) . " - | " . shell_quote(@_[1..$#_]));
}

sub hadoop_cmd_setup(@) {
  my ($map, $combine, $reduce) = @_;
  my ($nuke_inputs, @ipath) = hdfs_input_path;

  my ($mapper, @map_cmd) = hadoop_lambda_file 'mapper', $map;
  my ($combiner, @combine_cmd) = $combine
    ? hadoop_lambda_file 'combiner', $combine : ();
  my ($reducer, @reduce_cmd) = $reduce
    ? hadoop_lambda_file 'reducer', $reduce : ();

  my $streaming_jar = hadoop_streaming_jar;

  $mapper, \@map_cmd, $combiner, \@combine_cmd, 
    $reducer, \@reduce_cmd, $nuke_inputs, \@ipath, $streaming_jar;
}


sub make_hadoop_cmd($$$$$$$$$) {
  my ($mapper, $map_cmd_ref, $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref, $streaming_jar, $ipaths, $opath) = @_;
  $mapper   =~ s|^file://||;
  $combiner =~ s|^file://|| if $combiner;
  $reducer  =~ s|^file://|| if $reducer;

  my @map_cmd = @$map_cmd_ref;
  my @combine_cmd = @$combine_cmd_ref;
  my @reduce_cmd = @$reduce_cmd_ref;

  (my $mapper_file   = $mapper)         =~ s|.*/||;
  (my $combiner_file = $combiner || '') =~ s|.*/||;
  (my $reducer_file  = $reducer  || '') =~ s|.*/||;

  my @jobconf =
    grep $reducer || !/reduce/,             # HACK
    grep length, split /\s+/, dor conf 'hadoop/jobconf', '';

  my $job_name = "ni @$ipaths -> $opath";
  my $n_addl_paths = $#$ipaths; 
  my $first_path = $$ipaths[0];
  $job_name = "ni $first_path and $n_addl_paths others" .
              " -> $opath" if $n_addl_paths > 0;
  push @jobconf, "mapreduce.job.name=" . $job_name;
  
  my @ordered_jobconf = hadoop_generic_options(@jobconf);
 
  my $cmd = shell_quote
    conf 'hadoop/name',
    jar => $streaming_jar,
    -files  => join(",", grep defined, ($mapper, $combiner, $reducer)),
    @ordered_jobconf,
    map((-input => $_), @$ipaths),
    -output => $opath,
    -mapper => hadoop_embedded_cmd($mapper_file, @map_cmd),
    (defined $combiner
      ? (-combiner => hadoop_embedded_cmd($combiner_file, @combine_cmd))
      : ()),
    (defined $reducer
      ? (-reducer => hadoop_embedded_cmd($reducer_file, @reduce_cmd))
      : (-reducer => 'NONE'));
    
  $cmd;
}


defoperator hadoop_streaming => q{
  my ($mapper, $map_cmd_ref, 
      $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref,
      $nuke_inputs, $ipath_ref,
      $streaming_jar) = hadoop_cmd_setup @_;

  for my $ipaths (@$ipath_ref) {
    my $opath = resource_tmp "hdfs://";
    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref,
                              $combiner, $combine_cmd_ref,
                              $reducer, $reduce_cmd_ref, 
                              $streaming_jar, $ipaths, $opath);
    my $hadoop_fh = siproc {
     sh "$cmd 1>&2";
    };

    close $hadoop_fh;
    warn "ni: hadoop streaming failed" if $hadoop_fh->await;

    /^hdfsrm:/ && resource_nuke($_) for @$ipaths;

    (my $result_path = $opath) =~ s/^hdfs:/hdfst:/;
    print "$result_path/part-*\n";
  }

  if ($nuke_inputs) {resource_nuke $_ for map @$_, @$ipath_ref}

  resource_nuke $mapper;
  resource_nuke $combiner if defined $combiner;
  resource_nuke $reducer  if defined $reducer;
};

defoperator hadoop_make_nukeable =>
  q{print sr $_, qr/^hdfst?/, 'hdfsrm' while <STDIN>};

BEGIN {defparseralias hadoop_streaming_lambda => palt pmap(q{undef}, prc '_'),
                                                      pmap(q{[]},    prc ':'),
                                                      _qfn}

defhadoopalt S => pmap q{hadoop_streaming_op @$_},
                  pseq pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda;

defhadoopalt '#' => pmap q{hadoop_make_nukeable_op}, pnone;

defhadoopalt DS => pmap q{my ($m, $c, $r) = @$_;
                          my @cr =
                            (defined $c ? (row_sort_op(sort_args [0]), @$c) : (),
                             defined $r ? (row_sort_op(sort_args [0]), @$r) : ());
                          [@$m, @cr]},
                   pseq pc hadoop_streaming_lambda,
                        pc hadoop_streaming_lambda,
                        pc hadoop_streaming_lambda;

defhadoopalt R =>
  pmap q{configure_op {'hadoop/jobconf' => "mapred.reduce.tasks=$_"},
                      [hadoop_streaming_op [], undef, []]},
  pc number;

#Hadoop quick configuration.
#This will be useful for spinning up more customizable jobs once I
#figure out exactly how configure_op works.

defoperator hadoop_test => q{
  my ($mapper, $map_cmd_ref, 
      $combiner, $combine_cmd_ref,
      $reducer, $reduce_cmd_ref,
      $nuke_inputs, $ipath_ref,
      $streaming_jar) = hadoop_cmd_setup @_;

  for my $ipaths (@$ipath_ref) {
    my $opath = resource_tmp "hdfs://";
    my $cmd = make_hadoop_cmd($mapper, $map_cmd_ref, 
                              $combiner, $combine_cmd_ref,
                              $reducer, $reduce_cmd_ref,
                              $streaming_jar, $ipaths, $opath);
    print "$cmd\n";
  }
};

defhadoopalt T => pmap q{hadoop_test_op @$_},
                  pseq pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda;


