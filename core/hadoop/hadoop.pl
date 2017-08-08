# Hadoop operator.
# The entry point for running various kinds of Hadoop jobs.

BEGIN {defshort '/H', defdsp 'hadoopalt', 'hadoop job dispatch table'}

defconfenv 'hadoop/name',          NI_HADOOP               => 'hadoop';
defconfenv 'hadoop/streaming-jar', NI_HADOOP_STREAMING_JAR => undef;

defconfenv 'hdfs/tmpdir', NI_HDFS_TMPDIR => '/tmp';

defconfenv 'hadoop/jobname', NI_HADOOP_JOBNAME => undef;
defconfenv 'hadoop/jobconf', NI_HADOOP_JOBCONF => undef;
defconfenv 'hadoop/fieldsep', NI_HADOOP_FIELDSEP => undef;
defconfenv 'hadoop/nfields', NI_HADOOP_NFIELDS => undef;
defconfenv 'hadoop/partopt', NI_HADOOP_PARTOPT => undef;
defconfenv 'hadoop/sortopt', NI_HADOOP_SORTOPT => undef;


our %mr_generic_conf = (
'db',      'dfs.blocksize',
'dbpc',    'dfs.bytes-per-checksum',
'dcwps',   'dfs.client-write-packet-size',
'dchkr',   'dfs.client.https.keystore.resource',
'dchna',   'dfs.client.https.need-auth',
'dcrps',   'dfs.client.read.prefetch.size',
'dcst',    'dfs.client.socket-timeout',
'ddns',    'dfs.datanode.StorageId',
'ddnbb',   'dfs.datanode.balance.bandwidthPerSec',
'ddndd',   'dfs.datanode.data.dir',
'ddnh',    'dfs.datanode.hostname',
'ddnmtt',  'dfs.datanode.max.transfer.threads',
'dmsi',    'dfs.metrics.session-id',
'dnnap',   'dfs.namenode.accesstime.precision',
'dnnba',   'dfs.namenode.backup.address',
'dnnbha',  'dfs.namenode.backup.http-address',
'dnncd',   'dfs.namenode.checkpoint.dir',
'dnnced',  'dfs.namenode.checkpoint.edits.dir',
'dnncp',   'dfs.namenode.checkpoint.period',
'dnned',   'dfs.namenode.edits.dir',
'dnnhri',  'dfs.namenode.heartbeat.recheck-interval',
'dnnhttp', 'dfs.namenode.http-address',
'dnnhttps','dfs.namenode.https-address',
'dnnmo',   'dfs.namenode.max.objects',
'dnnnd',   'dfs.namenode.name.dir',
'dnnndr',  'dfs.namenode.name.dir.restore',
'dnnrc',   'dfs.namenode.replication.considerLoad',
'dnnri',   'dfs.namenode.replication.interval',
'dnnrms',  'dfs.namenode.replication.max-streams',
'dnnrm',   'dfs.namenode.replication.min',
'dnnrpts', 'dfs.namenode.replication.pending.timeout-sec',
'dnnse',   'dfs.namenode.safemode.extension',
'dnnstp',  'dfs.namenode.safemode.threshold-pct',
'dnnsha',  'dfs.namenode.secondary.http-address',
'dnnup',   'dfs.namenode.upgrade.permission',
'dpe',     'dfs.permissions.enabled',
'dps',     'dfs.permissions.superusergroup',
'fcbd',    'fs.client.buffer.dir',
'fd',      'fs.defaultFS',
'fdi',     'fs.df.interval',
'inla',    'io.native.lib.available',
'ccp',     'mapreduce.client.completion.pollinterval',
'cgu',     'mapreduce.client.genericoptionsparser.used',
'cof',     'mapreduce.client.output.filter',
'cpp',     'mapreduce.client.progressmonitor.pollinterval',
'csfr',    'mapreduce.client.submit.file.replication',
'cae',     'mapreduce.cluster.acls.enabled',
'cld',     'mapreduce.cluster.local.dir',
'cmm',     'mapreduce.cluster.mapmemory.mb',
'cps',     'mapreduce.cluster.permissions.supergroup',
'crm',     'mapreduce.cluster.reducememory.mb',
'ctd',     'mapreduce.cluster.temp.dir',
'fdfs',    'mapreduce.fieldsel.data.field.separator',
'fmokvfs', 'mapreduce.fieldsel.map.output.key.value.fields.spec',
'frokvfs', 'mapreduce.fieldsel.reduce.output.key.value.fields.spec',
'ifi',     'mapreduce.input.fileinputformat.inputdir',
'ifsmax',  'mapreduce.input.fileinputformat.split.maxsize',
'ifsmin',  'mapreduce.input.fileinputformat.split.minsize',
'ifsmpn',  'mapreduce.input.fileinputformat.split.minsize.per.node',
'ifsmpr',  'mapreduce.input.fileinputformat.split.minsize.per.rack',
'ikkvs',   'mapreduce.input.keyvaluelinerecordreader.key.value.separator',
'ill',     'mapreduce.input.lineinputformat.linespermap',
'illm',    'mapreduce.input.linerecordreader.line.maxlength',
'imdf',    'mapreduce.input.multipleinputs.dir.formats',
'imdm',    'mapreduce.input.multipleinputs.dir.mappers',
'ipc',     'mapreduce.input.pathFilter.class',
'isc',     'mapreduce.input.sequencefileinputfilter.class',
'isf',     'mapreduce.input.sequencefileinputfilter.frequency',
'isr',     'mapreduce.input.sequencefileinputfilter.regex',
'jca',     'mapreduce.job.cache.archives',
'jcat',    'mapreduce.job.cache.archives.timestamps',
'jcf',     'mapreduce.job.cache.files',
'jcft',    'mapreduce.job.cache.files.timestamps',
'jcla',    'mapreduce.job.cache.local.archives',
'jclf',    'mapreduce.job.cache.local.files',
'jcsc',    'mapreduce.job.cache.symlink.create',
'jcpa',    'mapreduce.job.classpath.archives',
'jcpf',    'mapreduce.job.classpath.files',
'jcc',     'mapreduce.job.combine.class',
'jcscn',   'mapreduce.job.committer.setup.cleanup.needed',
'jenra',   'mapreduce.job.end-notification.retry.attempts',
'jenri',   'mapreduce.job.end-notification.retry.interval',
'jenu',    'mapreduce.job.end-notification.url',
'ji',      'mapreduce.job.id',
'jic',     'mapreduce.job.inputformat.class',
'jj',      'mapreduce.job.jar',
'jjn',     'mapreduce.job.jvm.numtasks',
'jld',     'mapreduce.job.local.dir',
'jmc',     'mapreduce.job.map.class',
'jm',      'mapreduce.job.maps',
'jmpt',    'mapreduce.job.maxtaskfailures.per.tracker',
'jn',      'mapreduce.job.name',
'jogcc',   'mapreduce.job.output.group.comparator.class',
'jokc',    'mapreduce.job.output.key.class',
'jokcc',   'mapreduce.job.output.key.comparator.class',
'jovc',    'mapreduce.job.output.value.class',
'joc',     'mapreduce.job.outputformat.class',
'jpc',     'mapreduce.job.partitioner.class',
'jp',      'mapreduce.job.priority',
'jq',      'mapreduce.job.queuename',
'jrc',     'mapreduce.job.reduce.class',
'jrsc',    'mapreduce.job.reduce.slowstart.completedmaps',
'jr',      'mapreduce.job.reduces',
'jso',     'mapreduce.job.skip.outdir',
'js',      'mapreduce.job.skiprecords',
'jssnt',   'mapreduce.job.speculative.slownodethreshold',
'jsstt',   'mapreduce.job.speculative.slowtaskthreshold',
'jssc',    'mapreduce.job.speculative.speculativecap',
'jun',     'mapreduce.job.user.name',
'jurh',    'mapreduce.job.userlog.retain.hours',
'jwd',     'mapreduce.job.working.dir',
'jcci',    'mapreduce.jobcontrol.createdir.ifnotexist',
'jta',     'mapreduce.jobtracker.address',
'jtbat',   'mapreduce.jobtracker.blacklist.average.threshold',
'jteti',   'mapreduce.jobtracker.expire.trackers.interval',
'jthc',    'mapreduce.jobtracker.handler.count',
'jthis',   'mapreduce.jobtracker.heartbeats.in.second',
'jthef',   'mapreduce.jobtracker.hosts.exclude.filename',
'jthf',    'mapreduce.jobtracker.hosts.filename',
'jtha',    'mapreduce.jobtracker.http.address',
'jti',     'mapreduce.jobtracker.instrumentation',
'jtjbs',   'mapreduce.jobtracker.jobhistory.block.size',
'jtjcl',   'mapreduce.jobtracker.jobhistory.completed.location',
'jtjl',    'mapreduce.jobtracker.jobhistory.location',
'jtjlcs',  'mapreduce.jobtracker.jobhistory.lru.cache.size',
'jtjt',    'mapreduce.jobtracker.jobinit.threads',
'jtmmm',   'mapreduce.jobtracker.maxmapmemory.mb',
'jtmrm',   'mapreduce.jobtracker.maxreducememory.mb',
'jtmp',    'mapreduce.jobtracker.maxtasks.perjob',
'jtpja',   'mapreduce.jobtracker.persist.jobstatus.active',
'jtpjd',   'mapreduce.jobtracker.persist.jobstatus.dir',
'jtpjh',   'mapreduce.jobtracker.persist.jobstatus.hours',
'jtrr',    'mapreduce.jobtracker.restart.recover',
'jtrcs',   'mapreduce.jobtracker.retiredjobs.cache.size',
'jtr',     'mapreduce.jobtracker.retirejobs',
'jtsd',    'mapreduce.jobtracker.system.dir',
'jttl',    'mapreduce.jobtracker.taskcache.levels',
'jtt',     'mapreduce.jobtracker.taskscheduler',
'jttmp',   'mapreduce.jobtracker.taskscheduler.maxrunningtasks.perjob',
'jtttc',   'mapreduce.jobtracker.taskscheduler.taskalloc.capacitypad',
'jtttm',   'mapreduce.jobtracker.tasktracker.maxblacklists',
'jtwt',    'mapreduce.jobtracker.webinterface.trusted',
'je',      'mapreduce.join.expr',
'jk',      'mapreduce.join.keycomparator',
'mcm',     'mapreduce.map.combine.minspills',
'mds',     'mapreduce.map.debug.script',
'me',      'mapreduce.map.env',
'mfm',     'mapreduce.map.failures.maxpercent',
'mif',     'mapreduce.map.input.file',
'mil',     'mapreduce.map.input.length',
'mis',     'mapreduce.map.input.start',
'mjo',     'mapreduce.map.java.opts',
'mll',     'mapreduce.map.log.level',
'mm',      'mapreduce.map.maxattempts',
'mmm',     'mapreduce.map.memory.mb',
'moc',     'mapreduce.map.output.compress',
'mocc',    'mapreduce.map.output.compress.codec',
'mokc',    'mapreduce.map.output.key.class',
'mokfs',   'mapreduce.map.output.key.field.separator',
'movc',    'mapreduce.map.output.value.class',
'msm',     'mapreduce.map.skip.maxrecords',
'mspcai',  'mapreduce.map.skip.proc-count.auto-incr',
'mssp',    'mapreduce.map.sort.spill.percent',
'ms',      'mapreduce.map.speculative',
'mr',      'mapreduce.mapper.regex',
'mrg',     'mapreduce.mapper.regexmapper..group',
'ofc',     'mapreduce.output.fileoutputformat.compress',
'ofcc',    'mapreduce.output.fileoutputformat.compress.codec',
'ofct',    'mapreduce.output.fileoutputformat.compress.type',
'ofo',     'mapreduce.output.fileoutputformat.outputdir',
'olo',     'mapreduce.output.lazyoutputformat.outputformat',
'oskc',    'mapreduce.output.seqbinaryoutputformat.key.class',
'osvc',    'mapreduce.output.seqbinaryoutputformat.value.class',
'ots',     'mapreduce.output.textoutputformat.separator',
'pblo',    'mapreduce.partition.binarypartitioner.left.offset',
'pbro',    'mapreduce.partition.binarypartitioner.right.offset',
'pkco',    'mapreduce.partition.keycomparator.options',
'pkpo',    'mapreduce.partition.keypartitioner.options',
'pcp',     'mapreduce.pipes.commandfile.preserve',
'pe',      'mapreduce.pipes.executable',
'pei',     'mapreduce.pipes.executable.interpretor',
'pif',     'mapreduce.pipes.inputformat',
'pijm',    'mapreduce.pipes.isjavamapper',
'pijrr',   'mapreduce.pipes.isjavarecordreader',
'pijrw',   'mapreduce.pipes.isjavarecordwriter',
'pijr',    'mapreduce.pipes.isjavareducer',
'pp',      'mapreduce.pipes.partitioner',
'rds',     'mapreduce.reduce.debug.script',
're',      'mapreduce.reduce.env',
'rfm',     'mapreduce.reduce.failures.maxpercent',
'ribp',    'mapreduce.reduce.input.buffer.percent',
'rjo',     'mapreduce.reduce.java.opts',
'rll',     'mapreduce.reduce.log.level',
'rmbp',    'mapreduce.reduce.markreset.buffer.percent',
'rm',      'mapreduce.reduce.maxattempts',
'rmm',     'mapreduce.reduce.memory.mb',
'rmt',     'mapreduce.reduce.memory.totalbytes',
'rmit',    'mapreduce.reduce.merge.inmem.threshold',
'rsct',    'mapreduce.reduce.shuffle.connect.timeout',
'rsibp',   'mapreduce.reduce.shuffle.input.buffer.percent',
'rsmp',    'mapreduce.reduce.shuffle.merge.percent',
'rsp',     'mapreduce.reduce.shuffle.parallelcopies',
'rsrt',    'mapreduce.reduce.shuffle.read.timeout',
'rsm',     'mapreduce.reduce.skip.maxgroups',
'rspcai',  'mapreduce.reduce.skip.proc-count.auto-incr',
'rs',      'mapreduce.reduce.speculative',
'tai',     'mapreduce.task.attempt.id',
'tdl',     'mapreduce.task.debugout.lines',
'tfpft',   'mapreduce.task.files.preserve.failedtasks',
'tfpfp',   'mapreduce.task.files.preserve.filepattern',
'tid',     'mapreduce.task.id',
'tisf',    'mapreduce.task.io.sort.factor',
'tism',    'mapreduce.task.io.sort.mb',
'tim',     'mapreduce.task.ismap',
'tmpr',    'mapreduce.task.merge.progress.records',
'tod',     'mapreduce.task.output.dir',
'tpart',   'mapreduce.task.partition',
'tprof',   'mapreduce.task.profile',
'tpm',     'mapreduce.task.profile.maps',
'tpp',     'mapreduce.task.profile.params',
'tpr',     'mapreduce.task.profile.reduces',
'tssa',    'mapreduce.task.skip.start.attempts',
'tt',      'mapreduce.task.timeout',
'ttd',     'mapreduce.task.tmp.dir',
'tulk',    'mapreduce.task.userlog.limit.kb',
'ttcls',   'mapreduce.tasktracker.cache.local.size',
'ttct',    'mapreduce.tasktracker.contention.tracking',
'ttdi',    'mapreduce.tasktracker.dns.interface',
'ttdn',    'mapreduce.tasktracker.dns.nameserver',
'tteb',    'mapreduce.tasktracker.events.batchsize',
'tthi',    'mapreduce.tasktracker.healthchecker.interval',
'tthsa',   'mapreduce.tasktracker.healthchecker.script.args',
'tthsp',   'mapreduce.tasktracker.healthchecker.script.path',
'tthst',   'mapreduce.tasktracker.healthchecker.script.timeout',
'tthn',    'mapreduce.tasktracker.host.name',
'ttha',    'mapreduce.tasktracker.http.address',
'ttht',    'mapreduce.tasktracker.http.threads',
'ttim',    'mapreduce.tasktracker.indexcache.mb',
'tti',     'mapreduce.tasktracker.instrumentation',
'ttldmsk', 'mapreduce.tasktracker.local.dir.minspacekill',
'ttldmss', 'mapreduce.tasktracker.local.dir.minspacestart',
'ttmtm',   'mapreduce.tasktracker.map.tasks.maximum',
'ttnsr',   'mapreduce.tasktracker.net.static.resolutions',
'ttrtm',   'mapreduce.tasktracker.reduce.tasks.maximum',
'ttra',    'mapreduce.tasktracker.report.address',
'ttr',     'mapreduce.tasktracker.resourcecalculatorplugin',
'ttt',     'mapreduce.tasktracker.taskcontroller',
'tttm',    'mapreduce.tasktracker.taskmemorymanager.monitoringinterval',
'ttts',    'mapreduce.tasktracker.tasks.sleeptimebeforesigkill',
'ntcnm',   'net.topology.configured.node.mapping',
'ntnsmi',  'net.topology.node.switch.mapping.impl',
'ntsfn',   'net.topology.script.file.name',
'ntsna',   'net.topology.script.number.args',
'sjcpa',   'security.job.client.protocol.acl',
'sjtpa',   'security.job.task.protocol.acl',
);

our %ni_mr_abbrevs = reverse %mr_generic_conf;

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

sub make_hadoop_command($$$$$$$$$) {
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

  my $cmd = shell_quote
    conf 'hadoop/name',
    jar => $streaming_jar,
    # <GENERIC HADOOP OPTIONS>
    -D  => "mapreduce.job.name=" . dor(conf 'hadoop/jobname', "ni @$ipaths -> $opath"),
    -D  => "stream.num.map.output.key.fields=" . dor(conf 'hadoop/nfields', 1),
    -D  => "stream.map.output.field.separator=" . dor(conf 'hadoop/fieldsep', '"\\t"'),
    -D  => "mapreduce.partition.keypartitioner.options=" . dor(conf 'hadoop/partopt', "-k1,1"),
    -D  => "mapreduce.job.output.key.comparator.class=org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedComparator",
    -D  => "mapreduce.partition.keycomparator.options=" . dor(conf 'hadoop/sortopt', "-k1,1"),
    map((-D => $_), @jobconf),
    -files  => join(",", grep defined, ($mapper, $combiner, $reducer)),
    # </GENERIC HADOOP OPTIONS>
    # <HADOOP "COMMAND" OPTIONS>
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
  my ($map, $combine, $reduce) = @_;
  my ($nuke_inputs, @ipath) = hdfs_input_path;

  my ($mapper, @map_cmd) = hadoop_lambda_file 'mapper', $map;
  my ($combiner, @combine_cmd) = $combine
    ? hadoop_lambda_file 'combiner', $combine : ();
  my ($reducer, @reduce_cmd) = $reduce
    ? hadoop_lambda_file 'reducer', $reduce : ();

  my $streaming_jar = hadoop_streaming_jar;

  for my $ipaths (@ipath) {
    my $opath = resource_tmp "hdfs://";
    my $hadoop_fh = siproc {
      $mapper   =~ s|^file://||;
      $combiner =~ s|^file://|| if $combiner;
      $reducer  =~ s|^file://|| if $reducer;

      (my $mapper_file   = $mapper)         =~ s|.*/||;
      (my $combiner_file = $combiner || '') =~ s|.*/||;
      (my $reducer_file  = $reducer  || '') =~ s|.*/||;

      my @jobconf =
        grep $reducer || !/reduce/,             # HACK
        grep length, split /\s+/, dor conf 'hadoop/jobconf', '';

      my $cmd = shell_quote
        conf 'hadoop/name',
        jar => $streaming_jar,
        -D  => "mapreduce.job.name=" . dor(conf 'hadoop/jobname', "ni @$ipaths -> $opath"),
        -D  => "stream.num.map.output.key.fields=" . dor(conf 'hadoop/nfields', 1),
        -D  => "stream.map.output.field.separator=" . dor(conf 'hadoop/fieldsep', '"\\t"'),
        -D  => "mapreduce.partition.keypartitioner.options=" . dor(conf 'hadoop/partopt', "-k1,1"),
        -D  => "mapreduce.job.output.key.comparator.class=org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedComparator",
        -D  => "mapreduce.partition.keycomparator.options=" . dor(conf 'hadoop/sortopt', "-k1,1"),
        map((-D => $_), @jobconf),
        -files  => join(",", grep defined, ($mapper,$combiner,$reducer)),
        # </GENERIC HADOOP OPTIONS>
        # <HADOOP "COMMAND" OPTIONS>
        map((-input => $_), @$ipaths),
        -output => $opath,
        -mapper => hadoop_embedded_cmd($mapper_file, @map_cmd),
        (defined $combiner
          ? (-combiner => hadoop_embedded_cmd($combiner_file, @combine_cmd))
          : ()),
        (defined $reducer
          ? (-reducer => hadoop_embedded_cmd($reducer_file, @reduce_cmd))
          : (-reducer => 'NONE'));
     sh "$cmd 1>&2";
    };

    close $hadoop_fh;
    warn "ni: hadoop streaming failed" if $hadoop_fh->await;

    /^hdfsrm:/ && resource_nuke($_) for @$ipaths;

    (my $result_path = $opath) =~ s/^hdfs:/hdfst:/;
    print "$result_path/part-*\n";
  }

  if ($nuke_inputs) {resource_nuke $_ for map @$_, @ipath}

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

our %hdp_conf = (
"R", "mapreduce.job.reduces",
"Rm", "mapreduce.reduce.memory.mb",
"Mm", "mapreduce.map.memory.mb",
"P", "mapreduce.job.priority.num",
"Ss", "mapreduce.job.reduce.slowstart.completedmaps"
);

defoperator hadoop_test => q{
  my ($map, $combine, $reduce) = @_;
  my ($nuke_inputs, @ipath) = hdfs_input_path;

  my ($mapper, @map_cmd) = hadoop_lambda_file 'mapper', $map;
  my ($combiner, @combine_cmd) = $combine
    ? hadoop_lambda_file 'combiner', $combine : ();
  my ($reducer, @reduce_cmd) = $reduce
    ? hadoop_lambda_file 'reducer', $reduce : ();

  my $streaming_jar = hadoop_streaming_jar;

  for my $ipaths (@ipath) {
    my $opath = resource_tmp "hdfs://";
    my $cmd = make_hadoop_command($mapper, \@map_cmd, $combiner, \@combine_cmd,
                                  $reducer, \@reduce_cmd, $streaming_jar, $ipaths, $opath);
    print "$cmd\n";
  }
};

defhadoopalt T => pmap q{hadoop_test_op @$_},
                  pseq pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda,
                       pc hadoop_streaming_lambda;


