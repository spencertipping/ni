# Hadoop operator.
# The entry point for running various kinds of Hadoop jobs.

BEGIN {defshort '/H', defdsp 'hadoopalt', 'hadoop job dispatch table'}

our %mr_generics = (
'Hdb',      'dfs.blocksize',
'Hdbpc',    'dfs.bytes-per-checksum',
'Hdcwps',   'dfs.client-write-packet-size',
'Hdchkr',   'dfs.client.https.keystore.resource',
'Hdchna',   'dfs.client.https.need-auth',
'Hdcrps',   'dfs.client.read.prefetch.size',
'Hdcst',    'dfs.client.socket-timeout',
'Hddns',    'dfs.datanode.StorageId',
'Hddnbb',   'dfs.datanode.balance.bandwidthPerSec',
'Hddndd',   'dfs.datanode.data.dir',
'Hddnh',    'dfs.datanode.hostname',
'Hddnmtt',  'dfs.datanode.max.transfer.threads',
'Hdmsi',    'dfs.metrics.session-id',
'Hdnnap',   'dfs.namenode.accesstime.precision',
'Hdnnba',   'dfs.namenode.backup.address',
'Hdnnbha',  'dfs.namenode.backup.http-address',
'Hdnncd',   'dfs.namenode.checkpoint.dir',
'Hdnnced',  'dfs.namenode.checkpoint.edits.dir',
'Hdnncp',   'dfs.namenode.checkpoint.period',
'Hdnned',   'dfs.namenode.edits.dir',
'Hdnnhri',  'dfs.namenode.heartbeat.recheck-interval',
'Hdnnhttp', 'dfs.namenode.http-address',
'Hdnnhttps','dfs.namenode.https-address',
'Hdnnmo',   'dfs.namenode.max.objects',
'Hdnnnd',   'dfs.namenode.name.dir',
'Hdnnndr',  'dfs.namenode.name.dir.restore',
'Hdnnrc',   'dfs.namenode.replication.considerLoad',
'Hdnnri',   'dfs.namenode.replication.interval',
'Hdnnrms',  'dfs.namenode.replication.max-streams',
'Hdnnrm',   'dfs.namenode.replication.min',
'Hdnnrpts', 'dfs.namenode.replication.pending.timeout-sec',
'Hdnnse',   'dfs.namenode.safemode.extension',
'Hdnnstp',  'dfs.namenode.safemode.threshold-pct',
'Hdnnsha',  'dfs.namenode.secondary.http-address',
'Hdnnup',   'dfs.namenode.upgrade.permission',
'Hdpe',     'dfs.permissions.enabled',
'Hdps',     'dfs.permissions.superusergroup',
'Hfcbd',    'fs.client.buffer.dir',
'Hfd',      'fs.defaultFS',
'Hfdi',     'fs.df.interval',
'Hinla',    'io.native.lib.available',
'Hccp',     'mapreduce.client.completion.pollinterval',
'Hcgu',     'mapreduce.client.genericoptionsparser.used',
'Hcof',     'mapreduce.client.output.filter',
'Hcpp',     'mapreduce.client.progressmonitor.pollinterval',
'Hcsfr',    'mapreduce.client.submit.file.replication',
'Hcae',     'mapreduce.cluster.acls.enabled',
'Hcld',     'mapreduce.cluster.local.dir',
'Hcmm',     'mapreduce.cluster.mapmemory.mb',
'Hcps',     'mapreduce.cluster.permissions.supergroup',
'Hcrm',     'mapreduce.cluster.reducememory.mb',
'Hctd',     'mapreduce.cluster.temp.dir',
'Hfdfs',    'mapreduce.fieldsel.data.field.separator',
'Hfmokvfs', 'mapreduce.fieldsel.map.output.key.value.fields.spec',
'Hfrokvfs', 'mapreduce.fieldsel.reduce.output.key.value.fields.spec',
'Hifi',     'mapreduce.input.fileinputformat.inputdir',
'Hifsmax',  'mapreduce.input.fileinputformat.split.maxsize',
'Hifsmin',  'mapreduce.input.fileinputformat.split.minsize',
'Hifsmpn',  'mapreduce.input.fileinputformat.split.minsize.per.node',
'Hifsmpr',  'mapreduce.input.fileinputformat.split.minsize.per.rack',
'Hikkvs',   'mapreduce.input.keyvaluelinerecordreader.key.value.separator',
'Hill',     'mapreduce.input.lineinputformat.linespermap',
'Hillm',    'mapreduce.input.linerecordreader.line.maxlength',
'Himdf',    'mapreduce.input.multipleinputs.dir.formats',
'Himdm',    'mapreduce.input.multipleinputs.dir.mappers',
'Hipc',     'mapreduce.input.pathFilter.class',
'Hisc',     'mapreduce.input.sequencefileinputfilter.class',
'Hisf',     'mapreduce.input.sequencefileinputfilter.frequency',
'Hisr',     'mapreduce.input.sequencefileinputfilter.regex',
'Hjca',     'mapreduce.job.cache.archives',
'Hjcat',    'mapreduce.job.cache.archives.timestamps',
'Hjcf',     'mapreduce.job.cache.files',
'Hjcft',    'mapreduce.job.cache.files.timestamps',
'Hjcla',    'mapreduce.job.cache.local.archives',
'Hjclf',    'mapreduce.job.cache.local.files',
'Hjcsc',    'mapreduce.job.cache.symlink.create',
'Hjcpa',    'mapreduce.job.classpath.archives',
'Hjcpf',    'mapreduce.job.classpath.files',
'Hjcc',     'mapreduce.job.combine.class',
'Hjcscn',   'mapreduce.job.committer.setup.cleanup.needed',
'Hjenra',   'mapreduce.job.end-notification.retry.attempts',
'Hjenri',   'mapreduce.job.end-notification.retry.interval',
'Hjenu',    'mapreduce.job.end-notification.url',
'Hji',      'mapreduce.job.id',
'Hjic',     'mapreduce.job.inputformat.class',
'Hjj',      'mapreduce.job.jar',
'Hjjn',     'mapreduce.job.jvm.numtasks',
'Hjld',     'mapreduce.job.local.dir',
'Hjmc',     'mapreduce.job.map.class',
'Hjm',      'mapreduce.job.maps',
'Hjmpt',    'mapreduce.job.maxtaskfailures.per.tracker',
'Hjn',      'mapreduce.job.name',
'Hjogcc',   'mapreduce.job.output.group.comparator.class',
'Hjokc',    'mapreduce.job.output.key.class',
'Hjokcc',   'mapreduce.job.output.key.comparator.class',
'Hjovc',    'mapreduce.job.output.value.class',
'Hjoc',     'mapreduce.job.outputformat.class',
'Hjpc',     'mapreduce.job.partitioner.class',
'Hjp',      'mapreduce.job.priority',
'Hjq',      'mapreduce.job.queuename',
'Hjrc',     'mapreduce.job.reduce.class',
'Hjrsc',    'mapreduce.job.reduce.slowstart.completedmaps',
'Hjr',      'mapreduce.job.reduces',
'Hjso',     'mapreduce.job.skip.outdir',
'Hjs',      'mapreduce.job.skiprecords',
'Hjssnt',   'mapreduce.job.speculative.slownodethreshold',
'Hjsstt',   'mapreduce.job.speculative.slowtaskthreshold',
'Hjssc',    'mapreduce.job.speculative.speculativecap',
'Hjun',     'mapreduce.job.user.name',
'Hjurh',    'mapreduce.job.userlog.retain.hours',
'Hjwd',     'mapreduce.job.working.dir',
'Hjcci',    'mapreduce.jobcontrol.createdir.ifnotexist',
'Hjta',     'mapreduce.jobtracker.address',
'Hjtbat',   'mapreduce.jobtracker.blacklist.average.threshold',
'Hjteti',   'mapreduce.jobtracker.expire.trackers.interval',
'Hjthc',    'mapreduce.jobtracker.handler.count',
'Hjthis',   'mapreduce.jobtracker.heartbeats.in.second',
'Hjthef',   'mapreduce.jobtracker.hosts.exclude.filename',
'Hjthf',    'mapreduce.jobtracker.hosts.filename',
'Hjtha',    'mapreduce.jobtracker.http.address',
'Hjti',     'mapreduce.jobtracker.instrumentation',
'Hjtjbs',   'mapreduce.jobtracker.jobhistory.block.size',
'Hjtjcl',   'mapreduce.jobtracker.jobhistory.completed.location',
'Hjtjl',    'mapreduce.jobtracker.jobhistory.location',
'Hjtjlcs',  'mapreduce.jobtracker.jobhistory.lru.cache.size',
'Hjtjt',    'mapreduce.jobtracker.jobinit.threads',
'Hjtmmm',   'mapreduce.jobtracker.maxmapmemory.mb',
'Hjtmrm',   'mapreduce.jobtracker.maxreducememory.mb',
'Hjtmp',    'mapreduce.jobtracker.maxtasks.perjob',
'Hjtpja',   'mapreduce.jobtracker.persist.jobstatus.active',
'Hjtpjd',   'mapreduce.jobtracker.persist.jobstatus.dir',
'Hjtpjh',   'mapreduce.jobtracker.persist.jobstatus.hours',
'Hjtrr',    'mapreduce.jobtracker.restart.recover',
'Hjtrcs',   'mapreduce.jobtracker.retiredjobs.cache.size',
'Hjtr',     'mapreduce.jobtracker.retirejobs',
'Hjtsd',    'mapreduce.jobtracker.system.dir',
'Hjttl',    'mapreduce.jobtracker.taskcache.levels',
'Hjtt',     'mapreduce.jobtracker.taskscheduler',
'Hjttmp',   'mapreduce.jobtracker.taskscheduler.maxrunningtasks.perjob',
'Hjtttc',   'mapreduce.jobtracker.taskscheduler.taskalloc.capacitypad',
'Hjtttm',   'mapreduce.jobtracker.tasktracker.maxblacklists',
'Hjtwt',    'mapreduce.jobtracker.webinterface.trusted',
'Hje',      'mapreduce.join.expr',
'Hjk',      'mapreduce.join.keycomparator',
'Hmcm',     'mapreduce.map.combine.minspills',
'Hmds',     'mapreduce.map.debug.script',
'Hme',      'mapreduce.map.env',
'Hmfm',     'mapreduce.map.failures.maxpercent',
'Hmif',     'mapreduce.map.input.file',
'Hmil',     'mapreduce.map.input.length',
'Hmis',     'mapreduce.map.input.start',
'Hmjo',     'mapreduce.map.java.opts',
'Hmll',     'mapreduce.map.log.level',
'Hmm',      'mapreduce.map.maxattempts',
'Hmmm',     'mapreduce.map.memory.mb',
'Hmoc',     'mapreduce.map.output.compress',
'Hmocc',    'mapreduce.map.output.compress.codec',
'Hmokc',    'mapreduce.map.output.key.class',
'Hmokfs',   'mapreduce.map.output.key.field.separator',
'Hmovc',    'mapreduce.map.output.value.class',
'Hmsm',     'mapreduce.map.skip.maxrecords',
'Hmspcai',  'mapreduce.map.skip.proc-count.auto-incr',
'Hmssp',    'mapreduce.map.sort.spill.percent',
'Hms',      'mapreduce.map.speculative',
'Hmr',      'mapreduce.mapper.regex',
'Hmrg',     'mapreduce.mapper.regexmapper..group',
'Hofc',     'mapreduce.output.fileoutputformat.compress',
'Hofcc',    'mapreduce.output.fileoutputformat.compress.codec',
'Hofct',    'mapreduce.output.fileoutputformat.compress.type',
'Hofo',     'mapreduce.output.fileoutputformat.outputdir',
'Holo',     'mapreduce.output.lazyoutputformat.outputformat',
'Hoskc',    'mapreduce.output.seqbinaryoutputformat.key.class',
'Hosvc',    'mapreduce.output.seqbinaryoutputformat.value.class',
'Hots',     'mapreduce.output.textoutputformat.separator',
'Hpblo',    'mapreduce.partition.binarypartitioner.left.offset',
'Hpbro',    'mapreduce.partition.binarypartitioner.right.offset',
'Hpkco',    'mapreduce.partition.keycomparator.options',
'Hpkpo',    'mapreduce.partition.keypartitioner.options',
'Hpcp',     'mapreduce.pipes.commandfile.preserve',
'Hpe',      'mapreduce.pipes.executable',
'Hpei',     'mapreduce.pipes.executable.interpretor',
'Hpif',     'mapreduce.pipes.inputformat',
'Hpijm',    'mapreduce.pipes.isjavamapper',
'Hpijrr',   'mapreduce.pipes.isjavarecordreader',
'Hpijrw',   'mapreduce.pipes.isjavarecordwriter',
'Hpijr',    'mapreduce.pipes.isjavareducer',
'Hpp',      'mapreduce.pipes.partitioner',
'Hrds',     'mapreduce.reduce.debug.script',
'Hre',      'mapreduce.reduce.env',
'Hrfm',     'mapreduce.reduce.failures.maxpercent',
'Hribp',    'mapreduce.reduce.input.buffer.percent',
'Hrjo',     'mapreduce.reduce.java.opts',
'Hrll',     'mapreduce.reduce.log.level',
'Hrmbp',    'mapreduce.reduce.markreset.buffer.percent',
'Hrm',      'mapreduce.reduce.maxattempts',
'Hrmm',     'mapreduce.reduce.memory.mb',
'Hrmt',     'mapreduce.reduce.memory.totalbytes',
'Hrmit',    'mapreduce.reduce.merge.inmem.threshold',
'Hrsct',    'mapreduce.reduce.shuffle.connect.timeout',
'Hrsibp',   'mapreduce.reduce.shuffle.input.buffer.percent',
'Hrsmp',    'mapreduce.reduce.shuffle.merge.percent',
'Hrsp',     'mapreduce.reduce.shuffle.parallelcopies',
'Hrsrt',    'mapreduce.reduce.shuffle.read.timeout',
'Hrsm',     'mapreduce.reduce.skip.maxgroups',
'Hrspcai',  'mapreduce.reduce.skip.proc-count.auto-incr',
'Hrs',      'mapreduce.reduce.speculative',
'Htai',     'mapreduce.task.attempt.id',
'Htdl',     'mapreduce.task.debugout.lines',
'Htfpft',   'mapreduce.task.files.preserve.failedtasks',
'Htfpfp',   'mapreduce.task.files.preserve.filepattern',
'Htid',     'mapreduce.task.id',
'Htisf',    'mapreduce.task.io.sort.factor',
'Htism',    'mapreduce.task.io.sort.mb',
'Htim',     'mapreduce.task.ismap',
'Htmpr',    'mapreduce.task.merge.progress.records',
'Htod',     'mapreduce.task.output.dir',
'Htpart',   'mapreduce.task.partition',
'Htprof',   'mapreduce.task.profile',
'Htpm',     'mapreduce.task.profile.maps',
'Htpp',     'mapreduce.task.profile.params',
'Htpr',     'mapreduce.task.profile.reduces',
'Htssa',    'mapreduce.task.skip.start.attempts',
'Htt',      'mapreduce.task.timeout',
'Httd',     'mapreduce.task.tmp.dir',
'Htulk',    'mapreduce.task.userlog.limit.kb',
'Httcls',   'mapreduce.tasktracker.cache.local.size',
'Httct',    'mapreduce.tasktracker.contention.tracking',
'Httdi',    'mapreduce.tasktracker.dns.interface',
'Httdn',    'mapreduce.tasktracker.dns.nameserver',
'Htteb',    'mapreduce.tasktracker.events.batchsize',
'Htthi',    'mapreduce.tasktracker.healthchecker.interval',
'Htthsa',   'mapreduce.tasktracker.healthchecker.script.args',
'Htthsp',   'mapreduce.tasktracker.healthchecker.script.path',
'Htthst',   'mapreduce.tasktracker.healthchecker.script.timeout',
'Htthn',    'mapreduce.tasktracker.host.name',
'Httha',    'mapreduce.tasktracker.http.address',
'Httht',    'mapreduce.tasktracker.http.threads',
'Httim',    'mapreduce.tasktracker.indexcache.mb',
'Htti',     'mapreduce.tasktracker.instrumentation',
'Httldmsk', 'mapreduce.tasktracker.local.dir.minspacekill',
'Httldmss', 'mapreduce.tasktracker.local.dir.minspacestart',
'Httmtm',   'mapreduce.tasktracker.map.tasks.maximum',
'Httnsr',   'mapreduce.tasktracker.net.static.resolutions',
'Httrtm',   'mapreduce.tasktracker.reduce.tasks.maximum',
'Httra',    'mapreduce.tasktracker.report.address',
'Httr',     'mapreduce.tasktracker.resourcecalculatorplugin',
'Httt',     'mapreduce.tasktracker.taskcontroller',
'Htttm',    'mapreduce.tasktracker.taskmemorymanager.monitoringinterval',
'Httts',    'mapreduce.tasktracker.tasks.sleeptimebeforesigkill',
'Hntcnm',   'net.topology.configured.node.mapping',
'Hntnsmi',  'net.topology.node.switch.mapping.impl',
'Hntsfn',   'net.topology.script.file.name',
'Hntsna',   'net.topology.script.number.args',
'Hsjcpa',   'security.job.client.protocol.acl',
'Hsjtpa',   'security.job.task.protocol.acl',
'Hfieldsep','stream.map.output.field.separator',
'Hnfields', 'stream.num.map.output.key.fields',
);

for (keys %mr_generics) { 
  my $var_name = $mr_generics{$_}; 
  $var_name =~ tr/[a-z]\-./[A-Z]__/;
  my $env_var_name = 'NI_HADOOP_' . $var_name; 
  defconfenv $_, $env_var_name => undef;
}

our %mr_conf_abbrevs = reverse %mr_generics;

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


# These must be first in a hadoop command for... reasons.
our @priority_hadoop_opts = ("stream.num.map.output.key.fields",
                            "stream.map.output.field.separator",
                            "mapreduce.partition.keypartitioner.options",
                            "mapreduce.partition.keycomparator.options"); 
our @priority_hadoop_opt_abbrevs = map {$mr_conf_abbrevs{$_}} @priority_hadoop_opts;

sub priority_jobconf(%) {
  # Apparently some of the Hadoop options must be in order;
  # I have no idea what that order is exactly, but I follow
  # the convention laid out here:
  # https://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Comparator+Class
  # and here:
  # http://ischoolreview.com/iSR_Grav/entries/entry-2

  my %input_jobconf = @_;
  
  my @high_priority_jobconf = ();

  push @high_priority_jobconf, 
    -partitioner => 
      "org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedPartitioner" 
      if grep {$_ eq 'Hpkpo'} keys %input_jobconf;

  push @high_priority_jobconf, 
    -D => "mapreduce.job.output.key.comparator.class=" . 
          "org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedComparator"
    if grep {$_ eq 'Hpkco'} keys %input_jobconf;

  push @high_priority_jobconf, 
    map { -D => $mr_generics{$_} . "=" . $input_jobconf{$_}} 
    grep { defined($input_jobconf{$_}) } @priority_hadoop_opt_abbrevs; 

  my %low_priority_jobconf = delete @input_jobconf{@priority_hadoop_opt_abbrevs};

  \@high_priority_jobconf, \%low_priority_jobconf;
}

sub hadoop_generic_options($) {
  my @jobconf = @$_[0];
  my %jobconf = map {split /=/, $_} @jobconf;

  %jobconf = map {$mr_conf_abbrevs{$_}, $jobconf{$_}} keys %jobconf;
  my %raw = map {$_, dor(conf $_, $jobconf{$_})} keys %mr_generics;
  my %clean_jobconf = map {$_, $raw{$_}} grep {defined $raw{$_}} keys %raw;

  my ($high_priority_jobconf_ref, $low_priority_jobconf_ref) = priority_jobconf(%clean_jobconf);
  %jobconf = %$low_priority_jobconf_ref;
  my @high_priority_jobconf = @$high_priority_jobconf_ref;

  my @output_jobconf = map {$mr_generics{$_} . "=" . $clean_jobconf{$_}} keys %jobconf;
  my @low_priority_jobconf = map((-D => $_), @output_jobconf);
  push @high_priority_jobconf, @low_priority_jobconf;
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
  push @jobconf, "mapreduce.job.name=" . "ni @$ipaths -> $opath";
  
  my ($high_priority_options, $low_priority_options) = 
    hadoop_generic_options(\@jobconf);

  my $cmd = shell_quote
    conf 'hadoop/name',
    jar => $streaming_jar,
    # <GENERIC HADOOP OPTIONS>
    @$high_priority_options,
    @$low_priority_options,
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

our %hdp_conf = (
"R", "mapreduce.job.reduces",
"Rm", "mapreduce.reduce.memory.mb",
"Mm", "mapreduce.map.memory.mb",
"P", "mapreduce.job.priority.num",
"Ss", "mapreduce.job.reduce.slowstart.completedmaps"
);

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


