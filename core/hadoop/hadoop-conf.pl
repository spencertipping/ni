# MapReduce configuration is a huge pain;
# we aim to make it a little easier.

# Derived from https://github.com/Yelp/mrjob/blob/master/mrjob/compat.py
 
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

our %mr_conf_abbrevs = reverse %mr_generics;

our %compression_abbrevs = (
  "gz", "org.apache.hadoop.io.compress.GzipCodec",
  "lzo", "org.apache.hadoop.io.compress.DefaultCodec",
  "bz", "org.apache.hadoop.io.compress.BZip2Codec",
  "snappy", "org.apache.hadoop.io.compress.SnappyCodec"
);

sub sortconf($) {
  my $spec = $_[0];
  return undef unless substr($spec, 0, 1) eq "g";
  join "", map {my $col = ord($_) - 64; $_ eq "n" ? "n" : $_ eq "-" ? "r" : " -k$col,$col" }
   split //, substr $spec, 1;
}

sub partconf($) {
  my $spec = $_[0];
  return undef unless substr($spec, 0, 1) eq "f";
  "-k" . join ",", map { ord($_) - 64 } split //, substr $spec, 1;
}

sub translate_mr_conf_var($$) {
  my ($k, $v) = @_;
  if ($k eq "Hofcc") {return $compression_abbrevs{$v} || $v;}
  if ($k eq "Hpkco") {return sortconf($v) || $v;}
  if ($k eq "Hpkpo") {return partconf($v) || $v;}
  $v;
}

# These must be first in a hadoop command for... reasons.
our @priority_hadoop_opts = ("stream.num.map.output.key.fields",
                             "stream.map.output.field.separator",
                             "mapreduce.partition.keypartitioner.options",
                             "mapreduce.partition.keycomparator.options"); 
our @priority_hadoop_opt_abbrevs = map {$mr_conf_abbrevs{$_}} @priority_hadoop_opts;

sub priority_jobconf(@) {
  # Apparently some of the Hadoop options must be in order;
  # I have no idea what that order is exactly, but I follow
  # the convention laid out here:
  # https://hadoop.apache.org/docs/r1.2.1/streaming.html#Hadoop+Comparator+Class
  # and here:
  # http://ischoolreview.com/iSR_Grav/entries/entry-2
  # Upshots: you need to use the stream.map.output.num.fields if 
  # you use comparators 
  my %input_jobconf = @_;

  my @high_priority_jobconf = ();

  my @field_based_opts = grep defined, @input_jobconf{'Hpkpo', 'Hpkco'};
  if(@field_based_opts) {
    my $max_field = max map {split /\D+/} @field_based_opts;
    push @high_priority_jobconf, 
      -D => "stream.num.map.output.key.fields=$max_field";
  }

  push @high_priority_jobconf, 
    -D => "mapreduce.job.output.key.comparator.class=" . 
          "org.apache.hadoop.mapreduce.lib.partition.KeyFieldBasedComparator"
    if grep {$_ eq 'Hpkco'} keys %input_jobconf;

  push @high_priority_jobconf, 
    map { -D => $mr_generics{$_} . "=" . $input_jobconf{$_}} 
      grep { defined($input_jobconf{$_}) } @priority_hadoop_opt_abbrevs; 

  delete @input_jobconf{@priority_hadoop_opt_abbrevs};
  \@high_priority_jobconf, \%input_jobconf;
}

sub hadoop_generic_options(@) {
  my @jobconf = @_;
  my %jobconf = map {split /=/, $_, 2} @jobconf;
    %jobconf = map {$mr_conf_abbrevs{$_}, $jobconf{$_}} keys %jobconf;

  my %raw = map {$_, dor(conf $_, $jobconf{$_})} keys %mr_generics;
  my %clean_jobconf = map {$_, $raw{$_}} grep {defined $raw{$_}} keys %raw;
  my %clean_jobconf = map {$_, translate_mr_conf_var($_, $clean_jobconf{$_})} keys %clean_jobconf;
  $clean_jobconf{'Hpkpo'} = "-k1,1" if exists($clean_jobconf{'Hpkco'}) and !exists($clean_jobconf{'Hpkpo'});
  print join "\t", %clean_jobconf, "\n";
  my $needs_partitioner = grep {$_ eq 'Hpkpo'} keys %clean_jobconf; 

  my ($high_priority_jobconf_ref, $low_priority_jobconf_ref) = priority_jobconf(%clean_jobconf);
  %jobconf = %$low_priority_jobconf_ref;
  my @output_jobconf = @$high_priority_jobconf_ref;

  my @low_priority_options = map {$mr_generics{$_} . "=" . $clean_jobconf{$_}} keys %jobconf;
  my @low_priority_jobconf = map((-D => $_), @low_priority_options);

  # -partitioner is actually not a generic option
  # so it must follow the lowest priority generic option.
  push @low_priority_jobconf, 
    -partitioner => "org.apache.hadoop.mapred.lib.KeyFieldBasedPartitioner" 
    if $needs_partitioner;

  push @output_jobconf, @low_priority_jobconf;

  @output_jobconf;
}

