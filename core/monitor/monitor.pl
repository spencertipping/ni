# Pipeline monitoring.
# nfu provided a simple throughput/data count for each pipeline stage. ni can do
# much more, for instance determining the cause of a bottleneck and previewing
# data.

sub unit_bytes($) {
  return $_[0] >> 10, "K" if $_[0] >> 10 <= 99999;
  return $_[0] >> 20, "M" if $_[0] >> 20 <= 99999;
  return $_[0] >> 30, "G" if $_[0] >> 30 <= 99999;
  return $_[0] >> 40, "T" if $_[0] >> 40 <= 99999;
  return $_[0] >> 50, "P";
}

defconfenv 'monitor',          NI_MONITOR          => 'yes';
defconfenv 'monitor/start',    NI_MONITOR_START    => 2;
defconfenv 'monitor/interval', NI_MONITOR_INTERVAL => 0.1;

defmetaoperator stderr_monitor_transform => q{
  my ($args, $left) = @_;
  my ($interval) = @$args;
  [map {;$$left[$_], stderr_monitor_op($_, json_encode $$left[$_], $interval)}
        0..$#{$left}];
};

defoperator stderr_monitor => q{
  BEGIN {eval {require Time::HiRes; Time::HiRes->import('time')}}
  my ($monitor_id, $monitor_name, $update_rate) = (@_, 1);
  my ($itime, $otime, $bytes) = (0, 0, 0);
  my $last_update = 0;
  my $start_time  = 0;
  my $width       = $ENV{COLUMNS} || 80;
  my $runtime     = 1;
  my $preview     = "";
  my $factor_log  = 0;
  my ($stdin, $stdout) = (\*STDIN, \*STDOUT);

  my $monitor_start = conf 'monitor/start';

  while (1) {
    my $t1 = time; $bytes += my $n = saferead $stdin, $_, 65536;
                   last unless $n;
    my $t2 = time; safewrite_exactly $stdout, $_;
    my $t3 = time;

    # Start the clocks only once some data starts moving; we ignore the initial
    # read/write warmup
    if ($start_time)
    {
      $itime += $t2 - $t1;
      $otime += $t3 - $t2;
    }
    else
    {
      $start_time = $t2;
    }

    if ($t3 - $last_update > $update_rate && $t3 - $start_time > $monitor_start) {
      $last_update = $t3;
      $runtime = $t3 - $start_time || 1;
      if ($t3 & 3 && /\n(.*)\n/) {
        ($preview = substr $1, 0, $width - 20) =~ s/\t/  /g;
        $preview =~ s/[[:cntrl:]]/./g;
        $preview = substr $preview, 0, $width - 20;
      } else {
        $preview = substr $monitor_name, 0, $width - 20;
      }

      $factor_log = log(($otime || 1) / ($itime || 1)) / log 2;

      safewrite \*STDERR,
        sprintf "\033[%d;1H%d \r\033[K%5d%s %5d%s/s% 4d %s\n",
          $monitor_id + 1,
          int($t3),
          unit_bytes $bytes,
          unit_bytes $bytes / $runtime,
          $factor_log * 10,
          $preview;
    }
  }

  # Indicate EOF by dropping = into whitespaces.
  if (time() - $start_time > $monitor_start)
  {
    safewrite \*STDERR,
      sprintf "\033[%d;1H%d=\r\033[K%5d%s=%5d%s/s% 4d=%s\n",
        $monitor_id + 1,
        int($last_update),
        unit_bytes $bytes,
        unit_bytes $bytes / $runtime,
        $factor_log * 10,
        substr $monitor_name, 0, $width - 20;
  }
};

my $original_main_operator = $ni::main_operator;
$ni::main_operator = sub {
  return &$original_main_operator(@_) if conf 'monitor' ne 'yes';
  &$original_main_operator(@_,
    stderr_monitor_transform_op(conf 'monitor/interval'));
};
