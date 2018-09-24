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

defconfenv 'monitor', NI_MONITOR => 'yes';
defconfenv 'monitor/atomic', NI_MONITOR_ATOMIC => 'no';

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
  my ($stdin, $stdout) = (\*STDIN, \*STDOUT);
  my $atomic = conf('monitor/atomic') eq 'yes';

  $_ = '';
  while (1) {
    my $istart = time;
    $bytes += my $n = saferead $stdin, $_, 65536 - length, length;
    last unless $n;
    my $iend = time;

    # If atomic writes have been requested, then write on newline boundaries
    # no more than 512 bytes long if possible.
    my $ostart = $iend;
    if ($atomic)
    {
      for (my $offset = 0; $offset < length;)
      {
        # Skip at least 256 bytes ahead so we don't make a whole bunch of tiny
        # writes.
        my $next_newline = index $_, "\n", $offset + 256;
        if ($next_newline == -1 || $next_newline > $offset + 512)
        {
          $_ = substr $_, $offset;
          $offset = length;
        }
        else
        {
          safewrite_exactly $stdout,
            substr $_, $offset, $next_newline + 1 - $offset;
          $offset = $next_newline + 1;
        }
      }
    }
    else
    {
      safewrite_exactly $stdout, $_;
    }
    my $oend = time;

    # Start the clocks only once some data starts moving; we ignore the initial
    # read/write warmup
    if ($start_time)
    {
      $itime += $iend - $istart;
      $otime += $oend - $ostart;
    }
    else
    {
      $start_time = $ostart;
    }

    if ($oend - $last_update > $update_rate && $oend - $start_time > 2) {
      $last_update = $oend;
      my $runtime = $oend - $start_time || 1;
      my $width   = $ENV{COLUMNS} || 80;
      my $preview;
      if ($oend & 3 && /\n(.*)\n/) {
        ($preview = substr $1, 0, $width - 20) =~ s/\t/  /g;
        $preview =~ s/[[:cntrl:]]/./g;
        $preview = substr $preview, 0, $width - 20;
      } else {
        $preview = substr $monitor_name, 0, $width - 20;
      }

      my $factor_log = log(($otime || 1) / ($itime || 1)) / log 2;

      safewrite \*STDERR,
        sprintf "\033[%d;1H%d \r\033[K%5d%s %5d%s/s% 4d %s\n",
          $monitor_id + 1,
          int($oend),
          unit_bytes $bytes,
          unit_bytes $bytes / $runtime,
          $factor_log * 10,
          $preview;
    }

    $_ = '' unless $atomic;
  }
};

my $original_main_operator = $ni::main_operator;
$ni::main_operator = sub {
  return &$original_main_operator(@_) if conf 'monitor' ne 'yes';
  &$original_main_operator(@_, stderr_monitor_transform_op(0.1));
};
