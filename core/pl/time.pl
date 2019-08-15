# Time conversion functions.
# Dependency-free functions that do various time-conversion tasks for you in a
# standardized way. They include:

# | @parts = tep($elements, $epoch): convert an epoch to specified pieces
#   $epoch = tpe($elements, @values): convert values to an epoch

# Everything always happens in UTC. If you want a different timezone, you'll need
# to shift your epochs by some multiple of 3600.

use POSIX ();
BEGIN {eval {require Time::HiRes; Time::HiRes->import('time')}}

use constant time_pieces => 'SMHdmYwjDN';

our $mktime_error = 0;          # bugfix for OSX

sub time_element_indexes($) {map index(time_pieces, $_), split //, $_[0]}

sub time_epoch_pieces($;$) {
  no warnings;
  local $_;
  my ($es, $t) = $_[0] =~ /^[SMHdmYwjDN]+$/ ? @_ : ('YmdHMS', @_);

  # Accept nanos, micros, or millis; using year 3000 as an upper limit
  $t /= 1_000_000_000 if $t > 32503683601_000_000;
  $t /= 1_000_000     if $t > 32503683601_000;
  $t /= 1_000         if $t > 32503683601;

  my @pieces = gmtime $t;
  push @pieces, int(1_000_000_000 * ($t - int $t));
  $pieces[5] += 1900;
  $pieces[4]++;
  @pieces[time_element_indexes $es];
}

sub time_epoch_formatted($;$)
{
  my ($es, $t) = $_[0] =~ /^\D+$/ ? @_ : ('Y-m-d H:M:S', @_);
  my $pieces = join"", $es =~ /[SMHdmYjDN]/g;
  (my $format = $es) =~ s/([a-zA-Z])/$1 eq "Y" ? "%04d" : "%02d"/eg;
  sprintf $format, time_epoch_pieces $pieces, $t;
}

sub time_pieces_epoch {
  no warnings;
  local $_;
  my ($es, @ps) = $_[0] =~ /^[SMHdmYwjDN]+$/ ? @_ : ('YmdHMS', @_);
  my @tvs = (0, 0, 0, 1, 1, 1970, 0, 0, -1, 0);
  @tvs[time_element_indexes $es] = @ps;
  $tvs[5] -= 1900;
  $tvs[4]--;
  POSIX::mktime(@tvs[0..5]) + $tvs[9] / 1_000_000_000 - $mktime_error;
}

# Day of Week and Hour of Day.
# These methods are for converting timestamps in GMT; if you have data from another location on the globe (and you probably do), you'll need to use a timezone shift as described above.

our @days = qw(Thu Fri Sat Sun Mon Tue Wed);
sub day_of_week($) {
  my $ts = $_[0];
  my $weekday = int(($ts % 604800)/86400);
  $days[$weekday];
}

sub hour_of_day($) {
  my $ts = $_[0];
  int(($ts %86400)/3600);
}

sub hour_of_week($) {
  my $ts = $_[0];
  my $dow = day_of_week($ts);
  my $hod = sprintf "%02d", hour_of_day($ts);
  $dow . "_" . $hod;
}

sub year_month($) {
  my @year_month = tep('Ym', $_[0]);
  my $year = $year_month[0];
  my $month = sprintf "%02d", $year_month[1];
  $year . "-" . $month;
}

sub year_month_day($) {
  my @year_month = tep('Ymd', $_[0]);
  my $year = $year_month[0];
  my $month = sprintf "%02d", $year_month[1];
  my $day = sprintf "%02d", $year_month[2];
  join "-", $year, $month, $day;
}

# Round to day/hour/quarter-hour/minute.

BEGIN {my %duration_in_seconds = ('day' => 86400, 
                                  'hour' => 3600,
                                  'quarter_hour' => 900, 
                                  'minute' => 60,
                                  'week' => 7*86400);
        for my $time_period (keys %duration_in_seconds) {
         my $seconds = $duration_in_seconds{$time_period};
         eval sprintf 'sub truncate_to_%s($) {my $ts = $_[0]; %d * int($ts/%d)}',
                      $time_period, $seconds, $seconds}}

# Approximate timezone shifts by lat/lng.
# Uses the Bilow-Steinmetz approximation to quickly calculate a timezone offset
# (in seconds, which can be added to a GMT epoch) for a given latitude/longitude.
# It may be off by a few hours but is generally unbiased.

sub timezone_seconds {
  my ($lat, $lng) = @_;
  240 * int($lng + 7);
}

sub gh60_localtime($$) {
  my ($ts, $gh) = @_;
  my ($lat, $lng) = ghd $gh, 60;
  $ts + timezone_seconds($lat, $lng);
}

sub gh_localtime($$) {
  my ($ts, $gh) = @_;
  my ($lat, $lng) = ghd $gh;
  $ts + timezone_seconds($lat, $lng);
}

{
  my $t = time;
  $mktime_error = int time_pieces_epoch(time_epoch_pieces $t) - $t;
}


# ISO 8601 is a standard format for time data; it looks like: 
# 2017-06-24T07:58:59+00:00 or 2017-06-24T07:58:59Z
# There's also a form with no colons or dashes that's supported:
# 20170624T075859Z
# And also a form with a space between the time and the date:
# 2017-06-24 07:58:59.729
# The added or subtracted amount at the end corresponds to the
# local timezone.

sub iso_8601_epoch($) {
  my $iso_time = $_[0];
  my ($date_part, $time_part) = split /[\sT]/, $iso_time;
  my ($y, $m, $d);
  if ($date_part !~ /^\d{4}-/) {
    ($y, $m, $d) = /^(\d{4})(\d{2})(\d{2})/;
  } else {
    ($y, $m, $d) = split /-/, $date_part;
  }

  return time_pieces_epoch($y, $m, $d) unless $time_part;

  my ($h, $min, $s, $tz_part) = ($time_part =~ /^(\d{2}):?(\d{2}):?([0-9.]{2,})([Z+-].*)?$/);
  my $raw_ts = time_pieces_epoch($y, $m, $d, $h, $min, $s) + $s - int $s;
  return $raw_ts unless defined $tz_part;
  return $raw_ts if $tz_part eq "Z";

  my ($offset_type, $offset_hr, $offset_min) = ($tz_part =~ /([+-])(\d{2}):?(\d{2})?/);

  my $offset_amt = $offset_type eq "-" ? 1 : -1; 
  my $offset = $offset_amt * (3600 * $offset_hr + 60 * $offset_min); 
  $raw_ts + $offset;
}

# Converts an epoch timestamp to the corresponding 
# time zone; gives local time when a second argument
# corresponding to the local timezone is given.

sub make_tz_str($) {
  my $tz_raw = shift;
  my $epoch_offset;
  my $tz_str;
  if ($tz_raw =~ /^-?\d+\.?\d*$/) {
    die("badly formatted ISO timezone: $tz_raw\n") if abs $tz_raw > 12;
    $epoch_offset = $tz_raw*3600;
    my $tz_hr = int($tz_raw);
    my $tz_min = abs int(($tz_raw - $tz_hr)*60);
    my $tz_sign = $tz_raw < 0 ? "-" : "+";
    $tz_str = sprintf "%s%02d:%02d", $tz_sign, abs $tz_hr, $tz_min;
  } elsif ($tz_raw =~ /^[+-]\d{1,2}:?(\d{2})?$/) {
    my $tz_sign = substr($tz_raw, 0, 1);
    my ($tz_hr, $tz_min) = ($tz_raw =~ /^[+-](\d{1,2}):?(\d{2})?$/);
    $tz_str = sprintf "%s%02d:%02d", $tz_sign, $tz_hr, $tz_min || 0;
    my $offset_amt = 3600 * $tz_hr + 60 * ($tz_min || 0);
    $epoch_offset = $tz_sign eq "+"? $offset_amt : -$offset_amt;
  } elsif ($tz_raw eq "Z") {
    $epoch_offset = 0;
    $tz_str = $tz_raw;
  } else {
    die("badly formatted ISO 8601 timestamp: $tz_raw");
  }
  $tz_str, $epoch_offset;
}

sub epoch_iso_8601($;$) {
  my ($epoch, $tz_raw) = $#_ == 2 ? @_ : (@_ , "Z");
  my ($tz_str, $epoch_offset) = make_tz_str($tz_raw);
  $epoch += $epoch_offset;
  my ($y, $m, $d, $h, $min, $s) = time_epoch_pieces($epoch);
  my $iso_time = sprintf "%d-%02d-%02dT%02d:%02d:%02d%s", $y, $m, $d, $h, $min, $s, $tz_str;
  $iso_time;
}

sub time_parts_iso_8601 {
  my ($y, $m, $d, $h, $min, $s, $tz_raw) = @_;
  my ($tz_str, $epoch_offset) = make_tz_str($tz_raw);
  my $iso_time = sprintf "%d-%02d-%02dT%02d:%02d:%02d%s", $y, $m, $d, $h, $min, $s, $tz_str;
  $iso_time;
}

sub mdy_epoch {
  my ($m, $d, $y, $h, $min, $s) = split m#[/:\s]+#, $_[0];
  $y = $y < 100 ? $y + 2000 : $y;
  time_pieces_epoch($y, $m, $d, $h, $min, $s);
}


BEGIN {
  *tep  = \&time_epoch_pieces;
  *tef  = \&time_epoch_formatted;
  *tpe  = \&time_pieces_epoch;
  *tsec = \&timezone_seconds;
  *ghl = \&gh_localtime;
  *gh6l = \&gh60_localtime;
  *dow = \&day_of_week;
  *hod = \&hour_of_day;
  *how = \&hour_of_week;
  *ym = \&year_month;
  *ymd = \&year_month_day;
  *ttw = \&truncate_to_week;
  *ttd = \&truncate_to_day;
  *tth = \&truncate_to_hour;
  *tt15 = \&truncate_to_quarter_hour;
  *ttm = \&truncate_to_minute;
  *i2e = \&iso_8601_epoch;
  *e2i = \&epoch_iso_8601;
  *tpi = \&time_parts_iso_8601;
  *usfe = \&mdy_epoch
}


