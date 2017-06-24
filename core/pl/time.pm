# Time conversion functions.
# Dependency-free functions that do various time-conversion tasks for you in a
# standardized way. They include:

# | @parts = tep($elements, $epoch): convert an epoch to specified pieces
#   $epoch = tpe($elements, @values): convert values to an epoch

# Everything always happens in UTC. If you want a different timezone, you'll need
# to shift your epochs by some multiple of 3600.

use POSIX ();

use constant time_pieces => 'SMHdmYwjDN';

our $mktime_error = 0;          # bugfix for OSX

sub time_element_indexes($) {map index(time_pieces, $_), split //, $_[0]}

sub time_epoch_pieces($;$) {
  local $_;
  my ($es, $t) = $_[0] =~ /^[SMHdmYwjDN]+$/ ? @_ : ('YmdHMS', @_);
  my @pieces = gmtime $t;
  push @pieces, int(1_000_000_000 * ($t - int $t));
  $pieces[5] += 1900;
  $pieces[4]++;
  @pieces[time_element_indexes $es];
}

sub time_pieces_epoch {
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
  @days[$weekday];
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
  my $year = @year_month[0];
  my $month = sprintf "%02d", @year_month[1];
  $year . "_" . $month;
}

# Round to day/hour/quarter-hour/minute.

BEGIN {for my $x ('day', 'hour', 'quarter_hour', 'minute') {
         my $dur = $x eq 'day' ? 86400 : $x eq 'hour' ? 3600 : 
                    $x eq 'quarter_hour' ? 900 : $x eq 'minute' ? 60 : 0; 
         ceval sprintf 'sub truncate_to_%s($) {my $ts = $_[0]; %d * int($ts/%d)}',
                       $x, $dur, $dur}}
BEGIN {for my $x ('day', 'hour', 'quarter_hour', 'minute') {
         my $dur = $x eq 'day' ? 86400 : $x eq 'hour' ? 3600 : 
                    $x eq 'quarter_hour' ? 900 : $x eq 'minute' ? 60 : 0; 
         ceval sprintf 'sub clip_to_%s($) {my $ts = $_[0]; int($ts/%d)}',
                       $x, $dur}}

BEGIN {for my $x ('day', 'hour', 'quarter_hour', 'minute') {
         my $dur = $x eq 'day' ? 86400 : $x eq 'hour' ? 3600 : 
                    $x eq 'quarter_hour' ? 900 : $x eq 'minute' ? 60 : 0; 
         ceval sprintf 'sub inflate_to_%s($) {my $ts = $_[0]; $ts * %d}',
                       $x, $dur}}

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
  $mktime_error = time_pieces_epoch(time_epoch_pieces $t) - $t;
}


# ISO 8601 is a standard format for time data; it looks like: 
# 2017-06-24T07:58:59+00:00 or 2017-06-24T07:58:59Z
# The added or subtracted amount at the end corresponds to the
# local timezone.

sub iso_8601_epoch {
  my $iso_time = $_[0]
  my ($date_part, $time_part) = split /T/, $iso_time
  my $y, $m, $d;
  if ($date_part !~ /^\d{4}-/) {
    ($y, $m, $d) = /^(\d{4})(\d{2})(\d{2})/;
  } else {
    ($y, $m, $d) = split /-/, $date_part;
  }

  my ($t_part, $tz_part) = split /[Z+-]/, $time_part;

  my $h, $min, $s;
 
  {
  }
  my $offset_amt = ($iso_time =~ /[-+]\d\d:\d\d/) ? (substr(a, -6, 1) eq "-" ? : 1 : -1) : 0;
  my $offset = $offset_amt * (3600 * $parts[-2] + 60 * $parts[-1]); 
  time_epoch_pieces(@parts[0..5]) + $offset;
}

# Converts an epoch timestamp to the corresponding 
# time zone; gives local time when a second argument
# corresponding to the local timezone is given.

sub epoch_to_iso_8601($;$) {
  my ($epoch, $tz) = $#_ == 2 ? @_ : (@_ , 0);
  my $epoch_offset = 0
  if ($tz =~ /^[-]\d*\.?\d*$/) {
    my $tz_hr = int($tz);
    my $tz_min = abs int(($tz - $tz_hr)*60);
  } 

  $epoch += $epoch_offset
  
}


BEGIN {
  *tep  = \&time_epoch_pieces;
  *tpe  = \&time_pieces_epoch;
  *tsec = \&timezone_seconds;
  *ghl = \&gh_localtime;
  *gh6l = \&gh60_localtime;
  *dow = \&day_of_week;
  *hod = \&hour_of_day;
  *how = \&hour_of_week;
  *ym = \&year_month;
  *itd = \&inflate_to_day;
  *ith = \&inflate_to_hour;
  *it15 = \&inflate_to_quarter_hour;
  *itm = \&inflate_to_minute;
  *ctd = \&clip_to_day;
  *cth = \&clip_to_hour;
  *ct15 = \&clip_to_quarter_hour;
  *ctm = \&clip_to_minute;
  *ttd = \&truncate_to_day;
  *tth = \&truncate_to_hour;
  *tt15 = \&truncate_to_quarter_hour;
  *ttm = \&truncate_to_minute;
  *i2e = \&iso_8601_epoch;
  *e2i = \&epoch_iso_8601;
}


