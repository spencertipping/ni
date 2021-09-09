# File Readers
sub rf($)  {my $fh; open $fh, "< $_[0]" or open $fh, $_[0] or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rfl($) {my $fh; open $fh, "< $_[0]" or open $fh, $_[0] or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc($) {chomp(my $r = rf shift); $r}

# ri(my $var, "< file"): read entire contents into
sub ri  {my $fh; open $fh, "< $_[1]" or open $fh, $_[1] or die "ri $_[1]: $!"; 1 while read $fh, $_[0], 65536, length $_[0]}

sub dirbase($)  {my @xs = $_[0] =~ /^(.*)\/+([^\/]+)\/*$/; @xs ? @xs : ('', $_[0])}
sub basename($) {(dirbase $_[0])[1]}
sub dirname($)  {(dirbase $_[0])[0]}

sub mkdir_p {-d $_[0] or !length $_[0] or mkdir_p(dirname $_[0]) && mkdir $_[0]}

sub wf {
  local $_;
  my $f = shift;
  my $fh;
  if ($f =~ /^[\|>]/) {
    open $fh, $f or die "wf $f: $!";
  } else {
    mkdir_p dirname $f;
    open $fh, "> $f" or die "wf $f: $!";
  }
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

sub af {
  local $_;
  my $f = shift;
  mkdir_p dirname $f;
  open my $fh, ">> $f" or die "af $f: $!";
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

# el = invoke this sub on $_ and @_ = split/\t/ for each line in the specified
# file
sub el(&$)
{
  local $_;
  my ($fn, $f) = @_;
  my $fh;
  open $fh, "< $f" or open $fh, $f or die "el $f: $!";
  while (<$fh>) { chomp; &$fn(split /\t/) }
}
