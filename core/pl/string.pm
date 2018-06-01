# String utilities

sub startswith($$) {
  $_[1] eq substr $_[0], 0, length $_[1];
}

sub endswith($$) {
  length($_[0]) >= length($_[1])
    and $_[1] eq substr $_[0], length($_[0]) - length($_[1]);
}

# Number to letter 1 => "A", 2 => "B", etc.
sub alph($) {chr($_[0] + 64)}

sub restrict_hdfs_path ($$) {
  my ($path, $restriction) = @_;
  my ($zeroes) = ($restriction =~ /^1(0*)$/);
  if (endswith $path, "part-*") {
    $path =~ s/part-\*/part-$zeroes\*/;
  } else {
    $path = $path . "/part-$zeroes*"
  }
  $path;
}

# Syntactic sugar for join/split
BEGIN
{
  my %short_separators =
    ("c" => ",",
     "C" => ":",
     "n" => "\n",
     "p" => "|",
     "t" => "\t",
     "u" => "_",
     "w" => " ");

   for my $abbrev (keys %short_separators)
   {
     my $sep = $short_separators{$abbrev};
     ceval sprintf 'sub jj%s      {join "%s",      @_;}',
       $abbrev, $sep;
     ceval sprintf 'sub jj%s%s    {join "%s%s",    @_;}',
       $abbrev, $abbrev, $sep, $sep;
     ceval sprintf 'sub ss%s($)   {split /\Q%s\E/,   $_[0]}',
       $abbrev, $sep;
     ceval sprintf 'sub ss%s%s($) {split /\Q%s%s\E/, $_[0]}',
       $abbrev, $abbrev, $sep, $sep;
    }
}

