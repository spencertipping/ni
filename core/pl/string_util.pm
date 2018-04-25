# String utilities

sub startswith($$) {
  $_[0] =~ /^$_[1]/;
}

sub endswith($$) {
  $_[0] =~/$_[1]$/;
}

# Number to letter 1 => "A", 2 => "B", etc.
sub alph($) {chr($_[0] + 64)}

# Syntactic sugar for join/split
BEGIN { 
  my %short_separators =
    ("c" => ",", 
     "p" => "|", 
     "u" => "_",
     "w" => " ");
   my %regex_separators = ("/" => '\/', "|" => '\|');
   for my $sep_abbrev(keys %short_separators) {
     $join_sep = $short_separators{$sep_abbrev};
     $split_sep = $regex_separators{$join_sep} || $join_sep;
     ceval sprintf 'sub j%s      {join "%s",      @_;}', 
       $sep_abbrev, $join_sep;
     ceval sprintf 'sub j%s%s    {join "%s%s",    @_;}',
       $sep_abbrev, $sep_abbrev, $join_sep, $join_sep;
     ceval sprintf 'sub s%s($)   {split /%s/,   $_[0]}',
       $sep_abbrev, $split_sep;
     ceval sprintf 'sub s%s%s($) {split /%s%s/, $_[0]}',
       $sep_abbrev, $sep_abbrev, $split_sep, $split_sep;
    }
}

sub restrict_hdfs_path ($$) {
  my ($path, $restriction) = @_;
  my ($zeroes) = ($restriction =~ /^1(0*)$/);
  if (endswith $path, "part-*") {
    $path =~ s/part-\d*\*/part-$zeroes\*/;
  } else {
    $path = $path . "/part-$zeroes*"
  }
  $path;
}

