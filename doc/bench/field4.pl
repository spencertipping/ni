while (<>) {
  my $t = index $_, "\t";
  @_ = (substr($_, 0, $t));
  print $_[0], "\t", length($_[0]), "\n";
}
