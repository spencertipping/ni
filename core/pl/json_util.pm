# JSON utils 

# for extracting a small number of fields from
# complex JSON

sub get_array {
  my @raw_data = $_[0] =~ /"$_[1]":\[([^]]+)/;
  return map {eval $_} map {split /,/, $_} @raw_data;
}

sub get_scalar {
  my ($output_val,) = $_[0] =~ /"$_[1]":("[^"]*"|\d+)/;
  return eval $output_val;
}

sub get_flat_hash {
  my @raw_data = $_[0] =~ /"$_[1]":({[^}]*})/;
  return @raw_data;
}

