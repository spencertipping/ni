# JSON utils 

# for extracting a small number of fields from
# complex JSON

sub get_array {
  my @raw_data = $_[0] =~ /"$_[1]":\[([^]]+)/;
  return map {split /,/, $_} @raw_data;
}



