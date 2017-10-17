# JSON utils 

# for extracting a small number of fields from
# complex JSON

sub get_array {
  my @raw_data = $_[0] =~ /"$_[1]":\[([^]]+)/;
  return map {eval $_} map {split /,/, $_} @raw_data;
}

sub get_scalar {
  my ($output_val,) = $_[0] =~ /"$_[1]":("[^"]*"|-?\d+.?\d*)/;
  return eval $output_val;
}

sub get_flat_hash {
  my @raw_data = $_[0] =~ /"$_[1]":({[^}]*})/;
  return @raw_data;
}

sub join_json_parts {
  if (substr($_[1], 0, 1) eq ",") {
    return join '', $_[0], substr($_[1], 1); 
  } elsif ($_[1] eq "}" and substr($_[0], -1) eq ",") {
    return join '', substr($_[0], 0, -1), $_[1];
  }  else {
    return join '', @_;
  }
}

sub delete_array {
  my @raw_data = $_[0] =~ /^(.*[{,])"$_[1]":\[[^]]+\]([},].*)$/;
  if(@raw_data) {
    return join_json_parts @raw_data;
  } else {
    return $_[0];
  }
}

sub delete_scalar {
  my @raw_data = $_[0] =~ /^(.*[{,])"$_[1]":(?:"[^"]*"|-?\d+.?\d*)([},].*)$/;
  if(@raw_data) {
    return join_json_parts @raw_data;
  } else {
    return $_[0];
  }
}

sub delete_flat_hash {
  my @raw_data = $_[0] =~ /^(.*[{,])"$_[1]":{[^}]*}([},].*)$/;
  if(@raw_data) {
    return join_json_parts @raw_data;
  } else {
    return $_[0];
  }
}
