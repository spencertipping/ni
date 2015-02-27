# Types for gen objects
# IO source/sink stuff is organized around the idea that sources provide data
# for sinks, but this data can exist in various forms:
#
# L - a line, with newline, is present in $_.
# O - an object is present in $_ (if a string, the string has been chomped).
# F - values are present in @_.

our %type_conversions = (
  'F:L' => q{ $_ = join("\t", @_) . "\n"; %@body },
  'F:O' => q{ $_ = join("\t", @_); %@body },
  'L:O' => q{ chomp; %@body },
  'L:F' => q{ chomp; @_ = split /\t/; %@body },
  'O:F' => q{ @_ = ($_); %@body },
  'O:L' => q{ $_ .= "\n"; %@body });

$type_conversions{$_} = gen "conversion:$_", {}, $type_conversions{$_}
  for keys %type_conversions;

sub with_type {
  my ($type, $gen) = @_;
  return $gen if $$gen{sig}{type} eq $type;

  my $k = "$type:$$gen{sig}{type}";
DEBUG
  die "undefined type conversion: $$gen{sig}{type} -> $type"
    unless defined $type_conversions{$k};
DEBUG_END
  $type_conversions{$k} % {body => $gen};
}

sub typed_save_recover {
  my ($type) = @_;
  if ($type eq 'F') {
    my $xs = [];
    (gen('s:F', {xs => $xs}, q{ @{%:xs} = @_ }),
     gen('r:F', {xs => $xs}, q{ @_ = @{%:xs} }));
  } else {
    my $x = '';
    (gen("s:$type", {x => \$x}, q{ ${%:x} = $_ }),
     gen("r:$type", {x => \$x}, q{ $_ = ${%:x} }));
  }
}
