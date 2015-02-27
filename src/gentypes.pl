# Types for gen objects
# IO source/sink stuff is organized around the idea that sources provide data
# for sinks, but this data can exist in various forms:
#
# L  - a line, with newline, is present in $_.
# O  - an object is present in $_ (if a string, the string has been chomped).
# Ix - values are stored in $_, and @_ contains field offsets up to x.
# F  - values are present in @_.

our %conversions = (
  'F:L' => q{ $_ = join("\t", @_) . "\n"; %@body },
  'F:O' => q{ $_ = join("\t", @_); %@body },
  'L:O' => q{ chomp; %@body },
  'L:F' => q{ chomp; @_ = split /\t/; %@body },
  'O:F' => q{ @_ = split /\t/; %@body },
  'O:L' => q{ $_ .= "\n"; %@body });

$conversions{$_} = gen "conversion:$_", {}, $conversions{$_}
  for keys %conversions;

sub with_type {
  my ($type, $gen) = @_;
  return $gen if $$gen{sig}{type} eq $type;
  my $k = "$$gen{sig}{type}:$type";
DEBUG
  die "undefined type conversion: $$gen{sig}{type} -> $type"
    unless defined $conversions{$k};
DEBUG_END
  $conversions{$k} % {body => $gen};
}

sub typed_save_recover {
  my ($type) = @_;
  if ($type eq 'F') {
    my $xs = [];
    (gen('s:F', {xs => $xs}, q{ @{%:xs} = @_ }),
     gen('r:F', {xs => $xs}, q{ @_ = @{%:xs} }));
  } elsif ($type =~ /^I/) {
    my $xs = [];
    my $x  = '';
    (gen("s:$type", {xs => $xs, x => \$x}, q{ @{%:xs} = @_; ${%:x} = $_ }),
     gen("r:$type", {xs => $xs, x => \$x}, q{ @_ = @{%:xs}; $_ = ${%:x} }));
  } else {
    my $x = '';
    (gen("s:$type", {x => \$x}, q{ ${%:x} = $_ }),
     gen("r:$type", {x => \$x}, q{ $_ = ${%:x} }));
  }
}
