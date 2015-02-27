# Types for gen objects
# IO source/sink stuff is organized around the idea that sources provide data
# for sinks, but this data can exist in various forms:
#
# L  - a line, with newline, is present in $_.
# O  - an object is present in $_ (if a string, the string has been chomped).
# Ix - values are stored in $_, and $_f0, $_f1, ..., $_fx are field boundaries.
# F  - values are present in @_.

our %type_conversions = (
  'F:L' => q{ $_ = join("\t", @_) . "\n"; %@body },
  'F:O' => q{ $_ = join("\t", @_); %@body },
  'L:O' => q{ chomp; %@body },
  'L:F' => q{ chomp; @_ = split /\t/; %@body },
  'O:F' => q{ @_ = ($_); %@body },
  'O:L' => q{ $_ .= "\n"; %@body });

$type_conversions{$_} = gen "conversion:$_", {}, $type_conversions{$_}
  for keys %type_conversions;

our $field_boundary_gensyms = gen 'empty:field_boundaries',
  {_n => 0, map(("_f$_" => 0), 0 .. 255)},
  join "\n", "# %:_n", map "# %:_f$_", 0 .. 255;

sub gen_li_conversion;
sub with_type {
  my ($type, $gen) = @_;
  return $gen if $$gen{sig}{type} eq $type;

  # Special handling of Ix types.
  if ($$gen{sig}{type} =~ /^I(\d+)$/) {
    my $gi = $1;
    return $type_conversions{'L:O'}
         % {body => gen_li_conversion($gi, -1, $gen)} if $type eq 'L';
    return gen_li_conversion $gi, $1, $gen if $type =~ /^I(\d+)$/;
DEBUG
    confess "type error: should never be adapting $type to a $gi gen "
          . "(this is not only slow, but a bad idea for semantic reasons)";
DEBUG_END
  } else {
    my $k = "$type:$$gen{sig}{type}";
DEBUG
    die "undefined type conversion: $$gen{sig}{type} -> $type"
      unless defined $type_conversions{$k};
DEBUG_END
    $type_conversions{$k} % {body => $gen};
  }
}

sub i_field_reference {
  my ($n)  = @_;
  my $prev = $n - 1;
  ($n ? gen("field_$n", {"_f$n" => 0, "_f$prev" => 0},
            qq{ substr(\$_, %:_f$prev, %:_f$n) })
      : gen("field 0", {_f0 => 0}, q{ substr($_, 0, %:_f0) }))
  ->inherit_gensyms_from($field_boundary_gensyms);
}

sub i_length_init {
  gen('i_length_init', {_n => 0}, q{ %:_n = length $_ })
  ->inherit_gensyms_from($field_boundary_gensyms);
}

sub i_field_construct {
  my ($n) = @_;
  my $prev = $n - 1;
  ($n ? gen("field_cons_$n", {"_f$n" => 0, "_f$prev" => 0, _n => 0},
            qq{ %:_f$n = 1 + index(\$_, "\\t", %:_f$prev);
                %:_f$n = %:_n if %:_f$n == 0 })
      : gen("field_cons_0", {_f0 => 0, _n => 0},
            qq{ %:_f0 = 1 + index(\$_, "\\t");
                %:_f0 = %:_n if %:_f0 == 0 }))
  ->inherit_gensyms_from($field_boundary_gensyms);
}

sub gen_li_conversion {
  my ($required, $have_so_far, $gen) = @_;
  return $gen if $have_so_far >= $required;
  gen_seq(($have_so_far >= 0 ? () : i_length_init),
          map(i_field_construct($_), ($have_so_far + 1) .. $required),
          $gen);
}

sub typed_save_recover {
  my ($type) = @_;
  if ($type eq 'F') {
    my $xs = [];
    (gen('s:F', {xs => $xs}, q{ @{%:xs} = @_ }),
     gen('r:F', {xs => $xs}, q{ @_ = @{%:xs} }));
  } elsif ($type =~ /^I/) {
    # FIXME
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
