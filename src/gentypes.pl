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
  'O:F' => q{ @_ = ($_); %@body },
  'O:L' => q{ $_ .= "\n"; %@body });

$conversions{$_} = gen "conversion:$_", {}, $conversions{$_}
  for keys %conversions;

sub gen_li_conversion;
sub with_type {
  my ($type, $gen) = @_;
  return $gen if $$gen{sig}{type} eq $type;

  # Special handling of I types.
  if ($$gen{sig}{type} =~ /^I(\d+)$/) {
    my $gi = $1;
    return gen_seq "li_conversion:$type",
      'chomp', gen_li_conversion($gi, -1, $gen) if $type eq 'L';
    return gen_li_conversion $gi, $1, $gen if $type =~ /^I(\d+)$/;
    die "type error: should never be adapting $type to a $gi gen "
      . "(this is not only slow, but a bad idea for semantic reasons)";
  } else {
    my $k = "$type:$$gen{sig}{type}";
DEBUG
    die "undefined type conversion: $$gen{sig}{type} -> $type"
      unless defined $conversions{$k};
DEBUG_END
    $conversions{$k} % {body => $gen};
  }
}

sub gen_li_conversion {
  my ($required, $have_so_far, $gen) = @_;
  return $gen if $have_so_far >= $required;

  # Generate optimized code in certain cases. Are we appending to @_ or
  # replacing it altogether?
  if ($have_so_far < 0) {
    # Replacing altogether, so no sense in generating any push() calls.
    my @vars = map(("f$_" => 0), 0 .. $required + 1);
    my $init = gen
      'li_init',
      {n => 0, @vars},
      join(";\n", q{%:n = length $_;},
                  map(qq{%:f$_ = 1 + index(\$_, "\\t", } . ($_ ? "%:f$_" : 0)
                                                         . qq{);\n} .
                      qq{%:f$_ = %:n if %:f$_ == 0;},
                      1 .. ($required + 1)),
                  q{@_ = (} . join(', ', map "%:f$_", 0 .. ($required + 1))
                            . q{);});

    gen "li_conversion:I$required",
        {init => $init, body => $gen},
        q{ %@init
           %@body };
  } else {
    my @vars = map(("f$_" => 0), $have_so_far + 1 .. $required + 1);
    my $init = gen
      'li_init',
      {n => 0, @vars},
      join(";\n", q{%:n = length $_;},
                  map(qq{%:f$_ = 1 + index(\$_, "\\t", } . ($_ ? "%:f$_"
                                                               : '$_[-1]')
                                                         . qq{);\n} .
                      qq{%:f$_ = %:n if %:f$_ == 0;},
                      ($have_so_far + 1) .. ($required + 1)),
                  q{@_ = (} . join(', ', '@_',
                                         map "%:f$_", ($have_so_far + 1)
                                                   .. ($required + 1))
                            . q{);});

    gen "li_conversion:I$required",
        {init => $init, body => $gen},
        q{ %@init
           %@body };
  }
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
