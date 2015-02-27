# Module support
# ni allows you to define modules and cat them onto the end. These will be
# saved in a --self image, and support some nice stuff like namespacing for
# short functions.

our %short_functions;
our %short_function_modules;

sub defshortfn {
  my ($name, $f) = @_;

  # Is this short function already defined? Complain and rename it, first using
  # module mnemonics and then by appending digits.
  if (exists $short_functions{$name}) {
    my $new_name = "${name}_";
    my $i = 0;
    while (exists $short_functions{$new_name}
           && $i < length $ni::current_module) {
      $new_name .= substr $ni::current_module, $i, 1;
      ++$i;
    }
    $i = 0;
    $new_name = ($new_name =~ s/\d+$//r) . $i++
      while exists $short_functions{$new_name};
    print STDERR "defshortfn: $name from module $ni::current_module is "
               . "already defined by module '$short_function_modules{$name}', "
               . "so defining it as $new_name instead\n";
    $name = $new_name;
  }

  $short_functions{$name}        = $f;
  $short_function_modules{$name} = $ni::current_module;
  *{"::$name"} = $f;
}

sub parse_modules {
  # Looks for NI_MODULE and NI_MODULE_END, evaluating each one as we finish
  # parsing it.
  my ($fh) = @_;
  my @modules;
  my $module_name;
  my @module_code;
  while (!/^NI_END_OF_MODULES$/ && defined($_ = <$fh>)) {
    if (/^\s*NI_MODULE (\w+)\s*$/) {
      $module_name = $1;
      @module_code = ();
    } elsif (/^\s*NI_MODULE_END\s*$/ || /^\s*NI_END_MODULE\s*$/) {
      push @modules, [$module_name, join '', @module_code];
      $module_name = undef;
    } elsif (defined $module_name) {
      push @module_code, $_;
    } elsif (/^\s*$/) {
      # Ignore this line
    } else {
      die "ni: found this stray line not inside a NI_MODULE:\n$_";
    }
  }

  die "ni: missing NI_MODULE_END for module $module_name"
    if defined $module_name;
  @modules;
}

sub run_module {
  my ($name, $code) = @{$_[0]};
  $ni::current_module = $name;
  my @result = eval qq{
    package ni::$name;
    BEGIN {
      *{"ni::${name}::\$_"} = *{"ni::\$_"} for keys %{ni::};
    }
    $code};

  undef $ni::current_module;
  die "ni: failed to execute module $name: $@" if $@;
  @result;
}

# modules are loaded from the outer <DATA>, but this works (somewhat
# paradoxically) because we're inside an eval already.
BEGIN {
  @ni::modules = parse_modules $ni::data_fh;
  run_module $_ for @ni::modules;
}
