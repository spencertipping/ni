# Data source definitions
our @data_names;
our %data_matchers;
our %data_transformers;

sub defdata {
  my ($name, $matcher, $transfomer) = @_;
  die "data type $name is already defined" if exists $data_matchers{$name};
  push @data_names, $name;
  $data_matchers{$name}     = $matcher;
  $data_transformers{$name} = $transfomer;
}

sub ni_io_for {
  my ($f, @args) = @_;
  for my $n (@data_names) {
    return $data_transformers{$n}->($f, @args)
      if $data_matchers{$n}->($f, @args);
  }
  die "$f does not match any known ni::io constructor";
}

sub ::ni {
  my ($f, @args) = @_;
  return undef unless defined $f;
  return $f if ref $f && $f->isa('ni::io');
  return ni_io_for($f, @args);
}

*{"ni::ni"} = *{"::ni"};
