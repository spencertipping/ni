BEGIN {

# Bidirectional file IO
defio 'file',
sub { [@_] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    unless (ref $$self[0] eq 'GLOB') {
      open my $fh, $$self[0] or die "failed to open $$self[0]: $!";
      $$self[0] = $fh;
    }
    gen('file_source:VV', {fh => $$self[0]},
      q{ while (<%:fh>) {
           %@body
         } }) % {body => with_input_type('L', $destination)};
  },

  sink_gen => sub {
    my ($self) = @_;
    unless (ref $$self[1] eq 'GLOB') {
      open my $fh, $$self[1] or die "failed to open $$self[1]: $!";
      $$self[1] = $fh;
    }
    gen('file_sink:LV', {fh => $$self[1]}, q{ print %:fh $_; });
  },
};

defio 'memory',
sub { [@_] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    gen('memory_source:VV', {xs => $self},
      q{ for (@{%:xs}) {
           %@body
         } }) % {body => with_input_type('R', $destination)};
  },

  sink_gen => sub {
    my ($self) = @_;
    gen('memory_sink:RV', {xs => $self}, q{ push @{%:xs}, $_; });
  },
};

# Sum of multiple IOs
defio 'sum',
sub { [map $_->flatten, @_] },
{
  flatten    => sub { @{$_[0]} },
  source_gen => sub {
    my ($self, $destination) = @_;
    return gen('empty', {}, '') unless @$self;
    my ($first, @others) = @$self;
    my $gen = $first->source_gen($destination);
    $gen = gen('sum_source:VV', {}, q{ %@x; %@y }) %
      {x => $gen,
       y => $_->source_gen($destination)} for @others;
    $gen;
  },
};

# Concatenation of an IO of IOs
defio 'cat',
sub { \$_[0] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self->source_gen(gen('cat_source:VV', {dest => $destination},
      q{ $_->source_gen(%:dest)->run; }));
  },
};

# Introduces arbitrary indirection into an IO's code stream.
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen($$self{code_transform}($destination));
  },
};

sub invocation {
  my ($f, @args) = @_;
  if (@args || ref $f eq 'CODE' || $f =~ s/^;//) {
    # We need to generate a function call.
    gen('fn:FF', {f => compile($f), args => [@args]},
      q{ %:f->(@_, @{%:args}) });
  } else {
    # We can inline the expression to avoid function call overhead.
    gen('fn:FF', {}, q{ (%@f) }) % {f => $f};
  }
}

# Bindings for common transformations
sub flatmap_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("flatmap:${is}V", {},
      q{ for (%@invocation) {
           %@body
         } }) % {invocation => $i, body => with_input_type('R', $into)};
  };
}

sub mapone_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("mapone:${is}V", {},
      q{ if (@_ = %@invocation) {
           %@body
         } }) % {invocation => $i, body => with_input_type('F', $into)};
  };
}

sub grep_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("grep:${is}V", {},
      q{ if (%@invocation) {
           %@body
         } }) % {invocation => $i, body => with_input_type($is, $into)};
  };
}

sub reduce_binding {
  my ($f, $init) = @_;
  $f = compile $f;
  sub {
    my ($into) = @_;
    gen('reduce:FV', {f => $f, init => $init},
      q{ (%:init, @_) = %:f->(%:init, @_);
         for (@_) {
           %@body
         } }) % {body => with_input_type('R', $into)};
  };
}

}
