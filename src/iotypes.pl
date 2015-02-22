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
    gen('from:file', {fh => $$self[0]},
      q{ while (<%fh>) {
           %<<body
         } }) % {body => $destination};
  },

  sink_gen => sub {
    my ($self) = @_;
    unless (ref $$self[1] eq 'GLOB') {
      open my $fh, $$self[1] or die "failed to open $$self[1]: $!";
      $$self[1] = $fh;
    }
    gen('into:file', {fh => $$self[1]}, q{ print %fh $_; });
  },
};

defio 'memory',
sub { [@_] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    gen('from:memory', {xs => $self},
      q{ for (@{%xs}) {
           %<<body
         } }) % {body => $destination};
  },

  sink_gen => sub {
    my ($self) = @_;
    gen('into:memory', {xs => $self}, q{ push @{%xs}, $_; });
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
    $gen = gen('+', {}, q{ %<<x; %<<y }) %
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
    $$self->source_gen(gen('cat:transform', {dest => $destination},
      q{ $_->source_gen(%dest)->run; }));
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

sub into_form {
  # TODO
  ...
}

sub invocation {
  my ($f, @args) = @_;
  if (@args || ref $f eq 'CODE' || $f =~ s/^#//) {
    # We need to generate a function call.
    gen('fn:AA', {f => $f, args => [@args]},
      q{ @_ = %f->(@_, @{%args}); });
  } else {
    # We can inline the expression to avoid any function call overhead.
    gen('fn:AA', {}, qq{ \@_ = (%<<f); }) % {f => $f};
  }
}

# Bindings for common transformations
sub flatmap_binding {
  my $i = invocation @_;
  sub {
    my ($into) = @_;
    gen('flatmap:AV', {},
      q{ %<<invocation
         for (@_) {
           %<<body
         } }) % {invocation => into_form('AL', $i), body => $into};
  };
}

sub mapone_binding {
  my $i = invocation @_;
  sub {
    my ($into) = @_;
    alternatives(
      gen('mapone:AA', {},
        q{ %<<invocation
           if (@_) {
             %<<body
           } }) % {invocation => $i, body => $into},

      gen('mapone:AL', {},
        q{ %<<invocation
           if (defined $_) {
             %<<body
           } }) % {invocation => $i, body => $into}
    );
  };
}

sub grep_binding {
  my $i = invocation @_;
  sub {
    my ($into) = @_;
    alternatives(
      gen('grep:AA', {},
        q{ %<<invocation
           if ($_) {
             %<<body
           } }) % {invocation => $i, body => $into},

      gen('grep:AL', {},
        q{ %<<invocation
           if ($_[0]) {
             %<<body
           } }) % {invocation => $i, body => $into}
    );
  };
}

sub reduce_binding {
  my ($f, $init, @args) = @_;
  my $args_ref = [@args];
  my $args_gensym = gensym 'args';
  my $init_gensym = gensym 'init';
  my $f_gensym    = gensym 'f';
  sub {
    my ($code, $refs) = @_;
    $refs->{$init_gensym} = $init;
    my $out_gensym = gensym 'results';
    if (ref $f eq 'CODE' || @args) {
      $refs->{$args_gensym} = $args_ref;
      $refs->{$f_gensym}    = compile $f;
      qq{ (\$$init_gensym, \@$out_gensym) =
          \$_[0]->{'$f_gensym'}->(\$$init_gensym, \$_,
                                  \@{\$_[0]->{'$args_gensym'}});
          for (\@$out_gensym) {
            $code
          } };
    } else {
      # Replace the custom symbol '%init' with the accumulator so the function
      # has a way to refer to it.
      $f =~ s/%init/\$$init_gensym/g;
      qq{ (\$$init_gensym, \@$out_gensym) = $f;
          for (\@$out_gensym) {
            $code;
          } };
    }
  };
}

}
