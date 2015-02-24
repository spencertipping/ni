sub invocation {
  my ($f, @args) = @_;
  if (@args || ref $f eq 'CODE' || $f =~ s/^;//) {
    # We need to generate a function call.
    @args
      ? gen('fn:FF', {f => compile($f), args => [@args]},
            q{ %:f->(@_, @{%:args}) })
      : gen('fn:FF', {f => compile($f)}, q{ %:f->(@_) });
  } else {
    # We can inline the expression to avoid function call overhead.
    gen('fn:FF', {f => $f}, q{ (%@f) });
  }
}

# Bindings for common transformations
sub flatmap_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "flatmap:${is}V", {invocation => $i,
                             body       => $into->sink_gen('R')},
        q{ for (%@invocation) {
             %@body
           } };
  };
}

sub mapone_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "mapone:${is}V", {invocation => $i,
                            body       => $into->sink_gen('F')},
        q{ if (@_ = %@invocation) {
             %@body
           } };
  };
}

sub grep_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen "grep:${is}V", {invocation => $i,
                          body       => $into->sink_gen($is)},
        q{ if (%@invocation) {
             %@body
           } };
  };
}

sub reduce_binding {
  my ($f, $init) = @_;
  $f = compile $f;
  sub {
    my ($into, $type) = @_;
    with_input_type $type,
      gen 'reduce:FV', {f    => $f,
                        init => $init,
                        body => $into->sink_gen('R')},
        q{ (%:init, @_) = %:f->(%:init, @_);
           for (@_) {
             %@body
           } };
  };
}

# Stream manipulation
sub tee_binding {
  my ($tee) = @_;
  sub {
    my ($into, $type) = @_;
    my $init    = gen 'tee_init', {x => []},
                      $type eq 'F' ? q{ @{%:x} = @_ } : q{ %:x = $_ };

    my $recover = gen('tee_recover', {x => []},
                      $type eq 'F' ? q{ @_ = @{%:x} } : q{ $_ = %:x })
                  ->inherit_gensyms_from($init);

    gen_seq "tee:${type}V", $init,    $tee->sink_gen($type),
                            $recover, $into->sink_gen($type);
  };
}
