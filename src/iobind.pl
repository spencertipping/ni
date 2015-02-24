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
    my ($into) = @_;
    gen("flatmap:${is}V", {invocation => $i,
                           body       => with_input_type('R', $into)},
      q{ for (%@invocation) {
           %@body
         } });
  };
}

sub mapone_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("mapone:${is}V", {invocation => $i,
                          body       => with_input_type('F', $into)},
      q{ if (@_ = %@invocation) {
           %@body
         } });
  };
}

sub grep_binding {
  my $i  = invocation @_;
  my $is = input_sig $i;
  sub {
    my ($into) = @_;
    gen("grep:${is}V", {invocation => $i,
                        body       => with_input_type($is, $into)},
      q{ if (%@invocation) {
           %@body
         } });
  };
}

sub reduce_binding {
  my ($f, $init) = @_;
  $f = compile $f;
  sub {
    my ($into) = @_;
    gen('reduce:FV', {f    => $f,
                      init => $init,
                      body => with_input_type('R', $into)},
      q{ (%:init, @_) = %:f->(%:init, @_);
         for (@_) {
           %@body
         } });
  };
}
