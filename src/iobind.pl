# Bindings for common transformations
sub flatmap_binding {
  my @args = @_;
  ["flatmap @args", sub {
    my ($into, $type) = @_;
    my $i = invocation $type, @args;
    gen "flatmap:$type", {invocation => $i,
                          body       => $into->sink_gen('O')},
      q{ for (%@invocation) {
           %@body
         } };
  }];
}

sub mapone_binding {
  my @args = @_;
  ["mapone @args", sub {
    my ($into, $type) = @_;
    my $i = invocation $type, @args;
    gen "mapone:$type", {invocation => $i,
                         body       => $into->sink_gen('F')},
      q{ if (@_ = %@invocation) {
           %@body
         } };
  }];
}

sub grep_binding {
  my @args = @_;
  ["grep @_", sub {
    my ($into, $type) = @_;
    my $i = invocation $type, @_;
    gen "grep:$type", {invocation => $i,
                       body       => $into->sink_gen($type)},
      q{ if (%@invocation) {
           %@body
         } };
  }];
}

sub reduce_binding {
  my ($f, $init, @args) = @_;
  ["reduce $f $init @args", sub {
    my ($into, $type) = @_;
    my $i = invocation $type, $f;
    with_type $type,
      gen 'reduce:F', {f    => $f,
                       init => $init,
                       body => $into->sink_gen('O')},
        q{ (%:init, @_) = %:f->(%:init, @_);
           for (@_) {
             %@body
           } };
  }];
}

# Stream manipulation
sub tee_binding {
  my ($tee) = @_;
  ["tee $tee", sub {
    my ($into, $type) = @_;
    my ($save, $recover) = typed_save_recover $type;
    gen_seq "tee:$type", $save,    $tee->sink_gen($type),
                         $recover, $into->sink_gen($type);
  }];
}

sub take_binding {
  my ($n) = @_;
  die "must take a positive number of elements" unless $n > 0;
  ["take $n", sub {
    my ($into, $type) = @_;
    gen "take:${type}", {body      => $into->sink_gen($type),
                         remaining => $n},
      q{ %@body;
         return if --%:remaining <= 0; };
  }];
}

sub drop_binding {
  my ($n) = @_;
  ["drop $n", sub {
    my ($into, $type) = @_;
    gen "take:${type}", {body      => $into->sink_gen($type),
                         remaining => $n},
      q{ if (--%:remaining < 0) {
           %@body
         }};
  }];
}
