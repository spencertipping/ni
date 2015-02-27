# Bindings for common transformations
sub deffnbinding {
  my ($name, $bodytype, $body) = @_;
  *{"ni::${name}_binding"} = sub {
    my ($f) = @_;
    ["$name $f", sub {
      my ($into, $type) = @_;
      my ($fc, $required_type) = fn($f, $type);
      with_type $type,
        gen "$name:$required_type",
            {f    => $fc,
             body => $into->sink_gen($bodytype || $required_type)},
            $body;
    }];
  };
}

BEGIN {

deffnbinding 'flatmap', 'O', q{ for (%@f)       { %@body } };
deffnbinding 'mapone',  'F', q{ if (@_ = (%@f)) { %@body } };
deffnbinding 'grep',    '',  q{ if (%@f)        { %@body } };

}

sub reduce_binding {
  my ($f, $init) = @_;
  ["reduce $f $init", sub {
    my ($into, $type) = @_;
    with_type $type,
      gen 'reduce:F', {f    => fn($f),
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

sub zip_binding {
  my ($other) = @_;
  ["zip $other->explain", sub {
    my ($into, $type) = @_;
    my $other_source = $other->reader_fh;

    with_type $type,
      gen 'zip:F', {body  => $into->sink_gen('F'),
                    other => $other_source,
                    l     => ''},
        q{ chomp(%:l = <%:other>);
           @_ = (@_, split /\t/, %:l);
           %@body };
  }];
}
