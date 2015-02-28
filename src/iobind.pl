# Bindings for common transformations
sub deffnbinding {
  my ($name, $bodytype, $body) = @_;
  *{"ni::${name}_binding"} = sub {
    my ($f) = @_;
    ["$name $f", sub {
      my ($into, $type) = @_;
      my ($fc, $required_type) = fn($f, $type);
      my ($each, $end) = $into->sink_gen($bodytype || $required_type);
      with_type($type,
        gen "$name:$required_type",
            {f    => $fc,
             body => $each},
            $body), $end;
    }];
  };
}

BEGIN {

sub ::row;
deffnbinding 'flatmap', 'F', q{ *{"::row"} = sub { %@body }; %@f };
deffnbinding 'mapone',  'F', q{ if (@_ = (%@f)) { %@body } };
deffnbinding 'grep',    '',  q{ if (%@f)        { %@body } };

}

sub reduce_binding {
  my ($f, $init) = @_;
  ["reduce $f $init", sub {
    my ($into, $type) = @_;
    my ($each, $end)  = $into->sink_gen('O');
    with_type($type,
      gen 'reduce:F', {f    => fn($f),
                       init => $init,
                       body => $each},
        q{ (%:init, @_) = %:f->(%:init, @_);
           for (@_) {
             %@body
           } }), $end;
  }];
}

# Stream manipulation
sub tee_binding {
  my ($tee) = @_;
  ["tee $tee", sub {
    my ($into, $type) = @_;
    my ($save, $recover) = typed_save_recover $type;
    my ($tee_each,  $tee_end)  = $tee->sink_gen($type);
    my ($into_each, $into_end) = $into->sink_gen($type);

    gen_seq("tee:$type", $save,    $tee->sink_gen($type),
                         $recover, $into->sink_gen($type)),
    gen_seq("tee_end:$type", $tee_end, $into_end);
  }, sub { $tee->close_writer }];
}

sub take_binding {
  my ($n) = @_;
  die "must take a positive number of elements" unless $n > 0;
  ["take $n", sub {
    my ($into, $type) = @_;
    my ($each, $end)  = $into->sink_gen($type);
    gen("take:${type}", {body      => $each,
                         end       => $end,
                         remaining => $n},
      q{ %@body;
         if (--%:remaining <= 0) {
           %@end
           die 'DONE';
         } }), $end;
  }];
}

sub drop_binding {
  my ($n) = @_;
  ["drop $n", sub {
    my ($into, $type) = @_;
    my ($each, $end)  = $into->sink_gen($type);
    gen("take:${type}", {body      => $each,
                         remaining => $n},
      q{ if (--%:remaining < 0) {
           %@body
         } }), $end;
  }];
}

sub uniq_binding {
  my ($count, @fields) = @_;
  ["uniq $count @fields", sub {
    
  }];
}

sub zip_binding {
  my ($other) = @_;
  ["zip $other", sub {
    my ($into, $type) = @_;
    my ($each, $end)  = $into->sink_gen('F');
    my $other_source  = $other->reader_fh;

    with_type($type,
      gen 'zip:F', {body  => $each,
                    live  => 1,
                    other => $other_source,
                    l     => ''},
        q{ %:live &&= defined(%:l = <%:other>);
           chomp %:l;
           @_ = (@_, split /\t/, %:l);
           %@body }), $end;
  }, sub { $other->close_reader }];
}


