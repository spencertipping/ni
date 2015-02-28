BEGIN {

use File::Temp qw/tmpnam/;
use List::Util qw/min max/;
use POSIX qw/dup2 mkfifo/;

sub to_fh {
  return undef unless defined $_[0];
  return $_[0]->() if ref $_[0] eq 'CODE';
  return $_[0]     if ref $_[0] eq 'GLOB';
  open my $fh, $_[0] or die "failed to open $_[0]: $!";
  $fh;
}

# Partial implementations
defio 'sink_as',
sub { +{description => $_[0], f => $_[1]} },
{
  explain         => sub { "[sink as: " . ${$_[0]}{description} . "]" },
  supports_reads  => sub { 0 },
  supports_writes => sub { 1 },
  sink_gen        => sub { ${$_[0]}{f}->(@_[1..$#_]) },
};

defio 'source_as',
sub { +{description => $_[0], f => $_[1]} },
{
  explain    => sub { "[source as: " . ${$_[0]}{description} . "]" },
  source_gen => sub { ${$_[0]}{f}->(@_[1..$#_]) },
};

sub sink_as(&)   { ni_sink_as("[anonymous sink]", @_) }
sub source_as(&) { ni_source_as("[anonymous source]", @_) }

# Bidirectional filehandle IO with lazy creation
defio 'file',
sub {
  die "ni_file() requires three constructor arguments (got @_)" unless @_ >= 3;
  +{description => $_[0], reader => $_[1], writer => $_[2], on_close => $_[3]}
},
{
  explain => sub { ${$_[0]}{description} },

  reader_fh => sub {
    my ($self) = @_;
    die "io $self not configured for reading" unless $self->supports_reads;
    $$self{reader} = to_fh $$self{reader};
  },

  writer_fh => sub {
    my ($self) = @_;
    die "io $self not configured for writing" unless $self->supports_writes;
    $$self{writer} = to_fh $$self{writer};
  },

  supports_reads  => sub { defined ${$_[0]}{reader} },
  supports_writes => sub { defined ${$_[0]}{writer} },
  has_reader_fh   => sub { ${$_[0]}->supports_reads },
  has_writer_fh   => sub { ${$_[0]}->supports_writes },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'file_source', {fh   => $self->reader_fh,
                        body => $destination->sink_gen('L')},
      q{ while (<%:fh>) {
           %@body
         } };
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    with_type $type,
      gen 'file_sink:L', {fh => $self->writer_fh},
        q{ print %:fh $_; };
  },

  close_reader => sub {
    my ($self) = @_;
    if (ref $$self{reader} eq 'GLOB') {
      $$self{on_close}->($self, 0) if defined $$self{on_close};
      close $$self{reader};
      undef $$self{reader};
    }
    $self;
  },

  close_writer => sub {
    my ($self) = @_;
    if (ref $$self{writer} eq 'GLOB') {
      $$self{on_close}->($self, 1) if defined $$self{on_close};
      close $$self{writer};
      undef $$self{writer};
    }
    $self;
  },
};

# An array of stuff in memory
defio 'memory',
sub { [@_] },
{
  explain => sub {
    "[memory io of " . scalar(@{$_[0]}) . " element(s): "
                     . "[" . join(', ', @{$_[0]}[0 .. min(3, $#{$_[0]})],
                                        @{$_[0]} > 4 ? ("...") : ()) . "]]";
  },

  supports_writes => sub { 1 },
  process_local   => sub { 1 },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'memory_source', {xs   => $self,
                          body => $destination->sink_gen('O')},
      q{ for (@{%:xs}) {
           %@body
         } };
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    $type eq 'F'
      ? gen 'memory_sink:F', {xs => $self}, q{ push @{%:xs}, [@_]; }
      : with_type $type,
          gen 'memory_sink:O', {xs => $self}, q{ push @{%:xs}, $_; };
  },
};

# A ring buffer of a specified size
defio 'ring',
sub { die "ring must contain at least one element" unless $_[0] > 0;
      my $n = 0;
      +{xs       => [map undef, 1..$_[0]],
        overflow => $_[1],
        n        => \$n} },
{
  explain => sub {
    my ($self) = @_;
    "[ring io of " . min(${$$self{n}}, scalar @{$$self{xs}})
                   . " element(s)"
                   . ($$self{overflow} ? ", > $$self{overflow}]"
                                       : "]");
  },

  supports_writes => sub { 1 },
  process_local   => sub { 1 },

  source_gen => sub {
    my ($self, $destination) = @_;
    my $i     = ${$$self{n}};
    my $size  = @{$$self{xs}};
    my $start = max 0, $i - $size;

    # Emit two loops, one before and one after the break. This way we won't end
    # up doing a modulus per loop iteration.
    gen 'ring_source', {xs    => $$self{xs},
                        n     => $size,
                        end   => $i % $size,
                        i     => $start % $size,
                        body  => $destination->sink_gen('O')},
      q{ %:i = %@i;
         while (%:i < %@n) {
           $_ = ${%:xs}[%:i++];
           %@body
         }
         %:i = 0;
         while (%:i < %@end) {
           $_ = ${%:xs}[%:i++];
           %@body
         } };
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    if (defined $$self{overflow}) {
      with_type $type,
        gen "ring_sink:O", {xs   => $$self{xs},
                            size => scalar(@{$$self{xs}}),
                            body => $$self{overflow}->sink_gen('O'),
                            n    => $$self{n},
                            v    => 0,
                            i    => 0},
          q{ %:v = $_;
             %:i = ${%:n} % %@size;
             if (${%:n}++ >= %@size) {
               $_ = ${%:xs}[%:i];
               %@body
             }
             ${%:xs}[%:i] = %:v; };
    } else {
      gen "ring_sink:O", {xs   => $$self{xs},
                          size => scalar(@{$$self{xs}}),
                          n    => $$self{n}},
        q{ ${%:xs}[${%:n}++ % %@size] = $_; };
    }
  },
};

# Infinite source of repeated function application
defio 'iterate', sub { +{x => $_[0], f => $_[1]} },
{
  explain => sub {
    my ($self) = @_;
    "[iterate $$self{x} $$self{f}]";
  },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'iterate_source', {f    => fn($$self{f}),
                           x    => \$$self{x},
                           y    => 0,
                           body => $destination->sink_gen('O')},
      q{ while (1) {
           %:y = ${%:x};
           ${%:x} = %:f->(${%:x});
           $_ = %:y;
           %@body
         } };
  },
};

# Empty source, null sink
defio 'null', sub { +{} },
{
  explain         => sub { '[null io]' },
  supports_writes => sub { 1 },
  source_gen      => sub { gen 'empty', {}, '' },
  sink_gen        => sub { gen "null_sink:$_[1]V", {}, '' },
};

# Sum of multiple IOs
defio 'sum',
sub { [map $_->flatten, @_] },
{
  explain => sub {
    "[sum: " . join(' + ', @{$_[0]}) . "]";
  },

  transform  => sub {
    my ($self, $f) = @_;
    my $x = $f->($self);
    $x eq $self ? ni_sum(map $_->transform($f), @$self)
                : $x;
  },

  flatten    => sub { @{$_[0]} },
  source_gen => sub {
    my ($self, $destination) = @_;
    return gen 'empty', {}, '' unless @$self;
    gen_seq 'sum_source', map $_->source_gen($destination), @$self;
  },
};

# Concatenation of an IO of IOs
defio 'cat',
sub { \$_[0] },
{
  explain => sub { "[cat ${$_[0]}]" },

  source_gen => sub {
    my ($self, $destination) = @_;
    $$self->source_gen(sink_as {
      my ($type) = @_;
      with_type $type,
        gen 'cat_source:F',
            {dest => $destination},
            q{ $_[0]->into(%:dest, 1); }});
  },
};

# Introduces arbitrary indirection into an IO's code stream
defio 'bind',
sub {
  die "code transform must be [description, f, [ios...]]"
    unless ref $_[1] eq 'ARRAY';
  +{ base => $_[0], code_transform => $_[1], other_ios => $_[2] }
},
{
  explain => sub {
    my ($self) = @_;
    "$$self{base} >> $$self{code_transform}[0]";
  },

  supports_reads  => sub { ${$_[0]}{base}->supports_reads },
  supports_writes => sub { ${$_[0]}{base}->supports_writes },

  transform => sub {
    my ($self, $f) = @_;
    my $x = $f->($self);
    $x eq $self ? ni_bind($$self{base}->transform($f), $$self{code_transform})
                : $x;
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    $$self{code_transform}[1]->($$self{base}, $type);
  },

  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen(sink_as {
      my ($type) = @_;
      $$self{code_transform}[1]->($destination, $type);
    });
  },

  close_reader => sub {
    my ($self) = @_;
    $_->close_reader for $$self{base}, @{$$self{other_ios}};
    $self;
  },

  close_writer => sub {
    my ($self) = @_;
    $_->close_writer for $$self{base}, @{$$self{other_ios}};
    $self;
  },
};

# A file-descriptor pipe
defioproxy 'pipe', sub {
  pipe my $out, my $in or die "pipe failed: $!";
  select((select($out), $|++)[0]);
  select((select($in),  $|++)[0]);
  ni_file("[pipe in = " . fileno($in) . ", out = " . fileno($out). "]",
          $out, $in);
};

# A named FIFO
defioproxy 'fifo', sub {
  my ($name) = @_;
  mkfifo $name //= tmpnam, 0700 or die "mkfifo failed: $!";
  ni_file($name, "< $name", "> $name", sub {
    unlink $name or warn "failed to unlink fifo $name: $!";
  });
};

# A temporary file
defioproxy 'filename', sub {
  my ($name) = @_;
  $name //= tmpnam;
  ni_file($name, "< $name", "> $name");
};

# Stdin/stdout of an external process with stdin, stdout, neither, or both
# redirected to the specified ios. If you don't specify them, this function
# creates pipes and returns a lazy io wrapping them.
defioproxy 'process', sub {
  my ($command, $stdin_fh, $stdout_fh) = @_;
  my $stdin  = undef;
  my $stdout = undef;

  $stdin  = $stdin_fh,  $stdin_fh  = $stdin_fh->reader_fh  if is_io $stdin_fh;
  $stdout = $stdout_fh, $stdout_fh = $stdout_fh->writer_fh if is_io $stdout_fh;

  unless (defined $stdin_fh) {
    $stdin    = ni_pipe();
    $stdin_fh = $stdin->reader_fh;
  }

  unless (defined $stdout_fh) {
    $stdout    = ni_pipe();
    $stdout_fh = $stdout->writer_fh;
  }

  my $pid = undef;
  my $create_process = sub {
    return if defined $pid;
    unless ($pid = fork) {
      close STDIN;  $stdin->close_writer  if defined $stdin;
      close STDOUT; $stdout->close_reader if defined $stdout;
      dup2 fileno $stdin_fh,  0 or die "dup2 0 failed: $!";
      dup2 fileno $stdout_fh, 1 or die "dup2 1 failed: $!";
      exec $command or die "exec $command failed: $!";
    }
    close $stdin_fh;
    close $stdout_fh;
  };

  ni_file(
    "[process $command, stdin = $stdin, stdout = $stdout]",
    sub { $create_process->(); defined $stdout ? $stdout->reader_fh : undef },
    sub { $create_process->(); defined $stdin  ? $stdin->writer_fh  : undef });
};

# Filtered through shell processes
defioproxy 'filter', sub {
  my ($base, $read_filter, $write_filter) = @_;
  ni_file(
    "[filter $base, read = $read_filter, write = $write_filter]",
    $base->supports_reads && defined $read_filter
      ? sub {ni_process($read_filter, $base->reader_fh, undef)->reader_fh}
      : undef,
    $base->supports_writes && defined $write_filter
      ? sub {ni_process($write_filter, undef, $base->writer_fh)->writer_fh}
      : undef);
};

}
