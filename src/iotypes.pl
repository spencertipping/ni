BEGIN {

use POSIX qw/dup2/;

sub to_fh {
  return undef unless defined $_[0];
  return $_[0]->() if ref $_[0] eq 'CODE';
  return $_[0]     if ref $_[0] eq 'GLOB';
  open my $fh, $_[0] or die "failed to open $_[0]: $!";
  $fh;
}

# Bidirectional filehandle IO with lazy creation
defio 'file',
sub { +{reader => $_[0], writer => $_[1]} },
{
  reader_fh => sub {
    my ($self) = @_;
    die "io not configured for reading" unless $self->supports_reads;
    $$self{reader} = to_fh $$self{reader};
  },

  writer_fh => sub {
    my ($self) = @_;
    die "io not configured for writing" unless $self->supports_writes;
    $$self{writer} = to_fh $$self{writer};
  },

  supports_reads  => sub { defined ${$_[0]}{reader} },
  supports_writes => sub { defined ${$_[0]}{writer} },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen('file_source:VV', {fh   => $self->reader_fh,
                           body => with_input_type('L', $destination)},
      q{ while (<%:fh>) {
           %@body
         } });
  },

  sink_gen => sub {
    my ($self) = @_;
    gen('file_sink:LV', {fh => $self->writer_fh}, q{ print %:fh $_; });
  },

  close => sub { close $_[0]->writer; $_[0] },
};

# An array of stuff in memory
defio 'memory',
sub { [@_] },
{
  supports_writes => sub { 1 },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen('memory_source:VV', {xs   => $self,
                             body => with_input_type('R', $destination)},
      q{ for (@{%:xs}) {
           %@body
         } });
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
    gen_seq('sum_source:VV', map $_->source_gen($destination), @$self);
  },
};

# Tee: sink an IO into multiple other IOs at once (should probably be a
# binding, not an io)
defio 'tee',
sub { +{source => $_[0], watchers => [@_]} },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    return $$self{source}->source_gen($destination)
      unless @{$$self{watchers}};

    # Find all input types we need and generate gensyms for them. This way we
    # don't need to have each destination do its own conversion.
    my @sink_gens = map $_->sink_gen, @{$$self{watchers}};
    my @sink_sigs = map input_sig($_), @sink_gens;
    my %sink_types;
    ++$sink_types{$_} for @sink_sigs;
    $sink_types{L} |= $sink_types{R} |= $sink_types{F};

    # TODO: if all sinks are type F, then just use @_ and don't force anything
    # back into $_. That way it's possible to pass references around.

    my %type_init = (
      L => gen('sink_store:LV', {l  => undef}, '%:l = $_'),
      R => gen('sink_store:RV', {r  => undef}, '%:r = %:l =~ s/\n$//r'),
      F => gen('sink_store:FV', {xs => []},    '@{%:xs} = split /\t/, %:r'),
    );

    my %sink_init = (
      L => gen('sink_init:VL', {l  => undef}, '$_ = %:l'),
      R => gen('sink_init:VR', {r  => undef}, '$_ = %:r'),
      F => gen('sink_init:VF', {xs => []},    '@_ = @{%:xs}'),
    );

    $sink_init{$_}->inherit_gensyms_from($type_init{$_})
    for keys %sink_init;

    my $init = gen_seq('type_init:LV',
      map $type_init{$_},
      grep $sink_types{$_}, qw/L R F/);

    # Now prepend the requisite initialization to each sink invocation. We
    # can't reuse because sinks might involve loops, etc.
    $$self{source}->source_gen(
      gen_seq('tee_sink:LV',
              $init,
              $destination,
              map gen_seq('sink_with_init:VV',
                          $sink_init{$sink_sigs[$_]},
                          $sink_gens[$_]),
                  0 .. $#sink_gens));
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

# Introduces arbitrary indirection into an IO's code stream
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen($$self{code_transform}($destination));
  },
};

# A file-descriptor pipe
defioproxy 'pipe', sub {
  pipe my $out, my $in or die "pipe failed: $!";
  ni_file($out, $in);
};

# Stdin/stdout of an external process with stdin, stdout, neither, or both
# redirected to the specified fhs. If you don't specify them, this function
# creates pipes and returns an io wrapping them.
defioproxy 'process', sub {
  my ($command, $stdin_fh, $stdout_fh) = @_;
  my $stdin_writer;
  my $stdout_reader;
  pipe $stdin_fh, $stdin_writer   or die "pipe: $!" unless defined $stdin_fh;
  pipe $stdout_reader, $stdout_fh or die "pipe: $!" unless defined $stdout_fh;

  unless (fork) {
    close STDIN;
    close STDOUT;
    close $stdin_writer  if defined $stdin_writer;
    close $stdout_reader if defined $stdout_reader;
    dup2 fileno($stdin_fh), 0  or die "dup2 failed: $!";
    dup2 fileno($stdout_fh), 1 or die "dup2 failed: $!";
    exec $command or die "failed to exec: $!";
  }
  ni_file($stdout_reader, $stdin_writer);
};

# Filtered through shell processes
defioproxy 'filter', sub {
  my ($base, $read_filter, $write_filter) = @_;
  ni_file(
    $base->supports_reads && defined $read_filter
      ? sub {ni_process($read_filter, $base->reader_fh, undef)->reader_fh}
      : undef,
    $base->supports_writes && defined $write_filter
      ? sub {ni_process($write_filter, undef, $base->writer_fh)->writer_fh}
      : undef);
};

}
