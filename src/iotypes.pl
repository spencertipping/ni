BEGIN {

use POSIX qw/dup2/;

sub to_fh {
  return undef unless defined $_[0];
  return $_[0]->() if ref $_[0] eq 'CODE';
  return $_[0]     if ref $_[0] eq 'GLOB';
  open my $fh, $_[0] or die "failed to open $_[0]: $!";
  $fh;
}

# Partial implementations
defio 'sink_as',
sub { \$_[0] },
{
  supports_reads  => sub { 0 },
  supports_writes => sub { 1 },
  sink_gen        => sub { ${$_[0]}->(@_[1..$#_]) },
};

defio 'source_as',
sub { \$_[0] },
{
  source_gen => sub { ${$_[0]}->(@_[1..$#_]) },
};

sub sink_as(&)   { ni_sink_as(@_) }
sub source_as(&) { ni_source_as(@_) }

# Bidirectional filehandle IO with lazy creation
defio 'file',
sub { +{reader => $_[0], writer => $_[1]} },
{
  reader_fh => sub {
    my ($self) = @_;
    die "io not configured for reading" unless $self->supports_reads;
    $$self{reader} = to_fh($$self{reader});
  },

  writer_fh => sub {
    my ($self) = @_;
    die "io not configured for writing" unless $self->supports_writes;
    $$self{writer} = to_fh($$self{writer});
  },

  supports_reads  => sub { defined ${$_[0]}{reader} },
  supports_writes => sub { defined ${$_[0]}{writer} },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'file_source:VV', {fh   => $self->reader_fh,
                           body => $destination->sink_gen('L')},
      q{ while (<%:fh>) {
           %@body
         } };
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    with_input_type $type,
      gen('file_sink:LV', {fh => $self->writer_fh}, q{ print %:fh $_; });
  },

  close => sub { close $_[0]->writer_fh; $_[0] },
};

# An array of stuff in memory
defio 'memory',
sub { [@_] },
{
  supports_writes => sub { 1 },

  source_gen => sub {
    my ($self, $destination) = @_;
    gen 'memory_source:VV', {xs   => $self,
                             body => $destination->sink_gen('L')},
      q{ for (@{%:xs}) {
           %@body
         } };
  },

  sink_gen => sub {
    my ($self, $type) = @_;
    $type eq 'F' ? gen 'memory_sink:FV', {xs => $self},
                       q{ push @{%:xs}, [@_]; }
                 : gen "memory_sink:${type}V",
                       {xs => $self},
                       q{ push @{%:xs}, $_; };
  },
};

# Sum of multiple IOs
defio 'sum',
sub { [map $_->flatten, @_] },
{
  flatten    => sub { @{$_[0]} },
  source_gen => sub {
    my ($self, $destination) = @_;
    return gen 'empty', {}, '' unless @$self;
    gen_seq 'sum_source:VV', map $_->source_gen($destination), @$self;
  },
};

# Concatenation of an IO of IOs
defio 'cat',
sub { \$_[0] },
{
  source_gen => sub {
    my ($self, $destination) = @_;
    $$self->source_gen(sink_as {
      gen 'cat_source:VV',
          {dest => $destination},
          q{ $_->source_gen(%:dest)->run; }});
  },
};

# Introduces arbitrary indirection into an IO's code stream
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  supports_reads  => sub { ${$_[0]}{base}->supports_reads },
  supports_writes => sub { ${$_[0]}{base}->supports_writes },

  sink_gen => sub {
    my ($self, $type) = @_;
    $$self{code_transform}->($$self{base}, $type);
  },

  source_gen => sub {
    my ($self, $destination) = @_;
    $$self{base}->source_gen(sink_as {
      my ($type) = @_;
      $$self{code_transform}->($destination, $type);
    });
  },

  close => sub { ${$_[0]}{base}->close; $_[0] },
};

# A file-descriptor pipe
defioproxy 'pipe', sub {
  pipe my $out, my $in or die "pipe failed: $!";
  ni_file($out, $in);
};

# Stdin/stdout of an external process with stdin, stdout, neither, or both
# redirected to the specified ios. If you don't specify them, this function
# creates pipes and returns an io wrapping them.
defioproxy 'process', sub {
  my ($command, $stdin, $stdout) = @_;
  $stdin  = defined $stdin  ? defined $stdin->reader_fh ? $stdin
                                                        : ni_pipe() <= $stdin
                            : ni_pipe();

  $stdout = defined $stdout ? defined $stdout->reader_fh ? $stdout
                                                         : ni_pipe() >= $stdout
                            : ni_pipe();
  unless (fork) {
    close STDIN;
    close STDOUT;
    close $stdin->writer_fh;
    close $stdout->reader_fh;
    dup2 fileno $stdin->reader_fh,  0 or die "dup2 failed: $!";
    dup2 fileno $stdout->writer_fh, 1 or die "dup2 failed: $!";
    exec $command or exit;
  }
  ni_file($stdout->reader_fh, $stdin->writer_fh);
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
