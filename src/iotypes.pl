use POSIX qw/dup2/;
use B::Deparse;

our $deparser = B::Deparse->new;

sub next_or_empty {
  my ($fh) = @_;
  my $r = <$fh>;
  defined $r ? ($r) : ();
}

defio 'array',
      sub {
        my ($self, @xs) = @_;
        push $self->{peek_buffer}, @xs;
        $self;
      },
{
  name  => sub { "<array [@{$_[0]->{peek_buffer}}]>" },
  _next => sub { () },
};

defio 'fifo',
      sub {
        my ($self) = @_;
        pipe $self->{out}, $self->{in};
        $self;
      },
{
  name    => sub { "<fifo>" },
  _next   => sub { next_or_empty $_[0]->{out} },
  enqueue => sub { $_[0]->{in}->print($_[1]); $_[0] },
  close   => sub { close $_[0]->{in};         $_[0] },
};

defio 'sum',
      sub {
        my ($self, $sources) = @_;
        $self->{sources} = $sources;
        $self->{current} = $sources->next;
        $self;
      },
{
  name  => sub { "<sum " . ($_[0]->{current} ? $_[0]->{current}->name : '')
                         . join(' ', $_[0]->{sources}->peek(
                                       $_[0]->{sources}->avail))
                         . " ...>" },
  _next => sub {
    my ($self) = @_;
    return () unless defined $self->{current};
    my $n = $self->{current}->next;
    until (defined $n) {
      return () unless defined($self->{current} = ni $self->{sources}->next);
      $n = $self->{current}->next;
    }
    ($n);
  },
};

defio 'map',
      sub {
        my ($self, $source, $f, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{args}   = [@args];
        $self;
      },
{
  name  => sub { "<map " . $deparser->coderef2str($_[0]->{f})
                         . " [@{$_[0]->{args}}]"
                         . " " . $_[0]->{source}->name . ">" },
  _next => sub {
    my ($self) = @_;
    my $next = $self->{source}->next;
    return () unless defined $next;
    $self->{f}->($next, @{$self->{args}});
  },
};

defio 'reduce',
      sub {
        my ($self, $source, $f, $init, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{init}   = $init;
        $self->{args}   = [@args];
        $self;
      },
{
  name  => sub { "<reduce " . $deparser->coderef2str($_[0]->{f})
                            . " $_[0]->{init}"
                            . " [@{$_[0]->{args}}]"
                            . " " . $_[0]->{source}->name . ">" },
  _next => sub {
    my ($self) = @_;
    my @result;
    ($self->{init}, @result) = $self->{f}->($self->{init},
                                            $self->{source}->next,
                                            @{$self->{args}})
      until $self->{source}->eof || @result;
    @result;
  },
};

defio 'grep',
      sub {
        my ($self, $source, $f, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{args}   = [@args];
        $self;
      },
{
  name  => sub { "<grep " . $deparser->coderef2str($_[0]->{f})
                          . " [@{$_[0]->{args}}]"
                          . " " . $_[0]->{source}->name . ">" },
  _next => sub {
    my ($self) = @_;
    my $next;
    my $accept = 0;
    1 until $self->{source}->eof
         or $accept = $self->{f}->($next = $self->{source}->next,
                                   @{$self->{args}});
    $accept ? ($next) : ();
  },
};

defio 'fh',
      sub {
        my ($self, $open_expr) = @_;
        $self->{open_expr} = $open_expr;
        if (ref $open_expr eq 'GLOB') {
          $self->{fh} = $open_expr;
        } else {
          open $self->{fh}, $open_expr
            or die "failed to open $open_expr: $!";
        }
        hot $self->{fh};
        $self;
      },
{
  name    => sub { "<fh " . $_[0]->{open_expr} . ">" },
  enqueue => sub {
    $_[0]->{fh}->print($_[1]);
    $_[0];
  },

  close => sub {
    close $_[0]->{fh};
    $_[0];
  },

  _next => sub { next_or_empty $_[0]->{fh} },
};

defio 'process',
      sub {
        my ($self, @args) = @_;
        pipe my $in_r,        $self->{stdin};
        pipe $self->{stdout}, my $out_w;

        $self->{exec_args} = [@args];

        # Go ahead and start the process.
        unless ($self->{pid} = fork) {
          close STDIN;
          dup2 fileno($in_r),  0 or die "failed to connect stdin: $!";
          dup2 fileno($out_w), 1 or die "failed to connect stdout: $!";
          exec @args;
        }
        hot $self->{stdin};
        hot $self->{stdout};
        $self;
      },
{
  name => sub { "<process " . $_[0]->{pid}
                            . " [" . join(' ', @{$_[0]->{exec_args}}) . "]"
                            . ">" },

  enqueue => sub {
    $_[0]->{stdin}->print($_[1]);
    $_[0];
  },

  close => sub {
    close $_[0]->{stdin};
    $_[0];
  },

  _next => sub {
    my ($self) = @_;
    my $fh = $self->{stdout};
    my $v  = <$fh>;           # force scalar context
    defined $v ? ($v) : ();
  },
};

# Bidirectional filtered file thing
defio 'filter',
      sub {
        my ($self, $base, $read, $write) = @_;
        $self->{base}      = $base;
        $self->{read_cmd}  = $read;
        $self->{write_cmd} = $write;
        $self->{reader}    = undef;
        $self->{writer}    = undef;
        $self;
      },
{
  name => sub { "<filter [" . ($_[0]->{in_cmd}  // '') . "] "
                      . "[" . ($_[0]->{out_cmd} // '') . "] "
                      . $_[0]->{base}->name . ">" },

  reader => sub {
    $_[0]->{reader} //= $_[0]->{base}->pipe($_[0]->{read_cmd}
      // die "filter object " . $_[0]->name . " created without a reader");
  },

  writer => sub {
    unless (defined $_[0]->{writer}) {
      $_[0]->{writer} = ni_process($_[0]->{write_cmd}
        // die "filter object " . $_[0]->name . " created without a writer");
      $_[0]->{base}->from($_[0]->{writer});
    }
    $_[0]->{writer};
  },

  enqueue => sub {
    $_[0]->writer->enqueue($_[1]);
    $_[0];
  },

  close => sub {
    close $_[0]->writer;
    $_[0];
  },

  _next => sub {
    my $v = $_[0]->reader->next;
    defined $v ? ($v) : ();
  },
};
