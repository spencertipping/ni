use POSIX qw/dup2/;

sub next_or_empty {
  my ($fh) = @_;
  my $r = <$fh>;
  defined $r ? ($r) : ();
}

defio 'array',
      sub {
        my ($self, @xs) = @_;
        $self->{peek_buffer} = [@xs];
        $self;
      },
{
  _next => sub { () },
};

defio 'fifo',
      sub {
        my ($self) = @_;
        pipe $self->{out}, $self->{in};
        $self;
      },
{
  _next   => sub { next_or_empty $self->{out} },
  enqueue => sub { $self->{in}->print($_[1]); $self },
  close   => sub { close $self->{in};         $self },
}

defio 'sum',
      sub {
        my ($self, $sources) = @_;
        $self->{sources} = $sources;
        $self->{current} = undef;
        $self;
      },
{
  _next => sub {
    my ($self) = @_;
    my $n;
    until ($self->{sources}->eof || defined $n) {
      $self->{current} //= ni $self->{sources}->next;
      $n = $self->{current}->next;
      shift $self->{sources} unless defined $n;
    }
    defined $n ? ($n) : ();
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
        my ($self, $base, $in, $out) = @_;
        $self->{base}    = $base;
        $self->{in_cmd}  = $in;
        $self->{out_cmd} = $out;
        $self->{reader}  = undef;
        $self->{writer}  = undef;
        $self;
      },
{
  reader => sub {
    die "filter object " . $_[0]->name . " created without a reader"
      unless defined $self->{in_cmd};
    $_[0]->{reader} //= $_[0]->{base}->pipe($_[0]->{in_cmd});
  },

  writer => sub {
    die "filter object " . $_[0]->name . " created without a writer"
      unless defined $_[0]->{out_cmd};
    $_[0]->{writer} //=
      ni::io::process->new($_[0]->{out_cmd})->from($_[0]->{base});
  },

  enqueue => sub {
    $_[0]->reader->enqueue($_[1]);
    $_[0];
  },

  close => sub {
    close $_[0]->reader;
    $_[0];
  },

  _next => sub {
    my $v = $_[0]->writer->next;
    defined $v ? ($v) : ();
  },
};
