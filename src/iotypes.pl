use POSIX qw/dup2/;

defio 'sum',
      sub { 0 },
      sub {
        my ($self, @sources) = @_;
        $self->{sources} = [map ::ni($_), @sources];
        $self;
      },
{
  _avail => sub { 0 },
  _next  => sub {
    my ($self) = @_;
    my $n;
    until (!@{$self->{sources}} || defined $n) {
      $n = $self->{sources}->[0]->next;
      shift $self->{sources} unless defined $n;
    }
    defined $n ? ($n) : ();
  },
};

defio 'map',
      sub { 0 },
      sub {
        my ($self, $source, $f, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{args}   = [@args];
        $self;
      },
{
  _avail => sub { 0 },
  _next  => sub {
    my ($self) = @_;
    my $next = $self->{source}->next;
    return () unless defined $next;
    $self->{f}->($next, @{$self->{args}});
  },
};

defio 'reduce',
      sub { 0 },
      sub {
        my ($self, $source, $f, $init, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{init}   = $init;
        $self->{args}   = [@args];
        $self;
      },
{
  _avail => sub { 0 },
  _next  => sub {
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
      sub { 0 },
      sub {
        my ($self, $source, $f, @args) = @_;
        $self->{source} = $source;
        $self->{f}      = compile $f;
        $self->{args}   = [@args];
        $self;
      },
{
  _avail => sub { 0 },
  _next  => sub {
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
      sub { ref $_[0] eq 'GLOB' || $_[0] =~ /^[><|]|\|$/},
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

  _next => sub {
    my $fh = $_[0]->{fh};
    my $n  = <$fh>;
    defined $n ? ($n) : ();
  },

  _avail => sub { 0 },
};

defio 'process',
      sub { $_[0] =~ s/^sh:// },
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

  _avail => sub { 0 },        # FIXME?
};
