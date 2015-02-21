=head1 ni::io

Lazy, functional IO streams with transformation operators. This class gets
extended dynamically by other ni extensions, and its methods are exposed as ni
command-line options.

=cut

BEGIN {ni::extend {
  @ni::io_types        = ();
  %ni::io_detectors    = ();
  %ni::io_constructors = ();

  sub ni {
    my ($thing, @args) = @_;
    return $thing if ref $thing && $thing->isa(ni::io);
    for (@ni::io_types) {
      return $ni::io_constructors{$_}->("ni::io::$_", $thing, @args)
        if $ni::io_detectors{$_}->($thing, @args);
    }
    die "ni: don't know how to construct instance for $thing @args";
  }

  sub ni::defio {
    my ($name, $detector, $constructor, $methods) = @_;
    push @ni::io_types, $name;
    $ni::io_detectors{$name} = $detector;
    *{"ni::io::${name}::new"} = $ni::io_constructors{$name} = sub {
      my ($class, @args) = @_;
      bless $constructor->(ni::io->new, @args), $class;
    };
    *{"ni::io::$name::$_"} = $methods->{$_} for keys %$methods;
    push @{"ni::io::${name}::ISA"}, 'ni::io';
  }

  package ni::io;
  use overload qw# +  plus  * map  / reduce  % grep
                   <> next  0+ avail  "" name  ! eof
                   |  pipe
                   >  into  >> copy
                   <  from  << enqueue #;

  sub new {
    my ($class) = @_;
    bless {eof         => 0,
           peek_buffer => [],
           listeners   => []}, $class;
  }

  # implemented by subclasses
  sub _avail  { ... }   # number of next() calls that will not block
  sub _next   { ... }
  sub enqueue { die "ni::io object " . $self->name . " cannot be written to" }
  sub close   { die "ni::io object " . $self->name . " cannot be closed" }
  sub name    { ref $_[0] }

  sub eof { $_[0]->{eof} }

  sub next {
    my ($self) = @_;
    return undef if $self->{eof};
    push $self->{peek_buffer}, $self->_next
      unless @{$self->{peek_buffer}};
    my $next = shift $self->{peek_buffer};
    if (defined $next) {
      $_->enqueue($next) for @{$self->{listeners}};
      return $next;
    } else {
      $_->close for @{$self->{listeners}};
      $self->{eof} = 1;
      return undef;
    }
  }

  sub peek {
    my ($self, $n) = @_;
    my $x;
    push $self->{peek_buffer}, $x
      while defined($x = $self->next) && @{$self->{peek_buffer}} < $n;
    @{$self->{peek_buffer}}[0..$n];
  }

  sub slice {
    my ($self, $lower, $upper) = @_;
    return () if $upper <= $lower;
    @{$self->peek($upper)}[$lower .. $upper - 1];
  }

  sub avail {
    my ($self) = @_;
    $self->_avail + scalar @{$_[0]->{peek_buffer}};
  }

  sub copy {
    my ($self, $dest) = @_;
    push $self->{listeners}, ::ni $dest;
    $self;
  }

  sub into {
    # Forwards all contents into the given io, blocking until complete.
    # WARNING: this function leaves the destination open afterwards.
    my ($self, $dest) = @_;
    $dest = ::ni $dest;
    my $x;
    $dest->enqueue($x) while defined($x = $self->next);
    $dest;
  }

  sub from {
    # Sources from the given thing, closing afterwards.
    my ($self, $source) = @_;
    unless (fork) {
      ::ni($source)->into($self);
      $self->close;
      exit;
    }
    $self->close;
    $self;
  }

  sub pipe {
    my ($self, $command) = @_;
    my $process = ni::io::process->new($command);
    unless (fork) {
      $self->into($process);
      $process->close;
      exit;
    }
    $process->close;
    $process;
  }

  sub plus { ni::io::sum->new(grep $_, @_) }
  sub map  { ni::io::map->new(grep $_, @_) }
  sub grep { ni::io::grep->new(grep $_, @_) }
  sub reduce {
    my ($self, $f, $init, @xs) = @_;
    ni::io::reduce->new($self, $f, $init // {}, @xs);
  }
}}
