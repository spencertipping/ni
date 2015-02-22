#!/usr/bin/env perl
eval($ni::selfcode = join '', <DATA>); die $@ if $@;
__DATA__
use v5.14;
no strict 'refs';
sub ni;
package ni;
sub ni;
sub self {
  join "\n", "#!/usr/bin/env perl",
             q{eval($ni::selfcode = join '', <DATA>); die $@ if $@;},
             "__DATA__",
             $ni::selfcode;
}
# Memoized function compilation
our %compiled_functions;

sub expand_function_shorthands {
  my ($code) = @_;
  $code =~ s/%(\d+)/\$_[$1]/g;

  # JSON shortcuts
  1 while $code =~ s/([a-zA-Z0-9_\)\}\]\?\$])
                     \.
                     ([\$_a-zA-Z](?:-[0-9\w\?\$]|[0-9_\w?\$])*)
                    /$1\->{'$2'}/x;
  $code;
}

sub compile {
  return $_[0] if ref $_[0] eq 'CODE';
  return $compiled_functions{$_[0]}
     //= eval "package::; sub {\n" . expand_function_shorthands($_[0])
                                   . "\n}";
}
our %io_constructors;

sub hot {
  my ($fh) = @_;
  select((select($fh), $|++)[0]);
  $fh;
}

sub defio {
  my ($name, $constructor, $methods) = @_;
  *{"ni::io::${name}::new"} = $io_constructors{$name} = sub {
    my ($class, @args) = @_;
    bless $constructor->(ni::io->new, @args), $class;
  };
  *{"::ni_$name"} = *{"ni::ni_$name"} =
    sub { ${"ni::io::${name}::"}{new}("ni::io::$name", @_) };
  *{"ni::io::$name::$_"} = $methods->{$_} for keys %$methods;
  push @{"ni::io::${name}::ISA"}, 'ni::io';
}

{
  package ni::io;
  use overload qw# +  plus_op  * bind_op  / reduce_op  % grep_op
                             >>= bind_op

                   <> next  0+ avail  "" name  ! eof  bool not_eof
                   |  pipe
                   >  into     >> copy
                   <  from_op  << enqueue #;

  sub new {
    my ($class) = @_;
    bless {eof         => 0,
           peek_buffer => [],
           listeners   => []}, $class;
  }

  sub empty {
    my $io = ni::io->new;
    $io->{eof} = 1;
    $io;
  }

  # implemented by subclasses
  sub _avail  { 0 }
  sub _next   { ... }
  sub name    { '[]' }
  sub enqueue { die "ni::io object " . $_[0]->name . " cannot be written to" }
  sub close   { die "ni::io object " . $_[0]->name . " cannot be closed" }

  sub eof     {  $_[0]->{eof} }
  sub not_eof { !$_[0]->{eof} }

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
    my @xs;
    return () if $n <= 0;
    push $self->{peek_buffer}, @xs
      while (@xs = $self->_next) && @{$self->{peek_buffer}} < $n;
    @{$self->{peek_buffer}}[0..($n - 1)];
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
    $dest = ::ni($dest);
    my $x;
    $dest->enqueue($x) while defined($x = $self->next);
    $dest;
  }

  sub from {
    # Sources from the given thing(s), closing afterwards.
    my ($self, @sources) = @_;
    for (@sources) {
      unless (fork) {
        ni($_)->into($self);
        $self->close;
        exit;
      }
    }
    $self->close;
    $self;
  }

  sub from_op { $_[0]->from($_[1]) }

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

  sub plus_op   { ni::io::sum   ->new(ni::io::array->new(@_[0, 1])) }
  sub bind_op   { ni::io::map   ->new(@_[0, 1]) }
  sub grep_op   { ni::io::grep  ->new(@_[0, 1]) }
  sub reduce_op { ni::io::reduce->new(@_[0, 1], {}) }

  sub plus   { ni::io::sum   ->new(ni::io::array->new(@_)) }
  sub bind   { ni::io::map   ->new(@_) }
  sub grep   { ni::io::grep  ->new(@_) }
  sub reduce { ni::io::reduce->new(@_) }
}
# Data source definitions
our @data_names;
our %data_matchers;
our %data_transformers;

sub defdata {
  my ($name, $matcher, $transfomer) = @_;
  die "data type $name is already defined" if exists $data_matchers{$name};
  push @data_names, $name;
  $data_matchers{$name}     = $matcher;
  $data_transformers{$name} = $transfomer;
}

sub ni_io_for {
  my ($f, @args) = @_;
  for my $n (@data_names) {
    return $data_transformers{$n}->($f, @args)
      if $data_matchers{$n}->($f, @args);
  }
  die "$f does not match any known ni::io constructor";
}

sub ::ni {
  my ($f, @args) = @_;
  return undef unless defined $f;
  return $f if ref $f && $f->isa('ni::io');
  return ni_io_for($f, @args);
}

*{"ni::ni"} = *{"::ni"};
defdata 'globfile', sub { ref $_[0] eq 'GLOB' }, sub { ni_fh($_[0]) };

sub deffilter {
  my ($extension, $read, $write) = @_;
  $extension = qr/\.$extension$/;
  defdata $extension,
    sub { $_[0] =~ /$extension/ },
    sub { ni_filter(ni_fh("+< $_[0]"), $read, $write) };
}

deffilter 'gz',  'gzip -d',  'gzip';
deffilter 'lzo', 'lzop -d',  'lzop';
deffilter 'xz',  'xz -d',    'xz';
deffilter 'bz2', 'bzip2 -d', 'bzip2';

sub rw_file {
  my $fh;
  open $fh, "+< $_[0]" or open $fh, "< $_[0]"
    or die "couldn't open $_[0] for reading or r/w: $!";
  ni $fh;
}

defdata 'file', sub { -e $_[0] || $_[0] =~ s/^file:// }, sub { rw_file $_[0] };
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
# High-level pipe operations, each of which corresponds to a command-line
# option. They can also be used from compiled code.

our %op_shorthand_lookups;      # keyed by short
our %op_shorthands;             # keyed by long
our %op_formats;                # ditto
our %op_usage;                  # ditto
our %op_fns;                    # ditto

sub defop {
  my ($long, $short, $format, $usage, $fn) = @_;
  if (defined $short) {
    $op_shorthands{$long}         = $short;
    $op_shorthand_lookups{$short} = "--$long";
  }
  $op_formats{$long} = $format;
  $op_usage{$long}   = $usage;
  $op_fns{$long}     = $fn;

  die "operator $long already exists (possibly as a method rather than an op)"
    if exists $ni::io::{$long};

  *{"ni::io::$long"} = $fn;     # programmatic access
}

our %format_matchers = (
  a => qr/^[a-zA-Z]+$/,
  d => qr/^[-+\.0-9]+$/,
  s => qr/^.*$/,
  v => qr/^[^-].*$/,
);

sub apply_format {
  my ($format, @args) = @_;
  my @format = split //, $format;
  my @parsed;

  for (@format) {
    die "too few arguments for $format" if !@args && !/[A-Z]/;
    my $a = shift @args;
    if ($a =~ /$format_matchers{lc $_}/) {
      push @parsed, $a;
    } else {
      die "failed to match format $format" unless /[A-Z]/;
      push @parsed, undef;
    }
  }

  [@parsed], @args;
}

sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map ['plus', $_], @_ if $o eq '--';
    if ($o =~ /^--/) {
      my $c = $o =~ s/^--//r;
      die "unknown long command: $o" unless exists $op_fns{$c};
      my ($args, @rest) = apply_format $op_formats{$c}, @_;
      push @parsed, [$c, @$args];
      @_ = @rest;
    } elsif ($o =~ s/^-//) {
      unshift @_, map $op_shorthand_lookups{$_} // $_,
                      $o =~ /([:+^=%\/]?[a-zA-Z]|[-+\.0-9]+)/g;
    } else {
      push @parsed, ['plus', $o];
    }
  }
  @parsed;
}
# Operator implementations

use File::Temp qw/tmpnam/;

# Meta
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni::io::array->new(self) };

defop 'explain', undef, '',
  'explains the current pipeline',
  sub { print STDERR $_[0]->name, "\n"; $_[0] };

# Functional transforms
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * record_transformer $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % record_transformer $_[1] };

defop 'deref', 'r', '',
  'interprets each record as a data source and emits it',
  sub { ni::io::sum->new($_[0] * \&::ni) };

defop 'ref', 'R', 'V',
  'collects data into a file and emits the filename',
  sub { my $f = $_[1] // tmpnam;
        $_[0]->into(ni $_[1]);
        ni::io::array->new($f) };

defop 'branch', 'b', 's',
  'splits input by its first field, forwarding to subprocesses',
  sub {
    my ($in, $subprocesses) = @_;
    my @subs = unpack_branch_map $subprocesses;
    my $fifo = ni::io::fifo->new->from(map ${$_}[1], @subs);

    unless (fork) {
      my $line;
      while (defined($line = <$in>)) {
        my ($k, $v) = split /\t/, $line, 2;
        for my $s (@subs) {
          if ($s->[0]->($k)) {
            $s->[1]->enqueue($line);
            last;
          }
        }
      }
      exit;
    }
    $fifo;
  };

# Sorting (shells out to command-line sort)
sub sort_options {
  my @fieldspec = split //, $_[0] // '';
  # TODO
}

defop 'order', 'o', 'AD',
  'order {n|N|g|G|l|L|u|U|m} [fields]',
  sub {
    my ($in, $flags, $fields) = @_;
    $in | 'sort';
  };
# Preprocess command line, collapsing stuff into array and hash references as
# appropriate.

sub preprocess_cli {
  my @preprocessed;
  for (my $o; defined($o = shift @_);) {
    if ($o =~ s/\[$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\]$/;
        $depth += /\[$/;
        push @xs, $_;
      }
      push @preprocessed, bless [@xs], $o;
    } elsif ($o =~ s/\{$//) {
      my @xs;
      my $depth = 1;
      for (@_) {
        last unless $depth -= /^\}$/;
        $depth += /\{$/;
        push @xs, $_;
      }
      push @preprocessed, bless {@xs}, $o;
    } else {
      push @preprocessed, $o;
    }
  }
  @preprocessed;
}

$|++;
my $data = -t STDIN ? ni::io::empty : ni::io::fh->new(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $data = $ni::io::{$command}($data, @args);
}
$data->into(\*STDOUT);
