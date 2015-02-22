#!/usr/bin/env perl
eval($ni::selfcode = join '', <DATA>); die $@ if $@;
__DATA__
use v5.14;
no strict 'refs';
package ni;
sub ni;
sub ::ni;

sub self {
  join "\n", "#!/usr/bin/env perl",
             q{eval($ni::selfcode = join '', <DATA>); die $@ if $@;},
             "__DATA__",
             $ni::selfcode;
}

use POSIX qw/:sys_wait_h/;

$SIG{CHLD} = sub {
  local ($!, $?);
  waitpid -1, WNOHANG;
};
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
     //= eval "package main; sub {\n" . expand_function_shorthands($_[0])
                                      . "\n}";
}
# Extensible IO stream abstraction
# Streams are defined by the Perl code that runs in order to put their values
# somewhere. This abstraction ends up getting completely erased at runtime,
# which is good because Perl OO is really slow.
#
# If you want to construct one of these and use it for IO purposes, the fastest
# option should be to get a filehandle for it first:
#
# my $fh = $ni_io->into_fh;
# while (<$fh>) {
#   ...
# }
#
# my $fh = $ni_io->from_fh;
# print $fh "foo bar\n";
#
# This will fork the compiled code into a separate process, which is still
# usually faster than the abstraction otherwise required.

our %io_constructors;

sub is_io { ref $_[0] && $_[0]->isa('ni::io') }

sub defio {
  my ($name, $constructor, $methods) = @_;
  *{"ni::io::${name}::new"} = $io_constructors{$name} = sub {
    my ($class, @args) = @_;
    bless $constructor->(@args), $class;
  };
  *{"::ni_$name"} = *{"ni::ni_$name"} =
    sub { ${"ni::io::${name}::"}{new}("ni::io::$name", @_) };
  *{"ni::io::$name::$_"} = $methods->{$_} for keys %$methods;
  push @{"ni::io::${name}::ISA"}, 'ni::io';
}

sub mapone_binding;
sub flatmap_binding;
sub reduce_binding;
sub grep_binding;
sub pipe_binding;

our $gensym_id  = 0;
our $randomness = join '', map sprintf("%04x", rand(65536)), 0..8;
sub gensym { ($_[0] // '') . "_${randomness}_" . $gensym_id++ }

# Internally we're using these IO objects to generate imperative code, so it's
# going to be source-driven. This means we can't do much until we know where
# the values need to go (though we can defer that by fork/piping).

{
  package ni::io;
  use overload qw# + plus_op  * mapone_op  / reduce_op  % grep_op  | pipe_op
                   >>= bind_op
                   > into
                   < from #;

  use POSIX qw/dup2/;

  # Methods implemented by children
  sub quoted_into { ... }
  sub quoted_from { ... }

  sub flatten { ($_[0]) }

  # Transforms
  sub plus_op   { $_[0]->plus($_[1]) }
  sub bind_op   { $_[0]->bind($_[1]) }
  sub mapone_op { $_[0]->mapone($_[1]) }
  sub reduce_op { $_[0]->reduce($_[1], {}) }
  sub grep_op   { $_[0]->grep($_[1]) }
  sub pipe_op   { $_[0]->pipe($_[1]) }

  sub plus    { ::ni_sum(@_) }
  sub bind    { ::ni_bind(@_) }
  sub mapone  { ::ni_bind($_[0], ni::mapone_binding  @_[1..$#_]) }
  sub flatmap { ::ni_bind($_[0], ni::flatmap_binding @_[1..$#_]) }
  sub reduce  { ::ni_bind($_[0], ni::reduce_binding  @_[1..$#_]) }
  sub grep    { ::ni_bind($_[0], ni::grep_binding    @_[1..$#_]) }
  sub pipe    { ::ni_bind($_[0], ni::pipe_binding    @_[1..$#_]) }

  # Utility functions
  sub free_refs_after {
    # Breaks cycles that would prevent Perl from freeing the refs hash
    my $refs = shift @_;
    delete ${$refs}{$_} for keys %$refs;
    @_;
  }

  # User-facing methods
  sub from {
    my ($self, $source_fh) = @_;
    my $fh_gensym = ni::gensym 'fh';
    my $code      = $self->quoted_from("while (<\$$fh_gensym>)",
                                       my $refs = {});
    $refs->{$fh_gensym} = $source_fh;

    my $f = eval($code = qq{
    package main;
    sub {
      my \$$fh_gensym = \$_[0]->{'$fh_gensym'};
      $code
    }});
    die "$@ evaluating\n$code" if $@;
    free_refs_after $refs, $f->($refs);
  }

  sub from_fh {
    my ($self) = @_;
    pipe my $out, my $in or die "pipe failed: $!";
    unless (fork) {
      close $in;
      $self->from($out);
      close $out;
      exit;
    }
    close $out;
    $in;
  }

  sub into {
    my ($self, $dest_fh) = @_;
    my $fh_gensym = ni::gensym 'fh';
    my $code      = $self->quoted_into("print \$$fh_gensym \$_;",
                                       my $refs = {});
    $refs->{$fh_gensym} = $dest_fh;

    my $f = eval($code = qq{
    package main;
    sub {
      my \$$fh_gensym = \$_[0]->{'$fh_gensym'};
      $code
    }});
    die "$@ evaluating\n$code" if $@;
    free_refs_after $refs, $f->($refs);
  }

  sub into_fh {
    my ($self) = @_;
    pipe my $out, my $in or die "pipe failed: $!";
    unless (fork) {
      close $out;
      $self->into($in);
      close $in;
      exit;
    }
    close $in;
    $out;
  }
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
# Data source/sink implementations

defdata 'globfile', sub { ref $_[0] eq 'GLOB' }, sub { ni_file($_[0]) };

=comment

sub deffilter {
  my ($extension, $read, $write) = @_;
  $extension = qr/\.$extension$/;
  defdata $extension,
    sub { $_[0] =~ /$extension/ },
    sub { ni_filter(ni_file($_[0]), $read, $write) };
}

deffilter 'gz',  'gzip -d',  'gzip';
deffilter 'lzo', 'lzop -d',  'lzop';
deffilter 'xz',  'xz -d',    'xz';
deffilter 'bz2', 'bzip2 -d', 'bzip2';

=cut

defdata 'file', sub { -e $_[0] || $_[0] =~ s/^file:// }, sub { ni_file($_[0]) };
# Bidirectional file IO
defio 'file',
sub {
  my ($read, $write) = @_;

  open my $read_new, $read or die "failed to open $read: $!"
    if !ref $read and defined $read;
  $read_new //= $read;

  open my $write_new, $write or die "failed to open $write: $!"
    if !ref $write and defined $write;
  $write_new //= $write;

  die "must specify at least one end (read, write) of the file"
    unless defined $read_new || defined $write_new;
  [$read_new, $write_new];
},
{
  quoted_into => sub {
    my ($self, $code, $refs) = @_;
    my $fh_gensym = gensym 'fh';
    die "cannot read from file (opened write-only)"
      unless defined($refs->{$fh_gensym} = $self->[0]);
    qq{
      my \$$fh_gensym = \$_[0]->{'$fh_gensym'};
      while (<\$$fh_gensym>) {
        $code
      }
    };
  },

  quoted_from => sub {
    my ($self, $prefix, $refs) = @_;
    my $fh_gensym = gensym 'fh';
    die "cannot write into file (opened read-only)"
      unless defined($refs->{$fh_gensym} = $self->[1]);
    qq{
      my \$$fh_gensym = \$_[0]->{'$fh_gensym'};
      $prefix {
        print \$$fh_gensym \$_;
      }
    };
  },
};

defio 'const',
sub { [@_] },
{
  quoted_into => sub {
    my ($self, $code, $refs) = @_;
    my $gensym = gensym 'const';
    $refs->{$gensym} = $self;
    qq{
      for (\@{\$_[0]->{'$gensym'}}) {
        $code
      }
    };
  },
};

# Sum of multiple IOs
defio 'sum',
sub { [map $_->flatten, @_] },
{
  flatten     => sub { @{$_[0]} },
  quoted_into => sub {
    my ($self, $code, $refs) = @_;
    join "\n;\n", map $_->quoted_into($code, $refs), @$self;
  },
};

# Concatenation of an IO of IOs
defio 'cat',
sub { \$_[0] },
{
  quoted_into => sub {
    my ($self, $code, $refs) = @_;
    my $refs_gensym = gensym 'refs';
    my $code_gensym = gensym 'code';
    $refs->{$refs_gensym} = $refs;
    $refs->{$code_gensym} = $code;

    my $loop = $$self->quoted_into(qq{
      eval \$_->quoted_into(\$$code_gensym, \$$refs_gensym);
      die \$@ if \$@;
    }, $refs);

    qq{
      my \$$refs_gensym = \$_[0]->{'$refs_gensym'};
      my \$$code_gensym = \$_[0]->{'$code_gensym'};
      $loop
    };
  },
};

# Introduces arbitrary indirection into an IO's code stream.
defio 'bind',
sub { +{ base => $_[0], code_transform => $_[1] } },
{
  quoted_into => sub {
    my ($self, $code, $refs) = @_;
    my $transformed = $self->{code_transform}->($code, $refs);
    $self->{base}->quoted_into($transformed, $refs);
  },
};

# Bindings for common transformations
sub flatmap_binding {
  my ($f, @args) = @_;
  my $args_ref = [@args];
  my $args_gensym = gensym 'args';
  my $f_gensym    = gensym 'f';
  sub {
    my ($code, $refs) = @_;
    if (ref $f eq 'CODE' || @args) {
      $refs->{$args_gensym} = $args_ref;
      $refs->{$f_gensym}    = compile $f;
      qq{ for (\$_[0]->{'$f_gensym'}->(\$_, \@{\$_[0]->{'$args_gensym'}})) {
            $code
          } };
    } else {
      qq{ for ($f) {
            $code;
          } };
    }
  };
}

sub mapone_binding {
  my ($f, @args) = @_;
  my $args_ref = [@args];
  my $args_gensym = gensym 'args';
  my $f_gensym    = gensym 'f';
  sub {
    my ($code, $refs) = @_;
    if (1 || ref $f eq 'CODE' || @args) {
      $refs->{$args_gensym} = $args_ref;
      $refs->{$f_gensym}    = compile $f;
      qq{ if (defined(\$_ = \$_[0]->{'$f_gensym'}->(
                \$_,
                \@{\$_[0]->{'$args_gensym'}}))) {
            $code
          } };
    } else {
      qq{ if (defined(\$_ = $f)) {;
            $code
          } };
    }
  };
}

sub grep_binding {
  my ($f, @args) = @_;
  my $args_ref = [@args];
  my $args_gensym = gensym 'args';
  my $f_gensym    = gensym 'f';
  sub {
    my ($code, $refs) = @_;
    if (ref $f eq 'CODE' || @args) {
      $refs->{$args_gensym} = $args_ref;
      $refs->{$f_gensym}    = compile $f;
      qq{ if (\$_[0]->{'$f_gensym'}->(\$_, \@{\$_[0]->{'$args_gensym'}})) {
            $code
          } };
    } else {
      qq{ if ($f) {
            $code
          } };
    }
  };
}

sub reduce_binding {
  my ($f, $init, @args) = @_;
  my $args_ref = [@args];
  my $args_gensym = gensym 'args';
  my $init_gensym = gensym 'init';
  my $f_gensym    = gensym 'f';
  sub {
    my ($code, $refs) = @_;
    $refs->{$init_gensym} = $init;
    my $out_gensym = gensym 'results';
    if (ref $f eq 'CODE' || @args) {
      $refs->{$args_gensym} = $args_ref;
      $refs->{$f_gensym}    = compile $f;
      qq{ (\$$init_gensym, \@$out_gensym) =
          \$_[0]->{'$f_gensym'}->(\$$init_gensym, \$_,
                                  \@{\$_[0]->{'$args_gensym'}});
          for (\@$out_gensym) {
            $code
          } };
    } else {
      # Replace the custom symbol '%init' with the accumulator so the function
      # has a way to refer to it.
      $f =~ s/%init/\$$init_gensym/g;
      qq{ (\$$init_gensym, \@$out_gensym) = $f;
          for (\@$out_gensym) {
            $code;
          } };
    }
  };
}
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

sub file_opt { ['plus', ni $_[0]] }
sub parse_commands {
  my @parsed;
  for (my $o; defined($o = shift @_);) {
    return @parsed, map file_opt($_), @_ if $o eq '--';
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
      push @parsed, file_opt $o;
    }
  }
  @parsed;
}

# Record transformation
# Most of the transforms within ni are in-process, so it's possible for one
# operator to append a data element that would be interpreted differently if we
# later added an external command like a sort. We want to avoid this issue as
# much as possible, which we can do by using some default idioms.

sub ::row {
  my $s = join "\t", @_;
  $s =~ s/\n//g;
  "$s\n";
}

sub with_fields {
  my ($code) = @_;
  compile "chomp; \@_ = split /\\t/; \$_ = $code";
}
# Operator implementations

use File::Temp qw/tmpnam/;

# Meta
defop 'self', undef, '',
  'adds the source code of ni',
  sub { $_[0] + ni_const(self) };

defop 'explain-pipeline', undef, '',
  'explains the current pipeline',
  sub { ni_const($_[0]->quoted_into('print $_;', {})) };

# Functional transforms
defop 'map', 'm', 's',
  'transforms each record using the specified function',
  sub { $_[0] * with_fields $_[1] };

defop 'keep', 'k', 's',
  'keeps records for which the function returns true',
  sub { $_[0] % with_fields $_[1] };

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

use POSIX qw/dup2/;

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
my $data = -t STDIN ? ni_sum() : ni_file(\*STDIN);
for (parse_commands preprocess_cli @ARGV) {
  my ($command, @args) = @$_;
  $data = $ni::io::{$command}($data, @args);
}

if (-t STDOUT && !exists $ENV{NI_NO_PAGER}) {
  # Use a pager rather than writing straight to the terminal
  close STDIN;
  dup2 0, fileno $data->into_fh or die "dup2 failed: $!";
  exec $ENV{NI_PAGER} // $ENV{PAGER} // 'less';
  exec 'more';
  # Ok, we're out of options; just write to the terminal after all
  print while <>;
} else {
  $data > \*STDOUT;
}
