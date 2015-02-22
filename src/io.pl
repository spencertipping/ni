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
