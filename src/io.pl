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

sub defioproxy {
  my ($name, $f) = @_;
  *{"::ni_$name"} = *{"ni::ni_$name"} = $f;
}

sub mapone_binding;
sub flatmap_binding;
sub reduce_binding;
sub grep_binding;
sub pipe_binding;

# Internally we're using these IO objects to generate imperative code, so it's
# going to be source-driven. This means we can't do much until we know where
# the values need to go (though we can defer that by fork/piping).

{

package ni::io;
use overload qw# + plus_op  * mapone_op  / reduce_op  % grep_op  | pipe_op
                 eq compare_refs
                 "" explain
                 >>= bind_op
                 > into  >= into_bg
                 < from  <= from_bg #;

use Scalar::Util qw/refaddr/;

BEGIN { *gen = \&ni::gen }

use POSIX qw/dup2/;

# Methods implemented by children
sub source_gen { ... }          # gen to source from this thing
sub sink_gen   { ... }          # gen to sink into this thing
sub explain    { ... }

sub transform {
  my ($self, $f) = @_;
  $f->($self);
}

sub reader_fh { (::ni_pipe() <= $_[0])->reader_fh }
sub writer_fh { (::ni_pipe() >= $_[0])->writer_fh }

sub has_reader_fh { 0 }
sub has_writer_fh { 0 }
sub process_local { 0 }

sub supports_reads  { 1 }
sub supports_writes { 0 }

sub flatten { ($_[0]) }
sub close   { $_[0] }

# Transforms
sub plus_op   { $_[0]->plus($_[1]) }
sub bind_op   { $_[0]->bind($_[1]) }
sub mapone_op { $_[0]->mapone($_[1]) }
sub reduce_op { $_[0]->reduce($_[1], {}) }
sub grep_op   { $_[0]->grep($_[1]) }
sub pipe_op   { $_[0]->pipe($_[1]) }

sub plus    { ::ni_sum(@_) }
sub bind    { ::ni_bind(@_) }
sub mapone  { $_[0] >>= ni::mapone_binding  @_[1..$#_] }
sub flatmap { $_[0] >>= ni::flatmap_binding @_[1..$#_] }
sub reduce  { $_[0] >>= ni::reduce_binding  @_[1..$#_] }
sub grep    { $_[0] >>= ni::grep_binding    @_[1..$#_] }
sub pipe    { ::ni_process($_[1], $_[0], undef) }

sub compare_refs { refaddr($_[0]) eq refaddr($_[1]) }

# User-facing methods
sub from {
  my ($self, $source) = @_;
  ::ni($source)->source_gen($self)->run;
  $self;
}

sub from_bg {
  my ($self, $source) = @_;
DEBUG
  die "cannot background-load a process-local io $self"
    if $self->process_local;
DEBUG_END
  $self < $source, exit unless fork;
  $self;
}

sub into {
  my ($self, $dest) = @_;
  $self->source_gen(::ni $dest)->run;
  $self;
}

sub into_bg {
  my ($self, $dest) = @_;
DEBUG
  die "cannot background-load a process-local io $dest"
    if $dest->process_local;
DEBUG_END
  $self > $dest, exit unless fork;
  $self;
}

}
