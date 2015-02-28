# Ops to wrap common shell tools like sort, uniq, join, comm, etc

our $sort_buffer_size = $ENV{NI_SORT_BUFFER}   // '256M';
our $sort_parallel    = $ENV{NI_SORT_PARALLEL} // 4;
our $sort_compress    = $ENV{NI_SORT_COMPRESS} // '';

sub sort_invocation {
  my ($fields, @options) = @_;
  my @fields = split //, $fields // '';
  shell_quote 'sort', '-S', $sort_buffer_size,
              "--parallel=$sort_parallel",
              @fields
                ? ('-t', "\t", map {('-k', "${_}b,$_")} map {$_ + 1} @fields)
                : (),
              length $sort_compress
                ? ("--compress-program=$sort_compress")
                : (),
              @options;
}

sub expand_sort_flags {
  my ($flags) = @_;
  return () unless defined $flags;
  map "-$_", split //, $flags =~ s/([A-Z])/"r" . lc $1/gre;
}

defop 'sort', 's', 'AD',
  '[flags] [fields], flags = [nNgGr][u] with their default meaning',
  sub {
    my ($self, $flags, $fields) = @_;
    $self | sort_invocation $fields, expand_sort_flags $flags;
  };

defop 'merge', undef, 'ADs',
  '[flags] [fields] merge-data: see "sort" for flags',
  sub {
    my ($self, $flags, $fields, $data) = @_;
    $self | sort_invocation $fields, expand_sort_flags($flags), '-m',
                            '-',
                            ni_fifo->from_bg($data);
  };

defop 'join', 'j', 'aDs',
  '[flags] [field] join-data, flags = one of lrbnLRBN',
  sub {
    my ($self, $flag, $field, $data) = @_;
    my $outer_join = $flag =~ y/[A-Z]/[a-z]/;
    my $sort_left  = $flag =~ /[rn]/;
    my $sort_right = $flag =~ /[ln]/;
    $field //= 0;

    my $left  = $sort_left ? $self->__sort(undef, $field) : $self;
    my $right = ni $data;
    $right = $right->__sort(undef, 0) if $sort_right;

    $left | shell_quote('join', '-1', $field ? $field + 1 : '1',
                                '-2', 1,
                                '-t', "\t",
                                $outer_join ? ('-a', 1) : (),
                                '-',
                                ni_fifo->from_bg($right));
  };

defop 'uniq', 'u', 'D',
  'unique lines (or fields); count if prefixed with +',
  sub {
    # Don't shell out to uniq for this for two reasons. One is a dumb thing,
    # but the shell command uniq doesn't tab-delimit its output, which makes it
    # really hard to parse later on. The other is that our field descriptions
    # might not be contiguous ranges, whereas all uniq can do is skip the first
    # N fields.
    my ($self, $fields) = @_;
    my $count = ($fields //= '') =~ s/^\+//;
    $self->uniq($count, split //, $fields);
  };
