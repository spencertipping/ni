# Data source/sink implementations

our %read_filters;
our %write_filters;

defdata 'file',
  sub { -e $_[0] || $_[0] =~ s/^file:// },
  sub {
    my ($f)       = @_;
    my $extension = ($f =~ /\.(\w+)$/)[0];
    my $file      = ni_file("[file $f]", "< $f", "> $f");
    exists $read_filters{$extension}
      ? ni_filter($file, $read_filters{$extension}, $write_filters{$extension})
      : $file;
  };

sub deffilter {
  my ($extension, $read, $write) = @_;
  $read_filters{$extension}  = $read;
  $write_filters{$extension} = $write;

  my $prefix_detector = qr/^$extension:/;
  defdata $extension,
    sub { $_[0] =~ s/$prefix_detector// },
    sub { ni_filter(ni($_[0]), $read, $write) };
}

deffilter 'gz',  'gzip -d',  'gzip';
deffilter 'lzo', 'lzop -d',  'lzop';
deffilter 'xz',  'xz -d',    'xz';
deffilter 'bz2', 'bzip2 -d', 'bzip2';

defdata 'ssh',
  sub { $_[0] =~ /^\w*@[^:\/]+:/ },
  sub { $_[0] =~ /^([^:@]+)@([^:]+):(.*)$/;
        my ($user, $host, $file) = ($1, $2, $3);

        };

defdata 'globfile', sub { ref $_[0] eq 'GLOB' },
                    sub { ni_file("[fh = " . fileno($_[0]) . "]",
                                  $_[0], $_[0]) };

defdata 'quoted', sub { ref $_[0] eq '[' },
                  sub { self_pipe @{$_[0]} };
