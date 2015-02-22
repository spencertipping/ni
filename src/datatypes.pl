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
