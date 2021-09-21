# Ar archive support (requires the "ar" command-line tool)

sub ar_listing_fh($)
{
  my $f = shift;
  soproc { sh shell_quote(ar => '-t', $f) };
}

sub ar_file_fh($$)
{
  my ($arf, $entry) = @_;
  soproc { sh shell_quote(ar => '-p', $arf, $entry) };
}

# Ar file listing: ar:///path/to/file.a
defresource 'ar',
  read => q{
    my $filename = $_[1];
    soproc {
      my $fh = ar_listing_fh $filename;
      print "arentry://$filename:$_" while <$fh> };
  };

# Single-entry unpacking: arentry:///path/to/file.a:subfilename
defresource 'arentry',
  read => q{
    my ($arfile, $fname) = split /:/, $_[1], 2;
    ar_file_fh $arfile, $fname;
  };
