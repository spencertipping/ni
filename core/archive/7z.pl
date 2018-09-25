# 7zip archive support (requires the "7z" tool or one of its variants)

sub which_7zip()
{
    !system("7z > /dev/null")  ? '7z'
  : !system("7za > /dev/null") ? '7za'
  : !system("7zr > /dev/null") ? '7zr'
  : 'p7zip';
}

sub sevenzip_listing_fh($)
{
  my $f = shift;
  soproc { sh shell_quote(which_7zip, 'l', '-slt', $f)
                . " | grep '^Path = '"
                . " | tail -n+2"
                . " | cut -c8-" };
}

sub sevenzip_file_fh($$)
{
  my ($f, $entry) = @_;
  soproc { sh shell_quote(which_7zip, 'x', '-so', $f, $entry) };
}

# 7z file listing: 7z:///path/to/7zfile
defresource '7z',
  read => q{
    my $filename = $_[1];
    soproc {
      my $fh = sevenzip_listing_fh $filename;
      print "7zentry://$filename:$_" while <$fh> };
  };

# Single-entry unpacking: 7zentry:///path/to/file.7z:subfilename
defresource '7zentry',
  read => q{
    my ($zipfile, $fname) = split /:/, $_[1], 2;
    sevenzip_file_fh $zipfile, $fname;
  };
