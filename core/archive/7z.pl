# 7zip archive support (requires the "7z" tool)

use constant sevenzip_names => [qw/ 7z 7za 7zr /];

sub sevenzip_listing_fh($)
{
  my $f = shift;
  soproc { sh join '||', map shell_quote($_, 'l', '-slt', $f)
                             . " | grep '^Path = '"
                             . " | tail -n+2"
                             . " | cut -c8-",
                             @{+sevenzip_names} };
}

sub sevenzip_file_fh($$)
{
  my ($f, $entry) = @_;
  soproc { sh join '||', map shell_quote($_, 'x', '-so', $f, $entry),
                             @{+sevenzip_names} };
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
