# Zip archive support (requires the "unzip" command-line tool)

sub zip_listing_fh($)
{
  my $f = shift;
  soproc { sh shell_quote(unzip => '-l', '-qq', $f)
            . " | cut -c31- " };
}

sub zip_file_fh($$)
{
  my ($zipf, $entry) = @_;
  soproc { sh shell_quote(unzip => '-p', $zipf, $entry) };
}

# Zip file listing: zip:///path/to/zipfile
defresource 'zip',
  read => q{
    my $filename = $_[1];
    soproc {
      my $fh = zip_listing_fh $filename;
      print "zipentry://$filename:$_" while <$fh> };
  };

# Single-entry unpacking: gitentry:///path/to/file.zip:subfilename
defresource 'zipentry',
  read => q{
    my ($zipfile, $fname) = split /:/, $_[1], 2;
    zip_file_fh $zipfile, $fname;
  };
