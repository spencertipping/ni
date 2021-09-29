# Rar archive support (requires the "unrar" command-line tool, specifically
# the non-free variant)

sub rar_listing_fh($)
{
  my $f = shift;
  soproc { sh shell_quote(unrar => 'l', $f)
            . q{ | perl -ne 'print if /^    \S/' | cut -c42- } };
}

sub rar_file_fh($$)
{
  my ($rarf, $entry) = @_;
  soproc { sh shell_quote(unrar => 'p', $rarf, $entry)
            . q{ | tail -n+9 | head -n-2 } };
}

# Rar file listing: rar:///path/to/zipfile
defresource 'rar',
  read => q{
    my $filename = $_[1];
    soproc {
      my $fh = rar_listing_fh $filename;
      print "rarentry://$filename:$_" while <$fh> };
  };

# Single-entry unpacking: rarentry:///path/to/file.rar:subfilename
defresource 'rarentry',
  read => q{
    my ($rarfile, $fname) = split /:/, $_[1], 2;
    rar_file_fh $rarfile, $fname;
  };
