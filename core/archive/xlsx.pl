# xlsx parsing
# This backs into the zip archive handler, adding some shorthands for working
# with xlsx worksheets.

# List all worksheets: xlsx:///path/to/file.xlsx
defresource 'xlsx',
  read => q{
    my $file = $_[1];
    soproc {
      my $fh = zip_listing_fh $file;
      /^xl\/worksheets\/sheet(\d+)\.xml/ && print "xlsxsheet://$file:$1\n"
        while <$fh> };
  };

sub worksheet_col_to_offset($)
{
  # Takes a column ID like "AZ" and converts it to the zero-based offset it
  # represents; that is, the offset of A = 0.
  my $i = 0;
  $i = $i * 26 + (1 + ord() - ord"A") for split //, shift;
  $i - 1;
}

# Decode a worksheet: xlssheet:///path/to/file.xlsx:<sheet-number>
defresource 'xlssheet',
  read => q{
    my ($file, $sheet_id) = split /:/, $_[1], 2;
    soproc {
      my $fh = zip_file_fh $file, "xl/worksheets/sheet$sheet_id.xml";
      my @row;
      my $i;                  # next col index to assign
      while (<$fh>)
      {
        chomp;
        if (/<\/row>/)
        {
          print join("\t", @row), "\n";
          @row = ();
        }
        elsif (/<c r="([A-Z]+)\d*"/)
        {
          $i = worksheet_col_to_offset $1;
        }
        elsif (/<[vt]>([^<]+)<\/[vt]>/)
        {
          push @row, "" until $#row >= $i;
          $row[$i] = $1;
        }
      } };
  };
