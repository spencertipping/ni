# xlsx parsing
# This backs into the zip archive handler, adding some shorthands for working
# with xlsx worksheets.
#
# NB: this is a good resource: https://stackoverflow.com/questions/18334314/what-do-excel-xml-cell-attribute-values-mean

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

sub workbook_shared_strings($)
{
  # Takes a workbook filename and returns an array of shared strings. This will
  # fail if any strings are multiline (TODO)
  my $fh = zip_file_fh shift, "xl/sharedStrings.xml";
  my @r;
  /<t>(.*)<\/t>/ and push @r, $1 while <$fh>;
  @r;
}

# Decode a worksheet: xlsxsheet:///path/to/file.xlsx:<sheet-number>
defresource 'xlsxsheet',
  read => q{
    my ($file, $sheet_id) = split /:/, $_[1], 2;
    soproc {
      my $fh = zip_file_fh $file, "xl/worksheets/sheet$sheet_id.xml";
      my @ss = workbook_shared_strings $file;
      my @row;
      my $i;                  # next col index to assign
      my $is_shared = 0;      # true if <v> node is a shared string index
      while (<$fh>)
      {
        chomp;
        if (/<\/row>/)
        {
          print join("\t", @row), "\n";
          @row = ();
        }

        if (/<c /)
        {
          # We need to know the data type of the cell. If it's a shared string,
          # type it as such so we know how to interpret the <v> node.
          $i         = worksheet_col_to_offset $1 if /r="([A-Z]+)/;
          $is_shared = /t="s"/;
        }

        if (/<[vt]>([^<]+)<\/[vt]>/)
        {
          push @row, "" until $#row >= $i;
          $row[$i] = $is_shared ? $ss[$1] : $1;
        }
      } };
  };
