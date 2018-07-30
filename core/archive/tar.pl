# tar unpacking (file version)
# These functions auto-decompress using sdecode, so you won't need to worry
# about knowing which algorithm was used. You also get automatic speedups from
# things like pbzip2 if they're installed.

sub tar_listing_fh($)
{
  my $tarfile = shift;
  soproc {
    sforward soproc {scat($tarfile)},
             siproc {sh shell_quote(tar => '-t')} };
}

sub tar_file_fh($$)
{
  my ($tarfile, $entry) = @_;
  soproc {
    sforward soproc {scat($tarfile)},
             siproc {sh shell_quote(tar => '-x', '-O', $entry)} };
}

# Entry point: tar:///path/to/tarfile.tar
defresource 'tar',
  read => q{
    my $tarfile = $_[1];
    soproc {
      my $fh = tar_listing_fh $tarfile;
      print "tarentry://$tarfile:$_" while <$fh> };
  };

# Single-file extraction: tarentry:///path/to/tarfile.tar:filename
defresource 'tarentry',
  read => q{
    my ($tarfile, $entry) = split /:/, $_[1], 2;
    tar_file_fh $tarfile, $entry;
  };
