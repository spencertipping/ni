# solr-via-HTTP URI integration
# Example usage:
#
#   ni i[hostname model os] i[iniidae x250 linux] \>solr://localhost/collection
#   ni solr://localhost/collection/model=x250

sub solr_parse_url($)
{
  my ($host, $port, $core, $query) =
    $_[0] =~ /^([^\/:]+)(?::(\d+))?(?:\/([^\/]+)(?:\/(.*))?)?/;
  die "ni: invalid solr:// URI: $_[0]" unless defined $host;
  $port //= 8983;
  ($host, $port, $core, $query);
}

# solr:// does a few things depending on syntax.
#
# solr://host[:port]/ expands to a list of cores followed by some JSON metadata
# for each one.
#
# solr://host[:port]/core/query queries the core and returns results.

sub solr_corelist
{
  my ($host, $port) = @_;
  my $r = json_decode rf "http://$host:$port/solr/admin/cores?action=STATUS";
  print "solr://$host:$port/$_\t" . json_encode($$r{status}{$_}) . "\n"
    for sort keys %{$$r{status}};
}

sub solr_query
{
  my ($host, $port, $core, $args) = @_;
  sh shell_quote curl => "-Ss",
    "http://$host:$port/solr/$core/select?wt=csv&csv.separator=%09&q.op=AND&rows=1000000000&q=$args";
}

defresource 'solr',
  read => q{
    my ($host, $port, $core, $stuff) = solr_parse_url $_[1];
    soproc { defined $core
      ? solr_query    $host, $port, $core, $stuff
      : solr_corelist $host, $port };
  },
  write => q{
    my ($host, $port, $core) = solr_parse_url $_[1];
    siproc {
      sh shell_quote(curl =>
        "-Ss", "-H", "Content-Type: application/csv", "--data-binary", "\@-",
        "http://$host:$port/solr/$core/update/csv?separator=%09&keepEmpty=true&encapsulator=%00&commit=true") };
  };
