# DuckDB tools


sub which_duckdb()
{
  # DuckDB is in a fixed location if the installer created it
  my $ipath = "$ENV{HOME}/.duckdb/cli/latest/duckdb";
  -x $ipath and return $ipath;

  return $_ for split/\n/, qx{which duckdb};

  # If none of those, install first and then return its location
  return $ipath
    unless system "curl -sSL https://install.duckdb.org | sh >&2";

  die "DuckDB installation failed";
}


# Use DuckDB to handle Parquet files
defresource 'parquetjson',
  read => q{
    my ($url, $pq) = @_;
    my ($path, $query) = split/:/, $pq, 2;
    $query //= '*';
    return soproc{
      exec which_duckdb, '-json', '-c', qq{
        copy (select $query from read_parquet('$path'))
        to stdout with (format json); }} },
  write => q{
    my ($url, $path) = @_;
    return siproc{
      exec which_duckdb, '-c', qq{
        copy (select * from read_json_auto('/dev/stdin'))
        to '$path' with (format 'parquet', compression 'zstd'); }} };

defresource 'parquet',
  read => q{
    my ($url, $pq) = @_;
    my ($path, $query) = split/:/, $pq, 2;
    $query //= '*';
    return soproc{
      exec which_duckdb, '-c', qq{
        copy (select $query from read_parquet('$path'))
        to stdout with (format csv, delimiter E'\t', header true); }} },
  write => q{
    my ($url, $path) = @_;
    return siproc{
      exec which_duckdb, '-c', qq{
        copy (select * from read_csv_auto('/dev/stdin', delim=E'\\t', header=true))
        to '$path' (format 'parquet', compression 'zstd') }} };

defresource 'parquetmeta',
  read => q{
    my ($url, $path) = @_;
    return soproc{
      exec which_duckdb, '-c', qq{ describe select * from read_parquet('$path')}} };


defresource 'duckjson',
  read => q{
    my ($url, $query) = @_;
    return soproc{
      exec which_duckdb, '-json', '-c', qq{
        copy ($query) to stdout with (format json); }} };

defresource 'duck',
  read => q{
    my ($url, $query) = @_;
    return soproc{
      exec which_duckdb, '-json', '-c', qq{
        copy ($query) to stdout with (format csv, delimiter E'\t', header true); }} };
