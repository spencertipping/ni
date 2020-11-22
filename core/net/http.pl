# Web resources that you can access with URI syntax.

defresource 'http',
  read      => q{soproc {exec 'curl', '-sS', $_[0]} @_},
  multiread => q{soproc {exec 'curl', '-sS', @_} @_},
  write     => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'https',
  read      => q{soproc {exec 'curl', '-sS', $_[0]} @_},
  multiread => q{soproc {exec 'curl', '-sS', @_} @_},
  write     => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'sftp',
  read   => q{my ($host, $path) = $_[1] =~ m|^([^:/]+):?(.*)|;
              soproc {exec 'ssh', $host, 'cat', $path}};

defresource 's3cmd',
  read   => q{soproc {exec 's3cmd', '--no-progress', '--stop-on-error', 'get', "s3://$_[1]", '-'} @_},
  write  => q{siproc {exec 's3cmd', 'put', '-', "s3://$_[1]"} @_};
  # TODO
