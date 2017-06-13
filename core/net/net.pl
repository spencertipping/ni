# Networking stuff.
# SSH tunneling to other hosts. Allows you to run a ni lambda elsewhere. ni does
# not need to be installed on the remote system, nor does its filesystem need to
# be writable.

BEGIN {defparseralias ssh_host => prx '[^][/,]+'}

defoperator ssh => q{
  my ($host, $lambda) = @_;
  my $ssh_pipe = siproc {exec 'ssh', @$host, shell_quote ni_quoted_exec_args};
  quote_ni_into $ssh_pipe, @$lambda;
};

defshort '/s', pmap q{ssh_op @$_},
  pseq palt(pc pmap(q{[$_]}, ssh_host), pc multiword), _qfn;

# Network resources.

defresource 'http', read  => q{soproc {exec 'curl', '-sS', $_[0]} @_},
                    write => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'https', read  => q{soproc {exec 'curl', '-sS', $_[0]} @_},
                     write => q{siproc {exec 'curl', '-sSd', '-', $_[0]} @_};

defresource 'sftp',
  read   => q{my ($host, $path) = $_[1] =~ m|^([^:/]+):?(.*)|;
              soproc {exec 'ssh', $host, 'cat', $path}};

defresource 's3cmd',
  read   => q{soproc {exec 's3cmd', '-q', 'get', "s3://$_[1]", '-'} @_},
  write  => q{siproc {exec 's3cmd', 'put', '-', "s3://$_[1]"} @_};
  # TODO
