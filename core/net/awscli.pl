# AWS CLI tools

defresource 's3u',
  read  => q{soproc {exec 'aws', 's3', 'cp', "s3://$_[1]", '-', '--no-sign-request'} @_},
  write => q{siproc {exec 'aws', 's3', 'cp', '-', "s3://$_[1]", '--no-sign-request'} @_};

defresource 's3',
  read  => q{soproc {exec 'aws', 's3', 'cp', "s3://$_[1]", '-'} @_},
  write => q{siproc {exec 'aws', 's3', 'cp', '-', "s3://$_[1]"} @_};

defresource 's3r',
  read  => q{requires_dangermode("s3r://");
             soproc {exec 'aws', 's3', 'cp', "s3://$_[1]", '-', '--request-payer'} @_},
  write => q{requires_dangermode("s3r://");
             siproc {exec 'aws', 's3', 'cp', '-', "s3://$_[1]", '--request-payer'} @_};


sub awscli_ls_format($$)
{
  my ($prefix, $fh) = @_;

  # Convert from ls or lsu (listing) to s3 or s3u (download)
  $prefix =~ s/^s3ls/s3/;

  my ($bucket_prefix) = $prefix =~ /^(\w+:\/\/[^\/]+)/;
  while (<$fh>)
  {
    my ($date, $time, $size, $path) = /^(\S+)\s+(\S+)\s+(\d+)\s+([^\/]+\/.*)/;
    printf "%s/%s\t%d\t%sT%sZ\n", $bucket_prefix, $path, $size, $date, $time;
  }
}

defresource 's3lsu',
  read => q{awscli_ls_format $_[0], soproc {exec 'aws', 's3', 'ls', "s3://$_[1]", '--recursive', '--no-sign-request'} @_};

defresource 's3ls',
  read => q{awscli_ls_format $_[0], soproc {exec 'aws', 's3', 'ls', "s3://$_[1]", '--recursive'} @_};

defresource 's3lsr',
  read => q{requires_dangermode("s3lsr://");
            awscli_ls_format $_[0], soproc {exec 'aws', 's3', 'ls', "s3://$_[1]", '--recursive', '--request-payer'} @_};


{
  no strict 'refs';
  defresource 's3.ls.u', read => ${"ni::resource_read"}{s3lsu};
  defresource 's3.ls.r', read => ${"ni::resource_read"}{s3lsr};
  defresource 's3.ls',   read => ${"ni::resource_read"}{s3ls};
}
