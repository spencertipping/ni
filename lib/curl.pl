NI_MODULE curl

defdata 'http', sub { $_[0] =~ /^https?:\/\// },
  sub {
    my ($url) = @_;
    ni_file "[curl $url]",
      sub { ni_process(shell_quote 'curl', $url)->reader_fh },
      sub { ni_process(shell_quote('curl', '--data-binary', '@-', $url),
                       undef,
                       \*STDERR)->writer_fh };
  };

NI_MODULE_END
