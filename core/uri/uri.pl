# Resources identified by URI.
# A way for ni to interface with URIs. URIs are self-appending like files; to
# quote them you should use the `\'` prefix or the `i` operator:

# | ni http://google.com          # prints contents of google.com
#   ni \'http://google.com        # prints "http://google.com"
#   ni ihttp://google.com         # prints "http://google.com"

# If you've got a lot of resources, you can use `\'` with a lambda to quote all
# of them:

# | ni \'[ http://foo.com http://bar.com ]

sub uri_scheme($) {my ($scheme) = $_[0] =~ /^([^:]+):/; $scheme}
sub uri_path($)   {my $s = uri_scheme $_[0]; sr $_[0], qr|^\Q$s://\E|, ''}

BEGIN {
  no strict 'refs';

  deflong '/resource', defdsp 'resourcealt', 'dispatch table for URI prefixes';

  for my $op (qw/read write exists tmp nuke/) {
    %{"ni::resource_$op"} = ();
    *{"ni::resource_$op"} = sub ($) {
      my ($r) = @_;
      my $scheme = uri_scheme $r;
      my $f = ${"ni::resource_$op"}{$scheme} or
        die "ni: $scheme resources don't support the $op operation";
      &$f($r, uri_path $r);
    };
  }
}

our %nuke_on_exit;
sub nuke_on_exit($) {$nuke_on_exit{$_[0]} = $$}

END {$nuke_on_exit{$_} eq $$ and resource_nuke $_ for keys %nuke_on_exit}

our %resource_read;
our %resource_write;
our %resource_exists;
our %resource_tmp;
our %resource_nuke;

defoperator resource_quote => q{sio; print "$_[0]\n"};
defoperator resource_append => q{
  sio;
  my $decoder = siproc {sdecode};
  sforward resource_read $_[0], $decoder;
  close $decoder;
  $decoder->await;
};

defoperator resource_quote_many => q{sio; print "$_\n" for @_};

defshort "/'", pmap q{resource_quote_many_op @$_},
  pn 1, prc qr/\[/, prep(prc '[^]].*'), prc qr/\]/;

sub defresource($%) {
  my ($scheme, %opts) = @_;
  defresourcealt("'$scheme://",
    pmap qq{resource_quote_op "$scheme://\$_"}, prx '.*');
  defresourcealt("$scheme://",
    pmap qq{resource_append_op "$scheme://\$_"}, prx '.*');

  $resource_read{$scheme}   = fn $opts{read}   if exists $opts{read};
  $resource_write{$scheme}  = fn $opts{write}  if exists $opts{write};
  $resource_exists{$scheme} = fn $opts{exists} if exists $opts{exists};
  $resource_tmp{$scheme}    = fn $opts{tmp}    if exists $opts{tmp};
  $resource_nuke{$scheme}   = fn $opts{nuke}   if exists $opts{nuke};
}

# Stream function extensions.
# Add resource support to srfile and swfile.

my $original_srfile = \&srfile;
my $original_swfile = \&swfile;
my $original_glob_expand = \&glob_expand;

sub is_uri($) {$_[0] =~ /^[^:\/]+:\/\//}

{
  no warnings 'redefine';
  *glob_expand = sub($) {
    return $_[0] if is_uri $_[0] or -e $_[0];
    glob $_[0];
  };

  *srfile = sub($) {
    return resource_read $_[0] if is_uri $_[0];
    &$original_srfile($_[0]);
  };

  *swfile = sub($) {
    return resource_write $_[0] if is_uri $_[0];
    &$original_swfile($_[0]);
  };
}

# Filesystem resources.
# Things that behave like files: local files, HDFS, S3, sftp, etc.

sub uri_temp_noise() {"ni." . getpwuid($<) . "." . noise_str 32}
defconfenv 'tmpdir', TMPDIR => '/tmp';

defresource 'file',
  read   => q{srfile $_[1]},
  write  => q{swfile $_[1]},
  exists => q{-e $_[1]},
  tmp    => q{"file://" . conf('tmpdir') . "/" . uri_temp_noise},
  nuke   => q{unlink $_[1]};

defresource 'pipe',
  read   => q{my $fh = srfile $_[1]; unlink $_[1]; $fh},
  write  => q{use POSIX qw/mkfifo/;
              mkdir_p dirname $_[1] or die "ni >$_[0]: failed to mkdir: $!";
              mkfifo $_[1], 0700 or die "ni >$_[0]: failed to mkfifo: $!";
              swfile $_[1]},
  exists => q{-e $_[1]},
  tmp    => q{"pipe://" . conf('tmpdir') . "/" . uri_temp_noise},
  nuke   => q{unlink $_[1]};
