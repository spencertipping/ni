NI_MODULE json

our $json;

if (eval {require JSON}) {
  JSON->import;
  no warnings qw/uninitialized/;
  $json = JSON->new->allow_nonref->utf8(1);
} elsif (eval {require JSON::PP}) {
  JSON::PP->import;
  no warnings qw/uninitialized/;
  $json = JSON::PP->new->allow_nonref->utf8(1);
} else {
  # No builtin JSON. At some point I'd like to write one, but until then just
  # complain about the lamentable situation.
  print STDERR
    "ni: no JSON support detected (cpan install JSON should fix it)\n";
}

sub ::json_encode { $json->encode(@_) }
sub ::json_decode { $json->decode(@_) }

defshortfn 'je', \&::json_encode;
defshortfn 'jd', \&::json_decode;

NI_MODULE_END
