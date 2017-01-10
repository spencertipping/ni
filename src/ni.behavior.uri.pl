u('ni.scheme:ni.scheme')->create('json',
  name        => 'A JSON string interpreted as an object',
  synopsis    => q{ u('json:[1,2,3]')->[0]
                  | u('json:{"foo":"bar"}')->{foo} },
  description => q{
    Decodes a JSON object specified as the body of a URI. This class also
    side-effectfully populates the ni::json package, which provides
    general-purpose encoding and decoding support without any external
    dependencies.})

  ->uses(u"ni.behavior.code:" . q(
    {
    package ni::json;
    use Scalar::Util qw/looks_like_number/;
    our %unescapes =
      ("\\\\" => "\\\\", "/" => "/", "\"" => "\"", b => "\b", n => "\n",
       r => "\r", t => "\t");
    sub unescape_one($) {$unescapes{$_[0]} || chr hex substr $_[0], 1}
    sub unescape($) {
      my $x = substr $_[0], 1, -1;
      $x =~ s/\\\\(["\\\\\/bfnrt]|u[0-9a-fA-F]{4})/unescape_one $1/eg;
      $x;
    }
    sub decode($) {
      local $_;
      my @v = [];
      for ($_[0] =~ /[][{}]|true|false|null|"(?:[^"\\\\]+|\\\\.)*"|[-+eE\d.]+/g) {
        if (/^[[{]$/) {
          push @v, [];
        } elsif (/^\]$/) {
          die "decode $_[0]: too many closing brackets" if @v < 2;
          push @{$v[-2]}, $v[-1];
          pop @v;
        } elsif (/^\}$/) {
          die "decode $_[0]: too many closing brackets" if @v < 2;
          push @{$v[-2]}, {@{$v[-1]}};
          pop @v;
        } else {
          push @{$v[-1]}, /^"/      ? unescape $_
                        : /^true$/  ? 1
                        : /^false$/ ? 0
                        : /^null$/  ? undef
                        :             0 + $_;
        }
      }
      my $r = pop @v;
      die "decode $_[0]: not enough closing brackets" if @v;
      wantarray ? @$r : $$r[0];
    }

    our %escapes = map {;$unescapes{$_} => $_} keys %unescapes;
    sub escape($) {
      (my $x = $_[0]) =~ s/([\b\f\n\r\t"\\\\])/"\\\\" . $escapes{$1}/eg;
      "\"$x\"";
    }
    sub encode($);
    sub encode($) {
      local $_;
      my ($v) = @_;
      return "[" . join(',', map encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
      return "{" . join(',', map escape($_) . ":" . encode($$v{$_}),
                                 sort keys %$v) . "}" if 'HASH' eq ref $v;
      looks_like_number $v ? $v : defined $v ? escape $v : 'null';
    }
    }

    use overload qw/@{} json %{} json/;
    sub create {
      my ($self, $json) = @_;
      $json = ni::json::decode $json unless ref $json;
      bless \$json, $self->package;
    }
    sub json   {${$_[0]}}
    sub as_uri {"json:" . ni::json::encode $_[0]->json}));

u('ni.scheme:ni.scheme')->create('uri',
  name        => 'A structured URI object',
  synopsis    => q{ u("uri:<scheme>:...")->scheme
                  | u("uri:<scheme>:<host>/<path>")->host
                  | u("uri:<scheme>:[json array]")->data
                  | u("uri:<scheme>:{json object}")->data },
  description => q{
    ni has an object layer that parses and generates URIs. The URI syntax
    parsed by ni has nonstandard aspects, including a wide array of unofficial
    schemes and support for JSON objects and arrays in place of the normal
    post-scheme URI authority/path/etc. These modifications provide a way for
    URIs to encode array+hash data structures used to initialize objects.})

  ->uses(u"ni.behavior.code:" . q(
    {
    package ni::uri;
    sub decode($) {
      my ($scheme, $rest) = $_[0] =~ /^([^:]+):([\s\S]+)$/;
      return {scheme => $scheme,
              data   => ni::json::decode $rest} if $rest =~ /^[{[]/;
      my ($auth, $path, $query, $fragment) =
        $rest =~ /^(|\/\/[^\/?#]*)(|\/[^?#]*)(|\?[^#]+)(|#[\s\S]*)$/;
      return {scheme    => $scheme,
              authority => $auth,
              path      => $path,
              query     => $query,
              fragment  => $fragment};
    }
    sub encode {
      my %fields = @_;
      return "$fields{scheme}:" . ni::json::encode $fields{data}
        if exists $fields{data};
      join '', "$fields{scheme}:",
               @fields{qw/authority path query fragment/};
    }
    }

    sub create {
      my ($self, $uri, @args) = @_;
      my $r;
      if (@args && $uri =~ /:/) {
        $r = ni::uri::decode $uri;
        $r = exists $$r{data}
           ? {scheme => $$r{scheme},
              data   => ref $$r{data} eq 'HASH' ? {%{$$r{data}}, @args}
                                                : [@{$$r{data}}, @args]}
           : {%$r, @args};
      } else {
        $r = $uri =~ /:/ ? ni::uri::decode $uri
                         : {scheme => $uri, data => \@args};
      }
      bless $r, $self->package;
    }
    sub uri    {ni::uri::encode %{$_[0]}}
    sub as_uri {"uri:" . $_[0]->uri}));

u('ni.scheme:ni.behavior')->create('uri',
  name        => 'Adds URI serialization/deserialization to a class',
  synopsis    => 'u("ni.behavior:uri")->modify(u"ni.scheme:<scheme>")',
  description => q{ TODO },
  modifier    => u"ni.fn:" . q{
    my ($self, $scheme) = @_;
    $scheme->eval(q{
      use overload qw/"" as_uri/;
      # TODO: create a base class to autogenerate URIs for objects
    });
  });

u($_)->uses(u"ni.behavior:uri") for grep /^ni\.scheme:/, keys %ni::live;
