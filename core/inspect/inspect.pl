# Inspect ni's internal state

use constant inspect_gen => gen $ni::self{'core/inspect/html'};

sub inspect_page
{
  my $reply = shift;
  http_reply $reply, 200,
    inspect_gen->(
      css     => $ni::self{'core/inspect/css'},
      js      => join("\n", @ni::self{qw| core/jsplot/jquery.min.js
                                          core/inspect/inspect.js |}),
      content => join "\n", @_);
}

sub inspect_snip
{
  my $reply = shift;
  http_reply $reply, 200, join "\n", @_;
}

sub inspect_linkable_things()
{
  # There's a lot of stuff here: operators, attributes, libraries, etc.
  (bootcode => [bootcode => 'bootcode'],
   map(($_        => [sub      => $_]), map /sub\s+(\w+)/g,          @ni::self{grep /\.pl$/, sort keys %ni::self}),
   map(($_        => [constant => $_]), map /use constant\s+(\w+)/g, @ni::self{grep /\.pl$/, sort keys %ni::self}),
   map(($_        => [lib      => $_]), grep s/\/lib$//, sort keys %ni::self),
   map(($_        => [doc      => $_]), grep /^doc\//,   sort keys %ni::self),
   map(($_        => [short    => $_]), sort keys %ni::shorts),
   map(($_        => [attr     => $_]), sort keys %ni::self),
   map(($_        => [op       => $_]), sort keys %ni::operators),
   map(("${_}_op" => [op       => $_]), sort keys %ni::operators),
   map(($_        => [meta_op  => $_]), sort keys %ni::meta_operators),
   map(("${_}_op" => [meta_op  => $_]), sort keys %ni::meta_operators),
   map(($_        => [dsp      => $_]), sort keys %ni::dsps),
   map(($_        => [alt      => $_]), sort keys %ni::alts),
   map(($_     => [cli_special => $_]), sort keys %ni::cli_special));
}

sub inspect_link_for($)
{
  my ($type, $thing) = @{+shift};
  "<a href='/$type/$thing'>$thing</a>";
}

sub inspect_linkify
{
  my @xs = @_;
  my %linkable_things = inspect_linkable_things;
  my $link_detector = join"|", map qr/\Q$_\E/,
                               sort {length($b) - length($a)}
                               keys %linkable_things;
  my $r = qr/(^|\W)($link_detector)(\W|$)/;
  s/$r/$1 . inspect_link_for($linkable_things{$2}) . $3/ge for @xs;
  @xs;
}

sub inspect_text { ("<pre>", inspect_linkify(@_), "</pre>") }

sub inspect_attr
{
  my ($reply, $k) = @_;
  inspect_snip $reply, "<h1>Attribute <code>$k</code></h1>",
    inspect_text $ni::self{$k};
}

sub inspect_lib
{
  my ($reply, $k) = @_;
  inspect_snip $reply, "<h1>Library <code>$k</code></h1>",
    inspect_text lib_entries $k, $ni::self{"$k/lib"};
}

sub inspect_defined
{
  my ($reply, $type, $def_regex, $k) = @_;
  my @defined_in = grep $ni::self{$_} =~ /$def_regex\Q$k\E\W/s, sort keys %ni::self;

  if (@defined_in == 1)
  {
    my ($in) = @defined_in;
    inspect_snip $reply, "<h1><code>$type $k</code></h1>",
      inspect_linkify("<h2>Defined in <code>$in</code></h2>"),
      inspect_text $ni::self{$in} =~ /($def_regex\Q$k\E\W.*$)/s;
  }
  else
  {
    inspect_snip $reply, "<h1><code>$type $k</code></h1>",
      "<h2>Defined in</h2>",
      "<ul>",
      inspect_linkify(map "<li>$_</li>", @defined_in),
      "</ul>";
  }
}

sub inspect_sub         { inspect_defined $_[0], sub         => qr/sub\s+/, $_[1] }
sub inspect_constant    { inspect_defined $_[0], constant    => qr/use constant\s+/, $_[1] }
sub inspect_cli_special { inspect_defined $_[0], cli_special => qr/defclispecial\s+['"]?\s*/, $_[1] }
sub inspect_short       { inspect_defined $_[0], short       => qr/defshort\s+["']?\s*/, $_[1] }
sub inspect_op          { inspect_defined $_[0], op          => qr/defoperator\s+["']?\s*/, $_[1] }
sub inspect_meta_op     { inspect_defined $_[0], meta_op     => qr/defmetaoperator\s+["']?\s*/, $_[1] }

sub inspect_bootcode
{
  my ($reply) = @_;
  inspect_snip $reply, "<h1>Bootcode</h1>", inspect_text ni::boot_header;
}

sub inspect_explain
{
  my ($reply, $command) = @_;
  my ($ops, @rest) = eval {cli shell_unquote $command};
  http_reply $reply, 200,
    json_encode {ops      => inspect_linkify(json_encode $ops),
                 unparsed => [@rest]};
}

sub inspect_root
{
  my ($reply) = @_;
  my %links;
  my @linkables = inspect_linkable_things;

  for (my $i = 0; $i + 1 < @linkables; $i += 2)
  {
    my ($k, $v)         = @linkables[$i, $i + 1];
    my ($type, $target) = @$v;
    next if $k =~ /_op$/ || $type eq 'short' && $k =~ /^\/\$/;
    push @{$links{$type} ||= []}, "<a href='/$type/$target'><code>$k</code></a>";
  }

  my @rendered;
  my %names = (short       => "Short operator parsers",
               op          => "Stream operators",
               meta_op     => "Meta operators",
               cli_special => "Toplevel CLI options",
               lib         => "Internal libraries",
               attr        => "Internal attributes",
               sub         => "Internal functions",
               constant    => "Internal constants",
               dsp         => "Parser dispatch table",
               alt         => "Parser alternative list");

  for (qw/ short op meta_op cli_special lib attr sub constant dsp alt /)
  {
    push @rendered,
      "<h1>$names{$_}</h1>",
      "<ul>",
      map("<li>$_</li>", @{$links{$_}}),
      "</ul>";
  }

  inspect_snip $reply, @rendered;
}

sub inspect_root_page
{
  my ($reply) = @_;
  inspect_page $reply, 'Loading...';
}

sub inspect_server
{
  my ($port) = @_;
  http $port, sub
  {
    my ($url, $req, $reply) = @_;
    return print "http://localhost:$port/\n" unless defined $reply;
    return inspect_root_page   $reply     if $url eq '/';
    return inspect_root        $reply     if $url =~ /^\/root/;
    return inspect_bootcode    $reply     if $url =~ /^\/bootcode/;
    return inspect_explain     $reply, $1 if $url =~ /^\/explain\/(.*)/;
    return inspect_attr        $reply, $1 if $url =~ /^\/attr\/(.*)/;
    return inspect_lib         $reply, $1 if $url =~ /^\/lib\/(.*)/;
    return inspect_sub         $reply, $1 if $url =~ /^\/sub\/(.*)/;
    return inspect_constant    $reply, $1 if $url =~ /^\/constant\/(.*)/;
    return inspect_short       $reply, $1 if $url =~ /^\/short\/(.*)/;
    return inspect_op          $reply, $1 if $url =~ /^\/op\/(.*)/;
    return inspect_meta_op     $reply, $1 if $url =~ /^\/meta_op\/(.*)/;
    return inspect_cli_special $reply, $1 if $url =~ /^\/cli_special\/(.*)/;
  };
}

defclispecial '--inspect', q{inspect_server $_[0] || 9200}, <<'_';
Usage: ni --inspect [port=9200]
Runs a web interface that allows you to inspect ni's internal attributes,
defined operators, and grammar.
_
