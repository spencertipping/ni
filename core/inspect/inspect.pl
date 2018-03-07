# Inspect ni's internal state

use constant inspect_gen => gen $ni::self{'core/inspect/html'};

sub markdown_to_html($)
{
  join "",
  map
  {
      /^\`\`\`(.*)([\s\S]*)\`\`\`\h*$/ ? ("<div class='code-$1'>", inspect_text($2), "</div>")
    : /^!\[[^]]*\]\((.*)\)/            ? "<img src='$1'>"
    : /^\[([^]]+)\]\((https?:.*)\)/    ? "<a href='$2'>" . markdown_to_html($1) . "</a>"
    : /^\[([^]]+)\]\((.*)\)/           ?
      exists $ni::self{"doc/$2"}
        ? "<a href='/doc/doc/$2'>"  . markdown_to_html($1) . "</a>"
        : "<a href='/attr/doc/$2'>" . markdown_to_html($1) . "</a>"

    : /^>/                             ?
      "<blockquote>"
        . markdown_to_html(join"\n", map substr($_, 1), split /\n/, $_)
        . "</blockquote>"

    : /^\n######(.*)/    ? "<h6>"   . markdown_to_html($1) . "</h6>"
    : /^\n#####(.*)/     ? "<h5>"   . markdown_to_html($1) . "</h5>"
    : /^\n####(.*)/      ? "<h4>"   . markdown_to_html($1) . "</h4>"
    : /^\n###(.*)/       ? "<h3>"   . markdown_to_html($1) . "</h3>"
    : /^\n##(.*)/        ? "<h2>"   . markdown_to_html($1) . "</h2>"
    : /^\n#(.*)/         ? "<h1>"   . markdown_to_html($1) . "</h1>"
    : /^\n\h*[-*] (.*)/  ? "<li>"   . markdown_to_html($1) . "</li>"
    : /^\n\h*\d+\. (.*)/ ? "<li>"   . markdown_to_html($1) . "</li>"
    : /^\*\*(.*)\*\*$/   ? "<b>"    . markdown_to_html($1) . "</b>"
    : /^([_*])(.*)\1$/   ? "<i>"    . markdown_to_html($2) . "</i>"
    : /^\`([^\`]*)\`$/   ? "<code>" . markdown_to_html($1) . "</code>"
    : /^\n\h*$/          ? "<p>"
    :                      $_;
  }
  split /
    (
      \n\#+.*$                                # headings
    | \n\h*$                                  # paragraph breaks
    | _\S(?:[^_]*\S)?_                        # italics
    | \*\S(?:[^*]*\S)?\*                      # italics with *
    | \*\*\S(?:[^*]*?\S)?\*\*                 # bold
    | ^>.*(?:\n>.*)*$                         # blockquote
    | ^\`\`\`.*\n(?:[\s\S]*?\n)?\`\`\`\h*$    # code block
    | \n\h*[-*]\s.*                           # bulleted list item
    | \n\h*\d+\.\s.*                          # numbered list item
    | \`[^\`]*\`                              # inline code
    | !?\[[^]]+\]\([^\)]+\)                   # link
    )
  /mx, shift;
}

sub html_escape($)
{
  my $x = shift;
  $x =~ s/&/&amp;/g;
  $x =~ s/</&lt;/g;
  $x =~ s/>/&gt;/g;
  $x;
}

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
   map(("$_:"     => [resource => $_]), sort keys %ni::resource_read),
   map(($_        => [conf     => $_]), sort keys %ni::conf_variables),
   map(($_        => [parser   => $_]), sort keys %ni::parsers),
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

sub inspect_link_for($$)
{
  return "<span class='error'>MISSING LINK for $_[0]</span>" unless ref $_[1];
  my ($name, $type, $thing) = (shift, @{+shift});
  "<a href='/$type/$thing'>$name</a>";
}

sub inspect_linkify
{
  my @xs = @_;
  my %linkable_things = inspect_linkable_things;
  my $link_detector = join"|", map qr/\Q$_\E/,
                               sort {length($b) - length($a)}
                               keys %linkable_things;
  my $r = qr/(?<=\W)($link_detector)(?=\W)/;
  s/$r/inspect_link_for $1, $linkable_things{$1}/ge for @xs;
  @xs;
}

sub inspect_text { ("<pre>", inspect_linkify(map html_escape($_), @_), "</pre>") }

sub inspect_attr
{
  my ($reply, $k) = @_;
  (my $lib = $k) =~ s/\/[^\/]+$//;
  inspect_snip $reply,
    inspect_linkify("<h1>Attribute <code>$k</code></h1>",
                    "<h2>Defined in library <code>$lib</code></h2>"),
    inspect_text $ni::self{$k};
}

sub inspect_lib
{
  my ($reply, $k) = @_;
  inspect_snip $reply, "<h1>Library <code>$k</code></h1>",
    "<h2>Defined in <code><a href='/root'>ni root</a></code></h2>",
    "<ul>",
    map(inspect_linkify("<li>$_</li>"), lib_entries $k, $ni::self{"$k/lib"}),
    "</ul>";
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
sub inspect_resource    { inspect_defined $_[0], resource    => qr/defresource\s+['"]?\s*/, $_[1] }
sub inspect_constant    { inspect_defined $_[0], constant    => qr/use constant\s+/, $_[1] }
sub inspect_cli_special { inspect_defined $_[0], cli_special => qr/defclispecial\s+['"]?\s*/, $_[1] }
sub inspect_short       { inspect_defined $_[0], short       => qr/defshort\s+["']?\s*/, $_[1] }
sub inspect_op          { inspect_defined $_[0], op          => qr/defoperator\s+["']?\s*/, $_[1] }
sub inspect_meta_op     { inspect_defined $_[0], meta_op     => qr/defmetaoperator\s+["']?\s*/, $_[1] }

sub inspect_parser_short
{
  my ($reply, $short) = @_;
  inspect_snip $reply,
    inspect_linkify("<h1>Parser for short operator <code>$short</code></h1>"),
    inspect_text(parser_ebnf $ni::shorts{$short});
}

sub inspect_parser
{
  my ($reply, $p) = @_;
  inspect_snip $reply,
    inspect_linkify("<h1>Parser <code>$p</code></h1>"),
    inspect_text(parser_ebnf $ni::parsers{$p});
}

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

sub inspect_doc
{
  my ($reply, $doc) = @_;
  inspect_snip $reply,
    "<div class='markdown'>",
    markdown_to_html "\n$ni::self{$doc}",
    "</div>";
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
    my $thing = "<a href='/$type/$target'><code>$k</code></a>";

    if ($type eq 'conf')
    {
      my ($env) = $ni::conf_variables{$k} =~ /conf_env '(\w+)'/;
      $env ||= '';
      $thing .= " <code class='conf-env'>\$$env</code>";
      $thing .= " = <code>" . conf($k) . "</code>" if length conf $k;
    }

    $thing .= " [<a href='/parser_short/$k'>grammar</a>]"
      if $type eq "short";

    push @{$links{$type} ||= []}, $thing;
  }

  my @rendered =
    ("<h1>Reference</h1>",
     "<ul>",
     "<li><a href='/doc/doc/ni_by_example_1.md'>Ni By Example, chapter 1</a></li>",
     "<li><a href='/doc/doc/ni_by_example_2.md'>Ni By Example, chapter 2</a></li>",
     "<li><a href='/doc/doc/ni_by_example_3.md'>Ni By Example, chapter 3</a></li>",
     "<li><a href='/doc/doc/ni_by_example_4.md'>Ni By Example, chapter 4</a></li>",
     "<li><a href='/doc/doc/ni_by_example_5.md'>Ni By Example, chapter 5</a></li>",
     "<li><a href='/doc/doc/ni_by_example_6.md'>Ni By Example, chapter 6</a></li>",
     "</ul>",
     "<ul>",
     "<li><a href='/bootcode'>Bootcode</a></li>",
     "<li><a href='/attr/core/boot/ni.map'>Image map file</a></li>",
     "</ul>");

  my %names = (doc         => "Documentation",
               lib         => "Installed libraries",
               resources   => "Resources",
               conf        => "Configuration variables",
               short       => "Short operator parsers",
               op          => "Stream operators",
               meta_op     => "Meta operators",
               cli_special => "Toplevel CLI options",
               attr        => "Internal attributes",
               sub         => "Internal functions",
               constant    => "Internal constants",
               dsp         => "Parser dispatch table",
               alt         => "Parser alternative list");

  for (qw/ lib cli_special doc short op meta_op conf attr sub constant dsp alt /)
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

    return inspect_root_page    $reply     if $url eq '/';
    return inspect_root         $reply     if $url =~ /^\/root/;
    return inspect_bootcode     $reply     if $url =~ /^\/bootcode/;
    return inspect_explain      $reply, $1 if $url =~ /^\/explain\/(.*)/;

    1 while $url =~ s/\/\.\.\//\//g;

    return inspect_doc          $reply, $1 if $url =~ /^\/doc\/(.*)/;
    return inspect_attr         $reply, $1 if $url =~ /^\/attr\/(.*)/;
    return inspect_lib          $reply, $1 if $url =~ /^\/lib\/(.*)/;
    return inspect_sub          $reply, $1 if $url =~ /^\/sub\/(.*)/;
    return inspect_resource     $reply, $1 if $url =~ /^\/resource\/(.*)/;
    return inspect_constant     $reply, $1 if $url =~ /^\/constant\/(.*)/;
    return inspect_short        $reply, $1 if $url =~ /^\/short\/(.*)/;
    return inspect_op           $reply, $1 if $url =~ /^\/op\/(.*)/;
    return inspect_meta_op      $reply, $1 if $url =~ /^\/meta_op\/(.*)/;
    return inspect_cli_special  $reply, $1 if $url =~ /^\/cli_special\/(.*)/;
    return inspect_parser       $reply, $1 if $url =~ /^\/parser\/(.*)/;
    return inspect_parser_short $reply, $1 if $url =~ /^\/parser_short\/(.*)/;
  };
}

defclispecial '--inspect', q{inspect_server $_[0] || 9200}, <<'_';
Usage: ni --inspect [port=9200]
Runs a web interface that allows you to inspect ni's internal attributes,
defined operators, and grammar.
_
