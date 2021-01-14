# Inspect ni's internal state

use constant inspect_gen => gen $ni::self{'core/inspect/html'};

our %inspect_index;
our @inspect_precedence
  = qw/ op meta_op cli_special parser short constant sub resource
        conf attr doc lib dsp alt /;

sub inspect_build_index()
{
  # There's a lot of stuff here: operators, attributes, libraries, etc.
  %inspect_index = ();

  $inspect_index{bootcode}{bootcode} = 'bootcode';
  $inspect_index{$_}{sub}      = $_ for map +(/sub\s+(\w+)/g, /\*(\w+)\s*=/g), @ni::self{grep /\.p[lm]$/, keys %ni::self};
  $inspect_index{$_}{constant} = $_ for map /use constant\s+(\w+)/g,           @ni::self{grep /\.p[lm]$/, keys %ni::self};
  $inspect_index{$_}{lib}      = $_ for grep s/\/lib$//, keys %ni::self;
  $inspect_index{$_}{doc}      = $_ for grep /^doc\//,   keys %ni::self;

  $inspect_index{"$_:"}{resource} = $_ for keys %ni::resource_read;
  $inspect_index{$_}{conf}        = $_ for keys %ni::conf_varibles;
  $inspect_index{$_}{parser}      = $_ for keys %ni::parsers;
  $inspect_index{$_}{short}       = $_ for keys %ni::shorts;
  $inspect_index{$_}{long}        = $_ for keys %ni::longs;
  $inspect_index{$_}{attr}        = $_ for keys %ni::self;
  $inspect_index{$_}{op}
    = $inspect_index{"${_}_op"}{op} = $_ for keys %ni::operators;

  $inspect_index{$_}{meta_op}
    = $inspect_index{"${_}_op"}{meta_op} = $_ for keys %ni::meta_operators;

  $inspect_index{$_}{dsp}         = $_ for keys %ni::dsps;
  $inspect_index{$_}{alt}         = $_ for keys %ni::alts;
  $inspect_index{$_}{cli_special} = $_ for keys %ni::cli_special;
}

sub markdown_table($)
{
  (my $table = shift)
    =~ s/^(.*)\n[-|\s]+\n/join("|", map"**$_**", split m(\|), $1) . "\n"/e;
  ("<table><tbody>",
   map(("<tr>",
        map(("<td>", markdown_to_html($_), "</td>"), split /\|/),
        "</tr>"),
       split /\n/, $table),
   "</tbody></table>");
}

sub markdown_to_html($)
{
  map /^\`\`\`(.*)([\s\S]*)\`\`\`\h*$/ ?
      ("<div class='code-$1'>", inspect_text($2), "</div>")

    : /^\`([^\`]*)\`$/ ?
      ("<code>", html_escape($1), "</code>")

    : /^!\[[^]]*\]\((.*)\)/ ?
      "<img src='$1'>"

    : /^\[([^]]+)\]\((https?:.*)\)/ ?
      ("<a href='$2'>", markdown_to_html($1), "</a>")

    : /^\[([^]]+)\]\((.*)\)/ ?
      exists $ni::self{"doc/$2"}
        ? ("<a href='/doc/doc/$2'>",  markdown_to_html($1), "</a>")
        : ("<a href='/attr/doc/$2'>", markdown_to_html($1), "</a>")

    : /^>/ ?
      ("<blockquote>",
       markdown_to_html(join"\n", map substr($_, 1), split /\n/, $_),
       "</blockquote>")

    : /^\n(\h*)(?:[-*]|\d\.) (.*)/ ?
      ("<ul style='padding-left:".(2 + length($1))."em'><li>",
       markdown_to_html($2),
       "</li></ul>")

    : /^\n######(.*)/  ? ("<h6>",   markdown_to_html($1), "</h6>")
    : /^\n#####(.*)/   ? ("<h5>",   markdown_to_html($1), "</h5>")
    : /^\n####(.*)/    ? ("<h4>",   markdown_to_html($1), "</h4>")
    : /^\n###(.*)/     ? ("<h3>",   markdown_to_html($1), "</h3>")
    : /^\n##(.*)/      ? ("<h2>",   markdown_to_html($1), "</h2>")
    : /^\n#(.*)/       ? ("<h1>",   markdown_to_html($1), "</h1>")
    : /\|.*\n.*\|/     ? markdown_table $_
    : /^\*\*(.*)\*\*$/ ? ("<b>",    markdown_to_html($1), "</b>")
    : /^([_*])(.*)\1$/ ? ("<i>",    markdown_to_html($2), "</i>")
    : /^\n\h*$/        ? "<p>"
    :                    $_,
  split /
    (
      \n\#+.*$                                  # headings
    | \n\h*$                                    # paragraph breaks
    | _\S(?:[^_]*\S)?_                          # italics
    | \*\S(?:[^*]*\S)?\*                        # italics with *
    | \*\*\S(?:[^*]*?\S)?\*\*                   # bold
    | ^>.*(?:\n>.*)*$                           # blockquote
    | ^\`\`\`.*\n(?:[\s\S]*?\n)?\`\`\`\h*$      # code block
    | \`[^\`]*\`                                # inline code
    | (?:[^|\`\n]+(?:[-\h]\|[-\h][^|\n]*)+\n)+  # tables
    | \n\h*[-*]\s.*                             # bulleted list item
    | \n\h*\d+\.\s.*                            # numbered list item
    | !?\[[^]]+\]\([^\)]+\)                     # link
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
  http_reply $reply, 200, join "", @_;
}

sub inspect_disambiguation_for($)
{
  my $k   = shift;
  my $ref = $inspect_index{$k};
  return () unless keys(%$ref) > 1;

  ("<h2><code>$k</code> refers to:</h2>",
   "<ul>",
   map("<li><code><a href='/$_/$$ref{$_}'>$_ $k</a></code></li>",
       sort keys %$ref),
   "</ul>");
}

sub inspect_link_for($)
{
  my $k      = shift;
  my $entry  = $inspect_index{$k};
  my ($type) = grep exists $$entry{$_}, @inspect_precedence;
  "<a href='/$type/$$entry{$type}'>$k</a>";
}

sub inspect_linkify
{
  my @xs = @_;
  my $link_detector = join"|", map qr/\Q$_\E/,
                               sort {length($b) - length($a)}
                               keys %inspect_index;
  my $r = qr/(?<=\W)($link_detector)(?=\W)/;
  s/$r/inspect_link_for $1/ge for @xs;
  @xs;
}

sub inspect_text { ("<pre>", inspect_linkify(map html_escape($_), @_), "</pre>") }

sub inspect_attr
{
  my ($reply, $k) = @_;
  my $lib = $k;
  $lib =~ s/\/[^\/]+$// until $lib !~ /\// || exists $ni::self{"$lib/lib"};
  inspect_snip $reply,
    inspect_linkify("<h1>Attribute <code>$k</code></h1>"),
    inspect_disambiguation_for($k),
    inspect_linkify("<h2>Defined in library <code>$lib</code></h2>"),
    inspect_text $ni::self{$k};
}

sub inspect_lib
{
  my ($reply, $k) = @_;
  inspect_snip $reply, "<h1>Library <code>$k</code></h1>",
    "<h2>Defined in <code><a href='/root'>ni root</a></code></h2>",
    inspect_disambiguation_for($k),
    "<ul>",
    map(inspect_linkify("<li>$_</li>"), lib_entries $k, $ni::self{"$k/lib"}),
    "</ul>";
}

sub inspect_defined
{
  my ($type, $def_regex, $k) = @_;
  map +(inspect_linkify("<h2>Defined in <code>$_</code></h2>"),
        inspect_text $ni::self{$_} =~ /\n?([^\n]*$def_regex.*$)/s),
      grep $ni::self{$_} =~ /$def_regex/s, sort keys %ni::self;
}

sub inspect_defined_snip
{
  my ($reply, $type, $def_regex, $k) = @_;
  inspect_snip $reply,
    "<h1><code>$type $k</code></h1>",
    inspect_disambiguation_for($k),
    inspect_defined($type, $def_regex, $k);
 }

sub inspect_sub         { inspect_defined_snip $_[0], sub         => qr/sub\s+$_[1]\W|\*$_[1]\s*=/, $_[1] }
sub inspect_resource    { inspect_defined_snip $_[0], resource    => qr/defresource\s+['"]?\s*$_[1]\W/, $_[1] }
sub inspect_constant    { inspect_defined_snip $_[0], constant    => qr/use constant\s+$_[1]\W/, $_[1] }
sub inspect_cli_special { inspect_defined_snip $_[0], cli_special => qr/defclispecial\s+['"]?\s*\Q$_[1]\E\W/, $_[1] }
sub inspect_short       { inspect_defined_snip $_[0], short       => qr/defshort\s+["']?\s*\Q$_[1]\E\W/, $_[1] }
sub inspect_long        { inspect_defined_snip $_[0], long        => qr/deflong\s+["']?\s*\Q$_[1]\E\W/, $_[1] }
sub inspect_op          { inspect_defined_snip $_[0], op          => qr/defoperator\s+["']?\s*\Q$_[1]\E\W/, $_[1] }
sub inspect_meta_op     { inspect_defined_snip $_[0], meta_op     => qr/defmetaoperator\s+["']?\s*\Q$_[1]\E\W/, $_[1] }

sub inspect_parser_short
{
  my ($reply, $short) = @_;
  inspect_snip $reply,
    inspect_linkify("<h1>Parser for short operator <code>$short</code></h1>"),
    inspect_disambiguation_for($short),
    inspect_text(parser_ebnf $ni::shorts{$short}),
    inspect_defined(parser => qr/defparser\w*\s+['"]?\s*\Q$short\E\W/, $short);
}

sub inspect_parser_long
{
  my ($reply, $long) = @_;
  inspect_snip $reply,
    inspect_linkify("<h1>Parser for long operator <code>$long</code></h1>"),
    inspect_disambiguation_for($long),
    inspect_text(parser_ebnf $ni::longs{$long}),
    inspect_defined(parser => qr/defparser\w*\s+['"]?\s*\Q$long\E\W/, $long);
}

sub inspect_parser
{
  my ($reply, $p) = @_;
  inspect_snip $reply,
    inspect_linkify("<h1>Parser <code>$p</code></h1>"),
    inspect_disambiguation_for($p),
    inspect_text(parser_ebnf $ni::parsers{$p}),
    inspect_defined(parser => qr/defparser\w*\s+['"]?\s*\Q$p\E\W/, $p);
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
    json_encode {
      ops      => join("<br/>", map inspect_linkify(json_encode $_), @$ops),
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

  while (my ($k, $v) = each %inspect_index)
  {
    while (my ($type, $target) = each %$v)
    {
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
  }

  my @rendered =
    ("<h1>Reference</h1>",
     "<h2><i>Ni By Example</i>, by <a href='https://github.com/michaelbilow'>Michael Bilow</a></h2>",
     "<ul>",
     "<li><a href='/doc/doc/ni_by_example_1.md'>Chapter 1</a></li>",
     "<li><a href='/doc/doc/ni_by_example_2.md'>Chapter 2</a></li>",
     "<li><a href='/doc/doc/ni_by_example_3.md'>Chapter 3</a></li>",
     "<li><a href='/doc/doc/ni_by_example_4.md'>Chapter 4</a></li>",
     "<li><a href='/doc/doc/ni_by_example_5.md'>Chapter 5</a></li>",
     "<li><a href='/doc/doc/ni_by_example_6.md'>Chapter 6</a></li>",
     "<li><a href='/doc/doc/ni_fu.md'>Ni Fu</a></li>",
     "<li><a href='/doc/doc/cheatsheet_op.md'>Operator Cheatsheet</a></li>",
     "<li><a href='/doc/doc/cheatsheet_perl.md'>Ni Perl Cheatsheet</a></li>",
     "</ul>",
     "<h2>Internal documentation: entry points</h2>",
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
    my $ls = $links{$_};
    push @rendered,
      "<h1>$names{$_}</h1>",
      "<ul>",
      map("<li>$_</li>", sort @$ls),
      "</ul>" if ref $ls;
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

  inspect_build_index;
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
    return inspect_long         $reply, $1 if $url =~ /^\/long\/(.*)/;
    return inspect_op           $reply, $1 if $url =~ /^\/op\/(.*)/;
    return inspect_meta_op      $reply, $1 if $url =~ /^\/meta_op\/(.*)/;
    return inspect_cli_special  $reply, $1 if $url =~ /^\/cli_special\/(.*)/;
    return inspect_parser       $reply, $1 if $url =~ /^\/parser\/(.*)/;
    return inspect_parser_short $reply, $1 if $url =~ /^\/parser_short\/(.*)/;
    return inspect_parser_long  $reply, $1 if $url =~ /^\/parser_long\/(.*)/;
  };
}

defclispecial '--inspect', q{inspect_server $_[0] || 9200}, <<'_';
Usage: ni --inspect [port=9200]
Runs a web interface that allows you to inspect ni's internal attributes,
defined operators, and grammar.
_
