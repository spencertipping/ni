# Row-level operations.
# These reorder/drop/create entire rows without really looking at fields.

defoperator head => q{exec 'head', @_};
defoperator tail => q{exec 'tail', $_[0], join "", @_[1..$#_]};

defoperator safe_head => q{$. <= $_[0] && print while <STDIN>};

defconfenv 'row/seed', NI_ROW_SEED => 42;

defoperator row_every => q{($. - 1) % $_[0] || print while <STDIN>};
defoperator row_match => q{$\ = "\n"; chomp, /$_[0]/o && print while <STDIN>};
defoperator row_sample => q{
  srand conf 'row/seed';
  my $first_line = 0;
  while (<STDIN>) {
    print, if rand() < $_[0] || (++$first_line == 1);
  }
};

defoperator row_repeat => q{
  my $col = shift;
  while (defined (my $l = <STDIN>))
  {
    my $r = (split /\t/, $l, $col + 2)[$col];
    print $l for 1..$r;
  }
};

defoperator row_cols_defined => q{
  my ($floor, @cs) = @_;
  my @pieces = ('[^\t\n]*') x $floor;
  $pieces[$_] = '[^\t\n]+' for @cs;
  my $r = join '\t', @pieces;
  $r = qr/^$r/;
  /$r/ and print while <STDIN>;
};

defoperator row_include_or_exclude_exact => q{
  my ($include_mode, $col, $lambda) = @_;
  my %set;
  my $fh = sni @$lambda;
  chomp, ++$set{$_} while <$fh>;
  close $fh;
  $fh->await;
  while (<STDIN>) {
    chomp;
    my @fs = split /\t/, $_, $col + 2;
    print "$_\n" if !$include_mode == !$set{$fs[$col]};
  }
};

defshort '/r',
  defalt 'rowalt', 'alternatives for the /r row operator',
    pmap(q{tail_op '-n', '',  $_},       pn 1, prx '[+~]', integer),
    pmap(q{tail_op '-n', '+', ($_ + 1)}, pn 1, prx '-',    integer),
    pmap(q{safe_head_op  $_},            pn 1, prx 's',    number),
    pmap(q{row_every_op  $_},            pn 1, prx 'x',    number),
    pmap(q{row_repeat_op $_},            pn 1, prx 'x',    colspec1),
    pmap(q{row_match_op  $_},            pn 1, prx '/',    regex),
    pmap(q{row_sample_op $_},                  prx '\.\d+'),
    pmap(q{head_op '-n', 0 + $_},        integer),
    pmap(q{row_include_or_exclude_exact_op 1, @$_}, pn [1, 2], pstr 'i', colspec1, _qfn),
    pmap(q{row_include_or_exclude_exact_op 0, @$_}, pn [1, 2], pstr 'I', colspec1, _qfn),
    pmap(q{row_cols_defined_op @$_},     colspec_fixed);


# Re-splitting rows.
# The R operator is similar to F for fields: it's a fast way to change row
# boundaries. This can be useful in conjunction with S[].
#
# These operators must be fast. The typical use case is to rearrange line
# boundaries before entering a S[] block to parallelize a computation -- so we
# get only one thread to do whatever we need to do. This means where possible,
# we skip Perl-level line processing and anything else that would slow us down.

defoperator unrow_cr   => q{ exec 'tr', "\n", "\r" };
defoperator unrow_tabs => q{ exec 'tr', "\n", "\t" };

defoperator unrow_sized => q{
  use bytes;
  my ($join, $len) = @_;
  my $buf = '';
  my $read_size = max conf('pipeline/io-size'), $len;

  while (1)
  {
    saferead \*STDIN, $buf, $read_size, length $buf or goto end
      until length $buf >= $len;

    # Now we can start searching for a line boundary.
    my $n = -1;
    for (my $i = $len; ($n = index $buf, "\n", $i) == -1;)
    {
      $i = length $buf;
      saferead \*STDIN, $buf, $read_size, length $buf or goto end;
    }

    # We have a line boundary. Replace newlines in everything to its left, then
    # emit it.
    (my $row = substr $buf, 0, $n) =~ s/\n/$join/g;
    print $row, "\n";
    $buf = substr $buf, $n + 1;
  }

end:
  # Replace all newlines in $buf and terminate with a newline.
  chomp $buf;
  $buf =~ s/\n/$join/g;
  print $buf, "\n";
};

defshort '/R^' => pmap q{$_ ? unrow_sized_op "\r", $_ : unrow_cr_op},
                       popt datasize;

defshort '/R,' => pmap q{$_ ? unrow_sized_op "\t", $_ : unrow_tabs_op},
                       popt datasize;

defoperator row_split_str => q{
  my ($str) = @_;
  my $read_size = max 65536, 4 * length $str;
  my $buf = '';
  my $last = 0;

  while (saferead \*STDIN, $buf, $read_size, length $buf)
  {
    my $i;
    if (($i = index $buf, $str, $last) == -1)
    {
      # Nothing found yet, but the next character we read might do it. Set $last
      # accordingly.
      $last = length $buf - length $str + 1;
    }
    else
    {
      $buf =~ s/\Q$str\E/\n/g;
      $last = 1 + rindex $buf, "\n";
      print substr $buf, 0, $last;
      $buf = substr $buf, $last;
      $last = 0;
    }
  }

  # If we have anything left, do the search/replace and print it.
  $buf =~ s/\Q$str\E/\n/g;
  print $buf;
};

defshort '/R=' => pmap q{row_split_str_op $_}, prx '.*';


defconfenv 'row/regex-bufsize',     NI_ROW_REGEX_BUFSIZE     => 1048576;
defconfenv 'row/regex-contextsize', NI_ROW_REGEX_CONTEXTSIZE => 8192;

defoperator row_regex => q{
  my ($re) = @_;
  my $buf = '';
  my $r = qr/$re/;

  my $bufsize     = conf('row/regex-bufsize');
  my $contextsize = conf('row/regex-contextsize');

  while (saferead \*STDIN, $buf, $bufsize, length $buf)
  {
    next if length $buf < $contextsize;

    my $last = max 0, length($buf) - $contextsize;
    while ($buf =~ /($r)/g)
    {
      $last = $+[0];
      print $1, "\n";
    }
    $buf = substr $buf, $last;
  }

  print $1, "\n" while $buf =~ /($r)/g;
};

defshort '/R/' => pmap q{row_regex_op $_}, regex;


# Sorting.
# ni has four sorting operators, each of which can take modifiers:

# | g     group: sort by byte ordering
#   G     groupuniq: sort + uniq by byte ordering
#   o     order: sort numeric ascending
#   O     rorder: sort numeric descending

# Modifiers follow the operator and dictate the column index and, optionally, the
# type of sort to perform on that column (though a lot of this is already
# specified by which sort operator you use). Columns are specified as A-Z, and
# modifiers, which are optional, are any of these:

# | g     general numeric sort (not available for all 'sort' versions)
#   n     numeric sort
#   -     reverse (I would use 'r', but it conflicts with the row operator)

BEGIN {defparseralias sortspec => prep pseq colspec1, popt prx '[-gn]+'}

sub sort_args {'-t', "\t",
               map {my $i = $$_[0] + 1;
                    (my $m = defined $$_[1] ? $$_[1] : '') =~ s/-/r/g;
                    ('-k', "$i$m,$i")} @_}

# Compatibility detection.
# GNU coreutils sort supports some useful options like `--buffer-size` and
# `--compress-program`. We should use these if they exist because they can make a
# huge difference when processing large datasets.

# Note that we localize compatibility detection down to the operator -- we don't
# do it system-wide or at parse time. The reason is that parameterized operators
# can be moved, potentially across machines; this really is the only way to do it
# reliably.

sub sort_supports(@) {
  my $args = shell_quote @_;
  my $p    = siproc {sh "sort $args >/dev/null 2>&1"};
  close $p;
  return !$p->await;
}

sub sort_extra_args(@) {
  my @r;
  sort_supports @r, $_ and push @r, $_ for @_;
  @r;
}

my $n_cpus = -e "/proc/cpuinfo"
  ? grep(/^processor\s*:/, rl '/proc/cpuinfo')
  : 4;

defconfenv 'row/sort-compress', NI_ROW_SORT_COMPRESS => 'gzip';
defconfenv 'row/sort-buffer',   NI_ROW_SORT_BUFFER   => '1024M';
defconfenv 'row/sort-parallel', NI_ROW_SORT_PARALLEL => $n_cpus;

defconfenv 'row/mergesort-max-inputs', NI_ROW_MERGESORT_MAX_INPUTS => 1024;

defoperator row_sort => q{
  exec 'sort', sort_extra_args(
    length(conf 'row/sort-compress')
      ? ('--compress-program=' . conf 'row/sort-compress') : (),
    '--buffer-size=' . conf 'row/sort-buffer',
    '--parallel='    . conf 'row/sort-parallel'), @_};

defoperator row_mergesort => q{
  my @files;
  while (<STDIN>)
  {
    chomp;
    push @files, is_uri $_ ? uri_path $_ : $_;
    die "ni row_mergesort: too many inputs"
      if @files > conf('row/mergesort-max-inputs');
  }
  exec 'sort', '-m', @_, @files;
};

defoperator partial_sort => q{
  my $sort_size = shift;
  my @buff = ();
  while (<STDIN>) {
    push @buff, $_;
    if (@buff == $sort_size) {
      print sort(@buff);
      @buff = ();
    }
  }
  print sort(@buff) if @buff;
};

defshort '/g',
  defalt 'sortalt', 'alternatives for the /g row operator',
    pmap(q{partial_sort_op               $_}, pn 1, prx '_', integer),
    pmap(q{row_mergesort_op   sort_args @$_}, pn 1, prx 'M', sortspec),
    pmap(q{row_sort_op        sort_args @$_}, sortspec);

defshort '/o', pmap q{row_sort_op '-n',  sort_args @$_}, sortspec;
defshort '/O', pmap q{row_sort_op '-rn', sort_args @$_}, sortspec;

defoperator row_grouped_sort => q{
  my ($key_col, $sort_cols) = @_;
  my $key_expr = $key_col
    ? qq{(split /\\t/)[$key_col]}
    : qq{/^([^\\t\\n]*)/};

  my $sort_expr = join ' || ',
    map {my $sort_op = $$_[1] =~ /[gn]/ ? '<=>' : 'cmp';
         $$_[1] =~ /-/ ? qq{\$b[$$_[0]] $sort_op \$a[$$_[0]]}
                       : qq{\$a[$$_[0]] $sort_op \$b[$$_[0]]}} @$sort_cols;

  ni::eval gen(q{
    my $k;
    my @group;
    push @group, $_ = <STDIN>;
    ($k) = %key_expr;
    while (<STDIN>) {
      my ($rk) = %key_expr;
      if ($rk ne $k) {
        print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
        @group = $_;
        $k = $rk;
      } else {
        push @group, $_;
      }
    }
    print sort {my @a = split /\t/, $a; my @b = split /\t/, $b; %sort_expr} @group;
  })->(key_expr => $key_expr, sort_expr => $sort_expr);
};

defshort '/gg', pmap q{row_grouped_sort_op @$_}, pseq colspec1, sortspec;

# Counting.
# Sorted and unsorted streaming counts.

defoperator count => q{
  my ($n, $last) = (0, undef);
  while (<STDIN>) {
    if (!defined $last or $_ ne $last) {
      print "$n\t$last" if defined $last;
      $n = 0;
      $last = $_;
    }
    ++$n;
  }
  print "$n\t$last" if defined $last;
};

defoperator uniq => q{exec 'uniq'};

defshort '/c', pmap q{count_op}, pnone;
defshort '/u', pmap q{uniq_op},  pnone;

defoperator unordered_count => q{
  my %h;
  chomp, ++$h{$_} while <STDIN>;
  while (my ($v, $c) = each %h) {print "$c\t$v\n"}
};

defshort '/U', pmap q{unordered_count_op}, pnone;

defoperator wc_l => q{sh 'wc -l'};
defshort '/wcl', pmap q{wc_l_op}, pnone;

defoperator cleandos => q{exec shell_quote 'perl', '-npe', 's/\r\n/\n/g'};
defshort '/cleandos', pmap q{cleandos_op}, pnone;

defoperator mdtable => q{
  chomp(my $header = <STDIN>);
  my $cols = $header =~ y/\t/|/;
  print "|$header|\n";
  print "|:----:|" . ":----:|" x $cols . "\n";
  chomp, y/\t/|/, print "|$_|\n" while <STDIN>;
};

defshort '/mdtable', pmap q{mdtable_op}, pnone;
