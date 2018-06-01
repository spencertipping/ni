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
  $. = 0;
  while (<STDIN>) {
    print, $. -= -log(1 - rand()) / $_[0] if $. >= 0;
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

defconfenv 'row/sort-compress', NI_ROW_SORT_COMPRESS => '';
defconfenv 'row/sort-buffer',   NI_ROW_SORT_BUFFER   => '64M';
defconfenv 'row/sort-parallel', NI_ROW_SORT_PARALLEL => '4';

defoperator row_sort => q{
  exec 'sort', sort_extra_args(
    length(conf 'row/sort-compress')
      ? ('--compress-program=' . conf 'row/sort-compress') : (),
    '--buffer-size=' . conf 'row/sort-buffer',
    '--parallel='    . conf 'row/sort-parallel'), @_};

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
  print "$h{$_}\t$_\n" for sort keys %h;
};

defshort '/U', pmap q{unordered_count_op}, pnone;

defoperator cleandos => q{exec shell_quote 'perl', '-npe', 's/\r\n/\n/g'};
defshort '/cleandos', pmap q{cleandos_op}, pnone; 

defoperator mdtable => q{
  my @lines;
  chomp, push @lines, $_ while <STDIN>;
  my $n_field_seps = $lines[0] =~ tr/\t//;
  my $n_fields = $n_field_seps + 1;
  my @output_lines = map {"|$_|"} map {local $_ = $_; $_ =~ s/\t/\|/g; $_} @lines;
  splice @output_lines, 1, 0, "|" . ":----:|" x $n_fields;
  print join "\n", @output_lines;
};

defshort '/mdtable', pmap q{mdtable_op}, pnone;
