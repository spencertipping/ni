# Column manipulation operators.
# In root context, ni interprets columns as being tab-delimited.

# Column selection.
# Normally perl is fast at text manipulation, but on most UNIX systems
# `/usr/bin/cut` is at least an order of magnitude faster. We can use it if the
# column access order is strictly ascending and has no duplicates.

sub col_cut {
  my ($floor, $rest, @fs) = @_;
  exec 'cut', '-f', join ',', $rest ? (@fs, "$floor-") : @fs;
}

defconfenv 'col/disallow-cut', NI_COL_DISALLOW_CUT => 0;

use constant cols_gen =>
  gen q{@_ = split /\t/, $_, %limit; print join "\t", @_[%is]};

defoperator cols => q{
  my ($floor, @cs) = @_;
  my $asc = join('', @cs) eq join('', sort {$a <=> $b} @cs);
  my %dup; ++$dup{$_} for @cs;
  return col_cut $floor + 1, scalar(grep $_ == -1, @cs), map $_ + 1, @cs
    if !conf "col/disallow-cut" && $asc && !grep $_ > 1, values %dup;
  exec 'perl', '-lne',
       cols_gen->(limit => $floor + 1,
                  is    => join ',', map $_ == -1 ? "$floor..\$#_" : $_, @cs);
};

defshort '/f',
  defalt 'colalt', 'list of alternatives for /f field-select operator',
    pmap q{cols_op @$_}, colspec;

# Column swapping.
# This is such a common thing to do that it gets its own operator `x`. The idea
# is that you're swapping the specified column(s) into the first N position(s).

defoperator colswap => q{
  my ($floor, @cs) = @_;
  my %cs; ++$cs{$_} for @cs;
  die "ni colswap: . doesn't make sense"    if grep $_ == -1, @cs;
  die "ni colswap: can't duplicate columns" if grep $_ > 1, values %cs;
  my $n = 0;
  my @cols = 0..$floor-1;
  swap $cols[$n++], $cols[$_] for @cs;
  exec 'perl', '-lne', cols_gen->(limit => $floor + 1,
                                  is    => join ',', @cols, "$floor..\$#_");
};

defshort '/x', pmap q{ref $_ ? colswap_op @$_ : colswap_op 2, 1}, popt colspec;

# Column splitting.
# Adapters for input formats that don't have tab delimiters. Common ones are,
# with their split-spec mnemonics:

# | commas:       C
#   slashes:      D
#   "proper CSV": V
#   pipes:        P
#   whitespace:   S
#   non-words:    W
#   non English words: EW (this allows apostrophes and hyphens)

# You can also field-split on arbitrary regexes, or extend the splitalt dsp to
# add custom split operators.

defoperator split_chr   => q{exec 'perl', '-lnpe', $_[0] =~ /\// ? "y#$_[0]#\\t#" : "y/$_[0]/\\t/"};
defoperator scan_regex  => q{exec 'perl', '-lne',  'print join "\t", /' . "$_[0]/g"};

defoperator split_regex => q{
  (my $quoted = shift) =~ s/([\$\@])/\\\\$1/g;
  my $r = qr/$quoted/;
  exec 'perl', '-lnpe', "s/$r/\$1\\t/g";
};

defoperator split_proper_csv => q{
  while (<STDIN>)
  {
    $_ = ",$_";
    $_ .= <STDIN> while 1 & (() = /"/g);
    chomp;
    print join("\t",
      map { s/^"|"$//g; s/\t/        /g; y/\n/\r/; s/""/\n/g; s/"//g; y/\n/"/; $_ }
          /\G,((?:"(?:[^"]+|"")*"|[^",]+)*)/g), "\n";
  }
};

defshort '/F',
  defdsp 'splitalt', 'dispatch table for /F split operator',
    'C'  => pmap(q{split_chr_op   ','},                pnone),
    'D'  => pmap(q{split_chr_op   '\/'},               pnone),
    'V'  => pmap(q{split_proper_csv_op},               pnone),
    'P'  => pmap(q{split_chr_op   '|'},                pnone),
    'S'  => pmap(q{split_regex_op '\s+'},              pnone),
    'W'  => pmap(q{split_regex_op '[^\w\n]+'},         pnone),
    'EW' => pmap(q{split_regex_op "[^-'\\\\w\\\\n]+"}, pnone),
    '/'  => pmap(q{split_regex_op $_},                 regex),
    ':'  => pmap(q{split_chr_op   $_},                 prx '.'),
    'm'  => pn(1, pstr '/', pmap q{scan_regex_op $_},  regex);

# Combining
defshort '/F^',
  defdsp 'combinealt', 'dispatch table for /F^ combine operator',
    'S' => pmap(q{sh_op 'tr "\t" " "'},      pnone),
    'C' => pmap(q{sh_op 'tr "\t" ,'},        pnone),
    'P' => pmap(q{sh_op 'tr "\t" "|"'},      pnone),
    ':' => pmap(q{sh_op 'tr "\t" "'.$_.'"'}, prx '.');

# Juxtaposition.
# You can juxtapose two data sources horizontally by using `w` for `with`.

defoperator with_right => q{
  my $fh = sni @_;
  my $l;
  while (<STDIN>) {
    chomp;
    return unless defined($l = <$fh>);
    print "$_\t$l";
  }
};

defoperator with_left => q{
  my $fh = sni @_;
  my $l;
  while (<STDIN>) {
    return unless defined($l = <$fh>);
    chomp $l;
    print "$l\t$_";
  }
};

defshort '/w', pmap q{with_right_op @$_}, _qfn;
defshort '/W', pmap q{with_left_op  @$_}, _qfn;


# Tab flattening.
# You often won't care about tabs when you're viewing results. This operator
# buffers the output, calculates field sizes, and flattens tabs to spaces in a
# way that aligns each column vertically.
#
# At most 1024 lines will be loaded at a time. If you have more, each 1024-line
# block will be aligned independently.

defoperator flatten_tabs =>
q{
  my ($gapsize) = @_;
  my $eof = 0;
  $gapsize //= 2;

  until ($eof)
  {
    my @lines;
    my @widths = ();
    while (@lines < 1024 && !($eof ||= !defined($_ = <STDIN>)))
    {
      chomp;
      my @fs = split /\t/;
      push @lines, \@fs;
      $widths[$_] = max $widths[$_] // 0, length $fs[$_] // 0 for 0..$#fs;
    }

    # The final width is never necessary because nothing after it needs to be
    # aligned. If we tried to align it, we'd append a bunch of trailing spaces
    # to each line.
    pop @widths;

    my $format = join("", map "%-" . ($_ + $gapsize) . "s", @widths) . "%s\n";
    printf $format, @$_ for @lines;
  }
};

defshort '/_' => pmap q{flatten_tabs_op $_}, popt integer;


# Vertical transformation.
# This is useful when you want to apply a streaming transformation to a specific
# set of columns. For example, if you have five columns and want to lowercase the
# middle one:

# | ni vCplc              # lowercase column C

# WARNING: your process needs to output exactly one line per input. If the driver
# is forced to buffer too much memory it will hang waiting for the process to
# catch up.

# TODO: optimize this. Right now it's horrendously slow.

defoperator vertical_apply => q{
  my ($colspec, $lambda) = @_;
  my ($limit, @cols) = @$colspec;
  my ($i, $o) = sioproc {exec ni_quoted_exec_args};
  safewrite $i, ni_quoted_image 0, @$lambda;

  vec(my $rbits = '', fileno $o, 1) = 1;
  vec(my $wbits = '', fileno $i, 1) = 1;
  fh_nonblock $i;

  my $read_buf = '';
  my $write_buf = '';
  my @queued;
  my @awaiting_completion;
  my $stdin_ok = my $proc_ok = 1;
  while ($stdin_ok || $proc_ok) {
    my $l = sum map length, @queued;
    $_ = '';
    chomp, push @queued, $_ while ($l += length) <= 1048576
                              and $stdin_ok &&= defined($_ = <STDIN>);

    while (@queued && sum(map length, @awaiting_completion) < 1048576
                   && select undef, my $wout=$wbits, undef, 0) {
      my $n = 0;
      my @chopped;
      push @chopped, join "\t", (split /\t/, $queued[$n++], $limit)[@cols]
        while $n < @queued && 8192 > sum map 1 + length, @chopped;
      ++$n unless $n;
      push @awaiting_completion, @queued[0..$n-1];
      @queued = @queued[$n..$#queued];
      my $s  = $write_buf . join '', map "$_\n", @chopped;
      my $sn = safewrite $i, $s;
      $write_buf = substr $s, $sn;
    }

    close $i if !@queued && !$stdin_ok;

    $proc_ok &&= saferead $o, $read_buf, 8192, length $read_buf
      while $proc_ok && select my $rout=$rbits, undef, undef, 0;

    my @lines = split /\n/, $read_buf . " ";
    $proc_ok ? $read_buf = substr pop(@lines), 0, -1 : pop @lines;
    for (@lines) {
      die "ni: vertical apply's process emitted too many lines: $_"
        unless @awaiting_completion;
      my @fs = split /\t/, shift @awaiting_completion;
      @fs[@cols] = my @cs = split /\t/;
      print join("\t", @fs, @cs[@fs..$#cs]), "\n";
    }
  }

  die "ni: vertical apply's process ultimately lost "
    . scalar(@awaiting_completion) . " line(s)"
  if @awaiting_completion;

  close $o;
  $o->await;
};

defshort '/v', pmap q{vertical_apply_op @$_}, pseq colspec_fixed, _qfn;
