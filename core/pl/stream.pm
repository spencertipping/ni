# Perl stream-related functions.
# Utilities to parse and emit streams of data. Handles the following use cases:

# $ ni n10p'a + a'             # emit single value
# $ ni n10p'a, a * a'          # emit multiple values vertically
# $ ni n10p'r a, a * a'        # emit multiple values horizontally

# Lowercase letters followed by underscores are field-extractors that can take an
# array of lines and return an array of field values. These are useful in
# conjunction with the line-reading functions `rw`, `ru`, and `re`.

no warnings 'uninitialized';

our @q;
our @F;

sub rl(;$);
sub rl(;$) {return ($_, map rl(), 2..$_[0]) if @_;
            chomp($_ = @q ? shift @q : <STDIN>); @F = split /\t/; $_}
sub pl($)  {chomp, push @q, $_ until @q >= $_[0] || !defined($_ = <STDIN>); @q[0..$_[0]-1]}
sub F_(@)  {@_ ? @F[@_] : @F}
sub FM()   {$#F}
sub FR($)  {$_[0] > 0 ? @F[$_[0]..$#F] : @F[($#F + $_[0] + 1)..$#F]}
sub FT($)  {$_[0] > 0 ? @F[0..($_[0] - 1)] : @F[0..($#F + $_[0])]}
sub r(@)   {(my $l = join "\t", @_) =~ s/\n//g; print $l, "\n"; ()}

sub dor($$) { defined $_[0] ? $_[0] : $_[1] }

BEGIN {ceval sprintf 'sub %s():lvalue {$ni::pl::F[%d]}', $_, ord($_) - 97 for 'a'..'l';
       ceval sprintf 'sub %s_ {local $_;
                               die "coercing %s_() to a scalar is a mistake" unless wantarray;
                               map(dor((split /\t/)[%d], ""), map split(/\n/), @_)}',
                     $_, $_, ord($_) - 97 for 'a'..'l'}
BEGIN {ceval sprintf 'sub %s__ {my @r;
                                die "coercing %s__() to a scalar is a mistake" unless wantarray;
                                foreach my $line(@_) 
                                { my @line_arr = split /\t/, $line; 
                                  my @short = @line_arr[%d..$#line_arr];
                                  push @r, @short } @r }', $_, $_, ord($_) - 97 for 'a'..'l'}

sub FRA() {@F}
BEGIN {ceval sprintf 'sub FR%s() {FR %d}', $_, ord($_) - 65 for 'B'..'Z'}

sub cols(@) {
  local $_;
  for my $i (0..min map $#{$_}, @_) {
    r map $_[$_][$i], 0..$#_;
  }
  ();
}

# Hash constructors.
# Pairs of letters you can use to index one column against another. For example,
# `%h = ab_ @lines` is the same as `@h{a_ @lines} = b_ @lines`.

BEGIN {for my $x ('a'..'l') {
         ceval sprintf 'sub %s%s_ {my %%r; @r{%s_ @_} = %s_ @_; %%r}',
                       $x, $_, $x, $_ for 'a'..'l'}}
BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
         ceval sprintf 'sub %s%sS {my %%r; my @key_arr = %s_ @_; my @val_arr = %s_ @_;
                                    $r{$key_arr[$_]} += $val_arr[$_] for 0..$#key_arr; %%r}',
                       $x, $y, $x, $y }}}
BEGIN {for my $x ('a'..'l') {
        for my $y ('a'..'l') {
         ceval sprintf 'sub %s%sSNN {my %%r; my @key_arr = %s_ @_; my @val_arr = %s_ @_;
                                    $r{$key_arr[$_]} += $val_arr[$_] for 0..$#key_arr;
                                    my %%r_output; my @filtered_keys = grep {$_ ne ""} keys %%r;
                                    @r_output{@filtered_keys} = @r{@filtered_keys}; %%r_output}',
                       $x, $y, $x, $y }}}

## Seeking functions.
# It's possible to read downwards (i.e. future lines), which returns an array and
# sends the after-rejected line into the lookahead queue to be used by the next
# iteration. Mnemonics:

# | rw: read while condition
#   ru: read until condition
#   re: read while equal

# These functions all read things into memory. If you want to stream stuff, you
# can do it in two ways. One is to use control flow with the 'rl' (read line)
# function:

# | do_stuff until rl =~ /<\//;           # iterate until closing XML tag
#   push @q, $_;                          # important: stash rejected line

sub r1()  {my @r = ($_); push @r, $_ while  defined rl; push @q, $_ if defined $_;  @r}
sub rw(&) {my @r = ($_); push @r, $_ while  defined rl && &{$_[0]}; push @q, $_ if defined $_; @r}
sub ru(&) {my @r = ($_); push @r, $_ until !defined rl || &{$_[0]}; push @q, $_ if defined $_; @r}
sub re(&) {my ($f, $i) = ($_[0], &{$_[0]}); rw {&$f eq $i}}
sub rea() {re {a}}
sub reA() {re {a}}
BEGIN {ceval sprintf '
       our $warned_about_lowercase_reX = 0;
       sub re%s() {
         warn "WARNING: the lowercase re<col> functions are deprecated "
            . "because ref conflicts with the perl ref() builtin; you "
            . "should use re%s instead" unless $warned_about_lowercase_reX++;
         re {join "\t", @ni::pl::F[0..%d]}}',
       $_, uc, ord($_) - 97 for 'b'..'l'}

BEGIN {ceval sprintf 'sub re%s() {re {join "\t", @ni::pl::F[0..%d]}}',
       $_, ord($_) - 65 for 'B'..'Z'}

# Streaming aggregations.
# These functions are like the ones above, but designed to work in constant
# space:

# | se<column>: streaming reduce while everything up to column is equal
#   sr: streaming reduce all data

sub se(&$@) {my ($f, $e, @xs) = @_; my $k = &$e;
             @xs = &$f(@xs), rl while defined and &$e eq $k;
             push @q, $_ if defined; @xs}
BEGIN {ceval sprintf 'sub se%s(&$@) {
                        my ($f, @xs) = @_;
                        se {&$f(@_)} sub {join "\t", @ni::pl::F[0..%d]}, @xs;
                      }', $_, ord($_) - 97 for 'a'..'l'}
BEGIN {ceval sprintf 'sub se%s(&$@) {
                        my ($f, @xs) = @_;
                        se {&$f(@_)} sub {join "\t", @ni::pl::F[0..%d]}, @xs;
                      }', $_, ord($_) - 65 for 'A'..'Z'}

sub sr(&@) {my ($f, @xs) = @_; @xs = &$f(@xs), rl while defined;
            wantarray ? @xs : $xs[0]}
