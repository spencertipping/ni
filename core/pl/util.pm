# Utility library functions.
# Mostly inherited from nfu. This is all loaded inline before any Perl mapper
# code. Note that List::Util, the usual solution to a lot of these problems, is
# introduced in v5.7.3, so we can't rely on it being there.

sub ceval {eval $_[0]; die "error evaluating $_[0]: $@" if $@}

sub first  {$_[0]}
sub final  {$_[$#_]}
sub rando  {$_[int(rand($#_ + 1))]}
sub max    {local $_; my $m = pop @_; $m = $m >  $_ ? $m : $_ for @_; $m}
sub min    {local $_; my $m = pop @_; $m = $m <  $_ ? $m : $_ for @_; $m}
sub maxstr {local $_; my $m = pop @_; $m = $m gt $_ ? $m : $_ for @_; $m}
sub minstr {local $_; my $m = pop @_; $m = $m lt $_ ? $m : $_ for @_; $m}

sub take($@) {my ($n, @xs) = @_; @xs[0..($n-1)]}
sub drop($@) {my ($n, @xs) = @_; @xs[$n..$#xs]} 

sub take_while(&@) {
  local $_;
  my ($f, @xs) = @_; 
  my @out; 
  for (@xs) { if(&$f($_)) {push @out, $_} else {last} }
  @out;
}

sub drop_while(&@) {
  local $_;
  my ($f, @xs) = @_;
  my @out;
  my $count = 0;
  for (@xs) { if(!&$f($_)) {return drop $count, @xs} $count++;}
  ();
} 

sub take_every($$@) {
  my ($every, $start, @r) = @_;
  @r[grep { ($_ - $start) % $every == 0 } 0..$#r];
}

sub take_even(@) {
  take_every(2, 0, @_);
}

sub take_odd(@) {
  take_every(2, 1, @_);
}


sub deltas {local $_; return () unless @_ > 1; map $_[$_] - $_[$_ - 1], 1..$#_}
sub totals {local $_; my ($x, @xs) = 0; push @xs, $x += $_ for @_; @xs}

sub kbv_dsc { my %h = @_; sort { $h{$b} <=> $h{$a} } keys %h }
sub kbv_asc { my %h = @_; sort { $h{$a} <=> $h{$b} } keys %h }

sub argmax(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) > $fm;
  }
  $m;
}

sub argmin(&@) {
  local $_;
  my ($f, $m, @xs, $fx) = @_;
  my $fm = &$f($_ = $m);
  for (@xs) {
    ($m, $fm) = ($_, $fx) if ($fx = &$f($_)) < $fm;
  }
  $m;
}

sub any(&@) {local $_; my ($f, @xs) = @_; &$f($_) && return 1 for @xs; 0}
sub all(&@) {local $_; my ($f, @xs) = @_; &$f($_) || return 0 for @xs; 1}

sub uniq  {local $_; my(%seen, @xs); $seen{$_}++ or push @xs, $_ for @_; @xs}
sub freqs {local $_; my %fs; ++$fs{$_} for @_; \%fs}

sub reduce(&$@) {local $_; my ($f, $x, @xs) = @_; $x = &$f($x, $_) for @xs; $x}
sub reductions(&$@) {
  local $_;
  my ($f, $x, @xs, @ys) = @_;
  push @ys, $x = &$f($x, $_) for @xs;
  @ys;
}

sub cart {
  use integer;
  local $_;
  return () unless @_;
  @$_ or return () for @_;
  return map [$_], @{$_[0]} if @_ == 1;
  my @ns = map scalar(@$_), @_;
  map {
    my ($i, $xs) = ($_ - 1, []);
    for (reverse 0..$#ns) {
      unshift @$xs, ${$_[$_]}[$i % $ns[$_]];
      $i /= $ns[$_];
    }
    $xs;
  } 1..prod(@ns);
}

sub clip {
  local $_;
  my ($lower, $upper, @xs) = @_;
  wantarray ? map min($upper, max $lower, $_), @xs
            : min $upper, max $lower, $xs[0];
}

sub within {
  local $_;
  my ($lower, $upper, @xs) = @_;
  not grep $_ < $lower || $_ > $upper, @xs;
}

sub rf  {open my $fh, "< $_[0]" or die "rf $_[0]: $!"; my $r = join '', <$fh>; close $fh; $r}
sub rfl {open my $fh, "< $_[0]" or die "rl $_[0]: $!"; my @r =          <$fh>; close $fh; @r}
sub rfc {chomp(my $r = rf @_); $r}

sub dirbase($)  {my @xs = $_[0] =~ /^(.*)\/+([^\/]+)\/*$/; @xs ? @xs : ('', $_[0])}
sub basename($) {(dirbase $_[0])[1]}
sub dirname($)  {(dirbase $_[0])[0]}

sub mkdir_p {-d $_[0] or !length $_[0] or mkdir_p(dirname $_[0]) && mkdir $_[0]}

sub wf {
  local $_;
  my $f = shift;
  my $fh;
  if ($f =~ /^\|/) {
    open $fh, $f or die "wf $f: $!";
  } else {
    mkdir_p dirname $f;
    open $fh, "> $f" or die "wf $f: $!";
  }
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

sub af {
  local $_;
  my $f = shift;
  mkdir_p dirname $f;
  open my $fh, ">> $f" or die "af $f: $!";
  print $fh /\n$/ ? $_ : "$_\n" for @_;
  close $fh;
  $f;
}

sub testpath {
  $_ =~ s/-\*/-0000\*/;
  $_;
}

our $base64_digits = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/!#$%&()[]*@?|;<>';
our @base64_digits = split //, $base64_digits; 
our $base64_ext_digits = substr($base64_digits, -16);
our @base64_ext_digits = split //, $base64_ext_digits;
our %base64_ext_decode = map(($base64_ext_digits[$_], 1), 0..$#base64_ext_digits);
our %base64_decode = map(($base64_digits[$_], ($_ % 64)), 0..$#base64_digits);

sub hex2extbase64($) {
  my $hex_num = hex $_[0];
  my $n_hex_chars = length $_[0];
  if ($n_hex_chars == 3) {
    $base64_digits[$hex_num >> 6] . $base64_digits[$hex_num % 64];
  } elsif ($n_hex_chars == 2) {
    $base64_digits[$hex_num >> 2] . $base64_ext_digits[$hex_num % 4];
  } else {
    $base64_ext_digits[$hex_num];
  }
}

sub hex2base64($) {
  $_[0] =~ tr/\-//d;
  my @hex_strs = unpack("(A3)*", $_[0]); 
  my $last_hex_str = pop @hex_strs;
  my @hex_nums = map {hex $_} @hex_strs;
  my @b64_strs = map { $base64_digits[$_ >> 6] . $base64_digits[$_ % 64] } @hex_nums;
  my $b64_str = join("", @b64_strs);
  my $last_chars = hex2extbase64 $last_hex_str;
  $b64_str . $last_chars;
}

sub extbase642hex ($) {
  my $n_hex_digits = length($_[0]) + 1 - sum(map { $base64_ext_decode{$_} } (split //, $_[0]));
  my $fmt_str = "%0" . $n_hex_digits . "x";
  my $value;
  if ($n_hex_digits > 1) {
    $shift_amt = $n_hex_digits == 2 ? 2 : 6;
    $value = ($base64_decode{substr($_[0], 0, 1)} << $shift_amt) + $base64_decode{substr($_[0], 1, 1)}; 
  } else {
    $value = $base64_decode{$_[0]}; 
  }
  sprintf $fmt_str, $value;
}

sub base642hex($) {
  my @b64_strs = unpack("(A2)*", $_[0]); 
  my $last_b64 = pop @b64_strs;
  my $last_hex_str = extbase642hex $last_b64;
  my @b64_nums = map { ($base64_decode{substr($_, 0, 1)} << 6) + $base64_decode{substr($_, 1, 1)}} @b64_strs;
  my @hex_strs = map {sprintf "%03x", $_} @b64_nums;
  my $output_str = join("", @hex_strs) .  $last_hex_str;
  $output_str
}

sub hyphenate_uuid($) {
  join("-", substr($_[0], 0, 8), substr($_[0], 8, 4), 
            substr($_[0], 12, 4), substr($_[0], 16, 4),
            substr($_[0], 20))
}

sub startswith($$) {
  my $affix_length = length($_[1]);
  substr($_[0], 0, $affix_length) eq $_[1]
}

sub endswith($$) {
  my $affix_length = length($_[1]);
  substr($_[0], -$affix_length) eq $_[1]
}

# Indexed Hash Methods
#

sub ihash_get {
  my @raw_output = ihash_all(@_);
  map {first grep defined, @$_} @raw_output;
}

sub ihash_def {
  my @raw_output = ihash_all(@_);
  map {my @def_out = grep defined, @$_; \@def_out;} @raw_output;
}

sub ihash_all {
  my ($ks_ref, $min_key_length, @hash_and_val_refs) = @_;
  unless (ref($ks_ref)) { my @ks = ($ks_ref, ); $ks_ref = \@ks; }
  my @index_hash_refs = take_even @hash_and_val_refs;
  my @hash_val_refs = take_odd @hash_and_val_refs;
  my @potential_keys = map{ my $k = $_; map {substr($k, 0, $_)} $min_key_length..length($k) } @$ks_ref; 
  my @output;
  for (0..$#hash_val_refs) {
    my %index_hash = %{$index_hash_refs[$_]};
    my @hash_vals = @{$hash_val_refs[$_]};
    my @val_indices = @index_hash{@potential_keys};
    my @output_vals = @hash_vals[@val_indices];
    push @output, \@output_vals;
  }
  @output;
}


BEGIN {
  *h2b64 = \&hex2base64;
  *b642h = \&base642hex;
}
