# Compound reductions.
# Suppose you want to calculate, in parallel, the sum of one column and the mean
# of another. You can't use two separate `sr` calls since the first one will
# force the whole stream. Instead, you need a way to build a single compound
# function that maintains the two separate state elements. That's what `rc`
# (reduce compound) is all about.

# In fast languages we could probably get away with some nice combinatory stuff
# here, but this is performance-critical and Perl isn't fast. So I'm making some
# epic use of codegen and `eval` to help Perl be all it can be. We end up
# compiling into a single function body for a `cr` call, which is then mapped
# through a finalizer to eliminate intermediate states.

sub rsum($)  {+{reduce => gen "%1 + ($_[0])",
                init   => [0],
                end    => gen '%1'}}

sub rmean($) {+{reduce => gen "%1 + ($_[0]), %2 + 1",
                init   => [0, 0],
                end    => gen '%1 / (%2 || 1)'}}

sub rmin($)  {+{reduce => gen "defined %1 ? min %1, ($_[0]) : ($_[0])",
                init   => [undef],
                end    => gen '%1'}}

sub rmax($)  {+{reduce => gen "defined %1 ? max %1, ($_[0]) : ($_[0])",
                init   => [undef],
                end    => gen '%1'}}

sub rarr($)  {+{reduce => gen "[\@{%1}, ($_[0])]",
                init   => [[]],
                end    => gen '%1'}}

sub rfn($$)  {+{reduce => gen $_[0],
                init   => [@_[1..$#_]],
                end    => gen join ', ', map "%$_", 1..$#_}}

sub compound_reducer(@) {
  local $_;
  my $slots = 0;
  my @indexes = map {my $n = @{$$_{init}}; $slots += $n; $slots - $n} @_;
  my @mapping = map {my $i = $_;
                     [map {;$_ => sprintf "\$_[%d]", $indexes[$i] + $_ - 1}
                          1..@{$_[$i]{init}}]} 0..$#_;
  (init   => [map @{$$_{init}}, @_],
   reduce => join(', ', map $_[$_]{reduce}->(@{$mapping[$_]}), 0..$#_),
   end    => join(', ', map $_[$_]{end}->(@{$mapping[$_]}),    0..$#_));
}

# Reduce compound function.
# Executes a compound reduction using the specified stream reducer function.
# Typical usage would be like this:

# | ($sum, $mean) = rc \&sea, fsum A, fmean B;

sub rc {
  my ($f, @rs) = @_;
  my %c        = compound_reducer @rs;
  my $reduce   = eval "sub{\n($c{reduce})\n}" or die "sc: '$c{reduce}': $@";
  my $end      = eval "sub{\n($c{end})\n}"    or die "sc: '$c{end}': $@";
  &$end(&$f($reduce, @{$c{init}}));
}

# Just like for `se` functions, we define shorthands such as `rca ...` = `rc
# \&sea, ...`.

BEGIN {ceval sprintf 'sub rc%s {rc \&se%s, @_}', $_, $_ for 'a'..'q'}
