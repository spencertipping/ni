#!/usr/bin/env perl
use strict;
use warnings;

# Figuring out how to write a class that uses tie() to provide custom operators
# for an overloaded reference (or itself).

# fndict->new(sub {...}) is an object that accepts push() against its array
# dereference and read-only queries to its hash dereference. The values of the
# hash are $fn->($v), where $v are the things you've pushed. For simplicity,
# nothing is cached except for the length of the array (which requires that we
# double-tie the object).
package fndict
{
  package fndict::array
  {
    sub TIEARRAY  { my $obj = $_[1]; bless \$obj, $_[0] }
    sub FETCH     { $$${$_[0]}{xs}[$_[1]] }
    sub STORE     { $$${$_[0]}{xs}[$_[1]] = $_[2] }
    sub FETCHSIZE { $$${$_[0]}{n} }
    sub STORESIZE { $$${$_[0]}{n} = $_[1] }
    sub CLEAR     { @{$$${$_[0]}{xs}} = (); $$${$_[0]}{n} = 0 }
    sub PUSH      { push @{$$${$_[0]}{xs}}, @_[1..$#_]; $$${$_[0]}{n} += $#_ }
  }

  package fndict::hash
  {
    sub TIEHASH { my $obj = $_[1]; bless \$obj, $_[0] }
    sub FETCH   { $$${$_[0]}{fn}->($_[1]) }
  }

  use overload qw/ %{} to_h @{} to_a /;
  sub new
  {
    my ($class, $fn) = @_;

    my $self = bless \{ xs => [], n => 0, fn => $fn }, $class;
    tie %{$$$self{magic_hash}  = {}}, 'fndict::hash',  $self;
    tie @{$$$self{magic_array} = []}, 'fndict::array', $self;
    $self;
  }

  sub n { ${+shift}->{n} }

  sub to_h { ${+shift}->{magic_hash}  }
  sub to_a { ${+shift}->{magic_array} }
}

my $squares = fndict->new(sub { shift() ** 2 });
push @$squares, 10, 20, 30;

print "@$squares\n";
print "@$squares{10, 20, 30}\n";
print $squares->n, "\n";
