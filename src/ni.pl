#!/usr/bin/env perl
$ni::license = <<'_';
ni: https://github.com/spencertipping/ni
Copyright (c) 2016-2017 Spencer Tipping

MIT license

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
_
$ni::init = <<'_';
require 'ni.scheme.pl';
exit 0;
_
$ni::data = \*DATA;
eval($ni::boot = <<'_' . $ni::init);
package ni;
use strict;
use warnings;

our $image = join '', $ni::boot, $ni::init, map <$ni::data>, 1..<$ni::data>;
our %schemes;
our %live;

sub u {
  return $_[0] if ref $_[0];
  return $live{$_[0]} if defined $live{$_[0]};
  return $schemes{$1}->u($2) if $_[0] =~ /^([^:]+):(.*)/;
}

eval 'package ni::scheme;' . ($ni::scheme_meta_boot = q{
sub u {no strict 'refs'; &{$_[0]->package . "::create"}(@_)}
sub uses {$_[1]->modify($_[0])}
sub package {(my $p = ${$_[0]}{id}) =~ s/\./::/g; $p}
sub create {
  my ($self, $child, %stuff) = @_;
  $ni::schemes{$child}
    = $ni::live{"ni.scheme:$child"}
    = bless {id => $child, %stuff}, $self->package}});

eval 'package ni::behavior::code;' . ($ni::behavior_code_meta_boot = q{
sub create {bless {code => $_[1], @_[2..$#_]}, $_[0]->package}
sub modify {
  my ($self, $scheme) = @_;
  my $p = $scheme->package;
  eval "package $p;\n$$self{code}";
  die "ni: error applying $self to $scheme: $@" if $@;
  $self;
}
});

bless({id => 'ni.scheme'}, 'ni::scheme')->create('ni.scheme');
_
die $@;
__DATA__
0
