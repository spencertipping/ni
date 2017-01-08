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
MAP_GOES_HERE
_
eval($ni::boot = <<'_');
package ni;
use strict;
use warnings;

our $data = \*DATA;
our $size = <$data>;
our $image;
read $data, $image, $size - length $image, length $image
  until $! ? die "ni boot error: $! while reading state"
           : length $image >= $size;

our %live;
sub fn {$live{"ni.fn:$_[$#_]"} = eval "sub {$_[$#_]\n}"}
sub u  {ref $_[0] ? $_[0] : $live{$_[0]}}

$live{'ni.scheme:ni.scheme'} = bless {id => 'ni.scheme'}, ni::scheme;
sub ni::scheme::create {
  my ($self, $child, %stuff) = @_;
  $live{"ni.scheme:$child"} = bless {id => $child, %stuff}, ni::scheme;
}
sub ni::scheme::uses {
  my ($self, $behavior) = @_;
  u$behavior->modify($self);
  $self;
}
eval $ni::init;
_
die $@;
__DATA__
