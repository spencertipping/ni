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
BEGIN{eval($ni::code{boot} = <<'_')}
package ni::code;
push our @ISA, 'ni::obj';
sub init {
  my $self = shift;
  @$self{qw/name code/} = @_;
  $ni::code{$$self{name}} = $self;
}
sub named {$ni::code{$_[0]}}
sub eval {
  my ($self, $package) = @_;
  my $p = defined $package ? "package $package;" : '';
  eval qq{$p$$self{code}};
  die "$@ evaluating $self in $package" if $@;
}

package ni::obj;
sub new {
  local $_;
  my $class = shift;
  my $self = bless {}, $class;
  $self->init(@_);
  $self;
}
sub class {$ni::classes{ref shift}}

package ni::class;
push our @ISA, 'ni::obj';
sub init {
  my ($self, $name, @meta) = @_;
  $ni::class{$name} = $self;
  $$self{name} = $name;
  $$self{journal} = [];
  $self->eval(@meta);
}
sub named {$ni::class{$_[0]}}
sub package {shift->{name}}
sub eval {
  local $_;
  my $self = shift;
  push @{$$self{journal}}, @_;
  $_->eval($self->package) for @_;
}

ni::class->new("ni::$_") for qw/class obj code/;
ni::code->new('boot', $ni::code{boot});
_
die $@ if $@;

package ni;
use strict;
use warnings;

ni::code->new('ni::obj::serialize', q{
sub dependencies {
  my @immediates = @_;
  my %ds = ($immediates[0] => 1);
  my @ds;
  while (@immediates = grep !exists $ds{$_},
                       map $_->immediate_dependencies, @immediates) {
    @ds{@immediates} = @immediates;
    push @ds, @immediates;
  }
  @ds;
}

sub serialize {
  my $self = shift;
  $_->serialize_self(@_) for $self->dependencies;
  $self->serialize_self(@_);
  shift;
}
});

ni::code->new('ni::class::serialize', q{
sub immediate_dependencies {my $self = shift; $self->class, @{$$self{journal}}}
sub name {my $self = shift; "ni::class::named('$$self{name}')"}
sub serialize_self {
  my ($self, $dest) = @_;
  $dest->method_call('ni::class->new', $self->name, @{$$self{journal}});
}
});

ni::code->new('ni::code::serialize', q{
sub immediate_dependencies {shift->class}
sub name {my $self = shift; "ni::code::named('$$self{name}')"}
sub serialize_self {
  my ($self, $dest) = @_;
  $dest->method_call('ni::code->new', @$self{qw/name code/});
}
});

ni::class::named('ni::obj')->eval(ni::code::named('ni::obj::serialize'));
ni::class::named('ni::class')->eval(ni::code::named('ni::class::serialize'));
ni::class::named('ni::code')->eval(ni::code::named('ni::code::serialize'));

package s;
sub method_call {push @{$_[0]}, "[" . join(' ', @_) . "]"}

my $s = bless [], 's';
ni::class::named('ni::code')->serialize($s);
print "$_\n" for @$s;
