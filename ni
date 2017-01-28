#!/usr/bin/env perl
chomp($ni::license = <<'_');
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
package ni;
eval($ni::context{'ni.module:/boot'} = <<'_');
sub ni::context {
  my @r = @ni::context{@_};
  return wantarray ? @r : $r[0] unless grep !defined, @r;
  die 'ni::context: failed to resolve '
    . join(', ', grep !defined($ni::context{$_}), @_);
}

sub ni::exists {exists $ni::context{$_[0]}}

*ni::c = \&ni::context;
*ni::e = \&ni::exists;

sub ni::eval {
  my @r;
  wantarray ? @r = eval $_[0] : $r[0] = eval $_[0];
  die "$@ evaluating " . ($_[1] || "anon {$_[0]}") if $@;
  wantarray ? @r : $r[0];
}

package ni::persistent_object;
push our @ISA, qw/ni::constructable ni::persistent_identity/;

package ni::constructable;
sub new {
  my $class = shift;
  my $self = bless {@_}, $class;
  $self->init;
  $self;
}

package ni::named;
use overload qw/"" name fallback 1/;
sub scheme {(my $s = ref shift) =~ s/::/./g; $s}
sub name {my $self = shift; $self->scheme . ":$$self{name}"}

package ni::persistent_identity;
push our @ISA, 'ni::named';
sub add_to_context {my $self = shift; $ni::context{$self->name} = $self}

package ni::module;
push our @ISA, qw/ni::persistent_object/;
sub init {my $self = shift; $self->add_to_context; $self->eval}
sub eval {my $self = shift; ni::eval $$self{code}, $self->name if exists $$self{code}}

package ni::boot_module;
push our @ISA, qw/ni::module/;
sub scheme {'ni.module'}
sub init {
  my $self = shift;
  chomp $$self{code};
  $self->add_to_context;
}
ni::boot_module->new(name => '/boot', code => ni::context 'ni.module:/boot');
_
die "$@ evaluating /boot" if $@;
ni::module::->new(q'code',q'package ni::class;
push our @ISA, qw/ni::persistent_object/;
sub class {ni::context \'ni.class:\' . ref shift}
sub init {
  my $self = shift;
  $self->add_to_context;
  *{$self->package . "::class"} = \\&ni::class::class;
  @{$self->package . "::ISA"} = @{$$self{isa}} if exists $$self{isa};
  $self->eval($$self{code}) if exists $$self{code};
}
sub parents {map ni::context("ni.class:$_"), @{shift->package . "::ISA"}}
sub package {shift->{name}}
sub eval {
  my $self = shift;
  ni::eval "package " . $self->package . ";use strict;use warnings;$_[0]",
           $_[1] || "anon : " . $self->name . " {$_[0]}";
}
sub def {
  my $self = shift;
  my %ks = @_;
  *{$self->package . "::$_"} = $ks{$_} for keys %ks;
}

package ni::boot_class;
push our @ISA, qw/ni::class/;
sub scheme {\'ni.class\'}
',q'dependencies',[q'ni.module:/boot'],q'name',q'/class.boot');
ni::boot_class::->new(q'name',q'ni::boot_class');
ni::boot_class::->new(q'name',q'ni::boot_module');
ni::boot_class::->new(q'name',q'ni::class');
ni::boot_class::->new(q'name',q'ni::module');
ni::boot_class::->new(q'name',q'ni::persistent_object');
ni::class::->new(q'code',q'use overload qw/"" __str/;
push @ni::quotation::method_call::ISA, \'ni::quotation\';
our $AUTOLOAD;
sub AUTOLOAD {
  my $self = shift;
  (my $method = $AUTOLOAD) =~ s/^.*:://;
  bless {receiver => $self,
         method   => $method,
         args     => [map ni::quote_value($_), @_]},
        \'ni::quotation::method_call\';
}
sub __str {shift->{expr}}
sub ni::quotation::method_call::__str {
  my $self = shift;
  join \'\', $$self{receiver}, \'->\', $$self{method},
           \'(\', join(\',\', @{$$self{args}}), \')\';
}
',q'name',q'ni::quotation');
ni::class::->new(q'code',q'sub init {
  my $self = shift;
  $self->add_to_context;
  $self->target_class->eval($$self{code}, $self->name) if exists $$self{code};
}
sub target_class {ni::context "ni.class:$1" if shift->{name} =~ /^([^\\/]+)\\//}
',q'isa',[q'ni::persistent_object'],q'name',q'ni::slice');
ni::module::->new(q'code',q'use Scalar::Util;
sub ni::quote($)       {bless {expr => shift}, \'ni::quotation\'}
sub ni::quote_hash($)  {ni::quote \'{\' . join(\',\', map ni::quote_value($_), %{+shift}) . \'}\'}
sub ni::quote_array($) {ni::quote \'[\' . join(\',\', map ni::quote_value($_), @{+shift}) . \']\'}
sub ni::quote_context_lookup($) {"ni::context(" . ni::quote_scalar(shift) . ")"}
sub ni::quote_scalar($) {
  my $v = shift;
  return \'undef\' unless defined $v;
  return $v if Scalar::Util::looks_like_number $v;
  $v =~ s/([\\\\\'])/\\\\$1/g;
  "q\'$v\'";
}
sub ni::quote_value($) {
  my $v = shift;
  return ni::quote_hash($v)  if \'HASH\'  eq ref $v;
  return ni::quote_array($v) if \'ARRAY\' eq ref $v;
  return ni::quote_context_lookup($v->name) if ref $v;
  ni::quote_scalar($v);
}
',q'dependencies',[q'ni.class:ni::quotation'],q'name',q'/lib/quote');
ni::slice::->new(q'code',q'sub immediate_dependencies {ni::context \'ni.module:/class.boot\'}
',q'name',q'ni::boot_class/serializable');
ni::slice::->new(q'code',q'sub immediate_dependencies {}
sub serialize_self {
  my ($self, $into) = @_;
  $into << join "\\n",
    \'#!/usr/bin/env perl\',
    q{chomp($ni::license = <<\'_\');}, $ni::license, \'_\',
    \'package ni;\',
    \'eval($ni::context{\\\'\' . $self->name . \'\\\'} = <<\\\'_\\\');\', $$self{code}, \'_\',
    qq{die "\\$@ evaluating $$self{name}" if \\$@};
}
',q'name',q'ni::boot_module/serializable');
ni::slice::->new(q'code',q'sub immediate_dependencies {
  my $self = shift;
  ni::context($self->declared_dependencies), $self->class, $self->parents;
}
',q'name',q'ni::class/serializable');
ni::slice::->new(q'code',q'sub immediate_dependencies {ni::context shift->declared_dependencies}
',q'name',q'ni::module/serializable');
ni::slice::->new(q'code',q'sub immediate_dependencies {
  my $self = shift;
  ni::context($self->declared_dependencies), $self->class, $self->target_class;
}
',q'name',q'ni::slice/serializable');
ni::class::->new(q'code',q'push @ni::persistent_object::ISA, __PACKAGE__;
sub declared_dependencies {
  my $ds = shift->{dependencies};
  return () unless $ds;
  return @$ds if \'ARRAY\' eq ref $ds;
  return &$ds;
}
sub dependencies {
  my @q;
  my %d = ($_[0] => [@q = $_[0]->immediate_dependencies]);
  @q = grep !$d{$_}, map @{$d{$_} ||= [$_->immediate_dependencies]}, @q while @q;
  my @d;
  while (keys %d) {
    my @ks = grep {my $k = $_; !grep {$_->name ne $k && exists $d{$_}} @{$d{$k}}} sort keys %d;
    die "ni::serializable/dependencies: circular dependencies in\\n"
      . join("\\n", map "  $_ [@{$d{$_}}]", sort keys %d) unless @ks;
    push @d, @ks;
    delete @d{@ks};
  }
  ni::context @d;
}
sub serialize {
  my $self = shift;
  $_->serialize_self(@_) for $self->dependencies;
  shift;
}
sub serialize_self {
  my ($self, $into) = @_;
  $into << ni::quote(ref($self) . \'::\')
             ->new(%$self{sort grep !/^_/, keys %$self});
}
sub immediate_dependencies {
  my $self = shift;
  $self->declared_dependencies, $self->class;
}
',q'dependencies',[q'ni.module:/lib/quote'],q'name',q'ni::serializable');
ni::module::->new(q'dependencies',[q'ni.slice:ni::boot_module/serializable',q'ni.slice:ni::module/serializable',q'ni.slice:ni::boot_class/serializable',q'ni.slice:ni::class/serializable',q'ni.slice:ni::slice/serializable',q'ni.class:ni::serializable'],q'name',q'/lib/serializable');
ni::module::->new(q'code',q'package ni::printer;
use overload qw/<< print/;
sub new {
  my $class = shift;
  bless {fh => shift}, $class;
}
sub print {shift->{fh}->print(@_, ";\\n")}
ni::context(\'ni.module:/lib/printer\')->serialize(ni::printer->new(\\*STDOUT));
',q'dependencies',[q'ni.module:/lib/serializable',q'ni.module:/lib/quote'],q'name',q'/lib/printer');
