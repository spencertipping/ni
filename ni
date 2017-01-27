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
eval($ni::context{'ni.module:/boot'} = <<'_');
sub ni::context {
  my @r = @ni::context{@_};
  return wantarray ? @r : $r[0] unless grep !defined, @r;
  die 'ni::context: failed to resolve '
    . join(', ', grep !defined($ni::context{$_}), @_);
}

package ni::constructable;
sub new {
  my $class = shift;
  my $self = bless {@_}, $class;
  $self->init;
  $self;
}

package ni::named;
use overload qw/"" name/;
sub scheme {(my $s = ref shift) =~ s/::/./g; $s}
sub name {my $self = shift; $self->scheme . ":$$self{name}"}

package ni::transient_identity;
push our @ISA, 'ni::named';
use Scalar::Util qw/weaken/;
sub DESTROY {delete $ni::context{shift->name}}
sub add_to_context {my $self = shift; weaken($ni::context{$self->name} = $self); $self}

package ni::persistent_identity;
push our @ISA, 'ni::named';
sub add_to_context {my $self = shift; $ni::context{$self->name} = $self}

package ni::boot_module;
push our @ISA, qw/ni::constructable ni::persistent_identity/;
sub scheme {'ni.module'}
sub init {
  my $self = shift;
  chomp $$self{code};
  $self->add_to_context;
}
ni::boot_module->new(name => '/boot', code => ni::context 'ni.module:/boot');

package ni::module;
push our @ISA, qw/ni::constructable ni::persistent_identity/;
sub init {my $self = shift; $self->add_to_context; $self->eval}
sub eval {my $self = shift; eval $$self{code}; die "$@ evaluating $$self{name}" if $@}
_
die "$@ evaluating /boot" if $@;
ni::module::->new(q'code',q'sub ni::quote($)       {bless {expr => shift}, \'ni::quotation\'}
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

package ni::quotation;
use Scalar::Util;
use overload qw/"" __str/;
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
',q'dependencies',[q'ni.module:/boot'],q'name',q'/lib/quote');
ni::module::->new(q'code',q'package ni::serializable;
sub dependencies {
  my %ds;
  @ds{my @ds = my @i = shift} = 1;
  push @ds, @i while @i = grep !$ds{$_}++, map $_->immediate_dependencies, @i;
  @ds;
}
sub serialize {
  my $self = shift;
  $_->serialize_self(@_) for reverse $self->dependencies;
  shift;
}
sub serialize_self {
  my ($self, $into) = @_;
  $into << ni::quote(ref($self) . \'::\')->new(%$self{sort keys %$self});
}

package ni::boot_module;
push our @ISA, qw/ni::serializable/;
sub immediate_dependencies {}
sub serialize_self {
  my ($self, $into) = @_;
  $into << join "\\n",
    \'#!/usr/bin/env perl\',
    q{chomp($ni::license = <<\'_\');}, $ni::license, \'_\',
    \'eval($ni::context{\\\'\' . $self->name . \'\\\'} = <<\\\'_\\\');\', $$self{code}, \'_\',
    qq{die "\\$@ evaluating $$self{name}" if \\$@};
}

package ni::module;
push our @ISA, qw/ni::serializable/;
sub immediate_dependencies {ni::context @{shift->{dependencies}}}
',q'dependencies',[q'ni.module:/boot'],q'name',q'/lib/serializable');
ni::module::->new(q'code',q'package ni::printer;
use overload qw/<< print/;
sub new {
  my $class = shift;
  bless {fh => shift}, $class;
}
sub print {shift->{fh}->print(@_, ";\\n")}
ni::context(\'ni.module:/lib/printer\')->serialize(ni::printer->new(\\*STDOUT));
',q'dependencies',[q'ni.module:/lib/serializable',q'ni.module:/lib/quote'],q'name',q'/lib/printer');
