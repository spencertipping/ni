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
BEGIN{eval($ni::boot = <<'_');}
use strict;
use warnings;
no warnings qw/redefine void/;
no strict 'refs';
use Scalar::Util;
chomp $ni::boot;
$ni::self = bless {named => {}}, 'lib/ni';
sub ni(;$) {@_ ? ref($_[0]) ? $_[0] : $ni::self->resolve($_[0]) : $ni::self}
sub ni::eval {eval shift}
*{'lib/ni::def'} = sub {
  my ($self, %kvs) = @_;
  $$self{named}{$_} = $kvs{$_} for keys %kvs;
};
*{'lib/fn::OVERLOAD'} = {};
*{'lib/fn::(bool'} = sub {1};
*{'lib/fn::()'}    = sub {};
*{'lib/fn::(&{}'}  = sub {$_[0]->{fn} ||= $_[0]->compile};
*{'lib/fn::compile'} = sub {
  my $self = shift;
  $$self{fn} = ni::eval "sub{$$self{code}\n}";
  die "ni:lib/fn failed to compile $$self{code}: $@\n" if $@;
  $$self{fn};
};
sub fn($);
_
die "$@ evaluating ni boot code" if $@;

*{'lib/ni::resolve'} = sub {shift->{named}{$_[0]} || die "ni: failed to resolve $_[0]"};
*{'lib/fn::new'} = sub {
  my $self = bless {code => $_[1]}, $_[0];
  $self->compile;
  $self;
};

sub fn($) {'lib/fn'->new(shift)}

# This method call is present here only the first time around; ni rewrites
# itself to make this call at the end of its image, outside of END{} blocks (so
# you can use END{} for normal purposes).
END {ni->run(@ARGV)}
# lib/slice.b
ni->def('ni:lib/slice.b' =>
  bless {
    name => 'lib/slice.b',
    methods => {
      apply => fn q{
        local $_;
        my ($self, $p) = @_;
        $p = $p->package if ref $p;
        return if $$self{applied_to}{$p};
        for (keys %{$$self{methods}}) {
          die "$self: overlapping method $p\::$_" if defined *{"$p\::$_"}{CODE};
        }
        $self->apply_unsafe($p);
      },

      apply_unsafe => fn q{
        local $_;
        my ($self, $p) = @_;
        return if $$self{applied_to}{$p};
        $$self{applied_to}{$p} = 1;
        push @{"$p\::ctors"}, $$self{ctor} if $$self{ctor};
        push @{"$p\::dtors"}, $$self{dtor} if $$self{dtor};
        if (grep /^\(/, keys %{$$self{methods}}) {
          *{"$p\::()"} = sub {};
          *{"$p\::OVERLOAD"} = {};
        }
        *{"$p\::$_"} = \&{$$self{methods}{$_}} for keys %{$$self{methods}};
        $self;
      }
    }
  }, 'lib/slice');

ni('ni:lib/slice.b')->{methods}->{apply_unsafe}
  ->(ni('ni:lib/slice.b'), 'lib/slice');

ni->def('ni:lib/slice_init.b' =>
  bless {
    name => 'lib/slice_init.b',
    methods => {
      instantiate => fn q{
        my $class = shift;
        my $name  = shift;
        my %args  = @_;
        +{ctor       => delete($args{ctor}),
          dtor       => delete($args{dtor}),
          applied_to => delete($args{applied_to}),
          name       => $name,
          methods    => \%args};
      },
    }
  }, 'lib/slice');

ni('ni:lib/slice_init.b')->apply('lib/slice');

ni->def('ni:lib/instantiable.b' =>
  bless {
    name => 'lib/instantiable.b',
    methods => {
      new => fn q{
        local $_;
        my $class = ref $_[0] ? shift->package : shift;
        my $self = bless $class->instantiate(@_), $class;
        $_->($self) for @{ref($self) . "::ctors"};
        $self;
      },
      DESTROY => fn q{
        local $_;
        my $self = shift;
        $_->($self) for @{ref($self) . "::dtors"};
      }
    }
  }, 'lib/slice');

ni('ni:lib/instantiable.b')->apply('lib/slice');

ni->def(
  'ni:lib/named_in_ni.b' =>
    'lib/slice'->new('lib/named_in_ni.b',
      namespace => fn q{'ni'}),
  'ni:lib/named.b' =>
    'lib/slice'->new('lib/named.b',
      ctor => fn q{my $s = shift; ni->def($s->name, $s)},
      name => fn q{$_[0]->namespace . ":" . $_[0]->{name}}));

ni('ni:lib/named.b')->apply('lib/slice');
ni('ni:lib/named_in_ni.b')->apply('lib/slice');

'lib/slice'->new('lib/namespaced.b',
  package => fn q{shift->{name}});

ni('ni:lib/namespaced.b')->apply('lib/slice');

# lib/tag.b
'lib/slice'->new('lib/resolver.b',
  resolve => fn q{ref $_[1] ? $_[1] : ni"ni:$_[1]"});

'lib/slice'->new('lib/tag.b',
  apply => fn q{
    local $_;
    my ($self, $p) = @_;
    $_->apply($p) for @{$$self{slices}};
    $self;
  });

'lib/slice'->new('lib/tag_init.b',
  instantiate => fn q{
    local $_;
    my $class = shift;
    my $name  = shift;
    +{name   => $name,
      slices => [map $class->resolve($_), @_]};
  });

ni('ni:lib/tag.b')->apply('lib/tag');
ni('ni:lib/tag_init.b')->apply('lib/tag');
ni('ni:lib/named.b')->apply('lib/tag');
ni('ni:lib/named_in_ni.b')->apply('lib/tag');
ni('ni:lib/namespaced.b')->apply('lib/tag');
ni('ni:lib/instantiable.b')->apply('lib/tag');
ni('ni:lib/resolver.b')->apply('lib/tag');

# lib/branch.b
'lib/slice'->new('lib/branch.b',
  apply => fn q{
    local $_;
    my ($self, $p) = @_;
    $p = $p->package if ref $p;
    $$self{applied_to}{$p} = 1;
    $_->apply($p) for @{$$self{slices}};
    $self;
  },
  add => fn q{
    local $_;
    my $self = shift;
    my @s = map $self->resolve($_), @_;
    push @{$$self{slices}}, @s;
    for my $p (sort keys %{$$self{applied_to}}) {$_->apply($p) for @s}
    $self;
  });

'lib/slice'->new('lib/branch_init.b',
  instantiate => fn q{
    local $_;
    my $class = shift;
    my $name  = shift;
    +{name       => $name,
      applied_to => {},
      slices     => [map $class->resolve($_), @_]};
  });

'lib/tag'->new('lib/perlbranch.b',
  'lib/branch.b',
  'lib/named.b',
  'lib/named_in_ni.b',
  'lib/namespaced.b',
  'lib/resolver.b');

ni('ni:lib/perlbranch.b')->apply('lib/branch');
ni('ni:lib/branch_init.b')->apply('lib/branch');

'lib/slice'->new('lib/instance.b', class => fn q{ni 'ni:' . ref shift});
# class and metaclass
'lib/slice'->new('lib/class_init.b',
  ctor => fn q{my $s = shift; $s->apply($s->package)},
  instantiate => fn q{
    local $_;
    my ($class, $name, @slices) = @_;
    +{name   => $name,
      slices => [map $class->resolve($_), @slices]};
  });

for (qw/ class metaclass class.c metaclass.c module.c /) {
  ni('ni:lib/perlbranch.b')->apply($_);
  ni('ni:lib/instantiable.b')->apply($_);
  ni('ni:lib/class_init.b')->apply($_);
}

'metaclass.c'->new('metaclass', 'lib/perlbranch.b', 'lib/instantiable.b', 'lib/class_init.b');

ni('ni:metaclass')->new('metaclass.c');
ni('ni:metaclass')->new('object.c');
ni('ni:metaclass')->new('module.c', 'object.c', 'lib/instantiable.b');
ni('ni:module.c')->new('module', 'lib/perlbranch.b', 'lib/class_init.b');

ni('ni:metaclass')->new('class.c', 'module.c');
ni('ni:class.c')->new('class', 'module', 'lib/instantiable.b', 'lib/class_init.b');
ni('ni:metaclass.c')->add('class');

ni('ni:object.c')->add('class');
ni('ni:object.c')->new('object', 'lib/instance.b');

ni('ni:module')->add('object');

ni('ni:class')->add('module');
ni('ni:metaclass')->add('module');

# lib/{slice,tag,branch}
ni('ni:metaclass')->new('lib/behavior.c', 'object.c');
ni('ni:metaclass')->new('lib/slice.c',    'lib/behavior.c');
ni('ni:metaclass')->new('lib/tag.c',      'lib/behavior.c');
ni('ni:metaclass')->new('lib/branch.c',   'lib/behavior.c');

ni('ni:lib/behavior.c')->new('lib/behavior', 'object');
ni("ni:lib/$_.c")->new("lib/$_",
  'lib/behavior', 'lib/named.b',
  "lib/$_.b", "lib/$_\_init.b") for qw/slice branch tag/;

ni('ni:module')->add('lib/behavior');
ni('ni:module.c')->add('lib/behavior.c');

# Definition
ni('ni:lib/branch')->new('lib/definition.b');
ni('ni:lib/branch')->add('lib/definition.b');

ni('ni:module')->add('lib/definition.b');

ni('ni:lib/definition.b')->add(
  ni('ni:lib/slice')->new('lib/definition_def.b',
    def => fn q{shift->add(ni('ni:lib/slice')->new(@_))}));

ni('ni:lib/definition.b')->def('lib/accessor.b',
  ro => fn q{
    my ($self, $slice, @rs) = @_;
    $self->add(ni('ni:lib/slice')->new(
      $slice,
      map +($_ => fn qq{shift->{'$_'}}), @rs));
  },
  rw => fn q{
    my ($self, $slice, @as) = @_;
    $self->add(ni('ni:lib/slice')->new(
      $slice,
      map +($_ => fn qq{\@_ == 2 ? \$_[0]->{'$_'} = \$_[1] : shift->{'$_'}}), @as));
  });

# Subclassing
ni('ni:class')->def('lib/subclass.b',
  child => fn q{
    my ($self, $name, @slices) = @_;
    ni("ni:metaclass")->new("$name.c", $self->class)
    ->new($name, $self, @slices);
  });

# Common behaviors
ni('ni:lib/definition.b')
  ->def('lib/name_as_string.b', '(""' => fn q{shift->name})
  ->def('lib/ref_eq.b',
    '(eq' => fn q{
      ref($_[0]) eq ref($_[1])
        and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1]);
    });

# Data slices
ni('ni:lib/behavior')->child('lib/dataslice')
  ->def('lib/dataslice_init.b',
    instantiate => fn q{
      my $class = shift;
      my $name = shift;
      +{name => $name, data => {@_}};
    })
  ->def('lib/dataslice_apply.b',
    apply => fn q{
      local $_;
      my ($self, $p) = @_;
      $p = $p->package if ref $p;
      return if $$self{applied_to}{$p};
      $$self{applied_to}{$p} = 1;
      *{"$p\::$_"} = $$self{data}{$_} for keys %{$$self{data}};
      $self;
    });

ni('ni:lib/definition.b')->def('lib/definition_defdata.b',
  defdata => fn q{shift->add(ni('ni:lib/dataslice')->new(@_))});

# Static function support
ni('ni:module')->new('ni');
ni('ni:module')->new('main');
ni('ni:object')->child('lib/doc', 'lib/named.b')
  ->def('lib/doc_init.b',
    instantiate => fn q{shift; +{name => shift, doc => []}})

  ->def('lib/doc_namespace.b', namespace => fn q{'ni.doc'})
  ->def('lib/doc_define.b',
    AUTOLOAD => fn q{
      my $self = shift;
      (my $method = ${__PACKAGE__ . "::AUTOLOAD"}) =~ s/^.*:://;
      push @{$$self{doc}}, [$method, @_];
      $self;
    })

  ->def('lib/doc_test.b',
    tests => fn q{
      my $self = shift;
      my @flattened = map @$_, @{$$self{doc}};
      my @tests;
      return () unless @flattened;
      for (0..$#flattened - 1) {
        push @tests, $flattened[$_ + 1]
          if !ref $flattened[$_] && $flattened[$_] eq 'eg';
      }
      @tests;
    },

    eg => fn q{
      my $self = shift;
      push @{$$self{doc}}, [eg => $_] for @_;
      $self;
    });

ni('ni:lib/behavior')->def('lib/documentable.b',
  doc => fn q{
    my $self = shift;
    (my $name = $self->name) =~ s/^[^:]*://;
    return ni("ni.doc:$name") if ni->can('exists') && ni->exists("ni.doc:$name");
    ni('ni:lib/doc')->new($name);
  });
ni('ni:lib/slice')->new('lib/fn_init.b',
  instantiate => fn q{
    my $class = shift;
    my $code  = pop;
    my $proto = @_ && $_[-1] =~ /^\(/ ? pop : '';
    +{code        => $code,
      proto       => $proto,
      annotations => [@_]};
  },
  compile => fn q{
    local $@;
    my $self = shift;
    $$self{proto} ||= '';
    $$self{fn} = ni::eval "sub $$self{proto} {$$self{code}\n}";
    die "ni:lib/fn: failed to compile $$self{code}: $@" if $@;
    $$self{fn};
  },
  ctor => fn q{shift->compile});

delete @{'lib/fn::'}{qw/new compile/};
ni('ni:object')->child('lib/fn', 'lib/instantiable.b', 'lib/fn_init.b');

ni('ni:lib/fn')
  ->ro('lib/fn_ro.b', qw/code annotations fn/)
  ->def('lib/fn_ops.b',
    '(""' => fn q{shift->{code}},
    '(eq' => fn q{
      ref($_[0]) eq ref($_[1])
        and Scalar::Util::refaddr($_[0]) == Scalar::Util::refaddr($_[1])});

delete ${main::}{fn};

ni('ni:main')->def('lib/static_fn.b',
  fn => ni('ni:lib/fn')->new('($)',  q{ni('ni:lib/fn')->new(@_)}),
  fp => ni('ni:lib/fn')->new('($$)', q{ni('ni:lib/fn')->new(@_)}));
{
  my %json_unescapes = (
    "\\" => "\\", "/" => "/", "\"" => "\"", b => "\b", n => "\n", r => "\r",
    t => "\t");
  my %json_escapes = map +($json_unescapes{$_} => $_), keys %json_unescapes;
  ni('ni:ni')
    ->defdata('lib/json_data.b',
      json_unescapes => \%json_unescapes,
      json_escapes   => \%json_escapes);
}

ni('ni:ni')
  ->def('lib/json.b',
    json_unescape_one =>
      fp('($)', q{$ni::json_unescapes{$_[0]} || chr hex substr $_[0], 1}),

    json_unescape => fp('($)', q{
      my $x = substr $_[0], 1, -1;
      $x =~ s/\\\\(["\\\\\/bfnrt]|u[0-9a-fA-F]{4})/ni::json_unescape_one($1)/eg;
      $x;
    }),

    json_decode => fp('($)', q#
      local $_;
      my @v = [];
      for ($_[0] =~ /[][{}]|true|false|null|"(?:[^"\\\\]+|\\\\.)*"|[-+eE\d.]+/g) {
        if (/^[[{]$/) {
          push @v, [];
        } elsif (/^\]$/) {
          die "json_decode $_[0]: too many closing brackets" if @v < 2;
          push @{$v[-2]}, $v[-1];
          pop @v;
        } elsif (/^\}$/) {
          die "json_decode $_[0]: too many closing brackets" if @v < 2;
          push @{$v[-2]}, {@{$v[-1]}};
          pop @v;
        } else {
          push @{$v[-1]}, /^"/      ? json_unescape $_
                        : /^true$/  ? 1
                        : /^false$/ ? 0
                        : /^null$/  ? undef
                        :             0 + $_;
        }
      }
      my $r = pop @v;
      die "json_decode $_[0]: not enough closing brackets" if @v;
      wantarray ? @$r : $$r[0];
    #),

    json_escape => fp('($)', q{
      (my $x = $_[0]) =~ s/([\b\f\n\r\t"\\\\])/"\\\\" . $ni::json_escapes{$1}/eg;
      "\"$x\"";
    }),

    json_encode => fp('($)', q{
      local $_;
      my ($v) = @_;
      return "[" . join(',', map ni::json_encode($_), @$v) . "]" if 'ARRAY' eq ref $v;
      return "{" . join(',', map ni::json_escape($_) . ":" . ni::json_encode($$v{$_}),
                                 sort keys %$v) . "}" if 'HASH' eq ref $v;
      Scalar::Util::looks_like_number $v
        ? $v
        : defined $v ? ni::json_escape($v) : 'null';
    }));
ni('ni:object')->child('lib/test_value')
  ->def('lib/test_value_init.b', instantiate => fn q{\$_[1]})
  ->def('lib/test_value_eq.b',
    diff => fn q{
      my ($self, $rhs) = @_;
      my $class = $self->class;
      my $lhs = $$self;
      my $rl = ref $lhs;
      my $rr = ref $rhs;
      my $realtype = Scalar::Util::reftype($lhs) || "";
      return {type_difference => [$rl, $rr]} unless $rl eq $rr;
      if ($realtype eq 'HASH') {
        my @left_only  = grep !exists $$rhs{$_}, keys %$lhs;
        my @right_only = grep !exists $$lhs{$_}, keys %$rhs;
        return {hash_key_mismatch => 1,
                object_type       => $rl,
                left_only         => \@left_only,
                right_only        => \@right_only}
          if @left_only || @right_only;
        my %diff;
        $diff{$_} = $class->new($$lhs{$_})->diff($$rhs{$_})
          for keys %$lhs;
        delete @diff{grep !defined($diff{$_}), keys %diff};
        return {hash_value_mismatch => 1,
                object_type         => $rl,
                diffs               => \%diff} if keys %diff;
      } elsif ($realtype eq 'ARRAY') {
        my $n_diff = @$lhs - @$rhs;
        return {array_length_mismatch => $n_diff} if $n_diff;
        my %diff;
        $diff{$_} = $class->new($$lhs[$_])->diff($$rhs[$_])
          for 0..$#{$lhs};
        delete @diff{grep !defined($diff{$_}), keys %diff};
        return {array_value_mismatch => 1,
                object_type          => $rl,
                diffs                => \%diff} if keys %diff;
      } elsif ($realtype eq 'SCALAR') {
        return $class->new($$lhs)->diff($$rhs);
      } elsif (!$rl) {
        return {scalar_difference => [$lhs, $rhs]} unless $lhs eq $rhs;
      }
      return undef;
    },

    '(==' => fn q{
      my ($self, $rhs) = @_;
      my $diff = $self->diff($rhs);
      die $self->class->new($diff) if defined $diff;
      1;
    })

  ->def('lib/test_value_str.b',
    '(""' => fn q{ni::json_encode ${$_[0]}});

ni('ni:main')->def('lib/global_static_test.b',
  now => fp('($)', q{ni('ni:lib/test_value')->new(shift)}));
# TODO
# Extend the serialization protocol by making a "running context" that is aware
# of sessions and can be used to implement RMI.

ni('ni:object')->child('lib/image')
  ->def('lib/image_init.b',
    instantiate => fn q{
      my $class = shift;
      my %args  = (
        include_shebang => 1,
        include_license => 1,
        include_boot    => 1,
        include_classes => 1,
        include_run     => 1,
        local_vars      => 0,
        use_newlines    => 0,
        @_);

      +{include_shebang => $args{include_shebang},
        include_license => $args{include_license},
        include_boot    => $args{include_boot},
        include_classes => $args{include_classes},
        include_run     => $args{include_run},
        local_vars      => $args{local_vars},
        use_newlines    => $args{use_newlines},

        gensym_n     => 0,
        circular     => [],
        definitions  => {},
        objects      => {},
        side_effects => [],
        finalizers   => [],
        visited      => {},
        ordering     => []};
    })

  ->def('lib/image_quoting.b',
    gensym => fn q{
      my $n = shift->{gensym_n}++;
      my $s = '$' .
        substr "cdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
               $n % 50, 1;
      $n = int $n / 50;
      while ($n) {
        $s .= substr "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
                     $n % 63, 1;
        $n = int $n / 63;
      }
      $s;
    },

    circular_links => fn q{
      local $_;
      my $self = shift;
      map "\$$$self{visited}{$$_[0]}$$_[1]=$$self{visited}{$$_[2]};",
          @{$$self{circular}};
    },

    reconstruction => fn q{
      my $self = shift;
      (@{$$self{definitions}}{@{$$self{ordering}}},
       $self->circular_links,
       @{$$self{side_effects}},
       @{$$self{finalizers}});
    },

    write => fn q{
      local $_;
      my ($self, $fh) = @_;
      $fh->print($_) for
        ($$self{include_shebang} ? ("#!/usr/bin/env perl\n") : ()),
        ($$self{include_license} ? ("chomp(\$ni::license=<<'_');\n", $ni::license, "\n_\n") : ()),
        ($$self{include_boot}    ? ("BEGIN{eval(\$ni::boot=<<'_')}\n", $ni::boot, "\n_\n") : ()),
        ($$self{use_newlines}    ? map("$_\n", $self->reconstruction) : $self->reconstruction),
        ($$self{include_run}     ? ("ni->run(\@ARGV);", "\n__DATA__\n") : ());
    },

    address => fn q{
      return 'undef' unless defined $_[1];
      return "id:$_[1]" if !ref $_[1] && length $_[1] < 16;
      "addr:" . Scalar::Util::refaddr(ref $_[1] ? $_[1] : \$_[1]);
    },

    quote => fn q{
      my $self = shift;
      return $self->quote_scalar($_[0])
        if !ref $_[0] && Scalar::Util::looks_like_number($_[0]);
      my $a = $self->address($_[0]);
      $$self{objects}{$a} = \$_[0];
      my $v = $$self{visited}{$a};
      return ref $v ? 'undef' : $v if defined $v;
      $$self{visited}{$a} = \'undef';
      $self->allocate_gensym($_[0], $self->quote_value($_[0]));
    },

    allocate_gensym => fn q{
      my $self = shift;
      my $a = $self->address(shift);
      return $$self{visited}{$a} if $_[0] =~ /^\$\w+$/;
      my $g = $$self{visited}{$a} = $self->gensym;
      $$self{definitions}{$g} = $$self{local_vars} ? "my$g=$_[0];" : "$g=$_[0];";
      push @{$$self{ordering}}, $g;
      $g;
    },

    # TODO: replace this with more structured alternatives
    side_effect      => fn q{push    @{${$_[0]}{side_effects}}, $_[1]; $_[0]},
    boot_side_effect => fn q{unshift @{${$_[0]}{side_effects}}, $_[1]; $_[0]},
    finalizer        => fn q{push    @{${$_[0]}{finalizers}},   $_[1]; $_[0]},

    quote_value => fn q{
      my $self = shift;
      return $self->quote_scalar($_[0]) unless ref $_[0];
      return $self->quote_array($_[0])  if 'ARRAY' eq ref $_[0];
      return $self->quote_hash($_[0])   if 'HASH'  eq ref $_[0];
      die "cannot serialize $_[0]"      if 'CODE'  eq ref $_[0];
      $self->quote_object($_[0]);
    },

    is_circular => fn q{
      my $self = shift;
      ref $$self{visited}{$self->address($_[0])};
    },

    quote_hash => fn q{
      local $_;
      my ($self, $v) = @_;
      my $a = $self->address($v);
      my @ks = sort keys %$v;
      my @qs;
      for my $k (@ks) {
        push @{$$self{circular}}, [$a, "{" . $self->quote($k) . "}",
                                       $self->address($$v{$k})]
        if $self->is_circular($$v{$k});
        push @qs, $self->quote($k) . "," . $self->quote($$v{$k});
      }
      '{' . join(",", @qs) . '}';
    },

    quote_array => fn q{
      local $_;
      my ($self, $v) = @_;
      my $a = $self->address($v);
      $self->is_circular($$v[$_])
        && push @{$$self{circular}}, [$a, "[$_]", $self->address($$v[$_])]
      for 0..$#{$v};
      '[' . join(',', map $self->quote($_), @$v) . ']';
    },

    quote_scalar => fn q{
      my $v = $_[1];
      return 'undef' unless defined $v;
      return $v if Scalar::Util::looks_like_number $v;
      $v =~ s/([\\\\'])/\\\\$1/g;
      "q'$v'";
    },

    quote_blessed => fn q{
      my ($self, $x, $r) = @_;
      $r ||= ref $x;
      $self->quote_class($r);
      my $t = Scalar::Util::reftype $x;
      my $quoted = $t eq 'HASH' ? $self->quote_hash($x) : $self->quote_array($x);
      "bless($quoted," . $self->quote($r) . ")";
    },

    quote_class => fn q{
      my ($self, $class) = @_;
      $self->quote(ni"ni:$class") if $$self{include_classes} && ni->exists("ni:$class");
    },

    quote_object => fn q{
      my $self = shift;
      my $q = $self->allocate_gensym($_[0],
        $_[0]->can('serialize') ? $_[0]->serialize($self) : $self->quote_blessed(@_));
      $self->finalizer("&\$_($q)for\@" . $self->quote(ref($_[0]) . "::ctors") . ";");
      $q;
    });

ni('ni:lib/fn')->def('lib/fn_serialize.b',
  serialize => fn q{
    local $_;
    my ($self, $quote) = @_;
    $quote->quote_class(ref $self);

    (my $code = $$self{code}) =~ s/^\s*\n|\s*$//g;
    my @lines = split /\n/, $code;
    my $spaces = length $code;
    for (@lines) {
      $spaces = length $1 if /^([ \t]*)\S/ && length $1 < $spaces;
    }
    $spaces = ' ' x $spaces;
    s/^$spaces// for @lines;

    my %state = %$self;
    delete $state{fn};
    $state{code} = join "\n", @lines;
    $quote->quote_blessed(\%state, ref $self);
  });

ni('ni:lib/slice')->def('lib/slice_serialize.b',
  serialize => fn q{
    local $_;
    my ($self, $quote) = @_;
    my $name = $self->name;
    $quote->quote_class(ref $self);

    if (defined $name and $name eq 'ni:lib/slice.b') {
      my %methods;
      my @ks = sort keys %{$$self{methods}};
      @methods{@ks} = map $quote->quote($_), @{$$self{methods}}{@ks};
      for my $p (sort keys %{$$self{applied_to}}) {
        $quote->boot_side_effect(
          '*' . $quote->quote("$p\::$_") . "=\\\\\&$methods{$_};")
          for @ks;
      }
    }

    my $g = $quote->allocate_gensym($self,
      $quote->quote_blessed({%$self, applied_to => {}}, ref $self));
    $quote->side_effect("$g\->apply_unsafe(" . $quote->quote($_) . ");")
      for sort keys %{$$self{applied_to}};
    $g;
  });
ni('ni:object')->child('lib/ni')
  ->def('lib/ni_self.b',
    is_mutable => fn q{$0 ne "-" && -w $0},
    modify => fn q{
      my ($self, $fn) = @_;
      # TODO: replace all of this with a generalized "atomic-update" function
      # against UNIX files.
      die "ni: cannot modify immutable instance" unless $self->is_mutable;
      my (undef, undef, $mode) = stat $0;
      my $temp = map chr 97 + rand(26), 1..16;
      my @r = split /\//, $0;
      $r[-1] =~ s/^/./;
      $r[-1] =~ s/$/.$temp/;
      my $r = join '/', @r;
      open my $w, '>', $r or die "ni: failed to create staging file: $!";
      chmod $mode, $r or die "ni: failed to chmod $r to $mode: $!";
      &$fn($w);
      close $w;
      rename $r, $0 or die "ni: failed to rename: $!";
    })

  ->def('lib/ni_image.b',
    exists => fn q{exists $_[0]->{named}{$_[1]}},
    quoted => fn q{
      my $self = shift;
      my $q = ni('ni:lib/image')->new(@_);
      my $gs = $q->quote($self);
      $q->side_effect("\$ni::self=$gs;");
      $q;
    })

  ->def('lib/ni_main.b',
    run => fn q{
      my $self = shift;
      shift, exit $self->$1(@_) if $_[0] =~ /^(--.*)$/ && $self->can($1);
      exit $self->default(@_);
    },

    '--internal/eval' => fn q{
      my $self = shift;
      for (@_) {
        my $r = ni::eval($_);
        print $@ ? "ERROR $@\n" : "$r\n";
      }
      0;
    },

    '--internal/+=' => fn q{
      my $self = shift;
      for (@_) {
        my $r = do $_;
        die "ni: failed to parse $_: $@" if $@;
        die "ni: failed to execute $_: $!" unless defined $r;
        die "ni: failed to run $_: $!" unless $r;
      }
      my $q = $self->quoted(use_newlines => 1);
      $self->modify(sub {$q->write(shift)});
      0;
    },

    '--internal/test' => fn q{
      my $self = shift;
      my @tests = map ni($_)->tests, grep /^ni\.doc:/, keys %{$$self{named}};
      my $fails = 0;
      print scalar(@tests) . " test(s)\n";
      my %names = %{ni->{named}};
      for my $t (@tests) {
        %{ni->{named}} = %names;
        my $r = eval {&$t};
        if ($@) {
          ++$fails;
          print "FAIL: $@ in $t\n";
        } elsif (!$r) {
          ++$fails;
          print "FAIL: $r\n";
        }
      }
      my $passed = @tests - $fails;
      print "$passed test(s) passed\n";
      !!$fails;
    },

    '--internal/image' => fn q{
      shift->quoted(use_newlines => 1)->write(\*STDOUT);
      0;
    });

ni('ni:lib/slice')->new('lib/ni_resolver.b',
  resolver_for => fn q{
    my $self = shift;
    ${$$self{resolvers}}{$_[0]} = $_[1];
    $self;
  },
  resolve => fn q{
    my $self = shift;
    return $$self{named}{$_[0]} if exists $$self{named}{$_[0]};
    return $$self{resolvers}{$1}->($_[0]) if
      $_[0] =~ /^([^:]+):/ and exists $$self{resolvers}{$1};
    die "ni:self failed to resolve $_[0]";
  });

{
  my $ni = ni('ni:lib/ni');
  my $r  = ni('ni:lib/ni_resolver.b');
  delete ${'lib/ni::'}{resolve};
  $ni->add($r);
}
