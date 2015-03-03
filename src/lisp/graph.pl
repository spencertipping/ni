# Continuation graph structure
# Represents code in a form much like CPS, but with degrees of freedom to
# encode partial ordering. Specifically:
#
# (+ (f x) (g y))
# CPS -> (λk (f x
#          (λfx (g y
#            (λgy (+ fx gy k))))))
#
# In our continuation graph structure, we replace the fixed evaluation order
# with a (co*) form that provides parallelism:
#
# (+ (f x) (g y))
# -> (λk (co* (λk1 (f x k1))
#             (λk2 (g y k2))
#             (λ[fx gy]
#               (+ fx gy k))))
#
# Semantically, (co*) returns once all of the sub-continuations are invoked,
# and additionally when any of the sub-continuations is re-invoked. So:
#
# > (co* (λk1 ...)
#        (λk2 ...)
#        (λk3 ...)
#        (λ[v1 v2 v3] (print v1 v2 v3)))
# > (k1 5)              # nothing happens
# > (k2 6)              # nothing happens
# > (k1 7)              # nothing happens
# > (k3 9)              # (print 5 6 9) (print 7 6 9)
# > (k2 4)              # (print 7 4 9)
#
# Non-triggering continuations are required to hold only a weak reference to
# the other continuation queues. That way if k3 is freed before being called,
# k1 and k2's space usage will be constant even if they are called repeatedly
# (which would normally enqueue stuff).
#
# NB: the order of arguments relative to one another is explicitly undefined;
# that is, if we have (k1 4) (k1 5) (k1 6) and (k2 a) (k2 b), the (co*)
# continuation might see [4 a] [5 a] [6 a] or it might see [4 a] [4 b] [5 b],
# etc. The queues of k1 and k2 are mutually unordered.

{

package ni::lisp;

sub fn_node      { ni::lisp::fn->new(@_) }
sub co_node      { ni::lisp::co->new(@_) }
sub if_node      { ni::lisp::if->new(@_) }
sub apply_node   { ni::lisp::apply->new(@_) }

sub global_node  { ni::lisp::global->new(@_) }
sub value_node   { ni::lisp::value->new(@_) }
sub bless_node   { ni::lisp::value->new(@_[1..$#_])->type($_[0]) }
sub literal_node { ni::lisp::value->new->constant(@_) }

}

{

package ni::lisp::node;

sub then {
  my ($self, $x) = @_;
  push @{$$x{wait_for} //= []}, $self;
  $self;
}

sub is_constant { 0 }

our $gensym_id = 0;
sub gensym { '__' . ($_[0] // 'gensym') . '_' . ++$gensym_id }

sub defnodetype {
  my ($t, $ctor, %methods) = @_;
  @{"ni::lisp::${t}::ISA"} = qw/ ni::lisp::node /;
  *{"ni::lisp::${t}::new"} = sub {
    local $_;
    my ($class, @args) = @_;
    bless $ctor->(@args), $class;
  };
  *{"ni::lisp::${t}::$_"} = $methods{$_} for keys %methods;
}

defnodetype 'fn',
sub { +{self_ref => $_[0],
        formal   => $_[1],
        body     => $_[2]} },
{
  compile => sub {
    # Compile into a reified function within the language in question.
    my ($self, $language) = @_;
    my $gensym   = gensym 'fn';
    my $v_gensym = gensym 'x';
    $language->defn($gensym, $v_gensym, $$self{body});
  },
};

defnodetype 'co',
sub { [@_] },
{
  compile => sub {
    # For now, compile in sequential order and return a reified list.
    my ($self, $language) = @_;
    my $gensym = gensym 'co';
    $language->array($gensym, map $_->compile($language), @$self);
  },
};

defnodetype 'if',
sub { +{cond => $_[0], then => $_[1], else => $_[2]} },
{
  compile => sub {
    my ($self, $language) = @_;
    my $cond = $$self{cond}->compile($language);
    $language->choose($cond, $$self{then}, $$self{else});
  },
};

defnodetype 'apply',
sub { +{f => $_[0], args => [@_[1..$#_]]} },
{
  compile => sub {
    my ($self, $language) = @_;
    my $f             = $$self{f};
    my $compiled_args = co->new(@{$$self{args}})->compile($language);
    if ($f->is_constant) {
      $f = $$f{value};
      die "cannot call a non-function $f" unless ref $f eq 'ni::lisp::fn';
      my $inlined_body = $$f{body}->to_graph({''            => $$f{scope},
                                              $$f{formal}   => $compiled_args,
                                              $$f{self_ref} => $f});
      $inlined_body->compile($language);
    } else {
      $language->apply($f->compile($language), $compiled_args);
    }
  },
};

defnodetype 'value',
sub { +{value => $_[1], type => $_[0], constant => 0} },
{
  constant => sub {
    my ($self, $type, $x) = @_;
    $$self{constant} = 1;
    $$self{type}     = $type;
    $$self{value}    = $x;
    $self;
  },

  type => sub {
    my ($self, $t) = @_;
    $$self{type} = $t;
    $self;
  },

  value => sub {
    my ($self) = @_;
    $$self{value};
  },

  is_constant => sub {
    my ($self) = @_;
    $$self{constant};
  },

  compile => sub {
    my ($self, $language) = @_;
    $self->is_constant
      ? $language->typed_constant($$self{type}, $$self{value})
      : $$self{value}->compile($language);
  },
};

}
