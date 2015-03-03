# Continuation graph structure
# Encodes operations over values as a possibly nonlinear series of nodes, each
# node representing a computation over a value.

{

package ni::lisp;

sub fn_node      { ni::lisp::fn_node->new(@_) }
sub co_node      { ni::lisp::co_node->new(@_) }
sub if_node      { ni::lisp::if_node->new(@_) }
sub apply_node   { ni::lisp::apply_node->new(@_) }

sub global_node  { ni::lisp::global_node->new(@_) }
sub value_node   { ni::lisp::value_node->new(@_) }
sub bless_node   { ni::lisp::value_node->new(@_[1..$#_])->type($_[0]) }
sub literal_node { ni::lisp::value_node->new->constant(@_) }

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
sub { +{formal => value_node, body => undef} },
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
    my $cond_gensym = $$self{cond}->compile($language);
    $language->choose($cond, $then, $else);
  },
};

defnodetype 'apply',
sub { +{f => $_[0], args => [@_[1..$#_]]} },
{
  compile => sub {
    my ($self, $language) = @_;
    my $f             = $$self{f};
    my $compiled_args = co(@{$$self{args}})->compile($language);
    if ($f->is_constant) {
      $f = $$f{value};
      die "cannot call a non-function $f" unless ref $f eq 'ni::lisp::fn';
      my $inlined_body  = $$f{body}->to_graph({''            => $$f{scope},
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
