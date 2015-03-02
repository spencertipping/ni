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
  push @{$$self{effect_continuations}}, $x;
  $self;
}

sub defnodetype {
  my ($t, $ctor, %methods) = @_;
  @{"ni::lisp::${t}::ISA"} = qw/ ni::lisp::node /;
  *{"ni::lisp::${t}::new"} = sub {
    local $_;
    my ($class, @args) = @_;
    my $self = $ctor->(@args);
    $$self{$_} //= [] for qw/ continuations effect_continuations /;
    bless $self, $class;
  };
  *{"ni::lisp::${t}::$_"} = $methods{$_} for keys %methods;
}

defnodetype 'fn',
sub { +{formal => value_node, body => undef} },
{
};

defnodetype 'co',
sub { +{subgraphs => [@_]} },
{
};

defnodetype 'if',
sub { my ($cond, $then, $else) = @_;
      my $result = {then => $then, else => $else};
      $cond->to($result);
      $result },
{
};

defnodetype 'apply',
sub { my ($args_co) = @_;
      my $result = {};
      $args_co->to($result);
      $result },
{
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
};

}
