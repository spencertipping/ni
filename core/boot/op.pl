# Operator definition.
# Like ni's parser combinators, operators are indirected using names. This
# provides an intermediate representation that can be inspected and serialized.

# Meta operators are applied before any pipeline forking happens, and are at
# liberty to modify anything to their left. (Or anything at all really, but it's
# counterintuitive for them to act rightwards.)

# Note that only toplevel meta operators are evaluated pre-pipeline. Meta
# operators inside lambdas are applied when the lambdas are evaluated (this is a
# feature).

our %operators;
our %meta_operators;

defdocumentable operator => \%operators, q{
  my ($op, $doc) = @_;
  doc_sections
    "OPERATOR $op"   => $doc,
    "IMPLEMENTATION" => $ni::operators{$op};
};

defdocumentable meta_operator => \%meta_operators, q{
  my ($mop, $doc) = @_;
  doc_sections
    "META OPERATOR $mop" => $doc,
    "IMPLEMENTATION" => $ni::meta_operators{$mop};
};

sub defmetaoperator($$) {
  my ($name, $f) = @_;
  die "ni: cannot redefine meta operator $name" if exists $meta_operators{$name};
  warn "ni: overlapping op/meta definition $name" if exists $operators{$name};
  $meta_operators{$name} = fn $f;
  ni::eval "sub ${name}_op(@) {['$name', \@_]}", "defmetaoperator $name";
}

sub meta_operate($$$) {
  my ($operator, $position, $context) = @_;
  my ($op, @args) = @$operator;
  my ($left, $right) = ([$position ? @$context[0..$position-1] : ()],
                        [@$context[$position+1..$#{$context}]]);
  my ($new_left, $new_right) = $meta_operators{$op}->([@args], $left, $right);
  (@{$new_left || $left}, @{$new_right || $right});
}

sub flatten_operators($);
sub flatten_operators($) {
  my ($name) = @{$_[0]};
  return $_[0] unless ref $name;
  map flatten_operators $_, @{$_[0]};
}

sub apply_meta_operators;
sub apply_meta_operators(@) {
  local $_;
  exists $meta_operators{$_[$_]->[0]}
    and return apply_meta_operators meta_operate $_[$_], $_, [@_]
    for 0..$#_;
  @_;
}

# Regular operators.
# Each of these is a filter process that is forked and piped against standard
# input. Operators act independently of one another.

sub defoperator($$) {
  my ($name, $f) = @_;
  die "ni: cannot redefine operator $name" if exists $operators{$name};
  warn "ni: overlapping op/meta definition $name" if exists $meta_operators{$name};
  $operators{$name} = fn $f;
  ni::eval "sub ${name}_op(@) {['$name', \@_]}", "defoperator $name";
  ni::eval "sub ${name}_run(@) {\$ni::operators{$name}->(\@_)}",
           "defoperator $name ($f)";
}

sub operate {
  my ($name, @args) = @_;
  die "ni operate: undefined operator: $name" unless exists $operators{$name};
  $0 = "ni" . json_encode([$name, @args]);
  $operators{$name}->(@args);
}
