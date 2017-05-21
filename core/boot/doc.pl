# Documentation generator.
# Produces documentation by inspecting data structures. For the most part this is
# delegated.

our %doc;
our %doc_fns;
our %doc_entities;

sub documentation_for($$) {
  my ($type, $thing) = @_;
  $doc_fns{$type}->($thing, $doc{$type}{$thing}, $type);
}
sub default_doc_fn($$) {$_[1]}

sub doc_sections(@) {
  my %sections      = @_;
  my @section_order = map $_[$_ * 2], 0..$#_ >> 1;
  join "\n", map {('', ($_ ? '## ' : '# ') . $section_order[$_],
                   map "\t$_", split /\n/, $sections{$section_order[$_]})}
             0..$#section_order;
}

sub defdocumentable($$;$) {
  my ($name, $global_hash, $fn) = @_;
  $doc{$name}          = {};
  $doc_fns{$name}      = defined $fn ? fn $fn : \&default_doc_fn;
  $doc_entities{$name} = $global_hash;
  ni::eval "sub doc$name(\$\$) {\$ni::doc{'$name'}{\$_[0]} = \$_[1]}";
  ni::eval "sub ${name}_doc(\$) {documentation_for '$name', \$_[0]}";
}
