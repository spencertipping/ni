# Perl compiler backend
{

package ni::lisp::perl;

sub new {
  # Creates a new code context for Perl, which in our case stores global
  # definitions and function-constructor stuff.
  my ($class, $context) = @_;
  bless {globals => {}, context => $context}, $class;
}

sub defn {
  my ($self, $self_name, $formal_name, $body) = @_;
  
}

}
