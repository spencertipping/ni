# Block comments: \#[ ... ]
# Word comments: \#' ... '

# NOTE: don't confuse this with \<\#, which is read-and-delete

defmetaoperator comment => q{
  my ($args, $left, $right) = @_;
  return ($left, $right);
};

defshort '/#', pmap q{comment_op $_}, id_text;
