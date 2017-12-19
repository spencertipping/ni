# Data closures.
# Data closures are a way to ship data along with a process, for example over
# hadoop or SSH. The idea is to make your data as portable as ni is.

sub add_closure_key($$) {
  # TODO: use a lib for all data closures
  my ($k, $v) = @_;
  self_append_resource "transient/closure/$k", pack 'u', $v;
}

sub closure_keys()  {grep s/^transient\/closure\///, keys %ni::self}
sub closure_data($) {unpack 'u', $ni::self{"transient/closure/$_[0]"}}

defmetaoperator memory_data_closure => q{
  my ($name, $f) = @{$_[0]};
  my $data;
  my $fh = sni @$f;
  1 while saferead $fh, $data, 8192, length $data;
  close $fh;
  $fh->await;
  add_closure_key $name, $data;
  ();
};

defoperator memory_closure_append => q{sio; print closure_data $_[0]};

BEGIN {defparseralias closure_name => prx '[^][]+'}

defshort '///:', pmap q{memory_closure_append_op $_}, closure_name;
defshort '/::',  pmap q{memory_data_closure_op @$_},
                 pseq pc closure_name, _qfn;
