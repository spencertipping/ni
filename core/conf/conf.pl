# Configuration variables.
# These can be specified as environment vars or overridden locally for specific
# operations.

our %conf_variables;
our %conf_defaults;

sub conf($) {
  die "ni: nonexistent configuration variable $_[0] (ni //ni/conf to list)"
    unless exists $conf_variables{$_[0]};
  dor $conf_variables{$_[0]}->(), $conf_defaults{$_[0]};
}

sub conf_set($$) {
  die "ni: nonexistent configuration variable $_[0] (ni //ni/conf to list)"
    unless exists $conf_variables{$_[0]};
  $conf_variables{$_[0]}->($_[1]);
}

defoperator conf_get => q{
  my ($name) = @_;
  sio();
  print conf $name, "\n";
};

sub defconf($$) {
  $conf_variables{$_[0]} = fn $_[1];
  defshort '/$' . $_[0], pmap qq{conf_get_op '$_[0]'}, pnone;
}

sub conf_env {
  my ($name, $v) = @_;
  $ENV{$name} = $v if @_ == 2;
  $ENV{$name};
}

sub defconfenv($$$) {
  my ($name, $env, $v) = @_;
  defconf $name, qq{conf_env '$env', \@_};
  $conf_defaults{$name} = $v;
}

defoperator configure => q{
  my ($vars, $f) = @_;
  conf_set $_, $$vars{$_} for keys %$vars;
  conf_set monitor => 0 unless exists $$vars{monitor};
  &$ni::main_operator(flatten_operators $f);
};

BEGIN {defparseralias config_map_key   => prx '[^=]+';
       defparseralias config_map_value => prc '.*[^}]+|'}
BEGIN {defparseralias config_map_kv    => pn [0, 2], config_map_key, pstr '=',
                                                     config_map_value}
BEGIN {defparseralias config_option_map
         => pmap q{my %h; $h{$$_[0]} = $$_[1] for @{$_[0]}; \%h},
            pn 0, prep(config_map_kv), prc '}'}

defshort '/^{', pmap q{configure_op @$_}, pseq config_option_map, _qfn;
