# Operator->operator functions.
# This provides a mechanism for ni to implement aliases and other shorthands.
# Internally we do this by defining a "rewrite parser" that modifies the unparsed
# elements in front of it. Rewriting is namespaced: if you define a lambda in the
# root context, it won't apply in other contexts.

# Here's an example of a lambda that looks for manpages matching the given
# filename pattern:

# | defn ['/manpages', 'pattern'],
#        qw[e[find /usr/share/man -name $pattern*] \<];

# After this definition, ni will perform substitutions like the following:

# | $ ni manpages ls
#   # ni e[find /usr/share/man -name ls*] \<

# Other types of definitions.
# A parameter like 'pattern' will just consume a single unparsed argument, but
# sometimes you want to apply parsing structure to things. You can do that by
# type-tagging the parameters:

# | defexpander ['/manpages-matching', condition => plcode],
#               qw[e[find /usr/share/man -type f] rp$condition];

sub evaluate_fn_expansion(\%@) {
  local $_;
  my ($args, @expansion) = @_;
  return &{$expansion[0]}(%$args) if ref $expansion[0];

  return @expansion unless keys %$args;

  my @result;
  my $scalar_args = join '|', map qr/\$\Q$_\E/, keys %$args;
  $scalar_args = qr/$scalar_args/;
  my $array_args = join '|', map qr/@\Q$_\E/, keys %$args;
  $array_args = qr/$array_args/;

  for (@expansion) {
    if (/^($array_args)$/) {
      push @result, @$args{substr $1, 1};
    } else {
      my @pieces = split /($scalar_args)/;
      $_ & 1 and $pieces[$_] = $$args{substr $pieces[$_], 1}
        for 0..$#pieces;
      push @result, join '', @pieces;
    }
  }
  @result;
}

BEGIN {
  defparser fn_expander => '$$$$',
    q{
      my ($self, @xs) = @_;
      my (undef, $context, $formals, $positions, $expansion) = @$self;
      my ($parsed_formals, @rest) = parse pn(1, popt pempty, $formals), @xs;
      return () unless defined $parsed_formals;

      my %args;
      $args{$_} = $$parsed_formals[$$positions{$_}] for keys %$positions;
      parse parser "$context/op",
            evaluate_fn_expansion(%args, @$expansion), @rest;
    };
}

sub defexpander($@) {
  my ($fn, @expansion) = @_;
  my ($short_spec, @args) = ref $fn ? @$fn : ($fn);
  my ($context, $name) = split /\//, $short_spec, 2;

  my @arg_parsers;
  my %arg_positions;
  if (@args > 1 && ref $args[1]) {
    for (my $i = 0; $i < @args; $i += 2) {
      my ($k, $v) = @args[$i, $i + 1];
      $arg_positions{$k} = $i >> 1;
      push @arg_parsers, $v;
    }
  } elsif (@args) {
    $arg_positions{@args[0..$#args]} = 0..$#args;
    @arg_parsers = map prc '.*', @args;
  }

  defshort $short_spec,
    fn_expander $context, pseq(map pc $_, @arg_parsers),
                          \%arg_positions,
                          \@expansion;
}
