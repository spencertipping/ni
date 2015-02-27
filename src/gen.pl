# Code generator, type solver, and adaptive JIT
# ni executes all IO by emitting pipeline-specific code and running it. This
# file contains a bunch of definitions to make that possible.
#
# At the core of this abstraction is a code-quoting mechanism that lets you
# drop live references into compiled code. This lowers the barrier between a
# regular lambda function and a JIT template.
#
# Slightly above that is a type solver. The type system isn't very
# sophisticated; all we have is a very basic description of how to access the
# inputs to a function. Types are short strings. For example, "L" and "A" refer
# to data stored in $_; "F" refers to data in @_, etc. See below for a full
# list.
#
# Above all of this is the adaptive JIT, which starts with a series of
# alternatives from genalt() calls, profiles each one, and recompiles down to
# whichever one ends up being fastest. Each adaptive branch is fully
# type-compiled, so this is as much about representational optimization as it
# is about general optimization. Adaptive JIT on running code imposes some
# constraints that impact how code must be written; see below for details.

# Type representation details
# ni has a very simple addressing scheme in that there's only one piece of
# addressable data at a time. This means that type solving is basically linear;
# each code join point will have two possibilities:
#
# 1. An appropriately-typed branch exists, so use that monomorphically.
# 2. No appropriate branch exists, so generate a polyrmorphic alternative that
#    will be adaptively optimized.
#
# Possible types are:
#
# - L: we have an unsplit line in $_; basically a straight read from a file.
# - A\d+: data is unsplit in $_, but the first N field boundaries are known and
#   stored in $f0, $f1, ..., $fN-1. If N = 0, then we know all field
#   boundaries.
# - F: we have data split into fields and stored in @_.

# Adaptive JIT details
# This is actually a lot easier than it sounds. We need to preserve two
# invariants:
#
# 1. Any monomorphic alternative must contain only monomorphic alternatives.
# 2. Any code containing a polymorphic alternative must be resumable.
#
# (1) is necessary to prevent the JIT from preferring alternatives with only
# monomorphic descendants (since resumable, profiled, branched code is more
# expensive). (2) makes it possible to suspend from an inner loop, reduce some
# polymorphic branches to monomorphic ones (possibly more than just the ones we
# jumped out of), and pick up where we left off.

our $gensym_id = 0;
sub gensym { '__' . ($_[0] // 'gensym') . '_' . ++$gensym_id . '__' }

sub analyze_gen_template;
sub gen {
  # A piece of type-erased, possibly-resumable code. May or may not contain
  # polymorphic descendants.
  my ($refs, $nonresumable_form, $resumable_form, $completion_detector) = @_;
  my ($nonrefs, $nonsubsts, $nonresumable_code) =
    analyze_gen_template $refs, $nonresumable_form;

  # If all descendants are monomorphic, then all we need is nonresumable code.
  # That way we can bypass the resumable analysis altogether.
  return bless { refs     => $nonrefs,
                 code     => join('', @$nonresumable_code),
                 complete => sub { 1 },
                 poly     => [] }, 'ni::gen::mono'
    unless @$nonsubsts;

DEBUG
  die "must provide a resumable alternative for $nonresumable_code"
    unless defined $resumable_form;
DEBUG_END

  # Need to use a resumable form, so compile that one instead. Keep the
  # nonresumable form on hand so we can switch to it as soon as all of the
  # sub-poly elements have been decided.
  my ($resumable_refs, $resumable_substs, $resumable_code) =
    analyze_gen_template $refs, $resumable_form;

  bless { refs              => $rrefs,
          code              => $resumable_code,
          complete          => $completion_detector,
          nonresumable_refs => $nonrefs,
          nonresumable_code => $nonresumable_code,
          poly              => $resumable_substs }, 'ni::gen::mono';
}

{

package ni::gen::mono;
use overload qw/ "" code /;

sub refs { my ($self) = @_; $$self{refs} }
sub poly { my ($self) = @_; grep $_->poly, @{$$self{poly}} }

sub can_become_mono {
  my ($self) = @_;
  return 0 unless $$self{complete}->();
  $_->can_become_mono || return 0 for @{$$self{poly}};
  return 1;
}

sub code {
  # NB: can't cache this until all descendants are mono; over time any poly
  # elements we have will stabilize into mono forms, so we need to re-join our
  # code snippets accordingly (hence the overload of "").
  my ($self, $assume_not_suspended) = @_;
  return $$self{code} unless ref $$self{code};          # all-mono case

  # If we still contain polymorphic descendants, then recompile our resumable
  # code and keep going.
  return join '', @{$$self{code}} if $self->poly;

  # Ok, now for the interesting cases. Right now we're in a situation where we
  # started off with poly descendants but all of them have become monomorphic.
  # Ordinarily this would be trivial to deal with, but some of the
  # poly-now-mono descendants might have been suspended. Because of this, we
  # need to compile two forms in sequence: the (transitively) resumable version
  # followed by the (transitively) nonresumable version.
  #
  # Here's why this works. Let's suppose we jumped out at the point marked
  # ESCAPE in the following code:
  #
  # while (1) {                         # outer resumable loop
  #   unless ($$fh_retrieved) {         # idempotence-gated side effect
  #     last unless defined($$x = <$fh>);
  #     $$fh_retrieved = 1;
  #   }
  #   BRANCH {                          # chosen poly alternative
  #     unless ($$f_called) {           # idempotence-gated side effect
  #       @$vs = f($$x);                # inner gen, one mono alternative
  #       $$f_called = 1;
  #     }
  #     while ($$i < @$vs) {            # resumable loop
  #       $_ = $$vs[$$i++] . "\n";      # type conversion to L for inner gen
  #       print;                        # innermost mono gen
  #       ESCAPE
  #     }
  #     $$f_called = 0;                 # prepare for next iteration
  #     $$i = 0;                        # resumable loop reset
  #   }
  #   BRANCH {                          # discarded poly alternative
  #     ...
  #   }
  #   $$fh_retrieved = 0;
  # }
  #
  # The continuation will be run automatically because of the way we've gated
  # the various side-effects, so it's fine to re-execute this resumable code.
  # As soon as all of the inner resumable blocks have completed, we should cut
  # over to the nonresumable version; this means we'll emit code like this:
  #
  # until ($gen->can_become_mono) {     # outer resumable loop
  #   unless ($$fh_retrieved) {         # idempotence-gated side effect
  #     last unless defined($$x = <$fh>);
  #     $$fh_retrieved = 1;
  #   }
  #   unless ($$f_called) {             # idempotence-gated side effect
  #     @$vs = f($$x);                  # inner gen, one mono alternative
  #     $$f_called = 1;
  #   }
  #   while ($$i < @$vs) {              # resumable loop
  #     $_ = $$vs[$$i++] . "\n";        # type conversion to L for inner gen
  #     print;                          # innermost mono gen
  #     ESCAPE
  #   }
  #   $$f_called = 0;                   # prepare for next iteration
  #   $$i = 0;                          # resumable loop reset
  #   $$fh_retrieved = 0;
  # }
  # while (<$fh>) {                     # nonresumable outer gen
  #   for (f($_)) {                     # nonresumable inner gen
  #     $_ .= "\n";
  #     print;
  #   }
  # }
  #
  # 
}

}

sub genalt {
  # Polymorphic code indexed by type. Not always compiled down to a polymorphic
  # branch; often the container can match its source type with one of the
  # alternatives here, and that's usually assumed to be faster than an adaptive
  # JIT branch.
  my %alternatives = @_;
  my @branches = sort keys %alternatives;

  bless { alternatives  => \%alternatives,
          branch_types  => [@branches],
          branches      => [map $alternatives{$_}, @branches],
          branch_times  => [map 0, @branches],
          branch_counts => [map 0, @branches],
          decided       => 0 },
        'ni::gen::poly';
}

sub analyze_gen_template {
  # Takes ($refs, $code) and returns ($refs, $substs, $code). The $refs
  # returned are a subset of the originals, since the original contains both
  # value closures and gen substitutions. $substs contains a hash of unstable
  # (i.e. poly) code substitutions.

  my ($refs, $code) = @_;
  my @pieces;
  my $contains_poly = 0;
  my %refs;
  my %gensyms;
  my %subst;

  for (split /(:\<\w+\>|\:\w+)/, $code) {
    if (/^:(\w+)$/) {
      DEBUG
      die "gen template $code used with undefined gensym ref $1"
        unless defined $$refs{$1};
      DEBUG_END
      push @pieces, $gensyms{$1} //= gensym $1;
      $refs{$gensyms{$1}} = $$refs{$1};
    } elsif (/^:\<(\w+)\>$/) {
      DEBUG
      die "gen template $code used with undefined subst ref $1"
        unless defined $$refs{$1};
      DEBUG_END
      my $s = $$refs{$1};
      if (ref $s) {
        my $s_refs = $s->refs;
        $refs{$_} = $$s_refs{$_} for keys %$s_refs;
        push @pieces, $s->poly ? $subst{$1} //= $s : "$s";
      } else {
        push @pieces, $s;
      }
    } else {
      push @pieces, $_;
    }
  }

  (\%refs, \%subst, \@pieces);
}
