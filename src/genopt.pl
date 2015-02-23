# Profile-guided optimization
# Sometimes we'll be in a situation where we can't (or I'm too lazy to) tell at
# compile-time which strategy will be fastest. In these cases we can emit
# multiple alternatives, dispatch randomly, and measure empirically,
# recompiling when we reach a conclusion.
#
# The obvious problem with this approach is how we resume an interrupted
# computation. It turns out, though, that because we maintain all of the code
# references it isn't particularly difficult. We just need a few contrivances:
#
# 1. When compiling a loop over an array, we need to save the array and index.
# 2. When resuming within new code, we need a "repair" branch to finish any
#    incomplete loops.
#
# This is explained further below.

use constant GENOPT_REFINE_STDDEV => 3;
use constant GENOPT_REFINE_MIN_MS => 100;
use constant GENOPT_REFINE_MIN_N  => 5;
use constant UNREACHABLE          => gen('unreachable',
                                         {},
                                         q{ die 'unreachable'; });

use List::Util qw/max min/;

sub profiled_element {
  my ($body) = @_;
  my $counters = [0, 0.0];
  gen('profiled', {counters => $counters, body => $body},
    q{ $%:counters[0] = -($%:counters[0] + 1);
       $%:counters[1] -= time;
       %@body
       $%:counters[1] += time;
       $%:counters[0] = -$%:counters[0]; });
}

# Resumable control flow structures
# Computation is resumed in two stages. First we finish out any loops that were
# happening when we exited. This code is different because a resumable loop is
# slower than a non-resumable loop:
#
#   while ($i < @stuff) { ... }    slower than    for (@stuff) { ... }
#
# We want to capitalize on better performance as soon as we can, so rather than
# emitting code that resumes and then reuses that loop, e.g.:
#
#   $fh_ = $stashed_fh_;
#   $i = 20;
#   @i_array = @$stashed_i_array;
#   goto resume_while_loop;
#   while (defined($fh_ = <$fh>)) {
#     @i_array = ...;
#     resume_while_loop:
#     while ($i < @i_array) {
#       ...
#     }
#   }
#
# we instead want the outer loop to use the non-resumable fast version on the
# next iteration. So what we actually emit is this:
#
#   $fh_ = $stashed_fh_;
#   $i = 20;
#   @i_array = @$stashed_i_array;
#   while ($i < @i_array) {             # slow i_array loop
#     ...
#   }
#   while (defined($fh_ = <$fh>)) {
#     @i_array = ...;
#     for (@i_array) {                  # fast, non-resumable loop
#       ...
#     }
#   }

sub nth_option;
sub should_refine;
sub refinement_signal;
sub alternatives {
  return $_[0] if @_ <= 1;

  # Generate the selector structure, which should prefer each alternative
  # equally but produce random decisions. Randomness matters because it
  # guarantees expected unbiased sampling wrt data elements.
  my @options   = @_;
  my @multiples = map $_ / @options, 1..@options;
  my $r = \0;           # NB: must be a reference since gensyms differ

  my $decision = UNREACHABLE;
  for (my $i = $#options; $i >= 0; --$i) {
    $decision = gen("alternative_branch:$i", {r      => $r,
                                              m      => $multiples[$i],
                                              option => $options[$i],
                                              next   => $decision},
                  q{ if ($%:r < %:m) {
                       %@option
                     } else {
                       %@next
                     } });
  }

  my $counters       = [map $$_{refs}{counters}, @options];
  my $alternative_id = ni::gen::gensym 'alternative';
  my $alternative    =
    gen($alternative_id, {r               => $r,
                          counters        => $counters,
                          refinement_data => 0,
                          decision        => $decision},
      q{ # state gensym: %:refinement_data (NB: don't remove this comment)
         $%:r = rand();
         %@decision
         if (ni::should_refine(%:counters)) {
           %@signal_refinement
         } });

  $alternative % {signal_refinement => signal_refinement $alternative};
}

sub nth_option {
  my ($gen, $n) = @_;
  die "must be an alternative branch"
    unless $$gen{sig} =~ /^alternative_branch:(\d+)/;

  $gen = $$gen{inserted_code}{next} while $n--;
  $$gen{option};
}

sub should_refine {
  # TODO: redo this logic completely. We should be considering expected gain
  # per refinement cost, which we can estimate by timing the initial
  # compilation.

  # The time taken by each branch should follow an exponential distribution, so
  # the goal is to look for cases where we're unlikely to have gotten it wrong.
  # In this case we want a 1% error rate; 99% of refinements should result in
  # us choosing the long-run optimal branch. 3 standard deviations of a normal
  # distribution can be used to approximate the exponential confidence
  # interval, which should give us well under 1% error.
  #
  # Note that we refine only if each alternative has used at least 100ms;
  # otherwise the effort isn't really justifiable.

  my ($counters) = @_;
  $$_[1] * 1000 >= GENOPT_REFINE_MIN_MS or return 0 for @$counters;
  $$_[0]        >= GENOPT_REFINE_MIN_N  or return 0 for @$counters;

  # Ok, we probably have enough to work with. The goal is to identify a case
  # where we stand to gain significant performance by eliminating branches. If
  # we have several high-performance options, it probably doesn't matter much
  # which one we take.
  my @ps;
  my @ls;
  my @us;

  for (@$counters) {
    my ($n, $t) = @$_;
    my $adjust = GENOPT_REFINE_STDDEV / sqrt $n;
    push @ps, my $p = $t / $n;
    push @ls, $p * (1 - $adjust);
    push @us, $p * (1 + $adjust);
  }

  # Refine if we have a decisive preference. (TODO: weigh the implications of
  # having a single refinement vs progressive ones in cases of fast-branch
  # uncertainty.)
  min(@us) > max(@ls);
}

sub signal_refinement {
  my ($alternative) = @_;
  gen('refinement_signal', {alternative => $alternative},
    q{ %@save_iterator_state
       %@rewrite_alternative
       return [%@alternative_key, %:alternative] });
}
