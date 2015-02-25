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
#
# This information can all be tracked using gen() signatures.
#
# Compilation cycle
# All compiled code is run by a genopt instance, which has the following
# high-level workflow:
#
# 1. Someone creates a genopt around an io's ->source_gen() output. The goal of
#    doing this is to execute the io transfer using adaptive JIT.
# 2. The genopt gets the source_gen, which contains just semantic nodes and has
#    no alternative branching. It then calls the stuff in gentypes.pl to get a
#    series of type alternatives.
# 3. genopt then goes through the structure, fixing the type at each choice
#    point and producing an alternative. (Basically we're lifting the
#    alternatives from gentypes space into genopt space.)
# 4. genopt compiles the code with self-profiling and resumable structures. The
#    code is then run until we decide to recompile something.
# 5. The code triggers an escape, which genopt catches. It looks at the
#    profiling data and figures out which alternatives to eliminate, then
#    recompiles the code and resumes. This is a little interesting because the
#    recovery branches may have different types than the ones we end up with,
#    though since the "returns" are all CPS it doesn't actually create any
#    problems.
#
# gentypes tries not to generate very many alternatives in most cases; usually
# it's fairly obvious which representation makes sense.

# genopt-compatible constructors
# Anyone who wants to produce genopt-executable code needs to use these
# constructors rather than writing gens that contain loops or branches.

sub genblock  { ... }
sub genarray  { ... }
sub geniter   { ... }
sub genif     { ... }
sub gensink   { ... }
sub genlambda { ... }
sub genescape { ... }

{

package ni::genopt;

sub new {
  my ($class, $gen) = @_;
  bless { gen => $gen }, $class;
}

}
