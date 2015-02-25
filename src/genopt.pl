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
# Structures in this file are related to types; see gentypes.pl for a
# discussion of what we're doing here.

# TODO
