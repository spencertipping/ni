# Refactor notes
## o47
Right now we've got a heavy metaclass system whose instances serialize
themselves as data structures (which side-effect into defining the Perl
namespace methods required to make it all work). It's not the worst system, but
it's not exactly what we need either. It sends too much state to remotes whose
only purpose would be RMI.

It's simpler, faster, and better to use a different strategy. Metaprogramming
is fine for a bootstrap layer, but the goal shouldn't be to build up in-memory
object state. Instead, we need to generate pieces of code that can be combined
with `pack()` into meta-erased remotes. No need to send things like
documentation, which means we need a way to track piece/piece dependencies. If
we do this right, we can also MD5 the pieces up front and end up with a
constructive history model -- so remotes can be updated in place when new
metaprogramming happens, and history is garbage-collected after being applied
but remotes are still aware of their revision level. (Similar to git, but
without storing the commit history.)


