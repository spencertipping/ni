# Parser combinators
# Just like in any other language, but uses eval() to compile new functions.
# Also, parse states are a combination of positional argument index and
# substring position within that argument; e.g.
#
#   $0 $1      $2
#   ni --group -m'r a + b'
#        ^
#        |
#        parse state = 1, 2, ?          <- parse states can return values
#
# Usual calling convention: result is returned indirectly through $1. The
# 'arrow' macro condenses this by allocating temporary variables and chaining
# things for you.

:
