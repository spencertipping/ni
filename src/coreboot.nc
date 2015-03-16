# ni core library
# This builds on the primitives defined by the bootstrap interpreter to provide
# the usual list manipulation and other stuff we'll use to write macros in
# concatenative code.

# Definition function
# The bootstrap layer doesn't define this; instead we'll need to write it in
# terms of getr and setr.
#
#   (value 'name def) = ()

(getr assoc setr) dup 'def swap eval

# Decisionals
# Derivation:
#
#   c (t) (e) if              =  either t or e
#   c (t) (e) r< !            =  (t) (e) !c
#   (t) (e) !c nth            =  (t) (e) (f)      # where f is either t or e
#   (t) (e) (f) r> drop drop  =  (f)
#   (f) eval                  =  f

(r< ! ! nth r> drop drop eval) 'if def

# List functions
(swap   cons) 'swons   def
(uncons swap) 'unswons def

# Derivation for map:
#   x:xs (f) map            =  x f xs (f) map swons
#     xs (f) swap dup nil?  =  (f) xs 1|0
#
# Inductive case:
#   (f) x:xs    unswons r< swap r>  =  xs x (f)
#   xs x (f)    dup r> eval         =  xs (f) x f
#   xs (f) x f  r> map swons        =  x f xs (f) map swons
#
# Base case:
#   (f) () swap drop  =  ()

(swap dup nil?
  (swap drop)
  (unswons r< swap r> dup r> eval r> map swons)
  if) 'map def

# Derivation for filter:
#   x:xs (f) filter         =  x xs (f) filter swons
#     xs (f) swap dup nil?  =  (f) xs 1|0
#
# Inductive case:
#   (f) x:xs      uncons dup r>  = (f) x xs x
#   (f) x xs x    3 nth eval     = (f) x xs x f
#   (f) x xs x f
#     ((f) x xs r< filter swons)
#     ((f) x xs r> drop filter) if
#
# Base case:
#   (f) () swap drop  =  ()

(swap dup nil?
  (swap drop)
  (uncons dup r> 3 nth eval
    (r< filter swons)
    (r> drop filter)
    if)
  if) 'filter def
