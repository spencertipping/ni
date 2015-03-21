# Bootstrap ni-lisp to concatenative macroexpander-ish thing
#
# Backstory here:
# ni-lisp is compiled into concatenative form by a continuation transformer
# that is itself a concatenative function. In that sense it's self-hosting; the
# continuation transformer is a defined and quotable quantity that can then be
# translated into any backend-supported language. This provides the ability to
# launch a repl from any backend.
#
# The problem is that writing one of these things in concatenative form is
# miserable, so I'm cheating a little and writing the
# applicative-to-concatenative compiler twice: once here in Perl and later in
# ni-lisp. That way I can treat concatenative as a purely intermediate
# representation.
