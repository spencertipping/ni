# ni core language definitions
(defcps call-cc (fn* [f k] (f k k)))

# It's worth escaping from CPS-land as soon as possible.
