# ni core language definitions
(defcps call-cc (fn* [f k] (f k k)))

# It's worth escaping from CPS-land as soon as possible, so let's define some
# macros that do that for us.
(defmacrocps fn_
  (fn* [args body k]
    (k (cps-convert (list (str-sym "fn*") args body)
                    (list (str-sym "fn*") [(str-sym "x")] (str-sym "x"))))))

(print (fn_ [x] (print x)) (fn* [x] x))
