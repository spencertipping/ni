# ni core language definitions
(defcps_ 'id_'     (fn* [x] x) (fn* [x] x))
(defcps_ 'call-cc' (fn* [f k] (f k k)) id_)

# It's worth escaping from CPS-land as soon as possible, so let's define some
# macros that do that for us.
(defmacrocps_ 'fn_'
  (fn* [args body k]
    (co* (fn* [k1] (str-sym 'fn*' k1))
         (fn* [k2] (str-sym 'x'   k2))
         (fn* [fnsym xsym]
           (co* (fn* [k1] (list fnsym args body k1))
                (fn* [k2] (list fnsym (to-array (list xsym)) xsym k2))
                (fn* [fn-form k-form]
                  (cps-convert fn-form k-form k))))))
  id_)

(fn_ [x] x)
