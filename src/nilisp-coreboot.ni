# ni core language definitions: bootstrap layer stuff
# In the real language, the compiler does CPS for us and provides call-cc.

(defcps* 'id*'     (fn* [x] x) (fn* [x] x))
(defcps* 'call-cc' (fn* [f k] (f k k)) id*)

# It's worth escaping from CPS-land as soon as possible, so let's define some
# macros that do that for us.
(defmacrocps* 'cps*'
  (fn* [form k]
    (macroexpand form
      (fn* [mform]
        (co* (fn* [k1] (str-sym 'fn*' k1))
             (fn* [k2] (str-sym 'x'   k2))
             (fn* [k3] (str-sym 'x'
               (fn* [xsym] (list xsym
                 (fn* [xsymlist] (to-array xsymlist k3))))))
             (fn* [fnsym xsym xsymarray]
               (list fnsym xsymarray xsym
                 (fn* [k-form]
                   (cps-convert mform k-form k))))))))
  id*)

(defmacrocps* 'defmacro'
  (cps*
    (fn* [name f]
      (list (str-sym 'defmacrocps*')
            (sym-str name)
            (list (str-sym 'cps*') f)
            (str-sym 'id*'))))
  id*)

(defmacro def
  (fn* [name x]
    (list (str-sym 'defcps*')
          (sym-str name)
          (list (str-sym 'cps*') x)
          (str-sym 'id*'))))
