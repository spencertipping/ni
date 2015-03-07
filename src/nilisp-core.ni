# ni core language definitions
(defcps_ 'id_'     (fn* [x] x) (fn* [x] x))
(defcps_ 'call-cc' (fn* [f k] (f k k)) id_)

# It's worth escaping from CPS-land as soon as possible, so let's define some
# macros that do that for us.
(defmacrocps_ 'cps_'
  (fn* [form k]
    (co* (fn* [k1] (str-sym 'fn*' k1))
         (fn* [k2] (str-sym 'x'   k2))
         (fn* [k3] (str-sym 'x'
           (fn* [xsym] (list xsym
             (fn* [xsymlist] (to-array xsymlist k3))))))
         (fn* [fnsym xsym xsymarray]
           (list fnsym xsymarray xsym
             (fn* [k-form]
               (cps-convert form k-form k))))))
  id_)

(defmacrocps_ 'defmacro_'
  (cps_
    (fn* [name f]
      (list (str-sym 'defmacrocps_')
            (sym-str name)
            (list (str-sym 'cps_') f)
            (str-sym 'id_'))))
  id_)

(defmacro_ def_
  (fn* [name x]
    (list (str-sym 'defcps_')
          (sym-str name)
          (list (str-sym 'cps_') x)
          (str-sym 'id_'))))

(def_ say-hi (fn* [x] (print x)))
(cps_ (say-hi 'there'))
