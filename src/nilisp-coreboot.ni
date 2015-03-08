# ni core language definitions: bootstrap layer stuff
# In the real language, the compiler does CPS for us and provides call-cc.

(defcps* (fn* [x] x) 'id*'     (fn* [x] x))
(defcps* id*         'call/cc' (fn* [k f] (f k k)))

# It's worth escaping from CPS-land as soon as possible, so let's define some
# macros that do that for us.
(defmacrocps* id* 'cps*'
  (fn* [k form]
    (macroexpand
      (fn* [mform]
        (co* (fn* [fnsym xsym xsymarray]
               (list (fn* [k-form] (cps-convert k mform k-form))
                     fnsym xsymarray xsym))
             (fn* [k1] (str-sym k1 'fn*' k1))
             (fn* [k2] (str-sym k2 'x'   k2))
             (fn* [k3] (str-sym
               (fn* [xsym] (list (fn* [xsymlist] (to-array k3 xsymlist))
                                 xsym))
               'x'))))
      form)))

(defmacrocps* id* 'defmacro'
  (cps*
    (fn* [name f]
      (list (str-sym 'defmacrocps*')
            (str-sym 'id*')
            (sym-str name)
            (list (str-sym 'cps*') f)))))

(defmacro def
  (fn* [name x]
    (list (str-sym 'defcps*')
          (str-sym 'id*')
          (sym-str name)
          (list (str-sym 'cps*') x))))

(defmacro macroprint (fn* [form]
  ((fn* [x] 'macroprint') (print (macroexpand form)))))
