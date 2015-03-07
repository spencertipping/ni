# ni lisp core definitions
# This file builds up the standard language from ni-lisp primitives.

(defmacro let* (fn* [n v body]
  (list (list (str-sym 'fn*')
              (to-array (list n))
              body)
        v)))

(defmacro if (fn* [cond then else]
  (list (list (str-sym 'nth*')
              (list (str-sym 'not') cond)
              (list (str-sym 'fn*') (to-array (nil)) then)
              (list (str-sym 'fn*') (to-array (nil)) else)))))

# List functions. Macros are hard to write without some ability to work with
# lists/arrays/etc.
# TODO
