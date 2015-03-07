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
(def list-unfold
  (fn* [xs]
    (if (count xs)
      (fn* [k] (k (car xs) (list-unfold (cdr xs))))
      (fn* [k] (k)))))

(def reduce
  (fn* [f init generator]
    (generator (fn* xs
      (if (count xs)
        (reduce f (f init (aget xs 0)) (aget xs 1))
        init)))))

(cps*
  (reduce (fn* [x y] (print x y))
          1
          (list-unfold [2 3 4])))
