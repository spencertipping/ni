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
(def listgen
  (fn* [xs]
    (if (count xs)
      (fn* [k] (k (car xs) (listgen (cdr xs))))
      (fn* [k] (k)))))

(def lreduce*
  (fn* [f init generator]
    (generator (fn* xs
      (if (count xs)
        (lreduce* f (f init (aget xs 0)) (aget xs 1))
        init)))))

(def rreduce*
  (fn* [f end generator]
    (generator (fn* xs
      (if (count xs)
        (f (aget xs 0) (rreduce* f end (aget xs 1)))
        end)))))

(def gen
  (fn* [xs]
    (if (= (type xs) 'list')
      (listgen xs)
      (listgen (to-list xs)))))

(def map
  (fn* [f xs]
    (rreduce* (fn* [x r] (cons (f x) r))
              (nil)
              (gen xs))))

(def filter
  (fn* [f xs]
    (rreduce* (fn* [x r] (if (f x) (cons x r) r))
              (nil)
              (gen xs))))

(def append (fn* [xs ys] (rreduce* cons ys (gen xs))))
(def rappend (fn* [xs ys]
  (if (count xs)
    (rappend (cdr xs) (cons (car xs) ys))
    ys)))

(def reverse (fn* [xs] (rappend xs (nil))))

# Useful macros
(def else 1)
(defmacro cond
  (fn* cases
    (if (count cases)
      (list (str-sym 'if')
            (car cases)
            (car (cdr cases))
            (append (list (str-sym 'cond'))
                    (cdr (cdr cases))))
      (list (str-sym 'nil')))))

(defmacro ->>
  (fn* forms
    (lreduce* (fn* [x form] (append form (list x)))
              (car forms)
              (gen (cdr forms)))))

(defmacro ->
  (fn* forms
    (lreduce* (fn* [x form] (cons (car form)
                                  (cons x (cdr form))))
              (car forms)
              (gen (cdr forms)))))

(defmacro <<- (fn* forms (cons (str-sym '->>') (reverse forms))))

# Quoting
(def q*
  (<<- (fn* [form])
       (let* t (type form))
       (cond (= t 'list')   (cons (str-sym 'list') (map q* form))
             (= t 'array')  (cons (str-sym 'to-array')
                                  (list (cons (str-sym 'list')
                                              (map q* form))))
             (= t 'hash')   (cons (str-sym 'to-hash')
                                  (list (cons (str-sym 'list')
                                              (map q* form))))
             (= t 'symbol') (list (str-sym 'str-sym')
                                  (sym-str form))
             else           form)))

(defmacro q q*)

(cps* (eval (q (print "hi there!"))))
