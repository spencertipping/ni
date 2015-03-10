# ni lisp core definitions
# This file builds up the standard language from ni-lisp primitives.

(defmacro let* (fn* [n v body]
  (list (list (str-sym "fn*") [n] body)
        v)))

(defmacro if (fn* [cond then else]
  (list (list (str-sym "nth*")
              (list (str-sym "not") cond)
              (list (str-sym "fn*") [] then)
              (list (str-sym "fn*") [] else)))))

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
    (if (= (type xs) "list")
      (listgen xs)
      (listgen (to-list xs)))))

(def map
  (fn* [f xs]
    (rreduce* (fn* [x r] (cons (f x) r))
              nil
              (gen xs))))

(def filter
  (fn* [f xs]
    (rreduce* (fn* [x r] (if (f x) (cons x r) r))
              nil
              (gen xs))))

(def append (fn* [xs ys] (rreduce* cons ys (gen xs))))
(def rappend (fn* [xs ys]
  (if (count xs)
    (rappend (cdr xs) (cons (car xs) ys))
    ys)))

(def reverse (fn* [xs] (rappend xs nil)))

# Useful macros
(def else 1)
(defmacro cond
  (fn* cases
    (if (count cases)
      ((macro-fn "if") (car cases)
                       (car (cdr cases))
                       (apply (macro-fn "cond") (cdr (cdr cases))))
      (list (str-sym "nil")))))

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

(defmacro <<- (fn* forms (apply (macro-fn "->>") (reverse forms))))

# Quoting
(def qq*
  (<<- (fn* [form substs])
       (let* t     (type form))
       (let* recur (fn* [f] (qq* f substs)))
       (cond (= t "list")   (cons (str-sym "list") (map recur form))
             (= t "array")  (to-array (map recur form))
             (= t "hash")   (to-hash  (map recur form))
             (= t "symbol") (get substs form
                                 (list (str-sym "str-sym") (sym-str form)))
             else           form)))

(def q* (fn* [form] (qq* form {})))
(defmacro q q*)

(defmacro qq
  (<<- (fn* [form substs])
       (let* subgs (gensym "substs"))
       (let* qform (qq* form (lreduce* (fn* [h k]
                               (assoc h k (list (q get) subgs (q* k))))
                               {}
                               (gen (keys substs)))))
       (let* qsub  (lreduce* (fn* [h k] (assoc h (q* k) (get substs k)))
                             {}
                             (gen (keys substs))))

       (list (list (q fn*) [subgs] qform) qsub)))

(def qe* (fn* [form] (q* (macroexpand form))))
(defmacro qe qe*)
