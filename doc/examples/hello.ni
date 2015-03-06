(def_ say-hi (fn* [x k] (print "hello" x "in CPS!" k)))
(say-hi "world" (fn* [x] x))
