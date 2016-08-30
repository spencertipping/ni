;; Ok Wes, this one's on you dude.
;; The contents of the data stream we want to process are coming in on FD 3,
;; which should be available for reading (just like 0 normally is). In theory
;; you could issue a read() syscall on fd 3 immediately and it would give you
;; data, so you just have to convince Lisp to do this.

(print :uhoh-no-fd-3-yet)
