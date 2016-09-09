;;;;;;;;
;; NB: don't delete the line of semicolons above; SBCL throws away the first few
;; bytes on CentOS.
(declaim (optimize (speed 3) (safety 0)))
(setf *read-default-float-format* 'double-float)

;;; utility functions from wu-sugar

(defun str (&rest values)
  (with-output-to-string (s)
    (dolist (val values)
      (princ val s))))

(defun join (separator &rest strings)
  "Concatenates STRINGS, joining them by SEPARATOR."
  (when (characterp separator)
    (setf separator (string separator)))
  (if strings
      (reduce (lambda (a b) (str a separator b)) strings)
      ""))

(defun split (string &rest delimiter-chars)
  "Splits STRING by one or more delimiter characters, returning a list."
  (let ((current-pos 0)
	result)
    (loop
       (push (subseq string 
		     current-pos 
		     (setf current-pos (position-if (lambda (c) (member c delimiter-chars)) 
						    string 
						    :start current-pos)))
	     result)
       (unless current-pos (return))
       (incf current-pos))
    (nreverse result)))

(defun starts-with-p (seq subseq)
  (let ((subseq-len (length subseq))) 
    (if (<= subseq-len (length seq)) 
	(search subseq seq :end2 subseq-len)
	nil)))

(defun ends-with-p (seq subseq)
  (let ((seq-len (length seq))
	(subseq-len (length subseq))) 
    (if (<= (length subseq) (length seq)) 
	(search subseq seq :from-end t :start2 (- seq-len subseq-len))
	nil)))

(defun partial (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))


(defvar *l*)
(defvar *cols*)

(defun read-col (text)
  (when text
    (let* ((*read-eval* nil)
           (r (read-from-string text)))
      (etypecase r
        (symbol text)
        (number r)))))

(defun %r (&rest values)
  (apply #'join #\Tab values))

(defmacro r (&rest values)
  `(multiple-value-call #'%r ,@values))

(defvar *line-q* (make-array 10 :adjustable t :fill-pointer 0))

(declaim (inline next-line))
(defun next-line ()
  (or (when (plusp (length *line-q*))
        (vector-pop *line-q*))
      (read-line *standard-input* nil)))

(defun %sr (&rest reducefn_inputfn_current)
  (loop for l = (next-line) while l do
       (let ((*cols* (split l #\Tab)))
         (loop for (reducefn inputfn current) in reducefn_inputfn_current
              for ric in reducefn_inputfn_current do
              (setf (third ric)
                    (funcall reducefn current (funcall inputfn))))))
  (apply #'values (mapcar #'third reducefn_inputfn_current)))

(defmacro sr (&rest reducer_input_initial)
  `(let* ((bindings (list ,@(loop for (reducer input initial) in reducer_input_initial append
                                 (list reducer `(lambda () ,input) initial))))
          (fixed-bindings (loop for (reducefn inputfn initial) on bindings by #'cdddr collect
                               (list reducefn inputfn (if initial                                                          
                                                          (funcall reducefn initial (funcall inputfn))
                                                          (funcall inputfn))))))
     (apply #'%sr fixed-bindings)))

(defun %se (reducefn inputfn continuefn current)
  (loop for l = (next-line) while l do
       (let ((*cols* (split l #\Tab)))
         (if (funcall continuefn)
             (setf current (funcall reducefn current (funcall inputfn)))
             (progn
               (vector-push-extend l *line-q*)
               (return)))))
  current)

(defmacro se (reducer input partition &optional (initial nil initial-supplied-p))
  `(let* ((inputfn (lambda () ,input))
          (partfn (lambda () ,partition))
          (first-partfn-result (funcall partfn))
          (continuefn (lambda () (equal first-partfn-result (funcall partfn))))
          (reducefn ,reducer))
     (%se reducefn inputfn continuefn ,(if initial-supplied-p               
                                          `(funcall reducefn ,initial (funcall inputfn))
                                          `(funcall inputfn)))))

(defun %rw (continue-fn)
  (let ((result (list *l*)))
    (loop for *l* = (next-line) while *l* do
         (let ((*cols* (split *l* #\Tab)))
           (if (funcall continue-fn)
               (push *l* result)
               (progn
                 (vector-push-extend *l* *line-q*)
                 (return)))))
    (apply #'values (nreverse result))))

(defmacro rw (condition)
  `(let ((continue-fn (lambda () ,condition)))
     (%rw continue-fn)))

(defmacro ru (condition)
  `(let ((continue-fn (lambda () (not ,condition))))
     (%rw continue-fn)))

;; TODO: convert row strings to numbers
;; (defun %mean (&rest values)
;;   (/ (apply #'+ values)
;;      (length values)))

;; (defmacro mean (form)
;;   `(multiple-value-call #'%mean ,form))

(defun output-rows (&rest rows)
  (dolist (row rows)
    (princ row)
    (terpri)))

(defmacro with-ni-env (filter-p &body body)
  (let ((l-var '*l* ;;(gensym "L")
         ))
    `(let ((*standard-input* (sb-sys:make-fd-stream 3)))
       (symbol-macrolet ,(loop for colname in '(a b c d e f g h i j k l m n o p q)
                            for index from 0
                            collect (list colname `(read-col (nth ,index *cols*))))     
         (loop for ,l-var = (next-line) while ,l-var do
              (let ((*cols* (split ,l-var #\Tab)))
                ,(if filter-p
                     `(when (progn ,@body)
                        (write-string ,l-var)
                        (terpri))
                     `(progn
                        ,@(loop for form in body collect
                               `(multiple-value-call #'output-rows ,form)
                        )))))))))
