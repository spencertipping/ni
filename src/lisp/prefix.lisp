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

(defun read-col (text)
  (when text
    (let* ((*read-eval* nil)
           (r (read-from-string text)))
      (etypecase r
        (symbol text)
        (number r)))))

(defun r (&rest values)
  (apply #'join #\Tab values))

(defmacro with-ni-env (filter-p &body body)
  (let ((l-var (gensym "L"))
        (cols-var (gensym "COLS")))
    `(let ((*standard-input* (sb-sys:make-fd-stream 3)))
       (symbol-macrolet ,(loop for colname in '(a b c d e f g h i j k l m n o p q)
                            for index from 0
                            collect (list colname `(read-col (nth ,index ,cols-var))))     
         (loop for ,l-var = (read-line *standard-input* nil) while ,l-var do
              (let ((,cols-var (split ,l-var #\Tab)))
                ,(if filter-p
                     `(when (progn ,@body)
                        (write-string ,l-var)
                        (terpri))
                     `(progn
                        ,@(loop for form in body collect
                               `(progn
                                  (princ ,form)
                                  (terpri)))))))))))
