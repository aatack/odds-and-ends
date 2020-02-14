(defun palindromep (input)
  (eval `(and ,@(mapcar (lambda (a b) (eql a b)) input (reverse input)))))

(defun presentp (target input)
  (if input
    (if (eq target (car input))
      t
      (presentp target (cdr input)))
    nil))
