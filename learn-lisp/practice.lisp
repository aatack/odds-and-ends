(defun palindromep (input)
  (eval `(and ,@(mapcar (lambda (a b) (eql a b)) input (reverse input)))))
