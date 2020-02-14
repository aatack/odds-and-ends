(defun palindromep (input)
  "Determine whether or not a list is a palindrome."
  (eval `(and ,@(mapcar (lambda (a b) (eql a b)) input (reverse input)))))

(defun presentp (target input)
  "Determine whether an atom appears anywhere in a tree."
  (if (or (null input) (atom input))
    (eql target input)
    (eval `(or ,@(mapcar (lambda (child) (presentp target child)) input)))))
