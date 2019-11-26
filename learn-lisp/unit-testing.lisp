(defun report-result (form)
  "Check that a form evaluates to t, printing a helpful readout."
  (let ((result (eval form)))
    (format t "~%~:[FAIL~;Pass~] --- ~a" result form)
    result))

(defmacro check (&rest forms)
  "Evaluate a list of forms and return their results."
  `(progn
     ,@(loop for f in forms collect `(report-result ',f))))

(defmacro use-gensyms (names &body body)
  "Assign a gensym to each symbol in name, then execute the body."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro non-terminating-and (&body forms)
  "Return t iff all forms evaluate to t."
  (use-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defvar test-forms '((= 1 2) (= 2 2) (= 3 2)))
