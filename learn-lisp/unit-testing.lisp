(defvar *stack-trace* nil)

(defun report-result (form)
  "Check that a form evaluates to t, printing a helpful readout."
  (let ((result (eval form)))
    (format t "~%~:[FAIL~;Pass~] --- ~a: ~a" result *stack-trace* form)
    result))

(defmacro check (&rest forms)
  "Evaluate a list of forms and return their results."
  `(non-terminating-and
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

(defmacro deftest (name parameters docstring &body body)
  "Define a test function that runs and reports a suite of tests."
  `(defun ,name ,parameters ,docstring
      (let ((*stack-trace* (append *stack-trace* (list ',name))))
        ,@body)))

(deftest test-+ ()
  "Run some simple tests for the addition operator."
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  "Run some simple tests for the multiplication operator."
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  "Run some simple tests for arithmetic operators."
  (test-+)
  (test-*))
