(defun report-result (form)
  (let ((result (eval form)))
    (format t "~%~:[FAIL~;Pass~] --- ~a" result form)
    result))

(defmacro check (&rest forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ',f))))
