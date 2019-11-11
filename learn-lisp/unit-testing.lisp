(defun report-result (result form)
  (format t "~%~:[FAIL~;Pass~] --- ~a" result form))

(defmacro check (&rest forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))
