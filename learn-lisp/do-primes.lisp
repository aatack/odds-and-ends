(defun prime-p (number)
  (when (> number 1))
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac))))

(defun next-prime (number)
  (loop for n from number when (prime-p n) return n))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms-1 (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
        ((> ,var ,ending-value-name))
      ,@body)))

(defmacro with-gensyms-1 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
      ,@body))
