(defun prime-p (number)
  (when (> number 1))
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac))))

(defun next-prime (number)
  (loop for n from number when (prime-p n) return n))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
        ((> ,var ,end))
      ,@body))
