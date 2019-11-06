(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
    (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-cd ()
    (make-cd
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped")))

(defun save-db (file-name)
    (with-open-file (out file-name
        :direction :output
        :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (file-name)
    (with-open-file (in file-name)
        (with-standard-io-syntax
            (setf *db* (read in)))))

(load-db "learn-lisp/serialisation-test.db")
(dump-db)
