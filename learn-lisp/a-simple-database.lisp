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

(defun select (selector)
    (remove-if-not selector *db*))

(defmacro where (&rest clauses)
    `#'(lambda (cd) (and ,@(make-comparator-list clauses))))

(defun update (selector &key title artist rating (ripped nil ripped-p))
    (setf *db*
        (mapcar
            #'(lambda (row)
                (when (funcall selector row)
                    (if title (setf (getf row :title) title))
                    (if artist (setf (getf row :artist) artist))
                    (if rating (setf (getf row :rating) rating))
                    (if ripped-p (setf (getf row :ripped) ripped)))
            row) *db*)))

(defun delete-records (selector)
    (setf *db* (remove-if selector *db*)))

(defun make-comparator (field value)
    `(equal (getf cd ,field) ,value))

(defun make-comparator-list (fields)
    (loop while fields
        collecting (make-comparator (pop fields) (pop fields))))
