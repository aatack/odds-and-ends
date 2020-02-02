(defun component-present-p (value)
    (and value (not (equal value :unspecific))))

(defun directory-pathname-p (name)
    (and
        (not (component-present-p (pathname-name name)))
        (not (component-present-p (pathname-type name)))
        name))

(defun pathname-as-directory (name)
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't reliably convert wild pathnames."))
        (if (not (directory-pathname-p name))
            (make-pathname
                :directory (append
                    (or (pathname-directory pathname) (list :relative))
                    (list (file-namestring pathname)))
                :name nil
                :type nil
                :defaults pathname)
            pathname)))
