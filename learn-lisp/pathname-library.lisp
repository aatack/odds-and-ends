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

(defun directory-wildcard (directory-name)
    (make-pathname
        :name :wild
        :type #-clisp :wild #+clisp nil
        :defaults (pathname-as-directory directory-name)))

(defun list-directory (directory-name)
    (when (wild-pathname-p directory-name)
        (error "Can only list concrete directory names."))
    (let ((wildcard (directory-wildcard directory-name)))
    
        #+(or sbcl cmu lispworks)
        (directory wildcard)

        #+openmcl
        (directory wildcard :directories t)

        #+allegro
        (directory wildcard :directories-are-files nil)

        #+clisp
        (nconc
            (directory wildcard)
            (directory (clisp-subdirectories-wildcard wildcard)))

        #-(or sbcl cmu lispworks openmcl allegro clisp)
        (error "list-directory not implemented.")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
    (make-pathname
        :directory (append (pathname-directory wildcard) (list :wild))
        :name nil
        :type nil
        :defaults wildcard))

(defun file-exists-p (pathname)
    #+(or sbcl lispworks openmcl)
    (probe-file pathname)
    
    #+(or allegro cmu)
    (or (probe-file (pathname-as-directory pathname)) (probe-file pathname))

    #+clisp
    (or (ignore-errors
            (probe-file (pathname-as-file pathname)))
        (ignore-errors
            (let ((directory-form (pathname-as-directory pathname)))
                (when (ext:probe-directory directory-form)
                    directory-form))))

    #-(or sbcl lispworks openmcl allegro cmu clisp)
    (error "file-exists-p not implemented"))

(defun pathname-as-file (name)
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't reliably convert wild pathnames."))
        (if (directory-pathname-p pathname)
            (let* ((directory (pathname-directory pathname))
                (name-and-type (pathname (first (last directory)))))
                (make-pathname
                    :directory (butlast directory)
                    :name (pathname-name name-and-type)
                    :type (pathname-type name-and-type)
                    :defaults pathname))
            pathname)))
