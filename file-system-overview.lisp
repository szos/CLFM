(in-package :common-lisp-file-manager)

(defclass clfm-fs-overview-directory ()
  ((pathname :initarg :path
	     :accessor path
	     :initform nil
	     :documentation "The path to the object")
   (directory :initarg :directory-pathname
  	      :accessor directory-pathname
              :documentation "the pathname describing a clfm directory")
   (expandedp :initarg :expandedp
  	      :accessor expandedp
  	      :initform nil
  	      :documentation "controlls whether the directory is expanded or collapsed")
   (subdirs :initarg :subdirectories
	    :accessor subdirectories
	    :documentation "a list of clfm-fs-overview-directory objects from this directory")))

(defparameter *filesystem-from-root*
  (loop for directory in (uiop:subdirectories "/")
	collect (make-instance 'clfm-fs-overview-directory
			       :path (format nil "~a" directory)
			       :expandedp nil
			       :subdirectories nil)))

(defun hidden-file-or-directory-p (path)
  "checks if the final final file or directory in PATH is hidden or not."
  (let* ((p (namestring path))
         (l (length p)))
    (uiop:hidden-pathname-p (if (and (string= "/" (subseq p (- l 1)))
				     (not (string= "/./" (subseq p (- l 3)))))
				(subseq p 0 (- l 1))
				p))))

(defun display-file-system (frame pane)
  (declare (ignore frame))
  (setf (medium-background pane) *background-color*
        (medium-foreground pane) *foreground-color*)
  (labels ((display-directories (directory depth)
	     (with-end-of-line-action (pane :scroll)
	       (if (expandedp directory)
		   (progn
		     (with-output-as-presentation (pane
						   directory
						   'expand-or-collapse-directory)
		       (when  (< 0 depth)
			 (loop for x from 1 to (- depth 1)
			       do (stream-increment-cursor-position pane 10 0))
			 (format pane "↳"))
		       (format pane "~a~&" (car
                                            (reverse
                                             (cl-ppcre:split "/"
                                                             (path
                                                              directory))))))
		     (loop for dir in (subdirectories directory)
			   do (display-directories dir (+ depth 1))))
		   (with-output-as-presentation (pane
						 directory
						 'expand-or-collapse-directory)
		     (when  (< 0 depth)
		       (loop for x from 1 to (- depth 1)
			     do (stream-increment-cursor-position pane 10 0))
		       (format pane "↳"))
		     (format pane "~a~&" (car
                                          (reverse
                                           (cl-ppcre:split "/"
                                                           (path
                                                            directory))))))))))
    (multiple-value-bind (x y) (bounding-rectangle-position (sheet-parent pane))
      (loop for dir in *filesystem-from-root*
	    do (display-directories dir 0))
      (scroll-extent pane x y))))


