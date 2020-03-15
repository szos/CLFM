
(in-package :clfm-2)

(defparameter *directory-deletion-validation-function* 'validate-directory)

(defun validate-directory (directory)
  (uiop:directory-exists-p directory))

(defun %delete-directory (dir)
  (uiop:delete-directory-tree dir
			      :validate *directory-deletion-validation-function*))

(defun delete-directory (directory)
  (let ((reter (clfm-notify (*application-frame* "IRREVERSIBLE OPERATION" nil
						 :exit-boxes ((t "PROCEED")
							      (nil "ABORT"))
						 :width 400)
		 (with-end-of-line-action (pane :wrap*)
		   (with-etbembo (pane nil 18)
		     (format pane "You are attempting to delete a file or directory. This operation is ")
		     (with-etbembo (pane :bold 18)
		       (format pane "IRREVERSIBLE"))
		     (format pane ". Proceed with caution."))))))
    (when reter
      (%delete-directory (pathname directory)))))

(defun delete-item (item)
  (let ((reter (clfm-notify (*application-frame* "IRREVERSIBLE OPERATION" nil
						 :exit-boxes ((t "PROCEED")
							      (nil "ABORT"))
						 :width 400)
		 (with-end-of-line-action (pane :wrap*)
		   (with-etbembo (pane nil 18)
		     (format pane "You are attempting to delete a file or directory. This operation is ")
		     (with-etbembo (pane :bold 18)
		       (format pane "IRREVERSIBLE"))
		     (format pane ". Proceed with caution."))))))
    (when reter
      (cond ((uiop:directory-exists-p item)
	     (%delete-directory (pathname item)))
	    ((uiop:file-exists-p item)
	     (delete-file item))))))

(define-clfm-command (com-delete) ((item string))
  (cond ((uiop:directory-exists-p item)
	 (delete-directory item))
	((uiop:file-exists-p item)
	 (delete-item item))))
