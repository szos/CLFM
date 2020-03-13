
(in-package :clfm-2)

(defparameter *directory-deletion-validation-function* 'validate-directory)

(defun validate-directory (directory)
  (uiop:directory-exists-p directory))

(defun %delete-directory (dir)
  (uiop:delete-directory-tree dir
			      :validate *directory-deletion-validation-function*))

(define-clfm-command (com-delete-directory) ((directory string))
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

