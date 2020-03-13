
(in-package :clfm-2)

(defparameter *directory-deletion-validation-function* 'validate-directory)

(defun validate-directory (directory)
  (uiop:directory-exists-p directory))

(defun %delete-directory (dir)
  (uiop:delete-directory-tree dir
			      :validate *directory-deletion-validation-function*))

(define-clfm-command (com-delete-directory) ((directory string))
  (%delete-directory (pathname directory)))

