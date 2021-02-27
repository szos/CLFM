(in-package :common-lisp-file-manager)

(define-clfm-command (com-expand-collapse-fs-overview-directory)
    ((directory clfm-fs-overview-directory))
  (if (expandedp directory)
      (labels ((collapse-subdirs (dir)
                 (when (subdirectories dir)
                   (loop for subdir in (subdirectories dir)
                         do (collapse-subdirs subdir))
                   (setf (subdirectories dir) nil))))
        (collapse-subdirs directory)
        (setf (expandedp directory) nil))
      (setf (subdirectories directory)
            (loop for d in (remove-if #'hidden-file-or-directory-p
                                      (uiop:subdirectories (path directory)))
                  collect (make-instance 'clfm-fs-overview-directory
                                         :path (format nil "~a" d)
                                         :expandedp nil
                                         :subdirectories nil))
            (expandedp directory) t)))

(define-presentation-type expand-or-collapse-directory ())
(define-presentation-to-command-translator expand-collapse-directory
    (expand-or-collapse-directory com-expand-collapse-fs-overview-directory clfm
     :gesture :select
     :documentation "Expand or Collapse Directory")
    (obj)
  (list obj))
