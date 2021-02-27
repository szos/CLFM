;;;; common-lisp-file-manager.lisp

(in-package #:common-lisp-file-manager)

(defun app-main (&key (layout 'one-panel-fs))
  ;; (initialize-fonts)
  ;; (handler-case (load "~/.clfm.d/init.lisp")
  ;;   (t () ()))
  (let ((frame (make-application-frame 'clfm)))
    (handler-case (progn (setf (frame-current-layout frame) layout)
                         (run-frame-top-level frame))
      (restart-clfm ()
        (frame-exit frame)
        (app-main :layout layout)))))
