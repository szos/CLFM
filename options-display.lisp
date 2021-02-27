(in-package :common-lisp-file-manager)

(define-presentation-type clfm-option ())

(define-presentation-method present
    (option (type clfm-option) stream view &key)
  ;; assumes within slim:with-table
  (slim:row
    (slim:cell (format stream "~A" (car option)))
    (slim:cell (format stream "~A" (symbol-value (car option))))))

(defun display-options (frame pane)
  (declare (ignore frame))
  (with-end-of-line-action (pane :allow) 
    (slim:with-table (pane)
      (slim:row
        (slim:cell (format pane "Option"))
        (slim:cell (format pane "")))
      (loop for option in *options-list*
            do (present option 'clfm-option :stream pane :single-box t)))))

(define-clfm-command (com-prompt-for-option) ((option list))
  (funcall (cdr option) (car option)))

(define-presentation-to-command-translator prompt-for-option-value
    (clfm-option com-prompt-for-option clfm)
    (obj)
  (list obj))

(define-clfm-command (com-prompt-for-option-subframe) ()
  (prompt-for-option *application-frame*))


