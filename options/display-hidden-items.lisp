(in-package :common-lisp-file-manager)

(defun make-y/n-result-button (type &optional label)
  (make-pane 'push-button
             :label (or label (if type
                                  "Yes"
                                  "No"))
             :activate-callback
             (lambda (gadget)
               (let ((calling-frame (gadget-client gadget)))
                 (setf (clfm-prompt-result calling-frame) (if type t nil))
                 (frame-exit calling-frame)))))

(define-prompt-frame display-hidden-items-frame ()
  (:layouts
   (default
    (vertically (:equalize-width t)
      (labelling (:label "Display Hidden Files?"
                  :text-style (make-text-style :serif :roman :huge)
                  :align-x :center)
        (horizontally (:equalize-height t)
          (make-y/n-result-button t "Display Files Beginning With a Dot")
          (make-y/n-result-button nil "Don't Display Files Beginning With a Dot")))))))
