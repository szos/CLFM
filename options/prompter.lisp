(in-package :common-lisp-file-manager)

;;; This is laid out much the same way as the demodemo file of the clim examples

(defclass prompt-result-mixin ()
  ((clfm-prompt-result :initarg :clfm-prompt-result
                       :initform nil
                       :accessor clfm-prompt-result)))

(defmacro define-prompt-frame (name slots &rest options)
  `(define-application-frame ,name (prompt-result-mixin standard-application-frame)
     ,slots ,@options))

(define-prompt-frame unimplemented-prompter ()
  (:layouts
   (default
    (vertically (:equalize-width t)
      (labelling (:label "Unimplemented"
                  :text-style (make-text-style :serif :roman :huge)
                  :align-x :center)
        (make-pane 'push-button
                   :label "OK"
                   :activate-callback
                   (lambda (gadget)
                     (let ((frame (gadget-client gadget)))
                       (frame-exit frame)))))))))

(defun make-prompt-button (title frame-class set-callback
                           &optional restart-required)
  (make-pane 'push-button
             :label title
             :activate-callback
             (lambda (gadget)
               (let* ((calling-frame (gadget-client gadget))
                      (f (make-application-frame
                          frame-class :calling-frame calling-frame)))
                 (run-frame-top-level f)
                 (when restart-required
                   (notify-user calling-frame
                                "Restart required to take effect"
                                :exit-boxes '((:ok "OK"))))
                 (funcall set-callback (clfm-prompt-result f))))))

(macrolet ((setopt (place value)
             `(handler-bind ((defconfig:invalid-datum-error
                               (lambda (c)
                                 (declare (ignore c))
                                 (let ((r (find-restart 'continue)))
                                   (when r (invoke-restart r))))))
                (setv ,place ,value :db *clfm-options-database*))))
  (define-application-frame clfm-options ()
    ()
    (:menu-bar nil)
    (:layouts
     (default
      (vertically (:equalize-width t)
        (labelling (:label "CLFM Options"
                    :text-style (make-text-style :serif :roman :huge)
                    :align-x :center)
          (horizontally ()
            (labelling (:label "Look and Feel")
              (vertically (:equalize-width t)
                (make-prompt-button
                 "Display Hidden Items"
                 'display-hidden-items-frame
                 (lambda (result)
                   (setopt *display-hidden-items* result)))
                (make-prompt-button "Display Permissions"
                                    'display-permissions-frame
                                    (lambda (result)
                                      (setopt *display-permissions* result)))
                (make-prompt-button "Monospace Font"
                                    'unimplemented-prompter ; 'font-selector-frame
                                    (lambda (result)
                                      (setopt *display-permissions* result)))
                (make-prompt-button "Foreground Color" 'color-picker
                                    (lambda (result)
                                      (setopt *foreground-color* result)))
                (make-prompt-button "Background Color" 'color-picker
                                    (lambda (result)
                                      (setopt *background-color* result))
                                    t)))))
        (horizontally ()
          +fill+
          (200 (make-pane 'push-button :label "Done"
                                       :activate-callback
                                       (lambda (gadget)
                                         (let* ((frame (gadget-client gadget)))
                                           (frame-exit frame)))))))))))

(defun prompt-for-option (&optional calling-frame)
  (let ((frame (make-application-frame 'clfm-options
                                       :calling-frame calling-frame)))
    (run-frame-top-level frame)))
