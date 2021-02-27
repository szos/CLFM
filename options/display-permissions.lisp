(in-package :common-lisp-file-manager)


(defun make-y/n-button (label callback-with-frame)
  (make-pane 'push-button :label label
                          :activate-callback
                          (lambda (gadget)
                            (let ((frame (gadget-client gadget)))
                              (funcall callback-with-frame frame)))))

(define-prompt-frame display-permissions-frame
    ((user :initform nil
           :accessor display-permissions-frame-user)
     (group :initform nil
            :accessor display-permissions-frame-group)
     (others :initform nil
             :accessor display-permissions-frame-others))
  (:panes (permissions :application
                       :scroll-bars nil
                       :incremental-redisplay t
                       :display-function 'display-permissions-preview))
  (:layouts
   (default
    (vertically (:equalize-width t)
      (labelling (:label "Select Permissions to Display"
                  :text-style (make-text-style :serif :roman :huge)
                  :align-x :center)
        (vertically (:equalize-width t)
          (horizontally (:equalize-height t)
            permissions)
          (horizontally (:equalize-height t)
            (make-y/n-button
             "Display User Permissions"
             (lambda (frame)
               (setf (display-permissions-frame-user frame)
                     (not (display-permissions-frame-user frame)))
               (redisplay-frame-pane frame 'permissions)))
            (make-y/n-button
             "Display Group Permissions"
             (lambda (frame)
               (setf (display-permissions-frame-group frame)
                     (not (display-permissions-frame-group frame)))
               (redisplay-frame-pane frame 'permissions)))
            (make-y/n-button
             "Display Others Permissions"
             (lambda (frame)
               (setf (display-permissions-frame-others frame)
                     (not (display-permissions-frame-others frame)))
               (redisplay-frame-pane frame 'permissions))))
          (horizontally ()
            +fill+
            (make-pane 'push-button
                       :label "Done"
                       :activate-callback
                       (lambda (gadget)
                         (let ((frame (gadget-client gadget)))
                           (with-slots (user group others) frame
                             (setf (clfm-prompt-result frame)
                                   (append (when user '(:user))
                                           (when group '(:group))
                                           (when others '(:others))))
                             (frame-exit frame))))))))))))

(defmethod initialize-instance :after ((obj display-permissions-frame)
                                       &key &allow-other-keys)
  (with-slots (user group others clfm-prompt-result) obj
    (setf user (member :user *display-permissions*)
          group (member :group *display-permissions*)
          others (member :others *display-permissions*)
          clfm-prompt-result *display-permissions*)))

(defun display-permissions-preview (frame pane)
  (let* ((user (display-permissions-frame-user frame))
         (group (display-permissions-frame-group frame))
         (others (display-permissions-frame-others frame)))
    (when user (format pane "Displaying User Permissions~&"))
    (when group (format pane "Displaying Group Permissions~&"))
    (when Others (format pane "Displaying Others Permissions~&"))))
