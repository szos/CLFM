(in-package :common-lisp-file-manager)

(make-command-table 'clfm-file-menu
                    :errorp nil
                    :menu '(("New" :menu clfm-file-new-menu)
                            ("Properties" :command com-prompt-for-option-subframe)
                            ("Restart" :command com-restart)
                            ("Quit" :command com-quit)))

(make-command-table 'clfm-file-new-menu
                    :errorp nil
                    :menu '(("File" :command com-touch-file)
                            ("Directory" :command com-mkdir)))

(define-prompt-frame clfm-get-text
    ((label :initarg :label
            :initform "Enter Text"
            :accessor frame-label))
  (:panes (text-edit :text-editor
                     :value ""
                     :value-changed-callback
                     (lambda (pane value)
                       (setf (clfm-prompt-result (pane-frame pane)) value))
                     :width 200))
  (:layouts
   (default
    (labelling (:label (frame-label *application-frame*)
                :text-style (make-text-style :serif :roman :huge)
                :align-x :center)
      (vertically (:equalize-width t)
        text-edit
        (horizontally ()
          +fill+
          (make-pane 'push-button
                     :label "Accept"
                     :activate-callback
                     (lambda (gadget)
                       (let ((frame (gadget-client gadget)))
                         (frame-exit frame))))
          (make-pane 'push-button
                     :label "Cancel"
                     :activate-callback
                     (lambda (gadget)
                       (let ((frame (gadget-client gadget)))
                         (setf (clfm-prompt-result frame) nil)
                         (frame-exit frame))))))))))

(defun prompt-for-text-input (label calling-frame)
  (let ((frame (make-application-frame
                'clfm-get-text
                :calling-frame calling-frame
                :label label)))
    (run-frame-top-level frame)
    (clfm-prompt-result frame)))

(define-prompt-frame clfm-prompt-filesystem-entry
    ((label :initarg :label
            :initform "Enter Text"
            :accessor frame-label)
     (width :initarg :text-editor-width
            :initform 200
            :accessor frame-text-editor-width)
     (dirs :initarg :directory-list
           :accessor frame-panel-directories)
     (dir :initarg :dir
          :accessor frame-dir)
     (text :initarg :text
           :accessor frame-text))
  (:panes (text-edit :text-editor
                     :value ""
                     :value-changed-callback
                     (lambda (pane value)
                       (setf (clfm-prompt-result (pane-frame pane)) value))
                     :width 200)
          (where :option-pane
                 :value (car (frame-panel-directories *application-frame*))
                 :items (frame-panel-directories *application-frame*)
                 :value-changed-callback
                 (lambda (pane value)
                   (setf (frame-dir (pane-frame pane)) value))))
  (:layouts
   (default
    (labelling (:label (frame-label *application-frame*)
                :text-style (make-text-style :serif :roman :huge)
                :align-x :center)
      (vertically (:equalize-width t)
        (labelling (:label "In Directory"
                    :align-x :center)
          where)
        text-edit
        (horizontally () +fill+
          (make-pane 'push-button
                     :label "Accept"
                     :activate-callback
                     (lambda (gadget)
                       (let ((frame (gadget-client gadget)))
                         (setf (clfm-prompt-result frame)
                               (concatenate 'string
                                            (frame-dir frame)
                                            (clfm-prompt-result frame)))
                         (frame-exit frame))))
          (make-pane 'push-button
                     :label "Cancel"
                     :activate-callback
                     (lambda (gadget)
                       (let ((frame (gadget-client gadget)))
                         (setf (clfm-prompt-result frame) nil)
                         (frame-exit frame))))))))))

;; pass in both panels current directories. 

(define-clfm-command (com-touch-file) ()
  (let* ((p1 (find-pane-named *application-frame* 'panel-1))
         (p2 (find-pane-named *application-frame* 'panel-2))
         (frame (make-application-frame
                 'clfm-prompt-filesystem-entry
                 :calling-frame *application-frame*
                 :label "Touch File"
                 :width 200
                 :directory-list `(,@(when p1
                                       (list (namestring (pane-directory p1))))
                                   ,@(when p2
                                       (list (namestring (pane-directory p2)))))
                 :dir (namestring (pane-directory p1)))))
    (run-frame-top-level frame)
    (notify-user *application-frame*
                 (format nil "touch '~A'" (clfm-prompt-result frame)))))

(define-clfm-command (com-mkdir) ()
  (let* ((p1 (find-pane-named *application-frame* 'panel-1))
         (p2 (find-pane-named *application-frame* 'panel-2))
         (frame (make-application-frame
                 'clfm-prompt-filesystem-entry
                 :calling-frame *application-frame*
                 :label "Create Directory"
                 :width 200
                 :directory-list `(,@(when p1
                                       (list (namestring (pane-directory p1))))
                                   ,@(when p2
                                       (list (namestring (pane-directory p2)))))
                 :dir (namestring (pane-directory p1)))))
    (run-frame-top-level frame)
    (notify-user *application-frame*
                 (format nil "mkdir '~A'" (clfm-prompt-result frame)))))

(define-clfm-command (com-open-file-with) ((file clfm-file))
  (let ((program (prompt-for-text-input "Enter Program" *application-frame*)))
    (notify-user *application-frame*
                 (format nil "~A '~A'" program (clfm-path file)))))
