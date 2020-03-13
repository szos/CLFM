(in-package :clfm-2)

(define-application-frame clfm-notify-user-frame ()
  ((message-string :initarg :message-string)
   (message-body :initarg :message-body)
   (exit-boxes :initarg :exit-boxes)
   (title :initarg :title)
   (style :initarg :style)
   (text-style :initarg :text-style)
   (width :initarg :width)
   (return-value :initarg nil :initform :abort))
  (:pane (clfm-notify-user-dialog *application-frame*)))

(defun clfm-notify-generate-exit-box-buttons (specs)
  (mapcar
   (lambda (spec)
     (destructuring-bind (action string &rest args) spec
       (spacing (:thickness 2)
         (apply #'make-pane
                'push-button
                :label string
                :text-style (make-text-style :sans-serif :roman :small) ; XXX
                :activate-callback
                (lambda (gadget)
                  (declare (ignore gadget))
                  ;; This is fboundp business is weird, and only implied by a
                  ;; random message on the old CLIM list. Does the user function
                  ;; take arguments?
                  (when (or (typep action 'function) (fboundp action))
                    (funcall action))
                  (setf (slot-value *application-frame* 'return-value) action)
                  ;; This doesn't work:
                  #+NIL
                  (when (eql action :abort)
                    (and (find-restart 'abort)
                         (invoke-restart 'abort)))
                  (frame-exit *application-frame*))
                args))))
   specs))

(defun clfm-notify-user-dialog (frame)
  (with-slots (message-string message-body exit-boxes text-style width) frame
    (vertically ()
      (spacing (:thickness 6)
	(make-pane 'label-pane :label (or message-string "I'm speechless.") :text-style text-style))
      (spacing (:thickness 6)
	(make-pane 'application-pane
		   :display-function
		   (or (and message-body (eval `(lambda (frame pane)
						  (declare (ignore frame))
						  ,message-body)))
		       (lambda (frame pane)
			 (declare (ignore frame)
				  (ignore pane))))
		   :width width))
      
      (spacing (:thickness 4)
	(make-pane 'hbox-pane :contents
		   (cons '+fill+
			 (clfm-notify-generate-exit-box-buttons exit-boxes)))))))

(defun clfm-notify-user-frame-manager
    (frame-manager  
     &key frame associated-window message-body message-string width
       (title "")
       documentation
       (exit-boxes '((:exit "OK")))
       ;; The 'name' arg is in the spec but absent from the Lispworks
       ;; manual, and I can't imagine what it would do differently
       ;; than 'title'.
       name
       style
       (text-style (make-text-style :sans-serif :roman :small)))
  (declare (ignore associated-window documentation))
  (let ((frame (make-application-frame 'clfm-notify-user-frame
				       :calling-frame frame
				       :pretty-name title
				       :message-string message-string
				       :message-body message-body
				       :frame-manager frame-manager
				       :exit-boxes exit-boxes
				       :title (or name title)
				       :style style
				       :text-style text-style
				       :width width)))
    (run-frame-top-level frame)
    (slot-value frame 'return-value)))

(defparameter *clfm-notify-pass-to-notification* nil)

(defmacro clfm-notify ((frame message (&rest args-to-pass) &rest args)
		       &body message-body)
  `(progn
     (setf *clfm-notify-pass-to-notification*
	   `(,,@(loop for arg in args-to-pass
		      collect arg)))
     (apply #'clfm-notify-user-frame-manager
	    (if ,frame (frame-manager ,frame) (find-frame-manager))
	    :message-string ,message
	    :message-body `(let ,(loop for arg in ',args-to-pass
				       for v in *clfm-notify-pass-to-notification*
				       collect `(,arg (handler-case ,v
							(t ()
							  ',v))))
			     ,@',message-body)
	    :frame ,frame
	    ',args)))
