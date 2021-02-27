(in-package :common-lisp-file-manager)

;; Taken from colorslider clim demo
(defclass abstract-colored-gadget (basic-gadget) ())
(defclass generic-colored-gadget (abstract-colored-gadget)
  ((color :initform +black+ :accessor colored-gadget-color)))

(defmethod (setf colored-gadget-color) :after
    ((new-value t) (gadget generic-colored-gadget))
  ;; Taken from colorslider clim demo
  (repaint-sheet gadget +everywhere+))

(defmethod handle-repaint ((gadget generic-colored-gadget) region)
  ;; Taken from colorslider clim demo
  (with-bounding-rectangle* (x1 y1 x2 y2) region
    (draw-rectangle* gadget x1 y1 x2 y2 :ink (colored-gadget-color gadget))))

(defun make-slider (label writer)
  ;; Taken from colorslider clim demo
  (flet ((update (gadget value)
           (let ((frame (gadget-client gadget)))
             (funcall writer value frame))))
    (make-pane :slider :label label
                       :min-value 0
                       :max-value 1
                       :value 0
                       :show-value-p t :decimal-places 2
                       :orientation :horizontal
                       :drag-callback #'update
                       :value-changed-callback #'update)))

(define-prompt-frame color-picker 
    ((%red :accessor red :initform 0)
     (%green :accessor green :initform 0)
     (%blue :accessor blue :initform 0)
     (%return :accessor return-color :initform (make-rgb-color 0 0 0))
     ;; (%ihs-te :accessor ihs-text :initform "#00ff00")
     (%rgb-te :accessor rgb-text :initform "#00ff00"))
  (:menu-bar nil)
  (:panes
   (combined generic-colored-gadget :min-width 40)
   (red generic-colored-gadget :min-width 20)
   (green generic-colored-gadget :min-width 20)
   (blue generic-colored-gadget :min-width 20)
   (slider-red (make-slider "Red" #'(setf red)))
   (slider-green (make-slider "Green" #'(setf green)))
   (slider-blue (make-slider "Blue" #'(setf blue)))
   (rgb-text-edit  :text-editor
                   :value "#00ff00"
                   :value-changed-callback
                   (lambda (pane value)
                     (setf (rgb-text (pane-frame pane)) value))))
  (:layouts
   (default
    (vertically (:equalize-width t)
      (labelling (:label "Select a Color"
                  :text-style (make-text-style :serif :roman :huge)
                  :align-x :center)
        (horizontally (:equalize-height t)
          (vertically (:equalize-width t)
            (horizontally ()
              (labelling (:label "Resulting Color") combined)
              (:fill (labelling (:label "Component Colors")
                       (vertically (:y-spacing 8)
                         (horizontally (:x-spacing 4)
                           red (:fill slider-red))
                         (horizontally (:x-spacing 4)
                           green (:fill slider-green))
                         (horizontally (:x-spacing 4)
                           blue (:fill slider-blue))))))
            (horizontally ()
              +fill+ (make-pane 'push-button
                                :label "Use Color"
                                :activate-callback
                                (lambda (gadget)
                                  (let ((frame (gadget-client gadget)))
                                    (setf (clfm-prompt-result frame)
                                          (make-rgb-color (red frame)
                                                          (green frame)
                                                          (blue frame)))
                                    (frame-exit frame))))))
          (vertically (:equalize-width t)
            (labelling (:label "Use Color") rgb-text-edit)
            (horizontally ()
              +fill+
              (make-pane 'push-button
                         :label "Use Color"
                         :activate-callback
                         (lambda (gadget)
                           (let* ((frame (gadget-client gadget))
                                  (r (parse-string-into-color (rgb-text frame))))
                             (if (typep r 'color)
                                 (progn
                                   (setf (clfm-prompt-result frame) r)
                                   (frame-exit frame))
                                 (notify-user frame
                                              (format nil "~a does not represent a color" r))))))))))))))

(defun update (frame)
  (let ((red (red frame))
        (green (green frame))
        (blue (blue frame)))
    (flet ((update-gadget (gadget-name color)
             (let ((colored (find-pane-named frame gadget-name)))
               (setf (colored-gadget-color colored) color))))
      (update-gadget 'combined (make-rgb-color red green blue))
      (update-gadget 'red (make-rgb-color red 0 0))
      (update-gadget 'green (make-rgb-color 0 green 0))
      (update-gadget 'blue (make-rgb-color 0 0 blue)))))

(defmethod (setf red) :after (new-value (frame color-picker))
  (update frame))

(defmethod (setf green) :after (new-value (frame color-picker))
  (update frame))

(defmethod (setf blue) :after (new-value (frame color-picker))
  (update frame))

(defun pick-color (calling-frame)
  (let ((frame (make-application-frame 'color-picker
                                       :calling-frame calling-frame)))
    (run-frame-top-level frame)
    (return-color frame)))
