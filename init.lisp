;;; This is an example init file, which has some basic configuration stuff.

(in-package :clfm-2)

(define-operation notify-me (item finished)
  (declare (ignore finished))
  (when item
    (clfm-notify (*application-frame* "A Notification" (item))
      (format pane "This is a notificaton of an item were operating on.")
      (print item pane))))

(file-prefer-marks)
