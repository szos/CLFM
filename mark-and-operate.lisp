
(in-package :clfm-2)

(defparameter *marks* nil)

(define-clfm-command (com-add-mark) ((mark string))
  (setf *marks* (cons mark *marks*)))

(defun operate-on-marks (function)
  (loop for thing in *marks*
	do (handler-case (funcall function thing)
	     (t (err)
	       (let ((ret (notify-user *application-frame*
				       (format nil "Error '~a'~%was encountered while running~%function '~a'~%with argument '~a'" err function thing)
				       :exit-boxes '((nil "Continue")
						     (:destroy-marks "Abort")
						     (t
						      "Abort Preserving Marks"))
				       :title "CLFM Mark Error")))
		 (when ret
		   (when (eq :destroy-marks ret)
		     (setf *marks* nil))
		   (return-from operate-on-marks))))))
  (setf *marks* nil))

(define-clfm-command (com-operate-on-marks) ()
  ;; ((function 'symbol :prompt "Function"))
  (let (fun
	(stream (frame-standard-input *application-frame*)))
    (accepting-values (stream
		       :own-window t
		       :initially-select-query-identifier 'function)
      (formatting-table (stream)
	(formatting-column (stream)
	  (formatting-cell (stream)
	    (setf fun (accept 'symbol
			      :prompt "Operate Using Function"
			      :stream stream
			      :query-identifier 'function))))))
    (operate-on-marks fun)))
