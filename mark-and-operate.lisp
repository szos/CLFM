
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
    (operate-on-marks (cdr (assoc fun *mark-operation-commands*)))))

;;; IDEA:
;;; Make a «defcommand» type thing and an accompanying accept type to use which
;;; searches through all commands and then calls one that matches, searching as
;;; strings! - this would mean that in the command com-operate-on-marks, we first
;;; accept a string or symbol, and then search in our faux-database for it, and
;;; call the associated function!

(defparameter *mark-operation-commands* nil)

(defun replace-alist-entry (alist key value &optional unintern-value)
  (alexandria:when-let ((k (assoc key alist)))
    (when unintern-value
      (unintern (cdr k)))
    (setf (cdr k) value)))

(defmacro define-operation (name args &body body)
  "This macro defines a function and stores it in an alist. these functions can be
used when operating on marks, and as such must take one variable, the marked file
or directory to operate upon. The function that is defined is named after a gensym
and cannot be called normally. When redefining a command the old gensym is 
uninterned to keep the function namespace clean."
  (let ((n (gensym (symbol-name name))))
    `(progn
       (unless (replace-alist-entry *mark-operation-commands* ',name ',n t)
	 (setf *mark-operation-commands*
	       (cons (cons ',name ',n) *mark-operation-commands*)))
       (defun ,n ,args ,@body))))

(define-operation rm (item)
  (cond ((uiop:directory-exists-p item)
	 (com-delete-directory item))
	((uiop:file-exists-p item)
	 (com-rm-file item))))

(define-operation delete (item)
  (cond ((uiop:directory-exists-p item)
	 (com-delete-directory item))
	((uiop:file-exists-p item)
	 (com-rm-file item))
	(t
	 (notify-user
	  *application-frame*
	  (format nil "~a~%is not an existing file or directory, and cannot be deleted" item)
	  :exit-boxes '((t "OK"))
	  :text-style (make-text-style "ETBembo" "SemiBoldOSF" 18)))))

(define-operation display-item (item)
  (notify-user *application-frame*
	       (format nil "Displaying the item ~a" item)
	       :exit-boxes '((t "OK"))
	       :text-style (make-text-style "ETBembo" "SemiBoldOSF" 18)))
