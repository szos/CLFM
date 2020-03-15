
(in-package :clfm-2)

(defparameter *marks* nil)

(define-clfm-command (com-add-mark) ((mark string))
  (if (member mark *marks* :test #'string=)
      (setf *marks* (remove mark *marks* :test #'string=))
      (setf *marks* (cons mark *marks*))))

(define-clfm-command (com-add-mark) ((markl t))
  (let ((mark (if (listp markl)
		  (namestring (car markl))
		  markl)))
    (if (member mark *marks* :test #'string=)
	(setf *marks* (remove mark *marks* :test #'string=))
	(setf *marks* (cons mark *marks*)))))

(defun operate-on-marks (operation)
  (let* ((op (symbol-name operation))
	 (function
	   (if (assoc operation *mark-operation-commands*)
	       (cdr (assoc operation *mark-operation-commands*))
	       (let ((reter
		       (clfm-notify (*application-frame* "Unknown Operation"
				     (op)
				     :exit-boxes ((t "Retry")
						  (nil "OK"))
				     :width 300
				     :text-style
				     ("ETBembo" "BoldLF" 18))
			 (with-end-of-line-action (pane :wrap*)
			   (with-etbembo (pane nil 18)
			     (format pane "The operation \"~a\" is unknown"
				     op))))))
		 (when reter
		   (com-operate-on-marks))))))
    (when function
      (loop for thing in *marks*
	    do (handler-case (funcall function thing nil)
		 (t (err)
		   (let ((ret (notify-user *application-frame*
					   (format nil "Error '~a'~%was encountered while running~%function '~a'~%with argument '~a'" err function thing)
					   :exit-boxes
					   '((nil "Continue")
					     (:destroy-marks "Abort")
					     (t
					      "Abort Preserving Marks"))
					   :title "CLFM Mark Error")))
		     (when ret
		       (when (eq :destroy-marks ret)
			 (setf *marks* nil))
		       (return-from operate-on-marks))))))
      (funcall function nil t)
      (setf *marks* nil))))

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
    (operate-on-marks fun)
    ;; (if (assoc fun *mark-operation-commands*)
    ;; 	(operate-on-marks (cdr (assoc fun *mark-operation-commands*)))
    ;; 	(let ((reter
    ;; 		(clfm-notify (*application-frame* "Unknown Operation" (fun)
    ;; 						  :exit-boxes ((t "Retry")
    ;; 							       (nil "OK"))
    ;; 						  :width 300
    ;; 						  :text-style
    ;; 						  ("ETBembo" "BoldLF" 18))
    ;; 		  (with-end-of-line-action (pane :wrap*)
    ;; 		    (with-etbembo (pane nil 18)
    ;; 		      (format pane "The operation \"~a\" is unknown" fun))))))
    ;; 	  (when reter
    ;; 	    (com-operate-on-marks))))
    ))

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
used to operate on marks. Each function must take two arguments - the item to be 
operated upon, and a finished argument. This function will be called on every mark
with with the finished argument being nil. When no marks remain it will be called
one last time with the item being nil and the finished argument being t. The
function that is defined is named after a gensym and cannot be called normally.
When redefining a command the old gensym is uninterned to keep the function
namespace clean."
  (let ((n (gensym (symbol-name name))))
    `(progn
       (unless (replace-alist-entry *mark-operation-commands* ',name ',n t)
	 (setf *mark-operation-commands*
	       (cons (cons ',name ',n) *mark-operation-commands*)))
       (defun ,n ,args ,@body))))

(define-operation delete (item finished)
  (declare (ignore finished))
  (when item
    (let ((reter (clfm-notify (*application-frame* "IRREVERSIBLE OPERATION" nil
						   :exit-boxes ((t "PROCEED")
								(nil "ABORT"))
						   :width 400)
		   (with-end-of-line-action (pane :wrap*)
		     (with-etbembo (pane nil 18)
		       (format pane "You are attempting to delete a file or directory. This operation is ")
		       (with-etbembo (pane :bold 18)
			 (format pane "IRREVERSIBLE"))
		       (format pane ". Proceed with caution."))))))
      (when reter
	(cond ((uiop:directory-exists-p item)
	       (com-delete-directory item))
	      ((uiop:file-exists-p item)
	       (com-rm-file item))
	      (t
	       (clfm-notify (*application-frame* "Not a file or directory" (item)
						 :width 400)
		 (with-etbembo (pane :italic 18)
		   (format pane "~a~%is not an existing file or directory, and cannot be deleted" item)))))))))

(define-operation display (item finished)
  (declare (ignore finished))
  (when item
    (let ((message "The item is: "))
      (test/clfm-notify (*application-frame* message (item)
					     :width 500)
	(with-end-of-line-action (pane :wrap*)
	  (with-etbembo (pane)
	    (format pane "~a" item)))))))

(let (copy-to)
  (define-operation copy (item finished)
    (when finished (setf copy-to nil))
    (when item
      (unless copy-to
	(let ((stream (frame-standard-input *application-frame*)))
	  (accepting-values (stream :own-window t
				    :initially-select-query-identifier 'copy)
	    (formatting-table (stream)
	      (formatting-column (stream)
		(formatting-cell (stream)
		  (setf copy-to
			(accept 'string
				:prompt "Copy To"
				:stream stream
				:query-identifier 'copy))))))))
      (ensure-directories-exist copy-to)
      (if (uiop:file-exists-p item)
	  (%%copy-file item (concatenate 'string copy-to
					   (coerce
					    (reverse
					     (loop for c across (reverse item)
						   while (not (char= c #\/))
						   collect c))
					    'string)))
	  (%copy-directory-contents item copy-to)))))

;; (let (move-to)
;;   (define-operation move (item finished)
;;     (when finished (setf move-to nil))
;;     (when item
;;       (unless move-to
;; 	(let ((stream (frame-standard-input *application-frame*)))
;; 	  (accepting-values (stream :own-window t
;; 				    :initially-select-query-identifier 'move)
;; 	    (formatting-table (stream)
;; 	      (formatting-column (stream)
;; 		(formatting-cell (stream)
;; 		  (setf move-to
;; 			(accept 'string
;; 				:prompt "Move To"
;; 				:stream stream
;; 				:query-identifier 'move))))))))
;;       (com-move-to ))
;;     ))
