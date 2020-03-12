
(in-package :clfm-2)

(defparameter *passwd-test* nil)

(define-clfm-command (com-change-directory) ((directory string))
  (handler-case 
      (when (uiop:directory-exists-p directory)
	(uiop:chdir directory))
    (sb-posix:syscall-error (err)
      (case 
	  (notify-user *application-frame*
		       (let ((errno (sb-posix:syscall-errno err)))
			 (cond ((= errno 13)
				"You don't have permission to access this directory

To access this directory CLFM needs to be running as root

To relaunch CLFM please ensure it is in your $PATH")
			       (t
				(format nil "SB-POSIX Error Code: ~a" errno))))
		       :title "CLFM Error"
		       :name "CLFM Error"
		       :documentation "CLFM Error"
		       :text-style (make-text-style "ETBembo" "BoldLF" 18)
		       :exit-boxes '((t "OK")
				     (:sudo "Relaunch with sudo")
				     (:root "Relaunch as root")))
	((:sudo)
	 (and (notify-user *application-frame*
			   "WARNING: CLFM will now be running with root privilidges. 
This is not safe, and should be avoided. 

If you continue, type your password when presented with the blank window and use 
M-RET (meta + return or Alt + return) to submit it"
			   :exit-boxes '((t "CONTINUE") (nil "ABORT")))
	      (let (passwd
		    (stream (frame-standard-input *application-frame*)))
		(accepting-values (stream
				   :own-window t
				   :initially-select-query-identifier 'password)
		  (formatting-table (stream)
		    (formatting-column (stream)
		      (formatting-cell (stream)
			(setf passwd (accept 'my-password
					     ;; 'string
					     :prompt "sudo password"
					     :stream stream
					     :query-identifier 'password))))))
		;; (uiop:launch-program (format nil "echo ~a | sudo -S clfm" passwd))
		(setf *passwd-test* passwd))))
	((root)
	 )))))

(define-clfm-command (com-chdir-~/ :name "Go Home") ()
  (com-change-directory "~/"))


