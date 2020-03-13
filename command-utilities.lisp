
(in-package :clfm-2)

(defmacro accept-with-table ((stream) &body accept-statements)
  "this simple macro wraps every accept statement in a formatting table, column,
and cell. "
  `(formatting-table (,stream)
     ,@(loop for item in accept-statements
	     collect `(formatting-column (,stream)
			(formatting-cell (,stream)
			  ,item)))))

(defmacro make-popup ((stream label) &body body)
  `(restart-case
       (let ((stream ,stream))
	 (accepting-values (stream :own-window t
				   :label ,label)
	   ,@body
           (terpri stream)
	   (terpri stream)
	   (accept 'string
		   :prompt "DUMMY INPUT DO NOT USE"
		   :stream stream)))
     (abort () ())))

(defun file-or-directory-exists-p (item)
  (or (uiop:file-exists-p item)
      (uiop:directory-exists-p)))

(defun escape-spaces (string)
  (coerce (loop for char in (coerce string 'list)
		if (char= #\space char)
		  collect #\\
		  and collect #\space
		else collect char)
	  'string))

(defun test/escape-spaces (string)
  (coerce (loop for char in (coerce string 'list)
		if (char= #\space char)
		  collect #\\
		  and collect #\space
		else collect char)
	  'string))

;; (nlet churner ((charlist '(#\\ #\space #\\ ))
;; 	       (counter 1)) ; count down
;;   (cond ((not (char=  #\\ (car charlist)))
;; 	 counter)
;; 	(t
;; 	 (if (= counter 0)
;; 	     (churner (cdr charlist) 1)
;; 	     (churner (cdr charlist) 0)))))

;; (defun spaces-escaped (string)
;;   (nlet spaceloop ((s (coerce string 'list))
;; 		   (previous-chars '()))
;;     (if (char= (car s) #\space)
;; 	(if (and (char= (car previous-chars) #\\)
;; 		 (char= (cadr previous-chars #\\)))
;; 	    ))
;;     (cond ((char= (car s) #\\)
;; 	   ))))

(defun string-contains (string character)
  (let ((char (if (stringp character) (coerce character 'character) character)))
    (loop for c across string
	  when (char= c char)
	    do (return-from string-contains t))))

(define-clfm-command (com-run-shell-command :name "Run Shell Command")
    ((command string :prompt "/bin/sh -c"))
  (uiop:launch-program (format nil "/bin/sh -c ~a" command)))

(define-clfm-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))
(define-clfm-command (com-exit :name "Exit") ()
  (frame-exit *application-frame*))

(define-clfm-command (com-display-help-window) ()
  (let ((frame (make-application-frame 'clfm-help)))
    (run-frame-top-level frame)))

(define-clfm-command (com-loadrc :name "Load Init File") ()
  (handler-case (load "~/.clfm.d/init.lisp")
    (t () ())))

(define-clfm-command (com-show-marks) ()
  (setf (frame-current-layout *application-frame*) 'default))

(define-clfm-command (com-hide-marks) ()
  (setf (frame-current-layout *application-frame*) 'no-marks))
