
(in-package :clfm-2)

(defun %new-directory (&optional redo)
  (restart-case
      (let (dir
	    (stream (frame-standard-input *application-frame*)))
	(accepting-values (stream :own-window t
				  :label (or (and redo "That didnt appear to be a directory, please try again, and ensure theres a trailing “/”. ")
					     "Enter a new directory name")
				  :initially-select-query-identifier 'dirprompt)
	  (formatting-table (stream)
	    (formatting-column (stream)
	      (formatting-cell (stream)
		(setf dir (accept 'string
				  :prompt "Directory"
				  :stream stream
				  :query-identifier 'dirprompt))))))
	(and dir (com-make-directory dir)))
    (abort () ())))

(define-clfm-command (com-new-dir) ()
  (%new-directory))

(define-clfm-command (com-make-directory) ((dir 'string))
  (let ((fd (coerce dir 'list))
	(rd (reverse (coerce dir 'list))))
    (if (char= #\/ (car rd))
	(if (or (char= (car fd) #\/)
		(char= (car fd) #\~))
	    (ensure-directories-exist dir)
	    (ensure-directories-exist
	     (concatenate 'string (namestring (uiop:getcwd)) dir)))
	(%new-directory t))))
