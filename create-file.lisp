
(in-package :clfm-2)

(defparameter *touch-file* nil)

(define-clfm-command (com-touch) ((filename string))
  (let ((file (if (string-contains filename "/")
		  filename
		  (concatenate 'string (namestring (uiop:getcwd)) filename))))
    (open file :direction :probe :if-does-not-exist :create)))

(define-clfm-command (com-new-file) ()
  (restart-case 
      (let (file
	    (stream (frame-standard-input *application-frame*)))
	(accepting-values (stream :own-window t
				  :label "Enter a new file name"
				  :initially-select-query-identifier 'fileprompt)
	  (formatting-table (stream)
	    (formatting-column (stream)
	      (formatting-cell (stream)
		(setf file (accept 'string
				   :prompt "Filename"
				   :stream stream
				   :query-identifier 'fileprompt))))))
	(and file (com-touch file)))
    (abort () ())))
