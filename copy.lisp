
(in-package :clfm-2)

(defun %copy-file (&optional file new-file)
  (restart-case
      (let ((stream (frame-standard-input *application-frame*))
	    oldname newname)
	(accepting-values (stream :own-window t
				  :label (or (and
					      file
					      (format nil "Copy ~a" file))
					     "Copy a file")
				  :initially-select-query-identifier
				  (or (and file 'newfile) 'file))
	  (accept-with-table (stream)
	    (or (and file (setf oldname file))
		(setf oldname (accept 'string
				      :prompt "Filename"
				      :stream stream
				      :query-identifier 'file)))
	    (or (and new-file (setf newname new-file))
		(setf newname (accept 'string
				      :prompt "Copy to"
				      :stream stream
				      :query-identifier 'newfile)))))
	(unless (member #\/ (coerce newname 'list))
	  (setf newname (concatenate 'string
				     (namestring (uiop:getcwd))
				     newname)))
	(and oldname newname
	     (with-open-file (infile oldname :direction :input
					     :element-type '(unsigned-byte 8))
	       (with-open-file (outfile newname :direction :output
						:element-type '(unsigned-byte 8)
						:if-exists :error)
		 (when infile
		   (loop for byte = (read-byte infile nil)
			 while byte
			 do (write-byte byte outfile)))))))
    (abort () ())))

(define-clfm-command (com-copy-file) ((file string) (copy-to string))
  (%copy-file file copy-to))

(define-clfm-command (com-copy-file-prompt) ((file string))
  (%copy-file file))
