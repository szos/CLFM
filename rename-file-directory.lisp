
(in-package :clfm-2)

(defun %rename-file-directory (&optional old-name new-name)
  (restart-case
      (let ((stream (frame-standard-input *application-frame*))
	    oldname newname)
	(accepting-values (stream :own-window t
				  :label (or (and
					      old-name
					      (format nil "Rename ~a" old-name))
					     "Rename a file")
				  :initially-select-query-identifier
				  (or (and old-name 'newfile) 'file))
	  (formatting-table (stream)
	    (or (and old-name (setf oldname old-name))
		(formatting-row (stream)
		  (formatting-column (stream)
		    (formatting-cell (stream)
		      (setf oldname (accept 'string
					    :prompt "Filename   "
					    :stream stream
					    :query-identifier 'file))))))
	    (or (and new-name (setf newname new-name))
		(formatting-row (stream)
		  (formatting-column (stream)
		    (formatting-cell (stream)
		      (setf newname (accept 'string
					    :prompt "Rename to"
					    :stream stream
					    :query-identifier 'newfile))))))))
	(and oldname newname (rename-file oldname newname)))
    (abort () ())))

(define-clfm-command (com-rename) ((thing string) (newname string))
  (rename-file thing newname))

(define-clfm-command (com-rename-this) ((thing string))
  (%rename-file-directory thing))

(define-clfm-command (com-rename-this) ((thing t))
  (%rename-file-directory thing))

(define-clfm-command (com-rename-prompt) ()
  (%rename-file-directory))
