
(in-package :clfm-2)

(defun open-file (file)
  "Opens the provided file with the default application a la xdg-open"
  (check-type file string)
  (uiop:launch-program (format nil "xdg-open ~a" (escape-spaces file))))

(define-clfm-command (com-open-file) ((file string))
  "This command opens a file after checking that it exists"
  (when (uiop:file-exists-p (escape-spaces file))
    (open-file file)))

(define-clfm-command (com-open-file-with) ((file string))
  "Takes a file and prompts for a program to open it with"
  (let (program
	(stream (frame-standard-input *application-frame*))
	;; (x (framex ))
	)
    (restart-case
	(progn
	  (accepting-values (stream :own-window t
				    :label "Enter Program"
				    :initially-select-query-identifier 'program
				    :resynchronize-every-pass t
				    ;; :resize-frame t
				    :height 150
				    :width 500)
	    (formatting-table (stream)
	      (formatting-column (stream)
		(formatting-cell (stream)
		  (setf program (accept 'string
					:prompt "Program"
					:stream stream
					:query-identifier 'program))))))
	  (and program (uiop:launch-program
			(format nil "~a ~a" program (escape-spaces file)))))
      (abort () ()))))
