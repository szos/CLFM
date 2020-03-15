
(in-package :clfm-2)

(defun %copy-file (f1 f2)
  (with-open-file (infile f1 :direction :input
			     :element-type '(unsigned-byte 8))
    (with-open-file (outfile f2 :direction :output
				:element-type '(unsigned-byte 8)
				:if-exists :error)
      (when infile
	(loop for byte = (read-byte infile nil)
	      while byte
	      do (write-byte byte outfile))))))

(defun copy-file (&optional file new-file)
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
	(and oldname newname (%copy-file oldname newname)))
    (abort () ())))

(define-clfm-command (com-copy-file) ((file string) (copy-to string))
  (copy-file file copy-to))

(define-clfm-command (com-copy-file-prompt) ((file string))
  (copy-file file))

(defun %copy-directory-contents (from to)
  "copy the contents of from to to. This is a helper"
  (ensure-directories-exist to)
  (labels
      ((copier (tdir fdir list)
	 (print fdir)
	 (print list)
	 (cond ((uiop:directory-exists-p (car list))
		(let ((dirname
			(coerce
			 (reverse
			  (loop for c in (cdr (coerce (reverse
						       (namestring (car list)))
						      'list))
				while (not (char= c #\/))
				collect c))
			 'string)))
		  (ensure-directories-exist (concatenate 'string
							 (namestring tdir)
							 dirname "/"))
		  (copier (concatenate 'string (namestring tdir) dirname "/")
			  (concatenate 'string (namestring fdir) dirname "/")
			  (append (uiop:subdirectories
				   (pathname
				    (concatenate 'string (namestring fdir)
						 dirname "/")))
				  (uiop:directory-files
				   (pathname
				    (concatenate 'string (namestring fdir)
						 dirname "/")))))
		  (copier tdir fdir (cdr list))))
	       ((uiop:file-exists-p (car list))
		(let ((cto
			(coerce
			 (reverse
			  (loop for c in (coerce (reverse (namestring (car list)))
						 'list)
				while (not (char= c #\/))
				collect c))
			 'string)))
		  (%copy-file (car list) (concatenate 'string (namestring tdir)
						       cto))
		  (copier tdir fdir (cdr list)))))))
    (copier to from (append (uiop:subdirectories (pathname from))
			    (uiop:directory-files (pathname from))))))

(defun copy-directory (old-dir new-dir)
  (let (ndir
	(n (cdr (reverse (coerce (namestring new-dir) 'list)))))
    (setf ndir
	  (if (member #\/ n :test #'char=)
	      ;; TODO: 
	      ;; if new-dir is an absolute path... no thats not what this does.
	      ;; this checks if theres any other directories in the path...
	      ;; so it would fail if we were in ~/docs/ and copying from
	      ;; ~/docs/lisp to scheme/docs/new. This is not the behavior we 
	      ;; want, this needs to be reworked to allow relative paths with
	      ;; subdirectories
	      new-dir
	      (concatenate 'string (namestring (uiop:getcwd))
			   (namestring new-dir))))
    (ensure-directories-exist ndir)
    (%copy-directory-contents old-dir ndir)))

(defun test/copy-directory (&optional old new)
  (restart-case
      (let ((stream (frame-standard-input *application-frame*))
	    oldname newname)
	(accepting-values (stream :own-window t
				  :label (or (and
					      old
					      (format nil "Copy ~a" old))
					     "Copy a directory")
				  :initially-select-query-identifier
				  (or (and old 'newname) 'directory))
	  (accept-with-table (stream)
	    (or (and old (setf oldname old))
		(setf oldname (accept 'string
				      :prompt "Directory"
				      :stream stream
				      :query-identifier 'directory)))
	    (or (and new (setf newname new))
		(setf newname (accept 'string
				      :prompt "Copy to"
				      :stream stream
				      :query-identifier 'newname)))))
	(ensure-directories-exist newname)
	(%copy-directory-contents oldname newname))
    (abort () ())))

(define-clfm-command (com-copy-directory) ((file string) (copy-to string))
  (test/copy-directory file copy-to))

(define-clfm-command (com-copy-directory-prompt) ((file string))
  (test/copy-directory file))



