(in-package :clfm-2)

#+NIL
(defun move-item (item new-item)
  (unless (file-or-directory-exists-p new-item)
    (let ((typeof-item (or (and (uiop:directory-exists-p item) :directory)
			   (and (uiop:file-exists-p item) :file)
			   :nonexistant))
	  (typeof-new-item (or (and (uiop:directory-exists-p item) :directory)
			       (and (uiop:file-exists-p item) :file)
			       :nonexistant)))
      (cond ((eq typeof-item :nonexistant) nil)
	    ((eq typeof-item :directory)
	     (when (eq typeof-new-item :nonexistant)
	       ))
	    ((eq typeof-item :file))))
    (let ((r (reverse (coerce new-file 'list)))
	  newpath)
      (cond ((uiop:directory-exists-p item)
	     (if (and (char= (car r) #\/) ; if it ends in a / and contains other /s
		      (string-contains (coerce (cdr r) 'string) #\/))
		 ;; then move to another directory name
		 (rename-file item )))
	    ((uiop:file-exists-p item)
	     )
	    )
      (if (char= (car r) #\/)
	  (if (string-contains (coerce (cdr r) 'string) #\/)
	      ;; then we want to move it to some other directory
	      nil
	      ;; else we want to move it to something else in the current directory
	      (setf newpath (concatenate 'string (namestring (uiop:getcwd))
					 new-file))))))
  (if (string-contains new-file )))

;; ok.. what we want to do here, is to immitate mv... we could just shell out,
;; but that isnt optimal... 

;; (define-clfm-command )
