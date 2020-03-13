;;;; clfm-2.lisp

(in-package #:clfm-2)

(defparameter *hide-files* t)

(defmacro nlet (name vars &body body)
  (let ((gensym-list (loop for x in vars collect (gensym)))
	(name-gensym (gensym "Name"))
	(block-gensym (gensym "Block")))
    `(macrolet
	 ((,name ,gensym-list
	    `(progn
	       (psetq
		,@(loop for r in ',vars
			for l in (list ,@gensym-list)
			collect (car r)
			collect l))
	       (go ,',name-gensym))))
       (block ,block-gensym
	 (let ,vars
	   (tagbody
	      ,name-gensym
	      (return-from ,block-gensym (progn ,@body))))))))

(defmacro with-etbembo ((stream &optional type (size 16)) &body body)
  `(with-text-style (,stream (make-text-style "ETBembo"
					      (cond ((eq ,type :bold)
						     "BoldLF")
						    ((eq ,type :semi-bold)
						     "SemiBoldOSF")
						    ((eq ,type :italic)
						     "DisplayItalic")
						    ((eq ,type :italic-bold)
						     "DisplayItalicBold")
						    (t "RomanLF"))
					      ,size))
     ,@body))

(defun make-executable ()
  (sb-ext:save-lisp-and-die "clfm"
			    :toplevel (lambda ()
					(app-main))
			    :executable t
			    :purify t))

(defun file-extension (file)
  (let ((r (reverse (car (cl-ppcre:split "\\." (reverse (if (pathnamep file)
							    (format nil "~a" file)
							    file)))))))
    r))

(defun initialize-fonts ()
  (loop for f in (uiop:directory-files (asdf:system-relative-pathname
					"clfm-2" "fonts/" :type :directory))
	when (string= "ttf" (file-extension f))
	  do ;; (format t "Font: ~a" f)
	     (loop for size in '(8 10 12 14 16 18 24 48 72)
		   do (mcclim-truetype::make-truetype-font (find-port) f size))))

(defun gather-users-uid-gid ()
  (let ((uid-lists '())
	(gid-lists '())
	(un (uiop:process-info-output (uiop:launch-program "awk -F: '{print $1}' /etc/passwd" :output :stream)))
	(ui (uiop:process-info-output (uiop:launch-program "awk -F: '{print $3}' /etc/passwd" :output :stream)))
	(gi (uiop:process-info-output (uiop:launch-program "awk -F: '{print $4}' /etc/passwd" :output :stream))))
    (do ((n (read-line un nil)
	    (read-line un nil))
	 (u (read-line ui nil)
	    (read-line ui nil))
	 (g (read-line gi nil)
	    (read-line gi nil)))
	((or (null n) (null u) (null g)))
      (setf uid-lists (append uid-lists (list (cons u n))))
      (setf gid-lists (append gid-lists (list (cons g n)))))
    (values uid-lists gid-lists)))

(multiple-value-bind (uidl gidl)
    (gather-users-uid-gid)
  (defparameter *uid-username* uidl)
  (defparameter *gid-username* gidl))

;;; The variable +clfm-permissions+ and the function permissions-as-string are a
;;; modified version of the varables and functions in osicat. these modified
;;; versions return a string instead of a list of keywords to show permissions

(defvar +clfm-permissions+ '(("r" . #.nix:s-irusr)
			     ("w" . #.nix:s-iwusr)
			     ("x" . #.nix:s-ixusr)
			     ("r" . #.nix:s-irgrp)
			     ("w" . #.nix:s-iwgrp)
			     ("x" . #.nix:s-ixgrp)
			     ("r" . #.nix:s-iroth)
			     ("w" . #.nix:s-iwoth)
			     ("x" . #.nix:s-ixoth))
  "for use in permissions-as-string, used to show permissions like ls -l")

(defun permissions-as-string (pathname)
  (apply 'concatenate 'string
	 (let ((mode (nix:stat-mode (nix:stat pathname))))
	   (loop for (name . value) in +clfm-permissions+
		 if (plusp (logand mode value))
		   collect name
		 else collect "-"))))

(defun hidden-pathname-p (path)
  (let ((pathname (namestring path)))
    (uiop:hidden-pathname-p
     (if (string= "/" (subseq pathname (- (length pathname) 1) (length pathname)))
	 (subseq pathname 0 (- (length pathname) 1))
	 pathname))))

(defun list-directory (directory)
  (let ((contents (osicat:list-directory directory)))
    (loop for path in contents
	  unless (hidden-pathname-p path)
	    collect (let ((stat (osicat-posix:stat path)))
		      (list path
                            (cdr (assoc
				  (format nil "~a" (osicat-posix:stat-uid stat))
				  *uid-username* :test #'string-equal))
			    (cdr (assoc
				  (format nil "~a" (osicat-posix:stat-gid stat))
				  *gid-username* :test #'string-equal))
			    (permissions-as-string path))))))

(define-application-frame clfm () ()
  (:menu-bar clfm-menu-bar)
  (:top-level (clim:default-frame-top-level :prompt #'commander-prompt))
  (:panes
   (info :application
	 :display-function #'display-info
	 :width 600)
   (current-directory :application
		      :display-function #'display-current-directory)
   (interactor :interactor))
  (:layouts
   (default
    (vertically ()
      (20 info)
      (horizontally ()
	(:fill current-directory))
      (make-pane 'clim-extensions:box-adjuster-gadget)
      (100 interactor)))))

(defun commander-prompt (pane frame)
  (declare (ignore frame))
  (format pane "COMMAND: ")
  (stream-increment-cursor-position pane 5 0))

(defun app-main ()
  (initialize-fonts)
  (handler-case (load "~/.clfm.d/init.lisp")
    (t () ()))
  (run-frame-top-level (make-application-frame 'clfm)))

(defun path> (one two)
  (string> (namestring one) (namestring two)))

(defun path< (one two)
  (string< (namestring one) (namestring two)))

(defun file/directory-name (path)
  (car (last (cl-ppcre:split "/" path))))

(defun display-info (frame pane)
  (declare (ignore frame))
  (labels ((looper (input &optional ac)
	     (if input
		 (progn
		   (with-output-as-presentation
		       (pane (format nil "~{~a~^/~}/"
				     (append ac (list (car input))))
			     'chdir-presentation)
		     (format pane "~a/" (car input)))
		   (looper (cdr input) (append ac (list (car input)))))
		 (unless ac
		   (with-output-as-presentation (pane "/"
						      'chdir-presentation)
		     (format pane "/"))))))
    (with-etbembo (pane :bold)
      (format pane "Current Directory: "))
    (with-etbembo (pane :italic-bold)
      (looper (cl-ppcre:split "/" (namestring (uiop:getcwd)))))))

;; 📁 is the folder uncode codepoint

(defun display-current-directory (frame pane)
  (declare (ignore frame))
  (with-end-of-line-action (pane :scroll)
    (slim:with-table (pane)
      (with-etbembo (pane :bold)
	(slim:row
	  (slim:cell (format pane "Type")) (slim:cell (format pane "Name"))
	  (slim:cell (format pane "User")) (slim:cell (format pane "Group"))
	  (slim:cell (format pane "Permissions"))))
      (with-output-as-presentation (pane (concatenate 'string
						      (namestring (uiop:getcwd))
						      "..")
					 'chdir-presentation
					 :single-box t)
	(let* ((path (concatenate 'string (namestring (uiop:getcwd)) ".."))
	       (stat (osicat-posix:stat path)))
	  (slim:row
	    (with-etbembo (pane)
	      (with-drawing-options (pane :ink +blue+)
		(slim:cell (format pane "Dir")))
	      (slim:cell (format pane "Up One Directory"))
	      (slim:cell (format pane "~a"
				 (cdr
				  (assoc
				   (format nil "~a" (osicat-posix:stat-uid stat))
				   *uid-username* :test #'string-equal))))
	      (slim:cell (format pane "~a"
				 (cdr
				  (assoc
				   (format nil "~a" (osicat-posix:stat-gid stat))
				   *gid-username* :test #'string-equal)))))
	    (slim:cell (format pane "~a" (permissions-as-string path))))))
      (let ((contents (sort (uiop:subdirectories (uiop:getcwd)) #'path<)))
	(loop for path in contents
	      unless (and *hide-files* (hidden-pathname-p path))
		do (let ((stat (handler-case (osicat-posix:stat path)
				 (t () nil))))
		     (with-output-as-presentation (pane
						   (namestring path)
						   'chdir-presentation
						   :single-box t)
		       (slim:row
			 (with-drawing-options
			     (pane :ink (if (member (namestring path) *marks*
						    :test #'string=)
					    +orange-red+ +black+))
			   (with-text-style
			       (pane (make-text-style
				      "ETBembo" (if (member (namestring path)
							    *marks*
							    :test #'string=)
						    "DisplayItalicBold" "RomanLF")
				      16))
			     (slim:cell
			       (with-drawing-options
				   (pane :ink (if (member (namestring path)
							  *marks* :test #'string=)
						  +orange-red+ +blue+))
				 (format pane "Dir")))
			     (slim:cell (format pane "~a" (file/directory-name
							   (namestring path))))
			     (slim:cell
			       (format pane "~a"
				       (cdr
					(assoc
					 (format nil "~a"
						 (and stat
						      (osicat-posix:stat-uid stat)))
					 *uid-username* :test #'string-equal))))
			     (slim:cell
			       (format pane "~a"
				       (cdr
					(assoc
					 (format nil "~a"
						 (and stat
						      (osicat-posix:stat-gid stat)))
					 *gid-username* :test #'string-equal)))))
			   
			   (slim:cell
			     (format pane "~a"
				     (and stat (permissions-as-string
						(namestring path)))))))))))
      (let ((contents (sort (uiop:directory-files (uiop:getcwd)) #'path<)))
	(loop for path in contents
	      unless (and *hide-files* (hidden-pathname-p path))
		do (let ((stat (handler-case (osicat-posix:stat path)
				 (t () nil))))
		     (with-output-as-presentation (pane
						   (namestring path)
						   'fopen-presentation
						   :single-box t)
		       (slim:row
			 (with-drawing-options
			     (pane :ink (if (member (namestring path) *marks*
						    :test #'string=)
					    +orange-red+ +black+))
			   (with-text-style
			       (pane (make-text-style
				      "ETBembo" (if (member (namestring path)
							    *marks*
							    :test #'string=)
						    "DisplayItalicBold" "RomanLF")
				      16))
			     (slim:cell
			       (with-drawing-options
				   (pane :ink (if (member (namestring path)
							  *marks* :test #'string=)
						  +orange-red+ +green4+))
				 (format pane "File")))
			     (slim:cell (format pane "~a" (file/directory-name
							   (namestring path))))
			     (slim:cell
			       (format pane "~a"
				       (cdr
					(assoc
					 (format nil "~a"
						 (and stat
						      (osicat-posix:stat-uid stat)))
					 *uid-username* :test #'string-equal))))
			     (slim:cell
			       (format pane "~a"
				       (cdr
					(assoc
					 (format nil "~a"
						 (and stat
						      (osicat-posix:stat-gid stat)))
					 *gid-username* :test #'string-equal)))))
			   (slim:cell
			     (format pane "~a"
				     (and stat (permissions-as-string path)))))))))))))
