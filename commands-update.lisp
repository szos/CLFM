(in-package :clfm-2)

(defparameter *override-characters-as-string*
  '(("RET" . :return)
    ("SPC" . #\space)
    ("TAB" . #\tab)
    ("æ" . #\latin_small_letter_ae)
    ("ø" . #\LATIN_SMALL_LETTER_O_WITH_STROKE)
    ("å" . #\LATIN_SMALL_LETTER_A_WITH_RING_ABOVE)
    ("ł" . #\LATIN_SMALL_LETTER_L_WITH_STROKE)))

(defun key-chord (chord-def)
  (let* ((elements (reverse (cl-ppcre:split "-" chord-def)))
	 (char (pop elements)))
    (setf char
	  (if (assoc char *override-characters-as-string* :test #'string-equal)
	      (cdr (assoc char *override-characters-as-string*
			   :test #'string-equal))
	      (coerce char 'character)))
    (cons char (remove-if #'not (loop for s in elements
				      collect (cond ((string= s "S")
						     (setf char (char-upcase char))
						     nil)
						    ((string= s "C") :control)
						    ((string= s "M") :Meta)
						    ((string= s "s") :super)
						    ((string= s "H") :Hyper)))))))

(defun kbd (key-seq)
  "This function parses a string into a list of gestures. The string is split by 
spaces, and every one of these is parsed into a single gesture. the final character
is the character, all others are modifiers, and hyphens are ignored. Modifier 
translation is as follows: C - :control, M - :meta, H - :hyper, s - :super. 
a capital S in the modifier position changes the character to a capital. "
  (let* ((str-seq (cl-ppcre:split " " key-seq))
	 (seq (loop for str in str-seq
		    collect
		    (let* ((charbag (reverse (coerce str 'list)))
			   (char (pop charbag))
			   (mods (loop for c in charbag
				       unless (char= c #\-)
					 collect (cond ((char= c #\S)
							(setf
							 char
							 (char-upcase char))
							nil)
						       ((char= c #\C) :control)
						       ((char= c #\M) :Meta)
						       ((char= c #\s) :super)
						       ((char= c #\H) :Hyper)))))
		      (cons char (remove-if #'not mods))))))
    seq))

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
      (looper (cl-ppcre:split "/" (namestring (uiop:getcwd))))
      (format pane "~a"
	      (car (reverse (cl-ppcre:split "/" (namestring (current-item)))))))))

(define-clfm-command (com-change-directory-temper) ((directory string))
  (handler-case 
      (when (uiop:directory-exists-p directory)
	(change-directory directory))
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
				     (:sudo "Relaunch as root")))
	((:sudo)
	 (and (notify-user *application-frame*
			   "WARNING: CLFM will now be running with root privilidges. 
This is not safe, and should be avoided. 

If you continue, type your password when presented with the blank window and use 
M-RET (meta + return or Alt + return) to submit it"
			   :exit-boxes '((t "CONTINUE") (nil "ABORT"))
			   :text-style (make-text-style "ETBembo" "BoldLF" 18))
	      (let (passwd
		    (stream (frame-standard-input *application-frame*)))
		(accepting-values (stream
				   :own-window t
				   :initially-select-query-identifier 'password)
		  (formatting-table (stream)
		    (formatting-column (stream)
		      (formatting-cell (stream)
			(setf passwd (accept 'password
					     ;; 'string
					     :prompt "sudo password"
					     :stream stream
					     :query-identifier 'password))))))
		(uiop:launch-program
		 (format nil "echo \"~a\" | sudo -S clfm" passwd)))))))))

(define-clfm-command (com-add-mark) ((markl t))
  (let ((mark (if (listp markl)
		  (namestring (car markl))
		  markl)))
    (if (member mark *marks* :test #'string=)
	(setf *marks* (remove mark *marks* :test #'string=))
	(setf *marks* (cons mark *marks*)))))

(define-clfm-command (com-delete) ((thing t))
  (let ((item (if (listp thing)
		  (namestring (car thing))
		  thing)))
    (cond ((uiop:directory-exists-p item)
	   (delete-directory item))
	  ((uiop:file-exists-p item)
	   (delete-item item)))))

(define-clfm-command (com-open) ((thing t))
  (let ((item (if (listp thing)
		  (namestring (car thing))
		  thing)))
    (cond ((uiop:directory-exists-p item)
	   (com-change-directory-temper item))
	  ((uiop:file-exists-p item)
	   (open-file (namestring item))))))

(define-clfm-command (com-open-with) ((thing t))
  (let (program
	(item (if (listp thing)
		  (namestring (car thing))
		  thing)))
    (cond ((uiop:file-exists-p item)
	   (restart-case
	       (progn
		 (accepting-values (stream :own-window t
					   :label "Enter Program"
					   :initially-select-query-identifier
					   'program
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
			       (format nil "~a ~a" program (escape-spaces item)))))
	     (abort () ()))) )))

(define-gesture-name :meta-click-left :pointer-button (:left :meta))

(define-gesture-name :meta-control :keyboard (:control :meta))

(define-gesture-name :next-item :keyboard (#\n :control))
(define-gesture-name :prev-item :keyboard (#\p :control))
(define-gesture-name :mark-item :keyboard (#\m :control))

(define-presentation-type filesystem-presentation ())

(defun set-operation-order (&key (delete -1)
			      (open 4)
			      (open-with 3)
			      (rename 2)
			      (copy 1))
  (define-presentation-to-command-translator up-selection
      (filesystem-presentation com-move-up clfm
       :gesture :control-p
       :documentation "Up one selection")
      (item)
    (list item))
  (define-presentation-to-command-translator down-selection
      (filesystem-presentation com-move-down clfm
       :gesture :control-n
       :documentation "Down one selection")
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-mark
      (filesystem-presentation com-add-mark clfm
       :gesture :meta-click-left
       :documentation "Mark")
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-delete
      (filesystem-presentation com-delete clfm
       :gesture :select
       :documentation "Delete (PERMANENT)"
       :priority delete)
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-open
      (filesystem-presentation com-open clfm
       :gesture :select
       :documentation "Open"
       :priority open)
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-open-with
      (filesystem-presentation com-open-with clfm
       :gesture :select
       :documentation "Open With"
       :priority open-with
       :tester ((item) (uiop:file-exists-p (if (listp item)
					       (namestring (car item))
					       item))))
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-rename
      (filesystem-presentation com-rename clfm
       :gesture :select
       :documentation "Rename"
       :priority rename)
      (item)
    (list item))
  (define-presentation-to-command-translator filesystem-copy
      (filesystem-presentation com-copy clfm
       :gesture :select
       :documentation "Copy"
       :priority copy)
      (item)
    (list item)))

(set-operation-order)

(defun pane-scrolled-to-bottom-p (pane)
  (multiple-value-bind (x y) (transform-position (sheet-transformation pane)
                                                 0 0)
    (declare (ignore x))
    (with-bounding-rectangle* (x1 y1 x2 y2) pane
      (declare (ignore x1 y1 x2))
      (with-bounding-rectangle* (ax1 ay1 ax2 ay2) (sheet-parent pane)
        (declare (ignore ax1 ay1 ax2))
        (<= (+ y y2) ay2)))))

(defun scroll-pane-to-bottom (pane)
  (scroll-extent pane 0 (max 0 (- (bounding-rectangle-height pane)
                                  (bounding-rectangle-height
				   (sheet-parent pane))))))

(defmacro with-pane-kept-scrolled-to-bottom ((pane-form) &body body)
  "Ensure that the pane in PANE-FORM has the same scroll state
after BODY terminates as it had before:

If the pane is scrolled to some position before the end, it is
kept there.  If the pane is at the bottom of the pane, the
viewport is reset to the then-current bottom after BODY is
finished."
  (let ((pane (gensym))
        (bottom-p (gensym)))
    `(let* ((,pane ,pane-form)
            (,bottom-p (pane-scrolled-to-bottom-p ,pane)))
       (multiple-value-prog1 (progn ,@body)
	 (when ,bottom-p (scroll-pane-to-bottom ,pane))))))

(let (pre-selection-contents
      (selected-item
	(let ((all (append (sort (uiop:subdirectories (uiop:getcwd)) #'path<)
			   (sort (uiop:directory-files (uiop:getcwd)) #'path<))))
	  (if *hide-files*
	      (car (remove-if #'hidden-pathname-p all))
	      (car all))))
      (post-selection-contents
	(let ((all (append (sort (uiop:subdirectories (uiop:getcwd)) #'path<)
			   (sort (uiop:directory-files (uiop:getcwd)) #'path<))))
	  (if *hide-files*
              (cdr (remove-if #'hidden-pathname-p all))
	      (cdr all)))))
  (defun current-item () selected-item)
  (defun add-current-selected-to-marks ()
    (com-add-mark (namestring selected-item)))
  (defun enter-or-open-current-selection ()
    (com-open selected-item))
  (defun change-directory (directory)
    (uiop:chdir directory)
    (let ((contents
	    (append (sort (uiop:subdirectories (uiop:getcwd)) #'path<)
		    (sort (uiop:directory-files (uiop:getcwd)) #'path<))))
      (setf pre-selection-contents nil
	    selected-item (if *hide-files*
			      (car (remove-if #'hidden-pathname-p contents))
			      (car contents))
	    post-selection-contents (if *hide-files*
					(cdr (remove-if #'hidden-pathname-p
							contents))
					(cdr contents))))
    (redisplay-frame-panes *application-frame* :force-p t))
  (defun move-selection-up ()
    (when pre-selection-contents
      (setf post-selection-contents (cons selected-item post-selection-contents))
      (setf selected-item (pop pre-selection-contents))))
  (defun move-selection-down ()
    (when post-selection-contents
      (setf pre-selection-contents (cons selected-item pre-selection-contents))
      (setf selected-item (pop post-selection-contents))))
  (defun display-current-directory (frame pane)
    (declare (ignore frame))
    (slim:with-table (pane)
      (labels ((display-item (path &optional current)
		 (let ((stat (handler-case (osicat-posix:stat path) (t () nil))))
		   (with-output-as-presentation (pane
						 (namestring path)
						 'filesystem-presentation
						 :single-box t)
		     (slim:row
		       (slim:cell
			 (if (member (namestring path)
				     *marks* :test #'string-equal)
			     (with-drawing-options (pane :ink +red+)
			       (format pane "*"))
			     (with-drawing-options (pane :ink +black+)
			       (format pane " "))))
		       (with-drawing-options
			   (pane :ink (if (member (namestring path) *marks*
						  :test #'string=)
					  (if current +purple+ +orange-red+)
					  (if current +purple+ +black+)))
			 (with-text-style
			     (pane (make-text-style
				    "ETBembo"
				    (cond ((member (namestring path) *marks*
						   :test #'string=)
					   "DisplayItalicBold")
					  ;; (current "SuperBoldOSF")
					  (t "RomanLF"))
				    
				    16))
			   (slim:cell
			     (if (uiop:directory-exists-p path)
				 (with-drawing-options
				     (pane :ink (if (member (namestring path)
							    *marks*
							    :test #'string=)
						    +orange-red+ +blue+))
				   (format pane "DIR"))
				 (with-drawing-options
				     (pane :ink (if (member (namestring path)
							    *marks*
							    :test #'string=)
						    +orange-red+ +green4+))
				   (format pane "FILE"))))
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
				     (and stat (permissions-as-string
						path)))))))))
		 ))
	(when pre-selection-contents
	  (loop for path in (reverse pre-selection-contents)
		unless (and *hide-files* (hidden-pathname-p path))
		  do (display-item path)))
	(when selected-item
	  (display-item selected-item t))
	(when post-selection-contents
	  (loop for path in post-selection-contents
		unless (and *hide-files* (hidden-pathname-p path))
		  do (display-item path)))
	;; (when (< (length post-selection-contents) (length pre-selection-contents))
	;;   (scroll-pane-to-bottom pane))
	))))

(define-gesture-name :next-item :keyboard (#\n :control))
(define-gesture-name :prev-item :keyboard (#\p :control))
(define-gesture-name :mark-item :keyboard (#\space :control))
(define-gesture-name :up-directory :keyboard (#\p :meta))
(define-gesture-name :open :keyboard (#\o :control))
(define-gesture-name :open :keyboard (:return :control))
(define-gesture-name test :keyboard (#\t :control :meta)
  ;; (:return :control)
  )

;; broken (define-gesture-name :carriage-return :keyboard ())
(define-gesture-name :carriage-return :keyboard (#\m :control))


(define-clfm-command (com-move-up :name "Move Up Selection"
				  :keystroke :prev-item) ()
  (move-selection-up))

(define-clfm-command (com-move-down :name "Move Down Selection"
				    :keystroke :next-item) ()
  (move-selection-down))

(define-clfm-command (com-keyboard-mark :keystroke :mark-item) ()
  (add-current-selected-to-marks))

(define-clfm-command (com-up-directory :keystroke (#\a :control)) ()
  (change-directory (concatenate 'string (namestring (uiop:getcwd)) "../")))

(define-clfm-command (com-keyboard-open :keystroke (:return :control)) ()
  (enter-or-open-current-selection))

;; (define-clfm-command (com-test-test-keyboard :keystroke ;; (#\k :control)
;; 					     (:test-cc :test-mc)) ()
;;   (enter-or-open-current-selection))

(define-clfm-command (com-keyboard-open :keystroke (#\i :shift :super)) ()
  (enter-or-open-current-selection))

;; (add-input-editor-command
;;  )

(define-gesture-name :test-cc :keyboard (#\c :control))
(define-gesture-name :test-mc :keyboard (#\c :meta))

;; ok this is broken... somehow... IDK why... 
(defmacro define-main-keybinding (gesture &body body)
  "Takes a gesture by name or by specification, and defines a command which runs 
body. If gesture is a string, it MUST be a single key chord. "
  (let ((command (intern (symbol-name (gensym "COM-ANONYMOUS-KEYBINDING-"))))
	(key-seq (if (stringp gesture)
		     (key-chord gesture)
		     gesture)))
    `(define-clfm-command (,command :keystroke ,key-seq) ()
       ,@body)))

;; (define-main-keybinding "C-ł"
;;   ;; "M- " ;; (#\j :meta)
;;   (add-current-selected-to-marks))

;; (define-main-keybinding (#\latin_small_letter_ae :control)
;;   (add-current-selected-to-marks))

;; (define-main-keybinding (:return :control)
;;   (enter-or-open-current-selection))

