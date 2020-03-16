(in-package :clfm-2)

(define-gesture-name :meta-click-left :pointer-button (:left :meta))

(define-gesture-name :meta-control :keyboard (:control :meta))

(define-presentation-type fopen-presentation ())

(defun set-file-command-order (&key
				 (delete -1)
				 (open 4)
				 (open-with 3)
				 (rename 2)
				 (copy 1)
				 ;; (mark 0)
				 )
  "the higher the number, the higher on the list it will be"
  ;; (define-presentation-to-command-translator fshift-click
  ;;     (fopen-presentation com-mark-between-items))
  (define-presentation-to-command-translator fmark
      (fopen-presentation com-add-mark clfm
       ;; :gesture :select
       :gesture :meta-click-left
       :documentation "Mark File"
       ;; :priority mark
       )
      (file)
    (list file))
  (define-presentation-to-command-translator fdelete
      (fopen-presentation com-rm-file clfm
       :gesture :select
       :documentation "Delete File (PERMANENT)"
       :priority delete)
      (f)
    (list f))
  (define-presentation-to-command-translator fopen
      (fopen-presentation com-open-file clfm
       :gesture :select
       :documentation "Open File"
       :priority open)
      (f)
    (list f))
  (define-presentation-to-command-translator fopen-with
      (fopen-presentation com-open-file-with clfm
       :gesture :select
       :documentation "Open File With"
       :priority open-with)
      (f)
    (list f))
  (define-presentation-to-command-translator frename
      (fopen-presentation com-rename-this clfm
       :gesture :select
       :documentation "Rename this"
       :priority rename)
      (f)
    (list f))
  (define-presentation-to-command-translator fcopy
      (fopen-presentation com-copy-file-prompt clfm
       :gesture :select
       :documentation "Copy this file"
       :priority copy)
      (f)
    (list f)))

(set-file-command-order)

(define-presentation-type chdir-presentation ())

;; (define-gesture-name :double-click :pointer-button (:left :double))

(defun set-directory-command-order (&key
				      (change-directory 3)
				      (delete -1)
				      ;; (mark 2)
				      (copy 1))
  (define-presentation-to-command-translator dcopy
      (chdir-presentation com-copy-directory-prompt clfm
       :gesture :select
       :documentation "Copy Directory"
       :priority copy)
      (dir)
    (list dir))
  (define-presentation-to-command-translator chdir
      (chdir-presentation com-change-directory-temper clfm
       :gesture :select
       ;; :gesture :double-click
       :Documentation "Change to Directory"
       :priority change-directory
       )
      (dir)
    (list dir))
  (define-presentation-to-command-translator dir-delete
      (chdir-presentation com-delete-directory clfm
       :gesture :select
       :documentation "Delete Directory (PERMANENT)"
       :priority delete)
      (dir)
    (list dir))
  (define-presentation-to-command-translator dmark
      (chdir-presentation com-add-mark clfm
       ;; :gesture :select
       :gesture :meta-click-left
       :documentation "Mark Directory"
       ;; :priority mark
       )
      (file)
    (list file)))

(set-directory-command-order)

(defun file-prefer-marks ()
  (set-file-command-order :mark 5))

(defun directory-prefer-marks ()
  (set-directory-command-order :mark 3))

;; (define-drag-and-drop-translator drag-and-drop-directory
;;     (chdir-presentation
;;      chdir-presentation dcopy clfm
;;      :documentation "drag and drop file"
;;      :gesture ))

(make-command-table 'clfm-menu-bar
		    :errorp nil
		    :menu '(("Quit" :command com-quit)
			    ("File" :menu clfm-file)
			    ("Operate/Marks" :menu clfm-operation-menu)
			    ("View" :menu clfm-view-menu)
			    ("Help" :menu clfm-help-menu)))

(make-command-table 'clfm-file
		    :errorp nil
		    :menu '(("New File" :command com-new-file)
			    ("New Directory" :command com-new-dir)
			    ("Go Home" :command com-chdir-~/)
			    ("Rename…" :command com-rename-prompt)
			    ("Load Init File" :command com-loadrc)))

(make-command-table 'clfm-operation-menu
		    :errorp nil
		    :menu '(("Select Operation" :command com-operate-on-marks)))

(make-command-table 'clfm-view-menu
		    :errorp nil
		    :menu '(("Marks" :menu clfm-view-marks-menu)
			    ("Display Hidden Files" :command
			     com-toggle-hidden-files)))

(make-command-table 'clfm-view-marks-menu
		    :errorp nil
		    :menu '(("Show Marks" :command com-show-marks)
			    ("Hide Marks" :command com-hide-marks)))

(make-command-table 'clfm-help-menu
		    :errorp nil
		    :menu '(("Help Window" :command com-display-help-window)
			    ("test" :command com-move-down
			     :keystroke (:n :control))))



