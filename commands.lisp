(in-package :clfm-2)

(define-presentation-type fopen-presentation ())

(defun set-file-command-order (&key
				 (delete -1)
				 (open 4)
				 (open-with 3)
				 (rename 2)
				 (copy 1)
				 (mark 0))
  "the higher the number, the higher on the list it will be"
  (define-presentation-to-command-translator fmark
      (fopen-presentation com-add-mark clfm
       :gesture :select
       :documentation "Mark File"
       :priority mark)
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

(defun set-directory-command-order (&key
				      (change-directory 2)
				      (delete -1)
				      (mark 1))
  (define-presentation-to-command-translator chdir
      (chdir-presentation com-change-directory clfm
       :gesture :select
       :Documentation "Change to Directory"
       :priority change-directory)
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
       :gesture :select
       :documentation "Mark Directory"
       :priority mark)
      (file)
    (list file)))

(set-directory-command-order)

(make-command-table 'clfm-menu-bar
		    :errorp nil
		    :menu '(("Quit" :command com-quit)
			    ("File" :menu clfm-file)
			    ("Operate/Marks" :menu clfm-operation-menu)
			    ("Help" :menu clfm-help-menu)))

(make-command-table 'clfm-file
		    :errorp nil
		    :menu '(("New File" :command com-new-file)
			    ("New Directory" :command com-new-dir)
			    ("Go Home" :command com-chdir-~/)
			    ("Rename…" :command com-rename-prompt)
			    ("Load Init File" :command com-loadrc)))

(make-command-table 'clfm-help-menu
		    :errorp nil
		    :menu '(("Help Window" :command com-display-help-window)))

(make-command-table 'clfm-operation-menu
		    :errorp nil
		    :menu '(("Select Operation" :command com-operate-on-marks)))

