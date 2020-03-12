(in-package :clfm-2)

(define-presentation-type fopen-presentation ())

(define-presentation-to-command-translator fdelete
    (fopen-presentation com-rm-file clfm
     :gesture :select
     :documentation "Delete File (PERMANENT)"
     :priority -1)
    (f)
  (list f))

(define-presentation-to-command-translator fopen
    (fopen-presentation com-open-file clfm
     :gesture :select
     :documentation "Open File")
    (f)
  (list f))

(define-presentation-to-command-translator fopen-with
    (fopen-presentation com-open-file-with clfm
     :gesture :select
     :documentation "Open File With"
     :priority -1)
    (f)
  (list f))

(define-presentation-type chdir-presentation ())

(define-presentation-to-command-translator chdir
    (chdir-presentation com-change-directory clfm
     :gesture :select
     :Documentation "Change to Directory")
    (dir)
  (list dir))

(define-presentation-to-command-translator dir-delete
    (chdir-presentation com-delete-directory clfm
     :gesture :select
     :documentation "Delete Directory (PERMANENT)"
     :priority -1)
    (dir)
  (list dir))

(define-presentation-to-command-translator frename
    (fopen-presentation com-rename-this clfm
     :gesture :select
     :documentation "Rename this"
     :priority -1)
    (f)
  (list f))

(define-presentation-to-command-translator fcopy
    (fopen-presentation com-copy-file-prompt clfm
     :gesture :select
     :documentation "Copy this file"
     :priority -1)
    (f)
  (list f))

(make-command-table 'clfm-menu-bar
		    :errorp nil
		    :menu '(("Quit" :command com-quit)
			    ("File" :menu clfm-file)
			    ("Help" :menu clfm-help-menu)))

(make-command-table 'clfm-file
		    :errorp nil
		    :menu '(("New File" :command com-new-file)
			    ("New Directory" :command com-new-dir)
			    ("Go Home" :command com-chdir-~/)
			    ("Rename…" :command com-rename-prompt)))

(make-command-table 'clfm-help-menu
		    :errorp nil
		    :menu '(("Help Window" :command com-display-help-window)))
