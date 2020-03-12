
(in-package :clfm-2)

(define-application-frame clfm-help () ()
  (:menu-bar clfm-help-window-menu-bar)
  (:panes
   (keybindings :application
		:display-function #'help-display-keybindings
		:width 600)
   (generic :application
	    :display-function #'help-display-generic
	    :width 600))
  (:layouts
   (default
    generic)
   (keybindings keybindings)))

(define-clfm-help-command (com-switch-to-generic-page) ()
  (setf (frame-current-layout *application-frame*) 'default))

(define-clfm-help-command (com-switch-to-keybindings-page) ()
  (setf (frame-current-layout *application-frame*) 'keybindings))

;; (define-clfm-help-command (com-switch-to-opening-files) ()
;;   )

(make-command-table 'clfm-help-window-menu-bar
		    :errorp nil
		    :menu
		    '(("General" :command com-switch-to-generic-page)
		      ("Keybindings" :command com-switch-to-keybindings-page)))

(defun help-display-keybindings (frame pane)
  (declare (ignore frame))
  (slim:with-table (pane)
    (slim:row (slim:cell (format pane "Keybindings: ")))
    (slim:row (slim:cell (format pane " ")))
    (slim:row
      (slim:cell (format pane "M-RET"))
      (slim:cell (format pane "Submit Input")))))

(defun help-display-generic (frame pane)
  (declare (ignore frame))
  (with-end-of-line-action (pane :wrap*)
    (with-etbembo (pane :bold)
      (format pane "This is the help page for CLFM. It should contain documentation on the usage and settings of CLFM"))
    (terpri pane)
    (terpri pane)
    (with-etbembo (pane)
      (format pane "CLFM stands for ")
      (with-etbembo (pane :italic)
	(format pane "Common Lisp File Manager"))
      (format pane ". It is primarily mouse driven, though it is planned to implement keybindings (and an init file where users can change them) to drive this application via the keyboard. "))))
