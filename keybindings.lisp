(in-package :clfm-2)

(make-command-table 'clfm-control-x :errorp nil)

(add-keystroke-to-command-table 'clfm '(#\x :control) :menu 'clfm-control-x
				:errorp nil)

(add-keystroke-to-command-table 'clfm-control-x '(#\c :control)
				:command '(com-exit)
				:errorp nil)

;; (clim:add-keystroke-to-command-table 'clfm-control-x)
;;; this doesnt work, but it should.... hmmmmmm....
