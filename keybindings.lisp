(in-package :clfm-2)

(make-command-table 'clfm-control-x)

(add-keystroke-to-command-table 'clfm '(#\x :control) :menu 'clfm-control-x)

(add-keystroke-to-command-table 'clfm-control-x '(#\c :control)
				:command '(com-exit))
