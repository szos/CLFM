
(in-package :clfm-2)

(define-clfm-command (com-rm-file) ((file string))
  (delete-file file))
