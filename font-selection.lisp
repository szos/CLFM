(in-package :common-lisp-file-manager)

(defun launch-font-selector (calling-frame)
  (clim-demo::select-font :calling-frame calling-frame))
