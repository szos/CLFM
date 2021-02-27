(in-package #:common-lisp-file-manager)

(defun string-up-dir (thing)
  (let ((string (uiop:native-namestring thing)))
    (format nil "~{~A/~}" (butlast (cl-ppcre:split "/" string)))))

(defun display-directory-name (frame pane)
  (declare (ignore frame))
  (setf (medium-background pane) *background-color*
        (medium-foreground pane) *foreground-color*)
  (format pane "  ~A" (pane-directory pane)))

(defun display-name (path)
  (let* ((p (namestring path))
         (name (car (last (cl-ppcre:split "/" p)))))
    (if name
        name
        "/")))

(let ((permissions-list '(("r" . #.nix:s-irusr)
                          ("w" . #.nix:s-iwusr)
                          ("x" . #.nix:s-ixusr)
                          ("r" . #.nix:s-irgrp)
                          ("w" . #.nix:s-iwgrp)
                          ("x" . #.nix:s-ixgrp)
                          ("r" . #.nix:s-iroth)
                          ("w" . #.nix:s-iwoth)
                          ("x" . #.nix:s-ixoth))))
  (defun file-permissions-list (pathspec)
    (let ((mode (nix:stat-mode (nix:stat pathspec))))
      (loop for (name . value) in permissions-list
            if (plusp (logand mode value))
              collect name
            else collect "-")))
  (defun file-permissions-string (path &aux (pathspec (pathname path)))
    (apply 'concatenate 'string 
           (let ((list (file-permissions-list pathspec)))
             (loop for (r w x) on list by 'cdddr
                   for c in '(:user :group :others)
                   when (member c *display-permissions*)
                     collect (concatenate 'string r w x))))))

(defun display-thing (type pane thing &rest initargs)
  (present (apply 'make-instance (append (list type :path thing) initargs))
           type
           :stream pane
           :single-box t))

(defun display-directory-contents (frame pane)
  (declare (ignore frame))
  (setf (medium-background pane) *background-color*
        (medium-foreground pane) *foreground-color*)
  (let* ((dir (pane-directory pane))
         (subdirs (uiop:subdirectories dir))
         (files (uiop:directory-files dir))
         (updir (string-up-dir dir))
         (updir-obj (make-instance 'clfm-up-one-dir
                                   :path (pathname
                                          (if (string= updir "")
                                              "/"
                                              updir))
                                   :pane pane))
         (dirs-to-display (if *display-hidden-items*
                              subdirs
                              (remove-if 'hidden-file-or-directory-p subdirs)))
         (files-to-display (if *display-hidden-items*
                               files
                               (remove-if 'hidden-file-or-directory-p files))))
    (present updir-obj 'clfm-up-one-dir :stream pane)
    (slim:with-table (pane)
      (loop for thing in dirs-to-display
            do (display-thing 'clfm-dir pane thing :pane pane))
      (loop for thing in files-to-display
            do (display-thing 'clfm-file pane thing)))))

;;; scrolling functions to revisit:
#|

scroll-extent 
window-set-viewport-region
window-viewport-region

We could have our scroll commands set the slot of the application frame... no, we 
need to indicate the pane to scroll too... ok, so we use M-f and M-b to swap 
between our two directory panes (or fs pane), and store the current one in a frame
slot. then when we note that we want to scroll up/down/left/right, we invoke that
on that pane! 

|#
