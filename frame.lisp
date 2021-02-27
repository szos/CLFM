(in-package :common-lisp-file-manager)

;;; IDEA: we could create classes programmatically with the directory slot
;;;       being allocated on a class basis instead of an object basis, and when
;;;       we create a new pane we... yeah come back to this.
(defclass directory-tracker-mixin ()
  ((directory :initarg :directory
              :accessor pane-directory
              :initform (uiop:parse-native-namestring
                         (uiop:native-namestring "~/")))))

(defclass directory-binding-mixin ()
  ((bound-to :initarg :bound-to :initform nil :accessor pane-bound-to-panes)))

(defclass directory-metadata (directory-tracker-mixin directory-binding-mixin) ())

(defclass common-directory-pane (directory-metadata application-pane)
  ()
  (:default-initargs
   :incremental-redisplay t
   :background *background-color*
   :foreground *foreground-color*))
(defclass directory-name-pane (common-directory-pane) ()
  (:default-initargs :display-function 'display-directory-name
                     :incremental-redisplay nil))
(defclass directory-contents-pane (common-directory-pane) ()
  (:default-initargs :display-function 'display-directory-contents))

(defclass directory-pane (directory-metadata vrack-pane) ())

(defgeneric directory-update-directory (pane directory))

(defmethod directory-update-directory ((pane directory-metadata)
                                       (directory pathname))
  (setf (pane-directory pane) directory)
  (loop for p in (pane-bound-to-panes pane)
        do (setf (pane-directory p) directory)))

;; (defmethod directory-update-directory ((pane common-directory-pane)
;;                                        (directory pathname))
;;   (setf (pane-directory pane) directory
;;         (pane-directory (pane-bound-to-panes pane)) directory))

;; (defmethod directory-update-directory ((pane common-directory-pane)
;;                                        (directory string))
;;   (let ((dir (uiop:parse-native-namestring directory)))
;;     (setf (pane-directory pane) dir
;;           (pane-directory (pane-bound-to-panes pane)) dir)))


(defmacro make-directory ((dir &key (name-size 20) adjuster) &rest other-options)
  (alexandria:with-gensyms (name-pane contents-pane holder)
    `(let* ((,name-pane (make-pane 'directory-name-pane :directory ,dir))
            (,contents-pane (make-pane 'directory-contents-pane :directory ,dir))
            (,holder (make-pane 'directory-pane
                                :directory ,dir
                                :bound-to (list ,name-pane ,contents-pane)
                                ,@other-options
                                :contents
                                (list
                                 (list ',name-size
                                       (outlining (:thickness 1)
                                         ,name-pane))
                                 ,@(when adjuster
                                     `((make-pane 'clime:box-adjuster-gadget)))
                                 (list ':fill
                                       (outlining (:thickness 1)
                                         (scrolling nil ,contents-pane)))))))
       (setf (pane-bound-to-panes ,name-pane) (list ,contents-pane ,holder)
             (pane-bound-to-panes ,contents-pane) (list ,name-pane ,holder))
       ,holder)))

(define-application-frame clfm ()
  ((scroll-note :initform nil :accessor note-scrolling)
   (current-pane :initform 'fs-overview :accessor clfm-current-pane)
   (reinit :initform nil :accessor clfm-frame-reinit))
  (:menu-bar clfm-menu-bar)
  (:panes
   (fs-overview :application
                :display-function 'display-file-system
                :background *background-color*
                :foreground *foreground-color*
                :scroll-bars :vertical)
   (options :application
            :display-function 'display-options
            :incremental-redisplay t)
   (panel-1 (make-directory (#P"/home/szos/" :adjuster nil)))
   (panel-2 (make-directory (#P"/home/szos/" :adjuster nil))))
  (:layouts
   (default panel-1)
   (one-panel panel-1)
   (one-panel-fs
    (horizontally ()
      (1/5  fs-overview ;; (spacing (:thickness 5))
            )
      (make-pane 'clime:box-adjuster-gadget)
      (:fill (outlining (:thickness 1) panel-1))))
   (one-panel-options
    (horizontally ()
      (:fill (outlining (:thickness 1) panel-1))
      (make-pane 'clime:box-adjuster-gadget)
      (1/4 (outlining (:thickness 1) options))))
   (one-panel-fs-options
    (horizontally ()
      (1/5 (outlining (:thickness 1) (spacing (:thickness 2) fs-overview)))
      (make-pane 'clime:box-adjuster-gadget)
      (:fill (outlining (:thickness 1) panel-1))
      (make-pane 'clime:box-adjuster-gadget)
      (1/4 (outlining (:thickness 1) options))))
   #|add two-panel variants.|#
   (two-panel (horizontally ()
                (1/2 panel-1)
                (make-pane 'clime:box-adjuster-gadget)
                (1/2 panel-2)))
   (two-panel-fs
    (horizontally ()
      (1/5  fs-overview ;; (spacing (:thickness 5))
            )
      (make-pane 'clime:box-adjuster-gadget)
      (:fill
       (horizontally ()
         (outlining (:thickness 1) panel-1)
         (make-pane 'clime:box-adjuster-gadget)
         (outlining (:thickness 1) panel-2)))))))

(define-clfm-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(make-command-table 'clfm-menu-bar
                    :errorp nil
                    :menu '(("File" :menu clfm-file-menu)
                            ("View" :menu clfm-view-menu)))

(make-command-table 'clfm-view-menu
                    :errorp nil
                    :menu '(("Toggle FS Tree" :command com-toggle-fs-tree)
                            ("Toggle Second Panel" :command com-toggle-2panel)))

(defvar *option-toggle-alist*
  '((one-panel . one-panel-options)
    (one-panel-fs . one-panel-fs-options)
    (one-panel-options . one-panel)
    (one-panel-fs-options . one-panel-fs)))

(defvar *fs-tree-toggle-alist*
  '((one-panel . one-panel-fs)
    (one-panel-fs . one-panel)))

(defvar *second-panel-toggle-alist*
  '((one-panel . two-panel)
    (two-panel . one-panel)
    (one-panel-fs . two-panel-fs)
    (two-panel-fs . one-panel-fs)))

(define-clfm-command (com-toggle-options) ()
  (let ((layout (cdr (assoc (frame-current-layout *application-frame*)
                            *option-toggle-alist*))))
    (when layout (setf (frame-current-layout *application-frame*) layout))))

(define-clfm-command (com-toggle-fs-tree) ()
  (let ((layout (cdr (assoc (frame-current-layout *application-frame*)
                            *fs-tree-toggle-alist*))))
    (when layout (setf (frame-current-layout *application-frame*) layout))))

(define-clfm-command (com-toggle-2panel) ()
  (let ((layout (cdr (assoc (frame-current-layout *application-frame*)
                            *second-panel-toggle-alist*))))
    (when layout (setf (frame-current-layout *application-frame*) layout))))

(define-clfm-command (com-update-background) ()
  (setf (clfm-frame-reinit *application-frame*) t))

(define-condition restart-clfm () ())

(define-clfm-command (com-restart) ()
  (error 'restart-clfm))
