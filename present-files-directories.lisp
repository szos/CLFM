(in-package :common-lisp-file-manager)

(defclass clfm-entry ()
  ((path :initform "" :initarg :path :accessor clfm-path)))

(defclass clfm-file (clfm-entry) ())

(define-presentation-method present
    (file (type clfm-file) stream view &key)
  ;; assumes that were in a slim:with-table call
  (slim:row
    (slim:cell
      (with-drawing-options (stream :ink *file-color*)
        (format stream "FILE")))
    (with-drawing-options (stream :ink (if *color-entry*
                                           *file-color*
                                           *foreground-color*))
      (slim:cell (with-main-font (stream)
                   (format stream "~A" (display-name (clfm-path file)))))
      (slim:cell (monospaced (stream)
                   (format stream (file-permissions-string (clfm-path file))))))))

(defclass clfm-dir (clfm-entry)
  ((pane :initarg :pane :accessor clfm-dir-pane)))

(define-presentation-method present
    (dir (type clfm-dir) stream view &key)
  ;; assumes that were in a slim:with-table call
  (slim:row
    (slim:cell
      (with-drawing-options (stream :ink *directory-color*)
        (format stream "DIR")))
    (with-drawing-options (stream :ink (if *color-entry*
                                           *directory-color*
                                           *foreground-color*))
      (slim:cell (with-main-font (stream)
                   (format stream "~A" (display-name (clfm-path dir)))))
      (slim:cell (monospaced (stream)
                   (format stream (file-permissions-string (clfm-path dir))))))))

(defclass clfm-up-one-dir (clfm-dir) ())

(define-presentation-method present
    (dir (type clfm-up-one-dir) stream view &key)
  ;; assumes that were in a slim:with-table call
  (with-drawing-options (stream :ink *directory-color*)
    (format stream "UP ONE DIRECTORY"))
  (with-drawing-options (stream :ink (if *color-entry*
                                           *directory-color*
                                           *foreground-color*))
    (with-main-font (stream)
      (format stream ":  ~A~&" (display-name (clfm-path dir))))))

;;;;;;;;;;;;;;;;;;;;;
;;; File Commands ;;;
;;;;;;;;;;;;;;;;;;;;;

(labels
    ((find-filetype (file)
       (string-trim
        '(#\space #\newline)
        (with-output-to-string (s)
          (uiop:run-program (format nil "xdg-mime query filetype ~A" file)
                            :output s))))
     (file-default-program (file)
       (string-trim
        '(#\space #\newline)
        (with-output-to-string (s)
          (uiop:run-program (format nil "xdg-mime query default ~A"
                                    (find-filetype file))
                            :output s)))))
  (define-clfm-command (com-open-file :name t) ((file clfm-file))
    (let ((program
            (notify-user *application-frame*
                         (format nil "Open \"~A\" using ~A ?"
                                 (clfm-path file)
                                 (file-default-program
                                  (format nil "'~A'" (clfm-path file))))
                         :text-style (make-text-style :serif :roman :large)
                         :exit-boxes '((:proceed "Yes")
                                       (:abort "No")))))
      (when (eq program :proceed)
        (uiop:launch-program
         (format nil "xdg-open '~A'" (clfm-path file)))))))

(define-presentation-to-command-translator open-file
    (clfm-file com-open-file clfm :priority 9)
    (object)
  (list object))

(define-clfm-command (com-open-with) ((file clfm-file))
  (com-open-file-with file))

(define-presentation-to-command-translator open-file-with
    (clfm-file com-open-with clfm :priority 8)
    (object)
  (list object))

(define-clfm-command (com-rm-file :name t) ((file clfm-file))
  (notify-user *application-frame* (format nil "rm '~A'"
                                           (clfm-path file))))

(define-presentation-to-command-translator delete-file
    (clfm-file com-rm-file clfm :priority 0)
    (obj)
  (list obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-clfm-command (com-enter-directory :name t) ((directory clfm-dir))
  (directory-update-directory (clfm-dir-pane directory)
                              (clfm-path directory)))

(define-presentation-to-command-translator enter-directory
    (clfm-dir com-enter-directory clfm :priority 9)
    (object)
  (list object))

(define-clfm-command (com-rm-dir :name t) ((directory clfm-dir))
  (notify-user *application-frame* (format nil "rm -rf '~A'"
                                           (clfm-path directory))))

(define-presentation-to-command-translator delete-directory
    (clfm-dir com-rm-dir clfm :priority 0)
    (obj)
  (list obj))
