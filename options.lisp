(in-package :common-lisp-file-manager)

(defparameter *options-list* '())

(defconfig:define-defconfig-db *clfm-options-database* :clfm-options
  :doc "database for clfm options")

(defmacro define-option (name value promptfn &rest defconfig-keys)
  "promtfn should be a function which takes a single argument, the option, and 
return a value to set that option to. If the option itself is returned (it will
be sent in quoted) then we can assume that the user failed to input a value."
  (with-gensyms (arg result)
    `(progn
       (defconfig ,name ,value ,@defconfig-keys
         :db *clfm-options-database*)
       (unless (assoc ',name *options-list*)
         (push (cons ',name (lambda (,arg)
                              (let ((,result (funcall ,promptfn ,arg)))
                                (unless (eql ,result ,arg)
                                  (setv ,name ,result
                                        :db *clfm-options-database*)))))
               *options-list*)))))

(defun prompt-t-or-f (option)
  (notify-user nil
               (format nil "Set the value of ~A" option)
               :text-style (make-text-style :serif :roman :large)
               :exit-boxes `((t "TRUE")
                             (nil "FALSE")
                             (,option "CANCEL"))))

(define-option *display-hidden-items* nil 'prompt-t-or-f
  :typespec '(or t null))

(defun file-permissions-list-p (list)
  (null (remove :user (remove :group (remove :others list)))))

(deftype file-permissions-list ()
  '(satisfies file-permissions-list-p))

(defun checkbox-changed-callback (radio-box value)
  (setf *dbg* (format nil "radio box ~A set to ~A" radio-box value)))

(define-application-frame promt-for-permissions () ()
  (:panes
   (select (with-radio-box (:orientation :vertical :type :some-of)
             "User" "Group" "Others")))
  (:layouts (default select)))

(defun prompt-permissions (option)
  (declare (ignore option))
  (let (user group others)
    
    (accepting-values (stream :own-window t
                              :label "Enter permissions"
                              :initially-select-query-identifier 'user
                              :resize-frame t)
      (formatting-table (stream)
        (formatting-row (stream)
          (setf user (accept '(null-or-type t) :prompt "Y/N" :stream stream
                                               :query-identifier 'user)))
        (formatting-row (stream)
          (setf group (accept '(null-or-type t) :prompt "Y/N" :stream stream
                                                :query-identifier 'group)))
        (formatting-row (stream)
          (setf others (accept '(null-or-type t) :prompt "Y/N" :stream stream
                                                 :query-identifier 'others)))))
    (setf *dbg* (list user group others)))
  ;; (remove nil 
  ;;         (list (notify-user nil
  ;;                            "Show user permissions?"
  ;;                            :text-style (make-text-style :serif :roman :large)
  ;;                            :exit-boxes '((:user "YES")
  ;;                                          (nil "NO")))
  ;;               (notify-user nil
  ;;                            "Show group permissions?"
  ;;                            :text-style (make-text-style :serif :roman :large)
  ;;                            :exit-boxes '((:group "YES")
  ;;                                          (nil "NO")))
  ;;               (notify-user nil
  ;;                            "Show others permissions?"
  ;;                            :text-style (make-text-style :serif :roman :large)
  ;;                            :exit-boxes '((:others "YES")
  ;;                                          (nil "NO")))))
  )

(define-option *display-permissions* '(:user :group :others) 'prompt-permissions
  :typespec 'file-permissions-list)

(defun prompt-for-font (option)
  (let ((result (launch-font-selector nil)))
    (if (typep result 'text-style)
        result
        option)))

(define-option *monospaced-font* (make-text-style :fix :roman :normal)
  'prompt-for-font
  :typespec 'text-style)

(define-option *main-font* *default-text-style* 'prompt-for-font
  :typespec 'text-style)

(define-option *filetype-font* *default-text-style* 'prompt-for-font
  :typespec 'text-style)

(defun parse-string-into-color (string)
  (labels ((lookup-color-name (s)
             (let ((sym (intern (concatenate 'string "+" (string-upcase s) "+"))))
               (symbol-value sym)))
           (rgb-numbers->color (s)
             (let ((color-string (if (char= (char s 0) #\#)
                                     (subseq s 1)
                                     s)))
               (funcall
                'make-rgb-color
                (/ (parse-integer color-string :start 0 :end 2 :radix 16) 255)
                (/ (parse-integer color-string :start 2 :end 4 :radix 16) 255)
                (/ (parse-integer color-string :start 4 :end 6 :radix 16) 255)))))
    (handler-case (rgb-numbers->color string)
      (error ()
        (handler-case (lookup-color-name string)
          (error () string))))))

(defun defconfig-color-coercer (value)
  (typecase value
    (symbol (symbol-value value))
    (string (parse-string-into-color value))
    (otherwise value)))

(defun prompt-for-color (option)
  (let (result)
    (accepting-values (stream :own-window t
                              :label "Enter color, either named or by hex code"
                              :initially-select-query-identifier 'color
                              :resize-frame t)
      (formatting-table (stream)
        (formatting-column (stream)
          (formatting-cell (stream)
            (setf result (accept 'string :prompt "Color: "
                                         :stream stream
                                         :query-identifier 'color))))))
    (or result option)))

(define-option *foreground-color* +black+ 'prompt-for-color
  :typespec 'color
  :coercer 'defconfig-color-coercer)

(define-option *background-color* +white+ 'prompt-for-color
  :typespec 'color
  :coercer 'defconfig-color-coercer)

(define-option *directory-color* +blue+ 'prompt-for-color
  :typespec 'color
  :coercer 'defconfig-color-coercer)

(define-option *file-color* +dark-green+ 'prompt-for-color
  :typespec 'color
  :coercer 'defconfig-color-coercer)

(define-option *color-entry* nil 'prompt-t-or-f ; takes any t or nil value
  :validator (lambda (v) (or v t)))


#|

IDEA: define a macro called defopt, or defoption, or define-option, or whatever, 
which wraps around defconfig, and takes a symbol, default value, typespec, 
(optionally) coercer, and a prompt function (which should call accepting-values
with :own-window t). This macro should intern these options in a list to be used 
when displaying options, and there should be a presentation->command translator 
that calls the prompt function to get a value and sets the option to the value. 

|#

(macrolet ((opt (option)
             `(cons ',option ,option))
           (do-options (&body options)
             `(list
               ,@(loop for option in options
                       collect `(opt ,option)))))
  (defparameter *options-alist*
    (do-options
      *display-hidden-items*
      *display-permissions*)))
