(in-package :common-lisp-file-manager)

(defmacro with-gensyms ((&body syms) &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (gensym ,(symbol-name sym)))) syms)
     ,@body))

(defmacro monospaced ((stream) &body body)
  `(with-text-style (,stream *monospaced-font*)
     ,@body))

(defmacro with-main-font ((stream) &body body)
  `(with-text-style (,stream *main-font*)
     ,@body))
