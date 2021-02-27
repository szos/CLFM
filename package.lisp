;;;; package.lisp

(defpackage #:common-lisp-file-manager
  (:use #:clim #:clim-lisp)
  (:export #:app-main)
  (:import-from :defconfig
                #:defconfig
                #:defconfig-minimal
                #:define-variable-config
                #:setv
                #:with-atomic-setv*))
