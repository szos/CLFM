;;;; common-lisp-file-manager.asd

(asdf:defsystem #:common-lisp-file-manager
  :description "Describe common-lisp-file-manager here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:osicat #:alexandria #:mcclim #:slim
                        #:cl-ppcre #:defconfig #:clim-examples)
  :components ((:file "package")
               (:file "macros")
               (:file "options")
               
               (:file "options/prompter")
               (:file "options/display-hidden-items")
               (:file "options/display-permissions")
               (:file "options/colors")
               
               (:file "font-selection")
               (:file "file-system-overview")
               (:file "frame")
               (:file "file-menu-commands")
               (:file "directory-display")
               (:file "present-files-directories")
               (:file "fs-overview-commands")
               
               (:file "options-display")
               
               (:file "common-lisp-file-manager")))
