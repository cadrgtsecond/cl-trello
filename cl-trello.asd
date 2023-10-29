(in-package :asdf-user)

(defsystem "cl-trello"
  :description "A clone of Trello with Common Lisp and HTMX"
  :author "Abhinav Krishna <abhinavkrishnacr2020@gmail.com>"
  :version "0.0.1"
  :depends-on (ten ningle clack mito)
  :defsystem-depends-on (ten)
  :components ((:module "src"
                :serial t
                :components
                ((:file "model")
                 (:file "template-package")
                 (:ten-template "templates" :file-extension "html" :package :cl-trello.templates)
                 (:file "main")))))
