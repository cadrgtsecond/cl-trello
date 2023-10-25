(in-package :asdf-user)

(defsystem "cl-trello"
  :description "A clone of Trello with Common Lisp and HTMX"
  :author "Abhinav Krishna <abhinavkrishnacr2020@gmail.com>"
  :version "0.0.1"
  :depends-on (ten ningle clack)
  :defsystem-depends-on (ten)
  :components ((:module "src"
                :depends-on ("templates")
                :components
                ((:file "main")))
               (:module "templates"
                :serial t
                :components
                ((:file "main")
                 (:ten-template "index" :file-extension "html" :package :cl-trello.templates)))))
