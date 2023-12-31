(in-package :asdf-user)

(defsystem "cl-trello"
  :description "A clone of Trello with Common Lisp and HTMX"
  :author "Abhinav Krishna <abhinavkrishnacr2020@gmail.com>"
  :version "0.0.1"
  :depends-on (defcss spinneret ningle clack mito iterate alexandria str)
  :components ((:module "src"
                :serial t
                :components
                ((:file "model")
                 (:file "templates")
                 (:file "main")))))
