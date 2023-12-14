(defpackage cl-trello.model
  (:use :cl :sxql)
  (:documentation "Structs and functions to pull data from the database")
  (:export todo get-todos create-todo todo-desc group-desc group-id))
(in-package :cl-trello.model)

(mito:connect-toplevel :sqlite3 :database-name #p"db.sqlite3")
(mito:deftable todo-group ()
  ((id :col-type :text
       :primary-key t)
   (desc :col-type :text))
  (:conc-name group-))

(mito:deftable todo ()
  ((desc :col-type :text)
   (done-p :col-type :boolean
           :initform nil)
   (group :col-type todo-group)))

(defun group= (g1 g2)
  (string= (group-id g1) (group-id g2)))

#+nil
(progn
  (mito:create-dao 'todo-group :id "HI" :desc "Helpful Info")
  (mito:create-dao 'todo-group :id "TD" :desc "To Do")
  (mito:create-dao 'todo-group :id "IP" :desc "In Progress")
  (mito:create-dao 'todo-group :id "RL" :desc "Ready for Launch"))

#+nil
(progn
  (create-todo "HI" "What is this?")
  (create-todo "HI" "Target launch date: ")
  (create-todo "TD" "Add some blink")
  (create-todo "TD" "Support custom groups")
  (create-todo "IP" "In Progress")
  (create-todo "IP" "Get it done"))

(defun create-todo (group desc)
  (mito:create-dao 'todo :group (make-instance 'todo-group :id group) :desc desc))

(defun get-todos ()
  (loop for g in (serapeum:assort
                   (mito:select-dao 'todo
                     (mito:includes 'todo-group))

                   :key #'todo-group
                   :test #'group=)
        collect (cons (todo-group (car g)) g)))
