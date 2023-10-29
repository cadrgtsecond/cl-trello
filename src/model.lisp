(defpackage cl-trello.model
  (:use :cl :sxql)
  (:documentation "Structs and functions to pull data from the database")
  (:export todo get-todos create-todo))
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
  (create-todo "TD" "Add Envy")
  (create-todo "TD" "Support custom groups")
  (create-todo "IP" "In Progress")
  (create-todo "IP" "Get it done"))

(defun create-todo (group desc)
  (mito:create-dao 'todo :group (make-instance 'todo-group :id group) :desc desc))

;; TODO: Improve this shitty code
(defun get-todos ()
  (let* ((groups (mito:select-dao 'todo-group))
         (res (mapcar 'list groups))
         (todos (mito:select-dao 'todo
                 (mito:includes 'todo-group))))
    (loop
         for todo in todos
         do (push todo (cdr (assoc (todo-group todo) res :test #'group=)))
         finally (return res))))
