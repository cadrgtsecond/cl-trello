(defpackage cl-trello.model
  (:use :cl :sxql)
  (:documentation "Structs and functions to pull data from the database")
  (:export todo get-todos create-todo todo-desc group-desc group-id todo-id))
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
   (group :references (todo-group id))))

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
  (mito:create-dao 'todo :group group :desc desc))

(defun plist->dao (class plist &rest mappings)
  "Useful function to convert a plist into a mito type. See `get-todos` for an example"
  (apply
    #'mito:make-dao-instance
    class
    (loop for (key attr) on mappings by #'cddr
          appending `(,key ,(getf plist attr)))))
(defun plist->dao* (class &rest mappings)
  "`plist->dao` but curried"
  (lambda (i) (apply #'plist->dao class i mappings)))
#+nil
(describe
  (plist->dao
    'todo
    '(:GROUP "HI" :GROUP-DESC "Helpful Info" :DESC "What is this?" :ID 1 :DONE-P 0)
    :id :id
    :desc :desc))

(defun get-todos ()
  (mapcar
    (lambda (g)
      (list*
         (plist->dao 'todo-group (car g) :id :group :desc :group_desc)
         (mapcar (plist->dao* 'todo :desc :desc :id :id) g)))
    (serapeum:assort
      (mito:retrieve-by-sql
        (select ((:as :todo_group.id :group)
                 (:as :todo_group.desc :group_desc)
                 :todo.desc :todo.id :done_p)
          (from :todo_group)
          (left-join :todo :on (:= :todo_group.id :group))))
      :key (lambda (e) (getf e :group))
      :test #'string=)))
