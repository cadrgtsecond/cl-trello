(defpackage cl-trello.model
  (:use :cl :sxql)
  (:documentation "Structs and functions to pull data from the database")
  (:export todo
           get-todos
           reorder-todos
           create-todo
           todo-desc
           todo-order
           todo-group
           group-desc
           group-id))
(in-package :cl-trello.model)

(mito:connect-toplevel :sqlite3 :database-name #p"db.sqlite3")

(mito:deftable todo-group ()
  ((id :col-type :integer
       :primary-key t
       :initform (fields (select ((:coalesce (:+ 1 (:max :id)) 0))
                           (from :todo_group))))
   (desc :col-type :text))
(:conc-name group-)
(:documentation "A group of todos. The id indicates the ordering"))

(mito:deftable todo ()
  ((order :col-type :integer)
   (desc :col-type :text)
   (done-p :col-type :boolean
           :initform nil)
   (group :references todo-group))
(:primary-key order group))

(defmethod initialize-instance :after ((self todo) &key id group)
  "Ensure id is incrementing and unique"
  (unless id
    (setf (todo-order self) (fields
                               (select ((:coalesce (:+ 1 (:max :todo.order)) 0))
                                  (from :todo)
                                  (where (:= :group group)))))))

#+nil
(progn
  (mito:create-dao 'todo-group :desc "Helpful Info")
  (mito:create-dao 'todo-group :desc "To Do")
  (mito:create-dao 'todo-group :desc "In Progress")
  (mito:create-dao 'todo-group :desc "Ready for Launch"))

#+nil
(progn
  (create-todo 0 "What is this?")
  (create-todo 0 "Target launch date: ")
  (create-todo 1 "Add some blink")
  (create-todo 1 "Support custom groups")
  (create-todo 1 "Setup end to end testing")
  (create-todo 2 "In Progress")
  (create-todo 2 "Get it done"))

(defun create-todo (group desc)
  (mito:create-dao 'todo :group group :desc desc))
#+nil
(insert-into :todo
  (set= :group "HI"
        :desc "Desc"
        :order (fields (select ((:+ 1 (:max :order))) (from :todo)))))

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

(defun get-todos ()
  (mapcar
    (lambda (g)
      (list*
         (plist->dao 'todo-group (car g) :id :group :desc :group-desc)
         (mapcar (plist->dao* 'todo :group :group :order :order :desc :desc) g)))
    (serapeum:assort
      (mito:retrieve-by-sql
        (select ((:as :todo_group.id :group)
                 (:as :todo_group.desc :group-desc)
                 :todo.desc :todo.order :done_p)
          (from :todo_group)
          (left-join :todo :on (:= :todo_group.id :todo.group))
          (order-by :group :todo.order)))
      :key (lambda (e) (getf e :group)))))

(defun reorder-todos (&key to from neword oldord)
  "Reorders todos"
  (dbi:with-transaction mito:*connection*
    (mito:execute-sql
      (update 'todo
        (set= :order (:+ :order 1)
              :group to)
        (where (:and (:= :order oldord) (:= :group from)))))))
