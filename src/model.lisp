(defpackage cl-trello.model
  (:use :cl :sxql)
  (:documentation "Structs and functions to pull data from the database")
  (:export todos groups)
  (:local-nicknames (:fly datafly)))
(in-package :cl-trello.model)

(fly:connect-toplevel :sqlite3 :database-name #p"db.sqlite3")

#| 
create table todos
  (id int primary key,
   isdone boolean not null,
   desc text not null);
create table groups (id integer primary key, desc text unique);
create table todo_groups (todo_id int references todos, group_id int references groups);
|#

(defun group-desc->id (name)
  "Convert the group name to an integer by hashing"
  (mod (ironclad:octets-to-integer
                     (ironclad:digest-sequence
                     :md5
                     (babel:string-to-octets name)))
                   most-positive-fixnum))

(defun create-group (&key desc)
  (insert-into :groups
    (set= :desc desc
          :id (group-desc->id))))

(defun create-todo (&key (isdone 0) desc group)
  "Returns a query to create a todo. `group` may be a string(representing the description)
   or an integer(representing the todo_id)"
  (insert-into :todo_groups
    (set= :todo_id :todo_id
          :group_id (etypecase group
                      (number group)
                      (string (group-desc->id group)))
    (where
      (:in :todo_id (insert-into :todo
        (set= :isdone isdone
              :desc desc)
        (returning :id)))))))
