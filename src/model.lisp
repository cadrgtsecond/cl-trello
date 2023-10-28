(defpackage cl-trello.model
  (:use :cl)
  (:documentation "Structs and functions to pull data from the database")
  (:export get-todos create-todo))
(in-package :cl-trello.model)

(defvar *db* `(("HI" "Helpful Info" "What is this?" "Target launch date: ")
               ("TD" "To Do" "Add Envy" "Support custom groups")
               ("IP" "In Progress" "Work on getting it done")
               ("RL" "Ready for Launch")))

(defun create-todo (desc group)
  (setf (cddr (assoc group *db* :test #'string=))
        (cons desc (cddr (assoc group *db* :test #'string=)))))

(defun get-todos () *db*)
