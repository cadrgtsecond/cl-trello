(defpackage cl-trello
  (:use :cl))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/todos" :method :GET)
  (lambda (params)
    (declare (ignore params))
    `(200 () (,(cl-trello.templates:index.html
                 `(("Helpful Info" "What is this?" "Target launch date: ")
                   ("To Do" "Add Envy" "Support custom groups")
                   ("In Progress" "Work on getting it done")
                   ("Ready for Launch")))))))
(setf (ningle:route *app* "/" :method :GET)
  (constantly '(301 (:location "/todos") (""))))

(defun start ()
  (clack:clackup
    ;; TODO: Use Envy
    (lack:builder
      :backtrace
      (:static :path "/css/"
               :root #p"css/")
      *app*)
    :server :hunchentoot))

#+nil
(ten:compile-template #p"src/templates.html" :cl-trello.templates)
#+nil
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
