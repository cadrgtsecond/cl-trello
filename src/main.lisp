(defpackage cl-trello
  (:use :cl)
  (:local-nicknames (:model cl-trello.model)
                    (:templates cl-trello.templates)))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/" :method :GET)
  (constantly '(301 (:location "/todos") (""))))

(setf (ningle:route *app* "/todos" :method :GET)
  (lambda (params)
    (declare (ignore params))
    `(200 () (,(templates:index.html (model:get-todos))))))

(setf (ningle:route *app* "/groups/:group" :method :POST)
  (lambda (params)
    (let ((desc (cdr (assoc "desc" params :test #'string=)))
          (group (cdr (assoc :group params :test #'string=))))
       (if (eql (length desc) 0)
           `(400 () ())
           (progn
             (model:create-todo desc group)
             (templates:todo-element desc))))))

(defun start ()
  (clack:clackup
    ;; TODO: Use Envy
    (lack:builder
      :backtrace
      (:static :path "/css/"
               :root #p"css/")
      (:static :path "/vendor/"
               :root #p"vendor/")
      *app*)
    :server :hunchentoot))

#+nil
(ten:compile-template #p"src/templates.html" :cl-trello.templates)
#+nil
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
