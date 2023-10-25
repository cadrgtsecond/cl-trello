(defpackage cl-trello
  (:use :cl))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/" :method :GET)
  (lambda (params)
    (declare (ignore params))
    `(200 () (,(cl-trello.templates:index.html)))))

(defun start ()
  (clack:clackup
    ;; TODO: Use Envy
    (lack:builder
      :backtrace
      (:static :path "/css/"
               :root #p"./css/")
      *app*)
    :server :hunchentoot))

#+nil
(ten:compile-template #p"templates/index.html" :cl-trello.templates)
#+nil
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
