(defpackage cl-trello
  (:use :cl :iterate)
  (:local-nicknames (:model cl-trello.model)
                    (:templates cl-trello.templates)
                    (:a alexandria)))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

;; Serve css
(setf (ningle:route *app* "/css/index.css")
  (lambda (params)
    `(200 (:content-type "text/css") (,(defcss:file->string templates:*index.css*)))))

(defmacro ok-html (html &key headers)
  ``(200 ,,headers (,(spinneret:with-html-string ,html))))

(setf (ningle:route *app* "/" :method :GET)
  (constantly '(301 (:location "/todos") (""))))

(setf (ningle:route *app* "/todos" :method :GET)
  (lambda (params)
    (declare (ignore params))
    (ok-html (templates:index (model:get-todos)))))

(setf (ningle:route *app* "/groups/:group" :method :POST)
  (lambda (params)
    (let ((desc (cdr (assoc "desc" params :test #'string=)))
          (group (parse-integer (cdr (assoc :group params :test #'string=)))))
      (check-type desc string)
      (check-type group integer)
      (a:if-let (todo (model:create-todo group desc))
        (ok-html (templates:todo-element todo))
        `(500 () ())))))

(setf (ningle:route *app* "/groups/:group" :method :PUT)
  (lambda (params)
    (model:reorder-todos
     ;; A little repetition is okay
      :to (parse-integer (cdr (assoc "to" params :test #'string=)))
      :from (parse-integer (cdr (assoc "from" params :test #'string=)))
      :oldord (parse-integer (cdr (assoc "oldord" params :test #'string=)))
      :neword (parse-integer (cdr (assoc "neword" params :test #'string=))))
    `(204 ())))

(setf (ningle:route *app* "/groups/:group/:order" :method :DELETE)
  (lambda (params)
    (let ((group (parse-integer (cdr (assoc :group params))))
          (order (parse-integer (cdr (assoc :order params)))))
      (mito:delete-dao (make-instance 'model:todo :group group :order order)))))

(defun start ()
  (clack:clackup
    (lack:builder
      (:mito '(:sqlite3 :database-name "db.sqlite3"))
      :backtrace
      (:static :path "/vendor/"
               :root #p"vendor/")
      (:static :path "/js/"
               :root #p"js/")
      *app*)
    :server :hunchentoot))

#+nil
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
