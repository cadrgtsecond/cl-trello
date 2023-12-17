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
          (group (cdr (assoc :group params :test #'string=))))
      (check-type desc string)
      (check-type group string)
      (a:if-let (todo (model:create-todo group desc))
        (ok-html (templates:todo-element todo))
        `(500 () ())))))

(setf (ningle:route *app* "/groups/:group" :method :PUT)
  (lambda (params)
    (let ((todos (loop for p in params
                       when (string= (car p) "item")
                       collect (parse-integer (cdr p))))
          (group (cdr (assoc :group params))))
      (print todos)
      (print group)
      (print "------------")
      (finish-output)
    `(204 ()))))

(setf (ningle:route *app* "/todos/:id" :method :DELETE)
  (lambda (params)
    (let ((id (cdr (assoc :id params))))
      (mito:delete-dao (make-instance 'model:todo :id id)))))

(defun start ()
  (clack:clackup
    (lack:builder
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
