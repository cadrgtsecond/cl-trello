(defpackage cl-trello
  (:use :cl)
  (:local-nicknames (:model cl-trello.model)
                    (:a alexandria))
  (:import-from :flute
                flute:h
                flute:define-element))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

(define-element page () (h
  (html
    (head
      (meta :name "viewport" :content "width=device-width, initial-scale=1")
      (meta :name "charset" :content "UTF-8")
      (link :rel "stylesheet" :href "css/index.css")
      (script :src "/vendor/htmx.js" ""))
    (body '() flute:children))))

(defun ok-html (html &key headers)
  `(200 ,headers (,(flute:elem-str html))))

(setf (ningle:route *app* "/" :method :GET)
  (constantly '(301 (:location "/todos") (""))))

(define-element todo-element (todo) (h
  (li.todo-element
    (button.todo-element__button
      :hx-delete (format nil "/todos/~a" nil)
      :hx-target "closest li"
      :hx-swap "outerHTML" "-")
    (model:todo-desc todo))))

(define-element group-card (group todos) (h
  (div.group-card
    (h3.group-card__title (model:group-desc group))
    (ul.group-card__todo-list '()
      (loop for todo in todos
            collect (todo-element :todo todo))))))

(define-element index (todos) (h
  (page
    (h1 "CL Trello")
    (div.group-list '()
      (loop for (group . todos) in todos
            collect (group-card :group group :todos todos))))))

(setf (ningle:route *app* "/todos" :method :GET)
  (lambda (params)
    (declare (ignore params))
    (ok-html (index :todos (model:get-todos)))))

(setf (ningle:route *app* "/groups/:group" :method :POST)
  (lambda (params)
    (let ((desc (cdr (assoc "desc" params :test #'string=)))
          (group (cdr (assoc :group params :test #'string=))))
      (a:if-let (todo (model:create-todo group desc))
        (ok-html (todo-element todo))))))

(setf (ningle:route *app* "/todos/:id" :method :DELETE)
  (lambda (params)
    (let ((id (cdr (assoc :id params))))
      (mito:delete-dao (make-instance 'model:todo :id id)))))

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
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
