(defpackage cl-trello
  (:use :cl :iterate)
  (:local-nicknames (:model cl-trello.model)
                    (:a alexandria))
  (:import-from :flute
                flute:h
                flute:define-element))
(in-package :cl-trello)

(defvar *app* (make-instance 'ningle:app))

(defparameter *colors*
  ;; From Material UI: https://materialui.co/colors
  '(:primary-background "#fff"      ; white
    :secondary-background "#FAFAFA" ; gray 50
    :shadow-color "#B0BEC5"         ; blue gray 200
    :text-color "#37474F"           ; blue gray 800
    :primary-color "#0D47A1"        ; light blue 900
   ))

(defun colors->stylesheet (&optional (colors *colors*) (selector :root))
  "Takes in a plist of colors, and returns a css style"
  (lass:compile-and-write
      (iter (for (key val) on colors by #'cddr)
            (appending
              (list (a:format-symbol :keyword "--~a" key) val)
              into res)
            (finally (return (cons selector res))))))

(define-element page () (h
  (html
    (head
      (meta :name "viewport" :content "width=device-width, initial-scale=1")
      (meta :name "charset" :content "UTF-8")
      (link :rel "stylesheet" :href "css/index.css")
      (script :src "/vendor/htmx.js" "")
      (script :src "/vendor/css-scope-inline.js" "")
      (style '() (colors->stylesheet))
      (style '() "
      * {
        box-sizing: border-box;
        color: var(--text-color);
      }

      body {
        width: auto;
        height: 100vh;
        background: var(--primary-background);
      }"))
    (body '() flute:children))))

(defmacro inline-style (&body body)
  "Easily create an inline <style> tag. `&body` is a lass expression"
  `(flute:style ,(lass:compile-and-write `(me ,@body))))

(defun ok-html (html &key headers)
  `(200 ,headers (,(flute:elem-str html))))

(setf (ningle:route *app* "/" :method :GET)
  (constantly '(301 (:location "/todos") (""))))

(define-element todo-element (todo) (h
  "A single todo element. See `group-card` for the styling"
  (li
    (inline-style
      :display flex
      :gap 0.5rem

      (button
        :height 1.6rem
        :width 1.6rem
        :font-size 1.3rem

        :align-self center
        :background (:var --primary-color)
        :color (:var --primary-background)
        :border 0
        :border-radius 0.2rem)
      (input
        :background (:var --primary-background)
        :border 0
        :font-size 1.5rem
        :flex-grow 1))

    (button
      :hx-delete (format nil "/todos/~a" nil)
      :hx-target "closest li"
      :hx-swap "outerHTML" "-")
    (model:todo-desc todo))))

(define-element group-card (group todos) (h
  (div
    (inline-style
      :background (:var --secondary-background)
      :width 100%
      :padding 0.5rem
      :box-radius 0.2rem
      :box-shadow 0.1rem 0.1rem 0.2rem (:var --shadow-color)

      (h3
        :top 0
        :font-size 1rem)
      (ul
        :padding-left 0
        :list-style-type none
        (li
          :margin-bottom 0.5rem)))

    (h3 (model:group-desc group))
    (ul '()
      (loop for todo in todos
            collect (todo-element :todo todo))))))

(define-element index (todos) (h
  (page
    (h1 "CL Trello")
    (div '()
      (inline-style
        :display flex
        :flex-direction row
        :justify-content space-evenly
        :gap 1rem)
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
        (ok-html (todo-element todo))
        `(400 () ())))))

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
      *app*)
    :server :hunchentoot))

#+nil
(progn
  (defvar *handle* nil)
  (when *handle* (clack:stop *handle*))
  (setf *handle* (start)))
