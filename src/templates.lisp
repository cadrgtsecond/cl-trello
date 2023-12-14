(defpackage cl-trello.templates
  (:use :cl :iter :defcss)
  (:local-nicknames (:a :alexandria)
                    (:model :cl-trello.model))
  (:export todo-element index *index.css*))
(in-package cl-trello.templates)

(push "hx-" spinneret:*unvalidated-attribute-prefixes*)

(defparameter *index.css* (make-css-file))

(defcss (:root (:global t) (:in *index.css*))
  :--text-color (:var --gray-8)
  :--primary-background (:var --gray-1)
  :--secondary-background (:var --gray-0)
  :--primary-color (:var --blue-9))

(defcss (* (:global t) (:in *index.css*))
  :box-sizing border-box)

(defcss (body (:global t) (:in *index.css*))
  :width auto
  :height 100vh
  :color (:var --text-color)
  :background (:var --primary-background))
        
(defmacro with-page (&rest body)
  `(spinneret:with-html
    (:doctype)
    (:html
      (:head
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:meta :name "charset" :content "UTF-8")
        (:link :rel "stylesheet" :href "/vendor/openprops.min.css")
        (:link :rel "stylesheet" :href "/css/index.css")
        (:script :src "/vendor/htmx.js" ""))
      (:body ,@body))))

(defcss (todo-element-s (:in *index.css*))
  :display flex
  :gap (:var --size-fluid-1)

  (button
    ;; Reset notrmal styles
    :padding none
    :border none

    :width (:var --size-6)
    :height (:var --size-6)
    :font-size (:var --font-size-4)

    :align-self center
    :background (:var --primary-color)
    :color (:var --primary-background)
    :border-radius (:var --radius-2))
  (input
    :background (:var --primary-background)
    :border 0px
    :font-size (:var --size-6)
    :flex-grow 1))

(defun todo-element (todo)
  "A single todo element. See `group-card` for the styling"
  (spinneret:with-html
    (:li :class (style-class todo-element-s)
      (:button
        :hx-delete (format nil "/todos/~a" nil)
        :hx-target "closest li"
        :hx-swap "outerHTML" "-")
      (model:todo-desc todo))))

(defcss (group-card-s (:in *index.css*))
  :background (:var --secondary-background)
  :width 100%
  :padding (:var --size-3)
  :border-radius (:var --radius-3)
  :box-shadow (:var --shadow-2)

  (h3
    :font-size (:var --font-size-3)
    :margin-top 0px)
  (ul
    :padding-left 0px
    :list-style-type none
    :line-height (:var --font-lineheight-5)))

(defun group-card (group todos)
  (spinneret:with-html
    (:div
      :class (style-class group-card-s)
      (:h3 (model:group-desc group))
      (:ul '()
        (loop for todo in todos
              collect (todo-element todo))))))

(defcss (group-container (:in *index.css*))
  :display flex
  :flex-direction row
  :justify-content space-evenly
  :gap (:var --size-fluid-1))

(defun index (todos)
  (with-page
    (:h1 "CL Trello")
    (:div
      :class (style-class group-container)
      (loop for (group . todos) in todos
            do (group-card group todos)))))
