(defpackage cl-trello.templates
  (:use :cl :iter :defcss)
  (:local-nicknames (:a :alexandria)
                    (:model :cl-trello.model))
  (:export todo-element index *index.css*))
(in-package cl-trello.templates)

(push "hx-" spinneret:*unvalidated-attribute-prefixes*)
(push "js-" spinneret:*unvalidated-attribute-prefixes*)

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
  :padding (:var --size-2)
  :margin 0
  :color (:var --text-color)
  :background (:var --primary-background))

(defcss (nice-button (:in *index.css*))
  ;; Reset notrmal styles
  :border none

  :background (:var --primary-color)
  :color (:var --primary-background)
  :border-radius (:var --radius-2))
        
(defmacro with-page (head &rest body)
  `(spinneret:with-html
    (:doctype)
    (:html
      (:head
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:meta :name "charset" :content "UTF-8")
        (:link :rel "stylesheet" :href "/vendor/openprops.min.css")
        (:link :rel "stylesheet" :href "/css/index.css")
        (:script :src "/vendor/htmx.js" "")
        ,@head)
      (:body ,@body))))

(defcss (todo-element-s (:in *index.css*))
  :display flex
  :gap (:var --size-fluid-1)

  (button
    :width (:var --size-6)
    :height (:var --size-6)
    :font-size (:var --font-size-4)
    :align-self center))

(defun todo-element (todo)
  "A single todo element"
  (spinneret:with-html
    (:li :class (style-class todo-element-s)
      (:button
        :class (style-class nice-button)
        :hx-delete (format nil "/groups/~a/~a" (model:todo-group todo)
                                              (model:todo-order todo))
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

(defcss (todo-form (:in *index.css*))
  :display flex
  :gap (:var --size-2)
  :height (:var --size-6)

  (input
    :background (:var --gray-1)
    :border (:var --border-size-1) solid (:var --gray-4)
    :flex-grow 1
    :font-size (:var --font-size-3)
    :border-radius (:var --radius-2))
  ((:and input :focus)
    :outline (:var --border-size-1) solid (:var --gray-9))
  (button
    :width (:var --size-6)
    :font-size (:var --font-size-4)))

(defun group-card (group todos)
  "A card containing many todos"
  (spinneret:with-html
    (:div
      :class (style-class group-card-s)
      (:h3 (model:group-desc group))
      (:ul
        :hx-put (format nil "/groups/~a" (model:group-id group))
        :hx-trigger "reordered"
        :hx-vals "js:{to: event.to, from: event.from, oldord: event.oldord, neword: event.neword}"
        :data-groupid (model:group-id group)
        :js-todos t
        (loop for todo in todos
              when (model:todo-order todo)
              collect (todo-element todo)))
      (:form
        :class (style-class todo-form)
        :hx-post (format nil "/groups/~a" (model:group-id group))
        :hx-target "previous ul"
        :hx-swap "beforeend"
        (:input :name "desc")
        (:button :class (style-class nice-button) :type "submit" "+")))))

(defcss (group-container (:in *index.css*))
  :display flex
  :flex-direction row
  :justify-content space-evenly
  :gap (:var --size-fluid-1))

(defun index (todos)
  (with-page
     ((:title "Todos")
      (:script :src "/vendor/sortable.js")
      (:script :defer "" :src "/js/index.js" ""))
    (:h1 "CL Trello")
    (:div
      :class (style-class group-container)
      (loop for (group . todos) in todos
            do (group-card group todos)))))
