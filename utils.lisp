(in-package #:image-board)

;;; utils

(defvar *title* nil)
(defvar *css-file* nil)

(defun start-server (&key (port 4242))
  (make-instance 'mp-store :directory "~/data/object-store/"
                           :subsystems (list
                                        (make-instance 'store-object-subsystem)
                                        (make-instance 'blob-subsystem)))
  (start (make-instance 'easy-acceptor :address "0.0.0.0" :port port)))

(defmacro define-easy-handler-with-url (name uri args &body body)
  `(progn (defun ,(intern (format nil "~a-URL" name)) (&optional ,@args)
            (with-output-to-string (url)
              (format url "~a?" ,uri)
              ,@(loop :for arg :in args
                      :collect `(when ,arg
                                  (format url ,(format nil "&~(~a~)=~~a" arg)
                                          ,arg)))))
          (hunchentoot:define-easy-handler (,name :uri ,uri) ,args
            ,@body)))

;;; sha256

(defvar *secret* nil)

(defun hex (bytes)
  (ironclad:byte-array-to-hex-string bytes))

(defun hmac-sha256 (text)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array *secret*)
                                  :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (hex (ironclad:hmac-digest hmac))))

;;; Page

(defun head ()
  (with-html
    (:head
     (:title *title*)
     (:link :rel "stylesheet" :href "/style.css"))))

(defun nav ()
  (with-html
    (:nav :class "flex"
          (:a :href "/posts" "Logo")
          (:a :href "/posts" "Home")
          (:a :href "/user/home" "My Account")
          (:a :href (view-posts-url) "Posts")
          (:a :href "/tags" "Tags")
          (:form :style "margin-left: auto;"
                 :action "/search"
                 :accept-charset "UTF-8"
                 :method "post"
                 (:input :type "text"
                         :name "search"
                         :placeholder "Search")
                 (:button :type "submit" "Search")))))

(defmacro page-body (&body body)
  `(with-html-string
     (:doctype)
     (:html
      (head)
      (:body
       (nav)
       ,@body))))
