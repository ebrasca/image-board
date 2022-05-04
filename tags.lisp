(in-package #:image-board)

;;; Tags

(define-persistent-class tag ()
  ((name
    :update :index-type string-unique-index
            :index-reader tag-with-name
            :index-values all-tags)
   (aliases
    :update :index-type hash-list-index
            :index-initargs (:test #'equalp)
            :index-reader tag-with-alias
            :index-values all-aliases)
   (implications
    :update)
   (counter
    :update)))

(defun make-tag (name &optional (aliases nil) (implications nil))
  (make-instance 'tag
                 :counter 0
                 :name name
                 :aliases aliases
                 :implications implications))

(defun del-tag (tag)
  (with-transaction ()
    ;; Remove tag from posts
    (loop :for post :in (store-objects-with-class 'post)
          :when (member tag (post-tags post))
            :do (setf (post-tags post)
                      (remove tag (post-tags post))))
    ;; Remove tag from tags
    (loop :for tag :in (all-tags)
          :when (member tag (tag-implications tag))
            :do (setf (tag-implications tag)
                      (remove tag (tag-implications tag))))
    ;; Delete tag
    (delete-object tag)))

(defun add-alias (from to)
  (with-transaction ()
    ;; Merge aliases from tags
    (setf (tag-aliases to)
          (sort (union (cons (tag-name from) (tag-aliases from))
                       (tag-aliases to)
                       :test #'string=)
                #'string<))
    ;; Merge implications from tags
    (setf (tag-implications to)
          (sort (union (tag-implications from) (tag-implications to)
                       :test #'string=
                       :key #'tag-name)
                #'string<
                :key #'tag-name))
    ;; Update posts tags
    (loop :for post :in (store-objects-with-class 'post)
          :for tags := (map-tags* (post-tags post))
          :for counter := 0
          :do (when (member from (post-tags post))
                (setf (post-tags post)
                      (sort
                       (remove-duplicates
                        (cons to
                              (remove from
                                      (post-tags post))))
                       #'string<
                       :key #'tag-name))
                (unless (member to (post-tags post))
                  (incf counter)))
          :finally (incf (tag-counter to) counter))
    ;; Delete tag
    (delete-object from)))

(defun del-alias (tag alias)
  (with-transaction ()
    (setf (tag-aliases tag)
          (remove alias (tag-aliases tag) :test #'string=))))

(defun add-implication (from to)
  (with-transaction ()
    (setf (tag-implications from)
          (sort (cons to (tag-implications from))
                #'string<
                :key #'tag-name))
    ;; Update posts tags
    (loop :for post :in (store-objects-with-class 'post)
          :for old-tags := (post-tags post)
          :for new-tags := (map-tags* (post-tags post))
          :do (when (set-exclusive-or old-tags new-tags :test #'eq)
                (setf (post-tags post)
                      new-tags)
                ;; Update tag counter
                (mapcar #'(lambda (tag)
                            (incf (tag-counter tag)))
                        (set-difference new-tags old-tags))))))

(defun del-implication (tag impliation)
  (with-transaction ()
    (setf (tag-implications tag)
          (remove impliation (tag-implications tag)))))

(defun map-tags (fn tags)
  (labels ((map-tags-aux (tags)
             (cond ((typep tags 'standard-object)
                    (funcall fn tags)
                    (map-tags-aux (tag-implications tags)))
                   ((consp tags)
                    (map-tags-aux (car tags))
                    (map-tags-aux (cdr tags))))))
    (remove-duplicates
     (map-tags-aux tags))))

(defun map-tags* (tags)
  (let ((res '()))
    (map-tags #'(lambda (tag)
                  (push tag res))
              tags)
    (sort res #'string< :key #'tag-name)))

(defun tags-circularp (from to)
  (map-tags #'(lambda (tag)
                (when (eq from tag)
                  (return-from tags-circularp t)))
            to))

(defun tags-check-p (from to)
  (or (not from)
      (not to)
      (eq from to)
      (tags-circularp from to)))

(defun parse-tags (string)
  (if (string= "" string)
      nil
      (remove-duplicates
       (loop :for name :in (split-sequence:split-sequence
                            #\Space
                            (string-upcase
                             (string-trim '(#\Space)
                                          string)))
             :for tag := (or (tag-with-name name)
                             (car (tag-with-alias name))
                             (make-tag name))
             :when tag
               :collect tag))))

(defmacro tags-body (&body body)
  `(page-body
     (:nav (:a :href "/tags" "List")
           (:a :href "/tag/add-form" "Add tag")
           (:a :href "/alias/add-form" "Add alias")
           (:a :href "/implication/add-form" "Add implication"))
     ,@body))

(define-easy-handler-with-url tag-del "/tag/del" (tag-id)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (del-tag (store-object-with-id (parse-integer tag-id :junk-allowed t)))
  (hunchentoot:redirect "/tags" :protocol :http))

(hunchentoot:define-easy-handler (tag-add :uri "/tag/add") (tag)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let ((tag (string-upcase tag)))
    (cond ((or (string= "" tag)
               (tag-with-name tag))
           (abort-request-handler "Error adding tag"))
          (t
           (make-tag tag)
           (hunchentoot:redirect "/tags" :protocol :http)))))

(hunchentoot:define-easy-handler (tag-add-form :uri "/tag/add-form") ()
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (tags-body
    (:form :action "/tag/add"
           :accept-charset "UTF-8"
           :method "post"
           (:input :type "text"
                   :name "tag"
                   :placeholder "New tag")
           (:button :type "submit" "Create tag"))))

(define-easy-handler-with-url alias-del "/alias/del" (id alias)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (del-alias (store-object-with-id (parse-integer id :junk-allowed t))
             (string-upcase alias))
  (hunchentoot:redirect "/tags" :protocol :http))

(hunchentoot:define-easy-handler (alias-add :uri "/alias/add") (from to)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let ((from (tag-with-name (string-upcase from)))
        (to (tag-with-name (string-upcase to))))
    (cond ((tags-check-p from to)
           (abort-request-handler "Error adding alias"))
          (t
           (add-alias from to)
           (hunchentoot:redirect "/tags" :protocol :http)))))

(hunchentoot:define-easy-handler (alias-add-form :uri "/alias/add-form") ()
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (tags-body
    (:form :action "/alias/add"
           :accept-charset "UTF-8"
           :method "post"
           (:table
            (:thead
             (:tr (:th "From")
                  (:th "To")))
            (:tbody
             (:tr (:th (:input :type "text"
                               :name "from"
                               :placeholder "From"))
                  (:th (:input :type "text"
                               :name "to"
                               :placeholder "To"))
                  (:th (:button :type "submit" "Add alias"))))))))

(define-easy-handler-with-url implication-del "/implication/del"
    (tag-id implication-id)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (del-implication
   (store-object-with-id (parse-integer tag-id :junk-allowed t))
   (store-object-with-id (parse-integer implication-id :junk-allowed t)))
  (hunchentoot:redirect "/tags" :protocol :http))

(hunchentoot:define-easy-handler (implication-add :uri "/implication/add")
    (from to)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let ((from (tag-with-name (string-upcase from)))
        (to (tag-with-name (string-upcase to))))
    (cond ((tags-check-p from to)
           (abort-request-handler "Error adding implication"))
          (t
           (add-implication from to)
           (hunchentoot:redirect "/tags" :protocol :http)))))

(hunchentoot:define-easy-handler (implication-add-form
                                  :uri "/implication/add-form")
    ()
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (tags-body
    (:form :action "/implication/add"
           :accept-charset "UTF-8"
           :method "post"
           (:table
            (:thead
             (:tr (:th "From")
                  (:th "To")))
            (:tbody
             (:tr (:th (:input :type "text"
                               :name "from"
                               :placeholder "From"))
                  (:th (:input :type "text"
                               :name "to"
                               :placeholder "To"))
                  (:th (:button :type "submit" "Create implications"))))))))

(hunchentoot:define-easy-handler (tags-page :uri "/tags") ()
  (tags-body
    (:table
     (:thead
      (:tr (:th "Posts")
           (:th "Name")
           (:th "Aliases")
           (:th "Implications")
           (:th :colspan "1" "Action")))
     (:tbody
      (loop :for tag :in (all-tags)
            :for id := (store-object-id tag)
            :for name := (tag-name tag)
            :for aliases := (tag-aliases tag)
            :for implications := (tag-implications tag)
            :do (:tr (:td (tag-counter tag))
                     (:td
                      (:a :href (tag-del-url id)
                          name))
                     (:td
                      (loop :for alias :in aliases
                            :do (:a :href (alias-del-url id alias)
                                    alias)))
                     (:td
                      (loop :for implication :in implications
                            :for implication-id := (store-object-id implication)
                            :do (:a :href
                                    (implication-del-url id implication-id)
                                    (tag-name implication))))))))))
