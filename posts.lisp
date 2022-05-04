(in-package #:image-board)

;;; Post

(define-persistent-class post ()
  ((user-id
    :read)
   (upload-date
    :read)
   (tags
    :update :index-type hash-list-index
            :index-initargs (:test #'equalp)
            :index-reader post-with-tag)
   (content
    :read)
   (content-preview
    :read)))

(defun make-post (user-id upload-date tags content content-preview)
  (make-instance 'post
                 :user-id user-id
                 :upload-date upload-date
                 :tags tags
                 :content content
                 :content-preview content-preview))

(defun next-post (id)
  (loop :for post :in (store-objects-with-class 'post)
        :for post-id := (store-object-id post)
        :when (< id post-id)
          :do (return post-id)))

(defun previous-post (id)
  (loop :for post :in (nreverse (store-objects-with-class 'post))
        :for post-id := (store-object-id post)
        :when (> id post-id)
          :do (return post-id)))

(defun post-date (post)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time
       (post-upload-date post))
    (format nil "~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    year month date hour minute second)))

(defun post-not-found (id)
  (page-body
    (:div "Post " id " not found.")))

(defun post-search-url (search)
  (format nil "/search?search=~a" search))

(hunchentoot:define-easy-handler (post-search :uri "/search") (search)
  (hunchentoot:redirect (view-posts-url 0 (if (string= "" search)
                                              nil
                                              search))
                        :protocol :http))

(defun random-post-url (em)
  (with-output-to-string (url)
    (format url "/post/random")
    (when em
      (format url "?em=~a" em))))

(hunchentoot:define-easy-handler (post-random :uri "/post/random") (em)
  (let* ((list (store-objects-with-class 'post))
         (post (nth (random (length list)) list)))
    (hunchentoot:redirect (if em
                              (post-url (store-object-id post))
                              (media-url (post-content post)))
                          :protocol :http)))

(defun delete-url (id)
  (format nil "/post/delete?id=~a" id))

(hunchentoot:define-easy-handler (post-delete :uri "/post/delete") (id)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let ((post (store-object-with-id (parse-integer id))))
    (cond ((/= (session-value :user-id)
               (post-user-id post))
           (abort-request-handler "401 Unauthorized"))
          (t
           (with-transaction ()
             ;; Update tag counter
             (mapcar #'(lambda (tag)
                         (decf (tag-counter tag)))
                     (post-tags post))
             (delete-object post))
           (hunchentoot:redirect (view-posts-url 0) :protocol :http)))))

(hunchentoot:define-easy-handler (edit-tags :uri "/edit-tags") (id tags)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let* ((id (parse-integer id))
         (post (store-object-with-id id))
         (old-tags (post-tags post))
         (new-tags (map-tags* (parse-tags tags))))
    (unless (tree-equal new-tags old-tags)
      (with-transaction ()
        (setf (post-tags post) new-tags)
        ;; Update tag counter
        (mapcar #'(lambda (tag)
                    (incf (tag-counter tag)))
                (set-difference new-tags old-tags))
        (mapcar #'(lambda (tag)
                    (decf (tag-counter tag)))
                (set-difference old-tags new-tags))))
    (hunchentoot:redirect (post-url id) :protocol :http)))

(defun render-post (id post)
  (page-body
    (let ((media (post-content post)))
      (:div
       :class "flex"
       (:aside
        (:ul
         (:li
          (let ((previous-post (previous-post id)))
            (if previous-post
                (:a :href (post-url previous-post)
                    "Previous")
                "Previous")))
         (:li
          (let ((next-post (next-post id)))
            (if next-post
                (:a :href (post-url next-post)
                    "Next")
                "Next")))
         (:li
          (:a :href (random-post-url t)
              "Random")))
        (:h5 "Tags")
        (:ul
         (dolist (tag (post-tags post))
           (:li
            (:a :href (post-search-url (tag-name tag))
                (tag-name tag))
            (tag-counter tag))))
        (:h5 "Statistics")
        (:ul
         (:li "ID:" id)
         (:li "Posted:" (post-date post))
         (:li "by:" (user-username (store-object-with-id (post-user-id post))))
         (:li "Resolution: " (media-resolution media))
         (:li "File size:" (media-size media)))
        (:h5 "Options")
        (:ul
         (:li
          (:a :href (media-url media)
              "View larger version"))
         (:li
          (:a :href (media-url media) :download t
              "Download larger version"))
         (:li
          (:a :href (delete-url id)
              "Delete")))
        (:form :action "/edit-tags"
               :accept-charset "UTF-8"
               :method "post"
               (:input :type "hidden"
                       :name "id"
                       :value id)
               (:input :type "text"
                       :name "tags"
                       :value (with-output-to-string (tags)
                                (dolist (tag (post-tags post))
                                  (format tags "~a " (tag-name tag)))))
               (:button :type "submit" "Edit tags")))
       (render-content media)))))

(defun post-url (id)
  (format nil "/post/view?id=~a" id))

(hunchentoot:define-easy-handler (post :uri "/post/view") (id)
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((id (parse-integer id))
         (post (store-object-with-id id)))
    (if post
        (render-post id post)
        (post-not-found id))))

;;; Posts

(define-easy-handler-with-url view-mode "/view-mode" (offset tags)
  (setf (session-value :view-mode)
        (not (session-value :view-mode)))
  (hunchentoot:redirect (view-posts-url offset tags) :protocol :http))

(define-easy-handler-with-url view-posts "/view/posts" (offset tags)
  (setf (hunchentoot:content-type*) "text/html")
  (page-body
    (let ((offset (parse-integer (or offset "0"))))
      (:nav (:a :href (view-posts-url (max 0 (- offset 20)) tags)
                "Previous")
            (:a :href (view-posts-url (+ 20 offset) tags)
                "Next")
            (:a :href (random-post-url nil)
                "Random")
            (:a :href (view-mode-url offset tags)
                (if (session-value :view-mode)
                    "Use Normal View"
                    "Use Raw View"))
            (:a :href "/post/upload" "Upload"))
      (:div :class "flex"
       (loop :with tags := (parse-tags tags)
             :for post :in (store-objects-with-class 'post)
             :for post-id := (store-object-id post)
             :when (subsetp tags (post-tags post) :test #'eq)
               :do (:a :href (if (session-value :view-mode)
                                 (media-url (post-content post))
                                 (post-url post-id))
                       (render-preview (post-content-preview post))))))))

(hunchentoot:define-easy-handler (upload :uri "/upload") (file tags)
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (let* ((pathname (first file))
         ;; (file-name (second file))
         (content-type (third file))
         (tags (map-tags* (parse-tags tags))))
    (with-transaction ()
      (make-post (session-value :user-id)
                 (get-universal-time)
                 tags
                 (make-media pathname content-type)
                 (make-preview pathname))
      ;; Update tag counter
      (mapcar #'(lambda (tag)
                  (incf (tag-counter tag)))
              tags)))
  (hunchentoot:redirect "/post/upload" :protocol :http))

(define-easy-handler (post-upload :uri "/post/upload") ()
  (unless (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (setf (hunchentoot:content-type*) "text/html")
  (page-body
    (:form :enctype "multipart/form-data"
           :action "/upload"
           :accept-charset "UTF-8"
           :method "post"
           (:table
            (:tbody
             (:tr
              (:th (:label :for "file" "File"))
              (:td (:input :type "file"
                           :name "file")))
             (:tr
              (:th (:label :for "tags" "Tags"))
              (:td (:input :type "text"
                           :name "tags"))))
            (:tfoot
             (:tr
              (:td :colspan "2"
                   (:input :type "submit"
                           :value "Upload"))))))))
