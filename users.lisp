(in-package #:image-board)

;;; User

(defvar *user-counter* -1)
(defvar *users* (make-hash-table))

(define-persistent-class user ()
  ((permissions :update)
   (username
    :read :index-type string-unique-index
          :index-reader user-with-name
          :index-values all-users)
   (hashed-password
    :update)))

(defun make-user (username hashed-password)
  (make-instance 'user :username username
                       :hashed-password hashed-password))

(hunchentoot:define-easy-handler (logout :uri "/logout") ()
  (remove-session (start-session))
  (hunchentoot:redirect "/user/home" :protocol :http))

(hunchentoot:define-easy-handler (login :uri "/login") (username password)
  (when (and (string/= "" username)
             (string/= "" password))
    (let ((user (user-with-name username)))
      (unless (and user (string= (user-hashed-password user)
                                 (hmac-sha256 password)))
        (abort-request-handler "401 Unauthorized"))
      (start-session)
      (setf (session-value :user-id) (store-object-id user)
            (session-value :username) username)
      (hunchentoot:redirect "/user/home" :protocol :http))))

(hunchentoot:define-easy-handler (user-login :uri "/user/login") ()
  (when (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (page-body
    (:div
     (:h4 "Login")
     (:form :action "/login"
            :accept-charset "UTF-8"
            :method "post"
            (:table
             (:tbody
              (:tr (:th "Username")
                   (:td (:input :type "text"
                                :name "username"
                                :required t)))
              (:tr (:th "Password")
                   (:td (:input :type "password"
                                :name "password"
                                :required t))))
             (:tfoot
              (:tr (:td :colspan "2"
                        (:input :type "submit"
                                :value "Login")))))))))

(hunchentoot:define-easy-handler (signup :uri "/signup")
    (username password0 password1)
  (when (and (string/= "" username)
             (string/= "" password0)
             (not (user-with-name username))
             (string= password0 password1))
    (let ((user (make-user username (hmac-sha256 password0))))
      (start-session)
      (setf (session-value :user-id) (store-object-id user)
            (session-value :username) username))
    (hunchentoot:redirect "/user/home" :protocol :http)))

(hunchentoot:define-easy-handler (user-signup :uri "/user/signup") ()
  (when (session-value :user-id)
    (hunchentoot:redirect "/user/home" :protocol :http))
  (page-body
    (:div
     (:h4 "Signup")
     (:form :action "/signup"
            :accept-charset "UTF-8"
            :method "post"
            (:table
             (:tbody
              (:tr (:th "Username")
                   (:td (:input :type "text"
                                :name "username"
                                :required t)))
              (:tr (:th "Password")
                   (:td (:input :type "password"
                                :name "password0"
                                :required t)))
              (:tr (:th "Confirm password")
                   (:td (:input :type "password"
                                :name "password1"
                                :required t))))
             (:tfoot
              (:tr (:td :colspan "2"
                        (:input :type "submit"
                                :value "Signup")))))))))

(hunchentoot:define-easy-handler (user-home :uri "/user/home") ()
  (page-body
    (if (session-value :user-id)
        (:div (:h2 (format nil "Hello ~a!" (session-value :username)))
              (:ul (:li (:a :href "/logout" "Logout"))
                   (:li (:a :href "/user/edit" "Settings"))
                   (:li (:a :href "/user/change-password" "Change Password"))))
        (:div (:h2 "You are not logged in.")
              (:ul (:li (:a :href "/user/login" "Log in"))
                   (:li (:a :href "/user/signup" "Sign up")))))))
