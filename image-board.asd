(asdf:defsystem #:image-board
  :description "Describe image-board here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:spinneret
               #:lass
               #:trivial-shell
               #:bknr.indices
               #:bknr.datastore
               #:clobber
               #:ironclad)
  :components ((:file "package")
               (:file "utils")
               (:file "users")
               (:file "tags")
               (:file "css")
               (:file "media")
               (:file "posts")))
