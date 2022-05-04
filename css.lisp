(in-package #:image-board)

(hunchentoot:define-easy-handler (css :uri "/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (lass:compile-and-write
   '(a
     :padding-right "2px"
     :padding-left "2px")
   '((:and a
      (:or :link :visited))
     :text-decoration "none")
   '((:and a
      (:or :hover :active))
     :text-decoration "underline")
   '(ul
     :padding "0"
     :list-style-type "none")
   '((:and img
      (:or .image .audio .video))
     :max-height "200px"
     :max-width "200px")
   '(img.video
     :border "3px solid #0000ff")
   '(.flex
     :display "flex"
     :flex-flow "row wrap"
     :width "100%"
     :height "100%")))
