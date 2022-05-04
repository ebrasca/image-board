(in-package #:image-board)

(define-persistent-class image (blob)
  ())

(define-persistent-class video (blob)
  ())

(define-persistent-class audio (blob)
  ())

(defun make-media (pathname content-type)
  (let* ((category (string-upcase
                    (first (split-sequence:split-sequence #\/ content-type))))
         (class (cond ((string= category "IMAGE")
                       'image)
                      ((string= category "VIDEO")
                       'video)
                      ((string= category "AUDIO")
                       'audio))))
    (make-blob-from-file pathname class :type content-type)))

(defun make-preview (pathname)
  (let ((preview-pathname (format nil "/tmp/preview/~a.jpg"
                                  (pathname-name pathname))))
    (ensure-directories-exist "/tmp/preview/")
    (trivial-shell:shell-command
     (format nil
             "ffmpeg -y -xerror -i \"~a\" -pix_fmt yuvj444p -filter:v \"scale='if(gte(iw,ih),512,-1):if(gte(ih,iw),512,-1)'\" -sws_flags area+full_chroma_inp+full_chroma_int \"~a\""
             pathname preview-pathname))
    (make-blob-from-file preview-pathname 'image :type "image/jpeg")))

(defun media-resolution (media)
  (trivial-shell:shell-command
   (format nil "ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=s=x:p=0 \"~a\""
           (blob-pathname media))))

(defun media-size (media)
  (let ((octets (blob-size media)))
    (cond ((< 0 (ash octets -40))
           (format nil "~a TiB" (ceiling (/ octets (ash 1 40)))))
          ((< 0 (ash octets -30))
           (format nil "~a GiB" (ceiling (/ octets (ash 1 30)))))
          ((< 0 (ash octets -20))
           (format nil "~a MiB" (ceiling (/ octets (ash 1 20)))))
          ((< 0 (ash octets -10))
           (format nil "~a kiB" (ceiling (/ octets (ash 1 10)))))
          (t
           (format nil "~a B" octets)))))

(defmethod render-preview ((media image))
  (with-html
    (:img :class "image"
          :src (media-url media))))

(defmethod render-preview ((media video))
  (with-html
    (:img :class "video"
          :src (media-url media))))

(defmethod render-preview ((media audio))
  (with-html
    (:img :class "audio"
          :src (media-url media))))

(defmethod render-content ((media image))
  (with-html
    (:div :class "content"
          (:img :src (media-url media)
                :style "max-width: 100vmin;max-height: 100vmin;height: auto"))))

(defmethod render-content ((media video))
  (with-html
    (:div :class "content"
          (:video :width "100%" :height "auto" :controls t
                  (:source :src (media-url media)
                           :type (blob-type media))
                  "Your browser does not support this video format."))))

(defmethod render-content ((media audio))
  (with-html
    (:div :class "content"
          (:audio :controls t
                  (:source :src (media-url media)
                           :type (blob-type media))
                  "Your browser does not support this audio format."))))

;;; Post File IO

(defun media-url (media)
  (format nil "/media/view?id=~a" (store-object-id media)))

(define-easy-handler (img :uri "/media/view") (id)
  (let ((blob (store-object-with-id (parse-integer id))))
    (when blob
      (setf (hunchentoot:content-type*) (blob-type blob))
      (blob-to-stream blob (send-headers)))))
