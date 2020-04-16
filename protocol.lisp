#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defclass person ()
  ((name :initarg :name :accessor name)
   (link :initarg :link :accessor link)
   (email :initarg :email :accessor email)))

(defclass generator ()
  ((name :initarg :name :accessor name)
   (link :initarg :link :accessor link)
   (version :initarg :version :accessor version)))

(defclass link ()
  ((url :initarg :url :accessor url)
   (relation :initarg :relation :accessor relation)
   (content-type :initarg :content-type :accessor content-type)
   (language :initarg :language :accessor language)
   (title :initarg :title :accessor title)))

(defclass authored-item ()
  ((id :initarg :id :acccessor id)
   (author :initarg :author :acccessor author)
   (categories :initarg :categories :acccessor categories)
   (contributors :initarg :contributors :acccessor contributors)
   (published-on :initarg :published-on :acccessor published-on)
   (updated-on :initarg :updated-on :acccessor updated-on)
   (rights :initarg :rights :acccessor rights)
   (link :initarg :link :acccessor link)
   (language :initarg :language :acccessor language)
   (title :initarg :title :acccessor title)
   (summary :initarg :summary :acccessor summary)
   (content :initarg :content :acccessor content)))

(defclass feed (authored-item)
  ((cache-time :initarg :cache-time :accessor cache-time)
   (generator :initarg :generator :accessor generator)
   (logo :initarg :logo :accessor logo)
   (webmaster :initarg :webmaster :accessor webmaster)))

(defclass entry (authored-item)
  ((comment-section :initarg :comment-section :accessor comment-section)
   (source :initarg :source :accessor source)))

(defclass format ()
  ())

(defgeneric source-has-format-p (source format))
(defgeneric parse-feed (source format))
(defgeneric serialize-feed (feed format &optional stream))

(defmethod source-has-format-p (source (format symbol))
  (source-has-format-p source (make-instance format)))

(defmethod parse-feed (source (format symbol))
  (parse-feed source (make-instance format)))

(defmethod parse-feed ((source plump:root) (format (eql T)))
  (loop for class in '(rss atom)
        for format = (make-instance class)
        do (when (source-has-format-p source format)
             (return (parse-feed source format)))
        finally (error "Source has unknown format.")))

(defmethod serialize-feed (feed (format symbol) &optional stream)
  (serialize-feed feed (make-instance format) stream))

(defmethod serialize-feed :around (feed format &optional stream)
  (etypecase stream
    (null
     (with-output-to-string (stream)
       (call-next-method feed format stream)))
    ((eql T)
     (call-next-method feed format *standard-output*))
    (stream
     (call-next-method))))
