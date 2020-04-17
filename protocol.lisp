#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defclass remote-item ()
  ((link :initarg :link :accessor link)))

(defmethod url ((item remote-item))
  (url (link item)))

(defmethod url ((string string))
  string)

(defclass person (remote-item)
  ((name :initarg :name :accessor name)
   (email :initarg :email :accessor email)))

(defclass generator (remote-item)
  ((name :initarg :name :accessor name)
   (version :initarg :version :accessor version)))

(defclass link ()
  ((url :initarg :url :accessor url)
   (relation :initarg :relation :accessor relation)
   (content-type :initarg :content-type :accessor content-type)
   (language :initarg :language :accessor language)
   (title :initarg :title :accessor title)))

(defclass authored-item (remote-item)
  ((id :initarg :id :accessor id)
   (author :initarg :author :accessor author)
   (categories :initarg :categories :accessor categories)
   (contributors :initarg :contributors :accessor contributors)
   (published-on :initarg :published-on :accessor published-on)
   (updated-on :initarg :updated-on :accessor updated-on)
   (rights :initarg :rights :accessor rights)
   (language :initarg :language :accessor language)
   (title :initarg :title :accessor title)
   (summary :initarg :summary :accessor summary)
   (content :initarg :content :accessor content)))

(defmethod url ((item authored-item))
  (let ((link (link item)))
    (etypecase link
      (link (url link))
      (string link))))

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
(defgeneric serialize-feed (feed format))
(defgeneric serialize-to (target thing format))

(defmethod source-has-format-p (source (format symbol))
  (source-has-format-p source (make-instance format)))

(defmethod parse-feed (source (format symbol))
  (parse-feed source (make-instance format)))

(defmethod parse-feed (source (format format))
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (parse-feed (plump:parse source) format)))

(defmethod parse-feed ((source plump:root) (format (eql T)))
  (loop for class in '(rss atom)
        for format = (make-instance class)
        do (when (source-has-format-p source format)
             (return (parse-feed source format)))
        finally (error "Source has unknown format.")))

(defmethod serialize-feed (feed (format symbol))
  (serialize-feed feed (make-instance format)))

(defmethod serialize-feed ((feed feed) (format format))
  (let ((root (plump:make-root)))
    (set-attributes (plump:make-xml-header root)
      :version "1.0"
      :encoding "UTF-8")
    (serialize-to root feed format)))
