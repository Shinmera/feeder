#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defclass remote-item ()
  ((link :initarg :link :initform NIL :accessor link)))

(defmethod url ((item remote-item))
  (url (link item)))

(defmethod url ((string string))
  string)

(defclass person (remote-item)
  ((name :initarg :name :initform (cerror "NAME required") :accessor name)
   (email :initarg :email :initform (cerror "EMAIL required") :accessor email)))

(defclass generator (remote-item)
  ((name :initarg :name :initform (cerror "NAME required") :accessor name)
   (version :initarg :version :initform NIL :accessor version)))

(defclass link ()
  ((url :initarg :url :initform (cerror "URL required") :accessor url)
   (relation :initarg :relation :initform NIL :accessor relation)
   (content-type :initarg :content-type :initform NIL :accessor content-type)
   (language :initarg :language :initform NIL :accessor language)
   (title :initarg :title :initform NIL :accessor title)))

(defclass authored-item (remote-item)
  ((id :initarg :id :initform (cerror "ID required") :accessor id)
   (author :initarg :author :initform NIL :accessor author)
   (categories :initarg :categories :initform () :accessor categories)
   (authors :initarg :authors :initform () :accessor authors)
   (contributors :initarg :contributors :initform () :accessor contributors)
   (published-on :initarg :published-on :initform NIL :accessor published-on)
   (updated-on :initarg :updated-on :initform NIL :accessor updated-on)
   (rights :initarg :rights :initform NIL :accessor rights)
   (language :initarg :language :initform NIL :accessor language)
   (link :initform (cerror "LINK required"))
   (title :initarg :title :initform (cerror "TITLE required") :accessor title)
   (summary :initarg :summary :initform (cerror "SUMMARY required") :accessor summary)
   (content :initarg :content :initform NIL :accessor content)))

(defmethod url ((item authored-item))
  (let ((link (link item)))
    (etypecase link
      (link (url link))
      (string link))))

(defclass feed (authored-item)
  ((cache-time :initarg :cache-time :initform NIL :accessor cache-time)
   (generator :initarg :generator :initform NIL :accessor generator)
   (logo :initarg :logo :initform NIL :accessor logo)
   (webmaster :initarg :webmaster :initform NIL :accessor webmaster)))

(defclass entry (authored-item)
  ((comment-section :initarg :comment-section :initform NIL :accessor comment-section)
   (source :initarg :source :initform NIL :accessor source)))

(defclass format ()
  ())

(defgeneric source-has-format-p (source format))
(defgeneric parse-feed (source format))
(defgeneric serialize-feed (feed format))

(defgeneric parse-to (target thing format))
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

(defmethod parse-to ((name symbol) thing format)
  (parse-to (find-class name) thing format))

(defmethod parse-to ((class class) thing format)
  (let ((target (handler-bind ((error #'continue))
                  (make-instance class))))
    (serialize-to target thing format)
    target))

(defmethod serialize-feed (feed (format symbol))
  (serialize-feed feed (make-instance format)))

(defmethod serialize-feed ((feed feed) (format format))
  (let ((root (plump:make-root)))
    (set-attributes (plump:make-xml-header root)
      :version "1.0"
      :encoding "UTF-8")
    (serialize-to root feed format)))
