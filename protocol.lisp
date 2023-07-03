(in-package #:org.shirakumo.feeder)

(defclass remote-item ()
  ((link :initarg :link :initform NIL :accessor link)))

(defmethod url ((item remote-item))
  (when (link item)
    (url (link item))))

(defmethod url ((string string))
  string)

(defclass person (remote-item)
  ((name :initarg :name :initform (arg! :NAME) :accessor name)
   (email :initarg :email :initform (arg! :EMAIL) :accessor email)))

(defmethod print-object ((person person) stream)
  (print-unreadable-object (person stream :type T)
    (cl:format stream "~@[~a~]~@[ <~a>~]"
               (name person) (email person))))

(defclass generator (remote-item)
  ((name :initarg :name :initform (arg! :NAME) :accessor name)
   (version :initarg :version :initform NIL :accessor version)))

(defmethod print-object ((generator generator) stream)
  (print-unreadable-object (generator stream :type T)
    (cl:format stream "~@[~a~]~@[ ~a~]"
               (name generator) (version generator))))

(defclass link ()
  ((url :initarg :url :initform (arg! :URL) :accessor url)
   (relation :initarg :relation :initform NIL :accessor relation)
   (content-type :initarg :content-type :initform NIL :accessor content-type)
   (language :initarg :language :initform NIL :accessor language)
   (title :initarg :title :initform NIL :accessor title)))

(defmethod print-object ((link link) stream)
  (print-unreadable-object (link stream :type T)
    (cl:format stream "~a"
               (url link))))

(defclass authored-item (remote-item)
  ((id :initarg :id :initform (arg! :ID) :accessor id)
   (categories :initarg :categories :initform () :accessor categories)
   (authors :initarg :authors :initform () :accessor authors)
   (contributors :initarg :contributors :initform () :accessor contributors)
   (published-on :initarg :published-on :initform NIL :accessor published-on)
   (updated-on :initarg :updated-on :initform (local-time:now) :accessor updated-on)
   (rights :initarg :rights :initform NIL :accessor rights)
   (language :initarg :language :initform NIL :accessor language)
   (link :initform (arg! :LINK))
   (title :initarg :title :initform (arg! :TITLE) :accessor title)
   (summary :initarg :summary :initform (arg! :SUMMARY) :accessor summary)
   (content :initarg :content :initform NIL :accessor content)))

(defmethod print-object ((item authored-item) stream)
  (print-unreadable-object (item stream :type T)
    (cl:format stream "~s~@[ ~a~]"
               (title item) (id item))))

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

(defclass xml-format (format)
  ())

(defgeneric source-has-format-p (source format))
(defgeneric instance-for-type (type format))
(defgeneric parse-feed (source format))
(defgeneric serialize-feed (feed format))

(defgeneric parse-to (target thing format))
(defgeneric serialize-to (target thing format))

(defmethod instance-for-type (type (format format))
  (handler-bind ((argument-missing #'continue))
    (make-instance type)))

(defmethod source-has-format-p (source (format symbol))
  (source-has-format-p source (make-instance format)))

(defmethod source-has-format-p (source (format xml-format))
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (source-has-format-p (plump:parse source) format)))

(defmethod parse-feed (source (format symbol))
  (parse-feed source (make-instance format)))

(defmethod parse-feed (source (format xml-format))
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (parse-feed (plump:parse source) format)))

;;; KLUDGE: The way this is done SOURCE is parsed from XML many times,
;;;         once for each SOURCE-HAS-FORMAT-P, and once for the actual
;;;         PARSE-FEED
(defmethod parse-feed (source (format (eql T)))
  (loop for class in '(rss atom)
        for format = (make-instance class)
        do (when (source-has-format-p source format)
             (return (parse-feed source format)))
        finally (restart-case (error 'unknown-format :source source)
                  (use-value (value)
                    :report "Specify the format to use."
                    :interactive (lambda () (read *query-io*))
                    (parse-feed source value)))))

(defmethod parse-to ((name symbol) thing format)
  (let ((target (instance-for-type name format)))
    (parse-to target thing format)
    target))

(defmethod serialize-feed (feed (format symbol))
  (serialize-feed feed (make-instance format)))

(defmethod serialize-feed ((feed feed) (format xml-format))
  (let ((root (plump:make-root)))
    (set-attributes (plump:make-xml-header root)
      :version "1.0"
      :encoding "UTF-8")
    (serialize-to root feed format)
    root))
