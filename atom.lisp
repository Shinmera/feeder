#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defclass atom (format)
  ())

(defmethod source-has-format-p ((source plump-dom:root) (format atom))
  (loop for child across (plump:children source)
        thereis (and (string-equal "feed" (plump:tag-name child))
                     (string-equal "http://www.w3.org/2005/Atom" (plump:get-attribute child "xlmns")))))

(defmethod parse-feed ((source plump-dom:root) (format atom))
  (let ((feeds ()))
    (with-child (child source :feed)
      (push (parse-to 'feed child format) feeds))
    (nreverse feeds)))

(defun parse-atom-content (node &optional (type (plump:attribute node "type")))
  (cond ((or (null type)
             (string= "" type)
             (string-equal "text" type)
             (string-equal "text/plain" type))
         (text node))
        ((or (string-equal "html" type)
             (string-equal "text/html" type))
         (plump:parse (text node)))
        ((or (string-equal "xhtml" type)
             (string-equal "application/xhtml+xml" type))
         (change-class (plump:clone-node node T) 'plump:root))
        (T
         (restart-case (error "Do not know how to parse content of type ~s" type)
           (use-type (type)
             :report "Supply a different content type to use."
             (parse-atom-content node type))
           (treat-as-plaintext ()
             :report "Treat the content as plaintext."
             (text node))
           (continue ()
             :report "Do not parse the content."
             NIL)))))

(defmethod parse-to ((date (eql 'date)) (node plump:element) (format atom))
  (local-time:parse-rfc3339-timestring (text node) :fail-on-error NIL))

(defmethod parse-to ((link link) (node plump:element) (format atom))
  (setf (url link) (plump:attribute node "href"))
  (setf (relation link) (plump:attribute node "rel"))
  (setf (content-type link) (plump:attribute node "type"))
  (setf (language link) (plump:attribute node "hreflang"))
  (setf (title link) (plump:attribute node "title")))

(defmethod parse-to ((person person) (node plump:element) (format atom))
  (with-children (child node)
    (:name
     (setf (name person) (text child)))
    (:email
     (setf (email person) (text child)))
    (:uri
     (setf (link person) (parse-to 'link child format)))))

(defmethod parse-to ((generator generator) (node plump:element) (format atom))
  (setf (link generator) (plump:attribute node "uri"))
  (setf (version generator) (plump:attribute node "version"))
  (setf (name generator) (text node)))

(defmethod parse-to ((item authored-item) (node plump:element) (format atom))
  (with-children (child node)
    (:title
     (setf (title item) (parse-atom-content child)))
    (:id
     (setf (id item) (text child)))
    (:updated
     (setf (updated-on item) (parse-to 'date child format)))
    (:rights
     (setf (rights item) (text child)))
    (:link
     (setf (link item) (parse-to 'link child format)))
    (:author
     (push (parse-to 'person child format) (authors item)))
    (:contributor
     (push (parse-to 'person child format) (contributors item)))
    (:category
     (with-child (term child :term)
       (push (text term) (categories item))))))

(defmethod parse-to ((entry entry) (node plump:element) (format atom))
  (call-next-method)
  (with-children (child node)
    (:published
     (setf (published-on entry) (parse-to 'date child format)))
    (:source
     (with-children (sub child)
       (:id
        (setf (source entry) (text id)))
       (:link
        (setf (source entry) (parse-to 'link link format)))))
    (:summary
     (setf (summary entry) (parse-atom-content child)))
    (:content
     (setf (content entry) (parse-atom-content child)))))

(defmethod parse-to ((feed feed) (node plump:element) (format atom))
  (call-next-method)
  (with-children (child node)
    (:subtitle
     (setf (summary feed) (parse-atom-content child)))
    (:generator
     (setf (generator feed) (parse-to 'generator child format)))
    (:logo
     (setf (logo feed) (text child)))
    (:entry
     (push (parse-to 'entry child format) (content feed)))))

(defmethod serialize-to ((target plump:nesting-node) (date local-time:timestamp) (format atom))
  (plump:make-text-node target (format-time date local-time:+rfc3339-format+)))

(defmethod serialize-to ((target plump:nesting-node) (person person) (format atom))
  (make-element target :name - (name person))
  (when (email person)
    (make-element target :email - (email person)))
  (when (link person)
    (make-element target :uri - (url person))))

(defmethod serialize-to ((target plump:nesting-node) (generator generator) (format atom))
  (make-element target :generator
    :uri (url generator)
    :version (version generator)
    - (name generator)))

(defmethod serialize-to ((target plump:nesting-node) (link link) (format atom))
  (make-element target :link
    :href (url link)
    :rel (relation link)
    :type (content-type link)
    :hreflang (language link)
    :title (title link)))

(defmethod serialize-to ((target plump:nesting-node) (string string) (format atom))
  (setf (plump:attribute target "type") "text")
  (plump:make-text-node target string))

(defmethod serialize-to ((target plump:nesting-node) (node plump:node) (format atom))
  (setf (plump:attribute target "type") "html")
  (plump:make-text-node target (ensure-string node)))

(defmethod serialize-to ((target plump:nesting-node) (item authored-item) (format atom))
  (serialize-to (make-element target :title) (title item) format)
  (typecase (id item)
    (link
     (make-element target :id - (url (id item))))
    (T
     (make-element target :id - (princ-to-string (id item)))))
  (serialize-to (make-element target :updated) (updated-on item) format)
  (when (rights item)
    (make-element target :rights - (rights item)))
  (typecase (link item)
    (null)
    (link
     (serialize-to target (link item) format))
    (string
     (make-element target :link - (link item))))
  (dolist (author (authors item))
    (serialize-to (make-element target :author) author format))
  (dolist (contributor (contributors item))
    (serialize-to (make-element target :contributor) contributor format))
  (dolist (category (categories item))
    (make-element target :category :term category)))

(defmethod serialize-to ((target plump:nesting-node) (entry entry) (format atom))
  (let ((target (make-element target :entry)))
    (call-next-method target entry format)
    (when (published-on entry)
      (serialize-to (make-element target :published) (published-on entry) format))
    (when (source entry)
      (let ((source (make-element target :source)))
        (make-element source :title - (title (source entry)))
        (serialize-to source (source entry) format)))
    (serialize-to (make-element target :summary) (summary entry) format)
    (when (content entry)
      (serialize-to (make-element target :content) (content entry) format))))

(defmethod serialize-to ((target plump:nesting-node) (feed feed) (format atom))
  (let ((target (make-element target :feed
                  :xlmns "http://www.w3.org/2005/Atom")))
    (call-next-method target feed format)
    (serialize-to (make-element target :subtitle) (summary feed) format)
    (when (generator feed)
      (serialize-to target (generator feed) format))
    (when (logo feed)
      (make-element target :logo - (url (logo feed))))
    (dolist (entry (content feed))
      (serialize-to target entry format))))
