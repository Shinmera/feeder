#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defclass rss (format)
  ())

(defmethod source-has-format-p ((source plump-dom:root) (format rss))
  (loop for child across (plump:children source)
        thereis (string-equal "rss" (plump:tag-name child))))

(defmethod parse-feed ((source plump-dom:root) (format rss))
  (let ((feeds ()))
    (with-child (child source :rss)
      (with-child (channel child :channel)
        (push (parse-to 'feed channel format) feeds)))
    (nreverse feeds)))

(defmethod serialize-to ((target plump:nesting-node) (date local-time:timestamp) (format rss))
  (plump:make-text-node target (format-time date local-time:+rfc-1123-format+)))

(defmethod serialize-to ((target plump:nesting-node) (item authored-item) (format rss))
  (make-element target :title - (title item))
  (make-element target :link - (url (link item)))
  (make-element target :description - (ensure-string (summary item)))
  (typecase (id item)
    (null)
    (link
     (make-element target :guid
       "isPermaLink" "true"
       - (url (id item))))
    (T
     (make-element target :guid
       "isPermaLink" "false"
       - (princ-to-string (id item)))))
  (when (published-on item)
    (serialize-to (make-element target "pubDate") (published-on item) format))
  (when (updated-on item)
    (serialize-to (make-element target "lastBuildDate") (published-on item) format))
  (when (rights item)
    (make-element target :copyright - (rights item)))
  (when (language item)
    (make-element target :language - (language item)))
  (dolist (category (categories item))
    (make-element target :category - category)))

(defmethod serialize-to ((target plump:nesting-node) (entry entry) (format rss))
  (let ((item (make-element target :item)))
    (call-next-method item entry format)
    (let ((author (first (authors entry))))
      (when author
        (make-element item :author - (email author))))
    (when (comment-section entry)
      (make-element item :comments - (url (comment-section entry))))
    (when (source entry)
      (make-element item :source
        :url (url (source entry))
        - (title (source entry))))
    (when (content entry)
      (make-element item :content\:encoded
        - (ensure-string (content entry))))))

(defmethod serialize-to ((target plump:nesting-node) (feed feed) (format rss))
  (let* ((rss (make-element target :rss
                :version "2.0"
                :xmlns\:content "http://purl.org/rss/1.0/modules/content/"))
         (channel (make-element rss :channel)))
    (call-next-method channel feed format)
    (when (generator feed)
      (make-element channel :generator
        :url (url (generator feed))
        - (cl:format NIL "~a~@[ ~a~]" (name (generator feed)) (version (generator feed)))))
    (when (logo feed)
      (let ((image (make-element channel :image)))
        (make-element image :url
          - (logo feed))
        (make-element image :title
          - "logo")
        (make-element image :link
          - (url feed))))
    (when (cache-time feed)
      (make-element channel :ttl
        - (princ-to-string (cache-time feed))))
    (let ((author (first (authors feed))))
      (when author
        (make-element target "managingEditor" - (email author))))
    (when (webmaster feed)
      (make-element channel "webMaster" - (email (webmaster feed))))
    (dolist (entry (content feed))
      (serialize-to channel entry format))))
