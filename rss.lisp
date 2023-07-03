(in-package #:org.shirakumo.feeder)

(defclass rss (xml-format)
  ())

(defmethod source-has-format-p ((source plump-dom:root) (format rss))
  (with-child (child source :rss)
    (return T)))

(defmethod parse-feed ((source plump-dom:root) (format rss))
  (let ((feeds ()))
    (with-child (child source :rss)
      (with-child (channel child :channel)
        (push (parse-to 'feed channel format) feeds)))
    (nreverse feeds)))

(defmethod parse-to ((date (eql 'date)) (node plump:element) (format rss))
  (or (ignore-errors (parse-rfc3339-alike (text node)))
      (parse-rfc822-alike (text node))))

;; TODO: handle Atom elements being included via Atom namespace

(defmethod parse-to ((html (eql 'html)) (node plump:element) (format rss))
  (let ((plump:*tag-dispatchers* plump:*html-tags*))
    (plump:parse (text node))))

(defmethod parse-to ((link link) (node plump:element) (format rss))
  (setf (url link) (text node)))

;;; Try to be smart about informal name formats.
(defmethod parse-to ((person person) (node plump:element) (format rss))
  (let ((text (text node)))
    (cond ((find #\< text) ;; Name <name@example.com>
           (let* ((open (position #\< text))
                  (close (or (position #\> text :start open) (length text))))
             (setf (email person) (subseq text (1+ open) close))
             (setf (name person) (cl:format NIL "~a ~a" (trim (subseq text 0 open)) (trim (subseq text close))))))
          ((find #\( text) ;; name@example.com (Name)
           (let* ((open (position #\( text))
                  (close (or (position #\) text :start open) (length text))))
             (setf (email person) (cl:format NIL "~a ~a" (trim (subseq text 0 open)) (trim (subseq text close))))
             (setf (name person) (subseq text (1+ open) close))))
          ((find #\  text) ;; Name name@example.com
           (let* ((parts (split #\  text))
                  (email (loop for part in parts
                               do (when (find #\@ part)
                                    (return part)))))
             (setf (email person) email)
             (setf (name person) (cl:format NIL "~{~a~^ ~}" (remove email parts)))))
          ((find #\@ text) ;; name@example.com
           (setf (email person) text))
          (T ;; Name
           (setf (name person) text)))))

(defmethod parse-to ((generator generator) (node plump:element) (format rss))
  (setf (name generator) (text node)))

(defmethod parse-to ((item authored-item) (node plump:element) (format rss))
  (with-children (child node)
    (:title
     (setf (title item) (text child)))
    (:link
     (setf (link item) (make-instance 'link :url (text child))))
    (:description
     (setf (summary item) (parse-to 'html child format)))
    (:guid
     (if (string-equal "true" (plump:attribute child "isPermaLink"))
         (setf (id item) (make-instance 'link :url (text child)))
         (setf (id item) (text child))))
    ("pubDate"
     (setf (published-on item) (parse-to 'date child format)))
    ("lastBuildDate"
     (setf (updated-on item) (parse-to 'date child format)))
    (:copyright
     (setf (rights item) (text child)))
    (:language
     (setf (language item) (text child)))
    (:category
     (push (text child) (categories item)))))

(defmethod parse-to ((entry entry) (node plump:element) (format rss))
  (call-next-method)
  (with-children (child node)
    (:author
     (push (parse-to 'person child format) (authors entry)))
    (:comments
     (setf (comment-section entry) (parse-to 'link child format)))
    (:source
     (setf (source entry) (make-instance 'link :title (text child) :url (plump:attribute child "url"))))
    (:content\:encoded
     (setf (content entry) (parse-to 'html child format)))))

(defmethod parse-to ((feed feed) (node plump:element) (format rss))
  (call-next-method)
  (with-children (child node)
    (:generator
     (setf (generator feed) (parse-to 'generator child format)))
    (:logo
     (with-child (url node :url)
       (setf (logo feed) (text url))))
    (:ttl
     (setf (cache-time feed) (parse-integer (text child))))
    ("managingEditor"
     (push (parse-to 'person child format) (authors feed)))
    ("webMaster"
     (setf (webmaster feed) (parse-to 'person child format)))
    (:item
     (push (parse-to 'entry child format) (content feed))))
  (setf (content feed) (nreverse (content feed))))

(defmethod serialize-to ((target plump:nesting-node) (date local-time:timestamp) (format rss))
  (plump:make-text-node target (format-time date local-time:+rfc-1123-format+)))

(defmethod serialize-to ((target plump:element) (person person) (format rss))
  (plump:make-text-node target (cond ((and (name person) (email person))
                                      (cl:format NIL "~a <~a>" (name person) (email person)))
                                     (T
                                      (or (name person) (! (email person)))))))

(defmethod serialize-to ((target plump:nesting-node) (item authored-item) (format rss))
  (make-element target :title - (! (title item)))
  (make-element target :link - (url (! (link item))))
  (make-element target :description - (ensure-string (! (summary item))))
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
    (serialize-to (make-element target "lastBuildDate") (updated-on item) format))
  (when (rights item)
    (make-element target :copyright - (rights item)))
  (when (language item)
    (make-element target :language - (language item)))
  (dolist (category (categories item))
    (make-element target :category - category)))

(defmethod serialize-to ((target plump:nesting-node) (entry entry) (format rss))
  (let ((target (make-element target :item)))
    (call-next-method target entry format)
    (let ((author (first (authors entry))))
      (when author
        (serialize-to (make-element target :author) author format)))
    (when (comment-section entry)
      (make-element target :comments - (url (comment-section entry))))
    (when (source entry)
      (make-element target :source
        :url (url (source entry))
        - (title (source entry))))
    (when (content entry)
      (make-element target :content\:encoded
        - (ensure-string (content entry))))))

(defmethod serialize-to ((target plump:nesting-node) (feed feed) (format rss))
  (let* ((rss (make-element target :rss
                :version "2.0"
                :xmlns\:content "http://purl.org/rss/1.0/modules/content/"))
         (target (make-element rss :channel)))
    (call-next-method target feed format)
    (when (generator feed)
      (make-element target :generator
        :url (url (generator feed))
        - (cl:format NIL "~a~@[ ~a~]" (name (generator feed)) (version (generator feed)))))
    (when (logo feed)
      (let ((image (make-element target :image)))
        (make-element image :url
          - (logo feed))
        (make-element image :title
          - "logo")
        (make-element image :link
          - (url feed))))
    (when (cache-time feed)
      (make-element target :ttl
        - (princ-to-string (cache-time feed))))
    (let ((author (first (authors feed))))
      (when author
        (serialize-to (make-element target "managingEditor") author format)))
    (when (webmaster feed)
      (serialize-to (make-element target "webMaster") (webmaster feed) format))
    (dolist (entry (content feed))
      (serialize-to target entry format))))
