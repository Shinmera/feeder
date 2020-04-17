#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.feeder
  (:use #:cl)
  (:shadow #:atom #:format)
  ;; protocol.lisp
  (:export
   #:remote-item
   #:link
   #:url
   #:person
   #:name
   #:email
   #:generator
   #:name
   #:version
   #:link
   #:url
   #:relation
   #:content-type
   #:language
   #:title
   #:authored-item
   #:id
   #:categories
   #:authors
   #:contributors
   #:published-on
   #:updated-on
   #:rights
   #:language
   #:link
   #:title
   #:summary
   #:content
   #:feed
   #:cache-time
   #:generator
   #:logo
   #:webmaster
   #:entry
   #:comment-section
   #:source
   #:format
   #:xml-format
   #:source-has-format-p
   #:parse-feed
   #:serialize-feed
   #:parse-to
   #:serialize-to)
  ;; rss.lisp
  (:export
   #:rss)
  ;; atom.lisp
  (:export
   #:atom))
