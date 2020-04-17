#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(defun ensure-attribute-name (thing)
  (etypecase thing
    (symbol (string-downcase thing))
    (string thing)))

(defmacro set-attributes (element &body attributes)
  (let ((elg (gensym "ELEMENT")))
    `(let ((,elg ,element))
       ,@(loop for (key val) on attributes by #'cddr
               collect (if (eql key '-)
                           `(plump:make-text-node ,elg ,val)
                           `(setf (plump:attribute ,elg (ensure-attribute-name ,key)) ,val)))
       ,elg)))

(defmacro make-element (parent tag-name &body attributes)
  `(set-attributes (plump:make-element ,parent (ensure-attribute-name ,tag-name))
     ,@attributes))

(defun format-time (time format)
  (etypecase time
    (local-time:timestamp
     (local-time:format-timestring NIL time :format format :timezone local-time:+utc-zone+))
    (integer
     (format-time (local-time:universal-to-timestamp time) format))))

(defun ensure-string (content)
  (etypecase content
    (string content)
    (plump:node
     (let ((plump:*tag-dispatchers* plump:*html-tags*))
       (plump:serialize content NIL)))))
