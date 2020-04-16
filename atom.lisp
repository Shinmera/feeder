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

(defmethod parse-feed ((source plump-dom:node) (format atom)))

(defmethod serialize-feed ((feed feed) (format atom) &optional stream))
