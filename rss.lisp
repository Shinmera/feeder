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

(defmethod parse-feed ((source plump-dom:node) (format rss)))

(defmethod serialize-feed ((feed feed) (format rss) &optional stream))
