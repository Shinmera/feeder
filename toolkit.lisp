#|
 This file is a part of feeder
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.feeder)

(define-condition feed-condition (condition)
  ())

(define-condition argument-missing (feed-condition error)
  ((argument :initarg :argument))
  (:report (lambda (c s) (cl:format s "The argument~%  ~s~%is required but was not supplied."
                                    (slot-value c 'argument)))))

(define-condition nil-value (feed-condition error)
  ((form :initarg :form))
  (:report (lambda (c s) (cl:format s "The value of~%  ~s~%is required to be non-NIL."
                                    (slot-value c 'form)))))

(define-condition unknown-format (feed-condition error)
  ((source :initarg :source))
  (:report (lambda (c s) (cl:format s "The given source has an unknown format."))))

(defun arg! (argument)
  (restart-case (error 'argument-missing :argument argument)
    (use-value (value)
      :report "Use the supplied value instead."
      :interactive (lambda () (eval (read *query-io*)))
      value)
    (continue ()
      :report "Force the argument to NIL."
      NIL)))

(defmacro ! (form)
  `(or ,form
       (restart-case (error 'nil-value :form ',form)
         (use-value (value)
           :report "Use a different value instead."
           :interactive (lambda () (eval (read *query-io*)))
           value))))

(defun ensure-attribute-name (thing)
  (etypecase thing
    (symbol (string-downcase thing))
    (string thing)))

(defmacro set-attributes (element &body attributes)
  (let ((elg (gensym "ELEMENT")))
    `(let ((,elg ,element))
       ,@(loop for (key val) on attributes by #'cddr
               collect `(when ,val
                          ,(if (and (typep key 'symbol) (string= "-" key))
                               `(plump:make-text-node ,elg ,val)
                               `(setf (plump:attribute ,elg ,(ensure-attribute-name key)) ,val))))
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

(defmacro with-child ((name root tag-name) &body body)
  (let ((tag (gensym "TAG")))
    `(loop with ,tag = ,tag-name
           for ,name across (plump:children ,root)
           do (when (and (typep ,name 'plump:element)
                         (string-equal ,tag (plump:tag-name ,name)))
                ,@body))))

(defmacro with-children ((name root) &body clauses)
  (let ((tag-name (gensym "TAG-NAME")))
    `(loop for ,name across (plump:children ,root)
           do (when (typep ,name 'plump:element)
                (let ((,tag-name (plump:tag-name ,name)))
                  (cond ,@(loop for (tag . body) in clauses
                                collect `((string-equal ,tag ,tag-name)
                                          ,@body))))))))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Linefeed) string))

(defun text (entity)
  (trim (plump:text entity)))

(defun split (splitter string)
  (let ((parts ())
        (buffer (make-string-output-stream)))
    (flet ((end ()
             (let ((string (get-output-stream-string buffer)))
               (when (string/= "" string)
                 (push string parts)))))
      (loop for c across string
            do (if (char= c splitter)
                   (end)
                   (write-char c buffer))
            finally (end)))
    (nreverse parts)))

(defun month-digit (month)
  (position month local-time:+short-month-names+ :test #'string-equal))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun tz-offset (tz)
  ;; RFC822 TZ spec parsing. What a fucking mess.
  (let ((tz-end (or (position #\+ tz)
                    (position #\- tz)
                    (length tz))))
    (+ (* 60 (cond ((= 1 tz-end)
                    (let ((char (char-downcase (char tz 0))))
                      (cond ((char<= #\a char #\j)
                             (- (char-code #\a) (char-code char) 1))
                            ;; J is ignored in the spec, we just map J and K to the same to be extra lenient.
                            ((char<= #\k char #\m)
                             (- (char-code #\a) (char-code char)))
                            ((char<= #\n char #\y)
                             (- (char-code char) (char-code #\m)))
                            ;; Other crap is just treated as zero.
                            (T
                             0))))
                   ((prefix-p "ES" tz) -5)
                   ((prefix-p "ED" tz) -4)
                   ((prefix-p "CS" tz) -6)
                   ((prefix-p "CD" tz) -5)
                   ((prefix-p "MS" tz) -7)
                   ((prefix-p "MD" tz) -6)
                   ((prefix-p "PS" tz) -8)
                   ((prefix-p "PD" tz) -7)
                   (T 0)))
       (if (< tz-end (length tz))
           (+ (* 60 (parse-integer tz :start tz-end :end (+ 2 tz-end)))
              (parse-integer tz :start (+ 2 tz-end)))
           0))))
