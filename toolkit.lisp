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

(defun rfc822-tz-offset (tz)
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

(defun rfc3339-tz-offset (text)
  (cond ((string-equal "z" text)
         0)
        ((find #\: text)
         (destructuring-bind (hh mm) (split #\: text)
           (* 60
              (+ (* 60 (parse-integer hh))
                 (parse-integer mm)))))
        ((= 4 (length text))
         (* 60
            (+ (* 60 (parse-integer text :end 2))
               (parse-integer text :start 2))))))

(defun parse-rfc3339-alike (text)
  ;; RFC3339 date like 1994-11-06 8:49:37
  (let ((parts (cond ((find #\T text) (split #\T text))
                     ((find #\t text) (split #\t text))
                     (T (split #\  text))))
        y m d hh mm ss (offset 0))
    (destructuring-bind (date time) parts
      (cond ((find #\- date)
             (destructuring-bind (yy mm dd) (split #\- date)
               (setf y (parse-integer yy))
               (setf m (parse-integer mm))
               (setf d (parse-integer dd))))
            ((= 8 (length date))
             (setf y (parse-integer date :start 0 :end 4))
             (setf m (parse-integer date :start 4 :end 6))
             (setf d (parse-integer date :start 6 :end 8))))
      (let ((pos (or (position #\z time)
                     (position #\Z time)
                     (position #\+ time)
                     (position #\- time))))
        (when pos
          (setf offset (rfc3339-tz-offset (subseq time pos)))
          (setf time (subseq time 0 pos))))
      (cond ((find #\: time)
             (destructuring-bind (h m &optional s) (split #\: time)
               (setf hh (parse-integer h))
               (setf mm (parse-integer m))
               (setf ss (if s (parse-integer s) 0))))
            ((= 6 (length time))
             (setf hh (parse-integer time :start 0 :end 2))
             (setf mm (parse-integer time :start 2 :end 4))
             (setf ss (parse-integer time :start 4 :end 6))))
      (when (and y hh)
        (local-time:encode-timestamp 00 ss mm hh d m y :offset offset)))))

(defun parse-rfc822-alike (text)
  ;; RFC822 date like: Sun, 06 Nov 1994 08:49:37 GMT
  (let ((parts (split #\  text)))
    ;; Ignore day marker
    (when (and parts (not (digit-char-p (char (first parts) 0))))
      (pop parts))
    (destructuring-bind (day month year time &optional tz) parts
      (let ((d (parse-integer day))
            (m (month-digit month))
            (y (parse-integer year))
            (time (split #\: time))
            (offset (if tz (rfc822-tz-offset tz) 0)))
        ;; Deal with 2-character years. This sucks. Why would you ever do this??
        (when (= 2 (length year))
          (if (< 80 y)
              (incf y 1900)
              (incf y 2000)))
        (destructuring-bind (hh mm &optional (ss "0")) time
          (let ((hh (parse-integer hh))
                (mm (parse-integer mm))
                (ss (parse-integer ss)))
            (local-time:encode-timestamp
             0 ss mm hh d m y :offset offset)))))))
