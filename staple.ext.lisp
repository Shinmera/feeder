(asdf:load-system :staple-markless)

(defpackage "feeder-docs"
  (:use #:cl)
  (:local-nicknames
   (#:feeder #:org.shirakumo.feeder)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "feeder-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :feeder))))
  'page*)

(defmethod staple:packages ((system (eql (asdf:find-system :feeder))))
  (list (find-package (string '#:org.shirakumo.feeder))))

#+sbcl
(defmethod staple:definition-wanted-p ((definition definitions:source-transform) (page page*)) NIL)
