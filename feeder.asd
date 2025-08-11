(asdf:defsystem feeder
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "RSS, Atom and general feed parsing and generating"
  :homepage "https://shinmera.com/docs/feeder/"
  :bug-tracker "https://shinmera.com/project/feeder/issues"
  :source-control (:git "https://shinmera.com/project/feeder.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "rss")
               (:file "atom")
               (:file "documentation"))
  :depends-on (:plump
               :local-time
               :documentation-utils))
