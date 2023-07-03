(asdf:defsystem feeder
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "RSS, Atom and general feed parsing and generating"
  :homepage "https://shinmera.github.io/feeder/"
  :bug-tracker "https://github.com/shinmera/feeder/issues"
  :source-control (:git "https://github.com/shinmera/feeder.git")
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
