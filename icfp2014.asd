(in-package :cl-user)

(asdf:defsystem :icfp2014
  :serial t
  :components ((:file "package"))
  :depends-on (:iterate :alexandria))
