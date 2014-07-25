(in-package :cl-user)

(asdf:defsystem :icfp2014
  :serial t
  :components ((:file "package")
               (:file "gcc" :depends-on ("package")))
  :depends-on (:iterate :alexandria))
