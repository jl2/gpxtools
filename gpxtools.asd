;;;; gpxtools.asd

(asdf:defsystem #:gpxtools
  :serial t
  :version 0.1
  :description "Tools to work with GPX files in Common Lisp"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:cxml
               #:xpath
               #:alexandria
               #:utm
               #:3d-vectors
               #:3d-matrices
               #:adw-charting
               #:adw-charting-vecto)
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "gpxtools")))

