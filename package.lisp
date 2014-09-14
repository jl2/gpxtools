;;;; package.lisp

(defpackage #:gpxtools
  (:use #:cl)
  (:export #:read-gpx
           #:distance
           #:elevation-gain
           #:elevation-loss
           #:distance-between
           #:elevation-diff
           #:meters-to-feet
           #:meters-to-miles
           #:summarize
           #:collect-points
           #:elevation-plot
           ))

