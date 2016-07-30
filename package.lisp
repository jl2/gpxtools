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
           #:get-summary
           #:summarize
           #:collect-points
           #:elevation-plot
           #:find-loop
           #:simplify
           #:write-gpx
           #:gpx-file-tracks
           #:gpx-track-segments
           #:gpx-segment-points
           #:gpx-segment-point-count

           #:gpx-point-lat
           #:gpx-point-lon
           #:gpx-point-ele
           #:gpx-point-time

           #:gpx-file-from-track
           ))

