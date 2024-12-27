;;;; package.lisp

(defpackage #:gpxtools
  (:use #:cl #:3d-vectors #:alexandria)
  (:export
   #:read-gpx
           #:elevation-plot

           #:distance

           #:elevation-gain
           #:elevation-loss
           #:elevation-diff

           #:distance-between

           
           #:meters-to-feet
           #:meters-to-miles

           #:get-summary
           #:summarize
           #:collect-points

           #:find-loop
           #:simplify
           #:write-gpx
           #:gpx-file-tracks
           #:gpx-track-segments
           #:gpx-segment-points
           #:gpx-segment-point-count

           #:gpx-pt-lat
           #:gpx-pt-lon
           #:gpx-pt-ele
           #:gpx-pt-time

           #:read-directory-to-points
           #:read-directory-to-segments
           #:find-segments

           #:lat
           #:lon
           #:ele
           #:gpx-file-from-track
           ))

