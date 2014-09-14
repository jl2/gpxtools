This is a small library for working with GPX files in Common Lisp.

Parts of the code are a little ugly, and it doesn't have many features, but it is still useful already.

Here is a sample usage from the REPL:

```commonlisp
* (ql:quickload 'gpxtools)
To load "gpxtools":
  Load 1 ASDF system:
    gpxtools
; Loading "gpxtools"
.....
(GPXTOOLS)
* (defparameter *gpx*
    (gpxtools:read-gpx "/Users/jeremiah/gpx_tracks/precarious_climb.gpx"))
Processing track: ACTIVE LOG
*GPX*
* (gpxtools:summarize *gpx*)
Total elevation gain: 3567.271946242661d0 feet
Total elevation loss: -3538.885209324304d0 feet
Total elevation loss: 6.437161570619994d0 miles
NIL
* (gpxtools:elevation-plot *gpx* :file-name "/Users/jeremiah/gpx_tracks/precarious.png")
collect-points returning 1906 points
Plotting 1905 points.
#P"/Users/jeremiah/gpx_tracks/precarious.png"
* 
```
![Screenshot](http://www.laroccophoto.com/photos/i-8FHW96c/0/L/i-8FHW96c-L.png "Elevation Profile")
