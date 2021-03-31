;;;; gpxtools.lisp

(in-package #:gpxtools)

(defun meters-to-feet (val)
  (* 3.281 val))

(defun meters-to-miles (val)
  (* (/ 1.0 1609.344) val))

(defun string-to-float (str)
  (with-input-from-string
   (in str)
   (coerce (read in) 'double-float)))

(defgeneric to-vec3 (pt))

(defstruct utm-pt
  (northing 0.0 :type double-float)
  (easting 0.0 :type double-float)
  (zone 0 :type fixnum)
  (ele 0.0 :type double-float))


(defstruct gpx-pt
  (lat 0.0 :type double-float)
  (lon 0.0 :type double-float)
  (ele 0.0 :type double-float)
  (time "" :type string))

(defmethod to-vec3 ((pt gpx-pt))
  (with-slots (lat lon ele) pt
    (values (vec3 lat lon ele))))

(defun to-utm (pt &key (zone nil))
  (let ((utm (utm:lat-lon-to-utm (gpx-pt-lat pt) (gpx-pt-lon pt) :zone zone)))
    (make-utm-pt :easting (car utm) :northing (cadr utm) :zone (caddr utm) :ele (gpx-pt-ele pt))))

(defstruct gpx-segment
  (points () :type list)
  (point-count 0 :type integer)
  (max-lat -361.0d0 :type double-float)
  (min-lat 361.0d0 :type double-float)
  (max-lon -361.0d0 :type double-float)
  (min-lon 361.0d0 :type double-float))

(defstruct gpx-track
  (name "" :type string)
  (segments () :type list))

(defstruct gpx-file
  (tracks () :type list))

(defun format-iso (tm)
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (decode-universal-time tm)
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" yr mon day hr min sec)))

(defgeneric write-gpx (el stream)
  (:documentation "Write a GPX element to a file."))

(defmethod write-gpx ((pt gpx-pt) (stm stream))
  (format stm "<trkpt lat=\"~,9f\" lon=\"~,9f\"><ele>~,9f</ele><time>~a</time></trkpt>~%"
          (gpx-pt-lat pt)
          (gpx-pt-lon pt)
          (gpx-pt-ele pt)
          (gpx-pt-time pt)))

(defmethod write-gpx ((seg gpx-segment) (stm stream))
  (format stm "<trkseg>")
  (loop for i in (gpx-segment-points seg) do
       (write-gpx i stm))
  (format stm "</trkseg>"))

(defmethod write-gpx ((track gpx-track) (stm stream))
  (format stm "<trk><name>~a</name>" (gpx-track-name track))
  (loop for seg in (gpx-track-segments track) do
       (write-gpx seg stm))
  (format stm "</trk>"))

(defmethod write-gpx ((file gpx-file) (file-name string))
  (with-open-file
      (stream file-name :direction :output)
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?><gpx version=\"1.0\" creator=\"gpxtools\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.topografix.com/GPX/1/0\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">")
    (format stream "<time>~a</time>" (format-iso (get-universal-time)))
    (loop for track in (gpx-file-tracks file) do
         (write-gpx track stream))
    (format stream "</gpx>")))

(defun read-gpx (file-name)
  (let ((doc (cxml:parse-file file-name (cxml-dom:make-dom-builder)))
        (rval ()))
    (labels
        ((process-track
             (track-node)
           (let ((segs ())
                 (tname ""))
             (xpath:do-node-set
                 (node (xpath:evaluate "gpx:trkseg" track-node))
               (let ((name (xpath:evaluate "string(gpx:name)" track-node)))
                 (setf tname name))
               (setf segs (cons (process-trackseg node) segs)))
             (make-gpx-track :segments segs :name tname)))

         (process-trackseg
             (seg-node)
	   (let ((track (make-gpx-segment)))
             (xpath:do-node-set
                 (node (xpath:evaluate "gpx:trkpt" seg-node))
               (let ((np (process-trackpt node)))
                 (setf (gpx-segment-points track) (cons np (gpx-segment-points track)))
                 (setf (gpx-segment-max-lat track) (max (gpx-pt-lat np) (gpx-segment-max-lat track)))
                 (setf (gpx-segment-min-lat track) (min (gpx-pt-lat np) (gpx-segment-min-lat track)))
                 (setf (gpx-segment-max-lon track) (max (gpx-pt-lon np) (gpx-segment-max-lon track)))
                 (setf (gpx-segment-min-lon track) (min (gpx-pt-lon np) (gpx-segment-min-lon track)))
                 (incf (gpx-segment-point-count track))))
             track))

         (process-trackpt
             (pt-node)
	   (let ((lat (string-to-float (xpath:evaluate "string(@lat)" pt-node)))
                 (lon (string-to-float (xpath:evaluate "string(@lon)" pt-node)))
                 (ele (string-to-float (xpath:evaluate "string(gpx:ele)" pt-node)))
                 (time (xpath:evaluate "string(gpx:time)" pt-node)))
             (make-gpx-pt :lat lat :lon lon :ele ele :time time))))

      (xpath:with-namespaces
	  (("gpx" (xpath:evaluate "namespace-uri(/*)" doc)))
        (xpath:do-node-set
            (node (xpath:evaluate "/gpx:gpx/gpx:trk" doc))
          (setf rval (cons (process-track node) rval)))))
    (make-gpx-file :tracks rval)))

(defun distance-between (p1 p2)
  (let* ((pt1 (to-utm p1))
         (pt2 (to-utm p2 :zone (utm-pt-zone pt1) ))
         (ndiff (- (utm-pt-northing pt1) (utm-pt-northing pt2)))
         (ediff (- (utm-pt-easting pt1) (utm-pt-easting pt2)))
         (eldiff (- (utm-pt-ele pt1) (utm-pt-ele pt2)))
         (ndiff2 (* ndiff ndiff))
         (ediff2 (* ediff ediff))
         (eldiff2 (* eldiff eldiff)))
    (sqrt (+ ndiff2 ediff2 eldiff2))))

;; (defgeneric distance (el)
;;   (:documentation "Compute the distance of the element."))

;; (defmethod distance ((seg gpx-segment))
;;   (loop for i in (gpx-segment-points seg)
;; 		for j in (cdr (gpx-segment-points seg))
;; 		summing (distance-between i j) into total
;; 		finally (return total)))

;; (defmethod distance ((track gpx-track))
;;   (loop for seg in (gpx-track-segments track)
;; 		summing (distance seg) into total
;; 		finally (return total)))

;; (defmethod distance ((file gpx-file))
;;   (loop for track in (gpx-file-tracks file)
;; 		summing (distance track)))

(defun elevation-diff (p1 p2)
  (- (gpx-pt-ele p1) (gpx-pt-ele p2)))

(defun ele-gain (p1 p2)
  (let ((diff (elevation-diff p1 p2)))
    (if (< 0.0 diff)
        diff
        0.0)))

(defun ele-loss (p1 p2)
  (let ((diff (elevation-diff p1 p2)))
    (if (> 0.0 diff)
        diff
        0.0)))

(defgeneric traverse2 (el func)
  (:documentation "Traverse the GPX element and sum the results of (func point[i] point[i+1])"))

(defmethod traverse2 ((seg gpx-segment) func)
  (loop for i in (gpx-segment-points seg)
     for j in (cdr (gpx-segment-points seg))
     summing (apply func (list i j)) into total
     finally (return total)))

(defmethod traverse2 ((track gpx-track) func)
  (loop for seg in (gpx-track-segments track)
     summing (traverse2 seg func) into total
     finally (return total)))

(defmethod traverse2 ((file gpx-file) func)
  (loop for track in (gpx-file-tracks file)
     summing (traverse2 track func)))

(defgeneric collect-points (el)
  (:documentation "Traverse the GPX element and sum the results of (func point[i] point[i+1])"))

(defmethod collect-points ((seg gpx-segment))
  (gpx-segment-points seg))

(defmethod collect-points ((track gpx-track))
  (loop for seg in (gpx-track-segments track)
     appending (collect-points seg) into rval
     finally (return rval)))

(defmethod collect-points ((file gpx-file))
  (loop for track in (gpx-file-tracks file)
     appending (collect-points track) into rval
     finally (return rval)))

(defun elevation-gain (el)
  (traverse2 el #'ele-gain))

(defun elevation-loss (el)
  (traverse2 el #'ele-loss))

(defun distance (el)
  (traverse2 el #'distance-between))

(defun get-summary (gpx &key (units 'metric))
  (let ((eg (elevation-gain gpx))
        (el (elevation-loss gpx))
        (dist (distance gpx))
        (shortunit (if (eq units 'imperial) "feet" "meters"))
        (longunit (if (eq units 'imperial) "miles" "kilometers")))
    (list 
     (list 'total-elevation-gain (if (eq units 'imperial) (meters-to-feet eg) eg) shortunit)
     (list 'total-elevation-lost (if (eq units 'imperial) (meters-to-feet el) el) shortunit)
     (list 'total-distance (if (eq units 'imperial) (meters-to-miles dist) (/ dist 1000.0)) longunit))))

(defun summarize (gpx &key (units 'metric))
  (let ((eg (elevation-gain gpx))
        (el (elevation-loss gpx))
        (dist (distance gpx))
        (shortunit (if (eq units 'imperial) "feet" "meters"))
        (longunit (if (eq units 'imperial) "miles" "kilometers")))
  (format t "Total elevation gain: ~a ~a~%" (if (eq units 'imperial) (meters-to-feet eg) eg) shortunit)
  (format t "Total elevation loss: ~a ~a~%" (if (eq units 'imperial) (meters-to-feet el) el) shortunit)
  (format t "Total distance:       ~a ~a~%" (if (eq units 'imperial) (meters-to-miles dist) (/ dist 1000.0)) longunit)))

(defun elevation-plot (gpx &key (file-name))
  (let ((all-pts (collect-points gpx))
        (total-distance 0.0)
        (new-points ()))
    (loop for i in all-pts
          for j in (cdr all-pts)
          do
          (incf total-distance (distance-between i j))
          (push (list (meters-to-miles total-distance) (meters-to-feet (gpx-pt-ele i))) new-points))
    (adw-charting:with-chart (:line 1600 1200)
                             (adw-charting:add-series "Elevation" new-points)
                             (adw-charting:set-axis :y "Elevation (feet)")
                             (adw-charting:set-axis :x "Distance (miles)")
                             (adw-charting:save-file file-name))))

(defun find-loop (gpx &key (eps 0.001))
  (let* ((all-pts (collect-points gpx))
         (first-pt (car all-pts)))
    (format t "Checking ~a points~%" (length all-pts))
    (loop for i in (cdr all-pts)
          for j upto (- (length all-pts) 1) do
          (let ((this-dist (distance-between first-pt i)))
            (if (< this-dist eps)
                (format t "Point ~a is ~a meters away from the start!~%" j this-dist))))))

(defun simplify (gpx &key (dist 0.1))
  (let* ((all-pts (collect-points gpx))
         (cur-pt (car all-pts))
         (new-pts (list cur-pt)))

    (loop for i in (cdr all-pts) do
          (let ((this-dist (distance-between cur-pt i)))
            (cond ((> this-dist dist)
                   (setf cur-pt i)
                   (push cur-pt new-pts)))))

    
    (make-gpx-file
     :tracks (list (make-gpx-track
                    :segments (list (make-gpx-segment :points (reverse new-pts) :point-count (length new-pts)))
                    :name "Simplified")))))
                   

(defun gpx-file-from-track (track)
  (make-gpx-file :tracks (list track)))

(defun read-directory-to-points (directory)
  (let* ((files (directory (format nil "~a/*.gpx" directory)))
         (all-points (apply
                      (curry #'concatenate 'list)
                      (mapcar
                       (compose (curry #'mapcar #'to-vec3) #'gpxtools:collect-points #'gpxtools:read-gpx)
                       files))))
    (make-array (length all-points) :element-type 'vec3 :initial-contents all-points :adjustable nil)))



(defun read-directory-to-segments (directory)
  (let* ((files (directory (format nil "~a/*.gpx" directory)))
         (all-segments (apply
                      (curry #'concatenate 'list)
                      (mapcar
                       (compose (lambda (points)
                                  (loop
                                     for (from goes-to) on points by #'cdr
                                     when goes-to
                                     collect (cons from goes-to)))
                                (curry #'mapcar #'to-vec3)
                                #'gpxtools:collect-points
                                #'gpxtools:read-gpx)
                       files))))
    (make-array (length all-segments) :element-type 'vec3 :initial-contents all-segments :adjustable nil)))
