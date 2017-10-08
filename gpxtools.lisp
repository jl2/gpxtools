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

(defgeneric to-v3 (pt))

(defclass utm-pt ()
  ((pt :initarg :pt :type vec3)
   (zone :initarg :zone :type fixnum)))

(defclass gpx-pt ()
  ((pt :initarg :pt :type vec3)
   (time :initarg :time :type string)))


(defun lat (pt)
  (vx (slot-value pt 'pt)))

(defun latitude (pt)
  (lat pt))


(defun lon (pt)
  (vy (slot-value pt 'pt)))

(defun longitude (pt)
  (lon pt))


(defun ele (pt)
  (vz (slot-value pt 'pt)))

(defun elevation (pt)
  (ele pt))

(defun pt-time (pt)
  (slot-value pt 'time))


(defun to-utm (pt &key (zone nil))
  (let ((utm (utm:lat-lon-to-utm (lat pt) (lon pt) :zone zone)))
	(make-instance 'utm-pt :pt (vec3 (car utm) (cadr utm) (ele pt)) :zone (caddr utm))))


(defclass gpx-segment ()
  ((points :initform nil :initarg :points :type list)
  (point-count :initform 0 :initarg :point-count :type fixnum)
  (max-lat :initform -361.0)
  (min-lat :initform 361.0)
  (max-lon :initform -361.0)
  (min-lon :initform 361.0)))

(defclass gpx-track ()
  ((name :initform "" :initarg :name :type string)
   (segments :initform () :initarg :segments :type list)))

(defclass gpx-file ()
  ((tracks :initform () :initarg :tracks :type list)))

(defun format-iso (tm)
    (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                         (decode-universal-time tm)
                         (declare (ignorable dow dst-p tz))
                         (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" yr mon day hr min sec)))

(defgeneric write-gpx (el stream)
  (:documentation "Write a GPX element to a file."))

(defmethod write-gpx ((pt gpx-pt) (stm stream))
  (format stm "<trkpt lat=\"~,9f\" lon=\"~,9f\"><ele>~,9f</ele><time>~a</time></trkpt>~%"
          (lat pt)
          (lon pt)
          (ele pt)
          (pt-time pt)))

(defmethod write-gpx ((seg gpx-segment) (stm stream))
  (format stm "<trkseg>")
  (loop for i in (slot-value seg 'segment-points) do
        (write-gpx i stm))
  (format stm "</trkseg>"))

(defmethod write-gpx ((track gpx-track) (stm stream))
  (format stm "<trk><name>~a</name>" (slot-value track 'track-name))
  (loop for seg in (slot-value track 'track-segments) do
        (write-gpx seg stm))
  (format stm "</trk>"))

(defmethod write-gpx ((file gpx-file) (file-name string))
  (with-open-file
   (stream file-name :direction :output)
   (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?><gpx version=\"1.0\" creator=\"gpxtools\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.topografix.com/GPX/1/0\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">")
   (format stream "<time>~a</time>" (format-iso (get-universal-time)))
   (loop for track in (slot-value file 'file-tracks) do
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
            (make-instance 'gpx-track :segments segs :name tname)))

	  (process-trackseg
	   (seg-node)
	   (let ((track (make-instance 'gpx-segment)))
             (xpath:do-node-set
              (node (xpath:evaluate "gpx:trkpt" seg-node))
              (let ((np (process-trackpt node)))
			(setf (gpx-segment-points track) (cons np (gpx-segment-points track)))
			(setf (gpx-segment-max-lat track) (max (lat np) (gpx-segment-max-lat track)))
			(setf (gpx-segment-min-lat track) (min (lat np) (gpx-segment-min-lat track)))
			(setf (gpx-segment-max-lon track) (max (lon np) (gpx-segment-max-lon track)))
			(setf (gpx-segment-min-lon track) (min (lon np) (gpx-segment-min-lon track)))
			(incf (gpx-segment-point-count track))))
		 track))

	  (process-trackpt
	   (pt-node)
	   (let ((lat (string-to-float (xpath:evaluate "string(@lat)" pt-node)))
			 (lon (string-to-float (xpath:evaluate "string(@lon)" pt-node)))
			 (ele (string-to-float (xpath:evaluate "string(gpx:ele)" pt-node)))
                         (time (xpath:evaluate "string(gpx:time)" pt-node)))
		 (make-instance 'gpx-pt  :pt (vec3 lat lon ele) :time time))))

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

(defun get-summary (gpx &key (units 'imperial))
  (let ((eg (elevation-gain gpx))
        (el (elevation-loss gpx))
        (dist (distance gpx))
        (shortunit (if (eq units 'imperial) "feet" "meters"))
        (longunit (if (eq units 'imperial) "miles" "kilometers")))
    (list 
     (list 'total-elevation-gain (if (eq units 'imperial) (meters-to-feet eg) eg) shortunit)
     (list 'total-elevation-lost (if (eq units 'imperial) (meters-to-feet el) el) shortunit)
     (list 'total-distance (if (eq units 'imperial) (meters-to-miles dist) (/ dist 1000.0)) longunit))))

(defun summarize (gpx &key (units 'imperial))
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

(defun read-directory (directory)
  (let* ((files (directory (format nil "~a/*.gpx" directory)))
         (all-points (apply
                      (curry #'concatenate 'list)
                      (mapcar
                       (compose #'gpxtools:collect-points #'gpxtools:read-gpx)
                       files))))
    all-points))

(defun find-segments (points)
  (format t "Total number of points: ~a~%" (length points)))
