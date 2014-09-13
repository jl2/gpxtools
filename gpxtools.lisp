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

(defstruct utm-pt
  (northing 0.0 :type double-float)
  (easting 0.0 :type double-float)
  (zone 0 :type fixnum)
  (ele 0.0 :type double-float))


(defstruct gpx-pt
  (lat 0.0 :type double-float)
  (lon 0.0 :type double-float)
  (ele 0.0 :type double-float))

(defun to-utm (pt)
  (let ((utm (utm:lat-lon-to-utm (gpx-pt-lat pt) (gpx-pt-lon pt))))
	(make-utm-pt :easting (car utm) :northing (cadr utm) :zone (caddr utm) :ele (gpx-pt-ele pt))))

(defstruct gpx-segment
  (points () :type list)
  (name "" :type string)
  (num-points 0 :type integer)
  (max-lat -361.0d0 :type double-float)
  (min-lat 361.0d0 :type double-float)
  (max-lon -361.0d0 :type double-float)
  (min-lon 361.0d0 :type double-float))

(defstruct gpx-track
  (segments () :type list))

(defstruct gpx-file
  (tracks () :type list))
  
(defun read-gpx (file-name)
  (let ((doc (cxml:parse-file file-name (cxml-dom:make-dom-builder)))
		(rval ()))
	(labels
	 ((process-track
	  (track-node)
	  (let ((segs ()))
		(xpath:do-node-set
		 (node (xpath:evaluate "gpx:trkseg" track-node))
		 (let ((name (xpath:evaluate "string(gpx:name)" track-node)))
		 (format t "Processing track: ~a~%" name)
		 (setf segs (cons (process-trackseg node name) segs))))
		(make-gpx-track :segments segs)))

	  (process-trackseg
	   (seg-node name)
	   (let ((track (make-gpx-segment :name name)))
		 (xpath:do-node-set
		  (node (xpath:evaluate "gpx:trkpt" seg-node))
		  (let ((np (process-trackpt node)))
			(setf (gpx-segment-points track) (cons np (gpx-segment-points track)))
			(setf (gpx-segment-max-lat track) (max (gpx-pt-lat np) (gpx-segment-max-lat track)))
			(setf (gpx-segment-min-lat track) (min (gpx-pt-lat np) (gpx-segment-min-lat track)))
			(setf (gpx-segment-max-lon track) (max (gpx-pt-lon np) (gpx-segment-max-lon track)))
			(setf (gpx-segment-min-lon track) (min (gpx-pt-lon np) (gpx-segment-min-lon track)))
			(incf (gpx-segment-num-points track))))
		 track))

	  (process-trackpt
	   (pt-node)
	   (let ((lat (string-to-float (xpath:evaluate "string(@lat)" pt-node)))
			 (lon (string-to-float (xpath:evaluate "string(@lon)" pt-node)))
			 (ele (string-to-float (xpath:evaluate "string(gpx:ele)" pt-node))))
		 (make-gpx-pt :lat lat :lon lon :ele ele))))

	 (xpath:with-namespaces
	  (("gpx" "http://www.topografix.com/GPX/1/0"))
	  (xpath:do-node-set
	   (node (xpath:evaluate "/gpx:gpx/gpx:trk" doc))
	   (setf rval (cons (process-track node) rval)))))
	(make-gpx-file :tracks rval)))

(defun distance-between (p1 p2)
  (let* ((pt1 (to-utm p1))
		 (pt2 (to-utm p2))
		 (ndiff (- (utm-pt-northing pt1) (utm-pt-northing pt2)))
		 (ediff (- (utm-pt-easting pt1) (utm-pt-easting pt2)))
		 (eldiff (- (utm-pt-ele pt1) (utm-pt-ele pt2)))
		 (ndiff2 (* ndiff ndiff))
		 (ediff2 (* ediff ediff))
		 (eldiff2 (* eldiff eldiff)))
	(sqrt (+ ndiff2 ediff2 eldiff2))))

(defgeneric distance (el)
  (:documentation "Compute the distance of the element."))

(defmethod distance ((seg gpx-segment))
  (loop for i in (gpx-segment-points seg)
		for j in (cdr (gpx-segment-points seg))
		summing (distance-between i j) into total
		finally (return total)))

(defmethod distance ((track gpx-track))
  (loop for seg in (gpx-track-segments track)
		summing (distance seg) into total
		finally (return total)))

(defmethod distance ((file gpx-file))
  (loop for track in (gpx-file-tracks file)
		summing (distance track)))

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

(defgeneric elevation-gain (el)
  (:documentation "Compute the elevation gain of the element."))

(defmethod elevation-gain ((seg gpx-segment))
  (loop for i in (gpx-segment-points seg)
		for j in (cdr (gpx-segment-points seg))
		summing (ele-gain i j) into total
		finally (return total)))

(defmethod elevation-gain ((track gpx-track))
  (loop for seg in (gpx-track-segments track)
		summing (elevation-gain seg) into total
		finally (return total)))

(defmethod elevation-gain ((file gpx-file))
  (loop for track in (gpx-file-tracks file)
		summing (elevation-gain track)))

(defgeneric elevation-loss (el)
  (:documentation "Compute the elevation loss of the element."))

(defmethod elevation-loss ((seg gpx-segment))
  (loop for i in (gpx-segment-points seg)
		for j in (cdr (gpx-segment-points seg))
		summing (ele-loss i j) into total
		finally (return total)))

(defmethod elevation-loss ((track gpx-track))
  (loop for seg in (gpx-track-segments track)
		summing (elevation-loss seg) into total
		finally (return total)))

(defmethod elevation-loss ((file gpx-file))
  (loop for track in (gpx-file-tracks file)
		summing (elevation-loss track)))

(defun summarize (gpx &key (units 'imperial))
  (let ((eg (elevation-gain gpx))
        (el (elevation-loss gpx))
        (dist (distance gpx))
        (shortunit (if (eq units 'imperial) "feet" "meters"))
        (longunit (if (eq units 'imperial) "miles" "kilometers")))
  (format t "Total elevation gain: ~a ~a~%" (if (eq units 'imperial) (meters-to-feet eg) eg) shortunit)
  (format t "Total elevation loss: ~a ~a~%" (if (eq units 'imperial) (meters-to-feet el) el) shortunit)
  (format t "Total elevation loss: ~a ~a~%" (if (eq units 'imperial) (meters-to-miles dist) (/ dist 1000.0)) longunit)))
