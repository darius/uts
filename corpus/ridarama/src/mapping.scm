(define-record box (origin extent))

;; Output, using integer coordinates for a picture WIDTH pixels wide,
;; the streets clipped to BOX.
(define write-map
  (lambda (width box)
    (for-each print
	      (map (projector width box)
		   (filter identity
			   (map (lambda (segment) (clip segment box))
				(get-segments)))))))

(define projector
  (lambda (width box)
    (let* ((origin (box.origin box))
	   (shrinkage (/ width (- (point.longitude (box.extent box))
				  (point.longitude origin)))))
      (lambda (segment)
	(segment-map (lambda (p) (scale shrinkage (project p)))
		     (segment-map (lambda (p) (vector- p origin))
				  segment))))))

(define map-center (make-point 80 30))

(define shrink-factor
  (lambda (point)
    (cos (point.latitude point))))

(define shrink (shrink-factor map-center))

(define project
  (lambda (point)
    (make-point (* shrink (point.longitude point))
		(point.latitude point))))

;; Return the intersection of SEGMENT with BOX, or #f if that's not a
;; proper segment.  (The result if nonfalse is also a segment.)
(define clip
  (lambda (segment box)
    ...))
