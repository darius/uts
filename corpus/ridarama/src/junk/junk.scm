;(define make-trip-request
;  (lambda (start dest time-constraint)
;    (variant-case time-constraint
;      (leaving-at (time)
;	(make-trip-computation 
;	 (make-event start time #f)
;	 (make-problem neighbors 
;		       event-interval
;		       (lambda (state) 
;			 (min-possible-interval (event.station state) dest))
;		       same-station?)
;	 identity))
;      (arriving-at (time)
;	(make-trip-computation 
;	 (make-event dest time #f)
;	 (make-problem backwards-neighbors 
;		       backwards-event-interval
;		       (lambda (state) 
;			 (min-possible-interval start (event.station state)))
;		       same-station?)
;	 reverse-path)))))



; FIXME: the resulting point is on the chord between TAIL and HEAD,
; not the arc along the Earth's surface...
; Maybe we should go back to lat/lon representation -- it's all right
; as long as distances are short.
(define point-interpolate
  (lambda (tail head fraction)
    (vector+ tail
	     (scale fraction (vector- head tail)))))

(define vector+
  (lambda (p1 p2)
    (make-point (+ (point.x p1) (point.x p2))
		(+ (point.y p1) (point.y p2))
		(+ (point.z p1) (point.z p2)))))

(define vector-
  (lambda (p1 p2)
    (make-point (- (point.x p1) (point.x p2))
		(- (point.y p1) (point.y p2))
		(- (point.z p1) (point.z p2)))))

(define scale
  (lambda (scalar point)
    (make-point (* scalar (point.x point))
		(* scalar (point.y point))
		(* scalar (point.z point)))))

