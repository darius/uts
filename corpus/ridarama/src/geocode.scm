; Computing geographical locations of addresses.

; Return the point ADDRESS is at, or #f if it can't be found.
(define geocode 
  (lambda (address)
    (variant-case address
      (address (number street-name)
	(find-segment number (find-street street-name)
		      (lambda (segment side)
			(let ((range 
			       (vector-ref (segment.ranges segment) side)))
			  (segment-interpolate segment number range))))))))

(define-record address (number street-name))

(define-record street (name segments))

(define-record segment
  (tail					;Origin point
   head					;Destination point
   ranges))				;Vector of address ranges on the two
					;sides of the street - 0=left, 1=right

(define-record range (from to))

(define the-streets '())

(define load-streets!
  (lambda (streets)
    (set! the-streets streets)))

; Return the street named STREET-NAME.
(define find-street
  (lambda (street-name)
    (find (lambda (street) (string=? (street.name street) street-name))
	  the-streets)))

; Return (RECEIVER segment side) where (NUMBER, STREET) is located on
; the given side of the given segment; or return #f if (NUMBER, STREET)
; can't be located.
(define find-segment
  (lambda (number street receiver)
    (cond ((any (segment-with-number number) 
		(street.segments street))
	   => (lambda (pair) (apply receiver pair)))
	  (else #f))))

(define segment-with-number
  (lambda (number)

    (define good-range?
      (lambda (range)
	(variant-case range
	  (range (from to)
	    (<= from number to)))))	;FIXME: respect odd/even

    (lambda (segment)
      (let ((side (vector-find-index good-range? (segment.ranges segment))))
	(and side
	     (list segment side))))))

(define segment-interpolate
  (lambda (segment number range)
    (variant-case segment
      (segment (tail head)
        (variant-case range
	  (range (from to)
	    (point-interpolate tail head
			       (/ (- number from)
				  (- to from)))))))))

; This representation is all right as long as distances are short
; so we can approximate the map as flat.  Otherwise it gets ugly
; and expensive.
(define-record point 
  (lon					;Longitude in radians
   lat))				;Latitude in radians

; These functions all assume the flat-map approximation:

(define point-interpolate
  (lambda (tail head fraction)
    (vector+ tail
	     (scale fraction (vector- head tail)))))

(define vector+
  (lambda (p1 p2)
    (make-point (+ (point.lon p1) (point.lon p2))
		(+ (point.lat p1) (point.lat p2)))))

(define vector-
  (lambda (p1 p2)
    (make-point (- (point.lon p1) (point.lon p2))
		(- (point.lat p1) (point.lat p2)))))

(define scale
  (lambda (scalar point)
    (make-point (* scalar (point.lon point))
		(* scalar (point.lat point)))))


(define lon/lat
  (lambda (lon lat)
    (make-point (deg->radians lon)
		(deg->radians lat))))

(define test-geocode
  (lambda ()
    (let* ((s1 (make-segment (lon/lat 80 30)
			     (lon/lat 81 29)
			     (vector (make-range 101 199)
				     (make-range 100 198))))
	   (college (make-street "college ave" (list s1))))
      (load-streets! (list college))
      (print (geocode (make-address 150 "college ave")))
      (print (geocode (make-address 250 "college ave"))))))

