; Diameter of planet earth in kilometers.
(define earth-diameter 12765.0)

; The great circle distance between two cities.
(define air-distance
  (lambda (city1 city2)
    (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
      ; D is the straight-line chord between the two cities,
      ; The length of the subtending arc is given by:
      (* earth-diameter (asin (/ d 2))))))

; Return the x,y,z coordinates of a point on a sphere.
; The center is (0 0 0) and the north pole is (0 0 1).
(define xyz-coords
  (lambda (city)
    (let ((psi (deg->radians (city.lat city)))
	  (phi (deg->radians (city.long city))))
      (list (* (cos psi) (cos phi))
	    (* (cos psi) (sin phi))
	    (sin psi)))))

; The Euclidean distance between two points.
; The points are coordinates in n-dimensional space.
(define distance 
  (lambda (point1 point2)
    (sqrt (foldl + 0 
		 (map (lambda (a b) (expt (- a b) 2))
		      point1 point2)))))

; Convert degrees to radians.
(define deg->radians
  (lambda (deg)
    (* deg pi (/ 1 180))))


; Return all cities within 1000 kilometers.
;(define neighbors 
;  (lambda (city)
;    (filter (lambda (c)
;	      (and (not (eq? c city))
;		   (< (air-distance c city) 1000.0)))
;	    (map cdr cities))))
