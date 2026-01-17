(define make-random
  (lambda seed
    (let ((state (if (null? seed) 74755 (car seed))))
      (lambda ()
	(set! state 
	      (remainder (+ (* seed 1309) 13849)
			 262144))
	state))))

(define random (make-random))

(define random-in-range
  (lambda (limit)
    (modulo (random) limit)))
