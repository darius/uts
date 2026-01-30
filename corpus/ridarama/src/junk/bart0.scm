(define-record leaving-at (time))
(define-record arriving-at (time))

(define-record event (station time arc))

; Return a request for a trip from START to DEST and satisfying
; TIME-CONSTRAINT.
(define make-trip-request
  (lambda (start dest time-constraint)
    (variant-case time-constraint
      (leaving-at (time)
	(make-fooey time start dest #t))
      (arriving-at (time)
	(make-fooey time dest start #f)))))

(define make-fooey
  (lambda (time search-origin search-goal forwards?)

    (define tail (if forwards? 1 0))

    ; Return a list of the arcs leaving STATION, including at least
    ; all those relevant to TIME.  (i.e. at or after, if FORWARDS?,
    ; or at or before, if not FORWARDS?)
    (define get-arcs
      (lambda (station time)
	(get-arcs-on forwards? station)))

    ; Return the number of seconds between TIME1 and TIME2.  This
    ; number is always positive because a smaller TIME2 is interpreted
    ; as being in the following day.  FIXME: fix comment
    ; Pre: both times are in 0 <= t < 86400
    (define time-interval
      (let ((delta (if forwards?
		       (lambda (t1 t2) (- t2 t1))
		       (lambda (t1 t2) (- t1 t2)))))
	(lambda (time1 time2)
	  (let ((difference (delta time1 time2)))
	    (if (<= 0 difference)
		difference
		(+ 86400 difference))))))	;handles wraparound at midnight

    (define event-interval
      (lambda (event1 event2)
	(time-interval (event.time event1) (event.time event2))))

    ; Return true iff TIME1 is hastier than TIME2, relative to BASE-TIME.
    (define hastier?
      (lambda (base-time time1 time2)
	(< (time-interval base-time time1)
	   (time-interval base-time time2))))

    (define neighbors
      (lambda (event)
	(variant-case event
	  (event (station time)
	    (map (lambda (arc)
		   (make-event (arc.destination arc) (arc.end arc) arc))
		 (hastiest-arcs time (get-arcs station time)))))))

    ; Return a list of the hastiest element of ARCS for each destination,
    ; relative to time START.
    (define hastiest-arcs
      (lambda (start arcs)
	(foldl (add-if-hasty start) '() arcs)))

    ; Return a list of the hastiest elements of (cons ARC HASTIEST) for
    ; each destination.  (Hastiest relative to time START.)
    ; Pre: HASTIEST has at most one element for each destination.
    (define add-if-hasty
      (lambda (start)
	(lambda (arc hastiest)
	  (let ((d (arc.destination arc))
		(e (arc.end arc)))
	    (let checking ((arcs hastiest))
	      (if (null? arcs)
		  (list arc)
		  (variant-case (car arcs)
		    (arc (destination end)
		      (if (eq? d destination)
			  (if (hastier? start e end)
			      (cons arc (cdr arcs))
			      arcs)
			  (cons (car arcs)
				(checking (cdr arcs))))))))))))

    ; FIXME: rename
    (define arc.destination
      (lambda (arc)
	(node.station (vector-ref (arc.endpoints arc) tail))))

    (define arc.end
      (lambda (arc)
	(node.time (vector-ref (arc.endpoints arc) tail))))

    (let ((problem
	   (make-problem neighbors
			 event-interval
			 (lambda (state) 
			   (min-possible-interval (event.station state)
						  search-goal))
			 same-station?))
	  (state (make-event search-origin time #f))
	  (path-receiver identity))
      (make-trip-computation state problem path-receiver))))

(define-record trip-computation (search-origin problem path-receiver))

; Search for the best solution to TRIP-REQUEST.
(define trip
  (lambda (trip-request)
    (variant-case trip-request
      (trip-computation (search-origin problem path-receiver)
	(cond ((a*-search (list search-origin) problem)
	       => path-receiver)
	      (else #f))))))

(define same-station?
  (lambda (event1 event2)
    (eq? (event.station event1)
	 (event.station event2))))

(define min-possible-interval
  (lambda (start dest)
    (* 60 (stops-between start dest))))	;a guess -- 1 minute per stop

