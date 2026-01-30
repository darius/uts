; read in schedule

(define read-schedule
  (lambda (filename)
    (call-with-input-file filename read-schedule-port)))

(define read-schedule-port
  (lambda (port)
    (let reading ()
      (let ((tag (read port)))
	(cond ((not (eof-object? tag))
	       (case tag
		 ((ride)
		  (let* ((ride-id (read port))
			 (route-id (read port)))
		    (add-ride! ride-id route-id)))
		 ((arc)
		  (let* ((ride-id (read port))
			 (name1 (read port))
			 (time1 (read port))
			 (arrow (read port))
			 (name2 (read port))
			 (time2 (read port)))
		    (if (not (eq? arrow '->))
			(panic "Bad schedule input"))
		    (add-arc! ride-id name1 time1 name2 time2)
		    (connect! name1 name2)))
		 (else
		  (panic "Bad tag" tag)))
	       (reading)))))))


(define-record ride (ride-id route-id))

(define rides '())

(define add-ride!
  (lambda (ride-id route-id)
    (set! rides 
	  (acons ride-id (make-ride ride-id route-id) rides))))

(define get-ride
  (lambda (ride-id)
    (cdr (assv ride-id rides))))


(define-record arc (ride endpoints))
(define-record node (station time))

(define arcs (vector '() '()))

(define get-arcs-on
  (lambda (forwards? name)
    (cdr (assq name (vector-ref arcs (if forwards? 0 1))))))

(define add-arc!
  (lambda (ride-id name1 time1 name2 time2)
    (let ((arc (make-arc (get-ride ride-id) 
			 (vector (make-node name1 time1)
				 (make-node name2 time2)))))
      (define add!
	(lambda (name direction)
	  (cond ((assq name (vector-ref arcs direction))
		 => (lambda (entry)
		      (set-cdr! entry (cons arc (cdr entry)))))
		(else 
		 (vector-set! arcs direction
			      (acons name (list arc) 
				     (vector-ref arcs direction)))))))

      (add! name1 0)
      (add! name2 1))))

(define graph '())

(define connect!
  (lambda (name1 name2)
    (let ((edge (cons name1 name2)))
      (if (not (member edge graph))
	  (set! graph (cons edge graph))))))

(define graph-neighbors
  (lambda (path)
    (map (lambda (pair) (cons (cdr pair) path))
	 (filter (lambda (pair) 
		   (and (eq? (car pair) (car path))
			(not (memq (cdr pair) path))))
		 graph))))

(define stops-between
  (lambda (source dest)
    (length 
     (cdr 
      (breadth-first-search (list source)
			    (lambda (x) (eq? (car x) dest))
			    graph-neighbors)))))

(define breadth-first-search
  (lambda (start goal? successors)
    (define x 0)
    (let searching ((q (enqueue empty-queue (list start))))
      (set! x (+ x 1))
      (if (queue-empty? q)
	  #f
	  (let ((L (dequeue q)))
	    (let ((path (car L))
		  (q (cadr L)))
	      (if (goal? (car path))
		  path
		  (searching 
		   (enqueue-many q (map (pathify path) 
					(successors (car path))))))))))))

(define pathify
  (lambda (path)
    (lambda (state)
      (cons state path))))
