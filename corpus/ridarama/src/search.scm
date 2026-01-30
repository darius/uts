(define-record problem (successors cost heuristic state=?))

(define-record path (state previous cost-so-far total-cost))

(define infinity (expt 2 29))

(define new-path
  (lambda (state)
    (make-path state #f 0 infinity)))

; Find a path whose state satisfies goal?.  Start with paths, and
; expand successors, exploring least cost first.  When there are
; duplicate states, keep the one with the lower cost and discard the
; other.  Originally from Norvig, PAIP.
(define a*-search
  (lambda (states problem)
    (variant-case problem
      (problem (successors cost heuristic state=?)
	(define cost-fn cost)
	(define cost-left-fn heuristic)
	(define goal? (lambda (path) (= (path.cost-so-far path)
					(path.total-cost path))))

	(let searching ((paths (map new-path states)) (old-paths '()))
	  (print paths)
	  (cond
	   ((null? paths) #f)
	   ((goal? (car paths))
	    (car paths))
	   (else
	    (let* ((path (car paths))
		   (paths (cdr paths))
		   (state (path.state path)))

	      ; Return only those states that don't loop back on the path.
	      (define remove-waffling
		(lambda (states)
		  (filter (lambda (new-state)
			    (not (goes-through? state=? path new-state)))
			  states)))

	      ; Extend the path with STATE2 and add it to the search.
	      (define inspect-successor
		(lambda (state2)
		  (let* ((cost (+ (path.cost-so-far path)
				  (cost-fn state state2)))
			 (cost2 (cost-left-fn state2))
			 (path2 (make-path state2 path cost (+ cost cost2))))
		    ; Place the new path, path2, in the right list:
		    (cond ((find-path state2 paths state=?)
			   => (lambda (old)
				(if (better-path path2 old)
				    (set! paths 
					  (insert-path path2
						       (delq old paths))))))
			  ((find-path state2 old-paths state=?)
			   => (lambda (old)
				(if (better-path path2 old)
				    (begin
				      (set! paths (insert-path path2 paths))
				      (set! old-paths (delq old old-paths))))))
			  (else
			   (set! paths (insert-path path2 paths)))))))

	      ; Update PATHS and OLD-PATHS to reflect
	      ; the new successors of STATE:
	      (set! old-paths (insert-path path old-paths))
	      (print (successors state))
	      (for-each inspect-successor 
			(remove-waffling (successors state)))

	      ; Finally, call A* again with the updated path lists:
	      (searching paths old-paths)))))))))

; Find the path with this state among a list of paths.
(define find-path
  (lambda (state paths state=?)
    (find (lambda (path) (state=? state (path.state path)))
	  paths)))

; Is path1 cheaper than path2?
(define better-path 
  (lambda (path1 path2)
    (< (path.total-cost path1) (path.total-cost path2))))

; Put path into the right position, sorted by total cost.
(define insert-path
  (lambda (path paths)
    (let ((cost (path.total-cost path)))
      (let inserting ((paths paths))
	(cond ((null? paths) (list path))
	      ((< cost (path.total-cost (car paths)))
	       (cons path paths))
	      (else
	       (cons (car paths)
		     (inserting (cdr paths)))))))))

; Call fn on each state in the path, collecting results.
(define map-path
  (lambda (fn path)
    (if (not path)
	'()
	(cons (fn (path.state path))
	      (map-path fn (path.previous path))))))

; Return true iff STATE is on the PATH, according to STATE=?.
(define goes-through?
  (lambda (state=? path state)
    (let checking ((path path))
      (cond ((not path) #f)
	    ((state=? state (path.state path))
	     #t)
	    (else (checking (path.previous path)))))))

(define print-path
  (lambda (path stream)
    (format stream "#<Path to ~a cost ~,1f>"
	    (path.state path) (path.total-cost path))))
