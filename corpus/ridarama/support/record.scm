;;;;
;;;; Record structures
;;;;

(define *debugging* #t)

;; Return a predicate that determines whether an object is a record 
;; of type TAG.
(define record-predicate
  (lambda (tag)
    (lambda (obj)
      (and (vector? obj)
	   (< 0 (vector-length obj))
	   (eq? (vector-ref obj 0) tag)))))

;; Return the type-tag of a record.
(define record-tag
  (if *debugging*
      (lambda (record)
	(if (and (vector? record)
		 (< 0 (vector-length record)))
	    (vector-ref record 0)
	    (panic "Took the tag of a non-record" record)))
      (lambda (record)
	(vector-ref record 0))))

;; Return a constructor for records with type TAG and ARITY slots.
(define record-maker
  (lambda (tag arity)
    (case arity
      ((1) (lambda (a) (vector tag a)))
      ((2) (lambda (a b) (vector tag a b)))
      ((3) (lambda (a b c) (vector tag a b c)))
      ((4) (lambda (a b c d) (vector tag a b c d)))
      (else 
       (lambda args
	 (if (= (length args) arity)
	     (list->vector (cons tag args))
	     (panic "Wrong number of arguments to constructor" tag args)))))))

;; Return a accessor function for the OFFSETth slot of a TAG-type record.
;; Slot numbering starts from 1.
(define record-accessor
  (lambda (tag offset)
    (if *debugging*
	(let ((ok? (record-predicate tag)))
	  (lambda (record)
	    (if (not (ok? record))
		(panic "Bad record access" tag record))
	    (vector-ref record offset)))
	(lambda (record)
	  (vector-ref record offset)))))

;; Return a mutator function for the OFFSETth slot of a TAG-type record.
;; Slot numbering starts from 1.
(define record-mutator
  (lambda (tag offset)
    (if *debugging*
        (let ((ok? (record-predicate tag)))
          (lambda (record obj)
            (if (not (ok? record))
                (panic "Bad record mutation" tag record))
	    (vector-set! record offset obj)))
        (lambda (record obj)
          (vector-set! record offset obj)))))

