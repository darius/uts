;;;;
;;;; Purely functional, mergeable priority queues.
;;;;
;;;; A priority queue is a bag of ordered elements, with addition of
;;;; elements, removal of the minimum element, and merging of two
;;;; queues into one.  The order of elements is given by a predicate
;;;; that's a curried argument to each operation; using different 
;;;; predicates on the same queue will produce unpredictable results.
;;;;
;;;; PQ-MIN, PQ-INSERT, and PQ-MERGE are constant-time, while
;;;; PQ-REMOVE-MIN is conjectured to take logarithmic amortized time,
;;;; assuming a single thread of use; see Chris Okasaki's paper
;;;; ``Functional Data Structures'' for a way to handle general access
;;;; patterns efficiently.  This code is adapted from that paper.
;;;; 
;;;; We represent a queue by a pairing heap: either the empty heap, or
;;;; a pair of the minimum element and a list of sub-heaps that merge
;;;; to form the overall heap.  The sub-heaps must not be empty.
;;;;
;;;; This code is in the public domain.
;;;; 
;;;; Darius Bacon <darius@accesscom.com>
;;;; http://www.accesscom.com/~darius
;;;; 


;; This is the only non-R4RS procedure used:
;(define error
;  (lambda (complaint object) ...))


;; For a significant speedup, get your compiler to inline these:
(define make-pq cons)
(define pq/elem car)
(define pq/rest cdr)


;; The priority queue with no elements.
(define empty-pq '())

;; True iff PQ is empty.
(define pq-empty? null?)

;; Return the minimum element of PQ.
;; Signals an error if PQ is empty.
(define pq-min
  (lambda (<=?)
    (lambda (pq)
      (if (pq-empty? pq)
	  (error "PQ-MIN of an empty pq" '())
	  (pq/elem pq)))))

;; Return a priority queue with a single element, ELEM.
(define unit-pq
  (lambda (<=?)
    (lambda (elem)
      (make-pq elem '()))))

;; Return a priority queue combining the elements of PQ1 and PQ2.
(define pq-merge
  (lambda (<=?)
    (lambda (pq1 pq2)
      (cond ((pq-empty? pq1) pq2)
	    ((pq-empty? pq2) pq1)
	    (else (let ((min1 (pq/elem pq1))
			(min2 (pq/elem pq2)))
		    (if (<=? min1 min2)
			(make-pq min1 (cons pq2 (pq/rest pq1)))
			(make-pq min2 (cons pq1 (pq/rest pq2))))))))))

;; Return PQ with ELEM inserted.
(define pq-insert
  (lambda (<=?)
    (lambda (pq elem)
;     ((pq-merge <=?) (unit-pq elem) pq)))
      (cond ((pq-empty? pq) (make-pq elem '()))
	    (else (let ((min1 (pq/elem pq))
			(min2 elem))
		    (if (<=? min1 min2)
			(make-pq min1 (cons (make-pq elem '()) (pq/rest pq)))
			(make-pq min2 (cons pq '())))))))))

;; Return PQ with its minimum element removed.
;; Signals an error if PQ is empty.
(define pq-remove-min
  (lambda (<=?)
    (let ((merge (pq-merge <=?)))
      (lambda (pq)
	(if (pq-empty? pq)
	    (error "PQ-REMOVE-MIN of empty pq" '())
	    (let merging ((pqs (pq/rest pq)))
	      (cond ((null? pqs) empty-pq)
		    ((null? (cdr pqs)) (car pqs))
		    (else (merge 
			   ;((pq-merge <=?) (car pqs) (cadr pqs))
			   (let* ((pq1 (car pqs))
				  (pq2 (cadr pqs))
				  (min1 (pq/elem pq1))
				  (min2 (pq/elem pq2)))
			     (if (<=? min1 min2)
				 (make-pq min1 (cons pq2 (pq/rest pq1)))
				 (make-pq min2 (cons pq1 (pq/rest pq2)))))
			   (merging (cddr pqs)))))))))))


;;;
;;; Testing
;;;

(define int-min        (pq-min <=))
(define int-insert     (pq-insert <=))
(define int-remove-min (pq-remove-min <=))

(define grow-and-shrink
  (lambda (numbers empty empty? get-min insert remove-min)

    (let* ((history '())
	   (insert (lambda (pq elem)
		     (let ((pq (insert pq elem)))
		       (set! history (cons (get-min pq) history))
		       pq)))
	   (remove-min (lambda (pq)
			 (let ((pq (remove-min pq)))
			   (set! history (cons (if (empty? pq)
						   'empty
						   (get-min pq))
					       history))
			   pq))))

      (define growing 
	(lambda (pq ns)
	  (if (null? ns)
	      pq
	      (let ((pq (insert pq (car ns)))
		    (ns (cdr ns)))
		(if (null? ns)
		    pq
		    (growing (remove-min (insert pq (car ns))) 
			     (cdr ns)))))))
      
      (define shrinking
	(lambda (pq ns)
	  (if (empty? pq)
	      (reverse history)
	      (shrinking (remove-min
			  (remove-min
			   (insert pq (car ns))))
			 (cdr ns)))))

      (shrinking (growing empty-pq numbers) numbers))))

(define test-with
  (lambda (numbers)

    (define insert-sorted
      (lambda (ns n)
	(if (or (null? ns) (<= n (car ns)))
	    (cons n ns)
	    (cons (car ns)
		  (insert-sorted (cdr ns) n)))))

    (let ((history-1 
	   (grow-and-shrink 
	    numbers 
	    empty-pq pq-empty? int-min int-insert int-remove-min))
	  (history-2 
	   (grow-and-shrink numbers 
			    '() null? car insert-sorted cdr)))
      (if (equal? history-1 history-2)
	  history-1
	  (error "Test failed" (list history-1 history-2))))))

(define test-each-tail
  (lambda (numbers)
    (do ((ns numbers (cdr ns)))
	((null? ns))
      (test-with ns)
      (test-with (reverse ns)))))

(define test
  (lambda ()
    (test-each-tail
     '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 
	 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0
	 2 7 1 8 2 8 1 8 2 8 4 5 9 0 4 5))
    (test-each-tail '(0 1 2 3 4 5))
    (test-each-tail '(0 0 0 0 0))))
