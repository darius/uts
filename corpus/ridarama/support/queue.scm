;;;
;;; Immutable queues
;;;
;;; Sequences with first-in-first-out addition and removal.
;;; 
;;; The pair (left . right) represents the sequence 
;;; (append left (reverse right)).  Elements are removed on
;;; the left and added on the right.
;;;
;;; Note that these queues are only efficient if we amortize over
;;; a single sequence of additions and removals.
;;;
;;; Other functions we might like:
;;;  on-queue? 
;;;  queue-length
;;;  queue-remove-if
;;;  queue-remove-each
;;;  queue->list
;;;


;; The queue with no elements.
(define empty-queue '(() . ()))

;; True iff QUEUE is empty.
(define queue-empty? 
  (lambda (queue)
    (and (null? (car queue)) 
	 (null? (cdr queue)))))

;; Return QUEUE with ELEMENT appended on the right.
(define enqueue 
  (lambda (queue element)
    (cons (car queue) 
	  (cons element (cdr queue)))))

;; Return QUEUE with ELEMENTS appended on the right.
(define enqueue-many 
  (lambda (queue elements)
    (cons (car queue) 
	  (append (reverse elements) (cdr queue)))))

;; Return a list (leftmost rest) where LEFTMOST is the leftmost
;; element of QUEUE's sequence, and REST is a queue of the remainder.
;; Signals an error if QUEUE is empty.
(define dequeue 
  (lambda (queue)
    (let ((left (car queue))
	  (right (cdr queue)))
      (if (null? left)
	  (let ((sequence (reverse right)))
	    (if (null? sequence)
		(error "DEQUEUE of empty queue" '())
		(list (car sequence) 
		      (cons (cdr sequence) '()))))
	  (list (car left)
		(cons (cdr left) right))))))
