;;;
;;; Tries
;;;
;;; A trie is a table whose keys are sequences.
;;;
;;; We represent it by a pair of:
;;;  - the value for the empty list, and
;;;  - an a-list mapping each different CAR of all sequences in the
;;; set to the trie for all the CDRs of the sequences with that CAR.
;;;
;;; (For subsequences that aren't keys in the table, we use a special
;;; `unbound' marker in the value slot.)
;;;
;;; Many of the functions take a parameter =? which compares sequence
;;; elements for equality.  An XS parameter will be a list of such
;;; elements.
;;;
;;; I haven't needed deletion yet...
;;; 
;;; Darius Bacon <djello@well.com>
;;; http://www.well.com/~djello
;;; 


(define trie/make cons)
(define trie/value car) 
(define trie/a-list cdr)
(define trie/set-value! set-car!) 
(define trie/set-a-list! set-cdr!)

(define trie/null-marker (list 'null))

(define trie/unbound?
  (lambda (trie)
    (eq? trie/null-marker (trie/value trie))))

;; Return an equivalent of ASSOC that compares using =?.
(define trie/associator
  (lambda (=?)
    (cond ((eq? =? eq?) assq)
	  ((eq? =? eqv?) assv)
	  ((eq? =? equal?) assoc)
	  (else
	   (lambda (x a-list)
	     (let searching ((a-list a-list))
	       (cond ((null? a-list) #f)
		     ((=? x (caar a-list)) (car a-list))
		     (else (searching (cdr a-list))))))))))


;; Return a new trie with no members.
(define make-empty-trie
  (lambda () 
    (trie/make trie/null-marker '())))

;; Return true iff TRIE has no members.
(define trie-empty?
  (lambda (trie)
    (and (trie/unbound? trie)
	 (null? (trie/a-list trie)))))

;; Mutate TRIE to have the new member XS if it doesn't already.
(define trie-bind!
  (lambda (=?)
    (let ((ass= (trie/associator =?)))
      (lambda (trie xs new-value)
	(let stepping ((trie trie) (xs xs))
	  (if (null? xs)
	      (trie/set-value! trie new-value)
	      (cond ((ass= (car xs) (trie/a-list trie))
		     => (lambda (pair)
			  (stepping (cdr pair) (cdr xs))))
		    (else
		     (trie/set-a-list! 
		      trie 
		      (cons (cons (car xs) 
				  (let ((rest-trie (make-empty-trie)))
				    (stepping rest-trie (cdr xs))
				    rest-trie))
			    (trie/a-list trie)))))))))))

;; Mutate TRIE to have the new member XS if it doesn't already.
(define trie-adjoin!
  (lambda (=?)
    (let ((bind! (trie-bind! =?)))
      (lambda (trie xs)
	(bind! trie xs #t)))))

;; Return the trie containing all the CDRs of the sequences in TRIE
;; whose CAR equals X; or #f if none.  (Mutations to the returned trie
;; also affect TRIE.)
(define trie-step
  (lambda (=?)
    (let ((ass= (trie/associator =?)))
      (lambda (trie x)
	(cond ((ass= x (trie/a-list trie))
	       => cdr)
	      (else #f))))))

;; Return the set of all CARs of members of TRIE, as an unordered
;; list without duplicates.
(define trie-prefixes
  (lambda (trie)
    (map car (trie/a-list trie))))

;; Return true iff TRIE has the member XS.
(define trie-lookup
  (lambda (=?)
    (let ((step (trie-step =?)))
      (lambda (trie xs default-value)
	(let stepping ((trie trie) (xs xs))
	  (cond ((not trie) default-value)
		((null? xs) 
		 (if (trie/unbound? trie)
		     default-value
		     (trie/value trie)))
		(else 
		 (stepping (step trie (car xs))
			   (cdr xs)))))))))

;; Return all members of TRIE that differ from XS by exactly DISTANCE 
;; steps of insertion, deletion, or substitution.
(define trie-misspellings
  (lambda (=?)
    (let ((step (trie-step =?))
	  (lookup (trie-lookup =?)))
      (lambda (trie xs distance)
	(let stepping ((trie trie) (xs xs) (d distance))

	  (define try-deletion
	    (lambda ()
	      (stepping trie (cdr xs) (- d 1))))

	  (define try-insertion
	    (lambda (prefix)
	      (take-step prefix xs (- d 1))))

	  (define try-substitution
	    (lambda (prefix)
	      (take-step prefix 
			 (cdr xs)
			 (if (=? prefix (car xs))
			     d
			     (- d 1)))))

	  (define take-step
	    (lambda (prefix xs d)
	      (map (lambda (suffix) (cons prefix suffix))
		   (stepping (step trie prefix) xs d))))

	  (cond ((not trie) '())
		((= d 0)
		 (if (lookup trie xs #f) (list xs) '()))
		((null? xs)
		 (flatmap try-insertion (trie-prefixes trie)))
		(else 
		 (let ((prefixes (trie-prefixes trie)))
		   (append (try-deletion)
			   (flatmap try-insertion prefixes)
			   (flatmap try-substitution prefixes))))))))))
