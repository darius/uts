;;;
;;; Tries
;;;
;;; A trie is a set of sequences.
;;;
;;; We represent it by a pair of:
;;;  - a flag telling whether the empty list is in the set, and
;;;  - an a-list mapping each different CAR of all sequences in the
;;; set to the trie for all the CDRs of the sequences with that CAR.
;;;
;;; Many of the functions take a parameter =? which compares sequence
;;; elements for equality.  An XS parameter will be a list of such
;;; elements.
;;;
;;; I haven't needed to implement deletion yet...
;;; 
;;; Darius Bacon <darius@accesscom.com>
;;; http://www.accesscom.com/~darius
;;; 


(define trie/make cons)
(define trie/null? car) 
(define trie/a-list cdr)
(define trie/set-null! set-car!) 
(define trie/set-a-list! set-cdr!)

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
    (trie/make #f '())))

;; Return true iff TRIE has no members.
(define trie-empty?
  (lambda (trie)
    (and (not (trie/null? trie))
	 (null? (trie/a-list trie)))))

;; Mutate TRIE to have the new member XS if it doesn't already.
(define trie-adjoin!
  (lambda (=?)
    (let ((ass= (trie/associator =?)))
      (lambda (trie xs)
	(let stepping ((trie trie) (xs xs))
	  (if (null? xs)
	      (trie/set-null! trie #t)
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
(define trie-member?
  (lambda (=?)
    (let ((step (trie-step =?)))
      (lambda (trie xs)
	(let stepping ((trie trie) (xs xs))
	  (cond ((not trie) #f)
		((null? xs) (trie/null? trie))
		(else 
		 (stepping (step trie (car xs))
			   (cdr xs)))))))))


;;;
;;; Testing
;;;
;;; Take an input file of one word per line, and write all words not in
;;; the system dictionary.  Compare against `spell'.
;;;

(define read-line
  (lambda (port)
    (let loop ((acc '()))
      (let ((c (read-char port)))
	(cond ((eof-object? c)
	       (if (null? acc)
		   c
		   (list->string (reverse acc))))
	      ((char=? c #\newline)
	       (list->string (reverse acc)))
	      (else
	       (loop (cons c acc))))))))

(define read-word
  (let ((constituent? 
	 (lambda (c)
	   (or (char-alphabetic? c)
	       (char=? c #\')))))
    (lambda (port)
      (let skipping ((c (read-char port)))
	(cond ((eof-object? c) c)
	      ((not (constituent? c))
	       (skipping (read-char port)))
	      (else
	       (let consing ((acc (list c)))
		 (let ((c (read-char port)))
		   (if (and (char? c) (constituent? c))
		       (consing (cons c acc))
		       (list->string (reverse acc)))))))))))

(define for-each-input-line
  (lambda (proc port)
    (let reading ()
      (let ((line (read-line port)))
	(cond ((not (eof-object? line))
	       (proc line)
	       (reading)))))))

(define for-each-input-word
  (lambda (proc port)
    (let reading ()
      (let ((line (read-word port)))
	(cond ((not (eof-object? line))
	       (proc line)
	       (reading)))))))

(define adjoin! (trie-adjoin! eqv?))
(define member? (trie-member? eqv?))

(define install-dictionary!
  (lambda (trie port)
    (for-each-input-line
     (let ((counter 0))
       (lambda (word)
	 (set! counter (+ counter 1))
	 (if (= 0 (modulo counter 1000))
	     (display "."))
	 (adjoin! trie (string->list word))))
     port)
    (newline)))

(define words (make-empty-trie))

(define write-misspellings
  (lambda (port)
    (for-each-input-word
     (lambda (word)
       (if (not (member? words 
			 (map char-downcase (string->list word))))
	   (begin (display word) (newline))))
     port)))

(define (testme)
  (call-with-input-file "wordlist.txt"  ; "/usr/dict/words"
    (lambda (port)
      (install-dictionary! words port)))

  (adjoin! words (string->list "i"))
  (adjoin! words (string->list "a"))

  (write-misspellings (current-input-port)))
