;;;;
;;;; String algorithms
;;;; WARNING: this code is only lightly tested.
;;;;
;;;; This code is in the public domain.
;;;; 
;;;; Darius Bacon <darius@accesscom.com>
;;;; http://www.accesscom.com/~darius
;;;;

;;; Preliminaries
;;; These are implementation-dependent.

(define alphabet-size 256)

; (char->index CHAR) returns an integer in (0..ALPHABET-SIZE].
(define char->index char->integer)


;;;
;;; String matching
;;; Return index of first substring in DAT equal to PAT, or #f.
;;; Note that (string-match "" "") = 0.
;;;

;; Brute force algorithm
(define string-match/brute
  (lambda (pat dat)
    (let ((P (string-length pat))
	  (D (string-length dat)))
      (let ((j-limit (+ (- D P) 1)))
	(let outer ((j 0))
	  (if (<= j-limit j)
	      #f
	      (let inner ((i 0))
		(cond ((= i P)
		       j)
		      ((char=? (string-ref pat i)
			       (string-ref dat (+ i j)))
		       (inner (+ i 1)))
		      (else
		       (outer (+ j 1)))))))))))

;; Boyer-Moore-Horspool
(define string-matcher
  (lambda (pat)
    (let ((m (- (string-length pat) 1)))
      (if (< m 0)
	  (lambda (dat) 
	    0)
	  (let ((skip (make-vector alphabet-size (string-length pat))))
	    ; Initialize the skip table
	    (do ((j 0 (+ j 1)))
		((= m j))
	      (vector-set! skip (char->index (string-ref pat j))
			   (- m j)))

	    ; Search
	    (lambda (dat)
	      (let ((n (- (string-length dat) 1)))
		(letrec ((outer 
			  (lambda (i)
			    (if (<= i n)
				(inner i m i)
				#f)))
			 (inner 
			  (lambda (k j i)
			    (if (char=? (string-ref dat k)
					(string-ref pat j))
				(if (= j 0)
				    k
				    (inner (- k 1) (- j 1) i))
				(outer 
				 (+ i (vector-ref skip 
						  (char->index 
						   (string-ref dat i)))))))))
		  (outer m)))))))))
