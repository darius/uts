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
(define lookup (trie-lookup eqv?))
(define misspellings (trie-misspellings eqv?))

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

(call-with-input-file "/usr/dict/words"
  (lambda (port)
    (install-dictionary! words port)))

(adjoin! words (string->list "i"))
(adjoin! words (string->list "a"))

(define write-misspellings
  (lambda (port)
    (for-each-input-word write-word-misspellings port)))

(define write-word-misspellings
  (lambda (word)
    (let ((chars (map char-downcase (string->list word))))
      (cond ((not (lookup words chars #f))
	     (display word)
	     (for-each (lambda (mis) 
			 (display #\space)
			 (for-each display mis))
		       (misspellings words chars 1))
	     (newline))))))
