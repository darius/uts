;;;
;;; Reading
;;;

; This is a high-level Scheme implementation of `read', using Scheme's
; low-level I/O procedures (mainly read-char and peek-char).  It relies on
; char->integer returning a number in [0..255].  

; The inner-loop code (skip-blanks and read-atom) has been moved into the 
; bytecode interpreter for speed.

; FIXME: error reporting could be a lot better...

(define read 
  (let ()

    (define the-readtable (make-vector 256 %read-atom))

    (define (install-read-macro char reader)
      (vector-set! the-readtable (char->integer char) reader))

    (define (read in-port)
      (let ((char (read-char in-port)))
	(if (eof-object? char)
	    char
	    ((vector-ref the-readtable (char->integer char)) 
	     in-port 
	     char))))

    (define dot-symbol (string->symbol "."))

    ; A list has the syntax:
    ;           list ::= '(' ( ')' | rest-of-list )
    ;   rest-of-list ::= expr+ ( ')' | '.' expr ')' )
    (define (read-list in-port char)
      (%skip-blanks in-port)
      (let ((char (peek-char in-port)))
	(cond 
	  ((eof-object? char) (read-error in-port "Unexpected EOF in list"))
	  ((char=? char #\) ) 
	   (read-char in-port)
	   '())
	  (else 
	   (let read-rest-of-list ()
	     (let ((head (read in-port)))
	       (cons head
		 (let loop ()
		   (%skip-blanks in-port)
		   (let ((char (peek-char in-port)))
		     (cond
		       ((eof-object? char) 
			(read-error in-port "Unexpected EOF in list"))
		       ((char=? char #\) )
			(read-char in-port)
			'())
		       ((char=? char #\.)
			(read-char in-port)
			(let ((next (%read-atom in-port char)))
			  (if (not (eq? next dot-symbol))
			      (cons next (loop))
			      (let ((result (read in-port)))
				(%skip-blanks in-port)
				(if (not (eqv? (read-char in-port) #\) ))
				    (read-error 
				     in-port 
				     "Unbalanced parentheses: no trailing ')'"))
				result))))
		       (else (read-rest-of-list))))))))))))

    (define (read-number in-port)
      (let ((n (%read-atom in-port)))
	(if (number? n)
	    n
	    (read-error in-port "Expected a number" n))))

    (define (read-error port message . irritants)
      (set! %error-cont #f)
      (%complain "Read error" message irritants)
      (%flush-input-line port)
      (%reset))
    

    ;; White space
    (let ((read-after (lambda (in-port char) 
	                (%skip-blanks in-port)
	                (read in-port))))
      (for-each (lambda (white) (install-read-macro white read-after))
	        '(#\space #\tab #\newline #\return)))

    (install-read-macro #\;
      (lambda (in-port char)
	(%flush-input-line in-port)
	(read in-port)))

    (install-read-macro #\( read-list)

    (install-read-macro #\)
      (lambda (in-port char)
	(read-error in-port "Unbalanced parentheses")))

    (install-read-macro #\@
      (lambda (in-port char)
	(read-error in-port "Bare '@' character")))

    (install-read-macro #\#
      (lambda (in-port char)
	(if (memv (peek-char in-port) 
		  '(#\x #\d #\o #\b #\e #\i #\X #\D #\O #\B #\E #\I))
	    (let ((n (%read-atom in-port char)))
	      (if (number? n)
		  n
		  (read-error in-port "Not a number" n)))
	    (let ((next (read-char in-port)))
	      (case next
		((#\f #\F) #f)
		((#\t #\T) #t)
		((#\\)
		 (let ((next (read-char in-port)))
		   (if (and (char-alphabetic? next)
			    (char-alphabetic? (peek-char in-port)))
		       (let ((symbol (%read-atom in-port next)))
			 (let ((table '(; (backspace . #\backspace)
					; (escape . #\escape)
					; (page . #\page)
					(newline . #\newline)
					(return . #\return)
					(space . #\space)
					(tab . #\tab)
					)))
			   (let ((pair (assq symbol table)))
			     (if (pair? pair)
				 (cdr pair)
				 (read-error in-port 
					     "Unknown character constant - #\\"
					     symbol)))))
		       next)))
		(( #\( )	; vector constant
		 (list->vector (read-list in-port next)))
		(else (read-error in-port "Unknown '#' read macro")))))))

    (install-read-macro #\"
      (lambda (in-port char)
	(let loop ((prev-chars '()))
	  (let ((char (read-char in-port)))
	    (cond
	     ((eof-object? char)
	      (read-error in-port "Unexpected EOF in string constant"))
	     ((char=? char #\")
	      (list->string (reverse prev-chars)))
	     ((char=? char #\\)
	      (let ((char (read-char in-port)))
		(cond
		 ((eof-object? char)
		  (read-error in-port "Unexpected EOF in string escape sequence"))
		 ((assv char '((#\\ . #\\)
			       (#\" . #\")
			       (#\n . #\newline)
			       (#\t . #\tab)
			       (#\r . #\return)))
		  => (lambda (pair)
		       (loop (cons (cdr pair) prev-chars))))
                 ((eqv? char #\x) ;; Hex escape sequence
                  (let scanning ((pre '())
                                 (c (read-char in-port)))
                    (cond ((eof-object? c)
                           (read-error in-port "Unexpected EOF in string escape sequence"))
                          ((eq? c #\;) ;; End of hex sequence
                           (let* ((code-str (list->string (reverse pre)))
                                  (code (string->number code-str 16)))
                             ;; (using string->number was kind of dodgy: allows numeric syntax frills)
                             (if (and code (exact? code) (<= 0 code) (< code 256))
                                 (loop (cons (integer->char code) prev-chars))
                                 (read-error in-port "Invalid hex escape sequence in string"))))
                          (else
                           (scanning (cons c pre) (read-char in-port))))))
		 (else (read-error in-port 
				   "Unknown escape sequence in string")))))
	     (else (loop (cons char prev-chars))))))))

    (install-read-macro #\'
      (lambda (in-port char)
	(list 'quote (read in-port))))
    
    (install-read-macro #\`
      (lambda (in-port char)
	(list 'quasiquote (read in-port))))

    (install-read-macro #\,
      (lambda (in-port char)
	(list (if (eqv? (peek-char in-port) #\@)
	          (begin (read-char in-port) 'unquote-splicing)
	          'unquote)
	      (read in-port))))

    (lambda opt:in-port
      (read (%optional-arg opt:in-port (current-input-port))))))
