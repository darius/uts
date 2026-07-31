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

    ;; (preceded by '#', '\\')
    (define (read-char-literal in-port char)
      (cond
       ((eof-object? char)
        (read-error in-port "Unterminated #\\ literal"))
       ((and (char-alphabetic? char)
	     (or (char-alphabetic? (peek-char in-port))
                 (and (eqv? char #\x) (char-numeric? (peek-char in-port)))))
        ;; TODO execrable logic shoehorning in the hex escape case
	(let ((symbol (%read-atom in-port char)))
	  (let ((table '(;; (backspace . #\backspace)
                         ;; (escape . #\escape)
			 (space . #\space)
			 (tab . #\tab)
			 (newline . #\newline)
			 (return . #\return)
			 )))
	    (let ((pair (assq symbol table)))
	      (cond
               ((pair? pair) (cdr pair))
               ((and (eqv? (string-ref (symbol->string symbol) 0) #\x)
                     (string->number
                      (string-append (string #\#) (symbol->string symbol))))
                => (lambda (code)
                     ;; Hex character code like #\x1f
                     ;; (using string->number was kind of dodgy: allows numeric syntax frills)
                     ;; TODO deduplicate wrt string literal parsing
                     (if (and (exact? code) (<= 0 code) (< code 256))
                         (integer->char code)
                         (read-error in-port
				     "Unknown character literal - #\\"
				     symbol))))
               (else
		(read-error in-port 
			    "Unknown character literal - #\\"
			    symbol)))))))
       (else char)))

    ;; (preceded by '\\' inside a string literal)
    (define (read-string-escape in-port char)
      (cond
       ((eof-object? char)
	(read-error in-port "Unexpected EOF in string escape sequence"))
       ((assv char '((#\\ . #\\)
		     (#\" . #\")
		     (#\n . #\newline)
		     (#\t . #\tab)
		     (#\r . #\return)))
	=> cdr)
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
                       (integer->char code)
                       (read-error in-port "Invalid hex escape sequence in string"))))
                (else
                 (scanning (cons c pre) (read-char in-port))))))
       (else (read-error in-port 
			 "Unknown escape sequence in string"))))

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
		(( #\\ ) (read-char-literal in-port (read-char in-port)))
		(( #\( ) (list->vector (read-list in-port next)))
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
	      (loop (cons (read-string-escape in-port (read-char in-port))
                          prev-chars)))
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
