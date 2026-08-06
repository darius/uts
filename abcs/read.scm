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

    (define (delimiter? char)
      ;; TODO I'm not actually sure what to consider a delimiter
      (or (memv char '(#\space #\tab #\newline #\return #\; #\( #\) #\" #\' #\` #\, #\#))
          (eof-object? char)))

    (define (read-hex-char-code in-port prev-chars)
      (let ((char (peek-char in-port)))
        (cond ((and (char? char) (or (char-alphabetic? char) (char-numeric? char)))
               (read-char in-port)
               (read-hex-char-code in-port (cons char prev-chars)))
              (else (list->string (reverse prev-chars))))))

    (define (hex-char-code->code str)
      (let ((code (string->number str 16)))  ;; N.B. always #f or exact integer
        (and code (<= 0 code) (< code 256) code)))
      
    ;; (preceded by '#', '\\')
    (define (read-char-literal in-port char)
      (cond
       ((eof-object? char)
        (read-error in-port "Unterminated #\\ literal"))
       ((eqv? char #\x) ;; expect either just x or a hex escape sequence
        (let ((str (read-hex-char-code in-port '())))
          (cond ((string=? str "")
                 (read-nonhex-char-literal in-port char))
                ((hex-char-code->code str)
                 => (lambda (code)
                      (if (delimiter? (peek-char in-port))
                          (integer->char code)
                          (read-error in-port "Char literal must be followed by a delimiter"))))
                (else (read-error in-port "Bad hex-escaped char literal" (string-append "#\\x" str))))))
       (else
        (read-nonhex-char-literal in-port char))))

    (define (read-nonhex-char-literal in-port char)
      (cond
       ((and (char-alphabetic? char)
             (char-alphabetic? (peek-char in-port)))
	(let ((symbol (%read-atom in-port char)))
	  (let ((table '(;; (backspace . #\backspace)
                         ;; (escape . #\escape)
			 (space . #\space)
			 (tab . #\tab)
			 (newline . #\newline)
			 (return . #\return)
			 )))
	    (cond ((assq symbol table) => cdr)
                  (else
		   (read-error in-port 
			       "Unknown character literal - #\\"
			       symbol))))))
       (else
        (if (delimiter? (peek-char in-port)) ;TODO
            char
            (read-error in-port "Char literal must be followed by a delimiter")))))

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
        (let* ((hex (read-hex-char-code in-port '()))
               (delim (read-char in-port)))
          (cond ((and (eqv? delim #\;) (hex-char-code->code hex)) => integer->char)
                (else (read-error in-port "Bad hex-escaped char literal"
                                  (string-append "#\\x" hex (if (char? delim) (string delim) "")))))))
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
