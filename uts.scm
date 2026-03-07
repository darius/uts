;;;; 
;;;; The runtime library, with compiler and debugger.
;;;;

(begin
  (define complex? number?)
  (define real? number?)
  (define rational? real?))

;;;
;;; car/cdr compositions
;;;

(begin
  (define caar (lambda (x) (car (car x))))
  (define cdar (lambda (x) (cdr (car x))))
  (define cadr (lambda (x) (car (cdr x))))
  (define cddr (lambda (x) (cdr (cdr x))))
  (define caaar (lambda (x) (car (car (car x)))))
  (define cdaar (lambda (x) (cdr (car (car x)))))
  (define cadar (lambda (x) (car (cdr (car x)))))
  (define cddar (lambda (x) (cdr (cdr (car x)))))
  (define caadr (lambda (x) (car (car (cdr x)))))
  (define cdadr (lambda (x) (cdr (car (cdr x)))))
  (define caddr (lambda (x) (car (cdr (cdr x)))))
  (define cdddr (lambda (x) (cdr (cdr (cdr x)))))
  (define caaaar (lambda (x) (car (car (car (car x))))))
  (define cdaaar (lambda (x) (cdr (car (car (car x))))))
  (define cadaar (lambda (x) (car (cdr (car (car x))))))
  (define cddaar (lambda (x) (cdr (cdr (car (car x))))))
  (define caadar (lambda (x) (car (car (cdr (car x))))))
  (define cdadar (lambda (x) (cdr (car (cdr (car x))))))
  (define caddar (lambda (x) (car (cdr (cdr (car x))))))
  (define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
  (define caaadr (lambda (x) (car (car (car (cdr x))))))
  (define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
  (define cadadr (lambda (x) (car (cdr (car (cdr x))))))
  (define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
  (define caaddr (lambda (x) (car (car (cdr (cdr x))))))
  (define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
  (define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
  (define cddddr (lambda (x) (cdr (cdr (cdr (cdr x)))))))


;;;
;;; Variable-arity versions of byteops
;;;

;; All the following code assumes the open-coding of calls to primitives with
;; the 'right' number of args.  Thus such a call in the definition of the
;; primitive is not recursive -- these definitions are not valid Scheme,
;; just a convenient but implementation-dependent way of defining the
;; variable-arity primitives in terms of fixed-arity bytecodes.

;; This also means that eta-reducing expressions like (lambda (x y) (* x y))
;; would be a mistake.  (There are other apparently senseless eta-expansions
;; later on in the library, for a similar reason -- protecting against 
;; reassignment to the top-level binding.  We should probably collect 
;; these all together in a LET wrapper.)

(begin

  (define (+ . args) 
    (cond ((null? args) 0)
	  ((null? (cdr args)) (car args))
	  ((null? (cdr (cdr args))) (+ (car args) (car (cdr args))))
	  (else (+ (+ (car args) (car (cdr args)))
		   (apply + (cdr (cdr args)))))))

  (define (* . args) 
    (cond ((null? args) 1)
	  ((null? (cdr args)) (car args))
	  ((null? (cdr (cdr args))) (* (car args) (car (cdr args))))
	  (else (* (* (car args) (car (cdr args)))
		   (apply * (cdr (cdr args)))))))

  (define (- x . rest)
    (if (null? rest) 
	(- 0 x)
	(do ((n (- x (car rest)) (- n (car L)))
	     (L (cdr rest) (cdr L)))
	    ((null? L) n))))

  (define (/ x . rest)
    (if (null? rest) 
	(/ 1 x)
	(do ((n (/ x (car rest)) (/ n (car L)))
	     (L (cdr rest) (cdr L)))
	    ((null? L) n))))

  (define (@extend-transitive-rel rel?)
    (lambda (x y . rest)
      (and (rel? x y)
	   (if (null? rest) ; first test moved out of loop 'cause of dumb compiler
	       #t
	       (let loop ((y y) (rest rest))
		 (and (rel? y (car rest))
		      (if (null? (cdr rest))
			  #t
			  (loop (car rest) (cdr rest)))))))))

  (define <= (@extend-transitive-rel (lambda (x y) (<= x y))))
  (define <  (@extend-transitive-rel (lambda (x y) (< x y))))
  (define =  (@extend-transitive-rel (lambda (x y) (= x y))))

  (define (@0-or-1 proc default)
    (lambda opt:arg
      (proc
       (cond ((null? opt:arg) default)
	     ((null? (cdr opt:arg)) (car opt:arg))
	     (else (%error "Expected 0 or 1 args to" proc opt:arg))))))

  (define peek-char (@0-or-1 (lambda (x) (peek-char x)) (current-input-port)))
  (define read-char (@0-or-1 (lambda (x) (read-char x)) (current-input-port)))

  ; FIXME: er, default should be a thunk so that we get the -current- output
  ; port, below; doesn't matter right now because that never changes.
  (define (@1-or-2 proc default)
    (lambda (arg . opt:arg)
      (proc 
       arg
       (cond ((null? opt:arg) default)
	     ((null? (cdr opt:arg)) (car opt:arg))
	     (else (%error "Expected 1 or 2 args to" 
			   proc (cons arg opt:arg)))))))

  (define make-vector
    (@1-or-2 (lambda (x y) (make-vector x y)) #f))

  (define make-string 
    (@1-or-2 (lambda (x y) (make-string x y)) #\space))

  (define number->string
    (@1-or-2 (lambda (x y) (number->string x y)) 10))

  (define string->number
    (@1-or-2 (lambda (x y) (string->number x y)) 10))

  (define write-char
    (@1-or-2 (lambda (x y) (write-char x y)) (current-output-port)))

  (define write
    (@1-or-2 (lambda (x y) (write x y)) (current-output-port)))

  (define display
    (@1-or-2 (lambda (x y) (display x y)) (current-output-port)))

  ; `atan' takes either one argument or two:
  (define (atan arg1 . rest)
    (cond
      ((null? rest) (atan arg1))
      ((null? (cdr rest)) (atan arg1 (car rest)))
      (else (%error "Too many arguments -- ATAN" (cons arg1 rest)))))

  (define (append . lsts)
    (if (null? lsts)
	'()
	(let loop ((L1 (car lsts)) (lsts (cdr lsts)))
	  (if (null? lsts)
	      L1
	      (append L1 (loop (car lsts) (cdr lsts))))))))

;;;
;;; End of special variable-arity definitions
;;;


; Misc

(begin

  (define (equal? x y)
    (cond 
      ((eqv? x y) #t)
      ((pair? x)
       (and (pair? y)
	    (equal? (car x) (car y))
	    (equal? (cdr x) (cdr y))))
      ((string? x)
       (and (string? y)
	    (string=? x y)))
      ((vector? x)
       (and (vector? y)
	    (= (vector-length x) (vector-length y))
	    (let loop ((i (- (vector-length x) 1)))
	      (if (< i 0)
		  #t
		  (and (equal? (vector-ref x i) (vector-ref y i))
		       (loop (- i 1)))))))
      (else #f)))

  (define (@optional-arg arg-list default-value)
    (cond ((null? arg-list) default-value)
	  ((null? (cdr arg-list)) (car arg-list))
	  (else (%error "Too many arguments to procedure" arg-list))))


  ; List procs

  (define (list? x)
    (let loop ((slow x) (fast x))
      (if (null? fast)
	  #t
	  (and (pair? fast)
	       (if (null? (cdr fast))
		   #t
		   (and (pair? (cdr fast))
			(not (eq? slow (cdr fast)))
			(loop (cdr slow) (cddr fast))))))))

  (define (list-ref lst n)
    (car (list-tail lst n)))

  (define (list-tail lst n)
    (if (= n 0) 
	lst
	(list-tail (cdr lst) (- n 1))))

  (define (list . args) args)

  (define (@reduce fn id lst)
    (let loop ((lst lst))
      (if (null? lst)
	  id
	  (fn (car lst)
	      (loop (cdr lst))))))

  (define (member obj lst)
    (let loop ((lst lst))
      (cond ((null? lst) #f)
	    ((equal? obj (car lst)) lst)
	    (else (loop (cdr lst))))))

  (define (assoc obj a-list)
    (let loop ((a-list a-list))
      (cond ((null? a-list) #f)
	    ((equal? obj (car (car a-list)))
	     (car a-list))
	    (else (loop (cdr a-list))))))

  (define map
    ;; ...whence the need to insulate this particular built-in this way?
    (letrec ((map
	      (lambda (fn lst . lsts)
		(cond
		  ((null? lsts)		; special-case for speed
		   (let loop ((lst lst) (result '()))
		     (if (null? lst)
			 (reverse result)
			 (loop (cdr lst) 
			       (cons (fn (car lst)) result)))))
		  (else
		   (let loop ((lsts (cons lst lsts)) (result '()))
		     (if (null? (car lsts))
			 (reverse result)
			 (loop (map cdr lsts)
			       (cons (apply fn (map car lsts)) result)))))))))
      map))

  (define (for-each fn lst . lsts)
    (cond
     ((null? lsts)		; special-case for speed
      (let loop ((lst lst))
	(cond ((not (null? lst))
	       (fn (car lst))
	       (loop (cdr lst))))))
     (else
      (let loop ((lsts (cons lst lsts)))
	(cond ((not (null? (car lsts)))
	       (apply fn (map car lsts))
	       (loop (map cdr lsts))))))))


  ; Numbers

  (define (gcd . args)
    (define (gcd2 a b)
      (if (= b 0)
	  a
	  (gcd2 b (remainder a b))))
    (let loop ((g 0) (args args))
      (if (null? args)
	  g
	  (loop (gcd2 (abs (car args)) g)
		(cdr args)))))

  (define (lcm . args)
    (let loop ((L 1) (args args))
      (if (null? args)
	  L
	  (loop (let ((g (gcd (car args) L)))
		  (if (= g 0)
		      g
		      (* (quotient (abs (car args)) g) L)))
		(cdr args)))))

  (define (abs n) (if (< n 0) (- 0 n) n))

  (define (positive? n)  (< 0 n))
  (define (zero? n)      (= n 0))
  (define (negative? n)  (< n 0))

  (define (odd? n)  (= (modulo n 2) 1))
  (define (even? n) (= (remainder n 2) 0))

  (define >  (@extend-transitive-rel (lambda (x y) (< y x))))
  (define >= (@extend-transitive-rel (lambda (x y) (<= y x))))

  (define (min n . L)
    (do ((n n (if (<= n (car L))
		  (@inexact-contagion n (car L))
		  (@inexact-contagion (car L) n)))
	 (L L (cdr L)))
	((null? L) n)))

  (define (max n . L)
    (do ((n n (if (< (car L) n) 
		  (@inexact-contagion n (car L))
		  (@inexact-contagion (car L) n)))
	 (L L (cdr L)))
	((null? L) n)))

  (define (@inexact-contagion x y)
    (if (inexact? y)
	(exact->inexact x)
	x))

  (define (ceiling n) (- 0 (floor (- 0 n))))

  (define (truncate n)
    (if (< n 0)
	(ceiling n)
	(floor n)))


  ; Characters

  (define (char>=? c1 c2) (not (char<? c1 c2)))
  (define (char>? c1 c2)  (char<? c2 c1))

  (define (@char-ci-tester test?)
    (lambda (c1 c2) (test? (char-upcase c1) (char-upcase c2))))

  (define char-ci<?  (@char-ci-tester (lambda (c1 c2) (char<? c1 c2))))
  (define char-ci<=? (@char-ci-tester (lambda (c1 c2) (char<=? c1 c2))))
  (define char-ci=?  (@char-ci-tester (lambda (c1 c2) (char=? c1 c2))))
  (define char-ci>=? (@char-ci-tester char>=?))
  (define char-ci>?  (@char-ci-tester char>?))

  (define (char-numeric? c)     (and (char<=? #\0 c) (char<=? c #\9)))
  (define (char-lower-case? c)  (and (char<=? #\a c) (char<=? c #\z)))
  (define (char-upper-case? c)  (and (char<=? #\A c) (char<=? c #\Z)))

  (define (char-alphabetic? c)  (if (char-lower-case? c) #t (char-upper-case? c)))

  (define (char-upcase c)
    (if (char-lower-case? c)
	(integer->char (+ (char->integer c)
			  (- (char->integer #\A) (char->integer #\a))))
	c))

  (define (char-downcase c)
    (if (char-upper-case? c)
	(integer->char (+ (char->integer c)
			  (- (char->integer #\a) (char->integer #\A))))
	c))


  ; Vector procs

  (define (vector . elements) (list->vector elements))

  (define (vector-fill! vec fill)
    (do ((n (vector-length vec) (- n 1)))
	((= n 0))
      (vector-set! vec (- n 1) fill)))

  (define (vector->list vec)
    (do ((n (vector-length vec) (- n 1))
	 (L '() (cons (vector-ref vec (- n 1)) L)))
	((= n 0) L)))


  ; Strings

  (define (@string-compare <? =? length-compare?)
    (lambda (s1 s2)
      (let ((limit (min (string-length s1) (string-length s2))))
	(let loop ((i 0))
	  (cond
	    ((= i limit)
	     (length-compare? (string-length s1) (string-length s2)))
	    ((=? (string-ref s1 i) (string-ref s2 i))
	     (loop (+ i 1)))
	    (else (<? (string-ref s1 i) (string-ref s2 i))))))))

  (define string<?  (@string-compare char<? char=? (lambda (x y) (< x y))))
  (define string<=? (@string-compare char<? char=? (lambda (x y) (<= x y))))

  (define (string>? s1 s2)  (string<? s2 s1))
  (define (string>=? s1 s2) (string<=? s2 s1))

  (define string-ci<? 
    (@string-compare char-ci<? char-ci=? (lambda (x y) (< x y))))

  (define string-ci<=? 
    (@string-compare char-ci<? char-ci=? (lambda (x y) (<= x y))))

  (define string-ci=? 
    (@string-compare (lambda (x y) #f) char-ci=? (lambda (x y) (= x y))))

  (define (string-ci>=? s1 s2) (string-ci<=? s2 s1))
  (define (string-ci>? s1 s2)  (string-ci<? s2 s1))

  (define (string-append . strs)
    (let ((result
	   (make-string 
	    (let loop ((sum 0) (strs strs))
	      (if (null? strs)
		  sum
		  (loop (+ (string-length (car strs)) sum)
			(cdr strs))))
	    #\x)))
      (let loop ((i 0) (strs strs))
	(if (null? strs)
	    result
	    (let ((s (car strs)) (L (string-length (car strs))))
	      (let copy-s ((j (- L 1)))
		(if (< j 0)
		    (loop (+ i L) (cdr strs))
		    (begin (string-set! result (+ i j) (string-ref s j))
			   (copy-s (- j 1))))))))))

  (define (substring str start end)
    (if (< end start)
	(%error "End of substring precedes start" start end)
	(let ((result (make-string (- end start) #\x)))
	  (let loop ((i start))
	    (if (= i end)
		result
		(begin (string-set! result (- i start) (string-ref str i))
		       (loop (+ i 1))))))))

  (define (string-fill! str fill)
    (do ((n (string-length str) (- n 1)))
	((= n 0))
      (string-set! str (- n 1) fill)))

  (define (string . chars) (list->string chars))

  (define (list->string lst)
    (let ((str (make-string (length lst) #\x)))
      (do ((i 0 (+ i 1))
	   (lst lst (cdr lst)))
	  ((null? lst) str)
	(string-set! str i (car lst)))))

  (define (string->list str)
    (do ((n (string-length str) (- n 1))
	 (L '() (cons (string-ref str (- n 1)) L)))
	((= n 0) L)))

  (define (string-copy str)
    (let ((copy (make-string (string-length str) #\x)))
      (do ((i (- (string-length str) 1) (- i 1)))
	  ((< i 0) copy)
	(string-set! copy i (string-ref str i)))))


  ; I/O

  (define (newline . opt:port)
    (write-char #\newline
		(@optional-arg opt:port (current-output-port))))

  (define (call-with-input-file file proc)
    (let ((port (open-input-file file)))
      (let ((result (proc port)))
	(close-input-port port)
	result)))

  (define (call-with-output-file file proc)
    (let ((port (open-output-file file)))
      (let ((result (proc port)))
	(close-output-port port)
	result)))

  ;(define (with-input-from-file file proc)
  ;  (let ((prev-port @current-input-port))
  ;    (call-with-input-file file
  ;      (lambda (port)
  ;	(set! @current-input-port port)
  ;	(proc)))
  ;    (set! @current-input-port prev-port)))

  ;(define (with-output-to-file file proc)
  ;  (let ((prev-port @current-output-port))
  ;    (call-with-output-file file
  ;      (lambda (port)
  ;	(set! @current-output-port port)
  ;	(proc)))
  ;    (set! @current-output-port prev-port)))

  ;(define (current-input-port) @current-input-port)
  ;(define (current-output-port) @current-output-port)

)

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

    (define the-readtable (make-vector 256 @read-atom))

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
      (@skip-blanks in-port)
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
		   (@skip-blanks in-port)
		   (let ((char (peek-char in-port)))
		     (cond
		       ((eof-object? char) 
			(read-error in-port "Unexpected EOF in list"))
		       ((char=? char #\) )
			(read-char in-port)
			'())
		       ((char=? char #\.)
			(read-char in-port)
			(let ((next (@read-atom in-port char)))
			  (if (not (eq? next dot-symbol))
			      (cons next (loop))
			      (let ((result (read in-port)))
				(@skip-blanks in-port)
				(if (not (eqv? (read-char in-port) #\) ))
				    (read-error 
				     in-port 
				     "Unbalanced parentheses: no trailing ')'"))
				result))))
		       (else (read-rest-of-list))))))))))))

    (define (read-number in-port)
      (let ((n (@read-atom in-port)))
	(if (number? n)
	    n
	    (read-error in-port "Expected a number" n))))

    (define (read-error port message . irritants)
      (set! %error-cont #f)
      (@complain "Read error" message irritants)
      (%flush-input-line port)
      (%reset '*))
    

    ;; White space
    (let ((read-after (lambda (in-port char) 
	                (@skip-blanks in-port)
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

    (install-read-macro #\#
      (lambda (in-port char)
	(if (memv (peek-char in-port) 
		  '(#\x #\d #\o #\b #\e #\i #\X #\D #\O #\B #\E #\I))
	    (let ((n (@read-atom in-port char)))
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
		       (let ((symbol (@read-atom in-port next)))
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
		  (read-error in-port "Unexpected EOF in escape sequence"))
		 ((assv char '((#\\ . #\\)
			       (#\" . #\")
			       (#\n . #\newline)
			       (#\t . #\tab)
			       (#\r . #\return)))
		  => (lambda (pair)
		       (loop (cons (cdr pair) prev-chars))))
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
	(list 
	 (if (eqv? (peek-char in-port) #\@)
	     (begin (read-char in-port) 'unquote-splicing)
	     'unquote)
	 (read in-port))))

    (lambda opt:in-port
      (read (@optional-arg opt:in-port (current-input-port))))))



;;;; 
;;;; More miscellany
;;;;

(define unspecified (string->symbol "#!unspecified"))

(define (andmap test? ls)
  (if (null? ls)
      #t
      (and (test? (car ls))
	   (andmap test? (cdr ls)))))


;;;;
;;;; lex-env.scm
;;;;

(begin

  (define make-lexical-address   cons)
  (define lexical-address/depth  car)
  (define lexical-address/offset cdr)

  ;; Return the lexical address, or v itself if free.
  (define (lexical-env/lookup s v)
    (let nesting ((depth 0)
		  (s s))
      (if (null? s)
	  v
	  (let searching ((vars (car s))
			  (index 0))
	    (cond ((null? vars)
		   (nesting (+ depth 1) (cdr s)))
		  ((eq? (car vars) v)
                   (if (< 255 depth) (%error "Code too complex: nesting too deep"))
                   (if (< 255 index) (%error "Code too complex: too many locals"))
		   (make-lexical-address depth index))
		  (else
		   (searching (cdr vars) (+ index 1))))))))

  (define lexical-env/empty '())

  (define (lexical-env/extend s vals)
    (cons vals s))

  ;; Get the name behind a lexical address.
  ;; If the address is not mapped, return '<?>
  ;; -- that's not expected to come up, but I want this
  ;; to be robust to stripping debug info.
  (define (locals-map-ref locals-map depth offset)
    (list-ref/default (list-ref/default locals-map depth '())
                      offset
                      '<?>))

  (define (list-ref/default ls n default)
    (let loop ((ls ls) (n n))
      (cond ((null? ls) default)
            ((= n 0) (car ls))
            (else (loop (cdr ls) (- n 1)))))))


;;;;
;;;; parse.scm
;;;;

(begin

  ;;;
  ;;; Constants tables
  ;;;

  (define (constants/new)
    (cons 0 '()))

  (define (constants/lookup datum constants)
    (cond ((assv datum (cdr constants))
	   => cdr)
	  (else
	   (let ((c (car constants)))
             (if (<= 255 c) (%error "Code too complex: too many constants"))
	     (set-car! constants (+ c 1))
	     (set-cdr! constants (cons (cons datum c) (cdr constants)))
	     c))))

  (define (constants->vector constants)
    (let ((vec (make-vector (car constants) #f)))
      (for-each (lambda (pair)
		  (vector-set! vec (cdr pair) (car pair)))
		(cdr constants))
      vec))


  ;;;
  ;;; Lap
  ;;; A misnomer, in that these are raw bytecodes...
  ;;;

  (define @%lit 0)
  (define @%varref 1)
  (define @%varset 2)
  (define @%global-ref 3)
  (define @%global-set 4)
  (define @%global-define 5)
  (define @%if-false 6)
  (define @%jump 7)
  (define @%proc 8)
  (define @%extend-normal-env 9)
  (define @%extend-&rest-env 10)
  (define @%restore 11)
  (define @%invoke 12)
  (define @%save 13)
  (define @%apply 14)
  (define @%get-cc 15)
  (define @%set-cc 16)
  (define @%prim-0 17)
  (define @%prim-1 18)
  (define @%prim-2 19)
  (define @%prim-3 20)
  (define @%drop 21)
  (define @%halt 22)

  (define instruc-names            ;; Needed by the disassembler.
    '#(lit
       varref 
       varset
       global-ref
       global-set
       global-define
       if-false
       jump
       proc
       extend-normal-env
       extend-&rest-env
       restore
       invoke
       save
       apply
       get-cc
       set-cc
       prim-0
       prim-1
       prim-2
       prim-3
       drop
       halt))

  (define @instruc-args 
    '#((d) (v) (v) (d) (d) (d) (w) (w) (d) (locals) (locals) () () (w) () () () (b) (b) (b) (b) () ()))

  (define lap/position length)
  (define lap/append append)

  (define lap/restore 
    (list @%restore))

  (define (lap/offset pos lap)
    (let ((offset (- (lap/position lap) pos)))
      (cons (quotient offset 256)
	    (cons (remainder offset 256)
		  lap))))

  (define (lap/jump pos lap)
    (cons @%jump
	  (lap/offset pos lap)))

  (define (lap/if-false pos lap)
    (cons @%if-false
	  (lap/offset pos lap)))

  (define (lap/varref addr lap)
    (cons @%varref
	  (cons (lexical-address/depth addr)
		(cons (lexical-address/offset addr)
		      lap))))

  (define (lap/varset addr lap)
    (cons @%varset
	  (cons (lexical-address/depth addr)
		(cons (lexical-address/offset addr)
		      lap))))

  (define (lap/extend-normal-env count lap)
    (cons @%extend-normal-env
	  (cons count lap)))

  (define (lap/extend-&rest-env count lap)
    (cons @%extend-&rest-env
	  (cons count lap)))

  (define (lap/save pos lap)
    (cons @%save 
	  (lap/offset pos lap)))

  (define (lap/invoke lap)
    (cons @%invoke lap))

  (define (lap/drop lap)
    (cons @%drop lap))

  (define (lap/prim-0 prim lap)
    (cons @%prim-0
	  (cons prim lap)))

  (define (lap/prim-1 prim lap)
    (cons @%prim-1
	  (cons prim lap)))

  (define (lap/prim-2 prim lap)
    (cons @%prim-2
	  (cons prim lap)))

  (define (lap/prim-3 prim lap)
    (cons @%prim-3
	  (cons prim lap)))

  (define (lap/lit datum constants lap)
    (cons @%lit
	  (cons (constants/lookup datum constants) 
		lap)))

  (define (lap/global-ref symbol constants lap)
    (cons @%global-ref
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/global-set symbol constants lap)
    (cons @%global-set
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/global-define symbol constants lap)
    (cons @%global-define
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/proc code constants lap)
    (cons @%proc
	  (cons (constants/lookup code constants)
		lap)))


  (define gensym
    (let ((counter 0))
      (lambda ()
	(set! counter (+ counter 1))
	(string->symbol
	 (string-append "#!g_" (number->string counter))))))

  (define *open-code-primitives?* #t)

  ;; List of global functions to spare from tail call optimization, so the 
  ;; caller's frame stays visible on the stack. User-settable.
  (define %dont-tail-on-me '(error %error %avast))


  ;; (PARSE-FORM form) returns a compiled code vector for a top-level form.
  ;; The code will assume no params and an empty lexical environment ("top-level").
  (define parse-form 
    (let ()

      ;; The expression parser.

      (define (parse-exp constants label exp s k)

	(let pe ((exp exp) (k k))

	  (cond

	    ((symbol? exp) 
	     (let ((addr (lexical-env/lookup s exp)))
	       (if (symbol? addr) 
		   (lap/global-ref addr constants k)
		   (lap/varref addr k))))

	    ((not (pair? exp))
	     (lap/lit exp constants k))

	    (else
	     (let ((rator (car exp)) 
		   (rands (cdr exp))
		   (num-rands (length (cdr exp)))
		   (assert (lambda (ok?) 
			     (if (not ok?) (syntax-error "Bad syntax" exp)))))

	       (case rator

		 ((quote)
		  (assert (= num-rands 1))
		  (lap/lit (car rands) constants k))

		 ((if)
		  (assert (or (= num-rands 2) 
			      (= num-rands 3)))
		  (let ((consequent (cadr rands))
			(alternative (if (= num-rands 3) 
					 (caddr rands) 
					 `',unspecified)))
		    (pe (car rands)
			(if (eq? k lap/restore)
			    (let ((e (pe alternative k)))
			      (lap/if-false (lap/position e)
					    (lap/append (pe consequent k)
							e)))
			    (let ((j (lap/position k))
				  (e (pe alternative k)))
			      (lap/if-false (lap/position e)
					    (pe consequent 
						(lap/jump j e))))))))

		 ((lambda)
		  (assert (and (< 1 num-rands)
			       (valid-formals? (car rands))))
		  (let* ((formals (car rands))
			 (rest-args? (not (list? formals)))
			 (fixed-formals (if (not rest-args?)
					    formals
					    (let sans-dot ((f formals))
					      (if (pair? f)
						  (cons (car f) (sans-dot (cdr f)))
						  (list f))))))
		    (let ((body-constants (constants/new))
                          (var-count (length fixed-formals))
                          (nest-s (lexical-env/extend s fixed-formals)))
		      (let ((lap (parse-exp body-constants
					    label 
					    (expand-lambda-body (cdr rands))
					    nest-s
					    lap/restore)))
			(lap/proc
			 (codify (if rest-args?
				     (lap/extend-&rest-env (- var-count 1) lap)
				     (lap/extend-normal-env var-count lap))
				 (constants->vector body-constants)
				 label
                                 nest-s)
			 constants
			 k)))))

		 ((let)
		  (assert (pair? rands))
		  (pe 
		   (if (symbol? (car rands)) ; named-let form
		       (begin
			 (assert (valid-let? (cdr rands)))
			 (let ((proc (car rands)) 
			       (decls (cadr rands)) 
			       (body (cddr rands)))
			   `((letrec ((,proc (lambda ,(map car decls) . ,body)))
			       ,proc)
			     . ,(map cadr decls))))
		       (begin
			 (assert (valid-let? rands))
			 (let ((names (map car (car rands)))
			       (exps (map cadr (car rands)))
			       (body (cdr rands)))
			   (if (null? names)
			       (expand-lambda-body body)
			       `((lambda ,names . ,body)
				 . ,(map (lambda (name exp)
					   `(@label ,name ,exp))
					 names
					 exps))))))
		   k))

		 ((letrec)
		  (assert (valid-let? rands))
		  (let ((vars (map car (car rands)))
			(exps (map cadr (car rands)))
			(body (cdr rands)))
		    (pe 
		     `((lambda ,vars 
			 ,@(map (lambda (var exp) 
				  `(set! ,var (@label ,var ,exp)))
				vars 
				exps)
			 (let () . ,body))
		       . ,(map (lambda (var) ''*uninitialized*) vars))
		     k)))

		 ((set!)
		  (assert (and (symbol? (car rands))
			       (= num-rands 2)))
		  (let ((name (car rands))
			(exp (cadr rands)))
		    (let ((addr (lexical-env/lookup s name)))
		      (pe exp
			  (if (symbol? addr) 
			      (lap/global-set addr constants k)
			      (lap/varset addr k))))))

		 ((begin)
		  (if (null? rands)
		      (lap/lit unspecified constants k)
		      (let loop ((head (car rands)) (tail (cdr rands)))
			(pe head
			    (if (null? tail)
				k
				(lap/drop (loop (car tail) (cdr tail))))))))

		 ((let*)
		  (assert (pair? rands))
		  (pe
		   (let ((decls (car rands))
			 (body (cdr rands)))
		     (if (null? decls)
			 `(let () . ,body)
			 `(let (,(car decls))
			    (let* ,(cdr decls) . ,body))))
		   k))

		 ((and)
		  (pe
		   (case num-rands
		     ((0) #t)
		     ((1) (car rands))
		     (else `(if ,(car rands) (and . ,(cdr rands)) #f)))
		   k))

		 ((or)
		  (pe
		   (case num-rands
		     ((0) #f)
		     ((1) (car rands))
		     (else (let ((head (gensym)))
			     `(let ((,head ,(car rands)))
				(if ,head ,head (or . ,(cdr rands)))))))
		   k))

		 ((cond)
		  (pe
		   (cond
		     ((null? rands) `',unspecified)
		     ((not (pair? (car rands)))
		      (syntax-error "Invalid cond clause" (car rands)))
		     ((eq? (caar rands) 'else)
		      (if (null? (cdr rands))
			  `(begin . ,(cdar rands))
			  (syntax-error "Else-clause is not last" rands)))
		     ((null? (cdar rands))
		      `(or ,(caar rands) (cond . ,(cdr rands))))
		     ((and (pair? (cdar rands)) (eq? (cadar rands) '=>))
		      (if (not (and (list? (car rands))
				    (= (length (car rands)) 3)))
			  (syntax-error "Bad cond clause syntax" rands))
		      (let ((test-var (gensym)))
			`(let ((,test-var ,(caar rands)))
			   (if ,test-var
			       (,(caddar rands) ,test-var)
			       (cond . ,(cdr rands))))))
		     (else `(if ,(caar rands) 
				(begin . ,(cdar rands))
				(cond . ,(cdr rands)))))
		   k))

		 ((case)
		  (assert (pair? rands))
		  (pe
		   (let ((test (car rands))
			 (sym (gensym)))
		     `(let ((,sym ,test))
			(cond 
			 . ,(map 
			     (lambda (clause)
			       (cond
				 ((eq? (car clause) 'else)
				  clause)
				 ((null? (cdar clause))
				  `((@primitive eqv?
				     ,sym ',(caar clause)) . ,(cdr clause)))
				 (else
				  `((@primitive memv
				     ,sym ',(car clause)) . ,(cdr clause)))))
			     (cdr rands)))))
		   k))

		 ((do)
		  (assert (and (<= 2 num-rands)
			       (let valid-clauses? ((clauses (car rands)))
				 (if (null? clauses)
				     #t
				     (let ((clause (car clauses)))
				       (and (pair? clause)
					    (symbol? (car clause))
					    (pair? (cdr clause))
					    (if (null? (cddr clause))
						#t
						(null? (cdddr clause)))
					    (valid-clauses? (cdr clauses))))))
			       (pair? (cadr rands))))
		  (pe
		   (let ((loop (gensym))
			 (variables (map car (car rands)))
			 (inits (map cadr (car rands)))
			 (steps (map (lambda (clause)
				       (if (null? (cddr clause))
					   (car clause) ;default step leaves var
							;unchanged.
					   (caddr clause)))
				     (car rands)))
			 (test (caadr rands))
			 (result (cdadr rands))
			 (body (cddr rands)))
		     `(letrec ((,loop 
				(lambda ,variables
				  (if ,test
				      (begin . ,result)
				      (begin 
					,@body
					(,loop . ,steps))))))
			(,loop . ,inits)))
		   k))

		 ((quasiquote)
		  (assert (= num-rands 1))
		  (pe
		   (expand-quasiquote (car rands) 0)
		   k))

		 ((define)
		  (assert #f)) ; (define ...) is never a valid expression

		 ((@label)
		  (assert (= num-rands 2))
                  ;; (@label NAME EXP) in a context labeled FOO
                  ;;  parses EXP in a context labeled (NAME . FOO).
                  ;;  This labels any closures created in EXP, for display
                  ;;  by put_object in utsvm.c, case a_closure.
		  (parse-exp constants
			     (cons (car rands) label)
			     (cadr rands)
			     s
			     k))

		 ((@primitive)
		  (assert (and (<= 1 num-rands) (<= num-rands 4)))
                  (let* ((nr (- num-rands 1))
                         (prim (prim-lookup (car rands) nr)))
                    (parse-prim-app pe prim (cdr rands) nr k)))

                 ((%yo) ;; crude printf-debugging convenience
                  (assert (= num-rands 1))
                  (pe `(let ((v ,(car rands)))
                         ;; XXX hygiene
                         (display "[%yo ")
                         (write ',(car rands))
                         (display " : ")
                         (write v)
                         (display "]\n")
                         v)
                      k))

                 (else
                   (cond
                    ;; application, maybe needing special handling
		    ((and (symbol? rator)
		          (symbol? (lexical-env/lookup s rator)))
                     (cond ((and *open-code-primitives?*
		                 (prim-lookup rator num-rands))
		            => (lambda (prim)
		                 (parse-prim-app pe prim rands num-rands k)))
                           (else
                             (parse-call pe (not (memq rator %dont-tail-on-me)) rator rands k))))
		    (else
                      (parse-call pe #t rator rands k))))))))))


      ;; Calls and primitive applications

      (define (parse-call pe tail-ok? rator rands k)
	(let ((lin (lambda (k2)
		     (@reduce pe 
			      (pe rator 
				  (lap/invoke k2))
			      rands))))
	  (if (and tail-ok? (eq? k lap/restore))
	      (lin '())
	      (lap/save (lap/position k)
			(lin k)))))

      (define (parse-prim-app pe prim rands num-rands k)
        (let ((primop-k ((case num-rands
		           ((0) lap/prim-0)
		           ((1) lap/prim-1)
		           ((2) lap/prim-2)
		           ((3) lap/prim-3))
	                 prim
	                 k)))
          (@reduce pe primop-k rands)))

      (define (prim-lookup sym num-rands)
        (cond ((assq sym (case num-rands
			   ((0) prim-0-list)
			   ((1) prim-1-list)
			   ((2) prim-2-list)
			   ((3) prim-3-list)
			   (else '())))
               => cadr)
              (else #f)))


      ;;
      ;; Lambdas and defines
      ;;

      ; True iff formals is a (proper or improper) list of symbols, all distinct.
      (define (valid-formals? formals) 
	(let loop ((formals formals) (vars '()))
	  (define (valid-var? formal)
	    (and (symbol? formal)
		 (not (memq formal vars))))
	  (or (null? formals)
	      (valid-var? formals)
	      (and (pair? formals)
		   (valid-var? (car formals))
		   (loop (cdr formals)
			 (cons (car formals) vars))))))   ;** avoid this consing

      ; sexps is a list of expressions forming a lambda body.
      ; Return a single equivalent expression with any internal defines 
      ; made into a letrec.
      (define (expand-lambda-body sexps)
	(scan-out-defines sexps
	  (lambda (vars defs body)
	    `(letrec ,(map list vars defs) . ,body))
	  (lambda (body) `(begin . ,body))))

      ; Find leading definitions in sexps; 
      ; if none, return (BODY-k sexps);
      ; if any, return (DEF-K vars defs body)
      ;         where vars is a list of all variables so defined,
      ;               defs is the corresponding value expressions,
      ;           and body is the remaining sexps.
      (define (scan-out-defines sexps def-k body-k)
	(if (null? sexps) 
	    (syntax-error "Lambda expression has no body")
	    (maybe-parse-definition (car sexps)
	      (lambda (vars1 exps1)
		(scan-out-defines (cdr sexps)
		  (lambda (vars exps body) 
		    (def-k (append vars1 vars) (append exps1 exps) body))
		  (lambda (body)
		    (def-k vars1 exps1 body))))
	      (lambda ()
		(body-k sexps)))))

      ; If sexp is a definition, return (SUCCEED vars defs) with vars and
      ; defs as in scan-out-defines; else return (FAIL).
      (define (maybe-parse-definition sexp succeed fail)
	(cond
	  ((not (pair? sexp)) (fail))
	  ((eq? (car sexp) 'define)
	   (parse-define sexp succeed))
	  ((eq? (car sexp) 'begin)
	   (let loop ((defs (cdr sexp)) (succeed succeed) (fail fail))
	     (cond
	       ((null? defs) (succeed '() '()))
	       ((not (pair? defs)) (fail))
	       (else
		(maybe-parse-definition (car defs)
		  (lambda (vars1 exps1)
		    (loop (cdr defs)
		      (lambda (vars exps)
			(succeed (append vars1 vars) (append exps1 exps)))
		      (lambda () 
			(syntax-error "Invalid definition" sexp))))
		  fail)))))
	  (else (fail))))

      ; Pre: (and (pair? sexp) (eq? (car sexp) 'define))
      ; Return (K vars defs), where vars and defs are as above.
      ; (But note they're always 1-element lists...)
      (define (parse-define sexp k)
	(cond
	  ((or (not (list? sexp))
	       (< (length sexp) 3))
	   (syntax-error "Invalid definition" sexp))
	  ((symbol? (cadr sexp))
	   (if (= (length sexp) 3)
	       (k (list (cadr sexp)) (list (caddr sexp)))
	       (syntax-error "Invalid definition" sexp)))
	  ((and (pair? (cadr sexp))
		(valid-formals? (cdadr sexp)))
	   (k (list (car (cadr sexp)))
	      (list `(lambda ,(cdadr sexp) . ,(cddr sexp)))))
	  (else (syntax-error "Invalid definition" sexp))))


      ;; LET forms

      ; True iff `(LET ,rands) is a valid let-expression (simple, not 
      ; named LET).  This is helpful in parsing named LET and LETREC, too.
      (define (valid-let? rands)
	(and (<= 2 (length rands))
	     (list? (car rands))
	     (andmap (lambda (decl) 
			(and (pair? decl)
			     (pair? (cdr decl))
			     (null? (cddr decl))
			     (symbol? (car decl))))
		      (car rands))
	     (valid-formals? (map car (car rands)))))


      ;; Backquote expansion: based on _Paradigms of AI Programming_ p. 824
      ;; (adapted to expand nested quasiquotes as in R4RS, and for hygiene)

      (define (expand-quasiquote exp nesting)
	(define (assert ok?)
	  (if (not ok?) (syntax-error "Bad syntax" exp)))
	(cond
	 ((vector? exp)
	  `(@primitive list->vector
		       ,(expand-quasiquote (vector->list exp) nesting)))
	 ((not (pair? exp)) 
	  (if (constant? exp) exp (list 'quote exp)))
	 ((eq? (car exp) 'unquote)
	  (assert (= (length exp) 2))
	  (if (= nesting 0)
	      (cadr exp)
	      (combine-skeletons ''unquote 
				 (expand-quasiquote (cdr exp) (- nesting 1))
				 exp)))
	 ((eq? (car exp) 'quasiquote)
	  (assert (= (length exp) 2))
	  (combine-skeletons ''quasiquote 
			     (expand-quasiquote (cdr exp) (+ nesting 1))
			     exp))
	 ((and (pair? (car exp))
	       (eq? (caar exp) 'unquote-splicing))
	  (assert (= (length (car exp)) 2))
	  (if (= nesting 0)
	      (if (null? (cdr exp))
		  (cadar exp)
		  `(@primitive append
			       ,(cadar exp)
			       ,(expand-quasiquote (cdr exp) nesting)))
	      (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
				 (expand-quasiquote (cdr exp) nesting)
				 exp)))
	 (else (combine-skeletons (expand-quasiquote (car exp) nesting)
				  (expand-quasiquote (cdr exp) nesting)
				  exp))))

      (define (combine-skeletons left right exp)
	(define (my-eval constant)
	  (if (pair? constant)       ;; must be quoted constant
	      (cadr constant)
	      constant))
	(if (and (constant? left) (constant? right)) 
	    (if (and (eqv? (my-eval left) (car exp))
		     (eqv? (my-eval right) (cdr exp)))
		(list 'quote exp)
		(list 'quote (cons (my-eval left) (my-eval right))))
	    `(@primitive cons ,left ,right)))

      (define (constant? exp)
	(if (pair? exp)
	    (eq? (car exp) 'quote)
	    (not (symbol? exp))))


      ;; Error handling

      (define (syntax-error message . irritants)
	(%error "Syntax error" message irritants))


      ;; Body of PARSE-FORM

      (lambda (form)
	(let ((constants (constants/new)))
	  (let ((lap
		 (maybe-parse-definition 
		  form
		  (lambda (names exps)
		    (do ((names (reverse names) (cdr names))
			 (exps  (reverse exps)  (cdr exps))
			 (k lap/restore
			    (parse-exp constants
				       (car names)
				       (car exps)
				       lexical-env/empty
				       (lap/global-define (car names) 
							  constants
							  (if (null? (cdr names))
							      k
							      (lap/drop k))))))
			((null? names) k)))
		  (lambda () 
		    (parse-exp constants '() form lexical-env/empty 
			       lap/restore)))))
	    (codify lap (constants->vector constants) #f lexical-env/empty))))))


  (define (@make-code-vector constants-vec bytes label locals-map)
    (vector 'code-vector constants-vec bytes label locals-map 0))

  (define (codify lap constants-vec label locals-map)
    (@make-code-vector constants-vec
		       (list->string (map integer->char lap))
		       label
                       locals-map))

  (define (%eval form)
    ;; Compile to a top-level procedure with no params, and call it.
    ((@make-closure '#() (parse-form form)))))


;;;;
;;;; globals.scm
;;;;

(begin

  (define prim-0-list
    '((current-input-port	 0) 
      (current-output-port 	 1)
      (%runtime		 	 2)
      ))

  (define prim-1-list
    '((boolean?		 	 0) 
      (char?			 1) 
      (eof-object?		 2) 
      (exact?			 3) 
      (inexact?			 4)
      (input-port?		 5) 
      (integer?			 6) 
      (number?			 7) 
      (output-port?		 8)
      (pair?			 9)
      (procedure?		10)
      (string?			11) 
      (symbol?			12) 
      (vector?			13)
      (floor			14)
      (round			15) 
      (exact->inexact		16) 
      (inexact->exact		17) 
      (integer->char		18)
      (char->integer		19)
      (car			20)
      (cdr			21)
      (close-input-port		22)
      (close-output-port	23) 
      (open-input-file		24)
      (open-output-file		25)
      (string->symbol		26)
      (symbol->string		27)
      (string-length		28)
      (vector-length		29)
      (sqrt			30) 
      (exp			31) 
      (log			32) 
      (sin			33) 
      (cos			34) 
      (tan			35) 
      (asin			36) 
      (acos			37)
      (atan			38)
      (@closure->lex-env	39)
      (@closure->code		40)
      (null?			41) 
      (not			42)
      (char-whitespace?		43)
      (reverse			44)
      (length			45) 
      (@skip-blanks		46)
      (%flush-input-line	47)
      (%read-fasl-header	48)
      (%read-fasl		49)
      (%exit			50)
      (peek-char  		51)
      (read-char 		52)
      (list->vector     	53)
      (%system                  54)
      (bitwise-not		55)
      ))

  (define prim-2-list
    '((eq? 			 0) 
      (eqv? 			 1)
      (string=? 		 2)
      (modulo 			 3) 
      (quotient 		 4)
      (remainder 		 5)
      (cons 			 6) 
      (set-car! 		 7)
      (set-cdr! 		 8)
      (string-ref 		 9)
      (vector-ref 		10)
      (@make-closure 		11)
      (expt 			12) 
      (atan 			13)
      (assq 			14)
      (assv 			15)
      (char<? 			16)
      (char<=? 			17)
      (char=? 			18)
      (@read-atom 		19)
      (@display-string		20)
      (write-char 		21)
      (+ 			22) 
      (- 			23)
      (* 			24)
      (/ 			25)
      (< 			26) 
      (<= 			27)
      (= 			28) 
      (make-vector 		29)
      (make-string 		30)
      (number->string 		31) 
      (string->number 		32)
      (memq             	33)
      (memv             	34)
      (append			35)
      (write			36)
      (display			37)
      (bitwise-and		38)
      (bitwise-ior		39)
      (bitwise-xor		40)
      (arithmetic-shift		41)
      ))

  (define prim-3-list
    '((string-set! 		 0) 
      (vector-set! 		 1)
      ))

  (define variable-arity-prim-list
    '(peek-char read-char 
      write-char + - * / < <= = 
      make-vector make-string number->string string->number
      append write display)))


;;;;
;;;; repl.scm
;;;;

(begin

  ;; Loading

  (define (load file)
    (call-with-input-file file
      (lambda (port)
	(let loop ()
	  (let ((exp (read port)))
	    (cond ((not (eof-object? exp))
		   (%eval exp)
		   (loop))))))))

  ;; Read-eval-print loop.

  (define %reset                ; (this definition will be reassigned)
    (lambda (val)
      (display "*** ERROR DURING STARTUP." (current-output-port))
      (newline (current-output-port))
      (%exit 1)))

  (define %arguments-to-scheme '())

  (define (@start-scheming)  ; called by utsvm once this fasl file is loaded
    (set! %arguments-to-scheme (cddr %command-line-arguments)) ; skip past the fasl file
    (cond ((pair? %arguments-to-scheme)
           (set! %reset (lambda (_) (%exit 1)))
	   (load (car %arguments-to-scheme)))
          (else
           (display "Enter an expression. On an error, enter ,d to debug. For more commands: ,help\n")
	   (call-with-current-continuation (lambda (k) (set! %reset k)))
           (%scheming))))

  (define (%scheming)  ; read-eval-print loop
    ;; In editing the following, stay conscious of what will appear
    ;; in backtraces on error: we want only one frame from this repl.
    (display "-> ")
    (let ((cmd (read)))
      (if (eof-object? cmd)
          (newline)
          (cond ((and (pair? cmd) (eq? (car cmd) 'unquote) (pair? (cdr cmd)) (null? (cddr cmd)))
                 ;; A comma command
                 (case (cadr cmd)
                   ((help)
                    (display ",help        - this message\n")
                    (display ",d           - (debug)\n")
                    (display ",l name      - (load \"name.scm\")\n")
                    (display ",l \"x.scm\"   - (load \"x.scm\")\n"))
                   ((d)
                    (debug))
                   ((l)
                    (let ((arg (read)))
                      (cond ((string? arg) (load arg))
                            ((symbol? arg) (load (string-append (symbol->string arg) ".scm")))
                            (else (display "usage: ,l \"string\" or ,l symbol\n")))))
                   (else
                     (display "Unknown ,command. Try ,help\n")))
                 (%scheming))
                (else
                  ;; An expression
	          (let ((obj (%eval cmd)))
                    (cond ((not (eq? obj unspecified))
                           (set! %%% %%)
                           (set! %% %)
                           (set! % obj)
	                   (write obj)
	                   (newline)))
	            (%scheming)))))))

  ;; Output history (with a short memory)
  (define % unspecified)
  (define %% unspecified)
  (define %%% unspecified)

  (define (%error message . irritants)
    (call-with-current-continuation 
      (lambda (cont)
	(set! %error-cont cont)
	(@complain "Error" message irritants)
	(%reset '*))))

  (define %error-cont '*)

  (define (@complain error-type message irritants)
    (newline) 
    (display "[")
    (display error-type)
    (display "!] ")
    (display message)
    (for-each (lambda (i) (newline) (write i))
	      irritants)
    (newline))

  (define (%proceed value)
    (if (procedure? %error-cont)
	(%error-cont value)
	(%error "No error to proceed from, or unproceedable")))

  ;; SRFI-23 compatible alias
  (define error %error))


;;;;
;;;; Disassembly
;;;;

(begin

  (define prim-name-vectors
    (vector (list->vector (map car prim-0-list))
	    (list->vector (map car prim-1-list))
	    (list->vector (map car prim-2-list))
	    (list->vector (map car prim-3-list))))

  (define (dis proc)
    (disassemble (@closure->code proc) -1))

  (define (disassemble code current-pc)
    (dump-asm (disassemble-instrucs code) 2 current-pc))

  (define (dump-asm asm margin current-pc)

    (define (write-prim arity index)
      (write-char #\space)
      (display (vector-ref (vector-ref prim-name-vectors arity) index))
      (newline))

    (define (indent margin) 
      (cond ((< 0 margin) 
	     (write-char #\space) 
	     (indent (- margin 1)))))

    (for-each (lambda (pair) 
		(let ((offset (car pair))
		      (i      (cadr pair)))

		  (cond ((= offset current-pc)
			 (indent (- margin 2))
			 (display "=>"))
			(else 
			 (indent margin)))

		  (let ((s (number->string offset)))
		    (indent (- 3 (string-length s)))
		    (display s)
		    (write-char #\space))

		  (write (car i))

		  (case (car i)
		    ((proc)
                     (let ((code (cadr i)))
                       (write-char #\space)
                       (write (code->label code))
		       (newline)
		       (dump-asm (disassemble-instrucs code)
			         (+ margin 5)
			         -1)))
		    ((prim-0) (write-prim 0 (cadr i)))
		    ((prim-1) (write-prim 1 (cadr i)))
		    ((prim-2) (write-prim 2 (cadr i)))
		    ((prim-3) (write-prim 3 (cadr i)))
		    (else
		     (for-each (lambda (a)
				 (write-char #\space)
				 (write a))
			       (cdr i))
		     (newline)))))
	      asm))


  (define (disassemble-instrucs code)
    (let ((L (string-length (code->bytecodes code))))
      (let loop ((i 0) (acc '()))
	(if (<= L i)
	    (reverse acc)
	    (disassemble-instruc i code
				 (lambda (width dis)
				   (loop (+ i width)
					 (cons (list i dis)
					       acc))))))))

  ;; Return (k width parts)
  ;;   where width is the #bytes encoding this instruction + its args
  ;;   and parts is the instruction name and arguments as parsed from the encoding.
  (define (disassemble-instruc pc code k)
    (define (byte-ref offset) 
      (char->integer (string-ref (code->bytecodes code) (+ pc offset))))
    (define (take nbytes . args)
      (let ((iname (vector-ref instruc-names (byte-ref 0))))
        (k (+ nbytes 1) (cons iname args))))
    (let ((specs (vector-ref @instruc-args (byte-ref 0))))
      (cond ((null? specs) (take 0))
            ((not (null? (cdr specs)))
             (%error "Bad instruc specs" specs))
            (else
              (case (car specs)
	        ((d) (take 1 (vector-ref (code->constants code) (byte-ref 1))))
	        ((w) (take 2 (+ (+ pc 3)
			        (+ (* 256 (byte-ref 1)) (byte-ref 2)))))
	        ((b) (take 1 (byte-ref 1)))
                ((locals) (let ((n-locals (byte-ref 1))
                                (lmap (code->locals-map code)))
                            (take 1 (if (pair? lmap) ; (let's be robust to stripping debug info)
                                        (car lmap)
                                        n-locals))))
                ((v) (let ((depth (byte-ref 1))
                           (offset (byte-ref 2)))
                       (take 2
                             (locals-map-ref (code->locals-map code) depth offset)
                             `(at ,depth ,offset))))
	        (else (%error "BUG: bad instruc arg" spec))))))))


;;;;
;;;; Writing cyclic structures
;;;;

;;;;
;;;; Write list structures with cyclic references cut short.
;;;; E.g.:
;;;; > (define a (list 'x))
;;;; > (set-cdr! a a)
;;;; > (cycle-write a)
;;;; #1 = (x . #1) 
;;;;
;;;; I'll fold this into the main system writer sometime...
;;;;

(begin

  (define (cycle-write x . optional-port)
    (let ((out (@optional-arg optional-port (current-output-port)))
	  (table (make-cycle-table)))
      (traverse table x)
      (cw out table x)
      (begin)))

  (define (traverse table obj)
    (let walk ((obj obj))
      (cond ((and (pair? obj)
		  (not (table-visit! table obj)))
	     (walk (car obj))
	     (walk (cdr obj)))
	    ((and (vector? obj)
		  (< 0 (vector-length obj))
		  (not (table-visit! table obj)))
	     (let loop ((i (vector-length obj)))
	       (cond ((< 0 i)
		      (walk (vector-ref obj (- i 1)))
		      (loop (- i 1)))))))))

  ;; The cycle table has two fields:
  ;; an a-list from objects to markers, and a counter.
  ;; A marker is either #t or an integer:
  ;;   #t: we've seen the object only once so far; 
  ;;    N: (positive) we've seen it more than once, but not written it; 
  ;;   -N: (negative) we've seen it and written it.
  ;; The counter assigns a unique N to each object seen more than once.

  (define (make-cycle-table)
    (list '() 0))

  (define (table-visit! table obj)
    (cond ((assq obj (car table))
	   => (lambda (pair)
		(cond ((eq? #t (cdr pair))
		       (set-car! (cdr table)
				 (+ 1 (car (cdr table))))
		       (set-cdr! pair (car (cdr table)))))
		#t))
	  (else
	   (set-car! table
		     (cons (cons obj #t) (car table)))
	   #f)))

  (define (table-ref table obj)
    (cond ((assq obj (car table)) => cdr)
	  (else #f)))

  (define (table-set! table obj value)
    (cond ((assq obj (car table))
	   => (lambda (pair)
		(set-cdr! pair value)))))

  (define (cw out table obj)

    (define (put str)
      (display str out))

    (define (check-table obj write-obj)
      (cond ((table-ref table obj)
	     => (lambda (label)
		  (if (not (number? label))     ; Only one visit
		      (write-obj)
		      (cond ((< 0 label)	; First visit to obj
			     (put "#")
			     (put label)
			     (put "=")
			     (table-set! table obj (- label)) ; Mark first visit
			     (write-obj))
			    (else		; A later visit
			     (put "#")
			     (put (- label)))))))
	    (else
	     (write-obj))))

    (let recur ((obj obj))
      (cond
	((pair? obj)
	 (check-table obj
	   (lambda ()
	     (put "(")
	     (recur (car obj))
	     (let loop ((L (cdr obj)))
	       (let ((label (table-ref table L)))
		 (cond ((null? L) 'ok)
		       ((or (not (pair? L)) 
			    (number? label))
			(put " . ")
			(recur L))
		       (else 
			(put " ")
			(recur (car L))
			(loop (cdr L))))))
	     (put ")"))))
	((vector? obj)
	 (check-table obj
	   (lambda ()
	     (put "#(")
	     (if (< 0 (vector-length obj))
		 (recur (vector-ref obj 0)))
	     (let loop ((i 1))
	       (cond ((< i (vector-length obj))
		      (put " ")
		      (recur (vector-ref obj i))
		      (loop (+ i 1)))))
	     (put ")"))))
	(else (write obj out))))))


;;;;
;;;; The debugger
;;;;

(begin

  ;;; The parts of a closure: lexical environment and code object.
  ;;; The env is a linked list of env frames, where each frame is
  ;;; represented by a vector with the link in slot 0.
  ;;; The code object holds "actual code" for the vm interpreter, plus
  ;;; for the debugger a label (human-readable full name) and locals-map.

  (define (environment? x)
    (vector? x))

  (define (env-empty? env)
    (= 0 (vector-length env)))

  (define (env->enclosing env)
    (vector-ref env 0))

  (define (env->inner-frame env)
    (cdr (vector->list env)))

  (define (code? x)
    (and (vector? x)
	 (= (vector-length x) 6)
	 (eq? (vector-ref x 0) 'code-vector)))

  (define (vector-ref-at i)
    (lambda (vec) (vector-ref vec i)))

  (define code->constants (vector-ref-at 1))
  (define code->bytecodes (vector-ref-at 2))
  (define code->label     (vector-ref-at 3))
  (define code->locals-map (vector-ref-at 4))
  (define code->profile   (vector-ref-at 5))


  ;;; Continuations
  ;;; Implemented as a procedure with magical bytecode, with the interpreter's
  ;;; stack saved as a Scheme vector in the closure's lex-env slot.

  (define continuation? 
    (let ((cont-code (call-with-current-continuation @closure->code)))
      (lambda (obj)
	(and (procedure? obj)
	     (eq? (@closure->code obj) cont-code)))))

  (define (continuation->stack cont)
    (vector-ref (@closure->lex-env cont) 1))


  ;;; Continuation stack frames: contiguous segments of a stack vector (svec),
  ;;; each designated by an index (sptr) pointing just after the segment.

  (define (make-frame svec sptr)
    (list svec sptr))

  (define frame/svec car)
  (define frame/sptr cadr)

  (define (frame/ref offset tag ok?)
    (lambda (frame)
      (let ((x (vector-ref (frame/svec frame) (- (frame/sptr frame) offset))))
	(if (ok? x)
	    x
	    (%error "Bad ref to" tag x)))))

  (define frame->pc      (frame/ref 4 'pc integer?))
  (define frame->code    (frame/ref 3 'code code?))
  (define frame->lex-env (frame/ref 2 'env environment?))
  (define frame->base    (frame/ref 1 'base integer?)) ; value is the index of the start of this segment

  ;; The caller is the next frame to the left of the base; or #f
  ;; for the final frame, a halt_code sentinel of no interest.
  (define (frame->caller frame)
    (let ((caller (make-frame (frame/svec frame) (frame->base frame))))
      (if (= (frame->base caller) 0)
          #f
          caller)))

  ;; Return the local stack elements as a list, with bottom of stack first.
  (define (frame->stack frame)
    (let ((svec (frame/svec frame))
          (base (frame->base frame)))
      (do ((sp (- (frame/sptr frame) 5)
               (- sp 1))
	   (acc '()
                (cons (vector-ref svec sp) acc)))
	  ((< sp base) acc))))

  ;; Return a list of the frame and all its successive callers, with
  ;; the final caller first.
  (define (caller* frame)
    (do ((frame frame (frame->caller frame))
         (ls '() (cons frame ls)))
        ((not frame) ls)))


  ;;; The interactive debugger

  (define (debug)
    (if (continuation? %error-cont)
	(inspect-cont %error-cont)
	(%error "No context to debug")))

  ;; Break into the debugger after printing the args.
  (define (%avast . args)
    (display "\n[Breakpoint!] %avast\n")
    (for-each (lambda (arg) (write arg) (newline))
              args)
    (call-with-current-continuation
     (lambda (cont)
       (inspect-cont cont)
       (if (null? args) unspecified (car args)))))

  (define (inspect-cont cont)
    (let ((outer-frame 
	   (let* ((stack (continuation->stack cont))
		  (stack-top (vector-length stack)))
	     (make-frame stack stack-top))))

      (define (prompt-and-read prompt)
	(display prompt)
	(let ((obj (read)))
          (if (eof-object? obj) 'quit obj)))

      (define (say message)
	(display message)
	(newline))

      (define (print-each ls)
	(for-each (lambda (x) (cycle-write x) (newline))
		  ls))

      ;; lmap is a locals-map, env is a corresponding runtime env
      ;; As usual we're robust to a stripped locals-map.
      (define (show-env lmap env)
	(if (not (env-empty? env))
            (let ((vars (if (pair? lmap) (car lmap) '()))
                  (vals (env->inner-frame env)))
              (if (= (length vars) (length vals))
                  (for-each (lambda (var val)
                              (write var) (display ": ") (cycle-write val) (newline))
                            vars
                            vals)
	          (print-each vals)))))

      (define (help)
	(say "? HELP      - this message")
	(say "Q QUIT      - quit the debugger")
	(say "B BACKTRACE - names of the current procedure and its callers")
	(say "A ASSEMBLY  - show assembly source of the current procedure")
	(say "E ENV       - show the inner frame of the current environment")
	(say "N NEXT      - show the next frame of the current environment")
	(say "S STACK     - show the local value stack")
	(say "U UP        - up to caller")
	(say "D DOWN      - down to callee"))

      (define (go-to-frame frame callees)
          (interact frame callees
                    (code->locals-map (frame->code frame))
		    (frame->lex-env frame)))

      (define (interact frame callees lmap env)

	(define (again)
	  (interact frame callees lmap env))

	(case (prompt-and-read "debug> ")

	  ((? help)
	   (help)
	   (again))

	  ((q quit)
           unspecified)

	  ((u up)
	   (let ((caller (frame->caller frame)))
	     (cond (caller 
		    (go-to-frame caller (cons frame callees)))
		   (else
		    (say "At top.")
		    (again)))))

	  ((d down)
	   (cond ((null? callees)
		  (say "At bottom.")
		  (again))
		 (else
		  (go-to-frame (car callees) (cdr callees)))))

	  ((e env)
	   (show-env (code->locals-map (frame->code frame)) ;TODO ugh code dup
                     (frame->lex-env frame))
	   (go-to-frame frame callees))

	  ((n next)
	   (let ((next (if (env-empty? env)
			   env
			   (env->enclosing env)))
                 (next-lmap (if (null? lmap) '() (cdr lmap))))
	     (cond ((env-empty? next)
		    (say "No more environment frames.")
		    (again))
		   (else
		    (show-env next-lmap next)
		    (interact frame callees next-lmap next)))))

	  ((a assembly)
	   (disassemble (frame->code frame) (frame->pc frame))
	   (again))

	  ((s stack)
	   (print-each (frame->stack frame))
	   (again))

	  ((b backtrace)
	   (print-each (map (lambda (frame)
			      (code->label (frame->code frame)))
			    (caller* frame)))
	   (again))

	  (else 
	   (say "Huh?  Enter HELP for help.")
	   (again))))

      (display "Enter ? for help.\n")
      (interact outer-frame '()
                (code->locals-map (frame->code outer-frame))
                (frame->lex-env outer-frame)))))
