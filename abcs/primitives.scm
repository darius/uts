;;;; 
;;;; The runtime library: basic primitives
;;;;

(begin
  (define complex? number?)
  (define real? number?)
  (define rational? real?)
  (define call/cc call-with-current-continuation))

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

  (define (%extend-transitive-rel rel?)
    (lambda (x y . rest)
      (and (rel? x y)
	   (if (null? rest) ; first test moved out of loop 'cause of dumb compiler
	       #t
	       (let loop ((y y) (rest rest))
		 (and (rel? y (car rest))
		      (if (null? (cdr rest))
			  #t
			  (loop (car rest) (cdr rest)))))))))

  (define <= (%extend-transitive-rel (lambda (x y) (<= x y))))
  (define <  (%extend-transitive-rel (lambda (x y) (< x y))))
  (define =  (%extend-transitive-rel (lambda (x y) (= x y))))

  (define (%0-or-1 proc default)
    (lambda opt:arg
      (proc
       (cond ((null? opt:arg) default)
	     ((null? (cdr opt:arg)) (car opt:arg))
	     (else (%error "Expected 0 or 1 args to" proc opt:arg))))))

  (define peek-char (%0-or-1 (lambda (x) (peek-char x)) (current-input-port)))
  (define read-char (%0-or-1 (lambda (x) (read-char x)) (current-input-port)))

  ; FIXME: er, default should be a thunk so that we get the -current- output
  ; port, below; doesn't matter right now because that never changes.
  (define (%1-or-2 proc default)
    (lambda (arg . opt:arg)
      (proc 
       arg
       (cond ((null? opt:arg) default)
	     ((null? (cdr opt:arg)) (car opt:arg))
	     (else (%error "Expected 1 or 2 args to" 
			   proc (cons arg opt:arg)))))))

  (define make-vector
    (%1-or-2 (lambda (x y) (make-vector x y)) #f))

  (define make-string 
    (%1-or-2 (lambda (x y) (make-string x y)) #\space))

  (define number->string
    (%1-or-2 (lambda (x y) (number->string x y)) 10))

  (define string->number
    (%1-or-2 (lambda (x y) (string->number x y)) 10))

  (define write-char
    (%1-or-2 (lambda (x y) (write-char x y)) (current-output-port)))

  (define write
    (%1-or-2 (lambda (x y) (write x y)) (current-output-port)))

  (define display
    (%1-or-2 (lambda (x y) (display x y)) (current-output-port)))

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

  (define (%optional-arg arg-list default-value)
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

  (define >  (%extend-transitive-rel (lambda (x y) (< y x))))
  (define >= (%extend-transitive-rel (lambda (x y) (<= y x))))

  (define (min n . L)
    (do ((n n (if (<= n (car L))
		  (%inexact-contagion n (car L))
		  (%inexact-contagion (car L) n)))
	 (L L (cdr L)))
	((null? L) n)))

  (define (max n . L)
    (do ((n n (if (< (car L) n) 
		  (%inexact-contagion n (car L))
		  (%inexact-contagion (car L) n)))
	 (L L (cdr L)))
	((null? L) n)))

  (define (%inexact-contagion x y)
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

  (define (%char-ci-tester test?)
    (lambda (c1 c2) (test? (char-upcase c1) (char-upcase c2))))

  (define char-ci<?  (%char-ci-tester (lambda (c1 c2) (char<? c1 c2))))
  (define char-ci<=? (%char-ci-tester (lambda (c1 c2) (char<=? c1 c2))))
  (define char-ci=?  (%char-ci-tester (lambda (c1 c2) (char=? c1 c2))))
  (define char-ci>=? (%char-ci-tester char>=?))
  (define char-ci>?  (%char-ci-tester char>?))

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

  (define (%string-compare <? =? length-compare?)
    (lambda (s1 s2)
      (let ((limit (min (string-length s1) (string-length s2))))
	(let loop ((i 0))
	  (cond
	    ((= i limit)
	     (length-compare? (string-length s1) (string-length s2)))
	    ((=? (string-ref s1 i) (string-ref s2 i))
	     (loop (+ i 1)))
	    (else (<? (string-ref s1 i) (string-ref s2 i))))))))

  (define string<?  (%string-compare char<? char=? (lambda (x y) (< x y))))
  (define string<=? (%string-compare char<? char=? (lambda (x y) (<= x y))))

  (define (string>? s1 s2)  (string<? s2 s1))
  (define (string>=? s1 s2) (string<=? s2 s1))

  (define string-ci<? 
    (%string-compare char-ci<? char-ci=? (lambda (x y) (< x y))))

  (define string-ci<=? 
    (%string-compare char-ci<? char-ci=? (lambda (x y) (<= x y))))

  (define string-ci=? 
    (%string-compare (lambda (x y) #f) char-ci=? (lambda (x y) (= x y))))

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
		(%optional-arg opt:port (current-output-port))))

  (define (call-with-input-file file proc)
    (let ((port (open-input-file file)))
      (let ((result (proc port)))
	(close-input-port port)
	result)))

  (define (call-with-output-file file proc)
    (let ((port (open-output-file file)))
      (let ((result (proc port)))
	(close-output-port port)
	result))))
