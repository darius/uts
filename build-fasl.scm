;;;; Building a new uts.fasl file.
;;;; fasl is a binary RPN format.

;; It'd be easy to compile-file from an arbitrary Scheme file to a
;; fasl file, and overload (load filename) to load from either Scheme
;; source or fasl; I dropped this feature because I wasn't using it.
;; (compile-file was like build-system minus the all-primitive-defs.)

(begin

  ;;; Compile the primitives + the Scheme source to a fasl file.
  ;; N.B. *open-code-primitives?* must be true for the primitives to compile correctly
  (define (build-system scheme-filename fasl-filename)
    (compile-to-fasl (cons (all-primitive-defs) (read-all scheme-filename))
                     fasl-filename))

  (define (read-all filename)
    (call-with-input-file filename
      (lambda (in)
        (let reading ()
          (let ((o (read in)))
	    (if (eof-object? o)
                '()
                (cons o (reading))))))))

  (define (compile-to-fasl forms filename)
    (%write-fasl (list->vector (map parse-form forms))
                 filename))

  ;;; Generate code defining fixed-arity primitives.

  (define (variable-arity? prim-name)
    (memq prim-name variable-arity-prim-list))

  (define (prim-def-source-code prim-names args)
    `(begin
       ,@(@reduce (lambda (prim-name defs)
		    (if (variable-arity? prim-name)
			defs
			(cons `(define ,prim-name
				 (lambda ,args
				   (,prim-name ,@args)))
			      defs)))
		  '()
		  prim-names)))

  (define closure-for-apply
    (@make-closure '#()
		   (codify (cons @%apply '())
			   '#()
			   'apply
                           lexical-env/empty)))

  (define closure-for-call/cc
    (@make-closure '#()
		    (codify (lap/params 1
			      (cons @%get-cc
				(lap/var (make-lexical-address 0 0)
				  (lap/invoke '()))))
			    '#()
			    'call-with-current-continuation
                            (lexical-env/extend lexical-env/empty '(callee)))))

  (define (all-primitive-defs)
    `(begin
       ,(prim-def-source-code (map car prim-0-list) '())
       ,(prim-def-source-code (map car prim-1-list) '(x))
       ,(prim-def-source-code (map car prim-2-list) '(x y))
       ,(prim-def-source-code (map car prim-3-list) '(x y z))
       (define apply ',closure-for-apply)
       (define call-with-current-continuation ',closure-for-call/cc)))
  )



;;; The fasl format.

(define (%write-fasl obj filename)
  (call-with-output-file filename
    (lambda (out)
      (%write-fasl-header out)
      (let ((ref-table (make-eq-table)))
        (%write-fasl-obj obj ref-table out)))))

(define major-version 4)
(define minor-version 0)

(define (%write-fasl-header port)

  (define (write-byte byte)
    (write-char (integer->char byte) port))

  (define (write-unsigned unsigned)
    (write-byte (quotient unsigned 256))
    (write-byte (remainder unsigned 256)))

  ;; Magic number.
  (write-unsigned #xFADD)
  (write-unsigned #xF00D)

  ;; Version number.
  (write-byte major-version)
  (write-byte minor-version))

(define (%write-fasl-obj obj ref-table port)

  (define (tag obj)
    (cond
      ((symbol? obj)      #\Y)
      ((pair? obj)        #\P)
      ((and (integer? obj)
	    (exact? obj)) #\I)
      ((real? obj)        #\R)
      ((null? obj)        #\U)
      ((code? obj)        #\O)
      ((procedure? obj)   #\L)
      ((vector? obj)      #\V)
      ((boolean? obj)     #\B)
      ((string? obj)      #\S)
      ((char? obj)        #\C)
      (else (%error "Undumpable" obj))))

  (define (dump-int n)
    ;; Signed integer via zigzag + 7-bit variable-length encoding
    ;; zigzag: 0->0, -1->1, 1->2, -2->3, 2->4, ...
    (let loop ((n (if (< n 0) (- (* -2 n) 1) (* 2 n))))
      (if (< n 128)
          (write-char (integer->char n) port)
          (begin
            (write-char (integer->char (+ 128 (remainder n 128))) port)
            (loop (quotient n 128))))))

  (define (dump-string str)
    (dump-int (string-length str))
    (display str port))

  (let recur ((obj obj))
    (cond
      ((and (or (pair? obj) (symbol? obj))
            (ref-table 'get obj))
       => (lambda (index)
            (write-char #\= port)
            (dump-int index)))
      ((pair? obj)
       (recur (cdr obj))
       (recur (car obj))
       (write-char (tag obj) port)
       (ref-table 'add obj))
      ((code? obj)
       (recur (code->locals-map obj))
       (recur (code->label obj))
       (recur (code->bytecodes obj))
       (recur (code->constants obj))
       (write-char (tag obj) port))
      ((procedure? obj)
       (recur (@closure->code obj))
       (recur (@closure->lex-env obj))
       (write-char (tag obj) port))
      ((vector? obj)
       (let loop ((i (- (vector-length obj) 1)))
	 (cond ((<= 0 i)
		(recur (vector-ref obj i))
		(loop (- i 1)))))
       (write-char (tag obj) port)
       (dump-int (vector-length obj)))
      ((symbol? obj)
       (write-char #\Y port)
       (dump-string (symbol->string obj))
       (ref-table 'add obj))
      (else
        (let ((obj-tag (tag obj)))
	  (write-char obj-tag port)
	  (case obj-tag
	    ((#\Y) (dump-string (symbol->string obj))) ;YYY redundant
	    ((#\U) 'ignore)
	    ((#\I) (dump-int obj))
	    ((#\B) (write-char (if obj #\t #\f) port))
	    ((#\S) (dump-string obj))
	    ((#\C) (write-char obj port))
	    ((#\R) (dump-string (number->string obj)))
	    (else (%error "undumpable" obj))))))))

(define (make-eq-table)
  (let ((table '()))
    (lambda (msg arg)
      (case msg
        ((get)
         (cond ((memq arg table) => length-1)
               (else #f)))
        ((add)
         (set! table (cons arg table))
         #f)
        ((size)
         (length table))
        (else (%error "huh?" msg))))))

(define (length-1 ls)
  (- (length ls) 1))

;; If we're being loaded from the command line with two filename arguments, build the system.
(if (= (length %arguments-to-scheme) 3)
    (apply build-system (cdr %arguments-to-scheme)))
