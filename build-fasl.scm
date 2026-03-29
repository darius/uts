;;;; Building a new init image as C byte array literal.

(define char->ascii char->integer)  ;; NB not portable Scheme

(define nwritten 0)

(define (another port)
  (set! nwritten (+ nwritten 1))
  (write-char #\, port)
  (write-char (if (= 0 (modulo nwritten 16)) #\newline #\space)
              port))

(define (write-tag char port)
  (write-char #\' port)
  (write-char char port)
  (write-char #\' port)
  (another port))

(define (write-byte byte port)
  (write byte port)
  (another port))



(begin

  ;;; Compile the primitives + the Scheme source to a fasl file.
  ;; N.B. *open-code-primitives?* must be true for the primitives to compile correctly
  ;; TODO avoid that requirement by using %primitive form instead
  (define (build-system fasl-filename sources)
    (compile-to-fasl (cons (all-primitive-defs)
                           (apply append (map read-all sources)))
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
       ,@(%reduce (lambda (prim-name defs)
		    (if (variable-arity? prim-name)
			defs
			(cons `(define ,prim-name
				 (lambda ,args
				   (,prim-name ,@args)))
			      defs)))
		  '()
		  prim-names)))

  (define closure-for-apply
    (%make-closure '#()
		   (codify (cons %bop-apply '())
			   '#()
			   'apply
                           lexical-env/empty)))

  (define closure-for-call/cc
    (%make-closure '#()
		    (codify (lap/params 1
			      (cons %bop-get-cc
				(lap/var (make-lexical-address 0 0)
				  (lap/invoke '()))))
			    '#()
			    'call-with-current-continuation
                            (lexical-env/extend lexical-env/empty '(receiver)))))

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
      (let ((ref-table (make-eq-table)))
        (%write-fasl-obj obj ref-table out)))))

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
          (write-byte n port)
          (begin
            (write-byte (+ 128 (remainder n 128)) port)
            (loop (quotient n 128))))))

  (define (dump-string str)
    (dump-int (string-length str))
    (for-each (lambda (ch) (write-byte (char->ascii ch) port))
              (string->list str)))

  (let recur ((obj obj))
    (cond
      ((and (or (pair? obj) (symbol? obj))
            (ref-table 'get obj))
       => (lambda (index)
            (write-tag #\= port)
            (dump-int index)))
      ((pair? obj)
       (recur (cdr obj))
       (recur (car obj))
       (write-tag (tag obj) port)
       (ref-table 'add obj))
      ((code? obj)
       (recur (code->locals-map obj))
       (recur (code->label obj))
       (recur (code->bytecodes obj))
       (recur (code->constants obj))
       (write-tag (tag obj) port))
      ((procedure? obj)
       (recur (%closure->code obj))
       (recur (%closure->lex-env obj))
       (write-tag (tag obj) port))
      ((vector? obj)
       (let loop ((i (- (vector-length obj) 1)))
	 (cond ((<= 0 i)
		(recur (vector-ref obj i))
		(loop (- i 1)))))
       (write-tag (tag obj) port)
       (dump-int (vector-length obj)))
      ((symbol? obj)
       (write-tag #\Y port)
       (dump-string (symbol->string obj))
       (ref-table 'add obj))
      (else
        (let ((obj-tag (tag obj)))
	  (write-tag obj-tag port)
	  (case obj-tag
	    ((#\Y) (dump-string (symbol->string obj))) ;YYY redundant
	    ((#\U) 'ignore)
	    ((#\I) (dump-int obj))
	    ((#\B) (write-tag (if obj #\t #\f) port))
	    ((#\S) (dump-string obj))
	    ((#\C) (write-byte (char->ascii obj) port))
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


;; If we're being loaded from the command line with >=2 filename arguments, build the system.
(if (<= 3 (length %arguments-to-scheme))
    (let ((filenames (cdr %arguments-to-scheme)))
      (build-system (car filenames) (cdr filenames))))
