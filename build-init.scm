;;;; Building a new init image as C byte array literal.

(define (another port)
  (display ", " port))

(define (write-tag tag port)
  (display "\nini_" port)
  (display tag port)
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

  (define (read-all filename)           ;; TODO unify with %read-all
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
      ((symbol? obj)      'symbol)
      ((pair? obj)        'cons)
      ((and (integer? obj)
	    (exact? obj)) 'int)
      ((null? obj)        'nil)
      ((%closure? obj)    'closure)
      ((vector? obj)      'vector)
      ((boolean? obj)     (if obj 'true 'false))
      ((string? obj)      'string)
      ((char? obj)        'char)
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
            (write-tag 'ref port)
            (dump-int index)))
      ((pair? obj)
       (recur (cdr obj))
       (recur (car obj))
       (write-tag (tag obj) port)
       (ref-table 'add obj))
      ((%closure? obj)
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
       (write-tag 'symbol port)
       (dump-string (symbol->string obj))
       (ref-table 'add obj)
       (display "/* " port)
       (display (symbol->string obj) port)
       (display " */ " port))
      (else
        (let ((obj-tag (tag obj)))
	  (write-tag obj-tag port)
	  (case obj-tag
	    ((nil true false) 'ok)
	    ((symbol) (dump-string (symbol->string obj)))
	    ((int) (dump-int obj))
	    ((string) (dump-string obj))
	    ((char) (write-byte (char->ascii obj) port))
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


(define (main . filenames)
  (build-system (car filenames) (cdr filenames)))
