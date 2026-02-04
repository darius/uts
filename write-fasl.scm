;;;;
;;;; Writing fasl files.
;;;; 

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


(define (%write-fasl obj port)
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
      ((pair? obj)
       (recur (cdr obj))
       (recur (car obj))
       (write-char (tag obj) port))
      ((code? obj)
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
      (else
        (let ((obj-tag (tag obj)))
	  (write-char obj-tag port)
	  (case obj-tag
	    ((#\Y) (dump-string (symbol->string obj)))
	    ((#\U) 'ignore)
	    ((#\I) (dump-int obj))
	    ((#\B) (write-char (if obj #\t #\f) port))
	    ((#\S) (dump-string obj))
	    ((#\C) (write-char obj port))
	    ((#\R) (dump-string (number->string obj)))
	    (else (%error "undumpable" obj)))))))
  (write-char #\Z port))
