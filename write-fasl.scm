;;;;
;;;; Writing fasl files.
;;;; This stuff is off in a separate file for two reasons:
;;;; - you don't normally need it
;;;; - some of the integer constants in the source are themselves too
;;;;   big for the fasl format 
;;;; 

(define interpreter-path "/home/me/src/scm/uts/uts")

(define major-version 4)
(define minor-version 0)

(define (@write-fasl-header port)

  (define (write-byte byte)
    (write-char (integer->char byte) port))

  (define (write-unsigned unsigned)
    (write-byte (quotient unsigned 256))
    (write-byte (remainder unsigned 256)))

  ;; Unix #! line.
  (display "#!" port)
  (display interpreter-path port)
  (newline port)

  ;; Magic number.
  (write-unsigned #xFADD)
  (write-unsigned #xF00D)

  ;; Version number.
  (write-byte major-version)
  (write-byte minor-version))


(define (@write-fasl obj port)
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
      (else (@error "Undumpable" obj))))
  (define (dump-integer n)
    (if (and (integer? n) 
	     (<= -32768 n) 
	     (<= n 32767))
	(let ((u (+ n 32768)))
	  (write-char (integer->char (quotient u 256)) port)
	  (write-char (integer->char (remainder u 256)) port))
	(@error "Undumpable" n)))
  (define (dump-string str)
    (dump-integer (string-length str))
    (display str port))

  (let recur ((obj obj))
    (cond
      ((pair? obj)
       (recur (cdr obj))
       (recur (car obj))
       (write-char (tag obj) port))
      ((code? obj)
       (recur (code->owner obj))
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
       (dump-integer (vector-length obj)))
      (else
        (let ((obj-tag (tag obj)))
	  (write-char obj-tag port)
	  (case obj-tag
	    ((#\Y) (dump-string (symbol->string obj)))
	    ((#\U) 'ignore)
	    ((#\I) (dump-integer obj))
	    ((#\B) (write-char (if obj #\t #\f) port))
	    ((#\S) (dump-string obj))
	    ((#\C) (write-char obj port))
	    ((#\R) (dump-string (number->string obj)))
	    (else (@error "undumpable" obj)))))))
  (write-char #\Z port))

