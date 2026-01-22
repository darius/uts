;;;; error-tests.scm - Tests for expected error conditions
;;;;
;;;; Usage: (load "r4rs.scm") (load "error-tests.scm")
;;;;
;;;; Note: Error messages still print to output (noisy but functional).
;;;; The errors? function works by temporarily replacing %reset.

;;; Error-catching infrastructure

(define @saved-reset %reset)

(define (errors? thunk)
  "Returns #t if thunk signals an error, #f otherwise."
  (call-with-current-continuation
    (lambda (escape)
      (set! %reset (lambda (x)
                     (set! %reset @saved-reset)
                     (escape #t)))
      (thunk)
      (set! %reset @saved-reset)
      #f)))

;;; Test helper (integrates with r4rs.scm test harness if loaded)

(define (test-error desc thunk)
  "Test that thunk signals an error. desc is a description string."
  (display desc)
  (display "  ==> ")
  (let ((errored (errors? thunk)))
    (if errored
        (begin (display "error (ok)") (newline) #t)
        (begin
          (display "no error BUT EXPECTED error")
          (newline)
          (if (defined? 'record-error)
              (record-error (list 'no-error 'error (list 'test-error desc))))
          #f))))

;;; Helper to check if a symbol is defined
(define (defined? sym)
  (not (errors? (lambda () (%eval sym)))))

;;;
;;; Error condition tests
;;;

(SECTION 'type-errors)

;; car/cdr on non-pairs
(test-error "(car 5)" (lambda () (car 5)))
(test-error "(cdr 5)" (lambda () (cdr 5)))
(test-error "(car 'symbol)" (lambda () (car 'symbol)))
(test-error "(car \"string\")" (lambda () (car "string")))
(test-error "(car #\\a)" (lambda () (car #\a)))

;; Calling a non-procedure
(test-error "(5 1 2)" (lambda () (5 1 2)))
(test-error "('foo)" (lambda () ('foo)))
(test-error "(#t 1)" (lambda () (#t 1)))

;; Arithmetic type errors
(test-error "(+ 1 'a)" (lambda () (+ 1 'a)))
(test-error "(+ 1 \"two\")" (lambda () (+ 1 "two")))
(test-error "(- 'a 1)" (lambda () (- 'a 1)))
(test-error "(* 1 #t)" (lambda () (* 1 #t)))
(test-error "(/ 1 'a)" (lambda () (/ 1 'a)))

;; Comparison type errors
(test-error "(< 1 'a)" (lambda () (< 1 'a)))
(test-error "(= 'a 'b)" (lambda () (= 'a 'b)))

;; String operations on non-strings
(test-error "(string-length 5)" (lambda () (string-length 5)))
(test-error "(string-ref 5 0)" (lambda () (string-ref 5 0)))

;; Vector operations on non-vectors
(test-error "(vector-length 5)" (lambda () (vector-length 5)))
(test-error "(vector-ref 5 0)" (lambda () (vector-ref 5 0)))

;; char->integer on non-char
(test-error "(char->integer 97)" (lambda () (char->integer 97)))
(test-error "(char->integer \"a\")" (lambda () (char->integer "a")))

;; char predicates on non-chars
(test-error "(char-whitespace? 32)" (lambda () (char-whitespace? 32)))
(test-error "(char<? #\\a 98)" (lambda () (char<? #\a 98)))

;; symbol->string on non-symbol
(test-error "(symbol->string \"hello\")" (lambda () (symbol->string "hello")))
(test-error "(symbol->string 42)" (lambda () (symbol->string 42)))

;; string->symbol on non-string
(test-error "(string->symbol 'foo)" (lambda () (string->symbol 'foo)))

;; Port type mismatches
(test-error "(read-char (current-output-port))" (lambda () (read-char (current-output-port))))
(test-error "(write-char #\\a (current-input-port))" (lambda () (write-char #\a (current-input-port))))


(SECTION 'range-errors)

;; Vector index out of range
(test-error "(vector-ref #(a b) 5)" (lambda () (vector-ref '#(a b) 5)))
(test-error "(vector-ref #(a b) -1)" (lambda () (vector-ref '#(a b) -1)))
(test-error "(vector-ref #() 0)" (lambda () (vector-ref '#() 0)))

;; String index out of range
(test-error "(string-ref \"hi\" 10)" (lambda () (string-ref "hi" 10)))
(test-error "(string-ref \"hi\" -1)" (lambda () (string-ref "hi" -1)))
(test-error "(string-ref \"\" 0)" (lambda () (string-ref "" 0)))

;; list-ref out of range
(test-error "(list-ref '(a b) 5)" (lambda () (list-ref '(a b) 5)))

;; integer->char out of range
(test-error "(integer->char 300)" (lambda () (integer->char 300)))
(test-error "(integer->char -1)" (lambda () (integer->char -1)))

;; make-vector/make-string with bad size
(test-error "(make-vector -1 'x)" (lambda () (make-vector -1 'x)))
(test-error "(make-string -1 #\\x)" (lambda () (make-string -1 #\x)))

;; substring range errors
(test-error "(substring \"hi\" 0 10)" (lambda () (substring "hi" 0 10)))
(test-error "(substring \"hi\" 5 6)" (lambda () (substring "hi" 5 6)))
(test-error "(substring \"hi\" 2 1)" (lambda () (substring "hi" 2 1)))


(SECTION 'arithmetic-errors)

;; Division by zero
(test-error "(/ 1 0)" (lambda () (/ 1 0)))
(test-error "(/ 1.0 0)" (lambda () (/ 1.0 0)))
(test-error "(quotient 5 0)" (lambda () (quotient 5 0)))
(test-error "(remainder 5 0)" (lambda () (remainder 5 0)))
(test-error "(modulo 5 0)" (lambda () (modulo 5 0)))

;; Bad radix for number conversion
(test-error "(number->string 42 99)" (lambda () (number->string 42 99)))
(test-error "(number->string 42 1)" (lambda () (number->string 42 1)))


(SECTION 'arity-errors)

;; Too few arguments (if detectable at runtime)
;; Note: Some of these may be caught at compile time instead

;; Too many arguments to fixed-arity procedures
(test-error "(car 1 2)" (lambda () (car 1 2)))
(test-error "(cons 1 2 3)" (lambda () (cons 1 2 3)))

;; apply with non-list as final argument
(test-error "(apply + 1)" (lambda () (apply + 1)))
(test-error "(apply + 1 2 3)" (lambda () (apply + 1 2 3)))
(test-error "(apply + '(1 . 2))" (lambda () (apply + '(1 . 2))))


(SECTION 'other-errors)

;; Unbound variable
(test-error "unbound variable" (lambda () (eval 'this-variable-is-not-defined)))

;; set-car!/set-cdr! on non-pairs
(test-error "(set-car! 5 'a)" (lambda () (set-car! 5 'a)))
(test-error "(set-cdr! 5 'a)" (lambda () (set-cdr! 5 'a)))

;; vector-set! errors
(test-error "(vector-set! 5 0 'a)" (lambda () (vector-set! 5 0 'a)))
(test-error "(vector-set! #(a) 5 'b)" (lambda () (vector-set! '#(a) 5 'b)))

;; string-set! errors
(test-error "(string-set! 5 0 #\\a)" (lambda () (string-set! 5 0 #\a)))
(test-error "(string-set! \"x\" 5 #\\a)" (lambda () (string-set! "x" 5 #\a)))


(SECTION 'improper-list-errors)

;; Operations that require proper lists
(test-error "(length '(a . b))" (lambda () (length '(a . b))))
(test-error "(reverse '(a . b))" (lambda () (reverse '(a . b))))
(test-error "(append '(a . b) '(c))" (lambda () (append '(a . b) '(c))))
(test-error "(list->vector '(a . b))" (lambda () (list->vector '(a . b))))

;; assq/assv with improper alist
(test-error "(assq 'x '((a . 1) . bad))" (lambda () (assq 'x '((a . 1) . bad))))
(test-error "(assv 'x '((a . 1) . bad))" (lambda () (assv 'x '((a . 1) . bad))))

;; memq/memv with improper list
(test-error "(memq 'x '(a . b))" (lambda () (memq 'x '(a . b))))
(test-error "(memv 'x '(a . b))" (lambda () (memv 'x '(a . b))))


(SECTION 'port-errors)

;; Reading from closed port
(test-error "read-char from closed port"
  (lambda ()
    (let ((p (open-input-file "test.scm")))
      (close-input-port p)
      (read-char p))))

;; Writing to closed port
(test-error "write-char to closed port"
  (lambda ()
    (let ((p (open-output-file "/tmp/uts-error-test.txt")))
      (close-output-port p)
      (write-char #\a p))))


;;; Report results (if r4rs.scm harness is loaded)

(if (defined? 'report-errs)
    (report-errs)
    (display "Error tests completed.\n"))
