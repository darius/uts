;;; test.scm - Test suite for uts
;;;
;;; Tests adapted from SISC r5rs_pitfall.scm (public domain)
;;; Plus additional tests for uts-specific features

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (should-be test-id expected expression)
  (if (equal? expression expected)
      (begin
        (set! *tests-passed* (+ *tests-passed* 1))
        (display "."))
      (begin
        (set! *tests-failed* (+ *tests-failed* 1))
        (newline)
        (display "FAIL: ")
        (display test-id)
        (display ", expected '")
        (write expected)
        (display "', got '")
        (write expression)
        (display "'")
        (newline))))

(define (run-pitfall-tests)
  ;; Section 4: No identifiers are reserved
  ;; (Brian M. Moore)
  ;; Note: 4.2 and 4.3 test R5RS edge cases that differ in R4RS
  (should-be "4.1" '(x)
    ((lambda lambda lambda) 'x))

  ;; Section 5: #f/() distinctness
  ;; (Scott Miller)
  (should-be "5.1" #f (eq? #f '()))
  (should-be "5.2" #f (eqv? #f '()))
  (should-be "5.3" #f (equal? #f '()))

  ;; Section 6: string->symbol case sensitivity
  ;; (Jens Axel Sogaard)
  (should-be "6.1" #f
    (eq? (string->symbol "f") (string->symbol "F")))

  ;; Section 7: First class continuations
  ;; (Scott Miller)
  (let ()
    (define r #f)
    (define a #f)
    (define b #f)
    (define c #f)
    (define i 0)
    (should-be "7.1" 28
      (let ()
        (set! r (+ 1 (+ 2 (+ 3 (call-with-current-continuation
                                 (lambda (k) (set! a k) 4))))
                   (+ 5 (+ 6 (call-with-current-continuation
                                 (lambda (k) (set! b k) 7))))))
        (if (not c)
            (set! c a))
        (set! i (+ i 1))
        (case i
          ((1) (a 5))
          ((2) (b 8))
          ((3) (a 6))
          ((4) (c 4)))
        r)))

  ;; Same test in reverse order
  (let ()
    (define r #f)
    (define a #f)
    (define b #f)
    (define c #f)
    (define i 0)
    (should-be "7.2" 28
      (let ()
        (set! r (+ 1 (+ 2 (+ 3 (call-with-current-continuation
                                 (lambda (k) (set! a k) 4))))
                   (+ 5 (+ 6 (call-with-current-continuation
                                 (lambda (k) (set! b k) 7))))))
        (if (not c)
            (set! c a))
        (set! i (+ i 1))
        (case i
          ((1) (b 8))
          ((2) (a 5))
          ((3) (b 7))
          ((4) (c 4)))
        r)))

  ;; Yin-yang puzzle variant (Scott G. Miller)
  (should-be "7.4" '(10 9 8 7 6 5 4 3 2 1 0)
    (let ((x '())
          (y 0))
      (call-with-current-continuation
       (lambda (escape)
         (let* ((yin ((lambda (foo)
                        (set! x (cons y x))
                        (if (= y 10)
                            (escape x)
                            (begin
                              (set! y 0)
                              foo)))
                      (call-with-current-continuation (lambda (bar) bar))))
                (yang ((lambda (foo)
                         (set! y (+ y 1))
                         foo)
                       (call-with-current-continuation (lambda (baz) baz)))))
           (yin yang))))))

  ;; Section 8: Miscellaneous
  ;; (Al Petrofsky)
  (should-be "8.1" -1
    (let - ((n (- 1))) n))

  (should-be "8.2" '(1 2 3 4 1 2 3 4 5)
    (let ((ls (list 1 2 3 4)))
      (append ls ls '(5))))

)

(define (run-uts-tests)
  ;; Fixnum boundaries (62-bit)
  ;; Build max via multiplication to avoid expt flonum overflow
  (should-be "fix.1" #t (integer? 2305843009213693951))  ; FIXNUM_MAX literal
  (should-be "fix.2" #t (integer? -2305843009213693952)) ; FIXNUM_MIN literal
  (should-be "fix.3" 2305843009213693951 (+ 2305843009213693950 1))
  (should-be "fix.4" -2305843009213693952 (- -2305843009213693951 1))

  ;; Overflow to flonum
  (should-be "fix.5" #t (inexact? (+ 2305843009213693951 1)))
  (should-be "fix.6" #t (inexact? (* 2305843009213693951 2)))

  ;; Arithmetic - factorial(19) as a large integer test
  (should-be "arith.1" 121645100408832000
    (letrec ((fac (lambda (n) (if (<= n 1) 1 (* n (fac (- n 1)))))))
      (fac 19)))

  (should-be "arith.3" 0 (quotient 5 10))
  (should-be "arith.4" 5 (remainder 5 10))
  (should-be "arith.5" -1 (quotient -5 3))
  (should-be "arith.6" -2 (remainder -5 3))

  ;; Basic data types
  (should-be "type.1" #t (pair? '(1 . 2)))
  (should-be "type.2" #t (null? '()))
  (should-be "type.3" #t (symbol? 'foo))
  (should-be "type.4" #t (string? "hello"))
  (should-be "type.5" #t (vector? '#(1 2 3)))
  (should-be "type.6" #t (procedure? car))

  ;; List operations
  (should-be "list.1" '(1 2 3) (list 1 2 3))
  (should-be "list.2" 3 (length '(a b c)))
  (should-be "list.3" '(3 2 1) (reverse '(1 2 3)))
  (should-be "list.4" 'c (list-ref '(a b c d) 2))
  (should-be "list.5" '(a b c d e) (append '(a b) '(c d e)))
  (should-be "list.6" '(1 4 9) (map (lambda (x) (* x x)) '(1 2 3)))

  ;; String operations
  (should-be "str.1" 5 (string-length "hello"))
  (should-be "str.2" #\e (string-ref "hello" 1))
  (should-be "str.3" "hello" (string-append "hel" "lo"))
  (should-be "str.4" #t (string=? "abc" "abc"))
  (should-be "str.5" #t (string<? "abc" "abd"))

  ;; Vector operations
  (should-be "vec.1" 3 (vector-length '#(a b c)))
  (should-be "vec.2" 'b (vector-ref '#(a b c) 1))
  (should-be "vec.3" '#(0 0 0) (make-vector 3 0))

  ;; Bitwise operations
  (should-be "bit.1" 7 (bitwise-and 15 7))
  (should-be "bit.2" 11 (bitwise-ior 8 3))
  (should-be "bit.3" 8 (bitwise-xor 15 7))
  (should-be "bit.4" -1 (bitwise-not 0))
  (should-be "bit.5" 16 (arithmetic-shift 1 4))
  (should-be "bit.6" 4 (arithmetic-shift 16 -2))

)

(define (run-prim-tests)
  ;; === Type predicates ===
  (should-be "pred.1" #t (boolean? #t))
  (should-be "pred.2" #t (boolean? #f))
  (should-be "pred.3" #f (boolean? 0))
  (should-be "pred.4" #f (boolean? '()))
  (should-be "pred.5" #t (char? #\a))
  (should-be "pred.6" #f (char? 97))
  (should-be "pred.7" #t (number? 42))
  (should-be "pred.8" #t (number? 3.14))
  (should-be "pred.9" #f (number? "42"))
  (should-be "pred.10" #t (integer? 42))
  (should-be "pred.11" #t (integer? -17))
  (should-be "pred.12" #f (integer? 3.5))
  (should-be "pred.13" #t (integer? 3.0))  ; inexact integer
  (should-be "pred.14" #t (input-port? (current-input-port)))
  (should-be "pred.15" #f (input-port? (current-output-port)))
  (should-be "pred.16" #t (output-port? (current-output-port)))
  (should-be "pred.17" #f (output-port? (current-input-port)))

  ;; === Numeric rounding ===
  (should-be "round.1" 3.0 (floor 3.7))
  (should-be "round.2" -4.0 (floor -3.2))
  (should-be "round.3" 4.0 (ceiling 3.2))
  (should-be "round.4" -3.0 (ceiling -3.7))
  (should-be "round.5" 3.0 (truncate 3.7))
  (should-be "round.6" -3.0 (truncate -3.7))
  (should-be "round.7" 4.0 (round 3.5))
  (should-be "round.8" 4.0 (round 4.5))  ; round to even
  (should-be "round.9" 3 (floor 3))      ; identity on integers
  (should-be "round.10" -3 (ceiling -3))

  ;; === Exactness ===
  (should-be "exact.1" 3.0 (exact->inexact 3))
  (should-be "exact.2" #t (inexact? (exact->inexact 3)))
  (should-be "exact.3" 3 (inexact->exact 3.0))
  (should-be "exact.4" #t (exact? (inexact->exact 3.0)))
  (should-be "exact.5" 3 (inexact->exact 3))  ; identity on exact

  ;; === Transcendental functions ===
  (should-be "trans.1" 2.0 (sqrt 4))
  (should-be "trans.2" 3.0 (sqrt 9.0))
  (should-be "trans.3" 1.0 (exp 0))
  (should-be "trans.4" 0.0 (log 1))
  (should-be "trans.5" 0.0 (sin 0))
  (should-be "trans.6" 1.0 (cos 0))
  (should-be "trans.7" 0.0 (tan 0))
  (should-be "trans.8" 0.0 (asin 0))
  (should-be "trans.9" 0.0 (acos 1))
  (should-be "trans.10" 0.0 (atan 0))

  ;; === Expt and atan2 ===
  (should-be "expt.1" 8 (expt 2 3))
  (should-be "expt.2" 1 (expt 5 0))
  (should-be "expt.3" 0.5 (expt 2 -1))
  (should-be "expt.4" 1024 (expt 2 10))
  ;; atan with two arguments (atan2)
  (should-be "atan2.1" 0.0 (atan 0 1))

  ;; === Division and modulo ===
  (should-be "div.1" 2.5 (/ 5 2))
  (should-be "div.2" 2 (/ 6 3))
  (should-be "div.3" 0.5 (/ 2))
  (should-be "mod.1" 1 (modulo 7 3))
  (should-be "mod.2" 2 (modulo -7 3))   ; sign follows divisor
  (should-be "mod.3" -2 (modulo 7 -3))
  (should-be "mod.4" -1 (modulo -7 -3))

  ;; === Character operations ===
  (should-be "char.1" 97 (char->integer #\a))
  (should-be "char.2" #\a (integer->char 97))
  (should-be "char.3" #t (char-whitespace? #\space))
  (should-be "char.4" #t (char-whitespace? (integer->char 10)))  ; newline
  (should-be "char.5" #t (char-whitespace? (integer->char 9)))   ; tab
  (should-be "char.6" #f (char-whitespace? #\a))

  ;; === Symbol/string conversion ===
  (should-be "sym.1" "hello" (symbol->string 'hello))
  (should-be "sym.2" 'hello (string->symbol "hello"))
  (should-be "sym.3" #t (eq? (string->symbol "foo") 'foo))

  ;; === Number/string conversion with radix ===
  (should-be "numstr.1" "42" (number->string 42))
  (should-be "numstr.2" "101010" (number->string 42 2))
  (should-be "numstr.3" "52" (number->string 42 8))
  (should-be "numstr.4" "2A" (number->string 42 16))
  (should-be "numstr.5" "-42" (number->string -42))
  (should-be "numstr.6" 42 (string->number "42"))
  (should-be "numstr.7" #f (string->number "not-a-number"))
  (should-be "numstr.8" 42 (string->number "101010" 2))
  (should-be "numstr.9" 42 (string->number "52" 8))
  (should-be "numstr.10" 42 (string->number "2a" 16))

  ;; === File port operations ===
  ;; Test open-output-file and close-output-port
  (should-be "port.1" #t
    (let ((p (open-output-file "/tmp/uts-test-output.txt")))
      (display "test" p)
      (close-output-port p)
      #t))
  (should-be "port.2" #t
    (let ((p (open-input-file "/tmp/uts-test-output.txt")))
      (let ((result (eq? (read-char p) #\t)))
        (close-input-port p)
        result)))
)

(define (run-tests)
  (set! *tests-passed* 0)
  (set! *tests-failed* 0)
  (display "Running tests...")
  (newline)
  (run-pitfall-tests)
  (run-uts-tests)
  (run-prim-tests)
  ;; Summary
  (newline)
  (newline)
  (display "Tests passed: ")
  (display *tests-passed*)
  (newline)
  (display "Tests failed: ")
  (display *tests-failed*)
  (newline)
  (if (= *tests-failed* 0)
      (display "All tests passed!")
      (display "SOME TESTS FAILED"))
  (newline))

(run-tests)
