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

(define (run-tests)
  (set! *tests-passed* 0)
  (set! *tests-failed* 0)

  (display "Running tests...")
  (newline)

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
  ;; (Jens Axel Søgaard)
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

  ;; === Additional uts-specific tests ===

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
