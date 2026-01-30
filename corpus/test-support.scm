;;; Shared test support for corpus tests

(define *test-failures* 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (begin (display "  PASS: ") (display name) (newline))
      (begin (display "  FAIL: ") (display name)
             (display " expected ") (write expected)
             (display " got ") (write actual) (newline)
             (set! *test-failures* (+ *test-failures* 1)))))

(define (test-section name)
  (display "--- ") (display name) (display " ---") (newline))

(define (test-summary)
  (if (= *test-failures* 0)
      (begin (display "All tests passed") (newline))
      (begin (display "FAILURES: ") (display *test-failures*) (newline)
             (%error "Tests failed"))))
