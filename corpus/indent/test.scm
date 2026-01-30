;;; Tests for indent

(load "../test-support.scm")
(load "loadme.scm")

(test-section "Indent parser")

;; Helper to parse a file and convert to s-expressions
(define (parse-file filename)
  (map post-read (read-file filename)))

;; Test existing examples
(let ((result (parse-file "examples/factorial")))
  (check "factorial parses"
         '((define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))))
         result))

;; These are just smoke tests - they verify parsing doesn't crash
;; and produces a define form, but don't check the full structure
(let ((result (parse-file "examples/sqrt")))
  (check "sqrt parses (smoke test)" 'define (caar result)))

(let ((result (parse-file "examples/hashtable")))
  (check "hashtable parses (smoke test)" 'define (caar result)))

(test-section "Round-trip tests")

;; Round-trip test: S-expr -> indented -> S-expr
(define (round-trip expr)
  (call-with-output-file "test-round-trip.tmp"
    (lambda (port)
      (print-indented expr port)))
  (parse-file "test-round-trip.tmp"))

;; Test cases that should round-trip correctly
;; Note: indent syntax has limitations with forms starting with lists:
;;   - (lambda (x) ...) - params must be atom, not list
;;   - (let ((x 1)) ...) - binding list causes issues
;;   - (cond ((test) ...) ...) - clauses starting with lists
;;   - nullary calls like (newline) in non-head position
(define test-cases
  '(
    ;; Simple function calls
    (+ 1 2)
    (+ 1 2 3 4 5)
    (f x)
    (f x y z)

    ;; Define variable
    (define x 42)

    ;; Define function (works because (f x) normalizes)
    (define (square x) (* x x))

    ;; Conditionals
    (if a b c)
    (if (= x 0) 1 (* x y))

    ;; Nested calls
    (+ (* a b) (* c d))
    (f (g x) (h y))
    (if (= x 0) 1 (* x (factorial (- x 1))))

    ;; Quote
    (quote foo)
    (quote (a b c))

    ;; Multiple args with nesting
    (list (+ 1 2) (+ 3 4) (+ 5 6))
    ))

(define (test-round-trip expr)
  (let ((result (round-trip expr)))
    (if (and (= 1 (length result))
             (equal? (car result) expr))
        #t
        (begin
          (display "  Round-trip failed for: ") (write expr) (newline)
          (display "  Got: ") (write result) (newline)
          #f))))

(let loop ((cases test-cases) (pass 0) (fail 0))
  (if (null? cases)
      (begin
        (check "round-trip tests passed" 0 fail)
        (display "  ") (display pass) (display " expressions round-tripped correctly")
        (newline))
      (if (test-round-trip (car cases))
          (loop (cdr cases) (+ pass 1) fail)
          (loop (cdr cases) pass (+ fail 1)))))

(test-summary)
