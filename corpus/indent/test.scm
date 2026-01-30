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

(let ((result (parse-file "examples/sqrt")))
  (check "sqrt is single define" 1 (length result))
  (check "sqrt parses to define" 'define (caar result)))

(let ((result (parse-file "examples/hashtable")))
  (check "hashtable parses" 'define (caar result)))

(test-section "Round-trip tests")

;; Version of print-indented that writes to a port
(define (print-indented-to-port x port)
  (let ((column 0))

    (define (nl)
      (newline port)
      (set! column 0))

    (define (show str)
      (display str port)
      (set! column (+ column (string-length str))))

    (define (output atom)
      (show (coerce-string atom)))

    (define (indent c)
      (do ()
          ((<= c column))
        (show " ")))

    (define (print x)
      (cond ((not (pair? x))
             (output x)
             (nl))
            (else
             (let ((x (normalize-list x)))
               (output (car x))
               (let ((rest (cdr x)))
                 (cond ((and (not (= (length rest) 1))
                             (one-liner? (+ column 1) rest))
                        (show ":")
                        (print-one-line rest))
                       (else
                        (print-each rest))))))))

    (define (normalize-list ls)
      (if (pair? (car ls))
          (cons (string->symbol "(") ls)
          ls))

    (define (one-liner? column ls)
      (and (all atom? ls)
           (<= (+ column (one-liner-length ls))
               comfortable-width)))

    (define (one-liner-length ls)
      (+ (length ls)
         (sum (map (compose string-length coerce-string) ls))))

    (define (print-one-line ls)
      (for-each (lambda (x)
                  (show " ")
                  (output x))
                ls)
      (nl))

    (define (print-each ls)
      (show " ")
      (for-each (let ((c column))
                  (lambda (arg)
                    (indent c)
                    (print arg)))
                ls))

    (print x)))

;; Round-trip test: S-expr -> indented -> S-expr
(define (round-trip expr)
  (call-with-output-file "test-round-trip.tmp"
    (lambda (port)
      (print-indented-to-port expr port)))
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
