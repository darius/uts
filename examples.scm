;;; examples.scm - Miscellaneous examples and supplemental tests
;;;
;;; These demonstrate Scheme features and serve as additional test coverage.

;;; ============================================================
;;; Coroutines via continuations
;;; ============================================================

(define (test-coroutine)
  ;; Simple ping-pong between two continuations
  (let ((count 0))
    (call-with-current-continuation
     (lambda (exit)
       (letrec ((ping (lambda ()
                        (set! count (+ count 1))
                        (if (< count 5) (pong) (exit count))))
                (pong (lambda ()
                        (set! count (+ count 1))
                        (ping))))
         (ping))))))


;;; ============================================================
;;; Amb - nondeterministic choice operator
;;; ============================================================

(define *amb-fail* (lambda () (error "amb: no more choices")))

(define (amb . choices)
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (choice)
        (let ((old-fail *amb-fail*))
          (call-with-current-continuation
           (lambda (k)
             (set! *amb-fail* (lambda () (set! *amb-fail* old-fail) (k #f)))
             (return choice)))))
      choices)
     (*amb-fail*))))

(define (require pred)
  (if (not pred) (*amb-fail*)))

(define (test-amb)
  ;; Find pythagorean triples
  (call-with-current-continuation
   (lambda (exit)
     (set! *amb-fail* (lambda () (exit 'no-solution)))
     (let* ((a (amb 1 2 3 4 5 6 7))
            (b (amb 1 2 3 4 5 6 7))
            (c (amb 1 2 3 4 5 6 7)))
       (require (<= a b))
       (require (= (+ (* a a) (* b b)) (* c c)))
       (list a b c)))))  ; => (3 4 5)


;;; ============================================================
;;; Quine - a program that outputs itself
;;; ============================================================

(define quine
  ((lambda (x) (list x (list 'quote x)))
   '(lambda (x) (list x (list 'quote x)))))

(define (test-quine)
  (equal? quine
          ((lambda (x) (list x (list 'quote x)))
           '(lambda (x) (list x (list 'quote x))))))


;;; ============================================================
;;; Tiny pattern matcher
;;; ============================================================

(define (match pattern datum)
  ;; Returns alist of bindings or #f
  ;; Patterns: symbols bind, (? pred) tests, literals match exactly, lists recurse
  (cond
    ((and (pair? pattern)
          (eq? (car pattern) '?)
          (pair? (cdr pattern)))
     ;; (? predicate) - test predicate
     (if ((cadr pattern) datum) '() #f))
    ((symbol? pattern)
     ;; Variable - bind it
     (list (cons pattern datum)))
    ((pair? pattern)
     (if (pair? datum)
         (let ((car-match (match (car pattern) (car datum))))
           (if car-match
               (let ((cdr-match (match (cdr pattern) (cdr datum))))
                 (if cdr-match
                     (append car-match cdr-match)
                     #f))
               #f))
         #f))
    (else
     ;; Literal - must match exactly
     (if (equal? pattern datum) '() #f))))

(define (test-match)
  (and
   (equal? (match '(a b c) '(1 2 3))
           '((a . 1) (b . 2) (c . 3)))
   (equal? (match (list (list '? number?) 'x) '(42 hello))
           '((x . hello)))
   (not (match '(a b) '(1)))))


;;; ============================================================
;;; Streams (lazy lists)
;;; ============================================================

(define (stream-cons head tail-thunk)
  (cons head tail-thunk))

(define (stream-car s) (car s))
(define (stream-cdr s) ((cdr s)))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s)
            (stream-take (- n 1) (stream-cdr s)))))

(define (stream-map f s)
  (stream-cons (f (stream-car s))
               (lambda () (stream-map f (stream-cdr s)))))

(define (integers-from n)
  (stream-cons n (lambda () (integers-from (+ n 1)))))

(define (sieve s)
  ;; Sieve of Eratosthenes on a stream
  (let ((p (stream-car s)))
    (stream-cons p
                 (lambda ()
                   (sieve
                    (stream-filter
                     (lambda (x) (not (= 0 (remainder x p))))
                     (stream-cdr s)))))))

(define (stream-filter pred s)
  (if (pred (stream-car s))
      (stream-cons (stream-car s)
                   (lambda () (stream-filter pred (stream-cdr s))))
      (stream-filter pred (stream-cdr s))))

(define primes (sieve (integers-from 2)))

(define (test-streams)
  (equal? (stream-take 10 primes)
          '(2 3 5 7 11 13 17 19 23 29)))


;;; ============================================================
;;; Deriv - symbolic differentiation
;;; ============================================================

(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((symbol? expr) (if (eq? expr var) 1 0))
    ((eq? (car expr) '+)
     (list '+ (deriv (cadr expr) var) (deriv (caddr expr) var)))
    ((eq? (car expr) '*)
     (list '+
           (list '* (cadr expr) (deriv (caddr expr) var))
           (list '* (deriv (cadr expr) var) (caddr expr))))
    (else (error "deriv: unknown operator" (car expr)))))

(define (simplify expr)
  (cond
    ((not (pair? expr)) expr)
    (else
     (let ((op (car expr))
           (a (simplify (cadr expr)))
           (b (simplify (caddr expr))))
       (cond
         ((and (eq? op '+) (eqv? a 0)) b)
         ((and (eq? op '+) (eqv? b 0)) a)
         ((and (eq? op '*) (eqv? a 0)) 0)
         ((and (eq? op '*) (eqv? b 0)) 0)
         ((and (eq? op '*) (eqv? a 1)) b)
         ((and (eq? op '*) (eqv? b 1)) a)
         ((and (number? a) (number? b))
          (if (eq? op '+) (+ a b) (* a b)))
         (else (list op a b)))))))

(define (test-deriv)
  (and
   (equal? (simplify (deriv '(* x x) 'x))
           '(+ x x))
   (equal? (simplify (deriv '(+ (* 3 x) (* x x)) 'x))
           '(+ 3 (+ x x)))))


;;; ============================================================
;;; Bit manipulation - Gray code
;;; ============================================================

(define (gray-encode n)
  ;; Convert integer to Gray code
  (bitwise-xor n (arithmetic-shift n -1)))

(define (gray-decode g)
  ;; Convert Gray code back to integer
  (let loop ((g g) (n 0))
    (if (= g 0)
        n
        (loop (arithmetic-shift g -1)
              (bitwise-xor n g)))))

(define (test-gray)
  (and
   ;; First 8 Gray codes: 0,1,3,2,6,7,5,4
   (equal? (map gray-encode '(0 1 2 3 4 5 6 7))
           '(0 1 3 2 6 7 5 4))
   ;; Round-trip
   (equal? (map (lambda (n) (gray-decode (gray-encode n)))
                '(0 1 2 3 100 255 1000))
           '(0 1 2 3 100 255 1000))))


;;; ============================================================
;;; Run all example tests
;;; ============================================================

(define (run-examples)
  (define passed 0)
  (define failed 0)

  (define (check name thunk)
    (if (thunk)
        (begin (set! passed (+ passed 1)) (display "."))
        (begin (set! failed (+ failed 1))
               (newline) (display "FAIL: ") (display name) (newline))))

  (display "Running examples...")
  (newline)

  (check "coroutine" (lambda () (= (test-coroutine) 5)))
  (check "amb" (lambda () (equal? (test-amb) '(3 4 5))))
  (check "quine" test-quine)
  (check "match" test-match)
  (check "streams" test-streams)
  (check "deriv" test-deriv)
  (check "gray" test-gray)

  (newline)
  (display "Examples: ")
  (display passed)
  (display " passed, ")
  (display failed)
  (display " failed")
  (newline)

  (= failed 0))

(run-examples)
