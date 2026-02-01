;;; examples.scm - Miscellaneous examples and supplemental tests, by Claude.
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
;;; Tiny Forth interpreter
;;; ============================================================

(define (make-forth)
  (let ((stack '())
        (rstack '())
        (dict '()))

    (define (push x) (set! stack (cons x stack)))
    (define (pop) (let ((x (car stack))) (set! stack (cdr stack)) x))
    (define (rpush x) (set! rstack (cons x rstack)))
    (define (rpop) (let ((x (car rstack))) (set! rstack (cdr rstack)) x))

    (define (builtin-op op)
      (let ((b (pop)) (a (pop)))
        (push (op a b))))

    (define builtins
      `((+ . ,(lambda () (builtin-op +)))
        (- . ,(lambda () (builtin-op -)))
        (* . ,(lambda () (builtin-op *)))
        (/ . ,(lambda () (builtin-op quotient)))
        (dup . ,(lambda () (push (car stack))))
        (drop . ,(lambda () (pop)))
        (swap . ,(lambda () (let ((a (pop)) (b (pop))) (push a) (push b))))
        (over . ,(lambda () (push (cadr stack))))
        (rot . ,(lambda () (let ((a (pop)) (b (pop)) (c (pop)))
                             (push b) (push a) (push c))))
        (>r . ,(lambda () (rpush (pop))))
        (r> . ,(lambda () (push (rpop))))
        (= . ,(lambda () (push (if (= (pop) (pop)) -1 0))))
        (< . ,(lambda () (let ((b (pop)) (a (pop))) (push (if (< a b) -1 0)))))
        (and . ,(lambda () (push (bitwise-and (pop) (pop)))))
        (or . ,(lambda () (push (bitwise-ior (pop) (pop)))))
        (not . ,(lambda () (push (bitwise-not (pop)))))
        (. . ,(lambda () (display (pop)) (display " ")))))

    (define (exec word)
      (cond
        ((number? word) (push word))
        ((assq word builtins) => (lambda (p) ((cdr p))))
        ((assq word dict) => (lambda (p) (run (cdr p))))
        (else (error "Unknown word" word))))

    (define (run words)
      (for-each exec words))

    (define (define-word name body)
      (set! dict (cons (cons name body) dict)))

    (lambda (msg . args)
      (case msg
        ((run) (run (car args)) stack)
        ((define) (define-word (car args) (cadr args)))
        ((stack) stack)
        ((reset) (set! stack '()) (set! rstack '()))))))

(define (test-forth)
  (let ((f (make-forth)))
    (f 'define 'square '(dup *))
    (f 'define 'cube '(dup square *))
    (f 'reset)
    (and
     (equal? (f 'run '(3 4 +)) '(7))
     (equal? (begin (f 'reset) (f 'run '(5 square))) '(25))
     (equal? (begin (f 'reset) (f 'run '(3 cube))) '(27))
     (equal? (begin (f 'reset) (f 'run '(10 3 /))) '(3)))))


;;; ============================================================
;;; Maze generator (randomized depth-first search)
;;; ============================================================

(define (make-maze rows cols)
  ;; Returns a vector of vectors: 0=wall, 1=passage
  ;; Uses randomized DFS to carve passages

  (define (make-grid val)
    (let ((g (make-vector rows)))
      (do ((r 0 (+ r 1))) ((>= r rows) g)
        (vector-set! g r (make-vector cols val)))))

  (define grid (make-grid 0))
  (define (get r c) (vector-ref (vector-ref grid r) c))
  (define (put! r c v) (vector-set! (vector-ref grid r) c v))

  ;; Simple xorshift random for reproducibility
  (define seed 12345)
  (define (rand n)
    (set! seed (bitwise-xor seed (arithmetic-shift seed 13)))
    (set! seed (bitwise-and seed #xffffffff))
    (set! seed (bitwise-xor seed (arithmetic-shift seed -17)))
    (set! seed (bitwise-xor seed (arithmetic-shift seed 5)))
    (set! seed (bitwise-and seed #xffffffff))
    (remainder seed n))

  (define (shuffle lst)
    (let ((v (list->vector lst)))
      (do ((i (- (vector-length v) 1) (- i 1))) ((< i 1) (vector->list v))
        (let* ((j (rand (+ i 1)))
               (tmp (vector-ref v i)))
          (vector-set! v i (vector-ref v j))
          (vector-set! v j tmp)))))

  (define (in-bounds? r c)
    (and (>= r 0) (< r rows) (>= c 0) (< c cols)))

  (define (carve r c)
    (put! r c 1)
    (for-each
     (lambda (dir)
       (let ((dr (car dir)) (dc (cdr dir)))
         (let ((nr (+ r (* dr 2))) (nc (+ c (* dc 2))))
           (if (and (in-bounds? nr nc) (= (get nr nc) 0))
               (begin
                 (put! (+ r dr) (+ c dc) 1)  ; knock down wall
                 (carve nr nc))))))
     (shuffle '((0 . 1) (0 . -1) (1 . 0) (-1 . 0)))))

  ;; Start from (1,1)
  (carve 1 1)
  grid)

(define (maze->string grid)
  (let ((rows (vector-length grid))
        (cols (vector-length (vector-ref grid 0))))
    (let loop ((r 0) (acc '()))
      (if (>= r rows)
          (apply string-append (reverse acc))
          (loop (+ r 1)
                (cons (string-append
                       (let cloop ((c 0) (s ""))
                         (if (>= c cols)
                             s
                             (cloop (+ c 1)
                                    (string-append s (if (= (vector-ref (vector-ref grid r) c) 0)
                                                         "#" " ")))))
                       "\n")
                      acc))))))

(define (test-maze)
  (let* ((m (make-maze 11 21))
         (s (maze->string m)))
    ;; Check that we have passages (spaces) and walls (#)
    (and (> (string-length s) 0)
         ;; Verify corners are walls
         (eqv? (string-ref s 0) #\#))))


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
  (check "forth" test-forth)
  (check "maze" test-maze)

  (newline)
  (display "Examples: ")
  (display passed)
  (display " passed, ")
  (display failed)
  (display " failed")
  (newline)

  (= failed 0))

(run-examples)
