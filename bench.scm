;;; bench.scm - Simple benchmarks for uts
;;;
;;; Focus: arithmetic, recursion, list operations

(define (time-thunk name thunk)
  ;; Run thunk and report time
  ;; uts doesn't have built-in timing, so we'll just run and report completion
  (display name)
  (display ": ")
  (let ((result (thunk)))
    (display "done, result=")
    (write result)
    (newline)
    result))

(define (repeat n thunk)
  ;; Run thunk n times, return last result
  (if (<= n 1)
      (thunk)
      (begin (thunk) (repeat (- n 1) thunk))))

;;; Tak - classic recursive benchmark (tests arithmetic + non-tail recursion)
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

;;; Fib - exponential recursion (tests arithmetic + calls)
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;;; Ack - Ackermann function (tests deep recursion)
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

;;; Sum - tight arithmetic loop
(define (sum-to n)
  (let loop ((i 0) (acc 0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

;;; Sumfp - floating point arithmetic
(define (sum-fp n)
  (let loop ((i 0) (acc 0.0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc (* i 1.0))))))

;;; List operations
(define (make-list n)
  (let loop ((i 0) (acc '()))
    (if (>= i n)
        acc
        (loop (+ i 1) (cons i acc)))))

(define (sum-list lst)
  (let loop ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (loop (cdr lst) (+ acc (car lst))))))

;;; Append benchmark (tests cons allocation)
(define (append-bench n)
  (let ((lst (make-list 100)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (length acc)
          (loop (+ i 1) (append lst acc))))))

;;; Large integer arithmetic (tests 62-bit fixnums)
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(define (fac-sum limit)
  ;; Sum of factorials, exercises large multiply
  (let loop ((i 1) (acc 0))
    (if (> i limit)
        acc
        (loop (+ i 1) (+ acc (factorial i))))))

;;; Run all benchmarks
(define (run-benchmarks)
  (display "=== uts benchmarks ===")
  (newline)

  ;; Tak - classic, about 63609 calls for (tak 18 12 6)
  (time-thunk "tak(18,12,6)" (lambda () (tak 18 12 6)))

  ;; Fib - exponential, fib(30) = 832040 with ~2.7M calls
  (time-thunk "fib(30)" (lambda () (fib 30)))

  ;; Ackermann - deep recursion, ack(3,7) = 1021 (3,9 overflows)
  (time-thunk "ack(3,7)" (lambda () (ack 3 7)))

  ;; Sum - tight loop with fixnum add
  (time-thunk "sum(1000000)" (lambda () (sum-to 1000000)))

  ;; Sum-fp - floating point
  (time-thunk "sum-fp(1000000)" (lambda () (sum-fp 1000000)))

  ;; List operations
  (time-thunk "list-sum(10000)"
              (lambda () (sum-list (make-list 10000))))

  ;; Append - cons allocation
  (time-thunk "append(1000)" (lambda () (append-bench 1000)))

  ;; Large integers
  (time-thunk "fac-sum(19)" (lambda () (fac-sum 19)))

  (display "=== done ===")
  (newline))

(run-benchmarks)
