;;; superopt.scm - Circuit superoptimizer benchmark
;;; Ported from https://github.com/darius/superbench

(define (superopt truth-table max-gates)
  (let ((n-inputs (the-integer (log2 (string-length truth-table)))))
    (find-circuits (string->number truth-table 2) n-inputs max-gates)))

(define (the-integer x)
  (if (integer? x)
      (inexact->exact x)
      (error "Not an integer" x)))

(define (log2 n)
  (case n
    ((1) 0)
    ((2) 1)
    ((4) 2)
    ((8) 3)
    ((16) 4)
    ((32) 5)
    (else (error "log2: unsupported" n))))

(define (pow2 n)
  (arithmetic-shift 1 n))

(define (find-circuits wanted n-inputs max-gates)
  (let ((inputs (tabulate-inputs n-inputs))
        (mask (- (pow2 (pow2 n-inputs)) 1)))

    (define (find-for-n n-gates)
      (let* ((n-wires (+ n-inputs n-gates))
             (L-input (make-vector n-gates #f))
             (R-input (make-vector n-gates #f))
             (wire (list->vector (append inputs (vector->list L-input))))
             (found? #f))
        (let sweeping ((gate 0))
          (do ((L 0 (+ L 1)))  ((= L (+ n-inputs gate)))
            (let ((L-wire (vector-ref wire L)))
              (vector-set! L-input gate L)
              (do ((R 0 (+ R 1)))  ((= R (+ L 1)))
                (let ((value (nand L-wire (vector-ref wire R))))
                  (vector-set! R-input gate R)
                  (vector-set! wire (+ n-inputs gate) value)
                  (cond ((< (+ gate 1) n-gates)
                         (sweeping (+ gate 1)))
                        ((= wanted (bitwise-and mask value))
                         (set! found? #t)))))))
          found?)))

    (some? find-for-n (iota1 max-gates))))

(define (nand x y)
  (bitwise-not (bitwise-and x y)))

(define (some? ok? xs)
  (and (not (null? xs))
       (or (ok? (car xs))
           (some? ok? (cdr xs)))))

(define (iota1 n)
  (let loop ((i 1))
    (if (< n i)
        '()
        (cons i (loop (+ i 1))))))

(define (tabulate-inputs n-inputs)
  (if (= n-inputs 0)
      '()
      (let ((shift (pow2 (- n-inputs 1))))
        (cons (- (pow2 shift) 1)
              (map (lambda (iv) (bitwise-ior iv (arithmetic-shift iv shift)))
                   (tabulate-inputs (- n-inputs 1)))))))

;;; Benchmark entry point
(define (run-superopt)
  ;; 3-input majority: exhaustive search up to 6 gates (fails, ~2.5s work)
  (superopt "11101000" 6))
