;;; um.scm - Universal Machine interpreter
;;; Ported from https://github.com/darius/superbench
;;; See http://www.boundvariable.org/task.shtml for the UM spec

(define (make-um program)
  ;; Returns a closure that runs one instruction and returns #t to continue, #f to halt
  (let ((mem (make-vector 1000 #f))
        (freelist '())
        (reg (make-vector 8 0))
        (pc 0))

    (vector-set! mem 0 program)

    (lambda ()
      (let* ((inst (vector-ref (vector-ref mem 0) pc))
             (opcode (arithmetic-shift inst -28)))
        (set! pc (+ pc 1))

        (cond
          ((= opcode 13)  ; ortho
           (let ((a (bitwise-and 7 (arithmetic-shift inst -25)))
                 (val (bitwise-and inst #x1FFFFFF)))
             (vector-set! reg a val))
           #t)

          ((= opcode 0)  ; cmov
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (if (not (= (vector-ref reg c) 0))
                 (vector-set! reg a (vector-ref reg b))))
           #t)

          ((= opcode 1)  ; index
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! reg a
               (vector-ref (vector-ref mem (vector-ref reg b))
                           (vector-ref reg c))))
           #t)

          ((= opcode 2)  ; amend
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! (vector-ref mem (vector-ref reg a))
                          (vector-ref reg b)
                          (vector-ref reg c)))
           #t)

          ((= opcode 3)  ; add
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! reg a
               (bitwise-and (+ (vector-ref reg b) (vector-ref reg c))
                            #xFFFFFFFF)))
           #t)

          ((= opcode 4)  ; mul
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! reg a
               (bitwise-and (* (vector-ref reg b) (vector-ref reg c))
                            #xFFFFFFFF)))
           #t)

          ((= opcode 5)  ; div
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! reg a
               (quotient (vector-ref reg b) (vector-ref reg c))))
           #t)

          ((= opcode 6)  ; nand
           (let ((a (bitwise-and 7 (arithmetic-shift inst -6)))
                 (b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (vector-set! reg a
               (bitwise-and (bitwise-not
                              (bitwise-and (vector-ref reg b)
                                           (vector-ref reg c)))
                            #xFFFFFFFF)))
           #t)

          ((= opcode 7)  ; halt
           #f)

          ((= opcode 8)  ; alloc
           (let* ((b (bitwise-and 7 (arithmetic-shift inst -3)))
                  (c (bitwise-and 7 inst))
                  (size (vector-ref reg c))
                  (i (if (null? freelist)
                         (let loop ((j 1))
                           (if (vector-ref mem j)
                               (loop (+ j 1))
                               j))
                         (let ((i (car freelist)))
                           (set! freelist (cdr freelist))
                           i))))
             (vector-set! mem i (make-vector size 0))
             (vector-set! reg b i))
           #t)

          ((= opcode 9)  ; abandon
           (let ((c (bitwise-and 7 inst)))
             (let ((i (vector-ref reg c)))
               (vector-set! mem i #f)
               (set! freelist (cons i freelist))))
           #t)

          ((= opcode 10) ; output
           (let ((c (bitwise-and 7 inst)))
             (write-char (integer->char (vector-ref reg c))))
           #t)

          ((= opcode 11) ; input
           (let ((c (bitwise-and 7 inst)))
             (let ((ch (read-char)))
               (vector-set! reg c
                 (if (eof-object? ch) #xFFFFFFFF (char->integer ch)))))
           #t)

          ((= opcode 12) ; load program
           (let ((b (bitwise-and 7 (arithmetic-shift inst -3)))
                 (c (bitwise-and 7 inst)))
             (let ((b-val (vector-ref reg b)))
               (if (not (= b-val 0))
                   (let* ((src (vector-ref mem b-val))
                          (copy (make-vector (vector-length src) 0)))
                     (do ((i 0 (+ i 1)))
                         ((>= i (vector-length src)))
                       (vector-set! copy i (vector-ref src i)))
                     (vector-set! mem 0 copy)))
               (set! pc (vector-ref reg c))))
           #t)

          (else
           (error "Bad opcode" opcode)))))))

;;; Create a looping benchmark program
(define (make-loop-program iterations)
  ;; A simple loop using the UM instruction set
  ;; reg0 = counter, reg1 = -1 (via nand trick), reg2 = 0
  ;; reg3 = loop addr, reg4 = exit addr, reg5 = temp
  (let ((prog (make-vector 16 0)))
    ;; 0: ortho reg0, iterations
    (vector-set! prog 0 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 0 25)
                           iterations))
    ;; 1: ortho reg2, 0
    (vector-set! prog 1 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 2 25)
                           0))
    ;; 2: ortho reg3, 5 (loop addr)
    (vector-set! prog 2 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 3 25)
                           5))
    ;; 3: ortho reg4, 10 (exit addr)
    (vector-set! prog 3 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 4 25)
                           10))
    ;; 4: nand reg1, reg2, reg2 (reg1 = ~0 = 0xFFFFFFFF = -1)
    (vector-set! prog 4 (+ (arithmetic-shift 6 28)
                           (arithmetic-shift 1 6)
                           (arithmetic-shift 2 3)
                           2))
    ;; Loop body (starts at 5):
    ;; 5: nand reg5, reg0, reg0 (work instruction)
    (vector-set! prog 5 (+ (arithmetic-shift 6 28)
                           (arithmetic-shift 5 6)
                           (arithmetic-shift 0 3)
                           0))
    ;; 6: add reg0, reg0, reg1 (reg0 += -1, i.e., reg0--)
    (vector-set! prog 6 (+ (arithmetic-shift 3 28)
                           (arithmetic-shift 0 6)
                           (arithmetic-shift 0 3)
                           1))
    ;; 7: ortho reg6, 5 (reload loop addr in case cmov changed it)
    (vector-set! prog 7 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 6 25)
                           5))
    ;; 8: cmov reg6, reg4, reg0 (if reg0 != 0, reg6 stays loop; else exit)
    ;; Wait, this is backwards again. cmov sets reg6=reg4 if reg0 != 0.
    ;; We want: if reg0 == 0, jump to exit.
    ;; So: start with reg6 = exit, cmov reg6, loop, reg0
    ;; 7: ortho reg6, 10 (exit addr)
    (vector-set! prog 7 (+ (arithmetic-shift 13 28)
                           (arithmetic-shift 6 25)
                           10))
    ;; 8: cmov reg6, reg3, reg0 (if reg0 != 0, reg6 = loop_addr)
    (vector-set! prog 8 (+ (arithmetic-shift 0 28)
                           (arithmetic-shift 6 6)
                           (arithmetic-shift 3 3)
                           0))
    ;; 9: load-program 0, reg6 (jump to reg6)
    (vector-set! prog 9 (+ (arithmetic-shift 12 28)
                           (arithmetic-shift 2 3)  ; b=reg2 (0)
                           6))                      ; c=reg6
    ;; 10: halt
    (vector-set! prog 10 (arithmetic-shift 7 28))
    prog))

(define (run-um-bench iterations)
  ;; Run the UM with a looping program
  ;; Returns instruction count
  (let ((step (make-um (make-loop-program iterations))))
    (let loop ((count 0))
      (if (step)
          (loop (+ count 1))
          count))))
