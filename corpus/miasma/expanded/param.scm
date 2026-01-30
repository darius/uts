(define opcode-byte-param (lambda (byte) (quasiquote (bytes u 1 (unquote byte)))))

(define register-param (lambda (register) (quasiquote (bytes u 0 0))))

(define size-mode-param (lambda (bits) (if (= bits 32) (quasiquote (bytes u 0 0)) (quasiquote (bytes u 1 102)))))

(define signed-immediate-param (lambda (operand) (let ((size (operand.size operand))) (quasiquote (bytes i (unquote size) (arg int))))))

(define unsigned-immediate-param (lambda (operand) (let ((size (operand.size operand))) (quasiquote (bytes u (unquote size) (arg int))))))

(define relative-jump-param (lambda (operand) (let ((size (operand.size operand))) (quasiquote (bytes i (unquote size) (- (arg int) (hereafter)))))))

(define offset-param (lambda (operand) (let ((size (operand.size operand))) (quasiquote (bytes u (unquote size) (arg int))))))

(define condition-param (lambda (opcode-byte) (quasiquote (bytes u 1 (+ (unquote opcode-byte) (arg cc))))))

(define opcode+register-param (lambda (opcode-byte operand) (let ((size (operand.size operand))) (quasiquote (bytes u 1 (+ (unquote opcode-byte) (arg reg (unquote size))))))))

(define ex.gx-param (lambda (ex gx) (quasiquote (swap-args (mod-r/m (arg reg (unquote (operand.size gx))) (arg (unquote ex)))))))

(define gx.ex-param (lambda (gx ex) (quasiquote (mod-r/m (arg reg (unquote (operand.size gx))) (arg (unquote ex))))))

(define ex-param (lambda (extended-opcode operand) (quasiquote (mod-r/m (unquote extended-opcode) (arg (unquote operand))))))

