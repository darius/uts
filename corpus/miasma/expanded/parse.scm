(define make-spec (lambda (mnemonic stem params doc-string uses) (list mnemonic stem (filter (lambda (x) (not (equal? x (quote (bytes u 0 0))))) params) doc-string uses #f)))

(define spec.mnemonic car)

(define spec.stem cadr)

(define spec.params caddr)

(define spec.doc-string cadddr)

(define spec.uses (lambda (l) (car (cddddr l))))

(define spec.takes (lambda (l) (cadr (cddddr l))))

(define make-mnemonic (lambda (stem stuff-list) (make-combined-mnemonic stem (map coerce-string (suffixes stuff-list)))))

(define make-combined-mnemonic (lambda (stem specs) (string->symbol (string-join (cons stem specs) "."))))

(define suffixes (lambda (stuff-list) (define suffixes1 (lambda (x) (cond ((memq x (quote (sreg cr dr))) (quote ())) ((memq x (quote (=16 =32 + /r /0 /1 /2 /3 /4 /5 /6 /7))) (quote ())) ((symbol? x) (list x)) ((not (pair? x)) (quote ())) (else (flatmap suffixes1 x))))) (flatmap suffixes1 stuff-list)))

(define the-specs (quote ()))

(define setup-spec-table (lambda () (set! the-specs (map parse-spec (snarf (make-pathname "src" "tables" "i386.scm"))))))

(define find-spec (lambda (mnemonic) (or (assq mnemonic the-specs) (panic "Unknown instruction" mnemonic))))

(define unparse-spec (lambda (spec) (quasiquote ((unquote (spec.mnemonic spec)) (unquote-splicing (map unparse-param (spec.params spec))) (unquote (spec.doc-string spec))))))

(define parse-spec (lambda (spec) (insist "Spec has everything" (and (list? spec) (<= 3 (length spec)) (symbol? (car spec)))) (let loop ((p (cdr spec)) (r (quote ()))) (if (pair? p) (if (string? (car p)) (make-spec (make-mnemonic (symbol->string (car spec)) (reverse r)) (symbol->string (car spec)) (map parse-param (reverse r)) (car p) (if (pair? (cdr p)) (cadr p) #f)) (loop (cdr p) (cons (car p) r))) (impossible (quote parse-spec) spec)))))

(define parse-param (lambda (param) (let ((param (expand-abbrev param))) (cond ((byte? param) (opcode-byte-param param)) ((register? param) (register-param param)) ((memq param (quote (=16 =32))) (size-mode-param (case param ((=16) 16) ((=32) 32)))) ((operand? (quote i) param) (signed-immediate-param param)) ((operand? (quote u) param) (unsigned-immediate-param param)) ((operand? (quote j) param) (relative-jump-param param)) ((operand? (quote o) param) (offset-param param)) ((pair? param) (let* ((ls (map expand-abbrev (cdr param))) (l (length ls))) (case (car param) ((?) (insist "? syntax" (and (= l 1) (byte? (car ls)))) (condition-param (car ls))) ((+) (insist "+ syntax" (and (= l 2) (byte? (car ls)) (operand? (quote g) (cadr ls)))) (opcode+register-param (car ls) (cadr ls))) ((/r) (insist "/r syntax" (and (= l 2) (or (and (operand? (quote e) (car ls)) (operand? (quote g) (cadr ls))) (and (operand? (quote g) (car ls)) (operand? (quote e) (cadr ls)))))) (if (operand? (quote e) (car ls)) (ex.gx-param (car ls) (cadr ls)) (gx.ex-param (car ls) (cadr ls)))) ((/0 /1 /2 /3 /4 /5 /6 /7) (insist "/n syntax" (and (= l 1) (operand? (quote e) (car ls)))) (let ((extended-opcode (cadr (assq (car param) (quote ((/0 0) (/1 1) (/2 2) (/3 3) (/4 4) (/5 5) (/6 6) (/7 7))))))) (ex-param extended-opcode (car ls)))) (else (impossible (quote parse-param) param))))) (else (impossible (quote parse-param) param))))))

(define abbrevs (map (lambda (abbrev-pair expanded-pair) (list (concat-symbol (car abbrev-pair) (cdr abbrev-pair)) (car expanded-pair) (cdr expanded-pair))) (outer-product (quote (e g i u m r j o s)) (quote (b w v d))) (outer-product (quote (e g i u e e j o s)) (quote (1 2 4 4)))))

(define expand-abbrev (lambda (x) (or (assq x abbrevs) x)))

(define operand? (lambda (tag x) (and (pair? x) (symbol? (car x)) (starts-with? tag (cdr x)) (null? (cdddr x)) (memv (caddr x) (quote (1 2 4))))))

(define operand.symbol car)

(define operand.tag cadr)

(define operand.size caddr)

