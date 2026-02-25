;;;; Disassemble a fasl file, to stdout.

(define (dump-fasl filename)
  (for-each (lambda (code)
	      (newline)
	      (disassemble code -1))
            (load-fasl filename)))

(define (load-fasl filename)
  (vector->list (call-with-input-file filename %read-fasl)))

(dump-fasl "uts.fasl")
