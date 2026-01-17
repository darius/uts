;;;;
;;;; Disassemble a fasl file, to stdout.
;;;;

(define (dump-fasl file)
  (load-fasl file 
	     (lambda (code)
	       (newline)
	       (disassemble code -1))))

(define (load-fasl file evaluate)
  (call-with-input-file file
    (lambda (port)
      (@read-fasl-header port)
      (let loop ()
	(let ((o (@read-fasl port)))
	  (cond ((not (eof-object? o))
		 (evaluate o)
		 (loop))))))))
