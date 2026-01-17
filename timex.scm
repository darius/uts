;; Return (k elapsed (proc))
;; where elapsed is the time in seconds it took to evaluate (proc).

(define timex
  (lambda (proc k)
    (let ((start (@runtime)))
      (let ((result (proc)))
	(k (- (@runtime) start)
	   result)))))
