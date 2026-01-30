;;; Tests for indent

(load "../test-support.scm")
(load "loadme.scm")

(test-section "Indent parser")

;; Helper to parse a file and convert to s-expressions
(define (parse-file filename)
  (map post-read (read-file filename)))

;; Test factorial example
(let ((result (parse-file "examples/factorial")))
  (check "factorial parses"
         '((define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))))
         result))

;; Test sqrt example
(let ((result (parse-file "examples/sqrt")))
  (check "sqrt is single define"
         1
         (length result))
  (check "sqrt parses to define"
         'define
         (caar result)))

;; Test hashtable example
(let ((result (parse-file "examples/hashtable")))
  (check "hashtable parses"
         'define
         (caar result)))

(test-summary)
