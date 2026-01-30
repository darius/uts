;;; Tests for ridarama

(load "../test-support.scm")
(load "loadme.scm")
(load "fun-tests.scm")

(test-section "Ridarama transit planner")

;; Run the built-in tests (A* search on BART/Caltrain data)
(display "  Running do-tests (A* search)...") (newline)
(do-tests)
(display "  A* search test passed") (newline)

;; The test succeeds if no error was thrown by expect in fun-tests.scm
(check "do-tests completed" #t #t)

;; Compare search output against reference
;; We run the search again, capturing output to compare
(display "  Comparing output against reference...") (newline)
(let ((diff-result (%system "../../uts ../../uts.fasl -f run-search.scm 2>&1 | grep -v '\"' > output.test && diff -q output.test output.reference")))
  (check "search output matches reference" 0 diff-result))

;; Clean up
(%system "rm -f output.test")

(test-summary)
