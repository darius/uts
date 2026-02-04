;;; Tests for ridarama

(load "../test-support.scm")

(test-section "Ridarama transit planner")

;; Run the A* search test and compare output against reference
(display "  Running do-tests (A* search)...") (newline)
;; N.B. The env var UTS is set by ../run-corpus
(let ((diff-result (%system "$UTS run-search.scm 2>&1 | grep -v '\"' > output.test && diff -q output.test output.reference")))
  (check "search output matches reference" 0 diff-result))

;; Clean up
(%system "rm -f output.test")

(test-summary)
