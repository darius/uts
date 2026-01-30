;;; Tests for consp

(load "../test-support.scm")
(load "loadme.scm")  ; loads tests.scm at the end

(test-section "Consp capability-secure language")

;; Run the built-in tests (mail, factory, voting, betting)
(display "  Running do-tests (mail, factory, voting, betting)...") (newline)
(let ((result (do-tests)))
  (check "do-tests returns success" "All tests passed" result))

(test-summary)
