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

(test-summary)
