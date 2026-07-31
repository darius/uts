;;;; Misc further tests not under the R4RS umbrella

;;; (provides test, SECTION, report-errs)
(load "testlib.scm")

(define (itself x) x)

(SECTION 'char-literals)

(test #\x41 itself #\A)


;;;
;;; Done
;;;

(report-errs)
(raise-errs-to-os)
