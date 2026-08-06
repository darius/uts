;;;; Misc further tests not under the R4RS umbrella

;;; (provides test, SECTION, report-errs)
(load "testlib.scm")

(define (itself x) x)

(SECTION 'char-literals)

(test #\x41 itself #\A)
(test "a\xf;\n\t\rz" string #\a (integer->char 15) #\newline #\tab #\return #\z)


;;;
;;; Done
;;;

(report-errs)
(raise-errs-to-os)
