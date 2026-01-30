;;; Tests for miasma

(load "../test-support.scm")
(load "loadme.scm")

(test-section "Miasma x86 assembler generator")

;; Generate the C assembler header and verify it was created
(display "  Generating c/asm.h.test...") (newline)
(generate-c-assembler "c/asm.h.test")

;; Verify the file was created by trying to open it
(call-with-input-file "c/asm.h.test"
  (lambda (port)
    (let ((c (read-char port)))
      (check "generated header not empty"
             #t
             (char? c)))))

(display "  Generated and verified C assembler header") (newline)

;; Note: can't easily delete the test file without delete-file primitive

(test-summary)
