;;; Tests for miasma

(load "../test-support.scm")
(load "loadme.scm")

(test-section "Miasma x86 assembler generator")

;; Generate the C assembler header
(display "  Generating c/asm.h.test...") (newline)
(generate-c-assembler "c/asm.h.test")

;; Compare against reference
(display "  Comparing against reference...") (newline)
(let ((diff-result (%system "diff -q c/asm.h.test c/asm.h.reference")))
  (check "generated matches reference" 0 diff-result))

;; Clean up
(%system "rm -f c/asm.h.test")

(test-summary)
