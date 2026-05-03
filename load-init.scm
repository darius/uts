(let ()

  (define %error error)

  (include "opcodes.scm")
  (include "primcodes.scm")

  (include "abcs/compiler.scm")

  ;; N.B. since these new types (code and closure objects)
  ;; are not disjoint in representation from vectors,
  ;; you need to test for them before you test for vector type.

  ;; -- adaptor just for Chez --
  (define closure-tag (list 'closure))

  (define (%closure? x)
    (and (vector? x)
         (= (vector-length x) 3)
         (eq? (vector-ref x 0) closure-tag)))

  (define (%make-closure lex-env code)
    (vector closure-tag lex-env code))

  (define (vector-ref-at i)
    (lambda (vec) (vector-ref vec i)))

  (define %closure->lex-env (vector-ref-at 1))
  (define %closure->code    (vector-ref-at 2))
  ;; ----

  (include "build-init.scm")

  (main "new-init.c"

        ;; TODO these could be moved to just before compiler.scm
        "opcodes.scm"
        "primcodes.scm"

        "abcs/primitives.scm"
        "abcs/read.scm"
        "abcs/compiler.scm"
        "abcs/dev-env.scm"
        )
  )
