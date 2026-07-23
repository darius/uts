;;;; r4rs-supplement.scm - Additional R4RS tests for functions not in Jaffer's suite
;;;; Tests math functions, edge cases, etc.
;;;;
;;;; UTS-specific behaviors (deviations from R4RS):
;;;;   - number->string uses uppercase hex (R4RS specifies lowercase)
;;;;
;;;; Known UTS limitations (not tested here):
;;;;   - No complex numbers (make-rectangular, real-part, etc.)
;;;;   - No rationals (numerator, denominator, rationalize)
;;;;   - No delay/force (requires macros)
;;;;   - No char-ready?, transcript-on/off

;;; (provides test, SECTION, report-errs)
(load "testlib.scm")

;; Approximate equality for floating point
(define (approx= a b tolerance)
  (< (abs (- a b)) tolerance))

(define (test-approx expect fun . args)
  (write (cons fun args))
  (display "  ==> ")
  (let ((res (apply fun args)))
    (write res)
    (newline)
    (cond ((not (approx= expect res 1e-9))
           (record-error (list res expect (cons fun args)))
           (display " BUT EXPECTED approximately ")
           (write expect)
           (newline)
           #f)
          (else #t))))

;;;
;;; FLOOR, CEILING, TRUNCATE, ROUND
;;; Note: These return exact when given exact, inexact when given inexact
;;;

(SECTION 'floor-ceiling-truncate-round)

;; floor - largest integer <= x
(test 3 floor 3)
(test 3. floor 3.0)
(test 3. floor 3.5)
(test 3. floor 3.9)
(test -4. floor -3.1)
(test -4. floor -3.9)
(test 0. floor 0.5)
(test -1. floor -0.5)

;; ceiling - smallest integer >= x
(test 4 ceiling 4)
(test 4. ceiling 3.1)
(test 4. ceiling 3.9)
(test -3. ceiling -3.1)
(test -3. ceiling -3.9)
(test 1. ceiling 0.5)
(test 0. ceiling -0.5)

;; truncate - toward zero
(test 3 truncate 3)
(test 3. truncate 3.9)
(test -3. truncate -3.9)
(test 0. truncate 0.5)
(test 0. truncate -0.5)

;; round - to nearest, ties to even
(test 3 round 3)
(test 4. round 3.5)  ; ties go to even
(test 4. round 3.6)
(test 3. round 3.4)
(test 4. round 4.5)  ; 4.5 -> 4 (even)
(test -4. round -3.5)
(test -4. round -4.5)
(test 0. round 0.4)
(test 0. round -0.4)

;;;
;;; SQRT (always returns inexact)
;;;

(SECTION 'sqrt)

(test 0. sqrt 0)
(test 1. sqrt 1)
(test 2. sqrt 4)
(test 3. sqrt 9)
(test 10. sqrt 100)
(test-approx 1.4142135623730951 sqrt 2)
(test-approx 1.7320508075688772 sqrt 3)
(test 0. sqrt 0.0)
(test-approx 0.5 sqrt 0.25)

;;;
;;; EXPT
;;;

(SECTION 'expt)

;; Integer exponents
(test 1 expt 2 0)
(test 2 expt 2 1)
(test 4 expt 2 2)
(test 8 expt 2 3)
(test 1024 expt 2 10)
(test 1 expt 0 0)  ; debatable but common
(test 0 expt 0 1)
(test 0 expt 0 10)
(test 1 expt 1 1000)
(test 1 expt -1 0)
(test -1 expt -1 1)
(test 1 expt -1 2)
(test -1 expt -1 3)
(test 27 expt 3 3)
(test 81 expt 3 4)

;; Exact results above 2^53: must not be rounded through a double.
;; These fit in a 62-bit fixnum but not in a double's mantissa.
;; Known bug: expt uses pow(); see notes/code-review-2026-07.md item A1.
(test 1350851717672992089 expt 3 38)
(test 1490116119384765625 expt 5 26)
(test 0 - (expt 3 38) (* 3 (expt 3 37)))
(test 1152921504606846976 expt 2 60)  ; near fixnum limit, exactly a power of 2
(test #t inexact? (expt 2 62))        ; overflows fixnums: falls back to flonum

;; Negative exponents (result is float)
(test-approx 0.5 expt 2 -1)
(test-approx 0.25 expt 2 -2)
(test-approx 0.125 expt 2 -3)

;; Float base
(test-approx 2.25 expt 1.5 2)
(test-approx 15.625 expt 2.5 3)

;; Float exponent
(test 2. expt 4 0.5)  ; sqrt(4)
(test-approx 2.0 expt 8 0.3333333333333333)  ; cube root of 8

;;;
;;; EXP and LOG
;;;

(SECTION 'exp-log)

(test 1. exp 0)
(test-approx 2.718281828459045 exp 1)  ; e
(test-approx 7.38905609893065 exp 2)   ; e^2
(test-approx 0.36787944117144233 exp -1) ; 1/e

(test 0. log 1)
(test-approx 1.0 log 2.718281828459045)  ; log(e) = 1
(test-approx 2.302585092994046 log 10)   ; log(10)
(test-approx 0.6931471805599453 log 2)   ; log(2)

;; exp and log are inverses
(test-approx 5.0 exp (log 5))
(test-approx 5.0 log (exp 5))

;;;
;;; TRIGONOMETRIC FUNCTIONS
;;;

(SECTION 'trig)

(define pi 3.141592653589793)
(define pi/2 1.5707963267948966)
(define pi/4 0.7853981633974483)
(define pi/6 0.5235987755982989)

;; sin
(test 0. sin 0)
(test-approx 1.0 sin pi/2)
(test-approx 0.0 sin pi)
(test-approx -1.0 sin (* 3 pi/2))
(test-approx 0.5 sin pi/6)
(test-approx 0.7071067811865476 sin pi/4)

;; cos
(test 1. cos 0)
(test-approx 0.0 cos pi/2)
(test-approx -1.0 cos pi)
(test-approx 0.0 cos (* 3 pi/2))
(test-approx 0.8660254037844387 cos pi/6)
(test-approx 0.7071067811865476 cos pi/4)

;; tan
(test 0. tan 0)
(test-approx 1.0 tan pi/4)
(test-approx 0.5773502691896257 tan pi/6)  ; 1/sqrt(3)

;; sin^2 + cos^2 = 1
(let ((x 1.234))
  (test-approx 1.0 + (* (sin x) (sin x)) (* (cos x) (cos x))))

;;;
;;; INVERSE TRIGONOMETRIC FUNCTIONS
;;;

(SECTION 'inverse-trig)

;; asin
(test 0. asin 0)
(test-approx pi/2 asin 1)
(test-approx (- pi/2) asin -1)
(test-approx pi/6 asin 0.5)

;; acos
(test-approx pi/2 acos 0)
(test 0. acos 1)
(test-approx pi acos -1)
(test-approx pi/6 acos 0.8660254037844387)

;; atan (one argument)
(test 0. atan 0)
(test-approx pi/4 atan 1)
(test-approx (- pi/4) atan -1)

;; atan (two arguments) - atan2(y, x)
(test 0. atan 0 1)           ; atan2(0, 1) = 0
(test-approx pi/2 atan 1 0)  ; atan2(1, 0) = pi/2
(test-approx pi/4 atan 1 1)  ; atan2(1, 1) = pi/4
(test-approx pi atan 0 -1)   ; atan2(0, -1) = pi
(test-approx (- pi/4) atan -1 1)  ; atan2(-1, 1) = -pi/4

;; asin and sin are inverses (within domain)
(test-approx 0.5 sin (asin 0.5))
(test-approx 0.5 asin (sin 0.5))

;;;
;;; ABS edge cases
;;;

(SECTION 'abs-edge-cases)

(test 0 abs 0)
(test 1 abs 1)
(test 1 abs -1)
(test 0.0 abs 0.0)
(test 0.0 abs -0.0)
(test 3.14 abs 3.14)
(test 3.14 abs -3.14)

;;;
;;; MODULO and REMAINDER edge cases
;;;

(SECTION 'modulo-remainder-edge)

;; Signs of modulo: result has sign of divisor
(test 1 modulo 13 4)
(test 3 modulo -13 4)
(test -3 modulo 13 -4)
(test -1 modulo -13 -4)

;; Signs of remainder: result has sign of dividend
(test 1 remainder 13 4)
(test -1 remainder -13 4)
(test 1 remainder 13 -4)
(test -1 remainder -13 -4)

;; Zero dividend
(test 0 modulo 0 5)
(test 0 remainder 0 5)

;;;
;;; MIN and MAX with mixed types
;;;

(SECTION 'min-max-mixed)

(test 1 min 1 2 3)
(test 1. min 1.0 2 3)
(test 1. min 1 2.0 3)
(test -3. min 1 2 -3.0)
(test 3 max 1 2 3)
(test 3. max 1.0 2 3)
(test 3. max 1 2 3.0)

;;;
;;; GCD and LCM edge cases
;;;

(SECTION 'gcd-lcm-edge)

(test 0 gcd)
(test 5 gcd 5)
(test 5 gcd -5)
(test 5 gcd 0 5)
(test 5 gcd 5 0)
(test 1 gcd 7 13)
(test 6 gcd 12 18 24)

(test 1 lcm)
(test 5 lcm 5)
(test 5 lcm -5)
(test 0 lcm 0 5)
(test 0 lcm 5 0)
(test 91 lcm 7 13)
(test 12 lcm 4 6)
(test 72 lcm 12 18 24)

;;;
;;; NUMBER->STRING and STRING->NUMBER edge cases
;;;

(SECTION 'number-string-edge)

(test "0" number->string 0)
(test "-1" number->string -1)
;; TODO resolve the following claim from Claude which I couldn't confirm in R4RS:
;;   R4RS specifies lowercase: "ff", but UTS returns "FF"
;;   Uncomment to test R4RS compliance:
;;   (test "ff" number->string 255 16)
(test "FF" number->string 255 16)  ; UTS-specific: uppercase
(test "377" number->string 255 8)
(test "11111111" number->string 255 2)

(test 0 string->number "0")
(test -1 string->number "-1")
(test 255 string->number "ff" 16)
(test 255 string->number "FF" 16)
(test 255 string->number "377" 8)
(test 255 string->number "11111111" 2)
(test #f string->number "")
(test #f string->number "abc")
(test #f string->number "12.34.56")
(test #f string->number (make-string 2000 #\1))  ; UTS-specific: no bignums, and too long to parse as a float
(test 1e300 string->number (make-string 300 #\9))  ; UTS-specific: not too long, but inexact (TODO is this actually compliant?)

;;;
;;; INTEGER? with floats
;;;

(SECTION 'integer-pred)

(test #t integer? 3)
(test #t integer? -3)
(test #t integer? 0)
(test #t integer? 3.0)  ; exact integer value
(test #f integer? 3.5)
(test #t integer? -0.0)

;;;
;;; NEGATIVE?, POSITIVE?, ZERO? edge cases
;;;

(SECTION 'sign-predicates)

(test #t zero? 0)
(test #t zero? 0.0)
(test #t zero? -0.0)
(test #f zero? 1)
(test #f zero? -1)

(test #t positive? 1)
(test #t positive? 0.001)
(test #f positive? 0)
(test #f positive? -1)

(test #t negative? -1)
(test #t negative? -0.001)
(test #f negative? 0)
(test #f negative? 1)

;;;
;;; LIST operations edge cases
;;;

(SECTION 'list-edge)

(test '() reverse '())
(test '(1) reverse '(1))
(test '() append)
(test '(1) append '(1))
(test '(1 2) append '(1) '(2))
(test '(1 . 2) append '() '(1 . 2))
(test 'a append '() 'a)

(test '() list-tail '(1 2 3) 3)
(test '(3) list-tail '(1 2 3) 2)
(test '(1 2 3) list-tail '(1 2 3) 0)

;;;
;;; STRING operations edge cases
;;;

(SECTION 'string-edge)

(test "" make-string 0)
(test "" make-string 0 #\x)
(test "xxx" make-string 3 #\x)
(test 0 string-length "")
(test "" substring "hello" 0 0)
(test "h" substring "hello" 0 1)
(test "hello" substring "hello" 0 5)
(test "" string-append)
(test "hello" string-append "hello")
(test "helloworld" string-append "hello" "world")
(test "" list->string '())
(test '() string->list "")

;;;
;;; VECTOR operations edge cases
;;;

(SECTION 'vector-edge)

(test '#() make-vector 0)
(test '#() make-vector 0 'x)
(test '#(x x x) make-vector 3 'x)
(test 0 vector-length '#())
(test '#() vector)
(test '#(1) vector 1)
(test '() vector->list '#())
(test '#() list->vector '())

;;;
;;; CHAR comparisons
;;;

(SECTION 'char-compare)

(test #t char<? #\a #\b)
(test #f char<? #\b #\a)
(test #f char<? #\a #\a)
(test #t char<=? #\a #\a)
(test #t char<=? #\a #\b)
(test #t char>? #\b #\a)
(test #t char>=? #\a #\a)
(test #t char=? #\a #\a)
(test #f char=? #\a #\A)

;; case-insensitive
(test #t char-ci=? #\a #\A)
(test #t char-ci<? #\a #\B)
(test #t char-ci<? #\A #\b)

;;;
;;; CHAR predicates and conversions
;;;

(SECTION 'char-pred-conv)

(test #t char-alphabetic? #\a)
(test #t char-alphabetic? #\Z)
(test #f char-alphabetic? #\0)
(test #f char-alphabetic? #\space)

(test #t char-numeric? #\0)
(test #t char-numeric? #\9)
(test #f char-numeric? #\a)

(test #t char-whitespace? #\space)
(test #t char-whitespace? #\newline)
(test #f char-whitespace? #\a)

(test #t char-upper-case? #\A)
(test #f char-upper-case? #\a)
(test #t char-lower-case? #\a)
(test #f char-lower-case? #\A)

(test #\A char-upcase #\a)
(test #\A char-upcase #\A)
(test #\a char-downcase #\A)
(test #\a char-downcase #\a)

(test 65 char->integer #\A)
(test 97 char->integer #\a)
(test #\A integer->char 65)

;;;
;;; APPLY edge cases
;;;

(SECTION 'apply-edge)

(test '() apply list '())
(test '(1) apply list '(1))
(test '(1 2 3) apply list 1 '(2 3))
(test '(1 2 3) apply list 1 2 '(3))
(test 6 apply + '(1 2 3))
(test 6 apply + 1 '(2 3))

;;;
;;; MAP and FOR-EACH edge cases
;;;

(SECTION 'map-for-each-edge)

(test '() map car '())
(test '(1) map car '((1 2)))
(test '() map + '() '())
(test '(5 7 9) map + '(1 2 3) '(4 5 6))

;; for-each return value is unspecified, just verify no error
(for-each (lambda (x) x) '())
(for-each (lambda (x) x) '(1 2 3))
(for-each (lambda (x y) (+ x y)) '(1 2) '(3 4))

;;;
;;; Done
;;;

(report-errs)
(raise-errs-to-os)
