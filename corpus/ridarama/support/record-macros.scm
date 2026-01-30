; Defined since '. isn't portable
(define dot (string->symbol "."))

;; Define a record type with the given tag and slots.
;; Pre: SLOTS is a list of distinct symbols.
;;
;; E.g. (define-record point (x y)) defines
;;   point?, make-point, point.x, and point.y, where
;;   (point? x) returns true iff X is a point
;;   (make-point a b) returns a new point with X=a and Y=b.
;;   (point.x p) returns the X slot of P.
;;   (point.y p) returns the Y slot of P.
;;   (point.x! p nx) changes the X slot of P to NX.
;;   (point.y! p ny) changes the Y slot of P to NY.
;; These definitions use the record representation of record.scm.
;;
(define-macro 'define-record
  (lambda (tag slots)
    (let ((arity (length slots)))
      `(begin
	 (define ,(concat-symbol 'make- tag) (record-maker ',tag ,arity))
	 (define ,(concat-symbol tag '?) (record-predicate ',tag))

	 ,@(map (lambda (slot index)
                  `(define ,(concat-symbol tag dot slot '!)
                     (record-mutator ',tag ,index)))
                slots
                (iota arity))

	 ,@(map (lambda (slot index)
		  `(define ,(concat-symbol tag dot slot)
		     (record-accessor ',tag ,index)))
		slots
		(iota arity))))))

;; Dispatch on the type of SUBJECT and deconstruct it in the CLAUSES.
;; E.g. (variant-case foo 
;;        (point (x y) (+ x y))
;;        (else 0))
;; evaluates (+ X Y) if FOO is a POINT-record, with X and Y bound to
;; the slots named X and Y in FOO; otherwise it returns 0.
;;
;; A clause can also look like a CASE clause; such clauses are 
;; treated like a CASE on the tag of the record.
;; E.g. (define (point? obj)
;;        (record-case obj
;;          ((point2d point3d) #t)
;;          (else #f)))
;;
;; If there's no else-clause, an error is raised if SUBJECT matches 
;; none of the clauses.
(define-macro 'variant-case
  (lambda (subject . clauses)
    (let ((subject-var (generate-symbol '-subject)))

      (define expand-clause
	(lambda (clause)
	  (if (or (not (pair? clause))
		  (not (symbol? (car clause)))
		  (eq? (car clause) 'else))
	      clause
	      (let ((tag (car clause))
		    (slots (cadr clause))
		    (body (cddr clause)))
		`((,tag)
		  (let ,(map (lambda (slot)
			       `(,slot (,(concat-symbol tag dot slot) 
					,subject-var)))
			     slots)
		    ,@body))))))

      `(let ((,subject-var ,subject))
	 (case (record-tag ,subject-var)
	   ,@(map expand-clause clauses)
	   ,@(if (starts-with? 'else (last clauses))
		 '()
		 (list `(else (panic "No matching record-case clause" 
				     ,subject-var)))))))))

;; Return a new symbol with a prefix of NAME, different from all
;; others generated so far.
(define generate-symbol
  (lambda (name)
    (set! generate-symbol-counter (+ generate-symbol-counter 1))
    (concat-symbol name '- generate-symbol-counter)))

(define generate-symbol-counter 0)
