;;;; 
;;;; More miscellany
;;;;

(define unspecified (string->symbol "#!unspecified"))

(define (%reduce fn id lst)
  (let loop ((lst lst))
       (if (null? lst)
	   id
	   (fn (car lst)
	       (loop (cdr lst))))))

(define (andmap test? ls)
  (if (null? ls)
      #t
      (and (test? (car ls))
	   (andmap test? (cdr ls)))))


;;;;
;;;; lex-env.scm
;;;;

(begin

  (define make-lexical-address   cons)
  (define lexical-address/depth  car)
  (define lexical-address/offset cdr)

  ;; Return the lexical address, or v itself if free.
  (define (lexical-env/lookup s v)
    (let nesting ((depth 0)
		  (s s))
      (if (null? s)
	  v
	  (let searching ((vars (car s))
			  (index 0))
	    (cond ((null? vars)
		   (nesting (+ depth 1) (cdr s)))
		  ((eq? (car vars) v)
                   (if (< 255 depth) (%error "Code too complex: nesting too deep"))
                   (if (< 255 index) (%error "Code too complex: too many locals"))
		   (make-lexical-address depth index))
		  (else
		   (searching (cdr vars) (+ index 1))))))))

  (define lexical-env/empty '())

  (define (lexical-env/extend s vals)
    (cons vals s))

  ;; Get the name behind a lexical address.
  ;; If the address is not mapped, return '<?>
  ;; -- that's not expected to come up, but I want this
  ;; to be robust to stripping debug info.
  (define (locals-map-ref locals-map depth offset)
    (list-ref/default (list-ref/default locals-map depth '())
                      offset
                      '<?>))

  (define (list-ref/default ls n default)
    (let loop ((ls ls) (n n))
      (cond ((null? ls) default)
            ((= n 0) (car ls))
            (else (loop (cdr ls) (- n 1)))))))


;;;;
;;;; parse.scm
;;;;

(begin

  ;;; Macros

  (define %macroexpanders '())

  (define (%get-macroexpander symbol)
    (cond ((assq symbol %macroexpanders) => cdr)
          (else #f)))

  (define (%define-macro symbol expander)
    (set! %macroexpanders
          (cons (cons symbol expander) %macroexpanders)))

  (define (%macroexpander form)
    (and (pair? form)
         (symbol? (car form))
         (%get-macroexpander (car form))))

  (define (%macroexpand-1 form)
    (cond ((%macroexpander form) => (lambda (expand) (apply expand (cdr form))))
          (else form)))


  ;;;
  ;;; Constants tables
  ;;;

  (define (constants/new)
    (cons 0 '()))

  (define (constants/lookup datum constants)
    (cond ((assv datum (cdr constants))
	   => cdr)
	  (else
	   (let ((c (car constants)))
             (if (<= 255 c) (%error "Code too complex: too many constants"))
	     (set-car! constants (+ c 1))
	     (set-cdr! constants (cons (cons datum c) (cdr constants)))
	     c))))

  (define (constants->vector constants)
    (let ((vec (make-vector (car constants) #f)))
      (for-each (lambda (pair)
		  (vector-set! vec (cdr pair) (car pair)))
		(cdr constants))
      vec))


  ;;;
  ;;; Bytecode assembler ("lap", traditional name for Lisp assembler)
  ;;;

  (define lap/position length)
  (define lap/append append)

  (define lap/restore 
    (list %bop-restore))

  (define (lap/offset pos lap)
    (let ((offset (- (lap/position lap) pos)))
      (cons (quotient offset 256)
	    (cons (remainder offset 256)
		  lap))))

  (define (lap/jump pos lap)
    (cons %bop-jump
	  (lap/offset pos lap)))

  (define (lap/unless pos lap)
    (cons %bop-unless
	  (lap/offset pos lap)))

  (define (lap/var addr lap)
    (cons %bop-var
	  (cons (lexical-address/depth addr)
		(cons (lexical-address/offset addr)
		      lap))))

  (define (lap/var! addr lap)
    (cons %bop-var!
	  (cons (lexical-address/depth addr)
		(cons (lexical-address/offset addr)
		      lap))))

  (define (lap/params count lap)
    (cons %bop-params
	  (cons count lap)))

  (define (lap/&rest-params count lap)
    (cons %bop-&rest-params
	  (cons count lap)))

  (define (lap/save pos lap)
    (cons %bop-save 
	  (lap/offset pos lap)))

  (define (lap/invoke lap)
    (cons %bop-invoke lap))

  (define (lap/drop lap)
    (cons %bop-drop lap))

  (define (lap/prim-0 prim lap)
    (cons %bop-prim-0
	  (cons prim lap)))

  (define (lap/prim-1 prim lap)
    (cons %bop-prim-1
	  (cons prim lap)))

  (define (lap/prim-2 prim lap)
    (cons %bop-prim-2
	  (cons prim lap)))

  (define (lap/prim-3 prim lap)
    (cons %bop-prim-3
	  (cons prim lap)))

  (define (lap/lit datum constants lap)
    (cons %bop-lit
	  (cons (constants/lookup datum constants) 
		lap)))

  (define (lap/glo symbol constants lap)
    (cons %bop-glo
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/glo! symbol constants lap)
    (cons %bop-glo!
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/define symbol constants lap)
    (cons %bop-define
	  (cons (constants/lookup symbol constants) 
		lap)))

  (define (lap/proc code constants lap)
    (cons %bop-proc
	  (cons (constants/lookup code constants)
		lap)))


  (define gensym
    (let ((counter 0))
      (lambda ()
	(set! counter (+ counter 1))
	(string->symbol
	 (string-append "#!g_" (number->string counter))))))

  (define *open-code-primitives?* #t)

  ;; List of global functions to spare from tail call optimization, so the 
  ;; caller's frame stays visible on the stack. User-settable.
  (define %dont-tail-on-me '(error %error %avast))


  ;; (PARSE-FORM form) returns a compiled code vector for a top-level form.
  ;; The code will assume no params and an empty lexical environment ("top-level").
  (define parse-form 
    (let ()

      ;; The expression parser.

      (define (parse-exp constants label exp s k)

	(let pe ((exp exp) (k k))

	  (cond

	    ((symbol? exp) 
	     (let ((addr (lexical-env/lookup s exp)))
	       (if (symbol? addr) 
		   (lap/glo addr constants k)
		   (lap/var addr k))))

	    ((not (pair? exp))
	     (lap/lit exp constants k))

	    (else
	     (let ((rator (car exp)) 
		   (rands (cdr exp))
		   (num-rands (length (cdr exp)))
		   (assert (lambda (ok?) 
			     (if (not ok?) (syntax-error "Bad syntax" exp)))))

	       (case rator

		 ((quote)
		  (assert (= num-rands 1))
		  (lap/lit (car rands) constants k))

		 ((if)
		  (assert (or (= num-rands 2) 
			      (= num-rands 3)))
		  (let ((consequent (cadr rands))
			(alternative (if (= num-rands 3) 
					 (caddr rands) 
					 `',unspecified)))
		    (pe (car rands)
			(if (eq? k lap/restore)
			    (let ((e (pe alternative k)))
			      (lap/unless (lap/position e)
					  (lap/append (pe consequent k)
						      e)))
			    (let ((j (lap/position k))
				  (e (pe alternative k)))
			      (lap/unless (lap/position e)
					  (pe consequent 
					      (lap/jump j e))))))))

		 ((lambda)
		  (assert (and (< 1 num-rands)
			       (valid-formals? (car rands))))
		  (let* ((formals (car rands))
			 (rest-args? (not (list? formals)))
			 (fixed-formals (if (not rest-args?)
					    formals
					    (let sans-dot ((f formals))
					      (if (pair? f)
						  (cons (car f) (sans-dot (cdr f)))
						  (list f))))))
		    (let ((body-constants (constants/new))
                          (var-count (length fixed-formals))
                          (nest-s (lexical-env/extend s fixed-formals)))
		      (let ((lap (parse-exp body-constants
					    label 
					    (expand-lambda-body (cdr rands))
					    nest-s
					    lap/restore)))
			(lap/proc
			 (codify (if rest-args?
				     (lap/&rest-params (- var-count 1) lap)
				     (lap/params var-count lap))
				 (constants->vector body-constants)
				 label
                                 nest-s)
			 constants
			 k)))))

		 ((let)
		  (assert (pair? rands))
		  (pe 
		   (if (symbol? (car rands)) ; named-let form
		       (begin
			 (assert (valid-let? (cdr rands)))
			 (let ((proc (car rands)) 
			       (decls (cadr rands)) 
			       (body (cddr rands)))
			   `((letrec ((,proc (lambda ,(map car decls) . ,body)))
			       ,proc)
			     . ,(map cadr decls))))
		       (begin
			 (assert (valid-let? rands))
			 (let ((names (map car (car rands)))
			       (exps (map cadr (car rands)))
			       (body (cdr rands)))
			   (if (null? names)
			       (expand-lambda-body body)
			       `((lambda ,names . ,body)
				 . ,(map (lambda (name exp)
					   `(%label ,name ,exp))
					 names
					 exps))))))
		   k))

		 ((letrec)
		  (assert (valid-let? rands))
		  (let ((vars (map car (car rands)))
			(exps (map cadr (car rands)))
			(body (cdr rands)))
		    (pe 
		     `((lambda ,vars 
			 ,@(map (lambda (var exp) 
				  `(set! ,var (%label ,var ,exp)))
				vars 
				exps)
			 (let () . ,body))
		       . ,(map (lambda (var) ''*uninitialized*) vars))
		     k)))

		 ((set!)
		  (assert (and (symbol? (car rands))
			       (= num-rands 2)))
		  (let ((name (car rands))
			(exp (cadr rands)))
		    (let ((addr (lexical-env/lookup s name)))
		      (pe exp
			  (if (symbol? addr) 
			      (lap/glo! addr constants k)
			      (lap/var! addr k))))))

		 ((begin)
		  (if (null? rands)
		      (lap/lit unspecified constants k)
		      (let loop ((head (car rands)) (tail (cdr rands)))
			(pe head
			    (if (null? tail)
				k
				(lap/drop (loop (car tail) (cdr tail))))))))

		 ((let*)
		  (assert (pair? rands))
		  (pe
		   (let ((decls (car rands))
			 (body (cdr rands)))
		     (if (null? decls)
			 `(let () . ,body)
			 `(let (,(car decls))
			    (let* ,(cdr decls) . ,body))))
		   k))

		 ((and)
		  (pe
		   (case num-rands
		     ((0) #t)
		     ((1) (car rands))
		     (else `(if ,(car rands) (and . ,(cdr rands)) #f)))
		   k))

		 ((or)
		  (pe
		   (case num-rands
		     ((0) #f)
		     ((1) (car rands))
		     (else (let ((head (gensym)))
			     `(let ((,head ,(car rands)))
				(if ,head ,head (or . ,(cdr rands)))))))
		   k))

		 ((cond)
		  (pe
		   (cond
		     ((null? rands) `',unspecified)
		     ((not (pair? (car rands)))
		      (syntax-error "Invalid cond clause" (car rands)))
		     ((eq? (caar rands) 'else)
		      (if (null? (cdr rands))
			  `(begin . ,(cdar rands))
			  (syntax-error "Else-clause is not last" rands)))
		     ((null? (cdar rands))
		      `(or ,(caar rands) (cond . ,(cdr rands))))
		     ((and (pair? (cdar rands)) (eq? (cadar rands) '=>))
		      (if (not (and (list? (car rands))
				    (= (length (car rands)) 3)))
			  (syntax-error "Bad cond clause syntax" rands))
		      (let ((test-var (gensym)))
			`(let ((,test-var ,(caar rands)))
			   (if ,test-var
			       (,(caddar rands) ,test-var)
			       (cond . ,(cdr rands))))))
		     (else `(if ,(caar rands) 
				(begin . ,(cdar rands))
				(cond . ,(cdr rands)))))
		   k))

		 ((case)
		  (assert (pair? rands))
		  (pe
		   (let ((test (car rands))
			 (sym (gensym)))
		     `(let ((,sym ,test))
			(cond 
			 . ,(map 
			     (lambda (clause)
			       (cond
				 ((eq? (car clause) 'else)
				  clause)
				 ((null? (cdar clause))
				  `((%primitive eqv?
				     ,sym ',(caar clause)) . ,(cdr clause)))
				 (else
				  `((%primitive memv
				     ,sym ',(car clause)) . ,(cdr clause)))))
			     (cdr rands)))))
		   k))

		 ((do)
		  (assert (and (<= 2 num-rands)
			       (let valid-clauses? ((clauses (car rands)))
				 (if (null? clauses)
				     #t
				     (let ((clause (car clauses)))
				       (and (pair? clause)
					    (symbol? (car clause))
					    (pair? (cdr clause))
					    (if (null? (cddr clause))
						#t
						(null? (cdddr clause)))
					    (valid-clauses? (cdr clauses))))))
			       (pair? (cadr rands))))
		  (pe
		   (let ((loop (gensym))
			 (variables (map car (car rands)))
			 (inits (map cadr (car rands)))
			 (steps (map (lambda (clause)
				       (if (null? (cddr clause))
					   (car clause) ;default step leaves var
							;unchanged.
					   (caddr clause)))
				     (car rands)))
			 (test (caadr rands))
			 (result (cdadr rands))
			 (body (cddr rands)))
		     `(letrec ((,loop 
				(lambda ,variables
				  (if ,test
				      (begin . ,result)
				      (begin 
					,@body
					(,loop . ,steps))))))
			(,loop . ,inits)))
		   k))

		 ((quasiquote)
		  (assert (= num-rands 1))
		  (pe
		   (expand-quasiquote (car rands) 0)
		   k))

		 ((define)
		  (assert #f)) ; (define ...) is never a valid expression

                 ((%defmacro)
                  (assert (and (<= num-rands 3) (symbol? (car rands))))
                  (pe `(%define-macro ',(car rands)
                                      (%label ,(car rands) (lambda ,(cadr rands) ,@(cddr rands))))
                      k))

		 ((%label)
		  (assert (= num-rands 2))
                  ;; (%label NAME EXP) in a context labeled FOO
                  ;;  parses EXP in a context labeled (NAME . FOO).
                  ;;  This labels any closures created in EXP, for display
                  ;;  by put_object in utsvm.c, case a_closure.
		  (parse-exp constants
			     (cons (car rands) label)
			     (cadr rands)
			     s
			     k))

		 ((%primitive)
		  (assert (and (<= 1 num-rands) (<= num-rands 4)))
                  (let* ((nr (- num-rands 1))
                         (prim (prim-lookup (car rands) nr)))
                    (parse-prim-app pe prim (cdr rands) nr k)))

                 (else
                   (cond
                    ((symbol? rator)
                     (cond ((%get-macroexpander rator)
                            ;; macro
                            ;; (N.B. this case deliberately overrides the rator-is-bound case)
                            => (lambda (expand)
                                 (pe (apply expand rands) k)))
		           ((symbol? (lexical-env/lookup s rator))
                            ;; rator is not locally bound -- application of global variable
                            (cond ((and *open-code-primitives?*
		                        (prim-lookup rator num-rands))
                                   ;; open-coded primitive app
		                   => (lambda (prim)
		                        (parse-prim-app pe prim rands num-rands k)))
                                  (else
                                   ;; ordinary app, possibly with TCO suppressed
                                   (parse-call pe (not (memq rator %dont-tail-on-me)) rator rands k))))
                           (else
                            ;; ordinary app
                            (parse-call pe #t rator rands k))))
		    (else
                     (parse-call pe #t rator rands k))))))))))


      ;; Calls and primitive applications

      (define (parse-call pe tail-ok? rator rands k)
	(let ((lin (lambda (k2)
		     (%reduce pe 
			      (pe rator 
				  (lap/invoke k2))
			      rands))))
	  (if (and tail-ok? (eq? k lap/restore))
	      (lin '())
	      (lap/save (lap/position k)
			(lin k)))))

      (define (parse-prim-app pe prim rands num-rands k)
        (let ((primop-k ((case num-rands
		           ((0) lap/prim-0)
		           ((1) lap/prim-1)
		           ((2) lap/prim-2)
		           ((3) lap/prim-3))
	                 prim
	                 k)))
          (%reduce pe primop-k rands)))

      (define (prim-lookup sym num-rands)
        (cond ((assq sym (case num-rands
			   ((0) prim-0-list)
			   ((1) prim-1-list)
			   ((2) prim-2-list)
			   ((3) prim-3-list)
			   (else '())))
               => cadr)
              (else #f)))


      ;;
      ;; Lambdas and defines
      ;;

      ; True iff formals is a (proper or improper) list of symbols, all distinct.
      (define (valid-formals? formals) 
	(let loop ((formals formals) (vars '()))
	  (define (valid-var? formal)
	    (and (symbol? formal)
		 (not (memq formal vars))))
	  (or (null? formals)
	      (valid-var? formals)
	      (and (pair? formals)
		   (valid-var? (car formals))
		   (loop (cdr formals)
			 (cons (car formals) vars))))))   ;** avoid this consing

      ; sexps is a list of expressions forming a lambda body.
      ; Return a single equivalent expression with any internal defines 
      ; made into a letrec.
      (define (expand-lambda-body sexps)
	(scan-out-defines sexps
	  (lambda (vars defs body)
	    `(letrec ,(map list vars defs) . ,body))
	  (lambda (body) `(begin . ,body))))

      ; Find leading definitions in sexps; 
      ; if none, return (BODY-k sexps);
      ; if any, return (DEF-K vars defs body)
      ;         where vars is a list of all variables so defined,
      ;               defs is the corresponding value expressions,
      ;           and body is the remaining sexps.
      (define (scan-out-defines sexps def-k body-k)
	(if (null? sexps) 
	    (syntax-error "Lambda expression has no body")
	    (maybe-parse-definition (car sexps)
	      (lambda (vars1 exps1)
		(scan-out-defines (cdr sexps)
		  (lambda (vars exps body) 
		    (def-k (append vars1 vars) (append exps1 exps) body))
		  (lambda (body)
		    (def-k vars1 exps1 body))))
	      (lambda ()
		(body-k sexps)))))

      ; If sexp is a definition, return (SUCCEED vars defs) with vars and
      ; defs as in scan-out-defines; else return (FAIL).
      (define (maybe-parse-definition sexp succeed fail)
	(cond
	  ((not (pair? sexp)) (fail))
	  ((eq? (car sexp) 'define)
	   (parse-define sexp succeed))
	  ((eq? (car sexp) 'begin)
	   (let loop ((defs (cdr sexp)) (succeed succeed) (fail fail))
	     (cond
	       ((null? defs) (succeed '() '()))
	       ((not (pair? defs)) (fail))
	       (else
		(maybe-parse-definition (car defs)
		  (lambda (vars1 exps1)
		    (loop (cdr defs)
		      (lambda (vars exps)
			(succeed (append vars1 vars) (append exps1 exps)))
		      (lambda () 
			(syntax-error "Invalid definition" sexp))))
		  fail)))))
	  (else (fail))))

      ; Pre: (and (pair? sexp) (eq? (car sexp) 'define))
      ; Return (K vars defs), where vars and defs are as above.
      ; (But note they're always 1-element lists...)
      (define (parse-define sexp k)
	(cond
	  ((or (not (list? sexp))
	       (< (length sexp) 3))
	   (syntax-error "Invalid definition" sexp))
	  ((symbol? (cadr sexp))
	   (if (= (length sexp) 3)
	       (k (list (cadr sexp)) (list (caddr sexp)))
	       (syntax-error "Invalid definition" sexp)))
	  ((and (pair? (cadr sexp))
		(valid-formals? (cdadr sexp)))
	   (k (list (car (cadr sexp)))
	      (list `(lambda ,(cdadr sexp) . ,(cddr sexp)))))
	  (else (syntax-error "Invalid definition" sexp))))


      ;; LET forms

      ; True iff `(LET ,rands) is a valid let-expression (simple, not 
      ; named LET).  This is helpful in parsing named LET and LETREC, too.
      (define (valid-let? rands)
	(and (<= 2 (length rands))
	     (list? (car rands))
	     (andmap (lambda (decl) 
			(and (pair? decl)
			     (pair? (cdr decl))
			     (null? (cddr decl))
			     (symbol? (car decl))))
		      (car rands))
	     (valid-formals? (map car (car rands)))))


      ;; Backquote expansion: based on _Paradigms of AI Programming_ p. 824
      ;; (adapted to expand nested quasiquotes as in R4RS, and for hygiene)

      (define (expand-quasiquote exp nesting)
	(define (assert ok?)
	  (if (not ok?) (syntax-error "Bad syntax" exp)))
	(cond
	 ((vector? exp)
	  `(%primitive list->vector
		       ,(expand-quasiquote (vector->list exp) nesting)))
	 ((not (pair? exp)) 
	  (if (constant? exp) exp (list 'quote exp)))
	 ((eq? (car exp) 'unquote)
	  (assert (= (length exp) 2))
	  (if (= nesting 0)
	      (cadr exp)
	      (combine-skeletons ''unquote 
				 (expand-quasiquote (cdr exp) (- nesting 1))
				 exp)))
	 ((eq? (car exp) 'quasiquote)
	  (assert (= (length exp) 2))
	  (combine-skeletons ''quasiquote 
			     (expand-quasiquote (cdr exp) (+ nesting 1))
			     exp))
	 ((and (pair? (car exp))
	       (eq? (caar exp) 'unquote-splicing))
	  (assert (= (length (car exp)) 2))
	  (if (= nesting 0)
	      (if (null? (cdr exp))
		  (cadar exp)
		  `(%primitive append
			       ,(cadar exp)
			       ,(expand-quasiquote (cdr exp) nesting)))
	      (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
				 (expand-quasiquote (cdr exp) nesting)
				 exp)))
	 (else (combine-skeletons (expand-quasiquote (car exp) nesting)
				  (expand-quasiquote (cdr exp) nesting)
				  exp))))

      (define (combine-skeletons left right exp)
	(define (my-eval constant)
	  (if (pair? constant)       ;; must be quoted constant
	      (cadr constant)
	      constant))
	(if (and (constant? left) (constant? right)) 
	    (if (and (eqv? (my-eval left) (car exp))
		     (eqv? (my-eval right) (cdr exp)))
		(list 'quote exp)
		(list 'quote (cons (my-eval left) (my-eval right))))
	    `(%primitive cons ,left ,right)))

      (define (constant? exp)
	(if (pair? exp)
	    (eq? (car exp) 'quote)
	    (not (symbol? exp))))


      ;; Error handling

      (define (syntax-error message . irritants)
	(%error "Syntax error" message irritants))


      ;; Body of PARSE-FORM

      (lambda (form)
	(let ((constants (constants/new)))
	  (let ((lap
		 (maybe-parse-definition 
		  form
		  (lambda (names exps)
		    (do ((names (reverse names) (cdr names))
			 (exps  (reverse exps)  (cdr exps))
			 (k lap/restore
			    (parse-exp constants
				       (car names)
				       (car exps)
				       lexical-env/empty
				       (lap/define (car names) 
						   constants
						   (if (null? (cdr names))
						       k
						       (lap/drop k))))))
			((null? names) k)))
		  (lambda () 
		    (parse-exp constants '() form lexical-env/empty 
			       lap/restore)))))
	    (codify lap (constants->vector constants) #f lexical-env/empty))))))


  (define (%make-code-vector constants-vec bytes label locals-map)
    (vector 'code-vector constants-vec bytes label locals-map 0))

  (define (codify lap constants-vec label locals-map)
    (%make-code-vector constants-vec
		       (list->string (map integer->char lap))
		       label
                       locals-map))

  (define (%eval form)
    ;; Compile to a top-level procedure with no params, and call it.
    ((%make-closure '#() (parse-form form)))))


;;;; Misc macros
;;;; Of course, these operate in the target system, not in this source file.
(begin

  (%define-macro '%yo ;; crude printf-debugging convenience
                 (lambda rands
                   (if (not (and (pair? rands) (null? (cdr rands))))
                       (%error "Syntax error" "Requires one operand" `(%yo ,@rands)))
                   `(let ((v ,(car rands)))
                      ;; XXX hygiene
                      (display "[%yo ")
                      (write ',(car rands))
                      (display " : ")
                      (write v)
                      (display "]\n")
                      v)))
  )

;;;;
;;;; globals.scm
;;;;

(begin

  (define variable-arity-prim-list
    '(peek-char read-char 
      write-char + - * / < <= = 
      make-vector make-string number->string string->number
      append write display)))


;;;;
;;;; repl.scm
;;;;

(begin

  ;; Loading

  (define (load file)
    (call-with-input-file file
      (lambda (port)
	(let loop ()
	  (let ((exp (read port)))
	    (cond ((not (eof-object? exp))
		   (%eval exp)
		   (loop))))))))

  ;; Read-eval-print loop.

  (define %reset                ; (this definition will be reassigned)
    (lambda (val)
      (display "*** ERROR DURING STARTUP." (current-output-port))
      (newline (current-output-port))
      (%exit 1)))

  (define %arguments-to-scheme '())

  (define (%start-scheming)  ; called by utsvm once this fasl file is loaded
    (set! %arguments-to-scheme (cddr %command-line-arguments)) ; skip past the fasl file
    (cond ((pair? %arguments-to-scheme)
           (set! %reset (lambda (_) (%exit 1)))
	   (load (car %arguments-to-scheme)))
          (else
           (display "Enter an expression. On an error, enter ,d to debug. For more commands: ,help\n")
	   (call/cc (lambda (k) (set! %reset k)))
           (%scheming))))

  (define (%scheming)  ; read-eval-print loop
    ;; In editing the following, stay conscious of what will appear
    ;; in backtraces on error: we want only one frame from this repl.
    (display "-> ")
    (let ((cmd (read)))
      (if (eof-object? cmd)
          (newline)
          (cond ((and (pair? cmd) (eq? (car cmd) 'unquote) (pair? (cdr cmd)) (null? (cddr cmd)))
                 (%comma-command (cadr cmd))
                 (%scheming))
                (else
                 ;; An expression
	         (let ((obj (%eval cmd)))
                   (cond ((not (eq? obj unspecified))
                          (set! %%% %%)
                          (set! %% %)
                          (set! % obj)
	                  (write obj)
	                  (newline)))
	           (%scheming)))))))

  (define (%comma-command cmd)
    (case cmd
      ((help)
       (display ",help        - this message\n")
       (display ",d           - (debug)\n")
       (display ",l name      - (load \"name.scm\")\n")
       (display ",l \"x.scm\"   - (load \"x.scm\")\n")
       (display ",! expr      - evaluate expr for effect, don't print it")
       (display ",time expr   - time the evaluation of expr"))
      ((d)
       (debug))
      ((l)
       (let ((arg (read)))
         (cond ((string? arg) (load arg))
               ((symbol? arg) (load (string-append (symbol->string arg) ".scm")))
               (else (display "usage: ,l \"string\" or ,l symbol\n")))))
      ((!)
       (%eval (read)))
      ((time)
       (let* ((thunk (%eval `(lambda () ,(read))))
              (outcome (%time thunk)))
         (display "Seconds: ") (write (car outcome)) (newline)
         (display "Value: ")   (write (cadr outcome)) (newline)
         ))
      (else
       (display "Unknown ,command. Try ,help\n"))))

  (define (%time thunk)
    (let* ((start (%runtime))
           (result (thunk)))
      (list (- (%runtime) start)
	    result)))

  ;; Output history (with a short memory)
  (define % unspecified)
  (define %% unspecified)
  (define %%% unspecified)

  (define (%error message . irritants)
    (call/cc
      (lambda (cont)
	(set! %error-cont cont)
	(%complain "Error" message irritants)
	(%reset '*))))

  (define %error-cont '*)

  (define (%complain error-type message irritants)
    (newline) 
    (display "[")
    (display error-type)
    (display "!] ")
    (display message)
    (for-each (lambda (i) (newline) (write i))
	      irritants)
    (newline))

  (define (%proceed value)
    (if (procedure? %error-cont)
	(%error-cont value)
	(%error "No error to proceed from, or unproceedable")))

  ;; SRFI-23 compatible alias
  (define error %error))


;;;;
;;;; Disassembly
;;;;

(begin

  (define prim-name-vectors
    (vector (list->vector (map car prim-0-list))
	    (list->vector (map car prim-1-list))
	    (list->vector (map car prim-2-list))
	    (list->vector (map car prim-3-list))))

  (define (dis proc)
    (disassemble (%closure->code proc) -1))

  (define (disassemble code current-pc)
    (dump-asm (disassemble-instrucs code) 2 current-pc))

  (define (dump-asm asm margin current-pc)

    (define (write-prim arity index)
      (write-char #\space)
      (display (vector-ref (vector-ref prim-name-vectors arity) index))
      (newline))

    (define (indent margin) 
      (cond ((< 0 margin) 
	     (write-char #\space) 
	     (indent (- margin 1)))))

    (for-each (lambda (pair) 
		(let ((offset (car pair))
		      (i      (cadr pair)))

		  (cond ((= offset current-pc)
			 (indent (- margin 2))
			 (display "=>"))
			(else 
			 (indent margin)))

		  (let ((s (number->string offset)))
		    (indent (- 3 (string-length s)))
		    (display s)
		    (write-char #\space))

		  (write (car i))

		  (case (car i)
		    ((proc)
                     (let ((code (cadr i)))
                       (write-char #\space)
                       (write (code->label code))
		       (newline)
		       (dump-asm (disassemble-instrucs code)
			         (+ margin 5)
			         -1)))
		    ((prim-0) (write-prim 0 (cadr i)))
		    ((prim-1) (write-prim 1 (cadr i)))
		    ((prim-2) (write-prim 2 (cadr i)))
		    ((prim-3) (write-prim 3 (cadr i)))
		    (else
		     (for-each (lambda (a)
				 (write-char #\space)
				 (write a))
			       (cdr i))
		     (newline)))))
	      asm))


  (define (disassemble-instrucs code)
    (let ((L (string-length (code->bytecodes code))))
      (let loop ((i 0) (acc '()))
	(if (<= L i)
	    (reverse acc)
	    (disassemble-instruc i code
				 (lambda (width dis)
				   (loop (+ i width)
					 (cons (list i dis)
					       acc))))))))

  ;; Return (k width parts)
  ;;   where width is the #bytes encoding this instruction + its args
  ;;   and parts is the instruction name and arguments as parsed from the encoding.
  (define (disassemble-instruc pc code k)
    (define (byte-ref offset) 
      (char->integer (string-ref (code->bytecodes code) (+ pc offset))))
    (define (take nbytes . args)
      (let ((iname (vector-ref %instruc-names (byte-ref 0))))
        (k (+ nbytes 1) (cons iname args))))
    (let ((specs (vector-ref %instruc-args (byte-ref 0))))
      (cond ((null? specs) (take 0))
            ((not (null? (cdr specs)))
             (%error "Bad instruc specs" specs))
            (else
              (case (car specs)
	        ((d) (take 1 (vector-ref (code->constants code) (byte-ref 1))))
	        ((w) (take 2 (+ (+ pc 3)
			        (+ (* 256 (byte-ref 1)) (byte-ref 2)))))
	        ((b) (take 1 (byte-ref 1)))
                ((locals) (let ((n-locals (byte-ref 1))
                                (lmap (code->locals-map code)))
                            (take 1 (if (pair? lmap) ; (let's be robust to stripping debug info)
                                        (car lmap)
                                        n-locals))))
                ((v) (let ((depth (byte-ref 1))
                           (offset (byte-ref 2)))
                       (take 2
                             (locals-map-ref (code->locals-map code) depth offset)
                             `(at ,depth ,offset))))
	        (else (%error "BUG: bad instruc arg" spec))))))))


;;;;
;;;; Writing cyclic structures
;;;;

;;;;
;;;; Write list structures with cyclic references cut short.
;;;; E.g.:
;;;; > (define a (list 'x))
;;;; > (set-cdr! a a)
;;;; > (cycle-write a)
;;;; #1 = (x . #1) 
;;;;
;;;; I'll fold this into the main system writer sometime...
;;;;

(begin

  (define (cycle-write x . optional-port)
    (let ((out (%optional-arg optional-port (current-output-port)))
	  (table (make-cycle-table)))
      (traverse table x)
      (cw out table x)
      (begin)))

  (define (traverse table obj)
    (let walk ((obj obj))
      (cond ((and (pair? obj)
		  (not (table-visit! table obj)))
	     (walk (car obj))
	     (walk (cdr obj)))
	    ((and (vector? obj)
		  (< 0 (vector-length obj))
		  (not (table-visit! table obj)))
	     (let loop ((i (vector-length obj)))
	       (cond ((< 0 i)
		      (walk (vector-ref obj (- i 1)))
		      (loop (- i 1)))))))))

  ;; The cycle table has two fields:
  ;; an a-list from objects to markers, and a counter.
  ;; A marker is either #t or an integer:
  ;;   #t: we've seen the object only once so far; 
  ;;    N: (positive) we've seen it more than once, but not written it; 
  ;;   -N: (negative) we've seen it and written it.
  ;; The counter assigns a unique N to each object seen more than once.

  (define (make-cycle-table)
    (list '() 0))

  (define (table-visit! table obj)
    (cond ((assq obj (car table))
	   => (lambda (pair)
		(cond ((eq? #t (cdr pair))
		       (set-car! (cdr table)
				 (+ 1 (car (cdr table))))
		       (set-cdr! pair (car (cdr table)))))
		#t))
	  (else
	   (set-car! table
		     (cons (cons obj #t) (car table)))
	   #f)))

  (define (table-ref table obj)
    (cond ((assq obj (car table)) => cdr)
	  (else #f)))

  (define (table-set! table obj value)
    (cond ((assq obj (car table))
	   => (lambda (pair)
		(set-cdr! pair value)))))

  (define (cw out table obj)

    (define (put str)
      (display str out))

    (define (check-table obj write-obj)
      (cond ((table-ref table obj)
	     => (lambda (label)
		  (if (not (number? label))     ; Only one visit
		      (write-obj)
		      (cond ((< 0 label)	; First visit to obj
			     (put "#")
			     (put label)
			     (put "=")
			     (table-set! table obj (- label)) ; Mark first visit
			     (write-obj))
			    (else		; A later visit
			     (put "#")
			     (put (- label)))))))
	    (else
	     (write-obj))))

    (let recur ((obj obj))
      (cond
	((pair? obj)
	 (check-table obj
	   (lambda ()
	     (put "(")
	     (recur (car obj))
	     (let loop ((L (cdr obj)))
	       (let ((label (table-ref table L)))
		 (cond ((null? L) 'ok)
		       ((or (not (pair? L)) 
			    (number? label))
			(put " . ")
			(recur L))
		       (else 
			(put " ")
			(recur (car L))
			(loop (cdr L))))))
	     (put ")"))))
	((vector? obj)
	 (check-table obj
	   (lambda ()
	     (put "#(")
	     (if (< 0 (vector-length obj))
		 (recur (vector-ref obj 0)))
	     (let loop ((i 1))
	       (cond ((< i (vector-length obj))
		      (put " ")
		      (recur (vector-ref obj i))
		      (loop (+ i 1)))))
	     (put ")"))))
	(else (write obj out))))))


;;;;
;;;; The debugger
;;;;

(begin

  ;;; The parts of a closure: lexical environment and code object.

  ;;; The env is a linked list of env frames, where each frame is
  ;;; represented by a vector with the link in slot 0.

  (define (environment? x)
    (vector? x))

  (define (env-empty? env)
    (= 0 (vector-length env)))

  (define (env->enclosing env)
    (vector-ref env 0))

  (define (env->inner-frame env)
    (cdr (vector->list env)))

  ;; lmap is a locals-map, env is a corresponding runtime env.
  ;; As usual we're robust to a stripped locals-map.
  (define (%show-env-outer-frame lmap env)
    (if (not (env-empty? env))
        (%show-env-frame (if (pair? lmap) (car lmap) '())
                         (env->inner-frame env))))

  (define (%show-env-frame vars vals)
    (if (= (length vars) (length vals))
        (for-each (lambda (var val)
                    (write var) (display ": ") (cycle-write val) (newline))
                  vars
                  vals)
	(%print-each vals)))

  (define (%print-each ls)
    (for-each (lambda (x) (cycle-write x) (newline))
	      ls))

  ;;; The code object holds "actual code" for the vm interpreter, plus
  ;;; for the debugger a label (human-readable full name) and locals-map.

  (define (code? x)
    (and (vector? x)
	 (= (vector-length x) 6)
	 (eq? (vector-ref x 0) 'code-vector)))

  (define (vector-ref-at i)
    (lambda (vec) (vector-ref vec i)))

  (define code->constants (vector-ref-at 1))
  (define code->bytecodes (vector-ref-at 2))
  (define code->label     (vector-ref-at 3))
  (define code->locals-map (vector-ref-at 4))
  (define code->profile   (vector-ref-at 5))


  ;;; Continuations
  ;;; Implemented as a procedure with a particular code-vector, with the
  ;;; interpreter's stack saved as a Scheme vector in the closure's lex-env slot.

  (define continuation? 
    (let ((cont-code (call/cc %closure->code)))
      (lambda (obj)
	(and (procedure? obj)
	     (eq? (%closure->code obj) cont-code)))))

  (define (continuation->stack cont)
    (vector-ref (%closure->lex-env cont) 1))


  ;;; Continuation stack frames: contiguous segments of a stack vector (svec),
  ;;; each designated by an index (sptr) pointing just after the segment.

  (define (make-frame svec sptr)
    (list svec sptr))

  (define frame/svec car)
  (define frame/sptr cadr)

  (define (frame/ref offset tag ok?)
    (lambda (frame)
      (let ((x (vector-ref (frame/svec frame) (- (frame/sptr frame) offset))))
	(if (ok? x)
	    x
	    (%error "Bad ref to" tag x)))))

  (define frame->pc      (frame/ref 4 'pc integer?))
  (define frame->code    (frame/ref 3 'code code?))
  (define frame->lex-env (frame/ref 2 'env environment?))
  (define frame->base    (frame/ref 1 'base integer?)) ; value is the index of the start of this segment

  ;; The caller is the next frame to the left of the base; or #f
  ;; for the final frame, a halt_code sentinel of no interest.
  (define (frame->caller frame)
    (let ((caller (make-frame (frame/svec frame) (frame->base frame))))
      (if (= (frame->base caller) 0)
          #f
          caller)))

  ;; Return the local stack elements as a list, with bottom of stack first.
  (define (frame->stack frame)
    (let ((svec (frame/svec frame))
          (base (frame->base frame)))
      (do ((sp (- (frame/sptr frame) 5)
               (- sp 1))
	   (acc '()
                (cons (vector-ref svec sp) acc)))
	  ((< sp base) acc))))

  ;; Return a list of the frame and all its successive callers, with
  ;; the final caller first.
  (define (caller* frame)
    (do ((frame frame (frame->caller frame))
         (ls '() (cons frame ls)))
        ((not frame) ls)))


  ;;; The interactive debugger

  (define (debug)
    (if (continuation? %error-cont)
	(inspect-cont %error-cont)
	(%error "No context to debug")))

  ;; Break into the debugger after printing the args.
  (define (%avast . args)
    (display "\n[Breakpoint!] %avast\n")
    (for-each (lambda (arg) (write arg) (newline))
              args)
    (call/cc (lambda (cont)
               (inspect-cont cont)
               (if (null? args) unspecified (car args)))))

  (define (inspect-cont cont)

    (define (prompt-and-read prompt)
      (display prompt)
      (let ((obj (read)))
        (if (eof-object? obj) 'quit obj)))

    (define (say message)
      (display message)
      (newline))

    (define (help)
      (say "? help      - this message")
      (say "q quit      - quit the debugger")
      (say "b backtrace - names of the current procedure and its callers")
      (say "a assembly  - show assembly source of the current procedure")
      (say "e env       - show the inner frame of the current environment")
      (say "n next      - show the next frame of the current environment")
      (say "s stack     - show the local value stack")
      (say "u up        - up to caller")
      (say "d down      - down to callee"))

    (define (go-to-frame frame callees)
      (interact frame callees
                (code->locals-map (frame->code frame))
		(frame->lex-env frame)))

    (define (interact frame callees lmap env)

      (define (again)
	(interact frame callees lmap env))

      (case (prompt-and-read "debug> ")

	((? help)
	 (help)
	 (again))

	((q quit)
         unspecified)

	((u up)
	 (let ((caller (frame->caller frame)))
	   (cond (caller 
		  (go-to-frame caller (cons frame callees)))
		 (else
		  (say "At top.")
		  (again)))))

	((d down)
	 (cond ((null? callees)
		(say "At bottom.")
		(again))
	       (else
		(go-to-frame (car callees) (cdr callees)))))

	((e env)
	 (%show-env-outer-frame (code->locals-map (frame->code frame)) ;TODO ugh code dup
                                (frame->lex-env frame))
	 (go-to-frame frame callees))

	((n next)
	 (let ((next (if (env-empty? env)
			 env
			 (env->enclosing env)))
               (next-lmap (if (null? lmap) '() (cdr lmap))))
	   (cond ((env-empty? next)
		  (say "No more environment frames.")
		  (again))
		 (else
		  (%show-env-outer-frame next-lmap next)
		  (interact frame callees next-lmap next)))))

	((a assembly)
	 (disassemble (frame->code frame) (frame->pc frame))
	 (again))

	((s stack)
	 (%print-each (frame->stack frame))
	 (again))

	((b backtrace)
	 (%print-each (map (lambda (frame)
			     (code->label (frame->code frame)))
			   (caller* frame)))
	 (again))

	(else 
	 (say "Huh?  Enter HELP for help.")
	 (again))))

    (display "Enter ? for help.\n")
    (let ((outer-frame 
	   (let ((stack (continuation->stack cont)))
	     (make-frame stack (vector-length stack)))))
      (interact outer-frame '()
                (code->locals-map (frame->code outer-frame))
                (frame->lex-env outer-frame)))))
