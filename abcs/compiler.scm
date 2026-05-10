;;;; 
;;;; More miscellany
;;;;

(define %void (string->symbol "#!%void"))

(define (%foldr fn id lst)
  (let loop ((lst lst))
       (if (null? lst)
	   id
	   (fn (car lst)
	       (loop (cdr lst))))))

(define (%every test? ls)
  (if (null? ls)
      #t
      (and (test? (car ls))
	   (%every test? (cdr ls)))))


;;;;
;;;; lex-env.scm
;;;;

(begin

  (define %make-lexical-address   cons)
  (define %lexical-address/depth  car)
  (define %lexical-address/offset cdr)

  ;; Return the lexical address, or v itself if free.
  (define (%locals-map/lookup s v)
    (let nesting ((depth 0)
		  (s s))
      (if (null? s)
	  v
	  (let searching ((vars (car s))
			  (index 0))
	    (cond ((null? vars)
		   (nesting (+ depth 1) (cdr s)))
		  ((eq? (car vars) v)
                   (if (< 255 depth) (%error "Code too complex: nesting too deep" depth))
                   (if (< 255 index) (%error "Code too complex: too many locals" index))
		   (%make-lexical-address depth index))
		  (else
		   (searching (cdr vars) (+ index 1))))))))

  (define %locals-map/empty '())

  (define (%locals-map/extend s vals)
    (cons vals s))

  ;; Get the name behind a lexical address.
  ;; If the address is not mapped, return '<?>
  ;; -- that's not expected to come up, but I want this
  ;; to be robust to stripping debug info.
  (define (%locals-map-ref locals-map depth offset)
    (%list-ref/default (%list-ref/default locals-map depth '())
                       offset
                       '<?>))

  (define (%list-ref/default ls n default)
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

  (define (%define-macro symbol expander)
    (set! %macroexpanders
          (cons (cons symbol expander) %macroexpanders)))

  (define (%get-macroexpander symbol)
    (cond ((assq symbol %macroexpanders) => cdr)
          (else #f)))

  (define (%macroexpander form)
    (and (pair? form)
         (symbol? (car form))
         (%get-macroexpander (car form))))

  (define (%macroexpand-1 form)
    (cond ((%macroexpander form) => (lambda (expand)
                                      (apply expand (cdr form))))
          (else form)))

  (define (%macroexpand-outermost form)
    (cond ((%macroexpander form) => (lambda (expand)
                                      (%macroexpand-outermost (apply expand (cdr form)))))
          (else form)))

  ;;;
  ;;; Constants tables
  ;;;

  (define (%constants/new)
    (cons 0 '()))

  (define (%constants/lookup datum constants)
    (cond ((assv datum (cdr constants))
	   => cdr)
	  (else
	   (let ((c (car constants)))
             (if (<= 255 c) (%error "Code too complex: too many constants" c))
	     (set-car! constants (+ c 1))
	     (set-cdr! constants (cons (cons datum c) (cdr constants)))
	     c))))

  (define (%constants->vector constants)
    (let ((vec (make-vector (car constants) #f)))
      (for-each (lambda (pair)
		  (vector-set! vec (cdr pair) (car pair)))
		(cdr constants))
      vec))


  ;;;
  ;;; Bytecode assembler ("lap", traditional name for Lisp assembler)
  ;;;

  (define %lap/position length)
  (define %lap/append append)

  (define %lap/restore 
    (list %bop-restore))

  (define (%lap/offset pos lap)
    (let ((offset (- (%lap/position lap) pos)))
      (cons (quotient offset 256)
	    (cons (remainder offset 256)
		  lap))))

  (define (%lap/jump pos lap)
    (cons %bop-jump
	  (%lap/offset pos lap)))

  (define (%lap/unless pos lap)
    (cons %bop-unless
	  (%lap/offset pos lap)))

  (define (%lap/var addr lap)
    (cons %bop-var
	  (cons (%lexical-address/depth addr)
		(cons (%lexical-address/offset addr)
		      lap))))

  (define (%lap/var! addr lap)
    (cons %bop-var!
	  (cons (%lexical-address/depth addr)
		(cons (%lexical-address/offset addr)
		      lap))))

  (define (%lap/params count lap)
    (cons %bop-params
	  (cons count lap)))

  (define (%lap/&rest-params count lap)
    (cons %bop-&rest-params
	  (cons count lap)))

  (define (%lap/save pos lap)
    (cons %bop-save 
	  (%lap/offset pos lap)))

  (define (%lap/invoke lap)
    (cons %bop-invoke lap))

  (define (%lap/drop lap)
    (cons %bop-drop lap))

  (define (%lap/prim-0 prim lap)
    (cons %bop-prim-0
	  (cons prim lap)))

  (define (%lap/prim-1 prim lap)
    (cons %bop-prim-1
	  (cons prim lap)))

  (define (%lap/prim-2 prim lap)
    (cons %bop-prim-2
	  (cons prim lap)))

  (define (%lap/prim-3 prim lap)
    (cons %bop-prim-3
	  (cons prim lap)))

  (define (%lap/lit datum constants lap)
    (cons %bop-lit
	  (cons (%constants/lookup datum constants) 
		lap)))

  (define (%lap/glo symbol constants lap)
    (cons %bop-glo
	  (cons (%constants/lookup symbol constants) 
		lap)))

  (define (%lap/glo! symbol constants lap)
    (cons %bop-glo!
	  (cons (%constants/lookup symbol constants) 
		lap)))

  (define (%lap/define symbol constants lap)
    (cons %bop-define
	  (cons (%constants/lookup symbol constants) 
		lap)))

  (define (%lap/proc code constants lap)
    (cons %bop-proc
	  (cons (%constants/lookup code constants)
		lap)))


  (define %gensym
    (let ((counter 0))
      (lambda ()
	(set! counter (+ counter 1))
	(string->symbol
	 (string-append "#!g_" (number->string counter))))))

  (define %open-code-primitives? #t)

  ;; List of global functions to spare from tail call optimization, so the 
  ;; caller's frame stays visible on the stack. User-settable.
  (define %dont-tail-on-me '(error %error %avast))


  ;; (%COMPILE-FORM form) returns a compiled code vector for a top-level form.
  ;; The code will assume no params and an empty lexical environment ("top-level").
  (define %compile-form 
    (let ()

      ;; The expression parser.

      (define (parse-exp constants label exp s k)

	(let pe ((exp exp) (k k))

	  (cond

	    ((symbol? exp) 
	     (let ((addr (%locals-map/lookup s exp)))
	       (if (symbol? addr) 
		   (%lap/glo addr constants k)
		   (%lap/var addr k))))

	    ((not (pair? exp))
	     (%lap/lit exp constants k))

	    (else
	     (let ((rator (car exp)) 
		   (rands (cdr exp))
		   (num-rands (length (cdr exp)))
		   (assert (lambda (ok?) 
			     (if (not ok?) (syntax-error "Bad syntax" exp)))))

	       (case rator

		 ((quote)
		  (assert (= num-rands 1))
		  (%lap/lit (car rands) constants k))

		 ((if)
		  (assert (or (= num-rands 2) 
			      (= num-rands 3)))
		  (let ((consequent (cadr rands))
			(alternative (if (= num-rands 3) 
					 (caddr rands) 
					 `',%void)))
		    (pe (car rands)
			(if (eq? k %lap/restore)
			    (let ((e (pe alternative k)))
			      (%lap/unless (%lap/position e)
					   (%lap/append (pe consequent k)
						        e)))
			    (let ((j (%lap/position k))
				  (e (pe alternative k)))
			      (%lap/unless (%lap/position e)
					   (pe consequent 
					       (%lap/jump j e))))))))

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
		    (let ((body-constants (%constants/new))
                          (var-count (length fixed-formals))
                          (nest-s (%locals-map/extend s fixed-formals)))
		      (let ((lap (parse-exp body-constants
					    label 
					    (expand-lambda-body (cdr rands))
					    nest-s
					    %lap/restore)))
			(%lap/proc
			 (%codify (if rest-args?
				      (%lap/&rest-params (- var-count 1) lap)
				      (%lap/params var-count lap))
				  (%constants->vector body-constants)
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
		    (let ((addr (%locals-map/lookup s name)))
		      (pe exp
			  (if (symbol? addr) 
			      (%lap/glo! addr constants k)
			      (%lap/var! addr k))))))

		 ((begin)
		  (if (null? rands)
		      (%lap/lit %void constants k)
		      (let loop ((head (car rands)) (tail (cdr rands)))
			(pe head
			    (if (null? tail)
				k
				(%lap/drop (loop (car tail) (cdr tail))))))))

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
		     (else (let ((head (%gensym)))
			     `(let ((,head ,(car rands)))
				(if ,head ,head (or . ,(cdr rands)))))))
		   k))

		 ((cond)
		  (pe
		   (cond
		     ((null? rands) `',%void)
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
		      (let ((test-var (%gensym)))
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
			 (sym (%gensym)))
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
		   (let ((loop (%gensym))
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
		           ((symbol? (%locals-map/lookup s rator))
                            ;; rator is not locally bound -- application of global variable
                            (cond ((and %open-code-primitives?
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
		     (%foldr pe 
			     (pe rator 
				 (%lap/invoke k2))
			     rands))))
	  (if (and tail-ok? (eq? k %lap/restore))
	      (lin '())
	      (%lap/save (%lap/position k)
			 (lin k)))))

      (define (parse-prim-app pe prim rands num-rands k)
        (let ((primop-k ((case num-rands
		           ((0) %lap/prim-0)
		           ((1) %lap/prim-1)
		           ((2) %lap/prim-2)
		           ((3) %lap/prim-3))
	                 prim
	                 k)))
          (%foldr pe primop-k rands)))

      (define (prim-lookup sym num-rands)
        (cond ((assq sym (case num-rands
			   ((0) %prim-0-list)
			   ((1) %prim-1-list)
			   ((2) %prim-2-list)
			   ((3) %prim-3-list)
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

      ; If sexp-0 is a definition, return (SUCCEED vars defs) with vars and
      ; defs as in scan-out-defines; else return (FAIL).
      ;; TODO macroexpanding here is a hack, because it changes the order
      ;;  and count of macroexpansions from that of the most straightforward compiler.
      ;; TODO if we keep using this, then return the expanded sexp in the fail case too.
      (define (maybe-parse-definition sexp-0 succeed fail)
        (let ((sexp (%macroexpand-outermost sexp-0)))
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
                          (syntax-error "Invalid definition" sexp-0))))
                    fail)))))
            (else (fail)))))

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
	     (%every (lambda (decl) 
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


      ;; Body of %COMPILE-FORM

      (lambda (form)
	(let ((constants (%constants/new)))
	  (let ((lap
		 (maybe-parse-definition 
		  form
		  (lambda (names exps)
		    (do ((names (reverse names) (cdr names))
			 (exps  (reverse exps)  (cdr exps))
			 (k %lap/restore
			    (parse-exp constants
				       (car names)
				       (car exps)
				       %locals-map/empty
				       (%lap/define (car names) 
						    constants
						    (if (null? (cdr names))
						        k
						        (%lap/drop k))))))
			((null? names) k)))
		  (lambda () 
		    (parse-exp constants '() form %locals-map/empty 
			       %lap/restore)))))
	    (%codify lap (%constants->vector constants) #f %locals-map/empty))))))


  (define (%make-code-vector constants-vec bytes label locals-map)
    (vector 'code-vector constants-vec bytes label locals-map 0))

  (define (%codify lap constants-vec label locals-map)
    (%make-code-vector constants-vec
		       (list->string (map integer->char lap))
		       label
                       locals-map))

  (define (%eval form)
    ;; Compile to a top-level procedure with no params, and call it.
    ((%make-closure '#() (%compile-form form)))))


;;;;
;;;; globals.scm
;;;;

(begin

  (define %variable-arity-prim-list
    '(peek-char read-char 
      write-char + - * / < <= = 
      make-vector make-string number->string string->number
      append write display)))
