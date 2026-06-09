;;;; Misc macros
(begin

  (%define-macro 'delay
                 (lambda rands
                   (if (not (and (pair? rands) (null? (cdr rands))))
                       (%error "Syntax error" "Requires one operand" `(delay ,@rands)))
                   (let ((expr (car rands)))
                     `(%make-promise (lambda () ,expr)))))

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

  ;; (include "filename") is like (load "filename") but splicing into
  ;; its lexical context at macroexpansion time
  (%define-macro 'include
                 (lambda rands
                   (if (not (and (pair? rands) (null? (cdr rands))))
                       (%error "Syntax error" "Requires one operand" `(include ,@rands)))
                   (let ((filename (car rands)))

                     (define (%read-all filename)
                       (call-with-input-file filename
                         (lambda (in)
                           (let reading ()
                             (let ((o (read in)))
	                       (if (eof-object? o)
                                   '()
                                   (cons o (reading))))))))

                     (if (not (string? filename))
                         (%error "Syntax error" "Include filename must be a literal string" filename))
                     `(begin ,@(%read-all filename)))))

  )

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

  (define (%reset)
    (%reset-cont 'ignored))

  (define %reset-cont ; (this definition gets reassigned after startup)
    (lambda (val)
      ;; We only get here when startup is borked somehow, so this is coded to
      ;; rely on the runtime as minimally as we can. Just inlined primitives.
      (display "*** ERROR DURING STARTUP." (current-output-port))
      (newline (current-output-port))
      (%exit 1)))

  (define %arguments-to-scheme '())

  (define (%start-scheming)  ; called by main() once this init heap is all loaded
    (set! %arguments-to-scheme (cdr %command-line-arguments)) ; skip past the executable name
    (cond ((pair? %arguments-to-scheme)
           (set! %reset-cont (lambda (_) (%exit 1)))
	   (load (car %arguments-to-scheme)))
          (else
           (display "Enter an expression. On an error, enter ,d to debug. For more commands: ,help\n")
	   (call/cc (lambda (k) (set! %reset-cont k)))
           (%scheming))))

  (define (%scheming)  ; read-eval-print loop
    ;; In editing the following, stay conscious of what will appear
    ;; in backtraces on error: we want only one frame from this repl.
    (display "-> ")
    (let ((cmd (read)))
      (if (eof-object? cmd)
          (newline)
          (cond ((and (pair? cmd) (eq? (car cmd) 'unquote) (pair? (cdr cmd)) (null? (cddr cmd)))
                 ;; A command
                 (%comma-command (cadr cmd))
                 (%scheming))
                (else
                 ;; A Scheme form
	         (let ((obj (%eval cmd)))
                   (cond ((not (eq? obj %void))
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
       (display ",d           - debug the last error, i.e. (%debug)\n")
       (display ",l name      - (load \"name.scm\")\n")
       (display ",l \"x.scm\"   - (load \"x.scm\")\n")
       (display ",! expr      - evaluate expr for effect, don't print the value\n")
       (display ",time expr   - time the evaluation of expr\n"))
      ((d)
       (%debug))
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
  (define % %void)
  (define %% %void)
  (define %%% %void)

  (define (%error message . irritants)
    (call/cc
      (lambda (cont)
	(set! %error-cont cont)
	(%complain "Error" message irritants)
	(%reset))))

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

  (define %prim-name-vectors
    (vector (list->vector (map car %prim-0-list))
	    (list->vector (map car %prim-1-list))
	    (list->vector (map car %prim-2-list))
	    (list->vector (map car %prim-3-list))))

  (define (%disassemble proc)
    (%disassemble-code (%closure->code proc) -1))

  (define (%disassemble-code code current-pc)
    (%dump-asm (%disassemble-instrucs code) 2 current-pc))

  (define (%dump-asm asm margin current-pc)

    (define (write-prim arity index)
      (write-char #\space)
      (display (vector-ref (vector-ref %prim-name-vectors arity) index))
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
                       (write (%code->label code))
		       (newline)
		       (%dump-asm (%disassemble-instrucs code)
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


  (define (%disassemble-instrucs code)
    (let ((L (string-length (%code->bytecodes code))))
      (let loop ((i 0) (acc '()))
	(if (<= L i)
	    (reverse acc)
	    (%disassemble-instruc i code
				  (lambda (width dis)
				    (loop (+ i width)
					  (cons (list i dis)
					        acc))))))))

  ;; Return (k width parts)
  ;;   where width is the #bytes encoding this instruction + its args
  ;;   and parts is the instruction name and arguments as parsed from the encoding.
  (define (%disassemble-instruc pc code k)
    (define (byte-ref offset) 
      (char->integer (string-ref (%code->bytecodes code) (+ pc offset))))
    (define (take nbytes . args)
      (let ((iname (vector-ref %instruc-names (byte-ref 0))))
        (k (+ nbytes 1) (cons iname args))))
    (let ((specs (vector-ref %instruc-args (byte-ref 0))))
      (cond ((null? specs) (take 0))
            ((not (null? (cdr specs)))
             (%error "Bad instruc specs" specs))
            (else
              (case (car specs)
	        ((d) (take 1 (vector-ref (%code->constants code) (byte-ref 1))))
	        ((w) (take 2 (+ (+ pc 3)
			        (+ (* 256 (byte-ref 1)) (byte-ref 2)))))
	        ((b) (take 1 (byte-ref 1)))
                ((locals) (let ((n-locals (byte-ref 1))
                                (lmap (%code->locals-map code)))
                            (take 1 (if (pair? lmap) ; (let's be robust to stripping debug info)
                                        (car lmap)
                                        n-locals))))
                ((v) (let ((depth (byte-ref 1))
                           (offset (byte-ref 2)))
                       (take 2
                             (%locals-map-ref (%code->locals-map code) depth offset)
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
;;;; > (%cycle-write a)
;;;; #1 = (x . #1) 
;;;;
;;;; I'll fold this into the main system writer sometime...
;;;;

(begin

  (define (%cycle-write x . optional-port)
    (let ((out (%optional-arg optional-port (current-output-port)))
	  (table (%make-cycle-table)))
      (%traverse table x)
      (%cw out table x)
      (begin)))

  (define (%traverse table obj)
    (let walk ((obj obj))
      (cond ((and (pair? obj)
		  (not (%table-visit! table obj)))
	     (walk (car obj))
	     (walk (cdr obj)))
	    ((and (vector? obj)
		  (< 0 (vector-length obj))
		  (not (%table-visit! table obj)))
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

  (define (%make-cycle-table)
    (list '() 0))

  (define (%table-visit! table obj)
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

  (define (%table-ref table obj)
    (cond ((assq obj (car table)) => cdr)
	  (else #f)))

  (define (%table-set! table obj value)
    (cond ((assq obj (car table))
	   => (lambda (pair)
		(set-cdr! pair value)))))

  (define (%cw out table obj)

    (define (put str)
      (display str out))

    (define (check-table obj write-obj)
      (cond ((%table-ref table obj)
	     => (lambda (label)
		  (if (not (number? label))     ; Only one visit
		      (write-obj)
		      (cond ((< 0 label)	; First visit to obj
			     (put "#")
			     (put label)
			     (put "=")
			     (%table-set! table obj (- label)) ; Mark first visit
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
	       (let ((label (%table-ref table L)))
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

  ;;; The parts of a closure: runtime environment (renv) and code object.

  ;;; The renv is a linked list of environment frames (one frame per
  ;;; scope level), where each frame is a vector whose slot 0 links to
  ;;; the enclosing renv, and the other slots hold variable values.

  (define (%renv? x)
    (vector? x))

  (define (%renv/empty? renv)
    (= 0 (vector-length renv)))

  (define (%renv->enclosing renv)
    (vector-ref renv 0))

  (define (%renv->inner-frame renv)
    (cdr (vector->list renv)))

  ;; lmap is a locals-map, renv is a corresponding runtime environment.
  ;; As usual we're robust to a stripped locals-map.
  (define (%show-env-outer-frame lmap renv)
    (if (not (%renv/empty? renv))
        (%show-env-frame (if (pair? lmap) (car lmap) '())
                         (%renv->inner-frame renv))))

  (define (%show-env-frame vars vals)
    (if (= (length vars) (length vals))
        (for-each (lambda (var val)
                    (write var) (display ": ") (%cycle-write val) (newline))
                  vars
                  vals)
	(%print-each vals)))

  (define (%print-each xs)
    (for-each (lambda (x) (%cycle-write x) (newline))
	      xs))

  ;;; The code object holds "actual code" for the vm interpreter, plus
  ;;; for the debugger a label (human-readable full name) and locals-map.

  (define (%code? x)
    (and (vector? x)
	 (= (vector-length x) 6)
	 (eq? (vector-ref x 0) 'code-vector)))

  (define (%vector-ref-at i)
    (lambda (vec) (vector-ref vec i)))

  (define %code->constants  (%vector-ref-at 1))
  (define %code->bytecodes  (%vector-ref-at 2))
  (define %code->label      (%vector-ref-at 3))
  (define %code->locals-map (%vector-ref-at 4))
  (define %code->profile    (%vector-ref-at 5))


  ;;; Continuations
  ;;; Implemented as a procedure with a particular code-vector, with the
  ;;; interpreter's stack saved as a Scheme vector in the closure's renv slot.

  (define %continuation?
    (let ((cont-code (call/cc %closure->code)))
      (lambda (obj)
	(and (procedure? obj)
	     (eq? (%closure->code obj) cont-code)))))

  (define (%continuation->stack cont)
    (vector-ref (%closure->renv cont) 1))


  ;;; Continuation stack frames: contiguous segments of a stack vector (svec),
  ;;; each designated by an index (sptr) pointing just after the segment.

  (define (%make-frame svec sptr)
    (list svec sptr))

  (define %frame/svec car)
  (define %frame/sptr cadr)

  (define (%frame/ref offset tag ok?)
    (lambda (frame)
      (let ((x (vector-ref (%frame/svec frame) (- (%frame/sptr frame) offset))))
	(if (ok? x)
	    x
	    (%error "Bad ref to" tag x)))))

  (define %frame->pc   (%frame/ref 4 'pc integer?))
  (define %frame->code (%frame/ref 3 'code %code?))
  (define %frame->renv (%frame/ref 2 'renv %renv?))
  (define %frame->base (%frame/ref 1 'base integer?)) ; value is the index of the start of this segment

  ;; The caller is the next frame to the left of the base; or #f
  ;; for the final frame, a halt_code sentinel of no interest.
  (define (%frame->caller frame)
    (let ((caller (%make-frame (%frame/svec frame) (%frame->base frame))))
      (if (= (%frame->base caller) 0)
          #f
          caller)))

  ;; Return the local stack elements as a list, with bottom of stack first.
  (define (%frame->stack frame)
    (let ((svec (%frame/svec frame))
          (base (%frame->base frame)))
      (do ((sp (- (%frame/sptr frame) 5)
               (- sp 1))
	   (acc '()
                (cons (vector-ref svec sp) acc)))
	  ((< sp base) acc))))

  ;; Return a list of the frame and all its successive callers, with
  ;; the final caller first.
  (define (%caller* frame)
    (do ((frame frame (%frame->caller frame))
         (ls '() (cons frame ls)))
        ((not frame) ls)))


  ;;; The interactive debugger

  (define (%debug)
    (if (%continuation? %error-cont)
	(%inspect-cont %error-cont)
	(%error "No context to debug")))

  ;; Break into the debugger after printing the args.
  (define (%avast . args)
    (display "\n[Breakpoint!] %avast\n")
    (for-each (lambda (arg) (write arg) (newline))
              args)
    (call/cc (lambda (cont)
               (%inspect-cont cont)
               (if (null? args) %void (car args)))))

  (define (%inspect-cont cont)

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
                (%code->locals-map (%frame->code frame))
		(%frame->renv frame)))

    (define (interact frame callees lmap renv)

      (define (again)
	(interact frame callees lmap renv))

      (case (prompt-and-read "debug> ")

	((? help)
	 (help)
	 (again))

	((q quit)
         %void)

	((u up)
	 (let ((caller (%frame->caller frame)))
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
	 (%show-env-outer-frame (%code->locals-map (%frame->code frame)) ;TODO ugh code dup
                                (%frame->renv frame))
	 (go-to-frame frame callees))

	((n next)
	 (let ((next (if (%renv/empty? renv)
			 renv
			 (%renv->enclosing renv)))
               (next-lmap (if (null? lmap) '() (cdr lmap))))
	   (cond ((%renv/empty? next)
		  (say "No more environment frames.")
		  (again))
		 (else
		  (%show-env-outer-frame next-lmap next)
		  (interact frame callees next-lmap next)))))

	((a assembly)
	 (%disassemble-code (%frame->code frame) (%frame->pc frame))
	 (again))

	((s stack)
	 (%print-each (%frame->stack frame))
	 (again))

	((b backtrace)
	 (%print-each (map (lambda (frame)
			     (%code->label (%frame->code frame)))
			   (%caller* frame)))
	 (again))

	(else 
	 (say "Huh?  Enter HELP for help.")
	 (again))))

    (display "Enter ? for help.\n")
    (let ((outer-frame 
	   (let ((stack (%continuation->stack cont)))
	     (%make-frame stack (vector-length stack)))))
      (interact outer-frame '()
                (%code->locals-map (%frame->code outer-frame))
                (%frame->renv outer-frame)))))
