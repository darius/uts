(define (scanning chars)
  (tokenize (columnize chars)))

(to (columnize chars)
  (begin loop ((column 0)
               (chars chars)
               (result '()))
    (match chars
      ('() (reverse result))
      (_   (loop (next-column column chars.first)
                 chars.rest
                 (cons {input (car chars) column}
                       result))))))

(to (next-column column char)
  (match char
    (#\newline 0)
    (#\tab     (+ column (- 8 (modulo column 8))))
    (_         (+ column 1))))

(to (tokenize inputs)
  (if inputs.empty?
      '({token indent -1 #no})
      (do
        (let {input char-1 column-1} inputs.first)
	(match char-1
	  (#\space   (tokenize inputs.rest))
	  (#\tab     (tokenize inputs.rest))
	  (#\newline (scan-indentation inputs.rest))
	  (#\:       (cons {token ': column-1 #no} (tokenize inputs.rest)))
	  (_ (begin scanning ((inputs inputs) (chars '()))
               (match inputs
                 (`({input ,(? constituent? ch) ,_} ,@rest)
                  (scanning rest (cons ch chars)))
                 (_
                  (cons {token 'atom column-1 (cook-atom (reverse chars))}
                        (tokenize inputs))))))))))

(to (scan-indentation inputs)
  (match inputs
    ('() '({token indent -1 #no}))
    (`({input ,ch ,column} ,@rest)
     (if ch.whitespace?
         (scan-indentation rest)
         `({token indent ,column #no} ,@(tokenize inputs))))))

(to (constituent? ch)
  (and (not ch.whitespace?)
       (not= ch #\:)))

(to (cook-atom chars)
  (let str (string<-list chars))
  (or (number<-string str)   ;; TODO is this the actual squeam code?
      (symbol<-string str)))
