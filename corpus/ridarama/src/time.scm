; Example: (am 12 30) --> 12:30am
(define am
  (lambda (hour minute)
    (make-time (modulo hour 12) minute 0)))

; Example: (pm 12 30) --> 12:30pm
(define pm
  (lambda (hour minute)
    (make-time (+ 12 (modulo hour 12)) minute 0)))

; Return the time at (hours,minutes,seconds) after midnight.
(define make-time
  (lambda (hours minutes seconds)
    (+ seconds
       (* 60 (+ minutes
		(* 60 hours))))))

; Return am/pm plus hh:mm:ss version of TIME.
(define unparse-time
  (lambda (time)
    (let ((ss (remainder time 60))
	  (minutes (quotient time 60)))
      (let ((mm (remainder minutes 60))
	    (hours (quotient minutes 60)))
	(let ((am/pm (if (< hours 12) 'am 'pm))
	      (hh (modulo hours 12)))
	  (list am/pm (if (= hh 0) 12 hh) mm ss))))))

