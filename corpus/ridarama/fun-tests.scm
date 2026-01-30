(define do-tests
  (lambda ()
    (setup)
    (print 'initialized)
    (test1)
    ))

(define do-expensive-tests
  (lambda ()
    'none))

;(define do-test
;  (lambda (goal filename result)
;    (expect result test goal filename)))

(define setup
  (lambda ()
    (set! arcs (vector '() '()))
    (set! graph '())
    (read-schedule "data/arcs")
    (read-schedule "data/caltrain-arcs")))


(define test1
  (lambda ()
    (expect
     '#(path #(event embar 85500 #(arc #(ride 619 9) #(#(node w-oak 85140) #(node embar 85500)))) #(path #(event w-oak 85140 #(arc #(ride 619 9) #(#(node lakem 84840) #(node w-oak 85140)))) #(path #(event lakem 84600 #(arc #(ride 371 6) #(#(node frtvl 84300) #(node lakem 84600)))) #(path #(event frtvl 84300 #(arc #(ride 371 6) #(#(node colis 84120) #(node frtvl 84300)))) #(path #(event colis 84120 #(arc #(ride 371 6) #(#(node slean 83880) #(node colis 84120)))) #(path #(event slean 83880 #(arc #(ride 371 6) #(#(node bfair 83640) #(node slean 83880)))) #(path #(event bfair 83640 #(arc #(ride 371 6) #(#(node hay 83400) #(node bfair 83640)))) #(path #(event hay 83400 #(arc #(ride 371 6) #(#(node shay 83160) #(node hay 83400)))) #(path #(event shay 83160 #(arc #(ride 371 6) #(#(node ucity 82860) #(node shay 83160)))) #(path #(event ucity 82800 #f) #f 0 536870912) 360 840) 600 1020) 840 1200) 1080 1380) 1320 1560) 1500 1680) 1800 1920) 2340 2400) 2700 2700)
     trip
     (make-trip-request 'ucity 'embar (make-leaving-at (pm 11 00))))))
