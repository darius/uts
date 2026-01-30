;;; Tests for scheme-data-structures

(load "queue.scm")
(load "pairing-heap.scm")
(load "trie.scm")

(define (check name expected actual)
  (if (equal? expected actual)
      (begin (display "PASS: ") (display name) (newline))
      (begin (display "FAIL: ") (display name)
             (display " expected ") (write expected)
             (display " got ") (write actual) (newline))))

;;; Queue tests
(display "--- Queue tests ---") (newline)
(check "empty-queue is empty" #t (queue-empty? empty-queue))
(check "enqueue makes non-empty" #f (queue-empty? (enqueue empty-queue 1)))

(define q3 (enqueue (enqueue (enqueue empty-queue 1) 2) 3))
(check "dequeue gets first" 1 (car (dequeue q3)))
(check "dequeue rest not empty" #f (queue-empty? (cadr (dequeue q3))))

;;; Pairing heap tests
(display "--- Pairing heap tests ---") (newline)
(check "empty heap is empty" #t (pq-empty? empty-pq))
(define h1 ((pq-insert <=) empty-pq 5))
(define h2 ((pq-insert <=) h1 3))
(define h3 ((pq-insert <=) h2 7))
(check "pq-min gets smallest" 3 ((pq-min <=) h3))
(check "after remove-min" 5 ((pq-min <=) ((pq-remove-min <=) h3)))
;; Run built-in comprehensive test (uses 'do')
(display "Running pairing-heap built-in test...") (newline)
(test)
(display "pairing-heap built-in test passed") (newline)

;;; Trie tests
(display "--- Trie tests ---") (newline)
(define trie1 (make-empty-trie))
(check "empty trie is empty" #t (trie-empty? trie1))
(check "empty trie lookup fails" #f ((trie-member? eq?) trie1 '(a b c)))
((trie-adjoin! eq?) trie1 '(a b c))
(check "trie lookup after adjoin" #t ((trie-member? eq?) trie1 '(a b c)))
(check "trie lookup different key" #f ((trie-member? eq?) trie1 '(a b)))
((trie-adjoin! eq?) trie1 '(a b))
(check "trie lookup both keys" #t (and ((trie-member? eq?) trie1 '(a b c))
                                       ((trie-member? eq?) trie1 '(a b))))

(display "--- All tests complete ---") (newline)
