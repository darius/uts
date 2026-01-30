;;; Tests for scheme-data-structures

(load "../test-support.scm")

(load "queue.scm")
(load "pairing-heap.scm")
(load "trie.scm")
(load "sets.scm")
(load "strings.scm")

;;; Queue tests
(test-section "Queue")
(check "empty-queue is empty" #t (queue-empty? empty-queue))
(check "enqueue makes non-empty" #f (queue-empty? (enqueue empty-queue 1)))
(let ((q3 (enqueue (enqueue (enqueue empty-queue 1) 2) 3)))
  (check "dequeue gets first" 1 (car (dequeue q3)))
  (check "dequeue rest not empty" #f (queue-empty? (cadr (dequeue q3)))))

;;; Pairing heap tests
(test-section "Pairing heap")
(check "empty heap is empty" #t (pq-empty? empty-pq))
(let* ((h1 ((pq-insert <=) empty-pq 5))
       (h2 ((pq-insert <=) h1 3))
       (h3 ((pq-insert <=) h2 7)))
  (check "pq-min gets smallest" 3 ((pq-min <=) h3))
  (check "after remove-min" 5 ((pq-min <=) ((pq-remove-min <=) h3))))
;; Run built-in comprehensive test
(display "  Running built-in pairing-heap test...") (newline)
(test)  ; built-in from pairing-heap.scm
(display "  Built-in test passed") (newline)

;;; Trie tests
(test-section "Trie")
(let ((t (make-empty-trie)))
  (check "empty trie is empty" #t (trie-empty? t))
  (check "empty trie lookup fails" #f ((trie-member? eq?) t '(a b c)))
  ((trie-adjoin! eq?) t '(a b c))
  (check "trie lookup after adjoin" #t ((trie-member? eq?) t '(a b c)))
  (check "trie lookup prefix fails" #f ((trie-member? eq?) t '(a b)))
  ((trie-adjoin! eq?) t '(a b))
  (check "trie lookup both keys" #t (and ((trie-member? eq?) t '(a b c))
                                         ((trie-member? eq?) t '(a b)))))
;; Load dictionary as stress test
(display "  Loading wordlist.txt (~45k words)...")
(let ((t (make-empty-trie))
      (count 0))
  (call-with-input-file "wordlist.txt"
    (lambda (port)
      (let loop ()
        (let ((line (read-line port)))
          (if (not (eof-object? line))
              (begin
                ((trie-adjoin! eqv?) t (string->list line))
                (set! count (+ count 1))
                (loop)))))))
  (display count) (display " words loaded") (newline)
  (check "can find 'hello'" #t ((trie-member? eqv?) t (string->list "hello")))
  (check "can't find 'xyzzy'" #f ((trie-member? eqv?) t (string->list "xyzzy"))))

;;; Sets tests
(test-section "Sets")
(check "empty set" '() set-empty)
(let* ((adj (set-adjoin eq?))
       (mem? (lambda (x s) (if ((set-member? eq?) x s) #t #f)))
       (s1 (adj 'a set-empty))
       (s2 (adj 'b s1))
       (s3 (adj 'a s2)))  ; duplicate
  (check "adjoin creates set" #t (mem? 'a s2))
  (check "adjoin both elements" #t (mem? 'b s2))
  (check "not member" #f (mem? 'c s2))
  (check "adjoin duplicate same size" 2 (length s3)))
(let* ((l->s (list->set eq?))
       (mem? (lambda (x s) (if ((set-member? eq?) x s) #t #f)))
       (union (set-union eq?))
       (inter (set-intersect eq?))
       (diff (set-difference eq?))
       (s1 (l->s '(a b c)))
       (s2 (l->s '(b c d))))
  (check "union size" 4 (length (union s1 s2)))
  (check "intersect size" 2 (length (inter s1 s2)))
  (check "difference" #t (and (mem? 'a (diff s1 s2))
                              (not (mem? 'b (diff s1 s2))))))

;;; Strings tests
(test-section "Strings")
(check "brute match found" 3 (string-match/brute "lo" "hello"))
(check "brute match not found" #f (string-match/brute "xyz" "hello"))
(check "brute empty pattern" 0 (string-match/brute "" "hello"))
(let ((find-lo (string-matcher "lo")))
  (check "BMH match found" 3 (find-lo "hello"))
  (check "BMH match not found" #f (find-lo "goodbye")))
(let ((find-needle (string-matcher "needle")))
  (check "BMH longer pattern" 14 (find-needle "haystack with needle in it")))

(test-summary)
