(define in-directory
  (lambda (directory)
    (lambda (filename)
      (make-pathname directory filename))))

(define load-from
  (lambda (directory)
    (lambda (filename)
      (write filename)
      (newline)
      (load (make-pathname directory filename)))))

(for-each (load-from "support") '("helpers.scm"
				  "macro.scm"
				  "record-macros.scm"
				  "record.scm"
				  "queue.scm"
				  "trie.scm"
				  ))

(define load-macroexpanded 
  (lambda (directory filenames)
    (for-each macroexpand-file 
	      (map (in-directory directory) filenames) 
	      (map (in-directory "expanded") filenames))
    (for-each (load-from "expanded") filenames)))

(load-macroexpanded "src" '("search.scm"
			    "time.scm"
			    "geography.scm"
			    "geocode.scm"
			    "read.scm"
			    "bart.scm"
			    ))

(load "fun-tests.scm")
