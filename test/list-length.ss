(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (length ls)
    (if (null? ls)
	0
	(+ 1 (length (cdr ls)))))
  
  (define (do-test)
    (eq? (length '(10 11 12 13 14 15)) 5)))
