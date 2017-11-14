(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (find item ls index)
    (if (null? ls)
	-1
	(if (eq? (car ls) item)
	    index
	    (find item (cdr ls) (+ 1 index)))))

  (define (do-test)
    (eq? (find 13 '(10 11 12 13 14 15) 0) 3)))
