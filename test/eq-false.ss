(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (if (eq? 2 3)
	#f
	#t)))
