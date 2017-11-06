(library
    (trivial)
  (export do-test)
  (import (rnrs))
  
  (define (do-test)
    (if (zero? 42)
	0
	1)))
