(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (loop x y z)
    (if (zero? x)
	1
	(loop y z x)))
  
  (define (do-test)
    (loop 2 1 0)))
