(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (eq? (car `(3 ,(+ 4 2))) 3)))
