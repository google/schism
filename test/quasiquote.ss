(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (eq? `,(+ 1 2) 3)))
