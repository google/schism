(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (f)
    1)

  (define (g)
    0)
  
  (define (do-test)
    (f)))
