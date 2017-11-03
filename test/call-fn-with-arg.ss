(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (f x)
    1)

  (define (g)
    0)
  
  (define (do-test)
    (f 42)))
