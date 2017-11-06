(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (f x)
    x)
  
  (define (do-test)
    (f 1)))
