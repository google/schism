(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (eq? '5 5)))
