(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (if (eq? (read) 42)
        #f
        #t)))
