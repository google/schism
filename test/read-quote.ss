(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (list-all-eq? (read) ''5)))
