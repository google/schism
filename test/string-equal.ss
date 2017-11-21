(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (string-equal? "abc" "abc")))
