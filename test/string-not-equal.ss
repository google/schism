(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (if (string-equal? "abc" "def") #f #t)))
