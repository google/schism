(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (if (char-numeric? #\A) #f #t)))
