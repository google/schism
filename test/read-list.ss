(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (list-all-eq? (read) '(1 2 #x42 #t #f))))
