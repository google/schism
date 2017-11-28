(library
    (trivial)
  (export do-test)
  (import (rnrs))
  
  (define (do-test)
    (eq? (car (read)) 'library)))
