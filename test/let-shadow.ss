(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (foo n)
    (let ((n 5))
      n))
  
  (define (do-test)
    (eq? (foo 6) 5)))
