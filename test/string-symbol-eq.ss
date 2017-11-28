(library
    (trivial)
  (export do-test)
  (import (rnrs))
  
  (define (do-test)
    (eq? (string->symbol "abc") (string->symbol "abc"))))
