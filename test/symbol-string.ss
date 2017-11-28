(library
    (trivial)
  (export do-test)
  (import (rnrs))
  
  (define (do-test)
    (string-equal? "abc" (symbol->string (string->symbol "abc")))))
