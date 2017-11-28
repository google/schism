(library
    (trivial)
  (export do-test)
  (import (rnrs))
  
  (define (do-test)
    (not (string-equal? "def" (symbol->string (string->symbol "abc"))))))
