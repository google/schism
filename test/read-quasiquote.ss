(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (deep-eq? a b)
    (if (and (pair? a) (pair? b))
        (and (deep-eq? (car a) (car b))
             (deep-eq? (cdr a) (cdr b)))
        (eq? a b)))
  
  (define (do-test)
    (deep-eq? (read) '`(x ,5))))
