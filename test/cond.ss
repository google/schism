(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (cond
     ((zero? 1) #f)
     ((pair? (cons 1 2)) #t)
     (else #f))))
