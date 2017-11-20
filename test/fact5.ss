(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (fact n)
    (if (zero? n)
        1
        (* n (fact (- n 1)))))
  
  (define (do-test)
    (eq? (fact 5) 120)))
