(library
    (trivial)
  (export do-test)
  (import (rnrs))

  ;; Make sure we can parse it, but we don't have car and cdr yet, so
  ;; we can't do much.

  (define (foo a b)
    b)
  
  (define (do-test)
    (foo '(1 2 3) #t)))
