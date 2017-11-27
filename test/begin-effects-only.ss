(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (let ((p (cons 1 2)))
      (begin (set-car! p #t) (set-cdr! p #f) (car p)))))
