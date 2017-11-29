(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (let ((n 237))
      (not (and (< n #x40) (> n (- 0 #x40)))))))
