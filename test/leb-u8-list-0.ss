(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (number->leb-u8-list n)
    (if (and (< n #x40) (> n (- 0 #x40)))
        `(,(bitwise-and n #x7f))
        (cons (bitwise-ior #x80 (bitwise-and n 127))
              (number->leb-u8-list (bitwise-arithmetic-shift-right n 7)))))

  (define (do-test)
    (list-all-eq? (number->leb-u8-list 0) '(0))))
