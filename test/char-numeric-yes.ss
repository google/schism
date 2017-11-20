(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (char-numeric? #\5)))
