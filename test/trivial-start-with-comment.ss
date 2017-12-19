;; This test starts with a comment.

(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    1))
