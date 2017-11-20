(library
    (trivial)
  (export do-test)
  (import (rnrs)
		  ;; This is actually ignored, but we might as well do it propery.
		  (rt))

  (define (do-test)
    (eq? (rt-add1 5) 6)))
