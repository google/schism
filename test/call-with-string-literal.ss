(library
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (f s)
    1)

  (define (do-test)
    (f "Hello, WebAssembly!")))
