#!/usr/bin/env scheme-script

(import (rnrs)
	(schism compiler))

(put-bytevector
 (open-file-output-port "out.wasm" (file-options no-fail))
 (compile-library
  '(library
       (import (rnrs))
     (export return_5)

     (define (return_5)
       5))))
