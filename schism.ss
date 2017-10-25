#!/usr/bin/env scheme-script

(import (rnrs)
	(schism compiler))

(put-bytevector
 (standard-output-port)
 (compile-library
  '(library
       (import (rnrs))
     (export return_5)

     (define (return_5)
       5))))
