#!/usr/bin/env scheme-script

(import (rnrs)
	(schism compiler))

(if (eq? (length (command-line)) 1)
    (let ((input-file-name (cadr (command-line))))
      (let ((source (read (open-file-input-port input-file-name))))
	(put-bytevector
	 (open-file-output-port "out.wasm" (file-options no-fail))
	 (compile-library source)))))
