#!/usr/bin/env scheme-script

(import (rnrs)
	(schism compiler))

(if (eq? (length (command-line)) 2)
    (let ((input-file-name (cadr (command-line))))
      (let ((source (read (open-file-input-port input-file-name
                                                (file-options)
                                                (buffer-mode line)
                                                (native-transcoder)))))
        (put-bytevector
         (open-file-output-port "out.wasm" (file-options no-fail))
         (u8-list->bytevector (compile-library source)))))
    (display "usage: schism.ss <filename>"))
