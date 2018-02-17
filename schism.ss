#!/usr/bin/env scheme-script

;; Copyright 2018 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(import (rnrs)
	(schism compiler))

(define (flatten-compiled-bytes bytes tail)
  (if (null? bytes)
      tail
      (if (pair? bytes)
          (flatten-compiled-bytes (car bytes) (flatten-compiled-bytes (cdr bytes) tail))
          (cons bytes tail))))

(if (eq? (length (command-line)) 2)
    (let ((input-file-name (cadr (command-line))))
      (let ((source (read (open-file-input-port input-file-name
                                                (file-options)
                                                (buffer-mode line)
                                                (native-transcoder)))))
        (put-bytevector
         (open-file-output-port "out.wasm" (file-options no-fail))
         (u8-list->bytevector (flatten-compiled-bytes (compile-library source) '())))))
    (display "usage: schism.ss <filename>"))
