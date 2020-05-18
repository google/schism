;; Copyright 2018 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the License);
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an AS IS BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
    (equal? (number->leb-u8-list 237) '(237 1))))
