;; Copyright 2020 Google LLC
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

(library (define-top)
  (export do-test)
  (import (rnrs))

  (define f cons)
  (define y "abc")
  (define x (f y 12))
  ;;  (define w (let ((a 12)) (let* ((z (lambda (y) (f y y)))) (z (cons a a)))))

  (define (do-test)
    (equal? x '("abc" . 12)))

  )
