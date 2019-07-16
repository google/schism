;; Copyright 2019 Google LLC
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
    (mod0)
  (export do-test)
  (import (rnrs))

  (define (for-each f ls)
    (unless (null? ls)
      (f (car ls))
      (for-each f (cdr ls))))
  (define (tests)
    ;; R6RS §11.7.3.1.
    '((123 10 3)
      (123 -10 3)
      (-123 10 -3)
      (-123 -10 -3)))
  (define (do-test)
    (for-each
     (lambda (t)
       (let ((n (car t))
             (d (cadr t))
             (q (caddr t)))
         (unless (eq? q (mod0 n d))
           (display t)
           (newline)
           (error 'do-test "unexpected result"))))
     (tests))))
