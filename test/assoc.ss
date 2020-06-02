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

(library (test assoc)
  (export do-test)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define alist
    `((2 . a) (3 . b)))

  (define (do-test)
    (set-cdr! (assoc 3 alist) 'c)
    (and (equal? (assoc '(a) '(((a) . a) (-1 . b)))
                 '((a) . a))
         (not (assoc '(a) '(((b) . b) (a . c))))
         (equal? alist
                 `((2 . a) (3 . c))))))
