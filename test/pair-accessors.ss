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
    (pair-accessors)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (and (eq? 'a (caar '((a b) c)))
         (eq? 'b (cadr '(a b)))
         (eq? 'b (car (cdar '((a b) c))))
         (eq? 'c (car (cddr '(a b c))))
         (eq? 'a (caaar '(((a b) c) d)))
         (eq? 'b (caadr '(a (b c) d)))
         (eq? 'c (car (cddar '((a b c) d))))
         (eq? 'c (caddr '(a b c d)))
         (eq? 'c (car (cdadr '(a (b c) d))))
         (eq? 'd (car (cdddr '(a b c d e))))
         (eq? 'c (caaddr '(a b (c d) e)))
         (eq? 'c (caddar '((a b c) d)))
         (eq? 'c (cadadr '(a (b c) d)))
         (eq? 'c (caddar '((a b c) d)))
         (eq? 'd (car (cdaddr '(a b (c d))))))))
