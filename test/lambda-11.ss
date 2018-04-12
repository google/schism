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
    (lambda-11)
  (export do-test)
  (import (rnrs))

  (define (map2 p a b)
    (if (null? a)
	'()
	(cons (p (car a) (car b)) (map2 p (cdr a) (cdr b)))))
  
  ;; Test if in lambda
  (define (do-test)
     (map2 (lambda (a d) (cons a d)) '(a b c) '(1 2 3))))
