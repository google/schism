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

(library (test find)
  (export do-test)
  (import (rnrs))

  (define (do-test)
    (and (eq? 3 (find odd? '(2 3 5 7 11)))
         (eq? 2 (find even? '(2 3 5 7 11)))
         (not (find even? '(3 5 7 11)))
         (not (find (lambda (x) (not x)) '(2 3 #f 7 11))))))
