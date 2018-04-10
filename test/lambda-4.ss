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
    (lambda-4)
  (export do-test)
  (import (rnrs))

  ;; Test if we can call lambdas in a local variable
  (define (do-test)
    (let ((fst (lambda (a b) a))
	  (snd (lambda (a b) b)))
      (and (eq? (fst 1 2) 1)
	   (eq? (snd 1 2) 2)))))
