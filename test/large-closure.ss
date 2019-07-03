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
    (large-closure)
  (export do-test)
  (import (rnrs))

  ;; We had a bug where sufficiently large closures would be
  ;; overwritten by subsequent allocations. This is meant to
  ;; illustrate the bug and make sure it's fixed.
  (define (do-test)
    (let ((a0 1)
          (a1 2)
          (a3 3)
          (a4 4)
          (a5 5)
          (a6 6)
          (a7 7)
          (a8 8)
          (a9 9)
          (a10 10)
          (a11 11)
          (a12 12)
          (a13 13)
          (a14 14)
          (a15 15)
          (a16 16)
          (a17 17)
          (a18 18))
      (let* ((big-lambda (lambda ()
                           (and (eq? a0 1)
                                (eq? a1 2)
                                (eq? a3 3)
                                (eq? a4 4)
                                (eq? a5 5)
                                (eq? a6 6)
                                (eq? a7 7)
                                (eq? a8 8)
                                (eq? a9 9)
                                (eq? a10 10)
                                (eq? a11 11)
                                (eq? a12 12)
                                (eq? a13 13)
                                (eq? a14 14)
                                (eq? a15 15)
                                (eq? a16 16)
                                (eq? a17 17)
                                (eq? a18 18))))
             (pair (cons 20 21)))
        (big-lambda)))))
