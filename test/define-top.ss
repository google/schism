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
  (export do-test stub)
  (import (rnrs))

  ;; once there's support for failed tests, there should be one to
  ;; check for failure when definitions reference variables that
  ;; haven't been defined yet
  (define stub)
  (define a "abc")
  (define f cons)
  (define b (cons a (f a 12)))

  (define (do-test)
    (equal? b '("abc" . ("abc" . 12)))))
