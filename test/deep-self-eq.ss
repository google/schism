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
    (deep-self-eq)
  (export do-test)
  (import (rnrs))

  (define (deep-eq? a b)
    (if (and (pair? a) (pair? b))
	(and (deep-eq? (car a) (car b))
	     (deep-eq? (cdr a) (cdr b)))
	(eq? a b)))
  
  (define (do-test)
    (let ((datum (read)))
      (deep-eq? datum datum))))
