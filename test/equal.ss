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

(library (equal)
  (export do-test)
  (import (rnrs) (rnrs mutable-pairs))

  (define (do-test)
    (and
     ;; from r6rs 11.5
     (equal? 'a 'a)
     (equal? '(a) '(a))
     (equal? '(a (b) c)
             '(a (b) c))
     (equal? "abc" "abc")
     (equal? 2 2)
     (let* ((x '(a))
            (y '(a))
            (z `(,x ,y)))
       (and (equal? z `(,y ,x))
            (equal? z `(,x ,x))))
     ;; from https://scheme.com/tspl4/objects.html#./objects:h2
     (not (equal? 'a 3))
     (not (equal? #t 't))
     (not (equal? "abc" 'abc))
     (not (equal? "hi" '(hi)))
     (let ((x (* 123456789 2)))
       (equal? x x))
     (equal? #t #t)
     (equal? #f #f)
     (not (equal? #t #f))
     (equal? (null? '()) #t)
     (equal? (null? '(a)) #f)
     (equal? (cdr '(a)) '())
     (equal? 'a 'a)
     (not (equal? 'a 'b))
     (equal? 'a (string->symbol "a"))
     (not (equal? '(a) '(b)))
     (equal? '(a) '(a))
     (let ((x '(a . b))) (equal? x x))
     (let ((x (cons 'a 'b))) (equal? x x))
     (equal? (cons 'a 'b) (cons 'a 'b))
     (equal? (string->list "abc") (string->list "abc"))
     (not (equal? "abc" "cba"))
     (equal? "abc" "abc")
     (let ((x "hi")) (equal? x x))
     ;; vector/(string ...)/set!/generic numbers left out for time being
     ;; (equal? car car)
     ;; (not (equal? car cdr))
     ;; fixme - infinite loop:
     ;; (equal?
     ;;  (let ((x (cons 'x 'x)))
     ;;    (set-car! x x)
     ;;    (set-cdr! x x)
     ;;    x)
     ;;  (let ((x (cons 'x 'x)))
     ;;    (set-car! x x)
     ;;    (set-cdr! x x)
     ;;    (cons x x)))
     )))
