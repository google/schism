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
    (trivial)
  (export do-test)
  (import (rnrs))

  (define (debug thing)
    (display thing) (newline))

  (define (var c)
    `(var . ,c))

  (define (var? x)
    (and (pair? x) (eq? 'var (car x))))

  (define (var=? x y)
    (eq? (cdr x) (cdr y)))

  (define (walk u s)
    (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
      (if pr (walk (cdr pr) s) u)))

  (define (ext-s x v s)
    `((,x . ,v) . ,s))

  (define (unit s/c)
    (cons s/c '()))

  (define (=== u v)
    (lambda (s/c)
      (let ((s (unify u v (car s/c))))
  	(if s (unit `(,s . ,(cdr s/c))) '()))))

  (define (unify u v s)
    (let ((u (walk u s)) (v (walk v s)))
      (cond ((eq? u v) s)
            ((and (var? u) (var? v) (var=? u v)) s)
            ((var? u) (ext-s u v s))
            ((var? v) (ext-s v u s))
            ((and (pair? u) (pair? v))
             (let ((s (unify (car u) (car v) s)))
               (and s (unify (cdr u) (cdr v) s))))
            (else (equal? u v)))))

  (define (call/fresh f)
    (lambda (s/c)
      (let ((c (cdr s/c)))
        ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

  (define (disj g1 g2)
    (lambda (s/c)
      (mplus (g1 s/c) (g2 s/c))))

  (define (conj g1 g2)
    (lambda (s/c)
      (bind (g1 s/c) g2)))

  (define (mplus $1 $2)
    (cond ((null? $1) $2)
          ((procedure? $1) (lambda () (mplus $2 ($1))))
          (else (cons (car $1) (mplus $2 (cdr $1))))))

  (define (bind $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind ($) g)))
          (else (mplus (g (car $)) (bind (cdr $) g)))))

  (define (fives x)
    (disj (=== x 5)
          (lambda (s/c)
            (lambda ()
              ((fives x) s/c)))))

  (define (sixes x)
    (disj (=== x 6)
          (lambda (s/c)
            (lambda ()
              ((sixes x) s/c)))))

  (define (pull $)
    (if (procedure? $)
        (pull ($))
        $))

  (define (take n $)
    (if (zero? n)
        '()
        (let (($ (pull $)))
          (if (null? $)
              '()
              (cons (car $)
                    (take (- n 1) (cdr $)))))))

  (define (do-test)
    (let ((empty-state '(() . 0))
          (lx (var 'x)))
      (let ((example-5
             ((call/fresh (lambda (q) (=== q 5)))
              empty-state))
            (example-56
             (take 2 ((call/fresh (lambda (x) (disj (fives x) (sixes x))))
                      empty-state)))
            (example-1&10/3
             ((conj (call/fresh (lambda (x) (=== x 1)))
                    (call/fresh (lambda (y) (disj (=== y 10)
                                             (conj (=== y lx)
                                                   (=== lx 3))))))
              empty-state)))
        ;;; map car to ignore id for next fresh variable
        (and (equal? (map car example-5)
                     '((((var . 0) . 5))))
             (equal? (map car example-56)
                     '((((var . 0) . 5))
                       (((var . 0) . 6))))
             (equal? (map car example-1&10/3)
                     '((((var . 1) . 10)
                        ((var . 0) . 1))
                       (((var . x) . 3)
                        ((var . 1) var . x)
                        ((var . 0) . 1)))))))))
