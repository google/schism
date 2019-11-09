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

  ;;; Logic variables
  (define (var c)
    `(var . ,c))

  (define (var? x)
    (and (pair? x) (eq? 'var (car x))))

  (define (var=? x y)
    (eq? (cdr x) (cdr y)))

  ;;; Unification
  (define (lookup var s)
    (let ((b (and (var? var) (assp (lambda (u) (var=? u var)) s))))
      (if b (lookup (cdr b) s) var)))

  (define (lookup* var s)
    (let ((v (lookup var s)))
      (cond ((var? v) v)
            ((pair? v) (cons (lookup* (car v) s)
                             (lookup* (cdr v) s)))
            (else v))))

  (define (ext-s var value s)
    `((,var . ,value) . ,s))

  (define (unify u v s)
    (let ((u (lookup u s)) (v (lookup v s)))
      (cond ((eq? u v) s)
            ((var? u) (ext-s u v s))
            ((var? v) (ext-s v u s))
            ((and (pair? u) (pair? v))
             (let ((s (unify (car u) (car v) s)))
               (and s (unify (cdr u) (cdr v) s))))
            (else (equal? u v)))))

  ;;; Goals
  (define (succeed s.c) `(,s.c))
  (define (fail s.c) '())

  (define (== u v)
    (lambda (s.c)
      (let ((s (unify u v (car s.c))))
        (if s (succeed `(,s . ,(cdr s.c))) (fail s)))))

  (define (run n goal)
    (let ((result (goal '(() . 0))))
      (if (null? result)
          result
          (map car (take n result)))))

  (define (run* n var goal)
    (let ((result (goal '(() . 0))))
      (take n (map-$ (lambda (s.c) (lookup* var (car s.c))) result))))

  (define (choice var lst)
    (if (null? lst)
        fail
        (disj (== var (car lst))
              (choice var (cdr lst)))))

  (define (call/fresh f)
    (lambda (s.c)
      (let ((c (cdr s.c)))
        ((f (var c)) `(,(car s.c) . ,(+ c 1))))))

  (define (disj g1 g2)
    (lambda (s.c)
      (interleave (g1 s.c) (g2 s.c))))

  (define (conj g1 g2)
    (lambda (s.c)
      (bind (g1 s.c) g2)))

  (define (interleave $1 $2)
    (cond ((null? $1) $2)
          ((procedure? $1) (lambda () (interleave $2 ($1))))
          (else (cons (car $1) (interleave $2 (cdr $1))))))

  (define (bind $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind ($) g)))
          (else (interleave (g (car $)) (bind (cdr $) g)))))

  (define (map-$ g $)
    (cond ((null? $) '())
          ((procedure? $) (map-$ g ($)))
          (else (cons (g (car $))
                      (lambda () (map-$ g (cdr $)))))))

  (define (natural* x n)
    (disj (== x n)
          (lambda (s)
            (lambda ()
              ((natural* x (+ n 1)) s)))))
  
  (define (natural x)
    (natural* x 0))

  (define (pull $)
    (if (procedure? $) ($) $))

  (define (take n $)
    (if (zero? n)
        '()
        (let (($ (pull $)))
          (if (null? $)
              '()
              (cons (car $) (take (- n 1) (cdr $)))))))

  (define (common-element l1 l2)
    (call/fresh (lambda (v)
                  (conj (choice v l1)
                        (choice v l2)))))

  (define (conso a b l)
    (== (cons a b) l))

  (define (appendo x y z)
    (disj (conj (== x '()) (== y z))
          (call/fresh
           (lambda (hd)
             (call/fresh
              (lambda (tl)
                (call/fresh
                 (lambda (z*)
                   (conj (conso hd tl x)
                         (conj (conso hd z* z)
                               (appendo tl y z*)))))))))))


  (define (do-test)
    (let ((empty-state '(() . 0))
          (vx (var 'x))
          (vy (var 'y))
          (vz (var 'z)))
      (and (equal? '(1 2)
                   (run* 2 vx (choice vx '(1 2 3))))
           (equal? '(0 1 2 3 4)
                   (run* 5 vy (conj (== vy vx) (natural vx))))
           (equal? '()
                   (run* 2 vx (conj (choice vx '(1 2 3)) (== vx 0))))
           (equal? '((() (0 1 2)) ((0) (1 2)) ((0 1) (2)) ((0 1 2) ()))
                   (run* 4 vz (conj (== vz `(,vx ,vy))
                                    (appendo vx vy '(0 1 2)))))))))
