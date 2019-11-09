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

  ;;; The uKanren paper uses vectors to wrap variables. For now, we
  ;;; tag pairs.
  (define (var name)
    `(var . ,name))

  (define (var? x)
    (and (pair? x) (eq? 'var (car x))))

  (define (var=? x y)
    (eq? (cdr x) (cdr y)))

  ;;;; Unification
  ;;; lookup is referred to as walk in the uKanren paper. The name
  ;;; lookup is used in sokuza-karen.
  (define (lookup var s)
    (let ((binding (and (var? var) (assp (lambda (u) (var=? u var)) s))))
      (if binding (lookup (cdr binding) s) var)))

  ;;; A more diligent lookup.
  (define (lookup* var s)
    (let ((v (lookup var s)))
      (cond
       ((var? v) v)
       ((pair? v) (cons (lookup* (car v) s) (lookup* (cdr v) s)))
       (else v))))

  (define (extend-s var val s)
    `((,var . ,val) . ,s))

  (define (unify u v s)
    (let ((u (lookup u s)) (v (lookup v s)))
      (cond
       ((var? u) (extend-s u v s))
       ((var? v) (extend-s v u s))
       ((and (pair? u) (pair? v))
        (let ((s (unify (car u) (car v) s)))
          (and s (unify (cdr u) (cdr v) s))))
       (else (and (eq? u v) s)))))

  ;;;; Core
  ;;; uKanren paper refers to these as unit and mzero.
  (define (succeed s.c) `(,s.c))
  (define (fail s.c) '())

  (define (== u v)
    (lambda (s.c)
      (lambda ()
        (let ((s (unify u v (car s.c))))
          (if s (succeed `(,s . ,(cdr s.c))) (fail s))))))

  ;;; Use int c in s.c pair to introduce a new variable to give to f
  ;;; to build a goal.
  (define (call/fresh g)
    (lambda (s.c)
      (lambda ()
        (let ((c (cdr s.c)))
          ((g (var c)) `(,(car s.c) . ,(+ c 1)))))))

  (define (run n var goal)
    (let ((result (goal '(() . 0))))
      (take n (map-$ (lambda (s.c) (lookup* var (car s.c))) result))))

  (define (run* var goal)
    (let ((result (goal '(() . 0))))
      (take-all (map-$ (lambda (s.c) (lookup* var (car s.c))) result))))


  ;;; To map over a possibly infinite stream.
  (define (map-$ f $)
    (cond
     ((null? $) '())
     ((procedure? $) (map-$ f ($)))
     (else (cons (f (car $)) (lambda () (map-$ f (cdr $)))))))

  ;;;; Conjunction/Disjunction
  ;;; conj/disj use interleave or choice if suffixed with -i to
  ;;; advance their results.
  (define (disj g1 g2)
    (lambda (s.c)
      (interleave (g1 s.c) (g2 s.c))))

  (define (disj-i g1 g2)
    (lambda (s.c)
      (choice (g1 s.c) (g2 s.c))))

  (define (conj g1 g2)
    (lambda (s.c)
      (bind (g1 s.c) g2)))

  (define (conj-i g1 g2)
    (lambda (s.c)
      (bind-i (g1 s.c) g2)))

  ;;; Conjunct a list of goals. Kanrens normally use syntax to
  ;;; accomplish this.
  (define (conj* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((conj (car gs) (conj* (cdr gs))) s.c)))
       (else (succeed s.c)))))

  (define (conj-i* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((conj-i (car gs) (conj-i* (cdr gs))) s.c)))
       (else (succeed s.c)))))

  (define (disj* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((disj (car gs) (disj* (cdr gs))) s.c)))
       (else (fail s.c)))))

  (define (disj-i* gs)
    (lambda (s.c)
      (cond
       ((pair? gs)
        (if (null? (cdr gs))
            ((car gs) s.c)
            ((disj-i (car gs) (disj-i* (cdr gs))) s.c)))
       (else (fail s.c)))))

  ;;; The uKanren paper uses the haskellism mplus. Two streams take
  ;;; turns advancing.
  (define (interleave $1 $2)
    (cond
     ((null? $1) $2)
     ((procedure? $1) (lambda () (interleave $2 ($1))))
     (else (cons (car $1) (lambda () (interleave $2 (cdr $1)))))))

  ;;; This one is otherwise called mplusi. The distinction from
  ;;; interleave is that one stream is exhausted before the next is
  ;;; tried.
  (define (choice $1 $2)
    (cond
     ((null? $1) $2)
     ((procedure? $1) (lambda () (choice ($1) $2)))
     (else (cons (car $1) (lambda () (choice (cdr $1) $2))))))

  ;;; g is a goal whose results must agree with those coming from $.
  (define (bind $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind ($) g)))
          (else (interleave (g (car $)) (bind (cdr $) g)))))

  (define (bind-i $ g)
    (cond ((null? $) '())
          ((procedure? $) (lambda () (bind-i ($) g)))
          (else (choice (g (car $)) (bind-i (cdr $) g)))))

  (define (ife g1 g2 g3)
    (lambda (s.c)
      (lambda () (interleave ((conj g1 g2) s.c)
                        (lambda () (g3 s.c))))))

  (define (natural* x n)
    (disj (== x n)
          (lambda (s)
            (lambda ()
              ((natural* x (+ n 1)) s)))))
  
  (define (natural x)
    (natural* x 0))

  (define (negative* x n)
    (disj (== x n)
          (lambda (s)
            (lambda ()
              ((negative* x (- n 1)) s)))))

  (define (negative x)
    (negative* x -1))

  (define (integer x)
    (disj (natural x)
          (negative x)))

  ;;;; Streams
  (define (pull $)
    (if (procedure? $) ($) $))

  (define (take-all $)
    (let (($ (pull $)))
      (if (null? $)
          $
          (cons (car $) (take-all (cdr $))))))

  (define (take n $)
    (if (zero? n)
        '()
        (let (($ (pull $)))
          (if (null? $)
              '()
              (cons (car $) (take (- n 1) (cdr $)))))))

  ;;;; Various Relations
  (define (nullo x)
    (== '() x))

  (define (conso a d x)
    (== (cons a d) x))

  (define (caro x a)
    (call/fresh
     (lambda (d)
       (conso a d x))))

  (define (cdro x d)
    (call/fresh
     (lambda (a)
       (conso a d x))))

  (define (pairo x)
    (call/fresh
     (lambda (a)
       (call/fresh
        (lambda (d)
          (conso a d x))))))

  (define (listo l)
    (disj (nullo l)
          (call/fresh
           (lambda (l*)
             (conj (cdro l l*)
                   (listo l*))))))

  (define (element-of var lst)
    (if (null? lst)
        fail
        (disj (== var (car lst))
              (element-of var (cdr lst)))))

  (define (common-element var l1 l2)
    (conj (element-of var l1) (element-of var l2)))

  (define (membero x l)
    (disj*
     `(,(conj (nullo l) fail)
       ,(caro l x)
       ,(call/fresh
         (lambda (tl)
           (conj (cdro l tl) (membero x tl)))))))

  (define (appendo x y z)
    (disj (conj (== x '()) (== y z))
          (call/fresh
           (lambda (hd)
             (call/fresh
              (lambda (tl)
                (call/fresh
                 (lambda (z*)
                   (conj*
                    `(,(conso hd tl x)
                      ,(conso hd z* z)
                      ,(appendo tl y z*)))))))))))

  (define (display-ln x)
    (display x) (newline))
  
  (define (do-test)
    (let ((empty-state '(() . 0))
          (vx (var 'x))
          (vy (var 'y))
          (vz (var 'z))
          (vq (var 'q)))
      (and (equal? '(1 2 3)
                   (run* vx (element-of vx '(1 2 3))))
           (equal? '()
                   (run* vx (conj* `(,(== vx 6)
                                     ,(== vx vy)
                                     ,(== vy 5)))))
           (equal? '(0 1 2 3 4)
                   (run 5 vy (conj (== vy vx)
                                   (natural vx))))
           (equal? '()
                   (run* vx (conj (element-of vx '(1 2 3))
                                  (== vx 0))))
           (equal? '((() (s c h i s m))
                     ((s) (c h i s m))
                     ((s c) (h i s m))
                     ((s c h) (i s m))
                     ((s c h i) (s m))
                     ((s c h i s) (m))
                     ((s c h i s m) ()))
                   (run* vz (conj (== vz `(,vx ,vy))
                                  (appendo vx vy '(s c h i s m)))))
           (equal? '(i)
                   (run* vx (common-element vx '(s c h i s m) '(a e i o u))))
           (equal? '(0 0 1 1 2 2 3 3 4 4)
                   (run 10 vy (conj (element-of vx '(1 2))
                                    (natural vy))))
           (equal? '(0 1 2 3 4 5 6 7 8 9)
                   (run 10 vy (conj-i (element-of vx '(1 2))
                                      (natural vy))))))))
