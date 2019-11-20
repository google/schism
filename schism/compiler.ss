;; Copyright 2018, 2019 Google LLC
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

(library (schism compiler)
  (export compile-library compile-stdin->stdout)
  (import (rnrs)
          (rnrs mutable-pairs)
          (schism)
          (%schism-runtime))
  ;; ====================== ;;
  ;; Helpers, etc.          ;;
  ;; ====================== ;;
  (define (void) (when #f #f))
  (define (trace-value x)
    (write x)
    (newline)
    x)
  (define (trace-and-error x where what)
    (trace-value x)
    (error where what))
  (define (index-of-helper x ls index same?)
    (and (pair? ls)
         (if (same? x (car ls))
             index
             (index-of-helper x (cdr ls) (+ 1 index) same?))))
  (define (index-of x ls same?)
    (index-of-helper x ls 0 same?))
  (define (adjoin x set same?)
    (if (index-of x set same?)
        set
        (cons x set)))
  (define (remove-duplicates ls same?)
    (fold-right (lambda (x ls) (adjoin x ls same?)) '() ls))
  (define (filter pred ls)
    (fold-right (lambda (x ls) (if (pred x) (cons x ls) ls)) '() ls))
  (define (filter-out pred ls)
    (filter (lambda (x) (not (pred x))) ls))
  (define (and-map f ls)
    (or (null? ls) (and (f (car ls)) (and-map f (cdr ls)))))
  (define (or-map f ls)
    (and (pair? ls) (or (f (car ls)) (or-map f (cdr ls)))))
  (define (map2 f l1 l2)
    (if (null? l1)
        '()
        (cons (f (car l1) (car l2)) (map2 f (cdr l1) (cdr l2)))))
  (define (union a b)
    (cond
     ((null? a) b)
     ((memq (car a) b) (union (cdr a) b))
     (else (cons (car a) (union (cdr a) b)))))
  (define (set-diff set sub)
    (if (null? set)
        '()
        (if (memq (car set) sub)
            (set-diff (cdr set) sub)
            (cons (car set) (set-diff (cdr set) sub)))))

  ;; TODO: move this into the library
  (define (memq x ls)
    (cond
     ((null? ls) #f)
     ((eq? (car ls) x) ls)
     (else (memq x (cdr ls)))))

  (define (runtime-imports)
    '((bool eq? (scm x) (scm y))

      (bool number? (scm x))
      (bool char? (scm x))

      (scm %make-number (i32 x))
      (scm %make-char (i32 x))

      (i32 %number-value (scm x))
      (i32 %char-value (scm x))

      (bool string? (scm x))
      (bool %symbol? (scm x))
      (bool %string=? (scm x) (scm y))

      (scm %list->string (scm x))
      (scm %string->list (scm x))

      (scm %string-append (scm x) (scm y))

      (scm %string->symbol (scm x))
      (scm %symbol->string (scm x))

      (scm %make-gensym (scm x))
      (bool %gensym? (scm x))

      (scm cons (scm x) (scm y))
      (bool pair? (scm x))
      (scm %car (scm x))
      (scm %cdr (scm x))
      (void %set-car! (scm x) (scm y))
      (void %set-cdr! (scm x) (scm y))

      (scm %get-false)
      (scm %get-true)
      (scm %get-null)
      (scm eof-object)
      (scm %get-void)

      (scm %make-closure (i32 index) (i32 nfields))
      (bool procedure? (scm x))
      (i32 %closure-index (scm x))
      (void %set-closure-free-var! (scm x) (i32 i) (scm y))
      (scm %closure-free-var (scm x) (i32 i))

      (i32 %read-char)
      (i32 %peek-char)
      (void %write-char (i32 c))
      (void error (scm where) (scm what))
      (void %log-char (i32 c))
      (void %flush-log)
      ;; TODO: replace %open-as-stdin and %file-exists? with proper ports
      (void %open-as-stdin (scm filename))
      (bool %file-exists? (scm filename))))

  ;; TODO: The %-intrinsics should not be accessible to user code.
  (define (intrinsics)
    '((scm %unreachable)
      (i32 bitwise-not (i32 x))
      (i32 bitwise-and (i32 x) (i32 y))
      (i32 bitwise-ior (i32 x) (i32 y))
      (i32 bitwise-arithmetic-shift-left (i32 x) (i32 bits))
      (i32 bitwise-arithmetic-shift-right (i32 x) (i32 bits))
      (i32 + (i32 x) (i32 y))
      (i32 * (i32 x) (i32 y))
      (i32 - (i32 x) (i32 y))
      (i32 div0 (i32 x) (i32 y))
      (i32 mod0 (i32 x) (i32 y))
      (bool < (i32 x) (i32 y))))

  ;; ====================== ;;
  ;; Resolve Imports        ;;
  ;; ====================== ;;
  ;;
  ;; See docs/libraries.md for more detail about what this pass does and how it works.
  (define (read-imports lib)
    (let ((name (cadr lib))
          (imports (cdr (cadddr lib))))
      (cons lib (read-library-list imports (cons name '())))))

  (define (read-library-list library-names visited)
    (if (null? library-names)
        '()
        (let* ((name (library-name-from-import (car library-names)))
               (lib (read-library name)))
          ;; lib = (library name (export exports ...) (import imports...) body)
          (let ((name (cadr lib))
                (exports (cdaddr lib))
                (imports (cdr (cadddr lib)))
                (body (cdddr lib)))
            (cons lib (read-library-list
                       (append (filter-imports imports visited) (cdr library-names))
                       (cons name visited)))))))

  (define (library-name-equal? lib1 lib2)
    (list-all-eq? lib1 lib2))

  (define (library-name-from-import import)
    (if (pair? import)
        (if (eq? (car import) 'only)
            (cadr import)
            import)
        (error 'library-name-from-import "Malformed import clause")))

  (define (read-library-from-file filename)
    (%open-as-stdin filename)
    (read))

  (define (find-library-file name)
    (let ((search-paths '("./test/lib"
                          "./lib"
                          "./scheme-lib"))
          (path-suffix (string-append
                        (fold-left (lambda (path part)
                                     (string-append
                                      path
                                      (string-append "/" (symbol->string part))))
                                   ""
                                   name)
                        ".ss")))
      (fold-left (lambda (found base-path)
                   (or found
                       (let ((path (string-append base-path path-suffix)))
                         (and (%file-exists? path) path))))
                 #f
                 search-paths)))

  (define (read-library name)
    (cond
     ;; Special case internal libraries
     ;;
     ;; TODO: only bring the intrinsic names into scope if these
     ;; libraries are loaded.
     ((library-name-equal? name '(%schism-runtime))
      '(library (%schism-runtime) (export) (import)))
     (else (let ((path (find-library-file name)))
             (if path
                 (read-library-from-file path)
                 (begin
                   (display name) (newline)
                   (error 'read-library "Could not find library")))))))

  (define (filter-imports imports visited)
    (if (null? imports)
        '()
        (if (find-library-name (car imports) visited)
            (filter-imports (cdr imports) visited)
            (cons (car imports) (filter-imports (cdr imports) visited)))))

  (define (find-library-name name name-list)
    (and (pair? name-list)
         (or (list-all-eq? name (car name-list))
             (find-library-name name (cdr name-list)))))

  ;; ====================== ;;
  ;; Parsing                ;;
  ;; ====================== ;;
  (define (quasicons head tail)
    (if (and (pair? tail) (eq? (car tail) 'quote)
             (pair? (cdr tail)) (null? (cddr tail))
             (pair? head) (eq? (car head) 'quote)
             (pair? (cdr head)) (null? (cddr head)))
        `'(,(cadr head) . ,(cadr tail))
        `(cons ,head ,tail)))
  (define (expand-quasiquote expr level)
    (if (pair? expr)
        (let ((head (car expr))
              (tail (cdr expr)))
          (cond
           ((and (eq? head 'unquote) (pair? tail) (null? (cdr tail)))
            (if (zero? level)
                (expand-macros (car tail))
                (quasicons ''unquote (expand-quasiquote tail (- level 1)))))
           ((and (eq? head 'quasiquote) (pair? tail) (null? (cdr tail)))
            (quasicons ''quasiquote
                       (expand-quasiquote tail (+ level 1))))
           (else
            (quasicons (expand-quasiquote head level)
                       (expand-quasiquote tail level)))))
        `',expr))

  (define (expand-macros expr)
    (if (pair? expr)
        (let ((tag (car expr)))
          (cond
           ((eq? tag 'quote) expr)
           ((eq? tag 'quasiquote) (expand-quasiquote (cadr expr) 0))
           ((eq? tag 'when)
            (expand-macros `(if ,(cadr expr) (begin . ,(cddr expr)) (begin))))
           ((eq? tag 'unless)
            (expand-macros `(if ,(cadr expr) (begin) (begin . ,(cddr expr)))))
           ((eq? tag 'or)
            (if (null? (cdr expr))
                #f
                (if (null? (cddr expr))
                    (expand-macros (cadr expr))
                    (let ((t (gensym "t")))
                      `(let ((,t ,(expand-macros (cadr expr))))
                         (if ,t ,t ,(expand-macros (cons 'or (cddr expr)))))))))
           ((eq? tag 'and)
            (if (null? (cdr expr))
                #t
                (if (null? (cddr expr))
                    (expand-macros (cadr expr))
                    `(if ,(expand-macros (cadr expr))
                         ,(expand-macros (cons 'and (cddr expr)))
                         #f))))
           ((eq? tag 'not)
            `(if ,(expand-macros (cadr expr)) #f #t))
           ((eq? tag 'cond)
            (let ((clause (cadr expr))
                  (rest (cddr expr)))
              (if (eq? (car clause) 'else)
                  (expand-macros (cons 'begin (cdr clause)))
                  `(if ,(expand-macros (car clause))
                       ,(expand-macros (cons 'begin (cdr clause)))
                       ,(expand-macros (cons 'cond rest))))))
           ((eq? tag 'let*)
            (let ((bindings (cadr expr)))
              (if (null? bindings)
                  (expand-macros (cons 'begin (cddr expr)))
                  ;; bindings: ((x e) . rest)
                  (let ((x (caar bindings))
                        (e (cadar bindings))
                        (rest (cdr bindings)))
                    `(let ((,x ,(expand-macros e)))
                       ,(expand-macros `(let* ,rest . ,(cddr expr))))))))
           (else (map expand-macros expr))))
        expr))

  (define (empty-env) '())
  (define (add-env name thunk env)
    (cons (cons name thunk) env))
  (define (append-env env1 env2)
    (append env1 env2))
  (define (add-lexical name var env)
    (add-env name
             (lambda () `(var ,var))
             env))
  (define (rename-var var)
    (gensym (symbol->string var)))
  (define (lexical-refs vars)
    (map (lambda (v) `(var ,v)) vars))
  ;; Eta-expansion for by-value reference to top-level and intrinsic
  ;; definitions.
  (define (adapt-type from to expr)
    (cond
     ((eq? from to) expr)
     ((eq? from 'void) `(seq ,expr (const ,(void))))
     ((and (eq? from 'scm) (eq? to 'i32)) `(call %number-value ,expr))
     ((and (eq? from 'i32) (eq? to 'scm)) `(call %make-number ,expr))
     ((and (eq? from 'bool) (eq? to 'scm)) `(if ,expr (const #t) (const #f)))
     (else
      (trace-and-error (cons from to) 'adapt-type "unhandled case"))))
  (define (add-top-level env def)
    (let ((tag (car def)))
      (unless (eq? (car def) 'define)
        (trace-and-error def 'add-top-level "unmatched top-level declaration"))
      (let ((name (caadr def)) (args (cdadr def)))
        (add-env name
                 (lambda ()
                   (let ((args (map rename-var args)))
                     `(lambda ,args (call ,name . ,(lexical-refs args)))))
                 env))))
  (define (add-typed-primitive env tag prim)
    (let ((return-type (car prim))
          (name (cadr prim))
          (arg-types (map car (cddr prim)))
          (arg-names (map cadr (cddr prim))))
      (cond
       ((memq name '(%make-number %number-value))
        ;; All other primitives can be adapted to take and receive
        ;; Scheme values, but the whole point of this one is to convert
        ;; between Scheme numbers and i32 -- so we need to avoid adding
        ;; adapters that would undo the conversion!
        (add-top-level env `(define (,name . ,arg-names))))
       (else
        (add-env name
                 (lambda ()
                   (let ((args (map rename-var arg-names)))
                     `(lambda ,args
                        ,(adapt-type
                          return-type 'scm
                          `(,tag ,name .
                                 ,(map2 (lambda (expr type)
                                          (adapt-type 'scm type expr))
                                        (lexical-refs args)
                                        arg-types))))))
                 env)))))
  (define (add-intrinsic env intrinsic)
    (add-typed-primitive env 'icall intrinsic))
  (define (add-import env import)
    (add-typed-primitive env 'call import))

  (define (add-top-levels defs env)
    (fold-left add-top-level env defs))
  (define (add-top-levels-filter filter? defs env)
    (fold-left (lambda (env def)
                 (if (filter? def)
                     (add-top-level env def)
                     env))
               env
               defs))
  (define (add-intrinsics intrinsics env)
    (fold-left add-intrinsic env intrinsics))
  (define (add-imports imports env)
    (fold-left add-import env imports))
  (define (add-lexicals vars renamed env)
    (if (null? vars)
        env
        (add-lexical (car vars) (car renamed)
                     (add-lexicals (cdr vars) (cdr renamed) env))))
  (define (lookup name env)
    (let ((pair (assq name env)))
      (unless pair
        (trace-and-error name 'lookup "unbound identifier"))
      ((cdr pair))))

  (define (make-export-environment lib)
    (let* ((name (cadr lib))
           (body (cddr lib))
           (exports (cdar body))
           (defs (cddr body)))
      (cons name
            (add-top-levels-filter
             (lambda (def)
               ;; def == (define (name args ...) body ...)
               (let ((name (caadr def)))
                 (memq name exports)))
             defs
             (empty-env)))))

  (define (make-let-bindings vars values)
    (map2 (lambda (var value) `(,var ,value)) vars values))

  (define (parse-expr expr env)
    (cond
     ((or (null? expr) (number? expr) (boolean? expr) (char? expr) (string? expr))
      `(const ,expr))
     ((symbol? expr) (lookup expr env))
     ((pair? expr)
      (let ((op (car expr)))
        (cond
         ((eq? op 'quote) `(const ,(cadr expr)))
         ((eq? op 'if)
          (let ((t (cadr expr))
                (c (caddr expr))
                (a (cadddr expr)))
            `(if (call eq? ,(parse-expr t env) (const #f))
                 ,(parse-expr a env)
                 ,(parse-expr c env))))
         ((eq? op 'let)
          (let* ((vars (map car (cadr expr)))
                 (values (parse-exprs (map cadr (cadr expr)) env))
                 (vars* (map rename-var vars))
                 (bindings (make-let-bindings vars* values))
                 (body (parse-body (cddr expr)
                                   (add-lexicals vars vars* env))))
            `(let ,bindings ,body)))
         ((eq? op 'begin)
          (if (null? (cdr expr))
              `(const ,(void))
              (parse-begin (parse-expr (cadr expr) env) (cddr expr) env)))
         ((eq? op 'lambda)
          (let* ((args (cadr expr))
                 (args* (map rename-var args))
                 (body (parse-body (cddr expr)
                                   (add-lexicals args args* env))))
            `(lambda ,args* ,body)))
         (else
          `(apply-procedure . ,(parse-exprs expr env))))))
     (else
      (trace-and-error expr 'parse-expr "Unrecognized expression"))))
  (define (parse-begin head tail env)
    ;; To make the simplifier's job easier, the front-end should not
    ;; create a seq with a seq in its tail.
    (cond
     ((null? tail) head)
     ((and (pair? (car tail)) (eq? (caar tail) 'begin))
      (parse-begin head (append (cdar tail) (cdr tail)) env))
     (else
      (parse-begin `(seq (drop ,head) ,(parse-expr (car tail) env))
                   (cdr tail) env))))
  (define (parse-exprs exprs env)
    (map (lambda (expr) (parse-expr expr env)) exprs))
  (define (parse-body body env)
    (unless (pair? body) (error 'parse-body "Empty body"))
    (parse-begin (parse-expr (car body) env) (cdr body) env))
  (define (parse-function function env)
    (let ((type (car function)))
      (cond
       ((eq? 'define type)
        (let* ((name (caadr function))
               (args (cdadr function))
               (args* (map rename-var args))
               (body (parse-body (cddr function)
                                 (add-lexicals args args* env))))
          `(,(cons name args*) ,body)))
       (else
        (trace-and-error
         function 'parse-function "invalid top-level declaration")))))
  (define (parse-functions functions env)
    (map (lambda (fn) (parse-function fn env)) functions))
  (define (compute-imported-functions lib imports)
    (map (lambda (import) `(%wasm-import ,lib . ,import)) imports))
  (define (parse-library lib import-envs)
    ;; For now just assume it's correctly formed. We can do error checking later.
    (let* ((body (cddr lib))     ;; skip the library and name
           (exports (cdar body)) ;; names of the functions exported
           (library-imports (cdadr body))
           (defs (cddr body))
           (imports (runtime-imports))
           (env (add-intrinsics (intrinsics)
                                (add-imports imports (empty-env))))
           (body-env (add-imported
                      ;; every library implicitly imports itself
                      (cons (cadr lib) library-imports)
                      import-envs
                      env))
           (body-env (add-top-levels-filter
                      ;; Filter out names that are exported, since
                      ;; those have already been included.
                      (lambda (def)
                        (let ((name (caadr def)))
                          (not (memq name exports))))
                      defs body-env)))
      (cons exports
            (append (compute-imported-functions "rt" imports)
                    (parse-functions defs body-env)))))
  (define (parse-libraries libs)
    (let ((imported-envs (map make-export-environment libs)))
      (let ((first (parse-library (car libs) imported-envs))
            (imported-functions (fold-left (lambda (functions lib)
                                             (append functions
                                                     (cdr (parse-library lib imported-envs))))
                                           '()
                                           (cdr libs))))
        (cons (car first) (append (cdr first) imported-functions)))))

  (define (add-imported imports import-envs env)
    (if (null? imports)
        env
        (begin
          (add-imported (cdr imports)
                        import-envs
                        (find-import (library-name-from-import (car imports))
                                     import-envs
                                     env)))))
  (define (find-import library import-envs env)
    (if (null? import-envs)
        (begin
          (display library) (newline)
          (error 'find-import "Could not find library"))
        (if (list-all-eq? library (caar import-envs))
            (append-env (cdar import-envs) env)
            (find-import library (cdr import-envs) env))))

  (define (expand-quote expr)
    (cond
     ;; Literals self-evaluate
     ((or (number? expr) (boolean? expr) (char? expr) (string? expr) (null? expr))
      expr)
     ((symbol? expr)
      `(string->symbol ,(symbol->string expr)))
     ((pair? expr)
      `(cons ,(expand-quote (car expr)) ,(expand-quote (cdr expr))))
     (else
      (trace-and-error expr 'expand-quote "invalid datum"))))

  ;; ====================== ;;
  ;; Simplification         ;;
  ;; ====================== ;;

  ;; The parsing phase introduces eta-expanded lambdas for top-levels
  ;; and intrinsics, and canonicalizes "if" tests to be relop
  ;; intrinsics.  This pass simplifies away the introduced complexity
  ;; where it's not needed.

  (define (effect-free-callee? callee)
    ;; Imports known to be effect-free.
    (memq callee '(%peek-char
                   eq? number? char?
                   %make-number %make-char
                   %number-value %char-value
                   string? %symbol? %string=?
                   %list->string %string->list
                   %string->symbol %symbol->string
                   %make-gensym %gensym?
                   cons pair? %car %cdr
                   %get-false %get-true %get-null eof-object %get-void
                   %make-closure procedure? %closure-index
                   %closure-free-var)))
  (define (effect-free-intrinsic? op)
    (memq op '(bitwise-not bitwise-and bitwise-ior
               bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
               + * - <)))
  (define (effect-free? expr)
    (let ((tag (car expr)))
      (or (eq? tag 'lambda)
          (eq? tag 'var)
          (eq? tag 'const)
          (eq? tag 'i32)
          (and (eq? tag 'call)
               (effect-free-callee? (cadr expr))
               (and-map effect-free? (cddr expr)))
          (and (eq? tag 'icall)
               (effect-free-intrinsic? (cadr expr))
               (and-map effect-free? (cddr expr))))))

  (define (simplify-seq head tail)
    (cond
     ((eq? (car head) 'nop) tail)
     ((eq? (car tail) 'nop) head)
     ((eq? (car tail) 'seq)
      ;; Quadratic, but shouldn't be hit normally.
      (simplify-seq (simplify-seq head (cadr tail)) (caddr tail)))
     (else `(seq ,head ,tail))))

  (define (literal-expr? x)
    (eq? (car x) 'const))
  (define (literal-value x)
    (cadr x))

  (define (boolean-expr? x)
    (or (eq? (car x) 'lambda) (literal-expr? x)))
  (define (boolean-value x)
    (not (and (eq? (car x) 'const) (eq? (cadr x) #f))))

  (define (simplify-if t c a)
    (or
     (and
      (eq? (car t) 'i32)
      ;; (if (i32 0) C A) -> A; C otherwise.  Note that the continuation
      ;; of the test is of type i32.
      (if (zero? (cadr t)) a c))

     (and
      ;; (if (if TT TC TA) C A)
      (eq? (car t) 'if)
      (let ((tt (cadr t))
            (tc (caddr t))
            (ta (cadddr t)))
        (and (boolean-expr? tc) (boolean-expr? ta)
             (let ((tc (boolean-value tc)) (ta (boolean-value ta)))
               (cond
                ((eq? tc ta)
                 ;; (if (if TT b b) C A) -> (seq TT ,(if b C A))
                 (simplify-seq (simplify-drop tt) (if tc c a)))
                (tc
                 ;; (if (if TT #t #f) C A) -> (if TT C A)
                 (simplify-if tt c a))
                (else
                 ;; (if (if TT #f #t) C A) -> (if TT A C)
                 (simplify-if tt a c)))))))

     (and
      ;; (if (call eq? X Y) C A)
      (eq? (car t) 'call)
      (eq? (cadr t) 'eq?)
      (let ((x (caddr t)) (y (cadddr t)))
        (or
         (and (literal-expr? x) (literal-expr? y)
              ;; Fold eq? between literals.
              (if (eq? (literal-value x) (literal-value y)) c a))
         ;; Simplify (if (eq? X #f) #f #t) result from beta-reduction.
         (and (eq? (car x) 'if)
              (boolean-expr? y) (not (boolean-value y))
              (let ((tft (cadr x))
                    (tfc (caddr x))
                    (tfa (cadddr x)))
                (and (boolean-expr? tfc) (boolean-expr? tfa)
                     (let ((tfc (boolean-value tfc)) (tfa (boolean-value tfa)))
                       (cond
                        ((eq? tfc tfa)
                         ;; (if (call eq? (if TFT b b) #f) C A) -> (seq TFT ,(if b A C))
                         (simplify-seq (simplify-drop tft) (if tfc a c)))
                        (tfc
                         ;; (if (call eq? (if TFT #t #f) #f) C A) -> (if TFT A C)
                         (simplify-if tft a c))
                        (else
                         ;; (if (call eq? (if TFT #f #t) #f) C A) -> (if TFT C A)
                         (simplify-if tft c a))))))))))

     ;; Fallback.
     `(if ,t ,c ,a)))

  (define (simplify-drop expr)
    (if (effect-free? expr)
        '(nop)
        (let ((tag (car expr)))
          (cond
           ((eq? tag 'seq)
            (let ((head (cadr expr))
                  (tail (simplify-drop (caddr expr))))
              (if (eq? (car tail) 'nop)
                  head
                  `(seq ,head ,tail))))
           ((eq? tag 'let)
            (let ((bindings (cadr expr))
                  (body (simplify-drop (caddr expr))))
              (if (eq? (car body) 'nop)
                  ;; Convert bindings to sequence.
                  (fold-left (lambda (head expr)
                               (simplify-seq head (simplify-drop expr)))
                             '(nop)
                             (map cadr bindings))
                  `(let ,bindings ,body))))
           ((eq? tag 'if)
            `(if/void ,(cadr expr)
                      ,(simplify-drop (caddr expr))
                      ,(simplify-drop (cadddr expr))))
           ((or (eq? tag 'drop) (eq? tag 'if/void))
            (trace-and-error expr 'simplify-drop "unrecognized-expr"))
           (else
            `(drop ,expr))))))

  (define (inline-call-arg var value arg)
    (cond
     ((eq? (car arg) 'var)
      (and (eq? (cadr arg) var)
           value))
     ((and (or (eq? (car arg) 'call)
               (eq? (car arg) 'icall))
           (pair? (cddr arg))
           (null? (cdddr arg)))
      ;; Try to inline through unary calls, to punch through type
      ;; conversions.
      (let ((x (inline-call-arg var value (caddr arg))))
        (and x
             `(,(car arg) ,(cadr arg) ,x))))
     (else #f)))
  (define (inline-call-args vars values args)
    (cond
     ((null? args) '())
     ((null? vars) #f)
     (else
      (let ((tail (inline-call-args (cdr vars) (cdr values) (cdr args))))
        (and tail
             (let ((head (inline-call-arg (car vars) (car values) (car args))))
               (and head
                    (cons head tail))))))))

  (define (reify-let vars values body)
    `(let ,(make-let-bindings vars values) ,body))

  (define (simplify-let vars values body)
    ;; Many beta reductions just remove the eta-expanded top-levels
    ;; introduced in the parse phase.  In that case, the "let" is
    ;; unnecessary.  Here we have little hack to inline the "let" in
    ;; those cases..
    (let ((tag (car body)))
      (cond
       ((null? vars) body)
       ((eq? tag 'var)
        (if (eq? (cadr body) (car vars))
            (simplify-seq (simplify-drop
                           (simplify-let (cdr vars) (cdr values)
                                         `(const ,(void))))
                          (car values))
            (simplify-seq (simplify-drop (car values))
                          (simplify-let (cdr vars) (cdr values) body))))
       ((or (eq? tag 'i32) (eq? tag 'const))
        (simplify-seq (simplify-drop (car values))
                      (simplify-let (cdr vars) (cdr values) body)))
       ((eq? tag 'seq)
        (if (literal-expr? (caddr body))
            `(seq ,(simplify-let vars values (cadr body))
                  ,(caddr body))
            (reify-let vars values body)))
       ((and (eq? tag 'if)
             (literal-expr? (caddr body))
             (literal-expr? (cadddr body)))
        (simplify-if (simplify-let vars values (cadr body))
                     (caddr body)
                     (cadddr body)))
       ((or (eq? tag 'call) (eq? tag 'icall))
        (let ((callee (cadr body))
              (args (cddr body)))
          (cond
           ((null? args)
            (simplify-seq (simplify-drop (car values))
                          (simplify-let (cdr vars) (cdr values) body)))
           ((null? (cdr args))
            `(,tag ,callee ,(simplify-let vars values (car args))))
           (else
            (let ((args* (inline-call-args vars values args)))
              (if args*
                  `(,tag ,callee . ,args*)
                  (reify-let vars values body)))))))
       (else
        (reify-let vars values body)))))

  (define (constant-arg? expr)
    (memq (car expr) '(i32 const)))

  (define (fold-constants/call callee args)
    ;; Note, we know that the arity of the call is correct, and that
    ;; argument types match what the callee expects, so we can directly
    ;; reach in and pluck out i32/const values.
    (cond
     ((eq? callee '%make-number)
      `(const ,(cadr (car args))))
     ((eq? callee '%number-value)
      `(i32 ,(cadr (car args))))
     ((eq? callee '%make-char)
      `(const ,(integer->char (cadr (car args)))))
     ((eq? callee '%char-value)
      `(i32 ,(char->integer (cadr (car args)))))
     ((eq? callee 'eq?)
      `(i32 ,(if (eq? (cadr (car args)) (cadr (cadr args))) 1 0)))
     (else
      ;; Add more cases here.
      #f)))

  (define (simplify-call callee args)
    (or (and (effect-free-callee? callee)
             (and-map constant-arg? args)
             (fold-constants/call callee args))
        (let ((inverse (assq callee '((%make-number . %number-value)
                                      (%number-value . %make-number)
                                      (%make-char . %char-value)
                                      (%char-value . %make-char)))))
          (and inverse
               (let ((operand (car args)))
                 (and (eq? (car operand) 'call)
                      (eq? (cadr operand) (cdr inverse))
                      (caddr operand)))))
        `(call ,callee . ,args)))

  (define (simplify-icall op args)
    `(icall ,op . ,args))

  (define (simplify-apply-procedure target args)
    (if (and (eq? 'lambda (car target))
             (eq? (length (cadr target)) (length args)))
        ;; ((lambda (x ...) body) arg ...) -> (let ((x arg) ...) body)
        (simplify-let (cadr target) args (caddr target))
        `(apply-procedure ,target . ,args)))

  (define (simplify-expr expr)
    (let ((tag (car expr)))
      (cond
       ((or (eq? tag 'i32) (eq? tag 'const)) expr)
       ((eq? tag 'var) expr)
       ((eq? tag 'let)
        (let* ((vars (map car (cadr expr)))
               (vals (map simplify-expr (map cadr (cadr expr))))
               (body (simplify-expr (caddr expr))))
          (simplify-let vars vals body)))
       ((eq? tag 'drop)
        (simplify-drop (simplify-expr (cadr expr))))
       ((eq? tag 'seq)
        (simplify-seq (simplify-expr (cadr expr)) (simplify-expr (caddr expr))))
       ((eq? tag 'if)
        (let ((t (simplify-expr (cadr expr)))
              (c (simplify-expr (caddr expr)))
              (a (simplify-expr (cadddr expr))))
          (simplify-if t c a)))
       ((eq? tag 'call)
        (simplify-call (cadr expr) (map simplify-expr (cddr expr))))
       ((eq? tag 'icall)
        (simplify-icall (cadr expr) (map simplify-expr (cddr expr))))
       ((eq? tag 'apply-procedure)
        (simplify-apply-procedure (simplify-expr (cadr expr))
                                  (map simplify-expr (cddr expr))))
       ((eq? tag 'lambda)
        `(lambda ,(cadr expr) ,(simplify-expr (caddr expr))))
       (else
        (trace-and-error expr 'simplify-expr "unrecognized expr")))))
  (define (simplify-function fn)
    `(,(car fn) ,(simplify-expr (cadr fn))))
  (define (simplify-functions functions)
    (map simplify-function functions))

  ;; ====================== ;;
  ;; Closure conversion     ;;
  ;; ====================== ;;

  ;; Closure conversion will go through a couple of passes.
  ;;
  ;; 1. annotate-free-vars - find all the lambdas and turn them into
  ;; expressions with their free variables listed at the top
  ;; level. Also lifts closure bodies.
  ;;
  ;; That's basically it for now. Later passes will lower these forms further.

  (define (convert-closures fn*)
    (let* ((bodies (cons '() '())) ; Mutated in place by annotate-free-vars.
           (result (annotate-free-vars fn* bodies)))
      (append result
              (map generate-closure-function (car bodies)))))

  (define (annotate-free-vars fn* bodies)
    (map (lambda (fn)
           (let ((def (car fn))
                 (body (cadr fn)))
             `(,def ,(annotate-free-vars-expr body bodies))))
         fn*))
  (define (annotate-free-vars-expr expr bodies)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'let)
        (let ((bindings (annotate-free-vars-bindings (cadr expr) bodies))
              (body (annotate-free-vars-expr (caddr expr) bodies)))
          `(let ,bindings ,body)))
       ((eq? tag 'drop)
        `(drop ,(annotate-free-vars-expr (cadr expr) bodies)))
       ((eq? tag 'seq)
        `(seq ,(annotate-free-vars-expr (cadr expr) bodies)
              ,(annotate-free-vars-expr (caddr expr) bodies)))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        `(,tag ,(annotate-free-vars-expr (cadr expr) bodies)
               ,(annotate-free-vars-expr (caddr expr) bodies)
               ,(annotate-free-vars-expr (cadddr expr) bodies)))
       ((or (eq? tag 'call) (eq? tag 'icall))
        `(,tag ,(cadr expr) . ,(annotate-free-vars-expr* (cddr expr) bodies)))
       ((eq? tag 'apply-procedure)
        `(apply-procedure . ,(annotate-free-vars-expr* (cdr expr) bodies)))
       ((or (eq? tag 'var) (eq? tag 'nop) (eq? tag 'i32) (eq? tag 'const))
        expr)
       ((eq? tag 'lambda)
        ;; (lambda args body) -> (make-closure args (free-vars x*) body-tag)
        (let* ((body-tag (gensym "closure-body"))
               (args (cadr expr))
               (free-vars (find-free-vars expr))
               (body (annotate-free-vars-expr (caddr expr) bodies)))
          (set-car! bodies (cons `(,body-tag ,args ,free-vars ,body) (car bodies)))
          (let ((closure-var (gensym "closure-var")))
            `(let ((,closure-var (call %make-closure (%function-index ,body-tag)
                                       (i32 ,(length free-vars)))))
               ,(generate-save-free-vars `(var ,closure-var)
                                         closure-var free-vars 0)))))
       (else
        (trace-and-error expr 'annotate-free-vars-expr "unrecognized expr")))))
  (define (generate-save-free-vars tail closure free-vars index)
    (if (null? free-vars)
        tail
        `(seq ,(generate-save-free-vars `(call %set-closure-free-var!
                                               (var ,closure) (i32 ,index)
                                               (var ,(car free-vars)))
                                        closure (cdr free-vars) (+ 1 index))
              ,tail)))
  (define (annotate-free-vars-bindings bindings bodies)
    (map (lambda (binding)
           `(,(car binding)
             ,(annotate-free-vars-expr (cadr binding) bodies)))
         bindings))
  (define (annotate-free-vars-expr* expr* bodies)
    (map (lambda (expr) (annotate-free-vars-expr expr bodies)) expr*))

  (define (find-free-vars expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'var) (cdr expr))
       ((or (eq? tag 'nop) (eq? tag 'i32) (eq? tag 'const)) '())
       ((eq? tag 'drop) (find-free-vars (cadr expr)))
       ((eq? tag 'seq) (find-free-vars-expr* (cdr expr)))
       ((eq? tag 'lambda)
        (set-diff (find-free-vars (caddr expr))
                  (cadr expr)))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        (find-free-vars-expr* (cdr expr)))
       ((or (eq? tag 'call) (eq? tag 'icall))
        (find-free-vars-expr* (cddr expr)))
       ((eq? tag 'apply-procedure)
        (find-free-vars-expr* (cdr expr)))
       ((eq? tag 'let)
        (let ((rhs-vars (fold-left (lambda (fv binding)
                                     (union fv (find-free-vars (cadr binding))))
                                   '()
                                   (cadr expr)))
              (body-vars (set-diff (find-free-vars (caddr expr))
                                   (map (lambda (binding) (car binding)) (cadr expr)))))
          (union rhs-vars body-vars)))
       (else (trace-and-error expr 'find-free-vars "unrecognized expr")))))
  (define (find-free-vars-expr* expr*)
    (fold-left (lambda (free-vars expr)
                 (union free-vars (find-free-vars expr)))
               '()
               expr*))

  (define (generate-closure-function body)
    ;; (tag args free-vars body) -> (tag . body)
    (let ((closure-var (gensym "closure")))
      (let ((tag (car body))
            ;; Add an extra argument for the closure
            (args (cons closure-var (cadr body)))
            (free-vars (caddr body))
            (body (cadddr body)))
        (if (null? free-vars)
            `((,tag . ,args) ,body)
            `((,tag . ,args) (let ,(bind-free-vars closure-var free-vars 0)
                               ,body))))))
  (define (bind-free-vars closure free-vars index)
    (if (null? free-vars)
        '()
        (cons `(,(car free-vars)
                (call %closure-free-var (var ,closure) (i32 ,index)))
              (bind-free-vars closure (cdr free-vars) (+ 1 index)))))

  ;; cache := ((x . n) ...)
  (define (find-interned-constant x cache)
    (cond
     ((pair? x) #f)
     ((string? x)
      (or-map (lambda (y)
                (and (string? (car y))
                     (string=? x (car y))
                     (cdr y)))
              cache))
     (else
      (let ((entry (assq x cache)))
        (and entry (cdr entry))))))
  ;; env := (cache nglobals start)
  (define (intern-constant x env)
    (or
     (find-interned-constant x (car env))
     (let* ((init
             (cond
              ((null? x)      `(call %get-null))
              ((eq? x (void)) `(call %get-void))
              ((number? x)    `(call %make-number (i32 ,x)))
              ((boolean? x)   (if x `(call %get-true) `(call %get-false)))
              ((char? x)      `(call %make-char (i32 ,(char->integer x))))
              ((string? x)
               `(call %list->string
                      (icall get-global
                             (i32 ,(intern-constant (string->list x) env)))))
              ((symbol? x)
               `(call string->symbol
                      (icall get-global
                             (i32 ,(intern-constant (symbol->string x) env)))))
              ((pair? x)
               `(call cons
                      (icall get-global
                             (i32 ,(intern-constant (car x) env)))
                      (icall get-global
                             (i32 ,(intern-constant (cdr x) env)))))
              (else
               (trace-and-error x 'intern-constant "unexpected constant"))))
            ;; Note -- recursive calls to `intern-constant` mutate
            ;; `env`.  Destructure the environment here to avoid working
            ;; on a stale state.
            (cache (car env))
            (n (cadr env))
            (start (caddr env))
            (init `(icall set-global (i32 ,n) ,init)))
       (unless (pair? x)
         ;; Avoid caching pairs for the time being.
         (set-car! env (cons (cons x n) cache)))
       (set-car! (cdr env) (+ n 1))
       (set-car! (cddr env) (if start `(seq ,start ,init) init))
       n)))

  (define (lower-literals expr env)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'const)
        `(icall get-global (i32 ,(intern-constant (cadr expr) env))))
       ((or (eq? tag 'var) (eq? tag 'nop) (eq? tag 'i32))
        expr)
       ((eq? tag 'let)
        (let ((vars (map car (cadr expr)))
              (vals (lower-literals* (map cadr (cadr expr)) env))
              (body (lower-literals (caddr expr) env)))
          (reify-let vars vals body)))
       ((eq? tag 'drop)
        `(drop ,(lower-literals (cadr expr) env)))
       ((eq? tag 'seq)
        `(seq ,(lower-literals (cadr expr) env)
              ,(lower-literals (caddr expr) env)))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        `(,tag ,(lower-literals (cadr expr) env)
               ,(lower-literals (caddr expr) env)
               ,(lower-literals (cadddr expr) env)))
       ((or (eq? tag 'call) (eq? tag 'icall))
        `(,tag ,(cadr expr) . ,(lower-literals* (cddr expr) env)))
       ((eq? tag 'apply-procedure)
        `(apply-procedure . ,(lower-literals* (cdr expr) env)))
       ((eq? tag 'lambda)
        `(lambda ,(cadr expr) ,(lower-literals (caddr expr) env)))
       ((eq? tag '%function-index)
        expr)
       (else
        (trace-and-error expr 'lower-literals "unexpected expr")))))
  (define (lower-literals* expr* env)
    (map (lambda (expr) (lower-literals expr env)) expr*))
  (define (lower-literals-in-function fn env)
    (let ((def (car fn))
          (body (cadr fn)))
      `(,def ,(lower-literals body env))))

  ;; -> ((type . init) ...)
  (define (make-globals-for-literals nglobals)
    (if (zero? nglobals)
        '()
        (cons (cons 'anyref '(ref.null))
              (make-globals-for-literals (- nglobals 1)))))
  ;; -> (fns globals start)
  (define (lower-literals-in-functions fns)
    (let* ((cache '())
           (nglobals 0)
           (start #f)
           (env (cons cache (cons nglobals (cons start '()))))
           (fns (map (lambda (fn) (lower-literals-in-function fn env)) fns))
           (cache (car env))
           (nglobals (cadr env))
           (start (caddr env))
           (globals (make-globals-for-literals nglobals)))
      (cons fns (cons globals (cons start '())))))

  ;; ====================== ;;
  ;; Compile (make wasm)    ;;
  ;; ====================== ;;
  (define (args->types args)
    (map (lambda (_) 'anyref) args))
  (define (compile-icall op args env)
    (cond
     ((eq? op '%unreachable)
      '(unreachable))
     ((eq? op '+)
      (let ((a (compile-expr (car args) env #f))
            (b (compile-expr (cadr args) env #f)))
        `(i32.add ,a ,b)))
     ((eq? op '-)
      `(i32.sub ,(compile-expr (car args) env #f)
                ,(compile-expr (cadr args) env #f)))
     ((eq? op '*)
      (let ((a (compile-expr (car args) env #f))
            (b (compile-expr (cadr args) env #f)))
        `(i32.mul ,a ,b)))
     ((eq? op 'div0)
      (let ((a (compile-expr (car args) env #f))
            (b (compile-expr (cadr args) env #f)))
        `(i32.div_s ,a ,b)))
     ((eq? op 'mod0)
      (let ((a (compile-expr (car args) env #f))
            (b (compile-expr (cadr args) env #f)))
        `(i32.rem_s ,a ,b)))
     ((eq? op 'bitwise-and)
      (cons 'i32.and (compile-exprs args env)))
     ((eq? op 'bitwise-not)
      `(i32.xor (i32.const -1) . ,(compile-exprs args env)))
     ((eq? op 'bitwise-ior)
      (cons 'i32.or (compile-exprs args env)))
     ((eq? op 'bitwise-arithmetic-shift-left)
      (let ((num (compile-expr (car args) env #f))
            (shift-amount (compile-expr (cadr args) env #f)))
        `(i32.shl ,num ,shift-amount)))
     ((eq? op 'bitwise-arithmetic-shift-right)
      (let ((num (compile-expr (car args) env #f))
            (shift-amount (compile-expr (cadr args) env #f)))
        `(i32.shr_s ,num ,shift-amount)))
     ((eq? op '<)
      `(i32.lt_s ,(compile-expr (car args) env #f)
                 ,(compile-expr (cadr args) env #f)))
     ((eq? op 'get-global)
      `(get-global ,(cadr (car args))))
     ((eq? op 'set-global)
      `(seq ,(compile-expr (cadr args) env #f)
            (set-global ,(cadr (car args)))))
     (else
      (trace-and-error op 'compile-icall "unrecognized intrinsic call"))))
  (define (compile-exprs exprs env)
    (map (lambda (expr) (compile-expr expr env #f)) exprs))
  (define (compile-expr expr env tail?)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'nop) '(nop))
       ((eq? tag 'drop) `(drop ,(compile-expr (cadr expr) env #f)))
       ((eq? tag 'seq) `(seq ,(compile-expr (cadr expr) env #f)
                             ,(compile-expr (caddr expr) env tail?)))
       ((eq? tag 'i32) (cons 'i32.const (cdr expr)))
       ((eq? tag 'var)
        `(get-local
          ,(cdr (or (assq (cadr expr) env)
                    (trace-and-error (cons (cadr expr) env)
                                     'compile-expr "unbound local")))))
       ((eq? tag 'call)
        (cons (if tail? 'tail-call 'call)
              (cons (cadr expr) (compile-exprs (cddr expr) env))))
       ((eq? tag 'apply-procedure)
        (let ((args (args->types (cdr expr))))
          `(,(if tail? 'tail-call-indirect 'call-indirect)
            (fn ,args (anyref))
            . ,(append (compile-exprs (cdr expr) env)
                       `((call %closure-index ,(compile-expr (cadr expr) env #f)))))))
       ((eq? tag 'icall)
        (compile-icall (cadr expr) (cddr expr) env))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        `(,tag ,(compile-expr (cadr expr) env #f)
               ,(compile-expr (caddr expr) env tail?)
               ,(compile-expr (cadddr expr) env tail?)))
       ((eq? tag 'let)
        (let ((index (length env)))
          (compile-bindings (compile-expr (caddr expr)
                                          (bindings->env (cadr expr) env index)
                                          tail?)
                            (cadr expr) env index)))
       ((eq? tag '%function-index) expr)
       (else (trace-and-error expr 'compile-expr "unrecognized expression")))))

  (define (bindings->env bindings env index)
    (if (null? bindings)
        env
        (cons (cons (caar bindings) index)
              (bindings->env (cdr bindings) env (+ 1 index)))))
  (define (compile-bindings tail bindings env index)
    (if (null? bindings)
        tail
        (let ((val (compile-expr (cadar bindings) env #f))
              (init `(set-local ,index)))
          `(seq (seq ,val
                     ,(compile-bindings init (cdr bindings) env (+ 1 index)))
                ,tail))))
  (define (compile-function fn)
    (let* ((args (number-variables (cdar fn) 0))
           (body (compile-expr (cadr fn) args #t)))
      `(,(max (count-locals body) (length args)) ;; Number of local variables
        ,body)))
  (define (compile-functions fn*)
    (map compile-function fn*))

  ;; Determines how many locals were used in a body.
  (define (count-locals body)
    (let ((tag (car body)))
      (cond
       ((eq? tag 'nop) 0)
       ((eq? tag 'seq)
        (max (count-locals (cadr body)) (count-locals (caddr body))))
       ((memq tag '(call tail-call call-indirect tail-call-indirect))
        (count-locals-exprs (cddr body)))
       ((eq? tag 'get-local) (+ 1 (cadr body)))
       ((eq? tag 'set-local) (+ 1 (cadr body)))
       ((eq? tag 'get-global) 0)
       ((eq? tag 'set-global) 0)
       ((wasm-simple-op? tag)
        (count-locals-exprs (cdr body)))
       ((eq? tag 'i32.const) 0)
       ((eq? tag '%function-index) 0)
       ((or (eq? tag 'i32.store) (eq? tag 'i32.load))
        (count-locals-exprs (cddr body)))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        (count-locals-exprs (cdr body)))
       (else (trace-and-error body 'count-locals "unrecognized expression")))))
  (define (count-locals-exprs exprs)
    (if (null? exprs)
        0
        (max (count-locals (car exprs)) (count-locals-exprs (cdr exprs)))))

  (define (number-variables vars index)
    (if (pair? vars)
        (cons (cons (car vars) index) (number-variables (cdr vars) (+ 1 index)))
        '()))

  (define (types-equal? t1* t2*)
    (if (and (pair? t1*) (pair? t2*))
        (and (eq? (car t1*) (car t2*))
             (types-equal? (cdr t1*) (cdr t2*)))
        (and (null? t1*) (null? t2*))))
  (define (type-equal? t1 t2)
    (or (eq? t1 t2)
        (and (pair? t1) (pair? t2)
             (types-equal? (cadr t1) (cadr t2))
             (types-equal? (caddr t1) (caddr t2)))))
  (define (lookup-type t types)
    (index-of t types type-equal?))

  (define (wasm-import? fn)
    (eq? (car fn) '%wasm-import))
  (define (wasm-import-name fn)
    (cadddr fn))
  (define (import-arg-wasm-type type)
    (cond ((eq? type 'scm) 'anyref)
          ((eq? type 'i32) 'i32)
          (else (trace-and-error type 'import-arg-wasm-type "unhandled"))))
  (define (import-return-wasm-type type)
    (cond ((eq? type 'scm) '(anyref))
          ((eq? type 'i32) '(i32))
          ((eq? type 'void) '())
          ((eq? type 'bool) '(i32))
          (else (trace-and-error type 'import-return-wasm-type "unhandled"))))
  (define (wasm-import-type fn)
    (let ((ret (import-return-wasm-type (caddr fn)))
          (args (map import-arg-wasm-type (map car (cdr (cdddr fn))))))
      `(fn ,args ,ret)))

  (define (function-name fn)
    (caar fn))
  (define (function-type fn)
    ;; Scheme functions are assumed to always return an anyref and
    ;; take some number of anyrefs as inputs.
    `(fn ,(args->types (cdar fn)) (anyref)))

  (define (annotate-function-names-and-types defs)
    (map (lambda (def)
           (cons (function-name def) (cons (function-type def) def)))
         defs))
  (define (add-start-function name body annotated-defs)
    (cons (cons name (cons '(fn () ()) `((,name) ,body)))
          annotated-defs))

  (define (functions->type-ids annotated-defs types)
    (map (lambda (annotated-def)
           (let ((type (cadr annotated-def)))
             (or (lookup-type type types)
                 (trace-and-error type 'functions->type-ids
                                  "type not found"))))
         annotated-defs))

  (define (build-exports exports names)
    (map (lambda (ex)
           `(fn ,(or (index-of ex names eq?)
                     (trace-and-error ex 'build-exports "export not found"))
                ,(symbol->string ex)))
         exports))
  (define (build-imports imports types)
    (map (lambda (entry)
           (let ((module (cadr entry))
                 (name (symbol->string (cadddr entry)))
                 (type (lookup-type (wasm-import-type entry) types)))
             `(,module ,name ,type)))
         imports))

  (define (number-list ls i)
    (if (null? ls)
        '()
        (cons i (number-list (cdr ls) (+ 1 i)))))

  (define (wasm-simple-op? op)
    (or (eq? op 'i32.and) (eq? op 'i32.add) (eq? op 'i32.sub) (eq? op 'i32.mul)
        (eq? op 'i32.div_s) (eq? op 'i32.rem_s)
        (eq? op 'i32.or) (eq? op 'i32.xor)
        (eq? op 'i32.lt_s) (eq? op 'i32.shr_s) (eq? op 'i32.shl) (eq? op 'drop)
        (eq? op 'unreachable) (eq? op 'ref.null)))

  (define (resolve-calls-exprs exprs env types)
    (map (lambda (expr) (resolve-calls-expr expr env types)) exprs))
  (define (resolve-calls-expr expr env types)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'nop) expr)
       ((eq? tag 'i32.const) expr)
       ((eq? tag 'get-local) expr)
       ((eq? tag 'set-local) expr)
       ((eq? tag 'get-global) expr)
       ((eq? tag 'set-global) expr)
       ((eq? tag 'seq)
        `(seq ,(resolve-calls-expr (cadr expr) env types)
              ,(resolve-calls-expr (caddr expr) env types)))
       ((or (eq? tag 'call) (eq? tag 'tail-call))
        (cons tag (cons (or (index-of (cadr expr) env eq?)
                            (trace-and-error expr 'resolve-calls
                                             "callee not found"))
                        (resolve-calls-exprs (cddr expr) env types))))
       ((or (eq? tag 'call-indirect) (eq? tag 'tail-call-indirect))
        (let ((type-id (lookup-type (cadr expr) types)))
          `(,tag ,type-id . ,(resolve-calls-exprs (cddr expr) env types))))
       ((eq? tag '%function-index)
        `(i32.const ,(or (index-of (cadr expr) env eq?)
                         (trace-and-error expr 'resolve-calls
                                          "closure target not found"))))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        `(,tag
          ,(resolve-calls-expr (cadr expr) env types)
          ,(resolve-calls-expr (caddr expr) env types)
          ,(resolve-calls-expr (cadddr expr) env types)))
       ((eq? tag 'i32.store)
        (let ((offset (cadr expr))
              (index (resolve-calls-expr (caddr expr) env types))
              (value (resolve-calls-expr (cadddr expr) env types)))
          `(i32.store ,offset ,index ,value)))
       ((eq? tag 'i32.load)
        (let ((offset (cadr expr))
              (index (resolve-calls-expr (caddr expr) env types)))
          `(i32.load ,offset ,index)))
       ((wasm-simple-op? tag)
        (let ((args (resolve-calls-exprs (cdr expr) env types)))
          (cons tag args)))
       (else
        (trace-and-error expr 'resolve-calls-expr "unrecognized expression")))))
  (define (resolve-calls-fn function env types)
    `(,(car function) ,(resolve-calls-expr (cadr function) env types)))
  (define (resolve-calls functions env types)
    (map (lambda (fn) (resolve-calls-fn fn env types)) functions))

  ;; ====================== ;;
  ;; Wasm Binary Generation ;;
  ;; ====================== ;;
  (define (encode-sleb n)
    (if (let ((n* (+ n 64)))
          (eq? n* (bitwise-and n* #x7f)))
        `(,(bitwise-and n #x7f))
        (cons (bitwise-ior #x80 (bitwise-and n #x7f))
              (encode-sleb (bitwise-arithmetic-shift-right n 7)))))
  (define (encode-uleb n)
    (let ((next (bitwise-arithmetic-shift-right n 7)))
      (if (zero? next)
          `(,n)
          (cons (bitwise-ior #x80 (bitwise-and n #x7f))
                (encode-uleb next)))))

  (define (encode-string s)
    (let ((chars (string->list s)))
      (make-vec (length chars) (map char->integer chars))))

  (define (wasm-header)
    '(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))

  (define (make-vec length contents)
    (cons (encode-uleb length) contents))

  ;; id is the number, contents is a list of bytes
  (define (byte-count ls)
    (cond
     ((pair? ls) (+ (byte-count (car ls)) (byte-count (cdr ls))))
     ((null? ls) 0)
     (else 1)))
  (define (make-section id contents)
    (cons id (make-vec (byte-count contents) contents)))

  (define (encode-type type)
    (cond
     ((eq? type 'i32) '(#x7f))
     ((eq? type 'i64) '(#x7e))
     ((eq? type 'f32) '(#x7d))
     ((eq? type 'f64) '(#x7c))
     ((eq? type 'anyref) '(#x6f))
     ((eq? type 'void) '(#x40))
     ;; functions are (fn (t1 ...) (t2 ...)), for t1 ... -> t2 ...
     ((and (pair? type) (eq? (car type) 'fn))
      (cons #x60 (cons (encode-type-vec (cadr type)) (encode-type-vec (caddr type)))))
     (else (trace-and-error type 'encode-type "unrecognized type"))))
  (define (encode-type-vec types)
    (make-vec (length types) (map encode-type types)))
  (define (wasm-type-section types)
    (make-section 1 (encode-type-vec types)))

  (define (encode-import import)
    (let ((module (car import))
          (name (cadr import)))
      (cons (encode-string module)
            (cons (encode-string name)
                  (cons '(#x00) (encode-uleb (caddr import)))))))
  (define (wasm-import-section imports)
    ;; Add 1 to the length because we import a memory too.
    (make-section 2 (make-vec (length imports)
                              (map encode-import imports))))

  (define (encode-u32-vec nums)
    (make-vec (length nums) (map encode-uleb nums)))

  (define (wasm-function-section function-type-ids)
    (make-section 3 (encode-u32-vec function-type-ids)))

  (define (encode-export export)
    (cond
     ((eq? (car export) 'fn)
      (cons (encode-string (caddr export)) (cons #x00 (encode-uleb (cadr export)))))
     (else
      (trace-and-error export 'encode-export "unrecognized export"))))
  (define (wasm-export-section exports)
    (make-section 7 (make-vec (length exports) (map encode-export exports))))

  (define (encode-simple-op op expr)
    (cons (map encode-expr (cdr expr)) op))

  (define (encode-expr expr)
    (let ((tag (car expr)))
      (cond
       ((eq? tag 'seq)
        (cons (encode-expr (cadr expr)) (encode-expr (caddr expr))))
       ((eq? tag 'i32.const)
        (cons #x41 (encode-sleb (cadr expr))))
       ((eq? tag 'i32.lt_s)
        (encode-simple-op #x48 expr))
       ((eq? tag 'get-local)
        (cons #x20 (encode-uleb (cadr expr))))
       ((eq? tag 'set-local)
        (cons #x21 (encode-uleb (cadr expr))))
       ((eq? tag 'get-global)
        (cons #x23 (encode-uleb (cadr expr))))
       ((eq? tag 'set-global)
        (cons #x24 (encode-uleb (cadr expr))))
       ((eq? tag 'call)
        (cons (map encode-expr (cddr expr))
              (cons #x10 (encode-uleb (cadr expr)))))
       ((eq? tag 'tail-call)
        (cons (map encode-expr (cddr expr))
              (cons #x12 (encode-uleb (cadr expr)))))
       ((eq? tag 'call-indirect)
        `(,(map encode-expr (cddr expr))
          (#x11 ,(encode-uleb (cadr expr)) #x00)))
       ((eq? tag 'tail-call-indirect)
        `(,(map encode-expr (cddr expr))
          (#x13 ,(encode-uleb (cadr expr)) #x00)))
       ((or (eq? tag 'if) (eq? tag 'if/void))
        (let ((type (if (eq? tag 'if) 'anyref 'void))
              (t (cadr expr))
              (c (caddr expr))
              (a (cadddr expr)))
          (cons (encode-expr t)
                (cons #x04 (cons (encode-type type)
                                 (cons (encode-expr c)
                                       (cons #x05 (cons (encode-expr a) '(#x0b)))))))))
       ((eq? tag 'i32.store)
        (let ((align 0)
              (offset (cadadr expr))
              (index (encode-expr (caddr expr)))
              (value (encode-expr (cadddr expr))))
          (cons (cons index value)
                (cons '(#x36 #x0) ;;always use 0 alignment
                      (encode-uleb offset)))))
       ((eq? tag 'i32.load)
        (let ((align 0)
              (offset (cadadr expr))
              (index (encode-expr (caddr expr))))
          (cons index (cons '(#x28 #x0) ;;always use 0 alignment
                            (encode-uleb offset)))))
       ((eq? tag 'i32.add)
        (encode-simple-op #x6a expr))
       ((eq? tag 'i32.sub)
        (encode-simple-op #x6b expr))
       ((eq? tag 'i32.mul)
        (encode-simple-op #x6c expr))
       ((eq? tag 'i32.div_s)
        (encode-simple-op #x6d expr))
       ((eq? tag 'i32.rem_s)
        (encode-simple-op #x6f expr))
       ((eq? tag 'i32.and)
        (encode-simple-op #x71 expr))
       ((eq? tag 'i32.or)
        (encode-simple-op #x72 expr))
       ((eq? tag 'i32.xor)
        (encode-simple-op #x73 expr))
       ((eq? tag 'i32.shl)
        (encode-simple-op #x74 expr))
       ((eq? tag 'i32.shr_s)
        (encode-simple-op #x75 expr))
       ((eq? tag 'nop)
        '())
       ((eq? tag 'drop)
        (encode-simple-op #x1a expr))
       ((eq? tag 'unreachable)
        (encode-simple-op #x00 expr))
       ((eq? tag 'ref.null)
        (encode-simple-op #xd0 expr))
       (else
        (trace-and-error expr 'encode-expr "unrecognized expr")))))

  (define (encode-code locals body)
    (let ((contents (cons
                     (if (zero? locals)
                         (make-vec 0 '())
                         (make-vec 1 (cons (encode-uleb locals)
                                           (encode-type 'anyref))))
                     (cons (encode-expr body)
                           '(#x0b)))))
      (make-vec (byte-count contents) contents)))
  (define (encode-codes codes)
    (map (lambda (code) (encode-code (car code) (cadr code))) codes))
  (define (wasm-code-section codes)
    (make-section 10 (make-vec (length codes) (encode-codes codes))))

  (define (wasm-name-section names)
    (make-section 0 (cons (encode-string "name")
                          (make-section 1 ;; 1 for function name subsection
                                        (make-vec (length names)
                                                  (encode-name-maps names 0))))))
  (define (encode-name-maps names index)
    (if (null? names)
        '()
        (cons (cons (encode-uleb index)
                    (encode-string (symbol->string (car names))))
              (encode-name-maps (cdr names) (+ 1 index)))))

  (define (wasm-table-section num-items)
    (make-section 4 (make-vec 1
                              `(#x70 #x00 . ,(encode-uleb num-items)))))

  (define (encode-global global)
    (cons (cons (encode-type (car global)) #x01)
          (cons (encode-expr (cdr global)) #x0b)))
  (define (wasm-global-section globals)
    (make-section 6 (make-vec (length globals)
                              (map encode-global globals))))

  (define (wasm-start-section id)
    (make-section 8 (encode-uleb id)))

  (define (wasm-element-section element-ids)
    (make-section 9 (make-vec 1 `(0 ,(encode-expr `(i32.const 0))
                                    #x0b
                                    ,(make-vec (length element-ids)
                                               (map encode-uleb element-ids))))))

  ;; Takes a library and returns a list of the corresponding Wasm module
  ;; bytes
  (define (compile-library library)
    (let* ((libraries (read-imports library))
           (exports+fns (parse-libraries (map expand-macros libraries)))
           (exports (car exports+fns))
           (fns (cdr exports+fns))
           (imports (filter wasm-import? fns))
           (fns (filter-out wasm-import? fns))
           (fns (simplify-functions fns))
           (fns (convert-closures fns))
           (fns+globals+start (lower-literals-in-functions fns))
           (fns (car fns+globals+start))
           (globals (cadr fns+globals+start))
           (start (caddr fns+globals+start))
           (fns* (annotate-function-names-and-types fns))
           (start-name (gensym "start"))
           (fns* (add-start-function start-name start fns*))
           (fn-names (append (map wasm-import-name imports)
                             (map car fns*)))
           (types (remove-duplicates
                   (append (map wasm-import-type imports)
                           (map cadr fns*))
                   type-equal?))
           (fn-types (functions->type-ids fns* types))
           (fns (compile-functions (map cddr fns*)))
           (fns (resolve-calls fns fn-names types))
           (start-id (index-of start-name fn-names eq?))
           (imports (build-imports imports types))
           (exports (build-exports exports fn-names)))
      (cons
       (wasm-header)
       (cons
        (wasm-type-section types)
        (cons
         (wasm-import-section imports)
         (cons
          (wasm-function-section fn-types)
          (cons
           (wasm-table-section (length fn-names))
           (cons
            (wasm-global-section globals)
            (cons
             (wasm-export-section exports)
             (cons
              (wasm-start-section start-id)
              (cons
               (wasm-element-section (number-list fn-names 0))
               (cons
                (wasm-code-section fns)
                (wasm-name-section fn-names)))))))))))))

  (define (write-bytes ls)
    (cond
     ((null? ls) #t)
     ((number? ls) (write-char (integer->char ls)))
     (else
      (write-bytes (car ls))
      (write-bytes (cdr ls)))))
  (define (compile-stdin->stdout)
    (write-bytes (compile-library (read)))))
